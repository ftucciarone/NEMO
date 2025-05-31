MODULE p4zdiaz
   !!======================================================================
   !!                         ***  MODULE p4zdiaz  ***
   !! TOP :   PISCES Compute Nitrogen Fixation ( Diazotrophy )
   !!         This module is common to both PISCES and PISCES-QUOTA
   !!=========================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             5.0  !  2023-12  (O. Aumont, C. Ethe) 
   !!----------------------------------------------------------------------
   !!   p4z_rem       :  Compute remineralization/dissolution of organic compounds
   !!   p4z_rem_init  :  Initialisation of parameters for remineralisation
   !!   p4z_rem_alloc :  Allocate remineralisation variables
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zche          !  chemical model
   USE p4zprod         !  Growth rate of the 2 phyto groups
   USE p2zlim          !  Co-limitations of differents nutrients
   USE p4zlim          !  Co-limitations of differents nutrients
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager


   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_diaz         ! called in p4zbio.F90
   PUBLIC   p4z_diaz_init    ! called in trcini_pisces.F90
   PUBLIC   p4z_diaz_alloc   ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::   nitrfix      !: Nitrogen fixation rate
   REAL(wp), PUBLIC ::   diazolight   !: Nitrogen fixation sensitivty to light
   REAL(wp), PUBLIC ::   concfediaz   !: Fe half-saturation Cste for diazotrophs

   REAL(wp), SAVE :: r1_rday, xtemp13, xtemp23

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: nitrpot    !: Nitrogen fixation

   LOGICAL         :: l_dia_nfix
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS


   SUBROUTINE p4z_diaz( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_diaz  ***
      !!
      !! ** Purpose : - Compute Nitrogen Fixation 
      !!                Small source iron from particulate inorganic iron
      !!
      !! ** Method  : - Potential nitrogen fixation dependant on temperature and iron
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt         ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, itt
      !
      REAL(wp) ::  ztrfer, ztrpo4s, ztrdp, zwdust, zmudia
      REAL(wp) ::  zsoufer, zlight, ztrpo4, ztrdop, zratpo4
      REAL(wp) ::  zfact, ztemp, zdiano3, zdianh4
      !
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_diaz')
      !
      IF( kt == nittrc000 )  l_dia_nfix   = iom_use( "Nfix" ) .OR. iom_use( "Nfixo2" )
      !
#if defined key_RK3
      ! Don't consider mid-step values if online coupling
      ! because these are possibly non-monotonic (even with FCT): 
      IF ( l_offline ) THEN ; itt = Kmm ; ELSE ; itt = Kbb ; ENDIF 
#else 
      itt = Kmm
#endif
      ! Nitrogen fixation process
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         !                      ! Potential nitrogen fixation dependant on temperature and iron
         zlight  =  ( 1.- EXP( -etot_ndcy(ji,jj,jk) / diazolight ) ) * ( 1. - fr_i(ji,jj) )
         !
         ztemp = ts(ji,jj,jk,jp_tem,itt)
         zmudia = MAX( 0.,-0.001096*ztemp*ztemp + 0.057*ztemp -0.637 ) / rno3
         !       Potential nitrogen fixation dependant on temperature and iron
         IF( ln_p2z ) THEN
            zdiano3 = tr(ji,jj,jk,jpno3,Kbb) / ( concnno3 + tr(ji,jj,jk,jpno3,Kbb) )
            zfact   = ( 1. - zdiano3 ) * rfact2
            ztrfer  = biron(ji,jj,jk) / ( concfediaz + biron(ji,jj,jk) )
            nitrpot(ji,jj,jk) =  zmudia * r1_rday * zfact * ztrfer * zlight
         ELSE
            zdianh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( concnnh4 + tr(ji,jj,jk,jpnh4,Kbb) )
            zdiano3 = tr(ji,jj,jk,jpno3,Kbb) / ( concnno3 + tr(ji,jj,jk,jpno3,Kbb) ) * (1. - zdianh4)
            zfact   = ( 1. - zdiano3 - zdianh4 ) * rfact2
            ztrfer  = biron(ji,jj,jk) / ( concfediaz + biron(ji,jj,jk) )
            ztrpo4  = tr(ji,jj,jk,jppo4,Kbb) / ( 1E-6 + tr(ji,jj,jk,jppo4,Kbb) )
            IF (ln_p5z) THEN
               ztrdop  = tr(ji,jj,jk,jpdop,Kbb) / ( 1E-6 + tr(ji,jj,jk,jpdop,Kbb) ) * (1. - ztrpo4)
               ztrpo4  = ztrpo4 + ztrdop
            ENDIF
            nitrpot(ji,jj,jk) =  zmudia * r1_rday * zfact * MIN( ztrfer, ztrpo4 ) * zlight
         ENDIF
         zsoufer = zlight * 1.5E-11**2 / ( 1.5E-11**2 + biron(ji,jj,jk)**2 )
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + 0.01 * 4E-10 * zsoufer * rfact2 / rday
      END_3D
      !
      ! Nitrogen change due to nitrogen fixation
      ! ----------------------------------------
      IF( ln_p2z ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zfact = nitrpot(ji,jj,jk) * nitrfix
            !
            tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) + zfact * xtemp13
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - rno3 * zfact * xtemp13
            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zfact * 2.0 * xtemp13
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zfact * xtemp13
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zfact * xtemp13
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - zfact * xtemp13 * feratz
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) + ( o2ut + o2nit ) * zfact
         END_3D
      ELSE
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zfact = nitrpot(ji,jj,jk) * nitrfix
            !
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zfact * xtemp13
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zfact * xtemp13
            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zfact * 2.0 * xtemp13
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zfact * xtemp13
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zfact * 2.0 * xtemp23
            tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) + zfact * xtemp23
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) + ( ( o2ut + o2nit ) * 2.0 + o2nit ) * zfact * xtemp13
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - 30E-6 * zfact * xtemp13
            tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + 30E-6 * zfact * 2.0 * xtemp23
            tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + 30E-6 * zfact * xtemp23
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + concdnh4 / ( concdnh4 + tr(ji,jj,jk,jppo4,Kbb) ) &
                 &                    * 3E-4 * tr(ji,jj,jk,jpdoc,Kbb) * xstep
         END_3D
      ENDIF
      !
      IF( ln_p4z ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zfact = nitrpot(ji,jj,jk) * nitrfix
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - zfact * 2.0 * xtemp13
         END_3D
      ENDIF
      !
      IF( ln_p5z ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            ztrpo4  = tr(ji,jj,jk,jppo4,Kbb) / ( 1E-6 + tr(ji,jj,jk,jppo4,Kbb) )
            ztrdop  = tr(ji,jj,jk,jpdop,Kbb) / ( 1E-6 + tr(ji,jj,jk,jpdop,Kbb) ) * (1. - ztrpo4)
            zratpo4 = ztrpo4 / (ztrpo4 + ztrdop + rtrn)
            !
            zfact = nitrpot(ji,jj,jk) * nitrfix
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - 16.0 / 46.0 * zfact * 2.0 * xtemp13  &
            &                     * zratpo4
            tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zfact * xtemp13
            tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + 16.0 / 46.0 * zfact * xtemp13  &
            &                     - 16.0 / 46.0 * zfact * 2.0 * xtemp13 * (1.0 - zratpo4)
            tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) + zfact * 2.0 * xtemp23
            tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) + 16.0 / 46.0 * zfact * 2.0 * xtemp23
            tr(ji,jj,jk,jpgon,Krhs) = tr(ji,jj,jk,jpgon,Krhs) + zfact * xtemp23
            tr(ji,jj,jk,jpgop,Krhs) = tr(ji,jj,jk,jpgop,Krhs) + 16.0 / 46.0 * zfact * xtemp23
         END_3D
      ENDIF
         
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('diaz')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF

      IF( l_dia_nfix .AND. knt == nrdttrc ) THEN 
!            zfact = rno3 * 1.e+3 * rfact2r !  conversion from molC/l/kt  to molN/m3/s
          CALL iom_put( "Nfix"  , nitrpot(:,:,:) * nitrfix * rno3 * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! diazotrophy
          CALL iom_put( "Nfixo2", nitrpot(:,:,:) * nitrfix * o2nit * rno3 * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! O2 production by diazotrophy
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_diaz')
      !
   END SUBROUTINE p4z_diaz


   SUBROUTINE p4z_diaz_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_diaz_init  ***
      !!
      !! ** Purpose :   Initialization of diazotrophy parameters
      !!
      !! ** Method  :   Read the nampisdiaz namelist and check the parameters
      !!
      !! ** input   :   Namelist nampisdiaz
      !!
      !!----------------------------------------------------------------------
      NAMELIST/nampisdiaz/nitrfix, diazolight, concfediaz
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_diaz_init : Initialization of diazotrophy parameters'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,nampisdiaz)
      READ_NML_CFG(numnatp,nampisdiaz)
      IF(lwm) WRITE( numonp, nampisdiaz )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist parameters for diazotrophy, nampisdiaz'
         WRITE(numout,*) '      nitrogen fixation rate                       nitrfix = ', nitrfix
         WRITE(numout,*) '      nitrogen fixation sensitivty to light    diazolight  = ', diazolight
         IF( .NOT. ln_p2z ) &
           &  WRITE(numout,*) '      Fe half-saturation cste for diazotrophs  concfediaz  = ', concfediaz
      ENDIF
      !
      xtemp13 = 1.0 / 3.0
      xtemp23 = xtemp13 * xtemp13
      !
      r1_rday  = 1. / rday
      nitrpot(:,:,:) = 0._wp
      !
   END SUBROUTINE p4z_diaz_init


   INTEGER FUNCTION p4z_diaz_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_diaz_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( nitrpot(A2D(0),jpk), STAT=p4z_diaz_alloc )
      !
      IF( p4z_diaz_alloc /= 0 )   CALL ctl_stop( 'STOP', 'p4z_diaz_alloc: failed to allocate arrays' )
      !
   END FUNCTION p4z_diaz_alloc

   !!======================================================================
END MODULE p4zdiaz
