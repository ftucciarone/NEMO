MODULE p4zsed
   !!======================================================================
   !!                         ***  MODULE p4sed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12 (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06 (C. Ethe) USE of fldread
   !!             3.5  !  2012-07 (O. Aumont) improvment of river input of nutrients 
   !!----------------------------------------------------------------------
   !!   p4z_sed        :  Compute loss of organic matter in the sediments
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zsink         !  Sinking fluxes
   USE sed             !  Sediment module
   USE iom             !  I/O manager
   USE prtctl          !  print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sed  
   PUBLIC   p4z_sed_init
   PUBLIC   p4z_sed_alloc
 
   !
   REAL(wp) :: bureffmin      !: Minimum burial efficiency
   REAL(wp) :: bureffvar      !: Variable coef. for burial efficiency
   !
   REAL(wp) :: sedsilfrac     !: percentage of silica loss in the sediments
   REAL(wp) :: sedcalfrac     !: percentage of calcite loss in the sediments
   REAL(wp) :: sedfactcalmin  !: Minimum value for dissolving calcite at the bottom
   REAL(wp) :: sedfactcalvar  !: Variable  value for dissolving calcite at the bottom
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:  ) :: sdenit     !: Nitrate reduction in the sediments
   !
   LOGICAL  ::  l_dia_sed, l_dia_sdenit

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_sed( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed  ***
      !!
      !! ** Purpose :   Compute loss of organic matter in the sediments. This
      !!              is by no way a sediment model. The loss is simply 
      !!              computed to balance the inout from rivers and dust
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt, knt ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      INTEGER  ::  ji, jj, jk, ikt
      REAL(wp) ::  zbureff, zflx, zflx1
      REAL(wp) ::  zfact, zfactcal
      REAL(wp) ::  zo2, zno3, zpdenit, z1pdenit, zolimit
      REAL(wp) ::  zsiloss, zsiloss2, zcaloss, zdep
      REAL(wp) ::  zwstpoc, zwstpon, zwstpop
      !
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(A2D(0)) :: zdenit2d, zrivno3, zrivalk, zrivsil
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p4z_sed')
      !
      IF( kt == nittrc000 )  THEN
         l_dia_sdenit = iom_use( "Sdenit" )
         l_dia_sed    = iom_use( "SedC" ) .OR. iom_use( "SedCal" ) .OR. iom_use( "SedSi" ) 
      ENDIF
      !
      zdenit2d(:,:) = 0.e0
      zrivno3 (:,:) = 0.e0
      zrivalk (:,:) = 0.e0
      zrivsil (:,:) = 0.e0

      ! Computation of the sediment denitrification proportion: The metamodel from midlleburg (2006) is being used
      ! Computation of the fraction of organic matter that is permanently buried from Dunne's model
      ! -------------------------------------------------------
      DO_2D( 0, 0, 0, 0 )
        IF( tmask(ji,jj,1) == 1 ) THEN
           ikt = mbkt(ji,jj)
           zflx = sinkpocb(ji,jj) / xstep  * 1E6 
           zdep  = gdepw(ji,jj,ikt+1,Kmm) 
           !
           zflx1  = zflx * 1E3 / 1E4
           zflx1  = LOG10( MAX( 1E-3, zflx1 ) )
           zo2    = LOG10( MAX( 10. , tr(ji,jj,ikt,jpoxy,Kbb) * 1E6 ) )
           zno3   = LOG10( MAX( 1.  , tr(ji,jj,ikt,jpno3,Kbb) * 1E6 * rno3 ) )
           zpdenit = -2.2567 - 1.185 * zflx1 - 0.221 * zflx1 * zflx1 - 0.3995 * zno3 * zo2 + 1.25 * zno3    &
             &                + 0.4721 * zo2 - 0.0996 * LOG10(zdep) + 0.4256 * zflx1 * zo2
           zdenit2d(ji,jj) = 10.0**zpdenit
           !
           zflx1 = ( 7.0 + zflx )
           zbureff = bureffmin + bureffvar * zflx * zflx / ( zflx1 * zflx1 ) * MIN( zdep / 1000.00, 1.0 )
           zrivno3(ji,jj) = 1. - zbureff
        ENDIF
      END_2D
      ! This loss is scaled at each bottom grid cell for equilibrating the total budget of silica in the ocean.
      ! Thus, the amount of silica lost in the sediments equal the supply at the surface (dust+rivers)
      ! ------------------------------------------------------
      !
      DO_2D( 0, 0, 0, 0 )
         ikt  = mbkt(ji,jj)
         zdep = 1._wp / e3t(ji,jj,ikt,Kmm)
         zcaloss = sinkcalb(ji,jj) * zdep
         !
         zfactcal = MAX(-0.1, MIN( excess(ji,jj,ikt), 0.2 ) )
         zfactcal = sedfactcalmin + sedfactcalvar * MIN( 1., (0.1 + zfactcal) / ( 0.5 - zfactcal ) )
         zrivalk(ji,jj) = sedcalfrac * zfactcal
         tr(ji,jj,ikt,jptal,Krhs) =  tr(ji,jj,ikt,jptal,Krhs) + zcaloss * zrivalk(ji,jj) * 2.0
         tr(ji,jj,ikt,jpdic,Krhs) =  tr(ji,jj,ikt,jpdic,Krhs) + zcaloss * zrivalk(ji,jj)
      END_2D

      IF( .NOT. ln_p2z ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikt  = mbkt(ji,jj)
            zdep = 1._wp / e3t(ji,jj,ikt,Kmm)
            zsiloss = sinksilb(ji,jj) * zdep
            zsiloss2 = sinksilb(ji,jj) / xstep * 365.0 * 1E3 * 1E-4 * 1E6
            zrivsil(ji,jj) = 1.0 - sedsilfrac * zsiloss2 / ( 15.0 + zsiloss2 )
            tr(ji,jj,ikt,jpsil,Krhs) = tr(ji,jj,ikt,jpsil,Krhs) + zsiloss * zrivsil(ji,jj)
         END_2D
      ENDIF
      ! The 0.5 factor in zpdenit is to avoid negative NO3 concentration after
      ! denitrification in the sediments. Not very clever, but simpliest option.
      IF( ln_p2z ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikt  = mbkt(ji,jj)
            zwstpoc = sinkpocb(ji,jj) / e3t(ji,jj,ikt,Kmm)
            zpdenit  = MIN( 0.5 * ( tr(ji,jj,ikt,jpno3,Kbb) - rtrn ) / rdenit, zdenit2d(ji,jj) * zwstpoc * zrivno3(ji,jj) )
            z1pdenit = zwstpoc * zrivno3(ji,jj) - zpdenit
            zolimit = MIN( ( tr(ji,jj,ikt,jpoxy,Kbb) - rtrn ) / (o2ut + o2nit), z1pdenit * ( 1.- nitrfac(ji,jj,ikt) ) )
            tr(ji,jj,ikt,jpdoc,Krhs) = tr(ji,jj,ikt,jpdoc,Krhs) + z1pdenit - zolimit
            tr(ji,jj,ikt,jpno3,Krhs) = tr(ji,jj,ikt,jpno3,Krhs) + zpdenit + zolimit - rdenit * zpdenit
            tr(ji,jj,ikt,jpoxy,Krhs) = tr(ji,jj,ikt,jpoxy,Krhs) - zolimit * (o2ut + o2nit)
            tr(ji,jj,ikt,jptal,Krhs) = tr(ji,jj,ikt,jptal,Krhs) - rno3 * (zolimit + (1.-rdenit) * zpdenit )
            tr(ji,jj,ikt,jpdic,Krhs) = tr(ji,jj,ikt,jpdic,Krhs) + zpdenit + zolimit
            sdenit(ji,jj) = rdenit * zpdenit * e3t(ji,jj,ikt,Kmm)
         END_2D
      ELSE
         DO_2D( 0, 0, 0, 0 )
            ikt  = mbkt(ji,jj)
            zwstpoc = sinkpocb(ji,jj) / e3t(ji,jj,ikt,Kmm)
            zpdenit  = MIN( 0.5 * ( tr(ji,jj,ikt,jpno3,Kbb) - rtrn ) / rdenit, zdenit2d(ji,jj) * zwstpoc * zrivno3(ji,jj) )
            z1pdenit = zwstpoc * zrivno3(ji,jj) - zpdenit
            zolimit = MIN( ( tr(ji,jj,ikt,jpoxy,Kbb) - rtrn ) / o2ut, z1pdenit * ( 1.- nitrfac(ji,jj,ikt) ) )
            tr(ji,jj,ikt,jpdoc,Krhs) = tr(ji,jj,ikt,jpdoc,Krhs) + z1pdenit - zolimit
            tr(ji,jj,ikt,jppo4,Krhs) = tr(ji,jj,ikt,jppo4,Krhs) + zpdenit + zolimit
            tr(ji,jj,ikt,jpnh4,Krhs) = tr(ji,jj,ikt,jpnh4,Krhs) + zpdenit + zolimit
            tr(ji,jj,ikt,jpno3,Krhs) = tr(ji,jj,ikt,jpno3,Krhs) - rdenit * zpdenit
            tr(ji,jj,ikt,jpoxy,Krhs) = tr(ji,jj,ikt,jpoxy,Krhs) - zolimit * o2ut
            tr(ji,jj,ikt,jptal,Krhs) = tr(ji,jj,ikt,jptal,Krhs) + rno3 * (zolimit + (1.+rdenit) * zpdenit )
            tr(ji,jj,ikt,jpdic,Krhs) = tr(ji,jj,ikt,jpdic,Krhs) + zpdenit + zolimit 
            sdenit(ji,jj) = rdenit * zpdenit * e3t(ji,jj,ikt,Kmm)
         END_2D
      ENDIF
      IF( ln_p5z ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikt  = mbkt(ji,jj)
            zdep = 1._wp / e3t(ji,jj,ikt,Kmm)
            zwstpoc = sinkpocb(ji,jj) * zdep
            zwstpop = sinkpopb(ji,jj) * zdep
            zwstpon = sinkponb(ji,jj) * zdep
            zpdenit  = MIN( 0.5 * ( tr(ji,jj,ikt,jpno3,Kbb) - rtrn ) / rdenit, zdenit2d(ji,jj) * zwstpoc * zrivno3(ji,jj) )
            z1pdenit = zwstpoc * zrivno3(ji,jj) - zpdenit
            zolimit = MIN( ( tr(ji,jj,ikt,jpoxy,Kbb) - rtrn ) / o2ut, z1pdenit * ( 1.- nitrfac(ji,jj,ikt) ) )
            tr(ji,jj,ikt,jpdon,Krhs) = tr(ji,jj,ikt,jpdon,Krhs) + ( z1pdenit - zolimit ) * zwstpon / (zwstpoc + rtrn)
            tr(ji,jj,ikt,jpdop,Krhs) = tr(ji,jj,ikt,jpdop,Krhs) + ( z1pdenit - zolimit ) * zwstpop / (zwstpoc + rtrn)
         END_2D
      ENDIF
      IF( knt == nrdttrc ) THEN
!            zfact = 1.e+3 * rfact2r !  conversion from molC/l/kt  to molC/m3/s
          IF( l_dia_sdenit ) CALL iom_put( "Sdenit", sdenit(:,:) *  rno3 * 1.e+3 * rfact2r )
          IF( l_dia_sed ) THEN
               CALL iom_put( "SedCal", ( 1.0 - zrivalk(:,:) ) * sinkcalb(:,:) * 1.e+3 * rfact2r )
               CALL iom_put( "SedC"  , ( 1.0 - zrivno3(:,:) ) * sinkpocb(:,:) * 1.e+3 * rfact2r )
            IF( .NOT. ln_p2z )  &
             & CALL iom_put( "SedSi" , ( 1.0 - zrivsil(:,:) ) * sinksilb(:,:) * 1.e+3 * rfact2r )
          ENDIF
          !
      ENDIF
      !
      IF(sn_cfctl%l_prttrc) THEN  ! print mean trneds (USEd for debugging)
         WRITE(charout, fmt="('sed ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('p4z_sed')
      !
   END SUBROUTINE p4z_sed

   SUBROUTINE p4z_sed_init
      !!----------------------------------------------------------------------
      !!                  ***  routine p4z_sed_init  ***
      !!
      !! ** purpose :   initialization of some parameters
      !!
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      INTEGER  :: ios                 ! Local integer output status for namelist read
      !
      !!
      NAMELIST/nampissed/bureffmin, bureffvar, &
         &               sedsilfrac, sedcalfrac, sedfactcalmin, sedfactcalvar
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_sed_init : initialization of sediment mobilisation '
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      !                            !* set file information
      READ_NML_REF(numnatp,nampissed)
      READ_NML_CFG(numnatp,nampissed)
      IF(lwm) WRITE ( numonp, nampissed )

      IF(lwp) THEN
         WRITE(numout,*) '      Minimum burial efficiency                           bureffmin      = ', bureffmin
         WRITE(numout,*) '      Variable coef. for burial efficiency                bureffvar      = ', bureffvar
         WRITE(numout,*) '      percentage of silica loss in the sediments          sedsilfrac     = ', sedsilfrac
         WRITE(numout,*) '      percentage of calcite loss in the sediments         sedcalfrac     = ', sedcalfrac
         WRITE(numout,*) '      Minimum value for dissolving calcite at the bottom  sedfactcalmin  = ', sedfactcalmin
         WRITE(numout,*) '      variable value for dissolving calcite at the bottom sedfactcalvar  = ', sedfactcalvar
      ENDIF
      !
      lk_sed = ln_sediment .AND. ln_sed_2way 
      !
      sdenit(:,:) = 0._wp
      !
   END SUBROUTINE p4z_sed_init


   INTEGER FUNCTION p4z_sed_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( sdenit(A2D(0)), STAT=p4z_sed_alloc )
      !
      IF( p4z_sed_alloc /= 0 )   CALL ctl_stop( 'STOP', 'p4z_sed_alloc: failed to allocate arrays' )
      !
   END FUNCTION p4z_sed_alloc
   !!======================================================================
END MODULE p4zsed
