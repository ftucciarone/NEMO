MODULE p4zsink
   !!======================================================================
   !!                         ***  MODULE p4zsink  ***
   !! TOP :  PISCES  vertical flux of particulate matter due to 
   !!        gravitational sinking
   !!        This module is the same for both PISCES and PISCES-QUOTA
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Change aggregation formula
   !!             3.5  !  2012-07  (O. Aumont) Introduce potential time-splitting
   !!             4.0  !  2019     (O. Aumont) an external subroutine is called
   !!                                          to compute the impact of sinking
   !!----------------------------------------------------------------------
   !!   p4z_sink       :  Compute vertical flux of particulate matter due to gravitational sinking
   !!   p4z_sink_init  :  Unitialisation of sinking speed parameters
   !!   p4z_sink_alloc :  Allocate sinking speed variables
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE trcsink         !  General routine to compute sedimentation
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager
   USE lib_mpp
   USE lib_fortran     ! Fortran routines library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sink         ! called in p4zbio.F90
   PUBLIC   p4z_sink_init    ! called in trcini_pisces.F90
   PUBLIC   p4z_sink_alloc   ! called in trcini_pisces.F90

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  sinkpocb  !: POC sinking fluxes at bottom 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  sinkcalb  !: CaCO3 sinking fluxes at bottom
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  sinksilb  !: BSi sinking fluxes at bottom
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  sinkponb  !: POC sinking fluxes at bottom 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  sinkpopb  !: POC sinking fluxes at bottom 
   INTEGER, PUBLIC, SAVE :: ik100

   REAL(wp) :: xfact
   LOGICAL  :: l_dia_sink, l_diag

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_sink ( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sink  ***
      !!
      !! ** Purpose :   Compute vertical flux of particulate matter due to 
      !!                gravitational sinking. 
      !!
      !! ** Method  : - An external advection subroutine is called to compute
      !!                the impact of sinking on the particles. The tracers
      !!                concentrations are updated in this subroutine which
      !!                is mandatory to deal with negative concentrations
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt, knt
      INTEGER, INTENT(in) :: Kbb, Kmm, Krhs  ! time level indices
      INTEGER  ::   ji, jj, jk, ikb
      CHARACTER (len=25) :: charout
      REAL(wp) :: zmax, zfact
      REAL(wp), DIMENSION(A2D(0),jpk) :: zsinking, zsinking2
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_sink')

      IF( kt == nittrc000 )  THEN
         l_dia_sink = iom_use( "EPC100" ) .OR. iom_use( "EPFE100" ) .OR. iom_use( "EPCAL100" ) .OR. iom_use( "EPSI100" )  &
           &    .OR.  iom_use( "EXPC"   ) .OR. iom_use( "EXPFE"   ) .OR. iom_use( "EXPCAL"   ) .OR. iom_use( "EXPSI"   )  &
           &    .OR.  iom_use( "tcexp"  ) 
         !
         xfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
      ENDIF
      l_diag = l_dia_sink .AND. knt == nrdttrc

      ! Initialize the local arrays
      zsinking(:,:,:) = 0._wp ; zsinking2(:,:,:) = 0._wp

      ! Sinking speeds of big detritus is increased with depth as shown
      ! by data and from the coagulation theory. This is controled by
      ! wsbio2max and wsbio2scale. If wsbio2max is set to wsbio2, then
      ! sinking speed is constant with depth.
      ! CaCO3 and bSi are supposed to sink at the big particles speed 
      ! due to their high density
      ! ---------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zmax  = MAX( heup_01(ji,jj), hmld(ji,jj) )
         zfact = MAX( 0., gdepw(ji,jj,jk+1,Kmm) - zmax ) / wsbio2scale
         wsbio4(ji,jj,jk) = wsbio2 + MAX(0., ( wsbio2max - wsbio2 )) * zfact
      END_3D

      wsbio4(:,:,jpk) = wsbio2

      ! Sinking speed of the small particles is always constant
      ! except in PISCES reduced
      IF( ln_p2z ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zmax  = MAX( heup_01(ji,jj), hmld(ji,jj) )
            zfact = MAX( 0., gdepw(ji,jj,jk+1,Kmm) - zmax ) * tgfunc(ji,jj,jk) * ( 0.03 / wsbio2 - 0.015/ wsbio)
            zfact = MIN(3.0, EXP(-zfact) )
            wsbio3(ji,jj,jk) = ( wsbio + wsbio2 * ( sizen(ji,jj,1) - 1.0 ) * 0.05 * zfact )    &
               &               / ( 1.0 + ( sizen(ji,jj,1) - 1.0 ) * 0.05 * zfact )
         END_3D
         wsbio3(:,:,jpk) = wsbio
      ELSE
         wsbio3(:,:,:) = wsbio
      ENDIF

      ! Compute the sedimentation term using trc_sink for all the sinking particles
      ! ---------------------------------------------------------------------------
      !
      CALL trc_sink( kt, Kbb, Kmm, wsbio3, zsinking , jppoc, rfact2 )
      IF( .NOT. ln_p2z ) THEN
         CALL trc_sink( kt, Kbb, Kmm, wsbio4, zsinking2, jpgoc, rfact2 )
      ENDIF
      DO_2D( 0, 0, 0, 0 )
         ikb = mbkt(ji,jj)+1
         sinkpocb(ji,jj) = zsinking(ji,jj,ikb) + zsinking2(ji,jj,ikb)
      END_2D
      !
      IF( l_diag ) THEN
         t_oce_co2_exp = glob_2Dsum( 'p4zsink', ( zsinking(A2D(0),ik100) + zsinking2(A2D(0),ik100) ) &
            &                                    * xfact * e1e2t(A2D(0)) * tmask(A2D(0),ik100), cdelay = 'co2exp' )
         CALL iom_put( "EPC100", ( zsinking(A2D(0),ik100) + zsinking2(A2D(0),ik100) ) * xfact * tmask(A2D(0),ik100) )  ! Export of carbon at 100m
         CALL iom_put( "EXPC"  , ( zsinking(A2D(0),:)     + zsinking2(A2D(0),:) )     * xfact * tmask(A2D(0),:) )      ! Export of carbon in the water column
         CALL iom_put( "tcexp" , t_oce_co2_exp )      ! Total cabon exort
      ENDIF
      !
      IF( .NOT. ln_p2z ) THEN
         !
         CALL trc_sink( kt, Kbb, Kmm, wsbio4, zsinking, jpcal, rfact2 )
         DO_2D( 0, 0, 0, 0 )
            ikb = mbkt(ji,jj)+1
            sinkcalb(ji,jj) = zsinking(ji,jj,ikb)
         END_2D
         IF( l_diag ) THEN
            CALL iom_put( "EPCAL100",  zsinking(A2D(0),ik100) * xfact * tmask(A2D(0),ik100) )  ! Export of calcite at 100m
            CALL iom_put( "EXPCAL"  ,  zsinking(A2D(0),:)     * xfact * tmask(A2D(0),:) )             ! Export of calcite in the water column
         ENDIF

         CALL trc_sink( kt, Kbb, Kmm, wsbio4, zsinking, jpgsi, rfact2 )
         DO_2D( 0, 0, 0, 0 )
            ikb = mbkt(ji,jj)+1
            sinksilb(ji,jj) = zsinking(ji,jj,ikb)
         END_2D
         IF( l_diag ) THEN
            CALL iom_put( "EPSI100", zsinking(A2D(0),ik100) * xfact * tmask(A2D(0),ik100) )  ! Export of Silicate at 100m
            CALL iom_put( "EXPSI"  , zsinking(A2D(0),:)     * xfact * tmask(A2D(0),:)   )             ! Export of Silicate in the water column
         ENDIF
         !
         CALL trc_sink( kt, Kbb, Kmm, wsbio3, zsinking , jpsfe, rfact2 )
         CALL trc_sink( kt, Kbb, Kmm, wsbio4, zsinking2, jpbfe, rfact2 )
         IF( l_diag ) THEN
            CALL iom_put( "EPFE100", ( zsinking(A2D(0),ik100) + zsinking2(A2D(0),ik100) ) * xfact * tmask(A2D(0),ik100) )  ! Export of iron at 100m
            CALL iom_put( "EXPFE"  , ( zsinking(A2D(0),:)     + zsinking2(A2D(0),:) )     * xfact * tmask(A2D(0),:) )      ! Export of iron in the water column
         ENDIF
      ENDIF
      !
      ! PISCES-QUOTA part
      IF( ln_p5z ) THEN
         !
         CALL trc_sink( kt, Kbb, Kmm, wsbio3, zsinking , jppon, rfact2 )
         CALL trc_sink( kt, Kbb, Kmm, wsbio4, zsinking2, jpgon, rfact2 )
         DO_2D( 0, 0, 0, 0 )
            ikb = mbkt(ji,jj)+1
            sinkponb(ji,jj) = zsinking(ji,jj,ikb) + zsinking2(ji,jj,ikb)
         END_2D
         !
         CALL trc_sink( kt, Kbb, Kmm, wsbio3, zsinking , jppop, rfact2 )
         CALL trc_sink( kt, Kbb, Kmm, wsbio4, zsinking2, jpgop, rfact2 )
         DO_2D( 0, 0, 0, 0 )
            ikb = mbkt(ji,jj)+1
            sinkpopb(ji,jj) = zsinking(ji,jj,ikb) + zsinking2(ji,jj,ikb)
         END_2D
         !
      ENDIF
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('sink')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Kbb), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_sink')
      !
   END SUBROUTINE p4z_sink


   SUBROUTINE p4z_sink_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_sink_init  ***
      !!
      !! ** Purpose :   Initialization of sinking parameters
      !!
      !! ** Method  :   
      !!
      !! ** input   :   
      !!----------------------------------------------------------------------
      INTEGER :: jk
      !!----------------------------------------------------------------------
      !
      ik100 = 10        !  last level where depth less than 100 m
      DO jk = jpkm1, 1, -1
         IF( gdept_1d(jk) > 100. )  ik100 = jk - 1
      END DO
      IF (lwp) WRITE(numout,*)
      IF (lwp) WRITE(numout,*) ' Level corresponding to 100m depth ',  ik100 + 1
      IF (lwp) WRITE(numout,*)
      !
   END SUBROUTINE p4z_sink_init

   INTEGER FUNCTION p4z_sink_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sink_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr(3)
      !!----------------------------------------------------------------------
      !
      ierr(:) = 0
      !
      ALLOCATE( sinkpocb(A2D(0)), sinkcalb(A2D(0)), STAT=ierr(1) )
      !
      IF( .NOT. ln_p2z ) THEN
         ALLOCATE( sinksilb(A2D(0)), STAT=ierr(2) )                
         !
         IF( ln_p5z ) ALLOCATE( sinkponb(A2D(0)), sinkpopb(A2D(0)), STAT=ierr(3) )
      ENDIF
      !
      p4z_sink_alloc = MAXVAL( ierr )
      IF( p4z_sink_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p4z_sink_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p4z_sink_alloc
   
   !!======================================================================
END MODULE p4zsink
