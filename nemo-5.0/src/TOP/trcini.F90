MODULE trcini
   !!======================================================================
   !!                         ***  MODULE trcini  ***
   !! TOP :   Manage the passive tracer initialization
   !!======================================================================
   !! History :   -   ! 1991-03 (O. Marti)  original code
   !!            1.0  ! 2005-03 (O. Aumont, A. El Moussaoui) F90
   !!            2.0  ! 2005-10 (C. Ethe, G. Madec) revised architecture
   !!            4.0  ! 2011-01 (A. R. Porter, STFC Daresbury) dynamical allocation
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_init    :   Initialization for passive tracer
   !!   top_alloc   :   allocate the TOP arrays
   !!   top_dealloc :   deallocate the TOP arrays
   !!----------------------------------------------------------------------
   USE par_trc         ! need jptra, number of passive tracers
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables
   USE trcnam          ! Namelist read
   USE daymod          ! calendar manager
   USE prtctl          ! Print control passive tracers (prt_ctl_init routine)
   USE trcrst
   USE lib_mpp         ! distribued memory computing library
   USE lib_fortran     ! Fortran routines library
   USE trcice          ! tracers in sea ice
   USE trcbc           ! generalized Boundary Conditions
   USE trcais          ! tracers from Antartic Ice Sheet
   USE trcbdy          ! passive-tracer open boundary conditions

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   trc_init      ! called by nemogcm.F90
   PUBLIC   top_dealloc   ! called by nemogcm.F90

      !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   
   SUBROUTINE trc_init( Kbb, Kmm, Kaa )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_init  ***
      !!
      !! ** Purpose :   Initialization of the passive tracer fields 
      !!
      !! ** Method  : - read namelist
      !!              - control the consistancy 
      !!              - compute specific initialisations
      !!              - set initial tracer fields (either read restart 
      !!                or read data or analytical formulation
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: Kbb, Kmm, Kaa   ! time level indices
      !
      IF( ln_timing )   CALL timing_start('trc_init')
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'trc_init : initial set up of the passive tracers'
      IF(lwp) WRITE(numout,*) '~~~~~~~~'
      !
      CALL trc_nam       ! read passive tracers namelists
      CALL top_alloc()   ! allocate TOP arrays

      !
      IF(.NOT.ln_trcdta )   ln_trc_ini(:) = .FALSE.
      !
      IF(lwp) WRITE(numout,*)
      IF( ln_rsttr .AND. .NOT. l_offline ) CALL trc_rst_cal( nit000, 'READ' )   ! calendar
      IF(lwp) WRITE(numout,*)
      !
      CALL trc_ini_sms( Kbb )   ! SMS
      CALL trc_ini_trp          ! passive tracers transport
      CALL trc_ice_ini          ! Tracers in sea ice
      !
      IF( lwm .AND. sn_cfctl%l_trcstat ) THEN
         CALL ctl_opn( numstr, 'tracer.stat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp , narea )
      ENDIF
      !
      CALL trc_ini_state( Kbb, Kmm, Kaa )  !  passive tracers initialisation : from a restart or from clim
      !
      CALL trc_ini_inv( Kmm )              ! Inventories
      !
      IF( ln_timing )   CALL timing_stop('trc_init')
      !
   END SUBROUTINE trc_init


   SUBROUTINE trc_ini_inv( Kmm )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ini_stat  ***
      !! ** Purpose :      passive tracers inventories at initialsation phase
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kmm    ! time level index
      INTEGER             ::  ji, jj, jk, jn  ! dummy loop indices
      CHARACTER (len=25) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: z4d
      REAL(wp), ALLOCATABLE, DIMENSION(:) :: ztra

      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'trc_ini_inv : initial passive tracers inventories'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      !
      ALLOCATE( z4d(jpi,jpj,jpk,jptra+1), ztra(jptra+1) )
      !                          ! masked grid volume
      DO jk = 1, jpk
         cvol(:,:,jk) = e1e2t(:,:) * e3t(:,:,jk,Kmm) * tmask(:,:,jk)
      END DO
      ! 
      DO jn = 1, jptra
         z4d(:,:,:,jn) = tr(:,:,:,jn,Kmm) * cvol(:,:,:)
      ENDDO
      z4d(:,:,:,jptra+1)  = cvol(:,:,:) 
      !
      ztra(1:jptra+1) = glob_3Dsum( 'trcini', z4d(:,:,:,1:jptra+1) )
      !
      trai(1:jptra) = ztra(1:jptra)      !  initial content of all tracers
      areatot       = ztra(jptra+1)      ! total volume of the ocean 
      !
      IF(lwp) THEN               ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   ==>>>   Total number of passive tracer jptra = ', jptra
         WRITE(numout,*) '           Total volume of ocean                = ', areatot
         WRITE(numout,*) '           Total inital content of all tracers '
         WRITE(numout,*)
         DO jn = 1, jptra
            WRITE(numout,9000) jn, TRIM( ctrcnm(jn) ), trai(jn)
         ENDDO
         WRITE(numout,*)
      ENDIF
      IF(lwp) WRITE(numout,*)
      IF(sn_cfctl%l_prttrc) THEN            ! print mean trends (used for debugging)
         CALL prt_ctl_init( 'top', jptra )
         WRITE(charout, FMT="('ini ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl( tab4d_1=tr(:,:,:,:,Kmm), mask1=tmask, clinfo=ctrcnm )
      ENDIF
9000  FORMAT('      tracer nb : ',i2,'      name :',a10,'      initial content :',e18.10)
      !
      DEALLOCATE( z4d, ztra )
      !
   END SUBROUTINE trc_ini_inv


   SUBROUTINE trc_ini_sms( Kbb )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ini_sms  ***
      !! ** Purpose :   SMS initialisation
      !!----------------------------------------------------------------------
      USE trcini_pisces  ! PISCES   initialisation
      USE trcini_cfc     ! CFC      initialisation
      USE trcini_c14     ! C14  initialisation
      USE trcini_age     ! age initialisation
      USE trcini_my_trc  ! MY_TRC   initialisation
      !
      INTEGER, INTENT(in) ::   Kbb ! time level indices
      INTEGER :: jn
      !!----------------------------------------------------------------------
      !
      ! Pass sn_tracer fields to specialized arrays 
      DO jn = 1, jp_bgc
         ctrcnm    (jn) = TRIM( sn_tracer(jn)%clsname )
         ctrcln    (jn) = TRIM( sn_tracer(jn)%cllname )
         ctrcun    (jn) = TRIM( sn_tracer(jn)%clunit  )
         ln_trc_ini(jn) =       sn_tracer(jn)%llinit
         ln_trc_sbc(jn) =       sn_tracer(jn)%llsbc
         ln_trc_cbc(jn) =       sn_tracer(jn)%llcbc
         ln_trc_obc(jn) =       sn_tracer(jn)%llobc
         ln_trc_ais(jn) =       sn_tracer(jn)%llais
      END DO
      !
      IF( .NOT.ln_trcbc ) THEN
         DO jn = 1, jp_bgc
            ln_trc_sbc(jn) = .FALSE.
            ln_trc_cbc(jn) = .FALSE.
            ln_trc_obc(jn) = .FALSE.
         END DO
      ENDIF
     
      lltrcbc = ( COUNT(ln_trc_sbc) + COUNT(ln_trc_obc) + COUNT(ln_trc_cbc) ) > 0 
      !    
      IF( ln_pisces      )   CALL trc_ini_pisces( Kbb )     !  PISCES model
      IF( ln_my_trc      )   CALL trc_ini_my_trc( Kbb )     !  MY_TRC model
      IF( ll_cfc         )   CALL trc_ini_cfc   ( Kbb )     !  CFC's
      IF( ln_c14         )   CALL trc_ini_c14   ( Kbb )     !  C14 model
      IF( ln_age         )   CALL trc_ini_age   ( Kbb )     !  AGE
      !
      IF(lwp) THEN                   ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_init_sms : Summary for selected passive tracers'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '    ID     NAME     INI  SBC  CBC  OBC  AIS'
         DO jn = 1, jptra
            WRITE(numout,9001) jn, TRIM(ctrcnm(jn)), ln_trc_ini(jn),ln_trc_sbc(jn),ln_trc_cbc(jn),ln_trc_obc(jn),ln_trc_ais(jn)
         END DO
      ENDIF
      IF( lwp .AND. ln_trcbc .AND. lltrcbc ) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' Applying tracer boundary conditions '
      ENDIF
      !
      IF( lwp .AND. ln_trcais ) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' Applying tracer from Antarctic Ice Sheet '
      ENDIF
     
9001  FORMAT(3x,i3,1x,a10,3x,l2,3x,l2,3x,l2,3x,l2,3x,l2)
      !
   END SUBROUTINE trc_ini_sms


   SUBROUTINE trc_ini_trp
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ini_trp  ***
      !!
      !! ** Purpose :   Allocate all the dynamic arrays of the OCE modules
      !!----------------------------------------------------------------------
      USE trcdmp , ONLY:  trc_dmp_ini
      USE trcadv , ONLY:  trc_adv_ini
      USE trcldf , ONLY:  trc_ldf_ini
      USE trcrad , ONLY:  trc_rad_ini
      USE trcsink, ONLY:  trc_sink_ini
      !
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      IF( ln_trcdmp )  CALL  trc_dmp_ini          ! damping
                       CALL  trc_adv_ini          ! advection
                       CALL  trc_ldf_ini          ! lateral diffusion
                       !                          ! vertical diffusion: always implicit time stepping scheme
                       CALL  trc_rad_ini          ! positivity of passive tracers 
                       CALL  trc_sink_ini         ! Vertical sedimentation of particles
      !
   END SUBROUTINE trc_ini_trp


   SUBROUTINE trc_ini_state( Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ini_state ***
      !! ** Purpose :          Initialisation of passive tracer concentration 
      !!----------------------------------------------------------------------
      USE trcrst          ! passive tracers restart
      USE trcdta          ! initialisation from files
      !
      INTEGER, INTENT(in) :: Kbb, Kmm, Kaa   ! time level index
      INTEGER             :: jn              ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( ln_trcdta )   CALL trc_dta_ini( jptra )           ! set initial tracers values
      !
      tr(:,:,:,:,Kaa) = 0._wp
      !
      IF( ln_rsttr ) THEN              ! restart from a file
        !
        CALL trc_rst_read( Kbb, Kmm )
        !
      ELSE                             ! Initialisation of tracer from a file that may also be used for damping
        IF( ln_trcdta .AND. nb_trcdta > 0 ) THEN
            ! update passive tracers arrays with input data read from file
            DO jn = 1, jptra
               IF( ln_trc_ini(jn) ) THEN
                  CALL trc_dta( nit000, jn, tr(:,:,:,jn,Kbb) )
               ENDIF
            END DO
            !
        ENDIF
        !
        tr(:,:,:,:,Kmm) = tr(:,:,:,:,Kbb)
        ! 
      ENDIF
      !
      IF( ln_bdy ) CALL trc_bdy_ini( jptra )
      !
      IF( ln_trcbc .AND. lltrcbc ) CALL trc_bc_ini ( jptra, Kbb  )      ! set tracers Boundary Conditions
      !
      IF( ln_trcais ) CALL trc_ais_ini   ! set tracers from Antarctic Ice Sheet
      !                                                         ! Partial top/bottom cell: GRADh(tr(Kmm))
   END SUBROUTINE trc_ini_state


   SUBROUTINE top_alloc
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE top_alloc  ***
      !!
      !! ** Purpose :   Allocate all the dynamic arrays of the OCE modules
      !!----------------------------------------------------------------------
      USE trdtrc_oce    , ONLY:   trd_trc_oce_alloc
#if defined key_trdmxl_trc 
      USE trdmxl_trc    , ONLY:   trd_mxl_trc_alloc
#endif
      !
      INTEGER ::   ierr   ! local integer
      !!----------------------------------------------------------------------
      !
      ierr =        trc_alloc()
      ierr = ierr + trd_trc_oce_alloc()
#if defined key_trdmxl_trc 
      ierr = ierr + trd_mxl_trc_alloc()
#endif
      !
      CALL mpp_sum( 'trcini', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'top_alloc : unable to allocate standard ocean arrays' )
      !
   END SUBROUTINE top_alloc


   SUBROUTINE top_dealloc()
      USE trdtrc_oce    , ONLY:   trd_trc_oce_dealloc
#if defined key_trdmxl_trc 
      USE trdmxl_trc    , ONLY:   trd_mxl_trc_dealloc
#endif
      CALL trc_dealloc()  
      CALL trd_trc_oce_dealloc()
#if defined key_trdmxl_trc 
      CALL trd_mxl_trc_dealloc()
#endif
   END SUBROUTINE top_dealloc
   
   
#else
   !!----------------------------------------------------------------------
   !!  Empty module :                                     No passive tracer
   !!----------------------------------------------------------------------
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_init                      ! Dummy routine   
   END SUBROUTINE trc_init
#endif

   !!======================================================================
END MODULE trcini
