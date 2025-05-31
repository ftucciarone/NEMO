MODULE trcstp_rk3
   !!======================================================================
   !!                       ***  MODULE trcstp_rk3  ***
   !! Time-stepping    : time loop of opa for passive tracer
   !!======================================================================
   !! History :  1.0  !  2004-03  (C. Ethe)  Original
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rewrite in preparation for new timestepping scheme
   !!            4.x  !  2021-08  (S. Techene, G. Madec) preparation and finalisation for RK3 time-stepping only
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   trc_stp_start : prepare  passive tracer system time-stepping
   !!   trc_stp_end   : finalise passive tracer system time-stepping
   !!----------------------------------------------------------------------
   USE par_trc        ! need jptra, number of passive tracers
   USE oce_trc        ! ocean dynamics and active tracers variables
   USE sbc_oce
   USE trc
   USE trc_oce , ONLY :   l_offline   ! offline flag
   USE trctrp         ! passive tracers transport
   USE trcbc          ! Tracers boundary condtions          (trc_bc routine)
   USE trcais         ! Antarctic Ice Sheet tracers         (trc_ais routine)
   USE trcsms         ! passive tracers sources and sinks
   USE trcwri
   USE trcrst
   USE trcadv         ! passive tracers advection      (trc_adv routine)
   USE trcsbc         ! passive tracers surface boundary condition !!st WARNING USELESS TO BE REMOVED
   USE trcbdy         ! passive tracers transport open boundary

   USE trdtrc_oce
   USE trdmxl_trc
   !
   USE prtctl         ! Print control for debbuging
   USE iom            !
   USE lib_fortran    ! Fortran routines library
   USE in_out_manager !

   IMPLICIT NONE
   PRIVATE

   PUBLIC  trc_stp_rk3

   LOGICAL  ::   llnew                   ! ???
   LOGICAL  ::   l_trcstat               ! flag for tracer statistics
   REAL(wp) ::   rdt_sampl               ! ???
   INTEGER  ::   nb_rec_per_day, ktdcy   ! ???
   REAL(wp) ::   rsecfst, rseclast       ! ???
   REAL(wp), DIMENSION(:,:,:), SAVE, ALLOCATABLE ::   qsr_arr   ! save qsr during TOP time-step

      !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
  
    SUBROUTINE trc_stp_rk3( kstg, kt, Kbb, Kmm, Krhs, Kaa, pFu, pFv, pFw )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp_start  ***
      !!                      
      !! ** Purpose :   Prepare time loop of opa for passive tracer
      !! 
      !! ** Method  :   Compute the passive tracers trends 
      !!                Update the passive tracers
      !!                Manage restart file
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kstg                        ! RK3 stage
      INTEGER, INTENT( in ) :: kt                  ! ocean time-step index
      INTEGER, INTENT( in ) :: Kbb, Kmm, Krhs, Kaa ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk), OPTIONAL, INTENT(in) ::   pFu, pFv, pFw  ! advective transport
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::   ze3Tb, ze3Tr, z1_e3t     ! local scalars
      CHARACTER (len=25) ::   charout   !
      !!-------------------------------------------------------------------

      IF( ln_timing )   CALL timing_start('trc_stp_rk3')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_stp_rk3 : Runge Kutta 3rd order at stage ', kstg
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF
     !
      SELECT CASE( kstg )
      !                    !-------------------!
      CASE ( 1 , 2 )       !==  Stage 1 & 2  ==!   stg1:  Kbb = N  ;  Kaa = N+1/3
         !                 !-------------------!   stg2:  Kbb = N  ;  Kmm = N+1/3  ;  Kaa = N+1/2
         !
         IF( kstg == 1 ) CALL trc_stp_start( kt, Kbb, Kmm, Krhs, Kaa )
         !
!!st+gm : probably QUICK 
         IF( .NOT. ln_c1d ) THEN
            IF( .NOT.ln_trcadv_mus .AND. .NOT.ln_trcadv_qck ) THEN
              !
              DO jn = 1, jptra
                 tr(:,:,:,jn,Krhs) = 0._wp        ! set tracer trends to zero !!st ::: required because of tra_adv new loops
              END DO
              !                                   !==  advection of passive tracers  ==!
              rDt_trc = rDt
              !
              CALL trc_sbc_RK3( kt, Kbb, Kmm, tr, Krhs, kstg )              ! surface boundary condition
              !
              IF( l_offline ) THEN
                CALL trc_adv ( kt, Kbb, Kmm, Kaa, tr, Krhs ) ! horizontal & vertical advection
              ELSE
                CALL trc_adv ( kt, Kbb, Kmm, Kaa, tr, Krhs, pFu, pFv, pFw ) ! horizontal & vertical advection
              ENDIF
              !
              !                                      !==  time integration  ==!   ∆t = rn_Dt/3 (stg1) or rn_Dt/2 (stg2)
              DO jn = 1, jptra
                 DO_3D( 0, 0, 0, 0, 1, jpkm1 )
                   ze3Tb  = e3t(ji,jj,jk,Kbb) * tr(ji,jj,jk,jn,Kbb )
                   ze3Tr  = e3t(ji,jj,jk,Kmm) * tr(ji,jj,jk,jn,Krhs)
                   z1_e3t = 1._wp / e3t(ji,jj,jk, Kaa)
                   tr(ji,jj,jk,jn,Kaa) = ( ze3Tb + rDt_trc * ze3Tr * tmask(ji,jj,jk) ) * z1_e3t
                END_3D
              END DO
              !
!!st need a lnc lkn at stage 1 & 2 otherwise tr@Kmm will not be usable in trc_adv
              CALL lbc_lnk( 'stprk3_stg', tr(:,:,:,:,Kaa), 'T', 1._wp )
              !
           ENDIF
         ENDIF
         !                 !---------------!
      CASE ( 3 )           !==  Stage 3  ==!   add all RHS terms but advection (=> Kbb only)
         !                 !---------------!
         !
         DO jn = 1, jptra
            tr(:,:,:,jn,Krhs) = 0._wp
         END DO
         !                                         !==  advection of passive tracers  ==!
         rDt_trc = rDt
         !
         IF( ln_trcbc .AND. lltrcbc )  CALL trc_bc ( kt, Kbb, Kmm, tr, Krhs )   ! tracers: surface and lateral Boundary Conditions
         IF( ln_trcais )               CALL trc_ais( kt, Kbb, Kmm, tr, Krhs )   ! tracers from Antarctic Ice Sheet (icb, isf)
         !
         CALL trc_sms    ( kt, Kbb, Kmm, Krhs      )       ! tracers: sinks and sources
         !
         CALL trc_sbc_RK3( kt, Kbb, Kmm, tr, Krhs, kstg )              ! surface boundary condition
         !
         IF( .NOT. ln_c1d ) THEN
           !
           IF( l_offline ) THEN
              CALL trc_adv ( kt, Kbb, Kmm, Kaa, tr, Krhs ) ! horizontal & vertical advection
           ELSE
              CALL trc_adv ( kt, Kbb, Kmm, Kaa, tr, Krhs, pFu, pFv, pFw ) ! horizontal & vertical advection
           ENDIF
           !
         ENDIF
         !
         CALL trc_trp    ( kt, Kbb, Kmm, Krhs, Kaa )       ! transport of passive tracers (without advection)
         !
         CALL trc_stp_end( kt, Kbb, Kmm,       Kaa )
         !
      END SELECT
      !
      IF( ln_timing )   CALL timing_stop('trc_stp_rk3')
      !
   END SUBROUTINE trc_stp_rk3

      !
   SUBROUTINE trc_stp_start( kt, Kbb, Kmm, Krhs, Kaa )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp_start  ***
      !!                      
      !! ** Purpose :   Prepare time loop of opa for passive tracer
      !! 
      !! ** Method  :   Compute the passive tracers trends 
      !!                Update the passive tracers
      !!                Manage restart file
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt                  ! ocean time-step index
      INTEGER, INTENT( in ) :: Kbb, Kmm, Krhs, Kaa ! time level indices
      !
      INTEGER ::   jk, jn   ! dummy loop indices
      CHARACTER (len=25) ::   charout   !
      !!-------------------------------------------------------------------
      IF( ln_timing )   CALL timing_start('trc_stp_start')
      !
      l_trcstat  = ( sn_cfctl%l_trcstat ) .AND. &
           &       ( ( MOD( kt, sn_cfctl%ptimincr ) == 0 ) .OR. ( kt == nitend ) )
      !
      IF( kt == nittrc000 .AND. lk_trdmxl_trc )  CALL trd_mxl_trc_init    ! trends: Mixed-layer
      !
      IF( .NOT.lk_linssh ) THEN                                           ! update ocean volume due to ssh temporal evolution
         DO jk = 1, jpk
            cvol(:,:,jk) = e1e2t(:,:) * e3t(:,:,jk,Kmm) * tmask(:,:,jk)
         END DO
         IF( l_trcstat .OR. kt == nitrst ) areatot = glob_3Dsum( 'trcstp', cvol(:,:,:) )
      ENDIF
      !
      IF( l_trcdm2dc )   CALL trc_mean_qsr( kt )
      !    
      IF(sn_cfctl%l_prttrc) THEN
         WRITE(charout,FMT="('kt =', I4,'  d/m/y =',I2,I2,I4)") kt, nday, nmonth, nyear
         CALL prt_ctl_info( charout, cdcomp = 'top' )
      ENDIF
      !
      CALL trc_rst_opn  ( kt )                            ! Open tracer restart file 
      IF( lrst_trc )  CALL trc_rst_cal  ( kt, 'WRITE' )   ! calendar
      !
      IF( ln_timing )   CALL timing_stop('trc_stp_start')
      !
   END SUBROUTINE trc_stp_start


   SUBROUTINE trc_stp_end( kt, Kbb, Kmm, Kaa )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp_end  ***
      !!                      
      !! ** Purpose :   Finalise time loop of opa for passive tracer
      !! 
      !! ** Method  :   Write restart and outputs 
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt                  ! ocean time-step index
      INTEGER, INTENT( in ) :: Kbb, Kmm, Kaa ! time level indices
      !
      INTEGER ::   jk, jn   ! dummy loop indices
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: z4d
      REAL(wp), ALLOCATABLE, DIMENSION(:) :: ztraa
      CHARACTER (len=25) ::   charout   !
      !!-------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_stp_end')
      !
      !
           ! Note passive tracers have been time-filtered in trc_trp but the time level
           ! indices will not be swapped until after tra_atf/dyn_atf/ssh_atf in stp. Subsequent calls here
           ! anticipate this update which will be: Nrhs= Nbb ; Nbb = Nnn ; Nnn = Naa ; Naa = Nrhs
           ! and use the filtered levels explicitly.
           !
      IF( kt == nittrc000 ) THEN
         CALL iom_close( numrtr )                         ! close input tracer restart file
         IF(lrxios) CALL iom_context_finalize(      cr_toprst_cxt          )
         IF(lwm) CALL FLUSH( numont )                     ! flush namelist output
      ENDIF
      IF( lrst_trc )            CALL trc_rst_wri  ( kt, Kbb, Kmm, Kaa )       ! write tracer restart file
      IF( lk_trdmxl_trc  )      CALL trd_mxl_trc  ( kt,           Kaa )       ! trends: Mixed-layer
      !
      IF ( l_trcstat ) THEN
         !
         ALLOCATE( z4d(jpi,jpj,jpk,jptra), ztraa(jptra) )
         DO jn = 1, jptra
            z4d(:,:,:,jn) = tr(:,:,:,jn,Kaa) * cvol(:,:,:)
         ENDDO
         !
         ztraa(1:jptra) = glob_3Dsum( 'trcstp_rk3', z4d(:,:,:,1:jptra) )
         IF( lwm ) WRITE(numstr,9300) kt,  SUM( ztraa ) / areatot
         !
         DEALLOCATE( z4d, ztraa )
      ENDIF
      !
9300  FORMAT(i10,D23.16)
      !
      CALL trc_wri      ( kt,      Kaa            )       ! output of passive tracers with iom I/O manager before time level swap 
      !
      IF( ln_timing )   CALL timing_stop('trc_stp_end')
      !
   END SUBROUTINE trc_stp_end


   SUBROUTINE trc_mean_qsr( kt )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE trc_mean_qsr  ***
      !!
      !! ** Purpose :  Compute daily mean qsr for biogeochemical model in case
      !!               of diurnal cycle
      !!
      !! ** Method  : store in TOP the qsr every hour ( or every time-step if the latter 
      !!              is greater than 1 hour ) and then, compute the  mean with 
      !!              a moving average over 24 hours. 
      !!              In coupled mode, the sampling is done at every coupling frequency 
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji,jj,jn   ! dummy loop indices
      REAL(wp) ::   zkt, zrec     ! local scalars
      CHARACTER(len=1) ::   cl1   ! 1 character
      CHARACTER(len=2) ::   cl2   ! 2 characters
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_mean_qsr')
      !
      IF( kt == nittrc000 ) THEN
         !
         rdt_sampl = REAL( ncpl_qsr_freq )
         nb_rec_per_day = INT( rday / ncpl_qsr_freq )
         !
         IF(lwp) THEN
            WRITE(numout,*) 
            WRITE(numout,*) ' Sampling frequency dt = ', rdt_sampl, 's','   Number of sampling per day  nrec = ', nb_rec_per_day
            WRITE(numout,*) 
         ENDIF
         !
         ALLOCATE( qsr_arr(A2D(0),nb_rec_per_day ) )
         !
         !                                            !* Restart: read in restart file
         IF( ln_rsttr .AND. nn_rsttr /= 0 .AND. iom_varid( numrtr, 'qsr_mean' , ldstop = .FALSE. ) > 0  &
           &                              .AND. iom_varid( numrtr, 'qsr_arr_1', ldstop = .FALSE. ) > 0  &
           &                              .AND. iom_varid( numrtr, 'ktdcy'    , ldstop = .FALSE. ) > 0  &
           &                              .AND. iom_varid( numrtr, 'nrdcy'    , ldstop = .FALSE. ) > 0  ) THEN
            CALL iom_get( numrtr, 'ktdcy', zkt )  
            rsecfst = INT( zkt ) * rn_Dt
            IF(lwp) WRITE(numout,*) 'trc_qsr_mean:   qsr_mean read in the restart file at time-step rsecfst =', rsecfst, ' s '
            CALL iom_get( numrtr, jpdom_auto, 'qsr_mean', qsr_mean )   !  A mean of qsr
            CALL iom_get( numrtr, 'nrdcy', zrec )   !  Number of record per days
            IF( INT( zrec ) == nb_rec_per_day ) THEN
               DO jn = 1, nb_rec_per_day 
                  IF( jn <= 9 )  THEN
                    WRITE(cl1,'(i1)') jn
                    CALL iom_get( numrtr, jpdom_auto, 'qsr_arr_'//cl1, qsr_arr(:,:,jn) )   !  A mean of qsr
                  ELSE
                    WRITE(cl2,'(i2.2)') jn
                    CALL iom_get( numrtr, jpdom_auto, 'qsr_arr_'//cl2, qsr_arr(:,:,jn) )   !  A mean of qsr
                  ENDIF
              END DO
            ELSE
               DO jn = 1, nb_rec_per_day
                  qsr_arr(:,:,jn) = qsr_mean(:,:)
               END DO
            ENDIF
         ELSE                                         !* no restart: set from nit000 values
            IF(lwp) WRITE(numout,*) 'trc_qsr_mean:   qsr_mean set to nit000 values'
            rsecfst  = kt * rn_Dt
            !
            DO_2D( 0, 0, 0, 0 )
               qsr_mean(ji,jj) = qsr(ji,jj)
            END_2D
            DO jn = 1, nb_rec_per_day
               qsr_arr(:,:,jn) = qsr_mean(:,:)
            END DO
         ENDIF
         !
      ENDIF
      !
      rseclast = kt * rn_Dt
      !
      llnew   = ( rseclast - rsecfst ) >=  rdt_sampl    !   new shortwave to store
      IF( llnew ) THEN
          ktdcy = kt
          IF( lwp .AND. kt < nittrc000 + 100 ) WRITE(numout,*) ' New shortwave to sample for TOP at time kt = ', ktdcy, &
             &                      ' time = ', rseclast/3600.,'hours '
          rsecfst = rseclast
          DO jn = 1, nb_rec_per_day - 1
             qsr_arr(:,:,jn) = qsr_arr(:,:,jn+1)
          END DO
          qsr_arr (:,:,nb_rec_per_day) = qsr(:,:)
          qsr_mean(:,:) = SUM( qsr_arr(:,:,:), 3 ) / nb_rec_per_day
      ENDIF
      !
      IF( lrst_trc ) THEN    !* Write the mean of qsr in restart file 
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_mean_qsr : write qsr_mean in restart file  kt =', kt
         IF(lwp) WRITE(numout,*) '~~~~~~~'
         zkt  = REAL( ktdcy, wp )
         zrec = REAL( nb_rec_per_day, wp )
         CALL iom_rstput( kt, nitrst, numrtw, 'ktdcy', zkt  )
         CALL iom_rstput( kt, nitrst, numrtw, 'nrdcy', zrec )
          DO jn = 1, nb_rec_per_day 
             IF( jn <= 9 )  THEN
               WRITE(cl1,'(i1)') jn
               CALL iom_rstput( kt, nitrst, numrtw, 'qsr_arr_'//cl1, qsr_arr(:,:,jn) )
             ELSE
               WRITE(cl2,'(i2.2)') jn
               CALL iom_rstput( kt, nitrst, numrtw, 'qsr_arr_'//cl2, qsr_arr(:,:,jn) )
             ENDIF
         END DO
         CALL iom_rstput( kt, nitrst, numrtw, 'qsr_mean', qsr_mean(:,:) )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_mean_qsr')
      !
   END SUBROUTINE trc_mean_qsr

#else
   !!----------------------------------------------------------------------
   !!   Default key                                     NO passive tracers
   !!----------------------------------------------------------------------
   USE oce_trc
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_stp_rk3( kstg, kt, Kbb, Kmm, Krhs, Kaa, pFu, pFv, pFw )         ! Empty routine
      INTEGER, INTENT(in   ) ::   kstg                                            ! RK3 stage
      INTEGER, INTENT(in   ) ::   kt                                              ! time-step index
      INTEGER, INTENT(in   ) ::   Kbb, Kmm, Krhs, Kaa                             ! time-level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk), OPTIONAL, INTENT(in) ::   pFu, pFv, pFw   ! advective transport
      WRITE(*,*) 'trc_stp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_stp_rk3
#endif

   !!======================================================================
END MODULE trcstp_rk3
