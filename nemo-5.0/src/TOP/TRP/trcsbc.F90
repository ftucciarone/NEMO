MODULE trcsbc
   !!==============================================================================
   !!                       ***  MODULE  trcsbc  ***
   !! Ocean passive tracers:  surface boundary condition
   !!======================================================================
   !! History :  8.2  !  1998-10  (G. Madec, G. Roullet, M. Imbard)  Original code
   !!            8.2  !  2001-02  (D. Ludicone)  sea ice and free surface
   !!            8.5  !  2002-06  (G. Madec)  F90: Free form and module
   !!            9.0  !  2004-03  (C. Ethe)  adapted for passive tracers
   !!                 !  2006-08  (C. Deltel) Diagnose ML trends for passive tracers
   !!==============================================================================
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_sbc      : update the tracer trend at ocean surface
   !!----------------------------------------------------------------------
   USE par_trc        ! need jptra, number of passive tracers
   USE oce_trc        ! ocean dynamics and active tracers variables
   USE trc            ! ocean  passive tracers variables
   USE prtctl         ! Print control for debbuging
   USE iom
   USE trd_oce
   USE trdtra

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sbc       ! routine called by trctrp.F90
   PUBLIC   trc_sbc_RK3   ! routine called by stprk3_stg.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sbc ( kt, Kmm, ptr, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_sbc  ***
      !!                   
      !! ** Purpose :   Compute the tracer surface boundary condition trend of
      !!      (concentration/dilution effect) and add it to the general 
      !!       trend of tracer equations.
      !!
      !! ** Method :
      !!      * concentration/dilution effect:
      !!            The surface freshwater flux modify the ocean volume
      !!         and thus the concentration of a tracer as :
      !!            tr(Krhs) = tr(Krhs) + emp * tr(Kmm) / e3t_ + fwfice * tri / e3t   for k=1
      !!          - tr(Kmm) , the concentration of tracer in the ocean
      !!          - tri, the concentration of tracer in the sea-ice
      !!          - emp, the surface freshwater budget (evaporation minus precipitation + fwfice)
      !!            given in kg/m2/s is divided by 1035 kg/m3 (density of ocean water) to obtain m/s.
      !!          - fwfice, the flux asscociated to freezing-melting of sea-ice 
      !!            In linear free surface case (lk_linssh=T), the volume of the
      !!            ocean does not change with the water exchanges at the (air+ice)-sea
      !!
      !! ** Action  : - Update the 1st level of tr(:,:,:,:,Krhs) with the trend associated
      !!                with the tracer surface boundary condition 
      !!
      !!----------------------------------------------------------------------
      INTEGER,                                    INTENT(in   ) :: kt        ! ocean time-step index
      INTEGER,                                    INTENT(in   ) :: Kmm, Krhs ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) :: ptr       ! passive tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jn                      ! dummy loop indices
      REAL(wp) ::   zse3t, zrtrn, zfact     ! local scalars
      REAL(wp) ::   zdtra          !   -      -
      CHARACTER (len=22) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrtrd
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sbc')
      !
      ! Allocate temporary workspace
      IF( l_trdtrc )  ALLOCATE( ztrtrd(T2D(0),jpk) )
      !
      zrtrn = 1.e-15_wp

      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_sbc : Passive tracers surface boundary condition'
         IF(lwp) WRITE(numout,*) '~~~~~~~ '
         !
         IF( ln_rsttr .AND. .NOT.ln_top_euler .AND.   &                     ! Restart: read in restart  file
            iom_varid( numrtr, 'sbc_'//TRIM(ctrcnm(1))//'_b', ldstop = .FALSE. ) > 0 ) THEN
            IF(lwp) WRITE(numout,*) '          nittrc000-1 surface tracer content forcing fields read in the restart file'
            zfact = 0.5_wp
            DO jn = 1, jptra
               CALL iom_get( numrtr, jpdom_auto, 'sbc_'//TRIM(ctrcnm(jn))//'_b', sbc_trc_b(:,:,jn) )   ! before tracer content sbc
            END DO
         ELSE                                         ! No restart or restart not found: Euler forward time stepping
           zfact = 1._wp
           sbc_trc_b(:,:,:) = 0._wp
         ENDIF
      ELSE                                         ! Swap of forcing fields
         IF( ln_top_euler ) THEN
            zfact = 1._wp
            sbc_trc_b(:,:,:) = 0._wp
         ELSE
            zfact = 0.5_wp
            sbc_trc_b(:,:,:) = sbc_trc(:,:,:)
         ENDIF
         !
      ENDIF

      ! 0. initialization
      SELECT CASE ( nn_ice_tr )

      CASE ( -1 ) ! ! No tracers in sea ice ( trc_i = 0 )
         !
         DO jn = 1, jptra
            DO_2D( 0, 0, 0, 0 )
               sbc_trc(ji,jj,jn) = 0._wp
            END_2D
         END DO
         !
         IF( lk_linssh ) THEN  !* linear free surface  
            DO jn = 1, jptra
               DO_2D( 0, 0, 0, 0 )
                  sbc_trc(ji,jj,jn) = sbc_trc(ji,jj,jn) + r1_rho0 * emp(ji,jj) * ptr(ji,jj,1,jn,Kmm) !==>> add concentration/dilution effect due to constant volume cell
               END_2D
            END DO
         ENDIF
         !
      CASE ( 0 )  ! Same concentration in sea ice and in the ocean ( trc_i = ptr(...,Kmm)  )
         !
         DO jn = 1, jptra
            DO_2D( 0, 0, 0, 0 )
               sbc_trc(ji,jj,jn) = fwfice(ji,jj) * r1_rho0 * ptr(ji,jj,1,jn,Kmm)
            END_2D
         END DO
         !
         IF( lk_linssh ) THEN  !* linear free surface  
            DO jn = 1, jptra
               DO_2D( 0, 0, 0, 0 )
                  sbc_trc(ji,jj,jn) = sbc_trc(ji,jj,jn) + r1_rho0 * emp(ji,jj) * ptr(ji,jj,1,jn,Kmm) !==>> add concentration/dilution effect due to constant volume cell
               END_2D
            END DO
         ENDIF
         !
      CASE ( 1 )  ! Specific treatment of sea ice fluxes with an imposed concentration in sea ice 
         !
         DO jn = 1, jptra
            DO_2D( 0, 0, 0, 0 )
               sbc_trc(ji,jj,jn) = fwfice(ji,jj) * r1_rho0 * trc_i(ji,jj,jn)
            END_2D
         END DO
         !
         IF( lk_linssh ) THEN  !* linear free surface  
            DO jn = 1, jptra
               DO_2D( 0, 0, 0, 0 )
                  sbc_trc(ji,jj,jn) = sbc_trc(ji,jj,jn) + r1_rho0 * emp(ji,jj) * ptr(ji,jj,1,jn,Kmm) !==>> add concentration/dilution effect due to constant volume cell
               END_2D
            END DO
         ENDIF
         !
         DO jn = 1, jptra
            DO_2D( 0, 0, 0, 0 )
               zse3t = rDt_trc / e3t(ji,jj,1,Kmm)
               zdtra = ptr(ji,jj,1,jn,Kmm) + sbc_trc(ji,jj,jn) * zse3t 
               IF( zdtra < 0. ) sbc_trc(ji,jj,jn) = MAX( zdtra, -ptr(ji,jj,1,jn,Kmm) / zse3t  ) ! avoid negative concentration that can occurs if trc_i > ptr 
            END_2D
         END DO
         !                             
      END SELECT
      !
      DO jn = 1, jptra
         !
         IF( l_trdtrc )   ztrtrd(:,:,:) = ptr(T2D(0),:,jn,Krhs)  ! save trends
         !
         DO_2D( 0, 0, 0, 0 )
            zse3t = zfact / e3t(ji,jj,1,Kmm)
            ptr(ji,jj,1,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) + ( sbc_trc_b(ji,jj,jn) + sbc_trc(ji,jj,jn) ) * zse3t
         END_2D
         !
         IF( l_trdtrc ) THEN
            ztrtrd(:,:,:) = ptr(T2D(0),:,jn,Krhs) - ztrtrd(:,:,:)
            CALL trd_tra( kt, Kmm, Krhs, 'TRC', jn, jptra_nsr, ztrtrd )
         END IF
         !                                                       ! ===========
      END DO                                                     ! tracer loop
      !                                                          ! ===========
      !
      !                                           Write in the tracer restar  file
      !                                          *******************************
      IF( lrst_trc .AND. .NOT.ln_top_euler ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'sbc : ocean surface tracer content forcing fields written in tracer restart file ',   &
            &                    'at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*) '~~~~'
         DO jn = 1, jptra
            CALL iom_rstput( kt, nitrst, numrtw, 'sbc_'//TRIM(ctrcnm(jn))//'_b', sbc_trc(:,:,jn) )
         END DO
      ENDIF
      !
      IF( sn_cfctl%l_prttrc )   THEN
         WRITE(charout, FMT="('sbc ')") ;  CALL prt_ctl_info( charout, cdcomp = 'top' )
                                           CALL prt_ctl( tab4d_1=ptr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm, clinfo3='trd' )
      ENDIF
      IF( l_trdtrc )  DEALLOCATE( ztrtrd )
      !
      IF( ln_timing )   CALL timing_stop('trc_sbc')
      !
   END SUBROUTINE trc_sbc


   SUBROUTINE trc_sbc_RK3 ( kt, Kbb, Kmm, ptr, Krhs, kstg )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_sbc_RK3  ***
      !!                   
      !! ** Purpose :   Compute the tracer surface boundary condition trend of
      !!      (concentration/dilution effect) and add it to the general 
      !!       trend of tracer equations.
      !!
      !! ** Method :
      !!      * concentration/dilution effect:
      !!            The surface freshwater flux modify the ocean volume
      !!         and thus the concentration of a tracer as :
      !!            tr(Krhs) = tr(Krhs) + emp * tr(Kmm) / e3t_   for k=1
      !!         where emp, the surface freshwater budget (evaporation minus
      !!         precipitation ) given in kg/m2/s is divided
      !!         by 1035 kg/m3 (density of ocean water) to obtain m/s.
      !!
      !! ** Action  : - Update the 1st level of tr(:,:,:,:,Krhs) with the trend associated
      !!                with the tracer surface boundary condition 
      !!
      !!----------------------------------------------------------------------
      INTEGER                                   , INTENT(in   ) ::   kt, Kbb, Kmm, Krhs   ! ocean time-step and time-level indices
      INTEGER                                   , INTENT(in   ) ::   kstg            ! RK3 stage index
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) ::   ptr       ! passive tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jn           ! dummy loop indices
      REAL(wp) ::   z1_rho0_e3t          ! local scalars
      REAL(wp) ::   zftra, zdtra, ztfx   !   -      -
      CHARACTER (len=22) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrtrd
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sbc_RK3')
      !
      IF( kt == nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_sbc_RK3 : Passive tracers surface boundary condition'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~ '
      ENDIF
      !
!!st note that trc_sbc can be removed only re-use in atf (not relevant for RK3)
      SELECT CASE( kstg )
         !
      CASE( 1 , 2 )                       !=  stage 1 and 2  =!   only in non linear ssh
         !
         IF( .NOT.lk_linssh ) THEN           !* only passive tracer fluxes associated with mass fluxes
            !                                        ! no passive tracer concentration modification due to ssh variation
!!st emp includes fwfice see iceupdate.F90
!!not sure about trc_i case... (1)
            DO jn = 1, jptra
               DO_2D( 0, 0, 0, 0 )              !!st WHY 1 : exterior here ? 
                  z1_rho0_e3t = r1_rho0 / e3t(ji,jj,1,Kmm)
                  ptr(ji,jj,1,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) - emp(ji,jj) * ptr(ji,jj,1,jn,Kbb) * z1_rho0_e3t
               END_2D
            END DO
            !
         ENDIF
         !
      CASE( 3 )
         !
         ! Allocate temporary workspace
         IF( l_trdtrc )  ALLOCATE( ztrtrd(T2D(0),jpk) )
         !
         DO jn = 1, jptra
            IF( l_trdtrc )   ztrtrd(:,:,:) = ptr(T2D(0),:,jn,Krhs)  ! save trends
         END DO
         !
         IF( lk_linssh ) THEN                !* linear free surface (add concentration/dilution effect artificially since no volume variation)
            !
            SELECT CASE ( nn_ice_tr )
               !
            CASE ( -1 ) ! No tracers in sea ice (null concentration in sea ice)
               !
               DO jn = 1, jptra
                  DO_2D( 0, 0, 0, 0 )
                     z1_rho0_e3t = r1_rho0  / e3t(ji,jj,1,Kmm)
                     ptr(ji,jj,1,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) + emp(ji,jj) *  ptr(ji,jj,1,jn,Kbb)  * z1_rho0_e3t
                  END_2D
               END DO
               !
            CASE ( 0 )  ! Same concentration in sea ice and in the ocean fwfice contribution to concentration/dilution effect has to be removed
               !
               DO jn = 1, jptra
                  DO_2D( 0, 0, 0, 0 )
                     z1_rho0_e3t = r1_rho0  / e3t(ji,jj,1,Kmm)
                     ptr(ji,jj,1,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) + ( emp(ji,jj) + fwfice(ji,jj) ) * ptr(ji,jj,1,jn,Kbb)  * z1_rho0_e3t
                  END_2D
               END DO
               !
            CASE ( 1 )  ! Specific treatment of sea ice fluxes with an imposed concentration in sea ice !!st TODO : check Christian new implementation
               !
               DO jn = 1, jptra
                  DO_2D( 0, 0, 0, 0 )
                     z1_rho0_e3t = r1_rho0  / e3t(ji,jj,1,Kmm)
                     ! tracer flux at the ice/ocean interface (tracer/m2/s)
                     zftra = trc_i(ji,jj,jn) * fwfice(ji,jj) ! uptake of tracer in the sea ice
                     !                                       ! only used in the levitating sea ice case
                     ! tracer flux only       : add concentration dilution term in net tracer flux, no F-M in volume flux
                     ! tracer and mass fluxes : no concentration dilution term in net tracer flux, F-M term in volume flux
                     ztfx  = zftra                        ! net tracer flux
                     !
                     zdtra =  z1_rho0_e3t * ( ztfx +  ( emp(ji,jj) + fwfice(ji,jj) ) * ptr(ji,jj,1,jn,Kbb) ) 
                     IF ( zdtra < 0. ) THEN
                        zdtra  = MAX(zdtra, -ptr(ji,jj,1,jn,Kbb) * e3t(ji,jj,1,Kmm) / rDt_trc )   ! avoid negative concentrations to arise
                     ENDIF
                     ptr(ji,jj,1,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) + zdtra
                  END_2D
               END DO
               !
            END SELECT
            !
         ELSE                                !* non linear free surface (concentration/dilution effect due to volume variation)
            !
            SELECT CASE ( nn_ice_tr )
            ! CASE ( -1 ) natural concentration/dilution effect due to volume variation : nothing to do
            !
            CASE ( 0 )  ! Same concentration in sea ice and in the ocean : correct concentration/dilution effect due to "freezing - melting"
               !
               DO jn = 1, jptra
                  DO_2D( 0, 0, 0, 0 )
                     z1_rho0_e3t = r1_rho0  / e3t(ji,jj,1,Kmm)
                     ptr(ji,jj,1,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) + fwfice(ji,jj) * ptr(ji,jj,1,jn,Kbb) * z1_rho0_e3t
                  END_2D
               END DO
               !
            CASE ( 1 )  ! Specific treatment of sea ice fluxes with an imposed concentration in sea ice 
               !
               DO jn = 1, jptra
                  DO_2D( 0, 0, 0, 0 )
                     ! tracer flux at the ice/ocean interface (tracer/m2/s)
                     zftra = trc_i(ji,jj,jn) * fwfice(ji,jj) ! uptake of tracer in the sea ice
                     !                                       ! only used in the levitating sea ice case
                     ! tracer flux only       : add concentration dilution term in net tracer flux, no F-M in volume flux
                     ! tracer and mass fluxes : no concentration dilution term in net tracer flux, F-M term in volume flux
                     ztfx  = zftra                        ! net tracer flux
                     !
                     zdtra = z1_rho0_e3t * ( ztfx + fwfice(ji,jj) * ptr(ji,jj,1,jn,Kbb) ) 
                     IF ( zdtra < 0. ) THEN
                        zdtra  = MAX(zdtra, -ptr(ji,jj,1,jn,Kbb) * e3t(ji,jj,1,Kmm) / rDt_trc )   ! avoid negative concentrations to arise
                     ENDIF
                     ptr(ji,jj,1,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) + zdtra
                  END_2D
               END DO
               !
            END SELECT
            !
         ENDIF
         !
         !
         !                                       Concentration dilution effect on tracers due to evaporation & precipitation 
         DO jn = 1, jptra
            !
            IF( l_trdtrc ) THEN
               ztrtrd(:,:,:) = ptr(T2D(0),:,jn,Krhs) - ztrtrd(:,:,:)
               CALL trd_tra( kt, Kbb, Krhs, 'TRC', jn, jptra_nsr, ztrtrd )
            END IF
            !
         END DO
         !
         IF( l_trdtrc )  DEALLOCATE( ztrtrd )
         !
      END SELECT
      !
      IF( sn_cfctl%l_prttrc )   THEN
         WRITE(charout, FMT="('sbc ')") ;  CALL prt_ctl_info( charout, cdcomp = 'top' )
                                           CALL prt_ctl( tab4d_1=ptr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm, clinfo3='trd' )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_sbc_RK3')
      !
   END SUBROUTINE trc_sbc_RK3


#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                      NO passive tracer
   !!----------------------------------------------------------------------
   USE par_oce
   USE par_trc
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_sbc ( kt, Kbb, Kmm, ptr, Krhs )      ! Empty routine
      INTEGER,                                    INTENT(in   ) :: kt        ! ocean time-step index
      INTEGER,                                    INTENT(in   ) :: Kbb, Kmm, Krhs ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) :: ptr       ! passive tracers and RHS of tracer equation
      WRITE(*,*) 'trc_sbc: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sbc
#endif
   
   !!======================================================================
END MODULE trcsbc
