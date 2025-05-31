MODULE traldf_lev
   !!==============================================================================
   !!                       ***  MODULE  traldf_lev  ***
   !! Ocean tracers:  iso-level diffusive tracer trend  (laplacian and bilaplacian)
   !!==============================================================================
   !! History :  3.7  ! 2014-01  (G. Madec, S. Masson)  Original code, re-entrant laplacian (traldf_lap_blp module)
   !!            4.5  ! 2022-06  (S. Techene, G, Madec)  refactorization to reduce local memory usage
   !!                 !                                + removal of old partial-step treatment
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   traldf_lev_lap   : tracer trend update using an iso-level   laplacian diffusive operator
   !!   traldf_lev_blp   :   -      -      -     -   an iso-level bilaplacian     -        -
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE domutl  , ONLY : is_tile
   USE ldftra  , ONLY : ahtu, ahtv   ! lateral physics: eddy diffusivity coefficients
   USE diaptr         ! poleward transport diagnostics
   USE diaar5         ! AR5 diagnostics
!!gm this is useless I guess since trc is passed in argument with pt
!   USE trc_oce        ! share passive tracers/Ocean variables
!!gm end
   !
   USE in_out_manager ! I/O manager
!!gm   USE iom            ! I/O library
!!gm   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
!!gm   USE lib_mpp        ! distribued memory computing library
!!gm   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   traldf_lev_lap   ! called by traldf.F90
   PUBLIC   traldf_lev_blp   ! called by traldf.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE traldf_lev_lap( kt, Kbb, Kmm, pt, Krhs, ld_ptr, ld_hst )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_ldf_lap  ***
      !!
      !! ** Purpose :   Compute the before horizontal tracer (t & s) diffusive
      !!      trend and add it to the general trend of tracer equation.
      !!
      !! ** Method  :   iso-level laplacian diffusive operator evaluated using
      !!      Kbb fields (forward time integration). The horizontal diffusive 
      !!      trends of the tracer is given by:
      !!          difft = 1/(e1e2t*e3t_Kmm) {  di-1[ ahtu e2u*e3u_Kmm/e1u di(t(Kbb)) ]
      !!                                     + dj-1[ ahtv e1v*e3v_Kmm/e2v dj(t(Kbb)) ] }
      !!      Add this trend to the general tracer trend pt_rhs :
      !!          pt_rhs = pt_rhs + difft
      !!
      !! ** Action :   pt(Krhs)   increased by the laplacian diffusive trend 
      !!----------------------------------------------------------------------
      INTEGER                       , INTENT(in   ) ::   kt, Kbb, Kmm, Krhs   ! ocean time-step and time-level indices
      LOGICAL , OPTIONAL            , INTENT(in   ) ::   ld_hst, ld_ptr       ! T-S diagnostic flags
      REAL(wp), DIMENSION(:,:,:,:,:), INTENT(inout) ::   pt                   ! tracers, in: at kbb ; out: at Krhs
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      INTEGER  ::   itra             ! number of tracers
      REAL(wp) ::   zaheeu, zaheev   ! local scalar
      LOGICAL  ::   ll_ptr, ll_hst
      !
      REAL(wp), DIMENSION(T2D(1)) ::   zfu, zfv
      !!----------------------------------------------------------------------
      ll_ptr = .FALSE. ; ll_hst = .FALSE.
      IF( PRESENT(ld_ptr) ) ll_ptr = l_diaptr .AND. ld_ptr
      IF( PRESENT(ld_hst) ) ll_hst = ld_hst
      !
      itra = SIZE( pt, dim=4 )         ! number of tracers
      !
      !  
      !                             ! =========== !
      DO jn = 1, itra               ! tracer loop !
         !                          ! =========== !
         !
         DO jk = 1, jpkm1                 ! horizontal slab
            !
            DO_2D( 1, 0, 1, 0 )                 !-  1st derivative   (masked as ahtu/v are masked)
               zaheeu     = ahtu(ji,jj,jk) * e2_e1u(ji,jj) * e3u(ji,jj,jk,Kmm)
               zaheev     = ahtv(ji,jj,jk) * e1_e2v(ji,jj) * e3v(ji,jj,jk,Kmm)
               zfu(ji,jj) = zaheeu * ( pt(ji+1,jj  ,jk,jn,Kbb) - pt(ji,jj,jk,jn,Kbb) )
               zfv(ji,jj) = zaheev * ( pt(ji  ,jj+1,jk,jn,Kbb) - pt(ji,jj,jk,jn,Kbb) )
            END_2D
            DO_2D( 0, 0, 0, 0 )                 !-  2nd derivative added to the general tracer trends (with PLUS sign)
               pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs)                  &
                  &                 + (  ( zfu(ji,jj) - zfu(ji-1,jj) )      &
                  &                    + ( zfv(ji,jj) - zfv(ji,jj-1) )  )   &
                  &                 * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
            END_2D
            !
            !                                   !=  "Poleward" & 2D-integrated diffusive heat and salt transports  =!
            !                                       Note sign is reversed to give down-gradient diffusive transports
            IF( ll_ptr )  CALL dia_ptr_hst( jn, 'ldf', -zfv(:,:) )
            IF( ll_hst )  CALL dia_ar5_hst( jn, 'ldf', -zfu(:,:), -zfv(:,:), ldfin=(jk == jpkm1) )
         END DO                          ! end horizontal slab
         !
         !                          ! ==================
      END DO                        ! end of tracer loop
      !                             ! ==================
      !
   END SUBROUTINE traldf_lev_lap


   SUBROUTINE traldf_lev_blp( kt, Kbb, Kmm, pt, Krhs, ld_ptr, ld_hst )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tra_ldf_blp  ***
      !!
      !!
      !!           NO use of zps_hde ==>>  New HPG calculation
      !!           **        *******
      !!      
      !!
      !! ** Purpose :   Compute the before lateral tracer diffusive
      !!      trend and add it to the general trend of tracer equation.
      !!
      !! ** Method  :   The lateral diffusive trends is provided by a bilaplacian
      !!      iso-level operator applied to pt(Kbb) (forward time integration).
      !!
      !! ** Action :   pt(Krhs)   increased by the bilaplacian diffusive trend 
      !!----------------------------------------------------------------------
      INTEGER                       , INTENT(in   ) ::   kt, Kbb, Kmm, Krhs   ! ocean time-step and time-level indices
      LOGICAL , OPTIONAL            , INTENT(in   ) ::   ld_hst, ld_ptr       ! T-S diagnostic flags
      REAL(wp), DIMENSION(:,:,:,:,:), INTENT(inout) ::   pt                   ! tracers, in: at kbb ; out: at Krhs
      !
      INTEGER  ::   ji, jj, jk, jn      ! dummy loop indices
      INTEGER  ::   itra                ! number of tracers
      LOGICAL  ::   ll_ptr, ll_hst
      REAL(wp), DIMENSION(T2D(2)) ::   zaheeu, zfu
      REAL(wp), DIMENSION(T2D(2)) ::   zaheev, zfv
      REAL(wp), DIMENSION(T2D(1)) ::   zlap             ! laplacian at t-point
      !!---------------------------------------------------------------------
      !
      ll_ptr = .FALSE. ; ll_hst = .FALSE.
      IF( PRESENT(ld_ptr) ) ll_ptr = l_diaptr .AND. ld_ptr
      IF( PRESENT(ld_hst) ) ll_hst = ld_hst
      !
      itra = SIZE( pt, dim=4 )      ! number of tracers
      !  
      !                             ! =========== !
      DO jn = 1, itra               ! tracer loop !
         !                          ! =========== !
         !
         DO jk = 1, jpkm1                 ! horizontal slab
            !
            DO_2D( 2, 1, 2, 1 )
               zaheeu(ji,jj) = ahtu(ji,jj,jk) * e2_e1u(ji,jj) * e3u(ji,jj,jk,Kmm)
               zaheev(ji,jj) = ahtv(ji,jj,jk) * e1_e2v(ji,jj) * e3v(ji,jj,jk,Kmm)
            END_2D
            !                                   !-  first derivative   (masked as ahtu/v are masked)
            DO_2D( 2, 1, 2, 1 )
               zfu(ji,jj) = zaheeu(ji,jj) * ( pt(ji+1,jj  ,jk,jn,Kbb) - pt(ji,jj,jk,jn,Kbb) )
               zfv(ji,jj) = zaheev(ji,jj) * ( pt(ji  ,jj+1,jk,jn,Kbb) - pt(ji,jj,jk,jn,Kbb) )
            END_2D
            !                                   !-  Second derivative (divergence)    (with PLUS sign)
            DO_2D( 1, 1, 1, 1 )
               zlap(ji,jj) = (  ( zfu(ji,jj) - zfu(ji-1,jj) )      &       ! t-masked as fluxes are u/v-masked
                  &           + ( zfv(ji,jj) - zfv(ji,jj-1) )  )   &
                  &        *  r1_e1e2t(ji,jj) /  e3t(ji,jj,jk,Kmm)
            END_2D
            !                                   !-  3rd derivative  -!   (masked as ahtu/v are masked)
            DO_2D( 1, 0, 1, 0 )
               zfu(ji,jj) = zaheeu(ji,jj) * ( zlap(ji+1,jj  ) - zlap(ji,jj) )
               zfv(ji,jj) = zaheev(ji,jj) * ( zlap(ji  ,jj+1) - zlap(ji,jj) )
            END_2D
            !
            DO_2D( 0, 0, 0, 0 )                 !-  4th derivative added to the general tracer trends (with MINUS sign)
               pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs)                    &
                  &                 - (  ( zfu(ji,jj) - zfu(ji-1,jj) )      &
                  &                    + ( zfv(ji,jj) - zfv(ji,jj-1) )  )   &
                  &                 *  r1_e1e2t(ji,jj) /  e3t(ji,jj,jk,Kmm)
            END_2D
            !
            !                                   !=  "Poleward" & 2D-integrated diffusive heat and salt transports  =!
            !                                       Note sign is reversed to give down-gradient diffusive transports
            IF( ll_ptr )  CALL dia_ptr_hst( jn, 'ldf', -zfv(:,:) )
            IF( ll_hst )  CALL dia_ar5_hst( jn, 'ldf', -zfu(:,:), -zfv(:,:), ldfin=(jk == jpkm1) )
         END DO                           ! end horizontal slab
         !
         !                          ! ==================
      END DO                        ! end of tracer loop
      !                             ! ==================
      !
   END SUBROUTINE traldf_lev_blp

   !!==============================================================================
END MODULE traldf_lev
