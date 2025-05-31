MODULE isfdynatf
   !!=========================================================================
   !!                       ***  MODULE  isfnxt  ***
   !! Ice shelf update: compute the dynatf ice shelf contribution
   !!=========================================================================
   !! History :  OPA  !  2019-09  (P. Mathiot)  Original code
   !!-------------------------------------------------------------------------
  
   !!-------------------------------------------------------------------------
   !!   isfdynatf       : apply correction needed for the ice shelf to ensure conservation
   !!-------------------------------------------------------------------------

   USE isf_oce

   USE phycst , ONLY: r1_rho0         ! physical constant
   USE par_oce                        ! ocean space and time domain
   USE dom_oce                        ! time and space domain
   USE oce, ONLY : ssh                ! sea-surface height for qco substitution

   USE in_out_manager

   IMPLICIT NONE

   PRIVATE

   PUBLIC isf_dynatf
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
CONTAINS

   SUBROUTINE isf_dynatf ( kt, Kmm, pe3t_f )
      !!--------------------------------------------------------------------
      !!                  ***  ROUTINE isf_dynatf  ***
      !!
      !! ** Purpose : compute the ice shelf volume filter correction for cavity, param, ice sheet coupling case
      !!
      !!-------------------------- OUT -------------------------------------
      INTEGER                         , INTENT(in   ) ::   kt       ! ocean time step
      INTEGER                         , INTENT(in   ) ::   Kmm      ! ocean time level index
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   pe3t_f   ! time filtered scale factor to be corrected
      !!--------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk  ! loop index
      REAL(wp) ::   ztmp
      !!--------------------------------------------------------------------
      !
      ! ice shelf cavity
      IF( ln_isfcav_mlt ) THEN
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ztmp = rn_atfp * rn_Dt * ( fwfisf_cav_b(ji,jj) - fwfisf_cav(ji,jj) ) / ( ht(ji,jj,Kmm) + 1._wp - ssmask(ji,jj) ) * r1_rho0
            !
            DO jk = 1, jpkm1
               pe3t_f(ji,jj,jk) = pe3t_f(ji,jj,jk) + tmask(ji,jj,jk) * ztmp * e3t(ji,jj,jk,Kmm)
            END DO
         END_2D
      ENDIF
      !
      ! ice shelf parametrised
      IF( ln_isfpar_mlt ) THEN
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ztmp = rn_atfp * rn_Dt * ( fwfisf_par_b(ji,jj) - fwfisf_par(ji,jj) ) / ( ht(ji,jj,Kmm) + 1._wp - ssmask(ji,jj) ) * r1_rho0
            !
            DO jk = 1, jpkm1
               pe3t_f(ji,jj,jk) = pe3t_f(ji,jj,jk) + tmask(ji,jj,jk) * ztmp * e3t(ji,jj,jk,Kmm)
            END DO
         END_2D
      ENDIF
      !
      ! if coupled
      IF( ln_isfcpl .AND. ln_rstart .AND. kt == nit000+1 ) THEN
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
            pe3t_f(ji,jj,jk) = pe3t_f(ji,jj,jk) - rn_atfp * rn_Dt * risfcpl_vol(ji,jj,jk) * r1_e1e2t(ji,jj)
         END_3D
      END IF
      !
   END SUBROUTINE isf_dynatf

END MODULE isfdynatf
