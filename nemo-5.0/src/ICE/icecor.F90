MODULE icecor
   !!======================================================================
   !!                     ***  MODULE  icecor  ***
   !!   sea-ice: Corrections on sea-ice variables at the end of the time step
   !!======================================================================
   !! History :  3.0  !  2006-04  (M. Vancoppenolle) Original code
   !!            3.5  !  2014-06  (C. Rousset)       Complete rewriting/cleaning
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!    ice_cor      : corrections on sea-ice variables
   !!----------------------------------------------------------------------
   USE par_ice
   USE par_oce
   USE phycst         ! physical constants
   USE ice            ! sea-ice: variable
   USE iceitd  , ONLY : ice_itd_reb
   USE icevar  , ONLY : ice_var_zapsmall
   USE icectl         ! sea-ice: control prints
   USE sbc_oce,  ONLY : sss_m

   USE in_out_manager ! I/O manager
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_cor   ! called by icestp.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_cor( kt, kn )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE ice_cor  ***
      !!               
      !! ** Purpose :   Computes corrections on sea-ice global variables at 
      !!              the end of the dynamics (kn=1) and thermodynamics (kn=2)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! number of iteration
      INTEGER, INTENT(in) ::   kn    ! 1 = after dyn ; 2 = after thermo
      !
      INTEGER  ::   ji, jj, jk, jl   ! dummy loop indices
      REAL(wp) ::   zsal
      !!----------------------------------------------------------------------
      ! controls
      IF( ln_timing    )   CALL timing_start('icecor')                                                             ! timing
      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'icecor', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      IF( ln_icediachk )   CALL ice_cons2D  (0, 'icecor',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft) ! conservation
      !
      IF( kt == nit000 .AND. lwp .AND. kn == 2 ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'ice_cor:  correct sea ice variables if out of bounds ' 
         WRITE(numout,*) '~~~~~~~'
      ENDIF
      !                             !-----------------------------------------------------
      !                             !  ice thickness must exceed himin (for temp. diff.) !
      !                             !-----------------------------------------------------
      WHERE( a_i(A2D(0),:) >= epsi20 )   ;   h_i(A2D(0),:) = v_i(A2D(0),:) / a_i(A2D(0),:)
      ELSEWHERE                          ;   h_i(A2D(0),:) = 0._wp
      END WHERE
      IF( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
         WHERE( h_i(A2D(0),:) < rn_himin )  a_ip(A2D(0),:) = a_ip(A2D(0),:) * h_i(A2D(0),:) / rn_himin
      ENDIF
      WHERE( h_i(A2D(0),:) < rn_himin )     a_i (A2D(0),:) = a_i (A2D(0),:) * h_i(A2D(0),:) / rn_himin
      !
      !                             !-----------------------------------------------------
      !                             !  ice concentration should not exceed amax          !
      !                             !-----------------------------------------------------
      at_i(A2D(0)) = SUM( a_i(A2D(0),:), dim=3 )
      DO jl = 1, jpl
         WHERE( at_i(A2D(0)) > rn_amax_2d(A2D(0)) )   a_i(A2D(0),jl) = a_i(A2D(0),jl) * rn_amax_2d(A2D(0)) / at_i(A2D(0))
      END DO    
      !                             !-----------------------------------------------------
      !                             !  Rebin categories with thickness out of bounds     !
      !                             !-----------------------------------------------------
      IF( jpl > 1 )   CALL ice_itd_reb( kt )
      !
      !                             !-----------------------------------------------------
      !                             !  salinity must stay in bounds [Simin,Simax]        !
      !                             !-----------------------------------------------------
      IF ( nn_icesal == 2 ) THEN
         DO jl = 1, jpl
            DO_2D( 0, 0, 0, 0 )
               zsal = sv_i(ji,jj,jl)
               sv_i(ji,jj,jl) = MIN( MAX( rn_simin*v_i(ji,jj,jl) , sv_i(ji,jj,jl) ) , rn_sinew*sss_m(ji,jj)*v_i(ji,jj,jl)  )
               IF( kn /= 0 ) & ! no ice-ocean exchanges if kn=0 (for bdy for instance) otherwise conservation diags will fail
                  &   sfx_res(ji,jj) = sfx_res(ji,jj) - ( sv_i(ji,jj,jl) - zsal ) * rhoi * r1_Dt_ice   ! associated salt flux
            END_2D
         END DO
      ELSEIF ( nn_icesal == 4 ) THEN
         DO jl = 1, jpl
            DO_3D( 0, 0, 0, 0, 1, nlay_i )
               zsal = szv_i(ji,jj,jk,jl)
               szv_i(ji,jj,jk,jl) = MIN( MAX( szv_i(ji,jj,jk,jl) , rn_simin              * v_i(ji,jj,jl) * r1_nlay_i ) , &
                  &                                                rn_sinew*sss_m(ji,jj) * v_i(ji,jj,jl) * r1_nlay_i )
               IF( kn /= 0 ) & ! no ice-ocean exchanges if kn=0 (for bdy for instance) otherwise conservation diags will fail
                  &   sfx_res(ji,jj) = sfx_res(ji,jj) - ( szv_i(ji,jj,jk,jl) - zsal ) * rhoi * r1_Dt_ice   ! associated salt flux
            END_3D
         END DO
      ENDIF
      !
      IF( kn /= 0 ) THEN   ! no zapsmall if kn=0 (for bdy for instance) because we do not want ice-ocean exchanges (wfx,sfx,hfx)
         !                                                              otherwise conservation diags will fail
         !                          !-----------------------------------------------------
         CALL ice_var_zapsmall      !  Zap small values                                  !
         !                          !-----------------------------------------------------
      ENDIF
      !
      ! controls
      IF( sn_cfctl%l_prtctl ) &
         &                 CALL ice_prt3D   ('icecor')                                                             ! prints
      IF( ln_icectl .AND. kn == 2 ) &
         &                 CALL ice_prt     ( kt, iiceprt, jiceprt, 2, ' - Final state - ' )                       ! prints
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'icecor', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      IF( ln_icediachk )   CALL ice_cons2D  (1, 'icecor',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft) ! conservation
      IF( ln_timing    )   CALL timing_stop ('icecor')                                                             ! timing
      !
   END SUBROUTINE ice_cor

#else
   !!----------------------------------------------------------------------
   !!   Default option           Dummy module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icecor
