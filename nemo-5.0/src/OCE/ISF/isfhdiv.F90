MODULE isfhdiv
   !!======================================================================
   !!                       ***  MODULE  isfhdiv  ***
   !! ice shelf horizontal divergence module :  update the horizontal divergence
   !!                   with the ice shelf melt and coupling correction
   !!======================================================================
   !! History :  4.0  !  2019-09  (P. Mathiot) Original code
   !!            4.2  !  2022-05  (S. Techene) Update wrt RK3 time-stepping
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isf_hdiv    : update the horizontal divergence with the ice shelf 
   !!                 melt and coupling correction
   !!----------------------------------------------------------------------

   USE isf_oce                ! ice shelf

   USE par_oce                ! ocean space and time domain
   USE dom_oce                ! time and space domain
   USE phycst , ONLY: r1_rho0 ! physical constant
   USE in_out_manager         !

   IMPLICIT NONE
   PRIVATE

   PUBLIC isf_hdiv

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
CONTAINS

   SUBROUTINE isf_hdiv( kt, Kmm, phdiv )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE isf_hdiv  ***
      !!       
      !! ** Purpose :   update the horizontal divergence with the ice shelf contribution
      !!                (parametrisation, explicit, ice sheet coupling conservation
      !!                 increment)
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt
      INTEGER, INTENT(in) ::   Kmm      !  ocean time level index
      REAL(wp), DIMENSION(A2D(1),jpk), INTENT( inout ) ::   phdiv   ! horizontal divergence
      !!----------------------------------------------------------------------
      !
      IF ( ln_isf ) THEN
         !
#if defined key_RK3
         ! ice shelf cavity contribution (RK3)
         IF ( ln_isfcav_mlt ) CALL isf_hdiv_mlt(misfkt_cav, misfkb_cav, rhisf_tbl_cav, rfrac_tbl_cav, &
            &                                                                             fwfisf_cav, phdiv)
         !
         ! ice shelf parametrisation contribution (RK3)
         IF ( ln_isfpar_mlt ) CALL isf_hdiv_mlt(misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, &
                                                                                          fwfisf_par, phdiv)
#else
         ! ice shelf cavity contribution (MLF)
         IF ( ln_isfcav_mlt ) CALL isf_hdiv_mlt(misfkt_cav, misfkb_cav, rhisf_tbl_cav, rfrac_tbl_cav, &
            &                                                                             fwfisf_cav, phdiv, fwfisf_cav_b)
         !
         ! ice shelf parametrisation contribution (MLF)
         IF ( ln_isfpar_mlt ) CALL isf_hdiv_mlt(misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, &
                                                                                          fwfisf_par, phdiv, fwfisf_par_b)
#endif
         !
         ! ice sheet coupling contribution
         IF ( ln_isfcpl .AND. kt /= 0 ) THEN
            !
            ! Dynamical stability at start up after change in under ice shelf cavity geometry is achieve by correcting the divergence.
            ! This is achieved by applying a volume flux in order to keep the horizontal divergence after remapping 
            ! the same as at the end of the latest time step. So correction need to be apply at nit000 (euler time step) and
            ! half of it at nit000+1 (leap frog time step).
            IF ( kt == nit000   ) CALL isf_hdiv_cpl(Kmm, risfcpl_vol       , phdiv)
            IF ( kt == nit000+1 ) CALL isf_hdiv_cpl(Kmm, risfcpl_vol*0.5_wp, phdiv)
            !
            ! correct divergence every time step to remove any trend due to coupling
            ! conservation option
            IF ( ln_isfcpl_cons ) CALL isf_hdiv_cpl(Kmm, risfcpl_cons_vol, phdiv)
            !
         END IF
         !
      END IF
      !
   END SUBROUTINE isf_hdiv


   SUBROUTINE isf_hdiv_mlt( ktop, kbot, phtbl, pfrac, pfwf, phdiv, pfwf_b )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE sbc_isf_div  ***
      !!       
      !! ** Purpose :   update the horizontal divergence with the ice shelf inflow
      !!
      !! ** Method  :   pfwf is positive (outflow) and expressed as kg/m2/s
      !!                increase the divergence
      !!
      !! ** Action  :   phdivn   increased by the ice shelf outflow
      !!----------------------------------------------------------------------
      INTEGER , DIMENSION(A2D(1))           , INTENT(in   ) ::   ktop , kbot
      REAL(wp), DIMENSION(A2D(1))           , INTENT(in   ) ::   pfrac, phtbl
      REAL(wp), DIMENSION(jpi,jpj)          , INTENT(in   ) ::   pfwf
      REAL(wp), DIMENSION(A2D(1),jpk)       , INTENT(inout) ::   phdiv
      REAL(wp), DIMENSION(:,:)    , OPTIONAL, INTENT(in   ) ::   pfwf_b
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   ikt, ikb 
      REAL(wp) ::   zhdiv
      !!----------------------------------------------------------------------
      !
      !==   fwf distributed over several levels   ==!
      !
      ! update divergence at each level affected by ice shelf top boundary layer
      DO_2D( 1, 1, 1, 1 )
         ! compute integrated divergence correction
         IF( phtbl(ji,jj) /= 0._wp ) THEN
#if defined key_RK3
            zhdiv = pfwf(ji,jj) * r1_rho0 / phtbl(ji,jj)
#else
            zhdiv = 0.5_wp * ( pfwf(ji,jj) + pfwf_b(ji,jj) ) * r1_rho0 / phtbl(ji,jj)
#endif
         ELSE
            zhdiv = 0._wp
         ENDIF
         !
         ikt = ktop(ji,jj)
         ikb = kbot(ji,jj)
         ! level fully include in the ice shelf boundary layer
         DO jk = ikt, ikb - 1
            phdiv(ji,jj,jk) = phdiv(ji,jj,jk) - zhdiv
         END DO
         ! level partially include in ice shelf boundary layer 
         phdiv(ji,jj,ikb) = phdiv(ji,jj,ikb) - zhdiv * pfrac(ji,jj)
      END_2D
      !
   END SUBROUTINE isf_hdiv_mlt


   SUBROUTINE isf_hdiv_cpl( Kmm, pqvol, phdiv )
      !!----------------------------------------------------------------------
      !!                  ***  SUBROUTINE isf_hdiv_cpl  ***
      !!       
      !! ** Purpose :   update the horizontal divergence with the ice shelf 
      !!                coupling conservation increment
      !!
      !! ** Method  :   pqvol is positive (outflow) and expressed as m3/s
      !!                increase the divergence
      !!
      !! ** Action  :   phdivn   increased by the ice shelf outflow
      !!
      !!----------------------------------------------------------------------
      INTEGER,                          INTENT(in)    ::   Kmm     ! ocean time level index
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in   ) ::   pqvol
      REAL(wp), DIMENSION(A2D(1) ,jpk), INTENT(inout) ::   phdiv
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk
      !!----------------------------------------------------------------------
      !
      DO_3D( 1, 1, 1, 1, 1, jpkm1 )
         phdiv(ji,jj,jk) = phdiv(ji,jj,jk) + pqvol(ji,jj,jk) * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
      END_3D
      !
   END SUBROUTINE isf_hdiv_cpl

END MODULE isfhdiv
