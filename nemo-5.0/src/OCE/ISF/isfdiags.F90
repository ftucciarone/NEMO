MODULE isfdiags
   !!======================================================================
   !!                       ***  MODULE  isfdiags  ***
   !! ice shelf diagnostics module :  manage the 2d and 3d flux outputs from the ice shelf module
   !!======================================================================
   !! History :  3.2  !  2011-02  (C.Harris  ) Original code isf cav
   !!            X.X  !  2006-02  (C. Wang   ) Original code bg03
   !!            3.4  !  2013-03  (P. Mathiot) Merging + parametrization
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_isf       : update sbc under ice shelf
   !!----------------------------------------------------------------------

   USE in_out_manager ! I/O manager
   USE par_oce        ! ocean space and time domain
   USE dom_oce
   USE isf_oce        ! ice shelf variable
   USE iom            ! 

   IMPLICIT NONE

   PRIVATE

   PUBLIC   isf_diags_flx

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE isf_diags_flx( Kmm, ktop, kbot, phtbl, pfrac, cdisf, pfwf, pqoce, pqlat, pqhc )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_diags_flx ***
      !!
      !! ** Purpose : manage the 2d and 3d flux outputs of the ice shelf module
      !!              from isf to oce fwf, latent heat, heat content fluxes
      !!
      !!----------------------------------------------------------------------
      INTEGER,                     INTENT(in) ::   Kmm                       ! ocean time level index
      INTEGER , DIMENSION(A2D(1)), INTENT(in) ::   ktop , kbot               ! top and bottom level of the tbl
      REAL(wp), DIMENSION(A2D(1)), INTENT(in) ::   phtbl, pfrac              ! thickness of the tbl and fraction of last cell affected by the tbl
      REAL(wp), DIMENSION(A2D(0)), INTENT(in) ::   pfwf, pqoce, pqlat, pqhc  ! 2d var to map in 3d
      CHARACTER(LEN=3), INTENT(in) ::   cdisf                                ! parametrisation or interactive melt
      !!---------------------------------------------------------------------
      CHARACTER(LEN=256) ::   cvarfwf  , cvarqoce  , cvarqlat  , cvarqhc
      CHARACTER(LEN=256) ::   cvarfwf3d, cvarqoce3d, cvarqlat3d, cvarqhc3d
      !!---------------------------------------------------------------------
      !
      ! output melt
      cvarfwf  = 'fwfisf_'//cdisf  ; cvarfwf3d  = 'fwfisf3d_'//cdisf
      cvarqoce = 'qoceisf_'//cdisf ; cvarqoce3d = 'qoceisf3d_'//cdisf
      cvarqlat = 'qlatisf_'//cdisf ; cvarqlat3d = 'qlatisf3d_'//cdisf 
      cvarqhc  = 'qhcisf_'//cdisf  ; cvarqhc3d  = 'qhcisf3d_'//cdisf
      !
      ! output 2d melt rate, latent heat and heat content flux from the injected water
      CALL iom_put( TRIM(cvarfwf) , pfwf(:,:) )   ! mass         flux ( > 0 from isf to oce)
      CALL iom_put( TRIM(cvarqoce), pqoce(:,:) )   ! oce to ice   flux ( > 0 from isf to oce)
      CALL iom_put( TRIM(cvarqlat), pqlat(:,:) )   ! latent heat  flux ( > 0 from isf to oce)
      CALL iom_put( TRIM(cvarqhc) , pqhc (:,:) )   ! heat content flux ( > 0 from isf to oce)
      !
      ! output 3d Diagnostics
      IF ( iom_use( TRIM(cvarfwf3d)  ) ) CALL isf_diags_2dto3d( Kmm, ktop, kbot, phtbl, pfrac, TRIM(cvarfwf3d)  , pfwf(:,:))
      IF ( iom_use( TRIM(cvarqoce3d) ) ) CALL isf_diags_2dto3d( Kmm, ktop, kbot, phtbl, pfrac, TRIM(cvarqoce3d) , pqoce(:,:))
      IF ( iom_use( TRIM(cvarqlat3d) ) ) CALL isf_diags_2dto3d( Kmm, ktop, kbot, phtbl, pfrac, TRIM(cvarqlat3d) , pqoce(:,:))
      IF ( iom_use( TRIM(cvarqhc3d)  ) ) CALL isf_diags_2dto3d( Kmm, ktop, kbot, phtbl, pfrac, TRIM(cvarqhc3d)  , pqhc (:,:))
      !
   END SUBROUTINE isf_diags_flx

   SUBROUTINE isf_diags_2dto3d( Kmm, ktop, kbot, phtbl, pfrac, cdvar, pvar2d )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_diags_2dto3d ***
      !!
      !! ** Purpose : compute the 3d flux outputs as they are injected into NEMO 
      !!              (ie uniformaly spread into the top boundary layer or parametrisation layer)
      !!
      !!----------------------------------------------------------------------
      INTEGER,                     INTENT(in) ::   Kmm           ! ocean time level index
      INTEGER , DIMENSION(A2D(1)), INTENT(in) ::   ktop , kbot   ! top and bottom level of the tbl
      REAL(wp), DIMENSION(A2D(1)), INTENT(in) ::   phtbl, pfrac  ! thickness of the tbl and fraction of last cell affected by the tbl
      REAL(wp), DIMENSION(A2D(0)), INTENT(in) ::   pvar2d        ! 2d var to map in 3d
      CHARACTER(LEN=*), INTENT(in) ::   cdvar
      !!---------------------------------------------------------------------
      INTEGER  :: ji, jj, jk                      ! loop indices
      INTEGER  :: ikt, ikb                        ! top and bottom level of the tbl
      REAL(wp), DIMENSION(A2D(0))     ::   zvar2d   !
      REAL(wp), DIMENSION(A2D(0),jpk) ::   zvar3d   ! 3d var to output
      !!---------------------------------------------------------------------
      !
      ! compute 3d output
      DO_2D( 0, 0, 0, 0 )
         zvar2d(ji,jj) = pvar2d(ji,jj) / phtbl(ji,jj)
      END_2D
      zvar3d(:,:,:) = 0._wp
      !
      DO_2D( 0, 0, 0, 0 )
         ikt = ktop(ji,jj)
         ikb = kbot(ji,jj)
         DO jk = ikt, ikb - 1
            zvar3d(ji,jj,jk) = zvar2d(ji,jj) * e3t(ji,jj,jk,Kmm)
         END DO
         zvar3d(ji,jj,ikb) = zvar2d(ji,jj) * e3t(ji,jj,ikb,Kmm) * pfrac(ji,jj)
      END_2D
      !
      CALL iom_put( TRIM(cdvar) , zvar3d(:,:,:) )
      !
   END SUBROUTINE isf_diags_2dto3d

END MODULE isfdiags
