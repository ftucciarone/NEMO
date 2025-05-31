MODULE isfparmlt
   !!======================================================================
   !!                       ***  MODULE  isfparmlt  ***
   !! Ice shelf parametrisation module :  update surface ocean boundary condition under ice
   !!                   shelf using an ice shelf melt parametrisation
   !!======================================================================
   !! History :  4.0  ! original code
   !!----------------------------------------------------------------------

   USE isf_oce                  ! ice shelf
   USE isftbl                   ! ice shelf depth average

   USE par_oce                  ! ocean space and time domain
   USE dom_oce                  ! ocean space and time domain
   USE oce    , ONLY: ts        ! ocean dynamics and tracers
   USE phycst , ONLY: rcp, rho0 ! physical constants

   USE in_out_manager              ! I/O manager
   USE fldread    , ONLY: fld_read, FLD, FLD_N !
   USE lib_fortran, ONLY: glob_2Dsum
   USE lib_mpp    , ONLY: ctl_stop

   IMPLICIT NONE

   PRIVATE

   PUBLIC  isfpar_mlt 

   !! * Substitutions
#  include "domzgr_substitute.h90"   
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS


   SUBROUTINE isfpar_mlt( kt, Kmm, ptfrz, ptavg, pqhc, pqoce, pfwf )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt  ***
      !!
      !! ** Purpose : Compute Salt and Heat fluxes related to ice_shelf 
      !!              melting and freezing 
      !!
      !! ** Method  :  2 parameterizations are available according
      !!                        1 : Specified melt flux
      !!                        2 : Beckmann & Goose parameterization
      !!
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      INTEGER, INTENT(in) ::   Kmm  ! ocean time level index
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   ptfrz, ptavg       ! tbl freezing and averaged temperatures
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pfwf, pqoce, pqhc  ! fresh water, ice-ocean heat and heat content fluxes
      !!---------------------------------------------------------------------
      !
      ! Choose among the available ice shelf parametrisation
      SELECT CASE ( cn_isfpar_mlt )
      CASE ( 'spe' )    ! specified runoff in depth (Mathiot et al., 2017 in preparation)
         !
         CALL isfpar_mlt_spe(   kt, Kmm, ptfrz, pqhc, pqoce, pfwf )
         !
      CASE ( 'bg03' )    ! Beckmann and Goosse parametrisation 
         !
         CALL isfpar_mlt_bg03(  kt, Kmm, ptfrz, ptavg, pqhc, pqoce, pfwf )
         !
      CASE ( 'oasis' )
         !
         CALL isfpar_mlt_oasis( kt, Kmm, ptfrz, pqhc, pqoce, pfwf )
         !
      CASE DEFAULT
         CALL ctl_stop('STOP', 'unknown isf melt formulation : cn_isfpar (should not see this)')
      END SELECT
      !
   END SUBROUTINE isfpar_mlt


   SUBROUTINE isfpar_mlt_spe( kt, Kmm, ptfrz, pqhc, pqoce, pfwf )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt_spe  ***
      !!
      !! ** Purpose : prescribed ice shelf melting in case ice shelf cavities are closed.
      !!              data read into a forcing files.
      !!
      !!--------------------------------------------------------------------
      INTEGER,  INTENT(in) ::   kt
      INTEGER,  INTENT(in) ::   Kmm    !  ocean time level index
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   ptfrz              ! tbl freezing temp
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pqhc, pfwf, pqoce  ! fresh water and ice-ocean heat fluxes
      !!
      INTEGER ::   ji, jj     ! dummy loop indices
      !!--------------------------------------------------------------------
      !
      ! Read specified fwf from isf to oce
      CALL fld_read ( kt, 1, sf_isfpar_fwf )
      !
      DO_2D( 0, 0, 0, 0 )
         pfwf(ji,jj) =   sf_isfpar_fwf(1)%fnow(ji,jj,1)    * mskisf_par(ji,jj)  ! fresh water flux from the isf (fwfisf <0 mean melting)       ( > 0 from isf to oce)
         pqoce(ji,jj) = - pfwf(ji,jj) * rLfusisf           * mskisf_par(ji,jj)  ! ocean/ice shelf flux assume to be equal to latent heat flux  ( > 0 from isf to oce)
         pqhc (ji,jj) =   pfwf(ji,jj) * ptfrz(ji,jj) * rcp * mskisf_par(ji,jj)  ! heat content flux                                            ( > 0 from isf to oce)
      END_2D
      !
      !
   END SUBROUTINE isfpar_mlt_spe

   SUBROUTINE isfpar_mlt_bg03( kt, Kmm, ptfrz, ptavg, pqhc, pqoce, pfwf )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt_bg03  ***
      !!
      !! ** Purpose : compute an estimate of ice shelf melting and 
      !!              latent, ocean-ice and heat content heat fluxes
      !!              in case cavities are closed based on the far fields T and S properties. 
      !!
      !! ** Method  : The ice shelf melt is computed as proportional to the differences between the 
      !!              mean temperature and mean freezing point in front of the ice shelf averaged 
      !!              over the ice shelf min ice shelf draft and max ice shelf draft and the freezing point
      !!
      !! ** Reference : Beckmann and Goosse (2003), "A parameterization of ice shelf-ocean
      !!                interaction for climate models", Ocean Modelling 5(2003) 157-170.
      !!----------------------------------------------------------------------
      INTEGER,  INTENT(in) ::   kt
      INTEGER,  INTENT(in) ::   Kmm    !  ocean time level index
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   ptfrz, ptavg       ! tbl freezing and averaged temp
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pqhc, pfwf, pqoce  ! fresh water and ice-ocean heat fluxes
      !!
      INTEGER ::   ji, jj     ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      ! Net heat flux and fresh water flux due to the ice shelf
      DO_2D( 0, 0, 0, 0 )
         pfwf (ji,jj) =  rho0 * rcp * rn_isfpar_bg03_gt0 * risfLeff(ji,jj) * e1t(ji,jj) * ( ptavg(ji,jj) - ptfrz(ji,jj) ) &
            &                 * r1_e1e2t(ji,jj) / rLfusisf * mskisf_par(ji,jj)   ! ( > 0 from isf to oce)
         pqoce(ji,jj) = - pfwf(ji,jj) * rLfusisf           * mskisf_par(ji,jj)   ! ocean/ice shelf flux assume to be equal to latent heat flux  ( > 0 from isf to oce)
         pqhc (ji,jj) =   pfwf(ji,jj) * ptfrz(ji,jj) * rcp * mskisf_par(ji,jj)   ! heat content flux                                            ( > 0 from isf to oce)
      END_2D
      !
   END SUBROUTINE isfpar_mlt_bg03

   SUBROUTINE isfpar_mlt_oasis( kt, Kmm, ptfrz, pqhc , pqoce, pfwf )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE isfpar_mlt_oasis  ***
      !!
      !! ** Purpose    : scale the fwf read from input file by the total amount received by the sbccpl interface
      !!
      !! ** Purpose    : - read ice shelf melt from forcing file and scale it by the input file total amount => pattern
      !!                 - compute total amount of fwf given by sbccpl (fwfisf_oasis)
      !!                 - scale fwf and compute heat fluxes
      !!
      !!---------------------------------------------------------------------
      INTEGER                    , INTENT(in   ) ::   kt                 ! current time step
      INTEGER                    , INTENT(in   ) ::   Kmm                !  ocean time level index
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   ptfrz              ! tbl freezing temp
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pqhc, pqoce, pfwf  ! heat content, latent heat and fwf fluxes
      !!
      INTEGER  ::   ji, jj           ! dummy loop indices
      REAL(wp), DIMENSION(A2D(0),2) ::   ztmp
      REAL(wp), DIMENSION(2)        ::   zbg
      !!--------------------------------------------------------------------
      !
      ! Read specified fwf from isf to oce
      CALL fld_read ( kt, 1, sf_isfpar_fwf )
      !
      DO_2D( 0, 0, 0, 0 )
         ! ice shelf 2d map
         pfwf(ji,jj) = sf_isfpar_fwf(1)%fnow(ji,jj,1)
         ztmp(ji,jj,1) = e1e2t(ji,jj) * pfwf(ji,jj)
         ztmp(ji,jj,2) = e1e2t(ji,jj) * fwfisf_oasis(ji,jj)
      END_2D
      !
      ! compute glob sum from input file and from atm->oce ice shelf fwf
      zbg = glob_2Dsum( 'isfpar_mlt', ztmp, cdelay = 'isfmlt_tag' )
      !
      ! Define fwf and qoce
      ! ocean heat flux is assume to be equal to the latent heat
      DO_2D( 0, 0, 0, 0 )
         pfwf (ji,jj) =   pfwf(ji,jj) * zbg(2) / zbg(1)    * mskisf_par(ji,jj)   ! scale fwf ( > 0 from isf to oce)
         pqoce(ji,jj) = - pfwf(ji,jj) * rLfusisf           * mskisf_par(ji,jj)   ! ocean heat flux    ( > 0 from isf to oce) (assumed to be the latent heat flux)
         pqhc (ji,jj) =   pfwf(ji,jj) * ptfrz(ji,jj) * rcp * mskisf_par(ji,jj)   ! heat content flux  ( > 0 from isf to oce)
      END_2D
      !
   END SUBROUTINE isfpar_mlt_oasis

END MODULE isfparmlt
