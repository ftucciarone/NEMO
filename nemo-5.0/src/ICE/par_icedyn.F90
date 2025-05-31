MODULE par_icedyn
   !!======================================================================
   !!                        ***  par_icedyn  ***
   !! Ice :   set the ice dynamics parameters
   !!======================================================================
   !! History :  4.x  !  2023     (Rousset)  Original code
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_kind          ! kind parameters

   IMPLICIT NONE
   PUBLIC

   !!----------------------------------------------------------------------
   !!                   shared namelist parameters
   !!----------------------------------------------------------------------
   !
   !                                     !!** ice-dynamics namelist (namdyn) **
   REAL(wp), PUBLIC ::   rn_ishlat        !: lateral boundary condition for sea-ice
   LOGICAL , PUBLIC ::   ln_landfast_L16  !: landfast ice parameterizationfrom lemieux2016
   REAL(wp), PUBLIC ::   rn_lf_depfra     !:    fraction of ocean depth that ice must reach to initiate landfast ice
   REAL(wp), PUBLIC ::   rn_lf_bfr        !:    maximum bottom stress per unit area of contact (lemieux2016) or per unit volume (home)
   REAL(wp), PUBLIC ::   rn_lf_relax      !:    relaxation time scale (s-1) to reach static friction
   REAL(wp), PUBLIC ::   rn_lf_tensile    !:    isotropic tensile strength
   !
   !                                     !!** ice-rheology namelist (namdyn_rhg) **
   LOGICAL , PUBLIC ::   ln_rhg_VP        !: VP  rheology
   INTEGER , PUBLIC ::   nn_vp_nout       !:   Number of outer iterations
   INTEGER , PUBLIC ::   nn_vp_ninn       !:   Number of inner iterations (linear system solver)
   INTEGER , PUBLIC ::   nn_vp_chkcvg     !:   Number of iterations every each convergence is checked
   !
   LOGICAL , PUBLIC ::   ln_rhg_EVP       !: EVP rheology switch, used for rdgrft and rheology
   LOGICAL , PUBLIC ::   ln_rhg_EAP       !: EAP rheology switch, used for rdgrft and rheology
   LOGICAL , PUBLIC ::   ln_aEVP          !:   using adaptive EVP (T or F)
   REAL(wp), PUBLIC ::   rn_creepl        !:   creep limit (has to be low enough, circa 10-9 m/s, depending on rheology)
   REAL(wp), PUBLIC ::   rn_ecc           !:   eccentricity of the elliptical yield curve
   INTEGER , PUBLIC ::   nn_nevp          !:   number of iterations for subcycling
   REAL(wp), PUBLIC ::   rn_relast        !:   ratio => telast/rDt_ice (1/3 or 1/9 depending on nb of subcycling nevp)
   INTEGER , PUBLIC ::   nn_rhg_chkcvg    !:   check ice rheology convergence
   !
   !                                     !!** ice-strength namelist (namdyn_rdgrft) **
   LOGICAL , PUBLIC ::   ln_str_H79       !: ice strength parameterization: Hibler 79 (can be used in rheology)
   REAL(wp), PUBLIC ::   rn_crhg          !:    determines changes in ice strength
   REAL(wp), PUBLIC ::   rn_pstar         !:    determines ice strength, Hibler 79
   LOGICAL , PUBLIC ::   ln_str_R75       !: ice strength parameterization: Rothrock 75
   REAL(wp), PUBLIC ::   rn_pe_rdg        !:    coef accounting for frictional dissipation
   LOGICAL , PUBLIC ::   ln_str_CST       !: ice strength parameterization: Constant
   REAL(wp), PUBLIC ::   rn_str           !:    constant value of ice strength
   !
#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE par_icedyn
