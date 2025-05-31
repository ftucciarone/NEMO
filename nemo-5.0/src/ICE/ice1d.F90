MODULE ice1D
   !!======================================================================
   !!                       ***  MODULE ice1D  ***
   !! sea-ice :   Ice thermodynamics variables in 1D
   !!=====================================================================
   !! History :  3.0  !  2002-11  (C. Ethe)          original code
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   USE par_ice , ONLY :   nlay_i, nlay_s, jpl, ln_zdf_chkcvg, ln_sal_chk   ! number of ice/snow layers and categories
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice1D_alloc   ! called by icestp.F90

   !!----------------------
   !! * 1D Module variables
   !!----------------------
   !: In ice thermodynamics, to spare memory, the vectors are folded
   !: from 1D to 2D vectors. The following variables, with ending _1d
   !: are the variables corresponding to 2d vectors

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nptidx !: selected points for ice thermo
   INTEGER , PUBLIC                                  ::   npti   !  number of selected points

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qlead_1d     
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qtr_ice_bot_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qsr_ice_1d  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qns_ice_1d  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t_bo_1d     
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   rn_amax_1d
   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qml_ice_1d     !: heat available for snow / ice surface melting [W/m2] 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qcn_ice_1d     !: heat available for snow / ice surface sublimation [W/m2] 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qtr_ice_top_1d !: solar flux transmitted below the ice surface [W/m2] 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t1_ice_1d      !: temperature of the 1st layer        (ln_cndflx=T) [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   cnd_ice_1d     !: conductivity at the top of ice/snow (ln_cndflx=T) [W/K/m2]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_sum_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_bom_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_bog_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_dif_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_opw_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_snw_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_dyn_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_err_dif_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qt_oce_ai_1d

   ! heat flux associated with ice-atmosphere mass exchange
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_sub_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_spr_1d

   ! heat flux associated with ice-ocean mass exchange
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_thd_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   hfx_res_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_snw_sni_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_snw_sum_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_snw_dyn_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_sub_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_snw_sub_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_ice_sub_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_err_sub_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_lam_1d 

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_bog_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_bom_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_sum_1d  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_sni_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_opw_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_res_1d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_spr_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_dyn_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   wfx_pnd_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_bri_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_bog_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_bom_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_sum_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_sni_1d    
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_opw_1d   
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_res_1d  
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_sub_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_lam_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sfx_dyn_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sprecip_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   at_i_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ato_i_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qsb_ice_bot_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   fhld_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dqns_ice_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   evap_ice_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qprec_ice_1d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t_su_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   t_si_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   a_i_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   a_ib_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   h_i_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   h_ib_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   h_s_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_s_tot      !: Snow accretion/ablation        [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_sum      !: Ice surface ablation [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_itm      !: Ice internal ablation [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_bom      !: Ice bottom ablation  [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_bog      !: Ice bottom accretion  [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_i_sub      !: Ice surface sublimation [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_s_sum      !: Snow surface melt [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_s_itm      !: Snow internal melt [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   dh_snowice    !: Snow ice formation             [m of ice]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   s_i_1d        !: Ice bulk salinity [ppt]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   v_i_1d        !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   v_s_1d        !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sv_i_1d       !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   oa_i_1d       !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   o_i_1d        !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   a_ip_1d       !: ice ponds
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   v_ip_1d       !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   h_ip_1d       !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   v_il_1d       !: Ice pond lid
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   h_il_1d       !:

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_s_1d      !: corresponding to the 2D var  t_s
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_i_1d      !: corresponding to the 2D var  t_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sz_i_1d     !: profiled ice salinity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   szv_i_1d    !: profiled ice salinity content
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   e_i_1d      !:    Ice  enthalpy per unit volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   e_s_1d      !:    Snow enthalpy per unit volume

   ! Conduction flux diagnostics (SIMIP)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qcn_ice_bot_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   qcn_ice_top_1d

   ! surface fields from the ocean
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sst_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   sss_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   frq_m_1d

   ! convergence check
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   tice_cvgerr_1d   !: convergence of ice/snow temp (dT)          [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   tice_cvgstp_1d   !: convergence of ice/snow temp (subtimestep) [-]

   ! sanity checks for salinity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   sneg_drain_1d , sneg_flush_1d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   cfl_drain_1d  , cfl_flush_1d
   ! 
   !!----------------------
   !! * 2D Module variables
   !!----------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   a_i_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   v_i_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   v_s_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   oa_i_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sv_i_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   a_ip_2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   v_ip_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   v_il_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   t_su_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   h_i_2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   s_i_2d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   a_ib_2d
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   h_ib_2d

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e_i_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   e_s_2d 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   szv_i_2d 
   
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION ice1D_alloc()
      !!---------------------------------------------------------------------!
      !!                ***  ROUTINE ice1D_alloc ***
      !!---------------------------------------------------------------------!
      INTEGER ::   ice1D_alloc   ! return value
      INTEGER ::   ierr(15), ii
      !!---------------------------------------------------------------------!
      ierr(:) = 0
      ii = 0
      
      ! * Ice global state variables
      ii = ii + 1
      ALLOCATE( nptidx  (jpij) , &
         &      h_i_1d  (jpij) , a_i_1d (jpij) , v_i_1d  (jpij) , &
         &      v_s_1d  (jpij) , h_s_1d (jpij) ,                  &
         &      s_i_1d  (jpij) , sv_i_1d(jpij) , o_i_1d  (jpij) , oa_i_1d (jpij) , &
         &      a_ip_1d (jpij) , v_ip_1d(jpij) , h_ip_1d (jpij) , &
         &      v_il_1d (jpij) , h_il_1d(jpij) ,                  &
         &      t_su_1d (jpij) , t_s_1d (jpij,nlay_s) , t_i_1d (jpij,nlay_i), sz_i_1d(jpij,nlay_i) , szv_i_1d(jpij,nlay_i) , &
         &      ato_i_1d(jpij) , STAT=ierr(ii) )  
      ii = ii + 1
      ALLOCATE( e_i_1d(jpij,nlay_i) , e_s_1d(jpij,nlay_s) , STAT=ierr(ii) )

      ! * Before values of global variables
      ii = ii + 1
      ALLOCATE( a_ib_1d (jpij) , h_ib_1d (jpij) , STAT=ierr(ii) )

      ! * heat fluxes
      ii = ii + 1
      ALLOCATE( qsr_ice_1d  (jpij) , qsb_ice_bot_1d(jpij) , qns_ice_1d    (jpij) , qml_ice_1d(jpij) , &
         &      qprec_ice_1d(jpij) , dqns_ice_1d   (jpij) , qlead_1d      (jpij) , fhld_1d   (jpij) , &
         &      qt_oce_ai_1d(jpij) , qtr_ice_bot_1d(jpij) , qtr_ice_top_1d(jpij) , &
         &      hfx_sum_1d(jpij) , hfx_bom_1d    (jpij) , hfx_bog_1d(jpij) ,   & 
         &      hfx_dif_1d(jpij) , hfx_opw_1d    (jpij) , hfx_dyn_1d(jpij) ,   &
         &      hfx_thd_1d(jpij) , hfx_spr_1d    (jpij) ,                      &
         &      hfx_snw_1d(jpij) , hfx_sub_1d    (jpij) ,                      &
         &      hfx_res_1d(jpij) , hfx_err_dif_1d(jpij) , STAT=ierr(ii) )     
      ii = ii + 1
      ALLOCATE( qcn_ice_1d(jpij) , qcn_ice_bot_1d(jpij) , qcn_ice_top_1d(jpij) , &
         &      cnd_ice_1d(jpij) , t1_ice_1d     (jpij) , STAT=ierr(ii) )
      
      ! * mass fluxes
      ii = ii + 1
      ALLOCATE( sprecip_1d    (jpij) , evap_ice_1d   (jpij) ,                         &
         &      wfx_snw_sni_1d(jpij) , wfx_spr_1d    (jpij) , wfx_snw_sum_1d(jpij) ,  &
         &      wfx_sub_1d    (jpij) , wfx_bog_1d    (jpij) , wfx_bom_1d    (jpij) ,  &
         &      wfx_sum_1d    (jpij) , wfx_sni_1d    (jpij) , wfx_opw_1d    (jpij) , wfx_res_1d    (jpij) ,  &
         &      wfx_snw_sub_1d(jpij) , wfx_snw_dyn_1d(jpij) , wfx_ice_sub_1d(jpij) , wfx_err_sub_1d(jpij) ,  &
         &      wfx_lam_1d    (jpij) , wfx_dyn_1d    (jpij) , wfx_pnd_1d    (jpij) , STAT=ierr(ii) )
      
      ! * salt fluxes
      ii = ii + 1
      ALLOCATE( sfx_bri_1d(jpij) , sfx_bog_1d (jpij) , sfx_bom_1d (jpij) , sfx_sum_1d (jpij),  &
         &      sfx_sni_1d(jpij) , sfx_opw_1d (jpij) , sfx_res_1d (jpij) , sfx_sub_1d (jpij),  &
         &      sfx_lam_1d(jpij) , sfx_dyn_1d(jpij)  , STAT=ierr(ii) )
      
      ! * thermo tickness change
      ii = ii + 1
      ALLOCATE( dh_s_tot(jpij) , dh_i_sum(jpij) , dh_i_itm(jpij) , dh_i_bom  (jpij) , dh_i_bog(jpij) ,  &    
         &      dh_i_sub(jpij) , dh_s_sum(jpij) , dh_s_itm(jpij) , dh_snowice(jpij) , STAT=ierr(ii)  )

      ! * other
      ii = ii + 1
      ALLOCATE( at_i_1d(jpij) , rn_amax_1d(jpij) , t_si_1d (jpij) , t_bo_1d (jpij) , &
         &      sst_1d (jpij) , sss_1d    (jpij) , frq_m_1d(jpij) , STAT=ierr(ii)  )
      !
      ! * 2d arrays
      ii = ii + 1
      ALLOCATE( a_i_2d (jpij,jpl) , a_ib_2d(jpij,jpl) , h_i_2d (jpij,jpl) , h_ib_2d(jpij,jpl) ,  &
         &      v_i_2d (jpij,jpl) , v_s_2d (jpij,jpl) , oa_i_2d(jpij,jpl) , sv_i_2d(jpij,jpl) ,  &
         &      a_ip_2d(jpij,jpl) , v_ip_2d(jpij,jpl) , t_su_2d(jpij,jpl) , v_il_2d(jpij,jpl) , s_i_2d(jpij,jpl) , &
         &      STAT=ierr(ii) )
      !
      ! * 3d arrays
      ii = ii + 1
      ALLOCATE( e_i_2d(jpij,nlay_i,jpl) , e_s_2d(jpij,nlay_s,jpl) , szv_i_2d(jpij,nlay_i,jpl) , STAT=ierr(ii) )

      ! * checks
      IF( ln_zdf_chkcvg ) THEN
         ii = ii + 1
         ALLOCATE( tice_cvgerr_1d(jpij) , tice_cvgstp_1d(jpij) , STAT=ierr(ii) )
      ENDIF
      IF( ln_sal_chk ) THEN
         ii = ii + 1
         ALLOCATE( sneg_drain_1d(jpij), sneg_flush_1d(jpij), cfl_drain_1d(jpij), cfl_flush_1d(jpij), STAT=ierr(ii) )
      ENDIF
      
      ice1D_alloc = MAXVAL( ierr(:) )
      IF( ice1D_alloc /= 0 )   CALL ctl_stop( 'STOP',  'ice1D_alloc: failed to allocate arrays.'  )
      !
   END FUNCTION ice1D_alloc
   
#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE ice1D_alloc          ! Empty routine
   END SUBROUTINE ice1D_alloc
#endif
 
   !!======================================================================
END MODULE ice1D
