MODULE icethd_dh
   !!======================================================================
   !!                       ***  MODULE icethd_dh ***
   !!   seaice : thermodynamic growth and melt
   !!======================================================================
   !! History :       !  2003-05  (M. Vancoppenolle) Original code in 1D
   !!                 !  2005-06  (M. Vancoppenolle) 3D version
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd_dh        : vertical sea-ice growth and melt
   !!----------------------------------------------------------------------
   USE par_ice        ! SI3 parameters
   USE par_kind, ONLY : wp
   USE par_oce , ONLY : jpij
   USE phycst
   USE ice1D          ! sea-ice: thermodynamics variables
   USE icevar  , ONLY : ice_var_snwblow, ice_var_vremap

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_dh        ! called by ice_thd

   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd_dh
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_thd_dh  ***
      !!
      !! ** Purpose :   compute ice and snow thickness changes due to growth/melting
      !!
      !! ** Method  :   Ice/Snow surface melting arises from imbalance in surface fluxes
      !!                Bottom accretion/ablation arises from flux budget
      !!                Snow thickness can increase by precipitation and decrease by sublimation
      !!                If snow load excesses Archmiede limit, snow-ice is formed by
      !!                the flooding of sea-water in the snow
      !!
      !!                - Compute available flux of heat for surface ablation
      !!                - Compute snow and sea ice enthalpies
      !!                - Surface ablation and sublimation
      !!                - Bottom accretion/ablation
      !!                - Snow ice formation
      !!
      !! ** Note     :  h=max(0,h+dh) are often used to ensure positivity of h.
      !!                very small negative values can occur otherwise (e.g. -1.e-20)
      !!
      !! References : Bitz and Lipscomb, 1999, J. Geophys. Res.
      !!              Fichefet T. and M. Maqueda 1997, J. Geophys. Res., 102(C6), 12609-12646
      !!              Vancoppenolle, Fichefet and Bitz, 2005, Geophys. Res. Let.
      !!              Vancoppenolle et al.,2009, Ocean Modelling
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jk       ! dummy loop indices
      !
      REAL(wp) ::   ztmelts      ! local scalar
      REAL(wp) ::   zdum
      REAL(wp) ::   zt_i_new     ! bottom formation temperature
      REAL(wp) ::   z1_rho       ! 1/(rhos+rho0-rhoi)
      !
      REAL(wp) ::   zQm          ! enthalpy exchanged with the ocean (J/m2), >0 towards the ocean
      REAL(wp) ::   zEi          ! specific enthalpy of sea ice (J/kg)
      REAL(wp) ::   zEw          ! specific enthalpy of exchanged water (J/kg)
      REAL(wp) ::   zdE          ! specific enthalpy difference (J/kg)
      REAL(wp) ::   zfmdt        ! exchange mass flux x time step (J/m2), >0 towards the ocean
      REAL(wp) ::   zevap_rema   ! remaining mass flux from sublimation        (kg.m-2)
      REAL(wp) ::   zdeltah, zs_i_new, zds, zs_sni
      REAL(wp) ::   zswitch_sal
      !
      REAL(wp) ::   zq_top      ! heat for surface ablation                   (J.m-2)
      REAL(wp) ::   zq_bot      ! heat for bottom ablation                    (J.m-2)
      REAL(wp) ::   zf_tt       ! Heat budget to determine melting or freezing(W.m-2)
      REAL(wp), DIMENSION(jpij) ::   zsnw        ! distribution of snow after wind blowing
      !
      INTEGER , DIMENSION(nlay_i)     ::   icount    ! number of layers vanishing by melting
      REAL(wp), DIMENSION(nlay_i)     ::   zs_i      ! ice salinity
      REAL(wp), DIMENSION(0:nlay_i+1) ::   zh_i      ! ice layer thickness (m)
      REAL(wp), DIMENSION(0:nlay_s  ) ::   zh_s      ! snw layer thickness (m)
      REAL(wp), DIMENSION(0:nlay_s  ) ::   ze_s      ! snw layer enthalpy (J.m-3)
      REAL(wp), DIMENSION(0:nlay_i+1) ::   zh_i_old  ! old thickness
      REAL(wp), DIMENSION(0:nlay_i+1) ::   ze_i_old  ! old enthalpy
      REAL(wp), DIMENSION(0:nlay_i+1) ::   zs_i_old  ! old salt content
      !!------------------------------------------------------------------

      ! Discriminate between time varying salinity and constant
      SELECT CASE( nn_icesal )                  ! varying salinity or not
         CASE( 1 , 3 )   ;   zswitch_sal = 0._wp   ! prescribed salinity profile
         CASE( 2 , 4 )   ;   zswitch_sal = 1._wp   ! varying salinity profile
      END SELECT
      !
      ! snow distribution over ice after wind blowing
      CALL ice_var_snwblow( 1._wp - at_i_1d(1:npti), zsnw(1:npti) )
      !
      ! for snw-ice formation
      z1_rho = 1._wp / ( rhos+rho0-rhoi )
      !
      !                       ! ==================== !
      !                       ! Start main loop here !
      !                       ! ==================== !
      DO ji = 1, npti
         !                       ! ============================================== !
         !                       ! Available heat for surface and bottom ablation !
         !                       ! ============================================== !
         IF( .NOT.ln_cndflx .OR. ln_cndemulate ) THEN
            IF( t_su_1d(ji) >= rt0 ) THEN
               qml_ice_1d(ji) = qns_ice_1d(ji) + qsr_ice_1d(ji) - qtr_ice_top_1d(ji) - qcn_ice_top_1d(ji)
            ELSE
               qml_ice_1d(ji) = 0._wp
            ENDIF
         ENDIF
         !
         zq_top = MAX( 0._wp, qml_ice_1d(ji) * rDt_ice )
         zf_tt  = qcn_ice_bot_1d(ji) + qsb_ice_bot_1d(ji) + fhld_1d(ji) + qtr_ice_bot_1d(ji) * frq_m_1d(ji)
         zq_bot = MAX( 0._wp, zf_tt * rDt_ice )
         !
         ! initialize salinity
         IF( nn_icesal == 4 ) THEN   ;   zs_i(:) = sz_i_1d(ji,:)  ! use layer salinity if nn_icesal=4 
         ELSE                        ;   zs_i(:) = s_i_1d (ji)    !     bulk salinity otherwise (for conservation purpose)
         ENDIF
         !
         ! initialize ice layer thicknesses and enthalpies
         zs_i_old(0:nlay_i+1) = 0._wp
         ze_i_old(0:nlay_i+1) = 0._wp
         zh_i_old(0:nlay_i+1) = 0._wp
         zh_i    (0:nlay_i+1) = 0._wp
         DO jk = 1, nlay_i
            zs_i_old(jk) = h_i_1d(ji) * r1_nlay_i * zs_i  (jk)
            ze_i_old(jk) = h_i_1d(ji) * r1_nlay_i * e_i_1d(ji,jk)
            zh_i_old(jk) = h_i_1d(ji) * r1_nlay_i
            zh_i    (jk) = h_i_1d(ji) * r1_nlay_i
         END DO
         !
         ! initialize snw layer thicknesses and enthalpies
         zh_s(0) = 0._wp
         ze_s(0) = 0._wp
         DO jk = 1, nlay_s
            zh_s(jk) = h_s_1d(ji) * r1_nlay_s
            ze_s(jk) = e_s_1d(ji,jk)
         END DO
         !
         !                       ! ============ !
         !                       !     Snow     !
         !                       ! ============ !
         !
         ! Internal melting
         ! ----------------
         ! IF snow temperature is above freezing point, THEN snow melts (should not happen but sometimes it does)
         DO jk = 1, nlay_s
            IF( t_s_1d(ji,jk) > rt0 ) THEN
               hfx_res_1d    (ji) = hfx_res_1d    (ji) - ze_s(jk) * zh_s(jk) * a_i_1d(ji) * r1_Dt_ice   ! heat flux to the ocean [W.m-2], < 0
               wfx_snw_sum_1d(ji) = wfx_snw_sum_1d(ji) + rhos     * zh_s(jk) * a_i_1d(ji) * r1_Dt_ice   ! mass flux
               ! updates
               dh_s_itm(ji) =             dh_s_itm(ji) - zh_s(jk)
               h_s_1d  (ji) = MAX( 0._wp, h_s_1d  (ji) - zh_s(jk) )
               zh_s    (jk) = 0._wp
               ze_s    (jk) = 0._wp
            END IF
         END DO

         ! Snow precipitation
         !-------------------
         IF( sprecip_1d(ji) > 0._wp ) THEN
            zh_s(0) = zsnw(ji) * sprecip_1d(ji) * rDt_ice * r1_rhos / at_i_1d(ji)   ! thickness of precip
            ze_s(0) = MAX( 0._wp, - qprec_ice_1d(ji) )                              ! enthalpy of the precip (>0, J.m-3)
            !
            hfx_spr_1d(ji) = hfx_spr_1d(ji) + ze_s(0) * zh_s(0) * a_i_1d(ji) * r1_Dt_ice   ! heat flux from snow precip (>0, W.m-2)
            wfx_spr_1d(ji) = wfx_spr_1d(ji) - rhos    * zh_s(0) * a_i_1d(ji) * r1_Dt_ice   ! mass flux, <0
            !
            ! update thickness
            h_s_1d(ji) = h_s_1d(ji) + zh_s(0)
         ENDIF

         ! Snow melting
         ! ------------
         ! If heat still available (zq_top > 0)
         ! then all snw precip has been melted and we need to melt more snow
         DO jk = 0, nlay_s
            IF( zh_s(jk) > 0._wp .AND. zq_top > 0._wp ) THEN
               !
               zdum = - zq_top / MAX( ze_s(jk), epsi20 )   ! thickness change
               zdum = MAX( zdum , - zh_s(jk) )                 ! bound melting
               
               hfx_snw_1d    (ji) = hfx_snw_1d    (ji) - ze_s(jk) * zdum * a_i_1d(ji) * r1_Dt_ice   ! heat used to melt snow(W.m-2, >0)
               wfx_snw_sum_1d(ji) = wfx_snw_sum_1d(ji) - rhos     * zdum * a_i_1d(ji) * r1_Dt_ice   ! snow melting only = water into the ocean
               
               ! updates available heat + thickness
               dh_s_sum(ji) =              dh_s_sum(ji) + zdum
               zq_top       = MAX( 0._wp , zq_top       + zdum * ze_s(jk) )
               h_s_1d  (ji) = MAX( 0._wp , h_s_1d  (ji) + zdum )
               zh_s    (jk) = MAX( 0._wp , zh_s    (jk) + zdum )
!!$               IF( zh_s(jk) == 0._wp )   ze_s(jk) = 0._wp
               !
            ENDIF
         END DO

         ! Snow sublimation
         !-----------------
         ! qla_ice is always >=0 (upwards), heat goes to the atmosphere, therefore snow sublimates
         !    comment: not counted in mass/heat exchange in iceupdate.F90 since this is an exchange with atm. (not ocean)
         zdeltah    = MAX( - evap_ice_1d(ji) * r1_rhos * rDt_ice, - h_s_1d(ji) )   ! amount of snw that sublimates, < 0
         zevap_rema =        evap_ice_1d(ji)           * rDt_ice + zdeltah * rhos  ! remaining evap in kg.m-2 (used for ice sublimation later on)
         DO jk = 0, nlay_s
            zdum = MAX( -zh_s(jk), zdeltah ) ! snow layer thickness that sublimates, < 0
            !
            hfx_sub_1d    (ji) = hfx_sub_1d    (ji) + ze_s(jk) * zdum * a_i_1d(ji) * r1_Dt_ice  ! Heat flux of snw that sublimates [W.m-2], < 0
            wfx_snw_sub_1d(ji) = wfx_snw_sub_1d(ji) - rhos     * zdum * a_i_1d(ji) * r1_Dt_ice  ! Mass flux by sublimation

            ! update thickness
            h_s_1d(ji) = MAX( 0._wp , h_s_1d(ji) + zdum )
            zh_s  (jk) = MAX( 0._wp , zh_s  (jk) + zdum )
!!$            IF( zh_s(jk) == 0._wp )   ze_s(jk) = 0._wp

            ! update sublimation left
            zdeltah = MIN( zdeltah - zdum, 0._wp )
         END DO

         !
         !                       ! ============ !
         !                       !     Ice      !
         !                       ! ============ !

         ! Surface ice melting
         !--------------------
         DO jk = 1, nlay_i
            ztmelts = - rTmlt * sz_i_1d(ji,jk)   ! Melting point of layer k [C]

            IF( t_i_1d(ji,jk) >= (ztmelts+rt0) ) THEN   !-- Internal melting

               zEi            = - e_i_1d(ji,jk) * r1_rhoi             ! Specific enthalpy of layer k [J/kg, <0]
               zdE            =   0._wp                               ! Specific enthalpy difference (J/kg, <0)
               !                                                          set up at 0 since no energy is needed to melt water...(it is already melted)
               zdum           = MIN( 0._wp , - zh_i(jk) )             ! internal melting occurs when the internal temperature is above freezing
               !                                                          this should normally not happen, but sometimes, heat diffusion leads to this
               zfmdt          = - zdum * rhoi                         ! Recompute mass flux [kg/m2, >0]
               !
               dh_i_itm(ji)   = dh_i_itm(ji) + zdum                   ! Cumulate internal melting
               !
               hfx_res_1d(ji) = hfx_res_1d(ji) + zEi  * zfmdt           * a_i_1d(ji) * r1_Dt_ice    ! Heat flux to the ocean [W.m-2], <0
               !                                                                                        ice enthalpy zEi is "sent" to the ocean
               wfx_res_1d(ji) = wfx_res_1d(ji) - rhoi * zdum            * a_i_1d(ji) * r1_Dt_ice    ! Mass flux
               sfx_res_1d(ji) = sfx_res_1d(ji) - rhoi * zdum * zs_i(jk) * a_i_1d(ji) * r1_Dt_ice    ! Salt flux
               !
            ELSE                                        !-- Surface melting

               zEi            = - e_i_1d(ji,jk) * r1_rhoi             ! Specific enthalpy of layer k [J/kg, <0]
               zEw            =    rcp * ztmelts                      ! Specific enthalpy of resulting meltwater [J/kg, <0]
               zdE            =    zEi - zEw                          ! Specific enthalpy difference < 0

               zfmdt          = - zq_top / zdE                        ! Mass flux to the ocean [kg/m2, >0]

               zdum           = - zfmdt * r1_rhoi                     ! Melt of layer jk [m, <0]

               zdum           = MIN( 0._wp , MAX( zdum , - zh_i(jk) ) )    ! Melt of layer jk cannot exceed the layer thickness [m, <0]

               zq_top         = MAX( 0._wp , zq_top - zdum * rhoi * zdE ) ! update available heat

               dh_i_sum(ji)   = dh_i_sum(ji) + zdum                   ! Cumulate surface melt

               zfmdt          = - rhoi * zdum                         ! Recompute mass flux [kg/m2, >0]

               zQm            = zfmdt * zEw                           ! Energy of the melt water sent to the ocean [J/m2, <0]

               hfx_thd_1d(ji) = hfx_thd_1d(ji) + zEw  * zfmdt           * a_i_1d(ji) * r1_Dt_ice    ! Heat flux [W.m-2], < 0
               hfx_sum_1d(ji) = hfx_sum_1d(ji) - zdE  * zfmdt           * a_i_1d(ji) * r1_Dt_ice    ! Heat flux used in this process [W.m-2], > 0
               wfx_sum_1d(ji) = wfx_sum_1d(ji) - rhoi * zdum            * a_i_1d(ji) * r1_Dt_ice    ! Mass flux
               sfx_sum_1d(ji) = sfx_sum_1d(ji) - rhoi * zdum * zs_i(jk) * a_i_1d(ji) * r1_Dt_ice    ! Salt flux >0
               !
            END IF
            ! update thickness
            zh_i  (jk) = MAX( 0._wp, zh_i  (jk) + zdum )
            h_i_1d(ji) = MAX( 0._wp, h_i_1d(ji) + zdum )
            !
            ! update heat content (J.m-2), salt content and layer thickness
            zs_i_old(jk) = zs_i_old(jk) + zdum * zs_i(jk)
            ze_i_old(jk) = ze_i_old(jk) + zdum * e_i_1d(ji,jk)
            zh_i_old(jk) = zh_i_old(jk) + zdum
            !
            !
            ! Ice sublimation
            ! ---------------
            zdum               = MAX( - zh_i(jk) , - zevap_rema * r1_rhoi )
            !
            hfx_sub_1d(ji)     = hfx_sub_1d(ji)     + e_i_1d(ji,jk) * zdum            * a_i_1d(ji) * r1_Dt_ice ! Heat flux [W.m-2], < 0
            wfx_ice_sub_1d(ji) = wfx_ice_sub_1d(ji) - rhoi          * zdum            * a_i_1d(ji) * r1_Dt_ice ! Mass flux > 0
            sfx_sub_1d(ji)     = sfx_sub_1d(ji)     - rhoi          * zdum * zs_i(jk) * a_i_1d(ji) * r1_Dt_ice ! Salt flux >0
            !                                                                                                    clem: flux is sent to the ocean for simplicity
            !                                                                                                          but salt should remain in the ice except
            !                                                                                                          if all ice is melted. => must be corrected
            ! update remaining mass flux and thickness
            zevap_rema   = zevap_rema + zdum * rhoi
            zh_i  (jk)   = MAX( 0._wp, zh_i  (jk) + zdum )
            h_i_1d(ji)   = MAX( 0._wp, h_i_1d(ji) + zdum )
            dh_i_sub(ji) = dh_i_sub(ji) + zdum

            ! update heat content (J.m-2), salt content and layer thickness
            zs_i_old(jk) = zs_i_old(jk) + zdum * zs_i(jk)
            ze_i_old(jk) = ze_i_old(jk) + zdum * e_i_1d(ji,jk)
            zh_i_old(jk) = zh_i_old(jk) + zdum

            ! record which layers have disappeared (for bottom melting)
            !    => icount=0 : no layer has vanished
            !    => icount=5 : 5 layers have vanished
            IF( zh_i(jk) > 0._wp ) THEN ; icount(jk) = 0
            ELSE                        ; icount(jk) = 1 ; ENDIF

         END DO

         ! remaining "potential" evap is sent to ocean
         wfx_err_sub_1d(ji) = wfx_err_sub_1d(ji) - zevap_rema * a_i_1d(ji) * r1_Dt_ice  ! <=0 (net evap for the ocean in kg.m-2.s-1)


         ! Ice Basal growth
         !------------------
         ! Basal growth is driven by heat imbalance at the ice-ocean interface,
         ! between the inner conductive flux  (qcn_ice_bot), from the open water heat flux
         ! (fhld) and the sensible ice-ocean flux (qsb_ice_bot).
         ! qcn_ice_bot is positive downwards. qsb_ice_bot and fhld are positive to the ice
         !
         zs_i_new = 0._wp
         !
         IF(  zf_tt < 0._wp  ) THEN

            zs_i_new       = zswitch_sal * rn_sinew * sss_1d(ji) + ( 1. - zswitch_sal ) * zs_i(1)          ! New ice salinity
            
            ztmelts        = - rTmlt * zs_i_new                                                            ! New ice melting point (C)
            
            zt_i_new       = zswitch_sal * t_bo_1d(ji) + ( 1. - zswitch_sal) * t_i_1d(ji, nlay_i)
            
            zEi            = rcpi * ( zt_i_new - (ztmelts+rt0) ) &                                         ! Specific enthalpy of forming ice (J/kg, <0)
               &             - rLfus * ( 1.0 - ztmelts / ( MIN( zt_i_new - rt0, -epsi10 ) ) ) + rcp * ztmelts
            
            zEw            = rcp  * ( t_bo_1d(ji) - rt0 )                                                  ! Specific enthalpy of seawater (J/kg, < 0)
            
            zdE            = zEi - zEw                                                                     ! Specific enthalpy difference (J/kg, <0)
            
            dh_i_bog(ji)   = rDt_ice * MAX( 0._wp , zf_tt / ( zdE * rhoi ) )

            ! Contribution to Energy and Salt Fluxes
            zfmdt = - rhoi * dh_i_bog(ji)                                                                  ! Mass flux x time step (kg/m2, < 0)

            hfx_thd_1d(ji) = hfx_thd_1d(ji) + zEw  * zfmdt                   * a_i_1d(ji) * r1_Dt_ice   ! Heat flux to the ocean [W.m-2], >0
            hfx_bog_1d(ji) = hfx_bog_1d(ji) - zdE  * zfmdt                   * a_i_1d(ji) * r1_Dt_ice   ! Heat flux used in this process [W.m-2], <0
            wfx_bog_1d(ji) = wfx_bog_1d(ji) - rhoi * dh_i_bog(ji)            * a_i_1d(ji) * r1_Dt_ice   ! Mass flux, <0
            sfx_bog_1d(ji) = sfx_bog_1d(ji) - rhoi * dh_i_bog(ji) * zs_i_new * a_i_1d(ji) * r1_Dt_ice   ! Salt flux, <0

            ! update thickness
            zh_i(nlay_i+1) = zh_i(nlay_i+1) + dh_i_bog(ji)
            h_i_1d(ji)     = h_i_1d(ji)     + dh_i_bog(ji)

            ! update heat content (J.m-2), salt content and layer thickness
            zs_i_old(nlay_i+1) = zs_i_old(nlay_i+1) + dh_i_bog(ji) * zs_i_new
            ze_i_old(nlay_i+1) = ze_i_old(nlay_i+1) + dh_i_bog(ji) * (-zEi * rhoi)
            zh_i_old(nlay_i+1) = zh_i_old(nlay_i+1) + dh_i_bog(ji)

         ENDIF

         ! Ice Basal melt
         !---------------
         DO jk = nlay_i, 1, -1
            IF(  zf_tt  >  0._wp  .AND. jk > icount(jk) ) THEN   ! do not calculate where layer has already disappeared by surface melting

               ztmelts = - rTmlt * sz_i_1d(ji,jk)  ! Melting point of layer jk (C)

               IF( t_i_1d(ji,jk) >= (ztmelts+rt0) ) THEN   !-- Internal melting

                  zEi            = - e_i_1d(ji,jk) * r1_rhoi     ! Specific enthalpy of melting ice (J/kg, <0)
                  zdE            = 0._wp                         ! Specific enthalpy difference   (J/kg, <0)
                  !                                                  set up at 0 since no energy is needed to melt water...(it is already melted)
                  zdum           = MIN( 0._wp , - zh_i(jk) )  ! internal melting occurs when the internal temperature is above freezing
                  !                                                  this should normally not happen, but sometimes, heat diffusion leads to this
                  dh_i_itm (ji)  = dh_i_itm(ji) + zdum
                  !
                  zfmdt          = - zdum * rhoi                 ! Mass flux x time step > 0
                  !
                  hfx_res_1d(ji) = hfx_res_1d(ji) + zEi  * zfmdt           * a_i_1d(ji) * r1_Dt_ice   ! Heat flux to the ocean [W.m-2], <0
                  !                                                                                       ice enthalpy zEi is "sent" to the ocean
                  wfx_res_1d(ji) = wfx_res_1d(ji) - rhoi * zdum            * a_i_1d(ji) * r1_Dt_ice   ! Mass flux
                  sfx_res_1d(ji) = sfx_res_1d(ji) - rhoi * zdum * zs_i(jk) * a_i_1d(ji) * r1_Dt_ice   ! Salt flux
                  !
               ELSE                                        !-- Basal melting

                  zEi            = - e_i_1d(ji,jk) * r1_rhoi                       ! Specific enthalpy of melting ice (J/kg, <0)
                  zEw            = rcp * ztmelts                                   ! Specific enthalpy of meltwater (J/kg, <0)
                  zdE            = zEi - zEw                                       ! Specific enthalpy difference   (J/kg, <0)

                  zfmdt          = - zq_bot / zdE                                  ! Mass flux x time step (kg/m2, >0)

                  zdum           = - zfmdt * r1_rhoi                               ! Gross thickness change

                  zdum           = MIN( 0._wp , MAX( zdum, - zh_i(jk) ) )          ! bound thickness change

                  zq_bot         = MAX( 0._wp , zq_bot - zdum * rhoi * zdE )       ! update available heat. MAX is necessary for roundup errors

                  dh_i_bom(ji)   = dh_i_bom(ji) + zdum                             ! Update basal melt

                  zfmdt          = - zdum * rhoi                                   ! Mass flux x time step > 0

                  zQm            = zfmdt * zEw                                     ! Heat exchanged with ocean

                  hfx_thd_1d(ji) = hfx_thd_1d(ji) + zEw  * zfmdt           * a_i_1d(ji) * r1_Dt_ice   ! Heat flux to the ocean [W.m-2], <0
                  hfx_bom_1d(ji) = hfx_bom_1d(ji) - zdE  * zfmdt           * a_i_1d(ji) * r1_Dt_ice   ! Heat used in this process [W.m-2], >0
                  wfx_bom_1d(ji) = wfx_bom_1d(ji) - rhoi * zdum            * a_i_1d(ji) * r1_Dt_ice   ! Mass flux
                  sfx_bom_1d(ji) = sfx_bom_1d(ji) - rhoi * zdum * zs_i(jk) * a_i_1d(ji) * r1_Dt_ice   ! Salt flux
                  !
               ENDIF
               ! update thickness
               zh_i  (jk) = MAX( 0._wp, zh_i  (jk) + zdum )
               h_i_1d(ji) = MAX( 0._wp, h_i_1d(ji) + zdum )
               !
               ! update heat content (J.m-2), salt content and layer thickness
               zs_i_old(jk) = zs_i_old(jk) + zdum * zs_i(jk)
               ze_i_old(jk) = ze_i_old(jk) + zdum * e_i_1d(ji,jk)
               zh_i_old(jk) = zh_i_old(jk) + zdum
            ENDIF
         END DO

         ! Remove snow if ice has melted entirely
         ! --------------------------------------
         IF( h_i_1d(ji) == 0._wp ) THEN
            DO jk = 0, nlay_s
               ! mass & energy loss to the ocean
               hfx_res_1d(ji) = hfx_res_1d(ji) - ze_s(jk) * zh_s(jk) * a_i_1d(ji) * r1_Dt_ice  ! heat flux to the ocean [W.m-2], < 0
               wfx_res_1d(ji) = wfx_res_1d(ji) + rhos     * zh_s(jk) * a_i_1d(ji) * r1_Dt_ice  ! mass flux

               ! update thickness and energy
               h_s_1d(ji) = 0._wp
               ze_s  (jk) = 0._wp
               zh_s  (jk) = 0._wp
            END DO
         ENDIF

         ! Snow-Ice formation
         ! ------------------
         ! When snow load exceeds Archimede's limit, snow-ice interface goes down under sea-level,
         ! flooding of seawater transforms snow into ice. Thickness that is transformed is dh_snowice (positive for the ice)
         !
         dh_snowice(ji) = MAX( 0._wp , ( rhos * h_s_1d(ji) + (rhoi-rho0) * h_i_1d(ji) ) * z1_rho )

         h_i_1d(ji)    = h_i_1d(ji) + dh_snowice(ji)
         h_s_1d(ji)    = h_s_1d(ji) - dh_snowice(ji)

         ! Contribution to energy flux to the ocean [J/m2], >0 (if sst<0)
         zfmdt          = ( rhos - rhoi ) * dh_snowice(ji)    ! <0
         zEw            = rcp * sst_1d(ji)
         zQm            = zfmdt * zEw

         hfx_thd_1d(ji) = hfx_thd_1d(ji) + zEw        * zfmdt * a_i_1d(ji) * r1_Dt_ice ! Heat flux
         sfx_sni_1d(ji) = sfx_sni_1d(ji) + sss_1d(ji) * zfmdt * a_i_1d(ji) * r1_Dt_ice ! Salt flux

         ! Case constant salinity in time: virtual salt flux to keep salinity constant
         IF( nn_icesal == 1 .OR. nn_icesal == 3 )  THEN
            sfx_bri_1d(ji) = sfx_bri_1d(ji) - sss_1d(ji) * zfmdt              * a_i_1d(ji) * r1_Dt_ice  &  ! put back sss_m     into the ocean
               &                            - zs_i(1) * dh_snowice(ji) * rhoi * a_i_1d(ji) * r1_Dt_ice     ! and get  rn_icesal from the ocean
         ENDIF

         ! Mass flux: All snow is thrown in the ocean, and seawater is taken to replace the volume
         wfx_sni_1d    (ji) = wfx_sni_1d    (ji) - dh_snowice(ji) * rhoi * a_i_1d(ji) * r1_Dt_ice
         wfx_snw_sni_1d(ji) = wfx_snw_sni_1d(ji) + dh_snowice(ji) * rhos * a_i_1d(ji) * r1_Dt_ice

         ! update thickness
         zh_i(0) = zh_i(0) + dh_snowice(ji)
         zdeltah =           dh_snowice(ji)

         ! update heat content (J.m-2), salt content and layer thickness
         zs_i_old(0) = zs_i_old(0) - zfmdt * sss_1d(ji) * r1_rhoi      ! clem: s(0) could be > rn_sinew*sss
         zh_i_old(0) = zh_i_old(0) + dh_snowice(ji)
         ze_i_old(0) = ze_i_old(0) + zfmdt * zEw           ! 1st part (sea water enthalpy)

         !
         DO jk = nlay_s, 0, -1   ! flooding of snow starts from the base
            zdum        = MIN( zdeltah, zh_s(jk) )         ! amount of snw that floods, > 0
            zh_s(jk)    = MAX( 0._wp, zh_s(jk) - zdum )    ! remove some snow thickness
            ze_i_old(0) = ze_i_old(0) + zdum * ze_s(jk)    ! 2nd part (snow enthalpy)
            ! update dh_snowice
            zdeltah     = MAX( 0._wp, zdeltah - zdum )
         END DO
         !
         !
!!$      ! --- Update snow diags --- !
!!$      !!clem: this is wrong. dh_s_tot is not used anyway
!!$      DO ji = 1, npti
!!$         dh_s_tot(ji) = dh_s_tot(ji) + dh_s_sum(ji) + zdeltah + zdh_s_sub(ji) - dh_snowice(ji)
!!$      END DO
         !
         ! Remapping of snw enthalpy on a regular grid
         !--------------------------------------------
         e_s_1d(ji,:) = snw_ent( zh_s, ze_s )
   
         ! recalculate t_s_1d from e_s_1d
         IF( h_s_1d(ji) > 0._wp ) THEN
            DO jk = 1, nlay_s
               t_s_1d(ji,jk) = rt0 + ( - e_s_1d(ji,jk) * r1_rhos * r1_rcpi + rLfus * r1_rcpi )
            END DO
         ELSE
            DO jk = 1, nlay_s
               t_s_1d(ji,jk) = rt0
            END DO
         ENDIF

         ! Remapping of ice enthalpy/salt on a regular grid
         !-------------------------------------------------
                                CALL ice_var_vremap( zh_i_old, ze_i_old, e_i_1d (ji,:) )
         IF( nn_icesal == 4 )   CALL ice_var_vremap( zh_i_old, zs_i_old, sz_i_1d(ji,:) )
         IF( nn_icesal == 2 )   THEN ! Update ice salinity from snow-ice and bottom growth
            zs_sni = sss_1d(ji) * ( rhoi - rhos ) * r1_rhoi                                       ! salinity of snow ice
            zds    =       ( zs_sni   - s_i_1d(ji) ) * dh_snowice(ji) / MAX( epsi10, h_i_1d(ji) ) ! snow-ice    
            zds    = zds + ( zs_i_new - s_i_1d(ji) ) * dh_i_bog  (ji) / MAX( epsi10, h_i_1d(ji) ) ! bottom growth
            !
            s_i_1d(ji) = s_i_1d(ji) + zds
         ENDIF
         
      END DO ! npti
      !                       ! ================== !
      !                       ! End main loop here !
      !                       ! ================== !
 
      ! --- ensure that a_i = 0 & h_s = 0 where h_i = 0 ---
      WHERE( h_i_1d(1:npti) == 0._wp )
         a_i_1d (1:npti) = 0._wp
         h_s_1d (1:npti) = 0._wp
         t_su_1d(1:npti) = rt0
      END WHERE

   END SUBROUTINE ice_thd_dh

   FUNCTION snw_ent( ph_old, pe_old )
      !!-------------------------------------------------------------------
      !!               ***   ROUTINE snw_ent  ***
      !! ** Purpose :
      !!           This routine computes new vertical grids in the snow,
      !!           and consistently redistributes temperatures.
      !!           Redistribution is made so as to ensure to energy conservation
      !!
      !!
      !! ** Method  : linear conservative remapping
      !!
      !! ** Steps : 1) cumulative integrals of old enthalpies/thicknesses
      !!            2) linear remapping on the new layers
      !!
      !! ------------ cum0(0)                        ------------- cum1(0)
      !!                                    NEW      -------------
      !! ------------ cum0(1)               ==>      -------------
      !!     ...                                     -------------
      !! ------------                                -------------
      !! ------------ cum0(nlay_s+1)                 ------------- cum1(nlay_s)
      !!
      !!
      !! References : Bitz & Lipscomb, JGR 99; Vancoppenolle et al., GRL, 2005
      !!-------------------------------------------------------------------
      REAL(wp), DIMENSION(0:nlay_s), INTENT(in) ::   ph_old             ! old thicknesses (m)
      REAL(wp), DIMENSION(0:nlay_s), INTENT(in) ::   pe_old             ! old enthlapies (J.m-3)
      REAL(wp), DIMENSION(1:nlay_s)             ::   snw_ent            ! new enthlapies (J.m-3, remapped)
      !
      INTEGER  :: ji         !  dummy loop indices
      INTEGER  :: jk0, jk1   !  old/new layer indices
      !
      REAL(wp), DIMENSION(0:nlay_s+1) ::   zeh_cum0, zh_cum0   ! old cumulative enthlapies and layers interfaces
      REAL(wp), DIMENSION(0:nlay_s)   ::   zeh_cum1, zh_cum1   ! new cumulative enthlapies and layers interfaces
      REAL(wp)                        ::   zhnew               ! new layers thicknesses
      !!-------------------------------------------------------------------

      !--------------------------------------------------------------------------
      !  1) Cumulative integral of old enthalpy * thickness and layers interfaces
      !--------------------------------------------------------------------------
      zeh_cum0(0) = 0._wp
      zh_cum0 (0) = 0._wp
      DO jk0 = 1, nlay_s+1
         zeh_cum0(jk0) = zeh_cum0(jk0-1) + pe_old(jk0-1) * ph_old(jk0-1)
         zh_cum0 (jk0) = zh_cum0 (jk0-1) + ph_old(jk0-1)
      END DO

      !------------------------------------
      !  2) Interpolation on the new layers
      !------------------------------------
      ! new layer thickesses
      zhnew = SUM( ph_old(0:nlay_s) ) * r1_nlay_s

      ! new layers interfaces
      zh_cum1(0) = 0._wp
      DO jk1 = 1, nlay_s
         zh_cum1(jk1) = zh_cum1(jk1-1) + zhnew
      END DO

      zeh_cum1(0:nlay_s) = 0._wp
      ! new cumulative q*h => linear interpolation
      DO jk0 = 1, nlay_s+1
         DO jk1 = 1, nlay_s-1
            IF( zh_cum1(jk1) <= zh_cum0(jk0) .AND. zh_cum1(jk1) > zh_cum0(jk0-1) )   THEN
               zeh_cum1(jk1) = ( zeh_cum0(jk0-1) * ( zh_cum0(jk0) - zh_cum1(jk1  ) ) +  &
                  &              zeh_cum0(jk0  ) * ( zh_cum1(jk1) - zh_cum0(jk0-1) ) )  &
                  &            / ( zh_cum0(jk0) - zh_cum0(jk0-1) )
            ENDIF
         END DO
      END DO
      ! to ensure that total heat content is strictly conserved, set:
      zeh_cum1(nlay_s) = zeh_cum0(nlay_s+1)

      ! new enthalpies
      DO jk1 = 1, nlay_s
         snw_ent(jk1) = MAX( 0._wp, zeh_cum1(jk1) - zeh_cum1(jk1-1) ) / MAX( zhnew, epsi20 ) ! max for roundoff error
      END DO


   END FUNCTION snw_ent
   
#else
   !!----------------------------------------------------------------------
   !!   Default option                                NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icethd_dh
