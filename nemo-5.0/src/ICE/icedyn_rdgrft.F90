MODULE icedyn_rdgrft
   !!======================================================================
   !!                       ***  MODULE icedyn_rdgrft ***
   !!    sea-ice : Mechanical impact on ice thickness distribution
   !!======================================================================
   !! History :       !  2006-02  (M. Vancoppenolle) Original code
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_dyn_rdgrft       : ridging/rafting of sea ice
   !!   ice_dyn_rdgrft_init  : initialization of ridging/rafting of sea ice
   !!   ice_strength         : ice strength calculation
   !!----------------------------------------------------------------------
   USE par_ice        ! SI3 parameters
   USE par_icedyn     ! SI3 dynamics parameters
   USE phycst         ! physical constants (ocean directory)
   USE sbc_oce , ONLY : sss_m, sst_m   ! surface boundary condition: ocean fields
   USE ice1D          ! sea-ice: thermodynamics
   USE ice            ! sea-ice: variables
   USE icetab         ! sea-ice: 1D <==> 2D transformation
   USE icevar  , ONLY : ice_var_roundoff
   USE icectl         ! sea-ice: control prints
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lbclnk         ! lateral boundary conditions (or mpp links)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dyn_rdgrft        ! called by icestp
   PUBLIC   ice_dyn_rdgrft_init   ! called by icedyn
   PUBLIC   ice_strength          ! called by icedyn_rhg_evp

   INTEGER ::              nice_str   ! choice of the type of strength
   !                                        ! associated indices:
   INTEGER, PARAMETER ::   np_strh79     = 1   ! Hibler 79
   INTEGER, PARAMETER ::   np_strr75     = 2   ! Rothrock 75
   INTEGER, PARAMETER ::   np_strcst     = 3   ! Constant value

   ! Variables shared among ridging subroutines
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)     ::   closing_net     ! net rate at which area is removed    (1/s)
      !                                                               ! (ridging ice area - area of new ridges) / dt
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)     ::   opning          ! rate of opening due to divergence/shear
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)     ::   closing_gross   ! rate at which area removed, not counting area of new ridges
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   apartf          ! participation function; fraction of ridging/closing associated w/ category n
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hrmin           ! minimum ridge thickness
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hrmax           ! maximum ridge thickness
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hrexp           ! e-folding ridge thickness
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hraft           ! thickness of rafted ice
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hi_hrdg         ! thickness of ridging ice / mean ridge thickness
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   aridge          ! participating ice ridging
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   araft           ! participating ice rafting
   !
   ! For ridging diagnostics
   LOGICAL                                       ::   ll_diag_rdg     ! activate ridging diagnostics or not
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)     ::   airdg1          ! ridging ice area loss
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)     ::   airft1          ! rafting ice area loss
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)     ::   airdg2          ! new ridged ice area gain
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)     ::   airft2          ! new rafted ice area gain
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   opning_2d       ! lead opening rate diagnostic
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dairdg1dt       ! ridging ice area loss rate diagnostic
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dairft1dt       ! rafting ice area loss rate diagnostic
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dairdg2dt       ! new ridged ice area gain rate diagnostic
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   dairft2dt       ! new rafted ice area gain rate diagnostic
   !
   REAL(wp), PARAMETER ::   hrdg_hi_min = 1.1_wp    ! min ridge thickness multiplier: min(hrdg/hi)
   REAL(wp), PARAMETER ::   hi_hrft     = 0.5_wp    ! rafting multiplier: (hi/hraft)
   !
   ! ** namelist (namdyn_rdgrft) **
   LOGICAL  ::   ln_str_smooth    ! ice strength spatial smoothing
   LOGICAL  ::   ln_distf_lin     ! redistribution of ridged ice: linear (Hibler 1980)
   LOGICAL  ::   ln_distf_exp     ! redistribution of ridged ice: exponential (Lipscomb et al 2017)
   REAL(wp) ::   rn_murdg         !    gives e-folding scale of ridged ice (m^.5)
   REAL(wp) ::   rn_csrdg         ! fraction of shearing energy contributing to ridging
   LOGICAL  ::   ln_partf_lin     ! participation function linear (Thorndike et al. (1975))
   REAL(wp) ::   rn_gstar         !    fractional area of young ice contributing to ridging
   LOGICAL  ::   ln_partf_exp     ! participation function exponential (Lipscomb et al. (2007))
   REAL(wp) ::   rn_astar         !    equivalent of G* for an exponential participation function
   LOGICAL  ::   ln_ridging       ! ridging of ice or not
   REAL(wp) ::   rn_hstar         !    thickness that determines the maximal thickness of ridged ice
   REAL(wp) ::   rn_porordg       !    initial porosity of ridges (0.3 regular value)
   REAL(wp) ::   rn_fsnwrdg       !    fractional snow loss to the ocean during ridging
   REAL(wp) ::   rn_fpndrdg       !    fractional pond loss to the ocean during ridging
   LOGICAL  ::   ln_rafting       ! rafting of ice or not
   REAL(wp) ::   rn_hraft         !    threshold thickness (m) for rafting / ridging
   REAL(wp) ::   rn_craft         !    coefficient for smoothness of the hyperbolic tangent in rafting
   REAL(wp) ::   rn_fsnwrft       !    fractional snow loss to the ocean during rafting
   REAL(wp) ::   rn_fpndrft       !    fractional pond loss to the ocean during rafting
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL licence     (./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION ice_dyn_rdgrft_alloc()
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE ice_dyn_rdgrft_alloc ***
      !!-------------------------------------------------------------------
      IF( ll_diag_rdg ) THEN
         ALLOCATE( closing_net(jpij) , opning(jpij)    , closing_gross(jpij),                   &
            &      apartf(jpij,0:jpl), hrmin (jpij,jpl), hraft(jpij,jpl)    , aridge(jpij,jpl), &
            &      hrmax (jpij,jpl)  , hrexp (jpij,jpl), hi_hrdg(jpij,jpl)  , araft(jpij,jpl) , &
            &      airdg1(jpij)      , airft1(jpij)    , airdg2(jpij)       , airft2(jpij)    , &
            ! diagnostics
            &      opning_2d(A2D(0)) , dairdg1dt(A2D(0)) , dairft1dt(A2D(0)) , dairdg2dt(A2D(0)) , dairft2dt(A2D(0)) , &
            !
            &      STAT=ice_dyn_rdgrft_alloc )
      ELSE
         ALLOCATE( closing_net(jpij) , opning(jpij)    , closing_gross(jpij),                   &
            &      apartf(jpij,0:jpl), hrmin (jpij,jpl), hraft(jpij,jpl)    , aridge(jpij,jpl), &
            &      hrmax (jpij,jpl)  , hrexp (jpij,jpl), hi_hrdg(jpij,jpl)  , araft(jpij,jpl) , &
            &      airdg1(jpij)      , airft1(jpij)    , airdg2(jpij)       , airft2(jpij)    , &
            &      STAT=ice_dyn_rdgrft_alloc )         
      ENDIF
      
      CALL mpp_sum ( 'icedyn_rdgrft', ice_dyn_rdgrft_alloc )
      IF( ice_dyn_rdgrft_alloc /= 0 )   CALL ctl_stop( 'STOP',  'ice_dyn_rdgrft_alloc: failed to allocate arrays'  )
      !
   END FUNCTION ice_dyn_rdgrft_alloc


   SUBROUTINE ice_dyn_rdgrft( kt )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE ice_dyn_rdgrft ***
      !!
      !! ** Purpose :   computes the mechanical redistribution of ice thickness
      !!
      !! ** Method  :   Steps :
      !!       0) Identify grid cells with ice
      !!       1) Calculate closing rate, divergence and opening
      !!       2) Identify grid cells with ridging
      !!       3) Start ridging iterations
      !!          - prep = ridged and rafted ice + closing_gross
      !!          - shift = move ice from one category to another
      !!
      !! ** Details
      !!    step1: The net rate of closing is due to convergence and shear, based on Flato and Hibler (1995).
      !!           The energy dissipation rate is equal to the net closing rate times the ice strength.
      !!
      !!    step3: The gross closing rate is equal to the first two terms (open
      !!           water closing and thin ice ridging) without the third term
      !!           (thick, newly ridged ice).
      !!
      !! References :   Flato, G. M., and W. D. Hibler III, 1995, JGR, 100, 18,611-18,626.
      !!                Hibler, W. D. III, 1980, MWR, 108, 1943-1973, 1980.
      !!                Rothrock, D. A., 1975: JGR, 80, 4514-4519.
      !!                Thorndike et al., 1975, JGR, 80, 4501-4513.
      !!                Bitz et al., JGR, 2001
      !!                Amundrud and Melling, JGR 2005
      !!                Babko et al., JGR 2002
      !!
      !!     This routine is based on CICE code and authors William H. Lipscomb,
      !!     and Elizabeth C. Hunke, LANL are gratefully acknowledged
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! number of iteration
      !!
      INTEGER  ::   ii, ji, jj, jk, jl         ! dummy loop index
      INTEGER  ::   iter, iterate_ridging      ! local integer
      INTEGER  ::   ipti                       ! local integer
      REAL(wp) ::   zfac                       ! local scalar
      INTEGER , DIMENSION(jpij) ::   iptidx        ! compute ridge/raft or not
      REAL(wp), DIMENSION(jpij) ::   zdivu, zdelt  ! 1D divu_i & delta_i
      REAL(wp), DIMENSION(jpij) ::   zconv         ! 1D rdg_conv (if EAP rheology)
      !!
      REAL(wp), DIMENSION(A2D(0)) ::   zmsk       ! Temporary array for ice presence mask
      !
      INTEGER, PARAMETER ::   jp_itermax = 20
      !!-------------------------------------------------------------------
      ! controls
      IF( ln_timing    )   CALL timing_start('icedyn_rdgrft')                                                             ! timing
      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'icedyn_rdgrft', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      IF( ln_icediachk )   CALL ice_cons2D  (0, 'icedyn_rdgrft',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft) ! conservation

      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*)'ice_dyn_rdgrft: ice ridging and rafting'
         IF(lwp) WRITE(numout,*)'~~~~~~~~~~~~~~'
      ENDIF     

      ! Initialise ridging diagnostics if required
      IF( ll_diag_rdg ) THEN    
         opning_2d(:,:) = 0.0_wp
         dairdg1dt(:,:) = 0.0_wp ; dairft1dt(:,:) = 0.0_wp
         dairdg2dt(:,:) = 0.0_wp ; dairft2dt(:,:) = 0.0_wp
      ENDIF

      !--------------------------------
      ! 0) Identify grid cells with ice
      !--------------------------------
      zmsk(:,:) = MERGE( 1._wp, 0._wp, at_i(A2D(0)) >= epsi10 ) ! 1 if ice, 0 if no ice
      !
      at_i(A2D(0)) = SUM( a_i(A2D(0),:), dim=3 )
#if defined key_si3_1D
      DO_2D( 0, 0, 0, 0 )
         npti = 0   ;   nptidx(:) = 0
         ipti = 0   ;   iptidx(:) = 0
         IF ( at_i(ji,jj) > epsi10 ) THEN
            npti           = 1
            nptidx( npti ) = (jj - 1) * jpi + ji
         ENDIF
#else
      npti = 0   ;   nptidx(:) = 0
      ipti = 0   ;   iptidx(:) = 0
      DO_2D( 0, 0, 0, 0 )
         IF ( at_i(ji,jj) > epsi10 ) THEN
            npti           = npti + 1
            nptidx( npti ) = (jj - 1) * jpi + ji
         ENDIF
      END_2D
#endif
      !--------------------------------------------------------
      ! 1) Dynamical inputs (closing rate, divergence, opening)
      !--------------------------------------------------------
      IF( npti > 0 ) THEN

         ! just needed here
         CALL tab_2d_1d( npti, nptidx(1:npti), zdelt   (1:npti)  , delta_i )
         CALL tab_2d_1d( npti, nptidx(1:npti), zconv   (1:npti)  , rdg_conv )
         ! needed here and in the iteration loop
         CALL tab_2d_1d( npti, nptidx(1:npti), zdivu   (1:npti)  , divu_i) ! zdivu is used as a work array here (no change in divu_i)
         CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d  (1:npti,:), a_i   )
         CALL tab_3d_2d( npti, nptidx(1:npti), v_i_2d  (1:npti,:), v_i   )
         CALL tab_2d_1d( npti, nptidx(1:npti), ato_i_1d(1:npti)  , ato_i )

         DO ii = 1, npti
            ! closing_net = rate at which open water area is removed + ice area removed by ridging
            !                                                        - ice area added in new ridges
            IF( ln_rhg_EVP .OR. ln_rhg_VP ) &
               &               closing_net(ii) = rn_csrdg * 0.5_wp * ( zdelt(ii) - ABS( zdivu(ii) ) ) - MIN( zdivu(ii), 0._wp )
            IF( ln_rhg_EAP )   closing_net(ii) = zconv(ii)
            !
            IF( zdivu(ii) < 0._wp )   closing_net(ii) = MAX( closing_net(ii), -zdivu(ii) )   ! make sure the closing rate is large enough
            !                                                                                ! to give asum = 1.0 after ridging
            ! Opening rate (non-negative) that will give asum = 1.0 after ridging.
            opning(ii) = closing_net(ii) + zdivu(ii)
         END DO
         !
         !------------------------------------
         ! 2) Identify grid cells with ridging
         !------------------------------------
         CALL rdgrft_prep( a_i_2d, v_i_2d, ato_i_1d, &                                                          ! <<== in
            &              apartf, aridge, araft, hi_hrdg, hraft, hrmin, hrmax, hrexp, closing_gross, opning, & ! ==>> out
            &              closing_net )                                                                        ! <<== in

         DO ii = 1, npti
            IF( SUM( apartf(ii,1:jpl) ) > 0._wp .AND. closing_gross(ii) > 0._wp ) THEN
               ipti = ipti + 1
               iptidx     (ipti)   = nptidx     (ii)
               ! adjust to new indices
               a_i_2d     (ipti,:) = a_i_2d     (ii,:)
               v_i_2d     (ipti,:) = v_i_2d     (ii,:)
               ato_i_1d   (ipti)   = ato_i_1d   (ii)
               closing_net(ipti)   = closing_net(ii)
               zdivu      (ipti)   = zdivu      (ii)
               opning     (ipti)   = opning     (ii)
            ENDIF
         END DO

      ENDIF

      ! grid cells with ridging
      nptidx(:) = iptidx(:)
      npti      = ipti

      !-----------------
      ! 3) Start ridging
      !-----------------
      IF( npti > 0 ) THEN

         CALL ice_dyn_1d2d( 1 )            ! --- Move to 1D arrays --- !

         iter            = 1
         iterate_ridging = 1
         !                                                        !----------------------!
         DO WHILE( iterate_ridging > 0 .AND. iter < jp_itermax )  !  ridging iterations  !
            !                                                     !----------------------!
            ! Calculate participation function (apartf)
            !       and transfer      function
            !       and closing_gross (+correction on opening)
            CALL rdgrft_prep( a_i_2d, v_i_2d, ato_i_1d, &                                                          ! <<== in
               &              apartf, aridge, araft, hi_hrdg, hraft, hrmin, hrmax, hrexp, closing_gross, opning, & ! ==>> out
               &              closing_net )                                                                        ! <<== in

            ! Redistribute area, volume, and energy between categories
            CALL rdgrft_shift( apartf, aridge, araft, hi_hrdg, hraft, hrmin, hrmax, hrexp, closing_gross, opning, & ! <<== in
               &               sst_1d, sss_1d )                                                                     ! <<== in

            ! Do we keep on iterating?
            !-------------------------
            ! Check whether a_i + ato_i = 0
            ! If not, because the closing and opening rates were reduced above, ridge again with new rates
            iterate_ridging = 0
            DO ii = 1, npti
               zfac = 1._wp - ( ato_i_1d(ii) + SUM( a_i_2d(ii,:) ) )
               IF( ABS( zfac ) < epsi10 ) THEN
                  closing_net(ii) = 0._wp
                  opning     (ii) = 0._wp
                  ato_i_1d   (ii) = MAX( 0._wp, 1._wp - SUM( a_i_2d(ii,:) ) )
               ELSE
                  iterate_ridging  = 1
                  zdivu      (ii) = zfac * r1_Dt_ice
                  closing_net(ii) = MAX( 0._wp, -zdivu(ii) )
                  opning     (ii) = MAX( 0._wp,  zdivu(ii) )
               ENDIF
            END DO
            !
            iter = iter + 1
            IF( iter  >  jp_itermax )    CALL ctl_stop( 'STOP',  'icedyn_rdgrft: non-converging ridging scheme'  )
            !
         END DO
         !
         CALL ice_dyn_1d2d( 2 )            ! --- Move to 2D arrays --- !
         ! 
      ENDIF  ! npti>0

#if defined key_si3_1D
      END_2D
#endif

      IF( ll_diag_rdg ) THEN
        CALL iom_put( 'lead_open', opning_2d(:,:) * zmsk(:,:) )  ! Lead area opening rate
        CALL iom_put( 'rdg_loss',  dairdg1dt(:,:) * zmsk(:,:) )  ! Ridging ice area loss rate
        CALL iom_put( 'rft_loss',  dairft1dt(:,:) * zmsk(:,:) )  ! Rafting ice area loss rate
        CALL iom_put( 'rdg_gain',  dairdg2dt(:,:) * zmsk(:,:) )  ! New ridged ice area gain rate
        CALL iom_put( 'rft_gain',  dairft2dt(:,:) * zmsk(:,:) )  ! New rafted ice area gain rate
      ENDIF

      ! clem: those fields must be updated on the halos: ato_i, a_i, v_i, v_s, sv_i, oa_i, a_ip, v_ip, v_il, e_i, e_s, szv_i

      ! clem: I think we can comment this line but I am not sure it does not change results
!!$      CALL ice_var_agg( 1 )

      ! controls
      IF( sn_cfctl%l_prtctl )   CALL ice_prt3D('icedyn_rdgrft')                                                           ! prints
      IF( ln_icectl    )   CALL ice_prt     (kt, iiceprt, jiceprt,-1, ' - ice dyn rdgrft - ')                             ! prints
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'icedyn_rdgrft', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft) ! conservation
      IF( ln_icediachk )   CALL ice_cons2D  (1, 'icedyn_rdgrft',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft) ! conservation
      IF( ln_timing    )   CALL timing_stop ('icedyn_rdgrft')                                                             ! timing
      !
   END SUBROUTINE ice_dyn_rdgrft


   SUBROUTINE rdgrft_prep( pa_i, pv_i, pato_i, &                                                                          ! <<== in
      &                    papartf, paridge, paraft, phi_hrdg, phraft, phrmin, phrmax, phrexp, pclosing_gross, popning, & ! ==>> out
      &                    pclosing_net )                                                                                 ! <<== in
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE rdgrft_prep ***
      !!
      !! ** Purpose :   preparation for ridging calculations
      !!
      !! ** Method  :   Compute the thickness distribution of the ice and open water
      !!                participating in ridging and of the resulting ridges.
      !!-------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:)       , INTENT(in   ) ::   pa_i, pv_i
      REAL(wp), DIMENSION(:)         , INTENT(in   ) ::   pato_i
      REAL(wp), DIMENSION(jpij,0:jpl), INTENT(  out) ::   papartf
      REAL(wp), DIMENSION(jpij,  jpl), INTENT(  out) ::   paridge, paraft, phi_hrdg, phraft, phrmin, phrmax, phrexp
      REAL(wp), DIMENSION(jpij)      , INTENT(inout), OPTIONAL ::   pclosing_gross, popning
      REAL(wp), DIMENSION(:)         , INTENT(in   ), OPTIONAL ::   pclosing_net
      !!
      INTEGER  ::   ii, jl                  ! dummy loop indices
      REAL(wp) ::   z1_gstar, z1_astar, zhmean, zfac   ! local scalar
      REAL(wp), DIMENSION(jpij)        ::   zasum, z1_asum           ! sum of a_i+ato_i and inverse
      REAL(wp), DIMENSION(jpij)        ::   zaksum                   ! normalisation factor
      REAL(wp), DIMENSION(jpij,jpl)    ::   zhi                      ! ice thickness
      REAL(wp), DIMENSION(jpij,-1:jpl) ::   zGsum                    ! zGsum(n) = sum of areas in categories 0 to n
      !--------------------------------------------------------------------

      z1_gstar = 1._wp / rn_gstar
      z1_astar = 1._wp / rn_astar

      !                       ! Ice thickness needed for rafting
      ! In single precision there were floating point invalids due a sqrt of zhi which happens to have negative values
      ! To solve that an extra check about the value of pv_i was added.
      ! Although adding this condition is safe, the double definition (one for single other for double) has been kept to preserve the results of the sette test.
#if defined key_single

      WHERE( pa_i(1:npti,:) > epsi10 .and. pv_i(1:npti,:) > epsi10 )   ;   zhi(1:npti,:) = pv_i(1:npti,:) / pa_i(1:npti,:)
#else
      WHERE( pa_i(1:npti,:) > epsi10 )   ;   zhi(1:npti,:) = pv_i(1:npti,:) / pa_i(1:npti,:)
#endif
      ELSEWHERE                          ;   zhi(1:npti,:) = 0._wp
      END WHERE

      ! 1) Participation function (apartf): a(h) = b(h).g(h)
      !-----------------------------------------------------------------
      ! Compute the participation function = total area lost due to ridging/closing
      ! This is analogous to
      !   a(h) = b(h)g(h) as defined in Thorndike et al. (1975).
      !   assuming b(h) = (2/Gstar) * (1 - G(h)/Gstar).
      !
      ! apartf = integrating b(h)g(h) between the category boundaries
      ! apartf is always >= 0 and SUM(apartf(0:jpl))=1
      !-----------------------------------------------------------------
      !
      ! Compute total area of ice plus open water.
      ! This is in general not equal to one because of divergence during transport
      zasum(1:npti) = pato_i(1:npti) + SUM( pa_i(1:npti,:), dim=2 )
      !
      WHERE( zasum(1:npti) > epsi10 )   ;   z1_asum(1:npti) = 1._wp / zasum(1:npti)
      ELSEWHERE                         ;   z1_asum(1:npti) = 0._wp
      END WHERE
      !
      ! Compute cumulative thickness distribution function
      ! Compute the cumulative thickness distribution function zGsum,
      ! where zGsum(n) is the fractional area in categories 0 to n.
      ! initial value (in h = 0) = open water area
      zGsum(1:npti,-1) = 0._wp
      zGsum(1:npti,0 ) = pato_i(1:npti) * z1_asum(1:npti)
      DO jl = 1, jpl
         zGsum(1:npti,jl) = ( pato_i(1:npti) + SUM( pa_i(1:npti,1:jl), dim=2 ) ) * z1_asum(1:npti)  ! sum(1:jl) is correct (and not jpl)
      END DO
      !
      IF( ln_partf_lin ) THEN          !--- Linear formulation (Thorndike et al., 1975)
         DO jl = 0, jpl
            DO ii = 1, npti
               IF    ( zGsum(ii,jl)   < rn_gstar ) THEN
                  papartf(ii,jl) = z1_gstar * ( zGsum(ii,jl) - zGsum(ii,jl-1) ) * &
                     &                       ( 2._wp - ( zGsum(ii,jl-1) + zGsum(ii,jl) ) * z1_gstar )
               ELSEIF( zGsum(ii,jl-1) < rn_gstar ) THEN
                  papartf(ii,jl) = z1_gstar * ( rn_gstar     - zGsum(ii,jl-1) ) *  &
                     &                       ( 2._wp - ( zGsum(ii,jl-1) + rn_gstar     ) * z1_gstar )
               ELSE
                  papartf(ii,jl) = 0._wp
               ENDIF
            END DO
         END DO
         !
      ELSEIF( ln_partf_exp ) THEN      !--- Exponential, more stable formulation (Lipscomb et al, 2007)
         !
         zfac = 1._wp / ( 1._wp - EXP(-z1_astar) )
         DO jl = -1, jpl
            DO ii = 1, npti
               zGsum(ii,jl) = EXP( -zGsum(ii,jl) * z1_astar ) * zfac
            END DO
         END DO
         DO jl = 0, jpl
            DO ii = 1, npti
               papartf(ii,jl) = zGsum(ii,jl-1) - zGsum(ii,jl)
            END DO
         END DO
         !
      ENDIF

      !                                !--- Ridging and rafting participation concentrations
      IF( ln_rafting .AND. ln_ridging ) THEN             !- ridging & rafting
         DO jl = 1, jpl
            DO ii = 1, npti
               paridge(ii,jl) = ( 1._wp + TANH ( rn_craft * ( zhi(ii,jl) - rn_hraft ) ) ) * 0.5_wp * papartf(ii,jl)
               paraft (ii,jl) = papartf(ii,jl) - paridge(ii,jl)
            END DO
         END DO
      ELSEIF( ln_ridging .AND. .NOT. ln_rafting ) THEN   !- ridging alone
         DO jl = 1, jpl
            DO ii = 1, npti
               paridge(ii,jl) = papartf(ii,jl)
               paraft (ii,jl) = 0._wp
            END DO
         END DO
      ELSEIF( ln_rafting .AND. .NOT. ln_ridging ) THEN   !- rafting alone
         DO jl = 1, jpl
            DO ii = 1, npti
               paridge(ii,jl) = 0._wp
               paraft (ii,jl) = papartf(ii,jl)
            END DO
         END DO
      ELSE                                               !- no ridging & no rafting
         DO jl = 1, jpl
            DO ii = 1, npti
               paridge(ii,jl) = 0._wp
               paraft (ii,jl) = 0._wp
            END DO
         END DO
      ENDIF

      ! 2) Transfer function
      !-----------------------------------------------------------------
      ! If assuming ridged ice is uniformly distributed between hrmin and  
      ! hrmax (ln_distf_lin):
      !
      ! Compute max and min ridged ice thickness for each ridging category.
      !
      ! This parameterization is a modified version of Hibler (1980).
      ! The mean ridging thickness, zhmean, is proportional to hi^(0.5)
      !  and for very thick ridging ice must be >= hrdg_hi_min*hi
      !
      ! The minimum ridging thickness, hrmin, is equal to 2*hi
      !  (i.e., rafting) and for very thick ridging ice is
      !  constrained by hrmin <= (zhmean + hi)/2.
      !
      ! The maximum ridging thickness, hrmax, is determined by zhmean and hrmin.
      !
      ! These modifications have the effect of reducing the ice strength
      ! (relative to the Hibler formulation) when very thick ice is ridging.
      !
      !-----------------------------------------------------------------
      ! If assuming ridged ice ITD is a negative exponential 
      ! (ln_distf_exp) and following CICE implementation: 
      ! 
      !  g(h) ~ exp[-(h-hrmin)/hrexp], h >= hrmin 
      ! 
      ! where hrmin is the minimum thickness of ridging ice and 
      ! hrexp is the e-folding thickness.
      ! 
      ! Here, assume as above that hrmin = min(2*hi, hi+maxraft).
      ! That is, the minimum ridge thickness results from rafting,
      !  unless the ice is thicker than maxraft.
      !
      ! Also, assume that hrexp = mu_rdg*sqrt(hi).
      ! The parameter mu_rdg is tuned to give e-folding scales mostly
      !  in the range 2-4 m as observed by upward-looking sonar.
      !
      ! Values of mu_rdg in the right column give ice strengths
      !  roughly equal to values of Hstar in the left column
      !  (within ~10 kN/m for typical ITDs):
      !
      !   Hstar     mu_rdg
      !
      !     25        3.0
      !     50        4.0
      !     75        5.0
      !    100        6.0
      !
      ! zaksum = net area removed/ total area participating
      ! where total area participating = area of ice that ridges
      !         net area removed = total area participating - area of new ridges
      !-----------------------------------------------------------------
      zfac = 1._wp / hi_hrft
      zaksum(1:npti) = papartf(1:npti,0)
      !
      DO jl = 1, jpl
         DO ii = 1, npti
            IF ( papartf(ii,jl) > 0._wp ) THEN
               zhmean        = MAX( SQRT( rn_hstar * zhi(ii,jl) ), zhi(ii,jl) * hrdg_hi_min )
               phrmin(ii,jl) = MIN( 2._wp * zhi(ii,jl), 0.5_wp * ( zhmean + zhi(ii,jl) ) )
               phraft(ii,jl) = zhi(ii,jl) * zfac
               !
               IF( ln_distf_lin ) THEN
                  phrmax  (ii,jl) = 2._wp * zhmean - phrmin(ii,jl)
                  phi_hrdg(ii,jl) = zhi(ii,jl) / MAX( zhmean, epsi20 )
               ELSEIF( ln_distf_exp ) THEN
                  phrexp  (ii,jl) = rn_murdg * SQRT( zhi(ii,jl) )
                  phi_hrdg(ii,jl) = zhi(ii,jl) / MAX( epsi20, phrmin(ii,jl) + phrexp(ii,jl) )
               ENDIF
               !
               ! Normalization factor : zaksum, ensures mass conservation
               zaksum(ii) = zaksum(ii) + paridge(ii,jl) * ( 1._wp - phi_hrdg(ii,jl) )    &
                  &                    + paraft (ii,jl) * ( 1._wp - hi_hrft )
            ELSE
               phrmin  (ii,jl) = 0._wp
               phrmax  (ii,jl) = 0._wp
               phrexp  (ii,jl) = 0._wp
               phraft  (ii,jl) = 0._wp
               phi_hrdg(ii,jl) = 1._wp
            ENDIF
         END DO
      END DO
      !
      IF( PRESENT( pclosing_net ) ) THEN
         !
         ! 3) closing_gross
         !-----------------
         ! Based on the ITD of ridging and ridged ice, convert the net closing rate to a gross closing rate.
         ! NOTE: 0 < aksum <= 1
         WHERE( zaksum(1:npti) > epsi10 )   ;   pclosing_gross(1:npti) = pclosing_net(1:npti) / zaksum(1:npti)
         ELSEWHERE                          ;   pclosing_gross(1:npti) = 0._wp
         END WHERE
         
         ! correction to closing rate if excessive ice removal
         !----------------------------------------------------
         ! Reduce the closing rate if more than 100% of any ice category would be removed
         ! Reduce the opening rate in proportion
         DO jl = 1, jpl
            DO ii = 1, npti
               zfac = papartf(ii,jl) * pclosing_gross(ii) * rDt_ice
               IF( zfac > pa_i(ii,jl) .AND. papartf(ii,jl) /= 0._wp ) THEN
                  pclosing_gross(ii) = pa_i(ii,jl) / papartf(ii,jl) * r1_Dt_ice
               ENDIF
            END DO
         END DO
         
         ! 4) correction to opening if excessive open water removal
         !---------------------------------------------------------
         ! Reduce the closing rate if more than 100% of the open water would be removed
         ! Reduce the opening rate in proportion
         DO ii = 1, npti
            zfac = pato_i(ii) + ( popning(ii) - papartf(ii,0) * pclosing_gross(ii) ) * rDt_ice
            IF( zfac < 0._wp ) THEN           ! would lead to negative ato_i
               popning(ii) = papartf(ii,0) * pclosing_gross(ii) - pato_i(ii) * r1_Dt_ice
            ELSEIF( zfac > zasum(ii) ) THEN   ! would lead to ato_i > asum
               popning(ii) = papartf(ii,0) * pclosing_gross(ii) + ( zasum(ii) - pato_i(ii) ) * r1_Dt_ice
            ENDIF
         END DO
         !
      ENDIF
      !
   END SUBROUTINE rdgrft_prep


   SUBROUTINE rdgrft_shift( papartf, paridge, paraft, phi_hrdg, phraft, phrmin, phrmax, phrexp, pclosing_gross, popning, & ! <<== in
      &                     psst, psss )                                                                                   ! <<== in
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE rdgrft_shift ***
      !!
      !! ** Purpose :   shift ridging ice among thickness categories of ice thickness
      !!
      !! ** Method  :   Remove area, volume, and energy from each ridging category
      !!                and add to thicker ice categories.
      !!-------------------------------------------------------------------
      REAL(wp), DIMENSION(jpij,0:jpl), INTENT(in) ::   papartf
      REAL(wp), DIMENSION(:,:)       , INTENT(in) ::   paridge, paraft, phi_hrdg, phraft, phrmin, phrmax, phrexp
      REAL(wp), DIMENSION(:)         , INTENT(in) ::   pclosing_gross, popning
      REAL(wp), DIMENSION(:)         , INTENT(in) ::   psst, psss
      !
      INTEGER  ::   ii, jl, jl1, jl2, jk       ! dummy loop indices
      REAL(wp) ::   hL, hR, farea              ! left and right limits of integration and new area going to jl2
      REAL(wp) ::   expL, expR                 ! exponentials involving hL, hR
      REAL(wp) ::   vsw                        ! vol of water trapped into ridges
      REAL(wp) ::   afrdg, afrft               ! fraction of category area ridged/rafted
      REAL(wp), DIMENSION(jpij) ::   oirdg, aprdg, virdg, vsrdg, vprdg, vlrdg  ! area etc of new ridges
      REAL(wp), DIMENSION(jpij) ::   oirft, aprft, virft, vsrft, vprft, vlrft  ! area etc of rafted ice
      REAL(wp), DIMENSION(jpij,nlay_i) ::   sirdg, sirft
      !
      REAL(wp) ::   ersw             ! enthalpy of water trapped into ridges
      REAL(wp) ::   zswitch, fvol    ! new ridge volume going to jl2
      REAL(wp) ::   z1_ai            ! 1 / a
      REAL(wp), DIMENSION(jpij) ::   zvti             ! sum(v_i)
      !
      REAL(wp), DIMENSION(jpij,nlay_s) ::   esrft     ! snow energy of rafting ice
      REAL(wp), DIMENSION(jpij,nlay_i) ::   eirft     ! ice  energy of rafting ice
      REAL(wp), DIMENSION(jpij,nlay_s) ::   esrdg     ! enth*volume of new ridges
      REAL(wp), DIMENSION(jpij,nlay_i) ::   eirdg     ! enth*volume of new ridges
      !
      INTEGER , DIMENSION(jpij) ::   itest_rdg, itest_rft   ! test for conservation
      LOGICAL , DIMENSION(jpij) ::   ll_shift         ! logical for doing calculation or not
      !!-------------------------------------------------------------------
      !
      zvti(1:npti) = SUM( v_i_2d(1:npti,:), dim=2 )   ! total ice volume
      !
      ! 1) Change in open water area due to closing and opening
      !--------------------------------------------------------
      DO ii = 1, npti
         ato_i_1d(ii) = MAX( 0._wp, ato_i_1d(ii) + ( popning(ii) - papartf(ii,0) * pclosing_gross(ii) ) * rDt_ice )
      END DO

      ! 2) compute categories in which ice is removed (jl1)
      !----------------------------------------------------
      IF( nn_icesal == 1 .OR. nn_icesal == 3 )  THEN
         CALL tab_3d_2d( npti, nptidx(1:npti), s_i_2d(1:npti,:), s_i(:,:,:) )
      ENDIF

      DO jl1 = 1, jpl

         DO ii = 1, npti

            ! set logical to true when ridging
            IF( papartf(ii,jl1) > 0._wp .AND. pclosing_gross(ii) > 0._wp ) THEN   ;   ll_shift(ii) = .TRUE.
            ELSE                                                                  ;   ll_shift(ii) = .FALSE.
            ENDIF
            
            IF( ll_shift(ii) ) THEN   ! only if ice is ridging

               IF( a_i_2d(ii,jl1) > epsi10 ) THEN   ;   z1_ai = 1._wp / a_i_2d(ii,jl1)
               ELSE                                 ;   z1_ai = 0._wp
               ENDIF

               ! area of ridging / rafting ice (airdg1) and of new ridge (airdg2)
               airdg1(ii) = paridge(ii,jl1) * pclosing_gross(ii) * rDt_ice
               airft1(ii) = paraft (ii,jl1) * pclosing_gross(ii) * rDt_ice

               airdg2(ii) = airdg1(ii) * phi_hrdg(ii,jl1)
               airft2(ii) = airft1(ii) *  hi_hrft

               ! ridging /rafting fractions
               afrdg = airdg1(ii) * z1_ai
               afrft = airft1(ii) * z1_ai

               ! volume and enthalpy (J/m2, >0) of seawater trapped into ridges
               IF    ( zvti(ii) <= 10. ) THEN ; vsw = v_i_2d(ii,jl1) * afrdg * rn_porordg                                           ! v <= 10m then porosity = rn_porordg
               ELSEIF( zvti(ii) >= 20. ) THEN ; vsw = 0._wp                                                                         ! v >= 20m then porosity = 0
               ELSE                           ; vsw = v_i_2d(ii,jl1) * afrdg * rn_porordg * MAX( 0._wp, 2._wp - 0.1_wp * zvti(ii) ) ! v > 10m and v < 20m then porosity = linear transition to 0
               ENDIF
               ersw = -rhoi * vsw * rcp * psst(ii)   ! clem: if sst>0, then ersw <0 (is that possible?)

               ! volume etc of ridging / rafting ice and new ridges (vi, vs, sm, oi, es, ei)
               virdg(ii) = v_i_2d (ii,jl1)    * afrdg + vsw
               vsrdg(ii) = v_s_2d (ii,jl1)    * afrdg
               oirdg(ii) = oa_i_2d(ii,jl1)    * afrdg * phi_hrdg(ii,jl1)

               virft(ii) = v_i_2d (ii,jl1)    * afrft
               vsrft(ii) = v_s_2d (ii,jl1)    * afrft
               oirft(ii) = oa_i_2d(ii,jl1)    * afrft * hi_hrft

               IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
                  aprdg(ii) = a_ip_2d(ii,jl1)    * afrdg * phi_hrdg(ii,jl1)
                  vprdg(ii) = v_ip_2d(ii,jl1)    * afrdg
                  aprft(ii) = a_ip_2d(ii,jl1)    * afrft *  hi_hrft
                  vprft(ii) = v_ip_2d(ii,jl1)    * afrft
                  IF ( ln_pnd_lids ) THEN
                     vlrdg(ii) = v_il_2d(ii,jl1) * afrdg
                     vlrft(ii) = v_il_2d(ii,jl1) * afrft
                  ENDIF
               ENDIF
               
               esrdg(ii,:) = e_s_2d (ii,:,jl1) * afrdg
               esrft(ii,:) = e_s_2d (ii,:,jl1) * afrft
               eirdg(ii,:) = e_i_2d (ii,:,jl1) * afrdg + ersw * r1_nlay_i
               eirft(ii,:) = e_i_2d (ii,:,jl1) * afrft

               IF( nn_icesal == 4 ) THEN 
                  sirdg(ii,:) = szv_i_2d(ii,:,jl1) * afrdg + vsw * psss(ii) * r1_nlay_i
                  sirft(ii,:) = szv_i_2d(ii,:,jl1) * afrft
               ELSE
                  sirdg(ii,1) = sv_i_2d (ii,  jl1) * afrdg + vsw * psss(ii)
                  sirft(ii,1) = sv_i_2d (ii,  jl1) * afrft
               ENDIF
               
               ! Ice-ocean exchanges associated with ice porosity
               wfx_dyn_1d(ii) = wfx_dyn_1d(ii) - vsw * rhoi * r1_Dt_ice   ! increase in ice volume due to seawater frozen in voids
               sfx_dyn_1d(ii) = sfx_dyn_1d(ii) - vsw * psss(ii) * rhoi * r1_Dt_ice
               hfx_dyn_1d(ii) = hfx_dyn_1d(ii) + ersw * r1_Dt_ice          ! > 0 [W.m-2]

               ! Put the snow and pond lost by ridging into the ocean
               !  Note that esrdg > 0; the ocean must cool to melt snow. If the ocean temp = Tf already, new ice must grow.
               wfx_snw_dyn_1d(ii) = wfx_snw_dyn_1d(ii) + ( rhos * vsrdg(ii) * ( 1._wp - rn_fsnwrdg )   &   ! fresh water source for ocean
                  &                                      + rhos * vsrft(ii) * ( 1._wp - rn_fsnwrft ) ) * r1_Dt_ice
               DO jk = 1, nlay_s
                  hfx_dyn_1d(ii) = hfx_dyn_1d(ii) + ( - esrdg(ii,jk) * ( 1._wp - rn_fsnwrdg )   &          ! heat sink for ocean (<0, W.m-2)
                     &                                - esrft(ii,jk) * ( 1._wp - rn_fsnwrft ) ) * r1_Dt_ice
               END DO

               IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
                  wfx_pnd_1d(ii)    = wfx_pnd_1d(ii)   + ( rhow * vprdg(ii) * ( 1._wp - rn_fpndrdg )   &   ! fresh water source for ocean
                     &                                   + rhow * vprft(ii) * ( 1._wp - rn_fpndrft ) ) * r1_Dt_ice
                  IF ( ln_pnd_lids ) THEN
                     wfx_pnd_1d(ii) = wfx_pnd_1d(ii)   + ( rhow * vlrdg(ii) * ( 1._wp - rn_fpndrdg )   &   ! fresh water source for ocean
                        &                                + rhow * vlrft(ii) * ( 1._wp - rn_fpndrft ) ) * r1_Dt_ice
                  ENDIF
               ENDIF

               ! virtual salt flux to keep salinity constant
               IF( nn_icesal == 1 .OR. nn_icesal == 3 )  THEN
                  sirdg(ii,1)    = sirdg(ii,1)    - ( psss(ii) - s_i_2d(ii,jl1) ) * vsw                      ! ridge salinity = s_i
                  sfx_bri_1d(ii) = sfx_bri_1d(ii) + ( psss(ii) - s_i_2d(ii,jl1) ) * vsw * rhoi * r1_Dt_ice   ! put back sss_m into the ocean
                  !                                                                                          ! and get  s_i  from the ocean
               ENDIF

               ! Remove area, volume of new ridge to each category jl1
               !------------------------------------------------------
               a_i_2d (ii,jl1) = a_i_2d (ii,jl1) - airdg1(ii) - airft1(ii)
               v_i_2d (ii,jl1) = v_i_2d (ii,jl1)     * ( 1._wp - afrdg - afrft ) 
               v_s_2d (ii,jl1) = v_s_2d (ii,jl1)     * ( 1._wp - afrdg - afrft )
               oa_i_2d(ii,jl1) = oa_i_2d(ii,jl1)     * ( 1._wp - afrdg - afrft )
               IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
                  a_ip_2d(ii,jl1)  = a_ip_2d(ii,jl1) * ( 1._wp - afrdg - afrft )
                  v_ip_2d(ii,jl1)  = v_ip_2d(ii,jl1) * ( 1._wp - afrdg - afrft ) 
                  IF ( ln_pnd_lids ) v_il_2d(ii,jl1) = v_il_2d(ii,jl1) * ( 1._wp - afrdg - afrft )
               ENDIF
               !
               e_s_2d(ii,:,jl1) = e_s_2d(ii,:,jl1)   * ( 1._wp - afrdg - afrft )
               e_i_2d(ii,:,jl1) = e_i_2d(ii,:,jl1)   * ( 1._wp - afrdg - afrft )
               !
               IF( nn_icesal == 4 ) THEN   ;   szv_i_2d(ii,:,jl1) = szv_i_2d(ii,:,jl1) * ( 1._wp - afrdg - afrft ) 
               ELSE                        ;   sv_i_2d (ii,  jl1) = sv_i_2d (ii,  jl1) * ( 1._wp - afrdg - afrft ) 
               ENDIF
               
            ENDIF
            
         END DO ! ii
         
 
         ! 3) compute categories in which ice is added (jl2)
         !--------------------------------------------------
         itest_rdg(1:npti) = 0
         itest_rft(1:npti) = 0
         DO jl2  = 1, jpl
            !
            DO ii = 1, npti

               IF( ll_shift(ii) ) THEN

                  ! Compute the fraction of ridged ice area and volume going to thickness category jl2
                  IF( ln_distf_lin ) THEN ! Hibler (1980) linear formulation
                     !
                     IF( phrmin(ii,jl1) <= hi_max(jl2) .AND. phrmax(ii,jl1) > hi_max(jl2-1) ) THEN
                        hL = MAX( phrmin(ii,jl1), hi_max(jl2-1) )
                        hR = MIN( phrmax(ii,jl1), hi_max(jl2)   )
                        farea = ( hR      - hL      ) / ( phrmax(ii,jl1)                  - phrmin(ii,jl1)                  )
                        fvol  = ( hR * hR - hL * hL ) / ( phrmax(ii,jl1) * phrmax(ii,jl1) - phrmin(ii,jl1) * phrmin(ii,jl1) )
                        !
                        itest_rdg(ii) = 1   ! test for conservation
                     ELSE
                        farea = 0._wp
                        fvol  = 0._wp
                     ENDIF
                     !
                  ELSEIF( ln_distf_exp ) THEN ! Lipscomb et al. (2007) exponential formulation                      
                     !
                     IF( jl2 < jpl ) THEN
                        !
                        IF( phrmin(ii,jl1) <= hi_max(jl2) ) THEN
                           hL    = MAX( phrmin(ii,jl1), hi_max(jl2-1) )
                           hR    = hi_max(jl2)
                           expL  = EXP( -( hL - phrmin(ii,jl1) ) / MAX( epsi20, phrexp(ii,jl1) ) )
                           expR  = EXP( -( hR - phrmin(ii,jl1) ) / MAX( epsi20, phrexp(ii,jl1) ) )
                           farea = expL - expR
                           fvol  = ( ( hL + phrexp(ii,jl1) ) * expL  &
                              &    - ( hR + phrexp(ii,jl1) ) * expR ) / MAX( epsi20, phrmin(ii,jl1) + phrexp(ii,jl1) )
                        ELSE
                           farea = 0._wp
                           fvol  = 0._wp
                        END IF
                        !                 
                     ELSE             ! jl2 = jpl
                        !
                        hL    = MAX( phrmin(ii,jl1), hi_max(jl2-1) )
                        expL  = EXP(-( hL - phrmin(ii,jl1) ) / MAX( epsi20, phrexp(ii,jl1) ) )
                        farea = expL
                        fvol  = ( hL + phrexp(ii,jl1) ) * expL / MAX( epsi20, phrmin(ii,jl1) + phrexp(ii,jl1) )
                        !
                     END IF            ! jl2 < jpl
                     ! 
                     itest_rdg(ii) = 1   ! test for conservation => clem: I am not sure about that
                     !
                  END IF             ! ridge redistribution
                     
                  ! Compute the fraction of rafted ice area and volume going to thickness category jl2
                  IF( phraft(ii,jl1) <= hi_max(jl2) .AND. phraft(ii,jl1) >  hi_max(jl2-1) ) THEN
                     zswitch = 1._wp
                     !
                     itest_rft(ii) = 1   ! test for conservation
                  ELSE
                     zswitch = 0._wp
                  ENDIF
                  !
                  ! Patch to ensure perfect conservation if ice thickness goes mad
                  ! Sometimes thickness is larger than hi_max(jpl) because of advection scheme (for very small areas)
                  ! Then ice volume is removed from one category but the ridging/rafting scheme
                  ! does not know where to move it, leading to a conservation issue.
                  IF( itest_rdg(ii) == 0 .AND. jl2 == jpl ) THEN   ;   farea = 1._wp   ;   fvol = 1._wp   ;   ENDIF
                  IF( itest_rft(ii) == 0 .AND. jl2 == jpl ) zswitch = 1._wp
                  !
                  ! Add area, volume of new ridge to category jl2
                  !----------------------------------------------
                  a_i_2d (ii,jl2) = a_i_2d (ii,jl2) + ( airdg2(ii)              * farea + airft2(ii)              * zswitch )
                  oa_i_2d(ii,jl2) = oa_i_2d(ii,jl2) + ( oirdg (ii)              * farea + oirft (ii)              * zswitch )
                  v_i_2d (ii,jl2) = v_i_2d (ii,jl2) + ( virdg (ii)              * fvol  + virft (ii)              * zswitch )
                  v_s_2d (ii,jl2) = v_s_2d (ii,jl2) + ( vsrdg (ii) * rn_fsnwrdg * fvol  + vsrft (ii) * rn_fsnwrft * zswitch )
                  IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
                     v_ip_2d (ii,jl2) = v_ip_2d(ii,jl2)    + ( vprdg(ii) * rn_fpndrdg * fvol  + vprft(ii) * rn_fpndrft * zswitch )
                     a_ip_2d (ii,jl2) = a_ip_2d(ii,jl2)    + ( aprdg(ii) * rn_fpndrdg * farea + aprft(ii) * rn_fpndrft * zswitch )
                     IF ( ln_pnd_lids ) THEN
                        v_il_2d (ii,jl2) = v_il_2d(ii,jl2) + ( vlrdg(ii) * rn_fpndrdg * fvol  + vlrft(ii) * rn_fpndrft * zswitch )
                     ENDIF
                  ENDIF
                  e_s_2d(ii,:,jl2) = e_s_2d(ii,:,jl2) + ( esrdg(ii,:) * rn_fsnwrdg * fvol + esrft(ii,:) * rn_fsnwrft * zswitch )
                  e_i_2d(ii,:,jl2) = e_i_2d(ii,:,jl2) + ( eirdg(ii,:)              * fvol + eirft(ii,:)              * zswitch )
                  IF( nn_icesal == 4 ) THEN
                     szv_i_2d(ii,:,jl2) = szv_i_2d(ii,:,jl2) + ( sirdg(ii,:) * fvol + sirft(ii,:) * zswitch )
                  ELSE
                     sv_i_2d (ii,  jl2) = sv_i_2d (ii,  jl2) + ( sirdg(ii,1) * fvol + sirft(ii,1) * zswitch )
                  ENDIF
               ENDIF

            END DO
            !
         END DO ! jl2
         !
      END DO ! jl1
      !
      ! roundoff errors
      !----------------
      ! In case ridging/rafting lead to very small negative values (sometimes it happens)
      CALL ice_var_roundoff( a_i_2d, v_i_2d, v_s_2d, sv_i_2d, oa_i_2d, a_ip_2d, v_ip_2d, v_il_2d, e_s_2d, e_i_2d, szv_i_2d )
      !
   END SUBROUTINE rdgrft_shift


   SUBROUTINE ice_strength
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE ice_strength ***
      !!
      !! ** Purpose :   computes ice strength used in dynamics routines of ice thickness
      !!
      !! ** Method  :   Compute the strength of the ice pack, defined as the energy (J m-2) 
      !!              dissipated per unit area removed from the ice pack under compression,
      !!              and assumed proportional to the change in potential energy caused
      !!              by ridging. Note that ice strength using Hibler's formulation must be
      !!              smoothed.
      !!----------------------------------------------------------------------
      INTEGER             ::   ii, ji, jj, jl  ! dummy loop indices
      REAL(wp)            ::   z1_3        ! local scalars
      REAL(wp), DIMENSION(A2D(0))   ::   zworka         ! temporary array used here
      REAL(wp), DIMENSION(jpij,jpl) ::   za_i_cap       ! local capped ice concentration
      !!
      LOGICAL             ::   ln_str_R75
      REAL(wp)            ::   zhi, zcp
      REAL(wp)            ::   h2rdg                     ! mean value of h^2 for new ridge
      REAL(wp), PARAMETER ::   zmax_strength = 200.e3_wp ! Max strength for R75 formulation. Richter-Menge and Elder (1998) estimate maximum in Beaufort Sea in wintertime of the order 150 kN/m.
      ! Coon et al. (2007) state that 20 kN/m is ~10% of the maximum compressive strength of isotropic ice, giving max strength of 200 kN/m.
      REAL(wp), DIMENSION(jpij) ::   zstrength           ! strength in 1D   
      REAL(wp), DIMENSION(jpij) ::   zaksum              ! normalisation factor
      !!----------------------------------------------------------------------
      ! at_i needed for strength
      at_i(:,:) = SUM( a_i, dim=3 )
      !
      SELECT CASE( nice_str )          !--- Set which ice strength is chosen

      CASE ( np_strr75 )           !== Rothrock(1975)'s method ==!

         ! this should be defined once for all at the 1st time step
         zcp = 0.5_wp * grav * (rho0-rhoi) * rhoi * r1_rho0   ! proport const for PE
         !
         strength(:,:) = 0._wp
         !
         ! Initialise local capped a_i to zero
         ! Note that if 0 < a_i < epsi10, can end up with zhi=0 but apartf>0 rdgrft_prep; za_i_cap avoids this here
         za_i_cap(:,:) = 0.0
         !
         ! Identify grid cells with ice
#if defined key_si3_1D
         DO_2D( 0, 0, 0, 0 )
            npti = 0   ;   nptidx(:) = 0
            IF ( at_i(ji,jj) > epsi10 ) THEN
               npti           = 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
#else
         npti = 0   ;   nptidx(:) = 0
         DO_2D( 0, 0, 0, 0 )
            IF ( at_i(ji,jj) > epsi10 ) THEN
               npti           = npti + 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
         END_2D
#endif

         IF( npti > 0 ) THEN
            CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d  (1:npti,:), a_i   )
            CALL tab_3d_2d( npti, nptidx(1:npti), v_i_2d  (1:npti,:), v_i   )
            CALL tab_2d_1d( npti, nptidx(1:npti), ato_i_1d(1:npti)  , ato_i )
            CALL tab_2d_1d( npti, nptidx(1:npti), zstrength(1:npti) , strength )

            ! Cap a_i to avoid zhi in rdgrft_prep going below minimum
            za_i_cap(1:npti,:) = a_i_2d(1:npti,:)
            DO jl = 1, jpl
               DO ii = 1, npti
                  IF ( a_i_2d(ii,jl) > epsi10 .AND. ( v_i_2d(ii,jl) / a_i_2d(ii,jl) ) .LT. rn_himin ) THEN
                    za_i_cap(ii,jl) = a_i_2d(ii,jl) * ( v_i_2d(ii,jl) / a_i_2d(ii,jl) ) / rn_himin
                  ENDIF
               END DO
            END DO

            CALL rdgrft_prep( za_i_cap, v_i_2d, ato_i_1d, &                                ! <<== in
               &              apartf, aridge, araft, hi_hrdg, hraft, hrmin, hrmax, hrexp ) ! ==>> out
            !
            zaksum(1:npti) = apartf(1:npti,0) !clem: aksum should be defined in the header => local to module
            DO jl = 1, jpl
               DO ii = 1, npti
                  IF ( apartf(ii,jl) > 0._wp ) THEN
                     zaksum(ii) = zaksum(ii) + aridge(ii,jl) * ( 1._wp - hi_hrdg(ii,jl) )    &
                        &                    + araft (ii,jl) * ( 1._wp - hi_hrft )
                  ENDIF
               END DO
            END DO
            !
            z1_3 = 1._wp / 3._wp
            DO jl = 1, jpl
               DO ii = 1, npti
                  !
                  IF( apartf(ii,jl) > 0._wp ) THEN
                     !
                     IF( ln_distf_lin ) THEN       ! Uniform redistribution of ridged ice               
                        h2rdg = z1_3 * ( hrmax(ii,jl) * hrmax(ii,jl) +     & ! (a**3-b**3)/(a-b) = a*a+ab+b*b
                           &             hrmin(ii,jl) * hrmin(ii,jl) +     &
                           &             hrmax(ii,jl) * hrmin(ii,jl) )
                        !
                     ELSEIF( ln_distf_exp ) THEN   ! Exponential redistribution of ridged ice
                        h2rdg =          hrmin(ii,jl) * hrmin(ii,jl)   &
                           &   + 2._wp * hrmin(ii,jl) * hrexp(ii,jl)   &
                           &   + 2._wp * hrexp(ii,jl) * hrexp(ii,jl)
                     END IF
                     !
                     IF( a_i_2d(ii,jl) > epsi10 ) THEN   ;   zhi = v_i_2d(ii,jl) / a_i_2d(ii,jl)
                     ELSE                                ;   zhi = 0._wp
                     ENDIF

	             ! Make sure ice thickness is not below the minimum
                     ! Do not adjust concentration as don't want strength routine to be able to do this
	             IF( a_i_2d(ii,jl) > epsi10 .AND. zhi < rn_himin ) THEN
                       zhi = rn_himin
                     ENDIF


!!$                     zstrength(ii) = zstrength(ii) -         apartf(ii,jl) * zhi * zhi                  ! PE loss from deforming ice
!!$                     zstrength(ii) = zstrength(ii) + 2._wp * araft (ii,jl) * zhi * zhi                  ! PE gain from rafting ice
!!$                     zstrength(ii) = zstrength(ii) +         aridge(ii,jl) * hi_hrdg(ii,jl) * z1_3 *  & ! PE gain from ridging ice
!!$                        &                                   ( hrmax(ii,jl) * hrmax(ii,jl) +           & ! (a**3-b**3)/(a-b) = a*a+ab+b*b
!!$                        &                                     hrmin(ii,jl) * hrmin(ii,jl) +           &
!!$                        &                                     hrmax(ii,jl) * hrmin(ii,jl) )
                     zstrength(ii) = zstrength(ii) - apartf(ii,jl) * zhi * zhi                  ! PE loss
                     zstrength(ii) = zstrength(ii) + 2._wp * araft(ii,jl) * zhi * zhi           ! PE gain (rafting)
                     zstrength(ii) = zstrength(ii) + aridge(ii,jl) * h2rdg *  hi_hrdg(ii,jl)    ! PE gain (ridging)
                     
                  ENDIF
                  !
               END DO
            END DO
            !
            zstrength(1:npti) = rn_pe_rdg * zcp * zstrength(1:npti) / zaksum(1:npti)
            !
            ! Enforce a maximum for R75 strength
            WHERE( zstrength(1:npti) > zmax_strength ) ; zstrength(1:npti) = zmax_strength
            END WHERE
            !
            CALL tab_1d_2d( npti, nptidx(1:npti), zstrength(1:npti), strength )
            !
         ENDIF
#if defined key_si3_1D
         END_2D
#endif
         CALL lbc_lnk( 'icedyn_rdgrft', strength, 'T', 1.0_wp ) ! this call could be removed if calculations were done on the full domain
         !                                                      ! but we decided it is more efficient this way
         !
      CASE ( np_strh79 )           !== Hibler(1979)'s method ==!
         !
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            IF( at_i(ji,jj) > epsi10 ) THEN
               strength(ji,jj) = rn_pstar * SUM( v_i(ji,jj,:) ) * EXP( -rn_crhg * ( 1._wp - at_i(ji,jj) ) )
            ELSE
               strength(ji,jj) = 0._wp
            ENDIF
         END_2D
         !
      CASE ( np_strcst )           !== Constant strength ==!
         !
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            IF( at_i(ji,jj) > epsi10 ) THEN
               strength(ji,jj) = rn_str
            ELSE
               strength(ji,jj) = 0._wp
            ENDIF
         END_2D
         !
      END SELECT
      !
      IF( ln_str_smooth ) THEN         !--- Spatial smoothing
         DO_2D( 0, 0, 0, 0 )
            IF( at_i(ji,jj) > epsi10 ) THEN
               zworka(ji,jj) = ( 4._wp * strength(ji,jj)              &
                  &                    + ( ( strength(ji-1,jj) * tmask(ji-1,jj,1) + strength(ji+1,jj) * tmask(ji+1,jj,1) ) &
                  &                      + ( strength(ji,jj-1) * tmask(ji,jj-1,1) + strength(ji,jj+1) * tmask(ji,jj+1,1) ) ) &
                  &            ) / ( 4._wp + tmask(ji-1,jj,1) + tmask(ji+1,jj,1) + tmask(ji,jj-1,1) + tmask(ji,jj+1,1) )
            ELSE
               zworka(ji,jj) = 0._wp
            ENDIF
         END_2D

         DO_2D( 0, 0, 0, 0 )
            strength(ji,jj) = zworka(ji,jj)
         END_2D
         CALL lbc_lnk( 'icedyn_rdgrft', strength, 'T', 1.0_wp )
         !
      ENDIF
      !
   END SUBROUTINE ice_strength


   SUBROUTINE ice_dyn_1d2d( kn )
      !!-----------------------------------------------------------------------
      !!                   ***  ROUTINE ice_dyn_1d2d ***
      !!
      !! ** Purpose :   move arrays from 1d to 2d and the reverse
      !!-----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kn   ! 1= from 2D to 1D   ;   2= from 1D to 2D
      !
      INTEGER ::   jl, jk   ! dummy loop indices
      !!-----------------------------------------------------------------------
      !
      SELECT CASE( kn )
      !                    !---------------------!
      CASE( 1 )            !==  from 2D to 1D  ==!
         !                 !---------------------!
         ! fields used but not modified
         CALL tab_2d_1d( npti, nptidx(1:npti), sss_1d(1:npti), sss_m(:,:) )
         CALL tab_2d_1d( npti, nptidx(1:npti), sst_1d(1:npti), sst_m(:,:) )
         ! the following fields are modified in this routine
         !!CALL tab_2d_1d( npti, nptidx(1:npti), ato_i_1d(1:npti), ato_i(:,:) )
         !!CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d(1:npti,:), a_i(:,:,:) )
         !!CALL tab_3d_2d( npti, nptidx(1:npti), v_i_2d  (1:npti,:), v_i  (:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), v_s_2d  (1:npti,:)  , v_s (:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), sv_i_2d (1:npti,:)  , sv_i(:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), oa_i_2d (1:npti,:)  , oa_i(:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), a_ip_2d (1:npti,:)  , a_ip(:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), v_ip_2d (1:npti,:)  , v_ip(:,:,:) )
         CALL tab_3d_2d( npti, nptidx(1:npti), v_il_2d (1:npti,:)  , v_il(:,:,:) )
         CALL tab_4d_3d( npti, nptidx(1:npti), e_s_2d  (1:npti,:,:), e_s   )
         CALL tab_4d_3d( npti, nptidx(1:npti), e_i_2d  (1:npti,:,:), e_i   )
         CALL tab_4d_3d( npti, nptidx(1:npti), szv_i_2d(1:npti,:,:), szv_i )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_dyn_1d    (1:npti), sfx_dyn    (:,:) )
         CALL tab_2d_1d( npti, nptidx(1:npti), sfx_bri_1d    (1:npti), sfx_bri    (:,:) )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_dyn_1d    (1:npti), wfx_dyn    (:,:) )
         CALL tab_2d_1d( npti, nptidx(1:npti), hfx_dyn_1d    (1:npti), hfx_dyn    (:,:) )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_snw_dyn_1d(1:npti), wfx_snw_dyn(:,:) )
         CALL tab_2d_1d( npti, nptidx(1:npti), wfx_pnd_1d    (1:npti), wfx_pnd    (:,:) )
         !
         !                 !---------------------!
      CASE( 2 )            !==  from 1D to 2D  ==!
         !                 !---------------------!
         CALL tab_1d_2d( npti, nptidx(1:npti), ato_i_1d(1:npti)    , ato_i(:,:)  )
         CALL tab_2d_3d( npti, nptidx(1:npti), a_i_2d  (1:npti,:)  , a_i (:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), v_i_2d  (1:npti,:)  , v_i (:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), v_s_2d  (1:npti,:)  , v_s (:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), sv_i_2d (1:npti,:)  , sv_i(:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), oa_i_2d (1:npti,:)  , oa_i(:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), a_ip_2d (1:npti,:)  , a_ip(:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), v_ip_2d (1:npti,:)  , v_ip(:,:,:) )
         CALL tab_2d_3d( npti, nptidx(1:npti), v_il_2d (1:npti,:)  , v_il(:,:,:) )
         CALL tab_3d_4d( npti, nptidx(1:npti), e_s_2d  (1:npti,:,:), e_s   )
         CALL tab_3d_4d( npti, nptidx(1:npti), e_i_2d  (1:npti,:,:), e_i   )
         CALL tab_3d_4d( npti, nptidx(1:npti), szv_i_2d(1:npti,:,:), szv_i )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_dyn_1d    (1:npti), sfx_dyn    (:,:) )
         CALL tab_1d_2d( npti, nptidx(1:npti), sfx_bri_1d    (1:npti), sfx_bri    (:,:) )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_dyn_1d    (1:npti), wfx_dyn    (:,:) )
         CALL tab_1d_2d( npti, nptidx(1:npti), hfx_dyn_1d    (1:npti), hfx_dyn    (:,:) )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_snw_dyn_1d(1:npti), wfx_snw_dyn(:,:) )
         CALL tab_1d_2d( npti, nptidx(1:npti), wfx_pnd_1d    (1:npti), wfx_pnd    (:,:) )
         !
         ! --- Ridging diagnostics --- !
         IF( ll_diag_rdg ) THEN
           CALL tab_1d_2d( npti, nptidx(1:npti), opning(1:npti)             , opning_2d(:,:) )
           CALL tab_1d_2d( npti, nptidx(1:npti), airdg1(1:npti) * r1_Dt_ice , dairdg1dt(:,:) )
           CALL tab_1d_2d( npti, nptidx(1:npti), airft1(1:npti) * r1_Dt_ice , dairft1dt(:,:) )
           CALL tab_1d_2d( npti, nptidx(1:npti), airdg2(1:npti) * r1_Dt_ice , dairdg2dt(:,:) )
           CALL tab_1d_2d( npti, nptidx(1:npti), airft2(1:npti) * r1_Dt_ice , dairft2dt(:,:) )
         ENDIF
         !
      END SELECT
      !
   END SUBROUTINE ice_dyn_1d2d


   SUBROUTINE ice_dyn_rdgrft_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_dyn_rdgrft_init ***
      !!
      !! ** Purpose :   Physical constants and parameters linked
      !!                to the mechanical ice redistribution
      !!
      !! ** Method  :   Read the namdyn_rdgrft namelist
      !!                and check the parameters values
      !!                called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namdyn_rdgrft
      !!-------------------------------------------------------------------
      INTEGER :: ios, ioptio                ! Local integer output status for namelist read
      !!
      NAMELIST/namdyn_rdgrft/ ln_str_H79, rn_pstar, rn_crhg, ln_str_R75, rn_pe_rdg, ln_str_CST, rn_str, ln_str_smooth, &
         &                    ln_distf_lin, ln_distf_exp, rn_murdg, rn_csrdg,            &
         &                    ln_partf_lin, rn_gstar, ln_partf_exp, rn_astar,            &
         &                    ln_ridging, rn_hstar, rn_porordg, rn_fsnwrdg, rn_fpndrdg,  &
         &                    ln_rafting, rn_hraft, rn_craft  , rn_fsnwrft, rn_fpndrft
      !!-------------------------------------------------------------------
      !
      READ_NML_REF(numnam_ice,namdyn_rdgrft)
      READ_NML_CFG(numnam_ice,namdyn_rdgrft)
      IF(lwm) WRITE ( numoni, namdyn_rdgrft )
      !
      IF (lwp) THEN                          ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_dyn_rdgrft_init: ice parameters for ridging/rafting '
         WRITE(numout,*) '~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namdyn_rdgrft:'
         WRITE(numout,*) '      ice strength parameterization Hibler (1979)              ln_str_H79   = ', ln_str_H79
         WRITE(numout,*) '            1st bulk-rheology parameter                        rn_pstar     = ', rn_pstar
         WRITE(numout,*) '            2nd bulk-rhelogy parameter                         rn_crhg      = ', rn_crhg
         WRITE(numout,*) '      ice strength parameterization Rothrock (1975)            ln_str_R75   = ', ln_str_R75
         WRITE(numout,*) '            coef accounting for frictional dissipation         rn_pe_rdg    = ', rn_pe_rdg         
         WRITE(numout,*) '      ice strength parameterization Constant                   ln_str_CST   = ', ln_str_CST
         WRITE(numout,*) '            ice strength value                                 rn_str       = ', rn_str
         WRITE(numout,*) '      spatial smoothing of the strength                        ln_str_smooth= ', ln_str_smooth
         WRITE(numout,*) '      redistribution of ridged ice: linear (Hibler 1980)       ln_distf_lin = ', ln_distf_lin
         WRITE(numout,*) '      redistribution of ridged ice: exponential(Lipscomb 2017) ln_distf_exp = ', ln_distf_exp
         WRITE(numout,*) '            e-folding scale of ridged ice                      rn_murdg     = ', rn_murdg
         WRITE(numout,*) '      Fraction of shear energy contributing to ridging         rn_csrdg     = ', rn_csrdg
         WRITE(numout,*) '      linear ridging participation function                    ln_partf_lin = ', ln_partf_lin
         WRITE(numout,*) '            Fraction of ice coverage contributing to ridging   rn_gstar     = ', rn_gstar
         WRITE(numout,*) '      Exponential ridging participation function               ln_partf_exp = ', ln_partf_exp
         WRITE(numout,*) '            Equivalent to G* for an exponential function       rn_astar     = ', rn_astar
         WRITE(numout,*) '      Ridging of ice sheets or not                             ln_ridging   = ', ln_ridging
         WRITE(numout,*) '            max ridged ice thickness                           rn_hstar     = ', rn_hstar
         WRITE(numout,*) '            Initial porosity of ridges                         rn_porordg   = ', rn_porordg
         WRITE(numout,*) '            Fraction of snow volume conserved during ridging   rn_fsnwrdg   = ', rn_fsnwrdg
         WRITE(numout,*) '            Fraction of pond volume conserved during ridging   rn_fpndrdg   = ', rn_fpndrdg
         WRITE(numout,*) '      Rafting of ice sheets or not                             ln_rafting   = ', ln_rafting
         WRITE(numout,*) '            Parmeter thickness (threshold between ridge-raft)  rn_hraft     = ', rn_hraft
         WRITE(numout,*) '            Rafting hyperbolic tangent coefficient             rn_craft     = ', rn_craft
         WRITE(numout,*) '            Fraction of snow volume conserved during rafting   rn_fsnwrft   = ', rn_fsnwrft
         WRITE(numout,*) '            Fraction of pond volume conserved during rafting   rn_fpndrft   = ', rn_fpndrft
      ENDIF
      !
      ioptio = 0
      IF( ln_str_H79    ) THEN   ;   ioptio = ioptio + 1   ;   nice_str = np_strh79       ;   ENDIF
      IF( ln_str_R75    ) THEN   ;   ioptio = ioptio + 1   ;   nice_str = np_strr75       ;   ENDIF
      IF( ln_str_CST    ) THEN   ;   ioptio = ioptio + 1   ;   nice_str = np_strcst       ;   ENDIF
      IF( ioptio /= 1 )   &
         &   CALL ctl_stop( 'ice_dyn_rdgrft_init: one and only one ice strength option has to be defined ' )
      !
      ioptio = 0
      IF( ln_distf_lin ) THEN   ;   ioptio = ioptio + 1   ;   ENDIF
      IF( ln_distf_exp ) THEN   ;   ioptio = ioptio + 1   ;   ENDIF
      IF( ioptio /= 1 )   &
         &   CALL ctl_stop( 'ice_dyn_rdgrft_init: choose one and only one redistribution function (ln_distf_lin or ln_distf_exp)' )
      !
      ioptio = 0
      IF( ln_partf_lin ) THEN   ;   ioptio = ioptio + 1   ;   ENDIF
      IF( ln_partf_exp ) THEN   ;   ioptio = ioptio + 1   ;   ENDIF
      IF( ioptio /= 1 )   &
         &   CALL ctl_stop( 'ice_dyn_rdgrft_init: choose one and only one participation function (ln_partf_lin or ln_partf_exp)' )
      !
      IF( .NOT. ln_icethd ) THEN
         rn_porordg = 0._wp
         rn_fsnwrdg = 1._wp ; rn_fsnwrft = 1._wp
         rn_fpndrdg = 1._wp ; rn_fpndrft = 1._wp
         IF( lwp ) THEN
            WRITE(numout,*) '      ==> only ice dynamics is activated, thus some parameters must be changed'
            WRITE(numout,*) '            rn_porordg   = ', rn_porordg
            WRITE(numout,*) '            rn_fsnwrdg   = ', rn_fsnwrdg
            WRITE(numout,*) '            rn_fpndrdg   = ', rn_fpndrdg
            WRITE(numout,*) '            rn_fsnwrft   = ', rn_fsnwrft
            WRITE(numout,*) '            rn_fpndrft   = ', rn_fpndrft
         ENDIF
      ENDIF
      !
      ! diagnostics
      IF( iom_use('lead_open') .OR. iom_use('rdg_loss') .OR. iom_use('rft_loss') .OR. &
         &                          iom_use('rdg_gain') .OR. iom_use('rft_gain') ) THEN
         ll_diag_rdg = .TRUE.
      ELSE
         ll_diag_rdg = .FALSE.
      ENDIF
      !                              ! allocate arrays
      IF( ice_dyn_rdgrft_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'ice_dyn_rdgrft_init: unable to allocate arrays' )
      !
  END SUBROUTINE ice_dyn_rdgrft_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icedyn_rdgrft
