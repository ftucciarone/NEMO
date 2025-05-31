MODULE sbc_ice
   !!======================================================================
   !!                 ***  MODULE  sbc_ice  ***
   !! Surface module - SI3 & CICE: parameters & variables defined in memory
   !!======================================================================
   !! History :  3.0   !  2006-08  (G. Madec)        Surface module
   !!            3.2   !  2009-06  (S. Masson)       merge with ice_oce
   !!            3.3.1 !  2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!            3.4   !  2011-11  (C. Harris)       CICE added as an option
   !!            4.0   !  2018     (many people)     SI3 compatibility
   !!----------------------------------------------------------------------
#if defined key_si3 || defined key_cice
   !!----------------------------------------------------------------------
   !!   'key_si3' or 'key_cice' :              SI3 or CICE sea-ice model
   !!----------------------------------------------------------------------
   USE par_oce          ! ocean parameters
   USE sbc_oce          ! surface boundary condition: ocean
# if defined key_si3
   USE par_ice          ! SI3 parameters
# endif
# if defined key_cice
   USE ice_domain_size, only: ncat
#endif
   USE lib_mpp          ! MPP library
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_ice_alloc   ! called in sbcmod.F90 or sbcice_cice.F90
   PUBLIC   sbc_ice_dealloc ! called in nemogcm.F90

# if defined  key_si3
   LOGICAL         , PUBLIC, PARAMETER ::   lk_si3     = .TRUE.   !: SI3 ice model
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .FALSE.  !: no CICE
# endif
# if defined  key_cice
   LOGICAL         , PUBLIC, PARAMETER ::   lk_si3     = .FALSE.  !: no SI3
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .TRUE.   !: CICE ice model
# endif

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qns_ice        !: non solar heat flux over ice                  [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qsr_ice        !: solar heat flux over ice                      [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qla_ice        !: latent flux over ice                          [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dqla_ice       !: latent sensibility over ice                 [W/m2/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   dqns_ice       !: non solar heat flux over ice (LW+SEN+LA)    [W/m2/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tn_ice         !: ice surface temperature                          [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   alb_ice        !: ice albedo                                       [-]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qml_ice        !: heat available for snow / ice surface melting     [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qcn_ice        !: heat conduction flux in the layer below surface   [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qtr_ice_top    !: solar flux transmitted below the ice surface      [W/m2]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   utau_ice       !: atmos-ice u-stress. T-pts                  [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   vtau_ice       !: atmos-ice v-stress. T-pts                  [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_ice        !: sublimation - precip over sea ice          [kg/m2/s]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   topmelt            !: category topmelt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   botmelt            !: category botmelt

#if defined  key_si3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   evap_ice       !: sublimation                              [kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   devap_ice      !: sublimation sensitivity                [kg/m2/s/K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qns_oce        !: non solar heat flux over ocean              [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qsr_oce        !: non solar heat flux over ocean              [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qemp_oce       !: heat flux of precip and evap over ocean     [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qemp_ice       !: heat flux of precip and evap over ice       [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qevap_ice      !: heat flux of evap over ice                  [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qprec_ice      !: enthalpy of precip over ice                 [J/m3]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_oce        !: evap - precip over ocean                 [kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wndm_ice       !: wind speed module at T-point                 [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sstfrz         !: sea surface freezing temperature            [degC]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rCdU_ice       !: ice-ocean drag at T-point (<0)               [m/s]
#endif

#if defined key_cice
   !
   ! for consistency with SI3, these are declared with three dimensions
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qlw_ice            !: incoming long-wave
   !
   ! other forcing arrays are two dimensional
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ss_iou             !: x ice-ocean surface stress at NEMO U point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ss_iov             !: y ice-ocean surface stress at NEMO V point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qatm_ice           !: specific humidity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wndi_ice           !: i wind at T point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wndj_ice           !: j wind at T point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nfrzmlt            !: NEMO frzmlt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fr_iu              !: ice fraction at NEMO U point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fr_iv              !: ice fraction at NEMO V point

   ! variables used in the coupled interface
   INTEGER , PUBLIC, PARAMETER ::   jpl = ncat
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   u_ice, v_ice

   ! already defined in ice.F90 for SI3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  a_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  h_i, h_s

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   tatm_ice       !: air temperature [K]
#endif

   !! arrays relating to embedding ice in the ocean
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass        !: mass of snow and ice at current  ice time step   [Kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass_b      !: mass of snow and ice at previous ice time step   [Kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_fmass       !: time evolution of mass of snow+ice               [Kg/m2/s]

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_ice_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  FUNCTION sbc_ice_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr(5), ii
      !!----------------------------------------------------------------------
      ierr(:) = 0
      ii = 0

      ii = ii + 1
      ALLOCATE( snwice_mass(jpi,jpj) , snwice_mass_b(jpi,jpj), snwice_fmass(jpi,jpj) , STAT=ierr(ii) )

#if defined key_si3
      ! ----------------- !
      ! == FULL ARRAYS == !
      ! ----------------- !
      ii = ii + 1
      ALLOCATE( utau_ice(jpi,jpj) , vtau_ice(jpi,jpj) ,  &
         &      rCdU_ice(A2D(1))                      , STAT= ierr(ii) )
      ! -------------------- !
      ! == REDUCED ARRAYS == !
      ! -------------------- !
      ii = ii + 1
      ALLOCATE( wndm_ice(A2D(0))     , &
         &      qns_ice (A2D(0),jpl) , qsr_ice  (A2D(0),jpl) ,     &
         &      qla_ice (A2D(0),jpl) , dqla_ice (A2D(0),jpl) ,     &
         &      dqns_ice(A2D(0),jpl) , tn_ice   (A2D(0),jpl) , alb_ice    (A2D(0),jpl) ,   &
         &      qml_ice (A2D(0),jpl) , qcn_ice  (A2D(0),jpl) , qtr_ice_top(A2D(0),jpl) ,   &
         &      evap_ice(A2D(0),jpl) , devap_ice(A2D(0),jpl) , qprec_ice  (A2D(0))     ,   &
         &      qemp_ice(A2D(0))     , qevap_ice(A2D(0),jpl) , qemp_oce   (A2D(0))     ,   &
         &      qns_oce (A2D(0))     , qsr_oce  (A2D(0))     , emp_oce    (A2D(0))     ,   &
         &      emp_ice (A2D(0))     , sstfrz   (A2D(0))     , STAT= ierr(ii) )
#endif

#if defined key_cice
      ii = ii + 1
      ALLOCATE( qla_ice(jpi,jpj,1)    , qlw_ice(jpi,jpj,1)    , qsr_ice(jpi,jpj,1)    , &
                wndi_ice(jpi,jpj)     , tatm_ice(jpi,jpj)     , qatm_ice(jpi,jpj)     , &
                wndj_ice(jpi,jpj)     , nfrzmlt(jpi,jpj)      , ss_iou(jpi,jpj)       , &
                ss_iov(jpi,jpj)       , fr_iu(jpi,jpj)        , fr_iv(jpi,jpj)        , &
                a_i(jpi,jpj,ncat)     , topmelt(jpi,jpj,ncat) , botmelt(jpi,jpj,ncat) , &
                STAT= ierr(ii) )
      ii = ii + 1
      IF( ln_cpl )   ALLOCATE( u_ice(jpi,jpj)        , tn_ice (jpi,jpj,1)    , &
         &                     v_ice(jpi,jpj)        , alb_ice(jpi,jpj,1)    , &
         &                     emp_ice(jpi,jpj)      , qns_ice(jpi,jpj,1)    , dqns_ice(jpi,jpj,1)   , &
         &                     h_i(jpi,jpj,jpl) , h_s(jpi,jpj,jpl) ,STAT= ierr(ii) )
#endif

      sbc_ice_alloc = MAXVAL( ierr )
      CALL mpp_sum ( 'sbc_ice', sbc_ice_alloc )
      IF( sbc_ice_alloc > 0 )   CALL ctl_warn('sbc_ice_alloc: allocation of arrays failed')
   END FUNCTION sbc_ice_alloc

   
   SUBROUTINE sbc_ice_dealloc()
      IF( ALLOCATED(snwice_mass) )   DEALLOCATE( snwice_mass , snwice_mass_b, snwice_fmass )
#if defined key_si3
      IF( ALLOCATED(utau_ice) )   DEALLOCATE( utau_ice , vtau_ice ,  rCdU_ice )
      IF( ALLOCATED(wndm_ice) )  &
         &    DEALLOCATE( wndm_ice   , &
         &      qns_ice  , qsr_ice   ,     &
         &      qla_ice  , dqla_ice  ,     &
         &      dqns_ice , tn_ice    , alb_ice     ,   &
         &      qml_ice  , qcn_ice   , qtr_ice_top ,   &
         &      evap_ice , devap_ice , qprec_ice       ,   &
         &      qemp_ice , qevap_ice , qemp_oce        ,   &
         &      qns_oce  , qsr_oce   , emp_oce         ,   &
         &      emp_ice  , sstfrz  )
#endif

   END SUBROUTINE sbc_ice_dealloc

#else
   !!----------------------------------------------------------------------
   !!   Default option                      NO SI3 or CICE sea-ice model
   !!----------------------------------------------------------------------
   USE lib_mpp          ! MPP library
   USE in_out_manager   ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_ice_alloc     !
   PUBLIC   sbc_ice_dealloc   !

   LOGICAL         , PUBLIC, PARAMETER ::   lk_si3     = .FALSE.  !: no SI3 ice model
   LOGICAL         , PUBLIC, PARAMETER ::   lk_cice    = .FALSE.  !: no CICE ice model

   INTEGER         , PUBLIC, PARAMETER ::   jpl = 1
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   u_ice, v_ice                        ! jpi, jpj
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tn_ice, alb_ice, qns_ice, dqns_ice  ! (jpi,jpj,jpl)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   a_i
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   emp_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qsr_ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   h_i, h_s
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   topmelt, botmelt
   !
   !! arrays related to embedding ice in the ocean.
   !! These arrays need to be declared even if no ice model is required.
   !! In the no ice model or traditional levitating ice cases they contain only zeros
   !! ---------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass        !: mass of snow and ice at current  ice time step   [Kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_mass_b      !: mass of snow and ice at previous ice time step   [Kg/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   snwice_fmass       !: time evolution of mass of snow+ice               [Kg/m2/s]
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_ice_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  FUNCTION sbc_ice_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr(1)
      !!----------------------------------------------------------------------
      ierr(:) = 0
      ALLOCATE( snwice_mass(jpi,jpj) , snwice_mass_b(jpi,jpj), snwice_fmass(jpi,jpj) , STAT=ierr(1) )
      sbc_ice_alloc = MAXVAL( ierr )
      CALL mpp_sum ( 'sbc_ice', sbc_ice_alloc )
      IF( sbc_ice_alloc > 0 )   CALL ctl_warn('sbc_ice_alloc: allocation of arrays failed')
   END FUNCTION sbc_ice_alloc

   SUBROUTINE sbc_ice_dealloc()
      IF( ALLOCATED(snwice_mass) ) DEALLOCATE( snwice_mass, snwice_mass_b, snwice_fmass )
   END SUBROUTINE sbc_ice_dealloc
   
#endif

   !!======================================================================
END MODULE sbc_ice
