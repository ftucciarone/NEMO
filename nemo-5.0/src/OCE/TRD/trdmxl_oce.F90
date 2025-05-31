MODULE trdmxl_oce
   !!======================================================================
   !!                   ***  MODULE trdmxl_oce  ***
   !! Ocean trends :   set tracer and momentum trend variables
   !!======================================================================
   !! History :  1.0  ! 2004-08  (C. Talandier)  New trends organization
   !!            3.5  ! 2012-02  (G. Madec) suppress the trend keys + new trdmxl formulation
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trdmxl_oce_alloc    ! Called in trdmxl.F90

   !                                                !* mixed layer trend indices
   INTEGER, PUBLIC, PARAMETER ::   jpltrd = 12      !: number of mixed-layer trends arrays
   INTEGER, PUBLIC            ::   jpktrd           !: max level for mixed-layer trends diag.
   !
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_xad =  1   !: i-componant of advection   
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_yad =  2   !: j-componant of advection
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_zad =  3   !: k-component of advection 
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_ldf =  4   !: lateral diffusion (geopot. or iso-neutral)
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_zdf =  5   !: vertical diffusion  
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_npc =  6   !: non penetrative convective adjustment
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_bbc =  7   !: geothermal flux
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_bbl =  8   !: bottom boundary layer (advective/diffusive)
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_for =  9   !: forcing 
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_dmp = 10   !: internal restoring trend
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_zdfp = 11  !: ! iso-neutral diffusion:"pure" vertical diffusion
   INTEGER, PUBLIC, PARAMETER ::   jpmxl_atf  = 12  !: asselin trend (**MUST BE THE LAST ONE**)
   !                                                            !!* Namelist namtrd_mxl:  trend diagnostics in the mixed layer *
   INTEGER           , PUBLIC ::   nn_ctls  = 0                  !: control surface type for trends vertical integration
   REAL(wp)          , PUBLIC ::   rn_rho_c = 0.01               !: density criteria for MLD definition
   REAL(wp)          , PUBLIC ::   rn_ucf   = 1.                 !: unit conversion factor (for netCDF trends outputs)
                                                                 !  =1. (=86400.) for degC/s (degC/day) and psu/s (psu/day)
   CHARACTER(len=32), PUBLIC ::   cn_trdrst_in  = "restart_mxl"  !: suffix of ocean restart name (input)
   CHARACTER(len=32), PUBLIC ::   cn_trdrst_out = "restart_mxl"  !: suffix of ocean restart name (output)
   LOGICAL          , PUBLIC ::   ln_trdmxl_instant = .FALSE.    !: flag to diagnose inst./mean ML T/S trends
   LOGICAL          , PUBLIC ::   ln_trdmxl_restart = .FALSE.    !: flag to restart mixed-layer diagnostics


   !! Arrays used for diagnosing mixed-layer trends 
   !!---------------------------------------------------------------------
   CHARACTER(LEN=80) , PUBLIC :: clname, ctrd(jpltrd+1,2)

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nmxl   !: mixed layer depth indexes 
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   nbol   !: mixed-layer depth indexes when read from file

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   wkx    !:

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  &
      hmxl   ,                      & !: mixed layer depth (m) corresponding to nmld
      tml    , sml  ,               & !: \ "now" mixed layer temperature/salinity
      tmlb   , smlb ,               & !: /  and associated "before" fields
      tmlbb  , smlbb,               & !: \  idem, but valid at the 1rst time step of the
      tmlbn  , smlbn,               & !: /  current analysis window
      tmltrdm, smltrdm,             & !: total cumulative trends over the analysis window
      tml_sum,                      & !: mixed layer T, summed over the current analysis period
      tml_sumb,                     & !: idem, but from the previous analysis period
      tmltrd_atf_sumb,              & !: Asselin trends, summed over the previous analysis period
      sml_sum,                      & !: 
      sml_sumb,                     & !:    ( idem for salinity )
      smltrd_atf_sumb,              & !: 
      hmxl_sum, hmxlbn                !: needed to compute the leap-frog time mean of the ML depth

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  &
      tmlatfb, tmlatfn ,            & !: "before" Asselin contribution at begining of the averaging
      smlatfb, smlatfn,             & !: period (i.e. last contrib. from previous such period) and 
                                      !: "now" Asselin contribution to the ML temp. & salinity trends
      tmlatfm, smlatfm                !: accumulator for Asselin trends (needed for storage only)

   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) ::  &
      tmltrd,                       & !: \ physical contributions to the total trend (for T/S),
      smltrd,                       & !: / cumulated over the current analysis window
      tmltrd_sum,                   & !: sum of these trends over the analysis period
      tmltrd_csum_ln,               & !: now cumulated sum of the trends over the "lower triangle"
      tmltrd_csum_ub,               & !: before (prev. analysis period) cumulated sum over the upper triangle
      smltrd_sum,                   & !: 
      smltrd_csum_ln,               & !:    ( idem for salinity )
      smltrd_csum_ub                  !: 

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

  INTEGER FUNCTION trdmxl_oce_alloc()
     !!----------------------------------------------------------------------
     !!                 ***  FUNCTION trdmxl_oce_alloc   ***
     !!----------------------------------------------------------------------
     USE lib_mpp
     INTEGER :: ierr(5)
     !!----------------------------------------------------------------------

     ! Initialise jpktrd here as can no longer do it in MODULE body since
     ! jpk is now a variable.
     jpktrd = jpk   !: max level for mixed-layer trends diag.

     ierr(:) = 0

     ALLOCATE( nmxl (T2D(0))    , nbol (T2D(0)),    &
        &      wkx  (T2D(0),jpk), hmxl (T2D(0)),    &
        &      tml  (T2D(0))    , sml  (T2D(0)),    &
        &      tmlb (T2D(0))    , smlb (T2D(0)),    &
        &      tmlbb(T2D(0))    , smlbb(T2D(0)), STAT = ierr(1) )

     ALLOCATE( tmlbn(T2D(0))  , smlbn(T2D(0)),   &
        &      tmltrdm(T2D(0)), smltrdm(T2D(0)), &
        &      tml_sum(T2D(0)), tml_sumb(T2D(0)),&
        &      tmltrd_atf_sumb(T2D(0))           , STAT=ierr(2) )

     ALLOCATE( sml_sum(T2D(0)), sml_sumb(T2D(0)), &
        &      smltrd_atf_sumb(T2D(0)),           &
        &      hmxl_sum(T2D(0)), hmxlbn(T2D(0)),  &
        &      tmlatfb(T2D(0)), tmlatfn(T2D(0)), STAT = ierr(3) )

     ALLOCATE( smlatfb(T2D(0)), smlatfn(T2D(0)), &
        &      tmlatfm(T2D(0)), smlatfm(T2D(0)), &
        &      tmltrd(T2D(0),jpltrd),   smltrd(T2D(0),jpltrd), STAT=ierr(4))

     ALLOCATE( tmltrd_sum(T2D(0),jpltrd),tmltrd_csum_ln(T2D(0),jpltrd),      &
        &      tmltrd_csum_ub(T2D(0),jpltrd), smltrd_sum(T2D(0),jpltrd),     &
        &      smltrd_csum_ln(T2D(0),jpltrd), smltrd_csum_ub(T2D(0),jpltrd), STAT=ierr(5) )
      !
      trdmxl_oce_alloc = MAXVAL( ierr )
      CALL mpp_sum ( 'trdmxl_oce', trdmxl_oce_alloc )
      IF( trdmxl_oce_alloc /= 0 )   CALL ctl_stop( 'STOP', 'trdmxl_oce_alloc: failed to allocate arrays' )
      !
   END FUNCTION trdmxl_oce_alloc

   !!======================================================================
END MODULE trdmxl_oce
