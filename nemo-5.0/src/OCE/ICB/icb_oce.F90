MODULE icb_oce
   !!======================================================================
   !!                       ***  MODULE  icb_oce  ***
   !! Icebergs:  declare variables for iceberg tracking
   !!======================================================================
   !! History :  3.3  !  2010-01  (T. Martin & A. Adcroft)  Original code
   !!             -   !  2011-03  (G. Madec)  Part conversion to NEMO form
   !!             -   !                       Removal of mapping from another grid
   !!             -   !  2011-04  (S. Alderson) Extensive rewrite ; Split into separate modules
   !!----------------------------------------------------------------------
   !!
   !! Track Icebergs as Lagrangian objects within the model domain
   !! Interaction with the other model variables through 'icebergs_gridded'
   !!
   !! A single iceberg is held as an instance of type 'iceberg'
   !! This type defines a linked list, so each instance contains a pointer 
   !! to the previous and next icebergs in the list
   !!
   !! Type 'icebergs' is a convenience container for all relevant arrays
   !! It contains one pointer to an 'iceberg' instance representing all icebergs in the processor
   !!
   !! Each iceberg has a position represented as a real cartesian coordinate which is 
   !! fractional grid cell, centred on T-points; so an iceberg position of (1.0,1.0) lies 
   !! exactly on the first T-point and the T-cell spans 0.5 to 1.5 in each direction
   !!
   !! Each iceberg is assigned a unique id even in MPI
   !! This consists of an array of integers: the first element is used to label, the second
   !! and subsequent elements are used to count the number of times the first element wraps
   !! around all possible values within the valid size for this datatype.
   !! Labelling is done by starting the first label in each processor (even when only one)
   !! as narea, and then incrementing by jpnij (i.e. the total number of processors.
   !! This means that the source processor for each iceberg can be identified by arithmetic
   !! modulo jpnij.
   !! 
   !!----------------------------------------------------------------------
   USE par_oce   ! ocean parameters
   USE lib_mpp   ! MPP library

   IMPLICIT NONE
   PUBLIC

   PUBLIC   icb_alloc   ! routine called by icb_init in icbini.F90 module

   INTEGER, PUBLIC, PARAMETER ::   nclasses = 10   !: Number of icebergs classes   
   INTEGER, PUBLIC, PARAMETER ::   nkounts  =  3   !: Number of integers combined for unique naming

   TYPE, PUBLIC ::   icebergs_gridded   !: various icebergs properties on model grid
      REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   calving         ! Calving mass rate  (into stored ice)         [kg/s]
      REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   calving_hflx    ! Calving heat flux [heat content of calving]  [W/m2]
      REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   floating_melt   ! Net melting rate to icebergs + bits      [kg/s/m^2]
      INTEGER , DIMENSION(:,:)  , ALLOCATABLE ::   maxclass        ! maximum class number at calving source point
      REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   tmp             ! Temporary work space
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   stored_ice      ! Accumulated ice mass flux at calving locations [kg]
      REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   stored_heat     ! Heat content of stored ice                      [J]
   END TYPE icebergs_gridded

   TYPE, PUBLIC ::   point              !: properties of an individual iceberg (position, mass, size, etc...)
      INTEGER  ::   year
      REAL(wp) ::   xi , yj , zk                                              ! iceberg coordinates in the (i,j) referential (global) and deepest level affected
      REAL(wp) ::   e1 , e2                                                   ! horizontal scale factors at the iceberg position
      REAL(wp) ::   lon, lat, day                                             ! geographic position
      REAL(wp) ::   mass, thickness, width, length, uvel, vvel                ! iceberg physical properties
      REAL(wp) ::   ssu, ssv, ui, vi, ua, va, ssh_x, ssh_y, sst, sss, cn, hi  ! properties of iceberg environment 
      REAL(wp) ::   mass_of_bits, heat_density
      INTEGER  ::   kb                                                   ! icb bottom level
   END TYPE point

   TYPE, PUBLIC ::   iceberg            !: linked list defining all the icebergs present in the model domain
      TYPE(iceberg), POINTER      ::   prev=>NULL(), next=>NULL()   ! pointers to previous and next unique icebergs in linked list
      INTEGER, DIMENSION(nkounts) ::   number                       ! variables which do not change for this iceberg
      REAL(wp)                    ::   mass_scaling                 !    -        -            -                -  
      TYPE(point), POINTER        ::   current_point => NULL()      ! variables which change with time are held in a separate type
   END TYPE iceberg


   TYPE(icebergs_gridded), POINTER ::   berg_grid                 !: master instance of gridded iceberg type
   TYPE(iceberg)         , POINTER ::   first_berg => NULL()      !: master instance of linked list iceberg type

   !                                                             !!! parameters controlling iceberg characteristics and modelling
   REAL(wp)                            ::   berg_dt                   !: Time-step between iceberg CALLs (should make adaptive?)
   REAL(wp), DIMENSION(:), ALLOCATABLE ::   first_width, first_length !: 
   LOGICAL                             ::   l_restarted_bergs=.FALSE.  ! Indicate whether we read state from a restart or not
   !                                                               ! arbitrary numbers for diawri entry
   REAL(wp), DIMENSION(nclasses), PUBLIC ::   class_num=(/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
   !
   REAL(wp), PUBLIC, DIMENSION(:,:), ALLOCATABLE ::   sshdyn_icb, hi_icb
   !
   !!gm almost all those PARAM ARE defined in NEMO
   REAL(wp), PUBLIC, PARAMETER :: pp_rho_ice      = 916.7_wp   !: Density of fresh ice   @ 0oC [kg/m^3]
   REAL(wp), PUBLIC, PARAMETER :: pp_rho_water    = 999.8_wp   !: Density of fresh water @ 0oC [kg/m^3]
   REAL(wp), PUBLIC, PARAMETER :: pp_rho_air      = 1.1_wp     !: Density of air         @ 0oC [kg/m^3]
   REAL(wp), PUBLIC, PARAMETER :: pp_rho_seawater = 1025._wp   !: Approx. density of surface sea water @ 0oC [kg/m^3]
   !!gm end
   REAL(wp), PUBLIC, PARAMETER :: pp_Cd_av = 1.3_wp      !: (Vertical) Drag coefficient between bergs and atmos 
   REAL(wp), PUBLIC, PARAMETER :: pp_Cd_ah = 0.0055_wp   !: (lateral ) Drag coefficient between bergs and atmos 
   REAL(wp), PUBLIC, PARAMETER :: pp_Cd_wv = 0.9_wp      !: (Vertical) Drag coefficient between bergs and ocean
   REAL(wp), PUBLIC, PARAMETER :: pp_Cd_wh = 0.0012_wp   !: (lateral ) Drag coefficient between bergs and ocean 
   REAL(wp), PUBLIC, PARAMETER :: pp_Cd_iv = 0.9_wp      !: (Vertical) Drag coefficient between bergs and sea-ice
!TOM> no horizontal drag for sea ice! real, PARAMETER :: pp_Cd_ih=0.0012 ! (lateral) Drag coeff. between bergs and sea-ice

   !                                                         !!* namberg namelist parameters (and defaults) **
   LOGICAL , PUBLIC ::   ln_bergdia                      !: Calculate budgets
   INTEGER , PUBLIC ::   nn_verbose_level                !: Turn on debugging when level > 0
   INTEGER , PUBLIC ::   nn_test_icebergs                !: Create icebergs in absence of a restart file from the supplied class nb
   REAL(wp), PUBLIC, DIMENSION(4) ::   rn_test_box       !: lon1,lon2,lat1,lat2 box to create them in
   LOGICAL , PUBLIC ::   ln_use_calving                  !: Force use of calving data even with nn_test_icebergs > 0 
                                                         !  (default is not to use calving data with test bergs)
   INTEGER , PUBLIC ::   nn_sample_rate                  !: Timesteps between sampling of position for trajectory storage
   INTEGER , PUBLIC ::   nn_verbose_write                !: timesteps between verbose messages
   REAL(wp), PUBLIC ::   rn_rho_bergs                    !: Density of icebergs
   REAL(wp), PUBLIC ::   rho_berg_1_oce                  !: convertion factor (thickness to draft) (rn_rho_bergs/pp_rho_seawater)
   REAL(wp), PUBLIC ::   rn_LoW_ratio                    !: Initial ratio L/W for newly calved icebergs
   REAL(wp), PUBLIC ::   rn_bits_erosion_fraction        !: Fraction of erosion melt flux to divert to bergy bits
   REAL(wp), PUBLIC ::   rn_sicn_shift                   !: Shift of sea-ice concentration in erosion flux modulation (0<sicn_shift<1)
   LOGICAL , PUBLIC ::   ln_operator_splitting           !: Use first order operator splitting for thermodynamics
   LOGICAL , PUBLIC ::   ln_passive_mode                 !: iceberg - ocean decoupling
   LOGICAL , PUBLIC ::   ln_time_average_weight          !: Time average the weight on the ocean    !!gm I don't understand that !
   REAL(wp), PUBLIC ::   rn_speed_limit                  !: CFL speed limit for a berg
   LOGICAL , PUBLIC ::   ln_M2016, ln_icb_grd            !: use Nacho's Merino 2016 work
   !
   ! restart
   CHARACTER(len=256), PUBLIC :: cn_icbrst_indir , cn_icbrst_in  !:  in: restart directory, restart name
   CHARACTER(len=256), PUBLIC :: cn_icbrst_outdir, cn_icbrst_out !: out: restart directory, restart name
   !
   !                                     ! Mass thresholds between iceberg classes [kg]
   REAL(wp), DIMENSION(nclasses), PUBLIC ::   rn_initial_mass      ! Fraction of calving to apply to this class [non-dim]
   REAL(wp), DIMENSION(nclasses), PUBLIC ::   rn_distribution      ! Ratio between effective and real iceberg mass (non-dim)
   REAL(wp), DIMENSION(nclasses), PUBLIC ::   rn_mass_scaling      ! Total thickness of newly calved bergs [m]
   REAL(wp), DIMENSION(nclasses), PUBLIC ::   rn_initial_thickness !  Single instance of an icebergs type initialised in icebergs_init and updated in icebergs_run
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   src_calving, src_calving_hflx    !: accumulate input ice
   INTEGER , PUBLIC             , SAVE                     ::   micbkb                           !: deepest level affected by icebergs
   INTEGER , PUBLIC             , SAVE                     ::   numicb                           !: iceberg IO
   INTEGER , PUBLIC             , SAVE, DIMENSION(nkounts) ::   num_bergs                        !: iceberg counter
   INTEGER , PUBLIC             , SAVE                     ::   nicbdi, nicbei, nicbdj, nicbej   !: processor bounds
   REAL(wp), PUBLIC             , SAVE                     ::   ricb_left, ricb_right            !: cyclical bounds
   INTEGER , PUBLIC             , SAVE                     ::   nicbpack                         !: packing integer
   INTEGER , PUBLIC             , SAVE                     ::   nktberg, nknberg                 !: helpers
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   nicbfldpts                       !: nfold packed points
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   nicbflddest                      !: nfold destination proc
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   nicbfldproc                      !: nfold destination proc
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   nicbfldnsend                     !: nfold number of bergs to send to nfold neighbour
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   nicbfldexpect                    !: nfold expected number of bergs
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)       ::   nicbfldreq                       !: nfold message handle (immediate send)
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   
   INTEGER FUNCTION icb_alloc()
      !!----------------------------------------------------------------------
      !!                ***  ROUTINE icb_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER ::   ill
      !!----------------------------------------------------------------------
      !
      icb_alloc = 0
      ALLOCATE( berg_grid, STAT=ill )
      icb_alloc = icb_alloc + ill
      ALLOCATE( berg_grid%calving    (jpi,jpj) , berg_grid%calving_hflx (jpi,jpj)          ,   &
         &      berg_grid%stored_heat(jpi,jpj) , berg_grid%floating_melt(jpi,jpj)          ,   &
         &      berg_grid%maxclass   (jpi,jpj) , berg_grid%stored_ice   (jpi,jpj,nclasses) ,   &
         &      berg_grid%tmp        (jpi,jpj) , STAT=ill)
      icb_alloc = icb_alloc + ill
      !
      ALLOCATE( first_width(nclasses) , first_length(nclasses) ,   &
         &      src_calving (jpi,jpj) ,                            &
         &      src_calving_hflx(jpi,jpj) , STAT=ill)
      icb_alloc = icb_alloc + ill
      !
      ALLOCATE( nicbfldpts(jpi) , nicbflddest(jpi) , nicbfldproc(jpni) , &
         &      nicbfldnsend(jpni), nicbfldexpect(jpni) , nicbfldreq(jpni), STAT=ill)
      icb_alloc = icb_alloc + ill
      !
      ALLOCATE( sshdyn_icb(jpi,jpj), hi_icb(jpi,jpj) , STAT=ill)
      icb_alloc = icb_alloc + ill

      !
      CALL mpp_sum ( 'icb_oce', icb_alloc )
      IF( icb_alloc > 0 )   CALL ctl_warn('icb_alloc: allocation of arrays failed')
      !
   END FUNCTION icb_alloc

   !!======================================================================
END MODULE icb_oce
