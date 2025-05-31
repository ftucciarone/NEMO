MODULE obs_group_def
   !!=====================================================================
   !!                       ***  MODULE obs_group_def  ***
   !! Observation diagnostics: Routines for defining and reading observation groups
   !!=====================================================================
   !! History : 4.0  !  2021-06  (D Ford)  Original code
   !!----------------------------------------------------------------------
   !!   obs_group_alloc         : allocate observation group types
   !!   obs_group_dealloc       : deallocate observation group types
   !!   obs_group_read_namelist : read observation group namelists
   !!   obs_group_check         : check observation group options
   !!----------------------------------------------------------------------

   !! * Modules used
   USE iom                ! In/out manager
   USE par_kind, ONLY : & ! Precision variables
      & wp
   USE obs_surf_def       ! Surface data definitions
   USE obs_profiles_def   ! Profile data definitions
   USE tradmp, ONLY : &   ! Damping to climatology
      & ln_tradmp
   USE obs_grid, ONLY : & ! Use global distribution of observations
      & ln_grid_global

   IMPLICIT NONE

   !! * Routine/type accessibility
   PRIVATE

   PUBLIC &
      & obs_group,               &
      & obs_group_alloc,         &
      & obs_group_dealloc,       &
      & obs_group_read_namelist, &
      & obs_group_check

   !! * Shared Module variables
   INTEGER, PARAMETER :: jpmaxntypes = 1000   ! Maximum number of obs types for each obs group
   INTEGER, PARAMETER :: jpmaxnfiles = 1000   ! Maximum number of files for each obs group

   INTEGER, PARAMETER, PUBLIC ::   jpmaxavtypes = 20   !: Max number of daily avgd obs types
   
   ! Expected names for observation types with special behaviours (not needed for all observation types)
   CHARACTER(LEN=8), PUBLIC :: cobsname_uvel = 'UVEL' ! Expected variable name for U velocity (2D or 3D)
   CHARACTER(LEN=8), PUBLIC :: cobsname_vvel = 'VVEL' ! Expected variable name for V velocity (2D or 3D)
   CHARACTER(LEN=8), PUBLIC :: cobsname_sla  = 'SLA'  ! Expected variable name for sea level anomaly
   CHARACTER(LEN=8), PUBLIC :: cobsname_fbd  = 'FBD'  ! Expected variable name for sea ice freeboard
   CHARACTER(LEN=8), PUBLIC :: cobsname_t3d  = 'POTM' ! Expected variable name for 3D temperature
   CHARACTER(LEN=8), PUBLIC :: cobsname_s3d  = 'PSAL' ! Expected variable name for 3D salinity
   CHARACTER(LEN=8), PUBLIC :: cobsname_t2d  = 'SST'  ! Expected variable name for 2D temperature
   CHARACTER(LEN=8), PUBLIC :: cobsname_s2d  = 'SSS'  ! Expected variable name for 2D salinity

   !! * Type definition for observation groups
   TYPE obs_group
      !
      CHARACTER(LEN=25)                             :: cgroupname    !: Name of obs group (for stdout)
      CHARACTER(LEN=8),   DIMENSION(:), ALLOCATABLE :: cobstypes     !: Observation types to read from files
      CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: cobsfiles     !: Observation file names
      CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: cobsbiasfiles !: Bias input file names
      CHARACTER(LEN=128)                            :: cbiasvarname  !: Bias variable name in input file
      CHARACTER(LEN=128)                            :: caltbiasfile  !: Altimeter bias input file name
      CHARACTER(LEN=1)                              :: cgrid         !: Grid type (T,U,V)
      !
      INTEGER,            DIMENSION(:), ALLOCATABLE :: nprofdavtypes !: Profile data types representing a daily average
      !
      INTEGER  :: nobstypes          !: Number of observation types
      INTEGER  :: nobsfiles          !: Number of observation files
      INTEGER  :: nobsbiasfiles      !: Number of bias files
      INTEGER  :: nbiasvar           !: Index of observation type to be bias corrected
      INTEGER  :: navtypes           !: Number of profile data types representing a daily average
      INTEGER  :: nextvars           !: Number of extra variables in addition to any in input files
      INTEGER  :: naddvars           !: Number of additional variables in addition to any in input files
      INTEGER  :: n1dint             !: Type of vertical interpolation method
      INTEGER  :: n2dint             !: Type of horizontal interpolation method
      INTEGER  :: nmsshc             !: MSSH correction scheme
      INTEGER  :: nuvel              !: Index of variable representing U velocity
      INTEGER  :: nvvel              !: Index of variable representing V velocity
      INTEGER  :: nsla               !: Index of variable representing SLA
      INTEGER  :: nfbd               !: Index of variable representing FBD
      INTEGER  :: nadd_ssh           !: Index of additional variable representing SSH
      INTEGER  :: next_mdt           !: Index of extra variable representing MDT
      INTEGER  :: nadd_fbd           !: Index of additional variable representing ice freeboard
      INTEGER  :: next_snow          !: Index of extra variable representing snow thickness
      INTEGER  :: next_rhosw         !: Index of extra variable representing seawater density
      INTEGER  :: nadd_clm           !: Index of additional variable representing climatology
      !
      LOGICAL  :: lenabled           !: Logical switch for group being processed and not ignored
      LOGICAL  :: lsurf              !: Logical switch for surface data
      LOGICAL  :: lprof              !: Logical switch for profile data
      LOGICAL  :: lvel               !: Logical switch for velocity data
      LOGICAL  :: lsla               !: Logical switch for SLA data
      LOGICAL  :: lfbd               !: Logical switch for ice freeboard data
      LOGICAL  :: laltbias           !: Logical switch for altimeter bias correction
      LOGICAL  :: lobsbias           !: Logical switch for bias correction
      LOGICAL  :: lnea               !: Logical switch for rejecting observations near land
      LOGICAL  :: lbound_reject      !: Logical switch for rejecting obs near the boundary
      LOGICAL  :: lignmis            !: Logical switch for ignoring missing files
      LOGICAL  :: lall_at_all        !: Logical switch for computing all model variables at all obs points
      LOGICAL  :: lnight             !: Logical switch for calculating night-time average
      LOGICAL  :: ltime_mean_bkg     !: Logical switch for applying time mean of background (e.g. to remove tidal signal)
      LOGICAL  :: loutput_clim       !: Logical switch for outputting climatological temperature/salinity
      LOGICAL  :: lfp_indegs         !: Logical: T=> averaging footprint is in degrees, F=> in metres
      !
      REAL(wp) :: ravglamscl         !: E/W diameter of observation footprint (metres/degrees)
      REAL(wp) :: ravgphiscl         !: N/S diameter of observation footprint (metres/degrees)
      REAL(wp) :: rmdtcorr           !: MDT correction
      REAL(wp) :: rmdtcutoff         !: MDT cutoff for computed correction
      REAL(wp) :: rtime_mean_period  !: Meaning period if ltime_mean_bkg
      REAL(wp) :: radar_snow_penetr  !: Snow depth penetration factor for radar freeboard conversion to ice thickness 
      !
      REAL(wp), POINTER, DIMENSION(:,:,:)   :: rglam  !: Longitudes
      REAL(wp), POINTER, DIMENSION(:,:,:)   :: rgphi  !: Latitudes
      REAL(wp), POINTER, DIMENSION(:,:,:,:) :: rmask  !: Land/sea masks
      !
      TYPE(obs_surf) :: ssurfdata    !: Initial surface data
      TYPE(obs_surf) :: ssurfdataqc  !: Surface data after quality control
      TYPE(obs_prof) :: sprofdata    !: Initial profile data
      TYPE(obs_prof) :: sprofdataqc  !: Profile data after quality control
      !
   END TYPE

CONTAINS

   SUBROUTINE obs_group_alloc( sdobsgroup )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE obs_group_alloc  ***
      !!
      !! ** Purpose : - Allocate data for observation group types
      !!
      !! ** Method  : - Allocate arrays
      !!
      !! ** Action  : - Allocate arrays
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obs_group), INTENT(INOUT) :: sdobsgroup ! Obs group to be allocated
      !!----------------------------------------------------------------------

      ALLOCATE( sdobsgroup%cobstypes    (sdobsgroup%nobstypes            ), &
         &      sdobsgroup%cobsfiles    (sdobsgroup%nobsfiles            ), &
         &      sdobsgroup%cobsbiasfiles(sdobsgroup%nobsbiasfiles        ), &
         &      sdobsgroup%nprofdavtypes(sdobsgroup%navtypes             ), &
         &      sdobsgroup%rglam        (jpi,jpj,    sdobsgroup%nobstypes), &
         &      sdobsgroup%rgphi        (jpi,jpj,    sdobsgroup%nobstypes), &
         &      sdobsgroup%rmask        (jpi,jpj,jpk,sdobsgroup%nobstypes) )

   END SUBROUTINE obs_group_alloc


   SUBROUTINE obs_group_dealloc( sdobsgroup )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE obs_group_dealloc  ***
      !!
      !! ** Purpose : - Deallocate data for observation group types
      !!
      !! ** Method  : - Deallocate arrays
      !!
      !! ** Action  : - Deallocate arrays
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obs_group), INTENT(INOUT) :: sdobsgroup ! Obs group to be deallocated
      !!----------------------------------------------------------------------

      DEALLOCATE( sdobsgroup%cobstypes,     &
         &        sdobsgroup%cobsfiles,     &
         &        sdobsgroup%cobsbiasfiles, &
         &        sdobsgroup%nprofdavtypes, &
         &        sdobsgroup%rglam,         &
         &        sdobsgroup%rgphi,         &
         &        sdobsgroup%rmask )

   END SUBROUTINE obs_group_dealloc


   SUBROUTINE obs_group_read_namelist( sdobsgroup, kgroup )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE obs_group_read_namelist  ***
      !!
      !! ** Purpose : - Read namelist for observation group types
      !!
      !! ** Method  : - Read namelist
      !!
      !! ** Action  : - Read namelist
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obs_group), INTENT(INOUT) :: sdobsgroup ! Obs group to be populated
      INTEGER,         INTENT(IN   ) :: kgroup     ! Number of group
      !! * Local variables
      INTEGER :: ios                               ! Status for namelist read
      INTEGER :: itype, ifile                      ! Loop counters
      INTEGER :: jtype, jfile                      ! Loop counters
      INTEGER :: jobs_rdstart                      ! Loop counters
      INTEGER :: jobs_loc                          ! Loop counters
      INTEGER :: jobs_count                        ! Loop counters
      CHARACTER(LEN=50) :: cerrmsg                 ! Error string
      !
      CHARACTER(LEN=128)                         :: cn_groupname
      CHARACTER(LEN=8),   DIMENSION(jpmaxntypes) :: cn_obstypes
      CHARACTER(LEN=128), DIMENSION(jpmaxnfiles) :: cn_obsfiles
      CHARACTER(LEN=128), DIMENSION(jpmaxnfiles) :: cn_obsbiasfiles
      CHARACTER(LEN=128)                         :: cn_type_to_biascorrect
      CHARACTER(LEN=128)                         :: cn_obsbiasfile_varname
      CHARACTER(LEN=128)                         :: cn_altbiasfile
      INTEGER,            DIMENSION(jpmaxavtypes) ::   nn_profdavtypes
      INTEGER                                    :: nn_1dint
      INTEGER                                    :: nn_2dint
      INTEGER                                    :: nn_msshc
      LOGICAL                                    :: ln_enabled
      LOGICAL                                    :: ln_surf
      LOGICAL                                    :: ln_prof
      LOGICAL                                    :: ln_altbias
      LOGICAL                                    :: ln_obsbias
      LOGICAL                                    :: ln_nea
      LOGICAL                                    :: ln_bound_reject
      LOGICAL                                    :: ln_ignmis
      LOGICAL                                    :: ln_all_at_all
      LOGICAL                                    :: ln_night
      LOGICAL                                    :: ln_time_mean_bkg
      LOGICAL                                    :: ln_output_clim
      LOGICAL                                    :: ln_fp_indegs
      REAL(wp)                                   :: rn_avglamscl
      REAL(wp)                                   :: rn_avgphiscl
      REAL(wp)                                   :: rn_mdtcorr
      REAL(wp)                                   :: rn_mdtcutoff
      REAL(wp)                                   :: rn_time_mean_period
      REAL(wp)                                   ::   rn_radar_snow_penetr = 1.0_wp
      !!
      NAMELIST/namobs_dta/cn_groupname, ln_prof, ln_surf, ln_enabled,           &
         &                cn_obsfiles, cn_obstypes, ln_nea, ln_bound_reject,    &
         &                ln_ignmis, nn_2dint, nn_1dint, nn_profdavtypes,       &
         &                ln_fp_indegs, rn_avglamscl, rn_avgphiscl, ln_obsbias, &
         &                cn_obsbiasfiles, cn_type_to_biascorrect,              &
         &                cn_obsbiasfile_varname, ln_night, ln_time_mean_bkg,   &
         &                rn_time_mean_period, ln_altbias, cn_altbiasfile,      &
         &                nn_msshc, rn_mdtcorr, rn_mdtcutoff, ln_all_at_all,    &
         &                ln_output_clim, rn_radar_snow_penetr
      !!----------------------------------------------------------------------

      cn_obstypes(:)     = ''
      cn_obsfiles(:)     = ''
      cn_obsbiasfiles(:) = ''
      nn_profdavtypes(:) = -1

      ! Read namobs_dta block in namelist_ref
      READ  ( numnam_ref, namobs_dta, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namobs_dta in reference namelist' )

      ! Read namobs_dta block in namelist_cfg relating to kgroup obsgroup
      ! Do this by finding the kgroup'th occurence of namobs_dta in the
      ! character buffer as the starting point.
      ! This is based on what's done for the BDY namelists
      jobs_rdstart = 1
      DO jobs_count = 1, kgroup
         jobs_loc = INDEX( numnam_cfg( jobs_rdstart: ), 'namobs_dta' )
         IF( jobs_loc .GT. 0 ) THEN
            jobs_rdstart = jobs_rdstart + jobs_loc
         ELSE
            WRITE(cerrmsg,'(A,I4,A)') 'Error: entry number ',kgroup,' of namobs_dta not found'
            ios = -1
            CALL ctl_nam ( ios , cerrmsg )
         ENDIF
      END DO
      jobs_rdstart = MAX( 1, jobs_rdstart - 2 )
      READ  ( numnam_cfg( jobs_rdstart: ), namobs_dta, IOSTAT = ios, ERR = 902)
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namobs_dta in configuration namelist' )
      IF(lwm) WRITE( numond, namobs_dta )
      
      sdobsgroup%cgroupname = cn_groupname
      sdobsgroup%lenabled   = ln_enabled

      IF (sdobsgroup%lenabled) THEN
         sdobsgroup%nobstypes     = 0
         sdobsgroup%nobsfiles     = 0
         sdobsgroup%naddvars      = 0
         sdobsgroup%nextvars      = 0
         sdobsgroup%navtypes      = 0
         sdobsgroup%nobsbiasfiles = 0
         sdobsgroup%lvel          = .false.
         sdobsgroup%lsla          = .false.
         sdobsgroup%lfbd          = .false.
         sdobsgroup%loutput_clim  = .false.
         sdobsgroup%nuvel         = 0
         sdobsgroup%nvvel         = 0
         sdobsgroup%nsla          = 0
         sdobsgroup%nfbd          = 0
         sdobsgroup%nadd_ssh      = 0
         sdobsgroup%next_mdt      = 0
         sdobsgroup%nadd_fbd      = 0
         sdobsgroup%nadd_clm      = 0
         sdobsgroup%next_snow     = 0
         sdobsgroup%next_rhosw    = 0

         DO jtype = 1, jpmaxntypes
            IF ( TRIM(cn_obstypes(jtype)) /= '' ) THEN
               sdobsgroup%nobstypes = sdobsgroup%nobstypes + 1
            ENDIF
         END DO
         DO jfile = 1, jpmaxnfiles
            IF ( TRIM(cn_obsfiles(jfile)) /= '' ) THEN
               sdobsgroup%nobsfiles = sdobsgroup%nobsfiles + 1
            ENDIF
         END DO
         DO jtype = 1, jpmaxavtypes
            IF ( nn_profdavtypes(jtype) /= -1 ) THEN
               sdobsgroup%navtypes = sdobsgroup%navtypes + 1
            ENDIF
         END DO
         DO jfile = 1, jpmaxnfiles
            IF ( TRIM(cn_obsbiasfiles(jfile)) /= '' ) THEN
               sdobsgroup%nobsbiasfiles = sdobsgroup%nobsbiasfiles + 1
            ENDIF
         END DO

         CALL obs_group_alloc( sdobsgroup )

         itype = 0
         DO jtype = 1, jpmaxntypes
            IF ( TRIM(cn_obstypes(jtype)) /= '' ) THEN
               itype = itype + 1
               sdobsgroup%cobstypes(itype) = TRIM(cn_obstypes(jtype))
               IF ( TRIM(sdobsgroup%cobstypes(itype)) == cobsname_uvel ) THEN
                  sdobsgroup%lvel  = .true.
                  sdobsgroup%nuvel = itype
               ELSEIF ( TRIM(sdobsgroup%cobstypes(itype)) == cobsname_vvel ) THEN
                  sdobsgroup%lvel = .true.
                  sdobsgroup%nvvel = itype
               ELSEIF ( TRIM(sdobsgroup%cobstypes(itype)) == cobsname_sla ) THEN
                  sdobsgroup%lsla = .true.
                  sdobsgroup%nsla = itype
                  ! SSH=additional, MDT=extra
                  sdobsgroup%naddvars = sdobsgroup%naddvars + 1
                  sdobsgroup%nextvars = sdobsgroup%nextvars + 1
                  sdobsgroup%nadd_ssh = sdobsgroup%naddvars
                  sdobsgroup%next_mdt = sdobsgroup%nextvars
               ELSEIF ( TRIM(sdobsgroup%cobstypes(itype)) == cobsname_fbd ) THEN
                  sdobsgroup%lfbd = .true.
                  sdobsgroup%nfbd = itype
                  ! freeboard=additional, snow thickness=extra, seawater density=extra
                  ! Freeboard is treated as an additional variable because ultimately we
                  ! want to calculate the ice thickness derived from freeboard measurements
                  ! The extra variables will be used in the conversion from freeboard 
                  ! to thickness
                  sdobsgroup%naddvars  = sdobsgroup%naddvars + 1
                  sdobsgroup%nextvars  = sdobsgroup%nextvars + 1
                  sdobsgroup%nadd_fbd  = sdobsgroup%naddvars
                  sdobsgroup%next_snow = sdobsgroup%nextvars
                  sdobsgroup%nextvars  = sdobsgroup%nextvars + 1
                  sdobsgroup%next_rhosw = sdobsgroup%nextvars
               ENDIF
               !
               IF ( ln_output_clim .AND. ln_tradmp .AND. (.NOT. sdobsgroup%loutput_clim) ) THEN
                  IF ( (TRIM(sdobsgroup%cobstypes(itype)) == cobsname_t3d) .OR. &
                     & (TRIM(sdobsgroup%cobstypes(itype)) == cobsname_s3d) .OR. &
                     & (TRIM(sdobsgroup%cobstypes(itype)) == cobsname_t2d) .OR. &
                     & (TRIM(sdobsgroup%cobstypes(itype)) == cobsname_s2d) ) THEN
                     sdobsgroup%loutput_clim = .TRUE.
                     sdobsgroup%naddvars = sdobsgroup%naddvars + 1
                     sdobsgroup%nadd_clm = sdobsgroup%naddvars
                  ENDIF
               ENDIF
               !
               IF (TRIM(sdobsgroup%cobstypes(itype)) == cobsname_uvel) THEN
                  sdobsgroup%cgrid              = 'U'
                  sdobsgroup%rglam(:,:,itype)   = glamu(:,:)
                  sdobsgroup%rgphi(:,:,itype)   = gphiu(:,:)
                  sdobsgroup%rmask(:,:,:,itype) = umask(:,:,:)
               ELSEIF (TRIM(sdobsgroup%cobstypes(itype)) == cobsname_vvel) THEN
                  sdobsgroup%cgrid              = 'V'
                  sdobsgroup%rglam(:,:,itype)   = glamv(:,:)
                  sdobsgroup%rgphi(:,:,itype)   = gphiv(:,:)
                  sdobsgroup%rmask(:,:,:,itype) = vmask(:,:,:)
               ELSE
                  sdobsgroup%cgrid              = 'T'
                  sdobsgroup%rglam(:,:,itype)   = glamt(:,:)
                  sdobsgroup%rgphi(:,:,itype)   = gphit(:,:)
                  sdobsgroup%rmask(:,:,:,itype) = tmask(:,:,:)
               ENDIF
            ENDIF
         END DO
         ifile = 0
         DO jfile = 1, jpmaxnfiles
            IF ( TRIM(cn_obsfiles(jfile)) /= '' ) THEN
               ifile = ifile + 1
               sdobsgroup%cobsfiles(ifile) = TRIM(cn_obsfiles(jfile))
            ENDIF
         END DO
         itype = 0
         DO jtype = 1, jpmaxavtypes
            IF ( nn_profdavtypes(jtype) /= -1 ) THEN
               itype = itype + 1
               sdobsgroup%nprofdavtypes(itype) = nn_profdavtypes(jtype)
            ENDIF
         END DO
         ifile = 0
         DO jfile = 1, jpmaxnfiles
            IF ( TRIM(cn_obsbiasfiles(jfile)) /= '' ) THEN
               ifile = ifile + 1
               sdobsgroup%cobsbiasfiles(ifile) = cn_obsbiasfiles(jfile)
            ENDIF
         END DO
         IF ( ln_obsbias ) THEN
            sdobsgroup%nbiasvar = -1
            DO jtype = 1, sdobsgroup%nobstypes
               IF ( TRIM(sdobsgroup%cobstypes(itype)) == TRIM(cn_type_to_biascorrect) ) THEN
                  sdobsgroup%nbiasvar = jtype
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         sdobsgroup%caltbiasfile       = cn_altbiasfile
         sdobsgroup%n1dint             = nn_1dint
         sdobsgroup%n2dint             = nn_2dint
         sdobsgroup%nmsshc             = nn_msshc
         sdobsgroup%lsurf              = ln_surf
         sdobsgroup%lprof              = ln_prof
         sdobsgroup%laltbias           = ln_altbias
         sdobsgroup%lobsbias           = ln_obsbias
         sdobsgroup%cbiasvarname       = cn_obsbiasfile_varname
         sdobsgroup%lnea               = ln_nea
         sdobsgroup%lbound_reject      = ln_bound_reject
         sdobsgroup%lignmis            = ln_ignmis
         sdobsgroup%lall_at_all        = ln_all_at_all
         sdobsgroup%lnight             = ln_night
         sdobsgroup%ltime_mean_bkg     = ln_time_mean_bkg
         sdobsgroup%lfp_indegs         = ln_fp_indegs
         sdobsgroup%ravglamscl         = rn_avglamscl
         sdobsgroup%ravgphiscl         = rn_avgphiscl
         sdobsgroup%rmdtcorr           = rn_mdtcorr
         sdobsgroup%rmdtcutoff         = rn_mdtcutoff
         sdobsgroup%rtime_mean_period  = rn_time_mean_period
         sdobsgroup%radar_snow_penetr  = rn_radar_snow_penetr
      ENDIF

   END SUBROUTINE obs_group_read_namelist


   SUBROUTINE obs_group_check( sdobsgroup, kgroup )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE obs_group_check  ***
      !!
      !! ** Purpose : - Error check observation group types
      !!
      !! ** Method  : - Check and print options
      !!
      !! ** Action  : - Check and print options
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obs_group), INTENT(IN) :: sdobsgroup ! Obs group to be checked
      INTEGER,         INTENT(IN) :: kgroup     ! Number of group being checked
      !! * Local variables
      INTEGER :: jtype, jfile                   ! Loop counters
      !!----------------------------------------------------------------------

      IF (lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'obs_group_check : Options for group ', kgroup, ', ', TRIM(sdobsgroup%cgroupname)
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '          Logical switch for group being enabled                  ln_enabled = ', sdobsgroup%lenabled
         IF ( .NOT. sdobsgroup%lenabled ) THEN
            WRITE(numout,*) '             Group disabled, will not be used'
         ELSE
            WRITE(numout,*) '          Observation types in group:', sdobsgroup%nobstypes
            DO jtype = 1, sdobsgroup%nobstypes
               WRITE(numout,*) '             ', TRIM(sdobsgroup%cobstypes(jtype))
            END DO
               WRITE(numout,*) '          Observation files in group:', sdobsgroup%nobsfiles
            DO jfile = 1, sdobsgroup%nobsfiles
               WRITE(numout,*) '             ', TRIM(sdobsgroup%cobsfiles(jfile))
            END DO
            WRITE(numout,*) '          General settings:'
            WRITE(numout,*) '             Logical switch for surface data                         ln_surf = ', sdobsgroup%lsurf
            WRITE(numout,*) '             Logical switch for profile data                         ln_prof = ', sdobsgroup%lprof
            WRITE(numout,*) '             Rejection of observations near land switch               ln_nea = ', sdobsgroup%lnea
            WRITE(numout,*) '             Rejection of obs near open bdys                 ln_bound_reject = ', sdobsgroup%lbound_reject
            WRITE(numout,*) '             Logical switch for ignoring missing files             ln_ignmis = ', sdobsgroup%lignmis
            WRITE(numout,*) '             Type of horizontal interpolation method                nn_2dint = ', sdobsgroup%n2dint
            IF ( sdobsgroup%n2dint <= 4 ) THEN
               WRITE(numout,*) '                model counterparts will be interpolated horizontally'
            ELSE
               WRITE(numout,*) '                model counterparts will be averaged horizontally'
            ENDIF
            WRITE(numout,*) '          Settings only for surface data, which is ', sdobsgroup%lsurf
            WRITE(numout,*) '             Obs footprint in deg [T] or m [F]                  ln_fp_indegs = ', sdobsgroup%lfp_indegs
            WRITE(numout,*) '             E/W diameter of obs footprint                      rn_avglamscl = ', sdobsgroup%ravglamscl
            WRITE(numout,*) '             N/S diameter of obs footprint                      rn_avgphiscl = ', sdobsgroup%ravgphiscl
            WRITE(numout,*) '             Logical switch for night-time average                  ln_night = ', sdobsgroup%lnight
            WRITE(numout,*) '             Logical switch for time-mean background        ln_time_mean_bkg = ', sdobsgroup%ltime_mean_bkg
            WRITE(numout,*) '             Meaning period (hours) for time-mean bkg    rn_time_mean_period = ', sdobsgroup%rtime_mean_period
            WRITE(numout,*) '             Logical switch for bias correction                   ln_obsbias = ', sdobsgroup%lobsbias
            IF ( sdobsgroup%lobsbias ) THEN
               WRITE(numout,*) '             Observation type to be bias corrected    cn_type_to_biascorrect = ', TRIM(sdobsgroup%cobstypes(sdobsgroup%nbiasvar))
               WRITE(numout,*) '             Bias variable name in bias files         cn_obsbiasfile_varname = ', TRIM(sdobsgroup%cbiasvarname)
               WRITE(numout,*) '             Bias files in group:', sdobsgroup%nobsbiasfiles
               DO jfile = 1, sdobsgroup%nobsbiasfiles
                  WRITE(numout,*) '                ', TRIM(sdobsgroup%cobsbiasfiles(jfile))
               END DO
            ENDIF
            WRITE(numout,*) '          Settings only for profile data, which is ', sdobsgroup%lprof
            WRITE(numout,*) '             Type of vertical interpolation method                  nn_1dint = ', sdobsgroup%n1dint
            WRITE(numout,*) '             Number of daily average types                                   = ', sdobsgroup%navtypes
            IF ( sdobsgroup%navtypes > 0 ) THEN
               WRITE(numout,*) '             Daily average types                             nn_profdavtypes = ', sdobsgroup%nprofdavtypes
            ENDIF
            WRITE(numout,*) '             Logical switch to compute all vars at all pts     ln_all_at_all = ', sdobsgroup%lall_at_all
            WRITE(numout,*) '          Settings only for SLA data, which is ', sdobsgroup%lsla
            WRITE(numout,*) '             Logical switch for alt bias                          ln_altbias = ', sdobsgroup%laltbias
            WRITE(numout,*) '             Alt bias file name                               cn_altbiasfile = ', TRIM(sdobsgroup%caltbiasfile)
            WRITE(numout,*) '             MSSH correction scheme                                 nn_msshc = ', sdobsgroup%nmsshc
            WRITE(numout,*) '             MDT  correction                                      rn_mdtcorr = ', sdobsgroup%rmdtcorr
            WRITE(numout,*) '             MDT cutoff for computed correction                 rn_mdtcutoff = ', sdobsgroup%rmdtcutoff
            WRITE(numout,*) '          Settings only for temperature/salinity data'
            WRITE(numout,*) '             Logical switch for outputting climatology        ln_output_clim = ', sdobsgroup%loutput_clim

            IF ( (       sdobsgroup%lsurf .AND.       sdobsgroup%lprof ) .OR. &
               & ( .NOT. sdobsgroup%lsurf .AND. .NOT. sdobsgroup%lprof ) ) THEN
               CALL ctl_stop( ' One and only one of ln_surf or ln_prof must be set for observation group', &
                  &           TRIM(sdobsgroup%cgroupname) )
            ENDIF

            IF ( sdobsgroup%nobstypes == 0 ) THEN
               CALL ctl_stop( ' No observation types specified for observation group', &
                  &           TRIM(sdobsgroup%cgroupname) )
            ENDIF

            IF ( sdobsgroup%nobsfiles == 0 ) THEN
               CALL ctl_stop( ' No observation files specified for observation group', &
                  &           TRIM(sdobsgroup%cgroupname) )
            ENDIF

            IF ( (sdobsgroup%lobsbias) .AND. (sdobsgroup%nobsbiasfiles == 0) ) THEN
               CALL ctl_stop( ' No bias files specified for observation group', &
                  &           TRIM(sdobsgroup%cgroupname) )
            ENDIF

            IF ( (sdobsgroup%n2dint < 0) .OR. (sdobsgroup%n2dint > 6) ) THEN
               CALL ctl_stop( ' Invalid horizontal interpolation type for observation group', &
                  &           TRIM(sdobsgroup%cgroupname) )
            ENDIF

            IF( (sdobsgroup%n1dint < 0) .OR. (sdobsgroup%n1dint > 1) ) THEN
               CALL ctl_stop(' Invalid vertical interpolation type for observation group', &
                  &           TRIM(sdobsgroup%cgroupname) )
            ENDIF

            IF ( (sdobsgroup%n2dint > 4) .AND. (sdobsgroup%n2dint <= 6) ) THEN
               IF ( sdobsgroup%ravglamscl <= 0._wp ) THEN
                  CALL ctl_stop( ' Incorrect value set for averaging footprint scale rn_avglamscl for observation group', &
                     &           TRIM(sdobsgroup%cgroupname) )
               ENDIF
               IF ( sdobsgroup%ravgphiscl <= 0._wp ) THEN
                  CALL ctl_stop( ' Incorrect value set for averaging footprint scale rn_avgphiscl for observation group', &
                     &           TRIM(sdobsgroup%cgroupname) )
               ENDIF
            ENDIF

            IF ( sdobsgroup%lsla .AND. sdobsgroup%laltbias ) THEN
               IF ( TRIM(sdobsgroup%caltbiasfile) == '' ) THEN
                  CALL ctl_stop( ' No altimeter bias file specified for observation group', &
                     &           TRIM(sdobsgroup%cgroupname) )
               ENDIF
            ENDIF

            IF( sdobsgroup%lvel .AND. (.NOT. ln_grid_global) ) THEN
               CALL ctl_stop( ' Velocity data only works with ln_grid_global=.true., observation group', &
                  &           TRIM(sdobsgroup%cgroupname) )
            ENDIF

         ENDIF

      ENDIF

   END SUBROUTINE obs_group_check

END MODULE obs_group_def
