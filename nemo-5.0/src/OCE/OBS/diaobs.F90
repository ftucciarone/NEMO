MODULE diaobs
   !!======================================================================
   !!                       ***  MODULE diaobs  ***
   !! Observation diagnostics: Computation of the misfit between data and
   !!                          their model equivalent 
   !!======================================================================
   !! History :  1.0  !  2006-03  (K. Mogensen) Original code
   !!             -   !  2006-05  (K. Mogensen, A. Weaver) Reformatted
   !!             -   !  2006-10  (A. Weaver) Cleaning and add controls
   !!             -   !  2007-03  (K. Mogensen) General handling of profiles
   !!             -   !  2007-04  (G. Smith) Generalized surface operators
   !!            2.0  !  2008-10  (M. Valdivieso) obs operator for velocity profiles
   !!            3.4  !  2014-08  (J. While) observation operator for profiles in all vertical coordinates
   !!             -   !                      Incorporated SST bias correction  
   !!            3.6  !  2015-02  (M. Martin) Simplification of namelist and code
   !!             -   !  2015-08  (M. Martin) Combined surface/profile routines.
   !!            4.0  !  2017-11  (G. Madec) style only
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dia_obs_init  : Reading and prepare observations
   !!   dia_obs       : Compute model equivalent to observations
   !!   dia_obs_wri   : Write observational diagnostics
   !!   calc_date     : Compute the date of timestep in YYYYMMDD.HHMMSS format
   !!   ini_date      : Compute the initial date YYYYMMDD.HHMMSS
   !!   fin_date      : Compute the final date YYYYMMDD.HHMMSS
   !!----------------------------------------------------------------------
   USE par_kind                ! Precision variables
   USE in_out_manager          ! I/O manager
   USE timing                  ! Timing
   USE par_oce                 ! Ocean parameters
   USE dom_oce                 ! Ocean space and time domain variables
   USE sbc_oce                 ! Sea-ice fraction
   !
   USE obs_read_prof           ! Reading and allocation of profile obs
   USE obs_read_surf           ! Reading and allocation of surface obs
   USE obs_bias                ! Bias correction routine
   USE obs_readmdt             ! Reading and allocation of MDT for SLA.
   USE obs_read_mod_fbd_conver ! Get model variables for conversion of freeboard to ice thickness
   USE obs_prep                ! Preparation of obs. (grid search etc).
   USE obs_oper                ! Observation operators
   USE obs_write               ! Writing of observation related diagnostics
   USE obs_grid                ! Grid searching
   USE obs_profiles_def        ! Profile data definitions
   USE obs_surf_def            ! Surface data definitions
   USE obs_types               ! Definitions for observation types
   USE obs_group_def           ! Definitions for observation groups
   USE obs_mpp                 ! Obs MPP operations
   !
   USE mpp_map                 ! MPP mapping
   USE lib_mpp                 ! For ctl_warn/stop

   IMPLICIT NONE
   PRIVATE

   PUBLIC dia_obs_init     ! Initialize and read observations
   PUBLIC dia_obs          ! Compute model equivalent to observations
   PUBLIC dia_obs_wri      ! Write model equivalent to observations
   PUBLIC calc_date        ! Compute the date of a timestep

   LOGICAL, PUBLIC :: ln_diaobs            !: Logical switch for the obs operator

   INTEGER :: nn_obsgroups

   TYPE(obs_group), DIMENSION(:), ALLOCATABLE ::   sobsgroups   ! Obs groups

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dia_obs_init( Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_obs_init  ***
      !!          
      !! ** Purpose : Initialize and read observations
      !!
      !! ** Method  : Read the namelist and call reading routines
      !!
      !! ** Action  : Read the namelist and call reading routines
      !!
      !!----------------------------------------------------------------------
#if defined key_si3
      USE ice, ONLY : &     ! Sea ice variables
         & vt_s             ! Snow depth for freeboard conversion
#elif defined key_cice
      USE sbc_oce, ONLY : & ! Sea ice variables
         & thick_s          ! Snow depth for freeboard conversion
#endif
      USE oce, ONLY : &     ! Ocean variables
         & rhop             ! Sea water density for freeboard conversion
      USE obs_fbm, ONLY : &
         & fbrmdi           ! Real missing data indicator

      IMPLICIT NONE

      INTEGER, INTENT(in) :: Kmm                  ! ocean time level indices
      !
      INTEGER, PARAMETER  :: jpmaxnfiles = 1000   ! Maximum number of files for each obs type
      INTEGER             :: ios                  ! Local integer output status for namelist read
      INTEGER             :: jtype                ! Counter for obs types
      INTEGER             :: jvar                 ! Counter for variables
      INTEGER             :: jfile                ! Counter for files
      INTEGER             :: jgroup               ! Counter for obs groups
      INTEGER             :: jenabled             ! Counter for enabled obs groups
      !
      REAL(dp)            :: dn_dobsini           ! Obs   window start date YYYYMMDD.HHMMSS
      REAL(dp)            :: dn_dobsend           ! Obs   window end   date YYYYMMDD.HHMMSS
      REAL(dp)            :: dldobsini            ! Model window start date YYYYMMDD.HHMMSS
      REAL(dp)            :: dldobsend            ! Model window end   date YYYYMMDD.HHMMSS
      !
      LOGICAL, DIMENSION(:), ALLOCATABLE :: llvar ! Switches for observed variables
      !!
      NAMELIST/namobs/ln_diaobs, nn_obsgroups,               &
         &            ln_grid_global, ln_grid_search_lookup, &
         &            cn_gridsearchfile, rn_gridsearchres,   &
         &            dn_dobsini, dn_dobsend
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Read namelist parameters
      !-----------------------------------------------------------------------

      ! Read namelist namobs : control observation diagnostics
      READ_NML_REF(numnam,namobs)
      READ_NML_CFG(numnam,namobs)
      IF(lwm) WRITE ( numond, namobs )

      IF( .NOT.ln_diaobs ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dia_obs_init : NO Observation diagnostic used'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
         RETURN
      ENDIF

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_obs_init : Observation diagnostic initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namobs : set observation diagnostic parameters'
         WRITE(numout,*) '      Number of namobs_dta namelists to read             nn_obsgroups = ', nn_obsgroups
         WRITE(numout,*) '      Global distribution of observations              ln_grid_global = ', ln_grid_global
         WRITE(numout,*) '      Logical switch for obs grid search lookup ln_grid_search_lookup = ', ln_grid_search_lookup
         WRITE(numout,*) '      Initial date in window                               dn_dobsini = ', dn_dobsini
         WRITE(numout,*) '      Final date in window                                 dn_dobsend = ', dn_dobsend
         IF (ln_grid_search_lookup) THEN
            WRITE(numout,*) '      Grid search lookup file header                cn_gridsearchfile = ', cn_gridsearchfile
            WRITE(numout,*) '      Grid search resolution                         rn_gridsearchres = ', rn_gridsearchres
         ENDIF
      ENDIF

      ! Check obs window within run window
      !
      CALL calc_date( nit000 - 1, dldobsini )
      CALL calc_date( nitend    , dldobsend )
      !
      IF ( ( dn_dobsini > dldobsend ) .OR. ( dn_dobsend < dldobsini ) .OR. ( dn_dobsini >= dn_dobsend ) ) THEN
          CALL ctl_warn( ' dia_obs_init: period dn_dobsini to dn_dobsend outside model run period',   &
             &           ' so turning off calls to dia_obs'  )
          ln_diaobs = .FALSE.
          RETURN
      ENDIF

      IF( ln_grid_global ) THEN
         IF( jpnij < jpni * jpnj ) THEN
            CALL ctl_stop( 'STOP', 'dia_obs_init: ln_grid_global=T is not available when land subdomains are suppressed' )
         END IF
         CALL ctl_warn( 'dia_obs_init: ln_grid_global=T may cause memory issues when used with a large number of processors' )
      ENDIF

      !-----------------------------------------------------------------------
      ! Read namobs_dta namelists and set up observation groups
      !-----------------------------------------------------------------------

      IF( nn_obsgroups == 0 ) THEN
         CALL ctl_warn( 'dia_obs_init: ln_diaobs is set to true, but nn_obsgroups == 0',   &
            &           ' so turning off calls to dia_obs'  )
         ln_diaobs = .FALSE.
         RETURN
      ENDIF

      ALLOCATE( sobsgroups(nn_obsgroups) )

      jenabled = 0
      DO jgroup = 1, nn_obsgroups
         CALL obs_group_read_namelist( sobsgroups(jgroup), jgroup )
         CALL obs_group_check( sobsgroups(jgroup), jgroup )
         IF (sobsgroups(jgroup)%lenabled) THEN
            jenabled = jenabled + 1
         ENDIF
      END DO

      IF( jenabled == 0 ) THEN
         CALL ctl_warn( 'dia_obs_init: ln_diaobs is set to true, and nn_obsgroups > 0',   &
            &           ' but no groups are enabled so turning off calls to dia_obs'  )
         ln_diaobs = .FALSE.
         RETURN
      ENDIF

      !-----------------------------------------------------------------------
      ! Open obs.stat file if requested
      !-----------------------------------------------------------------------
      !
      IF( lwm .AND. sn_cfctl%l_obsstat ) THEN
         CALL ctl_opn( numobsstat, 'obs.stat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp , narea )
      ENDIF

      !-----------------------------------------------------------------------
      ! Obs operator parameter checking and initialisations
      !-----------------------------------------------------------------------
      !
      CALL obs_typ_init
      IF( ln_grid_global )   CALL mppmap_init
      !
      CALL obs_grid_setup( )

      !-----------------------------------------------------------------------
      ! Depending on switches read the various observation types
      !-----------------------------------------------------------------------
      !
      DO jgroup = 1, nn_obsgroups
         IF ( sobsgroups(jgroup)%lenabled ) THEN
            IF ( sobsgroups(jgroup)%lprof ) THEN
               !
               ! Read in profile or profile obs types
               !
               ALLOCATE( llvar(sobsgroups(jgroup)%nobstypes) )
               llvar(:) = .TRUE.
               !
               CALL obs_rea_prof( sobsgroups(jgroup)%sprofdata,   &
                  &               sobsgroups(jgroup)%nobsfiles,   &
                  &               sobsgroups(jgroup)%cobsfiles,   &
                  &               sobsgroups(jgroup)%nobstypes,   &
                  &               sobsgroups(jgroup)%naddvars,    &
                  &               sobsgroups(jgroup)%nextvars,    &
                  &               nitend-nit000+2,                &
                  &               dn_dobsini,                     &
                  &               dn_dobsend,                     &
                  &               llvar,                          &
                  &               sobsgroups(jgroup)%lignmis,     &
                  &               sobsgroups(jgroup)%lall_at_all, &
                  &               .FALSE.,                        &
                  &               sobsgroups(jgroup)%cobstypes,   &
                  &               sobsgroups(jgroup)%navtypes,    &
                  &               kdailyavtypes = sobsgroups(jgroup)%nprofdavtypes )
               !
               DO jvar = 1, sobsgroups(jgroup)%nobstypes
                  CALL obs_prof_staend( sobsgroups(jgroup)%sprofdata, jvar )
               END DO
               !
               IF ( sobsgroups(jgroup)%sprofdata%next > 0 ) THEN
                  CALL obs_prof_staend_ext( sobsgroups(jgroup)%sprofdata )
               ENDIF
               !
               IF( sobsgroups(jgroup)%loutput_clim ) THEN
                  sobsgroups(jgroup)%sprofdata%caddvars(sobsgroups(jgroup)%nadd_clm)  = 'CLM'
                  DO jvar = 1, sobsgroups(jgroup)%nobstypes
                     sobsgroups(jgroup)%sprofdata%var(jvar)%vadd(:,sobsgroups(jgroup)%nadd_clm) = fbrmdi
                     sobsgroups(jgroup)%sprofdata%caddlong(sobsgroups(jgroup)%nadd_clm,jvar) = 'Climatology'
                     sobsgroups(jgroup)%sprofdata%caddunit(sobsgroups(jgroup)%nadd_clm,jvar) = sobsgroups(jgroup)%sprofdata%cunit(jvar)
                  END DO
               ENDIF
               !
               sobsgroups(jgroup)%sprofdata%cgrid = sobsgroups(jgroup)%cgrid
               !
               CALL obs_pre_prof( sobsgroups(jgroup)%sprofdata,     &
                  &               sobsgroups(jgroup)%sprofdataqc,   &
                  &               llvar,                            &
                  &               jpi, jpj, jpk,                    &
                  &               sobsgroups(jgroup)%rmask,         &
                  &               sobsgroups(jgroup)%rglam,         &
                  &               sobsgroups(jgroup)%rgphi,         &
                  &               sobsgroups(jgroup)%lnea,          &
                  &               sobsgroups(jgroup)%lbound_reject, &
                  &               Kmm,                              &
                  &               sobsgroups(jgroup)%navtypes,      &
                  &               kdailyavtypes = sobsgroups(jgroup)%nprofdavtypes )
               !
               DEALLOCATE( llvar )
               !
            ELSEIF (sobsgroups(jgroup)%lsurf) THEN
               !
               ! Read in surface obs types
               !
               CALL obs_rea_surf( sobsgroups(jgroup)%ssurfdata,         &
                  &               sobsgroups(jgroup)%nobsfiles,         &
                  &               sobsgroups(jgroup)%cobsfiles,         &
                  &               sobsgroups(jgroup)%nobstypes,         &
                  &               sobsgroups(jgroup)%naddvars,          &
                  &               sobsgroups(jgroup)%nextvars,          &
                  &               nitend-nit000+2,                      &
                  &               dn_dobsini,                           &
                  &               dn_dobsend,                           &
                  &               sobsgroups(jgroup)%rtime_mean_period, &
                  &               sobsgroups(jgroup)%ltime_mean_bkg,    &
                  &               sobsgroups(jgroup)%lignmis,           &
                  &               .FALSE.,                              &
                  &               sobsgroups(jgroup)%lnight,            &
                  &               sobsgroups(jgroup)%cobstypes )
               !
               ! Define extra/additional variables where required
               !
               IF( sobsgroups(jgroup)%lsla ) THEN
                  sobsgroups(jgroup)%ssurfdata%cextvars(sobsgroups(jgroup)%next_mdt) = 'MDT'
                  sobsgroups(jgroup)%ssurfdata%cextlong(sobsgroups(jgroup)%next_mdt) = 'Mean dynamic topography'
                  sobsgroups(jgroup)%ssurfdata%cextunit(sobsgroups(jgroup)%next_mdt) = 'Metres'
                  sobsgroups(jgroup)%ssurfdata%caddvars(sobsgroups(jgroup)%nadd_ssh) = 'SSH'
                  DO jvar = 1, sobsgroups(jgroup)%nobstypes
                     sobsgroups(jgroup)%ssurfdata%caddlong(sobsgroups(jgroup)%nadd_ssh,jvar) = 'Model Sea surface height'
                     sobsgroups(jgroup)%ssurfdata%caddunit(sobsgroups(jgroup)%nadd_ssh,jvar) = 'Metres'
                  END DO
               ENDIF
               !
               IF( sobsgroups(jgroup)%lfbd ) THEN
                  sobsgroups(jgroup)%ssurfdata%cextvars(sobsgroups(jgroup)%next_snow)  = 'SNOW'
                  sobsgroups(jgroup)%ssurfdata%cextlong(sobsgroups(jgroup)%next_snow)  = 'Snow thickness'
                  sobsgroups(jgroup)%ssurfdata%cextunit(sobsgroups(jgroup)%next_snow)  = 'Metres'
                  sobsgroups(jgroup)%ssurfdata%cextvars(sobsgroups(jgroup)%next_rhosw) = 'OC_DENS'
                  sobsgroups(jgroup)%ssurfdata%cextlong(sobsgroups(jgroup)%next_rhosw) = 'Seawater density'
                  sobsgroups(jgroup)%ssurfdata%cextunit(sobsgroups(jgroup)%next_rhosw) = 'kg/m3'
                  sobsgroups(jgroup)%ssurfdata%caddvars(sobsgroups(jgroup)%nadd_fbd)   = 'FBD'
                  DO jvar = 1, sobsgroups(jgroup)%nobstypes
                     sobsgroups(jgroup)%ssurfdata%caddlong(sobsgroups(jgroup)%nadd_fbd,jvar) = 'Freeboard'
                     sobsgroups(jgroup)%ssurfdata%caddunit(sobsgroups(jgroup)%nadd_fbd,jvar) = 'Metres'
                  END DO
               ENDIF
               !
               IF( sobsgroups(jgroup)%loutput_clim ) THEN
                  sobsgroups(jgroup)%ssurfdata%caddvars(sobsgroups(jgroup)%nadd_clm)  = 'CLM'
                  DO jvar = 1, sobsgroups(jgroup)%nobstypes
                     sobsgroups(jgroup)%ssurfdata%radd(:,:,jvar) = fbrmdi
                     sobsgroups(jgroup)%ssurfdata%caddlong(sobsgroups(jgroup)%nadd_clm,jvar) = 'Climatology'
                     sobsgroups(jgroup)%ssurfdata%caddunit(sobsgroups(jgroup)%nadd_clm,jvar) = sobsgroups(jgroup)%ssurfdata%cunit(jvar)
                  END DO
               ENDIF
               !
               sobsgroups(jgroup)%ssurfdata%cgrid = sobsgroups(jgroup)%cgrid
               !
               CALL obs_pre_surf( sobsgroups(jgroup)%ssurfdata,      &
                  &               sobsgroups(jgroup)%ssurfdataqc,    &
                  &               jpi, jpj,                          &
                  &               sobsgroups(jgroup)%rmask(:,:,1,:), &
                  &               sobsgroups(jgroup)%rglam,          &
                  &               sobsgroups(jgroup)%rgphi,          &
                  &               sobsgroups(jgroup)%lnea,           &
                  &               sobsgroups(jgroup)%lbound_reject )
               !
               IF( sobsgroups(jgroup)%lsla ) THEN
                  CALL obs_rea_mdt( sobsgroups(jgroup)%ssurfdataqc, &
                     &              sobsgroups(jgroup)%n2dint,      &
                     &              Kmm,                            &
                     &              sobsgroups(jgroup)%nsla,        &
                     &              sobsgroups(jgroup)%next_mdt,    &
                     &              sobsgroups(jgroup)%nmsshc,      &
                     &              sobsgroups(jgroup)%rmdtcorr,    &
                     &              sobsgroups(jgroup)%rmdtcutoff )
                  IF( sobsgroups(jgroup)%laltbias ) THEN
                     CALL obs_app_bias( sobsgroups(jgroup)%ssurfdataqc,   &
                        &               sobsgroups(jgroup)%next_mdt,      &
                        &               sobsgroups(jgroup)%n2dint,        &
                        &               1,                                &
                        &               sobsgroups(jgroup)%caltbiasfile,  &
                        &               'altbias',                        &
                        &               ld_extvar=.TRUE. )
                  ENDIF
               ENDIF
               !
               IF( sobsgroups(jgroup)%lfbd ) THEN
#if defined key_si3
                  CALL obs_rea_snowdepth( sobsgroups(jgroup)%ssurfdataqc, &
                     &                    sobsgroups(jgroup)%n2dint,      &
                     &                    sobsgroups(jgroup)%nfbd,        &
                     &                    sobsgroups(jgroup)%next_snow,   &
                     &                    vt_s(:,:) )
#elif defined key_cice
                  CALL obs_rea_snowdepth( sobsgroups(jgroup)%ssurfdataqc, &
                     &                    sobsgroups(jgroup)%n2dint,      &
                     &                    sobsgroups(jgroup)%nfbd,        &
                     &                    sobsgroups(jgroup)%next_snow,   &
                     &                    thick_s(:,:) )
#endif
                  CALL obs_rea_rho_seawater( sobsgroups(jgroup)%ssurfdataqc, &
                     &                       sobsgroups(jgroup)%n2dint,      &
                     &                       sobsgroups(jgroup)%nfbd,        &
                     &                       sobsgroups(jgroup)%next_rhosw,  &
                     &                       rhop(:,:,1) )
               ENDIF
               !
               IF( sobsgroups(jgroup)%lobsbias ) THEN
                  CALL obs_app_bias( sobsgroups(jgroup)%ssurfdataqc,   &
                     &               sobsgroups(jgroup)%nbiasvar,      &
                     &               sobsgroups(jgroup)%n2dint,        &
                     &               sobsgroups(jgroup)%nobsbiasfiles, &
                     &               sobsgroups(jgroup)%cobsbiasfiles, &
                     &               sobsgroups(jgroup)%cbiasvarname )
               ENDIF
               !
            ENDIF
         ENDIF
      END DO
      !
   END SUBROUTINE dia_obs_init


   SUBROUTINE dia_obs( kstp, Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_obs  ***
      !!          
      !! ** Purpose : Call the observation operators on each time step
      !!
      !! ** Method  : Call the observation operators on each time step to
      !!              compute the model equivalent of the following data:
      !!               - Profile data, currently T/S or U/V
      !!               - Surface data, currently SST, SLA or sea-ice concentration.
      !!
      !! ** Action  :
      !!----------------------------------------------------------------------
      USE oce    , ONLY : ts, uu, vv, ssh     ! Ocean dynamics and tracers variables (Kmm time-level only)
      USE phycst , ONLY : rday                ! Physical constants
#if defined key_si3
      USE ice    , ONLY : at_i, vt_i          ! SI3 Ice model variables
#elif defined key_cice
      USE sbc_oce, ONLY : fr_i, thick_i       ! CICE Ice model variables
#endif
      USE tradmp,  ONLY : tclim, sclim        ! T&S climatology
      USE obs_fbm, ONLY : fbrmdi              ! Real missing data indicator

      IMPLICIT NONE

      !! * Arguments
      INTEGER, INTENT(IN) :: kstp  ! Current timestep
      INTEGER, INTENT(in) :: Kmm   ! ocean time level indices
      !! * Local declarations
      INTEGER :: idaystp           ! Number of timesteps per day
      INTEGER :: imeanstp          ! Number of timesteps for time averaging
      INTEGER :: jtype             ! Loop counters
      INTEGER :: jvar              ! ..
      INTEGER :: jgroup            ! ..
      INTEGER :: ji, jj, jk, jobs  ! ..
      INTEGER :: jo                ! ..
      INTEGER :: inumgoodobs       ! Temporary variables for obs.stat calculation
      INTEGER :: inumgoodobsmpp    ! ..
      REAL(wp) :: zsumx            ! ..
      REAL(wp) :: zsumx2           ! ..
      REAL(wp) :: zomb             ! ..
      LOGICAL :: lstp0             ! Flag special treatment on zeroth time step
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zprofvar, &             ! Model values for variables in a prof ob
         & zprofclim               ! Climatology values for variables in a prof ob
      REAL(wp), DIMENSION(:,:), ALLOCATABLE :: &
         & zsurfvar, &             ! Model values for variables in a surf ob
         & zsurfclim               ! Climatology values for variables in a surf ob
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: zdept, zdepw

      !-----------------------------------------------------------------------

      IF( ln_timing )   CALL timing_start('dia_obs')

      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_obs : Call the observation operators', kstp
         WRITE(numout,*) '~~~~~~~'
      ENDIF

      idaystp = NINT( rday / rn_Dt )

      !-----------------------------------------------------------------------
      ! Call the profile and surface observation operators
      !-----------------------------------------------------------------------

      ALLOCATE( zprofvar(jpi,jpj,jpk),  &
         &      zprofclim(jpi,jpj,jpk), &
         &      zsurfvar(jpi,jpj),      &
         &      zsurfclim(jpi,jpj),     &
         &      zdept(jpi,jpj,jpk),     &
         &      zdepw(jpi,jpj,jpk) )

      DO jk = 1, jpk
         zdept(:,:,jk) = gdept(:,:,jk,Kmm)
         zdepw(:,:,jk) = gdepw(:,:,jk,Kmm)
      END DO

      DO jgroup = 1, nn_obsgroups
         IF ( sobsgroups(jgroup)%lenabled ) THEN

            IF ( sobsgroups(jgroup)%lprof ) THEN

               zprofclim(:,:,:) = fbrmdi

               DO jvar = 1, sobsgroups(jgroup)%nobstypes

                  SELECT CASE ( TRIM(sobsgroups(jgroup)%cobstypes(jvar)) )
                  CASE('POTM')
                     zprofvar(:,:,:) = ts(:,:,:,jp_tem,Kmm)
                     IF (sobsgroups(jgroup)%loutput_clim) THEN
                        zprofclim(:,:,:) = tclim(:,:,:)
                     ENDIF
                  CASE('PSAL')
                     zprofvar(:,:,:) = ts(:,:,:,jp_sal,Kmm)
                     IF (sobsgroups(jgroup)%loutput_clim) THEN
                        zprofclim(:,:,:) = sclim(:,:,:)
                     ENDIF
                  CASE('UVEL')
                     zprofvar(:,:,:) = uu(:,:,:,Kmm)
                  CASE('VVEL')
                     zprofvar(:,:,:) = vv(:,:,:,Kmm)
                  CASE DEFAULT
                     CALL ctl_stop( 'Unknown profile observation type '//TRIM(sobsgroups(jgroup)%cobstypes(jvar))//' in dia_obs' )
                  END SELECT

                  CALL obs_prof_opt( sobsgroups(jgroup)%sprofdataqc,       &
                     &               kstp, jpi, jpj, jpk,                  &
                     &               nit000, idaystp, jvar,                &
                     &               zprofvar,                             &
                     &               sobsgroups(jgroup)%loutput_clim,      &
                     &               sobsgroups(jgroup)%nadd_clm,          &
                     &               zprofclim,                            &
                     &               zdept(:,:,:),                         &
                     &               zdepw(:,:,:),                         &
                     &               sobsgroups(jgroup)%rmask(:,:,:,jvar), &
                     &               sobsgroups(jgroup)%rglam(:,:,jvar),   &
                     &               sobsgroups(jgroup)%rgphi(:,:,jvar),   &
                     &               sobsgroups(jgroup)%n1dint,            &
                     &               sobsgroups(jgroup)%n2dint,            &
                     &               sobsgroups(jgroup)%navtypes,          &
                     &               kdailyavtypes = sobsgroups(jgroup)%nprofdavtypes )

               END DO

            ELSEIF (sobsgroups(jgroup)%lsurf) THEN

               zsurfclim(:,:) = fbrmdi

               DO jvar = 1, sobsgroups(jgroup)%nobstypes

                  lstp0 = .FALSE.
                  SELECT CASE ( TRIM(sobsgroups(jgroup)%cobstypes(jvar)) )
                  CASE('SST')
                     zsurfvar(:,:) = ts(:,:,1,jp_tem,Kmm)
                     IF (sobsgroups(jgroup)%loutput_clim) THEN
                        zsurfclim(:,:) = tclim(:,:,1)
                     ENDIF
                  CASE('SLA')
                     zsurfvar(:,:) = ssh(:,:,Kmm)
                  CASE('SSS')
                     zsurfvar(:,:) = ts(:,:,1,jp_sal,Kmm)
                     IF (sobsgroups(jgroup)%loutput_clim) THEN
                        zsurfclim(:,:) = sclim(:,:,1)
                     ENDIF
                  CASE('UVEL')
                     zsurfvar(:,:) = uu(:,:,1,Kmm)
                  CASE('VVEL')
                     zsurfvar(:,:) = vv(:,:,1,Kmm)
                  CASE('ICECONC')
                     IF ( kstp == nit000 - 1 ) THEN
                        lstp0 = .TRUE.
                     ELSE
#if defined key_si3
                        zsurfvar(:,:) = at_i(:,:)
#elif defined key_cice
                        zsurfvar(:,:) = fr_i(:,:)
#else
                        CALL ctl_stop( ' Trying to run sea-ice observation operator', &
                           &           ' but no sea-ice model appears to have been defined' )
#endif
                     ENDIF
                  CASE('SIT','FBD')
                     IF ( kstp == nit000 - 1 ) THEN
                        lstp0 = .TRUE.
                     ELSE
#if defined key_si3
                        zsurfvar(:,:) = vt_i(:,:)
#elif defined key_cice
                        zsurfvar(:,:) = thick_i(:,:)
#else
                        CALL ctl_stop( ' Trying to run sea-ice observation operator', &
                           &           ' but no sea-ice model appears to have been defined' )
#endif
                     ENDIF
                  END SELECT

                  IF ( lstp0 ) THEN
                     IF ( sobsgroups(jgroup)%ssurfdataqc%nsstpmpp(1) > 0 ) THEN
                        DO jobs = sobsgroups(jgroup)%ssurfdataqc%nsurfup + 1, &
                           &      sobsgroups(jgroup)%ssurfdataqc%nsurfup + sobsgroups(jgroup)%ssurfdataqc%nsstp(1)
                           sobsgroups(jgroup)%ssurfdata%nqc(jobs) = IBSET(sobsgroups(jgroup)%ssurfdata%nqc(jobs),13)
                        END DO
                        IF ( lwp ) THEN
                           CALL ctl_warn( TRIM(sobsgroups(jgroup)%cobstypes(jvar))// &
                              &           ' not initialised on zeroth '           // &
                              &           'time-step but some obs are valid then.' )
                           WRITE(numout,*)sobsgroups(jgroup)%ssurfdataqc%nsstpmpp(1), &
                              &           TRIM(sobsgroups(jgroup)%cobstypes(jvar)),   &
                              &           'observations will be flagged as bad'
                        ENDIF
                     ENDIF
                     IF ( jvar == sobsgroups(jgroup)%ssurfdataqc%nvar ) THEN
                        sobsgroups(jgroup)%ssurfdataqc%nsurfup = sobsgroups(jgroup)%ssurfdataqc%nsurfup + &
                           &                                     sobsgroups(jgroup)%ssurfdataqc%nsstp(1)
                     ENDIF
                  ELSE
                     IF ( sobsgroups(jgroup)%ltime_mean_bkg ) THEN
                        ! Number of time-steps in meaning period
                        imeanstp = NINT( ( sobsgroups(jgroup)%rtime_mean_period * 60.0_wp * 60.0_wp ) / rdt )
                     ELSE
                        imeanstp = 1
                     ENDIF
                     CALL obs_surf_opt( sobsgroups(jgroup)%ssurfdataqc,                          &
                        &               kstp, jpi, jpj,                                          &
                        &               nit000, idaystp,                                         &
                        &               sobsgroups(jgroup)%cgroupname,                           &
                        &               jvar, zsurfvar,                                          &
                        &               sobsgroups(jgroup)%loutput_clim,                         &
                        &               sobsgroups(jgroup)%nadd_clm,                             &
                        &               zsurfclim,                                               &
                        &               sobsgroups(jgroup)%rmask(:,:,1,jvar),                    &
                        &               sobsgroups(jgroup)%n2dint,                               &
                        &               sobsgroups(jgroup)%lnight,                               &
                        &               sobsgroups(jgroup)%ravglamscl,                           &
                        &               sobsgroups(jgroup)%ravgphiscl,                           &
                        &               sobsgroups(jgroup)%lfp_indegs,                           &
                        &               sobsgroups(jgroup)%ltime_mean_bkg,                       &
                        &               imeanstp,                                                &
                        &               kssh=sobsgroups(jgroup)%nadd_ssh,                        &
                        &               kmdt=sobsgroups(jgroup)%next_mdt,                        &
                        &               kfbd=sobsgroups(jgroup)%nadd_fbd,                        &
                        &               ksnow=sobsgroups(jgroup)%next_snow,                      &
                        &               krhosw=sobsgroups(jgroup)%next_rhosw,                    &
                        &               kradar_snow_penetr=sobsgroups(jgroup)%radar_snow_penetr )
                  ENDIF

               END DO

            ENDIF

         ENDIF
      END DO

      DEALLOCATE( zprofvar, zprofclim, &
         &        zsurfvar, zsurfclim )

      IF ( sn_cfctl%l_obsstat ) THEN
         IF ( lwm ) THEN
!$AGRIF_DO_NOT_TREAT
            WRITE(numobsstat,'(I10,1X)',advance='no') kstp
!$AGRIF_END_DO_NOT_TREAT
         ENDIF
         DO jgroup = 1, nn_obsgroups
            IF ( sobsgroups(jgroup)%lenabled ) THEN
               IF ( sobsgroups(jgroup)%lprof ) THEN
                  DO jvar = 1, sobsgroups(jgroup)%sprofdataqc%nvar
                     zsumx = 0.0_wp
                     zsumx2 = 0.0_wp
                     inumgoodobs = 0
                     DO jo = 1, sobsgroups(jgroup)%sprofdataqc%nprof
                        DO jk = sobsgroups(jgroup)%sprofdataqc%npvsta(jo,jvar), sobsgroups(jgroup)%sprofdataqc%npvend(jo,jvar)
                           IF ( ( sobsgroups(jgroup)%sprofdataqc%var(jvar)%vobs(jk) < 9999.0_wp ) .AND. &
                              & ( sobsgroups(jgroup)%sprofdataqc%var(jvar)%vdep(jk) < 9999.0_wp ) .AND. &
                              & ( sobsgroups(jgroup)%sprofdataqc%var(jvar)%vmod(jk) < 9999.0_wp ) ) THEN
                              zomb = sobsgroups(jgroup)%sprofdataqc%var(jvar)%vmod(jk) - sobsgroups(jgroup)%sprofdataqc%var(jvar)%vobs(jk)
                              zsumx = zsumx + zomb
                              zsumx2 = zsumx2 + zomb**2
                              inumgoodobs = inumgoodobs + 1
                           ENDIF
                        ENDDO
                     ENDDO
                     CALL obs_mpp_sum_integer( inumgoodobs, inumgoodobsmpp )
                     CALL mpp_sum('dia_obs', zsumx)
                     CALL mpp_sum('dia_obs', zsumx2)
                     IF ( lwm ) THEN
!$AGRIF_DO_NOT_TREAT
                        WRITE(numobsstat,'(A8,1X)',        advance='no') sobsgroups(jgroup)%sprofdataqc%cvars(jvar)
                        WRITE(numobsstat,'(A4,1X,I12,1X)', advance='no') 'GOOD', inumgoodobsmpp
                        IF ( inumgoodobsmpp > 0 ) THEN
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') 'MEAN', zsumx / REAL(inumgoodobsmpp, wp)
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') ' RMS', sqrt( zsumx2 / REAL(inumgoodobsmpp, wp) )
                        ELSE
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') 'MEAN', 0.0_wp
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') ' RMS', 0.0_wp
                        ENDIF
!$AGRIF_END_DO_NOT_TREAT
                     ENDIF
                  ENDDO
               ELSEIF ( sobsgroups(jgroup)%lsurf ) THEN
                  DO jvar = 1, sobsgroups(jgroup)%ssurfdataqc%nvar
                     zsumx = 0.0_wp
                     zsumx2 = 0.0_wp
                     inumgoodobs = 0
                     DO jo = 1, sobsgroups(jgroup)%ssurfdataqc%nsurf
                        IF ( ( sobsgroups(jgroup)%ssurfdataqc%robs(jo,jvar) < 9999.0_wp ) .AND. &
                           & ( sobsgroups(jgroup)%ssurfdataqc%rmod(jo,jvar) < 9999.0_wp ) ) THEN
                           zomb = sobsgroups(jgroup)%ssurfdataqc%rmod(jo,jvar) - sobsgroups(jgroup)%ssurfdataqc%robs(jo,jvar)
                           zsumx = zsumx + zomb
                           zsumx2 = zsumx2 + zomb**2
                           inumgoodobs = inumgoodobs + 1
                        ENDIF
                     ENDDO
                     CALL obs_mpp_sum_integer( inumgoodobs, inumgoodobsmpp )
                     CALL mpp_sum('dia_obs', zsumx)
                     CALL mpp_sum('dia_obs', zsumx2)
                     IF ( lwm ) THEN
!$AGRIF_DO_NOT_TREAT
                        WRITE(numobsstat,'(A8,1X)',        advance='no') sobsgroups(jgroup)%ssurfdataqc%cvars(jvar)
                        WRITE(numobsstat,'(A4,1X,I12,1X)', advance='no') 'GOOD', inumgoodobsmpp
                        IF ( inumgoodobsmpp > 0 ) THEN
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') 'MEAN', zsumx / REAL(inumgoodobsmpp, wp)
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') ' RMS', sqrt( zsumx2 / REAL(inumgoodobsmpp, wp) )
                        ELSE
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') 'MEAN', 0.0_wp
                           WRITE(numobsstat,'(A4,1X,D23.16,1X)',advance='no') ' RMS', 0.0_wp
                        ENDIF
!$AGRIF_END_DO_NOT_TREAT
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
         IF ( lwm ) THEN
            WRITE(numobsstat,'(A1)') ' '
         ENDIF
      ENDIF

      IF( ln_timing )   CALL timing_stop('dia_obs')

   END SUBROUTINE dia_obs

   SUBROUTINE dia_obs_wri
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_obs_wri  ***
      !!          
      !! ** Purpose : Call observation diagnostic output routines
      !!
      !! ** Method  : Call observation diagnostic output routines
      !!
      !! ** Action  :
      !!
      !! History :
      !!        !  06-03  (K. Mogensen) Original code
      !!        !  06-05  (K. Mogensen) Reformatted
      !!        !  06-10  (A. Weaver) Cleaning
      !!        !  07-03  (K. Mogensen) General handling of profiles
      !!        !  08-09  (M. Valdivieso) Velocity component (U,V) profiles
      !!        !  15-08  (M. Martin) Combined writing for prof and surf types
      !!----------------------------------------------------------------------
      !! * Modules used
      USE obs_rot_vel          ! Rotation of velocities

      IMPLICIT NONE

      !! * Local declarations
      INTEGER :: jgroup                   ! Data set loop variable
      INTEGER :: jo, jvar, jk, jadd, jext, jadd2, jext2
      INTEGER :: iuvar, ivvar
      REAL(wp), DIMENSION(:), ALLOCATABLE :: &
         & zu, &
         & zv
      LOGICAL, DIMENSION(:), ALLOCATABLE :: ll_write
      TYPE(obswriinfo) :: sladd, slext

      IF( ln_timing )   CALL timing_start('dia_obs_wri')

      !-----------------------------------------------------------------------
      ! Depending on switches call various observation output routines
      !-----------------------------------------------------------------------

      DO jgroup = 1, nn_obsgroups
         IF (sobsgroups(jgroup)%lenabled) THEN

            IF (sobsgroups(jgroup)%lprof) THEN

               IF (sobsgroups(jgroup)%lvel) THEN
                  iuvar = sobsgroups(jgroup)%nuvel
                  ivvar = sobsgroups(jgroup)%nvvel
                  IF ( (iuvar > 0) .AND. (ivvar > 0) ) THEN

                     ! For velocity data, rotate the model velocities to N/S, E/W
                     ! using the compressed data structure.
                     ALLOCATE( &
                        & zu(sobsgroups(jgroup)%sprofdataqc%nvprot(iuvar)), &
                        & zv(sobsgroups(jgroup)%sprofdataqc%nvprot(ivvar))  &
                        & )

                     CALL obs_rotvel_pro( sobsgroups(jgroup)%sprofdataqc, sobsgroups(jgroup)%n2dint, &
                        &                 iuvar, ivvar, zu, zv )

                     DO jo = 1, sobsgroups(jgroup)%sprofdataqc%nprof
                        DO jk = sobsgroups(jgroup)%sprofdataqc%npvsta(jo,iuvar), sobsgroups(jgroup)%sprofdataqc%npvend(jo,iuvar)
                           sobsgroups(jgroup)%sprofdataqc%var(iuvar)%vmod(jk) = zu(jk)
                        END DO
                        DO jk = sobsgroups(jgroup)%sprofdataqc%npvsta(jo,ivvar), sobsgroups(jgroup)%sprofdataqc%npvend(jo,ivvar)
                           sobsgroups(jgroup)%sprofdataqc%var(ivvar)%vmod(jk) = zv(jk)
                        END DO
                     END DO

                     DEALLOCATE( zu )
                     DEALLOCATE( zv )

                  ELSE
                     CALL ctl_stop( 'Could not identify velocity observation variables to rotate in group', &
                        &           TRIM(sobsgroups(jgroup)%cgroupname) )
                  END IF
               END IF

               CALL obs_prof_decompress( sobsgroups(jgroup)%sprofdataqc, &
                  &                      sobsgroups(jgroup)%sprofdata, .TRUE., numout )

               ! Put additional and extra variable information into obswriinfo structure
               ! used by obs_write.
               ! add/ext variables generated by the OBS code (1...sobsgroups(jgroup)%naddvars)
               ! may duplicate ones read in (%naddvars+1...sobsgroups(jgroup)%sprofdata%nadd)
               ! Check for this, and if so only write out the version generated by the OBS code
               sladd%inum = sobsgroups(jgroup)%sprofdata%nadd
               ALLOCATE( ll_write(sobsgroups(jgroup)%sprofdata%nadd) )
               ll_write(:) = .TRUE.
               IF ( (sobsgroups(jgroup)%naddvars > 0) .AND. &
                  & (sobsgroups(jgroup)%sprofdata%nadd > sobsgroups(jgroup)%naddvars) ) THEN
                  DO jadd = sobsgroups(jgroup)%naddvars + 1, sobsgroups(jgroup)%sprofdata%nadd
                     DO jadd2 = 1, sobsgroups(jgroup)%naddvars
                        IF ( TRIM(sobsgroups(jgroup)%sprofdata%caddvars(jadd )) == &
                           & TRIM(sobsgroups(jgroup)%sprofdata%caddvars(jadd2)) ) THEN
                           sladd%inum = sladd%inum - 1
                           ll_write(jadd) = .FALSE.
                        ENDIF
                     END DO
                  END DO
               ENDIF
               IF ( sladd%inum > 0 ) THEN
                  ALLOCATE( sladd%ipoint(sladd%inum),                                   &
                     &      sladd%cdname(sladd%inum),                                   &
                     &      sladd%cdlong(sladd%inum,sobsgroups(jgroup)%sprofdata%nvar), &
                     &      sladd%cdunit(sladd%inum,sobsgroups(jgroup)%sprofdata%nvar) )
                  jadd2 = 0
                  DO jadd = 1, sobsgroups(jgroup)%sprofdata%nadd
                     IF ( ll_write(jadd) ) THEN
                        jadd2 = jadd2 + 1
                        sladd%ipoint(jadd2) = jadd
                        sladd%cdname(jadd2) = sobsgroups(jgroup)%sprofdata%caddvars(jadd)
                        DO jvar = 1, sobsgroups(jgroup)%sprofdata%nvar
                           sladd%cdlong(jadd2,jvar) = sobsgroups(jgroup)%sprofdata%caddlong(jadd,jvar)
                           sladd%cdunit(jadd2,jvar) = sobsgroups(jgroup)%sprofdata%caddunit(jadd,jvar)
                        END DO
                     ENDIF
                  END DO
               ENDIF
               DEALLOCATE( ll_write )

               slext%inum = sobsgroups(jgroup)%sprofdata%next
               ALLOCATE( ll_write(sobsgroups(jgroup)%sprofdata%next) )
               ll_write(:) = .TRUE.
               IF ( (sobsgroups(jgroup)%nextvars > 0) .AND. &
                  & (sobsgroups(jgroup)%sprofdata%next > sobsgroups(jgroup)%nextvars) ) THEN
                  DO jext = sobsgroups(jgroup)%nextvars + 1, sobsgroups(jgroup)%sprofdata%next
                     DO jext2 = 1, sobsgroups(jgroup)%nextvars
                        IF ( TRIM(sobsgroups(jgroup)%sprofdata%cextvars(jext )) == &
                           & TRIM(sobsgroups(jgroup)%sprofdata%cextvars(jext2)) ) THEN
                           slext%inum = slext%inum - 1
                           ll_write(jext) = .FALSE.
                        ENDIF
                     END DO
                  END DO
               ENDIF
               IF ( slext%inum > 0 ) THEN
                  ALLOCATE( slext%ipoint(slext%inum),   &
                     &      slext%cdname(slext%inum),   &
                     &      slext%cdlong(slext%inum,1), &
                     &      slext%cdunit(slext%inum,1) )
                  jext2 = 0
                  DO jext = 1, sobsgroups(jgroup)%sprofdata%next
                     IF ( ll_write(jext) ) THEN
                        jext2 = jext2 + 1
                        slext%ipoint(jext2)   = jext
                        slext%cdname(jext2)   = sobsgroups(jgroup)%sprofdata%cextvars(jext)
                        slext%cdlong(jext2,1) = sobsgroups(jgroup)%sprofdata%cextlong(jext)
                        slext%cdunit(jext2,1) = sobsgroups(jgroup)%sprofdata%cextunit(jext)
                     ENDIF
                  END DO
               ENDIF
               DEALLOCATE( ll_write )

               CALL obs_wri_prof( sobsgroups(jgroup)%sprofdata, sobsgroups(jgroup)%cgroupname, sladd, slext )

               IF ( sladd%inum > 0 ) THEN
                  DEALLOCATE( sladd%ipoint, sladd%cdname, sladd%cdlong, sladd%cdunit )
               ENDIF
               IF ( slext%inum > 0 ) THEN
                  DEALLOCATE( slext%ipoint, slext%cdname, slext%cdlong, slext%cdunit )
               ENDIF

            ELSEIF (sobsgroups(jgroup)%lsurf) THEN

               IF (sobsgroups(jgroup)%lvel) THEN
                  iuvar = sobsgroups(jgroup)%nuvel
                  ivvar = sobsgroups(jgroup)%nvvel
                  IF ( (iuvar > 0) .AND. (ivvar > 0) ) THEN

                     ! For velocity data, rotate the model velocities to N/S, E/W
                     ! using the compressed data structure.
                     ALLOCATE( &
                        & zu(sobsgroups(jgroup)%ssurfdataqc%nsurf), &
                        & zv(sobsgroups(jgroup)%ssurfdataqc%nsurf)  &
                        & )

                     CALL obs_rotvel_surf( sobsgroups(jgroup)%ssurfdataqc, sobsgroups(jgroup)%n2dint, &
                        &                  iuvar, ivvar, zu, zv )

                     DO jo = 1, sobsgroups(jgroup)%ssurfdataqc%nsurf
                        sobsgroups(jgroup)%ssurfdataqc%rmod(jo,iuvar) = zu(jo)
                        sobsgroups(jgroup)%ssurfdataqc%rmod(jo,ivvar) = zv(jo)
                     END DO

                     DEALLOCATE( zu )
                     DEALLOCATE( zv )

                  ELSE
                     CALL ctl_stop( 'Could not identify velocity observation variables to rotate in group', &
                        &           TRIM(sobsgroups(jgroup)%cgroupname) )
                  END IF
               END IF

               CALL obs_surf_decompress( sobsgroups(jgroup)%ssurfdataqc, &
                  &                      sobsgroups(jgroup)%ssurfdata, .TRUE., numout )

               IF (sobsgroups(jgroup)%lfbd) THEN
                  ! Input observations were freeboard, but we're outputting ice thickness
                  jvar = sobsgroups(jgroup)%nfbd
                  sobsgroups(jgroup)%ssurfdata%cvars(jvar) = 'SIT'
                  sobsgroups(jgroup)%ssurfdata%clong(jvar) = 'Sea ice thickness'
                  sobsgroups(jgroup)%ssurfdata%cunit(jvar) = 'm'
               ENDIF

               ! Put additional and extra variable information into obswriinfo structure
               ! used by obs_write.
               ! add/ext variables generated by the OBS code (1...sobsgroups(jgroup)%naddvars)
               ! may duplicate ones read in (%naddvars+1...sobsgroups(jgroup)%ssurfdata%nadd)
               ! Check for this, and if so only write out the version generated by the OBS code
               sladd%inum = sobsgroups(jgroup)%ssurfdata%nadd
               ALLOCATE( ll_write(sobsgroups(jgroup)%ssurfdata%nadd) )
               ll_write(:) = .TRUE.
               IF ( (sobsgroups(jgroup)%naddvars > 0) .AND. &
                  & (sobsgroups(jgroup)%ssurfdata%nadd > sobsgroups(jgroup)%naddvars) ) THEN
                  DO jadd = sobsgroups(jgroup)%naddvars + 1, sobsgroups(jgroup)%ssurfdata%nadd
                     DO jadd2 = 1, sobsgroups(jgroup)%naddvars
                        IF ( TRIM(sobsgroups(jgroup)%ssurfdata%caddvars(jadd )) == &
                           & TRIM(sobsgroups(jgroup)%ssurfdata%caddvars(jadd2)) ) THEN
                           sladd%inum = sladd%inum - 1
                           ll_write(jadd) = .FALSE.
                        ENDIF
                     END DO
                  END DO
               ENDIF
               IF ( sladd%inum > 0 ) THEN
                  ALLOCATE( sladd%ipoint(sladd%inum),                                   &
                     &      sladd%cdname(sladd%inum),                                   &
                     &      sladd%cdlong(sladd%inum,sobsgroups(jgroup)%ssurfdata%nvar), &
                     &      sladd%cdunit(sladd%inum,sobsgroups(jgroup)%ssurfdata%nvar) )
                  jadd2 = 0
                  DO jadd = 1, sobsgroups(jgroup)%ssurfdata%nadd
                     IF ( ll_write(jadd) ) THEN
                        jadd2 = jadd2 + 1
                        sladd%ipoint(jadd2) = jadd
                        sladd%cdname(jadd2) = sobsgroups(jgroup)%ssurfdata%caddvars(jadd)
                        DO jvar = 1, sobsgroups(jgroup)%ssurfdata%nvar
                           sladd%cdlong(jadd2,jvar) = sobsgroups(jgroup)%ssurfdata%caddlong(jadd,jvar)
                           sladd%cdunit(jadd2,jvar) = sobsgroups(jgroup)%ssurfdata%caddunit(jadd,jvar)
                        END DO
                     ENDIF
                  END DO
               ENDIF
               DEALLOCATE( ll_write )

               slext%inum = sobsgroups(jgroup)%ssurfdata%nextra
               ALLOCATE( ll_write(sobsgroups(jgroup)%ssurfdata%nextra) )
               ll_write(:) = .TRUE.
               IF ( (sobsgroups(jgroup)%nextvars > 0) .AND. &
                  & (sobsgroups(jgroup)%ssurfdata%nextra > sobsgroups(jgroup)%nextvars) ) THEN
                  DO jext = sobsgroups(jgroup)%nextvars + 1, sobsgroups(jgroup)%ssurfdata%nextra
                     DO jext2 = 1, sobsgroups(jgroup)%nextvars
                        IF ( TRIM(sobsgroups(jgroup)%ssurfdata%cextvars(jext )) == &
                           & TRIM(sobsgroups(jgroup)%ssurfdata%cextvars(jext2)) ) THEN
                           slext%inum = slext%inum - 1
                           ll_write(jext) = .FALSE.
                        ENDIF
                     END DO
                  END DO
               ENDIF
               IF ( slext%inum > 0 ) THEN
                  ALLOCATE( slext%ipoint(slext%inum),   &
                     &      slext%cdname(slext%inum),   &
                     &      slext%cdlong(slext%inum,1), &
                     &      slext%cdunit(slext%inum,1) )
                  jext2 = 0
                  DO jext = 1, sobsgroups(jgroup)%ssurfdata%nextra
                     IF ( ll_write(jext) ) THEN
                        jext2 = jext2 + 1
                        slext%ipoint(jext2)   = jext
                        slext%cdname(jext2)   = sobsgroups(jgroup)%ssurfdata%cextvars(jext)
                        slext%cdlong(jext2,1) = sobsgroups(jgroup)%ssurfdata%cextlong(jext)
                        slext%cdunit(jext2,1) = sobsgroups(jgroup)%ssurfdata%cextunit(jext)
                     ENDIF
                  END DO
               ENDIF
               DEALLOCATE( ll_write )

               CALL obs_wri_surf( sobsgroups(jgroup)%ssurfdata, sobsgroups(jgroup)%cgroupname, sladd, slext )

               IF ( sladd%inum > 0 ) THEN
                  DEALLOCATE( sladd%ipoint, sladd%cdname, sladd%cdlong, sladd%cdunit )
               ENDIF
               IF ( slext%inum > 0 ) THEN
                  DEALLOCATE( slext%ipoint, slext%cdname, slext%cdlong, slext%cdunit )
               ENDIF

            ENDIF

         ENDIF

      END DO

      IF( ln_timing )   CALL timing_stop('dia_obs_wri')

   END SUBROUTINE dia_obs_wri

   SUBROUTINE calc_date( kstp, ddobs )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE calc_date  ***
      !!          
      !! ** Purpose : Get date in double precision YYYYMMDD.HHMMSS format
      !!
      !! ** Method  : Get date in double precision YYYYMMDD.HHMMSS format
      !!
      !! ** Action  : Get date in double precision YYYYMMDD.HHMMSS format
      !!
      !! ** Action  : Get initial date in double precision YYYYMMDD.HHMMSS format
      !!
      !! History :
      !!        !  06-03  (K. Mogensen)  Original code
      !!        !  06-05  (K. Mogensen)  Reformatted
      !!        !  06-10  (A. Weaver) Cleaning
      !!        !  06-10  (G. Smith) Calculates initial date the same as method for final date
      !!        !  10-05  (D. Lea) Update to month length calculation for NEMO vn3.2
      !!        !  2014-09  (D. Lea) New generic routine now deals with arbitrary initial time of day
      !!----------------------------------------------------------------------
      USE phycst, ONLY : &            ! Physical constants
         & rday
      USE dom_oce, ONLY : &           ! Ocean space and time domain variables
         & rn_Dt

      IMPLICIT NONE

      !! * Arguments
      REAL(KIND=dp), INTENT(OUT) :: ddobs                        ! Date in YYYYMMDD.HHMMSS
      INTEGER, INTENT(in) :: kstp

      !! * Local declarations
      INTEGER :: iyea        ! date - (year, month, day, hour, minute)
      INTEGER :: imon
      INTEGER :: iday
      INTEGER :: ihou
      INTEGER :: imin
      INTEGER :: imday       ! Number of days in month.
      REAL(wp) :: zdayfrc    ! Fraction of day

      INTEGER, DIMENSION(12) ::   imonth_len    !: length in days of the months of the current year

      !!----------------------------------------------------------------------
      !! Initial date initialization (year, month, day, hour, minute)
      !!----------------------------------------------------------------------
      iyea =   ndate0 / 10000
      imon = ( ndate0 - iyea * 10000 ) / 100
      iday =   ndate0 - iyea * 10000 - imon * 100
      ihou =   nn_time0 / 100
      imin = ( nn_time0 - ihou * 100 ) 

      !!----------------------------------------------------------------------
      !! Compute number of days + number of hours + min since initial time
      !!----------------------------------------------------------------------
      zdayfrc = kstp * rn_Dt / rday
      zdayfrc = zdayfrc - aint(zdayfrc)
      imin = imin + int( zdayfrc * 24 * 60 ) 
      DO WHILE (imin >= 60) 
        imin=imin-60
        ihou=ihou+1
      END DO
      DO WHILE (ihou >= 24)
        ihou=ihou-24
        iday=iday+1
      END DO 
      iday = iday + kstp * rn_Dt / rday 

      !-----------------------------------------------------------------------
      ! Convert number of days (iday) into a real date
      !----------------------------------------------------------------------

      CALL calc_month_len( iyea, imonth_len )

      DO WHILE ( iday > imonth_len(imon) )
         iday = iday - imonth_len(imon)
         imon = imon + 1 
         IF ( imon > 12 ) THEN
            imon = 1
            iyea = iyea + 1
            CALL calc_month_len( iyea, imonth_len )  ! update month lengths
         ENDIF
      END DO

      !----------------------------------------------------------------------
      ! Convert it into YYYYMMDD.HHMMSS format.
      !----------------------------------------------------------------------
      ddobs = iyea * 10000_dp + imon * 100_dp + &
         &    iday + ihou * 0.01_dp + imin * 0.0001_dp

   END SUBROUTINE calc_date

END MODULE diaobs
