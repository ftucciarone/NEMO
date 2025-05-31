MODULE obs_bias
   !!======================================================================
   !!                       ***  MODULE obs_bias  ***
   !! Observation diagnostics: Read the bias for observation data
   !!======================================================================
   !!----------------------------------------------------------------------
   !!   obs_app_bias : Driver for reading and applying the bias
   !!----------------------------------------------------------------------
   !! * Modules used   
   USE par_kind, ONLY : &       ! Precision variables
      & wp
   USE par_oce, ONLY : &        ! Domain parameters
      & jpi, &
      & jpj
   USE in_out_manager, ONLY : & ! I/O manager
      & lwp,    &
      & numout 
   USE obs_surf_def             ! Surface observation definitions
   USE dom_oce, ONLY : &        ! Domain variables
      & tmask, &
      & gphit, &
      & glamt
   USE obs_inter_h2d
   USE obs_utils               ! Various observation tools
   USE obs_inter_sup
   IMPLICIT NONE
   !! * Routine accessibility
   PRIVATE
   PUBLIC obs_app_bias     ! Read the observation bias
CONTAINS
   SUBROUTINE obs_app_bias( obsdata, kvar, k2dint, knumtypes, &
                            cl_bias_files, cd_biasname,       &
                            ld_extvar )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_app_bias ***
      !!
      !! ** Purpose : Read bias data from files and apply correction to
      !!              observations
      !!
      !! ** Method  :
      !!
      !! ** Action  :
      !!
      !! References :
      !!
      !! History : 
      !!      ! :  2014-08 (J. While) Bias correction code for SST obs,
      !!      !                       based on obs_rea_altbias
      !!      ! :  2021-07 (D. Ford)  Renamed obs_app_bias and made generic
      !!----------------------------------------------------------------------
      !! * Modules used
      USE iom
      USE netcdf

      !! * Arguments
      TYPE(obs_surf), INTENT(INOUT) :: obsdata       ! Observation data
      INTEGER, INTENT(IN) :: kvar    ! Index of obs type being bias corrected
      INTEGER, INTENT(IN) :: k2dint
      INTEGER, INTENT(IN) :: knumtypes !number of bias types to read in
      CHARACTER(LEN=128), DIMENSION(knumtypes), INTENT(IN) :: &
                          cl_bias_files !List of files to read
      CHARACTER(LEN=*), INTENT(IN) :: cd_biasname ! Variable name in file
      LOGICAL, OPTIONAL, INTENT(IN)  :: ld_extvar ! If present and true correct the extra variable with index kvar
                                                  ! Otherwise, correct the observation variable with index kvar

      !! * Local declarations
      INTEGER :: jobs         ! Obs loop variable
      INTEGER :: iico         ! Grid point indices
      INTEGER :: ijco
      INTEGER :: jt
      INTEGER, DIMENSION(knumtypes) :: &
         & ibiastypes             ! Array of the bias types in each file
      REAL(wp), DIMENSION(jpi,jpj,knumtypes) :: & 
         & z_obsbias              ! Array to store the bias values
      REAL(wp), DIMENSION(jpi,jpj) :: & 
         & z_obsbias_2d           ! Array to store the bias values   
      REAL(wp), DIMENSION(1) :: &
         & zext, &
         & zobsmask
      REAL(wp), DIMENSION(2,2,1) :: &
         & zweig
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zmask, &
         & zglam, &
         & zgphi
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zmask_tmp, &
         & zglam_tmp, &
         & zgphi_tmp   
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::  zbias   
      REAL(wp) :: zlam
      REAL(wp) :: zphi
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: &
         & igrdi, &
         & igrdj
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: &
         & igrdi_tmp, &
         & igrdj_tmp   
      INTEGER ::   inumobsbias
      INTEGER(KIND=NF90_INT) :: ifile_source     
      INTEGER :: incfile
      INTEGER :: jtype
      INTEGER :: iret 
      INTEGER :: inumtype
      LOGICAL :: ll_extvar

      IF ( PRESENT(ld_extvar) ) THEN
         ll_extvar = ld_extvar
      ELSE
         ll_extvar = .FALSE.
      ENDIF
      IF ( ll_extvar .AND. ( knumtypes /= 1 ) ) THEN
         CALL ctl_stop( 'obs_app_bias: If correcting an extra variable', &
            &           '              knumtypes must be 1' )
      ENDIF
      
      IF (lwp) WRITE(numout,*) 
      IF (lwp) WRITE(numout,*) 'obs_app_bias : '
      IF (lwp) WRITE(numout,*) '----------------- '
      IF ( ll_extvar ) THEN
         IF (lwp) WRITE(numout,*) 'Read observation bias for ', TRIM(obsdata%cextvars(kvar))
      ELSE
         IF (lwp) WRITE(numout,*) 'Read observation bias for ', TRIM(obsdata%cvars(kvar))
      ENDIF

      ! Open and read the files
      z_obsbias(:,:,:) = 0.0_wp
      DO jtype = 1, knumtypes
     
         inumobsbias = 0
         IF (lwp) WRITE(numout,*) 'Opening ', cl_bias_files(jtype)
         CALL iom_open( cl_bias_files(jtype), inumobsbias, ldstop=.FALSE. )
         IF (inumobsbias > 0) THEN
     
            IF ( .NOT. ll_extvar ) THEN
               !Read the bias type from the file
               !No IOM get attribute command at time of writing, 
               !so have to use NETCDF
               !routines directly - should be upgraded in the future
               iret = NF90_OPEN(TRIM(cl_bias_files(jtype)), NF90_NOWRITE, incfile)
               IF ( .NOT. ll_extvar ) THEN
                  iret=NF90_GET_ATT( incfile, NF90_GLOBAL, TRIM(obsdata%cvars(kvar))//"_source", &
                                    ifile_source )
                  ibiastypes(jtype) = ifile_source
               ENDIF
               iret = NF90_CLOSE(incfile)
               IF ( iret /= 0  ) CALL ctl_stop( &
                  'obs_app_bias : Cannot read bias type from file '// &
                  cl_bias_files(jtype) )
            ENDIF

            ! Get the bias data
            CALL iom_get( inumobsbias, jpdom_global, TRIM(cd_biasname), z_obsbias_2d(:,:), 1 )
            z_obsbias(:,:,jtype) = z_obsbias_2d(:,:)       
            ! Close the file
            CALL iom_close(inumobsbias)
         ELSE
            CALL ctl_stop('obs_app_bias: File '// & 
                           TRIM( cl_bias_files(jtype) )//' Not found')
         ENDIF
      END DO
           
      ! Interpolate the bias already on the model grid at the observation point
      ALLOCATE( &
         & igrdi(2,2,obsdata%nsurf), &
         & igrdj(2,2,obsdata%nsurf), &
         & zglam(2,2,obsdata%nsurf), &
         & zgphi(2,2,obsdata%nsurf), &
         & zmask(2,2,obsdata%nsurf)  )
       
      DO jobs = 1, obsdata%nsurf 
         igrdi(1,1,jobs) = obsdata%mi(jobs,kvar)-1
         igrdj(1,1,jobs) = obsdata%mj(jobs,kvar)-1
         igrdi(1,2,jobs) = obsdata%mi(jobs,kvar)-1
         igrdj(1,2,jobs) = obsdata%mj(jobs,kvar)
         igrdi(2,1,jobs) = obsdata%mi(jobs,kvar)
         igrdj(2,1,jobs) = obsdata%mj(jobs,kvar)-1
         igrdi(2,2,jobs) = obsdata%mi(jobs,kvar)
         igrdj(2,2,jobs) = obsdata%mj(jobs,kvar)
      END DO
      CALL obs_int_comm_2d( 2, 2, obsdata%nsurf, jpi, jpj, &
         &                  igrdi, igrdj, glamt, zglam )
      CALL obs_int_comm_2d( 2, 2, obsdata%nsurf, jpi, jpj, &
         &                  igrdi, igrdj, gphit, zgphi )
      CALL obs_int_comm_2d( 2, 2, obsdata%nsurf, jpi, jpj, &
         &                  igrdi, igrdj, tmask(:,:,1), zmask )
      DO jtype = 1, knumtypes
         
         !Find the number observations of type and allocate tempory arrays
         inumtype = COUNT( obsdata%ntyp(:) == ibiastypes(jtype) )
         ALLOCATE( &
            & igrdi_tmp(2,2,inumtype), &
            & igrdj_tmp(2,2,inumtype), &
            & zglam_tmp(2,2,inumtype), &
            & zgphi_tmp(2,2,inumtype), &
            & zmask_tmp(2,2,inumtype), &
            & zbias( 2,2,inumtype ) )
         jt = 1
         DO jobs = 1, obsdata%nsurf 
            IF ( obsdata%ntyp(jobs) == ibiastypes(jtype) ) THEN
               igrdi_tmp(:,:,jt) = igrdi(:,:,jobs) 
               igrdj_tmp(:,:,jt) = igrdj(:,:,jobs)
               zglam_tmp(:,:,jt) = zglam(:,:,jobs)
               zgphi_tmp(:,:,jt) = zgphi(:,:,jobs)
               zmask_tmp(:,:,jt) = zmask(:,:,jobs)
               jt = jt +1
            ENDIF
         END DO
                         
         CALL obs_int_comm_2d( 2, 2, inumtype, jpi, jpj, &
               &           igrdi_tmp(:,:,:), igrdj_tmp(:,:,:), &
               &           z_obsbias(:,:,jtype), zbias(:,:,:) )
         jt = 1
         DO jobs = 1, obsdata%nsurf
            IF ( ( obsdata%ntyp(jobs) == ibiastypes(jtype) ) .OR. &
               &  ll_extvar ) THEN
               zlam = obsdata%rlam(jobs)
               zphi = obsdata%rphi(jobs)
               iico = obsdata%mi(jobs,kvar)
               ijco = obsdata%mj(jobs,kvar)
               CALL obs_int_h2d_init( 1, 1, k2dint, zlam, zphi,         &
                  &                   zglam_tmp(:,:,jt), &
                  &                   zgphi_tmp(:,:,jt), &
                  &                   zmask_tmp(:,:,jt), zweig, zobsmask )
               CALL obs_int_h2d( 1, 1, zweig, zbias(:,:,jt),  zext )
               ! adjust observations with bias field
               IF ( ll_extvar ) THEN
                  obsdata%rext(jobs,kvar) = obsdata%rext(jobs,kvar) - zext(1)
               ELSE
                  obsdata%robs(jobs,kvar) = obsdata%robs(jobs,kvar) - zext(1)
               ENDIF
               jt = jt + 1
            ENDIF
         END DO 
               
         !Deallocate arrays
         DEALLOCATE( &
         & igrdi_tmp, &
         & igrdj_tmp, &
         & zglam_tmp, &
         & zgphi_tmp, &
         & zmask_tmp, &
         & zbias )           
      END DO
      DEALLOCATE( &
         & igrdi, &
         & igrdj, &
         & zglam, &
         & zgphi, &
         & zmask )

      IF(lwp) THEN
         WRITE(numout,*) " "
         WRITE(numout,*) "Bias correction applied successfully"
         IF ( .NOT. ll_extvar ) THEN
            WRITE(numout,*) "Obs types: ", ibiastypes(:), &
               &            " Have all been bias corrected"
         ENDIF
      ENDIF
   END SUBROUTINE obs_app_bias
 
END MODULE obs_bias
