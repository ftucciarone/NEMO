MODULE obs_read_surf
   !!======================================================================
   !!                       ***  MODULE obs_read_surf  ***
   !! Observation diagnostics: Read the surface data from feedback files
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rea_surf : Driver for reading surface data from feedback files
   !!----------------------------------------------------------------------

   !! * Modules used
   USE par_kind                 ! Precision variables
   USE in_out_manager           ! I/O manager
   USE dom_oce                  ! Ocean space and time domain variables
   USE obs_mpp                  ! MPP support routines for observation diagnostics
   USE julian                   ! Julian date routines
   USE obs_utils                ! Observation operator utility functions
   USE obs_grid                 ! Grid search
   USE obs_sort                 ! Sorting observation arrays
   USE obs_surf_def             ! Surface observation definitions
   USE obs_types                ! Observation type definitions
   USE obs_fbm                  ! Feedback routines
   USE netcdf                   ! NetCDF library
   USE obs_group_def, ONLY : &  ! Observation variable information
      & cobsname_uvel, &
      & cobsname_vvel

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rea_surf      ! Read the surface observations from the point data

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_rea_surf( surfdata, knumfiles, cdfilenames, &
      &                     kvars, kadd, kextr, kstp, ddobsini, ddobsend, &
      &                     ptime_mean_period, ld_time_mean_bkg, &
      &                     ldignmis, ldmod, ldnightav, cdvars )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_surf ***
      !!
      !! ** Purpose : Read from file the surface data
      !!
      !! ** Method  : Read in the data from feedback format files and 
      !!              put into the NEMO internal surface data structure
      !!
      !! ** Action  : 
      !!
      !!
      !! History :  
      !!      ! :  2009-01 (K. Mogensen) Initial version based on old versions
      !!      ! :  2015-02 (M. Martin)   Unify the different surface data type reading.
      !!----------------------------------------------------------------------
      !! * Modules used

      !! * Arguments
      TYPE(obs_surf), INTENT(INOUT) :: &
         & surfdata                             ! Surface data to be read
      INTEGER,  INTENT(IN) :: knumfiles         ! Number of corio format files to read
      CHARACTER(LEN=128), INTENT(IN) :: &
         & cdfilenames(knumfiles)               ! File names to read in
      INTEGER,  INTENT(IN) :: kvars             ! Number of variables in surfdata
      INTEGER,  INTENT(IN) :: kadd              ! Number of additional fields
                                                !   in addition to those in the input file(s)
      INTEGER,  INTENT(IN) :: kextr             ! Number of extra fields
                                                !   in addition to those in the input file(s)
      INTEGER,  INTENT(IN) :: kstp              ! Ocean time-step index
      REAL(dp), INTENT(IN) :: ddobsini          ! Obs. ini time in YYYYMMDD.HHMMSS
      REAL(dp), INTENT(IN) :: ddobsend          ! Obs. end time in YYYYMMDD.HHMMSS
      REAL(wp), INTENT(IN) :: ptime_mean_period ! Averaging period in hours
      LOGICAL,  INTENT(IN) :: ld_time_mean_bkg  ! Will reset times to end of averaging period
      LOGICAL,  INTENT(IN) :: ldignmis          ! Ignore missing files
      LOGICAL,  INTENT(IN) :: ldmod             ! Initialize model from input data
      LOGICAL,  INTENT(IN) :: ldnightav         ! Observations represent a night-time average
      CHARACTER(len=8), DIMENSION(kvars), INTENT(IN) :: cdvars  ! Expected variable names

      !! * Local declarations
      CHARACTER(LEN=12), PARAMETER :: cpname = 'obs_rea_surf'
      CHARACTER(len=8) :: clrefdate
      CHARACTER(len=ilenname), DIMENSION(:),   ALLOCATABLE :: clvarsin
      CHARACTER(len=ilenlong), DIMENSION(:),   ALLOCATABLE :: cllongin
      CHARACTER(len=ilenunit), DIMENSION(:),   ALLOCATABLE :: clunitin
      CHARACTER(len=ilengrid), DIMENSION(:),   ALLOCATABLE :: clgridin
      CHARACTER(len=ilenname), DIMENSION(:),   ALLOCATABLE :: claddvarsin
      CHARACTER(len=ilenlong), DIMENSION(:,:), ALLOCATABLE :: claddlongin
      CHARACTER(len=ilenunit), DIMENSION(:,:), ALLOCATABLE :: claddunitin
      CHARACTER(len=ilenname), DIMENSION(:),   ALLOCATABLE :: clextvarsin
      CHARACTER(len=ilenlong), DIMENSION(:),   ALLOCATABLE :: clextlongin
      CHARACTER(len=ilenunit), DIMENSION(:),   ALLOCATABLE :: clextunitin
      INTEGER :: ji
      INTEGER :: jj
      INTEGER :: jk
      INTEGER :: jind
      INTEGER :: jvar
      INTEGER :: jext
      INTEGER :: jadd
      INTEGER :: jadd2
      INTEGER :: iadd
      INTEGER :: iaddin
      INTEGER :: iextr
      INTEGER :: iflag
      INTEGER :: inobf
      INTEGER :: i_file_id
      INTEGER :: inowin
      INTEGER :: iyea
      INTEGER :: imon
      INTEGER :: iday
      INTEGER :: ihou
      INTEGER :: imin
      INTEGER :: isec
      INTEGER :: itype
      INTEGER :: iobsmpp
      INTEGER :: iobs
      INTEGER :: iobstot
      INTEGER :: ios
      INTEGER :: ioserrcount
      INTEGER, PARAMETER :: jpsurfmaxtype = 1024
      INTEGER, DIMENSION(knumfiles) :: irefdate
      INTEGER, DIMENSION(jpsurfmaxtype+1) :: &
         & ityp, &
         & itypmpp
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: &
         & iobsi,    &
         & iobsj,    &
         & iproc
      INTEGER, DIMENSION(:), ALLOCATABLE :: &
         & iindx,    &
         & ifileidx, &
         & isurfidx
      REAL(wp), DIMENSION(:), ALLOCATABLE :: &
         & zphi, &
         & zlam
      REAL(dp), DIMENSION(:), ALLOCATABLE :: &
         & zdat
      REAL(dp), DIMENSION(knumfiles) ::   dljulini, dljulend
      LOGICAL :: llvalprof
      TYPE(obfbdata), POINTER, DIMENSION(:) :: &
         & inpfiles
      CHARACTER(LEN=256) ::   clout1   ! Diagnostic output line

      ! Local initialization
      iobs = 0

      !-----------------------------------------------------------------------
      ! Count the number of files needed and allocate the obfbdata type
      !-----------------------------------------------------------------------

      inobf = knumfiles

      ALLOCATE( inpfiles(inobf) )

      iadd  = 0
      iextr = 0

      surf_files : DO jj = 1, inobf

         !---------------------------------------------------------------------
         ! Prints
         !---------------------------------------------------------------------
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' obs_rea_surf : Reading from file = ', &
               & TRIM( TRIM( cdfilenames(jj) ) )
            WRITE(numout,*) ' ~~~~~~~~~~~'
            WRITE(numout,*)
         ENDIF

         !---------------------------------------------------------------------
         !  Initialization: Open file and get dimensions only
         !---------------------------------------------------------------------

         iflag = nf90_open( TRIM( TRIM( cdfilenames(jj) ) ), nf90_nowrite, &
            &                      i_file_id )

         IF ( iflag /= nf90_noerr ) THEN

            IF ( ldignmis ) THEN
               inpfiles(jj)%nobs = 0
               CALL ctl_warn( 'File ' // TRIM( TRIM( cdfilenames(jj) ) ) // &
                  &           ' not found' )
            ELSE 
               CALL ctl_stop( 'File ' // TRIM( TRIM( cdfilenames(jj) ) ) // &
                  &           ' not found' )
            ENDIF

         ELSE 

            !------------------------------------------------------------------
            !  Close the file since it is opened in read_obfbdata
            !------------------------------------------------------------------

            iflag = nf90_close( i_file_id )

            !------------------------------------------------------------------
            !  Read the surface file into inpfiles
            !------------------------------------------------------------------
            CALL init_obfbdata( inpfiles(jj) )
            CALL read_obfbdata( TRIM( cdfilenames(jj) ), inpfiles(jj), &
               &                ldgrid = .TRUE. )

            IF ( inpfiles(jj)%nvar /= kvars ) THEN
               CALL ctl_stop( 'Feedback format error: ', &
                  &           ' unexpected number of vars in feedback file', &
                  &           TRIM(cdfilenames(jj)) )
            ENDIF

            IF ( ldmod .AND. ( inpfiles(jj)%nadd == 0 ) ) THEN
               CALL ctl_stop( 'Model not in input data in', &
                  &           TRIM(cdfilenames(jj)) )
               RETURN
            ENDIF

            IF ( (iextr > 0) .AND. (inpfiles(jj)%next /= iextr) ) THEN
               CALL ctl_stop( 'Number of extra variables not consistent', &
                  &           ' with previous files for this type in', &
                  &           TRIM(cdfilenames(jj)) )
            ELSE
               iextr = inpfiles(jj)%next
            ENDIF

            ! Ignore model counterpart
            iaddin = inpfiles(jj)%nadd
            DO ji = 1, iaddin
               IF ( TRIM(inpfiles(jj)%caddname(ji)) == 'Hx' ) THEN
                  iaddin = iaddin - 1
                  EXIT
               ENDIF
            END DO
            IF ( ldmod .AND. ( inpfiles(jj)%nadd == iaddin ) ) THEN
               CALL ctl_stop( 'Model not in input data', &
                  &           TRIM(cdfilenames(jj)) )
            ENDIF

            IF ( (iadd > 0) .AND. (iaddin /= iadd) ) THEN
               CALL ctl_stop( 'Number of additional variables not consistent', &
                  &           ' with previous files for this type in', &
                  &           TRIM(cdfilenames(jj)) )
            ELSE
               iadd = iaddin
            ENDIF

            IF ( jj == 1 ) THEN
               ALLOCATE( clvarsin( inpfiles(jj)%nvar ) )
               ALLOCATE( cllongin( inpfiles(jj)%nvar ) )
               ALLOCATE( clunitin( inpfiles(jj)%nvar ) )
               ALLOCATE( clgridin( inpfiles(jj)%nvar ) )
               DO ji = 1, inpfiles(jj)%nvar
                 clvarsin(ji) = inpfiles(jj)%cname(ji)
                 cllongin(ji) = inpfiles(jj)%coblong(ji)
                 clunitin(ji) = inpfiles(jj)%cobunit(ji)
                 clgridin(ji) = inpfiles(jj)%cgrid(ji)
                 IF ( clvarsin(ji) /= cdvars(ji) ) THEN
                    CALL ctl_stop( 'Feedback file variables do not match', &
                        &           ' expected variable names for this type in', &
                        &           TRIM(cdfilenames(jj)) )
                 ENDIF
               END DO
               IF ( iadd > 0 ) THEN
                  ALLOCATE( claddvarsin( iadd ) )
                  ALLOCATE( claddlongin( iadd, inpfiles(jj)%nvar ) )
                  ALLOCATE( claddunitin( iadd, inpfiles(jj)%nvar ) )
                  jadd = 0
                  DO ji = 1, inpfiles(jj)%nadd
                    IF ( TRIM(inpfiles(jj)%caddname(ji)) /= 'Hx' ) THEN
                       jadd = jadd + 1
                       claddvarsin(jadd) = inpfiles(jj)%caddname(ji)
                       DO jk = 1, inpfiles(jj)%nvar
                          claddlongin(jadd,jk) = inpfiles(jj)%caddlong(ji,jk)
                          claddunitin(jadd,jk) = inpfiles(jj)%caddunit(ji,jk)
                       END DO
                    ENDIF
                  END DO
               ENDIF
               IF ( iextr > 0 ) THEN
                  ALLOCATE( clextvarsin( iextr ) )
                  ALLOCATE( clextlongin( iextr ) )
                  ALLOCATE( clextunitin( iextr ) )
                  DO ji = 1, iextr
                    clextvarsin(ji) = inpfiles(jj)%cextname(ji)
                    clextlongin(ji) = inpfiles(jj)%cextlong(ji)
                    clextunitin(ji) = inpfiles(jj)%cextunit(ji)
                  END DO
               ENDIF
            ELSE
               DO ji = 1, inpfiles(jj)%nvar
                  IF ( inpfiles(jj)%cname(ji) /= clvarsin(ji) ) THEN
                     CALL ctl_stop( 'Feedback file variables not consistent', &
                        &           ' with previous files for this type in', &
                        &           TRIM(cdfilenames(jj)) )
                  ENDIF
               END DO
               IF ( iadd > 0 ) THEN
                  jadd = 0
                  DO ji = 1, inpfiles(jj)%nadd
                     IF ( TRIM(inpfiles(jj)%caddname(ji)) /= 'Hx' ) THEN
                        jadd = jadd + 1
                        IF ( inpfiles(jj)%caddname(ji) /= claddvarsin(jadd) ) THEN
                           CALL ctl_stop( 'Feedback file additional variables not consistent', &
                              &           ' with previous files for this type in', &
                              &           TRIM(cdfilenames(jj)) )
                        ENDIF
                     ENDIF
                  END DO
               ENDIF
               IF ( iextr > 0 ) THEN
                  DO ji = 1, iextr
                     IF ( inpfiles(jj)%cextname(ji) /= clextvarsin(ji) ) THEN
                        CALL ctl_stop( 'Feedback file extra variables not consistent', &
                           &           ' with previous files for this type in', &
                           &           TRIM(cdfilenames(jj)) )
                     ENDIF
                  END DO
               ENDIF

            ENDIF

            IF (lwp) WRITE(numout,*) 'Observation file contains ', inpfiles(jj)%nobs, ' observations'

            !------------------------------------------------------------------
            !  Change longitude (-180,180)
            !------------------------------------------------------------------

            DO ji = 1, inpfiles(jj)%nobs

               IF ( inpfiles(jj)%plam(ji) < -180. ) &
                  &   inpfiles(jj)%plam(ji) = inpfiles(jj)%plam(ji) + 360.

               IF ( inpfiles(jj)%plam(ji) >  180. ) &
                  &   inpfiles(jj)%plam(ji) = inpfiles(jj)%plam(ji) - 360.

            END DO

            !------------------------------------------------------------------
            !  Calculate the date  (change eventually)
            !------------------------------------------------------------------
            clrefdate=inpfiles(jj)%cdjuldref(1:8)
            READ(clrefdate,'(I8)') irefdate(jj)

            CALL ddatetoymdhms( ddobsini, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, dljulini(jj), &
               &           krefdate = irefdate(jj) )
            CALL ddatetoymdhms( ddobsend, iyea, imon, iday, ihou, imin, isec )
            CALL greg2jul( isec, imin, ihou, iday, imon, iyea, dljulend(jj), &
               &           krefdate = irefdate(jj) )

            IF ( ldnightav ) THEN

               IF ( lwp ) THEN
                  WRITE(numout,*)'Resetting time of night-time averaged observations', &
                     &             ' to the end of the day'
               ENDIF

               DO ji = 1, inpfiles(jj)%nobs
                  !  for night-time averaged data force the time
                  !  to be the last time-step of the day, but still within the day.
                  IF ( inpfiles(jj)%ptim(ji) >= 0. ) THEN
                     inpfiles(jj)%ptim(ji) = &
                        & INT(inpfiles(jj)%ptim(ji)) + 0.9999
                  ELSE
                     inpfiles(jj)%ptim(ji) = &
                        & INT(inpfiles(jj)%ptim(ji)) - 0.0001
                  ENDIF
               END DO
            ENDIF

            IF ( inpfiles(jj)%nobs > 0 ) THEN
               inpfiles(jj)%iproc(:,:) = -1
               inpfiles(jj)%iobsi(:,:) = -1
               inpfiles(jj)%iobsj(:,:) = -1
            ENDIF

            ! If observations are representing a time mean then set the time
            ! of the obs to the end of that meaning period relative to the start of the run
            IF ( ld_time_mean_bkg ) THEN
               DO ji = 1, inpfiles(jj)%nobs
                  ! Only do this for obs within time window
                  IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND. &
                     & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) ) ) THEN
                     inpfiles(jj)%ptim(ji) = dljulini(jj) + ( ptime_mean_period / 24.0_wp )
                  ENDIF
               END DO
            ENDIF

            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) )       ) THEN
                  inowin = inowin + 1
               ENDIF
            END DO
            ALLOCATE( zlam (inowin)       )
            ALLOCATE( zphi (inowin)       )
            ALLOCATE( iobsi(inowin,kvars) )
            ALLOCATE( iobsj(inowin,kvars) )
            ALLOCATE( iproc(inowin,kvars) )
            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) )       ) THEN
                  inowin = inowin + 1
                  zlam(inowin) = inpfiles(jj)%plam(ji)
                  zphi(inowin) = inpfiles(jj)%pphi(ji)
               ENDIF
            END DO

            ! Do grid search
            ! Assume anything other than velocity is on T grid
            ! Save resource by not repeating for variables on the same grid
            jind = 0
            DO jvar = 1, kvars
               IF ( TRIM(inpfiles(jj)%cname(jvar)) == cobsname_uvel ) THEN
                  CALL obs_grid_search( inowin, zlam, zphi, iobsi(:,jvar), iobsj(:,jvar), &
                     &                  iproc(:,jvar), 'U' )
               ELSE IF ( TRIM(inpfiles(jj)%cname(jvar)) == cobsname_vvel ) THEN
                  CALL obs_grid_search( inowin, zlam, zphi, iobsi(:,jvar), iobsj(:,jvar), &
                     &                  iproc(:,jvar), 'V' )
               ELSE
                  IF ( jind > 0 ) THEN
                     iobsi(:,jvar) = iobsi(:,jind)
                     iobsj(:,jvar) = iobsj(:,jind)
                     iproc(:,jvar) = iproc(:,jind)
                  ELSE
                     jind = jvar
                     CALL obs_grid_search( inowin, zlam, zphi, iobsi(:,jvar), iobsj(:,jvar), &
                        &                  iproc(:,jvar), 'T' )
                  ENDIF
               ENDIF
            END DO

            inowin = 0
            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) )       ) THEN
                  inowin = inowin + 1
                  DO jvar = 1, kvars
                     inpfiles(jj)%iproc(ji,jvar) = iproc(inowin,jvar)
                     inpfiles(jj)%iobsi(ji,jvar) = iobsi(inowin,jvar)
                     inpfiles(jj)%iobsj(ji,jvar) = iobsj(inowin,jvar)
                  END DO
               ENDIF
            END DO
            DEALLOCATE( zlam, zphi, iobsi, iobsj, iproc )

            DO ji = 1, inpfiles(jj)%nobs
               IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND. &
                  & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) )       ) THEN
                  IF ( narea == 1 ) THEN
                     IF ( inpfiles(jj)%iproc(ji,1) >  narea-1 ) CYCLE
                  ELSE
                     IF ( inpfiles(jj)%iproc(ji,1) /= narea-1 ) CYCLE
                  ENDIF
                  llvalprof = .FALSE.
                  IF ( .NOT. BTEST(inpfiles(jj)%ivlqc(1,ji,1),2) ) THEN
                     iobs = iobs + 1
                  ENDIF
               ENDIF
            END DO

         ENDIF

      END DO surf_files

      !-----------------------------------------------------------------------
      ! Get the time ordered indices of the input data
      !-----------------------------------------------------------------------

      !---------------------------------------------------------------------
      !  Loop over input data files to count total number of profiles
      !---------------------------------------------------------------------
      iobstot = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) )       ) THEN
               iobstot = iobstot + 1
            ENDIF
         END DO
      END DO

      ALLOCATE( iindx(iobstot), ifileidx(iobstot), &
         &      isurfidx(iobstot), zdat(iobstot) )
      jk = 0
      DO jj = 1, inobf
         DO ji = 1, inpfiles(jj)%nobs
            IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND. &
               & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) )       ) THEN
               jk = jk + 1
               ifileidx(jk) = jj
               isurfidx(jk) = ji
               zdat(jk)     = inpfiles(jj)%ptim(ji)
            ENDIF
         END DO
      END DO
      CALL sort_dp_indx( iobstot, &
         &               zdat,     &
         &               iindx   )

      CALL obs_surf_alloc( surfdata, iobs, kvars, kadd+iadd, kextr+iextr, kstp, jpi, jpj )

      ! Read obs/positions, QC, all variable and assign to surfdata

      iobs = 0

      surfdata%cvars(:)  = clvarsin(:)
      surfdata%clong(:)  = cllongin(:)
      surfdata%cunit(:)  = clunitin(:)
      surfdata%cgrid(:)  = clgridin(:)
      IF ( iadd > 0 ) THEN
         surfdata%caddvars(kadd+1:)   = claddvarsin(:)
         surfdata%caddlong(kadd+1:,:) = claddlongin(:,:)
         surfdata%caddunit(kadd+1:,:) = claddunitin(:,:)
      ENDIF
      IF ( iextr > 0 ) THEN
         surfdata%cextvars(kextr+1:) = clextvarsin(:)
         surfdata%cextlong(kextr+1:) = clextlongin(:)
         surfdata%cextunit(kextr+1:) = clextunitin(:)
      ENDIF

      ityp   (:) = 0
      itypmpp(:) = 0

      ioserrcount = 0

      DO jk = 1, iobstot

         jj = ifileidx(iindx(jk))
         ji = isurfidx(iindx(jk))
         IF ( ( inpfiles(jj)%ptim(ji) >  dljulini(jj) ) .AND.  &
            & ( inpfiles(jj)%ptim(ji) <= dljulend(jj) ) ) THEN

            IF ( narea == 1 ) THEN
               IF ( inpfiles(jj)%iproc(ji,1) >  narea-1 ) CYCLE
            ELSE
               IF ( inpfiles(jj)%iproc(ji,1) /= narea-1 ) CYCLE
            ENDIF

            ! Set observation information

            IF ( .NOT. BTEST(inpfiles(jj)%ivlqc(1,ji,1),2) ) THEN

               iobs = iobs + 1

               CALL jul2greg( isec,                   &
                  &           imin,                   &
                  &           ihou,                   &
                  &           iday,                   &
                  &           imon,                   &
                  &           iyea,                   &
                  &           inpfiles(jj)%ptim(ji), &
                  &           irefdate(jj) )


               ! Surface time coordinates
               surfdata%nyea(iobs) = iyea
               surfdata%nmon(iobs) = imon
               surfdata%nday(iobs) = iday
               surfdata%nhou(iobs) = ihou
               surfdata%nmin(iobs) = imin

               ! Surface space coordinates
               surfdata%rlam(iobs) = inpfiles(jj)%plam(ji)
               surfdata%rphi(iobs) = inpfiles(jj)%pphi(ji)

               ! Coordinate search parameters
               DO jvar = 1, kvars
                  surfdata%mi(iobs,jvar) = inpfiles(jj)%iobsi(ji,jvar)
                  surfdata%mj(iobs,jvar) = inpfiles(jj)%iobsj(ji,jvar)
               END DO

               ! WMO number
               surfdata%cwmo(iobs) = inpfiles(jj)%cdwmo(ji)

               ! Instrument type
               READ( inpfiles(jj)%cdtyp(ji), '(I4)', IOSTAT = ios, ERR = 901 ) itype
901            IF ( ios /= 0 ) THEN
                  IF (ioserrcount == 0) THEN
                     CALL ctl_warn ( 'Problem converting an instrument type ', &
                        &            'to integer. Setting type to zero' )
                  ENDIF
                  ioserrcount = ioserrcount + 1
                  itype = 0
               ENDIF
               surfdata%ntyp(iobs) = itype
               IF ( itype < jpsurfmaxtype + 1 ) THEN
                  ityp(itype+1) = ityp(itype+1) + 1
               ELSE
                  CALL ctl_warn ( 'Increase jpsurfmaxtype in ', &
                     &            cpname )
               ENDIF

               ! Bookkeeping data to match observations
               surfdata%nsidx(iobs) = iobs
               surfdata%nsfil(iobs) = iindx(jk)

               DO jvar = 1, kvars

                  ! QC flags
                  surfdata%nqc(iobs) = inpfiles(jj)%ivqc(ji,jvar)

                  ! Observed value
                  surfdata%robs(iobs,jvar) = inpfiles(jj)%pob(1,ji,jvar)

                  ! Additional variables
                  surfdata%rmod(iobs,jvar) = fbrmdi
                  IF ( iadd > 0 ) THEN
                     jadd2 = 0
                     DO jadd = 1, inpfiles(jj)%nadd
                        IF ( TRIM(inpfiles(jj)%caddname(jadd)) == 'Hx' ) THEN
                           IF ( ldmod ) THEN
                              surfdata%rmod(iobs,jvar) = inpfiles(jj)%padd(1,ji,jadd,jvar)
                           ENDIF
                        ELSE
                           jadd2 = jadd2 + 1
                           surfdata%radd(iobs,kadd+jadd2,jvar) = &
                              &                inpfiles(jj)%padd(1,ji,jadd,jvar)
                        ENDIF
                     END DO
                  ENDIF

               END DO
                  
               ! Extra variables
               IF ( iextr > 0 ) THEN
                  DO jext = 1, iextr
                     surfdata%rext(iobs,kextr+jext) = inpfiles(jj)%pext(1,ji,jext)
                  END DO
               ENDIF
            ENDIF
         ENDIF

      END DO

      !-----------------------------------------------------------------------
      ! Sum up over processors
      !-----------------------------------------------------------------------

      CALL obs_mpp_sum_integer( iobs, iobsmpp )
      CALL obs_mpp_sum_integers( ityp, itypmpp, jpsurfmaxtype + 1 )

      !-----------------------------------------------------------------------
      ! Output number of observations.
      !-----------------------------------------------------------------------
      IF (lwp) THEN
         DO jvar = 1, surfdata%nvar       
            IF ( jvar == 1 ) THEN
               clout1=TRIM(surfdata%cvars(1))
            ELSE
               WRITE(clout1,'(A,A1,A)') TRIM(clout1), '/', TRIM(surfdata%cvars(jvar))
            ENDIF
         END DO
 
         WRITE(numout,*)
         WRITE(numout,'(1X,A)')TRIM( clout1 )//' data'
         WRITE(numout,'(1X,A)')'--------------'
         DO jj = 1,8
            IF ( itypmpp(jj) > 0 ) THEN
               WRITE(numout,'(1X,A4,I4,A3,I10)') 'Type ', jj, ' = ', itypmpp(jj)
            ENDIF
         END DO
         WRITE(numout,'(1X,A)') &
            & '---------------------------------------------------------------'
         WRITE(numout,'(1X,A,I8)') &
            & 'Total data for variable '//TRIM( clout1 )// &
            & '           = ', iobsmpp
         WRITE(numout,'(1X,A)') &
            & '---------------------------------------------------------------'
         WRITE(numout,*)

      ENDIF

      !-----------------------------------------------------------------------
      ! Deallocate temporary data
      !-----------------------------------------------------------------------
      DEALLOCATE( ifileidx, isurfidx, zdat, clvarsin, &
         &        cllongin, clunitin, clgridin )
      IF ( iadd > 0 ) THEN
         DEALLOCATE( claddvarsin, claddlongin, claddunitin)
      ENDIF
      IF ( iextr > 0 ) THEN
         DEALLOCATE( clextvarsin, clextlongin, clextunitin )
      ENDIF

      !-----------------------------------------------------------------------
      ! Deallocate input data
      !-----------------------------------------------------------------------
      DO jj = 1, inobf
         IF ( inpfiles(jj)%lalloc ) THEN
            CALL dealloc_obfbdata( inpfiles(jj) )
         ENDIF
      END DO
      DEALLOCATE( inpfiles )

   END SUBROUTINE obs_rea_surf

END MODULE obs_read_surf
