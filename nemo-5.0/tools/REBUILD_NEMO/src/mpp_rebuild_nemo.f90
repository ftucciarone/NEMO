
PROGRAM mpp_rebuild_nemo

   !!=========================================================================
   !!                        ***  rebuild_nemo  ***
   !!=========================================================================
   !!
   !!  A routine to rebuild NEMO files from multiple processors into one file.
   !!  This routine is designed to be much quicker than the old IOIPSL rebuild
   !!  but at the cost of an increased memory usage.
   !!
   !!  NEMO rebuild has the following features:
   !!     * dynamically works out what variables require rebuilding
   !!     * does not copy subdomain halo regions
   !!     * works for 1,2,3,4 and 5d arrays or types for all valid NetCDF types
   !!     * utilises MPI parallelisation (1 writer and multiple readers)
   !!     * time 'slicing' for lower memory use 
   !!       (only for 4D vars with unlimited dimension)
   !!
   !!  History: Ed Blockley - August 2011
   !!           (based on original code by Matt Martin)
   !!           Julien Palmieri and Andrew Coward - September 2018 (add compression and chunking)
   !!           Mireck - 2019 (?) : orignal version of the mpp code
   !!           P. Mathiot - 2023 : cleaning and remove open mp option
   !!           
   !!
   !!-------------------------------------------------------------------------
   !!
   !!  The code reads the filestem and number of subdomains from the namelist file nam_rebuild.
   !! 
   !!  The 1st subdomain file is used to determine the dimensions and variables in all the input files. 
   !!  It is also used to find which dimensions (and hence which variables) require rebuilding 
   !!  as well as information about the global domain.
   !!
   !!  It then opens all the input files (unbuffered) and creates an array of netcdf identifiers
   !!  before looping through all the variables and updating the rebuilt output file (either by direct 
   !!  copying or looping over the number of domains and rebuilding as appropriate).
   !!  
   !!  The code looks more complicated than it is because it has lots of case statements to deal with all 
   !!  the various NetCDF data types and with various data dimensions (up to 4d).
   !!
   !!  Diagnostic output is written to numout (default 6 - stdout)
   !!  and errors are written to numerr (default 0 - stderr).
   !!
   !!  If time slicing is specified the code will use less memory but take a little longer.
   !!  It does this by breaking down the 4D input variables over their 4th dimension 
   !!  (generally time) by way of a while loop.
   !!
   !!-------------------------------------------------------------------------------
 
   USE netcdf
   USE mpi
   !USE mpi_f08

   IMPLICIT NONE

   ! kind specifications
   INTEGER,PARAMETER :: i1=SELECTED_INT_KIND(2)          ! NF90_BYTE 
   INTEGER,PARAMETER :: i2=SELECTED_INT_KIND(4)          ! NF90_SHORT
   INTEGER,PARAMETER :: i4=SELECTED_INT_KIND(9)          ! NF90_INT
   INTEGER,PARAMETER :: sp=SELECTED_REAL_KIND(6,37)      ! NF90_FLOAT
   INTEGER,PARAMETER :: dp=SELECTED_REAL_KIND(12,307)    ! NF90_DOUBLE

   INTEGER,PARAMETER :: numnam = 11
   INTEGER,PARAMETER :: numout = 6
   INTEGER,PARAMETER :: numerr = 0
   LOGICAL :: l_verbose = .true. 

   ! common variables
   CHARACTER(LEN=nf90_max_name), ALLOCATABLE :: indimnames(:)
   CHARACTER(LEN=nf90_max_name), DIMENSION(2) :: dims
   INTEGER, ALLOCATABLE  :: outdimids(:), outdimlens(:), indimlens(:), inncids(:)
   INTEGER, ALLOCATABLE  :: chunksizes(:)
   INTEGER, ALLOCATABLE  :: global_sizes(:), rebuild_dims(:)
   INTEGER, DIMENSION(2) :: halo_start, halo_end, local_sizes
   INTEGER  :: ncid

   ! timestamp attribute
   LOGICAL  :: l_timestamp = .false. ! flag to add timestamp attribute
   CHARACTER(LEN=nf90_max_name) :: time, date, zone, timestamp

   ! min/max management
   LOGICAL  :: l_valid = .false.   ! flag to compute global valid min/max
   REAL(sp) :: ValMin, ValMax      ! global min / max
   REAL(sp) :: InMin , InMax       ! local  min / max
   REAL(sp) :: sr_data(2)          ! buffer of min/max value sent from reader to writer

   ! missing data management
   LOGICAL :: l_maskout = .false.  ! flag to set to 0 missing domains
   REAL(dp), DIMENSION(:), ALLOCATABLE :: mdiVals    ! array of missing values (size nvars)
   REAL(sp) :: rmdi                ! variable misssing value

   ! global arrays
   ! NF90_BYTE global data arrays
   INTEGER(i1) :: globaldata_0d_i1
   INTEGER(i1), ALLOCATABLE, DIMENSION(:) :: globaldata_1d_i1
   INTEGER(i1), ALLOCATABLE, DIMENSION(:,:) :: globaldata_2d_i1
   INTEGER(i1), ALLOCATABLE, DIMENSION(:,:,:) :: globaldata_3d_i1
   INTEGER(i1), ALLOCATABLE, DIMENSION(:,:,:,:) :: globaldata_4d_i1
   INTEGER(i1), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: globaldata_5d_i1

   ! NF90_SHORT global data arrays
   INTEGER(i2) :: globaldata_0d_i2
   INTEGER(i2), ALLOCATABLE, DIMENSION(:) :: globaldata_1d_i2
   INTEGER(i2), ALLOCATABLE, DIMENSION(:,:) :: globaldata_2d_i2
   INTEGER(i2), ALLOCATABLE, DIMENSION(:,:,:) :: globaldata_3d_i2
   INTEGER(i2), ALLOCATABLE, DIMENSION(:,:,:,:) :: globaldata_4d_i2
   INTEGER(i2), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: globaldata_5d_i2

   ! NF90_INT global data arrays
   INTEGER(i4) :: globaldata_0d_i4
   INTEGER(i4), ALLOCATABLE, DIMENSION(:) :: globaldata_1d_i4
   INTEGER(i4), ALLOCATABLE, DIMENSION(:,:) :: globaldata_2d_i4
   INTEGER(i4), ALLOCATABLE, DIMENSION(:,:,:) :: globaldata_3d_i4
   INTEGER(i4), ALLOCATABLE, DIMENSION(:,:,:,:) :: globaldata_4d_i4
   INTEGER(i4), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: globaldata_5d_i4
 
   ! NF90_FLOAT global data arrays
   REAL(sp) :: globaldata_0d_sp
   REAL(sp), ALLOCATABLE, DIMENSION(:) :: globaldata_1d_sp
   REAL(sp), ALLOCATABLE, DIMENSION(:,:) :: globaldata_2d_sp
   REAL(sp), ALLOCATABLE, DIMENSION(:,:,:) :: globaldata_3d_sp
   REAL(sp), ALLOCATABLE, DIMENSION(:,:,:,:) :: globaldata_4d_sp
   REAL(sp), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: globaldata_5d_sp

   ! NF90_DOUBLE global data arrays
   REAL(dp) :: globaldata_0d_dp
   REAL(dp), ALLOCATABLE, DIMENSION(:) :: globaldata_1d_dp
   REAL(dp), ALLOCATABLE, DIMENSION(:,:) :: globaldata_2d_dp
   REAL(dp), ALLOCATABLE, DIMENSION(:,:,:) :: globaldata_3d_dp
   REAL(dp), ALLOCATABLE, DIMENSION(:,:,:,:) :: globaldata_4d_dp
   REAL(dp), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: globaldata_5d_dp

   ! misc. variables 
   CHARACTER(LEN=nf90_max_name) :: attname, dimname, varname
   INTEGER, ALLOCATABLE, DIMENSION(:) :: varids
   INTEGER :: ndomain, ndomain_file
   INTEGER :: outid, idim, istop
   INTEGER :: natts, attid, xtype, varid, rbdims 
   INTEGER :: jv, ndims, nvars, dimlen, dimids(5)
   INTEGER :: dimid, unlimitedDimId, di, dj, dr
   INTEGER :: nmax_unlimited, nt, ntslice,len_timeframe
   INTEGER :: ntslice_max = 1  ! max number of time slice that fit in a MPI message
   INTEGER :: memRSS
   INTEGER :: ji, jj, jk, jl, jr
   INTEGER :: n_noRebuild = 0        ! integer flag to check dataset contains rebuild dimensions
   LOGICAL :: l_noRebuild = .false.  ! flag to flag variable that doesn't need to be rebuild
   LOGICAL :: l_findDims  = .true.   
   LOGICAL :: l_namexist  = .false.  ! flag when namelist missing
   LOGICAL :: ltimevar    = .false.  ! used to define time dimension and define variable len without time
   CHARACTER(LEN=6) :: format   

   CHARACTER(LEN=256) :: cnampath, cdimlst, cdim

   ! namelist variable definition
   INTEGER :: nslicesize = 0         !
   INTEGER :: ndigits = 4            ! number of digit used for the domain number input file
   INTEGER :: fchunksize = 32000000  ! NetCDF global file chunk cache size
   INTEGER :: patchchunk             ! NetCDF processor-domain file chunk cache size
   INTEGER :: nthreads = 1
   INTEGER :: chunkalg = 0           ! NetCDF4 variable chunking algorithm
                                     ! Default variable chunksizes (typical ORCA025
                                     ! recommendations which can be adjusted via namelist
                                     ! or will be bounded if too large for domain.)
   INTEGER :: nc4_xchunk = 206       ! Default x (longitude) variable chunk size
   INTEGER :: nc4_ychunk = 135       ! Default y (latitude)  variable chunk size
   INTEGER :: nc4_zchunk = 1         ! Default z (depth) variable chunk size (almost always 1)
   INTEGER :: nc4_tchunk = 1         ! Default t (time)  variable chunk size (almost always 1)
   INTEGER :: deflate_level = 1      ! Default compression level

   ! input file name management
   CHARACTER(LEN=nf90_max_name), DIMENSION(:), ALLOCATABLE :: filenames ! domain filename list
   CHARACTER(LEN=nf90_max_name) :: filebase, suffix  ! filebase (restart_ice) and suffix (_0000.nc) needed 
                                                     ! to define each domain filename
   INTEGER :: ifile
   INTEGER :: ifstart=1              ! Default offset for file suffix. Setting a namelist value > 1 will associate
                                     ! this file with the first rank (used to determine dimensions and variables)
                                     ! The rest of the ndomain files will be assigned sequentially, cycling
                                     ! through 0 to ifstart-1 after ndomain. This is useful if the *_0000.nc 
                                     ! file does not contain a representative set of variables or dimensions; as
                                     ! can happen for icb (iceberg) restarts. In this case, it it best to start
                                     ! with a equatorial domain that contains only the 2D fields that need to
                                     ! be collated. The iceberg trajectory information can be added later by the
                                     ! icb_combrst.py script
  
   ! txt file output management
   CHARACTER(256)    :: fclient,cmype   ! client_cmype.txt output
   CHARACTER(16)     :: fmt_cmype       ! adjustable zero padded format string
   integer           :: ndigit          ! digits required for suffix

   ! MPI management variable
   INTEGER  :: ierror,nproc,mype   ! error id, total MPI, rank number
   REAL(dp) :: send_time           ! time spend to send data
   INTEGER  :: NMAX_VARS
   INTEGER  :: bdata(3)            ! buffer for broadcasting scalar
   INTEGER  :: mpi_count_value      ! type used to define max size of the buffer and so ntslice_max
   !INTEGER(MPI_COUNT_KIND) :: mpi_count_value ! Should be used for MPI-3 but not tested (allow bigger buffer)

   ! namelist management
   EXTERNAL          :: getarg
   INTEGER, EXTERNAL :: iargc
   INTEGER           :: nargs = 0
   CHARACTER(256)    :: namelist_path,sys_str
 
   NAMELIST/nam_rebuild/ filebase, ndomain, dims, nslicesize, l_maskout, deflate_level,       &
                       & nc4_xchunk, nc4_ychunk, nc4_zchunk, nc4_tchunk, fchunksize, ndigits, &
                       & l_timestamp, ifstart

   ! Initialize MPI
   CALL MPI_Init ( ierror )
   CALL MPI_Comm_size ( MPI_COMM_WORLD, nproc, ierror )
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, mype , ierror )

   IF(nproc < 2) THEN 
      CALL MPI_Finalize(ierror)
      STOP 'NEED more than 1 processor'
   ENDIF
!
!--------------------------------------------------------------------------------
!1.1 Get the namelist path
   !Determine the number of arguments on the command line
   nargs=COMMAND_ARGUMENT_COUNT()
   !Check that the required argument is present, if it is not then set it to the default value: nam_rebuild
   IF (nargs == 0) THEN
      WRITE(numout,*)
      WRITE(numout,*) 'W A R N I N G : Namelist path not supplied as command line argument. Using default, nam_rebuild.'
      cnampath='nam_rebuild'
   ELSE IF (nargs == 1) THEN
      CALL GET_COMMAND_ARGUMENT(1, cnampath)
   ELSE 
      WRITE(numerr,*) 'E R R O R ! : Incorrect number of command line arguments. Please supply only'
      WRITE(numerr,*) '         the path to the namelist file, or no arguments to use default value'
      STOP 1
   END IF

   ! check presence of namelist
   INQUIRE(FILE=cnampath, EXIST=l_namexist)
   IF (.NOT. l_namexist) THEN
      WRITE(numout,*)
      WRITE(numout,*) 'E R R O R : Namelist '//TRIM(cnampath)//' not present.'
      STOP 42
   END IF

!1.2 Read in the namelist 

   dims(:) = ""
   OPEN( UNIT=numnam, FILE=TRIM(cnampath), FORM='FORMATTED', ACTION = 'READ' , STATUS='OLD' )
   READ( numnam, nam_rebuild )
   CLOSE( numnam )
   IF( .NOT. ALL(dims(:) == "") ) l_findDims = .false.

!1.3 Set up the filenames and fileids
   !
   ALLOCATE(filenames(ndomain))
   l_verbose = l_verbose .AND. (mype .EQ. 0)
   IF (l_verbose) WRITE(numout,*) 'Rebuilding the following files:'
   !
   IF( ndigits.eq.4) THEN
     format = "(i4.4)"
   ELSE IF(ndigits.eq.5) THEN
     format = "(i5.5)"
   ELSE
     STOP 'ndigits'
   ENDIF
   !
   ifstart=MAX(1, ifstart)  ! Ensure ifstart >= 1
   DO ifile = 1, ndomain
      WRITE(suffix,format) MOD(ifile-1+ifstart,ndomain)
      filenames(ifile) = TRIM(filebase)//'_'//TRIM(suffix)//'.nc'
      IF (l_verbose) WRITE(numout,*) TRIM(filenames(ifile))
   END DO
   ALLOCATE(inncids(ndomain))
 
!---------------------------------------------------------------------------
!2. Open input files

   ! Set a file chunk cache size for the processor-domain files that scales with the number of processors
   patchchunk = max(8192, fchunksize/ndomain)

   IF(mype.ne.0) THEN
      DO ifile = 2, ndomain
         CALL check_nf90( nf90_open( TRIM(filenames(ifile)), nf90_share, inncids(ifile), chunksize = patchchunk ),'0: Open' )
      ENDDO    
   ENDIF
   CALL check_nf90( nf90_open( TRIM(filenames(1)), nf90_share, ncid, chunksize = patchchunk ),'Open' )
   inncids(1) = ncid

   IF(mype.eq.0) THEN
      CALL check_nf90( nf90_inquire( ncid, ndims, nvars, natts ),'0: Inq' )
      IF( nvars < nproc-1 ) THEN
         WRITE(numerr,*) 'ERROR! : More readers are defined than variables'
         WRITE(numerr,*) 'Please try again with ', nvars+1, ' or fewer tasks'
         STOP 11
      ENDIF
      bdata(1) = nvars
      bdata(2) = ndims
      bdata(3) = natts 
   ENDIF

   call MPI_Bcast(bdata, 3, MPI_INT, 0, MPI_COMM_WORLD, ierror)

   if(mype.ne.0) then
      nvars = bdata(1)
      ndims = bdata(2)
      natts = bdata(3)  
   endif
   NMAX_VARS = nvars

   ! try read from different file
   if(nproc.lt.ndomain) ncid = inncids(mype+1)

!2.0 Read in the total number of processors the file is expecting and check it's correct  
   if(mype.eq.0) CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_number_total', ndomain_file ),'0:DOMAIN' )
   call MPI_Bcast(ndomain_file, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)

   IF( ndomain /= ndomain_file ) THEN
      WRITE(numerr,*) 'ERROR! : number of files to rebuild in file does not agree with namelist'
      WRITE(numerr,*) 'Attribute DOMAIN_number_total is : ', ndomain_file
      WRITE(numerr,*) 'Number of files specified in namelist is: ', ndomain
      STOP 2
   ENDIF
  
!2.1 Set up the output file
   IF(mype.eq.0) THEN   
      CALL check_nf90( nf90_create( TRIM(filebase)//'.nc', nf90_netcdf4, outid, chunksize=fchunksize ),'0:create' )
   ENDIF

!2.2 Set up dimensions in output file
   !
   !2.2.0 Find out how many dimensions are required to be rebuilt and which ones they are
   if(mype.eq.0) CALL check_nf90( nf90_inquire_attribute( ncid, nf90_global, 'DOMAIN_dimensions_ids', xtype, rbdims, attid),'0:inq_att' )
   call MPI_Bcast(rbdims, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)

   ALLOCATE(rebuild_dims(rbdims))
   ALLOCATE(global_sizes(rbdims))

   if(mype.eq.0) then
      CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_dimensions_ids', rebuild_dims ),'0:Domain' )
      CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_size_global', global_sizes ),'0: DOMAIN' )
   endif

   call MPI_Bcast(rebuild_dims, rbdims, MPI_INT, 0, MPI_COMM_WORLD, ierror)
   call MPI_Bcast(global_sizes, rbdims, MPI_INT, 0, MPI_COMM_WORLD, ierror)

   IF (l_verbose) WRITE(numout,*) 'Size of global arrays: ', global_sizes

   !2.2.1 Copy the dimensions into the output file apart from rebuild_dims() which are dimensioned globally 
   ALLOCATE(indimlens(ndims), indimnames(ndims), outdimlens(ndims))
   CALL check_nf90( nf90_inquire( ncid, unlimitedDimId = unlimitedDimId ),'Inq' )
   istop = 0
   DO idim = 1, ndims
      CALL check_nf90( nf90_inquire_dimension( ncid, idim, dimname, dimlen ),'Inq_dim' )
      CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_size_local', local_sizes ),'get_att' )
      indimlens(idim) = dimlen    
      indimnames(idim) = dimname
      IF (l_findDims) THEN
         IF( idim == rebuild_dims(1) ) THEN
            IF( dimlen == local_sizes(1) ) THEN 
               dimlen = global_sizes(1)
               dims(1) = trim(dimname)
            ELSE
               istop = 1
            ENDIF
         ENDIF
         IF( rbdims > 1 .AND. idim == rebuild_dims(2) ) THEN
            IF( dimlen == local_sizes(2) ) THEN
               dimlen = global_sizes(2)
               dims(2) = trim(dimname)
            ELSE
               istop = 1
            ENDIF
         ENDIF
      ELSE ! l_findDims = false
         IF( TRIM(dimname) == TRIM(dims(1))) THEN
            dimlen = global_sizes(1)
            rebuild_dims(1) = idim
         ENDIF
         IF( rbdims > 1 .AND. TRIM(dimname) == TRIM(dims(2))) THEN
            dimlen = global_sizes(2)
            rebuild_dims(2) = idim
         ENDIF
      ENDIF

      IF( idim == unlimitedDimId ) THEN
         IF(mype.eq.0) CALL check_nf90( nf90_def_dim( outid, dimname, nf90_unlimited, dimid),'def_dim' )
         nmax_unlimited = dimlen
      ELSE
         IF(mype.eq.0) CALL check_nf90( nf90_def_dim( outid, dimname, dimlen, dimid),'def_dim' )
      ENDIF
      outdimlens(idim) = dimlen
   END DO
   ! nmax_unlimited is only used for time-slicing so we set it to be at least 1 to 
   ! account for files with no record dimension or zero length record dimension(!)
   nmax_unlimited = max(nmax_unlimited,1)

   IF( istop == 1 ) THEN
      WRITE(numerr,*) 'ERROR! : DOMAIN_local_sizes attribute does not match rebuild dimension lengths in the first file'
      WRITE(numerr,*) 'Attribute DOMAIN_local_sizes is : ', local_sizes
      WRITE(numerr,*) 'Dimensions to be rebuilt are of size : ', outdimlens(rebuild_dims(1)), outdimlens(rebuild_dims(2)) 
      STOP 3
   ENDIF

   IF (l_findDims) THEN
      IF (l_verbose) WRITE(numout,*) 'Finding rebuild dimensions from the first file...'
   ELSE
      IF (l_verbose) WRITE(numout,*) 'Using rebuild dimensions given in namelist...'
   ENDIF

   IF( rbdims > 1 ) THEN
      IF (l_verbose) WRITE(numout,*) 'Rebuilding across dimensions '//TRIM(indimnames(rebuild_dims(1)))//  &
         &                      ' and '//TRIM(indimnames(rebuild_dims(2)))
   ELSE
      IF (l_verbose) WRITE(numout,*) 'Rebuilding across dimension '//TRIM(indimnames(rebuild_dims(1)))
   ENDIF
      
   !2.2.2 Copy the global attributes into the output file, apart from those beginning with DOMAIN_  
   !      Also need to change the file_name attribute and the TimeStamp attribute.
   IF(mype.eq.0) THEN
      DO attid = 1, natts
         CALL check_nf90( nf90_inq_attname( ncid, nf90_global, attid, attname ),'inq_attname' )
         IF( INDEX( attname, "DOMAIN_" ) == 1 ) CYCLE
         IF( INDEX( attname, "file_name") == 1 ) CYCLE
         IF( INDEX( attname, "associate_file") == 1 ) CYCLE
         IF (l_verbose) WRITE(numout,*) 'Copying attribute '//TRIM(attname)//' into destination file...'
         CALL check_nf90( nf90_copy_att( ncid, nf90_global, attname, outid, nf90_global ) ,'Global')
      END DO

      IF (l_timestamp) THEN
         CALL check_nf90( nf90_put_att( outid, nf90_global, "file_name", TRIM(filebase)//'.nc'),'Global' )

         CALL DATE_AND_TIME ( date=date, time=time, zone=zone )
         timestamp = date(7:8) // "/" // date(5:6) // "/" // date(1:4) // " " // &
                     time(1:2) // ":" // time(3:4) // ":" // time(5:6) // " " // &
                     zone  
         CALL check_nf90( nf90_put_att( outid, nf90_global, "TimeStamp", timestamp ),'put_att' )
         IF (l_verbose) WRITE(numout,*) 'Writing new TimeStamp attribute'
      ENDIF
   ENDIF 

   !2.2.3 Copy the variable definitions and attributes into the output file.
   ALLOCATE(mdiVals(nvars))
   mdiVals(:)=0

   ! define variable
   IF(mype.eq.0) ALLOCATE(varids(nvars))
   ntslice_max = nmax_unlimited
   n_noRebuild = 1
   DO jv = 1, nvars
      CALL check_nf90( nf90_inquire_variable( ncid, jv, varname, xtype, ndims, dimids, natts ),'Inq_var' )
      IF(mype.eq.0) THEN
         ALLOCATE(chunksizes(ndims))
         ALLOCATE(outdimids(ndims))
         len_timeframe=1
         IF( ndims > 0 ) then
            IF( ANY( dimids(1:ndims) == rebuild_dims(1) )) n_noRebuild = 0
            IF( ndims > 1 ) THEN
               IF( ANY( dimids(1:ndims) == rebuild_dims(2) )) n_noRebuild = 0
            ENDIF
            DO idim = 1, ndims

               ltimevar=.FALSE.

               outdimids(idim) = dimids(idim)
               chunksizes(idim) = outdimlens(dimids(idim))
               cdim='|'//TRIM(indimnames(dimids(idim)))//'|'

    ! trick to find var in a list of suggestion (var0 and var1 : INDEX(|var0|var1|,|var|)
               cdimlst='|x|x_grid_T|x_grid_U|x_grid_V|x_grid_W|'
               if( INDEX(TRIM(cdimlst),TRIM(cdim)) > 0 ) &
        &                             chunksizes(idim) = min(outdimlens(dimids(idim)), max(nc4_xchunk,1))

               cdimlst='|y|y_grid_T|y_grid_U|y_grid_V|y_grid_W|'
               if( INDEX(TRIM(cdimlst),TRIM(cdim)) > 0 ) &
        &                             chunksizes(idim) = min(outdimlens(dimids(idim)), max(nc4_ychunk,1))

               cdimlst='|z|deptht|depthu|depthv|depthw|depth|nav_lev|basin|'
               if( INDEX(TRIM(cdimlst),TRIM(cdim)) > 0 ) &
        &                             chunksizes(idim) = min(outdimlens(dimids(idim)), max(nc4_zchunk,1))

               cdimlst='|t|time|time_counter|'
               IF( INDEX(TRIM(cdimlst),TRIM(cdim)) > 0 ) THEN
                   ltimevar=.TRUE.
                   chunksizes(idim) = min(outdimlens(dimids(idim)), max(nc4_tchunk,1))
               END IF

               ! compute the len of a variable without the time dimension
               IF (.NOT. ltimevar) THEN
                  len_timeframe=len_timeframe*outdimlens(dimids(idim))
               END IF

            END DO

            ! definition of ntslice_max that fit in the buffer that works for all variables
            ntslice_max=MIN(ntslice_max,INT(HUGE(mpi_count_value)/len_timeframe))

            CALL check_nf90( nf90_def_var( outid, varname, xtype, outdimids, varid, deflate_level=deflate_level), '0: def_var' )
            IF (l_verbose) WRITE(numout,*) 'Defining variable '//TRIM(varname)//'... id =',jv,'deflate_lvl=',deflate_level
            IF (l_verbose) WRITE(numout,*) 'names   : ',(TRIM(indimnames(dimids(idim)))//' ',idim=1,ndims)
            IF (l_verbose) WRITE(numout,*) 'lens    : ',(outdimlens(dimids(idim)),idim=1,ndims)
            IF (l_verbose) WRITE(numout,*) 'Chunking: ',chunksizes
            IF (l_verbose) WRITE(numout,*) 'Deflation : ',deflate_level
            IF (l_verbose) WRITE(numout,*) 'Chunk algo: ',chunkalg
            IF (l_verbose) FLUSH(numout)
            CALL check_nf90( nf90_def_var_chunking( outid, varid, chunkalg, &
                  &                                 chunksizes ),'0: def_var_chunk' )
         ELSE
            CALL check_nf90( nf90_def_var( outid, varname, xtype, outdimids, varid ),'0: def_var' )
         END IF

         varids(jv)=varid
         DEALLOCATE(chunksizes)
         DEALLOCATE(outdimids)
         IF (l_verbose) WRITE(numout,*) 'Defining variable '//TRIM(varname)//'...'

      ENDIF 

      ! Missing value definition
      call MPI_Bcast(varid, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)
      IF( natts > 0 ) THEN
         DO attid = 1, natts
            CALL check_nf90( nf90_inq_attname( ncid, varid, attid, attname ),'Inq_attname' )
            IF ( attname == "_FillValue" ) THEN
               CALL check_nf90( nf90_get_att( ncid, varid, attname, rmdi ),'Get_att' )
               mdiVals(jv)=rmdi
            ENDIF
            IF(mype.eq.0) CALL check_nf90( nf90_copy_att( ncid, varid, attname, outid, varid ),'0: Copy_att' )
         END DO
      ENDIF
      
   END DO

   ! broadcast ntslice_max to all and sanity check
   call MPI_Bcast(ntslice_max, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)
   IF (l_verbose) WRITE(numout,*) 'Max time slice is: ',ntslice_max
   IF (ntslice_max < 1) THEN
      WRITE(numerr,*) 'ERROR! : variable size without time too big for the maximal MPI_SOURCE_COUNT ', ntslice_max
      STOP 2
   END IF
   ! broadcast n_noRebuild to all and sanity check
   call MPI_Bcast(n_noRebuild, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)
   IF (n_noRebuild == 1) THEN
      WRITE(numerr,*) 'ERROR! : dataset does not contain any rebuild dimensions'
      STOP 2
   END IF

!2.3 End definitions in output file and copy 1st file ncid to the inncids array
   IF(mype.eq.0) CALL check_nf90( nf90_enddef( outid ),'Enddef' )

   IF (l_verbose) WRITE(numout,*) 'Finished defining output file.'

!---------------------------------------------------------------------------
!3. Read in data from each file for each variable and write it
!   mype = 0 is the writer
!   mype != 0 are the readers and there is nproc-1 readers
!   each reader is in charge of nvars/nproc-1 variables
   send_time = 0.
   if(mype.eq.0) then
      CALL write_variable(nvars,outid)
   else
      ndigit=int(log10(REAL(nproc-2))) + 1
      WRITE(fmt_cmype,'("(I", I0,".",I0, ")")') ndigit, ndigit
      WRITE(cmype, TRIM(fmt_cmype)) mype
      WRITE (fclient, "(A7,A,A4)") 'reader_',TRIM(cmype),'.txt'
      OPEN(20,file=fclient, STATUS = 'REPLACE')
      DO jv = mype, nvars,nproc-1
         CALL read_variable(jv, mype)
      ENDDO
   endif

!---------------------------------------------------------------------------
!4. Close files
   if(mype.eq.0) then
      IF (l_verbose) WRITE(numout,*) 'Closing output file...'
      CALL check_nf90( nf90_close( outid ),'Close' )
      CALL check_nf90( nf90_close( ncid ),'Close' )
      IF (l_verbose) WRITE(numout,*) 'NEMO rebuild completed successfully'
      IF (l_verbose) WRITE(numout,*)
   else
      WRITE(20,*) mype,' Send time [s]: ', send_time
      CLOSE(20)
      DO ifile = 1, ndomain
         CALL check_nf90( nf90_close( inncids(ifile) ),' Close' )
      END DO
   endif
   
   CALL MPI_Finalize(ierror)

   IF (l_verbose) WRITE(numout,*) 'END PROGRAM'

   STOP

CONTAINS

   SUBROUTINE check_nf90(status, cmsg, errorFlag)
   !---------------------------------------------------------------------
   !  Checks return code from nf90 library calls and warns if needed
   !  If errorFlag is present then it just increments this flag
   !
   !---------------------------------------------------------------------
      INTEGER, INTENT(IN   ) :: status
      CHARACTER(len=*), INTENT(in) :: cmsg
      INTEGER, INTENT(INOUT), OPTIONAL :: errorFlag
   !---------------------------------------------------------------------

   IF( status /= nf90_noerr ) THEN
      WRITE(numerr,*) TRIM(cmsg)//' !ERROR! : '//TRIM(nf90_strerror(status))
         IF( PRESENT( errorFlag ) ) THEN
            errorFlag = errorFlag + status
         ELSE
            WRITE(numerr,*) "*** NEMO rebuild failed ***"
            WRITE(numerr,*)
            STOP 14
         ENDIF
      ENDIF
   RETURN
   END SUBROUTINE check_nf90

   SUBROUTINE read_variable(jv, mype)
   USE netcdf
   USE mpi
!   USE mpi_f08
   IMPLICIT NONE
   INTEGER::jv, outid, mype, max_mem

   ! NF90_BYTE local data arrays
   INTEGER(i1), ALLOCATABLE, SAVE, DIMENSION(:) :: localdata_1d_i1
   INTEGER(i1), ALLOCATABLE, SAVE, DIMENSION(:,:) :: localdata_2d_i1
   INTEGER(i1), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: localdata_3d_i1
   INTEGER(i1), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: localdata_4d_i1
   INTEGER(i1), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) :: localdata_5d_i1

   ! NF90_SHORT local data arrays
   INTEGER(i2), ALLOCATABLE, SAVE, DIMENSION(:) :: localdata_1d_i2
   INTEGER(i2), ALLOCATABLE, SAVE, DIMENSION(:,:) :: localdata_2d_i2
   INTEGER(i2), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: localdata_3d_i2
   INTEGER(i2), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: localdata_4d_i2
   INTEGER(i2), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) :: localdata_5d_i2

   ! NF90_INT local data arrays
   INTEGER(i4), ALLOCATABLE, SAVE, DIMENSION(:) :: localdata_1d_i4
   INTEGER(i4), ALLOCATABLE, SAVE, DIMENSION(:,:) :: localdata_2d_i4
   INTEGER(i4), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: localdata_3d_i4
   INTEGER(i4), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: localdata_4d_i4
   INTEGER(i4), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) :: localdata_5d_i4

   ! NF90_FLOAT local data arrays
   REAL(sp), ALLOCATABLE, SAVE, DIMENSION(:) :: localdata_1d_sp
   REAL(sp), ALLOCATABLE, SAVE, DIMENSION(:,:) :: localdata_2d_sp
   REAL(sp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: localdata_3d_sp
   REAL(sp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: localdata_4d_sp
   REAL(sp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) :: localdata_5d_sp

   ! NF90_DOUBLE local data arrays
   REAL(dp), ALLOCATABLE, SAVE, DIMENSION(:) :: localdata_1d_dp
   REAL(dp), ALLOCATABLE, SAVE, DIMENSION(:,:) :: localdata_2d_dp
   REAL(dp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: localdata_3d_dp
   REAL(dp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: localdata_4d_dp
   REAL(dp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) :: localdata_5d_dp

   INTEGER, DIMENSION(2) :: idomain, jdomain, rdomain, start_pos
   !INTEGER(MPI_COUNT_KIND) :: len_data  ! for MPI-3 => buffer bigger because seems to allow 64b integer
   INTEGER :: len_data
   INTEGER :: ierr
   INTEGER :: mythread
   REAL(dp) :: r_time, ssend_time

   r_time = MPI_Wtime();

   !read variable
   ValMin = 1.e10
   ValMax = -1.e10
   l_valid = .false.
   istop = nf90_noerr
   nt = 1

   ! slice management
   !     - We use the min between ntslice_max and the user define nslice
   !     - If ntslice_max < nslicesize => md5sum between mpp and nompp will be different 
   !       because of the way netcdf are wrote on disk
   ntslice = nmax_unlimited
   nslicesize=MIN(nslicesize,ntslice_max)
   IF( nslicesize == 0 ) nslicesize = nmax_unlimited

!3.2 Inquire variable to find out name and how many dimensions it has
!    and importantly whether it contains the dimensions in rebuild_dims()
   CALL check_nf90( nf90_inquire_variable( ncid, jv, varname, xtype, ndims, dimids, natts ),'R: get_var' )

   l_noRebuild = .true.
   IF( ANY( dimids(1:ndims) == rebuild_dims(1) )) l_noRebuild = .false.
   IF( rbdims > 1 ) THEN
      IF( ANY( dimids(1:ndims) == rebuild_dims(2) )) l_noRebuild = .false.
   ENDIF

!3.2.0 start while loop for time chunking
   DO WHILE( nt <= nmax_unlimited )

      IF( ndims > 3 ) THEN
         ntslice = MIN( nslicesize, nmax_unlimited + 1 - nt )
      ENDIF

      IF (l_noRebuild) THEN

         IF( nslicesize == nmax_unlimited .OR. ndims <= 3 ) THEN
            WRITE(20,*) 'Copying data from variable '//TRIM(varname)//'...'
         ELSE
            WRITE(20,'(A,I3,A,I3,A)') ' Copying data from variable '  &
            &                 //TRIM(varname)//' for slices ',nt,' to ',nt+ntslice-1,' ...'
         ENDIF

!3.2.1 If rebuilding not required then just need to read in variable
!      for copying direct into output file after the loop.
         IF( ndims == 0 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_0d_i1 ),'R: get_var' )
               CASE( NF90_SHORT )
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_0d_i2 ),'R: get_var' )
               CASE( NF90_INT )
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_0d_i4 ),'R: get_var' )
               CASE( NF90_FLOAT )
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_0d_sp ),'R: get_var' )
               CASE( NF90_DOUBLE )
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_0d_dp ),'R: get_var' )
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  CALL FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 1 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_1d_i1(indimlens(dimids(1))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_1d_i1 ),'R: get_var' )
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_1d_i2(indimlens(dimids(1))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_1d_i2 ),'R: get_var' )
               CASE( NF90_INT )
                  ALLOCATE(globaldata_1d_i4(indimlens(dimids(1))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_1d_i4 ),'R: get_var' )
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_1d_sp(indimlens(dimids(1))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_1d_sp ),'R: get_var' )
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_1d_dp(indimlens(dimids(1))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_1d_dp ),'R: get_var' )
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 2 ) THEN
            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_2d_i1(indimlens(dimids(1)),indimlens(dimids(2))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_2d_i1 ),'R: get_var' )
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_2d_i2(indimlens(dimids(1)),indimlens(dimids(2))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_2d_i2 ),'R: get_var' )
               CASE( NF90_INT )
                  ALLOCATE(globaldata_2d_i4(indimlens(dimids(1)),indimlens(dimids(2))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_2d_i4 ),'R: get_var' )
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_2d_sp(indimlens(dimids(1)),indimlens(dimids(2))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_2d_sp ),'R: get_var' )
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_2d_dp(indimlens(dimids(1)),indimlens(dimids(2))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_2d_dp ),'R: get_var' )
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 3 ) THEN
            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_3d_i1(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_3d_i1 ),'R: get_var' )
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_3d_i2(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_3d_i2 ),'R: get_var' )
               CASE( NF90_INT )
                  ALLOCATE(globaldata_3d_i4(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_3d_i4 ) ,'R: get_var')
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_3d_sp(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_3d_sp ),'R: get_var' )
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_3d_dp(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3))))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_3d_dp ),'R: get_var' )
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 4 ) THEN
            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_4d_i1(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_4d_i1, start=(/1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_4d_i2(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_4d_i2, start=(/1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_INT )
                  ALLOCATE(globaldata_4d_i4(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_4d_i4, start=(/1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_4d_sp(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_4d_sp, start=(/1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_4d_dp(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_4d_dp, start=(/1,1,1,nt/) ),'R: get_var' )
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 5 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_5d_i1(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_5d_i1, start=(/1,1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_5d_i2(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_5d_i2, start=(/1,1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_INT )
                  ALLOCATE(globaldata_5d_i4(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_5d_i4, start=(/1,1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_5d_sp(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_5d_sp, start=(/1,1,1,1,nt/) ),'R: get_var' )
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_5d_dp(indimlens(dimids(1)),indimlens(dimids(2)),       &
                     &                      indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                  CALL check_nf90( nf90_get_var( ncid, jv, globaldata_5d_dp, start=(/1,1,1,1,nt/) ),'R: get_var' )
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ENDIF

      ELSE  ! l_noRebuild = .false.

      !3.2.2 For variables that require rebuilding we need to read in from all ndomain files
      !      Here we allocate global variables ahead of looping over files
         IF( nslicesize == nmax_unlimited .OR. ndims <= 3 ) THEN
            WRITE(20,*) 'Rebuilding data from variable '//TRIM(varname)//'...'
         ELSE
            WRITE(20,'(A,I3,A,I3,A)') ' Rebuilding data from variable '  &
            &                 //TRIM(varname)//' for slices ',nt,' to ',nt+ntslice-1,' ...'
         ENDIF
         
         IF( ndims == 1 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_1d_i1(outdimlens(dimids(1))))
                  IF (l_maskout) globaldata_1d_i1(:)=mdiVals(jv)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_1d_i2(outdimlens(dimids(1))))
                  IF (l_maskout) globaldata_1d_i2(:)=mdiVals(jv)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_1d_i4(outdimlens(dimids(1))))
                  IF (l_maskout) globaldata_1d_i4(:)=mdiVals(jv)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_1d_sp(outdimlens(dimids(1))))
                  IF (l_maskout) globaldata_1d_sp(:)=mdiVals(jv)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_1d_dp(outdimlens(dimids(1))))
                  IF (l_maskout) globaldata_1d_dp(:)=mdiVals(jv)
               CASE DEFAULT
                  WRITE(numerr,*) '1d alloc unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 2 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_2d_i1(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  IF (l_maskout) globaldata_2d_i1(:,:)=mdiVals(jv)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_2d_i2(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  IF (l_maskout) globaldata_2d_i2(:,:)=mdiVals(jv)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_2d_i4(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  IF (l_maskout) globaldata_2d_i4(:,:)=mdiVals(jv)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_2d_sp(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  IF (l_maskout) globaldata_2d_sp(:,:)=mdiVals(jv)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_2d_dp(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  IF (l_maskout) globaldata_2d_dp(:,:)=mdiVals(jv)
               CASE DEFAULT
                  WRITE(numerr,*) '2d alloc unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 3 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_3d_i1(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3))))
                  IF (l_maskout) globaldata_3d_i1(:,:,:)=mdiVals(jv)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_3d_i2(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3))))
                  IF (l_maskout) globaldata_3d_i2(:,:,:)=mdiVals(jv)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_3d_i4(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3))))
                  IF (l_maskout) globaldata_3d_i4(:,:,:)=mdiVals(jv)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_3d_sp(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3))))
                  IF (l_maskout) globaldata_3d_sp(:,:,:)=mdiVals(jv)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_3d_dp(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3))))
                  IF (l_maskout) globaldata_3d_dp(:,:,:)=mdiVals(jv)
               CASE DEFAULT
                  WRITE(numerr,*) '3d alloc unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 4 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_4d_i1(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),ntslice))
                  IF (l_maskout) globaldata_4d_i1(:,:,:,:)=mdiVals(jv)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_4d_i2(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),ntslice))
                  IF (l_maskout) globaldata_4d_i2(:,:,:,:)=mdiVals(jv)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_4d_i4(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),ntslice))
                  IF (l_maskout) globaldata_4d_i4(:,:,:,:)=mdiVals(jv)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_4d_sp(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),ntslice))
                  IF (l_maskout) globaldata_4d_sp(:,:,:,:)=mdiVals(jv)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_4d_dp(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),ntslice))
                  IF (l_maskout) globaldata_4d_dp(:,:,:,:)=mdiVals(jv)
               CASE DEFAULT
                  WRITE(numerr,*) '4d alloc unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 5 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_5d_i1(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  IF (l_maskout) globaldata_5d_i1(:,:,:,:,:)=mdiVals(jv)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_5d_i2(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  IF (l_maskout) globaldata_5d_i2(:,:,:,:,:)=mdiVals(jv)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_5d_i4(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  IF (l_maskout) globaldata_5d_i4(:,:,:,:,:)=mdiVals(jv)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_5d_sp(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  IF (l_maskout) globaldata_5d_sp(:,:,:,:,:)=mdiVals(jv)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_5d_dp(outdimlens(dimids(1)),outdimlens(dimids(2)),     &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  IF (l_maskout) globaldata_5d_dp(:,:,:,:,:)=mdiVals(jv)
               CASE DEFAULT
                  WRITE(numerr,*) '5d alloc unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT
         ELSE
            WRITE(numerr,*) 'ERROR! : A netcdf variable has more than 5 dimensions which is not taken into account'
            STOP 4
         ENDIF
        
            CALL check_nf90( nf90_inquire_variable( inncids(1), jv, varname, xtype, ndims, dimids, natts ), 'R: inq_var', istop )
            CALL check_nf90( nf90_inquire( inncids(1), unlimitedDimId = unlimitedDimId ), 'R: inq', istop )         
            DO attid = 1, natts
               CALL check_nf90( nf90_inq_attname( inncids(1), jv, attid, attname ), 'R: get_attname', istop )
               IF( INDEX( attname, "valid_min" ) == 1 ) THEN
                  CALL check_nf90( nf90_get_att( inncids(1), jv, attname, InMin), 'R: get_att valid_min', istop )
                  l_valid = .true.
               ENDIF
               IF( INDEX( attname, "valid_max" ) == 1 ) THEN
                  CALL check_nf90( nf90_get_att( inncids(1), jv, attname, InMax ), 'R: get_att valid_max', istop )
                  l_valid = .true.
               ENDIF
            END DO

         DO ifile = 1, ndomain
            ncid = inncids(ifile)
            CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_size_local', local_sizes ), 'R: get_att DOMAIN', istop )
            CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_position_first', start_pos ), 'R: get_att DOMAIN', istop )
            CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_halo_size_start', halo_start ), 'R: get_att DOMAIN', istop )
            CALL check_nf90( nf90_get_att( ncid, nf90_global, 'DOMAIN_halo_size_end', halo_end ), 'R: get_att DOMAIN', istop )

            ! set defaults for rebuilding so that i is 1st, j 2nd
            di=1
            dj=2

            IF( rbdims == 1 ) THEN
               ! override defaults above and set other variables
               start_pos(2) = 1
               local_sizes(2) = outdimlens(3-dimids(2))
               halo_end(2) = 0
               halo_start(2) = 0
               di=rebuild_dims(1)
               dj=3-di
            ENDIF

            !3.3.1 Generate local domain interior sizes from local_sizes and halo sizes
            !      idomain defines the 1st and last interior points in the i direction and
            !      jdomain defines the 1st and last interior points in the j direction

            idomain(1) = 1 + halo_start(di)
            idomain(2) = local_sizes(di) - halo_end(di)
            jdomain(1) = 1 + halo_start(dj)
            jdomain(2) = local_sizes(dj) - halo_end(dj)

            !3.3.2 For rbdims or more dimensions put the data array from this input file into the correct
            !      part of the output data array. Assume the first dimensions are those to be rebuilt.

            IF( ndims == 1 ) THEN

               IF( rebuild_dims(1) == 1 ) THEN
                  dr = di
                  rdomain = idomain
               ELSE
                  dr = dj
                  rdomain = jdomain
               ENDIF

              SELECT CASE( xtype )
                  CASE( NF90_BYTE )
                     ALLOCATE(localdata_1d_i1(local_sizes(dr)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_1d_i1 ), 'R: get_var', istop )
                     DO jr = rdomain(1), rdomain(2)
                        globaldata_1d_i1(start_pos(dr) + jr - 1) = localdata_1d_i1(jr)
                     END DO
                     DEALLOCATE(localdata_1d_i1)
                  CASE( NF90_SHORT )
                     ALLOCATE(localdata_1d_i2(local_sizes(dr)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_1d_i2 ), 'R: get_var', istop )
                     DO jr = rdomain(1), rdomain(2)
                        globaldata_1d_i2(start_pos(dr) + jr - 1) = localdata_1d_i2(jr)
                     END DO
                     DEALLOCATE(localdata_1d_i2)
                  CASE( NF90_INT )
                     ALLOCATE(localdata_1d_i4(local_sizes(dr)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_1d_i4 ), 'R: get_var', istop )
                     DO jr = rdomain(1), rdomain(2)
                        globaldata_1d_i4(start_pos(dr) + jr - 1) = localdata_1d_i4(jr)
                     END DO
                     DEALLOCATE(localdata_1d_i4)
                  CASE( NF90_FLOAT )
                     ALLOCATE(localdata_1d_sp(local_sizes(dr)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_1d_sp ), 'R: get_var', istop )
                     DO jr = rdomain(1), rdomain(2)
                        globaldata_1d_sp(start_pos(dr) + jr - 1) = localdata_1d_sp(jr)
                     END DO
                     DEALLOCATE(localdata_1d_sp)
                  CASE( NF90_DOUBLE )
                     ALLOCATE(localdata_1d_dp(local_sizes(dr)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_1d_dp ), 'R: get_var', istop )
                     DO jr = rdomain(1), rdomain(2)
                        globaldata_1d_dp(start_pos(dr) + jr - 1) = localdata_1d_dp(jr)
                     END DO
                     DEALLOCATE(localdata_1d_dp)
                  CASE DEFAULT
                     WRITE(numerr,*) '1d mapping unknown nf90 type: ', xtype
                     FLUSH(numerr)
                     istop = istop + 1
               END SELECT

            ELSEIF( ndims == 2 ) THEN
        
               SELECT CASE( xtype )
                  CASE( NF90_BYTE )
                     ALLOCATE(localdata_2d_i1(local_sizes(di),local_sizes(dj)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_2d_i1 ), 'R: get_var', istop )
                        DO jj = jdomain(1), jdomain(2)
                           DO ji = idomain(1), idomain(2)
                              globaldata_2d_i1(start_pos(di) + ji - 1, start_pos(dj) + jj - 1) = localdata_2d_i1(ji,jj)
                        END DO
                     END DO
                     DEALLOCATE(localdata_2d_i1)
                  CASE( NF90_SHORT )
                     ALLOCATE(localdata_2d_i2(local_sizes(di),local_sizes(dj)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_2d_i2 ), 'R: get_var', istop )
                     DO jj = jdomain(1), jdomain(2)
                        DO ji = idomain(1), idomain(2)
                           globaldata_2d_i2(start_pos(di) + ji - 1, start_pos(dj) + jj - 1) = localdata_2d_i2(ji,jj)
                        END DO
                     END DO
                     DEALLOCATE(localdata_2d_i2)
                  CASE( NF90_INT )
                     ALLOCATE(localdata_2d_i4(local_sizes(di),local_sizes(dj)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_2d_i4 ), 'R: get_var', istop )
                     DO jj = jdomain(1), jdomain(2)
                        DO ji = idomain(1), idomain(2)
                           globaldata_2d_i4(start_pos(di) + ji - 1, start_pos(dj) + jj - 1) = localdata_2d_i4(ji,jj)
                        END DO
                     END DO
                     DEALLOCATE(localdata_2d_i4)
                  CASE( NF90_FLOAT )
                     ALLOCATE(localdata_2d_sp(local_sizes(di),local_sizes(dj)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_2d_sp ), 'R: get_var', istop )
                     DO jj = jdomain(1), jdomain(2)
                        DO ji = idomain(1), idomain(2)
                           globaldata_2d_sp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1) = localdata_2d_sp(ji,jj)
                        END DO
                     END DO
                     DEALLOCATE(localdata_2d_sp)
                  CASE( NF90_DOUBLE )
                     ALLOCATE(localdata_2d_dp(local_sizes(di),local_sizes(dj)))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_2d_dp ), 'R: get_var', istop )
                     DO jj = jdomain(1), jdomain(2)
                        DO ji = idomain(1), idomain(2)
                           globaldata_2d_dp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1) = localdata_2d_dp(ji,jj)
                        END DO
                     END DO
                     DEALLOCATE(localdata_2d_dp) 
                  CASE DEFAULT
                     WRITE(numerr,*) '2d mapping unknown nf90 type: ', xtype
                     FLUSH(numerr)
                     istop = istop + 1
               END SELECT

            ELSEIF( ndims == 3 ) THEN

               SELECT CASE( xtype )
                  CASE( NF90_BYTE )
                     ALLOCATE(localdata_3d_i1(local_sizes(di),local_sizes(dj),indimlens(dimids(3))))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_3d_i1 ), 'R: get_var', istop )
                     DO jk = 1, indimlens(dimids(3))
                        DO jj = jdomain(1), jdomain(2)
                           DO ji = idomain(1), idomain(2)
                              globaldata_3d_i1(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk) = localdata_3d_i1(ji,jj,jk)
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_3d_i1)
                  CASE( NF90_SHORT )
                     ALLOCATE(localdata_3d_i2(local_sizes(di),local_sizes(dj),indimlens(dimids(3))))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_3d_i2 ), 'R: get_var', istop )
                     DO jk = 1, indimlens(dimids(3))
                        DO jj = jdomain(1), jdomain(2)
                           DO ji = idomain(1), idomain(2)
                              globaldata_3d_i2(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk) = localdata_3d_i2(ji,jj,jk)
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_3d_i2)
                  CASE( NF90_INT )
                     ALLOCATE(localdata_3d_i4(local_sizes(di),local_sizes(dj),indimlens(dimids(3))))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_3d_i4 ), 'R: get_var', istop )
                     DO jk = 1, indimlens(dimids(3))
                        DO jj = jdomain(1), jdomain(2)
                           DO ji = idomain(1), idomain(2)
                              globaldata_3d_i4(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk) = localdata_3d_i4(ji,jj,jk)
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_3d_i4)
                  CASE( NF90_FLOAT )
                     ! TG: This if statement is added to check if the 1st dimension is the corners (for lon_bounds) variables
                     ! TG: Had to add the unsatisfactory check for 'lon' as it failed for diaptr files
                     ! TG: Would like to find a better assumption for this.
                     IF ( trim(indimnames(dimids(1))) /= dims(1) .AND. indimnames(dimids(1)) .NE. 'lon' ) THEN 
                        ALLOCATE(localdata_3d_sp(indimlens(dimids(1)),local_sizes(di),local_sizes(dj)))
                        CALL check_nf90( nf90_get_var( ncid, jv, localdata_3d_sp ), 'R: get_var', istop )
                        DO jj = jdomain(1), jdomain(2)
                           DO ji = idomain(1), idomain(2)
                              DO jk = 1, indimlens(dimids(1))
                                 globaldata_3d_sp(jk, start_pos(di) + ji - 1, start_pos(dj) + jj - 1) = localdata_3d_sp(jk,ji,jj)
                              END DO
                           END DO
                        END DO
                     ELSE
                        ALLOCATE(localdata_3d_sp(local_sizes(di),local_sizes(dj),indimlens(dimids(3))))
                        CALL check_nf90( nf90_get_var( ncid, jv, localdata_3d_sp ), 'R: get_var', istop ) 
                        DO jk = 1, indimlens(dimids(3))
                           DO jj = jdomain(1), jdomain(2)
                              DO ji = idomain(1), idomain(2)
                                 globaldata_3d_sp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk) = localdata_3d_sp(ji,jj,jk)
                              END DO
                           END DO
                        END DO
                     ENDIF
                     DEALLOCATE(localdata_3d_sp)
                  CASE( NF90_DOUBLE )
                     IF ( trim(indimnames(dimids(1))) /= dims(1) ) THEN
                        ALLOCATE(localdata_3d_dp(indimlens(dimids(1)),local_sizes(di),local_sizes(dj)))
                        CALL check_nf90( nf90_get_var( ncid, jv, localdata_3d_dp ), 'R: get_var', istop ) 
                        DO jj = jdomain(1), jdomain(2)
                           DO ji = idomain(1), idomain(2)
                              DO jk = 1, indimlens(dimids(1))
                                 globaldata_3d_dp(jk, start_pos(di) + ji - 1, start_pos(dj) + jj - 1) = localdata_3d_dp(jk,ji,jj)
                              END DO
                           END DO
                        END DO
                     ELSE
                        ALLOCATE(localdata_3d_dp(local_sizes(di),local_sizes(dj),indimlens(dimids(3))))
                        CALL check_nf90( nf90_get_var( ncid, jv, localdata_3d_dp ), 'R: get_var', istop ) 
                        DO jk = 1, indimlens(dimids(3))
                           DO jj = jdomain(1), jdomain(2)
                              DO ji = idomain(1), idomain(2)
                                 globaldata_3d_dp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk) = localdata_3d_dp(ji,jj,jk)
                              END DO
                           END DO
                        END DO
                     ENDIF
                     DEALLOCATE(localdata_3d_dp)
                  CASE DEFAULT
                     WRITE(numerr,*) '3d mapping unknown nf90 type: ', xtype
                     FLUSH(numerr)
                     istop = istop + 1
               END SELECT
      
            ELSEIF (ndims == 4) THEN
        
               SELECT CASE( xtype )
                  CASE( NF90_BYTE )
                     ALLOCATE(localdata_4d_i1(local_sizes(di),local_sizes(dj),               &
                         &                     indimlens(dimids(3)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_4d_i1, start=(/1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                        DO jk = 1, indimlens(dimids(3))
                           DO jj = jdomain(1), jdomain(2)
                              DO ji = idomain(1), idomain(2)
                                 globaldata_4d_i1(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jl) = localdata_4d_i1(ji,jj,jk,jl)
                              END DO
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_4d_i1)
                  CASE( NF90_SHORT )
                     ALLOCATE(localdata_4d_i2(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_4d_i2, start=(/1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                        DO jk = 1, indimlens(dimids(3))
                           DO jj = jdomain(1), jdomain(2) 
                              DO ji = idomain(1), idomain(2)
                                 globaldata_4d_i2(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jl) = localdata_4d_i2(ji,jj,jk,jl)
                              END DO
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_4d_i2)
                  CASE( NF90_INT )
                     ALLOCATE(localdata_4d_i4(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_4d_i4, start=(/1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                        DO jk = 1, indimlens(dimids(3))
                           DO jj = jdomain(1), jdomain(2) 
                              DO ji = idomain(1), idomain(2)
                                 globaldata_4d_i4(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jl) = localdata_4d_i4(ji,jj,jk,jl)
                              END DO
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_4d_i4)
                  CASE( NF90_FLOAT )
                     ALLOCATE(localdata_4d_sp(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_4d_sp, start=(/1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                        DO jk = 1, indimlens(dimids(3))
                           DO jj = jdomain(1), jdomain(2) 
                              DO ji = idomain(1), idomain(2)
                                 globaldata_4d_sp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jl) = localdata_4d_sp(ji,jj,jk,jl)
                              END DO
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_4d_sp)
                  CASE( NF90_DOUBLE )
                     ALLOCATE(localdata_4d_dp(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_4d_dp, start=(/1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                        DO jk = 1, indimlens(dimids(3))
                           DO jj = jdomain(1), jdomain(2) 
                              DO ji = idomain(1), idomain(2)
                                 globaldata_4d_dp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jl) = localdata_4d_dp(ji,jj,jk,jl)
                              END DO
                           END DO
                        END DO
                     END DO
                     DEALLOCATE(localdata_4d_dp)
                  CASE DEFAULT
                     WRITE(numerr,*) '4d mapping unknown nf90 type: ', xtype
                     FLUSH(numerr)
                     istop = istop + 1
               END SELECT


            ELSEIF (ndims == 5) THEN

               SELECT CASE( xtype )

                  CASE( NF90_BYTE )
                     ALLOCATE(localdata_5d_i1(local_sizes(di),local_sizes(dj),               &
                         &                     indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_5d_i1, start=(/1,1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                       DO jr = 1, indimlens(dimids(4))
                          DO jk = 1, indimlens(dimids(3))
                             DO jj = jdomain(1), jdomain(2)
                                DO ji = idomain(1), idomain(2)
                                   globaldata_5d_i1(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jr, jl) = localdata_5d_i1(ji,jj,jk,jr,jl)
                                END DO
                             END DO
                          END DO
                       END DO
                     END DO
                     DEALLOCATE(localdata_5d_i1)

                  CASE( NF90_SHORT )
                     ALLOCATE(localdata_5d_i2(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_5d_i2, start=(/1,1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                       DO jr = 1, indimlens(dimids(4))
                          DO jk = 1, indimlens(dimids(3))
                             DO jj = jdomain(1), jdomain(2)
                                DO ji = idomain(1), idomain(2)
                                   globaldata_5d_i2(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jr, jl) = localdata_5d_i2(ji,jj,jk,jr,jl)
                                END DO
                             END DO
                          END DO
                       END DO
                     END DO
                     DEALLOCATE(localdata_5d_i2)

                  CASE( NF90_INT )
                     ALLOCATE(localdata_5d_i4(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_5d_i4, start=(/1,1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                       DO jr = 1, indimlens(dimids(4))
                          DO jk = 1, indimlens(dimids(3))
                             DO jj = jdomain(1), jdomain(2)
                                DO ji = idomain(1), idomain(2)
                                   globaldata_5d_i4(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jr, jl) = localdata_5d_i4(ji,jj,jk,jr,jl)
                                END DO
                             END DO
                          END DO
                       END DO
                     END DO
                     DEALLOCATE(localdata_5d_i4)

                  CASE( NF90_FLOAT )
                     ALLOCATE(localdata_5d_sp(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_5d_sp, start=(/1,1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                       DO jr = 1, indimlens(dimids(4))
                          DO jk = 1, indimlens(dimids(3))
                             DO jj = jdomain(1), jdomain(2)
                                DO ji = idomain(1), idomain(2)
                                   globaldata_5d_sp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jr, jl) = localdata_5d_sp(ji,jj,jk,jr,jl)
                                END DO
                             END DO
                          END DO
                       END DO
                     END DO
                     DEALLOCATE(localdata_5d_sp)

                  CASE( NF90_DOUBLE )
                     ALLOCATE(localdata_5d_dp(local_sizes(di),local_sizes(dj),               &
                        &                     indimlens(dimids(3)),indimlens(dimids(4)),ntslice))
                     CALL check_nf90( nf90_get_var( ncid, jv, localdata_5d_dp, start=(/1,1,1,1,nt/) ), 'R: get_var', istop )
                     DO jl = 1, ntslice
                       DO jr = 1, indimlens(dimids(4))
                          DO jk = 1, indimlens(dimids(3))
                             DO jj = jdomain(1), jdomain(2)
                                DO ji = idomain(1), idomain(2)
                                   globaldata_5d_dp(start_pos(di) + ji - 1, start_pos(dj) + jj - 1, jk, jr, jl) = localdata_5d_dp(ji,jj,jk,jr,jl)
                                END DO
                             END DO
                          END DO
                       END DO
                     END DO
                     DEALLOCATE(localdata_5d_dp)

                  CASE DEFAULT
                     WRITE(numerr,*) '5d mapping unknown nf90 type: ', xtype
                     FLUSH(numerr)
                     istop = istop + 1
               END SELECT


            ENDIF ! l_noRebuild false

!3.4 Work out if the valid_min and valid_max attributes exist for this variable.
!    If they do then calculate the extrema over all input files.

            IF (l_valid) THEN
               CALL check_nf90( nf90_get_att( ncid, jv, "valid_min", InMin ), 'R: get_att valid_min', istop )
               CALL check_nf90( nf90_get_att( ncid, jv, "valid_max", InMax ), 'R: get_att valid_max', istop )
               IF( InMin < ValMin ) ValMin = InMin
               IF( InMax > ValMax ) ValMax = InMax
            ENDIF

!3.5 Abort if failure and only 1 thread 

            IF( istop /= nf90_noerr )  THEN
               WRITE(numerr,*) '*** NEMO rebuild failed! ***'
               STOP 5
            ENDIF
        
         END DO  ! loop over files
 
!3.6 Abort if any of the MPI threads failed
         IF( istop /= nf90_noerr )  THEN
            WRITE(numerr,*) '*** NEMO rebuild failed! ***'
            STOP 5
         ENDIF

      ENDIF ! ndims > 2

!---------------------------------------------------------------------------
!4. Send data to processor 0

!4.2 Write the data to the output file depending on how many dimensions it has
      ssend_time = MPI_Wtime()
      IF( ndims == 0 ) THEN

         SELECT CASE( xtype )
            CASE( NF90_BYTE )
             CALL MPI_Send(globaldata_0d_i1, 1, MPI_INTEGER1, 0, jv, MPI_COMM_WORLD, ierr)
            CASE( NF90_SHORT )
             CALL MPI_Send(globaldata_0d_i2, 1, MPI_INTEGER2, 0, jv, MPI_COMM_WORLD, ierr)
            CASE( NF90_INT )
             CALL MPI_Send(globaldata_0d_i4, 1, MPI_INTEGER4, 0, jv, MPI_COMM_WORLD, ierr)
            CASE( NF90_FLOAT )
             CALL MPI_Send(globaldata_0d_sp, 1, MPI_REAL, 0, jv, MPI_COMM_WORLD, ierr)
            CASE( NF90_DOUBLE )
             CALL MPI_Send(globaldata_0d_dp, 1, MPI_DOUBLE_PRECISION, 0, jv, MPI_COMM_WORLD, ierr)
            CASE DEFAULT   
               WRITE(numerr,*) '0d write Unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT

      ELSEIF( ndims == 1 ) THEN

         SELECT CASE( xtype )
            CASE( NF90_BYTE )
               CALL MPI_Send(globaldata_1d_i1, outdimlens(dimids(1)), MPI_INTEGER1, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_1d_i1)
            CASE( NF90_SHORT )
               CALL MPI_Send(globaldata_1d_i2, outdimlens(dimids(1)), MPI_INTEGER2, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_1d_i2)
            CASE( NF90_INT )
               CALL MPI_Send(globaldata_1d_i4, outdimlens(dimids(1)), MPI_INTEGER4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_1d_i4)
            CASE( NF90_FLOAT )
               CALL MPI_Send(globaldata_1d_sp, outdimlens(dimids(1)), MPI_REAL4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_1d_sp)
            CASE( NF90_DOUBLE )
               CALL MPI_Send(globaldata_1d_dp, outdimlens(dimids(1)), MPI_DOUBLE_PRECISION, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_1d_dp)
            CASE DEFAULT   
               WRITE(numerr,*) '1d write Unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT

      ELSEIF( ndims == 2 ) THEN  
         len_data = outdimlens(dimids(1))*outdimlens(dimids(2))     
         SELECT CASE( xtype )   
            CASE( NF90_BYTE )                   
               CALL MPI_Send(globaldata_2d_i1, len_data, MPI_INTEGER1, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_2d_i1)
            CASE( NF90_SHORT )                   
               CALL MPI_Send(globaldata_2d_i2, len_data, MPI_INTEGER2, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_2d_i2)
            CASE( NF90_INT )                              
               CALL MPI_Send(globaldata_2d_i4, len_data, MPI_INTEGER4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_2d_i4)
            CASE( NF90_FLOAT )                              
               CALL MPI_Send(globaldata_2d_sp, len_data, MPI_REAL4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_2d_sp)
            CASE( NF90_DOUBLE )                                         
               CALL MPI_Send(globaldata_2d_dp, len_data, MPI_DOUBLE_PRECISION, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_2d_dp)
            CASE DEFAULT   
               WRITE(numerr,*) '2d write unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT     
                      
      ELSEIF( ndims == 3 ) THEN
         len_data = outdimlens(dimids(1))*outdimlens(dimids(2))*outdimlens(dimids(3)) 
         SELECT CASE( xtype ) 
            CASE( NF90_BYTE )                   
               CALL MPI_Send(globaldata_3d_i1, len_data, MPI_INTEGER1, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_3d_i1)
            CASE( NF90_SHORT )                   
               CALL MPI_Send(globaldata_3d_i2, len_data, MPI_INTEGER2, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_3d_i2)
            CASE( NF90_INT )                              
               CALL MPI_Send(globaldata_3d_i4, len_data, MPI_INTEGER4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_3d_i4)
            CASE( NF90_FLOAT )                              
               CALL MPI_Send(globaldata_3d_sp, len_data, MPI_REAL4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_3d_sp)
            CASE( NF90_DOUBLE )                                         
               CALL MPI_Send(globaldata_3d_dp, len_data, MPI_DOUBLE_PRECISION, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_3d_dp)
            CASE DEFAULT   
               WRITE(numerr,*) '3d write unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT     
    
      ELSEIF( ndims == 4 ) THEN
         len_data = outdimlens(dimids(1))*outdimlens(dimids(2))*outdimlens(dimids(3))*ntslice
         SELECT CASE( xtype )   
            CASE( NF90_BYTE )                   
               CALL MPI_Send(globaldata_4d_i1, len_data, MPI_INTEGER1, 0, jv, MPI_COMM_WORLD,ierr)
               DEALLOCATE(globaldata_4d_i1)
            CASE( NF90_SHORT )                   
               CALL MPI_Send(globaldata_4d_i2, len_data, MPI_INTEGER2, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_4d_i2)
            CASE( NF90_INT )                              
               CALL MPI_Send(globaldata_4d_i4, len_data, MPI_INTEGER4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_4d_i4)
            CASE( NF90_FLOAT )                              
               CALL MPI_Send(globaldata_4d_sp, len_data, MPI_REAL4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_4d_sp)
            CASE( NF90_DOUBLE )                                         
               CALL MPI_Send(globaldata_4d_dp, len_data, MPI_DOUBLE_PRECISION, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_4d_dp)
            CASE DEFAULT   
               WRITE(numerr,*) '4d write unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT     

      ELSEIF( ndims == 5 ) THEN
         len_data = outdimlens(dimids(1))*outdimlens(dimids(2))*outdimlens(dimids(3))*outdimlens(dimids(4))*ntslice
         SELECT CASE( xtype )
            CASE( NF90_BYTE )
               CALL MPI_Send(globaldata_5d_i1, len_data, MPI_INTEGER1, 0, jv, MPI_COMM_WORLD,ierr)
               DEALLOCATE(globaldata_5d_i1)
            CASE( NF90_SHORT )
               CALL MPI_Send(globaldata_5d_i2, len_data, MPI_INTEGER2, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_5d_i2)
            CASE( NF90_INT )
               CALL MPI_Send(globaldata_5d_i4, len_data, MPI_INTEGER4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_5d_i4)
            CASE( NF90_FLOAT )
               CALL MPI_Send(globaldata_5d_sp, len_data, MPI_REAL4, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_5d_sp)
            CASE( NF90_DOUBLE )
               CALL MPI_Send(globaldata_5d_dp, len_data, MPI_DOUBLE_PRECISION, 0, jv, MPI_COMM_WORLD, ierr)
               DEALLOCATE(globaldata_5d_dp)
            CASE DEFAULT
               WRITE(numerr,*) '5d MPI_Send unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT
      ENDIF

      !send max and min to 0 if set
      IF (l_valid) THEN
         sr_data(1) = ValMin
         sr_data(2) = ValMax 
         CALL MPI_Send(sr_data, 2, MPI_REAL4, 0, NMAX_VARS+jv, MPI_COMM_WORLD, ierr)
      ENDIF
      send_time = send_time + (MPI_Wtime()-ssend_time)

      nt = nt + ntslice

   END DO ! WHILE loop
   r_time = MPI_Wtime()-r_time
   call mem_usage(max_mem)

   WRITE(20,*) 'PROCESSOR: ', mype, 'Sending variable '//TRIM(varname)//'... (processed in ',r_time ,' s); MEM (GB) ', max_mem/1024.**2

   RETURN

   END SUBROUTINE read_variable

   SUBROUTINE write_variable(max_vars,outid)
   USE netcdf
   USE mpi
   !USE mpi_f08
   IMPLICIT NONE
   INTEGER STATUS(MPI_STATUS_SIZE)
   INTEGER :: ierror, nstatus
   INTEGER::jv, outid, jvar
   INTEGER :: it, max_vars
   !INTEGER(MPI_COUNT_KIND) :: len_data ! MPI-3 option for bigger buffer (not tested)
   INTEGER :: len_data
   INTEGER:: ierr, jp, jd, max_mem
   INTEGER, DIMENSION(2) :: idomain, jdomain, rdomain, start_pos
   REAL(dp) :: w_time,swait_time, wait_time
   INTEGER :: irq1, irq2
   LOGICAL :: lirq2
   open(20,file='writer.txt', STATUS = 'REPLACE')

   wait_time = 0.
   do it = 1, max_vars

      istop = nf90_noerr
      nt = 1

      ! slice management
      !     - We use the min between ntslice_max and the user define nslice
      !     - If ntslice_max < nslicesize => md5sum between mpp and nompp will be different 
      !       because of the way netcdf are wrote on disk
      ntslice = nmax_unlimited
      nslicesize=MIN(nslicesize,ntslice_max)
      IF( nslicesize == 0 ) nslicesize = nmax_unlimited
   
      irq1 = MPI_REQUEST_NULL
      irq2 = MPI_REQUEST_NULL 

! 3.2 start while loop for time chunking
      w_time = MPI_Wtime()
      DO WHILE( nt <= nmax_unlimited )
         swait_time = MPI_Wtime()
         !
         ! set TAG to variable number to keep order of variable the same in output file
         ! it may affect performance in case multiple readers
         ! remove any order option (MPI_ANY_TAG) because not convinced it works with nslicesize /= nmax_unlimited
         CALL MPI_Probe(MPI_ANY_SOURCE, it, MPI_COMM_WORLD, status, ierr)
         wait_time = wait_time + (MPI_Wtime() - swait_time)
         jv = status(MPI_TAG)   
         jp = status(MPI_SOURCE)

         ! Inquire variable to find out name and how many dimensions it has
         !    and importantly whether it contains the dimensions in rebuild_dims()
         CALL check_nf90( nf90_inquire_variable( ncid, jv, varname, xtype, ndims, dimids, natts ), 'W: inq_var' )

         IF( ndims > 3 ) THEN
            ntslice = MIN( nslicesize, nmax_unlimited + 1 - nt )
         ENDIF
!
         WRITE(20,*) 'RECEIVING data from ', jp,' variable ',jv,' '//TRIM(varname)//'...'

!        Allocate and receive global variable
         IF( ndims == 0 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  CALL MPI_IRECV(globaldata_0d_i1, 1, MPI_INTEGER1, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_SHORT )
                  CALL MPI_IRECV(globaldata_0d_i2, 1, MPI_INTEGER2, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_INT )
                  CALL MPI_IRECV(globaldata_0d_i4, 1, MPI_INTEGER4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_FLOAT )
                  CALL MPI_IRECV(globaldata_0d_sp, 1, MPI_REAL4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_DOUBLE )
                  CALL MPI_IRECV(globaldata_0d_dp, 1, MPI_DOUBLE_PRECISION, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 1 ) THEN

            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_1d_i1(outdimlens(dimids(1))))
                  CALL MPI_IRECV(globaldata_1d_i1, outdimlens(dimids(1)), MPI_INTEGER1, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_1d_i2(outdimlens(dimids(1))))
                  CALL MPI_IRECV(globaldata_1d_i2, outdimlens(dimids(1)), MPI_INTEGER2, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_1d_i4(outdimlens(dimids(1))))
                  CALL MPI_IRECV(globaldata_1d_i4, outdimlens(dimids(1)), MPI_INTEGER4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_1d_sp(outdimlens(dimids(1))))
                  CALL MPI_IRECV(globaldata_1d_sp, outdimlens(dimids(1)), MPI_REAL4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_1d_dp(outdimlens(dimids(1))))
                  CALL MPI_IRECV(globaldata_1d_dp, outdimlens(dimids(1)), MPI_DOUBLE_PRECISION, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 2 ) THEN
            len_data = outdimlens(dimids(1))*outdimlens(dimids(2))
            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_2d_i1(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  CALL MPI_IRECV(globaldata_2d_i1, len_data, MPI_INTEGER1, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_2d_i2(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  CALL MPI_IRECV(globaldata_2d_i2, len_data, MPI_INTEGER2, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_2d_i4(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  CALL MPI_IRECV(globaldata_2d_i4, len_data, MPI_INTEGER4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_2d_sp(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  CALL MPI_IRECV(globaldata_2d_sp, len_data, MPI_REAL4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_2d_dp(outdimlens(dimids(1)),outdimlens(dimids(2))))
                  CALL MPI_IRECV(globaldata_2d_dp, len_data, MPI_DOUBLE_PRECISION, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 3 ) THEN
            len_data = outdimlens(dimids(1))*outdimlens(dimids(2))*outdimlens(dimids(3))
            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_3d_i1(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3))))
                  CALL MPI_IRECV(globaldata_3d_i1, len_data, MPI_INTEGER1, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_3d_i2(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3))))
                  CALL MPI_IRECV(globaldata_3d_i2, len_data, MPI_INTEGER2, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_3d_i4(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3))))
                  CALL MPI_IRECV(globaldata_3d_i4, len_data, MPI_INTEGER4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_3d_sp(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3))))
                  CALL MPI_IRECV(globaldata_3d_sp, len_data, MPI_REAL4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_3d_dp(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3))))
                  CALL MPI_IRECV(globaldata_3d_dp, len_data, MPI_DOUBLE_PRECISION, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 4 ) THEN
            len_data = outdimlens(dimids(1))*outdimlens(dimids(2))*outdimlens(dimids(3))*ntslice
            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_4d_i1(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),ntslice))
                  CALL MPI_IRECV(globaldata_4d_i1, len_data, MPI_INTEGER1, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_4d_i2(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),ntslice))
                  CALL MPI_IRECV(globaldata_4d_i2, len_data, MPI_INTEGER2, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_4d_i4(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),ntslice))
                  CALL MPI_IRECV(globaldata_4d_i4, len_data, MPI_INTEGER4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_4d_sp(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),ntslice))
                  CALL MPI_IRECV(globaldata_4d_sp, len_data, MPI_REAL4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_4d_dp(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),ntslice))
                  CALL MPI_IRECV(globaldata_4d_dp, len_data, MPI_DOUBLE_PRECISION, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ELSEIF( ndims == 5 ) THEN
            len_data = outdimlens(dimids(1))*outdimlens(dimids(2))*outdimlens(dimids(3))*outdimlens(dimids(4))*ntslice
            SELECT CASE( xtype )
               CASE( NF90_BYTE )
                  ALLOCATE(globaldata_5d_i1(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  CALL MPI_IRECV(globaldata_5d_i1, len_data, MPI_INTEGER1, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_SHORT )
                  ALLOCATE(globaldata_5d_i2(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  CALL MPI_IRECV(globaldata_5d_i2, len_data, MPI_INTEGER2, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_INT )
                  ALLOCATE(globaldata_5d_i4(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  CALL MPI_IRECV(globaldata_5d_i4, len_data, MPI_INTEGER4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_FLOAT )
                  ALLOCATE(globaldata_5d_sp(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  CALL MPI_IRECV(globaldata_5d_sp, len_data, MPI_REAL4, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE( NF90_DOUBLE )
                  ALLOCATE(globaldata_5d_dp(outdimlens(dimids(1)),outdimlens(dimids(2)),       &
                     &                      outdimlens(dimids(3)),outdimlens(dimids(4)),ntslice))
                  CALL MPI_IRECV(globaldata_5d_dp, len_data, MPI_DOUBLE_PRECISION, jp, jv, MPI_COMM_WORLD, irq1, ierr)
               CASE DEFAULT
                  WRITE(numerr,*) 'Unknown nf90 type: ', xtype
                  FLUSH(numerr)
                  STOP 4
            END SELECT

         ENDIF
         call mem_usage(max_mem)

!3.4 Work out if the valid_min and valid_max attributes exist for this variable.
!    If they do then receive the extrema over all input files.
         ValMin = 1.e10
         ValMax = -1.e10
         l_valid = .false.
         DO attid = 1, natts
            CALL check_nf90( nf90_inq_attname( ncid, jv, attid, attname ), 'W: ing_attname', istop )
            IF( INDEX( attname, "valid_min" ) == 1 ) THEN
               l_valid = .true.
            ENDIF
            IF( INDEX( attname, "valid_max" ) == 1 ) THEN
               l_valid = .true.
            ENDIF
         END DO

         IF (l_valid) THEN
            CALL MPI_IRECV(sr_data, 2, MPI_REAL4, jp, NMAX_VARS+jv, MPI_COMM_WORLD, irq2, ierr)
         ENDIF

!3.5 Abort if failure and only 1 thread 
         IF( nthreads == 1 .AND. istop /= nf90_noerr )  THEN
            WRITE(numerr,*) '*** NEMO rebuild failed! ***'
            STOP 5
         ENDIF

!3.6 Abort if any of the MPI threads failed
         IF( istop /= nf90_noerr )  THEN
            WRITE(numerr,*) '*** NEMO rebuild failed! ***'
            STOP 5
         ENDIF

!---------------------------------------------------------------------------
!4. Write data to output file
      varid=varids(jv)
      WRITE(20,*) 'Writing variable '//TRIM(varname)//'...; MEM GB ', max_mem/1024.**2
      FLUSH(20)

!4.1 If the valid min and max attributes exist then update them in the file
      IF( l_valid ) THEN
         CALL MPI_Wait(irq2, status, ierr)
         ValMin = sr_data(1)
         ValMax = sr_data(2)
         CALL check_nf90( nf90_put_att( outid, varid, "valid_min", ValMin ), 'W: put_att valid_min')
         CALL check_nf90( nf90_put_att( outid, varid, "valid_max", ValMax ), 'W: put_att valid_max' )
      ENDIF
      CALL MPI_Wait(irq1, status, ierr)

!4.2 Write the data to the output file depending on how many dimensions it has
      IF( ndims == 0 ) THEN

         SELECT CASE( xtype )
            CASE( NF90_BYTE )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_0d_i1 ), 'W: put_var 0d' )
            CASE( NF90_SHORT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_0d_i2 ), 'W: put_var 0d' )
            CASE( NF90_INT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_0d_i4 ), 'W: put_var 0d' )
            CASE( NF90_FLOAT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_0d_sp ), 'W: put_var 0d' )
            CASE( NF90_DOUBLE )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_0d_dp ), 'W: put_var 0d' )
         END SELECT

      ELSEIF( ndims == 1 ) THEN

         SELECT CASE( xtype )
            CASE( NF90_BYTE )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_1d_i1 ), 'W: put_var 1d' )
               DEALLOCATE(globaldata_1d_i1)
            CASE( NF90_SHORT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_1d_i2 ), 'W: put_var 1d' )
               DEALLOCATE(globaldata_1d_i2)
            CASE( NF90_INT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_1d_i4 ), 'W: put_var 1d' )
               DEALLOCATE(globaldata_1d_i4)
            CASE( NF90_FLOAT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_1d_sp ), 'W: put_var 1d' )
               DEALLOCATE(globaldata_1d_sp)
            CASE( NF90_DOUBLE )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_1d_dp ), 'W: put_var 1d' )
               DEALLOCATE(globaldata_1d_dp)
         END SELECT

      ELSEIF( ndims == 2 ) THEN  

         SELECT CASE( xtype )   
            CASE( NF90_BYTE )                   
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_2d_i1 ), 'W: put_var 2d' )
               DEALLOCATE(globaldata_2d_i1)
            CASE( NF90_SHORT )                   
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_2d_i2 ), 'W: put_var 2d' )
               DEALLOCATE(globaldata_2d_i2)
            CASE( NF90_INT )                              
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_2d_i4 ), 'W: put_var 2d' )
               DEALLOCATE(globaldata_2d_i4)
            CASE( NF90_FLOAT )                             
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_2d_sp ), 'W: put_var 2d' )
               DEALLOCATE(globaldata_2d_sp)
            CASE( NF90_DOUBLE )                                         
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_2d_dp ), 'W: put_var 2d' )
               DEALLOCATE(globaldata_2d_dp)
            CASE DEFAULT   
               WRITE(numerr,*) 'Write Unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT     
                      
      ELSEIF( ndims == 3 ) THEN
      
         SELECT CASE( xtype ) 
            CASE( NF90_BYTE )                   
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_3d_i1 ) , 'W: put_var 3d')
               DEALLOCATE(globaldata_3d_i1)
            CASE( NF90_SHORT )                   
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_3d_i2 ) , 'W: put_var 3d')
               DEALLOCATE(globaldata_3d_i2)
            CASE( NF90_INT )                              
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_3d_i4 ) , 'W: put_var 3d')
               DEALLOCATE(globaldata_3d_i4)
            CASE( NF90_FLOAT )                              
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_3d_sp ) , 'W: put_var 3d')
               DEALLOCATE(globaldata_3d_sp)
            CASE( NF90_DOUBLE )                                         
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_3d_dp ) , 'W: put_var 3d')
               DEALLOCATE(globaldata_3d_dp)
            CASE DEFAULT   
               WRITE(numerr,*) 'Write Unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT     
    
      ELSEIF( ndims == 4 ) THEN

         SELECT CASE( xtype )   
            CASE( NF90_BYTE )                   
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_4d_i1, start=(/1,1,1,nt/) ) , 'W: put_var 4d')
               DEALLOCATE(globaldata_4d_i1)
            CASE( NF90_SHORT )                   
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_4d_i2, start=(/1,1,1,nt/) ) , 'W: put_var 4d')
               DEALLOCATE(globaldata_4d_i2)
            CASE( NF90_INT )                              
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_4d_i4, start=(/1,1,1,nt/) ) , 'W: put_var 4d')
               DEALLOCATE(globaldata_4d_i4)
            CASE( NF90_FLOAT )                              
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_4d_sp, start=(/1,1,1,nt/) ) , 'W: put_var 4d')
               DEALLOCATE(globaldata_4d_sp)
            CASE( NF90_DOUBLE )                                         
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_4d_dp, start=(/1,1,1,nt/) ) , 'W: put_var 4d')
               DEALLOCATE(globaldata_4d_dp)
            CASE DEFAULT   
               WRITE(numerr,*) 'Write Unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT     
         CALL check_nf90( nf90_sync(outid),'W: sync') 
         
      ELSEIF( ndims == 5 ) THEN

         SELECT CASE( xtype )
            CASE( NF90_BYTE )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_5d_i1, start=(/1,1,1,1,nt/) ) , 'W: put_var 5d')
               DEALLOCATE(globaldata_5d_i1)
            CASE( NF90_SHORT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_5d_i2, start=(/1,1,1,1,nt/) ) , 'W: put_var 5d')
               DEALLOCATE(globaldata_5d_i2)
            CASE( NF90_INT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_5d_i4, start=(/1,1,1,1,nt/) ) , 'W: put_var 5d')
               DEALLOCATE(globaldata_5d_i4)
            CASE( NF90_FLOAT )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_5d_sp, start=(/1,1,1,1,nt/) ) , 'W: put_var 5d')
               DEALLOCATE(globaldata_5d_sp)
            CASE( NF90_DOUBLE )
               CALL check_nf90( nf90_put_var( outid, varid, globaldata_5d_dp, start=(/1,1,1,1,nt/) ) , 'W: put_var 5d')
               DEALLOCATE(globaldata_5d_dp)
            CASE DEFAULT
               WRITE(numerr,*) 'Write Unknown nf90 type: ', xtype
               FLUSH(numerr)
               STOP 4
         END SELECT
         CALL check_nf90( nf90_sync(outid),'W: sync') 
    
      ENDIF

      nt = nt + ntslice

   END DO ! WHILE loop
   w_time = MPI_Wtime() - w_time
   WRITE(20, *)' WRITE variable: ', trim(varname) ,' in ', w_time ,'[s]'; call flush(20)
   END DO
   WRITE(20, *)' TOTAL WAIT TIME: ', wait_time ,'[s]'; call flush(20)
   RETURN

   END SUBROUTINE write_variable

   SUBROUTINE mem_usage(valueRSS)
   implicit none
   integer, intent(out) :: valueRSS

   character(len=200):: filename=' '
   character(len=80) :: line
   character(len=8)  :: pid_char=' '
   integer :: pid,getpid
   logical :: ifxst

   valueRSS=-1    ! return negative number if not found

!--- get process ID

   pid=getpid()
   write(pid_char,'(I8)') pid
   filename='/proc/'//trim(adjustl(pid_char))//'/status'

!--- read system file

   inquire (file=filename,exist=ifxst)
   if (.not.ifxst) then
      write (*,*) 'system file does not exist'
      return
   endif

   open(unit=13, file=filename, action='read')
   do
      read (13,'(a)',end=99) line
      if (line(1:6).eq.'VmRSS:') then
         read (line(7:),*) valueRSS
         exit
      endif
   enddo
99 continue
   close(13)
   return

   END SUBROUTINE mem_usage

END PROGRAM mpp_rebuild_nemo
