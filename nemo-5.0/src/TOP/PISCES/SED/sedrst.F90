MODULE sedrst
   !!======================================================================
   !!                       *** MODULE sedrst ***
   !!   Read and write the restart files for sediment
   !!======================================================================

   !!----------------------------------------------------------------------
   !! * Modules used
   !! ==============
   USE oce_sed
   USE sed
   USE trc_oce, ONLY : l_offline
   USE phycst , ONLY : rday
   USE iom
   USE daymod
   USE lib_mpp         ! distribued memory computing library


   !! * Accessibility
   IMPLICIT NONE
   PRIVATE

   !! * Accessibility
   PUBLIC sed_rst_opn       ! called by ???
   PUBLIC sed_rst_read
   PUBLIC sed_rst_wri
   PUBLIC sed_rst_cal

CONTAINS


   SUBROUTINE sed_rst_opn( kt )
      !!----------------------------------------------------------------------
      !!                    ***  sed_rst_opn  ***
      !!
      !! ** purpose  :   output of sed-trc variable in a netcdf file
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      !
      CHARACTER(LEN=20)   ::   clkt     ! ocean time-step define as a character
      CHARACTER(LEN=50)   ::   clname   ! trc output restart file name
      CHARACTER(LEN=256)  ::   clpath   ! full path to ocean output restart file
      CHARACTER(LEN=3)    ::   cdcomp
      !!----------------------------------------------------------------------
      !
      IF( l_offline ) THEN
         IF( kt == nittrc000 ) THEN
            lrst_sed = .FALSE.
            IF( ln_rst_list ) THEN
               nrst_lst = 1
               nitrst = nn_stocklist( nrst_lst )
            ELSE
               nitrst = nitend
            ENDIF
         ENDIF
         IF( .NOT. ln_rst_list .AND. MOD( kt - 1, nn_stock ) == 0 ) THEN
            ! we use kt - 1 and not kt - nittrc000 to keep the same periodicity from the beginning of the experiment
            nitrst = kt + nn_stock - 1                  ! define the next value of nitrst for restart writing
            IF( nitrst > nitend )   nitrst = nitend   ! make sure we write a restart at the end of the run
         ENDIF
      ELSE
         IF( kt == nittrc000 ) lrst_sed = .FALSE.
      ENDIF

      IF( .NOT. ln_rst_list .AND. nn_stock == -1 )   RETURN   ! we will never do any restart
      ! to get better performances with NetCDF format:
      ! we open and define the tracer restart file one tracer time step before writing the data (-> at nitrst - 2*nn_dttrc + 1)
      ! except if we write tracer restart files every tracer time step or if a tracer restart file was writen at nitend - 2*nn_dttrc + 1
      IF( kt == nitrst - 1 .OR. nn_stock == 1 .OR. ( kt == nitend - 1 .AND. .NOT. lrst_sed ) ) THEN
         ! beware of the format used to write kt (default is i8.8, that should be large enough)
         IF( nitrst > 1.0e9 ) THEN   ;   WRITE(clkt,*       ) nitrst
         ELSE                        ;   WRITE(clkt,'(i8.8)') nitrst
         ENDIF
         ! create the file
         IF(lwp) WRITE(numsed,*)
         clname = TRIM(cexper)//"_"//TRIM(ADJUSTL(clkt))//"_"//TRIM(cn_sedrst_out)
         clpath = TRIM(cn_sedrst_outdir)
         IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath) // '/'
         IF(lwp) WRITE(numsed,*) &
             '             open sed restart.output NetCDF file: ',TRIM(clpath)//clname
         cdcomp ='SED'
         CALL iom_open( TRIM(clpath)//TRIM(clname), numrsw, ldwrt = .TRUE., kdlev = jpksed, cdcomp = cdcomp )
         lrst_sed = .TRUE.
      ENDIF
      !
   END SUBROUTINE sed_rst_opn

   SUBROUTINE sed_rst_read 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_rst_read  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------

      !! * local declarations
      INTEGER :: ji, jj, jk, js, jn 
      REAL(wp), DIMENSION(jpi,jpj,jpksed,jptrased) :: zdta
      REAL(wp), DIMENSION(jpi,jpj,jpksed)          :: zdta1 
      REAL(wp), DIMENSION(jpoce,jpksed)            :: zhipor
      REAL(wp), DIMENSION(jpi,jpj,jpsol)           :: zburial
      REAL(wp) :: zkt
      CHARACTER(len = 20) ::   cltra
      CHARACTER(LEN=20)   ::   name1
      LOGICAL             ::   llok
      !--------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_rst_read')

      IF (lwp) WRITE(numsed,*) ' '      
      IF (lwp) WRITE(numsed,*) ' Initilization of Sediment components from restart'
      IF (lwp) WRITE(numsed,*) ' '

      zdta  = 1.
      zdta1 = 1.

      DO jn = 1, jptrased
         cltra = TRIM(sedtrcd(jn))
         IF( iom_varid( numrsr, TRIM(cltra) , ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrsr, jpdom_auto, TRIM(cltra), zdta(:,:,:,jn) )
         ELSE
            zdta(:,:,:,jn) = 0.0
         ENDIF
      ENDDO

      DO jn = 1, jpsol
         DO jk = 1, jpksed
            solcp(:,jk,jn) = PACK( zdta(:,:,jk,jn), sedmask == 1.0 )
         END DO
      END DO

      DO jn = 1, jpwat
         DO jk = 1, jpksed
            pwcp(:,jk,jn) = PACK( zdta(:,:,jk,jpsol+jn), sedmask == 1.0 )
         END DO
      END DO

      IF( iom_varid( numrsr, "SedpH", ldstop = .FALSE. ) > 0 ) THEN
         CALL iom_get( numrsr, jpdom_auto, "SedpH", zdta1(:,:,:) )
      ELSE
         zdta1(:,:,:) = 0.0
      ENDIF

      zhipor(:,:) = 0.
      DO jk = 1, jpksed
         zhipor(:,jk) = PACK(zdta1(:,:,jk), sedmask == 1.0 )
      END DO

      ! Initialization of [h+] in mol/kg
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            hipor (ji,jk) = 10.**( -1. * zhipor(ji,jk) )
         ENDDO
      ENDDO

      ! Initialization of sediment composant only ie jk=2 to jk=jpksed 
      ! ( nothing in jk=1)
      solcp(1:jpoce,1,:) = 0.
      pwcp (1:jpoce,1,:) = 0.

      DO js = 1, jpsol
         cltra = "burial" // TRIM(sedtrcd(js))
         IF( iom_varid( numrsr, TRIM(cltra) , ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrsr, jpdom_auto, TRIM(cltra), zburial(:,:,js) )
         ELSE
            zburial(:,:,js) = 0.0
         ENDIF
      END DO

      DO js = 1, jpsol
         burial(:,js) = PACK( zburial(:,:,js), sedmask == 1.0 )
      END DO

      IF( ln_timing )  CALL timing_stop('sed_rst_read')
     
   END SUBROUTINE sed_rst_read

   SUBROUTINE sed_rst_wri( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_rst_wri  ***
      !!
      !! ** Purpose :  save field which are necessary for sediment restart
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------
      !!* Modules used
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      !! * local declarations
      INTEGER  :: ji, jj, jk, js, jn
      REAL(wp), DIMENSION(1) ::  zinfo
      CHARACTER(len=50) :: clname
      CHARACTER(len=20) :: cltra, name1 
      REAL(wp), DIMENSION(jpoce,jpksed)   :: zdta   
      REAL(wp), DIMENSION(jpi,jpj,jpksed) :: zdta2
      REAL(wp), DIMENSION(jpi,jpj,jpsol)  :: zburial
      !! -----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_rst_wri')

         !! 0. initialisations
         !! ------------------
         
      IF(lwp) WRITE(numsed,*) ' '
      IF(lwp) WRITE(numsed,*) 'sed_rst_write : write the sediment restart file in NetCDF format ',   &
            'at it= ',kt
      IF(lwp) WRITE(numsed,*) '~~~~~~~~~'

      !! 1. WRITE in nutwrs
      !! ------------------
      zinfo(1) = REAL( kt)
      CALL iom_rstput( kt, nitrst, numrsw, 'kt', zinfo  )

      ! Back to 2D geometry
      DO jn = 1, jpsol
         DO jk = 1, jpksed
            zdta2(:,:,jk) = UNPACK( solcp(:,jk,jn), sedmask == 1.0, 0.0 )
         END DO
         cltra = TRIM(sedtrcd(jn))
         CALL iom_rstput( kt, nitrst, numrsw, TRIM(cltra), zdta2(:,:,:) )
      END DO

      DO jn = 1, jpwat
         DO jk = 1, jpksed
            zdta2(:,:,jk) = UNPACK( pwcp(:,jk,jn), sedmask == 1.0, 0.0 )
         END DO
         cltra = TRIM(sedtrcd(jpsol+jn))
         CALL iom_rstput( kt, nitrst, numrsw, TRIM(cltra), zdta2(:,:,:) )
      END DO
      ! pH
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zdta(ji,jk) = -LOG10( hipor(ji,jk) / ( densSW(ji) + rtrn ) + rtrn )
         ENDDO
         zdta2(:,:,jk) = UNPACK( zdta(:,jk), sedmask == 1.0, 0.0 )
      ENDDO
      CALL iom_rstput( kt, nitrst, numrsw, "SedpH", zdta2(:,:,:) )
         
      ! prognostic variables
      ! --------------------
      DO js = 1, jpsol
         zburial(:,:,js) = UNPACK( burial(:,js), sedmask == 1.0, 0.0 )
      END DO

      DO js = 1, jpsol
         cltra = "burial" // TRIM(sedtrcd(js))
         CALL iom_rstput( kt, nitrst, numrsw, TRIM(cltra), zburial(:,:,js) )
      END DO

      IF( kt == nitrst ) THEN
          CALL iom_close( numrsw )     ! close the restart file (only at last time step)
          IF( ln_rst_list ) THEN
             nrst_lst = nrst_lst + 1
             nitrst = nn_stocklist( nrst_lst )
          ENDIF
      ENDIF

      IF( ln_timing )  CALL timing_stop('sed_rst_wri')
         
   END SUBROUTINE sed_rst_wri


   SUBROUTINE sed_rst_cal( kt, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE sed_rst_cal  ***
      !!
      !!  ** Purpose : Read or write calendar in restart file:
      !!
      !!  WRITE(READ) mode:
      !!       kt        : number of time step since the begining of the experiment at the
      !!                   end of the current(previous) run
      !!       adatrj(0) : number of elapsed days since the begining of the experiment at the
      !!                   end of the current(previous) run (REAL -> keep fractions of day)
      !!       ndastp    : date at the end of the current(previous) run (coded as yyyymmdd integer)
      !!
      !!   According to namelist parameter nrstdt,
      !!       nn_rsttr = 0  no control on the date (nittrc000 is  arbitrary).
      !!       nn_rsttr = 1  we verify that nittrc000 is equal to the last
      !!                   time step of previous run + 1.
      !!       In both those options, the  exact duration of the experiment
      !!       since the beginning (cumulated duration of all previous restart runs)
      !!       is not stored in the restart and is assumed to be (nittrc000-1)*rdt.
      !!       This is valid is the time step has remained constant.
      !!
      !!       nn_rsttr = 2  the duration of the experiment in days (adatrj)
      !!                    has been stored in the restart file.
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      !
      LOGICAL  ::  llok
      REAL(wp) ::  zkt, zrdttrc1
      REAL(wp) ::  zndastp

      ! Time domain : restart
      ! ---------------------

      IF( TRIM(cdrw) == 'READ' ) THEN

         IF(lwp) WRITE(numsed,*)
         IF(lwp) WRITE(numsed,*) 'sed_rst_cal : read the SED restart file for calendar'
         IF(lwp) WRITE(numsed,*) '~~~~~~~~~~~~'

         IF( ln_rst_sed ) THEN
            CALL iom_open( TRIM(cn_sedrst_indir)//'/'//cn_sedrst_in, numrsr )
            CALL iom_get ( numrsr, 'kt', zkt )   ! last time-step of previous run

            IF(lwp) THEN
               WRITE(numsed,*) ' *** Info read in restart : '
               WRITE(numsed,*) '   previous time-step                               : ', NINT( zkt )
               WRITE(numsed,*) ' *** restart option'
               SELECT CASE ( nn_rstsed )
               CASE ( 0 )   ;   WRITE(numsed,*) ' nn_rstsed = 0 : no control of nittrc000'
               CASE ( 1 )   ;   WRITE(numsed,*) ' nn_rstsed = 1 : no control the date at nittrc000 (use ndate0 read in the namelist)'
               CASE ( 2 )   ;   WRITE(numsed,*) ' nn_rstsed = 2 : calendar parameters read in restart'
               END SELECT
               WRITE(numsed,*)
            ENDIF
            ! Control of date 
            IF( nittrc000  - NINT( zkt ) /= 1 .AND.  nn_rstsed /= 0 )                                  &
               &   CALL ctl_stop( ' ===>>>> : problem with nittrc000 for the restart',                 &
               &                  ' verify the restart file or rerun with nn_rsttr = 0 (namelist)' )
         ENDIF
         !
         IF( l_offline ) THEN
            !                                          ! set the date in offline mode     
            IF( ln_rst_sed .AND. nn_rstsed == 2 ) THEN
               CALL iom_get( numrsr, 'ndastp', zndastp )
               ndastp = NINT( zndastp )
               CALL iom_get( numrsr, 'adatrj', adatrj  )
            ELSE
               ndastp = ndate0 - 1     ! ndate0 read in the namelist in dom_nam
               adatrj = ( REAL( nittrc000-1, wp ) * rn_Dt ) / rday
               ! note this is wrong if time step has changed during run
            ENDIF
            !
            IF(lwp) THEN
               WRITE(numsed,*) ' *** Info used values : '
               WRITE(numsed,*) '   date ndastp                                      : ', ndastp
               WRITE(numsed,*) '   number of elapsed days since the begining of run : ', adatrj
               WRITE(numsed,*)
            ENDIF
            !
            CALL day_init          ! compute calendar
            !
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN
         !
         IF(  kt == nitrst ) THEN
            IF(lwp) WRITE(numsed,*)
            IF(lwp) WRITE(numsed,*) 'trc_wri : write the TOP restart file (NetCDF) at it= ', kt, ' date= ', ndastp
            IF(lwp) WRITE(numsed,*) '~~~~~~~'
         ENDIF
         CALL iom_rstput( kt, nitrst, numrsw, 'kt'     , REAL( kt    , wp) )   ! time-step
         CALL iom_rstput( kt, nitrst, numrsw, 'ndastp' , REAL( ndastp, wp) )   ! date
         CALL iom_rstput( kt, nitrst, numrsw, 'adatrj' , adatrj            )   ! number of elapsed days since
         !                                                                     ! the begining of the run [s]
      ENDIF

   END SUBROUTINE sed_rst_cal

END MODULE sedrst
