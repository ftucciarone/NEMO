MODULE stpctl
   !!======================================================================
   !!                       ***  MODULE  stpctl  ***
   !! Ocean run control :  gross check of the ocean time stepping
   !!======================================================================
   !! History :  OPA  ! 1991-03  (G. Madec) Original code
   !!            6.0  ! 1992-06  (M. Imbard)
   !!            8.0  ! 1997-06  (A.M. Treguier)
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            2.0  ! 2009-07  (G. Madec)  Add statistic for time-spliting
   !!            3.7  ! 2016-09  (G. Madec)  Remove solver
   !!            4.0  ! 2017-04  (G. Madec)  regroup global communications
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp_ctl      : Control the run
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean space and time domain variables 
   !  
   USE diawri          ! Standard run outputs       (dia_wri_state routine)
   USE in_out_manager  ! I/O manager
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp         ! distributed memory computing
   USE timing          ! timing
   !
   USE netcdf          ! NetCDF library
   USE, INTRINSIC :: ieee_arithmetic, ONLY : ieee_is_nan

   IMPLICIT NONE
   PRIVATE

   PUBLIC stp_ctl           ! routine called by step.F90

   INTEGER, PARAMETER         ::   jpvar = 2
   INTEGER                    ::   nrunid   ! netcdf file id
   INTEGER, DIMENSION(jpvar)  ::   nvarid   ! netcdf variable id

   !! * Substitutions
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE stp_ctl( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE stp_ctl  ***
      !!
      !! ** Purpose :   Control the run
      !!
      !! ** Method  : - Save the time step in numstp
      !!              - Print it each 50 time steps
      !!              - Stop the run IF problem encountered by setting nstop > 0
      !!                Problems checked: e3t0+ssh minimum smaller that 0
      !!                                  |U|   maximum larger than 10 m/s 
      !!                                  ( not for SWE : negative sea surface salinity )
      !!
      !! ** Actions :   "time.step" file = last ocean time-step
      !!                "run.stat"  file = run statistics
      !!                 nstop indicator sheared among all local domain
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in   ) ::   kt       ! ocean time-step index
      INTEGER, INTENT(in   ) ::   Kmm      ! ocean time level index
      !!
      INTEGER, PARAMETER              ::   jptst = 2
      INTEGER                         ::   ji                                    ! dummy loop indices
      INTEGER                         ::   idtime, istatus
      INTEGER , DIMENSION(jptst)      ::   iareasum, iareamin, iareamax
      INTEGER , DIMENSION(3,jptst)    ::   iloc                                  ! min/max loc indices
      REAL(wp)                        ::   zzz                                   ! local real 
      REAL(wp), DIMENSION(jpvar+1)    ::   zmax
      REAL(wp), DIMENSION(jptst)      ::   zmaxlocal
      LOGICAL                         ::   ll_wrtstp, ll_colruns, ll_wrtruns, ll_0oce
      LOGICAL, DIMENSION(jpi,jpj,jpk) ::   llmsk
      CHARACTER(len=20)               ::   clname
      !!----------------------------------------------------------------------
      !
      IF( nstop > 0 .AND. ngrdstop > -1 )   RETURN   !   stpctl was already called by a child grid
      !
      IF( ln_timing )   CALL timing_start( 'stp_ctl' )
      !
      ll_wrtstp  = ( MOD( kt-nit000, sn_cfctl%ptimincr ) == 0 ) .OR. ( kt == nitend )
      ll_colruns = sn_cfctl%l_runstat .AND. ll_wrtstp .AND. jpnij > 1
      ll_wrtruns = sn_cfctl%l_runstat .AND. ll_wrtstp .AND. lwm
      !
      IF( kt == nit000 ) THEN
         !
         IF( lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'stp_ctl : time-stepping control'
            WRITE(numout,*) '~~~~~~~'
         ENDIF
         !                                ! open time.step    ascii file, done only by 1st subdomain
         IF( lwm )   CALL ctl_opn( numstp, 'time.step', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
         !
         IF( ll_wrtruns ) THEN
            !                             ! open run.stat     ascii file, done only by 1st subdomain
            CALL ctl_opn( numrun, 'run.stat', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
            !                             ! open run.stat.nc netcdf file, done only by 1st subdomain
            clname = 'run.stat.nc'
            IF( .NOT. Agrif_Root() )   clname = TRIM(Agrif_CFixed())//"_"//TRIM(clname)
            istatus = NF90_CREATE( TRIM(clname), NF90_CLOBBER, nrunid )
            istatus = NF90_DEF_DIM( nrunid, 'time', NF90_UNLIMITED, idtime )
            istatus = NF90_DEF_VAR( nrunid, 'e3t0ssh_min', NF90_DOUBLE, (/ idtime /), nvarid(1) )
            istatus = NF90_DEF_VAR( nrunid,   'abs_u_max', NF90_DOUBLE, (/ idtime /), nvarid(2) )
            istatus = NF90_ENDDEF(nrunid)
         ENDIF
         !
      ENDIF
      !
      !                                   !==              write current time step              ==!
      !                                   !==  done only by 1st subdomain at writting timestep  ==!
      IF( lwm .AND. ll_wrtstp ) THEN
         WRITE ( numstp, '(1x, i8)' )   kt
         REWIND( numstp )
      ENDIF
      !                                   !==            test of local extrema           ==!
      !                                   !==  done by all processes at every time step  ==!
      !
      llmsk(     1:nn_hls,:,:) = .FALSE.                                          ! exclude halos from the checked region
      llmsk(Nie0+1:   jpi,:,:) = .FALSE.
      llmsk(:,     1:nn_hls,:) = .FALSE.
      llmsk(:,Nje0+1:   jpj,:) = .FALSE.
      !
      llmsk(Nis0:Nie0,Njs0:Nje0,1) = ssmask(Nis0:Nie0,Njs0:Nje0) == 1._wp         ! define only the inner domain
      !
      ll_0oce = .NOT. ANY( llmsk(:,:,1) )                                         ! no ocean point in the inner domain?
      !
      zmax(1) = MAXVAL( -e3t_0(:,:,1)-ssh(:,:,Kmm) , mask = llmsk(:,:,1)  )       ! e3t_Kmm min
      !
      llmsk(Nis0:Nie0,Njs0:Nje0,:) = umask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
      zmax(2) = MAXVAL(  ABS( uu(:,:,:,Kmm) )      , mask = llmsk(:,:,:) )        ! velocity max (zonal only)
      zmax(jpvar+1) = REAL( nstop, wp )                                           ! stop indicator

      !                                   !==               get global extrema             ==!
      !                                   !==  done by all processes if writting run.stat  ==!
      IF( ll_colruns ) THEN
         zmaxlocal(:) = zmax(1:jptst)
         CALL mpp_max( "stpctl", zmax )          ! max over the global domain
         nstop = NINT( zmax(jpvar+1) )           ! update nstop indicator (now sheared among all local domains)
      ELSE
         ! if no ocean point: MAXVAL returns -HUGE => we must overwrite this value to avoid error handling bellow.
         IF( ll_0oce )   zmax(1:jptst) = 0._wp        ! default "valid" values...
      ENDIF
      !
      zmax(1) = -zmax(1)                              ! move back from max(-zz) to min(zz) : easier to manage!
      IF( ll_colruns ) zmaxlocal(1) = -zmaxlocal(1)   ! move back from max(-zz) to min(zz) : easier to manage! 
      !
      !                                   !==              write "run.stat" files              ==!
      !                                   !==  done only by 1st subdomain at writting timestep  ==!
      IF( ll_wrtruns ) THEN
         WRITE(numrun,9500) kt, zmax(1:jptst)
         DO ji = 1, jpvar
            istatus = NF90_PUT_VAR( nrunid, nvarid(ji), (/zmax(ji)/), (/kt/), (/1/) )
         END DO
         IF( kt == nitend )   istatus = NF90_CLOSE(nrunid)
      ENDIF
      !                                   !==               error handling               ==!
      !                                   !==  done by all processes at every time step  ==!
      !
      IF(   zmax(1) <=   0._wp .OR.                 &         ! negative e3t_Kmm
         &  zmax(2) >   10._wp .OR.                 &         ! too large velocity ( > 10 m/s)
         & ieee_is_nan( SUM(zmax(1:jptst)) ) .OR.   &         ! NaN encounter in the tests
         & ABS(   SUM(zmax(1:jptst)) ) > HUGE(1._wp) ) THEN   ! Infinity encounter in the tests
         !
         iloc(:,:) = 0
         IF( ll_colruns ) THEN   ! zmax is global, so it is the same on all subdomains -> no dead lock with mpp_maxloc
            ! first: close the netcdf file, so we can read it
            IF( lwm .AND. kt /= nitend )   istatus = NF90_CLOSE(nrunid)
            ! get global loc on the min/max
            llmsk(Nis0:Nie0,Njs0:Nje0,1) = ssmask(Nis0:Nie0,Njs0:Nje0 ) == 1._wp         ! define only the inner domain
            CALL mpp_minloc( 'stpctl', e3t_0(:,:,1) + ssh(:,:,Kmm), llmsk(:,:,1), zzz, iloc(1:2,1) )   ! mpp_maxloc ok if mask = F
            llmsk(Nis0:Nie0,Njs0:Nje0,:) = umask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
            CALL mpp_maxloc( 'stpctl', ABS( uu(:,:,:,Kmm))        , llmsk(:,:,:), zzz, iloc(1:3,2) )
            ! find which subdomain has the max.
            iareamin(:) = jpnij+1   ;   iareamax(:) = 0   ;   iareasum(:) = 0
            DO ji = 1, jptst
               IF( zmaxlocal(ji) == zmax(ji) ) THEN
                  iareamin(ji) = narea   ;   iareamax(ji) = narea   ;   iareasum(ji) = 1
               ENDIF
            END DO
            CALL mpp_min( "stpctl", iareamin )         ! min over the global domain
            CALL mpp_max( "stpctl", iareamax )         ! max over the global domain
            CALL mpp_sum( "stpctl", iareasum )         ! sum over the global domain
         ELSE                    ! find local min and max locations:
            ! if we are here, this means that the subdomain contains some oce points -> no need to test the mask used in maxloc
            llmsk(Nis0:Nie0,Njs0:Nje0,1) = ssmask(Nis0:Nie0,Njs0:Nje0 ) == 1._wp        ! define only the inner domain
            iloc(1:2,1) = MINLOC( e3t_0(:,:,1) + ssh(:,:,Kmm), mask = llmsk(:,:,1) )
            !
            llmsk(Nis0:Nie0,Njs0:Nje0,:) = umask(Nis0:Nie0,Njs0:Nje0,:) == 1._wp        ! define only the inner domain
            iloc(1:3,2) = MAXLOC( ABS(  uu(:,:,:,       Kmm)), mask = llmsk(:,:,:) )
            DO ji = 1, jptst   ! local domain indices ==> global domain indices, excluding halos
               iloc(1:2,ji) = (/ mig(iloc(1,ji),0), mjg(iloc(2,ji),0) /)
            END DO
            iareamin(:) = narea   ;   iareamax(:) = narea   ;   iareasum(:) = 1         ! this is local information
         ENDIF
         !
         WRITE(ctmp1,*) ' stp_ctl:  e3t0+ssh <= 0 m  or  |U| > 10 m/s  or  NaN encounter in the tests'
         CALL wrt_line( ctmp2, kt, 'e3t0+ssh min',  zmax(1), iloc(:,1), iareasum(1), iareamin(1), iareamax(1) )
         CALL wrt_line( ctmp3, kt, '   |U|   max',  zmax(2), iloc(:,2), iareasum(2), iareamin(2), iareamax(2) )
         IF( Agrif_Root() ) THEN
            WRITE(ctmp6,*) '      ===> output of last computed fields in output.abort* files'
         ELSE
            WRITE(ctmp6,*) '      ===> output of last computed fields in '//TRIM(Agrif_CFixed())//'_output.abort* files'
         ENDIF
         !
         CALL dia_wri_state( Kmm, 'output.abort' )     ! create an output.abort file
         !
         IF( ll_colruns .OR. jpnij == 1 ) THEN   ! all processes synchronized -> use lwp to print in opened ocean.output files
            IF(lwp) THEN   ;   CALL ctl_stop( ctmp1, ' ', ctmp2, ctmp3, ' ', ctmp6 )
            ELSE           ;   nstop = MAX(1, nstop)   ! make sure nstop > 0 (automatically done when calling ctl_stop)
            ENDIF
         ELSE                                    ! only mpi subdomains with errors are here -> STOP now
            CALL ctl_stop( 'STOP', ctmp1, ' ', ctmp2, ctmp3, ' ', ctmp6 )
         ENDIF
         !
      ENDIF
      !
      IF( nstop > 0 ) THEN                                                  ! an error was detected and we did not abort yet...
         ngrdstop = Agrif_Fixed()                                           ! store which grid got this error
         IF( .NOT. ll_colruns .AND. jpnij > 1 )   CALL ctl_stop( 'STOP' )   ! we must abort here to avoid MPI deadlock
      ENDIF
      !
9500  FORMAT(' it :', i8, ' e3t0+ssh_min: ', D23.16, ' |U|_max: ', D23.16)
      !
      IF( ln_timing )   CALL timing_stop( 'stp_ctl' )
      !
   END SUBROUTINE stp_ctl


   SUBROUTINE wrt_line( cdline, kt, cdprefix, pval, kloc, ksum, kmin, kmax )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE wrt_line  ***
      !!
      !! ** Purpose :   write information line
      !!
      !!----------------------------------------------------------------------
      CHARACTER(len=*),      INTENT(  out) ::   cdline
      CHARACTER(len=*),      INTENT(in   ) ::   cdprefix
      REAL(wp),              INTENT(in   ) ::   pval
      INTEGER, DIMENSION(3), INTENT(in   ) ::   kloc
      INTEGER,               INTENT(in   ) ::   kt, ksum, kmin, kmax
      !
      CHARACTER(len=80) ::   clsuff
      CHARACTER(len=9 ) ::   clkt, clsum, clmin, clmax
      CHARACTER(len=9 ) ::   cli, clj, clk
      CHARACTER(len=1 ) ::   clfmt
      CHARACTER(len=4 ) ::   cl4   ! needed to be able to compile with Agrif, I don't know why
      INTEGER           ::   ifmtk
      !!----------------------------------------------------------------------
      WRITE(clkt , '(i9)') kt
      
      WRITE(clfmt, '(i1)') INT(LOG10(REAL(jpnij  ,wp))) + 1     ! how many digits to we need to write ? (we decide max = 9)
      !!! WRITE(clsum, '(i'//clfmt//')') ksum                   ! this is creating a compilation error with AGRIF
      cl4 = '(i'//clfmt//')'   ;   WRITE(clsum, cl4) ksum
      WRITE(clfmt, '(i1)') INT(LOG10(REAL(MAX(1,jpnij-1),wp))) + 1    ! how many digits to we need to write ? (we decide max = 9)
      cl4 = '(i'//clfmt//')'   ;   WRITE(clmin, cl4) kmin-1
                                   WRITE(clmax, cl4) kmax-1
      !
      WRITE(clfmt, '(i1)') INT(LOG10(REAL(jpiglo,wp))) + 1      ! how many digits to we need to write jpiglo? (we decide max = 9)
      cl4 = '(i'//clfmt//')'   ;   WRITE(cli, cl4) kloc(1)      ! this is ok with AGRIF
      WRITE(clfmt, '(i1)') INT(LOG10(REAL(jpjglo,wp))) + 1      ! how many digits to we need to write jpjglo? (we decide max = 9)
      cl4 = '(i'//clfmt//')'   ;   WRITE(clj, cl4) kloc(2)      ! this is ok with AGRIF
      !
      IF( ksum == 1 ) THEN   ;   WRITE(clsuff,9100) TRIM(clmin)
      ELSE                   ;   WRITE(clsuff,9200) TRIM(clsum), TRIM(clmin), TRIM(clmax)
      ENDIF
      IF(kloc(3) == 0) THEN
         ifmtk = INT(LOG10(REAL(jpk,wp))) + 1                   ! how many digits to we need to write jpk? (we decide max = 9)
         clk = REPEAT(' ', ifmtk)                               ! create the equivalent in blank string
         WRITE(cdline,9300) TRIM(ADJUSTL(clkt)), TRIM(ADJUSTL(cdprefix)), pval, TRIM(cli), TRIM(clj), clk(1:ifmtk), TRIM(clsuff)
      ELSE
         WRITE(clfmt, '(i1)') INT(LOG10(REAL(jpk,wp))) + 1      ! how many digits to we need to write jpk? (we decide max = 9)
         !!! WRITE(clk, '(i'//clfmt//')') kloc(3)               ! this is creating a compilation error with AGRIF
         cl4 = '(i'//clfmt//')'   ;   WRITE(clk, cl4) kloc(3)   ! this is ok with AGRIF
         WRITE(cdline,9400) TRIM(ADJUSTL(clkt)), TRIM(ADJUSTL(cdprefix)), pval, TRIM(cli), TRIM(clj),    TRIM(clk), TRIM(clsuff)
      ENDIF
      !
9100  FORMAT('MPI rank ', a)
9200  FORMAT('found in ', a, ' MPI tasks, spread out among ranks ', a, ' to ', a)
9300  FORMAT('kt ', a, ' ', a, ' ', 1pg11.4, ' at i j   ', a, ' ', a, ' ', a, ' ', a)
9400  FORMAT('kt ', a, ' ', a, ' ', 1pg11.4, ' at i j k ', a, ' ', a, ' ', a, ' ', a)
      !
   END SUBROUTINE wrt_line


   !!======================================================================
END MODULE stpctl
