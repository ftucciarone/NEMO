MODULE restart
   !!======================================================================
   !!                     ***  MODULE  restart  ***
   !! Ocean restart :  write the ocean restart file
   !!======================================================================
   !! History :  OPA  !  1999-11  (M. Imbard)  Original code
   !!   NEMO     1.0  !  2002-08  (G. Madec)  F90: Free form
   !!            2.0  !  2006-07  (S. Masson)  use IOM for restart
   !!            3.3  !  2010-04  (M. Leclair, G. Madec)  modified LF-RA
   !!            - -  !  2010-10  (C. Ethe, G. Madec) TRC-TRA merge (T-S in 4D)
   !!            3.7  !  2014-01  (G. Madec) suppression of curl and hdiv from the restart
   !!             -   !  2014-12  (G. Madec) remove KPP scheme
   !!            4.1  !  2020-11  (S. Techene, G. Madec)  move ssh initiatlisation in rst_read_ssh
   !!             -   !                                   add restart in Shallow Water Eq. case
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   rst_opn       : open the ocean restart file for writting
   !!   rst_write     : write the ocean restart file
   !!   rst_read_open : open the restart file for reading 
   !!   rst_read      : read the ocean restart file
   !!   rst_read_ssh  : ssh set from restart or domcfg.nc file or usr_def_istat_ssh
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_ice        ! only lk_si3
   USE phycst         ! physical constants
   USE eosbn2         ! equation of state
   USE wet_dry        ! Wetting/Drying flux limiting
   USE usrdef_istate, ONLY : usr_def_istate_ssh   ! user defined ssh initial state 
   USE trdmxl_oce     ! ocean active mixed layer tracers trends variables
   USE diu_bulk       ! ???
#if defined key_agrif
#if defined key_si3
   USE agrif_ice_interp
#endif
   USE agrif_oce_interp
#endif
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O module
   USE lib_mpp        ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   rst_opn         ! called by step.F90
   PUBLIC   rst_write       ! called by step.F90
   PUBLIC   rst_read_open   ! called in rst_read_ssh
   PUBLIC   rst_read        ! called by istate.F90
   PUBLIC   rst_read_ssh    ! called by domain.F90
   
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE rst_opn( kt )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE rst_opn  ***
      !!
      !! ** Purpose : + initialization (should be read in the namelist) of nitrst
      !!              + open the restart when we are one time step before nitrst
      !!                   - restart header is defined when kt = nitrst-1
      !!                   - restart data  are written when kt = nitrst
      !!              + define lrst_oce to .TRUE. when we need to define or write the restart
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt     ! ocean time-step
      !!
      CHARACTER(LEN=20)   ::   clkt     ! ocean time-step deine as a character
      CHARACTER(LEN=50)   ::   clname   ! ocean output restart file name
      CHARACTER(lc)       ::   clpath   ! full path to ocean output restart file
      CHARACTER(LEN=52)   ::   clpname   ! ocean output restart file name including prefix for AGRIF
      CHARACTER(LEN=256)  ::   clinfo    ! info character
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN   ! default definitions
         lrst_oce = .FALSE.
         IF( ln_rst_list ) THEN
            nrst_lst = 1
            nitrst = nn_stocklist( nrst_lst )
         ELSE
            nitrst = nitend
         ENDIF
      ENDIF

      IF( .NOT. ln_rst_list .AND. nn_stock == -1 )   RETURN   ! we will never do any restart

      ! frequency-based restart dumping (nn_stock)
      IF( .NOT. ln_rst_list .AND. MOD( kt - 1, nn_stock ) == 0 ) THEN
         ! we use kt - 1 and not kt - nit000 to keep the same periodicity from the beginning of the experiment
         nitrst = kt + nn_stock - 1                  ! define the next value of nitrst for restart writing
         IF( nitrst > nitend )   nitrst = nitend   ! make sure we write a restart at the end of the run
      ENDIF
      ! to get better performances with NetCDF format:
      ! we open and define the ocean restart file one time step before writing the data (-> at nitrst - 1)
      ! except if we write ocean restart files every time step or if an ocean restart file was writen at nitend - 1
      IF( kt == nitrst - 1 .OR. nn_stock == 1 .OR. ( kt == nitend .AND. .NOT. lrst_oce ) ) THEN
         IF( nitrst <= nitend .AND. nitrst > 0 ) THEN
            ! beware of the format used to write kt (default is i8.8, that should be large enough...)
            IF( nitrst > 999999999 ) THEN   ;   WRITE(clkt, *       ) nitrst
            ELSE                            ;   WRITE(clkt, '(i8.8)') nitrst
            ENDIF
            ! create the file
            clname = TRIM(cexper)//"_"//TRIM(ADJUSTL(clkt))//"_"//TRIM(cn_ocerst_out)
            clpath = TRIM(cn_ocerst_outdir)
            IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath) // '/'
            IF(lwp) THEN
               WRITE(numout,*)
               IF(.NOT.lwxios) THEN
                  WRITE(numout,*) '             open ocean restart NetCDF file: ',TRIM(clpath)//TRIM(clname)
                  IF ( snc4set%luse )      WRITE(numout,*) '             opened for NetCDF4 chunking and compression'
                  IF( kt == nitrst - 1 ) THEN   ;   WRITE(numout,*) '             kt = nitrst - 1 = ', kt
                  ELSE                          ;   WRITE(numout,*) '             kt = '             , kt
                  ENDIF
               ENDIF
            ENDIF
            !
            IF(.NOT.lwxios) THEN
               CALL iom_open( TRIM(clpath)//TRIM(clname), numrow, ldwrt = .TRUE. )
            ELSE
#if defined key_xios
               cw_ocerst_cxt = "rstw_"//TRIM(ADJUSTL(clkt))
               IF( TRIM(Agrif_CFixed()) == '0' ) THEN
                  clpname = clname
               ELSE
                  clpname = TRIM(Agrif_CFixed())//"_"//clname
               ENDIF
               numrow = iom_xios_setid(TRIM(clpath)//TRIM(clpname))
               CALL iom_init( cw_ocerst_cxt, kdid = numrow, ld_closedef = .false. )
               CALL iom_swap(      cxios_context          )
#else
               clinfo = 'Can not use XIOS in rst_opn'
               CALL ctl_stop(TRIM(clinfo))
#endif
            ENDIF
            lrst_oce = .TRUE.
         ENDIF
      ENDIF
      !
   END SUBROUTINE rst_opn


   SUBROUTINE rst_write( kt, Kbb, Kmm, Kaa )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE rstwrite  ***
      !!
      !! ** Purpose :   Write restart fields in NetCDF format
      !!
      !! ** Method  :   Write in numrow when kt == nitrst in NetCDF
      !!              file, save fields which are necessary for restart
      !!
      !!                NB: ssh is written here (rst_write)
      !!                    but is read or set in rst_read_ssh
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt         ! ocean time-step
      INTEGER, INTENT(in) ::   Kbb, Kmm   ! ocean time level indices
      INTEGER, OPTIONAL, INTENT(in) ::   Kaa        ! ocean time level index required for RK3
      !!----------------------------------------------------------------------
      !
      CALL iom_rstput( kt, nitrst, numrow, 'rdt', rn_Dt )   ! dynamics time step
      !
      CALL iom_delay_rst( 'WRITE', numrow, kt )   ! save all delayed global communication variables
      !
      IF( .NOT.ln_diurnal_only ) THEN
         !
#if defined key_RK3
         CALL iom_rstput( kt, nitrst, numrow, 'sshn', ssh(:,:        ,Kbb) )     ! before fields
         CALL iom_rstput( kt, nitrst, numrow, 'un'  , uu(:,:,:       ,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'vn'  , vv(:,:,:       ,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'tn'  , ts(:,:,:,jp_tem,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'sn'  , ts(:,:,:,jp_sal,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'uu_n', uu_b(:,:       ,Kbb) )     
         CALL iom_rstput( kt, nitrst, numrow, 'vv_n', vv_b(:,:       ,Kbb) )
         !
         IF( PRESENT(Kaa) )   CALL iom_rstput( kt, nitrst, numrow, 'ssha', ssh(:,:,Kaa) )   ! after  fields
#else
         CALL iom_rstput( kt, nitrst, numrow, 'sshb', ssh(:,:        ,Kbb) )     ! before fields
         CALL iom_rstput( kt, nitrst, numrow, 'ub'  , uu(:,:,:       ,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'vb'  , vv(:,:,:       ,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'tb'  , ts(:,:,:,jp_tem,Kbb) )
         CALL iom_rstput( kt, nitrst, numrow, 'sb'  , ts(:,:,:,jp_sal,Kbb) )
         !
         CALL iom_rstput( kt, nitrst, numrow, 'sshn', ssh(:,:        ,Kmm) )     ! now fields 
         CALL iom_rstput( kt, nitrst, numrow, 'un'  , uu(:,:,:       ,Kmm) )
         CALL iom_rstput( kt, nitrst, numrow, 'vn'  , vv(:,:,:       ,Kmm) )
         CALL iom_rstput( kt, nitrst, numrow, 'tn'  , ts(:,:,:,jp_tem,Kmm) )
         CALL iom_rstput( kt, nitrst, numrow, 'sn'  , ts(:,:,:,jp_sal,Kmm) )
#endif
      ENDIF

      IF( ln_diurnal )   CALL iom_rstput( kt, nitrst, numrow, 'Dsst', x_dsst )
      IF( kt == nitrst ) THEN
         IF( .NOT.lwxios ) THEN
            CALL iom_close( numrow )     ! close the restart file (only at last time step)
         ELSE
            CALL iom_context_finalize(      cw_ocerst_cxt          )
            iom_file(numrow)%nfid       = 0
            numrow = 0
         ENDIF
!!gm         IF( .NOT. lk_trdmld )   lrst_oce = .FALSE.
!!gm  not sure what to do here   ===>>>  ask to Sebastian
         lrst_oce = .FALSE.
         IF( ln_rst_list ) THEN
            nrst_lst = MIN(nrst_lst + 1, SIZE(nn_stocklist,1))
            nitrst   = nn_stocklist( nrst_lst )
         ENDIF
      ENDIF
      !
   END SUBROUTINE rst_write


   SUBROUTINE rst_read_open
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE rst_read_open  ***
      !!
      !! ** Purpose :   Open read files for NetCDF restart
      !!
      !! ** Method  :   Use a non-zero, positive value of numror to assess whether or not
      !!                the file has already been opened
      !!----------------------------------------------------------------------
      LOGICAL             ::   llok
      CHARACTER(len=lc)   ::   clpath   ! full path to ocean output restart file
      CHARACTER(len=lc+2) ::   clpname  ! file name including agrif prefix
      !!----------------------------------------------------------------------
      !
      IF( numror <= 0 ) THEN
         IF(lwp) THEN                                             ! Contol prints
            WRITE(numout,*)
            WRITE(numout,*) 'rst_read : read oce NetCDF restart file'
            IF ( snc4set%luse )      WRITE(numout,*) 'rst_read : configured with NetCDF4 support'
            WRITE(numout,*) '~~~~~~~~'
         ENDIF
         lxios_sini = .FALSE.
         clpath = TRIM(cn_ocerst_indir)
         IF( clpath(LEN_TRIM(clpath):) /= '/' ) clpath = TRIM(clpath) // '/'
         CALL iom_open( TRIM(clpath)//cn_ocerst_in, numror )
! are we using XIOS to read the data? Part above will have to modified once XIOS
! can handle checking if variable is in the restart file (there will be no need to open
! restart)
         lrxios = lrxios.AND.lxios_sini

         IF( lrxios) THEN
             cr_ocerst_cxt = 'oce_rst'
             IF(lwp) WRITE(numout,*) 'Enable restart reading by XIOS'
!            IF( TRIM(Agrif_CFixed()) == '0' ) THEN
!               clpname = cn_ocerst_in
!            ELSE
!               clpname = TRIM(Agrif_CFixed())//"_"//cn_ocerst_in
!            ENDIF
             CALL iom_init( cr_ocerst_cxt, kdid = numror, ld_closedef = .TRUE. )
             CALL iom_swap(      cxios_context          )
         ENDIF

      ENDIF

   END SUBROUTINE rst_read_open


   SUBROUTINE rst_read( Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE rst_read  ***
      !!
      !! ** Purpose :   Read velocity and T-S fields in the restart file
      !!
      !! ** Method  :   Read in restart.nc fields which are necessary for restart
      !!
      !!                NB: restart file openned           in DOM/domain.F90:dom_init
      !!                    before field in restart tested in DOM/domain.F90:dom_init
      !!                    (sshb)
      !!
      !!                NB: ssh is read or set in rst_read_ssh
      !!----------------------------------------------------------------------
      INTEGER          , INTENT(in) ::   Kbb, Kmm   ! ocean time level indices
      INTEGER  ::   ji, jk
      INTEGER  ::   id1 
      INTEGER  ::   idvar
      INTEGER, DIMENSION(1)  ::   idimsz 
      REAL(wp), DIMENSION(jpi, jpj, jpk) :: w3d
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)   :: zgdept       ! 3D workspace for QCO
      !!----------------------------------------------------------------------
      !
      CALL iom_delay_rst( 'READ', numror )   ! read all delayed global communication variables
      !
      !                             !*  Diurnal DSST
      IF( ln_diurnal )   CALL iom_get( numror, jpdom_auto, 'Dsst' , x_dsst )
      IF( ln_diurnal_only ) THEN
         IF(lwp) WRITE( numout, * ) &
         &   "rst_read:- ln_diurnal_only set, setting rhop=rho0"
         rhop = rho0
         CALL iom_get( numror, jpdom_auto, 'tn'     , w3d )
         ts(:,:,1,jp_tem,Kmm) = w3d(:,:,1)
         RETURN
      ENDIF
      !
#if defined key_RK3
      !                             !*  Read Kbb fields   (NB: in RK3 Kmm = Kbb = Nbb)
      IF(lwp) WRITE(numout,*) '           Kbb u, v and T-S fields read in the restart file'
      CALL iom_get( numror, jpdom_auto, 'un'   , uu(:,:,:       ,Kbb), cd_type = 'U', psgn = -1._wp )
      CALL iom_get( numror, jpdom_auto, 'vn'   , vv(:,:,:       ,Kbb), cd_type = 'V', psgn = -1._wp )
      CALL iom_get( numror, jpdom_auto, 'tn'   , ts(:,:,:,jp_tem,Kbb) )
      CALL iom_get( numror, jpdom_auto, 'sn'   , ts(:,:,:,jp_sal,Kbb) )
      id1 = iom_varid( numror, 'uu_n', ldstop = .FALSE. )  !*  check presence
      IF( id1 > 0 ) THEN
         CALL iom_get( numror, jpdom_auto, 'uu_n' , uu_b(:,:,Kbb), cd_type = 'U', psgn = -1._wp )
         CALL iom_get( numror, jpdom_auto, 'vv_n' , vv_b(:,:,Kbb), cd_type = 'V', psgn = -1._wp )
      ELSE
         uu_b(:,:,Kbb) = uu(:,:,1,Kbb)*e3u_0(:,:,1)*umask(:,:,1)
         vv_b(:,:,Kbb) = vv(:,:,1,Kbb)*e3v_0(:,:,1)*vmask(:,:,1)
         DO jk = 2, jpkm1
            uu_b(:,:,Kbb) =  uu_b(:,:,Kbb) + uu(:,:,jk,Kbb)*e3u_0(:,:,jk)*umask(:,:,jk)
            vv_b(:,:,Kbb) =  vv_b(:,:,Kbb) + vv(:,:,jk,Kbb)*e3v_0(:,:,jk)*vmask(:,:,jk)
         END DO
         uu_b(:,:,Kbb) = uu_b(:,:,Kbb) * r1_hu_0(:,:)
         vv_b(:,:,Kbb) = vv_b(:,:,Kbb) * r1_hv_0(:,:)
      ENDIF
      uu(:,:,:  ,Kmm) = uu(:,:,:  ,Kbb)         ! Kmm values set to Kbb for initialisation (sbc_ssm_init)
      vv(:,:,:  ,Kmm) = vv(:,:,:  ,Kbb)
      ts(:,:,:,:,Kmm) = ts(:,:,:,:,Kbb)
      !
      uu_b(:,:,Kmm)   = uu_b(:,:,Kbb)           ! Kmm value set to Kbb for initialisation in Agrif_Regrid
      vv_b(:,:,Kmm)   = vv_b(:,:,Kbb)
#else
      !                             !*  Read Kmm fields   (MLF only)
      IF(lwp) WRITE(numout,*)    '           Kmm u, v and T-S fields read in the restart file'
      CALL iom_get( numror, jpdom_auto, 'un', uu(:,:,:       ,Kmm), cd_type = 'U', psgn = -1._wp )
      CALL iom_get( numror, jpdom_auto, 'vn', vv(:,:,:       ,Kmm), cd_type = 'V', psgn = -1._wp )
      CALL iom_get( numror, jpdom_auto, 'tn', ts(:,:,:,jp_tem,Kmm) )
      CALL iom_get( numror, jpdom_auto, 'sn', ts(:,:,:,jp_sal,Kmm) )
      !
      IF( l_1st_euler ) THEN        !*  Euler restart   (MLF only)
         IF(lwp) WRITE(numout,*) '           Kbb u, v and T-S fields set to Kmm values'
         uu(:,:,:  ,Kbb) = uu(:,:,:  ,Kmm)         ! all before fields set to now values
         vv(:,:,:  ,Kbb) = vv(:,:,:  ,Kmm)
         ts(:,:,:,:,Kbb) = ts(:,:,:,:,Kmm)
         !
      ELSE                          !* Leap frog restart   (MLF only)
         IF(lwp) WRITE(numout,*) '           Kbb u, v and T-S fields read in the restart file'
         CALL iom_get( numror, jpdom_auto, 'ub', uu(:,:,:       ,Kbb), cd_type = 'U', psgn = -1._wp )
         CALL iom_get( numror, jpdom_auto, 'vb', vv(:,:,:       ,Kbb), cd_type = 'V', psgn = -1._wp )
         CALL iom_get( numror, jpdom_auto, 'tb', ts(:,:,:,jp_tem,Kbb) )
         CALL iom_get( numror, jpdom_auto, 'sb', ts(:,:,:,jp_sal,Kbb) )
      ENDIF
#endif
      !
   END SUBROUTINE rst_read


   SUBROUTINE rst_read_ssh( Kbb, Kmm, Kaa )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE rst_read_ssh  ***
      !!
      !! ** Purpose :   ssh initialization of the sea surface height (ssh)
      !!
      !! ** Method  :   set ssh from restart or read configuration, or user_def
      !!              * ln_rstart = T
      !!                   USE of IOM library to read ssh in the restart file
      !!                   Leap-Frog: Kbb and Kmm are read except for l_1st_euler=T
      !!
      !!              * otherwise 
      !!                   call user defined ssh or
      !!                   set to -ssh_ref in wet and drying case with domcfg.nc
      !!
      !!              NB: ssh_b/n are written by restart.F90
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa   ! ocean time level indices
      !
      INTEGER ::   ji, jj, jk   ! dummy loop arguments
      INTEGER ::   id1          ! local integer
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'rst_read_ssh : ssh initialization'
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      !
      !                            !=============================!
      IF( ln_rstart ) THEN         !==  Read the restart file  ==!
         !                         !=============================!
         !
#if defined key_RK3
         !                                     !*  RK3: Read ssh at Kbb
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*)    '      Kbb sea surface height read in the restart file'
         CALL iom_get( numror, jpdom_auto, 'sshn'   , ssh(:,:,Kbb) )
         !
         !                                     !*  RK3: Set  ssh at Kmm (for AGRIF)
         ssh(:,:,Kmm) = ssh(:,:,Kbb)
         !                                     !*  RK3: Set  ssh at Kaa (for stp2d)
         id1 = iom_varid( numror, 'ssha', ldstop = .FALSE. )
         !
         IF( id1 > 0 ) THEN                        ! restart from RK3 run
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*)    '      Kaa extrapolated sea surface height read in the restart file'
            CALL iom_get( numror, jpdom_auto, 'ssha'   , ssh(:,:,Kaa) )
            !
         ELSE                                      ! restart from MLF run
            ssh(:,:,Kaa) = ssh(:,:,Kbb)               ! no ssh variation in ww computation
         ENDIF
#else
         !                                     !*  MLF: Read ssh at Kmm
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*)    '      Kmm sea surface height read in the restart file'
         CALL iom_get( numror, jpdom_auto, 'sshn'   , ssh(:,:,Kmm) )
         !
         IF( l_1st_euler ) THEN                !*  MLF: Euler at first time-step
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '      Euler first time step : ssh(Kbb) = ssh(Kmm)'
            ssh(:,:,Kbb) = ssh(:,:,Kmm)
#if defined key_agrif
            ! Set ghosts points from parent 
            IF (.NOT.Agrif_Root()) CALL Agrif_istate_ssh( Kbb, Kmm, Kaa, .true. )
#endif
            !
         ELSE                                  !*  MLF: read ssh at Kbb
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '      Kbb sea surface height read in the restart file'
            CALL iom_get( numror, jpdom_auto, 'sshb', ssh(:,:,Kbb) )
         ENDIF
         !
         ssh(:,:,Kaa) = 0._wp                  !*  MLF: set  ssh at Kaa (for AGRIF)
         !
#endif
         !                         !============================!
      ELSE                         !==  Initialize at "rest"  ==!
         !                         !============================!
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*)    '      initialization at rest'
         !
         IF( ll_wd ) THEN                      !* wet and dry 
            !
            IF( ln_read_cfg ) THEN                 ! read configuration : ssh_ref is read in domain_cfg file
!!st  why ssh is not masked : i.e. ssh(:,:,Kmm) = -ssh_ref*ssmask(:,:),
!!st  since at the 1st time step lbclnk will be applied on ssh at Kaa but not initially at Kbb and Kmm
               ssh(:,:,Kbb) = -ssh_ref
               !
               DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
                  IF( ht_0(ji,jj)-ssh_ref <  rn_wdmin1 ) THEN   ! if total depth is less than min depth
                     ssh(ji,jj,Kbb) = rn_wdmin1 - ht_0(ji,jj)
                  ENDIF
               END_2D
            ELSE                                   ! user define configuration case  
               CALL usr_def_istate_ssh( tmask, ssh(:,:,Kbb) )
            ENDIF
            !
         ELSE                                  !* no wet and dry 
            IF( ln_read_cfg ) THEN                 ! read configuration : ssh is set to 0
               IF(lwp) WRITE(numout,*) '      rst_read_ssh : Ocean at rest, ssh is zero'
               ssh(:,:,Kbb) = 0._wp
            ELSE                                   ! user define configuration case  
               CALL usr_def_istate_ssh( tmask, ssh(:,:,Kbb) )
            ENDIF
            !
         ENDIF
         !
         ssh(:,:,Kaa) = ssh(:,:,Kbb)           !*  set ssh at Kaa (for AGRIF)
         !
#if defined key_agrif
         ! Set ghosts points from parent 
         IF (.NOT.Agrif_Root()) THEN 
            ! Set ghosts points from parent 
            CALL Agrif_istate_ssh( Kbb, Kmm, Kaa, .true. ) 
#if defined key_si3
            ! Possibly add ssh increment from parent grid
            ! only if there is no ice model in the child grid
            CALL Agrif_istate_icevol( Kbb, Kmm, Kaa ) 
#endif
         ENDIF
#endif
         !
         ssh(:,:,Kmm) = ssh(:,:,Kbb)           !*  set now values from to before ones
         !
      ENDIF
      !
   END SUBROUTINE rst_read_ssh

   !!=====================================================================
END MODULE restart
