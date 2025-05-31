MODULE obs_read_mod_fbd_conver
   !!======================================================================
   !!                       ***  MODULE obs_read_mod_fbd_conver  ***
   !! Observation diagnostics: Get the model variables for freeboard conversion to thickness
   !!======================================================================
   !! History :      ! 2018-10 (E. Fiedler)  Subroutine to read and interpolate model snow depth
   !!                ! 2022-07 (D. Carneiro) Subroutine to read and interpolate model seawater density
   !!
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   obs_read_mod_fbd_conver: Reading model variables for freeboard conversion to thickness
   !!
   !!
   !!----------------------------------------------------------------------
   USE par_kind         ! Precision variables
   USE par_oce          ! Domain parameters
   USE in_out_manager   ! I/O manager
   USE obs_surf_def     ! Surface observation definitions
   USE obs_inter_sup    ! Interpolation support routines
   USE obs_inter_h2d    ! 2D interpolation
   USE obs_utils        ! Various observation tools
   USE iom_nf90         ! IOM NetCDF
   USE netcdf           ! NetCDF library
   USE lib_mpp          ! MPP library
   USE dom_oce, ONLY : &                  ! Domain variables
      &                    tmask, tmask_i, e1t, e2t, gphit, glamt
   USE obs_const, ONLY :   obfillflt      ! Fillvalue

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC   obs_rea_snowdepth     ! called by dia_obs
   PUBLIC   obs_rea_rho_seawater  ! called by dia_obs

   !!----------------------------------------------------------------------
   !! NEMO/OBS 5.0 , NEMO Consortium (2024)
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE obs_rea_snowdepth( fbddata, k2dint, kfbd, ksnow, thick_s )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_snowdepth ***
      !!
      !! ** Purpose : Get snowdepth at observation points
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!----------------------------------------------------------------------
      USE iom
      !
      TYPE(obs_surf), INTENT(inout)  ::   fbddata   ! Sea ice freeboard data
      INTEGER       , INTENT(in)     ::   k2dint    ! Type of horizontal interpolation method
      INTEGER       , INTENT(in)    ::    kfbd      ! Index of freeboard var
      INTEGER       , INTENT(in)    ::    ksnow     ! Index of snow thickness extra var
      REAL(wp), INTENT(IN), DIMENSION(jpi,jpj) :: thick_s ! Model snow depth
     
      CHARACTER(LEN=12), PARAMETER ::   cpname  = 'obs_rea_snowdepth'

      INTEGER ::   jobs                ! Obs loop variable
      INTEGER ::   jpi_thick_s, jpj_thick_s      ! Number of grid point in lat/lon for the snow depth
      INTEGER ::   iico, ijco          ! Grid point indices

      !
      REAL(wp), DIMENSION(1)     ::   zext, zobsmask
      REAL(wp), DIMENSION(2,2,1) ::   zweig
      !
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   zmask, z_thick_s_l, zglam, zgphi
      INTEGER , DIMENSION(:,:,:), ALLOCATABLE ::   igrdi, igrdj
      !
      REAL(wp), DIMENSION(jpi,jpj) ::  z_thick_s, thick_s_mask
         
      REAL(wp) :: zlam, zphi, zfill, zinfill    ! local scalar
      !!----------------------------------------------------------------------

      IF(lwp)WRITE(numout,*) 
      IF(lwp)WRITE(numout,*) ' obs_rea_snowdepth : Get model snow depth for freeboard conversion to sea ice thickness'
      IF(lwp)WRITE(numout,*) ' ------------- '
      CALL FLUSH(numout)

      ! Get ice thickness information
      z_thick_s = thick_s

      ! Setup mask based on tmask
      thick_s_mask(:,:) = tmask(:,:,1)

      ! Interpolate the snow depth already on the model grid at the observation points
  
      ALLOCATE( &
         & igrdi(2,2,fbddata%nsurf), &
         & igrdj(2,2,fbddata%nsurf), &
         & zglam(2,2,fbddata%nsurf), &
         & zgphi(2,2,fbddata%nsurf), &
         & zmask(2,2,fbddata%nsurf), &
         & z_thick_s_l(2,2,fbddata%nsurf)  &
         & )
         
      DO jobs = 1, fbddata%nsurf

         igrdi(1,1,jobs) = fbddata%mi(jobs,kfbd)-1
         igrdj(1,1,jobs) = fbddata%mj(jobs,kfbd)-1
         igrdi(1,2,jobs) = fbddata%mi(jobs,kfbd)-1
         igrdj(1,2,jobs) = fbddata%mj(jobs,kfbd)
         igrdi(2,1,jobs) = fbddata%mi(jobs,kfbd)
         igrdj(2,1,jobs) = fbddata%mj(jobs,kfbd)-1
         igrdi(2,2,jobs) = fbddata%mi(jobs,kfbd)
         igrdj(2,2,jobs) = fbddata%mj(jobs,kfbd)

      END DO

      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, glamt, zglam )
      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, gphit, zgphi )
      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, thick_s_mask, zmask )
      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, z_thick_s, z_thick_s_l )

      DO jobs = 1, fbddata%nsurf
            
         zlam = fbddata%rlam(jobs)
         zphi = fbddata%rphi(jobs)

         CALL obs_int_h2d_init( 1, 1, k2dint, zlam, zphi,         &
            &                   zglam(:,:,jobs), zgphi(:,:,jobs), &
            &                   zmask(:,:,jobs), zweig, zobsmask )
            
         CALL obs_int_h2d( 1, 1, zweig, z_thick_s_l(:,:,jobs),  zext )
 
         fbddata%rext(jobs,ksnow) = zext(1)

         ! mark any masked data with a QC flag
         IF( zobsmask(1) == 0.0_wp )   fbddata%nqc(jobs) = IBSET(fbddata%nqc(jobs),15)

         END DO
         
      DEALLOCATE( &
         & igrdi, &
         & igrdj, &
         & zglam, &
         & zgphi, &
         & zmask, &
         & z_thick_s_l  &
         & )

      IF(lwp)WRITE(numout,*) ' ------------- '
      !
   END SUBROUTINE obs_rea_snowdepth

   SUBROUTINE obs_rea_rho_seawater( fbddata, k2dint, kfbd, krho, prhosw )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rea_rho_seawater ***
      !!
      !! ** Purpose : Get sea water density at observation points
      !!
      !! ** Method  : 
      !!
      !! ** Action  : 
      !!----------------------------------------------------------------------
      USE iom
      !
      TYPE(obs_surf), INTENT(inout)  ::   fbddata   ! Sea ice freeboard data
      INTEGER       , INTENT(in)     ::   k2dint    ! Type of horizontal interpolation method
      INTEGER       , INTENT(in)     ::   kfbd      ! Index of freeboard var
      INTEGER       , INTENT(in)     ::   krho      ! Index of sea water density extra var
      REAL(wp), INTENT(IN), DIMENSION(jpi,jpj) ::   prhosw   ! Model sea water density
     
      CHARACTER(LEN=12), PARAMETER ::   cpname  = 'obs_rea_rho_seawater'
      INTEGER ::   jobs                ! Obs loop variable

      !
      REAL(wp), DIMENSION(1)     ::   zext, zobsmask
      REAL(wp), DIMENSION(2,2,1) ::   zweig
      !
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   zmask, z_rhosw_l, zglam, zgphi
      INTEGER , DIMENSION(:,:,:), ALLOCATABLE ::   igrdi, igrdj
      !
      REAL(wp), DIMENSION(jpi,jpj) ::   z_rhosw, zrhosw_mask
         
      REAL(wp) :: zlam, zphi    ! local scalar
      !!----------------------------------------------------------------------

      IF(lwp)WRITE(numout,*) 
      IF(lwp)WRITE(numout,*) ' obs_rea_rho_seawater : Get model sea water density for freeboard conversion to ice thickness'
      IF(lwp)WRITE(numout,*) ' ------------- '
      CALL FLUSH(numout)

      ! Get sea water density information
      z_rhosw = prhosw

      ! Setup mask based on tmask
      zrhosw_mask(:,:) = tmask(:,:,1)

      ! Interpolate the sea water density already on the model grid at the observation points
  
      ALLOCATE( &
         & igrdi(2,2,fbddata%nsurf), &
         & igrdj(2,2,fbddata%nsurf), &
         & zglam(2,2,fbddata%nsurf), &
         & zgphi(2,2,fbddata%nsurf), &
         & zmask(2,2,fbddata%nsurf), &
         & z_rhosw_l(2,2,fbddata%nsurf)  &
         & )
         
      DO jobs = 1, fbddata%nsurf

         igrdi(1,1,jobs) = fbddata%mi(jobs,kfbd)-1
         igrdj(1,1,jobs) = fbddata%mj(jobs,kfbd)-1
         igrdi(1,2,jobs) = fbddata%mi(jobs,kfbd)-1
         igrdj(1,2,jobs) = fbddata%mj(jobs,kfbd)
         igrdi(2,1,jobs) = fbddata%mi(jobs,kfbd)
         igrdj(2,1,jobs) = fbddata%mj(jobs,kfbd)-1
         igrdi(2,2,jobs) = fbddata%mi(jobs,kfbd)
         igrdj(2,2,jobs) = fbddata%mj(jobs,kfbd)

      END DO

      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, glamt, zglam )
      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, gphit, zgphi )
      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, zrhosw_mask, zmask )
      CALL obs_int_comm_2d( 2, 2, fbddata%nsurf, jpi, jpj, igrdi, igrdj, z_rhosw, z_rhosw_l )

      DO jobs = 1, fbddata%nsurf
            
         zlam = fbddata%rlam(jobs)
         zphi = fbddata%rphi(jobs)

         CALL obs_int_h2d_init( 1, 1, k2dint, zlam, zphi,         &
            &                   zglam(:,:,jobs), zgphi(:,:,jobs), &
            &                   zmask(:,:,jobs), zweig, zobsmask )
            
         CALL obs_int_h2d( 1, 1, zweig, z_rhosw_l(:,:,jobs),  zext )
 
         fbddata%rext(jobs,krho) = zext(1)

         ! mark any masked data with a QC flag
         IF( zobsmask(1) == 0.0_wp )   fbddata%nqc(jobs) = IBSET(fbddata%nqc(jobs),15)

         END DO
         
      DEALLOCATE( &
         & igrdi, &
         & igrdj, &
         & zglam, &
         & zgphi, &
         & zmask, &
         & z_rhosw_l  &
         & )

      IF(lwp)WRITE(numout,*) ' ------------- '
      !
   END SUBROUTINE obs_rea_rho_seawater
   
   !!======================================================================
END MODULE obs_read_mod_fbd_conver
