MODULE obs_rot_vel
   !!======================================================================
   !!                       ***  MODULE obs_rot_vel  ***
   !! Observation diagnostics: Read the velocity profile observations
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_rotvel : Rotate velocity data into N-S,E-W directorions
   !!----------------------------------------------------------------------
   !! * Modules used   
   USE par_kind                 ! Precision variables
   USE par_oce                  ! Ocean parameters
   USE in_out_manager           ! I/O manager
   USE dom_oce                  ! Ocean space and time domain variables
   USE obs_grid                 ! Grid search
   USE obs_utils                ! For error handling
   USE obs_profiles_def         ! Profile definitions
   USE obs_surf_def             ! Surface definitions
   USE obs_inter_h2d            ! Horizontal interpolation
   USE obs_inter_sup            ! MPP support routines for interpolation
   USE geo2ocean                ! Rotation of vectors
   USE obs_fbm                  ! Feedback definitions

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE

   PUBLIC obs_rotvel_pro        ! Rotate the profile velocity observations
   PUBLIC obs_rotvel_surf       ! Rotate the surface velocity observations

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE obs_rotvel_pro( profdata, k2dint, kuvar, kvvar, pu, pv )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rotvel_pro ***
      !!
      !! ** Purpose : Rotate velocity data into N-S,E-W directions
      !!
      !! ** Method  : Interpolation of geo2ocean coefficients on U,V grid
      !!              to observation point followed by a similar computations
      !!              as in geo2ocean.
      !!
      !! ** Action  : Review if there is a better way to do this.
      !!
      !! References : 
      !!
      !! History :  
      !!      ! :  2009-02 (K. Mogensen) : New routine
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * Arguments
      TYPE(obs_prof), INTENT(INOUT) :: profdata    ! Profile data to be read
      INTEGER, INTENT(IN) :: k2dint     ! Horizontal interpolation method
      INTEGER, INTENT(IN) :: kuvar      ! Index of U velocity
      INTEGER, INTENT(IN) :: kvvar      ! Index of V velocity
      REAL(wp), DIMENSION(:), INTENT(OUT) :: &
         & pu, &
         & pv
      !! * Local declarations
      REAL(wp), DIMENSION(2,2,1) :: zweig
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zmasku, &
         & zmaskv, &
         & zcoslu, &
         & zsinlu, &
         & zcoslv, &
         & zsinlv, &
         & zglamu, &
         & zgphiu, &
         & zglamv, &
         & zgphiv
      REAL(wp), DIMENSION(1) :: &
         & zsinu, &
         & zcosu, &
         & zsinv, &
         & zcosv
      REAL(wp) :: zsin
      REAL(wp) :: zcos
      REAL(wp), DIMENSION(1) :: zobsmask
      REAL(wp), DIMENSION(jpi,jpj) :: zsingu,zcosgu,zsingv,zcosgv
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: &
         & igrdiu, &
         & igrdju, &
         & igrdiv, &
         & igrdjv
      INTEGER :: ji
      INTEGER :: jk


      !-----------------------------------------------------------------------
      ! Allocate data for message parsing and interpolation
      !-----------------------------------------------------------------------

      ALLOCATE( &
         & igrdiu(2,2,profdata%nprof), &
         & igrdju(2,2,profdata%nprof), &
         & zglamu(2,2,profdata%nprof), &
         & zgphiu(2,2,profdata%nprof), &
         & zmasku(2,2,profdata%nprof), &
         & zcoslu(2,2,profdata%nprof), &
         & zsinlu(2,2,profdata%nprof), &
         & igrdiv(2,2,profdata%nprof), &
         & igrdjv(2,2,profdata%nprof), &
         & zglamv(2,2,profdata%nprof), &
         & zgphiv(2,2,profdata%nprof), &
         & zmaskv(2,2,profdata%nprof), &
         & zcoslv(2,2,profdata%nprof), &
         & zsinlv(2,2,profdata%nprof)  &
         & )

      !-----------------------------------------------------------------------
      ! Receive the angles on the U and V grids.
      !-----------------------------------------------------------------------

      CALL obs_rot( zsingu, zcosgu, zsingv, zcosgv )

      DO ji = 1, profdata%nprof
         igrdiu(1,1,ji) = profdata%mi(ji,kuvar)-1
         igrdju(1,1,ji) = profdata%mj(ji,kuvar)-1
         igrdiu(1,2,ji) = profdata%mi(ji,kuvar)-1
         igrdju(1,2,ji) = profdata%mj(ji,kuvar)
         igrdiu(2,1,ji) = profdata%mi(ji,kuvar)
         igrdju(2,1,ji) = profdata%mj(ji,kuvar)-1
         igrdiu(2,2,ji) = profdata%mi(ji,kuvar)
         igrdju(2,2,ji) = profdata%mj(ji,kuvar)
         igrdiv(1,1,ji) = profdata%mi(ji,kvvar)-1
         igrdjv(1,1,ji) = profdata%mj(ji,kvvar)-1
         igrdiv(1,2,ji) = profdata%mi(ji,kvvar)-1
         igrdjv(1,2,ji) = profdata%mj(ji,kvvar)
         igrdiv(2,1,ji) = profdata%mi(ji,kvvar)
         igrdjv(2,1,ji) = profdata%mj(ji,kvvar)-1
         igrdiv(2,2,ji) = profdata%mi(ji,kvvar)
         igrdjv(2,2,ji) = profdata%mj(ji,kvvar)
      END DO

      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiu, igrdju, &
         &                  glamu, zglamu )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiu, igrdju, &
         &                  gphiu, zgphiu )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiu, igrdju, &
         &                  umask(:,:,1), zmasku )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiu, igrdju, &
         &                  zsingu, zsinlu )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiu, igrdju, &
         &                  zcosgu, zcoslu )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiv, igrdjv, &
         &                  glamv, zglamv )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiv, igrdjv, &
         &                  gphiv, zgphiv )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiv, igrdjv, &
         &                  vmask(:,:,1), zmaskv )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiv, igrdjv, &
         &                  zsingv, zsinlv )
      CALL obs_int_comm_2d( 2, 2, profdata%nprof, jpi, jpj, igrdiv, igrdjv, &
         &                  zcosgv, zcoslv )

      DO ji = 1, profdata%nprof
            
         CALL obs_int_h2d_init( 1, 1, k2dint, &
            &                   profdata%rlam(ji), profdata%rphi(ji), &
            &                   zglamu(:,:,ji), zgphiu(:,:,ji), &
            &                   zmasku(:,:,ji), zweig, zobsmask )
         
         CALL obs_int_h2d( 1, 1, zweig, zsinlu(:,:,ji),  zsinu )

         CALL obs_int_h2d( 1, 1, zweig, zcoslu(:,:,ji),  zcosu )

         CALL obs_int_h2d_init( 1, 1, k2dint, &
            &                   profdata%rlam(ji), profdata%rphi(ji), &
            &                   zglamv(:,:,ji), zgphiv(:,:,ji), &
            &                   zmaskv(:,:,ji), zweig, zobsmask )
         
         CALL obs_int_h2d( 1, 1, zweig, zsinlv(:,:,ji),  zsinv )

         CALL obs_int_h2d( 1, 1, zweig, zcoslv(:,:,ji),  zcosv )

         ! Assume that the angle at observation point is the 
         ! mean of u and v cosines/sines

         zcos = 0.5_wp * ( zcosu(1) + zcosv(1) )
         zsin = 0.5_wp * ( zsinu(1) + zsinv(1) )
         
         IF ( ( profdata%npvsta(ji,kuvar) /= profdata%npvsta(ji,kvvar) ) .OR. &
            & ( profdata%npvend(ji,kuvar) /= profdata%npvend(ji,kvvar) ) ) THEN
            CALL fatal_error( 'Different number of U and V observations '// &
               'in a profile in obs_rotvel', __LINE__ )
         ENDIF

         DO jk = profdata%npvsta(ji,kuvar), profdata%npvend(ji,kuvar)
            IF ( ( profdata%var(kuvar)%vmod(jk) /= fbrmdi ) .AND. &
               & ( profdata%var(kvvar)%vmod(jk) /= fbrmdi ) ) THEN
               pu(jk) = profdata%var(kuvar)%vmod(jk) * zcos - &
                  &     profdata%var(kvvar)%vmod(jk) * zsin
               pv(jk) = profdata%var(kvvar)%vmod(jk) * zcos + &
                  &     profdata%var(kuvar)%vmod(jk) * zsin
            ELSE
               pu(jk) = fbrmdi
               pv(jk) = fbrmdi
            ENDIF

         END DO

      END DO
      
      DEALLOCATE( &
         & igrdiu, &
         & igrdju, &
         & zglamu, &
         & zgphiu, &
         & zmasku, &
         & zcoslu, &
         & zsinlu, &
         & igrdiv, &
         & igrdjv, &
         & zglamv, &
         & zgphiv, &
         & zmaskv, &
         & zcoslv, &
         & zsinlv  &
         & )

   END SUBROUTINE obs_rotvel_pro

   SUBROUTINE obs_rotvel_surf( surfdata, k2dint, kuvar, kvvar, pu, pv )
      !!---------------------------------------------------------------------
      !!
      !!                   *** ROUTINE obs_rotvel_surf ***
      !!
      !! ** Purpose : Rotate surface velocity data into N-S,E-W directions
      !!
      !! ** Method  : Interpolation of geo2ocean coefficients on U,V grid
      !!              to observation point followed by a similar computations
      !!              as in geo2ocean.
      !!
      !! ** Action  : Review if there is a better way to do this.
      !!
      !! References : 
      !!
      !! History :  
      !!      ! :  2009-02 (K. Mogensen) : New routine
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * Arguments
      TYPE(obs_surf), INTENT(INOUT) :: surfdata    ! Surface data to be read
      INTEGER, INTENT(IN) :: k2dint     ! Horizontal interpolation method
      INTEGER, INTENT(IN) :: kuvar      ! Index of U velocity
      INTEGER, INTENT(IN) :: kvvar      ! Index of V velocity
      REAL(wp), DIMENSION(:), INTENT(OUT) :: &
         & pu, &
         & pv
      !! * Local declarations
      REAL(wp), DIMENSION(2,2,1) :: zweig
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zmasku, &
         & zmaskv, &
         & zcoslu, &
         & zsinlu, &
         & zcoslv, &
         & zsinlv, &
         & zglamu, &
         & zgphiu, &
         & zglamv, &
         & zgphiv
      REAL(wp), DIMENSION(1) :: &
         & zsinu, &
         & zcosu, &
         & zsinv, &
         & zcosv
      REAL(wp) :: zsin
      REAL(wp) :: zcos
      REAL(wp), DIMENSION(1) :: zobsmask
      REAL(wp), DIMENSION(jpi,jpj) :: zsingu,zcosgu,zsingv,zcosgv
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: &
         & igrdiu, &
         & igrdju, &
         & igrdiv, &
         & igrdjv
      INTEGER :: ji
      INTEGER :: jk

      !-----------------------------------------------------------------------
      ! Allocate data for message parsing and interpolation
      !-----------------------------------------------------------------------

      ALLOCATE( &
         & igrdiu(2,2,surfdata%nsurf), &
         & igrdju(2,2,surfdata%nsurf), &
         & zglamu(2,2,surfdata%nsurf), &
         & zgphiu(2,2,surfdata%nsurf), &
         & zmasku(2,2,surfdata%nsurf), &
         & zcoslu(2,2,surfdata%nsurf), &
         & zsinlu(2,2,surfdata%nsurf), &
         & igrdiv(2,2,surfdata%nsurf), &
         & igrdjv(2,2,surfdata%nsurf), &
         & zglamv(2,2,surfdata%nsurf), &
         & zgphiv(2,2,surfdata%nsurf), &
         & zmaskv(2,2,surfdata%nsurf), &
         & zcoslv(2,2,surfdata%nsurf), &
         & zsinlv(2,2,surfdata%nsurf)  &
         & )

      !-----------------------------------------------------------------------
      ! Receive the angles on the U and V grids.
      !-----------------------------------------------------------------------

      CALL obs_rot( zsingu, zcosgu, zsingv, zcosgv )

      DO ji = 1, surfdata%nsurf
         igrdiu(1,1,ji) = surfdata%mi(ji,kuvar)-1
         igrdju(1,1,ji) = surfdata%mj(ji,kuvar)-1
         igrdiu(1,2,ji) = surfdata%mi(ji,kuvar)-1
         igrdju(1,2,ji) = surfdata%mj(ji,kuvar)
         igrdiu(2,1,ji) = surfdata%mi(ji,kuvar)
         igrdju(2,1,ji) = surfdata%mj(ji,kuvar)-1
         igrdiu(2,2,ji) = surfdata%mi(ji,kuvar)
         igrdju(2,2,ji) = surfdata%mj(ji,kuvar)
         igrdiv(1,1,ji) = surfdata%mi(ji,kvvar)-1
         igrdjv(1,1,ji) = surfdata%mj(ji,kvvar)-1
         igrdiv(1,2,ji) = surfdata%mi(ji,kvvar)-1
         igrdjv(1,2,ji) = surfdata%mj(ji,kvvar)
         igrdiv(2,1,ji) = surfdata%mi(ji,kvvar)
         igrdjv(2,1,ji) = surfdata%mj(ji,kvvar)-1
         igrdiv(2,2,ji) = surfdata%mi(ji,kvvar)
         igrdjv(2,2,ji) = surfdata%mj(ji,kvvar)
      END DO

      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiu, igrdju, &
         &                  glamu, zglamu )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiu, igrdju, &
         &                  gphiu, zgphiu )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiu, igrdju, &
         &                  umask(:,:,1), zmasku )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiu, igrdju, &
         &                  zsingu, zsinlu )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiu, igrdju, &
         &                  zcosgu, zcoslu )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiv, igrdjv, &
         &                  glamv, zglamv )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiv, igrdjv, &
         &                  gphiv, zgphiv )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiv, igrdjv, &
         &                  vmask(:,:,1), zmaskv )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiv, igrdjv, &
         &                  zsingv, zsinlv )
      CALL obs_int_comm_2d( 2, 2, surfdata%nsurf, jpi, jpj, igrdiv, igrdjv, &
         &                  zcosgv, zcoslv )

      DO ji = 1, surfdata%nsurf
            
         CALL obs_int_h2d_init( 1, 1, k2dint, &
            &                   surfdata%rlam(ji), surfdata%rphi(ji), &
            &                   zglamu(:,:,ji), zgphiu(:,:,ji), &
            &                   zmasku(:,:,ji), zweig, zobsmask )
         
         CALL obs_int_h2d( 1, 1, zweig, zsinlu(:,:,ji),  zsinu )

         CALL obs_int_h2d( 1, 1, zweig, zcoslu(:,:,ji),  zcosu )

         CALL obs_int_h2d_init( 1, 1, k2dint, &
            &                   surfdata%rlam(ji), surfdata%rphi(ji), &
            &                   zglamv(:,:,ji), zgphiv(:,:,ji), &
            &                   zmaskv(:,:,ji), zweig, zobsmask )
         
         CALL obs_int_h2d( 1, 1, zweig, zsinlv(:,:,ji),  zsinv )

         CALL obs_int_h2d( 1, 1, zweig, zcoslv(:,:,ji),  zcosv )

         ! Assume that the angle at observation point is the 
         ! mean of u and v cosines/sines

         zcos = 0.5_wp * ( zcosu(1) + zcosv(1) )
         zsin = 0.5_wp * ( zsinu(1) + zsinv(1) )

         IF ( ( surfdata%rmod(ji,kuvar) /= fbrmdi ) .AND. &
            & ( surfdata%rmod(ji,kvvar) /= fbrmdi ) ) THEN
            pu(ji) = surfdata%rmod(ji,kuvar) * zcos - &
               &     surfdata%rmod(ji,kvvar) * zsin
            pv(ji) = surfdata%rmod(ji,kvvar) * zcos + &
               &     surfdata%rmod(ji,kuvar) * zsin
         ELSE
            pu(ji) = fbrmdi
            pv(ji) = fbrmdi
         ENDIF


      END DO
      
      DEALLOCATE( &
         & igrdiu, &
         & igrdju, &
         & zglamu, &
         & zgphiu, &
         & zmasku, &
         & zcoslu, &
         & zsinlu, &
         & igrdiv, &
         & igrdjv, &
         & zglamv, &
         & zgphiv, &
         & zmaskv, &
         & zcoslv, &
         & zsinlv  &
         & )

   END SUBROUTINE obs_rotvel_surf

END MODULE obs_rot_vel
