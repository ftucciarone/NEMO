MODULE obs_oper
   !!======================================================================
   !!                       ***  MODULE obs_oper  ***
   !! Observation diagnostics: Observation operators for various observation
   !!                          types
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   obs_prof_opt :    Compute the model counterpart of profile data
   !!   obs_surf_opt :    Compute the model counterpart of surface data
   !!----------------------------------------------------------------------
   USE obs_inter_sup                                        ! Interpolation support
   USE obs_inter_h2d, ONLY : obs_int_h2d, obs_int_h2d_init  ! Horizontal interpolation to the obs pt
   USE obs_averg_h2d, ONLY : obs_avg_h2d, obs_avg_h2d_init, obs_max_fpsize    ! Horizontal averaging to the obs footprint
   USE obs_inter_z1d, ONLY : obs_int_z1d, obs_int_z1d_spl   ! Vertical interpolation to the obs pt
   USE obs_const    , ONLY : obfillflt                      ! Obs fill value
   USE dom_oce,       ONLY :   glamt, glamf, gphit, gphif   ! lat/lon of ocean grid-points
   USE lib_mpp,       ONLY :   ctl_warn, ctl_stop           ! Warning and stopping routines
   USE sbcdcy,        ONLY :   sbc_dcy, nday_qsr            ! For calculation of where it is night-time
   USE obs_grid,      ONLY :   obs_level_search
   USE obs_group_def, ONLY : cobsname_sla, cobsname_fbd, jpmaxavtypes
#if defined key_si3 || defined key_cice
   USE phycst,        ONLY : rhos, rhoi                     ! For conversion from sea ice freeboard to thickness
#endif
   !
   USE par_kind     , ONLY :   wp   ! Precision variables
   USE in_out_manager               ! I/O manager
   USE lib_fortran    ! to use sign with key_nosignedzero

   IMPLICIT NONE
   PRIVATE

   PUBLIC   obs_prof_opt   !: Compute the model counterpart of profile obs
   PUBLIC   obs_surf_opt   !: Compute the model counterpart of surface obs

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE obs_prof_opt( prodatqc, kt, kpi, kpj, kpk, &
      &                     kit000, kdaystp, kvar,       &
      &                     pvar,                        &
      &                     ldclim, kclim, pclim,        &
      &                     pgdept, pgdepw,              &
      &                     pmask,                       &  
      &                     plam, pphi,                  &
      &                     k1dint, k2dint,              &
      &                     knavtypes, kdailyavtypes )
      !!-----------------------------------------------------------------------
      !!                     ***  ROUTINE obs_pro_opt  ***
      !!
      !! ** Purpose : Compute the model counterpart of profiles
      !!              data by interpolating from the model grid to the 
      !!              observation point.
      !!
      !! ** Method  : Linearly interpolate to each observation point using 
      !!              the model values at the corners of the surrounding grid box.
      !!
      !!    First, a vertical profile of horizontally interpolated model
      !!    now values is computed at the obs (lon, lat) point.
      !!    Several horizontal interpolation schemes are available:
      !!        - distance-weighted (great circle) (k2dint = 0)
      !!        - distance-weighted (small angle)  (k2dint = 1)
      !!        - bilinear (geographical grid)     (k2dint = 2)
      !!        - bilinear (quadrilateral grid)    (k2dint = 3)
      !!        - polynomial (quadrilateral grid)  (k2dint = 4)
      !!
      !!    Next, the vertical profile is interpolated to the
      !!    data depth points. Two vertical interpolation schemes are
      !!    available:
      !!        - linear       (k1dint = 0)
      !!        - Cubic spline (k1dint = 1)
      !!
      !!    For the cubic spline the 2nd derivative of the interpolating 
      !!    polynomial is computed before entering the vertical interpolation 
      !!    routine.
      !!
      !!    If the logical is switched on, the model equivalent is
      !!    a daily mean model temperature field. So, we first compute
      !!    the mean, then interpolate only at the end of the day.
      !!
      !!    Note: in situ temperature observations must be converted
      !!    to potential temperature (the model variable) prior to
      !!    assimilation. 
      !!
      !! ** Action  :
      !!
      !! History :
      !!      ! 97-11 (A. Weaver, S. Ricci, N. Daget)
      !!      ! 06-03 (G. Smith) NEMOVAR migration
      !!      ! 06-10 (A. Weaver) Cleanup
      !!      ! 07-01 (K. Mogensen) Merge of temperature and salinity
      !!      ! 07-03 (K. Mogensen) General handling of profiles
      !!      ! 15-02 (M. Martin) Combined routine for all profile types
      !!      ! 17-02 (M. Martin) Include generalised vertical coordinate changes
      !!-----------------------------------------------------------------------
      USE obs_profiles_def ! Definition of storage space for profile obs.

      IMPLICIT NONE

      TYPE(obs_prof), INTENT(inout) ::   prodatqc        ! Subset of profile data passing QC
      INTEGER       , INTENT(in   ) ::   kt              ! Time step
      INTEGER       , INTENT(in   ) ::   kpi, kpj, kpk   ! Model grid parameters
      INTEGER       , INTENT(in   ) ::   kit000          ! Number of the first time step (kit000-1 = restart time)
      INTEGER       , INTENT(in   ) ::   k1dint          ! Vertical interpolation type (see header)
      INTEGER       , INTENT(in   ) ::   k2dint          ! Horizontal interpolation type (see header)
      INTEGER       , INTENT(in   ) ::   kdaystp         ! Number of time steps per day
      INTEGER       , INTENT(in   ) ::   kvar            ! Index of variable in prodatqc
      INTEGER       , INTENT(in   ) ::   kclim           ! Index of climatology in prodatqc
      LOGICAL       , INTENT(in   ) ::   ldclim          ! Switch to interpolate climatology
      REAL(KIND=wp) , INTENT(in   ), DIMENSION(kpi,kpj,kpk) ::   pvar             ! Model field
      REAL(KIND=wp) , INTENT(in   ), DIMENSION(kpi,kpj,kpk) ::   pclim            ! Climatology field
      REAL(KIND=wp) , INTENT(in   ), DIMENSION(kpi,kpj,kpk) ::   pmask            ! Land-sea mask
      REAL(KIND=wp) , INTENT(in   ), DIMENSION(kpi,kpj)     ::   plam             ! Model longitude
      REAL(KIND=wp) , INTENT(in   ), DIMENSION(kpi,kpj)     ::   pphi             ! Model latitudes
      REAL(KIND=wp) , INTENT(in   ), DIMENSION(kpi,kpj,kpk) ::   pgdept, pgdepw   ! depth of T and W levels 
      INTEGER       , INTENT(in   )                         ::   knavtypes        ! Number of daily average types
      INTEGER, DIMENSION(knavtypes), INTENT(in), OPTIONAL   ::   kdailyavtypes    ! Types for daily averages

      !! * Local declarations
      INTEGER ::   ji
      INTEGER ::   jj
      INTEGER ::   jk
      INTEGER ::   jobs
      INTEGER ::   inrc
      INTEGER ::   ipro
      INTEGER ::   idayend
      INTEGER ::   ista
      INTEGER ::   iend
      INTEGER ::   iobs
      INTEGER ::   iin, ijn, ikn, ik   ! looping indices over interpolation nodes 
      INTEGER ::   inum_obs
      INTEGER, DIMENSION(knavtypes) :: &
         & idailyavtypes
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: &
         & igrdi, &
         & igrdj
      INTEGER, ALLOCATABLE, DIMENSION(:) :: iv_indic

      REAL(KIND=wp) :: zlam
      REAL(KIND=wp) :: zphi
      REAL(KIND=wp) :: zdaystp
      REAL(KIND=wp), DIMENSION(kpk) :: &
         & zobsk,  &
         & zobs2k, &
         & zclm2k
      REAL(KIND=wp), DIMENSION(2,2,1) :: &
         & zweig1, &
         & zweig
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE :: &
         & zmask,  &
         & zclim,  &
         & zint,   &
         & zinm,   &
         & zgdept, & 
         & zgdepw
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zglam,  &
         & zgphi
      REAL(KIND=wp), DIMENSION(1) :: zmsk
      REAL(KIND=wp), DIMENSION(:,:,:), ALLOCATABLE :: interp_corner
      REAL(KIND=wp), DIMENSION(:,:,:), ALLOCATABLE :: interp_corner_clim

      LOGICAL :: ld_dailyav

      !------------------------------------------------------------------------
      ! Local initialization 
      !------------------------------------------------------------------------
      ! Record and data counters
      inrc = kt - kit000 + 2
      ipro = prodatqc%npstp(inrc)

      ! Daily average types
      ld_dailyav = .FALSE.
      IF ( PRESENT(kdailyavtypes) .AND. ( knavtypes > 0 ) ) THEN
         idailyavtypes(:) = kdailyavtypes(:)
         IF ( ANY (idailyavtypes(:) /= -1) ) ld_dailyav = .TRUE.
      ELSE
         idailyavtypes(:) = -1
      ENDIF

      ! Daily means are calculated for values over timesteps:
      !  [1 <= kt <= kdaystp], [kdaystp+1 <= kt <= 2*kdaystp], ...
      idayend = MOD( kt - kit000 + 1, kdaystp )

      IF ( ld_dailyav ) THEN

         ! Initialize daily mean for first timestep of the day
         IF ( idayend == 1 .OR. kt == 0 ) THEN
            prodatqc%vdmean(:,:,:,kvar) = 0.0_wp
         ENDIF

         ! Increment field 1 for computing daily mean
         prodatqc%vdmean(:,:,:,kvar) = prodatqc%vdmean(:,:,:,kvar) + pvar(:,:,:)

         ! Compute the daily mean at the end of day
         zdaystp = 1.0_wp / REAL( kdaystp, KIND=wp )
         IF ( ( idayend == 0 ) .AND. ( kt > 0 ) ) THEN
            prodatqc%vdmean(:,:,:,kvar) = prodatqc%vdmean(:,:,:,kvar) * zdaystp
         ENDIF

      ENDIF

      ! Get the data for interpolation
      ALLOCATE( &
         & igrdi(2,2,ipro),      &
         & igrdj(2,2,ipro),      &
         & zglam(2,2,ipro),      &
         & zgphi(2,2,ipro),      &
         & zmask(2,2,kpk,ipro),  &
         & zint(2,2,kpk,ipro),   &
         & zgdept(2,2,kpk,ipro), & 
         & zgdepw(2,2,kpk,ipro)  & 
         & )

      IF ( ldclim ) THEN
         ALLOCATE( zclim(2,2,kpk,ipro) )
      ENDIF

      DO jobs = prodatqc%nprofup + 1, prodatqc%nprofup + ipro
         iobs = jobs - prodatqc%nprofup
         igrdi(1,1,iobs) = prodatqc%mi(jobs,kvar)-1
         igrdj(1,1,iobs) = prodatqc%mj(jobs,kvar)-1
         igrdi(1,2,iobs) = prodatqc%mi(jobs,kvar)-1
         igrdj(1,2,iobs) = prodatqc%mj(jobs,kvar)
         igrdi(2,1,iobs) = prodatqc%mi(jobs,kvar)
         igrdj(2,1,iobs) = prodatqc%mj(jobs,kvar)-1
         igrdi(2,2,iobs) = prodatqc%mi(jobs,kvar)
         igrdj(2,2,iobs) = prodatqc%mj(jobs,kvar)
      END DO

      ! Initialise depth arrays
      zgdept(:,:,:,:) = 0.0
      zgdepw(:,:,:,:) = 0.0

      CALL obs_int_comm_2d( 2, 2, ipro, kpi, kpj, igrdi, igrdj, plam, zglam )
      CALL obs_int_comm_2d( 2, 2, ipro, kpi, kpj, igrdi, igrdj, pphi, zgphi )
      CALL obs_int_comm_3d( 2, 2, ipro, kpi, kpj, kpk, igrdi, igrdj, pmask, zmask )
      CALL obs_int_comm_3d( 2, 2, ipro, kpi, kpj, kpk, igrdi, igrdj, pvar,   zint )

      CALL obs_int_comm_3d( 2, 2, ipro, kpi, kpj, kpk, igrdi, igrdj, pgdept, zgdept ) 
      CALL obs_int_comm_3d( 2, 2, ipro, kpi, kpj, kpk, igrdi, igrdj, pgdepw, zgdepw ) 

      IF ( ldclim ) THEN
         CALL obs_int_comm_3d( 2, 2, ipro, kpi, kpj, kpk, igrdi, igrdj, pclim, zclim )
      ENDIF

      ! At the end of the day also get interpolated means
      IF ( ld_dailyav .AND. idayend == 0 ) THEN

         ALLOCATE( zinm(2,2,kpk,ipro) )

         CALL obs_int_comm_3d( 2, 2, ipro, kpi, kpj, kpk, igrdi, igrdj, &
            &                  prodatqc%vdmean(:,:,:,kvar), zinm )

      ENDIF

      ! Return if no observations to process 
      ! Has to be done after comm commands to ensure processors 
      ! stay in sync 
      IF ( ipro == 0 ) RETURN 

      DO jobs = prodatqc%nprofup + 1, prodatqc%nprofup + ipro

         iobs = jobs - prodatqc%nprofup

         IF ( kt /= prodatqc%mstp(jobs) ) THEN

            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) ' E R R O R : Observation',              &
                  &            ' time step is not consistent with the', &
                  &            ' model time step'
               WRITE(numout,*) ' ========='
               WRITE(numout,*)
               WRITE(numout,*) ' Record  = ', jobs,                    &
                  &            ' kt      = ', kt,                      &
                  &            ' mstp    = ', prodatqc%mstp(jobs), &
                  &            ' ntyp    = ', prodatqc%ntyp(jobs)
            ENDIF
            CALL ctl_stop( 'obs_pro_opt', 'Inconsistent time' )
         ENDIF

         zlam = prodatqc%rlam(jobs)
         zphi = prodatqc%rphi(jobs)

         ! Horizontal weights 
         ! Masked values are calculated later.  
         IF ( prodatqc%npvend(jobs,kvar) > 0 ) THEN

            CALL obs_int_h2d_init( 1, 1, k2dint, zlam, zphi,     &
               &                   zglam(:,:,iobs), zgphi(:,:,iobs), &
               &                   zmask(:,:,1,iobs), zweig1, zmsk )

         ENDIF

         IF ( prodatqc%npvend(jobs,kvar) > 0 ) THEN

            zobsk(:) = obfillflt

            IF ( ANY (idailyavtypes(:) == prodatqc%ntyp(jobs)) ) THEN

               IF ( idayend == 0 )  THEN
                  ! Daily averaged data

                  ! vertically interpolate all 4 corners 
                  ista = prodatqc%npvsta(jobs,kvar) 
                  iend = prodatqc%npvend(jobs,kvar) 
                  inum_obs = iend - ista + 1 
                  ALLOCATE(interp_corner(2,2,inum_obs),iv_indic(inum_obs))
                  IF ( ldclim ) THEN
                     ALLOCATE( interp_corner_clim(2,2,inum_obs) )
                  ENDIF

                  DO iin=1,2 
                     DO ijn=1,2 

                        IF ( k1dint == 1 ) THEN 
                           CALL obs_int_z1d_spl( kpk, & 
                              &     zinm(iin,ijn,:,iobs), & 
                              &     zobs2k, zgdept(iin,ijn,:,iobs), & 
                              &     zmask(iin,ijn,:,iobs))

                           IF ( ldclim ) THEN
                              CALL obs_int_z1d_spl( kpk, &
                                 &     zclim(iin,ijn,:,iobs), &
                                 &     zclm2k, zgdept(iin,ijn,:,iobs), &
                                 &     zmask(iin,ijn,:,iobs))
                           ENDIF
                        ENDIF
       
                        CALL obs_level_search(kpk, & 
                           &    zgdept(iin,ijn,:,iobs), & 
                           &    inum_obs, prodatqc%var(kvar)%vdep(ista:iend), & 
                           &    iv_indic) 

                        CALL obs_int_z1d(kpk, iv_indic, k1dint, inum_obs, & 
                           &    prodatqc%var(kvar)%vdep(ista:iend), & 
                           &    zinm(iin,ijn,:,iobs), & 
                           &    zobs2k, interp_corner(iin,ijn,:), & 
                           &    zgdept(iin,ijn,:,iobs), & 
                           &    zmask(iin,ijn,:,iobs))

                        IF ( ldclim ) THEN
                           CALL obs_int_z1d(kpk, iv_indic, k1dint, inum_obs, &
                              &    prodatqc%var(kvar)%vdep(ista:iend), &
                              &    zclim(iin,ijn,:,iobs), &
                              &    zclm2k, interp_corner_clim(iin,ijn,:), &
                              &    zgdept(iin,ijn,:,iobs), &
                              &    zmask(iin,ijn,:,iobs))
                        ENDIF

                     ENDDO 
                  ENDDO 

               ENDIF !idayend

            ELSE   

               ! Point data 
     
               ! vertically interpolate all 4 corners 
               ista = prodatqc%npvsta(jobs,kvar) 
               iend = prodatqc%npvend(jobs,kvar) 
               inum_obs = iend - ista + 1 
               ALLOCATE(interp_corner(2,2,inum_obs), iv_indic(inum_obs))
               IF ( ldclim ) THEN
                  ALLOCATE( interp_corner_clim(2,2,inum_obs) )
               ENDIF
               DO iin=1,2
                  DO ijn=1,2 
                    
                     IF ( k1dint == 1 ) THEN 
                        CALL obs_int_z1d_spl( kpk, & 
                           &    zint(iin,ijn,:,iobs),& 
                           &    zobs2k, zgdept(iin,ijn,:,iobs), & 
                           &    zmask(iin,ijn,:,iobs))

                        IF ( ldclim ) THEN
                           CALL obs_int_z1d_spl( kpk, &
                              &    zclim(iin,ijn,:,iobs),&
                              &    zclm2k, zgdept(iin,ijn,:,iobs), &
                              &    zmask(iin,ijn,:,iobs))
                        ENDIF
                     ENDIF
       
                     CALL obs_level_search(kpk, & 
                         &        zgdept(iin,ijn,:,iobs),& 
                         &        inum_obs, prodatqc%var(kvar)%vdep(ista:iend), & 
                         &        iv_indic) 

                     CALL obs_int_z1d(kpk, iv_indic, k1dint, inum_obs,     & 
                         &          prodatqc%var(kvar)%vdep(ista:iend),     & 
                         &          zint(iin,ijn,:,iobs),            & 
                         &          zobs2k,interp_corner(iin,ijn,:), & 
                         &          zgdept(iin,ijn,:,iobs),         & 
                         &          zmask(iin,ijn,:,iobs) )

                     IF ( ldclim ) THEN
                        CALL obs_int_z1d(kpk, iv_indic, k1dint, inum_obs,     &
                            &          prodatqc%var(kvar)%vdep(ista:iend),     &
                            &          zclim(iin,ijn,:,iobs),            &
                            &          zclm2k,interp_corner_clim(iin,ijn,:), &
                            &          zgdept(iin,ijn,:,iobs),         &
                            &          zmask(iin,ijn,:,iobs) )
                     ENDIF

                  ENDDO 
               ENDDO 
             
            ENDIF 

            !------------------------------------------------------------- 
            ! Compute the horizontal interpolation for every profile level 
            !------------------------------------------------------------- 
             
            DO ikn=1,inum_obs 
               iend=ista+ikn-1
                  
               zweig(:,:,1) = 0._wp 
   
               ! This code forces the horizontal weights to be  
               ! zero IF the observation is below the bottom of the  
               ! corners of the interpolation nodes, Or if it is in  
               ! the mask. This is important for observations near  
               ! steep bathymetry 
               DO iin=1,2 
                  DO ijn=1,2 
     
                     depth_loop: DO ik=kpk,2,-1 
                        IF(zmask(iin,ijn,ik-1,iobs ) > 0.9 )THEN   
                            
                           zweig(iin,ijn,1) = &  
                              & zweig1(iin,ijn,1) * & 
                              & MAX( SIGN(1._wp,(zgdepw(iin,ijn,ik,iobs) ) & 
                              &  - prodatqc%var(kvar)%vdep(iend)),0._wp) 
                            
                           EXIT depth_loop 

                        ENDIF 

                     ENDDO depth_loop
     
                  ENDDO 
               ENDDO 
   
               CALL obs_int_h2d( 1, 1, zweig, interp_corner(:,:,ikn), & 
                  &              prodatqc%var(kvar)%vmod(iend:iend) ) 

               IF ( ldclim ) THEN
                  CALL obs_int_h2d( 1, 1, zweig, interp_corner_clim(:,:,ikn), &
                     &              prodatqc%var(kvar)%vadd(iend:iend,kclim) )
               ENDIF

               ! Set QC flag for any observations found below the bottom
               ! needed as the check here is more strict than that in obs_prep
               IF (sum(zweig) == 0.0_wp) prodatqc%var(kvar)%nvqc(iend:iend)=4
 
            ENDDO 
 
            DEALLOCATE(interp_corner,iv_indic)
            IF ( ldclim ) THEN
               DEALLOCATE( interp_corner_clim )
            ENDIF

         ENDIF

      ENDDO

      ! Deallocate the data for interpolation
      DEALLOCATE(  &
         & igrdi,  &
         & igrdj,  &
         & zglam,  &
         & zgphi,  &
         & zmask,  &
         & zint,   &
         & zgdept, &
         & zgdepw  &
         & )

      IF ( ldclim ) THEN
         DEALLOCATE( zclim )
      ENDIF

      ! At the end of the day also get interpolated means
      IF ( ld_dailyav .AND. idayend == 0 ) THEN
         DEALLOCATE( zinm )
      ENDIF

      IF ( kvar == prodatqc%nvar ) THEN
         prodatqc%nprofup = prodatqc%nprofup + ipro 
      ENDIF

   END SUBROUTINE obs_prof_opt

   SUBROUTINE obs_surf_opt( surfdataqc, kt, kpi, kpj,                     &
      &                     kit000, kdaystp, cdgroupname, kvar, psurf,    &
      &                     ldclim, kclim, pclim, psurfmask,              &
      &                     k2dint, ldnightav, plamscl, pphiscl,          &
      &                     lindegrees, ldtime_mean, kmeanstp,            &
      &                     kssh, kmdt, kfbd, ksnow, krhosw,              &
      &                     kradar_snow_penetr )

      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_surf_opt  ***
      !!
      !! ** Purpose : Compute the model counterpart of surface
      !!              data by interpolating from the model grid to the 
      !!              observation point.
      !!
      !! ** Method  : Linearly interpolate to each observation point using 
      !!              the model values at the corners of the surrounding grid box.
      !!
      !!    The new model value is first computed at the obs (lon, lat) point.
      !!
      !!    Several horizontal interpolation schemes are available:
      !!        - distance-weighted (great circle) (k2dint = 0)
      !!        - distance-weighted (small angle)  (k2dint = 1)
      !!        - bilinear (geographical grid)     (k2dint = 2)
      !!        - bilinear (quadrilateral grid)    (k2dint = 3)
      !!        - polynomial (quadrilateral grid)  (k2dint = 4)
      !!
      !!    Two horizontal averaging schemes are also available:
      !!        - weighted radial footprint        (k2dint = 5)
      !!        - weighted rectangular footprint   (k2dint = 6)
      !!
      !!
      !! ** Action  :
      !!
      !! History :
      !!      ! 07-03 (A. Weaver)
      !!      ! 15-02 (M. Martin) Combined routine for surface types
      !!      ! 17-03 (M. Martin) Added horizontal averaging options
      !!-----------------------------------------------------------------------
      USE obs_surf_def  ! Definition of storage space for surface observations

      IMPLICIT NONE

      TYPE(obs_surf), INTENT(INOUT) :: &
         & surfdataqc                  ! Subset of surface data passing QC
      INTEGER, INTENT(IN) :: kt        ! Time step
      INTEGER, INTENT(IN) :: kpi       ! Model grid parameters
      INTEGER, INTENT(IN) :: kpj
      INTEGER, INTENT(IN) :: kit000    ! Number of the first time step 
                                       !   (kit000-1 = restart time)
      INTEGER, INTENT(IN) :: kdaystp   ! Number of time steps per day
      CHARACTER(LEN=25), INTENT(IN) :: &
         & cdgroupname                 ! Name of observation group
      INTEGER, INTENT(IN) :: kvar      ! Index of variable in surfdataqc
      INTEGER, INTENT(IN) :: kclim     ! Index of climatology in surfdataqc
      INTEGER, INTENT(IN) :: k2dint    ! Horizontal interpolation type (see header)
      LOGICAL, INTENT(IN) :: ldclim    ! Switch to interpolate climatology
      REAL(wp), INTENT(IN), DIMENSION(kpi,kpj) :: &
         & psurf,  &                   ! Model surface field
         & pclim,  &                   ! Climatology surface field
         & psurfmask                   ! Land-sea mask
      LOGICAL, INTENT(IN) :: ldnightav ! Logical for averaging night-time data
      REAL(KIND=wp), INTENT(IN) :: &
         & plamscl, &                  ! Diameter in metres of obs footprint in E/W, N/S directions
         & pphiscl                     ! This is the full width (rather than half-width)
      LOGICAL, INTENT(IN) :: &
         & lindegrees                  ! T=> plamscl and pphiscl are specified in degrees, F=> in metres
      LOGICAL, INTENT(IN) :: &
         & ldtime_mean                 ! Observations/background represent a time mean
      INTEGER, INTENT(IN) :: kmeanstp  ! Number of time steps for meaning if ldtime_mean
      INTEGER, OPTIONAL, INTENT(IN)  :: &
         & kssh                        ! Index of additional variable representing SSH
      INTEGER, OPTIONAL, INTENT(IN)  :: &
         & kmdt                        ! Index of extra variable representing MDT
      INTEGER, OPTIONAL, INTENT(IN)  :: &
         & kfbd                        ! Index of additional variable representing ice freeboard
      INTEGER, OPTIONAL, INTENT(IN)  :: &
         & ksnow                       ! Index of extra variable representing ice snow thickness
      INTEGER, OPTIONAL, INTENT(IN)  :: &
         & krhosw                      ! Index of extra variable representing seawater density
      REAL(wp), OPTIONAL, INTENT(IN) :: &
         & kradar_snow_penetr          ! Snow depth penetration factor for radar ice freeboard conversion

      !! * Local declarations
      INTEGER :: ji
      INTEGER :: jj
      INTEGER :: jobs
      INTEGER :: inrc
      INTEGER :: isurf
      INTEGER :: iobs
      INTEGER :: imaxifp, imaxjfp
      INTEGER :: imodi, imodj
      INTEGER :: idayend
      INTEGER :: imeanend
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: &
         & igrdi,   &
         & igrdj,   &
         & igrdip1, &
         & igrdjp1
      INTEGER, DIMENSION(:,:), SAVE, ALLOCATABLE :: &
         & icount_night,      &
         & imask_night
      REAL(wp) :: zlam
      REAL(wp) :: zphi
      REAL(wp), DIMENSION(1) :: zext, zobsmask, zclm
      REAL(wp) :: zdaystp
      REAL(wp) :: zmeanstp
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: &
         & zweig,  &
         & zmask,  &
         & zsurf,  &
         & zsurfm, &
         & zsurftmp, &
         & zclim,  &
         & zglam,  &
         & zgphi,  &
         & zglamf, &
         & zgphif

      REAL(wp), DIMENSION(:,:), SAVE, ALLOCATABLE :: &
         & zintmp,  &
         & zouttmp, &
         & zmeanday    ! to compute model sst in region of 24h daylight (pole)

      !------------------------------------------------------------------------
      ! Local initialization 
      !------------------------------------------------------------------------
      ! Record and data counters
      inrc = kt - kit000 + 2
      isurf = surfdataqc%nsstp(inrc)

      ! Work out the maximum footprint size for the 
      ! interpolation/averaging in model grid-points - has to be even.

      CALL obs_max_fpsize( k2dint, plamscl, pphiscl, lindegrees, psurfmask, imaxifp, imaxjfp )

      IF ( ldtime_mean .AND. ldnightav ) THEN
         CALL ctl_stop( 'obs_surf_opt: Can have ldtime_mean or ldnightav but not both, in group', &
            &           TRIM(cdgroupname) )
      ENDIF

      ! Initialize time mean for first timestep
      imeanend = MOD( kt - kit000 + 1, kmeanstp )

      IF ( ldtime_mean ) THEN
         IF (lwp) WRITE(numout,*) 'Obs time mean ', kt, kit000, kmeanstp, imeanend

         ! Added kt == 0 test to catch restart case
         IF ( ( imeanend == 1 ) .OR. ( kt == 0 ) ) THEN
            IF (lwp) WRITE(numout,*) 'Reset surfdataqc%vdmean on time-step: ', kt
            DO jj = 1, jpj
               DO ji = 1, jpi
                  surfdataqc%vdmean(ji,jj,kvar) = 0.0_wp
               END DO
            END DO
         ENDIF

         ! On each time-step, increment the field for computing time mean
         IF (lwp) WRITE(numout,*)'Accumulating surfdataqc%vdmean on time-step: ', kt
         DO jj = 1, jpj
            DO ji = 1, jpi
               surfdataqc%vdmean(ji,jj,kvar) = surfdataqc%vdmean(ji,jj,kvar) &
                  &                            + psurf(ji,jj)
            END DO
         END DO

         ! Compute the time mean at the end of time period
         IF ( imeanend == 0 ) THEN
            zmeanstp = 1.0_wp / REAL( kmeanstp, KIND=wp )
            IF (lwp) WRITE(numout,*) 'Calculating surfdataqc%vdmean time mean on time-step: ', &
               &                     kt, ' with weight: ', zmeanstp
            DO jj = 1, jpj
               DO ji = 1, jpi
                  surfdataqc%vdmean(ji,jj,kvar) = surfdataqc%vdmean(ji,jj,kvar) &
                     &                            * zmeanstp
               END DO
            END DO
         ENDIF
      ENDIF

      ! Night-time means are calculated for night-time values over timesteps:
      !  [1 <= kt <= kdaystp], [kdaystp+1 <= kt <= 2*kdaystp], .....
      idayend = MOD( kt - kit000 + 1, kdaystp )

      IF ( ldnightav ) THEN

         ! Initialize array for night mean
         IF ( kt == 0 ) THEN
            ALLOCATE ( icount_night(kpi,kpj) )
            ALLOCATE ( imask_night(kpi,kpj) )
            ALLOCATE ( zintmp(kpi,kpj) )
            ALLOCATE ( zouttmp(kpi,kpj) )
            ALLOCATE ( zmeanday(kpi,kpj) )
            nday_qsr = -1   ! initialisation flag for nbc_dcy
         ENDIF

         ! Initialize night-time mean for first timestep of the day
         IF ( idayend == 1 .OR. kt == 0 ) THEN
            DO_2D( 1, 1, 1, 1 )
               surfdataqc%vdmean(ji,jj,kvar) = 0.0_wp
               zmeanday(ji,jj) = 0.0
               icount_night(ji,jj) = 0
            END_2D
         ENDIF

         zintmp(:,:) = 0.0
         zouttmp(:,:) = sbc_dcy( zintmp(:,:), .TRUE. )
         imask_night(:,:) = INT( zouttmp(:,:) )

         DO_2D( 1, 1, 1, 1 )
            ! Increment the temperature field for computing night mean and counter
            surfdataqc%vdmean(ji,jj,kvar) = surfdataqc%vdmean(ji,jj,kvar)  &
                   &                        + psurf(ji,jj) * REAL( imask_night(ji,jj), KIND=wp )
            zmeanday(ji,jj)          = zmeanday(ji,jj) + psurf(ji,jj)
            icount_night(ji,jj)      = icount_night(ji,jj) + imask_night(ji,jj)
         END_2D

         ! Compute the night-time mean at the end of the day
         zdaystp = 1.0 / REAL( kdaystp )
         IF ( idayend == 0 ) THEN
            IF (lwp) WRITE(numout,*) 'Calculating surfdataqc%vdmean on time-step: ',kt
            DO_2D( 1, 1, 1, 1 )
               ! Test if "no night" point
               IF ( icount_night(ji,jj) > 0 ) THEN
                  surfdataqc%vdmean(ji,jj,kvar) = surfdataqc%vdmean(ji,jj,kvar) &
                    &                             / REAL( icount_night(ji,jj), KIND=wp )
               ELSE
                  !At locations where there is no night (e.g. poles),
                  ! calculate daily mean instead of night-time mean.
                  surfdataqc%vdmean(ji,jj,kvar) = zmeanday(ji,jj) * zdaystp
               ENDIF
            END_2D
         ENDIF

      ENDIF

      ! Get the data for interpolation

      ALLOCATE( &
         & zweig(imaxifp,imaxjfp,1),      &
         & igrdi(imaxifp,imaxjfp,isurf), &
         & igrdj(imaxifp,imaxjfp,isurf), &
         & zglam(imaxifp,imaxjfp,isurf), &
         & zgphi(imaxifp,imaxjfp,isurf), &
         & zmask(imaxifp,imaxjfp,isurf), &
         & zsurf(imaxifp,imaxjfp,isurf), &
         & zsurftmp(imaxifp,imaxjfp,isurf) &
         & )

      IF ( k2dint > 4 ) THEN
         ALLOCATE( &
            & zglamf(imaxifp+1,imaxjfp+1,isurf),  &
            & zgphif(imaxifp+1,imaxjfp+1,isurf),  &
            & igrdip1(imaxifp+1,imaxjfp+1,isurf), &
            & igrdjp1(imaxifp+1,imaxjfp+1,isurf)  &
            & )
      ENDIF

      IF ( ldclim ) THEN
         ALLOCATE( zclim(imaxifp,imaxjfp,isurf) )
      ENDIF

      DO jobs = surfdataqc%nsurfup + 1, surfdataqc%nsurfup + isurf
         iobs = jobs - surfdataqc%nsurfup
         DO ji = 0, imaxifp
            imodi = surfdataqc%mi(jobs,kvar) - int(imaxifp/2) + ji - 1
            !
            !Deal with wrap around in longitude
            IF ( imodi < 1      ) imodi = imodi + jpiglo
            IF ( imodi > jpiglo ) imodi = imodi - jpiglo
            !
            DO jj = 0, imaxjfp
               imodj = surfdataqc%mj(jobs,kvar) - int(imaxjfp/2) + jj - 1
               !If model values are out of the domain to the north/south then
               !set them to be the edge of the domain
               IF ( imodj < 1      ) imodj = 1
               IF ( imodj > jpjglo ) imodj = jpjglo
               !
               IF ( k2dint > 4 ) THEN
                  igrdip1(ji+1,jj+1,iobs) = imodi
                  igrdjp1(ji+1,jj+1,iobs) = imodj
               ENDIF
               !
               IF ( ji >= 1 .AND. jj >= 1 ) THEN
                  igrdi(ji,jj,iobs) = imodi
                  igrdj(ji,jj,iobs) = imodj
               ENDIF
               !
            END DO
         END DO
      END DO

      CALL obs_int_comm_2d( imaxifp, imaxjfp, isurf, kpi, kpj, &
         &                  igrdi, igrdj, glamt, zglam )
      CALL obs_int_comm_2d( imaxifp, imaxjfp, isurf, kpi, kpj, &
         &                  igrdi, igrdj, gphit, zgphi )
      CALL obs_int_comm_2d( imaxifp, imaxjfp, isurf, kpi, kpj, &
         &                  igrdi, igrdj, psurfmask, zmask )

      ! At the end of the averaging period get interpolated means
      IF ( ldtime_mean ) THEN
         IF ( imeanend == 0 ) THEN
            ALLOCATE( zsurfm(imaxifp,imaxjfp,isurf) )
            IF (lwp) WRITE(numout,*)' Interpolating the time mean values on time step: ', kt
            CALL obs_int_comm_2d( imaxifp, imaxjfp, isurf, kpi, kpj, &
               &                  igrdi, igrdj, surfdataqc%vdmean(:,:,kvar), zsurfm )
         ENDIF
      ELSE
         CALL obs_int_comm_2d( imaxifp, imaxjfp, isurf, kpi, kpj, &
            &                  igrdi, igrdj, psurf, zsurf )
      ENDIF

      IF ( k2dint > 4 ) THEN
         CALL obs_int_comm_2d( imaxifp+1, imaxjfp+1, isurf, kpi, kpj, &
            &                  igrdip1, igrdjp1, glamf, zglamf )
         CALL obs_int_comm_2d( imaxifp+1, imaxjfp+1, isurf, kpi, kpj, &
            &                  igrdip1, igrdjp1, gphif, zgphif )
      ENDIF

      IF ( ldclim ) THEN
         CALL obs_int_comm_2d( imaxifp, imaxjfp, isurf, kpi, kpj, &
            &                  igrdi, igrdj, pclim, zclim )
      ENDIF

      ! At the end of the day get interpolated means
      IF ( ldnightav ) THEN
         IF ( idayend == 0 ) THEN

            ALLOCATE( &
               & zsurfm(imaxifp,imaxjfp,isurf)  &
               & )

            CALL obs_int_comm_2d( imaxifp,imaxjfp, isurf, kpi, kpj, igrdi, igrdj, &
            &               surfdataqc%vdmean(:,:,kvar), zsurfm )

         ENDIF
      ENDIF

      ! Loop over observations
      DO jobs = surfdataqc%nsurfup + 1, surfdataqc%nsurfup + isurf

         iobs = jobs - surfdataqc%nsurfup

         IF ( kt /= surfdataqc%mstp(jobs) ) THEN

            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) ' E R R O R : Observation',              &
                  &            ' time step is not consistent with the', &
                  &            ' model time step'
               WRITE(numout,*) ' ========='
               WRITE(numout,*)
               WRITE(numout,*) ' Record  = ', jobs,                &
                  &            ' kt      = ', kt,                  &
                  &            ' mstp    = ', surfdataqc%mstp(jobs), &
                  &            ' ntyp    = ', surfdataqc%ntyp(jobs)
            ENDIF
            CALL ctl_stop( 'obs_surf_opt', 'Inconsistent time in group:', TRIM(cdgroupname) )

         ENDIF

         zlam = surfdataqc%rlam(jobs)
         zphi = surfdataqc%rphi(jobs)

         IF ( ( ldnightav .AND. idayend == 0 ) .OR. (ldtime_mean .AND. imeanend == 0) ) THEN
            ! Night-time or N=kmeanstp timestep averaged data
            zsurftmp(:,:,iobs) = zsurfm(:,:,iobs)
         ELSE
            zsurftmp(:,:,iobs) = zsurf(:,:,iobs)
         ENDIF

         IF ( ( .NOT. ldtime_mean ) .OR. ( ldtime_mean .AND. imeanend == 0) ) THEN

            IF ( k2dint <= 4 ) THEN

               ! Get weights to interpolate the model value to the observation point
               CALL obs_int_h2d_init( 1, 1, k2dint, zlam, zphi,         &
                  &                   zglam(:,:,iobs), zgphi(:,:,iobs), &
                  &                   zmask(:,:,iobs), zweig, zobsmask )

               ! Interpolate the model value to the observation point
               CALL obs_int_h2d( 1, 1, zweig, zsurftmp(:,:,iobs), zext )

               IF ( ldclim ) THEN
                  CALL obs_int_h2d( 1, 1, zweig, zclim(:,:,iobs), zclm )
               ENDIF

            ELSE

               ! Get weights to average the model field to the observation footprint
               CALL obs_avg_h2d_init( 1, 1, imaxifp, imaxjfp, k2dint, zlam,  zphi, &
                  &                   zglam(:,:,iobs), zgphi(:,:,iobs), &
                  &                   zglamf(:,:,iobs), zgphif(:,:,iobs), &
                  &                   zmask(:,:,iobs), plamscl, pphiscl, &
                  &                   lindegrees, zweig )

               ! Average the model field to the observation footprint
               CALL obs_avg_h2d( 1, 1, imaxifp, imaxjfp, &
                  &              zweig, zsurftmp(:,:,iobs),  zext )

               IF ( ldclim ) THEN
                  CALL obs_avg_h2d( 1, 1, imaxifp, imaxjfp, &
                     &              zweig, zclim(:,:,iobs),  zclm )
               ENDIF

            ENDIF

            IF ( TRIM(surfdataqc%cvars(kvar)) == cobsname_sla .AND. PRESENT(kssh) .AND. PRESENT(kmdt) ) THEN
               ! ... Remove the MDT from the SSH at the observation point to get the SLA
               surfdataqc%radd(jobs,kssh,kvar) = zext(1)
               surfdataqc%rmod(jobs,kvar) = surfdataqc%radd(jobs,kssh,kvar) - surfdataqc%rext(jobs,kmdt)
#if defined key_si3 || defined key_cice
            ELSE IF ( TRIM(surfdataqc%cvars(kvar)) == cobsname_fbd ) THEN
               ! Checking all required variables are present for freeboard conversion into thickness
               IF ( ( .NOT. PRESENT(kfbd) ) .OR. ( .NOT. PRESENT(ksnow) ) .OR. ( .NOT. PRESENT(krhosw) ) ) THEN
                  IF (lwp) THEN
                     WRITE(numout,*)
                     WRITE(numout,*) ' E R R O R : Required variables for',  &
                  &            ' freeboard conversion into thickness',       &
                  &            ' are not present in group',                  &
                  &            TRIM(cdgroupname)
                     WRITE(numout,*) ' ========='
                     WRITE(numout,*)
                  ENDIF
                  CALL ctl_stop( 'obs_surf_opt: Required variables for freeboard conversion are missing' )
               ENDIF
               surfdataqc%rmod(jobs,kvar) = zext(1)
               ! Convert radar freeboard to true freeboard
               ! (add 1/4 snow depth; 1/4 based on ratio of speed of light in vacuum
               !  compared to snow (3.0e8 vs 2.4e8 m/s))
               surfdataqc%radd(jobs,kfbd,kvar) = surfdataqc%robs(jobs,kvar)
               surfdataqc%robs(jobs,kvar) = surfdataqc%radd(jobs,kfbd,kvar) -                            &
                  &                         (1.0_wp - kradar_snow_penetr)*surfdataqc%rext(jobs,ksnow) +  &
                  &                         kradar_snow_penetr * 0.25_wp * surfdataqc%rext(jobs,ksnow)
               ! If the corrected freeboard observation is outside -0.3 to 3.0 m (CPOM) then set the QC flag to bad
               IF ((surfdataqc%robs(jobs,kvar) < -0.3_wp) .OR. (surfdataqc%robs(jobs,kvar) > 3.0_wp)) THEN
                  surfdataqc%nqc(jobs) = 4
               ENDIF
               ! Convert corrected freeboard to ice thickness following Tilling et al. (2016)
               surfdataqc%robs(jobs,kvar) = (surfdataqc%robs(jobs,kvar)*surfdataqc%rext(jobs,krhosw) +  &
                  &                          surfdataqc%rext(jobs,ksnow)*rhos)/(surfdataqc%rext(jobs,krhosw) - rhoi)
#endif
            ELSE
               surfdataqc%rmod(jobs,kvar) = zext(1)
            ENDIF

            IF ( ldclim ) THEN
               surfdataqc%radd(jobs,kclim,kvar) = zclm(1)
            ENDIF

            IF ( zext(1) == obfillflt ) THEN
               ! If the observation value is a fill value, set QC flag to bad
               surfdataqc%nqc(jobs) = 4
            ENDIF

         ENDIF

      END DO

      ! Deallocate the data for interpolation
      DEALLOCATE( &
         & zweig, &
         & igrdi, &
         & igrdj, &
         & zglam, &
         & zgphi, &
         & zmask, &
         & zsurf, &
         & zsurftmp &
         & )

      IF ( k2dint > 4 ) THEN
         DEALLOCATE( &
            & zglamf, &
            & zgphif, &
            & igrdip1,&
            & igrdjp1 &
            & )
      ENDIF

      IF ( ldclim ) THEN
         DEALLOCATE( zclim )
      ENDIF

      ! At the end of the day also deallocate time mean array
      IF ( ( idayend == 0 .AND. ldnightav ) .OR. ( imeanend == 0 .AND. ldtime_mean ) ) THEN
         DEALLOCATE( &
            & zsurfm  &
            & )
      ENDIF
      !
      IF ( kvar == surfdataqc%nvar ) THEN
         surfdataqc%nsurfup = surfdataqc%nsurfup + isurf
      ENDIF
      !
   END SUBROUTINE obs_surf_opt

   !!======================================================================
END MODULE obs_oper
