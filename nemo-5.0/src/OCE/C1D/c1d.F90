MODULE c1d
   !!======================================================================
   !!                     ***  MODULE  c1d  ***
   !! Ocean domain  :  1D configuration
   !!=====================================================================
   !! History :  2.0  !  2004-09 (C. Ethe)     Original code
   !!            3.0  !  2008-04 (G. Madec)    adaptation to SBC
   !!            3.5  !  2013-10 (D. Calvert)  add namelist
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   c1d_init      : read in the C1D namelist
   !!----------------------------------------------------------------------
   USE par_kind       ! kind parameters
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   !
   USE oce            ! ocean dynamics and tracers
   USE phycst         ! physical constants
   USE dom_oce        ! ocean space and time domain
   USE tradmp         ! ocean: internal damping
   USE zdf_oce        ! ocean: vertical physics
   USE phycst         ! physical constants
   USE zdfmxl         ! vertical physics: mixed layer depth
   !
   USE fldread        ! read input fields
   USE timing         ! Timing
   USE prtctl         ! Print control
   USE iom            ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   c1d_init       ! called by nemogcm.F90
   PUBLIC   dta_uvd_init   ! called by nemogcm.F90
   PUBLIC   dta_uvd        ! called by istate.F90 and dyndmp.90
   PUBLIC   dyn_dmp_init   ! called by nemogcm.F90
   PUBLIC   dyn_dmp        ! called by step_c1d.F90

   REAL(wp), PUBLIC ::   rn_lat1d     !: Column latitude
   REAL(wp), PUBLIC ::   rn_lon1d     !: Column longitude

   LOGICAL , PUBLIC ::   ln_dyndmp    !: Flag for Newtonian damping

   LOGICAL , PUBLIC ::   ln_uvd_init   = .FALSE.   !: Flag to initialise with U & V current data
   LOGICAL , PUBLIC ::   ln_uvd_dyndmp = .FALSE.   !  Flag for Newtonian damping toward U & V current data

   CHARACTER(len=100)        ::   cn_dir              ! Root directory for location of files to be used
   TYPE(FLD_N)               ::   sn_ucur, sn_vcur    ! U & V data namelist information
   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_uvd   ! structure for input U & V current (file information and data)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  utrdmp    !: damping U current trend (m/s2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  vtrdmp    !: damping V current trend (m/s2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  resto_uv  !: restoring coeff. on U & V current

   !! * Substitutions
#  include "read_nml_substitute.h90"
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0 , NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE c1d_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE c1d_init  ***
      !! 
      !! ** Purpose :   Initialization of C1D options
      !!
      !! ** Method  :   Read namelist namc1d 
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !!
      NAMELIST/namc1d/ rn_lat1d, rn_lon1d, ln_dyndmp,   &
         &             ln_uvd_init, ln_uvd_dyndmp, cn_dir, sn_ucur, sn_vcur
      !!----------------------------------------------------------------------
      !
      READ_NML_REF(numnam,namc1d)
      READ_NML_CFG(numnam,namc1d)
      IF(lwm) WRITE ( numond, namc1d )
      !
      IF(lwp) THEN                    ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'c1d_init : Initialize 1D model configuration options'
         WRITE(numout,*) '~~~~~~~~'
         WRITE(numout,*) '   Namelist namc1d : set options for the C1D model'
         WRITE(numout,*) '      column latitude                 rn_lat1d     = ', rn_lat1d
         WRITE(numout,*) '      column longitude                rn_lon1d     = ', rn_lon1d
      ENDIF
      !
   END SUBROUTINE c1d_init

 
   SUBROUTINE dta_uvd_init( ld_dyndmp )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE dta_uvd_init  ***
      !!                    
      !! ** Purpose :   initialization of U & V current input data 
      !! 
      !! ** Method  : - read namc1d_uvd namelist
      !!              - allocate U & V current data structure
      !!              - fld_fill data structure with namelist information
      !!----------------------------------------------------------------------
      LOGICAL, INTENT(in), OPTIONAL ::   ld_dyndmp   ! force the initialization when dyndmp is used
      !
      INTEGER ::   ios, ierr0, ierr1, ierr2, ierr3     ! local integers
      TYPE(FLD_N), DIMENSION(2) ::   suv_i             ! Combined U & V namelist information
      !!----------------------------------------------------------------------
      !
      ierr0 = 0   ;   ierr1 = 0   ;   ierr2 = 0  ;   ierr3 = 0
      !
      !                             ! force the initialization when dyndmp is used
      IF( PRESENT( ld_dyndmp ) )   ln_uvd_dyndmp = .TRUE.
      
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dta_uvd_init : U & V current data '
         WRITE(numout,*) '~~~~~~~~~~~~ '
         WRITE(numout,*) '   Namelist namc1d : Set flags'
         WRITE(numout,*) '      Initialization of ocean U & V current with input data   ln_uvd_init   = ', ln_uvd_init
         WRITE(numout,*) '      Damping of ocean U & V current toward input data        ln_uvd_dyndmp = ', ln_uvd_dyndmp
         WRITE(numout,*)
         IF( .NOT. ln_uvd_init .AND. .NOT. ln_uvd_dyndmp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) '   U & V current data not used'
         ENDIF
      ENDIF
      !                             ! no initialization when restarting
      IF( ln_rstart .AND. ln_uvd_init ) THEN
         CALL ctl_warn( 'c1d_uvd_init: ocean restart and U & V current data initialization, ',   &
            &           'we keep the restart U & V current values and set ln_uvd_init to FALSE' )
         ln_uvd_init = .FALSE.
      ENDIF

      !
      IF( ln_uvd_init .OR. ln_uvd_dyndmp ) THEN
         !                          !==   allocate the data arrays   ==!
         ALLOCATE( sf_uvd(2), STAT=ierr0 )
         IF( ierr0 > 0 ) THEN
            CALL ctl_stop( 'c1d_uvd_init: unable to allocate sf_uvd structure' )             ;   RETURN
         ENDIF
         !
                                 ALLOCATE( sf_uvd(1)%fnow(jpi,jpj,jpk)   , STAT=ierr0 )
         IF( sn_ucur%ln_tint )   ALLOCATE( sf_uvd(1)%fdta(jpi,jpj,jpk,2) , STAT=ierr1 )
                                 ALLOCATE( sf_uvd(2)%fnow(jpi,jpj,jpk)   , STAT=ierr2 )
         IF( sn_vcur%ln_tint )   ALLOCATE( sf_uvd(2)%fdta(jpi,jpj,jpk,2) , STAT=ierr3 )
         !
         IF( ierr0 + ierr1 + ierr2 + ierr3 > 0 ) THEN
            CALL ctl_stop( 'dta_uvd_init : unable to allocate U & V current data arrays' )   ;   RETURN
         ENDIF
         !                          !==   fill sf_uvd with sn_ucur, sn_vcur and control print   ==!
         suv_i(1) = sn_ucur   ;   suv_i(2) = sn_vcur
         CALL fld_fill( sf_uvd, suv_i, cn_dir, 'c1d_dta_uvd', 'U & V current data', 'namc1d' )
         !
      ENDIF
      !
   END SUBROUTINE dta_uvd_init


   SUBROUTINE dta_uvd( kt, Kmm, pud, pvd )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE dta_uvd  ***
      !!                    
      !! ** Purpose :   provides U & V current data at time step kt
      !! 
      !! ** Method  : - call fldread routine
      !!              - ORCA_R2: make some hand made alterations to the data (EMPTY)
      !!              - s- or mixed s-zps coordinate: vertical interpolation onto model mesh
      !!              - zps coordinate: vertical interpolation onto last partial level
      !!              - ln_uvd_dyndmp=False: deallocate the U & V current data structure,
      !!                                     as the data is no longer used
      !!
      !! ** Action  :   puvd,  U & V current data interpolated onto model mesh at time-step kt
      !!----------------------------------------------------------------------
      INTEGER                           , INTENT(in   ) ::   kt     ! ocean time-step
      INTEGER                           , INTENT(in   ) ::   Kmm    ! time level index
      REAL(wp), DIMENSION(jpi,jpj,jpk)  , INTENT(  out) ::   pud    ! U & V current data
      REAL(wp), DIMENSION(jpi,jpj,jpk)  , INTENT(  out) ::   pvd    ! U & V current data
      !
      INTEGER ::   ji, jj, jk, jl, jkk               ! dummy loop indicies
      INTEGER ::   ik, il0, il1, ii0, ii1, ij0, ij1  ! local integers
      REAL(wp)::   zl, zi                            ! local floats
#if ! defined key_PSYCLONE_2p5p0
      REAL(wp), ALLOCATABLE, DIMENSION(:) ::  zup, zvp   ! 1D workspace
#else
      REAL(wp), DIMENSION(jpk) ::  zup, zvp          ! 1D workspace
#endif
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dta_uvd')
      !
      CALL fld_read( kt, 1, sf_uvd )      !==   read U & V current data at time step kt   ==!
      !
      pud(:,:,:) = sf_uvd(1)%fnow(:,:,:)                 ! NO mask
      pvd(:,:,:) = sf_uvd(2)%fnow(:,:,:) 
      !
      IF( l_sco ) THEN                   !==   s- or mixed s-zps-coordinate   ==!
         !
#if ! defined key_PSYCLONE_2p5p0
         ALLOCATE( zup(jpk), zvp(jpk) )
#endif
         !
         IF( kt == nit000 .AND. lwp )THEN
            WRITE(numout,*)
            WRITE(numout,*) 'dta_uvd: interpolate U & V current data onto the s- or mixed s-z-coordinate mesh'
         ENDIF
         !
         DO_2D( 1, 1, 1, 1 )           ! vertical interpolation of U & V current:
            DO jk = 1, jpk
               zl = gdept(ji,jj,jk,Kmm)
               IF    ( zl < gdept_1d(1  ) ) THEN          ! extrapolate above the first level of data
                  zup(jk) =  pud(ji,jj,1)
                  zvp(jk) =  pvd(ji,jj,1)
               ELSEIF( zl > gdept_1d(jpk) ) THEN          ! extrapolate below the last level of data
                  zup(jk) =  pud(ji,jj,jpkm1)
                  zvp(jk) =  pvd(ji,jj,jpkm1)
               ELSE                                      ! inbetween : vertical interpolation between jkk & jkk+1
                  DO jkk = 1, jpkm1                      ! when  dept(jkk) < zl < dept(jkk+1)
                     IF( (zl-gdept_1d(jkk)) * (zl-gdept_1d(jkk+1)) <= 0._wp ) THEN
                        zi = ( zl - gdept_1d(jkk) ) / (gdept_1d(jkk+1)-gdept_1d(jkk))
                        zup(jk) = pud(ji,jj,jkk) + ( pud(ji,jj,jkk+1) - pud(ji,jj,jkk) ) * zi 
                        zvp(jk) = pvd(ji,jj,jkk) + ( pvd(ji,jj,jkk+1) - pvd(ji,jj,jkk) ) * zi
                     ENDIF
                  END DO
               ENDIF
            END DO
            DO jk = 1, jpkm1           ! apply mask
               pud(ji,jj,jk) = zup(jk) * umask(ji,jj,jk)
               pvd(ji,jj,jk) = zvp(jk) * vmask(ji,jj,jk)
            END DO
            pud(ji,jj,jpk) = 0._wp
            pvd(ji,jj,jpk) = 0._wp
         END_2D
         ! 
#if ! defined key_PSYCLONE_2p5p0
         DEALLOCATE( zup, zvp )
#endif
         ! 
      ELSE                                !==   z- or zps- coordinate   ==!
         !                             
         pud(:,:,:) = pud(:,:,:) * umask(:,:,:)       ! apply mask
         pvd(:,:,:) = pvd(:,:,:) * vmask(:,:,:)
         !
      ENDIF
      !
      IF( .NOT. ln_uvd_dyndmp    ) THEN   !==   deallocate U & V current structure   ==! 
         !                                !==   (data used only for initialization)  ==!
         IF(lwp) WRITE(numout,*) 'dta_uvd: deallocate U & V current arrays as they are only used to initialize the run'
                                   DEALLOCATE( sf_uvd(1)%fnow )     ! U current arrays in the structure
         IF( sf_uvd(1)%ln_tint )   DEALLOCATE( sf_uvd(1)%fdta )
                                   DEALLOCATE( sf_uvd(2)%fnow )     ! V current arrays in the structure
         IF( sf_uvd(2)%ln_tint )   DEALLOCATE( sf_uvd(2)%fdta )
                                   DEALLOCATE( sf_uvd         )     ! the structure itself
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('dta_uvd')
      !
   END SUBROUTINE dta_uvd

   
   INTEGER FUNCTION dyn_dmp_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION c1d_dmp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( utrdmp(jpi,jpj,jpk), vtrdmp(jpi,jpj,jpk), resto_uv(jpi,jpj,jpk), STAT= dyn_dmp_alloc )
      !
      CALL mpp_sum ( 'dyndmp', dyn_dmp_alloc )
      IF( dyn_dmp_alloc > 0 )   CALL ctl_warn('dyn_dmp_alloc: allocation of arrays failed')
      !
   END FUNCTION dyn_dmp_alloc


   SUBROUTINE dyn_dmp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_dmp_init  ***
      !! 
      !! ** Purpose :   Initialization for the Newtonian damping 
      !!
      !! ** Method  : - get the ln_dyndmp parameter from the namc1d namelist
      !!              - allocate damping arrays
      !!              - check the parameters of the namtra_dmp namelist
      !!              - calculate damping coefficient
      !!----------------------------------------------------------------------
      INTEGER ::   ios, imask   ! local integers
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                           ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dyn_dmp_init : U and V current Newtonian damping'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namc1d        : Set damping flag'
         WRITE(numout,*) '      add a damping term or not       ln_dyndmp = ', ln_dyndmp
         WRITE(numout,*) '   Namelist namtra_dmp    : Set damping parameters'
         WRITE(numout,*) '      Apply relaxation   or not       ln_tradmp = ', ln_tradmp
         WRITE(numout,*) '      mixed layer damping option      nn_zdmp   = ', nn_zdmp
         WRITE(numout,*) '      Damping file name               cn_resto  = ', cn_resto
         WRITE(numout,*)
      ENDIF
      !
      IF( ln_dyndmp ) THEN
         !                                   !==   allocate the data arrays   ==!
         IF( dyn_dmp_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'dyn_dmp_init: unable to allocate arrays' )
         !
         SELECT CASE ( nn_zdmp )             !==   control print of vertical option   ==!
         CASE ( 0    )   ;   IF(lwp) WRITE(numout,*) '   momentum damping throughout the water column'
         CASE ( 1    )   ;   IF(lwp) WRITE(numout,*) '   no momentum damping in the turbocline (avt > 5 cm2/s)'
         CASE ( 2    )   ;   IF(lwp) WRITE(numout,*) '   no momentum damping in the mixed layer'
         CASE DEFAULT
            WRITE(ctmp1,*) '          bad flag value for nn_zdmp = ', nn_zdmp
            CALL ctl_stop(ctmp1)
         END SELECT
         !
         IF( .NOT. ln_uvd_dyndmp ) THEN      ! force the initialization of U & V current data for damping
            CALL ctl_warn( 'dyn_dmp_init: U & V current read data not initialized, we force ln_uvd_dyndmp=T' )
            CALL dta_uvd_init( ld_dyndmp=ln_dyndmp )
         ENDIF
         !
         utrdmp(:,:,:) = 0._wp               ! internal damping trends
         vtrdmp(:,:,:) = 0._wp
         !
         !Read in mask from file
         CALL iom_open ( cn_resto, imask)
         CALL iom_get  ( imask, jpdom_auto, 'resto', resto_uv)
         CALL iom_close( imask )
      ENDIF
      !
   END SUBROUTINE dyn_dmp_init


   SUBROUTINE dyn_dmp( kt, Kbb, Kmm, puu, pvv, Krhs )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE dyn_dmp  ***
      !!                  
      !! ** Purpose :   Compute the momentum trends due to a newtonian damping
      !!      of the ocean velocities towards the given data and add it to the 
      !!      general momentum trends.
      !!
      !! ** Method  :   Compute Newtonian damping towards u_dta and v_dta 
      !!      and add to the general momentum trends:
      !!                     puu(Krhs) = puu(Krhs) + resto_uv * (u_dta - puu(Kbb))
      !!                     pvv(Krhs) = pvv(Krhs) + resto_uv * (v_dta - pvv(Kbb))
      !!      The trend is computed either throughout the water column
      !!      (nn_zdmp=0), where the vertical mixing is weak (nn_zdmp=1) or
      !!      below the well mixed layer (nn_zdmp=2)
      !!
      !! ** Action  : - (puu(:,:,:,Krhs),pvv(:,:,:,Krhs))   momentum trends updated with the damping trend
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT(in   ) ::   kt             ! ocean time-step index
      INTEGER                             , INTENT(in   ) ::   Kbb, Kmm, Krhs ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::   puu, pvv       ! ocean velocities and RHS of momentum equation
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zua, zva     ! local scalars
      REAL(wp), DIMENSION(jpi,jpj,jpk,2) ::   zuv_dta   ! Read in data 
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start( 'dyn_dmp' )
      !
      !
      !                           !==   read and interpolate U & V current data at kt   ==!
      CALL dta_uvd( kt, Kmm, zuv_dta(:,:,:,1), zuv_dta(:,:,:,2))
      !
      SELECT CASE ( nn_zdmp )     !==   Calculate/add Newtonian damping to the momentum trend   ==!
      !
      CASE( 0 )                   ! Newtonian damping throughout the water column
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zua = resto_uv(ji,jj,jk) * ( zuv_dta(ji,jj,jk,1) - puu(ji,jj,jk,Kbb) )
            zva = resto_uv(ji,jj,jk) * ( zuv_dta(ji,jj,jk,2) - pvv(ji,jj,jk,Kbb) )
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) + zua
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) + zva
            utrdmp(ji,jj,jk) = zua           ! save the trends
            vtrdmp(ji,jj,jk) = zva      
         END_3D
         !
      CASE ( 1 )                  ! no damping above the turbocline (avt > 5 cm2/s)
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            IF( avt(ji,jj,jk) <= avt_c ) THEN
               zua = resto_uv(ji,jj,jk) * ( zuv_dta(ji,jj,jk,1) - puu(ji,jj,jk,Kbb) )
               zva = resto_uv(ji,jj,jk) * ( zuv_dta(ji,jj,jk,2) - pvv(ji,jj,jk,Kbb) )
            ELSE
               zua = 0._wp
               zva = 0._wp  
            ENDIF
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) + zua
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) + zva
            utrdmp(ji,jj,jk) = zua           ! save the trends
            vtrdmp(ji,jj,jk) = zva
         END_3D
         !
      CASE ( 2 )                  ! no damping in the mixed layer
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            IF( gdept(ji,jj,jk,Kmm) >= hmlp (ji,jj) ) THEN
               zua = resto_uv(ji,jj,jk) * ( zuv_dta(ji,jj,jk,1) - puu(ji,jj,jk,Kbb) )
               zva = resto_uv(ji,jj,jk) * ( zuv_dta(ji,jj,jk,2) - pvv(ji,jj,jk,Kbb) )
            ELSE
               zua = 0._wp
               zva = 0._wp  
            ENDIF
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) + zua
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) + zva
            utrdmp(ji,jj,jk) = zua           ! save the trends
            vtrdmp(ji,jj,jk) = zva
         END_3D
         !
      END SELECT
      !
      !                           ! Control print
      IF( sn_cfctl%l_prtctl   )   CALL prt_ctl( tab3d_1=puu(:,:,:,Krhs), clinfo1=' dmp  - Ua: ', mask1=umask,   &
         &                                      tab3d_2=pvv(:,:,:,Krhs), clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
      !
      IF( ln_timing )   CALL timing_stop( 'dyn_dmp')
      !
   END SUBROUTINE dyn_dmp
   
   !!======================================================================
END MODULE c1d
