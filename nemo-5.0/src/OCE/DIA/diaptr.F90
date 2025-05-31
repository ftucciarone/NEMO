MODULE diaptr
   !!======================================================================
   !!                       ***  MODULE  diaptr  ***
   !! Ocean physics:  Computes meridonal transports and zonal means
   !!=====================================================================
   !! History :  1.0  ! 2003-09  (C. Talandier, G. Madec)  Original code
   !!            2.0  ! 2006-01  (A. Biastoch)  Allow sub-basins computation
   !!            3.2  ! 2010-03  (O. Marti, S. Flavoni) Add fields
   !!            3.3  ! 2010-10  (G. Madec)  dynamical allocation
   !!            3.6  ! 2014-12  (C. Ethe) use of IOM
   !!            3.6  ! 2016-06  (T. Graham) Addition of diagnostics for CMIP6
   !!            4.0  ! 2010-08  ( C. Ethe, J. Deshayes ) Improvment
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dia_ptr      : Poleward Transport Diagnostics module
   !!   dia_ptr_init : Initialization, namelist read
   !!   ptr_sjk      : "zonal" mean computation of a field - tracer or flux array
   !!   ptr_sj       : "zonal" and vertical sum computation of a "meridional" flux array
   !!                   (Generic interface to ptr_sj_3d, ptr_sj_2d)
   !!----------------------------------------------------------------------
   USE oce              ! ocean dynamics and active tracers
   USE dom_oce          ! ocean space and time domain
   USE domtile
   USE phycst           ! physical constants
   !
   USE iom              ! IOM library
   USE in_out_manager   ! I/O manager
   USE lib_mpp          ! MPP library
   USE timing           ! preformance summary

   IMPLICIT NONE
   PRIVATE

   INTERFACE ptr_sum
      MODULE PROCEDURE ptr_sum_2d, ptr_sum_3d
   END INTERFACE
   INTERFACE ptr_mpp_sum
      MODULE PROCEDURE ptr_mpp_sum_2d, ptr_mpp_sum_3d, ptr_mpp_sum_4d
   END INTERFACE
   INTERFACE ptr_sj
      MODULE PROCEDURE ptr_sj_2d, ptr_sj_3d
   END INTERFACE
   INTERFACE dia_ptr_hst
      MODULE PROCEDURE dia_ptr_hst_2d, dia_ptr_hst_3d
   END INTERFACE

   PUBLIC   dia_ptr        ! call in step module
   PUBLIC   dia_ptr_init   ! call in stprk3 module
   PUBLIC   dia_ptr_hst    ! called from tra_ldf/tra_adv routines

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   hstr_all   !: Heat/salt transports(adv, ldf, eiv) + heat/salt transport (meridional)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   pvtr_int, pzon_int             !: Other zonal integrals

   LOGICAL, PUBLIC    ::   l_diaptr       !: tracers  trend flag
   INTEGER, PARAMETER ::   jp_msk = 3
   INTEGER, PARAMETER ::   jp_vtr = 4

   REAL(wp) ::   rc_sv    = 1.e-6_wp   ! conversion from m3/s to Sverdrup
   REAL(wp) ::   rc_pwatt = 1.e-15_wp  ! conversion from W    to PW (further x rho0 x Cp)
   REAL(wp) ::   rc_ggram = 1.e-9_wp   ! conversion from g    to Gg  (further x rho0)

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: btmsk   ! T-point basin interior masks
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: btmsk34 ! mask out Southern Ocean (=0 south of 34°S)

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   ! NOTE: [tiling] tiling sometimes changes the diagnostics very slightly, usually where there are few zonal points e.g. the northern Indian Ocean basin. The difference is usually very small, for one point in one diagnostic. Presumably this is because of the additional zonal integration step over tiles.
   SUBROUTINE dia_ptr( kt, Kmm, pvtr )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr  ***
      !!----------------------------------------------------------------------
      INTEGER                    , INTENT(in)           ::   kt     ! ocean time-step index
      INTEGER                    , INTENT(in)           ::   Kmm    ! time level index
      REAL(wp), DIMENSION(:,:,:) , INTENT(in), OPTIONAL ::   pvtr   ! j-effective transport
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dia_ptr')
      !
      ! Calculate zonal integrals
      IF( PRESENT( pvtr ) ) THEN
         CALL dia_ptr_zint( Kmm, pvtr )
      ELSE
         CALL dia_ptr_zint( Kmm )
      ENDIF

      ! Calculate diagnostics only when zonal integrals have finished
      IF( .NOT. l_istiled .OR. ntile == nijtile ) CALL dia_ptr_iom(kt, Kmm, pvtr)         ! Do only for the last tile

      IF( ln_timing )   CALL timing_stop('dia_ptr')
      !
   END SUBROUTINE dia_ptr


   SUBROUTINE dia_ptr_iom( kt, Kmm, pvtr )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr_iom  ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Calculate diagnostics and send to XIOS
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in)           ::   kt     ! ocean time-step index
      INTEGER                   , INTENT(in)           ::   Kmm    ! time level index
      REAL(wp), DIMENSION(:,:,:), INTENT(in), OPTIONAL ::   pvtr   ! j-effective transport (used only by PRESENT)
      !
      INTEGER  ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp), DIMENSION(A2D(0))   ::  z2d                   ! 2D workspace
      !
      !overturning calculation
      REAL(wp) ::   zmsk0
      REAL(wp), DIMENSION(:)      , ALLOCATABLE ::   zmsk1
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE ::   z4d1, z4d2
      REAL(wp), DIMENSION(:,:,:  ), ALLOCATABLE ::   z3dtr, zstr
      !!----------------------------------------------------------------------
      !
      ALLOCATE( z3dtr(A2D(0),nbasin) )

      IF( PRESENT( pvtr ) ) THEN
         IF( lk_mpp ) CALL ptr_mpp_sum( pvtr_int(:,:,:,:) )      ! Call MPI sum

         IF( iom_use( 'zomsf' ) ) THEN    ! effective MSF
            ALLOCATE( z4d1(A2D(0),jpk,nbasin) )
            !
            DO jn = 1, nbasin                                    ! by sub-basins
               z4d1(Nis0,:,:,jn) =  pvtr_int(:,:,jn,jp_vtr)                  ! zonal cumulative effective transport excluding closed seas
               DO jk = jpkm1, 1, -1
                  z4d1(Nis0,:,jk,jn) = z4d1(Nis0,:,jk+1,jn) - z4d1(Nis0,:,jk,jn)    ! effective j-Stream-Function (MSF)
               END DO
               DO ji = Nis0+1, Nie0
                  z4d1(ji,:,:,jn) = z4d1(Nis0,:,:,jn)
               ENDDO
            END DO
            CALL iom_put( 'zomsf', z4d1 * rc_sv )
            !
            DEALLOCATE( z4d1 )
         ENDIF
         IF( iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) ) THEN
            ALLOCATE( zstr(A1Dj(0),nbasin,jpts) )
            !
            DO jn = 1, nbasin
               zstr(:,jn,jp_tem) = 0._wp
               zstr(:,jn,jp_sal) = 0._wp
               ! i-mean T and S, j-Stream-Function, basin
               DO jk = 1, jpk
                  DO jj = Njs0, Nje0
                     zmsk0 = pvtr_int(jj,jk,jn,jp_msk)
                     IF( zmsk0 /= 0._wp ) THEN
                        zstr(jj,jn,jp_tem) = zstr(jj,jn,jp_tem) + pvtr_int(jj,jk,jn,jp_vtr)*pvtr_int(jj,jk,jn,jp_tem) / zmsk0
                        zstr(jj,jn,jp_sal) = zstr(jj,jn,jp_sal) + pvtr_int(jj,jk,jn,jp_vtr)*pvtr_int(jj,jk,jn,jp_sal) / zmsk0
                     ENDIF
                  END DO
               END DO
            ENDDO
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = zstr(:,jn,jp_tem) * rc_pwatt  !  (conversion in PW)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtove', z3dtr )
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = zstr(:,jn,jp_sal) * rc_ggram  !  (conversion in Gg)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstove', z3dtr )
            !
            DEALLOCATE( zstr )
         ENDIF

         IF( iom_use( 'sopstbtr' ) .OR. iom_use( 'sophtbtr' ) ) THEN
            ! Calculate barotropic heat and salt transport here
            ALLOCATE( zmsk1(A1Dj(0)), zstr(A1Dj(0),nbasin,jpts) )
            !
            DO jn = 1, nbasin
               !
               zmsk1(:) = SUM( pvtr_int(:,:,jn,jp_msk), 2 )
               !
               WHERE( zmsk1 /= 0._wp )
                  zstr(:,jn,jp_tem) = SUM( pvtr_int(:,:,jn,jp_vtr), 2 ) * SUM( pvtr_int(:,:,jn,jp_tem), 2 ) / zmsk1
                  zstr(:,jn,jp_sal) = SUM( pvtr_int(:,:,jn,jp_vtr), 2 ) * SUM( pvtr_int(:,:,jn,jp_sal), 2 ) / zmsk1
               ELSEWHERE
                  zstr(:,jn,jp_tem) = 0._wp
                  zstr(:,jn,jp_sal) = 0._wp
               ENDWHERE
               !
            ENDDO
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = zstr(:,jn,jp_tem) * rc_pwatt  !  (conversion in PW)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtbtr', z3dtr )
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = zstr(:,jn,jp_sal) * rc_ggram  !  (conversion in Gg)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstbtr', z3dtr )
            !
            DEALLOCATE( zmsk1, zstr )
         ENDIF
         !
         pvtr_int(:,:,:,:) = 0._wp
      ELSE
         IF( lk_mpp ) THEN             ! Call MPI sum
            CALL ptr_mpp_sum( hstr_all(:,:,:,:) )
            CALL ptr_mpp_sum( pzon_int(:,:,:,:) )
         ENDIF

         IF( iom_use( 'zotem' ) .OR. iom_use( 'zosal' ) .OR. iom_use( 'zosrf' )  ) THEN    ! i-mean i-k-surface
            ALLOCATE( z4d1(A2D(0),jpk,nbasin), z4d2(A2D(0),jpk,nbasin) )
            !
            DO jn = 1, nbasin
               DO jk = 1, jpk
                  DO jj = Njs0, Nje0
                     z4d1(Nis0,jj,jk,jn) = pzon_int(jj,jk,jn,jp_msk)
                     DO ji = Nis0+1, Nie0
                        z4d1(ji,jj,jk,jn) = z4d1(Nis0,jj,jk,jn)
                     ENDDO
                  END DO
               ENDDO
            ENDDO
            CALL iom_put( 'zosrf', z4d1 )
            !
            DO jn = 1, nbasin
               DO jk = 1, jpk
                  DO jj = Njs0, Nje0
                     z4d2(Nis0,jj,jk,jn) = pzon_int(jj,jk,jn,jp_tem) / MAX( z4d1(Nis0,jj,jk,jn), 10.e-15 )
                     DO ji = Nis0+1, Nie0
                        z4d2(ji,jj,jk,jn) = z4d2(Nis0,jj,jk,jn)
                     END DO
                  END DO
               ENDDO
            ENDDO
            CALL iom_put( 'zotem', z4d2 )
            !
            DO jn = 1, nbasin
               DO jk = 1, jpk
                  DO jj = Njs0, Nje0
                     z4d2(Nis0,jj,jk,jn) = pzon_int(jj,jk,jn,jp_sal) / MAX( z4d1(Nis0,jj,jk,jn), 10.e-15 )
                     DO ji = Nis0+1, Nie0
                        z4d2(ji,jj,jk,jn) = z4d2(Nis0,jj,jk,jn)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            CALL iom_put( 'zosal', z4d2 )
            !
            DEALLOCATE( z4d1, z4d2 )
         ENDIF
         !
         !                                ! Advective and diffusive heat and salt transport
         IF( iom_use( 'sophtadv' ) .OR. iom_use( 'sopstadv' ) ) THEN
            !
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_tem,1) * rc_pwatt  !  (conversion in PW)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtadv', z3dtr )
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_sal,1) * rc_ggram !  (conversion in Gg)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstadv', z3dtr )
         ENDIF
         !
         IF( iom_use( 'sophtldf' ) .OR. iom_use( 'sopstldf' ) ) THEN
            !
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_tem,2) * rc_pwatt  !  (conversion in PW)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophtldf', z3dtr )
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_sal,2) * rc_ggram !  (conversion in Gg)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstldf', z3dtr )
         ENDIF
         !
         IF( iom_use( 'sophteiv' ) .OR. iom_use( 'sopsteiv' ) ) THEN
            !
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_tem,3) * rc_pwatt  !  (conversion in PW)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sophteiv', z3dtr )
            DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_sal,3) * rc_ggram !  (conversion in Gg)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopsteiv', z3dtr )
         ENDIF
         !
         IF( iom_use( 'sopstvtr' ) .OR. iom_use( 'sophtvtr' ) ) THEN
             DO jn = 1, nbasin
                z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_tem,4) * rc_pwatt  !  (conversion in PW)
                DO ji = Nis0+1, Nie0
                   z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
                ENDDO
             ENDDO
             CALL iom_put( 'sophtvtr', z3dtr )
             DO jn = 1, nbasin
               z3dtr(Nis0,:,jn) = hstr_all(:,jn,jp_sal,4) * rc_ggram !  (conversion in Gg)
               DO ji = Nis0+1, Nie0
                  z3dtr(ji,:,jn) = z3dtr(Nis0,:,jn)
               ENDDO
            ENDDO
            CALL iom_put( 'sopstvtr', z3dtr )
         ENDIF
         !
         IF( iom_use( 'uocetr_vsum_cumul' ) ) THEN
            CALL iom_get_var(  'uocetr_vsum_op', z2d ) ! get uocetr_vsum_op from xml
            z2d(:,:) = ptr_ci_2d( z2d(:,:) )
            CALL iom_put( 'uocetr_vsum_cumul', z2d )
         ENDIF
         !
         hstr_all(:,:,:,:) = 0._wp       ! Zero before next timestep
         pzon_int(:,:,:,:) = 0._wp
      ENDIF
      !
      DEALLOCATE( z3dtr )
      !
   END SUBROUTINE dia_ptr_iom


   SUBROUTINE dia_ptr_zint( Kmm, pvtr )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ptr_zint ***
      !!----------------------------------------------------------------------
      !! ** Purpose : i and i-k sum operations on arrays
      !!
      !! ** Method  : - Call ptr_sjk (i sum) or ptr_sj (i-k sum) to perform the sum operation
      !!              - Call ptr_sum to add this result to the sum over tiles
      !!
      !! ** Action  : pvtr_int - terms for volume streamfunction, heat/salt transport barotropic/overturning terms
      !!              pzon_int - terms for i mean temperature/salinity
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in)             :: Kmm          ! time level index
      REAL(wp), DIMENSION(:,:,:), INTENT(in), OPTIONAL   :: pvtr         ! j-effective transport
      !
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   zts          ! 4D workspace
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   sjk          ! Zonal sum: i-k surface area, j-effective transport, (T, S)
      INTEGER  ::   ji, jj, jk, jn                              ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      ALLOCATE( zts(T2D(0),jpk), sjk(T1Dj(0),jpk,nbasin) )
      !
      IF( PRESENT( pvtr ) ) THEN
         ! i sum of effective j transport excluding closed seas
         IF( iom_use( 'zomsf' ) .OR. iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) ) THEN
            !
            DO jn = 1, nbasin
               sjk(:,:,jn) = ptr_sjk( pvtr(:,:,:), btmsk34(:,:,jn) )
            ENDDO
            !
            CALL ptr_sum( pvtr_int(:,:,:,jp_vtr), sjk(:,:,:) )
            !
         ENDIF

         ! i sum of j surface area, j surface area - temperature/salinity product on V grid
         IF(  iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) .OR. iom_use( 'sopstbtr' ) .OR. iom_use( 'sophtbtr' ) ) THEN
            !
            ! mask
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = vmask(ji,jj,jk) * e1v(ji,jj) * e3v(ji,jj,jk,Kmm)
            END_3D
            DO jn = 1, nbasin
               sjk(:,:,jn) = ptr_sjk( zts(:,:,:), btmsk(:,:,jn) )
            ENDDO
            CALL ptr_sum( pvtr_int(:,:,:,jp_msk), sjk(:,:,:) )
            ! temperature
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = 0.5 * ( ts(ji,jj,jk,jp_tem,Kmm) + ts(ji,jj+1,jk,jp_tem,Kmm) ) &
                  &                *  e1v(ji,jj) * e3v(ji,jj,jk,Kmm)  ! Tracers averaged onto V grid
            END_3D
            DO jn = 1, nbasin
               sjk(:,:,jn) = ptr_sjk( zts(:,:,:), btmsk(:,:,jn) )
            ENDDO
            CALL ptr_sum( pvtr_int(:,:,:,jp_tem), sjk(:,:,:) )
            ! salinity
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = 0.5 * ( ts(ji,jj,jk,jp_sal,Kmm) + ts(ji,jj+1,jk,jp_sal,Kmm) ) &
                  &                *  e1v(ji,jj) * e3v(ji,jj,jk,Kmm)
            END_3D
            DO jn = 1, nbasin
               sjk(:,:,jn) = ptr_sjk( zts(:,:,:), btmsk(:,:,jn) )
            ENDDO            
            CALL ptr_sum( pvtr_int(:,:,:,jp_sal), sjk(:,:,:) )
            !
         ENDIF
      ELSE
         ! i sum of j surface area - temperature/salinity product on T grid
         IF( iom_use( 'zotem' ) .OR. iom_use( 'zosal' ) .OR. iom_use( 'zosrf' )  ) THEN
            !
            ! mask
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = tmask(ji,jj,jk) * e1t(ji,jj) * e3t(ji,jj,jk,Kmm)
            END_3D
            DO jn = 1, nbasin
               sjk(:,:,jn) = ptr_sjk( zts(:,:,:) , btmsk(:,:,jn) )
            ENDDO
            CALL ptr_sum( pzon_int(:,:,:,jp_msk), sjk(:,:,:) )
            ! temperature
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = ts(ji,jj,jk,jp_tem,Kmm) * e1t(ji,jj) * e3t(ji,jj,jk,Kmm)
            END_3D
            DO jn = 1, nbasin
               sjk(:,:,jn) = ptr_sjk( zts(:,:,:), btmsk(:,:,jn) )
            ENDDO       
            CALL ptr_sum( pzon_int(:,:,:,jp_tem), sjk(:,:,:) )
            ! salinity
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = ts(ji,jj,jk,jp_sal,Kmm) * e1t(ji,jj) * e3t(ji,jj,jk,Kmm)
            END_3D
            DO jn = 1, nbasin
               sjk(:,:,jn) = ptr_sjk( zts(:,:,:), btmsk(:,:,jn) )
            ENDDO       
            CALL ptr_sum( pzon_int(:,:,:,jp_sal), sjk(:,:,:) )
            !
         ENDIF

         ! i-k sum of j surface area - temperature/salinity product on V grid
         IF( iom_use( 'sopstvtr' ) .OR. iom_use( 'sophtvtr' ) ) THEN
            !
            ! temperature
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = 0.5 * ( ts(ji,jj,jk,jp_tem,Kmm) + ts(ji,jj+1,jk,jp_tem,Kmm) ) &
                  &                *  e1v(ji,jj) * e3v(ji,jj,jk,Kmm) !Tracers averaged onto V grid
            END_3D
            CALL dia_ptr_hst( jp_tem, 'vtr', zts(:,:,:) )
            ! salinity
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               zts(ji,jj,jk) = 0.5 * ( ts(ji,jj,jk,jp_sal,Kmm) + ts(ji,jj+1,jk,jp_sal,Kmm) ) &
                  &                *  e1v(ji,jj) * e3v(ji,jj,jk,Kmm)
            END_3D
            CALL dia_ptr_hst( jp_sal, 'vtr', zts(:,:,:) )
            !
         ENDIF
      ENDIF
      !
      DEALLOCATE( zts, sjk )
      !
   END SUBROUTINE dia_ptr_zint


   SUBROUTINE dia_ptr_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ptr_init  ***
      !!
      !! ** Purpose :   Initialization
      !!----------------------------------------------------------------------
      INTEGER ::  inum, jn           ! local integers
      !!
      REAL(wp), DIMENSION(jpi,jpj) :: zmsk
      !!----------------------------------------------------------------------

      ! l_diaptr is defined with iom_use
      !   --> dia_ptr_init must be done after the call to iom_init
      !   --> cannot be .TRUE. without cpp key: key_xios -->  nbasin define by iom_init is initialized
      l_diaptr = iom_use( 'zomsf'    ) .OR. iom_use( 'zotem'    ) .OR. iom_use( 'zosal'    ) .OR.  &
         &       iom_use( 'zosrf'    ) .OR. iom_use( 'sopstove' ) .OR. iom_use( 'sophtove' ) .OR.  &
         &       iom_use( 'sopstbtr' ) .OR. iom_use( 'sophtbtr' ) .OR. iom_use( 'sophtadv' ) .OR.  &
         &       iom_use( 'sopstadv' ) .OR. iom_use( 'sophtldf' ) .OR. iom_use( 'sopstldf' ) .OR.  &
         &       iom_use( 'sophteiv' ) .OR. iom_use( 'sopsteiv' ) .OR. iom_use( 'sopstvtr' ) .OR.  &
         &       iom_use( 'sophtvtr' ) .OR. iom_use( 'uocetr_vsum_cumul' )

      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'dia_ptr_init : poleward transport and msf initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '      Poleward heat & salt transport (T) or not (F)      l_diaptr  = ', l_diaptr
      ENDIF

      IF( l_diaptr ) THEN
         !
         IF( dia_ptr_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'dia_ptr_init : unable to allocate arrays' )
         !
         rc_pwatt = rc_pwatt * rho0_rcp          ! conversion from K.s-1 to PetaWatt
         rc_ggram = rc_ggram * rho0              ! conversion from m3/s to Gg/s

         IF( lk_mpp )   CALL mpp_ini_znl( numout )     ! Define MPI communicator for zonal sum

         btmsk(:,:,1) = tmask_i(:,:)
         IF( nbasin == 5 ) THEN   ! nbasin has been initialized in iom_init to define the axis "basin"
            CALL iom_open( 'subbasins', inum )
            CALL iom_get( inum, jpdom_global, 'atlmsk', btmsk(:,:,2) )   ! Atlantic basin
            CALL iom_get( inum, jpdom_global, 'pacmsk', btmsk(:,:,3) )   ! Pacific  basin
            CALL iom_get( inum, jpdom_global, 'indmsk', btmsk(:,:,4) )   ! Indian   basin
            CALL iom_close( inum )
            btmsk(:,:,5) = MAX ( btmsk(:,:,3), btmsk(:,:,4) )            ! Indo-Pacific basin
         ENDIF
         DO jn = 2, nbasin
            btmsk(:,:,jn) = btmsk(:,:,jn) * tmask_i(:,:)                 ! interior domain only
         END DO
         ! JD : modification so that overturning streamfunction is available in Atlantic at 34S to compare with observations
         WHERE( gphit(:,:)*tmask_i(:,:) < -34._wp)
           zmsk(:,:) = 0._wp      ! mask out Southern Ocean
         ELSE WHERE
           zmsk(:,:) = ssmask(:,:)
         END WHERE
         btmsk34(:,:,1) = btmsk(:,:,1)
         DO jn = 2, nbasin
            btmsk34(:,:,jn) = btmsk(:,:,jn) * zmsk(:,:)                  ! interior domain only
         ENDDO

         ! Initialise arrays to zero because diatpr is called before they are first calculated
         ! Note that this means diagnostics will not be exactly correct when model run is restarted.
         hstr_all(:,:,:,:) = 0._wp
         pvtr_int(:,:,:,:) = 0._wp
         pzon_int(:,:,:,:) = 0._wp
         !
      ENDIF
      !
   END SUBROUTINE dia_ptr_init


   SUBROUTINE dia_ptr_hst_2d( ktra, cptr, pvflx )
      !!
      INTEGER,                  INTENT(in)  ::  ktra  ! tracer index
      CHARACTER(len=3),         INTENT(in)  ::  cptr  ! transport type  'adv'/'ldf'/'eiv'
      REAL(wp), DIMENSION(:,:), INTENT(in)  ::  pvflx ! 2D input array of advection/diffusion
      !!
      CALL dia_ptr_hst_t( ktra, cptr, pvflx2d=pvflx(:,:), ktvflx=lbnd_ij(pvflx) )
   END SUBROUTINE dia_ptr_hst_2d


   SUBROUTINE dia_ptr_hst_3d( ktra, cptr, pvflx )
      !!
      INTEGER,                    INTENT(in)  ::  ktra  ! tracer index
      CHARACTER(len=3),           INTENT(in)  ::  cptr  ! transport type  'adv'/'ldf'/'eiv'
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::  pvflx ! 3D input array of advection/diffusion
      !!
      CALL dia_ptr_hst_t( ktra, cptr, pvflx3d=pvflx(:,:,:), ktvflx=lbnd_ij(pvflx) )
   END SUBROUTINE dia_ptr_hst_3d


   SUBROUTINE dia_ptr_hst_t( ktra, cptr, pvflx2d, pvflx3d, ktvflx )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ptr_hst ***
      !!----------------------------------------------------------------------
      !! Wrapper for heat and salt transport calculations to calculate them for each basin
      !! Called from all advection and/or diffusion routines
      !!----------------------------------------------------------------------
      INTEGER,  DIMENSION(2),                INTENT(in)            ::  ktvflx
      INTEGER,                               INTENT(in)            ::  ktra    ! tracer index
      CHARACTER(len=3),                      INTENT(in)            ::  cptr    ! transport type  'adv'/'ldf'/'eiv'
      REAL(wp), DIMENSION(AB2D(ktvflx)),     INTENT(in), OPTIONAL  ::  pvflx2d ! 2D input array of advection/diffusion
      REAL(wp), DIMENSION(AB2D(ktvflx),JPK), INTENT(in), OPTIONAL  ::  pvflx3d ! 3D input array of advection/diffusion
      !!
      REAL(wp), DIMENSION(T1Dj(0),nbasin)                          ::  zsj     !
      INTEGER                                                      ::  jn      !
      !!----------------------------------------------------------------------
      IF( PRESENT(pvflx2d) ) THEN
         DO jn = 1, nbasin
            zsj(:,jn) = ptr_sj( pvflx2d(:,:),   btmsk(:,:,jn) )
         END DO
      ELSE IF( PRESENT(pvflx3d) ) THEN
         DO jn = 1, nbasin
            zsj(:,jn) = ptr_sj( pvflx3d(:,:,:), btmsk(:,:,jn) )
         END DO
      ENDIF
      !
      IF( cptr == 'adv' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_all(:,:,jp_tem,1), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_all(:,:,jp_sal,1), zsj(:,:) )
      ELSE IF( cptr == 'ldf' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_all(:,:,jp_tem,2), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_all(:,:,jp_sal,2), zsj(:,:) )
      ELSE IF( cptr == 'eiv' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_all(:,:,jp_tem,3), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_all(:,:,jp_sal,3), zsj(:,:) )
      ELSE IF( cptr == 'vtr' ) THEN
         IF( ktra == jp_tem )  CALL ptr_sum( hstr_all(:,:,jp_tem,4), zsj(:,:) )
         IF( ktra == jp_sal )  CALL ptr_sum( hstr_all(:,:,jp_sal,4), zsj(:,:) )
      ENDIF
      !
   END SUBROUTINE dia_ptr_hst_t


   SUBROUTINE ptr_sum_2d( phstr, pva )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sum_2d ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Add two 2D arrays with (j,nbasin) dimensions
      !!
      !! ** Method  : - phstr = phstr + pva
      !!
      !! ** Action  : phstr
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(A1Dj(0),nbasin), INTENT(inout)  ::  phstr  !
      REAL(wp), DIMENSION(T1Dj(0),nbasin), INTENT(in   )  ::  pva    !
      INTEGER                                             ::  jj, jn
      !!----------------------------------------------------------------------
      DO jn = 1, nbasin
         DO_1Dj( 0, 0 )
            phstr(jj,jn) = phstr(jj,jn)  + pva(jj,jn)
         END_1D
      END DO
   END SUBROUTINE ptr_sum_2d


   SUBROUTINE ptr_sum_3d( phstr, pva )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sum_3d ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Add two 3D arrays with (j,k,nbasin) dimensions
      !!
      !! ** Method  : - phstr = phstr + pva
      !!
      !! ** Action  : phstr
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(A1Dj(0),jpk,nbasin), INTENT(inout)   ::  phstr  !
      REAL(wp), DIMENSION(T1Dj(0),jpk,nbasin), INTENT(in   )   ::  pva    !
      INTEGER                                                  ::  jj, jk, jn
      !!----------------------------------------------------------------------
      DO jn = 1, nbasin
         DO jk = 1, jpk
            DO_1Dj( 0, 0 )
               phstr(jj,jk,jn) = phstr(jj,jk,jn) + pva(jj,jk,jn)
            END_1D
         END DO
      END DO
   END SUBROUTINE ptr_sum_3d


   SUBROUTINE ptr_mpp_sum_2d( phstr )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_mpp_sum_2d ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Call mpp_sum for 2D flux array
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(inout)  ::  phstr  !
      INTEGER                              ::  isize
      REAL(wp), DIMENSION(:), ALLOCATABLE  ::  zwork
      !!----------------------------------------------------------------------
#if ! defined key_mpi_off
      isize = SIZE(phstr)
      ALLOCATE( zwork(isize) )
      zwork(:) = RESHAPE( phstr(:,:), (/ isize /) )
      CALL mpp_sum( 'diaptr', zwork, ncomm_znl )
      phstr(:,:) = RESHAPE( zwork, SHAPE(phstr) )
      DEALLOCATE( zwork )
#endif
   END SUBROUTINE ptr_mpp_sum_2d


   SUBROUTINE ptr_mpp_sum_3d( phstr )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_mpp_sum_3d ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Call mpp_sum for 3D flux array
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), INTENT(inout)   ::  phstr  !
      INTEGER                              ::  isize
      REAL(wp), DIMENSION(:), ALLOCATABLE  ::  zwork
      !!----------------------------------------------------------------------
#if ! defined key_mpi_off
      isize = SIZE(phstr)
      ALLOCATE( zwork(isize) )
      zwork(:) = RESHAPE( phstr(:,:,:), (/ isize /) )
      CALL mpp_sum( 'diaptr', zwork, ncomm_znl )
      phstr(:,:,:) = RESHAPE( zwork, SHAPE(phstr) )
      DEALLOCATE( zwork )
#endif
   END SUBROUTINE ptr_mpp_sum_3d


   SUBROUTINE ptr_mpp_sum_4d( phstr )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_mpp_sum_4d ***
      !!----------------------------------------------------------------------
      !! ** Purpose : Call mpp_sum for 4D flux array
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout)   ::  phstr  !
      INTEGER                              ::  isize
      REAL(wp), DIMENSION(:), ALLOCATABLE  ::  zwork
      !!----------------------------------------------------------------------
#if ! defined key_mpi_off
      isize = SIZE(phstr)
      ALLOCATE( zwork(isize) )
      zwork(:) = RESHAPE( phstr(:,:,:,:), (/ isize /) )
      CALL mpp_sum( 'diaptr', zwork, ncomm_znl )
      phstr(:,:,:,:) = RESHAPE( zwork, SHAPE(phstr) )
      DEALLOCATE( zwork )
#endif
   END SUBROUTINE ptr_mpp_sum_4d


   FUNCTION dia_ptr_alloc()
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ptr_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER               ::   dia_ptr_alloc   ! return value
      INTEGER, DIMENSION(2) ::   ierr
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ! nbasin has been initialized in iom_init to define the axis "basin"
      !
      IF( .NOT. ALLOCATED( btmsk ) ) THEN
         ALLOCATE( btmsk(A2D(nn_hls),nbasin)    , btmsk34(A2D(nn_hls),nbasin)  , &
            &      hstr_all(A1Dj(0),nbasin,jpts,4), STAT=ierr(1)  )
            !
         ALLOCATE( pvtr_int(A1Dj(0),jpk,nbasin,jpts+2), &
            &      pzon_int(A1Dj(0),jpk,nbasin,jpts+1), STAT=ierr(2) )
         !
         dia_ptr_alloc = MAXVAL( ierr )
         CALL mpp_sum( 'diaptr', dia_ptr_alloc )
      ENDIF
      !
   END FUNCTION dia_ptr_alloc


   FUNCTION ptr_sj_3d( pvflx, pmsk )   RESULT ( p_fval )
      !!
      REAL(wp), INTENT(in), DIMENSION(:,:,:)       ::  pvflx  ! mask flux array at V-point
      REAL(wp), INTENT(in), DIMENSION(A2D(nn_hls)) ::  pmsk   ! Optional 2D basin mask
      !
      REAL(wp), DIMENSION(T1Dj(0)) :: p_fval              ! i-k-mean poleward flux of pvflx
      !!
      CALL ptr_sj_3d_t( pvflx(:,:,:), lbnd_ij(pvflx), pmsk(:,:), p_fval(:) )
   END FUNCTION ptr_sj_3d


   SUBROUTINE ptr_sj_3d_t( pvflx, ktvflx, pmsk, p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sj_3d  ***
      !!
      !! ** Purpose :   i-k sum computation of a j-flux array
      !!
      !! ** Method  : - i-k sum of pvflx using the interior 2D vmask (vmask_i).
      !!              pvflx is supposed to be a masked flux (i.e. * vmask*e1v*e3v)
      !!
      !! ** Action  : - p_fval: i-k-mean poleward flux of pvflx
      !!----------------------------------------------------------------------
      INTEGER,  INTENT(in   ), DIMENSION(2)                 ::   ktvflx
      REAL(wp), INTENT(in   ), DIMENSION(AB2D(ktvflx),JPK)  ::   pvflx  ! mask flux array at V-point
      REAL(wp), INTENT(in   ), DIMENSION(A2D(nn_hls))       ::   pmsk   ! Optional 2D basin mask
      REAL(wp), INTENT(  out), DIMENSION(T1Dj(0))           ::   p_fval ! i-k-mean poleward flux of pvflx
      !
      INTEGER :: ji, jj, jk  ! dummy loop arguments
      !!--------------------------------------------------------------------
      !
      p_fval(:) = 0._wp
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         p_fval(jj) = p_fval(jj) + pvflx(ji,jj,jk) * pmsk(ji,jj) * tmask_i(ji,jj)
      END_3D
   END SUBROUTINE ptr_sj_3d_t


   FUNCTION ptr_sj_2d( pvflx, pmsk )   RESULT ( p_fval )
      !!
      REAL(wp), INTENT(in), DIMENSION(:,:)         ::  pvflx  ! mask flux array at V-point
      REAL(wp), INTENT(in), DIMENSION(A2D(nn_hls)) ::  pmsk   ! Optional 2D basin mask
      !
      REAL(wp), DIMENSION(T1Dj(0)) :: p_fval              ! i-k-mean poleward flux of pvflx
      !!
      CALL ptr_sj_2d_t( pvflx(:,:), lbnd_ij(pvflx), pmsk(:,:), p_fval(:) )
   END FUNCTION ptr_sj_2d


   SUBROUTINE ptr_sj_2d_t( pvflx, ktvflx, pmsk, p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sj_2d  ***
      !!
      !! ** Purpose :   "zonal" and vertical sum computation of a j-flux array
      !!
      !! ** Method  : - i-k sum of pvflx using the interior 2D vmask (vmask_i).
      !!      pvflx is supposed to be a masked flux (i.e. * vmask*e1v*e3v)
      !!
      !! ** Action  : - p_fval: i-k-mean poleward flux of pvflx
      !!----------------------------------------------------------------------
      INTEGER,  INTENT(in   ), DIMENSION(2)             ::   ktvflx
      REAL(wp), INTENT(in   ), DIMENSION(AB2D(ktvflx))  ::   pvflx  ! mask flux array at V-point
      REAL(wp), INTENT(in   ), DIMENSION(A2D(nn_hls))   ::   pmsk   ! Optional 2D basin mask
      REAL(wp), INTENT(  out), DIMENSION(T1Dj(0))       ::   p_fval ! i-k-mean poleward flux of pvflx
      !
      INTEGER :: ji, jj  ! dummy loop arguments
      !!--------------------------------------------------------------------
      !
      p_fval(:) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         p_fval(jj) = p_fval(jj) + pvflx(ji,jj) * pmsk(ji,jj) * tmask_i(ji,jj)
      END_2D
   END SUBROUTINE ptr_sj_2d_t


   FUNCTION ptr_ci_2d( pva )   RESULT ( p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_ci_2d  ***
      !!
      !! ** Purpose :   "meridional" cumulated sum computation of a j-flux array
      !!
      !! ** Method  : - j cumulated sum of pva using the interior 2D vmask (umask_i).
      !!
      !! ** Action  : - p_fval: j-cumulated sum of pva
      !!----------------------------------------------------------------------
      REAL(wp) , INTENT(in), DIMENSION(T2D(0))  ::   pva   ! mask flux array at V-point
      !
      INTEGER                     ::   ji,jj,jc  ! dummy loop arguments
      INTEGER                     ::   ijpj      ! ???
      REAL(wp), DIMENSION(T2D(0)) :: p_fval      ! function value
      !!--------------------------------------------------------------------
      !
      p_fval(:,:) = 0._wp
      DO jc = 1, jpnj ! looping over all processors in j axis
         DO_2D( 0, 0, 0, 0 )
            p_fval(ji,jj) = p_fval(ji,jj-1) + pva(ji,jj) * tmask_i(ji,jj)
         END_2D
      END DO
      !
   END FUNCTION ptr_ci_2d


   FUNCTION ptr_sjk( pta, pmsk )   RESULT ( p_fval )
      !!
      REAL(wp) , INTENT(in), DIMENSION(:,:,:)       ::   pta    ! mask flux array at V-point
      REAL(wp) , INTENT(in), DIMENSION(A2D(nn_hls)) ::   pmsk   ! Optional 2D basin mask
      !
      REAL(wp), DIMENSION(T1Dj(0),jpk) :: p_fval            ! i-sum of masked field
      !!
      CALL ptr_sjk_t( pta(:,:,:), lbnd_ij(pta), pmsk(:,:), p_fval(:,:) )
   END FUNCTION ptr_sjk


   SUBROUTINE ptr_sjk_t( pta, ktta, pmsk, p_fval )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE ptr_sjk  ***
      !!
      !! ** Purpose :   i-sum computation of an array
      !!
      !! ** Method  : - i-sum of field using the interior 2D vmask (pmsk).
      !!
      !! ** Action  : - p_fval: i-sum of masked field
      !!----------------------------------------------------------------------
      INTEGER,  INTENT(in   ), DIMENSION(2)              ::   ktta
      REAL(wp), INTENT(in   ), DIMENSION(AB2D(ktta),JPK) ::   pta    ! mask flux array at V-point
      REAL(wp), INTENT(in   ), DIMENSION(A2D(nn_hls))    ::   pmsk   ! Optional 2D basin mask
      REAL(wp), INTENT(  out), DIMENSION(T1Dj(0),jpk)    ::   p_fval ! i-sum of masked field
      !
      INTEGER :: ji, jj, jk  ! dummy loop arguments
      !!--------------------------------------------------------------------
      !
      p_fval(:,:) = 0._wp
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         p_fval(jj,jk) = p_fval(jj,jk) + pta(ji,jj,jk) * pmsk(ji,jj) * tmask_i(ji,jj)
      END_3D
   END SUBROUTINE ptr_sjk_t


   !!======================================================================
END MODULE diaptr
