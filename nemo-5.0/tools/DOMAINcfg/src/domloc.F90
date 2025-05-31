MODULE domloc
   !!==============================================================================
   !!                       ***  MODULE domloc  ***
   !! Ocean domain : To localise a vertical coordinate system
   !!==============================================================================
   !! History :
   !!  NEMO      4.2  ! 2023-02  (D. Bruciaferri) Original code  
   !!----------------------------------------------------------------------
   
   !!----------------------------------------------------------------------
   !!   loc_zgr          : localise a vertical coordinate system 
   !!----------------------------------------------------------------------
   !!
   !
   USE dom_oce           ! ocean domain
   USE depth_e3          ! depth <=> e3
   !
   USE in_out_manager    ! I/O manager
   USE iom               ! I/O library
   USE lbclnk            ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp           ! distributed memory computing library
 
   IMPLICIT NONE
   PRIVATE
 
   PUBLIC   loc_zgr      ! called by dommes.F90
   PUBLIC   zgr_read     ! called by domzgr.F90
   !
   ! INPUT GLOBAL VERTICAL COORDINATE SYSTEM
   !
   LOGICAL                                 :: ln_zco_in                !: z-coordinate - full step
   LOGICAL                                 :: ln_zps_in                !: z-coordinate - partial step
   LOGICAL                                 :: ln_sco_in                !: s-coordinate or hybrid z-s coordinate
   LOGICAL                                 :: ln_isfcav_in             !: presence of ISF
   INTEGER , ALLOCATABLE, DIMENSION(:,:)   :: k_top_in, k_bot_in 
   REAL(wp), ALLOCATABLE, DIMENSION(:)     :: e3t_1d_in  , e3w_1d_in   !: ref. scale factors (m)
   REAL(wp), ALLOCATABLE, DIMENSION(:)     :: gdept_1d_in, gdepw_1d_in !: ref. depth (m)
   !
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: e3t_in, e3u_in , e3v_in , e3f_in !: scale factors [m]
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: e3w_in, e3uw_in, e3vw_in         !:   -      -
   ! 
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: gdept_in, gdepw_in               !: depths [m]
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
 
CONTAINS

   SUBROUTINE loc_zgr
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE zgr_loc  ***
      !!
      !! ** Purpose : Wrap the steps to generate a localised vertical 
      !!              coordinate systems
      !!----------------------------------------------------------------------
      INTEGER                :: ji, jj, jk     ! dummy loop index
      INTEGER, DIMENSION(4)  :: ierr
      !-----------------------------------------------------------------------
      !
      ! Allocating input grid arrays
      ALLOCATE(gdept_1d_in(jpk), gdepw_1d_in(jpk), e3t_1d_in (jpk), e3w_1d_in(jpk), STAT=ierr(1))
      ALLOCATE(k_top_in(jpi,jpj), k_bot_in(jpi,jpj), STAT=ierr(2))
      ALLOCATE(gdept_in(jpi,jpj,jpk), gdepw_in(jpi,jpj,jpk), e3t_in(jpi,jpj,jpk), e3w_in(jpi,jpj,jpk), STAT=ierr(3))
      ALLOCATE(e3u_in(jpi,jpj,jpk), e3v_in(jpi,jpj,jpk), e3f_in(jpi,jpj,jpk), e3uw_in(jpi,jpj,jpk), e3vw_in(jpi,jpj,jpk), STAT=ierr(4))
      IF( MAXVAL(ierr) /= 0 )   CALL ctl_stop( 'STOP', 'loc_zgr: unable to allocate standard ocean arrays' )
      !
      ! Reading the input vertical grid that will be used globally
      ! -----------------------------------------------------------------
      !
      CALL zgr_read( ln_zco_in  , ln_zps_in  , ln_sco_in, ln_isfcav_in, &
         &           gdept_1d_in, gdepw_1d_in, e3t_1d_in, e3w_1d_in   , &  ! 1D gridpoints depth
         &           gdept_in   , gdepw_in   ,                          &  ! gridpoints depth
         &           e3t_in     , e3u_in     , e3v_in   , e3f_in      , &  ! vertical scale factors
         &           e3w_in     , e3uw_in    , e3vw_in  ,               &  ! vertical scale factors
         &           k_top_in   , k_bot_in                              )
      !
      ! COMPUTE e3 as finite differences and depth of T-LEVES at cell centers
      ! ---------------------------------------------------------------------- 
      IF ( ln_e3_dep.AND.ln_dept_mid ) THEN
         ! Define depths at cell centers
         DO jk = 1, jpkm1
            gdept_1d_in(jk) = 0.5_wp * (gdepw_1d_in(jk)+gdepw_1d_in(jk+1))
         END DO
         DO jk = 2, jpk
            e3w_1d_in(jk) = gdept_1d_in(jk) - gdept_1d_in(jk-1)
         END DO
         e3w_1d_in(1  ) = 2._wp * (gdept_1d_in(1) - gdepw_1d_in(1))

         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )   
            gdept_in(ji,jj,jk) = 0.5_wp * ( gdepw_in(ji,jj,jk) +  gdepw_in(ji,jj,jk+1) )
         END_3D
         e3w_in(:,:,1) = e3t_in(:,:,1)
         e3uw_in(:,:,1) = e3u_in(:,:,1)
         e3vw_in(:,:,1) = e3v_in(:,:,1)
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 2, jpk )   
            e3w_in(ji,jj,jk) = 0.5_wp * (e3t_in(ji,jj,jk)+e3t_in(ji,jj,jk-1))
            e3uw_in(ji,jj,jk) = 0.5_wp * (e3u_in(ji,jj,jk)+e3u_in(ji,jj,jk-1))
            e3vw_in(ji,jj,jk) = 0.5_wp * (e3v_in(ji,jj,jk)+e3v_in(ji,jj,jk-1))
         END_3D
      ENDIF
      !
      ! Creating the local coordinate system within the global input vertical grid
      ! ---------------------------------------------------------------------------
      !      
      IF ( ln_mes ) CALL loc_zgr_mes 
      ! TO ADD CALL for ln_sco IN THE CASE OF JDHA szt
      !
      DEALLOCATE( gdept_1d_in, gdepw_1d_in, e3t_1d_in  , e3w_1d_in )
      DEALLOCATE( k_top_in, k_bot_in )
      DEALLOCATE( gdept_in, gdepw_in, e3t_in, e3w_in )
      DEALLOCATE( e3u_in  , e3v_in  , e3f_in, e3uw_in, e3vw_in )

   END SUBROUTINE loc_zgr
  
   SUBROUTINE zgr_read( ld_zco  , ld_zps  , ld_sco  , ld_isfcav,   &   ! type of vertical coordinate
      &                 pdept_1d, pdepw_1d, pe3t_1d , pe3w_1d  ,   &   ! 1D reference vertical coordinate
      &                 pdept , pdepw ,                            &   ! 3D t & w-points depth
      &                 pe3t  , pe3u  , pe3v   , pe3f ,            &   ! vertical scale factors
      &                 pe3w  , pe3uw , pe3vw         ,            &   !     -      -      -
      &                 k_top  , k_bot    )                            ! top & bottom ocean level
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE zgr_read  ***
      !!
      !! ** Purpose :   Read the vertical information in the domain configuration file
      !!
      !!----------------------------------------------------------------------
      LOGICAL                   , INTENT(out) ::   ld_zco, ld_zps, ld_sco      ! vertical coordinate flags
      LOGICAL                   , INTENT(out) ::   ld_isfcav                   ! under iceshelf cavity flag
      REAL(wp), DIMENSION(:)    , INTENT(out) ::   pdept_1d, pdepw_1d          ! 1D grid-point depth       [m]
      REAL(wp), DIMENSION(:)    , INTENT(out) ::   pe3t_1d , pe3w_1d           ! 1D vertical scale factors [m]
      REAL(wp), DIMENSION(:,:,:), INTENT(out) ::   pdept, pdepw                ! grid-point depth          [m]
      REAL(wp), DIMENSION(:,:,:), INTENT(out) ::   pe3t , pe3u , pe3v , pe3f   ! vertical scale factors    [m]
      REAL(wp), DIMENSION(:,:,:), INTENT(out) ::   pe3w , pe3uw, pe3vw         !    -       -      -
      INTEGER , DIMENSION(:,:)  , INTENT(out) ::   k_top , k_bot               ! first & last ocean level
      !
      INTEGER  ::   jk     ! dummy loop index
      INTEGER  ::   inum   ! local logical unit
      REAL(WP) ::   z_zco, z_zps, z_sco, z_cav
      REAL(wp), DIMENSION(jpi,jpj) ::   z2d   ! 2D workspace
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   zgr_read : read the vertical coordinates in ', TRIM( cn_domcfg ), ' file'
         WRITE(numout,*) '   ~~~~~~~~'
      ENDIF
      !
      CALL iom_open( cn_domcfg, inum )
      !
      !                          !* type of vertical coordinate
      CALL iom_get( inum, 'ln_zco'   , z_zco )
      CALL iom_get( inum, 'ln_zps'   , z_zps )
      CALL iom_get( inum, 'ln_sco'   , z_sco )
      IF( z_zco == 0._wp ) THEN   ;   ld_zco = .false.   ;   ELSE   ;   ld_zco = .true.   ;   ENDIF
      IF( z_zps == 0._wp ) THEN   ;   ld_zps = .false.   ;   ELSE   ;   ld_zps = .true.   ;   ENDIF
      IF( z_sco == 0._wp ) THEN   ;   ld_sco = .false.   ;   ELSE   ;   ld_sco = .true.   ;   ENDIF  
      !
      !                          !* ocean cavities under iceshelves
      CALL iom_get( inum, 'ln_isfcav', z_cav )
      IF( z_cav == 0._wp ) THEN   ;   ld_isfcav = .false.   ;   ELSE   ;   ld_isfcav = .true.   ;   ENDIF
      !
      !                          !* vertical scale factors
      CALL iom_get( inum, jpdom_unknown, 'e3t_1d'  , pe3t_1d  )                     ! 1D reference coordinate
      CALL iom_get( inum, jpdom_unknown, 'e3w_1d'  , pe3w_1d  )
      !
      CALL iom_get( inum, jpdom_global, 'e3t_0'  , pe3t , cd_type = 'T', psgn = 1._wp, kfill = jpfillcopy )    ! 3D coordinate
      CALL iom_get( inum, jpdom_global, 'e3u_0'  , pe3u , cd_type = 'U', psgn = 1._wp, kfill = jpfillcopy )
      CALL iom_get( inum, jpdom_global, 'e3v_0'  , pe3v , cd_type = 'V', psgn = 1._wp, kfill = jpfillcopy )
      CALL iom_get( inum, jpdom_global, 'e3f_0'  , pe3f , cd_type = 'F', psgn = 1._wp, kfill = jpfillcopy )
      CALL iom_get( inum, jpdom_global, 'e3w_0'  , pe3w , cd_type = 'W', psgn = 1._wp, kfill = jpfillcopy )
      CALL iom_get( inum, jpdom_global, 'e3uw_0' , pe3uw, cd_type = 'U', psgn = 1._wp, kfill = jpfillcopy )
      CALL iom_get( inum, jpdom_global, 'e3vw_0' , pe3vw, cd_type = 'V', psgn = 1._wp, kfill = jpfillcopy )
      !
      !                          !* depths
      !                                   !- old depth definition (obsolescent feature)
      IF(  iom_varid( inum, 'gdept_1d', ldstop = .FALSE. ) > 0  .AND.  &
         & iom_varid( inum, 'gdepw_1d', ldstop = .FALSE. ) > 0  .AND.  &
         & iom_varid( inum, 'gdept_0' , ldstop = .FALSE. ) > 0  .AND.  &
         & iom_varid( inum, 'gdepw_0' , ldstop = .FALSE. ) > 0    ) THEN
         CALL ctl_warn( 'zgr_read : old definition of depths and scale factors used ', & 
            &           '           depths at t- and w-points read in the domain configuration file')
         CALL iom_get( inum, jpdom_unknown, 'gdept_1d', pdept_1d )   
         CALL iom_get( inum, jpdom_unknown, 'gdepw_1d', pdepw_1d )
         CALL iom_get( inum, jpdom_global , 'gdept_0' , pdept, kfill = jpfillcopy )
         CALL iom_get( inum, jpdom_global , 'gdepw_0' , pdepw, kfill = jpfillcopy )
         !
      ELSE                                !- depths computed from e3. scale factors
         CALL e3_to_depth( pe3t_1d, pe3w_1d, pdept_1d, pdepw_1d )    ! 1D reference depth
         CALL e3_to_depth( pe3t   , pe3w   , pdept   , pdepw    )    ! 3D depths
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) '              Reference 1D z-coordinate depth and scale factors:'
            WRITE(numout, "(9x,' level  gdept_1d  gdepw_1d  e3t_1d   e3w_1d  ')" )
            WRITE(numout, "(10x, i4, 4f9.2)" ) ( jk, pdept_1d(jk), pdepw_1d(jk), pe3t_1d(jk), pe3w_1d(jk), jk = 1, jpk )
         ENDIF
      ENDIF
      !
      !                          !* ocean top and bottom level
      CALL iom_get( inum, jpdom_global, 'top_level'    , z2d )   ! 1st wet T-points (ISF)
      k_top(:,:) = NINT( z2d(:,:) )
      CALL iom_get( inum, jpdom_global, 'bottom_level' , z2d )   ! last wet T-points
      k_bot(:,:) = NINT( z2d(:,:) )
      !
      ! reference depth for negative bathy (wetting and drying only)
      ! IF( ll_wd )  CALL iom_get( inum,  'rn_wd_ref_depth' , ssh_ref   )
      !
      CALL iom_close( inum )
      !
   END SUBROUTINE zgr_read

   SUBROUTINE loc_zgr_mes
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE loc_zgr_mes  ***
      !!
      !! ** Purpose :   Create a local MEs grid within a gloabal grid 
      !!                using different vertical coordinates.
      !!
      !!----------------------------------------------------------------------
      INTEGER                      ::   ji, jj, jk ! dummy loop index
      INTEGER                      ::   inum       ! local logical unit
      REAL                         ::   zwrk
      REAL(wp), DIMENSION(jpi,jpj) ::   l2g_wgt    ! weigths for computing model
                                                   ! levels in transition area
      !!----------------------------------------------------------------------
 
      IF(lwp) THEN
        WRITE(numout,*)
        WRITE(numout,*) '   loc_zgr_mes: localising ME s-coordinates'
        WRITE(numout,*) '   ~~~~~~~~'
      ENDIF
      !
      CALL iom_open( 'bathy_meter.nc', inum )
      CALL iom_get( inum, jpdom_data, 's2z_msk', l2g_msk)
      CALL iom_get( inum, jpdom_data, 's2z_wgt', l2g_wgt)
 
      ! The interpolation routine DOM/dtatsd.F90 assumes that when using ln_sco
      ! the initial T/S data are defined on the e3t_1d/e3w_1d grid   
      e3t_1d(:) = e3t_1d_in(:)
      e3w_1d(:) = e3w_1d_in(:)

      DO jj = 1,jpj
         DO ji = 1,jpi
            SELECT CASE (INT(l2g_msk(ji,jj)))
              CASE (0) ! global zps area
                   mbathy (ji,jj  ) = k_bot_in(ji,jj)
                   gdept_0(ji,jj,:) = gdept_in(ji,jj,:)
                   gdepw_0(ji,jj,:) = gdepw_in(ji,jj,:)
                   e3t_0  (ji,jj,:) = e3t_in (ji,jj,:)
                   e3w_0  (ji,jj,:) = e3w_in (ji,jj,:)
                   e3u_0  (ji,jj,:) = e3u_in (ji,jj,:)
                   e3v_0  (ji,jj,:) = e3v_in (ji,jj,:)
                   e3f_0  (ji,jj,:) = e3f_in (ji,jj,:)
                   e3uw_0 (ji,jj,:) = e3uw_in (ji,jj,:)
                   e3vw_0 (ji,jj,:) = e3vw_in (ji,jj,:)
              CASE (1) ! MEs to zps transition area 
                   gdept_0(ji,jj,:) =           l2g_wgt(ji,jj)   * gdept_0(ji,jj,:) + &
                     &                ( 1._wp - l2g_wgt(ji,jj) ) * gdept_in(ji,jj,:)
                   gdepw_0(ji,jj,:) =           l2g_wgt(ji,jj)   * gdepw_0(ji,jj,:) + &
                     &                ( 1._wp - l2g_wgt(ji,jj) ) * gdepw_in(ji,jj,:)
              CASE (2) ! MEs area
                   CYCLE     
            END SELECT
         END DO
      END DO
      !
      ! e3t, e3w for transition zone
      ! as finite differences
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( l2g_msk(ji,jj) == 1._wp ) THEN
               DO jk = 1,jpkm1
                  e3t_0(ji,jj,jk)   = gdepw_0(ji,jj,jk+1) - gdepw_0(ji,jj,jk)
                  e3w_0(ji,jj,jk+1) = gdept_0(ji,jj,jk+1) - gdept_0(ji,jj,jk)
               ENDDO
               ! Surface
               jk = 1
               e3w_0(ji,jj,jk) = 2.0_wp * (gdept_0(ji,jj,1) - gdepw_0(ji,jj,1))
               !
               ! Bottom
               jk = jpk
               e3t_0(ji,jj,jk) = 2.0_wp * (gdept_0(ji,jj,jk) - gdepw_0(ji,jj,jk))
            END IF
         END DO
      END DO
      !
      ! MBATHY transition zone
      DO jj = 1, jpj
         DO ji = 1, jpi
            IF ( l2g_msk(ji,jj) == 1._wp ) THEN
               DO jk = 1, jpkm1
                  IF( bathy(ji,jj) >= gdept_0(ji,jj,jk) ) mbathy(ji,jj) = MAX( 2, jk )
               END DO
            END IF
         END DO
      END DO
      !
      WHERE (bathy(:,:)<=0) mbathy(:,:) = 0
      !
      ! Computing e3u_0, e3v_0, e3f_0, e3uw_0, e3vw_0 
      ! for transition zone
      !
      DO jj = 1, jpjm1
         DO ji = 1, jpim1
            IF ( l2g_msk(ji,jj) == 1._wp ) THEN
               DO jk = 1, jpk

                  zwrk = MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji+1,jj)))
                  e3u_0(ji,jj,jk)=(MIN(1,mbathy(ji  ,jj))*e3t_0(ji  ,jj,jk)    +  &
                                   MIN(1,mbathy(ji+1,jj))*e3t_0(ji+1,jj,jk)) / zwrk
 
                  zwrk = MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji,jj+1)))                 
                  e3v_0(ji,jj,jk)=(MIN(1,mbathy(ji,jj  ))*e3t_0(ji,jj  ,jk)    +   &
                                   MIN(1,mbathy(ji,jj+1))*e3t_0(ji,jj+1,jk)) / zwrk
 
                  zwrk = MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji+1,jj)))                 
                  e3uw_0(ji,jj,jk)=(MIN(1,mbathy(ji  ,jj))*e3w_0(ji  ,jj,jk)   +   &
                                    MIN(1,mbathy(ji+1,jj))*e3w_0(ji+1,jj,jk)) / zwrk
 
                  zwrk = MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji,jj+1)))                 
                  e3vw_0(ji,jj,jk)=(MIN(1,mbathy(ji,jj  ))*e3w_0(ji,jj  ,jk)   +   &
                                    MIN(1,mbathy(ji,jj+1))*e3w_0(ji,jj+1,jk)) / zwrk

                  zwrk = MAX(1, MIN(1,mbathy(ji  ,jj))+MIN(1,mbathy(ji  ,jj+1))    &
                     &   +      MIN(1,mbathy(ji+1,jj))+MIN(1,mbathy(ji+1,jj+1)))                  
                  e3f_0(ji,jj,jk)=(MIN(1,mbathy(ji  ,jj  ))*e3t_0(ji  ,jj  ,jk)  + &
                     &             MIN(1,mbathy(ji+1,jj  ))*e3t_0(ji+1,jj  ,jk)  + &
                     &             MIN(1,mbathy(ji+1,jj+1))*e3t_0(ji+1,jj+1,jk)  + &
                     &             MIN(1,mbathy(ji  ,jj+1))*e3t_0(ji  ,jj+1,jk)) / &
                     &             zwrk
                     
               END DO
            END IF
         END DO
      END DO
 
      WHERE (e3t_0   (:,:,:) == 0.0)  e3t_0(:,:,:) = 1.0
      WHERE (e3u_0   (:,:,:) == 0.0)  e3u_0(:,:,:) = 1.0
      WHERE (e3v_0   (:,:,:) == 0.0)  e3v_0(:,:,:) = 1.0
      WHERE (e3f_0   (:,:,:) == 0.0)  e3f_0(:,:,:) = 1.0
      WHERE (e3w_0   (:,:,:) == 0.0)  e3w_0(:,:,:) = 1.0
      WHERE (e3uw_0  (:,:,:) == 0.0)  e3uw_0(:,:,:) = 1.0
      WHERE (e3vw_0  (:,:,:) == 0.0)  e3vw_0(:,:,:) = 1.0

   END SUBROUTINE loc_zgr_mes
 
END MODULE domloc
 
