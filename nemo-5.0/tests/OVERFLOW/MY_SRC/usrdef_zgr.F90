MODULE usrdef_zgr
   !!======================================================================
   !!                     ***  MODULE usrdef_zgr  ***
   !!
   !!                       ===  OVERFLOW case  ===
   !!
   !! user defined :  vertical coordinate system of a user configuration
   !!======================================================================
   !! History :  4.0  ! 2016-08  (G. Madec, S. Flavoni)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_zgr   : user defined vertical coordinate system (required)
   !!       zgr_z1d   : reference 1D z-coordinate 
   !!---------------------------------------------------------------------
   USE oce            ! ocean variables
   USE dom_oce        ! ocean space and time domain
   USE usrdef_nam     ! User defined : namelist variables
   !
   USE in_out_manager ! I/O manager
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! distributed memory computing library
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_zgr   ! called by domzgr.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS             

   SUBROUTINE usr_def_zgr( ld_zco  , ld_zps  , ld_sco  , ld_isfcav,    &   ! type of vertical coordinate
      &                    k_top   , k_bot                        ,    &   ! top & bottom ocean level
      &                    pdept_1d, pdepw_1d, pe3t_1d , pe3w_1d  ,    &   ! 1D reference vertical coordinate
      &                    pe3t  , pe3u  , pe3v   , pe3f ,             &   ! vertical scale factors
      &                    pdept , pdepw ,                             &   ! 3D t & w-points depth
      &                    pe3w  , pe3uw , pe3vw                       )   ! vertical scale factors
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE usr_def_zgr  ***
      !!
      !! ** Purpose :   User defined the vertical coordinates
      !!
      !!----------------------------------------------------------------------
      LOGICAL                   , INTENT(out) ::   ld_zco, ld_zps, ld_sco      ! vertical coordinate flags
      LOGICAL                   , INTENT(out) ::   ld_isfcav                   ! under iceshelf cavity flag
      INTEGER , DIMENSION(:,:)  , INTENT(out) ::   k_top, k_bot                ! first & last ocean level
      REAL(wp), DIMENSION(:)    , INTENT(out) ::   pdept_1d, pdepw_1d          ! 1D grid-point depth     [m]
      REAL(wp), DIMENSION(:)    , INTENT(out) ::   pe3t_1d , pe3w_1d           ! 1D grid-point depth     [m]
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, INTENT(out) ::   pdept, pdepw                ! grid-point depth        [m]
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, INTENT(out) ::   pe3t , pe3u , pe3v , pe3f   ! vertical scale factors  [m]
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, INTENT(out) ::   pe3w , pe3uw, pe3vw         ! i-scale factors 
      !
      INTEGER  ::   ji, jj, jk        ! dummy indices
      INTEGER  ::   ik                ! local integers
      REAL(wp) ::   zfact, z1_jpkm1   ! local scalar
      REAL(wp) ::   ze3min            ! local scalar
      REAL(wp), DIMENSION(jpi,jpj) ::   zht, zhu, z2d   ! 2D workspace
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_zgr : OVERFLOW configuration (z(ps)- or s-coordinate closed box ocean without cavities)'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      !
      !
      ! type of vertical coordinate
      ! ---------------------------
      ! already set in usrdef_nam.F90 by reading the namusr_def namelist except for ISF
      ld_isfcav = .FALSE.
      ld_zco  = .FALSE.
      ld_zps  = .FALSE.
      ld_sco  = .FALSE.
      SELECT CASE( nn_COORD )
      CASE( 0 )
         ld_zco  = .TRUE.
      CASE( 1 )
         ld_zps  = .TRUE.
      CASE( 2 )
         ld_sco  = .TRUE.
      END SELECT
      !
      ! Build the vertical coordinate system
      ! ------------------------------------
      !
      !                       !==  UNmasked meter bathymetry  ==!
      !
      ! western continental shelf (500m deep) and eastern deep ocean (2000m deep)
      ! (set through the jpk and jpi (see userdef_nam.F90))
      ! with a hyperbolic tangent transition centered at 40km
      ! NB: here glamt is in kilometers
      !
      zht(:,:) = + (  500. + 0.5 * 1500. * ( 1.0 + tanh( (glamt(:,:) - 40.) / 7. ) )  )
      !
      ! at u-point: averaging zht
      DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls )
         zhu(ji,jj) = 0.5_wp * ( zht(ji,jj) + zht(ji+1,jj) )
      END_2D
      CALL lbc_lnk( 'usrdef_zgr', zhu, 'U', 1._wp )     ! boundary condition: this mask the surrouding grid-points
      !                                ! ==>>>  set by hand non-zero value on first/last columns & rows 
      !     
      CALL zgr_z1d( pdept_1d, pdepw_1d, pe3t_1d , pe3w_1d )   ! Reference z-coordinate system
      !
      !
      !                       !==  top masked level bathymetry  ==!  (all coordinates)
      !
      ! no ocean cavities : top ocean level is ONE, except over land
      ! the ocean basin surrounded by land (1+nn_hls grid-points) set through lbc_lnk call
      z2d(:,:) = 1._wp                    ! surface ocean is the 1st level
      CALL lbc_lnk( 'usrdef_zgr', z2d, 'T', 1._wp )        ! closed basin, see userdef_nam.F90
      k_top(:,:) = NINT( z2d(:,:) )
      !
      !                              
      !
      IF( lk_vco_3d ) THEN      !==  s-coordinate  ==!   (terrain-following coordinate)
         !
         IF( ld_sco ) THEN
            !
            k_bot(:,:) = jpkm1 * k_top(:,:)  !* bottom ocean = jpk-1 (here use k_top as a land mask)
            !
            !                                !* terrain-following coordinate with e3.(k)=cst)
            !                                !  OVERFLOW case : identical with j-index (T=V, U=F)
            z1_jpkm1 = 1._wp / REAL( jpkm1 , wp)
            DO jk = 1, jpk
               pdept(:,:,jk) = zht(:,:) * z1_jpkm1 * ( REAL( jk   , wp ) - 0.5_wp )
               pdepw(:,:,jk) = zht(:,:) * z1_jpkm1 * ( REAL( jk-1 , wp )          )
               pe3t (:,:,jk) = zht(:,:) * z1_jpkm1
               pe3u (:,:,jk) = zhu(:,:) * z1_jpkm1
               pe3v (:,:,jk) = zht(:,:) * z1_jpkm1
               pe3f (:,:,jk) = zhu(:,:) * z1_jpkm1
               pe3w (:,:,jk) = zht(:,:) * z1_jpkm1
               pe3uw(:,:,jk) = zhu(:,:) * z1_jpkm1
               pe3vw(:,:,jk) = zht(:,:) * z1_jpkm1
            END DO
            !
         ELSEIF( ld_zco ) THEN
            k_bot(:,:) = jpkm1 * k_top(:,:)     ! here use k_top as a land mask
            DO jk = 1, jpkm1
               WHERE( pdept_1d(jk) < zht(:,:) .AND. zht(:,:) <= pdept_1d(jk+1) )   k_bot(:,:) = jk * k_top(:,:)
            END DO
            !         !* horizontally uniform coordinate (reference z-co everywhere)
            DO jk = 1, jpk
               pdept(:,:,jk) = pdept_1d(jk)
               pdepw(:,:,jk) = pdepw_1d(jk)
               pe3t (:,:,jk) = pe3t_1d (jk)
               pe3u (:,:,jk) = pe3t_1d (jk)
               pe3v (:,:,jk) = pe3t_1d (jk)
               pe3f (:,:,jk) = pe3t_1d (jk)
               pe3w (:,:,jk) = pe3w_1d (jk)
               pe3uw(:,:,jk) = pe3w_1d (jk)
               pe3vw(:,:,jk) = pe3w_1d (jk)
            END DO
         ELSEIF( ld_zps ) THEN
            k_bot(:,:) = jpkm1 * k_top(:,:)     ! here use k_top as a land mask
            DO jk = 1, jpkm1
               WHERE( pdept_1d(jk) < zht(:,:) .AND. zht(:,:) <= pdept_1d(jk+1) )   k_bot(:,:) = jk * k_top(:,:)
            END DO
            !         !* horizontally uniform coordinate (reference z-co everywhere)
            DO jk = 1, jpk
               pdept(:,:,jk) = pdept_1d(jk)
               pdepw(:,:,jk) = pdepw_1d(jk)
               pe3w (:,:,jk) = pe3w_1d (jk)
               pe3uw(:,:,jk) = pe3w_1d (jk)
               pe3vw(:,:,jk) = pe3w_1d (jk)
            END DO
            !
            DO jk = 1, jpk                      ! initialization to the reference z-coordinate
               pe3t (:,:,jk) = pe3t_1d (jk)
               pe3u (:,:,jk) = pe3t_1d (jk)
               pe3v (:,:,jk) = pe3t_1d (jk)
               pe3f (:,:,jk) = pe3t_1d (jk)
            END DO
            DO_2D( 0, 0, 0, 0 )
               ik = k_bot(ji,jj)
               pe3t (ji,jj,ik  ) = MIN( zht(ji,jj) , pdepw_1d(ik+1) ) - pdepw_1d(ik)   ! last wet level thickness
               pe3t (ji,jj,ik+1) = pe3t (ji,jj,ik  )
            END_2D
            !                                   ! bottom scale factors and depth at  U-, V-, UW and VW-points
            !                                   ! usually Computed as the minimum of neighbooring scale factors
            pe3u (:,:,:) = pe3t(:,:,:)          ! HERE OVERFLOW configuration : 
            pe3v (:,:,:) = pe3t(:,:,:)          !    e3 increases with i-index and identical with j-index
            pe3f (:,:,:) = pe3t(:,:,:)          !    so e3 minimum of (i,i+1) points is (i) point in j-direction e3v=e3t and e3f=e3v
            !                                   !    ==>>  no need of lbc_lnk calls
         ENDIF
      ENDIF
      !
      !
      IF( lk_vco_1d ) THEN      !==  z-coordinate  ==!   (step-like topography)
         !
         !                                !* bottom ocean compute from the depth of grid-points
         k_bot(:,:) = jpkm1 * k_top(:,:)     ! here use k_top as a land mask
         DO jk = 1, jpkm1
            WHERE( pdept_1d(jk) < zht(:,:) .AND. zht(:,:) <= pdept_1d(jk+1) )   k_bot(:,:) = jk * k_top(:,:)
         END DO
      ENDIF
      !
      !
      IF( lk_vco_1d3d ) THEN    !==  zps-coordinate  ==!   (partial bottom-steps)
         !
         ze3min = 0.1_wp * rn_dz
         IF(lwp) WRITE(numout,*) '   minimum thickness of the partial cells = 10 % of e3 = ', ze3min
         !
         !
         !                                !* bottom ocean compute from the depth of grid-points
         k_bot(:,:) = jpkm1
         DO jk = jpkm1, 1, -1
            WHERE( zht(:,:) < pdepw_1d(jk) + ze3min )   k_bot(:,:) = jk-1
         END DO
         !
         IF( ld_zps ) THEN                !* vertical coordinate system
            DO jk = 1, jpk                      ! initialization to the reference z-coordinate
               pe3t (:,:,jk) = pe3t_1d (jk)
               pe3u (:,:,jk) = pe3t_1d (jk)
               pe3v (:,:,jk) = pe3t_1d (jk)
               pe3f (:,:,jk) = pe3t_1d (jk)
            END DO
            DO_2D( 0, 0, 0, 0 )
               ik = k_bot(ji,jj)                ! last wet level thickness
               pe3t (ji,jj,ik  ) = MIN( zht(ji,jj) , pdepw_1d(ik+1) ) - pdepw_1d(ik)   ! min(sum_e3,pdep_W(k_bot+1))-pdep_W(k_bot) 
               pe3t (ji,jj,ik+1) = pe3t (ji,jj,ik  ) 
            END_2D
            !                                   ! bottom scale factors and depth at  U-, V-, UW and VW-points
            !                                   ! usually Computed as the minimum of neighbooring scale factors
            pe3u (:,:,:) = pe3t(:,:,:)          ! HERE OVERFLOW configuration : 
            pe3v (:,:,:) = pe3t(:,:,:)          !    e3 increases with i-index and identical with j-index
            pe3f (:,:,:) = pe3t(:,:,:)          !    so e3 minimum of (i,i+1) points is (i) point in j-direction e3v=e3t and e3f=e3v
            !                                   !    ==>>  no need of lbc_lnk calls
         ELSEIF( ld_zco ) THEN
            DO jk = 1, jpk                      ! initialization to the reference z-coordinate
               pe3t (:,:,jk) = pe3t_1d (jk)
               pe3u (:,:,jk) = pe3t_1d (jk)
               pe3v (:,:,jk) = pe3t_1d (jk)
               pe3f (:,:,jk) = pe3t_1d (jk)
            END DO
         ENDIF
      ENDIF
      !
   END SUBROUTINE usr_def_zgr


   SUBROUTINE zgr_z1d( pdept_1d, pdepw_1d, pe3t_1d , pe3w_1d )   ! 1D reference vertical coordinate
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zgr_z1d  ***
      !!
      !! ** Purpose :   set the depth of model levels and the resulting 
      !!      vertical scale factors.
      !!
      !! ** Method  :   1D z-coordinate system (use in all type of coordinate)
      !!       The depth of model levels is set from dep(k), an analytical function:
      !!                   w-level: depw_1d  = dep(k)
      !!                   t-level: dept_1d  = dep(k+0.5)
      !!       The scale factors are the discrete derivative of the depth:
      !!                   e3w_1d(jk) = dk[ dept_1d ] 
      !!                   e3t_1d(jk) = dk[ depw_1d ]
      !!
      !!            ===    Here constant vertical resolution   ===
      !!
      !! ** Action  : - pdept_1d, pdepw_1d : depth of T- and W-point (m)
      !!              - pe3t_1d , pe3w_1d  : scale factors at T- and W-levels (m)
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:), INTENT(out) ::   pdept_1d, pdepw_1d   ! 1D grid-point depth        [m]
      REAL(wp), DIMENSION(:), INTENT(out) ::   pe3t_1d , pe3w_1d    ! 1D vertical scale factors  [m]
      !
      INTEGER  ::   jk       ! dummy loop indices
      REAL(wp) ::   zt, zw   ! local scalar
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN                         ! Parameter print
         WRITE(numout,*)
         WRITE(numout,*) '    zgr_z1d : Reference vertical z-coordinates: uniform dz = ', rn_dz
         WRITE(numout,*) '    ~~~~~~~'
      ENDIF
      !
      ! Reference z-coordinate (depth - scale factor at T- and W-points)   ! Madec & Imbard 1996 function
      ! ----------------------
      DO jk = 1, jpk
         zw = REAL( jk , wp )
         zt = REAL( jk , wp ) + 0.5_wp
         pdepw_1d(jk) =    rn_dz *   REAL( jk-1 , wp )
         pdept_1d(jk) =    rn_dz * ( REAL( jk-1 , wp ) + 0.5_wp )
         pe3w_1d (jk) =    rn_dz
         pe3t_1d (jk) =    rn_dz
      END DO
      !
      IF(lwp) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*) '              Reference 1D z-coordinate depth and scale factors:'
         WRITE(numout, "(9x,' level  gdept_1d  gdepw_1d  e3t_1d   e3w_1d  ')" )
         WRITE(numout, "(10x, i4, 4f9.2)" ) ( jk, pdept_1d(jk), pdepw_1d(jk), pe3t_1d(jk), pe3w_1d(jk), jk = 1, jpk )
      ENDIF
      !
   END SUBROUTINE zgr_z1d
   
   !!======================================================================
END MODULE usrdef_zgr

