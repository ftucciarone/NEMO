MODULE iceitd
   !!======================================================================
   !!                       ***  MODULE iceitd ***
   !!   sea-ice : ice thickness distribution
   !!======================================================================
   !! History :  3.0  !  2005-12  (M. Vancoppenolle) original code (based on CICE)
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_itd_rem   : redistribute ice thicknesses after thermo growth and melt
   !!   itd_glinear   : build g(h) satisfying area and volume constraints
   !!   itd_shiftice  : shift ice across category boundaries, conserving everything
   !!   ice_itd_reb   : rebin ice thicknesses into bounded categories
   !!   ice_itd_init  : read ice thicknesses mean and min from namelist
   !!----------------------------------------------------------------------
   USE par_ice        ! SI3 parameters
   USE phycst         ! physical constants
   USE ice1D          ! sea-ice: thermodynamic variables
   USE ice            ! sea-ice: variables
   USE icevar  , ONLY : ice_var_roundoff
   USE icectl         ! sea-ice: conservation tests
   USE icetab         ! sea-ice: convert 1D<=>2D
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_itd_init  ! called in icestp
   PUBLIC   ice_itd_rem   ! called in icethd
   PUBLIC   ice_itd_reb   ! called in icecor

   INTEGER            ::   nice_catbnd     ! choice of the type of ice category function
   !                                       ! associated indices:
   INTEGER, PARAMETER ::   np_cathfn = 1   ! categories defined by a function
   INTEGER, PARAMETER ::   np_catusr = 2   ! categories defined by the user
   !
   !                                           !! ** namelist (namitd) **
   LOGICAL                    ::   ln_cat_hfn   ! ice categories are defined by function like rn_himean**(-0.05)
   REAL(wp)                   ::   rn_himean    ! mean thickness of the domain
   LOGICAL                    ::   ln_cat_usr   ! ice categories are defined by rn_catbnd
   REAL(wp), DIMENSION(0:100) ::   rn_catbnd    ! ice categories bounds
   REAL(wp)                   ::   rn_himax     ! maximum ice thickness allowed
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_itd_rem( kt )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_itd_rem ***
      !!
      !! ** Purpose :   computes the redistribution of ice thickness
      !!                after thermodynamic growth of ice thickness
      !!
      !! ** Method  :   Linear remapping
      !!
      !! References :   W.H. Lipscomb, JGR 2001
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   kt      ! Ocean time step
      !
      INTEGER  ::   ii, ji, jj, jl, jcat ! dummy loop index
      INTEGER  ::   ipti                 ! local integer
      REAL(wp) ::   zx1, zwk1, zdh0, zetamin, zdamax   ! local scalars
      REAL(wp) ::   zx2, zwk2, zda0, zetamax           !   -      -
      REAL(wp) ::   zx3
      REAL(wp) ::   zslope          ! used to compute local thermodynamic "speeds"
      !
      INTEGER , DIMENSION(jpij)       ::   iptidx          ! compute remapping or not
      INTEGER , DIMENSION(jpij,jpl-1) ::   jdonor          ! donor category index
      REAL(wp), DIMENSION(jpij,jpl)   ::   zdhice          ! ice thickness increment
      REAL(wp), DIMENSION(jpij,jpl)   ::   g0, g1          ! coefficients for fitting the line of the ITD
      REAL(wp), DIMENSION(jpij,jpl)   ::   hL, hR          ! left and right boundary for the ITD for each thickness
      REAL(wp), DIMENSION(jpij,jpl-1) ::   zdaice, zdvice  ! local increment of ice area and volume
      REAL(wp), DIMENSION(jpij)       ::   zhb0, zhb1      ! category boundaries for thinnes categories
      REAL(wp), DIMENSION(jpij,0:jpl) ::   zhbnew          ! new boundaries of ice categories
      !!------------------------------------------------------------------
      IF( ln_timing )   CALL timing_start('iceitd_rem')

      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_itd_rem: remapping ice thickness distribution'

      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'iceitd_rem', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (0, 'iceitd_rem',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)

      !-----------------------------------------------------------------------------------------------
      !  1) Identify grid cells with ice
      !-----------------------------------------------------------------------------------------------
      at_i(A2D(0)) = SUM( a_i(A2D(0),:), dim=3 )
      !
#if defined key_si3_1D
      DO_2D( 0, 0, 0, 0 )
         npti = 0   ;   nptidx(:) = 0
         IF ( at_i(ji,jj) > epsi10 ) THEN
            npti = 1
            nptidx( npti ) = (jj - 1) * jpi + ji
         ENDIF
#else
      npti = 0   ;   nptidx(:) = 0
      DO_2D( 0, 0, 0, 0 )
         IF ( at_i(ji,jj) > epsi10 ) THEN
            npti = npti + 1
            nptidx( npti ) = (jj - 1) * jpi + ji
         ENDIF
      END_2D
#endif
      !-----------------------------------------------------------------------------------------------
      !  2) Compute new category boundaries
      !-----------------------------------------------------------------------------------------------
      IF( npti > 0 ) THEN
         !
         zdhice(:,:) = 0._wp
         zhbnew(:,:) = 0._wp
         !
         CALL tab_3d_2d( npti, nptidx(1:npti), h_i_2d (1:npti,:), h_i   )
         CALL tab_3d_2d( npti, nptidx(1:npti), h_ib_2d(1:npti,:), h_i_b )
         CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d (1:npti,:), a_i   )
         CALL tab_3d_2d( npti, nptidx(1:npti), a_ib_2d(1:npti,:), a_i_b )
         !
         DO jl = 1, jpl
            ! Compute thickness change in each ice category
            DO ii = 1, npti
               IF( a_i_2d(ii,jl) > epsi10 )   zdhice(ii,jl) = h_i_2d(ii,jl) - h_ib_2d(ii,jl)
            END DO
         END DO
         !
         ! --- New boundaries for category 1:jpl-1 --- !
         DO jl = 1, jpl - 1
            !
            DO ii = 1, npti
               !
               ! --- New boundary: Hn* = Hn + Fn*dt --- !
               !     Fn*dt = ( fn + (fn+1 - fn)/(hn+1 - hn) * (Hn - hn) ) * dt = zdhice + zslope * (Hmax - h_i_b)
               !
               IF    ( a_ib_2d(ii,jl) >  epsi10 .AND. a_ib_2d(ii,jl+1) >  epsi10 ) THEN   ! a(jl+1) & a(jl) /= 0
                  zslope        = ( zdhice(ii,jl+1) - zdhice(ii,jl) ) / ( h_ib_2d(ii,jl+1) - h_ib_2d(ii,jl) )
                  zhbnew(ii,jl) = hi_max(jl) + zdhice(ii,jl) + zslope * ( hi_max(jl) - h_ib_2d(ii,jl) )
               ELSEIF( a_ib_2d(ii,jl) >  epsi10 .AND. a_ib_2d(ii,jl+1) <= epsi10 ) THEN   ! a(jl+1)=0 => Hn* = Hn + fn*dt
                  zhbnew(ii,jl) = hi_max(jl) + zdhice(ii,jl)
               ELSEIF( a_ib_2d(ii,jl) <= epsi10 .AND. a_ib_2d(ii,jl+1) >  epsi10 ) THEN   ! a(jl)=0 => Hn* = Hn + fn+1*dt
                  zhbnew(ii,jl) = hi_max(jl) + zdhice(ii,jl+1)
               ELSE                                                                       ! a(jl+1) & a(jl) = 0
                  zhbnew(ii,jl) = hi_max(jl)
               ENDIF
               !
               ! --- 2 conditions for remapping --- !
               ! 1) hn(t+1)+espi < Hn* < hn+1(t+1)-epsi
               !    Note: hn(t+1) must not be too close to either HR or HL otherwise a division by nearly 0 is possible
               !          in itd_glinear in the case (HR-HL) = 3(Hice - HL) or = 3(HR - Hice)
# if defined key_single
               IF( a_i_2d(ii,jl  ) > epsi10 .AND. h_i_2d(ii,jl  ) > ( zhbnew(ii,jl) - epsi06 ) )   nptidx(ii) = 0
               IF( a_i_2d(ii,jl+1) > epsi10 .AND. h_i_2d(ii,jl+1) < ( zhbnew(ii,jl) + epsi06 ) )   nptidx(ii) = 0
# else
               IF( a_i_2d(ii,jl  ) > epsi10 .AND. h_i_2d(ii,jl  ) > ( zhbnew(ii,jl) - epsi10 ) )   nptidx(ii) = 0
               IF( a_i_2d(ii,jl+1) > epsi10 .AND. h_i_2d(ii,jl+1) < ( zhbnew(ii,jl) + epsi10 ) )   nptidx(ii) = 0
# endif
               !
               ! 2) Hn-1 < Hn* < Hn+1
               IF( zhbnew(ii,jl) < hi_max(jl-1) )   nptidx(ii) = 0
               IF( zhbnew(ii,jl) > hi_max(jl+1) )   nptidx(ii) = 0
               !
            END DO
         END DO
         !
         ! --- New boundaries for category jpl --- !
         DO ii = 1, npti
            IF( a_i_2d(ii,jpl) > epsi10 ) THEN
               zhbnew(ii,jpl) = MAX( hi_max(jpl-1), 3._wp * h_i_2d(ii,jpl) - 2._wp * zhbnew(ii,jpl-1) )
            ELSE
               zhbnew(ii,jpl) = hi_max(jpl)
            ENDIF
            !
            ! --- 1 additional condition for remapping (1st category) --- !
            ! H0+epsi < h1(t) < H1-epsi
            !    h1(t) must not be too close to either HR or HL otherwise a division by nearly 0 is possible
            !    in itd_glinear in the case (HR-HL) = 3(Hice - HL) or = 3(HR - Hice)
# if defined key_single
            IF( h_ib_2d(ii,1) < ( hi_max(0) + epsi06 ) )   nptidx(ii) = 0
            IF( h_ib_2d(ii,1) > ( hi_max(1) - epsi06 ) )   nptidx(ii) = 0
# else
            IF( h_ib_2d(ii,1) < ( hi_max(0) + epsi10 ) )   nptidx(ii) = 0
            IF( h_ib_2d(ii,1) > ( hi_max(1) - epsi10 ) )   nptidx(ii) = 0
# endif
         END DO
         !
         !-----------------------------------------------------------------------------------------------
         !  3) Identify cells where remapping
         !-----------------------------------------------------------------------------------------------
         ipti = 0   ;   iptidx(:) = 0
         DO ii = 1, npti
            IF( nptidx(ii) /= 0 ) THEN
               ipti = ipti + 1
               iptidx(ipti)   = nptidx(ii)
               zhbnew(ipti,:) = zhbnew(ii,:)  ! adjust zhbnew to new indices
            ENDIF
         END DO
         nptidx(:) = iptidx(:)
         npti      = ipti
         !
      ENDIF

      !-----------------------------------------------------------------------------------------------
      !  4) Compute g(h)
      !-----------------------------------------------------------------------------------------------
      IF( npti > 0 ) THEN
         !
         zhb0(:) = hi_max(0)   ;   zhb1(:) = hi_max(1)
         g0(:,:) = 0._wp       ;   g1(:,:) = 0._wp
         hL(:,:) = 0._wp       ;   hR(:,:) = 0._wp
         !
         DO jl = 1, jpl
            !
            CALL tab_2d_1d( npti, nptidx(1:npti), h_ib_1d(1:npti), h_i_b(:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), h_i_1d (1:npti), h_i  (:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d (1:npti), a_i  (:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), v_i_1d (1:npti), v_i  (:,:,jl) )
            !
            IF( jl == 1 ) THEN
               !
               ! --- g(h) for category 1 --- !
               CALL itd_glinear( zhb0(1:npti)  , zhb1(1:npti)  , h_ib_1d(1:npti)  , a_i_1d(1:npti)  ,  &   ! in
                  &              g0  (1:npti,1), g1  (1:npti,1), hL     (1:npti,1), hR    (1:npti,1)   )   ! out
               !
               ! Area lost due to melting of thin ice
               DO ii = 1, npti
                  !
                  IF( a_i_1d(ii) > epsi10 ) THEN
                     !
                     zdh0 =  h_i_1d(ii) - h_ib_1d(ii)
                     IF( zdh0 < 0.0 ) THEN      ! remove area from category 1
                        zdh0 = MIN( -zdh0, hi_max(1) )
                        !Integrate g(1) from 0 to dh0 to estimate area melted
                        zetamax = MIN( zdh0, hR(ii,1) ) - hL(ii,1)
                        !
                        IF( zetamax > 0.0 ) THEN
                           zx1    = zetamax
                           zx2    = 0.5 * zetamax * zetamax
                           zda0   = g1(ii,1) * zx2 + g0(ii,1) * zx1                ! ice area removed
                           zdamax = a_i_1d(ii) * (1.0 - h_i_1d(ii) / h_ib_1d(ii) ) ! Constrain new thickness <= h_i
                           zda0   = MIN( zda0, zdamax )                            ! ice area lost due to melting of thin ice (zdamax > 0)
                           ! Remove area, conserving volume
                           h_i_1d(ii) = h_i_1d(ii) * a_i_1d(ii) / ( a_i_1d(ii) - zda0 )
                           a_i_1d(ii) = a_i_1d(ii) - zda0
                           v_i_1d(ii) = a_i_1d(ii) * h_i_1d(ii) ! useless ?
                        ENDIF
                        !
                     ELSE ! if ice accretion zdh0 > 0
                        ! zhbnew was 0, and is shifted to the right to account for thin ice growth in openwater (F0 = f1)
                        zhbnew(ii,0) = MIN( zdh0, hi_max(1) )
                     ENDIF
                     !
                  ENDIF
                  !
               END DO
               !
               CALL tab_1d_2d( npti, nptidx(1:npti), h_i_1d(1:npti), h_i(:,:,jl) )
               CALL tab_1d_2d( npti, nptidx(1:npti), a_i_1d(1:npti), a_i(:,:,jl) )
               CALL tab_1d_2d( npti, nptidx(1:npti), v_i_1d(1:npti), v_i(:,:,jl) )
               !
            ENDIF ! jl=1
            !
            ! --- g(h) for each thickness category --- !
            CALL itd_glinear( zhbnew(1:npti,jl-1), zhbnew(1:npti,jl), h_i_1d(1:npti)   , a_i_1d(1:npti)   ,  &   ! in
               &              g0    (1:npti,jl  ), g1    (1:npti,jl), hL    (1:npti,jl), hR    (1:npti,jl)   )   ! out
            !
         END DO

         !-----------------------------------------------------------------------------------------------
         !  5) Compute area and volume to be shifted across each boundary (Eq. 18)
         !-----------------------------------------------------------------------------------------------
         DO jl = 1, jpl - 1
            !
            DO ii = 1, npti
               !
               ! left and right integration limits in eta space
               IF (zhbnew(ii,jl) > hi_max(jl)) THEN ! Hn* > Hn => transfer from jl to jl+1
                  zetamin = MAX( hi_max(jl)   , hL(ii,jl) ) - hL(ii,jl)   ! hi_max(jl) - hL
                  zetamax = MIN( zhbnew(ii,jl), hR(ii,jl) ) - hL(ii,jl)   ! hR - hL
                  jdonor(ii,jl) = jl
               ELSE                                 ! Hn* <= Hn => transfer from jl+1 to jl
                  zetamin = 0.0
                  zetamax = MIN( hi_max(jl), hR(ii,jl+1) ) - hL(ii,jl+1)  ! hi_max(jl) - hL
                  jdonor(ii,jl) = jl + 1
               ENDIF
               zetamax = MAX( zetamax, zetamin ) ! no transfer if etamax < etamin
               !
               zx1  = zetamax - zetamin
               zwk1 = zetamin * zetamin
               zwk2 = zetamax * zetamax
               zx2  = 0.5 * ( zwk2 - zwk1 )
               zwk1 = zwk1 * zetamin
               zwk2 = zwk2 * zetamax
               zx3  = 1.0 / 3.0 * ( zwk2 - zwk1 )
               jcat = jdonor(ii,jl)
               zdaice(ii,jl) = g1(ii,jcat)*zx2 + g0(ii,jcat)*zx1
               zdvice(ii,jl) = g1(ii,jcat)*zx3 + g0(ii,jcat)*zx2 + zdaice(ii,jl)*hL(ii,jcat)
               !
            END DO
         END DO

         !----------------------------------------------------------------------------------------------
         ! 6) Shift ice between categories
         !----------------------------------------------------------------------------------------------
         CALL itd_shiftice ( jdonor(1:npti,:), zdaice(1:npti,:), zdvice(1:npti,:) )
         !
      ENDIF
      !
#if defined key_si3_1D
      END_2D
#endif
      
      ! the following fields need to be updated in the halos (done afterwards):
      ! a_i, v_i, v_s, sv_i, oa_i, h_i, a_ip, v_ip, v_il, t_su, e_i, e_s
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'iceitd_rem', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (1, 'iceitd_rem',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)
      IF( ln_timing    )   CALL timing_stop ('iceitd_rem')
      !
   END SUBROUTINE ice_itd_rem


   SUBROUTINE itd_glinear( HbL, Hbr, phice, paice, pg0, pg1, phL, phR )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE itd_glinear ***
      !!
      !! ** Purpose :   build g(h) satisfying area and volume constraints (Eq. 6 and 9)
      !!
      !! ** Method  :   g(h) is linear and written as: g(eta) = g1(eta) + g0
      !!                with eta = h - HL
      !!------------------------------------------------------------------
      REAL(wp), DIMENSION(:), INTENT(in   ) ::   HbL, HbR      ! left and right category boundaries
      REAL(wp), DIMENSION(:), INTENT(in   ) ::   phice, paice  ! ice thickness and concentration
      REAL(wp), DIMENSION(:), INTENT(inout) ::   pg0, pg1      ! coefficients in linear equation for g(eta)
      REAL(wp), DIMENSION(:), INTENT(inout) ::   phL, phR      ! min and max value of range over which g(h) > 0
      !
      INTEGER  ::   ii           ! horizontal indices
      REAL(wp) ::   z1_3 , z2_3  ! 1/3 , 2/3
      REAL(wp) ::   zh13         ! HbL + 1/3 * (HbR - HbL)
      REAL(wp) ::   zh23         ! HbL + 2/3 * (HbR - HbL)
      REAL(wp) ::   zdhr         ! 1 / (hR - hL)
      REAL(wp) ::   zwk1, zwk2   ! temporary variables
      !!------------------------------------------------------------------
      !
      z1_3 = 1._wp / 3._wp
      z2_3 = 2._wp / 3._wp
      !
      DO ii = 1, npti
         !
         IF( paice(ii) > epsi10  .AND. phice(ii) > epsi10 )  THEN
            !
            ! Initialize hL and hR
            phL(ii) = HbL(ii)
            phR(ii) = HbR(ii)
            !
            ! Change hL or hR if hice falls outside central third of range,
            ! so that hice is in the central third of the range [HL HR]
            zh13 = z1_3 * ( 2._wp * phL(ii) +         phR(ii) )
            zh23 = z1_3 * (         phL(ii) + 2._wp * phR(ii) )
            !
            IF    ( phice(ii) < zh13 ) THEN   ;   phR(ii) = 3._wp * phice(ii) - 2._wp * phL(ii) ! move HR to the left
            ELSEIF( phice(ii) > zh23 ) THEN   ;   phL(ii) = 3._wp * phice(ii) - 2._wp * phR(ii) ! move HL to the right
            ENDIF
            !
            ! Compute coefficients of g(eta) = g0 + g1*eta
            IF( phR(ii) > phL(ii) ) THEN   ;   zdhr = 1._wp / (phR(ii) - phL(ii))
            ELSE                           ;   zdhr = 0._wp ! if hR=hL=hice => no remapping
            ENDIF
            !!zdhr = 1._wp / (phR(ii) - phL(ii))
            zwk1 = 6._wp * paice(ii) * zdhr
            zwk2 = ( phice(ii) - phL(ii) ) * zdhr
            pg0(ii) = zwk1 * ( z2_3 - zwk2 )                    ! Eq. 14
            pg1(ii) = 2._wp * zdhr * zwk1 * ( zwk2 - 0.5_wp )   ! Eq. 14
            !
         ELSE  ! remap_flag = .false. or a_i < epsi10
            phL(ii) = 0._wp
            phR(ii) = 0._wp
            pg0(ii) = 0._wp
            pg1(ii) = 0._wp
         ENDIF
         !
      END DO
      !
   END SUBROUTINE itd_glinear


   SUBROUTINE itd_shiftice( kdonor, pdaice, pdvice )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE itd_shiftice ***
      !!
      !! ** Purpose :   shift ice across category boundaries, conserving everything
      !!              ( area, volume, energy, age*vol, and mass of salt )
      !!------------------------------------------------------------------
      INTEGER , DIMENSION(:,:), INTENT(in) ::   kdonor   ! donor category index
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pdaice   ! ice area transferred across boundary
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   pdvice   ! ice volume transferred across boundary
      !
      INTEGER  ::   ii, jl, jk         ! dummy loop indices
      INTEGER  ::   jl2, jl1           ! local integers
      REAL(wp) ::   zworka, zworkv, ztrans ! ice/snow transferred
      REAL(wp), DIMENSION(jpij)            ::   ztmp             ! workspace
      REAL(wp), DIMENSION(jpij,jpl)        ::   zaTsfn           !  -    -
      !!------------------------------------------------------------------

      CALL tab_3d_2d( npti, nptidx(1:npti), h_i_2d  (1:npti,:)  , h_i   )
      CALL tab_3d_2d( npti, nptidx(1:npti), a_i_2d  (1:npti,:)  , a_i   )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_i_2d  (1:npti,:)  , v_i   )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_s_2d  (1:npti,:)  , v_s   )
      CALL tab_3d_2d( npti, nptidx(1:npti), oa_i_2d (1:npti,:)  , oa_i  )
      CALL tab_3d_2d( npti, nptidx(1:npti), sv_i_2d (1:npti,:)  , sv_i  )
      CALL tab_3d_2d( npti, nptidx(1:npti), a_ip_2d (1:npti,:)  , a_ip  )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_ip_2d (1:npti,:)  , v_ip  )
      CALL tab_3d_2d( npti, nptidx(1:npti), v_il_2d (1:npti,:)  , v_il  )
      CALL tab_3d_2d( npti, nptidx(1:npti), t_su_2d (1:npti,:)  , t_su  )
      CALL tab_4d_3d( npti, nptidx(1:npti), e_s_2d  (1:npti,:,:), e_s   )
      CALL tab_4d_3d( npti, nptidx(1:npti), e_i_2d  (1:npti,:,:), e_i   )
      CALL tab_4d_3d( npti, nptidx(1:npti), szv_i_2d(1:npti,:,:), szv_i )
      ! to correct roundoff errors on a_i
      CALL tab_2d_1d( npti, nptidx(1:npti), rn_amax_1d(1:npti), rn_amax_2d )

      !----------------------------------------------------------------------------------------------
      ! 1) Define a variable equal to a_i*T_su
      !----------------------------------------------------------------------------------------------
      DO jl = 1, jpl
         DO ii = 1, npti
            zaTsfn(ii,jl) = a_i_2d(ii,jl) * t_su_2d(ii,jl)
         END DO
      END DO

      !-------------------------------------------------------------------------------
      ! 2) Transfer volume and energy between categories
      !-------------------------------------------------------------------------------
      DO jl = 1, jpl - 1
         DO ii = 1, npti
            !
            jl1 = kdonor(ii,jl)
            !
            IF( jl1 > 0 ) THEN
               !
               IF ( jl1 == jl  ) THEN   ;   jl2 = jl1+1
               ELSE                     ;   jl2 = jl
               ENDIF
               !
               IF( v_i_2d(ii,jl1) >= epsi10 ) THEN   ;   zworkv = pdvice(ii,jl) / v_i_2d(ii,jl1)
               ELSE                                  ;   zworkv = 0._wp
               ENDIF
               IF( a_i_2d(ii,jl1) >= epsi10 ) THEN   ;   zworka = pdaice(ii,jl) / a_i_2d(ii,jl1)
               ELSE                                  ;   zworka = 0._wp
               ENDIF
               !
               a_i_2d(ii,jl1) = a_i_2d(ii,jl1) - pdaice(ii,jl)       ! Ice areas
               a_i_2d(ii,jl2) = a_i_2d(ii,jl2) + pdaice(ii,jl)
               !
               v_i_2d(ii,jl1) = v_i_2d(ii,jl1) - pdvice(ii,jl)       ! Ice volumes
               v_i_2d(ii,jl2) = v_i_2d(ii,jl2) + pdvice(ii,jl)
               !
               ztrans         = v_s_2d(ii,jl1) * zworkv              ! Snow volumes
               v_s_2d(ii,jl1) = v_s_2d(ii,jl1) - ztrans
               v_s_2d(ii,jl2) = v_s_2d(ii,jl2) + ztrans
               !
               ztrans          = oa_i_2d(ii,jl1) * zworka            ! Ice age
               oa_i_2d(ii,jl1) = oa_i_2d(ii,jl1) - ztrans
               oa_i_2d(ii,jl2) = oa_i_2d(ii,jl2) + ztrans
               !
               ztrans          = zaTsfn(ii,jl1) * zworka             ! Surface temperature
               zaTsfn(ii,jl1)  = zaTsfn(ii,jl1) - ztrans
               zaTsfn(ii,jl2)  = zaTsfn(ii,jl2) + ztrans
               !
               IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
                  ztrans          = a_ip_2d(ii,jl1) * zworka         ! Pond fraction
                  a_ip_2d(ii,jl1) = a_ip_2d(ii,jl1) - ztrans
                  a_ip_2d(ii,jl2) = a_ip_2d(ii,jl2) + ztrans
                  !
                  ztrans          = v_ip_2d(ii,jl1) * zworkv         ! Pond volume
                  v_ip_2d(ii,jl1) = v_ip_2d(ii,jl1) - ztrans
                  v_ip_2d(ii,jl2) = v_ip_2d(ii,jl2) + ztrans
                  !
                  IF ( ln_pnd_lids ) THEN                            ! Pond lid volume
                     ztrans          = v_il_2d(ii,jl1) * zworkv
                     v_il_2d(ii,jl1) = v_il_2d(ii,jl1) - ztrans
                     v_il_2d(ii,jl2) = v_il_2d(ii,jl2) + ztrans
                  ENDIF
               ENDIF
               !
               DO jk = 1, nlay_s                                     ! Snow heat content
                  ztrans            = e_s_2d(ii,jk,jl1) * zworkv
                  e_s_2d(ii,jk,jl1) = e_s_2d(ii,jk,jl1) - ztrans
                  e_s_2d(ii,jk,jl2) = e_s_2d(ii,jk,jl2) + ztrans
               ENDDO
               DO jk = 1, nlay_i                                     ! Ice heat content
                  ztrans            = e_i_2d(ii,jk,jl1) * zworkv
                  e_i_2d(ii,jk,jl1) = e_i_2d(ii,jk,jl1) - ztrans
                  e_i_2d(ii,jk,jl2) = e_i_2d(ii,jk,jl2) + ztrans
               ENDDO
               !                                                     ! Ice salinity
               IF( nn_icesal == 4 ) THEN
                  DO jk = 1, nlay_i
                     ztrans              = szv_i_2d(ii,jk,jl1) * zworkv
                     szv_i_2d(ii,jk,jl1) = szv_i_2d(ii,jk,jl1) - ztrans
                     szv_i_2d(ii,jk,jl2) = szv_i_2d(ii,jk,jl2) + ztrans
                  ENDDO
               ELSE
                  ztrans          = sv_i_2d(ii,jl1) * zworkv
                  sv_i_2d(ii,jl1) = sv_i_2d(ii,jl1) - ztrans
                  sv_i_2d(ii,jl2) = sv_i_2d(ii,jl2) + ztrans
               ENDIF
               !
            ENDIF   ! jl1 >0
         END DO
         !
      END DO                   ! boundaries, 1 to jpl-1

      !-------------------
      ! 3) roundoff errors
      !-------------------
      ! clem: The transfer between one category to another can lead to very small negative values (-1.e-20)
      !       because of truncation error ( i.e. 1. - 1. /= 0 )
      CALL ice_var_roundoff( a_i_2d, v_i_2d, v_s_2d, sv_i_2d, oa_i_2d, a_ip_2d, v_ip_2d, v_il_2d, e_s_2d, e_i_2d, szv_i_2d )

      ! at_i must be <= rn_amax
      ztmp(1:npti) = SUM( a_i_2d(1:npti,:), dim=2 )
      DO jl  = 1, jpl
         WHERE( ztmp(1:npti) > rn_amax_1d(1:npti) )   &
            &   a_i_2d(1:npti,jl) = a_i_2d(1:npti,jl) * rn_amax_1d(1:npti) / ztmp(1:npti)
      END DO

      !-------------------------------------------------------------------------------
      ! 4) Update ice thickness and temperature
      !-------------------------------------------------------------------------------
# if defined key_single
      WHERE( a_i_2d(1:npti,:) >= epsi06 )
# else
      WHERE( a_i_2d(1:npti,:) >= epsi20 )
# endif
         h_i_2d (1:npti,:)  =  v_i_2d(1:npti,:) / a_i_2d(1:npti,:)
         t_su_2d(1:npti,:)  =  zaTsfn(1:npti,:) / a_i_2d(1:npti,:)
      ELSEWHERE
         h_i_2d (1:npti,:)  = 0._wp
         t_su_2d(1:npti,:)  = rt0
      END WHERE
      !
      CALL tab_2d_3d( npti, nptidx(1:npti), h_i_2d  (1:npti,:)  , h_i   )
      CALL tab_2d_3d( npti, nptidx(1:npti), a_i_2d  (1:npti,:)  , a_i   )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_i_2d  (1:npti,:)  , v_i   )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_s_2d  (1:npti,:)  , v_s   )
      CALL tab_2d_3d( npti, nptidx(1:npti), oa_i_2d (1:npti,:)  , oa_i  )
      CALL tab_2d_3d( npti, nptidx(1:npti), sv_i_2d (1:npti,:)  , sv_i  )
      CALL tab_2d_3d( npti, nptidx(1:npti), a_ip_2d (1:npti,:)  , a_ip  )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_ip_2d (1:npti,:)  , v_ip  )
      CALL tab_2d_3d( npti, nptidx(1:npti), v_il_2d (1:npti,:)  , v_il  )
      CALL tab_2d_3d( npti, nptidx(1:npti), t_su_2d (1:npti,:)  , t_su  )
      CALL tab_3d_4d( npti, nptidx(1:npti), e_s_2d  (1:npti,:,:), e_s   )
      CALL tab_3d_4d( npti, nptidx(1:npti), e_i_2d  (1:npti,:,:), e_i   )
      CALL tab_3d_4d( npti, nptidx(1:npti), szv_i_2d(1:npti,:,:), szv_i )
      !
   END SUBROUTINE itd_shiftice


   SUBROUTINE ice_itd_reb( kt )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_itd_reb ***
      !!
      !! ** Purpose : rebin - rebins thicknesses into defined categories
      !!
      !! ** Method  : If a category thickness is out of bounds, shift part (for down to top)
      !!              or entire (for top to down) area, volume, and energy
      !!              to the neighboring category
      !!------------------------------------------------------------------
      INTEGER , INTENT (in) ::   kt      ! Ocean time step
      INTEGER ::   ii, ji, jj, jl   ! dummy loop indices
      !
      INTEGER , DIMENSION(jpij,jpl-1) ::   jdonor           ! donor category index
      REAL(wp), DIMENSION(jpij,jpl-1) ::   zdaice, zdvice   ! ice area and volume transferred
      !!------------------------------------------------------------------
      IF( ln_timing )   CALL timing_start('iceitd_reb')
      !
      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_itd_reb: rebining ice thickness distribution'
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(0, 'iceitd_reb', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (0, 'iceitd_reb',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)
      !
      jdonor(:,:) = 0
      zdaice(:,:) = 0._wp
      zdvice(:,:) = 0._wp
      !
      !                       !---------------------------------------
      DO jl = 1, jpl-1        ! identify thicknesses that are too big
         !                    !---------------------------------------
#if defined key_si3_1D
         DO_2D( 0, 0, 0, 0 )
            npti = 0   ;   nptidx(:) = 0
            IF( a_i(ji,jj,jl) > 0._wp .AND. v_i(ji,jj,jl) > (a_i(ji,jj,jl) * hi_max(jl)) ) THEN
               npti = 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
#else
         npti = 0   ;   nptidx(:) = 0
         DO_2D( 0, 0, 0, 0 )
            IF( a_i(ji,jj,jl) > 0._wp .AND. v_i(ji,jj,jl) > (a_i(ji,jj,jl) * hi_max(jl)) ) THEN
               npti = npti + 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
         END_2D
#endif
         !
         IF( npti > 0 ) THEN
            !!clem   CALL tab_2d_1d( npti, nptidx(1:npti), h_i_1d(1:npti), h_i(:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d(1:npti), a_i(:,:,jl) )
            CALL tab_2d_1d( npti, nptidx(1:npti), v_i_1d(1:npti), v_i(:,:,jl) )
            !
            DO ii = 1, npti
               jdonor(ii,jl)  = jl
               ! how much of a_i you send in cat sup is somewhat arbitrary
               ! these are from CICE => transfer everything
               !!zdaice(ii,jl)  = a_i_1d(ii)
               !!zdvice(ii,jl)  = v_i_1d(ii)
               ! these are from LLN => transfer only half of the category
               zdaice(ii,jl)  =                       0.5_wp  * a_i_1d(ii)
               zdvice(ii,jl)  = v_i_1d(ii) - (1._wp - 0.5_wp) * a_i_1d(ii) * hi_mean(jl)
            END DO
            !
            CALL itd_shiftice( jdonor(1:npti,:), zdaice(1:npti,:), zdvice(1:npti,:) )  ! Shift jl=>jl+1
            ! Reset shift parameters
            jdonor(1:npti,jl) = 0
            zdaice(1:npti,jl) = 0._wp
            zdvice(1:npti,jl) = 0._wp
         ENDIF
         !
#if defined key_si3_1D
         END_2D
#endif
      END DO

      !                       !-----------------------------------------
      DO jl = jpl-1, 1, -1    ! Identify thicknesses that are too small
         !                    !-----------------------------------------
#if defined key_si3_1D
         DO_2D( 0, 0, 0, 0 )
            npti = 0 ; nptidx(:) = 0
            IF( a_i(ji,jj,jl+1) > 0._wp .AND. v_i(ji,jj,jl+1) <= (a_i(ji,jj,jl+1) * hi_max(jl)) ) THEN
               npti = 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
#else
         npti = 0 ; nptidx(:) = 0
         DO_2D( 0, 0, 0, 0 )
            IF( a_i(ji,jj,jl+1) > 0._wp .AND. v_i(ji,jj,jl+1) <= (a_i(ji,jj,jl+1) * hi_max(jl)) ) THEN
               npti = npti + 1
               nptidx( npti ) = (jj - 1) * jpi + ji
            ENDIF
         END_2D
#endif
         !
         IF( npti > 0 ) THEN
            CALL tab_2d_1d( npti, nptidx(1:npti), a_i_1d(1:npti), a_i(:,:,jl+1) ) ! jl+1 is ok
            CALL tab_2d_1d( npti, nptidx(1:npti), v_i_1d(1:npti), v_i(:,:,jl+1) ) ! jl+1 is ok
            DO ii = 1, npti
               jdonor(ii,jl) = jl + 1
               zdaice(ii,jl) = a_i_1d(ii)
               zdvice(ii,jl) = v_i_1d(ii)
            END DO
            !
            CALL itd_shiftice( jdonor(1:npti,:), zdaice(1:npti,:), zdvice(1:npti,:) )  ! Shift jl+1=>jl
            ! Reset shift parameters
            jdonor(1:npti,jl) = 0
            zdaice(1:npti,jl) = 0._wp
            zdvice(1:npti,jl) = 0._wp
         ENDIF
         !
#if defined key_si3_1D
         END_2D
#endif
      END DO
      !
      ! clem: those fields must be updated on the halos: a_i, v_i, v_s, sv_i, oa_i, h_i, t_su, a_ip, v_ip, v_il, e_i, e_s
      !       note: ice_itd_reb is called in icedyn
      !             and in icethd (but once the arrays are already updated on the boundaries)
      !
      IF( ln_icediachk )   CALL ice_cons_hsm(1, 'iceitd_reb', rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft)
      IF( ln_icediachk )   CALL ice_cons2D  (1, 'iceitd_reb',  diag_v,  diag_s,  diag_t,  diag_fv,  diag_fs,  diag_ft)
      IF( ln_timing    )   CALL timing_stop ('iceitd_reb')
      !
   END SUBROUTINE ice_itd_reb


   SUBROUTINE ice_itd_init
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_itd_init ***
      !!
      !! ** Purpose :   Initializes the ice thickness distribution
      !! ** Method  :   ...
      !! ** input   :   Namelist namitd
      !!-------------------------------------------------------------------
      INTEGER  ::   jl            ! dummy loop index
      INTEGER  ::   ios, ioptio   ! Local integer output status for namelist read
      REAL(wp) ::   zhmax, znum, zden, zalpha   !   -      -
      !
      NAMELIST/namitd/ ln_cat_hfn, rn_himean, ln_cat_usr, rn_catbnd, rn_himin, rn_himax
      !!------------------------------------------------------------------
      !
      rn_catbnd(:) =  0._wp ! Circumvent possible initialization by compiler
                            ! to prevent from errors when writing output
      READ_NML_REF(numnam_ice,namitd)
      READ_NML_CFG(numnam_ice,namitd)
      IF(lwm) WRITE( numoni, namitd )
      !
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_itd_init: Initialization of ice cat distribution '
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namitd: '
         WRITE(numout,*) '      Ice categories are defined by a function of rn_himean**(-0.05)    ln_cat_hfn = ', ln_cat_hfn
         WRITE(numout,*) '         mean ice thickness in the domain                               rn_himean  = ', rn_himean
         WRITE(numout,*) '      Ice categories are defined by rn_catbnd                           ln_cat_usr = ', ln_cat_usr
         WRITE(numout,*) '      minimum ice thickness allowed                                     rn_himin   = ', rn_himin
         WRITE(numout,*) '      maximum ice thickness allowed                                     rn_himax   = ', rn_himax
      ENDIF
      !
      !-----------------------------------!
      !  Thickness categories boundaries  !
      !-----------------------------------!
      !                             !== set the choice of ice categories ==!
      ioptio = 0
      IF( ln_cat_hfn ) THEN   ;   ioptio = ioptio + 1   ;   nice_catbnd = np_cathfn    ;   ENDIF
      IF( ln_cat_usr ) THEN   ;   ioptio = ioptio + 1   ;   nice_catbnd = np_catusr    ;   ENDIF
      IF( ioptio /= 1 )   CALL ctl_stop( 'ice_itd_init: choose one and only one ice categories boundaries' )
      !
      SELECT CASE( nice_catbnd )
      !                                !------------------------!
      CASE( np_cathfn )                ! h^(-alpha) function
         !                             !------------------------!
         zalpha = 0.05_wp
         zhmax  = 3._wp * rn_himean
         hi_max(0) = 0._wp
         DO jl = 1, jpl
            znum = jpl * ( zhmax+1 )**zalpha
            zden = REAL( jpl-jl , wp ) * ( zhmax + 1._wp )**zalpha + REAL( jl , wp )
            hi_max(jl) = ( znum / zden )**(1./zalpha) - 1
         END DO
         !                             !------------------------!
      CASE( np_catusr )                ! user defined
         !                             !------------------------!
         DO jl = 0, jpl
            hi_max(jl) = rn_catbnd(jl)
         END DO
         !
      END SELECT
      !
      DO jl = 1, jpl                ! mean thickness by category
         hi_mean(jl) = ( hi_max(jl) + hi_max(jl-1) ) * 0.5_wp
      END DO
      !
      hi_max(jpl) = rn_himax        ! set to a big value to ensure that all ice is thinner than hi_max(jpl)
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '   ===>>>   resulting thickness category boundaries :'
      IF(lwp) WRITE(numout,*) '            hi_max(:)= ', hi_max(0:jpl)
      !
      IF( hi_max(1) < rn_himin )   CALL ctl_stop('ice_itd_init: the upper bound of the 1st category must be bigger than rn_himin')
      !
   END SUBROUTINE ice_itd_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :         Empty module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE iceitd
