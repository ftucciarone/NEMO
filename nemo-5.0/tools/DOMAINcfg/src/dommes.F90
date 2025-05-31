MODULE dommes
   !!==============================================================================
   !!                       ***  MODULE dommes   ***
   !! Ocean domain: Multi-Envelope s-coordinate system (MEs)
   !!==============================================================================
   !! History :
   !!  NEMO      4.2  ! 2023-02  (D. Bruciaferri) Original code  
   !!----------------------------------------------------------------------
    
   !!----------------------------------------------------------------------
   !!   zgr_mes          : build a global or local ME s-coordinate system 
   !!       mes_ini      : initialise ME vertical grid
   !!       mes_bld      : define discrete ME s-levels
   !!----------------------------------------------------------------------
   !!
   USE dom_oce           ! ocean domain
   !
   USE in_out_manager    ! I/O manager
   USE iom               ! I/O library
   USE lbclnk            ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp           ! distributed memory computing library
 
   IMPLICIT NONE
   PRIVATE
 
   PUBLIC   zgr_mes        ! called by domzgr.F90
   !
   CHARACTER(lc)      :: ctlmes                  ! control message error (lc=256)
   !                                 * Namelist namzgr_mes *
   INTEGER            :: nn_env                  ! Number of envelopes used
   INTEGER, PARAMETER :: max_nn_env = 5          ! Maximum allowed number of envelopes
   INTEGER            :: nn_slev(max_nn_env)     ! Array specifing number of levels of each enveloped vertical sub-zone
   INTEGER            :: nn_strt(max_nn_env)     ! Array specifing the stretching function 
                                                 ! for each envelope: Madec 1996 (0), 
                                                 ! Song and Haidvogel 1994 (1), Siddorn & Furner 2012 (2)
   REAL(wp)           :: rn_bot_min              ! min depth of ocean bottom (>0) (m)
   REAL(wp)           :: rn_bot_max              ! max depth of ocean bottom (= ocean depth) (>0) (m)
   REAL(wp)           :: max_dep_env(max_nn_env) ! global maximum depth of envelopes
   REAL(wp)           :: min_dep_env(max_nn_env) ! global minimum depth of envelopes
   REAL(wp)           :: rn_e_hc(max_nn_env)     ! Array specifing critical depth for transition to stretched
                                                 ! coordinates of each envelope
   REAL(wp)           :: rn_e_th(max_nn_env)     ! Array specifing surface control parameter (0<=th<=20) of SH94
                                                 ! or rn_zs of SF12 for each vertical sub-zone
   REAL(wp)           :: rn_e_bb(max_nn_env)     ! Array specifing bottom control parameter (0<=bb<=1) of SH94
                                                 ! or rn_zb_b of SF12 for each vertical sub-zone
   REAL(wp)           :: rn_e_ba(max_nn_env)     ! Array specifing bottom control parameter rn_zb_a of SF12 for
                                                 ! each vertical sub-zone
   REAL(wp)           :: rn_e_al(max_nn_env)     ! Array specifing stretching parameter rn_alpha of SF12 for
                                                 ! each vertical sub-zone
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: envl ! array for envelopes
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: z_gsigw3, z_gsigt3 ! arrays for debugging
   !! * Substitutions
#  include "do_loop_substitute.h90"
!
CONTAINS
 
   SUBROUTINE zgr_mes
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE zgr_mes  ***
      !!
      !! ** Purpose :  Generate a gloabal or localised 
      !!               ME s-coordinates systems
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk, je 
      !! Debugging variables
      INTEGER                          :: eii, eij, irnk           ! dummy loop argument
      INTEGER,  DIMENSION(2)           :: e_ij
      REAL(wp)                         :: min_dep
      REAL(wp), DIMENSION(max_nn_env)  :: rn_ebot_max
      REAL(wp), DIMENSION(jpk)         :: z_gsigw1, z_gsigt1       ! 1D arrays for debugging
      REAL(wp), DIMENSION(jpk)         :: gdepw1, gdept1           ! 1D arrays for debugging
      REAL(wp), DIMENSION(jpk)         :: e3t1, e3w1               ! 1D arrays for debugging
      REAL(wp), DIMENSION(jpi,jpj)     :: pmsk                     ! working array
      !-----------------------------------------------------------------------
      !
      ! Initialise ME s-coordinates
      ! -----------------------------------------------
      CALL mes_ini
      !
      ! Generating a global MEs vertical grid
      ! -----------------------------------------------
      CALL mes_bld
      !
      ! Some global domain printings
      ! -----------------------------------------------
      IF(lwp ) WRITE(numout,*) ' MIN val mbathy h90 ', MINVAL( mbathy(:,:) ),   &
         &                     ' MAX ', MAXVAL( mbathy(:,:) )

      IF( lwp )   THEN         ! min max values over the local domain
         WRITE(numout,*) ' MIN val mbathy  ', MINVAL( mbathy(:,:)    ), ' MAX ', MAXVAL( mbathy(:,:) )
         WRITE(numout,*) ' MIN val depth t ', MINVAL( gdept_0(:,:,:) ),   &
            &                          ' w ', MINVAL( gdepw_0(:,:,:) )
         WRITE(numout,*) ' MIN val e3    t ', MINVAL( e3t_0  (:,:,:) ), ' f '  , MINVAL( e3f_0  (:,:,:) ),   &
            &                          ' u ', MINVAL( e3u_0  (:,:,:) ), ' u '  , MINVAL( e3v_0  (:,:,:) ),   &
            &                          ' uw', MINVAL( e3uw_0 (:,:,:) ), ' vw'  , MINVAL( e3vw_0 (:,:,:) ),   &
            &                          ' w ', MINVAL( e3w_0  (:,:,:) )

         WRITE(numout,*) ' MAX val depth t ', MAXVAL( gdept_0(:,:,:) ),   &
            &                          ' w ', MAXVAL( gdepw_0(:,:,:) )
         WRITE(numout,*) ' MAX val e3    t ', MAXVAL( e3t_0  (:,:,:) ), ' f '  , MAXVAL( e3f_0  (:,:,:) ),   &
            &                          ' u ', MAXVAL( e3u_0  (:,:,:) ), ' u '  , MAXVAL( e3v_0  (:,:,:) ),   &
            &                          ' uw', MAXVAL( e3uw_0 (:,:,:) ), ' vw'  , MAXVAL( e3vw_0 (:,:,:) ),   &
            &                          ' w ', MAXVAL( e3w_0  (:,:,:) )
      ENDIF
      !
      ! Check coordinates makes sense
      ! -----------------------------------------------
      ! Extracting MEs depth profile in the shallowest point of the deepest 
      ! envelope for a first check of monotonicity of transformation 
      ! (also useful for debugging)
      pmsk(:,:) = 0.0
      WHERE ( bathy > 0.0 ) pmsk = 1.0
      CALL mpp_minloc( 'zgr_mes', envl(:,:,nn_env), pmsk(:,:), min_dep, e_ij )
      eii = e_ij(1)
      eij = e_ij(2)
      IF ((mi0(eii)>=1 .AND. mi0(eii)<=jpi) .AND. (mj0(eij)>=1 .AND. mj0(eij)<=jpj)) THEN
         irnk = mpprank
         DO je = 1, nn_env
            rn_ebot_max(je) = envl(mi0(eii),mj0(eij),je)
         END DO
         z_gsigt1(:) = z_gsigt3(mi0(eii),mj0(eij),:)
         z_gsigw1(:) = z_gsigw3(mi0(eii),mj0(eij),:)
         gdept1(:)   = gdept_0(mi0(eii),mj0(eij),:)
         gdepw1(:)   = gdepw_0(mi0(eii),mj0(eij),:)
         e3t1(:)     = e3t_0(mi0(eii),mj0(eij),:)
         e3w1(:)     = e3w_0(mi0(eii),mj0(eij),:)
      ELSE
         irnk = -1
      END IF
      IF( lk_mpp ) CALL mppsync
      IF( lk_mpp ) CALL mpp_max('zgr_mes', irnk)
      IF( lk_mpp ) CALL mpp_bcast_real(rn_ebot_max, max_nn_env, irnk )
      IF( lk_mpp ) CALL mpp_bcast_real(z_gsigt1   , jpk       , irnk )
      IF( lk_mpp ) CALL mpp_bcast_real(z_gsigw1   , jpk       , irnk )
      IF( lk_mpp ) CALL mpp_bcast_real(gdept1     , jpk       , irnk )
      IF( lk_mpp ) CALL mpp_bcast_real(gdepw1     , jpk       , irnk )
      IF( lk_mpp ) CALL mpp_bcast_real(e3t1       , jpk       , irnk )
      IF( lk_mpp ) CALL mpp_bcast_real(e3w1       , jpk       , irnk )

      IF( lwp ) THEN
        WRITE(numout,*) ""
        WRITE(numout,*) "mes_build:"
        WRITE(numout,*) "~~~~~~~~~"
        WRITE(numout,*) ""
        WRITE(numout,*) " FIRST CHECK: Checking MEs-levels profile in the shallowest point of the last envelope:"
        WRITE(numout,*) "              it is the most likely point where monotonicty of splines may be violeted. "
        WRITE(numout,*) ""
        DO je = 1, nn_env
           WRITE(numout,*) '              * depth of envelope ', je, ' at point (',eii,',',eij,') is ', rn_ebot_max(je)
        END DO
        WRITE(numout,*) ""
        WRITE(numout,*) "      MEs-coordinates"
        WRITE(numout,*) ""
        WRITE(numout,*) "      k     z_gsigw1      z_gsigt1"
        WRITE(numout,*) ""
        DO jk = 1, jpk
           WRITE(numout,*) '   ', jk, z_gsigw1(jk), z_gsigt1(jk)
        ENDDO
        WRITE(numout,*) ""
        WRITE(numout,*) "-----------------------------------------------------------------"
        WRITE(numout,*) ""
        WRITE(numout,*) "      MEs-levels depths and scale factors"
        WRITE(numout,*) ""
        WRITE(numout,*) "      k     gdepw1      e3w1       gdept1      e3t1"
        WRITE(numout,*) ""
        DO jk = 1, jpk 
           WRITE(numout,*) '   ', jk, gdepw1(jk), e3w1(jk), gdept1(jk), e3t1(jk)
        ENDDO
        WRITE(numout,*) "-----------------------------------------------------------------"
      ENDIF

      ! Checking monotonicity for cubic splines
      DO jk = 1, jpk-1
         IF ( gdept1(jk+1) < gdept1(jk) ) THEN
            WRITE(ctlmes,*) 'NOT MONOTONIC gdept_0: change envelopes'
            CALL ctl_stop( ctlmes )
         END IF
         IF ( gdepw1(jk+1) < gdepw1(jk) ) THEN
            WRITE(ctlmes,*) 'NOT MONOTONIC gdepw_0: change envelopes'
            CALL ctl_stop( ctlmes )
         END IF
         IF ( e3t1(jk) < 0.0 ) THEN
            WRITE(ctlmes,*) 'NEGATIVE e3t_0: change envelopes'
            CALL ctl_stop( ctlmes )
         END IF
         IF ( e3w1(jk) < 0.0 ) THEN
            WRITE(ctlmes,*) 'NEGATIVE e3w_0: change envelopes'
            CALL ctl_stop( ctlmes )
         END IF
      END DO

      ! CHECKING THE WHOLE DOMAIN
      DO ji = 1, jpi
         DO jj = 1, jpj
            DO jk = 1, jpk !mbathy(ji,jj)
               ! check coordinate is monotonically increasing
               IF (e3w_0(ji,jj,jk) <= 0._wp .OR. e3t_0(ji,jj,jk) <= 0._wp ) THEN
                  WRITE(ctmp1,*) 'ERROR mes_build:   e3w   or e3t   =< 0  at point (i,j,k)= ', ji, jj, jk
                  WRITE(numout,*) 'ERROR mes_build:   e3w   or e3t   =< 0  at point (i,j,k)= ', ji, jj, jk
                  WRITE(numout,*) 'e3w',e3w_0(ji,jj,:)
                  WRITE(numout,*) 'e3t',e3t_0(ji,jj,:)
                  CALL ctl_stop( ctmp1 )
               ENDIF
               ! and check it has never gone negative
               IF ( gdepw_0(ji,jj,jk) < 0._wp .OR. gdept_0(ji,jj,jk) < 0._wp ) THEN
                  WRITE(ctmp1,*) 'ERROR mes_build:   gdepw or gdept =< 0  at point (i,j,k)= ', ji, jj, jk
                  WRITE(numout,*) 'ERROR mes_build:   gdepw   or gdept   =< 0  at point (i,j,k)= ', ji, jj, jk
                  WRITE(numout,*) 'gdepw',gdepw_0(ji,jj,:)
                  WRITE(numout,*) 'gdept',gdept_0(ji,jj,:)
                  CALL ctl_stop( ctmp1 )
               ENDIF
            END DO
         END DO
      END DO

   END SUBROUTINE zgr_mes

   SUBROUTINE mes_bld
      !!-----------------------------------------------------------------------------
      !!                  ***  ROUTINE mes_bld  ***
      !!                     
      !! ** Purpose :   define the Multi-Envelope s-coordinate (MEs) system
      !!
      !! ** Method  :   Generalised s-coordinates are defined with respect to
      !!                multiple arbitrarily defined envelopes as detailed in
      !!
      !!                Bruciaferri, Shapiro, Wobus, 2018. Oce. Dyn. 
      !!                https://doi.org/10.1007/s10236-018-1189-x
      !!
      !!                The ME method relies on n envelopes (i.e., arbitrarily 
      !!                defined depth surfaces) dividing the ocean model vertical
      !!                domain into n subzones D_(i), with 1<=i<=n, each one bounded
      !!                by envelope He_(i-1) at the top and envelope He_(i) at the bottom,
      !!                with He_0 = eta (the free surface). Then:
      !!
      !!                (a) FOR ODD i, the transormation from computational space
      !!                    (MEs-space) to physical space (depth z-space) is
      !!
      !!                           z = He_0 + hc*s - C(s)*(He_1 - hc - He_0)
      !!                       
      !!                    where the depth z and envelopes are downward positive 
      !!                    defined, -1 <= s <= 0, with s(He_0)=0 and s(He_1)=-1
      !!                    and C(s) is a stretching function.
      !!                     
      !!                (b) FOR EVEN i: the transormation from MEs-space to z-space
      !!                    is given by
      !! 
      !!                           z = P3(C(s)) 
      !!
      !!                    where P3 is a 3rd order polynomial whose coefficients 
      !!                    are computed locally requiring monotonicity of the 
      !!                    transformation and continuity of its Jacobian and C(s) 
      !!                    is stretching function 
      !! 
      !!                Three options for stretching are given:
      !! 
      !!                   *) nn_strt(i) = 0 : Madec et al 1996 cosh/tanh function
      !!                   *) nn_strt(i) = 1 : Song and Haidvogel 1994 sinh/tanh function  
      !!                   *) nn_strt(i) = 2 : Siddorn and Furner gamma function
      !!
      !!-----------------------------------------------------------------------------------
      !!                SKETCH of the GEOMETRY OF A MEs-COORDINATE SYSTEM         
      !!
      !!                3 envelopes are used in this example, such that 
      !!                0 < He1 < He2 < He3 :
      !! 
      !!                   === 1st envelope He1 (the shallowest)
      !!                   ¬¬¬ 2nd envelope He2
      !!                   ___ 3rd envelope he3 (the deepest)
      !!                   --- W-levels
      !!                   
      !!                   D1: W-levels marked as D1 belong to the upper 
      !!                       sub-zone D1:
      !!                       *) The number of discrete levels is controlled 
      !!                          by the nn_slev(1) namelist parameter.
      !!                       *) The transormation from computational space
      !!                          to physical space is (a)
      !!                     
      !!                       Depth first W-lev: 0 m (surface)
      !!                       Depth last  W-lev: depth of 1st envelope                     
      !!
      !!                   D2: W-levels marked as D2 belong to the second 
      !!                       sub-zone D2:
      !!                       *) The number of discrete levels is controlled 
      !!                          by the nn_slev(2) namelist parameter.
      !!                       *) The transormation from computational space
      !!                          to physical space is (b)
      !!                       
      !!                       Depth last  W-lev: depth of 2nd envelope
      !!
      !!                   D3: W-levels marked as D3 belong to the third 
      !!                       sub-zone D3:
      !!                       *) The number of discrete levels is controlled 
      !!                          by the nn_slev(3) namelist parameter.
      !!                       *) The transormation from computational space
      !!                          to physical space is (a)
      !!        
      !!                       Depth last  W-lev: depth of 3rd envelope  
      !!    
      !!                   |~~~~~~~~~~~~~~~~~~~~D1~~~~~~~~~~~~~~~~~~~ SURFACE                 
      !!                   | 
      !!                   |--------------------D1------------------- nn_slev(1)=3
      !!                   |
      !!                   |====================D1=================== ENVELOPE 1
      !!                   | 
      !!                   |--------------------D2-------------------
      !!                   |                                          nn_slev(2)=3
      !!                   |--------------------D2-------------------
      !!                   |
      !!                   |¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬D2¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬ ENVELOPE 2
      !!                   |
      !!                   |--------------------D3------------------- nn_slev(3)=2
      !!                   |
      !!                   |____________________D3___________________ ENVELOPE 3
      !!                z \|/
      !!----------------------------------------------------------------------
      !! Coordinate transformation variables
      INTEGER                          :: nn_s, jks, jks1          ! for loops over envelopes
      INTEGER                          :: nn_s_top, nn_s_bot       ! for loops over envelopes
      REAL(wp)                         :: coefa, coefb             ! for loops over envelopes
      REAL(wp)                         :: alpha, env_hc            ! for loops over envelopes
      REAL(wp)                         :: zcoeft, zcoefw           ! temporary scalars
      REAL(wp)                         :: max_env_top, min_env_top ! to identify geopotential levels
      REAL(wp)                         :: max_env_bot, min_env_bot ! to identify geopotential levels
      REAL(wp), DIMENSION(jpi,jpj)     :: env_top, env_bot         ! for loops over envelopes
      !! Cubic Splines variables
      REAL(wp)                         :: dCds_top, dCds_bot       ! for loops over envelopes
      INTEGER                          :: ibcbeg, ibcend           ! boundary condition for cubic splines
      REAL(wp)                         :: c(4,2)                   ! matrix for cubic splines coefficents
      REAL(wp)                         :: d(2,2)                   ! matrix for depth values and depth 
                                                                   ! derivative at intervals' boundaries
      REAL(wp)                         :: tau(2)                   ! abscissas of intervals' boundaries
      REAL(wp), DIMENSION(jpi,jpj)     :: env0, env1, env2, env3   ! for loops over envelopes
                                                                   ! for cubic splines
      !
      INTEGER                          :: ji, jj, jk, je           ! dummy loop argument
      !!------------------------------------------------------------------------
      
      ! Initialise mbathy to the maximum ocean level available
      mbathy(:,:) = jpkm1

      ! Define scosrf
      scosrf(:,:) = 0._wp ! ocean surface depth (here zero: no under ice-shelf sea)
      
      !============================
      ! Compute 3D MEs-levels depth 
      !============================
      jks1 = 2
      nn_s = 0
      env_top = scosrf ! surface
      !
      DO je = 1, nn_env ! LOOP over all the requested envelopes
         !
         jks1 = jks1 + nn_s - 1
         nn_s = nn_slev(je)
         if ( je > 1 ) THEN
            nn_s = nn_s + 1
            env_top = envl(:,:,je-1)
         END IF
         env_bot = envl(:,:,je)
         !
         IF ( isodd(je) ) THEN
            !
            DO jk = 1, nn_s
               jks = jks1 + jk - 1
               DO jj = 1,jpj
                  DO ji = 1,jpi
                     !
                     ! -------------------------------------------
                     ! Stretching coefficients
                     !
                     env_hc  = rn_e_hc(je)
                     coefa   = rn_e_th(je)
                     coefb   = rn_e_bb(je)
                     alpha   = rn_e_al(je)
                     ! Coefficients for SF12 stretching
                     IF (nn_strt(je) == 2) THEN
                        coefa = rn_e_th(je) /  (env_bot(ji,jj)-env_top(ji,jj))
                        coefb = rn_e_bb(je) + ((env_bot(ji,jj)-env_top(ji,jj))*rn_e_ba(je))
                        coefb = 1.0_wp-(coefb/(env_bot(ji,jj)-env_top(ji,jj)))
                     END IF
                     ! 
                     ! 1) Computing MEs-coordinates (-1 <= MEs <= 0)
                     ! ===============================================
                     !
                     ! SHALLOW WATER, uniform sigma
                     IF (env_bot(ji,jj) < env_hc) THEN
                        zcoefw = -sigma('W', jk, nn_s) ! W-GRID
                        zcoeft = -sigma('T', jk, nn_s) ! T-GRID
                     ! DEEP WATER, stretched s
                     ELSE
                        zcoefw = -stretch( sigma('W',jk,nn_s), coefa,          &
                           &               coefb, alpha, nn_s, nn_strt(je) ) ! W-GRID
                        zcoeft = -stretch( sigma('T',jk,nn_s), coefa,          &
                           &               coefb, alpha, nn_s, nn_strt(je) ) ! T-GRID
                     ENDIF
                     !
                     ! For debugging
                     z_gsigw3(ji,jj,jks) = zcoefw
                     z_gsigt3(ji,jj,jks) = zcoeft
                     !
                     ! 2) Computing model levels depths
                     ! ===============================================
                     !
                     IF (nn_strt(je) == 2) env_hc = 0._wp
                     gdepw_0(ji,jj,jks) = z_mes(env_top(ji,jj), env_bot(ji,jj), zcoefw,  &
                        &                       -sigma('W', jk, nn_s), env_hc           ) ! W-GRID
                     gdept_0(ji,jj,jks) = z_mes(env_top(ji,jj), env_bot(ji,jj), zcoeft,  &
                        &                       -sigma('T', jk, nn_s), env_hc           ) ! T-GRID
                  END DO
               END DO
            END DO
            !
         ELSE
            !
            IF ( je == 2 ) THEN
               env0 = scosrf
            ELSE
               env0 = envl(:,:,je-2) 
            END IF
            env1 = env_top
            env2 = env_bot
            IF ( je < nn_env ) env3 = envl(:,:,je+1)
            !
            DO jj = 1,jpj
               DO ji = 1,jpi
                  d(1,1) = env_top(ji,jj)
                  d(1,2) = env_bot(ji,jj)
                  !
                  ! 1) Computing boundary conditions
                  !    for the 1st or 2nd derivative
                  !    to constrain splines
                  !===================================
                  !
                  ! SUB-ZONE ABOVE (je-1)
                  !
                  ibcbeg = 1 ! Bound. cond for the 1st derivative
                  ! -------------------------------------------
                  ! Stretching coefficients
                  !
                  env_hc = rn_e_hc(je-1)
                  alpha  = rn_e_al(je-1)
                  coefa  = rn_e_th(je-1)
                  coefb  = rn_e_bb(je-1)
                  ! Coefficients for SF12 stretching
                  IF (nn_strt(je-1) == 2) THEN
                     env_hc = 0._wp
                     coefa  = rn_e_th(je-1) / (env1(ji,jj)-env0(ji,jj))
                     coefb  = rn_e_bb(je-1) + ((env1(ji,jj)-env0(ji,jj))*rn_e_ba(je-1))
                     coefb  = 1.0_wp-(coefb/(env1(ji,jj)-env0(ji,jj)))                   
                  END IF
                  ! -------------------------------------------
                  ! Analytical derivative of coordinate 
                  ! transformation at envelope (je-1)
                  !
                  zcoefw = -1._wp
                  dCds_top = dstretch( zcoefw, coefa, coefb, alpha,  &
                      &                nn_slev(je-1), nn_strt(je-1) )
                  d(2,1) = dzdmes(env0(ji,jj), env1(ji,jj), dCds_top, env_hc)
                  !
                  ! SUB-ZONE BELOW (je+1)
                  !
                  IF ( je < nn_env ) THEN
                     ibcend = 1 ! Bound. cond for the 1st derivative
                     ! -------------------------------------------
                     ! Stretching coefficients
                     !
                     env_hc = rn_e_hc(je+1)
                     alpha  = rn_e_al(je+1)
                     coefa  = rn_e_th(je+1)
                     coefb  = rn_e_bb(je+1)
                     ! Coefficients for SF12 stretching
                     IF (nn_strt(je+1) == 2) THEN
                        env_hc = 0._wp
                        coefa  = rn_e_th(je+1) / (env3(ji,jj)-env2(ji,jj))
                        coefb  = rn_e_bb(je+1) + ((env3(ji,jj)-env2(ji,jj))*rn_e_ba(je+1))
                        coefb  = 1.0_wp-(coefb/(env3(ji,jj)-env2(ji,jj)))
                     END IF
                     ! -------------------------------------------
                     ! Analytical derivative of coordinate 
                     ! transformation at envelope (je+1)
                     !
                     zcoefw = 0._wp
                     dCds_bot = dstretch( zcoefw, coefa, coefb, alpha, &
                         &                nn_slev(je+1), nn_strt(je+1))
                     d(2,2) = dzdmes(env2(ji,jj), env3(ji,jj), dCds_bot, env_hc)               
                  ELSE
                     ! Splines are applied in the bottom most sub-zone:
                     ibcend = 2     ! Bound. cond for the 2nd derivative
                     d(2,2) = 0._wp ! 2nd der = 0
                  ENDIF
                  !
                  ! 2) Computing spline's coefficients
                  ! ===============================================
                  ! Interval's endpoints TAU are 0 and 1.
                  tau(1) = 0._wp
                  tau(2) = 1._wp
                  c = cub_spl(tau, d, 2, ibcbeg, ibcend )
                  ! TO ADD CONSTRAINED CUBIC SPLINES CASE
                  !
                  ! 3) Stretching coefficients for this sub-zone je
                  ! ===============================================
                  env_hc  = rn_e_hc(je)
                  coefa   = rn_e_th(je)
                  coefb   = rn_e_bb(je)
                  alpha   = rn_e_al(je)
                  !
                  DO jk = 1, nn_s
                     jks = jks1 + jk - 1
                     !
                     ! 4) Computing stretched distribution of 
                     !    interpolation points -> they represent 
                     !    MEs-coordinates (-1 <= MEs <= 0)
                     ! ===============================================
                     zcoefw = -stretch( sigma('W',jk,nn_s), coefa,          &
                        &               coefb, alpha, nn_s, nn_strt(je) ) ! W-GRID
                     zcoeft = -stretch( sigma('T',jk,nn_s), coefa,          &
                        &               coefb, alpha, nn_s, nn_strt(je) ) ! T-GRID
                     !
                     ! For debugging
                     z_gsigw3(ji,jj,jks) = zcoefw
                     z_gsigt3(ji,jj,jks) = zcoeft
                     !
                     ! 5) Computing model levels depths
                     ! ===============================================
                     gdept_0(ji,jj,jks) = z_cub_spl(zcoeft, tau, c)
                     gdepw_0(ji,jj,jks) = z_cub_spl(zcoefw, tau, c)
                     !
                  END DO ! jk
               END DO ! ji
            END DO ! jj
         END IF    
      END DO ! je     
        
      !==================================
      ! DEPTHS of T-LEVES at cell centers
      ! if requested (ln_dept_mid=TRUE)
      !==================================
      IF ( ln_dept_mid ) THEN
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )   
              gdept_0(ji,jj,jk) = 0.5_wp * ( gdepw_0(ji,jj,jk) +  gdepw_0(ji,jj,jk+1) )
         END_3D
      ENDIF

      !=================================
      ! COMPUTE e3t, e3w for ALL levels
      ! as finite differences
      ! ================================
      DO jk=1,jpkm1
         e3t_0(:,:,jk)   = gdepw_0(:,:,jk+1) - gdepw_0(:,:,jk)
         e3w_0(:,:,jk+1) = gdept_0(:,:,jk+1) - gdept_0(:,:,jk)
      ENDDO
      ! Surface
      jk = 1
      e3w_0(:,:,jk) = 2.0_wp * (gdept_0(:,:,1) - gdepw_0(:,:,1))
      !
      ! Bottom
      jk = jpk
      e3t_0(:,:,jk) = 2.0_wp * (gdept_0(:,:,jk) - gdepw_0(:,:,jk))
    
      ! Lateral B.C.
      ! ----------------------------------------------- 
      CALL lbc_lnk('dommes',e3t_0,'T', 1.)
      CALL lbc_lnk('dommes',e3w_0,'W', 1.) 

      !==============================
      ! Computing mbathy
      !==============================
      IF( lwp ) WRITE(numout,*) ''
      IF( lwp ) WRITE(numout,*) 'MEs mbathy:'
      IF( lwp ) WRITE(numout,*) ''

      DO jj = 1, jpj
         DO ji = 1, jpi
            DO jk = 1, jpkm1
               IF( bathy(ji,jj) >= gdept_0(ji,jj,jk) ) mbathy(ji,jj) = MAX( 2, jk )
            END DO
         END DO
      END DO

      WHERE (bathy(:,:)<=0) mbathy(:,:) = 0

      !==============================================
      ! Computing e3u_0, e3v_0, e3f_0, e3uw_0, e3vw_0
      !==============================================   
      DO jk = 1, jpk
         DO jj = 1,jpjm1
            DO ji = 1,jpim1
               e3u_0(ji,jj,jk) = ( MIN(1,mbathy(ji  ,jj)) * e3t_0(ji  ,jj,jk) +   &
                  &                MIN(1,mbathy(ji+1,jj)) * e3t_0(ji+1,jj,jk) ) / &
                  &              MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji+1,jj)))

               e3v_0(ji,jj,jk) = ( MIN(1,mbathy(ji,jj  )) * e3t_0(ji,jj  ,jk) +   &
                  &                MIN(1,mbathy(ji,jj+1)) * e3t_0(ji,jj+1,jk) ) / &
                  &              MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji,jj+1)))

               e3uw_0(ji,jj,jk) = ( MIN(1,mbathy(ji  ,jj)) * e3w_0(ji  ,jj,jk) +   &
                  &                 MIN(1,mbathy(ji+1,jj)) * e3w_0(ji+1,jj,jk) ) / &
                  &               MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji+1,jj)))

               e3vw_0(ji,jj,jk) = ( MIN(1,mbathy(ji,jj  )) * e3w_0(ji,jj  ,jk) +   &
                  &                 MIN(1,mbathy(ji,jj+1)) * e3w_0(ji,jj+1,jk) ) / &
                  &               MAX(1, MIN(1,mbathy(ji,jj))+MIN(1,mbathy(ji,jj+1)))

               e3f_0(ji,jj,jk) = ( MIN(1,mbathy(ji  ,jj  )) * e3t_0(ji  ,jj  ,jk) +      &
                  &               MIN(1,mbathy(ji+1,jj  )) * e3t_0(ji+1,jj  ,jk)  +      &
                  &               MIN(1,mbathy(ji+1,jj+1)) * e3t_0(ji+1,jj+1,jk)  +      &
                  &               MIN(1,mbathy(ji  ,jj+1)) * e3t_0(ji  ,jj+1,jk) ) /     &
                  &              MAX(1,  MIN(1,mbathy(ji  ,jj))+MIN(1,mbathy(ji  ,jj+1)) &
                  &                    + MIN(1,mbathy(ji+1,jj))+MIN(1,mbathy(ji+1,jj+1))) 
      
            END DO
         END DO
      END DO
      !==============================================
      ! Adjusting for geopotential levels, if any
      !==============================================
      jks1 = 2
      nn_s = 0
      max_env_top = 0._wp  ! surface
      min_env_top = 0._wp  ! surface
      DO je = 1, nn_env ! LOOP over all the requested envelopes
         jks1 = jks1 + nn_s - 1
         nn_s = nn_slev(je)
         if ( je > 1 ) THEN
            nn_s = nn_s + 1
            max_env_top = max_dep_env(je-1)
            min_env_top = min_dep_env(je-1)
         END IF
         max_env_bot  = max_dep_env(je)
         min_env_bot  = min_dep_env(je)

         IF ( max_env_top == min_env_top .AND. max_env_bot == min_env_bot ) THEN
            IF( lwp ) WRITE(numout,*) '... Adjusting for geopotential levels ...'
            DO jj = 1,jpjm1
               DO ji = 1,jpim1
                  DO jk = 1, nn_s
                     jks = jks1 + jk - 1
                     !
                     e3u_0 (ji,jj,jks) = MIN( e3t_0(ji,jj,jks), e3t_0(ji+1,jj  ,jks))
                     e3uw_0(ji,jj,jks) = MIN( e3w_0(ji,jj,jks), e3w_0(ji+1,jj  ,jks))
                     e3v_0 (ji,jj,jks) = MIN( e3t_0(ji,jj,jks), e3t_0(ji  ,jj+1,jks))
                     e3vw_0(ji,jj,jks) = MIN( e3w_0(ji,jj,jks), e3w_0(ji  ,jj+1,jks))
                     e3f_0 (ji,jj,jks) = MIN( e3t_0(ji,jj,jks), e3t_0(ji+1,jj  ,jks))
                     !
                  END DO
               END DO
            END DO
         END IF 
      END DO
      !
      ! Lateral B.C.
      ! -----------------------------------------------
      CALL lbc_lnk( 'dommes', e3u_0 , 'U', 1._wp )
      CALL lbc_lnk( 'dommes', e3v_0 , 'V', 1._wp )
      CALL lbc_lnk( 'dommes', e3f_0 , 'F', 1._wp )
      CALL lbc_lnk( 'dommes', e3uw_0, 'U', 1._wp )
      CALL lbc_lnk( 'dommes', e3vw_0, 'V', 1._wp )
      !
      WHERE (e3t_0   (:,:,:) == 0.0)  e3t_0(:,:,:) = 1.0
      WHERE (e3u_0   (:,:,:) == 0.0)  e3u_0(:,:,:) = 1.0
      WHERE (e3v_0   (:,:,:) == 0.0)  e3v_0(:,:,:) = 1.0
      WHERE (e3f_0   (:,:,:) == 0.0)  e3f_0(:,:,:) = 1.0
      WHERE (e3w_0   (:,:,:) == 0.0)  e3w_0(:,:,:) = 1.0
      WHERE (e3uw_0  (:,:,:) == 0.0)  e3uw_0(:,:,:) = 1.0
      WHERE (e3vw_0  (:,:,:) == 0.0)  e3vw_0(:,:,:) = 1.0
      
      !!----------------------------------------------------------------------
   END SUBROUTINE mes_bld 
    
   SUBROUTINE mes_ini
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE mes_ini  ***
      !!
      !! ** Purpose :  Initialise a ME s-coordinates systems
      !!  
      !!----------------------------------------------------------------------
      CHARACTER(lc)                :: env_name   ! name of the externally defined envelopes
      INTEGER                      :: ji, jj, je
      INTEGER                      :: ierr, inum, ios
          
      NAMELIST/namzgr_mes/rn_bot_min, rn_bot_max, nn_strt, &
          &               nn_slev   , rn_e_hc   , rn_e_th, &
          &               rn_e_bb   , rn_e_ba   , rn_e_al     
      !-----------------------------------------------------------------------
      !
      ! Initialising some variables and arrays
      ALLOCATE( envl(jpi, jpj, max_nn_env), STAT = ierr )
      ALLOCATE(z_gsigw3(jpi, jpj, jpk), z_gsigt3(jpi, jpj, jpk), STAT = ierr )
      nn_env         = 0
      nn_strt(:)     = 0
      nn_slev(:)     = 0
      max_dep_env(:) = 0.0
      min_dep_env(:) = 0.0

      gdept_0(:,:,:) = 0._wp ; gdepw_0(:,:,:) = 0._wp ;
      e3t_0  (:,:,:) = 0._wp ; e3w_0  (:,:,:) = 0._wp ;
      e3u_0  (:,:,:) = 0._wp ; e3v_0  (:,:,:) = 0._wp ;
      e3uw_0 (:,:,:) = 0._wp ; e3vw_0 (:,:,:) = 0._wp ;
      !
      IF(lwp) THEN                           ! control print
         WRITE(numout,*) ''
         WRITE(numout,*) 'mes_ini: Setting a Multi-Envelope s-coordinate system (Bruciaferri et al. 2018)'
         WRITE(numout,*) '~~~~~~~~'
      END IF
      !
      ! Namelist namzgr_mes in reference namelist:
      !REWIND( numnam_ref )
      READ  ( numnam_ref, namzgr_mes, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzgr_mes in reference namelist')
      !
      ! Namelist namzgr_mes in configuration namelist:
      !REWIND( numnam_cfg )
      READ  ( numnam_cfg, namzgr_mes, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namzgr_mes in configuration namelist')
      IF(lwm) WRITE ( numond, namzgr_mes )
       
      ! 1) Reading Bathymetry and envelopes
      IF ( ntopo == 1 ) THEN
         IF ( lwp ) THEN
            WRITE(numout,*) '          Reading bathymetry and envelopes ...'
            WRITE(numout,*) ''
         END IF
         !
         CALL iom_open ( cn_topo, inum )
         CALL iom_get  ( inum, jpdom_auto, cn_bath, bathy )
         !
         DO je = 1, max_nn_env
            WRITE(env_name, '(A6,I0)') 'hbatt_', je
            IF ( iom_varid( inum, TRIM(env_name), ldstop = .FALSE. ) > 0 ) THEN
               CALL iom_get ( inum, jpdom_auto, TRIM(env_name), envl(:,:,je) )
               nn_env = nn_env + 1
            END IF
         END DO
         !
         CALL iom_close( inum )
      ELSE
         WRITE(ctlmes,*) 'parameter , ntopo = ', ntopo
         CALL ctl_stop( ctlmes )
      ENDIF
      !
      ! 2) Checking consistency of envelopes
      DO je = 1, nn_env-1
         WRITE(ctlmes,*) 'Envelope ', je+1, ' is shallower that Envelope ', je
         IF (MAXVAL(envl(:,:,je+1)) < MAXVAL(envl(:,:,je))) CALL ctl_stop( ctlmes )
      ENDDO
      !
      ! 3) Checking SF12 stretching function 
      !    is used only in the upper sub-zone
      DO je = 2, nn_env
         IF ( nn_strt(je) == 2 ) THEN
            WRITE(ctlmes,*) 'SF12 stretching function MUST be used only in the upper sub-zone'
            CALL ctl_stop( ctlmes )
         END IF     
      END DO   
      !    
      ! 4) Computing max and min depths of envelopes
      DO je = 1, nn_env
         max_dep_env(je) = MAXVAL(envl(:,:,je))
         min_dep_env(je) = MINVAL(envl(:,:,je))
         IF( lk_mpp ) CALL mpp_max( 'mes_ini', max_dep_env(je) )
         IF( lk_mpp ) CALL mpp_min( 'mes_ini', min_dep_env(je) )
      END DO
      IF( lk_mpp ) CALL mppsync
      !
      ! 5) Set maximum and minimum ocean depth
      bathy(:,:) = MIN( rn_bot_max, bathy(:,:) )
      DO jj = 1, jpj
         DO ji = 1, jpi
           IF( bathy(ji,jj) > 0._wp )   bathy(ji,jj) = MAX( rn_bot_min, bathy(ji,jj) )
         END DO
      END DO
      !
      IF( lwp ) THEN                           ! control print
        WRITE(numout,*)
        WRITE(numout,*) '          GEOMETRICAL FEATURES OF THE REQUESTED MEs GRID:'
        WRITE(numout,*) '          -----------------------------------------------'
        WRITE(numout,*) '          Minimum depth of the ocean   rn_bot_min = ', rn_bot_min
        WRITE(numout,*) '          Maximum depth of the ocean   rn_bot_max = ', rn_bot_max
        WRITE(numout,*) ''
        WRITE(numout,*) '          ENVELOPES:'
        WRITE(numout,*) '          ========= '
        WRITE(numout,*) ''
        DO je = 1, nn_env
            WRITE(numout,*) '             * envelope ', je, ':   min depth = ', min_dep_env(je)
            WRITE(numout,*) '                               max depth = ', max_dep_env(je)
            WRITE(numout,*) ''
        END DO
        WRITE(numout,*) ''
        WRITE(numout,*) '          SUBDOMAINS:'
        WRITE(numout,*) '          ========== '
        WRITE(numout,*) ''
        DO je = 1, nn_env
           WRITE(numout,*) '             * subdomain ', je,':'
           IF ( je == 1) THEN
              WRITE(numout,*) '                  Shallower envelope: free surface'
           ELSE
              WRITE(numout,*) '                  Shallower envelope: envelope ',je-1
           END IF
           WRITE(numout,*) '                  Deeper    envelope: envelope ',je
           WRITE(numout,*) '                  Number of MEs-levs: nn_slev(',je,') = ',nn_slev(je)
           IF( isodd(je) ) THEN
             WRITE(numout,*) '                  Stretched s-coordinates: '
           ELSE
             WRITE(numout,*) '                  Stretched CUBIC SPLINES: '
           END IF
           IF ( nn_strt(je) == 0 ) WRITE(numout,*) '                    M96  stretching function'
           IF ( nn_strt(je) == 1 ) WRITE(numout,*) '                    SH94 stretching function'
           IF ( nn_strt(je) == 2 ) WRITE(numout,*) '                    SF12 stretching function'
           WRITE(numout,*) '                    critical depth        rn_e_hc(',je,') = ',rn_e_hc(je)
           WRITE(numout,*) '                    surface stretc. coef. rn_e_th(',je,') = ',rn_e_th(je)
           IF( nn_strt(je) == 2 ) THEN
              WRITE(numout,*) '                    bottom  stretc. coef. rn_e_ba(',je,') = ',rn_e_ba(je)
           END IF
           WRITE(numout,*) '                    bottom  stretc. coef. rn_e_bb(',je,') = ',rn_e_bb(je)
           IF( nn_strt(je) == 2 ) THEN
             WRITE(numout,*) '                 bottom  stretc. coef. rn_e_al(',je,') = ',rn_e_al(je)
           END IF
           WRITE(numout,*) '          ------------------------------------------------------------------'
        ENDDO
      ENDIF
   END SUBROUTINE mes_ini
   !!
   
   FUNCTION isodd( a ) RESULT( odd )
      !!---------------------------------------------------------------------
      !!              ***  FUNCTION isodd  ***
      !!
      !! ** Purpose : Determine whether an interger is odd or not
      !!  
      !!----------------------------------------------------------------------
      INTEGER, INTENT (in) :: a
      LOGICAL              :: odd
      odd = btest(a, 0)
   END FUNCTION isodd
 
   FUNCTION sech( x ) RESULT( seh )
      !!---------------------------------------------------------------------
      !!              ***  FUNCTION sech  ***
      !!
      !! ** Purpose : Compute sech of a real number
      !!  
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ) :: x
      REAL(wp)                :: seh
      seh = 1._wp / COSH(x)     
   END FUNCTION sech

   FUNCTION sigma( vgrid, pk, kmax ) RESULT( ps1 )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE sigma  ***
      !!       
      !! ** Purpose :   provide the analytical function for sigma-coordinate
      !!                (not stretched s-coordinate).
      !!          
      !! ** Method  :   the function provide the non-dimensional position of
      !!                T and W points (i.e. between 0 and 1).
      !!                T-points at integer values (between 1 and jpk)
      !!                W-points at integer values - 1/2 (between 0.5 and
      !!                jpk-0.5)
      !!----------------------------------------------------------------------
      INTEGER, INTENT (in)          ::   pk    ! continuous k coordinate
      INTEGER, INTENT (in)          ::   kmax  ! number of levels
      CHARACTER(LEN=1), INTENT (in) ::   vgrid ! type of vertical grid: T, W
      REAL(wp)                      ::   kindx ! index of T or W points
      REAL(wp)                      ::   ps1   ! value of sigma coordinate (-1 <= ps1 <=0)
      !
      SELECT CASE (vgrid)
        CASE ('T') ! T-points at integer values (between 1 and jpk)
             kindx = REAL(pk,wp)
        CASE ('W') ! W-points at integer values - 1/2 (between 0.5 and jpk-0.5)
             kindx = REAL(pk,wp) - 0.5_wp
        CASE DEFAULT
             WRITE(ctlmes,*) 'No valid vertical grid option selected'
      END SELECT
      ps1 = -(kindx - 0.5) / REAL(kmax-1) ! The surface is at the first W level
                                          ! while the bottom coincides with the
                                          ! last W level
   END FUNCTION sigma

   FUNCTION stretch( s, ca, cb, alpha, kmax, ftype ) RESULT ( pf1 )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE stretch ***
      !!
      !! ** Purpose :   provide the analytical stretching function 
      !!                for s-coordinate.
      !!
      !! ** Method  :   if ftype = 2: Siddorn and Furner 2012
      !!                              analytical function is used
      !!                if ftype = 1: Song and Haidvogel 1994 
      !!                              analytical function is used
      !!                if ftype = 0: Madec et al. 1996
      !!                              analytical function is used
      !!                              (pag 65 of NEMO Manual)
      !!                              Reference  : Madec, Lott, Delecluse and
      !!                              Crepon, 1996. JPO, 26, 1393-1408
      !!
      !!                s MUST be NEGATIVE: -1 <= s <= 0
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in) ::   s              ! not stretched sigma-coordinate
      REAL(wp), INTENT(in) ::   ca             ! surface stretch. coeff
      REAL(wp), INTENT(in) ::   cb             ! bottom stretch. coeff
      REAL(wp), INTENT(in) ::   alpha          ! alpha stretch. coeff for SF12
      INTEGER, INTENT (in) ::   kmax           ! number of levels
      INTEGER,  INTENT(in) ::   ftype          ! type of stretching function
      REAL(wp)             ::   pf1            ! stretched s-coordinate (-1 <= pf1 <=0)
      ! SF12 stretch. funct. parameters
      REAL(wp)             ::   psmth          ! smoothing parameter for transition
                                               ! between shallow and deep areas
      REAL(wp)             ::   za1,za2,za3    ! local variables
      REAL(wp)             ::   zn1,zn2,ps     ! local variables
      REAL(wp)             ::   za,zb,zx       ! local variables
      !!----------------------------------------------------------------------
      SELECT CASE (ftype)
        CASE (0) ! M96  stretching function
           pf1 =   (   TANH( ca * ( s + cb )  ) - TANH( cb * ca ) ) &
            &    * (   COSH( ca )                                   &
            &        + COSH( ca * ( 2.e0 * cb - 1.e0 ) )          ) &
            &    / ( 2._wp * SINH( ca ) )
        CASE (1) ! SH94 stretching function
           IF ( ca == 0 ) THEN      ! uniform sigma
              pf1 = s
           ELSE                     ! stretched sigma
              pf1 = (1._wp - cb) * SINH(ca*s) / SINH(ca) + &
            &       cb * ( ( TANH(ca*(s + 0.5_wp)) - TANH(0.5_wp*ca) ) / &
            &       (2._wp*TANH(0.5_wp*ca)) )
           END IF
        CASE (2) ! SF12 stretching function
           psmth = 1.0_wp ! We consider only the case for efold = 0
           ps  = -s
           zn1 =  1. / REAL(kmax-1)
           zn2 =  1. -  zn1

           za1 = (alpha+2.0_wp)*zn1**(alpha+1.0_wp)-(alpha+1.0_wp)*zn1**(alpha+2.0_wp)
           za2 = (alpha+2.0_wp)*zn2**(alpha+1.0_wp)-(alpha+1.0_wp)*zn2**(alpha+2.0_wp)
           za3 = (zn2**3.0_wp - za2)/( zn1**3.0_wp - za1)

           za  = cb - za3*(ca-za1)-za2
           za  = za/( zn2-0.5_wp*(za2+zn2**2.0_wp) - za3*(zn1-0.5_wp*(za1+zn1**2.0_wp)) )
           zb  = (ca - za1 - za*( zn1-0.5_wp*(za1+zn1**2.0_wp ) ) ) / (zn1**3.0_wp - za1)
           zx  = 1.0_wp-za/2.0_wp-zb

           pf1 = za*(ps*(1.0_wp-ps/2.0_wp))+zb*ps**3.0_wp + &
            &    zx*( (alpha+2.0_wp)*ps**(alpha+1.0_wp) - &
            &         (alpha+1.0_wp)*ps**(alpha+2.0_wp) )
           pf1 = -pf1*psmth+ps*(1.0_wp-psmth)
      END SELECT
       
   END FUNCTION stretch     

   FUNCTION dstretch( s, ca, cb, alpha, kmax, ftype) RESULT( pf1 )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE dstretch ***
      !!
      !! ** Purpose :   provides the 1st derivative of the analytical 
      !!                stretching function dC(sigma)/dsigma.
      !!
      !! ** Method  :   if ftype = 2: Siddorn and Furner 2012
      !!                              analytical function is used
      !!                if ftype = 1: Song and Haidvogel 1994 
      !!                              analytical function is used
      !!                if ftype = 0: Madec et al. 1996
      !!                              analytical function is used
      !!                              (pag 65 of NEMO Manual)
      !!                              Reference  : Madec, Lott, Delecluse and
      !!                              Crepon, 1996. JPO, 26, 1393-1408
      !!
      !!                s MUST be NEGATIVE: -1 <= s <= 0
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ) ::   s           ! not stretched sigma-coordinate
      REAL(wp), INTENT(in)    ::   ca          ! surface stretch. coeff
      REAL(wp), INTENT(in)    ::   cb          ! bottom stretch. coeff
      REAL(wp), INTENT(in)    ::   alpha       ! alpha stretch. coeff for SF12
      INTEGER, INTENT (in)    ::   kmax        ! number of levels
      INTEGER,  INTENT(in   ) ::   ftype       ! type of stretching function
      REAL(wp)                ::   pf1         ! first derivative
      ! SF12 stretch. funct. parameters
      REAL(wp)                ::   psmth       ! smoothing parameter for transition
                                               ! between shallow and deep areas
      REAL(wp)                ::   za1,za2,za3 ! local variables
      REAL(wp)                ::   zn1,zn2,ps  ! local variables
      REAL(wp)                ::   za,zb,zx    ! local variables
      REAL(wp)                ::   zt1,zt2,zt3 ! local variables
      !!----------------------------------------------------------------------
      
      SELECT CASE (ftype)

        CASE (0) ! M96  stretching function
           pf1 =  ( ca * ( COSH(ca*(2._wp * cb - 1._wp)) + COSH(ca)) * &
                     sech(ca * (s+cb))**2 ) / (2._wp * SINH(ca))
        CASE (1) ! SH94 stretching function
           IF ( ca == 0 ) then      ! uniform sigma
              pf1 = 1._wp / REAL(kmax,wp)
           ELSE                     ! stretched sigma
              !pf1 = (1._wp - cb) * ca * COSH(ca*s) / (SINH(ca) * REAL(kmax,wp)) + &
              !       cb * ca * &
              !      (sech((s+0.5_wp)*ca)**2) / (2._wp * TANH(0.5_wp*ca) * REAL(kmax,wp))
              pf1 = (1._wp - cb) * ca * COSH(ca*s) / SINH(ca) + cb * ca * &
                    (sech((s+0.5_wp)*ca)**2) / (2._wp * TANH(0.5_wp*ca))
           END IF
        CASE (2) ! SF12 stretching function
           ps  = -s
           zn1 =  1. / REAL(kmax-1)
           zn2 =  1. -  zn1

           za1 = (alpha+2.0_wp)*zn1**(alpha+1.0_wp)-(alpha+1.0_wp)*zn1**(alpha+2.0_wp)
           za2 = (alpha+2.0_wp)*zn2**(alpha+1.0_wp)-(alpha+1.0_wp)*zn2**(alpha+2.0_wp)
           za3 = (zn2**3.0_wp - za2)/( zn1**3.0_wp - za1)

           za  = cb - za3*(ca-za1)-za2
           za  = za/( zn2-0.5_wp*(za2+zn2**2.0_wp) - za3*(zn1-0.5_wp*(za1+zn1**2.0_wp)) )
           zb  = (ca - za1 - za*( zn1-0.5_wp*(za1+zn1**2.0_wp ) ) ) / (zn1**3.0_wp - za1)
           zx  = (alpha+2.0_wp)*(alpha+1.0_wp)

           zt1 = 0.5_wp*za*(ps-1._wp)*((ps**2.0_wp + 3._wp*ps + 2._wp)*ps**alpha - 2._wp)
           zt2 = zb*(zx*ps**(alpha+1.0_wp) - zx*ps**(alpha) + 3._wp*ps**2._wp)
           zt3 = zx*(ps-1._wp)*ps**(alpha)

           pf1 = zt1 + zt2 - zt3

      END SELECT

   END FUNCTION dstretch

   FUNCTION z_mes(dep_top, dep_bot, Cs, s, hc) RESULT( z )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE z_mes ***
      !!
      !! ** Purpose :   provide the analytical trasformation from the 
      !!                computational space (mes-coordinate) to the  
      !!                physical space (depth z)
      !!
      !!    N.B.: z is downward positive defined as well as envelope surfaces.
      !!          Therefore, Cs MUST be positive, meaning 0. <= Cs <= 1.
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ) :: dep_top  ! shallower envelope
      REAL(wp), INTENT(in   ) :: dep_bot  ! deeper envelope
      REAL(wp), INTENT(in   ) :: Cs       ! stretched s coordinate   
      REAL(wp), INTENT(in   ) :: s        ! not stretched s coordinate
      REAL(wp), INTENT(in   ) :: hc       ! critical depth
      REAL                    :: z        ! downward positive depth
      !!----------------------------------------------------------------------
      ! 
      z = dep_top + hc*s + Cs*(dep_bot - hc - dep_top)

   END FUNCTION z_mes

   FUNCTION dzdmes(dep_top, dep_bot, dCds, hc) RESULT( d1z )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE Dzdmes ***
      !!
      !! ** Purpose :   provide the 1st derivative of the analytical 
      !!                trasformation from the computational space 
      !!                (mes-coordinate) to the physical space (depth z)
      !!
      !!    N.B.: z is downward positive defined as well as envelope surfaces.
      !!          Therefore, Cs MUST be positive, meaning 0. <= Cs <= 1.
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   ) :: dep_top  ! shallower envelope
      REAL(wp), INTENT(in   ) :: dep_bot  ! deeper envelope
      REAL(wp), INTENT(in   ) :: dCds     ! 1st derivative of the 
                                          ! stretched s coordinate   
      REAL(wp), INTENT(in   ) :: hc       ! critical depth
      REAL(wp)                :: d1z      ! 1st derivative of downward
                                          ! positive depth
      !!----------------------------------------------------------------------
      ! 
      d1z = hc + dCds * (dep_bot - hc - dep_top)
 
   END FUNCTION Dzdmes

   FUNCTION cub_spl(tau, d, n, ibcbeg, ibcend) RESULT ( c )
      !!----------------------------------------------------------------------
      !!
      !! CUBSPL defines an interpolatory cubic spline.
      !!
      !! Discussion:
      !!
      !!    A tridiagonal linear system for the unknown slopes S(I) of
      !!    F at TAU(I), I=1,..., N, is generated and then solved by Gauss
      !!    elimination, with S(I) ending up in C(2,I), for all I.
      !!
      !! Author: Carl de Boor
      !!
      !! Reference: Carl de Boor, Practical Guide to Splines,
      !!             Springer, 2001, ISBN: 0387953663, LC: QA1.A647.v27.
      !!
      !! Parameters:
      !!
      !!    Input: 
      !!            TAU(N) : the abscissas or X values of the data points.  
      !!                     The entries of TAU are assumed to be strictly 
      !!                     increasing.
      !!
      !!            N      : the number of data points.  N is assumed to be 
      !!                     at least 2.
      !! 
      !!            IBCBEG : boundary condition indicator at TAU(1).
      !!                     = 0 no boundary condition at TAU(1) is given.
      !!                         In this case, the "not-a-knot condition" 
      !!                         is used.
      !!                     = 1 the 1st derivative at TAU(1) is equal to 
      !!                         the input value D(2,1).
      !!                     = 2 the 2nd derivative at TAU(1) is equal to 
      !!                         the input value D(2,1).
      !!
      !!            IBCEND : boundary condition indicator at TAU(N).
      !!                     = 0 no boundary condition at TAU(N) is given.
      !!                         In this case, the "not-a-knot condition" 
      !!                         is used.
      !!                     = 1 the 1st derivative at TAU(N) is equal to 
      !!                         the input value D(2,2).
      !!                     = 2 the 2nd derivative at TAU(N) is equal to 
      !!                         the input value D(2,2).                                 
      !!
      !!            D(1,1): value of the function at TAU(1)
      !!            D(1,1): value of the function at TAU(N)
      !!            D(2,1): if IBCBEG is 1 (2) it is the value of the
      !!                    1st (2nd) derivative at TAU(1)
      !!            D(2,2): if IBCBEG is 1 (2) it is the value of the
      !!                    1st (2nd) derivative at TAU(N) 
      !!
      !!    Output: 
      !!            C(4,N) : contains the polynomial coefficients of the 
      !!                     cubic interpolating spline.
      !!                     In the interval interval (TAU(I), TAU(I+1)), 
      !!                     the spline F is given by
      !!
      !!                          F(X) = 
      !!                                  C(1,I) + 
      !!                                  C(2,I) * H +
      !!                                  C(3,I) * H^2 / 2 + 
      !!                                  C(4,I) * H^3 / 6.
      !!
      !!                     where H = X - TAU(I).  
      !!
      !!----------------------------------------------------------------------
      INTEGER,  INTENT(in   ) :: n
      REAL(wp), INTENT(in   ) :: tau(n)
      REAL(wp), INTENT(in   ) :: d(2,2)
      INTEGER,  INTENT(in   ) :: ibcbeg, ibcend
      REAL(wp)                :: c(4,n)
      REAL(wp)                :: divdf1
      REAL(wp)                :: divdf3
      REAL(wp)                :: dtau
      REAL(wp)                :: g
      INTEGER(wp)             :: i
      !!----------------------------------------------------------------------
      !  Initialise c and copy d values to c
      c(:,:) = 0.0D+00
      c(1,1) = d(1,1)
      c(1,n) = d(1,2)
      c(2,1) = d(2,1)
      c(2,n) = d(2,2)
      ! 
      !  C(3,*) and C(4,*) are used initially for temporary storage.
      !  Store first differences of the TAU sequence in C(3,*).
      !  Store first divided difference of data in C(4,*).
      DO i = 2, n
         c(3,i) = tau(i) - tau(i-1)
         c(4,i) = ( c(1,i) - c(1,i-1) ) / c(3,i)
      END DO

      !  Construct the first equation from the boundary condition
      !  at the left endpoint, of the form:
      !
      !    C(4,1) * S(1) + C(3,1) * S(2) = C(2,1)
      !
      !  IBCBEG = 0: Not-a-knot
      IF ( ibcbeg == 0 ) THEN

         IF ( n <= 2 ) THEN
            c(4,1) = 1._wp
            c(3,1) = 1._wp
            c(2,1) = 2._wp * c(4,2)
         ELSE
            c(4,1) = c(3,3)
            c(3,1) = c(3,2) + c(3,3)
            c(2,1) = ( ( c(3,2) + 2._wp * c(3,1) ) * c(4,2) &
                     * c(3,3) + c(3,2)**2 * c(4,3) ) / c(3,1)
         END IF

      !  IBCBEG = 1: derivative specified.
      ELSE IF ( ibcbeg == 1 ) then

         c(4,1) = 1._wp
         c(3,1) = 0._wp

      !  IBCBEG = 2: Second derivative prescribed at left end.
      ELSE IF ( ibcbeg == 2 ) then

         c(4,1) = 2._wp
         c(3,1) = 1._wp
         c(2,1) = 3._wp * c(4,2) - c(3,2) / 2._wp * c(2,1)

      ELSE
         WRITE(ctlmes,*) 'CUBSPL - Error, invalid IBCBEG input option!'
         CALL ctl_stop( ctlmes )
      END IF
  
      !  If there are interior knots, generate the corresponding
      !  equations and carry out the forward pass of Gauss,
      !  elimination after which the I-th equation reads:
      !
      !    C(4,I) * S(I) + C(3,I) * S(I+1) = C(2,I).

      IF ( n > 2 ) THEN

         DO i = 2, n-1
            g = -c(3,i+1) / c(4,i-1)
            c(2,i) = g * c(2,i-1) + 3._wp * &
                     ( c(3,i) * c(4,i+1) + c(3,i+1) * c(4,i) )
            c(4,i) = g * c(3,i-1) + 2._wp * ( c(3,i) + c(3,i+1))
         END DO

         !  Construct the last equation from the second boundary,
         !  condition of the form
         !
         !    -G * C(4,N-1) * S(N-1) + C(4,N) * S(N) = C(2,N)
         !
         !  If 1st der. is prescribed at right end ( ibcend == 1 ), 
         !  one can go directly to back-substitution, since the C
         !  array happens to be set up just right for it at this 
         !  point.

         IF ( ibcend < 1 ) THEN
            !  Not-a-knot and 3 <= N, and either 3 < N or also 
            !  not-a-knot at left end point.
            IF ( n /= 3 .OR. ibcbeg /= 0 ) THEN
               g      = c(3,n-1) + c(3,n)
               c(2,n) = ( ( c(3,n) + 2._wp * g ) * c(4,n) * c(3,n-1) &
                        + c(3,n)**2 * ( c(1,n-1) - c(1,n-2) ) / &
                        c(3,n-1) ) / g
               g      = - g / c(4,n-1)
               c(4,n) = c(3,n-1)
               c(4,n) = c(4,n) + g * c(3,n-1)
               c(2,n) = ( g * c(2,n-1) + c(2,n) ) / c(4,n)
            ELSE
            !  N = 3 and not-a-knot also at left.
               c(2,n) = 2._wp * c(4,n)
               c(4,n) = 1._wp
               g      = -1._wp / c(4,n-1)
               c(4,n) = c(4,n) - c(3,n-1) / c(4,n-1)
               c(2,n) = ( g * c(2,n-1) + c(2,n) ) / c(4,n)
            END IF

         ELSE IF ( ibcend == 2 ) THEN
         !  IBCEND = 2: Second derivative prescribed at right endpoint.
               c(2,n) = 3._wp * c(4,n) + c(3,n) / 2._wp * c(2,n)
               c(4,n) = 2._wp
               g      = -1._wp / c(4,n-1)
               c(4,n) = c(4,n) - c(3,n-1) / c(4,n-1)
               c(2,n) = ( g * c(2,n-1) + c(2,n) ) / c(4,n)
         END IF

      ELSE
         !  N = 2 (assumed to be at least equal to 2!).

         IF ( ibcend == 2  ) THEN

            c(2,n) = 3._wp * c(4,n) + c(3,n) / 2._wp * c(2,n)
            c(4,n) = 2._wp
            g      = -1._wp / c(4,n-1)
            c(4,n) = c(4,n) - c(3,n-1) / c(4,n-1)
            c(2,n) = ( g * c(2,n-1) + c(2,n) ) / c(4,n)

         ELSE IF ( ibcend == 0 .AND. ibcbeg /= 0 ) THEN

            c(2,n) = 2._wp * c(4,n)
            c(4,n) = 1._wp
            g      = -1._wp / c(4,n-1)
            c(4,n) = c(4,n) - c(3,n-1) / c(4,n-1)
            c(2,n) = ( g * c(2,n-1) + c(2,n) ) / c(4,n)

         ELSE IF ( ibcend == 0 .AND. ibcbeg == 0 ) THEN

            c(2,n) = c(4,n)

         END IF

      END IF
  
      !  Back solve the upper triangular system 
      !
      !    C(4,I) * S(I) + C(3,I) * S(I+1) = B(I)
      !
      !  for the slopes C(2,I), given that S(N) is already known.
      ! 
      DO i = n-1, 1, -1
         c(2,i) = ( c(2,i) - c(3,i) * c(2,i+1) ) / c(4,i)
      END DO

      !  Generate cubic coefficients in each interval, that is, the
      !  derivatives at its left endpoint, from value and slope at its
      !  endpoints.
      DO i = 2, n
         dtau     = c(3,i)
         divdf1   = ( c(1,i) - c(1,i-1) ) / dtau
         divdf3   = c(2,i-1) + c(2,i) - 2._wp * divdf1
         c(3,i-1) = 2._wp * ( divdf1 - c(2,i-1) - divdf3 ) / dtau
         c(4,i-1) = 6._wp * divdf3 / dtau**2
      END DO

   END FUNCTION cub_spl

   FUNCTION z_cub_spl(s, tau, c) RESULT ( z_cs )
      !----------------------------------------------------------------------
      !                 ***  function z_cub_spl ***
      !
      ! ** Purpose : evaluate the cubic spline F in the interval 
      !              (TAU(I), TAU(I+1)). F is given by 
      !              
      !
      !           F(X) = C1 + C2*H + (C3*H^2)/2 + (C4*H^3)/6
      !
      !              where H = S-TAU(I).
      !
      !              s MUST be positive: 0 <= s <= 1
      !---------------------------------------------------------------------
      !INTEGER, PARAMETER      :: nn = 2
      REAL(wp), INTENT(in   ) :: s
      REAL(wp), INTENT(in   ) :: tau(2)
      REAL(wp), INTENT(in   ) :: c(4,2)
      REAL(wp)                :: z_cs
      REAL(wp)                :: c1, c2, c3, c4
      REAL(wp)                :: H
      !---------------------------------------------------------------------
      c1 = c(1,1)
      c2 = c(2,1)
      c3 = c(3,1)
      c4 = c(4,1)

      H    = s - tau(1)

      z_cs = c1 + c2 * H + (c3 * H**2._wp)/2._wp + (c4 * H**3._wp)/6._wp

   END FUNCTION z_cub_spl  
  
END MODULE dommes
 
