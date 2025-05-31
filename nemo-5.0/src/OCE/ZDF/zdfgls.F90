MODULE zdfgls
   !!======================================================================
   !!                       ***  MODULE  zdfgls  ***
   !! Ocean physics:  vertical mixing coefficient computed from the gls
   !!                 turbulent closure parameterization
   !!======================================================================
   !! History :  3.0  !  2009-09  (G. Reffray)  Original code
   !!            3.3  !  2010-10  (C. Bricaud)  Add in the reference
   !!            4.0  !  2017-04  (G. Madec)  remove CPP keys & avm at t-point only
   !!             -   !  2017-05  (G. Madec)  add top friction as boundary condition
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_gls       : update momentum and tracer Kz from a gls scheme
   !!   zdf_gls_init  : initialization, namelist read, and parameters control
   !!   gls_rst       : read/write gls restart in ocean restart file
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE zdfdrg  , ONLY : ln_drg_OFF            ! top/bottom free-slip flag
   USE zdfdrg  , ONLY : r_z0_top , r_z0_bot   ! top/bottom roughness
   USE zdfdrg  , ONLY : rCdU_top , rCdU_bot   ! top/bottom friction
   USE sbc_oce        ! surface boundary condition: ocean
   USE phycst         ! physical constants
   USE zdfmxl         ! mixed layer
   USE sbcwave , ONLY : hsw   ! significant wave height
#if defined key_si3
   USE ice, ONLY: hm_i, h_i
#endif
#if defined key_cice
   USE sbc_ice, ONLY: h_i
#endif
   !
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! MPP manager
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_gls        ! called in zdfphy
   PUBLIC   zdf_gls_init   ! called in zdfphy
   PUBLIC   gls_rst        ! called in zdfphy

   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   hmxl_n    !: now mixing length

   !                              !! ** Namelist  namzdf_gls  **
   LOGICAL  ::   ln_length_lim     ! use limit on the dissipation rate under stable stratification (Galperin et al. 1988)
   LOGICAL  ::   ln_sigpsi         ! Activate Burchard (2003) modification for k-eps closure & wave breaking mixing
   INTEGER  ::   nn_mxlice         ! type of scaling under sea-ice (=0/1/2/3)
   INTEGER  ::   nn_bc_surf        ! surface boundary condition (=0/1)
   INTEGER  ::   nn_bc_bot         ! bottom boundary condition (=0/1)
   INTEGER  ::   nn_z0_met         ! Method for surface roughness computation
   INTEGER  ::   nn_z0_ice         ! Roughness accounting for sea ice
   INTEGER  ::   nn_stab_func      ! stability functions G88, KC or Canuto (=0/1/2)
   INTEGER  ::   nn_clos           ! closure 0/1/2/3 MY82/k-eps/k-w/gen
   REAL(wp) ::   rn_clim_galp      ! Holt 2008 value for k-eps: 0.267
   REAL(wp) ::   rn_epsmin         ! minimum value of dissipation (m2/s3)
   REAL(wp) ::   rn_emin           ! minimum value of TKE (m2/s2)
   REAL(wp) ::   rn_charn          ! Charnock constant for surface breaking waves mixing : 1400. (standard) or 2.e5 (Stacey value)
   REAL(wp) ::   rn_crban          ! Craig and Banner constant for surface breaking waves mixing
   REAL(wp) ::   rn_hsro           ! Minimum surface roughness
   REAL(wp) ::   rn_hsri           ! Ice ocean roughness
   REAL(wp) ::   rn_frac_hs        ! Fraction of wave height as surface roughness (if nn_z0_met > 1)

   REAL(wp) ::   rcm_sf        =  0.73_wp     ! Shear free turbulence parameters
   REAL(wp) ::   ra_sf         = -2.0_wp      ! Must be negative -2 < ra_sf < -1
   REAL(wp) ::   rl_sf         =  0.2_wp      ! 0 <rl_sf<vkarmn
   REAL(wp) ::   rghmin        = -0.28_wp
   REAL(wp) ::   rgh0          =  0.0329_wp
   REAL(wp) ::   rghcri        =  0.03_wp
   REAL(wp) ::   ra1           =  0.92_wp
   REAL(wp) ::   ra2           =  0.74_wp
   REAL(wp) ::   rb1           = 16.60_wp
   REAL(wp) ::   rb2           = 10.10_wp
   REAL(wp) ::   re2           =  1.33_wp
   REAL(wp) ::   rl1           =  0.107_wp
   REAL(wp) ::   rl2           =  0.0032_wp
   REAL(wp) ::   rl3           =  0.0864_wp
   REAL(wp) ::   rl4           =  0.12_wp
   REAL(wp) ::   rl5           = 11.9_wp
   REAL(wp) ::   rl6           =  0.4_wp
   REAL(wp) ::   rl7           =  0.0_wp
   REAL(wp) ::   rl8           =  0.48_wp
   REAL(wp) ::   rm1           =  0.127_wp
   REAL(wp) ::   rm2           =  0.00336_wp
   REAL(wp) ::   rm3           =  0.0906_wp
   REAL(wp) ::   rm4           =  0.101_wp
   REAL(wp) ::   rm5           = 11.2_wp
   REAL(wp) ::   rm6           =  0.4_wp
   REAL(wp) ::   rm7           =  0.0_wp
   REAL(wp) ::   rm8           =  0.318_wp
   REAL(wp) ::   rtrans        =  0.1_wp
   REAL(wp) ::   rc02, rc02r, rc03, rc04                          ! coefficients deduced from above parameters
   REAL(wp) ::   rsbc_tke1, rsbc_tke2, rfact_tke                  !     -           -           -        -
   REAL(wp) ::   rsbc_psi1, rsbc_psi2, rfact_psi                  !     -           -           -        -
   REAL(wp) ::   rsbc_zs1, rsbc_zs2                               !     -           -           -        -
   REAL(wp) ::   rc0, rc2, rc3, rf6, rcff, rc_diff                !     -           -           -        -
   REAL(wp) ::   rs0, rs1, rs2, rs4, rs5, rs6                     !     -           -           -        -
   REAL(wp) ::   rd0, rd1, rd2, rd3, rd4, rd5                     !     -           -           -        -
   REAL(wp) ::   rsc_tke, rsc_psi, rpsi1, rpsi2, rpsi3, rsc_psi0  !     -           -           -        -
   REAL(wp) ::   rpsi3m, rpsi3p, rpp, rmm, rnn                    !     -           -           -        -
   !
   REAL(wp) ::   r2_3 = 2._wp/3._wp   ! constant=2/3

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_gls_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION zdf_gls_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( hmxl_n(A2D(0),jpk), STAT=zdf_gls_alloc )
         !
      CALL mpp_sum ( 'zdfgls', zdf_gls_alloc )
      IF( zdf_gls_alloc /= 0 )   CALL ctl_stop( 'STOP', 'zdf_gls_alloc: failed to allocate arrays' )
   END FUNCTION zdf_gls_alloc


   SUBROUTINE zdf_gls( kt, Kbb, Kmm, p_sh2, p_avm, p_avt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE zdf_gls  ***
      !!
      !! ** Purpose :   Compute the vertical eddy viscosity and diffusivity
      !!              coefficients using the GLS turbulent closure scheme.
      !!----------------------------------------------------------------------
      USE zdf_oce , ONLY : en, avtb, avmb   ! ocean vertical physics
      !!
      INTEGER                         , INTENT(in   ) ::   kt             ! ocean time step
      INTEGER                         , INTENT(in   ) ::   Kbb, Kmm       ! ocean time level indices
      REAL(wp), DIMENSION(A2D(0) ,jpk), INTENT(in   ) ::   p_sh2          ! shear production term
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   p_avm          ! vertical eddy viscosity (w-points)
      REAL(wp), DIMENSION(A2D(0) ,jpk), INTENT(inout) ::   p_avt          ! vertical eddy diffusivity (w-points)
      !
      INTEGER  ::   ji, jj, jk                          ! dummy loop arguments
      INTEGER  ::   ibot, ibotm1                        ! local integers
      INTEGER  ::   itop, itopp1                        !   -       -
      REAL(wp) ::   zesh2, zsigpsi, zcoef, zex1 , zex2  ! local scalars
      REAL(wp) ::   ztx2, zty2, zup, zdown, zcof, zdir  !   -      -
      REAL(wp) ::   zratio, zrn2, zflxb, zsh, z_en      !   -      -
      REAL(wp) ::   zprod, zbuoy, zdiss, zzdiss, zsm    !   -      -
      REAL(wp) ::   zgh, zgm, zshr, zsqen, zavt, zavm   !   -      -
      REAL(wp) ::   zmsku, zmskv, zkar, zdep            !   -      -
      REAL(wp) ::   zflxs                                         ! Turbulence fluxed induced by internal waves
      REAL(wp), DIMENSION(T1Di(0))     ::   zhsro                 ! Surface roughness (surface waves)
      REAL(wp), DIMENSION(T1Di(0))     ::   zice_fra              ! Tapering of wave breaking under sea ice
      REAL(wp), DIMENSION(T1Di(0))     ::   zstar2_surf           ! Squared surface velocity scale at T-points
      REAL(wp), DIMENSION(T1Di(0))     ::   zstar2_top            ! Squared top     velocity scale at T-points
      REAL(wp), DIMENSION(T1Di(0))     ::   zstar2_bot            ! Squared bottom  velocity scale at T-points
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zeb                   ! tke at time before
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zhmxl_b               ! mixing length at time before
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zeps                   ! dissipation rate
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zwall_psi             ! Wall function use in the wb case (ln_sigpsi)
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zpsi                  ! psi at time now
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zd_lw, zd_up, zdiag   ! lower, upper  and diagonal of the matrix
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zstt, zstm            ! stability function on tracer and momentum
      REAL(wp), DIMENSION(T1Di(0),jpk) ::   zwall                 ! wall function
      !!--------------------------------------------------------------------
      !
      IF( nn_z0_ice == 0 ) zice_fra(:) = 0._wp             ! No attenuation of TKE due to sea ice
      IF( nn_z0_met == 0 ) zhsro(:) = rn_hsro              ! Constant surface roughness length
      IF( ln_drg_OFF ) THEN                                ! No top/bottom friction
         zstar2_top(:) = 0._wp
         zstar2_bot(:) = 0._wp
      ENDIF
      !
      !                                               ! ================= !
      DO_1Dj( 0, 0 )                                  !  i-k slices loop  !
         !                                            ! ================= !
         SELECT CASE ( nn_z0_ice )
            CASE( 1 )   ;   zice_fra(:) =        TANH( fr_i(T1Di(0),jj) * 10._wp )
            CASE( 2 )   ;   zice_fra(:) =              fr_i(T1Di(0),jj)
            CASE( 3 )   ;   zice_fra(:) = MIN( 4._wp * fr_i(T1Di(0),jj) , 1._wp )
         END SELECT

         ! Compute surface, top and bottom friction at T-points
         DO_1Di( 0, 0 )          !==  surface ocean friction
            zstar2_surf(ji) = r1_rho0 * taum(ji,jj) * tmask(ji,jj,1)   ! surface friction
         END_1D
         !
!!gm Rq we may add here r_ke0(_top/_bot) ?  ==>> think about that...
         !
         IF( .NOT.ln_drg_OFF ) THEN     !== top/bottom friction   (explicit before friction)
            DO_1Di( 0, 0 )                 ! bottom friction (explicit before friction)
               zmsku = 0.5_wp * ( 2._wp - umask(ji-1,jj,mbkt(ji,jj)) * umask(ji,jj,mbkt(ji,jj)) )
               zmskv = 0.5_wp * ( 2._wp - vmask(ji,jj-1,mbkt(ji,jj)) * vmask(ji,jj,mbkt(ji,jj)) )     ! (CAUTION: CdU<0)
               zstar2_bot(ji) = - rCdU_bot(ji,jj) * SQRT(  ( zmsku*( uu(ji,jj,mbkt(ji,jj),Kbb)+uu(ji-1,jj,mbkt(ji,jj),Kbb) ) )**2  &
                  &                                      + ( zmskv*( vv(ji,jj,mbkt(ji,jj),Kbb)+vv(ji,jj-1,mbkt(ji,jj),Kbb) ) )**2  )
            END_1D
            IF( ln_isfcav ) THEN
               DO_1Di( 0, 0 )              ! top friction
                  zmsku = 0.5_wp * ( 2. - umask(ji-1,jj,mikt(ji,jj)) * umask(ji,jj,mikt(ji,jj)) )
                  zmskv = 0.5_wp * ( 2. - vmask(ji,jj-1,mikt(ji,jj)) * vmask(ji,jj,mikt(ji,jj)) )     ! (CAUTION: CdU<0)
                  zstar2_top(ji) = - rCdU_top(ji,jj) * SQRT(  ( zmsku*( uu(ji,jj,mikt(ji,jj),Kbb)+uu(ji-1,jj,mikt(ji,jj),Kbb) ) )**2  &
                     &                                      + ( zmskv*( vv(ji,jj,mikt(ji,jj),Kbb)+vv(ji,jj-1,mikt(ji,jj),Kbb) ) )**2  )
               END_1D
            ENDIF
         ENDIF

         SELECT CASE ( nn_z0_met )      !==  Set surface roughness length  ==!
            CASE ( 1 )                     ! Standard Charnock formula
               DO_1Di( 0, 0 )
                  zhsro(ji) = MAX( rsbc_zs1 * zstar2_surf(ji) , rn_hsro )
               END_1D
            CASE ( 2 )                     ! Roughness formulae according to Rascle et al., Ocean Modelling (2008)
!!gm faster coding : the 2 comment lines should be used
!!gm         zcof = 2._wp * 0.6_wp / 28._wp
!!gm         zdep(:,:)  = 30._wp * TANH(  zcof/ SQRT( MAX(zstar2_surf(:,:),rsmall) )  )   ! Wave age (eq. 10)
               DO_1Di( 0, 0 )
                  zcof = 30.*TANH( 2.*0.3/(28.*SQRT(MAX(zstar2_surf(ji),rsmall))) )       ! Wave age (eq. 10)
                  zhsro(ji) = MAX(rsbc_zs2 * zstar2_surf(ji) * zcof**1.5, rn_hsro)        ! zhsro = rn_frac_hs * Hsw (eq. 11)
               END_1D
            CASE ( 3 )                     ! Roughness given by the wave model (coupled or read in file)
               DO_1Di( 0, 0 )
                  zhsro(ji) = MAX(rn_frac_hs * hsw(ji,jj), rn_hsro)   ! (rn_frac_hs=1.6 see Eq. (5) of Rascle et al. 2008 )
               END_1D
         END SELECT
         !
         ! adapt roughness where there is sea ice
         SELECT CASE( nn_mxlice )       !==  Type of scaling under sea-ice  ==!
            !
            CASE( 1 )                      ! scaling with constant sea-ice roughness
               DO_1Di( 0, 0 )
                  zhsro(ji) = ( (1._wp-zice_fra(ji)) * zhsro(ji) + zice_fra(ji) * rn_hsri )*tmask(ji,jj,1)  + (1._wp - tmask(ji,jj,1))*rn_hsro
               END_1D
               !
            CASE( 2 )                      ! scaling with mean sea-ice thickness
#if defined key_si3
               DO_1Di( 0, 0 )
                  zhsro(ji) = ( (1._wp-zice_fra(ji)) * zhsro(ji) + zice_fra(ji) * hm_i(ji,jj) )*tmask(ji,jj,1)  + (1._wp - tmask(ji,jj,1))*rn_hsro
               END_1D
#endif
               !
            CASE( 3 )                      ! scaling with max sea-ice thickness
#if defined key_si3 || defined key_cice
               DO_1Di( 0, 0 )
                  zhsro(ji) = ( (1._wp-zice_fra(ji)) * zhsro(ji) + zice_fra(ji) * MAXVAL(h_i(ji,jj,:)) )*tmask(ji,jj,1)  + (1._wp - tmask(ji,jj,1))*rn_hsro
               END_1D
#endif
            !
         END SELECT
         !
         DO_2Dik( 0, 0, 2, jpkm1, 1 )   !==  Compute dissipation rate  ==!
            zeps(ji,jk)  = rc03 * en(ji,jj,jk) * SQRT( en(ji,jj,jk) ) / hmxl_n(ji,jj,jk)
         END_2D

         ! Save tke at before time step
         DO_2Dik( 0, 0, 1, jpk, 1 )
            zeb    (ji,jk) = en    (ji,jj,jk)
            zhmxl_b(ji,jk) = hmxl_n(ji,jj,jk)
         END_2D

!!gm tmask or wmask ????
         IF( nn_clos == 0 ) THEN        ! Mellor-Yamada
            DO_2Dik( 0, 0, 2, jpkm1, 1 )
               zup   = hmxl_n(ji,jj,jk) * gdepw(ji,jj,mbkt(ji,jj)+1,Kmm)
               zdown = vkarmn * gdepw(ji,jj,jk,Kmm) * ( -gdepw(ji,jj,jk,Kmm) + gdepw(ji,jj,mbkt(ji,jj)+1,Kmm) )
               zcoef = ( zup / MAX( zdown, rsmall ) )
               zwall(ji,jk) = ( 1._wp + re2 * zcoef*zcoef ) * tmask(ji,jj,jk)
            END_2D
         ELSE
            DO_2Dik( 0, 0, 2, jpkm1, 1 )
               zwall(ji,jk) = tmask(ji,jj,jk)
            END_2D
         ENDIF

         !!---------------------------------!!
         !!   Equation to prognostic k      !!
         !!---------------------------------!!
         !
         ! Now Turbulent kinetic energy (output in en)
         ! -------------------------------
         ! Resolution of a tridiagonal linear system by a "methode de chasse"
         ! computation from level 2 to jpkm1  (e(1) computed after and e(jpk)=0 ).
         ! The surface boundary condition are set after
         ! The bottom boundary condition are also set after. In standard e(bottom)=0.
         ! zdiag : diagonal zd_up : upper diagonal zd_lw : lower diagonal
         ! Warning : after this step, en : right hand side of the matrix

         DO_2Dik( 0, 0, 2, jpkm1, 1 )
            !
            zbuoy = - p_avt(ji,jj,jk) * rn2(ji,jj,jk)                                                    ! stratif. destruction
            !
            zdiss = zeps(ji,jk)                                                                          ! dissipation
            !
            zdir = 0.5_wp + SIGN( 0.5_wp, p_sh2(ji,jj,jk) + zbuoy )  ! zdir =1(=0) if shear(ji,jj,jk)+buoy >0(<0)
            !
            zesh2 = zdir*(p_sh2(ji,jj,jk)+zbuoy)+(1._wp-zdir)*p_sh2(ji,jj,jk)                 ! production term
            zzdiss = zdir*(zdiss/en(ji,jj,jk))  +(1._wp-zdir)*(zdiss-zbuoy)/en(ji,jj,jk)      ! dissipation term
!!gm better coding, identical results
!               zesh2 =   p_sh2(ji,jj,jk) + zdir*buoy               ! production term
!               zdiss = ( diss - (1._wp-zdir)*buoy ) / en(ji,jj,jk) ! dissipation term
!!gm
            !
            ! Compute a wall function from 1. to rsc_psi*zwall/rsc_psi0
            ! Note that as long that Dirichlet boundary conditions are NOT set at the first and last levels (GOTM style)
            ! there is no need to set a boundary condition for zwall_psi at the top and bottom boundaries.
            ! Otherwise, this should be rsc_psi/rsc_psi0
            IF( ln_sigpsi ) THEN
               zsigpsi = MIN( 1._wp, zesh2 / zeps(ji,jk) )           ! 0. <= zsigpsi <= 1.
               zwall_psi(ji,jk) = rsc_psi /   &
                  &     (  zsigpsi * rsc_psi + (1._wp-zsigpsi) * rsc_psi0 / MAX( zwall(ji,jk), 1._wp )  )
            ELSE
               zwall_psi(ji,jk) = 1._wp
            ENDIF
            !
            ! building the matrix
            zcof = rfact_tke * tmask(ji,jj,jk)
            !                                        ! lower diagonal, in fact not used for jk = 2 (see surface conditions)
            zd_lw(ji,jk) = zcof * ( p_avm(ji,jj,jk  ) + p_avm(ji,jj,jk-1) )   &
               &                 / ( e3t(ji,jj,jk-1,Kmm) * e3w(ji,jj,jk,Kmm) )
            !                                        ! upper diagonal, in fact not used for jk = ibotm1 (see bottom conditions)
            zd_up(ji,jk) = zcof * ( p_avm(ji,jj,jk+1) + p_avm(ji,jj,jk  ) )   &
               &                 / ( e3t(ji,jj,jk  ,Kmm) * e3w(ji,jj,jk,Kmm) )
            !                                        ! diagonal
            zdiag(ji,jk) = 1._wp - zd_lw(ji,jk) - zd_up(ji,jk)  + rn_Dt * zzdiss * wmask(ji,jj,jk)
            !                                        ! right hand side in en
            en(ji,jj,jk) = en(ji,jj,jk) + rn_Dt * zesh2 * wmask(ji,jj,jk)
         END_2D
         !
         DO_1Di( 0, 0 )
            zdiag(ji,jpk) = 1._wp
            !
            ! Set surface condition on zwall_psi (1 at the bottom)
            zwall_psi(ji, 1 ) = zwall_psi(ji,2)
            zwall_psi(ji,jpk) = 1._wp
         END_1D
         !
         ! Surface boundary condition on tke
         ! ---------------------------------
         !
         SELECT CASE ( nn_bc_surf )
            !
            CASE ( 0 )             ! Dirichlet boundary condition (set e at k=1 & 2)
               DO_1Di( 0, 0 )
                  ! First level
                  en   (ji,jj,1) = MAX(  rn_emin , rc02r * zstar2_surf(ji) * (1._wp + (1._wp-zice_fra(ji))*rsbc_tke1)**r2_3  )
                  zd_lw(ji,   1) = en(ji,jj,1)
                  zd_up(ji,   1) = 0._wp
                  zdiag(ji,   1) = 1._wp
                  !
                  ! One level below
                  en   (ji,jj,2) =  MAX( rn_emin , rc02r * zstar2_surf(ji) * (1._wp + (1._wp-zice_fra(ji))*rsbc_tke1          &
                     &                             * ((zhsro(ji)+gdepw(ji,jj,2,Kmm)) / zhsro(ji) )**(1.5_wp*ra_sf)  )**r2_3 )
                  zd_lw(ji,   2) = 0._wp
                  zd_up(ji,   2) = 0._wp
                  zdiag(ji,   2) = 1._wp
               END_1D
               !
               IF( ln_isfcav) THEN     ! top boundary   (ocean cavity)
                  DO_1Di( 0, 0 )
                     IF( mikt(ji,jj) > 1 )THEN
                        itop   = mikt(ji,jj)       ! k   top w-point
                        itopp1 = mikt(ji,jj) + 1   ! k+1 1st w-point below the top one
                        !                                                ! mask at the
                        !                                                ocean surface
                        !                                                points
                        z_en = MAX( rc02r * zstar2_top(ji), rn_emin ) * ( 1._wp - tmask(ji,jj,1) )
                        !
                        ! Dirichlet condition applied at:
                        !     top level (itop)      &         Just below it (itopp1)
                        zd_lw(ji,   itop) = 0._wp   ;   zd_lw(ji,   itopp1) = 0._wp
                        zd_up(ji,   itop) = 0._wp   ;   zd_up(ji,   itopp1) = 0._wp
                        zdiag(ji,   itop) = 1._wp   ;   zdiag(ji,   itopp1) = 1._wp
                        en   (ji,jj,itop) = z_en    ;   en   (ji,jj,itopp1) = z_en
                     ENDIF
                  END_1D
               ENDIF
               !
            CASE ( 1 )             ! Neumann boundary condition (set d(e)/dz)
               !
               DO_1Di( 0, 0 )
                  ! Dirichlet conditions at k=1
                  en   (ji,jj,1) = MAX(  rn_emin , rc02r * zstar2_surf(ji) * (1._wp + (1._wp-zice_fra(ji))*rsbc_tke1)**r2_3  )
                  zd_lw(ji,   1) = en(ji,jj,1)
                  zd_up(ji,   1) = 0._wp
                  zdiag(ji,   1) = 1._wp
                  !
                  ! at k=2, set de/dz=Fw
                  !cbr
                  ! zdiag zd_lw not defined/used on the halo
                  zdiag(ji,   2) = zdiag(ji,2) +  zd_lw(ji,2) ! Remove zd_lw from zdiag
                  zd_lw(ji,   2) = 0._wp
                  !
                  zkar  = (rl_sf + (vkarmn-rl_sf)*(1.-EXP(-rtrans*gdept(ji,jj,1,Kmm)/zhsro(ji)) ))
                  zflxs = rsbc_tke2 * (1._wp-zice_fra(ji)) * zstar2_surf(ji)**1.5_wp * zkar &
                     &                    * (  ( zhsro(ji)+gdept(ji,jj,1,Kmm) ) / zhsro(ji)  )**(1.5_wp*ra_sf)
!!gm why not   :                        * ( 1._wp + gdept(:,:,1,Kmm) / zhsro(:) )**(1.5_wp*ra_sf)
                  en(ji,jj,2) = en(ji,jj,2) + zflxs / e3w(ji,jj,2,Kmm)
               END_1D
               !
               IF( ln_isfcav) THEN     ! top boundary   (ocean cavity)
                  DO_1Di( 0, 0 )
                     IF( mikt(ji,jj) > 1 )THEN
                        itop   = mikt(ji,jj)       ! k   top w-point
                        itopp1 = mikt(ji,jj) + 1   ! k+1 1st w-point below the top one
                        !                                           ! mask at the
                        !                                           ocean surface
                        !                                           points
                        z_en = MAX( rc02r * zstar2_top(ji), rn_emin ) * ( 1._wp - tmask(ji,jj,1) )
                        !
                        ! Top level Dirichlet condition:
                        !     Top level (itop)      &        Just below it (itopp1)
                        !         Dirichlet         !           Neumann
                        zd_lw(ji,   itop) = 0._wp   !   ! Remove zd_up from zdiag
                        zdiag(ji,   itop) = 1._wp   ;   zdiag(ji,itopp1) = zdiag(ji,itopp1) + zd_up(ji,itopp1)
                        zd_up(ji,   itop) = 0._wp   ;   zd_up(ji,itopp1) = 0._wp
                        en   (ji,jj,itop) = z_en
                     ENDIF
                  END_1D
               ENDIF
            !
         END SELECT

         ! Bottom boundary condition on tke
         ! --------------------------------
         !
         SELECT CASE ( nn_bc_bot )
            !
            CASE ( 0 )             ! Dirichlet
               !                      ! en(ibot) = u*^2 / Co2 and hmxl_n(ibot) = rn_lmin
               !                      ! Balance between the production and the dissipation terms
               DO_1Di( 0, 0 )
!!gm This means that bottom and ocean w-level above have a specified "en" value.   Sure ????
!!   With thick deep ocean level thickness, this may be quite large, no ???
!!   in particular in ocean cavities where top stratification can be large...
                  ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
                  ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
                  !
                  z_en =  MAX( rc02r * zstar2_bot(ji), rn_emin )
                  !
                  ! Dirichlet condition applied at:
                  !     Bottom level (ibot)   &         Just above it (ibotm1)
                  zd_lw(ji,   ibot) = 0._wp   ;   zd_lw(ji,   ibotm1) = 0._wp
                  zd_up(ji,   ibot) = 0._wp   ;   zd_up(ji,   ibotm1) = 0._wp
                  zdiag(ji,   ibot) = 1._wp   ;   zdiag(ji,   ibotm1) = 1._wp
                  en   (ji,jj,ibot) = z_en    ;   en   (ji,jj,ibotm1) = z_en
               END_1D
               !
            CASE ( 1 )             ! Neumman boundary condition
               !
               DO_1Di( 0, 0 )
                  ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
                  ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
                  !
                  z_en =  MAX( rc02r * zstar2_bot(ji), rn_emin )
                  !
                  ! Bottom level Dirichlet condition:
                  !     Bottom level (ibot)   &         Just above it (ibotm1)
                  !         Dirichlet         !            Neumann
                  zd_lw(ji,   ibot) = 0._wp   !   ! Remove zd_up from zdiag
                  zdiag(ji,   ibot) = 1._wp   ;   zdiag(ji,ibotm1) = zdiag(ji,ibotm1) + zd_up(ji,ibotm1)
                  zd_up(ji,   ibot) = 0._wp   ;   zd_up(ji,ibotm1) = 0._wp
                  en   (ji,jj,ibot) = z_en
               END_1D
            !
         END SELECT

         ! Matrix inversion (en prescribed at surface and the bottom)
         ! ----------------------------------------------------------
         !
         DO_2Dik( 0, 0, 2, jpkm1, 1 )                ! First recurrence : Dk = Dk - Lk * Uk-1 / Dk-1
            zdiag(ji,jk) = zdiag(ji,jk) - zd_lw(ji,jk) * zd_up(ji,jk-1) / zdiag(ji,jk-1)
         END_2D
         DO_2Dik( 0, 0, 2, jpkm1, 1 )                ! Second recurrence : Lk = RHSk - Lk / Dk-1 * Lk-1
            zd_lw(ji,jk) = en(ji,jj,jk) - zd_lw(ji,jk) / zdiag(ji,jk-1) * zd_lw(ji,jk-1)
         END_2D
         DO_2Dik( 0, 0, jpkm1, 2, -1 )               ! Third recurrence : Ek = ( Lk - Uk * Ek+1 ) / Dk
            en(ji,jj,jk) = ( zd_lw(ji,jk) - zd_up(ji,jk) * en(ji,jj,jk+1) ) / zdiag(ji,jk)
         END_2D
         !                                           ! set the minimum value of tke
         DO_2Dik( 0, 0, 1, jpk, 1 )
            en(ji,jj,jk) = MAX( en(ji,jj,jk), rn_emin )
         END_2D

         !!----------------------------------------!!
         !!   Solve prognostic equation for psi    !!
         !!----------------------------------------!!

         ! Set psi to previous time step value
         !
         SELECT CASE ( nn_clos )
            !
            CASE( 0 )               ! k-kl  (Mellor-Yamada)
               DO_2Dik( 0, 0, 2, jpkm1, 1 )
                  zpsi(ji,jk)  = zeb(ji,jk) * zhmxl_b(ji,jk)
               END_2D
               !
            CASE( 1 )               ! k-eps
               DO_2Dik( 0, 0, 2, jpkm1, 1 )
                  zpsi(ji,jk)  = zeps(ji,jk)
               END_2D
               !
            CASE( 2 )               ! k-w
               DO_2Dik( 0, 0, 2, jpkm1, 1 )
                  zpsi(ji,jk)  = SQRT( zeb(ji,jk) ) / ( rc0 * zhmxl_b(ji,jk) )
               END_2D
               !
            CASE( 3 )               ! generic
               DO_2Dik( 0, 0, 2, jpkm1, 1 )
                  zpsi(ji,jk)  = rc02 * zeb(ji,jk) * zhmxl_b(ji,jk)**rnn
               END_2D
            !
         END SELECT
         !
         ! Now gls (output in psi)
         ! -------------------------------
         ! Resolution of a tridiagonal linear system by a "methode de chasse"
         ! computation from level 2 to jpkm1  (e(1) already computed and e(jpk)=0 ).
         ! zdiag : diagonal zd_up : upper diagonal zd_lw : lower diagonal
         ! Warning : after this step, en : right hand side of the matrix

         DO_2Dik( 0, 0, 2, jpkm1, 1 )
            !
            ! psi / k
            zratio = zpsi(ji,jk) / zeb(ji,jk)
            !
            ! psi3+ : stable : B=-KhN²<0 => N²>0 if rn2>0 zdir = 1 (stable) otherwise zdir = 0 (unstable)
            zdir = 0.5_wp + SIGN( 0.5_wp, rn2(ji,jj,jk) )
            !
            rpsi3 = zdir * rpsi3m + ( 1._wp - zdir ) * rpsi3p
            !
            ! shear prod. - stratif. destruction
            zprod = rpsi1 * zratio * p_sh2(ji,jj,jk)
            !
            ! stratif. destruction
            zbuoy = rpsi3 * zratio * (- p_avt(ji,jj,jk) * rn2(ji,jj,jk) )
            !
            ! shear prod. - stratif. destruction
            zdiss = rpsi2 * zratio * zwall(ji,jk) * zeps(ji,jk)
            !
            zdir = 0.5_wp + SIGN( 0.5_wp, zprod + zbuoy )   ! zdir =1(=0) if shear(ji,jj,jk)+buoy >0(<0)
            !
            zesh2  = zdir * ( zprod + zbuoy )       + (1._wp - zdir ) * zprod                         ! production term
            zzdiss = zdir * ( zdiss / zpsi(ji,jk) ) + (1._wp - zdir ) * (zdiss-zbuoy) / zpsi(ji,jk) ! dissipation term
            !
            ! building the matrix
            zcof = rfact_psi * zwall_psi(ji,jk) * tmask(ji,jj,jk)
            !                                               ! lower diagonal
            zd_lw(ji,jk) = zcof * ( p_avm(ji,jj,jk  ) + p_avm(ji,jj,jk-1) )   &
               &                / ( e3t(ji,jj,jk-1,Kmm) * e3w(ji,jj,jk,Kmm) )
            !                                               ! upper diagonal
            zd_up(ji,jk) = zcof * ( p_avm(ji,jj,jk+1) + p_avm(ji,jj,jk  ) )   &
               &                / ( e3t(ji,jj,jk  ,Kmm) * e3w(ji,jj,jk,Kmm) )
            !                                               ! diagonal
            zdiag(ji,jk) = 1._wp - zd_lw(ji,jk) - zd_up(ji,jk) + rn_Dt * zzdiss * wmask(ji,jj,jk)
            !                                               ! right hand side in psi
            zpsi(ji,jk) = zpsi(ji,jk) + rn_Dt * zesh2 * wmask(ji,jj,jk)
         END_2D
         !
         DO_1Di( 0, 0 )
            zdiag(ji,jpk) = 1._wp
         END_1D

         ! Surface boundary condition on psi
         ! ---------------------------------
         !
         SELECT CASE ( nn_bc_surf )
            !
            CASE ( 0 )             ! Dirichlet boundary conditions
               !
               DO_1Di( 0, 0 )
                  ! Surface value
                  zdep = zhsro(ji) * rl_sf ! Cosmetic
                  zpsi (ji,1) = rc0**rpp * en(ji,jj,1)**rmm * zdep**rnn * tmask(ji,jj,1)
                  zd_lw(ji,1) = zpsi(ji,1)
                  zd_up(ji,1) = 0._wp
                  zdiag(ji,1) = 1._wp
                  !
                  ! One level below
                  zkar = (rl_sf + (vkarmn-rl_sf)*(1._wp-EXP(-rtrans*gdepw(ji,jj,2,Kmm)/zhsro(ji) )))
                  zdep = (zhsro(ji) + gdepw(ji,jj,2,Kmm)) * zkar
                  zpsi (ji,2) = rc0**rpp * en(ji,jj,2)**rmm * zdep**rnn * tmask(ji,jj,1)
                  zd_lw(ji,2) = 0._wp
                  zd_up(ji,2) = 0._wp
                  zdiag(ji,2) = 1._wp
               END_1D
               !
            CASE ( 1 )             ! Neumann boundary condition on d(psi)/dz
               !
               DO_1Di( 0, 0 )
                  ! Surface value: Dirichlet
                  zdep = zhsro(ji) * rl_sf
                  zpsi (ji,1) = rc0**rpp * en(ji,jj,1)**rmm * zdep**rnn * tmask(ji,jj,1)
                  zd_lw(ji,1) = zpsi(ji,1)
                  zd_up(ji,1) = 0._wp
                  zdiag(ji,1) = 1._wp
                  !
                  ! Neumann condition at k=2, zdiag zd_lw not defined/used on the halo
                  zdiag(ji,2) = zdiag(ji,2) +  zd_lw(ji,2) ! Remove zd_lw from zdiag
                  zd_lw(ji,2) = 0._wp
                  !
                  ! Set psi vertical flux at the surface:
                  zkar  = rl_sf + (vkarmn-rl_sf)*(1._wp-EXP(-rtrans*gdept(ji,jj,1,Kmm)/zhsro(ji) )) ! Length scale slope
                  zdep  = ((zhsro(ji) + gdept(ji,jj,1,Kmm)) / zhsro(ji))**(rmm*ra_sf)
                  zflxs = (rnn + (1._wp-zice_fra(ji))*rsbc_tke1 * (rnn + rmm*ra_sf) * zdep) &
                     &           *(1._wp + (1._wp-zice_fra(ji))*rsbc_tke1*zdep)**(2._wp*rmm/3._wp-1._wp)
                  zdep  = rsbc_psi1 * (zwall_psi(ji,1)*p_avm(ji,jj,1)+zwall_psi(ji,2)*p_avm(ji,jj,2)) * &
                     &           zstar2_surf(ji)**rmm * zkar**rnn * (zhsro(ji) + gdept(ji,jj,1,Kmm))**(rnn-1.)
                  zpsi (ji,2) = zpsi(ji,2) + (zdep * zflxs) / e3w(ji,jj,2,Kmm)
               END_1D
            !
         END SELECT

         ! Bottom boundary condition on psi
         ! --------------------------------
         !
         zpsi(:,jpk) = 0._wp
!!gm should be done for ISF (top boundary cond.)
!!gm so, totally new staff needed      ===>>> think about that !
         !
         SELECT CASE ( nn_bc_bot )     ! bottom boundary
            !
            CASE ( 0 )             ! Dirichlet
               !                      ! en(ibot) = u*^2 / Co2 and hmxl_n(ibot) = vkarmn * r_z0_bot
               !                      ! Balance between the production and the dissipation terms
               DO_1Di( 0, 0 )
                  ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
                  ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
                  zdep = vkarmn * r_z0_bot
                  zpsi (ji,ibot) = rc0**rpp * en(ji,jj,ibot)**rmm * zdep**rnn
                  zd_lw(ji,ibot) = 0._wp
                  zd_up(ji,ibot) = 0._wp
                  zdiag(ji,ibot) = 1._wp
                  !
                  ! Just above last level, Dirichlet condition again (GOTM like)
                  zdep = vkarmn * ( r_z0_bot + e3t(ji,jj,ibotm1,Kmm) )
                  zpsi (ji,ibotm1) = rc0**rpp * en(ji,jj,ibot  )**rmm * zdep**rnn
                  zd_lw(ji,ibotm1) = 0._wp
                  zd_up(ji,ibotm1) = 0._wp
                  zdiag(ji,ibotm1) = 1._wp
               END_1D
               !
               IF( ln_isfcav) THEN     ! top boundary   (ocean cavity)
                  DO_1Di( 0, 0 )
                     IF ( mikt(ji,jj) > 1 ) THEN
                        itop   = mikt(ji,jj)       ! k   top w-point
                        itopp1 = mikt(ji,jj) + 1   ! k+1 1st w-point below the top one
                        !
                        zdep = vkarmn * r_z0_top
                        zpsi (ji,itop) = rc0**rpp * en(ji,jj,itop)**rmm *zdep**rnn
                        zd_lw(ji,itop) = 0._wp
                        zd_up(ji,itop) = 0._wp
                        zdiag(ji,itop) = 1._wp
                        !
                        ! Just above last level, Dirichlet condition again (GOTM like)
                        zdep = vkarmn * ( r_z0_top + e3t(ji,jj,itopp1,Kmm) )
                        zpsi (ji,itopp1) = rc0**rpp * en(ji,jj,itop  )**rmm *zdep**rnn
                        zd_lw(ji,itopp1) = 0._wp
                        zd_up(ji,itopp1) = 0._wp
                        zdiag(ji,itopp1) = 1._wp
                     END IF
                  END_1D
               END IF
               !
            CASE ( 1 )             ! Neumman boundary condition
               !
               DO_1Di( 0, 0 )
                  ibot   = mbkt(ji,jj) + 1      ! k   bottom level of w-point
                  ibotm1 = mbkt(ji,jj)          ! k-1 bottom level of w-point but >=1
                  !
                  ! Bottom level Dirichlet condition:
                  zdep = vkarmn * r_z0_bot
                  zpsi(ji,ibot) = rc0**rpp * en(ji,jj,ibot)**rmm * zdep**rnn
                  !
                  zd_lw(ji,ibot) = 0._wp
                  zd_up(ji,ibot) = 0._wp
                  zdiag(ji,ibot) = 1._wp
                  !
                  ! Just above last level: Neumann condition with flux injection
                  zdiag(ji,ibotm1) = zdiag(ji,ibotm1) + zd_up(ji,ibotm1) ! Remove zd_up from zdiag
                  zd_up(ji,ibotm1) = 0.
                  !
                  ! Set psi vertical flux at the bottom:
                  zdep  = r_z0_bot + 0.5_wp*e3t(ji,jj,ibotm1,Kmm)
                  zflxb = rsbc_psi2 * ( p_avm(ji,jj,ibot) + p_avm(ji,jj,ibotm1) )   &
                     &  * (0.5_wp*(en(ji,jj,ibot)+en(ji,jj,ibotm1)))**rmm * zdep**(rnn-1._wp)
                  zpsi(ji,ibotm1) = zpsi(ji,ibotm1) + zflxb / e3w(ji,jj,ibotm1,Kmm)
               END_1D
               !
               IF( ln_isfcav) THEN     ! top boundary   (ocean cavity)
                  DO_1Di( 0, 0 )
                     IF ( mikt(ji,jj) > 1 ) THEN
                        itop   = mikt(ji,jj)       ! k   top w-point
                        itopp1 = mikt(ji,jj) + 1   ! k+1 1st w-point below the top one
                        !
                        ! Bottom level Dirichlet condition:
                        zdep = vkarmn * r_z0_top
                        zpsi(ji,itop) = rc0**rpp * en(ji,jj,itop)**rmm *zdep**rnn
                        !
                        zd_lw(ji,itop) = 0._wp
                        zd_up(ji,itop) = 0._wp
                        zdiag(ji,itop) = 1._wp
                        !
                        ! Just below cavity level: Neumann condition with flux
                        ! injection
                        zdiag(ji,itopp1) = zdiag(ji,itopp1) + zd_up(ji,itopp1) ! Remove zd_up from zdiag
                        zd_up(ji,itopp1) = 0._wp
                        !
                        ! Set psi vertical flux below cavity:
                        zdep  = r_z0_top + 0.5_wp*e3t(ji,jj,itopp1,Kmm)
                        zflxb = rsbc_psi2 * ( p_avm(ji,jj,itop) + p_avm(ji,jj,itopp1))   &
                           &  * (0.5_wp*(en(ji,jj,itop)+en(ji,jj,itopp1)))**rmm * zdep**(rnn-1._wp)
                        zpsi(ji,itopp1) = zpsi(ji,itopp1) + zflxb / e3w(ji,jj,itopp1,Kmm)
                     END IF
                  END_1D
               END IF
            !
         END SELECT

         ! Matrix inversion
         ! ----------------
         !
         DO_2Dik( 0, 0, 2, jpkm1, 1 )                ! First recurrence : Dk = Dk - Lk * Uk-1 / Dk-1
            zdiag(ji,jk) = zdiag(ji,jk) - zd_lw(ji,jk) * zd_up(ji,jk-1) / zdiag(ji,jk-1)
         END_2D
         DO_2Dik( 0, 0, 2, jpkm1, 1 )                ! Second recurrence : Lk = RHSk - Lk / Dk-1 * Lk-1
            zd_lw(ji,jk) = zpsi(ji,jk) - zd_lw(ji,jk) / zdiag(ji,jk-1) * zd_lw(ji,jk-1)
         END_2D
         DO_2Dik( 0, 0, jpkm1, 2, -1 )               ! Third recurrence : Ek = ( Lk - Uk * Ek+1 ) / Dk
            zpsi(ji,jk) = ( zd_lw(ji,jk) - zd_up(ji,jk) * zpsi(ji,jk+1) ) / zdiag(ji,jk)
         END_2D

         ! Set dissipation
         !----------------

         SELECT CASE ( nn_clos )
            !
            CASE( 0 )               ! k-kl  (Mellor-Yamada)
               DO_2Dik( 0, 0, 1, jpkm1, 1 )
                  zeps(ji,jk) = rc03 * en(ji,jj,jk) * en(ji,jj,jk) * SQRT( en(ji,jj,jk) ) / MAX( zpsi(ji,jk), rn_epsmin)
               END_2D
               !
            CASE( 1 )               ! k-eps
               DO_2Dik( 0, 0, 1, jpkm1, 1 )
                  zeps(ji,jk) = zpsi(ji,jk)
               END_2D
               !
            CASE( 2 )               ! k-w
               DO_2Dik( 0, 0, 1, jpkm1, 1 )
                  zeps(ji,jk) = rc04 * en(ji,jj,jk) * zpsi(ji,jk)
               END_2D
               !
            CASE( 3 )               ! generic
               zcoef = rc0**( 3._wp  + rpp/rnn )
               zex1  =      ( 1.5_wp + rmm/rnn )
               zex2  = -1._wp / rnn
               DO_2Dik( 0, 0, 1, jpkm1, 1 )
                  zeps(ji,jk) = zcoef * en(ji,jj,jk)**zex1 * zpsi(ji,jk)**zex2
               END_2D
            !
         END SELECT

         ! Limit dissipation rate under stable stratification
         ! --------------------------------------------------
         DO_2Dik( 0, 0, 1, jpkm1, 1 )   ! Note that this set boundary conditions on hmxl_n at the same time
            ! limitation
            zeps  (ji,   jk)  = MAX( zeps(ji,jk), rn_epsmin )
            hmxl_n(ji,jj,jk)  = rc03 * en(ji,jj,jk) * SQRT( en(ji,jj,jk) ) / zeps(ji,jk)
         END_2D
         IF( ln_length_lim ) THEN        ! Galperin criterium (NOTE : Not required if the proper value of C3 in stable cases is calculated)
            DO_2Dik( 0, 0, 1, jpkm1, 1 )
               zrn2 = MAX( rn2(ji,jj,jk), rsmall )
               hmxl_n(ji,jj,jk) = MIN(  rn_clim_galp * SQRT( 2._wp * en(ji,jj,jk) / zrn2 ), hmxl_n(ji,jj,jk) )
            END_2D
         ENDIF

         !
         ! Stability function and vertical viscosity and diffusivity
         ! ---------------------------------------------------------
         !
         SELECT CASE ( nn_stab_func )
            !
            CASE ( 0 , 1 )             ! Galperin or Kantha-Clayson stability functions
               DO_2Dik( 0, 0, 2, jpkm1, 1 )
                  ! zcof =  l²/q²
                  zcof = zhmxl_b(ji,jk) * zhmxl_b(ji,jk) / ( 2._wp*zeb(ji,jk) )
                  ! Gh = -N²l²/q²
                  zgh = - rn2(ji,jj,jk) * zcof
                  zgh = MIN( zgh, rgh0   )
                  zgh = MAX( zgh, rghmin )
                  ! Stability functions from Kantha and Clayson (if C2=C3=0 => Galperin)
                  zsh = ra2*( 1._wp-6._wp*ra1/rb1 ) / ( 1.-3.*ra2*zgh*(6.*ra1+rb2*( 1._wp-rc3 ) ) )
                  zsm = ( rb1**(-1._wp/3._wp) + ( 18._wp*ra1*ra1 + 9._wp*ra1*ra2*(1._wp-rc2) )*zsh*zgh ) / (1._wp-9._wp*ra1*ra2*zgh)
                  !
                  ! Store stability function in zstt and zstm
                  zstt(ji,jk) = rc_diff * zsh * tmask(ji,jj,jk)
                  zstm(ji,jk) = rc_diff * zsm * tmask(ji,jj,jk)
               END_2D
               !
            CASE ( 2, 3 )               ! Canuto stability functions
               DO_2Dik( 0, 0, 2, jpkm1, 1 )
                  ! zcof =  l²/q²
                  zcof = zhmxl_b(ji,jk)*zhmxl_b(ji,jk) / ( 2._wp * zeb(ji,jk) )
                  ! Gh = -N²l²/q²
                  zgh = - rn2(ji,jj,jk) * zcof
                  zgh = MIN( zgh, rgh0   )
                  zgh = MAX( zgh, rghmin )
                  zgh = zgh * rf6
                  ! Gm =  M²l²/q² Shear number
                  zshr = p_sh2(ji,jj,jk) / MAX( p_avm(ji,jj,jk), rsmall )
                  zgm = MAX( zshr * zcof , 1.e-10 )
                  zgm = zgm * rf6
                  zgm = MIN ( (rd0 - rd1*zgh + rd3*zgh*zgh) / (rd2-rd4*zgh) , zgm )
                  ! Stability functions from Canuto
                  rcff = rd0 - rd1*zgh +rd2*zgm + rd3*zgh*zgh - rd4*zgh*zgm + rd5*zgm*zgm
                  zsm = (rs0 - rs1*zgh + rs2*zgm) / rcff
                  zsh = (rs4 - rs5*zgh + rs6*zgm) / rcff
                  !
                  ! Store stability function in zstt and zstm
                  zstt(ji,jk) = rc_diff * zsh * tmask(ji,jj,jk)
                  zstm(ji,jk) = rc_diff * zsm * tmask(ji,jj,jk)
               END_2D
            !
         END SELECT

         ! Boundary conditions on stability functions for momentum (Neumann):
         ! Lines below are useless if GOTM style Dirichlet conditions are used

         DO_1Di( 0, 0 )
            zstm(ji,1  ) = zstm(ji,2)
            zstt(ji,1  ) = wmask(ji,jj,1  ) ! Avoid a bug when looking for undefined values (-fpe0)
         END_1D
         DO_1Di( 0, 0 )
            zstm(ji,jpk) = 0.               ! Avoid a bug when looking for undefined values (-fpe0) when jpk > mbkt(ji,jj)+1
            zstt(ji,jpk) = wmask(ji,jj,jpk) ! Avoid a bug when looking for undefined values (-fpe0)
         END_1D
         DO_1Di( 0, 0 )         ! update bottom with good values
            zstm(ji,mbkt(ji,jj)+1) = zstm(ji,mbkt(ji,jj))
         END_1D

!!gm should be done for ISF (top boundary cond.)
!!gm so, totally new staff needed!!gm

         ! Compute diffusivities/viscosities
         ! The computation below could be restrained to jk=2 to jpkm1 if GOTM style Dirichlet conditions are used
         !  -> yes BUT p_avm(:,:1) and p_avm(:,:jpk) are used when we compute zd_lw(:,:2) and zd_up(:,:jpkm1). These values are
         !     later overwritten by surface/bottom boundaries conditions, so we don't really care of p_avm(:,:1) and p_avm(:,:jpk)
         !     for zd_lw and zd_up but they have to be defined to avoid a bug when looking for undefined values (-fpe0)
         DO_2Dik( 0, 0, 1, jpk, 1 )
            zsqen = SQRT( 2._wp * en(ji,jj,jk) ) * hmxl_n(ji,jj,jk)
            zavt  = zsqen * zstt(ji,jk)
            zavm  = zsqen * zstm(ji,jk)
            p_avt(ji,jj,jk) = MAX( zavt, avtb(jk) ) * wmask(ji,jj,jk) ! apply mask for zdfmxl routine
            p_avm(ji,jj,jk) = MAX( zavm, avmb(jk) )                   ! Note that avm is not masked at the surface and the bottom
         END_2D
         !
         !                                            ! ================= !
      END_1D                                          !  i-k slices loop  !
      !                                               ! ================= !
      !
      p_avt(:,:,1) = 0._wp
      !
      IF(sn_cfctl%l_prtctl) THEN
         CALL prt_ctl( tab3d_1=en   , clinfo1=' gls  - e: ', tab3d_2=p_avt, clinfo2=' t: ' )
         CALL prt_ctl( tab3d_1=p_avm, clinfo1=' gls  - m: ' )
      ENDIF
      !
   END SUBROUTINE zdf_gls


   SUBROUTINE zdf_gls_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_gls_init  ***
      !!
      !! ** Purpose :   Initialization of the vertical eddy diffivity and
      !!              viscosity computed using a GLS turbulent closure scheme
      !!
      !! ** Method  :   Read the namzdf_gls namelist and check the parameters
      !!
      !! ** input   :   Namlist namzdf_gls
      !!
      !! ** Action  :   Increase by 1 the nstop flag is setting problem encounter
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   jk    ! dummy loop indices
      INTEGER ::   ios   ! Local integer output status for namelist read
      REAL(wp)::   zcr   ! local scalar
      !!
      NAMELIST/namzdf_gls/rn_emin, rn_epsmin, ln_length_lim,       &
         &            rn_clim_galp, ln_sigpsi, rn_hsro, rn_hsri,   &
         &            nn_mxlice, rn_crban, rn_charn, rn_frac_hs,   &
         &            nn_bc_surf, nn_bc_bot, nn_z0_met, nn_z0_ice, &
         &            nn_stab_func, nn_clos
      !!----------------------------------------------------------
      !
      READ_NML_REF(numnam,namzdf_gls)
      READ_NML_CFG(numnam,namzdf_gls)
      IF(lwm) WRITE ( numond, namzdf_gls )

      IF(lwp) THEN                     !* Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_gls_init : GLS turbulent closure scheme'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_gls : set gls mixing parameters'
         WRITE(numout,*) '      minimum value of en                           rn_emin        = ', rn_emin
         WRITE(numout,*) '      minimum value of eps                          rn_epsmin      = ', rn_epsmin
         WRITE(numout,*) '      Limit dissipation rate under stable stratif.  ln_length_lim  = ', ln_length_lim
         WRITE(numout,*) '      Galperin limit (Standard: 0.53, Holt: 0.26)   rn_clim_galp   = ', rn_clim_galp
         WRITE(numout,*) '      TKE Surface boundary condition                nn_bc_surf     = ', nn_bc_surf
         WRITE(numout,*) '      TKE Bottom boundary condition                 nn_bc_bot      = ', nn_bc_bot
         WRITE(numout,*) '      Modify psi Schmidt number (wb case)           ln_sigpsi      = ', ln_sigpsi
         WRITE(numout,*) '      Craig and Banner coefficient                  rn_crban       = ', rn_crban
         WRITE(numout,*) '      Charnock coefficient                          rn_charn       = ', rn_charn
         WRITE(numout,*) '      Surface roughness formula                     nn_z0_met      = ', nn_z0_met
         WRITE(numout,*) '      surface wave breaking under ice               nn_z0_ice      = ', nn_z0_ice
      ENDIF
      IF     ( nn_mxlice > 0 .AND. nn_ice == 0 ) THEN
         CALL ctl_warn( 'zdf_gls_init: with no ice at all, nn_mxlice is set to 0 ')
         nn_mxlice = 0   ! must be set for all processes, so out of the "IF(lwp)"
      ELSEIF ( nn_mxlice > 1 .AND. nn_ice == 1 ) THEN
         CALL ctl_stop( 'zdf_gls_init: with no ice model, nn_mxlice must be 0 or 1')
      ENDIF
      IF(lwp) THEN
         SELECT CASE( nn_z0_ice )
         CASE( 0 )   ;   WRITE(numout,*) '   ==>>>   no impact of ice cover on surface wave breaking'
         CASE( 1 )   ;   WRITE(numout,*) '   ==>>>   roughness uses rn_hsri and is weigthed by 1-TANH( fr_i(:,:) * 10 )'
         CASE( 2 )   ;   WRITE(numout,*) '   ==>>>   roughness uses rn_hsri and is weighted by 1-fr_i(:,:)'
         CASE( 3 )   ;   WRITE(numout,*) '   ==>>>   roughness uses rn_hsri and is weighted by 1-MIN( 1, 4 * fr_i(:,:) )'
         CASE DEFAULT
            CALL ctl_stop( 'zdf_gls_init: wrong value for nn_z0_ice, should be 0,1,2, or 3')
         END SELECT
         WRITE(numout,*) '      Wave height frac. (used if nn_z0_met=2)       rn_frac_hs     = ', rn_frac_hs
         WRITE(numout,*) '      Stability functions                           nn_stab_func   = ', nn_stab_func
         WRITE(numout,*) '      Type of closure                               nn_clos        = ', nn_clos
         WRITE(numout,*) '      Surface roughness (m)                         rn_hsro        = ', rn_hsro
         WRITE(numout,*) '      type of scaling under sea-ice                 nn_mxlice      = ', nn_mxlice
         IF( nn_mxlice == 1 ) &
            WRITE(numout,*) '      Ice-ocean roughness (used if nn_z0_ice/=0) rn_hsri        = ', rn_hsri
         SELECT CASE( nn_mxlice )             ! Type of scaling under sea-ice
            CASE( 0 )   ;   WRITE(numout,*) '   ==>>>   No scaling under sea-ice'
            CASE( 1 )   ;   WRITE(numout,*) '   ==>>>   scaling with constant sea-ice thickness'
            CASE( 2 )   ;   WRITE(numout,*) '   ==>>>   scaling with mean     sea-ice thickness'
            CASE( 3 )   ;   WRITE(numout,*) '   ==>>>   scaling with max      sea-ice thickness'
            CASE DEFAULT
               CALL ctl_stop( 'zdf_gls_init: wrong value for nn_mxlice, should be 0,1,2,3 ')
         END SELECT
         WRITE(numout,*)
      ENDIF

      !                                !* allocate GLS arrays
      IF( zdf_gls_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_gls_init : unable to allocate arrays' )

      !                                !* Check of some namelist values
      IF( nn_bc_surf < 0   .OR. nn_bc_surf   > 1 )              CALL ctl_stop( 'zdf_gls_init: bad flag: nn_bc_surf is 0 or 1' )
      IF( nn_bc_surf < 0   .OR. nn_bc_surf   > 1 )              CALL ctl_stop( 'zdf_gls_init: bad flag: nn_bc_surf is 0 or 1' )
      IF( nn_z0_met  < 0   .OR. nn_z0_met    > 3 )              CALL ctl_stop( 'zdf_gls_init: bad flag: nn_z0_met is 0, 1, 2 or 3' )
      IF( nn_z0_met == 3  .AND. .NOT. (ln_wave .AND. ln_sdw ) ) CALL ctl_stop( 'zdf_gls_init: nn_z0_met=3 requires ln_wave=T and ln_sdw=T' )
      IF( nn_stab_func < 0 .OR. nn_stab_func > 3 )              CALL ctl_stop( 'zdf_gls_init: bad flag: nn_stab_func is 0, 1, 2 and 3' )
      IF( nn_clos      < 0 .OR. nn_clos      > 3 )              CALL ctl_stop( 'zdf_gls_init: bad flag: nn_clos is 0, 1, 2 or 3' )

      SELECT CASE ( nn_clos )          !* set the parameters for the chosen closure
      !
      CASE( 0 )                              ! k-kl  (Mellor-Yamada)
         !
         IF(lwp) WRITE(numout,*) '   ==>>   k-kl closure chosen (i.e. closed to the classical Mellor-Yamada)'
         IF(lwp) WRITE(numout,*)
         rpp     = 0._wp
         rmm     = 1._wp
         rnn     = 1._wp
         rsc_tke = 1.96_wp
         rsc_psi = 1.96_wp
         rpsi1   = 0.9_wp
         rpsi3p  = 1._wp
         rpsi2   = 0.5_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = 2.53_wp       ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = 2.62_wp       ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = 2.38          ! Canuto B stability functions (caution : constant not identified)
         END SELECT
         !
      CASE( 1 )                              ! k-eps
         !
         IF(lwp) WRITE(numout,*) '   ==>>   k-eps closure chosen'
         IF(lwp) WRITE(numout,*)
         rpp     =  3._wp
         rmm     =  1.5_wp
         rnn     = -1._wp
         rsc_tke =  1._wp
         rsc_psi =  1.2_wp  ! Schmidt number for psi
         rpsi1   =  1.44_wp
         rpsi3p  =  1._wp
         rpsi2   =  1.92_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = -0.52_wp      ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = -0.629_wp     ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = -0.566        ! Canuto B stability functions
         END SELECT
         !
      CASE( 2 )                              ! k-omega
         !
         IF(lwp) WRITE(numout,*) '   ==>>   k-omega closure chosen'
         IF(lwp) WRITE(numout,*)
         rpp     = -1._wp
         rmm     =  0.5_wp
         rnn     = -1._wp
         rsc_tke =  2._wp
         rsc_psi =  2._wp
         rpsi1   =  0.555_wp
         rpsi3p  =  1._wp
         rpsi2   =  0.833_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = -0.58_wp       ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = -0.64_wp       ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = -0.64_wp       ! Canuto B stability functions caution : constant not identified)
         END SELECT
         !
      CASE( 3 )                              ! generic
         !
         IF(lwp) WRITE(numout,*) '   ==>>   generic closure chosen'
         IF(lwp) WRITE(numout,*)
         rpp     = 2._wp
         rmm     = 1._wp
         rnn     = -0.67_wp
         rsc_tke = 0.8_wp
         rsc_psi = 1.07_wp
         rpsi1   = 1._wp
         rpsi3p  = 1._wp
         rpsi2   = 1.22_wp
         !
         SELECT CASE ( nn_stab_func )
         CASE( 0, 1 )   ;   rpsi3m = 0.1_wp         ! G88 or KC stability functions
         CASE( 2 )      ;   rpsi3m = 0.05_wp        ! Canuto A stability functions
         CASE( 3 )      ;   rpsi3m = 0.05_wp        ! Canuto B stability functions caution : constant not identified)
         END SELECT
         !
      END SELECT

      !
      SELECT CASE ( nn_stab_func )     !* set the parameters of the stability functions
      !
      CASE ( 0 )                             ! Galperin stability functions
         !
         IF(lwp) WRITE(numout,*) '   ==>>   Stability functions from Galperin'
         rc2     =  0._wp
         rc3     =  0._wp
         rc_diff =  1._wp
         rc0     =  0.5544_wp
         rcm_sf  =  0.9884_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0233_wp
         rghcri  =  0.02_wp
         !
      CASE ( 1 )                             ! Kantha-Clayson stability functions
         !
         IF(lwp) WRITE(numout,*) '   ==>>   Stability functions from Kantha-Clayson'
         rc2     =  0.7_wp
         rc3     =  0.2_wp
         rc_diff =  1._wp
         rc0     =  0.5544_wp
         rcm_sf  =  0.9884_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0233_wp
         rghcri  =  0.02_wp
         !
      CASE ( 2 )                             ! Canuto A stability functions
         !
         IF(lwp) WRITE(numout,*) '   ==>>   Stability functions from Canuto A'
         rs0 = 1.5_wp * rl1 * rl5*rl5
         rs1 = -rl4*(rl6+rl7) + 2._wp*rl4*rl5*(rl1-(1._wp/3._wp)*rl2-rl3) + 1.5_wp*rl1*rl5*rl8
         rs2 = -(3._wp/8._wp) * rl1*(rl6*rl6-rl7*rl7)
         rs4 = 2._wp * rl5
         rs5 = 2._wp * rl4
         rs6 = (2._wp/3._wp) * rl5 * ( 3._wp*rl3*rl3 - rl2*rl2 ) - 0.5_wp * rl5*rl1 * (3._wp*rl3-rl2)   &
            &                                                    + 0.75_wp * rl1 * ( rl6 - rl7 )
         rd0 = 3._wp * rl5*rl5
         rd1 = rl5 * ( 7._wp*rl4 + 3._wp*rl8 )
         rd2 = rl5*rl5 * ( 3._wp*rl3*rl3 - rl2*rl2 ) - 0.75_wp*(rl6*rl6 - rl7*rl7 )
         rd3 = rl4 * ( 4._wp*rl4 + 3._wp*rl8)
         rd4 = rl4 * ( rl2 * rl6 - 3._wp*rl3*rl7 - rl5*(rl2*rl2 - rl3*rl3 ) ) + rl5*rl8 * ( 3._wp*rl3*rl3 - rl2*rl2 )
         rd5 = 0.25_wp * ( rl2*rl2 - 3._wp *rl3*rl3 ) * ( rl6*rl6 - rl7*rl7 )
         rc0 = 0.5268_wp
         rf6 = 8._wp / (rc0**6._wp)
         rc_diff = SQRT(2._wp) / (rc0**3._wp)
         rcm_sf  =  0.7310_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0329_wp
         rghcri  =  0.03_wp
         !
      CASE ( 3 )                             ! Canuto B stability functions
         !
         IF(lwp) WRITE(numout,*) '   ==>>   Stability functions from Canuto B'
         rs0 = 1.5_wp * rm1 * rm5*rm5
         rs1 = -rm4 * (rm6+rm7) + 2._wp * rm4*rm5*(rm1-(1._wp/3._wp)*rm2-rm3) + 1.5_wp * rm1*rm5*rm8
         rs2 = -(3._wp/8._wp) * rm1 * (rm6*rm6-rm7*rm7 )
         rs4 = 2._wp * rm5
         rs5 = 2._wp * rm4
         rs6 = (2._wp/3._wp) * rm5 * (3._wp*rm3*rm3-rm2*rm2) - 0.5_wp * rm5*rm1*(3._wp*rm3-rm2) + 0.75_wp * rm1*(rm6-rm7)
         rd0 = 3._wp * rm5*rm5
         rd1 = rm5 * (7._wp*rm4 + 3._wp*rm8)
         rd2 = rm5*rm5 * (3._wp*rm3*rm3 - rm2*rm2) - 0.75_wp * (rm6*rm6 - rm7*rm7)
         rd3 = rm4 * ( 4._wp*rm4 + 3._wp*rm8 )
         rd4 = rm4 * ( rm2*rm6 -3._wp*rm3*rm7 - rm5*(rm2*rm2 - rm3*rm3) ) + rm5 * rm8 * ( 3._wp*rm3*rm3 - rm2*rm2 )
         rd5 = 0.25_wp * ( rm2*rm2 - 3._wp*rm3*rm3 ) * ( rm6*rm6 - rm7*rm7 )
         rc0 = 0.5268_wp            !!       rc0 = 0.5540_wp (Warner ...) to verify !
         rf6 = 8._wp / ( rc0**6._wp )
         rc_diff = SQRT(2._wp)/(rc0**3.)
         rcm_sf  =  0.7470_wp
         rghmin  = -0.28_wp
         rgh0    =  0.0444_wp
         rghcri  =  0.0414_wp
         !
      END SELECT

      !                                !* Set Schmidt number for psi diffusion in the wave breaking case
      !                                     ! See Eq. (13) of Carniel et al, OM, 30, 225-239, 2009
      !                                     !  or Eq. (17) of Burchard, JPO, 31, 3133-3145, 2001
      IF( ln_sigpsi ) THEN
         ra_sf = -1.5 ! Set kinetic energy slope, then deduce rsc_psi and rl_sf
         ! Verification: retrieve Burchard (2001) results by uncomenting the line below:
         ! Note that the results depend on the value of rn_cm_sf which is constant (=rc0) in his work
         ! ra_sf = -SQRT(2./3.*rc0**3./rn_cm_sf*rn_sc_tke)/vkarmn
         rsc_psi0 = rsc_tke/(24.*rpsi2)*(-1.+(4.*rnn + ra_sf*(1.+4.*rmm))**2./(ra_sf**2.))
      ELSE
         rsc_psi0 = rsc_psi
      ENDIF

      !                                !* Shear free turbulence parameters
      !
      ra_sf  = -4._wp*rnn*SQRT(rsc_tke) / ( (1._wp+4._wp*rmm)*SQRT(rsc_tke) &
               &                              - SQRT(rsc_tke + 24._wp*rsc_psi0*rpsi2 ) )

      IF ( rn_crban==0._wp ) THEN
         rl_sf = vkarmn
      ELSE
         rl_sf = rc0 * SQRT(rc0/rcm_sf) * SQRT( ( (1._wp + 4._wp*rmm + 8._wp*rmm**2_wp) * rsc_tke        &
            &                                            + 12._wp*rsc_psi0*rpsi2 - (1._wp + 4._wp*rmm)   &
            &                                                     *SQRT(rsc_tke*(rsc_tke                 &
            &                                                        + 24._wp*rsc_psi0*rpsi2)) )         &
            &                                              /(12._wp*rnn**2.)                             )
      ENDIF

      !
      IF(lwp) THEN                     !* Control print
         WRITE(numout,*)
         WRITE(numout,*) '   Limit values :'
         WRITE(numout,*) '      Parameter  m = ', rmm
         WRITE(numout,*) '      Parameter  n = ', rnn
         WRITE(numout,*) '      Parameter  p = ', rpp
         WRITE(numout,*) '      rpsi1    = ', rpsi1
         WRITE(numout,*) '      rpsi2    = ', rpsi2
         WRITE(numout,*) '      rpsi3m   = ', rpsi3m
         WRITE(numout,*) '      rpsi3p   = ', rpsi3p
         WRITE(numout,*) '      rsc_tke  = ', rsc_tke
         WRITE(numout,*) '      rsc_psi  = ', rsc_psi
         WRITE(numout,*) '      rsc_psi0 = ', rsc_psi0
         WRITE(numout,*) '      rc0      = ', rc0
         WRITE(numout,*)
         WRITE(numout,*) '   Shear free turbulence parameters:'
         WRITE(numout,*) '      rcm_sf   = ', rcm_sf
         WRITE(numout,*) '      ra_sf    = ', ra_sf
         WRITE(numout,*) '      rl_sf    = ', rl_sf
      ENDIF

      !                                !* Constants initialization
      rc02  = rc0  * rc0   ;   rc02r = 1. / rc02
      rc03  = rc02 * rc0
      rc04  = rc03 * rc0
      rsbc_tke1 = -3._wp/2._wp*rn_crban*ra_sf*rl_sf                      ! Dirichlet + Wave breaking
      rsbc_tke2 = rn_Dt * rn_crban / rl_sf                                 ! Neumann + Wave breaking
      zcr = MAX(rsmall, rsbc_tke1**(1./(-ra_sf*3._wp/2._wp))-1._wp )
      rtrans = 0.2_wp / zcr                                              ! Ad. inverse transition length between log and wave layer
      rsbc_zs1  = rn_charn/grav                                          ! Charnock formula for surface roughness
      rsbc_zs2  = rn_frac_hs / 0.85_wp / grav * 665._wp                  ! Rascle formula for surface roughness
      rsbc_psi1 = -0.5_wp * rn_Dt * rc0**(rpp-2._wp*rmm) / rsc_psi
      rsbc_psi2 = -0.5_wp * rn_Dt * rc0**rpp * rnn * vkarmn**rnn / rsc_psi ! Neumann + NO Wave breaking
      !
      rfact_tke = -0.5_wp / rsc_tke * rn_Dt                                ! Cst used for the Diffusion term of tke
      rfact_psi = -0.5_wp / rsc_psi * rn_Dt                                ! Cst used for the Diffusion term of tke
      !
      !                                !* read or initialize all required files
      CALL gls_rst( nit000, 'READ' )      ! (en, avt_k, avm_k, hmxl_n)
      !
   END SUBROUTINE zdf_gls_init


   SUBROUTINE gls_rst( kt, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE gls_rst  ***
      !!
      !! ** Purpose :   Read or write TKE file (en) in restart file
      !!
      !! ** Method  :   use of IOM library
      !!                if the restart does not contain TKE, en is either
      !!                set to rn_emin or recomputed (nn_igls/=0)
      !!----------------------------------------------------------------------
      USE zdf_oce , ONLY : en, avt_k, avm_k   ! ocean vertical physics
      !!
      INTEGER         , INTENT(in) ::   kt     ! ocean time-step
      CHARACTER(len=*), INTENT(in) ::   cdrw   ! "READ"/"WRITE" flag
      !
      INTEGER ::   jit, jk   ! dummy loop indices
      INTEGER ::   id1, id2, id3, id4
      INTEGER ::   ji, jj, ikbu, ikbv
      REAL(wp)::   cbx, cby
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN        ! Read/initialise
         !                                   ! ---------------
         IF( ln_rstart ) THEN                   !* Read the restart file
            id1 = iom_varid( numror, 'en'    , ldstop = .FALSE. )
            id2 = iom_varid( numror, 'avt_k' , ldstop = .FALSE. )
            id3 = iom_varid( numror, 'avm_k' , ldstop = .FALSE. )
            id4 = iom_varid( numror, 'hmxl_n', ldstop = .FALSE. )
            !
            IF( MIN( id1, id2, id3, id4 ) > 0 ) THEN        ! all required arrays exist
               CALL iom_get( numror, jpdom_auto, 'en'    , en   , kfill = jpfillcopy  )   ! we devide by en -> must be != 0.
               CALL iom_get( numror, jpdom_auto, 'avt_k' , avt_k  )
               CALL iom_get( numror, jpdom_auto, 'avm_k' , avm_k  )
               CALL iom_get( numror, jpdom_auto, 'hmxl_n', hmxl_n, kfill = jpfillcopy )   ! we devide by hmxl_n -> must be != 0.
            ELSE
               IF(lwp) WRITE(numout,*)
               IF(lwp) WRITE(numout,*) '   ==>>   previous run without GLS scheme, set en and hmxl_n to background values'
               en    (:,:,:) = rn_emin
               hmxl_n(:,:,:) = 0.05_wp
               ! avt_k, avm_k already set to the background value in zdf_phy_init
            ENDIF
         ELSE                                   !* Start from rest
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '   ==>>   start from rest, set en and hmxl_n by background values'
            en    (:,:,:) = rn_emin
            hmxl_n(:,:,:) = 0.05_wp
            ! avt_k, avm_k already set to the background value in zdf_phy_init
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN   ! Create restart file
         !                                   ! -------------------
         IF(lwp) WRITE(numout,*) '---- gls-rst ----'
         CALL iom_rstput( kt, nitrst, numrow, 'en'    , en     )
         CALL iom_rstput( kt, nitrst, numrow, 'avt_k' , avt_k  )
         CALL iom_rstput( kt, nitrst, numrow, 'avm_k' , avm_k  )
         CALL iom_rstput( kt, nitrst, numrow, 'hmxl_n', hmxl_n )
         !
      ENDIF
      !
   END SUBROUTINE gls_rst

   !!======================================================================
END MODULE zdfgls
