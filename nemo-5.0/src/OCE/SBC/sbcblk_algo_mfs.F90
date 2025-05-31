MODULE sbcblk_algo_mfs
   !!======================================================================
   !!                       ***  MODULE  sbcblk_algo_mfs  ***
   !! Computes:
   !!   * bulk transfer coefficients C_D, C_E and C_H 
   !!   => used in bulk formulas in sbcblk.F90 to compute the fluxes
   !!=====================================================================
   !!   sbc_blk_mfs  : bulk formulation as ocean surface boundary condition
   !!                  Used for the Mediterranean and Black Sea Copernicus Service
   !!   turb_mfs     : computes Cd,Ce,Ch 
   !!   turb_cd_2z   : Computes iturb surface drag at 2m in wave-coupled mode (Large and Yeager 2004)
   !!   cd_n10_mfs   : Compute the Cd at 10m (uncoupled)
   !!   psi_m_mfs    : Universal profile stability function for momentum
   !!   psi_h_mfs    : Universal profile stability function for temperature and humidity
   !!   q_short      : Compute Solar Radiation according Astronomical formulae
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE sbc_oce, ONLY: ln_cdgw   
   USE sbcwave, ONLY: cdn_wave ! wave module
   USE sbc_phy         ! Catalog of functions for physical/meteorological parameters in the marine boundary layer
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC ::   TURB_MFS   ! routine called in sbcblk.F90 module
   !!----------------------------------------------------------------------

#  include "do_loop_substitute.h90"

CONTAINS

   SUBROUTINE turb_mfs( zt, zu, T_s, t_zt, dpt, q_s, U_zu, &
      &                 wndi, wndj, pu, pv,               &
      &                 cloud_frac, qsw, T_spt,      &
      &                 Cd, Ch, Ce,                       &
      &                 t_zu, q_zu, Ubzu, rspeed, rhom, qlwn,  & ! output
      &                 nb_iter,                          &
      &                 slp                                )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE fluxes_mfs  ***
      !! INPUT :
      !! -------
      !!    *  zt   : height for temperature and spec. hum. of air [m]
      !!    *  zu   : height for wind speed (usually 10m) [m]
      !!    *  t_zt : potential air temperature at zt [K]
      !!    *  q_zt : specific humidity of air at zt [kg/kg]
      !!    *  U_zu : scalar wind speed at zu [m/s]
      !!    *  T_s  : always "bulk SST" as input [K] (not potential ?????)
      !!    *  q_s  : SSQ aka saturation specific humidity at temp. T_s [kg/kg]
      !!
      !! OUTPUT :
      !! --------
      !!    *  Cd     : drag coefficient
      !!    *  Ch     : sensible heat coefficient
      !!    *  Ce     : evaporation coefficient
      !!    *  t_zu   : pot. air temperature adjusted at wind height zu [K]
      !!    *  q_zu   : specific humidity of air        // [kg/kg]
      !!    *  Ubzu   : bulk wind speed at zu [m/s]
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   )                    ::   zt       ! height for t_zt and q_zt                    [m]
      REAL(wp), INTENT(in   )                    ::   zu       ! height for U_zu                             [m]
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   T_s      ! sea surface temperature                [Kelvin]
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   t_zt     ! potential air temperature              [Kelvin]
      REAL(wp), INTENT(inout), DIMENSION(A2D(0)) ::   q_s      ! sea surface specific humidity           [kg/kg]
      REAL(wp), INTENT(inout), DIMENSION(A2D(0)) ::   T_spt    ! potential temperature  
      REAL(wp), INTENT(inout), DIMENSION(A2D(0)) ::   U_zu     ! relative wind module at zu                [m/s]
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   Cd       ! transfer coefficient for momentum         (tau)
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   Ch       ! transfer coefficient for sensible heat (Q_sens)
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   Ce       ! transfert coefficient for evaporation   (Q_lat)
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   t_zu     ! pot. air temp. adjusted at zu               [K]
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   q_zu     ! spec. humidity adjusted at zu           [kg/kg]
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   Ubzu     ! bulk wind speed at zu                     [m/s]
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   rspeed   ! relative wind speed                       [m/s]
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   rhom     ! density of the air for fluxes calculation
      REAL(wp), INTENT(inout), DIMENSION(A2D(0)) ::   qsw      ! short wave radiation                     [W/m2]
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   qlwn      ! net long wave radiation                      [W/m2]
      REAL(wp), INTENT(inout), DIMENSION(A2D(0)) ::   cloud_frac ! cloud cover     
      REAL(wp), INTENT(inout), DIMENSION(A2D(0)) ::   wndi, wndj ! wind components
      REAL(wp), INTENT(in   ), DIMENSION(A2D(1)) ::   pu, pv   ! surface current components
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   dpt      ! dew point temperature [K] 
      !
      INTEGER , INTENT(in   ), OPTIONAL                    :: nb_iter    ! number of iterations
      REAL(wp), INTENT(in   ), OPTIONAL, DIMENSION(A2D(0)) ::   slp      ! [Pa]
    
      INTEGER :: ji,jj
      REAL(wp)  :: wair, vtnow, ea, s, stp , fh , fe
      REAL(wp)  :: esre, cseep
      REAL(wp), DIMENSION(A2D(0)) ::   q_zt, delT, rel_wndi, rel_wndj
      !---------------------------------------------------------------------
      !--- define Kondo parameters
      !---------------------------------------------------------------------
      REAL(wp), DIMENSION(5) :: a_h = (/0.0_wp,0.927_wp,1.15_wp,1.17_wp,1.652_wp/)
      REAL(wp), DIMENSION(5) :: a_e = (/0.0_wp,0.969_wp,1.18_wp,1.196_wp,1.68_wp/)
      REAL(wp), DIMENSION(5) :: b_h = (/1.185_wp,0.0546_wp,0.01_wp,0.0075_wp,-0.017_wp/)
      REAL(wp), DIMENSION(5) :: b_e = (/1.23_wp,0.0521_wp,0.01_wp,0.008_wp,-0.016_wp/)
      REAL(wp), DIMENSION(5) :: c_h = (/0.0_wp,0.0_wp,0.0_wp,-0.00045_wp,0.0_wp/)
      REAL(wp), DIMENSION(5) :: c_e = (/0.0_wp,0.0_wp,0.0_wp,-0.0004_wp,0.0_wp/)
      REAL(wp), DIMENSION(5) :: p_h = (/-0.157_wp,1.0_wp,1.0_wp,1.0_wp,1.0_wp/)
      REAL(wp), DIMENSION(5) :: p_e = (/-0.16_wp,1.0_wp,1.0_wp,1.0_wp,1.0_wp/)
      INTEGER :: kku                        !index varing with wind speed
      !---------------------------------------------------------------------
      !--- REdefine specific and saturated humidity 
      !---------------------------------------------------------------------    
      q_s(:,:)=rdct_qsat_salt*(1._wp/1.22_wp)*640380._wp*EXP(-5107.4_wp/T_s(:,:))
      q_zt(:,:)=(1._wp/1.22_wp)*640380._wp*EXP(-5107.4_wp/dpt(:,:))
      ! Conversion of the cloud percentage in cloud fraction 
      cloud_frac(:,:)= MAX(0._wp,MIN(1._wp,cloud_frac(:,:) * 0.01_wp))

      DO_2D( 0, 0, 0, 0 )
          !!----------------------------------------------------------------------
          !! --- calculates the term :  ( Ts - Ta ) Temperature differences
          !!----------------------------------------------------------------------
          delT(ji,jj) = T_s(ji,jj) - t_zt (ji,jj)
          !!----------------------------------------------------------------------
          !! --- variable turbulent exchange coefficients ( from Kondo 1975 )
          !! --- calculate the Neutral Transfer Coefficent using an empiric formula
          !! --- by Kondo et al. Then it applies the diabatic approximation.
          !!----------------------------------------------------------------------
          s = delT(ji,jj)/(U_zu(ji,jj)**2._wp)   !! --- calculate S
          stp = s*abs(s)/(abs(s)+0.01_wp)        !! --- calculate the Stability Parameter
          !!----------------------------------------------------------------------
          !! --- for stable condition (sst-t_air < 0):
          !!----------------------------------------------------------------------
          IF (s.lt.0. .and. ((stp.gt.-3.3).and.(stp.lt.0.))) THEN
              fh = 0.1_wp+0.03_wp*stp+0.9_wp*exp(4.8_wp*stp)
              fe = fh
          ELSE IF (s.lt.0. .and. stp.le.-3.3) THEN
              fh = 0._wp
              fe = fh
          ELSE                                       ! --- for unstable condition
              fh = 1.0_wp+0.63_wp*sqrt(stp)
              fe = fh
          ENDIF
          !!----------------------------------------------------------------------
          !! --- calculate the coefficient CH,CE,CD
          !!----------------------------------------------------------------------
          IF (U_zu(ji,jj) >= 0. .AND. U_zu(ji,jj) <= 2.2)       THEN
              kku=1._wp
          ELSE IF (U_zu(ji,jj) > 2.2 .AND. U_zu(ji,jj) <= 5.0)  THEN
              kku=2._wp
          ELSE IF (U_zu(ji,jj) > 5.0 .AND. U_zu(ji,jj) <= 8.0)  THEN
              kku=3._wp
          ELSE IF (U_zu(ji,jj) > 8.0 .AND. U_zu(ji,jj) <= 25.0) THEN
              kku=4._wp
          ELSE IF (U_zu(ji,jj) > 25.0 )                         THEN
              kku=5._wp
          ENDIF
          !
          Ch(ji,jj) = ( a_h(kku) + b_h(kku) * U_zu(ji,jj) ** p_h(kku)      &
                  + c_h(kku) * (U_zu(ji,jj)- 8._wp ) **2) * fh
          Ce(ji,jj) = ( a_e(kku) + b_e(kku) * U_zu(ji,jj) ** p_e(kku)      &
                  + c_e(kku) * (U_zu(ji,jj)- 8._wp ) **2) * fe
          Ch(ji,jj) = Ch(ji,jj) / 1000.0_wp
          Ce(ji,jj) = Ce(ji,jj) / 1000.0_wp
          !
          IF (U_zu(ji,jj)<0.3) THEN
              Ch(ji,jj) = 1.3e-03_wp * fh
              Ce(ji,jj) = 1.5e-03_wp * fe
          ELSE IF(U_zu(ji,jj)>50.0) THEN
              Ch(ji,jj) = 1.25e-03_wp * fh
              Ce(ji,jj) = 1.30e-03_wp * fe
          ENDIF
      END_2D
      rel_wndi(:,:) = 0.0_wp
      rel_wndj(:,:) = 0.0_wp
      rspeed(:,:)= 0.0_wp
      DO_2D( 0, 0, 0, 0 )
         rel_wndi(ji,jj) = wndi(ji,jj) - ( pu(ji-1,jj) + pu(ji,jj) ) &
                 &  / MAX(1._wp, umask(ji-1,jj,1) + umask(ji,jj,1)) 
         rel_wndj(ji,jj) = wndj(ji,jj) - ( pv(ji,jj-1) + pv(ji,jj) ) &
                 &  / MAX(1._wp, vmask(ji,jj-1,1) + vmask(ji,jj,1)) 
         rspeed(ji,jj)= SQRT(rel_wndi(ji,jj)*rel_wndi(ji,jj) + rel_wndj(ji,jj)*rel_wndj(ji,jj))
      END_2D                                
      !!----------------------------------------------------------------------
      !! --- calculates the Drag Coefficient
      !!----------------------------------------------------------------------
      IF (ln_cdgw ) THEN
         CALL turb_cd_2z(zt,10._wp,T_s,t_zt+2._wp*0.0098_wp,q_s,q_zt,rspeed,nb_iter,Cd)
      ELSE
         CALL cd_n10_mfs(U_zu,-delT, Cd)
      ENDIF
      
      wndi(:,:)=rel_wndi(:,:)
      wndj(:,:)=rel_wndj(:,:)
      Ubzu(:,:) = U_zu(:,:)
      t_zu(:,:) = t_zt(:,:)
      q_zu(:,:) = q_zt(:,:)
      T_spt(:,:)= T_s(:,:)
      !
      ! Calculation of the short wave radiation    
      CALL qshort(cloud_frac,qsw)
      !    
      DO_2D( 0, 0, 0, 0 )
           wair= q_zt(ji,jj)/(1-q_zt(ji,jj))
           vtnow = (t_zt(ji,jj)*(reps0+wair))/(reps0*(1._wp+wair))
           ea   = (wair  / (wair  + 0.622_wp )) * slp(ji,jj)* 0.01_wp
           rhom(ji,jj) =(101300._wp/287._wp)/vtnow 
           !
           !Bignami et al. 1995 used in the Mediterranean Sea
           qlwn(ji,jj) = -( stefan*( T_s(ji,jj)**4._wp ) &
                        - ( stefan*( t_zt(ji,jj)**4._wp ) * ( 0.653_wp + 0.00535_wp*ea ) )  &
                        * ( 1._wp + 0.1762_wp*( cloud_frac(ji,jj)**2._wp ) ) )
    
           ! Rosati and Miyakoda (1988) used in the Black Sea
           !qlwn(ji,jj) = - ( emic*stefan*( sstk(ji,jj)**4._wp)*(0.39_wp-0.05_wp*sqrt(ea))    &
           !           * (1._wp-cldnow(ji,jj)*0.8_wp) + 4._wp*emic*stefan*(sstk(ji,jj)**3_wp) &
           !           * (sstk(ji,jj)-tnow(ji,jj)))
      END_2D
   END SUBROUTINE turb_mfs


   SUBROUTINE turb_cd_2z( zt, zu, sst, t_zt, ssq, q_zt, U_zu, nb_iter, Cd )
   !!----------------------------------------------------------------------
   !!               ***  modified from ROUTINE  turb_core  ***
   !!
   !! ** Purpose :   Computes turbulent transfert coefficients of surface
   !!                fluxes according to Large & Yeager (2004) and Large & Yeager (2008)
   !!                If relevant (zt /= zu), adjust temperature and humidity from height zt to zu
   !!
   !! ** Method : Monin Obukhov Similarity Theory
   !!             + Large & Yeager (2004,2008) closure: CD_n10 = f(U_n10)
   !!
   !! ** References :   Large & Yeager, 2004 / Large & Yeager, 2008
   !!
   !!----------------------------------------------------------------------
      REAL(wp), INTENT(in   )                    ::   zt       ! height for t_zt and q_zt                  [m]
      REAL(wp), INTENT(in   )                    ::   zu       ! height for dU                             [m]
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   sst      ! sea surface temperature              [Kelvin]
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   t_zt     ! potential air temperature            [Kelvin]
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   ssq      ! sea surface specific humidity         [kg/kg]
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   q_zt     ! specific air humidity                 [kg/kg]
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0)) ::   U_zu     ! relative wind module at zu              [m/s]
      INTEGER , INTENT(in   ), OPTIONAL          ::   nb_iter  ! number of iteration
      REAL(wp), INTENT(  out), DIMENSION(A2D(0)) ::   Cd       ! transfer coefficient for momentum       (tau)
      !
      INTEGER ::   nbit, jit
      LOGICAL ::   l_zt_equal_zu = .FALSE.      ! if q and t are given a different height than U
      !
      REAL(wp), DIMENSION(A2D(0)) ::   zCdN, zCeN, zChN    ! 10m neutral latent/ sensible coefficient
      REAL(wp), DIMENSION(A2D(0)) ::   Ch            ! transfer coefficient for sensible heat (Q_sens)
      REAL(wp), DIMENSION(A2D(0)) ::   Ce            ! transfert coefficient for evaporation   (Q_lat)
      REAL(wp), DIMENSION(A2D(0)) ::   Ubzu
      REAL(wp), DIMENSION(A2D(0)) ::   t_zu          ! air temp. shifted at zu                     [K]
      REAL(wp), DIMENSION(A2D(0)) ::   q_zu          ! spec. hum.shifted at zu               [kg/kg]
      REAL(wp), DIMENSION(A2D(0)) ::   zsqrt_Cdn     ! root square of Cd_n10
      REAL(wp), DIMENSION(A2D(0)) ::   zsqrt_Cd      ! root square of Cd
      REAL(wp), DIMENSION(A2D(0)) ::   zeta_u        ! stability parameter at height zu
      REAL(wp), DIMENSION(A2D(0)) ::   ztmp0, ztmp1, ztmp2
      !!----------------------------------------------------------------------
      nbit = nb_iter0 ! needed???
      IF( PRESENT(nb_iter) ) nbit = nb_iter
    
      l_zt_equal_zu = ( ABS(zu - zt) < 0.01_wp ) ! testing "zu == zt" is risky with double precision
    
      Ubzu = MAX( 0.5_wp , U_zu )   !  relative wind speed at zu (normally 10m), we don't want to fall under 0.5 m/s
    
      !! First guess of stability:
      ztmp0 = virt_temp(t_zt, q_zt) - virt_temp(sst, ssq)
      ztmp1 = 0.5_wp + sign(0.5,ztmp0)                           ! stab = 1 if dTv  > 0  => STABLE, 0 if unstable
    
      !! Neutral coefficients at 10m read from wave output:
      cdn_wave (:,:) = cdn_wave(:,:) + rsmall * ( 1._wp - smask0(:,:) )
      zCdN     (:,:) = cdn_wave(:,:)
    
      zsqrt_Cdn = SQRT( zCdN )
      Cd  = zCdN
      Ce  = 1.e-3_wp * ( 34.6_wp * zsqrt_Cdn )
      Ch  = 1.e-3_wp * zsqrt_Cdn * (18._wp * ztmp1 + 32.7_wp * (1._wp - ztmp1) )
      zsqrt_Cd = zsqrt_Cdn
    
      !! Initializing transf. coeff. with their first guess neutral equivalents:
      zCeN = Ce 
      zChN = Ch 
    
      !! Initializing values at z_u with z_t values:
      t_zu = t_zt
      q_zu = q_zt
    
      !!  * Now starting iteration loop
      DO jit= 1, nbit
          !
          ztmp1 = t_zu - sst   ! Updating air/sea differences
          ztmp2 = q_zu - ssq
    
          ! Updating turbulent scales :   (L&Y 2004 eq. (7))
          ztmp0 = zsqrt_Cd*Ubzu        ! u*!ztmp0 = t_zu*(1._wp + 0.608_wp*q_zu) ! virtual potential temperature at zu
          ztmp1 = Ch/zsqrt_Cd*ztmp1    ! theta*
          ztmp2 = Ce/zsqrt_Cd*ztmp2    ! q*
    
          ! Estimate the inverse of Monin-Obukov length (1/L) at height zu:
          ztmp0 = One_on_L( t_zu, q_zu, ztmp0, ztmp1, ztmp2 )
          !ztmp0 =  (vkarmn*grav/ztmp0*(ztmp1*(1._wp+0.608_wp*q_zu) + 0.608_wp*t_zu*ztmp2)) / (zsqrt_Cd*U_zu*U_zu)
          !                                                           ( Cd*U_zu*U_zu is U*^2 at zu)
          !! Stability parameters :
          zeta_u = zu*ztmp0   
          zeta_u = sign( min(abs(zeta_u),10.0_wp), zeta_u )
    
          !! Shifting temperature and humidity at zu (L&Y 2004 eq. (9b-9c))
          IF ( .NOT. l_zt_equal_zu ) THEN
              ztmp0 = zt*ztmp0 ;  
              ztmp0 = sign( min(abs(ztmp0),10.0_wp), ztmp0 )
              ztmp0 = LOG(zu/zt) -  psi_h_mfs( zeta_u ) + psi_h_mfs(ztmp0)  ! stab just used as temp array!!!
              t_zu = t_zt + ztmp1/vkarmn*ztmp0    ! ztmp1 is still theta*
              q_zu = q_zt + ztmp2/vkarmn*ztmp0    ! ztmp2 is still q*
              q_zu = max(0._wp, q_zu)
          END IF
    
          zsqrt_Cd = vkarmn / ( vkarmn / zsqrt_Cdn - psi_m_mfs( zeta_u ) )
          Cd       = zsqrt_Cd * zsqrt_Cd
          !
          ztmp0 = (LOG(zu/10._wp) - psi_h_mfs(zeta_u)) / vkarmn / zsqrt_Cdn
          ztmp2 = zsqrt_Cd / zsqrt_Cdn
          ztmp1 = 1._wp + zChN   * ztmp0
          Ch    = zChN  * ztmp2  / ztmp1  ! L&Y 2004 eq. (10b)
          ztmp1 = 1._wp + zCeN * ztmp0
          Ce    = zCeN  * ztmp2  / ztmp1  ! L&Y 2004 eq. (10c)
      END DO
      !
   END SUBROUTINE turb_cd_2z


   FUNCTION psi_m_mfs(pzeta)   !! Psis, L&Y 2004 eq. (8c), (8d), (8e)
      !-------------------------------------------------------------------------------
      ! universal profile stability function for momentum
      !-------------------------------------------------------------------------------
      REAL(wp), DIMENSION(A2D(0)), INTENT(in) :: pzeta
      REAL(wp), DIMENSION(A2D(0))             :: psi_m_mfs
      
      INTEGER  ::   ji, jj    ! dummy loop indices
      REAL(wp) :: zta, zx2, zx, zpsi_unst, zpsi_stab,  zstab   ! local scalars
      !-------------------------------------------------------------------------------
      !
      DO_2D( 0, 0, 0, 0 )
          !
          zta = pzeta(ji,jj) !MIN( pzeta(ji,jj) , 15._wp ) !! Very stable conditions (Lpositif and big!)
          !
          zx2 = SQRT( ABS(1._wp - 16._wp*zta) )  ! (1 - 16z)^0.5
          zx2 = MAX( zx2 , 1._wp )
          zx  = SQRT(zx2)                          ! (1 - 16z)^0.25
   
          zpsi_unst = 2._wp*LOG(ABS( (1._wp + zx )*0.5_wp ))   & 
             &            + LOG(ABS( (1._wp + zx2)*0.5_wp ))   &
             &          - 2._wp*ATAN(zx) + rpi*0.5_wp
          !
          zpsi_stab = -5._wp*zta
          !
          zstab = 0.5_wp + SIGN(0.5_wp, zta) ! zta > 0 => zstab = 1
          !
          psi_m_mfs(ji,jj) =           zstab  * zpsi_stab &   !Stable
             &              + (1._wp - zstab) * zpsi_unst     !Unstable
          !
      END_2D
      !
   END FUNCTION psi_m_mfs

   FUNCTION psi_h_mfs( pzeta )    !! Psis, L&Y 2004 eq. (8c), (8d), (8e)
      !-------------------------------------------------------------------------------
      ! universal profile stability function for temperature and humidity
      !-------------------------------------------------------------------------------
      REAL(wp), DIMENSION(A2D(0)), INTENT(in) ::   pzeta
      REAL(wp), DIMENSION(A2D(0))             ::   psi_h_mfs
      !
      INTEGER  ::   ji, jj     ! dummy loop indices
      REAL(wp) :: zta, zx2, zx, zpsi_unst, zpsi_stab, zstab  ! local scalars
      !-------------------------------------------------------------------------------
      !
      DO_2D( 0, 0, 0, 0 )
          zta = pzeta(ji,jj) ! MIN( pzeta(ji,jj) , 15._wp ) !! Very stable conditions (Lpositif and large!)
          !
          zx2 = SQRT( ABS(1._wp - 16._wp*zta) )  ! (1 -16z)^0.5
          zx2 = MAX( zx2 , 1._wp )
          zx  = SQRT(zx2)
          zpsi_unst = 2._wp*LOG( 0.5_wp*(1._wp + zx2) )
          !
          zpsi_stab = -5._wp*zta
          !
          zstab = 0.5_wp + SIGN(0.5_wp, zta) ! zta > 0 => zstab = 1
          !
          psi_h_mfs(ji,jj) =            zstab  * zpsi_stab   &  ! Stable
                           & + (1._wp - zstab) * zpsi_unst      ! Unstable
          !
      END_2D
      !
   END FUNCTION psi_h_mfs

   SUBROUTINE cd_n10_mfs(pw10,delt, Cd)
      !!----------------------------------------------------------------------
      !! --- calculates the Drag Coefficient as a function of the abs. value
      !! --- of the wind velocity ( Hellermann and Rosenstein )
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in ), DIMENSION(A2D(0)) :: pw10
      REAL(wp), INTENT(in ), DIMENSION(A2D(0)) :: delt
      REAL(wp), INTENT(out), DIMENSION(A2D(0)) :: Cd
      INTEGER :: ji,jj
      REAL(wp), PARAMETER  :: a1=0.934e-3_wp , a2=0.788e-4_wp, a3=0.868e-4_wp
      REAL(wp), PARAMETER  :: a4=-0.616e-6_wp, a5=-.120e-5_wp, a6=-.214e-5_wp
      !!----------------------------------------------------------------------
      DO_2D( 0, 0, 0, 0 )
           Cd(ji,jj) = a1 + a2 * pw10(ji,jj) + a3 * delt(ji,jj) + a4 * pw10(ji,jj) * pw10(ji,jj)   &
                 + a5 * delt(ji,jj) * delt(ji,jj)  + a6 * pw10(ji,jj) * delt(ji,jj)
      !     Cd(ji,jj) = MAX( Cd(ji,jj), Cx_min )
      END_2D
      !
   END SUBROUTINE cd_n10_mfs

   SUBROUTINE qshort(cldnow,qsw)
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE qshort  ***
      !!
      !! ** Purpose :   Compute Solar Radiation
      !! ** Method  :   Compute Solar Radiation according Astronomical
      !!                formulae
      !! References :   Reed RK (1975) and Reed RK (1977)
      !! Note: alat,alon - (lat, lon)  in radians
      !!----------------------------------------------------------------------
      REAL(wp) :: hour
      REAL(wp), DIMENSION(A2D(0)) :: alat,alon
      REAL(wp), INTENT(in ), DIMENSION(A2D(0)) :: cldnow
      REAL(wp), INTENT(out), DIMENSION(A2D(0)) :: qsw
      REAL(wp), DIMENSION(12) :: alpham
      REAL(wp), PARAMETER ::   eclips=23.439_wp* (3.141592653589793_wp / 180._wp)
      REAL(wp), PARAMETER ::   solar = 1350._wp
      REAL(wp), PARAMETER ::   tau = 0.7_wp
      REAL(wp), PARAMETER ::   aozone = 0.09_wp
      REAL(wp), PARAMETER ::   yrdays = 360._wp
      REAL(wp) :: days, th0,th02,th03, sundec, thsun, coszen, qatten
      REAL(wp) :: qzer, qdir,qdiff,qtot,tjul,sunbet
      REAL(wp) :: albedo
      INTEGER :: jj, ji
      !!----------------------------------------------------------------------
      !! --- albedo monthly values from Payne (1972) as means of the values
      !! --- at 40N and 30N for the Atlantic Ocean ( hence the same latitudinal
      !! --- band of the Mediterranean Sea ) :
      !!----------------------------------------------------------------------
   
      data alpham /0.095_wp,0.08_wp,0.065_wp,0.065_wp,0.06_wp,0.06_wp,0.06_wp,0.06_wp,        &
              0.065_wp,0.075_wp,0.09_wp,0.10_wp/
   
      !!----------------------------------------------------------------------
      !!   days is the number of days elapsed until the day=nday_year
      !!----------------------------------------------------------------------
      days = nday_year -1._wp
      th0  = 2._wp*rpi*days/yrdays
      th02 = 2._wp*th0
      th03 = 3._wp*th0
   
      !! --- sun declination :
      !!----------------------------------------------------------------------
      sundec = 0.006918_wp - 0.399912_wp*cos(th0) + 0.070257_wp*sin(th0) -   &
              0.006758_wp*cos(th02) + 0.000907_wp*sin(th02) -   &
              0.002697_wp*cos(th03) + 0.001480_wp*sin(th03)
   
      hour = (( nsec_year / rday ) - INT (nsec_year / rday)) * rjjhh
   
      DO_2D( 0, 0, 0, 0 )
              alon(ji,jj) = glamt(ji,jj) * rad
              alat(ji,jj) = gphit(ji,jj) * rad        
              !! --- sun hour angle :
              !!----------------------------------------------------------------------
              thsun = (hour -12._wp)*15._wp*rad + alon(ji,jj)
   
              !! --- cosine of the solar zenith angle :
              !!----------------------------------------------------------------------
              coszen =sin(alat(ji,jj))*sin(sundec)                 &
                      +cos(alat(ji,jj))*cos(sundec)*cos(thsun)
   
              IF(coszen .LE. 5.035D-04) THEN
                  coszen = 0.0_wp
                  qatten = 0.0_wp
              ELSE
                  qatten = tau**(1./coszen)
              END IF
   
              qzer  = coszen * solar *                                 &
                      (1.0_wp+1.67E-2_wp*cos(rpi*2._wp*(days-3.0_wp)/365.0_wp))**2._wp
              qdir  = qzer * qatten
              qdiff = ((1._wp-aozone)*qzer - qdir) * 0.5_wp
              
              qtot  =  qdir + qdiff
              tjul = (days -81._wp)*rad
   
              !! --- sin of the solar noon altitude in radians :
              !!----------------------------------------------------------------------
              sunbet=sin(alat(ji,jj))*sin(eclips*sin(tjul)) +   &
                      cos(alat(ji,jj))*cos(eclips*sin(tjul))
              !! --- solar noon altitude in degrees :
              !!----------------------------------------------------------------------
              sunbet = asin(sunbet)/rad
              !!----------------------------------------------------------------------
              !! --- calculates the albedo according to Payne (1972)
              !!----------------------------------------------------------------------
              albedo = alpham(nmonth)
              !!----------------------------------------------------------------------
              !! --- ( radiation as from Reed(1977), Simpson and Paulson(1979) )
              !! --- calculates SHORT WAVE FLUX ( watt/m*m )
              !! --- ( Rosati,Miyakoda 1988 ; eq. 3.8 )
              !!----------------------------------------------------------------------
              IF(cldnow(ji,jj).LT.0.3_wp) THEN
                  qsw(ji,jj) = qtot * (1._wp-albedo)
              ELSE
                  qsw(ji,jj) = qtot*(1._wp-0.62_wp*cldnow(ji,jj)              &
                             + .0019_wp*sunbet)*(1._wp-albedo)
              ENDIF
      END_2D
   END SUBROUTINE qshort
   !!======================================================================

END MODULE sbcblk_algo_mfs
