MODULE ldfslp
   !!======================================================================
   !!                       ***  MODULE  ldfslp  ***
   !! Ocean physics: slopes of neutral surfaces
   !!======================================================================
   !! History :  OPA  ! 1994-12  (G. Madec, M. Imbard)  Original code
   !!            8.0  ! 1997-06  (G. Madec)  optimization, lbc
   !!            8.1  ! 1999-10  (A. Jouzeau)  NEW profile in the mixed layer
   !!   NEMO     1.0  ! 2002-10  (G. Madec)  Free form, F90
   !!             -   ! 2005-10  (A. Beckmann)  correction for s-coordinates
   !!            3.3  ! 2010-10  (G. Nurser, C. Harris, G. Madec)  add Griffies operator
   !!             -   ! 2010-11  (F. Dupond, G. Madec)  bug correction in slopes just below the ML
   !!            3.7  ! 2013-12  (F. Lemarie, G. Madec)  add limiter on triad slopes
   !!            4.x  ! 2022-12  (S. Techene, G. Madec)  optmise memory and correct discrepancy wrt eos evolution
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   ldf_slp       : calculates the slopes of neutral surface   (Madec operator)
   !!   ldf_slp_triad : calculates the triads of isoneutral slopes (Griffies operator)
   !!   ldf_slp_init  : initialization of the slopes computation
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE isf_oce        ! ice shelf
   USE dom_oce        ! ocean space and time domain
!   USE ldfdyn         ! lateral diffusion: eddy viscosity coef.
   USE phycst         ! physical constants
   USE zdfmxl         ! mixed layer depth
   USE eosbn2         ! equation of states
   !
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! distribued memory computing library
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ldf_slp         ! routine called by step.F90
   PUBLIC   ldf_slp_triad   ! routine called by step.F90
   PUBLIC   ldf_slp_init    ! routine called by nemogcm.F90

   LOGICAL , PUBLIC ::   l_ldfslp = .FALSE.     !: slopes flag

   LOGICAL , PUBLIC ::   ln_traldf_iso   = .TRUE.       !: iso-neutral direction                           (nam_traldf namelist)
   LOGICAL , PUBLIC ::   ln_traldf_triad = .FALSE.      !: griffies triad scheme                           (nam_traldf namelist)
   LOGICAL , PUBLIC ::   ln_dynldf_iso                  !: iso-neutral direction                           (nam_dynldf namelist)

   LOGICAL , PUBLIC ::   ln_triad_iso    = .FALSE.      !: pure horizontal mixing in ML                    (nam_traldf namelist)
   LOGICAL , PUBLIC ::   ln_botmix_triad = .FALSE.      !: mixing on bottom                                (nam_traldf namelist)
   REAL(wp), PUBLIC ::   rn_sw_triad     = 1._wp        !: =1 switching triads ; =0 all four triads used   (nam_traldf namelist)
   REAL(wp), PUBLIC ::   rn_slpmax       = 0.01_wp      !: slope limit                                     (nam_traldf namelist)

   LOGICAL , PUBLIC ::   l_grad_zps = .FALSE.           !: special treatment for Horz Tgradients w partial steps (triad operator)
   
   !                                                     !! Classic operator (Madec)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)     ::   uslp, wslpi          !: i_slope at U- and W-points
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)     ::   vslp, wslpj          !: j-slope at V- and W-points
   !                                                     !! triad operator (Griffies)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)     ::   wslp2                !: wslp**2 from Griffies quarter cells
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) ::   triadi_g, triadj_g   !: skew flux  slopes relative to geopotentials
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) ::   triadi  , triadj     !: isoneutral slopes relative to model-coordinate
   !                                                     !! both operators
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)     ::   ah_wslp2             !: ah * slope^2 at w-point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)     ::   akz                  !: stabilizing vertical diffusivity


   REAL(wp) ::   repsln = 1.e-25_wp       ! tiny value used as minium of di(rho), dj(rho) and dk(rho)

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ldf_slp( kt, prd, pn2, Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE ldf_slp  ***
      !!
      !! ** Purpose :   Compute the slopes of neutral surface (slope of isopycnal
      !!              surfaces referenced locally) (ln_traldf_iso=T).
      !!
      !! ** Method  :   The slope in the i-direction is computed at U- and
      !!      W-points (uslp, wslpi) and the slope in the j-direction is
      !!      computed at V- and W-points (vslp, wslpj).
      !!      They are bounded by 1/100 over the whole ocean, and within the
      !!      surface layer they are bounded by the distance to the surface
      !!      ( slope<= depth/l  where l is the length scale of horizontal
      !!      diffusion (here, aht=2000m2/s ==> l=20km with a typical velocity
      !!      of 10cm/s)
      !!        A horizontal shapiro filter is applied to the slopes
      !!        l_sco=T, s-coordinate, add to the previously computed slopes
      !!      the slope of the model level surface.
      !!        macro-tasked on horizontal slab (jk-loop)  (2, jpk-1)
      !!      [slopes already set to zero at level 1, and to zero or the ocean
      !!      bottom slope (l_sco=T) at level jpk in inildf]
      !!
      !! ** Action : - uslp, wslpi, and vslp, wslpj, the i- and  j-slopes
      !!               of now neutral surfaces at u-, w- and v- w-points, resp.
      !!----------------------------------------------------------------------
      INTEGER , INTENT(in)                   ::   kt    ! ocean time-step index
      INTEGER , INTENT(in)                   ::   Kbb, Kmm   ! ocean time level indices
      REAL(wp), INTENT(in), DIMENSION(:,:,:) ::   prd   ! in situ density
      REAL(wp), INTENT(in), DIMENSION(:,:,:) ::   pn2   ! Brunt-Vaisala frequency (locally ref.)
      !!
      INTEGER  ::   ji , jj , jk                 ! dummy loop indices
      INTEGER  ::   iik, iikm1, itmp, iku, ikv   ! local integer
      REAL(wp) ::   zeps, zm1_g, zm1_2g, z1_16, zcofw, z1_slpmax ! local scalars
      REAL(wp) ::   zci, zfi, zau, zbu, zai, zbi, zmli   !   -      -
      REAL(wp) ::   zcj, zfj, zav, zbv, zaj, zbj, zmlj   !   -      -
      REAL(wp) ::   zck, zfk,      zbw          , zmlk   !   -      -
      REAL(wp) ::   zdepu, zdepv                         !   -      -
      REAL(wp), DIMENSION(A2D(2))   ::  zwz, zdzr
      REAL(wp), DIMENSION(A2D(2))   ::  zww
      REAL(wp), DIMENSION(A2D(2))   ::  zhmlpt
      REAL(wp), DIMENSION(A2D(1))   ::  zuslp_hml, zwslpi_hml, r1_hmlu
      REAL(wp), DIMENSION(A2D(1))   ::  zvslp_hml, zwslpj_hml, r1_hmlv
      REAL(wp), DIMENSION(A2D(1))   ::                         r1_hmlw
      REAL(wp), DIMENSION(A2D(2),2) ::  zgru, zgrv
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('ldf_slp')
      !
      zeps   =  1.e-20_wp           !==   Local constant initialization   ==!
      z1_16  =  1.0_wp / 16._wp
      zm1_g  = -1.0_wp / grav
      zm1_2g = -0.5_wp / grav
      z1_slpmax = 1._wp / rn_slpmax
      !
      zuslp_hml(:,:) = 0._wp   ;   zwslpi_hml(:,:) = 0._wp
      zvslp_hml(:,:) = 0._wp   ;   zwslpj_hml(:,:) = 0._wp
      !
      ! nmln calculation in zdfmxl is only on internal points
      DO_2D( 1, 2, 1, 2 )
         zhmlpt(ji,jj) = REAL( nmln(ji,jj), wp )
      END_2D
      !
      DO_2D( 1, 2, 1, 2 )                  ! depth of the last T-point inside the mixed layer
         zhmlpt(ji,jj) = gdept(ji,jj,nmln(ji,jj)-1,Kmm) * ssmask(ji,jj)
      END_2D
      !                             !==   Mixed layer height at u-, v- and w-points  ==!
      IF( ln_isfcav ) THEN
         DO_2D( 1, 1, 1, 1 )
            r1_hmlu(ji,jj) = 1._wp / ( MAX(zhmlpt (ji,jj), zhmlpt (ji+1,jj  ), 5._wp) &
               &                     - MAX(risfdep(ji,jj), risfdep(ji+1,jj  )       ) )
            r1_hmlv(ji,jj) = 1._wp / ( MAX(zhmlpt (ji,jj), zhmlpt (ji  ,jj+1), 5._wp) &
               &                     - MAX(risfdep(ji,jj), risfdep(ji  ,jj+1)       ) )
         END_2D
      ELSE
         DO_2D( 1, 1, 1, 1 )
            r1_hmlu(ji,jj) = 1._wp / MAX(zhmlpt(ji,jj), zhmlpt(ji+1,jj  ), 5._wp)
            r1_hmlv(ji,jj) = 1._wp / MAX(zhmlpt(ji,jj), zhmlpt(ji  ,jj+1), 5._wp)
         END_2D
      ENDIF
      !
      DO_2D( 1, 1, 1, 1 )
         r1_hmlw(ji,jj) = 1._wp / MAX( hmlp(ji,jj) - gdepw(ji,jj,mikt(ji,jj),Kmm), 10._wp )
      END_2D
      !
      iikm1 = 1   ;   iik = 2                              ! iik-index initialisation
      !
      DO_2D( 2, 1, 2, 1 )           !==   bottom i- & j-gradient of density at u- and v-points ==!
         zgru(ji,jj,iikm1) = umask(ji,jj,jpkm1) * ( prd(ji+1,jj  ,jpkm1) - prd(ji,jj,jpkm1) )
         zgrv(ji,jj,iikm1) = vmask(ji,jj,jpkm1) * ( prd(ji  ,jj+1,jpkm1) - prd(ji,jj,jpkm1) )
      END_2D
      !
      zdzr(:,:) = 0._wp             !==   bottom local vertical density gradient at T-point   == !
      !
      !                               !----------------------!
      DO jk = jpkm1, 2, -1            !-  Horizontal slice  -!
         !                            !----------------------!
         !
         itmp = iik   ;   iik = iikm1   ;   iikm1 = itmp   ! swap iik-index
         !
         DO_2D( 2, 1, 2, 1 )
            !                       !==   jk: i- & j-gradient of density  ==!
            zgru(ji,jj,iikm1) = umask(ji,jj,jk-1) * ( prd(ji+1,jj  ,jk-1) - prd(ji,jj,jk-1) )
            zgrv(ji,jj,iikm1) = vmask(ji,jj,jk-1) * ( prd(ji  ,jj+1,jk-1) - prd(ji,jj,jk-1) )
            !
         END_2D
         !                          !==   jk: Local vertical density gradient at T-point   == !
         DO_2D( 1, 2, 1, 2 )
            !                                ! zdzr = d/dz(prd)= - ( prd ) / grav * mk(pn2) -- at t point
            !                                !   trick: tmask(ik  )  = 0   =>   all pn2   = 0   =>   zdzr = 0
            !                                !    else  tmask(ik+1)  = 0   =>   pn2(ik+1) = 0   =>   zdzr divides by 1
            !                                !          umask(ik+1) /= 0   =>   all pn2  /= 0   =>   zdzr divides by 2
            !                                ! NB: 1/(tmask+1) = (1-.5*tmask)  substitute a / by a *  ==> faster
            zdzr(ji,jj) = zm1_g * ( prd(ji,jj,jk) + 1._wp )              &
               &                * ( pn2(ji,jj,jk) + pn2(ji,jj,jk+1) ) * ( 1._wp - 0.5_wp * tmask(ji,jj,jk+1) )
         END_2D
         !
         !
         ! Cray compiler creates faulty code at vector optimisation levels >= vector1. Possibly it 
         ! fails to spot the dependency on previous levels either through the zgr[u,v](:,:,[1,2]) 
         ! toggling or the re-use of zwslp[i,j]_hml(:,:) ? Either way keep this next directive
         ! and its companion following the 'END_2D' macro
         !dir$ NOVECTOR
         !                     !==================================!
         DO_2D( 1, 1, 1, 1 )   !==   Slopes at u and v points   ==!
            !                  !==================================!
            !                                              ! horizontal and vertical density gradient at u- and v-points
            zau = zgru(ji,jj,iik) * r1_e1u(ji,jj)
            zav = zgrv(ji,jj,iik) * r1_e2v(ji,jj)
            zbu = 0.5_wp * ( zdzr(ji,jj) + zdzr(ji+1,jj  ) )
            zbv = 0.5_wp * ( zdzr(ji,jj) + zdzr(ji  ,jj+1) )
            !                                              ! bound the slopes: abs(zw.)<= 1/100 and zb..<0
            !                                              ! + kxz max= ah slope max =< e1 e3 /(pi**2 2 dt)
            zbu = MIN(  zbu, - z1_slpmax * ABS( zau ) , -7.e+3_wp/e3u(ji,jj,jk,Kmm)* ABS( zau )  )
            zbv = MIN(  zbv, - z1_slpmax * ABS( zav ) , -7.e+3_wp/e3v(ji,jj,jk,Kmm)* ABS( zav )  )
            !
            !                       !==  slp=dxR(jk)/dzR(jk) under ml   slp=dep(jk)/hml*(dxR/dzR)_ml in ml ==!
            iku = MAX( nmln(ji+1,jj), nmln(ji,jj) )        ! mix-layer index
            ikv = MAX( nmln(ji,jj+1), nmln(ji,jj) )
            !                                              ! zfi/j=0 in the mix-layer otherwise zfi/j=1
            zfi = REAL( 1 - 1/(1 + jk / iku ), wp )
            zfj = REAL( 1 - 1/(1 + jk / ikv ), wp )
            !                                              ! zmi/j=1 when jk=nmln otherwise zmi/j=0
            zmli = REAL(  1/( 1 + jk / (iku + 1) )  - 1/( 1 + jk / iku ), wp  )
            zmlj = REAL(  1/( 1 + jk / (ikv + 1) )  - 1/( 1 + jk / ikv ), wp  )
            !
            !                                              ! thickness of water column between surface and level k at u/v point
            zdepu = 0.5_wp * ( ( gdept(ji,jj,jk,Kmm) + gdept(ji+1,jj,jk,Kmm) )   &
               &              - 2 * MAX( risfdep(ji,jj), risfdep(ji+1,jj) )      &
               &              - e3u(ji,jj,miku(ji,jj),Kmm)   )
            zdepv = 0.5_wp * ( ( gdept(ji,jj,jk,Kmm) + gdept(ji,jj+1,jk,Kmm) )   &
               &              - 2 * MAX( risfdep(ji,jj), risfdep(ji,jj+1) )      &
               &              - e3v(ji,jj,mikv(ji,jj),Kmm)   )
            !                                              ! slp at jk level
            zwz(ji,jj) = ( zfi * zau / ( zbu - zeps ) + ( 1._wp - zfi ) * zdepu * zuslp_hml(ji,jj) ) * umask(ji,jj,jk)
            zww(ji,jj) = ( zfj * zav / ( zbv - zeps ) + ( 1._wp - zfj ) * zdepv * zvslp_hml(ji,jj) ) * vmask(ji,jj,jk)
            !                                              ! store 1/hml*(dxR/dzR)_ml at nmln level
            zuslp_hml(ji,jj) = zmli * zwz(ji,jj) * r1_hmlu(ji,jj) + ( 1._wp - zmli ) * zuslp_hml(ji,jj)
            zvslp_hml(ji,jj) = zmlj * zww(ji,jj) * r1_hmlv(ji,jj) + ( 1._wp - zmlj ) * zvslp_hml(ji,jj)
         END_2D
         !dir$ VECTOR
         !
         !                          !==  horizontal Shapiro filter + decrease along coastal boundaries  ==!
         DO_2D( 0, 0, 0, 0 )                                 ! rows jj=2 and =jpjm1 only
            uslp(ji,jj,jk) = z1_16 * (      ( ( zwz(ji-1,jj-1) + zwz(ji+1,jj-1) )      &   ! need additional () for
               &                       +      ( zwz(ji-1,jj+1) + zwz(ji+1,jj+1) ) )    &   ! reproducibility around NP
               &                       + 2.*( ( zwz(ji  ,jj-1) + zwz(ji-1,jj  ) )      &
               &                       +      ( zwz(ji+1,jj  ) + zwz(ji  ,jj+1) ) )    &
               &                       + 4.*    zwz(ji  ,jj  )                      )  &
               &                   * ( umask(ji,jj+1,jk) + umask(ji,jj-1,jk  ) ) * 0.5_wp   &
               &                   * ( umask(ji,jj  ,jk) + umask(ji,jj  ,jk+1) ) * 0.5_wp
            vslp(ji,jj,jk) = z1_16 * (      ( ( zww(ji-1,jj-1) + zww(ji+1,jj-1) )      &
               &                       +      ( zww(ji-1,jj+1) + zww(ji+1,jj+1) ) )    &
               &                       + 2.*( ( zww(ji  ,jj-1) + zww(ji-1,jj  ) )      &
               &                       +      ( zww(ji+1,jj  ) + zww(ji  ,jj+1) ) )    &
               &                       + 4.*    zww(ji,jj    )                      )  &
               &                   * ( vmask(ji+1,jj,jk) + vmask(ji-1,jj,jk  ) ) * 0.5_wp   &
               &                   * ( vmask(ji  ,jj,jk) + vmask(ji  ,jj,jk+1) ) * 0.5_wp
         END_2D
         !
         ! Cray compiler creates faulty code at vector optimisation levels >= vector1. Possibly it 
         ! fails to spot the dependency on previous levels either through the zgr[u,v](:,:,[1,2]) 
         ! toggling or the re-use of zwslp[i,j]_hml(:,:) ? Either way keep this next directive
         ! and its companion following the 'END_2D' macro
         !dir$ NOVECTOR
         !                      !============================!
         DO_2D( 1, 1, 1, 1 )    !==   Slopes at w points   ==!
            !                   !============================!
            !                       !==  Local vertical density gradient evaluated from N^2  ==!
            zbw = zm1_2g * pn2 (ji,jj,jk) * ( prd (ji,jj,jk) + prd (ji,jj,jk-1) + 2. )
            !                       !==  Slopes at w point
            !                                        ! i- & j-gradient of density at w-points  ==!
            zci = MAX(  umask(ji-1,jj,jk   ) + umask(ji,jj,jk   )           &
               &      + umask(ji-1,jj,jk-1 ) + umask(ji,jj,jk-1 ) , zeps  ) * e1t(ji,jj)
            zcj = MAX(  vmask(ji,jj-1,jk   ) + vmask(ji,jj,jk-1 )           &
               &      + vmask(ji,jj-1,jk-1 ) + vmask(ji,jj,jk   ) , zeps  ) * e2t(ji,jj)
            zai =    (  ( zgru (ji-1,jj,iik  ) + zgru (ji,jj,iik  ) )           &     ! need additional () for reproducibility around NP
               &      + ( zgru (ji-1,jj,iikm1) + zgru (ji,jj,iikm1) )   ) / zci * wmask (ji,jj,jk)
            zaj =    (  ( zgrv (ji,jj-1,iik  ) + zgrv (ji,jj,iikm1) )           &
               &      + ( zgrv (ji,jj-1,iikm1) + zgrv (ji,jj,iik  ) )   ) / zcj * wmask (ji,jj,jk)
            !                                        ! bound the slopes: abs(zw.)<= 1/100 and zb..<0.
            !                                        ! + kxz max= ah slope max =< e1 e3 /(pi**2 2 dt)
            zbi = MIN( zbw ,- 100._wp* ABS( zai ) , -7.e+3_wp/e3w(ji,jj,jk,Kmm)* ABS( zai )  )
            zbj = MIN( zbw , -100._wp* ABS( zaj ) , -7.e+3_wp/e3w(ji,jj,jk,Kmm)* ABS( zaj )  )
            !
            !                                        ! zfk=0 in the mix-layer otherwise zfk=1
            zfk = REAL(   1 - 1/(  1 + jk / ( nmln(ji,jj) + 1 )  ) , wp   )
            !                                        ! zmlk=1 when jk=nmln+1 otherwise zmlk=0
            zmlk = REAL(  1/( 1 + jk / ( nmln(ji,jj) + 2 ) )  - 1/( 1 + jk / ( nmln(ji,jj) + 1 ) ), wp  )
            !
            !                                        ! thickness of water column between surface and level k at w point
            zck = ( gdepw(ji,jj,jk,Kmm) - gdepw(ji,jj,mikt(ji,jj),Kmm) )
            !                                        ! wslpi and wslpj with ML flattening (output in zwz and zww, resp.)
            zwz(ji,jj) = (  zfk * zai / ( zbi - zeps ) + ( 1._wp - zfk ) * zck * zwslpi_hml(ji,jj)  ) * wmask(ji,jj,jk)
            zww(ji,jj) = (  zfk * zaj / ( zbj - zeps ) + ( 1._wp - zfk ) * zck * zwslpj_hml(ji,jj)  ) * wmask(ji,jj,jk)
            !
            !                                        ! store 1/hml*(dxR/dzR)_ml at nmln+1 level (1st level above lower T-point in ML)
            zwslpi_hml(ji,jj) = zmlk * zwz(ji,jj) * r1_hmlw(ji,jj) + ( 1._wp - zmlk ) * zwslpi_hml(ji,jj)
            zwslpj_hml(ji,jj) = zmlk * zww(ji,jj) * r1_hmlw(ji,jj) + ( 1._wp - zmlk ) * zwslpj_hml(ji,jj)
         END_2D
         !dir$ VECTOR
         !                           !== horizontal Shapiro filter + decrease in vicinity of topography  ==!
         DO_2D( 0, 0, 0, 0 )                             ! rows jj=2 and =jpjm1 only
            zcofw = wmask(ji,jj,jk) * z1_16 * ( umask(ji,jj,jk) + umask(ji-1,jj,jk) )   &
               &                            * ( vmask(ji,jj,jk) + vmask(ji,jj-1,jk) ) * 0.25
            wslpi(ji,jj,jk) = (       ( ( zwz(ji-1,jj-1) + zwz(ji+1,jj-1) )     &   ! need additional () for
                 &               +      ( zwz(ji-1,jj+1) + zwz(ji+1,jj+1) ) )   &   ! reproducibility around NP
                 &               + 2.*( ( zwz(ji  ,jj-1) + zwz(ji-1,jj  ) )     &
                 &               +      ( zwz(ji+1,jj  ) + zwz(ji  ,jj+1) ) )   &
                 &               + 4.*    zwz(ji  ,jj  )                        ) * zcofw

            wslpj(ji,jj,jk) = (       ( ( zww(ji-1,jj-1) + zww(ji+1,jj-1) )     &
                 &               +      ( zww(ji-1,jj+1) + zww(ji+1,jj+1) ) )   &
                 &               + 2.*( ( zww(ji  ,jj-1) + zww(ji-1,jj  ) )     &
                 &               +      ( zww(ji+1,jj  ) + zww(ji  ,jj+1) ) )   &
                 &               + 4.*    zww(ji  ,jj  )                        ) * zcofw
         END_2D
         !
      END DO  ! end jk
      !                              !==  Lateral boundary conditions  ==!
      CALL lbc_lnk( 'ldfslp', uslp , 'U', -1.0_wp , vslp , 'V', -1.0_wp , wslpi, 'W', -1.0_wp, wslpj, 'W', -1.0_wp )
      !
      IF(sn_cfctl%l_prtctl) THEN
         CALL prt_ctl(tab3d_1=uslp , clinfo1=' slp  - u : ', tab3d_2=vslp,  clinfo2=' v : ')
         CALL prt_ctl(tab3d_1=wslpi, clinfo1=' slp  - wi: ', tab3d_2=wslpj, clinfo2=' wj: ')
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('ldf_slp')
      !
   END SUBROUTINE ldf_slp


   SUBROUTINE ldf_slp_triad ( kt, Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE ldf_slp_triad  ***
      !!
      !! ** Purpose :   Compute the squared slopes of neutral surfaces (slope
      !!      of iso-pycnal surfaces referenced locally) (ln_traldf_triad=T)
      !!      at W-points using the Griffies quarter-cells.
      !!
      !! ** Method  :   calculates alpha and beta at T-points
      !!
      !! ** Action : - triadi_g, triadj_g   T-pts i- and j-slope triads relative to geopot. (used for eiv)
      !!             - triadi , triadj    T-pts i- and j-slope triads relative to model-coordinate
      !!             - wslp2              squared slope of neutral surfaces at w-points.
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt             ! ocean time-step index
      INTEGER , INTENT(in)  ::   Kbb, Kmm       ! ocean time level indices
      !!
      INTEGER  ::   ji, jj, jk, jl, ip, jp, kp  ! dummy loop indices
      INTEGER  ::   iku, ikv                    ! local integer
      REAL(wp) ::   zfacti, zfactj              ! local scalars
      REAL(wp) ::   znot_thru_surface           ! local scalars
      REAL(wp) ::   zdit, zdis, zdkt, zbu, zbti, zisw
      REAL(wp) ::   zdjt, zdjs, zdks, zbv, zbtj, zjsw
      REAL(wp) ::   zdxrho_raw, zti_coord, zti_raw, zti_lim, zti_g_raw, zti_g_lim
      REAL(wp) ::   zdyrho_raw, ztj_coord, ztj_raw, ztj_lim, ztj_g_raw, ztj_g_lim
      REAL(wp) ::   zdzrho_raw
      REAL(wp) ::   zbeta0, ze3_e1, ze3_e2
      REAL(wp), DIMENSION(jpi,jpj)     ::   z1_mlbw
      REAL(wp), DIMENSION(jpi,jpj,jpk,0:1) ::   zdxrho , zdyrho, zdzrho     ! Horizontal and vertical density gradients
      REAL(wp), DIMENSION(jpi,jpj,0:1,0:1) ::   zti_mlb, ztj_mlb            ! for Griffies operator only
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('ldf_slp_triad')
      !
      !--------------------------------!
      !  Some preliminary calculation  !
      !--------------------------------!
      !
      DO jl = 0, 1                            !==  unmasked before density i- j-, k-gradients  ==!
         !
         ip = jl   ;   jp = jl                ! guaranteed nonzero gradients ( absolute value larger than repsln)
         DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )        ! done each pair of triad ! NB: not masked ==>  a minimum value is set
            zdit = ( ts(ji+1,jj,jk,jp_tem,Kbb) - ts(ji,jj,jk,jp_tem,Kbb) )    ! i-gradient of T & S at u-point
            zdis = ( ts(ji+1,jj,jk,jp_sal,Kbb) - ts(ji,jj,jk,jp_sal,Kbb) )
            zdjt = ( ts(ji,jj+1,jk,jp_tem,Kbb) - ts(ji,jj,jk,jp_tem,Kbb) )    ! j-gradient of T & S at v-point
            zdjs = ( ts(ji,jj+1,jk,jp_sal,Kbb) - ts(ji,jj,jk,jp_sal,Kbb) )
            zdxrho_raw = ( - rab_b(ji+ip,jj   ,jk,jp_tem) * zdit + rab_b(ji+ip,jj   ,jk,jp_sal) * zdis ) * r1_e1u(ji,jj)
            zdyrho_raw = ( - rab_b(ji   ,jj+jp,jk,jp_tem) * zdjt + rab_b(ji   ,jj+jp,jk,jp_sal) * zdjs ) * r1_e2v(ji,jj)
            zdxrho(ji+ip,jj   ,jk,1-ip) = SIGN(  MAX( repsln, ABS( zdxrho_raw ) ), zdxrho_raw  )   ! keep the sign
            zdyrho(ji   ,jj+jp,jk,1-jp) = SIGN(  MAX( repsln, ABS( zdyrho_raw ) ), zdyrho_raw  )
         END_3D
         !
      END DO

      DO kp = 0, 1                            !==  unmasked before density i- j-, k-gradients  ==!
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )      ! done each pair of triad ! NB: not masked ==>  a minimum value is set
            IF( jk+kp > 1 ) THEN              ! k-gradient of T & S a jk+kp
               zdkt = ( ts(ji,jj,jk+kp-1,jp_tem,Kbb) - ts(ji,jj,jk+kp,jp_tem,Kbb) )
               zdks = ( ts(ji,jj,jk+kp-1,jp_sal,Kbb) - ts(ji,jj,jk+kp,jp_sal,Kbb) )
            ELSE
               zdkt = 0._wp                                             ! 1st level gradient set to zero
               zdks = 0._wp
            ENDIF
            zdzrho_raw = ( - rab_b(ji,jj,jk   ,jp_tem) * zdkt & 
                       &   + rab_b(ji,jj,jk   ,jp_sal) * zdks &
                       & ) / e3w(ji,jj,jk+kp,Kmm)  
            zdzrho(ji,jj,jk,kp) = - MIN( - repsln , zdzrho_raw )    ! force zdzrho >= repsln
         END_3D
      END DO

      ! nmln calculation in zdfmxl is only on internal points
      DO_2D( 0, 0, 0, 0 )
         z1_mlbw(ji,jj) = REAL( nmln(ji,jj), wp )
      END_2D
      CALL lbc_lnk( 'ldfslp', z1_mlbw, 'T', 1.0_wp, kfillmode=jpfillcopy )  ! No 0 over closed boundaries
      nmln(:,:) = NINT( z1_mlbw(:,:) )
      !
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )                   !== Reciprocal depth of the w-point below ML base  ==!
         jk = MIN( nmln(ji,jj), mbkt(ji,jj) ) + 1     ! MIN in case ML depth is the ocean depth
         z1_mlbw(ji,jj) = 1._wp / gdepw(ji,jj,jk,Kmm)
      END_2D
      !
      !                                       !==  intialisations to zero  ==!
      !
      wslp2   (:,:,:)     = 0._wp             ! wslp2 will be cumulated 3D field set to zero
      triadi_g(:,:,:,:,:) = 0._wp             ! set surface and bottom slope to zero, fill undefined points at jpi/jpj
      triadj_g(:,:,:,:,:) = 0._wp
      !!gm _iso set to zero missing
      triadi  (:,:,:,:,:) = 0._wp
      triadj  (:,:,:,:,:) = 0._wp

      !-------------------------------------!
      !  Triads just below the Mixed Layer  !
      !-------------------------------------!
      !
      DO jl = 0, 1                            ! calculate slope of the 4 triads immediately ONE level below mixed-layer base
         DO kp = 0, 1                         ! with only the slope-max limit   and   MASKED
            DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
               ip = jl   ;   jp = jl
               !
               jk = nmln(ji+ip,jj) + 1
               IF( jk > mbkt(ji+ip,jj) ) THEN   ! ML reaches bottom
                  zti_mlb(ji+ip,jj   ,1-ip,kp) = 0.0_wp
               ELSE                             
                  ! Add s-coordinate slope at t-points (do this by *subtracting* gradient of depth)
                  zti_g_raw = (  zdxrho(ji+ip,jj,jk-kp,1-ip) / zdzrho(ji+ip,jj,jk-kp,kp)      &
                     &          - ( gdept(ji+1,jj,jk-kp,Kmm) - gdept(ji,jj,jk-kp,Kmm) ) * r1_e1u(ji,jj)  ) * umask(ji,jj,jk)
                  ze3_e1    =  e3w(ji+ip,jj,jk-kp,Kmm) * r1_e1u(ji,jj) 
                  zti_mlb(ji+ip,jj   ,1-ip,kp) = SIGN( MIN( rn_slpmax, 5.0_wp * ze3_e1  , ABS( zti_g_raw ) ), zti_g_raw )
               ENDIF
               !
               jk = nmln(ji,jj+jp) + 1
               IF( jk >  mbkt(ji,jj+jp) ) THEN  !ML reaches bottom
                  ztj_mlb(ji   ,jj+jp,1-jp,kp) = 0.0_wp
               ELSE
                  ztj_g_raw = (  zdyrho(ji,jj+jp,jk-kp,1-jp) / zdzrho(ji,jj+jp,jk-kp,kp)      &
                     &      - ( gdept(ji,jj+1,jk-kp,Kmm) - gdept(ji,jj,jk-kp,Kmm) ) / e2v(ji,jj)  ) * vmask(ji,jj,jk)
                  ze3_e2    =  e3w(ji,jj+jp,jk-kp,Kmm) / e2v(ji,jj)
                  ztj_mlb(ji   ,jj+jp,1-jp,kp) = SIGN( MIN( rn_slpmax, 5.0_wp * ze3_e2  , ABS( ztj_g_raw ) ), ztj_g_raw )
               ENDIF
            END_2D
         END DO
      END DO

      !-------------------------------------!
      !  Triads with surface limits         !
      !-------------------------------------!
      !
      DO kp = 0, 1                            ! k-index of triads
         DO jl = 0, 1
            ip = jl   ;   jp = jl             ! i- and j-indices of triads (i-k and j-k planes)
            DO jk = 1, jpkm1
               ! Must mask contribution to slope from dz/dx at constant s for triads jk=1,kp=0 that poke up though ocean surface
               znot_thru_surface = REAL( 1-1/(jk+kp), wp )  !jk+kp=1,=0.; otherwise=1.0
               DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
                  !
                  ! Calculate slope relative to geopotentials used for GM skew fluxes
                  ! Add s-coordinate slope at t-points (do this by *subtracting* gradient of depth)
                  ! Limit by slope *relative to geopotentials* by rn_slpmax, and mask by psi-point
                  ! masked by umask taken at the level of dz(rho)
                  !
                  ! raw slopes: unmasked unbounded slopes (relative to geopotential (zti_g) and model surface (zti)
                  !
                  zti_raw   = zdxrho(ji+ip,jj   ,jk,1-ip) / zdzrho(ji+ip,jj   ,jk,kp)                   ! unmasked
                  ztj_raw   = zdyrho(ji   ,jj+jp,jk,1-jp) / zdzrho(ji   ,jj+jp,jk,kp)
                  !
                  ! Must mask contribution to slope for triad jk=1,kp=0 that poke up though ocean surface
                  zti_coord = znot_thru_surface * ( gdept(ji+1,jj  ,jk,Kmm) - gdept(ji,jj,jk,Kmm) ) * r1_e1u(ji,jj)
                  ztj_coord = znot_thru_surface * ( gdept(ji  ,jj+1,jk,Kmm) - gdept(ji,jj,jk,Kmm) ) * r1_e2v(ji,jj)     ! unmasked
                  zti_g_raw = zti_raw - zti_coord      ! ref to geopot surfaces
                  ztj_g_raw = ztj_raw - ztj_coord
                  ! additional limit required in bilaplacian case
                  ze3_e1    = e3w(ji+ip,jj   ,jk+kp,Kmm) * r1_e1u(ji,jj)
                  ze3_e2    = e3w(ji   ,jj+jp,jk+kp,Kmm) * r1_e2v(ji,jj)
                  ! NB: hard coded factor 5 (can be a namelist parameter...)
                  zti_g_lim = SIGN( MIN( rn_slpmax, 5.0_wp * ze3_e1, ABS( zti_g_raw ) ), zti_g_raw )
                  ztj_g_lim = SIGN( MIN( rn_slpmax, 5.0_wp * ze3_e2, ABS( ztj_g_raw ) ), ztj_g_raw )
                  !
                  ! Below  ML use limited zti_g as is & mask
                  ! Inside ML replace by linearly reducing sx_mlb towards surface & mask
                  !
                  zfacti = REAL( 1 - 1/(1 + (jk+kp-1)/nmln(ji+ip,jj)), wp )  ! k index of uppermost point(s) of triad is jk+kp-1
                  zfactj = REAL( 1 - 1/(1 + (jk+kp-1)/nmln(ji,jj+jp)), wp )  ! must be .ge. nmln(ji,jj) for zfact=1
                  !                                                          !                   otherwise  zfact=0
                  zti_g_lim =          ( zfacti   * zti_g_lim                       &
                     &      + ( 1._wp - zfacti ) * zti_mlb(ji+ip,jj,1-ip,kp)   &
                     &                           * gdepw(ji+ip,jj,jk+kp,Kmm) * z1_mlbw(ji+ip,jj) ) * umask(ji,jj,jk+kp)
                  ztj_g_lim =          ( zfactj   * ztj_g_lim                       &
                     &      + ( 1._wp - zfactj ) * ztj_mlb(ji,jj+jp,1-jp,kp)   &
                     &                           * gdepw(ji,jj+jp,jk+kp,Kmm) * z1_mlbw(ji,jj+jp) ) * vmask(ji,jj,jk+kp)
                  !
                  triadi_g(ji+ip,jj   ,jk,1-ip,kp) = zti_g_lim
                  triadj_g(ji   ,jj+jp,jk,1-jp,kp) = ztj_g_lim
                  !
                  ! Get coefficients of isoneutral diffusion tensor
                  ! 1. Utilise gradients *relative* to s-coordinate, so add t-point slopes (*subtract* depth gradients)
                  ! 2. We require that isoneutral diffusion  gives no vertical buoyancy flux
                  !     i.e. 33 term = (real slope* 31, 13 terms)
                  ! To do this, retain limited sx**2  in vertical flux, but divide by real slope for 13/31 terms
                  ! Equivalent to tapering A_iso = sx_limited**2/(real slope)**2
                  !
                  zti_lim  = ( zti_g_lim + zti_coord ) * umask(ji,jj,jk+kp)    ! remove coordinate slope => relative to coordinate surfaces
                  ztj_lim  = ( ztj_g_lim + ztj_coord ) * vmask(ji,jj,jk+kp)
                  !
                  IF( ln_triad_iso ) THEN
                     zti_raw = zti_lim*zti_lim / zti_raw
                     ztj_raw = ztj_lim*ztj_lim / ztj_raw
                     zti_raw = SIGN( MIN( ABS(zti_lim), ABS( zti_raw ) ), zti_raw )
                     ztj_raw = SIGN( MIN( ABS(ztj_lim), ABS( ztj_raw ) ), ztj_raw )
                     zti_lim = zfacti * zti_lim + ( 1._wp - zfacti ) * zti_raw
                     ztj_lim = zfactj * ztj_lim + ( 1._wp - zfactj ) * ztj_raw
                  ENDIF
                  !                                      ! switching triad scheme 
                  zisw = (1._wp - rn_sw_triad ) + rn_sw_triad    &
                     &            * 2._wp * ABS( 0.5_wp - kp - ( 0.5_wp - ip ) * SIGN( 1._wp , zdxrho(ji+ip,jj,jk,1-ip) )  )
                  zjsw = (1._wp - rn_sw_triad ) + rn_sw_triad    &
                     &            * 2._wp * ABS( 0.5_wp - kp - ( 0.5_wp - jp ) * SIGN( 1._wp , zdyrho(ji,jj+jp,jk,1-jp) )  )
                  !
                  triadi(ji+ip,jj   ,jk,1-ip,kp) = zti_lim * zisw
                  triadj(ji   ,jj+jp,jk,1-jp,kp) = ztj_lim * zjsw
                  !
                  zbu  = e1e2u(ji   ,jj   ) * e3u(ji   ,jj   ,jk   ,Kmm)
                  zbv  = e1e2v(ji   ,jj   ) * e3v(ji   ,jj   ,jk   ,Kmm)
                  zbti = e1e2t(ji+ip,jj   ) * e3w(ji+ip,jj   ,jk+kp,Kmm)
                  zbtj = e1e2t(ji   ,jj+jp) * e3w(ji   ,jj+jp,jk+kp,Kmm)
                  !
                  wslp2(ji+ip,jj,jk+kp) = wslp2(ji+ip,jj,jk+kp) + 0.25_wp * zbu / zbti * zti_g_lim*zti_g_lim      ! masked
                  wslp2(ji,jj+jp,jk+kp) = wslp2(ji,jj+jp,jk+kp) + 0.25_wp * zbv / zbtj * ztj_g_lim*ztj_g_lim
               END_2D
            END DO
         END DO
      END DO
      !
      wslp2(:,:,1) = 0._wp                ! force the surface wslp to zero

      CALL lbc_lnk( 'ldfslp', wslp2, 'W', 1.0_wp )      ! lateral boundary confition on wslp2 only   ==>>> gm : necessary ? to be checked
      !
      IF( ln_timing )   CALL timing_stop('ldf_slp_triad')
      !
   END SUBROUTINE ldf_slp_triad


   SUBROUTINE ldf_slp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ldf_slp_init  ***
      !!
      !! ** Purpose :   Initialization for the isopycnal slopes computation
      !!
      !! ** Method  :   
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      INTEGER ::   ierr         ! local integer
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'ldf_slp_init : direction of lateral mixing'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      ALLOCATE( ah_wslp2(jpi,jpj,jpk) , akz(jpi,jpj,jpk) , STAT=ierr )
      IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'ldf_slp_init : unable to allocate ah_slp2 or akz' )
      !
      DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpk )
         akz     (ji,jj,jk) = 0._wp
         ah_wslp2(ji,jj,jk) = 0._wp
      END_3D
      !
      IF( ln_traldf_triad ) THEN        ! Griffies operator : triad of slopes
         IF(lwp) WRITE(numout,*) '   ==>>>   triad) operator (Griffies)'
         ALLOCATE( triadi_g(jpi,jpj,jpk,0:1,0:1) , triadj_g(jpi,jpj,jpk,0:1,0:1) ,     &
            &      triadi  (jpi,jpj,jpk,0:1,0:1) , triadj  (jpi,jpj,jpk,0:1,0:1) ,     &
            &      wslp2   (jpi,jpj,jpk)                                         , STAT=ierr )
         IF( ierr > 0      )   CALL ctl_stop( 'STOP', 'ldf_slp_init : unable to allocate Griffies operator slope' )
         IF( ln_dynldf_iso )   CALL ctl_stop( 'ldf_slp_init: Griffies operator on momentum not supported' )
         !
      ELSE                             ! Madec operator : slopes at u-, v-, and w-points
         IF(lwp) WRITE(numout,*) '   ==>>>   iso operator (Madec)'
         ALLOCATE( uslp(jpi,jpj,jpk) ,  wslpi(jpi,jpj,jpk) ,     &
            &      vslp(jpi,jpj,jpk) ,  wslpj(jpi,jpj,jpk) , STAT=ierr )
         IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'ldf_slp_init : unable to allocate Madec operator slope ' )

         ! Direction of lateral diffusion (tracers and/or momentum)
         ! ------------------------------
         uslp (:,:,:) = 0._wp      ! set the slope to zero (even in s-coordinates)
         vslp (:,:,:) = 0._wp
         wslpi(:,:,:) = 0._wp
         wslpj(:,:,:) = 0._wp

         !!gm I no longer understand this.....
!!gm         IF( (ln_traldf_hor .OR. ln_dynldf_hor) .AND. .NOT. (.NOT.lk_linssh .AND. ln_rstart) ) THEN
!            IF(lwp)   WRITE(numout,*) '          Horizontal mixing in s-coordinate: slope = slope of s-surfaces'
!
!            ! geopotential diffusion in s-coordinates on tracers and/or momentum
!            ! The slopes of s-surfaces are computed once (no call to ldfslp in step)
!            ! The slopes for momentum diffusion are i- or j- averaged of those on tracers
!
!            ! set the slope of diffusion to the slope of s-surfaces
!            !      ( c a u t i o n : minus sign as dep has positive value )
!            DO jk = 1, jpk
!               DO jj = 2, jpjm1
!                  DO ji = 2, jpim1   ! vector opt.
!                     uslp (ji,jj,jk) = - ( gdept(ji+1,jj,jk,Kmm) - gdept(ji ,jj ,jk,Kmm) ) * r1_e1u(ji,jj) * umask(ji,jj,jk)
!                     vslp (ji,jj,jk) = - ( gdept(ji,jj+1,jk,Kmm) - gdept(ji ,jj ,jk,Kmm) ) * r1_e2v(ji,jj) * vmask(ji,jj,jk)
!                     wslpi(ji,jj,jk) = - ( gdepw(ji+1,jj,jk,Kmm) - gdepw(ji-1,jj,jk,Kmm) ) * r1_e1t(ji,jj) * wmask(ji,jj,jk) * 0.5
!                     wslpj(ji,jj,jk) = - ( gdepw(ji,jj+1,jk,Kmm) - gdepw(ji,jj-1,jk,Kmm) ) * r1_e2t(ji,jj) * wmask(ji,jj,jk) * 0.5
!                  END DO
!               END DO
!            END DO
!            CALL lbc_lnk( 'ldfslp', uslp , 'U', -1._wp ; CALL lbc_lnk( 'ldfslp', vslp , 'V', -1._wp,  wslpi, 'W', -1._wp,  wslpj, 'W', -1._wp )
!!gm         ENDIF
      ENDIF
      !
   END SUBROUTINE ldf_slp_init

   !!======================================================================
END MODULE ldfslp
