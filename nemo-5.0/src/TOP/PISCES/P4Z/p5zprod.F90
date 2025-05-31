MODULE p5zprod
   !!======================================================================
   !!                         ***  MODULE p5zprod  ***
   !! TOP :  Growth Rate of the three phytoplanktons groups 
   !!        PISCES-QUOTA version of the module
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-05  (O. Aumont, C. Ethe) New parameterization of light limitation
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_prod       :   Compute the growth Rate of the two phytoplanktons groups
   !!   p5z_prod_init  :   Initialization of the parameters for growth
   !!   p5z_prod_alloc :   Allocate variables for growth
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p2zlim
   USE p4zlim
   USE p5zlim          !  Co-limitations of differents nutrients
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager
   USE lib_fortran     ! Fortran routines library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_prod         ! called in p5zbio.F90
   PUBLIC   p5z_prod_init    ! called in trcsms_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  pislopen        !: P-I slope of nanophytoplankton
   REAL(wp), PUBLIC ::  pislopep        !: P-I slope of picophytoplankton
   REAL(wp), PUBLIC ::  pisloped        !: P-I slope of diatoms
   REAL(wp), PUBLIC ::  excretn         !: Excretion ratio of nanophyto
   REAL(wp), PUBLIC ::  excretp         !: Excretion ratio of picophyto
   REAL(wp), PUBLIC ::  excretd         !: Excretion ratio of diatoms
   REAL(wp), PUBLIC ::  bresp           !: Basal respiration rate
   REAL(wp), PUBLIC ::  chlcmin         !: Minimum Chl/C ratio of phytoplankton
   REAL(wp), PUBLIC ::  grosip          !: Mean Si/C ratio of diatoms

   REAL(wp) :: r1_rday     !: 1 / rday
   REAL(wp) :: texcretn    !: 1 - excretn 
   REAL(wp) :: texcretp    !: 1 - excretp 
   REAL(wp) :: texcretd    !: 1 - excretd        
   REAL(wp) :: xq10_n      !: q10 coef for nano =  1. + xpsino3 * qnnmax
   REAL(wp) :: xq10_p      !: q10 coef for pico =  1. + xpsino3 * qnpmax
   REAL(wp) :: xq10_d      !: q10 coef for diat =  1. + xpsino3 * qndmax

   LOGICAL  :: l_dia_pp, l_dia_mu, l_dia_light, l_dia_lprod

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p5z_prod( kt , knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_prod  ***
      !!
      !! ** Purpose :   Compute the phytoplankton production depending on
      !!              light, temperature and nutrient availability
      !!              Computes also the uptake of nutrients. PISCES-quota
      !!              relies on a full quota formalism
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) :: kt, knt
      INTEGER, INTENT(in) :: Kbb, Kmm, Krhs      ! time level indices
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zsilfac, znanotot, zpicotot, zdiattot
      REAL(wp) ::   zration, zratiop, zratiof, zmax
      REAL(wp) ::   zprofmax, zratio, zratio2
      REAL(wp) ::   zpronewn, zpronewp, zpronewd
      REAL(wp) ::   zproregn, zproregp, zproregd
      REAL(wp) ::   zpropo4n, zpropo4p, zpropo4d
      REAL(wp) ::   zprodopn, zprodopp, zprodopd
      REAL(wp) ::   zlim, zsiborn, zprod, zprod1, zprontot, zproptot, zprodtot
      REAL(wp) ::   zproddoc, zproddon, zproddop, zprodsil, zprodfer, zprodlig
      REAL(wp) ::   zprnutmax, zprochln, zprochld, zprochlp
      REAL(wp) ::   zpislopen, zpislopep, zpisloped
      REAL(wp) ::   zval, zpo4tot, zpptot, zpnewtot, zpregtot
      REAL(wp) ::   zmxl_chl, zmxl_fac_nano,zmxl_fac_diat
      REAL(wp) ::   zqfpmax, zqfnmax, zqfdmax
      REAL(wp) ::   zfact, zrfact2, zmaxsi, zratiosi, zratiosi_4
      REAL(wp) ::   zsizetmp, zlimfac, zlimfac3, zsilim
      REAL(wp) ::   zpislopeadn, zpislopeadp, zpislopeadd
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprorcan, zprorcap, zprorcad
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprnut, zprbio, zprpic, zprdia, zysopt
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprchln, zprchlp, zprchld
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprofed, zprofep, zprofen
      REAL(wp), DIMENSION(A2D(0),jpk) :: zpronmaxn, zpronmaxp,zpronmaxd
      REAL(wp), DIMENSION(A2D(0),jpk) :: zpropmaxn, zpropmaxp,zpropmaxd
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprmaxn, zprmaxd, zprmaxp, zmxl
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_prod')
      !
      IF( kt == nittrc000 ) THEN
         l_dia_pp =  iom_use( "PPPHYN" ) .OR. iom_use( "PPPHYD" ) .OR. iom_use( "PPPHYP" ) &
             &  .OR. iom_use( "TPP"  )   .OR. iom_use( "GPPHYN" ) .OR. iom_use( "GPPHYD" ) &
             &  .OR. iom_use( "GPPHYP" ) .OR. iom_use( "PPNEWN" ) .OR. iom_use( "PPNEWD" ) &
             &  .OR. iom_use( "PPNEWP" ) .OR. iom_use( "TPNEW")   .OR. iom_use( "PFeN"   ) &
             &  .OR. iom_use( "PFeD"   ) .OR. iom_use( "PFeP"   ) .OR. iom_use( "TPBFE")   &
             &  .OR. iom_use( "PBSi"   ) .OR. iom_use( "PPNEWo2") .OR. iom_use( "PPRego2" )
         l_dia_mu    = iom_use( "Mumax"  ) .OR. iom_use( "MuN"    ) .OR. iom_use( "MuD") .OR. iom_use( "MuP")
         l_dia_light = iom_use( "LNlight") .OR. iom_use( "LDlight") .OR. iom_use( "LPlight")
         l_dia_lprod = ln_ligand .AND. ( iom_use( "LPRODP") .OR. iom_use( "LDETP") )
      ENDIF

      ! Initialize the local arrays
      zprorcan (:,:,:) = 0._wp ; zprorcap (:,:,:) = 0._wp ; zprorcad (:,:,:) = 0._wp
      zprofen  (:,:,:) = 0._wp ; zprofep  (:,:,:) = 0._wp ; zprofed  (:,:,:) = 0._wp
      zprbio   (:,:,:) = 0._wp ; zprpic   (:,:,:) = 0._wp ; zprdia   (:,:,:) = 0._wp
      zpronmaxn(:,:,:) = 0._wp ; zpronmaxp(:,:,:) = 0._wp ; zpronmaxd(:,:,:) = 0._wp
      zpropmaxn(:,:,:) = 0._wp ; zpropmaxp(:,:,:) = 0._wp ; zpropmaxd(:,:,:) = 0._wp
      zmxl     (:,:,:) = 0._wp ; zysopt   (:,:,:) = 0._wp 
      
      ! Computation of the optimal production rates and nutrient uptake
      ! rates. Based on a Q10 description of the thermal dependency.
      zprnut (:,:,:) =  0.65_wp *          r1_rday * tgfunc(:,:,:)
      zprmaxn(:,:,:)  = 0.65_wp * xq10_n * r1_rday * tgfunc(:,:,:)
      zprmaxd(:,:,:)  = 0.65_wp * xq10_d * r1_rday * tgfunc(:,:,:)
      zprmaxp(:,:,:)  = 0.45_wp * xq10_p * r1_rday * tgfunc(:,:,:)

      ! Impact of the day duration and light intermittency on phytoplankton growth
      ! Intermittency is supposed to have a similar effect on production as 
      ! day length (Shatwell et al., 2012). The correcting factor is zmxl_fac. 
      ! zmxl_chl is the fractional day length and is used to compute the mean
      ! PAR during daytime. The effect of mixing is computed using the 
      ! absolute light level definition of the euphotic zone
      ! ------------------------------------------------------------------------- 

      IF ( ln_p4z_dcyc ) THEN    ! Diurnal cycle in PISCES
         DO_3D( 0, 0, 0, 0, 1, nksr )
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
               zval = 24.0
               IF( gdepw(ji,jj,jk+1,Kmm) <= hmld(ji,jj) ) THEN
                  zval = zval * MIN(1., heup_01(ji,jj) / ( hmld(ji,jj) + rtrn ))
               ENDIF
               zmxl(ji,jj,jk) = zval 
            ENDIF
         END_3D
      ELSE ! No diurnal cycle in PISCES
         DO_3D( 0, 0, 0, 0, 1, nksr )
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
               zval = MAX( 1., strn(ji,jj) )
               IF( gdepw(ji,jj,jk+1,Kmm) <= hmld(ji,jj) ) THEN
                  zval = zval * MIN(1., heup_01(ji,jj) / ( hmld(ji,jj) + rtrn ))
               ENDIF
               zmxl(ji,jj,jk) = zval 
            ENDIF
         END_3D

      ENDIF

      ! Computation of the P-I slope for nanos, picos and diatoms
      ! The formulation proposed by Geider et al. (1997) has been used.
      DO_3D( 0, 0, 0, 0, 1, nksr )
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zmxl_fac_nano = 1.0 - EXP( -0.25 * MAX(0., zmxl(ji,jj,jk) - 2. ) )
            zmxl_fac_diat = 1.0 - EXP( -0.25 * MAX(0., zmxl(ji,jj,jk) - 1. ) )
            zmxl_chl = zmxl(ji,jj,jk) / 24.
            zprbio(ji,jj,jk) = zprmaxn(ji,jj,jk) * zmxl_fac_nano
            zprdia(ji,jj,jk) = zprmaxd(ji,jj,jk) * zmxl_fac_diat
            zprpic(ji,jj,jk) = zprmaxp(ji,jj,jk) * zmxl_fac_nano

            ! Computation of the P-I slope for nanos and diatoms
            ! Nanophytoplankton
            zpislopeadn = pislopen * tr(ji,jj,jk,jpnch,Kbb)    &
              &                    / ( tr(ji,jj,jk,jpphy,Kbb) * 12. + rtrn)

            ! Picophytoplankton
            zpislopeadp = pislopep * tr(ji,jj,jk,jppch,Kbb)    &
              &                    / ( tr(ji,jj,jk,jppic,Kbb) * 12. + rtrn)

            ! Diatoms
            zpislopeadd = pisloped * tr(ji,jj,jk,jpdch,Kbb)    &
              &                    / ( tr(ji,jj,jk,jpdia,Kbb) * 12. + rtrn)
            !
            zpislopen = zpislopeadn / ( zprbio(ji,jj,jk) * rday * xlimphy(ji,jj,jk) + rtrn )
            zpislopep = zpislopeadp / ( zprpic(ji,jj,jk) * rday * xlimpic(ji,jj,jk) + rtrn )
            zpisloped = zpislopeadd / ( zprdia(ji,jj,jk) * rday * xlimdia(ji,jj,jk) + rtrn )

            ! Computation of production function for Carbon
            ! Actual light levels are used here 
            !  ---------------------------------------------
            zprbio(ji,jj,jk) = zprbio(ji,jj,jk) * ( 1.- EXP( -zpislopen * enano(ji,jj,jk) )  )
            zprpic(ji,jj,jk) = zprpic(ji,jj,jk) * ( 1.- EXP( -zpislopep * epico(ji,jj,jk) )  )
            zprdia(ji,jj,jk) = zprdia(ji,jj,jk) * ( 1.- EXP( -zpisloped * ediat(ji,jj,jk) )  )

            !  Computation of production function for Chlorophyll
            !  Mean light level in the mixed layer (when appropriate)
            !  is used here (acclimation is in general slower than 
            !  the characteristic time scales of vertical mixing)
            !  ------------------------------------------------------
            zpislopen = zpislopen * zmxl_fac_nano / ( zmxl_chl + rtrn )
            zpisloped = zpisloped * zmxl_fac_diat / ( zmxl_chl + rtrn )
            zpislopep = zpislopep * zmxl_fac_nano / ( zmxl_chl + rtrn )
            !
            zprchln(ji,jj,jk) = ( 1.- EXP( -zpislopen * enanom(ji,jj,jk) )  )
            zprchlp(ji,jj,jk) = ( 1.- EXP( -zpislopep * epicom(ji,jj,jk) )  )
            zprchld(ji,jj,jk) = ( 1.- EXP( -zpisloped * ediatm(ji,jj,jk) )  )
         ENDIF
      END_3D

      DO_3D( 0, 0, 0, 0, 1, nksr )
          IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !
            ! Si/C of diatoms
            ! ------------------------
            ! Si/C increases with iron stress and silicate availability (zsilfac)
            ! Si/C is arbitrariliy increased for very high Si concentrations
            ! to mimic the very high ratios observed in the Southern Ocean (zsilfac)
            ! A parameterization derived from Flynn (2003) is used for the control
            ! when Si is not limiting which is similar to the parameterisation
            ! proposed by Gurney and Davidson (1999).
            ! -----------------------------------------------------------------------
            zlim       = tr(ji,jj,jk,jpsil,Kbb) / ( tr(ji,jj,jk,jpsil,Kbb) + xksi1 )
            zsilim     = MIN(1.0, xlimdia(ji,jj,jk) * zprdia(ji,jj,jk) / ( zprnut(ji,jj,jk) + rtrn ) )
            zsiborn    = tr(ji,jj,jk,jpsil,Kbb) * tr(ji,jj,jk,jpsil,Kbb) * tr(ji,jj,jk,jpsil,Kbb)
            IF (gphit(ji,jj) < -30 ) THEN
              zsilfac  = 1. + 2. * zsiborn / ( zsiborn + xksi2_3 )
            ELSE
              zsilfac  = 1.
            ENDIF
            zratiosi   = 1.0 - tr(ji,jj,jk,jpdsi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) * zsilfac * grosip * 3.0 + rtrn )
            zratiosi   = MAX(0., zratiosi)
            zratiosi_4 = zratiosi * zratiosi * zratiosi * zratiosi 
            zmaxsi     = ( 1.0 + 1.E-4 ) * zratiosi_4 / ( zratiosi_4 + 1.E-4 )
            IF ( xlimsi(ji,jj,jk) /= xlimdia(ji,jj,jk) ) THEN
               zysopt(ji,jj,jk) = zlim * zsilfac * grosip * 1.0 * zmaxsi
            ELSE
               zysopt(ji,jj,jk) = zlim * zsilfac * grosip * 1.0 * zsilim**0.7 * zmaxsi
            ENDIF
         ENDIF
      END_3D

      ! Sea-ice effect on production
      ! No production is assumed below sea ice
      ! --------------------------------------
      DO_3D( 0, 0, 0, 0, 1, nksr )
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zprbio(ji,jj,jk) = zprbio(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
            zprdia(ji,jj,jk) = zprdia(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
            zprpic(ji,jj,jk) = zprpic(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
            zprnut(ji,jj,jk) = zprnut(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
         ENDIF
      END_3D

      DO_3D( 0, 0, 0, 0, 1, nksr)
         zlimfac  =  MIN(1.0, xlimpics(ji,jj,jk), zprpic(ji,jj,jk) / ( zprmaxp(ji,jj,jk) + rtrn ) )
         zsizetmp = MIN( xsizern, 1.0 + xsizern * (1.0 - COS(3.141/2. * MAX(0., zlimfac - 0.2) / 0.8)) )
         sizepa(ji,jj,jk) = sizep(ji,jj,jk) + MAX(zprpic(ji,jj,jk), 1E-2 / rday ) * xlimpics(ji,jj,jk) * ( 1.05 - xlimpics(ji,jj,jk) )   &
           &          * rfact2 * ( zsizetmp - sizep(ji,jj,jk) )
         sizepa(ji,jj,jk) = MIN( xsizerp, sizepa(ji,jj,jk) )
         zlimfac  =  MIN(1.0, xlimphys(ji,jj,jk), zprbio(ji,jj,jk) / ( zprmaxn(ji,jj,jk) + rtrn ) )
         zsizetmp = MIN( xsizern, 1.0 + xsizern * (1.0 - COS(3.141/2. * MAX(0., zlimfac - 0.2) / 0.8)) )
         sizena(ji,jj,jk) = sizen(ji,jj,jk) + MAX(zprbio(ji,jj,jk), 1E-2 / rday ) * xlimphys(ji,jj,jk) * ( 1.05 - xlimphys(ji,jj,jk) )   &
           &          * rfact2 * ( zsizetmp - sizen(ji,jj,jk) )
         sizena(ji,jj,jk) = MIN( xsizern, sizena(ji,jj,jk) )
         zlimfac  =  MIN(1.0, xlimdias(ji,jj,jk), zprdia(ji,jj,jk) / ( zprmaxd(ji,jj,jk) + rtrn ) )
         zsizetmp = MIN( xsizerd, 1.0 + xsizerd * (1.0 - COS(3.141/2. * MAX(0., zlimfac - 0.2) / 0.8)) )
         sizeda(ji,jj,jk) = sized(ji,jj,jk) + MAX(zprdia(ji,jj,jk), 1E-2 / rday ) * xlimdias(ji,jj,jk) * ( 1.05 - xlimdias(ji,jj,jk) )   &
           &          * rfact2 * ( zsizetmp - sized(ji,jj,jk) )
         sizeda(ji,jj,jk) = MIN( xsizerd, sizeda(ji,jj,jk) )
      END_3D



      ! Computation of the various production and uptake terms of nanophytoplankton 
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, nksr )
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !  production terms for nanophyto.
            zprorcan(ji,jj,jk) = zprbio(ji,jj,jk)  * xlimphy(ji,jj,jk) * tr(ji,jj,jk,jpphy,Kbb) * rfact2

            ! Maximum potential uptake rate
            zration   = tr(ji,jj,jk,jpnph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zratiop   = tr(ji,jj,jk,jppph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zratiof   = tr(ji,jj,jk,jpnfe,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvnuptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jpphy,Kbb) * rfact2
            ! Uptake of nitrogen
            zratio    = ( 1.0 * xqnnmax(ji,jj,jk) - zration ) / ( 1.0 * xqnnmax(ji,jj,jk) - xqnnmin(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zpronmaxn(ji,jj,jk) = 1.5 * zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpnmin(ji,jj,jk) )   &
              &         / ( xqpnmax(ji,jj,jk) - xqpnmin(ji,jj,jk) + rtrn ) ) ) * xlimnfe(ji,jj,jk)

            ! Uptake of phosphorus and DOP
            zratio    = ( 1.0 * xqpnmax(ji,jj,jk) - zratiop ) / ( 1.0 * xqpnmax(ji,jj,jk) - xqpnmin(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zpropmaxn(ji,jj,jk) = 1.5 * zprnutmax * zmax * xlimnfe(ji,jj,jk)
            ! Uptake of iron
            zqfnmax   = xqfuncfecn(ji,jj,jk) + ( qfnmax - xqfuncfecn(ji,jj,jk) ) * xlimnpn(ji,jj,jk)
            zratio    = ( 1.0 * zqfnmax - zratiof ) / ( 1.0 * zqfnmax - xqfuncfecn(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zprofmax  = zprnutmax * zqfnmax * zmax * 0.6
            zprofen(ji,jj,jk) = zprofmax * xnanofer(ji,jj,jk) * (1. + 0.8 * xnanono3(ji,jj,jk) / ( rtrn  &
              &         + xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk) ) * (1. - xnanofer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Computation of the various production and uptake terms of picophytoplankton 
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, nksr )
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !
            !  production terms for picophyto.
            zprorcap(ji,jj,jk) = zprpic(ji,jj,jk)  * xlimpic(ji,jj,jk) * tr(ji,jj,jk,jppic,Kbb) * rfact2

            ! Maximum potential uptake rate of nutrients
            zration   = tr(ji,jj,jk,jpnpi,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zratiop   = tr(ji,jj,jk,jpppi,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zratiof   = tr(ji,jj,jk,jppfe,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvpuptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jppic,Kbb) * rfact2

            ! Uptake of nitrogen
            zratio    = ( 1.0 * xqnpmax(ji,jj,jk) - zration ) / ( 1.0 * xqnpmax(ji,jj,jk) - xqnpmin(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zpronmaxp(ji,jj,jk) = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqppmin(ji,jj,jk) )   &
              &          / ( xqppmax(ji,jj,jk) - xqppmin(ji,jj,jk) + rtrn ) ) ) * xlimpfe(ji,jj,jk)

            ! Uptake of phosphorus
            zratio    = ( 1.0 * xqppmax(ji,jj,jk) - zratiop ) / ( 1.0 * xqppmax(ji,jj,jk) - xqppmin(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zpropmaxp(ji,jj,jk) = zprnutmax * zmax * xlimpfe(ji,jj,jk) 

            ! Uptake of iron
            zqfpmax   = xqfuncfecp(ji,jj,jk) + ( qfpmax - xqfuncfecp(ji,jj,jk) ) * xlimnpp(ji,jj,jk)
            zratio    = ( 1.0 * zqfpmax - zratiof ) / ( 1.0 * zqfpmax - xqfuncfecp(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zprofmax  = zprnutmax * zqfpmax * zmax * 0.5
            zprofep(ji,jj,jk) = zprofmax * xpicofer(ji,jj,jk) * (1. + 0.8 * xpicono3(ji,jj,jk) / ( rtrn   & 
              &          + xpicono3(ji,jj,jk) + xpiconh4(ji,jj,jk) ) * (1. - xpicofer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Computation of the various production and uptake terms of diatoms
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, nksr )
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !
            !  production terms for diatomees
            zprorcad(ji,jj,jk) = zprdia(ji,jj,jk) * xlimdia(ji,jj,jk) * tr(ji,jj,jk,jpdia,Kbb) * rfact2

            ! Maximum potential uptake rate of nutrients
            zration   = tr(ji,jj,jk,jpndi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zratiop   = tr(ji,jj,jk,jppdi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zratiof   = tr(ji,jj,jk,jpdfe,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvduptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jpdia,Kbb) * rfact2

            ! Uptake of nitrogen
            zratio    = ( 1.0 * xqndmax(ji,jj,jk) - zration ) / ( 1.0 * xqndmax(ji,jj,jk) - xqndmin(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zpronmaxd(ji,jj,jk) = 1.5 * zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpdmin(ji,jj,jk) )   &
              &          / ( xqpdmax(ji,jj,jk) - xqpdmin(ji,jj,jk) + rtrn ) ) ) * xlimdfe(ji,jj,jk)

            ! Uptake of phosphorus
            zratio    = ( 1.0 * xqpdmax(ji,jj,jk) - zratiop ) / ( 1.0 * xqpdmax(ji,jj,jk) - xqpdmin(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zpropmaxd(ji,jj,jk) = 1.5 * zprnutmax * zmax * xlimdfe(ji,jj,jk)

            ! Uptake of iron
            zqfdmax   = xqfuncfecd(ji,jj,jk) + ( qfdmax - xqfuncfecd(ji,jj,jk) ) * xlimnpd(ji,jj,jk)
            zratio    = ( 1.0 * zqfdmax - zratiof ) / ( 1.0 * zqfdmax - xqfuncfecd(ji,jj,jk) + rtrn )
            zratio    = MAX(0., zratio )
            zratio2   = zratio * zratio
            zmax      = zratio2 / (0.01 + zratio2)
            zprofmax  = zprnutmax * zqfdmax * zmax * 0.6
            zprofed(ji,jj,jk) = zprofmax * xdiatfer(ji,jj,jk)        &
              &          * (1. + 0.8 * xdiatno3(ji,jj,jk) / ( rtrn   &
              &          + xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk) ) * (1. - xdiatfer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Production of Chlorophyll. The formulation proposed by Pahlow and Oschlies 
      ! is adopted here.
      ! --------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, nksr )
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zmxl_chl = zmxl(ji,jj,jk) / 24.

            !  production terms for nanophyto. ( chlorophyll )
            zpronewn = zpronmaxn(ji,jj,jk) * xnanono3(ji,jj,jk)
            zproregn = zpronmaxn(ji,jj,jk) * xnanonh4(ji,jj,jk)
            znanotot = enanom(ji,jj,jk) / ( zmxl_chl + rtrn )
            zprod1   = ( zprorcan(ji,jj,jk) * texcretn - xpsinh4 * zproregn                              &
              &        - xpsino3 * zpronewn ) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zprod    = zprod1 / ratchln(ji,jj,jk) * ( pislopen * znanotot / ( zprmaxn(ji,jj,jk) * rday )            &
              &        * ( 1.0 - zprchln(ji,jj,jk) ) * MAX(0.0, (1.0 - ratchln(ji,jj,jk) * tr(ji,jj,jk,jpnch,Kbb)   &
              &        / ( 12. * tr(ji,jj,jk,jpphy,Kbb) * xlimphy(ji,jj,jk) + rtrn ) ) )                 &
              &        - ratchln(ji,jj,jk) * zprchln(ji,jj,jk) ) + zprod1
            zprochln = MAX(zprod * tr(ji,jj,jk,jpnch,Kbb), chlcmin * 12 * zprorcan(ji,jj,jk) )

            !  production terms for picophyto. ( chlorophyll )
            zpronewp = zpronmaxp(ji,jj,jk) * xpicono3(ji,jj,jk)
            zproregp = zpronmaxp(ji,jj,jk) * xpiconh4(ji,jj,jk)
            zpicotot = epicom(ji,jj,jk) / ( zmxl_chl + rtrn )
            zprod1   = ( zprorcap(ji,jj,jk) * texcretp - xpsinh4 * zproregp                              &
              &        - xpsino3 * zpronewp ) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zprod    = zprod1 / ratchlp(ji,jj,jk) * ( pislopep * zpicotot / ( zprmaxp(ji,jj,jk) * rday )            &
              &        * ( 1.0 - zprchlp(ji,jj,jk) ) * MAX(0.0, (1.0 - ratchlp(ji,jj,jk) * tr(ji,jj,jk,jppch,Kbb)   &
              &        / ( 12. * tr(ji,jj,jk,jppic,Kbb) * xlimpic(ji,jj,jk) + rtrn ) ) )                 &
              &        - ratchlp(ji,jj,jk) * zprchlp(ji,jj,jk) ) + zprod1
            zprochlp = MAX(zprod * tr(ji,jj,jk,jppch,Kbb) , chlcmin * 12 * zprorcap(ji,jj,jk) )

            !  production terms for diatoms ( chlorophyll )
            zpronewd = zpronmaxd(ji,jj,jk) * xdiatno3(ji,jj,jk)
            zproregd = zpronmaxd(ji,jj,jk) * xdiatnh4(ji,jj,jk)
            zdiattot = ediatm(ji,jj,jk) / ( zmxl_chl + rtrn )
            zprod1   = ( zprorcad(ji,jj,jk) * texcretd - xpsinh4 * zproregd                              &
              &        - xpsino3 * zpronewd ) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zprod    = zprod1 / ratchld(ji,jj,jk) * ( pisloped * zdiattot / ( zprmaxd(ji,jj,jk) * rday )            &
              &        * ( 1.0 - zprchld(ji,jj,jk) ) * MAX(0.0, (1.0 - ratchld(ji,jj,jk) * tr(ji,jj,jk,jpdch,Kbb)   &
              &        / ( 12. * tr(ji,jj,jk,jpdia,Kbb) * xlimdia(ji,jj,jk) + rtrn ) ) )                 &
              &        - ratchld(ji,jj,jk) * zprchld(ji,jj,jk) ) + zprod1
            zprochld = MAX(zprod * tr(ji,jj,jk,jpdch,Kbb) , chlcmin * 12 * zprorcad(ji,jj,jk) )

            !   Update the arrays TRA which contain the Chla sources and sinks
            tr(ji,jj,jk,jpnch,Krhs) = tr(ji,jj,jk,jpnch,Krhs) + zprochln
            tr(ji,jj,jk,jpdch,Krhs) = tr(ji,jj,jk,jpdch,Krhs) + zprochld
            tr(ji,jj,jk,jppch,Krhs) = tr(ji,jj,jk,jppch,Krhs) + zprochlp
         ENDIF
      END_3D

      !   Update the arrays TRA which contain the biological sources and sinks
      DO_3D( 0, 0, 0, 0, 1, nksr )
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zpronewn = zpronmaxn(ji,jj,jk) * xnanono3(ji,jj,jk)
            zpronewp = zpronmaxp(ji,jj,jk) * xpicono3(ji,jj,jk) 
            zpronewd = zpronmaxd(ji,jj,jk) * xdiatno3(ji,jj,jk)
            !
            zproregn = zpronmaxn(ji,jj,jk) * xnanonh4(ji,jj,jk)
            zproregp = zpronmaxp(ji,jj,jk) * xpiconh4(ji,jj,jk)
            zproregd = zpronmaxd(ji,jj,jk) * xdiatnh4(ji,jj,jk)
            !
            zpropo4n = zpropmaxn(ji,jj,jk) * xnanopo4(ji,jj,jk)
            zpropo4p = zpropmaxp(ji,jj,jk) * xpicopo4(ji,jj,jk)
            zpropo4d = zpropmaxd(ji,jj,jk) * xdiatpo4(ji,jj,jk)
            !
            zprodopn = zpropmaxn(ji,jj,jk) * xnanodop(ji,jj,jk)
            zprodopp = zpropmaxp(ji,jj,jk) * xpicodop(ji,jj,jk)
            zprodopd = zpropmaxd(ji,jj,jk) * xdiatdop(ji,jj,jk)
            !
            zpo4tot  = zpropo4n + zpropo4d + zpropo4p
            zpnewtot = zpronewn + zpronewd + zpronewp
            zpregtot = zproregn + zproregd + zproregp
            !
            zpptot   = zprorcap(ji,jj,jk) + zprorcan(ji,jj,jk) + zprorcad(ji,jj,jk)

            zprontot = zpronewn + zproregn
            zproptot = zpronewp + zproregp
            zprodtot = zpronewd + zproregd
            !
            zproddoc = excretd * zprorcad(ji,jj,jk)                     &
              &        + excretn * zprorcan(ji,jj,jk)                   &
              &        + excretp * zprorcap(ji,jj,jk)
            !
            zproddop = excretd * zpropo4d - texcretd * zprodopd         &
              &        + excretn * zpropo4n - texcretn * zprodopn       &
              &        + excretp * zpropo4p - texcretp * zprodopp

            zproddon = excretd * zprodtot + excretn * zprontot + excretp * zproptot

            zprodfer = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
            !
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - zpo4tot
            tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) - zpnewtot
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) - zpregtot  
            !
            tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs)           &
              &                       + zprorcan(ji,jj,jk) * texcretn   &
              &                       - xpsino3 * zpronewn              &
              &                       - xpsinh4 * zproregn   
            tr(ji,jj,jk,jpnph,Krhs) = tr(ji,jj,jk,jpnph,Krhs) + zprontot * texcretn
            tr(ji,jj,jk,jppph,Krhs) = tr(ji,jj,jk,jppph,Krhs) + ( zpropo4n + zprodopn ) * texcretn
            tr(ji,jj,jk,jpnfe,Krhs) = tr(ji,jj,jk,jpnfe,Krhs) + zprofen(ji,jj,jk) * texcretn
            !
            tr(ji,jj,jk,jppic,Krhs) = tr(ji,jj,jk,jppic,Krhs)           &
              &                       + zprorcap(ji,jj,jk) * texcretp   &
              &                       - xpsino3 * zpronewp              &
              &                       - xpsinh4 * zproregp   
            tr(ji,jj,jk,jpnpi,Krhs) = tr(ji,jj,jk,jpnpi,Krhs) + zproptot * texcretp
            tr(ji,jj,jk,jpppi,Krhs) = tr(ji,jj,jk,jpppi,Krhs) + ( zpropo4p + zprodopp ) * texcretp
            tr(ji,jj,jk,jppfe,Krhs) = tr(ji,jj,jk,jppfe,Krhs) + zprofep(ji,jj,jk) * texcretp
            !
            tr(ji,jj,jk,jpdia,Krhs) = tr(ji,jj,jk,jpdia,Krhs)           &
              &                       + zprorcad(ji,jj,jk) * texcretd   &
              &                       - xpsino3 * zpronewd              &
              &                       - xpsinh4 * zproregd    
            !
            zprodsil = zprmaxd(ji,jj,jk) * zysopt(ji,jj,jk) * rfact2 * tr(ji,jj,jk,jpdia,Kbb) 
            !
            tr(ji,jj,jk,jpndi,Krhs) = tr(ji,jj,jk,jpndi,Krhs) + zprodtot * texcretd
            tr(ji,jj,jk,jppdi,Krhs) = tr(ji,jj,jk,jppdi,Krhs) + ( zpropo4d + zprodopd ) * texcretd
            tr(ji,jj,jk,jpdfe,Krhs) = tr(ji,jj,jk,jpdfe,Krhs) + zprofed(ji,jj,jk) * texcretd
            tr(ji,jj,jk,jpdsi,Krhs) = tr(ji,jj,jk,jpdsi,Krhs) + zprodsil
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zproddoc
            tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zproddon                                        
            tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zproddop
  
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs)           &
              &                       + o2ut * zpregtot + ( o2ut + o2nit ) * zpnewtot

            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - zprodfer
            consfe3(ji,jj,jk)       = zprodfer * 75.0 / ( rtrn + ( plig(ji,jj,jk) + 75.0 * (1.0 - plig(ji,jj,jk) ) )   &
              &                       * tr(ji,jj,jk,jpfer,Kbb) ) / rfact2
            tr(ji,jj,jk,jpsil,Krhs) = tr(ji,jj,jk,jpsil,Krhs) - zprodsil

            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zpptot  &
              &                       + xpsino3 * zpnewtot + xpsinh4 * zpregtot

            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * ( zpnewtot - zpregtot )
            !
         ENDIF
      END_3D
     
     ! Production and uptake of ligands by phytoplankton. This part is activated 
     ! when ln_ligand is set to .true. in the namelist. Ligand uptake is small 
     ! and based on the FeL model by Morel et al. (2008) and on the study of
     ! Shaked and Lis (2012)
     ! -------------------------------------------------------------------------
     IF( ln_ligand ) THEN
         DO_3D( 0, 0, 0, 0, 1, nksr )
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
              zproddoc = excretd * zprorcad(ji,jj,jk) + excretn * zprorcan(ji,jj,jk) + excretp * zprorcap(ji,jj,jk)
              zprodfer = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
              zprodlig = plig(ji,jj,jk) / ( rtrn + plig(ji,jj,jk) + 75.0 * (1.0 - plig(ji,jj,jk) ) ) * lthet 
              !
              tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) + zproddoc * ldocp - zprodfer * zprodlig
            ENDIF
        END_3D
     ENDIF

    ! Output of the diagnostics
    ! Total primary production per year
    IF( l_dia_pp )  &
        & tpp = glob_3Dsum( 'p5zprod',  ( zprorcan(:,:,:) + zprorcad(:,:,:) + zprorcap(:,:,:)     &
              &            - zpronmaxn(:,:,:) * ( xpsino3 * xnanono3(:,:,:) + xpsinh4 * xnanonh4(:,:,:) )   &
              &            - zpronmaxd(:,:,:) * ( xpsino3 * xdiatno3(:,:,:) + xpsinh4 * xdiatnh4(:,:,:) )   &
              &            - zpronmaxp(:,:,:) * ( xpsino3 * xpicono3(:,:,:) + xpsinh4 * xpiconh4(:,:,:) ) ) &
              &            * cvol(:,:,:), cdelay = 'tpp' ) 

    IF( knt == nrdttrc ) THEN
       !
       IF( l_dia_pp ) THEN  ! Net primary production
          CALL iom_put( "PPPHYN", ( zprorcan(:,:,:) - xpsinh4 * zpronmaxn(:,:,:) * xnanonh4(:,:,:)   &  ! nano
              &                  - xpsino3 * zpronmaxn(:,:,:) * xnanono3(:,:,:) ) * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          CALL iom_put( "PPPHYD", ( zprorcad(:,:,:) - xpsinh4 * zpronmaxd(:,:,:) * xdiatnh4(:,:,:)   &  ! diatomes
              &                  - xpsino3 * zpronmaxd(:,:,:) * xdiatno3(:,:,:) ) * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          CALL iom_put( "PPPHYP", ( zprorcap(:,:,:) - xpsinh4 * zpronmaxp(:,:,:) * xpiconh4(:,:,:)   &   ! pico
              &                  - xpsino3 * zpronmaxp(:,:,:) * xpicono3(:,:,:) ) * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          CALL iom_put( "TPP", ( zprorcan(:,:,:) + zprorcad(:,:,:) + zprorcap(:,:,:)                           &
              &               - zpronmaxn(:,:,:) * ( xpsino3 * xnanono3(:,:,:) + xpsinh4 * xnanonh4(:,:,:) )   &
              &               - zpronmaxd(:,:,:) * ( xpsino3 * xdiatno3(:,:,:) + xpsinh4 * xdiatnh4(:,:,:) )   &
              &               - zpronmaxp(:,:,:) * ( xpsino3 * xpicono3(:,:,:) + xpsinh4 * xpiconh4(:,:,:) ) ) &
              &                 * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          CALL iom_put( "PPNEWo2", ( zprorcan(:,:,:) + zprorcad(:,:,:) + zprorcap(:,:,:)                           &
              &               - zpronmaxn(:,:,:) * ( xpsino3 * xnanono3(:,:,:) + xpsinh4 * xnanonh4(:,:,:) )   &
              &               - zpronmaxd(:,:,:) * ( xpsino3 * xdiatno3(:,:,:) + xpsinh4 * xdiatnh4(:,:,:) )   &
              &               - zpronmaxp(:,:,:) * ( xpsino3 * xpicono3(:,:,:) + xpsinh4 * xpiconh4(:,:,:) ) ) &
              &                 * ( o2ut + o2nit )* 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! Oxygen production by the New Produc

          CALL iom_put( "tintpp"  , tpp * 1.e+3 * rfact2r )  !  global total integrated primary production molC/s
          CALL iom_put( "GPPHYN", zprorcan(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )  ! nano
          CALL iom_put( "GPPHYD", zprorcad(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )  ! diatomes
          CALL iom_put( "GPPHYP", zprorcap(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )  ! pico
          CALL iom_put( "PPNEWN", zpronmaxn(:,:,:) * xnanono3(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          CALL iom_put( "PPNEWD", zpronmaxd(:,:,:) * xdiatno3(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          CALL iom_put( "PPNEWP", zpronmaxp(:,:,:) * xpicono3(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          CALL iom_put( "TPNEW", ( zpronmaxn(:,:,:) * xnanono3(:,:,:) +  &
                &                  zpronmaxd(:,:,:) * xdiatno3(:,:,:) +  &
                &                  zpronmaxp(:,:,:) * xpicono3(:,:,:) ) &
                &                  * 1.e+3 * rfact2r * tmask(A2D(0),:) )  ! total
          CALL iom_put( "PBSi", zprmaxd(:,:,:) * zysopt(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )
          ! biogenic iron production by nanophyto
          CALL iom_put( "PFeN", zprofen(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! nano
          CALL iom_put( "PFeD", zprofed(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! diatomes
          CALL iom_put( "PFeP", zprofep(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! pico
          CALL iom_put( "TPBFE", ( zprofen(:,:,:) + zprofed(:,:,:) + zprofep(:,:,:) ) &
             &                  * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! total biogenic production
          !  Regenerated production
          CALL iom_put( "PPRego2", ( zpronmaxn(:,:,:) * xnanonh4(:,:,:) +  &
                &                  zpronmaxd(:,:,:) * xdiatnh4(:,:,:) +  &
                &                  zpronmaxp(:,:,:) * xpiconh4(:,:,:) ) &
                &                  * o2ut * 1.e+3 * rfact2r * tmask(A2D(0),:) ) 
       ENDIF
       !
       IF( l_dia_mu ) THEN   ! Realized growth rate
          CALL iom_put( "Mumax", zprmaxn(:,:,:)  * tmask(A2D(0),:) )
          CALL iom_put( "MuN", zprbio(:,:,:) * xlimphy(:,:,:) * tmask(A2D(0),:) )   ! nano
          CALL iom_put( "MuD", zprdia(:,:,:) * xlimdia(:,:,:) * tmask(A2D(0),:) )  ! diatomes
          CALL iom_put( "MuP", zprpic(:,:,:) * xlimpic(:,:,:) * tmask(A2D(0),:) )  !  pico
       ENDIF
       !
       IF( l_dia_light ) THEN ! light limitation term
          CALL iom_put( "LNlight", zprbio(:,:,:) / (zprmaxn(:,:,:)+rtrn) * tmask(A2D(0),:) )  ! nano
          CALL iom_put( "LDlight", zprdia(:,:,:) / (zprmaxd(:,:,:)+rtrn) * tmask(A2D(0),:) )  ! diatomes
          CALL iom_put( "LPlight", zprpic(:,:,:) / (zprmaxp(:,:,:)+rtrn) * tmask(A2D(0),:) ) ! pico
       ENDIF
       !
       IF( l_dia_lprod ) THEN
          CALL iom_put( "LPRODP", ( excretd * zprorcad(:,:,:) + excretn * zprorcan(:,:,:) +  &
             &                      excretp * zprorcap(:,:,:) ) * 1.e+3 * rfact2r * tmask(A2D(0),:) * ldocp * 1e9 )
           !
          CALL iom_put( "LDETP" , ( texcretn * zprofen(:,:,:) + texcretd * zprofed(:,:,:) +  &
            &                      texcretp * zprofep(:,:,:) ) * plig(:,:,:) &
            &                     / ( rtrn + plig(:,:,:) + 75.0 * (1.0 - plig(:,:,:) ) )  &
            &                     * 1.e+3 * rfact2r * tmask(A2D(0),:) * lthet * 1e9 )
       ENDIF
       !
     ENDIF
     !
     IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
        WRITE(charout, FMT="('prod')")
        CALL prt_ctl_info( charout, cdcomp = 'top' )
        CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
     ENDIF
     !
     IF( ln_timing )   CALL timing_stop('p5z_prod')
     !
   END SUBROUTINE p5z_prod


   SUBROUTINE p5z_prod_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_prod_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton production parameters
      !!
      !! ** Method  :   Read the namp5zprod namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp5zprod
      !!----------------------------------------------------------------------
      INTEGER :: ios    ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zprod/ pislopen, pislopep, pisloped, excretn, excretp, excretd,     &
         &                 chlcmin, grosip, bresp
      !!----------------------------------------------------------------------

      READ_NML_REF(numnatp,namp5zprod)
      READ_NML_CFG(numnatp,namp5zprod)
      IF(lwm) WRITE ( numonp, namp5zprod )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for phytoplankton growth, namp5zprod'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean Si/C ratio                           grosip       =', grosip
         WRITE(numout,*) '    P-I slope                                 pislopen     =', pislopen
         WRITE(numout,*) '    P-I slope  for diatoms                    pisloped     =', pisloped
         WRITE(numout,*) '    P-I slope  for picophytoplankton          pislopep     =', pislopep
         WRITE(numout,*) '    excretion ratio of nanophytoplankton      excretn      =', excretn
         WRITE(numout,*) '    excretion ratio of picophytoplankton      excretp      =', excretp
         WRITE(numout,*) '    excretion ratio of diatoms                excretd      =', excretd
         WRITE(numout,*) '    basal respiration in phytoplankton        bresp        =', bresp
         WRITE(numout,*) '    Maximum Chl/C in phytoplankton            chlcmin      =', chlcmin
      ENDIF
      !
      r1_rday   = 1._wp / rday 
      texcretn  = 1._wp - excretn
      texcretp  = 1._wp - excretp
      texcretd  = 1._wp - excretd
      tpp       = 0._wp
      !
      xq10_n = 1. + xpsinh4 * qnnmax
      xq10_d = 1. + xpsinh4 * qndmax
      xq10_p = 1. + xpsinh4 * qnpmax
      !
   END SUBROUTINE p5z_prod_init

   !!======================================================================
END MODULE p5zprod
