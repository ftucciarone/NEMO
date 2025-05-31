MODULE p4zmeso
   !!======================================================================
   !!                         ***  MODULE p4zmeso  ***
   !! TOP :   PISCES Compute the sources/sinks for mesozooplankton
   !!======================================================================
   !! History :   1.0  !  2002     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!----------------------------------------------------------------------
   !!   p4z_meso        : Compute the sources/sinks for mesozooplankton
   !!   p4z_meso_init   : Initialization of the parameters for mesozooplankton
   !!   p4z_meso_alloc  : Allocate variables for mesozooplankton 
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p4zprod         ! production
   USE p2zlim
   USE p4zlim
   USE prtctl          ! print control for debugging
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_meso              ! called in p4zbio.F90
   PUBLIC   p4z_meso_init         ! called in trcsms_pisces.F90
   PUBLIC   p4z_meso_alloc        ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  part2        !: part of calcite not dissolved in mesozoo guts
   REAL(wp), PUBLIC ::  xpref2d      !: mesozoo preference for diatoms
   REAL(wp), PUBLIC ::  xpref2n      !: mesozoo preference for nanophyto
   REAL(wp), PUBLIC ::  xpref2z      !: mesozoo preference for microzooplankton
   REAL(wp), PUBLIC ::  xpref2c      !: mesozoo preference for POC 
   REAL(wp), PUBLIC ::  xpref2m      !: mesozoo preference for mesozoo
   REAL(wp), PUBLIC ::  xthresh2zoo  !: zoo feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2dia  !: diatoms feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2phy  !: nanophyto feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2poc  !: poc feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2mes  !: mesozoo feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  xthresh2     !: feeding threshold for mesozooplankton 
   REAL(wp), PUBLIC ::  resrat2      !: exsudation rate of mesozooplankton
   REAL(wp), PUBLIC ::  mzrat2       !: microzooplankton mortality rate 
   REAL(wp), PUBLIC ::  grazrat2     !: maximal mesozoo grazing rate
   REAL(wp), PUBLIC ::  xkgraz2      !: non assimilated fraction of P by mesozoo 
   REAL(wp), PUBLIC ::  unass2       !: Efficicency of mesozoo growth 
   REAL(wp), PUBLIC ::  sigma2       !: Fraction of mesozoo excretion as DOM 
   REAL(wp), PUBLIC ::  epsher2      !: growth efficiency
   REAL(wp), PUBLIC ::  epsher2min   !: minimum growth efficiency at high food for grazing 2
   REAL(wp), PUBLIC ::  xsigma2      !: Width of the predation window
   REAL(wp), PUBLIC ::  xsigma2del   !: Maximum width of the predation window at low food density
   REAL(wp), PUBLIC ::  grazflux     !: mesozoo flux feeding rate
   REAL(wp), PUBLIC ::  xfracmig     !: Fractional biomass of meso that performs DVM
   LOGICAL , PUBLIC ::  ln_dvm_meso  !: Boolean to activate DVM of mesozooplankton
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) :: depmig  !: DVM of mesozooplankton : migration depth
   INTEGER , ALLOCATABLE, SAVE, DIMENSION(:,:) :: kmig    !: Vertical indice of the the migration depth

   REAL(wp)         ::  xfracmigm1     !: Fractional biomass of meso that performs DVM
   REAL(wp)         ::  rlogfactdn     !: Size ratio between diatoms and nanophytoplankton
   LOGICAL          :: l_dia_graz, l_dia_lprodz
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_meso( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_meso  ***
      !!
      !! ** Purpose :   Compute the sources/sinks for mesozooplankton
      !!                This includes ingestion and assimilation, flux feeding
      !!                and mortality. We use a passive prey switching  
      !!                parameterization.
      !!                All living compartments smaller than mesozooplankton
      !!                are potential preys of mesozooplankton as well as small
      !!                sinking particles 
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   ! ocean time step and ???
      INTEGER, INTENT(in)  ::  Kbb, kmm, Krhs ! time level indices
      !
      INTEGER  :: ji, jj, jk, jkt
      REAL(wp) :: zcompadi, zcompaph, zcompapoc, zcompaz, zcompam, zcompames
      REAL(wp) :: zgraze2, zdenom, zdenom2, zfact, zfood, zfoodlim, zproport, zproportm1, zbeta
      REAL(wp) :: zmortzgoc, zfrac, zfracfe, zratio, zratio2, zfracal, zgrazcal
      REAL(wp) :: zepsherf, zepshert, zepsherq, zepsherv, zgraztotc, zgraztotn, zgraztotf
      REAL(wp) :: zmigreltime, zprcaca, zmortz, zgrasratf, zgrasratn
      REAL(wp) :: zrespz, ztortz, zgrazdc, zgrazz, zgrazpof, zgraznc, zgrazpoc, zgraznf, zgrazdf
      REAL(wp) :: zgrazm, zgrazfffp, zgrazfffg, zgrazffep, zgrazffeg, zdep
      REAL(wp) :: zsigma, zsigma2, zsizedn, zdiffdn, ztmp1, ztmp2, ztmp3, ztmp4, ztmp5, ztmptot, zmigthick 
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(A2D(0),jpk) :: zgrarem, zgraref, zgrapoc, zgrapof, zgrabsi
      REAL(wp), DIMENSION(A2D(0),jpk) :: zproportd, zproportn
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zgramigrem, zgramigref, zgramigpoc, zgramigpof
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zgramigbsi
      REAL(wp), DIMENSION(:,:,:)  , ALLOCATABLE :: zgrazing2, zzligprod
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_meso')
      !
      IF( kt == nittrc000 )  THEN
         l_dia_graz    = iom_use( "GRAZ2" ) .OR. iom_use( "FEZOO2" ) .OR. iom_use( "MesoZo2" )
         l_dia_lprodz  = ln_ligand .AND. iom_use( "LPRODZ2" )
      ENDIF

      IF( l_dia_graz ) THEN
         ALLOCATE( zgrazing2(A2D(0),jpk) )     ;    zgrazing2(A2D(0),:) = 0.
      ENDIF
      !
      IF( l_dia_lprodz ) THEN
         ALLOCATE( zzligprod(A2D(0),jpk) )
         zzligprod(A2D(0),:) = tr(A2D(0),:,jplgw,Krhs)
      ENDIF
      !
      zgrapoc(:,:,:) = 0._wp    ;  zgrarem(:,:,:) = 0._wp
      zgraref (:,:,:) = 0._wp   ;  zgrapof(:,:,:) = 0._wp
      zgrabsi (:,:,:) = 0._wp
      !
      !
      ! Diurnal vertical migration of mesozooplankton
      ! Computation of the migration depth
      ! ---------------------------------------------
      IF (ln_dvm_meso) CALL p4z_meso_depmig( Kbb, Kmm )
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF ( tmask(ji,jj,jk) == 1 ) THEN
            ztmp1 = 0.09544 - 0.0628 * EXP(-0.078 * 6.0 * xsizerd)
            zproportd(ji,jj,jk) = (0.09544 - 0.0628 * EXP(-0.078 * sized(ji,jj,jk) * 6.0) ) / ztmp1
            ztmp1 = -0.006622 + 0.008891 * xsizern * 1.67
            zproportn(ji,jj,jk) = (-0.006622 + 0.008891 * sizen(ji,jj,jk) * 1.67) / ztmp1
         ELSE
            zproportd(ji,jj,jk) = 1.0
            zproportn(ji,jj,jk) = 1.0
         ENDIF
      END_3D
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zcompam   = MAX( ( tr(ji,jj,jk,jpmes,Kbb) - 1.e-9 ), 0.e0 )
         zfact     = xstep * tgfunc2(ji,jj,jk) * zcompam

         
         !  linear mortality of mesozooplankton
         !  A michaelis menten modulation term is used to avoid extinction of 
         !  mesozooplankton at very low food concentration. Mortality is

         !  enhanced in low O2 waters
         !  -----------------------------------------------------------------
         zrespz    = resrat2 * zfact * ( tr(ji,jj,jk,jpmes,Kbb) / ( xkmort + tr(ji,jj,jk,jpmes,Kbb) )  &
         &           + 3. * nitrfac(ji,jj,jk) )

         ! Zooplankton quadratic mortality. A square function has been selected with
         !  to mimic predation and disease (density dependent mortality). It also tends
         !  to stabilise the model
         !  -------------------------------------------------------------------------
         ztortz    = mzrat2 * 1.e6 * zfact * tr(ji,jj,jk,jpmes,Kbb)  * (1. - nitrfac(ji,jj,jk) )
         !
         !   Computation of the abundance of the preys
         !   A threshold can be specified in the namelist
         !   --------------------------------------------
         zcompadi  = zproportd(ji,jj,jk) * MAX( ( tr(ji,jj,jk,jpdia,Kbb) - xthresh2dia ), 0.e0 )
         zcompaz   = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - xthresh2zoo ), 0.e0 )
         zcompapoc = MAX( ( tr(ji,jj,jk,jppoc,Kbb) - xthresh2poc ), 0.e0 )
         zcompames = MAX( ( tr(ji,jj,jk,jpmes,Kbb) - xthresh2mes ), 0.e0 )
         zcompaph  = zproportn(ji,jj,jk) * MAX( ( tr(ji,jj,jk,jpphy,Kbb) - xthresh2phy ), 0.e0 )

         ! Mesozooplankton grazing
         ! The total amount of food is the sum of all preys accessible to mesozooplankton 
         ! multiplied by their food preference
         ! A threshold can be specified in the namelist (xthresh2). However, when food 
         ! concentration is close to this threshold, it is decreased to avoid the 
         ! accumulation of food in the mesozoopelagic domain
         ! -------------------------------------------------------------------------------
         zfood     = xpref2d * zcompadi + xpref2z * zcompaz + xpref2n * zcompaph + xpref2c * zcompapoc    &
           &         + xpref2m * zcompames 
         zfoodlim  = MAX( 0., zfood - MIN( 0.5 * zfood, xthresh2 ) )
         zdenom    = zfoodlim / ( xkgraz2 + zfoodlim )
         zgraze2   = grazrat2 * xstep * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jpmes,Kbb) * (1. - nitrfac(ji,jj,jk)) 

         ! An active switching parameterization is used here.
         ! We don't use the KTW parameterization proposed by 
         ! Vallina et al. because it tends to produce too steady biomass
         ! composition and the variance of Chl is too low as it grazes
         ! too strongly on winning organisms. We use a generalized
         ! switching parameterization proposed by Morozov and 
         ! Petrovskii (2013)
         ! ------------------------------------------------------------  
         ! The width of the selection window is increased when preys
         ! have low abundance, .i.e. zooplankton become less specific 
         ! to avoid starvation.
         ! ----------------------------------------------------------
         zdenom2 = zdenom * zdenom
         zsigma  = 1.0 - zdenom2/(0.05*0.05+zdenom2)
         zsigma  = xsigma2 + xsigma2del * zsigma
         zsigma2 = 2.0 * zsigma * zsigma
         ! Nanophytoplankton and diatoms are the only preys considered
         ! to be close enough to have potential interference
         ! -----------------------------------------------------------
         zsizedn = rlogfactdn + ( logsizen(ji,jj,jk) - logsized(ji,jj,jk) )
         zdiffdn = EXP( - zsizedn * zsizedn / zsigma2 )
         ztmp1 = xpref2n * zcompaph * ( zcompaph + zdiffdn * zcompadi )
         ztmp2 = xpref2m * zcompames*zcompames
         ztmp3 = xpref2c * zcompapoc*zcompapoc
         ztmp4 = xpref2d * zcompadi * ( zcompadi + zdiffdn * zcompaph )
         ztmp5 = xpref2z * zcompaz*zcompaz
         ztmptot = ztmp1 + ztmp2 + ztmp3 + ztmp4 + ztmp5 + rtrn
         ztmp1 = ztmp1 / ztmptot
         ztmp2 = ztmp2 / ztmptot
         ztmp3 = ztmp3 / ztmptot
         ztmp4 = ztmp4 / ztmptot
         ztmp5 = ztmp5 / ztmptot

         !   Mesozooplankton regular grazing on the different preys
         !   ------------------------------------------------------
         zgrazdc   = zgraze2  * ztmp4 * zdenom  ! diatoms
         zgraznc   = zgraze2  * ztmp1 * zdenom  ! nanophytoplankton
         zgrazpoc  = zgraze2  * ztmp3 * zdenom  ! small POC
         zgrazz    = zgraze2  * ztmp5 * zdenom  ! microzooplankton
         zgrazm    = zgraze2  * ztmp2 * zdenom

         ! Ingestion rates of the Fe content of the different preys
         zgraznf   = zgraznc  * tr(ji,jj,jk,jpnfe,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn)
         zgrazdf   = zgrazdc  * tr(ji,jj,jk,jpdfe,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         zgrazpof  = zgrazpoc * tr(ji,jj,jk,jpsfe,Kbb) / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn)

         !  Mesozooplankton flux feeding on GOC and POC. The feeding pressure
         ! is proportional to the flux
         !  ------------------------------------------------------------------
         zgrazffeg = grazflux  * xstep * wsbio4(ji,jj,jk)      &
         &           * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jpgoc,Kbb) * tr(ji,jj,jk,jpmes,Kbb) &
         &           * (1. - nitrfac(ji,jj,jk))
         zgrazfffg = zgrazffeg * tr(ji,jj,jk,jpbfe,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)
         zgrazffep = grazflux  * xstep *  wsbio3(ji,jj,jk)     &
         &           * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jppoc,Kbb) * tr(ji,jj,jk,jpmes,Kbb) &
         &           * (1. - nitrfac(ji,jj,jk))
         zgrazfffp = zgrazffep * tr(ji,jj,jk,jpsfe,Kbb) / (tr(ji,jj,jk,jppoc,Kbb) + rtrn)
         !
         zgraztotc = zgrazdc + zgrazz + zgraznc + zgrazm + zgrazpoc + zgrazffep + zgrazffeg

         ! Compute the proportion of filter feeders. It is assumed steady state.
         ! ---------------------------------------------------------------------
         zproport  = (zgrazffep + zgrazffeg)/(rtrn + zgraztotc)
         zproport = zproport * zproport

         ! Compute fractionation of aggregates. It is assumed that 
         ! diatoms based aggregates are more prone to fractionation
         ! since they are more porous (marine snow instead of fecal pellets)
         ! -----------------------------------------------------------------

         ! Compute fractionation of aggregates. It is assumed that 
         ! diatoms based aggregates are more prone to fractionation
         ! since they are more porous (marine snow instead of fecal pellets)
         zratio    = tr(ji,jj,jk,jpgsi,Kbb) / ( tr(ji,jj,jk,jpgoc,Kbb) + rtrn )
         zratio2   = zratio * zratio
         zfrac     = zproport * grazflux  * xstep * wsbio4(ji,jj,jk)      &
         &          * tr(ji,jj,jk,jpgoc,Kbb) * tr(ji,jj,jk,jpmes,Kbb)          &
         &          * ( 0.4 + 3.6 * zratio2 / ( 1.**2 + zratio2 ) )
         zfracfe   = zfrac * tr(ji,jj,jk,jpbfe,Kbb) / (tr(ji,jj,jk,jpgoc,Kbb) + rtrn)

         ! Flux feeding is multiplied by the fractional biomass of flux feeders
         zgrazffep = zproport * zgrazffep
         zgrazffeg = zproport * zgrazffeg
         zgrazfffp = zproport * zgrazfffp
         zgrazfffg = zproport * zgrazfffg
         zproportm1 = 1.0 - zproport
         zgrazdc   = zproportm1 * zgrazdc
         zgraznc   = zproportm1 * zgraznc
         zgrazz    = zproportm1 * zgrazz
         zgrazpoc  = zproportm1 * zgrazpoc
         zgrazm    = zproportm1 * zgrazm
         zgrazdf   = zproportm1 * zgrazdf
         zgraznf   = zproportm1 * zgraznf
         zgrazpof  = zproportm1 * zgrazpof

         ! Total ingestion rates in C, N, Fe
         zgraztotc = zgrazdc + zgrazz + zgraznc + zgrazpoc + zgrazm + zgrazffep + zgrazffeg  ! grazing by mesozooplankton
         IF( l_dia_graz ) zgrazing2(ji,jj,jk) = zgraztotc

         zgraztotn = zgrazdc * quotad(ji,jj,jk) + zgrazz + zgraznc * quotan(ji,jj,jk)   &
         &   + zgrazm + zgrazpoc + zgrazffep + zgrazffeg
         zgraztotf = zgrazdf + zgraznf + zgrazz * feratz + zgrazpof + zgrazfffp + zgrazfffg + zgrazm * feratm

         !   Stoichiometruc ratios of the food ingested by zooplanton 
         !   --------------------------------------------------------
         zgrasratf =  ( zgraztotf + rtrn )/ ( zgraztotc + rtrn )
         zgrasratn =  ( zgraztotn + rtrn )/ ( zgraztotc + rtrn )

         ! Mesozooplankton efficiency. 
         ! We adopt a formulation proposed by Mitra et al. (2007)
         ! The gross growth efficiency is controled by the most limiting nutrient.
         ! Growth is also further decreased when the food quality is poor. This is currently
         ! hard coded : it can be decreased by up to 50% (zepsherq)
         ! GGE can also be decreased when food quantity is high, zepsherf (Montagnes and 
         ! Fulton, 2012)
         ! -----------------------------------------------------------------------------------
         zepshert  = MIN( 1., zgrasratn, zgrasratf / feratm)
         zbeta     = MAX(0., (epsher2 - epsher2min) )
         ! Food quantity deprivation of GGE
         zepsherf  = epsher2min + zbeta / ( 1.0 + 0.04E6 * 12. * zfood * zbeta )
         ! Food quality deprivation of GGE
         zepsherq  = 0.5 + (1.0 - 0.5) * zepshert * ( 1.0 + 1.0 ) / ( zepshert + 1.0 )
         ! Actual GGE
         zepsherv  = zepsherf * zepshert * zepsherq
         ! 
         ! Impact of grazing on the prognostic variables
         ! ---------------------------------------------
         zmortz = ztortz + zrespz
         ! Mortality induced by the upper trophic levels, ztortz, is allocated 
         ! according to a infinite chain of predators (ANderson et al., 2013)
         zmortzgoc = unass2 / ( 1. - epsher2 ) * ztortz + zrespz

         tr(ji,jj,jk,jpmes,Krhs) = tr(ji,jj,jk,jpmes,Krhs) - zmortz + zepsherv * zgraztotc - zgrazm 
         tr(ji,jj,jk,jpdia,Krhs) = tr(ji,jj,jk,jpdia,Krhs) - zgrazdc
         tr(ji,jj,jk,jpzoo,Krhs) = tr(ji,jj,jk,jpzoo,Krhs) - zgrazz
         tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs) - zgraznc
         tr(ji,jj,jk,jpnch,Krhs) = tr(ji,jj,jk,jpnch,Krhs) - zgraznc * tr(ji,jj,jk,jpnch,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
         tr(ji,jj,jk,jpdch,Krhs) = tr(ji,jj,jk,jpdch,Krhs) - zgrazdc * tr(ji,jj,jk,jpdch,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         tr(ji,jj,jk,jpdsi,Krhs) = tr(ji,jj,jk,jpdsi,Krhs) - zgrazdc * tr(ji,jj,jk,jpdsi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         zgrabsi(ji,jj,jk)       = zgrazdc * tr(ji,jj,jk,jpdsi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         !
         tr(ji,jj,jk,jpnfe,Krhs) = tr(ji,jj,jk,jpnfe,Krhs) - zgraznf
         tr(ji,jj,jk,jpdfe,Krhs) = tr(ji,jj,jk,jpdfe,Krhs) - zgrazdf
         !
         tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) - zgrazpoc - zgrazffep + zfrac
         prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + zfrac
         conspoc(ji,jj,jk) = conspoc(ji,jj,jk) - zgrazpoc - zgrazffep
         !
         tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) - zgrazffeg - zfrac
         consgoc(ji,jj,jk) = consgoc(ji,jj,jk) - zgrazffeg - zfrac
         !
         tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) - zgrazpof - zgrazfffp + zfracfe
         tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) - zgrazfffg - zfracfe

         ! Calcite remineralization due to zooplankton activity
         ! part2 of the ingested calcite is not dissolving in the 
         ! acidic gut
         ! ------------------------------------------------------
         zfracal = tr(ji,jj,jk,jpcal,Kbb) / ( tr(ji,jj,jk,jpgoc,Kbb) + rtrn )
         zgrazcal = zgrazffeg * (1. - part2) * zfracal
         ! calcite production by zooplankton activity
         zprcaca = xfracal(ji,jj,jk) * zgraznc
         prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
         !
         zprcaca = part2 * zprcaca
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zgrazcal - zprcaca
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + 2. * ( zgrazcal - zprcaca )
         tr(ji,jj,jk,jpcal,Krhs) = tr(ji,jj,jk,jpcal,Krhs) - zgrazcal + zprcaca

         ! Computation of total excretion and egestion by mesozoo. 
         ! ---------------------------------------------------------
         zgrarem(ji,jj,jk) = zgraztotc * ( 1. - zepsherv - unass2 ) &
                 &         + ( 1. - epsher2 - unass2 ) / ( 1. - epsher2 ) * ztortz
         zgraref(ji,jj,jk) = zgraztotc * MAX( 0. , ( 1. - unass2 ) * zgrasratf - feratm * zepsherv )    &
                 &         + feratm * ( ( 1. - epsher2 - unass2 ) /( 1. - epsher2 ) * ztortz )
         zgrapoc(ji,jj,jk) = zgraztotc * unass2 + zmortzgoc
         zgrapof(ji,jj,jk) = zgraztotf * unass2 + feratm * zmortzgoc
         !
      END_3D

      ! Computation of the effect of DVM by mesozooplankton
      ! This part is only activated if ln_dvm_meso is set to true
      ! The parameterization has been published in Gorgues et al. (2019).
      ! -----------------------------------------------------------------
      IF (ln_dvm_meso) THEN
         ALLOCATE( zgramigrem(jpi,jpj), zgramigref(jpi,jpj), zgramigpoc(jpi,jpj), zgramigpof(jpi,jpj) )
         ALLOCATE( zgramigbsi(jpi,jpj) )
         zgramigrem(:,:) = 0.0    ;   zgramigref(:,:) = 0.0
         zgramigpoc(:,:) = 0.0    ;   zgramigpof(:,:) = 0.0
         zgramigbsi(:,:) = 0.0

        ! Compute the amount of materials that will go into vertical migration
        ! This fraction is sumed over the euphotic zone and is removed from 
        ! the fluxes driven by mesozooplankton in the euphotic zone.
        ! --------------------------------------------------------------------
        DO_3D( 0, 0, 0, 0, 1, jpk)
            zmigreltime = (1. - strn(ji,jj) / 24.)
            zmigthick   = (1. - zmigreltime ) * e3t(ji,jj,jk,Kmm) * tmask(ji,jj,jk)
            IF ( gdept(ji,jj,jk,Kmm) <= heup(ji,jj) ) THEN
               zgramigrem(ji,jj) = zgramigrem(ji,jj) + xfracmig * zgrarem(ji,jj,jk) * zmigthick 
               zgramigref(ji,jj) = zgramigref(ji,jj) + xfracmig * zgraref(ji,jj,jk) * zmigthick 
               zgramigpoc(ji,jj) = zgramigpoc(ji,jj) + xfracmig * zgrapoc(ji,jj,jk) * zmigthick 
               zgramigpof(ji,jj) = zgramigpof(ji,jj) + xfracmig * zgrapof(ji,jj,jk) * zmigthick 
               zgramigbsi(ji,jj) = zgramigbsi(ji,jj) + xfracmig * zgrabsi(ji,jj,jk) * zmigthick 

               zgrarem(ji,jj,jk) = zgrarem(ji,jj,jk) * ( xfracmigm1 + xfracmig * zmigreltime )
               zgraref(ji,jj,jk) = zgraref(ji,jj,jk) * ( xfracmigm1 + xfracmig * zmigreltime )
               zgrapoc(ji,jj,jk) = zgrapoc(ji,jj,jk) * ( xfracmigm1 + xfracmig * zmigreltime )
               zgrapof(ji,jj,jk) = zgrapof(ji,jj,jk) * ( xfracmigm1 + xfracmig * zmigreltime )
               zgrabsi(ji,jj,jk) = zgrabsi(ji,jj,jk) * ( xfracmigm1 + xfracmig * zmigreltime )
            ENDIF
         END_3D
      
         ! The inorganic and organic fluxes induced by migrating organisms are added at the 
         ! the migration depth (corresponding indice is set by kmig)
         ! --------------------------------------------------------------------------------
         DO_2D( 0, 0, 0, 0 )
            IF( tmask(ji,jj,1) == 1.) THEN
               jkt = kmig(ji,jj)
               zdep = 1. / e3t(ji,jj,jkt,Kmm)
               zgrarem(ji,jj,jkt) = zgrarem(ji,jj,jkt) + zgramigrem(ji,jj) * zdep 
               zgraref(ji,jj,jkt) = zgraref(ji,jj,jkt) + zgramigref(ji,jj) * zdep
               zgrapoc(ji,jj,jkt) = zgrapoc(ji,jj,jkt) + zgramigpoc(ji,jj) * zdep
               zgrapof(ji,jj,jkt) = zgrapof(ji,jj,jkt) + zgramigpof(ji,jj) * zdep
               zgrabsi(ji,jj,jkt) = zgrabsi(ji,jj,jkt) + zgramigbsi(ji,jj) * zdep
            ENDIF
         END_2D
         !
         ! Deallocate temporary variables
         ! ------------------------------
         DEALLOCATE( zgramigrem, zgramigref, zgramigpoc, zgramigpof, zgramigbsi )
      ! End of the ln_dvm_meso part
      ENDIF

      !   Update the arrays TRA which contain the biological sources and sinks
      !   This only concerns the variables which are affected by DVM (inorganic 
      !   nutrients, DOC agands, and particulate organic carbon). 
      DO_3D( 0, 0, 0, 0, 1, jpk)
         tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + zgrarem(ji,jj,jk) * sigma2
         tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zgrarem(ji,jj,jk) * sigma2
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zgrarem(ji,jj,jk) * ( 1. - sigma2 )
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - o2ut * zgrarem(ji,jj,jk) * sigma2
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zgraref(ji,jj,jk)
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zgrarem(ji,jj,jk) * sigma2
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zgrarem(ji,jj,jk) * sigma2             
         tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) + zgrapoc(ji,jj,jk)
         prodgoc(ji,jj,jk)   = prodgoc(ji,jj,jk)   + zgrapoc(ji,jj,jk)
         tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + zgrapof(ji,jj,jk)
         tr(ji,jj,jk,jpgsi,Krhs) = tr(ji,jj,jk,jpgsi,Krhs) + zgrabsi(ji,jj,jk)
      END_3D
      !
      IF( ln_ligand ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpk)
            tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) + zgrarem(ji,jj,jk) * ( 1. - sigma2 ) * ldocz
         END_3D
      ENDIF
      ! Write the output
      IF( knt == nrdttrc ) THEN
        !
        IF( l_dia_graz ) THEN  !
            CALL iom_put( "GRAZ2"   , zgrazing2(:,:,:)       * 1.e+3 * rfact2r * tmask(A2D(0),:) )
            CALL iom_put( "MesoZo2" , zgrazing2(:,:,:) * ( 1. - epsher2 - unass2 ) * (-o2ut) * sigma2  &
                 &                      * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! o2 consumption by Mesozoo
            CALL iom_put( "FEZOO2", zgraref  (:,:,:) * 1e9 * 1.e+3 * rfact2r * tmask(A2D(0),:) )
            DEALLOCATE( zgrazing2 )
        ENDIF
        !
        IF( l_dia_lprodz ) THEN
           CALL iom_put( "LPRODZ2", ( tr(A2D(0),:,jplgw,Krhs) - zzligprod(:,:,:) ) * 1e9 * 1.e+3 * rfact2r * tmask(A2D(0),:) )
           DEALLOCATE( zzligprod )
        ENDIF
        !
      ENDIF
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
        WRITE(charout, FMT="('meso')")
        CALL prt_ctl_info( charout, cdcomp = 'top' )
        CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_meso')
      !
   END SUBROUTINE p4z_meso


   SUBROUTINE p4z_meso_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_meso_init  ***
      !!
      !! ** Purpose :   Initialization of mesozooplankton parameters
      !!
      !! ** Method  :   Read the namp4zmes namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampismes
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !
      NAMELIST/namp4zmes/ part2, grazrat2, resrat2, mzrat2, xpref2n, xpref2d, xpref2z,   &
         &                xpref2c, xpref2m, xthresh2dia, xthresh2phy, xthresh2zoo, xthresh2poc, xthresh2mes, &
         &                xthresh2, xkgraz2, epsher2, epsher2min, sigma2, unass2, grazflux, ln_dvm_meso,  &
         &                xsigma2, xsigma2del, xfracmig
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*) 
         WRITE(numout,*) 'p4z_meso_init : Initialization of mesozooplankton parameters'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,namp4zmes)
      READ_NML_CFG(numnatp,namp4zmes)
      IF(lwm) WRITE( numonp, namp4zmes )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : namp4zmes'
         WRITE(numout,*) '      part of calcite not dissolved in mesozoo guts  part2        =', part2
         WRITE(numout,*) '      mesozoo preference for phyto                   xpref2n      =', xpref2n
         WRITE(numout,*) '      mesozoo preference for diatoms                 xpref2d      =', xpref2d
         WRITE(numout,*) '      mesozoo preference for zoo                     xpref2z      =', xpref2z
         WRITE(numout,*) '      mesozoo preference for poc                     xpref2c      =', xpref2c
         WRITE(numout,*) '      mesozoo preference for mesozoo                 xpref2m      = ', xpref2m
         WRITE(numout,*) '      microzoo feeding threshold  for mesozoo        xthresh2zoo  =', xthresh2zoo
         WRITE(numout,*) '      diatoms feeding threshold  for mesozoo         xthresh2dia  =', xthresh2dia
         WRITE(numout,*) '      nanophyto feeding threshold for mesozoo        xthresh2phy  =', xthresh2phy
         WRITE(numout,*) '      poc feeding threshold for mesozoo              xthresh2poc  =', xthresh2poc
         WRITE(numout,*) '      mesozoo feeding threshold for mesozoo          xthresh2mes  = ', xthresh2mes
         WRITE(numout,*) '      feeding threshold for mesozooplankton          xthresh2     =', xthresh2
         WRITE(numout,*) '      exsudation rate of mesozooplankton             resrat2      =', resrat2
         WRITE(numout,*) '      mesozooplankton mortality rate                 mzrat2       =', mzrat2
         WRITE(numout,*) '      maximal mesozoo grazing rate                   grazrat2     =', grazrat2
         WRITE(numout,*) '      mesozoo flux feeding rate                      grazflux     =', grazflux
         WRITE(numout,*) '      non assimilated fraction of P by mesozoo       unass2       =', unass2
         WRITE(numout,*) '      Efficiency of Mesozoo growth                   epsher2      =', epsher2
         WRITE(numout,*) '      Minimum Efficiency of Mesozoo growth           epsher2min   =', epsher2min
         WRITE(numout,*) '      Fraction of mesozoo excretion as DOM           sigma2       =', sigma2
         WRITE(numout,*) '      half sturation constant for grazing 2          xkgraz2      =', xkgraz2
         WRITE(numout,*) '      Width of the grazing window                     xsigma2     =', xsigma2
         WRITE(numout,*) '      Maximum additional width of the grazing window  xsigma2del  =', xsigma2del
         WRITE(numout,*) '      Diurnal vertical migration of mesozoo.         ln_dvm_meso  =', ln_dvm_meso
         WRITE(numout,*) '      Fractional biomass of meso  that performs DVM  xfracmig     =', xfracmig
      ENDIF
      !
      xfracmigm1 = 1.0 - xfracmig
      rlogfactdn = LOG(1.67 / 6.0)
      !
   END SUBROUTINE p4z_meso_init

   SUBROUTINE p4z_meso_depmig( Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_meso_depmig  ***
      !!
      !! ** Purpose :   Computation the migration depth of mesozooplankton
      !!
      !! ** Method  :   Computes the DVM depth of mesozooplankton from oxygen
      !!      temperature and chlorophylle following the parameterization 
      !!      proposed by Bianchi et al. (2013)
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)  ::  Kbb, kmm ! time level indices
      !
      INTEGER  :: ji, jj, jk, itt
      !
      REAL(wp) :: ztotchl, z1dep
      REAL(wp), DIMENSION(A2D(0)) :: oxymoy, tempmoy, zdepmoy

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p4z_meso_depmig')
      !
      oxymoy(:,:)  = 0.
      tempmoy(:,:) = 0.
      zdepmoy(:,:) = 0.
      depmig (:,:) = 5.
      kmig   (:,:) = 1
      !
      !
#if defined key_RK3
      ! Don't consider mid-step values if online coupling
      ! because these are possibly non-monotonic (even with FCT):
      IF ( l_offline ) THEN ; itt = Kmm ; ELSE ; itt = Kbb ; ENDIF
#else
      itt = Kmm
#endif

      ! Compute the averaged values of oxygen, temperature over the domain 
      ! 150m to 500 m depth.
      ! ------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpk)
         IF( tmask(ji,jj,jk) == 1.) THEN
            IF( gdept(ji,jj,jk,Kmm) >= 150. .AND. gdept(ji,jj,jk,kmm) <= 500.) THEN
               oxymoy(ji,jj)  = oxymoy(ji,jj)  + tr(ji,jj,jk,jpoxy,Kbb) * 1E6 * e3t(ji,jj,jk,Kmm)
               tempmoy(ji,jj) = tempmoy(ji,jj) + ts(ji,jj,jk,jp_tem,itt)      * e3t(ji,jj,jk,kmm)
               zdepmoy(ji,jj) = zdepmoy(ji,jj) + e3t(ji,jj,jk,Kmm)
            ENDIF
         ENDIF
      END_3D

      ! Compute the difference between surface values and the mean values in the mesopelagic
      ! domain
      ! ------------------------------------------------------------------------------------
      DO_2D( 0, 0, 0, 0 )
         z1dep = 1. / ( zdepmoy(ji,jj) + rtrn )
         oxymoy(ji,jj)  = tr(ji,jj,1,jpoxy,Kbb) * 1E6 - oxymoy(ji,jj)  * z1dep
         tempmoy(ji,jj) = ts(ji,jj,1,jp_tem,itt)      - tempmoy(ji,jj) * z1dep
      END_2D
      !
      ! Computation of the migration depth based on the parameterization of 
      ! Bianchi et al. (2013)
      ! -------------------------------------------------------------------
      DO_2D( 0, 0, 0, 0 )
         IF( tmask(ji,jj,1) == 1. ) THEN
            ztotchl = ( tr(ji,jj,1,jpnch,Kbb) + tr(ji,jj,1,jpdch,Kbb) ) * 1E6
            depmig(ji,jj) = 398. - 0.56 * oxymoy(ji,jj) -115. * log10(ztotchl) + 0.36 * hmld(ji,jj) -2.4 * tempmoy(ji,jj)
         ENDIF
      END_2D
      ! 
      ! Computation of the corresponding jk indice 
      ! ------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( depmig(ji,jj) >= gdepw(ji,jj,jk,Kmm) .AND. depmig(ji,jj) < gdepw(ji,jj,jk+1,Kmm) ) THEN
             kmig(ji,jj) = jk
          ENDIF
      END_3D
      !
      ! Correction of the migration depth and indice based on O2 levels
      ! If O2 is too low, imposing a migration depth at this low O2 levels
      ! would lead to negative O2 concentrations (respiration while O2 is close
      ! to 0. Thus, to avoid that problem, the migration depth is adjusted so
      ! that it falls above the OMZ
      ! -----------------------------------------------------------------------
      DO_2D( 0, 0, 0, 0 )
         IF( tr(ji,jj,kmig(ji,jj),jpoxy,Kbb) < 5E-6 ) THEN
            DO jk = kmig(ji,jj),1,-1
               IF( tr(ji,jj,jk,jpoxy,Kbb) >= 5E-6 .AND. tr(ji,jj,jk+1,jpoxy,Kbb)  < 5E-6) THEN
                  kmig(ji,jj) = jk
                  depmig(ji,jj) = gdept(ji,jj,jk,Kmm)
               ENDIF
            END DO
         ENDIF
      END_2D
      !
      IF( ln_timing )   CALL timing_stop('p4z_meso_depmig')
      !
   END SUBROUTINE p4z_meso_depmig

   INTEGER FUNCTION p4z_meso_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_meso_alloc  ***
      !!----------------------------------------------------------------------
      !
      ALLOCATE( depmig(A2D(0)), kmig(A2D(0)), STAT= p4z_meso_alloc  )
      !
      IF( p4z_meso_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p4z_meso_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p4z_meso_alloc

   !!======================================================================
END MODULE p4zmeso
