MODULE p4zmicro
   !!======================================================================
   !!                         ***  MODULE p4zmicro  ***
   !! TOP :   PISCES Compute the sources/sinks for microzooplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!----------------------------------------------------------------------
   !!   p4z_micro      : Compute the sources/sinks for microzooplankton
   !!   p4z_micro_init : Initialize and read the appropriate namelist
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p4zprod         ! production
   USE iom             ! I/O manager
   USE prtctl          ! print control for debugging

   IMPLICIT NONE
   PRIVATE

   !! * Shared module variables
   PUBLIC   p4z_micro         ! called in p4zbio.F90
   PUBLIC   p4z_micro_init    ! called in trcsms_pisces.F90

   REAL(wp), PUBLIC ::   part        !: part of calcite not dissolved in microzoo guts
   REAL(wp), PUBLIC ::   xprefc      !: microzoo preference for POC 
   REAL(wp), PUBLIC ::   xprefn      !: microzoo preference for nanophyto
   REAL(wp), PUBLIC ::   xprefd      !: microzoo preference for diatoms
   REAL(wp), PUBLIC ::   xprefz      !: microzoo preference for microzooplankton
   REAL(wp), PUBLIC ::   xthreshdia  !: diatoms feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::   xthreshphy  !: nanophyto threshold for microzooplankton 
   REAL(wp), PUBLIC ::   xthreshpoc  !: poc threshold for microzooplankton 
   REAL(wp), PUBLIC ::   xthreshzoo  !: microzoo threshold for microzooplankton 
   REAL(wp), PUBLIC ::   xthresh     !: feeding threshold for microzooplankton 
   REAL(wp), PUBLIC ::   resrat      !: exsudation rate of microzooplankton
   REAL(wp), PUBLIC ::   mzrat       !: microzooplankton mortality rate 
   REAL(wp), PUBLIC ::   grazrat     !: maximal microzoo grazing rate
   REAL(wp), PUBLIC ::   xkgraz      !: Half-saturation constant of assimilation
   REAL(wp), PUBLIC ::   unass       !: Non-assimilated part of food
   REAL(wp), PUBLIC ::   sigma1      !: Fraction of microzoo excretion as DOM 
   REAL(wp), PUBLIC ::   epsher      !: growth efficiency for grazing 1 
   REAL(wp), PUBLIC ::   epshermin   !: minimum growth efficiency for grazing 1
   REAL(wp), PUBLIC ::   xsigma      !: Width of the grazing window
   REAL(wp), PUBLIC ::   xsigmadel   !: Maximum additional width of the grazing window at low food density 

   REAL(wp)         :: rlogfactdn
   LOGICAL          :: l_dia_graz, l_dia_lprodz

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_micro( kt, knt, Kbb, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_micro  ***
      !!
      !! ** Purpose :   Compute the sources/sinks for microzooplankton
      !!                This includes ingestion and assimilation, flux feeding
      !!                and mortality. We use a passive prey switching  
      !!                parameterization.
      !!                All living compartments smaller than microzooplankton
      !!                are potential preys of microzooplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt    ! ocean time step
      INTEGER, INTENT(in) ::   knt   ! ??? 
      INTEGER, INTENT(in) ::   Kbb, Krhs  ! time level indices
      !
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompadi, zcompaz , zcompaph, zcompapoc
      REAL(wp) :: zgraze, zdenom, zdenom2, zfact, zfood, zfoodlim, zbeta
      REAL(wp) :: zepsherf, zepshert, zepsherq, zepsherv, zgrarsig, zgraztotc, zgraztotn, zgraztotf
      REAL(wp) :: zgrarem, zgrafer, zgrapoc, zprcaca, zmortz
      REAL(wp) :: zrespz, ztortz, zgrasratf, zgrasratn
      REAL(wp) :: zgraznc, zgrazz, zgrazpoc, zgrazdc, zgrazpof, zgrazdf, zgraznf
      REAL(wp) :: zsigma, zsigma2, zdiffdn, zsizedn, ztmp1, ztmp2, ztmp3, ztmp4, ztmptot
      REAL(wp) :: zmaxsi, zpexpod, zrsize, zrsizen, zrsized, znano, zdiat
      REAL(wp), DIMENSION(A2D(0),jpk) :: zproportn, zproportd, zprodlig
      REAL(wp), DIMENSION(:,:,:)  , ALLOCATABLE :: zgrazing, zfezoo, zzligprod
      CHARACTER (len=25) :: charout

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_micro')
      !
      IF( kt == nittrc000 )  THEN 
         l_dia_graz    = iom_use( "GRAZ1" ) .OR. iom_use( "FEZOO" ) .OR. iom_use( "MicroZo2" ) 
         l_dia_lprodz  = ln_ligand .AND. iom_use( "LPRODZ" ) 
      ENDIF
      !
      IF( l_dia_graz ) THEN
         ALLOCATE( zgrazing(A2D(0),jpk), zfezoo(A2D(0),jpk) ) 
         zgrazing(A2D(0),jpk) = 0.
         zfezoo(A2D(0),:)     = tr(A2D(0),:,jpfer,Krhs)
      ENDIF
      IF( l_dia_lprodz ) THEN
         ALLOCATE( zzligprod(A2D(0),jpk) )   
         zzligprod(A2D(0),:) = tr(A2D(0),:,jplgw,Krhs)
      ENDIF

      ! Computation of the proportion of diatoms that escapes predation by microzooplankton
      ! This is a fit to the actual relationship when a power law distribution is assumed 
      ! for diatoms.
      ! ------------------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF ( tmask(ji,jj,jk) == 1 ) THEN
            zproportd(ji,jj,jk) = EXP( 0.067 - 0.033 * sized(ji,jj,jk) * 6.0) / ( EXP( 0.067 -0.033 * 6.0 ) )
            zproportn(ji,jj,jk) = EXP( 0.05 - 0.036 * sizen(ji,jj,jk) * 1.67) / ( EXP( 0.05 -0.036 * 1.67 ) )
         ELSE
            zproportn(ji,jj,jk) = 1.0
            zproportd(ji,jj,jk) = 1.0
         ENDIF
      END_3D

      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zcompaz   = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - 1.e-9 ), 0.e0 )
         zfact     = xstep * tgfunc2(ji,jj,jk) * zcompaz

         ! Proportion of diatoms that are within the size range
         ! accessible to microzooplankton. 
         ! Diatoms are supposed to range from 5 to 50 um.
         ! There may be an issue if zpexpod = 0 which occurs for sized 
         ! close to 5.476 which can only occur when xsizern s larger than 6)

         !  linear mortality of mesozooplankton
         !  A michaelis menten modulation term is used to avoid extinction of 
         !  microzooplankton at very low food concentrations. Mortality is 
         !  enhanced in low O2 waters
         !  -----------------------------------------------------------------
         zrespz    = resrat * zfact * tr(ji,jj,jk,jpzoo,Kbb) / ( xkmort + tr(ji,jj,jk,jpzoo,Kbb) )  &
           &         * ( 1.0 + 3. * nitrfac(ji,jj,jk) )

         !  Zooplankton quadratic mortality. A square function has been selected with
         !  to mimic predation and disease (density dependent mortality). It also tends
         !  to stabilise the model
         !  -------------------------------------------------------------------------
         ztortz    = mzrat * 1.e6 * zfact * tr(ji,jj,jk,jpzoo,Kbb) * (1. - nitrfac(ji,jj,jk))
         zmortz    = ztortz + zrespz

         !   Computation of the abundance of the preys
         !   A threshold can be specified in the namelist
         !   Diatoms have a specific treatment. WHen concentrations 
         !   exceed a certain value, diatoms are suppposed to be too 
         !   big for microzooplankton.
         !   --------------------------------------------------------
         zcompadi  = zproportd(ji,jj,jk) * MAX( ( tr(ji,jj,jk,jpdia,Kbb) - xthreshdia ), 0.e0 )
         zcompaph  = zproportn(ji,jj,jk) * MAX( ( tr(ji,jj,jk,jpphy,Kbb) - xthreshphy ), 0.e0 )
         zcompapoc = MAX( ( tr(ji,jj,jk,jppoc,Kbb) - xthreshpoc ), 0.e0 )
         zcompaz   = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - xthreshzoo ), 0.e0 )
 
         ! Microzooplankton grazing
         ! The total amount of food is the sum of all preys accessible to mesozooplankton 
         ! multiplied by their food preference
         ! A threshold can be specified in the namelist (xthresh). However, when food 
         ! concentration is close to this threshold, it is decreased to avoid the 
         ! accumulation of food in the mesozoopelagic domain
         ! -------------------------------------------------------------------------------
         zfood     = xprefn * zcompaph + xprefc * zcompapoc + xprefd * zcompadi + xprefz * zcompaz
         zfoodlim  = zfood - MIN(xthresh,0.5*zfood)
         zdenom    = zfoodlim / ( xkgraz + zfoodlim )
         zgraze    = grazrat * xstep * tgfunc2(ji,jj,jk) * tr(ji,jj,jk,jpzoo,Kbb) * (1. - nitrfac(ji,jj,jk))

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
         zdenom2   = zdenom * zdenom
         zsigma    = 1.0 - zdenom2/(0.05 * 0.05 + zdenom2)
         zsigma    = xsigma + xsigmadel * zsigma
         zsigma2   = 2.0 * zsigma * zsigma
         !
         zsizedn   = rlogfactdn + ( logsizen(ji,jj,jk) - logsized(ji,jj,jk) )
         zdiffdn   = EXP( -zsizedn * zsizedn / zsigma2 )
         ztmp1     = xprefn * zcompaph * ( zcompaph + zdiffdn * zcompadi ) 
         ztmp2     = xprefd * zcompadi * ( zdiffdn * zcompaph + zcompadi )
         ztmp3     = xprefc * zcompapoc * zcompapoc 
         ztmp4     = xprefz * zcompaz * zcompaz
         ztmptot   = ztmp1 + ztmp2 + ztmp3 + ztmp4 + rtrn
         ztmp1     = ztmp1 / ztmptot
         ztmp2     = ztmp2 / ztmptot
         ztmp3     = ztmp3 / ztmptot
         ztmp4     = ztmp4 / ztmptot

         ! Ingestion terms on the different preys of microzooplankton
         zgraznc   = zgraze   * ztmp1 * zdenom  ! Nanophytoplankton
         zgrazdc   = zgraze   * ztmp2 * zdenom  ! Diatoms
         zgrazpoc  = zgraze   * ztmp3 * zdenom  ! POC
         zgrazz    = zgraze   * ztmp4 * zdenom  ! Microzoo

         ! Ingestion terms on the iron content of the different preys
         zgraznf   = zgraznc  * tr(ji,jj,jk,jpnfe,Kbb) / (tr(ji,jj,jk,jpphy,Kbb) + rtrn)
         zgrazpof  = zgrazpoc * tr(ji,jj,jk,jpsfe,Kbb) / (tr(ji,jj,jk,jppoc,Kbb) + rtrn)
         zgrazdf   = zgrazdc  * tr(ji,jj,jk,jpdfe,Kbb) / (tr(ji,jj,jk,jpdia,Kbb) + rtrn)
         !
         ! Total ingestion rate in C, Fe, N units
         zgraztotc = zgraznc + zgrazpoc + zgrazdc + zgrazz
         IF( l_dia_graz )   zgrazing(ji,jj,jk) = zgraztotc

         zgraztotf = zgraznf + zgrazdf  + zgrazpof + zgrazz * feratz
         zgraztotn = zgraznc * quotan(ji,jj,jk) + zgrazpoc + zgrazdc * quotad(ji,jj,jk) + zgrazz

         !   Stoichiometruc ratios of the food ingested by zooplanton 
         !   --------------------------------------------------------
         zgrasratf = ( zgraztotf + rtrn ) / ( zgraztotc + rtrn )
         zgrasratn = ( zgraztotn + rtrn ) / ( zgraztotc + rtrn )

         ! Microzooplankton efficiency. 
         ! We adopt a formulation proposed by Mitra et al. (2007)
         ! The gross growth efficiency is controled by the most limiting nutrient.
         ! Growth is also further decreased when the food quality is poor. This is currently
         ! hard coded : it can be decreased by up to 50% (zepsherq)
         ! GGE can also be decreased when food quantity is high, zepsherf (Montagnes and 
         ! Fulton, 2012)
         ! -----------------------------------------------------------------------------
         zepshert  =  MIN( 1., zgrasratn, zgrasratf / feratz )
         zbeta     =  MAX( 0., (epsher - epshermin) )
         ! Food quantity deprivation of the GGE
         zepsherf  = epshermin + zbeta / ( 1.0 + 0.04E6 * 12. * zfood * zbeta )
         ! Food quality deprivation of the GGE
         zepsherq  = 0.5 + (1.0 - 0.5) * zepshert * ( 1.0 + 1.0 ) / ( zepshert + 1.0 )
         ! Actual GGE of microzooplankton
         zepsherv  = zepsherf * zepshert * zepsherq
         ! Excretion of Fe
         zgrafer   = zgraztotc * MAX( 0. , ( 1. - unass ) * zgrasratf - feratz * zepsherv ) 
         ! Excretion of C, N, P
         zgrarem   = zgraztotc * ( 1. - zepsherv - unass )
         ! Egestion of C, N, P
         zgrapoc   = zgraztotc * unass

         !  Update of the TRA arrays
         !  ------------------------
         ! Fraction of excretion as inorganic nutrients and DIC
         zgrarsig  = zgrarem * sigma1
         tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + zgrarsig
         tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zgrarsig
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zgrarem - zgrarsig
         zprodlig(ji,jj,jk)      = (zgrarem - zgrarsig) * ldocz
         !
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - o2ut * zgrarsig
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zgrafer
         tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zgrapoc
         prodpoc(ji,jj,jk)       = prodpoc(ji,jj,jk) + zgrapoc
         tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + zgraztotf * unass
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zgrarsig
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zgrarsig
         !   Update the arrays TRA which contain the biological sources and sinks
         !   --------------------------------------------------------------------
         tr(ji,jj,jk,jpzoo,Krhs) = tr(ji,jj,jk,jpzoo,Krhs) - zmortz + zepsherv * zgraztotc - zgrazz 
         tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs) - zgraznc
         tr(ji,jj,jk,jpdia,Krhs) = tr(ji,jj,jk,jpdia,Krhs) - zgrazdc
         tr(ji,jj,jk,jpnch,Krhs) = tr(ji,jj,jk,jpnch,Krhs) - zgraznc * tr(ji,jj,jk,jpnch,Kbb)/(tr(ji,jj,jk,jpphy,Kbb)+rtrn)
         tr(ji,jj,jk,jpdch,Krhs) = tr(ji,jj,jk,jpdch,Krhs) - zgrazdc * tr(ji,jj,jk,jpdch,Kbb)/(tr(ji,jj,jk,jpdia,Kbb)+rtrn)
         tr(ji,jj,jk,jpdsi,Krhs) = tr(ji,jj,jk,jpdsi,Krhs) - zgrazdc * tr(ji,jj,jk,jpdsi,Kbb)/(tr(ji,jj,jk,jpdia,Kbb)+rtrn)
         tr(ji,jj,jk,jpgsi,Krhs) = tr(ji,jj,jk,jpgsi,Krhs) + zgrazdc * tr(ji,jj,jk,jpdsi,Kbb)/(tr(ji,jj,jk,jpdia,Kbb)+rtrn)
         tr(ji,jj,jk,jpnfe,Krhs) = tr(ji,jj,jk,jpnfe,Krhs) - zgraznf
         tr(ji,jj,jk,jpdfe,Krhs) = tr(ji,jj,jk,jpdfe,Krhs) - zgrazdf
         tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zmortz - zgrazpoc
         prodpoc(ji,jj,jk)       = prodpoc(ji,jj,jk) + zmortz
         conspoc(ji,jj,jk)       = conspoc(ji,jj,jk) - zgrazpoc
         tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + feratz * zmortz - zgrazpof
         !
         ! Calcite remineralization due to zooplankton activity
         ! part of the ingested calcite is not dissolving in the acidic gut
         ! ----------------------------------------------------------------
         zprcaca = xfracal(ji,jj,jk) * zgraznc
         prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
         !
         zprcaca = part * zprcaca
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zprcaca
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - 2. * zprcaca
         tr(ji,jj,jk,jpcal,Krhs) = tr(ji,jj,jk,jpcal,Krhs) + zprcaca
      END_3D

      IF (ln_ligand ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) + zprodlig(ji,jj,jk)
         END_3D 
      ENDIF
      !
      IF( knt == nrdttrc ) THEN
        !
        IF( l_dia_graz ) THEN  !   
            CALL iom_put( "GRAZ1"    , zgrazing(:,:,:) *  1.e+3 * rfact2r * tmask(A2D(0),:) )
            CALL iom_put( "MicroZo2" , zgrazing(:,:,:) * ( 1. - epsher - unass ) * (-o2ut) * sigma1  &
                 &                      * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! o2 consumption by Microzoo
            CALL iom_put( "FEZOO", ( tr(A2D(0),:,jpfer,Krhs) - zfezoo(:,:,:) ) * 1e9 * 1.e+3 * rfact2r * tmask(A2D(0),:) )
            DEALLOCATE( zgrazing, zfezoo )
        ENDIF
        !
        IF( l_dia_lprodz ) THEN
           CALL iom_put( "LPRODZ", ( tr(A2D(0),:,jplgw,Krhs) - zzligprod(:,:,:) ) * 1e9 * 1.e+3 * rfact2r * tmask(A2D(0),:) )
           DEALLOCATE( zzligprod )
        ENDIF
        !
      ENDIF
      !
      IF(sn_cfctl%l_prttrc) THEN      ! print mean trends (used for debugging)
         WRITE(charout, FMT="('micro')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_micro')
      !
   END SUBROUTINE p4z_micro


   SUBROUTINE p4z_micro_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_micro_init  ***
      !!
      !! ** Purpose :   Initialization of microzooplankton parameters
      !!
      !! ** Method  :   Read the namp4zzoo namelist and check the parameters
      !!                called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp4zzoo
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !
      NAMELIST/namp4zzoo/ part, grazrat, resrat, mzrat, xprefn, xprefc, &
         &                xprefd, xprefz, xthreshdia, xthreshphy, xthreshpoc, xthreshzoo, &
         &                xthresh, xkgraz, epsher, epshermin, sigma1, unass,  &
         &                xsigma, xsigmadel
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*) 
         WRITE(numout,*) 'p4z_micro_init : Initialization of microzooplankton parameters'
         WRITE(numout,*) '~~~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,namp4zzoo)
      READ_NML_CFG(numnatp,namp4zzoo)
      IF(lwm) WRITE( numonp, namp4zzoo )
      !
      IF(lwp) THEN                         ! control print
         !
         WRITE(numout,*) '   Namelist : namp4zzoo'
         WRITE(numout,*) '      part of calcite not dissolved in microzoo guts  part        =', part
         WRITE(numout,*) '      microzoo preference for POC                     xprefc      =', xprefc
         WRITE(numout,*) '      microzoo preference for nano                    xprefn      =', xprefn
         WRITE(numout,*) '      microzoo preference for diatoms                 xprefd      =', xprefd
         WRITE(numout,*) '      microzoo preference for microzooplankton        xprefz      =', xprefz
         WRITE(numout,*) '      diatoms feeding threshold  for microzoo         xthreshdia  =', xthreshdia
         WRITE(numout,*) '      nanophyto feeding threshold for microzoo        xthreshphy  =', xthreshphy
         WRITE(numout,*) '      poc feeding threshold for microzoo              xthreshpoc  =', xthreshpoc
         WRITE(numout,*) '      microzoo feeding threshold for microzoo         xthreshzoo  =', xthreshzoo
         WRITE(numout,*) '      feeding threshold for microzooplankton          xthresh     =', xthresh
         WRITE(numout,*) '      exsudation rate of microzooplankton             resrat      =', resrat
         WRITE(numout,*) '      microzooplankton mortality rate                 mzrat       =', mzrat
         WRITE(numout,*) '      maximal microzoo grazing rate                   grazrat     =', grazrat
         WRITE(numout,*) '      non assimilated fraction of P by microzoo       unass       =', unass
         WRITE(numout,*) '      Efficicency of microzoo growth                  epsher      =', epsher
         WRITE(numout,*) '      Minimum efficicency of microzoo growth          epshermin   =', epshermin
         WRITE(numout,*) '      Fraction of microzoo excretion as DOM           sigma1      =', sigma1
         WRITE(numout,*) '      half saturation constant for grazing 1          xkgraz      =', xkgraz
         WRITE(numout,*) '      Width of the grazing window                     xsigma      =', xsigma
         WRITE(numout,*) '      Maximum additional width of the grazing window  xsigmadel   =', xsigmadel
         !
      ENDIF
      !
      rlogfactdn = LOG(1.67 / 6.0)
      !
   END SUBROUTINE p4z_micro_init

   !!======================================================================
END MODULE p4zmicro
