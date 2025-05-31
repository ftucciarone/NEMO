MODULE p2zmicro
   !!======================================================================
   !!                         ***  MODULE p2zmicro  ***
   !! TOP :   REDUCED PISCES Compute the sources/sinks for microzooplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!----------------------------------------------------------------------
   !!   p2z_micro      : Compute the sources/sinks for microzooplankton
   !!   p2z_micro_init : Initialize and read the appropriate namelist
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p2zprod         ! production
   USE p4zsink         ! sedimentation of particles
   USE iom             ! I/O manager
   USE prtctl          ! print control for debugging

   IMPLICIT NONE
   PRIVATE

   !! * Shared module variables
   PUBLIC   p2z_micro         ! called in p2zbio.F90
   PUBLIC   p2z_micro_init    ! called in trcsms_pisces.F90

   REAL(wp), PUBLIC ::   part        !: part of calcite not dissolved in microzoo guts
   REAL(wp), PUBLIC ::   xprefc      !: microzoo preference for POC 
   REAL(wp), PUBLIC ::   xprefn      !: microzoo preference for nanophyto
   REAL(wp), PUBLIC ::   xprefz      !: microzoo preference for microzooplankton
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

   LOGICAL          ::   l_dia_graz, l_dia_lprodz

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_micro( kt, knt, Kbb, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_micro  ***
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
      REAL(wp) :: zcompaz , zcompaph, zcompapoc
      REAL(wp) :: zgraze, zdenom, zfact, zfood, zfoodlim, zbeta
      REAL(wp) :: zepsherf, zepshert, zepsherq, zepsherv, zgrarsig, zgraztotc, zgraztotn
      REAL(wp) :: zgrarem, zgrapoc, zprcaca, zmortz
      REAL(wp) :: zrespz, ztortz, zgrasratn
      REAL(wp) :: zgraznc, zgrazz, zgrazpoc
      REAL(wp) :: ztmp1, ztmp2, ztmp3, ztmptot, zproport, zproport2
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: zgrazing
      CHARACTER (len=25) :: charout

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_micro')
      !
      IF( kt == nittrc000 )  l_dia_graz  = iom_use( "GRAZ1" ) .OR. iom_use( "MicroZo2" )
      IF( l_dia_graz )  ALLOCATE( zgrazing(A2D(0),jpk) ) 
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zcompaz = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - 1.e-9 ), 0.e0 )
         zfact   = xstep * tgfunc2(ji,jj,jk) * zcompaz

         ! Proportion of diatoms that are within the size range
         ! accessible to microzooplankton. 
         ztmp1  = MAX(sizen(ji,jj,jk)/3.0,1.0)
         ztmp2  = sizen(ji,jj,jk) * sizen(ji,jj,jk)
         zproport  = ztmp1**(-0.48) * ( 1.0 - ( ztmp2 - 1.0 ) / 160.0)
         zproport2 = MIN(1.0, ( wsbio2 - wsbio3(ji,jj,jk) ) / ( wsbio2 - wsbio ) )

         !  linear mortality of mesozooplankton
         !  A michaelis menten modulation term is used to avoid extinction of 
         !  microzooplankton at very low food concentrations. Mortality is 
         !  enhanced in low O2 waters
         !  -----------------------------------------------------------------
         zrespz = resrat * zfact * ( tr(ji,jj,jk,jpzoo,Kbb) / ( xkmort + tr(ji,jj,jk,jpzoo,Kbb) )  &
            &   + 3. * nitrfac(ji,jj,jk) )

         !  Zooplankton quadratic mortality. A square function has been selected with
         !  to mimic predation and disease (density dependent mortality). It also tends
         !  to stabilise the model
         !  -------------------------------------------------------------------------
         ztortz = mzrat * 1.e6 * zfact * tr(ji,jj,jk,jpzoo,Kbb) * (1. - nitrfac(ji,jj,jk))
         zmortz = ztortz + zrespz

         !   Computation of the abundance of the preys
         !   A threshold can be specified in the namelist
         !   Diatoms have a specific treatment. WHen concentrations 
         !   exceed a certain value, diatoms are suppposed to be too 
         !   big for microzooplankton.
         !   --------------------------------------------------------
         zcompaph  = zproport * MAX( ( tr(ji,jj,jk,jpphy,Kbb) - xthreshphy ), 0.e0 )
         zcompapoc = zproport2 * MAX( ( tr(ji,jj,jk,jppoc,Kbb) - xthreshpoc ), 0.e0 )
         zcompaz   = MAX( ( tr(ji,jj,jk,jpzoo,Kbb) - xthreshzoo ), 0.e0 )
 
         ! Microzooplankton grazing
         ! The total amount of food is the sum of all preys accessible to mesozooplankton 
         ! multiplied by their food preference
         ! A threshold can be specified in the namelist (xthresh). However, when food 
         ! concentration is close to this threshold, it is decreased to avoid the 
         ! accumulation of food in the mesozoopelagic domain
         ! -------------------------------------------------------------------------------
         zfood     = xprefn * zcompaph + xprefc * zcompapoc + xprefz * zcompaz
         zfoodlim  = MAX( 0. , zfood - min(xthresh,0.5*zfood) )
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
         ztmp1 = xprefn * zcompaph * zcompaph
         ztmp2 = xprefc * zcompapoc * zcompapoc
         ztmp3 = xprefz * zcompaz * zcompaz
         ztmptot = ztmp1 + ztmp2 + ztmp3 + rtrn
         ztmp1 = ztmp1 / ztmptot
         ztmp2 = ztmp2 / ztmptot
         ztmp3 = ztmp3 / ztmptot

         ! Ingestion terms on the different preys of microzooplankton
         zgraznc   = zgraze * ztmp1 * zdenom  ! Nanophytoplankton
         zgrazpoc  = zgraze * ztmp2 * zdenom  ! POC
         zgrazz    = zgraze * ztmp3 * zdenom  ! Microzoo

         ! Ingestion terms on the iron content of the different preys
         ! Total ingestion rate in C, Fe, N units
         zgraztotc = zgraznc + zgrazpoc + zgrazz
         IF( l_dia_graz )   zgrazing(ji,jj,jk) = zgraztotc
         zgraztotn = zgraznc * quotan(ji,jj,jk) + zgrazpoc + zgrazz

         !   Stoichiometruc ratios of the food ingested by zooplanton 
         !   --------------------------------------------------------
         zgrasratn = ( zgraztotn + rtrn ) / ( zgraztotc + rtrn )

         ! Microzooplankton efficiency. 
         ! We adopt a formulation proposed by Mitra et al. (2007)
         ! The gross growth efficiency is controled by the most limiting nutrient.
         ! Growth is also further decreased when the food quality is poor. This is currently
         ! hard coded : it can be decreased by up to 50% (zepsherq)
         ! GGE can also be decreased when food quantity is high, zepsherf (Montagnes and 
         ! Fulton, 2012)
         ! -----------------------------------------------------------------------------
         zepshert  =  MIN( 1., zgrasratn )
         zbeta     =  MAX(0., (epsher - epshermin) )
         ! Food quantity deprivation of the GGE
         zepsherf  = epshermin + zbeta / ( 1.0 + 0.04E6 * 12. * zfood * zbeta )
         ! Food quality deprivation of the GGE
         zepsherq  = 0.5 + (1.0 - 0.5) * zepshert * ( 1.0 + 1.0 ) / ( zepshert + 1.0 )
         ! Actual GGE of microzooplankton
         zepsherv  = zepsherf * zepshert * zepsherq
         ! Excretion of C, N, P
         zgrarem   = zgraztotc * ( 1. - zepsherv - unass ) + ( 1. - epsher - unass ) / ( 1. - epsher ) * ztortz
         ! Egestion of C, N, P
         zgrapoc   = zgraztotc * unass + unass / ( 1. - epsher ) * ztortz + zrespz

         !  Update of the TRA arrays
         !  ------------------------
         ! Fraction of excretion as inorganic nutrients and DIC
         zgrarsig  = zgrarem * sigma1
         tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) + zgrarsig
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zgrarem - zgrarsig
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zgrarem * feratz
         !
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - (o2ut + o2nit) * zgrarsig
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zgrarsig
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - rno3 * zgrarsig
         !   Update the arrays TRA which contain the biological sources and sinks
         !   --------------------------------------------------------------------
         tr(ji,jj,jk,jpzoo,Krhs) = tr(ji,jj,jk,jpzoo,Krhs) - zmortz + zepsherv * zgraztotc - zgrazz 
         tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs) - zgraznc
         tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zgrapoc - zgrazpoc
         prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + zgrapoc
         conspoc(ji,jj,jk) = conspoc(ji,jj,jk) - zgrazpoc
         !
         ! Calcite remineralization due to zooplankton activity
         ! part of the ingested calcite is not dissolving in the acidic gut
         ! ----------------------------------------------------------------
         zprcaca = xfracal(ji,jj,jk) * zgraznc
         prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca * part  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
         !
         zprcaca = part * zprcaca
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zprcaca
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - 2. * zprcaca
      END_3D
      !
      IF( knt == nrdttrc ) THEN
        !
        IF( l_dia_graz ) THEN  
            zgrazing(A2D(0),jpk) = 0._wp
            CALL iom_put( "GRAZ1"    , zgrazing(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )  ! grazing of phyto by zoo
            CALL iom_put( "MicroZo2" , zgrazing(:,:,:) * ( 1. - epsher - unass ) * (-o2ut) * sigma1  &
                 &                      * 1.e+3 * rfact2r * tmask(A2D(0),:) ) ! o2 consumption by Microzoo
            DEALLOCATE( zgrazing )
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
      IF( ln_timing )   CALL timing_stop('p2z_micro')
      !
   END SUBROUTINE p2z_micro


   SUBROUTINE p2z_micro_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p2z_micro_init  ***
      !!
      !! ** Purpose :   Initialization of microzooplankton parameters
      !!
      !! ** Method  :   Read the namp2zzoo namelist and check the parameters
      !!                called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp2zzoo
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !
      NAMELIST/namp2zzoo/ part, grazrat, resrat, mzrat, xprefn, xprefc, &
         &                xprefz, xthreshphy, xthreshpoc, xthreshzoo, &
         &                xthresh, xkgraz, epsher, epshermin, sigma1, unass
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*) 
         WRITE(numout,*) 'p2z_micro_init : Initialization of microzooplankton parameters'
         WRITE(numout,*) '~~~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,namp2zzoo)
      READ_NML_CFG(numnatp,namp2zzoo)
      IF(lwm) WRITE( numonp, namp2zzoo )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : namp2zzoo'
         WRITE(numout,*) '      part of calcite not dissolved in microzoo guts  part        =', part
         WRITE(numout,*) '      microzoo preference for POC                     xprefc      =', xprefc
         WRITE(numout,*) '      microzoo preference for nano                    xprefn      =', xprefn
         WRITE(numout,*) '      microzoo preference for microzooplankton        xprefz      =', xprefz
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
      ENDIF
      !
   END SUBROUTINE p2z_micro_init

   !!======================================================================
END MODULE p2zmicro
