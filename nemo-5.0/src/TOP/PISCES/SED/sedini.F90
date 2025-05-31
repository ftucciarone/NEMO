MODULE sedini
   !!======================================================================
   !!              ***  MODULE  sedini  ***
   !! Sediment : define sediment variables
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   sed_ini    : initialization, namelist read, and parameters control
   !!----------------------------------------------------------------------
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedmat
   USE sedadv
   USE trcdmp_sed
   USE trcdta
   USE iom
   USE lib_mpp         ! distribued memory computing library


   IMPLICIT NONE
   PRIVATE

   !! Module variables

   REAL(wp)    ::  &
      sedzmin = 0.3    ,  &  !: Minimum vertical spacing
      sedhmax = 10.0   ,  &  !: Maximum depth of the sediment
      sedkth  = 5.0    ,  &  !: Default parameters
      sedacr  = 3.0          !: Default parameters
      
   REAL(wp)    ::  &
      porsurf =  0.95  ,  &  !: Porosity at the surface
      porinf  =  0.75  ,  &  !: Porosity at infinite depth
      rhox    =  2.0         !: Vertical length scale of porosity variation 

   REAL(wp)    ::  &
      rcopal  =   40.        !: reactivity for si    [l.mol-1.an-1]

   REAL(wp), PUBLIC    ::  &
      redO2    =  138.  ,  &  !: Redfield coef for Oxygen
      redNo3   =   16.  ,  &  !: Redfield coef for Nitrate
      redPo4   =    1.  ,  &  !: Redfield coef for Phosphate
      redC     =  117.  ,  &  !: Redfield coef for Carbon
      redfep   =  0.175 ,  &  !: Ratio for iron bound phosphorus
      rcorg1   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      rcorg2   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      rcorg3   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      rcorg4   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      rcorg5   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      rcorg6   =   50.  ,  &  !: reactivity for POC/O2 [l.mol-1.an-1]    
      rcnh4    =   10E6 ,  &  !: reactivity for O2/NH4 [l.mol-1.an-1]  
      rch2s    =   1.E5 ,  &  !: reactivity for O2/ODU [l.mol-1.an-1] 
      rcfe2    =   5.E8 ,  &  !: reactivity for O2/Fe2+ [l.mol-1.an-1]
      rcfeh2s  =   1.E4 ,  &  !: Reactivity for FEOH/H2S [l.mol-1.an-1]
      rcfeso   =   3.E5 ,  &  !: Reactivity for FES/O2 [l.mol-1.an-1]
      rcfesp   =   5E-6 ,  &  !: Precipitation of FeS [mol/l-1.an-1]
      rcfesd   =   1E-3 ,  &  !: Dissolution of FeS [an-1]
      rcapat   =   0.35 ,  &  !: Apatite formation [an-1]
      xksedo2  =   5E-6 ,  &  !: half-sturation constant for oxic remin.
      xksedno3 =   5E-6 ,  &  !: half-saturation constant for denitrification
      xksedfeo =   0.6  ,  & !: half-saturation constant for iron remin
      xksedso4 =   2E-3       !: half-saturation constant for SO4 remin

   REAL(wp)    ::  &
      rccal   = 1000.,      & !: reactivity for calcite         [l.mol-1.an-1]
      rcligc  = 1.E-4         !: L/C ratio in POC

   REAL(wp), PUBLIC    ::  dbiot   = 15. , &  !: coefficient for bioturbation    [cm**2.(n-1)]
      dbtbzsc =  10.0  ,    &  !: Vertical scale of variation. If no variation, mixed layer in the sed [cm]
      xirrzsc = 2.0            !: Vertical scale of irrigation variation.
   REAL(wp), PUBLIC    ::  &
      ryear = 365. * 24. * 3600. !:  1 year converted in second

   REAL(wp), DIMENSION(jpwat), PUBLIC  :: diff1
   DATA diff1/ 1.0007E-5, 9.7866E-6, 3.1022E-6, 9.8066E-6, 9.1762E-6, 5.0035E-6, 3.412E-6, 4.8132E-6, 4.8132E-6, 4.8132E-6, 4.59E-6 /


   REAL(wp), DIMENSION(jpwat), PUBLIC  :: diff2
   DATA diff2/ 4.528E-7, 3.656E-7, 1.627E-7, 3.884E-7, 4.177E-7, 2.258E-7, 1.477E-7, 2.521E-7, 2.521E-7, 2.521E-7, 1.74E-7 /

   !! *  Routine accessibility
   PUBLIC sed_ini          ! routine called by opa.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
CONTAINS


   SUBROUTINE sed_ini
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_ini  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - Reading namelist
      !!               - Read the deepest water layer thickness
      !!                 ( using as mask ) in Netcdf file
      !!               - Convert unity if necessary
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!               - sets sediment grid, porosity and others constants
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-07  (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      INTEGER  :: ji, jj, js, jn, jk, ikt, ierr
      REAL(wp) :: ztmp1, ztmp2 , zfact
      !!----------------------------------------------------------------------

      ! Reading namelist.sed variables
      !---------------------------------------

      CALL ctl_opn( numsed, 'sediment.output', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )
      IF(lwm) CALL ctl_opn( numons     , 'output.namelist.sed' , 'UNKNOWN', 'FORMATTED', 'SEQUENTIAL', -1, numout, .FALSE. )

      IF (lwp) THEN
         WRITE(numsed,*)
         WRITE(numsed,*) '                 PISCES framework'
         WRITE(numsed,*) '                 SEDIMENT model'
         WRITE(numsed,*) '                version 5.0  (2024) '
         WRITE(numsed,*)
         WRITE(numsed,*)
      ENDIF

      IF(lwp) WRITE(numsed,*) ' sed_ini : Initialization of sediment module  '
      IF(lwp) WRITE(numsed,*) ' '

      ! Read sediment Namelist
      !-------------------------
      CALL sed_ini_nam

      ! Allocate SEDIMENT arrays
      ierr =        sed_alloc()
      ierr = ierr + sed_oce_alloc()
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'sed_ini: unable to allocate sediment model arrays' )

      ! Determination of sediments number of points and allocate global variables
      epkbot(:,:) = 0.
      gdepbot(:,:) = 0.
      DO_2D( 0, 0, 0, 0 )
         ikt = mbkt(ji,jj) 
         IF( tmask(ji,jj,ikt) == 1 ) epkbot(ji,jj) = e3t_0(ji,jj,ikt)
         gdepbot(ji,jj) = gdepw_0(ji,jj,ikt+1)
      END_2D

      trc_data(:,:,:) = 0.0

      ! computation of total number of ocean points
      !--------------------------------------------
      jpoce  = MAX( COUNT( epkbot(:,:) > 0. ) , 1 )

      ! Allocate memory size of global variables
      ALLOCATE( pwcp (jpoce,jpksed,jpwat) )  ;  ALLOCATE( pwcp_dta  (jpoce,jpwat) )
      ALLOCATE( pwcpa(jpoce,jpksed,jpwat) )  ;  ALLOCATE( solcpa(jpoce,jpksed,jpsol) )
      ALLOCATE( pwcpaa(jpoce,jpksed,jpwat) )
      ALLOCATE( solcp(jpoce,jpksed,jpsol) )
      ALLOCATE( rainrg(jpoce,jpsol) )        ;  ALLOCATE( xirrigtrd(jpoce,jpwat) )
      ALLOCATE( xirrigtrdtmp(jpoce,jpwat) )
      ALLOCATE( dzdep(jpoce) )
      ALLOCATE( slatit(jpoce) )              ;  ALLOCATE( slongit(jpoce) )
      ALLOCATE( zkbot(jpoce) )               ;  ALLOCATE( db(jpoce,jpksed) )
      ALLOCATE( temp(jpoce) )                ;  ALLOCATE( salt(jpoce) )  
      ALLOCATE( diff(jpoce,jpksed,jpwat ) )  ;  ALLOCATE( irrig(jpoce, jpksed) )
      ALLOCATE( wacc(jpoce) )                ;  ALLOCATE( fecratio(jpoce) )
      ALLOCATE( xlabil(jpoce) )
      ALLOCATE( densSW(jpoce) )              ;  ALLOCATE( saturco3(jpoce,jpksed) ) 
      ALLOCATE( hipor(jpoce,jpksed) )        ;  ALLOCATE( co3por(jpoce,jpksed) )
      ALLOCATE( volw3d(jpoce,jpksed) )       ;  ALLOCATE( vols3d(jpoce,jpksed) )
      ALLOCATE( rearatpom(jpoce, jpksed) )   ;  ALLOCATE( volc(jpoce,jpksed,jpsol) )
      ALLOCATE( radssol(jpksed, jpwat) )     ;  ALLOCATE( rads1sol(jpksed, jpwat) )
      ALLOCATE( apluss(jpoce, jpksed) )      ;  ALLOCATE( aminuss(jpoce,jpksed) )

      ! Initialization of global variables
      pwcp  (:,:,:) = 0.   ;  pwcp_dta  (:,:) = 0.  
      pwcpa (:,:,:) = 0.   ;  solcpa  (:,:,:) = 0.
      pwcpa (:,:,:) = 0.
      solcp (:,:,:) = 0.   ;  xlabil(:) = 0.
      rainrg(:,:  ) = 0.   ;  wacc  (:) = 0.
      xirrigtrd(:,:) = 0.0 ;  xirrigtrdtmp(:,:) = 0.0
      dzdep (:    ) = 0.   ;  dzkbot (:   ) = 0.
      slatit(:    ) = 0.   ;  slongit(:   ) = 0.
      zkbot(:     ) = 0.   ;  diff  (:,:,:) = 0. 
      temp  (:    ) = 0.   ;  salt   (:   ) = 0.  ; zkbot     (:  ) = 0.
      densSW (:   ) = 0.   ;  db     (:,:)  = 0.  ; saturco3  (:,:) = 0.
      hipor (:,:  ) = 0.   ;  co3por (:,: ) = 0.  ; irrig     (:,:) = 0. 
      volw3d (:,: ) = 0.   ;  vols3d  (:,:) = 0.  ; volc     (:,:,:) = 0.     
      fecratio(:)   = 1E-5 ;  rearatpom(:,:)= 0. 
      radssol(:,:)  = 1.0  ;  rads1sol(:,:) = 0.
      apluss(:,:)   = 0.0  ;  aminuss(:,:)  = 0.0

      ! Chemical variables      
      ALLOCATE( akbs  (jpoce) )  ;  ALLOCATE( ak1s   (jpoce) )  ;  ALLOCATE( ak2s  (jpoce) ) ;  ALLOCATE( akws  (jpoce) )     
      ALLOCATE( ak1ps (jpoce) )  ;  ALLOCATE( ak2ps  (jpoce) )  ;  ALLOCATE( ak3ps (jpoce) ) ;  ALLOCATE( aksis (jpoce) )    
      ALLOCATE( aksps (jpoce) )  ;  ALLOCATE( ak12s  (jpoce) )  ;  ALLOCATE( ak12ps(jpoce) ) ;  ALLOCATE( ak123ps(jpoce) )    
      ALLOCATE( borats(jpoce) )  ;  ALLOCATE( calcon2(jpoce) )  ;  ALLOCATE( sieqs (jpoce) ) 
      ALLOCATE( aks3s(jpoce) )   ;  ALLOCATE( akf3s(jpoce) )    ;  ALLOCATE( sulfats(jpoce) )
      ALLOCATE( fluorids(jpoce) ) ; ALLOCATE( akh2s(jpoce) )    ;  ALLOCATE( aknh3(jpoce) )
      ALLOCATE( co3sat(jpoce) )

      akbs  (:) = 0. ;   ak1s   (:) = 0. ;  ak2s  (:) = 0. ;   akws   (:) = 0.
      ak1ps (:) = 0. ;   ak2ps  (:) = 0. ;  ak3ps (:) = 0. ;   aksis  (:) = 0.
      aksps (:) = 0. ;   ak12s  (:) = 0. ;  ak12ps(:) = 0. ;   ak123ps(:) = 0.
      borats(:) = 0. ;   calcon2(:) = 0. ;  sieqs (:) = 0. ;   akh2s  (:) = 0.
      aks3s(:)  = 0. ;   akf3s(:)   = 0. ;  sulfats(:) = 0. ;  fluorids(:) = 0.
      aknh3(:)  = 0. ;   co3sat(:)  = 0.

      ! Mass balance calculation  
      ALLOCATE( fromsed(jpoce, jpsol+jpads) ) ; ALLOCATE( tosed(jpoce, jpsol+jpads) )
      ALLOCATE( burial(jpoce, jpsol) )

      fromsed(:,:) = 0.    ;   tosed(:,:) = 0.
      burial(:,:) = 0.0

      ! Initialization of sediment geometry
      !------------------------------------
      CALL sed_ini_geom

      !---------------------------------------------
      ! Molecular weight [g/mol] for solid species
      !---------------------------------------------

      ! opal=sio2*0.4(h20)=28+2*16+0.4*(2+16)
      !---------------------------------------
      mol_wgt(jsopal) = 28. + 2. * 16. + 0.4 * ( 2. + 16. )

      !  clay
      !  some kind of Illit (according to Pape)
      !  K0.58(Al 1.38 Fe(III)0.37Fe(II)0.04Mg0.34)[(OH)2|(Si3.41Al0.59)O10]
      !--------------------------------------------------------------------
      mol_wgt(jsclay) = 0.58 * 39. + 1.38 * 27. + ( 0.37 + 0.04 ) * 56.+ &
         &              0.34 * 24. + 2. * ( 16. + 1. ) + 3.41 * 38. +    &
         &              0.59 * 27. + 10. * 16.

      mol_wgt(jsfeo)  = 55.0 + 3.0 * ( 16.0 + 1.0)


      mol_wgt(jsfes)  = 55.0 + 32.0

      ! for chemistry Poc : C(122)H(244)O(86)N(16)P(1)
      ! But den sity of Poc is an Hydrated material (= POC + 30H2O)
      ! So C(122)H(355)O(120)N(16)P(1)
      !------------------------------------------------------------
      mol_wgt(jspoc1:jspoc6) = ( redC * 12. + 355. + 120. * 16. + redNo3 * 14. + 31. ) / redC

      ! CaCO3
      !---------
      mol_wgt(jscal) = 40. + 12. + 3. * 16.

      ! Density of solid material in sediment [g/cm**3]
      !------------------------------------------------
      ALLOCATE( dens_sol(jpsol) )
      dens_sol(jsclay) = 2.6
      dens_sol(jscal)  = 2.7
      dens_sol(jsopal) = 2.1 
      dens_sol(jsfeo)  = 3.4
      dens_sol(jsfes)  = 4.8
      dens_sol(jspoc1:jspoc6) = 1.0

      ! Accumulation rate from Burwicz et al. (2011). This is used to
      ! compute the flux of clays and minerals
      ! --------------------------------------------------------------
      DO ji = 1, jpoce
          ztmp1 = 0.1 / ( 1.0 + ( zkbot(ji) / 200.)**3.3 )
          ztmp2 = 0.001 / ( 1.0 + ( MIN(zkbot(ji), 4500.) / 4500.)**11.4 )
          wacc(ji) = ztmp2+ztmp1
      END DO

      ! Vertical profile of of the adsorption factor for adsorbed species
      ! -----------------------------------------------------------------
      DO jk = 1, jpksed
         radssol(jk,jwfe2)  = 1.0 / ( 1.0 + adsfe2 * por1(jk) / por(jk) )
         radssol(jk,jwnh4)  = 1.0 / ( 1.0 + adsnh4 * por1(jk) / por(jk) )
         rads1sol(jk,jwnh4) = adsnh4 * por1(jk)
         rads1sol(jk,jwfe2) = adsfe2 * por1(jk)
      END DO

      xlabil(:) = 4.0 * exp(-zkbot(:) / 1000.0 ) + 1.0

      ! ---------------------------
      ! Conversion of volume units
      !----------------------------
      DO jn = 1, jpsol
         zfact = 1.0 / ( mol_wgt(jn) * 1.E-3 )
         DO jk = 1, jpksed
            DO ji = 1, jpoce
               volc(ji,jk,jn) =  vols3d(ji,jk) * zfact  / volw3d(ji,jk)
            END DO
         END DO
      END DO

      ! Compute coefficients commonly used in diffusion
      CALL sed_mat_coef

      ! Initialization of the array for non linear solving
      ! --------------------------------------------------

      ALLOCATE( jarr(jpksed*jpvode,2) )
      ALLOCATE( jsvode(jpvode), isvode(jptrased) )
      jsvode(1) = jwoxy ; jsvode(2) = jwno3 ; jsvode(3) = jwnh4
      jsvode(4) = jwh2s ; jsvode(5) = jwso4 ; jsvode(6) = jwfe2
      jsvode(7) = jpwat+jsfeo ; jsvode(8) = jpwat+jsfes
      isvode(:) = 0
      isvode(jwoxy) = 1 ; isvode(jwno3) = 2 ; isvode(jwnh4) = 3
      isvode(jwh2s) = 4 ; isvode(jwso4) = 5 ; isvode(jwfe2) = 6
      isvode(jpwat+jsfeo) = 7 ; isvode(jpwat+jsfes) = 8
      DO js = 1, jpvode
         DO jk = 1, jpksed
            jn = (jk-1) * jpvode + js
            jarr(jn,1) = jk
            jarr(jn,2) = jsvode(js)
         END DO
      END DO

      ALLOCATE( rstepros(jpoce) )

   END SUBROUTINE sed_ini

   SUBROUTINE sed_ini_geom
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_ini_geom  ***
      !!
      !! ** Purpose :  Initialization of sediment geometry
      !!               - Read the deepest water layer thickness
      !!                 ( using as mask ) in Netcdf file
      !!               - sets sediment grid, porosity and molecular weight
      !!                 and others constants
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  Original
      !!----------------------------------------------------------------------
      !! * Modules used
      !! * local declarations
      INTEGER  :: ji, jj, jk, jn
      REAL(wp) :: za0, za1, zt, zw, zsum, zsur, zprof, zprofw
      REAL(wp) :: zfact
      !---------------------------------------------------------- 

      IF(lwp) WRITE(numsed,*) ' sed_ini_geom : Initialization of sediment geometry '
      IF(lwp) WRITE(numsed,*) ' '

      ! Computation of 1D array of sediments points
      sedmask(:,:) = 0.0
      DO_2D( 0, 0, 0, 0 )
         IF (  epkbot(ji,jj) > 0. ) THEN
            sedmask(ji,jj) = 1.0
         ENDIF
      END_2D

      IF (lwp) WRITE(numsed,*) ' '
      IF (lwp) WRITE(numsed,*) ' total number of ocean points jpoce =  ',jpoce
      IF (lwp) WRITE(numsed,*) ' '

      ! initialization of dzkbot in [cm]
      !------------------------------------------------    
      dzkbot = PACK( epkbot, sedmask == 1.0 )
      dzkbot(1:jpoce) = dzkbot(1:jpoce) * 1.e+2
      zkbot   = PACK( gdepbot, sedmask == 1.0 )
      slatit  = PACK( gphit, sedmask == 1.0 )
      slongit = PACK( glamt, sedmask == 1.0 )

      ! Geometry and  constants 
      ! sediment layer thickness [cm]
      ! (1st layer= diffusive layer = pur water) 
      !------------------------------------------
      za1  = (  sedzmin - sedhmax / REAL(jpksed-1,wp)  )                                                             &
         & / ( TANH((1-sedkth)/sedacr) - sedacr/REAL(jpksed-1,wp) * (  LOG( COSH( (jpksed - sedkth) / sedacr) )      &
         &                                                           - LOG( COSH( ( 1     - sedkth) / sedacr) )  )  )
      za0  = sedzmin - za1 * TANH( (1-sedkth) / sedacr )
      zsur = - za0 - za1 * sedacr * LOG( COSH( (1-sedkth) / sedacr )  )

      dz(1)       = 0.2
      profsedw(1) = 0.0
      profsed(1)  = -dz(1) / 2.
      DO jk = 2, jpksed
         zw = REAL( jk , wp )
         zt = REAL( jk , wp ) - 0.5_wp
         profsed(jk)  = ( zsur + za0 * zt + za1 * sedacr * LOG ( COSH( (zt-sedkth) / sedacr ) )  ) 
         profsedw(jk) = ( zsur + za0 * zw + za1 * sedacr * LOG ( COSH( (zw-sedkth) / sedacr ) )  )
         dz(jk) = profsedw(jk) - profsedw(jk-1)
      END DO

      !  Porosity profile [0]
      !---------------------
      por(1) = 1.0
      xtortuosity(1) = 1.0
      DO jk = 2, jpksed
         por(jk) = porinf + ( porsurf-porinf) * exp(-rhox * profsed(jk) )
         xtortuosity(jk) = 1.0 / ( 1.0 - 2.0 * log(por(jk) ) )
      END DO
 
      ! inverse of  Porosity profile
      !-----------------------------
      por1(:) = 1. - por(:)

      ! WARNING : volw3d(:,1) and vols3d(:,1) are deepest water column volums
      vols(1:jpksed)    = dz(1:jpksed) * por1(1:jpksed)
      volw3d(1:jpoce,1) = dzkbot(1:jpoce) * por(1)
      vols3d(1:jpoce,1) = dzkbot(1:jpoce) * por1(1)
      DO jk = 2, jpksed
         volw3d(1:jpoce,jk) = dz(jk) * por (jk)
         vols3d(1:jpoce,jk) = dz(jk) * por1(jk)
      ENDDO

   END SUBROUTINE sed_ini_geom

   SUBROUTINE sed_ini_nam
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_ini_nam  ***
      !!
      !! ** Purpose :  Initialization of sediment geometry
      !!               - Reading namelist and defines constants variables
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  Original
      !!----------------------------------------------------------------------

      CHARACTER(:), ALLOCATABLE ::   numnamsed_ref   !! Character buffer for reference namelist sediment
      CHARACTER(:), ALLOCATABLE ::   numnamsed_cfg   !! Character buffer for configuration namelist sediment
      CHARACTER(LEN=20)   ::   clname
      INTEGER ::   ios   ! Local integer

      TYPE PSED
         CHARACTER(len = 20)  :: snamesed   !: short name
         CHARACTER(len = 80 ) :: lnamesed   !: long name
         CHARACTER(len = 20 ) :: unitsed    !: unit
      END TYPE PSED

      TYPE(PSED) , DIMENSION(jpsol     ) :: sedsol
      TYPE(PSED) , DIMENSION(jpwat     ) :: sedwat
      TYPE(PSED) , DIMENSION(jpdia2dsed) :: seddiag2d

      NAMELIST/nam_run/ln_sed_2way,nrosorder, rosatol, rosrtol
      NAMELIST/nam_geom/jpksed, sedzmin, sedhmax, sedkth, sedacr, porsurf, porinf, rhox
      NAMELIST/nam_trased/sedsol, sedwat
      NAMELIST/nam_diased/seddiag2d
      NAMELIST/nam_inorg/rcopal, rccal, ratligc, rcligc
      NAMELIST/nam_poc/redO2, redNo3, redPo4, redC, redfep, rcorg1,   &
         &             rcorg2, rcorg3, rcorg4, rcorg5, rcorg6,  &
         &             rcnh4, rch2s, rcfe2, rcfeh2s, rcfeso, rcfesp, &
         &             rcfesd, rcapat, xksedo2, xksedno3, xksedfeo, xksedso4
      NAMELIST/nam_btb/dbiot, ln_btbz, dbtbzsc, adsnh4, adsfe2, ln_irrig, xirrzsc
      NAMELIST/nam_rst/ln_rst_sed, nn_rstsed, cn_sedrst_indir, cn_sedrst_outdir, cn_sedrst_in, cn_sedrst_out

      INTEGER :: ji, jn, jn1
      !-------------------------------------------------------

      IF(lwp) WRITE(numsed,*) ' sed_ini_nam : Read namelists '
      IF(lwp) WRITE(numsed,*) ' '

      ! ryear = 1 year converted in second
      !------------------------------------
      IF (lwp) THEN
         WRITE(numsed,*) ' '
         WRITE(numsed,*) 'number of seconds in one year : ryear = ', ryear
         WRITE(numsed,*) ' '     
      ENDIF

      ! Reading namelist.sed variables
      !---------------------------------
      clname = 'namelist_sediment'
      IF(lwp) WRITE(numsed,*) ' sed_ini_nam : read SEDIMENT namelist'
      IF(lwp) WRITE(numsed,*) ' ~~~~~~~~~~~~~~'
      CALL load_nml( numnamsed_ref, TRIM( clname )//'_ref', numout, lwm )
      CALL load_nml( numnamsed_cfg, TRIM( clname )//'_cfg', numout, lwm )

      nitsed000 = nittrc000
      nitsedend = nitend

      ! Namelist nam_run
      READ_NML_REF(numnamsed,nam_run)
      READ_NML_CFG(numnamsed,nam_run)
      IF(lwm) WRITE( numons, nam_run )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_run'
         WRITE(numsed,*) ' 2-way coupling between PISCES and Sed ln_sed_2way = ', ln_sed_2way
         WRITE(numsed,*) ' Order of the Rosenbrock method (2,3,4) = ', nrosorder
         WRITE(numsed,*) ' Tolerance for absolute error = ', rosatol
         WRITE(numsed,*) ' Tolerance for relative order = ', rosrtol
      ENDIF

      IF ( ln_p5z .AND. ln_sed_2way ) CALL ctl_stop( '2 ways coupling with sediment cannot be activated with PISCES-QUOTA' )

      ! Namelist nam_geom 
      READ_NML_REF(numnamsed,nam_geom)
      READ_NML_CFG(numnamsed,nam_geom)
      IF(lwm) WRITE( numons, nam_geom )

      IF (lwp) THEN 
         WRITE(numsed,*) ' namelist nam_geom'
         WRITE(numsed,*) ' Number of vertical layers            jpksed  = ', jpksed
         WRITE(numsed,*) ' Minimum vertical spacing             sedzmin = ', sedzmin
         WRITE(numsed,*) ' Maximum depth of the sediment        sedhmax = ', sedhmax
         WRITE(numsed,*) ' Default parameter                    sedkth  = ', sedkth
         WRITE(numsed,*) ' Default parameter                    sedacr  = ', sedacr
         WRITE(numsed,*) ' Sediment porosity at the surface     porsurf = ', porsurf
         WRITE(numsed,*) ' Sediment porosity at infinite depth  porinf  = ', porinf
         WRITE(numsed,*) ' Length scale of porosity variation   rhox    = ', rhox
      ENDIF

      ! Namelist nam_trased
      READ_NML_REF(numnamsed,nam_trased)
      READ_NML_CFG(numnamsed,nam_trased)
      IF(lwm) WRITE( numons, nam_trased )

      DO jn = 1, jpsol
         sedtrcd(jn) = sedsol(jn)%snamesed
         sedtrcl(jn) = sedsol(jn)%lnamesed
         sedtrcu(jn) = sedsol(jn)%unitsed
      END DO

      DO jn = 1, jpwat
         jn1 = jn + jpsol
         sedtrcd(jn1) = sedwat(jn)%snamesed
         sedtrcl(jn1) = sedwat(jn)%lnamesed
         sedtrcu(jn1) = sedwat(jn)%unitsed
      END DO

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_trased'
         WRITE(numsed,*) ' '
         DO jn = 1, jptrased
            WRITE(numsed,*) 'name of 3d output sediment field number :',jn,' : ',TRIM(sedtrcd(jn))
            WRITE(numsed,*) 'long name ', TRIM(sedtrcl(jn))
            WRITE(numsed,*) ' in unit = ', TRIM(sedtrcu(jn))
            WRITE(numsed,*) ' '
         END DO
         WRITE(numsed,*) ' '
      ENDIF

      ! Namelist nam_diased
      READ_NML_REF(numnamsed,nam_diased)
      READ_NML_CFG(numnamsed,nam_diased)
      IF(lwm) WRITE( numons, nam_diased )

      DO jn = 1, jpdia2dsed
         seddia2d(jn) = seddiag2d(jn)%snamesed
         seddia2l(jn) = seddiag2d(jn)%lnamesed
         seddia2u(jn) = seddiag2d(jn)%unitsed
      END DO

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_diased'
         WRITE(numsed,*) ' '

         DO jn = 1, jpdia2dsed
            WRITE(numsed,*) 'name of 2D output diag number :',jn, ' : ', TRIM(seddia2d(jn))
            WRITE(numsed,*) 'long name ', TRIM(seddia2l(jn))
            WRITE(numsed,*) ' in unit = ',TRIM(seddia2u(jn))
            WRITE(numsed,*) ' '
         END DO

         WRITE(numsed,*) ' '
      ENDIF

      ! Inorganic chemistry parameters
      !----------------------------------
      ! Namelist nam_inorg
      READ_NML_REF(numnamsed,nam_inorg)
      READ_NML_CFG(numnamsed,nam_inorg)
      IF(lwm) WRITE( numons, nam_inorg )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_inorg'
         WRITE(numsed,*) ' reactivity for Si      rcopal  = ', rcopal
         WRITE(numsed,*) ' reactivity for calcite rccal   = ', rccal
         WRITE(numsed,*) ' L/C ratio in POC       ratligc = ', ratligc
         WRITE(numsed,*) ' reactivity for ligands rcligc  = ', rcligc
         WRITE(numsed,*) ' '
      ENDIF

      ! Unity conversion to get saturation conc. psat in [mol.l-1]
      ! and reactivity rc in  [l.mol-1.s-1]
      !----------------------------------------------------------
      reac_sil   = rcopal / ryear     
      reac_ligc  = rcligc / ryear

      ! Additional parameter linked to POC/O2/No3/Po4
      !----------------------------------------------
      ! Namelist nam_poc
      READ_NML_REF(numnamsed,nam_poc)
      READ_NML_CFG(numnamsed,nam_poc)
      IF(lwm) WRITE( numons, nam_poc )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_poc'
         WRITE(numsed,*) ' Redfield coef for oxy            redO2    = ', redO2
         WRITE(numsed,*) ' Redfield coef for no3            redNo3   = ', redNo3
         WRITE(numsed,*) ' Redfield coef for po4            redPo4   = ', redPo4
         WRITE(numsed,*) ' Redfield coef for carbon         redC     = ', redC
         WRITE(numsed,*) ' Ration for iron bound P          redfep   = ', redfep
         WRITE(numsed,*) ' reactivity for labile POC        rcorg1   = ', rcorg1
         WRITE(numsed,*) ' reactivity for labile POC        rcorg2   = ', rcorg2
         WRITE(numsed,*) ' reactivity for labile POC        rcorg3   = ', rcorg3
         WRITE(numsed,*) ' reactivity for labile POC        rcorg4   = ', rcorg4
         WRITE(numsed,*) ' reactivity for labile POC        rcorg5   = ', rcorg5
         WRITE(numsed,*) ' reactivity for labile POC        rcorg6   = ', rcorg6
         WRITE(numsed,*) ' reactivity for NH4               rcnh4    = ', rcnh4
         WRITE(numsed,*) ' reactivity for H2S               rch2s    = ', rch2s
         WRITE(numsed,*) ' reactivity for Fe2+              rcfe2    = ', rcfe2
         WRITE(numsed,*) ' reactivity for FeOH/H2S          rcfeh2s  = ', rcfeh2s
         WRITE(numsed,*) ' reactivity for FeS/O2            rcfeso   = ', rcfeso
         WRITE(numsed,*) ' Precipitation of FeS             rcfesp   = ', rcfesp
         WRITE(numsed,*) ' Dissolution of FeS               rcfesd   = ', rcfesd
         WRITE(numsed,*) ' Apatite formation rate           rcapat   = ', rcapat
         WRITE(numsed,*) ' Half-sat. cste for oxic remin    xksedo2  = ', xksedo2
         WRITE(numsed,*) ' Half-sat. cste for denit.        xksedno3 = ', xksedno3
         WRITE(numsed,*) ' Half-sat. cste for iron remin    xksedfeo = ', xksedfeo
         WRITE(numsed,*) ' Half-sat. cste for SO4 remin     xksedso4 = ', xksedso4
         WRITE(numsed,*) ' '
      ENDIF


      so2ut  = redO2    / redC
      srno3  = redNo3   / redC
      spo4r  = redPo4   / redC
      srDnit = ( (redO2 + 32. ) * 0.8 - redNo3 - redNo3 * 0.6 ) / redC
      ! reactivity rc in  [l.mol-1.s-1]
      reac_poc1  = rcorg1 / ryear
      reac_poc2  = rcorg2 / ryear
      reac_poc3  = rcorg3 / ryear
      reac_poc4  = rcorg4 / ryear
      reac_poc5  = rcorg5 / ryear
      reac_poc6  = rcorg6 / ryear
      reac_nh4   = rcnh4  / ryear
      reac_h2s   = rch2s  / ryear
      reac_fe2   = rcfe2  / ryear
      reac_feh2s = rcfeh2s/ ryear
      reac_feso  = rcfeso / ryear
      reac_fesp  = rcfesp / ryear
      reac_fesd  = rcfesd / ryear

      ! reactivity rc in  [l.mol-1.s-1]      
      reac_cal = rccal / ryear

      ! Bioturbation parameter
      !------------------------
      ! Namelist nam_btb 
      READ_NML_REF(numnamsed,nam_btb)
      READ_NML_CFG(numnamsed,nam_btb)
      IF(lwm) WRITE( numons, nam_btb )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist nam_btb ' 
         WRITE(numsed,*) ' coefficient for bioturbation      dbiot    = ', dbiot
         WRITE(numsed,*) ' Depth varying bioturbation        ln_btbz  = ', ln_btbz
         WRITE(numsed,*) ' coefficient for btb attenuation   dbtbzsc  = ', dbtbzsc
         WRITE(numsed,*) ' Adsorption coefficient of NH4     adsnh4   = ', adsnh4
         WRITE(numsed,*) ' Adsorption coefficient of Fe2     adsfe2   = ', adsfe2
         WRITE(numsed,*) ' Bioirrigation in sediment         ln_irrig = ', ln_irrig
         WRITE(numsed,*) ' coefficient for irrig attenuation xirrzsc  = ', xirrzsc
         WRITE(numsed,*) ' '
      ENDIF

      ! Initial value (t=0) for sediment pore water and solid components
      !----------------------------------------------------------------
      ! Namelist nam_rst 
      READ_NML_REF(numnamsed,nam_rst)
      READ_NML_CFG(numnamsed,nam_rst)
      IF(lwm) WRITE( numons, nam_rst )

      IF (lwp) THEN
         WRITE(numsed,*) ' namelist  nam_rst ' 
         WRITE(numsed,*) '  boolean term for restart (T or F) ln_rst_sed = ', ln_rst_sed 
         WRITE(numsed,*) ' '
      ENDIF

   END SUBROUTINE sed_ini_nam

END MODULE sedini
