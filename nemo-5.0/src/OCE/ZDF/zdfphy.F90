MODULE zdfphy
   !!======================================================================
   !!                      ***  MODULE  zdfphy  ***
   !! Vertical ocean physics :   manager of all vertical physics packages
   !!======================================================================
   !! History :  4.0  !  2017-04  (G. Madec)  original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_phy_init  : initialization of all vertical physics packages
   !!   zdf_phy       : upadate at each time-step the vertical mixing coeff.
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE zdf_oce        ! vertical physics: shared variables
   USE zdfdrg         ! vertical physics: top/bottom drag coef.
   USE zdfsh2         ! vertical physics: shear production term of TKE
   USE zdfric         ! vertical physics: RIChardson dependent vertical mixing
   USE zdftke         ! vertical physics: TKE vertical mixing
   USE zdfgls         ! vertical physics: GLS vertical mixing
   USE zdfosm         ! vertical physics: OSMOSIS vertical mixing
   USE zdfddm         ! vertical physics: double diffusion mixing
   USE zdfevd         ! vertical physics: convection via enhanced vertical diffusion
   USE zdfmfc         ! vertical physics: Mass Flux Convection
   USE zdfiwm         ! vertical physics: internal wave-induced mixing
   USE zdfswm         ! vertical physics: surface  wave-induced mixing
   USE zdfmxl         ! vertical physics: mixed layer
   USE tranpc         ! convection: non penetrative adjustment
   USE trc_oce        ! variables shared between passive tracer & ocean
   USE sbc_oce        ! surface module (only for nn_isf in the option compatibility test)
   USE sbcrnf         ! surface boundary condition: runoff variables
   USE domtile
#if defined key_agrif
   USE agrif_oce_interp   ! interpavm
#endif
   !
   USE in_out_manager ! I/O manager
   USE iom            ! IOM library
   USE lbclnk         ! lateral boundary conditions
   USE lib_mpp        ! distribued memory computing
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_phy_init  ! called by nemogcm.F90
   PUBLIC   zdf_phy       ! called by step.F90

   INTEGER ::   nzdf_phy   ! type of vertical closure used
   !                       ! associated indicators
   INTEGER, PARAMETER ::   np_CST = 1   ! Constant Kz
   INTEGER, PARAMETER ::   np_RIC = 2   ! Richardson number dependent Kz
   INTEGER, PARAMETER ::   np_TKE = 3   ! Turbulente Kinetic Eenergy closure scheme for Kz
   INTEGER, PARAMETER ::   np_GLS = 4   ! Generic Length Scale closure scheme for Kz
   INTEGER, PARAMETER ::   np_OSM = 5   ! OSMOSIS-OBL closure scheme for Kz

   LOGICAL, PUBLIC ::   l_zdfsh2   ! shear production term flag (=F for CST, =T otherwise (i.e. TKE, GLS, RIC))

#if ! defined key_PSYCLONE_2p5p0
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   sh2 !: Shear term
#endif

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE zdf_phy_init( Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_phy_init  ***
      !!
      !! ** Purpose :   initializations of the vertical ocean physics
      !!
      !! ** Method  :   Read namelist namzdf, control logicals
      !!                set horizontal shape and vertical profile of background mixing coef.
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in)    :: Kmm ! time level index (middle)
      !
      INTEGER ::   jk            ! dummy loop indices
      INTEGER ::   ioptio, ios   ! local integers
      !!
      NAMELIST/namzdf/ ln_zdfcst, ln_zdfric, ln_zdftke, ln_zdfgls,   &     ! type of closure scheme
         &             ln_zdfosm,                                    &     ! type of closure scheme
         &             ln_zdfmfc,                                    &     ! convection : mass flux
         &             ln_zdfevd, nn_evdm, rn_evd ,                  &     ! convection : evd
         &             ln_zdfnpc, nn_npc , nn_npcp,                  &     ! convection : npc
         &             ln_zdfddm, rn_avts, rn_hsbfr,                 &     ! double diffusion
         &             ln_zdfswm,                                    &     ! surface  wave-induced mixing
         &             ln_zdfiwm,                                    &     ! internal  -      -      -
         &             ln_zad_Aimp,                                  &     ! apdative-implicit vertical advection
         &             rn_avm0, rn_avt0, nn_avb, nn_havtb                  ! coefficients
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_phy_init: ocean vertical physics'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      !                           !==  Namelist  ==!
      READ_NML_REF(numnam,namzdf)
      READ_NML_CFG(numnam,namzdf)
      IF(lwm)   WRITE ( numond, namzdf )
      !
      IF(lwp) THEN                      ! Parameter print
         WRITE(numout,*) '   Namelist namzdf : set vertical mixing mixing parameters'
         WRITE(numout,*) '      adaptive-implicit vertical advection'
         WRITE(numout,*) '         Courant number targeted application   ln_zad_Aimp = ', ln_zad_Aimp
         WRITE(numout,*) '      vertical closure scheme'
         WRITE(numout,*) '         constant vertical mixing coefficient    ln_zdfcst = ', ln_zdfcst
         WRITE(numout,*) '         Richardson number dependent closure     ln_zdfric = ', ln_zdfric
         WRITE(numout,*) '         Turbulent Kinetic Energy closure (TKE)  ln_zdftke = ', ln_zdftke
         WRITE(numout,*) '         Generic Length Scale closure (GLS)      ln_zdfgls = ', ln_zdfgls
         WRITE(numout,*) '         OSMOSIS-OBL closure (OSM)               ln_zdfosm = ', ln_zdfosm
         WRITE(numout,*) '      convection: '
         WRITE(numout,*) '         convection mass flux (mfc)              ln_zdfmfc = ', ln_zdfmfc
         WRITE(numout,*) '         enhanced vertical diffusion             ln_zdfevd = ', ln_zdfevd
         WRITE(numout,*) '            applied on momentum (=1/0)             nn_evdm = ', nn_evdm
         WRITE(numout,*) '            vertical coefficient for evd           rn_evd  = ', rn_evd
         WRITE(numout,*) '         non-penetrative convection (npc)        ln_zdfnpc = ', ln_zdfnpc
         WRITE(numout,*) '            npc call  frequency                    nn_npc  = ', nn_npc
         WRITE(numout,*) '            npc print frequency                    nn_npcp = ', nn_npcp
         WRITE(numout,*) '      double diffusive mixing                    ln_zdfddm = ', ln_zdfddm
         WRITE(numout,*) '         maximum avs for dd mixing                 rn_avts = ', rn_avts
         WRITE(numout,*) '         heat/salt buoyancy flux ratio             rn_hsbfr= ', rn_hsbfr
         WRITE(numout,*) '      gravity wave-induced mixing'
         WRITE(numout,*) '         surface  wave (Qiao et al 2010)         ln_zdfswm = ', ln_zdfswm                                          ! surface wave induced mixing
         WRITE(numout,*) '         internal wave (de Lavergne et al 2017)  ln_zdfiwm = ', ln_zdfiwm
         WRITE(numout,*) '      coefficients : '
         WRITE(numout,*) '         vertical eddy viscosity                 rn_avm0   = ', rn_avm0
         WRITE(numout,*) '         vertical eddy diffusivity               rn_avt0   = ', rn_avt0
         WRITE(numout,*) '         constant background or profile          nn_avb    = ', nn_avb
         WRITE(numout,*) '         horizontal variation for avtb           nn_havtb  = ', nn_havtb
      ENDIF

      IF( ln_zad_Aimp ) THEN
         IF( zdf_phy_alloc() /= 0 )   &
            &       CALL ctl_stop( 'STOP', 'zdf_phy_init : unable to allocate adaptive-implicit z-advection arrays' )
         Cu_adv(:,:,:) = 0._wp
         wi    (:,:,:) = 0._wp
      ENDIF
      !                                  ! Initialise zdf_mxl arrays
      IF( zdf_mxl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_mxl : unable to allocate arrays' )
      ! Initialize halo for diagnostics (only updated on internal points)
      hmlp(:,:) = 0._wp
      hmld(:,:) = 0._wp
      !                          !==  Background eddy viscosity and diffusivity  ==!
      IF( nn_avb == 0 ) THEN             ! Define avmb, avtb from namelist parameter
         avmb(:) = rn_avm0
         avtb(:) = rn_avt0
      ELSE                               ! Background profile of avt (fit a theoretical/observational profile (Krauss 1990)
         avmb(:) = rn_avm0
         avtb(:) = rn_avt0 + ( 3.e-4_wp - 2._wp * rn_avt0 ) * 1.e-4_wp * gdepw_1d(:)   ! m2/s
         IF(l_sco .AND. lwp)   CALL ctl_warn( 'avtb profile not valid in sco' )
      ENDIF
      !                                  ! 2D shape of the avtb
      avtb_2d(:,:) = 1._wp                   ! uniform
      !
      IF( nn_havtb == 1 ) THEN               ! decrease avtb by a factor of ten in the equatorial band
           !                                 !   -15S -5S : linear decrease from avt0 to avt0/10.
           !                                 !   -5S  +5N : cst value avt0/10.
           !                                 !    5N  15N : linear increase from avt0/10, to avt0
           WHERE(-15. <= gphit(A2D(0)) .AND. gphit(A2D(0)) < -5 )   avtb_2d(:,:) = (1.  - 0.09 * (gphit(A2D(0)) + 15.))
           WHERE( -5. <= gphit(A2D(0)) .AND. gphit(A2D(0)) <  5 )   avtb_2d(:,:) =  0.1
           WHERE(  5. <= gphit(A2D(0)) .AND. gphit(A2D(0)) < 15 )   avtb_2d(:,:) = (0.1 + 0.09 * (gphit(A2D(0)) -  5.))
      ENDIF
      !
      DO jk = 1, jpk                      ! set turbulent closure Kz to the background value (avt_k, avm_k)
         avt_k(:,:,jk)    = avtb_2d(:,:) * avtb(jk) * wmask(A2D(0),jk)
         avm_k(A2D(1),jk) =                avmb(jk) * wmask(A2D(1),jk)
      END DO
!!gm  to be tested only the 1st & last levels
!      avt  (:,:, 1 ) = 0._wp   ;   avs(:,:, 1 ) = 0._wp   ;   avm  (:,:, 1 ) = 0._wp
!      avt  (:,:,jpk) = 0._wp   ;   avs(:,:,jpk) = 0._wp   ;   avm  (:,:,jpk) = 0._wp
!!gm
      avt  (:,:,:) = 0._wp   ;   avs(:,:,:) = 0._wp   ;   avm  (:,:,:) = 0._wp

      !                          !==  Convection  ==!
      !
      IF( ln_zdfnpc .AND. ln_zdfevd )   CALL ctl_stop( 'zdf_phy_init: chose between ln_zdfnpc and ln_zdfevd' )
      IF( ln_zdfosm .AND. ln_zdfevd )   CALL ctl_stop( 'zdf_phy_init: chose between ln_zdfosm and ln_zdfevd' )
      IF( ln_zdfmfc .AND. ln_zdfevd )   CALL ctl_stop( 'zdf_phy_init: chose between ln_zdfmfc and ln_zdfevd' )
      IF( ln_zdfmfc .AND. ln_zdfnpc )   CALL ctl_stop( 'zdf_phy_init: chose between ln_zdfmfc and ln_zdfnpc' )
      IF( ln_zdfmfc .AND. ln_zdfosm )   CALL ctl_stop( 'zdf_phy_init: chose between ln_zdfmfc and ln_zdfosm' )
      IF( lk_top    .AND. ln_zdfnpc )   CALL ctl_stop( 'zdf_phy_init: npc scheme is not working with key_top' )
      IF( lk_top    .AND. ln_zdfosm )   CALL ctl_warn( 'zdf_phy_init: osmosis gives no non-local fluxes for TOP tracers yet' )
      IF( lk_top    .AND. ln_zdfmfc )   CALL ctl_stop( 'zdf_phy_init: Mass Flux scheme is not working with key_top' )
      IF(lwp) THEN
         WRITE(numout,*)
         IF    ( ln_zdfnpc ) THEN  ;   WRITE(numout,*) '   ==>>>   convection: use non penetrative convective scheme'
         ELSEIF( ln_zdfevd ) THEN  ;   WRITE(numout,*) '   ==>>>   convection: use enhanced vertical diffusion scheme'
         ELSEIF( ln_zdfmfc ) THEN  ;   WRITE(numout,*) '   ==>>>   convection: use Mass Flux scheme'
         ELSE                      ;   WRITE(numout,*) '   ==>>>   convection: no specific scheme used'
         ENDIF
      ENDIF

      IF(lwp) THEN               !==  Double Diffusion Mixing parameterization  ==!   (ddm)
         WRITE(numout,*)
         IF( ln_zdfddm ) THEN   ;   WRITE(numout,*) '   ==>>>   use double diffusive mixing: avs /= avt'
         ELSE                   ;   WRITE(numout,*) '   ==>>>   No  double diffusive mixing: avs = avt'
         ENDIF
      ENDIF

      !                          !==  type of vertical turbulent closure  ==!    (set nzdf_phy)
      ioptio = 0
      IF( ln_zdfcst ) THEN   ;   ioptio = ioptio + 1   ;    nzdf_phy = np_CST   ;   ENDIF
      IF( ln_zdfric ) THEN   ;   ioptio = ioptio + 1   ;    nzdf_phy = np_RIC   ;   CALL zdf_ric_init          ;   ENDIF
      IF( ln_zdftke ) THEN   ;   ioptio = ioptio + 1   ;    nzdf_phy = np_TKE   ;   CALL zdf_tke_init          ;   ENDIF
      IF( ln_zdfgls ) THEN   ;   ioptio = ioptio + 1   ;    nzdf_phy = np_GLS   ;   CALL zdf_gls_init          ;   ENDIF
      IF( ln_zdfosm ) THEN   ;   ioptio = ioptio + 1   ;    nzdf_phy = np_OSM   ;   CALL zdf_osm_init( Kmm )   ;   ENDIF
      !
      IF( ioptio /= 1 )    CALL ctl_stop( 'zdf_phy_init: one and only one vertical diffusion option has to be defined ' )
      IF( ln_isfcav ) THEN
         IF( ln_zdfric ) CALL ctl_stop( 'zdf_phy_init: zdfric never tested with ice shelves cavities ' )
         IF( ln_zdfosm ) CALL ctl_stop( 'zdf_phy_init: the combination of ice-shelf cavities and the OSMOSIS scheme is not supported' )
      ENDIF
      !                                ! shear production term flag
      IF( ln_zdfcst .OR. ln_zdfosm ) THEN   ;   l_zdfsh2 = .FALSE.
      ELSE                                  ;   l_zdfsh2 = .TRUE.
      ENDIF
      !                          !== Mass Flux Convectiive algorithm  ==!
      IF( ln_zdfmfc )   CALL zdf_mfc_init       ! Convection computed with eddy diffusivity mass flux
      !
      !                          !== gravity wave-driven mixing  ==!
      IF( ln_zdfiwm )   CALL zdf_iwm_init       ! internal wave-driven mixing
      IF( ln_zdfswm )   CALL zdf_swm_init       ! surface  wave-driven mixing

      !                          !== top/bottom friction  ==!
      CALL zdf_drg_init
      !
      !                          !== time-stepping  ==!
      ! Check/update of time stepping done in dynzdf_init/trazdf_init
      !!gm move it here ?
      !
   END SUBROUTINE zdf_phy_init


   SUBROUTINE zdf_phy( kt, Kbb, Kmm, Krhs )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE zdf_phy  ***
      !!
      !! ** Purpose :  Update ocean physics at each time-step
      !!
      !! ** Method  :
      !!
      !! ** Action  :   avm, avt vertical eddy viscosity and diffusivity at w-points
      !!                nmld ??? mixed layer depth in level and meters   <<<<====verifier !
      !!                bottom stress.....                               <<<<====verifier !
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt         ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs   ! ocean time level indices
      !
      INTEGER ::   ji, jj, jk, jtile   ! dummy loop indice
#if defined key_PSYCLONE_2p5p0
      REAL(wp), DIMENSION(A2D(0),jpk) ::   sh2   ! shear term
#endif
      !! ---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('zdf_phy')
      !
      IF( l_zdfsh2 ) THEN        !* shear production at w-points (energy conserving form)
#if ! defined key_PSYCLONE_2p5p0
         ALLOCATE( sh2(A2D(0),jpk) )
#endif
         CALL zdf_sh2( Kbb, Kmm, avm_k,   &     ! <<== in
            &                      sh2    )     ! ==>> out : shear production
      ENDIF

      IF( ln_tile ) CALL dom_tile_start         ! [tiling] ZDF tiling loop
      DO jtile = 1, nijtile
         IF( ln_tile ) CALL dom_tile( ntsi, ntsj, ntei, ntej, ktile = jtile )
         !
         ! Update top/bottom drag
                                            CALL zdf_drg( kt, Kmm, 'BOTTOM' )  ! bottom drag
         IF( ln_isfcav .OR. ln_drgice_imp ) CALL zdf_drg( kt, Kmm, 'TOP'    )  ! top drag (ocean cavities)
         !
         CALL zdf_mxl( kt, Kmm )                        !* mixed layer depth, and level
         !
         !                       !==  Kz from chosen turbulent closure  ==!   (avm_k, avt_k)
         !
         SELECT CASE ( nzdf_phy )                  !* Vertical eddy viscosity and diffusivity coefficients at w-points
            CASE( np_RIC )   ;   CALL zdf_ric( kt, Kbb, Kmm, avm_k, avt_k )    ! Richardson number dependent Kz
            CASE( np_TKE )   ;   CALL zdf_tke( kt, Kbb, Kmm, sh2, avm_k, avt_k )    ! TKE closure scheme for Kz
            CASE( np_GLS )   ;   CALL zdf_gls( kt, Kbb, Kmm, sh2, avm_k, avt_k )    ! GLS closure scheme for Kz
            CASE( np_OSM )   ;   CALL zdf_osm( kt, Kbb, Kmm, Krhs, avm_k, avt_k )   ! OSMOSIS closure scheme for Kz
      !     CASE( np_CST )                                  ! Constant Kz (reset avt, avm to the background value)
      !         ! avt_k and avm_k set one for all at initialisation phase
!!gm            avt(2:jpim1,2:jpjm1,1:jpkm1) = rn_avt0 * wmask(2:jpim1,2:jpjm1,1:jpkm1)
!!gm            avm(2:jpim1,2:jpjm1,1:jpkm1) = rn_avm0 * wmask(2:jpim1,2:jpjm1,1:jpkm1)
         END SELECT
         !
#if defined key_agrif
      END DO
      IF( ln_tile ) CALL dom_tile_stop
      !
      !                          !==  ocean Kz  ==!   (avt, avs, avm)
      !
      ! interpolation parent grid => child grid for avm_k ( ex : at west border: update column 1 and 2)
      IF( l_zdfsh2 .AND. .NOT. Agrif_Root() )   CALL Agrif_avm

      IF( ln_tile ) CALL dom_tile_start         ! [tiling] ZDF tiling loop (continued)
      DO jtile = 1, nijtile
         IF( ln_tile ) CALL dom_tile( ntsi, ntsj, ntei, ntej, ktile = jtile )
#else
         !                          !==  ocean Kz  ==!   (avt, avs, avm)
#endif
         !
         !                                         !* start from turbulent closure values
         DO_3D( 0, 0, 0, 0, 2, jpkm1 )
            avt(ji,jj,jk) = avt_k(ji,jj,jk)
            avm(ji,jj,jk) = avm_k(ji,jj,jk)
         END_3D
         !
         IF( ln_rnf_mouth ) THEN                   !* increase diffusivity at rivers mouths
            DO_3D( 0, 0, 0, 0, 2, nkrnf )
               avt(ji,jj,jk) = avt(ji,jj,jk) + 2._wp * rn_avt_rnf * rnfmsk(ji,jj) * wmask(ji,jj,jk)
            END_3D
         ENDIF
         !
         IF( ln_zdfevd )   CALL zdf_evd( kt, Kmm, Krhs, avm, avt )  !* convection: enhanced vertical eddy diffusivity
         !
         !                                         !* double diffusive mixing
         IF( ln_zdfddm ) THEN                            ! update avt and compute avs
                           CALL zdf_ddm( kt, Kmm,  avm, avt, avs )
         ELSE                                            ! same mixing on all tracers
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               avs(ji,jj,jk) = avt(ji,jj,jk)
            END_3D
         ENDIF
         !
         !                                         !* wave-induced mixing
         IF( ln_zdfswm )   CALL zdf_swm( kt, Kmm, avm, avt, avs )   ! surface  wave (Qiao et al. 2004)
         IF( ln_zdfiwm )   CALL zdf_iwm( kt, Kmm, avm, avt, avs )   ! internal wave (de Lavergne et al 2017)
         !
         CALL zdf_mxl_turb( kt, Kmm )                   !* turbocline depth
      END DO
      IF( ln_tile ) CALL dom_tile_stop
      !
      !                                         !* Lateral boundary conditions (sign unchanged)
      ! Subroutines requiring halo points: zdf_sh2 (avm_k), dyn_zdf (avm)
      CALL lbc_lnk( 'zdfphy', avm, 'W', 1.0_wp )

      IF( lrst_oce ) THEN                       !* write TKE, GLS or RIC fields in the restart file
         IF( ln_zdftke )   CALL tke_rst( kt, 'WRITE' )
         IF( ln_zdfgls )   CALL gls_rst( kt, 'WRITE' )
         IF( ln_zdfric )   CALL ric_rst( kt, 'WRITE' )
         ! NB. OSMOSIS restart (osm_rst) will be called in step.F90 after ww has been updated
      ENDIF
      !
      ! diagnostics of energy dissipation
      IF( l_zdfsh2 ) THEN
         IF( iom_use('eshear_k') ) THEN
            DO_3D( 0, 0, 0, 0, 2, jpkm1 )
               sh2(ji,jj,jk) = sh2(ji,jj,jk) * wmask(ji,jj,jk)
            END_3D
         ENDIF
         IF( iom_use('avt_k'   ) ) CALL iom_put( 'avt_k'   ,   avt_k                 * wmask(A2D(0),:) )
         IF( iom_use('avm_k'   ) ) CALL iom_put( 'avm_k'   ,   avm_k(A2D(0),:)       * wmask(A2D(0),:) )
         IF( iom_use('estrat_k') ) CALL iom_put( 'estrat_k', - avt_k * rn2(A2D(0),:) * wmask(A2D(0),:) )
         IF( iom_use('eshear_k') ) CALL iom_put( 'eshear_k',   sh2                                     )
#if ! defined key_PSYCLONE_2p5p0
         DEALLOCATE( sh2 )
#endif
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('zdf_phy')
      !
   END SUBROUTINE zdf_phy


   INTEGER FUNCTION zdf_phy_alloc()
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION zdf_phy_alloc  ***
      !!----------------------------------------------------------------------
      ! Allocate wi array (declared in oce.F90) for use with the adaptive-implicit vertical velocity option
      ALLOCATE(     wi(jpi,jpj,jpk), Cu_adv(jpi,jpj,jpk),  STAT= zdf_phy_alloc )
      IF( zdf_phy_alloc /= 0 )   CALL ctl_warn('zdf_phy_alloc: failed to allocate ln_zad_Aimp=T required arrays')
      CALL mpp_sum ( 'zdfphy', zdf_phy_alloc )
   END FUNCTION zdf_phy_alloc

   !!======================================================================
END MODULE zdfphy
