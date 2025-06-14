MODULE isf_oce
   !!======================================================================
   !!                       ***  MODULE  isf_oce  ***
   !! Ice shelves :  ice shelves variables defined in memory
   !!======================================================================
   !! History :  3.2  !  2011-02  (C.Harris  ) Original code isf cav
   !!            X.X  !  2006-02  (C. Wang   ) Original code bg03
   !!            3.4  !  2013-03  (P. Mathiot) Merging + parametrization
   !!            4.1  !  2019-09  (P. Mathiot) Split param/explicit ice shelf and re-organisation
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isf          : define and allocate ice shelf variables
   !!----------------------------------------------------------------------

   USE par_oce
   USE lib_mpp       , ONLY: ctl_stop, mpp_sum      ! MPP library
   USE fldread        ! read input fields

   IMPLICIT NONE

   PRIVATE

   PUBLIC   isf_alloc, isf_alloc_par, isf_alloc_cav, isf_alloc_cpl
   !
   !-------------------------------------------------------
   ! 0 :              namelist parameter
   !-------------------------------------------------------
   !
   ! 0.1 -------- ice shelf cavity parameter --------------
   CHARACTER(LEN=256), PUBLIC :: cn_isfdir
   LOGICAL           , PUBLIC :: ln_isf
   LOGICAL           , PUBLIC :: ln_isfdebug
   !
   ! 0.2 -------- ice shelf cavity opened namelist parameter -------------
   LOGICAL           , PUBLIC :: ln_isfcav_mlt   !: logical for the use of ice shelf parametrisation
   REAL(wp)          , PUBLIC :: rn_gammat0      !: temperature exchange coeficient    []
   REAL(wp)          , PUBLIC :: rn_gammas0      !: salinity    exchange coeficient    []
   REAL(wp)          , PUBLIC :: rn_htbl         !: Losch top boundary layer thickness [m]
   REAL(wp)          , PUBLIC :: rn_isfload_T    !: 
   REAL(wp)          , PUBLIC :: rn_isfload_S    !: 
   CHARACTER(LEN=256), PUBLIC :: cn_gammablk     !: gamma formulation
   CHARACTER(LEN=256), PUBLIC :: cn_isfcav_mlt   !: melt formulation (cavity/param)
   CHARACTER(LEN=256), PUBLIC :: cn_isfload      !: ice shelf load computation method
   TYPE(FLD_N)       , PUBLIC :: sn_isfcav_fwf   !: information about the isf melting file to be read
   !
   ! 0.3 -------- ice shelf cavity parametrised namelist parameter -------------
   LOGICAL           , PUBLIC :: ln_isfpar_mlt      !: logical for the computation of melt inside the cavity
   REAL(wp)          , PUBLIC :: rn_isfpar_bg03_gt0 !: temperature exchange coeficient [m/s]
   CHARACTER(LEN=256), PUBLIC :: cn_isfpar_mlt      !: melt formulation (cavity/param)
   TYPE(FLD_N)       , PUBLIC :: sn_isfpar_fwf      !: information about the isf melting file to be read
   TYPE(FLD_N)       , PUBLIC :: sn_isfpar_zmax     !: information about the grounding line depth file to be read
   TYPE(FLD_N)       , PUBLIC :: sn_isfpar_zmin     !: information about the calving   line depth file to be read
   TYPE(FLD_N)       , PUBLIC :: sn_isfpar_Leff     !: information about the effective length     file to be read
   !
   ! 0.4 -------- coupling namelist parameter -------------
   LOGICAL, PUBLIC :: ln_isfcpl      !:
   LOGICAL, PUBLIC :: ln_isfcpl_cons !:
   INTEGER, PUBLIC :: nn_drown       !:
   !
   !-------------------------------------------------------
   ! 1 :              ice shelf parameter
   !-------------------------------------------------------
   !
   REAL(wp), PARAMETER, PUBLIC :: rLfusisf = 0.334e6_wp    !: latent heat of fusion of ice shelf     [J/kg]
   REAL(wp), PARAMETER, PUBLIC :: rcpisf = 2000.0_wp       !: specific heat of ice shelf             [J/kg/K]
   REAL(wp), PARAMETER, PUBLIC :: rkappa =   0._wp         !: ISOMIP: no heat diffusivity through the ice-shelf [m2/s]
   REAL(wp), PARAMETER, PUBLIC :: rhoisf = 920.0_wp        !: volumic mass of ice shelf              [kg/m3]
   REAL(wp), PARAMETER, PUBLIC :: rtsurf = -20.0           !: surface temperature                    [C]
   !
   !-------------------------------------------------------
   ! 2 :              ice shelf global variables
   !-------------------------------------------------------
   !
   ! 2.1 -------- ice shelf cavity parameter --------------
   LOGICAL , PUBLIC            :: l_isfoasis = .FALSE.
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)    ::   risfload                    !: ice shelf load
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)    ::   fwfisf_oasis
   !
   ! 2.2 -------- ice shelf cavity melt namelist parameter -------------
   INTEGER  , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   mskisf_cav                    !:
   INTEGER  , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   misfkt_cav   , misfkb_cav     !: shallowest and deepest level of the ice shelf
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rhisf_tbl_cav, rfrac_tbl_cav  !: thickness and fraction of deepest cell affected by the ice shelf
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fwfisf_cav   , fwfisf_cav_b   !: before and now net fwf from the ice shelf        [kg/m2/s]
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   risf_cav_tsc , risf_cav_tsc_b !: before and now T & S isf contents [K.m/s & PSU.m/s]  
   TYPE(FLD), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)     ::   sf_isfcav_fwf                 !:
   !
   REAL(wp) , PUBLIC                                      ::   risf_lamb1, risf_lamb2, risf_lamb3  !: freezing point linearization coeficient
   !
   ! 2.3 -------- ice shelf param. melt namelist parameter -------------
   INTEGER  , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   mskisf_par                    !:
   INTEGER  , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   misfkt_par   , misfkb_par     !:
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rhisf_tbl_par, rfrac_tbl_par  !: 
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fwfisf_par   , fwfisf_par_b   !: before and now net fwf from the ice shelf        [kg/m2/s]
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   risf_par_tsc , risf_par_tsc_b !: before and now T & S isf contents [K.m/s & PSU.m/s]  
   TYPE(FLD), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)     ::   sf_isfpar_fwf                 !:
   !
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rhisf0_tbl_par                !: thickness of tbl (initial value)  [m]
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   risfLeff                      !:
   !
   ! 2.4 -------- coupling namelist parameter -------------
   INTEGER , PUBLIC                                        ::   nstp_iscpl   !:
   REAL(wp), PUBLIC                                        ::   rdt_iscpl    !: 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   risfcpl_ssh, risfcpl_cons_ssh  !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   risfcpl_vol, risfcpl_cons_vol  !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   risfcpl_tsc, risfcpl_cons_tsc  !:
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE isf_alloc_par()
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_alloc_par  ***
      !!
      !! ** Purpose : 
      !!
      !! ** Method  : 
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ialloc
      !!----------------------------------------------------------------------
      ialloc = 0       ! set to zero if no array to be allocated
      ! -------------------- !
      ! == REDUCED ARRAYS == !
      ! -------------------- !
      ALLOCATE( misfkt_par   (A2D(1)) , misfkb_par    (A2D(1)) , rfrac_tbl_par(A2D(1)) , &
         &      rhisf_tbl_par(A2D(1)) , rhisf0_tbl_par(A2D(1)) ,                         &
         &      risfLeff     (A2D(0)) , mskisf_par(A2D(0))     , STAT=ialloc             )
      !
      CALL mpp_sum ( 'isf', ialloc )
      IF( ialloc /= 0 )   CALL ctl_stop( 'STOP', 'isf: failed to allocate arrays.' )
      !
   END SUBROUTINE isf_alloc_par

   
   SUBROUTINE isf_alloc_cav()
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_alloc_cav  ***
      !!
      !! ** Purpose : 
      !!
      !! ** Method  : 
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ialloc
      !!----------------------------------------------------------------------
      ialloc = 0       ! set to zero if no array to be allocated
      ! -------------------- !
      ! == REDUCED ARRAYS == !
      ! -------------------- !
      ALLOCATE( misfkt_cav   (A2D(1)) , misfkb_cav(A2D(1)) , rfrac_tbl_cav(A2D(1)) , &
         &      rhisf_tbl_cav(A2D(1)) , mskisf_cav(A2D(1)) , STAT=ialloc             )
      !
      CALL mpp_sum ( 'isf', ialloc )
      IF( ialloc /= 0 )   CALL ctl_stop( 'STOP', 'isf: failed to allocate arrays.' )
      !
   END SUBROUTINE isf_alloc_cav

   
   SUBROUTINE isf_alloc_cpl()
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_alloc_cpl  ***
      !!
      !! ** Purpose : allocate array use for the ice sheet coupling
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ierr, ialloc
      !!----------------------------------------------------------------------
      ierr = 0
      ! ----------------- !
      ! == FULL ARRAYS == !
      ! ----------------- !
      ALLOCATE( risfcpl_ssh     (jpi,jpj) , risfcpl_vol     (jpi,jpj,jpk) , &
         &      risfcpl_cons_ssh(jpi,jpj) , risfcpl_cons_vol(jpi,jpj,jpk) , STAT=ialloc )
      ierr = ierr + ialloc
      ! -------------------- !
      ! == REDUCED ARRAYS == !
      ! -------------------- !
      ALLOCATE( risfcpl_tsc     (A2D(0),jpk,jpts) , &
         &      risfcpl_cons_tsc(A2D(0),jpk,jpts) , STAT=ialloc )
      ierr = ierr + ialloc
      !
      CALL mpp_sum ( 'isf', ierr )
      IF( ierr /= 0 )   CALL ctl_stop('STOP','isfcpl: failed to allocate arrays.')
      !
   END SUBROUTINE isf_alloc_cpl

     
   SUBROUTINE isf_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_alloc  ***
      !!
      !! ** Purpose : allocate array used for the ice shelf cavity (cav and par)
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ierr, ialloc
      !!----------------------------------------------------------------------
      !
      ierr = 0       ! set to zero if no array to be allocated
      !
      ! ----------------- !
      ! == FULL ARRAYS == !
      ! ----------------- !
      ALLOCATE( fwfisf_par  (jpi,jpj) , fwfisf_cav  (jpi,jpj) , risfload(jpi,jpj) , &
#if ! defined key_RK3
         &      fwfisf_par_b(jpi,jpj) , fwfisf_cav_b(jpi,jpj) , &   ! MLF : need to allocate before arrays
#endif
         &                                                      STAT=ialloc )
      ierr = ierr + ialloc
      !
      ! -------------------- !
      ! == REDUCED ARRAYS == !
      ! -------------------- !
      ALLOCATE( fwfisf_oasis(A2D(0)) , risf_par_tsc  (A2D(0),jpts) , risf_cav_tsc  (A2D(0),jpts) , &
#if ! defined key_RK3
         &                             risf_par_tsc_b(A2D(0),jpts) , risf_cav_tsc_b(A2D(0),jpts) , &  ! MLF : need to allocate before arrays
#endif
         &                                                                  STAT=ialloc )
      ierr = ierr + ialloc
      !
      CALL mpp_sum ( 'isf', ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'isf: failed to allocate arrays.' )
      !
   END SUBROUTINE isf_alloc
   
   !!======================================================================
END MODULE isf_oce
