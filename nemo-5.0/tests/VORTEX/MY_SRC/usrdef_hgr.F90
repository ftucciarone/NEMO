MODULE usrdef_hgr
   !!======================================================================
   !!                       ***  MODULE  usrdef_hgr  ***
   !!
   !!                      ===  VORTEX configuration  ===
   !!
   !! User defined :   mesh and Coriolis parameter of a user configuration
   !!======================================================================
   !! History :  NEMO  ! 2017-11  (J. Chanut)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_hgr    : initialize the horizontal mesh for VORTEX configuration
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE par_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE usrdef_nam, ONLY: rn_dx, rn_dy, rn_ppgphi0   ! horizontal resolution in meters
   !                                                  and reference latitude
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_hgr   ! called by domhgr.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usr_def_hgr( plamt , plamu , plamv  , plamf  ,   &   ! geographic position (required)
      &                    pphit , pphiu , pphiv  , pphif  ,   &   !
      &                    kff   , pff_f , pff_t  ,            &   ! Coriolis parameter  (if domain not on the sphere)
      &                    pe1t  , pe1u  , pe1v   , pe1f   ,   &   ! scale factors       (required)
      &                    pe2t  , pe2u  , pe2v   , pe2f   ,   &   !
      &                    ke1e2u_v      , pe1e2u , pe1e2v     )   ! u- & v-surfaces (if gridsize reduction is used in strait(s))
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE usr_def_hgr  ***
      !!
      !! ** Purpose :   user defined mesh and Coriolis parameter
      !!
      !! ** Method  :   set all intent(out) argument to a proper value
      !!                VORTEX configuration : beta-plance with uniform grid spacing (rn_dx)
      !!
      !! ** Action  : - define longitude & latitude of t-, u-, v- and f-points (in degrees) 
      !!              - define coriolis parameter at f-point if the domain in not on the sphere (on beta-plane)
      !!              - define i- & j-scale factors at t-, u-, v- and f-points (in meters)
      !!              - define u- & v-surfaces (if gridsize reduction is used in some straits) (in m2)
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   plamt, plamu, plamv, plamf   ! longitude outputs                     [degrees]
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pphit, pphiu, pphiv, pphif   ! latitude outputs                      [degrees]
      INTEGER                 , INTENT(out) ::   kff                          ! =1 Coriolis parameter computed here, =0 otherwise
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pff_f, pff_t                 ! Coriolis factor at f-point                [1/s]
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pe1t, pe1u, pe1v, pe1f       ! i-scale factors                             [m]
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pe2t, pe2u, pe2v, pe2f       ! j-scale factors                             [m]
      INTEGER                 , INTENT(out) ::   ke1e2u_v                     ! =1 u- & v-surfaces computed here, =0 otherwise 
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pe1e2u, pe1e2v               ! u- & v-surfaces (if reduction in strait)   [m2]
      !
      INTEGER  ::   ji, jj     ! dummy loop indices
      REAL(wp) ::   zbeta, zf0 ! local scalars
      REAL(wp) ::   zroffsetx, zroffsety
      REAL(wp) ::   zti, ztj   
      !!-------------------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_hgr : VORTEX configuration bassin'
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) '          Beta-plane with regular grid-spacing'
      IF(lwp) WRITE(numout,*) '          given by rn_dx and rn_dy' 
      !
      !                          
      ! Position coordinates (in kilometers)
      !                          ==========
      ! offset is given at first f-point, i.e. at (i,j) = (nn_hls+1, nn_hls+1)
      ! Here we assume the grid is centred around a T-point at the middle of
      ! of the domain (hence domain size is odd) 
      zroffsetx = (-REAL(Ni0glo-1, wp) + 1._wp) * 0.5_wp * 1.e-3 * rn_dx
      zroffsety = (-REAL(Nj0glo-1, wp) + 1._wp) * 0.5_wp * 1.e-3 * rn_dy
#if defined key_agrif
      IF( .NOT.Agrif_Root() ) THEN
         ! deduce offset from parent:
!         zroffsetx = Agrif_Parent(zroffsetx) &
!                  & + REAL(-(nbghostcells_x_w - 1) + (Agrif_Parent(nbghostcells_x_w) + Agrif_Ix()-2)*Agrif_iRhox(), wp ) * 1.e-3 * rn_dx
!         zroffsety = Agrif_Parent(zroffsety) &
!                  & + REAL(-(nbghostcells_y_s - 1) + (Agrif_Parent(nbghostcells_y_s) + Agrif_Iy()-2)*Agrif_iRhoy(), wp ) * 1.e-3 * rn_dy
! JC: More accurate positioning implied by 1:1 tests:
!     (10-13 km shift between child and parent grids  otherwise)
         zroffsetx = ((-REAL(Agrif_Parent(Ni0glo)-1, wp) + 1._wp) * 0.5_wp * Agrif_Rhox() &
                   & + REAL(-(nbghostcells_x_w - 1) + (Agrif_Parent(nbghostcells_x_w)     & 
                   & + Agrif_Ix()-2)*Agrif_iRhox(), wp )) * 1.e-3 * rn_dx
         zroffsety = ((-REAL(Agrif_Parent(Nj0glo)-1, wp) + 1._wp) * 0.5_wp * Agrif_Rhoy() &
                   & + REAL(-(nbghostcells_y_s - 1) + (Agrif_Parent(nbghostcells_y_s)     & 
                   & + Agrif_Iy()-2)*Agrif_iRhoy(), wp )) * 1.e-3 * rn_dy
      ENDIF
#endif         
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         zti = REAL( mig(ji,0)-1, wp )  ! start at i=0 in the global grid without halos
         ztj = REAL( mjg(jj,0)-1, wp )  ! start at j=0 in the global grid without halos
         
         ! Longitudes
         IF ( nn_rot==0 ) THEN
            plamt(ji,jj) = zroffsetx + rn_dx * 1.e-3 * ( zti - 0.5_wp )
            plamu(ji,jj) = plamt(ji,jj) + rn_dx * 1.e-3 * 0.5_wp 
            plamv(ji,jj) = plamt(ji,jj) 
            plamf(ji,jj) = plamu(ji,jj) 
         ELSEIF ( nn_rot==1 ) THEN
            plamt(ji,jj) = -(zroffsety + rn_dy * 1.e-3 * ( ztj - 0.5_wp ))
            plamu(ji,jj) = plamt(ji,jj)
            plamv(ji,jj) = plamt(ji,jj) - rn_dy * 1.e-3 * 0.5_wp
            plamf(ji,jj) = plamv(ji,jj) 
         ELSEIF ( nn_rot==2 ) THEN
            plamt(ji,jj) = -(zroffsetx + rn_dx * 1.e-3 * ( zti - 0.5_wp ))
            plamu(ji,jj) = plamt(ji,jj) - rn_dx * 1.e-3 * 0.5_wp 
            plamv(ji,jj) = plamt(ji,jj) 
            plamf(ji,jj) = plamu(ji,jj) 
         ELSEIF ( nn_rot==3 ) THEN
            plamt(ji,jj) = (zroffsety + rn_dy * 1.e-3 * ( ztj - 0.5_wp ))
            plamu(ji,jj) = plamt(ji,jj)
            plamv(ji,jj) = plamt(ji,jj) + rn_dy * 1.e-3 * 0.5_wp
            plamf(ji,jj) = plamv(ji,jj) 
         ENDIF          
         ! Latitudes:
         IF ( nn_rot==0 ) THEN
            pphit(ji,jj) = (zroffsety + rn_dy * 1.e-3 * ( ztj - 0.5_wp ))
            pphiv(ji,jj) = pphit(ji,jj) + rn_dy * 1.e-3 * 0.5_wp 
            pphiu(ji,jj) = pphit(ji,jj) 
            pphif(ji,jj) = pphiv(ji,jj) 
         ELSEIF ( nn_rot==1 ) THEN
            pphit(ji,jj) = (zroffsetx + rn_dx * 1.e-3 * ( zti - 0.5_wp ))
            pphiv(ji,jj) = pphit(ji,jj) 
            pphiu(ji,jj) = pphit(ji,jj) + rn_dx * 1.e-3 * 0.5_wp 
            pphif(ji,jj) = pphiu(ji,jj) 
         ELSEIF ( nn_rot==2 ) THEN
            pphit(ji,jj) = -(zroffsety + rn_dy * 1.e-3 * ( ztj - 0.5_wp ))
            pphiv(ji,jj) = pphit(ji,jj) - rn_dy * 1.e-3 * 0.5_wp 
            pphiu(ji,jj) = pphit(ji,jj) 
            pphif(ji,jj) = pphiv(ji,jj) 
         ELSEIF ( nn_rot==3 ) THEN
            pphit(ji,jj) = -(zroffsetx + rn_dx * 1.e-3 * ( zti - 0.5_wp ))
            pphiv(ji,jj) = pphit(ji,jj) 
            pphiu(ji,jj) = pphit(ji,jj) - rn_dx * 1.e-3 * 0.5_wp 
            pphif(ji,jj) = pphiu(ji,jj) 
         ENDIF
      END_2D
      !     
      ! Horizontal scale factors (in meters)
      !                              ======
      IF     ((nn_rot==0).OR.(nn_rot==2)) THEN
         pe1t(:,:) = rn_dx  ;   pe2t(:,:) = rn_dy 
         pe1u(:,:) = rn_dx  ;   pe2u(:,:) = rn_dy 
         pe1v(:,:) = rn_dx  ;   pe2v(:,:) = rn_dy 
         pe1f(:,:) = rn_dx  ;   pe2f(:,:) = rn_dy 
      ELSEIF ((nn_rot==1).OR.(nn_rot==3)) THEN
         pe1t(:,:) = rn_dy  ;   pe2t(:,:) = rn_dx 
         pe1u(:,:) = rn_dy  ;   pe2u(:,:) = rn_dx 
         pe1v(:,:) = rn_dy  ;   pe2v(:,:) = rn_dx 
         pe1f(:,:) = rn_dy  ;   pe2f(:,:) = rn_dx 
      ENDIF
      !                             ! NO reduction of grid size in some straits 
      ke1e2u_v = 0                  !    ==>> u_ & v_surfaces will be computed in dom_hgr routine
      pe1e2u(:,:) = 0._wp           !    CAUTION: set to zero to avoid error with some compilers that
      pe1e2v(:,:) = 0._wp           !             require an initialization of INTENT(out) arguments
      !
      !
      !                       !==  Coriolis parameter  ==!
      kff = 1                       !  indicate not to compute Coriolis parameter afterward
      !
      zbeta = 2._wp * omega * COS( rad * rn_ppgphi0 ) / ra
      zf0   = 2._wp * omega * SIN( rad * rn_ppgphi0 )
      pff_f(:,:) = zf0 + zbeta * pphif(:,:) * 1.e+3
      pff_t(:,:) = zf0 + zbeta * pphit(:,:) * 1.e+3
      !
   END SUBROUTINE usr_def_hgr

   !!======================================================================
END MODULE usrdef_hgr
