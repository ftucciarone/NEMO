MODULE isfpar
   !!======================================================================
   !!                       ***  MODULE  isfpar  ***
   !! ice shelf module :  update ocean boundary condition under ice
   !!                   shelf
   !!======================================================================
   !! History :  3.2  !  2011-02  (C.Harris  ) Original code isf cav
   !!            X.X  !  2006-02  (C. Wang   ) Original code bg03
   !!            3.4  !  2013-03  (P. Mathiot) Merging + parametrization
   !!            4.1  !  2019-09  (P. Mathiot) Restructuration
   !!            4.2  !  2021-05  (C. Ethe   ) Test and fix oasis case
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfpar       : compute ice shelf melt using a prametrisation of ice shelf cavities
   !!----------------------------------------------------------------------
   USE isf_oce        ! ice shelf
   !
   USE isfrst   , ONLY: isfrst_read               ! ice shelf restart read/write subroutine
   USE isftbl                                     ! ice shelf top boundary layer properties subroutine
   USE isfparmlt, ONLY: isfpar_mlt                ! ice shelf melt formulation subroutine
   USE isfdiags , ONLY: isf_diags_flx             ! ice shelf diags subroutine
   USE isfutils , ONLY: debug, read_2dcstdta      ! ice shelf debug subroutine
   !
   USE oce      , ONLY: ts             ! ocean dynamics and tracers
   USE dom_oce                         ! ocean space and time domain
   USE par_oce                         ! ocean space and time domain
   USE phycst                          ! physical constants
   USE eosbn2   , ONLY: eos_fzp        ! equation of state
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O library
   USE fldread        ! read input field at current time step

   IMPLICIT NONE
   PRIVATE

   PUBLIC   isf_par, isf_par_init

   !! * Substitutions   
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
 
   SUBROUTINE isf_par( kt, Kmm, ptsc, pfwf )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE isf_par ***      
      !!
      !! ** Purpose : compute the heat and fresh water due to ice shelf melting/freezing using a parametrisation 
      !!
      !! ** Comment : in isf_par and all its call tree, 
      !!              'tbl' means parametrisation layer (ie how the far field temperature/salinity is computed) 
      !!              instead of in a proper top boundary layer as at the ice shelf ocean interface
      !!              as the action to compute the properties of the tbl or the parametrisation layer are the same,
      !!              (ie average T/S over a specific depth (can be across multiple levels))
      !!              the name tbl was kept.
      !!
      !! ** Convention : all fluxes are from isf to oce
      !!
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt                                     ! ocean time step
      INTEGER, INTENT(in) ::   Kmm                                    ! ocean time level index
      REAL(wp), DIMENSION(A2D(0))     , INTENT(inout) ::   pfwf
      REAL(wp), DIMENSION(A2D(0),jpts), INTENT(inout) ::   ptsc
      !!
      INTEGER ::   ji, jj, jk
      REAL(wp), DIMENSION(A2D(0)) ::   ztfrz, ztavg                   ! tbl freezing and averaged temperatures
      REAL(wp), DIMENSION(A2D(0)) ::   zqoce, zqhc, zqlat
      REAL(wp), DIMENSION(A2D(0),jpk) ::   ztfrz3d, ztmp
      !!---------------------------------------------------------------------
      !
      ! Mean freezing point
      CALL eos_fzp( ts, Kmm, ztfrz3d, 0 )
!!st      DO_3D( 0 ,0, 0, 0, 1, jpk )
!!st         ztmp(ji,jj,jk) = gdept(ji,jj,jk,Kmm)
!!st      END_3D
!!st      CALL eos_fzp( ts(A2D(0),:,jp_sal,Kmm), ztfrz3d(:,:,:), ztmp )
      !
      DO_3D( 0 ,0, 0, 0, 1, jpk )
         ztmp(ji,jj,jk) = e3t  (ji,jj,jk,Kmm)
      END_3D
      CALL isf_tbl_avg( misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, ztmp, ztfrz3d, & ! <<== in
         &                                                                            ztfrz  ) ! ==>> out

      ! Mean temperature (only for bg03)
      CALL isf_tbl_avg( misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, ztmp, ts(:,:,:,jp_tem,Kmm), & ! <<== in
         &                                                                                         ztavg  ) ! ==>> out

      ! compute heat content, latent heat and melt fluxes (2d)
      CALL isfpar_mlt( kt, Kmm, ztfrz, ztavg, zqhc, zqoce, pfwf )
      !
      DO_2D( 0, 0, 0, 0 )
         ! compute latent heat flux (from isf to oce)
         zqlat(ji,jj) = - pfwf(ji,jj) * rLfusisf    ! 2d latent heat flux (W/m2)
         !
         ! set temperature content
         ptsc(ji,jj,jp_tem) = ( zqhc (ji,jj) + zqoce(ji,jj) ) * r1_rho0_rcp
      END_2D
      !
      ! output fluxes
      CALL isf_diags_flx( Kmm, misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, 'par', pfwf, zqoce, zqlat, zqhc )
      !
      ! outputs
      CALL iom_put('isftfrz_par', ztfrz(:,:) * mskisf_par(:,:) ) ! freezing temperature
      IF( cn_isfpar_mlt == 'bg03' ) THEN
         CALL iom_put('ttbl_par',         ztavg(:,:)                * mskisf_par(:,:) )      ! ttbl
         CALL iom_put('isfthermald_par',( ztavg(:,:) - ztfrz(:,:) ) * mskisf_par(:,:) )      ! thermal driving
      ENDIF
      !
      ! debugs
      IF ( ln_isfdebug ) THEN
         IF(lwp) WRITE(numout,*)
         CALL debug('isf_par: ptsc T', ptsc (:,:,1))
         CALL debug('isf_par: ptsc S', ptsc (:,:,2))
         CALL debug( 'isfpar: qhc   ', zqhc (:,:)  )
         CALL debug( 'isfpar: qoce  ', zqoce(:,:)  )
         CALL debug( 'isfpar: fwf   ', pfwf (:,:)  )
         IF(lwp) WRITE(numout,*) ''
      END IF
      !
   END SUBROUTINE isf_par

   SUBROUTINE isf_par_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE isf_par_init  ***
      !!
      !! ** Purpose : initialisation of the variable needed for the parametrisation of ice shelf melt
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ierr
      INTEGER ::   ji, jj     ! dummy loop indices
      REAL(wp), DIMENSION(A2D(1)) ::   ztblmax, ztblmin
      !!----------------------------------------------------------------------
      !
      !==============
      ! 0: allocation
      !==============
      CALL isf_alloc_par()
      !
      !==================
      ! 1: initialisation
      !==================
      DO_2D( 1, 1, 1, 1 )
         misfkt_par   (ji,jj) = 1
         misfkb_par   (ji,jj) = 1         
         rhisf_tbl_par(ji,jj) = 1e-20
         rfrac_tbl_par(ji,jj) = 0.0_wp
      END_2D
      !
      ! define isf tbl tickness, top and bottom indice
      CALL read_2dcstdta( TRIM(sn_isfpar_zmax%clname), TRIM(sn_isfpar_zmax%clvar), ztblmax )
      CALL read_2dcstdta( TRIM(sn_isfpar_zmin%clname), TRIM(sn_isfpar_zmin%clvar), ztblmin )
      !
      DO_2D( 1, 1, 1, 1 )
         ! mask ice shelf parametrisation location
         ztblmax(ji,jj) = ztblmax(ji,jj) * ssmask(ji,jj)
         ztblmin(ji,jj) = ztblmin(ji,jj) * ssmask(ji,jj)
         !
         ! if param used under an ice shelf overwrite ztblmin by the ice shelf draft
         IF( risfdep(ji,jj) > 0._wp .AND. ztblmin(ji,jj) > 0._wp )   ztblmin(ji,jj) = risfdep(ji,jj)
         !
         ! ensure ztblmax <= bathy
         ztblmax(ji,jj) = MIN( ztblmax(ji,jj), bathy(ji,jj) )
      END_2D
      !
      ! compute ktop and update ztblmin to gdepw_0 at misfkt_par
      CALL isf_tbl_ktop(ztblmin, misfkt_par) !   out: misfkt_par
      !                                      ! inout: ztblmin
      !
      ! initial tbl thickness
      DO_2D( 1, 1, 1, 1 )
         rhisf0_tbl_par(ji,jj) = ztblmax(ji,jj) - ztblmin(ji,jj)
      END_2D
      !
      ! define iceshelf parametrisation mask
      mskisf_par = 0
      WHERE ( rhisf0_tbl_par(A2D(0)) > 0._wp )
         mskisf_par(:,:) = 1._wp
      END WHERE
      !
#if ! defined key_RK3
      !================
      ! 2: read restart
      !================
      ! MLF: read par variable from restart
      IF ( ln_rstart ) CALL isfrst_read( 'par', risf_par_tsc, fwfisf_par, risf_par_tsc_b, fwfisf_par_b )
#endif
      !
      !==========================================
      ! 3: specific allocation and initialisation (depending of scheme choice)
      !==========================================
      SELECT CASE ( TRIM(cn_isfpar_mlt) )
         !
      CASE ( 'spe' )
         !
         ALLOCATE( sf_isfpar_fwf(1), STAT=ierr )
         ALLOCATE( sf_isfpar_fwf(1)%fnow(A2D(0),1), sf_isfpar_fwf(1)%fdta(A2D(0),1,2) )
         CALL fld_fill( sf_isfpar_fwf, (/ sn_isfpar_fwf /), cn_isfdir, 'isf_par_init', 'read fresh water flux isf data', 'namisf' )
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '      ==>>>   ice melt read from forcing field (cn_isfmlt_par = spe)'
         !
      CASE ( 'bg03' )
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '      ==>>>   bg03 parametrisation (cn_isfmlt_par = bg03)'
         !
         ! read effective length
         CALL read_2dcstdta(TRIM(sn_isfpar_Leff%clname), TRIM(sn_isfpar_Leff%clvar), risfLeff)
         risfLeff = risfLeff*1000.0_wp           !: convertion in m
         !
      CASE ( 'oasis' )
         !
         ALLOCATE( sf_isfpar_fwf(1), STAT=ierr )
         ALLOCATE( sf_isfpar_fwf(1)%fnow(A2D(0),1), sf_isfpar_fwf(1)%fdta(A2D(0),1,2) )
         CALL fld_fill( sf_isfpar_fwf, (/ sn_isfpar_fwf /), cn_isfdir, 'isf_par_init', 'read fresh water flux isf data', 'namisf' )
         !
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '      ==>>>    isf melt provided by OASIS (cn_isfmlt_par = oasis)'
         !
      CASE DEFAULT
         CALL ctl_stop( 'sbc_isf_init: wrong value of nn_isf' )
      END SELECT
      !
   END SUBROUTINE isf_par_init

END MODULE isfpar
