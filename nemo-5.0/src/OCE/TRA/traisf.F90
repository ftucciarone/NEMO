MODULE traisf
   !!======================================================================
   !!                     ***  MODULE  traisf  ***
   !! Ocean active tracers:  ice shelf boundary condition
   !!======================================================================
   !! History :  4.0  !  2019-09  (P. Mathiot) original file
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_isf       : update the tracer trend at ocean surface
   !!       isf_mlt   : temperature trend due to the ice shelf melting
   !!       isf_cpl   : T-S         trend due to the ice shelf coupling
   !!----------------------------------------------------------------------
   USE isf_oce                                     ! Ice shelf variables
   USE par_oce , ONLY : nijtile, ntile, ntsi, ntei, ntsj, ntej
   USE dom_oce                                     ! ocean space domain variables
   USE isfutils, ONLY : debug                      ! debug option
   USE timing  , ONLY : timing_start, timing_stop  ! Timing
   USE in_out_manager                              ! I/O manager
   !
   USE prtctl                                      ! print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_isf   ! routine called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_isf( kt, Kmm, pts, Krhs )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE tra_isf  ***
      !!
      !! ** Purpose :  Compute the temperature trend due to the ice shelf melting (qhoce + qhc)
      !!
      !! ** Action  : - update pts(:,:,:,:,Krhs) for cav, par and cpl case
      !!-------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) :: kt        ! ocean time step
      INTEGER                                  , INTENT(in   ) :: Kmm, Krhs ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts       ! active tracers and RHS of tracer equation
      !!-------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_isf')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_isf : Ice shelf heat fluxes'
            IF(lwp) WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF
      !
#if defined key_RK3
      ! cavity case (RK3)
      IF ( ln_isfcav_mlt ) CALL isf_mlt(misfkt_cav, misfkb_cav, rhisf_tbl_cav, rfrac_tbl_cav, risf_cav_tsc, pts(:,:,:,:,Krhs))
      !
      ! parametrisation case (RK3)
      IF ( ln_isfpar_mlt ) CALL isf_mlt(misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, risf_par_tsc, pts(:,:,:,:,Krhs))
#else
      ! cavity case (MLF)
      IF ( ln_isfcav_mlt ) CALL isf_mlt(misfkt_cav, misfkb_cav, rhisf_tbl_cav, rfrac_tbl_cav, risf_cav_tsc, pts(:,:,:,:,Krhs), risf_cav_tsc_b)
      !
      ! parametrisation case (MLF)
      IF ( ln_isfpar_mlt ) CALL isf_mlt(misfkt_par, misfkb_par, rhisf_tbl_par, rfrac_tbl_par, risf_par_tsc, pts(:,:,:,:,Krhs), risf_par_tsc_b)
#endif
      !
      ! ice sheet coupling case
      IF ( ln_isfcpl ) THEN
         !
         ! Dynamical stability at start up after change in under ice shelf cavity geometry is achieve by correcting the divergence.
         ! This is achieved by applying a volume flux in order to keep the horizontal divergence after remapping
         ! the same as at the end of the latest time step. So correction need to be apply at nit000 (euler time step) and
         ! half of it at nit000+1 (leap frog time step).
         ! in accordance to this, the heat content flux due to injected water need to be added in the temperature and salt trend
         ! at time step nit000 and nit000+1
         IF ( kt == nit000  ) CALL isf_cpl(Kmm, risfcpl_tsc       , pts(:,:,:,:,Krhs))
         IF ( kt == nit000+1) CALL isf_cpl(Kmm, risfcpl_tsc*0.5_wp, pts(:,:,:,:,Krhs))
         !
         ! ensure 0 trend due to unconservation of the ice shelf coupling
         IF ( ln_isfcpl_cons ) CALL isf_cpl(Kmm, risfcpl_cons_tsc, pts(:,:,:,:,Krhs))
         !
      END IF
      !
      IF ( ln_isfdebug ) THEN
         IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN                       ! Do only for the full domain
            CALL debug('tra_isf: pts(:,:,:,:,Krhs) T', pts(:,:,:,1,Krhs))
            CALL debug('tra_isf: pts(:,:,:,:,Krhs) S', pts(:,:,:,2,Krhs))
         ENDIF
      END IF
      !
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Krhs), clinfo1=' isf  - Ta: ', mask1=tmask,   &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Krhs), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( ln_timing )   CALL timing_stop('tra_isf')
      !
   END SUBROUTINE tra_isf

   
   SUBROUTINE isf_mlt( ktop, kbot, phtbl, pfrac, ptsc, pts, ptsc_b )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE isf_mlt  ***
      !!
      !! *** Purpose :  Compute the temperature trend due to the ice shelf melting (qhoce + qhc) for cav or par case
      !!
      !! *** Action :: Update pts(:,:,:,:,Krhs) with the surface boundary condition trend
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(A2D(nn_hls),jpk,jpts)     , INTENT(inout) ::   pts
      INTEGER , DIMENSION(A2D(1))                   , INTENT(in   ) ::   ktop , kbot
      REAL(wp), DIMENSION(A2D(1))                   , INTENT(in   ) ::   phtbl, pfrac
      REAL(wp), DIMENSION(A2D(0),jpts)              , INTENT(in   ) ::   ptsc
      REAL(wp), DIMENSION(A2D(0),jpts)    , OPTIONAL, INTENT(in   ) ::   ptsc_b
      !!
      INTEGER  ::   ji, jj, jk   ! dummy loop index
      INTEGER  ::   ikt, ikb     ! top and bottom level of the tbl
      REAL(wp) ::   ztc          ! total ice shelf tracer trend
      !!----------------------------------------------------------------------
      !
      ! update pts(:,:,:,:,Krhs)
      DO_2D( 0, 0, 0, 0 )
         !
         IF( phtbl(ji,jj) /= 0._wp ) THEN
#if defined key_RK3
            ztc = ptsc(ji,jj,jp_tem) / phtbl(ji,jj)
#else
            ztc = 0.5_wp * ( ptsc(ji,jj,jp_tem) + ptsc_b(ji,jj,jp_tem) ) / phtbl(ji,jj)
#endif
         ELSE
            ztc = 0._wp
         ENDIF
         ! level fully include in the ice shelf boundary layer
         ikt = ktop(ji,jj)
         ikb = kbot(ji,jj)
         DO jk = ikt, ikb - 1
            pts(ji,jj,jk,jp_tem) = pts(ji,jj,jk,jp_tem) + ztc
         END DO
         !
         ! level partially include in ice shelf boundary layer
         pts(ji,jj,ikb,jp_tem) = pts(ji,jj,ikb,jp_tem) + ztc * pfrac(ji,jj)
         !
      END_2D
      !
   END SUBROUTINE isf_mlt


   SUBROUTINE isf_cpl( Kmm, ptsc, ptsa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE isf_cpl  ***
      !!
      !! *** Action :: Update pts(:,:,:,:,Krhs) with the ice shelf coupling trend
      !!
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   Kmm   ! ocean time-level index
      REAL(wp), DIMENSION(A2D(0),jpk,jpts)     , INTENT(in   ) ::   ptsc
      REAL(wp), DIMENSION(A2D(nn_hls),jpk,jpts), INTENT(inout) ::   ptsa
      !!
      INTEGER ::   ji, jj, jk   ! dummy loop index
      !!----------------------------------------------------------------------
      !
      DO_3D( 0, 0, 0, 0, 1, jpk )
         ptsa(ji,jj,jk,jp_tem) = ptsa(ji,jj,jk,jp_tem) + ptsc(ji,jj,jk,jp_tem) * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
         ptsa(ji,jj,jk,jp_sal) = ptsa(ji,jj,jk,jp_sal) + ptsc(ji,jj,jk,jp_sal) * r1_e1e2t(ji,jj) / e3t(ji,jj,jk,Kmm)
      END_3D
      !
   END SUBROUTINE isf_cpl
   
   !!======================================================================
END MODULE traisf
