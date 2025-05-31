MODULE stpmlf
   !!======================================================================
   !!                       ***  MODULE stpMLF  ***
   !! Time-stepping   : manager of the ocean, tracer and ice time stepping
   !!                   using Modified Leap Frog for OCE
   !!======================================================================
   !! History :  OPA  !  1991-03  (G. Madec)  Original code
   !!             -   !  1991-11  (G. Madec)
   !!             -   !  1992-06  (M. Imbard)  add a first output record
   !!             -   !  1996-04  (G. Madec)  introduction of dynspg
   !!             -   !  1996-04  (M.A. Foujols)  introduction of passive tracer
   !!            8.0  !  1997-06  (G. Madec)  new architecture of call
   !!            8.2  !  1997-06  (G. Madec, M. Imbard, G. Roullet)  free surface
   !!             -   !  1999-02  (G. Madec, N. Grima)  hpg implicit
   !!             -   !  2000-07  (J-M Molines, M. Imbard)  Open Bondary Conditions
   !!   NEMO     1.0  !  2002-06  (G. Madec)  free form, suppress macro-tasking
   !!             -   !  2004-08  (C. Talandier) New trends organization
   !!             -   !  2005-01  (C. Ethe) Add the KPP closure scheme
   !!             -   !  2005-11  (G. Madec)  Reorganisation of tra and dyn calls
   !!             -   !  2006-01  (L. Debreu, C. Mazauric)  Agrif implementation
   !!             -   !  2006-07  (S. Masson)  restart using iom
   !!            3.2  !  2009-02  (G. Madec, R. Benshila)  reintroduicing z*-coordinate
   !!             -   !  2009-06  (S. Masson, G. Madec)  TKE restart compatible with key_cpl
   !!            3.3  !  2010-05  (K. Mogensen, A. Weaver, M. Martin, D. Lea) Assimilation interface
   !!             -   !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase + merge TRC-TRA
   !!            3.4  !  2011-04  (G. Madec, C. Ethe) Merge of dtatem and dtasal
   !!            3.6  !  2012-07  (J. Simeon, G. Madec. C. Ethe)  Online coarsening of outputs
   !!            3.6  !  2014-04  (F. Roquet, G. Madec) New equations of state
   !!            3.6  !  2014-10  (E. Clementi, P. Oddo) Add Qiao vertical mixing in case of waves
   !!            3.7  !  2014-10  (G. Madec)  LDF simplication
   !!             -   !  2014-12  (G. Madec) remove KPP scheme
   !!             -   !  2015-11  (J. Chanut) free surface simplification (remove filtered free surface)
   !!            4.0  !  2017-05  (G. Madec)  introduction of the vertical physics manager (zdfphy)
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rewrite in preparation for new timestepping scheme
   !!            4.x  !  2020-08  (S. Techene, G. Madec)  quasi eulerian coordinate time stepping
   !!----------------------------------------------------------------------
#if ! defined key_RK3
# if defined key_qco   ||   defined key_linssh
   !!----------------------------------------------------------------------
   !!   'key_qco'                        Quasi-Eulerian vertical coordinate
   !!                          OR
   !!   'key_linssh                       Fixed in time vertical coordinate
   !!----------------------------------------------------------------------
   !!
   !!----------------------------------------------------------------------
   !!   stp_MLF       : NEMO modified Leap Frog time-stepping with qco or linssh
   !!----------------------------------------------------------------------
   USE step_oce       ! time stepping definition modules
   !
   USE domqco         ! quasi-eulerian coordinate

   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp_MLF   ! called by nemogcm.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE stp_MLF( kstp )
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp_MLF  ***
      !!
      !! ** Purpose : - Time stepping of OCE  (momentum and active tracer eqs.)
      !!              - Time stepping of SI3 (dynamic and thermodynamic eqs.)
      !!              - Time stepping of TRC  (passive tracer eqs.)
      !!
      !! ** Method  : -1- Update forcings and data
      !!              -2- Update ocean physics
      !!              -3- Compute the t and s trends
      !!              -4- Update t and s
      !!              -5- Compute the momentum trends
      !!              -6- Update the horizontal velocity
      !!              -7- Compute the diagnostics variables (rd,N2, hdiv,w)
      !!              -8- Outputs and diagnostics
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('stp_MLF')
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! model timestep
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      IF( l_1st_euler ) THEN     ! start or restart with Euler 1st time-step
         rDt   = rn_Dt   
         r1_Dt = 1._wp / rDt
      ENDIF
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! update I/O and calendar
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      IF( kstp == nit000 ) THEN                       ! initialize IOM context (must be done after nemo_init for AGRIF+XIOS+OASIS)
                             CALL iom_init( cxios_context, ld_closedef=.FALSE. )   ! for model grid (including possible AGRIF zoom)
                             CALL iom_init_closedef
      ELSE
                             CALL day( kstp )         ! Calendar (day was already called at nit000 in day_init)
      ENDIF
                             CALL iom_setkt( kstp - nit000 + 1,      cxios_context          )   ! tell IOM we are at time step kstp

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Update external forcing (tides, open boundaries, ice shelf interaction and surface boundary condition (including sea-ice)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL sbc     ( kstp, Nbb, Nnn )              ! Sea Boundary Condition (including sea-ice)

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  Ocean dynamics : hdiv, ssh, e3, u, v, w
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      IF( .NOT.lk_linssh ) THEN
                         CALL dom_qco_r3c( ssh(:,:,Naa), r3t(:,:,Naa), r3u(:,:,Naa), r3v(:,:,Naa)           )   ! "after" ssh/h_0 ratio at t,u,v pts
         IF( ln_dynspg_exp )   &
            &            CALL dom_qco_r3c( ssh(:,:,Nnn), r3t(:,:,Nnn), r3u(:,:,Nnn), r3v(:,:,Nnn), r3f(:,:) )   ! spg_exp : needed only for "now" ssh/h_0 ratio at f point
      ENDIF

      uu(:,:,:,Nrhs) = 0._wp            ! set dynamics trends to zero
      vv(:,:,:,Nrhs) = 0._wp

                            CALL dyn_spg( kstp, Nbb, Nnn, Nrhs, uu, vv, ssh, uu_b, vv_b, Naa )  ! surface pressure gradient

                            CALL dia_wri   ( kstp,      Nnn )      ! Ocean model outputs (default, tiling-unaware variant of 'dia_wri')
      
      ! Swap time levels
      Nrhs = Nbb
      Nbb = Nnn
      Nnn = Naa
      Naa = Nrhs
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! File manipulation at the end of the first time step
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( kstp == nit000 ) THEN                          ! 1st time step only
                                        CALL iom_close( numror )   ! close input  ocean restart file
         IF(lwm)                        CALL FLUSH    ( numond )   ! flush output namelist oce
      ENDIF

      !
#if defined key_xios
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Finalize contextes if end of simulation or error detected
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( kstp == nitend .OR. nstop > 0 ) THEN
                      CALL iom_context_finalize(      cxios_context          ) ! needed for XIOS+AGRIF
      ENDIF
#endif
      !
      IF( l_1st_euler ) THEN         ! recover Leap-frog timestep
         rDt   = 2._wp * rn_Dt
         r1_Dt = 1._wp / rDt
         l_1st_euler = .FALSE.
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('stp_MLF')
      !
   END SUBROUTINE stp_MLF
   

   SUBROUTINE mlf_baro_corr( Kmm, Kaa, puu, pvv )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mlf_baro_corr  ***
      !!
      !! ** Purpose :   Finalize after horizontal velocity.
      !!
      !! ** Method  : * Ensure after velocities transport matches time splitting
      !!             estimate (ln_dynspg_ts=T)
      !!
      !! ** Action :   puu(Kmm),pvv(Kmm)   updated now horizontal velocity (ln_bt_fw=F)
      !!               puu(Kaa),pvv(Kaa)   after horizontal velocity
      !!----------------------------------------------------------------------
      !!
      INTEGER                             , INTENT(in   ) ::   Kmm, Kaa   ! before and after time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::   puu, pvv   ! velocities
      !
      INTEGER  ::   ji,jj, jk   ! dummy loop indices
      REAL(wp), DIMENSION(A2D(0)) ::   zue, zve
      !!----------------------------------------------------------------------

      ! Ensure below that barotropic velocities match time splitting estimate
      ! Compute actual transport and replace it with ts estimate at "after" time step
      DO_2D( 0, 0, 0, 0 )
         zue(ji,jj) = e3u(ji,jj,1,Kaa) * puu(ji,jj,1,Kaa) * umask(ji,jj,1)
         zve(ji,jj) = e3v(ji,jj,1,Kaa) * pvv(ji,jj,1,Kaa) * vmask(ji,jj,1)
      END_2D
      DO_3D( 0, 0, 0, 0, 2, jpkm1 )
         zue(ji,jj) = zue(ji,jj) + e3u(ji,jj,jk,Kaa) * puu(ji,jj,jk,Kaa) * umask(ji,jj,jk)
         zve(ji,jj) = zve(ji,jj) + e3v(ji,jj,jk,Kaa) * pvv(ji,jj,jk,Kaa) * vmask(ji,jj,jk)
      END_3D
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         puu(ji,jj,jk,Kaa) = ( puu(ji,jj,jk,Kaa) - zue(ji,jj) * r1_hu(ji,jj,Kaa) + uu_b(ji,jj,Kaa) ) * umask(ji,jj,jk)
         pvv(ji,jj,jk,Kaa) = ( pvv(ji,jj,jk,Kaa) - zve(ji,jj) * r1_hv(ji,jj,Kaa) + vv_b(ji,jj,Kaa) ) * vmask(ji,jj,jk)
      END_3D
      !
      IF( .NOT.ln_bt_fw ) THEN
         ! Remove advective velocity from "now velocities"
         ! prior to asselin filtering
         ! In the forward case, this is done below after asselin filtering
         ! so that asselin contribution is removed at the same time
         DO jk = 1, jpkm1
            puu(:,:,jk,Kmm) = ( puu(:,:,jk,Kmm) - un_adv(:,:)*r1_hu(:,:,Kmm) + uu_b(:,:,Kmm) )*umask(:,:,jk)
            pvv(:,:,jk,Kmm) = ( pvv(:,:,jk,Kmm) - vn_adv(:,:)*r1_hv(:,:,Kmm) + vv_b(:,:,Kmm) )*vmask(:,:,jk)
         END DO
      ENDIF
      !
   END SUBROUTINE mlf_baro_corr


   SUBROUTINE finalize_lbc( kt, Kbb, Kaa, puu, pvv, pts )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE finalize_lbc  ***
      !!
      !! ** Purpose :   Apply the boundary condition on the after velocity
      !!
      !! ** Method  : * Apply lateral boundary conditions on after velocity
      !!             at the local domain boundaries through lbc_lnk call,
      !!             at the one-way open boundaries (ln_bdy=T),
      !!             at the AGRIF zoom   boundaries (lk_agrif=T)
      !!
      !! ** Action :   puu(Kaa),pvv(Kaa)   after horizontal velocity and tracers
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt         ! ocean time-step index
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kaa   ! before and after time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt)     , INTENT(inout) ::   puu, pvv   ! velocities to be time filtered
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) ::   pts        ! active tracers
      !!----------------------------------------------------------------------
      !
      ! Update after tracer and velocity on domain lateral boundaries
      !
      !                                        ! local domain boundaries  (T-point, unchanged sign)
      CALL lbc_lnk( 'finalize_lbc', puu(:,:,:,Kaa), 'U', -1._wp, pvv(:,:,:,Kaa), 'V', -1._wp, ldfull=.TRUE. )
      !

      ! dom_qco_r3c defines over [nn_hls, nn_hls-1, nn_hls, nn_hls-1]
      IF( .NOT. lk_linssh ) THEN
         CALL lbc_lnk( 'finalize_lbc', r3u(:,:,Kaa), 'U', 1._wp, r3v(:,:,Kaa), 'V', 1._wp, &
            &                          r3u_f(:,:),   'U', 1._wp, r3v_f(:,:),   'V', 1._wp  )
      ENDIF
      !
   END SUBROUTINE finalize_lbc

# else
   !!----------------------------------------------------------------------
   !!   default option             EMPTY MODULE           qco not activated
   !!----------------------------------------------------------------------
# endif
#endif
   
   !!======================================================================
END MODULE stpmlf
