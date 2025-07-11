MODULE dynatf_qco
   !!=========================================================================
   !!                       ***  MODULE  dynatf_qco  ***
   !! Ocean dynamics: time filtering
   !!=========================================================================
   !! History :  OPA  !  1987-02  (P. Andrich, D. L Hostis)  Original code
   !!                 !  1990-10  (C. Levy, G. Madec)
   !!            7.0  !  1993-03  (M. Guyon)  symetrical conditions
   !!            8.0  !  1997-02  (G. Madec & M. Imbard)  opa, release 8.0
   !!            8.2  !  1997-04  (A. Weaver)  Euler forward step
   !!             -   !  1997-06  (G. Madec)  lateral boudary cond., lbc routine
   !!    NEMO    1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!             -   !  2002-10  (C. Talandier, A-M. Treguier) Open boundary cond.
   !!            2.0  !  2005-11  (V. Garnier) Surface pressure gradient organization
   !!            2.3  !  2007-07  (D. Storkey) Calls to BDY routines.
   !!            3.2  !  2009-06  (G. Madec, R.Benshila)  re-introduce the vvl option
   !!            3.3  !  2010-09  D. Storkey, E.O'Dea) Bug fix for BDY module
   !!            3.3  !  2011-03  (P. Oddo) Bug fix for time-splitting+(BDY-OBC) and not VVL
   !!            3.5  !  2013-07  (J. Chanut) Compliant with time splitting changes
   !!            3.6  !  2014-04  (G. Madec) add the diagnostic of the time filter trends
   !!            3.7  !  2015-11  (J. Chanut) Free surface simplification
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) Rename dynnxt.F90 -> dynatfLF.F90. Now just does time filtering.
   !!-------------------------------------------------------------------------

   !!----------------------------------------------------------------------------------------------
   !!   dyn_atf_qco       : apply Asselin time filtering to "now" velocities and vertical scale factors
   !!----------------------------------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbcrnf         ! river runoffs
   USE phycst         ! physical constants
   USE dynadv         ! dynamics: vector invariant versus flux form
   USE dynspg_ts      ! surface pressure gradient: split-explicit scheme
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   USE trdken         ! trend manager: kinetic energy
   USE isf_oce   , ONLY: ln_isf     ! ice shelf
   USE isfdynatf , ONLY: isf_dynatf ! ice shelf volume filter correction subroutine
   USE zdfdrg    , ONLY: ln_drgice_imp, rCdU_top
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lbclnk         ! lateral boundary condition (or mpp link)
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC    dyn_atf_qco   ! routine called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_atf_qco( kt, Kbb, Kmm, Kaa, puu, pvv )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_atf_qco  ***
      !!
      !! ** Purpose :   Finalize after horizontal velocity. Apply the boundary
      !!             condition on the after velocity and apply the Asselin time
      !!             filter to the now fields.
      !!
      !! ** Method  : * Ensure after velocities transport matches time splitting
      !!             estimate (ln_dynspg_ts=T)
      !!
      !!              * Apply lateral boundary conditions on after velocity
      !!             at the local domain boundaries through lbc_lnk call
      !!
      !!              * Apply the Asselin time filter to the now fields
      !!             arrays to start the next time step:
      !!                (puu(Kmm),pvv(Kmm)) = (puu(Kmm),pvv(Kmm))
      !!                                    + atfp [ (puu(Kbb),pvv(Kbb)) + (puu(Kaa),pvv(Kaa)) - 2 (puu(Kmm),pvv(Kmm)) ]
      !!             Note that with flux form advection and non linear free surface,
      !!             the time filter is applied on thickness weighted velocity.
      !!             As a result, dyn_atf_lf MUST be called after tra_atf.
      !!
      !! ** Action :   puu(Kmm),pvv(Kmm)   filtered now horizontal velocity
      !!----------------------------------------------------------------------
      INTEGER                             , INTENT(in   ) :: kt               ! ocean time-step index
      INTEGER                             , INTENT(in   ) :: Kbb, Kmm, Kaa    ! before and after time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) :: puu, pvv         ! velocities to be time filtered
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zue3a, zue3n, zue3b, zcoef    ! local scalars
      REAL(wp) ::   zve3a, zve3n, zve3b           !   -      -
#if ! defined key_PSYCLONE_2p5p0
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zue, zve
#else
      REAL(wp), DIMENSION(jpi,jpj)            ::   zue, zve
#endif
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   zua, zva
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zutau, zvtau
      !!----------------------------------------------------------------------
      !
      IF( ln_timing    )   CALL timing_start('dyn_atf_qco')
#if ! defined key_PSYCLONE_2p5p0
      IF( ln_dynspg_ts )   ALLOCATE( zue(jpi,jpj)   , zve(jpi,jpj)    )
#endif
      IF( l_trddyn     )   ALLOCATE( zua(T2D(0),jpk), zva(T2D(0),jpk) )
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'dyn_atf_qco : Asselin time filtering'
         IF(lwp) WRITE(numout,*) '~~~~~~~'
      ENDIF
      !
      IF( l_trddyn ) THEN             ! prepare the atf trend computation + some diagnostics
         !
         !                                  ! Kinetic energy and Conversion
         IF( ln_KE_trd  ) THEN
            zua(:,:,:) = puu(T2D(0),:,Kaa)
            zva(:,:,:) = pvv(T2D(0),:,Kaa)
            CALL trd_dyn( zua(:,:,:), zva(:,:,:), jpdyn_ken, kt, Kmm )
         ENDIF
         !
         IF( ln_dyn_trd ) THEN              ! 3D output: total momentum trends
            zua(:,:,:) = ( puu(T2D(0),:,Kaa) - puu(T2D(0),:,Kbb) ) * r1_Dt
            zva(:,:,:) = ( pvv(T2D(0),:,Kaa) - pvv(T2D(0),:,Kbb) ) * r1_Dt
            CALL iom_put( "utrd_tot", zua )        ! total momentum trends, except the asselin time filter
            CALL iom_put( "vtrd_tot", zva )
         ENDIF
         !
         zua(:,:,:) = puu(T2D(0),:,Kmm)     ! save the now velocity before the asselin filter
         zva(:,:,:) = pvv(T2D(0),:,Kmm)     ! (caution: there will be a shift by 1 timestep in the
         !                                  !  computation of the asselin filter trends)
      ENDIF

      ! Time filter and swap of dynamics arrays
      ! ------------------------------------------

      IF( .NOT. l_1st_euler ) THEN    !* Leap-Frog : Asselin time filter
         !                                ! =============!
         IF( lk_linssh ) THEN             ! Fixed volume !
            !                             ! =============!
            DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
               puu(ji,jj,jk,Kmm) = puu(ji,jj,jk,Kmm) + rn_atfp * ( puu(ji,jj,jk,Kbb) - 2._wp * puu(ji,jj,jk,Kmm) + puu(ji,jj,jk,Kaa) )
               pvv(ji,jj,jk,Kmm) = pvv(ji,jj,jk,Kmm) + rn_atfp * ( pvv(ji,jj,jk,Kbb) - 2._wp * pvv(ji,jj,jk,Kmm) + pvv(ji,jj,jk,Kaa) )
            END_3D
            !                             ! ================!
         ELSE                             ! Variable volume !
            !                             ! ================!
            !
            IF( ln_dynadv_vec ) THEN      ! Asselin filter applied on velocity
               ! Before filtered scale factor at (u/v)-points
               DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
                  puu(ji,jj,jk,Kmm) = puu(ji,jj,jk,Kmm) + rn_atfp * ( puu(ji,jj,jk,Kbb) - 2._wp * puu(ji,jj,jk,Kmm) + puu(ji,jj,jk,Kaa) )
                  pvv(ji,jj,jk,Kmm) = pvv(ji,jj,jk,Kmm) + rn_atfp * ( pvv(ji,jj,jk,Kbb) - 2._wp * pvv(ji,jj,jk,Kmm) + pvv(ji,jj,jk,Kaa) )
               END_3D
               !
            ELSE                          ! Asselin filter applied on thickness weighted velocity
               !
               DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
                  zue3a = ( 1._wp + r3u(ji,jj,Kaa) * umask(ji,jj,jk) ) * puu(ji,jj,jk,Kaa)
                  zve3a = ( 1._wp + r3v(ji,jj,Kaa) * vmask(ji,jj,jk) ) * pvv(ji,jj,jk,Kaa)
                  zue3n = ( 1._wp + r3u(ji,jj,Kmm) * umask(ji,jj,jk) ) * puu(ji,jj,jk,Kmm)
                  zve3n = ( 1._wp + r3v(ji,jj,Kmm) * vmask(ji,jj,jk) ) * pvv(ji,jj,jk,Kmm)
                  zue3b = ( 1._wp + r3u(ji,jj,Kbb) * umask(ji,jj,jk) ) * puu(ji,jj,jk,Kbb)
                  zve3b = ( 1._wp + r3v(ji,jj,Kbb) * vmask(ji,jj,jk) ) * pvv(ji,jj,jk,Kbb)
                  !                                                 ! filtered scale factor at U-,V-points
                  puu(ji,jj,jk,Kmm) = ( zue3n + rn_atfp * ( zue3b - 2._wp * zue3n  + zue3a ) ) / ( 1._wp + r3u_f(ji,jj)*umask(ji,jj,jk) )
                  pvv(ji,jj,jk,Kmm) = ( zve3n + rn_atfp * ( zve3b - 2._wp * zve3n  + zve3a ) ) / ( 1._wp + r3v_f(ji,jj)*vmask(ji,jj,jk) )
               END_3D
               !
            ENDIF
            !
         ENDIF
         !
         IF( ln_dynspg_ts .AND. ln_bt_fw ) THEN
            ! Revert filtered "now" velocities to time split estimate
            ! Doing it here also means that asselin filter contribution is removed
            ! zue(:,:) = pe3u(:,:,1,Kmm) * puu(:,:,1,Kmm) * umask(:,:,1)
            ! zve(:,:) = pe3v(:,:,1,Kmm) * pvv(:,:,1,Kmm) * vmask(:,:,1)
            ! DO jk = 2, jpkm1
            !    zue(:,:) = zue(:,:) + pe3u(:,:,jk,Kmm) * puu(:,:,jk,Kmm) * umask(:,:,jk)
            !    zve(:,:) = zve(:,:) + pe3v(:,:,jk,Kmm) * pvv(:,:,jk,Kmm) * vmask(:,:,jk)
            ! END DO
            zue(:,:) = e3u(:,:,1,Kmm) * puu(:,:,1,Kmm) * umask(:,:,1)
            zve(:,:) = e3v(:,:,1,Kmm) * pvv(:,:,1,Kmm) * vmask(:,:,1)
            DO jk = 2, jpkm1
               zue(:,:) = zue(:,:) + e3u(:,:,jk,Kmm) * puu(:,:,jk,Kmm) * umask(:,:,jk)
               zve(:,:) = zve(:,:) + e3v(:,:,jk,Kmm) * pvv(:,:,jk,Kmm) * vmask(:,:,jk)
            END DO
            DO jk = 1, jpkm1
               puu(:,:,jk,Kmm) = puu(:,:,jk,Kmm) - (zue(:,:) * r1_hu(:,:,Kmm) - uu_b(:,:,Kmm)) * umask(:,:,jk)
               pvv(:,:,jk,Kmm) = pvv(:,:,jk,Kmm) - (zve(:,:) * r1_hv(:,:,Kmm) - vv_b(:,:,Kmm)) * vmask(:,:,jk)
            END DO
         ENDIF
         !
      ENDIF ! .NOT. l_1st_euler
      !
      ! This is needed for dyn_ldf_blp to be restartable
      CALL lbc_lnk( 'dynatfqco', puu(:,:,:,Kmm), 'U', -1.0_wp, pvv(:,:,:,Kmm), 'V', -1.0_wp )

      ! Set "now" and "before" barotropic velocities for next time step:
      ! JC: Would be more clever to swap variables than to make a full vertical
      ! integration
      ! CAUTION : calculation need to be done in the same way than see GM
#if defined key_linssh
      uu_b(:,:,Kaa) = e3u(:,:,1,Kaa) * puu(:,:,1,Kaa) * umask(:,:,1)
      uu_b(:,:,Kmm) = e3u(:,:,1,Kmm) * puu(:,:,1,Kmm) * umask(:,:,1)
      vv_b(:,:,Kaa) = e3v(:,:,1,Kaa) * pvv(:,:,1,Kaa) * vmask(:,:,1)
      vv_b(:,:,Kmm) = e3v(:,:,1,Kmm) * pvv(:,:,1,Kmm) * vmask(:,:,1)
      DO jk = 2, jpkm1
         uu_b(:,:,Kaa) = uu_b(:,:,Kaa) + e3u(:,:,jk,Kaa) * puu(:,:,jk,Kaa) * umask(:,:,jk)
         uu_b(:,:,Kmm) = uu_b(:,:,Kmm) + e3u(:,:,jk,Kmm) * puu(:,:,jk,Kmm) * umask(:,:,jk)
         vv_b(:,:,Kaa) = vv_b(:,:,Kaa) + e3v(:,:,jk,Kaa) * pvv(:,:,jk,Kaa) * vmask(:,:,jk)
         vv_b(:,:,Kmm) = vv_b(:,:,Kmm) + e3v(:,:,jk,Kmm) * pvv(:,:,jk,Kmm) * vmask(:,:,jk)
      END DO
      uu_b(:,:,Kaa) = uu_b(:,:,Kaa) * r1_hu(:,:,Kaa)
      vv_b(:,:,Kaa) = vv_b(:,:,Kaa) * r1_hv(:,:,Kaa)
      uu_b(:,:,Kmm) = uu_b(:,:,Kmm) * r1_hu(:,:,Kmm)
      vv_b(:,:,Kmm) = vv_b(:,:,Kmm) * r1_hv(:,:,Kmm)
#else
      uu_b(:,:,Kaa) = e3u(:,:,1,Kaa) * puu(:,:,1,Kaa) * umask(:,:,1)
      uu_b(:,:,Kmm) = (e3u_0(:,:,1) * ( 1._wp + r3u_f(:,:) * umask(:,:,1) )) * puu(:,:,1,Kmm) * umask(:,:,1)
      vv_b(:,:,Kaa) = e3v(:,:,1,Kaa) * pvv(:,:,1,Kaa) * vmask(:,:,1)
      vv_b(:,:,Kmm) = (e3v_0(:,:,1) * ( 1._wp + r3v_f(:,:) * vmask(:,:,1))) * pvv(:,:,1,Kmm) * vmask(:,:,1)
      DO jk = 2, jpkm1
         uu_b(:,:,Kaa) = uu_b(:,:,Kaa) + e3u(:,:,jk,Kaa) * puu(:,:,jk,Kaa) * umask(:,:,jk)
         uu_b(:,:,Kmm) = uu_b(:,:,Kmm) + (e3u_0(:,:,jk) * ( 1._wp + r3u_f(:,:) * umask(:,:,jk) )) * puu(:,:,jk,Kmm) * umask(:,:,jk)
         vv_b(:,:,Kaa) = vv_b(:,:,Kaa) + e3v(:,:,jk,Kaa) * pvv(:,:,jk,Kaa) * vmask(:,:,jk)
         vv_b(:,:,Kmm) = vv_b(:,:,Kmm) + (e3v_0(:,:,jk) * ( 1._wp + r3v_f(:,:) * vmask(:,:,jk) )) * pvv(:,:,jk,Kmm) * vmask(:,:,jk)
      END DO
      uu_b(:,:,Kaa) = uu_b(:,:,Kaa) * r1_hu(:,:,Kaa)
      vv_b(:,:,Kaa) = vv_b(:,:,Kaa) * r1_hv(:,:,Kaa)
      uu_b(:,:,Kmm) = uu_b(:,:,Kmm) * (r1_hu_0(:,:)/( 1._wp + r3u_f(:,:) ))
      vv_b(:,:,Kmm) = vv_b(:,:,Kmm) * (r1_hv_0(:,:)/( 1._wp + r3v_f(:,:) ))
#endif
      !
      IF( .NOT.ln_dynspg_ts ) THEN        ! output the barotropic currents
         CALL iom_put(  "ubar", uu_b(:,:,Kmm) )
         CALL iom_put(  "vbar", vv_b(:,:,Kmm) )
      ENDIF
      IF( l_trddyn ) THEN                ! 3D output: asselin filter trends on momentum
         zua(:,:,:) = ( puu(T2D(0),:,Kmm) - zua(:,:,:) ) * r1_Dt
         zva(:,:,:) = ( pvv(T2D(0),:,Kmm) - zva(:,:,:) ) * r1_Dt
         CALL trd_dyn( zua, zva, jpdyn_atf, kt, Kmm )
      ENDIF
      !
      IF ( iom_use("utau") ) THEN
         IF ( ln_drgice_imp.OR.ln_isfcav ) THEN
            ! TEMP: Declared with halo points so there is a consistent shape for XIOS
            ALLOCATE(zutau(A2D(nn_hls)))
            DO_2D( 0, 0, 0, 0 )
               jk = miku(ji,jj)
               zutau(ji,jj) = utau(ji,jj) + 0.5_wp * rho0 * rCdU_top(ji,jj) * ( puu(ji-1,jj,jk,Kaa) + puu(ji,jj,jk,Kaa) )
            END_2D
            CALL iom_put(  "utau", zutau(:,:) )
            DEALLOCATE(zutau)
         ELSE
            CALL iom_put(  "utau", utau(:,:) )
         ENDIF
      ENDIF
      !
      IF ( iom_use("vtau") ) THEN
         IF ( ln_drgice_imp.OR.ln_isfcav ) THEN
            ! TEMP: Declared with halo points so there is a consistent shape for XIOS
            ALLOCATE(zvtau(A2D(nn_hls)))
            DO_2D( 0, 0, 0, 0 )
               jk = mikv(ji,jj)
               zvtau(ji,jj) = vtau(ji,jj) + 0.5_wp * rho0 * rCdU_top(ji,jj) * ( pvv(ji,jj-1,jk,Kaa) + pvv(ji,jj,jk,Kaa) )
            END_2D
            CALL iom_put(  "vtau", zvtau(:,:) )
            DEALLOCATE(zvtau)
         ELSE
            CALL iom_put(  "vtau", vtau(:,:) )
         ENDIF
      ENDIF
      !
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Kaa), clinfo1=' nxt  - puu(:,:,:,Kaa): ', mask1=umask,   &
         &                                  tab3d_2=pvv(:,:,:,Kaa), clinfo2=' pvv(:,:,:,Kaa): '       , mask2=vmask )
      !
#if ! defined key_PSYCLONE_2p5p0
      IF( ln_dynspg_ts )   DEALLOCATE( zue, zve )
#endif
      IF( l_trddyn     )   DEALLOCATE( zua, zva )
      IF( ln_timing    )   CALL timing_stop('dyn_atf_qco')
      !
   END SUBROUTINE dyn_atf_qco

   !!=========================================================================
END MODULE dynatf_qco
