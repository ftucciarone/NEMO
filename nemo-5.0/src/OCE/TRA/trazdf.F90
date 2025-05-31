MODULE trazdf
   !!==============================================================================
   !!                 ***  MODULE  trazdf  ***
   !! Ocean active tracers:  vertical component of the tracer mixing trend
   !!==============================================================================
   !! History :  1.0  !  2005-11  (G. Madec)  Original code
   !!            3.0  !  2008-01  (C. Ethe, G. Madec)  merge TRC-TRA
   !!            4.0  !  2017-06  (G. Madec)  remove explict time-stepping option
   !!            4.5  !  2022-06  (G. Madec)  refactoring to reduce memory usage (j-k-i loops)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_zdf       : Update the tracer trend with the vertical diffusion
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean space and time domain variables
   USE phycst         ! physical constant
   USE zdf_oce        ! ocean vertical physics variables
   USE zdfmfc         ! Mass FLux Convection
   USE sbc_oce        ! surface boundary condition: ocean
   USE ldftra         ! lateral diffusion: eddy diffusivity
   USE ldfslp         ! lateral diffusion: iso-neutral slope
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends: tracer trend manager
   USE eosbn2   , ONLY: ln_SEOS, rn_b0
   !
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! MPP library
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_zdf       ! called by step.F90
   PUBLIC   tra_zdf_imp   ! called by trczdf.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_zdf( kt, Kbb, Kmm, Krhs, pts, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_zdf  ***
      !!
      !! ** Purpose :   compute the vertical ocean tracer physics.
      !!---------------------------------------------------------------------
      INTEGER                                  , INTENT(in)    :: kt                  ! ocean time-step index
      INTEGER                                  , INTENT(in)    :: Kbb, Kmm, Krhs, Kaa ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts                 ! active tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk   ! Dummy loop indices
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdt, ztrds   ! 3D workspace
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_zdf')
      !
      IF( kt == nit000 )  THEN
         IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                   ! Do only on the first tile
            IF(lwp)WRITE(numout,*)
            IF(lwp)WRITE(numout,*) 'tra_zdf : implicit vertical mixing on T & S'
            IF(lwp)WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF
      !
      IF( l_trdtra )   THEN                  !* Save ta and sa trends
         ALLOCATE( ztrdt(T2D(0),jpk), ztrds(T2D(0),jpk) )
         ztrdt(:,:,:) = pts(T2D(0),:,jp_tem,Kaa)
         ztrds(:,:,:) = pts(T2D(0),:,jp_sal,Kaa)
      ENDIF
      !
      !                                      !* compute lateral mixing trend and add it to the general trend
      CALL tra_zdf_imp( 'TRA', rDt, Kbb, Kmm, Krhs, pts, Kaa, jpts )

!!gm WHY here !   and I don't like that !
      ! DRAKKAR SSS control {
      ! JMM avoid negative salinities near river outlet ! Ugly fix
      ! JMM : restore negative salinities to small salinities:
!!jc: discard this correction in case salinity is not used in eos
      IF ( .NOT.(ln_SEOS.AND.(rn_b0==0._wp)) ) THEN
         WHERE( pts(T2D(0),:,jp_sal,Kaa) < 0._wp )   pts(T2D(0),:,jp_sal,Kaa) = 0.1_wp
      ENDIF
!!gm

      IF( l_trdtra )   THEN                      ! save the vertical diffusive trends for further diagnostics
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            ztrdt(ji,jj,jk) = (   (  pts(ji,jj,jk,jp_tem,Kaa)*e3t(ji,jj,jk,Kaa)     &
               &                 - pts(ji,jj,jk,jp_tem,Kbb)*e3t(ji,jj,jk,Kbb)  )  &
               &              / (  e3t(ji,jj,jk,Kmm)*rDt  )   )                 &
               &          - ztrdt(ji,jj,jk)
            ztrds(ji,jj,jk) = (   (  pts(ji,jj,jk,jp_sal,Kaa)*e3t(ji,jj,jk,Kaa)     &
               &                 - pts(ji,jj,jk,jp_sal,Kbb)*e3t(ji,jj,jk,Kbb)  )  &
               &             / (   e3t(ji,jj,jk,Kmm)*rDt  )   )                 &
               &          - ztrds(ji,jj,jk)
         END_3D
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_zdf, ztrdt )
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_sal, jptra_zdf, ztrds )
         DEALLOCATE( ztrdt , ztrds )
      ENDIF
      !                                          ! print mean trends (used for debugging)
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Kaa), clinfo1=' zdf  - Ta: ', mask1=tmask,               &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Kaa), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( ln_timing )   CALL timing_stop('tra_zdf')
      !
   END SUBROUTINE tra_zdf


   SUBROUTINE tra_zdf_imp( cdtype, p2dt, Kbb, Kmm, Krhs, pt, Kaa, kjpt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_zdf_imp  ***
      !!
      !! ** Purpose :   Compute the after tracer through a implicit computation
      !!     of the vertical tracer diffusion (including the vertical component
      !!     of lateral mixing (only for 2nd order operator, for fourth order
      !!     it is already computed and add to the general trend in traldf)
      !!
      !! ** Method  :  The vertical diffusion of a tracer ,t , is given by:
      !!          difft = dz( avt dz(t) ) = 1/e3t dk+1( avt/e3w dk(t) )
      !!      It is computed using a backward time scheme (t=after field)
      !!      which provide directly the after tracer field.
      !!      If ln_zdfddm=T, use avs for salinity or for passive tracers
      !!      Surface and bottom boundary conditions: no diffusive flux on
      !!      both tracers (bottom, applied through the masked field avt).
      !!      If iso-neutral mixing, add to avt the contribution due to lateral mixing.
      !!
      !! ** Action  : - pt(:,:,:,:,Kaa)  becomes the after tracer
      !!---------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs, Kaa  ! ocean time level indices
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype   ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt     ! number of tracers
      REAL(wp)                                 , INTENT(in   ) ::   p2dt     ! tracer time-step
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt       ! tracers and RHS of tracer equation
      !
      INTEGER  ::  ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::  zrhs, zzwi, zzws ! local scalars
      REAL(wp), DIMENSION(T1Di(0),jpk) ::  zwi, zwt, zwd, zws
      !!---------------------------------------------------------------------
      !
      !                                               ! ================= !
      DO_1Dj( 0, 0 )                                  !  i-k slices loop  !
         !                                            ! ================= !
         DO jn = 1, kjpt                              !    tracer loop    !
            !                                         ! ================= !
            !
            !  Matrix construction
            ! --------------------
            ! Build matrix if temperature or salinity (only in double diffusion case) or first passive tracer
            !
            IF(  ( cdtype == 'TRA' .AND. ( jn == jp_tem .OR. ( jn == jp_sal .AND. ln_zdfddm ) ) ) .OR.   &
               & ( cdtype == 'TRC' .AND. jn == 1 )  )  THEN
               !
               ! vertical mixing coef.: avt for temperature, avs for salinity and passive tracers
               !
               IF( cdtype == 'TRA' .AND. jn == jp_tem ) THEN     ! use avt  for temperature
                  !
                  IF( l_ldfslp ) THEN            ! use avt + isoneutral diffusion contribution
                     IF( ln_traldf_msc  ) THEN        ! MSC iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avt(ji,jj,jk) + akz(ji,jj,jk)
                        END_2D
                     ELSE                             ! standard or triad iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avt(ji,jj,jk) + ah_wslp2(ji,jj,jk)
                        END_2D
                     ENDIF
                  ELSE                          ! use avt only
                     DO_2Dik( 0, 0,   2, jpk, 1 )
                        zwt(ji,jk) = avt(ji,jj,jk)
                     END_2D
                  ENDIF
                  !
               ELSE                                               ! use avs for salinty or passive tracers
                  !
                  IF( l_ldfslp ) THEN            ! use avs + isoneutral diffusion contribution
                     IF( ln_traldf_msc  ) THEN        ! MSC iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avs(ji,jj,jk) + akz(ji,jj,jk)
                        END_2D
                     ELSE                             ! standard or triad iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avs(ji,jj,jk) + ah_wslp2(ji,jj,jk)
                        END_2D
                     ENDIF
                  ELSE                          !
                     DO_2Dik( 0, 0,   2, jpk, 1 )
                        zwt(ji,jk) = avs(ji,jj,jk)
                     END_2D
                  ENDIF
               ENDIF
               zwt(:,1) = 0._wp
               !
               ! Diagonal, lower (i), upper (s)  (including the bottom boundary condition since avt is masked)
               IF( ln_zad_Aimp ) THEN         ! Adaptive implicit vertical advection
                  DO_2Dik( 0, 0,   1, jpkm1, 1 )
                     zzwi = - p2dt * zwt(ji,jk  ) / e3w(ji,jj,jk  ,Kmm)
                     zzws = - p2dt * zwt(ji,jk+1) / e3w(ji,jj,jk+1,Kmm)
                     zwd(ji,jk) = e3t(ji,jj,jk,Kaa) - ( zzwi + zzws )   &
                        &              + p2dt * ( MAX( wi(ji,jj,jk  ) , 0._wp ) &
                        &                      - MIN( wi(ji,jj,jk+1) , 0._wp ) )
                     zwi(ji,jk) = zzwi + p2dt *   MIN( wi(ji,jj,jk  ) , 0._wp )
                     zws(ji,jk) = zzws - p2dt *   MAX( wi(ji,jj,jk+1) , 0._wp )
                  END_2D
               ELSE
                  DO_2Dik( 0, 0,   1, jpkm1, 1 )
                     zwi(ji,jk) = - p2dt * zwt(ji,jk  ) / e3w(ji,jj,jk,Kmm)
                     zws(ji,jk) = - p2dt * zwt(ji,jk+1) / e3w(ji,jj,jk+1,Kmm)
                     zwd(ji,jk) = e3t(ji,jj,jk,Kaa) - ( zwi(ji,jk) + zws(ji,jk) )
                  END_2D
               ENDIF
               !
!!gm  BUG?? : if edmfm is equivalent to a w  ==>>>   just add +/-  rDt * edmfm(ji,jj,jk+1/jk  )
!!            but edmfm is at t-point !!!!   crazy???  why not keep it at w-point????
!!gm   BUG ???   below  e3t_Kmm  should be used ?
!!               or even no multiplication by e3t unless there is a bug in wi calculation
               IF( ln_zdfmfc ) THEN    ! add upward Mass Flux in the matrix
                  DO_2Dik( 0, 0,   1, jpkm1, 1 )
                     ! zwi not updated- in the original zdfmfc.F90 calculation the added flux was zero over 1:jpkm1
                     zws(ji,jk) = zws(ji,jk) + e3t(ji,jj,jk,Kaa) * p2dt * edmfm(ji,jj,jk+1) / e3w(ji,jj,jk+1,Kmm)
                     zwd(ji,jk) = zwd(ji,jk) - e3t(ji,jj,jk,Kaa) * p2dt * edmfm(ji,jj,jk  ) / e3w(ji,jj,jk+1,Kmm)
                  END_2D
               ENDIF
               !
               !! Matrix inversion from the first level
               !!----------------------------------------------------------------------
               !   solve m.x = y  where m is a tri diagonal matrix ( jpk*jpk )
               !
               !        ( zwd1 zws1   0    0    0  )( zwx1 ) ( zwy1 )
               !        ( zwi2 zwd2 zws2   0    0  )( zwx2 ) ( zwy2 )
               !        (  0   zwi3 zwd3 zws3   0  )( zwx3 )=( zwy3 )
               !        (        ...               )( ...  ) ( ...  )
               !        (  0    0    0   zwik zwdk )( zwxk ) ( zwyk )
               !
               !   m is decomposed in the product of an upper and lower triangular matrix.
               !   The 3 diagonal terms are in 3d arrays: zwd, zws, zwi.
               !   Suffices i,s and d indicate "inferior" (below diagonal), diagonal
               !   and "superior" (above diagonal) components of the tridiagonal system.
               !   The solution will be in the 4d array pta.
               !   The 3d array zwt is used as a work space array.
               !   En route to the solution pt(:,:,:,:,Kaa) is used a to evaluate the rhs and then
               !   used as a work space array: its value is modified.
               !
               DO_1Di( 0, 0 )          !* 1st recurrence:   Tk = Dk - Ik Sk-1 / Tk-1   (increasing k) ! done one for all passive tracers (so included in the IF instruction)
                  zwt(ji,1) = zwd(ji,1)
               END_1D
               DO_2Dik( 0, 0,   2, jpkm1, 1 )
                  zwt(ji,jk) = zwd(ji,jk) - zwi(ji,jk) * zws(ji,jk-1) / zwt(ji,jk-1)
               END_2D
               !
            ENDIF
            !
            IF( ln_zdfmfc ) THEN    ! add Mass Flux to the RHS
               DO_2Dik( 0, 0,   1, jpkm1, 1 )
                  pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) + edmftra(ji,jj,jk,jn)
               END_2D
            ENDIF
            !
            DO_1Di( 0, 0 )             !* 2nd recurrence:    Zk = Yk - Ik / Tk-1  Zk-1
               pt(ji,jj,1,jn,Kaa) =       e3t(ji,jj,1,Kbb) * pt(ji,jj,1,jn,Kbb )    &
                  &               + p2dt * e3t(ji,jj,1,Kmm) * pt(ji,jj,1,jn,Krhs)
            END_1D
            DO_2Dik( 0, 0,   2, jpkm1, 1 )
               zrhs =       e3t(ji,jj,jk,Kbb) * pt(ji,jj,jk,jn,Kbb )   &
                  & + p2dt * e3t(ji,jj,jk,Kmm) * pt(ji,jj,jk,jn,Krhs)   ! zrhs=right hand side
               pt(ji,jj,jk,jn,Kaa) = zrhs - zwi(ji,jk) / zwt(ji,jk-1) * pt(ji,jj,jk-1,jn,Kaa)
            END_2D
            !
            DO_1Di( 0, 0 )             !* 3d recurrence:    Xk = (Zk - Sk Xk+1 ) / Tk   (result is the after tracer)
               pt(ji,jj,jpkm1,jn,Kaa) = pt(ji,jj,jpkm1,jn,Kaa) / zwt(ji,jpkm1) * tmask(ji,jj,jpkm1)
            END_1D
            DO_2Dik( 0, 0,   jpk-2, 1, -1 )
               pt(ji,jj,jk,jn,Kaa) = ( pt(ji,jj,jk,jn,Kaa) - zws(ji,jk) * pt(ji,jj,jk+1,jn,Kaa) )   &
                  &             / zwt(ji,jk) * tmask(ji,jj,jk)
            END_2D
            !                                         ! ================= !
         END DO                                       !    tracer loop    !
         !                                            ! ================= !
      END_1D                                          !  i-k slices loop  !
      !                                               ! ================= !
   END SUBROUTINE tra_zdf_imp

   !!==============================================================================
END MODULE trazdf
