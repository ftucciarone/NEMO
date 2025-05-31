MODULE trcsink
   !!======================================================================
   !!                         ***  MODULE trcsink  ***
   !! TOP :  vertical flux of particulate matter due to gravitational sinking
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Change aggregation formula
   !!             3.5  !  2012-07  (O. Aumont) Introduce potential time-splitting
   !!             4.0  !  2018-12  (O. Aumont) Generalize the PISCES code to make it usable by any model
   !!             5.0  !  2023-10  (C. Ethe ) Introduce semi-lagragian sinking scheme
   !!----------------------------------------------------------------------
   !!   trc_sink       :  Compute vertical flux of particulate matter due to gravitational sinking
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE lib_mpp
   USE lib_fortran     ! to use sign with key_nosignedzero

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_sink
   PUBLIC trc_sink_ini

   LOGICAL, PUBLIC :: ln_sink_mus    !: MUSCL sinkin scheme
   LOGICAL, PUBLIC :: ln_sink_slg    !: Semi-Lagrangian sinkin scheme
   INTEGER, PUBLIC :: nitermax       !: Maximum number of iterations for sinking ( ln_sink_mus )
   INTEGER, PUBLIC :: nn_sink_lbc    !: Type of boundary conditons for sinking ( ln_sink_slg )

   INTEGER, PARAMETER ::   np_MUS = 1   ! MUSCL sinking scheme 
   INTEGER, PARAMETER ::   np_SLG = 2   ! Semi-Lagrangian sinking scheme

   INTEGER  ::   nsnk     ! user choice of the type of sinking scheme

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!   'standard sinking parameterisation'                  ???
   !!----------------------------------------------------------------------

   SUBROUTINE trc_sink ( kt, Kbb, Kmm, pwsink, psinkflx, jp_tra, rsfact )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sink  ***
      !!
      !! ** Purpose :   Compute vertical flux of particulate matter due to 
      !!                gravitational sinking
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER , INTENT(in)  :: kt
      INTEGER , INTENT(in)  :: Kbb, Kmm
      INTEGER , INTENT(in)  :: jp_tra    ! tracer index index      
      REAL(wp), INTENT(in)  :: rsfact    ! time step duration
      REAL(wp), INTENT(in)   , DIMENSION(A2D(0),jpk) :: pwsink
      REAL(wp), INTENT(inout), DIMENSION(A2D(0),jpk) :: psinkflx
      !
      INTEGER  ::   ji, jj, jk
      INTEGER , ALLOCATABLE, DIMENSION(:,:)   :: iiter
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zwsink
      REAL(wp) ::   zfact, zwsmax, zmax
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sink')
      !
      !

      !                       !----------------------------!
      SELECT CASE( nsnk )     !  Sinking type              !
      !                       !----------------------------!
      CASE( np_MUS )                                     !==  MUSCL sinking scheme  ==!
         !
         ALLOCATE( iiter( A2D(0) ) )    ;     ALLOCATE( zwsink( A2D(0), jpk ) )
         ! OA This is (I hope) a temporary solution for the problem that may 
         ! OA arise in specific situation where the CFL criterion is broken 
         ! OA for vertical sedimentation of particles. To avoid this, a time
         ! OA splitting algorithm has been coded. A specific maximum
         ! OA iteration number is provided and may be specified in the namelist 
         ! OA This is to avoid very large iteration number when explicit free
         ! OA surface is used (for instance). When niter?max is set to 1, 
         ! OA this computation is skipped. The crude old threshold method is 
         ! OA then applied. This also happens when niter exceeds nitermax.
         IF( nitermax == 1 ) THEN
            iiter(:,:) = 1
         ELSE
            DO_2D( 0, 0, 0, 0 )
               iiter(ji,jj) = 1
               DO jk = 1, jpkm1
                  IF( tmask(ji,jj,jk) == 1.0 ) THEN
                      zwsmax =  0.5 * e3t(ji,jj,jk,Kmm) * rday / rsfact
                      iiter(ji,jj) =  MAX( iiter(ji,jj), INT( pwsink(ji,jj,jk) / zwsmax ) + 1 )
                  ENDIF
               END DO
            END_2D
            iiter(:,:) = MIN( iiter(:,:), nitermax )
         ENDIF

         zwsink(:,:,1) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zwsmax = 0.5 * e3t(ji,jj,jk,Kmm) * rday / rsfact
            zwsink(ji,jj,jk+1) = -MIN( pwsink(ji,jj,jk), zwsmax * REAL( iiter(ji,jj), wp ) ) / rday
         END_3D

         !  Initializa to zero all the sinking arrays 
         !  -----------------------------------------
         psinkflx(:,:,:) = 0.e0

         !   Compute the sedimentation term using trc_sink2_mus for the considered sinking particle
         !   -----------------------------------------------------
         CALL trc_sink2_mus( Kbb, Kmm, zwsink, psinkflx, jp_tra, iiter, rsfact )
         !
         DEALLOCATE( iiter )   ;     DEALLOCATE( zwsink ) 
         !
      CASE( np_SLG )                                     !==  Semi-Lagrangian sinking scheme ==!
         !
         !  Initializa to zero all the sinking arrays 
         !  -----------------------------------------
         psinkflx(:,:,:) = 0.e0

         !   Compute the sedimentation term using trc_sink2_slg for the considered sinking particle
         !   -----------------------------------------------------
         CALL trc_sink2_slg( Kbb, Kmm, pwsink, psinkflx, jp_tra, rsfact )
         !
      END SELECT
      !
      IF( ln_timing )   CALL timing_stop('trc_sink')
      !
   END SUBROUTINE trc_sink

   SUBROUTINE trc_sink2_mus( Kbb, Kmm, pwsink, psinkflx, jp_tra, kiter, rsfact )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sink2_mus  ***
      !!
      !! ** Purpose :   Compute the sedimentation terms for the various sinking
      !!     particles. The scheme used to compute the trends is based
      !!     on MUSCL.
      !!
      !! ** Method  : - this ROUTINE compute not exactly the advection but the
      !!      transport term, i.e.  div(u*tra).
      !!---------------------------------------------------------------------
      INTEGER,  INTENT(in   )                        ::   Kbb, Kmm  ! time level indices
      INTEGER,  INTENT(in   )                        ::   jp_tra    ! tracer index index      
      REAL(wp), INTENT(in   )                        ::   rsfact    ! duration of time step
      INTEGER,  INTENT(in   ), DIMENSION(A2D(0))     ::   kiter     ! number of iterations for time-splitting 
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0),jpk) ::   pwsink    ! sinking speed
      REAL(wp), INTENT(inout), DIMENSION(A2D(0),jpk) ::   psinkflx  ! sinking fluxe
      !
      INTEGER  ::   ji, jj, jk, jn, jt
      REAL(wp) ::   zigma,z0w,zign, zflx, zstep, zzwx, zzwy, zalpha
      REAL(wp) ::   ztraz, ztraz_km1
      REAL(wp), DIMENSION(jpk) :: zakz, ztrb, zsinking
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sink2_mus')
      !
      DO_2D( 0, 0, 0, 0 )
         ! Vertical advective flux
         zstep = rsfact / REAL( kiter(ji,jj), wp ) / 2.
         DO jt = 1, kiter(ji,jj)
            zakz (:) = 0.e0
            ztrb (:) = tr(ji,jj,:,jp_tra,Kbb)
            DO jn = 1, 2
               !              
               ztraz_km1 = ( ztrb(1) - ztrb(2) ) * tmask(ji,jj,2)
               DO jk = 2, jpkm1
                  ztraz     = ( ztrb(jk) - ztrb(jk+1) ) * tmask(ji,jj,jk+1)
                  zign      = 0.25 + SIGN( 0.25_wp, ztraz_km1 * ztraz )
                  zakz(jk)  = ( ztraz_km1 + ztraz ) * zign
                  zakz(jk)  = SIGN( 1.0_wp, zakz(jk) ) *        &
                     &        MIN( ABS( zakz(jk) ), 2. * ABS(ztraz), 2. * ABS(ztraz_km1) )
                  ztraz_km1 = ztraz
               END DO
      
               ! vertical advective flux
               zsinking(1) = 0.e0
               DO jk = 1, jpkm1
                  z0w      = SIGN( 0.5_wp, pwsink(ji,jj,jk+1) )
                  zalpha   = 0.5 + z0w 
                  zigma    = z0w - 0.5 * pwsink(ji,jj,jk+1) * zstep / e3w(ji,jj,jk+1,Kmm)
                  zzwx     = ztrb(jk+1) + zigma * zakz(jk+1)
                  zzwy     = ztrb(jk) + zigma * zakz(jk)
                  zsinking(jk+1) = -pwsink(ji,jj,jk+1) * ( zalpha * zzwx + (1.0 - zalpha) * zzwy ) * zstep
                  zflx     = ( zsinking(jk) - zsinking(jk+1) ) / e3t(ji,jj,jk,Kmm)
                  ztrb(jk) = ztrb(jk) + zflx * tmask(ji,jj,jk)
               END DO
            END DO
            DO jk = 1, jpkm1
               zflx = ( zsinking(jk) - zsinking(jk+1) ) / e3t(ji,jj,jk,Kmm)
               tr(ji,jj,jk,jp_tra,Kbb) = tr(ji,jj,jk,jp_tra,Kbb) + 2. * zflx * tmask(ji,jj,jk)
               psinkflx(ji,jj,jk)      = psinkflx(ji,jj,jk) + 2. * zsinking(jk)
            END DO
            psinkflx(ji,jj,jpk) = psinkflx(ji,jj,jpk) + 2. * zsinking(jpk)
         END DO
      END_2D
      !
      IF( ln_timing )  CALL timing_stop('trc_sink2_mus')
      !
   END SUBROUTINE trc_sink2_mus

   SUBROUTINE trc_sink2_slg( Kbb, Kmm, pwsink, psinkflx, jp_tra, rsfact )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sink2_slg  ***
      !!
      !! ** Purpose :   Compute the sedimentation terms for the various sinking particles.
      !!                The scheme used to compute the trends is based on 
      !!                a semi-Lagrangian advective flux algorithm
      !!
      !! ** Method  : - uses a parabolic,  vertical reconstructuion of the suspended particle 
      !!                in  the water column with PPT/WENO constraints to avoid oscillation
      !!                                                                     
      !!  References:                                                         
      !!                                                                      
      !!  Colella, P. and P. Woodward, 1984: The piecewise parabolic method   
      !!    (PPM) for gas-dynamical simulations, J. Comp. Phys., 54, 174-201. 
      !!                                                                      
      !!  Liu, X.D., S. Osher, and T. Chan, 1994: Weighted essentially        
      !!    nonoscillatory shemes, J. Comp. Phys., 115, 200-212.              
      !!                                                                      
      !!  Warner, J.C., C.R. Sherwood, R.P. Signell, C.K. Harris, and H.G.   
      !!    Arango, 2008:  Development of a three-dimensional,  regional,    
      !!    coupled wave, current, and sediment-transport model, Computers   
      !!    & Geosciences, 34, 1284-1306.                                     
      !!---------------------------------------------------------------------

      INTEGER,  INTENT(in   )                        ::   Kbb, Kmm  ! time level indices
      INTEGER,  INTENT(in   )                        ::   jp_tra    ! tracer index index      
      REAL(wp), INTENT(in   ), DIMENSION(A2D(0),jpk) ::   pwsink    ! sinking speed
      REAL(wp), INTENT(inout), DIMENSION(A2D(0),jpk) ::   psinkflx  ! sinking fluxe
      REAL(wp), INTENT(in   )                        ::   rsfact    ! duration of time step
      !
      INTEGER  :: ji, jj, jk, ik
      REAL(wp) :: zcff, zcu, zcffL, zcffR, zdltL, zdltR, zflx
      REAL(wp) :: zHz_inv, zHz_inv2, zHz_inv3
      !
      INTEGER , DIMENSION(A1Di(0),jpk) :: ksource
      REAL(wp), DIMENSION(A1Di(0),jpk+1) :: zFC
      REAL(wp), DIMENSION(A1Di(0),jpk) :: zqR
      REAL(wp), DIMENSION(A1Di(0),jpk) :: zqL
      REAL(wp), DIMENSION(A1Di(0),jpk) :: zWR
      REAL(wp), DIMENSION(A1Di(0),jpk) :: zWL
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sink2_slg')
      !
      zqR(:,:) = 0._wp
      zqL(:,:) = 0._wp
      zWR(:,:) = 0._wp
      zWL(:,:) = 0._wp
      zFC(:,:) = 0._wp

      !-----------------------------------------------------------------------
      !  Vertical sinking of particle concentration
      !-----------------------------------------------------------------------
      DO_1Dj( 0, 0 )                                  !  i-k slices loop  !
         !  Compute semi-Lagrangian flux due to sinking.
         DO_2Dik( 0, 0, 2, jpk, 1 )
            zHz_inv2   = 1._wp / ( e3t(ji,jj,jk,Kmm) + e3t(ji,jj,jk-1,Kmm)  )
            zFC(ji,jk) = ( tr(ji,jj,jk-1,jp_tra,Kbb) - tr(ji,jj,jk,jp_tra,Kbb) ) * zHz_inv2  
         END_2D
         !
         DO_2Dik( 0, 0, 2, jpk-1, 1 )
            !
            zdltR = e3t(ji,jj,jk,Kmm) * zFC(ji,jk)
            zdltL = e3t(ji,jj,jk,Kmm) * zFC(ji,jk+1)
            zcff  = e3t(ji,jj,jk+1,Kmm) + 2. * e3t(ji,jj,jk,kmm) + e3t(ji,jj,jk-1,Kmm)
            zcffR = zcff * zFC(ji,jk)
            zcffL = zcff * zFC(ji,jk+1)
            !
            !  Apply PPM monotonicity constraint to prevent oscillations within the grid box.
            IF( zdltR * zdltL <= 0._wp ) THEN
                zdltR = 0._wp
                zdltL = 0._wp
            ELSE IF( ABS( zdltR ) >= zcffL ) THEN
                zdltR = zcffL
            ELSE IF( ABS( zdltL ) > ABS( zcffR ) ) THEN
                zdltL = zcffR
            ENDIF
            !
            !  Compute right and left side values (qR,qL) of parabolic segments
            !  within grid box Hz(k); (WR,WL) are measures of quadratic variations.
            !
            !  NOTE: Although each parabolic segment is monotonic within its grid
            !        box, monotonicity of the whole profile is not guaranteed,
            !        because qL(k+1)-qR(k) may still have different sign than
            !        tr(k+1)-tr(k).  This possibility is excluded, after qL and qR
            !        are reconciled using WENO procedure.
            !
            zHz_inv3   = 1._wp / ( e3t(ji,jj,jk,Kmm) + e3t(ji,jj,jk-1,Kmm) + e3t(ji,jj,jk+1,Kmm) )
            zcff       = ( zdltR - zdltL ) * zHz_inv3
            zdltR      = zdltR - zcff * e3t(ji,jj,jk-1,Kmm)
            zdltL      = zdltL + zcff * e3t(ji,jj,jk+1,Kmm)
            zqR(ji,jk) = tr(ji,jj,jk,jp_tra,Kbb) + zdltR
            zqL(ji,jk) = tr(ji,jj,jk,jp_tra,Kbb) - zdltL
            zWR(ji,jk) = ( 2._wp * zdltR - zdltL )**2
            zWL(ji,jk) = ( zdltR - 2._wp * zdltL )**2
         END_2D
         !
         zcff = 1.e-14
         DO_2Dik( 0, 0, 2, jpk-2, 1 )
            zdltL        = MAX( zcff, zWL(ji,jk  ))
            zdltR        = MAX( zcff, zWR(ji,jk-1))
            zqR(ji,jk)   = ( zdltR * zqR(ji,jk) + zdltL * zqL(ji,jk-1) ) / ( zdltR + zdltL )
            zqL(ji,jk-1) = zqR(ji,jk)
         END_2D
         !
         SELECT CASE( nn_sink_lbc )     
      
         CASE( 1 )         !  linear continuation
            DO_1Di( 0, 0 )                                  
               zFC(ji,1)   = 0.              ! no-flux boundary condition
               !
               zqL(ji,1)   = zqR(ji,2)
               zqR(ji,1)   = 2._wp * tr(ji,jj,1,jp_tra,Kbb) - zqL(ji,1)
               !
               zqR(ji,jpk) = zqL(ji,jpk-1)
               zqL(ji,jpk) = 2._wp * tr(ji,jj,jk,jp_tra,Kbb) - zqR(ji,jpk)
            END_1D
         CASE( 2 )         !  Neumann conditions
            DO_1Di( 0, 0 )                                  
               zFC(ji,1)   = 0.              ! no-flux boundary condition
               !
               zqL(ji,1) = zqR(ji,2)
               zqR(ji,1) = 1.5_wp * tr(ji,jj,1,jp_tra,Kbb) - 0.5_wp * zqL(ji,1)
               !
               zqR(ji,jpk) = zqL(ji,jpk-1)
               zqL(ji,jpk) = 1.5_wp * tr(ji,jj,jk,jp_tra,Kbb) - 0.5_wp * zqR(ji,jpk)
            END_1D
         CASE DEFAULT    !  default strictly monotonic conditions
            DO_1Di( 0, 0 )                                  
               zFC(ji,1)=0.              ! no-flux boundary condition
               !
               zqR(ji,1) = tr(ji,jj,1,jp_tra,Kbb)         ! default strictly monotonic
               zqL(ji,1) = tr(ji,jj,1,jp_tra,Kbb)         ! conditions
               zqR(ji,2) = tr(ji,jj,1,jp_tra,Kbb)
               !
               ik =  mbkt(ji,jj)
               IF( ik > 1 ) THEN
                  zqR(ji,ik  ) = tr(ji,jj,ik,jp_tra,Kbb)               
                  zqL(ji,ik-1) = tr(ji,jj,ik,jp_tra,Kbb)  ! bottom grid boxes are re-assumed to be piecewise constant.
                  zqL(ji,ik  ) = tr(ji,jj,ik,jp_tra,Kbb)           
               ENDIF
            END_1D
         END SELECT
!
         !  Apply monotonicity constraint again, since the reconciled interfacial
         !  values may cause a non-monotonic behavior of the parabolic segments
         !  inside the grid box.
         DO_2Dik( 0, 0, 1, jpk, 1 )
            zdltR = zqR(ji,jk) - tr(ji,jj,jk,jp_tra,Kbb)
            zdltL = tr(ji,jj,jk,jp_tra,Kbb) - zqL(ji,jk)
            zcffR = 2._wp * zdltR
            zcffL = 2._wp * zdltL
            IF( zdltR * zdltL < 0._wp ) THEN
               zdltR = 0._wp
               zdltL = 0._wp
            ELSE IF( ABS( zdltR ) > ABS( zcffL ) ) THEN
               zdltR = zcffL
            ELSE IF( ABS( zdltL ) > ABS( zcffR ) ) THEN
               zdltL = zcffR
            ENDIF
            zqR(ji,jk) = tr(ji,jj,jk,jp_tra,Kbb) + zdltR
            zqL(ji,jk) = tr(ji,jj,jk,jp_tra,Kbb) - zdltL
         END_2D

         !  After this moment reconstruction is considered complete. The next
         !  stage is to compute vertical advective fluxes, FC. It is expected
         !  that sinking may occurs relatively fast, the algorithm is designed
         !  to be free of CFL criterion, which is achieved by allowing
         !  integration bounds for semi-Lagrangian advective flux to use as
         !  many grid boxes in upstream direction as necessary.

         !  In the two code segments below, WL is the z-coordinate of the
         !  departure point for grid box interface z_w with the same indices;
         !  FC is the finite volume flux; ksource(:,k) is index of vertical
         !  grid box which contains the departure point (restricted by N(ng)).
         !  During the search: also add in content of whole grid boxes
         !  participating in FC.

         DO_2Dik( 0, 0, 1, jpk-1, 1 )
            zcff           = rsfact * ABS( pwsink(ji,jj,jk) ) / rday * tmask(ji,jj,jk)
            zFC(ji,jk+1)   = 0._wp
            zWL(ji,jk)     = -gdepw(ji,jj,jk+1,Kmm) + zcff 
            zWR(ji,jk)     = e3t(ji,jj,jk,Kmm) * tr(ji,jj,jk,jp_tra,Kbb)
            ksource(ji,jk) = jk
         END_2D

         DO jk = 1, jpk
            DO ik = 2, jk
               DO_1Di( 0, 0 )                                  !  i- loop
                  IF( zWL(ji,jk) > -gdepw(ji,jj,ik,Kmm) ) THEN
                     ksource(ji,jk) = ik - 1
                     zFC(ji,jk+1)   = zFC(ji,jk+1) + zWR(ji,ik)
                  ENDIF
               END_1D
            ENDDO
         END DO
         !
         !  Finalize computation of flux: add fractional part.
         !
         DO_2Dik( 0, 0, 1, jpk-1, 1 )
            ik           = ksource(ji,jk)
            zHz_inv      = 1._wp / e3t(ji,jj,jk,Kmm)
            zcu          = MIN( 1._wp, ( zWL(ji,jk) + gdepw(ji,jj,ik+1,Kmm) ) * zHz_inv )
            zFC(ji,jk+1) = zFC(ji,jk+1)                                  & 
               &         + e3t(ji,jj,ik,Kmm) * zcu                      &
               &         * ( zqL(ji,ik) + zcu                           &
               &               * ( 0.5_wp * ( zqR(ji,ik) - zqL(ji,ik) ) &
               &                    - ( 1.5_wp - zcu ) * ( zqR(ji,ik) + zqL(ji,ik) - 2._wp * tr(ji,jj,ik,jp_tra,Kbb) ) ) ) 
         END_2D
         !
         DO_2Dik( 0, 0, 1, jpk-1, 1 )
            zHz_inv = 1._wp / e3t(ji,jj,jk,Kmm)
            zflx    = ( zFC(ji,jk) - zFC(ji,jk+1) ) * zHz_inv
            tr(ji,jj,jk,jp_tra,Kbb) = tr(ji,jj,jk,jp_tra,Kbb) + zflx
            psinkflx(ji,jj,jk)      = zFC(ji,jk)
         END_2D
         !
      END_1D
      !
      IF( ln_timing )  CALL timing_stop('trc_sink2_slg')
      !
   END SUBROUTINE trc_sink2_slg


  SUBROUTINE trc_sink_ini
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trc_sink_ini ***
      !!
      !! ** Purpose :   read  namelist options 
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio, ios   ! Local integer output status for namelist read
      !!
      NAMELIST/namtrc_snk/ ln_sink_slg, ln_sink_mus, nitermax, nn_sink_lbc
      !!----------------------------------------------------------------------
      !
      READ_NML_REF(numnat,namtrc_snk)
      READ_NML_CFG(numnat,namtrc_snk)
      IF(lwm) WRITE( numont, namtrc_snk )

      IF(lwp) THEN                     !   ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_sink : Sedimentation of particles '
         WRITE(numout,*) '~~~~~~~ '
         WRITE(numout,*) '   Namelist namtrc_snk : sedimentation of particles'
         WRITE(numout,*) '   Use MUSCL sinking scheme              ln_sink_mus     = ', ln_sink_mus
         WRITE(numout,*) '       Maximum number of iterations          nitermax    = ', nitermax
         WRITE(numout,*) '   Use Semi-Lagrangian sinking scheme    ln_sink_slg     = ', ln_sink_slg
         WRITE(numout,*) '       Type of boundary conditions           nn_sink_lbc = ', nn_sink_lbc
         WRITE(numout,*)
         IF( ln_sink_slg ) THEN
            SELECT CASE( nn_sink_lbc )             ! Type of boundary conditions
            CASE( 1 )    ;  WRITE(numout,*) '   ==>>>   Dirichlet condition : linear continuation' 
            CASE( 2 )    ;  WRITE(numout,*) '   ==>>>   Neumann condition '
            CASE DEFAULT ;  WRITE(numout,*) '   ==>>>   Strictly monotonic conditions'
            END SELECT
         ENDIF
      ENDIF

      ioptio = 0              !**  Parameter control  **!
      IF( ln_sink_mus  )   ioptio = ioptio + 1
      IF( ln_sink_slg  )   ioptio = ioptio + 1
      !
      IF( ioptio /= 1 )   CALL ctl_stop( 'Choose ONE type of sinking scheme namelist namtrc_snk' )
      !
      IF( ln_sink_mus )  nsnk = np_MUS
      IF( ln_sink_slg )  nsnk = np_SLG
      !
   END SUBROUTINE trc_sink_ini

   !!======================================================================
END MODULE trcsink
