MODULE trdpen
   !!======================================================================
   !!                       ***  MODULE  trdpen  ***
   !! Ocean diagnostics:  Potential Energy trends
   !!=====================================================================
   !! History :  3.5  !  2012-02  (G. Madec) original code 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   trd_pen       : compute and output Potential Energy trends from T & S trends
   !!   trd_pen_init  : initialisation of PE trends
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean domain 
   USE sbc_oce        ! surface boundary condition: ocean
   USE zdf_oce        ! ocean vertical physics
   USE trd_oce        ! trends: ocean variables
   USE eosbn2         ! equation of state and related derivatives
   USE ldftra         ! lateral diffusion: eddy diffusivity & EIV coeff.
   USE zdfddm         ! vertical physics: double diffusion
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trd_pen        ! called by all trdtra module
   PUBLIC   trd_pen_init   ! called by all nemogcm module

   INTEGER ::   nkstp   ! current time step 

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   rab_pe   ! partial derivatives of PE anomaly with respect to T and S

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   INTEGER FUNCTION trd_pen_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION trd_tra_alloc  ***
      !!---------------------------------------------------------------------
      ALLOCATE( rab_pe(T2D(0),jpk,jpts) , STAT= trd_pen_alloc )
      !
      CALL mpp_sum ( 'trdpen', trd_pen_alloc )
      IF( trd_pen_alloc /= 0 )   CALL ctl_stop( 'STOP',  'trd_pen_alloc: failed to allocate arrays'  )
   END FUNCTION trd_pen_alloc


   SUBROUTINE trd_pen( ptrdx, ptrdy, ktrd, kt, pdt, Kmm )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_tra_mng  ***
      !! 
      !! ** Purpose :   Dispatch all trends computation, e.g. 3D output, integral
      !!                constraints, barotropic vorticity, kinetic enrgy, 
      !!                potential energy, and/or mixed layer budget.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(T2D(0),jpk), INTENT(in) ::   ptrdx, ptrdy   ! Temperature & Salinity trends
      INTEGER                        , INTENT(in) ::   ktrd           ! tracer trend index
      INTEGER                        , INTENT(in) ::   kt             ! time step index
      INTEGER                        , INTENT(in) ::   Kmm            ! time level index
      REAL(wp)                       , INTENT(in) ::   pdt            ! time step [s]
      !
      INTEGER ::   ji, jj, jk                                            ! dummy loop indices
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)  ::   z2d            ! 2D workspace 
      REAL(wp), DIMENSION(T2D(0),jpk)        ::   zpe            ! 3D workspace
      !!----------------------------------------------------------------------
      !
      zpe(:,:,:) = 0._wp
      !
      IF( kt /= nkstp ) THEN     ! full eos: set partial derivatives at the 1st call of kt time step
         nkstp = kt
         CALL eos_pen( ts(:,:,:,:,Kmm), rab_pe, zpe, Kmm )
         CALL iom_put( "alphaPE", rab_pe(:,:,:,jp_tem) )
         CALL iom_put( "betaPE" , rab_pe(:,:,:,jp_sal) )
         CALL iom_put( "PEanom" , zpe )
      ENDIF
      !
      zpe(:,:,jpk) = 0._wp
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         zpe(ji,jj,jk) = ( - ( rab_n(ji,jj,jk,jp_tem) + rab_pe(ji,jj,jk,jp_tem) ) * ptrdx(ji,jj,jk)   &
            &              + ( rab_n(ji,jj,jk,jp_sal) + rab_pe(ji,jj,jk,jp_sal) ) * ptrdy(ji,jj,jk)  )
      END_3D

      SELECT CASE ( ktrd )
      CASE ( jptra_xad  )   ;   CALL iom_put( "petrd_xad", zpe )   ! zonal    advection
      CASE ( jptra_yad  )   ;   CALL iom_put( "petrd_yad", zpe )   ! merid.   advection
      CASE ( jptra_zad  )   ;   CALL iom_put( "petrd_zad", zpe )   ! vertical advection
                                IF( lk_linssh ) THEN                   ! cst volume : adv flux through z=0 surface
                                   ALLOCATE( z2d(T2D(0)) )
                                   DO_2D( 0, 0, 0, 0 )
                                      z2d(ji,jj) = ww(ji,jj,1) * ( &
                                        &   - ( rab_n(ji,jj,1,jp_tem) + rab_pe(ji,jj,1,jp_tem) ) * ts(ji,jj,1,jp_tem,Kmm)    &
                                        &   + ( rab_n(ji,jj,1,jp_sal) + rab_pe(ji,jj,1,jp_sal) ) * ts(ji,jj,1,jp_sal,Kmm)    &
                                        & ) / e3t(ji,jj,1,Kmm)
                                   END_2D
                                   CALL iom_put( "petrd_sad" , z2d )
                                   DEALLOCATE( z2d )
                                ENDIF
      CASE ( jptra_ldf  )   ;   CALL iom_put( "petrd_ldf" , zpe )   ! lateral  diffusion
      CASE ( jptra_zdf  )   ;   CALL iom_put( "petrd_zdf" , zpe )   ! lateral  diffusion (K_z)
      CASE ( jptra_zdfp )   ;   CALL iom_put( "petrd_zdfp", zpe )   ! vertical diffusion (K_z)
      CASE ( jptra_dmp  )   ;   CALL iom_put( "petrd_dmp" , zpe )   ! internal 3D restoring (tradmp)
      CASE ( jptra_bbl  )   ;   CALL iom_put( "petrd_bbl" , zpe )   ! bottom boundary layer
      CASE ( jptra_npc  )   ;   CALL iom_put( "petrd_npc" , zpe )   ! non penetr convect adjustment
      CASE ( jptra_nsr  )   ;   CALL iom_put( "petrd_nsr" , zpe )   ! surface forcing + runoff (ln_rnf=T)
      CASE ( jptra_qsr  )   ;   CALL iom_put( "petrd_qsr" , zpe )   ! air-sea : penetrative sol radiat
      CASE ( jptra_bbc  )   ;   CALL iom_put( "petrd_bbc" , zpe )   ! bottom bound cond (geoth flux)
      CASE ( jptra_atf  )   ;   CALL iom_put( "petrd_atf" , zpe )   ! asselin time filter (last trend)
         !
      END SELECT
      !
      !
   END SUBROUTINE trd_pen


   SUBROUTINE trd_pen_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_pen_init  ***
      !! 
      !! ** Purpose :   initialisation of 3D Kinetic Energy trend diagnostic
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trd_pen_init : 3D Potential ENergy trends'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF
      !                           ! allocate box volume arrays
      IF ( trd_pen_alloc() /= 0 )   CALL ctl_stop('trd_pen_alloc: failed to allocate arrays')
      !
      rab_pe(:,:,:,:) = 0._wp
      !
      IF( .NOT.lk_linssh )   CALL ctl_stop('trd_pen_init : PE trends not coded for variable volume')
      !
      nkstp     = nit000 - 1
      !
   END SUBROUTINE trd_pen_init

   !!======================================================================
END MODULE trdpen
