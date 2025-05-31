MODULE stprk3
   !!======================================================================
   !!                       ***  MODULE stpRK3  ***
   !! Time-stepping   : manager of the ocean, tracer and ice time stepping
   !!                   using a 3rd order Rung Kuta  with fixed or quasi-eulerian coordinate
   !!======================================================================
   !! History :  4.5  !  2021-01  (S. Techene, G. Madec, N. Ducousso, F. Lemarie)  Original code
   !!   NEMO     
   !!----------------------------------------------------------------------
#if defined key_qco   ||   defined key_linssh
   !!----------------------------------------------------------------------
   !!   'key_qco'                        Quasi-Eulerian vertical coordinate
   !!                          OR
   !!   'key_linssh                       Fixed in time vertical coordinate
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   stp_RK3       : NEMO 3rd order Runge-Kutta time-stepping 
   !!----------------------------------------------------------------------
   USE step_oce       ! time stepping used modules
   USE trd_oce        ! trends: ocean variables
   USE domqco         ! quasi-eulerian coordinate      (dom_qco_r3c routine)
   USE stprk3_stg     ! RK3 stages
   USE stp2d          ! external mode solver

   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp_RK3   ! called by nemogcm.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_agrif
   RECURSIVE SUBROUTINE stp_RK3( )
      INTEGER             ::   kstp   ! ocean time-step index
#else
   SUBROUTINE stp_RK3( kstp )
      INTEGER, INTENT(in) ::   kstp   ! ocean time-step index
#endif
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp_RK3  ***
      !!
      !! ** Purpose : - Time stepping of OCE  (momentum and active tracer Eqs.) (RK3)
      !!              - Time stepping of SI3 (dynamic and thermodynamic Eqs.)   (FBS)
      !!              - Time stepping of TRC  (passive tracer Eqs.)
      !!
      !! ** Method  : -1- Update forcings and data
      !!              -2- Update ocean physics
      !!              -3- Compute the after (Naa) ssh and velocity 
      !!              -4- diagnostics and output at Now (Nnn)
      !!              -4- Compute the after (Naa) T-S
      !!              -5- Update now 
      !!              -6- Update the horizontal velocity
      !!              -7- Compute the diagnostics variables (rd,N2, hdiv,w)
      !!              -8- Outputs and diagnostics
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk, jtile   ! dummy loop indice
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   zgdept
      !! ---------------------------------------------------------------------
#if defined key_agrif
      IF( nstop > 0 )   RETURN   ! avoid to go further if an error was detected during previous time step (child grid)
      kstp = nit000 + Agrif_Nb_Step()
      Kbb_a = Nbb   ; Kmm_a = Nbb   ;   Krhs_a = Nrhs   ! agrif_oce module copies of time level indices
      IF( lk_agrif_debug ) THEN
         IF( Agrif_Root() .AND. lwp)   WRITE(*,*) '---'
         IF(lwp)   WRITE(*,*) 'Grid Number', Agrif_Fixed(),' time step ', kstp, 'int tstep', Agrif_NbStepint()
      ENDIF
      IF( kstp == nit000 + 1 )   lk_agrif_fstep = .FALSE.
# if defined key_xios
      IF( Agrif_Nbstepint() == 0 )   CALL iom_swap( cxios_context )
# endif
#endif
      !
      IF( ln_timing )   CALL timing_start('stp_RK3')
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! update I/O and calendar
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      IF( kstp == nit000 ) THEN                       ! initialize IOM context (must be done after nemo_init for AGRIF+XIOS+OASIS)
                             CALL iom_init( cxios_context, ld_closedef=.FALSE. )   ! for model grid (including possible AGRIF zoom)
                             CALL iom_init_closedef
      ENDIF

      IF( kstp /= nit000 )   CALL day( kstp )         ! Calendar (day was already called at nit000 in day_init)
                             CALL iom_setkt( kstp - nit000 + 1,      cxios_context          )   ! tell IOM we are at time step kstp

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Update external forcing (tides, open boundaries, ice shelf interaction and surface boundary condition (including sea-ice)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         CALL sbc        ( kstp, Nbb, Nbb )                ! Sea Boundary Condition (including sea-ice)

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Ocean physics update
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  RK3 : single first external mode computation
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      CALL stp_2D( kstp, Nbb, Nbb, Naa, Nrhs )         ! out: ssh, (uu_b,vv_b) at Naa and (un_adv,vn_adv) between Nbb and Naa
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  RK3 time integration
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      ! Stage 1 :
      CALL stp_RK3_stg( 1, kstp, Nbb, Nbb, Nrhs, Naa )
      !
      Nrhs = Nnn   ;   Nnn  = Naa   ;   Naa  = Nrhs    ! Swap: Nbb unchanged, Nnn <==> Naa
      !
      ! Stage 2 :
      CALL stp_RK3_stg( 2, kstp, Nbb, Nnn, Nrhs, Naa )
      !
      Nrhs = Nnn   ;   Nnn  = Naa   ;   Naa  = Nrhs    ! Swap: Nbb unchanged, Nnn <==> Naa
      !
      ! Stage 3 :
      CALL stp_RK3_stg( 3, kstp, Nbb, Nnn, Nrhs, Naa )
      !
      Nrhs = Nbb   ;   Nbb  = Naa   ;   Naa  = Nrhs    ! Swap: Nnn unchanged, Nbb <==> Naa

      ! linear extrapolation of ssh to compute ww at the beginning of the next time-step
      ! ssh(n+1) = 2*ssh(n) - ssh(n-1)    
      ssh(:,:,Naa) = 2*ssh(:,:,Nbb) - ssh(:,:,Naa)
      !!st: ssh recomputed at the begining of stp2d

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! diagnostics and outputs
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      CALL dia_wri   ( kstp,      Nbb )      ! ocean model: outputs

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! File manipulation at the end of the first time step
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      IF( kstp == nit000 ) THEN                          ! 1st time step only
                                        CALL iom_close( numror )   ! close input  ocean restart file
         IF( lrxios )                   CALL iom_context_finalize( cr_ocerst_cxt )
         IF(lwm)                        CALL FLUSH    ( numond )   ! flush output namelist oce
         IF(lwm .AND. numoni /= -1 )    CALL FLUSH    ( numoni )   ! flush output namelist ice (if exist)
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
      IF( ln_timing )   CALL timing_stop('stp_RK3')
      !
   END SUBROUTINE stp_RK3

#else
   !!----------------------------------------------------------------------
   !!   default option             EMPTY MODULE           qco not activated
   !!----------------------------------------------------------------------
#endif
   
   !!======================================================================
END MODULE stprk3
