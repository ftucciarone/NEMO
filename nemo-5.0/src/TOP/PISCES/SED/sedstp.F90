MODULE sedstp
   !!======================================================================
   !!                       ***  MODULE sedstp   ***
   !!   Sediment model : Sediment model time-stepping
   !!======================================================================
   USE sed      ! sediment global variables
   USE seddta   ! data read
   USE sedchem  ! chemical constant
   USE sedsol   ! Organic reactions and diffusion
   USE sedadv   ! vertical advection
   USE sedsfc   ! sediment surface data
   USE sedrst   ! restart
   USE sedwri   ! outputs
   USE sedini
   USE trcdmp_sed
   USE lib_mpp         ! distribued memory computing library
   USE iom

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_stp  ! called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"

CONTAINS

   SUBROUTINE sed_stp ( kt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_stp  ***
      !!
      !! ** Purpose :   Sediment time stepping
      !!                Simulation of pore water chemistry
      !!
      !! ** Action  :
      !!
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) coupled with PISCES
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt       ! number of iteration
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      !!----------------------------------------------------------------------
      IF( ln_timing )           CALL timing_start('sed_stp')
        !
                                CALL sed_rst_opn  ( kt )       ! Open tracer restart file 
      IF( lrst_sed )            CALL sed_rst_cal  ( kt, 'WRITE' )   ! calenda

      dtsed  = rDt_trc
      IF (kt /= nitsed000)      CALL sed_dta( kt, Kbb, Kmm )    ! Load  Data for bot. wat. Chem and fluxes

      IF( kt /= nitsed000 )  &
        &  CALL sed_chem( kt )      ! update of chemical constant to account for salinity, temperature changes
           CALL sed_sol( kt )       ! Solute diffusion and reactions 
           CALL sed_adv( kt )       ! advection

      IF (ln_sed_2way) CALL sed_sfc( kt, Kbb )   ! Give back new bottom wat chem to tracer model

                            CALL sed_wri( kt )         ! outputs
      IF( kt == nitsed000 ) CALL iom_close( numrsr )       ! close input tracer restart file

      IF( lrst_sed )        CALL sed_rst_wri( kt )   ! restart file output
      IF( kt == nitsedend )     CLOSE( numsed )

      IF( ln_timing )           CALL timing_stop('sed_stp')

   END SUBROUTINE sed_stp

END MODULE sedstp
