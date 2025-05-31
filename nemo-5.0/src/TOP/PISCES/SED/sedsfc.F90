MODULE sedsfc
   !!======================================================================
   !!              ***  MODULE  sedsfc  ***
   !!    Sediment : Data at sediment surface
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedini
   USE seddta

   IMPLICIT NONE

   PUBLIC sed_sfc

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"

CONTAINS

   SUBROUTINE sed_sfc( kt, Kbb )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_sfc ***
      !!
      !! ** Purpose :  Give data from sediment model to tracer model
      !!
      !!
      !!   History :
      !!        !  06-04 (C. Ethe)  Orginal code
      !!----------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) ::  kt              ! time step
      INTEGER, INTENT(in) ::  Kbb             ! time level indices

      ! * local variables
      INTEGER :: ji, jj, ikt     ! dummy loop indices

      !------------------------------------------------------------------------
      ! reading variables

      IF( ln_timing )  CALL timing_start('sed_sfc')

      trc_data(:,:,1) = UNPACK( pwcp(:,1,jwalk), sedmask == 1.0, 0.0 )
      trc_data(:,:,2) = UNPACK( pwcp(:,1,jwdic), sedmask == 1.0, 0.0 )
      trc_data(:,:,3) = UNPACK( pwcp(:,1,jwno3), sedmask == 1.0, 0.0 )
      trc_data(:,:,4) = UNPACK( pwcp(:,1,jwpo4), sedmask == 1.0, 0.0 )
      trc_data(:,:,5) = UNPACK( pwcp(:,1,jwoxy), sedmask == 1.0, 0.0 )
      trc_data(:,:,6) = UNPACK( pwcp(:,1,jwsil), sedmask == 1.0, 0.0 )
      trc_data(:,:,7) = UNPACK( pwcp(:,1,jwnh4), sedmask == 1.0, 0.0 )
      trc_data(:,:,8) = UNPACK( pwcp(:,1,jwfe2), sedmask == 1.0, 0.0 )
      trc_data(:,:,9) = UNPACK( pwcp(:,1,jwlgw), sedmask == 1.0, 0.0 )

      DO_2D( 0, 0, 0, 0 )
         ikt = mbkt(ji,jj)
         IF ( tmask(ji,jj,ikt) == 1 ) THEN
            tr(ji,jj,ikt,jptal,Kbb) = trc_data(ji,jj,1)
            tr(ji,jj,ikt,jpdic,Kbb) = trc_data(ji,jj,2)
            tr(ji,jj,ikt,jpno3,Kbb) = trc_data(ji,jj,3) * redC / redNo3
            tr(ji,jj,ikt,jppo4,Kbb) = trc_data(ji,jj,4) * redC / redPo4
            tr(ji,jj,ikt,jpoxy,Kbb) = trc_data(ji,jj,5)
            tr(ji,jj,ikt,jpsil,Kbb) = trc_data(ji,jj,6)
            tr(ji,jj,ikt,jpnh4,Kbb) = trc_data(ji,jj,7) * redC / redNo3
            tr(ji,jj,ikt,jpfer,Kbb) = trc_data(ji,jj,8)
         ENDIF
      END_2D

      IF( ln_timing )  CALL timing_stop('sed_sfc')

   END SUBROUTINE sed_sfc

END MODULE sedsfc
