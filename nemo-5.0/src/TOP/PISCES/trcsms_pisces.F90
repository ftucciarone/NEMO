MODULE trcsms_pisces
   !!======================================================================
   !!                         ***  MODULE trcsms_pisces  ***
   !! TOP :   PISCES Source Minus Sink manager
   !!         This module is for LOBSTER, PISCES and PISCES-QUOTA
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
   !!   trcsms_pisces        :  Time loop of passive tracers sms
   !!----------------------------------------------------------------------
   USE par_pisces
   USE sms_pisces
   USE p4zsms

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms_pisces    ! called in trcsms.F90
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sms_pisces( kt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms_pisces  ***
      !!
      !! ** Purpose :   Managment of the call to Biological sources and sinks 
      !!                routines of PISCES/PISCES-QUOTA or LOBSTER bio-model
      !!
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT( in ) ::   kt               ! ocean time-step index      
      INTEGER, INTENT( in ) ::   Kbb, Kmm, Krhs   ! time level index
      !!---------------------------------------------------------------------
      !
      CALL p4z_sms( kt, Kbb, Kmm, Krhs )   !  PISCES
      !
   END SUBROUTINE trc_sms_pisces

   !!======================================================================
END MODULE trcsms_pisces 
