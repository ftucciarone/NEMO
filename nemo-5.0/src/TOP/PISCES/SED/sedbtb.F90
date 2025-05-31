MODULE sedbtb
   !!======================================================================
   !!              ***  MODULE  sedbtb  ***
   !!    Sediment : bioturbation of the solid components
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedmat  ! linear system of equations
   USE sedini
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_btb


CONTAINS
   
   SUBROUTINE sed_btb( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sed_btb  ***
      !!
      !! ** Purpose :  performs bioturbation of the solid sediment components
      !!
      !! ** Method  :  ``diffusion'' of solid sediment components. 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) F90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT(in)  :: kt   ! time step
      ! * local variables
      INTEGER :: ji, jk, js
      REAL(wp), DIMENSION(jpoce,jpksed,jpsol) ::  zrearat  !   solution
      REAL(wp), DIMENSION(jpoce)  :: zsumtot
      REAL(wp) :: zsolid1, zlimo2, zfact
      REAL(wp) :: zsolid2, zsolid3, zsolid4, zsolid5, zsolid6
      !------------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_btb')

      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_btb : bioturbation of solid and adsorbed species  '
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF

      ! Initializations
      !----------------
      zrearat(:,:,1:jspoc1-1) = 0.

      ! Remineralization rates of the different POC pools
      DO jk=2, jpksed
         DO ji = 1, jpoce
            zrearat(ji,jk,jspoc1) = -reac_poc1 * xlabil(ji)
            zrearat(ji,jk,jspoc2) = -reac_poc2 * xlabil(ji)
            zrearat(ji,jk,jspoc3) = -reac_poc3 * xlabil(ji)
            zrearat(ji,jk,jspoc4) = -reac_poc4 * xlabil(ji)
            zrearat(ji,jk,jspoc5) = -reac_poc5 * xlabil(ji)
            zrearat(ji,jk,jspoc6) = -reac_poc6 * xlabil(ji)
            zsolid1 = volc(ji,jk,jspoc1) * solcp(ji,jk,jspoc1)
            zsolid2 = volc(ji,jk,jspoc2) * solcp(ji,jk,jspoc2)
            zsolid3 = volc(ji,jk,jspoc3) * solcp(ji,jk,jspoc3)
            zsolid4 = volc(ji,jk,jspoc4) * solcp(ji,jk,jspoc4)
            zsolid5 = volc(ji,jk,jspoc5) * solcp(ji,jk,jspoc5)
            zsolid6 = volc(ji,jk,jspoc6) * solcp(ji,jk,jspoc6)
            rearatpom(ji,jk)  = reac_poc1 * zsolid1 + reac_poc2 * zsolid2 + reac_poc3 * zsolid3    &
            &          + reac_poc4 * zsolid4 + reac_poc5 * zsolid5 + reac_poc6 * zsolid6
            rearatpom(ji,jk)  = rearatpom(ji,jk) * xlabil(ji)
         END DO
      END DO

      zsumtot(:) = 0.
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            zsumtot(ji) = zsumtot(ji) + rearatpom(ji,jk) * volw3d(ji,jk) * ryear
         END DO
      END DO

         !    4/ Computation of the bioturbation coefficient
         !       This parameterization is taken from Archer et al. (2002)
         ! --------------------------------------------------------------
      DO ji = 1, jpoce
         zlimo2   = max(0.01, pwcp(ji,1,jwoxy) / (pwcp(ji,1,jwoxy) + 20.E-6) )
         db(ji,1) = dbiot * zsumtot(ji)**0.85 * zlimo2 / ryear
         IF (ln_irrig) THEN
            irrig(ji,2) = 0.33 * ( 1.0 - 0.99315 * exp( -0.55127 * zsumtot(ji) ) ) * zlimo2 / 86400.
         ENDIF
      END DO

      ! ------------------------------------------------------
      !    Vertical variations of the bioturbation coefficient
      ! ------------------------------------------------------
      IF (ln_btbz) THEN
         DO jk = 2, jpksed
            zfact = exp( -(profsedw(jk) / dbtbzsc)**2 )
            DO ji = 1, jpoce
               db(ji,jk) = db(ji,1) * zfact
            END DO
         END DO
      ELSE
         DO jk = 2, jpksed
            IF (profsedw(jk) <= dbtbzsc) THEN
               db(:,jk) = db(:,1)
            ELSE
               db(:,jk) = 0.0
            ENDIF
         END DO
      ENDIF

      ! Computation of the bioirrigation factor (from Archer, MUDS model)
      IF (ln_irrig) THEN
         DO jk = 2, jpksed
            zfact = exp( -(profsed(jk) / xirrzsc) )
            DO ji = 1, jpoce
               irrig(ji,jk) = irrig(ji,2) * zfact
            END DO
         END DO
      ELSE
         irrig(:,:) = 0.0
      ENDIF

      ! Compute bioturbation of the slow solid species
      !-----------------------------------------------
      CALL sed_mat_btbe( jpsol, solcp, zrearat(:,:,:), dtsed )

      DO jk=2, jpksed
         DO ji = 1, jpoce
            zsolid1 = volc(ji,jk,jspoc1) * solcp(ji,jk,jspoc1)
            zsolid2 = volc(ji,jk,jspoc2) * solcp(ji,jk,jspoc2)
            zsolid3 = volc(ji,jk,jspoc3) * solcp(ji,jk,jspoc3)
            zsolid4 = volc(ji,jk,jspoc4) * solcp(ji,jk,jspoc4)
            zsolid5 = volc(ji,jk,jspoc5) * solcp(ji,jk,jspoc5)
            zsolid6 = volc(ji,jk,jspoc6) * solcp(ji,jk,jspoc6)
            rearatpom(ji,jk)  = reac_poc1 * zsolid1 + reac_poc2 * zsolid2 + reac_poc3 * zsolid3    &
            &          + reac_poc4 * zsolid4 + reac_poc5 * zsolid5 + reac_poc6 * zsolid6
            rearatpom(ji,jk)  = rearatpom(ji,jk) * xlabil(ji)
         END DO
      END DO

      IF( ln_timing )  CALL timing_stop('sed_btb')

   END SUBROUTINE sed_btb

END MODULE sedbtb
