MODULE sedfunc
   !!======================================================================
   !!              ***  MODULE  sedfunc  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!    Diffusion of solutes in pore water
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedini
   USE seddsr
   USE sedmat
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_func

   !! * Module variables

CONTAINS
   
   SUBROUTINE sed_func(  NEQ, X, fval0, accmask ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_func  ***
      !! 
      !!  ** Purpose :  computes pore water diffusion and reactions
      !!                This is done only for species involved in fast
      !!                reactions for which is Rosebrock temporal scheme 
      !!                is used (see trosk.F90)
      !!
      !!  ** Method :   Computation of the redox and dissolution reactions 
      !!                in the sediment.
      !!                The main redox reactions are solved in sed_dsr whereas
      !!                the secondary reactions are solved in sed_dsr_redoxb.
      !!
      !!   History :
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) :: NEQ
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      REAL, DIMENSION(jpoce,NEQ), INTENT(in) :: X
      REAL, DIMENSION(jpoce,NEQ), INTENT(out) :: fval0
      ! --- local variables
      INTEGER  :: ji, jk, js, jn   ! dummy looop indices
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_func')
!

      pwcpa(:,1,jwalk) = 0.0
      pwcpa(:,1,jwpo4) = 0.0
      DO jn = 1, jpvode
         js = jsvode(jn)
         DO ji = 1, jpoce
            IF ( accmask(ji) == 0 ) THEN
               IF ( js <= jpwat ) THEN
                  pwcpa(ji,1,js) = 0.0_wp
               ELSE
                  solcpa(ji,1,js-jpwat) = 0.0_wp
               ENDIF
            ENDIF
         END DO
      END DO

      ! Unpack variables to standard format
      DO jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         DO ji = 1, jpoce
            IF ( accmask(ji) == 0 ) THEN
               IF (js <= jpwat) THEN
                  pwcp(ji,jk,js) = X(ji,jn) * 1E-6 
               ELSE
                  solcp(ji,jk,js-jpwat) = X(ji,jn) * 1E-6
               ENDIF
            ENDIF
         END DO
      END DO

      CALL sed_dsr( accmask )        ! Redox reactions
      ! Computes diffusive and bioirrigation fluxes
      DO jn = 1, jpvode
         js = jsvode(jn)
         IF (js <= jpwat) THEN
            CALL sed_mat_dsr( js, accmask )
         ENDIF
      END DO

      ! Bioturbation of adsorbed species (Fe2+ and NH4)
      call sed_mat_ads( jwnh4, accmask )
      call sed_mat_ads( jwfe2, accmask )

      ! Bioturbation of solid species
      call sed_mat_btb( jsfeo, accmask )
      call sed_mat_btb( jsfes, accmask )

      ! Repack variables to be used by the Rosenbrock scheme
      DO jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         DO ji = 1, jpoce
            IF ( accmask(ji) == 0 ) THEN
               IF (js <= jpwat) THEN
                  fval0(ji,jn) = pwcpa(ji,jk,js)  * 1E6
               ELSE
                  fval0(ji,jn) = solcpa(ji,jk,js-jpwat) * 1E6
               ENDIF
            ENDIF
         END DO 
      END DO

      IF( ln_timing )  CALL timing_stop('sed_func')
!      
   END SUBROUTINE sed_func

END MODULE sedfunc
