MODULE sedjac
   !!======================================================================
   !!              ***  MODULE  sedjac  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!    Diffusion of solutes in pore water
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedini
   USE seddsrjac
   USE sedmat
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_jac

   !! * Module variables

CONTAINS
   
   SUBROUTINE sed_jac( NEQ, X, jacvode, NROWPD, accmask ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_sol  ***
      !! 
      !!  ** Purpose :  computes the jacobian for tracers due to 
      !!                pore water diffusion and reactions
      !!                Only tracers involved in fast reactions are considered 
      !!                here for which a Rosenbrock scheme is used
      !!
      !!  ** Method  :  The jacobian is computed explicitly and stored in a 
      !!                condensed form. This routine is called in trosk.F90
      !!
      !!   History :
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) :: NEQ, NROWPD
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      REAL, DIMENSION(jpoce,NEQ), INTENT(in) :: X
      REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(out) :: jacvode
      ! --- local variables
      INTEGER  :: ji, jk, js, jn    ! dummy looop indices
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_jac')
!
      DO jn = 1, NEQ

         DO js = 1, NROWPD
            DO ji = 1, jpoce
               IF (accmask(ji) == 0) THEN
                  jacvode(ji,js,jn) = 0.0_wp
               ENDIF
            END DO
         END DO

         jk = jarr(jn,1)
         js = jarr(jn,2)
         IF (js <= jpwat) THEN
            DO ji = 1, jpoce
               IF (accmask(ji) == 0) THEN 
                  pwcp(ji,jk,js) = X(ji,jn) * 1E-6 
               ENDIF
            END DO
         ELSE
            DO ji = 1, jpoce
               IF (accmask(ji) == 0) THEN
                  solcp(ji,jk,js-jpwat) = X(ji,jn) * 1E-6
               ENDIF
            END DO
         ENDIF
      END DO

      CALL sed_dsrjac( NEQ, NROWPD, jacvode, accmask )        ! Redox reactions

      ! Computes diffusive and bioirrigation fluxes
      DO jn = 1, jpvode
         js = jsvode(jn)
         IF (js <= jpwat) THEN
            CALL sed_mat_dsrjac( js, NEQ, NROWPD, jacvode, accmask )
         ENDIF
      END DO

      ! Bioturbation of adsorbed species (Fe2+ and NH4)
      CALL sed_mat_adsjac( jwnh4, NEQ, NROWPD, jacvode, accmask )
      CALL sed_mat_adsjac( jwfe2, NEQ, NROWPD, jacvode, accmask )

      ! Bioturbation of solid species
      CALL sed_mat_btbjac( jsfeo, NEQ, NROWPD, jacvode, accmask )
      CALL sed_mat_btbjac( jsfes, NEQ, NROWPD, jacvode, accmask )

      IF( ln_timing )  CALL timing_stop('sed_jac')
!      
   END SUBROUTINE sed_jac

END MODULE sedjac
