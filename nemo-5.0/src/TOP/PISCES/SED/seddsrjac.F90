MODULE seddsrjac
   !!======================================================================
   !!              ***  MODULE  seddsr  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE seddsr
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_dsrjac

   !! * Module variables

CONTAINS
   
   SUBROUTINE sed_dsrjac( NEQ, NROWPD, jacvode, accmask )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dsr  ***
      !! 
      !!  ** Purpose :  computes pore water dissolution and reaction
      !!
      !!  ** Methode :  Computation of the redox reactions in sediment.
      !!                The main redox reactions are solved in sed_dsr whereas
      !!                the secondary reactions are solved in sed_dsr_redoxb.
      !!                A strand spliting approach is being used here (see 
      !!                sed_dsr_redoxb for more information). 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model.
      !!                             The original method is replaced by a 
      !!                              Strand splitting method which deals 
      !!                              well with stiff reactions.
      !!----------------------------------------------------------------------
      !! Arguments
      ! --- local variables
      INTEGER, INTENT(in) :: NEQ, NROWPD
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
      INTEGER :: ji, jni, jnj, jnij, jk, js, jw, jn, nivode   ! dummy looop indices

      REAL(wp) ::  zlimo2, zlimno3, zlimso4, zlimfeo    ! undersaturation ; indice jpwatp1 is for calcite   
      REAL(wp) ::  zlimdo2, zlimdno3, zlimdso4, zlimdfeo    ! undersaturation ; indice jpwatp1 is for calcite   
      REAL(wp) ::  zreasat, zlimtmpo2, zlimtmpno3, zlimtmpfeo, zlimtmpso4
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dsrjac')
!
      ! Initializations
      !----------------------
      
      !----------------------------------------------------------
      ! 5.  Beginning of solid reaction
      !---------------------------------------------------------
      
      ! Computes SMS of oxygen
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               zlimo2 = pwcp(ji,jk,jwoxy) / ( pwcp(ji,jk,jwoxy) + xksedo2 )
               zlimdo2 = xksedo2 / ( pwcp(ji,jk,jwoxy) + xksedo2 )**2
               nivode = (jk-1) * jpvode
               zlimno3 = pwcp(ji,jk,jwno3) / ( pwcp(ji,jk,jwno3) + xksedno3 )
               zlimdno3 = xksedno3 / ( pwcp(ji,jk,jwno3) + xksedno3 )**2
               zlimfeo  = solcp(ji,jk,jsfeo) / ( solcp(ji,jk,jsfeo) + xksedfeo )
               zlimdfeo = xksedfeo / ( solcp(ji,jk,jsfeo) + xksedfeo )**2
               zlimso4 = pwcp(ji,jk,jwso4) / ( pwcp(ji,jk,jwso4) + xksedso4 )
               zlimdso4 = xksedso4 / ( pwcp(ji,jk,jwso4) + xksedso4 )**2

               ! Acid Silicic 
               jni = (jk-1)*jpvode+isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - so2ut * rearatpom(ji,jk) * zlimdo2

               ! For nitrates
               jni = nivode+isvode(jwno3)
               jnj = nivode+isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - srDnit * rearatpom(ji,jk) * (1.0 - zlimo2 ) &
               &         * zlimdno3
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + srDnit * rearatpom(ji,jk) * zlimno3 * zlimdo2

               ! For FEOH
               zreasat = 4.0 * rearatpom(ji,jk) / volc(ji,jk,jsfeo)
               jni = nivode+isvode(jpwat+jsfeo)
               jnij = jpvode + 1
               zlimtmpfeo = ( 1.0 - zlimno3 ) * ( 1.0 - zlimo2 ) * zlimdfeo
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - zreasat * zlimtmpfeo
               jnj = nivode+isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               zlimtmpo2 = zlimfeo * zlimdo2 * ( 1.0 - zlimno3 )
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zreasat * zlimtmpo2 
               jnj = nivode+isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               zlimtmpno3 = zlimfeo * zlimdno3 * ( 1.0 - zlimo2 )
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zreasat * zlimtmpno3

               ! Iron
               zreasat = rearatpom(ji,jk) * 4.0 * radssol(jk,jwfe2)
               jni = nivode+isvode(jwfe2)
               jnj = nivode+isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zreasat * zlimtmpfeo
               jnj = nivode+isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zreasat * zlimtmpo2
               jnj = nivode+isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zreasat * zlimtmpno3

               ! For sulfur
               jni = nivode + isvode(jwso4)
               jnij = jpvode + 1
               zlimtmpso4 = ( 1.0 - zlimno3 ) * ( 1.0 - zlimo2 ) * ( 1.0 - zlimfeo ) * zlimdso4
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 0.5 * rearatpom(ji,jk) * zlimtmpso4
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               zlimtmpo2 = zlimso4 * zlimdo2 * ( 1.0 - zlimno3 )  &
               &      * ( 1.0 - zlimfeo ) 
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpo2
               jnj = nivode + isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               zlimtmpno3 = zlimso4 * ( 1.0 - zlimo2 ) * zlimdno3   &
               &      * ( 1.0 - zlimfeo )
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpno3 
               jnj = nivode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               zlimtmpfeo = zlimso4 * ( 1.0 - zlimo2 ) * ( 1.0 - zlimno3 ) * zlimdfeo
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpfeo
               jni = nivode + isvode(jwh2s)
               jnj = nivode + isvode(jwso4)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 0.5 * rearatpom(ji,jk) * zlimtmpso4
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.5 * rearatpom(ji,jk) * zlimtmpo2
               jnj = nivode + isvode(jwno3)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.5 * rearatpom(ji,jk) * zlimtmpno3
               jnj = nivode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.5 * rearatpom(ji,jk) * zlimtmpfeo
            ENDIF
         END DO
      ENDDO

      ! Secondary redox reactions
      ! -------------------------

      call sed_dsr_redoxbjac( NEQ, NROWPD, jacvode, accmask )

      IF( ln_timing )  CALL timing_stop('sed_dsrjac')
!      
   END SUBROUTINE sed_dsrjac

   SUBROUTINE sed_dsr_redoxbjac( NEQ, NROWPD, jacvode, accmask )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dsr_redox  ***
      !! 
      !!  ** Purpose :  computes secondary redox reactions
      !!
      !!   History :
      !!        !  18-08 (O. Aumont)  Original code
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) :: NEQ, NROWPD
      REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
      INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
      ! --- local variables
      INTEGER   ::  ji, jni, jnj, jnij, jk, nivode   ! dummy looop indices

      REAL(wp)  ::  zalpha, zexcess, zh2seq, zsedfer, zdsedfer, zfact
      REAL(wp), DIMENSION(jpoce,jpksed) :: zapproxfer, zderivfer
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dsr_redoxbjac')

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0 ) THEN
               zalpha = ( pwcp(ji,jk,jwfe2) - pwcp(ji,jk,jwlgw) ) * 1E9
               IF (zalpha < 2.0) THEN
                  zfact = exp(10.0*zalpha)
                  zapproxfer(ji,jk) = LOG(zfact + 1.0 ) /10.0 * 1E-9
                  zderivfer (ji,jk) = zfact / ( zfact + 1.0 ) 
               ELSE
                  zapproxfer(ji,jk) = zalpha * 1E-9
                  zderivfer (ji,jk) = 1.0
               ENDIF
            ENDIF
         END DO
      END DO

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               zsedfer  = zapproxfer(ji,jk)
               zdsedfer = zderivfer (ji,jk)
               ! First pass of the scheme. At the end, it is 1st order 
               ! -----------------------------------------------------
               ! Fe (both adsorbed and solute) + O2
               nivode = (jk-1) * jpvode
               jni = nivode + isvode(jwfe2)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_fe2 * pwcp(ji,jk,jwoxy) * zdsedfer
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fe2 * zsedfer
               jni = nivode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 0.25 * reac_fe2 * zsedfer / radssol(jk,jwfe2)
               jnj = nivode + isvode(jwfe2)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 0.25 * reac_fe2 / radssol(jk,jwfe2) * pwcp(ji,jk,jwoxy) * zdsedfer
               jni = nivode + isvode(jpwat+jsfeo)
               jnj = nivode + isvode(jwfe2)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fe2 / radssol(jk,jwfe2) * pwcp(ji,jk,jwoxy)   &
               &     * zdsedfer / volc(ji,jk,jsfeo)
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_fe2 / radssol(jk,jwfe2) * zsedfer  &
               &     / volc(ji,jk,jsfeo)
            ENDIF
         END DO
      END DO

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               ! H2S + O2
               nivode = (jk-1) * jpvode
               jni = nivode + isvode(jwh2s)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_h2s * pwcp(ji,jk,jwoxy)
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_h2s * pwcp(ji,jk,jwh2s)
               jni = nivode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 2.0 * reac_h2s * pwcp(ji,jk,jwh2s)
               jnj = nivode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 2.0 * reac_h2s * pwcp(ji,jk,jwoxy)
               jni = nivode + isvode(jwso4)
               jnj = nivode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_h2s * pwcp(ji,jk,jwoxy)
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_h2s * pwcp(ji,jk,jwh2s)
            ENDIF
         END DO
      END DO

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               ! NH4 + O2
               nivode = (jk-1) * jpvode
               jni = nivode + isvode(jwnh4)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_nh4 * pwcp(ji,jk,jwoxy)
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_nh4 * pwcp(ji,jk,jwnh4)
               jni = nivode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 2.0 * reac_nh4 * pwcp(ji,jk,jwnh4) / radssol(jk,jwnh4)
               jnj = nivode + isvode(jwnh4)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 2.0 * reac_nh4 * pwcp(ji,jk,jwoxy) / radssol(jk,jwnh4)
               jni = nivode + isvode(jwno3)
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_nh4 * pwcp(ji,jk,jwnh4) / radssol(jk,jwnh4)
               jnj = nivode + isvode(jwnh4)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_nh4 * pwcp(ji,jk,jwoxy) / radssol(jk,jwnh4)
            ENDIF
         END DO
      END DO

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               ! FeS - O2
               nivode = (jk-1) * jpvode
               jni = nivode + isvode(jpwat+jsfes)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_feso * pwcp(ji,jk,jwoxy) 
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_feso * solcp(ji,jk,jsfes)
               jni = nivode + isvode(jwoxy)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 2.0 * reac_feso * solcp(ji,jk,jsfes)  &
               &     * volc(ji,jk,jsfes)
               jnj = nivode + isvode(jpwat+jsfes)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 2.0 * reac_feso * pwcp(ji,jk,jwoxy)   &
               &     * volc(ji,jk,jsfes)
               jni = nivode + isvode(jwfe2)
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * solcp(ji,jk,jsfes)  &
               &     * volc(ji,jk,jsfes) * radssol(jk,jwfe2)
               jnj = nivode + isvode(jpwat+jsfes)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * pwcp(ji,jk,jwoxy)   &
               &     * volc(ji,jk,jsfes) * radssol(jk,jwfe2)
               jni = nivode + isvode(jwso4)
               jnj = nivode + isvode(jwoxy)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * solcp(ji,jk,jsfes)  &
               &     * volc(ji,jk,jsfes)
               jnj = nivode + isvode(jpwat+jsfes)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feso * pwcp(ji,jk,jwoxy)   &
               &     * volc(ji,jk,jsfes)
            ENDIF
         END DO
      END DO

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               ! FEOH + H2S
               nivode = (jk-1) * jpvode
               jni = nivode + isvode(jpwat+jsfeo)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - 8.0 * reac_feh2s * pwcp(ji,jk,jwh2s)
               jnj = nivode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - 8.0 * reac_feh2s * solcp(ji,jk,jsfeo)
               jni = nivode + isvode(jwh2s)
               jnij = jpvode + 1
               jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - reac_feh2s * solcp(ji,jk,jsfeo)  &
               &     * volc(ji,jk,jsfeo)
               jnj = nivode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_feh2s * pwcp(ji,jk,jwh2s)   &
               &     * volc(ji,jk,jsfeo)
               jni = nivode + isvode(jwfe2)
               jnj = nivode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 8.0 * reac_feh2s * solcp(ji,jk,jsfeo)  &
               &     * volc(ji,jk,jsfeo) * radssol(jk,jwfe2)
               jnj = nivode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + 8.0 * reac_feh2s * pwcp(ji,jk,jwh2s)   &
               &     * volc(ji,jk,jsfeo) * radssol(jk,jwfe2)
               jni = nivode + isvode(jwso4)
               jnj = nivode + isvode(jwh2s)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feh2s * solcp(ji,jk,jsfeo)  &
               &     * volc(ji,jk,jsfeo)
               jnj = nivode + isvode(jpwat+jsfeo)
               jnij = jni - jnj + jpvode + 1
               jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + reac_feh2s * pwcp(ji,jk,jwh2s)   &
               &     * volc(ji,jk,jsfeo)
            ENDIF
         END DO
      END DO

      DO jk = 2, jpksed
         DO ji = 1, jpoce
            IF (accmask(ji) == 0) THEN
               zsedfer  = zapproxfer(ji,jk)
               zdsedfer = zderivfer (ji,jk)
               zh2seq     = MAX(rtrn, 2.1E-3 * hipor(ji,jk)) * ( 1.0 + hipor(ji,jk) / ( densSW(ji) * akh2s(ji) ) )
               zexcess = pwcp(ji,jk,jwh2s) * zsedfer / zh2seq - 1.0
               nivode = (jk-1) * jpvode
               IF ( zexcess >= 0.0 ) THEN
                  zfact = reac_fesp / zh2seq
                  jni = nivode + isvode(jwfe2)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - zfact * pwcp(ji,jk,jwh2s) * zdsedfer * radssol(jk,jwfe2)
                  jnj = nivode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zfact * zsedfer * radssol(jk,jwfe2)
                  jni = nivode + isvode(jpwat+jsfes)
                  jnj = nivode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zfact * pwcp(ji,jk,jwh2s) * zdsedfer / volc(ji,jk,jsfes)
                  jnj = nivode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zfact * zsedfer / volc(ji,jk,jsfes)
                  jni = nivode + isvode(jwh2s)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - zfact * zsedfer
                  jnj = nivode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zfact * pwcp(ji,jk,jwh2s) * zdsedfer
               ELSE
                  zfact = reac_fesd / zh2seq
                  jni = nivode + isvode(jwfe2)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - zfact * pwcp(ji,jk,jwh2s)   &
                  &     * zdsedfer * radssol(jk,jwfe2) * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = nivode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zfact * zsedfer * radssol(jk,jwfe2)   &
                  &     * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = nivode + isvode(jpwat+jsfes)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesd * zexcess * radssol(jk,jwfe2) * volc(ji,jk,jsfes)
                  jni = nivode + isvode(jpwat+jsfes)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) + reac_fesd * zexcess
                  jnj = nivode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zfact * solcp(ji,jk,jsfes)   &
                  &     * zdsedfer * pwcp(ji,jk,jwh2s)
                  jnj = nivode + isvode(jwh2s)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) + zfact * solcp(ji,jk,jsfes)   &
                  &     * zsedfer
                  jni = nivode + isvode(jwh2s)
                  jnij = jpvode + 1
                  jacvode(ji,jnij,jni) = jacvode(ji,jnij,jni) - zfact * zsedfer    &
                  &     * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = nivode + isvode(jwfe2)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - zfact * pwcp(ji,jk,jwh2s)   &
                  &     * zdsedfer * solcp(ji,jk,jsfes) * volc(ji,jk,jsfes)
                  jnj = nivode + isvode(jpwat+jsfes)
                  jnij = jni - jnj + jpvode + 1
                  jacvode(ji,jnij,jnj) = jacvode(ji,jnij,jnj) - reac_fesd * zexcess * volc(ji,jk,jsfes)
               ENDIF
            ENDIF
         END DO
      END DO

      IF( ln_timing )  CALL timing_stop('sed_dsr_redoxbjac')

  END SUBROUTINE sed_dsr_redoxbjac

END MODULE seddsrjac
