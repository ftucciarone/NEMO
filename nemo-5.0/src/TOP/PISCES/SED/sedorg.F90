MODULE sedorg
   !!======================================================================
   !!              ***  MODULE  sedinorg  ***
   !!    Sediment : dissolution and reaction in pore water of 
   !!               inorganic species
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedini
   USE sedmat
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_org

CONTAINS
   
   SUBROUTINE sed_org( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_org  ***
      !! 
      !!  ** Purpose :  computes pore water dissolution and reaction
      !!
      !!  ** Methode :  implicit simultaneous computation of undersaturation
      !!               resulting from diffusive pore water transport and chemical
      !!               pore water reactions. Solid material is consumed according
      !!               to redissolution and remineralisation
      !!
      !!  ** Remarks :
      !!              - undersaturation : deviation from saturation concentration
      !!              - reaction rate   : sink of undersaturation from dissolution
      !!                                 of solid material 
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) f90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!        !  19-08 (O. Aumont) Debugging and improvement of the model
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in)  :: kt   ! time step
      INTEGER :: ji, jk
      ! --- local variables
      REAL(wp), DIMENSION(jpoce, jpksed) :: psms, preac
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_org')

      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_org : solute species which do not experience redox reactions '
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF
!
      preac(:,:) = 0.0_wp
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            pwcpaa(ji,jk,jwpo4) = pwcpaa(ji,jk,jwpo4) - rcapat / ryear * MAX(0.0, pwcp(ji,jk,jwpo4) - 3.7E-6 )
         END DO
      END DO
      ! PO4 in pore water
      CALL sed_mat_dsre( jwpo4, preac, pwcpaa(:,:,jwpo4), dtsed, pwcp(:,:,jwpo4) )

      ! Iron ligands in pore water
      psms(:,1) = 0.0
      psms (:,2:jpksed) = ratligc * rearatpom(:,2:jpksed)
      preac(:,2:jpksed) = -reac_ligc
      
      CALL sed_mat_dsre( jwlgw, preac, psms, dtsed, pwcp(:,:,jwlgw) )

      IF( ln_timing )  CALL timing_stop('sed_org')
!      
   END SUBROUTINE sed_org

END MODULE sedorg
