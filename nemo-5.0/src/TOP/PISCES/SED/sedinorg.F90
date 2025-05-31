MODULE sedinorg
   !!======================================================================
   !!              ***  MODULE  sedinorg  ***
   !!    Sediment : dissolution and reaction in pore water of 
   !!               inorganic species
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sed_oce
   USE sedini
   USE sedmat
   USE sedco3
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_inorg

CONTAINS
   
   SUBROUTINE sed_inorg( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_inorg  ***
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
      ! --- local variables
      INTEGER   ::  ji, jk, jn, jnt          ! dummy looop indices
      REAL(wp), DIMENSION(jpoce) ::  zsieq, reac_silf
      REAL(wp), DIMENSION(jpoce,jpksed) :: preac, zundsat1, psms
      REAL(wp), DIMENSION(jpoce,jpksed) :: psmsdic, psmsalk
      REAL(wp), DIMENSION(jpoce,jpksed) :: zdic1, zalk1, zdic2, zalk2, zdicb, zalkb
      REAL(wp)  ::  ztemp, zsolid1, zunder, zrearat, zdissol
      REAL(wp)  ::  zsolcpcl, zsolcpsi, zexcess
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_inorg')

      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_inorg : Dissolution of CaCO3 and BSi  '
         IF (lwp) WRITE(numsed,*) ' '
      ENDIF
!
      DO ji = 1, jpoce
         ! -----------------------------------------------
         ! Computation of Si solubility
         ! Param of Ridgwell et al. 2002
         ! -----------------------------------------------

         zsolcpcl = 0.0
         zsolcpsi = 0.0
         DO jk = 2, jpksed
            zsolcpsi = zsolcpsi + solcp(ji,jk,jsopal) * vols3d(ji,jk)
            zsolcpcl = zsolcpcl + solcp(ji,jk,jsclay) * vols3d(ji,jk)
         END DO
         zsolcpsi = MAX( zsolcpsi, rtrn )
         IF ( zsolcpcl / zsolcpsi <= 2.0 ) THEN
            zsieq(ji) = sieqs(ji) * exp(-0.16 * zsolcpcl / zsolcpsi )
         ELSE
            zsieq(ji) = sieqs(ji) * MAX( exp(-0.32 - 0.04 * ( zsolcpcl / zsolcpsi - 2.0 ) ), 0.2 ) 
         ENDIF
         co3sat(ji) = aksps(ji) * densSW(ji)**2 / ( calcon2(ji) + rtrn )
         reac_silf(ji) = reac_sil * ( 0.05 + 0.055 * ( 1.64 * ( zsolcpcl / zsolcpsi + 0.01 ) )**(-0.75) ) / 1.25 
      END DO

      
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zsolid1 = volc(ji,jk,jsopal) * solcp(ji,jk,jsopal)
            zundsat1(ji,jk) = MAX(0., ( zsieq(ji) - pwcp(ji,jk,jwsil) ) )
            zunder = zundsat1(ji,jk) / zsieq(ji)
            preac(ji,jk) = reac_silf(ji) * zsolid1 / zsieq(ji) / ( 1.0  + reac_silf(ji) * dtsed * zunder ) 
         END DO
      END DO

      psms(:,:) = 0.0
      CALL sed_mat_dsre( jwsil, -preac, psms, dtsed, zundsat1(:,:) )
      
      DO jk = 1, jpksed
         DO ji = 1, jpoce
            pwcp(ji,jk,jwsil) = zsieq(ji) - zundsat1(ji,jk)
            solcp(ji,jk,jsopal) = solcp(ji,jk,jsopal) - preac(ji,jk) * zundsat1(ji,jk) / ( volc(ji,jk,jsopal) + rtrn ) * dtsed
         END DO
      END DO

      !---------------------------------------------------------------
      ! Performs CaCO3 particle deposition and redissolution (indice 9)
      !--------------------------------------------------------------

      ! computes co3por from the updated pwcp concentrations (note [co3por] = mol/l)
      ! *densSW(l)**2 converts aksps [mol2/kg sol2] into [mol2/l2] to get [undsat] in [mol/l]

      zdicb = pwcp(:,:,jwdic)
      zalkb = pwcp(:,:,jwalk)
      zdic1 = zdicb
      zalk1 = zalkb
      CALL sed_co3( kt )
      preac = 0.0
      xirrigtrd(:,jwdic) = 0.0
      xirrigtrd(:,jwalk) = 0.0

      DO jk =1 ,jpksed
         DO ji = 1, jpoce
            zsolid1 = volc(ji,jk,jscal) * solcp(ji,jk,jscal)
            zunder = MAX(0., co3sat(ji) - co3por(ji,jk) ) / ( co3sat(ji) + rtrn )
            zdissol = reac_cal * zsolid1 * zunder / &
            &                ( 1. + reac_cal * dtsed / 3.0 * zunder )
            psmsdic(ji,jk) = rearatpom(ji,jk) + zdissol
            psmsalk(ji,jk) = pwcpaa(ji,jk,jwalk) + 2.0 * zdissol
         END DO
      END DO

      CALL sed_mat_dsri( jwdic, -preac, psmsdic, dtsed/3.0, zdic1(:,:) )
      CALL sed_mat_dsri( jwalk, -preac, psmsalk, dtsed/3.0, zalk1(:,:) )

       DO jk = 2, jpksed
          DO ji = 1, jpoce
             xirrigtrd(ji,jwdic) = xirrigtrd(ji,jwdic) - irrig(ji,jk) * (zdic1(ji,1) - zdic1(ji,jk) ) * volw3d(ji,jk) * 5.0 * dtsed / 11.0
             xirrigtrd(ji,jwalk) = xirrigtrd(ji,jwalk) - irrig(ji,jk) * (zalk1(ji,1) - zalk1(ji,jk) ) * volw3d(ji,jk) * 5.0 * dtsed / 11.0
          END DO
       END DO

      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zdic2(ji,jk) = 4./3. * zdic1(ji,jk) - 1./3. * zdicb(ji,jk)
            zalk2(ji,jk) = 4./3. * zalk1(ji,jk) - 1./3. * zalkb(ji,jk)
         END DO
      END DO

      pwcp(:,:,jwdic) = zdic2
      pwcp(:,:,jwalk) = zalk2
      CALL sed_co3( kt )

      DO jk =1 ,jpksed
         DO ji = 1, jpoce
            zsolid1 = volc(ji,jk,jscal) * solcp(ji,jk,jscal)
            zunder = MAX(0., co3sat(ji) - co3por(ji,jk) ) / ( co3sat(ji) + rtrn )
            solcp(ji,jk,jscal) = solcp(ji,jk,jscal) - reac_cal * zsolid1 * zunder * 5.0 / 11.0 * dtsed   &
            &                / ( 1. + reac_cal * 5.0 * dtsed / 11.0 * zunder ) / ( volc(ji,jk,jscal) + rtrn )
            zdissol = reac_cal * zsolid1 * zunder / &
            &                ( 1. + reac_cal * 2.0 * dtsed / 9.0 * zunder )
            psmsdic(ji,jk) = rearatpom(ji,jk) + zdissol
            psmsalk(ji,jk) = pwcpaa(ji,jk,jwalk) + 2.0 * zdissol
         END DO
      END DO

      CALL sed_mat_dsri( jwdic, -preac, psmsdic, 2.0 * dtsed/9.0, zdic2(:,:) )
      CALL sed_mat_dsri( jwalk, -preac, psmsalk, 2.0 * dtsed/9.0, zalk2(:,:) )

       DO jk = 2, jpksed
          DO ji = 1, jpoce
             xirrigtrd(ji,jwdic) = xirrigtrd(ji,jwdic) - irrig(ji,jk) * (zdic2(ji,1) - zdic2(ji,jk) ) * volw3d(ji,jk) * 4.0 * dtsed / 11.0
             xirrigtrd(ji,jwalk) = xirrigtrd(ji,jwalk) - irrig(ji,jk) * (zalk2(ji,1) - zalk2(ji,jk) ) * volw3d(ji,jk) * 4.0 * dtsed / 11.0
          END DO
       END DO

      DO jk = 1, jpksed
         DO ji = 1, jpoce
            zdic1(ji,jk) = 18.0/11.0 * zdic2(ji,jk) - 9./11. * zdic1(ji,jk) + 2./11. * zdicb(ji,jk)
            zalk1(ji,jk) = 18.0/11.0 * zalk2(ji,jk) - 9./11. * zalk1(ji,jk) + 2./11. * zalkb(ji,jk)
         END DO
      END DO

      pwcp(:,:,jwdic) = zdic1
      pwcp(:,:,jwalk) = zalk1
      CALL sed_co3( kt )

      DO jk =1 ,jpksed
         DO ji = 1, jpoce
            zsolid1 = volc(ji,jk,jscal) * solcp(ji,jk,jscal)
            zunder = MAX(0., co3sat(ji) - co3por(ji,jk) ) / ( co3sat(ji) + rtrn )
            solcp(ji,jk,jscal) = solcp(ji,jk,jscal) - reac_cal * zsolid1 * zunder * 4.0 / 11.0 * dtsed   &
            &                / ( 1. + reac_cal * 4.0 * dtsed / 11.0 * zunder ) / ( volc(ji,jk,jscal) + rtrn )
            zdissol = reac_cal * zsolid1 * zunder / &
            &                ( 1. + reac_cal * 2.0 * dtsed / 11.0 * zunder )
            psmsdic(ji,jk) = rearatpom(ji,jk) + zdissol
            psmsalk(ji,jk) = pwcpaa(ji,jk,jwalk) + 2.0 * zdissol
         END DO
      END DO

      CALL sed_mat_dsri( jwdic, -preac, psmsdic, 2.0 * dtsed/11.0, zdic1(:,:) )
      CALL sed_mat_dsri( jwalk, -preac, psmsalk, 2.0 * dtsed/11.0, zalk1(:,:) )

       DO jk = 2, jpksed
          DO ji = 1, jpoce
             xirrigtrd(ji,jwdic) = xirrigtrd(ji,jwdic) - irrig(ji,jk) * (zdic1(ji,1) - zdic1(ji,jk) ) * volw3d(ji,jk) * 2.0 * dtsed / 11.0
             xirrigtrd(ji,jwalk) = xirrigtrd(ji,jwalk) - irrig(ji,jk) * (zalk1(ji,1) - zalk1(ji,jk) ) * volw3d(ji,jk) * 2.0 * dtsed / 11.0
          END DO
       END DO

      pwcp(:,:,jwdic) = zdic1
      pwcp(:,:,jwalk) = zalk1
      CALL sed_co3( kt )

      DO jk =1 ,jpksed
         DO ji = 1, jpoce
            zsolid1 = volc(ji,jk,jscal) * solcp(ji,jk,jscal)
            zunder = MAX(0., co3sat(ji) - co3por(ji,jk) ) / ( co3sat(ji) + rtrn )
            solcp(ji,jk,jscal) = solcp(ji,jk,jscal) - reac_cal * zsolid1 * zunder * 4.0 / 11.0 * dtsed   &
            &                / ( 1. + reac_cal * 4.0 * dtsed / 11.0 * zunder ) / ( volc(ji,jk,jscal) + rtrn )
         END DO
      END DO


      IF( ln_timing )  CALL timing_stop('sed_inorg')
!      
   END SUBROUTINE sed_inorg

END MODULE sedinorg
