MODULE sedadv
   !!======================================================================
   !!              ***  MODULE  sedadv  ***
   !!    Sediment : vertical advection and burial
   !!=====================================================================
   !! * Modules used
   !!----------------------------------------------------------------------
   !!   sed_adv :
   !!----------------------------------------------------------------------
   USE sed     ! sediment global variable
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_adv

   REAL(wp) :: cpor
   REAL(wp) :: por1clay 
   REAL(wp) :: eps = 1.e-13

CONTAINS

   SUBROUTINE sed_adv( kt )
      !!-------------------------------------------------------------------------
      !!                  ***  ROUTINE sed_adv  ***
      !!
      !! ** Purpose : vertical solid sediment advection and burial 
      !!
      !! ** Method  : At each grid point the 1-dimensional solid sediment column
      !!              is shifted according the rain added to the top layer and
      !!              the gaps produced through redissolution so that in the end
      !!              the original sediment mixed layer geometry is reestablished.
      !!
      !!
      !!   History :
      !!        !  98-08 (E. Maier-Reimer, Christoph Heinze )  Original code
      !!        !  04-10 (N. Emprin, M. Gehlen ) F90
      !!        !  06-04 (C. Ethe)  Re-organization
      !!-------------------------------------------------------------------------
      !!* Arguments
      INTEGER, INTENT(in) ::  &
         kt                     ! time step
      ! * local variables
      INTEGER :: ji, jk, js 
      INTEGER :: jn
      
      REAL(wp), DIMENSION(jpoce,jpksed,jpsol+jpads) :: zsolcp
      REAL(wp), DIMENSION(jpoce) :: fulsed, zwb
      REAL(wp) :: solfu, zfilled, uebers, seddef
      REAL(wp) :: buried, spresent, zfract, refill

      !------------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_adv')
!
      IF( kt == nitsed000 ) THEN
         IF (lwp) THEN
            WRITE(numsed,*) ' '
            WRITE(numsed,*) ' sed_adv : vertical sediment advection  '
            WRITE(numsed,*) ' '
         ENDIF
      ENDIF

      ! Allocation of the temporary arrays
      ! ----------------------------------
      DO js = 1, jpsol
         zsolcp(:,:,js) = solcp(:,:,js)
      END DO
      zsolcp(:,1,jpsol+1:jpsol+jpads) = 0.0_wp
      DO jk = 2, jpksed
         zsolcp(:,jk,jpsol+1) = pwcp(:,jk,jwnh4) * adsnh4 
         zsolcp(:,jk,jpsol+2) = pwcp(:,jk,jwfe2) * adsfe2
      END DO

      ! Initialization of data for mass balance calculation
      !---------------------------------------------------
      fromsed(:,:) = 0._wp
      tosed  (:,:) = 0._wp

      solfu = 0.0
      DO jk = 2, jpksed
         solfu = solfu + dz(jk) * por1(jk)
      END DO
      ! Initiate gap 
      !--------------

      ! Initiate burial rates
      !-----------------------
      DO jk = 2, jpksed-1
         DO ji = 1, jpoce
            zfilled = 0._wp
            DO js = 1, jpsol
               zfilled = zfilled + zsolcp(ji,jk,js) / dens_sol(js)
            END DO
            zwb(ji) = MAX(0._wp, (zfilled - 1._wp) / (zfilled + rtrn) )
         END DO

         DO js = 1, jpsol + jpads
            DO ji = 1, jpoce
               uebers = zwb(ji) * zsolcp(ji,jk,js)
               zsolcp(ji,jk,js) = zsolcp(ji,jk,js) - uebers
               zsolcp(ji,jk+1,js) = zsolcp(ji,jk+1,js) + uebers * dz(jk) * por1(jk) / ( dz(jk+1) * por1(jk+1) )
            END DO
         END DO
      END DO

         
      DO ji = 1, jpoce
         zfilled = 0._wp
         DO js = 1, jpsol
            zfilled = zfilled + zsolcp(ji,jpksed,js) / dens_sol(js)
         END DO
         zwb(ji) = MAX(0._wp, (zfilled - 1._wp) / (zfilled + rtrn) )
      END DO

      DO js = 1, jpsol + jpads
         DO ji = 1, jpoce
            uebers = zwb(ji) * zsolcp(ji,jpksed,js)
            zsolcp(ji,jpksed,js) = zsolcp(ji,jpksed,js) - uebers
            tosed(ji,js) = uebers * dz(jpksed) * por1(jpksed)
         END DO
      END DO

      DO js = 1, jpsol
         burial(:,js) = burial(:,js) + tosed(:,js)
      END DO


      fulsed(:) = 0.0_wp
      DO jk = 2, jpksed
         DO ji = 1, jpoce
            zfilled = 0._wp
            DO js = 1, jpsol
               zfilled = zfilled + zsolcp(ji,jk,js) / dens_sol(js)
            END DO
            fulsed(ji) = fulsed(ji) + zfilled * dz(jk) * por1(jk)
         END DO 
      END DO

      DO ji = 1, jpoce
         seddef = solfu - fulsed(ji)
         spresent = 0.0
         DO js = 1, jpsol
            spresent = spresent + burial(ji,js) * mol_wgt(js) / dens_sol(js) 
         END DO

         burial(ji,jsclay) = burial(ji,jsclay) + MAX(0._wp, seddef - spresent) * dens_sol(jsclay) / mol_wgt(jsclay)

         buried = 0.
         DO js = 1, jpsol
            buried = buried + burial(ji,js) * mol_wgt(js) / dens_sol(js)
         END DO

         refill = seddef/(buried+rtrn)
         zfract = por1(jpksed)*dz(jpksed)
         DO js = 1, jpsol
            zsolcp(ji,jpksed,js) = zsolcp(ji,jpksed,js) + refill*burial(ji,js)/zfract
            fromsed(ji,js) = refill*burial(ji,js)
            burial(ji,js) = burial(ji,js) - refill*burial(ji,js)
         END DO
     END DO

     DO jk = jpksed, 3, -1
        DO ji = 1, jpoce
            zfilled = 0._wp
            DO js = 1, jpsol
               zfilled = zfilled + zsolcp(ji,jk,js) / dens_sol(js)
            END DO
            zwb(ji) = MAX(0._wp, (zfilled - 1._wp) / (zfilled + rtrn) )
        END DO
        DO js = 1, jpsol + jpads
           DO ji = 1, jpoce
              uebers = zwb(ji) * zsolcp(ji,jk,js)
              zsolcp(ji,jk,js) = zsolcp(ji,jk,js) - uebers
              zsolcp(ji,jk-1,js) = zsolcp(ji,jk-1,js) + uebers * dz(jk) * por1(jk) / ( dz(jk-1) * por1(jk-1) )
           END DO
        END DO

     END DO

      DO js = 1, jpsol
         solcp(:,:,js) = zsolcp(:,:,js)
      END DO
      DO jk = 2, jpksed
         pwcp(:,jk,jwnh4) = (pwcp(:,jk,jwnh4) + zsolcp(:,jk,jpsol+1) * por1(jk) / por(jk) ) * radssol(jk,jwnh4)
         pwcp(:,jk,jwfe2) = (pwcp(:,jk,jwfe2) + zsolcp(:,jk,jpsol+2) * por1(jk) / por(jk) ) * radssol(jk,jwfe2)
      END DO

      IF( ln_timing )  CALL timing_stop('sed_adv')

   END SUBROUTINE sed_adv


END MODULE sedadv
