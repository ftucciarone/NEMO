MODULE sedsol
   !!======================================================================
   !!              ***  MODULE  sedsol  ***
   !!    Sediment : dissolution and reaction in pore water related 
   !!    related to organic matter
   !!    Diffusion of solutes in pore water
   !!=====================================================================
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedini
   USE sedjac
   USE sedbtb
   USE sedinorg
   USE sedorg
# if ! defined key_agrif
   USE trosk
#endif
   USE lib_mpp         ! distribued memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_sol

CONTAINS
   
   SUBROUTINE sed_sol( kt ) 
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_sol  ***
      !! 
      !!  ** Purpose :  computes pore water diffusion and reactions
      !!
      !!  ** Method  :  Computation of the redox and dissolution reactions 
      !!                in the sediment.
      !!
      !!   :History :
      !!----------------------------------------------------------------------
      !! Arguments
      INTEGER, INTENT(in) ::   kt
      ! --- local variables
      INTEGER  :: ji, jk, js, jw, jn, neq   
      REAL(wp), DIMENSION( jpoce, jpvode * jpksed ) :: ZXIN, FVAL
# if ! defined key_agrif      
      INTEGER :: JINDEX, IJAC, MLJAC
      INTEGER :: MUJAC, LE1, LJAC, IDID, NMAXSTP, ROSM
      REAL(wp) :: X, XEND
      REAL(wp),DIMENSION(jpoce) :: H
      INTEGER, DIMENSION(jpoce) :: accmask
      REAL(wp), DIMENSION(jpvode * jpksed) :: RTOL, ATOL
      INTEGER, DIMENSION(jpoce,3)   :: ISTAT
      REAL(wp), DIMENSION(jpoce,2)  :: RSTAT
#endif
      REAL(wp) ::   zfact
      !!
      !!----------------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_sol')
!
      IF( kt == nitsed000 ) THEN
         IF (lwp) THEN
            WRITE(numsed,*) ' sed_sol : Organic/inorganic degradation related reactions and diffusion'
            WRITE(numsed,*) ' '
         ENDIF
!         ! 
      ENDIF

      ! New solid fractions (including solid rain fractions) for k=2
      !------------------------------------------------------------------
      zfact = dtsed / ( por1(2) * dz(2) )
      DO js = 1, jpsol
         DO ji = 1, jpoce
            solcp(ji,2,js) = solcp(ji,2,js) + rainrg(ji,js) * zfact
         END DO
      ENDDO

      ! --------------------------------------------------
      ! Computation of the diffusivities
      ! --------------------------------------------------
      DO js = 1, jpwat
         DO jk = 1, jpksed
            DO ji = 1, jpoce
               diff(ji,jk,js) = ( diff1(js) + diff2(js) * temp(ji) ) * xtortuosity(jk)
            END DO
         END DO
      END DO

      pwcpaa(:,:,jwalk) = 0.0
      pwcpaa(:,:,jwpo4) = 0.0
      DO jn = 1, jpvode
         js = jsvode(jn)
         IF (js <= jpwat) xirrigtrd(:,js) = 0.0
      END DO

      ! Apply bioturbation and compute the impact of the slow SMS on species
      CALL sed_btb( kt )

# if ! defined key_agrif
      ! The following part deals with the stiff ODEs
      ! This is the expensive part of the code and should be carefully
      ! chosen. We use the DVODE solver after many trials to find a cheap 
      ! way to solve the ODEs. This is not necessarily the most efficient 
      ! but this is the one that was not too much of a pain to code and that
      ! was the most precise and quick.
      ! The ones I tried : operator splitting (Strang), hybrid spectral methods
      ! Brent, Powell's hybrid method, ...
      ! ---------------------------------------------------------------------
      NEQ  = jpvode * jpksed
      XEND = dtsed
      RTOL = rosrtol
      ATOL = rosatol
      IJAC = 1
      DO jn = 1, NEQ
         js = jarr(jn,2)
         IF (js == jwfe2) ATOL(jn) = rosatol / 100.0
      END DO
      MLJAC = jpvode
      MUJAC = jpvode
      LE1  = 2*MLJAC+MUJAC+1
      LJAC = MLJAC+MUJAC+1
      X     = 0.0
      H(:)  = dtsed

      ! Put all the species in one local array (nb of tracers * vertical
      ! dimension
      DO jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         IF (js <= jpwat) THEN
            zxin(:,jn) = pwcp(:,jk,js) * 1E6
         ELSE
            zxin(:,jn) = solcp(:,jk,js-jpwat) * 1E6
         ENDIF
      END DO

      ! Set options for VODE : banded matrix. SParse option is much more
      ! expensive except if one computes the sparse Jacobian explicitly
      ! To speed up the computation, one way is to reduce ATOL and RTOL
      ! which may be a good option at the beginning of the simulations 
      ! during the spin up
      ! ----------------------------------------------------------------
      CALL ROSK(NROSORDER, NEQ,X,zxin,XEND,H,RTOL,ATOL,sed_jac,  &
           &   MLJAC,MUJAC,IDID,ISTAT)

      DO jn = 1, NEQ
         jk = jarr(jn,1)
         js = jarr(jn,2)
         IF (js <= jpwat) THEN
            pwcp(:,jk,js) = zxin(:,jn) * 1E-6
         ELSE
            solcp(:,jk,js-jpwat) = zxin(:,jn) * 1E-6
         ENDIF
      END DO
      rstepros(:) = ISTAT(:,3)
#endif

      ! CALL inorganic and organic slow redow/chemistry processes
      ! ---------------------------------------------------------
      CALL sed_inorg( kt )

      ! organic SMS of the slow species
      CALL sed_org( kt )

      ! Impact of bioirrigation on tracer in the water column
      DO jw = 1, jpwat
         DO ji = 1, jpoce
            pwcp(ji,1,jw) = pwcp(ji,1,jw) + xirrigtrd(ji,jw) / volw3d(ji,1)
         END DO
      END DO

      IF( ln_timing )  CALL timing_stop('sed_sol')
!      
   END SUBROUTINE sed_sol

   SUBROUTINE JACFAC
  
   END SUBROUTINE JACFAC

END MODULE sedsol
