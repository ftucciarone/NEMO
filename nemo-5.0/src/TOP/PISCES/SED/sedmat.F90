MODULE sedmat
   !!======================================================================
   !!              ***  MODULE  sedmat  ***
   !!    Sediment : linear system of equations
   !!=====================================================================
   !! * Modules used
   !!----------------------------------------------------------------------

   USE sed     ! sediment global variable
   USE par_sed, ONLY : jpksed
   USE lib_mpp         ! distribued memory computing library


   IMPLICIT NONE
   PRIVATE

   PUBLIC sed_mat_dsr 
   PUBLIC sed_mat_dsrjac
   PUBLIC sed_mat_dsri
   PUBLIC sed_mat_dsre
   PUBLIC sed_mat_btb
   PUBLIC sed_mat_btbjac
   PUBLIC sed_mat_ads
   PUBLIC sed_mat_adsjac
   PUBLIC sed_mat_btbi
   PUBLIC sed_mat_btbe
   PUBLIC sed_mat_coef

 CONTAINS

    SUBROUTINE sed_mat_coef
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_coef  ***
       !!
       !! ** Purpose :  Computes the non variable coefficient used to compute
       !!               the diffusion of solute species
       !!
       !! ** Method  : 
       !!----------------------------------------------------------------------
       !! * Arguments
       !---Local declarations
       INTEGER  ::  ji, jk
       REAL(wp) ::  aplus, aminus, dxplus, dxminus
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_coef')

       DO ji = 1, jpoce
          ! first sediment level          
          aplus  = ( por(1) + por(2) ) * 0.5
          dxplus = ( dz(1) + dz(2) ) / 2.
          apluss(ji,1) = ( 1.0 / ( volw3d(ji,1) ) ) * aplus / dxplus

          ! Interior of the sediment column
          DO jk = 2, jpksed - 1
             aminus  = ( por(jk-1) + por(jk) ) * 0.5
             dxminus = ( dz(jk-1) + dz(jk) ) / 2.

             aplus   = ( por(jk+1) + por(jk) ) * 0.5
             dxplus  = ( dz(jk) + dz(jk+1) ) / 2
             !
             aminuss(ji,jk) = ( 1.0 / volw3d(ji,jk) ) * aminus / dxminus
             apluss (ji,jk) = ( 1.0 / volw3d(ji,jk) ) * aplus / dxplus
          END DO

          ! Bottom of the sediment column
          aminus  = ( por(jpksed-1) + por(jpksed) ) * 0.5
          dxminus = ( dz(jpksed-1) + dz(jpksed) ) / 2.
          aminuss(ji,jpksed)  = ( 1.0 / volw3d(ji,jpksed) ) * aminus / dxminus
       END DO

       IF( ln_timing )  CALL timing_stop('sed_mat_coef')

    END SUBROUTINE sed_mat_coef

    SUBROUTINE sed_mat_dsr( nvar, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsr  ***
       !!
       !! ** Purpose :  Computes the SMS due to diffusion of solute species
       !!
       !! ** Method  : 
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar   ! number of variable
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask
       INTEGER :: ji

       !---Local declarations
       INTEGER  ::  jk, jn
       REAL(wp) ::  rplus,rminus, zirrigt, zfact
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_dsr')

       ! Computation of the tridiagonal matrix associated
       ! to diffusion of solute species
       !-------------------------------------------------
       jn = nvar
       zfact = 1.0

       ! Biorrigation is reduced strongly for Fe2+
       IF (jn == jwfe2) zfact = 0.1
 
       ! computes the SMS due to diffusion
       ! ---------------------------------
       DO ji = 1, jpoce
          IF (accmask(ji) == 0) THEN
             rplus  = apluss(ji,1) * diff(ji,1,jn) * radssol(1,jn)
             pwcpa(ji,1,jn) = pwcpa(ji,1,jn) + ( rplus * pwcp(ji,2,jn) - rplus * pwcp(ji,1,jn) )

             rminus  = aminuss(ji,jpksed) * diff(ji,jpksed-1,jn) * radssol(jpksed,jn)
             zirrigt = zfact * irrig(ji,jpksed) * (pwcp(ji,1,jn) - pwcp(ji,jpksed,jn) )
             pwcpa(ji,jpksed,jn) = pwcpa(ji,jpksed,jn) + rminus * ( pwcp(ji,jpksed-1,jn)    &
             &                     - pwcp(ji,jpksed,jn) ) + zirrigt
             xirrigtrdtmp(ji,jn) = -zirrigt * volw3d(ji,jpksed)
          ENDIF
       END DO
       DO jk = 2, jpksed - 1
          DO ji = 1, jpoce
             IF (accmask(ji) == 0) THEN
                rminus = aminuss(ji,jk) * diff(ji,jk-1,jn) * radssol(jk,jn)
                zirrigt = zfact * irrig(ji,jk) * (pwcp(ji,1,jn) - pwcp(ji,jk,jn) )
                rplus  = apluss (ji,jk) * diff(ji,jk,jn) * radssol(jk,jn)
                !     
                pwcpa(ji,jk,jn) = pwcpa(ji,jk,jn) + ( rplus * pwcp(ji,jk+1,jn) + rminus * pwcp(ji,jk-1,jn)    &
                &                  - ( rplus + rminus ) * pwcp(ji,jk,jn) ) + zirrigt
                xirrigtrdtmp(ji,jn) = xirrigtrdtmp(ji,jn) - zirrigt * volw3d(ji,jk)
             ENDIF
          END DO
       END DO

       IF( ln_timing )  CALL timing_stop('sed_mat_dsr')

    END SUBROUTINE sed_mat_dsr

    SUBROUTINE sed_mat_dsrjac( nvar, NEQ, NROWPD, jacvode, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsrjac  ***
       !!
       !! ** Purpose :  Computes the jacobian of the diffusion of solute
       !!               species
       !!
       !! ** Method  : 
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar, NEQ, NROWPD  ! number of variable
       REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask

       !---Local declarations
       INTEGER  ::  ji,jk, jn, jnn, jni, jnj ,jnij
       REAL(wp) ::  rplus,rminus, zfact
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_dsrjac')

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       jn = nvar
       zfact = 1.0

       ! Bioirrigation is strongly reduced for Fe2+
       IF (jn == jwfe2) zfact = 0.1

       ! Computes the non zero element of the jacobian matrix
       ! The jacobian is stored in a condensed form

       ! first and bottom sediment level
       jnn = isvode(jn)
       DO ji = 1, jpoce
          IF (accmask(ji) == 0 ) THEN
             rplus  = apluss(ji,1) * diff(ji,1,jn) * radssol(1,jn)

             jnij = jpvode + 1
             jacvode(ji, jnij, jnn) = jacvode(ji,jnij,jnn) - rplus
             jnj = jpvode + jnn
             jnij = jnn - jnj + jpvode + 1
             jacvode(ji, jnij, jnj) = jacvode(ji,jnij,jnj) + rplus

             rminus  = aminuss(ji,jpksed) * diff(ji,jpksed-1,jn) * radssol(jpksed,jn)

             jni = (jpksed-1) * jpvode + jnn
             jnj = (jpksed-2) * jpvode + jnn
             jnij = jni - jnj + jpvode + 1
             jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rminus
             jnij = jpvode + 1
             jacvode(ji, jnij, jni) = jacvode(ji, jnij, jni) - rminus - zfact * irrig(ji,jpksed)
          ENDIF
       END DO

       ! Interior of the sediment column
       DO jk = 2, jpksed - 1
          DO ji = 1, jpoce
             IF (accmask(ji) == 0 ) THEN
                rminus  = aminuss(ji,jk) * diff(ji,jk-1,jn) * radssol(jk,jn)
                rplus   = apluss (ji,jk) * diff(ji,jk,jn) * radssol(jk,jn)

                jni = (jk-1) * jpvode + jnn
                jnj = (jk-2) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rminus
                jnj = (jk-1) * jpvode + jnn
                jnij = jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - ( rminus + rplus ) - zfact * irrig(ji,jk)
                jnj = (jk) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rplus
             ENDIF
          END DO
       END DO

       IF( ln_timing )  CALL timing_stop('sed_mat_dsrjac')

    END SUBROUTINE sed_mat_dsrjac

    SUBROUTINE sed_mat_btbi( nvar, psol, preac, dtsed_in )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btbi  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!               associated to bioturbation
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!        2 - forward/backward substitution. 
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) :: nvar      ! number of sediment levels

       REAL(wp), DIMENSION(jpoce,jpksed,nvar), INTENT(inout) :: psol
       REAL(wp), DIMENSION(jpoce,jpksed,nvar), INTENT(in) :: preac
       REAL(wp), INTENT(in) :: dtsed_in

       !---Local declarations
       INTEGER  ::  ji, jk, jn

       REAL(wp) ::  aplus, aminus, rplus, rminus, dxplus, dxminus
       REAL(wp), DIMENSION(jpoce)           :: zbet
       REAL(wp), DIMENSION(jpoce,jpksed)    :: za, zb, zc
       REAL(wp), DIMENSION(jpoce,jpksed)    :: zr, zgamm
       !----------------------------------------------------------------------

       ! Computation left hand side of linear system of 
       ! equations for bioturbation
       !---------------------------------------------
       IF( ln_timing )  CALL timing_start('sed_mat_btbi')

       aplus  = ( dtsed_in / vols(2) ) * ( por1(2) + por1(3) ) / 2.0
       dxplus = ( dz(2) + dz(3) ) / 2.
       aminus = ( dtsed_in / vols(jpksed) ) * ( por1(jpksed-1) + por1(jpksed) ) * 0.5
       dxminus = ( dz(jpksed-1) + dz(jpksed) ) / 2.
       DO ji = 1, jpoce
          ! first sediment level
          rplus    = db(ji,2) * aplus / dxplus
          za(ji,2) = 0.
          zb(ji,2) = 1. + rplus
          zc(ji,2) = -rplus

          ! bottom sediment level
          rminus        = db(ji,jpksed-1) * aminus / dxminus
          za(ji,jpksed) = -rminus
          zb(ji,jpksed) = 1. + rminus
          zc(ji,jpksed) = 0.
       END DO

          ! interior of the sediment column
       DO jk = 3, jpksed - 1
          aminus  = ( dtsed_in / vols(jk) ) * ( por1(jk-1) + por1(jk) ) * 0.5
          dxminus = ( dz(jk-1) + dz(jk) ) / 2.
          aplus   = ( dtsed_in / vols(jk) ) * ( por1(jk) + por1(jk+1) ) * 0.5
          dxplus  = ( dz(jk) + dz(jk+1) ) / 2.
          DO ji = 1, jpoce
             rminus  = db(ji,jk-1) * aminus / dxminus
             !
             rplus   = db(ji,jk) * aplus / dxplus
             !     
             za(ji,jk) = -rminus
             zb(ji,jk) = 1. + rminus + rplus
             zc(ji,jk) = -rplus
          END DO
       END DO

       ! solves tridiagonal system of linear equations 
       ! -----------------------------------------------    
       DO jn = 1, nvar
          IF (isvode(jpwat+jn) == 0) THEN
             DO ji = 1, jpoce
                zr(ji,:)      = psol(ji,:,jn)
                zbet(ji)      = zb(ji,2) - preac(ji,2,jn) * dtsed_in
                psol(ji,2,jn) = zr(ji,2) / zbet(ji)
             END DO
                ! 
             DO jk = 3, jpksed
                DO ji = 1, jpoce
                   zgamm(ji,jk) =  zc(ji,jk-1) / zbet(ji)
                   zbet(ji)     =  zb(ji,jk) - preac(ji,jk,jn) * dtsed_in - za(ji,jk) * zgamm(ji,jk)
                   psol(ji,jk,jn) = ( zr(ji,jk) - za(ji,jk) * psol(ji,jk-1,jn) ) / zbet(ji)
                END DO
             END DO
                ! 
             DO jk = jpksed - 1, 2, -1
                DO ji = 1, jpoce
                   psol(ji,jk,jn) = psol(ji,jk,jn) - zgamm(ji,jk+1) * psol(ji,jk+1,jn)
                END DO
             END DO
          ENDIF
       END DO
       !
       IF( ln_timing )  CALL timing_stop('sed_mat_btbi')

    END SUBROUTINE sed_mat_btbi

    SUBROUTINE sed_mat_ads( nvar, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_ads  ***
       !!
       !! ** Purpose :  Computes the SMS due to bioturbation for adsorbed 
       !!               solute species
       !!
       !! ** Method  : 
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) :: nvar      ! Number of the adsorbed species
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask

       !---Local declarations
       INTEGER  ::  ji, jk, jn

       REAL(wp) ::  aplus, aminus, rplus, rminus, dxplus, dxminus
       !----------------------------------------------------------------------

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       IF( ln_timing )  CALL timing_start('sed_mat_ads')

       jn = nvar

       ! first and last sediment levels
       ! Computes the SMS (tridiagonal system)
       ! -------------------------------------
       aplus  = ( 1.0 / vols(2) ) * ( por1(2) + por1(3) ) * 0.5 * rads1sol(3,jn)   &
          &     / ( por(2) + rads1sol(2,jn) )
       dxplus = ( dz(2) + dz(3) ) / 2.
       aminus = ( 1.0 / vols(jpksed) ) * ( por1(jpksed-1) + por1(jpksed) ) * 0.5 * rads1sol(jpksed-1,jn)   &
          &     / ( por(jpksed) + rads1sol(jpksed,jn) )
       dxminus = ( dz(jpksed-1) + dz(jpksed) ) / 2.
       DO ji = 1, jpoce
          IF (accmask(ji) == 0) THEN
             rplus  = db(ji,2) * aplus / dxplus
             pwcpa(ji,2,jn) = pwcpa(ji,2,jn) + rplus * ( pwcp(ji,3,jn) - pwcp(ji,2,jn) )
 
             rminus = db(ji,jpksed-1) * aminus / dxminus
             pwcpa(ji,jpksed,jn) = pwcpa(ji,jpksed,jn) + rminus * ( pwcp(ji,jpksed-1,jn)    &
             &                     - pwcp(ji,jpksed,jn) )
          ENDIF
       END DO

       DO jk = 3, jpksed-1
          aminus  = ( 1.0 / vols(jk) ) * ( por1(jk-1) + por1(jk) ) * 0.5 * rads1sol(jk-1,jn)   &
            &       / ( por(jk) + rads1sol(jk,jn) )
          dxminus = ( dz(jk-1) + dz(jk) ) / 2.
          aplus   = ( 1.0 / vols(jk) ) * ( por1(jk) + por1(jk+1) ) * 0.5 * rads1sol(jk+1,jn)   &
            &       / ( por(jk) + rads1sol(jk,jn) )
          dxplus  = ( dz(jk) + dz(jk+1) ) / 2.
          DO ji = 1, jpoce
             IF (accmask(ji) == 0) THEN
                rminus  = db(ji,jk-1) * aminus / dxminus
                rplus   = db(ji,jk) * aplus / dxplus
                !     
                pwcpa(ji,jk,jn) =  pwcpa(ji,jk,jn) + ( rplus * pwcp(ji,jk+1,jn) + rminus * pwcp(ji,jk-1,jn)    &
                &                  - ( rplus + rminus ) * pwcp(ji,jk,jn) )
             ENDIF
          END DO
       END DO
       !
       IF( ln_timing )  CALL timing_stop('sed_mat_ads')
       
    END SUBROUTINE sed_mat_ads

    SUBROUTINE sed_mat_adsjac( nvar, NEQ, NROWPD, jacvode, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_adsjac  ***
       !!
       !! ** Purpose :  Computes the jacobian of diffusion due to bioturbation
       !!               for adsorbed solute species
       !!
       !! ** Method  : 
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar, NEQ, NROWPD  ! number of variable
       REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask

       !---Local declarations
       INTEGER  ::  ji, jk, jn, jnn, jni, jnj ,jnij
       REAL(wp) ::  aplus, aminus, rplus, rminus, dxplus, dxminus
       !----------------------------------------------------------------------

      ! Computation left hand side of linear system of 
      ! equations for dissolution reaction
      !---------------------------------------------
      IF( ln_timing )  CALL timing_start('sed_mat_adsjac')

      jn = nvar

      ! Computes the jacobian (tridiagonal system) which is 
      ! stored in a condensed form
      ! ---------------------------------------------------    
      jnn = isvode(jn)

            ! Top sediment level
      aplus  = ( 1.0 / vols(2) ) * ( por1(2) + por1(3) ) * 0.5 * rads1sol(3,jn)  &
        &      / ( por(2) + rads1sol(2,jn) )
      dxplus = ( dz(2) + dz(3) ) / 2.
      aminus = ( 1.0 / vols(jpksed) ) * ( por1(jpksed-1) + por1(jpksed) ) * 0.5 * rads1sol(jpksed-1,jn)  &
        &      / ( por(jpksed) + rads1sol(jpksed,jn) )
      dxminus = ( dz(jpksed-1) + dz(jpksed) ) / 2.
      DO ji = 1, jpoce
         IF ( accmask(ji) == 0 ) THEN
            rplus  = db(ji,2) * aplus / dxplus
            ! Top sediment level
            jni = jpvode + jnn
            jnij = jpvode + 1
            jacvode(ji, jnij, jni) = jacvode(ji,jnij,jni) - rplus
            jnj = 2 * jpvode + jnn
            jnij = jni - jnj + jpvode + 1
            jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rplus

            rminus  = db(ji,jpksed-1) * aminus / dxminus
            ! Bottom sediment level
            jni = (jpksed-1) * jpvode + jnn
            jnj = (jpksed-2) * jpvode + jnn
            jnij = jni - jnj + jpvode + 1
            jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rminus
            jnij = jpvode + 1
            jacvode(ji, jnij, jni) = jacvode(ji, jnij, jni) - rminus
         ENDIF
      END DO

            ! Interior of the sediment column
      DO jk = 3, jpksed-1
         aminus  = ( 1.0 / vols(jk) ) * ( por1(jk-1) + por1(jk) ) * 0.5 * rads1sol(jk-1,jn)   &
               &         / ( por(jk) + rads1sol(jk,jn) )
         dxminus = ( dz(jk-1) + dz(jk) ) * 0.5
         aplus   = ( 1.0 / vols(jk) ) * ( por1(jk) + por1(jk+1) ) * 0.5 * rads1sol(jk+1,jn)   &
               &         / ( por(jk) + rads1sol(jk,jn) )
         dxplus  = ( dz(jk) + dz(jk+1) ) * 0.5
         DO ji = 1, jpoce
            IF ( accmask(ji) == 0 ) THEN
               rminus  = db(ji,jk-1) * aminus / dxminus
               rplus   = db(ji,jk) * aplus / dxplus
               !     
               jni = (jk-1) * jpvode + jnn
               jnj = (jk-2) * jpvode + jnn
               jnij = jni - jnj + jpvode + 1
               jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rminus
               jnj = (jk-1) * jpvode + jnn
               jnij = jni - jnj + jpvode + 1
               jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - ( rminus + rplus )
               jnj = (jk) * jpvode + jnn
               jnij = jni - jnj + jpvode + 1
               jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rplus
            ENDIF
         END DO
      END DO
      !
      IF( ln_timing )  CALL timing_stop('sed_mat_adsjac')

    END SUBROUTINE sed_mat_adsjac

    SUBROUTINE sed_mat_btb( nvar, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btb  ***
       !!
       !! ** Purpose :  Computes the SMS due to bioturbation for solid species
       !!
       !! ** Method  : 
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) :: nvar    ! Number of solid species
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask

       !---Local declarations
       INTEGER  ::  ji, jk, jn
       REAL(wp) ::  aplus, aminus, rplus, rminus, dxplus, dxminus
       !----------------------------------------------------------------------

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       IF( ln_timing )  CALL timing_start('sed_mat_btb')

       jn = nvar

       ! Computes the SMS due to bioturbation (tridiagonal system)
       ! ---------------------------------------------------------    
       aplus  = ( 1.0 / vols(2) ) * ( por1(2) + por1(3) ) / 2.0
       dxplus = ( dz(2) + dz(3) ) / 2.
       aminus = ( 1.0 / vols(jpksed) ) * ( por1(jpksed-1) + por1(jpksed) ) * 0.5
       dxminus = ( dz(jpksed-1) + dz(jpksed) ) / 2.
       DO ji = 1, jpoce
          IF ( accmask(ji) == 0 ) THEN
             rplus  = db(ji,2) * aplus / dxplus
             solcpa(ji,2,jn) = solcpa(ji,2,jn) + rplus * ( solcp(ji,3,jn) - solcp(ji,2,jn) )
             rminus = db(ji,jpksed-1) * aminus / dxminus
             solcpa(ji,jpksed,jn) = solcpa(ji,jpksed,jn) + rminus * ( solcp(ji,jpksed-1,jn)    &
             &                     - solcp(ji,jpksed,jn) )
          ENDIF
       END DO

       DO jk = 3, jpksed-1
          aminus  = ( 1.0 / vols(jk) ) * ( por1(jk-1) + por1(jk) ) * 0.5
          dxminus = ( dz(jk-1) + dz(jk) ) / 2.
          aplus   = ( 1.0 / vols(jk) ) * ( por1(jk) + por1(jk+1) ) * 0.5
          dxplus  = ( dz(jk) + dz(jk+1) ) / 2.
          DO ji = 1, jpoce
             IF ( accmask(ji) == 0 ) THEN
                rminus = db(ji,jk-1) * aminus / dxminus
                rplus  = db(ji,jk) * aplus / dxplus
                solcpa(ji,jk,jn) = solcpa(ji,jk,jn) + ( rplus * solcp(ji,jk+1,jn) + rminus * solcp(ji,jk-1,jn)    &
                &                  - ( rminus + rplus ) * solcp(ji,jk,jn) )
             ENDIF
          END DO
       END DO
       !
       IF( ln_timing )  CALL timing_stop('sed_mat_btb')

    END SUBROUTINE sed_mat_btb

    SUBROUTINE sed_mat_btbjac( nvar, NEQ, NROWPD, jacvode, accmask )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btbjac  ***
       !!
       !! ** Purpose :  Computes the jacobian of bioturbation of solid species
       !!
       !! ** Method  : 
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar, NEQ, NROWPD  ! number of variable
       REAL, DIMENSION(jpoce,NROWPD,NEQ), INTENT(inout) :: jacvode
       INTEGER, DIMENSION(jpoce), INTENT(in) :: accmask

       !---Local declarations
       INTEGER  ::  ji, jk, jn, jnn, jni, jnj ,jnij
       REAL(wp) ::  aplus, aminus, rplus, rminus, dxplus, dxminus

       !----------------------------------------------------------------------

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       IF( ln_timing )  CALL timing_start('sed_mat_btbjac')

       jn = nvar

       ! Computes the jacobian (tridiagonal system) that is stored
       ! in a condensed form
       ! ---------------------------------------------------------    
       jnn = isvode(jpwat+jn)

       ! Top and bottom sediment levels
       aplus  = ( 1.0 / vols(2) ) * ( por1(2) + por1(3) ) * 0.5
       dxplus = ( dz(2) + dz(3) ) / 2.
       aminus = ( 1.0 / vols(jpksed) ) * ( por1(jpksed-1) + por1(jpksed) ) * 0.5
       dxminus = ( dz(jpksed-1) + dz(jpksed) ) / 2.
       DO ji = 1, jpoce
          IF (accmask(ji) == 0) THEN
             rplus  = db(ji,2) * aplus / dxplus
             jni = jpvode + jnn
             jnij = jpvode + 1
             jacvode(ji, jnij, jni) = jacvode(ji,jnij,jni) - rplus
             jnj = 2 * jpvode + jnn
             jnij = jni - jnj + jpvode + 1
             jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rplus

             rminus  = db(ji,jpksed-1) * aminus / dxminus
             jni = (jpksed-1) * jpvode + jnn
             jnj = (jpksed-2) * jpvode + jnn
             jnij = jni - jnj + jpvode + 1
             jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rminus
             jnij = jpvode + 1
             jacvode(ji, jnij, jni) = jacvode(ji, jnij, jni) - rminus
          ENDIF
       END DO

       ! Interior of the sediment level
       DO jk = 3, jpksed-1
          aminus  = ( 1.0 / vols(jk) ) * ( por1(jk-1) + por1(jk) ) * 0.5
          dxminus = ( dz(jk-1) + dz(jk) ) / 2.
          aplus   = ( 1.0 / vols(jk) ) * ( por1(jk) + por1(jk+1) ) * 0.5
          dxplus  = ( dz(jk) + dz(jk+1) ) / 2.
          DO ji = 1, jpoce
             IF (accmask(ji) == 0) THEN
                rminus = db(ji,jk-1) * aminus / dxminus
                rplus  = db(ji,jk) * aplus / dxplus
                jni = (jk-1) * jpvode + jnn
                jnj = (jk-2) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rminus
                jnj = (jk-1) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) - rminus - rplus
                jnj = (jk) * jpvode + jnn
                jnij = jni - jnj + jpvode + 1
                jacvode(ji, jnij, jnj) = jacvode(ji, jnij, jnj) + rplus
             ENDIF
          END DO
       END DO
       !
       IF( ln_timing )  CALL timing_stop('sed_mat_btbjac')

    END SUBROUTINE sed_mat_btbjac

    SUBROUTINE sed_mat_dsri( nvar, preac, psms, dtsed_in, psol )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsri  ***
       !!
       !! ** Purpose :  solves tridiagonal system of linear equations 
       !!               for solute species
       !!
       !! ** Method  : 
       !!        1 - computes left hand side of linear system of equations
       !!            for dissolution reaction
       !!        2 - forward/backward substitution. 
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar  ! number of variable

       REAL(wp), DIMENSION(jpoce,jpksed), INTENT(in   ) :: preac  ! reaction rates
       REAL(wp), DIMENSION(jpoce,jpksed), INTENT(in   ) :: psms  ! reaction rates
       REAL(wp), DIMENSION(jpoce,jpksed), INTENT(inout) :: psol  ! reaction rates
       REAL(wp), INTENT(in) ::  dtsed_in

       !---Local declarations
       INTEGER  ::  ji, jk, jn
       REAL(wp), DIMENSION(jpoce,jpksed) :: za, zb, zc, zr
       REAL(wp), DIMENSION(jpoce)        :: zbet
       REAL(wp), DIMENSION(jpoce,jpksed) :: zgamm

       REAL(wp) ::  rplus,rminus
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_dsri')

       ! Computation left hand side of linear system of 
       ! equations for dissolution reaction
       !---------------------------------------------
       jn = nvar
       ! first sediment level          
       DO ji = 1, jpoce
          rplus  = dtsed_in * apluss(ji,1) * diff(ji,1,jn) * radssol(1,jn)

          za(ji,1) = 0.
          zb(ji,1) = 1. + rplus
          zc(ji,1) = -rplus

          rminus  = dtsed_in * aminuss(ji,jpksed) * diff(ji,jpksed-1,jn) * radssol(jpksed,jn)
          !
          za(ji,jpksed) = -rminus
          zb(ji,jpksed) = 1. + rminus
          zc(ji,jpksed) = 0.
       ENDDO

       DO jk = 2, jpksed - 1
          DO ji = 1, jpoce
             rminus  = dtsed_in * aminuss(ji,jk) * diff(ji,jk-1,jn) * radssol(jk,jn)
             rplus   = dtsed_in * apluss (ji,jk) * diff(ji,jk,jn) * radssol(jk,jn)
                !     
             za(ji,jk) = -rminus
             zb(ji,jk) = 1. + rminus + rplus
             zc(ji,jk) = -rplus
          END DO
       END DO

       ! solves tridiagonal system of linear equations 
       ! -----------------------------------------------
       DO ji = 1, jpoce
          zr  (ji,:) = psol(ji,:) + (psms(ji,:) + irrig(ji,:) * psol(ji,1) ) * dtsed_in
          zb  (ji,:) = zb(ji,:) - (preac(ji,:) - irrig(ji,:) ) * dtsed_in
          zbet(ji  ) = zb(ji,1)
          psol(ji,1) = zr(ji,1) / zbet(ji)
       END DO
          ! 
       DO jk = 2, jpksed
          DO ji = 1, jpoce
             zgamm(ji,jk) =  zc(ji,jk-1) / zbet(ji)
             zbet(ji)     =  zb(ji,jk) - za(ji,jk) * zgamm(ji,jk)
             psol(ji,jk)  = ( zr(ji,jk) - za(ji,jk) * psol(ji,jk-1) ) / zbet(ji)
          END DO
       ENDDO
          ! 
       DO jk = jpksed - 1, 1, -1
          DO ji = 1, jpoce
             psol(ji,jk) = psol(ji,jk) - zgamm(ji,jk+1) * psol(ji,jk+1)
          END DO
       ENDDO

       DO jk = 2, jpksed
          DO ji = 1, jpoce
             xirrigtrd(ji,jn) = xirrigtrd(ji,jn) - irrig(ji,jk) * (psol(ji,1) - psol(ji,jk) ) * volw3d(ji,jk) * dtsed_in
          END DO
       END DO  

       IF( ln_timing )  CALL timing_stop('sed_mat_dsri')

    END SUBROUTINE sed_mat_dsri

    SUBROUTINE sed_mat_dsre( nvar, preac, psms, dtsed_in, psol )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_dsre  ***
       !!
       !! ** Purpose :  Computes diffusion of solute species
       !!
       !! ** Method  : 
       !!        1 - A BDF3 (3rd order) implicit scheme is used
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar  ! number of variable

       REAL(wp), DIMENSION(jpoce,jpksed), INTENT(in   ) :: preac  ! reaction rates
       REAL(wp), DIMENSION(jpoce,jpksed), INTENT(in   ) :: psms  ! reaction rates
       REAL(wp), DIMENSION(jpoce,jpksed), INTENT(inout) :: psol  ! reaction rates
       REAL(wp), INTENT(in) ::  dtsed_in

       !---Local declarations
       INTEGER  ::  ji, jk, jn
       REAL(wp), DIMENSION(jpoce,jpksed) :: psol1,psol2

       REAL(wp) ::  zirrigt
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_dsre')

       jn = nvar

       ! First step of BDF3
       psol1(:,:) = psol(:,:)
       CALL sed_mat_dsri( jn, preac(:,:), psms(:,:), dtsed_in/3., psol1(:,:) )

       DO jk = 2, jpksed
          DO ji = 1, jpoce
             xirrigtrd(ji,jn) = xirrigtrd(ji,jn) - irrig(ji,jk) * (psol1(ji,1) - psol1(ji,jk) ) * volw3d(ji,jk) * 5.0 * dtsed_in / 11.0
          END DO
       END DO

       ! Second step of BDF3
       psol2(:,:) = 4./3. * psol1(:,:) - 1./3. * psol(:,:)
       CALL sed_mat_dsri( jn, preac(:,:), psms(:,:), 2.0*dtsed_in/9., psol2(:,:) )

       DO jk = 2, jpksed
          DO ji = 1, jpoce
             xirrigtrd(ji,jn) = xirrigtrd(ji,jn) - irrig(ji,jk) * (psol2(ji,1) - psol2(ji,jk) ) * volw3d(ji,jk) * 4.0 * dtsed_in / 11.0
          END DO
       END DO

       ! Third step of BDF3
       psol(:,:) = 18.0 / 11.0 * psol2(:,:) - 9.0 / 11.0 * psol1(:,:) + 2.0 / 11.0 * psol(:,:)
       CALL sed_mat_dsri( jn, preac(:,:), psms(:,:), 2.0*dtsed_in/11., psol(:,:) )

       DO jk = 2, jpksed
          DO ji = 1, jpoce
             xirrigtrd(ji,jn) = xirrigtrd(ji,jn) - irrig(ji,jk) * (psol(ji,1) - psol(ji,jk) ) * volw3d(ji,jk) * 2.0 * dtsed_in / 11.0
          END DO
       END DO

       IF( ln_timing )  CALL timing_stop('sed_mat_dsre')

    END SUBROUTINE sed_mat_dsre


    SUBROUTINE sed_mat_btbe( nvar, psol, preac, dtsed_in )
       !!---------------------------------------------------------------------
       !!                  ***  ROUTINE sed_mat_btbe  ***
       !!
       !! ** Purpose :  Computes bioturbation of solid species
       !!
       !! ** Method  : 
       !!        A BDF3 (3rd order) implicit scheme is used
       !!
       !!   History :
       !!----------------------------------------------------------------------
       !! * Arguments
       INTEGER , INTENT(in) ::  nvar  ! number of variable

       REAL(wp), DIMENSION(jpoce,jpksed,nvar), INTENT(in   ) :: preac  ! reaction rates
       REAL(wp), DIMENSION(jpoce,jpksed,nvar), INTENT(inout) :: psol  ! reaction rates
       REAL(wp), INTENT(in) ::  dtsed_in

       !---Local declarations
       INTEGER  ::  ji, jk
       REAL(wp), DIMENSION(jpoce,jpksed,nvar) :: psol1, psol2
       !----------------------------------------------------------------------

       IF( ln_timing )  CALL timing_start('sed_mat_btbe')

       ! First step
       psol1(:,:,:) = psol(:,:,:)
       CALL sed_mat_btbi( nvar, psol1(:,:,:), preac(:,:,:), dtsed_in/3. )

       ! Second step
       psol2(:,:,:) = 4./3. * psol1(:,:,:) - 1./3. * psol(:,:,:)
       CALL sed_mat_btbi( nvar, psol2(:,:,:), preac(:,:,:), 2.0*dtsed_in/9. )

       ! Third step
       psol(:,:,:) = 18.0 / 11.0 * psol2(:,:,:) - 9.0 / 11.0 * psol1(:,:,:) + 2.0 / 11.0 * psol(:,:,:)
       CALL sed_mat_btbi( nvar, psol(:,:,:), preac(:,:,:), 2.0*dtsed_in/11. )


       IF( ln_timing )  CALL timing_stop('sed_mat_btbe')

    END SUBROUTINE sed_mat_btbe


 END MODULE sedmat
