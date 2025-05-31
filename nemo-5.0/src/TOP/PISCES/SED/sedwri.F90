MODULE sedwri
   !!======================================================================
   !!                     ***  MODULE  sedwri  ***
   !!         Sediment diagnostics :  write sediment output files
   !!======================================================================
   USE sed
   USE sedinorg
   USE lib_mpp         ! distribued memory computing library
   USE iom
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   !! * Accessibility
   PUBLIC sed_wri 

CONTAINS

   !!----------------------------------------------------------------------
   !!                                                   NetCDF output file
   !!----------------------------------------------------------------------
   SUBROUTINE sed_wri( kt )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_wri  ***
      !!
      !! ** Purpose :  output of sediment passive tracer
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------

      INTEGER, INTENT(in) :: kt

      INTEGER  :: ji, jj, jk, jn
      INTEGER  :: it
      CHARACTER(len = 20)  ::  cltra 
      REAL(wp) :: zinvdtsed
      REAL(wp), DIMENSION(jpoce, jptrased+1) :: zflx
      REAL(wp), DIMENSION(jpi, jpj, jpksed)   :: trcsedi
      REAL(wp), DIMENSION(jpi, jpj) :: flxsedi2d
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zw3d
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)     :: zdta
      !!-------------------------------------------------------------------

      ! 1.  Initilisations
      ! -----------------------------------------------------------------
      IF( ln_timing )  CALL timing_start('sed_wri')
!
      IF (lwp) WRITE(numsed,*) ' '
      IF (lwp) WRITE(numsed,*) 'sed_wri kt = ', kt
      IF (lwp) WRITE(numsed,*) ' '
      
      ! Initialize variables
      ! --------------------
      zinvdtsed          = 1.0_wp / dtsed

      ! 2.  Back to 2D geometry
      ! -----------------------------------------------------------------
      ! Calculation of fluxes mol/cm2/s
      DO jn = 1, jpwat
         DO ji = 1, jpoce
            zflx(ji,jn) = ( pwcp(ji,1,jn) - pwcp_dta(ji,jn) ) * ( 1.e-3 * dzkbot(ji) ) * zinvdtsed
         ENDDO
      ENDDO

      ! Calculation of fluxes g/cm2/s
      ! Calculation of accumulation rate per dt
      zflx(:,jptrased+1) = 0.0
      DO jn = 1, jpsol
         DO ji = 1, jpoce
            zflx(ji,jpwat+jn) = ( tosed(ji,jn) - fromsed(ji,jn) ) * zinvdtsed
            zflx(ji,jptrased+1) = zflx(ji,jptrased+1) + ( tosed(ji,jn) - fromsed(ji,jn) ) / ( dtsed * por1(jpksed) * dens_sol(jn) )
         ENDDO
      ENDDO

      !
      ! Start writing data
      ! ---------------------
     DO jn = 1, jptrased
         cltra = TRIM( sedtrcd(jn) ) ! short title for 3D diagnostic
         IF ( iom_use( cltra ) ) THEN
            IF ( jn <= jpsol ) THEN
               DO jk = 1, jpksed
                  trcsedi(:,:,jk) = UNPACK( solcp(:,jk,jn), sedmask == 1.0, 0.0 )
               END DO
            ELSE
               DO jk = 1, jpksed
                  trcsedi(:,:,jk) = UNPACK( pwcp(:,jk,jn-jpsol)*1E6, sedmask == 1.0, 0.0 )
               END DO
            ENDIF
            CALL iom_put( cltra, trcsedi(:,:,:) )
         ENDIF
      END DO

      DO jn = 1, jptrased+1
         cltra = TRIM( seddia2d(jn) ) ! short title for 2D diagnostic
         IF ( iom_use( cltra ) ) THEN
            flxsedi2d(:,:) = UNPACK( zflx(:,jn), sedmask == 1.0, 0.0 )
            CALL iom_put( cltra, flxsedi2d(:,:) )
         ENDIF
      END DO

      IF ( iom_use( "dzdep" ) ) THEN
         zflx(:,1) = dzdep(:) * zinvdtsed
         flxsedi2d(:,:) = UNPACK( zflx(:,1), sedmask == 1.0, 0.0 )
         CALL iom_put( "dzdep", flxsedi2d(:,:) )
      ENDIF

      IF ( iom_use( "Rstepros" ) ) THEN
         flxsedi2d(:,:) = UNPACK( rstepros(:), sedmask == 1.0, 0.0 )
         CALL iom_put( "Rstepros", flxsedi2d(:,:) ) 
      ENDIF

      IF ( iom_use( "SaturCO3" ) .OR. iom_use( "SedCO3por" ) .OR. iom_use( "SedpH" ) )  THEN

         ALLOCATE( zw3d(jpi,jpj,jpksed) )
      ENDIF
      IF ( iom_use( "SaturCO3" ) ) THEN

         DO jk = 1, jpksed
            DO ji = 1, jpoce
               saturco3(ji,jk) = (1.0 - co3por(ji,jk) /  co3sat(ji) )
            END DO
            zw3d(:,:,jk) = UNPACK( saturco3(:,jk), sedmask == 1.0, 0.0)
         END DO
         CALL iom_put( "SaturCO3", zw3d )
      ENDIF
      IF ( iom_use( "SedCO3por" ) ) THEN
         DO jk = 1, jpksed
            zw3d(:,:,jk) = UNPACK( co3por(:,jk), sedmask == 1.0, 0.0)
         END DO
         CALL iom_put( "SedCO3por", zw3d )
      ENDIF
      IF ( iom_use( "SedpH" ) ) THEN
         ALLOCATE( zdta(jpoce,jpksed) )

         DO jk = 1, jpksed
            DO ji = 1, jpoce
               zdta(ji,jk) = -LOG10( hipor(ji,jk) / ( densSW(ji) + rtrn ) + rtrn )
            END DO
            zw3d(:,:,jk) = UNPACK( zdta(:,jk), sedmask == 1.0, 0.0)
         END DO
         CALL iom_put( "SedpH", zw3d )
         DEALLOCATE( zdta )
      ENDIF

      IF ( iom_use( "SaturCO3" ) .OR. iom_use( "SedCO3por" ) .OR. iom_use( "SedpH" ) )  &
         &     DEALLOCATE( zw3d )


      IF( ln_timing )  CALL timing_stop('sed_wri')

   END SUBROUTINE sed_wri

END MODULE sedwri
