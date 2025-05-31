MODULE diadetide
   !!======================================================================
   !!                      ***  MODULE diadetide  ***
   !! Computation of weights for daily detided model diagnostics
   !!======================================================================
   !! History :       !  2019  (S. Mueller)
   !!----------------------------------------------------------------------
   USE par_oce        
   USE in_out_manager 
   USE iom            
   USE dom_oce        
   USE phycst        
   USE tide_mod
#if defined key_xios
   USE xios
#endif

   IMPLICIT NONE
   PRIVATE

   LOGICAL, PUBLIC                               ::   l_diadetide !:
   INTEGER                                       ::   ndiadetide
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:)     ::   tdiadetide

   PUBLIC ::   dia_detide_init, dia_detide

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dia_detide_init
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE dia_detide_init  ***
      !!
      !! ** Purpose : initialisation of the weight computation for daily
      !!              detided diagnostics (currently M2-detiding only)
      !!
      !!----------------------------------------------------------------------

      REAL(wp)                                   ::   zdt
      INTEGER                                    ::   jn
      CHARACTER (LEN=4), DIMENSION(jpmax_harmo)  ::   ctide_selected = ' n/a '
      TYPE(tide_harmonic), DIMENSION(:), POINTER ::   stideconst

      l_diadetide = .FALSE.
#if defined key_xios
      ! Enquire detiding activation state (test for presence of detiding-related
      ! weights field and output file group)
      IF ( xios_is_valid_field( "diadetide_weight" ).AND.xios_is_valid_filegroup( "diadetide_files" ).AND.ln_tide ) THEN
         l_diadetide = .TRUE.
      END IF
#endif

      IF (lwp) THEN
         WRITE (numout, *)
         WRITE (numout, *) 'dia_detide_init : weight computation for daily detided model diagnostics'
         WRITE (numout, *) '~~~~~~~~~~~~~~~'
         WRITE (numout, *) '                  l_diadetide = ', l_diadetide
      END IF

      IF (l_diadetide) THEN
         ! Retrieve information about M2 tidal constituent
         ctide_selected(1) = 'M2'
         CALL tide_init_harmonics(ctide_selected, stideconst) 

         ! For M2, twice the tidal period spans slightly more than one full
         ! day. Compute the maximum number of equal intervals that span exactly
         ! twice the tidal period *and* whose mid-points fall within a 24-hour
         ! period from midnight to midnight.
         zdt = 2.0_wp * 2.0_wp * rpi / stideconst(1)%omega
         ndiadetide = FLOOR( zdt / ( zdt - 86400.0_wp ) )
         ! Compute mid-points of the intervals to be included in the detided
         ! average
         ALLOCATE ( tdiadetide(ndiadetide) )
         DO jn = 1, ndiadetide
            tdiadetide(jn) = ( REAL( jn, KIND=wp) - 0.5_wp ) * zdt / REAL( ndiadetide, KIND=wp ) - ( zdt - 86400.0_wp ) * 0.5_wp
         END DO
      END IF

   END SUBROUTINE dia_detide_init

   SUBROUTINE dia_detide( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_detide  ***
      !!
      !! ** Purpose : weight computation for daily detided model diagnostics
      !!----------------------------------------------------------------------

      INTEGER, INTENT(in)          ::   kt
      REAL(wp), DIMENSION(T2D(0))  ::   zwght_2D
      REAL(wp)                     ::   zwght, ztmp
      INTEGER                      ::   ji, jj, jn

      ! Compute detiding weight at the current time-step; the daily total weight
      ! is one, and the daily summation of a diagnosed field multiplied by this
      ! weight should provide daily detided averages
      zwght = 0.0_wp
      DO jn = 1, ndiadetide
         ztmp = ( tdiadetide(jn) - REAL( nsec_day, KIND=wp ) ) / rn_Dt
         IF ( ( ztmp < 0.5_wp ).AND.( ztmp >= -0.5_wp ) ) THEN
            zwght = zwght + 1.0_wp / REAL( ndiadetide, KIND=wp )
         END IF
      END DO
 
      DO_2D( 0, 0, 0, 0 )
         zwght_2D(ji,jj) = zwght
      END_2D
      CALL iom_put( "diadetide_weight", zwght_2D)

   END SUBROUTINE dia_detide

END MODULE diadetide
