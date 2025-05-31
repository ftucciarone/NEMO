MODULE trcwri_c14
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    MY_SRC :   Additional outputs for C14 tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !! History :   2.0  !  2015 (A. Mouchet)  adapted code for C14
   !!----------------------------------------------------------------------
#if defined key_top && defined key_xios
   !!----------------------------------------------------------------------
   !! trc_wri_c14   :  outputs of ventilation fields
   !!----------------------------------------------------------------------
   USE oce_trc       ! Ocean variables
   USE trc           ! passive tracers common variables 
   USE iom           ! I/O manager 
   USE sms_c14
   USE lib_fortran   ! Fortran routines library

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_c14
   !
   !   Standard ratio: 1.176E-12 ; Avogadro's nbr = 6.022E+23 at/mol ; bomb C14 traditionally reported as 1.E+26 atoms
   REAL(wp), PARAMETER  :: atomc14 = 1.176 * 6.022E-15   ! conversion factor 

   REAL(wp) :: rrage
   LOGICAL  :: l_c14_flx, l_c14_delta, l_c14_invent

   !! * Substitutions
#  include "do_loop_substitute.h90"


CONTAINS

   SUBROUTINE trc_wri_c14( kt, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_c14  ***
      !!
      !! ** Purpose :   output additional C14 tracers fields 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt    ! time step
      INTEGER, INTENT(in)  :: Kmm   ! time level indices
      CHARACTER (len=20)   :: cltra         ! short title for tracer
      INTEGER              :: ji,jj,jk,jn   ! dummy loop indexes
      REAL(wp)             :: ztra
      REAL(wp), DIMENSION(4) :: ztemp   ! temporary
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: z2d1 ! temporary storage 2D
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   :: z2d ! temporary storage 2D
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: z3d ! temporary storage 3D
      !!---------------------------------------------------------------------

      IF( kt == nittrc000 ) THEN 
         l_c14_flx    = iom_use("qtr_c14") .OR. iom_use("qint_c14")    
         l_c14_delta  = iom_use("DeltaC14") .OR. iom_use("C14Age") .OR. iom_use("RAge")    
         l_c14_invent = iom_use("AtmC14") .OR. iom_use("K_C14") .OR.  &
              &         iom_use("K_CO2") .OR. iom_use("C14Inv") 
         rrage = -1._wp / rlam14 / rsiyea  ! factor for radioages in year
      ENDIF
 
      ! write the tracer concentrations in the file
      ! ---------------------------------------
      cltra = TRIM( ctrcnm(jp_c14) )                  ! short title for tracer
      CALL iom_put( cltra, tr(:,:,:,jp_c14,Kmm) )

      ! compute and write the tracer diagnostic in the file
      ! ---------------------------------------
      IF( l_c14_flx ) THEN
          CALL iom_put( "qtr_c14" , rsiyea * qtr_c14(:,:)  )   !  Radiocarbon surf flux [./m2/yr]
          CALL iom_put( "qint_c14", qint_c14(:,:)  )         ! cumulative flux [./m2]
      ENDIF
      
      IF( l_c14_delta ) THEN
         !
         ALLOCATE( z2d(A2D(0)), z3d(A2D(0),jpk) )
         !
         z3d(:,:,:)  = 1._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            IF( tmask(ji,jj,jk) > 0._wp) THEN
               z3d(ji,jj,jk) = tr(ji,jj,jk,jp_c14,Kmm)
            ENDIF
         END_3D
         CALL iom_put( "C14Age"   ,  rrage * LOG( z3d(:,:,:) ) )            !  Radiocarbon age [yr]
         CALL iom_put( "DeltaC14" ,  1.e03_wp * ( z3d(:,:,:) - 1._wp ) )    ! Delta C14 [permil]

         ! Reservoir age [yr]
         z2d(:,:) = 0._wp
         DO_2D( 0, 0, 0, 0 )
            ztra = z3d(ji,jj,1) / c14sbc(ji,jj)
            IF( ztra > 0._wp .AND. tmask(ji,jj,1) > 0._wp ) z2d(ji,jj) = LOG( ztra )
         END_2D
         CALL iom_put( "RAge" , rrage * z2d(:,:) )                     ! Reservoir age [yr]
         !
         DEALLOCATE( z2d, z3d )
         !
      ENDIF
      !
      !  0-D fields
      !
      CALL iom_put( "AtmCO2", co2sbc )  !     global atmospheric CO2 [ppm]
    
      IF( l_c14_invent ) THEN
         ALLOCATE( z2d1( A2D(0),4) )
         DO_2D( 0, 0, 0, 0 )
            z2d1(ji,jj,1) = e1e2t(ji,jj)       ! global ocean surface
            z2d1(ji,jj,2) = c14sbc(ji,jj) * e1e2t(ji,jj)
            z2d1(ji,jj,3) = exch_c14(ji,jj) * e1e2t(ji,jj)
            z2d1(ji,jj,4) = exch_co2(ji,jj) * e1e2t(ji,jj)
         END_2D
         !
         ztemp(1:4) = glob_2Dsum( 'trcwri_c14', z2d1(:,:,1:4)  ) 
         ! 
         CALL iom_put( "AtmC14" , ( ztemp(2) / ztemp(1) - 1._wp ) * 1000._wp )   ! Global atmospheric DeltaC14 [permil]
         CALL iom_put( "K_C14" , rsiyea * ztemp(3) / ztemp(1) )   ! global mean exchange velocity for C14/C ratio [m/yr]
         CALL iom_put( "K_CO2", 360000._wp * ztemp(4) / ztemp(1)   )  !  global mean CO2 piston velocity [cm/hr]
         !
         DEALLOCATE( z2d1 )
      END IF
      IF( iom_use("C14Inv") ) THEN
         ztemp = glob_3Dsum( 'trcwri_c14', tr(:,:,:,jp_c14,Kmm) * cvol(:,:,:) )
         ztemp = atomc14 * xdicsur * ztemp
         CALL iom_put( "C14Inv", ztemp )  !  Radiocarbon ocean inventory [10^26 atoms]
      END IF
      !
   END SUBROUTINE trc_wri_c14

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No C14 tracer
   !!----------------------------------------------------------------------
   IMPLICIT NONE
   PUBLIC trc_wri_c14
CONTAINS
   SUBROUTINE trc_wri_c14                     ! Empty routine  
   END SUBROUTINE trc_wri_c14
#endif

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trcwri_c14
