MODULE trabbc
   !!==============================================================================
   !!                       ***  MODULE  trabbc  ***
   !! Ocean active tracers:  bottom boundary condition (geothermal heat flux)
   !!==============================================================================
   !! History :  OPA  ! 1999-10 (G. Madec)  original code
   !!   NEMO     1.0  ! 2002-08 (G. Madec)  free form + modules
   !!             -   ! 2002-11 (A. Bozec)  tra_bbc_init: original code
   !!            3.3  ! 2010-10 (G. Madec)  dynamical allocation + suppression of key_trabbc
   !!             -   ! 2010-11 (G. Madec)  use mbkt array (deepest ocean t-level)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_bbc       : update the tracer trend at ocean bottom
   !!   tra_bbc_init  : initialization of geothermal heat flux trend
   !!----------------------------------------------------------------------
   USE oce            ! ocean variables
   USE dom_oce        ! domain: ocean
   USE phycst         ! physical constants
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends manager: tracers
   !
   USE in_out_manager ! I/O manager
   USE iom            ! xIOS
   USE fldread        ! read input fields
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! distributed memory computing library
   USE prtctl         ! Print control
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC tra_bbc          ! routine called by step.F90
   PUBLIC tra_bbc_init     ! routine called by opa.F90

   !                                 !!* Namelist nambbc: bottom boundary condition *
   LOGICAL, PUBLIC ::   ln_trabbc     !: Geothermal heat flux flag
   INTEGER         ::   nn_geoflx     !  Geothermal flux (=1:constant flux, =2:read in file )
   REAL(wp)        ::   rn_geoflx_cst !  Constant value of geothermal heat flux

   REAL(wp), PUBLIC , ALLOCATABLE, DIMENSION(:,:) ::   qgh_trd0   ! geothermal heating trend

   TYPE(FLD), ALLOCATABLE, DIMENSION(:) ::   sf_qgh   ! structure of input qgh (file informations, fields read)

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_bbc( kt, Kmm, pts, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_bbc  ***
      !!
      !! ** Purpose :   Compute the bottom boundary contition on temperature
      !!              associated with geothermal heating and add it to the
      !!              general trend of temperature equations.
      !!
      !! ** Method  :   The geothermal heat flux set to its constant value of
      !!              86.4 mW/m2 (Stein and Stein 1992, Huang 1999).
      !!       The temperature trend associated to this heat flux through the
      !!       ocean bottom can be computed once and is added to the temperature
      !!       trend juste above the bottom at each time step:
      !!            ta = ta + Qsf / (rho0 rcp e3T) for k= mbkt
      !!       Where Qsf is the geothermal heat flux.
      !!
      !! ** Action  : - update the temperature trends with geothermal heating trend
      !!              - send the trend for further diagnostics (ln_trdtra=T)
      !!
      !! References : Stein, C. A., and S. Stein, 1992, Nature, 359, 123-129.
      !!              Emile-Geay and Madec, 2009, Ocean Science.
      !!----------------------------------------------------------------------
      INTEGER,                                   INTENT(in   ) :: kt         ! ocean time-step index
      INTEGER,                                   INTENT(in   ) :: Kmm, Krhs  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts        ! active tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk    ! dummy loop indices
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrdt   ! 3D workspace
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_bbc')
      !
      IF( l_trdtra ) THEN           ! Save the input temperature trend
         ALLOCATE( ztrdt(T2D(0),jpk) )
         ztrdt(:,:,:) = pts(T2D(0),:,jp_tem,Krhs)
      ENDIF
      !                             !  Add the geothermal trend on temperature
      DO_2D( 0, 0, 0, 0 )
         pts(ji,jj,mbkt(ji,jj),jp_tem,Krhs) = pts(ji,jj,mbkt(ji,jj),jp_tem,Krhs)   &
            &             + qgh_trd0(ji,jj) / e3t(ji,jj,mbkt(ji,jj),Kmm)
      END_2D
      !
      IF( l_trdtra ) THEN        ! Send the trend for diagnostics
         ztrdt(:,:,:) = pts(T2D(0),:,jp_tem,Krhs) - ztrdt(:,:,:)
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_bbc, ztrdt )
         DEALLOCATE( ztrdt )
      ENDIF
      !
      CALL iom_put ( "hfgeou" , rho0_rcp * qgh_trd0(:,:) )

      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Krhs), clinfo1=' bbc  - Ta: ', mask1=tmask, clinfo3='tra-ta' )
      !
      IF( ln_timing )   CALL timing_stop('tra_bbc')
      !
   END SUBROUTINE tra_bbc


   SUBROUTINE tra_bbc_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_bbc_init  ***
      !!
      !! ** Purpose :   Compute once for all the trend associated with geothermal
      !!              heating that will be applied at each time step at the
      !!              last ocean level
      !!
      !! ** Method  :   Read the nambbc namelist and check the parameters.
      !!
      !! ** Input   : - Namlist nambbc
      !!              - NetCDF file  : geothermal_heating.nc ( if necessary )
      !!
      !! ** Action  : - read/fix the geothermal heat qgh_trd0
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj              ! dummy loop indices
      INTEGER  ::   inum                ! temporary logical unit
      INTEGER  ::   ios                 ! Local integer output status for namelist read
      INTEGER  ::   ierror              ! local integer
      !
      TYPE(FLD_N)        ::   sn_qgh    ! informations about the geotherm. field to be read
      CHARACTER(len=256) ::   cn_dir    ! Root directory for location of ssr files
      !!
      NAMELIST/nambbc/ln_trabbc, nn_geoflx, rn_geoflx_cst, sn_qgh, cn_dir
      !!----------------------------------------------------------------------
      !
      READ_NML_REF(numnam,nambbc)
      READ_NML_CFG(numnam,nambbc)
      IF(lwm) WRITE ( numond, nambbc )
      !
      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'tra_bbc : Bottom Boundary Condition (bbc), apply a Geothermal heating'
         WRITE(numout,*) '~~~~~~~   '
         WRITE(numout,*) '   Namelist nambbc : set bbc parameters'
         WRITE(numout,*) '      Apply a geothermal heating at ocean bottom   ln_trabbc     = ', ln_trabbc
         WRITE(numout,*) '      type of geothermal flux                      nn_geoflx     = ', nn_geoflx
         WRITE(numout,*) '      Constant geothermal flux value               rn_geoflx_cst = ', rn_geoflx_cst
         WRITE(numout,*)
      ENDIF
      !
      IF( ln_trabbc ) THEN             !==  geothermal heating  ==!
         !
         ALLOCATE( qgh_trd0(jpi,jpj) )    ! allocation
         !
         SELECT CASE ( nn_geoflx )        ! geothermal heat flux / (rauO * Cp)
         !
         CASE ( 1 )                          !* constant flux
            IF(lwp) WRITE(numout,*) '   ==>>>   constant heat flux  =   ', rn_geoflx_cst
            qgh_trd0(:,:) = r1_rho0_rcp * rn_geoflx_cst
            !
         CASE ( 2 )                          !* variable geothermal heat flux : read the geothermal fluxes in mW/m2
            IF(lwp) WRITE(numout,*) '   ==>>>   variable geothermal heat flux'
            !
            ALLOCATE( sf_qgh(1), STAT=ierror )
            IF( ierror > 0 ) THEN
               CALL ctl_stop( 'tra_bbc_init: unable to allocate sf_qgh structure' )   ;
               RETURN
            ENDIF
            ALLOCATE( sf_qgh(1)%fnow(jpi,jpj,1)   )
            IF( sn_qgh%ln_tint )   ALLOCATE( sf_qgh(1)%fdta(jpi,jpj,1,2) )
            ! fill sf_chl with sn_chl and control print
            CALL fld_fill( sf_qgh, (/ sn_qgh /), cn_dir, 'tra_bbc_init',   &
               &          'bottom temperature boundary condition', 'nambbc', no_print )

            CALL fld_read( nit000, 1, sf_qgh )                         ! Read qgh data
            qgh_trd0(:,:) = r1_rho0_rcp * sf_qgh(1)%fnow(:,:,1) * 1.e-3 ! conversion in W/m2
            !
         CASE DEFAULT
            WRITE(ctmp1,*) '     bad flag value for nn_geoflx = ', nn_geoflx
            CALL ctl_stop( ctmp1 )
         END SELECT
         !
      ELSE
         IF(lwp) WRITE(numout,*) '   ==>>>   no geothermal heat flux'
      ENDIF
      !
   END SUBROUTINE tra_bbc_init

   !!======================================================================
END MODULE trabbc
