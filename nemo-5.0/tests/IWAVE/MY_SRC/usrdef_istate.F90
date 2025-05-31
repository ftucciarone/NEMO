MODULE usrdef_istate
   !!==============================================================================
   !!                       ***  MODULE usrdef_istate  ***
   !!
   !!                         === IWAVE configuration  ===
   !!
   !! User defined : set the initial state of a user configuration
   !!==============================================================================
   !! History :  NEMO ! 2024-05  (J. Chanut) Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  usr_def_istate : initial state in Temperature and salinity
   !!----------------------------------------------------------------------
   USE par_oce              ! ocean space and time domain
   USE phycst               ! physical constants
   USE dom_oce, ONLY: glamt, e1t, r1_e1t ! longitude in km
   USE eosbn2, ONLY : rn_a0 ! thermal expansion
   USE usrdef_nam           ! User defined : namelist variables
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_istate       ! called by istate.F90
   PUBLIC   usr_def_istate_ssh   ! called by domqco.F90

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_istate.F90 14053 2020-12-03 13:48:38Z techene $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS
  
   SUBROUTINE usr_def_istate( pdept, ptmask, pts, pu, pv )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers
      !!                Here IWAVE configuration 
      !!
      !! ** Method  : - set temprature field
      !!              - set salinity   field
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   pdept   ! depth of t-point               [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask             [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(  out) ::   pts     ! T & S fields      [Celsius ; g/kg]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pu      ! i-component of the velocity  [m/s] 
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pv      ! j-component of the velocity  [m/s] 
      !
      INTEGER :: jk
      REAL(wp)  :: zn2, zdinc
      REAL(wp), DIMENSION(jpi,jpj) :: zi 
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate : IWAVE configuration, analytical definition of initial state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   Ocean at rest, with a constant salinity (not used as rho=F(T) '
      IF(lwp) WRITE(numout,*) ' '
      !
      !
      pu  (:,:,:) = 0._wp        ! ocean at rest
      pv  (:,:,:) = 0._wp
      !
      !                          ! T & S profiles
      zn2 =  rn_nn**2            ! N2: brunt vaisala squared
      ! 
      zdinc = rn_dinc
! trick to recover exact convergence whatever the vertical discretization is
! (that's a density jaobian issue)
!      zdinc = rn_dinc*REAL(nn_lev,wp)/REAL(nn_lev-1,wp)
      !
      IF     ( nn_test==0 ) THEN
         pts(:,:,:,jp_tem) = 10._wp
      ELSEIF ( nn_test==1 ) THEN
         zi(:,:) = 0.5_wp * rn_H + rn_a * SIN(rpi * glamt(:,:) / rn_L ) 
         DO jk=1, jpkm1
            WHERE (pdept (:,:,jk) < zi(:,:)) 
               pts(:,:,jk,jp_tem) = (10._wp + 0.5_wp*zdinc / rn_a0 ) * ptmask(:,:,jk)
            ELSEWHERE 
               pts(:,:,jk,jp_tem) = (10._wp - 0.5_wp*zdinc / rn_a0 ) * ptmask(:,:,jk)
            ENDWHERE
         END DO
         pts(:,:,jpk,jp_tem) = 0._wp
      ELSEIF ( nn_test==2 ) THEN 
         pts(:,:,:,jp_tem) = (10._wp - zn2 * pdept(:,:,:) * rho0 / grav / rn_a0 ) * ptmask(:,:,:)
      ENDIF
      !
      pts(:,:,:,jp_sal) = 35._wp * ptmask(:,:,:)
      !   
   END SUBROUTINE usr_def_istate


   SUBROUTINE usr_def_istate_ssh( ptmask, pssh )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate_ssh  ***
      !! 
      !! ** Purpose :   Initialization of the ssh
      !!                Here  IWAVE configuration 
      !!
      !! ** Method  :   set ssh to 0
      !!----------------------------------------------------------------------
      !
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask   [m]
      REAL(wp), DIMENSION(jpi,jpj)         , INTENT(  out) ::   pssh    ! sea-surface height   [m]
      !
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate_ssh : IWAVE configuration, analytical definition of initial state.'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~~~~'
      !          
      IF     ( nn_test==2 ) THEN
         ! Free surface seiche but slightly different initialization in Marsaleix case:
         pssh(:,:) = rn_a * glamt(:,:) / ( 0.5 * rn_L ) * ptmask(:,:,1)
!         pssh(:,:) = rn_a * SIN(rpi * glamt(:,:) / rn_L ) * ptmask(:,:,1)
      ELSEIF ( nn_test==1 ) THEN
         ! 2 layer seiche:
         pssh(:,:) = 0.5_wp*rn_dinc*r1_rho0*rn_a * SIN(rpi * glamt(:,:) / rn_L ) * ptmask(:,:,1)  

      ELSEIF ( nn_test==0 ) THEN
         ! External seiche:
         pssh(:,:) = rn_a * glamt(:,:) / ( 0.5 * rn_L ) * ptmask(:,:,1)
!         pssh(:,:) = rn_a * SIN(rpi * glamt(:,:) / rn_L ) * ptmask(:,:,1)
!         pssh(:,:) = 2._wp * rn_a * rn_L / rpi  * 1.e3 *  r1_e1t(:,:) * SIN(rpi * glamt(:,:) / rn_L ) &
!                   &       * SIN(0.5_wp * rpi * e1t(:,:) / 1.e3 / rn_L ) * ptmask(:,:,1)
      ENDIF
      !
   END SUBROUTINE usr_def_istate_ssh

   !!======================================================================
END MODULE usrdef_istate
