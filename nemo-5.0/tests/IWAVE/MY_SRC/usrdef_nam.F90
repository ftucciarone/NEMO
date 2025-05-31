MODULE usrdef_nam
   !!======================================================================
   !!                       ***  MODULE  usrdef_nam  ***
   !!
   !!                       === IWAVE configuration  ===
   !!
   !! User defined : set the domain characteristics of a user configuration
   !!======================================================================
   !! History :  NEMO ! 2024-05  (J. Chanut)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_nam   : read user defined namelist and set global domain size
   !!   usr_def_hgr   : initialize the horizontal mesh 
   !!----------------------------------------------------------------------
   USE dom_oce        ! flag of type of coordinate
   USE par_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE timing         ! Timing
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_nam   ! called by nemogcm.F90

   !                              !!* namusr_def namelist *!!
   INTEGER,  PUBLIC ::   nn_test   ! test type=0, 1, 2 
   INTEGER,  PUBLIC ::   nn_COORD  ! vertical coordinate type 
   LOGICAL,  PUBLIC ::   ln_bump   ! define bump or not (T if nn_test=2)
   REAL(wp), PUBLIC ::   rn_dx     ! resolution in meters defining the horizontal domain size
   REAL(wp), PUBLIC ::   rn_dz     ! resolution in meters defining the vertical domain size
   REAL(wp), PUBLIC ::   rn_L      ! domain length (km) 
   REAL(wp), PUBLIC ::   rn_H      ! domain depth (m) 
   REAL(wp), PUBLIC ::   rn_T      ! seiche period (hour)
   REAL(wp), PUBLIC ::   rn_hbump  ! bump height (m) (used if nn_test=2)
   REAL(wp), PUBLIC ::   rn_nn     ! Brunt vaisala frequency (s-1) nn_test=2
   REAL(wp), PUBLIC ::   rn_dinc   ! Density increment (2 layer case) 
   REAL(wp), PUBLIC ::   rn_a      ! Sea level or interface amplitude (m)
   INTEGER,  PUBlIC ::   nn_pts, nn_lev
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_nam.F90 13286 2020-07-09 15:48:29Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

      SUBROUTINE usr_def_nam( cd_cfg, kk_cfg, kpi, kpj, kpk, ldIperio, ldJperio, ldNFold, cdNFtype )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dom_nam  ***
      !!                    
      !! ** Purpose :   read user defined namelist and define the domain size
      !!
      !! ** Method  :   read in namusr_def containing all the user specific namelist parameter
      !!
      !!                Here IWAVE configuration
      !!
      !! ** input   : - namusr_def namelist found in namelist_cfg
      !!----------------------------------------------------------------------
      CHARACTER(len=*)              , INTENT(out) ::   cd_cfg          ! configuration name
      INTEGER                       , INTENT(out) ::   kk_cfg          ! configuration resolution
      INTEGER                       , INTENT(out) ::   kpi, kpj, kpk   ! global domain sizes 
      LOGICAL         , INTENT(out) ::   ldIperio, ldJperio   ! i- and j-periodicity
      LOGICAL         , INTENT(out) ::   ldNFold              ! North pole folding
      CHARACTER(len=1), INTENT(out) ::   cdNFtype             ! Folding type: T or F
      !
      INTEGER ::   ios, nn   ! Local integer
      REAL(wp) :: zdinc
      !!
      NAMELIST/namusr_def/ nn_test, nn_COORD, rn_nn, rn_dinc, rn_a, rn_L, rn_H, rn_hbump, nn_pts, nn_lev
      !!----------------------------------------------------------------------
      !
      READ  ( numnam_cfg, namusr_def, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namusr_def in configuration namelist' )
      !
      IF(lwm)   WRITE( numond, namusr_def )
      !
      IF ( nn_test < 2) THEN ; ln_bump = .false. ; ELSE; ln_bump = .true. ; ENDIF
      !
      cd_cfg = 'IWAVE'           ! name & resolution (not used)
!! If one wants to set the period and deduce depth, uncomment the two lines
!  below:
!!      rn_T = 12._wp
!!      rn_H = (2._wp*rn_L*1.e3/rn_T/3600._wp)**2/grav
      rn_T = 2._wp*rn_L*1.e3/SQRT(grav*rn_H) / 3600._wp
      rn_dx = rn_L*1.e3 / REAL(nn_pts, wp)
      rn_dz = rn_H / REAL(nn_lev, wp)
      kk_cfg = INT( rn_dx )
!      zdinc = 0._wp
!      DO  nn=1, nn_lev/2
!         zdinc = zdinc + 1._wp/(2._wp**nn) 
!       
!      END DO 
!      rn_dinc = rn_dinc/zdinc
!      nn = nn_lev/2
!      zdinc = 1.-1./2._wp**nn
!      rn_dinc = rn_dinc/zdinc
      !
      ! Global Domain size
      kpi = nn_pts + 2
      kpj = 3
      kpk = nn_lev + 1
      !
      !                             ! control print
      WRITE(numout,*) '   '
      WRITE(numout,*) 'usr_def_nam  : read the user defined namelist (namusr_def) in namelist_cfg'
      WRITE(numout,*) '~~~~~~~~~~~ '
      WRITE(numout,*) '   Namelist namusr_def : IWAVE test cases'
      WRITE(numout,*) '      Test case number                       nn_test  = ', nn_test
      WRITE(numout,*) '      Define bump or not                     ln_bump  = ', ln_bump
      WRITE(numout,*) '      Domain length (km)                       rn_L   = ', rn_L
      WRITE(numout,*) '      Domain depth (m)                         rn_H   = ', rn_H
      WRITE(numout,*) '      Seiche period (h)                        rn_T   = ', rn_T
      WRITE(numout,*) '      bump height (m)                      rn_hbump   = ', rn_hbump
      WRITE(numout,*) '      Brunt Vaisala frequency (s-1)            rn_nn  = ', rn_nn
      WRITE(numout,*) '      2 layer density increment (kg m-3)     rn_dinc  = ', rn_dinc
      WRITE(numout,*) '      Sea level amplitude (m)                  rn_a   = ', rn_a
      SELECT CASE( nn_COORD )
      CASE( 0 )
         WRITE(numout,*) '      type of coordinate  =  l_zco'
      CASE( 1 )
         WRITE(numout,*) '      type of coordinate  =  l_zps'
      CASE( 2 )
         WRITE(numout,*) '      type of coordinate  =  l_sco'
      CASE DEFAULT
         CALL ctl_stop( 'Choose ONE vertical coordinate nn_COORD is 0(zco), 1(zps) or 2(sco)' )
      END SELECT
      WRITE(numout,*) '      horizontal resolution                    rn_dx  = ', rn_dx, ' meters'
      WRITE(numout,*) '      vertical   resolution                    rn_dz  = ', rn_dz, ' meters'
      WRITE(numout,*) '         resulting global domain size :        Ni0glo = ', kpi
      WRITE(numout,*) '                                               Nj0glo = ', kpj
      WRITE(numout,*) '                                               jpkglo = ', kpk
      !
      ldIperio = .FALSE.   ;   ldJperio = .FALSE. 
      ldNFold  = .FALSE.   ;   cdNFtype = '-'
      !
      !
   END SUBROUTINE usr_def_nam

   !!======================================================================
END MODULE usrdef_nam
