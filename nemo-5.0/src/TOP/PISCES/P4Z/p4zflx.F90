MODULE p4zflx
   !!======================================================================
   !!                         ***  MODULE p4zflx  ***
   !! TOP :   PISCES CALCULATES GAS EXCHANGE AND CHEMISTRY AT SEA SURFACE
   !!======================================================================
   !! History :   -   !  1988-07  (E. MAIER-REIMER) Original code
   !!             -   !  1998     (O. Aumont) additions
   !!             -   !  1999     (C. Le Quere) modifications
   !!            1.0  !  2004     (O. Aumont) modifications
   !!            2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!                 !  2011-02  (J. Simeon, J. Orr) Include total atm P correction 
   !!----------------------------------------------------------------------
   !!   p4z_flx       :   CALCULATES GAS EXCHANGE AND CHEMISTRY AT SEA SURFACE
   !!   p4z_flx_init  :   Read the namelist
   !!   p4z_patm      :   Read sfc atm pressure [atm] for each grid cell
   !!----------------------------------------------------------------------
   USE oce_trc        !  shared variables between ocean and passive tracers 
   USE trc            !  passive tracers common variables
   USE sms_pisces     !  PISCES Source Minus Sink variables
   USE p4zche         !  Chemical model
   USE prtctl         !  print control for debugging
   USE iom            !  I/O manager
   USE fldread        !  read input fields
   USE lib_fortran    ! Fortran routines library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_flx  
   PUBLIC   p4z_flx_init  
   PUBLIC   p4z_flx_alloc  

   !                                 !!** Namelist  nampisext  **
   REAL(wp)          ::   atcco2      !: pre-industrial atmospheric [co2] (ppm) 	
   LOGICAL           ::   ln_co2int   !: flag to read in a file and interpolate atmospheric pco2 or not
   CHARACTER(len=34) ::   clname      !: filename of pco2 values
   INTEGER           ::   nn_offset   !: Offset model-data start year (default = 0) 

   !!  Variables related to reading atmospheric CO2 time history    
   INTEGER                                   ::   nmaxrec, numco2   !
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:) ::   atcco2h, years    !

   !                                  !!* nampisatm namelist (Atmospheric PRessure) *
   LOGICAL, PUBLIC ::   ln_presatm     !: ref. pressure: global mean Patm (F) or a constant (F)
   LOGICAL, PUBLIC ::   ln_presatmco2  !: accounting for spatial atm CO2 in the compuation of carbon flux (T) or not (F)

   REAL(wp) , ALLOCATABLE, SAVE, DIMENSION(:,:) ::   patm      ! atmospheric pressure at kt                 [N/m2]
   TYPE(FLD), ALLOCATABLE,       DIMENSION(:)   ::   sf_patm   ! structure of input fields (file informations, fields read)
   TYPE(FLD), ALLOCATABLE,       DIMENSION(:)   ::   sf_atmco2 ! structure of input fields (file informations, fields read)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::  satmco2   !: atmospheric pco2 

   REAL(wp) ::   xconv  = 0.01_wp / 3600._wp   !: coefficients for conversion 

   LOGICAL  :: l_dia_cflx, l_dia_tcflx
   LOGICAL  :: l_dia_oflx, l_dia_kg

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_flx ( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_flx  ***
      !!
      !! ** Purpose :   CALCULATES GAS EXCHANGE AND CHEMISTRY AT SEA SURFACE
      !!
      !! ** Method  : 
      !!              - Include total atm P correction via Esbensen & Kushnir (1981) 
      !!              - Remove Wanninkhof chemical enhancement;
      !!              - Add option for time-interpolation of atcco2.txt  
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   !
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs      ! time level indices
      !
      INTEGER  ::   ji, jj, jm, iind, iindm1, itt
      REAL(wp) ::   ztc, ztc2, ztc3, ztc4, zws, zkgwan
      REAL(wp) ::   zfld, zflu, zfld16, zflu16, zdens
      REAL(wp) ::   zvapsw, zsal, zfco2, zxc, zxc2, xCO2approx, ztkel, zfugcoeff
      REAL(wp) ::   zph, zph2, zdic, zsch_o2, zsch_co2
      REAL(wp) ::   zyr_dec, zdco2dt
      CHARACTER (len=25) ::   charout
      REAL(wp), DIMENSION(A2D(0)) ::   zkgco2, zkgo2, zh2co3, zoflx,  zpco2atm, zpco2oce  
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_flx')
      !
      IF( kt == nittrc000 )  THEN
         l_dia_cflx  = iom_use( "Cflx"    ) .OR. iom_use( "Dpco2" )  &
            &     .OR. iom_use( "pCO2sea" ) .OR. iom_use( "AtmCo2" )
         l_dia_oflx  = iom_use( "Oflx"    ) .OR. iom_use( "Dpo2" )  
         l_dia_tcflx = iom_use( "tcflx"   ) .OR. iom_use( "tcflxcum" )
         l_dia_kg    = iom_use( "Kg"   ) 
      ENDIF
      
      ! SURFACE CHEMISTRY (PCO2 AND [H+] IN
      !     SURFACE LAYER); THE RESULT OF THIS CALCULATION
      !     IS USED TO COMPUTE AIR-SEA FLUX OF CO2

      IF( kt /= nit000 .AND. .NOT.l_co2cpl .AND. knt == 1 )   CALL p4z_patm( kt )   ! Get sea-level pressure (E&K [1981] climatology) for use in flux calcs

      IF( ln_co2int .AND. .NOT.ln_presatmco2 .AND. .NOT.l_co2cpl ) THEN 
         ! Linear temporal interpolation  of atmospheric pco2.  atcco2.txt has annual values.
         ! Caveats: First column of .txt must be in years, decimal  years preferably. 
         ! For nn_offset, if your model year is iyy, nn_offset=(years(1)-iyy) 
         ! then the first atmospheric CO2 record read is at years(1)
         zyr_dec = REAL( nyear + nn_offset, wp ) + REAL( nday_year, wp ) / REAL( nyear_len(1), wp )
         jm = 1
         DO WHILE( jm <= nmaxrec .AND. years(jm) < zyr_dec ) ;  jm = jm + 1 ;  END DO
         iind = jm  ;   iindm1 = jm - 1
         zdco2dt = ( atcco2h(iind) - atcco2h(iindm1) ) / ( years(iind) - years(iindm1) + rtrn )
         atcco2  = zdco2dt * ( zyr_dec - years(iindm1) ) + atcco2h(iindm1)
         satmco2(:,:) = atcco2 
      ENDIF

      IF( l_co2cpl ) THEN
         DO_2D( 0, 0, 0, 0 )
            satmco2(ji,jj) = atm_co2(ji,jj)
         END_2D
      ENDIF

      DO_2D( 0, 0, 0, 0 )
         ! DUMMY VARIABLES FOR DIC, H+, AND BORATE
         zdens = rhop(ji,jj,1) / 1000. 
         zdic  = tr(ji,jj,1,jpdic,Kbb)
         zph   = MAX( hi(ji,jj,1), 1.e-10 ) / ( zdens + rtrn )
         zph2  = zph * zph 
         ! CALCULATE [H2CO3]
         zh2co3(ji,jj) = zdic/(1. + ak13(ji,jj,1)/zph + ak13(ji,jj,1)*ak23(ji,jj,1)/zph2)
      END_2D

      ! --------------
      ! COMPUTE FLUXES
      ! --------------

      ! FIRST COMPUTE GAS EXCHANGE COEFFICIENTS
      ! -------------------------------------------
      !
#if defined key_RK3
      ! Don't consider mid-step values if online coupling
      ! because these are possibly non-monotonic (even with FCT): 
      IF ( l_offline ) THEN ; itt = Kmm ; ELSE ; itt = Kbb ; ENDIF 
#else 
      itt = Kmm
#endif

      DO_2D( 0, 0, 0, 0 )
         ztc  = MIN( 35., ts(ji,jj,1,jp_tem,itt) )
         ztc2 = ztc * ztc
         ztc3 = ztc * ztc2 
         ztc4 = ztc2 * ztc2 
         ! Compute the schmidt Number both O2 and CO2
         zsch_co2 = 2116.8 - 136.25 * ztc + 4.7353 * ztc2 - 0.092307 * ztc3 + 0.0007555 * ztc4
         zsch_o2  = 1920.4 - 135.6  * ztc + 5.2122 * ztc2 - 0.109390 * ztc3 + 0.0009377 * ztc4
         !  wind speed 
         zws  = wndm(ji,jj) * wndm(ji,jj)
         ! Compute the piston velocity for O2 and CO2
         zkgwan = 0.251 * zws
         zkgwan = zkgwan * xconv * ( 1.- fr_i(ji,jj) ) * tmask(ji,jj,1)
         ! compute gas exchange for CO2 and O2
         zkgco2(ji,jj) = zkgwan * SQRT( 660./ zsch_co2 )
         zkgo2 (ji,jj) = zkgwan * SQRT( 660./ zsch_o2 )
      END_2D


      DO_2D( 0, 0, 0, 0 )
         ztkel = tempis(ji,jj,1) + 273.15
         zsal  = salinprac(ji,jj,1) + ( 1.- tmask(ji,jj,1) ) * 35.
         zvapsw    = EXP(24.4543 - 67.4509*(100.0/ztkel) - 4.8489*LOG(ztkel/100) - 0.000544*zsal)
         zpco2atm(ji,jj) = satmco2(ji,jj) * ( patm(ji,jj) - zvapsw )
         zxc       = ( 1.0 - zpco2atm(ji,jj) * 1E-6 )
         zxc2      = zxc * zxc
         zfugcoeff = EXP( patm(ji,jj) * (chemc(ji,jj,2) + 2.0 * zxc2 * chemc(ji,jj,3) )   &
         &           / ( 82.05736 * ztkel ))
         zfco2 = zpco2atm(ji,jj) * zfugcoeff

         ! Compute CO2 flux for the sea and air
         zfld = zfco2 * chemc(ji,jj,1) * zkgco2(ji,jj)  ! (mol/L) * (m/s)
         zflu = zh2co3(ji,jj) * zkgco2(ji,jj)                                   ! (mol/L) (m/s) ?
         zpco2oce(ji,jj) = zh2co3(ji,jj) / ( chemc(ji,jj,1) * zfugcoeff + rtrn )
         oce_co2(ji,jj)  = ( zfld - zflu ) * tmask(ji,jj,1) 
         ! compute the trend
         tr(ji,jj,1,jpdic,Krhs) = tr(ji,jj,1,jpdic,Krhs) + oce_co2(ji,jj) * rfact2 / e3t(ji,jj,1,Kmm)

         ! Compute O2 flux 
         zfld16 = patm(ji,jj) * chemo2(ji,jj,1) * zkgo2(ji,jj)          ! (mol/L) * (m/s)
         zflu16 = tr(ji,jj,1,jpoxy,Kbb) * zkgo2(ji,jj)
         zoflx(ji,jj) = ( zfld16 - zflu16 ) * tmask(ji,jj,1)
         tr(ji,jj,1,jpoxy,Krhs) = tr(ji,jj,1,jpoxy,Krhs) + zoflx(ji,jj) * rfact2 / e3t(ji,jj,1,Kmm)
      END_2D

      IF( l_dia_tcflx .OR. kt == nitrst )  THEN
         t_oce_co2_flx  = glob_2Dsum( 'p4zflx',  oce_co2(:,:) * e1e2t(A2D(0)) * 1000._wp, cdelay = 'co2flx' )   !  Total Flux of Carbon
         t_oce_co2_flx_cum = t_oce_co2_flx_cum + t_oce_co2_flx       !  Cumulative Total Flux of Carbon
!        t_atm_co2_flx     = glob_2Dsum( 'p4zflx', satmco2(:,:) * e1e2t(:,:) )       ! Total atmospheric pCO2
         t_atm_co2_flx     =  atcco2      ! Total atmospheric pCO2
      ENDIF
 
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('flx ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF

      IF( knt == nrdttrc ) THEN
        !
        IF( l_dia_cflx ) THEN
           CALL iom_put( "AtmCo2" , satmco2(:,:) * tmask(A2D(0),1) )   ! Atmospheric CO2 concentration
           CALL iom_put( "Cflx"   , oce_co2(:,:) * 1000._wp )         ! Carbon flux
           CALL iom_put( "Dpco2"  ,  ( zpco2atm(:,:) - zpco2oce(:,:) ) * tmask(A2D(0),1) ) ! atmospheric Dpco2
           CALL iom_put( "pCO2sea", zpco2oce(:,:) * tmask(A2D(0),1) ) ! oceanic Dpco2
        ENDIF
        !
        IF( l_dia_oflx ) THEN
           CALL iom_put( "Oflx", zoflx * 1000._wp )     ! oxygen flux
           CALL iom_put( "Dpo2", ( atcox * patm(:,:) - atcox * tr(A2D(0),1,jpoxy,Kbb) &  !  Dpo2
                 &              / ( chemo2(:,:,1) + rtrn ) ) * tmask(A2D(0),1) )
        ENDIF
        !
        IF( l_dia_kg )   CALL iom_put( "Kg", zkgco2(:,:) * tmask(A2D(0),1) )
        IF( l_dia_tcflx ) THEN
          CALL iom_put( "tcflx"   , t_oce_co2_flx )    ! global flux of carbon
          CALL iom_put( "tcflxcum", t_oce_co2_flx_cum )   !  Cumulative flux of carbon
        ENDIF
        !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_flx')
      !
   END SUBROUTINE p4z_flx


   SUBROUTINE p4z_flx_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_flx_init  ***
      !!
      !! ** Purpose :   Initialization of atmospheric conditions
      !!
      !! ** Method  :   Read the nampisext namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampisext
      !!----------------------------------------------------------------------
      INTEGER ::   jm, ios   ! Local integer 
      !!
      NAMELIST/nampisext/ln_co2int, atcco2, clname, nn_offset
      !!----------------------------------------------------------------------
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) ' p4z_flx_init : atmospheric conditions for air-sea flux calculation'
         WRITE(numout,*) ' ~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,nampisext)
      READ_NML_CFG(numnatp,nampisext)
      IF(lwm) WRITE ( numonp, nampisext )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampisext --- parameters for air-sea exchange'
         WRITE(numout,*) '      reading in the atm pCO2 file or constant value   ln_co2int =', ln_co2int
      ENDIF
      !
      CALL p4z_patm( nit000 )
      !
      IF( .NOT.ln_co2int .AND. .NOT.ln_presatmco2 ) THEN
         IF(lwp) THEN                         ! control print
            WRITE(numout,*) '         Constant Atmospheric pCO2 value               atcco2    =', atcco2
         ENDIF
         satmco2(:,:)  = atcco2      ! Initialisation of atmospheric pco2
      ELSEIF( ln_co2int .AND. .NOT.ln_presatmco2 ) THEN
         IF(lwp)  THEN
            WRITE(numout,*) '         Constant Atmospheric pCO2 value               atcco2    =', atcco2
            WRITE(numout,*) '         Atmospheric pCO2 value  from file             clname    =', TRIM( clname )
            WRITE(numout,*) '         Offset model-data start year                  nn_offset =', nn_offset
         ENDIF
         CALL ctl_opn( numco2, TRIM( clname) , 'OLD', 'FORMATTED', 'SEQUENTIAL', -1 , numout, lwp )
         jm = 0                      ! Count the number of record in co2 file
         DO
           READ(numco2,*,END=100) 
           jm = jm + 1
         END DO
 100     nmaxrec = jm - 1 
         ALLOCATE( years  (nmaxrec) )   ;   years  (:) = 0._wp
         ALLOCATE( atcco2h(nmaxrec) )   ;   atcco2h(:) = 0._wp
         !
         REWIND(numco2)
         DO jm = 1, nmaxrec          ! get  xCO2 data
            READ(numco2, *)  years(jm), atcco2h(jm)
            IF(lwp) WRITE(numout, '(f6.0,f7.2)')  years(jm), atcco2h(jm)
         END DO
         CLOSE(numco2)
      ELSEIF( .NOT.ln_co2int .AND. ln_presatmco2 ) THEN
         IF(lwp) WRITE(numout,*) '    Spatialized Atmospheric pCO2 from an external file'
      ELSE
         IF(lwp) WRITE(numout,*) '    Spatialized Atmospheric pCO2 from an external file'
      ENDIF
      !
!      oce_co2(:,:)  = 0._wp                ! Initialization of Flux of Carbon
      t_oce_co2_flx = 0._wp
      t_atm_co2_flx = 0._wp
      !
   END SUBROUTINE p4z_flx_init


   SUBROUTINE p4z_patm( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_atm  ***
      !!
      !! ** Purpose :   Read and interpolate the external atmospheric sea-level pressure
      !! ** Method  :   Read the files and interpolate the appropriate variables
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !
      INTEGER            ::   ierr, ios   ! Local integer
      CHARACTER(len=100) ::   cn_dir      ! Root directory for location of ssr files
      TYPE(FLD_N)        ::   sn_patm     ! informations about the fields to be read
      TYPE(FLD_N)        ::   sn_atmco2   ! informations about the fields to be read
      INTEGER  :: ji, jj
      !!
      NAMELIST/nampisatm/ ln_presatm, ln_presatmco2, sn_patm, sn_atmco2, cn_dir
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN    !==  First call kt=nittrc000  ==!
         !
         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) ' p4z_patm : sea-level atmospheric pressure'
            WRITE(numout,*) ' ~~~~~~~~'
         ENDIF
         !
         READ_NML_REF(numnatp,nampisatm)
         READ_NML_CFG(numnatp,nampisatm)
         IF(lwm) WRITE ( numonp, nampisatm )
         !
         !
         IF(lwp) THEN                                 !* control print
            WRITE(numout,*) '   Namelist : nampisatm --- Atmospheric Pressure as external forcing'
            WRITE(numout,*) '      constant atmopsheric pressure (F) or from a file (T)  ln_presatm    = ', ln_presatm
            WRITE(numout,*) '      spatial atmopsheric CO2 for flux calcs                ln_presatmco2 = ', ln_presatmco2
         ENDIF
         !
         IF( ln_presatm ) THEN
            ALLOCATE( sf_patm(1), STAT=ierr )           !* allocate and fill sf_patm (forcing structure) with sn_patm
            IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'p4z_flx: unable to allocate sf_patm structure' )
            !
            CALL fld_fill( sf_patm, (/ sn_patm /), cn_dir, 'p4z_flx', 'Atmospheric pressure ', 'nampisatm' )
                                   ALLOCATE( sf_patm(1)%fnow(jpi,jpj,1)   )
            IF( sn_patm%ln_tint )  ALLOCATE( sf_patm(1)%fdta(jpi,jpj,1,2) )
         ENDIF
         !                                         
         IF( ln_presatmco2 ) THEN
            ALLOCATE( sf_atmco2(1), STAT=ierr )           !* allocate and fill sf_atmco2 (forcing structure) with sn_atmco2
            IF( ierr > 0 )   CALL ctl_stop( 'STOP', 'p4z_flx: unable to allocate sf_atmco2 structure' )
            !
            CALL fld_fill( sf_atmco2, (/ sn_atmco2 /), cn_dir, 'p4z_flx', 'Atmospheric co2 partial pressure ', 'nampisatm' )
                                   ALLOCATE( sf_atmco2(1)%fnow(jpi,jpj,1)   )
            IF( sn_atmco2%ln_tint )  ALLOCATE( sf_atmco2(1)%fdta(jpi,jpj,1,2) )
         ENDIF
         !
         IF( .NOT.ln_presatm )   patm(:,:) = 1._wp    ! Initialize patm if no reading from a file
         !
      ENDIF
      !
      IF( ln_presatm ) THEN
         CALL fld_read( kt, 1, sf_patm )               !* input Patm provided at kt + 1/2
         DO_2D( 0, 0, 0, 0 )
            patm(ji,jj) = sf_patm(1)%fnow(ji,jj,1)/101325.0     ! atmospheric pressure
         END_2D
      ENDIF
      !
      IF( ln_presatmco2 ) THEN
         CALL fld_read( kt, 1, sf_atmco2 )               !* input atmco2 provided at kt + 1/2
         DO_2D( 0, 0, 0, 0 )
            satmco2(ji,jj) = sf_atmco2(1)%fnow(ji,jj,1)                        ! atmospheric pressure
         END_2D
      ELSE
         satmco2(:,:) = atcco2    ! Initialize atmco2 if no reading from a file
      ENDIF
      !
   END SUBROUTINE p4z_patm


   INTEGER FUNCTION p4z_flx_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_flx_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( satmco2(A2D(0)), patm(A2D(0)), STAT=p4z_flx_alloc )
      !
      IF( p4z_flx_alloc /= 0 )   CALL ctl_stop( 'STOP', 'p4z_flx_alloc : failed to allocate arrays' )
      !
   END FUNCTION p4z_flx_alloc

   !!======================================================================
END MODULE p4zflx
