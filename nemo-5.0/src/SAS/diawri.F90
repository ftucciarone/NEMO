MODULE diawri
   !!======================================================================
   !!                     ***  MODULE  diawri  ***
   !! Ocean diagnostics :  write ocean output files
   !!=====================================================================
   !! History :  OPA  ! 1991-03  (M.-A. Foujols)  Original code
   !!            4.0  ! 1991-11  (G. Madec)
   !!                 ! 1992-06  (M. Imbard)  correction restart file
   !!                 ! 1992-07  (M. Imbard)  split into diawri and rstwri
   !!                 ! 1993-03  (M. Imbard)  suppress writibm
   !!                 ! 1998-01  (C. Levy)  NETCDF format using ioipsl INTERFACE
   !!                 ! 1999-02  (E. Guilyardi)  name of netCDF files + variables
   !!            8.2  ! 2000-06  (M. Imbard)  Original code (diabort.F)
   !!   NEMO     1.0  ! 2002-06  (A.Bozec, E. Durand)  Original code (diainit.F)
   !!             -   ! 2002-09  (G. Madec)  F90: Free form and module
   !!             -   ! 2002-12  (G. Madec)  merge of diabort and diainit, F90
   !!                 ! 2005-11  (V. Garnier) Surface pressure gradient organization
   !!            3.2  ! 2008-11  (B. Lemaire) creation from old diawri
   !!            3.7  ! 2014-01  (G. Madec) remove eddy induced velocity from no-IOM output
   !!                 !                     change name of output variables in dia_wri_state
   !!            4.0  ! 2020-10  (A. Nasser, S. Techene) add diagnostic for SWE
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dia_wri       : create the standart output files
   !!   dia_wri_state : create an output NetCDF file for a single instantaeous ocean state and forcing fields
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers 
   USE abl            ! abl variables in case ln_abl = .true.
   USE dom_oce         ! ocean space and time domain
   USE zdf_oce         ! ocean vertical physics
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ice fields
   USE sbcssr          ! restoring term toward SST/SSS climatology
   USE phycst          ! physical constants
   USE zdfmxl          ! mixed layer
   USE dianam          ! build name of file (routine)
   USE zdfddm          ! vertical  physics: double diffusion
   USE diahth          ! thermocline diagnostics
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  ! I/O manager
   USE iom
   USE ioipsl
#if defined key_si3
   USE icewri
   USE ice   ,    ONLY: ato_i
#endif
   USE lib_mpp         ! MPP library
   USE timing          ! preformance summary

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dia_wri                 ! routines called by step.F90
   PUBLIC   dia_wri_state
   
   INTEGER ::   nid_T, nz_T, nh_T, nhTi
   INTEGER ::          nb_T
   INTEGER ::   nid_U, nz_U, nh_U, nhUi
   INTEGER ::   nid_V, nz_V, nh_V, nhVi
   INTEGER ::   nid_W, nz_W, nh_W, nhWi
   INTEGER ::   nid_A, nz_A, nh_A, nhAi

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/SAS 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

# if defined key_xios
   !!----------------------------------------------------------------------
   !!   'key_xios'                                        use IOM library
   !!----------------------------------------------------------------------
   
   SUBROUTINE dia_wri( kt, Kmm )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dia_wri  ***
      !!                   
      !! ** Purpose :   Standard output of opa: dynamics and tracer fields 
      !!      NETCDF format is used by default 
      !!      Standalone surface scheme 
      !!
      !! ** Method  :  use iom_put
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kmm     ! ocean time levelindex
      !!----------------------------------------------------------------------
      ! 
      ! Output the initial state and forcings
      IF( ninist == 1 ) THEN
         CALL dia_wri_state( Kmm, 'output.init' )
         ninist = 0
      ENDIF
      !
   END SUBROUTINE dia_wri

#else
   !!----------------------------------------------------------------------
   !!   Default option                                  use IOIPSL  library
   !!----------------------------------------------------------------------
  
   SUBROUTINE dia_wri( kt, Kmm )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dia_wri  ***
      !!                   
      !! ** Purpose :   Standard output of opa: dynamics and tracer fields 
      !!      NETCDF format is used by default 
      !!
      !! ** Method  :   At the beginning of the first time step (nit000), 
      !!      define all the NETCDF files and fields
      !!      At each time step call histdef to compute the mean if ncessary
      !!      Each nn_write time step, output the instantaneous or mean fields
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kmm     ! ocean time level index
      !
      LOGICAL ::   ll_print = .FALSE.                        ! =T print and flush numout
      CHARACTER (len=40) ::   clhstnam, clop, clmx           ! local names
      INTEGER  ::   ji, jj, jk                               ! dummy loop indices
      INTEGER  ::   jn, ierror                               ! local integers
      REAL(wp) ::   zsto, zout, zmax, zjulian                ! local scalars
      REAL(wp), DIMENSION(jpi,jpj) :: z2d     ! 2D workspace
      !!----------------------------------------------------------------------
      !
      IF( ninist == 1 ) THEN     !==  Output the initial state and forcings  ==!
         CALL dia_wri_state( Kmm, 'output.init' )
         ninist = 0
      ENDIF
      !
      IF( nn_write == -1 )   RETURN   ! we will never do any output
      ! 
      IF( ln_timing )   CALL timing_start('dia_wri')
      !
      ! 0. Initialisation
      ! -----------------

      ll_print = .FALSE.                  ! local variable for debugging
      ll_print = ll_print .AND. lwp

      ! Define frequency of output and means
      clop = "x"         ! no use of the mask value
#if defined key_diainstant
      zsto = REAL( nn_write, wp ) * rn_Dt
      clop = "inst("//TRIM(clop)//")"
#else
      zsto=rn_Dt
      clop = "ave("//TRIM(clop)//")"
#endif
      zout = REAL( nn_write, wp ) * rn_Dt
      zmax = REAL( nitend - nit000 + 1, wp ) * rn_Dt

      ! 1. Define NETCDF files and fields at beginning of first time step
      ! -----------------------------------------------------------------

      IF( kt == nit000 ) THEN

         ! Define the NETCDF files (one per grid)

         ! Compute julian date from starting date of the run
         CALL ymds2ju( nyear, nmonth, nday, rn_Dt, zjulian )
         zjulian = zjulian - adatrj   !   set calendar origin to the beginning of the experiment
         IF(lwp)WRITE(numout,*)
         IF(lwp)WRITE(numout,*) 'Date 0 used :', nit000, ' YEAR ', nyear,   &
            &                    ' MONTH ', nmonth, ' DAY ', nday, 'Julian day : ', zjulian
         IF(lwp)WRITE(numout,*) ' indexes of zoom = ', Nis0, Nie0, Njs0, Nje0,   &
                                 ' limit storage in depth = ', jpk

         ! Define the T grid FILE ( nid_T )

         CALL dia_nam( clhstnam, nn_write, 'grid_T' )
         IF(lwp) WRITE(numout,*) " Name of NETCDF file ", clhstnam    ! filename
         CALL histbeg( clhstnam, jpi, glamt, jpj, gphit,           &  ! Horizontal grid: glamt and gphit
            &          Nis0, Ni_0, Njs0, Nj_0,       &
            &          nit000-1, zjulian, rn_Dt, nh_T, nid_T, domain_id=nidom, snc4chunks=snc4set )
         CALL histvert( nid_T, "deptht", "Vertical T levels",      &  ! Vertical grid: gdept
            &           "m", jpk, gdept_1d, nz_T, "down" )

         ! Define the U grid FILE ( nid_U )

         CALL dia_nam( clhstnam, nn_write, 'grid_U' )
         IF(lwp) WRITE(numout,*) " Name of NETCDF file ", clhstnam    ! filename
         CALL histbeg( clhstnam, jpi, glamu, jpj, gphiu,           &  ! Horizontal grid: glamu and gphiu
            &          Nis0, Ni_0, Njs0, Nj_0,       &
            &          nit000-1, zjulian, rn_Dt, nh_U, nid_U, domain_id=nidom, snc4chunks=snc4set )
         CALL histvert( nid_U, "depthu", "Vertical U levels",      &  ! Vertical grid: gdept
            &           "m", jpk, gdept_1d, nz_U, "down" )

         ! Define the V grid FILE ( nid_V )

         CALL dia_nam( clhstnam, nn_write, 'grid_V' )                   ! filename
         IF(lwp) WRITE(numout,*) " Name of NETCDF file ", clhstnam
         CALL histbeg( clhstnam, jpi, glamv, jpj, gphiv,           &  ! Horizontal grid: glamv and gphiv
            &          Nis0, Ni_0, Njs0, Nj_0,       &
            &          nit000-1, zjulian, rn_Dt, nh_V, nid_V, domain_id=nidom, snc4chunks=snc4set )
         CALL histvert( nid_V, "depthv", "Vertical V levels",      &  ! Vertical grid : gdept
            &          "m", jpk, gdept_1d, nz_V, "down" )

         ! Define the W grid FILE ( nid_W )

         ! No W grid FILE
         IF( ln_abl ) THEN 
         ! Define the ABL grid FILE ( nid_A )
            CALL dia_nam( clhstnam, nn_write, 'grid_ABL' )
            IF(lwp) WRITE(numout,*) " Name of NETCDF file ", clhstnam    ! filename
            CALL histbeg( clhstnam, Ni_0, glamt(A2D(0)), Nj_0, gphit(A2D(0)),   &  ! Horizontal grid: glamt and gphit
               &          1, Ni_0, 1, Nj_0,       &
               &          nit000-1, zjulian, rn_Dt, nh_A, nid_A, domain_id=nidom, snc4chunks=snc4set )
            CALL histvert( nid_A, "ght_abl", "Vertical T levels",      &  ! Vertical grid: gdept
               &           "m", jpkam1, ght_abl(2:jpka), nz_A, "up" )
            !                                                            ! Index of ocean points
         ENDIF

         ! Declare all the output fields as NETCDF variables

         !                                                                                      !!! nid_T : 3D
         CALL histdef( nid_T, "sst_m", "Sea Surface temperature"            , "C"      ,   &  ! sst
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histdef( nid_T, "sss_m", "Sea Surface Salinity"               , "PSU"    ,   &  ! sss
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histdef( nid_T, "sowaflup", "Net Upward Water Flux"              , "Kg/m2/s",   &  ! emp, no runoff in SAS
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histdef( nid_T, "sosfldow", "downward salt flux"                 , "PSU/m2/s",  &  ! sfx
             &         jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histdef( nid_T, "sohefldo", "Net Downward Heat Flux"             , "W/m2"   ,   &  ! qns + qsr
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histdef( nid_T, "soshfldo", "Shortwave Radiation"                , "W/m2"   ,   &  ! qsr
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histdef( nid_T, "soicecov", "Ice fraction"                       , "[0,1]"  ,   &  ! fr_i
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histdef( nid_T, "sowindsp", "wind speed at 10m"                  , "m/s"    ,   &  ! wndm
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
!
         IF( ln_abl ) THEN
            CALL histdef( nid_A, "t_abl", "Potential Temperature"     , "K"        ,       &  ! t_abl
               &          jpi, jpj, nh_A, jpkam1, 1, jpkam1, nz_A, 32, clop, zsto, zout )
            CALL histdef( nid_A, "q_abl", "Humidity"                  , "kg/kg"    ,       &  ! q_abl
               &          jpi, jpj, nh_A, jpkam1, 1, jpkam1, nz_A, 32, clop, zsto, zout ) 
            CALL histdef( nid_A, "u_abl", "Atmospheric U-wind   "     , "m/s"        ,     &  ! u_abl
               &          jpi, jpj, nh_A, jpkam1, 1, jpkam1, nz_A, 32, clop, zsto, zout )
            CALL histdef( nid_A, "v_abl", "Atmospheric V-wind   "     , "m/s"    ,         &  ! v_abl
               &          jpi, jpj, nh_A, jpkam1, 1, jpkam1, nz_A, 32, clop, zsto, zout ) 
            CALL histdef( nid_A, "tke_abl", "Atmospheric TKE   "     , "m2/s2"    ,        &  ! tke_abl
               &          jpi, jpj, nh_A, jpkam1, 1, jpkam1, nz_A, 32, clop, zsto, zout ) 
            CALL histdef( nid_A, "avm_abl", "Atmospheric turbulent viscosity", "m2/s"   ,  &  ! avm_abl
               &          jpi, jpj, nh_A, jpkam1, 1, jpkam1, nz_A, 32, clop, zsto, zout ) 
            CALL histdef( nid_A, "avt_abl", "Atmospheric turbulent diffusivity", "m2/s2",  &  ! avt_abl
               &          jpi, jpj, nh_A, jpkam1, 1, jpkam1, nz_A, 32, clop, zsto, zout ) 
            CALL histdef( nid_A, "pblh", "Atmospheric boundary layer height "  , "m",      &  ! pblh
               &          jpi, jpj, nh_A,  1  , 1, 1   , -99 , 32, clop, zsto, zout )		 			   
#if defined key_si3
            CALL histdef( nid_A, "oce_frac", "Fraction of open ocean"  , " ",      &  ! ato_i
               &          jpi, jpj, nh_A,  1  , 1, 1   , -99 , 32, clop, zsto, zout )
#endif
            CALL histend( nid_A, snc4chunks=snc4set )
            !
         ENDIF
!
         CALL histdef( nid_T, "sozotaux", "Wind Stress along i-axis"           , "N/m2"   ,   &  ! utau
            &          jpi, jpj, nh_T, 1  , 1, 1  , - 99, 32, clop, zsto, zout )
         CALL histdef( nid_T, "sometauy", "Wind Stress along j-axis"           , "N/m2"   ,   &  ! vtau
            &          jpi, jpj, nh_T, 1  , 1, 1  , - 99, 32, clop, zsto, zout )

         CALL histend( nid_T, snc4chunks=snc4set )

         !                                                                                      !!! nid_U : 3D
         CALL histdef( nid_U, "ssu_m", "Velocity component in x-direction", "m/s"   ,         &  ! ssu
            &          jpi, jpj, nh_U, 1  , 1, 1  , -99, 32, clop, zsto, zout )

         CALL histend( nid_U, snc4chunks=snc4set )

         !                                                                                      !!! nid_V : 3D
         CALL histdef( nid_V, "ssv_m", "Velocity component in y-direction", "m/s",            &  ! ssv_m
            &          jpi, jpj, nh_V, 1  , 1, 1  , -99, 32, clop, zsto, zout )

         CALL histend( nid_V, snc4chunks=snc4set )

         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'End of NetCDF Initialization'
         IF(ll_print) CALL FLUSH(numout )

      ENDIF

      ! 2. Start writing data
      ! ---------------------

      ! ndex(1) est utilise ssi l'avant dernier argument est different de 
      ! la taille du tableau en sortie. Dans ce cas , l'avant dernier argument
      ! donne le nombre d'elements, et ndex la liste des indices a sortir

      IF( lwp .AND. MOD( kt - nit000 + 1, nn_write ) == 0 ) THEN 
         WRITE(numout,*) 'dia_wri : write model outputs in NetCDF files at ', kt, 'time-step'
         WRITE(numout,*) '~~~~~~ '
      ENDIF

      ! Write fields on T grid
      CALL histwrite( nid_T, "sst_m"   , kt, sst_m         , 1, (/-1/) )   ! sea surface temperature
      CALL histwrite( nid_T, "sss_m"   , kt, sss_m         , 1, (/-1/) )   ! sea surface salinity
      CALL histwrite( nid_T, "sowaflup", kt, emp           , 1, (/-1/) )   ! upward water flux, no runoff in SAS
      z2d(A2D(0)) = sfx(A2D(0))   ! sfx is an inner domain data
      CALL histwrite( nid_T, "sosfldow", kt, z2d           , 1, (/-1/) )   ! downward salt flux 
                                                                                  ! (includes virtual salt flux beneath ice 
                                                                                  ! in linear free surface case)

      z2d(A2D(0)) = qsr(A2D(0)) + qns(A2D(0))
      CALL histwrite( nid_T, "sohefldo", kt, z2d           , 1, (/-1/) )   ! total heat flux
      z2d(A2D(0)) = qsr(A2D(0))   ! qsr is an inner domain data
      CALL histwrite( nid_T, "soshfldo", kt, z2d           , 1, (/-1/) )   ! solar heat flux
      CALL histwrite( nid_T, "soicecov", kt, fr_i          , 1, (/-1/) )   ! ice fraction   
      z2d(A2D(0)) = wndm(A2D(0))   ! wndm is an inner domain data
      CALL histwrite( nid_T, "sowindsp", kt, z2d           , 1, (/-1/) )   ! wind speed   
      CALL histwrite( nid_T, "sozotaux", kt, utau          , 1, (/-1/) )   ! i-wind stress
      CALL histwrite( nid_T, "sometauy", kt, vtau          , 1, (/-1/) )   ! j-wind stress
!
      IF( ln_abl ) THEN 
             CALL histwrite( nid_A,  "pblh"   , kt, pblh(A2D(0))               , 1, (/ -1/) )	! pblh 
	     CALL histwrite( nid_A,  "u_abl"  , kt, u_abl   (:,:,2:jpka,nt_n  ), 1, (/ -1/) )	! u_abl
	     CALL histwrite( nid_A,  "v_abl"  , kt, v_abl   (:,:,2:jpka,nt_n  ), 1, (/ -1/) )	! v_abl
	     CALL histwrite( nid_A,  "t_abl"  , kt, tq_abl  (:,:,2:jpka,nt_n,1), 1, (/ -1/) )	! t_abl
	     CALL histwrite( nid_A,  "q_abl"  , kt, tq_abl  (:,:,2:jpka,nt_n,2), 1, (/ -1/) )	! q_abl		 
	     CALL histwrite( nid_A,  "tke_abl", kt, tke_abl (:,:,2:jpka,nt_n  ), 1, (/ -1/) )	! tke_abl
	     CALL histwrite( nid_A,  "avm_abl", kt, avm_abl (:,:,2:jpka	      ), 1, (/ -1/) )	! avm_abl
	     CALL histwrite( nid_A,  "avt_abl", kt, avt_abl (:,:,2:jpka	      ), 1, (/ -1/) )	! avt_abl	
#if defined key_si3
         CALL histwrite( nid_A,  "oce_frac"   , kt, ato_i(A2D(0))              , 1, (/ -1/) )   ! ato_i
#endif
	  ENDIF
!

         ! Write fields on U grid
      CALL histwrite( nid_U, "ssu_m"   , kt, ssu_m         , 1, (/ -1/) )   ! i-current speed

         ! Write fields on V grid
      CALL histwrite( nid_V, "ssv_m"   , kt, ssv_m         , 1, (/ -1/) )   ! j-current speed

      ! 3. Close all files
      ! ---------------------------------------
      IF( kt == nitend ) THEN
         CALL histclo( nid_T )
         CALL histclo( nid_U )
         CALL histclo( nid_V )
         IF(ln_abl) CALL histclo( nid_A )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('dia_wri')
      !
   END SUBROUTINE dia_wri
#endif

   SUBROUTINE dia_wri_state( Kmm, cdfile_name )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE dia_wri_state  ***
      !!        
      !! ** Purpose :   create a NetCDF file named cdfile_name which contains 
      !!      the instantaneous ocean state and forcing fields.
      !!        Used to find errors in the initial state or save the last
      !!      ocean state in case of abnormal end of a simulation
      !!
      !! ** Method  :   NetCDF files using ioipsl
      !!      File 'output.init.nc'  is created if ninist = 1 (namelist)
      !!      File 'output.abort.nc' is created in case of abnormal job end
      !!----------------------------------------------------------------------
      INTEGER           , INTENT( in ) ::   Kmm              ! ocean time levelindex
      CHARACTER (len=* ), INTENT( in ) ::   cdfile_name      ! name of the file created
      !!
      INTEGER :: inum
      !!----------------------------------------------------------------------
      ! 
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'dia_wri_state : single instantaneous ocean state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~   and forcing fields file created '
      IF(lwp) WRITE(numout,*) '                and named :', cdfile_name, '...nc'
      !
      CALL iom_open( TRIM(cdfile_name), inum, ldwrt = .TRUE. )
      !
      CALL iom_rstput( 0, 0, inum, 'votemper', ts (:,:,:,jp_tem,Kmm) )    ! now temperature
      CALL iom_rstput( 0, 0, inum, 'vosaline', ts (:,:,:,jp_sal,Kmm) )    ! now salinity
      CALL iom_rstput( 0, 0, inum, 'sossheig', ssh(:,:,         Kmm) )    ! sea surface height
      CALL iom_rstput( 0, 0, inum, 'vozocrtx', uu (:,:,:,       Kmm) )    ! now i-velocity
      CALL iom_rstput( 0, 0, inum, 'vomecrty', vv (:,:,:,       Kmm) )    ! now j-velocity
      CALL iom_rstput( 0, 0, inum, 'vovecrtz', ww                    )    ! now k-velocity
      CALL iom_rstput( 0, 0, inum, 'sowaflup', emp                   )    ! freshwater budget, no runoff in SAS
      CALL iom_rstput( 0, 0, inum, 'sohefldo', qsr + qns             )    ! total heat flux
      CALL iom_rstput( 0, 0, inum, 'soshfldo', qsr                   )    ! solar heat flux
      CALL iom_rstput( 0, 0, inum, 'soicecov', fr_i                  )    ! ice fraction
      CALL iom_rstput( 0, 0, inum, 'sozotaux', utau                  )    ! i-wind stress
      CALL iom_rstput( 0, 0, inum, 'sometauy', vtau                  )    ! j-wind stress
      !
      CALL iom_close( inum )
      !
#if defined key_si3
      IF( nn_ice == 2 ) THEN   ! condition needed in case agrif + ice-model but no-ice in child grid
         CALL iom_open( TRIM(cdfile_name)//'_ice', inum, ldwrt = .TRUE., kdlev = jpl, cdcomp = 'ICE' )
         CALL ice_wri_state( inum )
         CALL iom_close( inum )
      ENDIF
      !
#endif
   END SUBROUTINE dia_wri_state

   !!======================================================================
END MODULE diawri
