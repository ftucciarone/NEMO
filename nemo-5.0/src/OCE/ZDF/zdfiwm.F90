MODULE zdfiwm
   !!========================================================================
   !!                       ***  MODULE  zdfiwm  ***
   !! Ocean physics: Internal gravity wave-driven vertical mixing
   !!========================================================================
   !! History :  1.0  !  2004-04  (L. Bessieres, G. Madec)  Original code
   !!             -   !  2006-08  (A. Koch-Larrouy)  Indonesian strait
   !!            3.3  !  2010-10  (C. Ethe, G. Madec)  reorganisation of initialisation phase
   !!            3.6  !  2016-03  (C. de Lavergne)  New param: internal wave-driven mixing 
   !!            4.0  !  2017-04  (G. Madec)  renamed module, remove the old param. and the CPP keys
   !!            4.0  !  2020-12  (C. de Lavergne)  Update param to match published one
   !!            4.0  !  2021-09  (C. de Lavergne)  Add energy from trapped and shallow internal tides
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   zdf_iwm       : global     momentum & tracer Kz with wave induced Kz
   !!   zdf_iwm_init  : global     momentum & tracer Kz with wave induced Kz
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean space and time domain variables
   USE zdf_oce        ! ocean vertical physics variables
   USE zdfddm         ! ocean vertical physics: double diffusive mixing
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE eosbn2         ! ocean equation of state
   USE phycst         ! physical constants
   !
   USE fldread        ! field read
   USE prtctl         ! Print control
   USE in_out_manager ! I/O manager
   USE iom            ! I/O Manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! Fortran utilities (allows no signed zero when 'key_nosignedzero' defined)  

   IMPLICIT NONE
   PRIVATE

   PUBLIC   zdf_iwm        ! called in step module 
   PUBLIC   zdf_iwm_init   ! called in nemogcm module 

   !                      !!* Namelist  namzdf_iwm : internal wave-driven mixing *
   LOGICAL ::  ln_mevar    ! variable (=T) or constant (=F) mixing efficiency
   LOGICAL ::  ln_tsdiff   ! account for differential T/S wave-driven mixing (=T) or not (=F)

   REAL(wp)::  r1_6 = 1._wp / 6._wp
   REAL(wp)::  rnu  = 1.4e-6_wp   ! molecular kinematic viscosity

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ebot_iwm   ! bottom-intensified dissipation above abyssal hills (W/m2)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ecri_iwm   ! bottom-intensified dissipation at topographic slopes (W/m2)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ensq_iwm   ! dissipation scaling with squared buoyancy frequency (W/m2)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   esho_iwm   ! dissipation due to shoaling internal tides (W/m2)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hbot_iwm   ! decay scale for abyssal hill dissipation (m)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hcri_iwm   ! inverse decay scale for topographic slope dissipation (m-1)

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION zdf_iwm_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION zdf_iwm_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( ebot_iwm(A2D(0)),  ecri_iwm(A2D(0)),  ensq_iwm(A2D(0)) ,     &
      &         esho_iwm(A2D(0)),  hbot_iwm(A2D(0)),  hcri_iwm(A2D(0)) , STAT=zdf_iwm_alloc )
      !
      CALL mpp_sum ( 'zdfiwm', zdf_iwm_alloc )
      IF( zdf_iwm_alloc /= 0 )   CALL ctl_stop( 'STOP', 'zdf_iwm_alloc: failed to allocate arrays' )
   END FUNCTION zdf_iwm_alloc


   SUBROUTINE zdf_iwm( kt, Kmm, p_avm, p_avt, p_avs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_iwm  ***
      !!                   
      !! ** Purpose :   add to the vertical mixing coefficients the effect of
      !!              breaking internal waves.
      !!
      !! ** Method  : - internal wave-driven vertical mixing is given by:
      !!                  Kz_wave = min( f( Reb = zemx_iwm / (Nu * N^2) ), 100 cm2/s  )
      !!              where zemx_iwm is the 3D space distribution of the wave-breaking 
      !!              energy and Nu the molecular kinematic viscosity.
      !!              The function f(Reb) is linear (constant mixing efficiency)
      !!              if the namelist parameter ln_mevar = F and nonlinear if ln_mevar = T.
      !!
      !!              - Compute zemx_iwm, the 3D power density that allows to compute
      !!              Reb and therefrom the wave-induced vertical diffusivity.
      !!              This is divided into four components:
      !!                 1. Bottom-intensified dissipation at topographic slopes, expressed
      !!              as an exponential decay above the bottom.
      !!                     zemx_iwm(z) = ( ecri_iwm / rho0 ) * EXP( -(H-z)/hcri_iwm )
      !!                                   / ( 1. - EXP( - H/hcri_iwm ) ) * hcri_iwm
      !!              where hcri_iwm is the characteristic length scale of the bottom 
      !!              intensification, ecri_iwm a static 2D map of available power, and 
      !!              H the ocean depth.
      !!                 2. Bottom-intensified dissipation above abyssal hills, expressed
      !!              as an algebraic decay above bottom.
      !!                     zemx_iwm(z) = ( ebot_iwm / rho0 ) * ( 1 + hbot_iwm/H ) 
      !!                                   / ( 1 + (H-z)/hbot_iwm )^2
      !!              where hbot_iwm is the characteristic length scale of the bottom 
      !!              intensification and ebot_iwm is a static 2D map of available power.
      !!                 3. Dissipation scaling in the vertical with the squared buoyancy 
      !!              frequency (N^2).
      !!                     zemx_iwm(z) = ( ensq_iwm / rho0 ) * rn2(z)
      !!                                   / ZSUM( rn2 * e3w )
      !!              where ensq_iwm is a static 2D map of available power.
      !!                 4. Dissipation due to shoaling internal tides, scaling in the
      !!              vertical with the buoyancy frequency (N).
      !!                     zemx_iwm(z) = ( esho_iwm / rho0 ) * sqrt(rn2(z))
      !!                                   / ZSUM( sqrt(rn2) * e3w )
      !!              where esho_iwm is a static 2D map of available power.
      !!
      !!              - update the model vertical eddy viscosity and diffusivity:
      !!                     avt  = avt  +    av_wave 
      !!                     avs  = avs  +    av_wave
      !!                     avm  = avm  +    av_wave
      !!
      !!              - if namelist parameter ln_tsdiff = T, account for differential mixing:
      !!                     avs  = avs  +    av_wave * diffusivity_ratio(Reb)
      !!
      !! ** Action  : - avt, avs, avm, increased by internal wave-driven mixing    
      !!
      !! References :  de Lavergne et al. JAMES 2020, https://doi.org/10.1029/2020MS002065
      !!               de Lavergne et al. JPO 2016, https://doi.org/10.1175/JPO-D-14-0259.1
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   kt             ! ocean time step
      INTEGER                         , INTENT(in   ) ::   Kmm            ! time level index
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::   p_avm          ! vertical eddy viscosity (w-points)
      REAL(wp), DIMENSION(A2D(0) ,jpk), INTENT(inout) ::   p_avt, p_avs   ! vertical eddy diffusivity (w-points)
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), SAVE :: zztmp    ! need save for the tile (not working when we will use openmp)
      !
      REAL(wp), DIMENSION(T2D(0)) ::   zfact1, zfact2, zfact3, zfact4 ! Used for vertical structure
      REAL(wp), DIMENSION(T2D(0)) ::   zReb        ! Turbulence intensity parameter
      REAL(wp), DIMENSION(T2D(0)) ::   zemx_iwm    ! local energy density available for mixing (W/kg)
      REAL(wp), DIMENSION(T2D(0)) ::   zav_ratio   ! S/T diffusivity ratio (only for ln_tsdiff=T)
      REAL(wp), DIMENSION(T2D(0)) ::   zav_wave    ! Internal wave-induced diffusivity
      REAL(wp), DIMENSION(T2D(0)) ::   z2d         ! 2D workspace used for iom_put
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztmp0, ztmp1, ztmp2 ! 3D workspace used for iom_put 
      !!----------------------------------------------------------------------
      !
      ! Initialize variables at 1 & jpk for diagnostics
      IF( iom_use( 'bflx_iwm') .OR. iom_use('pcmap_iwm') ) THEN
         ALLOCATE( ztmp0(T2D(0),jpk) )
         ztmp0(:,:,1  ) = 0._wp
         ztmp0(:,:,jpk) = 0._wp
      ENDIF
      IF( iom_use('emix_iwm') ) THEN
         ALLOCATE( ztmp1(T2D(0),jpk) )
         ztmp1(:,:,1  ) = 0._wp
         ztmp1(:,:,jpk) = 0._wp
      ENDIF
      IF( iom_use('av_ratio') ) THEN
         ALLOCATE( ztmp2(T2D(0),jpk) )
         ztmp2(:,:,1  ) = 1._wp * wmask(T2D(0),1)       
         ztmp2(:,:,jpk) = 1._wp * wmask(T2D(0),jpk)
      ENDIF
      !
      !                       ! ----------------------------- !
      !                       !  Internal wave-driven mixing  !  (compute zav_wave)
      !                       ! ----------------------------- !
      !                             
      !                       !* 'cri' component: distribute energy over the time-varying
      DO_2D( 0, 0, 0, 0 )                ! part independent of the level
         IF( ht(ji,jj,Kmm) /= 0._wp ) THEN ; zfact1(ji,jj) = ecri_iwm(ji,jj) * r1_rho0  &
            &                                                                / ( 1._wp - EXP( -ht(ji,jj,Kmm) * hcri_iwm(ji,jj) ) )
         ELSE                              ; zfact1(ji,jj) = 0._wp
         ENDIF
      END_2D
      !                       !* 'bot' component: distribute energy over the time-varying
      DO_2D( 0, 0, 0, 0 )               ! part independent of the level
         IF( ht(ji,jj,Kmm) /= 0._wp ) THEN ; zfact2(ji,jj) = ebot_iwm(ji,jj) * ( 1._wp +  hbot_iwm(ji,jj) / ht(ji,jj,Kmm) ) * r1_rho0
         ELSE                              ; zfact2(ji,jj) = 0._wp
         ENDIF
      END_2D
      !                       !* 'nsq' and 'sho' components: distribute energy over the time-varying 
      zfact3(:,:) = 0._wp
      zfact4(:,:) = 0._wp
      DO_3D( 0, 0, 0, 0, 2, jpkm1 )     ! part independent of the level
         zfact3(ji,jj) = zfact3(ji,jj) + e3w(ji,jj,jk,Kmm) *       MAX( 0._wp, rn2(ji,jj,jk) )
         zfact4(ji,jj) = zfact4(ji,jj) + e3w(ji,jj,jk,Kmm) * SQRT( MAX( 0._wp, rn2(ji,jj,jk) ) )
      END_3D
      DO_2D( 0, 0, 0, 0 )
         IF( zfact3(ji,jj) /= 0._wp )   zfact3(ji,jj) = ensq_iwm(ji,jj) * r1_rho0 / zfact3(ji,jj)
         IF( zfact4(ji,jj) /= 0._wp )   zfact4(ji,jj) = esho_iwm(ji,jj) * r1_rho0 / zfact4(ji,jj)
      END_2D

      
      DO jk = 2, jpkm1     ! complete with the level-dependent part
         
         DO_2D( 0, 0, 0, 0 ) 
            !                          !* 'cri' component: distribute energy over the time-varying
            !                          !* ocean depth using an exponential decay from the seafloor.
            zemx_iwm(ji,jj) = ( zfact1(ji,jj) * (  EXP( ( gdept(ji,jj,jk  ,Kmm) - ht(ji,jj,Kmm) ) * hcri_iwm(ji,jj) )   &
               &                                 - EXP( ( gdept(ji,jj,jk-1,Kmm) - ht(ji,jj,Kmm) ) * hcri_iwm(ji,jj) ) ) &
               !                       !* 'bot' component: distribute energy over the time-varying
               !                       !* ocean depth using an algebraic decay above the seafloor.
               &              + zfact2(ji,jj) * ( 1._wp / ( 1._wp + ( ht(ji,jj,Kmm) - gdept(ji,jj,jk  ,Kmm) ) / hbot_iwm(ji,jj) )   &
               &                                 -1._wp / ( 1._wp + ( ht(ji,jj,Kmm) - gdept(ji,jj,jk-1,Kmm) ) / hbot_iwm(ji,jj) ) ) &
               &              ) * wmask(ji,jj,jk) / e3w(ji,jj,jk,Kmm) &
               !                       !* 'nsq' component: distribute energy over the time-varying 
               !                       !* ocean depth as proportional to rn2
               &              + zfact3(ji,jj) *       MAX( 0._wp, rn2(ji,jj,jk) ) &
               !                       !* 'sho' component: distribute energy over the time-varying 
               !                       !* ocean depth as proportional to sqrt(rn2)
               &              + zfact4(ji,jj) * SQRT( MAX( 0._wp, rn2(ji,jj,jk) ) )
            !
            ! Calculate turbulence intensity parameter Reb
            zReb(ji,jj) = zemx_iwm(ji,jj) / MAX( 1.e-20_wp, rnu * rn2(ji,jj,jk) )
            ! Define internal wave-induced diffusivity
            zav_wave(ji,jj) = zReb(ji,jj) * r1_6 * rnu  ! This corresponds to a constant mixing efficiency of 1/6
         END_2D
         !
         IF( ln_mevar ) THEN    ! Variable mixing efficiency case : modify zav_wave in the
            DO_2D( 0, 0, 0, 0 ) ! energetic (Reb > 480) and buoyancy-controlled (Reb <10.224) regimes
               IF( zReb(ji,jj) > 480.00_wp ) THEN
                  zav_wave(ji,jj) = 3.6515_wp  * rnu                * SQRT( MAX( 0._wp, zReb(ji,jj) ) )
               ELSEIF( zReb(ji,jj) < 10.224_wp ) THEN
                  zav_wave(ji,jj) = 0.052125_wp * rnu * zReb(ji,jj) * SQRT( MAX( 0._wp, zReb(ji,jj) ) )
               ENDIF
            END_2D
         ENDIF
         !
         DO_2D( 0, 0, 0, 0 )    ! Bound diffusivity by molecular value and 100 cm2/s
            zav_wave(ji,jj) = MIN( MAX( 1.4e-7_wp, zav_wave(ji,jj) ), 1.e-2_wp ) * wmask(ji,jj,jk)
         END_2D
         !
         !                          ! ----------------------- !
         !                          !   Update  mixing coefs  !                          
         !                          ! ----------------------- !
         !
         IF( ln_tsdiff ) THEN                !* Option for differential mixing of salinity and temperature
            DO_2D( 0, 0, 0, 0 ) ! Calculate S/T diffusivity ratio as a function of Reb (else it is set to 1)
               zav_ratio(ji,jj) = ( 0.505_wp + &
                  &                 0.495_wp * TANH( 0.92_wp * ( LOG10( MAX( 1.e-20, zReb(ji,jj) * 5._wp * r1_6 ) ) - 0.60_wp ) ) &
                  &               ) * wmask(ji,jj,jk)
            END_2D
         ELSE
            DO_2D( 0, 0, 0, 0 )
               zav_ratio(ji,jj) = 1._wp * wmask(ji,jj,jk)
            END_2D
         ENDIF
         !
         DO_2D( 0, 0, 0, 0 ) !* update momentum & tracer diffusivity with wave-driven mixing
            p_avs(ji,jj,jk) = p_avs(ji,jj,jk) + zav_wave(ji,jj) * zav_ratio(ji,jj)
            p_avt(ji,jj,jk) = p_avt(ji,jj,jk) + zav_wave(ji,jj)
            p_avm(ji,jj,jk) = p_avm(ji,jj,jk) + zav_wave(ji,jj)
         END_2D

         ! record jk for outputs
         IF( iom_use( 'bflx_iwm') .OR. iom_use('pcmap_iwm') )   ztmp0(:,:,jk) = zav_wave (:,:)
         IF( iom_use('emix_iwm') )                              ztmp1(:,:,jk) = zemx_iwm (:,:)
         IF( iom_use('av_ratio') )                              ztmp2(:,:,jk) = zav_ratio(:,:)        
         
      ENDDO

      ! outputs
      IF( iom_use('av_wave' ) )   CALL iom_put( "av_wave" , ztmp0 )
      IF( iom_use('emix_iwm') )   CALL iom_put( "emix_iwm", ztmp1 )
      IF( iom_use('av_ratio') )   CALL iom_put( "av_ratio", ztmp2 )

      !* output useful diagnostics: Kz*N^2 , 
      !  vertical integral of rho0 * Kz * N^2 , energy density (zemx_iwm)
      IF( iom_use( 'bflx_iwm') .OR. iom_use('pcmap_iwm') ) THEN
         z2d(:,:) = 0._wp      
         DO_3D( 0, 0, 0, 0, 2, jpkm1 )
            ztmp0(ji,jj,jk) = MAX( 0._wp, rn2(ji,jj,jk) ) * ztmp0(ji,jj,jk)
            z2d(ji,jj) = z2d(ji,jj) + rho0 * e3w(ji,jj,jk,Kmm) * ztmp0(ji,jj,jk) * wmask(ji,jj,jk)
         END_3D
         CALL iom_put(  "bflx_iwm", ztmp0 )
         CALL iom_put( "pcmap_iwm", z2d   )
      ENDIF
      !
      !* Control print at first time-step: diagnose the energy consumed by zav_wave
      IF( kt == nit000 .AND. ( iom_use( 'bflx_iwm') .OR. iom_use('pcmap_iwm') ) ) THEN
         IF( .NOT. l_istiled .OR. ntile == 1 ) zztmp = 0._wp                    ! Do only on the first tile
         DO_3D( 0, 0, 0, 0, 2, jpkm1 )   ! would be better to use DDPDD and COMPLEX...
            zztmp = zztmp + e3w(ji,jj,jk,Kmm) * e1e2t(ji,jj) * ztmp0(ji,jj,jk) * wmask(ji,jj,jk) * smask0_i(ji,jj)
         END_3D

         IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN                       ! Do only on the last tile
            CALL mpp_sum( 'zdfiwm', zztmp )
            zztmp = rho0 * zztmp ! Global integral of rho0 * Kz * N^2 = power contributing to mixing
            !
            IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'zdf_iwm : Internal wave-driven mixing (iwm)'
               WRITE(numout,*) '~~~~~~~ '
               WRITE(numout,*)
               WRITE(numout,*) '      Total power consumption by av_wave =  ', zztmp * 1.e-12_wp, 'TW'
            ENDIF
         ENDIF
      ENDIF

      IF(sn_cfctl%l_prtctl)   CALL prt_ctl(tab3d_1=p_avt , clinfo1=' iwm - N2*av_wave: ')
      !
      IF( iom_use( 'bflx_iwm') .OR. iom_use('pcmap_iwm') )   DEALLOCATE( ztmp0 )
      IF( iom_use('emix_iwm') )                              DEALLOCATE( ztmp1 )
      IF( iom_use('av_ratio') )                              DEALLOCATE( ztmp2 )
      !
   END SUBROUTINE zdf_iwm


   SUBROUTINE zdf_iwm_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE zdf_iwm_init  ***
      !!                     
      !! ** Purpose :   Initialization of the internal wave-driven vertical mixing, reading
      !!              of input power maps and decay length scales in a netcdf file.
      !!
      !! ** Method  : - Read the namzdf_iwm namelist and check the parameters
      !!
      !!              - Read the input data in a NetCDF file (zdfiwm_forcing.nc) with variables:
      !!              'power_bot' bottom-intensified dissipation above abyssal hills
      !!              'power_cri' bottom-intensified dissipation at topographic slopes
      !!              'power_nsq' dissipation scaling with squared buoyancy frequency
      !!              'power_sho' dissipation due to shoaling internal tides
      !!              'scale_bot' decay scale for abyssal hill dissipation
      !!              'scale_cri' decay scale for topographic-slope dissipation
      !!
      !! ** input   : - Namlist namzdf_iwm
      !!              - NetCDF file : zdfiwm_forcing.nc
      !!
      !! ** Action  : - Increase by 1 the nstop flag is setting problem encounter
      !!              - Define ebot_iwm, ecri_iwm, ensq_iwm, esho_iwm, hbot_iwm, hcri_iwm
      !!
      !! References : de Lavergne et al. JAMES 2020, https://doi.org/10.1029/2020MS002065
      !!----------------------------------------------------------------------
      INTEGER  ::   ifpr               ! dummy loop indices
      INTEGER  ::   inum               ! local integer
      INTEGER  ::   ios
      !
      CHARACTER(len=256)            ::   cn_dir                 ! Root directory for location of ssr files
      INTEGER, PARAMETER            ::   jpiwm  = 6             ! maximum number of variables to read
      INTEGER, PARAMETER            ::   jp_mpb = 1
      INTEGER, PARAMETER            ::   jp_mpc = 2
      INTEGER, PARAMETER            ::   jp_mpn = 3
      INTEGER, PARAMETER            ::   jp_mps = 4
      INTEGER, PARAMETER            ::   jp_dsb = 5
      INTEGER, PARAMETER            ::   jp_dsc = 6
      INTEGER                       ::   ji, jj
      !
      TYPE(FLD_N), DIMENSION(jpiwm) ::   slf_iwm                        ! array of namelist informations
      TYPE(FLD_N)                   ::   sn_mpb, sn_mpc, sn_mpn, sn_mps ! information about Mixing Power field to be read
      TYPE(FLD_N)                   ::   sn_dsb, sn_dsc                 ! information about Decay Scale field to be read
      TYPE(FLD  ), DIMENSION(jpiwm) ::   sf_iwm                         ! structure of input fields (file informations, fields read)
      !
      REAL(wp), DIMENSION(A2D(0),4) ::   ztmp
      REAL(wp), DIMENSION(4)        ::   zdia
      REAL(wp)                      ::   zcte
      !
      NAMELIST/namzdf_iwm/ ln_mevar, ln_tsdiff, &
          &                cn_dir, sn_mpb, sn_mpc, sn_mpn, sn_mps, sn_dsb, sn_dsc
      !!----------------------------------------------------------------------
      !
      READ_NML_REF(numnam,namzdf_iwm)
      READ_NML_CFG(numnam,namzdf_iwm)
      IF(lwm) WRITE ( numond, namzdf_iwm )
      !
      IF(lwp) THEN                  ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'zdf_iwm_init : internal wave-driven mixing'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namzdf_iwm : set wave-driven mixing parameters'
         WRITE(numout,*) '      Variable (T) or constant (F) mixing efficiency            = ', ln_mevar
         WRITE(numout,*) '      Differential internal wave-driven mixing (T) or not (F)   = ', ln_tsdiff
      ENDIF
      
      ! This internal-wave-driven mixing parameterization elevates avt and avm in the interior, and
      ! ensures that avt remains larger than its molecular value (=1.4e-7). Therefore, avtb should 
      ! be set here to a very small value, and avmb to its (uniform) molecular value (=1.4e-6).
      avmb(:) = rnu               ! molecular value
      avtb(:) = 1.e-10_wp         ! very small diffusive minimum (background avt is specified in zdf_iwm)
      avtb_2d(:,:) = 1._wp        ! uniform
      IF(lwp) THEN                ! Control print
         WRITE(numout,*)
         WRITE(numout,*) '   Force the background value applied to avm & avt in TKE to be everywhere ',   &
            &               'the viscous molecular value & a very small diffusive value, resp.'
      ENDIF
            
      !                             ! allocate iwm arrays
      IF( zdf_iwm_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'zdf_iwm_init : unable to allocate iwm arrays' )
      !
      ! store namelist information in an array
      slf_iwm(jp_mpb) = sn_mpb ; slf_iwm(jp_mpc) = sn_mpc ; slf_iwm(jp_mpn) = sn_mpn ; slf_iwm(jp_mps) = sn_mps
      slf_iwm(jp_dsb) = sn_dsb ; slf_iwm(jp_dsc) = sn_dsc
      !
      DO ifpr= 1, jpiwm
         ALLOCATE( sf_iwm(ifpr)%fnow(A2D(0),1)   )
         IF( slf_iwm(ifpr)%ln_tint ) ALLOCATE( sf_iwm(ifpr)%fdta(A2D(0),1,2) )
      END DO

      ! fill sf_iwm with sf_iwm and control print
      CALL fld_fill( sf_iwm, slf_iwm , cn_dir, 'zdfiwm_init', 'iwm input file', 'namiwm' )

      !                             ! hard-coded default values
      DO_2D( 0, 0, 0, 0 )
         sf_iwm(jp_mpb)%fnow(ji,jj,1) = 1.e-10_wp
         sf_iwm(jp_mpc)%fnow(ji,jj,1) = 1.e-10_wp
         sf_iwm(jp_mpn)%fnow(ji,jj,1) = 1.e-5_wp
         sf_iwm(jp_mps)%fnow(ji,jj,1) = 1.e-10_wp
         sf_iwm(jp_dsb)%fnow(ji,jj,1) = 100._wp
         sf_iwm(jp_dsc)%fnow(ji,jj,1) = 100._wp
      END_2D

      !                             ! read necessary fields
      CALL fld_read( nit000, 1, sf_iwm )

      DO_2D( 0, 0, 0, 0 )
         zcte = smask0(ji,jj)
         ebot_iwm(ji,jj) = sf_iwm(1)%fnow(ji,jj,1) * zcte    ! energy flux for dissipation above abyssal hills [W/m2]
         ecri_iwm(ji,jj) = sf_iwm(2)%fnow(ji,jj,1) * zcte    ! energy flux for dissipation at topographic slopes [W/m2]
         ensq_iwm(ji,jj) = sf_iwm(3)%fnow(ji,jj,1) * zcte    ! energy flux for dissipation scaling with N^2 [W/m2]
         esho_iwm(ji,jj) = sf_iwm(4)%fnow(ji,jj,1) * zcte    ! energy flux for dissipation due to shoaling [W/m2]
         hbot_iwm(ji,jj) = sf_iwm(5)%fnow(ji,jj,1)           ! spatially variable decay scale for abyssal hill dissipation [m]
         hcri_iwm(ji,jj) = 1._wp / sf_iwm(6)%fnow(ji,jj,1)   ! inverse decay scale for topographic slope dissipation [m-1]
      END_2D

      ! diags
      DO_2D( 0, 0, 0, 0 )
         zcte = e1e2t(ji,jj)
         ztmp(ji,jj,1) = zcte * ebot_iwm(ji,jj)
         ztmp(ji,jj,2) = zcte * ecri_iwm(ji,jj)
         ztmp(ji,jj,3) = zcte * ensq_iwm(ji,jj)
         ztmp(ji,jj,4) = zcte * esho_iwm(ji,jj)
      END_2D

      zdia(:) = glob_2Dsum( 'zdfiwm', ztmp )

      IF(lwp) THEN
         WRITE(numout,*) '      Dissipation above abyssal hills:        ', zdia(1) * 1.e-12_wp, 'TW'
         WRITE(numout,*) '      Dissipation along topographic slopes:   ', zdia(2) * 1.e-12_wp, 'TW'
         WRITE(numout,*) '      Dissipation scaling with N^2:           ', zdia(3) * 1.e-12_wp, 'TW'
         WRITE(numout,*) '      Dissipation due to shoaling:            ', zdia(4) * 1.e-12_wp, 'TW'
      ENDIF
      !
   END SUBROUTINE zdf_iwm_init

   !!======================================================================
END MODULE zdfiwm
