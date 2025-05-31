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
   USE oce            ! ocean dynamics and tracers 
   USE isf_oce
   USE isfcpl
   USE abl            ! abl variables in case ln_abl = .true.
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE dianam         ! build name of file (routine)
   USE diahth         ! thermocline diagnostics
   USE dynadv   , ONLY: ln_dynadv_vec
   USE icb_oce        ! Icebergs
   USE icbdia         ! Iceberg budgets
   USE ldftra         ! lateral physics: eddy diffusivity coef.
   USE ldfdyn         ! lateral physics: eddy viscosity   coef.
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE sbc_ice        ! Surface boundary condition: ice fields
   USE sbcssr         ! restoring term toward SST/SSS climatology
   USE sbcwave        ! wave parameters
   USE wet_dry        ! wetting and drying
   USE zdf_oce        ! ocean vertical physics
   USE zdfdrg         ! ocean vertical physics: top/bottom friction
   USE zdfmxl         ! mixed layer
   USE zdfosm         ! mixed layer
   !
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager ! I/O manager
   USE dia25h         ! 25h Mean output
   USE iom            ! 
   USE ioipsl         ! 

#if defined key_si3
   USE icewri 
   USE ice   ,    ONLY: ato_i
#endif
   USE lib_mpp         ! MPP library
   USE timing          ! preformance summary
   USE diu_bulk        ! diurnal warm layer
   USE diu_coolskin    ! Cool skin

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dia_wri                 ! routines called by step.F90
   PUBLIC   dia_wri_state
   
   INTEGER ::   nid_T, nz_T, nh_T
   INTEGER ::          nb_T
   INTEGER ::   nid_U, nz_U, nh_U
   INTEGER ::   nid_V, nz_V, nh_V
   INTEGER ::   nid_W, nz_W, nh_W
   INTEGER ::   nid_A, nz_A, nh_A

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_xios
   !!----------------------------------------------------------------------
   !!   'key_xios'                                        use IOM library
   !!----------------------------------------------------------------------
   
   SUBROUTINE dia_wri( kt, Kmm )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE dia_wri  ***
      !!                   
      !! ** Purpose :   Standard output of opa: dynamics and tracer fields 
      !!      NETCDF format is used by default 
      !!
      !! ** Method  :  use iom_put
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kmm     ! ocean time level index
      !!
      INTEGER ::   ji, jj, jk       ! dummy loop indices
      INTEGER ::   ikbot            ! local integer
      REAL(wp)::   ztmp , ztmpx, ztmpy   ! local scalar
      REAL(wp)::   ztau1, ztau2, ztau3, ztau4    ! local scalar
      REAL(wp), DIMENSION(T2D(0))     ::   z2d   ! 2D workspace
      REAL(wp), DIMENSION(T2D(0),jpk) ::   z3d   ! 3D workspace
      !!----------------------------------------------------------------------
      ! 
      IF( ln_timing )   CALL timing_start('dia_wri')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN   ! Do only for the first tile
         ! Output the initial state and forcings
         IF( ninist == 1 ) THEN
            CALL dia_wri_state( Kmm, 'output.init' )
            ninist = 0
         ENDIF
      ENDIF

      ! Output of initial vertical scale factor (this may be 1D or 3D, and we want only the internal part)
      IF( iom_use("e3t_0") .OR. iom_use("e3u_0") .OR. iom_use("e3v_0") .OR. iom_use("e3f_0") ) THEN 
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3t_0(ji,jj,jk)
         END_3D
         CALL iom_put( "e3t_0", z3d )
         !
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3u_0(ji,jj,jk)
         END_3D
         CALL iom_put( "e3u_0", z3d )
         !
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3v_0(ji,jj,jk)
         END_3D
         CALL iom_put( "e3v_0", z3d )
         !
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3f_0(ji,jj,jk)
         END_3D
         CALL iom_put( "e3f_0", z3d )
      ENDIF
      !
      IF ( iom_use("tpt_dep") ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) = gdept(ji,jj,jk,Kmm)
         END_3D
         CALL iom_put( "tpt_dep", z3d )
      ENDIF

      ! --- vertical scale factors --- !
      IF ( iom_use("e3t") .OR. iom_use("e3tdef") ) THEN  ! time-varying e3t
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3t(ji,jj,jk,Kmm)
         END_3D
         CALL iom_put( "e3t", z3d )
         IF ( iom_use("e3tdef") ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d(ji,jj,jk) = ( ( z3d(ji,jj,jk) - e3t_0(ji,jj,jk) ) / e3t_0(ji,jj,jk) * 100._wp * tmask(ji,jj,jk) ) ** 2
            END_3D
            CALL iom_put( "e3tdef", z3d )
         ENDIF
      ENDIF
      IF ( iom_use("e3u") ) THEN                         ! time-varying e3u
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3u(ji,jj,jk,Kmm)
         END_3D
         CALL iom_put( "e3u" , z3d )
      ENDIF
      IF ( iom_use("e3v") ) THEN                         ! time-varying e3v
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3v(ji,jj,jk,Kmm)
         END_3D
         CALL iom_put( "e3v" , z3d )
      ENDIF
      IF ( iom_use("e3w") ) THEN                         ! time-varying e3w
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3w(ji,jj,jk,Kmm)
         END_3D
         CALL iom_put( "e3w" , z3d )
      ENDIF
      IF ( iom_use("e3f") ) THEN                         ! time-varying e3f caution here at Kaa
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  e3f(ji,jj,jk)
         END_3D
         CALL iom_put( "e3f" , z3d )
      ENDIF

      IF ( iom_use("ssh") ) THEN
         IF( ll_wd ) THEN                                ! sea surface height (brought back to the reference used for wetting and drying)
            CALL iom_put( "ssh" , (ssh(:,:,Kmm)+ssh_ref)*ssmask(:,:) )
         ELSE
            CALL iom_put( "ssh" ,  ssh(:,:,Kmm) )        ! sea surface height
         ENDIF
      ENDIF

      IF( iom_use("wetdep") )    CALL iom_put( "wetdep" , ht_0(:,:) + ssh(:,:,Kmm) )   ! wet depth

#if defined key_qco
      CALL iom_put( "ht" , ht(:,:,Kmm) )   ! water column at t-point
      CALL iom_put( "hu" , hu(:,:,Kmm) )   ! water column at u-point
      CALL iom_put( "hv" , hv(:,:,Kmm) )   ! water column at v-point
      CALL iom_put( "hf" , hf_0(:,:)*( 1._wp + r3f(:,:) ) )   ! water column at f-point (caution here at Naa)
#endif

      ! --- tracers T&S --- !
      CALL iom_put( "toce", ts(:,:,:,jp_tem,Kmm) )    ! 3D temperature
      CALL iom_put(  "sst", ts(:,:,1,jp_tem,Kmm) )    ! surface temperature

      IF ( iom_use("sbt") ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikbot = mbkt(ji,jj)
            z2d(ji,jj) = ts(ji,jj,ikbot,jp_tem,Kmm)
         END_2D
         CALL iom_put( "sbt", z2d )                ! bottom temperature
      ENDIF

      CALL iom_put( "soce", ts(:,:,:,jp_sal,Kmm) )    ! 3D salinity
      CALL iom_put(  "sss", ts(:,:,1,jp_sal,Kmm) )    ! surface salinity
      IF ( iom_use("sbs") ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikbot = mbkt(ji,jj)
            z2d(ji,jj) = ts(ji,jj,ikbot,jp_sal,Kmm)
         END_2D
         CALL iom_put( "sbs", z2d )                ! bottom salinity
      ENDIF

      IF( .NOT.lk_SWE )   CALL iom_put( "rhop", rhop(:,:,:) )          ! 3D potential density (sigma0)

      ! --- momentum --- !
      IF ( iom_use("taubot") ) THEN                ! bottom stress
         ztmp = rho0 * 0.25_wp
         DO_2D( 0, 0, 0, 0 )
            ztau1 = ( rCdU_bot(ji+1,jj) + rCdU_bot(ji  ,jj) ) * uu(ji  ,jj,mbku(ji  ,jj),Kmm)
            ztau2 = ( rCdU_bot(ji  ,jj) + rCdU_bot(ji-1,jj) ) * uu(ji-1,jj,mbku(ji-1,jj),Kmm)
            ztau3 = ( rCdU_bot(ji,jj+1) + rCdU_bot(ji,jj  ) ) * vv(ji,jj  ,mbkv(ji,jj  ),Kmm)
            ztau4 = ( rCdU_bot(ji,jj  ) + rCdU_bot(ji,jj-1) ) * vv(ji,jj-1,mbkv(ji,jj-1),Kmm)
            z2d(ji,jj) = ztmp * SQRT( ztau1*ztau1 + ztau2*ztau2 + ztau3*ztau3 + ztau4*ztau4 ) * tmask(ji,jj,1)
         END_2D
         CALL iom_put( "taubot", z2d )
      ENDIF
         
      CALL iom_put( "uoce", uu(:,:,:,Kmm) )            ! 3D i-current
      CALL iom_put(  "ssu", uu(:,:,1,Kmm) )            ! surface i-current
      IF ( iom_use("sbu") ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikbot = mbku(ji,jj)
            z2d(ji,jj) = uu(ji,jj,ikbot,Kmm)
         END_2D
         CALL iom_put( "sbu", z2d )                ! bottom i-current
      ENDIF
      
      CALL iom_put( "voce", vv(:,:,:,Kmm) )            ! 3D j-current
      CALL iom_put(  "ssv", vv(:,:,1,Kmm) )            ! surface j-current
      IF ( iom_use("sbv") ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikbot = mbkv(ji,jj)
            z2d(ji,jj) = vv(ji,jj,ikbot,Kmm)
         END_2D
         CALL iom_put( "sbv", z2d )                ! bottom j-current
      ENDIF
      !                                            ! vertical velocity
      IF( iom_use('woce') ) THEN
         IF( ln_zad_Aimp ) THEN  ! explicit plus implicit parts
            z3d(T2D(0),:) = ww(T2D(0),:) + wi(T2D(0),:)
         ELSE
            z3d(T2D(0),:) = ww(T2D(0),:)
         ENDIF
         CALL iom_put( "woce", z3d )
      ENDIF

      IF( iom_use('w_masstr') .OR. iom_use('w_masstr2') ) THEN   ! vertical mass transport & its square value
         !                     ! Caution: in the VVL case, it only correponds to the baroclinic mass transport.
         IF( ln_zad_Aimp ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d(ji,jj,jk) = rho0 * e1e2t(ji,jj) * ( ww(ji,jj,jk) + wi(ji,jj,jk) )
            END_3D
         ELSE
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d(ji,jj,jk) = rho0 * e1e2t(ji,jj) * ww(ji,jj,jk)
            END_3D
         ENDIF
         CALL iom_put( "w_masstr" , z3d )
         IF( iom_use('w_masstr2') )   CALL iom_put( "w_masstr2", z3d * z3d )
      ENDIF

      CALL iom_put( "avt" , avt )        ! T vert. eddy diff. coef.
      CALL iom_put( "avs" , avs )        ! S vert. eddy diff. coef.
      CALL iom_put( "avm" , avm )        ! T vert. eddy visc. coef.

      IF( iom_use('logavt') )   CALL iom_put( "logavt", LOG( MAX( 1.e-20_wp, avt(:,:,:) ) ) )
      IF( iom_use('logavs') )   CALL iom_put( "logavs", LOG( MAX( 1.e-20_wp, avs(:,:,:) ) ) )

      IF ( iom_use("sssgrad") .OR. iom_use("sssgrad2") ) THEN
         DO_2D( 0, 0, 0, 0 )                       ! sss gradient
            ztmp  = ts(ji,jj,1,jp_sal,Kmm)
            ztmpx = (ts(ji+1,jj,1,jp_sal,Kmm) - ztmp) * r1_e1u(ji,jj) + (ztmp - ts(ji-1,jj  ,1,jp_sal,Kmm)) * r1_e1u(ji-1,jj)
            ztmpy = (ts(ji,jj+1,1,jp_sal,Kmm) - ztmp) * r1_e2v(ji,jj) + (ztmp - ts(ji  ,jj-1,1,jp_sal,Kmm)) * r1_e2v(ji,jj-1)
            z2d(ji,jj) = 0.25_wp * ( ztmpx * ztmpx + ztmpy * ztmpy )   &
               &                 * umask(ji,jj,1) * umask(ji-1,jj,1) * vmask(ji,jj,1) * vmask(ji,jj-1,1)
         END_2D
         CALL iom_put( "sssgrad2",  z2d )          ! square of module of sss gradient
         IF ( iom_use("sssgrad") ) THEN
            DO_2D( 0, 0, 0, 0 )
               z2d(ji,jj) = SQRT( z2d(ji,jj) )
            END_2D
            CALL iom_put( "sssgrad",  z2d )        ! module of sss gradient
         ENDIF
      ENDIF

      IF ( iom_use("sstgrad") .OR. iom_use("sstgrad2") ) THEN
         DO_2D( 0, 0, 0, 0 )                       ! sst gradient
            ztmp  = ts(ji,jj,1,jp_tem,Kmm)
            ztmpx = ( ts(ji+1,jj,1,jp_tem,Kmm) - ztmp ) * r1_e1u(ji,jj) + ( ztmp - ts(ji-1,jj  ,1,jp_tem,Kmm) ) * r1_e1u(ji-1,jj)
            ztmpy = ( ts(ji,jj+1,1,jp_tem,Kmm) - ztmp ) * r1_e2v(ji,jj) + ( ztmp - ts(ji  ,jj-1,1,jp_tem,Kmm) ) * r1_e2v(ji,jj-1)
            z2d(ji,jj) = 0.25_wp * ( ztmpx * ztmpx + ztmpy * ztmpy )   &
               &                 * umask(ji,jj,1) * umask(ji-1,jj,1) * vmask(ji,jj,1) * vmask(ji,jj-1,1)
         END_2D
         CALL iom_put( "sstgrad2",  z2d )          ! square of module of sst gradient
         IF ( iom_use("sstgrad") ) THEN
            DO_2D( 0, 0, 0, 0 )
               z2d(ji,jj) = SQRT( z2d(ji,jj) )
            END_2D
            CALL iom_put( "sstgrad",  z2d )        ! module of sst gradient
         ENDIF
      ENDIF

      ! heat and salt contents
      IF( iom_use("heatc") ) THEN
         z2d(:,:)  = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_tem,Kmm) * tmask(ji,jj,jk) * rho0_rcp
         END_3D
         CALL iom_put( "heatc", z2d )   ! vertically integrated heat content (J/m2)
      ENDIF

      IF( iom_use("saltc") ) THEN
         z2d(:,:)  = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_sal,Kmm) * tmask(ji,jj,jk) * rho0
         END_3D
         CALL iom_put( "saltc", z2d )       ! vertically integrated salt content (PSU*kg/m2)
      ENDIF
      !
      IF( iom_use("salt2c") ) THEN
         z2d(:,:)  = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_sal,Kmm) * ts(ji,jj,jk,jp_sal,Kmm) * tmask(ji,jj,jk) * rho0
         END_3D
         CALL iom_put( "salt2c", z2d )      ! vertically integrated square of salt content (PSU2*kg/m2)
      ENDIF
      !
      IF ( iom_use("ke") .OR. iom_use("ke_int") ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpk )
            ztmpx = uu(ji-1,jj  ,jk,Kmm) + uu(ji,jj,jk,Kmm)
            ztmpy = vv(ji  ,jj-1,jk,Kmm) + vv(ji,jj,jk,Kmm)
            z3d(ji,jj,jk) = 0.25_wp * ( ztmpx*ztmpx + ztmpy*ztmpy )
         END_3D
         CALL iom_put( "ke", z3d )                 ! kinetic energy

         z2d(:,:)  = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + e3t(ji,jj,jk,Kmm) * z3d(ji,jj,jk) * e1e2t(ji,jj) * tmask(ji,jj,jk)
         END_3D
         CALL iom_put( "ke_int", z2d )             ! vertically integrated kinetic energy
      ENDIF
      !
      IF ( iom_use("sKE") ) THEN                   ! surface kinetic energy at T point
         DO_2D( 0, 0, 0, 0 )
            z2d(ji,jj) = 0.25_wp * ( uu(ji  ,jj,1,Kmm) * uu(ji  ,jj,1,Kmm) * e1e2u(ji  ,jj) * e3u(ji  ,jj,1,Kmm)  &
               &                   + uu(ji-1,jj,1,Kmm) * uu(ji-1,jj,1,Kmm) * e1e2u(ji-1,jj) * e3u(ji-1,jj,1,Kmm)  &
               &                   + vv(ji,jj  ,1,Kmm) * vv(ji,jj  ,1,Kmm) * e1e2v(ji,jj  ) * e3v(ji,jj  ,1,Kmm)  &
               &                   + vv(ji,jj-1,1,Kmm) * vv(ji,jj-1,1,Kmm) * e1e2v(ji,jj-1) * e3v(ji,jj-1,1,Kmm)  )  &
               &                 * r1_e1e2t(ji,jj) / e3t(ji,jj,1,Kmm) * ssmask(ji,jj)
         END_2D
         CALL iom_put( "sKE" , z2d )
      ENDIF
      !
      IF ( iom_use("ssKEf") ) THEN                 ! surface kinetic energy at F point
         ! CAUTION : only valid in SWE, not with bathymetry
         DO_2D( 0, 0, 0, 0 )
            z2d(ji,jj) = 0.25_wp * ( uu(ji,jj  ,1,Kmm) * uu(ji,jj  ,1,Kmm) * e1e2u(ji,jj  ) * e3u(ji,jj  ,1,Kmm)  &
               &                   + uu(ji,jj+1,1,Kmm) * uu(ji,jj+1,1,Kmm) * e1e2u(ji,jj+1) * e3u(ji,jj+1,1,Kmm)  &
               &                   + vv(ji  ,jj,1,Kmm) * vv(ji,jj  ,1,Kmm) * e1e2v(ji  ,jj) * e3v(ji  ,jj,1,Kmm)  &
               &                   + vv(ji+1,jj,1,Kmm) * vv(ji+1,jj,1,Kmm) * e1e2v(ji+1,jj) * e3v(ji+1,jj,1,Kmm)  )  &
               &                 * r1_e1e2f(ji,jj) / e3f(ji,jj,1) * ssfmask(ji,jj)
         END_2D
         CALL iom_put( "ssKEf", z2d )
      ENDIF
      !
      IF ( iom_use("hdiv") ) THEN                 ! Horizontal divergence
         z3d(T2D(0),1:jpkm1) = hdiv(T2D(0),1:jpkm1)
         z3d(T2D(0),jpk) = 0._wp
         CALL iom_put( "hdiv", z3d )
      ENDIF
      !
      IF( iom_use("u_masstr") .OR. iom_use("u_masstr_vint") .OR. iom_use("u_heattr") .OR. iom_use("u_salttr") ) THEN

         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) = rho0 * uu(ji,jj,jk,Kmm) * e2u(ji,jj) * e3u(ji,jj,jk,Kmm) * umask(ji,jj,jk)
         END_3D
         CALL iom_put( "u_masstr"     , z3d )      ! mass transport in i-direction

         IF( iom_use("u_masstr_vint") ) THEN
            z2d(:,:) = 0._wp
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               z2d(ji,jj) = z2d(ji,jj) + z3d(ji,jj,jk)
            END_3D
            CALL iom_put( "u_masstr_vint", z2d )   ! mass transport in i-direction vertical sum
         ENDIF
         IF( iom_use("u_heattr") ) THEN
            z2d(:,:) = 0._wp
            ztmp = 0.5_wp * rcp
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               z2d(ji,jj) = z2d(ji,jj) + ztmp * z3d(ji,jj,jk) * ( ts(ji,jj,jk,jp_tem,Kmm) + ts(ji+1,jj,jk,jp_tem,Kmm) )
            END_3D
            CALL iom_put( "u_heattr", z2d )        ! heat transport in i-direction
         ENDIF
         IF( iom_use("u_salttr") ) THEN
            z2d(:,:) = 0._wp
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               z2d(ji,jj) = z2d(ji,jj) +   0.5 * z3d(ji,jj,jk) * ( ts(ji,jj,jk,jp_sal,Kmm) + ts(ji+1,jj,jk,jp_sal,Kmm) )
            END_3D
            CALL iom_put( "u_salttr", z2d )        ! heat transport in i-direction
         ENDIF

      ENDIF

      IF( iom_use("v_masstr") .OR. iom_use("v_heattr") .OR. iom_use("v_salttr") ) THEN

         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) = rho0 * vv(ji,jj,jk,Kmm) * e1v(ji,jj) * e3v(ji,jj,jk,Kmm) * vmask(ji,jj,jk)
         END_3D
         CALL iom_put( "v_masstr", z3d )           ! mass transport in j-direction

         IF( iom_use("v_heattr") ) THEN
            z2d(:,:) = 0._wp
            ztmp = 0.5_wp * rcp
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               z2d(ji,jj) = z2d(ji,jj) + ztmp * z3d(ji,jj,jk) * ( ts(ji,jj,jk,jp_tem,Kmm) + ts(ji,jj+1,jk,jp_tem,Kmm) )
            END_3D
            CALL iom_put( "v_heattr", z2d )        !  heat transport in j-direction
         ENDIF
         IF( iom_use("v_salttr") ) THEN
            z2d(:,:) = 0._wp
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               z2d(ji,jj) = z2d(ji,jj) +   0.5 * z3d(ji,jj,jk) * ( ts(ji,jj,jk,jp_sal,Kmm) + ts(ji,jj+1,jk,jp_sal,Kmm) )
            END_3D
            CALL iom_put( "v_salttr", z2d )        !  heat transport in j-direction
         ENDIF

      ENDIF

      IF( iom_use("tosmint") ) THEN
         z2d(:,:) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + rho0 * e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_tem,Kmm)
         END_3D
         CALL iom_put( "tosmint", z2d )            ! Vertical integral of temperature
      ENDIF
      IF( iom_use("somint") ) THEN
         z2d(:,:) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + rho0 * e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_sal,Kmm)
         END_3D
         CALL iom_put( "somint", z2d )             ! Vertical integral of salinity
      ENDIF

      CALL iom_put( "bn2", rn2 )                   ! Brunt-Vaisala buoyancy frequency (N^2)

      IF (l_dia25h)   CALL dia_25h( kt, Kmm )      ! 25h averaging

      ! Output of surface vorticity terms
      !
      IF ( iom_use("ssplavor")    .OR. iom_use("ssrelvor")    .OR. iom_use("ssEns")    .OR.   &
         & iom_use("ssrelpotvor") .OR. iom_use("ssabspotvor") ) THEN
         !
         DO_2D( 0, 0, 0, 0 )
            z2d(ji,jj) = ff_f(ji,jj)
         END_2D
         CALL iom_put( "ssplavor", z2d )           ! planetary vorticity ( f )

         DO_2D( 0, 0, 0, 0 )
            z2d(ji,jj) = (   e2v(ji+1,jj  ) * vv(ji+1,jj  ,1,Kmm) - e2v(ji,jj) * vv(ji,jj,1,Kmm)    &
            &              - e1u(ji  ,jj+1) * uu(ji  ,jj+1,1,Kmm) + e1u(ji,jj) * uu(ji,jj,1,Kmm)  ) * r1_e1e2f(ji,jj)
         END_2D
         CALL iom_put( "ssrelvor", z2d )           ! relative vorticity ( zeta )
         !
         IF ( iom_use("ssEns") .OR. iom_use("ssrelpotvor") .OR. iom_use("ssabspotvor") ) THEN
            DO_2D( 0, 0, 0, 0 )
               ztmp = (  e3t(ji,jj+1,1,Kmm) * e1e2t(ji,jj+1) + e3t(ji+1,jj+1,1,Kmm) * e1e2t(ji+1,jj+1)    &
                  &    + e3t(ji,jj  ,1,Kmm) * e1e2t(ji,jj  ) + e3t(ji+1,jj  ,1,Kmm) * e1e2t(ji+1,jj  )  ) * r1_e1e2f(ji,jj)
               IF( ztmp /= 0._wp ) THEN   ;   ztmp = 4._wp / ztmp
               ELSE                       ;   ztmp = 0._wp
               ENDIF
               z2d(ji,jj) = ztmp * z2d(ji,jj)
            END_2D
            CALL iom_put( "ssrelpotvor", z2d )     ! relative potential vorticity (zeta/h)
            !
            IF ( iom_use("ssEns") .OR. iom_use("ssabspotvor") ) THEN
               DO_2D( 0, 0, 0, 0 )
                  ztmp = (  e3t(ji,jj+1,1,Kmm) * e1e2t(ji,jj+1) + e3t(ji+1,jj+1,1,Kmm) * e1e2t(ji+1,jj+1)    &
                     &    + e3t(ji,jj  ,1,Kmm) * e1e2t(ji,jj  ) + e3t(ji+1,jj  ,1,Kmm) * e1e2t(ji+1,jj  )  ) * r1_e1e2f(ji,jj)
                  IF( ztmp /= 0._wp ) THEN   ;   ztmp = 4._wp / ztmp
                  ELSE                       ;   ztmp = 0._wp
                  ENDIF
                  z2d(ji,jj) = ztmp * ff_f(ji,jj) + z2d(ji,jj)
               END_2D
               CALL iom_put( "ssabspotvor", z2d )  ! absolute potential vorticity ( q )
               !
               IF ( iom_use("ssEns") ) THEN
                  DO_2D( 0, 0, 0, 0 )  
                     z2d(ji,jj) = 0.5_wp * z2d(ji,jj) * z2d(ji,jj)
                  END_2D
                  CALL iom_put( "ssEns", z2d )     ! potential enstrophy ( 1/2*q2 )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF( ln_timing )   CALL timing_stop('dia_wri')
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
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kmm  ! ocean time level index
      !
      LOGICAL ::   ll_print = .FALSE.                        ! =T print and flush numout
      CHARACTER (len=40) ::   clhstnam, clop, clmx           ! local names
      INTEGER  ::   ji, jj, jk                               ! dummy loop indices
      INTEGER  ::   jn, ierror                               ! local integers
      REAL(wp) ::   zsto, zout, zjulian                ! local scalars
      REAL(wp), DIMENSION(jpi,jpj    ) :: z2d     ! 2D workspace
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: z3d     ! 3D workspace
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: z3dabl     ! 3D workspace
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

         ! Define the U grid FILE ( nid_U )

         CALL dia_nam( clhstnam, nn_write, 'grid_U' )
         IF(lwp) WRITE(numout,*) " Name of NETCDF file ", clhstnam    ! filename
         CALL histbeg( clhstnam, jpi, glamu, jpj, gphiu,           &  ! Horizontal grid: glamu and gphiu
            &          Nis0, Ni_0, Njs0, Nj_0,       &
            &          nit000-1, zjulian, rn_Dt, nh_U, nid_U, domain_id=nidom, snc4chunks=snc4set )

         ! Define the V grid FILE ( nid_V )

         CALL dia_nam( clhstnam, nn_write, 'grid_V' )                   ! filename
         IF(lwp) WRITE(numout,*) " Name of NETCDF file ", clhstnam
         CALL histbeg( clhstnam, jpi, glamv, jpj, gphiv,           &  ! Horizontal grid: glamv and gphiv
            &          Nis0, Ni_0, Njs0, Nj_0,       &
            &          nit000-1, zjulian, rn_Dt, nh_V, nid_V, domain_id=nidom, snc4chunks=snc4set )

         ! Declare all the output fields as NETCDF variables

         CALL histdef( nid_T, "sossheig", "Sea Surface Height"               , "m"        ,   &  ! ssh
            &          jpi, jpj, nh_T, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histend( nid_T, snc4chunks=snc4set )
         CALL histdef( nid_U, "souubaro", "zonal barotropic velocity"        , "m/s"      ,   &  ! ssh
            &          jpi, jpj, nh_U, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
         CALL histend( nid_U, snc4chunks=snc4set )
         CALL histdef( nid_V, "sovvbaro", "meridional barotropic velocity"   , "m/s"      ,   &  ! ssh
            &          jpi, jpj, nh_V, 1  , 1, 1  , -99 , 32, clop, zsto, zout )
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

      CALL histwrite( nid_T, "sossheig", kt,  ssh(:,:,Kmm), 1, (/-1/) )   ! sea surface height
      CALL histwrite( nid_U, "souubaro", kt, uu_b(:,:,Kmm), 1, (/-1/) )   ! zonal barotropic velocity
      CALL histwrite( nid_V, "sovvbaro", kt, vv_b(:,:,Kmm), 1, (/-1/) )   ! meridional barotropic velocity

      ! 3. Close all files
      ! ---------------------------------------
      IF( kt == nitend ) THEN
         CALL histclo( nid_T )
         CALL histclo( nid_U )
         CALL histclo( nid_V )
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
      INTEGER           , INTENT( in ) ::   Kmm              ! time level index
      CHARACTER (len=* ), INTENT( in ) ::   cdfile_name      ! name of the file created
      !!
      INTEGER ::   ji, jj, jk       ! dummy loop indices
      INTEGER ::   inum
      REAL(wp), DIMENSION(A2D(0))     :: z2d
      REAL(wp), DIMENSION(A2D(0),jpk) :: z3d
      !!----------------------------------------------------------------------
      ! 
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_wri_state : single instantaneous ocean state'
         WRITE(numout,*) '~~~~~~~~~~~~~   and forcing fields file created '
         WRITE(numout,*) '                and named :', cdfile_name, '...nc'
      ENDIF 
      !
      CALL iom_open( TRIM(cdfile_name), inum, ldwrt = .TRUE. )
      !
      CALL iom_rstput( 0, 0, inum, 'votemper', ts(:,:,:,jp_tem,Kmm) )    ! now temperature
      CALL iom_rstput( 0, 0, inum, 'vosaline', ts(:,:,:,jp_sal,Kmm) )    ! now salinity
      CALL iom_rstput( 0, 0, inum, 'sossheig', ssh(:,:,Kmm)         )    ! sea surface height
      CALL iom_rstput( 0, 0, inum, 'vozocrtx', uu(:,:,:,Kmm)        )    ! now i-velocity
      CALL iom_rstput( 0, 0, inum, 'vomecrty', vv(:,:,:,Kmm)        )    ! now j-velocity
      IF( ln_zad_Aimp ) THEN
         z3d(A2D(0),:) = ww(A2D(0),:) + wi(A2D(0),:)
         CALL iom_rstput( 0, 0, inum, 'vovecrtz', z3d            )    ! now k-velocity
      ELSE
         CALL iom_rstput( 0, 0, inum, 'vovecrtz', ww             )    ! now k-velocity
      ENDIF
      CALL iom_rstput( 0, 0, inum, 'risfdep', risfdep            )
      CALL iom_rstput( 0, 0, inum, 'ht'     , ht(:,:,Kmm)        )    ! now water column height
      !
      IF ( ln_isf ) THEN
         IF (ln_isfcav_mlt) THEN
            CALL iom_rstput( 0, 0, inum, 'fwfisf_cav', fwfisf_cav          )
            CALL iom_rstput( 0, 0, inum, 'rhisf_cav_tbl', rhisf_tbl_cav    )
            CALL iom_rstput( 0, 0, inum, 'rfrac_cav_tbl', rfrac_tbl_cav    )
            CALL iom_rstput( 0, 0, inum, 'misfkb_cav', REAL(misfkb_cav,wp) )
            CALL iom_rstput( 0, 0, inum, 'misfkt_cav', REAL(misfkt_cav,wp) )
            CALL iom_rstput( 0, 0, inum, 'mskisf_cav', REAL(mskisf_cav,wp), ktype = jp_i1 )
         END IF
         IF (ln_isfpar_mlt) THEN
            CALL iom_rstput( 0, 0, inum, 'isfmsk_par', REAL(mskisf_par,wp) )
            CALL iom_rstput( 0, 0, inum, 'fwfisf_par', fwfisf_par          )
            CALL iom_rstput( 0, 0, inum, 'rhisf_par_tbl', rhisf_tbl_par    )
            CALL iom_rstput( 0, 0, inum, 'rfrac_par_tbl', rfrac_tbl_par    )
            CALL iom_rstput( 0, 0, inum, 'misfkb_par', REAL(misfkb_par,wp) )
            CALL iom_rstput( 0, 0, inum, 'misfkt_par', REAL(misfkt_par,wp) )
            CALL iom_rstput( 0, 0, inum, 'mskisf_par', REAL(mskisf_par,wp), ktype = jp_i1 )
         END IF
      END IF
      !
      IF( ALLOCATED(ahtu) ) THEN
         CALL iom_rstput( 0, 0, inum,  'ahtu', ahtu              )    ! aht at u-point
         CALL iom_rstput( 0, 0, inum,  'ahtv', ahtv              )    ! aht at v-point
      ENDIF
      IF( ALLOCATED(ahmt) ) THEN
         CALL iom_rstput( 0, 0, inum,  'ahmt', ahmt              )    ! ahmt at u-point
         CALL iom_rstput( 0, 0, inum,  'ahmf', ahmf              )    ! ahmf at v-point
      ENDIF
      IF( ln_rnf ) THEN 
         z2d(A2D(0)) = emp(A2D(0)) - rnf(A2D(0))
         CALL iom_rstput( 0, 0, inum, 'sowaflup', z2d            )    ! freshwater budget
      ELSE
         CALL iom_rstput( 0, 0, inum, 'sowaflup', emp            )    ! freshwater budget
      ENDIF
      z2d(A2D(0)) = qsr(A2D(0)) + qns(A2D(0))
      CALL iom_rstput( 0, 0, inum, 'sohefldo', z2d               )    ! total heat flux
      CALL iom_rstput( 0, 0, inum, 'soshfldo', qsr               )    ! solar heat flux
      CALL iom_rstput( 0, 0, inum, 'soicecov', fr_i              )    ! ice fraction
      CALL iom_rstput( 0, 0, inum, 'sozotaux', utau              )    ! i-wind stress
      CALL iom_rstput( 0, 0, inum, 'sometauy', vtau              )    ! j-wind stress
      IF(  .NOT.lk_linssh  ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpk )
           z3d(ji,jj,jk) = gdept(ji,jj,jk,Kmm)   ! 3D workspace for qco substitution
         END_3D
         CALL iom_rstput( 0, 0, inum, 'vovvldep', z3d            )    !  T-cell depth
         DO_3D( 0, 0, 0, 0, 1, jpk )
           z3d(ji,jj,jk) = e3t(ji,jj,jk,Kmm)     ! 3D workspace for qco substitution
         END_3D
         CALL iom_rstput( 0, 0, inum, 'vovvle3t', z3d            )    !  T-cell thickness
      END IF
      IF( ln_wave .AND. ln_sdw ) THEN
         CALL iom_rstput( 0, 0, inum, 'sdzocrtx', usd            )    ! now StokesDrift i-velocity
         CALL iom_rstput( 0, 0, inum, 'sdmecrty', vsd            )    ! now StokesDrift j-velocity
         CALL iom_rstput( 0, 0, inum, 'sdvecrtz', wsd            )    ! now StokesDrift k-velocity
      ENDIF
      IF ( ln_abl ) THEN
         CALL iom_rstput ( 0, 0, inum, "uz1_abl",   u_abl(:,:,2,nt_a  ) )   ! now first level i-wind
         CALL iom_rstput ( 0, 0, inum, "vz1_abl",   v_abl(:,:,2,nt_a  ) )   ! now first level j-wind
         CALL iom_rstput ( 0, 0, inum, "tz1_abl",  tq_abl(:,:,2,nt_a,1) )   ! now first level temperature
         CALL iom_rstput ( 0, 0, inum, "qz1_abl",  tq_abl(:,:,2,nt_a,2) )   ! now first level humidity
      ENDIF
      IF( ln_zdfosm ) THEN
         CALL iom_rstput( 0, 0, inum, 'hbl', hbl*tmask(:,:,1)        )      ! now boundary-layer depth
         CALL iom_rstput( 0, 0, inum, 'avt_k', avt_k*wmask(A2D(0),:) )      ! w-level diffusion
         CALL iom_rstput( 0, 0, inum, 'avm_k', avm_k*wmask           )      ! now w-level viscosity
         CALL iom_rstput( 0, 0, inum, 'ghamt', ghamt*wmask           )      ! non-local t forcing
         CALL iom_rstput( 0, 0, inum, 'ghams', ghams*wmask           )      ! non-local s forcing
         CALL iom_rstput( 0, 0, inum, 'ghamu', ghamu*umask           )      ! non-local u forcing
         CALL iom_rstput( 0, 0, inum, 'ghamv', ghamv*vmask           )      ! non-local v forcing
         IF( ln_osm_mle ) THEN
            CALL iom_rstput( 0, 0, inum, 'hmle', hmle*tmask(:,:,1)  ) ! now transition-layer depth
         END IF
      ENDIF
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
