MODULE usrdef_sbc
   !!======================================================================
   !!                       ***  MODULE  usrdef_sbc  ***
   !! 
   !!                      ===  BENCH configuration  ===
   !!
   !! User defined :   surface forcing of a user configuration
   !!======================================================================
   !! History :  4.0   ! 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_sbc    : user defined surface bounday conditions in BENCH case
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean space and time domain
   USE dom_oce        
   USE oce             ! ocean dynamics and tracers
   USE sbc_oce         ! Surface boundary condition: ocean fields
   USE sbc_ice         ! Surface boundary condition: ocean fields
   USE sbc_phy, ONLY   : pp_cldf
   USE phycst          ! physical constants
#if defined key_si3
   USE par_ice, ONLY   : jpl
   USE ice, ONLY       : at_i_b, a_i_b
#endif
   USE in_out_manager  ! I/O manager
   USE lib_mpp         ! MPP library
   USE lbclnk          ! lateral boundary conditions - mpp exchanges

   IMPLICIT NONE
   PRIVATE

   PUBLIC   usrdef_sbc_oce      ! routine called by sbcmod.F90 for sbc ocean
   PUBLIC   usrdef_sbc_ice_tau  ! routine called by icestp.F90 for ice dynamics
   PUBLIC   usrdef_sbc_ice_flx  ! routine called by icestp.F90 for ice thermo

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usrdef_sbc_oce( kt, Kbb )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE usr_def_sbc  ***
      !!              
      !! ** Purpose :   provide at each time-step the surface boundary
      !!              condition, i.e. the momentum, heat and freshwater fluxes.
      !!
      !! ** Method  :   all 0 fields, for BENCH case
      !!                CAUTION : never mask the surface stress field !
      !!
      !! ** Action  : - set to ZERO all the ocean surface boundary condition, i.e.   
      !!                   utau, vtau, taum, wndm, qns, qsr, emp, sfx
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      INTEGER, INTENT(in) ::   Kbb  ! ocean time index
      !!---------------------------------------------------------------------
      !     
      IF( kt == nit000 ) THEN
         !
         IF(lwp) WRITE(numout,*)' usr_sbc : BENCH case: surface forcing'
         IF(lwp) WRITE(numout,*)' ~~~~~~~~~~~   vtau = taum = wndm = qns = qsr = emp = sfx = 0'
         !
         utau(:,:) = 0._wp
         vtau(:,:) = 0._wp
         taum(:,:) = 0._wp
         wndm(:,:) = 0._wp
         !
         emp (:,:) = 0._wp
         sfx (:,:) = 0._wp
         qns (:,:) = 0._wp
         qsr (:,:) = 0._wp
         !
         utau_b(:,:) = 0._wp 
         vtau_b(:,:) = 0._wp
         emp_b (:,:) = 0._wp
         sfx_b (:,:) = 0._wp
         qns_b (:,:) = 0._wp
         !
      ENDIF
      !
   END SUBROUTINE usrdef_sbc_oce

   
   SUBROUTINE usrdef_sbc_ice_tau( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE usrdef_sbc_ice_tau  ***
      !!
      !! ** Purpose :   provide the surface boundary (momentum) condition over
      !sea-ice
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      !
      REAL(wp) ::   zztmp
      INTEGER  ::   ji, jj
      !!---------------------------------------------------------------------
#if defined key_si3
      IF( kt==nit000 .AND. lwp)   WRITE(numout,*)' usrdef_sbc_ice : BENCH case: constant stress forcing'
      !
      ! define unique value on each point. z2d ranging from 0.05 to -0.05
      !
      DO_2D( 0, 0, 0, 0 )
         zztmp = 0.1 * ( 0.5 - REAL( mig(ji,0) + (mjg(jj,0)-1) * Ni0glo, wp ) / REAL( Ni0glo * Nj0glo, wp ) )
         utau_ice(ji,jj) = 0.1_wp + zztmp
         vtau_ice(ji,jj) = 0.1_wp + zztmp
      END_2D

      IF( l_NFold .AND. c_NFtype == 'T' ) THEN   ! force 0 at the folding points
         utau_ice(mi0(jpiglo/2+1,nn_hls):mi1(jpiglo/2+1,nn_hls),mj0(jpjglo-nn_hls,nn_hls):mj1(jpjglo-nn_hls,nn_hls)) = 0._wp
         vtau_ice(mi0(jpiglo/2+1,nn_hls):mi1(jpiglo/2+1,nn_hls),mj0(jpjglo-nn_hls,nn_hls):mj1(jpjglo-nn_hls,nn_hls)) = 0._wp
         utau_ice(mi0(  nn_hls+1,nn_hls):mi1(  nn_hls+1,nn_hls),mj0(jpjglo-nn_hls,nn_hls):mj1(jpjglo-nn_hls,nn_hls)) = 0._wp
         vtau_ice(mi0(  nn_hls+1,nn_hls):mi1(  nn_hls+1,nn_hls),mj0(jpjglo-nn_hls,nn_hls):mj1(jpjglo-nn_hls,nn_hls)) = 0._wp
      ENDIF

      CALL lbc_lnk( 'usrdef_sbc', utau_ice, 'T', -1._wp, vtau_ice, 'T', -1._wp, ldfull = .TRUE. )
#endif
      !
   END SUBROUTINE usrdef_sbc_ice_tau

   
   SUBROUTINE usrdef_sbc_ice_flx( kt, phs, phi )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE usrdef_sbc_ice_flx  ***
      !!
      !! ** Purpose :   provide the surface boundary (flux) condition over sea-ice
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phs    ! snow thickness
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phi    ! ice thickness
      !!
      INTEGER  ::   jl
      REAL(wp) ::   zfr1, zfr2                ! local variables
      REAL(wp), DIMENSION(A2D(0)) ::   zsnw   ! snw distribution after wind blowing
      REAL(wp), DIMENSION(A2D(0)) ::   ztri
      !!---------------------------------------------------------------------
#if defined key_si3
      !
      IF( kt==nit000 .AND. lwp)   WRITE(numout,*)' usrdef_sbc_ice : BENCH case: NO flux forcing'
      !
      ! ocean variables (renaming)
      emp_oce (:,:)   = 0._wp   ! uniform value for freshwater budget (E-P)
      qsr_oce (:,:)   = 0._wp   ! uniform value for     solar radiation
      qns_oce (:,:)   = 0._wp   ! uniform value for non-solar heat flux

      ! ice variables
      alb_ice (:,:,:) = 0.7_wp  ! useless
      qsr_ice (:,:,:) = 0._wp   ! uniform value for     solar radiation
      qns_ice (:,:,:) = 0._wp   ! uniform value for non-solar heat flux
      dqns_ice(:,:,:) = 0._wp   ! uniform value for non solar heat flux sensitivity for ice
      sprecip (:,:)   = 0._wp   ! uniform value for snow precip
      evap_ice(:,:,:) = 0._wp   ! uniform value for sublimation

      ! ice fields deduced from above
      zsnw(:,:) = 1._wp
      !!CALL lim_thd_snwblow( at_i_b, zsnw )  ! snow distribution over ice after wind blowing 
      emp_ice  (:,:)   = SUM( a_i_b(:,:,:) * evap_ice(:,:,:), dim=3 ) - sprecip(:,:) * zsnw(:,:)
      emp_oce  (:,:)   = emp_oce(:,:) - sprecip(:,:) * (1._wp - zsnw(:,:) )
      qevap_ice(:,:,:) =   0._wp
      qprec_ice(:,:)   =   rhos * ( sst_m(A2D(0)) * rcpi - rLfus ) * smask0(:,:) !  in J/m3
      qemp_oce (:,:)   = - emp_oce(:,:) * sst_m(A2D(0)) * rcp
      qemp_ice (:,:)   =   sprecip(:,:) * zsnw * ( sst_m(A2D(0)) * rcpi - rLfus ) * smask0(:,:) ! solid precip (only)

      ! total fluxes
      emp_tot (:,:) = emp_ice  + emp_oce
      qns_tot (:,:) = at_i_b(:,:) * qns_oce(:,:) + SUM( a_i_b(:,:,:) * qns_ice(:,:,:), dim=3 ) + qemp_ice(:,:) + qemp_oce(:,:)
      qsr_tot (:,:) = at_i_b(:,:) * qsr_oce(:,:) + SUM( a_i_b(:,:,:) * qsr_ice(:,:,:), dim=3 )

      ! --- shortwave radiation transmitted below the surface (W/m2, see Grenfell Maykut 77) --- !
      cloud_fra(:,:) = pp_cldf
      ztri(:,:) = 0.18 * ( 1.0 - cloud_fra(:,:) ) + 0.35 * cloud_fra(:,:)  ! surface transmission when hi>10cm
      !
      DO jl = 1, jpl
         WHERE    ( phs(A2D(0),jl) <= 0._wp .AND. phi(A2D(0),jl) <  0.1_wp )     ! linear decrease from hi=0 to 10cm  
            qtr_ice_top(:,:,jl) = qsr_ice(:,:,jl) * ( ztri(:,:) + ( 1._wp - ztri(:,:) ) * ( 1._wp - phi(A2D(0),jl) * 10._wp ) )
         ELSEWHERE( phs(A2D(0),jl) <= 0._wp .AND. phi(A2D(0),jl) >= 0.1_wp )     ! constant (ztri) when hi>10cm
            qtr_ice_top(:,:,jl) = qsr_ice(:,:,jl) * ztri(:,:)
         ELSEWHERE                                                         ! zero when hs>0
            qtr_ice_top(:,:,jl) = 0._wp
         END WHERE
      ENDDO
#endif

   END SUBROUTINE usrdef_sbc_ice_flx

   !!======================================================================
END MODULE usrdef_sbc
