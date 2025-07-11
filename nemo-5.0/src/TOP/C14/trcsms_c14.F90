MODULE trcsms_c14
   !!======================================================================
   !!                      ***  MODULE trcsms_c14  ***
   !! TOP : Bomb C14 main module
   !!======================================================================
   !! History     -   ! 1994-05 ( J. Orr ) original code
   !!            1.0  ! 2006-02 ( J.M. Molines )  Free form + modularity
   !!            2.0  ! 2008-12 ( C. Ethe ) reorganisation
   !!            4.0  ! 2011-02 ( A.R. Porter, STFC Daresbury ) Dynamic memory
   !!                 ! 2015    (A. Mouchet) general C14 + update formulas
   !!----------------------------------------------------------------------
   !!   trc_sms_c14 :  compute and add C14 suface forcing to C14 trends
   !!----------------------------------------------------------------------
   USE oce_trc       ! Ocean variables
   USE par_trc       ! TOP parameters
   USE trc           ! TOP variables
   USE trd_oce    ! trends
   USE trdtrc    ! trends
   USE sms_c14    ! atmospheric forcing
   USE trcatm_c14    ! atmospheric forcing
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms_c14       ! called in trcsms.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sms_c14( kt, Kbb, Kmm, Krhs )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_sms_c14  ***
      !!
      !! ** Purpose :   Compute the surface boundary contition on C14
      !!      passive tracer associated with air-sea fluxes and add it to 
      !!      the general trend of tracers equations.
      !
      !    Method: 
      !          - transport the ratio C14/C as in Toggweiler et al. (JGR,1989)
      !          - if on-line a passive tracer (jpcref; NO sms) allows compensating for
      !            freshwater fluxes which should not impact the C14/C ratio
      !
      !        =>   Delta-C14= ( tr(...jp_c14,Kmm) -1)*1000.
      !!
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt               ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs   ! ocean time level
      !
      INTEGER  :: ji, jj, jk, itt   ! dummy loop indices 
      REAL(wp) :: zt, ztp, zsk      ! dummy variables
      REAL(wp) :: zsol              ! solubility
      REAL(wp) :: zsch              ! schmidt number
      REAL(wp) :: zv2               ! wind speed ( square)
      REAL(wp) :: zpv               ! piston velocity
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sms_c14')
      !
      IF( kt ==  nittrc000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' trc_sms_c14:  C14 model'
         IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
      ENDIF
      !
      ! Get co2sbc & c14sbc(ji,jj): at 1st iter for all, at each time step for transient
      IF( kc14typ >= 1 .OR.  kt ==  nittrc000 )   CALL trc_atm_c14( kt, co2sbc, c14sbc ) 

      ! -------------------------------------------------------------------
      !  Gas exchange coefficient (Wanninkhof, 1992, JGR, 97,7373-7382)
      !  Schmidt number of CO2 in seawater (Wanninkhof, 1992 & 2014)
      !  CO2 solubility (Weiss, 1974; Wanninkhof, 2014) 
      ! -------------------------------------------------------------------
#if defined key_RK3
      ! Don't consider mid-step values if online coupling
      ! because these are possibly non-monotonic (even with FCT): 
      IF ( l_offline ) THEN ; itt = Kmm ; ELSE ; itt = Kbb ; ENDIF
#else
      itt = Kmm
#endif
      DO_2D( 0, 0, 0, 0 )
         IF( tmask(ji,jj,1) >  0. ) THEN
            !
            zt   = MIN( 40. , ts(ji,jj,1,jp_tem,itt) )
            !
            !  Computation of solubility zsol in [mol/(L * atm)]
            !   after Wanninkhof (2014) referencing Weiss (1974)
            ztp  = ( zt + 273.16 ) * 0.01
            zsk  = 0.027766 + ztp * ( -0.025888 + 0.0050578 * ztp )   ! [mol/(L * atm)]
            zsol = EXP( -58.0931 + 90.5069 / ztp  + 22.2940 * LOG( ztp ) + zsk * ts(ji,jj,1,jp_sal,itt) )
            ! convert solubilities [mol/(L * atm)] -> [mol/(m^3 * ppm)]
            zsol = zsol * 1.e-03

            ! Computes the Schmidt number of CO2 in seawater
            !               Wanninkhof-2014
            zsch = 2116.8 + zt * ( -136.25 + zt * (4.7353 + zt * (-0.092307 + 0.0007555 * zt ) ) )

            ! Wanninkhof Piston velocity: zpv in units [m/s]
            zv2 = xkwind * (wndm(ji,jj) * wndm(ji,jj))              ! wind speed module at T points
            ! chemical enhancement (Wanninkhof & Knox, 1996)
            IF( ln_chemh ) zv2 = zv2 + 2.5 * ( 0.5246 + zt * (0.016256 + 0.00049946  * zt ) )
            zv2 = zv2/360000._wp                                    ! conversion cm/h -> m/s
            !
            zpv  = ( zv2 * SQRT( 660./ zsch ) ) * ( 1. - fr_i(ji,jj) ) * tmask(ji,jj,1)

            ! CO2 piston velocity (m/s)
            exch_co2(ji,jj)= zpv
            ! CO2 invasion rate (mol/ppm/m2/s) = 1st part of 14C/C exchange velocity
            exch_c14(ji,jj)= zpv * zsol
         ELSE
            exch_co2(ji,jj) = 0._wp
            exch_c14(ji,jj) = 0._wp
         ENDIF
      END_2D

      ! Exchange velocity for 14C/C ratio (m/s)
      zt = co2sbc / xdicsur
      exch_c14(:,:) = zt * exch_c14(:,:)
      !
      ! Flux of C-14 from air-to-sea; units: (C14/C ratio) x m/s
      !                               already masked
      DO_2D( 0, 0, 0, 0 )
         qtr_c14(ji,jj) = exch_c14(ji,jj) * ( c14sbc(ji,jj) - tr(ji,jj,1,jp_c14,Kbb) )
      END_2D
            
      ! cumulation of air-to-sea flux at each time step
      qint_c14(:,:) = qint_c14(:,:) + qtr_c14(:,:) * rn_Dt
      !
      ! Add the surface flux to the trend of jp_c14
      DO_2D( 0, 0, 0, 0 )
         tr(ji,jj,1,jp_c14,Krhs) = tr(ji,jj,1,jp_c14,Krhs) + qtr_c14(ji,jj) / e3t(ji,jj,1,Kmm) 
      END_2D
      !
      ! Computation of decay effects on jp_c14
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         tr(ji,jj,jk,jp_c14,Krhs) = tr(ji,jj,jk,jp_c14,Krhs) - rlam14 * tr(ji,jj,jk,jp_c14,Kbb) * tmask(ji,jj,jk) 
      END_3D
      !
      IF( lrst_trc ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' trc_rst_wri_c14 : Write specific variables from c14 model '
         IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
         !
         CALL iom_rstput( kt, nitrst, numrtw, 'co2sbc', co2sbc ) ! These five need      &
         CALL iom_rstput( kt, nitrst, numrtw, 'c14sbc', c14sbc ) ! &    to be written   &
         CALL iom_rstput( kt, nitrst, numrtw, 'exch_co2', exch_co2 ) ! &    for temporal    &
         CALL iom_rstput( kt, nitrst, numrtw, 'exch_c14', exch_c14 ) ! &    averages        &
         CALL iom_rstput( kt, nitrst, numrtw, 'qtr_c14', qtr_c14   ) ! &    to be coherent.
         CALL iom_rstput( kt, nitrst, numrtw, 'qint_c14', qint_c14 ) ! Cumulative
         !
      ENDIF

      IF( l_trdtrc )  CALL trd_trc( tr(:,:,:,jp_c14,Krhs), 1, jptra_sms, kt, Kmm )   ! save trends
      !
      IF( ln_timing )   CALL timing_stop('trc_sms_c14')
      !
   END SUBROUTINE trc_sms_c14

  !!======================================================================
END MODULE trcsms_c14
