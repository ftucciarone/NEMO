MODULE trc_oce
   !!======================================================================
   !!                      ***  MODULE  trc_oce  ***
   !! Ocean passive tracer  :  share SMS/Ocean variables
   !!======================================================================
   !! History :  1.0  !  2004-03  (C. Ethe)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   trc_oce_tab   : tabulated attenuation coefficients for RGB & 5BD light penetration
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE dom_oce        ! ocean space and time domain
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_oce_tab        ! routine called by traqsr.F90
   PUBLIC   trc_oce_ext_lev    ! function called by traqsr.F90 at least
   PUBLIC   trc_oce_alloc      ! function called by nemogcm.F90
   PUBLIC   trc_oce_dealloc    ! function called by nemogcm.F90

   LOGICAL , PUBLIC ::   l_co2cpl  = .false.   !: atmospheric pco2 recieved from oasis
   LOGICAL , PUBLIC ::   l_offline = .false.   !: offline passive tracers flag
   REAL(wp), PUBLIC ::   r_si2                 !: largest depth of extinction (blue & 0.01 mg.m-3)  (RGB & 5BD)
   LOGICAL , PUBLIC ::   ln_trcdc2dm           !: Diurnal cycle for TOP
   !
   REAL(wp), PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:,:) ::   etot3     !: light absortion coefficient
   REAL(wp), PUBLIC, SAVE, ALLOCATABLE, DIMENSION(:,:)   ::   oce_co2   !: ocean carbon flux
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qsr_mean  !: daily mean qsr

#if defined key_top 
   !!----------------------------------------------------------------------
   !!   'key_top'                                                 bio-model          
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_top     = .TRUE.   !: TOP model
#else
   !!----------------------------------------------------------------------
   !! Default option                          No bio-model light absorption      
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_top     = .FALSE.   !: TOP model
#endif

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_oce_alloc()
      !!----------------------------------------------------------------------
      !!                  ***  trc_oce_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( etot3(A2D(0),jpk), oce_co2(A2D(0)), qsr_mean(A2D(0)), STAT=trc_oce_alloc )

      IF( trc_oce_alloc /= 0 )   CALL ctl_warn('trc_oce_alloc: failed to allocate etot3 array')
      !
   END FUNCTION trc_oce_alloc


   SUBROUTINE trc_oce_dealloc()
      IF (ALLOCATED(etot3) )   DEALLOCATE( etot3, oce_co2, qsr_mean )
   END SUBROUTINE trc_oce_dealloc
   

   SUBROUTINE trc_oce_tab( ptab )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_opt_init  ***
      !!
      !! ** Purpose :   Initialization of of the optical scheme
      !!
      !! ** Method  :   Set a look up table for the optical coefficients
      !!                i.e. the attenuation coefficient for R-G-B light 
      !!                tabulated in Chlorophyll class (from JM Andre)
      !!
      !! ** Action  :   ptab(4,61) tabulated R-G-B-UV attenuation coef. 
      !!
      !! Reference  : Lengaigne et al. 2007, Clim. Dyn., V28, 5, 503-516.
      !!              Morel et Maritorena 2001,  J. Geophys. Res., 106(C4):7163-7180
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(4,61), INTENT(out) ::   ptab   ! tabulated attenuation coefficient
      !
      INTEGER  ::   jc     ! dummy loop indice
      INTEGER  ::   itab   ! temporary integer
      REAL(wp) ::   zchl   ! temporary scalar
      REAL(wp), DIMENSION(5,61) ::   ztab   ! tabulated attenuation coefficient (formerly read in 'kRGB61.txt')
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '   trc_oce_tab : Initialisation of the optical look-up table'
         WRITE(numout,*) '   ~~~~~~~~~~~ '
      ENDIF
      !
      !  Chlorophyll        !     Blue attenuation     !     Green attenuation    !     Red attenuation      !     UV attenuation
      ztab(1, 1) =  0.010   ;   ztab(2, 1) = 0.01618   ;   ztab(3, 1) = 0.07464   ;   ztab(4, 1) = 0.37807   ;   ztab(5, 1) = 0.03135
      ztab(1, 2) =  0.011   ;   ztab(2, 2) = 0.01654   ;   ztab(3, 2) = 0.07480   ;   ztab(4, 2) = 0.37823   ;   ztab(5, 2) = 0.03168
      ztab(1, 3) =  0.013   ;   ztab(2, 3) = 0.01693   ;   ztab(3, 3) = 0.07499   ;   ztab(4, 3) = 0.37840   ;   ztab(5, 3) = 0.03231
      ztab(1, 4) =  0.014   ;   ztab(2, 4) = 0.01736   ;   ztab(3, 4) = 0.07518   ;   ztab(4, 4) = 0.37859   ;   ztab(5, 4) = 0.03262
      ztab(1, 5) =  0.016   ;   ztab(2, 5) = 0.01782   ;   ztab(3, 5) = 0.07539   ;   ztab(4, 5) = 0.37879   ;   ztab(5, 5) = 0.03323
      ztab(1, 6) =  0.018   ;   ztab(2, 6) = 0.01831   ;   ztab(3, 6) = 0.07562   ;   ztab(4, 6) = 0.37900   ;   ztab(5, 6) = 0.03381
      ztab(1, 7) =  0.020   ;   ztab(2, 7) = 0.01885   ;   ztab(3, 7) = 0.07586   ;   ztab(4, 7) = 0.37923   ;   ztab(5, 7) = 0.03439
      ztab(1, 8) =  0.022   ;   ztab(2, 8) = 0.01943   ;   ztab(3, 8) = 0.07613   ;   ztab(4, 8) = 0.37948   ;   ztab(5, 8) = 0.03495
      ztab(1, 9) =  0.025   ;   ztab(2, 9) = 0.02005   ;   ztab(3, 9) = 0.07641   ;   ztab(4, 9) = 0.37976   ;   ztab(5, 9) = 0.03577
      ztab(1,10) =  0.028   ;   ztab(2,10) = 0.02073   ;   ztab(3,10) = 0.07672   ;   ztab(4,10) = 0.38005   ;   ztab(5,10) = 0.03657
      ztab(1,11) =  0.032   ;   ztab(2,11) = 0.02146   ;   ztab(3,11) = 0.07705   ;   ztab(4,11) = 0.38036   ;   ztab(5,11) = 0.03761
      ztab(1,12) =  0.035   ;   ztab(2,12) = 0.02224   ;   ztab(3,12) = 0.07741   ;   ztab(4,12) = 0.38070   ;   ztab(5,12) = 0.03837
      ztab(1,13) =  0.040   ;   ztab(2,13) = 0.02310   ;   ztab(3,13) = 0.07780   ;   ztab(4,13) = 0.38107   ;   ztab(5,13) = 0.03960
      ztab(1,14) =  0.045   ;   ztab(2,14) = 0.02402   ;   ztab(3,14) = 0.07821   ;   ztab(4,14) = 0.38146   ;   ztab(5,14) = 0.04080
      ztab(1,15) =  0.050   ;   ztab(2,15) = 0.02501   ;   ztab(3,15) = 0.07866   ;   ztab(4,15) = 0.38189   ;   ztab(5,15) = 0.04197
      ztab(1,16) =  0.056   ;   ztab(2,16) = 0.02608   ;   ztab(3,16) = 0.07914   ;   ztab(4,16) = 0.38235   ;   ztab(5,16) = 0.04334
      ztab(1,17) =  0.063   ;   ztab(2,17) = 0.02724   ;   ztab(3,17) = 0.07967   ;   ztab(4,17) = 0.38285   ;   ztab(5,17) = 0.04490
      ztab(1,18) =  0.071   ;   ztab(2,18) = 0.02849   ;   ztab(3,18) = 0.08023   ;   ztab(4,18) = 0.38338   ;   ztab(5,18) = 0.04664
      ztab(1,19) =  0.079   ;   ztab(2,19) = 0.02984   ;   ztab(3,19) = 0.08083   ;   ztab(4,19) = 0.38396   ;   ztab(5,19) = 0.04833
      ztab(1,20) =  0.089   ;   ztab(2,20) = 0.03131   ;   ztab(3,20) = 0.08149   ;   ztab(4,20) = 0.38458   ;   ztab(5,20) = 0.05039
      ztab(1,21) =  0.100   ;   ztab(2,21) = 0.03288   ;   ztab(3,21) = 0.08219   ;   ztab(4,21) = 0.38526   ;   ztab(5,21) = 0.05260
      ztab(1,22) =  0.112   ;   ztab(2,22) = 0.03459   ;   ztab(3,22) = 0.08295   ;   ztab(4,22) = 0.38598   ;   ztab(5,22) = 0.05496
      ztab(1,23) =  0.126   ;   ztab(2,23) = 0.03643   ;   ztab(3,23) = 0.08377   ;   ztab(4,23) = 0.38676   ;   ztab(5,23) = 0.05763
      ztab(1,24) =  0.141   ;   ztab(2,24) = 0.03842   ;   ztab(3,24) = 0.08466   ;   ztab(4,24) = 0.38761   ;   ztab(5,24) = 0.06042
      ztab(1,25) =  0.158   ;   ztab(2,25) = 0.04057   ;   ztab(3,25) = 0.08561   ;   ztab(4,25) = 0.38852   ;   ztab(5,25) = 0.06351
      ztab(1,26) =  0.178   ;   ztab(2,26) = 0.04289   ;   ztab(3,26) = 0.08664   ;   ztab(4,26) = 0.38950   ;   ztab(5,26) = 0.06705
      ztab(1,27) =  0.200   ;   ztab(2,27) = 0.04540   ;   ztab(3,27) = 0.08775   ;   ztab(4,27) = 0.39056   ;   ztab(5,27) = 0.07084
      ztab(1,28) =  0.224   ;   ztab(2,28) = 0.04811   ;   ztab(3,28) = 0.08894   ;   ztab(4,28) = 0.39171   ;   ztab(5,28) = 0.07487
      ztab(1,29) =  0.251   ;   ztab(2,29) = 0.05103   ;   ztab(3,29) = 0.09023   ;   ztab(4,29) = 0.39294   ;   ztab(5,29) = 0.07929
      ztab(1,30) =  0.282   ;   ztab(2,30) = 0.05420   ;   ztab(3,30) = 0.09162   ;   ztab(4,30) = 0.39428   ;   ztab(5,30) = 0.08424
      ztab(1,31) =  0.316   ;   ztab(2,31) = 0.05761   ;   ztab(3,31) = 0.09312   ;   ztab(4,31) = 0.39572   ;   ztab(5,31) = 0.08953
      ztab(1,32) =  0.355   ;   ztab(2,32) = 0.06130   ;   ztab(3,32) = 0.09474   ;   ztab(4,32) = 0.39727   ;   ztab(5,32) = 0.09545
      ztab(1,33) =  0.398   ;   ztab(2,33) = 0.06529   ;   ztab(3,33) = 0.09649   ;   ztab(4,33) = 0.39894   ;   ztab(5,33) = 0.10181
      ztab(1,34) =  0.447   ;   ztab(2,34) = 0.06959   ;   ztab(3,34) = 0.09837   ;   ztab(4,34) = 0.40075   ;   ztab(5,34) = 0.10887
      ztab(1,35) =  0.501   ;   ztab(2,35) = 0.07424   ;   ztab(3,35) = 0.10040   ;   ztab(4,35) = 0.40270   ;   ztab(5,35) = 0.11646
      ztab(1,36) =  0.562   ;   ztab(2,36) = 0.07927   ;   ztab(3,36) = 0.10259   ;   ztab(4,36) = 0.40480   ;   ztab(5,36) = 0.12482
      ztab(1,37) =  0.631   ;   ztab(2,37) = 0.08470   ;   ztab(3,37) = 0.10495   ;   ztab(4,37) = 0.40707   ;   ztab(5,37) = 0.13403
      ztab(1,38) =  0.708   ;   ztab(2,38) = 0.09056   ;   ztab(3,38) = 0.10749   ;   ztab(4,38) = 0.40952   ;   ztab(5,38) = 0.14405
      ztab(1,39) =  0.794   ;   ztab(2,39) = 0.09690   ;   ztab(3,39) = 0.11024   ;   ztab(4,39) = 0.41216   ;   ztab(5,39) = 0.15496
      ztab(1,40) =  0.891   ;   ztab(2,40) = 0.10374   ;   ztab(3,40) = 0.11320   ;   ztab(4,40) = 0.41502   ;   ztab(5,40) = 0.16696
      ztab(1,41) =  1.000   ;   ztab(2,41) = 0.11114   ;   ztab(3,41) = 0.11639   ;   ztab(4,41) = 0.41809   ;   ztab(5,41) = 0.18009
      ztab(1,42) =  1.122   ;   ztab(2,42) = 0.11912   ;   ztab(3,42) = 0.11984   ;   ztab(4,42) = 0.42142   ;   ztab(5,42) = 0.19443
      ztab(1,43) =  1.259   ;   ztab(2,43) = 0.12775   ;   ztab(3,43) = 0.12356   ;   ztab(4,43) = 0.42500   ;   ztab(5,43) = 0.21012
      ztab(1,44) =  1.413   ;   ztab(2,44) = 0.13707   ;   ztab(3,44) = 0.12757   ;   ztab(4,44) = 0.42887   ;   ztab(5,44) = 0.22731
      ztab(1,45) =  1.585   ;   ztab(2,45) = 0.14715   ;   ztab(3,45) = 0.13189   ;   ztab(4,45) = 0.43304   ;   ztab(5,45) = 0.24603
      ztab(1,46) =  1.778   ;   ztab(2,46) = 0.15803   ;   ztab(3,46) = 0.13655   ;   ztab(4,46) = 0.43754   ;   ztab(5,46) = 0.26650
      ztab(1,47) =  1.995   ;   ztab(2,47) = 0.16978   ;   ztab(3,47) = 0.14158   ;   ztab(4,47) = 0.44240   ;   ztab(5,47) = 0.28894
      ztab(1,48) =  2.239   ;   ztab(2,48) = 0.18248   ;   ztab(3,48) = 0.14701   ;   ztab(4,48) = 0.44765   ;   ztab(5,48) = 0.31353
      ztab(1,49) =  2.512   ;   ztab(2,49) = 0.19620   ;   ztab(3,49) = 0.15286   ;   ztab(4,49) = 0.45331   ;   ztab(5,49) = 0.34036
      ztab(1,50) =  2.818   ;   ztab(2,50) = 0.21102   ;   ztab(3,50) = 0.15918   ;   ztab(4,50) = 0.45942   ;   ztab(5,50) = 0.36966
      ztab(1,51) =  3.162   ;   ztab(2,51) = 0.22703   ;   ztab(3,51) = 0.16599   ;   ztab(4,51) = 0.46601   ;   ztab(5,51) = 0.40178
      ztab(1,52) =  3.548   ;   ztab(2,52) = 0.24433   ;   ztab(3,52) = 0.17334   ;   ztab(4,52) = 0.47313   ;   ztab(5,52) = 0.43690
      ztab(1,53) =  3.981   ;   ztab(2,53) = 0.26301   ;   ztab(3,53) = 0.18126   ;   ztab(4,53) = 0.48080   ;   ztab(5,53) = 0.47531
      ztab(1,54) =  4.467   ;   ztab(2,54) = 0.28320   ;   ztab(3,54) = 0.18981   ;   ztab(4,54) = 0.48909   ;   ztab(5,54) = 0.51733
      ztab(1,55) =  5.012   ;   ztab(2,55) = 0.30502   ;   ztab(3,55) = 0.19903   ;   ztab(4,55) = 0.49803   ;   ztab(5,55) = 0.56326
      ztab(1,56) =  5.623   ;   ztab(2,56) = 0.32858   ;   ztab(3,56) = 0.20898   ;   ztab(4,56) = 0.50768   ;   ztab(5,56) = 0.61346
      ztab(1,57) =  6.310   ;   ztab(2,57) = 0.35404   ;   ztab(3,57) = 0.21971   ;   ztab(4,57) = 0.51810   ;   ztab(5,57) = 0.66847
      ztab(1,58) =  7.079   ;   ztab(2,58) = 0.38154   ;   ztab(3,58) = 0.23129   ;   ztab(4,58) = 0.52934   ;   ztab(5,58) = 0.72850
      ztab(1,59) =  7.943   ;   ztab(2,59) = 0.41125   ;   ztab(3,59) = 0.24378   ;   ztab(4,59) = 0.54147   ;   ztab(5,59) = 0.79424
      ztab(1,60) =  8.912   ;   ztab(2,60) = 0.44336   ;   ztab(3,60) = 0.25725   ;   ztab(4,60) = 0.55457   ;   ztab(5,60) = 0.86611
      ztab(1,61) = 10.000   ;   ztab(2,61) = 0.47804   ;   ztab(3,61) = 0.27178   ;   ztab(4,61) = 0.56870   ;   ztab(5,61) = 0.94478
      !
      ptab(:,:) = ztab(2:5,:)
      !
      r_si2 = 1.e0 / ztab(2, 1)        ! blue with the smallest chlorophyll concentration)
      IF(lwp) WRITE(numout,*) '      RGB longest depth of extinction    r_si2 = ', r_si2
      !
      DO jc = 1, 61                         ! check
         zchl = ztab(1,jc)
         itab = NINT( 41 + 20.* LOG10( zchl ) + 1.e-15 )
         IF( itab /= jc ) THEN
            IF(lwp) WRITE(numout,*) '    jc =', jc, '  Chl = ', zchl, '  Chl class = ', itab
            CALL ctl_stop( 'trc_oce_tab : inconsistency in Chl tabulated attenuation coeff.' )
         ENDIF
      END DO
      !
   END SUBROUTINE trc_oce_tab


   FUNCTION trc_oce_ext_lev( prldex, pqsr_frc ) RESULT( pjl )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE trc_oce_ext_lev  ***
      !!       
      !! ** Purpose :   compute max. level for light penetration
      !!          
      !! ** Method  :   the function provides the level at which irradiance 
      !!                becomes negligible (i.e. = 1.e-15 W/m2) for 3 or 2 bands light
      !!                penetration: I(z) = pqsr_frc * EXP(hext/prldex) = 1.e-15 W/m2
      !!                # prldex is the longest depth of extinction:
      !!                   - prldex = 23 m (2 bands case)
      !!                   - prldex = 62 m (3 bands case: blue waveband & 0.01 mg/m2 for the chlorophyll)
      !!                # pqsr_frc is the fraction of solar radiation which penetrates,
      !!                considering Qsr=240 W/m2 and rn_abs = 0.58:
      !!                   - pqsr_frc = Qsr * (1-rn_abs)   = 1.00e2 W/m2 (2 bands case)
      !!                   - pqsr_frc = Qsr * (1-rn_abs)/3 = 0.33e2 W/m2 (3 bands case & equi-partition)
      !!
      !!----------------------------------------------------------------------
      REAL(wp), INTENT(in) ::   prldex    ! longest depth of extinction
      REAL(wp), INTENT(in) ::   pqsr_frc  ! frac. solar radiation which penetrates 
      !
      INTEGER  ::   jk, pjl            ! levels
      REAL(wp) ::   zhext              ! deepest level till which light penetrates
      REAL(wp) ::   zprec = 15._wp     ! precision to reach -LOG10(1.e-15)
      REAL(wp) ::   zem                ! temporary scalar 
      !!----------------------------------------------------------------------
      !
      ! It is not necessary to compute anything below the following depth
      zhext = prldex * ( LOG(10._wp) * zprec + LOG(pqsr_frc) )
      !
      ! Level of light extinction
      pjl = jpkm1
      DO jk = jpkm1, 1, -1
         IF(SUM(tmask(:,:,jk)) > 0 ) THEN
#if defined key_vco_3d
            zem = MAXVAL( gdepw_3d(:,:,jk+1) * tmask(:,:,jk) )
#else
            zem = MAXVAL( gdepw_1d(jk+1) * tmask(:,:,jk) )
#endif
            IF( zem >= zhext )   pjl = jk                       ! last T-level reached by Qsr
         ELSE
            pjl = jk                                            ! or regional sea-bed depth 
         ENDIF
      END DO
      !
   END FUNCTION trc_oce_ext_lev

   !!======================================================================
END MODULE trc_oce
