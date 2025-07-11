MODULE trcais
   !!======================================================================
   !!                         ***  MODULE trcais  ***
   !!  Module for passive tracers in Antarctic ice sheet
   !!  delivered by iceberg and ice shelf freshwater fluxes
   !!======================================================================
   !! History :  1.0  ! 2020    (R. Person, O. Aumont, C. Ethe),
   !!======================================================================
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP model
   !!----------------------------------------------------------------------
   !!   trc_ais       : external source of tracers from Antarctic ice sheet
   !!----------------------------------------------------------------------
   USE par_trc        !  passive tracers parameters
   USE oce_trc        !  shared variables between ocean and passive tracers
   USE trc            !  passive tracers common variables
   USE iom            !  I/O manager
   USE lib_mpp        !  MPP library
   USE sbc_oce        !
   USE isf_oce        ! ice shelf melting contribution
   USE sbcrnf         ! iceberg freshwater flux
   USE trcnam         ! Namelist read
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ais         ! called in trcstp.F90 or within TOP modules
   PUBLIC   trc_ais_ini     ! called in trcini.F90

   INTEGER  , SAVE, PUBLIC                              :: nb_trcais    ! number of tracers in AIS
   REAL(wp) , SAVE, PUBLIC                              :: rn_icbdep    ! mean underwater depth of iceberg (in meters)
   INTEGER  , SAVE, PUBLIC                              :: icblev       ! mean underwater depth of iceberg (in level depth)
   INTEGER  , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)   :: n_trc_indais ! index of tracer with AIS freswater flux
   REAL(wp) , SAVE, PUBLIC, ALLOCATABLE, DIMENSION(:)   :: rf_trafac    ! multiplicative factor for AIS tracer values

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_ais_ini
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ais_ini ***
      !!
      !! ** Purpose :   Initialization of passive tracers from the Antartic
      !!                Ice Sheet delivered by iceberg and ice shelf
      !!                freshwater flux
      !!
      !! ** Method  : - Read namtsd namelist
      !!              
      !!---------------------------------------------------------------------
      INTEGER            :: jl, jn, jk                     ! dummy loop indices
      INTEGER            :: ierr0, ierr1, ierr2, ierr3     ! temporary integers
      INTEGER            :: ios                            ! Local integer output status for namelist read
      REAL(wp)   , DIMENSION(jpmaxtrc) :: rn_trafac    ! multiplicative factor for tracer values
      !!
      NAMELIST/namtrc_ais/ nn_ais_tr, rn_trafac, rn_icbdep
      !!----------------------------------------------------------------------
      !
      IF( lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trc_ais_ini : Antarctic ice sheet tracer initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      ! 
      IF( .NOT. ln_rnf_icb ) THEN
         CALL ctl_stop( 'trc_ais_ini: no iceberg freswater flux in runoff file' )   ;  RETURN
      ENDIF
      !
      ! Compute the number of tracers to be initialised in iceberg and ice
      ! shelf freshwater flux
      ALLOCATE( n_trc_indais(jptra), STAT=ierr0 )
      IF( ierr0 > 0 ) THEN
         CALL ctl_stop( 'trc_ais_ini: unable to allocate n_trc_indais' )   ;  RETURN
      ENDIF
      nb_trcais       = 0
      n_trc_indais(:) = 0
      !
      ! Read Antarctic Ice Sheet Namelist
      READ_NML_REF(numnat,namtrc_ais)
      READ_NML_CFG(numnat,namtrc_ais)
      IF(lwm) WRITE ( numont, namtrc_ais )
      ! 
      IF( lwp ) THEN
         WRITE(numout,*) ' '
         WRITE(numout,*) '   Namelist : namtrc_ais'
         WRITE(numout,*) '   Antarctic Ice Sheet tracers option (nn_ais_tr) : ', nn_ais_tr
      ENDIF
      ! compose AIS data indexes
      DO jn = 1, jptra
         IF( ln_trc_ais(jn) ) THEN
             nb_trcais       = nb_trcais + 1   ;   n_trc_indais(jn) = nb_trcais
         ENDIF
      END DO

      ! Print summmary of Antarctic Ice Sheet tracers supply
      IF( lwp ) THEN
         WRITE(numout,*)
         WRITE(numout,'(a,i3)') '   Total tracers to be initialized with AIS freshwater fluxes:', nb_trcais
      ENDIF
      !
      IF( nb_trcais > 0 ) THEN
         ALLOCATE( rf_trafac(nb_trcais), STAT=ierr0 )
         DO jn = 1, jptra
            IF( ln_trc_ais(jn) ) THEN
                jl = n_trc_indais(jn)
                rf_trafac(jl) = rn_trafac(jn)
                IF(lwp) WRITE(numout, 9001) jn, ctrcnm(jn), 'AIS', rn_trafac(jn), rf_trafac(jl)
            ENDIF
         END DO
      ENDIF
9001  FORMAT(2x,i5, 8x, a15, 3x, a3, 5x, e11.3, 5x, e11.3)

      ! 
      icblev = 1        !  compute last level where depth less than rn_icbdep (120 m)
         DO jk = jpkm1, 1, -1
            IF( gdept_1d(jk) > rn_icbdep )   icblev = jk - 1
         END DO
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' Level corresponding to iceberg depth ',  icblev,' ', gdept_1d(icblev+1)
      !
   END SUBROUTINE trc_ais_ini

   SUBROUTINE trc_ais(kt, Kbb, Kmm, ptr, Krhs)
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_ais  ***
      !!
      !! ** Purpose :  Apply Antarctic Ice Sheet inputs to tracers
      !!
      !! ** Method  :  Read freswater flux from iceberg and ice shelf 
      !!               and update data 
      !!                
      !!----------------------------------------------------------------------
      !!
      INTEGER                                   , INTENT(in)           ::   kt ! ocean time-step index
      INTEGER                                   , INTENT(in)           ::   Kbb, Kmm, Krhs ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout)        ::   ptr ! passive tracers and RHS of tracer equation
      !!
      INTEGER  :: ji, jj, jk, jn, jl             ! Loop index
      INTEGER  :: ikt, ikb  ! top and bottom level of the tbl
      CHARACTER (len=22) :: charout
      REAL(wp) :: zfact, zcalv, zfrac, ztra
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_ais')

      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN ! Do only for the first tile
         IF( kt == nit000 .AND. lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'trc_ais : passive tracers from Antarctic Ice Sheet'
            WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF


      ! 0. initialization
      SELECT CASE ( nn_ais_tr )

      CASE ( 0 ) ! No tracers in Antarctic Ice Sheet (null concentration in AIS)
         !
         ! Iceberg freshwater dilution for tracers with absent iceberg load
         IF( ln_rnf_icb ) THEN
            DO jn = 1, jptra
               IF( ln_trc_ais(jn) ) THEN
                  jl = n_trc_indais(jn)
                  DO_2D( 0, 0, 0, 0 )
                     zfact = 1. / e3t(ji,jj,1,Kmm)
                     ztra = fwficb(ji,jj) * r1_rho0 * ptr(ji,jj,1,jn,Kbb) * zfact
                     ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,1,jn,Krhs) + ztra  * rDt_trc
                  END_2D
               END IF
            END DO   
         END IF   
         ! Ice shelf freshwater dilution for tracers with absent ice shelf load
         IF( ln_isf ) THEN
            DO jn = 1, jptra
               IF( ln_trc_ais(jn) ) THEN
                  jl = n_trc_indais(jn)
                  DO_2D( 0, 0, 0, 0 )
                     IF( ln_isfpar_mlt ) THEN
                        zcalv = fwfisf_par(ji,jj) * r1_rho0 / rhisf_tbl_par(ji,jj)
                        ikt = misfkt_par(ji,jj)
                        ikb = misfkb_par(ji,jj)
                        zfrac = rfrac_tbl_par(ji,jj)
                     END IF   
                     IF( ln_isfcav_mlt ) THEN
                        zcalv = fwfisf_cav(ji,jj) * r1_rho0 / rhisf_tbl_cav(ji,jj)
                        ikt = misfkt_cav(ji,jj)
                        ikb = misfkb_cav(ji,jj)
                        zfrac = rfrac_tbl_cav(ji,jj)
                     END IF   
                     ! level fully include in the ice shelf boundary layer
                     DO jk = ikt, ikb - 1
                        ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,jk,jn,Krhs) + zcalv * ptr(ji,jj,jk,jn,Kbb) * rDt_trc
                     END DO
                     ! level partially include in ice shelf boundary layer
                     ptr(ji,jj,ikb,jn,Krhs) = ptr(ji,jj,ikb,jn,Krhs) +  zcalv * ptr(ji,jj,ikb,jn,Kbb) * zfrac * rDt_trc
                  END_2D
               ENDIF   
            END DO
         END IF
         !
      CASE ( 1 )  ! Specific treatment  with an imposed concentration in AIS
         !
         ! source of bgc tracers from iceberg in Southern Ocean
         ! distributed along the water column until 120 m depth (Person et al., 2019)
         IF( ln_rnf_icb ) THEN
            DO jn = 1, jptra
               IF( ln_trc_ais(jn) ) THEN
                  jl = n_trc_indais(jn)
                  DO_2D( 0, 0, 0, 0 )
                     DO jk = 1, icblev
                        zcalv  =  fwficb(ji,jj) * r1_rho0 
                        ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,jk,jn,Krhs) + rf_trafac(jl) * zcalv / gdepw(ji,jj,icblev+1,Kmm) * rDt_trc
                     END DO   
                  END_2D
                END IF  
            END DO   
         END IF   
         ! source of bgc tracers from ice shelf in the Southern Ocean 
         ! with tbl treated as in Mathiot et al. (2017)
         IF( ln_isf ) THEN
            DO jn = 1, jptra
               IF( ln_trc_ais(jn) ) THEN
                  jl = n_trc_indais(jn)
                  DO_2D( 0, 0, 0, 0 )
                     IF( ln_isfpar_mlt ) THEN
                        zcalv = - fwfisf_par(ji,jj) * r1_rho0 / rhisf_tbl_par(ji,jj)
                        ikt = misfkt_par(ji,jj)
                        ikb = misfkb_par(ji,jj)
                        zfrac = rfrac_tbl_par(ji,jj)
                     END IF
                     IF( ln_isfcav_mlt ) THEN
                        zcalv = - fwfisf_cav(ji,jj) * r1_rho0 / rhisf_tbl_cav(ji,jj)
                        ikt = misfkt_cav(ji,jj)
                        ikb = misfkb_cav(ji,jj)
                        zfrac = rfrac_tbl_cav(ji,jj)
                     END IF
                     ! level fully include in the ice shelf boundary layer
                     DO jk = ikt, ikb - 1
                        ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,jk,jn,Krhs) + rf_trafac(jl) * zcalv * rDt_trc
                     END DO
                     ! level partially include in ice shelf boundary layer
                     ptr(ji,jj,ikb,jn,Krhs) = ptr(ji,jj,ikb,jn,Krhs) + rf_trafac(jl) * zcalv * zfrac * rDt_trc
                  END_2D
               ENDIF
            END DO
         END IF
      END SELECT
      !

      IF( ln_timing )   CALL timing_stop('trc_ais')
      !
      ! for debugging
!      IF( sn_cfctl%l_prttrc ) THEN ! print mean trends (used for debugging)
!         WRITE(charout, FMT="('ais ')")
!         CALL prt_ctl_trc_info(charout)
!         CALL prt_ctl_trc( tab4d=ptr(:,:,:,:,Krhs), mask=tmask, clinfo=ctrcnm, clinfo2='trd' )
!      ENDIF
      !
   END SUBROUTINE trc_ais

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                              NO 3D passive tracer data
   !!----------------------------------------------------------------------
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_ais_ini   ! Empty routine
   END SUBROUTINE trc_ais_ini
   SUBROUTINE trc_ais( kt, Kbb, Kmm, Krhs )        ! Empty routine
      INTEGER, INTENT(in) :: kt, Kbb, Kmm, Krhs ! time level indices
      WRITE(*,*) 'trc_ais: You should not have seen this print! error?', kt, Kbb, Kmm, Krhs
   END SUBROUTINE trc_ais
#endif

   !!======================================================================
END MODULE trcais
