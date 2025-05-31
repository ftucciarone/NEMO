MODULE p4zsms
   !!======================================================================
   !!                         ***  MODULE p4zsms  ***
   !! TOP :   PISCES Source Minus Sink manager
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
   !!   p4z_sms        : Time loop of passive tracers sms
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p4zbio          ! Biological model
   USE p4zche          ! Chemical model
   USE p4zlys          ! Calcite saturation
   USE p4zflx          ! Gas exchange
   USE p4zbc           ! External source of nutrients
   USE p4zsed          ! Sedimentation
   USE p4zint          ! time interpolation
   USE p4zrem          ! remineralisation
   USE p4zdiaz         !  Diazotrophy
   USE p4zpoc          !  POC remin
   USE iom             ! I/O manager
   USE trd_oce         ! Ocean trends variables
   USE trdtrc          ! TOP trends variables
   USE sedmodel        ! Sediment model
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE prtctl          ! print control for debugging
   USE lib_fortran     ! Fortran routines library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sms_init   ! called in trcini_pisces.F90
   PUBLIC   p4z_sms        ! called in trcsms_pisces.F90

   INTEGER ::    numco2, numnut, numnit      ! logical unit for co2 budget
   REAL(wp) ::   xfact, xfact3

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   xnegtr     ! Array used to indicate negative tracer values
   LOGICAL :: l_budget

      !!* restoring
   LOGICAL  ::  ln_pisdmp     = .FALSE.  !: restoring or not of nutrients to a mean value
   LOGICAL  ::  ln_pisdmp_alk = .FALSE.  !: restoring or not of nutrients to a mean value

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_sms( kt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sms  ***
      !!
      !! ** Purpose :   Managment of the call to Biological sources and sinks 
      !!              routines of PISCES bio-model
      !!
      !! ** Method  : - calls the various SMS subroutines
      !!              - calls the sediment module (if ln_sediment)  
      !!              - several calls of bio and sed (possible time-splitting)
      !!              - handles the potential negative concentrations (xnegtr)
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT( in ) ::   kt              ! ocean time-step index      
      INTEGER, INTENT( in ) ::   Kbb, Kmm, Krhs  ! time level index
      !!
      INTEGER ::   ji, jj, jk, jnt, jn, jl, ikty
      LOGICAL :: ll_dmp
      INTEGER ::   imm      ! local time-level index
      REAL(wp) ::  ztra
      CHARACTER (len=25) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:    ) :: zw2d
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:  ) :: zw3d
      REAL(wp), DIMENSION(jpi,jpj,jpk,jp_pisces) :: ztrbbio

      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_sms')
      !
      IF( kt == nittrc000 ) THEN
        !
        ALLOCATE( xnegtr(jpi,jpj,jpk) )
        !
        IF( .NOT. ln_rsttr ) THEN
            CALL p4z_che( Kbb, Kmm )                  ! initialize the chemical constants
            CALL ahini_for_at( hi, Kbb )              !  set PH at kt=nit000
            t_oce_co2_flx_cum = 0._wp
        ELSE
            CALL p4z_rst( nittrc000, Kbb, Kmm,  'READ' )  !* read or initialize all required fields
        ENDIF
        !
      ENDIF
      !
#if defined key_RK3
      imm = Kbb
#else
      imm = Kmm
#endif
      !
      ll_dmp  = ( ln_pisdmp .OR. ln_pisdmp_alk ) .AND. ( ln_rsttr .OR. kt /= 1 ) 
      ikty = nyear_len(1) * rday / NINT(rn_Dt)    ! time-step at the end of a year 
      ll_dmp = ll_dmp .AND.  MOD( kt-1, ikty ) == 0 

      IF( ll_dmp )    CALL p4z_dmp( kt, Kbb, imm )      ! Relaxation of some tracers
      !
      rfact = rDt_trc  ! time step of PISCES
      !
      IF( ( ln_top_euler .AND. kt == nittrc000 )  .OR. ( .NOT.ln_top_euler .AND. kt <= nittrc000 + 1 ) ) THEN
         rfactr  = 1. / rfact  ! inverse of the time step
         rfact2  = rfact / REAL( nrdttrc, wp )  ! time step of the biological SMS
         rfact2r = 1. / rfact2  ! Inverse of the biological time step
         xstep = rfact2 / rday         ! Time step duration for biology relative to a day
         xfact = 1.e+3 * rfact2r
         IF(lwp) WRITE(numout,*) 
         IF(lwp) WRITE(numout,*) '    Passive Tracer  time step    rfact  = ', rfact, ' rn_Dt = ', rn_Dt
         IF(lwp) write(numout,*) '    PISCES  Biology time step    rfact2 = ', rfact2
         IF(lwp) WRITE(numout,*)
      ENDIF
      !
      DO jn = jp_pcs0, jp_pcs1              !   Store the tracer concentrations before entering PISCES
         ztrbbio(:,:,:,jn) = tr(:,:,:,jn,Kbb)
      END DO

      !
      IF( ll_bc )    CALL p4z_bc( kt, Kbb, Kmm, Krhs )   ! external sources of nutrients 
      !
      CALL p4z_che(     Kbb, Kmm       ) ! computation of chemical constants
      CALL p4z_int( kt, Kbb, Kmm       ) ! computation of various rates for biogeochemistry
      !
      DO jnt = 1, nrdttrc          ! Potential time splitting if requested
         !
         IF( ln_bio ) CALL p4z_bio( kt, jnt, Kbb, Kmm, Krhs )   ! Biology
         IF( ln_p2z ) THEN
            IF( ln_lys ) CALL p2z_lys( kt, jnt, Kbb, Kmm, Krhs )   ! Compute CaCO3 saturation
         ELSE
            IF( ln_lys ) CALL p4z_lys( kt, jnt, Kbb,      Krhs )   ! Compute CaCO3 saturation
         ENDIF
         IF( .NOT. lk_sed .AND. ln_sed ) CALL p4z_sed( kt, jnt, Kbb, Kmm, Krhs )   ! Bottom boundary conditions

         IF( ln_flx ) CALL p4z_flx ( kt, jnt, Kbb, Kmm, Krhs )   ! Compute surface fluxes
         !
         ! Handling of the negative concentrations
         ! The biological SMS may generate negative concentrations
         ! Trends are tested at each grid cell. If a negative concentrations 
         ! is created at a grid cell, all the sources and sinks at that grid 
         ! cell are scale to avoid that negative concentration. This approach 
         ! is quite simplistic but it conserves mass.
         ! ------------------------------------------------------------------
         xnegtr(:,:,:) = 1.e0
         DO jn = jp_pcs0, jp_pcs1
            DO_3D( 0, 0, 0, 0, 1, jpk)
               IF( ( tr(ji,jj,jk,jn,Kbb) + tr(ji,jj,jk,jn,Krhs) ) < 0.e0 ) THEN
                  ztra             = ABS( tr(ji,jj,jk,jn,Kbb) ) / ( ABS( tr(ji,jj,jk,jn,Krhs) ) + rtrn )
                  xnegtr(ji,jj,jk) = MIN( xnegtr(ji,jj,jk),  ztra )
               ENDIF
            END_3D
         END DO
         !                                ! where at least 1 tracer concentration becomes negative
         !                                ! 
         ! Concentrations are updated
         DO jn = jp_pcs0, jp_pcs1
           tr(:,:,:,jn,Kbb) = tr(:,:,:,jn,Kbb) + xnegtr(:,:,:) * tr(:,:,:,jn,Krhs)
         END DO
        !
        IF(  iom_use( 'INTdtAlk' ) .OR. iom_use( 'INTdtDIC' ) .OR. iom_use( 'INTdtFer' ) .OR.  &
          &  iom_use( 'INTdtDIN' ) .OR. iom_use( 'INTdtDIP' ) .OR. iom_use( 'INTdtSil' ) )  THEN
          !
           ALLOCATE( zw3d(A2D(0),jpk), zw2d(A2D(0)) )
        ENDIF
        IF ( iom_use( 'INTdtAlk' ) ) THEN
           DO_3D( 0, 0, 0, 0, 1, jpkm1)
              zw3d(ji,jj,jk) = xnegtr(ji,jj,jk) * xfact * e3t(ji,jj,jk,Kmm) * tmask(ji,jj,jk)
           END_3D
           !
           zw2d(:,:) = 0.
           DO jk = 1, jpkm1
              DO_2D( 0, 0, 0, 0 )
                 zw2d(ji,jj) = zw2d(ji,jj) + zw3d(ji,jj,jk) *  tr(ji,jj,jk,jptal,Krhs) 
              END_2D
           ENDDO
           CALL iom_put( 'INTdtAlk', zw2d )
        ENDIF
          !
        IF ( iom_use( 'INTdtDIC' ) ) THEN
           zw2d(:,:) = 0.
           DO jk = 1, jpkm1
              DO_2D( 0, 0, 0, 0 )
                 zw2d(ji,jj) = zw2d(ji,jj) + zw3d(ji,jj,jk) *  tr(ji,jj,jk,jpdic,Krhs) 
              END_2D
           ENDDO
           CALL iom_put( 'INTdtDIC', zw2d )
        ENDIF
          !
        IF ( iom_use( 'INTdtDIN' ) ) THEN
           zw2d(:,:) = 0.
           DO jk = 1, jpkm1
              DO_2D( 0, 0, 0, 0 )
                 zw2d(ji,jj) = zw2d(ji,jj) + zw3d(ji,jj,jk) * rno3 * ( tr(ji,jj,jk,jpno3,Krhs) + tr(ji,jj,jk,jpnh4,Krhs) ) 
              END_2D
           ENDDO
           CALL iom_put( 'INTdtDIN', zw2d )
        ENDIF
          !
        IF ( iom_use( 'INTdtDIP' ) .AND. .NOT.ln_p2z ) THEN
           zw2d(:,:) = 0.
           DO jk = 1, jpkm1
              DO_2D( 0, 0, 0, 0 )
                 zw2d(ji,jj) = zw2d(ji,jj) + zw3d(ji,jj,jk) * po4r * tr(ji,jj,jk,jppo4,Krhs) 
              END_2D
           ENDDO
           CALL iom_put( 'INTdtDIP', zw2d )
        ENDIF
           !
        IF ( iom_use( 'INTdtFer' ) ) THEN
           zw2d(:,:) = 0.
           DO jk = 1, jpkm1
              DO_2D( 0, 0, 0, 0 )
                 zw2d(ji,jj) = zw2d(ji,jj) + zw3d(ji,jj,jk) *  tr(ji,jj,jk,jpfer,Krhs) 
              END_2D
           ENDDO
           CALL iom_put( 'INTdtFer', zw2d )
        ENDIF
          !
        IF ( iom_use( 'INTdtSil' ) .AND. .NOT.ln_p2z ) THEN
           zw2d(:,:) = 0.
           DO jk = 1, jpkm1
              DO_2D( 0, 0, 0, 0 )
                 zw2d(ji,jj) = zw2d(ji,jj) + zw3d(ji,jj,jk) *  tr(ji,jj,jk,jpsil,Krhs) 
              END_2D
           ENDDO
           CALL iom_put( 'INTdtSil', zw2d )
           !
        ENDIF 

        IF(  iom_use( 'INTdtAlk' ) .OR. iom_use( 'INTdtDIC' ) .OR. iom_use( 'INTdtFer' ) .OR.  &
          &  iom_use( 'INTdtDIN' ) .OR. iom_use( 'INTdtDIP' ) .OR. iom_use( 'INTdtSil' ) )  THEN
          DEALLOCATE( zw3d, zw2d )
        ENDIF
        !
        ! Trends are are reset to 0
         DO jn = jp_pcs0, jp_pcs1
            tr(:,:,:,jn,Krhs) = 0._wp
         END DO
         !
      END DO
      !
      !
      ! If ln_sediment is set to .true. then the sediment module is called
      IF( ln_sediment ) THEN 
         !
         CALL sed_model( kt, Kbb, Kmm, Krhs )     !  Main program of Sediment model
         !
      ENDIF
      !
      DO jn = jp_pcs0, jp_pcs1
         tr(:,:,:,jn,Krhs) = ( tr(:,:,:,jn,Kbb) - ztrbbio(:,:,:,jn) ) * rfactr
         tr(:,:,:,jn,Kbb ) = ztrbbio(:,:,:,jn)
      END DO
      !
      !
      IF( l_trdtrc ) THEN
         DO jn = jp_pcs0, jp_pcs1
           CALL trd_trc( tr(:,:,:,jn,Krhs), jn, jptra_sms, kt, imm )   ! save trends
         END DO
      END IF
      !  
      IF( lrst_trc )  CALL p4z_rst( kt, Kbb, Kmm,  'WRITE' )           !* Write PISCES informations in restart file 
      !

      CALL p4z_budget( kt, imm ) ! Budget checking

      IF( lwm .AND. kt == nittrc000    )  CALL FLUSH( numonp )         ! flush output namelist PISCES
      !
      IF( ln_timing )  CALL timing_stop('p4z_sms')
      !
   END SUBROUTINE p4z_sms


   SUBROUTINE p4z_sms_init
      !!----------------------------------------------------------------------
      !!                     ***  p4z_sms_init  ***  
      !!
      !! ** Purpose :   read the general PISCES namelist
      !!
      !! ** input   :   file 'namelist_pisces' containing the following
      !!                namelist: nampisbio, nampisdmp
      !!----------------------------------------------------------------------
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!
      NAMELIST/nampisbio/ nrdttrc, wsbio, xkmort, feratz, feratm, wsbio2, wsbio2max,    &
         &                wsbio2scale, ldocp, ldocz, lthet, no3rat3, po4rat3
         !
      NAMELIST/nampisdmp/ ln_pisdmp, ln_pisdmp_alk
      NAMELIST/nampisdbg/ ln_bio, ln_lys, ln_sed, ln_flx, &
         &                ln_fechem, ln_micro, ln_meso, ln_mort, &
         &                ln_prod, ln_agg, ln_rem, ln_poc, ln_diaz
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_sms_init : PISCES initialization'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF

      READ_NML_REF(numnatp,nampisbio)
      READ_NML_CFG(numnatp,nampisbio)
      IF(lwm) WRITE( numonp, nampisbio )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampisbio'
         WRITE(numout,*) '      frequency for the biology                 nrdttrc     =', nrdttrc
         WRITE(numout,*) '      POC sinking speed                         wsbio       =', wsbio
         WRITE(numout,*) '      half saturation constant for mortality    xkmort      =', xkmort 
         IF( ln_p5z ) THEN
            WRITE(numout,*) '      N/C in zooplankton                     no3rat3     =', no3rat3
            WRITE(numout,*) '      P/C in zooplankton                     po4rat3     =', po4rat3
         ENDIF
         WRITE(numout,*) '      Fe/C in microzooplankton                  feratz      =', feratz
         IF( .NOT. ln_p2z ) THEN
            WRITE(numout,*) '      Fe/C in mesozooplankton                   feratm      =', feratm
         ENDIF
         WRITE(numout,*) '      Big particles sinking speed               wsbio2      =', wsbio2
         WRITE(numout,*) '      Big particles maximum sinking speed       wsbio2max   =', wsbio2max
         WRITE(numout,*) '      Big particles sinking speed length scale  wsbio2scale =', wsbio2scale
         IF( ln_ligand ) THEN
            WRITE(numout,*) '      Phyto ligand production per unit doc           ldocp  =', ldocp
            WRITE(numout,*) '      Zoo ligand production per unit doc             ldocz  =', ldocz
            WRITE(numout,*) '      Proportional loss of ligands due to Fe uptake  lthet  =', lthet
         ENDIF
      ENDIF


      READ_NML_REF(numnatp,nampisdmp)
      READ_NML_CFG(numnatp,nampisdmp)
      IF(lwm) WRITE( numonp, nampisdmp )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   Namelist : nampisdmp --- relaxation to GLODAP'
         WRITE(numout,*) '      Relaxation of tracer to glodap mean value   ln_pisdmp =', ln_pisdmp
         WRITE(numout,*) '      Relaxation of alkalinity to glodap mean value   ln_pisdmp_alk  =', ln_pisdmp_alk
      ENDIF
      !
      READ_NML_REF(numnatp,nampisdbg)
      READ_NML_CFG(numnatp,nampisdbg)
      IF(lwm) WRITE( numonp, nampisdbg )
      !
   END SUBROUTINE p4z_sms_init


   SUBROUTINE p4z_rst( kt, Kbb, Kmm, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE p4z_rst  ***
      !!
      !!  ** Purpose : Read or write specific PISCES variables in restart file:
      !!
      !!  WRITE(READ) mode:
      !!       kt        : number of time step since the begining of the experiment at the
      !!                   end of the current(previous) run
      !!---------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt         ! ocean time-step
      INTEGER         , INTENT(in) ::   Kbb, Kmm   ! time level indices
      CHARACTER(len=*), INTENT(in) ::   cdrw       ! "READ"/"WRITE" flag
      !!---------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN
         !
         ! Read the specific variable of PISCES
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) ' p4z_rst : Read specific variables from pisces model '
         IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
         ! 
         ! Read the pH. If not in the restart file, then it is initialized from
         ! the initial conditions
         IF( iom_varid( numrtr, 'PH', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'PH' , hi(:,:,:)  )
         ELSE
            CALL p4z_che( Kbb, Kmm )                  ! initialize the chemical constants
            CALL ahini_for_at( hi, Kbb )
         ENDIF

         IF( ln_p2z ) THEN
            IF( iom_varid( numrtr, 'Thetanano', ldstop = .FALSE. ) > 0 ) THEN
               CALL iom_get( numrtr, jpdom_auto, 'Thetanano' , thetanano(:,:,:)  )
            ELSE
               thetanano(:,:,:) = 1.0 / 55.0
            ENDIF
         ENDIF

         IF( .NOT. ln_p2z ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'Silicalim', xksi(:,:) )
   
            ! Read the Si half saturation constant and the maximum Silica concentration
            IF( iom_varid( numrtr, 'Silicamax', ldstop = .FALSE. ) > 0 ) THEN
               CALL iom_get( numrtr, jpdom_auto, 'Silicamax' , xksimax(:,:)  )
            ELSE
               xksimax(:,:) = xksi(:,:)
            ENDIF
         ENDIF

         ! Read the Fe3 consumption term by phytoplankton
         IF( iom_varid( numrtr, 'Consfe3', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'Consfe3' , consfe3(:,:,:)  )
         ELSE
            consfe3(:,:,:) = 0._wp
         ENDIF

        ! Read mean remineralisation rate
         ll_poc_lab = .FALSE.
         IF( iom_varid( numrtr, 'remintpoc', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'remintpoc' , remintpoc(:,:,:)  )
            IF( ln_p4z .OR. ln_p5z ) &
            CALL iom_get( numrtr, jpdom_auto, 'remintgoc' , remintgoc(:,:,:)  )
         ELSE
                                     remintpoc(:,:,:) = 0.03_wp
            IF( ln_p4z .OR. ln_p5z ) remintgoc(:,:,:) = 0.03_wp
         ENDIF


         ! Read the cumulative total flux. If not in the restart file, it is set to 0
         IF( iom_varid( numrtr, 'tcflxcum', ldstop = .FALSE. ) > 0 ) THEN  ! cumulative total flux of carbon
            CALL iom_get( numrtr, 'tcflxcum' , t_oce_co2_flx_cum  )
         ELSE
            t_oce_co2_flx_cum = 0._wp
         ENDIF
         !
         ! PISCES size proxy
         ! Read the size of the different phytoplankton groups
         ! If not in the restart file, they are set to 1
         IF( iom_varid( numrtr, 'sizen', ldstop = .FALSE. ) > 0 ) THEN
            CALL iom_get( numrtr, jpdom_auto, 'sizen' , sizen(:,:,:)  )
               sizen(:,:,:) = MAX( 1.0, sizen(:,:,:) )
            IF( ln_p4z .OR. ln_p5z )THEN
               CALL iom_get( numrtr, jpdom_auto, 'sized' , sized(:,:,:)  )
               sized(:,:,:) = MAX( 1.0, sized(:,:,:) )
            ENDIF
            IF( ln_p5z ) THEN
               CALL iom_get( numrtr, jpdom_auto, 'sizep' , sizep(:,:,:)  )
               sizep(:,:,:) = MAX( 1.0, sizep(:,:,:) )
            ENDIF
         ELSE
                                     sizen(:,:,:) = 1.
            IF( ln_p4z .OR. ln_p5z ) sized(:,:,:) = 1.
            IF( ln_p5z )             sizep(:,:,:) = 1.
         ENDIF
        !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN
         ! write the specific variables of PISCES
         IF( kt == nitrst ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'p4z_rst : write pisces restart file  kt =', kt
            IF(lwp) WRITE(numout,*) '~~~~~~~'
         ENDIF
         CALL iom_rstput( kt, nitrst, numrtw, 'PH', hi(:,:,:)           )
         CALL iom_rstput( kt, nitrst, numrtw, 'sizen', sizen(:,:,:) )  ! Size of nanophytoplankton
         CALL iom_rstput( kt, nitrst, numrtw, 'tcflxcum', t_oce_co2_flx_cum )
         IF( ln_p2z ) THEN
            CALL iom_rstput( kt, nitrst, numrtw, 'Thetanano' , thetanano(:,:,:)  )
         ENDIF

         CALL iom_rstput( kt, nitrst, numrtw, 'Consfe3'  , consfe3(:,:,:) ) ! Si max concentration
         CALL iom_rstput( kt, nitrst, numrtw, 'remintpoc', remintpoc(:,:,:) ) ! Mean remineralisation rate of POC
         IF ( ln_p4z .OR. ln_p5z ) THEN
            CALL iom_rstput( kt, nitrst, numrtw, 'Silicalim', xksi(:,:)    )
            CALL iom_rstput( kt, nitrst, numrtw, 'Silicamax', xksimax(:,:) )
            CALL iom_rstput( kt, nitrst, numrtw, 'remintgoc', remintgoc(:,:,:) ) ! Mean remineralisation rate of GOC
            CALL iom_rstput( kt, nitrst, numrtw, 'sized', sized(:,:,:) )  ! Size of diatoms
         ENDIF
         IF( ln_p5z ) CALL iom_rstput( kt, nitrst, numrtw, 'sizep', sizep(:,:,:) )  ! Size of picophytoplankton
      ENDIF
      !
   END SUBROUTINE p4z_rst


   SUBROUTINE p4z_dmp( kt, Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  p4z_dmp  ***
      !!
      !! ** purpose  : Relaxation of the total budget of some elements
      !!               This routine avoids the model to drift far from the 
      !!               observed content in various elements
      !!               Elements that may be relaxed : Alk, P, N, Si
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT( in )  ::     kt            ! time step
      INTEGER, INTENT( in )  ::     Kbb, Kmm      ! time level indices
      !
      REAL(wp) ::  alkmean = 2426.     ! mean value of alkalinity ( Glodap ; for Goyet 2391. )
      REAL(wp) ::  po4mean = 2.174     ! mean value of phosphate
      REAL(wp) ::  no3mean = 31.00     ! mean value of nitrate
      REAL(wp) ::  silmean = 90.33     ! mean value of silicate
      !
      REAL(wp) :: zarea, zalksumn, zpo4sumn, zno3sumn, zsilsumn
      REAL(wp) :: zalksumb, zpo4sumb, zno3sumb, zsilsumb
      !!---------------------------------------------------------------------

      IF(lwp)  WRITE(numout,*)
      IF(lwp)  WRITE(numout,*) ' p4z_dmp : Restoring of nutrients at time-step kt = ', kt
      IF(lwp)  WRITE(numout,*)

      IF( cn_cfg == "ORCA" .OR. cn_cfg == "orca") THEN
         IF( .NOT. ln_c1d ) THEN      ! ORCA configuration (not 1D) !
            ! set total alkalinity, phosphate, nitrate & silicate
            zarea          = 1._wp / glob_3Dsum( 'p4zsms', cvol(:,:,:) ) * 1e6              
            !
            IF( ln_pisdmp ) THEN
               zno3sumn = glob_3Dsum( 'p4zsms', tr(:,:,:,jpno3,Kmm) * cvol(:,:,:)  ) * zarea * rno3
               ! Correct the trn mean content of NO3
               IF(lwp) WRITE(numout,*) '       NO3N  mean : ', zno3sumn
               tr(:,:,:,jpno3,Kmm) = tr(:,:,:,jpno3,Kmm) * no3mean / zno3sumn
               IF ( ln_p4z .OR. ln_p5z ) THEN
                  zpo4sumn = glob_3Dsum( 'p4zsms', tr(:,:,:,jppo4,Kmm) * cvol(:,:,:) ) * zarea * po4r
                  zsilsumn = glob_3Dsum( 'p4zsms', tr(:,:,:,jpsil,Kmm) * cvol(:,:,:) ) * zarea
                  ! Correct the trn mean content of PO4
                  IF(lwp) WRITE(numout,*) '       PO4N  mean : ', zpo4sumn
                  tr(:,:,:,jppo4,Kmm) = tr(:,:,:,jppo4,Kmm) * po4mean / zpo4sumn
                  ! Correct the trn mean content of SiO3
                  IF(lwp) WRITE(numout,*) '       SiO3N mean : ', zsilsumn
                  tr(:,:,:,jpsil,Kmm) = MIN( 400.e-6,tr(:,:,:,jpsil,Kmm) * silmean / zsilsumn )
               ENDIF
               !
               IF( .NOT. ln_top_euler ) THEN
                  IF(lwp) WRITE(numout,*) ' '
                  zno3sumb = glob_3Dsum( 'p4zsms', tr(:,:,:,jpno3,Kbb) * cvol(:,:,:) ) * zarea * rno3
                  ! Correct the trb mean content of NO3
                  IF(lwp) WRITE(numout,*) '       NO3B  mean : ', zno3sumb
                  tr(:,:,:,jpno3,Kbb) = tr(:,:,:,jpno3,Kbb) * no3mean / zno3sumb
                  IF( ln_p4z .OR. ln_p5z ) THEN
                     zpo4sumb = glob_3Dsum( 'p4zsms', tr(:,:,:,jppo4,Kbb) * cvol(:,:,:) ) * zarea * po4r
                     zsilsumb = glob_3Dsum( 'p4zsms', tr(:,:,:,jpsil,Kbb) * cvol(:,:,:) ) * zarea
                     ! Correct the trb mean content of PO4
                     IF(lwp) WRITE(numout,*) '       PO4B  mean : ', zpo4sumb
                     tr(:,:,:,jppo4,Kbb) = tr(:,:,:,jppo4,Kbb) * po4mean / zpo4sumb
                     ! Correct the trb mean content of SiO3
                     IF(lwp) WRITE(numout,*) '       SiO3B mean : ', zsilsumb
                     tr(:,:,:,jpsil,Kbb) = MIN( 400.e-6,tr(:,:,:,jpsil,Kbb) * silmean / zsilsumb )
                  ENDIF
               ENDIF
            ENDIF
            !
            IF( ln_pisdmp_alk ) THEN
               zalksumn = glob_3Dsum( 'p4zsms', tr(:,:,:,jptal,Kmm) * cvol(:,:,:)  ) * zarea
               ! Correct the trn mean content of alkalinity
               IF(lwp) WRITE(numout,*) '       TALKN mean : ', zalksumn
               tr(:,:,:,jptal,Kmm) = tr(:,:,:,jptal,Kmm) * alkmean / zalksumn
               !
               IF( .NOT. ln_top_euler ) THEN
                  zalksumb = glob_3Dsum( 'p4zsms', tr(:,:,:,jptal,Kbb) * cvol(:,:,:) ) * zarea
                  ! Correct the trb mean content of alkalinity
                  IF(lwp) WRITE(numout,*) '       TALKB mean : ', zalksumb
                  tr(:,:,:,jptal,Kbb) = tr(:,:,:,jptal,Kbb) * alkmean / zalksumb
               ENDIF
            ENDIF
        ENDIF
        !
      ENDIF
      !
   END SUBROUTINE p4z_dmp


   SUBROUTINE p4z_budget( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_chk_mass  ***
      !!
      !! ** Purpose :  Mass conservation check 
      !!
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt      ! ocean time-step index      
      INTEGER, INTENT( in ) :: Kmm     ! time level indices
      REAL(wp)              :: zdenittot, znitrpottot, zarea
      REAL(wp)              :: zalkbudget, zno3budget, zsilbudget, zferbudget, zpo4budget 
      CHARACTER(LEN=100)    :: cltxt
      INTEGER :: ji, jj, jk
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: zw3d
      REAL(wp), ALLOCATABLE, DIMENSION(:) :: zchk
      !!----------------------------------------------------------------------
      !
      IF( kt == nittrc000 ) THEN 
         xfact3 = 1.e+3 * rfact2r * rno3   ! conversion molC/l/kt ----> molN/m3/s
         l_budget =   iom_use( "pno3tot" ) .OR. iom_use( "ppo4tot" ) .OR. iom_use( "psiltot" )  &
           &     .OR. iom_use( "palktot" ) .OR. iom_use( "pfertot" ) .OR. iom_use( "tdenit"  )  &
           &     .OR. iom_use( "tnfix" ) 
      ENDIF

      ! Compute the budget of NO3
      IF( l_budget ) THEN
        !
        IF( ln_p2z ) THEN
           ALLOCATE( zw3d(A2D(0),jpk,6), zchk(6) )  ;  zw3d(:,:,jpk,:) = 0._wp
        ELSE
           ALLOCATE( zw3d(A2D(0),jpk,8), zchk(8) )  ;  zw3d(:,:,jpk,:) = 0._wp
        ENDIF
        !
        zw3d(A2D(0),1:jpkm1,1)  = cvol(A2D(0),1:jpkm1) 
        !
        IF( ln_p2z ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zw3d(ji,jj,jk,2)  = ( tr(ji,jj,jk,jpno3,Kmm) + tr(ji,jj,jk,jpphy,Kmm)                      &
               &              +   tr(ji,jj,jk,jppoc,Kmm) + tr(ji,jj,jk,jpdoc,Kmm)                      &
               &              +   tr(ji,jj,jk,jpzoo,Kmm) ) * cvol(ji,jj,jk)
            END_3D
        ELSE IF( ln_p4z ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zw3d(ji,jj,jk,2) = ( tr(ji,jj,jk,jpno3,Kmm) + tr(ji,jj,jk,jpnh4,Kmm)                      &
               &             +   tr(ji,jj,jk,jpphy,Kmm) + tr(ji,jj,jk,jpdia,Kmm)                      &
               &             +   tr(ji,jj,jk,jppoc,Kmm) + tr(ji,jj,jk,jpgoc,Kmm)  + tr(ji,jj,jk,jpdoc,Kmm)  &        
               &             +   tr(ji,jj,jk,jpzoo,Kmm) + tr(ji,jj,jk,jpmes,Kmm)  ) * cvol(ji,jj,jk)
            END_3D
        ELSE
            DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zw3d(ji,jj,jk,2) = ( tr(ji,jj,jk,jpno3,Kmm) + tr(ji,jj,jk,jpnh4,Kmm) + tr(ji,jj,jk,jpnph,Kmm)   &
               &             +   tr(ji,jj,jk,jpndi,Kmm) + tr(ji,jj,jk,jpnpi,Kmm)                      & 
               &             +   tr(ji,jj,jk,jppon,Kmm) + tr(ji,jj,jk,jpgon,Kmm) + tr(ji,jj,jk,jpdon,Kmm)   &
               &             + ( tr(ji,jj,jk,jpzoo,Kmm) + tr(ji,jj,jk,jpmes,Kmm) ) * no3rat3 ) * cvol(ji,jj,jk)
            END_3D
        ENDIF
        !
        ! Compute the budget of Iron
        IF( ln_p2z ) THEN
           DO_3D( 0, 0, 0, 0, 1, jpkm1)
              zw3d(ji,jj,jk,3) =   ( tr(ji,jj,jk,jpfer,Kmm) + tr(ji,jj,jk,jpphy,Kmm) * feratz  &
                  &            +     tr(ji,jj,jk,jppoc,Kmm) * feratz               &
                  &            +     tr(ji,jj,jk,jpzoo,Kmm) * feratz ) * cvol(ji,jj,jk)
            END_3D
        ELSE IF( ln_p4z ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpkm1)
               zw3d(ji,jj,jk,3) =   ( tr(ji,jj,jk,jpfer,Kmm) + tr(ji,jj,jk,jpnfe,Kmm) + tr(ji,jj,jk,jpdfe,Kmm)   &
                  &             +     tr(ji,jj,jk,jpbfe,Kmm) + tr(ji,jj,jk,jpsfe,Kmm)                      &
                  &             +     tr(ji,jj,jk,jpzoo,Kmm) * feratz + tr(ji,jj,jk,jpmes,Kmm) * feratm ) * cvol(ji,jj,jk)
            END_3D
        ELSE
            DO_3D( 0, 0, 0, 0, 1, jpkm1)
               zw3d(ji,jj,jk,3) =   ( tr(ji,jj,jk,jpfer,Kmm) + tr(ji,jj,jk,jpnfe,Kmm) + tr(ji,jj,jk,jpdfe,Kmm)   &
                  &             +     tr(ji,jj,jk,jppfe,Kmm) + tr(ji,jj,jk,jpbfe,Kmm) + tr(ji,jj,jk,jpsfe,Kmm)   &
                  &             +     tr(ji,jj,jk,jpzoo,Kmm) * feratz + tr(ji,jj,jk,jpmes,Kmm) * feratm ) * cvol(ji,jj,jk)
            END_3D
        ENDIF
        !
        ! Compute the budget of total alkalinity
        IF( ln_p2z ) THEN
           DO_3D( 0, 0, 0, 0, 1, jpkm1)
              zw3d(ji,jj,jk,4) =  ( tr(ji,jj,jk,jpno3,Kmm) * rno3 + tr(ji,jj,jk,jptal,Kmm) ) * cvol(ji,jj,jk)             
           END_3D
        ELSE
           DO_3D( 0, 0, 0, 0, 1, jpkm1)
              zw3d(ji,jj,jk,4) =  ( tr(ji,jj,jk,jpno3,Kmm) * rno3 + tr(ji,jj,jk,jptal,Kmm)   &
                &                 + tr(ji,jj,jk,jpcal,Kmm) * 2. - tr(ji,jj,jk,jpnh4,Kmm) * rno3 ) * cvol(ji,jj,jk)
           END_3D
        ENDIF
        !
        ! Global budget of N SMS : nitrogen fixation by the diazotrophs
        DO_3D( 0, 0, 0, 0, 1, jpkm1)
           zw3d(ji,jj,jk,5) =  nitrpot(ji,jj,jk) * nitrfix * cvol(ji,jj,jk)
        END_3D
        !
        ! Global budget of N SMS : denitrification in the water column and in the sediment
        !
        DO_2D( 0, 0, 0, 0 )
           zw3d(ji,jj,1,6) =  denitr(ji,jj,1) *  rdenit * xnegtr(ji,jj,1) * cvol(ji,jj,1) &
                &           + sdenit(ji,jj)   *  e1e2t(ji,jj) * tmask(ji,jj,1)
        END_2D
        DO_3D( 0, 0, 0, 0, 2, jpkm1)
           zw3d(ji,jj,jk,6) =  denitr(ji,jj,jk) *  rdenit * xnegtr(ji,jj,jk) * cvol(ji,jj,jk)
        END_3D
        !
        IF( .NOT. ln_p2z ) THEN
          !
          ! Compute the budget of PO4
          IF( ln_p4z ) THEN
             DO_3D( 0, 0, 0, 0, 1, jpkm1)
                zw3d(ji,jj,jk,7) = ( tr(ji,jj,jk,jppo4,Kmm)                                         &
                   &             +   tr(ji,jj,jk,jpphy,Kmm) + tr(ji,jj,jk,jpdia,Kmm)                      &
                   &             +   tr(ji,jj,jk,jppoc,Kmm) + tr(ji,jj,jk,jpgoc,Kmm)  + tr(ji,jj,jk,jpdoc,Kmm)  &
                   &             +   tr(ji,jj,jk,jpzoo,Kmm) + tr(ji,jj,jk,jpmes,Kmm) ) * cvol(ji,jj,jk)
             END_3D
           ELSE
             DO_3D( 0, 0, 0, 0, 1, jpkm1)
                zw3d(ji,jj,jk,7) = ( tr(ji,jj,jk,jppo4,Kmm) + tr(ji,jj,jk,jppph,Kmm)                      &
                  &              +   tr(ji,jj,jk,jppdi,Kmm) + tr(ji,jj,jk,jpppi,Kmm)                      &
                  &              +   tr(ji,jj,jk,jppop,Kmm) + tr(ji,jj,jk,jpgop,Kmm) + tr(ji,jj,jk,jpdop,Kmm)   &
                  &              + ( tr(ji,jj,jk,jpzoo,Kmm) + tr(ji,jj,jk,jpmes,Kmm) ) * po4rat3 ) * cvol(ji,jj,jk)
             END_3D
          ENDIF
          !
          ! Compute the budget of Silicate
          DO_3D( 0, 0, 0, 0, 1, jpkm1)
             zw3d(ji,jj,jk,8) =  ( tr(ji,jj,jk,jpsil,Kmm) + tr(ji,jj,jk,jpgsi,Kmm) + tr(ji,jj,jk,jpdsi,Kmm) ) * cvol(ji,jj,jk)
          END_3D
          !
        ENDIF
        !
        IF ( ln_p2z ) THEN
           zchk(1:6) = glob_3Dsum( 'p4zsms', zw3d(:,:,:,1:6), cdelay = 'biobg'  )
        ELSE
           zchk(1:8) = glob_3Dsum( 'p4zsms', zw3d(:,:,:,1:8), cdelay = 'biobg'  )
        ENDIF
        !
        zarea = 1._wp / zchk(1)
        !
        zno3budget = zchk(2) * zarea
        CALL iom_put( "pno3tot", zno3budget )
        !
        zferbudget = zchk(3) * zarea
        CALL iom_put( "pfertot", zferbudget )
        !
        zalkbudget = zchk(4) * zarea
        CALL iom_put( "palktot", zalkbudget )
        !
        znitrpottot = zchk(5)
        CALL iom_put( "tnfix"  , znitrpottot * xfact3 )
        !
        zdenittot   = zchk(6)
        CALL iom_put( "tdenit" , zdenittot * xfact3 )
        !
        IF ( .NOT. ln_p2z ) THEN
           zpo4budget =  zchk(7) * zarea
           CALL iom_put( "ppo4tot", zpo4budget )
           !
           zsilbudget = zchk(8) * zarea
           CALL iom_put( "psiltot", zsilbudget )
        ENDIF
        !
        DEALLOCATE( zw3d, zchk ) 
        !
      ENDIF
      !
   END SUBROUTINE p4z_budget

   !!======================================================================
END MODULE p4zsms 
