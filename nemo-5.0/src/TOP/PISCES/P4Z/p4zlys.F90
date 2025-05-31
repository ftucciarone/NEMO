MODULE p4zlys
   !!======================================================================
   !!                         ***  MODULE p4zlys  ***
   !! TOP :   PISCES 
   !!======================================================================
   !! History :    -   !  1988-07  (E. MAIER-REIMER) Original code
   !!              -   !  1998     (O. Aumont) additions
   !!              -   !  1999     (C. Le Quere) modifications
   !!             1.0  !  2004     (O. Aumont) modifications
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!                  !  2011-02  (J. Simeon, J. Orr)  Calcon salinity dependence
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Improvment of calcite dissolution
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p4z_lys        :   Compute the CaCO3 dissolution 
   !!   p4z_lys_init   :   Read the namelist parameters
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zche          !  Chemical model
   USE p4zsink         ! sinking of particles
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p2z_lys         ! called in trcsms_pisces.F90
   PUBLIC   p4z_lys         ! called in trcsms_pisces.F90
   PUBLIC   p4z_lys_init    ! called in trcsms_pisces.F90

   REAL(wp), PUBLIC ::   kdca   !: diss. rate constant calcite
   REAL(wp), PUBLIC ::   nca    !: order of reaction for calcite dissolution

   INTEGER  :: rmtss              ! number of seconds per month 
   LOGICAL  :: l_dia

   !! * Module variables
   REAL(wp) :: calcon = 1.03E-2           !: mean calcite concentration [Ca2+]  in sea water [mole/kg solution]
   ! J. ORR: Made consistent with mocsy's choice based on literature review from Munhoven
!   REAL(wp) :: calcon = 1.0287E-2           !: mean calcite concentration [Ca2+] in sea water [mole/kg solution]

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p2z_lys( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_lys  ***
      !!
      !! ** Purpose :   CALCULATES DEGREE OF CACO3 SATURATION IN THE WATER
      !!                COLUMN, DISSOLUTION/PRECIPITATION OF CACO3 AND LOSS
      !!                OF CACO3 TO THE CACO3 SEDIMENT POOL.
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   ! ocean time step and ???
      INTEGER, INTENT(in)  ::  Kbb, Kmm, Krhs ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zdispot, zfact, zcalcon, zdepexp, zdissol
      REAL(wp) ::   zomegaca, zexcess, zexcess0, zkd, zwsbio
      CHARACTER (len=25) ::   charout
      REAL(wp), DIMENSION(A2D(0),jpk) :: zhinit, zhi, zco3, zcaco3, ztra
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)  :: zcaldiss
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p2z_lys')
      !
      IF( kt == nittrc000 )  &
           & l_dia = iom_use( "PH" ) .OR. iom_use( "CO3" ) .OR. iom_use( "CO3sat" ) .OR. iom_use( "DCAL" ) .OR. iom_use( "PCAL" )
      !
      IF( l_dia )   THEN                  !* Save ta and sa trends
         ALLOCATE( zcaldiss(A2D(0),jpk) )    ;    zcaldiss(A2D(0),:) = 0._wp   ;   zco3(A2D(0),jpk) = 0._wp
      ENDIF
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zhinit(ji,jj,jk) = hi(ji,jj,jk) * 1000._wp / ( rhop(ji,jj,jk) + rtrn )
      END_3D
      !
      !     -------------------------------------------
      !     COMPUTE [CO3--] and [H+] CONCENTRATIONS
      !     -------------------------------------------

      CALL solve_at_general( zhinit, zhi, Kbb )

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zco3(ji,jj,jk) = tr(ji,jj,jk,jpdic,Kbb) * ak13(ji,jj,jk) * ak23(ji,jj,jk) / (zhi(ji,jj,jk)**2   &
            &             + ak13(ji,jj,jk) * zhi(ji,jj,jk) + ak13(ji,jj,jk) * ak23(ji,jj,jk) + rtrn )
         hi  (ji,jj,jk) = zhi(ji,jj,jk) * rhop(ji,jj,jk) / 1000._wp
      END_3D

      !     ---------------------------------------------------------
      !        CALCULATE DEGREE OF CACO3 SATURATION AND CORRESPONDING
      !        DISSOLOUTION AND PRECIPITATION OF CACO3 (BE AWARE OF
      !        MGCO3)
      !     ---------------------------------------------------------

      zkd = kdca * 0.2**(nca - 0.2)
      DO_3D( 0, 0, 0, 0, 1, jpkm1)

         ! DEVIATION OF [CO3--] FROM SATURATION VALUE
         ! Salinity dependance in zomegaca and divide by rhop to have good units
         zcalcon  = calcon * ( salinprac(ji,jj,jk) / 35._wp )
         zfact    = rhop(ji,jj,jk) / 1000._wp
         zomegaca = ( zcalcon * zco3(ji,jj,jk) ) / ( aksp(ji,jj,jk) * zfact + rtrn )

         ! SET DEGREE OF UNDER-/SUPERSATURATION
         excess(ji,jj,jk) = 1._wp - zomegaca
         zexcess0 = MAX( 0., excess(ji,jj,jk) )

         IF( zomegaca < 0.8 ) THEN
            zdispot = kdca * zexcess0**nca
         ELSE
            zdispot = zkd * zexcess0**0.2
         ENDIF

         !  CHANGE OF [CO3--] , [ALK], PARTICULATE [CACO3],
         !       AND [SUM(CO2)] DUE TO CACO3 DISSOLUTION/PRECIPITATION
         ztra(ji,jj,jk)  = zdispot / rmtss ! calcite dissolution
         !
      END_3D
      !
      zcaco3(:,:,:) = 0._wp
      !
      DO_2D( 0, 0, 0, 0 )
         zcaco3(ji,jj,1) = prodcal(ji,jj,1) * rfact2r / ( wsbio4(ji,jj,1) / e3t(ji,jj,1,Kmm) / rday + ztra(ji,jj,1) )
      END_2D

      DO_3D( 0, 0, 0, 0, 2, jpkm1)
         IF ( tmask(ji,jj,1) == 1. ) THEN
            zwsbio = wsbio4(ji,jj,jk) / rday
            IF ( ztra(ji,jj,jk) == 0.0 ) THEN
               zcaco3(ji,jj,jk) = zcaco3(ji,jj,jk-1) + prodcal(ji,jj,jk) * rfact2r / zwsbio * e3t(ji,jj,jk,Kmm) 
               zdissol = 0.0
            ELSE
               zdepexp = exp( - ztra(ji,jj,jk) * e3t(ji,jj,jk,Kmm) / zwsbio )
               zcaco3(ji,jj,jk) = prodcal(ji,jj,jk) * rfact2r / ztra(ji,jj,jk)       &
                 &                * (1.0 - zdepexp ) + zcaco3(ji,jj,jk-1) * zdepexp
               zdissol = prodcal(ji,jj,jk) * e3t(ji,jj,jk,Kmm) + prodcal(ji,jj,jk)   &
                 &       * zwsbio / ztra(ji,jj,jk) * ( zdepexp - 1.0 )               &
                 &       + zwsbio * zcaco3(ji,jj,jk-1) * ( 1.0 - zdepexp ) * rfact2 
               zdissol = zdissol / e3t(ji,jj,jk,Kmm)
            ENDIF
            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + zdissol
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + 2.0 * zdissol
            !
            IF( l_dia ) zcaldiss(ji,jj,jk) = zdissol
            !
         ENDIF
      END_3D
      DO_2D( 0, 0, 0, 0 )
         sinkcalb(ji,jj) = wsbio4(ji,jj,mbkt(ji,jj)) * zcaco3(ji,jj,mbkt(ji,jj)) * rfact2 / rday
      END_2D
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
        WRITE(charout, FMT="('lys ')")
        CALL prt_ctl_info( charout, cdcomp = 'top' )
        CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( l_dia .AND. knt == nrdttrc ) THEN
         CALL iom_put( "PCAL", prodcal(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )   ! Calcite production
         CALL iom_put( "DCAL", zcaldiss(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )  ! calcite dissolution
         CALL iom_put( "PH" , -1. * LOG10( MAX( hi(:,:,:), rtrn ) ) * tmask(A2D(0),:) )  !  PH
         CALL iom_put( "CO3", zco3(:,:,:) * 1.e+3 * tmask(A2D(0),:) )    ! bicarbonate
         CALL iom_put( "CO3sat", aksp(:,:,:) * rhop(A2D(0),:) / 1000._wp &     ! calcite saturation
              &                / calcon * ( salinprac(:,:,:) / 35._wp ) * 1.e+3 * tmask(A2D(0),:)  ) 
         CALL iom_put( "EPCAL100",  wsbio4(:,:,ik100) * zcaco3(:,:,ik100) * 1.e+3 / rday * tmask(A2D(0),ik100) )  ! Export of calcite at 100m
         CALL iom_put( "EXPCAL"  ,  wsbio4(:,:,:) * zcaco3(:,:,:) * 1.e+3 / rday * tmask(A2D(0),:) )             ! Export of calcite in the water column
         DEALLOCATE( zcaldiss )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p2z_lys')
      !
   END SUBROUTINE p2z_lys

   SUBROUTINE p4z_lys( kt, knt, Kbb, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_lys  ***
      !!
      !! ** Purpose :   CALCULATES DEGREE OF CACO3 SATURATION IN THE WATER
      !!                COLUMN, DISSOLUTION/PRECIPITATION OF CACO3 AND LOSS
      !!                OF CACO3 TO THE CACO3 SEDIMENT POOL.
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   ! ocean time step and ???
      INTEGER, INTENT(in)  ::  Kbb, Krhs ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zdispot, zfact, zcalcon, zdissol
      REAL(wp) ::   zomegaca, zexcess, zexcess0, zkd, zdens
      CHARACTER (len=25) ::   charout
      REAL(wp), DIMENSION(A2D(0),jpk) :: zhinit, zhi, zco3
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)  :: zcaldiss
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p4z_lys')
      !
     IF( kt == nittrc000 )  &
           & l_dia = iom_use( "PH" ) .OR. iom_use( "CO3" ) .OR. iom_use( "CO3sat" ) .OR. iom_use( "DCAL" ) .OR. iom_use( "PCAL" )

      IF( l_dia )   THEN                  !* Save ta and sa trends
         ALLOCATE( zcaldiss(A2D(0),jpk) )    ;    zcaldiss(A2D(0),jpk) = 0._wp   ;   zco3(A2D(0),jpk) = 0._wp
      ENDIF
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zdens = rhop(ji,jj,jk) / 1000._wp
         zhinit(ji,jj,jk) = hi(ji,jj,jk) / ( zdens + rtrn )
      END_3D
      !
      !     -------------------------------------------
      !     COMPUTE [CO3--] and [H+] CONCENTRATIONS
      !     -------------------------------------------

      CALL solve_at_general( zhinit, zhi, Kbb )

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zdens = rhop(ji,jj,jk) / 1000._wp
         zco3(ji,jj,jk) = tr(ji,jj,jk,jpdic,Kbb) * ak13(ji,jj,jk) * ak23(ji,jj,jk) / (zhi(ji,jj,jk)**2   &
           &              + ak13(ji,jj,jk) * zhi(ji,jj,jk) + ak13(ji,jj,jk) * ak23(ji,jj,jk) + rtrn )
         hi  (ji,jj,jk) = zhi(ji,jj,jk) * zdens
      END_3D

      !     ---------------------------------------------------------
      !        CALCULATE DEGREE OF CACO3 SATURATION AND CORRESPONDING
      !        DISSOLOUTION AND PRECIPITATION OF CACO3 (BE AWARE OF
      !        MGCO3)
      !     ---------------------------------------------------------

      zkd = kdca * 0.2**(nca - 0.2)
      DO_3D( 0, 0, 0, 0, 1, jpkm1)

         ! DEVIATION OF [CO3--] FROM SATURATION VALUE
         ! Salinity dependance in zomegaca and divide by rhop to have good units
         zcalcon  = calcon * ( salinprac(ji,jj,jk) / 35._wp )
         zdens    = rhop(ji,jj,jk) / 1000._wp
         zomegaca = ( zcalcon * zco3(ji,jj,jk) ) / ( aksp(ji,jj,jk) * zdens + rtrn )

         ! SET DEGREE OF UNDER-/SUPERSATURATION
         excess(ji,jj,jk) = 1._wp - zomegaca
         zexcess0 = MAX( 0., excess(ji,jj,jk) )

         IF( zomegaca < 0.8 ) THEN
            zexcess = kdca * zexcess0**nca
            ! AMOUNT CACO3 THAT RE-ENTERS SOLUTION
         ELSE
            zexcess = zkd * zexcess0**0.2
         ENDIF

         ! Calcite dissolution
         zdissol  = zexcess * tr(ji,jj,jk,jpcal,Kbb) * rfact2 / rmtss

         !  CHANGE OF [CO3--] , [ALK], PARTICULATE [CACO3],
         !       AND [SUM(CO2)] DUE TO CACO3 DISSOLUTION/PRECIPITATION
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + 2. * zdissol
         tr(ji,jj,jk,jpcal,Krhs) = tr(ji,jj,jk,jpcal,Krhs) -      zdissol
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) +      zdissol
         !
         IF( l_dia )  zcaldiss(ji,jj,jk) = zdissol
         !
      END_3D
      !
      IF( l_dia .AND. knt == nrdttrc ) THEN
         CALL iom_put( "PCAL", prodcal(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )   ! Calcite production
         CALL iom_put( "DCAL", zcaldiss(:,:,:) * 1.e+3 * rfact2r * tmask(A2D(0),:) )  ! calcite dissolution
         CALL iom_put( "PH" , -1. * LOG10( MAX( hi(:,:,:), rtrn ) ) * tmask(A2D(0),:) )  !  PH
         CALL iom_put( "CO3", zco3(:,:,:) * 1.e+3 * tmask(A2D(0),:) )    ! bicarbonate
         CALL iom_put( "CO3sat", aksp(:,:,:) * rhop(A2D(0),:) / 1000._wp &     ! calcite saturation
              &                / calcon * ( salinprac(:,:,:) / 35._wp ) * 1.e+3 * tmask(A2D(0),:)  ) 
         DEALLOCATE( zcaldiss )
      ENDIF
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
        WRITE(charout, FMT="('lys ')")
        CALL prt_ctl_info( charout, cdcomp = 'top' )
        CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_lys')
      !
   END SUBROUTINE p4z_lys


   SUBROUTINE p4z_lys_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_lys_init  ***
      !!
      !! ** Purpose :   Initialization of CaCO3 dissolution parameters
      !!
      !! ** Method  :   Read the nampiscal namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampiscal
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !
      NAMELIST/nampiscal/ kdca, nca
      !!----------------------------------------------------------------------
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_lys_init : initialization of CaCO3 dissolution'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,nampiscal)
      READ_NML_CFG(numnatp,nampiscal)
      IF(lwm) WRITE( numonp, nampiscal )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampiscal'
         WRITE(numout,*) '      diss. rate constant calcite (per month)        kdca =', kdca
         WRITE(numout,*) '      order of reaction for calcite dissolution      nca  =', nca
      ENDIF
      !
      ! Number of seconds per month 
      rmtss =  nyear_len(1) * rday / raamo
      !
      ! CE not really needed ; tempory, shoub be removed when quotan( A2D(0),jpk )
      excess(:,:,:) = 0._wp
      !
   END SUBROUTINE p4z_lys_init

   !!======================================================================
END MODULE p4zlys
