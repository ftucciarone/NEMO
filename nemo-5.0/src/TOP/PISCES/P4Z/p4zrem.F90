MODULE p4zrem
   !!======================================================================
   !!                         ***  MODULE p4zrem  ***
   !! TOP :   PISCES Compute remineralization/dissolution of organic compounds
   !!         except for POC which is treated in p4zpoc.F90
   !!         This module is common to both PISCES and PISCES-QUOTA
   !!=========================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!----------------------------------------------------------------------
   !!   p4z_rem       :  Compute remineralization/dissolution of organic compounds
   !!   p4z_rem_init  :  Initialisation of parameters for remineralisation
   !!   p4z_rem_alloc :  Allocate remineralisation variables
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zche          !  chemical model
   USE p4zprod         !  Growth rate of the 2 phyto groups
   USE p2zlim          !  Nutrient limitation terms
   USE p4zlim          !  Nutrient limitation terms
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager


   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_rem         ! called in p4zbio.F90
   PUBLIC   p2z_rem
   PUBLIC   p4z_rem_init    ! called in trcini_pisces.F90
   PUBLIC   p4z_rem_alloc   ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::   xremikc    !: remineralisation rate of DOC (p5z) 
   REAL(wp), PUBLIC ::   xremikn    !: remineralisation rate of DON (p5z) 
   REAL(wp), PUBLIC ::   xremikp    !: remineralisation rate of DOP (p5z) 
   REAL(wp), PUBLIC ::   nitrif     !: NH4 nitrification rate 
   REAL(wp), PUBLIC ::   xsirem     !: remineralisation rate of biogenic silica
   REAL(wp), PUBLIC ::   xsiremlab  !: fast remineralisation rate of BSi
   REAL(wp), PUBLIC ::   xsilab     !: fraction of labile biogenic silica 
   REAL(wp), PUBLIC ::   feratb     !: Fe/C quota in bacteria
   REAL(wp), PUBLIC ::   xkferb     !: Half-saturation constant for bacterial Fe/C
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: denitr   !: denitrification array

   LOGICAL         :: l_dia_remin, l_dia_bact, l_dia_denit
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_rem( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_rem  ***
      !!
      !! ** Purpose :   Compute remineralization/dissolution of organic compounds
      !!                Computes also nitrification of ammonium 
      !!                The solubilization/remineralization of POC is treated 
      !!                in p4zpoc.F90. The dissolution of calcite is processed
      !!                in p4zlys.F90. 
      !!
      !! ** Method  : - Bacterial biomass is computed implicitely based on a 
      !!                parameterization developed from an explicit modeling
      !!                of PISCES in an alternative version 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt         ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zremik, zremikc, ztemp
      REAL(wp) ::   zdep, zdepmin, zfactdep
      REAL(wp) ::   zammonic, zoxyremc, zolimic
      !
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(A2D(0),jpk) :: zdepbac
      REAL(wp), DIMENSION(A2D(0)    ) :: ztempbac
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::  zolimi
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_rem')
      !
      IF( kt == nittrc000 )  THEN
         l_dia_remin  = iom_use( "REMIN" )
         l_dia_denit  = iom_use( "DENIT" ) 
         l_dia_bact   = iom_use( "BACT" )
      ENDIF
      IF( l_dia_remin ) THEN
         ALLOCATE( zolimi(A2D(0),jpk) )    ;   zolimi(A2D(0),:) = tr(A2D(0),:,jpoxy,Krhs)
      ENDIF

      ! Computation of the mean bacterial concentration
      ! this parameterization has been deduced from a model version
      ! that was modeling explicitely bacteria. This is a very old parame
      ! that will be very soon updated based on results from a much more
      ! recent version of PISCES with bacteria.
      ! ----------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zdep = MAX( hmld(ji,jj), heup_01(ji,jj), gdept(ji,jj,1,Kmm) )
         IF ( gdept(ji,jj,jk,Kmm) <= zdep ) THEN
            zdepbac(ji,jj,jk) = 0.6 * ( tr(ji,jj,jk,jpzoo,Kbb) * 1.0E6 )**0.6 * 1.E-6
            ztempbac(ji,jj)   = zdepbac(ji,jj,jk)
         ELSE
            zdepmin = zdep / gdept(ji,jj,jk,Kmm)
            zdepbac(ji,jj,jk) = zdepmin**0.683 * ztempbac(ji,jj)
         ENDIF
      END_3D

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ! DOC ammonification. Depends on depth, phytoplankton biomass
         ! and a limitation term which is supposed to be a parameterization of the bacterial activity. 
         ! --------------------------------------------------------------------------
         zremik   = xstep / 1.e-6 * xlimbac(ji,jj,jk) * zdepbac(ji,jj,jk)
         zremik   = MAX( zremik, 2.74e-4 * xstep / xremikc )
         zremikc  = xremikc * zremik

         ! Ammonification in oxic waters with oxygen consumption
         ! -----------------------------------------------------
         zolimic  = zremikc * ( 1.- nitrfac(ji,jj,jk) ) * tr(ji,jj,jk,jpdoc,Kbb)
         zolimic  = MAX(0., MIN( ( tr(ji,jj,jk,jpoxy,Kbb) - rtrn ) / o2ut, zolimic ) )

         ! Ammonification in suboxic waters with denitrification
         ! -----------------------------------------------------
         zammonic = zremikc * nitrfac(ji,jj,jk) * tr(ji,jj,jk,jpdoc,Kbb)
         denitr(ji,jj,jk)  = zammonic * ( 1. - nitrfac2(ji,jj,jk) )
         denitr(ji,jj,jk)  = MAX(0., MIN(  ( tr(ji,jj,jk,jpno3,Kbb) - rtrn ) / rdenit, denitr(ji,jj,jk) ) )

         ! Ammonification in waters depleted in O2 and NO3 based on 
         ! other redox processes
         ! --------------------------------------------------------
         zoxyremc = MAX(0., zammonic - denitr(ji,jj,jk) )

         ! Update of the the trends arrays
         ztemp    = zolimic + denitr(ji,jj,jk) + zoxyremc 
         tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) - denitr (ji,jj,jk) * rdenit
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) - ztemp
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - zolimic * (o2ut + o2nit)
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + ztemp
         tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) + ztemp
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - rno3 * ( zolimic + zoxyremc - ( rdenit - 1.) * denitr(ji,jj,jk) )
      END_3D

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem1')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( knt == nrdttrc ) THEN
          !
          IF( l_dia_remin ) THEN    ! Remineralisation rate
             CALL iom_put( "REMIN", ( zolimi(:,:,:) - tr(A2D(0),:,jpoxy,Krhs) ) / o2ut * rfact2r * tmask(A2D(0),:) )
             DEALLOCATE( zolimi )
          ENDIF
          IF( l_dia_bact )  THEN
            zdepbac(:,:,jpk) = 0._wp
            CALL iom_put( "BACT", zdepbac(:,:,:) * 1.E6 * tmask(A2D(0),:) )  ! Bacterial biomass 
          ENDIF
          IF( l_dia_denit )  CALL iom_put( "DENIT", denitr(:,:,:) * 1.E3 * rfact2r * rno3 * tmask(A2D(0),:) ) ! Denitrification
          !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p2z_rem')
      !
   END SUBROUTINE p2z_rem

   SUBROUTINE p4z_rem( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_rem  ***
      !!
      !! ** Purpose :   Compute remineralization/dissolution of organic compounds
      !!                Computes also nitrification of ammonium 
      !!                The solubilization/remineralization of POC is treated 
      !!                in p4zpoc.F90. The dissolution of calcite is processed
      !!                in p4zlys.F90. 
      !!
      !! ** Method  : - Bacterial biomass is computed implicitely based on a 
      !!                parameterization developed from an explicit modeling
      !!                of PISCES in an alternative version 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt         ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zremik, zremikc, zremikn, zremikp, zsiremin
      REAL(wp) ::   zsatur, zsatur2, znusil, znusil2, zdep, zdepmin, zfactdep
      REAL(wp) ::   zbactfer, zonitr
      REAL(wp) ::   zammonic, zoxyremc, zosil, ztem, zdenitnh4, zolimic
      REAL(wp) ::   zfacsi, ztemp
      !
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(A2D(0),jpk) :: zdepbac, zdepeff, zfacsib
      REAL(wp), DIMENSION(A2D(0)    ) :: ztempbac
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zolimi, zfebact
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_rem')
      !
      IF( kt == nittrc000 )  THEN
         l_dia_remin  = iom_use( "REMIN" )  .OR. iom_use( "Remino2" )
         l_dia_bact   = iom_use( "FEBACT" ) .OR. iom_use( "BACT" )
         l_dia_denit  = iom_use( "DENIT" )
      ENDIF
      IF( l_dia_remin ) THEN
         ALLOCATE( zolimi(A2D(0),jpk) ) ;   zolimi(A2D(0),:) = tr(A2D(0),:,jpoxy,Krhs)
      ENDIF
      IF( l_dia_bact ) THEN
         ALLOCATE( zfebact(A2D(0),jpk) ) ;   zfebact(A2D(0),:) = tr(A2D(0),:,jpfer,Krhs)  ;  zdepbac(:,:,jpk) = 0._wp
      ENDIF

      ! Initialisation of arrays
      zfacsib(:,:,:)  = xsilab / ( 1.0 - xsilab )

      ! Computation of the mean bacterial concentration
      ! this parameterization has been deduced from a model version
      ! that was modeling explicitely bacteria. This is a very old parame
      ! that will be very soon updated based on results from a much more
      ! recent version of PISCES with bacteria.
      ! ----------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zdep = MAX( hmld(ji,jj), heup_01(ji,jj), gdept(ji,jj,1,Kmm) )
         IF ( gdept(ji,jj,jk,Kmm) <= zdep ) THEN
            zdepbac(ji,jj,jk) = 0.6 * ( ( tr(ji,jj,jk,jpzoo,Kbb) + tr(ji,jj,jk,jpmes,Kbb) ) * 1.0E6 )**0.6 * 1.E-6
            ztempbac(ji,jj)   = zdepbac(ji,jj,jk)
            zdepeff(ji,jj,jk) = 0.3
         ELSE
            zdepmin           = zdep / gdept(ji,jj,jk,Kmm)
            zdepbac(ji,jj,jk) = zdepmin**0.73 * ztempbac(ji,jj)
            zdepeff(ji,jj,jk) = 0.3 * zdepmin**0.8
         ENDIF
      END_3D

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ! DOC ammonification. Depends on depth, phytoplankton biomass
         ! and a limitation term which is supposed to be a parameterization of the bacterial activity. 
         ! --------------------------------------------------------------------------
         zremik   = xstep * 1.e6 * xlimbac(ji,jj,jk) * zdepbac(ji,jj,jk) 
         zremik   = MAX( zremik, 2.74e-4 * xstep / xremikc )
         zremikc  = xremikc * zremik

         ! Ammonification in oxic waters with oxygen consumption
         ! -----------------------------------------------------
         zolimic  = zremikc * ( 1.- nitrfac(ji,jj,jk) ) * tr(ji,jj,jk,jpdoc,Kbb) 
         zolimic  = MAX(0., MIN( ( tr(ji,jj,jk,jpoxy,Kbb) - rtrn ) / o2ut, zolimic ) ) 

         ! Ammonification in suboxic waters with denitrification
         ! -----------------------------------------------------
         zammonic = zremikc * nitrfac(ji,jj,jk) * tr(ji,jj,jk,jpdoc,Kbb)
         denitr(ji,jj,jk)  = zammonic * ( 1. - nitrfac2(ji,jj,jk) )
         denitr(ji,jj,jk)  = MAX(0., MIN( ( tr(ji,jj,jk,jpno3,Kbb) - rtrn ) / rdenit, denitr(ji,jj,jk) ) )

         ! Ammonification in waters depleted in O2 and NO3 based on 
         ! other redox processes
         ! --------------------------------------------------------
         zoxyremc = zammonic - denitr(ji,jj,jk)

         ! Update of the the trends arrays
         ztemp    = zolimic + denitr(ji,jj,jk) + zoxyremc
         tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) - denitr (ji,jj,jk) * rdenit
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) - ztemp
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - zolimic * o2ut
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) + ztemp
         IF( ln_p4z ) THEN ! PISCES-std
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + ztemp
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + ztemp
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * ( ztemp + rdenit * denitr(ji,jj,jk) )
         ELSE  ! PISCES-QUOTA (p5z)
            zremikn = xremikn / xremikc * tr(ji,jj,jk,jpdon,kbb) / ( tr(ji,jj,jk,jpdoc,Kbb) + rtrn )
            zremikp = xremikp / xremikc * tr(ji,jj,jk,jpdop,Kbb) / ( tr(ji,jj,jk,jpdoc,Kbb) + rtrn )
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + zremikp * ztemp
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zremikn * ztemp
            tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) - zremikn * ztemp
            tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) - zremikp * ztemp
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zremikn * ( ztemp + rdenit * denitr(ji,jj,jk) )
         ENDIF
      END_3D

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ! NH4 nitrification to NO3. Ceased for oxygen concentrations
         ! below 2 umol/L. Inhibited at strong light 
         ! ----------------------------------------------------------
         zonitr  = nitrif * xstep * tr(ji,jj,jk,jpnh4,Kbb) * ( 1.- nitrfac(ji,jj,jk) )  &
         &         / ( 1.+ emoy(ji,jj,jk) ) * ( 1. + fr_i(ji,jj) * emoy(ji,jj,jk) ) 
         zdenitnh4 = nitrif * xstep * tr(ji,jj,jk,jpnh4,Kbb) * nitrfac(ji,jj,jk)
         zdenitnh4 = MAX(0., MIN(  ( tr(ji,jj,jk,jpno3,Kbb) - rtrn ) / rdenita, zdenitnh4 ) )
         ! Update of the tracers trends
         ! ----------------------------
         tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) - zonitr - zdenitnh4
         tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) + zonitr - rdenita * zdenitnh4
         tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) - o2nit * zonitr
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - 2 * rno3 * zonitr + rno3 * ( rdenita - 1. ) * zdenitnh4
      END_3D

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem1')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ! Bacterial uptake of iron. No iron is available in DOC. So
         ! Bacteries are obliged to take up iron from the water. Some
         ! studies (especially at Papa) have shown this uptake to be significant
         ! ----------------------------------------------------------
         zbactfer = feratb * 0.4_wp * xstep * tgfunc(ji,jj,jk) * xlimbacl(ji,jj,jk) * biron(ji,jj,jk)    &
           &        / ( xkferb + biron(ji,jj,jk) ) * zdepeff(ji,jj,jk) * zdepbac(ji,jj,jk)
         
         ! Only the transfer of iron from its dissolved form to particles
         ! is treated here. The GGE of bacteria supposed to be equal to 
         ! 0.33. This is hard-coded. 
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - zbactfer * 0.1
         tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + zbactfer * 0.08
         tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + zbactfer * 0.02
         blim(ji,jj,jk)          = xlimbacl(ji,jj,jk)  * zdepbac(ji,jj,jk) / 1.e-6
      END_3D

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem2')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF

      ! Initialization of the array which contains the labile fraction
      ! of bSi. Set to a constant in the upper ocean
      ! ---------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ! Remineralization rate of BSi dependent on T and saturation
         ! The parameterization is taken from Ridgwell et al. (2002) 
         ! ---------------------------------------------------------
         zdep     = MAX( hmld(ji,jj), heup_01(ji,jj), gdept(ji,jj,1,Kmm) )
         zsatur   = MAX( rtrn, ( sio3eq(ji,jj,jk) - tr(ji,jj,jk,jpsil,Kbb) ) / ( sio3eq(ji,jj,jk) + rtrn ) )
         zsatur2  = ( 1. + ts(ji,jj,jk,jp_tem,Kmm) / 400.)**37
         znusil   = 0.225 * ( 1. + ts(ji,jj,jk,jp_tem,Kmm) / 15.) * zsatur + 0.775 * zsatur2 * zsatur**9 * SQRT(SQRT(zsatur))
 
         ! Two fractions of bSi are considered : a labile one and a more
         ! refractory one based on the commonly observed two step 
         ! dissolution of bSi (initial rapid dissolution followed by 
         ! more slowly dissolution).
         ! Computation of the vertical evolution of the labile fraction
         ! of bSi. This is computed assuming steady state.
         ! --------------------------------------------------------------
         zfacsi = xsilab
         IF ( gdept(ji,jj,jk,Kmm) >= zdep ) THEN
            zfactdep = EXP( -0.5 * ( xsiremlab - xsirem ) * znusil * e3t(ji,jj,jk,Kmm) / wsbio4(ji,jj,jk) )
            zfacsib(ji,jj,jk) = zfacsib(ji,jj,jk-1) * zfactdep
            zfacsi            = zfacsib(ji,jj,jk) / ( 1.0 + zfacsib(ji,jj,jk) )
            zfacsib(ji,jj,jk) = zfacsib(ji,jj,jk) * zfactdep
         ENDIF
         zsiremin = ( xsiremlab * zfacsi + xsirem * ( 1. - zfacsi ) ) * xstep * znusil
         zosil    = zsiremin * tr(ji,jj,jk,jpgsi,Kbb)
         !
         tr(ji,jj,jk,jpgsi,Krhs) = tr(ji,jj,jk,jpgsi,Krhs) - zosil
         tr(ji,jj,jk,jpsil,Krhs) = tr(ji,jj,jk,jpsil,Krhs) + zosil
      END_3D

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem3')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF

      IF( knt == nrdttrc ) THEN
          !
          IF( l_dia_remin ) THEN   
             CALL iom_put( "REMIN", ( zolimi(:,:,:) - tr(A2D(0),:,jpoxy,Krhs) ) / o2ut * rfact2r * tmask(A2D(0),:) ) ! Remineralisation rate
             CALL iom_put( "Remino2", -1. * ( zolimi(:,:,:) - tr(A2D(0),:,jpoxy,Krhs) ) * rfact2r * tmask(A2D(0),:) ) ! O2 consumption by nitrification
             DEALLOCATE( zolimi )
          ENDIF
          IF( l_dia_bact )  THEN
              CALL iom_put( "BACT", zdepbac(:,:,:) * 1.E6 * tmask(A2D(0),:) )  ! Bacterial biomass
              CALL iom_put( "FEBACT", ( zfebact(:,:,:) - tr(A2D(0),:,jpfer,Krhs) ) * 1e9 * rfact2r * tmask(A2D(0),:) )
             DEALLOCATE( zfebact )
          ENDIF
          IF( l_dia_denit )  CALL iom_put( "DENIT", denitr(:,:,:) * 1.E3 * rfact2r * rno3 * tmask(A2D(0),:) ) ! Denitrification
          !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_rem')
      !
   END SUBROUTINE p4z_rem


   SUBROUTINE p4z_rem_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_rem_init  ***
      !!
      !! ** Purpose :   Initialization of remineralization parameters
      !!
      !! ** Method  :   Read the nampisrem namelist and check the parameters
      !!      called at the first timestep
      !!
      !! ** input   :   Namelist nampisrem
      !!
      !!----------------------------------------------------------------------
      NAMELIST/nampisrem/nitrif, xsirem, xsiremlab, xsilab, feratb, xkferb, & 
         &               xremikc, xremikn, xremikp
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_rem_init : Initialization of remineralization parameters'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,nampisrem)
      READ_NML_CFG(numnatp,nampisrem)
      IF(lwm) WRITE( numonp, nampisrem )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist parameters for remineralization, nampisrem'
         WRITE(numout,*) '      remineralization rate of DOC              xremikc   =', xremikc
         IF( ln_p5z ) THEN 
            WRITE(numout,*) '      remineralization rate of DOC              xremikc   =', xremikc
            WRITE(numout,*) '      remineralization rate of DON              xremikn   =', xremikn
            WRITE(numout,*) '      remineralization rate of DOP              xremikp   =', xremikp
         ENDIF
         IF( ln_p5z .OR. ln_p4z ) THEN
            WRITE(numout,*) '      remineralization rate of Si               xsirem    =', xsirem
            WRITE(numout,*) '      fast remineralization rate of Si          xsiremlab =', xsiremlab
            WRITE(numout,*) '      fraction of labile biogenic silica        xsilab    =', xsilab
            WRITE(numout,*) '      NH4 nitrification rate                    nitrif    =', nitrif
            WRITE(numout,*) '      Bacterial Fe/C ratio                      feratb    =', feratb
            WRITE(numout,*) '      Half-saturation constant for bact. Fe/C   xkferb    =', xkferb
         ENDIF
      ENDIF
      !
      denitr(:,:,jpk) = 0._wp
      blim  (:,:,jpk) = 0._wp
      !
   END SUBROUTINE p4z_rem_init


   INTEGER FUNCTION p4z_rem_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_rem_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( denitr(A2D(0),jpk), STAT=p4z_rem_alloc )
      !
      IF( p4z_rem_alloc /= 0 )   CALL ctl_stop( 'STOP', 'p4z_rem_alloc: failed to allocate arrays' )
      !
   END FUNCTION p4z_rem_alloc

   !!======================================================================
END MODULE p4zrem
