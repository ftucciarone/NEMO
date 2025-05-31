MODULE seddta
   !!======================================================================
   !!                     ***  MODULE  seddta  ***
   !! Sediment data  :  read sediment input data from a file
   !!=====================================================================

   !! * Modules used
   USE trc
   USE sed
   USE sedini
   USE phycst, ONLY : rday
   USE iom
   USE lib_mpp         ! distribued memory computing library
   USE trc_oce , ONLY :   l_offline   ! offline flag

   IMPLICIT NONE
   PRIVATE

   !! * Routine accessibility
   PUBLIC sed_dta   ! 

   !! *  Module variables
   REAL(wp) ::  conv2    ! [kg/m2/month]-->[g/cm2/s] ( 1 month has 30 days )

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"

CONTAINS

   !!---------------------------------------------------------------------------
   !!   sed_dta  : read the NetCDF data file in online version using module iom
   !!---------------------------------------------------------------------------

   SUBROUTINE sed_dta( kt, Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_dta  ***
      !!                    
      !! ** Purpose :   Reads data from a netcdf file and 
      !!                initialization of rain and pore water (k=1) components
      !! 
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-04  (C. Ethe)  Re-organization ; Use of iom
      !!----------------------------------------------------------------------

      !! Arguments
      INTEGER, INTENT( in ) ::   kt    ! time-step
      INTEGER, INTENT( in ) ::   Kbb, Kmm ! time level indices

      !! * Local declarations
      INTEGER  ::  ji, jj, js, jw, ikt, itt

      REAL(wp), DIMENSION(jpoce) :: zdtap, zdtag
      REAL(wp), DIMENSION(jpi,jpj) :: zwsbio4, zwsbio3, zddust
      REAL(wp) :: zfact, zdep
      REAL(wp) :: zzf0, zzf1, zzf2, zzf3, zzf4, zzf5, zzf6

      !----------------------------------------------------------------------

      ! Initialization of sediment variable 
      ! Spatial dimension is merged, and unity converted if needed
      !-------------------------------------------------------------

      IF( ln_timing )  CALL timing_start('sed_dta')

      IF (lwp) THEN
         WRITE(numsed,*)
         WRITE(numsed,*) ' sed_dta : Bottom layer fields'
         WRITE(numsed,*) ' ~~~~~~'
         WRITE(numsed,*) ' Data from SMS model'
         WRITE(numsed,*)
      ENDIF


      ! open file
      IF( kt == nitsed000 ) THEN
         IF (lwp) WRITE(numsed,*) ' sed_dta : Sediment fields'
         dtsed = rDt_trc
         conv2 = 1.0e+3 /  1.0e+4 
      ENDIF

      ! reading variables
      IF (lwp) WRITE(numsed,*)
      IF (lwp) WRITE(numsed,*) ' sed_dta : Bottom layer fields at time  kt = ', kt
      ! reading variables
      !
      !    Sinking speeds of detritus is increased with depth as shown
      !    by data and from the coagulation theory
      !    -----------------------------------------------------------
      !
#if defined key_RK3 
      ! Don't consider mid-step values if online coupling
      ! because these are possibly non-monotonic (even with FCT):
      IF ( l_offline ) THEN ; itt = Kmm ; ELSE ; itt = Kbb ; ENDIF
#else
      itt = Kmm
#endif
      !          
      DO_2D( 0, 0, 0, 0 )
         ikt = mbkt(ji,jj)
         zdep = e3t(ji,jj,ikt,Kmm) / rDt_trc
         zwsbio4(ji,jj) = MIN( 0.99 * zdep, wsbio4(ji,jj,ikt) / rday  * 1E3 * 1E-4 )
         zwsbio3(ji,jj) = MIN( 0.99 * zdep, wsbio3(ji,jj,ikt) / rday  * 1E3 * 1E-4 )
      END_2D
      !
      trc_data(:,:,:) = 0.
      DO_2D( 0, 0, 0, 0 )
         ikt = mbkt(ji,jj)
         IF ( sedmask(ji,jj) == 1.0 ) THEN
            trc_data(ji,jj,jwsil) = tr(ji,jj,ikt,jpsil,Kbb)
            trc_data(ji,jj,jwoxy) = tr(ji,jj,ikt,jpoxy,Kbb)
            trc_data(ji,jj,jwdic) = tr(ji,jj,ikt,jpdic,Kbb)
            trc_data(ji,jj,jwno3) = tr(ji,jj,ikt,jpno3,Kbb) * redNo3 / redC
            trc_data(ji,jj,jwpo4) = tr(ji,jj,ikt,jppo4,Kbb) * redPo4 / redC
            trc_data(ji,jj,jwalk) = tr(ji,jj,ikt,jptal,Kbb) 
            trc_data(ji,jj,jwnh4) = tr(ji,jj,ikt,jpnh4,Kbb) * redNo3 / redC 
            trc_data(ji,jj,jwh2s) = 0.0
            trc_data(ji,jj,jwso4) = 0.14 * ts(ji,jj,ikt,jp_sal,itt) / 1.80655 / 96.062
            trc_data(ji,jj,jwfe2) = tr(ji,jj,ikt,jpfer,Kbb)
            trc_data(ji,jj,jwlgw) = 1.E-9
            trc_data(ji,jj,12)    = MIN(tr(ji,jj,ikt,jpgsi,Kbb), 1E-4) * zwsbio4(ji,jj)
            trc_data(ji,jj,13)    = MIN(tr(ji,jj,ikt,jppoc,Kbb), 1E-4) * zwsbio3(ji,jj) &
            &                       + MIN(tr(ji,jj,ikt,jpgoc,Kbb), 1E-4) * zwsbio4(ji,jj)
            trc_data(ji,jj,14)    = MIN(tr(ji,jj,ikt,jpcal,Kbb), 1E-4) * zwsbio4(ji,jj)
            trc_data(ji,jj,15)    = ts(ji,jj,ikt,jp_tem,itt)
            trc_data(ji,jj,16)    = ts(ji,jj,ikt,jp_sal,itt)
            trc_data(ji,jj,17)    = ( tr(ji,jj,ikt,jpsfe,Kbb) * zwsbio3(ji,jj) + tr(ji,jj,ikt,jpbfe,Kbb)  &
            &                       * zwsbio4(ji,jj) ) / ( trc_data(ji,jj,13) + rtrn )
            trc_data(ji,jj,17)    = MIN(1E-3, trc_data(ji,jj,17) )
         ENDIF
      END_2D

      ! Pore water initial concentration [mol/l] in  k=1
      !-------------------------------------------------
      DO jw = 1, jpwat
          pwcp_dta(:,jw) = PACK( trc_data(:,:,jw), sedmask == 1.0 )
      END DO

      !  Solid components : 
      !-----------------------
      !  Sinking fluxes for OPAL in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      rainrg(:,jsopal) = PACK( trc_data(:,:,12), sedmask == 1.0 ) 

      !  Sinking fluxes for POC in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      zdtap = PACK( trc_data(:,:,13), sedmask == 1.0 )
      zzf1 = 0.4471
      zzf2 = 0.3027
      zzf3 = 0.1393
      zzf4 = 0.0618
      zzf5 = 0.0273
      zzf6 = 0.0217
      DO ji = 1, jpoce
         rainrg(ji,jspoc1) = zzf1 * zdtap(ji)
         rainrg(ji,jspoc2) = zzf2 * zdtap(ji)
         rainrg(ji,jspoc3) = zzf3 * zdtap(ji)
         rainrg(ji,jspoc4) = zzf4 * zdtap(ji)
         rainrg(ji,jspoc5) = zzf5 * zdtap(ji)
         rainrg(ji,jspoc6) = zzf6 * zdtap(ji)
      END DO

      !  Sinking fluxes for Calcite in mol.m-2.s-1 ; conversion in mol.cm-2.s-1
      rainrg(:,jscal) = PACK( trc_data(:,:,14), sedmask == 1.0 )

      ! vector temperature [°C] and salinity 
      temp(:) = PACK( trc_data(:,:,15), sedmask == 1.0 )
      salt(:) = PACK( trc_data(:,:,16), sedmask == 1.0 )
      
      ! Clay rain rate in [mol/(cm**2.s)] 
      ! inputs data in [kg.m-2.sec-1] ---> 1e+3/(1e+4) [g.cm-2.s-1]   
      ! divided after by molecular weight g.mol-1      
      rainrg(:,jsclay) = PACK( dust(:,:), sedmask == 1.0 )
      
      zfact = por1(jpksed) * dens_sol(jsclay) / ryear
      DO ji = 1, jpoce
         rainrg(ji,jsclay) = rainrg(ji,jsclay) * conv2 + wacc(ji) * zfact 
         rainrg(ji,jsfeo)  = rainrg(ji,jsclay) / mol_wgt(jsfeo) * 0.035 * 0.5
         rainrg(ji,jsclay) = rainrg(ji,jsclay) / mol_wgt(jsclay) * ( 1.0 - 0.035 * 0.5 ) 
      END DO

      ! Iron monosulphide rain rates. Set to 0
      rainrg(1:jpoce,jsfes)  = 0. 

      ! Fe/C ratio in sinking particles that fall to the sediments
      fecratio(:) = PACK( trc_data(:,:,17), sedmask == 1.0 )

      ! sediment pore water at 1st layer (k=1)
      DO js = 1, jpwat
         pwcp(1:jpoce,1,js) = pwcp_dta(1:jpoce,js)
      END DO

      ! Calculation of raintg of each sol. comp.: rainrg in [g/(cm**2.s)]
      ! computation of dzdep = total thickness of solid material rained [cm] in each cell
      dzdep(:) = 0.
      DO js = 1, jpsol
         zfact = dtsed / ( dens_sol(js) * por1(2) )
         DO ji = 1, jpoce
            rainrg(ji,js) = rainrg(ji,js) * mol_wgt(js)
            dzdep(ji) = dzdep(ji) + rainrg(ji,js) * zfact
         END DO
      END DO
      
      zddust(:,:) = 0._wp

      IF( iom_use("sflxclay" ) ) THEN 
          zddust(:,:) = UNPACK( wacc(:), sedmask == 1.0, 0.0 )
          zddust(:,:) = dust(:,:) + zddust(:,:) / ( rday * 365.0 ) * por1(jpksed) * dens_sol(jsclay) / conv2
          CALL iom_put( "sflxclay", zddust(:,:) * 1E3 / 1.E4 )
      ENDIF
      IF( iom_use("sflxcal" ) )  CALL iom_put( "sflxcal", trc_data(:,:,14) )
      IF( iom_use("sflxbsi" ) )  CALL iom_put( "sflxbsi", trc_data(:,:,12) )
      IF( iom_use("sflxpoc" ) )  CALL iom_put( "sflxpoc", trc_data(:,:,13) )
      IF( iom_use("OceNO3"  ) )  CALL iom_put( "OceNO3",  trc_data(:,:,jwno3) )
      IF( iom_use("OceDIC"  ) )  CALL iom_put( "OceDIC",  trc_data(:,:,jwdic) )
      IF( iom_use("OceALK"  ) )  CALL iom_put( "OceALK",  trc_data(:,:,jwalk) )
      IF( iom_use("OceOXY"  ) )  CALL iom_put( "OceOXY",  trc_data(:,:,jwoxy) )
      IF( iom_use("OceSIL"  ) )  CALL iom_put( "OceSIL",  trc_data(:,:,jwsil) )
      IF( iom_use("OcePO4"  ) )  CALL iom_put( "OcePO4",  trc_data(:,:,jwpo4) )
      IF( iom_use("OceNH4"  ) )  CALL iom_put( "OceNH4",  trc_data(:,:,jwnh4) )
      IF( iom_use("OceSO4"  ) )  CALL iom_put( "OceSO4",  trc_data(:,:,jwso4) )
      IF( iom_use("OceFE2"  ) )  CALL iom_put( "OceFE2",  trc_data(:,:,jwfe2) )
      IF( iom_use("OceTEM"  ) )  CALL iom_put( "OceTEM",  trc_data(:,:,15) )
      IF( iom_use("OceSAL"  ) )  CALL iom_put( "OceSAL",  trc_data(:,:,16) )
      IF( iom_use("OceSFE"  ) )  CALL iom_put( "OceSFE",  trc_data(:,:,17) )

      IF( ln_timing )  CALL timing_stop('sed_dta')
      
   END SUBROUTINE sed_dta

END MODULE seddta
