MODULE sedinitrc
   !!======================================================================
   !!              ***  MODULE  sedinitrc  ***
   !! Sediment : define sediment variables
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   sed_init    : initialization, namelist read, and parameters control
   !!----------------------------------------------------------------------
   !! * Modules used
   USE sed     ! sediment global variable
   USE sedini
   USE seddta
   USE sedrst
   USE sedco3
   USE sedchem
   USE sed_oce
   USE lib_mpp         ! distribued memory computing library


   IMPLICIT NONE
   PRIVATE

   !! *  Routine accessibility
   PUBLIC sed_initrc          ! routine called by opa.F90

CONTAINS


   SUBROUTINE sed_initrc( Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_init  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - Reading namelist
      !!               - Read the deepest water layer thickness
      !!                 ( using as mask ) in Netcdf file
      !!               - Convert unity if necessary
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!               - sets sediment grid, porosity and others constants
      !!
      !!   History :
      !!        !  04-10  (N. Emprin, M. Gehlen )  Original code
      !!        !  06-07  (C. Ethe)  Re-organization
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kbb, Kmm  ! time level indices
      INTEGER :: ji, jj, ikt
      !!----------------------------------------------------------------------


      ! Initialize the sediment tracers concentrations
      !------------------------------------------------

      IF(lwp) WRITE(numsed,*) ' sed_initrc : Initialization of sediment concentration '
      IF(lwp) WRITE(numsed,*) ' '

      ! Determination of sediments number of points and allocate global variables

      IF(lwp) WRITE(numsed,*) ' nit000 = ', nit000, '  nitrc000 = ', nittrc000, '  nitsed000 = ', nitsed000
      IF(lwp) WRITE(numsed,*) ' '

      ! sets initial sediment composition
      ! ( only clay or reading restart file )
      !---------------------------------------
      CALL sed_init_data( Kbb, Kmm )


      CALL sed_init_wri


   END SUBROUTINE sed_initrc


   SUBROUTINE sed_init_data( Kbb, Kmm )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE sed_init_data  ***
      !!
      !! ** Purpose :  Initialization of sediment module
      !!               - sets initial sediment composition
      !!                 ( only clay or reading restart file )
      !!
      !!   History :
      !!        !  06-07  (C. Ethe)  original
      !!----------------------------------------------------------------------

      INTEGER, INTENT(in) ::   Kbb, Kmm  ! time level indices

      ! local variables
      INTEGER :: ji, jk, zhipor

      !--------------------------------------------------------------------
 

      IF( .NOT. ln_rst_sed ) THEN

         IF (lwp) WRITE(numsed,*) ' Initilization of default values of sediment components'

         ! default values for initial pore water concentrations [mol/l]
         pwcp(:,:,:) = 0.
         ! default value for initial solid component (fraction of dry weight dim=[0])
         ! clay
         solcp(:,:,:) = 0.
         solcp(:,2:jpksed,jsclay) = dens_sol(jsclay) * ( 1.0 - 0.035 * 0.5 )
         solcp(:,2:jpksed,jsfeo)  = dens_sol(jsfeo) * 0.035 * 0.5
         burial(:,:) = 0.0

         ! Initialization of [h+] and [co3--]

         zhipor = 8.0
         ! Initialization of [h+] in mol/kg
         DO jk = 1, jpksed
            DO ji = 1, jpoce
               hipor (ji,jk) = 10.**( -1. * zhipor )
            ENDDO
         ENDDO

         co3por(:,:) = 1E-6

      ELSE   
  
         IF (lwp) WRITE(numsed,*) ' Initilization of Sediment components from restart'

         CALL sed_rst_cal( nitsed000, 'READ' )
         CALL sed_rst_read

      ENDIF

!      dtsed = rDt_trc

      ! Load initial Pisces Data for bot. wat. Chem and fluxes
      CALL sed_dta ( nitsed000, Kbb, Kmm ) 

      ! Initialization of chemical constants
      CALL sed_chem ( nitsed000 )

      ! Conversion of [h+] in mol/Kg to get it in mol/l ( multiplication by density)
      DO jk = 1, jpksed
         hipor(1:jpoce,jk) = hipor(1:jpoce,jk) * densSW(1:jpoce)
      ENDDO


      ! In default case - no restart - sedco3 is run to initiate [h+] and [co32-]
      ! Otherwise initiate values of pH and co3 read in restart
      IF( .NOT. ln_rst_sed ) THEN
         ! sedco3 is run to initiate[h+] [co32-] in mol/l of solution
         CALL sed_co3 ( nitsed000 )

      ENDIF
            
   END SUBROUTINE sed_init_data

   SUBROUTINE sed_init_wri

      INTEGER :: jk

      IF (lwp) THEN
         WRITE(numsed,*)' '
         WRITE(numsed,*)'======== Write summary of sediment char.  ============'
         WRITE(numsed,*)' '
         WRITE(numsed,*)' '
         WRITE(numsed,*)'-------------------------------------------------------------------'
         WRITE(numsed,*)' Initial Conditions '
         WRITE(numsed,*)'-------------------------------------------------------------------'
         WRITE(numsed,*)'dzm = dzkbot minimum to calculate ', 0.
         WRITE(numsed,*)'Local zone : jpi, jpj, jpksed : ',jpi, jpj, jpksed
         WRITE(numsed,*)'jpoce = ',jpoce,' nbtot pts = ',jpij,' nb earth pts = ',jpij - jpoce
         WRITE(numsed,*)'sublayer thickness dz(1) [cm] : ', dz(1)
         WRITE(numsed,*)'Vertical domain of the sediment'
         WRITE(numsed,*)'-------------------------------'
         WRITE(numsed,*)' Indice, profsed, dz'
         DO jk = 2, jpksed
            WRITE(numsed,*) jk,profsed(jk),dz(jk) 
         END DO
         WRITE(numsed,*)' nb solid comp : ',jpsol
         WRITE(numsed,*)'1=FeO,2=FeS,3=CaCO3,4=Opal, 5=clay, (>5)=POC'
         WRITE(numsed,*)'weight mol 1,2,3,4,5,6,7'
         WRITE(numsed,'(6(F0.2,3X))')mol_wgt(jsfeo),mol_wgt(jsfes),mol_wgt(jscal),mol_wgt(jsopal),mol_wgt(jsclay),mol_wgt(jspoc2)
         WRITE(numsed,*)'nb dissolved comp',jpwat
         WRITE(numsed,*)'1=O2,,2=NO3,3=PO4,4=NH4,5=H2S,6=SO4,7=Fe2,8=ALK,9=LGW,10=DIC,11=Si'
         WRITE(numsed,*)'redfield coef C,O,N P Dit '
         WRITE(numsed,'(5(F0.2,3X))')1./spo4r,so2ut/spo4r,srno3/spo4r,spo4r/spo4r,srDnit/spo4r
         WRITE(numsed,*) ' '
         WRITE(numsed,*) ' End Of Initialization '
         WRITE(numsed,*) ' '
      ENDIF
!
   END SUBROUTINE sed_init_wri

END MODULE sedinitrc
