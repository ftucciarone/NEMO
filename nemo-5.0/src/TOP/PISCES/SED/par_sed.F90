MODULE par_sed
   !!======================================================================
   !!                        ***  par_sed  ***
   !! Sediment :   set sediment parameter
   !!======================================================================
   !! History :
   !!        !  06-12  (C. Ethe)  Orignal
   !!----------------------------------------------------------------------

   !! Domain characteristics
   USE par_kind
   USE par_oce , ONLY :       &
      jpi      =>   jpi   ,  & !: first  dimension of grid --> i
      jpj      =>   jpj   ,  & !: second dimension of grid --> j
      jpij     =>   jpij  ,  & !: jpi x jpj
      jp_tem   =>   jp_tem,  & !: indice of temperature
      jp_sal   =>   jp_sal     !: indice of salintity

!   USE par_pisces

   IMPLICIT NONE

   INTEGER, PARAMETER :: jpdta = 17

   ! Vertical sediment geometry
   INTEGER, PUBLIC   ::      &
      jpksed   = 11

   ! sediment tracer species
   INTEGER, PARAMETER ::    &
      jpsol = 11,           &   !: number of solid component
      jpwat = 11,           &   !: number of pore water component
      jpads = 2                 !! number adsorbed species

   
   ! pore water components       
   INTEGER, PARAMETER :: &
      jwoxy  = 1,        & !: oxygen
      jwno3  = 2,        & !: nitrate
      jwpo4  = 3,        & !: phosphate
      jwnh4  = 4,        & !: Ammonium
      jwh2s  = 5,        & !: Sulfate
      jwso4  = 6,        & !: H2S
      jwfe2  = 7,        & !: Fe2+
      jwalk  = 8,        & !: Alkalinity
      jwlgw  = 9,        & !: Alkalinity
      jwdic  = 10,       & !: DIC
      jwsil  = 11          !: Silicate

   ! solid components       
   INTEGER, PARAMETER ::  &
      jsfeo   = 1,        & !: iron hydroxides
      jsfes   = 2,        & !: FeS
      jscal   = 3,        & !: Calcite
      jsopal  = 4,        & !: Opal
      jsclay  = 5,        & !: clay
      jspoc1  = 6,        &
      jspoc2  = 7,        &
      jspoc3  = 8,        &
      jspoc4  = 9,        &
      jspoc5  = 10,       &
      jspoc6  = 11


   INTEGER, PARAMETER ::  &
      jptrased   = jpsol + jpwat , &
      jpvode     = jptrased - 14  , &
      jpdia2dsed = 25

!   REAL(wp), PUBLIC  :: rtrn  = 0.5 * EPSILON( 1.e0 )    !: truncation value

END MODULE par_sed
