MODULE p2zmort
   !!======================================================================
   !!                         ***  MODULE p2zmort  ***
   !! TOP :   PISCES Compute the mortality terms for phytoplankton
   !!======================================================================
   !! History :   1.0  !  2002     (O. Aumont)  Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
   !!   p4z_mort       : Compute the mortality terms for phytoplankton
   !!   p4z_mort_init  : Initialize the mortality params for phytoplankton
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE p2zlim          ! Phytoplankton limitation terms
   USE prtctl          ! print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p2z_mort           ! Called from p4zbio.F90 
   PUBLIC   p2z_mort_init      ! Called from trcini_pisces.F90 

   REAL(wp), PUBLIC ::   wchln    !: Quadratic mortality rate of nanophytoplankton
   REAL(wp), PUBLIC ::   mpratn   !: Linear mortality rate of nanophytoplankton

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_mort( kt, Kbb, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_mort_nano  ***
      !!
      !! ** Purpose :   Compute the mortality terms for nanophytoplankton
      !!
      !! ** Method  :   Both quadratic and simili linear mortality terms
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Krhs  ! time level indices
      !!---------------------------------------------------------------------
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zcompaph, zprcaca
      REAL(wp) ::   ztortp , zrespp , zmortp, zlim1, zlim2 
      CHARACTER (len=25) ::   charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_mort')
      !
      prodcal(:,:,:) = 0._wp   ! calcite production variable set to zero
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zcompaph = MAX( ( tr(ji,jj,jk,jpphy,Kbb) - 1e-9 ), 0.e0 )

         ! Quadratic mortality of nano due to aggregation during
         ! blooms (Doney et al. 1996)
         ! -----------------------------------------------------
         zlim2   = xlimphy(ji,jj,jk) * xlimphy(ji,jj,jk)
         zlim1   = 0.0625 / ( 0.0625 + zlim2 ) * tr(ji,jj,jk,jpphy,Kbb)
         zrespp  = wchln * 1.e6 * xstep * zlim1 * xdiss(ji,jj,jk) * zcompaph

         ! Phytoplankton linear mortality
         ! A michaelis-menten like term is introduced to avoid 
         ! extinction of nanophyto in highly limited areas
         ! ----------------------------------------------------
         ztortp = mpratn * tgfunc(ji,jj,jk) * xstep * zlim1 / ( xkmort + tr(ji,jj,jk,jpphy,Kbb) ) * zcompaph
         zmortp = zrespp + ztortp
         
         !   Update the arrays TRA which contains the biological sources and sinks
         tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs) - zmortp

         ! Production PIC particles due to mortality
         zprcaca = xfracal(ji,jj,jk) * zmortp
         prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)

         ! POC associated with the shell is supposed to be routed to 
         ! big particles because of the ballasting effect
         tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zprcaca
         tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - 2. * zprcaca
         tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zrespp
         tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + ztortp
         prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + zrespp
         !
         tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + ztortp * feratz
         !
      END_3D
      !
       IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('nano')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
       ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p2z_mort')
      !
   END SUBROUTINE p2z_mort

   SUBROUTINE p2z_mort_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p2z_mort_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton parameters
      !!
      !! ** Method  :   Read the namp4zmort namelist and check the parameters
      !!              called at the first timestep
      !!
      !! ** input   :   Namelist namp2zmort
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer
      !
      NAMELIST/namp2zmort/ wchln, mpratn
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*) 
         WRITE(numout,*) 'p2z_mort_init : Initialization of phytoplankton mortality parameters'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,namp2zmort)
      READ_NML_CFG(numnatp,namp2zmort)
      IF(lwm) WRITE( numonp, namp2zmort )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : namp4zmort'
         WRITE(numout,*) '      quadratic mortality of phytoplankton        wchln  =', wchln
         WRITE(numout,*) '      phytoplankton mortality rate                mpratn =', mpratn
      ENDIF
      !
   END SUBROUTINE p2z_mort_init

   !!======================================================================
END MODULE p2zmort
