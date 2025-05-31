MODULE p5zlim
   !!======================================================================
   !!                         ***  MODULE p5zlim  ***
   !! TOP :   PISCES-QUOTA : Computes the various nutrient limitation terms
   !!                        of phytoplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-04  (O. Aumont, C. Ethe) Limitation for iron modelled in quota 
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_lim        :   Compute the nutrients limitation terms 
   !!   p5z_lim_init   :   Read the namelist 
   !!----------------------------------------------------------------------
   USE oce_trc         ! Shared ocean-passive tracers variables
   USE trc             ! Tracers defined
   USE p2zlim          ! Nutrient limitation
   USE p4zlim          ! Nutrient limitation 
   USE sms_pisces      ! PISCES variables
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC p5z_lim           ! called in p4zbio.F90  
   PUBLIC p5z_lim_init      ! called in trcsms_pisces.F90 
   PUBLIC p5z_lim_alloc     ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  concpno3    !:  NO3 half saturation for picophyto  
   REAL(wp), PUBLIC ::  concpnh4    !:  NH4 half saturation for picophyto
   REAL(wp), PUBLIC ::  concnpo4    !:  PO4 half saturation for nanophyto
   REAL(wp), PUBLIC ::  concppo4    !:  PO4 half saturation for picophyto
   REAL(wp), PUBLIC ::  concdpo4    !:  PO4 half saturation for diatoms
   REAL(wp), PUBLIC ::  concpfer    !:  Iron half saturation for picophyto
   REAL(wp), PUBLIC ::  concbpo4    !:  PO4 half saturation for bacteria
   REAL(wp), PUBLIC ::  xsizepic    !:  Minimum size criteria for picophyto
   REAL(wp), PUBLIC ::  xsizerp     !:  Size ratio for picophytoplankton
   REAL(wp), PUBLIC ::  qfnopt      !:  optimal Fe quota for nanophyto
   REAL(wp), PUBLIC ::  qfpopt      !:  optimal Fe quota for picophyto
   REAL(wp), PUBLIC ::  qfdopt      !:  optimal Fe quota for diatoms
   REAL(wp), PUBLIC ::  qnnmin      !:  minimum N  quota for nanophyto
   REAL(wp), PUBLIC ::  qnnmax      !:  maximum N quota for nanophyto
   REAL(wp), PUBLIC ::  qpnmin      !:  minimum P quota for nanophyto
   REAL(wp), PUBLIC ::  qpnmax      !:  maximum P quota for nanophyto
   REAL(wp), PUBLIC ::  qnpmin      !:  minimum N quota for nanophyto
   REAL(wp), PUBLIC ::  qnpmax      !:  maximum N quota for nanophyto
   REAL(wp), PUBLIC ::  qppmin      !:  minimum P quota for nanophyto
   REAL(wp), PUBLIC ::  qppmax      !:  maximum P quota for nanophyto
   REAL(wp), PUBLIC ::  qndmin      !:  minimum N quota for diatoms
   REAL(wp), PUBLIC ::  qndmax      !:  maximum N quota for diatoms
   REAL(wp), PUBLIC ::  qpdmin      !:  minimum P quota for diatoms
   REAL(wp), PUBLIC ::  qpdmax      !:  maximum P quota for diatoms
   REAL(wp), PUBLIC ::  qfnmax      !:  maximum Fe quota for nanophyto
   REAL(wp), PUBLIC ::  qfpmax      !:  maximum Fe quota for picophyto
   REAL(wp), PUBLIC ::  qfdmax      !:  maximum Fe quota for diatoms
   REAL(wp), PUBLIC ::  xpsinh4     !:  respiration cost of NH4 assimilation
   REAL(wp), PUBLIC ::  xpsino3     !:  respiration cost of NO3 assimilation
   REAL(wp), PUBLIC ::  xpsiuptk    !:  Mean respiration cost

   !!*  Allometric variations of the quotas
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnnmin    !: Minimum N quota of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnnmax    !: Maximum N quota of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpnmin    !: Minimum P quota of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpnmax    !: Maximum P quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnpmin    !: Minimum N quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqnpmax    !: Maximum N quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqppmin    !: Minimum P quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqppmax    !: Maximum P quota of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqndmin    !: Minimum N quota of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqndmax    !: Maximum N quota of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpdmin    !: Minimum P quota of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE,   DIMENSION(:,:,:)  ::   xqpdmax    !: Maximum P quota of diatoms

   !!* Phytoplankton nutrient limitation terms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicono3   !: Limitation of NO3 uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpiconh4   !: Limitation of NH4 uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicopo4   !: Limitation of PO4 uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanodop   !: Limitation of DOP uptake by nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicodop   !: Limitation of DOP uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatdop   !: Limitation of DOP uptake by diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xpicofer   !: Limitation of Fe uptake by picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimpic    !: Limitation of picophyto PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimpics   !: Limitation of picophyto PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimphys   !: Limitation of nanophyto PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdias   !: Limitation of diatoms PP by nutrients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimpfe    !: Limitation of picophyto PP by Fe
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   fvnuptk    !: Maximum potential uptake rate of nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   fvpuptk    !: Maximum potential uptake rate of picophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   fvduptk    !: Maximum potential uptake rate of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xqfuncfecp !: 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimnpn, xlimnpp, xlimnpd
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   ratchlp

   ! Coefficient for iron limitation following Flynn and Hipkin (1999)
   REAL(wp) ::  xcoef1   = 0.00167  / 55.85
   REAL(wp) ::  xcoef2   = 1.21E-5 * 14. / 55.85 / 7.625 * 0.5 * 1.5
   REAL(wp) ::  xcoef3   = 1.15E-4 * 14. / 55.85 / 7.625 * 0.5 
   REAL(wp) ::  rlogfactdp, rlogfactnp

    LOGICAL  :: l_dia_nut_lim, l_dia_iron_lim, l_dia_fracal
    LOGICAL  :: l_dia_size_lim, l_dia_size_pro
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p5z_lim( kt, knt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim  ***
      !!
      !! ** Purpose :   Compute the co-limitations by the various nutrients
      !!                for the various phytoplankton species. Quota based
      !!                approach. The quota model is derived from theoretical
      !!                models proposed by Pahlow and Oschlies (2009) and 
      !!                Flynn (2001). Various adaptations from several 
      !!                publications by these authors have been also adopted. 
      !!
      !! ** Method  : Quota based approach. The quota model is derived from 
      !!              theoretical models by Pahlow and Oschlies (2009) and 
      !!              Flynn (2001). Various adaptations from several publications
      !!              by these authors have been also adopted.
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in)  :: kt, knt
      INTEGER, INTENT(in)  :: Kbb, Kmm  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, itt
      REAL(wp) ::   zlim1, zlim2, zlim3, zlim4, zno3, zferlim
      REAL(wp) ::   z1_trndia, z1_trnpic, z1_trnphy, ztem1, ztem2, zetot1
      REAL(wp) ::   zratio, zration, zratiof, znutlim, zfalim
      REAL(wp) ::   zconc1d, zconc1dnh4, zconc0n, zconc0nnh4, zconc0npo4, zconc0dpo4
      REAL(wp) ::   zconc0p, zconc0pnh4, zconc0ppo4, zconcpfe, zconcnfe, zconcdfe
      REAL(wp) ::   fanano, fananop, fananof, fadiat, fadiatp, fadiatf
      REAL(wp) ::   fapico, fapicop, fapicof, zlimpo4, zlimdop
      REAL(wp) ::   zrpho, zrass, zfuptk, ztrn, ztrp
      REAL(wp) ::   zproporteuk, zrassint
      REAL(wp) ::   zfvn, zfvp, zfvf, zsizen, zsizep, zsized, znanochl, zpicochl, zdiatchl
      REAL(wp) ::   zqfemn, zqfemp, zqfemd
      REAL(wp) ::   znutlimtot, zlimno3, zlimnh4, zlim1f, ztemp, zrphomin
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zw3d
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_lim')

      IF( kt == nittrc000 )  THEN
         l_dia_nut_lim  = iom_use( "LNnut"   ) .OR. iom_use( "LDnut" ) .OR. iom_use( "LPnut" )
         l_dia_iron_lim = iom_use( "LNFe"    ) .OR. iom_use( "LDFe"  ) .OR. iom_use( "LPFe"  )
         l_dia_size_lim = iom_use( "SIZEN"   ) .OR. iom_use( "SIZED" ) .OR. iom_use( "SIZEP" )
         l_dia_size_pro = iom_use( "RASSN"   ) .OR. iom_use( "RASSP" ) .OR. iom_use( "RASSP" )
         l_dia_fracal   = iom_use( "xfracal" )
      ENDIF
      !
      sizena(:,:,:) = 1.0                    ;  sizepa(:,:,:) = 1.0  
      sizeda(:,:,:) = 1.0
      !
      DO_3D( 0, 0, 0, 0, 1, jpk)
         logsizen(ji,jj,jk) = LOG( sizen(ji,jj,jk) )
         logsizep(ji,jj,jk) = LOG( sizep(ji,jj,jk) )
         logsized(ji,jj,jk) = LOG( sized(ji,jj,jk) )
      END_3D
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ! Computation of the Chl/C ratio of each phytoplankton group
         ! -------------------------------------------------------
         z1_trnphy   = 1. / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
         z1_trnpic   = 1. / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
         z1_trndia   = 1. / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         znanochl = tr(ji,jj,jk,jpnch,Kbb) * z1_trnphy
         zpicochl = tr(ji,jj,jk,jppch,Kbb) * z1_trnpic
         zdiatchl = tr(ji,jj,jk,jpdch,Kbb) * z1_trndia

         ! Computation of a variable Ks for the different phytoplankton
         ! group as a function of their relative size. Allometry
         ! from Edwards et al. (2012)
         !------------------------------------------------

         ! diatoms
         zsized            = EXP(logsized(ji,jj,jk)*0.81)
         zconcdfe          = concdfer * zsized
         zconc1d           = concdno3 * zsized
         zconc1dnh4        = concdnh4 * zsized
         zconc0dpo4        = concdpo4 * zsized

         ! picophytoplankton
         zsizep            = EXP(logsizep(ji,jj,jk)*0.81)
         zconcpfe          = concpfer * zsizep
         zconc0p           = concpno3 * zsizep
         zconc0pnh4        = concpnh4 * zsizep
         zconc0ppo4        = concppo4 * zsizep

         ! nanophytoplankton
         zsizen            = EXP(logsizen(ji,jj,jk)*0.81)
         zconcnfe          = concnfer * zsizen
         zconc0n           = concnno3 * zsizen
         zconc0nnh4        = concnnh4 * zsizen
         zconc0npo4        = concnpo4 * zsizen

         ! Allometric scaling of the Chl/C ratio
         ratchln(ji,jj,jk) = ratchl * EXP( -0.078 * ( rlogfactnp + logsizen(ji,jj,jk) ) )
         ratchlp(ji,jj,jk) = ratchl * EXP( -0.078 * logsizep(ji,jj,jk) )
         ratchld(ji,jj,jk) = ratchl * EXP( -0.078 * ( rlogfactdp + logsized(ji,jj,jk) ) )

         ! Allometric variations of the minimum and maximum quotas
         ! From Talmy et al. (2014) and Maranon et al. (2013)
         ! -------------------------------------------------------
         xqnnmin(ji,jj,jk) = qnnmin * EXP(-0.18 * logsizen(ji,jj,jk) )
         xqnnmax(ji,jj,jk) = qnnmax
         xqndmin(ji,jj,jk) = qndmin * EXP(-0.18 * logsized(ji,jj,jk) )
         xqndmax(ji,jj,jk) = qndmax
         xqnpmin(ji,jj,jk) = qnpmin * EXP(-0.18 * logsizep(ji,jj,jk) )
         xqnpmax(ji,jj,jk) = qnpmax
         !
         ! Michaelis-Menten Limitation term for nutrients Small flagellates
         ! -----------------------------------------------
         ztrn    = tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb)
         ztrp    = tr(ji,jj,jk,jppo4,Kbb) + tr(ji,jj,jk,jpdop,Kbb) / 200.0

         ! Computation of the optimal allocation parameters
         ! Based on the different papers by Pahlow et al., and Smith et al.
         ! -----------------------------------------------------------------
         ! Nanophytoplankton
         znutlim = ztrn / zconc0n
         fanano  = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = ztrp / zconc0npo4
         fananop = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = biron(ji,jj,jk) / zconcnfe
         fananof = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         ! Picophytoplankton
         znutlim = ztrn / zconc0p
         fapico  = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = ztrp / zconc0npo4
         fapicop = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = biron(ji,jj,jk) / zconcpfe
         fapicof = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         ! Diatoms
         znutlim = ztrn / zconc1d
         fadiat  = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = ztrp / zconc0dpo4
         fadiatp = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )
         znutlim = biron(ji,jj,jk) / zconcdfe
         fadiatf = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         !
         ! Michaelis-Menten Limitation term by nutrients of
         !  heterotrophic bacteria
         ! -------------------------------------------------------------
         zlim1    = ( tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) )  &
           &        / ( concbno3 + tr(ji,jj,jk,jpnh4,Kbb) + tr(ji,jj,jk,jpno3,Kbb) )
         !
         zlim2    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + concbpo4)
         zlim3    = biron(ji,jj,jk) / ( concbfe + biron(ji,jj,jk) )
         zlim4    = tr(ji,jj,jk,jpdoc,Kbb) / ( xkdoc   + tr(ji,jj,jk,jpdoc,Kbb) )

         ! Xlimbac is used for DOC solubilization whereas xlimbacl
         ! is used for all the other bacterial-dependent term
         ! -------------------------------------------------------
         xlimbacl(ji,jj,jk) = MAX( 0.1, MIN( zlim1, zlim2, zlim3 ) )
         xlimbac (ji,jj,jk) = xlimbacl(ji,jj,jk) * zlim4
         !
         ! Limitation of N based nutrients uptake (NO3 and NH4)
         zfalim     = (1.-fanano) / fanano
         zlimnh4    = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc0n + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3    = tr(ji,jj,jk,jpno3,Kbb) / ( zconc0n + tr(ji,jj,jk,jpno3,Kbb) )
         znutlimtot = (1. - fanano) * ztrn  / ( zfalim * zconc0n + ztrn )
         ztemp      = znutlimtot / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xnanonh4(ji,jj,jk) = 5.0 * zlimnh4 * ztemp
         xnanono3(ji,jj,jk) = zlimno3 * ztemp
         !
         ! Limitation of P based nutrients (PO4 and DOP)
         zfalim     = (1.-fananop) / fananop
         zlimpo4    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0npo4 )
         zlimdop    = tr(ji,jj,jk,jpdop,Kbb) / ( tr(ji,jj,jk,jpdop,Kbb) + zconc0npo4 )
         znutlimtot = (1. - fananop) * ztrp / ( zfalim * zconc0npo4 + ztrp )
         ztemp      = znutlimtot / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         xnanopo4(ji,jj,jk) = 100.0 * zlimpo4 * ztemp
         xnanodop(ji,jj,jk) = zlimdop * ztemp
         !
         ! Limitation of Fe uptake
         zfalim     = (1.-fananof) / fananof
         xnanofer(ji,jj,jk) = (1. - fananof) * biron(ji,jj,jk) / ( biron(ji,jj,jk) + zfalim * zconcnfe )
         !
         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zratiof   = tr(ji,jj,jk,jpnfe,Kbb) * z1_trnphy
         zqfemn    = xcoef1 * znanochl + xcoef2 + xcoef3 * xnanono3(ji,jj,jk)
         xqfuncfecn(ji,jj,jk) = zqfemn + qfnopt
         !
         zration   = tr(ji,jj,jk,jpnph,Kbb) * z1_trnphy
         zration   = MIN(xqnnmax(ji,jj,jk), MAX( xqnnmin(ji,jj,jk), zration ))
         fvnuptk(ji,jj,jk) = xpsiuptk / xpsinh4 * xqnnmin(ji,jj,jk) / (zration + rtrn)  &
         &                   * MAX(0., (1. - ratchln(ji,jj,jk) * znanochl / 12. ) )
         !
         zlim1     = (zration - xqnnmin(ji,jj,jk) ) / (xqnnmax(ji,jj,jk) - xqnnmin(ji,jj,jk) )

         ! The value of the optimal quota in the formulation below
         ! has been found by solving a non linear equation
         zlim1f    = ( 1.13 - xqnnmin(ji,jj,jk) ) / (xqnnmax(ji,jj,jk) - xqnnmin(ji,jj,jk) )
         zlim3     = MAX( 0., ( zratiof - zqfemn ) / qfnopt )
         ! computation of the various limitation terms of nanophyto
         ! growth and PP
         xlimnfe (ji,jj,jk) = MIN( 1., zlim3 )
         xlimphy (ji,jj,jk) = MIN( 1., zlim1, zlim3 )
         xlimphys(ji,jj,jk) = MIN( 1., zlim1 / ( zlim1f + rtrn ), zlim3 )
         xlimnpn (ji,jj,jk) = MIN( 1., zlim1 )

         !
         ! Michaelis-Menten Limitation term for nutrients picophytoplankton
         ! ----------------------------------------------------------------
         ! Limitation of N based nutrients uptake (NO3 and NH4) 
         zfalim     = (1.-fapico) / fapico 
         zlimnh4    = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc0p + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3    = tr(ji,jj,jk,jpno3,Kbb) / ( zconc0p + tr(ji,jj,jk,jpno3,Kbb) )
         znutlimtot = (1. - fapico) * ztrn / ( zfalim * zconc0p + ztrn )
         ztemp      = znutlimtot / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xpiconh4(ji,jj,jk) = 5.0 * zlimnh4 * ztemp
         xpicono3(ji,jj,jk) = zlimno3 * ztemp
         !
         ! Limitation of P based nutrients uptake (PO4 and DOP)
         zfalim     = (1.-fapicop) / fapicop 
         zlimpo4    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0ppo4 )
         zlimdop    = tr(ji,jj,jk,jpdop,Kbb) / ( tr(ji,jj,jk,jpdop,Kbb) + zconc0ppo4 )
         znutlimtot = (1. - fapicop) * ztrp / ( zfalim * zconc0ppo4 + ztrp)
         ztemp      = znutlimtot / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         xpicopo4(ji,jj,jk) = 100.0 * zlimpo4 * ztemp
         xpicodop(ji,jj,jk) = zlimdop * ztemp
         !
         zfalim     = (1.-fapicof) / fapicof
         xpicofer(ji,jj,jk) = (1. - fapicof) * biron(ji,jj,jk) / ( biron(ji,jj,jk) + zfalim * zconcpfe )
         !
         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zratiof    = tr(ji,jj,jk,jppfe,Kbb) * z1_trnpic
         zqfemp     = xcoef1 * zpicochl + xcoef2 + xcoef3 * xpicono3(ji,jj,jk)
         xqfuncfecp(ji,jj,jk) = zqfemp + qfpopt
         !
         zration    = tr(ji,jj,jk,jpnpi,Kbb) * z1_trnpic
         zration    = MIN(xqnpmax(ji,jj,jk), MAX( xqnpmin(ji,jj,jk), zration ))
         fvpuptk(ji,jj,jk) = xpsiuptk / xpsinh4 * xqnpmin(ji,jj,jk) / (zration + rtrn)  &
           &                 * MAX(0., (1. - ratchlp(ji,jj,jk) * zpicochl / 12. ) ) 
         !
         zlim1      = (zration - xqnpmin(ji,jj,jk) ) / (xqnpmax(ji,jj,jk) - xqnpmin(ji,jj,jk) )

         ! The value of the optimal quota in the formulation below
         ! has been found by solving a non linear equation
         zlim1f     = (1.13 - xqnpmin(ji,jj,jk) ) / (xqnpmax(ji,jj,jk) - xqnpmin(ji,jj,jk) )
         zlim3      = MAX( 0.,( zratiof - zqfemp ) / qfpopt )

         ! computation of the various limitation terms of picophyto
         ! growth and PP
         xlimpfe (ji,jj,jk) = MIN( 1., zlim3 )
         xlimpic (ji,jj,jk) = MIN( 1., zlim1, zlim3 )
         xlimnpp (ji,jj,jk) = MIN( 1., zlim1 )
         xlimpics(ji,jj,jk) = MIN( 1., zlim1 / ( zlim1f + rtrn ), zlim3 )
         !
         !   Michaelis-Menten Limitation term for nutrients Diatoms
         !   ------------------------------------------------------
         !
         ! Limitation of N based nutrients uptake (NO3 and NH4)
         zfalim     = (1.-fadiat) / fadiat 
         zlimnh4    = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc1d + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3    = tr(ji,jj,jk,jpno3,Kbb) / ( zconc1d + tr(ji,jj,jk,jpno3,Kbb) )
         znutlimtot = (1.0 - fadiat) * ztrn / ( zfalim * zconc1d + ztrn )
         ztemp      = znutlimtot / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xdiatnh4(ji,jj,jk) = 5.0 * zlimnh4 * ztemp
         xdiatno3(ji,jj,jk) = zlimno3 * ztemp
         !
         ! Limitation of P based nutrients uptake (PO4 and DOP)
         zfalim     = (1.-fadiatp) / fadiatp
         zlimpo4    = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0dpo4 )
         zlimdop    = tr(ji,jj,jk,jpdop,Kbb) / ( tr(ji,jj,jk,jpdop,Kbb) + zconc0dpo4 )
         znutlimtot = (1. - fadiatp) * ztrp / ( zfalim * zconc0dpo4 + ztrp )
         ztemp      = znutlimtot / ( zlimdop + 100.0 * zlimpo4 + rtrn )
         xdiatpo4(ji,jj,jk) = 100.0 * zlimpo4 * ztemp
         xdiatdop(ji,jj,jk) = zlimdop * ztemp
         !
         ! Limitation of Fe uptake
         zfalim     = (1.-fadiatf) / fadiatf
         xdiatfer(ji,jj,jk) = (1. - fadiatf) * biron(ji,jj,jk) / ( biron(ji,jj,jk) + zfalim * zconcdfe )
         !
         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zratiof    = tr(ji,jj,jk,jpdfe,Kbb) * z1_trndia
         zqfemd     = xcoef1 * zdiatchl + xcoef2 + xcoef3 * xdiatno3(ji,jj,jk)
         xqfuncfecd(ji,jj,jk) = zqfemd + qfdopt
         !
         zration    = tr(ji,jj,jk,jpndi,Kbb) * z1_trndia
         zration    = MIN(xqndmax(ji,jj,jk), MAX( xqndmin(ji,jj,jk), zration ))
         fvduptk(ji,jj,jk) = xpsiuptk / xpsinh4 * xqndmin(ji,jj,jk) / (zration + rtrn)   &
         &                   * MAX(0., (1. - ratchld(ji,jj,jk) * zdiatchl / 12. ) ) 
         !
         zlim1      = (zration - xqndmin(ji,jj,jk) ) / (xqndmax(ji,jj,jk) - xqndmin(ji,jj,jk) )
         ! The value of the optimal quota in the formulation below
         ! has been found by solving a non linear equation
         zlim1f     = (1.13 - xqndmin(ji,jj,jk) ) / (xqndmax(ji,jj,jk) - xqndmin(ji,jj,jk) )
         zlim3      = tr(ji,jj,jk,jpsil,Kbb) / ( tr(ji,jj,jk,jpsil,Kbb) + xksi(ji,jj) )
         zlim4      = MAX( 0., ( zratiof - zqfemd ) / qfdopt )
         ! computation of the various limitation terms of diatoms
         ! growth and PP
         xlimdfe(ji,jj,jk)  = MIN( 1., zlim4 )
         xlimdia(ji,jj,jk)  = MIN( 1., zlim1, zlim3, zlim4 )
         xlimdias(ji,jj,jk) = MIN( 1., zlim1 / (zlim1f + rtrn ), zlim3, zlim4 )
         xlimsi(ji,jj,jk)   = MIN( 1., zlim1, zlim4 )
         xlimnpd(ji,jj,jk)  = MIN( 1., zlim1 )
      END_3D

      !
      ! Compute the phosphorus quota values. It is based on Litchmann et al., 2004 and Daines et al, 2013.
      ! The relative contribution of three fonctional pools are computed: light harvesting apparatus, 
      ! nutrient uptake pool and assembly machinery. DNA is assumed to represent 1% of the dry mass of 
      ! phytoplankton (see Daines et al., 2013). 
      ! --------------------------------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ztrp     = MAX(1.E-6,tr(ji,jj,jk,jppo4,Kbb) + tr(ji,jj,jk,jpdop,Kbb) / 200.0)
         ! N/P ratio of nanophytoplankton
         ! ------------------------------
         zfuptk   = 0.2 + 0.12 / ( 4.0 * sizen(ji,jj,jk) + rtrn )
         ! Computed from Inomura et al. (2020) using Pavlova Lutheri
         zrpho    = 11.55 * tr(ji,jj,jk,jpnch,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) * 12. + rtrn )
         zrphomin = 11.55 * 0.0025
         zrass    = 0.62 * (0.25 + 0.75 * ( 1. - zrpho - zfuptk ) * xlimnpn(ji,jj,jk) )
         xqpnmin(ji,jj,jk) = ( 0.0078 + 0.62 * 0.25 * 0.0783 + zrphomin * 0.0089 ) * 16.
         xqpnmax(ji,jj,jk) = ( zrpho * 0.0089 + zrass * 0.0783 + 0.0078 + 0.022 ) * 16.
         xqpnmax(ji,jj,jk) = MIN( qpnmax, xqpnmax(ji,jj,jk) + 4000 * ztrp )

         ! N/P ratio of picophytoplankton
         ! ------------------------------
         zproporteuk = 0.15 + ( sizep(ji,jj,jk) - 1.0 ) * 0.96
         zfuptk   = 0.2 + 0.12 / ( 0.8 * sizep(ji,jj,jk) + rtrn )
         ! Computed from Inomura et al. (2020) using a synechococcus
         zrpho    = 13.4 * tr(ji,jj,jk,jppch,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) * 12. + rtrn )
         zrphomin = 13.4 * 0.0025
         zrassint = ( 0.4 * 0.0517 * (1.0 - zproporteuk) + 0.62 * 0.0783 * zproporteuk )
         xqppmin(ji,jj,jk) = ( 0.0078 + zrassint * 0.25 + zrphomin * 0.0078 ) * 16.
         zrass    = zrassint * ( 0.25 + 0.75 * ( 1. - zrpho - zfuptk ) * xlimnpp(ji,jj,jk) )
         xqppmax(ji,jj,jk) = ( zrpho * 0.0076 + zrass + 0.0078 + 0.022 ) * 16.
         xqppmax(ji,jj,jk) = MIN( qppmax, xqppmax(ji,jj,jk) + ( 2000 * (1.0 - zproporteuk) + 4000.0 * zproporteuk) * ztrp )

         ! N/P ratio of diatoms
         ! --------------------
         zfuptk   = 0.2 + 0.12 / ( 6.0 * sized(ji,jj,jk) + rtrn )
         ! Computed from Inomura et al. (2020) using a synechococcus
         zrpho    = 8.08 * tr(ji,jj,jk,jpdch,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) * 12. + rtrn )
         zrphomin = 8.08 * 0.0025
         zrass    = 0.66 * ( 0.25 + 0.75 * ( 1. - zrpho - zfuptk ) * xlimnpd(ji,jj,jk) )
         xqpdmin(ji,jj,jk) = ( 0.0078 + 0.66 * 0.25 * 0.0783 + zrphomin * 0.0135 ) * 16.
         xqpdmax(ji,jj,jk) = ( zrpho * 0.0135 + zrass * 0.0783 + 0.0078 + 0.022 ) * 16.
         xqpdmax(ji,jj,jk) = MIN( qpdmax, xqpdmax(ji,jj,jk) + 5500 * ztrp )
      END_3D

      ! Compute the fraction of nanophytoplankton that is made of calcifiers
      ! This is a purely adhoc formulation described in Aumont et al. (2015)
      ! This fraction depends on nutrient limitation, light, temperature
      ! --------------------------------------------------------------------
      !
#if defined key_RK3
      ! Don't consider mid-step values if online coupling
      ! because these are possibly non-monotonic (even with FCT):
      IF ( l_offline ) THEN ; itt = Kmm ; ELSE ; itt = Kbb ; ENDIF
#else
      itt = Kmm
#endif      

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         ztem1  = MAX( 0., ts(ji,jj,jk,jp_tem,itt) + 1.8 )
         ztem2  = ts(ji,jj,jk,jp_tem,itt) - 10.
         zetot1 = MAX( 0., etot_ndcy(ji,jj,jk) - 1.) / ( 4. + etot_ndcy(ji,jj,jk) ) * 30. / ( 30. + etot_ndcy(ji,jj,jk) ) 

         xfracal(ji,jj,jk) = caco3r * ztem1 / ( 0.1 + ztem1 )     &
            &                * MAX( 1., tr(ji,jj,jk,jpphy,Kbb) / xsizephy )           &
            &                * ( 1. + EXP(-ztem2 * ztem2 / 25. ) )                    &
            &                * zetot1 * MIN( 1., 50. / ( hmld(ji,jj) + rtrn ) )
         xfracal(ji,jj,jk) = MAX( 0.02, MIN( 0.8 , xfracal(ji,jj,jk) ) )
      END_3D
      !
      IF( knt == nrdttrc ) THEN        ! save output diagnostics
        !
        IF( l_dia_fracal ) THEN   ! fraction of calcifiers
          CALL iom_put( "xfracal",  xfracal(:,:,:) * tmask(A2D(0),:) )
        ENDIF
        !
        IF( l_dia_nut_lim ) THEN   ! Nutrient limitation term
          CALL iom_put( "LNnut",  xlimphy(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "LDnut",  xlimdia(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "LPnut",  xlimpic(:,:,:) * tmask(A2D(0),:) )
        ENDIF
        !
        IF( l_dia_iron_lim ) THEN   ! Iron limitation term
          CALL iom_put( "LNFe",  xlimnfe(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "LDFe",  xlimdfe(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "LPFe",  xlimpfe(:,:,:) * tmask(A2D(0),:) )
        ENDIF
        !
        IF( l_dia_size_lim ) THEN   ! Size limitation term
          CALL iom_put( "SIZEN",  sizen(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "SIZED",  sized(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "SIZEP",  sizep(:,:,:) * tmask(A2D(0),:) )
        ENDIF
        !
        IF( l_dia_size_pro ) THEN   ! Size of the protein machinery
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          DO_3D( 0, 0, 0, 0, 1, jpkm1)
             zfuptk = 0.2 + 0.12 / ( 3.0 * sizen(ji,jj,jk) + rtrn )
             zrpho  = 11.55 * tr(ji,jj,jk,jpnch,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) * 12. + rtrn )
             zrass  = 0.62 * (0.25 + 0.75 * ( 1. - zrpho - zfuptk ) * xlimnpn(ji,jj,jk) )
             zw3d(ji,jj,jk) = zrass * tmask(ji,jj,jk)
          END_3D
          CALL iom_put( "RASSN",  zw3d)
          DO_3D( 0, 0, 0, 0, 1, jpkm1)
             zfuptk = 0.2 + 0.12 / ( 0.7 * sizep(ji,jj,jk) + rtrn )
             zrpho  = 13.4 * tr(ji,jj,jk,jppch,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) * 12. + rtrn )
             zrass  = 0.4 * ( 0.25 + 0.75 * ( 1. - zrpho - zfuptk ) * xlimnpp(ji,jj,jk) )
             zw3d(ji,jj,jk) = zrass * tmask(ji,jj,jk)
          END_3D
          CALL iom_put( "RASSP",  zw3d)
          DO_3D( 0, 0, 0, 0, 1, jpkm1)
             zfuptk = 0.2 + 0.12 / ( 5.0 * sized(ji,jj,jk) + rtrn )
             zrpho  = 8.08 * tr(ji,jj,jk,jpdch,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) * 12. + rtrn )
             zrass  = 0.66 * ( 0.25 + 0.75 * ( 1. - zrpho - zfuptk ) * xlimnpd(ji,jj,jk) )
             zw3d(ji,jj,jk) = zrass * tmask(ji,jj,jk)
          END_3D
          CALL iom_put( "RASSD",  zw3d)
          DEALLOCATE( zw3d )
        ENDIF
        !
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('p5z_lim')
      !
   END SUBROUTINE p5z_lim


   SUBROUTINE p5z_lim_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_lim_init  ***
      !!
      !! ** Purpose :   Initialization of nutrient limitation parameters
      !!
      !! ** Method  :   Read the namp5zlim and nampisquota namelists and check
      !!      the parameters called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp5zlim
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zlim/ concnno3, concpno3, concdno3, concnnh4, concpnh4, concdnh4,  &
         &                concnfer, concpfer, concdfer, concbfe, concnpo4, concppo4,   &
         &                concdpo4, concbno3, concbnh4, concbpo4, xsizedia, xsizepic,  &
         &                xsizephy, xsizern, xsizerp, xsizerd, xksi1, xksi2, xkdoc,    &
         &                caco3r, oxymin, ratchl
         !
      NAMELIST/namp5zquota/ qnnmin, qnnmax, qpnmin, qpnmax, qnpmin, qnpmax, qppmin,      &
         &                  qppmax, qndmin, qndmax, qpdmin, qpdmax, qfnmax, qfpmax, qfdmax,  &
         &                  qfnopt, qfpopt, qfdopt
      !!----------------------------------------------------------------------
      !
      READ_NML_REF(numnatp,namp5zlim)
      READ_NML_CFG(numnatp,namp5zlim)
      IF(lwm) WRITE ( numonp, namp5zlim )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for nutrient limitations, namp5zlim'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean rainratio                           caco3r    = ', caco3r
         WRITE(numout,*) '    C associated with Chlorophyll            ratchl    = ', ratchl
         WRITE(numout,*) '    NO3 half saturation of nanophyto         concnno3  = ', concnno3
         WRITE(numout,*) '    NO3 half saturation of picophyto         concpno3  = ', concpno3
         WRITE(numout,*) '    NO3 half saturation of diatoms           concdno3  = ', concdno3
         WRITE(numout,*) '    NH4 half saturation for phyto            concnnh4  = ', concnnh4
         WRITE(numout,*) '    NH4 half saturation for pico             concpnh4  = ', concpnh4
         WRITE(numout,*) '    NH4 half saturation for diatoms          concdnh4  = ', concdnh4
         WRITE(numout,*) '    PO4 half saturation for phyto            concnpo4  = ', concnpo4
         WRITE(numout,*) '    PO4 half saturation for pico             concppo4  = ', concppo4
         WRITE(numout,*) '    PO4 half saturation for diatoms          concdpo4  = ', concdpo4
         WRITE(numout,*) '    half saturation constant for Si uptake   xksi1     = ', xksi1
         WRITE(numout,*) '    half saturation constant for Si/C        xksi2     = ', xksi2
         WRITE(numout,*) '    half-sat. of DOC remineralization        xkdoc     = ', xkdoc
         WRITE(numout,*) '    Iron half saturation for nanophyto       concnfer  = ', concnfer
         WRITE(numout,*) '    Iron half saturation for picophyto       concpfer  = ', concpfer
         WRITE(numout,*) '    Iron half saturation for diatoms         concdfer  = ', concdfer
         WRITE(numout,*) '    size ratio for nanophytoplankton         xsizern   = ', xsizern
         WRITE(numout,*) '    size ratio for picophytoplankton         xsizerp   = ', xsizerp
         WRITE(numout,*) '    size ratio for diatoms                   xsizerd   = ', xsizerd
         WRITE(numout,*) '    NO3 half saturation of bacteria          concbno3  = ', concbno3
         WRITE(numout,*) '    NH4 half saturation for bacteria         concbnh4  = ', concbnh4
         WRITE(numout,*) '    Minimum size criteria for diatoms        xsizedia  = ', xsizedia
         WRITE(numout,*) '    Minimum size criteria for picophyto      xsizepic  = ', xsizepic
         WRITE(numout,*) '    Minimum size criteria for nanophyto      xsizephy  = ', xsizephy
         WRITE(numout,*) '    Fe half saturation for bacteria          concbfe   = ', concbfe
         WRITE(numout,*) '    halk saturation constant for anoxia       oxymin   =' , oxymin
      ENDIF
      !
      xksi2_3 = xksi2 * xksi2 * xksi2
      !
      READ_NML_REF(numnatp,namp5zquota)
      READ_NML_CFG(numnatp,namp5zquota)
      IF(lwm) WRITE ( numonp, namp5zquota )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for nutrient limitations, namp5zquota'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    optimal Fe quota for nano.               qfnopt    = ', qfnopt
         WRITE(numout,*) '    optimal Fe quota for pico.               qfpopt    = ', qfpopt
         WRITE(numout,*) '    Optimal Fe quota for diatoms             qfdopt    = ', qfdopt
         WRITE(numout,*) '    Minimal N quota for nano                 qnnmin    = ', qnnmin
         WRITE(numout,*) '    Maximal N quota for nano                 qnnmax    = ', qnnmax
         WRITE(numout,*) '    Minimal P quota for nano                 qpnmin    = ', qpnmin
         WRITE(numout,*) '    Maximal P quota for nano                 qpnmax    = ', qpnmax
         WRITE(numout,*) '    Minimal N quota for pico                 qnpmin    = ', qnpmin
         WRITE(numout,*) '    Maximal N quota for pico                 qnpmax    = ', qnpmax
         WRITE(numout,*) '    Minimal P quota for pico                 qppmin    = ', qppmin
         WRITE(numout,*) '    Maximal P quota for pico                 qppmax    = ', qppmax
         WRITE(numout,*) '    Minimal N quota for diatoms              qndmin    = ', qndmin
         WRITE(numout,*) '    Maximal N quota for diatoms              qndmax    = ', qndmax
         WRITE(numout,*) '    Minimal P quota for diatoms              qpdmin    = ', qpdmin
         WRITE(numout,*) '    Maximal P quota for diatoms              qpdmax    = ', qpdmax
         WRITE(numout,*) '    Maximal Fe quota for nanophyto.          qfnmax    = ', qfnmax
         WRITE(numout,*) '    Maximal Fe quota for picophyto.          qfpmax    = ', qfpmax
         WRITE(numout,*) '    Maximal Fe quota for diatoms             qfdmax    = ', qfdmax
      ENDIF
      !
      ! Metabolic cost of nitrate and ammonium utilisation
      xpsino3  = 2.3 * rno3
      xpsinh4  = 1.8 * rno3
      xpsiuptk = 1.0 / 6.625
      !
      !
      rlogfactnp = LOG( 4.0 / 0.8 )
      rlogfactdp = LOG( 6.0 / 0.8 )
      !
      xfracal (:,:,jpk) = 0._wp
      xlimphy (:,:,jpk) = 0._wp    ;   xlimdia (:,:,jpk) = 0._wp   ;   xlimpic (:,:,jpk) = 0._wp
      xlimnfe (:,:,jpk) = 0._wp    ;   xlimdfe (:,:,jpk) = 0._wp   ;   xlimpfe (:,:,jpk) = 0._wp
      xnanono3(:,:,jpk) = 0._wp    ;   xdiatno3(:,:,jpk) = 0._wp   ;   xpicono3(:,:,jpk) = 0._wp
      xnanonh4(:,:,jpk) = 0._wp    ;   xdiatnh4(:,:,jpk) = 0._wp   ;   xpiconh4(:,:,jpk) = 0._wp
      xnanofer(:,:,jpk) = 0._wp    ;   xdiatfer(:,:,jpk) = 0._wp   ;   xpicofer(:,:,jpk) = 0._wp
      xnanopo4(:,:,jpk) = 0._wp    ;   xdiatpo4(:,:,jpk) = 0._wp   ;   xpicopo4(:,:,jpk) = 0._wp
      xlimbac (:,:,jpk) = 0._wp    ;   xlimbacl(:,:,jpk) = 0._wp
      xqfuncfecn(:,:,jpk) = 0._wp  ;   xqfuncfecd(:,:,jpk) = 0._wp ;   xqfuncfecp(:,:,jpk) = 0._wp
      fvnuptk (:,:,jpk) = 0._wp    ;   fvduptk (:,:,jpk) = 0._wp   ;   fvpuptk(:,:,jpk)  = 0._wp
      xlimphys(:,:,jpk) = 0._wp    ;   xlimdias(:,:,jpk) = 0._wp
      xlimnpp (:,:,jpk) = 0._wp    ;   xlimnpn (:,:,jpk) = 0._wp   ;   xlimnpd (:,:,jpk) = 0._wp
      xlimpics(:,:,jpk) = 0._wp   

   END SUBROUTINE p5z_lim_init


   INTEGER FUNCTION p5z_lim_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim_alloc  ***
      !!----------------------------------------------------------------------
      USE lib_mpp , ONLY: ctl_stop
      INTEGER ::   ierr(3)        ! Local variables
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      !*  Biological arrays for phytoplankton growth
      ALLOCATE( xpicono3(A2D(0),jpk), xpiconh4(A2D(0),jpk),       &
         &      xpicopo4(A2D(0),jpk), xpicodop(A2D(0),jpk),       &
         &      xnanodop(A2D(0),jpk), xdiatdop(A2D(0),jpk),       &
         &      xpicofer(A2D(0),jpk), xlimpfe (A2D(0),jpk),       &
         &      fvnuptk (A2D(0),jpk), fvduptk (A2D(0),jpk),       &
         &      xlimphys(A2D(0),jpk), xlimdias(A2D(0),jpk),       &
         &      xlimnpp (A2D(0),jpk), xlimnpn (A2D(0),jpk),       &
         &      xlimnpd (A2D(0),jpk),                              &
         &      xlimpics(A2D(0),jpk), xqfuncfecp(A2D(0),jpk),     &
         &      fvpuptk (A2D(0),jpk), xlimpic (A2D(0),jpk),    STAT=ierr(1) )
         !
      !*  Minimum/maximum quotas of phytoplankton
      ALLOCATE( xqnnmin (A2D(0),jpk), xqnnmax(A2D(0),jpk),       &
         &      xqpnmin (A2D(0),jpk), xqpnmax(A2D(0),jpk),       &
         &      xqnpmin (A2D(0),jpk), xqnpmax(A2D(0),jpk),       &
         &      xqppmin (A2D(0),jpk), xqppmax(A2D(0),jpk),       &
         &      xqndmin (A2D(0),jpk), xqndmax(A2D(0),jpk),       &
         &      xqpdmin (A2D(0),jpk), xqpdmax(A2D(0),jpk),     STAT=ierr(2) )
         !
      !*  Chl/C in chloroplast
      ALLOCATE( ratchlp (A2D(0),jpk),                          STAT=ierr(3) )
      !
      p5z_lim_alloc = MAXVAL( ierr )
      !
      IF( p5z_lim_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p5z_lim_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p5z_lim_alloc
   !!======================================================================
END MODULE p5zlim
