MODULE p4zlim
   !!======================================================================
   !!                         ***  MODULE p4zlim  ***
   !! TOP :   Computes the nutrient limitation terms of phytoplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-04  (O. Aumont, C. Ethe) Limitation for iron modelled in quota 
   !!----------------------------------------------------------------------
   !!   p4z_lim        :   Compute the nutrients limitation terms 
   !!   p4z_lim_init   :   Read the namelist 
   !!----------------------------------------------------------------------
   USE oce_trc         ! Shared ocean-passive tracers variables
   USE trc             ! Tracers defined
   USE sms_pisces      ! PISCES variables
   USE p2zlim          ! Reduced PISCES nutrient limitation
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC p4z_lim           ! called in p4zbio.F90 
   PUBLIC p4z_lim_init      ! called in trcsms_pisces.F90 
   PUBLIC p4z_lim_alloc     ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  concdno3    !:  Phosphate half saturation for diatoms  
   REAL(wp), PUBLIC ::  concnnh4    !:  NH4 half saturation for nanophyto  
   REAL(wp), PUBLIC ::  concdnh4    !:  NH4 half saturation for diatoms
   REAL(wp), PUBLIC ::  concdfer    !:  Iron half saturation for diatoms  
   REAL(wp), PUBLIC ::  concbnh4    !:  NH4 half saturation for bacteria
   REAL(wp), PUBLIC ::  xsizedia    !:  Minimum size criteria for diatoms
   REAL(wp), PUBLIC ::  xsizerd     !:  Size ratio for diatoms
   REAL(wp), PUBLIC ::  xksi1       !:  half saturation constant for Si uptake 
   REAL(wp), PUBLIC ::  xksi2       !:  half saturation constant for Si/C 
   REAL(wp), PUBLIC ::  qnfelim     !:  optimal Fe quota for nanophyto
   REAL(wp), PUBLIC ::  qdfelim     !:  optimal Fe quota for diatoms
   REAL(wp), PUBLIC ::  ratchl      !:  C associated with Chlorophyll

   REAL(wp), PUBLIC ::  xksi2_3     !:  xksi2**3 

   !!* Phytoplankton limitation terms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatno3   !: Diatoms limitation by NO3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanonh4   !: Nanophyto limitation by NH4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatnh4   !:  Diatoms limitation by NH4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanopo4   !: Nanophyto limitation by PO4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatpo4   !: Diatoms limitation by PO4
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdia    !: Nutrient limitation term of diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimdfe    !: Diatoms limitation by iron
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimsi     !: Diatoms limitation by Si
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanofer   !: Limitation of Fe uptake by nanophyto
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xdiatfer   !: Limitation of Fe uptake by diatoms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xqfuncfecd, xqfuncfecn
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   ratchln, ratchld

   ! Coefficient for iron limitation following Flynn and Hipkin (1999)
   REAL(wp) ::  xcoef1   = 0.0016  / 55.85  
   REAL(wp) ::  xcoef2   = 1.21E-5 * 14. / 55.85 / 7.3125 * 0.5 * 1.5
   REAL(wp) ::  xcoef3   = 1.15E-4 * 14. / 55.85 / 7.3125 * 0.5 
   REAL(wp) ::  rlogfactdn

   LOGICAL  :: l_dia_nut_lim, l_dia_iron_lim, l_dia_size_lim, l_dia_fracal

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_lim( kt, knt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_lim  ***
      !!
      !! ** Purpose :   Compute the co-limitations by the various nutrients
      !!                for the various phytoplankton species
      !!
      !! ** Method  : - Limitation follows the Liebieg law of the minimum
      !!              - Monod approach for N, P and Si. Quota approach 
      !!                for Iron
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt, knt
      INTEGER, INTENT(in)  :: Kbb, Kmm      ! time level indices
      !
      INTEGER  ::   ji, jj, jk, itt
      REAL(wp) ::   zlim1, zlim2, zlim3, zlim4, ztemp
      REAL(wp) ::   z1_trbdia, z1_trbphy, ztem1, ztem2, zetot1, zetot2
      REAL(wp) ::   zdenom, zratio, zironmin, zbactno3, zbactnh4
      REAL(wp) ::   zconc1d, zconc1dnh4, zconc0n, zconc0nnh4   
      REAL(wp) ::   fananof, fadiatf, znutlim, zfalim
      REAL(wp) ::   zsizen, zsized, zconcnfe, zconcdfe
      REAL(wp) ::   ztrn, zlimno3, zlimnh4
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p4z_lim')
      !
      IF( kt == nittrc000 )  THEN
         l_dia_nut_lim  = iom_use( "LNnut"   ) .OR. iom_use( "LDnut" )  
         l_dia_iron_lim = iom_use( "LNFe"    ) .OR. iom_use( "LDFe"  )
         l_dia_size_lim = iom_use( "SIZEN"   ) .OR. iom_use( "SIZED" )
         l_dia_fracal   = iom_use( "xfracal" )
      ENDIF
      !
      sizena(:,:,:) = 1.0                    ;   sizeda(:,:,:) = 1.0
      logsizen(:,:,:) = LOG( sizen(:,:,:) )  ;   logsized(:,:,:) = LOG(sized(:,:,:) )
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         
         ! Computation of a variable Ks for iron on diatoms taking into account
         ! that increasing biomass is made of generally bigger cells
         ! The allometric relationship is classical.
         !------------------------------------------------
         z1_trbphy  = 1. / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
         z1_trbdia  = 1. / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
         ztrn       = tr(ji,jj,jk,jpno3,Kbb) + tr(ji,jj,jk,jpnh4,Kbb)

         zsizen     = EXP(logsizen(ji,jj,jk)*0.81)
         zconcnfe   = concnfer * zsizen
         zconc0n    = concnno3 * zsizen
         zconc0nnh4 = concnnh4 * zsizen

         zsized     = EXP(logsized(ji,jj,jk)*0.81)
         zconcdfe   = concdfer * zsized
         zconc1d    = concdno3 * zsized
         zconc1dnh4 = concdnh4 * zsized

         ratchln(ji,jj,jk) = ratchl * EXP( -0.078 * logsizen(ji,jj,jk) )
         ratchld(ji,jj,jk) = ratchl * EXP( -0.078 * ( rlogfactdn + logsized(ji,jj,jk) ) )


         ! Computation of the optimal allocation parameters
         ! Based on the different papers by Pahlow et al., and 
         ! Smith et al.
         ! ---------------------------------------------------

         ! Nanophytoplankton
         znutlim    = biron(ji,jj,jk) / zconcnfe
         fananof    = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         ! Diatoms
         znutlim    = biron(ji,jj,jk) / zconcdfe
         fadiatf    = MAX(0.01, MIN(0.99, 1. / ( SQRT(znutlim) + 1.) ) )

         ! Michaelis-Menten Limitation term by nutrients of
         ! heterotrophic bacteria
         ! -------------------------------------------------
         zlimnh4    = tr(ji,jj,jk,jpnh4,Kbb) / ( concbno3 + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3    = tr(ji,jj,jk,jpno3,Kbb) / ( concbno3 + tr(ji,jj,jk,jpno3,Kbb) )
         zlim1      = ztrn   / ( concbno3 + ztrn )
         ztemp      = zlim1  / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         zbactnh4   = 5.0 * zlimnh4 * ztemp
         zbactno3   = zlimno3 * ztemp
         !
         zlim2      = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + concbnh4 )
         zlim3      = biron(ji,jj,jk) / ( concbfe + biron(ji,jj,jk) )
         zlim4      = tr(ji,jj,jk,jpdoc,Kbb) / ( xkdoc   + tr(ji,jj,jk,jpdoc,Kbb) )
         ! Xlimbac is used for DOC solubilization whereas xlimbacl
         ! is used for all the other bacterial-dependent terms
         ! -------------------------------------------------------
         xlimbacl(ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
         xlimbac (ji,jj,jk) = MIN( zlim1, zlim2, zlim3 ) * zlim4

         ! Michaelis-Menten Limitation term by nutrients: Nanophyto
         ! Optimal parameterization by Smith and Pahlow series of 
         ! papers is used. Optimal allocation is supposed independant
         ! for all nutrients. 
         ! --------------------------------------------------------

         ! Limitation of Fe uptake (Quota formalism)
         zfalim     = (1.-fananof) / fananof
         xnanofer(ji,jj,jk) = (1. - fananof) * biron(ji,jj,jk) / ( biron(ji,jj,jk) + zfalim * zconcnfe )

         ! Limitation of nanophytoplankton growth
         zlimnh4    = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc0n + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3    = tr(ji,jj,jk,jpno3,Kbb) / ( zconc0n + tr(ji,jj,jk,jpno3,Kbb) )
         zlim1      = ztrn / ( zconc0n + ztrn )
         ztemp      = zlim1  / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xnanonh4(ji,jj,jk) = 5.0 * zlimnh4 * ztemp
         xnanono3(ji,jj,jk) = zlimno3 * ztemp
         !
         zlim2      = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc0nnh4 )
         zratio     = tr(ji,jj,jk,jpnfe,Kbb) * z1_trbphy 

         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zironmin   = xcoef1 * tr(ji,jj,jk,jpnch,Kbb) * z1_trbphy + xcoef2 * zlim1 + xcoef3 * xnanono3(ji,jj,jk)
         xqfuncfecn(ji,jj,jk) = zironmin + qnfelim
         zlim3      = MAX( 0.,( zratio - zironmin ) / qnfelim )
         xnanopo4(ji,jj,jk) = zlim2
         xlimnfe (ji,jj,jk) = MIN( 1., zlim3 )
         xlimphy (ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
               
         !   Michaelis-Menten Limitation term by nutrients : Diatoms
         !   -------------------------------------------------------
         ! Limitation of Fe uptake (Quota formalism)
         zfalim     = (1.-fadiatf) / fadiatf
         xdiatfer(ji,jj,jk) = (1. - fadiatf) * biron(ji,jj,jk) / ( biron(ji,jj,jk) + zfalim * zconcdfe )

         ! Limitation of diatoms growth
         zlimnh4    = tr(ji,jj,jk,jpnh4,Kbb) / ( zconc1d + tr(ji,jj,jk,jpnh4,Kbb) )
         zlimno3    = tr(ji,jj,jk,jpno3,Kbb) / ( zconc1d + tr(ji,jj,jk,jpno3,Kbb) )
         zlim1      = ztrn   / ( zconc1d + ztrn )
         ztemp      = zlim1  / ( zlimno3 + 5.0 * zlimnh4 + rtrn )
         xdiatnh4(ji,jj,jk) = 5.0 * zlimnh4 * ztemp
         xdiatno3(ji,jj,jk) = zlimno3 * ztemp
         !
         zlim2      = tr(ji,jj,jk,jppo4,Kbb) / ( tr(ji,jj,jk,jppo4,Kbb) + zconc1dnh4  )
         zlim3      = tr(ji,jj,jk,jpsil,Kbb) / ( tr(ji,jj,jk,jpsil,Kbb) + xksi(ji,jj) + rtrn )
         zratio     = tr(ji,jj,jk,jpdfe,Kbb) * z1_trbdia

         ! The minimum iron quota depends on the size of PSU, respiration
         ! and the reduction of nitrate following the parameterization 
         ! proposed by Flynn and Hipkin (1999)
         zironmin   = xcoef1 * tr(ji,jj,jk,jpdch,Kbb) * z1_trbdia + xcoef2 * zlim1 + xcoef3 * xdiatno3(ji,jj,jk)
         xqfuncfecd(ji,jj,jk) = zironmin + qdfelim
         zlim4      = MAX( 0., ( zratio - zironmin ) / qdfelim )
         xdiatpo4(ji,jj,jk) = zlim2
         xlimdfe (ji,jj,jk) = MIN( 1., zlim4 )
         xlimdia (ji,jj,jk) = MIN( zlim1, zlim2, zlim3, zlim4 )
         xlimsi  (ji,jj,jk) = MIN( zlim1, zlim2, zlim4 )
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
         ztem1  = MAX( 0., ts(ji,jj,jk,jp_tem,itt) + 1.8)
         ztem2  = ts(ji,jj,jk,jp_tem,itt) - 10.
         zetot1 = MAX( 0., etot_ndcy(ji,jj,jk) - 1.) / ( 4. + etot_ndcy(ji,jj,jk) ) 
         zetot2 = 30. / ( 30.0 + etot_ndcy(ji,jj,jk) )

         xfracal(ji,jj,jk) = caco3r * xlimphy(ji,jj,jk)                             &
           &                        * ztem1 / ( 0.1 + ztem1 )                       &
           &                        * MAX( 1., tr(ji,jj,jk,jpphy,Kbb) / xsizephy )  &
           &                        * zetot1 * zetot2                               &
           &                        * ( 1. + EXP(-ztem2 * ztem2 / 25. ) )           &
           &                        * MIN( 1., 50. / ( hmld(ji,jj) + rtrn ) )
         xfracal(ji,jj,jk) = MIN( 0.8 , xfracal(ji,jj,jk) )
         xfracal(ji,jj,jk) = MAX( 0.02, xfracal(ji,jj,jk) )
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
        ENDIF
        !
        IF( l_dia_iron_lim ) THEN   ! Iron limitation term
          CALL iom_put( "LNFe",  xlimnfe(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "LDFe",  xlimdfe(:,:,:) * tmask(A2D(0),:) )
        ENDIF
        !
        IF( l_dia_size_lim ) THEN   ! Size limitation term
          CALL iom_put( "SIZEN",  sizen(:,:,:) * tmask(A2D(0),:) )
          CALL iom_put( "SIZED",  sized(:,:,:) * tmask(A2D(0),:) )
        ENDIF
        !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_lim')
      !
   END SUBROUTINE p4z_lim


   SUBROUTINE p4z_lim_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_lim_init  ***
      !!
      !! ** Purpose :   Initialization of the nutrient limitation parameters
      !!
      !! ** Method  :   Read the namp4zlim namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp4zlim
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer

      ! Namelist block
      NAMELIST/namp4zlim/ concnno3, concdno3, concnnh4, concdnh4, concnfer, concdfer, concbfe,   &
         &                concbno3, concbnh4, xsizedia, xsizephy, xsizern, xsizerd,          & 
         &                xksi1, xksi2, xkdoc, qnfelim, qdfelim, caco3r, oxymin, ratchl
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_lim_init : initialization of nutrient limitations'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,namp4zlim)
      READ_NML_CFG(numnatp,namp4zlim)
      IF(lwm) WRITE( numonp, namp4zlim )

      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : namp4zlim'
         WRITE(numout,*) '      mean rainratio                           caco3r    = ', caco3r
         WRITE(numout,*) '      C associated with Chlorophyll            ratchl    = ', ratchl
         WRITE(numout,*) '      NO3 half saturation of nanophyto         concnno3  = ', concnno3
         WRITE(numout,*) '      NO3 half saturation of diatoms           concdno3  = ', concdno3
         WRITE(numout,*) '      NH4 half saturation for phyto            concnnh4  = ', concnnh4
         WRITE(numout,*) '      NH4 half saturation for diatoms          concdnh4  = ', concdnh4
         WRITE(numout,*) '      half saturation constant for Si uptake   xksi1     = ', xksi1
         WRITE(numout,*) '      half saturation constant for Si/C        xksi2     = ', xksi2
         WRITE(numout,*) '      half-sat. of DOC remineralization        xkdoc     = ', xkdoc
         WRITE(numout,*) '      Iron half saturation for nanophyto       concnfer  = ', concnfer
         WRITE(numout,*) '      Iron half saturation for diatoms         concdfer  = ', concdfer
         WRITE(numout,*) '      size ratio for nanophytoplankton         xsizern   = ', xsizern
         WRITE(numout,*) '      size ratio for diatoms                   xsizerd   = ', xsizerd
         WRITE(numout,*) '      NO3 half saturation of bacteria          concbno3  = ', concbno3
         WRITE(numout,*) '      NH4 half saturation for bacteria         concbnh4  = ', concbnh4
         WRITE(numout,*) '      Minimum size criteria for diatoms        xsizedia  = ', xsizedia
         WRITE(numout,*) '      Minimum size criteria for nanophyto      xsizephy  = ', xsizephy
         WRITE(numout,*) '      Fe half saturation for bacteria          concbfe   = ', concbfe
         WRITE(numout,*) '      halk saturation constant for anoxia      oxymin    =' , oxymin
         WRITE(numout,*) '      optimal Fe quota for nano.               qnfelim   = ', qnfelim
         WRITE(numout,*) '      Optimal Fe quota for diatoms             qdfelim   = ', qdfelim
      ENDIF
      !
      rlogfactdn = log(6.0/1.67)
      !
      xksi2_3 = xksi2 * xksi2 * xksi2
      !
      xfracal (:,:,jpk) = 0._wp
      xlimphy (:,:,jpk) = 0._wp    ;      xlimdia (:,:,jpk) = 0._wp
      xlimnfe (:,:,jpk) = 0._wp    ;      xlimdfe (:,:,jpk) = 0._wp
      xnanono3(:,:,jpk) = 0._wp    ;      xdiatno3(:,:,jpk) = 0._wp
      xnanofer(:,:,jpk) = 0._wp    ;      xdiatfer(:,:,jpk) = 0._wp
      xnanonh4(:,:,jpk) = 0._wp    ;      xdiatnh4(:,:,jpk) = 0._wp
      xnanopo4(:,:,jpk) = 0._wp    ;      xdiatpo4(:,:,jpk) = 0._wp
      xdiatpo4(:,:,jpk) = 0._wp    ;      xdiatpo4(:,:,jpk) = 0._wp
      xlimdia (:,:,jpk) = 0._wp    ;      xlimdfe (:,:,jpk) = 0._wp
      xqfuncfecn(:,:,jpk) = 0._wp    ;    xqfuncfecd(:,:,jpk) = 0._wp
      xlimsi  (:,:,jpk) = 0._wp
      xlimbac (:,:,jpk) = 0._wp    ;      xlimbacl(:,:,jpk) = 0._wp
      !
   END SUBROUTINE p4z_lim_init


   INTEGER FUNCTION p4z_lim_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim_alloc  ***
      !! 
      !            Allocation of the arrays used in this module
      !!----------------------------------------------------------------------
      USE lib_mpp , ONLY: ctl_stop
      !!----------------------------------------------------------------------

      !*  Biological arrays for phytoplankton growth
      ALLOCATE( xdiatno3(A2D(0),jpk),                             &
         &      xnanonh4(A2D(0),jpk), xdiatnh4(A2D(0),jpk),       &
         &      xnanopo4(A2D(0),jpk), xdiatpo4(A2D(0),jpk),       &
         &      xnanofer(A2D(0),jpk), xdiatfer(A2D(0),jpk),       &
         &      xlimdia (A2D(0),jpk), xlimdfe (A2D(0),jpk),       &
         &      xqfuncfecn(A2D(0),jpk), xqfuncfecd(A2D(0),jpk),   &
         &      ratchln (A2D(0),jpk), ratchld (A2D(0),jpk),       &
         &      xlimsi  (A2D(0),jpk), STAT=p4z_lim_alloc )
      !
      IF( p4z_lim_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p4z_lim_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p4z_lim_alloc

   !!======================================================================
END MODULE p4zlim
