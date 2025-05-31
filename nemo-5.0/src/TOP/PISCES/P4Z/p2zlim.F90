MODULE p2zlim
   !!======================================================================
   !!                         ***  MODULE p2zlim  ***
   !! TOP :   Computes the nutrient limitation terms of phytoplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-04  (O. Aumont, C. Ethe) Limitation for iron modelled in quota 
   !!----------------------------------------------------------------------
   !!   p2z_lim        :   Compute the nutrients limitation terms 
   !!   p2z_lim_init   :   Read the namelist 
   !!----------------------------------------------------------------------
   USE oce_trc         ! Shared ocean-passive tracers variables
   USE trc             ! Tracers defined
   USE sms_pisces      ! PISCES variables
   USE iom             ! I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC p2z_lim           ! called in p4zbio.F90 
   PUBLIC p2z_lim_init      ! called in trcsms_pisces.F90 
   PUBLIC p2z_lim_alloc     ! called in trcini_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  concnno3    !:  NO3, PO4 half saturation   
   REAL(wp), PUBLIC ::  concbno3    !:  NO3 half saturation  for bacteria 
   REAL(wp), PUBLIC ::  concnfer    !:  Iron half saturation for nanophyto 
   REAL(wp), PUBLIC ::  xsizephy    !:  Minimum size criteria for nanophyto
   REAL(wp), PUBLIC ::  xsizern     !:  Size ratio for nanophytoplankton
   REAL(wp), PUBLIC ::  xkdoc       !:  2nd half-sat. of DOC remineralization  
   REAL(wp), PUBLIC ::  concbfe     !:  Fe half saturation for bacteria 
   REAL(wp), PUBLIC ::  caco3r      !:  mean rainratio 

   !!* Phytoplankton limitation terms
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xnanono3   !: Nanophyto limitation by NO3
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimphy    !: Nutrient limitation term of nanophytoplankton
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimbac    !: Bacterial limitation term
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimbacl   !: Bacterial limitation term
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  ::   xlimnfe    !: Nanophyto limitation by Iron

   LOGICAL  :: l_dia

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p2z_lim( kt, knt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p2z_lim  ***
      !!
      !! ** Purpose :   Compute the co-limitations by the various nutrients
      !!                for the unique phytoplankton species 
      !!
      !! ** Method  : - Limitation is computed according to Monod formalism
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt, knt
      INTEGER, INTENT(in)  :: Kbb, Kmm      ! time level indices
      !
      INTEGER  ::   ji, jj, jk, itt
      REAL(wp) ::   zconc0n, zconcnf, zlim1, zlim2, zlim3
      REAL(wp) ::   ztem1, ztem2, zetot1, zetot2, zsize
      REAL(wp) ::   zferlim, zno3
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p2z_lim')
      !
      IF( kt == nittrc000 )  &
         &     l_dia = iom_use( "LNnut" ) .OR. iom_use( "SIZEN" ) .OR. iom_use( "xfracal" )
      !
      sizena(:,:,:) = 1.0
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1)

         ! Tuning of the iron concentration to a minimum level that is set to the detection limit
         !-------------------------------------
         zno3    = tr(ji,jj,jk,jpno3,Kbb) / 40.e-6
         zferlim = MAX( 5e-11 * zno3 * zno3, 2e-11 )
         zferlim = MIN( zferlim, 5e-11 )
         tr(ji,jj,jk,jpfer,Kbb) = MAX( tr(ji,jj,jk,jpfer,Kbb), zferlim )
         
         ! Computation of a variable Ks for NO3 on phyto taking into account
         ! that increasing biomass is made of generally bigger cells
         ! The allometric relationship is classical.
         !------------------------------------------------
         zsize   = sizen(ji,jj,jk)**0.81
         zconc0n = concnno3 * zsize
         zconcnf = concnfer * zsize

         ! Michaelis-Menten Limitation term by nutrients of
         ! heterotrophic bacteria
         ! -------------------------------------------------
         zlim1   = tr(ji,jj,jk,jpno3,Kbb) / ( concbno3 + tr(ji,jj,jk,jpno3,Kbb) )
         zlim2   = biron(ji,jj,jk) / ( concbfe + biron(ji,jj,jk) )
         zlim3   = tr(ji,jj,jk,jpdoc,Kbb) / ( xkdoc   + tr(ji,jj,jk,jpdoc,Kbb) )

         ! Xlimbac is used for DOC solubilization whereas xlimbacl
         ! is used for all the other bacterial-dependent terms
         ! -------------------------------------------------------
         xlimbacl(ji,jj,jk) = MIN( zlim1, zlim2)
         xlimbac (ji,jj,jk) = xlimbacl(ji,jj,jk) * zlim3

         ! Michaelis-Menten Limitation term by nutrients: Nanophyto
         ! Optimal parameterization by Smith and Pahlow series of 
         ! papers is used. Optimal allocation is supposed independant
         ! for all nutrients. 
         ! --------------------------------------------------------

         ! Limitation of nanophytoplankton growth
         xnanono3(ji,jj,jk) = tr(ji,jj,jk,jpno3,Kbb) / ( zconc0n + tr(ji,jj,jk,jpno3,Kbb) )
         xlimnfe (ji,jj,jk) = biron(ji,jj,jk) / ( biron(ji,jj,jk) + zconcnf )
         xlimphy (ji,jj,jk) = MIN( xlimnfe(ji,jj,jk), xnanono3(ji,jj,jk) )
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

         xfracal(ji,jj,jk) = caco3r * xlimphy(ji,jj,jk)                              &
            &                       * ztem1 / ( 0.1 + ztem1 )                        &
            &                       * MAX( 1., tr(ji,jj,jk,jpphy,Kbb) / xsizephy )   &
            &                       * zetot1 * zetot2                                &
            &                       * ( 1. + EXP(-ztem2 * ztem2 / 25. ) )            &
            &                       * MIN( 1., 50. / ( hmld(ji,jj) + rtrn ) )
         xfracal(ji,jj,jk) = MIN( 0.8 , xfracal(ji,jj,jk) )
         xfracal(ji,jj,jk) = MAX( 0.02, xfracal(ji,jj,jk) )
      END_3D
      !
      IF( l_dia .AND. knt == nrdttrc ) THEN        ! save output diagnostics
        !
        CALL iom_put( "xfracal", xfracal(:,:,:) * tmask(A2D(0),:) ) ! fraction of calcifiers
        CALL iom_put( "LNnut"  , xlimphy(:,:,:) * tmask(A2D(0),:) ) ! Nutrient limitation term
        CALL iom_put( "SIZEN"  , sizen(:,:,:)   * tmask(A2D(0),:) ) ! Size limitation term
        !
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p2z_lim')
      !
   END SUBROUTINE p2z_lim


   SUBROUTINE p2z_lim_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p2z_lim_init  ***
      !!
      !! ** Purpose :   Initialization of the nutrient limitation parameters
      !!
      !! ** Method  :   Read the namp2zlim namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp2zlim
      !!
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer

      ! Namelist block
      NAMELIST/namp2zlim/ concnno3, concbno3, concnfer, xsizephy, xsizern,  &
         &                concbfe, xkdoc, caco3r, oxymin
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p2z_lim_init : initialization of nutrient limitations'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,namp2zlim)
      READ_NML_CFG(numnatp,namp2zlim)
      IF(lwm) WRITE( numonp, namp2zlim )

      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : namp2zlim'
         WRITE(numout,*) '      mean rainratio                           caco3r    = ', caco3r
         WRITE(numout,*) '      NO3 half saturation of phyto             concnno3  = ', concnno3
         WRITE(numout,*) '      Iron half saturation for nanophyto       concnfer  = ', concnfer
         WRITE(numout,*) '      Fe half saturation for bacteria          concbfe   = ', concbfe
         WRITE(numout,*) '      half-sat. of DOC remineralization        xkdoc     = ', xkdoc
         WRITE(numout,*) '      size ratio for phytoplankton             xsizern   = ', xsizern
         WRITE(numout,*) '      NO3 half saturation of bacteria          concbno3  = ', concbno3
         WRITE(numout,*) '      Minimum size criteria for phyto          xsizephy  = ', xsizephy
         WRITE(numout,*) '      halk saturation constant for anoxia      oxymin    =' , oxymin
      ENDIF
      !
      xfracal (:,:,jpk) = 0._wp
      xnanono3(:,:,jpk) = 0._wp
      xlimphy (:,:,jpk) = 0._wp
      xlimnfe (:,:,jpk) = 0._wp
      xlimbac (:,:,jpk) = 0._wp
      xlimbacl(:,:,jpk) = 0._wp
      !
   END SUBROUTINE p2z_lim_init


   INTEGER FUNCTION p2z_lim_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_lim_alloc  ***
      !! 
      !            Allocation of the arrays used in this module
      !!----------------------------------------------------------------------
      USE lib_mpp , ONLY: ctl_stop
      !!----------------------------------------------------------------------

      !*  Biological arrays for phytoplankton growth
      ALLOCATE( xnanono3(A2D(0),jpk), xlimphy (A2D(0),jpk),       &
         &      xlimnfe (A2D(0),jpk), xlimbac (A2D(0),jpk),       &
         &      xlimbacl(A2D(0),jpk),                       STAT=p2z_lim_alloc )
 
      !
      IF( p2z_lim_alloc /= 0 ) CALL ctl_stop( 'STOP', 'p2z_lim_alloc : failed to allocate arrays.' )
      !
   END FUNCTION p2z_lim_alloc

   !!======================================================================
END MODULE p2zlim
