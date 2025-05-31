MODULE p4zpoc
   !!======================================================================
   !!                         ***  MODULE p4zpoc  ***
   !! TOP :   PISCES Compute remineralization of organic particles
   !!         Same module for both PISCES and PISCES-QUOTA
   !!=========================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06  (O. Aumont, C. Ethe) Quota model for iron
   !!             3.6  !  2016-03  (O. Aumont) Quota model and diverse
   !!             4.0  !  2018     (O. Aumont) Variable lability parameterization
   !!----------------------------------------------------------------------
   !!   p4z_poc       :  Compute remineralization/dissolution of organic compounds
   !!   p4z_poc_init  :  Initialisation of parameters for remineralisation
   !!   alngam and gamain : computation of the incomplete gamma function
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager
   USE lib_fortran     ! Fortran routines library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_poc         ! called in p4zbio.F90
   PUBLIC   p4z_poc_init    ! called in trcini_pisces.F90
   PUBLIC   alngam          ! 
   PUBLIC   gamain          !

   REAL(wp), PUBLIC ::   xremipc    !: remineralisation rate of DOC
   REAL(wp), PUBLIC ::   xremipn    !: remineralisation rate of DON
   REAL(wp), PUBLIC ::   xremipp    !: remineralisation rate of DOP
   INTEGER , PUBLIC ::   jcpoc      !: number of lability classes
   REAL(wp), PUBLIC ::   rshape     !: shape factor of the gamma distribution

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:)       ::  alphan, reminp   !: variable lability of POC and initial distribution
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::  alphap, alphag    !: lability distribution of small particles
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  orem3    !: lability distribution of small particles
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:)     ::  pdep

   REAL(wp ) :: solgoc, rbound
   LOGICAL   :: l_dia_remin

   LOGICAL, PUBLIC  :: ll_poc_lab
   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zpoc.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_poc( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_poc  ***
      !!
      !! ** Purpose :   Compute remineralization of organic particles
      !!                A reactivity-continuum parameterization is chosen
      !!                to describe the lability of the organic particles
      !!                As a consequence, the remineralisation rates of the 
      !!                the different pools change with time as a function of 
      !!                the lability distribution
      !!
      !! ** Method  : - Computation of the remineralisation rates is performed
      !!                according to reactivity continuum formalism described
      !!                in Aumont et al. (2017). 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt         ! ocean time step and ???
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zremip, zremig, zorem, zorem2, zofer, zfact
      REAL(wp) ::   zopon, zopop, zopon2, zopop2
      REAL(wp) ::   zofer2, zofer3, zreminp1, zreminp2
      CHARACTER (len=25) :: charout
      LOGICAL  :: ll_newday
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zremipoc, zremigoc, zfolimi
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p4z_poc')
      !
      IF( kt == nittrc000 .AND. knt == 1 ) THEN
         l_dia_remin = iom_use( "REMINP" ) .OR. iom_use( "REMING" ) .OR. iom_use( "REMINF" )
         ALLOCATE( pdep(A2D(0)) )
      ENDIF
      !
      IF( l_dia_remin ) THEN
         ALLOCATE( zfolimi (A2D(0),jpk) )  ;  zfolimi(A2D(0),:) = tr(A2D(0),:,jpfer,Krhs)
      ENDIF

      ! Initialisation of arrays
      orem(:,:,jpk) = 0._wp
      DO_2D( 0, 0, 0, 0 )
         pdep(ji,jj) = MAX( hmld(ji,jj), gdept(ji,jj,1,Kmm) )
      END_2D
      !
      ll_newday  = nsec_day == NINT(0.5_wp * rn_Dt)  ! New day
      IF( ll_newday ) THEN   ;  ll_poc_lab = .TRUE.
      ELSE                   ;  ll_poc_lab = .FALSE.
      ENDIF
      !
      IF( kt == nittrc000 .AND. .NOT. ln_rsttr )   ll_poc_lab = .TRUE.
      !
      ll_poc_lab = ll_poc_lab .AND. knt == 1

      IF( .NOT. ln_p2z ) THEN
         !
         IF( ll_poc_lab ) THEN
            ! Initialisation of the lability distributions that are set to the distribution of newly produced organic particles
            IF(lwp) write(numout,*)
            IF(lwp) write(numout,*) ' Compute variable lability for biogeochemistry at kt =  ',  kt, '  day = ', nday_year
            IF(lwp) write(numout,*) '~~~~~~'
            !
            CALL p4z_goc_lab( kt, Kbb, Kmm )
            !
         ENDIF
         ! Lability parameterization. This is the big particles part (GOC)
         ! ----------------------------------------------------------------- 
         IF( l_dia_remin ) THEN
            ALLOCATE( zremigoc(A2D(0),jpk) )  ;  zremigoc(A2D(0),:) = tr(A2D(0),:,jpdoc,Krhs)
         ENDIF
         !
         ! The standard PISCES part
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            ! POC degradation by bacterial activity. It is a function
            ! of the mean lability and of temperature. This also includes
            ! shrinking of particles due to the bacterial activity
            ! -----------------------------------------------------------
            zremig  = remintgoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
            zorem2  = zremig * tr(ji,jj,jk,jpgoc,Kbb)
            orem(ji,jj,jk)  = zorem2
            orem3(ji,jj,jk) = zremig * solgoc * tr(ji,jj,jk,jpgoc,Kbb)
            zofer2 = zremig * tr(ji,jj,jk,jpbfe,Kbb)

            ! update of the TRA arrays
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + orem3(ji,jj,jk)
            tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) - zorem2 - orem3(ji,jj,jk)
            tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + solgoc * zofer2
            tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) - (1. + solgoc) * zofer2
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zorem2
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zofer2
         END_3D
         !
         IF ( ln_p5z ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpkm1)
               ! POC degradation by bacterial activity. It is a function
               ! of the mean lability and of temperature. This also includes
               ! shrinking of particles due to the bacterial activity
               ! --------------------------------------------------------
               zremig = remintgoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
               zopon2 = xremipn / xremipc * zremig * tr(ji,jj,jk,jpgon,Kbb)
               zopop2 = xremipp / xremipc * zremig * tr(ji,jj,jk,jpgop,Kbb)

               ! update of the TRA arrays
               tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) + solgoc * zopon2
               tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) + solgoc * zopop2
               tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zopon2
               tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zopop2
               tr(ji,jj,jk,jpgon,Krhs) = tr(ji,jj,jk,jpgon,Krhs) - zopon2 * (1. + solgoc)
               tr(ji,jj,jk,jpgop,Krhs) = tr(ji,jj,jk,jpgop,Krhs) - zopop2 * (1. + solgoc)
            END_3D
         ENDIF
         ! Remineralisation rate of large particles diag.
         IF( l_dia_remin .AND. knt == nrdttrc ) THEN
            CALL iom_put( "REMING", ( tr(A2D(0),:,jpdoc,Krhs) - zremigoc(:,:,:) ) / &
               &                    ( xstep * tgfunc(:,:,:) * tr(A2D(0),:,jpgoc,Kbb) + rtrn ) * tmask(A2D(0),:) )
            DEALLOCATE ( zremigoc )
         ENDIF
         !
         IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
            WRITE(charout, FMT="('poc1')")
            CALL prt_ctl_info( charout, cdcomp = 'top' )
            CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
         ENDIF
         !
      ENDIF
      !
      ! Lability parameterization for the small OM particles.
      ! ---------------------------------------------------------
      IF( ll_poc_lab ) THEN
         ! Initialisation of the lability distributions that are set to the distribution of newly produced organic particles
         IF(lwp) write(numout,*)
         IF(lwp) write(numout,*) ' Compute variable lability for biogeochemistry at kt =  ',  kt, '  day = ', nday_year
         IF(lwp) write(numout,*) '~~~~~~'
         !
         CALL p4z_poc_lab( kt, Kbb, Kmm )
         !
     ENDIF
     !
     IF( l_dia_remin ) THEN
         ALLOCATE( zremipoc(A2D(0),jpk) )  ;  zremipoc(A2D(0),:) = tr(A2D(0),:,jpdoc,Krhs) 
     ENDIF
     DO_3D( 0, 0, 0, 0, 1, jpkm1)
        ! POC disaggregation by turbulence and bacterial activity.It is a function
        ! of the mean lability and of temperature  
        ! --------------------------------------------------------
        zremip = remintpoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
        zorem  = zremip * tr(ji,jj,jk,jppoc,Kbb)
              
        ! Update of the TRA arrays
        tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zorem
        tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) - zorem
     END_3D
     IF( ln_p2z ) THEN
        DO_3D( 0, 0, 0, 0, 1, jpkm1)
           ! POC disaggregation by turbulence and bacterial activity.It is a function
           ! of the mean lability and of temperature  
           ! --------------------------------------------------------
           zremip = remintpoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
           zorem  = zremip * tr(ji,jj,jk,jppoc,Kbb)
           orem(ji,jj,jk)  =  zorem
           ! Update of the TRA arrays
           tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zorem * feratz
        END_3D
     ELSE
        DO_3D( 0, 0, 0, 0, 1, jpkm1)
           ! POC disaggregation by turbulence and bacterial activity.It is a function
           ! of the mean lability and of temperature  
           ! --------------------------------------------------------
           zremip = remintpoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
           zorem  = zremip * tr(ji,jj,jk,jppoc,Kbb)
           orem(ji,jj,jk)  = orem(ji,jj,jk) + zorem
           zofer  = zremip * tr(ji,jj,jk,jpsfe,Kbb)

           ! Update of the TRA arrays
           tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + zofer
           tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) - zofer
        END_3D
     ENDIF
     IF ( ln_p5z ) THEN
        DO_3D( 0, 0, 0, 0, 1, jpkm1)
           ! POC disaggregation by turbulence and bacterial activity.It is a function
           ! of the mean lability and of temperature  
           !--------------------------------------------------------
           zremip = remintpoc(ji,jj,jk) * xstep * tgfunc(ji,jj,jk)
           zopon  = xremipn / xremipc * zremip * tr(ji,jj,jk,jppon,Kbb)
           zopop  = xremipp / xremipc * zremip * tr(ji,jj,jk,jppop,Kbb)
           
           ! Update of the TRA arrays
           tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) - zopon
           tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) - zopop
           tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zopon 
           tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zopop 
        END_3D
     ENDIF
     IF( l_dia_remin .AND. knt == nrdttrc ) THEN
         ! Remineralisation rate of small particles diag.
         CALL iom_put( "REMINP", ( tr(A2D(0),:,jpdoc,Krhs) - zremipoc(:,:,:) ) / &
              &                  ( xstep * tgfunc(:,:,:) * tr(A2D(0),:,jppoc,Kbb) + rtrn ) * tmask(A2D(0),:) ) 
         ! Remineralisation of biogenic particulate iron
         CALL iom_put( "REMINF", ( tr(A2D(0),:,jpfer,Krhs) - zfolimi(:,:,:) ) * 1.e+9 * 1.e3 * rfact2r * tmask(A2D(0),:) )
        DEALLOCATE ( zfolimi, zremipoc )
     ENDIF

      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('poc2')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_poc')
      !
   END SUBROUTINE p4z_poc

   SUBROUTINE p4z_goc_lab( kt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_poc  ***
      !!
      !! ** Purpose :   Compute remineralization of organic particles
      !!                A reactivity-continuum parameterization is chosen
      !!                to describe the lability of the organic particles
      !!                As a consequence, the remineralisation rates of the 
      !!                the different pools change with time as a function of 
      !!                the lability distribution
      !!
      !! ** Method  : - Computation of the remineralisation rates is performed
      !!                according to reactivity continuum formalism described
      !!                in Aumont et al. (2017). 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt         ! ocean time step and ???
      INTEGER, INTENT(in) ::   Kbb, Kmm  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zsizek, alphat, zremint, alphatm1
      REAL(wp) ::   zpoc1, zpoc2, zfact, zconcpoc
      REAL(wp) ::   zreminp1, zreminp2
      REAL(wp) ::   ztemp, ztemp1, ztemp2
      REAL(wp), DIMENSION(jcpoc) :: alpham1

      DO jn = 1, jcpoc
         alphag(:,:,:,jn) = alphan(jn)
      END DO

     ! Lability parameterization. This is the big particles part (GOC)
     ! This lability parameterization is always active. However, if only one
     ! lability class is specified in the namelist, this is equivalent to 
     ! a standard parameterisation with a constant lability
     ! -----------------------------------------------------------------------
     remintgoc(:,:,:) = xremipc
     DO_2D( 0, 0, 0, 0)
        alpham1(:) = alphan(:)
        DO jk = 2, jpkm1
           IF( tmask(ji,jj,jk) == 1. .AND. gdept(ji,jj,jk,Kmm) > pdep(ji,jj) ) THEN
              !
              ! In the case of GOC, lability is constant in the mixed layer 
              ! It is computed only below the mixed layer depth
              ! ------------------------------------------------------------
              zsizek = e3t(ji,jj,jk,Kmm) / 2. / (wsbio4(ji,jj,jk) + rtrn)
              !
              ! standard algorithm in the rest of the water column
              ! See the comments in the previous block.
              ! ---------------------------------------------------
              zfact  = rday / rfact2 / ( tr(ji,jj,jk,jpgoc,Kbb) + rtrn )
              zpoc1  = MIN(rbound, MAX(-rbound, consgoc(ji,jj,jk) * zfact ) )
              zpoc2  = prodgoc(ji,jj,jk) * rday / rfact2
              !
              zconcpoc = ( e3t(ji,jj,jk-1,Kmm) * tr(ji,jj,jk-1,jpgoc,Kbb) + e3t(ji,jj,jk,Kmm) * tr(ji,jj,jk,jpgoc,Kbb) )   &
                   &        / ( e3t(ji,jj,jk-1,Kmm) + e3t(ji,jj,jk,Kmm) )
              !
              DO jn = 1, jcpoc
                 zreminp1 = reminp(jn) * tgfunc(ji,jj,jk) - zpoc1
                 ztemp    = MIN(rbound, MAX(-rbound,  zreminp1 ) )
                 ztemp1   = EXP( -ztemp * zsizek)
                 ztemp2   = zpoc2 * ( 1. - ztemp1 ) / ztemp * alphan(jn)
                 alphag(ji,jj,jk,jn) = alpham1(jn) * ztemp1 * zconcpoc + ztemp2
                 alpham1(jn) = alphag(ji,jj,jk,jn) * ztemp1 + ztemp2
              END DO

              alphatm1 = SUM( alpham1(:) ) + rtrn
              alphat   = SUM( alphag(ji,jj,jk,:) ) + rtrn
              alphag(ji,jj,jk,:) = alphag(ji,jj,jk,:) / alphat
              alpham1(:)         = alpham1(:) / alphatm1
              !
              ! The contribution of each lability class at the current level is computed
              zremint = SUM( alphag(ji,jj,jk,:) * reminp(:) )
              ! Computation of the mean remineralisation rate
              remintgoc(ji,jj,jk) = MIN( xremipc, zremint )
              !
           ENDIF
        END DO
     END_2D
     !
   END SUBROUTINE p4z_goc_lab

   SUBROUTINE p4z_poc_lab( kt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_poc  ***
      !!
      !! ** Purpose :   Compute remineralization of organic particles
      !!                A reactivity-continuum parameterization is chosen
      !!                to describe the lability of the organic particles
      !!                As a consequence, the remineralisation rates of the 
      !!                the different pools change with time as a function of 
      !!                the lability distribution
      !!
      !! ** Method  : - Computation of the remineralisation rates is performed
      !!                according to reactivity continuum formalism described
      !!                in Aumont et al. (2017). 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt         ! ocean time step and ???
      INTEGER, INTENT(in) ::   Kbb, Kmm  ! time level indices
      !
      INTEGER  ::   ji, jj, jk, jn
      REAL(wp) ::   zsizek, alphat, zremint, alphatm1
      REAL(wp) ::   zpoc1, zpoc2, zpoc3, zfact, zconcpoc
      REAL(wp) ::   zreminp1, zreminp2
      REAL(wp) ::   ztemp, ztemp1, ztemp2, ztemp3
      REAL(wp), DIMENSION(A2D(0)  )   :: ztotprod, ztotthick, ztotcons
      REAL(wp), DIMENSION(jcpoc) :: alpham1

      DO jn = 1, jcpoc
         alphap(:,:,:,jn) = alphan(jn)
      END DO

     ! Lability parameterization for the small OM particles. This param
     ! is based on the same theoretical background as the big particles.
     ! However, because of its low sinking speed, lability is not supposed
     ! to be equal to its initial value (the value of the freshly produced
     ! organic matte) in the MLD. It is however uniform in the mixed layer.
     ! ---------------------------------------------------------------------
     ztotprod (:,:)    = 0.
     ztotthick(:,:)    = 0.
     ztotcons (:,:)    = 0.
     remintpoc(:,:,:) = xremipc

     ! intregrated production and consumption of POC in the mixed layer
     ! ----------------------------------------------------------------
     DO_3D( 0, 0, 0, 0, 1, jpkm1)
        IF (tmask(ji,jj,jk) == 1. .AND. gdept(ji,jj,jk,Kmm) <= pdep(ji,jj) ) THEN
          zfact = e3t(ji,jj,jk,Kmm) * rday / rfact2
          ztotprod(ji,jj)  = ztotprod(ji,jj) + prodpoc(ji,jj,jk) * zfact
          ! The temperature effect is included here
          ztotthick(ji,jj) = ztotthick(ji,jj) + e3t(ji,jj,jk,Kmm) * tgfunc(ji,jj,jk)
          ztotcons(ji,jj)  = ztotcons(ji,jj) - conspoc(ji,jj,jk) * zfact / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn )
        ENDIF
     END_3D

     ! Computation of the lability spectrum in the mixed layer. In the mixed
     ! layer, this spectrum is supposed to be uniform as a result of intense
     ! mixing.
     ! ---------------------------------------------------------------------
     DO_3D( 0, 0, 0, 0, 1, jpkm1)
        IF (tmask(ji,jj,jk) == 1. .AND. gdept(ji,jj,jk,Kmm) <= pdep(ji,jj) ) THEN
           DO jn = 1, jcpoc
             ! For each lability class, the system is supposed to be
             ! at equilibrium: Prod - Sink - w alphap = 0.
             alphap(ji,jj,jk,jn) = ztotprod(ji,jj) * alphan(jn) / ( reminp(jn)    &
             &                     * ztotthick(ji,jj) + ztotcons(ji,jj) + wsbio3(ji,jj,jk) + rtrn )
             alphap(ji,jj,jk,jn) = MAX(0., alphap(ji,jj,jk,jn) )
          END DO
          alphat = SUM( alphap(ji,jj,jk,:) ) + rtrn
          alphap(ji,jj,jk,:)  = alphap(ji,jj,jk,:) / alphat
          zremint = SUM( alphap(ji,jj,jk,:) * reminp(:) )
          ! Mean remineralization rate in the mixed layer
          remintpoc(ji,jj,jk) =  MIN( xremipc, zremint )
        ENDIF
     END_3D
     !
     !
     ! The lability parameterization is used here. The code is here
     ! almost identical to what is done for big particles. The only difference
     ! is that an additional source from GOC to POC is included. This means
     ! that since we need the lability spectrum of GOC, GOC spectrum
     ! should be determined before.
     ! -----------------------------------------------------------------------
     DO_2D( 0, 0, 0, 0)
        alpham1(:) = alphap(ji,jj,1,:)
        DO jk = 2, jpkm1
           IF (tmask(ji,jj,jk) == 1. .AND. gdept(ji,jj,jk,Kmm) > pdep(ji,jj) ) THEN
              ! the scale factors are corrected with temperature
              zsizek  = e3t(ji,jj,jk,Kmm) / 2. / (wsbio3(ji,jj,jk) + rtrn)
              !
              ! Special treatment of the level just below the MXL
              ! See the comments in the GOC section
              zfact  = rday / rfact2 / ( tr(ji,jj,jk,jppoc,Kbb) + rtrn )
              zpoc1  = MIN(rbound, MAX(-rbound, conspoc(ji,jj,jk) * zfact ) )
              zpoc2  = prodpoc(ji,jj,jk) * rday / rfact2
              zpoc3  = orem3 (ji,jj,jk) * rday / rfact2
              !
              zconcpoc = ( e3t(ji,jj,jk-1,Kmm) * tr(ji,jj,jk-1,jppoc,Kbb) + e3t(ji,jj,jk,Kmm) * tr(ji,jj,jk,jppoc,Kbb) )   &
                &        / ( e3t(ji,jj,jk-1,Kmm) + e3t(ji,jj,jk,Kmm) )
              !
              DO jn = 1, jcpoc
                 zreminp1 = reminp(jn) * tgfunc(ji,jj,jk) - zpoc1
                 ztemp    = MIN(rbound, MAX(-rbound,  zreminp1 ) )
                 ztemp1   = EXP( MIN(rbound,-ztemp * zsizek ) )
                 ztemp2   = zpoc2 * ( 1. - ztemp1 ) / ztemp * alphan(jn)
                 ztemp3   = zpoc3 * ( 1. - ztemp1 ) / ztemp * alphag(ji,jj,jk,jn)
                 alphap(ji,jj,jk,jn) = alpham1(jn) * ztemp1 * zconcpoc + ztemp2 + ztemp3
                 alpham1(jn) = alphap(ji,jj,jk,jn) * ztemp1 + ztemp2 + ztemp3
              END DO
              !
              alphat = SUM( alphap(ji,jj,jk,:) ) + rtrn
              alphatm1 = SUM( alpham1(:) ) + rtrn
              alphap(ji,jj,jk,:) = alphap(ji,jj,jk,:) / alphat
              alpham1(:) = alpham1(:) / alphatm1
              ! The contribution of each lability class at the current level is computed
              zremint = SUM( alphap(ji,jj,jk,:) * reminp(:) )
              ! Computation of the mean remineralisation rate
              remintpoc(ji,jj,jk) =  MIN( xremipc, zremint )
           ENDIF
        END DO
     END_2D
     !
   END SUBROUTINE p4z_poc_lab

   SUBROUTINE p4z_poc_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_poc_init  ***
      !!
      !! ** Purpose :   Initialization of remineralization parameters
      !!
      !! ** Method  :   Read the nampispoc namelist and check the parameters
      !!              called at the first timestep
      !!
      !! ** input   :   Namelist nampispoc
      !!----------------------------------------------------------------------
      INTEGER ::   jn            ! dummy loop index
      INTEGER ::   ios           ! Local integer
      REAL(wp)::   zremindelta, zreminup, zremindown
      !!
      NAMELIST/nampispoc/ jcpoc  , rshape,  &
         &                xremipc, xremipn, xremipp
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_poc_init : Initialization of remineralization parameters'
         WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF
      !
      READ_NML_REF(numnatp,nampispoc)
      READ_NML_CFG(numnatp,nampispoc)
      IF(lwm) WRITE( numonp, nampispoc )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampispoc'
         WRITE(numout,*) '      remineralisation rate of POC              xremipc   =', xremipc
         IF( ln_p5z ) THEN 
            WRITE(numout,*) '      remineralisation rate of PON              xremipn   =', xremipn
            WRITE(numout,*) '      remineralisation rate of POP              xremipp   =', xremipp
         ENDIF
         WRITE(numout,*) '      Number of lability classes for POC        jcpoc     =', jcpoc
         WRITE(numout,*) '      Shape factor of the gamma distribution    rshape    =', rshape
      ENDIF
      !
      ! Discretization along the lability space
      ! ---------------------------------------
      !
      ALLOCATE( alphan(jcpoc) , reminp(jcpoc) )
      ALLOCATE( alphap(A2D(0),jpk,jcpoc), alphag(A2D(0),jpk,jcpoc) )
      ALLOCATE( orem3(A2D(0),jpk) )
      !
      IF (jcpoc > 1) THEN  ! Case when more than one lability class is used
         !
         zremindelta = LOG(4. * 1000. ) / REAL(jcpoc-1, wp)
         zreminup = 1./ 400. * EXP(zremindelta)
         !
         ! Discretization based on incomplete gamma functions
         ! As incomplete gamma functions are not available in standard 
         ! fortran 95, they have been coded as functions in this module (gamain)
         ! ---------------------------------------------------------------------
         !
         alphan(1) = gamain(zreminup, rshape)
         reminp(1) = gamain(zreminup, rshape+1.0) * xremipc / alphan(1)
         DO jn = 2, jcpoc-1
            zreminup = 1./ 400. * EXP( REAL(jn, wp) * zremindelta)
            zremindown = 1. / 400. * EXP( REAL(jn-1, wp) * zremindelta)
            alphan(jn) = gamain(zreminup, rshape) - gamain(zremindown, rshape)
            reminp(jn) = gamain(zreminup, rshape+1.0) - gamain(zremindown, rshape+1.0)
            reminp(jn) = reminp(jn) * xremipc / alphan(jn)
         END DO
         zremindown = 1. / 400. * EXP( REAL(jcpoc-1, wp) * zremindelta)
         alphan(jcpoc) = 1.0 - gamain(zremindown, rshape)
         reminp(jcpoc) = 1.0 - gamain(zremindown, rshape+1.0)
         reminp(jcpoc) = reminp(jcpoc) * xremipc / alphan(jcpoc)

      ELSE  ! Only one lability class is used
         alphan(jcpoc) = 1.
         reminp(jcpoc) = xremipc
      ENDIF

      DO jn = 1, jcpoc
         alphap(:,:,:,jn) = alphan(jn)
         alphag(:,:,:,jn) = alphan(jn)
      END DO

      ! Here we compute the GOC -> POC rate due to the shrinking
      ! of the fecal pellets/aggregates as a result of bacterial
      ! solubilization
      ! This is based on a fractal dimension of 2.56 and a spectral
      ! slope of -3.6 (identical to what is used in p4zsink to compute
      ! aggregation
      solgoc = 0.04/ 2.56 * 1./ ( 1.-50**(-0.04) )
      !
      rbound = 1.e+01_wp
      !
      orem3(:,:,:) = 0.

   END SUBROUTINE p4z_poc_init

   SUBROUTINE trc_ini_inv( Kmm )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_ini_stat  ***
      !! ** Purpose :      passive tracers inventories at initialsation phase
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   Kmm    ! time level index
      INTEGER             ::  ji, jj, jk, jn  ! dummy loop indices
      CHARACTER (len=25) :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: z4d
      REAL(wp), ALLOCATABLE, DIMENSION(:) :: ztra

      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'trc_ini_inv : initial passive tracers inventories'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      !
      ALLOCATE( z4d(jpi,jpj,jpk,jptra+1), ztra(jptra+1) )
      !                          ! masked grid volume
      DO jk = 1, jpk
         cvol(:,:,jk) = e1e2t(:,:) * e3t(:,:,jk,Kmm) * tmask(:,:,jk)
      END DO
      !
      DO jn = 1, jptra
         z4d(:,:,:,jn) = tr(:,:,:,jn,Kmm) * cvol(:,:,:)
      ENDDO
      z4d(:,:,:,jptra+1)  = cvol(:,:,:)
      !
      ztra(1:jptra+1) = glob_3Dsum( 'trcini', z4d(:,:,:,1:jptra+1) )
      !
      trai(1:jptra) = ztra(1:jptra)      !  initial content of all tracers
      areatot       = ztra(jptra+1)      ! total volume of the ocean
      !
      IF(lwp) THEN               ! control print
         WRITE(numout,*)
         WRITE(numout,*) '   ==>>>   Total number of passive tracer jptra = ', jptra
         WRITE(numout,*) '           Total volume of ocean                = ', areatot
         WRITE(numout,*) '           Total inital content of all tracers '
         WRITE(numout,*)
         DO jn = 1, jptra
            WRITE(numout,9000) jn, TRIM( ctrcnm(jn) ), trai(jn)
         ENDDO
         WRITE(numout,*)
      ENDIF
      IF(lwp) WRITE(numout,*)
      IF(sn_cfctl%l_prttrc) THEN            ! print mean trends (used for debugging)
         CALL prt_ctl_init( 'top', jptra )
         WRITE(charout, FMT="('ini ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl( tab4d_1=tr(:,:,:,:,Kmm), mask1=tmask, clinfo=ctrcnm )
      ENDIF
9000  FORMAT('      tracer nb : ',i2,'      name :',a10,'      initial content :',e18.10)
      !
      DEALLOCATE( z4d, ztra )
      !
   END SUBROUTINE trc_ini_inv

      !
   ELEMENTAL FUNCTION alngam( xvalue )
      !*****************************************************************************80
      !
      !! ALNGAM computes the logarithm of the gamma function.
      !
      !  Modified:    13 January 2008
      !
      !  Author  :    Allan Macleod
      !               FORTRAN90 version by John Burkardt
      !
      !  Reference:
      !    Allan Macleod, Algorithm AS 245,
      !    A Robust and Reliable Algorithm for the Logarithm of the Gamma Function,
      !    Applied Statistics,
      !    Volume 38, Number 2, 1989, pages 397-402.
      !
      !  Parameters:
      !
      !    Input, real ( kind = 8 ) XVALUE, the argument of the Gamma function.
      !
      !    0, no error occurred.
      !    1, XVALUE is less than or equal to 0.
      !    2, XVALUE is too big.
      !
      !    Output, real ( kind = 8 ) ALNGAM, the logarithm of the gamma function of X.
      !*****************************************************************************80
  REAL(wp) :: alngam
  real(wp), parameter :: alr2pi = 0.918938533204673E+00
  real(wp), parameter, dimension ( 9 ) :: r1 = (/ &
    -2.66685511495E+00, &
    -24.4387534237E+00, &
    -21.9698958928E+00, &
     11.1667541262E+00, &
     3.13060547623E+00, &
     0.607771387771E+00, &
     11.9400905721E+00, &
     31.4690115749E+00, &
     15.2346874070E+00 /)
  real(wp), parameter, dimension ( 9 ) :: r2 = (/ &
    -78.3359299449E+00, &
    -142.046296688E+00, &
     137.519416416E+00, &
     78.6994924154E+00, &
     4.16438922228E+00, &
     47.0668766060E+00, &
     313.399215894E+00, &
     263.505074721E+00, &
     43.3400022514E+00 /)
  real(wp), parameter, dimension ( 9 ) :: r3 = (/ &
    -2.12159572323E+05, &
     2.30661510616E+05, &
     2.74647644705E+04, &
    -4.02621119975E+04, &
    -2.29660729780E+03, &
    -1.16328495004E+05, &
    -1.46025937511E+05, &
    -2.42357409629E+04, &
    -5.70691009324E+02 /)
  real(wp), parameter, dimension ( 5 ) :: r4 = (/ &
     0.279195317918525E+00, &
     0.4917317610505968E+00, &
     0.0692910599291889E+00, &
     3.350343815022304E+00, &
     6.012459259764103E+00 /)
  real (wp) :: x
  real (wp) :: x1
  real (wp) :: x2
  real (wp), parameter :: xlge = 5.10E+05
  real (wp), parameter :: xlgst = 1.0E+30
  REAL (wp), INTENT(in) :: xvalue
  real (wp) :: y

  x = xvalue
  alngam = 0.0E+00
!
!  Check the input.
!
  if ( xlgst <= x ) then
    return
  end if
  if ( x <= 0.0E+00 ) then
    return
  end if

!
!  Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined.
!
  if ( x < 1.5E+00 ) then

    if ( x < 0.5E+00 ) then
      alngam = - log ( x )
      y = x + 1.0E+00
!
!  Test whether X < machine epsilon.
!
      if ( y == 1.0E+00 ) then
        return
      end if

    else

      alngam = 0.0E+00
      y = x
      x = ( x - 0.5E+00 ) - 0.5E+00

    end if

    alngam = alngam + x * (((( &
        r1(5)   * y &
      + r1(4) ) * y &
      + r1(3) ) * y &
      + r1(2) ) * y &
      + r1(1) ) / (((( &
                  y &
      + r1(9) ) * y &
      + r1(8) ) * y &
      + r1(7) ) * y &
      + r1(6) )

    return

  end if
!
!  Calculation for 1.5 <= X < 4.0.
!
  if ( x < 4.0E+00 ) then

    y = ( x - 1.0E+00 ) - 1.0E+00

    alngam = y * (((( &
        r2(5)   * x &
      + r2(4) ) * x &
      + r2(3) ) * x &
      + r2(2) ) * x &
      + r2(1) ) / (((( &
                  x &
      + r2(9) ) * x &
      + r2(8) ) * x &
      + r2(7) ) * x &
      + r2(6) )
!
!  Calculation for 4.0 <= X < 12.0.
!
  else if ( x < 12.0E+00 ) then

    alngam = (((( &
        r3(5)   * x &
      + r3(4) ) * x &
      + r3(3) ) * x &
      + r3(2) ) * x &
      + r3(1) ) / (((( &
                  x &
      + r3(9) ) * x &
      + r3(8) ) * x &
      + r3(7) ) * x &
      + r3(6) )
!
!  Calculation for 12.0 <= X.
!
  else

    y = log ( x )
    alngam = x * ( y - 1.0E+00 ) - 0.5E+00 * y + alr2pi

    if ( x <= xlge ) then

      x1 = 1.0E+00 / x
      x2 = x1 * x1

      alngam = alngam + x1 * ( ( &
             r4(3)   * &
        x2 + r4(2) ) * &
        x2 + r4(1) ) / ( ( &
        x2 + r4(5) ) * &
        x2 + r4(4) )

    end if

   end if

   END FUNCTION alngam


   ELEMENTAL FUNCTION gamain( x, p )
!*****************************************************************************80
!
!! GAMAIN computes the incomplete gamma ratio.
!
!  Discussion:
!
!    A series expansion is used if P > X or X <= 1.  Otherwise, a
!    continued fraction approximation is used.
!
!  Modified:
!
!    17 January 2008
!
!  Author:
!
!    G Bhattacharjee
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    G Bhattacharjee,
!    Algorithm AS 32:
!    The Incomplete Gamma Integral,
!    Applied Statistics,
!    Volume 19, Number 3, 1970, pages 285-287.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, P, the parameters of the incomplete 
!    gamma ratio.  0 <= X, and 0 < P.
!
!    0, no errors.
!    1, P <= 0.
!    2, X < 0.
!    3, underflow.
!    4, error return from the Log Gamma routine.
!
!    Output, real ( kind = 8 ) GAMAIN, the value of the incomplete
!    gamma ratio.
!

  real(wp) ::  gamain
  real(wp) ::  a
  real(wp), parameter :: acu = 1.0E-08
  real(wp) ::  an
  real(wp) ::  arg
  real(wp) ::  b
  real(wp) ::  dif
  real(wp) ::  factor
  real(wp) ::  g
  real(wp) ::  gin
  integer ::  i
  real(wp), parameter :: oflo = 1.0E+37
  REAL(wp), INTENT(in) :: p
  real(wp) ::  pn(6)
  real(wp) ::  rn
  real(wp) :: term
  real(wp), parameter :: uflo = 1.0E-37
  REAL(wp), intent(in) :: x
!
!  Check the input.
!
  if ( p <= 0.0E+00 ) then
    gamain = 0.0E+00
    return
  end if

  if ( x < 0.0E+00 ) then
    gamain = 0.0E+00
    return
  end if

  if ( x == 0.0E+00 ) then
    gamain = 0.0E+00
    return
  end if

  g = alngam ( p )

  arg = p * log ( x ) - x - g

  if ( arg < log ( uflo ) ) then
    gamain = 0.0E+00
    return
  end if

  factor = exp ( arg )
!
!  Calculation by series expansion.
!
  if ( x <= 1.0E+00 .or. x < p ) then

    gin = 1.0E+00
    term = 1.0E+00
    rn = p

    do

      rn = rn + 1.0E+00
      term = term * x / rn
      gin = gin + term

      if ( term <= acu ) then
        exit
      end if

    end do

    gamain = gin * factor / p
    return

  end if
!
!  Calculation by continued fraction.
!
  a = 1.0E+00 - p
  b = a + x + 1.0E+00
  term = 0.0E+00

  pn(1) = 1.0E+00
  pn(2) = x
  pn(3) = x + 1.0E+00
  pn(4) = x * b

  gin = pn(3) / pn(4)

  do

    a = a + 1.0E+00
    b = b + 2.0E+00
    term = term + 1.0E+00
    an = a * term
    do i = 1, 2
      pn(i+4) = b * pn(i+2) - an * pn(i)
    end do

    if ( pn(6) /= 0.0E+00 ) then

      rn = pn(5) / pn(6)
      dif = abs ( gin - rn )
!
!  Absolute error tolerance satisfied?
!
      if ( dif <= acu ) then
!
!  Relative error tolerance satisfied?
!
        if ( dif <= acu * rn ) then
          gamain = 1.0E+00 - factor * gin
          exit
        end if

      end if

      gin = rn

    end if

    do i = 1, 4
      pn(i) = pn(i+2)
    end do
    if ( oflo <= abs ( pn(5) ) ) then

      do i = 1, 4
        pn(i) = pn(i) / oflo
      end do

    end if

  end do

  END FUNCTION gamain

   !!======================================================================
END MODULE p4zpoc
