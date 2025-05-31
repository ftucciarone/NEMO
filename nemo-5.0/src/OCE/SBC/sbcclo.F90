MODULE sbcclo
   !!======================================================================
   !!                       ***  MODULE  sbcclo  ***
   !! Ocean forcing: redistribution of emp unbalance over closed sea into river mouth or open ocean
   !!=====================================================================
   !! History :  4.0 and earlier ! see closea.F90 history   
   !!   NEMO     4.1  ! 2019-09  (P. Mathiot) rewrite sbc_clo module to match new closed sea mask definition (original sbcclo.F90)
   !! 
   !!----------------------------------------------------------------------
   !
   !!----------------------------------------------------------------------
   !!   Public subroutines:
   !!   sbc_clo       : update emp and qns over target area and source area
   !!   sbc_clo_init  : initialise all variable needed for closed sea correction
   !!
   !!   Private subroutines:
   !!   alloc_csarr   : allocate closed sea array
   !!   get_cssrcsurf : compute source surface area
   !!   get_cstrgsurf : compute target surface area
   !!   prt_csctl     : closed sea control print
   !!   sbc_csupdate  : compute net fw from closed sea
   !!----------------------------------------------------------------------
   !
   USE closea                                  ! closed sea 
   USE in_out_manager                          ! I/O manager
   !
   USE dom_oce,     ONLY: e1e2t                ! ocean space and time domain
   USE phycst ,     ONLY: rcp                  ! physical constants
   USE sbc_oce,     ONLY: emp, qns, ln_rnf, rnf, sst_m ! ocean surface boundary conditions
   USE iom    ,     ONLY: iom_put              ! I/O routines
   USE lib_fortran, ONLY: glob_2Dsum           ! fortran library
   USE lib_mpp    , ONLY: mpp_min, ctl_stop    ! MPP library
   !
   IMPLICIT NONE
   !
   PRIVATE
   !
   PUBLIC sbc_clo
   PUBLIC sbc_clo_init
   !
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:) :: rsurfsrcg, rsurftrgg      !: closed sea source/target glo surface areas 
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:) :: rsurfsrcr, rsurftrgr      !: closed sea source/target rnf surface areas 
   REAL(wp), SAVE, ALLOCATABLE, DIMENSION(:) :: rsurfsrce, rsurftrge      !: closed sea source/target emp surface areas 
   !
   INTEGER, SAVE, ALLOCATABLE, DIMENSION(:)  :: mcsgrpg, mcsgrpr, mcsgrpe !: closed sea group for glo, rnf and emp
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
   CONTAINS
   !
   !!----------------------------------------------------------------------
   !!  Public subroutines
   !!----------------------------------------------------------------------
   !
   SUBROUTINE sbc_clo_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_clo_init  ***
      !!                    
      !! ** Purpose :  Initialisation of the variable needed for the net fw closed sea correction
      !!
      !! ** Method  :  - compute source surface area for each closed sea
      !!               - defined the group of each closed sea 
      !!                 (needed to manage multiple closed sea and one target area like great lakes / St Laurent outlet)
      !!               - compute target surface area
      !!----------------------------------------------------------------------
      !
      ! 0. Allocate cs variables (surf)
      CALL alloc_csarr( ncsg, rsurfsrcg, rsurftrgg, mcsgrpg ) 
      CALL alloc_csarr( ncsr, rsurfsrcr, rsurftrgr, mcsgrpr )
      CALL alloc_csarr( ncse, rsurfsrce, rsurftrge, mcsgrpe )
      !
      ! 1. compute source surface area
      CALL get_cssrcsurf( ncsg, mask_csglo, rsurfsrcg )
      CALL get_cssrcsurf( ncsr, mask_csrnf, rsurfsrcr )
      CALL get_cssrcsurf( ncse, mask_csemp, rsurfsrce )
      !
      ! 2. compute target surface area and group number (mcsgrp) for all cs and cases 
      ! glo could be simpler but for lisibility, all treated the same way
      ! It is only done once, so not a big deal
      CALL get_cstrgsurf( ncsg, mask_csglo, mask_csgrpglo, rsurftrgg, mcsgrpg )
      CALL get_cstrgsurf( ncsr, mask_csrnf, mask_csgrprnf, rsurftrgr, mcsgrpr )
      CALL get_cstrgsurf( ncse, mask_csemp, mask_csgrpemp, rsurftrge, mcsgrpe )
      ! 
      ! 3. print out in ocean.ouput
      IF ( lwp ) WRITE(numout,*) 'sbc_clo_init : compute surface area for source (closed sea) and target (river mouth)'
      IF ( lwp ) WRITE(numout,*) '~~~~~~~~~~~~~~'
      CALL prt_csctl( ncsg, rsurfsrcg, rsurftrgg, mcsgrpg, 'glo' )
      CALL prt_csctl( ncsr, rsurfsrcr, rsurftrgr, mcsgrpr, 'rnf' )
      CALL prt_csctl( ncse, rsurfsrce, rsurftrge, mcsgrpe, 'emp' )

   END SUBROUTINE sbc_clo_init

   SUBROUTINE sbc_clo( kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_clo  ***
      !!                    
      !! ** Purpose :   Special handling of closed seas
      !!
      !! ** Method  :   Water flux is forced to zero over closed sea
      !!      Excess is shared between remaining ocean, or
      !!      put as run-off in open ocean.
      !!
      !! ** Action  : - compute surface freshwater fluxes and associated heat content flux at kt
      !!              - output closed sea contribution to fw and heat budget
      !!              - update emp and qns
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in   ) ::   kt       ! ocean model time step
      !
      REAL(wp), DIMENSION(jpi,jpj) :: zwcs, zqcs    ! water flux and heat flux correction due to closed seas
      !!----------------------------------------------------------------------
      !
      ! 0. initialisation
      zwcs(:,:) = 0._wp ; zqcs(:,:) = 0._wp
      !
      ! 1. update emp and qns
      CALL sbc_csupdate( ncsg, mcsgrpg, mask_csglo, mask_csgrpglo, rsurfsrcg, rsurftrgg, 'glo', mask_opnsea, rsurftrgg, zwcs, zqcs )
      CALL sbc_csupdate( ncsr, mcsgrpr, mask_csrnf, mask_csgrprnf, rsurfsrcr, rsurftrgr, 'rnf', mask_opnsea, rsurftrgg, zwcs, zqcs )
      CALL sbc_csupdate( ncse, mcsgrpe, mask_csemp, mask_csgrpemp, rsurfsrce, rsurftrge, 'emp', mask_opnsea, rsurftrgg, zwcs, zqcs )
      !
      ! 2. ouput closed sea contributions
      CALL iom_put('wclosea',zwcs)
      CALL iom_put('qclosea',zqcs)
      !
      ! 3. update emp and qns
      emp(A2D(0)) = emp(A2D(0)) + zwcs(A2D(0))
      qns(:,:)    = qns(:,:)    + zqcs(A2D(0))
      !
   END SUBROUTINE sbc_clo
   !
   !!----------------------------------------------------------------------
   !!  Private subroutines
   !!----------------------------------------------------------------------
   !
   SUBROUTINE get_cssrcsurf(kncs, kmaskcs, psurfsrc)
      !!-----------------------------------------------------------------------
      !!                  ***  routine get_cssrcsurf  ***
      !!
      !! ** Purpose : compute closed sea (source) surface area
      !!----------------------------------------------------------------------
      INTEGER ,                 INTENT(in   ) ::   kncs          ! closed sea number
      INTEGER , DIMENSION(:,:), INTENT(in   ) ::   kmaskcs       ! closed sea mask
      REAL(wp), DIMENSION(:)  , INTENT(  out) ::   psurfsrc      ! source surface area
      ! local variables
      INTEGER ::   jcs                                           ! loop index
      REAL(wp), DIMENSION(jpi,jpj,kncs) ::   zmsksrc        ! source mask
      !!----------------------------------------------------------------------
      !
      ! 0. build river mouth mask for this lake
      DO jcs = 1, kncs  ! loop over closed seas
         zmsksrc(:,:,jcs) = MERGE( e1e2t(:,:), 0._wp, kmaskcs == jcs )
      END DO
      !
      ! 1. compute target area
      psurfsrc(1:kncs) = glob_2Dsum('closea', zmsksrc )
      !
   END SUBROUTINE get_cssrcsurf

   SUBROUTINE get_cstrgsurf(kncs, kmaskcs, kmaskcsgrp, psurftrg, kcsgrp )
      !!-----------------------------------------------------------------------
      !!                  ***  routine get_cstrgsurf  ***
      !!
      !! ** Purpose : compute closed sea (target) surface area
      !!----------------------------------------------------------------------
      ! input
      INTEGER,                 INTENT(in   ) ::   kncs                 ! closed sea number
      INTEGER, DIMENSION(:,:), INTENT(in   ) ::   kmaskcs, kmaskcsgrp  ! closed sea and group mask
      ! output
      INTEGER , DIMENSION(:) , INTENT(  out) ::   kcsgrp               ! closed sea group number
      REAL(wp), DIMENSION(:) , INTENT(  out) ::   psurftrg             ! target surface area
      !
      ! local variables
      INTEGER ::   jcs
      INTEGER , DIMENSION(kncs)         ::   jtmp
      INTEGER , DIMENSION(jpi,jpj)      ::   imsk    ! tmp mask
      REAL(wp), DIMENSION(jpi,jpj,kncs) ::   zmsktrg ! tmp target mask
      !!----------------------------------------------------------------------
      !
      ! 0. find group number for cs number jcs
      DO jcs = 1, kncs  ! loop over closed seas
         !
         ! set cs value where cs is defined
         ! imsk = HUGE outside the cs id jcs
         imsk(:,:) = MERGE( jcs, HUGE(1), kmaskcs(:,:) == jcs )
         !
         ! jtmp = jcs - group id for this lake
         imsk(:,:) = imsk(:,:) - kmaskcsgrp(:,:)
         jtmp(jcs) = MINVAL( imsk(:,:) )
      ENDDO
      CALL mpp_min( 'closea', jtmp(1:kncs) )

      ! kcsgrp = group id corresponding to the cs id jcs
      ! kcsgrp(jcs)=(jcs - (jcs - group id))=group id
      DO jcs = 1, kncs  ! loop over closed seas
         kcsgrp(jcs) = jcs - jtmp(jcs)
      ENDDO
      !
      ! 1. build the target river mouth mask for this lake
      DO jcs = 1, kncs  ! loop over closed seas
         zmsktrg(:,:,jcs) = MERGE( e1e2t(:,:), 0._wp, kmaskcsgrp(:,:) * mask_opnsea(:,:) == kcsgrp(jcs) )
      END DO
      
      ! 2. compute target area
      psurftrg(1:kncs) = glob_2Dsum( 'closea', zmsktrg )

   END SUBROUTINE get_cstrgsurf

   SUBROUTINE prt_csctl(kncs, psurfsrc, psurftrg, kcsgrp, cdcstype)
      !!-----------------------------------------------------------------------
      !!                  ***  routine prt_csctl  ***
      !!
      !! ** Purpose : output information about each closed sea (src id, trg id, src area and trg area)
      !!----------------------------------------------------------------------
      ! subroutine parameters
      INTEGER,                INTENT(in   ) :: kncs                ! closed sea number                
      INTEGER, DIMENSION(:) , INTENT(in   ) :: kcsgrp              ! closed sea group number
      !
      REAL(wp), DIMENSION(:), INTENT(in   ) :: psurfsrc, psurftrg  ! source and target surface area
      !
      CHARACTER(LEN=3)      , INTENT(in   ) :: cdcstype            ! closed sea scheme used for redistribution
      !!----------------------------------------------------------------------
      ! local variable
      INTEGER :: jcs
      !!----------------------------------------------------------------------
      !
      IF ( lwp .AND. kncs > 0 ) THEN
         WRITE(numout,*)''
         !
         WRITE(numout,*)'Closed sea target ',TRIM(cdcstype),' : '
         !
         DO jcs = 1,kncs
            WRITE(numout,FMT='(3a,i3,a,i3)') ' ',TRIM(cdcstype),' closed sea id is ',jcs,' and trg group id is : ', kcsgrp(jcs)
            WRITE(numout,FMT='(a,f12.2)'   ) ' src surface areas (km2) : ', psurfsrc(jcs) * 1.0e-6
            WRITE(numout,FMT='(a,f12.2)'   ) ' trg surface areas (km2) : ', psurftrg(jcs) * 1.0e-6
         END DO
         !
         WRITE(numout,*)''
      END IF

   END SUBROUTINE prt_csctl

   SUBROUTINE sbc_csupdate(kncs, kcsgrp, kmsk_src, kmsk_grp, psurfsrc, psurftrg, cdcstype, kmsk_opnsea, psurf_opnsea, pwcs, pqcs)
      !!-----------------------------------------------------------------------
      !!                  ***  routine sbc_csupdate  ***
      !!
      !! ** Purpose : - compute the net freshwater fluxes over each closed seas
      !!              - apply correction to closed sea source/target net fwf accordingly
      !!----------------------------------------------------------------------
      CHARACTER(LEN=3)        , INTENT(in   ) ::   cdcstype  ! closed sea scheme used for redistribution
      !
      INTEGER,                  INTENT(in)    ::   kncs                                 ! closed sea id
      INTEGER, DIMENSION(:)   , INTENT(in)    ::   kcsgrp                               ! closed sea group id
      INTEGER, DIMENSION(:,:) , INTENT(in)    ::   kmsk_src, kmsk_grp, kmsk_opnsea      ! source, target, open ocean mask 
      REAL(wp), DIMENSION(:)  , INTENT(in   ) ::   psurfsrc, psurftrg, psurf_opnsea     ! source, target and open ocean surface area
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   pwcs, pqcs                           ! water and heat flux correction due to closed seas
      !
      ! local variables
      INTEGER ::   jcs                                     ! loop index over closed sea 
      INTEGER , DIMENSION(jpi,jpj,kncs) ::   imsk_src, imsk_trg  ! tmp array source and target closed sea masks
      REAL(wp), DIMENSION(A2D(0) ,kncs) ::   zmsk_src            ! tmp array source and target closed sea masks
      REAL(wp), DIMENSION(A2D(0)      ) ::   zemp
      REAL(wp), DIMENSION(kncs) ::   zcsfw, zcsh, zsurftrg       ! total fresh water and associated heat over one closed sea
      REAL(wp) ::   zcsfwf                                                 ! mean fresh water flux over one closed sea
      !!----------------------------------------------------------------------
      !
      ! 0. get mask and surface of the closed sea
                     zemp(A2D(0)) =                emp(A2D(0))
      IF( ln_rnf )   zemp(A2D(0)) = zemp(A2D(0)) - rnf(A2D(0))
                     zemp(A2D(0)) = e1e2t(A2D(0)) * zemp(A2D(0))
      DO jcs = 1, kncs  ! loop over closed seas
         !
         imsk_src(:,:,jcs) = MERGE( 1, 0, kmsk_src(:,:) == jcs )
         !
         zmsk_src(:,:,jcs) = zemp(A2D(0)) * imsk_src(A2D(0),jcs)
         !
      ENDDO
      ! 1. Work out net freshwater over the closed sea from EMP - RNF.
      zcsfw(:) = glob_2Dsum( 'closea', zmsk_src, cdelay = 'cs1_'//cdcstype )
      
      ! 2. Work out net heat associated with the correction (needed for conservation)
      DO jcs = 1, kncs  ! loop over closed seas
         !
         ! Deal with runoff special case (net evaporation spread globally) and compute trg mask
         IF( cdcstype == 'rnf' .AND. zcsfw(jcs)  > 0._wp ) THEN
            zsurftrg(jcs)     = psurf_opnsea(1)                ! change the target area surface
            imsk_trg(:,:,jcs) = kcsgrp(jcs) * kmsk_opnsea(:,:) ! trg mask is now the open sea mask
         ELSE
            zsurftrg(jcs)     = psurftrg(jcs)
            imsk_trg(:,:,jcs) = kmsk_grp(:,:) * kmsk_opnsea(:,:)
         END IF
         !
         ! Subtract residuals from source points
         IF( zsurftrg(jcs) > 0._wp ) THEN  ; zcsfwf = zcsfw(jcs) / psurfsrc(jcs)
         ELSE                              ; zcsfwf = 0._wp
         ENDIF
         pwcs(:,:)   = pwcs(:,:) -       zcsfwf              * imsk_src(:,:,jcs)
         pqcs(:,:)   = pqcs(:,:) + rcp * zcsfwf * sst_m(:,:) * imsk_src(:,:,jcs)
         !
         zmsk_src(:,:,jcs) = e1e2t(A2D(0)) * rcp * zcsfwf * sst_m(A2D(0)) * imsk_src(A2D(0),jcs)
      ENDDO
      zcsh(:) = glob_2Dsum( 'closea', zmsk_src, cdelay = 'cs2_'//cdcstype )
      !
      ! 3. Add residuals to target points 
      !    Do not use pqcs(:,:) = pqcs(:,:) - rcp * zcsfw  * sst_m(:,:) / zsurftrg 
      !    as there is no reason heat will be conserved with this formulation
      DO jcs = 1, kncs  ! loop over closed seas
         WHERE( imsk_trg(:,:,jcs) == kcsgrp(jcs) .AND. zsurftrg(jcs) > 0._wp )
            pwcs(:,:) = pwcs(:,:) + zcsfw(jcs) / zsurftrg(jcs)
            pqcs(:,:) = pqcs(:,:) - zcsh (jcs) / zsurftrg(jcs)
         ENDWHERE
      END DO

   END SUBROUTINE sbc_csupdate

   SUBROUTINE alloc_csarr( klen, pvarsrc, pvartrg, kvargrp )
      !!-----------------------------------------------------------------------
      !!                  ***  routine alloc_cssurf  ***
      !!
      !! ** Purpose : allocate closed sea surface array
      !!----------------------------------------------------------------------
      ! subroutine parameters
      INTEGER,  INTENT(in) :: klen
      INTEGER,  ALLOCATABLE, DIMENSION(:), INTENT(  out) :: kvargrp
      REAL(wp), ALLOCATABLE, DIMENSION(:), INTENT(  out) :: pvarsrc, pvartrg 
      !
      ! local variables
      INTEGER :: ierr
      !!----------------------------------------------------------------------
      !
      ! klen (number of lake) can be zero so use MAX(klen,1) to avoid 0 length array
      ALLOCATE( pvarsrc(MAX(klen,1)) , pvartrg(MAX(klen,1)) , STAT=ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'sbc_clo: failed to allocate surf array')
      !
      ALLOCATE( kvargrp(MAX(klen,1)) , STAT=ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP', 'sbc_clo: failed to allocate group array')
      !
      ! initialise to 0
      pvarsrc(:) = 0.e0_wp
      pvartrg(:) = 0.e0_wp
      kvargrp(:) = 0
   END SUBROUTINE alloc_csarr

END MODULE sbcclo
