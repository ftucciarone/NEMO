MODULE tradmp
   !!======================================================================
   !!                       ***  MODULE  tradmp  ***
   !! Ocean physics: internal restoring trend on active tracers (T and S)
   !!======================================================================
   !! History :  OPA  ! 1991-03  (O. Marti, G. Madec)  Original code
   !!                 ! 1992-06  (M. Imbard)  doctor norme
   !!                 ! 1998-07  (M. Imbard, G. Madec) ORCA version
   !!            7.0  ! 2001-02  (M. Imbard)  add distance to coast, Original code
   !!            8.1  ! 2001-02  (G. Madec, E. Durand)  cleaning
   !!  NEMO      1.0  ! 2002-08  (G. Madec, E. Durand)  free form + modules
   !!            3.2  ! 2009-08  (G. Madec, C. Talandier)  DOCTOR norm for namelist parameter
   !!            3.3  ! 2010-06  (C. Ethe, G. Madec) merge TRA-TRC
   !!            3.4  ! 2011-04  (G. Madec, C. Ethe) Merge of dtatem and dtasal + suppression of CPP keys
   !!            3.6  ! 2015-06  (T. Graham)  read restoring coefficient in a file
   !!            3.7  ! 2015-10  (G. Madec)  remove useless trends arrays
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_dmp_alloc : allocate tradmp arrays
   !!   tra_dmp       : update the tracer trend with the internal damping
   !!   tra_dmp_init  : initialization, namlist read, parameters control
   !!----------------------------------------------------------------------
   USE oce            ! ocean: variables
   USE dom_oce        ! ocean: domain variables
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends manager: tracers
   USE zdf_oce        ! ocean: vertical physics
   USE phycst         ! physical constants
   USE dtatsd         ! data: temperature & salinity
   USE zdfmxl         ! vertical physics: mixed layer depth
   !
   USE in_out_manager ! I/O manager
   USE iom            ! XIOS
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_dmp        ! called by step.F90
   PUBLIC   tra_dmp_init   ! called by nemogcm.F90

   !                                           !!* Namelist namtra_dmp : T & S newtonian damping *
   LOGICAL            , PUBLIC ::   ln_tradmp   !: internal damping flag
   INTEGER            , PUBLIC ::   nn_zdmp     !: = 0/1/2 flag for damping in the mixed layer
   CHARACTER(LEN=200) , PUBLIC ::   cn_resto    !: name of netcdf file containing restoration coefficient field
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   resto    !: restoring coeff. on T and S (s-1)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   tclim    !: temperature climatology on each time step(Celcius)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sclim    !: salinity climatology on each time step (psu)

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION tra_dmp_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION tra_dmp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( resto(jpi,jpj,jpk), &
         &      tclim(jpi,jpj,jpk), &
         &      sclim(jpi,jpj,jpk), STAT= tra_dmp_alloc )
      !
      CALL mpp_sum ( 'tradmp', tra_dmp_alloc )
      IF( tra_dmp_alloc > 0 )   CALL ctl_warn('tra_dmp_alloc: allocation of arrays failed')
      !
   END FUNCTION tra_dmp_alloc


   SUBROUTINE tra_dmp( kt, Kbb, Kmm, pts, Krhs )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE tra_dmp  ***
      !!
      !! ** Purpose :   Compute the tracer trend due to a newtonian damping
      !!      of the tracer field towards given data field and add it to the
      !!      general tracer trends.
      !!
      !! ** Method  :   Newtonian damping towards t_dta and s_dta computed
      !!      and add to the general tracer trends:
      !!                     ta = ta + resto * (t_dta - tb)
      !!                     sa = sa + resto * (s_dta - sb)
      !!         The trend is computed either throughout the water column
      !!      (nlmdmp=0) or in area of weak vertical mixing (nlmdmp=1) or
      !!      below the well mixed layer (nlmdmp=2)
      !!
      !! ** Action  : - tsa: tracer trends updated with the damping trend
      !!----------------------------------------------------------------------
      INTEGER,                                   INTENT(in   ) :: kt              ! ocean time-step index
      INTEGER,                                   INTENT(in   ) :: Kbb, Kmm, Krhs  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts             ! active tracers and RHS of tracer equation
      !
      INTEGER ::   ji, jj, jk, jn   ! dummy loop indices
      REAL(wp), DIMENSION(T2D(nn_hls),jpk,jpts)     ::  zts_dta
      REAL(wp), DIMENSION(:,:,:)  , ALLOCATABLE ::  zwrk
      REAL(wp), DIMENSION(:,:,:,:), ALLOCATABLE ::  ztrdts
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_dmp')
      !
      IF( l_trdtra .OR. iom_use('hflx_dmp_cea') .OR. iom_use('sflx_dmp_cea') ) THEN   !* Save ta and sa trends
         ALLOCATE( ztrdts(T2D(0),jpk,jpts) )
         DO jn = 1, jpts
            DO_3D( 0, 0, 0, 0, 1, jpk )
               ztrdts(ji,jj,jk,jn) = pts(ji,jj,jk,jn,Krhs)
            END_3D
         END DO
      ENDIF
      !                           !==  input T-S data at kt  ==!
      CALL dta_tsd( kt, zts_dta )            ! read and interpolates T-S data at kt
      !
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         tclim(ji,jj,jk) = zts_dta(ji,jj,jk,jp_tem)
         sclim(ji,jj,jk) = zts_dta(ji,jj,jk,jp_sal)
      END_3D
      !
      SELECT CASE ( nn_zdmp )     !==  type of damping  ==!
      !
      CASE( 0 )                        !*  newtonian damping throughout the water column  *!
         DO jn = 1, jpts
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               pts(ji,jj,jk,jn,Krhs) = pts(ji,jj,jk,jn,Krhs)           &
                  &                  + resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jn) - pts(ji,jj,jk,jn,Kbb) )
            END_3D
         END DO
         !
      CASE ( 1 )                       !*  no damping in the turbocline (avt > 5 cm2/s)  *!
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            IF( avt(ji,jj,jk) <= avt_c ) THEN
               pts(ji,jj,jk,jp_tem,Krhs) = pts(ji,jj,jk,jp_tem,Krhs)   &
                  &                      + resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_tem) - pts(ji,jj,jk,jp_tem,Kbb) )
               pts(ji,jj,jk,jp_sal,Krhs) = pts(ji,jj,jk,jp_sal,Krhs)   &
                  &                      + resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_sal) - pts(ji,jj,jk,jp_sal,Kbb) )
            ENDIF
         END_3D
         !
      CASE ( 2 )                       !*  no damping in the mixed layer   *!
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            IF( gdept(ji,jj,jk,Kmm) >= hmlp (ji,jj) ) THEN
               pts(ji,jj,jk,jp_tem,Krhs) = pts(ji,jj,jk,jp_tem,Krhs)   &
                  &                      + resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_tem) - pts(ji,jj,jk,jp_tem,Kbb) )
               pts(ji,jj,jk,jp_sal,Krhs) = pts(ji,jj,jk,jp_sal,Krhs)   &
                  &                      + resto(ji,jj,jk) * ( zts_dta(ji,jj,jk,jp_sal) - pts(ji,jj,jk,jp_sal,Kbb) )
            ENDIF
         END_3D
         !
      END SELECT
      !
      ! outputs (clem trunk)
      IF( iom_use('hflx_dmp_cea') .OR. iom_use('sflx_dmp_cea') ) THEN
         ALLOCATE( zwrk(T2D(0),jpk) )          ! Needed to handle expressions containing e3t when using key_qco or key_linssh
         zwrk(:,:,:) = 0._wp

         IF( iom_use('hflx_dmp_cea') ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpk )
               zwrk(ji,jj,jk) = ( pts(ji,jj,jk,jp_tem,Krhs) - ztrdts(ji,jj,jk,jp_tem) ) * e3t(ji,jj,jk,Kmm)
            END_3D
            CALL iom_put('hflx_dmp_cea', SUM( zwrk(:,:,:), dim=3 ) * rcp * rho0 ) ! W/m2
         ENDIF
         IF( iom_use('sflx_dmp_cea') ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpk )
               zwrk(ji,jj,jk) = ( pts(ji,jj,jk,jp_sal,Krhs) - ztrdts(ji,jj,jk,jp_sal) ) * e3t(ji,jj,jk,Kmm)
            END_3D
            CALL iom_put('sflx_dmp_cea', SUM( zwrk(:,:,:), dim=3 ) * rho0 )       ! g/m2/s
         ENDIF

         DEALLOCATE( zwrk )
      ENDIF
      !
      IF( l_trdtra )   THEN       ! trend diagnostic
         ztrdts(:,:,:,:) = pts(T2D(0),:,:,Krhs) - ztrdts(:,:,:,:)
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_dmp, ztrdts(:,:,:,jp_tem) )
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_sal, jptra_dmp, ztrdts(:,:,:,jp_sal) )
         DEALLOCATE( ztrdts )
      ENDIF
      !                           ! Control print
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Krhs), clinfo1=' dmp  - Ta: ', mask1=tmask,   &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Krhs), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( ln_timing )   CALL timing_stop('tra_dmp')
      !
   END SUBROUTINE tra_dmp


   SUBROUTINE tra_dmp_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_dmp_init  ***
      !!
      !! ** Purpose :   Initialization for the newtonian damping
      !!
      !! ** Method  :   read the namtra_dmp namelist and check the parameters
      !!----------------------------------------------------------------------
      INTEGER ::   ios, imask   ! local integers
      !
      NAMELIST/namtra_dmp/ ln_tradmp, nn_zdmp, cn_resto
      !!----------------------------------------------------------------------
      !
      READ_NML_REF(numnam,namtra_dmp)
      READ_NML_CFG(numnam,namtra_dmp)
      IF(lwm) WRITE ( numond, namtra_dmp )
      !
      IF(lwp) THEN                  ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'tra_dmp_init : T and S newtonian relaxation'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namtra_dmp : set relaxation parameters'
         WRITE(numout,*) '      Apply relaxation   or not       ln_tradmp   = ', ln_tradmp
         WRITE(numout,*) '         mixed layer damping option      nn_zdmp  = ', nn_zdmp
         WRITE(numout,*) '         Damping file name               cn_resto = ', cn_resto
         WRITE(numout,*)
      ENDIF
      !
      IF( ln_tradmp ) THEN
         !                          ! Allocate arrays
         IF( tra_dmp_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'tra_dmp_init: unable to allocate arrays' )
         !
         SELECT CASE (nn_zdmp)      ! Check values of nn_zdmp
         CASE ( 0 )   ;   IF(lwp) WRITE(numout,*) '   tracer damping as specified by mask'
         CASE ( 1 )   ;   IF(lwp) WRITE(numout,*) '   no tracer damping in the mixing layer (kz > 5 cm2/s)'
         CASE ( 2 )   ;   IF(lwp) WRITE(numout,*) '   no tracer damping in the mixed  layer'
         CASE DEFAULT
            CALL ctl_stop('tra_dmp_init : wrong value of nn_zdmp')
         END SELECT
         !
         !!TG: Initialisation of dtatsd - Would it be better to have dmpdta routine
         !    so can damp to something other than intitial conditions files?
         !!gm: In principle yes. Nevertheless, we can't anticipate demands that have never been formulated.
         IF( .NOT.ln_tsd_dmp ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout, *)  '   read T-S data not initialized, we force ln_tsd_dmp=T'
            CALL dta_tsd_init( ld_tradmp=ln_tradmp )        ! forces the initialisation of T-S data
         ENDIF
         !                          ! Read in mask from file
         CALL iom_open ( cn_resto, imask)
         CALL iom_get  ( imask, jpdom_auto, 'resto', resto )
         CALL iom_close( imask )
      ENDIF
      !
   END SUBROUTINE tra_dmp_init

   !!======================================================================
END MODULE tradmp
