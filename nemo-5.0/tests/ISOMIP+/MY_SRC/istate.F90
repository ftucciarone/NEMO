MODULE istate
   !!======================================================================
   !!                     ***  MODULE  istate  ***
   !! Ocean state   :  initial state setting
   !!=====================================================================
   !! History :  OPA  !  1989-12  (P. Andrich)  Original code
   !!            5.0  !  1991-11  (G. Madec)  rewritting
   !!            6.0  !  1996-01  (G. Madec)  terrain following coordinates
   !!            8.0  !  2001-09  (M. Levy, M. Ben Jelloul)  istate_eel
   !!            8.0  !  2001-09  (M. Levy, M. Ben Jelloul)  istate_uvg
   !!   NEMO     1.0  !  2003-08  (G. Madec, C. Talandier)  F90: Free form, modules + EEL R5
   !!             -   !  2004-05  (A. Koch-Larrouy)  istate_gyre 
   !!            2.0  !  2006-07  (S. Masson)  distributed restart using iom
   !!            3.3  !  2010-10  (C. Ethe) merge TRC-TRA
   !!            3.4  !  2011-04  (G. Madec) Merge of dtatem and dtasal & suppression of tb,tn/sb,sn 
   !!            3.7  !  2016-04  (S. Flavoni) introduce user defined initial state 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   istate_init   : initial state setting
   !!   istate_uvg    : initial velocity in geostropic balance
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers 
   USE dom_oce        ! ocean space and time domain 
   USE daymod         ! calendar
   USE dtatsd         ! data temperature and salinity   (dta_tsd routine)
   USE c1d            ! data: U & V current             (dta_uvd routine)
   USE wet_dry         ! wetting and drying (needed for wad_istate)
   USE usrdef_istate   ! User defined initial state
   !
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O library
   USE lib_mpp         ! MPP library
   USE lbclnk         ! lateal boundary condition / mpp exchanges
   USE restart         ! restart

#if defined key_agrif
   USE agrif_oce       ! initial state interpolation
   USE agrif_oce_interp
#endif   

   IMPLICIT NONE
   PRIVATE

   PUBLIC   istate_init   ! routine called by nemogcm.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE istate_init( Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_init  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracer fields.
      !!
      !! ** Method  :   
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in )  ::  Kbb, Kmm, Kaa   ! ocean time level indices
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zgdept     ! 3D table for qco substitute
!!gm see comment further down
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   zuvd    ! U & V data workspace
!!gm end
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'istate_init : Initialization of the dynamics and tracers'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'

      CALL dta_tsd_init                 ! Initialisation of T & S input data
      IF( ln_c1d) CALL dta_uvd_init     ! Initialisation of U & V input data (c1d only)

      ts(:,:,:,:,Kaa) = 0._wp   ;   rn2  (:,:,:  ) = 0._wp            ! set one for all to 0 at levels 1 and jpk
      uu(:,:,:  ,Kaa) = 0._wp   ;   vv(:,:,:,Kaa)  = 0._wp            ! set one for all to 0 

      IF ( ALLOCATED( rhd ) ) THEN                                    ! SWE, for example, will not have allocated these
         rhd  (:,:,:      ) = 0._wp   ;   rhop (:,:,:  ) = 0._wp      ! set one for all to 0 at level jpk
         rn2b (:,:,:      ) = 0._wp                                   ! set one for all to 0 at level jpk
         rab_b(:,:,:,:    ) = 0._wp   ;   rab_n(:,:,:,:) = 0._wp      ! set one for all to 0 at level jpk
      ENDIF

#if defined key_agrif
      IF ( .NOT.Agrif_root() .AND. ln_init_chfrpar ) THEN
         numror = 0                           ! define numror = 0 -> no restart file to read
         ln_1st_euler = .true.                ! Set time-step indicator at nit000 (euler forward)
         CALL day_init 
         CALL agrif_istate_oce( Kbb, Kmm, Kaa )   ! Interp from parent
         !
         ts (:,:,:,:,Kmm) = ts (:,:,:,:,Kbb)
         uu (:,:,:  ,Kmm) = uu (:,:,:  ,Kbb)
         vv (:,:,:  ,Kmm) = vv (:,:,:  ,Kbb)
      ELSE
#endif
         IF( ln_rstart ) THEN                    ! Restart from a file
            !                                    ! -------------------
            CALL rst_read( Kbb, Kmm )            ! Read the restart file
            CALL day_init                        ! model calendar (using both namelist and restart infos)
            !
         ELSE                                    ! Start from rest
            !                                    ! ---------------
            numror = 0                           ! define numror = 0 -> no restart file to read
            l_1st_euler = .true.                 ! Set time-step indicator at nit000 (euler forward)
            CALL day_init                        ! model calendar (using both namelist and restart infos)
            !                                    ! Initialization of ocean to zero
            !
            IF( ln_tsd_init ) THEN               
               CALL dta_tsd( nit000, ts(:,:,:,:,Kbb), 'ini' )                     ! read 3D T and S data at nit000
            ENDIF
            !
            IF( ln_uvd_init .AND. ln_c1d ) THEN               
               CALL dta_uvd( nit000, Kbb, uu(:,:,:,Kbb), vv(:,:,:,Kbb) )   ! read 3D U and V data at nit000
            ELSE
               uu  (:,:,:,Kbb) = 0._wp               ! set the ocean at rest
               vv  (:,:,:,Kbb) = 0._wp  
            ENDIF
               !
               !
            IF( .NOT. ln_tsd_init .AND. .NOT. ln_uvd_init ) THEN
               DO jk = 1, jpk
                  zgdept(:,:,jk) = gdept(:,:,jk,Kbb)
               END DO
               CALL usr_def_istate( zgdept, tmask, ts(:,:,:,:,Kbb), uu(:,:,:,Kbb), vv(:,:,:,Kbb) )
               ! make sure that periodicities are properly applied 
               CALL lbc_lnk( 'istate', ts(:,:,:,jp_tem,Kbb), 'T',  1._wp, ts(:,:,:,jp_sal,Kbb), 'T',  1._wp,   &
                  &                    uu(:,:,:,       Kbb), 'U', -1._wp, vv(:,:,:,       Kbb), 'V', -1._wp )
            ENDIF
            ts  (:,:,:,:,Kmm) = ts (:,:,:,:,Kbb)       ! set now values from to before ones
            uu    (:,:,:,Kmm) = uu   (:,:,:,Kbb)
            vv    (:,:,:,Kmm) = vv   (:,:,:,Kbb)
         ENDIF 
#if defined key_agrif
      ENDIF
#endif
      ! 
#if defined key_RK3
      IF( .NOT. ln_rstart ) THEN
#endif
         ! Initialize "before" barotropic velocities. "now" values are always set but 
         ! "before" values may have been read from a restart to ensure restartability.
         ! In the non-restart or non-RK3 cases they need to be initialised here:
         uu_b(:,:,Kbb) = 0._wp   ;   vv_b(:,:,Kbb) = 0._wp
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
            uu_b(ji,jj,Kbb) = uu_b(ji,jj,Kbb) + e3u(ji,jj,jk,Kbb) * uu(ji,jj,jk,Kbb) * umask(ji,jj,jk)
            vv_b(ji,jj,Kbb) = vv_b(ji,jj,Kbb) + e3v(ji,jj,jk,Kbb) * vv(ji,jj,jk,Kbb) * vmask(ji,jj,jk)
         END_3D
         uu_b(:,:,Kbb) = uu_b(:,:,Kbb) * r1_hu(:,:,Kbb)
         vv_b(:,:,Kbb) = vv_b(:,:,Kbb) * r1_hv(:,:,Kbb)
         ! 
#if defined key_RK3
      ENDIF
#endif
      !
      ! Initialize "now" barotropic velocities:
      ! Do it whatever the free surface method, these arrays being used eventually 
      !
#if  defined key_RK3
      IF( .NOT. ln_rstart ) THEN
         uu_b(:,:,Kmm)   = uu_b(:,:,Kbb)   ! Kmm value set to Kbb for initialisation in Agrif_Regrid in namo_gcm
         vv_b(:,:,Kmm)   = vv_b(:,:,Kbb)
      ENDIF
#else
!!gm  the use of umask & vmask is not necessary below as uu(:,:,:,Kmm), vv(:,:,:,Kmm), uu(:,:,:,Kbb), vv(:,:,:,Kbb) are always masked
      uu_b(:,:,Kmm) = 0._wp   ;   vv_b(:,:,Kmm) = 0._wp
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
         uu_b(ji,jj,Kmm) = uu_b(ji,jj,Kmm) + e3u(ji,jj,jk,Kmm) * uu(ji,jj,jk,Kmm) * umask(ji,jj,jk)
         vv_b(ji,jj,Kmm) = vv_b(ji,jj,Kmm) + e3v(ji,jj,jk,Kmm) * vv(ji,jj,jk,Kmm) * vmask(ji,jj,jk)
      END_3D
      uu_b(:,:,Kmm) = uu_b(:,:,Kmm) * r1_hu(:,:,Kmm)
      vv_b(:,:,Kmm) = vv_b(:,:,Kmm) * r1_hv(:,:,Kmm)
#endif
      !
   END SUBROUTINE istate_init

   !!======================================================================
END MODULE istate
