MODULE icedyn_adv_umx
   !!==============================================================================
   !!                       ***  MODULE  icedyn_adv_umx  ***
   !! sea-ice : advection using the ULTIMATE-MACHO scheme
   !!==============================================================================
   !! History :  3.6  !  2014-11  (C. Rousset, G. Madec)  Original code
   !!            4.0  !  2018     (many people)           SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_dyn_adv_umx   : update the tracer fields
   !!   ultimate_x(_y)    : compute a tracer value at velocity points using ULTIMATE scheme at various orders
   !!   macho             : compute the fluxes
   !!   nonosc_ice        : limit the fluxes using a non-oscillatory algorithm
   !!----------------------------------------------------------------------
   USE par_ice        ! SI3 parameters
   USE phycst         ! physical constant
   USE sbc_oce , ONLY : nn_fsbc   ! update frequency of surface boundary condition
   USE ice            ! sea-ice variables
   USE icevar         ! sea-ice: operations
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! to use sign with key_nosignedzero
   USE lbclnk         ! lateral boundary conditions (or mpp links)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_dyn_adv_umx   ! called by icedyn_adv.F90
   !
   INTEGER, PARAMETER ::   np_advS = 2         ! advection for S and T:    dVS/dt = -div(      uVS     ) => np_advS = 1
   !                                                                    or dVS/dt = -div( uA * uHS / u ) => np_advS = 2
   !                                                                    or dVS/dt = -div( uV * uS  / u ) => np_advS = 3
   INTEGER, PARAMETER ::   np_limiter = 1      ! limiter: 1 = nonosc
   !                                                      2 = superbee
   !                                                      3 = h3
   LOGICAL            ::   ll_upsxy  = .TRUE.   ! alternate directions for upstream
   LOGICAL            ::   ll_hoxy   = .TRUE.   ! alternate directions for high order
   LOGICAL            ::   ll_neg    = .TRUE.   ! if T interpolated at u/v points is negative or v_i < 1.e-6
   !                                                 then interpolate T at u/v points using the upstream scheme
   LOGICAL            ::   ll_prelim = .FALSE.  ! prelimiter from: Zalesak(1979) eq. 14 => not well defined in 2D
   !
   REAL(wp)           ::   r1_6   = 1._wp /   6._wp   ! =1/6
   REAL(wp)           ::   r1_120 = 1._wp / 120._wp   ! =1/120
   !
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) ::   imsk_small, jmsk_small
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL licence     (./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_dyn_adv_umx( kn_umx, kt, pu_ice, pv_ice, ph_i, ph_s, ph_ip,  &
      &                        pato_i, pv_i, pv_s, psv_i, poa_i, pa_i, pa_ip, pv_ip, pv_il, pe_s, pe_i, pszv_i )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE ice_dyn_adv_umx  ***
      !!
      !! **  Purpose :   Compute the now trend due to total advection of
      !!                 tracers and add it to the general trend of tracer equations
      !!                 using an "Ultimate-Macho" scheme
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74.
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   kn_umx     ! order of the scheme (1-5=UM or 20=CEN2)
      INTEGER                     , INTENT(in   ) ::   kt         ! time step
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   pu_ice     ! ice i-velocity
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   pv_ice     ! ice j-velocity
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   ph_i       ! ice thickness
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   ph_s       ! snw thickness
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   ph_ip      ! ice pond thickness
      REAL(wp), DIMENSION(:,:)    , INTENT(inout) ::   pato_i     ! open water area
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_i       ! ice volume
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_s       ! snw volume
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   psv_i      ! salt content
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   poa_i      ! age content
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pa_i       ! ice concentration
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pa_ip      ! melt pond concentration
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_ip      ! melt pond volume
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_il      ! melt pond lid volume
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_s       ! snw heat content
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_i       ! ice heat content
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pszv_i     ! ice salt content
      !
      INTEGER  ::   ji, jj, jk, jl, jm, jt  ! dummy loop indices
      INTEGER  ::   ndim                    ! number of variables to advect
      INTEGER  ::   icycle                  ! number of sub-timestep for the advection
      REAL(wp) ::   zdt, z1_dt, zvi_cen
      REAL(wp) ::   zati2
      REAL(wp) ::   zcfl
      REAL(wp), DIMENSION(jpi,jpj)            ::   zudy, zvdx, zcu_box, zcv_box
      REAL(wp), DIMENSION(A2D(0))             ::   zati1
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   zu_cat, zv_cat
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   zua_ho, zva_ho, zua_ups, zva_ups
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   zuap_ho, zvap_ho, zuap_ups, zvap_ups
      REAL(wp), DIMENSION(jpi,jpj,jpl)        ::   z1_ai
      !
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ze_i
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ze_s
      REAL(wp), DIMENSION(A2D(0),jpl)           ::   zhi_max, zhs_max, zhip_max, zsi_max
      REAL(wp), DIMENSION(A2D(0),nlay_i,jpl)    ::   zei_max, zszi_max
      REAL(wp), DIMENSION(A2D(0),nlay_s,jpl)    ::   zes_max
      !
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)   ::   z1_aip
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)   ::   zs_i 
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   zsz_i
      !
      REAL(wp), ALLOCATABLE, DIMENSION(:)       ::   zamsk                   ! 1 if advection of concentration, 0 if advection of other tracers
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   zvar, zhvar
      !
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)   ::   zuv_ho, zvv_ho, zuv_ups, zvv_ups, z1_vi, z1_vs
      !! diagnostics
      REAL(wp), DIMENSION(A2D(0)) ::   zdiag_adv_mass, zdiag_adv_salt, zdiag_adv_heat
      !!----------------------------------------------------------------------
      !     
      IF( kt == nit000 .AND. lwp )   WRITE(numout,*) '-- ice_dyn_adv_umx: Ultimate-Macho advection scheme'
      !
      ndim = nlay_s + 2*nlay_i + 5 ! max number of tracers to advect at the same time
      !
      ! --- Allocate arrays --- !
      ALLOCATE( zvar(jpi,jpj,jpl,ndim), zhvar(jpi,jpj,jpl,ndim), zamsk(ndim) )
      IF( ln_pnd_LEV .OR. ln_pnd_TOPO )   ALLOCATE( z1_aip(jpi,jpj,jpl) )
      IF( np_advS == 3 )   ALLOCATE( zuv_ho(jpi,jpj,jpl), zvv_ho(jpi,jpj,jpl), zuv_ups(jpi,jpj,jpl), zvv_ups(jpi,jpj,jpl), &
         &                           z1_vi (jpi,jpj,jpl), z1_vs (jpi,jpj,jpl) )
      !
      !
      ! --- Record max of the surrounding 9-pts (for call Hbig) --- !
      !
      ! thickness
      CALL icemax3D_umx( ph_i , zhi_max )
      CALL icemax3D_umx( ph_s , zhs_max )
      IF( ln_pnd_LEV .OR. ln_pnd_TOPO )   CALL icemax3D_umx( ph_ip, zhip_max)
      !
      ! enthalpies
      ALLOCATE( ze_i(jpi,jpj,nlay_i,jpl) )
      DO jk = 1, nlay_i
         WHERE( pv_i(:,:,:) >= epsi10 ) ; ze_i(:,:,jk,:) = pe_i(:,:,jk,:) / pv_i(:,:,:)
         ELSEWHERE                      ; ze_i(:,:,jk,:) = 0._wp
         END WHERE
      END DO
      CALL icemax4D_umx( ze_i , zei_max )
      DEALLOCATE( ze_i )

      ALLOCATE( ze_s(jpi,jpj,nlay_s,jpl) )
      DO jk = 1, nlay_s
         WHERE( pv_s(:,:,:) >= epsi10 ) ; ze_s(:,:,jk,:) = pe_s(:,:,jk,:) / pv_s(:,:,:)
         ELSEWHERE                      ; ze_s(:,:,jk,:) = 0._wp
         END WHERE
      END DO
      CALL icemax4D_umx( ze_s , zes_max )
      DEALLOCATE( ze_s )
      !
      ! salt content
      IF( nn_icesal == 4 ) THEN
         ALLOCATE( zsz_i(jpi,jpj,nlay_i,jpl) )              
         DO jk = 1, nlay_i
            WHERE( pv_i(:,:,:) >= epsi10 ) ; zsz_i(:,:,jk,:) = pszv_i(:,:,jk,:) / pv_i(:,:,:)
            ELSEWHERE                      ; zsz_i(:,:,jk,:) = 0._wp
            END WHERE
         END DO
         CALL icemax4D_umx( zsz_i , zszi_max )
         DEALLOCATE( zsz_i )
      ELSE
         ALLOCATE( zs_i(jpi,jpj,jpl) )     
         WHERE( pv_i(:,:,:) >= epsi10 ) ; zs_i(:,:,:) = psv_i(:,:,:) / pv_i(:,:,:)
         ELSEWHERE                      ; zs_i(:,:,:) = 0._wp
         END WHERE
         CALL icemax3D_umx( zs_i , zsi_max )
         DEALLOCATE( zs_i )
      ENDIF
      !
      !
      ! --- If ice drift is too fast, use  subtime steps for advection (CFL test for stability) --- !
      !        Note: the advection split is applied at the next time-step in order to avoid blocking global comm.
      !              this should not affect too much the stability
      zcfl =            MAXVAL( ABS( pu_ice(:,:) ) * rDt_ice * r1_e1u(:,:) )
      zcfl = MAX( zcfl, MAXVAL( ABS( pv_ice(:,:) ) * rDt_ice * r1_e2v(:,:) ) )

      CALL mpp_max( 'icedyn_adv_umx', zcfl, cdelay = 'cflice' )

      IF    ( zcfl > 1.5 ) THEN   ;   icycle = 3
      ELSEIF( zcfl >  .5 ) THEN   ;   icycle = 2
      ELSE                        ;   icycle = 1
      ENDIF
!!$      !!test clem
!!$      icycle=3
!!$      !!test clem
      zdt = rDt_ice / REAL(icycle)
      z1_dt = 1._wp / zdt

      ! --- transport --- !
      zudy(:,:) = pu_ice(:,:) * e2u(:,:)
      zvdx(:,:) = pv_ice(:,:) * e1v(:,:)
      !
      ! setup transport for each ice cat
      DO jl = 1, jpl
         zu_cat(:,:,jl) = zudy(:,:)
         zv_cat(:,:,jl) = zvdx(:,:)
      END DO
      !
      ! --- define velocity for advection: u*grad(H) --- !
      DO_2D( 1, 2, 2, 2 )
         IF    ( pu_ice(ji,jj) * pu_ice(ji-1,jj) <= 0._wp ) THEN   ;   zcu_box(ji,jj) = 0._wp
         ELSEIF( pu_ice(ji,jj)                   >  0._wp ) THEN   ;   zcu_box(ji,jj) = pu_ice(ji-1,jj)
         ELSE                                                      ;   zcu_box(ji,jj) = pu_ice(ji  ,jj)   ;   ENDIF
      END_2D
      DO_2D( 2, 2, 1, 2 )
         IF    ( pv_ice(ji,jj) * pv_ice(ji,jj-1) <= 0._wp ) THEN   ;   zcv_box(ji,jj) = 0._wp
         ELSEIF( pv_ice(ji,jj)                   >  0._wp ) THEN   ;   zcv_box(ji,jj) = pv_ice(ji,jj-1)
         ELSE                                                      ;   zcv_box(ji,jj) = pv_ice(ji,jj  )   ;   ENDIF
      END_2D

      IF( lwp .AND. icycle > 1 )   WRITE(numout,*) 'icedyn_adv: CFL=',NINT(zcfl*10)/10.,' => number of cycles=',icycle     
      !---------------!
      !== advection ==!
      !---------------!
      DO jt = 1, icycle

         ! inverse of A and Ap
         WHERE( pa_i(:,:,:) >= epsi20 )   ;   z1_ai(:,:,:) = 1._wp / pa_i(:,:,:)
         ELSEWHERE                        ;   z1_ai(:,:,:) = 0.
         END WHERE
         IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
            WHERE( pa_ip(:,:,:) >= epsi20 )  ;   z1_aip(:,:,:) = 1._wp / pa_ip(:,:,:)
            ELSEWHERE                        ;   z1_aip(:,:,:) = 0.
            END WHERE
         ENDIF
         !
         ! setup a mask where advection will be upstream
         IF( ll_neg ) THEN
            IF( .NOT. ALLOCATED(imsk_small) )   ALLOCATE( imsk_small(jpi,jpj,jpl) )
            IF( .NOT. ALLOCATED(jmsk_small) )   ALLOCATE( jmsk_small(jpi,jpj,jpl) )
            DO jl = 1, jpl
               DO_2D( 2, 1, 2, 2 )
                  zvi_cen = 0.5_wp * ( pv_i(ji+1,jj,jl) + pv_i(ji,jj,jl) )
                  IF( zvi_cen < epsi06) THEN   ;   imsk_small(ji,jj,jl) = 0
                  ELSE                         ;   imsk_small(ji,jj,jl) = 1   ;   ENDIF
               END_2D
               DO_2D( 2, 2, 2, 1 )
                  zvi_cen = 0.5_wp * ( pv_i(ji,jj+1,jl) + pv_i(ji,jj,jl) )
                  IF( zvi_cen < epsi06) THEN   ;   jmsk_small(ji,jj,jl) = 0
                  ELSE                         ;   jmsk_small(ji,jj,jl) = 1   ;   ENDIF
               END_2D
            END DO
         ENDIF
         !
         ! diagnostics
         DO_2D( 0, 0, 0, 0 )
            zdiag_adv_mass(ji,jj) =   SUM( pv_i (ji,jj,:) ) * rhoi + SUM( pv_s (ji,jj,:) ) * rhos &
               &                    + SUM( pv_ip(ji,jj,:) ) * rhow + SUM( pv_il(ji,jj,:) ) * rhow
            zdiag_adv_heat(ji,jj) = - SUM( SUM( pe_i(ji,jj,1:nlay_i,:), dim=2 ) ) - SUM( SUM( pe_s(ji,jj,1:nlay_s,:), dim=2 ) )            
         END_2D
         IF( nn_icesal == 4 ) THEN
            DO_2D( 0, 0, 0, 0 )
               zdiag_adv_salt(ji,jj) = SUM( SUM( pszv_i(ji,jj,:,:), dim=2 ) ) * rhoi
            END_2D
         ELSE
            DO_2D( 0, 0, 0, 0 )
               zdiag_adv_salt(ji,jj) = SUM( psv_i(ji,jj,:) ) * rhoi
            END_2D
         ENDIF
         !
         ! record at_i before advection (for open water)
         zati1(:,:) = SUM( pa_i(A2D(0),:), dim=3 )
         !
         ! ----------------------- !
         ! ==> start advection <== !
         ! ----------------------- !
         !
         zamsk(1) = 1._wp
         !
         !== Ice area ==!
         zvar(:,:,:,1) = pa_i(:,:,:)
         CALL adv_umx( zamsk(1:1), kn_umx, jt, kt, zdt, zudy, zvdx, zu_cat , zv_cat , zcu_box, zcv_box, &
            &                                           zvar(:,:,:,1:1), zvar(:,:,:,1:1), zua_ups, zva_ups, zua_ho, zva_ho )
         pa_i(:,:,:) = zvar(:,:,:,1)

         !== Ice age ==!
         zvar(:,:,:,1) = poa_i(:,:,:)
         CALL adv_umx( zamsk(1:1), kn_umx, jt, kt, zdt, zudy , zvdx , zu_cat, zv_cat, zcu_box, zcv_box, &
            &                                           zvar(:,:,:,1:1), zvar(:,:,:,1:1) )
         poa_i(:,:,:) = zvar(:,:,:,1)

         !== melt ponds area ==!
         IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
            zvar(:,:,:,1) = pa_ip(:,:,:)
            CALL adv_umx( zamsk(1:1), kn_umx, jt, kt, zdt, zudy , zvdx , zu_cat , zv_cat , zcu_box, zcv_box,  &
               &                                           zvar(:,:,:,1:1), zvar(:,:,:,1:1), zuap_ups, zvap_ups, zuap_ho, zvap_ho )
            pa_ip(:,:,:) = zvar(:,:,:,1)
         ENDIF
         !
         !                             ! --------------------------------- !
         IF( np_advS == 1 ) THEN       ! -- advection form: -div( uVS ) -- !
            !                          ! --------------------------------- !

            !== Ice volume ==!
            jm = 1    ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_i(:,:,:)
            zhvar(:,:,:,jm) = pv_i(:,:,:) * z1_ai(:,:,:)
            !== Snw volume ==!
            jm = jm+1 ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_s(:,:,:)
            zhvar(:,:,:,jm) = pv_s(:,:,:) * z1_ai(:,:,:)
            !== Ice heat content ==!
            DO jk = 1, nlay_i
               jm = jm+1 ; zamsk(jm) = 1._wp
               zvar (:,:,:,jm) = pe_i(:,:,jk,:)
               zhvar(:,:,:,jm) = pe_i(:,:,jk,:)
            ENDDO
            !== Snw heat content ==!
            DO jk = 1, nlay_s
               jm = jm+1 ; zamsk(jm) = 1._wp
               zvar (:,:,:,jm) = pe_s(:,:,jk,:)
               zhvar(:,:,:,jm) = pe_s(:,:,jk,:)
            ENDDO
            !== Ice salt content ==!
            IF( nn_icesal == 4 ) THEN
               DO jk = 1, nlay_i
                  jm = jm+1 ; zamsk(jm) = 1._wp
                  zvar (:,:,:,jm) = pszv_i(:,:,jk,:)
                  zhvar(:,:,:,jm) = pszv_i(:,:,jk,:)
               ENDDO
            ELSE
               jm = jm+1 ; zamsk(jm) = 1._wp
               zvar (:,:,:,jm) = psv_i(:,:,:)
               zhvar(:,:,:,jm) = psv_i(:,:,:)
            ENDIF

            !== advection ==!
            CALL adv_umx( zamsk(1:jm), kn_umx, jt, kt, zdt, zudy, zvdx, zua_ho, zva_ho, zcu_box, zcv_box, &
               &                                            zhvar(:,:,:,1:jm), zvar(:,:,:,1:jm), zua_ups, zva_ups )
            !

            !== Recover quantities ==!
            jm = 1       ;    pv_i  (:,:,:)    = zvar (:,:,:,jm)
            jm = jm+1    ;    pv_s  (:,:,:)    = zvar (:,:,:,jm)
            DO jk = 1, nlay_i
               jm = jm+1 ;    pe_i  (:,:,jk,:) = zvar (:,:,:,jm) 
            ENDDO
            DO jk = 1, nlay_s
               jm = jm+1 ;    pe_s  (:,:,jk,:) = zvar (:,:,:,jm)
            ENDDO
            IF( nn_icesal == 4 ) THEN
               DO jk = 1, nlay_i
                  jm = jm+1 ; pszv_i(:,:,jk,:) = zvar (:,:,:,jm)
               ENDDO
            ELSE
               jm = jm+1 ;    psv_i (:,:,:)    = zvar (:,:,:,jm) 
            ENDIF

            !
            !                          ! ------------------------------------------ !
         ELSEIF( np_advS == 2 ) THEN   ! -- advection form: -div( uA * uHS / u ) -- !
            !                          ! ------------------------------------------ !

            !== Ice volume ==!
            jm = 1    ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_i(:,:,:)
            zhvar(:,:,:,jm) = pv_i(:,:,:) * z1_ai(:,:,:)
            !== Snw volume ==!
            jm = jm+1 ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_s(:,:,:)
            zhvar(:,:,:,jm) = pv_s(:,:,:) * z1_ai(:,:,:)
            !== Ice heat content ==!
            DO jk = 1, nlay_i
               jm = jm+1 ; zamsk(jm) = 0._wp
               zvar (:,:,:,jm) = pe_i(:,:,jk,:)
               zhvar(:,:,:,jm) = pe_i(:,:,jk,:) * z1_ai(:,:,:)
            ENDDO
            !== Snw heat content ==!
            DO jk = 1, nlay_s
               jm = jm+1 ; zamsk(jm) = 0._wp
               zvar (:,:,:,jm) = pe_s(:,:,jk,:)
               zhvar(:,:,:,jm) = pe_s(:,:,jk,:) * z1_ai(:,:,:)
            ENDDO
            !== Ice salt content ==!
            IF( nn_icesal == 4 ) THEN
               DO jk = 1, nlay_i
                  jm = jm+1 ; zamsk(jm) = 0._wp
                  zvar (:,:,:,jm) = pszv_i(:,:,jk,:)
                  zhvar(:,:,:,jm) = pszv_i(:,:,jk,:) * z1_ai(:,:,:)
               ENDDO
            ELSE
               jm = jm+1 ; zamsk(jm) = 0._wp
               zvar (:,:,:,jm) = psv_i(:,:,:)
               zhvar(:,:,:,jm) = psv_i(:,:,:) * z1_ai(:,:,:)
            ENDIF

            !== advection ==!
            CALL adv_umx( zamsk(1:jm), kn_umx, jt, kt, zdt, zudy, zvdx, zua_ho, zva_ho, zcu_box, zcv_box, &
               &                                            zhvar(:,:,:,1:jm), zvar(:,:,:,1:jm), zua_ups, zva_ups )
            !
            !== Recover quantities ==!
            jm = 1       ;    pv_i  (:,:,:)    = zvar (:,:,:,jm)
            jm = jm+1    ;    pv_s  (:,:,:)    = zvar (:,:,:,jm)
            DO jk = 1, nlay_i
               jm = jm+1 ;    pe_i  (:,:,jk,:) = zvar (:,:,:,jm) 
            ENDDO
            DO jk = 1, nlay_s
               jm = jm+1 ;    pe_s  (:,:,jk,:) = zvar (:,:,:,jm)
            ENDDO
            IF( nn_icesal == 4 ) THEN
               DO jk = 1, nlay_i
                  jm = jm+1 ; pszv_i(:,:,jk,:) = zvar (:,:,:,jm)
               ENDDO
            ELSE
               jm = jm+1 ;    psv_i (:,:,:)    = zvar (:,:,:,jm) 
            ENDIF
           
            !                          ! ----------------------------------------- !
         ELSEIF( np_advS == 3 ) THEN   ! -- advection form: -div( uV * uS / u ) -- !
            !                          ! ----------------------------------------- !
            !
            ! inverse of Vi
            WHERE( pv_i(:,:,:) >= epsi20 )   ;   z1_vi(:,:,:) = 1._wp / pv_i(:,:,:)
            ELSEWHERE                        ;   z1_vi(:,:,:) = 0.
            END WHERE
            ! inverse of Vs
            WHERE( pv_s(:,:,:) >= epsi20 )   ;   z1_vs(:,:,:) = 1._wp / pv_s(:,:,:)
            ELSEWHERE                        ;   z1_vs(:,:,:) = 0.
            END WHERE
            !
            ! It is important to first calculate the ice fields and then the snow fields (because we use the same arrays)
            !
            !== Ice volume ==!
            jm = 1 ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_i(:,:,:)
            zhvar(:,:,:,jm) = pv_i(:,:,:) * z1_ai(:,:,:)
            zuv_ups = zua_ups
            zvv_ups = zva_ups
            CALL adv_umx( zamsk(1:1), kn_umx, jt, kt, zdt, zudy, zvdx, zua_ho, zva_ho, zcu_box, zcv_box, &
               &                                           zhvar(:,:,:,1:1), zvar(:,:,:,1:1), zuv_ups, zvv_ups, zuv_ho, zvv_ho )
            !== Ice heat content ==!
            DO jk = 1, nlay_i
               jm = jm+1 ; zamsk(jm) = 0._wp
               zvar (:,:,:,jm) = pe_i(:,:,jk,:)
               zhvar(:,:,:,jm) = pe_i(:,:,jk,:) * z1_vi(:,:,:)
            ENDDO
             !== Ice salt content ==!
            IF( nn_icesal == 4 ) THEN
               DO jk = 1, nlay_i
                  jm = jm+1 ; zamsk(jm) = 0._wp
                  zvar (:,:,:,jm) = pszv_i(:,:,jk,:)
                  zhvar(:,:,:,jm) = pszv_i(:,:,jk,:) * z1_vi(:,:,:)
               ENDDO
            ELSE
               jm = jm+1 ; zamsk(jm) = 0._wp
               zvar (:,:,:,jm) = psv_i(:,:,:)
               zhvar(:,:,:,jm) = psv_i(:,:,:) * z1_vi(:,:,:)
            ENDIF
            CALL adv_umx( zamsk(2:jm), kn_umx, jt, kt, zdt, zudy, zvdx, zuv_ho, zvv_ho, zcu_box, zcv_box, &
               &                                            zhvar(:,:,:,2:jm), zvar(:,:,:,2:jm), zuv_ups, zvv_ups )
            !
            !== Recover quantities ==!
            jm = 1       ;    pv_i  (:,:,:)    = zvar (:,:,:,jm)
            DO jk = 1, nlay_i
               jm = jm+1 ;    pe_i  (:,:,jk,:) = zvar (:,:,:,jm) 
            ENDDO
            IF( nn_icesal == 4 ) THEN
               DO jk = 1, nlay_i
                  jm = jm+1 ; pszv_i(:,:,jk,:) = zvar (:,:,:,jm)
               ENDDO
            ELSE
               jm = jm+1 ;    psv_i (:,:,:)    = zvar (:,:,:,jm) 
            ENDIF

            !== Snw volume ==!
            jm = 1 ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_s(:,:,:)
            zhvar(:,:,:,jm) = pv_s(:,:,:) * z1_ai(:,:,:)
            zuv_ups = zua_ups
            zvv_ups = zva_ups
            CALL adv_umx( zamsk(1:1), kn_umx, jt, kt, zdt, zudy , zvdx, zua_ho , zva_ho , zcu_box, zcv_box, &
               &                                           zhvar(:,:,:,1:1), zvar(:,:,:,1:1), zuv_ups, zvv_ups, zuv_ho , zvv_ho )
            !== Snw heat content ==!
            DO jk = 1, nlay_s
               jm = jm+1 ; zamsk(jm) = 0._wp
               zvar (:,:,:,jm) = pe_s(:,:,jk,:)
               zhvar(:,:,:,jm) = pe_s(:,:,jk,:) * z1_vs(:,:,:)
            ENDDO
            CALL adv_umx( zamsk(2:jm), kn_umx, jt, kt, zdt, zudy, zvdx, zuv_ho, zvv_ho, zcu_box, zcv_box, &
               &                                            zhvar(:,:,:,2:jm), zvar(:,:,:,2:jm), zuv_ups, zvv_ups )
            !
            !== Recover quantities ==!
            jm = 1       ;    pv_s  (:,:,:)    = zvar (:,:,:,jm)
            DO jk = 1, nlay_s
               jm = jm+1 ;    pe_s  (:,:,jk,:) = zvar (:,:,:,jm)
            ENDDO
            !
            !
         ENDIF
         !
         !
         !== melt ponds ==!
         IF ( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN

            !== pond volume ==!
            jm = 1    ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_ip(:,:,:)
            zhvar(:,:,:,jm) = pv_ip(:,:,:) * z1_aip(:,:,:)
            !== lid volume ==!
            jm = jm+1 ; zamsk(jm) = 0._wp
            zvar (:,:,:,jm) = pv_il(:,:,:)
            zhvar(:,:,:,jm) = pv_il(:,:,:) * z1_aip(:,:,:)
            !
            !== advection ==!
            CALL adv_umx( zamsk(1:jm), kn_umx, jt, kt, zdt, zudy, zvdx, zuap_ho, zvap_ho, zcu_box, zcv_box, &
               &                                            zhvar(:,:,:,1:jm), zvar(:,:,:,1:jm), zuap_ups, zvap_ups )

            !== Recover quantities ==!
            jm = 1       ;    pv_ip  (:,:,:)    = zvar (:,:,:,jm)
            jm = jm+1    ;    pv_il  (:,:,:)    = zvar (:,:,:,jm)
         ENDIF
       
         ! --- diagnostics --- !
         DO_2D( 0, 0, 0, 0 )
            diag_adv_mass(ji,jj) = diag_adv_mass(ji,jj) + (   SUM( pv_i (ji,jj,:) ) * rhoi + SUM( pv_s (ji,jj,:) ) * rhos &
               &                                            + SUM( pv_ip(ji,jj,:) ) * rhow + SUM( pv_il(ji,jj,:) ) * rhow &
               &                                          - zdiag_adv_mass(ji,jj) ) * r1_Dt_ice
            diag_adv_heat(ji,jj) = diag_adv_heat(ji,jj) + ( - SUM(SUM( pe_i(ji,jj,1:nlay_i,:) , dim=2 ) ) &
               &                                            - SUM(SUM( pe_s(ji,jj,1:nlay_s,:) , dim=2 ) ) &
               &                                          - zdiag_adv_heat(ji,jj) ) * r1_Dt_ice
         END_2D
         IF( nn_icesal == 4 ) THEN
            DO_2D( 0, 0, 0, 0 )
               diag_adv_salt(ji,jj) = diag_adv_salt(ji,jj) + ( SUM( SUM( pszv_i(ji,jj,:,:), dim=2 ) ) * rhoi &
                  &                                          - zdiag_adv_salt(ji,jj) ) * r1_Dt_ice
            END_2D
         ELSE
            DO_2D( 0, 0, 0, 0 )
               diag_adv_salt(ji,jj) = diag_adv_salt(ji,jj) + ( SUM( psv_i(ji,jj,:) ) * rhoi &
                  &                                          - zdiag_adv_salt(ji,jj) ) * r1_Dt_ice
            END_2D
         ENDIF

         ! --- Ensure non-negative fields and in-bound thicknesses --- !
         ! Remove negative values (conservation is ensured)
         !    (because advected fields are not perfectly bounded and tiny negative values can occur, e.g. -1.e-20)
         CALL ice_var_zapneg( 0, rDt_ice, pv_i, pv_s, psv_i, poa_i, pa_i, pa_ip, pv_ip, pv_il, pe_s, pe_i, pszv_i )
         !
         ! --- Make sure ice thickness is not too big --- !
         !     (because ice thickness can be too large where ice concentration is very small)
         CALL Hbig_umx( rDt_ice, zhi_max, zhs_max, zhip_max, zsi_max, zes_max, zei_max, zszi_max, &
            &            pv_i, pv_s, pa_i, pa_ip, pv_ip, psv_i, pe_s, pe_i, pszv_i )
         !
         ! --- Ensure snow load is not too big --- !
         CALL Hsnow_umx( rDt_ice, pv_i, pv_s, pa_i, pa_ip, pe_s )
         !
         !
         !== open water area ==!
         DO_2D( 0, 0, 0, 0 )
            zati2 = SUM( pa_i(ji,jj,:) )
            pato_i(ji,jj) = MAX( 0._wp, pato_i(ji,jj) - ( zati2 - zati1(ji,jj) )            &   ! derive open water from ice concentration
               &                                      - (   ( zudy(ji,jj) - zudy(ji-1,jj) ) &   ! ad () for NP repro
               &                                          + ( zvdx(ji,jj) - zvdx(ji,jj-1) ) ) * r1_e1e2t(ji,jj) * zdt )
         END_2D
         ! note: no need of lbc_lnk for open water (never used in the halos)
         !
         ! --- Lateral boundary conditions --- !
         IF( jt <= (icycle-1) ) THEN ! only if we have 2 cycles and we are at the 1st one
            !
            IF( ln_pnd_LEV .OR. ln_pnd_TOPO ) THEN
               CALL lbc_lnk( 'icedyn_adv_umx', pa_i , 'T', 1._wp, pv_i , 'T', 1._wp, pv_s , 'T', 1._wp, &
                  &                            psv_i, 'T', 1._wp, poa_i, 'T', 1._wp, &
                  &                            pa_ip, 'T', 1._wp, pv_ip, 'T', 1._wp, pv_il, 'T', 1._wp )
            ELSE
               CALL lbc_lnk( 'icedyn_adv_umx', pa_i , 'T', 1._wp, pv_i , 'T', 1._wp, pv_s , 'T', 1._wp, &
                  &                            psv_i, 'T', 1._wp, poa_i, 'T', 1._wp )
            ENDIF
            IF( nn_icesal == 4 ) THEN
               CALL lbc_lnk( 'icedyn_adv_umx', pe_i, 'T', 1._wp, pe_s, 'T', 1._wp, pszv_i, 'T', 1._wp )
            ELSE
               CALL lbc_lnk( 'icedyn_adv_umx', pe_i, 'T', 1._wp, pe_s, 'T', 1._wp )
            ENDIF
            !
         ENDIF
         !
      END DO
      !
      !
      ! --- Deallocate arrays --- !
      DEALLOCATE( zvar, zhvar, zamsk )
      IF( ln_pnd_LEV .OR. ln_pnd_TOPO ) DEALLOCATE( z1_aip )
      IF( np_advS == 3 )                DEALLOCATE( zuv_ho, zvv_ho, zuv_ups, zvv_ups, z1_vi, z1_vs )
      !
   END SUBROUTINE ice_dyn_adv_umx


   SUBROUTINE adv_umx( pamsk, kn_umx, jt, kt, pdt, pu, pv, puc, pvc, pubox, pvbox,  &
      &                                            pt, ptc, pua_ups, pva_ups, pua_ho, pva_ho )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE adv_umx  ***
      !!
      !! **  Purpose :   Compute the now trend due to total advection of
      !!                 tracers and add it to the general trend of tracer equations
      !!
      !! **  Method  :   - calculate upstream fluxes and upstream solution for tracers V/A(=H) etc
      !!                 - calculate tracer H at u and v points (Ultimate)
      !!                 - calculate the high order fluxes using alterning directions (Macho)
      !!                 - apply a limiter on the fluxes (nonosc_ice)
      !!                 - convert this tracer flux to a "volume" flux (uH -> uV)
      !!                 - apply a limiter a second time on the volumes fluxes (nonosc_ice)
      !!                 - calculate the high order solution for V
      !!
      !! ** Action : solve 3 equations => a) dA/dt  = -div(uA)
      !!                                  b) dV/dt  = -div(uV)  using dH/dt = -u.grad(H)
      !!                                  c) dVS/dt = -div(uVS) using either dHS/dt = -u.grad(HS) or dS/dt = -u.grad(S)
      !!
      !!             in eq. b), - fluxes uH are evaluated (with UMx) and limited with nonosc_ice. This step is necessary to get a good H.
      !!                        - then we convert this flux to a "volume" flux this way => uH * uA / u
      !!                             where uA is the flux from eq. a)
      !!                             this "volume" flux is also limited with nonosc_ice (otherwise overshoots can occur)
      !!                        - at last we estimate dV/dt = -div(uH * uA / u)
      !!
      !!             in eq. c), one can solve the equation for  S (ln_advS=T), then dVS/dt = -div(uV * uS  / u)
      !!                                                or for HS (ln_advS=F), then dVS/dt = -div(uA * uHS / u)
      !!
      !! ** Note : - this method can lead to tiny negative V (-1.e-20) => set it to 0 while conserving mass etc.
      !!           - At the ice edge, Ultimate scheme can lead to:
      !!                              1) negative interpolated tracers at u-v points
      !!                              2) non-zero interpolated tracers at u-v points eventhough there is no ice and velocity is outward
      !!                              Solution for 1): apply an upstream scheme when it occurs. A better solution would be to degrade the order of
      !!                                               the scheme automatically by applying a mask of the ice cover inside Ultimate (not done).
      !!                              Solution for 2): we set it to 0 in this case
      !!           - Eventhough 1D tests give very good results (typically the one from Schar & Smolarkiewiecz), the 2D is less good.
      !!             Large values of H can appear for very small ice concentration, and when it does it messes the things up since we
      !!             work on H (and not V). It is partly related to the multi-category approach
      !!             Therefore, after advection we limit the thickness to the largest value of the 9-points around (only if ice
      !!             concentration is small). We also limit S and T.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:)          , INTENT(in   )           ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   )           ::   kn_umx           ! order of the scheme (1-5=UM or 20=CEN2)
      INTEGER                         , INTENT(in   )           ::   jt               ! number of sub-iteration
      INTEGER                         , INTENT(in   )           ::   kt               ! number of iteration
      REAL(wp)                        , INTENT(in   )           ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   )           ::   pu   , pv        ! 2 ice velocity components => u*e2
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   )           ::   puc  , pvc       ! 2 ice velocity components => u*e2 or u*a*e2u
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   )           ::   pubox, pvbox     ! upstream velocity
      REAL(wp), DIMENSION(:,:,:,:)    , INTENT(inout)           ::   pt               ! tracer field
      REAL(wp), DIMENSION(:,:,:,:)    , INTENT(inout)           ::   ptc              ! tracer content field
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(inout), OPTIONAL ::   pua_ups, pva_ups ! upstream u*a fluxes
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out), OPTIONAL ::   pua_ho, pva_ho   ! high order u*a fluxes
      !
      INTEGER  ::   ji, jj, jl, jm   ! dummy loop indices
      INTEGER  ::   ndim             ! number of variables to advect
      REAL(wp) ::   ztra             ! local scalar
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   zfu_ho , zfv_ho
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   zfu_ups, zfv_ups, zt_ups
      !!----------------------------------------------------------------------
      !
      ndim = SIZE( ptc, dim=4 )
      !
      ALLOCATE( zfu_ho (jpi,jpj,jpl,ndim), zfv_ho (jpi,jpj,jpl,ndim), &
         &      zfu_ups(jpi,jpj,jpl,ndim), zfv_ups(jpi,jpj,jpl,ndim), zt_ups(jpi,jpj,jpl,ndim)  )  
      !
      ! Upstream (_ups) fluxes
      ! -----------------------
      DO jm = 1, ndim
         CALL upstream( pamsk(jm), jt, kt, pdt, pt(:,:,:,jm), pu(:,:), pv(:,:), &                        ! <<= in
            &                                   zt_ups(:,:,:,jm), zfu_ups(:,:,:,jm), zfv_ups(:,:,:,jm) ) ! =>> out ( gives zt_ups(1,1,1,1), zfu_ups(2,1,1,1) & zfv_ups(1,1,2,1) )
      ENDDO
      !
      ! High order (_ho) fluxes
      ! -----------------------
      SELECT CASE( kn_umx )
         !
      CASE ( 20 )                          !== centered second order ==!
         DO jm = 1, ndim
            CALL cen2( pamsk(jm), jt, kt, pdt, pt(:,:,:,jm), pu(:,:), pv(:,:), &  ! <<= in
               &                         zfu_ups(:,:,:,jm), zfv_ups(:,:,:,jm), &  ! <<= in  (upstream)
               &                         zfu_ho (:,:,:,jm), zfv_ho (:,:,:,jm)  )  ! =>> out (high order) ( gives zfu_ho(2,1,1,1) & zfv_ho(1,1,2,1) )
         ENDDO
      CASE ( 1:5 )                         !== 1st to 5th order ULTIMATE-MACHO scheme ==!
         CALL macho( pamsk(:), kn_umx, jt, kt, pdt, pt(:,:,:,:), pu(:,:), pv(:,:), pubox(:,:), pvbox(:,:), &  ! <<= in
            &                                                          zfu_ups(:,:,:,:), zfv_ups(:,:,:,:), &  ! <<= in  (upstream)
            &                                                          zfu_ho (:,:,:,:), zfv_ho (:,:,:,:)  )  ! =>> out (high order) ( gives zfu_ho(2,1,1,1) & zfv_ho(1,1,2,1) )
      END SELECT
      !
      ! Flux limiter
      ! ------------
      IF( np_limiter == 1 ) THEN
         CALL lbc_lnk( 'icedyn_adv_umx', zt_ups, 'T', 1.0_wp )   ! nonosc needs zt_ups over the whole domain
         DO jm = 1, ndim
            CALL nonosc_ice( pamsk(jm), pdt, pu, pv, pt(:,:,:,jm), zt_ups(:,:,:,jm), zfu_ups(:,:,:,jm), zfv_ups(:,:,:,jm), &
               &                                                                     zfu_ho (:,:,:,jm), zfv_ho (:,:,:,jm)  ) ! gives zfu_ho(1,0,0,0) & zfv_ho(0,0,1,0)
         ENDDO
         !
      ENDIF
      !              --ho    --ho
      ! new fluxes = u*H  *  u*a / u
      ! ----------------------------
      DO jm = 1, ndim
         IF( pamsk(jm) == 0._wp ) THEN
            
            DO jl = 1, jpl
               DO_2D( 1, 0, 0, 0 )
                  IF( ABS( pu(ji,jj) ) > epsi10 ) THEN   ;   zfu_ho(ji,jj,jl,jm) = zfu_ho(ji,jj,jl,jm) * puc(ji,jj,jl) / pu(ji,jj)
                  ELSE                                   ;   zfu_ho(ji,jj,jl,jm) = 0._wp   ;   ENDIF
               END_2D
               DO_2D( 0, 0, 1, 0 )
                  IF( ABS( pv(ji,jj) ) > epsi10 ) THEN   ;   zfv_ho(ji,jj,jl,jm) = zfv_ho(ji,jj,jl,jm) * pvc(ji,jj,jl) / pv(ji,jj)
                  ELSE                                   ;   zfv_ho(ji,jj,jl,jm) = 0._wp   ;   ENDIF
               END_2D
               DO_2D( 2, 1, 1, 1 )
                  IF( ABS( pu(ji,jj) ) > epsi10 ) THEN   ;   zfu_ups(ji,jj,jl,jm) = zfu_ups(ji,jj,jl,jm) * pua_ups(ji,jj,jl) / pu(ji,jj)
                  ELSE                                   ;   zfu_ups(ji,jj,jl,jm) = 0._wp  ;   ENDIF
               END_2D
               DO_2D( 1, 1, 2, 1 )
                  IF( ABS( pv(ji,jj) ) > epsi10 ) THEN   ;   zfv_ups(ji,jj,jl,jm) = zfv_ups(ji,jj,jl,jm) * pva_ups(ji,jj,jl) / pv(ji,jj)
                  ELSE                                   ;   zfv_ups(ji,jj,jl,jm) = 0._wp  ;   ENDIF
               END_2D
              
               ! the new "volume" fluxes must also be "flux corrected" => we calculate the upstream solution and apply a limiter again
               DO_2D( 0, 0, 0, 0 )
                  ztra = - (  ( zfu_ups(ji,jj,jl,jm) - zfu_ups(ji-1,jj,jl,jm) ) &   ! add () for NP repro
                     &      + ( zfv_ups(ji,jj,jl,jm) - zfv_ups(ji,jj-1,jl,jm) ) )
                  !
                  zt_ups(ji,jj,jl,jm) = ( ptc(ji,jj,jl,jm) + ztra * r1_e1e2t(ji,jj) * pdt ) * tmask(ji,jj,1)
               END_2D
            END DO
         ENDIF
      ENDDO
      ! lbc needed for nonosc
      IF( MINVAL( pamsk(:) ) == 0._wp .AND. np_limiter == 1 )  &
         &                   CALL lbc_lnk( 'icedyn_adv_umx', zt_ups, 'T', 1.0_wp, zfu_ho, 'U', -1.0_wp, zfv_ho, 'V', -1.0_wp  )
      ! flux limiter
      DO jm = 1, ndim
         IF( pamsk(jm) == 0._wp ) THEN
            IF    ( np_limiter == 1 ) THEN
               CALL nonosc_ice( 1._wp, pdt, pu, pv, ptc(:,:,:,jm), zt_ups (:,:,:,jm), zfu_ups(:,:,:,jm), zfv_ups(:,:,:,jm), &
                  &                                                                   zfu_ho (:,:,:,jm), zfv_ho (:,:,:,jm)  ) ! gives zfu_ho(1,0,0,0) & zfv_ho(0,0,1,0)
            ELSEIF( np_limiter == 2 .OR. np_limiter == 3 ) THEN
               CALL limiter_x( pdt, pu, ptc(:,:,:,jm), zfu_ups(:,:,:,jm), zfu_ho(:,:,:,jm) )
               CALL limiter_y( pdt, pv, ptc(:,:,:,jm), zfv_ups(:,:,:,jm), zfv_ho(:,:,:,jm) )
               
            ENDIF
         ENDIF
      ENDDO
      !
      !                                   --ho    --ups
      ! in case of advection of A: output u*a and u*a
      ! -----------------------------------------------
      IF( PRESENT( pua_ho ) ) THEN
         DO jl = 1, jpl
            DO_2D( 1, 0, 0, 0 )
               pua_ho (ji,jj,jl) = zfu_ho (ji,jj,jl,1)
            END_2D
            DO_2D( 0, 0, 1, 0 )
               pva_ho (ji,jj,jl) = zfv_ho (ji,jj,jl,1)
            END_2D
            DO_2D( 2, 1, 1, 1 )
               pua_ups(ji,jj,jl) = zfu_ups(ji,jj,jl,1)
            END_2D
            DO_2D( 1, 1, 2, 1 )
               pva_ups(ji,jj,jl) = zfv_ups(ji,jj,jl,1)
            END_2D
         END DO
      ENDIF
      !
      ! final trend with corrected fluxes
      ! ---------------------------------
      DO jm = 1, ndim
         DO jl = 1, jpl
            DO_2D( 0, 0, 0, 0 )
               ztra = - (  ( zfu_ho(ji,jj,jl,jm) - zfu_ho(ji-1,jj,jl,jm) ) &   ! add () for NP repro
                  &      + ( zfv_ho(ji,jj,jl,jm) - zfv_ho(ji,jj-1,jl,jm) ) )
               !
               ptc(ji,jj,jl,jm) = ( ptc(ji,jj,jl,jm) + ztra * r1_e1e2t(ji,jj) * pdt ) * tmask(ji,jj,1)
            END_2D
         END DO
      ENDDO
      !
      DEALLOCATE( zfu_ho, zfv_ho, zfu_ups, zfv_ups, zt_ups )
      !
   END SUBROUTINE adv_umx


   SUBROUTINE upstream( pamsk, jt, kt, pdt, pt, pu, pv, pt_ups, pfu_ups, pfv_ups )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE upstream  ***
      !!
      !! **  Purpose :   compute the upstream fluxes and upstream guess of tracer
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   ) ::   jt               ! number of sub-iteration
      INTEGER                         , INTENT(in   ) ::   kt               ! number of iteration
      REAL(wp)                        , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt               ! tracer fields
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pu, pv           ! 2 ice velocity components
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pt_ups           ! upstream guess of tracer
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pfu_ups, pfv_ups ! upstream fluxes
      !
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp) ::   ztra          ! local scalar
      REAL(wp), DIMENSION(jpi,jpj) ::   zpt
      !!----------------------------------------------------------------------

      IF( .NOT. ll_upsxy ) THEN         !** no alternate directions **!
         !
         DO jl = 1, jpl
            DO_2D( 2, 1, 2, 1 )
               pfu_ups(ji,jj,jl) = MAX( pu(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pu(ji,jj), 0._wp ) * pt(ji+1,jj,jl)
               pfv_ups(ji,jj,jl) = MAX( pv(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pv(ji,jj), 0._wp ) * pt(ji,jj+1,jl)
            END_2D
         END DO
         !
      ELSE                              !** alternate directions **!
         !
         IF( MOD( (kt - 1) / nn_fsbc , 2 ) ==  MOD( (jt - 1) , 2 ) ) THEN   !==  odd ice time step:  adv_x then adv_y  ==!
            !
            DO jl = 1, jpl              !-- flux in x-direction
               DO_2D( 2, 1, 2, 2 )
                  pfu_ups(ji,jj,jl) = MAX( pu(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pu(ji,jj), 0._wp ) * pt(ji+1,jj,jl)
               END_2D
               !                        !-- first guess of tracer from u-flux
               DO_2D( 1, 1, 2, 2 )
                  ztra = - ( pfu_ups(ji,jj,jl) - pfu_ups(ji-1,jj,jl) )              &
                     &   + ( pu     (ji,jj   ) - pu     (ji-1,jj   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                  !
                  zpt(ji,jj) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
               END_2D
               !                        !-- flux in y-direction
               DO_2D( 1, 1, 2, 1 )
                  pfv_ups(ji,jj,jl) = MAX( pv(ji,jj), 0._wp ) * zpt(ji,jj) + MIN( pv(ji,jj), 0._wp ) * zpt(ji,jj+1)
               END_2D
            END DO
            !
         ELSE                                                               !==  even ice time step:  adv_y then adv_x  ==!
            !
            DO jl = 1, jpl              !-- flux in y-direction
               DO_2D( 2, 2, 2, 1 )
                  pfv_ups(ji,jj,jl) = MAX( pv(ji,jj), 0._wp ) * pt(ji,jj,jl) + MIN( pv(ji,jj), 0._wp ) * pt(ji,jj+1,jl)
               END_2D
               !                        !-- first guess of tracer from v-flux
               DO_2D( 2, 2, 1, 1 )
                  ztra = - ( pfv_ups(ji,jj,jl) - pfv_ups(ji,jj-1,jl) )  &
                     &   + ( pv     (ji,jj   ) - pv     (ji,jj-1   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                  !
                  zpt(ji,jj) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
               END_2D
               !                        !-- flux in x-direction
               DO_2D( 2, 1, 1, 1 )
                  pfu_ups(ji,jj,jl) = MAX( pu(ji,jj), 0._wp ) * zpt(ji,jj) + MIN( pu(ji,jj), 0._wp ) * zpt(ji+1,jj)
               END_2D
            END DO
            !
         ENDIF

      ENDIF
      !
      DO jl = 1, jpl                    !-- after tracer with upstream scheme
         DO_2D( 1, 1, 1, 1 )
            ztra = - (   ( pfu_ups(ji,jj,jl) - pfu_ups(ji-1,jj  ,jl) )   &   ! add () for NP repro
               &       + ( pfv_ups(ji,jj,jl) - pfv_ups(ji  ,jj-1,jl) ) ) &
               &   + (   ( pu     (ji,jj   ) - pu     (ji-1,jj     ) )   &
               &       + ( pv     (ji,jj   ) - pv     (ji  ,jj-1   ) ) ) * pt(ji,jj,jl) * (1.-pamsk)
            !
            pt_ups(ji,jj,jl) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
         END_2D
      END DO

   END SUBROUTINE upstream


   SUBROUTINE cen2( pamsk, jt, kt, pdt, pt, pu, pv, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE cen2  ***
      !!
      !! **  Purpose :   compute the high order fluxes using a centered
      !!                 second order scheme
      !!----------------------------------------------------------------------
      REAL(wp)                        , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                         , INTENT(in   ) ::   jt               ! number of sub-iteration
      INTEGER                         , INTENT(in   ) ::   kt               ! number of iteration
      REAL(wp)                        , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pt               ! tracer fields
      REAL(wp), DIMENSION(:,:  )      , INTENT(in   ) ::   pu, pv           ! 2 ice velocity components
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   pfu_ups, pfv_ups ! upstream fluxes
      REAL(wp), DIMENSION(jpi,jpj,jpl), INTENT(  out) ::   pfu_ho, pfv_ho   ! high order fluxes
      !
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp) ::   ztra          ! local scalar
      REAL(wp), DIMENSION(jpi,jpj) ::   zpt
      !!----------------------------------------------------------------------
      !
      IF( .NOT.ll_hoxy ) THEN           !** no alternate directions **!
         !
         DO jl = 1, jpl
            DO_2D( 2, 1, 2, 2 )
               pfu_ho(ji,jj,jl) = 0.5_wp * pu(ji,jj) * ( pt(ji,jj,jl) + pt(ji+1,jj  ,jl) )
            END_2D
            DO_2D( 2, 2, 2, 1 )
               pfv_ho(ji,jj,jl) = 0.5_wp * pv(ji,jj) * ( pt(ji,jj,jl) + pt(ji  ,jj+1,jl) )
            END_2D
         END DO
         !
         IF( np_limiter == 2 .OR. np_limiter == 3 ) THEN
            CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )
            CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
         ENDIF
         !
      ELSE                              !** alternate directions **!
         !
         IF( MOD( (kt - 1) / nn_fsbc , 2 ) ==  MOD( (jt - 1) , 2 ) ) THEN   !==  odd ice time step:  adv_x then adv_y  ==!
            !
            DO jl = 1, jpl              !-- flux in x-direction
               DO_2D( 2, 1, 2, 2 )
                  pfu_ho(ji,jj,jl) = 0.5_wp * pu(ji,jj) * ( pt(ji,jj,jl) + pt(ji+1,jj,jl) )
               END_2D
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )

            DO jl = 1, jpl              !-- first guess of tracer from u-flux
               DO_2D( 1, 1, 2, 2 )
                  ztra = - ( pfu_ho(ji,jj,jl) - pfu_ho(ji-1,jj,jl) )              &
                     &   + ( pu    (ji,jj   ) - pu    (ji-1,jj   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                  !
                  zpt(ji,jj) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
               END_2D

               !                        !-- flux in y-direction
               DO_2D( 1, 1, 2, 1 )
                  pfv_ho(ji,jj,jl) = 0.5_wp * pv(ji,jj) * ( zpt(ji,jj) + zpt(ji,jj+1) )
               END_2D
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )

         ELSE                                                               !==  even ice time step:  adv_y then adv_x  ==!
            !
            DO jl = 1, jpl              !-- flux in y-direction
               DO_2D( 2, 2, 2, 1 )
                  pfv_ho(ji,jj,jl) = 0.5_wp * pv(ji,jj) * ( pt(ji,jj,jl) + pt(ji,jj+1,jl) )
               END_2D
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
            !
            DO jl = 1, jpl              !-- first guess of tracer from v-flux
               DO_2D( 2, 2, 1, 1 )
                  ztra = - ( pfv_ho(ji,jj,jl) - pfv_ho(ji,jj-1,jl) )  &
                     &   + ( pv    (ji,jj   ) - pv    (ji,jj-1   ) ) * pt(ji,jj,jl) * (1.-pamsk)
                  !
                  zpt(ji,jj) = ( pt(ji,jj,jl) + ztra * pdt * r1_e1e2t(ji,jj) ) * tmask(ji,jj,1)
               END_2D
               !
               !                        !-- flux in x-direction
               DO_2D( 2, 1, 1, 1 )
                  pfu_ho(ji,jj,jl) = 0.5_wp * pu(ji,jj) * ( zpt(ji,jj) + zpt(ji+1,jj) )
               END_2D
            END DO
            IF( np_limiter == 2 .OR. np_limiter == 3 )   CALL limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )

         ENDIF

      ENDIF
      !
      !
   END SUBROUTINE cen2


   SUBROUTINE macho( pamsk, kn_umx, jt, kt, pdt, pt, pu, pv, pubox, pvbox, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE macho  ***
      !!
      !! **  Purpose :   compute the high order fluxes using Ultimate-Macho scheme
      !!
      !! **  Method  :   ...
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:)      , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      INTEGER                     , INTENT(in   ) ::   kn_umx           ! order of the scheme (1-5=UM or 20=CEN2)
      INTEGER                     , INTENT(in   ) ::   jt               ! number of sub-iteration
      INTEGER                     , INTENT(in   ) ::   kt               ! number of iteration
      REAL(wp)                    , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pt               ! tracer fields
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   pu, pv           ! 2 ice velocity components
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   pubox, pvbox     ! upstream velocity
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pfu_ups, pfv_ups ! upstream fluxes
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pfu_ho, pfv_ho   ! high order fluxes (only out)
      !
      INTEGER  ::   ji, jj, jl, jm    ! dummy loop indices
      INTEGER  ::   ndim              ! number of variables to advect
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   zt_u, zt_v, zpt
      !!----------------------------------------------------------------------
      ndim = SIZE( pt, dim=4 )
      !
      ALLOCATE( zt_u(jpi,jpj,jpl,ndim), zt_v(jpi,jpj,jpl,ndim), zpt(jpi,jpj,jpl,ndim) )
      
      !
      IF( MOD( (kt - 1) / nn_fsbc , 2 ) ==  MOD( (jt - 1) , 2 ) ) THEN   !==  odd ice time step:  adv_x then adv_y  ==!
         !
         !                                                        !--  ultimate interpolation of pt at u-point  --!
         CALL ultimate_x( 2, pamsk, kn_umx, pdt, pt, pu, zt_u, pfu_ho )
         !
         !                                                        !--  limiter in x --!
         DO jm = 1, ndim
            IF( np_limiter == 2 .OR. np_limiter == 3 ) CALL limiter_x( pdt, pu, pt(:,:,:,jm), pfu_ups(:,:,:,jm), pfu_ho(:,:,:,jm) )
         END DO
         !                                                        !--  advective form update in zpt  --!
         DO jm = 1, ndim
            DO jl = 1, jpl
               DO_2D( 1, 1, 2, 2 )
                  zpt(ji,jj,jl,jm) = (pt(ji,jj,jl,jm) - ( pubox(ji,jj   ) * ( zt_u(ji,jj,jl,jm) - zt_u(ji-1,jj,jl,jm) )       &
                     &                                                    * r1_e1t(ji,jj)                                     &
                     &                                  + pt(ji,jj,jl,jm) * ( pu  (ji,jj)       - pu  (ji-1,jj) ) * pamsk(jm) &
                     &                                                    * r1_e1e2t(ji,jj)                                   &
                     &                                  ) * pdt) * tmask(ji,jj,1)
               END_2D
            END DO
         END DO
         !                                                        !--  ultimate interpolation of pt at v-point  --!
         IF( ll_hoxy ) THEN
            CALL ultimate_y( 1, pamsk, kn_umx, pdt, zpt, pv, zt_v, pfv_ho )
         ELSE
            CALL ultimate_y( 1, pamsk, kn_umx, pdt,  pt, pv, zt_v, pfv_ho )
         ENDIF
         !                                                        !--  limiter in y --!
         DO jm = 1, ndim
            IF( np_limiter == 2 .OR. np_limiter == 3 ) CALL limiter_y( pdt, pv, pt(:,:,:,jm), pfv_ups(:,:,:,jm), pfv_ho(:,:,:,jm) )
         END DO
         !
         !
      ELSE                                                               !==  even ice time step:  adv_y then adv_x  ==!
         !
         !                                                        !--  ultimate interpolation of pt at v-point  --!
         CALL ultimate_y( 2, pamsk, kn_umx, pdt, pt, pv, zt_v, pfv_ho )
         !
         !                                                        !--  limiter in y --!
         DO jm = 1, ndim
            IF( np_limiter == 2 .OR. np_limiter == 3 ) CALL limiter_y( pdt, pv, pt(:,:,:,jm), pfv_ups(:,:,:,jm), pfv_ho(:,:,:,jm) )
         END DO
         !                                                        !--  advective form update in zpt  --!
         DO jm = 1, ndim
            DO jl = 1, jpl
               DO_2D( 2, 2, 1, 1 )
                  zpt(ji,jj,jl,jm) = (pt(ji,jj,jl,jm) - ( pvbox(ji,jj   ) * ( zt_v(ji,jj,jl,jm) - zt_v(ji,jj-1,jl,jm) )       &
                     &                                                    * r1_e2t(ji,jj)                                     &
                     &                                  + pt(ji,jj,jl,jm) * ( pv  (ji,jj)       - pv  (ji,jj-1) ) * pamsk(jm) &
                     &                                                    * r1_e1e2t(ji,jj)                                   &
                     &                                  ) * pdt) * tmask(ji,jj,1)
               END_2D
            END DO
         END DO
         !                                                        !--  ultimate interpolation of pt at u-point  --!
         IF( ll_hoxy ) THEN
            CALL ultimate_x( 1, pamsk, kn_umx, pdt, zpt, pu, zt_u, pfu_ho )
         ELSE
            CALL ultimate_x( 1, pamsk, kn_umx, pdt,  pt, pu, zt_u, pfu_ho )
         ENDIF
         !                                                        !--  limiter in x --!
         DO jm = 1, ndim
            IF( np_limiter == 2 .OR. np_limiter == 3 ) CALL limiter_x( pdt, pu, pt(:,:,:,jm), pfu_ups(:,:,:,jm), pfu_ho(:,:,:,jm) )
         END DO
         !
      ENDIF
      
      DEALLOCATE( zt_u, zt_v, zpt )

      !
   END SUBROUTINE macho


   SUBROUTINE ultimate_x( kloop, pamsk, kn_umx, pdt, pt, pu, pt_u, pfu_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE ultimate_x  ***
      !!
      !! **  Purpose :   compute tracer at u-points
      !!
      !! **  Method  :   ...
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74.
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   kloop     ! either 0 or nn_hls depending on the order of the call
      REAL(wp), DIMENSION(:)      , INTENT(in   ) ::   pamsk     ! advection of concentration (1) or other tracers (0)
      INTEGER                     , INTENT(in   ) ::   kn_umx    ! order of the scheme (1-5=UM or 20=CEN2)
      REAL(wp)                    , INTENT(in   ) ::   pdt       ! tracer time-step
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   pu        ! ice i-velocity component
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pt        ! tracer fields
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pt_u      ! tracer at u-point (only out)
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pfu_ho    ! high order flux   (only out)
      !
      INTEGER  ::   ji, jj, jl, jm   ! dummy loop indices
      INTEGER  ::   ndim             ! number of variables to advect
      REAL(wp) ::   zcu, zdx2, zdx4        !   -      -
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)     ::   ztu1, ztu3
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ztu2, ztu4
      !!----------------------------------------------------------------------
      ndim = SIZE( pt, dim=4 )
      !
      IF( kn_umx >= 3 )    ALLOCATE( ztu1(jpi,jpj), ztu2(jpi,jpj,jpl,ndim) )
      IF( kn_umx == 5 )    ALLOCATE( ztu3(jpi,jpj), ztu4(jpi,jpj,jpl,ndim) )
      !
      DO jm = 1, ndim
         !                                                     !--  Laplacian in i-direction  --!
         IF( kn_umx >= 3 ) THEN
            DO jl = 1, jpl
               DO_2D( 2, 1, kloop, kloop )                ! First derivative (gradient)
                  ztu1(ji,jj) = ( pt(ji+1,jj,jl,jm) - pt(ji,jj,jl,jm) ) * r1_e1u(ji,jj) * umask(ji,jj,1)
               END_2D
               DO_2D( 1, 1, kloop, kloop )                ! Second derivative (Laplacian)
                  ztu2(ji,jj,jl,jm) = ( ztu1(ji,jj) - ztu1(ji-1,jj) ) * r1_e1t(ji,jj)
               END_2D
            END DO
         ENDIF
         !                                                     !--  BiLaplacian in i-direction  --!
         IF( kn_umx == 5 ) THEN
            DO jl = 1, jpl
               DO_2D( 1, 0, kloop, kloop )                ! Third derivative
                  ztu3(ji,jj) = ( ztu2(ji+1,jj,jl,jm) - ztu2(ji,jj,jl,jm) ) * r1_e1u(ji,jj) * umask(ji,jj,1)
               END_2D
               DO_2D( 0, 0, kloop, kloop )                ! Fourth derivative
                  ztu4(ji,jj,jl,jm) = ( ztu3(ji,jj) - ztu3(ji-1,jj) ) * r1_e1t(ji,jj)
               END_2D
            END DO
         ENDIF
         !
      ENDDO
      ! lbc only needed for some orders
      IF    ( kn_umx == 3 .OR. kn_umx == 4 ) THEN   ;   CALL lbc_lnk( 'icedyn_adv_umx', ztu2, 'T', 1.0_wp )
      ELSEIF( kn_umx == 5 )                  THEN   ;   CALL lbc_lnk( 'icedyn_adv_umx', ztu2, 'T', 1.0_wp, ztu4, 'T', 1.0_wp )
      ENDIF
      !
      !
      DO jm = 1, ndim
         !
         SELECT CASE ( kn_umx )
            !
         CASE( 1 )                                                   !==  1st order central TIM  ==! (Eq. 21)
            !
            DO jl = 1, jpl
               DO_2D( 2, 1, kloop, kloop )
                  pt_u(ji,jj,jl,jm) = 0.5_wp * umask(ji,jj,1) * (                          ( pt(ji+1,jj,jl,jm) + pt(ji,jj,jl,jm) ) &
                     &                                        - SIGN( 1._wp, pu(ji,jj) ) * ( pt(ji+1,jj,jl,jm) - pt(ji,jj,jl,jm) ) )
               END_2D
            END DO
            !
         CASE( 2 )                                                   !==  2nd order central TIM  ==! (Eq. 23)
            !
            DO jl = 1, jpl
               DO_2D( 2, 1, kloop, kloop )
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  pt_u(ji,jj,jl,jm) = 0.5_wp * umask(ji,jj,1) * (                    ( pt(ji+1,jj,jl,jm) + pt(ji,jj,jl,jm) )  &
                     &                                                     - zcu   * ( pt(ji+1,jj,jl,jm) - pt(ji,jj,jl,jm) ) )
               END_2D
            END DO
            !
         CASE( 3 )                                                   !==  3rd order central TIM  ==! (Eq. 24)
            !
            DO jl = 1, jpl
               DO_2D( 2, 1, kloop, kloop )
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  zdx2 = e1u(ji,jj) * e1u(ji,jj)
                  !!rachid          zdx2 = e1u(ji,jj) * e1t(ji,jj)
                  pt_u(ji,jj,jl,jm) = 0.5_wp * umask(ji,jj,1) * ( (                  ( pt  (ji+1,jj,jl,jm) + pt  (ji,jj,jl,jm) )   &
                     &                                                     - zcu   * ( pt  (ji+1,jj,jl,jm) - pt  (ji,jj,jl,jm) ) ) &
                     & + r1_6 * zdx2 * ( zcu*zcu - 1._wp ) *      (                  ( ztu2(ji+1,jj,jl,jm) + ztu2(ji,jj,jl,jm) )   &
                     &                                        - SIGN( 1._wp, zcu ) * ( ztu2(ji+1,jj,jl,jm) - ztu2(ji,jj,jl,jm) ) ) )
               END_2D
            END DO
            !
         CASE( 4 )                                                   !==  4th order central TIM  ==! (Eq. 27)
            !
            DO jl = 1, jpl
               DO_2D( 2, 1, kloop, kloop )
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  zdx2 = e1u(ji,jj) * e1u(ji,jj)
                  !!rachid          zdx2 = e1u(ji,jj) * e1t(ji,jj)
                  pt_u(ji,jj,jl,jm) = 0.5_wp * umask(ji,jj,1) * ( (                  ( pt  (ji+1,jj,jl,jm) + pt  (ji,jj,jl,jm) )   &
                     &                                                     - zcu   * ( pt  (ji+1,jj,jl,jm) - pt  (ji,jj,jl,jm) ) ) &
                     & + r1_6 * zdx2 * ( zcu*zcu - 1._wp ) *      (                  ( ztu2(ji+1,jj,jl,jm) + ztu2(ji,jj,jl,jm) )   &
                     &                                            - 0.5_wp * zcu   * ( ztu2(ji+1,jj,jl,jm) - ztu2(ji,jj,jl,jm) ) ) )
               END_2D
            END DO
            !
         CASE( 5 )                                                   !==  5th order central TIM  ==! (Eq. 29)
            !
            DO jl = 1, jpl
               DO_2D( 2, 1, kloop, kloop )
                  zcu  = pu(ji,jj) * r1_e2u(ji,jj) * pdt * r1_e1u(ji,jj)
                  zdx2 = e1u(ji,jj) * e1u(ji,jj)
                  !!rachid          zdx2 = e1u(ji,jj) * e1t(ji,jj)
                  zdx4 = zdx2 * zdx2
                  pt_u(ji,jj,jl,jm) = 0.5_wp * umask(ji,jj,1) * ( (                  ( pt  (ji+1,jj,jl,jm) + pt  (ji,jj,jl,jm) )   &
                     &                                                     - zcu   * ( pt  (ji+1,jj,jl,jm) - pt  (ji,jj,jl,jm) ) ) &
                     & + r1_6   * zdx2 * ( zcu*zcu - 1._wp ) *    (                  ( ztu2(ji+1,jj,jl,jm) + ztu2(ji,jj,jl,jm) )   &
                     &                                            - 0.5_wp * zcu   * ( ztu2(ji+1,jj,jl,jm) - ztu2(ji,jj,jl,jm) ) ) &
                     & + r1_120 * zdx4 * ( zcu*zcu - 1._wp ) * ( zcu*zcu - 4._wp ) * ((ztu4(ji+1,jj,jl,jm) + ztu4(ji,jj,jl,jm) )   &
                     &                                        - SIGN( 1._wp, zcu ) * ( ztu4(ji+1,jj,jl,jm) - ztu4(ji,jj,jl,jm) ) ) )
               END_2D
            END DO
            !
         END SELECT
         !
         !
         ! if pt at u-point is negative then use the upstream value
         !    this should not be necessary if a proper sea-ice mask is set in Ultimate
         !    to degrade the order of the scheme when necessary (for ex. at the ice edge)
         IF( ll_neg ) THEN
            DO jl = 1, jpl
               DO_2D( 2, 1, kloop, kloop )
                  IF( pt_u(ji,jj,jl,jm) < 0._wp .OR. ( imsk_small(ji,jj,jl) == 0 .AND. pamsk(jm) == 0. ) ) THEN
                     pt_u(ji,jj,jl,jm) = 0.5_wp * umask(ji,jj,1) * (                      ( pt(ji+1,jj,jl,jm) + pt(ji,jj,jl,jm) ) &
                        &                                    - SIGN( 1._wp, pu(ji,jj) ) * ( pt(ji+1,jj,jl,jm) - pt(ji,jj,jl,jm) ) )
                  ENDIF
               END_2D
            END DO
         ENDIF
         !                                                     !-- High order flux in i-direction  --!
         DO jl = 1, jpl
            DO_2D( 2, 1, 1, 1 )
               pfu_ho(ji,jj,jl,jm) = pu(ji,jj) * pt_u(ji,jj,jl,jm)
            END_2D
         END DO
         !
      ENDDO
      
      IF( kn_umx >= 3 )    DEALLOCATE( ztu1, ztu2 )
      IF( kn_umx == 5 )    DEALLOCATE( ztu3, ztu4 )
      !
   END SUBROUTINE ultimate_x


   SUBROUTINE ultimate_y( kloop, pamsk, kn_umx, pdt, pt, pv, pt_v, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE ultimate_y  ***
      !!
      !! **  Purpose :   compute tracer at v-points
      !!
      !! **  Method  :   ...
      !!
      !! Reference : Leonard, B.P., 1991, Comput. Methods Appl. Mech. Eng., 88, 17-74.
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   kloop     ! either 0 or nn_hls depending on the order of the call
      REAL(wp), DIMENSION(:)      , INTENT(in   ) ::   pamsk     ! advection of concentration (1) or other tracers (0)
      INTEGER                     , INTENT(in   ) ::   kn_umx    ! order of the scheme (1-5=UM or 20=CEN2)
      REAL(wp)                    , INTENT(in   ) ::   pdt       ! tracer time-step
      REAL(wp), DIMENSION(:,:  )  , INTENT(in   ) ::   pv        ! ice j-velocity component
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   pt        ! tracer fields
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pt_v      ! tracer at v-point (only out)
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pfv_ho    ! high order flux   (only out)
      !
      INTEGER  ::   ji, jj, jl, jm   ! dummy loop indices
      INTEGER  ::   ndim             ! number of variables to advect
      REAL(wp) ::   zcv, zdy2, zdy4    !   -      -
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)     ::   ztv1, ztv3
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ztv2, ztv4
      !!----------------------------------------------------------------------
      ndim = SIZE( pt, dim=4 )
      !
      IF( kn_umx >= 3 )    ALLOCATE( ztv1(jpi,jpj), ztv2(jpi,jpj,jpl,ndim) )
      IF( kn_umx == 5 )    ALLOCATE( ztv3(jpi,jpj), ztv4(jpi,jpj,jpl,ndim) )
      !
      DO jm = 1, ndim
         !                                                     !--  Laplacian in j-direction  --!
         IF( kn_umx >= 3 ) THEN
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 2, 1 )                ! First derivative (gradient)
                  ztv1(ji,jj) = ( pt(ji,jj+1,jl,jm) - pt(ji,jj,jl,jm) ) * r1_e2v(ji,jj) * vmask(ji,jj,1)
               END_2D
               DO_2D( kloop, kloop, 1, 1 )                ! Second derivative (Laplacian)
                  ztv2(ji,jj,jl,jm) = ( ztv1(ji,jj) - ztv1(ji,jj-1) ) * r1_e2t(ji,jj)
               END_2D
            END DO
         ENDIF
         !                                                     !--  BiLaplacian in j-direction  --!
         IF( kn_umx == 5 ) THEN
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 1, 0 )                ! Third derivative
                  ztv3(ji,jj) = ( ztv2(ji,jj+1,jl,jm) - ztv2(ji,jj,jl,jm) ) * r1_e2v(ji,jj) * vmask(ji,jj,1)
               END_2D
               DO_2D( kloop, kloop, 0, 0 )                ! Fourth derivative
                  ztv4(ji,jj,jl,jm) = ( ztv3(ji,jj) - ztv3(ji,jj-1) ) * r1_e2t(ji,jj)
               END_2D
            END DO
         ENDIF
         !
      ENDDO
      ! lbc only needed for some orders
      IF    ( kn_umx == 3 .OR. kn_umx == 4 ) THEN   ;   CALL lbc_lnk( 'icedyn_adv_umx', ztv2, 'T', 1.0_wp )
      ELSEIF( kn_umx == 5 )                  THEN   ;   CALL lbc_lnk( 'icedyn_adv_umx', ztv2, 'T', 1.0_wp, ztv4, 'T', 1.0_wp )
      ENDIF
      !
      !
      DO jm = 1, ndim
         SELECT CASE ( kn_umx )
            !
         CASE( 1 )                                                !==  1st order central TIM  ==! (Eq. 21)
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 2, 1 )
                  pt_v(ji,jj,jl,jm) = 0.5_wp * vmask(ji,jj,1) * (                          ( pt(ji,jj+1,jl,jm) + pt(ji,jj,jl,jm) ) &
                     &                                        - SIGN( 1._wp, pv(ji,jj) ) * ( pt(ji,jj+1,jl,jm) - pt(ji,jj,jl,jm) ) )
               END_2D
            END DO
            !
         CASE( 2 )                                                !==  2nd order central TIM  ==! (Eq. 23)
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 2, 1 )
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  pt_v(ji,jj,jl,jm) = 0.5_wp * vmask(ji,jj,1) * (                    ( pt(ji,jj+1,jl,jm) + pt(ji,jj,jl,jm) ) &
                     &                                                     - zcv   * ( pt(ji,jj+1,jl,jm) - pt(ji,jj,jl,jm) ) )
               END_2D
            END DO
            !
         CASE( 3 )                                                !==  3rd order central TIM  ==! (Eq. 24)
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 2, 1 )
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  zdy2 = e2v(ji,jj) * e2v(ji,jj)
                  !!rachid          zdy2 = e2v(ji,jj) * e2t(ji,jj)
                  pt_v(ji,jj,jl,jm) = 0.5_wp * vmask(ji,jj,1) * ( (                  ( pt  (ji,jj+1,jl,jm) + pt  (ji,jj,jl,jm) )   &
                     &                                                     - zcv   * ( pt  (ji,jj+1,jl,jm) - pt  (ji,jj,jl,jm) ) ) &
                     & + r1_6 * zdy2 * ( zcv*zcv - 1._wp ) * (                       ( ztv2(ji,jj+1,jl,jm) + ztv2(ji,jj,jl,jm) )   &
                     &                                        - SIGN( 1._wp, zcv ) * ( ztv2(ji,jj+1,jl,jm) - ztv2(ji,jj,jl,jm) ) ) )
               END_2D
            END DO
            !
         CASE( 4 )                                                !==  4th order central TIM  ==! (Eq. 27)
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 2, 1 )
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  zdy2 = e2v(ji,jj) * e2v(ji,jj)
                  !!rachid          zdy2 = e2v(ji,jj) * e2t(ji,jj)
                  pt_v(ji,jj,jl,jm) = 0.5_wp * vmask(ji,jj,1) * ( (                  ( pt  (ji,jj+1,jl,jm) + pt  (ji,jj,jl,jm) )   &
                     &                                                     - zcv   * ( pt  (ji,jj+1,jl,jm) - pt  (ji,jj,jl,jm) ) ) &
                     & + r1_6 * zdy2 * ( zcv*zcv - 1._wp ) * (                       ( ztv2(ji,jj+1,jl,jm) + ztv2(ji,jj,jl,jm) )   &
                     &                                            - 0.5_wp * zcv   * ( ztv2(ji,jj+1,jl,jm) - ztv2(ji,jj,jl,jm) ) ) )
               END_2D
            END DO
            !
         CASE( 5 )                                                !==  5th order central TIM  ==! (Eq. 29)
            !
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 2, 1 )
                  zcv  = pv(ji,jj) * r1_e1v(ji,jj) * pdt * r1_e2v(ji,jj)
                  zdy2 = e2v(ji,jj) * e2v(ji,jj)
                  !!rachid          zdy2 = e2v(ji,jj) * e2t(ji,jj)
                  zdy4 = zdy2 * zdy2
                  pt_v(ji,jj,jl,jm) = 0.5_wp * vmask(ji,jj,1) * ( (                  ( pt  (ji,jj+1,jl,jm) + pt  (ji,jj,jl,jm) )   &
                     &                                                     - zcv   * ( pt  (ji,jj+1,jl,jm) - pt  (ji,jj,jl,jm) ) ) &
                     & + r1_6   * zdy2 * ( zcv*zcv - 1._wp ) *    (                  ( ztv2(ji,jj+1,jl,jm) + ztv2(ji,jj,jl,jm) )   &
                     &                                            - 0.5_wp * zcv   * ( ztv2(ji,jj+1,jl,jm) - ztv2(ji,jj,jl,jm) ) ) &
                     & + r1_120 * zdy4 * ( zcv*zcv - 1._wp ) * ( zcv*zcv - 4._wp ) * ((ztv4(ji,jj+1,jl,jm) + ztv4(ji,jj,jl,jm) )   &
                     &                                        - SIGN( 1._wp, zcv ) * ( ztv4(ji,jj+1,jl,jm) - ztv4(ji,jj,jl,jm) ) ) )
               END_2D
            END DO
            !
         END SELECT
         !
         ! if pt at v-point is negative then use the upstream value
         !    this should not be necessary if a proper sea-ice mask is set in Ultimate
         !    to degrade the order of the scheme when necessary (for ex. at the ice edge)
         IF( ll_neg ) THEN
            DO jl = 1, jpl
               DO_2D( kloop, kloop, 2, 1 )
                  IF( pt_v(ji,jj,jl,jm) < 0._wp .OR. ( jmsk_small(ji,jj,jl) == 0 .AND. pamsk(jm) == 0. ) ) THEN
                     pt_v(ji,jj,jl,jm) = 0.5_wp * vmask(ji,jj,1) * (                      ( pt(ji,jj+1,jl,jm) + pt(ji,jj,jl,jm) ) &
                        &                                    - SIGN( 1._wp, pv(ji,jj) ) * ( pt(ji,jj+1,jl,jm) - pt(ji,jj,jl,jm) ) )
                  ENDIF
               END_2D
            END DO
         ENDIF
         !                                                     !-- High order flux in j-direction  --!
         DO jl = 1, jpl
            DO_2D( 1, 1, 2, 1 )
               pfv_ho(ji,jj,jl,jm) = pv(ji,jj) * pt_v(ji,jj,jl,jm)
            END_2D
         END DO
         !
      ENDDO
      !
      IF( kn_umx >= 3 )    DEALLOCATE( ztv1, ztv2 )
      IF( kn_umx == 5 )    DEALLOCATE( ztv3, ztv4 )
      !
   END SUBROUTINE ultimate_y


   SUBROUTINE nonosc_ice( pamsk, pdt, pu, pv, pt, pt_ups, pfu_ups, pfv_ups, pfu_ho, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE nonosc_ice  ***
      !!
      !! **  Purpose :   compute monotonic tracer fluxes from the upstream
      !!       scheme and the before field by a non-oscillatory algorithm
      !!
      !! **  Method  :   ...
      !!----------------------------------------------------------------------
      REAL(wp)                   , INTENT(in   ) ::   pamsk            ! advection of concentration (1) or other tracers (0)
      REAL(wp)                   , INTENT(in   ) ::   pdt              ! tracer time-step
      REAL(wp), DIMENSION (:,:)  , INTENT(in   ) ::   pu               ! ice i-velocity => u*e2
      REAL(wp), DIMENSION (:,:)  , INTENT(in   ) ::   pv               ! ice j-velocity => v*e1
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pt, pt_ups       ! before field & upstream guess of after field
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pfv_ups, pfu_ups ! upstream flux
      REAL(wp), DIMENSION (:,:,:), INTENT(inout) ::   pfv_ho, pfu_ho   ! monotonic flux
      !
      INTEGER  ::   ji, jj, jl   ! dummy loop indices
      REAL(wp) ::   zpos, zneg, zbig, zup, zdo, z1_dt              ! local scalars
      REAL(wp) ::   zau, zbu, zcu, zav, zbv, zcv, zcoef, zzt       !   -      -
      REAL(wp), DIMENSION(jpi,jpj)     ::   zbup, zbdo
      REAL(wp), DIMENSION(jpi,jpj,jpl) ::   zbetup, zbetdo
      !!----------------------------------------------------------------------
      zbig = 1.e+20_wp   ! works ok with simple/double precison

      !
      ! antidiffusive flux : high order minus low order
      ! --------------------------------------------------
      DO jl = 1, jpl
         DO_2D( 2, 1, 1, 1 )
            pfu_ho(ji,jj,jl) = pfu_ho(ji,jj,jl) - pfu_ups(ji,jj,jl)
         END_2D
         DO_2D( 1, 1, 2, 1 )
            pfv_ho(ji,jj,jl) = pfv_ho(ji,jj,jl) - pfv_ups(ji,jj,jl)
         END_2D
      END DO
      
      ! extreme case where pfu_ho has to be zero
      ! ----------------------------------------
      !                                    pfu_ho
      !                           *         --->
      !                        |      |  *   |        |
      !                        |      |      |    *   |
      !                        |      |      |        |    *
      !            t_ups :       i-1     i       i+1       i+2
      IF( ll_prelim ) THEN         
        
         DO jl = 1, jpl
            DO_2D( 0, 0, 0, 0 )
               IF ( pfu_ho(ji,jj,jl) * ( pt_ups(ji+1,jj  ,jl) - pt_ups(ji,jj,jl) ) <= 0._wp .AND.  &
                  & pfv_ho(ji,jj,jl) * ( pt_ups(ji  ,jj+1,jl) - pt_ups(ji,jj,jl) ) <= 0._wp ) THEN
                  !
                  IF(  pfu_ho(ji,jj,jl) * ( pt_ups(ji+2,jj  ,jl) - pt_ups(ji+1,jj  ,jl) ) <= 0._wp .AND.  &
                     & pfv_ho(ji,jj,jl) * ( pt_ups(ji  ,jj+2,jl) - pt_ups(ji  ,jj+1,jl) ) <= 0._wp ) THEN
                     pfu_ho(ji,jj,jl)=0._wp
                     pfv_ho(ji,jj,jl)=0._wp
                  ENDIF
                  !
                  IF(  pfu_ho(ji,jj,jl) * ( pt_ups(ji,jj,jl) - pt_ups(ji-1,jj  ,jl) ) <= 0._wp .AND.  &
                     & pfv_ho(ji,jj,jl) * ( pt_ups(ji,jj,jl) - pt_ups(ji  ,jj-1,jl) ) <= 0._wp ) THEN
                     pfu_ho(ji,jj,jl)=0._wp
                     pfv_ho(ji,jj,jl)=0._wp
                  ENDIF
                  !
               ENDIF
            END_2D
         END DO
         CALL lbc_lnk( 'icedyn_adv_umx', pfu_ho, 'U', -1.0_wp, pfv_ho, 'V', -1.0_wp )   ! lateral boundary cond.
         
      ENDIF

      ! Search local extrema
      ! --------------------
      ! max/min of pt & pt_ups with large negative/positive value (-/+zbig) outside ice cover
      z1_dt = 1._wp / pdt

      DO jl = 1, jpl
         
         DO_2D( 2, 2, 2, 2 )
            IF    ( pt(ji,jj,jl) <= 0._wp .AND. pt_ups(ji,jj,jl) <= 0._wp ) THEN
               zbup(ji,jj) = -zbig
               zbdo(ji,jj) =  zbig
            ELSEIF( pt(ji,jj,jl) <= 0._wp .AND. pt_ups(ji,jj,jl) > 0._wp ) THEN
               zbup(ji,jj) = pt_ups(ji,jj,jl)
               zbdo(ji,jj) = pt_ups(ji,jj,jl)
            ELSEIF( pt(ji,jj,jl) > 0._wp .AND. pt_ups(ji,jj,jl) <= 0._wp ) THEN
               zbup(ji,jj) = pt(ji,jj,jl)
               zbdo(ji,jj) = pt(ji,jj,jl)
            ELSE
               zbup(ji,jj) = MAX( pt(ji,jj,jl) , pt_ups(ji,jj,jl) )
               zbdo(ji,jj) = MIN( pt(ji,jj,jl) , pt_ups(ji,jj,jl) )
            ENDIF
         END_2D
         
         DO_2D( 1, 1, 1, 1 )
            !
            zup  = MAX( zbup(ji,jj), zbup(ji-1,jj), zbup(ji+1,jj), zbup(ji,jj-1), zbup(ji,jj+1) )  ! search max/min in neighbourhood
            zdo  = MIN( zbdo(ji,jj), zbdo(ji-1,jj), zbdo(ji+1,jj), zbdo(ji,jj-1), zbdo(ji,jj+1) )
            !
            zpos = MAX( 0._wp, pfu_ho(ji-1,jj  ,jl) ) - MIN( 0._wp, pfu_ho(ji  ,jj  ,jl) ) &  ! positive/negative part of the flux
               & + MAX( 0._wp, pfv_ho(ji  ,jj-1,jl) ) - MIN( 0._wp, pfv_ho(ji  ,jj  ,jl) )
            zneg = MAX( 0._wp, pfu_ho(ji  ,jj  ,jl) ) - MIN( 0._wp, pfu_ho(ji-1,jj  ,jl) ) &
               & + MAX( 0._wp, pfv_ho(ji  ,jj  ,jl) ) - MIN( 0._wp, pfv_ho(ji  ,jj-1,jl) )
            !
            zpos = zpos - (  pt(ji,jj,jl) * MIN( 0., pu(ji,jj) - pu(ji-1,jj) )   &
               &           + pt(ji,jj,jl) * MIN( 0., pv(ji,jj) - pv(ji,jj-1) ) ) * ( 1. - pamsk )
            zneg = zneg + (  pt(ji,jj,jl) * MAX( 0., pu(ji,jj) - pu(ji-1,jj) )   &
               &           + pt(ji,jj,jl) * MAX( 0., pv(ji,jj) - pv(ji,jj-1) ) ) * ( 1. - pamsk )
            !
            !                                  ! up & down beta terms
            ! clem: zbetup and zbetdo must be 0 for zpos>1.e-10 & zneg>1.e-10 (do not put 0 instead of 1.e-10 !!!)
            IF( zpos > epsi10 ) THEN ; zbetup(ji,jj,jl) = MAX( 0._wp, zup - pt_ups(ji,jj,jl) ) / zpos * e1e2t(ji,jj) * z1_dt
            ELSE                     ; zbetup(ji,jj,jl) = 0._wp ! zbig
            ENDIF
            !
            IF( zneg > epsi10 ) THEN ; zbetdo(ji,jj,jl) = MAX( 0._wp, pt_ups(ji,jj,jl) - zdo ) / zneg * e1e2t(ji,jj) * z1_dt
            ELSE                     ; zbetdo(ji,jj,jl) = 0._wp ! zbig
            ENDIF
            !
            ! if all the points are outside ice cover
            IF( zup == -zbig )   zbetup(ji,jj,jl) = 0._wp ! zbig
            IF( zdo ==  zbig )   zbetdo(ji,jj,jl) = 0._wp ! zbig
            !
         END_2D
      END DO   

      ! monotonic flux in the y direction
      ! ---------------------------------
      DO jl = 1, jpl
         DO_2D( 1, 0, 0, 0 )
            zau = MIN( 1._wp , zbetdo(ji,jj,jl) , zbetup(ji+1,jj,jl) )
            zbu = MIN( 1._wp , zbetup(ji,jj,jl) , zbetdo(ji+1,jj,jl) )
            zcu = 0.5_wp + SIGN( 0.5_wp , pfu_ho(ji,jj,jl) )
            !
            zcoef = ( zcu * zau + ( 1._wp - zcu ) * zbu )
            !
            pfu_ho(ji,jj,jl) = pfu_ho(ji,jj,jl) * zcoef + pfu_ups(ji,jj,jl)
            !
         END_2D
         
         DO_2D( 0, 0, 1, 0 )
            zav = MIN( 1._wp , zbetdo(ji,jj,jl) , zbetup(ji,jj+1,jl) )
            zbv = MIN( 1._wp , zbetup(ji,jj,jl) , zbetdo(ji,jj+1,jl) )
            zcv = 0.5_wp + SIGN( 0.5_wp , pfv_ho(ji,jj,jl) )
            !
            zcoef = ( zcv * zav + ( 1._wp - zcv ) * zbv )
            !
            pfv_ho(ji,jj,jl) = pfv_ho(ji,jj,jl) * zcoef + pfv_ups(ji,jj,jl)
            !
         END_2D
         
      END DO
      !
      
   END SUBROUTINE nonosc_ice


   SUBROUTINE limiter_x( pdt, pu, pt, pfu_ups, pfu_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE limiter_x  ***
      !!
      !! **  Purpose :   compute flux limiter
      !!----------------------------------------------------------------------
      REAL(wp)                  , INTENT(in   ) ::   pdt          ! tracer time-step
      REAL(wp), DIMENSION(:,:  ), INTENT(in   ) ::   pu           ! ice i-velocity => u*e2
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pt           ! ice tracer
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pfu_ups      ! upstream flux
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   pfu_ho       ! high order flux
      !
      REAL(wp) ::   Cr, Rjm, Rj, Rjp, uCFL, zpsi, zh3, zlimiter, Rr
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp), DIMENSION (jpi,jpj) ::   zslpx       ! tracer slopes
      !!----------------------------------------------------------------------
      !

      DO jl = 1, jpl
         
         DO_2D( 2, 1, 0, 0 )
            zslpx(ji,jj) = ( pt(ji+1,jj,jl) - pt(ji,jj,jl) ) * umask(ji,jj,1)
         END_2D
         
         DO_2D( 1, 0, 0, 0 )
            uCFL = pdt * ABS( pu(ji,jj) ) * r1_e1e2t(ji,jj)

            Rjm = zslpx(ji-1,jj)
            Rj  = zslpx(ji  ,jj)
            Rjp = zslpx(ji+1,jj)

            IF( np_limiter == 3 ) THEN

               IF( pu(ji,jj) > 0. ) THEN   ;   Rr = Rjm
               ELSE                        ;   Rr = Rjp
               ENDIF

               zh3 = pfu_ho(ji,jj,jl) - pfu_ups(ji,jj,jl)
               IF( Rj > 0. ) THEN
                  zlimiter =  MAX( 0., MIN( zh3, MAX(-Rr * 0.5 * ABS(pu(ji,jj)),  &
                     &        MIN( 2. * Rr * 0.5 * ABS(pu(ji,jj)),  zh3,  1.5 * Rj * 0.5 * ABS(pu(ji,jj)) ) ) ) )
               ELSE
                  zlimiter = -MAX( 0., MIN(-zh3, MAX( Rr * 0.5 * ABS(pu(ji,jj)),  &
                     &        MIN(-2. * Rr * 0.5 * ABS(pu(ji,jj)), -zh3, -1.5 * Rj * 0.5 * ABS(pu(ji,jj)) ) ) ) )
               ENDIF
               pfu_ho(ji,jj,jl) = pfu_ups(ji,jj,jl) + zlimiter

            ELSEIF( np_limiter == 2 ) THEN
               IF( Rj /= 0. ) THEN
                  IF( pu(ji,jj) > 0. ) THEN   ;   Cr = Rjm / Rj
                  ELSE                        ;   Cr = Rjp / Rj
                  ENDIF
               ELSE
                  Cr = 0.
               ENDIF

               ! -- superbee --
               zpsi = MAX( 0., MAX( MIN(1.,2.*Cr), MIN(2.,Cr) ) )
               ! -- van albada 2 --
               !!zpsi = 2.*Cr / (Cr*Cr+1.)
               ! -- sweby (with beta=1) --
               !!zpsi = MAX( 0., MAX( MIN(1.,1.*Cr), MIN(1.,Cr) ) )
               ! -- van Leer --
               !!zpsi = ( Cr + ABS(Cr) ) / ( 1. + ABS(Cr) )
               ! -- ospre --
               !!zpsi = 1.5 * ( Cr*Cr + Cr ) / ( Cr*Cr + Cr + 1. )
               ! -- koren --
               !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( (1.+2*Cr)/3., 2. ) ) )
               ! -- charm --
               !IF( Cr > 0. ) THEN   ;   zpsi = Cr * (3.*Cr + 1.) / ( (Cr + 1.) * (Cr + 1.) )
               !ELSE                 ;   zpsi = 0.
               !ENDIF
               ! -- van albada 1 --
               !!zpsi = (Cr*Cr + Cr) / (Cr*Cr +1)
               ! -- smart --
               !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, 4. ) ) )
               ! -- umist --
               !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, MIN(0.75+0.25*Cr, 2. ) ) ) )

               ! high order flux corrected by the limiter
               pfu_ho(ji,jj,jl) = pfu_ho(ji,jj,jl) - ABS( pu(ji,jj) ) * ( (1.-zpsi) + uCFL*zpsi ) * Rj * 0.5

            ENDIF
         END_2D
      END DO
      !
   END SUBROUTINE limiter_x


   SUBROUTINE limiter_y( pdt, pv, pt, pfv_ups, pfv_ho )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE limiter_y  ***
      !!
      !! **  Purpose :   compute flux limiter
      !!----------------------------------------------------------------------
      REAL(wp)                   , INTENT(in   ) ::   pdt          ! tracer time-step
      REAL(wp), DIMENSION (:,:  ), INTENT(in   ) ::   pv           ! ice i-velocity => u*e2
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pt           ! ice tracer
      REAL(wp), DIMENSION (:,:,:), INTENT(in   ) ::   pfv_ups      ! upstream flux
      REAL(wp), DIMENSION (:,:,:), INTENT(inout) ::   pfv_ho       ! high order flux
      !
      REAL(wp) ::   Cr, Rjm, Rj, Rjp, vCFL, zpsi, zh3, zlimiter, Rr
      INTEGER  ::   ji, jj, jl    ! dummy loop indices
      REAL(wp), DIMENSION (jpi,jpj) ::   zslpy       ! tracer slopes
      !!----------------------------------------------------------------------
      !

      DO jl = 1, jpl

         DO_2D( 0, 0, 2, 1 )
            zslpy(ji,jj) = ( pt(ji,jj+1,jl) - pt(ji,jj,jl) ) * vmask(ji,jj,1)
         END_2D

         DO_2D( 0, 0, 1, 0 )
            vCFL = pdt * ABS( pv(ji,jj) ) * r1_e1e2t(ji,jj)

            Rjm = zslpy(ji,jj-1)
            Rj  = zslpy(ji,jj  )
            Rjp = zslpy(ji,jj+1)

            IF( np_limiter == 3 ) THEN

               IF( pv(ji,jj) > 0. ) THEN   ;   Rr = Rjm
               ELSE                        ;   Rr = Rjp
               ENDIF

               zh3 = pfv_ho(ji,jj,jl) - pfv_ups(ji,jj,jl)
               IF( Rj > 0. ) THEN
                  zlimiter =  MAX( 0., MIN( zh3, MAX(-Rr * 0.5 * ABS(pv(ji,jj)),  &
                     &        MIN( 2. * Rr * 0.5 * ABS(pv(ji,jj)),  zh3,  1.5 * Rj * 0.5 * ABS(pv(ji,jj)) ) ) ) )
               ELSE
                  zlimiter = -MAX( 0., MIN(-zh3, MAX( Rr * 0.5 * ABS(pv(ji,jj)),  &
                     &        MIN(-2. * Rr * 0.5 * ABS(pv(ji,jj)), -zh3, -1.5 * Rj * 0.5 * ABS(pv(ji,jj)) ) ) ) )
               ENDIF
               pfv_ho(ji,jj,jl) = pfv_ups(ji,jj,jl) + zlimiter

            ELSEIF( np_limiter == 2 ) THEN

               IF( Rj /= 0. ) THEN
                  IF( pv(ji,jj) > 0. ) THEN   ;   Cr = Rjm / Rj
                  ELSE                        ;   Cr = Rjp / Rj
                  ENDIF
               ELSE
                  Cr = 0.
               ENDIF

               ! -- superbee --
               zpsi = MAX( 0., MAX( MIN(1.,2.*Cr), MIN(2.,Cr) ) )
               ! -- van albada 2 --
               !!zpsi = 2.*Cr / (Cr*Cr+1.)
               ! -- sweby (with beta=1) --
               !!zpsi = MAX( 0., MAX( MIN(1.,1.*Cr), MIN(1.,Cr) ) )
               ! -- van Leer --
               !!zpsi = ( Cr + ABS(Cr) ) / ( 1. + ABS(Cr) )
               ! -- ospre --
               !!zpsi = 1.5 * ( Cr*Cr + Cr ) / ( Cr*Cr + Cr + 1. )
               ! -- koren --
               !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( (1.+2*Cr)/3., 2. ) ) )
               ! -- charm --
               !IF( Cr > 0. ) THEN   ;   zpsi = Cr * (3.*Cr + 1.) / ( (Cr + 1.) * (Cr + 1.) )
               !ELSE                 ;   zpsi = 0.
               !ENDIF
               ! -- van albada 1 --
               !!zpsi = (Cr*Cr + Cr) / (Cr*Cr +1)
               ! -- smart --
               !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, 4. ) ) )
               ! -- umist --
               !!zpsi = MAX( 0., MIN( 2.*Cr, MIN( 0.25+0.75*Cr, MIN(0.75+0.25*Cr, 2. ) ) ) )

               ! high order flux corrected by the limiter
               pfv_ho(ji,jj,jl) = pfv_ho(ji,jj,jl) - ABS( pv(ji,jj) ) * ( (1.-zpsi) + vCFL*zpsi ) * Rj * 0.5

            ENDIF
         END_2D
      END DO
      !
   END SUBROUTINE limiter_y


   SUBROUTINE Hbig_umx( pdt, phi_max, phs_max, phip_max, psi_max, pes_max, pei_max, pszi_max, &
      &                  pv_i, pv_s, pa_i, pa_ip, pv_ip, psv_i, pe_s, pe_i, pszv_i )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE Hbig_umx  ***
      !!
      !! ** Purpose : Thickness correction in case advection scheme creates
      !!              abnormally tick ice or snow
      !!
      !! ** Method  : 1- check whether ice thickness is larger than the surrounding 9-points
      !!                 (before advection) and reduce it by adapting ice concentration
      !!              2- check whether snow thickness is larger than the surrounding 9-points
      !!                 (before advection) and reduce it by sending the excess in the ocean
      !!
      !! ** input   : Max thickness of the surrounding 9-points
      !!-------------------------------------------------------------------
      REAL(wp)                              , INTENT(in   ) ::   pdt                                   ! tracer time-step
      REAL(wp), DIMENSION(A2D(0),jpl)       , INTENT(in   ) ::   phi_max, phs_max, phip_max, psi_max   ! max ice thick from surrounding 9-pts
      REAL(wp), DIMENSION(A2D(0),nlay_s,jpl), INTENT(in   ) ::   pes_max
      REAL(wp), DIMENSION(A2D(0),nlay_i,jpl), INTENT(in   ) ::   pei_max, pszi_max
      REAL(wp), DIMENSION(:,:,:)            , INTENT(inout) ::   pv_i, pv_s, pa_i, pa_ip, pv_ip, psv_i
      REAL(wp), DIMENSION(:,:,:,:)          , INTENT(inout) ::   pe_s
      REAL(wp), DIMENSION(:,:,:,:)          , INTENT(inout) ::   pe_i, pszv_i
      !
      INTEGER  ::   ji, jj, jk, jl         ! dummy loop indices
      REAL(wp) ::   z1_dt, zhip, zhi, zhs, zsi, zes, zei, zfra
      !!-------------------------------------------------------------------
      !
      z1_dt = 1._wp / pdt
      !
      DO jl = 1, jpl
         DO_2D( 0, 0, 0, 0 )
            IF ( pv_i(ji,jj,jl) > 0._wp .AND. pa_i(ji,jj,jl) > 0._wp ) THEN
               !
               !                               ! -- check h_ip -- !
               ! if h_ip is larger than the surrounding 9 pts => reduce h_ip and increase a_ip
               IF( ( ln_pnd_LEV .OR. ln_pnd_TOPO ) .AND. pv_ip(ji,jj,jl) > 0._wp ) THEN
                  zhip = pv_ip(ji,jj,jl) / MAX( epsi20, pa_ip(ji,jj,jl) )
                  IF( zhip > phip_max(ji,jj,jl) .AND. pa_ip(ji,jj,jl) < 0.15 ) THEN
                     pa_ip(ji,jj,jl) = pv_ip(ji,jj,jl) / phip_max(ji,jj,jl)
                  ENDIF
               ENDIF
               !
               !                               ! -- check h_i -- !
               ! if h_i is larger than the surrounding 9 pts => reduce h_i and increase a_i
               zhi = pv_i(ji,jj,jl) / pa_i(ji,jj,jl)
               IF( zhi > phi_max(ji,jj,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                  pa_i(ji,jj,jl) = pv_i(ji,jj,jl) / MIN( phi_max(ji,jj,jl), hi_max(jpl) )   !-- bound h_i to hi_max (99 m)
               ENDIF
               !
               !                               ! -- check h_s -- !
               ! if h_s is larger than the surrounding 9 pts => put the snow excess in the ocean
               zhs = pv_s(ji,jj,jl) / pa_i(ji,jj,jl)
               IF( pv_s(ji,jj,jl) > 0._wp .AND. zhs > phs_max(ji,jj,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                  zfra = phs_max(ji,jj,jl) / MAX( zhs, epsi20 )
                  !
                  wfx_res(ji,jj) = wfx_res(ji,jj) + ( pv_s(ji,jj,jl) - pa_i(ji,jj,jl) * phs_max(ji,jj,jl) ) * rhos * z1_dt
                  hfx_res(ji,jj) = hfx_res(ji,jj) - SUM( pe_s(ji,jj,1:nlay_s,jl) ) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                  !
                  pe_s(ji,jj,1:nlay_s,jl) = pe_s(ji,jj,1:nlay_s,jl) * zfra
                  pv_s(ji,jj,jl)          = pa_i(ji,jj,jl) * phs_max(ji,jj,jl)
               ENDIF
               !
            ENDIF
         END_2D
      END DO
      !
      !                                          ! -- check s_i -- !
      IF( nn_icesal == 4 ) THEN
         DO jl = 1, jpl
            DO_3D( 0, 0, 0, 0, 1, nlay_i )
               IF ( pv_i(ji,jj,jl) > 0._wp .AND. pa_i(ji,jj,jl) > 0._wp ) THEN
                  ! if szv_i/v_i is larger than the surrounding 9 pts => put the salt excess in the ocean
                  zsi = pszv_i(ji,jj,jk,jl) / pv_i(ji,jj,jl)
                  IF( zsi > pszi_max(ji,jj,jk,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                     zfra = pszi_max(ji,jj,jk,jl) / zsi
                     sfx_res(ji,jj) = sfx_res(ji,jj) + pszv_i(ji,jj,jk,jl) * ( 1._wp - zfra ) * rhoi * z1_dt
                     pszv_i(ji,jj,jk,jl) = pszv_i(ji,jj,jk,jl) * zfra
                  ENDIF
               ENDIF
            END_3D
         END DO
      ELSE
         DO jl = 1, jpl
            DO_2D( 0, 0, 0, 0 )
               IF ( pv_i(ji,jj,jl) > 0._wp .AND. pa_i(ji,jj,jl) > 0._wp ) THEN
                  ! if s_i is larger than the surrounding 9 pts => put salt excess in the ocean
                  zsi = psv_i(ji,jj,jl) / pv_i(ji,jj,jl)
                  IF( zsi > psi_max(ji,jj,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                     zfra = psi_max(ji,jj,jl) / zsi
                     sfx_res(ji,jj) = sfx_res(ji,jj) + psv_i(ji,jj,jl) * ( 1._wp - zfra ) * rhoi * z1_dt
                     psv_i(ji,jj,jl) = psv_i(ji,jj,jl) * zfra
                  ENDIF
                  !
               ENDIF
            END_2D
         END DO
      ENDIF
      !
      !                                           ! -- check e_i/v_i -- !
      DO jl = 1, jpl
         DO_3D( 0, 0, 0, 0, 1, nlay_i )
            IF ( pv_i(ji,jj,jl) > 0._wp .AND. pa_i(ji,jj,jl) > 0._wp ) THEN
               ! if e_i/v_i is larger than the surrounding 9 pts => put the heat excess in the ocean
               zei = pe_i(ji,jj,jk,jl) / pv_i(ji,jj,jl)
               IF( zei > pei_max(ji,jj,jk,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                  zfra = pei_max(ji,jj,jk,jl) / zei
                  hfx_res(ji,jj) = hfx_res(ji,jj) - pe_i(ji,jj,jk,jl) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                  pe_i(ji,jj,jk,jl) = pe_i(ji,jj,jk,jl) * zfra
               ENDIF
            ENDIF
         END_3D
      END DO
      !                                           ! -- check e_s/v_s -- !
      DO jl = 1, jpl
         DO_3D( 0, 0, 0, 0, 1, nlay_s )
            IF ( pv_s(ji,jj,jl) > 0._wp .AND. pa_i(ji,jj,jl) > 0._wp ) THEN
               ! if e_s/v_s is larger than the surrounding 9 pts => put the heat excess in the ocean
               zes = pe_s(ji,jj,jk,jl) / pv_s(ji,jj,jl)
               IF( zes > pes_max(ji,jj,jk,jl) .AND. pa_i(ji,jj,jl) < 0.15 ) THEN
                  zfra = pes_max(ji,jj,jk,jl) / zes
                  hfx_res(ji,jj) = hfx_res(ji,jj) - pe_s(ji,jj,jk,jl) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                  pe_s(ji,jj,jk,jl) = pe_s(ji,jj,jk,jl) * zfra
               ENDIF
            ENDIF
         END_3D
      END DO
      !
   END SUBROUTINE Hbig_umx


   SUBROUTINE Hsnow_umx( pdt, pv_i, pv_s, pa_i, pa_ip, pe_s )
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE Hsnow_umx  ***
      !!
      !! ** Purpose : 1- Check snow load after advection
      !!              2- Correct pond concentration to avoid a_ip > a_i
      !!
      !! ** Method :  If snow load makes snow-ice interface to deplet below the ocean surface
      !!              then put the snow excess in the ocean
      !!
      !! ** Notes :   This correction is crucial because of the call to routine icecor afterwards
      !!              which imposes a mini of ice thick. (rn_himin). This imposed mini can artificially
      !!              make the snow very thick (if concentration decreases drastically)
      !!              This behavior has been seen in Ultimate-Macho and supposedly it can also be true for Prather
      !!-------------------------------------------------------------------
      REAL(wp)                    , INTENT(in   ) ::   pdt   ! tracer time-step
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   pv_i, pv_s, pa_i, pa_ip
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pe_s
      !
      INTEGER  ::   ji, jj, jl   ! dummy loop indices
      REAL(wp) ::   z1_dt, zvs_excess, zfra
      !!-------------------------------------------------------------------
      !
      z1_dt = 1._wp / pdt
      !
      ! -- check snow load -- !
      DO jl = 1, jpl
         DO_2D( 0, 0, 0, 0 )
            IF ( pv_i(ji,jj,jl) > 0._wp ) THEN
               !
               zvs_excess = MAX( 0._wp, pv_s(ji,jj,jl) - pv_i(ji,jj,jl) * (rho0-rhoi) * r1_rhos )
               !
               IF( zvs_excess > 0._wp ) THEN   ! snow-ice interface deplets below the ocean surface
                  ! put snow excess in the ocean
                  zfra = ( pv_s(ji,jj,jl) - zvs_excess ) / MAX( pv_s(ji,jj,jl), epsi20 )
                  wfx_res(ji,jj) = wfx_res(ji,jj) + zvs_excess * rhos * z1_dt
                  hfx_res(ji,jj) = hfx_res(ji,jj) - SUM( pe_s(ji,jj,1:nlay_s,jl) ) * ( 1._wp - zfra ) * z1_dt ! W.m-2 <0
                  ! correct snow volume and heat content
                  pe_s(ji,jj,1:nlay_s,jl) = pe_s(ji,jj,1:nlay_s,jl) * zfra
                  pv_s(ji,jj,jl)          = pv_s(ji,jj,jl) - zvs_excess
               ENDIF
               !
            ENDIF
         END_2D
      END DO
      !
      !-- correct pond concentration to avoid a_ip > a_i -- !
      WHERE( pa_ip(:,:,:) > pa_i(:,:,:) )   pa_ip(:,:,:) = pa_i(:,:,:)
      !
   END SUBROUTINE Hsnow_umx

   SUBROUTINE icemax3D_umx( pice , pmax )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE icemax3D_umx ***
      !! ** Purpose :  compute the max of the 9 points around
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:)     , INTENT(in ) ::   pice   ! input
      REAL(wp), DIMENSION(A2D(0),jpl), INTENT(out) ::   pmax   ! output
      !
      REAL(wp), DIMENSION(Nis0:Nie0) ::   zmax1, zmax2
      REAL(wp)                       ::   zmax3
      INTEGER  ::   ji, jj, jl   ! dummy loop indices
      !!----------------------------------------------------------------------
      ! basic version: get the max of epsi20 + 9 neighbours
!!$      DO jl = 1, jpl
!!$         DO_2D( 0, 0, 0, 0 )
!!$            pmax(ji,jj,jl) = MAX( epsi20, pice(ji-1,jj-1,jl), pice(ji,jj-1,jl), pice(ji+1,jj-1,jl),   &
!!$               &                          pice(ji-1,jj  ,jl), pice(ji,jj  ,jl), pice(ji+1,jj  ,jl),   &
!!$               &                          pice(ji-1,jj+1,jl), pice(ji,jj+1,jl), pice(ji+1,jj+1,jl) )
!!$         END_2D
!!$      END DO
      ! optimized version : does a little bit more than 2 max of epsi20 + 3 neighbours
      DO jl = 1, jpl
         DO ji = Nis0, Nie0
            zmax1(ji) = MAX( epsi20, pice(ji,Njs0-1,jl), pice(ji-1,Njs0-1,jl), pice(ji+1,Njs0-1,jl) )
            zmax2(ji) = MAX( epsi20, pice(ji,Njs0  ,jl), pice(ji-1,Njs0  ,jl), pice(ji+1,Njs0  ,jl) )
         END DO
         DO_2D( 0, 0, 0, 0 )
            zmax3 = MAX( epsi20, pice(ji,jj+1,jl), pice(ji-1,jj+1,jl), pice(ji+1,jj+1,jl) )
            pmax(ji,jj,jl) = MAX( epsi20, zmax1(ji), zmax2(ji), zmax3 )
            zmax1(ji) = zmax2(ji)
            zmax2(ji) = zmax3
         END_2D
      END DO
   END SUBROUTINE icemax3D_umx

   SUBROUTINE icemax4D_umx( pice , pmax )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE icemax4D_umx ***
      !! ** Purpose :  compute the max of the 9 points around
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in ) ::   pice   ! input
      REAL(wp), DIMENSION(:,:,:,:), INTENT(out) ::   pmax   ! output
      !
      REAL(wp), DIMENSION(Nis0:Nie0) ::   zmax1, zmax2
      REAL(wp)                       ::   zmax3
      INTEGER  ::   ihls, jlay, ji, jj, jk, jl   ! dummy loop indices
      !!----------------------------------------------------------------------
      jlay = SIZE( pice , 3 )   ! size of input arrays

      ! pmax is A2D(0), so it needs to be shifted by nn_hls in the loops below
      IF( SIZE( pmax , 1 ) == jpi ) THEN ; ihls = 0
      ELSE                               ; ihls = nn_hls
      ENDIF
      
      ! basic version: get the max of epsi20 + 9 neighbours
!!$      DO jl = 1, jpl
!!$         DO jk = 1, jlay
!!$            DO_2D( 0, 0, 0, 0 )
!!$               pmax(ji,jj,jk,jl) = MAX( epsi20, pice(ji-1,jj-1,jk,jl), pice(ji,jj-1,jk,jl), pice(ji+1,jj-1,jk,jl),   &
!!$                  &                             pice(ji-1,jj  ,jk,jl), pice(ji,jj  ,jk,jl), pice(ji+1,jj  ,jk,jl),   &
!!$                  &                             pice(ji-1,jj+1,jk,jl), pice(ji,jj+1,jk,jl), pice(ji+1,jj+1,jk,jl) )
!!$            END_2D
!!$         END DO
!!$      END DO
      ! optimized version : does a little bit more than 2 max of epsi20 + 3 neighbours
      DO jl = 1, jpl
         DO jk = 1, jlay
            DO ji = Nis0, Nie0
               zmax1(ji) = MAX( epsi20, pice(ji,Njs0-1,jk,jl), pice(ji-1,Njs0-1,jk,jl), pice(ji+1,Njs0-1,jk,jl) )
               zmax2(ji) = MAX( epsi20, pice(ji,Njs0  ,jk,jl), pice(ji-1,Njs0  ,jk,jl), pice(ji+1,Njs0  ,jk,jl) )
            END DO
            DO_2D( 0, 0, 0, 0 )
               zmax3 = MAX( epsi20, pice(ji,jj+1,jk,jl), pice(ji-1,jj+1,jk,jl), pice(ji+1,jj+1,jk,jl) )
               pmax(ji-ihls,jj-ihls,jk,jl) = MAX( epsi20, zmax1(ji), zmax2(ji), zmax3 )
               zmax1(ji) = zmax2(ji)
               zmax2(ji) = zmax3
            END_2D
         END DO
      END DO
   END SUBROUTINE icemax4D_umx

#else
   !!----------------------------------------------------------------------
   !!   Default option           Dummy module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icedyn_adv_umx
