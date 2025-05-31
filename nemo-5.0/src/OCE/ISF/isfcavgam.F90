MODULE isfcavgam
   !!======================================================================
   !!                       ***  MODULE  isfgammats  ***
   !! Ice shelf gamma module :  compute exchange coeficient at the ice/ocean interface
   !!======================================================================
   !! History :  4.1  !  (P. Mathiot) original
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   isfcav_gammats       : compute exchange coeficient gamma 
   !!----------------------------------------------------------------------
   USE isf_oce
   USE isfutils, ONLY: debug

   USE oce     , ONLY: uu, vv              ! ocean dynamics
   USE phycst  , ONLY: grav, vkarmn        ! physical constant
   USE eosbn2  , ONLY: eos_rab             ! equation of state
   USE lib_mpp , ONLY: ctl_stop

   USE par_oce        ! ocean space and time domain
   USE dom_oce        ! ocean space and time domain
   USE in_out_manager ! I/O manager
   USE lib_fortran    ! to use sign with key_nosignedzero
  !
   IMPLICIT NONE
   !
   PRIVATE
   !
   PUBLIC   isfcav_gammats

   !! * Substitutions   
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   !
   SUBROUTINE isfcav_gammats( Kmm, l_converged, pustar, pttbl, pstbl, pqoce, pfwf, pRc, pgt, pgs )
      !!----------------------------------------------------------------------
      !! ** Purpose    : compute the coefficient echange for heat and fwf flux
      !!
      !! ** Method     : select the gamma formulation
      !!                 3 method available (cst, vel and vel_stab)
      !!---------------------------------------------------------------------
      INTEGER                    , INTENT(in   ) ::   Kmm             ! ocean time level index
      LOGICAL , DIMENSION(A2D(0)), INTENT(in   ) ::   l_converged     ! true when melting converges (per grid point)
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pustar          ! u*
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pttbl, pstbl    ! top boundary layer tracer
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pqoce, pfwf     ! isf heat and fwf
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pRc             ! Richardson number
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pgt, pgs        ! gamma t and gamma s 
      !!---------------------------------------------------------------------
      !
      ! --- compute gamma --- !
      !
      SELECT CASE( cn_gammablk )
      CASE( 'spe'  )       ! gamma is constant (specified in namelist)
         !
         pgt(:,:) = rn_gammat0
         pgs(:,:) = rn_gammas0
         !
      CASE( 'vel' )        ! gamma is proportional to u*
         !
         CALL gammats_vel( pustar, pgt, pgs )
         !
      CASE( 'vel_stab' )   ! gamma depends of stability of boundary layer and u*
         !
         CALL gammats_vel_stab( Kmm, l_converged, pustar, pttbl, pstbl, pqoce, pfwf, pRc, pgt, pgs )
         !
      CASE DEFAULT
         CALL ctl_stop('STOP','method to compute gamma (cn_gammablk) is unknown (should not see this)')
      END SELECT
      !
   END SUBROUTINE isfcav_gammats
   !
   SUBROUTINE gammats_vel( pustar,  &   ! <<== in
      &                    pgt, pgs )   ! ==>> out gammats [m/s]
      !!----------------------------------------------------------------------
      !! ** Purpose    : compute the coefficient echange coefficient 
      !!
      !! ** Method     : gamma is velocity dependent ( gt= gt0 * Ustar )
      !!
      !! ** Reference  : Asay-Davis et al., Geosci. Model Dev., 9, 2471-2497, 2016
      !!---------------------------------------------------------------------
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pustar    ! u*
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pgt, pgs  ! gammat and gammas [m/s]
      !!
      INTEGER ::   ji, jj                     ! loop index
      !!---------------------------------------------------------------------
      !
      DO_2D( 0, 0, 0, 0 )
         pgt(ji,jj) = pustar(ji,jj) * rn_gammat0
         pgs(ji,jj) = pustar(ji,jj) * rn_gammas0
      END_2D
      !
      !
   END SUBROUTINE gammats_vel

   SUBROUTINE gammats_vel_stab( Kmm, l_converged, pustar, pttbl, pstbl, pqoce, pfwf, pRc, &  ! <<== in
      &                                                                        pgt , pgs  )  ! ==>> out gammats [m/s]
      !!----------------------------------------------------------------------
      !! ** Purpose    : compute the coefficient echange coefficient 
      !!
      !! ** Method     : gamma is velocity dependent and stability dependent
      !!
      !! ** Reference  : Holland and Jenkins, 1999, JPO, p1787-1800
      !!---------------------------------------------------------------------
      INTEGER                    , INTENT(in   ) ::   Kmm            ! ocean time level index
      LOGICAL , DIMENSION(A2D(0)), INTENT(in   ) ::   l_converged    ! true when melting converges (per grid point)
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pustar         ! u*
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pttbl, pstbl   ! tracer in the losch top boundary layer
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pqoce, pfwf    ! surface heat flux and fwf flux
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pRc            ! Richardson number
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pgt, pgs       ! gammat and gammas
      !!
      INTEGER  ::   ji, jj                     ! loop index
      INTEGER  ::   ikt                        ! local integer
      REAL(wp) ::   zmob, zmols                ! Monin Obukov length, coriolis factor at T point
      REAL(wp) ::   zbuofdep, zhnu             ! Bouyancy length scale, sublayer tickness
      REAL(wp) ::   zhmax                      ! limitation of mol
      REAL(wp) ::   zetastar                   ! stability parameter
      REAL(wp) ::   zgmolet, zgmoles, zgturb   ! contribution of modelecular sublayer and turbulence 
      REAL(wp) ::   zdep
      REAL(wp), PARAMETER ::   zxsiN =    0.052_wp   ! dimensionless constant
      REAL(wp), PARAMETER ::   znu   =    1.95e-6_wp ! kinamatic viscosity of sea water (m2.s-1)
      REAL(wp), PARAMETER ::   zPr   =   13.8_wp
      REAL(wp), PARAMETER ::   zSc   = 2432.0_wp
      REAL(wp), DIMENSION(jpts) ::   zts, zab
      !!---------------------------------------------------------------------
      !
      ! compute gamma mole (eq ??)
      zgmolet = 12.5_wp * zPr ** (2.0/3.0) - 6.0_wp
      zgmoles = 12.5_wp * zSc ** (2.0/3.0) - 6.0_wp
      !
      ! compute gamma
      DO_2D( 0, 0, 0, 0 )
         !
         IF ( .NOT. l_converged(ji,jj) ) THEN
            !
            ikt = mikt(ji,jj)
            !
            IF( pustar(ji,jj) == 0._wp ) THEN           ! only for kt = 1 I think
               pgt(ji,jj) = rn_gammat0
               pgs(ji,jj) = rn_gammas0
            ELSE
               ! compute bouyancy 
               zts(jp_tem) = pttbl(ji,jj)
               zts(jp_sal) = pstbl(ji,jj)
               zdep        = gdepw(ji,jj,ikt,Kmm)
               !
               CALL eos_rab( zts, zdep, zab, Kmm )
               !
               ! compute length scale (Eq ??)
               zbuofdep = grav * ( zab(jp_tem) * pqoce(ji,jj) - zab(jp_sal) * pfwf(ji,jj) )
               !
               ! compute Monin Obukov Length
               ! Maximum boundary layer depth (Eq ??)
               zhmax = gdept(ji,jj,mbkt(ji,jj),Kmm) - gdepw(ji,jj,ikt,Kmm) -0.001_wp
               !
               ! Compute Monin obukhov length scale at the surface and Ekman depth: (Eq ??)
               zmob   = pustar(ji,jj) ** 3 / (vkarmn * (zbuofdep + 1.e-20_wp ))
               zmols  = SIGN(1._wp, zmob) * MIN(ABS(zmob), zhmax) * tmask(ji,jj,ikt)
               !
               ! compute eta* (stability parameter) (Eq ??)
               zetastar = 1._wp / ( SQRT(1._wp + MAX( 0._wp, zxsiN * pustar(ji,jj) &
                  &                                        / MAX( 1.e-20, ABS(ff_t(ji,jj)) * zmols * pRc(ji,jj) ) )))
               !
               ! compute the sublayer thickness (Eq ??)
               zhnu = 5 * znu / MAX( 1.e-20, pustar(ji,jj) )
               !
               ! compute gamma turb (Eq ??)
               zgturb = 1._wp / vkarmn * LOG(pustar(ji,jj) * zxsiN * zetastar * zetastar / MAX( 1.e-10_wp, ABS(ff_t(ji,jj)) * zhnu )) &
                  &   + 1._wp / ( 2 * zxsiN * zetastar ) - 1._wp / vkarmn
               !
               ! compute gammats
               pgt(ji,jj) = pustar(ji,jj) / (zgturb + zgmolet)
               pgs(ji,jj) = pustar(ji,jj) / (zgturb + zgmoles)
            ENDIF
            !  
         ENDIF
         !
      END_2D

   END SUBROUTINE gammats_vel_stab

END MODULE isfcavgam
