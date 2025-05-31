MODULE icetab
   !!======================================================================
   !!                       ***  MODULE icetab   ***
   !!   sea-ice : transform 1D (2D) array to a 2D (1D) table
   !!======================================================================
   !! History :  4.0  !  2018     (C. Rousset)       Original code SI3
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   tab_4d_3d  : 4-D <==> 3-D
   !!   tab_3d_2d  : 3-D <==> 2-D
   !!   tab_2d_3d  : 2-D <==> 3-D
   !!   tab_2d_1d  : 2-D <==> 1-D
   !!   tab_1d_2d  : 1-D <==> 2-D
   !!----------------------------------------------------------------------
   USE par_kind , ONLY :   wp
   USE par_oce  , ONLY :   jpi, jpj, nn_hls
   USE par_ice  , ONLY :   jpl

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tab_4d_3d
   PUBLIC   tab_3d_2d
   PUBLIC   tab_2d_1d
   PUBLIC   tab_3d_4d
   PUBLIC   tab_2d_3d
   PUBLIC   tab_1d_2d

   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tab_4d_3d( ndim1d, tab_ind, tab3d, tab4d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_4d_3d  ***
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   ndim1d   ! 1d size
      INTEGER , DIMENSION(ndim1d) , INTENT(in   ) ::   tab_ind  ! input index
      REAL(wp), DIMENSION(:,:,:,:), INTENT(in   ) ::   tab4d    ! input 4D field
      REAL(wp), DIMENSION(:,:,:)  , INTENT(inout) ::   tab3d    ! output 3D field
      !
      INTEGER ::   ipi, ipj, ipk, ji0, jj0, jk, jl, jn, jid, jjd
      !!----------------------------------------------------------------------
      ipi = SIZE(tab4d,1)   ! 1st dimension
      ipj = SIZE(tab4d,2)   ! 2nd dimension
      ipk = SIZE(tab4d,3)   ! 3d  dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! full arrays then no need to change index jid and jjd
         ji0 = 0 ; jj0 = 0
      ELSE                                     ! reduced arrays then need to shift index by nn_hls
         ji0 = nn_hls ; jj0 = nn_hls           !         since tab2d is shifted by nn_hls
      ENDIF                                    !           (i.e. from hls+1:jpi-hls  to  1:jpi-2*hls)
      !
      DO jl = 1, jpl
         DO jk = 1, ipk
            DO jn = 1, ndim1d
               jid          = MOD( tab_ind(jn) - 1 , jpi ) + 1 - ji0
               jjd          =    ( tab_ind(jn) - 1 ) / jpi + 1 - jj0
               tab3d(jn,jk,jl) = tab4d(jid,jjd,jk,jl)
            END DO
         END DO
      END DO
      !
   END SUBROUTINE tab_4d_3d

   SUBROUTINE tab_3d_2d( ndim1d, tab_ind, tab2d, tab3d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_3d_2d  ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   ndim1d   ! 1d size
      INTEGER , DIMENSION(ndim1d)     , INTENT(in   ) ::   tab_ind  ! input index
      REAL(wp), DIMENSION(:,:,:)      , INTENT(in   ) ::   tab3d    ! input 3D field
      REAL(wp), DIMENSION(ndim1d,jpl) , INTENT(inout) ::   tab2d    ! output 2D field
      !
      INTEGER ::   ipi, ipj, ji0, jj0, jl, jn, jid, jjd
      !!----------------------------------------------------------------------
      ipi = SIZE(tab3d,1)   ! 1st dimension
      ipj = SIZE(tab3d,2)   ! 2nd dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! full arrays then no need to change index jid and jjd
         ji0 = 0 ; jj0 = 0
      ELSE                                     ! reduced arrays then need to shift index by nn_hls
         ji0 = nn_hls ; jj0 = nn_hls           !         since tab2d is shifted by nn_hls
      ENDIF                                    !           (i.e. from hls+1:jpi-hls  to  1:jpi-2*hls)
      !
      DO jl = 1, jpl
         DO jn = 1, ndim1d
            jid          = MOD( tab_ind(jn) - 1 , jpi ) + 1 - ji0
            jjd          =    ( tab_ind(jn) - 1 ) / jpi + 1 - jj0
            tab2d(jn,jl) = tab3d(jid,jjd,jl)
         END DO
      END DO
   END SUBROUTINE tab_3d_2d


   SUBROUTINE tab_2d_1d( ndim1d, tab_ind, tab1d, tab2d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_2d_1d  ***
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   ndim1d   ! 1d size
      INTEGER , DIMENSION(ndim1d) , INTENT(in   ) ::   tab_ind  ! input index
      REAL(wp), DIMENSION(:,:)    , INTENT(in   ) ::   tab2d    ! input 2D field
      REAL(wp), DIMENSION(ndim1d) , INTENT(inout) ::   tab1d    ! output 1D field
      !
      INTEGER ::   ipi, ipj, ji0, jj0, jn, jid, jjd
      !!----------------------------------------------------------------------
      ipi = SIZE(tab2d,1)   ! 1st dimension
      ipj = SIZE(tab2d,2)   ! 2nd dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! full arrays then no need to change index jid and jjd
         ji0 = 0 ; jj0 = 0
      ELSE                                     ! reduced arrays then need to shift index by nn_hls
         ji0 = nn_hls ; jj0 = nn_hls           !         since tab2d is shifted by nn_hls
      ENDIF                                    !           (i.e. from hls+1:jpi-hls  to  1:jpi-2*hls)
      !
      DO jn = 1, ndim1d
         jid       = MOD( tab_ind(jn) - 1 , jpi ) + 1 - ji0
         jjd       =    ( tab_ind(jn) - 1 ) / jpi + 1 - jj0
         tab1d(jn) = tab2d(jid,jjd)
      END DO
   END SUBROUTINE tab_2d_1d

   SUBROUTINE tab_3d_4d( ndim1d, tab_ind, tab3d, tab4d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_3d_4d  ***
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   ndim1d    ! 1D size
      INTEGER , DIMENSION(ndim1d) , INTENT(in   ) ::   tab_ind   ! input index
      REAL(wp), DIMENSION(:,:,:)  , INTENT(in   ) ::   tab3d     ! input 3D field
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   tab4d     ! output 4D field
      !
      INTEGER ::   ipi, ipj, ipk, ji0, jj0, jk, jl, jn, jid, jjd
      !!----------------------------------------------------------------------
      ipi = SIZE(tab4d,1)   ! 1st dimension
      ipj = SIZE(tab4d,2)   ! 2nd dimension
      ipk = SIZE(tab4d,3)   ! 3d  dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! full arrays then no need to change index jid and jjd
         ji0 = 0 ; jj0 = 0
      ELSE                                     ! reduced arrays then need to shift index by nn_hls
         ji0 = nn_hls ; jj0 = nn_hls           !         since tab2d is shifted by nn_hls
      ENDIF                                    !           (i.e. from hls+1:jpi-hls  to  1:jpi-2*hls)
      !
      DO jl = 1, jpl
         DO jk = 1, ipk
            DO jn = 1, ndim1d
               jid               = MOD( tab_ind(jn) - 1 ,  jpi ) + 1 - ji0
               jjd               =    ( tab_ind(jn) - 1 ) / jpi  + 1 - jj0
               tab4d(jid,jjd,jk,jl) = tab3d(jn,jk,jl)
            END DO
         END DO
      END DO
      !
   END SUBROUTINE tab_3d_4d

   SUBROUTINE tab_2d_3d( ndim1d, tab_ind, tab2d, tab3d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_2d_3d  ***
      !!----------------------------------------------------------------------
      INTEGER                         , INTENT(in   ) ::   ndim1d    ! 1D size
      INTEGER , DIMENSION(ndim1d)     , INTENT(in   ) ::   tab_ind   ! input index
      REAL(wp), DIMENSION(ndim1d,jpl) , INTENT(in   ) ::   tab2d     ! input 2D field
      REAL(wp), DIMENSION(:,:,:)      , INTENT(inout) ::   tab3d     ! output 3D field
      !
      INTEGER ::   ipi, ipj, ji0, jj0, jl, jn, jid, jjd
      !!----------------------------------------------------------------------
      ipi = SIZE(tab3d,1)   ! 1st dimension
      ipj = SIZE(tab3d,2)   ! 2nd dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! full arrays then no need to change index jid and jjd
         ji0 = 0 ; jj0 = 0
      ELSE                                     ! reduced arrays then need to shift index by nn_hls
         ji0 = nn_hls ; jj0 = nn_hls           !         since tab2d is shifted by nn_hls
      ENDIF                                    !           (i.e. from hls+1:jpi-hls  to  1:jpi-2*hls)
      !
      DO jl = 1, jpl
         DO jn = 1, ndim1d
            jid               = MOD( tab_ind(jn) - 1 ,  jpi ) + 1 - ji0
            jjd               =    ( tab_ind(jn) - 1 ) / jpi  + 1 - jj0
            tab3d(jid,jjd,jl) = tab2d(jn,jl)
         END DO
      END DO
   END SUBROUTINE tab_2d_3d


   SUBROUTINE tab_1d_2d( ndim1d, tab_ind, tab1d, tab2d )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tab_1d_2d  ***
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   ndim1d    ! 1D size
      INTEGER , DIMENSION(ndim1d) , INTENT(in   ) ::   tab_ind   ! input index
      REAL(wp), DIMENSION(ndim1d) , INTENT(in   ) ::   tab1d     ! input 1D field
      REAL(wp), DIMENSION(:,:)    , INTENT(inout) ::   tab2d     ! output 2D field
      !
      INTEGER ::   ipi, ipj, ji0, jj0, jn, jid, jjd
      !!----------------------------------------------------------------------
      ipi = SIZE(tab2d,1)   ! 1st dimension
      ipj = SIZE(tab2d,2)   ! 2nd dimension
      !
      IF( ipi == jpi .AND. ipj == jpj ) THEN   ! full arrays then no need to change index jid and jjd
         ji0 = 0 ; jj0 = 0
      ELSE                                     ! reduced arrays then need to shift index by nn_hls
         ji0 = nn_hls ; jj0 = nn_hls           !         since tab2d is shifted by nn_hls
      ENDIF                                    !           (i.e. from hls+1:jpi-hls  to  1:jpi-2*hls)
      !
      DO jn = 1, ndim1d
         jid            = MOD( tab_ind(jn) - 1 ,  jpi ) + 1 - ji0
         jjd            =    ( tab_ind(jn) - 1 ) / jpi  + 1 - jj0
         tab2d(jid,jjd) = tab1d(jn)
      END DO
   END SUBROUTINE tab_1d_2d

#else
   !!----------------------------------------------------------------------
   !!   Default option           Dummy module         NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icetab
