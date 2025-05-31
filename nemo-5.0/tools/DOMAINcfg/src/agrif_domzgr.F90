MODULE agrif_domzgr

   USE agrif_profiles
   USE dom_oce

   IMPLICIT NONE
   PRIVATE

   REAL, ALLOCATABLE, SAVE , DIMENSION(:,:) :: ht0_parent
  
   PUBLIC :: agrif_create_bathy_meter
 

CONTAINS

#if defined key_agrif

   SUBROUTINE agrif_create_bathy_meter
  
      ALLOCATE(ht0_parent(jpi,jpj))
      CALL Agrif_Init_variable(ht0_id, procname = init_ht0)

      IF( ln_lin_int) THEN
        CALL Agrif_Init_variable(bathy_id, procname = init_bathy)
        IF( Agrif_Root() ) RETURN
         WHERE(ht0_parent(:,:) == 0._wp) bathy(:,:) = ht0_parent(:,:)
      ELSE
         bathy(:,:) = ht0_parent(:,:)
      ENDIF

      DEALLOCATE(ht0_parent)
 
   END SUBROUTINE agrif_create_bathy_meter

   SUBROUTINE init_bathy( ptab, i1, i2, j1, j2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      !!----------------------------------------------------------------------  
      INTEGER :: ji,jj

      IF( before) THEN
         ptab(i1:i2,j1:j2) = bathy(i1:i2,j1:j2)
         DO jj=j1,j2
            DO ji=i1,i2
               ptab(ji,jj) = SUM( e3t_0(ji,jj, 1:mbkt(ji,jj) ) ) * ssmask(ji,jj)
            END DO
         END DO
      ELSE
         bathy(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_bathy

   SUBROUTINE init_ht0( ptab, i1, i2, j1, j2, before, nb, ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpht0  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2)    , INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      !!----------------------------------------------------------------------  
      INTEGER                                         ::   ji, jj

      IF( before) THEN
         ptab(i1:i2,j1:j2) = bathy(i1:i2,j1:j2)
         DO jj=j1,j2
            DO ji=i1,i2
               ptab(ji,jj) = SUM( e3t_0(ji,jj, 1:mbkt(ji,jj) ) ) * ssmask(ji,jj)
            END DO
         END DO
      ELSE
         ht0_parent(i1:i2,j1:j2) = ptab(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE init_ht0

#else
   SUBROUTINE agrif_create_bathy_meter
   END SUBROUTINE agrif_create_bathy_meter
#endif
END MODULE agrif_domzgr
