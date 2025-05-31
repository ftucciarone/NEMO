MODULE trcwri
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    TOP :   Output of passive tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!----------------------------------------------------------------------
#if defined key_top && defined key_xios
   !!----------------------------------------------------------------------
   !!   'key_top'                                           TOP models
   !!----------------------------------------------------------------------
   !! trc_wri_trc   :  outputs of concentration fields
   !!----------------------------------------------------------------------
   USE dom_oce     ! ocean space and time domain variables
   USE oce_trc     ! shared variables between ocean and passive tracers
   USE trc         ! passive tracers common variables 
   USE iom         ! I/O manager
   USE dianam      ! Output file name
   USE trcwri_pisces
   USE trcwri_cfc
   USE trcwri_c14
   USE trcwri_age
   USE trcwri_my_trc

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri      

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"

CONTAINS

   SUBROUTINE trc_wri( kt, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri  ***
      !! 
      !! ** Purpose :   output passive tracers fields and dynamical trends
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in )     :: kt
      INTEGER, INTENT( in )     :: Kmm  ! time level indices
      !
      INTEGER                   :: ji,jj,jk,jn
      CHARACTER (len=20)        :: cltra
      CHARACTER (len=40)        :: clhstnam
      INTEGER ::   inum = 11            ! temporary logical unit
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   z3d, z3d0   ! 3D workspace
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_wri')
      !
      IF( l_offline ) THEN    ! WRITE root name in date.file for use by postpro
         IF(  kt == nittrc000 .AND. lwp ) THEN    ! WRITE root name in date.file for use by postpro
           CALL dia_nam( clhstnam, nn_writetrc,' ' )
           CALL ctl_opn( inum, 'date.file', 'REPLACE', 'FORMATTED', 'SEQUENTIAL', -1, numout, lwp, narea )
           WRITE(inum,*) clhstnam
           CLOSE(inum)
         ENDIF

         ALLOCATE( z3d(jpi,jpj,jpk) )  ;   z3d(:,:,:) = 0._wp
         ALLOCATE( z3d0(A2D(0),jpk) )  ;  z3d0(:,:,:) = 0._wp

         ! Output of initial vertical scale factor
         IF( lk_vco_1d ) THEN
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d(ji,jj,jk) =  e3t_0(ji,jj,jk)
            END_3D
            CALL iom_put( "e3t_0", z3d )
            !
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d(ji,jj,jk) =  e3u_0(ji,jj,jk)
            END_3D
            CALL iom_put( "e3u_0", z3d )
            !
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d(ji,jj,jk) =  e3v_0(ji,jj,jk)
            END_3D
            CALL iom_put( "e3v_0", z3d )
         ELSE
            CALL iom_put( "e3t_0", e3t_0(:,:,:) )
            CALL iom_put( "e3u_0", e3u_0(:,:,:) )
            CALL iom_put( "e3v_0", e3v_0(:,:,:) )
         ENDIF
         !
         IF( .NOT.lk_linssh )  CALL iom_put( "ssh" , ssh(:,:,Kmm) )              ! sea surface height
         !
         ! --- vertical scale factors --- !
         IF( iom_use("e3t") ) THEN  ! time-varying e3t
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d0(ji,jj,jk) =  e3t(ji,jj,jk,Kmm)
            END_3D
            CALL iom_put( "e3t", z3d0 )
         ENDIF
         IF ( iom_use("e3u") ) THEN                         ! time-varying e3u
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d0(ji,jj,jk) =  e3u(ji,jj,jk,Kmm)
            END_3D 
            CALL iom_put( "e3u" , z3d0 )
         ENDIF
         IF ( iom_use("e3v") ) THEN                         ! time-varying e3v
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z3d0(ji,jj,jk) =  e3v(ji,jj,jk,Kmm)
            END_3D
            CALL iom_put( "e3v" , z3d0 )
         ENDIF
         !
         DEALLOCATE( z3d )
         DEALLOCATE( z3d0 )
      ENDIF
      !
      ! write the tracer concentrations in the file
      ! ---------------------------------------
      IF( ln_pisces  )   CALL trc_wri_pisces( kt, Kmm )     ! PISCES 
      IF( ll_cfc     )   CALL trc_wri_cfc   ( kt, Kmm )     ! surface fluxes of CFC
      IF( ln_c14     )   CALL trc_wri_c14   ( kt, Kmm )     ! surface fluxes of C14
      IF( ln_age     )   CALL trc_wri_age   ( kt, Kmm )     ! AGE tracer
      IF( ln_my_trc  )   CALL trc_wri_my_trc( kt, Kmm )     ! MY_TRC  tracers
      !
      IF( ln_timing )   CALL timing_stop('trc_wri')
      !
   END SUBROUTINE trc_wri

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
   IMPLICIT NONE
   PUBLIC trc_wri
CONTAINS
   SUBROUTINE trc_wri( kt, Kmm )                     ! Empty routine   
   INTEGER, INTENT(in) :: kt
   INTEGER, INTENT(in) :: Kmm  ! time level indices
   END SUBROUTINE trc_wri
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trcwri
