MODULE trczdf
   !!==============================================================================
   !!                 ***  MODULE  trczdf  ***
   !! Ocean Passive tracers : vertical diffusive trends 
   !!=====================================================================
   !! History :  9.0  ! 2005-11  (G. Madec)  Original code
   !!       NEMO 3.0  ! 2008-01  (C. Ethe, G. Madec)  merge TRC-TRA 
   !!            4.0  ! 2017-04  (G. Madec)  remove the explicit case
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_zdf      : update the tracer trend with the vertical diffusion
   !!----------------------------------------------------------------------
   USE par_trc        ! need jptra, number of passive tracers
   USE trc           ! ocean passive tracers variables
   USE oce_trc       ! ocean dynamics and active tracers
   USE trd_oce       ! trends: ocean variables
   USE trazdf        ! tracer: vertical diffusion
   USE trdtra        ! trends manager: tracers 
   USE prtctl        ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_zdf         ! called by step.F90 

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_zdf( kt, Kbb, Kmm, Krhs, ptr, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_zdf  ***
      !!
      !! ** Purpose :   compute the vertical ocean tracer physics using
      !!              an implicit time-stepping scheme.
      !!---------------------------------------------------------------------
      INTEGER                                   , INTENT(in   ) ::   kt                   ! ocean time-step index
      INTEGER                                   , INTENT(in   ) ::   Kbb, Kmm, Krhs, Kaa  ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) ::   ptr                  ! passive tracers and RHS of tracer equation
      !
      INTEGER               ::  ji, jj, jk, jn   ! dummy loop indices
      CHARACTER (len=22)    :: charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   ztrtrd   ! 4D workspace
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_zdf')
      !
      IF( l_trdtrc ) THEN
         ALLOCATE( ztrtrd(T2D(0),jpk,jptra) )
         DO jn = 1, jptra
            DO_3D( 0, 0, 0, 0, 1, jpk )
               ztrtrd(ji,jj,jk,jn)  = ptr(ji,jj,jk,jn,Krhs)
            END_3D
         END DO
      ENDIF
      !
      CALL tra_zdf_imp( 'TRC', rDt_trc, Kbb, Kmm, Krhs, ptr, Kaa, jptra )    !   implicit scheme          
      !
      IF( l_trdtrc )   THEN                      ! save the vertical diffusive trends for further diagnostics
         DO jn = 1, jptra
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               ztrtrd(ji,jj,jk,jn) = ( ( ptr(ji,jj,jk,jn,Kaa) - ptr(ji,jj,jk,jn,Kbb) ) / rDt_trc ) - ztrtrd(ji,jj,jk,jn)
            END_3D
            CALL trd_tra( kt, Kmm, Krhs, 'TRC', jn, jptra_zdf, ztrtrd(:,:,:,jn) )
         END DO
         !
         DEALLOCATE( ztrtrd )
      ENDIF
      !                                          ! print mean trends (used for debugging)
      IF( sn_cfctl%l_prttrc )   THEN
         WRITE(charout, FMT="('zdf ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl( tab4d_1=tr(:,:,:,:,Kaa), mask1=tmask, clinfo=ctrcnm, clinfo3='trd' )
      END IF
      !
      IF( ln_timing )  CALL timing_stop('trc_zdf')
      !
   END SUBROUTINE trc_zdf
   
#else
   !!----------------------------------------------------------------------
   !!   Default option                                         Empty module
   !!----------------------------------------------------------------------
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_zdf( kt )
      INTEGER, INTENT(in) :: kt  
      WRITE(*,*) 'trc_zdf: You should not have seen this print! error?', kt
   END SUBROUTINE trc_zdf
#endif
   !!==============================================================================
END MODULE trczdf
