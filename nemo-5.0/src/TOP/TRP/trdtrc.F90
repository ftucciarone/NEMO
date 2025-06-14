MODULE trdtrc
   !!======================================================================
   !!                       ***  MODULE  trdtrc  ***
   !! Ocean diagnostics:  mixed layer passive tracer trends 
   !!======================================================================
   !! History :  3.0  !  2010-07  (C. Ethe)  Original code (from trdtrc.F90)
   !!----------------------------------------------------------------------
#if   defined key_top && ( defined key_trdmxl_trc   ||   defined key_trdtrc )
   !!----------------------------------------------------------------------
   !!   'key_trdmxl_trc'                  mixed layer trend diagnostics
   !!   'key_trdtrc'                      3D trend diagnostics
   !!----------------------------------------------------------------------
   !!   trdtrc      : passive tracer trends 
   !!----------------------------------------------------------------------
   USE trc               ! tracer definitions (tr(:,:,:,:,Kmm), tr(:,:,:,:,Kbb), tr(:,:,:,:,Krhs), etc.)
   USE trd_oce
   USE trdtrc_oce       ! definition of main arrays used for trends computations
   USE trdmxl_trc        ! Mixed layer trends diag.
   USE iom               ! I/O library
   USE domutl, ONLY : lbnd_ij
   USE par_kind

   IMPLICIT NONE
   PRIVATE

   PUBLIC trd_trc

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trd_trc( ptrtrd, kjn, ktrd, kt, Kmm )
      !!
      INTEGER                   , INTENT(in   )  ::   kt      ! time step
      INTEGER                   , INTENT(in   )  ::   Kmm     ! time level index
      INTEGER                   , INTENT(in   )  ::   kjn     ! tracer index
      INTEGER                   , INTENT(in   )  ::   ktrd    ! tracer trend index
      REAL(wp), DIMENSION(:,:,:), INTENT(inout)  ::   ptrtrd  ! Temperature or U trend
      !!
      CALL trd_trc_t( ptrtrd, lbnd_ij(ptrtrd), kjn, ktrd, kt, Kmm )
   END SUBROUTINE trd_trc


   SUBROUTINE trd_trc_t( ptrtrd, kttrtrd, kjn, ktrd, kt, Kmm )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trd_trc  ***
      !!----------------------------------------------------------------------
      INTEGER , DIMENSION(2)                , INTENT(in   )  ::   kttrtrd
      INTEGER                               , INTENT(in   )  ::   kt          ! time step
      INTEGER                               , INTENT(in   )  ::   Kmm         ! time level index
      INTEGER                               , INTENT(in   )  ::   kjn         ! tracer index
      INTEGER                               , INTENT(in   )  ::   ktrd        ! tracer trend index
      REAL(wp), DIMENSION(AB2D(kttrtrd),JPK), INTENT(inout)  ::   ptrtrd      ! Temperature or U trend
      CHARACTER (len=20) :: cltra
      !!----------------------------------------------------------------------

      IF( kt == nittrc000 ) THEN
!         IF(lwp)WRITE(numout,*)
!         IF(lwp)WRITE(numout,*) 'trd_trc:'
!         IF(lwp)WRITE(numout,*) '~~~~~~~~~~~~'
      ENDIF

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Mixed layer trends for passive tracers
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#if defined key_trdmxl_trc  
      IF( lk_trdmxl_trc .AND. ln_trdtrc( kjn ) ) THEN
         !
         SELECT CASE ( ktrd )
         CASE ( jptra_xad     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_xad, '3D', kjn, Kmm )
         CASE ( jptra_yad     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_yad, '3D', kjn, Kmm )
         CASE ( jptra_zad     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_zad, '3D', kjn, Kmm )
         CASE ( jptra_ldf     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_ldf, '3D', kjn, Kmm )
         CASE ( jptra_bbl     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_bbl, '3D', kjn, Kmm )
         CASE ( jptra_zdf     )
            IF( ln_trcldf_iso ) THEN
               CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_ldf, '3D', kjn, Kmm )
            ELSE
               CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_zdf, '3D', kjn, Kmm )
            ENDIF
         CASE ( jptra_dmp     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_dmp , '3D', kjn, Kmm )
         CASE ( jptra_nsr     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_sbc , '2D', kjn, Kmm )
         CASE ( jptra_sms     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_sms , '3D', kjn, Kmm )
         CASE ( jptra_radb    )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_radb, '3D', kjn, Kmm )
         CASE ( jptra_radn    )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_radn, '3D', kjn, Kmm )
         CASE ( jptra_atf     )   ;   CALL trd_mxl_trc_zint( ptrtrd, jpmxl_trc_atf , '3D', kjn, Kmm )
         END SELECT
         !
      END IF
#endif

      IF( lk_trdtrc .AND. ln_trdtrc( kjn ) ) THEN
         !
         SELECT CASE( ktrd )
         CASE( jptra_xad  )  ;  WRITE (cltra,'("XAD_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_yad  )  ;  WRITE (cltra,'("YAD_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_zad  )  ;  WRITE (cltra,'("ZAD_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_ldf  )  ;  WRITE (cltra,'("LDF_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_bbl  )  ;  WRITE (cltra,'("BBL_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_nsr  )  ;  WRITE (cltra,'("FOR_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_zdf  )  ;  WRITE (cltra,'("ZDF_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_zdfp )  ;  WRITE (cltra,'("ZDP_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_dmp  )  ;  WRITE (cltra,'("DMP_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_sms  )  ;  WRITE (cltra,'("SMS_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_tot  )  ;  WRITE (cltra,'("TOT_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_atf  )  ;  WRITE (cltra,'("ATF_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_radb )  ;  WRITE (cltra,'("RDB_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         CASE( jptra_radn )  ;  WRITE (cltra,'("RDN_",4a)')  ; cltra = TRIM(cltra)//TRIM(ctrcnm(kjn)) ; CALL iom_put( cltra,  ptrtrd(:,:,:) )
         END SELECT
         !
      END IF

   END SUBROUTINE trd_trc_t

#else
   !!----------------------------------------------------------------------
   !!   Default option :                                       Empty module
   !!----------------------------------------------------------------------

   USE par_kind

   IMPLICIT NONE

   PUBLIC trd_trc

CONTAINS

   SUBROUTINE trd_trc( ptrtrd, kjn, ktrd, kt, Kmm )
      INTEGER               , INTENT( in )     ::   kt      ! time step
      INTEGER               , INTENT( in )     ::   Kmm     ! time level index
      INTEGER               , INTENT( in )     ::   kjn     ! tracer index
      INTEGER               , INTENT( in )     ::   ktrd    ! tracer trend index
      REAL(wp), DIMENSION(:,:,:), INTENT( inout )  ::   ptrtrd  ! Temperature or U trend
      WRITE(*,*) 'trd_trc : You should not have seen this print! error?', ptrtrd(1,1,1)
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kjn
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', ktrd
      WRITE(*,*) '  "      "      : You should not have seen this print! error?', kt
   END SUBROUTINE trd_trc

#endif
   !!======================================================================
END MODULE trdtrc
