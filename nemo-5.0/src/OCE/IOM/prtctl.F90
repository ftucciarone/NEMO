MODULE prtctl
   !!======================================================================
   !!                       ***  MODULE prtctl   ***
   !! Ocean system : print all SUM trends for each processor domain
   !!======================================================================
   !! History :  9.0  !  05-07  (C. Talandier) original code
   !!            3.4  !  11-11  (C. Harris) decomposition changes for running with CICE
   !!----------------------------------------------------------------------
   USE dom_oce                     ! ocean space and time domain variables
   USE domutl, ONLY : lbnd_ij
   USE in_out_manager              ! I/O manager
   USE mppini                      ! distributed memory computing
   USE lib_mpp                     ! distributed memory computing

   IMPLICIT NONE
   PRIVATE

   INTEGER , DIMENSION(  :), ALLOCATABLE ::   numprt_oce, numprt_top
   INTEGER , DIMENSION(  :), ALLOCATABLE ::   nall_ictls, nall_ictle   ! first, last indoor index for each i-domain
   INTEGER , DIMENSION(  :), ALLOCATABLE ::   nall_jctls, nall_jctle   ! first, last indoor index for each j-domain
#if defined key_agrif
   REAL(wp), DIMENSION(  :), ALLOCATABLE ::   t_ctl , s_ctl            ! previous tracer trend values
   REAL(wp), DIMENSION(  :), ALLOCATABLE ::   u_ctl , v_ctl            ! previous velocity trend values
   REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   tra_ctl                  ! previous top trend values
#else
   COMPLEX(dp), DIMENSION(  :), ALLOCATABLE ::   t_ctl , s_ctl            ! previous tracer trend values
   COMPLEX(dp), DIMENSION(  :), ALLOCATABLE ::   u_ctl , v_ctl            ! previous velocity trend values
   COMPLEX(dp), DIMENSION(:,:), ALLOCATABLE ::   tra_ctl                  ! previous top trend values
#endif
   !
   PUBLIC prt_ctl         ! called by all subroutines
   PUBLIC prt_ctl_info    ! called by all subroutines
   PUBLIC prt_ctl_init    ! called by nemogcm.F90 and prt_ctl_trc_init

   INTERFACE prt_ctl_sum
      PROCEDURE prt_ctl_sum_2d, prt_ctl_sum_3d
   END INTERFACE prt_ctl_sum

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE prt_ctl (tab2d_1, tab3d_1, tab4d_1, tab2d_2, tab3d_2, mask1, mask2,   &
      &                 clinfo, clinfo1, clinfo2, clinfo3, kdim )
      !!----------------------------------------------------------------------
      !!                      ***  ROUTINE prt_ctl  ***
      !!
      !! ** Purpose : - print sum control of 2D or 3D arrays over the same area
      !!                in mono and mpp case. This way can be usefull when
      !!                debugging a new parametrization in mono or mpp.
      !!
      !! ** Method  : 2 possibilities exist when setting the sn_cfctl%prtctl parameter to
      !!                .true. in the ocean namelist:
      !!              - to debug a MPI run .vs. a mono-processor one;
      !!                the control print will be done over each sub-domain.
      !!                The nictl[se] and njctl[se] parameters in the namelist must
      !!                be set to zero and [ij]splt to the corresponding splitted
      !!                domain in MPI along respectively i-, j- directions.
      !!              - to debug a mono-processor run over the whole domain/a specific area;
      !!                in the first case the nictl[se] and njctl[se] parameters must be set
      !!                to zero else to the indices of the area to be controled. In both cases
      !!                isplt and jsplt must be set to 1.
      !!              - All arguments of the above calling sequence are optional so their
      !!                name must be explicitly typed if used. For instance if the 3D
      !!                array tn(:,:,:) must be passed through the prt_ctl subroutine,
      !!                it must look like: CALL prt_ctl(tab3d_1=tn).
      !!
      !!                    tab2d_1 : first 2D array
      !!                    tab3d_1 : first 3D array
      !!                    tab4d_1 : first 4D array
      !!                    mask1   : mask (3D) to apply to the tab[23]d_1 array
      !!                    clinfo1 : information about the tab[23]d_1 array
      !!                    tab2d_2 : second 2D array
      !!                    tab3d_2 : second 3D array
      !!                    mask2   : mask (3D) to apply to the tab[23]d_2 array
      !!                    clinfo2 : information about the tab[23]d_2 array
      !!                    kdim    : k- direction for 3D arrays
      !!                    clinfo3 : additional information
      !!----------------------------------------------------------------------
      REAL(wp),         DIMENSION(:,:)    , INTENT(in), OPTIONAL ::   tab2d_1
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   tab3d_1
      REAL(wp),         DIMENSION(:,:,:,:), INTENT(in), OPTIONAL ::   tab4d_1
      REAL(wp),         DIMENSION(:,:)    , INTENT(in), OPTIONAL ::   tab2d_2
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   tab3d_2
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   mask1
      REAL(wp),         DIMENSION(:,:,:)  , INTENT(in), OPTIONAL ::   mask2
      CHARACTER(len=*), DIMENSION(:)      , INTENT(in), OPTIONAL ::   clinfo    ! information about the tab3d array
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo1
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo2
      CHARACTER(len=*)                    , INTENT(in), OPTIONAL ::   clinfo3
      INTEGER                             , INTENT(in), OPTIONAL ::   kdim
      INTEGER,          DIMENSION(2)                             ::   ibndg, ibndm1, ibndm2
      !
      ibndg(1) = Nis0 ; ibndg(2) = Njs0
      IF( PRESENT(mask1) ) THEN ; ibndm1 = lbnd_ij(mask1) ; ELSE ; ibndm1 = ibndg ; ENDIF
      IF( PRESENT(mask2) ) THEN ; ibndm2 = lbnd_ij(mask2) ; ELSE ; ibndm2 = ibndg ; ENDIF

      IF(     PRESENT(tab2d_2) ) THEN
         CALL prt_ctl_t(ktab2d_1=lbnd_ij(tab2d_1), ktab3d_1=ibndg, ktab4d_1=ibndg, &
            &           ktab2d_2=lbnd_ij(tab2d_2), ktab3d_2=ibndg,                 &
            &           ptab2d_1=tab2d_1, ptab2d_2=tab2d_2,                        &
            &           kmask1=ibndm1, kmask2=ibndm2, pmask1=mask1, pmask2=mask2,  &
            &           cdinfo=clinfo, cdinfo1=clinfo1, cdinfo2=clinfo2, cdinfo3=clinfo3 )
      ELSEIF( PRESENT(tab3d_2) ) THEN
         CALL prt_ctl_t(ktab2d_1=ibndg, ktab3d_1=lbnd_ij(tab3d_1), ktab4d_1=ibndg, &
            &           ktab2d_2=ibndg, ktab3d_2=lbnd_ij(tab3d_2),                 &
            &           ptab3d_1=tab3d_1, ptab3d_2=tab3d_2,                        &
            &           kmask1=ibndm1, kmask2=ibndm2, pmask1=mask1, pmask2=mask2,  &
            &           cdinfo=clinfo, cdinfo1=clinfo1, cdinfo2=clinfo2, cdinfo3=clinfo3, kdim=kdim )
      ELSEIF( PRESENT(tab2d_1) ) THEN
         CALL prt_ctl_t(ktab2d_1=lbnd_ij(tab2d_1), ktab3d_1=ibndg, ktab4d_1=ibndg, &
            &           ktab2d_2=ibndg, ktab3d_2=ibndg,                            &
            &           ptab2d_1=tab2d_1,                                          &
            &           kmask1=ibndm1, kmask2=ibndm2, pmask1=mask1,                &
            &           cdinfo=clinfo, cdinfo1=clinfo1, cdinfo3=clinfo3 )
      ELSEIF( PRESENT(tab3d_1) ) THEN
         CALL prt_ctl_t(ktab2d_1=ibndg, ktab3d_1=lbnd_ij(tab3d_1), ktab4d_1=ibndg, &
            &           ktab2d_2=ibndg, ktab3d_2=ibndg,                            &
            &           ptab3d_1=tab3d_1,                                          &
            &           kmask1=ibndm1, kmask2=ibndm2, pmask1=mask1,                &
            &           cdinfo=clinfo, cdinfo1=clinfo1, cdinfo3=clinfo3, kdim=kdim )
      ELSEIF( PRESENT(tab4d_1) ) THEN     
         CALL prt_ctl_t(ktab2d_1=ibndg, ktab3d_1=ibndg, ktab4d_1=lbnd_ij(tab4d_1), &
            &           ktab2d_2=ibndg, ktab3d_2=ibndg,                            &
            &           ptab4d_1=tab4d_1,                                          &
            &           kmask1=ibndm1, kmask2=ibndm2, pmask1=mask1,                &
            &           cdinfo=clinfo, cdinfo1=clinfo1, cdinfo3=clinfo3, kdim=kdim )
      ENDIF

   END SUBROUTINE prt_ctl

   SUBROUTINE prt_ctl_t (ptab2d_1, ktab2d_1, ptab3d_1, ktab3d_1, ptab4d_1,   &
      &                  ktab4d_1, ptab2d_2, ktab2d_2, ptab3d_2, ktab3d_2,   &
      &                  pmask1,   kmask1,   pmask2,   kmask2,   cdinfo,     &
      &                  cdinfo1,  cdinfo2,  cdinfo3,  kdim )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_t  ***
      !!
      !! ** Purpose : see wrapper subroutine 'prt_ctl' above
      !!
      !! ** Method : see wrapper subroutine 'prt_ctl' above
      !!
      !!----------------------------------------------------------------------
      INTEGER,          DIMENSION(2),                  INTENT(in)           ::   ktab2d_1, ktab3d_1, ktab4d_1, ktab2d_2, ktab3d_2
      INTEGER,          DIMENSION(2),                  INTENT(in)           ::   kmask1, kmask2
      REAL(wp),         DIMENSION(AB2D(ktab2d_1)),     INTENT(in), OPTIONAL ::   ptab2d_1
      REAL(wp),         DIMENSION(AB2D(ktab3d_1),:),   INTENT(in), OPTIONAL ::   ptab3d_1
      REAL(wp),         DIMENSION(AB2D(ktab4d_1),:,:), INTENT(in), OPTIONAL ::   ptab4d_1
      REAL(wp),         DIMENSION(AB2D(ktab2d_2)),     INTENT(in), OPTIONAL ::   ptab2d_2
      REAL(wp),         DIMENSION(AB2D(ktab3d_2),:),   INTENT(in), OPTIONAL ::   ptab3d_2
      REAL(wp),         DIMENSION(AB2D(kmask1),:),     INTENT(in), OPTIONAL ::   pmask1
      REAL(wp),         DIMENSION(AB2D(kmask2),:),     INTENT(in), OPTIONAL ::   pmask2
      CHARACTER(len=*), DIMENSION(:),                  INTENT(in), OPTIONAL ::   cdinfo    ! information about the ptab3d array
      CHARACTER(len=*),                                INTENT(in), OPTIONAL ::   cdinfo1
      CHARACTER(len=*),                                INTENT(in), OPTIONAL ::   cdinfo2
      CHARACTER(len=*),                                INTENT(in), OPTIONAL ::   cdinfo3
      INTEGER,                                         INTENT(in), OPTIONAL ::   kdim
      !
      CHARACTER(len=30) :: cl1, cl2, cl3
      CHARACTER(len=6) :: clfmt
      CHARACTER(len=1) :: cli1
      INTEGER ::  jn, jl, kdir
      INTEGER ::  iis, iie, jjs, jje
      INTEGER ::  itra, inum
      COMPLEX(dp)       ::   ylsum1, ylsum2     ! Summation results
      COMPLEX(dp)       ::   yltmp1, yltmp2     ! Temporary variables
      INTEGER           ::   jk                 ! Loop index
      INTEGER           ::   isig               ! Number of significant digits in sums output
      CHARACTER(LEN=41) ::   clsum1, clsum2     ! String representation of sums
      !!----------------------------------------------------------------------
      !
      ! Arrays, scalars initialization
      cl1  = ''
      cl2  = ''
      kdir = jpkm1
      itra = 1

      ! Control of optional arguments
      IF( PRESENT(cdinfo1) )  cl1  = cdinfo1
      IF( PRESENT(cdinfo2) )  cl2  = cdinfo2
      IF( PRESENT(kdim)    )  kdir = kdim
      IF( PRESENT(ptab4d_1) ) itra = SIZE(ptab4d_1,dim=4)

      IF( wp == sp ) isig = 2 * PRECISION(0._sp)   !  16   ! 16 significant digits
      IF( wp == dp ) isig = 2 * PRECISION(0._dp)   !  34   ! 34 significant digits
      WRITE( clfmt, "(a1,i02,a1,i02)" ) 'D', isig+7, '.', isig
      
      ! Loop over each sub-domain, i.e. the total number of processors ijsplt
      DO jl = 1, SIZE(nall_ictls)

         iis = MAX( nall_ictls(jl), ntsi )
         iie = MIN( nall_ictle(jl), ntei )
         jjs = MAX( nall_jctls(jl), ntsj )
         jje = MIN( nall_jctle(jl), ntej )

         IF( PRESENT(cdinfo) ) THEN   ;   inum = numprt_top(jl)
         ELSE                         ;   inum = numprt_oce(jl)
         ENDIF

         ! Compute the sum control only where the inner domain and control print area overlap.
         ! Note that if tiling is enabled and currently active, the inner domain is that of the current tile.
         ! Otherwise, the inner domain is that of the current MPI domain (i.e. Nis0:Nie0, Njs0:Nje0)
         IF( iie >= iis .AND. jje >= jjs ) THEN
            DO jn = 1, itra

               ! 2D arrays
               IF( PRESENT(ptab2d_1) ) THEN
                  IF( PRESENT(pmask1) ) THEN
                     ylsum1 = prt_ctl_sum( ptab2d_1(iis:iie,jjs:jje) * pmask1(iis:iie,jjs:jje,1) )
                  ELSE
                     ylsum1 = prt_ctl_sum( ptab2d_1(iis:iie,jjs:jje) )
                  END IF
               ENDIF
               IF( PRESENT(ptab2d_2) ) THEN
                  IF( PRESENT(pmask2) ) THEN
                     ylsum2 = prt_ctl_sum( ptab2d_2(iis:iie,jjs:jje) * pmask2(iis:iie,jjs:jje,1) )
                  ELSE
                     ylsum2 = prt_ctl_sum( ptab2d_2(iis:iie,jjs:jje) )
                  END IF
               ENDIF

               ! 3D arrays
               IF( PRESENT(ptab3d_1) ) THEN
                  IF( PRESENT(pmask1) ) THEN
                     ylsum1 = prt_ctl_sum( ptab3d_1(iis:iie,jjs:jje,1:kdir) * pmask1(iis:iie,jjs:jje,1:kdir) )
                  ELSE
                     ylsum1 = prt_ctl_sum( ptab3d_1(iis:iie,jjs:jje,1:kdir) )
                  END IF
               ENDIF
               IF( PRESENT(ptab3d_2) ) THEN
                  IF( PRESENT(pmask2) ) THEN
                     ylsum2 = prt_ctl_sum( ptab3d_2(iis:iie,jjs:jje,1:kdir) * pmask2(iis:iie,jjs:jje,1:kdir) )
                  ELSE
                     ylsum2 = prt_ctl_sum( ptab3d_2(iis:iie,jjs:jje,1:kdir) )
                  END IF
               ENDIF

               ! 4D arrays
               IF( PRESENT(ptab4d_1) ) THEN
                  IF( PRESENT(pmask1) ) THEN
                     ylsum1 = prt_ctl_sum( ptab4d_1(iis:iie,jjs:jje,1:kdir,jn) * pmask1(iis:iie,jjs:jje,1:kdir) )
                  ELSE
                     ylsum1 = prt_ctl_sum( ptab4d_1(iis:iie,jjs:jje,1:kdir,jn) )
                  END IF
               ENDIF

               ! Print the result
               IF( PRESENT(cdinfo ) ) cl1  = cdinfo(jn)
               IF( PRESENT(cdinfo3) ) THEN
                  ! Replace sums with their difference to the corresponding sum
                  ! at the previous time step and record the current sums for
                  ! use during next time step
                  SELECT CASE( cdinfo3 )
                  CASE ( 'tra-ta' )
                     yltmp1 = t_ctl(jl)
                     t_ctl(jl) = ylsum1
                  CASE ( 'tra' )
                     yltmp1 = t_ctl(jl)
                     t_ctl(jl) = ylsum1
                     yltmp2 = s_ctl(jl)
                     s_ctl(jl) = ylsum2
                  CASE ( 'dyn' )
                     yltmp1 = u_ctl(jl)
                     u_ctl(jl) = ylsum1
                     yltmp2 = v_ctl(jl)
                     v_ctl(jl) = ylsum2
                  CASE default
                     yltmp1 = tra_ctl(jn,jl)
                     tra_ctl(jn,jl) = ylsum1
                  END SELECT
                  CALL DDPDD( -1._dp * yltmp1, ylsum1 )
                  IF ( PRESENT(ptab2d_2) .OR. PRESENT(ptab3d_2) ) CALL DDPDD( -1._dp * yltmp2, ylsum2 )
               END IF
               clsum1 = prt_ctl_write_sum( ylsum1, isig )
               IF ( PRESENT(ptab2d_2) .OR. PRESENT(ptab3d_2) ) THEN
                  clsum2 = prt_ctl_write_sum( ylsum2, isig )
                  WRITE(inum, "(3x,a,' : ',a,3x,a,' : ',a)") cl1, TRIM(clsum1), cl2, TRIM(clsum2)
               ELSE
                  WRITE(inum, "(3x,a,' : ',a)") cl1, TRIM(clsum1)
               END IF
               ! replace .false. by .true. to switch on theses prints of the last inner line
               IF( .FALSE. .AND. l_IdoNFold .AND. jje == Nje0 ) THEN
                  IF( PRESENT(ptab2d_1) ) THEN
                     WRITE(cli1, '(i1)') INT(LOG10(REAL(iie-iis+1,wp))) + 1            ! how many digits to we need to write ?
                     WRITE(cl3, "(i"//cli1//")") iie-iis+1
                     WRITE(inum, "(a,"//TRIM(cl3)//clfmt//")") 'Last line '//TRIM(cl1)//' ', ptab2d_1(iis:iie,jje)
                  ENDIF
                  IF( PRESENT(ptab3d_1) ) THEN
                     WRITE(cli1, '(i1)') INT(LOG10(REAL((iie-iis+1)*kdir,wp))) + 1     ! how many digits to we need to write ?
                     WRITE(cl3, "(i"//cli1//")") (iie-iis+1)*kdir
                     WRITE(inum, "(a,"//TRIM(cl3)//clfmt//")") 'Last line '//TRIM(cl1)//' ', ptab3d_1(iis:iie,jje,1:kdir)
                  ENDIF
                  IF( PRESENT(ptab2d_2) ) THEN
                     WRITE(cli1, '(i1)') INT(LOG10(REAL(iie-iis+1,wp))) + 1            ! how many digits to we need to write ?
                     WRITE(cl3, "(i"//cli1//")") iie-iis+1
                     WRITE(inum, "(a,"//TRIM(cl3)//clfmt//")") 'Last line '//TRIM(cl2)//' ', ptab2d_2(iis:iie,jje)
                  ENDIF
                  IF( PRESENT(ptab3d_2) ) THEN
                     WRITE(cli1, '(i1)') INT(LOG10(REAL((iie-iis+1)*kdir,wp))) + 1     ! how many digits to we need to write ?
                     WRITE(cl3, "(i"//cli1//")") (iie-iis+1)*kdir
                     WRITE(inum, "(a,"//TRIM(cl3)//clfmt//")") 'Last line '//TRIM(cl2)//' ', ptab3d_2(iis:iie,jje,1:kdir)
                  ENDIF
               ENDIF
            END DO
         ENDIF
         IF( jpnij == 1 ) CALL FLUSH(inum)
      END DO
      !
   END SUBROUTINE prt_ctl_t

   FUNCTION prt_ctl_sum_2d( ptab ) RESULT( ydsum )
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION prt_ctl_sum_3d  ***
      !!
      !! ** Purpose : summation of 2-dimensional real-valued arrays using
      !!              subroutine 'DDPDD' (of module 'lib_fortran'), which
      !!              returns the sum as REAL( ydsum ) + AIMAG( ydsum ) of the
      !!              complex value ydsum
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(in) ::   ptab
      COMPLEX(dp)                          ::   ydsum    ! Sum
      INTEGER                              ::   ji, jj   ! Loop indices
      !
      ydsum = CMPLX( 0._wp, kind = dp )
      DO jj = 1, SIZE( ptab, 2 )
         DO ji = 1, SIZE( ptab, 1 )
            CALL DDPDD( CMPLX( ptab(ji,jj), kind = dp ), ydsum )
         END DO
      END DO
      !
   END FUNCTION prt_ctl_sum_2d

   FUNCTION prt_ctl_sum_3d( ptab ) RESULT( ydsum )
      !!----------------------------------------------------------------------
      !!                 ***  FUNCTION prt_ctl_sum_3d  ***
      !!
      !! ** Purpose : summation of 3-dimensional real-valued arrays using
      !!              subroutine 'DDPDD' (of module 'lib_fortran'), which
      !!              returns the sum as REAL( ydsum ) + AIMAG( ydsum ) of the
      !!              complex value ydsum
      !!
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), INTENT(in) ::   ptab
      COMPLEX(dp)                            ::   ydsum        ! Sum
      INTEGER                    ::   ji, jj, jk   ! Loop indices
      !
      ydsum = CMPLX( 0._wp, kind = dp )
      DO jk = 1, SIZE( ptab, 3 )
         DO jj = 1, SIZE( ptab, 2 )
            DO ji = 1, SIZE( ptab, 1 )
               CALL DDPDD( CMPLX( ptab(ji,jj,jk), kind = dp ), ydsum )
            END DO
         END DO
      END DO
      !
   END FUNCTION prt_ctl_sum_3d

   FUNCTION prt_ctl_write_sum( ydsum, ksig ) RESULT( cdsum )
      !!----------------------------------------------------------------------
      !!                    ***  FUNCTION prt_ctl_write_sum  ***
      !!
      !! ** Purpose : convert the sum REAL(ydsum) + AIMAG(ydsum) into a string
      !!              representation with ksig significant digits
      !!
      !! ** Method : convert the two components of the sum into strings with 
      !!             one significant digits more than the target precision,
      !!             add the two components in string representation, and
      !!             round the result according the last digit of the
      !!             significand
      !!
      !!----------------------------------------------------------------------
      COMPLEX(dp), INTENT(IN   ) ::   ydsum           ! Summands (REAL(ydsum) and AIMAG(ydsum))
      INTEGER, INTENT(IN   )     ::   ksig            ! Requested number of significant digits
      CHARACTER(LEN=ksig+7)      ::   cdsum           ! Converted value (string of length ksig+7)
      !
      CHARACTER(LEN=8)           ::   clfmt           ! Format string for conversion of real values to strings
      CHARACTER(LEN=ksig+8)      ::   clv1, clv2      ! String representation of the components
      INTEGER                    ::   id1, id2, idp   ! Exponents of the values in string representation and their difference
      LOGICAL                    ::   lzero           ! Indicator of zero value
      INTEGER                    ::   ico             ! Carry-over value
      INTEGER                    ::   jd              ! Loop counter
      !
      ! Sort the two components by absolute value and convert them to a string
      ! representations in the range (-1, 1) with one significant digit more
      ! than the target precision (ksig)
      WRITE( clfmt, "(a2,i02,a1,i02,a1)" ) '(D', ksig+8, '.', ksig+1, ')'
      IF( ABS( REAL( ydsum ) ) >= ABS ( AIMAG( ydsum ) ) ) THEN
         WRITE( clv1, clfmt ) REAL(ydsum)
         WRITE( clv2, clfmt ) AIMAG(ydsum)
         lzero = ABS( AIMAG(ydsum) ) == 0._wp
      ELSE
         WRITE( clv1(:ksig+8), clfmt ) AIMAG(ydsum)
         WRITE( clv2(:ksig+8), clfmt ) REAL(ydsum)
         lzero = ABS( REAL(ydsum) ) == 0._wp
      END IF
      IF( .NOT. lzero ) THEN
         ! Align exponents of the string representations of the two components
         READ( clv1(ksig+6:ksig+8), "(i3)" ) id1               ! Exponent of value 1
         READ( clv2(ksig+6:ksig+8), "(i3)" ) id2               ! Exponent of value 2
         clv2(id1-id2+4:ksig+8-4) = clv2(4:ksig+8-4-id1+id2)   ! Re-aligned significand of value 2
         DO jd = 4, id1-id2+3                                  ! Backfilling of freed digits of value 2
            clv2(jd:jd) = '0'
         END DO
         ! Add each digit of the aligned string separately with carry-over and rounding to the value with larger absolute value,
         ! retain its sign, and reposition its exponent
         ico = 0
         DO jd = ksig+8-4, 4, -1                  ! Move through significand from right to left
            READ( clv1(jd:jd), "(i1)" ) id1                       ! Current digit of value 1
            READ( clv2(jd:jd), "(i1)" ) id2                       ! Current digit of value 2
            id2 = id2 + ico                                       ! Add carry-over value
            IF( clv1(1:1) .NE. clv2(1:1) ) THEN                   ! Subtraction if the sign differs
               ico = ( id2 - id1 + 9 ) / 10                       !    Update carry-over value
               id1 = MOD( 10 + id1 - id2, 10 )                    !    Compute current digit
               IF( jd == ksig+8-4 .AND. id1 > 5 ) ico = ico - 1   !    Adjust initial carry-over value if rounding occurs
            ELSE                                                  ! Addition if the sign matches
               ico = ( id1 + id2 ) / 10                           !    Update carry-over value
               id1 = MOD( id1 + id2 , 10 )                        !    Compute current digit
               IF( jd == ksig+8-4 .AND. id1 > 5 ) ico = ico + 1   !    Adjust initial carry-over value if rounding occurs
            END IF
            WRITE( clv1(jd:jd), "(i1)" ) id1                      ! Record current digit
         END DO
         IF( ico > 0 ) WRITE( clv1(2:2), "(i1)" ) ico             ! If required, add carry-over value beyond decimal point
      END IF
      clv1(ksig+4:ksig+7) = clv1(ksig+5:ksig+8)   ! Re-position exponent with regard to the target format
      cdsum = clv1(:ksig+7)                       ! Return string
      !
   END FUNCTION prt_ctl_write_sum

   SUBROUTINE prt_ctl_info (cdinfo, ivar, cdcomp )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_info  ***
      !!
      !! ** Purpose : - print information without any computation
      !!
      !! ** Action  : - input arguments
      !!                    cdinfo : information about the ivar
      !!                    ivar   : value to print
      !!----------------------------------------------------------------------
      CHARACTER(len=*),           INTENT(in) ::   cdinfo
      INTEGER         , OPTIONAL, INTENT(in) ::   ivar
      CHARACTER(len=3), OPTIONAL, INTENT(in) ::   cdcomp   ! only 'top' is accepted
      !
      CHARACTER(len=3) :: clcomp
      INTEGER ::  jl, inum
      !!----------------------------------------------------------------------
      !
      IF( PRESENT(cdcomp) ) THEN   ;   clcomp = cdcomp
      ELSE                         ;   clcomp = 'oce'
      ENDIF
      !
      DO jl = 1, SIZE(nall_ictls)
         !
         IF( clcomp == 'oce' )   inum = numprt_oce(jl)
         IF( clcomp == 'top' )   inum = numprt_top(jl)
         !
         IF ( PRESENT(ivar) ) THEN   ;   WRITE(inum,*) cdinfo, ivar
         ELSE                        ;   WRITE(inum,*) cdinfo
         ENDIF
         !
      END DO
      !
   END SUBROUTINE prt_ctl_info


   SUBROUTINE prt_ctl_init( cdcomp, kntra )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE prt_ctl_init  ***
      !!
      !! ** Purpose :   open ASCII files & compute indices
      !!----------------------------------------------------------------------
      CHARACTER(len=3), OPTIONAL, INTENT(in   ) ::   cdcomp   ! only 'top' is accepted
      INTEGER         , OPTIONAL, INTENT(in   ) ::   kntra    ! only for 'top': number of tracers
      !
      INTEGER ::   ji, jj, jl
      INTEGER ::   inum, idg, idg2
      INTEGER ::   ijsplt, iimax, ijmax
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::    iimppt, ijmppt, ijpi, ijpj, iproc
      INTEGER, DIMENSION(  :), ALLOCATABLE ::     iipos,  ijpos
      LOGICAL, DIMENSION(:,:), ALLOCATABLE ::   llisoce
      CHARACTER(len=64) :: clfile_out
      CHARACTER(LEN=64) :: clfmt, clfmt2, clfmt3, clfmt4
      CHARACTER(len=32) :: clname, cl_run
      CHARACTER(len= 3) :: clcomp
      !!----------------------------------------------------------------------
      !
      clname = 'output'
      IF( PRESENT(cdcomp) ) THEN
         clname = TRIM(clname)//'.'//TRIM(cdcomp)
         clcomp = cdcomp
      ELSE
         clcomp = 'oce'
      ENDIF
      !
      IF( jpnij > 1 ) THEN   ! MULTI processor run
         cl_run = 'MULTI processor run'
         idg = MAX( INT(LOG10(REAL(MAX(1,jpnij-1),wp))) + 1, 4 )    ! how many digits to we need to write? min=4, max=9
         WRITE(clfmt, "('(a,i', i1, '.', i1, ')')") idg, idg        ! '(a,ix.x)'
         WRITE(clfile_out,clfmt) 'mpp.'//trim(clname)//'_', narea - 1
         ijsplt = 1
      ELSE                   ! MONO processor run
         cl_run = 'MONO processor run '
         IF(lwp) THEN                  ! control print
            WRITE(numout,*)
            WRITE(numout,*) 'prt_ctl_init: sn_cfctl%l_prtctl parameters'
            WRITE(numout,*) '~~~~~~~~~~~~~'
         ENDIF
         IF( nn_ictls+nn_ictle+nn_jctls+nn_jctle == 0 )   THEN    ! print control done over the default area
            nn_isplt = MAX(1, nn_isplt)            ! number of processors following i-direction
            nn_jsplt = MAX(1, nn_jsplt)            ! number of processors following j-direction
            ijsplt = nn_isplt * nn_jsplt           ! total number of processors ijsplt
            IF( ijsplt == 1 )   CALL ctl_warn( 'nn_isplt & nn_jsplt are equal to 1 -> control sum done over the whole domain' )
            IF(lwp) WRITE(numout,*) '      number of proc. following i     nn_isplt   = ', nn_isplt
            IF(lwp) WRITE(numout,*) '      number of proc. following j     nn_jsplt   = ', nn_jsplt
            idg = MAX( INT(LOG10(REAL(MAX(1,ijsplt-1),wp))) + 1, 4 )    ! how many digits to we need to write? min=4, max=9
            WRITE(clfmt, "('(a,i', i1, '.', i1, ')')") idg, idg         ! '(a,ix.x)'
            IF( ijsplt == 1 ) WRITE(clfile_out,clfmt) 'mono.'//trim(clname)//'_', 0
         ELSE                                             ! print control done over a specific  area
            ijsplt = 1
            IF( nn_ictls < 1 .OR. nn_ictls > Ni0glo )   THEN
               CALL ctl_warn( '          - nictls must be 1<=nictls>=Ni0glo, it is forced to 1' )
               nn_ictls = 1
            ENDIF
            IF( nn_ictle < 1 .OR. nn_ictle > Ni0glo )   THEN
               CALL ctl_warn( '          - nictle must be 1<=nictle>=Ni0glo, it is forced to Ni0glo' )
               nn_ictle = Ni0glo
            ENDIF
            IF( nn_jctls < 1 .OR. nn_jctls > Nj0glo )   THEN
               CALL ctl_warn( '          - njctls must be 1<=njctls>=Nj0glo, it is forced to 1' )
               nn_jctls = 1
            ENDIF
            IF( nn_jctle < 1 .OR. nn_jctle > Nj0glo )   THEN
               CALL ctl_warn( '          - njctle must be 1<=njctle>=Nj0glo, it is forced to Nj0glo' )
               nn_jctle = Nj0glo
            ENDIF
            WRITE(numout,*) '      Start i indice for SUM control  nn_ictls   = ', nn_ictls
            WRITE(numout,*) '      End i indice for SUM control    nn_ictle   = ', nn_ictle
            WRITE(numout,*) '      Start j indice for SUM control  nn_jctls   = ', nn_jctls
            WRITE(numout,*) '      End j indice for SUM control    nn_jctle   = ', nn_jctle
            idg = MAXVAL( (/ nn_ictls,nn_ictle,nn_jctls,nn_jctle /) )   ! temporary use of idg to store the largest index
            idg = MAX( INT(LOG10(REAL(idg,wp))) + 1, 4 )                ! how many digits to we need to write? min=4, max=9
            WRITE(clfmt, "('(4(a,i', i1, '.', i1, '))')") idg, idg         ! '(4(a,ix.x))'
            WRITE(clfile_out,clfmt) 'mono.'//trim(clname)//'_', nn_ictls, '_', nn_ictle, '_', nn_jctls, '_', nn_jctle
         ENDIF
      ENDIF

      ! Allocate arrays
      IF( .NOT. ALLOCATED(nall_ictls) ) ALLOCATE( nall_ictls(ijsplt), nall_ictle(ijsplt), nall_jctls(ijsplt), nall_jctle(ijsplt) )

      IF( jpnij > 1 ) THEN   ! MULTI processor run
         !
         nall_ictls(1) = Nis0
         nall_ictle(1) = Nie0
         nall_jctls(1) = Njs0
         nall_jctle(1) = Nje0
         !
      ELSE                   ! MONO processor run
         !
         IF( nn_ictls+nn_ictle+nn_jctls+nn_jctle == 0 )   THEN    ! print control done over the default area
            !
            ALLOCATE(  iimppt(nn_isplt,nn_jsplt), ijmppt(nn_isplt,nn_jsplt),  ijpi(nn_isplt,nn_jsplt),  ijpj(nn_isplt,nn_jsplt),   &
               &      llisoce(nn_isplt,nn_jsplt),  iproc(nn_isplt,nn_jsplt), iipos(nn_isplt*nn_jsplt), ijpos(nn_isplt*nn_jsplt) )
            CALL mpp_basesplit( jpiglo, jpjglo, nn_hls, nn_isplt, nn_jsplt, iimax, ijmax, iimppt, ijmppt, ijpi, ijpj )
            CALL mpp_is_ocean( llisoce )
            CALL mpp_getnum( llisoce, iproc, iipos, ijpos )
            !
            DO jj = 1,nn_jsplt
               DO ji = 1, nn_isplt
                  jl = iproc(ji,jj) + 1
                  nall_ictls(jl) = iimppt(ji,jj) - 1 +      1      + nn_hls
                  nall_ictle(jl) = iimppt(ji,jj) - 1 + ijpi(ji,jj) - nn_hls
                  nall_jctls(jl) = ijmppt(ji,jj) - 1 +      1      + nn_hls
                  nall_jctle(jl) = ijmppt(ji,jj) - 1 + ijpj(ji,jj) - nn_hls
               END DO
            END DO
            !
            DEALLOCATE( iimppt, ijmppt, ijpi, ijpj, llisoce, iproc, iipos, ijpos )
            !
         ELSE                                             ! print control done over a specific  area
            !
            nall_ictls(1) = nn_ictls + nn_hls
            nall_ictle(1) = nn_ictle + nn_hls
            nall_jctls(1) = nn_jctls + nn_hls
            nall_jctle(1) = nn_jctle + nn_hls
            !
         ENDIF
      ENDIF

      ! Initialization
      IF( clcomp == 'oce' ) THEN
         ALLOCATE( t_ctl(ijsplt), s_ctl(ijsplt), u_ctl(ijsplt), v_ctl(ijsplt), numprt_oce(ijsplt) )
         t_ctl(:) = 0.e0
         s_ctl(:) = 0.e0
         u_ctl(:) = 0.e0
         v_ctl(:) = 0.e0
      ENDIF
      IF( clcomp == 'top' ) THEN
         ALLOCATE( tra_ctl(kntra,ijsplt), numprt_top(ijsplt) )
         tra_ctl(:,:) = 0.e0
      ENDIF

      DO jl = 1,ijsplt

         IF( ijsplt > 1 ) WRITE(clfile_out,clfmt) 'mono.'//trim(clname)//'_', jl-1

         CALL ctl_opn( inum, clfile_out, 'REPLACE', 'FORMATTED', 'SEQUENTIAL', 1, numout, .FALSE. )
         IF( clcomp == 'oce' )   numprt_oce(jl) = inum
         IF( clcomp == 'top' )   numprt_top(jl) = inum
         WRITE(inum,*)
         WRITE(inum,*) '   CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(inum,*) '                       NEMO team'
         WRITE(inum,*) '            Ocean General Circulation Model'
         IF( clcomp == 'oce' )   WRITE(inum,*) '                NEMO version 5.0  (2024) '
         IF( clcomp == 'top' )   WRITE(inum,*) '                 TOP version 5.0  (2024) '
         WRITE(inum,*)
         IF( ijsplt > 1 )   &
            &   WRITE(inum,*) '              MPI-subdomain number: ', jl-1
         IF(  jpnij > 1 )   &
            &   WRITE(inum,*) '              MPI-subdomain number: ', narea-1
         WRITE(inum,*)
         WRITE(inum,'(19x,a20)') cl_run
         WRITE(inum,*)
         WRITE(inum,*) 'prt_ctl :  Sum control indices'
         WRITE(inum,*) '~~~~~~~'
         WRITE(inum,*)
         !
         ! clfmt2: '              ----- jctle = XXX (YYY) -----'             -> '(18x, 13a1, a9, iM, a2, iN, a2, 13a1)'
         ! clfmt3: '              |                           |'             -> '(18x, a1, Nx, a1)'
         ! clfmt4: '        ictls = XXX (YYY)           ictle = XXX (YYY)'   -> '(Nx, a9, iM, a2, iP, a2, Qx, a9, iM, a2, iP, a2)'
         !         '              |                           |'
         !         '              ----- jctle = XXX (YYY) -----'
         ! clfmt5: '   njmpp = XXX'                                          -> '(Nx, a9, iM)'
         ! clfmt6: '           nimpp = XXX'                                  -> '(Nx, a9, iM)'
         !
         idg = MAXVAL( (/ nall_ictls(jl), nall_ictle(jl), nall_jctls(jl), nall_jctle(jl) /) )   ! temporary use of idg
         idg = INT(LOG10(REAL(idg,wp))) + 1                                                     ! how many digits do we use?
         idg2 = MAXVAL( (/ mig(nall_ictls(jl),0), mig(nall_ictle(jl),0), mjg(nall_jctls(jl),0), mjg(nall_jctle(jl),0) /) )
         idg2 = INT(LOG10(REAL(idg2,wp))) + 1                                                   ! how many digits do we use?
         WRITE(clfmt2, "('(18x, 13a1, a9, i', i1, ', a2, i',i1,', a2, 13a1)')") idg, idg2
         WRITE(clfmt3, "('(18x, a1, ', i2,'x, a1)')") 13+9+idg+2+idg2+2+13 - 2
         WRITE(clfmt4, "('(', i2,'x, a9, i', i1,', a2, i', i1,', a2, ', i2,'x, a9, i', i1,', a2, i', i1,', a2)')") &
            &          18-7, idg, idg2, 13+9+idg+2+idg2+2+13 - (2+idg+2+idg2+2+8), idg, idg2
         WRITE(inum,clfmt2) ('-', ji=1,13), ' jctle = ', nall_jctle(jl), ' (', mjg(nall_jctle(jl),0), ') ', ('-', ji=1,13)
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt4)                 ' ictls = ', nall_ictls(jl), ' (', mig(nall_ictls(jl),0), ') ',   &
            &                               ' ictle = ', nall_ictle(jl), ' (', mig(nall_ictle(jl),0), ') '
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt3) '|', '|'
         WRITE(inum,clfmt2) ('-', ji=1,13), ' jctls = ', nall_jctls(jl), ' (', mjg(nall_jctls(jl),0), ') ', ('-', ji=1,13)
         WRITE(inum,*)
         WRITE(inum,*)
         !
      END DO
      !
   END SUBROUTINE prt_ctl_init


   !!======================================================================
END MODULE prtctl
