MODULE trosk
# if ! defined key_agrif
!****************************************************************
!* NUMERICAL SOLUTION OF A STIFF SYSTEM OF FIRST 0RDER ORDINARY *
!* DIFFERENTIAL EQUATIONS Y'=F(X,Y) BY ROSENBROCK METHOD.       *
!* ------------------------------------------------------------ *
!* ------------------------------------------------------------ *
!* Ref.: From Numath Library By Tuan Dang Trong in Fortran 77   *
!*       [BIBLI 18].                                            * 
!*                                                              *
!*                       F90 Release 1.0 By J-P Moreau, Paris   *
!*                                (www.jpmoreau.fr)             *
!****************************************************************
  USE timing
  USE in_out_manager, ONLY : ln_timing ! I/O manager
  USE sed
  USE sedfunc

  IMPLICIT NONE
  PRIVATE 

  PUBLIC rosk

  INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: NFCN, NJAC, NSTEP, NACCPT, NREJCT


!define example #1
  INTERFACE
       SUBROUTINE JAC(NEQ,Y,DFY,LDFY,ACCMASK)
         INTEGER, PARAMETER :: WP = KIND(1.0D0)
         INTEGER, INTENT(IN) :: NEQ, LDFY
         REAL(WP)                  , INTENT(IN)  :: Y
         REAL(WP), DIMENSION(:,:,:), INTENT(OUT) :: DFY
         INTEGER , DIMENSION(:), INTENT(IN) :: ACCMASK
       END SUBROUTINE JAC
  END INTERFACE


  CONTAINS

!**********************************************************************
SUBROUTINE rosk(ROSM,N,X,Y,XEND,H, RTOL,ATOL,                  &
           &    JAC, MLJAC, MUJAC, IDID,ISTAT)
! ---------------------------------------------------------------------
!     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
!     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS  MY'=F(X,Y).
!     THIS IS AN EMBEDDED ROSENBROCK METHOD OF ORDER (3)4
!     (WITH STEP SIZE CONTROL).
!     C.F. SECTION IV.7
!
!     AUTHORS: E. HAIRER AND G. WANNER
!              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
!              CH-1211 GENEVE 24, SWITZERLAND
!              E-MAIL:  HAIRER@CGEUGE51.BITNET,  WANNER@CGEUGE51.BITNET
!
!     THIS CODE IS PART OF THE BOOK:
!         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
!         SPRINGER-VERLAG (1990)
!
!     VERSION OF OCTOBER 12, 1990
!
!     INPUT PARAMETERS
!     ----------------
!     N           DIMENSION OF THE SYSTEM
!
!     FCN         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE
!                 VALUE OF F(X,Y):
!                    SUBROUTINE FCN(N,X,Y,F)
!                    REAL*8 X,Y(N),F(N)
!                    F(1)=...   ETC.
!
!     X           INITIAL X-VALUE
!
!     Y(N)        INITIAL VALUES FOR Y
!
!     XEND        FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
!
!     H           INITIAL STEP SIZE GUESS;
!                 FOR STIFF EQUATIONS WITH INITIAL TRANSIENT,
!                 H=1.D0/(NORM OF F'), USUALLY 1.D-2 OR 1.D-3, IS GOOD.
!                 THIS CHOICE IS NOT VERY IMPORTANT, THE CODE QUICKLY
!                 ADAPTS ITS STEP SIZE. STUDY THE CHOSEN VALUES FOR A FEW
!                 STEPS IN SUBROUTINE "SOLOUT", WHEN YOU ARE NOT SURE.
!                 (IF H=0.D0, THE CODE PUTS H=1.D-6).
!
!     RTOL,ATOL   RELATIVE AND ABSOLUTE ERROR TOLERANCES. THEY
!                 CAN BE BOTH SCALARS OR ELSE BOTH VECTORS OF LENGTH N.
!
!     JAC         NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES
!                 THE PARTIAL DERIVATIVES OF F(X,Y) WITH RESPECT TO Y
!                 FOR IJAC=1, THIS SUBROUTINE MUST HAVE THE FORM:
!                    SUBROUTINE JAC(N,X,Y,DFY,LDFY)
!                    REAL*8 X,Y(N),DFY(LDFY,N)
!                    DFY(1,1)= ...
!                 LDFY, THE COLUMN-LENGTH OF THE ARRAY, IS
!                 FURNISHED BY THE CALLING PROGRAM.
!                 THE JACOBIAN IS TAKEN AS BANDED AND
!                    THE PARTIAL DERIVATIVES ARE STORED
!                    DIAGONAL-WISE AS
!                       DFY(I-J+MUJAC+1,J) = PARTIAL F(I) / PARTIAL Y(J).
!
!     MLJAC       SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN:
!                    0<=MLJAC<N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN
!                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
!                       THE MAIN DIAGONAL).
!
!     MUJAC       UPPER BANDWITH OF JACOBIAN  MATRIX (>= NUMBER OF NON-
!                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
!                 NEED NOT BE DEFINED IF MLJAC=N.
!
!-----------------------------------------------------------------------
!
!     OUTPUT PARAMETERS
!     -----------------
!     X           X-VALUE WHERE THE SOLUTION IS COMPUTED
!                 (AFTER SUCCESSFUL RETURN X=XEND)
!
!     Y(N)        SOLUTION AT X
!
!     H           PREDICTED STEP SIZE OF THE LAST ACCEPTED STEP
!
!     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
!                   IDID=1  COMPUTATION SUCCESSFUL,
!                   IDID=-1 COMPUTATION UNSUCCESSFUL.
!
! ---------------------------------------------------------
! *** *** *** *** *** *** *** *** *** *** *** *** ***
!          DECLARATIONS
! *** *** *** *** *** *** *** *** *** *** *** *** ***
      INTEGER, INTENT(in) :: ROSM, N, MLJAC, MUJAC
      REAL(wp), DIMENSION(1), INTENT(in) :: ATOL, RTOL
      INTEGER, INTENT(inout) :: IDID
      INTEGER , DIMENSION(jpoce,3), INTENT(out) :: ISTAT
      REAL(wp), INTENT(in) :: X, XEND
      REAL(wp), INTENT(out), DIMENSION(jpoce, N) :: Y
      REAL(wp), INTENT(out), DIMENSION(jpoce) :: H

      INTEGER :: NMAX, LDJAC, LDE
      REAL(wp) :: UROUND, HMAX, FAC1, FAC2, FACREJ
      EXTERNAL JAC
! --------------------------------------------------------------------
! --- COMMON STAT CAN BE USED FOR STATISTICS
! ---    NFCN      NUMBER OF FUNCTION EVALUATIONS (THOSE FOR NUMERICAL
!                  EVALUATION OF THE JACOBIAN ARE NOT COUNTED)
! ---    NJAC      NUMBER OF JACOBIAN EVALUATIONS (EITHER ANALYTICALLY
!                  OR NUMERICALLY)
! ---    NSTEP     NUMBER OF COMPUTED STEPS
! ---    NACCPT    NUMBER OF ACCEPTED STEPS
! ---    NREJCT    NUMBER OF REJECTED STEPS (AFTER AT LEAST ONE STEP
!                  HAS BEEN ACCEPTED)
! --------------------------------------------------------------------
! *** *** *** *** *** *** ***
!    SETTING THE PARAMETERS
! *** *** *** *** *** *** ***

      IF ( ln_timing ) CALL timing_start('rosk')

      ALLOCATE (NFCN(jpoce), NJAC(jpoce), NSTEP(jpoce), NACCPT(jpoce), NREJCT(jpoce))

      NFCN=0
      NJAC=0
      NSTEP=0
      NACCPT=0
      NREJCT=0
! -------- NMAX , THE MAXIMAL NUMBER OF STEPS -----
      NMAX = 100000
! -------- UROUND   SMALLEST NUMBER SATISFYING 1.D0+UROUND>1.D0
      UROUND = 1.E-16_wp
! -------- MAXIMAL STEP SIZE
      HMAX = XEND-X
! -------  FAC1,FAC2     PARAMETERS FOR STEP SIZE SELECTION
      FAC1 = 5.0_wp
      FAC2 = 1.0_wp / 6.0_wp
! -------  FACREJ    FOR THE HUMP
      FACREJ = 0.1_wp
! *** *** *** *** *** *** *** *** *** *** *** *** ***
!         COMPUTATION OF ARRAY ENTRIES
! *** *** *** *** *** *** *** *** *** *** *** *** ***
! -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS ---
! -- JACOBIAN
      LDJAC=MLJAC+MUJAC+1
      LDE=2*MLJAC+MUJAC+1
! -------- CALL TO CORE INTEGRATOR ------------
      SELECT CASE ( ROSM )
      CASE( 4 ) 
         CALL RO4COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,JAC,        &
            MLJAC,MUJAC,IDID,                 &
            NMAX,UROUND,FAC1,FAC2,FACREJ,     &
            LDJAC,LDE )
      CASE( 3 )
         CALL RO3COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,JAC,        &
            MLJAC,MUJAC,IDID,                 &
            NMAX,UROUND,FAC1,FAC2,FACREJ,     &
            LDJAC,LDE )
      CASE( 2 )
         CALL RO2COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,JAC,        &
            MLJAC,MUJAC,IDID,                 &
            NMAX,UROUND,FAC1,FAC2,FACREJ,     &
            LDJAC,LDE )
      END SELECT
! ----------- RETURN -----------

      ISTAT(:,1) = NFCN(:)
      ISTAT(:,2) = NJAC(:)
      ISTAT(:,3) = NSTEP(:)

      DEALLOCATE (NFCN, NJAC, NSTEP, NACCPT, NREJCT )

      IF ( ln_timing ) CALL timing_stop('rosk')

      RETURN

      END SUBROUTINE rosk

      SUBROUTINE RO2COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,JAC,      &
        MLJAC,MUJAC,IDID, NMAX,UROUND,FAC1,FAC2,FACREJ,       &
        LFJAC,LE)
! ----------------------------------------------------------
!     CORE INTEGRATOR FOR ROS2
!     PARAMETERS SAME AS IN ROS2 WITH WORKSPACE ADDED
! ----------------------------------------------------------
! ----------------------------------------------------------
!         DECLARATIONS
! ----------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(1) :: RTOL,ATOL
      REAL(wp), INTENT(in) :: X,XEND,HMAX,UROUND,FAC1,FAC2,FACREJ
      REAL(wp), INTENT(out), DIMENSION(jpoce,N) :: Y
      REAL(wp), INTENT(out), DIMENSION(jpoce) :: H
      INTEGER , INTENT(in) :: N,MLJAC,MUJAC,NMAX,LFJAC,LE
      INTEGER , INTENT(out) :: IDID
      EXTERNAL JAC
      !
      REAL(wp) :: A21,C21,B1,B2,E1,E2,DGAMMA
      REAL(wp) :: FACT,S,SK
      REAL(wp), DIMENSION(jpoce,N) :: YNEW, DY1, DY, AK1, AK2
      REAL(wp), DIMENSION(jpoce,LFJAC,N) :: FJAC
      REAL(wp), DIMENSION(jpoce, LE, N)  :: E
      REAL(wp), DIMENSION(jpoce) :: HNEW, XI
      REAL(wp), DIMENSION(jpoce) :: HC21
      REAL(wp), DIMENSION(jpoce) :: ERR, FACT1
      INTEGER :: I, I1, I2, J, ji, jk, jn, js
      INTEGER :: MBJAC, MDIAG, MLE, MUE
      INTEGER, DIMENSION(jpoce,N) :: IP
      LOGICAL, DIMENSION(jpoce) :: REJECT,RJECT2
      INTEGER, DIMENSION(jpoce) :: ACCMASK, ENDMASK, ERRMASK

      IF ( ln_timing ) CALL timing_start('ro2cor')

! ---- PREPARE BANDWIDTHS -----
      MLE=MLJAC
      MUE=MUJAC
      MBJAC=MLJAC+MUJAC+1
      MDIAG=MLE+MUE+1
! *** *** *** *** *** *** ***
!  INITIALISATIONS
! *** *** *** *** *** *** ***
      CALL ROS2 (A21,C21,B1,B2,E1,E2,DGAMMA)
! --- INITIAL PREPARATIONS
      DO ji = 1, jpoce
         H(ji)=MIN(MAX(1.E-10_wp,H(ji)),HMAX)
         REJECT(ji)=.FALSE.
         XI(ji) = X
      END DO
      ERRMASK(:) = 0
      ENDMASK(:) = 0

! --- BASIC INTEGRATION STEP
   1  CONTINUE
      DO ji = 1, jpoce
         IF ( ENDMASK(ji) == 0 ) THEN
            IF (NSTEP(ji) > NMAX .OR. XI(ji)+0.1*H(ji) == XI(ji) .OR. H(ji) <= UROUND) ERRMASK(ji) = 1
            IF ((XI(ji)-XEND)+UROUND > 0.0) ENDMASK(JI) = 1
            H(ji) = MIN( H(ji), XEND-XI(ji) )
         ENDIF
      END DO

      ACCMASK(:) = ENDMASK(:)

      IF ( COUNT( ENDMASK(:) == 1 ) == jpoce ) THEN
         IF ( ln_timing ) CALL timing_stop('ro2cor')
         RETURN
      ENDIF
      IF ( COUNT( ERRMASK(:) == 1 ) > 0 ) GOTO 79

      CALL sed_func(N,Y,DY1,ACCMASK)

      WHERE ( ACCMASK(:) == 0 )
            NFCN(:)=NFCN(:)+1
            NJAC(:)=NJAC(:)+1
      END WHERE

! *** *** *** *** *** *** ***
!  COMPUTATION OF THE JACOBIAN
! *** *** *** *** *** *** ***
      CALL JAC(N,Y,FJAC,LFJAC,ACCMASK)
   2  CONTINUE

! *** *** *** *** *** *** ***
!  COMPUTE THE STAGES
! *** *** *** *** *** *** ***
      WHERE ( ACCMASK(:) == 0 )
         HC21(:)=C21/H(:)
         FACT1(:)=1.0/(H(:)*DGAMMA)
      END WHERE

      DO J=1,N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               I1=MAX(1,MUJAC+2-J)
               I2=MIN(MBJAC,N+MUJAC+1-J)
               DO I=I1,I2
                  E(ji,I+MLE,J)=-FJAC(ji,I,J)
               END DO
            ENDIF
         END DO
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               E(ji,MDIAG,J)=E(ji,MDIAG,J)+FACT1(ji)
            ENDIF
         END DO
      END DO
      CALL DECB(N,LE,E,MLE,MUE,IP,ACCMASK)

! --- THIS PART COMPUTES THE STAGES IN THE CASE WHERE
! ---   1) THE DIFFERENTIAL EQUATION IS IN EXPLICIT FORM
! ---   2) THE JACOBIAN OF THE PROBLEM IS A BANDED MATRIX
! ---   3) THE DIFFERENTIAL EQUATION IS AUTONOMOUS
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK1(ji,I)=DY1(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK1,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A21*AK1(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK2(ji,I)=DY(ji,I)+HC21(ji)*AK1(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK2,IP,ACCMASK)

      WHERE ( ACCMASK(:) == 0 )
! *** *** *** *** *** *** ***
!  ERROR ESTIMATION
! *** *** *** *** *** *** ***
         NFCN(:) = NFCN(:) + 1
         NSTEP(:)= NSTEP(:) + 1
         ERR(:)  = 0.0
      END WHERE

      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
! ------------ NEW SOLUTION ---------------
               YNEW(ji,I)=Y(ji,I)+B1*AK1(ji,I)+B2*AK2(ji,I)
! ------------ COMPUTE ERROR ESTIMATION ----------------
               S = E1*AK1(ji,I)+E2*AK2(ji,I)
               SK = ATOL(I)+RTOL(I)*MAX(ABS(Y(ji,I)),ABS(YNEW(ji,I)))
               ERR(ji) = ERR(ji)+(S/SK)**2
            ENDIF
         END DO
      END DO

      CALL sed_func(N,YNEW,DY,ACCMASK)

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            ERR(ji) = SQRT(ERR(ji)/N)
! --- COMPUTATION OF HNEW
! --- WE REQUIRE .2<=HNEW/H<=6.
            FACT  = MAX(FAC2,MIN(FAC1,(ERR(ji))**.5/.9))
            HNEW(ji) = H(ji)/FACT

! *** *** *** *** *** *** ***
!  IS THE ERROR SMALL ENOUGH ?
! *** *** *** *** *** *** ***
            RJECT2(ji) = .TRUE.
        ENDIF
      END DO

      DO jn = 1, jpvode
         js = jsvode(jn)
         IF (js <= jpwat) THEN
            DO ji = 1, jpoce
               IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
                  xirrigtrd(ji,js) = xirrigtrd(ji,js) + xirrigtrdtmp(ji,js) * H(ji)
               ENDIF
            END DO
         ENDIF
      END DO

      DO jk = 1, jpksed
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
               pwcpaa(ji,jk,jwalk) = pwcpaa(ji,jk,jwalk) + pwcpa(ji,jk,jwalk) * H(ji) / dtsed
               pwcpaa(ji,jk,jwpo4) = pwcpaa(ji,jk,jwpo4) + pwcpa(ji,jk,jwpo4) * H(ji) / dtsed
            ENDIF
         END DO
      END DO


      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
               Y(ji,I) = YNEW(ji,I)
            ENDIF
         END DO
      END DO

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            IF (ERR(ji) <= 1.0) THEN
! --- STEP IS ACCEPTED
               NACCPT(ji) = NACCPT(ji)+1
               XI(ji) = XI(ji)+H(ji)
               IF (HNEW(ji) > HMAX) HNEW(ji)=HMAX
               IF (REJECT(ji)) HNEW(ji)=MIN(HNEW(ji),H(ji))
               REJECT(ji) = .FALSE.
               RJECT2(ji) = .FALSE.
               H(ji) = HNEW(ji)
               ACCMASK(ji) = 1
            ELSE
! --- STEP IS REJECTED
               IF (RJECT2(ji)) HNEW(ji)   = H(ji)*FACREJ
               RJECT2(ji) = REJECT(ji)
               REJECT(ji) = .TRUE.
               H(ji)=HNEW(ji)
               IF (NACCPT(ji) >= 1) NREJCT(ji) = NREJCT(ji)+1
            END IF
         ENDIF
      END DO
      IF (COUNT( ACCMASK(:) == 0 ) > 0 ) GOTO 2
      GOTO 1
! --- EXIT
 79   CONTINUE
 979  FORMAT(' EXIT OF ROS2 AT X=',D16.7,'   H=',D16.7)
      IDID=-1

      IF ( ln_timing ) CALL timing_stop('ro2cor')

      RETURN
      END SUBROUTINE RO2COR

      SUBROUTINE RO3COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,JAC,         &
        MLJAC,MUJAC,IDID, NMAX,UROUND,FAC1,FAC2,FACREJ,       &
        LFJAC,LE)
! ----------------------------------------------------------
!     CORE INTEGRATOR FOR RODAS3
!     PARAMETERS SAME AS IN RODAS3 WITH WORKSPACE ADDED
! ----------------------------------------------------------
! ----------------------------------------------------------
!         DECLARATIONS
! ----------------------------------------------------------     
      REAL(wp), INTENT(in), DIMENSION(1) :: RTOL,ATOL
      REAL(wp), INTENT(in) :: X,XEND,HMAX,UROUND,FAC1,FAC2,FACREJ
      REAL(wp), INTENT(out), DIMENSION(jpoce,N) :: Y
      REAL(wp), INTENT(out), DIMENSION(jpoce) :: H
      INTEGER , INTENT(in) :: N,MLJAC,MUJAC,NMAX,LFJAC,LE
      INTEGER , INTENT(out) :: IDID
      EXTERNAL JAC
      !
      REAL(wp) :: A21,A31,A32,A41,A42,A43,C21,C31,C32,C41,C42,C43,  &
     &            B1,B2,B3,B4,E1,E2,E3,E4,DGAMMA
      REAL(wp) :: FACT,S,SK
      REAL(wp), DIMENSION(jpoce,N) :: YNEW, DY1, DY, AK1, AK2, AK3, AK4
      REAL(wp), DIMENSION(jpoce,LFJAC,N) :: FJAC
      REAL(wp), DIMENSION(jpoce, LE, N)  :: E
      REAL(wp), DIMENSION(jpoce) :: HNEW, XI
      REAL(wp), DIMENSION(jpoce) :: HC21, HC31, HC32, HC41, HC42, HC43
      REAL(wp), DIMENSION(jpoce) :: ERR, FACT1
      INTEGER :: I, I1, I2, J, ji, jk, jn, js
      INTEGER :: MBJAC, MDIAG, MLE, MUE
      INTEGER, DIMENSION(jpoce,N) :: IP
      LOGICAL, DIMENSION(jpoce) :: REJECT,RJECT2
      INTEGER, DIMENSION(jpoce) :: ACCMASK, ENDMASK, ERRMASK

      IF ( ln_timing ) CALL timing_start('ro3cor')

! ---- PREPARE BANDWIDTHS -----
       MLE=MLJAC
       MUE=MUJAC
       MBJAC=MLJAC+MUJAC+1
       MDIAG=MLE+MUE+1
! *** *** *** *** *** *** ***
!  INITIALISATIONS
! *** *** *** *** *** *** ***
      CALL RODAS3 (A21,A31,A32,A41,A42,A43,C21,C31,C32,C41,C42,C43,  &
                B1,B2,B3,B4,E1,E2,E3,E4,DGAMMA)

! --- INITIAL PREPARATIONS
      DO ji = 1, jpoce
         H(ji)=MIN(MAX(1.E-10_wp,H(ji)),HMAX)
         REJECT(ji)=.FALSE.
         XI(ji) = X
      END DO
      ERRMASK(:) = 0
      ENDMASK(:) = 0

! --- BASIC INTEGRATION STEP
   1  CONTINUE

      DO ji = 1, jpoce
         IF ( ENDMASK(ji) == 0 ) THEN
            IF (NSTEP(ji) > NMAX .OR. XI(ji)+0.1*H(ji) == XI(ji) .OR. H(ji) <= UROUND) ERRMASK(ji) = 1
            IF ((XI(ji)-XEND)+UROUND > 0.0) ENDMASK(JI) = 1
            H(ji) = MIN( H(ji), XEND-XI(ji) )
         ENDIF
      END DO

      ACCMASK(:) = ENDMASK(:)

      IF ( COUNT( ENDMASK(:) == 1 ) == jpoce ) THEN
         IF ( ln_timing ) CALL timing_stop('ro3cor')
         RETURN
      ENDIF
      IF ( COUNT( ERRMASK(:) == 1 ) > 0 ) GOTO 79

      CALL sed_func(N,Y,DY1,ACCMASK)

      WHERE ( ACCMASK(:) == 0 ) 
            NFCN(:)=NFCN(:)+1
            NJAC(:)=NJAC(:)+1
      END WHERE

! *** *** *** *** *** *** ***
!  COMPUTATION OF THE JACOBIAN
! *** *** *** *** *** *** ***
      CALL JAC(N,Y,FJAC,LFJAC,ACCMASK)
! --- JACOBIAN IS BANDED
!      MUJACP=MUJAC+1
!      MD=MIN(MBJAC,N)
!      DO 16 K=1,MD
!      J=K
! 12   AK2(:,J)=Y(:,J)
!      AK3(:,J)=DSQRT(UROUND*MAX(1.D-5,ABS(Y(:,J))))
!      Y(:,J)=Y(:,J)+AK3(:,J)
!      J=J+MD
!      IF (J.LE.N) GOTO 12
!      CALL sed_func(N,Y,AK1,ACCMASK)
!      J=K
!      LBEG=MAX(1,J-MUJAC)
! 14   LEND=MIN(N,J+MLJAC)
!      Y(:,J)=AK2(:,J)
!      MUJACJ=MUJACP-J
!      DO L=LBEG,LEND
!         FJAC(:,L+MUJACJ,J)=(AK1(:,L)-DY1(:,L))/AK3(:,J)
!      END DO
!      J=J+MD
!      LBEG=LEND+1
!      IF (J.LE.N) GOTO 14
! 16   CONTINUE
   2  CONTINUE

! *** *** *** *** *** *** ***
!  COMPUTE THE STAGES
! *** *** *** *** *** *** ***
      WHERE ( ACCMASK(:) == 0 )
         HC21(:)=C21/H(:)
         HC31(:)=C31/H(:)
         HC32(:)=C32/H(:)
         HC41(:)=C41/H(:)
         HC42(:)=C42/H(:)
         HC43(:)=C43/H(:)
         FACT1(:)=1.0/(H(:)*DGAMMA)
      END WHERE

! --- THE MATRIX E (B=IDENTITY, JACOBIAN A BANDED MATRIX)
!            FACT=1.0/(H(ji)*DGAMMA)
      DO J=1,N
         DO I=1,MBJAC
            DO ji = 1, jpoce
               IF (ACCMASK(ji) == 0) THEN
                  E(ji,I+MLE,J)=-FJAC(ji,I,J)
               ENDIF
            END DO
         END DO
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               E(ji,MDIAG,J)=E(ji,MDIAG,J)+FACT1(ji)
            ENDIF
         END DO
      END DO

      CALL DECB(N,LE,E,MLE,MUE,IP,ACCMASK)


! --- THIS PART COMPUTES THE STAGES IN THE CASE WHERE
! ---   1) THE DIFFERENTIAL EQUATION IS IN EXPLICIT FORM
! ---   2) THE JACOBIAN OF THE PROBLEM IS A BANDED MATRIX
! ---   3) THE DIFFERENTIAL EQUATION IS AUTONOMOUS
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK1(ji,I)=DY1(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK1,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK2(ji,I)=DY1(ji,I)+HC21(ji)*AK1(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK2,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A31*AK1(ji,I)+A32*AK2(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK3(ji,I)=DY(ji,I)+HC31(ji)*AK1(ji,I)+HC32(ji)*AK2(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK3,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A41*AK1(ji,I)+A42*AK2(ji,I)+A43*AK3(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK4(ji,I)=DY(ji,I)+HC41(ji)*AK1(ji,I)+HC42(ji)*AK2(ji,I)+HC43(ji)*AK3(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK4,IP,ACCMASK)

      WHERE ( ACCMASK(:) == 0 )
! *** *** *** *** *** *** ***
!  ERROR ESTIMATION
! *** *** *** *** *** *** ***
         NFCN(:) = NFCN(:) + 2
         NSTEP(:)=NSTEP(:)+1
         ERR(:) = 0.0
      END WHERE

      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
! ------------ NEW SOLUTION ---------------
               YNEW(ji,I)=Y(ji,I)+B1*AK1(ji,I)+B2*AK2(ji,I)+B3*AK3(ji,I)+B4*AK4(ji,I)
! ------------ COMPUTE ERROR ESTIMATION ----------------
               S = E1*AK1(ji,I)+E2*AK2(ji,I)+E3*AK3(ji,I)+E4*AK4(ji,I)
               SK = ATOL(I)+RTOL(I)*MAX(ABS(Y(ji,I)),ABS(YNEW(ji,I)))
               ERR(ji) = ERR(ji)+(S/SK)**2
            ENDIF
         END DO
      END DO

      CALL sed_func(N,YNEW,DY,ACCMASK)

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            ERR(ji) = SQRT(ERR(ji)/N)
! --- COMPUTATION OF HNEW
! --- WE REQUIRE .2<=HNEW/H<=6.
            FACT  = MAX(FAC2,MIN(FAC1,ERR(ji)**(1.0/3.0)/.9))
            HNEW(ji) = H(ji)/FACT
! *** *** *** *** *** *** ***
!  IS THE ERROR SMALL ENOUGH ?
! *** *** *** *** *** *** ***
            RJECT2(ji) = .TRUE.
         ENDIF
      END DO

      DO jn = 1, jpvode
         js = jsvode(jn)
         IF (js <= jpwat) THEN
            DO ji = 1, jpoce
               IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
                  xirrigtrd(ji,js) = xirrigtrd(ji,js) + xirrigtrdtmp(ji,js) * H(ji)
               ENDIF
            END DO
         ENDIF
      END DO

      DO jk = 1, jpksed
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
               pwcpaa(ji,jk,jwalk) = pwcpaa(ji,jk,jwalk) + pwcpa(ji,jk,jwalk) * H(ji) / dtsed
               pwcpaa(ji,jk,jwpo4) = pwcpaa(ji,jk,jwpo4) + pwcpa(ji,jk,jwpo4) * H(ji) / dtsed
            ENDIF
         END DO
      END DO

      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
               Y(ji,I) = YNEW(ji,I)
            ENDIF
         END DO
      END DO 

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            IF (ERR(ji) <= 1.0) THEN
! --- STEP IS ACCEPTED
               NACCPT(ji) = NACCPT(ji)+1
               XI(ji) = XI(ji)+H(ji)
               IF (HNEW(ji) > HMAX) HNEW(ji)=HMAX
               IF (REJECT(ji)) HNEW(ji)=MIN(HNEW(ji),H(ji))
               REJECT(ji) = .FALSE.
               RJECT2(ji) = .FALSE.
               H(ji) = HNEW(ji)
               ACCMASK(ji) = 1
            ELSE
! --- STEP IS REJECTED
               IF (RJECT2(ji)) HNEW(ji)   = H(ji)*FACREJ
               RJECT2(ji) = REJECT(ji)
               REJECT(ji) = .TRUE.
               H(ji)=HNEW(ji)
               IF (NACCPT(ji) >= 1) NREJCT(ji) = NREJCT(ji)+1
            END IF
         ENDIF
      END DO
      IF (COUNT( ACCMASK(:) == 0 ) > 0 ) GOTO 2
      GOTO 1
! --- EXIT
 79   CONTINUE
 979  FORMAT(' EXIT OF RODAS3 AT X=',D16.7,'   H=',D16.7)
      IDID=-1

      IF ( ln_timing ) CALL timing_stop('ro3cor')

      RETURN

      END SUBROUTINE RO3COR

      SUBROUTINE RO4COR(N,X,Y,XEND,HMAX,H,RTOL,ATOL,JAC,         &
        MLJAC,MUJAC,IDID, NMAX,UROUND,FAC1,FAC2,FACREJ,       &
        LFJAC,LE)
! ----------------------------------------------------------
!     CORE INTEGRATOR FOR ROS4
!     PARAMETERS SAME AS IN ROS4 WITH WORKSPACE ADDED
! ----------------------------------------------------------
! ----------------------------------------------------------
!         DECLARATIONS
! ----------------------------------------------------------
      REAL(wp), INTENT(in), DIMENSION(1) :: RTOL,ATOL
      REAL(wp), INTENT(in) :: X,XEND,HMAX,UROUND,FAC1,FAC2,FACREJ
      REAL(wp), INTENT(out), DIMENSION(jpoce,N) :: Y
      REAL(wp), INTENT(out), DIMENSION(jpoce) :: H
      INTEGER , INTENT(in) :: N,MLJAC,MUJAC,NMAX,LFJAC,LE
      INTEGER , INTENT(out) :: IDID
      EXTERNAL JAC
      !
      REAL(wp) :: A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,A61,A62,A63,  &
         &        A64,A65,C21,C31,C32,C41,C42,C43,C51,C52,C53,C54,C61,C62,C63,  &
         &        C64,C65,B1,B2,B3,B4,B5,B6,E1,E2,E3,E4,E5,E6,DGAMMA
      REAL(wp) :: FACT,S,SK
      REAL(wp), DIMENSION(jpoce,N) :: YNEW, DY1, DY, AK1, AK2, AK3, AK4, AK5, AK6
      REAL(wp), DIMENSION(jpoce,LFJAC,N) :: FJAC
      REAL(wp), DIMENSION(jpoce, LE, N)  :: E
      REAL(wp), DIMENSION(jpoce) :: HNEW, XI
      REAL(wp), DIMENSION(jpoce) :: HC21, HC31, HC32, HC41, HC42, HC43
      REAL(wp), DIMENSION(jpoce) :: HC51, HC52, HC53, HC54, HC61, HC62
      REAL(wp), DIMENSION(jpoce) :: HC63, HC64, HC65
      REAL(wp), DIMENSION(jpoce) :: ERR, FACT1
      INTEGER :: I, I1, I2, J, ji, jk, jn, js
      INTEGER :: MBJAC, MDIAG, MLE, MUE
      INTEGER, DIMENSION(jpoce,N) :: IP
      LOGICAL, DIMENSION(jpoce) :: REJECT,RJECT2
      INTEGER, DIMENSION(jpoce) :: ACCMASK, ENDMASK, ERRMASK
! ---- PREPARE BANDWIDTHS -----
       MLE   = MLJAC
       MUE   = MUJAC
       MBJAC = MLJAC+MUJAC+1
       MDIAG = MLE+MUE+1
! *** *** *** *** *** *** ***
!  INITIALISATIONS
! *** *** *** *** *** *** ***
      CALL RODAS4(A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,A61,A62,A63,  &
                A64,A65,C21,C31,C32,C41,C42,C43,C51,C52,C53,C54,C61,C62,C63,  &
                C64,C65,B1,B2,B3,B4,B5,B6,E1,E2,E3,E4,E5,E6,DGAMMA)

! --- INITIAL PREPARATIONS
      DO ji = 1, jpoce
         H(ji) = MIN(MAX(1.E-10_wp,H(ji)),HMAX)
         REJECT(ji) = .FALSE.
         XI(ji) = X
      END DO
      ERRMASK(:) = 0
      ENDMASK(:) = 0

! --- BASIC INTEGRATION STEP
   1  CONTINUE
      DO ji = 1, jpoce
         IF ( ENDMASK(ji) == 0 ) THEN
            IF (NSTEP(ji) > NMAX .OR. XI(ji)+0.1*H(ji) == XI(ji) .OR. H(ji) <= UROUND) ERRMASK(ji) = 1
            IF ((XI(ji)-XEND)+UROUND > 0.0) ENDMASK(JI) = 1
            H(ji) = MIN( H(ji), XEND-XI(ji) )
         ENDIF
      END DO

      ACCMASK(:) = ENDMASK(:)

      IF ( COUNT( ENDMASK(:) == 1 ) == jpoce ) THEN
         IF ( ln_timing ) CALL timing_stop('ro4cor')
         RETURN
      ENDIF
      IF ( COUNT( ERRMASK(:) == 1 ) > 0 ) GOTO 79

      CALL sed_func(N,Y,DY1,ACCMASK)

      WHERE ( ACCMASK(:) == 0 )
            NFCN(:)=NFCN(:)+1
            NJAC(:)=NJAC(:)+1
      END WHERE

! *** *** *** *** *** *** ***
!  COMPUTATION OF THE JACOBIAN
! *** *** *** *** *** *** ***
      CALL JAC(N,Y,FJAC,LFJAC,ACCMASK)
   2  CONTINUE
! *** *** *** *** *** *** ***
!  COMPUTE THE STAGES
! *** *** *** *** *** *** ***
      WHERE ( ACCMASK(:) == 0 )
         HC21(:)=C21/H(:)
         HC31(:)=C31/H(:)
         HC32(:)=C32/H(:)
         HC41(:)=C41/H(:)
         HC42(:)=C42/H(:)
         HC43(:)=C43/H(:)
         HC51(:)=C51/H(:)
         HC52(:)=C52/H(:)
         HC53(:)=C53/H(:)
         HC54(:)=C54/H(:)
         HC61(:)=C61/H(:)
         HC62(:)=C62/H(:)
         HC63(:)=C63/H(:)
         HC64(:)=C64/H(:)
         HC65(:)=C65/H(:)
         FACT1(:)=1.0/(H(:)*DGAMMA)
      END WHERE

! --- THE MATRIX E (B=IDENTITY, JACOBIAN A BANDED MATRIX)
      DO J=1,N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               I1=MAX(1,MUJAC+2-J)
               I2=MIN(MBJAC,N+MUJAC+1-J)
               DO I=I1,I2
                  E(ji,I+MLE,J)=-FJAC(ji,I,J)
               END DO
            ENDIF
         END DO
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               E(ji,MDIAG,J)=E(ji,MDIAG,J)+FACT1(ji)
            ENDIF
         END DO
      END DO
      CALL DECB(N,LE,E,MLE,MUE,IP,ACCMASK)

! --- THIS PART COMPUTES THE STAGES IN THE CASE WHERE
! ---   1) THE DIFFERENTIAL EQUATION IS IN EXPLICIT FORM
! ---   2) THE JACOBIAN OF THE PROBLEM IS A BANDED MATRIX
! ---   3) THE DIFFERENTIAL EQUATION IS AUTONOMOUS
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK1(ji,I)=DY1(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK1,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A21*AK1(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK2(ji,I)=DY(ji,I)+HC21(ji)*AK1(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK2,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A31*AK1(ji,I)+A32*AK2(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK3(ji,I)=DY(ji,I)+HC31(ji)*AK1(ji,I)+HC32(ji)*AK2(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK3,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A41*AK1(ji,I)+A42*AK2(ji,I)+A43*AK3(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK4(ji,I)=DY(ji,I)+HC41(ji)*AK1(ji,I)+HC42(ji)*AK2(ji,I)+HC43(ji)*AK3(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK4,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A51*AK1(ji,I)+A52*AK2(ji,I)+A53*AK3(ji,I)+A54*AK4(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK5(ji,I)=DY(ji,I)+HC51(ji)*AK1(ji,I)+HC52(ji)*AK2(ji,I)+HC53(ji)*AK3(ji,I)+HC54(ji)*AK4(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK5,IP,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               YNEW(ji,I)=Y(ji,I)+A61*AK1(ji,I)+A62*AK2(ji,I)+A63*AK3(ji,I)+A64*AK4(ji,I)+A65*AK5(ji,I)
            ENDIF
         END DO
      END DO
      CALL sed_func(N,YNEW,DY,ACCMASK)
      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               AK6(ji,I)=DY(ji,I)+HC61(ji)*AK1(ji,I)+HC62(ji)*AK2(ji,I)+HC63(ji)*AK3(ji,I)+HC64(ji)*AK4(ji,I)   &
               &         + HC65(ji)*AK5(ji,I)
            ENDIF
         END DO
      END DO
      CALL SOLB(N,LE,E,MLE,MUE,AK6,IP,ACCMASK)

      WHERE ( ACCMASK(:) == 0 )
! *** *** *** *** *** *** ***
!  ERROR ESTIMATION
! *** *** *** *** *** *** ***
         NFCN(:) = NFCN(:) + 5
         NSTEP(:)= NSTEP(:) + 1
         ERR(:)  = 0.0
      END WHERE

      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
! ------------ NEW SOLUTION ---------------
               YNEW(ji,I)=Y(ji,I)+B1*AK1(ji,I)+B2*AK2(ji,I)+B3*AK3(ji,I)+B4*AK4(ji,I)+B5*AK5(ji,I)+B6*AK6(ji,I)
! ------------ COMPUTE ERROR ESTIMATION ----------------
               S = E1*AK1(ji,I)+E2*AK2(ji,I)+E3*AK3(ji,I)+E4*AK4(ji,I)+E5*AK5(ji,I)+E6*AK6(ji,I)
               SK = ATOL(I)+RTOL(I)*MAX(ABS(Y(ji,I)),ABS(YNEW(ji,I)))
               ERR(ji) = ERR(ji)+(S/SK)**2
            ENDIF
         END DO
      END DO

      CALL sed_func(N,YNEW,DY,ACCMASK)

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            ERR(ji) = SQRT(ERR(ji)/N)
! --- COMPUTATION OF HNEW
! --- WE REQUIRE .2<=HNEW/H<=6.
            FACT  = MAX(FAC2,MIN(FAC1,(ERR(ji))**0.25/0.9))
            HNEW(ji) = H(ji)/FACT
! *** *** *** *** *** *** ***
!  IS THE ERROR SMALL ENOUGH ?
! *** *** *** *** *** *** ***
            RJECT2(ji) = .TRUE.
         ENDIF
      END DO

      DO jn = 1, jpvode
         js = jsvode(jn)
         IF (js <= jpwat) THEN
            DO ji = 1, jpoce
               IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
                  xirrigtrd(ji,js) = xirrigtrd(ji,js) + xirrigtrdtmp(ji,js) * H(ji)
               ENDIF
            END DO
         ENDIF
      END DO

      DO jk = 1, jpksed
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
               pwcpaa(ji,jk,jwalk) = pwcpaa(ji,jk,jwalk) + pwcpa(ji,jk,jwalk) * H(ji) / dtsed
               pwcpaa(ji,jk,jwpo4) = pwcpaa(ji,jk,jwpo4) + pwcpa(ji,jk,jwpo4) * H(ji) / dtsed
            ENDIF
         END DO
      END DO

      DO I = 1, N
         DO ji = 1, jpoce
            IF (ACCMASK(ji) == 0 .AND. ERR(ji) <= 1.0) THEN
               Y(ji,I) = YNEW(ji,I)
            ENDIF
         END DO
      END DO

      DO ji = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            IF (ERR(ji) <= 1.0) THEN
! --- STEP IS ACCEPTED
               NACCPT(ji) = NACCPT(ji) + 1
               XI(ji) = XI(ji)+H(ji)
               IF (HNEW(ji) > HMAX) HNEW(ji)=HMAX
               IF (REJECT(ji)) HNEW(ji)=MIN(HNEW(ji),H(ji))
               REJECT(ji) = .FALSE.
               RJECT2(ji) = .FALSE.
               H(ji) = HNEW(ji)
               ACCMASK(ji) = 1
            ELSE
! --- STEP IS REJECTED
               IF (RJECT2(ji)) HNEW(ji)=H(ji)*FACREJ
               RJECT2(ji) = REJECT(ji)
               REJECT(ji) = .TRUE.
               H(ji) = HNEW(ji)
               IF (NACCPT(ji) >= 1) NREJCT(ji)=NREJCT(ji)+1
            END IF
         ENDIF
      END DO
      IF (COUNT( ACCMASK(:) == 0 ) > 0 ) GOTO 2
      GOTO 1
! --- EXIT
 79   CONTINUE
 979  FORMAT(' EXIT OF ROS4 AT X=',D16.7,'   H=',D16.7)
      IDID=-1
      RETURN

      END SUBROUTINE RO4COR

      SUBROUTINE ROS2 (A21,C21,B1,B2,E1,E2,DGAMMA)
      REAL(wp), INTENT(out) :: A21, C21
      REAL(wp), INTENT(out) :: B1, B2, E1, E2
      REAL(wp), INTENT(out) :: DGAMMA

         DGAMMA= 1.0 + 1.0/SQRT(2.)
         A21=1._wp/DGAMMA
         C21=-2._wp/DGAMMA
         B1=3./2./DGAMMA
         B2=1./2./DGAMMA
         E1=1./2./DGAMMA
         E2=1./2./DGAMMA

      RETURN
      END SUBROUTINE ROS2

      SUBROUTINE RODAS3 (A21,A31,A32,A41,A42,A43,C21,C31,C32,C41,C42,C43,  &
                B1,B2,B3,B4,E1,E2,E3,E4,DGAMMA)

      REAL(wp), INTENT(out) :: A21, A31, A32, A41, A42, A43, C21, C31
      REAL(wp), INTENT(out) :: C32, C41, C42, C43, B1, B2, B3, B4, E1
      REAL(wp), INTENT(out) :: E2, E3, E4, DGAMMA

         A21= 0.0
         A31= 2.0
         A32= 0.0
         A41= 2.0
         A42= 0.0
         A43= 1.0
         C21= 4.0
         C31= 1.0
         C32=-1.0
         C41= 1.0
         C42=-1.0
         C43=-8.0/3.0
         B1= 2.0
         B2= 0.0
         B3= 1.0
         B4= 1.0
         E1= 0.0
         E2= 0.0
         E3= 0.0
         E4= 1.0
         DGAMMA= 0.5
      RETURN
      END SUBROUTINE RODAS3

      SUBROUTINE RODAS4(A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,A61,A62,A63,  &
                A64,A65,C21,C31,C32,C41,C42,C43,C51,C52,C53,C54,C61,C62,C63,  &
                C64,C65,B1,B2,B3,B4,B5,B6,E1,E2,E3,E4,E5,E6,DGAMMA)

      REAL(wp), INTENT(out) :: A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,A61
      REAL(wp), INTENT(out) :: A62,A63,A64,A65,C21,C31,C32,C41,C42,C43,C51
      REAL(wp), INTENT(out) :: C52,C53,C54,C61,C62,C63,C64,C65,B1,B2,B3,B4,B5
      REAL(wp), INTENT(out) :: B6,E1,E2,E3,E4,E5,E6,DGAMMA

      A21 = 0.1544000000000000E+01
      A31 = 0.9466785280815826
      A32 = 0.2557011698983284
      A41 = 0.3314825187068521E+01
      A42 = 0.2896124015972201E+01
      A43 = 0.9986419139977817
      A51 = 0.1221224509226641E+01
      A52 = 0.6019134481288629E+01
      A53 = 0.1253708332932087E+02
      A54 =-0.6878860361058950
      A61 = A51
      A62 = A52
      A63 = A53
      A64 = A54
      A65 = 1.0
      C21 =-0.5668800000000000E+01
      C31 =-0.2430093356833875E+01
      C32 =-0.2063599157091915
      C41 =-0.1073529058151375
      C42 =-0.9594562251023355E+01
      C43 =-0.2047028614809616E+02
      C51 = 0.7496443313967647E+01
      C52 =-0.1024680431464352E+02
      C53 =-0.3399990352819905E+02
      C54 = 0.1170890893206160E+02
      C61 = 0.8083246795921522E+01
      C62 =-0.7981132988064893E+01
      C63 =-0.3152159432874371E+02
      C64 = 0.1631930543123136E+02
      C65 =-0.6058818238834054E+01
      B1 = A51
      B2 = A52
      B3 = A53
      B4 = A54
      B5 = 1.0
      B6 = 1.0
      E1 = 0.0
      E2 = 0.0
      E3 = 0.0
      E4 = 0.0
      E5 = 0.0
      E6 = 1.0
      DGAMMA= 0.25
      RETURN
      END SUBROUTINE RODAS4
!
      SUBROUTINE DECB (N, NDIM, A, ML, MU, IP, ACCMASK)
      IMPLICIT INTEGER (I-N)
      INTEGER, INTENT(in) :: ML, MU, N, NDIM
      REAL(wp), DIMENSION(jpoce,NDIM,N), INTENT(inout) :: A
      INTEGER, DIMENSION(jpoce, N), INTENT(out) :: IP
      INTEGER, DIMENSION(jpoce), INTENT(in) :: ACCMASK
      REAL(wp) :: T
      INTEGER :: JI
      INTEGER, DIMENSION(jpoce) :: JU, M1, MM
      REAL(wp), DIMENSION(jpoce) :: T1
!-----------------------------------------------------------------------
!  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED
!  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU
!  INPUT..
!     N       ORDER OF THE ORIGINAL MATRIX A.
!     NDIM    DECLARED DIMENSION OF ARRAY  A.
!     A       CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS
!                OF THE MATRIX ARE STORED IN THE COLUMNS OF  A  AND
!                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
!                ML+1 THROUGH 2*ML+MU+1 OF  A.
!     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!  OUTPUT..
!     A       AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
!                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
!     IP      INDEX VECTOR OF PIVOT INDICES.
!     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O .
!  USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
!  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1.
!  IF IP(N)=O, A IS SINGULAR, SOLB WILL DIVIDE BY ZERO.
!
!  REFERENCE..
!     THIS IS A MODIFICATION OF
!     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
!     C.A.C.M. 15 (1972), P. 274.
!-----------------------------------------------------------------------
    IF ( ln_timing ) CALL timing_start('decb')

    MD = ML + MU + 1
    MD1 = MD + 1
    MDM = MD - 1
    NM1 = N - 1
    IP(:,N) = 1
    DO JI = 1, jpoce
       IF (ACCMASK(ji) == 0) THEN
          A(ji,1:ML,MU+2:N) = 0.0
       ENDIF
    END DO

    JU(:) = 0
    DO K = 1, NM1
       KP1 = K + 1
       M1(:) = MD
       MDL = MIN(ML,N-K) + MD
       DO JI = 1, jpoce
          IF (ACCMASK(ji) == 0) THEN
             M1(ji) = MDM + MAXLOC(ABS(A(ji,MD:MDL,K)),DIM=1)
          ENDIF
       END DO

       DO JI = 1, jpoce
          IF (ACCMASK(ji) == 0) THEN
             IP(ji,K) = M1(ji) + K - MD
             T = A(ji,M1(ji),K)
             IF (M1(ji) /= MD) THEN
                IP(ji,N) = -IP(ji,N)
                A(ji,M1(ji),K) = A(ji,MD,K)
                A(ji,MD,K) = T
             ENDIF
             T1(ji) = 1.0/T
             JU(ji) = MIN(MAX(JU(ji),MU+IP(ji,K)),N)
             MM(ji) = MD
          ENDIF
       END DO

       DO JI = 1, jpoce
          IF (ACCMASK(ji) == 0) THEN
             DO I = MD1, MDL
                A(ji,I,K) = -A(ji,I,K)*T1(ji)
             END DO
          ENDIF
       END DO

       DO JI = 1, jpoce
          IF (ACCMASK(ji) == 0 .AND. JU(ji) >= KP1) THEN
             DO J = KP1, JU(ji)
                M1(ji) = M1(ji) - 1
                MM(ji) = MM(ji) - 1
                T = A(ji,M1(ji),J)
                IF (M1(ji) /= MM(ji)) THEN
                   A(ji,M1(ji),J) = A(ji,MM(ji),J)
                   A(ji,MM(ji),J) = T
                ENDIF
                IF (T /= 0.0) THEN
                   JK = J - K
                   DO I = MD1, MDL
                      IJK = I - JK
                      A(ji,IJK,J) = A(ji,IJK,J) + A(ji,I,K)*T
                   END DO
                ENDIF
             END DO
          ENDIF
       END DO
    END DO
 80 CONTINUE

    IF ( ln_timing ) CALL timing_stop('decb')

    RETURN
!----------------------- END OF SUBROUTINE DECB ------------------------
    END SUBROUTINE DECB

      SUBROUTINE SOLB (N, NDIM, A, ML, MU, B, IP, ACCMASK)
      IMPLICIT INTEGER (I-N)
      REAL(wp) :: T
      INTEGER, INTENT(in) :: N, NDIM, ML, MU
      REAL(wp), DIMENSION(jpoce,NDIM,N), INTENT(in) :: A
      REAL(wp), DIMENSION(jpoce,N), INTENT(inout)   :: B
      INTEGER, DIMENSION(jpoce,N), INTENT(in)       :: IP 
      INTEGER, DIMENSION(jpoce), INTENT(in)         :: ACCMASK
!-----------------------------------------------------------------------
!  SOLUTION OF LINEAR SYSTEM, A*X = B .
!  INPUT..
!    N      ORDER OF MATRIX A.
!    NDIM   DECLARED DIMENSION OF ARRAY  A .
!    A      TRIANGULARIZED MATRIX OBTAINED FROM DECB.
!    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
!    B      RIGHT HAND SIDE VECTOR.
!    IP     PIVOT VECTOR OBTAINED FROM DECB.
!  OUTPUT..
!    B      SOLUTION VECTOR, X .
!-----------------------------------------------------------------------

      IF ( ln_timing ) CALL timing_start('solb')

      MD = ML + MU + 1
      MD1 = MD + 1
      MDM = MD - 1
      NM1 = N - 1
      DO K = 1, NM1
         MDL = MIN(ML,N-K) + MD
         DO JI = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               M = IP(ji,K)
               T = B(ji,M)
               B(ji,M) = B(ji,K)
               B(ji,K) = T
               DO I = MD1, MDL
                  IMD = I + K - MD
                  B(ji,IMD) = B(ji,IMD) + A(ji,I,K)*T
               END DO
            ENDIF
         END DO
      END DO

      DO K = N, 2, -1
         KMD = MD - K
         LM = MAX(1,KMD+1)
         DO JI = 1, jpoce
            IF (ACCMASK(ji) == 0) THEN
               B(ji,K) = B(ji,K)/A(ji,MD,K)
               T = -B(ji,K)
               DO I = LM, MDM
                  IMD = I - KMD
                  B(ji,IMD) = B(ji,IMD) + A(ji,I,K)*T
               END DO
            ENDIF
         END DO
      END DO

      DO JI = 1, jpoce
         IF (ACCMASK(ji) == 0) THEN
            B(ji,1) = B(ji,1)/A(ji,MD,1)
         ENDIF
      END DO

      IF ( ln_timing ) CALL timing_stop('solb')

      RETURN
!----------------------- END OF SUBROUTINE SOLB ------------------------
      END SUBROUTINE SOLB
#endif
END MODULE trosk
