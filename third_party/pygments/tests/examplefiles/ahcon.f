        SUBROUTINE AHCON (SIZE,N,M,A,B,OLEVR,OLEVI,CLEVR,CLEVI,         TRUNCATED
     &                    SCR1,SCR2,IPVT,JPVT,CON,WORK,ISEED,IERR) !Test inline comment
C
C       FUNCTION:
CF
CF      Determines whether the pair (A,B) is controllable and flags
CF      the eigenvalues corresponding to uncontrollable modes.
CF      this ad-hoc controllability calculation uses a random matrix F
CF      and computes whether eigenvalues move from A to the controlled
CF      system A+B*F.
CF
C       USAGE:
CU
CU      CALL AHCON (SIZE,N,M,A,B,OLEVR,OLEVI,CLEVR,CLEVI,SCR1,SCR2,IPVT,
CU                  JPVT,CON,WORK,ISEED,IERR)
CU
CU      since AHCON generates different random F matrices for each
CU      call, as long as iseed is not re-initialized by the main
CU      program, and since this code has the potential to be fooled
CU      by extremely ill-conditioned problems, the cautious user
CU      may wish to call it multiple times and rely, perhaps, on
CU      a 2-of-3 vote.  We believe, but have not proved, that any
CU      errors this routine may produce are conservative--i.e., that
CU      it may flag a controllable mode as uncontrollable, but
CU      not vice-versa.
CU
C       INPUTS:
CI
CI      SIZE    integer - first dimension of all 2-d arrays.
CI
CI      N       integer - number of states.
CI
CI      M       integer - number of inputs.
CI
CI      A       double precision - SIZE by N array containing the
CI              N by N system dynamics matrix A.
CI
CI      B       double precision - SIZE by M array containing the
CI              N by M system input matrix B.
CI
CI      ISEED   initial seed for random number generator; if ISEED=0,
CI              then AHCON will set ISEED to a legal value.
CI
C       OUTPUTS:
CO
CO      OLEVR   double precision - N dimensional vector containing the
CO              real parts of the eigenvalues of A.
CO
CO      OLEVI   double precision - N dimensional vector containing the
CO              imaginary parts of the eigenvalues of A.
CO
CO      CLEVR   double precision - N dimensional vector work space
CO              containing the real parts of the eigenvalues of A+B*F,
CO              where F is the random matrix.
CO
CO      CLEVI   double precision - N dimensional vector work space
CO              containing the imaginary parts of the eigenvalues of
CO              A+B*F, where F is the random matrix.
CO
CO      SCR1    double precision - N dimensional vector containing the
CO              magnitudes of the corresponding eigenvalues of A.
CO
CO      SCR2    double precision - N dimensional vector containing the
CO              damping factors of the corresponding eigenvalues of A.
CO
CO      IPVT    integer - N dimensional vector; contains the row pivots
CO              used in finding the nearest neighbor eigenvalues between
CO              those of A and of A+B*F.  The IPVT(1)th eigenvalue of
CO              A and the JPVT(1)th eigenvalue of A+B*F are the closest
CO              pair.
CO
CO      JPVT    integer - N dimensional vector; contains the column
CO              pivots used in finding the nearest neighbor eigenvalues;
CO              see IPVT.
CO
CO      CON     logical - N dimensional vector; flagging the uncontrollable
CO              modes of the system.  CON(I)=.TRUE. implies the
CO              eigenvalue of A given by DCMPLX(OLEVR(IPVT(I)),OLEVI(IPVT(i)))
CO              corresponds to a controllable mode; CON(I)=.FALSE.
CO              implies an uncontrollable mode for that eigenvalue.
CO
CO      WORK    double precision - SIZE by N dimensional array containing
CO              an N by N matrix.  WORK(I,J) is the distance between
CO              the open loop eigenvalue given by DCMPLX(OLEVR(I),OLEVI(I))
CO              and the closed loop eigenvalue of A+B*F given by
CO              DCMPLX(CLEVR(J),CLEVI(J)).
CO
CO      IERR    integer - IERR=0 indicates normal return; a non-zero
CO              value indicates trouble in the eigenvalue calculation.
CO              see the EISPACK and EIGEN documentation for details.
CO
C       ALGORITHM:
CA
CA      Calculate eigenvalues of A and of A+B*F for a randomly
CA      generated F, and see which ones change.  Use a full pivot
CA      search through a matrix of euclidean distance measures
CA      between each pair of eigenvalues from (A,A+BF) to
CA      determine the closest pairs.
CA
C       MACHINE DEPENDENCIES:
CM
CM       NONE
CM
C       HISTORY:
CH
CH      written by:             Birdwell & Laub
CH      date:                   May 18, 1985
CH      current version:        1.0
CH      modifications:          made machine independent and modified for
CH                              f77:bb:8-86.
CH                              changed cmplx -> dcmplx: 7/27/88 jdb
CH
C       ROUTINES CALLED:
CC
CC      EIGEN,RAND
CC
C       COMMON MEMORY USED:
CM
CM      none
CM
C----------------------------------------------------------------------
C       written for:    The CASCADE Project
C                       Oak Ridge National Laboratory
C                       U.S. Department of Energy
C                       contract number DE-AC05-840R21400
C                       subcontract number 37B-7685 S13
C                       organization:   The University of Tennessee
C----------------------------------------------------------------------
C       THIS SOFTWARE IS IN THE PUBLIC DOMAIN
C       NO RESTRICTIONS ON ITS USE ARE IMPLIED
C----------------------------------------------------------------------
C
C--global variables:
C
        INTEGER         SIZE
        INTEGER         N
        INTEGER         M
        INTEGER         IPVT(1)
        INTEGER         JPVT(1)
        INTEGER         IERR
C
        DOUBLE PRECISION        A(SIZE,N)
        DOUBLE PRECISION        B(SIZE,M)
        DOUBLE PRECISION        WORK(SIZE,N)
        DOUBLE PRECISION        CLEVR(N)
        DOUBLE PRECISION        CLEVI(N)
        DOUBLE PRECISION        OLEVR(N)
        DOUBLE PRECISION        OLEVI(N)
        DOUBLE PRECISION        SCR1(N)
        DOUBLE PRECISION        SCR2(N)
C
        LOGICAL                 CON(N)
C
C--local variables:
C
        INTEGER         ISEED
        INTEGER         ITEMP
        INTEGER         K1
        INTEGER         K2
        INTEGER         I
        INTEGER         J
        INTEGER         K
        INTEGER         IMAX
        INTEGER         JMAX
C
        DOUBLE PRECISION        VALUE
        DOUBLE PRECISION        EPS
        DOUBLE PRECISION        EPS1
        DOUBLE PRECISION        TEMP
        DOUBLE PRECISION        CURR
        DOUBLE PRECISION        ANORM
        DOUBLE PRECISION        BNORM
        DOUBLE PRECISION        COLNRM
        DOUBLE PRECISION        RNDMNO
C
        DOUBLE COMPLEX		DCMPLX
C
C--compute machine epsilon
C
        EPS = 1.D0
100     CONTINUE
          EPS = EPS / 2.D0
          EPS1 = 1.D0 + EPS
        IF (EPS1 .NE. 1.D0) GO TO 100
        EPS = EPS * 2.D0
C
C--compute the l-1 norm of a
C
        ANORM = 0.0D0
        DO 120 J = 1, N
          COLNRM = 0.D0
          DO 110 I = 1, N
            COLNRM = COLNRM + ABS(A(I,J))
110       CONTINUE
          IF (COLNRM .GT. ANORM) ANORM = COLNRM
120     CONTINUE
C
C--compute the l-1 norm of b
C
        BNORM = 0.0D0
        DO 140 J = 1, M
          COLNRM = 0.D0
          DO 130 I = 1, N
            COLNRM = COLNRM + ABS(B(I,J))
130       CONTINUE
          IF (COLNRM .GT. BNORM) BNORM = COLNRM
140     CONTINUE
C
C--compute a + b * f
C
        DO 160 J = 1, N
          DO 150 I = 1, N
            WORK(I,J) = A(I,J)
150       CONTINUE
160     CONTINUE
C
C--the elements of f are random with uniform distribution
C--from -anorm/bnorm to +anorm/bnorm
C--note that f is not explicitly stored as a matrix
C--pathalogical floating point notes:  the if (bnorm .gt. 0.d0)
C--test should actually be if (bnorm .gt. dsmall), where dsmall
C--is the smallest representable number whose reciprocal does
C--not generate an overflow or loss of precision.
C
        IF (ISEED .EQ. 0) ISEED = 86345823
        IF (ANORM .EQ. 0.D0) ANORM = 1.D0
        IF (BNORM .GT. 0.D0) THEN
          TEMP = 2.D0 * ANORM / BNORM
        ELSE
          TEMP = 2.D0
        END IF
        DO 190 K = 1, M
          DO 180 J = 1, N
            CALL RAND(ISEED,ISEED,RNDMNO)
            VALUE = (RNDMNO - 0.5D0) * TEMP
            DO 170 I = 1, N
              WORK(I,J) = WORK(I,J) + B(I,K)*VALUE
170         CONTINUE
180       CONTINUE
190     CONTINUE
C
C--compute the eigenvalues of a + b*f, and several other things
C
        CALL EIGEN (0,SIZE,N,WORK,CLEVR,CLEVI,WORK,SCR1,SCR2,IERR)
        IF (IERR .NE. 0) RETURN
C
C--copy a so it is not destroyed
C
        DO 210 J = 1, N
          DO 200 I = 1, N
            WORK(I,J) = A(I,J)
200       CONTINUE
210     CONTINUE
C
C--compute the eigenvalues of a, and several other things
C
        CALL EIGEN (0,SIZE,N,WORK,OLEVR,OLEVI,WORK,SCR1,SCR2,IERR)
        IF (IERR .NE. 0) RETURN
C
C--form the matrix of distances between eigenvalues of a and
C--EIGENVALUES OF A+B*F
C
        DO 230 J = 1, N
          DO 220 I = 1, N
            WORK(I,J) =
     &        ABS(DCMPLX(OLEVR(I),OLEVI(I))-DCMPLX(CLEVR(J),CLEVI(J)))
220       CONTINUE
230     CONTINUE
C
C--initialize row and column pivots
C
        DO 240 I = 1, N
          IPVT(I) = I
          JPVT(I) = I
240     CONTINUE
C
C--a little bit messy to avoid swapping columns and
C--rows of work
C
        DO 270 I = 1, N-1
C
C--find the minimum element of each lower right square
C--submatrix of work, for submatrices of size n x n
C--through 2 x 2
C
          CURR = WORK(IPVT(I),JPVT(I))
          IMAX = I
          JMAX = I
          TEMP = CURR
C
C--find the minimum element
C
          DO 260 K1 = I, N
            DO 250 K2 = I, N
              IF (WORK(IPVT(K1),JPVT(K2)) .LT. TEMP) THEN
                TEMP = WORK(IPVT(K1),JPVT(K2))
                IMAX = K1
                JMAX = K2
              END IF
250         CONTINUE
260       CONTINUE
C
C--update row and column pivots for indirect addressing of work
C
          ITEMP = IPVT(I)
          IPVT(I) = IPVT(IMAX)
          IPVT(IMAX) = ITEMP
C
          ITEMP = JPVT(I)
          JPVT(I) = JPVT(JMAX)
          JPVT(JMAX) = ITEMP
C
C--do next submatrix
C
270     CONTINUE
C
C--this threshold for determining when an eigenvalue has
C--not moved, and is therefore uncontrollable, is critical,
C--and may require future changes with more experience.
C
        EPS1 = SQRT(EPS)
C
C--for each eigenvalue pair, decide if it is controllable
C
        DO 280 I = 1, N
C
C--note that we are working with the "pivoted" work matrix
C--and are looking at its diagonal elements
C
          IF (WORK(IPVT(I),JPVT(I))/ANORM .LE. EPS1) THEN
            CON(I) = .FALSE.
          ELSE
            CON(I) = .TRUE.
          END IF
280     CONTINUE
C
C--finally!
C
        RETURN
        END
