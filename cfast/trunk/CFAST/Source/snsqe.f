      SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
C***BEGIN PROLOGUE  SNSQE
C***DATE WRITTEN   800301   (YYMMDD)
C***REVISION DATE  840405   (YYMMDD)
C***CATEGORY NO.  F2A
C***KEYWORDS  EASY-TO-USE,NONLINEAR SQUARE SYSTEM,POWELL HYBRID METHOD,
C             ZERO
C***AUTHOR  HIEBERT, K. L., (SNLA)
C***PURPOSE  SNSQE is the easy-to-use version of SNSQ which finds a zero
C            of a system of N nonlinear functions in N variables by a
C            modification of Powell hybrid method.  This code is the
C            combination of the MINPACK codes(Argonne) HYBRD1 and HYBRJ1
C***DESCRIPTION
C
C 1. Purpose.
C
C
C       The purpose of SNSQE is to find a zero of a system of N non-
C       linear functions in N variables by a modification of the Powell
C       hybrid method.  This is done by using the more general nonlinear
C       equation solver SNSQ.  The user must provide a subroutine which
C       calculates the functions.  The user has the option of either to
C       provide a subroutine which calculates the Jacobian or to let the
C       code calculate it by a forward-difference approximation.  This
C       code is the combination of the MINPACK codes (Argonne) HYBRD1
C       and HYBRJ1.
C
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,
C      *                  WA,LWA)
C       INTEGER IOPT,N,NPRINT,INFO,LWA
C       REAL TOL
C       REAL X(N),FVEC(N),WA(LWA)
C       EXTERNAL FCN,JAC
C
C
C 3. Parameters.
C
C       Parameters designated as input parameters must be specified on
C       entry to SNSQE and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from SNSQE.
C
C       FCN is the name of the user-supplied subroutine which calculates
C         the functions.  FCN must be declared in an EXTERNAL statement
C         in the user calling program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of SNSQE.  In this case, set
C         IFLAG to a negative integer.
C
C       JAC is the name of the user-supplied subroutine which calculates
C         the Jacobian.  If IOPT=1, then JAC must be declared in an
C         EXTERNAL statement in the user calling program, and should be
C         written as follows.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         Calculate the Jacobian at X and return this
C         matrix in FJAC.  FVEC contains the function
C         values at X and should not be altered.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by JAC unless the
C         user wants to terminate execution of SNSQE.  In this case, set
C         IFLAG to a negative integer.
C
C         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
C
C       IOPT is an input variable which specifies how the Jacobian will
C         be calculated.  If IOPT=1, then the user must supply the
C         Jacobian through the subroutine JAC.  If IOPT=2, then the
C         code will approximate the Jacobian by forward-differencing.
C
C       N is a positive integer input variable set to the number of
C         functions and variables.
C
C       X is an array of length N.  On input, X must contain an initial
C         estimate of the solution vector.  On output, X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length N which contains the functions
C         evaluated at the output X.
C
C       TOL is a non-negative input variable.  Termination occurs when
C         the algorithm estimates that the relative error between X and
C         the solution is at most TOL.  Section 4 contains more details
C         about TOL.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iteration thereafter and immediately prior
C         to return, with X and FVEC available for printing. Appropriate
C         print statements must be added to FCN (see example). If NPRINT
C         is not positive, no special calls of FCN with IFLAG = 0 are
C         made.
C
C       INFO is an integer output variable.  If the user has terminated
C         execution, INFO is set to the (negative) value of IFLAG.  See
C         description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  improper input parameters.
C
C         INFO = 1  algorithm estimates that the relative error between
C                   X and the solution is at most TOL.
C
C         INFO = 2  number of calls to FCN has reached or exceeded
C                   100*(N+1) for IOPT=1 or 200*(N+1) for IOPT=2.
C
C         INFO = 3  TOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         INFO = 4  iteration is not making good progress.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       WA is a work array of length LWA.
C
C       LWA is a positive integer input variable not less than
C         (3*N**2+13*N))/2.
C
C
C 4. Successful Completion.
C
C       The accuracy of SNSQE is controlled by the convergence parame-
C       ter TOL.  This parameter is used in a test which makes a compar-
C       ison between the approximation X and a solution XSOL.  SNSQE
C       terminates when the test is satisfied.  If TOL is less than the
C       machine precision (as defined by the function R1MACH(4)), then
C       SNSQE attemps only to satisfy the test defined by the machine
C       precision.  Further progress is not usually possible.  Unless
C       high precision solutions are required, the recommended value
C       for TOL is the square root of the machine precision.
C
C       The test assumes that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian  coded consistently.  If these conditions
C       are not satisfied, SNSQE may incorrectly indicate convergence.
C       The coding of the Jacobian can be checked by the subroutine
C       CHKDER.  If the Jacobian is coded correctly or IOPT=2, then
C       the validity of the answer can be checked, for example, by
C       rerunning SNSQE with a tighter tolerance.
C
C       Convergence Test.  If ENORM(Z) denotes the Euclidean norm of a
C         vector Z, then this test attempts to guarantee that
C
C               ENORM(X-XSOL) .LE.  TOL*ENORM(XSOL).
C
C         If this condition is satisfied with TOL = 10**(-K), then the
C         larger components of X have K significant decimal digits and
C         INFO is set to 1.  There is a danger that the smaller compo-
C         nents of X may have large relative errors, but the fast rate
C         of convergence of SNSQE usually avoids this possibility.
C
C
C 5. Unsuccessful Completion.
C
C       Unsuccessful termination of SNSQE can be due to improper input
C       parameters, arithmetic interrupts, an excessive number of func-
C       tion evaluations, errors in the functions, or lack of good prog-
C       ress.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1, or
C         IOPT .GT. 2, or N .LE. 0, or TOL .LT. 0.E0, or
C         LWA .LT. (3*N**2+13*N)/2.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by SNSQE.  In this
C         case, it may be possible to remedy the situation by not evalu-
C         ating the functions here, but instead setting the components
C         of FVEC to numbers that exceed those in the initial FVEC.
C
C       Excessive Number of Function Evaluations.  If the number of
C         calls to FCN reaches 100*(N+1) for IOPT=1 or 200*(N+1) for
C         IOPT=2, then this indicates that the routine is converging
C         very slowly as measured by the progress of FVEC, and INFO is
C         set to 2.  This situation should be unusual because, as
C         indicated below, lack of good progress is usually diagnosed
C         earlier by SNSQE, causing termination with INFO = 4.
C
C       Errors in the Functions.  When IOPT=2, the choice of step length
C         in the forward-difference approximation to the Jacobian
C         assumes that the relative errors in the functions are of the
C         order of the machine precision.  If this is not the case,
C         SNSQE may fail (usually with INFO = 4).  The user should
C         then either use SNSQ and set the step length or use IOPT=1
C         and supply the Jacobian.
C
C       Lack of Good Progress.  SNSQE searches for a zero of the system
C         by minimizing the sum of the squares of the functions.  In so
C         doing, it can become trapped in a region where the minimum
C         does not correspond to a zero of the system and, in this situ-
C         ation, the iteration eventually fails to make good progress.
C         In particular, this will happen if the system does not have a
C         zero.  If the system has a zero, rerunning SNSQE from a dif-
C         ferent starting point may be helpful.
C
C
C 6. Characteristics of the Algorithm.
C
C       SNSQE is a modification of the Powell hybrid method.  Two of
C       its main characteristics involve the choice of the correction as
C       a convex combination of the Newton and scaled gradient direc-
C       tions, and the updating of the Jacobian by the rank-1 method of
C       Broyden.  The choice of the correction guarantees (under reason-
C       able conditions) global convergence for starting points far from
C       the solution and a fast rate of convergence.  The Jacobian is
C       calculated at the starting point by either the user-supplied
C       subroutine or a forward-difference approximation, but it is not
C       recalculated until the rank-1 method fails to produce satis-
C       factory progress.
C
C       Timing.  The time required by SNSQE to solve a given problem
C         depends on N, the behavior of the functions, the accuracy
C         requested, and the starting point.  The number of arithmetic
C         operations needed by SNSQE is about 11.5*(N**2) to process
C         each evaluation of the functions (call to FCN) and 1.3*(N**3)
C         to process each evaluation of the Jacobian (call to JAC,
C         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
C         the timing of SNSQE will be strongly influenced by the time
C         spent in FCN and JAC.
C
C       Storage.  SNSQE requires (3*N**2 + 17*N)/2 single precision
C         storage locations, in addition to the storage required by the
C         program.  There are no internally declared storage arrays.
C
C
C 7. Example.
C
C       The problem is to determine the values of X(1), X(2), ..., X(9),
C       which solve the system of tridiagonal equations
C
C       (3-2*X(1))*X(1)           -2*X(2)                   = -1
C               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
C                                   -X(8) + (3-2*X(9))*X(9) = -1
C
C       **********
C
C       PROGRAM TEST(INPUT,OUTPUT,TAPE6=OUTPUT)
C C
C C     Driver for SNSQE example.
C C
C       INTEGER J,N,IOPT,NPRINT,INFO,LWA,NWRITE
C       REAL TOL,FNORM
C       REAL X(9),FVEC(9),WA(180)
C       REAL ENORM,R1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 2
C       N = 9
C C
C C     The following starting values provide a rough solution.
C C
C       DO 10 J = 1, 9
C          X(J) = -1.E0
C    10    CONTINUE
C
C       LWA = 180
C       NPRINT = 0
C C
C C     Set TOL to the square root of the machine precision.
C C     Unless high precision solutions are required,
C C     this is the recommended setting.
C C
C       TOL = SQRT(R1MACH(4))
C C
C       CALL SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
C       FNORM = ENORM(N,FVEC)
C       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' EXIT PARAMETER',16X,I10 //
C      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
C       END
C       SUBROUTINE FCN(N,X,FVEC,IFLAG)
C       INTEGER N,IFLAG
C       REAL X(N),FVEC(N)
C       INTEGER K
C       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
C       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
C C
C       DO 10 K = 1, N
C          TEMP = (THREE - TWO*X(K))*X(K)
C          TEMP1 = ZERO
C          IF (K .NE. 1) TEMP1 = X(K-1)
C          TEMP2 = ZERO
C          IF (K .NE. N) TEMP2 = X(K+1)
C          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
C    10    CONTINUE
C       RETURN
C       END
C
C       Results obtained with different compilers or machines
C       may be slightly different.
C
C       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
C
C       EXIT PARAMETER                         1
C
C       FINAL APPROXIMATE SOLUTION
C
C       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
C       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
C       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
C***REFERENCES  POWELL, M. J. D.
C                 A HYBRID METHOD FOR NONLINEAR EQUATIONS.
C                 NUMERICAL METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS,
C                 P. RABINOWITZ, EDITOR.  GORDON AND BREACH, 1970.
C***ROUTINES CALLED  SNSQ,XERROR
C***END PROLOGUE  SNSQE
      include "precis.fi"
      INTEGER IOPT,N,NPRINT,INFO,LWA
      DIMENSION X(N),FVEC(N),WA(LWA)
      EXTERNAL FCN,JAC
      INTEGER INDEX,J,LR,MAXFEV,ML,MODE,MU,NFEV,NJEV
      DATA FACTOR,ONE,ZERO /1.0D2,1.0D0,0.0D0/
C***FIRST EXECUTABLE STATEMENT  SNSQE
      INFO = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0
     1    .OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 +13*N)/2)
     2   GO TO 20
C
C     CALL SNSQ.
C
      MAXFEV = 100*(N + 1)
      IF (IOPT .EQ. 2) MAXFEV = 2*MAXFEV
      XTOL = TOL
      ML = N - 1
      MU = N - 1
      EPSFCN = ZERO
      MODE = 2
      DO 10 J = 1, N
         WA(J) = ONE
   10    CONTINUE
      LR = (N*(N + 1))/2
      INDEX=6*N+LR
      CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,MU,
     1           EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
     2           WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),
     3           WA(5*N+1))
      IF (INFO .EQ. 5) INFO = 4
   20 CONTINUE
      IF (INFO .EQ. 0) CALL XERROR( 'SNSQE  -- INVALID INPUT PARAMETER.'
     1,34,2,1)
      RETURN
C
C     LAST CARD OF SUBROUTINE SNSQE.
C
      END
      SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
C***BEGIN PROLOGUE  DOGLEG
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine DOGLEG
C
C     Given an M by N matrix A, an N by N nonsingular DIAGONAL
C     matrix D, an M-vector B, and a positive number DELTA, the
C     problem is to determine the convex combination X of the
C     Gauss-Newton and scaled gradient directions that minimizes
C     (A*X - B) in the least squares sense, subject to the
C     restriction that the Euclidean norm of D*X be at most DELTA.
C
C     This subroutine completes the solution of the problem
C     if it is provided with the necessary information from the
C     QR factorization of A. That is, if A = Q*R, where Q has
C     orthogonal columns and R is an upper triangular matrix,
C     then DOGLEG expects the full upper triangle of R and
C     the first N components of (Q TRANSPOSE)*B.
C
C     The subroutine statement is
C
C       SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
C
C     where
C
C       N is a positive integer input variable set to the order of R.
C
C       R is an input array of length LR which must contain the upper
C         triangular matrix R stored by rows.
C
C       LR is a positive integer input variable not less than
C         (N*(N+1))/2.
C
C       DIAG is an input array of length N which must contain the
C         diagonal elements of the matrix D.
C
C       QTB is an input array of length N which must contain the first
C         N elements of the vector (Q TRANSPOSE)*B.
C
C       DELTA is a positive input variable which specifies an upper
C         bound on the Euclidean norm of D*X.
C
C       X is an output array of length N which contains the desired
C         convex combination of the Gauss-Newton direction and the
C         scaled gradient direction.
C
C       WA1 and WA2 are work arrays of length N.
C
C     Subprograms called
C
C       MINPACK-supplied ... R1MACH,ENORM
C
C       FORTRAN-supplied ... ABS,MAX,MIN,SQRT
C
C     MINPACK. Version of July 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  ENORM,R1MACH
C***END PROLOGUE  DOGLEG
      include "precis.fi"
      INTEGER N,LR
      DIMENSION R(LR),DIAG(N),QTB(N),X(N),WA1(N),WA2(N)
      INTEGER I,J,JJ,JP1,K,L
      DATA ONE,ZERO /1.0D0,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DOGLEG
      EPSMCH = D1MACH(4)
C
C     FIRST, CALCULATE THE GAUSS-NEWTON DIRECTION.
C
      JJ = (N*(N + 1))/2 + 1
      DO 50 K = 1, N
         J = N - K + 1
         JP1 = J + 1
         JJ = JJ - K
         L = JJ + 1
         SUM = ZERO
         IF (N .LT. JP1) GO TO 20
         DO 10 I = JP1, N
            SUM = SUM + R(L)*X(I)
            L = L + 1
   10       CONTINUE
   20    CONTINUE
         TEMP = R(JJ)
         IF (TEMP .NE. ZERO) GO TO 40
         L = J
         DO 30 I = 1, J
            TEMP = MAX(TEMP,ABS(R(L)))
            L = L + N - I
   30       CONTINUE
         TEMP = EPSMCH*TEMP
         IF (TEMP .EQ. ZERO) TEMP = EPSMCH
   40    CONTINUE
         X(J) = (QTB(J) - SUM)/TEMP
   50    CONTINUE
C
C     TEST WHETHER THE GAUSS-NEWTON DIRECTION IS ACCEPTABLE.
C
      DO 60 J = 1, N
         WA1(J) = ZERO
         WA2(J) = DIAG(J)*X(J)
   60    CONTINUE
      QNORM = ENORM(N,WA2)
      IF (QNORM .LE. DELTA) GO TO 140
C
C     THE GAUSS-NEWTON DIRECTION IS NOT ACCEPTABLE.
C     NEXT, CALCULATE THE SCALED GRADIENT DIRECTION.
C
      L = 1
      DO 80 J = 1, N
         TEMP = QTB(J)
         DO 70 I = J, N
            WA1(I) = WA1(I) + R(L)*TEMP
            L = L + 1
   70       CONTINUE
         WA1(J) = WA1(J)/DIAG(J)
   80    CONTINUE
C
C     CALCULATE THE NORM OF THE SCALED GRADIENT DIRECTION,
C     NORMALIZE, AND RESCALE THE GRADIENT.
C
      GNORM = ENORM(N,WA1)
      SGNORM = ZERO
      ALPHA = DELTA/QNORM
      IF (GNORM .EQ. ZERO) GO TO 120
      DO 90 J = 1, N
         WA1(J) = (WA1(J)/GNORM)/DIAG(J)
   90    CONTINUE
C
C     CALCULATE THE POINT ALONG THE SCALED GRADIENT
C     AT WHICH THE QUADRATIC IS MINIMIZED.
C
      L = 1
      DO 110 J = 1, N
         SUM = ZERO
         DO 100 I = J, N
            SUM = SUM + R(L)*WA1(I)
            L = L + 1
  100       CONTINUE
         WA2(J) = SUM
  110    CONTINUE
      TEMP = ENORM(N,WA2)
      SGNORM = (GNORM/TEMP)/TEMP
C
C     TEST WHETHER THE SCALED GRADIENT DIRECTION IS ACCEPTABLE.
C
      ALPHA = ZERO
      IF (SGNORM .GE. DELTA) GO TO 120
C
C     THE SCALED GRADIENT DIRECTION IS NOT ACCEPTABLE.
C     FINALLY, CALCULATE THE POINT ALONG THE DOGLEG
C     AT WHICH THE QUADRATIC IS MINIMIZED.
C
      BNORM = ENORM(N,QTB)
      TEMP = (BNORM/GNORM)*(BNORM/QNORM)*(SGNORM/DELTA)
      TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2
     1       + SQRT((TEMP-(DELTA/QNORM))**2
     2              +(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))
      ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP
  120 CONTINUE
C
C     FORM APPROPRIATE CONVEX COMBINATION OF THE GAUSS-NEWTON
C     DIRECTION AND THE SCALED GRADIENT DIRECTION.
C
      TEMP = (ONE - ALPHA)*MIN(SGNORM,DELTA)
      DO 130 J = 1, N
         X(J) = TEMP*WA1(J) + ALPHA*X(J)
  130    CONTINUE
  140 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DOGLEG.
C
      END
      DOUBLE PRECISION FUNCTION ENORM(N,X)
C***BEGIN PROLOGUE  ENORM
C***REFER TO  SNLS1,SNLS1E,SNSQ,SNSQE
C
C     **********
C     Function ENORM
C
C     Given an N-vector X, this function calculates the
C     Euclidean norm of X.
C
C     The Euclidean norm is computed by accumulating the sum of
C     squares in three different sums. The sums of squares for the
C     small and large components are scaled so that no overflows
C     occur. Non-destructive underflows are permitted. Underflows
C     and overflows do not occur in the computation of the unscaled
C     sum of squares for the intermediate components.
C     The definitions of small, intermediate and large components
C     depend on two constants, RDWARF and RGIANT. The main
C     restrictions on these constants are that RDWARF**2 not
C     underflow and RGIANT**2 not overflow. The constants
C     given here are suitable for every known computer.
C
C     The function statement is
C
C       REAL FUNCTION ENORM(N,X)
C
C     where
C
C       N is a positive integer input variable.
C
C       X is an input array of length N.
C
C     Subprograms called
C
C       FORTRAN-supplied ... ABS,SQRT
C
C     MINPACK. Version of October 1979.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  ENORM
      include "precis.fi"
      INTEGER N
      DIMENSION X(N)
      INTEGER I
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
C***FIRST EXECUTABLE STATEMENT  ENORM
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = ABS(X(I))
         IF (XABS .GT. RDWARF .AND. XABS .LT. AGIANT) GO TO 70
            IF (XABS .LE. RDWARF) GO TO 30
C
C              SUM FOR LARGE COMPONENTS.
C
               IF (XABS .LE. X1MAX) GO TO 10
                  S1 = ONE + S1*(X1MAX/XABS)**2
                  X1MAX = XABS
                  GO TO 20
   10          CONTINUE
                  S1 = S1 + (XABS/X1MAX)**2
   20          CONTINUE
               GO TO 60
   30       CONTINUE
C
C              SUM FOR SMALL COMPONENTS.
C
               IF (XABS .LE. X3MAX) GO TO 40
                  S3 = ONE + S3*(X3MAX/XABS)**2
                  X3MAX = XABS
                  GO TO 50
   40          CONTINUE
                  IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
   50          CONTINUE
   60       CONTINUE
            GO TO 80
   70    CONTINUE
C
C           SUM FOR INTERMEDIATE COMPONENTS.
C
            S2 = S2 + XABS**2
   80    CONTINUE
   90    CONTINUE
C
C     CALCULATION OF NORM.
C
      IF (S1 .EQ. ZERO) GO TO 100
         ENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)
         GO TO 130
  100 CONTINUE
         IF (S2 .EQ. ZERO) GO TO 110
            IF (S2 .GE. X3MAX)
     1         ENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
            IF (S2 .LT. X3MAX)
     1         ENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
            GO TO 120
  110    CONTINUE
            ENORM = X3MAX*SQRT(S3)
  120    CONTINUE
  130 CONTINUE
      RETURN
C
C     LAST CARD OF FUNCTION ENORM.
C
      END
      SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
     1   WA1,WA2)
C***BEGIN PROLOGUE  FDJAC1
C***REFER TO  SNSQ,SNSQE
C
C              Subroutine FDJAC1
C
C     This subroutine computes a forward-difference approximation
C     to the N by N Jacobian matrix associated with a specified
C     problem of N functions in N VARIABLES. If the Jacobian has
C     a banded form, then function evaluations are saved by only
C     approximating the nonzero terms.
C
C     The subroutine statement is
C
C       SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
C                         WA1,WA2)
C
C     where
C
C       FCN is the name of the user-supplied subroutine which
C         calculates the functions. FCN must be declared
C         in an external statement in the user calling
C         program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless
C         the user wants to terminate execution of FDJAC1.
C         In this case set IFLAG to a negative integer.
C
C       N Is a positive integer input variable set to the number
C         of functions and variables.
C
C       X is an input array of length N.
C
C       FVEC is an input array of length N which must contain the
C         functions evaluated at X.
C
C       FJAC is an output N by N array which contains the
C         approximation to the Jacobian matrix evaluated at X.
C
C       LDFJAC is a positive integer input variable not less than N
C         which specifies the leading dimension of the array FJAC.
C
C       IFLAG is an integer variable which can be used to terminate
C         the execution of FDJAC1. See description of FCN.
C
C       ML is a nonnegative integer input variable which specifies
C         the number of subdiagonals within the band of the
C         Jacobian matrix. If the Jacobian is not banded, set
C         ML to at least N - 1.
C
C       EPSFCN is an input variable used in determining a suitable
C         step length for the forward-difference approximation. This
C         approximation assumes that the relative errors in the
C         functions are of the order of EPSFCN. If EPSFCN is less
C         than the machine precision, it is assumed that the relative
C         errors in the functions are of the order of the machine
C         precision.
C
C       MU is a nonnegative integer input variable which specifies
C         the number of superdiagonals within the band of the
C         Jacobian matrix. If the Jacobian is not banded, set
C         MU to at least N - 1.
C
C       WA1 and WA2 are work arrays of length N. If ML + MU + 1 is at
C         least N, then the Jacobian is considered dense, and WA2 is
C         not referenced.
C
C     Subprograms called
C
C       MINPACK-supplied ... R1MACH
C
C       FORTRAN-supplied ... ABS,MAX,SQRT
C
C     MINPACK. Version of June 1979.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  R1MACH
C***END PROLOGUE  FDJAC1
      include "precis.fi"
      INTEGER N,LDFJAC,IFLAG,ML,MU
      DIMENSION X(N),FVEC(N),FJAC(LDFJAC,N),WA1(N),WA2(N)
      INTEGER I,J,K,MSUM
      DATA ZERO /0.0D0/
C***FIRST EXECUTABLE STATEMENT  FDJAC1
      EPSMCH = D1MACH(4)
C
      EPS = SQRT(MAX(EPSFCN,EPSMCH))
      MSUM = ML + MU + 1
      IF (MSUM .LT. N) GO TO 40
C
C        COMPUTATION OF DENSE APPROXIMATE JACOBIAN.
C
         DO 20 J = 1, N
            TEMP = X(J)
            H = EPS*ABS(TEMP)
            IF (H .EQ. ZERO) H = EPS
            X(J) = TEMP + H
            CALL FCN(N,X,WA1,IFLAG)
            IF (IFLAG .LT. 0) GO TO 30
            X(J) = TEMP
            DO 10 I = 1, N
               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
         GO TO 110
   40 CONTINUE
C
C        COMPUTATION OF BANDED APPROXIMATE JACOBIAN.
C
         DO 90 K = 1, MSUM
            DO 60 J = K, N, MSUM
               WA2(J) = X(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS
               X(J) = WA2(J) + H
   60          CONTINUE
            CALL FCN(N,X,WA1,IFLAG)
            IF (IFLAG .LT. 0) GO TO 100
            DO 80 J = K, N, MSUM
               X(J) = WA2(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS
               DO 70 I = 1, N
                  FJAC(I,J) = ZERO
                  IF (I .GE. J - MU .AND. I .LE. J + ML)
     1               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE FDJAC1.
C
      END
      SUBROUTINE QFORM(M,N,Q,LDQ,WA)
C***BEGIN PROLOGUE  QFORM
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine QFORM
C
C     This subroutine proceeds from the computed QR factorization of
C     an M by N matrix A to accumulate the M by M orthogonal matrix
C     Q from its factored form.
C
C     The subroutine statement is
C
C       SUBROUTINE QFORM(M,N,Q,LDQ,WA)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A and the order of Q.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       Q is an M by M array. On input the full lower trapezoid in
C         the first min(M,N) columns of Q contains the factored form.
C         On output Q has been accumulated into a square matrix.
C
C       LDQ is a positive integer input variable not less than M
C         which specifies the leading dimension of the array Q.
C
C       WA is a work array of length M.
C
C     Subprograms called
C
C       FORTRAN-supplied ... MIN0
C
C     MINPACK. Version of January 1979.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  QFORM
      include "precis.fi"
      INTEGER M,N,LDQ
      DIMENSION Q(LDQ,M),WA(M)
      INTEGER I,J,JM1,K,L,MINMN,NP1
      DATA ONE,ZERO /1.0D0,0.0D0/
C***FIRST EXECUTABLE STATEMENT  QFORM
      MINMN = MIN0(M,N)
      IF (MINMN .LT. 2) GO TO 30
      DO 20 J = 2, MINMN
         JM1 = J - 1
         DO 10 I = 1, JM1
            Q(I,J) = ZERO
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
C
C     INITIALIZE REMAINING COLUMNS TO THOSE OF THE IDENTITY MATRIX.
C
      NP1 = N + 1
      IF (M .LT. NP1) GO TO 60
      DO 50 J = NP1, M
         DO 40 I = 1, M
            Q(I,J) = ZERO
   40       CONTINUE
         Q(J,J) = ONE
   50    CONTINUE
   60 CONTINUE
C
C     ACCUMULATE Q FROM ITS FACTORED FORM.
C
      DO 120 L = 1, MINMN
         K = MINMN - L + 1
         DO 70 I = K, M
            WA(I) = Q(I,K)
            Q(I,K) = ZERO
   70       CONTINUE
         Q(K,K) = ONE
         IF (WA(K) .EQ. ZERO) GO TO 110
         DO 100 J = K, M
            SUM = ZERO
            DO 80 I = K, M
               SUM = SUM + Q(I,J)*WA(I)
   80          CONTINUE
            TEMP = SUM/WA(K)
            DO 90 I = K, M
               Q(I,J) = Q(I,J) - TEMP*WA(I)
   90          CONTINUE
  100       CONTINUE
  110    CONTINUE
  120    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE QFORM.
C
      END
      SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
C***BEGIN PROLOGUE  QRFAC
C***REFER TO  SNLS1,SNLS1E,SNSQ,SNSQE
C
C     **********
C     Subroutine QRFAC
C
C     This subroutine uses Householder transformations with column
C     pivoting (optional) to compute a QR factorization of the
C     M by N matrix A. That is, QRFAC determines an orthogonal
C     matrix Q, a permutation matrix P, and an upper trapezoidal
C     matrix R with diagonal elements of nonincreasing magnitude,
C     such that A*P = Q*R. The Householder transformation for
C     column K, K = 1,2,...,MIN(M,N), is of the form
C
C                           T
C           I - (1/U(K))*U*U
C
C     where U has zeros in the first K-1 positions. The form of
C     this transformation and the method of pivoting first
C     appeared in the corresponding LINPACK subroutine.
C
C     The subroutine statement is
C
C       SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       A is an M by N array. On input A contains the matrix for
C         which the QR factorization is to be computed. On output
C         the strict upper trapezoidal part of A contains the strict
C         upper trapezoidal part of R, and the lower trapezoidal
C         part of A contains a factored form of Q (the non-trivial
C         elements of the U vectors described above).
C
C       LDA is a positive integer input variable not less than M
C         which specifies the leading dimension of the array A.
C
C       PIVOT is a logical input variable. If pivot is set .TRUE.,
C         then column pivoting is enforced. If pivot is set .FALSE.,
C         then no column pivoting is done.
C
C       IPVT is an integer output array of length LIPVT. IPVT
C         defines the permutation matrix P such that A*P = Q*R.
C         Column J of P is column IPVT(J) of the identity matrix.
C         If pivot is .FALSE., IPVT is not referenced.
C
C       LIPVT is a positive integer input variable. If PIVOT is
C             .FALSE., then LIPVT may be as small as 1. If PIVOT is
C             .TRUE., then LIPVT must be at least N.
C
C       SIGMA is an output array of length N which contains the
C         diagonal elements of R.
C
C       ACNORM is an output array of length N which contains the
C         norms of the corresponding columns of the input matrix A.
C         If this information is not needed, then ACNORM can coincide
C         with SIGMA.
C
C       WA is a work array of length N. If pivot is .FALSE., then WA
C         can coincide with SIGMA.
C
C     Subprograms called
C
C       MINPACK-Supplied ... R1MACH,ENORM
C       FORTRAN-Supplied ... MAX,SQRT,MIN0
C
C     MINPACK. Version of December 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  ENORM,R1MACH
C***END PROLOGUE  QRFAC
      include "precis.fi"
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(LIPVT)
      LOGICAL PIVOT
      DIMENSION A(LDA,N),SIGMA(N),ACNORM(N),WA(N)
      INTEGER I,J,JP1,K,KMAX,MINMN
      DATA ONE,P05,ZERO /1.0D0,5.0D-2,0.0D0/
C***FIRST EXECUTABLE STATEMENT  QRFAC
      EPSMCH = D1MACH(4)
C
C     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
C
      DO 10 J = 1, N
         ACNORM(J) = ENORM(M,A(1,J))
         SIGMA(J) = ACNORM(J)
         WA(J) = SIGMA(J)
         IF (PIVOT) IPVT(J) = J
   10    CONTINUE
C
C     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
C
      MINMN = MIN0(M,N)
      DO 110 J = 1, MINMN
         IF (.NOT.PIVOT) GO TO 40
C
C        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
C
         KMAX = J
         DO 20 K = J, N
            IF (SIGMA(K) .GT. SIGMA(KMAX)) KMAX = K
   20       CONTINUE
         IF (KMAX .EQ. J) GO TO 40
         DO 30 I = 1, M
            TEMP = A(I,J)
            A(I,J) = A(I,KMAX)
            A(I,KMAX) = TEMP
   30       CONTINUE
         SIGMA(KMAX) = SIGMA(J)
         WA(KMAX) = WA(J)
         K = IPVT(J)
         IPVT(J) = IPVT(KMAX)
         IPVT(KMAX) = K
   40    CONTINUE
C
C        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
C        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
C
         AJNORM = ENORM(M-J+1,A(J,J))
         IF (AJNORM .EQ. ZERO) GO TO 100
         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
         DO 50 I = J, M
            A(I,J) = A(I,J)/AJNORM
   50       CONTINUE
         A(J,J) = A(J,J) + ONE
C
C        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
C        AND UPDATE THE NORMS.
C
         JP1 = J + 1
         IF (N .LT. JP1) GO TO 100
         DO 90 K = JP1, N
            SUM = ZERO
            DO 60 I = J, M
               SUM = SUM + A(I,J)*A(I,K)
   60          CONTINUE
            TEMP = SUM/A(J,J)
            DO 70 I = J, M
               A(I,K) = A(I,K) - TEMP*A(I,J)
   70          CONTINUE
            IF (.NOT.PIVOT .OR. SIGMA(K) .EQ. ZERO) GO TO 80
            TEMP = A(J,K)/SIGMA(K)
            SIGMA(K) = SIGMA(K)*SQRT(MAX(ZERO,ONE-TEMP**2))
            IF (P05*(SIGMA(K)/WA(K))**2 .GT. EPSMCH) GO TO 80
            SIGMA(K) = ENORM(M-J,A(JP1,K))
            WA(K) = SIGMA(K)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
         SIGMA(J) = -AJNORM
  110    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE QRFAC.
C
      END
      SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
C***BEGIN PROLOGUE  R1MPYQ
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine R1MPYQ
C
C     Given an M by N matrix A, this subroutine computes A*Q where
C     Q is the product of 2*(N - 1) transformations
C
C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
C
C     and GV(I), GW(I) are Givens rotations in the (I,N) plane which
C     eliminate elements in the I-th and N-th planes, respectively.
C     Q itself is not given, rather the information to recover the
C     GV, GW rotations is supplied.
C
C     The subroutine statement is
C
C       SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       A is an M by N ARRAY. On input A must contain the matrix
C         to be postmultiplied by the orthogonal matrix Q
C         described above. On output A*Q has replaced A.
C
C       LDA is a positive integer input variable not less than M
C         which specifies the leading dimension of the array A.
C
C       V is an input array of length N. V(I) must contain the
C         information necessary to recover the Givens rotation GV(I)
C         described above.
C
C       W is an input array of length N. W(I) must contain the
C         information necessary to recover the Givens rotation GW(I)
C         described above.
C
C     Subroutines called
C
C       FORTRAN-Supplied ... abs,sqrt
C
C     MINPACK. Version of December 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C     **********
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  R1MPYQ
      include "precis.fi"
      INTEGER M,N,LDA
      DIMENSION A(LDA,N),V(N),W(N)
      INTEGER I,J,NMJ,NM1
      DATA ONE /1.0D0/
C***FIRST EXECUTABLE STATEMENT  R1MPYQ
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 50
      DO 20 NMJ = 1, NM1
         J = N - NMJ
         IF (ABS(V(J)) .GT. ONE) COS = ONE/V(J)
         IF (ABS(V(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(V(J)) .LE. ONE) SIN = V(J)
         IF (ABS(V(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 10 I = 1, M
            TEMP = COS*A(I,J) - SIN*A(I,N)
            A(I,N) = SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   10       CONTINUE
   20    CONTINUE
C
C     APPLY THE SECOND SET OF GIVENS ROTATIONS TO A.
C
      DO 40 J = 1, NM1
         IF (ABS(W(J)) .GT. ONE) COS = ONE/W(J)
         IF (ABS(W(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(W(J)) .LE. ONE) SIN = W(J)
         IF (ABS(W(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 30 I = 1, M
            TEMP = COS*A(I,J) + SIN*A(I,N)
            A(I,N) = -SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE R1MPYQ.
C
      END
      SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
C***BEGIN PROLOGUE  R1UPDT
C***REFER TO  SNSQ,SNSQE
C
C     **********
C     Subroutine R1UPDT
C
C     Given an M by N lower trapezoidal matrix S, an M-vector U,
C     and an N-vector V, the problem is to determine an
C     orthogonal matrix Q such that
C
C                   T
C           (S + U*V )*Q
C
C     is again lower trapezoidal.
C
C     This subroutine determines Q as the product of 2*(N - 1)
C     transformations
C
C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
C
C     where GV(I), GW(I) are Givens rotations in the (I,N) plane
C     which eliminate elements in the I-th and N-th planes,
C     respectively. Q Itself is not accumulated, rather the
C     information to recover the GV, GW rotations is returned.
C
C     The subroutine statement is
C
C       SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of S.
C
C       N is a positive integer input variable set to the number
C         of columns of S. N must not exceed M.
C
C       S is an array of length LS. On input S must contain the lower
C         trapezoidal matrix S stored by columns. On output S contains
C         the lower trapezoidal matrix produced as described above.
C
C       LS is a positive integer input variable not less than
C         (N*(2*M-N+1))/2.
C
C       U is an input array of length M which must contain the
C         vector U.
C
C       V is an array of length N. On input V must contain the vector
C         V. On output V(I) contains the information necessary to
C         recover the Givens rotation GV(I) described above.
C
C       W is an output array of length M. W(I) contains information
C         necessary to recover the Givens rotation GW(I) described
C         above.
C
C       SING is a logical output variable. SING is set .TRUE. if any
C         of the diagonal elements of the output S are zero. Otherwise
C         SING is set .FALSE.
C
C     Subprograms called
C
C       MINPACK-supplied ... R1MACH
C       FORTRAN-supplied ... ABS,SQRT
C
C     MINPACK. Version of December 1978.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More,
C     John L. Nazareth
C     **********
C***ROUTINES CALLED  R1MACH
C***END PROLOGUE  R1UPDT
      include "precis.fi"
      INTEGER M,N,LS
      LOGICAL SING
      DIMENSION S(LS),U(M),V(N),W(M)
      INTEGER I,J,JJ,L,NMJ,NM1
      DATA ONE,P5,P25,ZERO /1.0D0,5.0D-1,2.5D-1,0.0D0/
C***FIRST EXECUTABLE STATEMENT  R1UPDT
      GIANT = D1MACH(2)
C
C     INITIALIZE THE DIAGONAL ELEMENT POINTER.
C
      JJ = (N*(2*M - N + 1))/2 - (M - N)
C
C     MOVE THE NONTRIVIAL PART OF THE LAST COLUMN OF S INTO W.
C
      L = JJ
      DO 10 I = N, M
         W(I) = S(L)
         L = L + 1
   10    CONTINUE
C
C     ROTATE THE VECTOR V INTO A MULTIPLE OF THE N-TH UNIT VECTOR
C     IN SUCH A WAY THAT A SPIKE IS INTRODUCED INTO W.
C
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 NMJ = 1, NM1
         J = N - NMJ
         JJ = JJ - (M - J + 1)
         W(J) = ZERO
         IF (V(J) .EQ. ZERO) GO TO 50
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C        J-TH ELEMENT OF V.
C
         IF (ABS(V(N)) .GE. ABS(V(J))) GO TO 20
            COTAN = V(N)/V(J)
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 30
   20    CONTINUE
            TAN = V(J)/V(N)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
   30    CONTINUE
C
C        APPLY THE TRANSFORMATION TO V AND STORE THE INFORMATION
C        NECESSARY TO RECOVER THE GIVENS ROTATION.
C
         V(N) = SIN*V(J) + COS*V(N)
         V(J) = TAU
C
C        APPLY THE TRANSFORMATION TO S AND EXTEND THE SPIKE IN W.
C
         L = JJ
         DO 40 I = J, M
            TEMP = COS*S(L) - SIN*W(I)
            W(I) = SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     ADD THE SPIKE FROM THE RANK 1 UPDATE TO W.
C
      DO 80 I = 1, M
         W(I) = W(I) + V(N)*U(I)
   80    CONTINUE
C
C     ELIMINATE THE SPIKE.
C
      SING = .FALSE.
      IF (NM1 .LT. 1) GO TO 140
      DO 130 J = 1, NM1
         IF (W(J) .EQ. ZERO) GO TO 120
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C        J-TH ELEMENT OF THE SPIKE.
C
         IF (ABS(S(JJ)) .GE. ABS(W(J))) GO TO 90
            COTAN = S(JJ)/W(J)
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 100
   90    CONTINUE
            TAN = W(J)/S(JJ)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
  100    CONTINUE
C
C        APPLY THE TRANSFORMATION TO S AND REDUCE THE SPIKE IN W.
C
         L = JJ
         DO 110 I = J, M
            TEMP = COS*S(L) + SIN*W(I)
            W(I) = -SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
  110       CONTINUE
C
C        STORE THE INFORMATION NECESSARY TO RECOVER THE
C        GIVENS ROTATION.
C
         W(J) = TAU
  120    CONTINUE
C
C        TEST FOR ZERO DIAGONAL ELEMENTS IN THE OUTPUT S.
C
         IF (S(JJ) .EQ. ZERO) SING = .TRUE.
         JJ = JJ + (M - J + 1)
  130    CONTINUE
  140 CONTINUE
C
C     MOVE W BACK INTO THE LAST COLUMN OF THE OUTPUT S.
C
      L = JJ
      DO 150 I = N, M
         S(L) = W(I)
         L = L + 1
  150    CONTINUE
      IF (S(JJ) .EQ. ZERO) SING = .TRUE.
      RETURN
C
C     LAST CARD OF SUBROUTINE R1UPDT.
C
      END
      SUBROUTINE SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,
     1   MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,R,LR,QTF,WA1,
     2   WA2,WA3,WA4)
C***BEGIN PROLOGUE  SNSQ
C***DATE WRITTEN   800301   (YYMMDD)
C***REVISION DATE  840405   (YYMMDD)
C***CATEGORY NO.  F2A
C***KEYWORDS  NONLINEAR SQUARE SYSTEM,POWELL HYBRID METHOD,ZERO
C***AUTHOR  HIEBERT, K. L., (SNLA)
C***PURPOSE  SNSQ finds to find a zero of a system of N nonlinear
C            functions in N variables by a modification of the Powell
C            hybrid method.  This code is the combination of the MINPACK
C            codes (Argonne) HYBRD and HYBRDJ.
C***DESCRIPTION
C
C 1. Purpose.
C
C       The purpose of SNSQ is to find a zero of a system of N non-
C       linear functions in N variables by a modification of the Powell
C       hybrid method.  The user must provide a subroutine which calcu-
C       lates the functions.  The user has the option of either to
C       provide a subroutine which calculates the Jacobian or to let the
C       code calculate it by a forward-difference approximation.
C       This code is the combination of the MINPACK codes (Argonne)
C       HYBRD and HYBRDJ.
C
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,
C      *                 ML,MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,
C      *                 NJEV,R,LR,QTF,WA1,WA2,WA3,WA4)
C       INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,NJEV,LR
C       REAL XTOL,EPSFCN,FACTOR
C       REAL X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),
C      *     WA1(N),WA2(N),WA3(N),WA4(N)
C       EXTERNAL FCN,JAC
C
C
C 3. Parameters.
C
C       Parameters designated as input parameters must be specified on
C       entry to SNSQ and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from SNSQ.
C
C       FCN is the name of the user-supplied subroutine which calculates
C         the functions.  FCN must be declared in an EXTERNAL statement
C         in the user calling program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of SNSQ.  In this case, set
C         IFLAG to a negative integer.
C
C       JAC is the name of the user-supplied subroutine which calculates
C         the Jacobian.  If IOPT=1, then JAC must be declared in an
C         EXTERNAL statement in the user calling program, and should be
C         written as follows.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         Calculate the Jacobian at X and return this
C         matrix in FJAC.  FVEC contains the function
C         values at X and should not be altered.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by JAC unless the
C         user wants to terminate execution of SNSQ.  In this case, set
C         IFLAG to a negative integer.
C
C         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
C
C       IOPT is an input variable which specifies how the Jacobian will
C         be calculated.  If IOPT=1, then the user must supply the
C         Jacobian through the subroutine JAC.  If IOPT=2, then the
C         code will approximate the Jacobian by forward-differencing.
C
C       N is a positive integer input variable set to the number of
C         functions and variables.
C
C       X is an array of length N.  On input, X must contain an initial
C         estimate of the solution vector.  On output, X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length N which contains the functions
C         evaluated at the output X.
C
C       FJAC is an output N by N array which contains the orthogonal
C         matrix Q produced by the QR factorization of the final approx-
C         imate Jacobian.
C
C       LDFJAC is a positive integer input variable not less than N
C         which specifies the leading dimension of the array FJAC.
C
C       XTOL is a non-negative input variable.  Termination occurs when
C         the relative error between two consecutive iterates is at most
C         XTOL.  Therefore, XTOL measures the relative error desired in
C         the approximate solution.  Section 4 contains more details
C         about XTOL.
C
C       MAXFEV is a positive integer input variable.  Termination occurs
C         when the number of calls to FCN is at least MAXFEV by the end
C         of an iteration.
C
C       ML is a non-negative integer input variable which specifies the
C         number of subdiagonals within the band of the Jacobian matrix.
C         If the Jacobian is not banded or IOPT=1, set ML to at
C         least N - 1.
C
C       MU is a non-negative integer input variable which specifies the
C         number of superdiagonals within the band of the Jacobian
C         matrix.  If the Jacobian is not banded or IOPT=1, set MU to at
C         least N - 1.
C
C       EPSFCN is an input variable used in determining a suitable step
C         for the forward-difference approximation.  This approximation
C         assumes that the relative errors in the functions are of the
C         order of EPSFCN.  If EPSFCN is less than the machine preci-
C         sion, it is assumed that the relative errors in the functions
C         are of the order of the machine precision.  If IOPT=1, then
C         EPSFCN can be ignored (treat it as a dummy argument).
C
C       DIAG is an array of length N.  If MODE = 1 (see below), DIAG is
C         internally set.  If MODE = 2, DIAG must contain positive
C         entries that serve as implicit (multiplicative) scale factors
C         for the variables.
C
C       MODE is an integer input variable.  If MODE = 1, the variables
C         will be scaled internally.  If MODE = 2, the scaling is speci-
C         fied by the input DIAG.  Other values of MODE are equivalent
C         to MODE = 1.
C
C       FACTOR is a positive input variable used in determining the ini-
C         tial step bound.  This bound is set to the product of FACTOR
C         and the Euclidean norm of DIAG*X if nonzero, or else to FACTOR
C         itself.  In most cases FACTOR should lie in the interval
C         (.1,100.).  100. is a generally recommended value.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iteration thereafter and immediately prior
C         to return, with X and FVEC available for printing. Appropriate
C         print statements must be added to FCN(see example).  If NPRINT
C         is not positive, no special calls of FCN with IFLAG = 0 are
C         made.
C
C       INFO is an integer output variable.  If the user has terminated
C         execution, INFO is set to the (negative) value of IFLAG.  See
C         description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  improper input parameters.
C
C         INFO = 1  relative error between two consecutive iterates is
C                   at most XTOL.
C
C         INFO = 2  number of calls to FCN has reached or exceeded
C                   MAXFEV.
C
C         INFO = 3  XTOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         INFO = 4  iteration is not making good progress, as measured
C                   by the improvement from the last five Jacobian eval-
C                   uations.
C
C         INFO = 5  iteration is not making good progress, as measured
C                   by the improvement from the last ten iterations.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       NFEV is an integer output variable set to the number of calls to
C         FCN.
C
C       NJEV is an integer output variable set to the number of calls to
C         JAC. (If IOPT=2, then NJEV is set to zero.)
C
C       R is an output array of length LR which contains the upper
C         triangular matrix produced by the QR factorization of the
C         final approximate Jacobian, stored rowwise.
C
C       LR is a positive integer input variable not less than
C         (N*(N+1))/2.
C
C       QTF is an output array of length N which contains the vector
C         (Q TRANSPOSE)*FVEC.
C
C       WA1, WA2, WA3, and WA4 are work arrays of length N.
C
C
C 4. Successful Completion.
C
C       The accuracy of SNSQ is controlled by the convergence parameter
C       XTOL.  This parameter is used in a test which makes a comparison
C       between the approximation X and a solution XSOL.  SNSQ termi-
C       nates when the test is satisfied.  If the convergence parameter
C       is less than the machine precision (as defined by the function
C       R1MACH(4)), then SNSQ only attempts to satisfy the test
C       defined by the machine precision.  Further progress is not
C       usually possible.
C
C       The test assumes that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian are coded consistently.  If these conditions
C       are not satisfied, then SNSQ may incorrectly indicate conver-
C       gence.  The coding of the Jacobian can be checked by the
C       subroutine CHKDER. If the Jacobian is coded correctly or IOPT=2,
C       then the validity of the answer can be checked, for example, by
C       rerunning SNSQ with a tighter tolerance.
C
C       Convergence Test.  If ENORM(Z) denotes the Euclidean norm of a
C         vector Z and D is the diagonal matrix whose entries are
C         defined by the array DIAG, then this test attempts to guaran-
C         tee that
C
C               ENORM(D*(X-XSOL)) .LE. XTOL*ENORM(D*XSOL).
C
C         If this condition is satisfied with XTOL = 10**(-K), then the
C         larger components of D*X have K significant decimal digits and
C         INFO is set to 1.  There is a danger that the smaller compo-
C         nents of D*X may have large relative errors, but the fast rate
C         of convergence of SNSQ usually avoids this possibility.
C         Unless high precision solutions are required, the recommended
C         value for XTOL is the square root of the machine precision.
C
C
C 5. Unsuccessful Completion.
C
C       Unsuccessful termination of SNSQ can be due to improper input
C       parameters, arithmetic interrupts, an excessive number of func-
C       tion evaluations, or lack of good progress.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1,
C         or IOPT .GT. 2, or N .LE. 0, or LDFJAC .LT. N, or
C         XTOL .LT. 0.E0, or MAXFEV .LE. 0, or ML .LT. 0, or MU .LT. 0,
C         or FACTOR .LE. 0.E0, or LR .LT. (N*(N+1))/2.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by SNSQ.  In this
C         case, it may be possible to remedy the situation by rerunning
C         SNSQ with a smaller value of FACTOR.
C
C       Excessive Number of Function Evaluations.  A reasonable value
C         for MAXFEV is 100*(N+1) for IOPT=1 and 200*(N+1) for IOPT=2.
C         If the number of calls to FCN reaches MAXFEV, then this
C         indicates that the routine is converging very slowly as
C         measured by the progress of FVEC, and INFO is set to 2.  This
C         situation should be unusual because, as indicated below, lack
C         of good progress is usually diagnosed earlier by SNSQ,
C         causing termination with INFO = 4 or INFO = 5.
C
C       Lack of Good Progress.  SNSQ searches for a zero of the system
C         by minimizing the sum of the squares of the functions.  In so
C         doing, it can become trapped in a region where the minimum
C         does not correspond to a zero of the system and, in this situ-
C         ation, the iteration eventually fails to make good progress.
C         In particular, this will happen if the system does not have a
C         zero.  If the system has a zero, rerunning SNSQ from a dif-
C         ferent starting point may be helpful.
C
C
C 6. Characteristics of the Algorithm.
C
C       SNSQ is a modification of the Powell hybrid method.  Two of its
C       main characteristics involve the choice of the correction as a
C       convex combination of the Newton and scaled gradient directions,
C       and the updating of the Jacobian by the rank-1 method of Broy-
C       den.  The choice of the correction guarantees (under reasonable
C       conditions) global convergence for starting points far from the
C       solution and a fast rate of convergence.  The Jacobian is
C       calculated at the starting point by either the user-supplied
C       subroutine or a forward-difference approximation, but it is not
C       recalculated until the rank-1 method fails to produce satis-
C       factory progress.
C
C       Timing.  The time required by SNSQ to solve a given problem
C         depends on N, the behavior of the functions, the accuracy
C         requested, and the starting point.  The number of arithmetic
C         operations needed by SNSQ is about 11.5*(N**2) to process
C         each evaluation of the functions (call to FCN) and 1.3*(N**3)
C         to process each evaluation of the Jacobian (call to JAC,
C         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
C         the timing of SNSQ will be strongly influenced by the time
C         spent in FCN and JAC.
C
C       Storage.  SNSQ requires (3*N**2 + 17*N)/2 single precision
C         storage locations, in addition to the storage required by the
C         program.  There are no internally declared storage arrays.
C
C
C 7. Example.
C
C       The problem is to determine the values of X(1), X(2), ..., X(9),
C       which solve the system of tridiagonal equations
C
C       (3-2*X(1))*X(1)           -2*X(2)                   = -1
C               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
C                                   -X(8) + (3-2*X(9))*X(9) = -1
C C     **********
C
C       PROGRAM TEST(INPUT,OUTPUT,TAPE6=OUTPUT)
C C
C C     Driver for SNSQ example.
C C
C       INTEGER J,IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,
C      *        NWRITE
C       REAL XTOL,EPSFCN,FACTOR,FNORM
C       REAL X(9),FVEC(9),DIAG(9),FJAC(9,9),R(45),QTF(9),
C      *     WA1(9),WA2(9),WA3(9),WA4(9)
C       REAL ENORM,R1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 2
C       N = 9
C C
C C     The following starting values provide a rough solution.
C C
C       DO 10 J = 1, 9
C          X(J) = -1.E0
C    10    CONTINUE
C C
C       LDFJAC = 9
C       LR = 45
C C
C C     Set XTOL to the square root of the machine precision.
C C     Unless high precision solutions are required,
C C     this is the recommended setting.
C C
C       XTOL = SQRT(R1MACH(4))
C C
C       MAXFEV = 2000
C       ML = 1
C       MU = 1
C       EPSFCN = 0.E0
C       MODE = 2
C       DO 20 J = 1, 9
C          DIAG(J) = 1.E0
C    20    CONTINUE
C       FACTOR = 1.E2
C       NPRINT = 0
C C
C       CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,MU,
C      *           EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
C      *           R,LR,QTF,WA1,WA2,WA3,WA4)
C       FNORM = ENORM(N,FVEC)
C       WRITE (NWRITE,1000) FNORM,NFEV,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' NUMBER OF FUNCTION EVALUATIONS',I10 //
C      *        5X,' EXIT PARAMETER',16X,I10 //
C      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
C       END
C       SUBROUTINE FCN(N,X,FVEC,IFLAG)
C       INTEGER N,IFLAG
C       REAL X(N),FVEC(N)
C       INTEGER K
C       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
C       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
C C
C       IF (IFLAG .NE. 0) GO TO 5
C C
C C     Insert print statements here when NPRINT is positive.
C C
C       RETURN
C     5 CONTINUE
C       DO 10 K = 1, N
C          TEMP = (THREE - TWO*X(K))*X(K)
C          TEMP1 = ZERO
C          IF (K .NE. 1) TEMP1 = X(K-1)
C          TEMP2 = ZERO
C          IF (K .NE. N) TEMP2 = X(K+1)
C          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
C    10    CONTINUE
C       RETURN
C       END
C
C       Results obtained with different compilers or machines
C       may be slightly different.
C
C       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
C
C       NUMBER OF FUNCTION EVALUATIONS        14
C
C       EXIT PARAMETER                         1
C
C       FINAL APPROXIMATE SOLUTION
C
C       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
C       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
C       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
C***REFERENCES  POWELL, M. J. D.
C                 A HYBRID METHOD FOR NONLINEAR EQUATIONS.
C                 NUMERICAL METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS,
C                 P. RABINOWITZ, EDITOR.  GORDON AND BREACH, 1970.
C***ROUTINES CALLED  DOGLEG,ENORM,FDJAC1,QFORM,QRFAC,R1MACH,R1MPYQ,
C                    R1UPDT,XERROR
C***END PROLOGUE  SNSQ
      include "precis.fi"
      INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,NJEV
      DIMENSION X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),WA1(N),
     1     WA2(N),WA3(N),WA4(N)
      EXTERNAL FCN
      INTEGER I,IFLAG,ITER,J,JM1,L,NCFAIL,NCSUC,NSLOW1,NSLOW2
      INTEGER IWA(1)
      LOGICAL JEVAL,SING
      DATA ONE,P1,P5,P001,P0001,ZERO
     .              /1.0D0,1.0D-1,5.0D-1,1.0D-3,1.0D-4,0.0D0/
C
C***FIRST EXECUTABLE STATEMENT  SNSQ
      EPSMCH = D1MACH(4)
C
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR.
     1    N .LE. 0 .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0
     2    .OR. ML .LT. 0 .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO
     3    .OR. LDFJAC .LT. N .OR. LR .LT. (N*(N + 1))/2) GO TO 300
      IF (MODE .NE. 2) GO TO 20
      DO 10 J = 1, N
         IF (DIAG(J) .LE. ZERO) GO TO 300
   10    CONTINUE
   20 CONTINUE
C
C     EVALUATE THE FUNCTION AT THE STARTING POINT
C     AND CALCULATE ITS NORM.
C
      IFLAG = 1
      CALL FCN(N,X,FVEC,IFLAG)
      NFEV = 1
      IF (IFLAG .LT. 0) GO TO 300
      FNORM = ENORM(N,FVEC)
C
C     INITIALIZE ITERATION COUNTER AND MONITORS.
C
      ITER = 1
      NCSUC = 0
      NCFAIL = 0
      NSLOW1 = 0
      NSLOW2 = 0
C
C     BEGINNING OF THE OUTER LOOP.
C
   30 CONTINUE
         JEVAL = .TRUE.
C
C        CALCULATE THE JACOBIAN MATRIX.
C
         IF (IOPT .EQ. 2) GO TO 31
C
C        USER SUPPLIES JACOBIAN
C
            CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
            NJEV = NJEV+1
            GO TO 32
C
C        CODE APPROXIMATES THE JACOBIAN
C
   31       IFLAG = 2
            CALL FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,WA1,
     1               WA2)
            NFEV = NFEV + MIN0(ML+MU+1,N)
C
   32    IF (IFLAG .LT. 0) GO TO 300
C
C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
C
         CALL QRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)
C
C        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
C        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
C
         IF (ITER .NE. 1) GO TO 70
         IF (MODE .EQ. 2) GO TO 50
         DO 40 J = 1, N
            DIAG(J) = WA2(J)
            IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   40       CONTINUE
   50    CONTINUE
C
C        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X
C        AND INITIALIZE THE STEP BOUND DELTA.
C
         DO 60 J = 1, N
            WA3(J) = DIAG(J)*X(J)
   60       CONTINUE
         XNORM = ENORM(N,WA3)
         DELTA = FACTOR*XNORM
         IF (DELTA .EQ. ZERO) DELTA = FACTOR
   70    CONTINUE
C
C        FORM (Q TRANSPOSE)*FVEC AND STORE IN QTF.
C
         DO 80 I = 1, N
            QTF(I) = FVEC(I)
   80       CONTINUE
         DO 120 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) GO TO 110
            SUM = ZERO
            DO 90 I = J, N
               SUM = SUM + FJAC(I,J)*QTF(I)
   90          CONTINUE
            TEMP = -SUM/FJAC(J,J)
            DO 100 I = J, N
               QTF(I) = QTF(I) + FJAC(I,J)*TEMP
  100          CONTINUE
  110       CONTINUE
  120       CONTINUE
C
C        COPY THE TRIANGULAR FACTOR OF THE QR FACTORIZATION INTO R.
C
         SING = .FALSE.
         DO 150 J = 1, N
            L = J
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 140
            DO 130 I = 1, JM1
               R(L) = FJAC(I,J)
               L = L + N - I
  130          CONTINUE
  140       CONTINUE
            R(L) = WA1(J)
            IF (WA1(J) .EQ. ZERO) SING = .TRUE.
  150       CONTINUE
C
C        ACCUMULATE THE ORTHOGONAL FACTOR IN FJAC.
C
         CALL QFORM(N,N,FJAC,LDFJAC,WA1)
C
C        RESCALE IF NECESSARY.
C
         IF (MODE .EQ. 2) GO TO 170
         DO 160 J = 1, N
            DIAG(J) = MAX(DIAG(J),WA2(J))
  160       CONTINUE
  170    CONTINUE
C
C        BEGINNING OF THE INNER LOOP.
C
  180    CONTINUE
C
C           IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
C
            IF (NPRINT .LE. 0) GO TO 190
            IFLAG = 0
            IF (MOD(ITER-1,NPRINT) .EQ. 0) CALL FCN(N,X,FVEC,IFLAG)
            IF (IFLAG .LT. 0) GO TO 300
  190       CONTINUE
C
C           DETERMINE THE DIRECTION P.
C
            CALL DOGLEG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)
C
C           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
C
            DO 200 J = 1, N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J)*WA1(J)
  200          CONTINUE
            PNORM = ENORM(N,WA3)
C
C           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
C
            IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
C
C           EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
C
            IFLAG = 1
            CALL FCN(N,WA2,WA4,IFLAG)
            NFEV = NFEV + 1
            IF (IFLAG .LT. 0) GO TO 300
            FNORM1 = ENORM(N,WA4)
C
C           COMPUTE THE SCALED ACTUAL REDUCTION.
C
            ACTRED = -ONE
            IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C
C           COMPUTE THE SCALED PREDICTED REDUCTION.
C
            L = 1
            DO 220 I = 1, N
               SUM = ZERO
               DO 210 J = I, N
                  SUM = SUM + R(L)*WA1(J)
                  L = L + 1
  210             CONTINUE
               WA3(I) = QTF(I) + SUM
  220          CONTINUE
            TEMP = ENORM(N,WA3)
            PRERED = ZERO
            IF (TEMP .LT. FNORM) PRERED = ONE - (TEMP/FNORM)**2
C
C           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
C           REDUCTION.
C
            RATIO = ZERO
            IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED
C
C           UPDATE THE STEP BOUND.
C
            IF (RATIO .GE. P1) GO TO 230
               NCSUC = 0
               NCFAIL = NCFAIL + 1
               DELTA = P5*DELTA
               GO TO 240
  230       CONTINUE
               NCFAIL = 0
               NCSUC = NCSUC + 1
               IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)
     1            DELTA = MAX(DELTA,PNORM/P5)
               IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5
  240       CONTINUE
C
C           TEST FOR SUCCESSFUL ITERATION.
C
            IF (RATIO .LT. P0001) GO TO 260
C
C           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
C
            DO 250 J = 1, N
               X(J) = WA2(J)
               WA2(J) = DIAG(J)*X(J)
               FVEC(J) = WA4(J)
  250          CONTINUE
            XNORM = ENORM(N,WA2)
            FNORM = FNORM1
            ITER = ITER + 1
  260       CONTINUE
C
C           DETERMINE THE PROGRESS OF THE ITERATION.
C
            NSLOW1 = NSLOW1 + 1
            IF (ACTRED .GE. P001) NSLOW1 = 0
            IF (JEVAL) NSLOW2 = NSLOW2 + 1
            IF (ACTRED .GE. P1) NSLOW2 = 0
C
C           TEST FOR CONVERGENCE.
C
            IF (DELTA .LE. XTOL*XNORM+XTOL .OR. FNORM .EQ. ZERO)INFO = 1
            IF (INFO .NE. 0) GO TO 300
C
C           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
C
            IF (NFEV .GE. MAXFEV) INFO = 2
            IF (P1*MAX(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3
            IF (NSLOW2 .EQ. 5) INFO = 4
            IF (NSLOW1 .EQ. 10) INFO = 5
            IF (INFO .NE. 0) GO TO 300
C
C           CRITERION FOR RECALCULATING JACOBIAN
C
            IF (NCFAIL .EQ. 2) GO TO 290
C
C           CALCULATE THE RANK ONE MODIFICATION TO THE JACOBIAN
C           AND UPDATE QTF IF NECESSARY.
C
            DO 280 J = 1, N
               SUM = ZERO
               DO 270 I = 1, N
                  SUM = SUM + FJAC(I,J)*WA4(I)
  270             CONTINUE
               WA2(J) = (SUM - WA3(J))/PNORM
               WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)
               IF (RATIO .GE. P0001) QTF(J) = SUM
  280          CONTINUE
C
C           COMPUTE THE QR FACTORIZATION OF THE UPDATED JACOBIAN.
C
            CALL R1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)
            CALL R1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)
            CALL R1MPYQ(1,N,QTF,1,WA2,WA3)
C
C           END OF THE INNER LOOP.
C
            JEVAL = .FALSE.
            GO TO 180
  290    CONTINUE
C
C        END OF THE OUTER LOOP.
C
         GO TO 30
  300 CONTINUE
C
C     TERMINATION, EITHER NORMAL OR USER IMPOSED.
C
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG)
      IF (INFO .LT. 0) CALL XERROR( 'SNSQ   -- EXECUTION TERMINATED BECA
     1USE USER SET IFLAG NEGATIVE.',63,1,1)
      IF (INFO .EQ. 0) CALL XERROR( 'SNSQ   -- INVALID INPUT PARAMETER.'
     1 ,34,2,1)
      IF (INFO .EQ. 2) CALL XERROR( 'SNSQ   -- TOO MANY FUNCTION EVALUAT
     1IONS.',40,9,1)
      IF (INFO .EQ. 3) CALL XERROR( 'SNSQ   -- XTOL TOO SMALL. NO FURTHE
     1R IMPROVEMENT POSSIBLE.',58,3,1)
      IF (INFO .GT. 4) CALL XERROR( 'SNSQ   -- ITERATION NOT MAKING GOOD
     1 PROGRESS.',45,1,1)
      RETURN
C
C     LAST CARD OF SUBROUTINE SNSQ.
C
      END
