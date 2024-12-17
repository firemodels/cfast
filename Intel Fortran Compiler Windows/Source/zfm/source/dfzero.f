      SUBROUTINE DFZERO(F,B,C,R,RE,AE,IFLAG) 
C***BEGIN PROLOGUE  DFZERO    
C***DATE WRITTEN   700901   (YYMMDD)    
C***REVISION DATE  890421   (YYMMDD)    
C***CATEGORY NO.  F1B    
C***KEYWORDS  BISECTION,DOUBLE PRECISION,NONLINEAR,ROOTS,ZEROS   
C***AUTHOR  SHAMPINE,L.F.,SNLA
C           WATTS,H.A.,SNLA   
C***PURPOSE  Search for a zero of a function F(X) in a given
C            interval (B,C).  It is designed primarily for problems   
C            where F(B) and F(C) have opposite signs.  
C***DESCRIPTION
C    
C       **** Double Precision version of FZERO ****    
C    
C     Based on a method by T J Dekker   
C     written by L F Shampine and H A Watts  
C    
C            DFZERO searches for a zero of a function F(X) between    
C            the given values B and C until the width of the interval 
C            (B,C) has collapsed to within a tolerance specified by   
C            the stopping criterion, ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).    
C            The method used is an efficient combination of bisection 
C            and the secant rule.  
C    
C     Description Of Arguments
C    
C     F,B,C,R,RE and AE are DOUBLE PRECISION input parameters.   
C     B and C are DOUBLE PRECISION output parameters and IFLAG (flagged    
C        by an * below). 
C    
C        F     - Name of the DOUBLE PRECISION valued external function.    
C                This name must be in an EXTERNAL statement in the    
C                calling program.  F must be a function of one double 
C                precision argument.    
C    
C       *B     - One end of the interval (B,C).  The value returned for    
C                B usually is the better approximation to a zero of F.
C    
C       *C     - The other end of the interval (B,C)   
C    
C        R     - A (better) guess of a zero of F which could help in  
C                speeding up convergence.  If F(B) and F(R) have 
C                opposite signs, a root will be found in the interval 
C                (B,R); if not, but F(R) and F(C) have opposite  
C                signs, a root will be found in the interval (R,C);   
C                otherwise, the interval (B,C) will be searched for a 
C                possible root.  When no better guess is known, it is 
C                recommended that r be set to B or C; because if R is 
C                not interior to the interval (B,C), it will be ignored.   
C    
C        RE    - Relative error used for RW in the stopping criterion.
C                If the requested RE is less than machine precision,  
C                then RW is set to approximately machine precision.   
C    
C        AE    - Absolute error used in the stopping criterion.  If the    
C                given interval (B,C) contains the origin, then a
C                nonzero value should be chosen for AE.
C    
C       *IFLAG - A status code.  User must check IFLAG after each call.    
C                Control returns to the user from FZERO in all cases. 
C                XERROR does not process diagnostics in these cases.  
C    
C                1  B is within the requested tolerance of a zero.    
C                   The interval (B,C) collapsed to the requested
C                   tolerance, the function changes sign in (B,C), and
C                   F(X) decreased in magnitude as (B,C) collapsed.   
C    
C                2  F(B) = 0.  However, the interval (B,C) may not have    
C                   collapsed to the requested tolerance.   
C    
C                3  B may be near a singular point of F(X). 
C                   The interval (B,C) collapsed to the requested tol-
C                   erance and the function changes sign in (B,C), but
C                   F(X) increased in magnitude as (B,C) collapsed,i.e.    
C                     abs(F(B out)) .GT. max(abs(F(B in)),abs(F(C in)))    
C    
C                4  No change in sign of F(X) was found although the  
C                   interval (B,C) collapsed to the requested tolerance.   
C                   The user must examine this case and decide whether
C                   B is near a local minimum of F(X), or B is near a 
C                   zero of even multiplicity, or neither of these.   
C    
C                5  Too many (.GT. 500) function evaluations used.    
C***REFERENCES  L. F. SHAMPINE AND H. A. WATTS, *FZERO, A ROOT-SOLVING
C                 CODE*, SC-TM-70-631, SEPTEMBER 1970. 
C               T. J. DEKKER, *FINDING A ZERO BY MEANS OF SUCCESSIVE  
C                 LINEAR INTERPOLATION*, 'CONSTRUCTIVE ASPECTS OF THE 
C                 FUNDAMENTAL THEOREM OF ALGEBRA', EDITED BY B. DEJON 
C                 P. HENRICI, 1969.
C***ROUTINES CALLED  D1MACH   
C***END PROLOGUE  DFZERO 
C
      DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,D1MACH,ER,
     +                 F,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z 
      INTEGER IC,IFLAG,KOUNT  
C    
C     ER IS TWO TIMES THE COMPUTER UNIT ROUNDOFF VALUE WHICH IS  
C     DEFINED HERE BY THE FUNCTION D1MACH.   
C    
C***FIRST EXECUTABLE STATEMENT  DFZERO   
      ER = 2.0D0 * D1MACH(4)  
C    
C     INITIALIZE    
C    
      Z = R 
      IF (R .LE. MIN(B,C)  .OR.  R .GE. MAX(B,C)) Z = C  
      RW = MAX(RE,ER)    
      AW = MAX(AE,0.D0)   
      IC = 0
      T = Z 
      FZ = F(T)  
      FC = FZ    
      T = B 
      FB = F(T)  
      KOUNT = 2  
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FB)) GO TO 1
      C = Z 
      GO TO 2  
    1 IF (Z .EQ. C) GO TO 2 
      T = C 
      FC = F(T)  
      KOUNT = 3  
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FC)) GO TO 2
      B = Z 
      FB = FZ    
    2 A = C 
      FA = FC    
      ACBS = ABS(B-C) 
      FX = MAX(ABS(FB),ABS(FC))    
C    
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4 
C     PERFORM INTERCHANGE
      A = B 
      FA = FB    
      B = C 
      FB = FC    
      C = A 
      FC = FA    
C    
    4 CMB = 0.5D0*(C-B) 
      ACMB = ABS(CMB) 
      TOL = RW*ABS(B) + AW   
C    
C     TEST STOPPING CRITERION AND FUNCTION COUNT  
C    
      IF (ACMB .LE. TOL) GO TO 10  
      IF (FB .EQ. 0.D0) GO TO 11 
      IF (KOUNT .GE. 500) GO TO 14    
C    
C     CALCULATE NEW ITERATE IMPLICITLY AS B+P/Q   
C     WHERE WE ARRANGE P .GE. 0.   
C     THE IMPLICIT FORM IS USED TO PREVENT OVERFLOW.   
C    
      P = (B-A)*FB    
      Q = FA - FB  
      IF (P .GE. 0.D0) GO TO 5  
      P = -P
      Q = -Q
C    
C     UPDATE A AND CHECK FOR SATISFACTORY REDUCTION    
C     IN THE SIZE OF THE BRACKETING INTERVAL.
C     IF NOT, PERFORM BISECTION.   
C    
    5 A = B 
      FA = FB    
      IC = IC + 1  
      IF (IC .LT. 4) GO TO 6  
      IF (8.0D0*ACMB .GE. ACBS) GO TO 8    
      IC = 0
      ACBS = ACMB
C    
C     TEST FOR TOO SMALL A CHANGE  
C    
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7    
C    
C     INCREMENT BY TOLERANCE  
C    
      B = B + SIGN(TOL,CMB)  
      GO TO 9  
C    
C     ROOT OUGHT TO BE BETWEEN B AND (C+B)/2.
C    
    7 IF (P .GE. CMB*Q) GO TO 8    
C    
C     USE SECANT RULE    
C    
      B = B + P/Q  
      GO TO 9  
C    
C     USE BISECTION   (C+B)/2 
C    
    8 B = B + CMB
C    
C     HAVE COMPLETED COMPUTATION FOR NEW ITERATE B
C    
    9 T = B 
      FB = F(T)  
      KOUNT = KOUNT + 1 
C    
C     DECIDE WHETHER NEXT STEP IS INTERPOLATION OR EXTRAPOLATION 
C    
      IF (SIGN(1.0D0,FB) .NE. SIGN(1.0D0,FC)) GO TO 3 
      C = A 
      FC = FA    
      GO TO 3  
C    
C    
C     FINISHED. PROCESS RESULTS FOR PROPER SETTING OF IFLAG 
C    
   10 IF (SIGN(1.0D0,FB) .EQ. SIGN(1.0D0,FC)) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN   
   11 IFLAG = 2
      RETURN   
   12 IFLAG = 3
      RETURN   
   13 IFLAG = 4
      RETURN   
   14 IFLAG = 5
      RETURN   
      END 
