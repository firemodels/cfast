      SUBROUTINE ENTRFL(TU,TL,FMD,Z,FMZ)
C
C     For the reference for this correlation, see the comments
C     in the routine "firplm."  The offset for the formulation of
C     an equivalent door jet is provided by requiring the plume
C     be long enough to be the appropriate plume for the fire of size
C     QJ.  Note that McCaffrey's units are kilojoules.  Also this should
C     be modified to account for the flat plume verus the round
C     plume in the theory.  What else can we do?
C
C     update history
C
C     July 24, 1990 modified coefiscients so that McCaffrey correlation is
C                   continuous.  the coeff's are calculated the first time
C                   this routine is called.  gpf
C
      include "precis.fi"
      include "cfast.fi"
      LOGICAL FIRSTC
      SAVE FIRSTC, A1, A2, A3, E1, E2, E3, F1, F2
      DATA FIRSTC /.TRUE./
C
C*** DEFINE ASSIGNMENT STATEMENT SUBROUTINES TO COMPUTE THREE PARTS
C    OF CORRELATION
C
      FM1(ZQ) = ZQ ** .566D0
      FM2(ZQ) = ZQ ** .909D0
      FM3(ZQ) = ZQ ** 1.895D0
C
C*** FIRST TIME IN FIRPLM CALCULATE COEFF'S 
C    TO INSURE THAT MCCAFFREY CORRELATION IS CONTINUOUS.
C    THAT IS, FOR A1 = .011, COMPUTE A2, A3 SUCH THAT
C  
C     A1*ZQ**.566 = A2*ZQ**.909  FOR ZQ = .08
C     A2*ZQ**.909 = A3*ZQ**1.895 FOR ZQ = .2
C
      IF (FIRSTC) THEN
C
C*** RESET FLAG SO THIS CODE DOESN'T GET EXECUTED NEXT TIME IN 
C    THIS ROUTINE
C
        FIRSTC = .FALSE.
C
C*** BREAKPOINTS FOR "FORWARD" CORRELATION
C
        T1 = .08D0
        T2 = .20D0
C
C*** COEF'S FOR "FORWARD" CORRELATION
C
        A1 = .011D0
        A2 = A1 * FM1(T1) / FM2(T1)
        A3 = A2 * FM2(T2) / FM3(T2)
C
C*** EXPONENTS FOR "INVERSE" CORRELATION
C
        E1 = 1.0D0 / .566D0
        E2 = 1.0D0 / .909D0
        E3 = 1.0D0 / 1.895D0
C
C*** BREAKPOINTS FOR "INVERSE" CORRELATION
C
        F1 = A1 * FM1(T1)
        F2 = A2 * FM2(T2)
      END IF
C
      XQJ = CP * (TU-TL) * 0.001D0
      QJ = XQJ * FMD
      FMDQJ = 1.D0 / XQJ
      IF (FMDQJ.GE.0.0D0.AND.FMDQJ.LE.F1) THEN
        Z0DQ = (FMDQJ/A1) ** E1
      ELSE IF (FMDQJ.GT.F1.AND.FMDQJ.LE.F2) THEN
        Z0DQ = (FMDQJ/A2) ** E2
      ELSE
        Z0DQ = (FMDQJ/A3) ** E3
      END IF
C
      ZDQ = Z / QJ ** 0.4D0 + Z0DQ
      IF (ZDQ.GT.0.2D0) THEN
        FMZ = A3 * FM3(ZDQ) * QJ
      ELSE IF (ZDQ.GT.0.08D0) THEN
        FMZ = A2 * FM2(ZDQ) * QJ
      ELSE
        FMZ = A1 * FM1(ZDQ) * QJ
      END IF
C
      XX0 = 0.0D0
      FMZ = MAX(XX0,FMZ-FMD)
      RETURN
      END
