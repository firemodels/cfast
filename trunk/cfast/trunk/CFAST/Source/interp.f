      SUBROUTINE INTERP(X,Y,N,T,ICODE,YINT)
      include "precis.fi"
C*BEG
C
C***  INTERP  GENERIC - THIS ROUTINE INTERPOLATES A TABLE OF
C             NUMBERS FOUND IN THE ARRAYS, X AND Y.
C
C*** SUBROUTINE ARGUMENTS
C
C  INPUT
C  -----
C  X,Y    -  ARRAYS OF SIZE N TO BE INTERPOLATED AT X=T
C  ICODE  -  CODE TO SELECT HOW TO EXTRAPOLATE VALUES IF T
C               IS LESS THAN X(1) OR GREATER THAN X(N).
C            IF ICODE = 1 THEN YINT = Y(1) FOR T < X(1)
C                          AND YINT = Y(N) FOR T > X(N).
C            IF ICODE = 2 THEN YINT IS EVALUATED BY INTERPOLATION
C            IF X(1) < T < X(N) AND BY EXTRAPOLATION IF T < X(1)
C            OR    T > X(N)
C
C  OUTPUT
C  ------
C  YINT   -  INTERPOLATED VALUE OF THE Y ARRAY AT T
C*END
      DIMENSION X(*), Y(*)
      SAVE
      DATA ILAST /1/
      IF (N.EQ.1) THEN
        YINT = Y(1)
        RETURN
      END IF
      IF (T.LE.X(1)) THEN
        IF (ICODE.EQ.1) THEN
          YINT = Y(1)
          RETURN
        ELSE
          IMID = 1
          GO TO 20
        END IF
      END IF
      IF (T.GE.X(N)) THEN
        IF (ICODE.EQ.1) THEN
          YINT = Y(N)
          RETURN
        ELSE
          IMID = N - 1
          GO TO 20
        END IF
      END IF
      IF (ILAST+1.LE.N) THEN
        IMID = ILAST
        IF (X(IMID).LE.T.AND.T.LE.X(IMID+1)) GO TO 20
      END IF
      IF (ILAST+2.LE.N) THEN
        IMID = ILAST + 1
        IF (X(IMID).LE.T.AND.T.LE.X(IMID+1)) GO TO 20
      END IF
      IA = 1
      IZ = N - 1
   10 CONTINUE
      IMID = (IA+IZ) / 2
      IF (T.LT.X(IMID)) THEN
        IZ = IMID - 1
        GO TO 10
      END IF
      IF (T.GE.X(IMID+1)) THEN
        IA = IMID + 1
        GO TO 10
      END IF
   20 CONTINUE
      DYDX = (Y(IMID+1)-Y(IMID)) / (X(IMID+1)-X(IMID))
      YINT = Y(IMID) + DYDX * (T-X(IMID))
      ILAST = IMID
      RETURN
      END
