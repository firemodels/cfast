      SUBROUTINE DATYPE(CRD,N1,N2,DTYPE)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DATYPE
C
C     Source File: DATYPE.SOR
C
C     Functional Class:  UTILITY
C
C     Description:  This routine determines the data type of a string.
C                   The character string is examined between given starting and
C                   ending positions to determine if the substring is an
C                   integer, a real number, or a non-numeric string.
C
C     Arguments: CRD    String containing number to be typed
C                N1     Starting position
C                N2     Ending position
C                DTYPE  Returned type (1=real, 2=integer, 3=non-numeric)
C
C     Revision History:
C        11/16/1992 by RDP, changed specification of CRD to character string.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      LOGICAL PERIOD, EFMT
      INTEGER N1, N2, DTYPE
      CHARACTER CRD*(*), NUM(12)*1
      DATA NUM /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', 
     +    '-'/
      PERIOD = .FALSE.
      EFMT = .FALSE.
C
C  DETERMINE DATA TYPE.  ASSUME A NUMERIC ENTRY UNTIL A NON-NUMERIC
C  CHARACTER IS FOUND.
C
      DO 20 I = N1, N2
        IF (CRD(I:I).EQ.'.') THEN
          IF (PERIOD) THEN
C
C  PERIOD IN STRING - NON NUMERIC
C
            GO TO 30
          ELSE
            PERIOD = .TRUE.
          END IF
          GO TO 20
        END IF
C
C     CHECK FOR DIGITS
C
        DO 10 J = 1, 12
          IF (CRD(I:I).EQ.NUM(J)) GO TO 20
   10   CONTINUE
        IF (INDEX('EeDd',CRD(I:I)).EQ.0.OR.EFMT) GO TO 30
        EFMT = .TRUE.
   20 CONTINUE
C
C  DETERMINE TYPE OF NUMERIC ENTRY
C
      IF (PERIOD.OR.EFMT) THEN
C
C  REAL
C
        DTYPE = 1
      ELSE
C
C  INTEGER
C
        DTYPE = 2
      END IF
      RETURN
C
C  NON-NUMERIC
C
   30 DTYPE = 3
      RETURN
      END
