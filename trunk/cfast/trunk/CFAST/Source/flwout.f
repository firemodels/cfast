      SUBROUTINE FLWOUT(OUTBUF,FLOW1,FLOW2,FLOW3,FLOW4,FLOW5,FLOW6)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FLWOUT
C
C     Functional Class:  I/O
C
C     Description:  Stuff the flow output after blanking appropriate zeros
C
C     Arguments: 
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "solvprm.fi"
      DIMENSION FLOW(6)
      CHARACTER OUTBUF*(*)

      OUTBUF = ' '
      FLOW(1) = FLOW1
      FLOW(2) = FLOW2
      FLOW(3) = FLOW3
      FLOW(4) = FLOW4
      FLOW(5) = FLOW5
      FLOW(6) = FLOW6
      X1000 = 1000.0D0
      X100 = 100.0D0
      X10 = 10.0D0
      X1 = 1.0D0
      X01 = 0.1D0
      DO 10 I = 1, 6
        IF (FLOW(I).GE.X1000) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5000) FLOW(I)
        ELSE IF (FLOW(I).GE.X100) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5010) FLOW(I)
        ELSE IF (FLOW(I).GE.X10) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5020) FLOW(I)
        ELSE IF (FLOW(I).GE.X1) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5030) FLOW(I)
        ELSE IF (FLOW(I).GE.X01) THEN
          WRITE (OUTBUF(13*(I-1)+1:13*I),5040) FLOW(I)
        ELSE
          WRITE (OUTBUF(13*(I-1)+1:13*I),5000) FLOW(I)
        END IF
        IF (FLOW(I).LE.ATOL) OUTBUF(13*(I-1)+1:13*I) = ' '
   10 CONTINUE
      RETURN
 5000 FORMAT (2X,1PG11.3)
 5010 FORMAT (F6.0,7X)
 5020 FORMAT (F7.1,6X)
 5030 FORMAT (F8.2,5X)
 5040 FORMAT (F9.3,4X)
      END
