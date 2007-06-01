      SUBROUTINE UPPERALL(FROM,TO)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPPERALL
C
C     Source File: UPPERALL.SOR
C
C     Functional Class:  
C
C     Description:  Convert a character string to upper case
C
C     Arguments: FROM
C                TO
C
C     Revision History:
C        Created:  5/5/1995 at 13:51 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      CHARACTER*(*) FROM, TO
      CHARACTER*1 C
      EXTERNAL LENGTH
      NFROM = LENGTH (FROM)
      NTO = LEN(TO)
      NN = MIN(NFROM,NTO)
      DO 10 I = 1, NN
         C = FROM(I:I)
         IF(C.GE.'a'.AND.C.LE.'z')THEN
            C = CHAR(ICHAR(C) + ICHAR('A')-ICHAR('a'))
         ENDIF
         TO(I:I) = C
   10 CONTINUE
      IF(NTO.GT.NN)TO(NN+1:NTO)=' '
      RETURN
      END
