      SUBROUTINE INDEXI(N,ARRIN,INDX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INDEXI
C
C     Source File: INDEXI.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routines sorts the array ARRIN passively via
C                   the permuation array INDX.  The elements
C                   ARRIN(INDX(I)), I=1, ..., N are in increasing order.
C                   This routine uses a bubble sort.  It should not be used
C                   for large N (N>30), since bubble sorts are not
C                   efficient.  The original INDEXI subroutine was deleted
C                   for two reasons.  1) it was based on copyrighted 
C                   numerical recipes code, 2) it gave correct but
C                   unexpected results in some cases.  (sorted arrays 
C                   containing constant values were reordered)
C
C     Arguments: 
C     INPUT:
C            N     number of elements in N
C            ARRIN array to be passively sorted
C     OUTPUT
C            INDX  permuation vector containing ordering such that
C                  ARRIN(INDX) is in increasing order.
C
C     Revision History:
C        Created:  09/30/1993 gpf
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      INTEGER ARRIN(*), INDX(*), AI, AIP1
      DO 10 I = 1, N
         INDX(I) = I
   10 CONTINUE
    5 CONTINUE
      ISWITCH = 0
      DO 20 I = 1, N-1, 2
         AI = ARRIN(INDX(I))
         AIP1 = ARRIN(INDX(I+1))
         IF(AI.LE.AIP1)GO TO 20
         ISWITCH = 1
         ITEMP = INDX(I)
         INDX(I) = INDX(I+1)
         INDX(I+1) = ITEMP
   20 CONTINUE
      DO 30 I = 2, N-1, 2
         AI = ARRIN(INDX(I))
         AIP1 = ARRIN(INDX(I+1))
         IF(AI.LE.AIP1)GO TO 30
         ISWITCH = 1
         ITEMP = INDX(I)
         INDX(I) = INDX(I+1)
         INDX(I+1) = ITEMP
   30 CONTINUE
      IF(ISWITCH.EQ.1)GO TO 5
      RETURN
      END
