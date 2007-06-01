      SUBROUTINE SORTFR(NFIRE,IFROOM,XFIRE,IFRPNT,NM1)
C*RB
C     Routine:  SORTFR
C
C     Function: Sort the two arrays IFROOM and XFIRE into increasing
C               room number in IFROOM.  These are used in this order
C               by the ceiling jet and radiation algorithms
C
C     Inputs:   NFIRE   Number of fires
C     Outputs:  IFROOM  Room numbers for each of the fires
C               XFIRE   Fire related quantities used by other routines.
C                       See routine FIRES for definition.
C               IFRPNT  Pointer array for sorted fire list.
C                       (r,1) = Number of fires in room r
C                       (r,2) = Pointer to beginning element in IFROOM
C                               and XFIRE for fires in room r
C
C     Revision History:
C        Modified: 2/7/93 by GPF:
C                  The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have 
C                  the number of fires as the first subscript.
C*RE
      include "precis.fi"
      include "cparams.fi"
      DIMENSION IFROOM(MXFIRE), XFIRE(MXFIRE,MXFIRP), IPERM(MXFIRE), 
     +    IWORK(MXFIRE), WORK(MXFIRE), IFRPNT(NR,2)
C
C     CREATE A PERMUTATION VECTOR FROM THE LIST OF FIRE ROOMS WHICH IS 
C     ORDERED BY INCREASING ROOM NUMBER
C
      DO 2 I = 1, NFIRE
    2    IPERM(I) = I
      CALL INDEXI(NFIRE,IFROOM,IPERM)
C
C     REORDER THE TWO ARRAYS WITH THE PERMUTATION VECTOR
C
      DO 10 I = 1, NFIRE
        IWORK(I) = IFROOM(IPERM(I))
   10 CONTINUE
      DO 20 I = 1, NFIRE
        IFROOM(I) = IWORK(I)
   20 CONTINUE
C
      DO 50 J = 1, MXFIRP
        DO 30 I = 1, NFIRE
          WORK(I) = XFIRE(IPERM(I),J)
   30   CONTINUE
        DO 40 I = 1, NFIRE
          XFIRE(I,J) = WORK(I)
   40   CONTINUE
   50 CONTINUE
C
C     DO THE POINTER ARRAYS FOR THE RADIATION AND CEILING JET ROUTINES
C
      DO 60 I = 1, NM1
        IFRPNT(I,1) = 0
        IFRPNT(I,2) = 0
   60 CONTINUE
      DO 70 I = 1, NFIRE
        IRM = IFROOM(I)
        IFRPNT(IRM,1) = IFRPNT(IRM,1) + 1
        IF (IFRPNT(IRM,2).EQ.0) IFRPNT(IRM,2) = I
   70 CONTINUE
      DO 80 I = 1, NM1
        IF (IFRPNT(I,2).EQ.0) IFRPNT(I,2) = 1
   80 CONTINUE
      RETURN
      END
