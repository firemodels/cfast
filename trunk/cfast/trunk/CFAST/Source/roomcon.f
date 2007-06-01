      SUBROUTINE ROOMCON(TSEC)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     ROOMCON
C
C     Source File: ROOMCON.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine determines whether flow from each room can
C                   reach the outside (perhaps through intermediate rooms)
C                   via horizontal or vertical vents.  If a room is 
C                   isolated from the outside then SNSQE has trouble finding
C                   an initial pressure solution.
C
C     Revision History:
C        Created:  1/31/96 by GPF
C        Modified: 2/10/97 by gpf
C                  use o(n) vent datastructures instead of o(n**2)
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "vents.fi"

      DIMENSION VNTOPN(NV)
      INTEGER ROOMC(NR,NR), TEMPMAT(NR,NR)
      INTEGER TOPRM, BOTRM
      DATA TOPRM /1/, BOTRM /2/

C*** initially assume that no rooms are connected
      DO 10 I = 1, N
        DO 20 J = 1, N
           ROOMC(I,J) = 0
   20   CONTINUE
        ROOMC(I,I) = 1
   10 CONTINUE

C*** check horizontal vent flow

      DO 30 I = 1, NVENTS
        IROOM1 = IZVENT(I,1)
        IROOM2 = IZVENT(I,2)
        IK = IZVENT(I,3)
        IM = MIN(IROOM1,IROOM2)
        IX = MAX(IROOM1,IROOM2)
        factor2 = qchfraction(qcvh,ijk(im,ix,ik),tsec)
        HEIGHT = ZZVENT(I,2) - ZZVENT(I,1)
        WIDTH = ZZVENT(I,3)
        avent = factor2 * height * width
        IF(AVENT.NE.0.0D0)THEN
          ROOMC(IROOM1,IROOM2) = 1
          ROOMC(IROOM2,IROOM1) = 1
        ENDIF
   30 CONTINUE

C*** check vertical vent flow

      DO 50 I = 1, NVVENT
         IROOM1 = IVVENT(I,TOPRM)
         IROOM2 = IVVENT(I,BOTRM)
         IF(VVAREA(IROOM1,IROOM2).NE.0.0D0)THEN
           ROOMC(IROOM1,IROOM2) = 1
           ROOMC(IROOM2,IROOM1) = 1
         ENDIF
   50 CONTINUE


C*** construct ROOMC**MATITER where MATITER > N
C    Note:  ROOMC is a transitiion matrix (from markov chain theory).
C           That is, ROOMC(i,j) is zero if there no connection between
C           room and room j.  Similarly, ROOMC(i,j) is one if there
C           is a connection between these two rooms.  ROOMC is symmetric.
C           The matrix ROOMC**2 is tells us whether flow can get from
C           room i to room j in two steps.  Since there are only N rooms,
C           ROOMC**N tells us whether any given room is connected to
C           any other room in N steps.  The entries ROOMC**N(i,n) then
C           indicates whether a room is connected to the outside (perhaps
C           through several other intermediate rooms).

      MATITER = 1
      DO 60 I = 1, N
        IF(N.LE.MATITER)GO TO 70
        CALL MAT2MULT(ROOMC,TEMPMAT,NR,N,matiter)
        MATITER = MATITER*2
   60 CONTINUE
   70 CONTINUE

      DO 80 I = 1, NM1
        IF(ROOMC(I,N).NE.0)THEN
          IZCON(I) = .TRUE.
         ELSE
          IZCON(I) = .FALSE.
        ENDIF
   80 CONTINUE


      RETURN
      END
      SUBROUTINE MAT2MULT(MAT1,MAT2,IDIM,N,MATITER)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     MAT2MULT
C
C     Source File: ROOMCON.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Given an NxN matrix MAT1 whose elements are either 0 or 1,
C                   this routine computes the matrix MAT1**2 and 
C                   returns the results in MAT1 (after scaling non-zero entries
C                   to 1).
C
C     Revision History:
C        Created:  1/31/96 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      DIMENSION MAT1(IDIM,N),MAT2(IDIM,N)
      DO 10 I = 1, N
        DO 20 J = 1, N
           MAT2(I,J) = IDOT(MAT1(I,1),IDIM,MAT1(1,J),1,N)
           IF(MAT2(I,J).GE.1)MAT2(I,J) = 1
   20   CONTINUE
   10 CONTINUE
      DO 30 I = 1, N
        DO 40 J = 1, N
          MAT1(I,J) = MAT2(I,J)
   40   CONTINUE
   30 CONTINUE
      RETURN
      END
      INTEGER FUNCTION IDOT(IX,INX,IY,INY,N)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     IDOT
C
C     Source File: ROOMCON.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine computes the integer dot product of two
C                   integer vectors.
C
C     Revision History:
C        Created:  1/31/96 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      INTEGER IX(*), IY(*)
      IDOT = 0
      II = 1 - INX
      JJ = 1 - INY
      DO 10 I = 1, N
        II = II + INX
        JJ = JJ + INY
        IDOT = IDOT + IX(II)*IY(JJ)
   10 CONTINUE
      RETURN
      END
