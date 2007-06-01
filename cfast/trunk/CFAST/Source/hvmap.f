      Subroutine HVMAP
C
C     Update History
C
C     created June 14, 1992
C
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"

      DIMENSION ISTACK(100)

C    Construct the array that maps between interior nodes
C    (nodes that dassl solves for) and the entire node array

      Do 10 I = 1, NNODE
        IZHVMAPI(I) = I
   10 Continue

C     Zero out exterior nodes

      Do 20 II = 1, NEXT
        I = HVNODE(2,II)
        IZHVMAPI(I) = 0
   20 Continue

C     Fill in the holes vacated by the exterior nodes

      II = 0
      Do 30 I = 1, NNODE
        If (IZHVMAPI(I).NE.0) Then
          II = II + 1
          IZHVMAPI(II) = IZHVMAPI(I)
        END IF
   30 Continue

C     Construct inverse of izhvmapi

      Do 40 I = 1, NNODE
        IZHVMAPE(I) = -1
   40 Continue
      Do 50 I = 1, NNODE - NEXT
        IZHVMAPE(IZHVMAPI(I)) = I
   50 Continue

C    Construct array that maps between all nodes and exterior nodes

      Do 60 I = 1, NNODE
        IZHVIE(I) = 0
   60 CONTINUE
      Do 70 II = 1, NEXT
        I = HVNODE(2,II)
        IZHVIE(I) = II
   70 CONTINUE

C    Construct array that maps between all nodes and hvac system 
C    number to which they belong

      Do 80 I = 1, NNODE
        IZHVSYS(I) = 0
   80 CONTINUE
      ICURSYS = 0
      IPTR = 0
   90 CONTINUE
      ICURNOD = 0
      Do 100 I = 1, NNODE
        If (IZHVSYS(I).EQ.0) Then
          ICURNOD = I
          Go To 110
        END IF
  100 CONTINUE
  110 CONTINUE
      If (ICURNOD.NE.0) Then
        ICURSYS = ICURSYS + 1
        IPTR = IPTR + 1
        ISTACK(IPTR) = ICURNOD
  120   Continue
        If (IPTR.EQ.0) Go To 90
        ICURNOD = ISTACK(IPTR)
        IPTR = IPTR - 1
        IZHVSYS(ICURNOD) = ICURSYS
        Do 130 J = 1, NCNODE(ICURNOD)
          NXTNODE = IN(ICURNOD,J)
          If (IZHVSYS(NXTNODE).EQ.0) Then
            IPTR = IPTR + 1
            ISTACK(IPTR) = NXTNODE
          END IF
  130   CONTINUE
        GO TO 120
      END IF
      NHVSYS = ICURSYS

C     WE HAVE TO UPDATE NEQUALS.  NEQUALS WAS ORIGINALLY DEFINED IN 
C     offset BUT offset WAS CALLED BEFORE NHVSYS WAS DEFINED.

      NEQUALS = NOFHVPR + NHVSYS*NLSPCT

      DO 140 I = 1, NNODE
         ISYS = IZHVSYS(I)
         DO 150 J = 1, NCNODE(I)
            IB = ICMV(I,J)
            IZHVBSYS(IB) = ISYS
  150    CONTINUE
  140 CONTINUE
      RETURN
      END
