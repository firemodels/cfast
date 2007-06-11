      SUBROUTINE WSET(NUMNODE,NSLAB,TSTOP,WALLDX,WSPLIT,WK,WSPEC,WRHO,
     +    WTHICK,WLEN,WTEMP,TAMB,TEXT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     WSET
C
C     Source File: WSET.SOR
C
C     Functional Class:  
C
C     Description:  Initializes temperature profiles, breakpoints used
C                   in wall conduction calculations.
C
C     Arguments: NUMNODE  Number of nodes in each slab
C                NSLAB    Number of slabs
C                TSTOP    Final simulation time
C                WALLDX   Wall position points
C                WSPLIT   fraction of points assigned to slabs 1, 2 and 3
C                WK       Wall thermal conductivity
C                WSPEC    Wall specific heat
C                WRHO     Wall density
C                WTHICK   Thickness of each slab
C                WLEN     Length of wall
C                WTEMP    Wall temperature profile
C                TAMB     Ambient temperature seen by interior wall
C                TEXT     Ambient temperature seen by exterior wall
C
C     Revision History:
C        Created:  by gpf
C        Modified: 02/7/1993 GPF:
C                  Fixed wall point allocations.
C                  06/14/1995 GPF:
C                  Wall temperature was defined to have a constant value
C                  of TAMB.  Now it is defined to change from TAMB to TEXT
C                  from the interior to exterior of wall. 
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
C
      DIMENSION WALLDX(*), XWALL(100)
      DIMENSION NUMNODE(*), WK(*), WSPEC(*), WRHO(*), WTHICK(*)
      DIMENSION WTEMP(*)
      INTEGER CUMPTS
      DIMENSION NUMPTS(10), CUMPTS(10), XPOS(10)
      DIMENSION WSPLIT(*)
C
      NX = NUMNODE(1)
      XXNX = NX
C      
      NINTX = NX - (NSLAB+1)
      IF (NSLAB.LE.2) THEN
        NSPLIT = (WSPLIT(1)+WSPLIT(2)) * XXNX
      ELSE
        NSPLIT = WSPLIT(1) * XXNX
      END IF
C
C*** calculate total walldepth
C
      XPOS(1) = 0.0D0
      DO 20 ISLAB = 1, NSLAB
        XPOS(ISLAB+1) = XPOS(ISLAB) + WTHICK(ISLAB)
   20 CONTINUE
      WLEN = XPOS(NSLAB+1)
C
C*** calculate break point based on first slab's properties
C
      ERRFC05 = 1.30D0
      XKRHOC = WK(1) / (WSPEC(1)*WRHO(1))
      ALPHA = SQRT(XKRHOC)
      XB = 2.0D0 * ALPHA * SQRT(TSTOP) * ERRFC05 * WLEN
      IF (XB.GT..50D0*WLEN) XB = .5D0 * WLEN
      IF (NSLAB.EQ.1) THEN
C
C*** SET UP WALL NODE LOCATIONS for 1 slab case
C    bunch points at interior and exterior boundary
C
        XXNSPLIT = NSPLIT
        W = 1.0D0 / XXNSPLIT 
        DO 30 I = 1, NSPLIT + 1 
          XXIM1 = I - 1
          XWALL(I) = XB * (XXIM1*W) ** 2
   30   CONTINUE
        W = 1.0D0 / (XXNX-(XXNSPLIT+1.0D0))
        DO 40 I = NSPLIT +2, NX
          II = NX + 1 - I 
          XXIIM1 = II - 1
          XWALL(I) = WLEN - (WLEN-XB) * (XXIIM1*W) ** 2
   40   CONTINUE
        NUMNODE(1+NSLAB) = NINTX
      ELSE
C
C*** SET UP WALL NODE LOCATIONS for multi-slab case.
C    bunch points at interior boundary of first slab,
C    exterior boundary of last slab and uniformly in middle slabs
C
C
C*** calculate number of points interior to each slab
C
        XXNINTX = NINTX
        NUMPTS(1) = WSPLIT(1) * XXNINTX * MIN(XB,WTHICK(1)) / WLEN
        IF (NUMPTS(1).LT.1) NUMPTS(1) = 1
        WMXB = WLEN - XB
        NUMPTS(NSLAB) = WSPLIT(3) * XXNINTX * MIN(WMXB,WTHICK(NSLAB)) / 
     +      WLEN
        IF (NUMPTS(NSLAB).LT.1) NUMPTS(NSLAB) = 1
        ISUM = NINTX - NUMPTS(1) - NUMPTS(NSLAB)
        XXNSLABM2 = NSLAB - 2
        DO 50 I = 2, NSLAB - 1
          NUMPTS(I) = XXNX * WSPLIT(2) * WTHICK(NSLAB) / XXNSLABM2 /WLEN
          IF (NUMPTS(I).LT.1) NUMPTS(I) = 1
          ISUM = ISUM - NUMPTS(I)
   50   CONTINUE
        NUMPTS(1) = NUMPTS(1) + (ISUM-ISUM/2)
        NUMPTS(NSLAB) = NUMPTS(NSLAB) + ISUM / 2
        IF (NUMPTS(NSLAB).LT.1) THEN
          NUMPTS(1) = NUMPTS(1) + NUMPTS(NSLAB) - 1
          NUMPTS(NSLAB) = 1
        END IF
C
C*** copy numpts data into numnode and keep a running total
C
        CUMPTS(1) = 1
        DO 60 ISLAB = 1, NSLAB
          NUMNODE(1+ISLAB) = NUMPTS(ISLAB)
          CUMPTS(ISLAB+1) = CUMPTS(ISLAB) + NUMPTS(ISLAB) + 1
   60   CONTINUE
C
C*** calculate wall positions for first slab (bunched near left)
C
        NINT = NUMPTS(1) + 1
        XXNINT = NINT
        DO 70 I = 1, NINT
          XXIM1 = I - 1
          XWALL(I) = XXIM1 ** 2 * XPOS(2) / XXNINT**2
   70   CONTINUE
C
C*** calculate wall positions for middle slabs (uniform)
C
        DO 90 ISLAB = 2, NSLAB - 1
          IBEG = CUMPTS(ISLAB)
          IEND = CUMPTS(ISLAB+1) - 1
          XXI3 = IEND+1-IBEG
          DO 80 I = IBEG, IEND
            XXI1 = IEND+1-I
            XXI2 = I-IBEG
            XWALL(I) = (XPOS(ISLAB)*XXI1+XPOS(ISLAB+1)*XXI2) / XXI3
   80     CONTINUE
C
C*** keep track of break points
C
   90   CONTINUE
C
C*** calculate wall positions for last slab (bunched near right)
C
        IF (NSLAB.GE.2) THEN
          IBEG = CUMPTS(NSLAB)

!*** include last point for last slab

          IEND = CUMPTS(NSLAB+1)
          XXI3 = IEND - IBEG
          DO 100 I = IBEG, IEND
            XXI1 = IEND - I
            XWALL(I) = XPOS(NSLAB+1) - XXI1 ** 2 * (XPOS(NSLAB+1)-
     +          XPOS(NSLAB)) / XXI3 ** 2
  100     CONTINUE
        END IF
      END IF
C
C*** finally calculate distances between each point
C    these distances are used by cnduct to setup discretization
C    tri-diagonal matrix
C
      DO 110 I = 1, NX - 1
        WALLDX(I) = XWALL(I+1) - XWALL(I)
  110 CONTINUE
C
C*** initialize temperature profile.  Note, WTEMP(1)=WTEMP(2) and
C    WTEMP(NX)=WTEMP(NX-1) so DASSL will think that no heat transfer
C    needs to occur to the wall (since dT/dx=0 here)
C
      WTEMP(1) = TAMB
      WTEMP(NX) = TEXT
      DTDW = (TEXT-TAMB)/(XWALL(NX-1)-XWALL(2))
      DO 10 I = 2, NX-1
        WTEMP(I) = TAMB + (XWALL(I)-XWALL(2))*DTDW
   10 CONTINUE
      RETURN
      END
