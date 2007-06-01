      SUBROUTINE CNDUCT(UPDATE,TEMPIN,TEMPOUT,DT,WK,WSPEC,WRHO,WTEMP,
     +    WALLDX,NUMNODE,NSLAB,
     +    WFLUXIN,WFLUXOUT,IWBOUND,TGRAD,TDERV)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CNDUCT
C
C     Source File: CNDUCT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: UPDATE   We don't keep solution unless UPDATE is 1 OR 2
C                TEMPIN   Temperature at interior wall
C                TEMPOUT  Temperature at exterior wall, not used now
C                DT       Time step interval from last valid solution point
C                WK       Wall thermal conductivity
C                WSPEC    Wall specific heat
C                WRHO     Wall density
C                WTEMP    Wall temperature profile
C                WALLDX   Wall position points
C                NUMNODE  Number of nodes in each slab
C                NSLAB    Number of slabs
C                WFLUXIN  Flux striking interior wall
C                WFLUXOUT Flux striking exterior wall
C                IWBOUND  Type of boundary condition for exterior wall
C                         (1=constant temperature, 2=insulated,
C                          3=flux based on ambient temperature on outside wall
C                          4=flux on both interior and exterior walls)
C                TGRAD    Temperature gradient
C                TDERV    Partial of Temperature gradient with
C                          respect to wall surface temperature.  This
C                          number is used to calculate wall Jacobian elements.
C
C     Revision History:
C        Created:  gpf
C        Modified by GPF 2/5/93
C               Added partial derivative calculations for use with
C               the reduced Jacobian option.  
C        Modified by GPF 2/25/94
C               Activated the TEMPOUT parameter for the room to room 
C               heat transfer option
C        Modified by GPF 10/10/94
C               Added fourth boundary condition option, flux on both
C               sides of wall.  This is needed when we do a full 
C               numerical conduction calculation of targets.  Currently,
C               we assume that targets are thin surfaces, ie we calculate
C               a temperature that results in a net flux of zero
C               striking the target.
C        Modified by GPF 4/26/95
C               Removed TAMB from argument list
C        Modified by GPF 6/28/95
C               increased local array size to 2*30=60 points.
C                
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C
C*** DECLARE LOCAL VARIABLES
C
      PARAMETER (NN = 60)
      DIMENSION A(NN), B(NN), C(NN), TNEW(NN), TDERIV(NN), DDIF(3), 
     . TGRAD(2), WK(*), WSPEC(*), WRHO(*), WTEMP(*), WALLDX(*), 
     . NUMNODE(*)
      INTEGER UPDATE

      NX = NUMNODE(1)

C*** construct right hand side (rhs) of tri-diagonal system for interior 
C    nodes.  rhs at boundary and slab break points are defined farther down.

        
      DO 10 I = 2, NX - 1
        TNEW(I) = WTEMP(I)
   10 CONTINUE

C*** set up tri-diagonal coefficient matrix

C*** setup first row
      IF(IWBOUND.NE.4)THEN
         A(1) = 1.0D0
         B(1) = 0.0D0
         C(1) = 0.0D0
         TNEW(1) = TEMPIN
        ELSE
         A(1) = 1.0D0
         B(1) = 0.0D0
         C(1) = -1.0D0
         TNEW(1) = WALLDX(1) * WFLUXIN / WK(1)
      ENDIF

C*** do interior points for each slab

      IEND = 0
      DO 30 ISLAB = 1, NSLAB
        NINTX = NUMNODE(1+ISLAB)
        XKRHOC = WK(ISLAB) / (WSPEC(ISLAB)*WRHO(ISLAB))
        S = 2.0D0 * DT * XKRHOC
        IBEG = IEND + 2
        IEND = IBEG + NINTX - 1
        DO 20 I = IBEG, IEND
          HI = WALLDX(I)
          HIM1 = WALLDX(I-1)
          A(I) = 1.0D0 + S / (HI*HIM1)
          B(I) = -S / (HIM1*(HI+HIM1))
          C(I) = -S / (HI*(HI+HIM1))
   20   CONTINUE
   30 CONTINUE

C*** do break points between each slab

      IBREAK = 1
      DO 40 ISLAB = 2, NSLAB
        NINTX = NUMNODE(ISLAB)
        IBREAK = IBREAK + NINTX + 1
        B(IBREAK) = WSPEC(ISLAB-1) * WRHO(ISLAB-1) / WALLDX(IBREAK-1)
        C(IBREAK) = WSPEC(ISLAB) * WRHO(ISLAB) / WALLDX(IBREAK)
        A(IBREAK) = -(B(IBREAK)+C(IBREAK))
        TNEW(IBREAK) = 0.0D0
   40 CONTINUE

C*** setup last row, note: last row depends on form of boundary condition

      IF (IWBOUND.EQ.1) THEN

C*** constant temperature boundary condition
C    (if we ever solve for both interior and exterior wall temperatures
C     then use change TNEW(NX) = tamb to TNEW(NX) = tempout)

        A(NX) = 1.0D0
        B(NX) = 0.0D0
        C(NX) = 0.0D0
        TNEW(NX) = TEMPOUT
      ELSE IF (IWBOUND.EQ.2) THEN

C*** insulated boundary condition

        A(NX) = 1.0D0
        B(NX) = -1.0D0
        C(NX) = 0.0D0
        TNEW(NX) = 0.0D0
      ELSE IF (IWBOUND.EQ.3.OR.IWBOUND.EQ.4) THEN

C*** flux boundary condition (using lagged temperatures

        A(NX) = 1.0D0
        B(NX) = -1.0D0
        C(NX) = 0.0D0
        TNEW(NX) = WALLDX(NX-1) * WFLUXOUT / WK(NSLAB)
      END IF
C     
C*** NOW PERFORM AN L-U FACTORIZATION OF THIS MATRIX (see atkinson p.455)
C    NOTE: MATRIX IS DIAGONALLY DOMINANT SO WE DON'T HAVE TO PIVOT

C*** note we do the following in case a(1) is not 1

      C(1) = C(1) / A(1)
      DO 50 I = 2, NX - 1
        A(I) = A(I) - B(I) * C(I-1)
        C(I) = C(I) / A(I)
   50 CONTINUE
      A(NX) = A(NX) - B(NX) * C(NX-1)
      DO 61 I = 1, NX
         TDERIV(I) = 0.0D0
   61 CONTINUE
      TDERIV(1) = 1.0D0

C*** NOW CONSTRUCT GUESS AT NEW TEMPERATURE PROFILE

C*** FORWARD SUBSTITION

      TNEW(1) = TNEW(1) / A(1)
      TDERIV(1) = TDERIV(1)/A(1)
      DO 60 I = 2, NX
        TNEW(I) = (TNEW(I)-B(I)*TNEW(I-1)) / A(I)
        TDERIV(I) = (TDERIV(I)-B(I)*TDERIV(I-1)) / A(I)
   60 CONTINUE

C*** BACKWARD SUBSTITION

      DO 70 I = NX - 1, 1, -1
        TNEW(I) = TNEW(I) - C(I) * TNEW(I+1)
        TDERIV(I) = TDERIV(I) - C(I) * TDERIV(I+1)
   70 CONTINUE

C***  WE DON'T KEEP solution UNLESS UPDATE IS 1 OR 2

      IF (UPDATE.NE.0) THEN
        DO 80 I = 1, NX
          WTEMP(I) = TNEW(I)
   80   CONTINUE
      END IF

C*** estimate temperature gradient at wall surface by constructing a 
C    quadratic polynomial that interpolates first three data points in 
C    the temperature profile.  we will use divided differences.

C*** first divided difference

      DDIF(1) = (TNEW(2)-TNEW(1)) / WALLDX(1)
      DDIF(2) = (TNEW(3)-TNEW(2)) / WALLDX(2)

C*** second divided difference

      DDIF(2) = (DDIF(2)-DDIF(1)) / (WALLDX(1)+WALLDX(2))

      TGRAD(1) = (DDIF(1)-DDIF(2)*WALLDX(1))
      TGRAD(2) = (TNEW(2)-TNEW(1)) / WALLDX(1)
      TDERV = TDERIV(2)
      RETURN
      END
