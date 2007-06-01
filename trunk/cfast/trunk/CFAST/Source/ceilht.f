      SUBROUTINE CEILHT(MPLUME,QCONV,ATC,TL,TU,TW,XW,YW,ZC,AXF,AYF,ZF,
     +    ZLAY,RHOL,RHOU,CJETOPT,XD,YD,ZD,ND,QCEIL,QFCLGA,
     +    QFWLA,QFWUA,TD,VD,TDMAX,VDMAX,DDMAX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CEILHT
C
C     Functional Class:  
C
C     Description:  
C       THIS SUBROUTINE CALCULATES CONVECTIVE HEAT TRANSFER TO
C	THE UNIFORM TEMPERATURE CEILING ABOVE A FIRE IN A PARALLEL-
C	OPIPED ROOM WITH A TWO-LAYER FIRE ENVIRONMENT.  ALSO CALCU-
C	LATED IS THE TOTAL RATE OF HEAT TRANSFER TO THE WALLS AND
C	THE AVERAGE FLUX TO THE UPPER AND LOWER PORTIONS OF THE
C	WALLS.

C   	INPUT
C
C	MPLUME		MASS FLOW RATE IN THE PLUME AT ZLAY IF ZF < ZLAY [KG/S]
C         ND    NUMBER OF DETECTORS
C	 QCONV		PORTION OF FIRE ENERGY RELEASE RATE CONVECTED IN PLUME [W]
C	    TC    AVERAGE TEMPERATURE OF CEILING [K]
C	 TL,TU    TEMPERATURE OF LOWER, UPPER LAYER [K]
C	    TW    AVERAGE TEMPERATURE OF WALL SURFACES [K]
C   XD,YD,ZD    X, Y, AND Z POSITIONS OF THE DETECTORS
C   XW,YW,ZC    CO-ORDINATES OF ROOM CORNER DIAGONALLY OPPOSITE TO ORIGIN 
C               OF AXES (ORIGIN IS AT A CORNER ON THE FLOOR, WITH X, Y 
C               AXES ALONG WALL/FLOOR JUNCTION AND Z AXES UPWARD) [M]
C   XF,YF,ZF    CO-ORDINATES OF CENTER OF BASE OF FIRE [M]
C       ZLAY    ELEVATION ABOVE FLOOR OF LAYER INTERFACE [M]
C  RHOL,RHOU    DENSITY OF LOWER, UPPER LAYER [KG/M**3]
C    CJETOPT    HEAT TRANSFER IS NOT CALCULATED IF CJETOPT=2
C
C	OUTPUT
C	-----
C
C      QCEIL    RATE OF HEAT TRANSFER TO CEILING [W]
C     QFCLGA		AVERAGE FLUX OF HEAT TRANSFER TO CEILING [W/M**2]
C QFWLA,QFWUA   AVERAGE FLUX OF HEAT TRANSFER TO LOWER AND
C               UPPER PORTIONS OF THE WALL SUFACES [W/M**2]
C         TD    TEMPERATURE OF JET AT XD,YD,ZD LOCATIONS
C         VD    VELOCITY OF JET AT XD,YD,ZD LOCATIONS
C
C***	SOME OTHER DEFINITIONS AND FIXED INPUT IN THIS SUBROUTINE

C      ALPHA    TU/TL
C     AWL(N)    AREA OF LOWER-LAYER PORTION OF WALL SEGMENT N
C     AWU(N)    AREA OF UPPER-LAYER PORTION OF WALL SEGMENT N
C         CP    SPECIFIC HEAT OF AIR AT CONSTANT PRESSURE [KJ/(KG*K)]
C         CT    9.115, CONSTANT IN POINT SOURCE PLUME EQN.
C          G    9.8, ACCELERATION OF GRAVITY [M/S**2]
C          H    IF LAYER INTERFACE IS BELOW CEILING: DISTANCE
C               OF CONTINUATION SOURCE BELOW CEILING; IF LAYER INTERFACE 
C               IS AT CEILING: DISTANCE OF EQUIVALENT SOURCE BELOW 
C               CEILING [M]
C      QCONT    STRENGTH OF CONTINUATION SOURCE [W]
C        QEQ    DIMENSIONLESS STRENGTH OF PLUME AT ZLAY
C         QH    DIMENSIONLESS STRENGTH OF PLUME AT ZC
C   QFWCL(N)    AVERAGE HEAT FLUX TO WALL NEAR CORNER N IN THE 
C               LOWER LAYER [W/M**2]
C   QFWCU(N)    AVERAGE HEAT FLUX TO WALL NEAR CORNER N IN
C               THE UPPER LAYER [W/M**2]
C  QFWLAN(N)    AVERAGE HEAT FLUX TO PORTION OF WALL SEGMENT
C               N IN THE LOWER LAYER [W/M**2]
C  QFWUAN(N)    AVERAGE HEAT FLUX TO PORTION OF WALL SEGMENT
C               N IN THE UPPER LAYER [W/M**2]
C   QFWSL(N)    AVERAGE HEAT FLUX TO PORTION OF WALL IN THE LOWER LAYER 
C               ALONG THE LINE PASSING THROUGH STAGNATION POINT N [W/M**2]
C   QFWSU(N)    AVERAGE HEAT FLUX TO PORTION OF WALL IN THE UPPER LAYER 
C               ALONG THE LINE PASSING THROUGH STAGNATION POINT N [W/M**2]
C      QFWST    HEAT TRANSFER FLUX TO WALL AT A WALL/CEILING-JET STAGNATION 
C               POINT [W/M**2]
C         PR    PRANDTL NUMBER
C      RC(N)    DISTANCE FROM PLUME/CEILING IMPINGEMENT POINT TO CORNER N [M]
C      RS(N)    DISTANCE FROM PLUME/CEILING IMPINGEMENT POINT TO WALL 
C               STAGNATION POINT N [M]
C        THT    TEMPERATURE OF AMBIENT IN UNCONFINED CEILING HEAT TRANSFER 
C               PROBLEM, EITHER TL OR TU [K] 
C        ZEQ    ELEVATION ABOVE FLOOR OF EQUIVALENT SOURCE IN LOWER LAYER [M]
C         ZS    ELEVATION ABOVE FLOOR OF CONTINUATION SOURCE [M]
C       TDMAX   MAXIMUM TEMPERATURE OF THE CEILING JET
C       VDMAX   MAXIMUM VELOCITY OF THE CEILING JET
C       DDMAX   ESTIMATE OF CEILING JET DEPTH AT R/H = .2
C               (GIVEN BY:  DDMAX/(.23*DELTA) = 2)
C       
C
C     Revision History:
C     GPF 10/14/93   CALCULATED VELOCITY AND TEMPERATURES AT DETECTOR
C                    LOCATIONS. NOTE, WHEN THERE IS MORE THAN ONE FIRE
C                    IN A ROOM WE SAVE THE 'WORST CASE', IE WE STORE
C                    THE CEILING JET VELOCITY AND TEMPERATURE FROM
C                    THE FIRE THAT CAUSES THESE VALUES TO BE THE LARGEST.
C                    ADDED OPTION FLAG, CJETOPT.  
C
C                    HEAT TRANSFER BETWEEN CEILING JET AND WALLS IS 
C                    NOT CALCULATED IF IF CJETOPT=2.
C     GPF 3/29/95    Disabled heat transfer to lower wall surfaces.  Previously,
C                    heat from the ceiling jet was added to the lower walls but
C                    not to the lower layer.  This inconsistency resulted in an
C                    unrealistic drop in lower layer temperatures.
C     GPF 4/24/95    removed references to qwall, since it was not
C                    being used
C     GPF 7/22/96    added maximum temperature, velocity and depth for hybrid option
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
C 
C********************************************************************
C   	This subroutine and the function QFCLG used in its called subroutines use the common blocks AINTCH
C********************************************************************
C
      EXTERNAL QFCLG
      INTEGER CJETOPT
      DIMENSION AWL(8), AWU(8), QFWCL(4), QFWCU(4), QFWLAN(8), 
     +    QFWUAN(8), QFWSL(4), QFWSU(4), RC(4), RS(4)
      DIMENSION XD(*), YD(*), ZD(*), TD(*), VD(*)
      COMMON /AINTCH/ H, HTCT, THT, THTQHP, C1, C2, C3, XF, YF, TC
      SAVE /AINTCH/
      LOGICAL FIRST
      SAVE FIRST, PI, G, CT, CP, PR, GSQRT, X2D3, X1D3, RK1
      DATA FIRST /.TRUE./
C
      XX0 = 0.0D0
      QCEIL = XX0
      QFCLGA = XX0
      QFWLA = XX0
      QFWUA = XX0
      IF (MPLUME.EQ.XX0.OR.QCONV.EQ.XX0) THEN
         DO 10 ID = 1, ND
            TD(ID) = MAX(TU,TD(ID))
            VD(ID) = MAX(XX0,VD(ID))
   10    CONTINUE
         GO TO 9999
      END IF
C
      IF (FIRST) THEN
         FIRST = .FALSE.
         ONE = 1.0D0
         PI = 4.0D0 * ATAN(ONE)
         G = 9.80D0
         CT = 9.115D0
         CP = 1000.0D0
         PR = .70D0
         GSQRT = SQRT(G)
         X2D3 = 2.0D0 / 3.0D0
         X1D3 = 1.0D0 / 3.0D0
         TWO = 2.0D0
         RK1 = (.23D0/.77D0) * LOG(SQRT(TWO)-ONE)
      END IF
      XF = AXF
      YF = AYF
      RFMIN = .20D0 * (ZC-ZF)
      IF (RFMIN.LT.XW/2.0D0) THEN
         IF (XF.LT.RFMIN) XF = RFMIN
         IF (XF.GT.XW-RFMIN) XF = XW - RFMIN
      ELSE
         XF = XW / 2.0D0
      END IF
      IF (RFMIN.LT.YW/2.0D0) THEN
         IF (YF.LT.RFMIN) YF = RFMIN
         IF (YF.GT.YW-RFMIN) YF = YW - RFMIN
      ELSE
         YF = YW / 2.0D0
      END IF
      TC = ATC
      ALPHA = TU / TL
      IF (ZF.LT.ZLAY) THEN

C     fire is below layer interface

        QEQ = (0.21D0*QCONV/(CP*TL*MPLUME)) ** 1.5D0
        ZEQ = ZLAY - (QCONV/(QEQ*RHOL*CP*TL*GSQRT)) ** (.4D0)
        IF (ZLAY.LT.ZC) THEN

C     layer interface is below ceiling

          ALFM1 = ALPHA - 1.0D0
          IF (ALFM1.NE.XX0) THEN
            SIGMA = -1.0D0 + CT * QEQ ** X2D3 / ALFM1
            A1 = SIGMA / (SIGMA+1.0D0)
            IF (SIGMA.GT.XX0) THEN
              SSQ = SIGMA ** 2
              TOP = 1.04599D0 * SIGMA + 0.360391D0 * SSQ
              BOTTOM = 1.D0 + 1.37748D0 * SIGMA + 0.360391D0 * SSQ
              MFRAC = TOP / BOTTOM
            ELSE
              MFRAC = XX0
              QCEIL = XX0
              QFCLGA = XX0
              QFWLA = XX0
              QFWUA = XX0
              DO 20 ID = 1, ND
                TD(ID) = MAX(TU,TD(ID))
                VD(ID) = MAX(XX0,VD(ID))
   20         CONTINUE
              GO TO 9999
            END IF
          ELSE
            A1 = 1.0D0
            MFRAC = 1.0D0
          END IF
          QCONT = QCONV * A1 * MFRAC
          ZS = ZLAY - (ZLAY-ZEQ) * ALPHA ** .6D0 * MFRAC ** .4D0 / A1 **
     +        .2D0
          THT = TU
          RHOHT = RHOU
        ELSE
C
C*** layer interface is at ceiling
C
          QCONT = QCONV
          ZS = ZEQ
          THT = TL
          RHOHT = RHOL
        END IF
      ELSE
C
C*** fire is at or above layer interface
C
        IF (ZF.LT.ZC) THEN
C
C*** fire is below ceiling
C
          QCONT = QCONV
          ZS = ZF
          THT = TU
          RHOHT = RHOU
        ELSE
C
C*** fire is at ceiling
C
          QCEIL = XX0
          QFCLGA = XX0
          QFWLA = XX0
          QFWUA = XX0
          GO TO 9999
        END IF
      END IF
      H = ZC - ZS
      SQRTGH = SQRT(G*H)
      QH = QCONT / (RHOHT*CP*THT*SQRTGH*H**2)
      QHP = (QH**X1D3)
      HTCT = RHOHT * CP * SQRTGH * QHP
      ANU = (0.04128D-7*THT**2.5D0) / (THT+110.4D0)
      RE = SQRTGH * H * QHP / ANU
      THTQHP = THT * QHP ** 2
      PRP = PR ** X2D3
      C1 = 8.82D0 / (SQRT(RE)*PRP)
      C2 = 5.D0 - 0.284D0 * RE ** 0.2D0
      C3 = 0.283032655D0 / (RE**0.3D0*PRP)
      C4 = 0.94D0 / ((RE**0.42D0)*PR)
C
      RMAX = SQRT(MAX(YF,YW-YF)**2+MAX(XF,XW-XF)**2)
C
C*** MAKE AN INTEGRAL TABLE OF SIZE NTAB FROM ZERO
C    TO RMAX.
C
      NTAB = 20
      CALL MAKTABL(RMAX,NTAB,QFCLG)

C*** DON'T NEED TO COMPUTE THE FOLLOWING IF WE ARN'T COMPUTING
C    CEILING JET HEAT TRANSFER

      IF (CJETOPT.NE.2) THEN
        CALL INT2D(XF,YF,XW,YW,RMAX,QCEIL)
        QFCLGA = QCEIL / (XW*YW)
      ENDIF
C
C***	Now calculate wall heat transfer:
C***	Step 1.
C***	Calculate radii at wall stagnation points:
      RS(1) = YF
      RS(2) = XW - XF
      RS(3) = YW - YF
      RS(4) = XF
C
C*** CALCULATE VELOCITY AND TEMPERATURES OF THE
C    CEILING JET AT VARIOUS LOCATIONS
C
      DO 30 ID = 1, ND
        RD = SQRT((AXF-XD(ID))**2+(AYF-YD(ID))**2)
        RDH = RD / H
        IF (RDH.LE..20D0) THEN
          RDH = .20D0
          RD = RDH * H
        END IF
        V = SQRTGH * QHP
        VMAX = .85D0 * V / RDH ** 1.1D0
        VDMAX = VMAX
        DELTA = .1D0 * H * RDH ** .9D0
        DZ = ZC - ZD(ID)
        ZDEL = DZ / (.23D0*DELTA)
        DDMAX = 2.0D0*0.23D0*DELTA
        IF (ZDEL.LE.1.0D0) THEN
          VCJ = VMAX * ZDEL ** (1.0D0/7.0D0) * (8.0D0-ZDEL) / 7.0D0
        ELSE
          ARG = RK1 * (ZDEL-1.0D0)
          VCJ = VMAX / COSH(ARG) ** 2
        END IF

        CALL INTTABL(RD,RLAMR)
        RLAMR = 2.0D0 * PI * RLAMR / QCONV
        TMAXMTU = 2.6D0 * (1.0D0-RLAMR) * QH ** X2D3 * TU / RDH ** .80D0
     +      - .90D0 * (TC-TU)
        THS = (TC-TU) / TMAXMTU
        IF (ZDEL.LE.1.0D0) THEN
          THTA = THS + 2.0D0 * (1.0D0-THS) * ZDEL - (1.0D0-THS) * ZDEL 
     +        ** 2
        ELSE
          THTA = VCJ / VMAX
        END IF
        TCJ = TU + THTA * TMAXMTU
        TDMAX = TU + TMAXMTU
        TD(ID) = MAX(TCJ,TD(ID))
        VD(ID) = MAX(VCJ,VD(ID))
   30 CONTINUE
 
C*** HEAT TRANSFER BETWEEN CEILING JET AND CEILING IS NOT
C    CALCULATED IF THE C.J. OPTION IS SET TO 2 IN THE FILE SOLVER.INI

      IF (CJETOPT.EQ.2) RETURN

C***	Calculate average heat transfer fluxes to lower and upper 
C***	walls along the vertical lines passing through the four wall/ 
C***	ceiling-jet stagnation points:

      DO 40 N = 1, 4
        RDH = RS(N) / H
        CALL SQFWST(RDH,H,C4,THT,HTCT,THTQHP,TW,QFWSL(N),QFWSU(N),ZC,
     +      ZLAY)
   40 CONTINUE

C***	Step 2.
C***	Calculate radii at room corners:

      RC(1) = SQRT(XF**2+YF**2)
      RC(2) = SQRT((XW-XF)**2+YF**2)
      RC(3) = SQRT((XW-XF)**2+(YW-YF)**2)
      RC(4) = SQRT(XF**2+(YW-YF)**2)

C***	Calculate average heat transfer fluxes to lower and upper

C***	walls along the vertical lines passing through the four room
C***	corners by assuming that the heat transfer there is as along
C***	a line passing through a point of normal ceiling-jet/wall im-
C***	pingement.

      DO 50 N = 1, 4
        RDH = RC(N) / H
        CALL SQFWST(RDH,H,C4,THT,HTCT,THTQHP,TW,QFWCL(N),QFWCU(N),ZC,
     +      ZLAY)
   50 CONTINUE
C
C***	Step 3.
C***	Calculate the average heat transfer fluxes to the lower and
C***	upper portions of the eight wall segments bounded by the room
C***	corners and the the vertical lines passing through the points
C***	of normal wall/ceiling-jet impingement.
C
      QFWUAN(1) = (QFWCU(1)+QFWSU(1)) * .5D0
      QFWLAN(1) = (QFWCL(1)+QFWSL(1)) * .5D0
      QFWUAN(2) = (QFWCU(2)+QFWSU(1)) * .5D0
      QFWLAN(2) = (QFWCL(2)+QFWSL(1)) * .5D0
      QFWUAN(3) = (QFWCU(2)+QFWSU(2)) * .5D0
      QFWLAN(3) = (QFWCL(2)+QFWSL(2)) * .5D0
      QFWUAN(4) = (QFWCU(3)+QFWSU(2)) * .5D0
      QFWLAN(4) = (QFWCL(3)+QFWSL(2)) * .5D0
      QFWUAN(5) = (QFWCU(3)+QFWSU(3)) * .5D0
      QFWLAN(5) = (QFWCL(3)+QFWSL(3)) * .5D0
      QFWUAN(6) = (QFWCU(4)+QFWSU(3)) * .5D0
      QFWLAN(6) = (QFWCL(4)+QFWSL(3)) * .5D0
      QFWUAN(7) = (QFWCU(4)+QFWSU(4)) * .5D0
      QFWLAN(7) = (QFWCL(4)+QFWSL(4)) * .5D0
      QFWUAN(8) = (QFWCU(1)+QFWSU(4)) * .5D0
      QFWLAN(8) = (QFWCL(1)+QFWSL(4)) * .5D0
C
C***	Step 4.
C
C***	For each of the upper layer segments use the area of the seg-
C***	ment and the previously calculated average heat transfer
C***	flux to calculate the eight contributions to theto the total
C***	rate of upper-layer wall heat transfer.  Sum these contribu-
C***	tions and obtain finally the average rate of heat transfer to
C***	the upper-layer portions of the walls.  Carry out analogous
C***	calculations for the lower wall surfaces.  Add rates of heat
C***	transfer to all 16 wall surface segments and obtain total
C***	rate of heat transfer to the wall.
C
      AWL(1) = XF * ZLAY
      AWU(1) = XF * (ZC-ZLAY)
      AWL(2) = (XW-XF) * ZLAY
      AWU(2) = (XW-XF) * (ZC-ZLAY)
      AWL(3) = YF * ZLAY
      AWU(3) = YF * (ZC-ZLAY)
      AWL(4) = (YW-YF) * ZLAY
      AWU(4) = (YW-YF) * (ZC-ZLAY)
      AWL(5) = AWL(2)
      AWU(5) = AWU(2)
      AWL(6) = AWL(1)
      AWU(6) = AWU(1)
      AWL(7) = AWL(4)
      AWU(7) = AWU(4)
      AWL(8) = AWL(3)
      AWU(8) = AWU(3)
      SUMAQL = XX0
      SUMAQU = XX0
      SUMAL = XX0
      SUMAU = XX0
      DO 60 N = 1, 8
        SUMAQL = AWL(N) * QFWLAN(N) + SUMAQL
        SUMAQU = AWU(N) * QFWUAN(N) + SUMAQU
        SUMAL = AWL(N) + SUMAL
        SUMAU = AWU(N) + SUMAU
   60 CONTINUE
C
C*** turn off heat transfer to lower wall surfaces
C
      SUMAQL = XX0
      IF (SUMAL.LE.XX0) THEN
        QFWLA = XX0
      ELSE
        QFWLA = SUMAQL / SUMAL
      END IF
      IF (SUMAU.LE.XX0) THEN
        QFWUA = XX0
      ELSE
        QFWUA = SUMAQU / SUMAU
      END IF
      
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE INT2D(XC,YC,XRECT,YRECT,R,ANS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INT2D
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  Integrates a function over a region formed by 
C         intersecting a rectangle with dimension (xrect,yrect) 
C         and a circle with center (xc,yc) and radius r.  
C
C     Arguments: XC
C                YC
C                XRECT
C                YRECT
C                R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      LOGICAL FIRST
      SAVE PI
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        ONE = 1.0D0
        PI = 4.0D0 * ATAN(ONE)
      END IF
C
      X1 = XRECT - XC
      X2 = XC
      Y1 = YRECT - YC
      Y2 = YC
C
      IF (R.LT.MIN(X1,X2,Y1,Y2)) THEN
        CALL INTTABL(R,FRINT)
        ANS = 2.0D0 * PI * FRINT
      ELSE
        CALL INTSQ(X1,Y1,R,ANS1)
        CALL INTSQ(X2,Y1,R,ANS2)
        CALL INTSQ(X1,Y2,R,ANS3)
        CALL INTSQ(X2,Y2,R,ANS4)
        ANS = ANS1 + ANS2 + ANS3 + ANS4
      END IF
      RETURN
      END
      SUBROUTINE INTSQ(S1,S2,R,ANS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INTSQ
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: S1
C                S2
C                R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      LOGICAL FIRST
      SAVE FIRST, PI
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        ONE = 1.0D0
        PI = 4.0D0 * ATAN(ONE)
      END IF
C
      IF (R.LE.MIN(S1,S2)) THEN
        CALL INTTABL(R,FRINT)
        ANS = PI * FRINT / 2.0D0
      ELSE
        CALL INTTRI(S1,S2,R,ANS1)
        CALL INTTRI(S2,S1,R,ANS2)
        ANS = ANS1 + ANS2
      END IF
      RETURN
      END
      SUBROUTINE INTTRI(X,Y,R,ANS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INTTRI
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: X
C                Y
C                R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C
      XX0 = 0.0D0
      IF (ABS(X).LT.1.D-5.OR.ABS(Y).LT.1.D-5) THEN
        ANS = XX0
        RETURN
      END IF
      THETA = ATAN(Y/X)
      IF (R.LT.X) THEN
        CALL INTTABL(R,FRINT)
        ANS = FRINT * THETA
        RETURN
      ELSE
        DIAG = SQRT(X**2+Y**2)
        IF (R.GT.DIAG) THEN
          YL = Y
        ELSE
          YL = SQRT(R**2-X**2)
        END IF
        THETAL = ATAN(YL/X)
        N = 1
        XXN = N
        DTH = THETAL / XXN
        ANS = XX0
        DO 10 J = 1, N
          XXJM1 = J - 1
          THETAJ = DTH / 2.0D0 + XXJM1 * DTH
          RJ = X / COS(THETAJ)
          CALL INTTABL(RJ,ARJ)
          ANS = ANS + ARJ
   10   CONTINUE
        ANS = ANS * DTH
        THETAU = THETA - THETAL
        CALL INTTABL(R,FRINTU)
        ANS = ANS + THETAU * FRINTU
      END IF
      RETURN
      END
      SUBROUTINE MAKTABL(R,N,FUNC)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     MAKTABL
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: R
C                N
C                FUNC
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"

      EXTERNAL FUNC
      DIMENSION TABL(100), FUN(100)
      COMMON /TRPTABL/ TABL, RMAX, NTAB
      SAVE /TRPTABL/
      
      XX0 = 0.0D0
      NTAB = N
      RMAX = R
      XXNTABM1 = NTAB - 1
      DR = RMAX / XXNTABM1
      DR2 = DR / 2.0D0
      TABL(1) = XX0
      FUN(1) = XX0
      DO 10 I = 2, NTAB
        XXIM1 = I - 1
        RR = XXIM1 * DR
        FUN(I) = RR * FUNC(RR)
        TABL(I) = TABL(I-1) + (FUN(I)+FUN(I-1)) * DR2
   10 CONTINUE
      RETURN
      END
      SUBROUTINE INTTABL(R,ANS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INTTABL
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      DIMENSION TABL(100)
      COMMON /TRPTABL/ TABL, RMAX, NTAB
      SAVE /TRPTABL/
      XXNTABM1 = NTAB - 1
      DR = RMAX / XXNTABM1 
      IR = 1.0D0 + R / DR
      IF (IR.LT.1) IR = 1
      IF (IR.GT.NTAB-1) IR = NTAB - 1
      TAB1 = TABL(IR)
      TAB2 = TABL(IR+1)
      XXIR = IR
      RR1 = (XXIR-1.0D0) * DR
      RR2 = XXIR * DR
      ANS = (TAB1*(RR2-R)+TAB2*(R-RR1)) / DR
      RETURN
      END
#ifdef pp_double
      DOUBLE PRECISION FUNCTION QFCLG(R)
#else
      FUNCTION QFCLG(R)
#endif
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     QFCLG
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description: This function computes the convective heat transfer 
C                  flux to the ceiling at location (X,Y)=(Z(1),Z(2)) 
C
C     Arguments: R
C
C     Revision History:
C        Created:  5/5/1995 at 14:18 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      COMMON /AINTCH/ H, HTCT, THT, THTQHP, C1, C2, C3, XF, YF, TC
      SAVE /AINTCH/
      RDH = R / H
      T0 = RDH ** (.8D0)
      T1 = 1.D0 - 1.1D0 * T0
      T2 = T0 ** 2
      T3 = T0 ** 3
      FF = (T1+0.808D0*T2) / (T1+2.2D0*T2+0.69D0*T3)
      IF (RDH.LT.0.2D0) THEN
        HTCLDH = C1 * (1.D0-C2*RDH)
        TADDIM = 10.22D0 - 14.9D0 * RDH
      ELSE
        HTCLDH = C3 * (RDH-0.0771D0) / ((RDH+0.279D0)*RDH**1.2D0)
        TADDIM = 8.390913361D0 * FF
      END IF
      HTCL = HTCLDH * HTCT
      TAD = TADDIM * THTQHP + THT
      QFCLG = HTCL * (TAD-TC)
      RETURN
      END
      SUBROUTINE SQFWST(RDH,H,C4,THT,HTCT,THTQHP,TW,QFWLOW,QFWUP,ZC,ZLAY
     +    )
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SQFWST
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  Calculate average heat transfer fluxes to lower and 
C            upper walls along a vertical line passing through a 
C            wall/ceiling-jet stagnation point
C
C
C     Arguments: RDH
C                H
C                C4
C                THT
C                HTCT
C                THTQHP
C                TW
C                QFWLOW
C                QFWUP
C                ZC
C                ZLAY 
C
C     Revision History:
C        Created:  5/5/1995 at 14:18 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      T1 = RDH ** (.8D0)
      T2 = T1 * T1
      T3 = T2 * T1
      F = (1.D0-1.1D0*T1+0.808D0*T2) / (1.D0-1.1D0*T1+2.2D0*T2+0.69D0*T3
     +    )
      IF (RDH.LT.0.2D0) THEN
        TADDIM = 10.22D0 - 14.9D0 * RDH
      ELSE
        TADDIM = 8.39D0 * F
      END IF
      HTCL = C4 * HTCT / RDH
      TAD = TADDIM * THTQHP + THT
      QFWST = HTCL * (TAD-TW)
      H8 = .80D0 * H
      H16 = H8 + H8
      IF (ZC.LE.H8) THEN
        QFWLOW = QFWST * (H16-(ZC-ZLAY)-ZC) / H16
        QFWUP = QFWST * (1.D0-(ZC-ZLAY)/H16)
      ELSE
        IF ((ZC-ZLAY).GT.H8) THEN
          QFWLOW = 0.0D0
          QFWUP = QFWST * (ZC-H8) / (2.0D0*(ZC-ZLAY))
        ELSE
          QFWLOW = QFWST * (H8-(ZC-ZLAY)) / (2.0D0*ZLAY)
          QFWUP = QFWST * (1.D0-(ZC-ZLAY)/H16)
        END IF
      END IF
      RETURN
      END
