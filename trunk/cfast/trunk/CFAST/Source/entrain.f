      SUBROUTINE ENTRAIN(DIRS12,YSLAB,XMSLAB,NSLAB,TU,TL,CP,YLAY,CONL,
     +    CONU,PMIX,MXPRD,NPROD,YVBOT,YVTOP,UFLW3,VSAS,VASA)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     ENTRAIN
C
C     Source File: ENTRAIN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: DIRS12
C    INPUT
C    -----
C   DIRS12 - A MEASURE OF THE DIRECTION OF THE ROOM 1 TO ROOM
C                       2 FLOW IN EACH SLAB
C   YSLAB  - SLAB HEIGHTS IN ROOMS 1,2 ABOVE DATUM ELEVATION [M]
C   XMSLAB - MAGNITUDE OF THE MASS FLOW RATE IN SLABS [KG/S]
C   NSLAB  - NUMBER OF SLABS BETWEEN BOTTOM AND TOP OF VENT
C   TU     - UPPER LAYER TEMPERATURE IN EACH ROOM [K]
C   TL     - LOWER LAYER TEMPERATURE IN EACH ROOM [K]
C   YLAY   - HEIGHT OF LAYER IN EACH ROOM ABOVE DATUM ELEVATION [M]
C
C   OUTPUT
C   ------
C   UFLW3(I,1,J), I=1 OR 2, J=1 OR 2 - MASS FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE TO ENTRAINMENT
C   UFLW3(I,2,J), I=1 OR 2, J=1 OR 2 - ENTHALPY FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I ENTRAINMENT
C   UFLW3(I,2+K,J), I=1 OR 2, K=1 TO NPROD, J=1 OR 2 - PRODUCT K FLOW
C            RATE TO UPPER (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE
C            ENTRAINMENT
C
C        Created:  
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "flwptrs.fi"
      INTEGER DIRS12(10)
      DIMENSION YSLAB(10), XMSLAB(10), PMIX(MXPRD)
      DIMENSION TU(2), TL(2), YLAY(2)
      DIMENSION CONL(MXPRD,2), CONU(MXPRD,2)
      DIMENSION UFLW3(2,MXPRD+2,2)
      DIMENSION VSAS(2), VASA(2)
      PARAMETER (XX0 = 0.0D0)
C
C*** INITIALIZE OUTPUTS
C
      DO 20 I = 1, 2
        DO 10 IPROD = 1, NPROD + 2
          UFLW3(I,IPROD,L) = XX0
          UFLW3(I,IPROD,U) = XX0
   10   CONTINUE
        VSAS(I) = XX0
        VASA(I) = XX0
   20 CONTINUE
C    
C
      DO 60 N = 1, NSLAB
C
C*** ELIMINATE CASES WHERE ENTRAINMENT DOES NOT OCCUR
C    I.E. A SLAB WHICH IS ADJACENT TO THE UPPER LAYER ON BOTH SIDES
C      OR A SLAB WHICH IS ADJACENT TO THE LOWER LAYER ON BOTH SIDES
C
        IF (YSLAB(N).LT.YLAY(1).OR.YSLAB(N).LT.YLAY(2)) THEN
          IF (YSLAB(N).GE.YLAY(1).OR.YSLAB(N).GE.YLAY(2)) THEN
C
C*** SLABS WITH NO FLOW CAUSE NO ENTRAINMENT
C
            IF (XMSLAB(N).NE.XX0) THEN
C
C*** DETERMINE WHAT ROOM FLOW IS COMING FROM
C
              IF (DIRS12(N).EQ.1) THEN
                IFROM = 1
                ITO = 2
              ELSE IF (DIRS12(N).EQ.0) THEN
C
C*** NO FLOW IN THIS SLAB SO WE CAN SKIP IT
C    (WE SHOULD NEVER GET HERE)
C
                GO TO 60
              ELSE IF (DIRS12(N).EQ.-1) THEN
                IFROM = 2
                ITO = 1
              END IF
C
C***  DETERMINE TEMPERATURE AND PRODUCT CONCENTRATIONS
C     OF ENTRAINED FLOW
C
              IF (YSLAB(N).LT.YLAY(ITO)) THEN
                TMIX = TL(ITO)
                DO 30 IPROD = 1, NPROD
                  PMIX(IPROD) = CONL(IPROD,ITO)
   30           CONTINUE
              ELSE
                TMIX = TU(ITO)
                DO 40 IPROD = 1, NPROD
                  PMIX(IPROD) = CONU(IPROD,ITO)
   40           CONTINUE
              END IF
C         
C*** COMPUTE THE SIZE OF THE ENTRAINED MASS FLOW
C
              IF (YSLAB(N).GE.YLAY(IFROM)) THEN
C
C*** INTO UPPER
C
                IF (TU(IFROM).GT.TL(ITO).AND.XMSLAB(N).NE.XX0) THEN
                  ZD = MAX(XX0,YLAY(ITO)-MAX(YVBOT,YLAY(IFROM)))
                  CALL ENTRFL(TU(IFROM),TL(ITO),XMSLAB(N),ZD,
     +                UFLW3(ITO,M,U))
                  UFLW3(ITO,M,L) = -UFLW3(ITO,M,U)
                  VSAS(ITO) = UFLW3(ITO,M,U)
                END IF
              ELSE
C
C*** INTO LOWER
C
                IF (TL(IFROM).LT.TU(ITO).AND.XMSLAB(N).NE.XX0) THEN
C               ZD = MAX(XX0,YLAY(IFROM)-MAX(YVBOT,YLAY(ITO)))

C*** need to re-work distance zd for both into upper and into
C         upper case.  the above doesn't work for all cases

                  ZD = MIN(YVTOP,YLAY(IFROM)) - MAX(YLAY(ITO),YVBOT)
                  CALL ENTRFL(TU(ITO),TL(IFROM),XMSLAB(N),ZD,
     +                UFLW3(ITO,M,L))
C*** The following factor (0.25 as of 10/1/93) now multiplies the lower layer
C*** entrainment to try to approximate the reduced Kelvin-Helmholz type mixing.
C*** This observation arises from the problems encountered in the test
C*** case temper1.dat.  This needs to be researched carefully!

                  UFLW3(ITO,M,L) = UFLW3(ITO,M,L) * 0.25D0
                  VASA(ITO) = UFLW3(ITO,M,L)
                  UFLW3(ITO,M,U) = -UFLW3(ITO,M,L)
                END IF
              END IF
C
C*** COMPUTE ENTHALPY AND PRODUCT FLOW RATES OF ENTRAINED FLOW
C    FROM THE MASS FLOW RATE
C
              UFLW3(ITO,Q,L) = CP * UFLW3(ITO,M,L) * TMIX
              UFLW3(ITO,Q,U) = CP * UFLW3(ITO,M,U) * TMIX
              DO 50 IPROD = 3, 2 + NPROD
                 UFLW3(ITO,IPROD,L) = UFLW3(ITO,M,L) * PMIX(IPROD-2)
                 UFLW3(ITO,IPROD,U) = UFLW3(ITO,M,U) * PMIX(IPROD-2)
   50         CONTINUE
            END IF
          END IF
        END IF
   60 CONTINUE
      RETURN
      END
