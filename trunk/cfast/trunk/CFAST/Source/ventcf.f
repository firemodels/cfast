      SUBROUTINE VENTCF(ITOP,IBOT,AVENT,NSHAPE,EPSP,XMVENT,TMVENT,ILAY)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     VENTCF
C
C     Source File: VENTCF.SOR
C
C     Functional Class:  
C
C     Description:  This routine calculates the flow of mass, enthalpy, 
C          and products of combustion through a horizontal vent joining 
C          an upper space 1 to a lower space 2. the subroutine uses input
C          data describing the two-layer environment of inside rooms and 
C          the uniform environment in outside spaces.
C
C     Arguments:
C     INPUT
C     -----
C     ITOP        - TOP ROOM NUMBER (physically with respect to the second compartment)
C     IBOT        - BOTTOM ROOM NUMBER
C     AVENT       - AREA OF THE VENT [M**2]                
C     NSHAPE      - NUMBER CHARACTERIZING VENT SHAPE: 1 = CIRCLE, 2 = SQUARE
C     EPSP        - ERROR TOLERANCE FOR DPREF [DIMENSIONLESS]
C
C     OUTPUT
C     ------
C     XMVENT(I)   I = 1, MASS FLOW FROM ROOM IBOT TO ROOM ITOP
C                 I = 2, MASS FLOW FROM ROOM ITOP TO ROOM IBOT
C
C     TMVENT(I)   I = 1, TEMPERATURE IN LAYER NEXT TO VENT IN TOP ROOM
C                 I = 2, TEMPERATURE IN LAYER NEXT TO VENT IN BOTTOM ROOM
C
C     ILAY(I)     I = 1, LAYER INDEX NEXT TO VENT IN TOP ROOM
C                 I = 2, LAYER INDEX NEXT TO VENT IN BOTTOM ROOM
C
C
C     Revision History:
C     May 15, 1991 the pressure was calculated incorrectly change pref->pofset
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
C
C
C     Procedure arguments:
C
      DIMENSION XMVENT(2), TMVENT(2), ILAY(2), PABS(2), DEN(2), 
     +    RELP(2)
      DIMENSION DENVNT(2), DP(2), VST(2), VVENT(2), IROOM(2)
C
      LOGICAL FIRSTC
      INTEGER L, U, Q, M
      PARAMETER (L = 2,U = 1,Q = 2,M = 1)
C
      DATA FIRSTC /.TRUE./
      SAVE FIRSTC, GAMCUT, GAMMAX, PI, XXTWO, XXONE, XXZERO
C
C     Initialization code - executed the first time ventcf is called.
C
      IF (FIRSTC) THEN
        XXZERO = 0.0D0
        XXONE = 1.0D0
        XXTWO = 2.0D0
        FIRSTC = .FALSE.
        GAMCUT = (XXTWO/(GAMMA+XXONE)) ** (GAMMA/(GAMMA-XXONE))
        ZZZ = GAMMA * ((XXTWO/(GAMMA+XXONE))**((GAMMA+XXONE)/(GAMMA-
     +      XXONE)))
        GAMMAX = SQRT(ZZZ)
        PI = 4.0D0 * ATAN(XXONE)
      END IF
C
C     calculate the PABS(I), DELP, the other properties
C     adjacent to the two sides of the vent, and DELDEN.
C
      DP(1) = XXZERO
      DP(2) = XXZERO
      IF (IBOT.LE.NM1) THEN
        DP(2) = -G * (ZZRHO(IBOT,L)*ZZHLAY(IBOT,L)+ZZRHO(IBOT,U)*
     +      ZZHLAY(IBOT,U))
        RELP(2) = ZZRELP(IBOT)
      ELSE
        RELP(2) = EPA(ITOP)
      END IF
C
      IF (ITOP.LE.NM1) THEN
        RELP(1) = ZZRELP(ITOP)
      ELSE
        DP(1) = -G * HRP(IBOT) * ERA(IBOT)
        RELP(1) = EPA(IBOT)
      END IF
      PABS(1) = RELP(1) + DP(1) + POFSET
      PABS(2) = RELP(2) + DP(2) + POFSET
C
C     DELP is pressure immediately below the vent less pressure
C     immediately above the vent.
C
      DELP = RELP(2) + DP(2) - (RELP(1)+DP(1))
C
C     ILAY(1) contains layer index in top room that is adjacent to vent
C     ILAY(2) contains layer index in bottom room that is adjacent to vent
C
      IF (ZZVOL(ITOP,L).LE.XXTWO*ZZVMIN(ITOP)) THEN
        ILAY(1) = U
      ELSE
        ILAY(1) = L
      END IF
      IF (ZZVOL(IBOT,U).LE.XXTWO*ZZVMIN(IBOT)) THEN
        ILAY(2) = L
      ELSE
        ILAY(2) = U
      END IF
C
C     DELDEN is density immediately above the vent less density
C     immediately below the vent
C
      IF (ITOP.LE.NM1) THEN
        DEN(1) = ZZRHO(ITOP,ILAY(1))
      ELSE
        DEN(1) = ERA(IBOT)
      END IF
      IF (IBOT.LE.NM1) THEN
        DEN(2) = ZZRHO(IBOT,ILAY(2))
      ELSE
        DEN(2) = ERA(ITOP)
      END IF
      DELDEN = DEN(1) - DEN(2)
C
C     calculate VST(I), the "standard" volume rate of flow
C     through the vent into space I
C
      IF (DELP.GE.XXZERO) THEN
        RHO = DEN(2)
        EPS = DELP / PABS(2)
      ELSE
        RHO = DEN(1)
        EPS = -DELP / PABS(1)
      END IF
      X = XXONE - EPS
      COEF = 0.68D0 + 0.17D0 * EPS
      EPSCUT = EPSP * MAX (XXONE, RELP(1), RELP(2))
      EPSCUT = SQRT(EPSCUT)
      SRDELP = SQRT(ABS(DELP))
      FNOISE = XXONE
      IF ((SRDELP/EPSCUT).LE.130.D0) FNOISE = XXONE - 
     +    EXP(-SRDELP/EPSCUT)
      IF (EPS.LE.0.1D-5) THEN
        W = XXONE - 0.75D0 * EPS / GAMMA
      ELSE
        IF (EPS.LT.GAMCUT) THEN
          GG = X ** (XXONE/GAMMA)
          FF = SQRT((XXTWO*GAMMA/(GAMMA-XXONE))*GG*GG*(XXONE-X/GG))
        ELSE
          FF = GAMMAX
        END IF
        W = FF / SQRT(EPS+EPS)
      END IF
      RHO2 = 2.0D0/RHO
      V = FNOISE * COEF * W * SQRT(RHO2) * AVENT * SRDELP
C
C     calculate VST for DELP > 0, DELP < 0 and DELP = 0
C
      IF (DELP.GT.XXZERO) THEN
        VST(1) = V
        VST(2) = XXZERO
      ELSE IF (DELP.LT.XXZERO) THEN
        VST(1) = XXZERO
        VST(2) = V
      ELSE
        VST(1) = XXZERO
        VST(2) = XXZERO
      END IF
C
C     calculate VEX, the exchange volume rate of flow through the vent
C
      IF (DELDEN.GT.XXZERO.AND.AVENT.NE.XXZERO) THEN
C
C     unstable configuration, calculate nonzero VEX
C
        IF (NSHAPE.EQ.1) THEN
          CSHAPE = 0.754D0
          D = XXTWO * SQRT(AVENT/PI)
        ELSE
          CSHAPE = 0.942D0
          D = SQRT(AVENT)
        END IF
        DELPFD = CSHAPE ** 2 * G * DELDEN * D ** 5 / (XXTWO*AVENT**2)
        DPDDPF = ABS(DELP/DELPFD)
        VEXMAX = 0.1D0 * SQRT(XXTWO*G*DELDEN*SQRT(AVENT**5)/(DEN(1)+
     +      DEN(2)))
        VEX = MAX(VEXMAX*(XXONE-DPDDPF),XXZERO)
      ELSE
C
C     Stable configuration, set VEX = 0
C
        VEX = XXZERO
      END IF
C
C     calculate VVENT(I), the volume rate of flow through the vent into space i
C
      DO 10 I = 1, 2
        VVENT(I) = VST(I) + VEX
   10 CONTINUE
C
C     calculate the vent flow properties
C
      DENVNT(1) = DEN(2)
      DENVNT(2) = DEN(1)
C
C     calculate the vent mass flow rates
C
      IROOM(1) = IBOT
      IROOM(2) = ITOP
      DO 20 I = 1, 2
        XMVENT(I) = DENVNT(I) * VVENT(I)
        IF (IROOM(I).LE.NM1) THEN
C
C        IROOM(I) is an inside room so use the environment variable
C        ZZTEMP for temperature 
C
          TMVENT(I) = ZZTEMP(IROOM(I),ILAY(3-I))
        ELSE
C
C        IROOM(I) is an outside room so use ETA(IROOM(3-I) for temperature
C
          TMVENT(I) = ETA(IROOM(3-I))
        END IF
   20 CONTINUE
      RETURN
      END
