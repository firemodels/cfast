      SUBROUTINE INITSPEC
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITSPEC
C
C     Source File: INITSPEC.SOR
C
C     Functional Class:  
C
C     Description:  This routine initializes variables associated with 
C         species it originally occured in CFAST and INITFS.  It was moved
C         to one subroutine to make maintenance easier
C
C     Arguments: 
C
C     Revision History:
C     July 26, 1990 modified mapping to eliminate referene to outside room
c     June 14, 1992 added initialization for species occuring in HVAC systems
c     4/24/95  removed reference to xx0 to eliminate flint complaint
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "params.fi"
      include "thermp.fi"
      include "cenviro.fi"
      include "wnodes.fi"
C
      DIMENSION XM(2)
C
C     WE MUST SET THE NITROGEN/OXYGEN FOR AMBIENT AIR
C
C     NOTE THAT THE FIRST CALL TO "TOXIC" IS WITH ZERO INTERVAL
C     THIS IS JUST TO SET THE PPM AND PPMDV VALUES FOR OUTPUT
C     IT SERVES NO REAL PURPOSE
C
      DO 50 I = 1, NM1
        XM(1) = RAMB(I) * ZZVOL(I,UPPER)
        XM(2) = RAMB(I) * ZZVOL(I,LOWER)
C
C     SET THE WATER CONTENT TO RELHUM - THE POLYNOMIAL FIT IS TO (T-273), AND
C     IS FOR SATURATION PRESSURE OF WATER.  THIS FIT COMES FROM THE STEAM
C     TABLES IN THE HANDBOOK OF PHYSICS AND CHEMISTRY.  WE ARE BEING CLEVER
C     HERE.  THE FINAL RESULT IN O2N2 SHOULD BE THE VALUE USED IN STPORT FOR
C     THE OUTSIDE AMBIENT.
C
        XT = TAMB(I)
        XTEMP = 23.2D0 - 3.816D3 / (XT-46.D0)
        XH2O = EXP(XTEMP) / 101325.0D0 * (18.D0/28.4D0)
        O2N2(8) = RELHUM * XH2O
C
C     NORMALIZE THE ATMOSPHERE
C
        TOTO2N2 = 0.0D0
        DO 10 J = 1, NS
          TOTO2N2 = TOTO2N2 + O2N2(J)
   10   CONTINUE
        DO 20 J = 1, NS
          O2N2(J) = O2N2(J) / TOTO2N2
   20   CONTINUE
C
        DO 40 K = UPPER, LOWER
          DO 30 LSP = 1, NS
            TOXICT(I,K,LSP) = 0.0D0
            MASS(K,I,LSP) = O2N2(LSP) * XM(K)
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE

      ISOF = NOFPRD
      DO 60 LSP = 1, NS
        IF (ACTIVS(LSP)) THEN
          DO 70 I = 1, NM1
            DO 80 K = UPPER, LOWER
              ISOF = ISOF + 1
              P(ISOF) = MASS(K,I,LSP) + MINMAS
   80       CONTINUE
   70     CONTINUE
        END IF
   60 CONTINUE

C     HVINIT DEFINE INITIAL PRODUCTS FOR HVAC SYSTEMS

      IF(NHVSYS.NE.0)THEN
         ISOF = NOFHVPR
         DO 220 LSP = 1, MIN(NS,9)
            IF(ACTIVS(LSP))THEN
               DO 230 ISYS = 1, NHVSYS
                  ISOF = ISOF + 1
                  P(ISOF) = O2N2(LSP)*HVTM(ISYS)
  230          CONTINUE
            ENDIF
  220    CONTINUE
      ENDIF
         
C     ADD IN HYDROGEN CHLORIDE DEPOSITION ONTO THE WALLS IF HCL IS TRACKED

      IF (ACTIVS(6)) THEN
        DO 90 I = 1, NM1
          DO 91 K = 1, NWAL
            ISOF = ISOF + 1
            P(ISOF) = MINMAS
   91     CONTINUE
   90   CONTINUE
      END IF

C     ADD SMOKE AGGLOMERATION IF SMOKE IS TRACKED

      IF (ACTIVS(9)) THEN
      END IF

c     the following line was commented out 8/27/92 because wall initialization
c     is now done by offset, nputt and initwall (wset)
c      CALL DATACOPY(P,ODEVARB)

C     CONNECT HVAC TO THE REST OF THE WORLD

      HVDELT = DELTAT

C     DEFINE PRODUCT MAP ARRAY

      IZPMAP(1) = 1
      IZPMAP(2) = 2
      IP = 2
      DO 100 IPROD = 1, NS
        IF (ACTIVS(IPROD)) THEN
          IP = IP + 1
          IZPMAP(IP) = IPROD + 2
        END IF
  100 CONTINUE
      RETURN
      END
