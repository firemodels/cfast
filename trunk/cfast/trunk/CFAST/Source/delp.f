      SUBROUTINE DELP(Y,NELEV,YFLOR,YLAY,DENL,DENU,PFLOR,EPSP,DP)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DELP
C
C     Source File: DELP.SOR
C
C     Functional Class:  
C
C     Description:  calculation of the absolute hydrostatic
C           pressures at a specified elevation in each of two adjacent
C           rooms and the pressure difference.  the basic calculation
C           involves a determination and differencing of hydrostatic
C           pressures above a specified datum pressure.
C
C     Arguments: 
C   INPUT
C   -----
C   Y     - VECTOR OF HEIGHTS ABOVE DATUM ELEVATION WHERE
C           PRESSURE DIFFERENCE IS TO BE CALCULATED [M]
C   NELEV - NUMBER OF HEIGHTS TO BE CALCULATED
C   YFLOR - HEIGHT OF FLOOR IN EACH ROOM ABOVE DATUM ELEVATION
C           [M]
C   YLAY  - HEIGHT OF LAYER IN EACH ROOM ABOVE DATUM ELEVATION [M]
C   DENL  - LOWER LAYER DENSITY IN EACH ROOM [KG/M**3]
C   DENU  - UPPER LAYER DENSITY IN EACH ROOM [KG/M**3]
C   PFLOR - PRESSURE AT BASE OF EACH ROOM ABOVE DATUM PRESSURE
C            [KG/(M*S**2) = PASCAL]
C
C   OUTPUT
C   ------
C   DP    - CHANGE IN PRESSURE BETWEEN TWO ROOMS [KG/(M*S**2) = PASCAL]
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      DIMENSION PROOM(2), Y(*)
      DIMENSION DENL(*), DENU(*), YFLOR(*), YLAY(*), PFLOR(*), DP(*)
      DIMENSION GDENL(2), GDENU(2), YGDEN(2)
      PARAMETER (G = 9.80D0)
      PARAMETER (X1 = 1.0D0)

      DO 10 IROOM = 1, 2
        YGDEN(IROOM) = -(YLAY(IROOM)-YFLOR(IROOM)) * DENL(IROOM) * G
        GDENL(IROOM) = -DENL(IROOM) * G
        GDENU(IROOM) = -DENU(IROOM) * G
   10 CONTINUE

C
      DO 30 I = 1, NELEV
        DO 20 IROOM = 1, 2
          IF (YFLOR(IROOM).LE.Y(I).AND.Y(I).LE.YLAY(IROOM)) THEN
C
C*** THE HEIGHT, Y, IS IN THE LOWER LAYER
C
            PROOM(IROOM) = (Y(I)-YFLOR(IROOM)) * GDENL(IROOM)
C
          ELSE IF (Y(I).GT.YLAY(IROOM)) THEN
C
C*** THE HEIGHT, Y, IS IN THE UPPER LAYER
C
            PROOM(IROOM) = YGDEN(IROOM) + GDENU(IROOM) * (Y(I)-
     +          YLAY(IROOM))
          ELSE
            PROOM(IROOM) = 0.0D0
          END IF
   20   CONTINUE
C
C*** CHANGE IN PRESSURE IS DIFFERENCE IN PRESSURES IN TWO ROOMS
C
        DP1 = PFLOR(1) + PROOM(1)
        DP2 = PFLOR(2) + PROOM(2)
C
C*** TEST OF DELP FUDGE
C
        EPSCUT = 10.0D0 * EPSP * MAX(X1,ABS(DP1),ABS(DP2))
C
C
        DPOLD = DP1 - DP2
C
C*** TEST FOR UNDERFLOW
C
        IF (ABS(DPOLD/EPSCUT).LE.130.0D0) THEN
            ZZ = 1.D0 - EXP(-ABS(DPOLD/EPSCUT))
            DP(I) = ZZ * DPOLD
          ELSE
            DP(I) = DPOLD
          END IF
   30   CONTINUE
        RETURN
        END
C
C
