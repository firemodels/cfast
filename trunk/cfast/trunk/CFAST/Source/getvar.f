      SUBROUTINE GETVAR(IVENT,IROOM,IROOM2,NPROD,YFLOR,YLAY,PFLOR,
     +                  DENL,DENU,CONL,CONU,TL,TU)
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "vents.fi"
C*BEG
C
C***  GETVAR  SPECIFIC - ROUTINE TO INTERFACE BETWEEN CCFM.VENTS GLOBAL
C             DATA STRUCTURES AND VENT (BOTH NATURAL AND FORCED) DATA
C             STRUCTURES.
C
C***  SUBROUTINE ARGUMENTS
C
C  INPUT
C  -----
C  IVENT - VENT NUMBER
C  IROOM - ROOM NUMBER
C
C  OUTPUT
C  ------
C  YFLOR   HEIGHT OF FLOOR ABOVE DATUM ELEVATION [M]
C  YLAY    HEIGHT OF LAYER ABOVE DATUM ELEVATION [M]
C  PFLOR   PRESSURE AT FLOOR RELATIVE TO AMBIENT [P]
C  DENL    DENSITY OF LOWER LAYER [KG/M**3]
C  DENU    DENSITY OF UPPER LAYER [KG/M**3]
C  CONL    CONCENTRATION OF LOWER LAYER FOR EACH PRODUCT
C          [UNIT OF PRODUCT/KG OF LAYER]
C  CONU    CONCENTRATION OF UPPER LAYER FOR EACH PRODUCT
C          [UNIT OF PRODUCT/KG OF LAYER]
C  TL      TEMPERATURE OF LOWER LAYER [K]
C  TU      TEMPERATURE OF UPPER LAYER [K]
C
C     Revision History:
C        Created:  
C        Modified: 07/22/1996 by GPF:
C                  Added logic to use lower layer if hall flow has not
C                  reached vent yet.
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C*END
      DIMENSION CONL(MXPRD), CONU(MXPRD)
      LOGICAL HALLFLAG
      INTEGER UP

      XX0 = 0.0D0
      HALLFLAG = .FALSE.

C*** for rooms that are halls only use upper layer properties
C    if the ceiling jet is beyond the vent

      UP = UPPER
C
      IF (IROOM.LT.N) THEN
        YFLOR = ZZYFLOR(IROOM)
        PFLOR = ZZRELP(IROOM)
        YLAY = ZZHLAY(IROOM,LOWER)

C*** this is a hall, the vent number is defined and flow is occuring

        IF(IZHALL(IROOM,IHROOM).EQ.1.AND.IVENT.NE.0.AND.
     .                             IZHALL(IROOM,IHMODE).EQ.IHDURING)THEN
          VENTDIST = ZZVENTDIST(IROOM,IVENT)
          IF(VENTDIST.GT.XX0)THEN
            TIME0 = ZZHALL(IROOM,IHTIME0)
            VEL = ZZHALL(IROOM,IHVEL)
            CJETDIST = VEL*(STIME-TIME0)
            IF(CJETDIST.LT.VENTDIST)THEN
              UP = LOWER
             ELSE
              UP = UPPER
              HALLFLAG = .TRUE.
            ENDIF
           ELSE
            UP = LOWER
          ENDIF
        ENDIF

        DENU = ZZRHO(IROOM,UP)
        DENL = ZZRHO(IROOM,LOWER)
        DO 10 IPROD = 1, NPROD
          IP = IZPMAP(IPROD+2) - 2
          CONL(IPROD) = ZZCSPEC(IROOM,LOWER,IP)
          CONU(IPROD) = ZZCSPEC(IROOM,UP,IP)
   10   CONTINUE
        TU = ZZTEMP(IROOM,UP)
        TL = ZZTEMP(IROOM,LOWER)
        ZLOC = HR(IROOM) - ZZHALL(IROOM,IHDEPTH)/2.0D0
        IF(HALLFLAG)THEN
          CALL HALLTRV(IROOM,CJETDIST,ZLOC,TU,RHOU,HALLVEL)
        ENDIF
      ELSE
        YFLOR = ZZYFLOR(IROOM2)
        PFLOR = EPA(IROOM2)
        YLAY = ZZHLAY(IROOM,LOWER)
        DENU = ERA(IROOM2)
        DENL = ERA(IROOM2)
        DO 20 IPROD = 1, NPROD
          IP = IZPMAP(IPROD+2) - 2
          CONL(IPROD) = ZZCSPEC(IROOM,LOWER,IP)
          CONU(IPROD) = ZZCSPEC(IROOM,UP,IP)
   20   CONTINUE
        TU = ETA(IROOM2)
        TL = ETA(IROOM2)
      END IF
      RETURN
      END
