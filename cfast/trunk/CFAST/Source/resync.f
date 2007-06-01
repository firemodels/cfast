      SUBROUTINE RESYNC(PDIF,IBEG)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RESYNC
C
C     Source File: RESYNC.SOR
C
C     Functional Class:   
C                        
C
C     Description:  Routine to resyncronize the total mass of the
C                   species with that of the total mass
C
C     Arguments: PDIF   The P array to resync
C                IBEG   The point at which species are started in P array
C
C     Revision History:
C        Created:  12/1/1992 at 12:39 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      DIMENSION PDIF(*)
      DIMENSION FACTOR(NR,2)
C
      XX0 = 0.0D0
      DO 10 IROOM = 1,NM1
         FACTOR(IROOM,UPPER) = XX0
         FACTOR(IROOM,LOWER) = XX0
   10 CONTINUE

      ISOF = IBEG
      DO 20 IPROD = 1, MIN(NS,9)
         IF (ACTIVS(IPROD)) THEN
            DO 30 IROOM = 1, NM1
               FACTOR(IROOM,UPPER) = FACTOR(IROOM,UPPER) + PDIF(ISOF)
               ISOF = ISOF + 1
               FACTOR(IROOM,LOWER) = FACTOR(IROOM,LOWER) + PDIF(ISOF)
               ISOF = ISOF + 1
   30       CONTINUE
         ENDIF
   20 CONTINUE
      DO 60 IROOM = 1, NM1
         IF (FACTOR(IROOM,UPPER).GT.XX0.AND.
     .                     ZZMASS(IROOM,UPPER).GT.XX0) THEN
           FACTOR(IROOM,UPPER) = ZZMASS(IROOM,UPPER)/FACTOR(IROOM,UPPER)
         ELSE
            FACTOR(IROOM,UPPER) = 1.0D0
         END IF
         IF (FACTOR(IROOM,LOWER).GT.XX0.AND.
     .                     ZZMASS(IROOM,LOWER).GT.XX0) THEN
           FACTOR(IROOM,LOWER) = ZZMASS(IROOM,LOWER)/FACTOR(IROOM,LOWER)
         ELSE
            FACTOR(IROOM,LOWER) = 1.0D0
         END IF
   60 CONTINUE

      ISOF = IBEG
      DO 50 IPROD = 1, MIN(NS,9)
        IF (ACTIVS(IPROD)) THEN
          DO 40 IROOM = 1, NM1
            PDIF(ISOF) = PDIF(ISOF) * FACTOR(IROOM,UPPER)
            ISOF = ISOF + 1
            PDIF(ISOF) = PDIF(ISOF) * FACTOR(IROOM,LOWER)
            ISOF = ISOF + 1
   40     CONTINUE
        END IF
   50 CONTINUE

      RETURN
      END
