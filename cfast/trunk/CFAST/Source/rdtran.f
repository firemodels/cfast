      SUBROUTINE RDFTRAN(MXFIRE,NZONE,NUP,ABSORB,HLAY,ZZ,NFIRE,
     +    ZFIRE,TAUFU,TAUFL,BLACK)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDFTRAN
C
C     Source File: RDTRAN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: MXFIRE
C                NZONE
C                NUP
C                ABSORB
C                HLAY
C                ZZ
C                NFIRE
C                ZFIRE
C                TAUFU
C                TAUFL
C
C     Revision History:
C
C        Modified: 4/21/97 by gpf
C                  Added BLACK option for the gas to allow better simulations
C                  for furnaces
C        Modified: 10/10/97 by gpf
C                  fixed subscript error in TAU's
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C     
      DIMENSION ABSORB(*), ZZ(*), ZFIRE(*)
      DIMENSION TAUFU(MXFIRE,*), TAUFL(MXFIRE,*)
      LOGICAL BLACK
      DO 30 I = 1, NFIRE
        DO 10 J = 1, NUP
          IF (ZFIRE(I).GT.HLAY) THEN
            BEAM = ABS(ZZ(J)-ZFIRE(I))
            TAUFL(I,J) = 1.0D0
            IF(.NOT.BLACK)THEN
              TAUFU(I,J) = EXP(-ABSORB(1)*BEAM)
             ELSE
              TAUFU(I,J) = 0.0D0
              TAUFL(I,J) = 0.0D0
            ENDIF
          ELSE
            BEAMU = ZZ(J) - HLAY
            BEAML = HLAY - ZFIRE(I)
            IF(.NOT.BLACK)THEN
              TAUFU(I,J) = EXP(-ABSORB(1)*BEAMU)
              TAUFL(I,J) = EXP(-ABSORB(2)*BEAML)
             ELSE
              TAUFU(I,J) = 0.0D0
              TAUFL(I,J) = 0.0D0
            ENDIF
          END IF
   10   CONTINUE
        DO 20 J = NUP + 1, NZONE
          IF (ZFIRE(I).LE.HLAY) THEN
            BEAM = ABS(ZZ(J)-ZFIRE(I))
            TAUFU(I,J) = 1.0D0
            IF(.NOT.BLACK)THEN
              TAUFL(I,J) = EXP(-ABSORB(2)*BEAM)
             ELSE
              TAUFL(I,J) = 0.0D0
              TAUFU(I,J) = 0.0D0
            ENDIF
          ELSE
            BEAMU = ZFIRE(I) - HLAY
            BEAML = HLAY - ZZ(J)
            IF(.NOT.BLACK)THEN
              TAUFU(I,J) = EXP(-ABSORB(1)*BEAMU)
              TAUFL(I,J) = EXP(-ABSORB(2)*BEAML)
             ELSE
              TAUFU(I,J) = 0.0D0
              TAUFL(I,J) = 0.0D0
            ENDIF
          END IF
   20   CONTINUE
   30 CONTINUE
      RETURN
      END
C
      SUBROUTINE RDRTRAN(NZONE,NUP,ABSORB,BEAM,HLAY,ZZ,TAUU,TAUL,BLACK)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDRTRAN
C
C     Source File: RDTRAN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: NZONE
C                NUP
C                ABSORB
C                BEAM
C                HLAY
C                ZZ
C                TAUU
C                TAUL
C
C     Revision History:
C        Created:  5/5/1995 at 15:16 by GPF
C        Modified: 4/21/97 by gpf
C                  Added BLACK option for the gas to allow better simulations
C                  for furnaces
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      DIMENSION ABSORB(*), BEAM(NZONE,NZONE), ZZ(*)
      DIMENSION TAUU(NZONE,NZONE), TAUL(NZONE,NZONE)
      LOGICAL BLACK
C
C*** DEFINE UPPER LAYER TRANSMISSION FACTORS
C
C ** UPPER TO UPPER
C
      DO 20 I = 1, NUP
        DO 10 J = I + 1, NUP
          IF(.NOT.BLACK)THEN
            TAUU(I,J) = EXP(-ABSORB(1)*BEAM(I,J))
           ELSE
            TAUU(I,J) = 0.0D0
          ENDIF
          TAUU(J,I) = TAUU(I,J)
   10   CONTINUE
        IF(.NOT.BLACK)THEN
          TAUU(I,I) = EXP(-ABSORB(1)*BEAM(I,I))
         ELSE
          TAUU(I,I) = 0.0D0
        ENDIF
   20 CONTINUE
C
C*** UPPER TO LOWER AND LOWER TO UPPER
C
      DO 40 I = 1, NUP
        DO 30 J = NUP + 1, NZONE
          FU = (ZZ(I)-HLAY) / (ZZ(I)-ZZ(J))
          IF(.NOT.BLACK)THEN
            TAUU(I,J) = EXP(-ABSORB(1)*BEAM(I,J)*FU)
           ELSE
            TAUU(I,J) = 0.0D0
          ENDIF
          TAUU(J,I) = TAUU(I,J)
   30   CONTINUE
   40 CONTINUE
C
C*** LOWER TO LOWER
C
      DO 60 I = NUP + 1, NZONE
        DO 50 J = NUP + 1, NZONE
          IF(.NOT.BLACK)THEN
            TAUU(I,J) = 1.0D0
           ELSE
            TAUU(I,J) = 0.0D0
          ENDIF
   50   CONTINUE
   60 CONTINUE
C
C*** DEFINE LOWER LAYER TRANSMISSION FACTORS
C
C ** LOWER TO LOWER
C
      DO 80 I = NUP + 1, NZONE
        DO 70 J = I + 1, NZONE
          IF(.NOT.BLACK)THEN
            TAUL(I,J) = EXP(-ABSORB(2)*BEAM(I,J))
           ELSE
            TAUL(I,J) = 0.0D0
          ENDIF
          TAUL(J,I) = TAUL(I,J)
   70   CONTINUE
        IF(.NOT.BLACK)THEN
          TAUL(I,I) = EXP(-ABSORB(2)*BEAM(I,I))
         ELSE
          TAUL(I,I) = 0.0D0
        ENDIF
   80 CONTINUE
C
C*** UPPER TO UPPER
C
      DO 100 I = 1, NUP
        DO 90 J = 1, NUP
          IF(.NOT.BLACK)THEN
            TAUL(I,J) = 1.0D0
           ELSE
            TAUL(I,J) = 0.0D0
          ENDIF
   90   CONTINUE
  100 CONTINUE
C
C*** UPPER TO LOEWR AND LOWER TO UPPER
C
      DO 120 I = NUP + 1, NZONE
        DO 110 J = 1, NUP
          FL = (HLAY-ZZ(I)) / (ZZ(J)-ZZ(I))
          IF(.NOT.BLACK)THEN
            TAUL(I,J) = EXP(-ABSORB(2)*BEAM(I,J)*FL)
           ELSE
            TAUL(I,J) = 0.0D0
          ENDIF
          TAUL(J,I) = TAUL(I,J)
  110   CONTINUE
  120 CONTINUE
      RETURN
      END
