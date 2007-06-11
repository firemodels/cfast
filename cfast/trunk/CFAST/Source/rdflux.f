      SUBROUTINE RDFLUX(MXFIRE,NZONE,NUP,AREA,HLAY,TLAY,ZFIRE,QFIRE,
     +    FIGS,TAUL,TAUU,TAUFL,TAUFU,FIRANG,NFIRE,QLLAY,QULAY,C)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDFLUX
C
C     Source File: RDFLUX.SOR
C
C     Functional Class:  
C
C     Description:  This routine calculates the 'c' vector in the
C        net radiation equations of seigel and howell and the 
C        heat absorbed by the lower and upper layer fires due
C        to gas layer emission and fires.  
C
C     Arguments: MXFIRE
C                NZONE
C                NUP
C                AREA
C                HLAY
C                TLAY
C                ZFIRE
C                QFIRE
C                FIGS
C                TAUL
C                TAUU
C                TAUFL
C                TAUFU
C                FIRANG
C                NFIRE
C                QLLAY
C                QULAY
C                C
C
C     Revision History:
C        Created:  5/5/1995 at 15:16 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C    
      INTEGER L, U
      PARAMETER (U = 1,L = 2)
      DIMENSION C(*)
      DIMENSION FIGS(NZONE,*), TAUL(NZONE,*), TAUU(NZONE,*)
      DIMENSION TAUFL(MXFIRE,*), TAUFU(MXFIRE,*)
      DIMENSION FIRANG(MXFIRE,*), ZFIRE(*)
      DIMENSION AREA(*), QFIRE(MXFIRE), TLAY(2)
      LOGICAL FIRST
      SAVE FIRST, PI, SIGMA
      DATA FIRST /.TRUE./
      XX1 = 1.0D0
      IF (FIRST) THEN
        FIRST = .FALSE.
        PI = 4.0D0 * ATAN(XX1)
        SIGMA = 5.67D-8
      END IF
C
C*** DEFINE C VECTOR
C
      QULAY = 0.D0
      QLLAY = 0.D0
      EU = SIGMA * TLAY(U) ** 4
      EL = SIGMA * TLAY(L) ** 4
      DO 40 K = 1, NUP
        C(K) = 0.D0
C
C*** CASE: UPPER TO UPPER
C
        DO 10 J = 1, NUP
          QUGAS = (XX1-TAUU(K,J)) * EU
          C(K) = C(K) + FIGS(K,J) * QUGAS
          QULAY = QULAY - AREA(K) * FIGS(K,J) * QUGAS
   10   CONTINUE
C
C*** CASE: LOWER TO UPPER
C
        DO 20 J = NUP + 1, NZONE
          QUGAS = (XX1-TAUU(K,J)) * EU
          QLGAS = (XX1-TAUL(K,J)) * EL
          C(K) = C(K) + FIGS(K,J) * (QUGAS+QLGAS*TAUU(K,J))
          WF = AREA(K) * FIGS(K,J)
          QULAY = QULAY + QLGAS * WF * (XX1-TAUU(K,J)) - QUGAS * WF
          QLLAY = QLLAY - QLGAS * WF
   20   CONTINUE
C
C*** CASE: FIRE TO UPPER LAYER
C
        DO 30 IFIRE = 1, NFIRE
          QFFLUX = 0.25D0*QFIRE(IFIRE) * FIRANG(IFIRE,K) / (PI*AREA(K))
          C(K) = C(K) + QFFLUX * TAUFL(IFIRE,K) * TAUFU(IFIRE,K)
          IF (ZFIRE(IFIRE).GT.HLAY) THEN
            FACTU = XX1 - TAUFU(IFIRE,K)
            FACTL = 0.0D0
          ELSE
            FACTU = (XX1-TAUFU(IFIRE,K)) * TAUFL(IFIRE,K)
            FACTL = XX1 - TAUFL(IFIRE,K)
          END IF
          QULAY = QULAY + FACTU * QFFLUX * AREA(K)
          QLLAY = QLLAY + FACTL * QFFLUX * AREA(K)
   30   CONTINUE
   40 CONTINUE
C
      DO 80 K = NUP + 1, NZONE
        C(K) = 0.0D0
C
C*** CASE: UPPER TO LOWER
C
        DO 50 J = 1, NUP
          QUGAS = (XX1-TAUU(K,J)) * EU
          QLGAS = (XX1-TAUL(K,J)) * EL
          C(K) = C(K) + FIGS(K,J) * (QUGAS*TAUL(K,J)+QLGAS)
          WF = AREA(K) * FIGS(K,J)
          QULAY = QULAY - QUGAS * WF
          QLLAY = QLLAY + QUGAS * WF * (XX1-TAUL(K,J)) - QLGAS * WF
   50   CONTINUE
C
C*** CASE: LOWER TO LOWER
C
        DO 60 J = NUP + 1, NZONE
          QLGAS = (1.0D0-TAUL(K,J)) * EL
          C(K) = C(K) + FIGS(K,J) * QLGAS
          QLLAY = QLLAY - QLGAS * AREA(K) * FIGS(K,J)
   60   CONTINUE
C
C*** CASE: FIRE TO LOWER LAYER
C
        DO 70 IFIRE = 1, NFIRE
          QFFLUX = 0.25D0*QFIRE(IFIRE) * FIRANG(IFIRE,K) / (PI*AREA(K))
          C(K) = C(K) + QFFLUX * TAUFL(IFIRE,K) * TAUFU(IFIRE,K)
          IF (ZFIRE(IFIRE).GT.HLAY) THEN
            FACTU = XX1 - TAUFU(IFIRE,K)
            FACTL = (XX1-TAUFL(IFIRE,K)) * TAUFU(IFIRE,K)
          ELSE
            FACTU = 0.0D0
            FACTL = XX1 - TAUFL(IFIRE,K)
          END IF
          QULAY = QULAY + FACTU * QFFLUX * AREA(K)
          QLLAY = QLLAY + FACTL * QFFLUX * AREA(K)
   70   CONTINUE
   80 CONTINUE
      RETURN
      END
