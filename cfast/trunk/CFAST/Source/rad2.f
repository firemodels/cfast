      SUBROUTINE RAD2(TWALL,TLAY,EMIS,ABSORB,IROOM,XROOM,YROOM,ZROOM,
     +    HLAY,QFIRE,XFIRE,YFIRE,ZFIRE,NFIRE,QFLUX,QLAY,MXFIRE,TAUFL,
     +    TAUFU,FIRANG,QOUT,BLACK,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RAD2
C
C     Source File: RAD2.SOR
C
C     Functional Class:  
C
C     Description:  This routine computes the radiative heat flux to 
C        the extended ceiling (ceiling + upper wall) and the extended 
C        floor (floor + lower wall) due to a point source fire, emitting 
C        absorbing gas layers (upper and lower) and heat emitting wall 
C        segments.  This routine also computes the heat absorbed by the 
C        lower and upper layers.
C
C     Arguments: TWALL
C*** INPUT
C
C   TWALL(I) - TWALL(I) IS THE TEMPERATURE OF THE I'TH SURFACE [K] . WHERE
C           I=1,2,3,4 DENOTES THE CEILING, THE UPPER WALL, THE LOWER WALL 
C           AND THE FLOOR RESPECTIVELY.  
C   TLAY - TLAY(I) IS THE TEMPERATURE OF THE I'TH LAYER [K] WHERE I=1,2
C          DENOTES THE UPPER, LOWER LAYERS RESPECTIVELY
C   EMIS - EMIS(I) IS THE EMISIVITY OF THE CEILING (I=1), WALLS (I=2)
C          AND FLOOR (I=3)
C   ABSORB - ABSORB(I) IS THE ABSORBIVITY [1/M] OF THE UPPER (I=1), LOWER LAYER 
C            (I=2)
C   XROOM - SIZE OF THE ROOM [M] IN THE X'TH COORDINATE DIRECTION.
C   YROOM - SIZE OF THE ROOM [M] IN THE Y'TH COORDINATE DIRECTION.
C   ZROOM - SIZE OF THE ROOM [M] IN THE Z'TH COORDINATE DIRECTION.
C   HLAY -  HEIGHT OF SMOKE LAYER INTERFACE ABOVE THE FLOOR [M]
C   QFIRE - ARRAY OF LENGTH NFIRE, QFIRE(IFIRE) IS THE ENERGY RELEASE RATE
C           DUE TO RADIATION OF THE IFIRE'TH FIRE [W]
C   XFIRE - X COORDINATE OF FIRE LOCATION [M]
C   YFIRE - Y COORDINATE OF FIRE LOCATION [M]
C   ZFIRE - Z COORDINATE OF FIRE LOCATION [M]
C
C*** OUTPUT
C
C    QFLUX - QFLUX(I) IS THE RADIANT HEAT FLUX [W/M**2] TO THE I'TH 
C            SURFACES WHERE I=1,2,3,4 DENOTES THE CEILING, THE UPPER WALL,
C            THE LOWER WALL AND THE FLOOR RESPECTIVELY.  NOTE THAT
C            QFLUX(1)=QFLUX(2) AND QFLUX(3)=QFLUX(4)
C    QLAY - QLAY(I) IS THE HEAT ABSORBED BY THE I'TH LAYER WHERE I=1,2
C           DENOTES THE UPPER, LOWER LAYERS RESPECTIVELY
C    QOUT   QOUT(I) is the output flux from the i'th wall
C    IERROR - Returns error codes
C
C     Revision History:
C        9/26/94 by GPF:
C        Added QOUT to the argument list of RAD4 and RAD2.
C        This variable is needed in order to perform
C        target flux/temperature calculations.
C        4/24/95 by gpf:
C        added check on dgefa's info flag
C        Modified: 9/5/1995 at 10:13 by PAR:
C                  Added support for IERROR and returning stops to main
C        2/5/96 by gpf:
C           removed reduced jacobian option added on 2/5/1993
C        Modified: 4/21/97 by gpf
C                  Added BLACK option for the gas to allow better simulations
C                  for furnaces
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"

      DIMENSION TLAY(2), TWALL(4), EMIS(4), ABSORB(2)
      DIMENSION XFIRE(*), YFIRE(*), ZFIRE(*)
      DIMENSION QLAY(2), QFLUX(4), QFIRE(*)
      DIMENSION TAUL(2,2), TAUU(2,2), BEAM(2,2)
      DIMENSION TAUFL(MXFIRE,*), TAUFU(MXFIRE,*)
      DIMENSION FIRANG(MXFIRE,*), AREA(2), AREA4(4)
      DIMENSION FIGS(2,2), EMIS2(2) 
      DIMENSION QOUT(4), QQOUT(2)
      DIMENSION XXL(2), XXU(2)
      LOGICAL BLACK

      DIMENSION A(2,2), B(2,2), E(2), C(2)
      DIMENSION IPVT(2), RHS(2), DQ(2), DQDE(2)

      LOGICAL FIRST
      INTEGER L, U
      PARAMETER (U = 1,L = 2)
      SAVE SIGMA,FIRST,X1,PI,THIRD,ONE

      DATA FIRST /.TRUE./

C
C*** DEFINE LOCAL CONSTANTS FIRST TIME RAD2 IS CALLED
C
      IF (FIRST) THEN
        SIGMA = 5.67D-8
        FIRST = .FALSE.
        X1 = 1.0D0
        PI = 4.0D0 * ATAN(X1)
        THIRD = 1.0D0 / 3.0D0
        ONE = 1.0D0
      END IF

C*** DEFINE AREAS OF UPPER AND LOWER PLATES

      AREA4(1) = XROOM * YROOM
      AREA4(2) = 2.0D0 * (ZROOM-HLAY) * (XROOM+YROOM)
      AREA4(3) = 2.0D0 * HLAY * (XROOM+YROOM)
      AREA4(4) = AREA4(1)
      AREA(1) = AREA4(1) + AREA4(2)
      AREA(2) = AREA4(3) + AREA4(4)
      AREAD = AREA4(1)

C*** DEFINE CONFIGURATION FACTORS 
      FIGS(1,1) = 1.0D0 - AREAD / AREA(1)
      FIGS(2,2) = 1.0D0 - AREAD / AREA(2)
      FIGS(1,2) = AREAD / AREA(1)
      FIGS(2,1) = AREAD / AREA(2)

C*** DEFINE TRANSMISSION FACTORS FOR SURFACES WITH RESPECT TO THEMSELVES

      BEAM(1,1) = (6.0D0*XROOM*YROOM*(ZROOM-HLAY)/PI) ** THIRD
      BEAM(2,2) = (6.0D0*XROOM*YROOM*HLAY/PI) ** THIRD
      BEAM(1,2) = ZROOM
      BEAM(2,1) = ZROOM
      FL = HLAY / ZROOM
      FU = 1.0D0 - FL
      IF(.NOT.BLACK)THEN
        TAUU(1,1) = EXP(-BEAM(1,1)*ABSORB(1))
        TAUL(2,2) = EXP(-BEAM(2,2)*ABSORB(2))
       ELSE
        TAUU(1,1) = 0.0D0
        TAUL(2,2) = 0.0D0
      ENDIF
      TAUU(2,2) = 1.0D0
      TAUL(1,1) = 1.0D0

      IF(.NOT.BLACK)THEN
        TAUU(1,2) = EXP(-FU*BEAM(1,2)*ABSORB(1))
        TAUL(1,2) = EXP(-FL*BEAM(1,2)*ABSORB(2))
       ELSE
        TAUU(1,2) = 0.0D0
        TAUL(1,2) = 0.0D0
      ENDIF
      TAUU(2,1) = TAUU(1,2)
      TAUL(2,1) = TAUL(1,2)

C*** DEFINE TRANMISSION FACTORS FOR SURFACES WITH RESPECT TO FIRE

      DO 20 IFIRE = 1, NFIRE
        IF (ZFIRE(IFIRE).GT.HLAY) THEN
          XXU(1) = ZROOM - ZFIRE(IFIRE)
          XXU(2) = ZFIRE(IFIRE) - HLAY
          XXL(1) = 0.0D0
          XXL(2) = HLAY
        ELSE
          XXU(1) = ZROOM - HLAY
          XXU(2) = 0.0D0
          XXL(1) = HLAY - ZFIRE(IFIRE)
          XXL(2) = ZFIRE(IFIRE)
        END IF
        DO 10 I = 1, 2
          IF(.NOT.BLACK)THEN 
            TAUFU(IFIRE,I) = EXP(-ABSORB(1)*XXU(I))
            TAUFL(IFIRE,I) = EXP(-ABSORB(2)*XXL(I))
           ELSE
            TAUFU(IFIRE,I) = 0.0D0
            TAUFL(IFIRE,I) = 0.0D0
          ENDIF
   10   CONTINUE
   20 CONTINUE

C*** COMPUTE SOLID ANGLES
      DO 30 IFIRE = 1, NFIRE
        XF = XFIRE(IFIRE)
        YF = YFIRE(IFIRE)
        ZF = ZFIRE(IFIRE)
        FIRANG(IFIRE,1) = RDSANG(-XF,XROOM-XF,-YF,YROOM-YF,HLAY-ZF)
        FIRANG(IFIRE,2) = 4.0d0*pi - FIRANG(IFIRE,1)
   30 CONTINUE
      F1D = RDPARFIG(XROOM,YROOM,ZROOM-HLAY)
      F2D = RDPARFIG(XROOM,YROOM,HLAY)

C*** DEFINE E VECTOR
      TUPPER4 = (TWALL(1)**4*F1D+TWALL(2)**4*(1.0D0-F1D))
      TLOWER4 = (TWALL(4)**4*F2D+TWALL(3)**4*(1.0D0-F2D))
      E(1) = SIGMA * TUPPER4
      E(2) = SIGMA * TLOWER4

C*** RE-MAP EMISSIVITY VECTOR

      EMIS2(1) = (EMIS(1)*AREA4(1) + EMIS(2)*AREA4(2))/AREA(1)
      EMIS2(2) = (EMIS(4)*AREA4(4) + EMIS(3)*AREA4(3))/AREA(2)

C*** DEFINE 'A' AND 'B' COEFFICICNT MATRIX
      DO 50 K = 1, 2
        DO 40 J = 1, 2
          AIJ = FIGS(K,J) * TAUL(K,J) * TAUU(K,J)
          A(K,J) = -AIJ * (1.0D0-EMIS2(J))
          B(K,J) = -AIJ
   40   CONTINUE
        A(K,K) = A(K,K) + 1.0D0
        B(K,K) = B(K,K) + 1.0D0
   50 CONTINUE

C*** DEFINE C VECTOR

      CALL RDFLUX(MXFIRE,2,1,AREA,HLAY,TLAY,ZFIRE,QFIRE,FIGS,TAUL,TAUU,
     +    TAUFL,TAUFU,FIRANG,NFIRE,QLLAY,QULAY,C)

C*** CONSTRUCT RIGHT HAND SIDE (RHS) OF LINEAR SYSTEM TO BE SOLVED

      RHS(1) = B(1,1) * E(1) + B(1,2) * E(2) - C(1)
      RHS(2) = B(2,1) * E(1) + B(2,2) * E(2) - C(2)
#ifdef pp_double
      CALL DGEFA(A,2,2,IPVT,INFO)
      CALL DGESL(A,2,2,IPVT,RHS,0)
#else
      CALL SGEFA(A,2,2,IPVT,INFO)
      CALL SGESL(A,2,2,IPVT,RHS,0)
#endif
      IF(INFO.NE.0) THEN
        CALL XERROR('RAD2 - Singular matrix',0,1,1)
        IERROR = 17
        RETURN
      END IF

C*** NOTE: EACH ROW K OF THE A MATRIX AS DEFINED BY SEIGAL AND HOWELL 
C    WAS DIVIDED BY EMIS2(K) (IN ORDER TO INSURE THAT THIS NEW 'A' WAS
C    DIAGONALLY DOMINANT.  NOW WE HAVE TO MULTIPLY THE SOLUTION TO THE
C    MODIFIED PROBLEM BY EMIS2(I) TO GET THE ORIGINAL ANSWERS

      DO 60 K = 1, 2
        DQDE(K) = RHS(K)
        QQOUT(K) = E(K) - (ONE - EMIS(K))*DQDE(K)
        DQ(K) = RHS(K) * EMIS2(K)
   60 CONTINUE

C*** TAKE SOLUTION AND COMPUTE ENERGY GAIN OR LOSS TO EACH PANEL
C    AND EACH LAYER.  ALSO COMPUTE FLUXES.  CHANGE SIGN SO THAT
C    A POSTIVE FLUX MEANS THAT HEAT IS FLOWING TO THE WALL

      QFLUX(1) = -DQ(1)
      QFLUX(2) = -DQ(1)
      QFLUX(3) = -DQ(2)
      QFLUX(4) = -DQ(2)

      QOUT(1) = QQOUT(1)
      QOUT(2) = QQOUT(1)
      QOUT(3) = QQOUT(2)
      QOUT(4) = QQOUT(2)

C*** COMPUTE RADIATION ABSORBED BY EACH LAYER

      CALL RDABS(2,1,E,DQDE,EMIS2,AREA,FIGS,TAUU,TAUL,QLLAY,QULAY)

      QLAY(U) = QULAY
      QLAY(L) = QLLAY

      RETURN
#ifdef pp_gui
      ENTRY INRAD2
      FIRST = .TRUE.
      RETURN
#endif
      END
