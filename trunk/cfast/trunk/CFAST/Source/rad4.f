      SUBROUTINE RAD4(TWALL,TLAY,EMIS,ABSORB,IROOM,XROOM,YROOM,ZROOM,
     +    HLAY,QFIRE,XFIRE,YFIRE,ZFIRE,NFIRE,QFLUX,QLAY,MXFIRE,TAUFL,
     +    TAUFU,FIRANG,QOUT,BLACK,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RAD4
C
C     Source File: RAD4.SOR
C
C     Functional Class:  
C
C     Description:  This routine computes the radiative heat flux 
C          to the ceiling, upper wall, lower wall and floor due to 
C          a point source fire, emitting absorbing gas layers (upper 
C          and lower) and heat emitting wall segments. This routine 
C          also computes the heat absorbed by the lower and upper layers.
C        Modified: 4/21/97 by gpf
C                  Added BLACK option for the gas to allow better simulations
C                  for furnaces
C
C     Arguments:
C*** INPUT
C
C   TWALL(I) - TWALL(I) IS THE TEMPERATURE OF THE I'TH SURFACE [K] . WHERE
C           I=1,2,3,4 DENOTES THE CEILING, THE UPPER WALL, THE LOWER WALL 
C           AND THE FLOOR RESPECTIVELY
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
C            THE LOWER WALL AND THE FLOOR RESPECTIVELY
C    QLAY -  QLAY(I) IS THE HEAT ABSORBED BY THE I'TH LAYER WHERE I=1,2
C            DENOTES THE UPPER, LOWER LAYERS RESPECTIVELY
C    QOUT   QOUT(I) is the output flux from the i'th wall
C    IERROR - Returns error codes
C
C     Revision History:
C     Modified: 9/26/94 by GPF:
C               Added QOUT to the argument list of RAD4 and RAD2.
C               This variable is needed in order to perform
C               target flux/temperature calculations.
C               4/24/95 by gpf:
C               added check on dgefa's info flag, removed zroom from call to
c               rdftran and rdrtran
C        Modified: 9/5/1995 at 10:13 by PAR:
C                  Added support for IERROR and returning stops to main
C        2/5/96 by gpf:
C           removed reduced jacobian option added on 2/5/1993
C        10/21/97 by gpf:
C           set fluxes to zero if radiation matrix is singular.
C           changes location of warning message so that it actually
C           prints before blowing up.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"

      DIMENSION TLAY(2), TWALL(4), EMIS(4), ABSORB(2)
      DIMENSION XFIRE(*), YFIRE(*), ZFIRE(*)
      DIMENSION QLAY(2), QFLUX(4), QFIRE(*)
      DIMENSION TAUL(4,4), TAUU(4,4), BEAM(4,4)
      DIMENSION TAUFL(MXFIRE,*), TAUFU(MXFIRE,*)
      DIMENSION FIRANG(MXFIRE,*), AREA(4)
      DIMENSION FIGS(4,4), QOUT(4)
      DIMENSION ZZ(4)
      LOGICAL BLACK

      DIMENSION A(4,4), B(4,4), E(4), C(4)
      DIMENSION IPVT(4), RHS(4), DQ(4), DQDE(4)

      INTEGER L, U
      PARAMETER (U = 1,L = 2,MXROOM = 100)
      DIMENSION IFLAG(MXROOM), F14(MXROOM)

      LOGICAL FIRST

      SAVE FIRST,SIGMA

      DATA FIRST /.TRUE./, IFLAG /MXROOM * 0/

C*** DEFINE LOCAL CONSTANTS FIRST TIME RAD4 IS CALLED

      IF (FIRST) THEN
        SIGMA = 5.67D-8
        FIRST = .FALSE.
      END IF
      IF (IFLAG(IROOM).EQ.0) THEN
        F14(IROOM) = RDPARFIG(XROOM,YROOM,ZROOM)
        IFLAG(IROOM) = 1
      END IF
      F14(IROOM) = RDPARFIG(XROOM,YROOM,ZROOM)

C*** DEFINE AREAS

      AREA(1) = XROOM * YROOM
      AREA(2) = 2.0D0 * (ZROOM-HLAY) * (XROOM+YROOM)
      AREA(3) = 2.0D0 * HLAY * (XROOM+YROOM)
      AREA(4) = AREA(1)

C*** DEFINE CONFIGURATION FACTORS

      F1D = RDPARFIG(XROOM,YROOM,ZROOM-HLAY)
      FF14 = F14(IROOM)
      F4D = RDPARFIG(XROOM,YROOM,HLAY)

      FIGS(1,1) = 0.0D0
      FIGS(1,2) = 1.0D0 - F1D
      FIGS(2,1) = AREA(1) * FIGS(1,2) / AREA(2)
      FIGS(2,2) = 1.0D0 - 2.0D0 * FIGS(2,1)
      FIGS(1,4) = FF14
      FIGS(4,1) = FIGS(1,4)

      FIGS(4,4) = 0.0D0
      FIGS(4,3) = 1.0D0 - F4D
      FIGS(3,4) = AREA(4) * FIGS(4,3) / AREA(3)
      FIGS(3,3) = 1.0D0 - 2.0D0 * FIGS(3,4)

      FIGS(1,3) = 1.0D0 - FIGS(1,4) - FIGS(1,2)
      FIGS(3,1) = AREA(1) * FIGS(1,3) / AREA(3)

      FIGS(3,2) = 1.0D0 - FIGS(3,4) - FIGS(3,3) - FIGS(3,1)
      FIGS(2,3) = AREA(3) * FIGS(3,2) / AREA(2)

      FIGS(2,4) = 1.0D0 - FIGS(2,3) - FIGS(2,2) - FIGS(2,1)
      FIGS(4,2) = AREA(2) * FIGS(2,4) / AREA(4)

C*** DEFINE TRANSMISSION FACTORS FOR SURFACES
C
C    BUT FIRST DEFINE BEAM LENGTHS

      ZZ(1) = ZROOM
      ZZ(2) = (HLAY+ZROOM) *.50D0
      ZZ(3) = HLAY * .50D0
      ZZ(4) = 0.0D0
      DX2 = (XROOM*.50D0) ** 2
      DY2 = (YROOM*.50D0) ** 2
      X2 = XROOM ** 2
      Y2 = YROOM ** 2

      BEAM(1,1) = 0.0D0

      DZ2 = (ZZ(1)-ZZ(2)) ** 2
      BEAM(1,2) = (SQRT(DZ2+DX2)+SQRT(DZ2+DY2))*.50D0

      DZ2 = (ZZ(1)-ZZ(3)) ** 2
      BEAM(1,3) = (SQRT(DZ2+DX2)+SQRT(DZ2+DY2))*.50D0

      BEAM(1,4) = ZROOM
      BEAM(2,2) = (XROOM+YROOM)*.50D0
      DZ2 = (ZROOM*.50D0) ** 2
      BEAM(2,3) = (SQRT(DZ2+X2)+SQRT(DZ2+Y2))*.50D0
      DZ2 = ((ZROOM+HLAY)*0.50D0) ** 2
      BEAM(2,4) = (SQRT(DZ2+DX2)+SQRT(DZ2+DY2))*.50D0
      BEAM(3,3) = BEAM(2,2)
      DH2 = (HLAY*.50D0) ** 2
      BEAM(3,4) = (SQRT(DH2+DX2)+SQRT(DH2+DY2))*.50D0
      BEAM(4,4) = 0.0D0
      DO 20 I = 1, 4
        DO 10 J = I + 1, 4
          BEAM(J,I) = BEAM(I,J)
   10   CONTINUE
   20 CONTINUE

      CALL RDRTRAN(4,2,ABSORB,BEAM,HLAY,ZZ,TAUU,TAUL,BLACK)

C*** DEFINE TRANSMISSION FACTORS FOR FIRES

      IF (NFIRE.NE.0) THEN
        CALL RDFTRAN(MXFIRE,4,2,ABSORB,HLAY,ZZ,NFIRE,ZFIRE,TAUFU,
     +      TAUFL,BLACK)
      END IF

C*** DEFINE SOLID ANGLES FOR FIRES

      IF (NFIRE.NE.0) THEN
        CALL RDFANG(MXFIRE,XROOM,YROOM,ZROOM,HLAY,NFIRE,XFIRE,YFIRE,
     +      ZFIRE,FIRANG)
      END IF

C*** NOTE: WE WANT TO SOLVE THE LINEAR SYSTEM
C          A*DQ = B*E + C
C   WHERE A AND B ARE NXN MATRICES, Q, E AND C ARE N VECTORS
C
C*** DEFINE E VECTOR

      DO 30 I = 1, 4
        E(I) = SIGMA * TWALL(I) ** 4
   30 CONTINUE

C*** DEFINE 'A' AND 'B' COEFFICICNT MATRIX
      DO 50 K = 1, 4
        DO 40 J = 1, 4
          AIJ = FIGS(K,J) * TAUL(K,J) * TAUU(K,J)
          A(K,J) = -AIJ * (1.0D0-EMIS(J))
          B(K,J) = -AIJ
   40   CONTINUE
        A(K,K) = A(K,K) + 1.0D0
        B(K,K) = B(K,K) + 1.0D0
   50 CONTINUE
      

C*** DEFINE C VECTOR
C    ALSO, CALCULATE ENERGY ABSORBED BY UPPER, LOWER LAYER GASES
C          DUE TO FIRES AND GAS LAYER EMISSION

      CALL RDFLUX(MXFIRE,4,2,AREA,HLAY,TLAY,ZFIRE,QFIRE,FIGS,TAUL,TAUU,
     +    TAUFL,TAUFU,FIRANG,NFIRE,QLLAY,QULAY,C)

C*** CONSTRUCT RIGHT HAND SIDE (RHS) OF LINEAR SYSTEM TO BE SOLVED
C    I.E. COMPUTE B*E - C
      DO 60 K = 1, 4
        RHS(K) = DDOT(4,B(K,1),4,E(1),1) - C(K)
   60 CONTINUE

C*** SOLVE THE LINEAR SYSTEM

      CALL DGEFA(A,4,4,IPVT,INFO)
      IF(INFO.NE.0) THEN
        CALL XERROR('RAD4 - Singular matrix',0,1,1)
        IERROR = 18
        DO 65 K = 1, 4
          RHS(K) = 0.0D0
   65   CONTINUE
       ELSE
        CALL DGESL(A,4,4,IPVT,RHS,0)
      ENDIF

C*** NOTE: EACH ROW K OF THE A MATRIX, AS DEFINED BY SEIGAL AND HOWELL 
C    WAS DIVIDED BY EMIS(K) (IN ORDER TO INSURE THAT THIS NEW 'A' WAS
C    DIAGONALLY DOMINANT.  NOW WE HAVE TO MULTIPLY THE SOLUTION TO THE
C    MODIFIED PROBLEM BY EMIS(I) TO GET THE ANSWER TO THE ORIGINAL
C    PROBLEM

      DO 70 K = 1, 4
        DQDE(K) = RHS(K)
        QOUT(K) = E(K) - (1.0d0 - EMIS(K))*DQDE(K)
        DQ(K) = RHS(K) * EMIS(K)
   70 CONTINUE

C*** TAKE SOLUTION AND COMPUTE ENERGY GAIN OR LOSS TO EACH PANEL
C    AND EACH LAYER.  ALSO COMPUTE FLUXES.  CHANGE SIGN SO THAT
C    A POSTIVE FLUX MEANS THAT HEAT IS FLOWING TO THE WALL

      DO 80 I = 1, 4
        QFLUX(I) = -DQ(I)
   80 CONTINUE

C*** COMPUTE RADIATION ABSORBED BY EACH LAYER

      CALL RDABS(4,2,E,DQDE,EMIS,AREA,FIGS,TAUU,TAUL,QLLAY,QULAY)

      QLAY(U) = QULAY
      QLAY(L) = QLLAY

      RETURN
      END
