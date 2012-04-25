      SUBROUTINE RDHEAT(FLWRAD,FLXRAD,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDHEAT
C
C     Source File: RDHEAT.SOR
C
C     Functional Class:  
C
C     Description:  Interface between DSUORC and RAD2 or RAD4.  Loops over
C                   rooms setting up varibles to pass.  If one or more fires
C                   are in a room calls RAD4 otherwise RAD2.
C
C     Arguments: FLWRAD      Net enthalphy into each layer
C                FLXRAD      Net enthalphy flux into surface
C                IERROR      Returns error codes
C
C     Revision History:
C        Created:  11/??/1991 at 10:20 by PAR
C        Modified by GPF 2/5/93
C                  Added partial derivative calculations for use with
C                  the reduced Jacobian option
C        Modified: 2/7/93 by GPF:
C                  The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have 
C                  the number of fires as the first subscript.
C        Modified: 9/26/94 by GPF:
C                  Added QOUT to the argument list of RAD4 and RAD2.
C                  This variable is needed in order to perform
C                  target flux/temperature calculations.
C        Modified: 7/13/95 by GPF:
C                  Reduced the number of radiation calculations performed
C                  during a Jacobian calculation
C        Modified: 9/5/1995 at 10:22 by PAR:
C                  Added support for IERROR and returning stops to main
C        Modified: 04/07/95 by JBH
C                  Deleted constant absorbances and replaced them with calls to
C                  ABSORB function to calculate gas and soot absorbance on a room-
C                  by-room basis.
C        Modified: 2/5/96 by gpf
C                  removed reduced jacobian option added on 2/5/1993
C        Modified: 9/12/96 by gpf
C                  Call ABSORB for all rooms if OPTION(FRAD)==1,
C                  Call ABSORB for only fire rooms if OPTION(FRAD)==2
C                  (recall that options are set in SOLVER.INI). 
C                  Note the code behaves the same as before.  It only
C                  behaves differently if you override OPTION(FRAD)
C                  and specify 2 in the SOLVER.INI file.
C        Modified: 4/21/97 by gpf
C                  Added BLACK option for the gas to allow better simulations
C                  for furnaces
C        Modified: 10/16/97 by gpf
C                  Use 0.5 and 0.01 for upper and lower layer absorbtivities
C                  if OPTION(FRAD) is set to 4 . (ie use the original
C                  values for absorbtivities)
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C*RB
C
C     Routine:    CFRAD
C     Function:   Interface between DSOURC and RAD2 or RAD4.  Loops over
C                 rooms setting up varibles to pass.  If one or more fires
C                 are in a room calls RAD4 otherwise RAD2.
C     Output:     FLWRAD      Net enthalphy into each layer
C                 FLXRAD      Net enthalphy flux into surface
C     Revision History:
C     PAR   11/??/91 Created.
C     Modified by GPF 2/5/93
C               Added partial derivative calculations for use with
C               the reduced Jacobian option
C     Modified: 2/7/93 by GPF:
C               The radiation routines expect to receive info for each
C               fire in a room.  Therefore, XFIRE a 2-d array must have 
C               the number of fires as the first subscript.
C     Modified: 9/26/94 by GPF:
C               Added QOUT to the argument list of RAD4 and RAD2.
C               This variable is needed in order to perform
C               target flux/temperature calculations.
C     Modified: 7/13/95 by GPF:
C               Reduced the number of radiation calculations performed
C               during a Jacobian calculation
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "wdervs.fi"
      include "opt.fi"
      include "fltarget.fi"

      DIMENSION FLWRAD(NR,2), FLXRAD(NR,NWAL), QLAY(2), QFLXW(NWAL)

      DIMENSION TWALL(NWAL), MAP(NWAL), EMIS(NWAL)
      DIMENSION TG(2)
      LOGICAL BLACK

C     WORK AND DUMMY ARRAYS PASSED TO RAD2 AND RAD4

      DIMENSION TAUFL(MXFIRE,NWAL), TAUFU(MXFIRE,NWAL), 
     +    FIRANG(MXFIRE,NWAL)
      DIMENSION DUMMY(4)
      DIMENSION FLXRAD0(NR,NWAL), FLWRAD0(NR,2)
      LOGICAL ROOMFLG(NR)
      SAVE FLXRAD0, FLWRAD0

      DATA MAP /1, 4, 2, 3/
      DO 20 I = 1, NM1
        DO 10 J = 1, NWAL
          FLXRAD(I,J) = 0.0D0
   10   CONTINUE
        FLWRAD(I,1) = 0.0D0
        FLWRAD(I,2) = 0.0D0
   20 CONTINUE

      IF (OPTION(FRAD)==OFF) RETURN
      BLACK = .FALSE.
      IF(OPTION(FRAD)==3)BLACK = .TRUE.

C*** Initially assume that we compute radiation transfer in every room

      DO 15 I = 1, NM1
        ROOMFLG(I) = .TRUE.
   15 CONTINUE

      IF(OPTION(FMODJAC)==ON)THEN
        IF(JACCOL>0)THEN

C*** If 2nd modified jacobian is active and dassl is computing a jacobian then
C    only compute the radiation heat transfer in the room where the dassl 
C    solution variable has been perturbed

          DO 16 I = 1, NM1
            ROOMFLG(I) = .FALSE.
   16     CONTINUE
          IEQTYP = IZEQMAP(JACCOL,1)
          IROOM = IZEQMAP(JACCOL,2)
          IF(IEQTYP==EQVU.OR.IEQTYP==EQTU.OR.IEQTYP==EQTL.OR.
     .       IEQTYP==EQWT)THEN
             IF(IEQTYP==EQWT)IROOM = IZWALL(IROOM,1)
             ROOMFLG(IROOM) = .TRUE.
          ENDIF
        ENDIF
      ENDIF

      DO 35 I = 1, NM1
        ZZBEAM(LOWER,I) = (1.8D0 * ZZVOL(I, LOWER)) / 
     +   (AR(I) + ZZHLAY(I, LOWER) * (DR(I) + BR(I)))
        ZZBEAM(UPPER,I) = (1.8 * ZZVOL(I, UPPER)) / 
     +   (AR(I) + ZZHLAY(I, UPPER) * (DR(I) + BR(I)))
   35 CONTINUE

      DEFABSUP = 0.5D0
      DEFABSLOW = 0.01D0
      IF(LFBO/=0.AND.OPTION(FRAD)/=4.AND.LFBT/=1)THEN
         DEFABSUP = ABSORB(LFBO,UPPER)
      ENDIF

      DO 40 I = 1, NM1
        IF(.NOT.ROOMFLG(I))GO TO 40
        TG(UPPER) = ZZTEMP(I,UPPER)
        TG(LOWER) = ZZTEMP(I,LOWER)
        ZZBEAM(LOWER,I) = (1.8D0 * ZZVOL(I, LOWER)) / 
     +   (AR(I) + ZZHLAY(I, LOWER) * (DR(I) + BR(I)))
        ZZBEAM(UPPER,I) = (1.8 * ZZVOL(I, UPPER)) / 
     +   (AR(I) + ZZHLAY(I, UPPER) * (DR(I) + BR(I)))
        DO 50 IWALL = 1, 4
          IF(MOD(IWALL,2)==1)THEN
            ILAY = UPPER
           ELSE
            ILAY = LOWER
          ENDIF
          IMAP = MAP(IWALL)
          IF (SWITCH(IWALL,I)) THEN
            TWALL(IMAP) = ZZWTEMP(I,IWALL,1)
            EMIS(IMAP) = EPW(IWALL,I)
           ELSE
            TWALL(IMAP) = ZZTEMP(I,ILAY)
            EMIS(IMAP) = 1.0D0
          endif
   50   CONTINUE
        IFIRE = IFRPNT(I,2)
        NRMFIRE = IFRPNT(I,1)
        IF (NRMFIRE/=0) THEN
          IF(.NOT.BLACK)THEN
            IF(OPTION(FRAD)==4.OR.LFBT==1)THEN
              ZZABSB(UPPER,I) = DEFABSUP
              ZZABSB(LOWER,I) = DEFABSLOW
             ELSE
              ZZABSB(UPPER,I) = ABSORB(I, UPPER)
              ZZABSB(LOWER,I) = ABSORB(I, LOWER)
            ENDIF
          ENDIF
          CALL RAD4(TWALL,TG,EMIS,ZZABSB(1,I),I,bR(I),dR(I),HR(I),
     +        ZZHLAY(I,LOWER),XFIRE(IFIRE,8),
     +        XFIRE(IFIRE,1),XFIRE(IFIRE,2),XFIRE(IFIRE,3),NRMFIRE,
     +        QFLXW,QLAY,MXFIRE,TAUFL,TAUFU,FIRANG,
     +        RDQOUT(1,I),BLACK,IERROR)
         ELSE
          IF(.NOT.BLACK)THEN
            IF(OPTION(FRAD)==2.OR.OPTION(FRAD)==4.OR.
     .         LFBT==1)THEN
              ZZABSB(UPPER,I) = DEFABSUP
              ZZABSB(LOWER,I) = DEFABSLOW
             ELSE
              ZZABSB(UPPER,I) = ABSORB(I, UPPER)
              ZZABSB(LOWER,I) = ABSORB(I, LOWER)
            ENDIF
          ENDIF
          CALL RAD2(TWALL,TG,EMIS,ZZABSB(1,I),I,bR(I),dR(I),HR(I),
     +        ZZHLAY(I,LOWER),XFIRE(IFIRE,8),
     +        XFIRE(IFIRE,1),XFIRE(IFIRE,2),XFIRE(IFIRE,3),NRMFIRE,
     +        QFLXW,QLAY,MXFIRE,TAUFL,TAUFU,FIRANG,
     +        RDQOUT(1,I),BLACK,IERROR)

        ENDIF
        IF (IERROR/=0) RETURN
        DO 30 J = 1, NWAL
          FLXRAD(I,J) = QFLXW(MAP(J))
   30   CONTINUE

        FLWRAD(I,1) = QLAY(1)
        FLWRAD(I,2) = QLAY(2)
        QR(1,I) = QLAY(1)
        QR(2,I) = QLAY(2)
   40 CONTINUE

      IF(OPTION(FMODJAC)==ON)THEN
        IF(JACCOL==0)THEN

C*** if the jacobian option is active and dassl is computing the base vector for
C    the jacobian calculation then save the flow and flux calculation for later use

          DO 55 IROOM = 1, NM1
            DO 60 IWALL = 1, NWAL
              FLXRAD0(IROOM,IWALL) = FLXRAD(IROOM,IWALL)
   60       CONTINUE
            FLWRAD0(IROOM,1) = FLWRAD(IROOM,1)
            FLWRAD0(IROOM,2) = FLWRAD(IROOM,2)
   55     CONTINUE
         ELSEIF(JACCOL>0)THEN

C***  dassl is computing the JACCOL'th column of a jacobian.  copy values into
C     the flow and flux vectors that have not changed from the base vector

          DO 70 IROOM = 1, NM1
            IF(ROOMFLG(IROOM))GO TO 70
            DO 80 IWALL = 1, NWAL
              FLXRAD(IROOM,IWALL) = FLXRAD0(IROOM,IWALL)
   80       CONTINUE
            FLWRAD(IROOM,1) = FLWRAD0(IROOM,1)
            FLWRAD(IROOM,2) = FLWRAD0(IROOM,2)
   70     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END

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
      endif

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
        IF (ZFIRE(IFIRE)>HLAY) THEN
          XXU(1) = ZROOM - ZFIRE(IFIRE)
          XXU(2) = ZFIRE(IFIRE) - HLAY
          XXL(1) = 0.0D0
          XXL(2) = HLAY
        ELSE
          XXU(1) = ZROOM - HLAY
          XXU(2) = 0.0D0
          XXL(1) = HLAY - ZFIRE(IFIRE)
          XXL(2) = ZFIRE(IFIRE)
        endif
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
      CALL DGEFA(A,2,2,IPVT,INFO)
      CALL DGESL(A,2,2,IPVT,RHS,0)
      IF(INFO/=0) THEN
        CALL XERROR('RAD2 - Singular matrix',0,1,1)
        IERROR = 17
        RETURN
      endif

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
      END

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
      endif
      IF (IFLAG(IROOM)==0) THEN
        F14(IROOM) = RDPARFIG(XROOM,YROOM,ZROOM)
        IFLAG(IROOM) = 1
      endif
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

      IF (NFIRE/=0) THEN
        CALL RDFTRAN(MXFIRE,4,2,ABSORB,HLAY,ZZ,NFIRE,ZFIRE,TAUFU,
     +      TAUFL,BLACK)
      endif

C*** DEFINE SOLID ANGLES FOR FIRES

      IF (NFIRE/=0) THEN
        CALL RDFANG(MXFIRE,XROOM,YROOM,ZROOM,HLAY,NFIRE,XFIRE,YFIRE,
     +      ZFIRE,FIRANG)
      endif

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
      IF(INFO/=0) THEN
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
      endif
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
          IF (ZFIRE(IFIRE)>HLAY) THEN
            FACTU = XX1 - TAUFU(IFIRE,K)
            FACTL = 0.0D0
          ELSE
            FACTU = (XX1-TAUFU(IFIRE,K)) * TAUFL(IFIRE,K)
            FACTL = XX1 - TAUFL(IFIRE,K)
          endif
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
          IF (ZFIRE(IFIRE)>HLAY) THEN
            FACTU = XX1 - TAUFU(IFIRE,K)
            FACTL = (XX1-TAUFL(IFIRE,K)) * TAUFU(IFIRE,K)
          ELSE
            FACTU = 0.0D0
            FACTL = XX1 - TAUFL(IFIRE,K)
          endif
          QULAY = QULAY + FACTU * QFFLUX * AREA(K)
          QLLAY = QLLAY + FACTL * QFFLUX * AREA(K)
   70   CONTINUE
   80 CONTINUE
      RETURN
      END

      SUBROUTINE RDABS(
     I                  NZONE,NUP,E,DQDE,EMIS2,AREA,
     I                  FIGS,TAUU,TAUL,
     O                  QLLAY,QULAY
     .                 )
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDABS
C
C     Source File: RDABS.SOR
C
C     Functional Class:  
C
C     Description:  This routine computes the energy absorbed by the upper and
C           lower layer due to radiation given off by heat emiiting rectangles
C           forming the enclosure.  Coming into this routine, qllay and qulay
C           were previously defined to be the heat absorbed by the lower and
C           upper layers due to gas emissions and fires.  this routine just
C           adds onto these values.
C
C     Arguments: NZONE
C                NUP
C                E
C                DQDE
C                EMIS2
C                AREA
C                FIGS
C                TAUU
C                TAUL
C                QLLAY
C                QULAY 
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C
C
      DIMENSION E(*), EMIS2(*), AREA(*),DQDE(*)
      DIMENSION FIGS(NZONE,*), TAUU(NZONE,*),TAUL(NZONE,*)
C
      DO 300 K = 1, NUP
         QOUT = E(K) - DQDE(K)*(1.0D0-EMIS2(K))
         QK = QOUT*AREA(K)
         DO 310 J = 1,NUP
            QULAY = QULAY + QK*FIGS(K,J)*(1.0D0-TAUU(K,J))
  310    CONTINUE
         DO 320 J = NUP + 1, NZONE
            QULAY = QULAY + QK*FIGS(K,J)*(1.0D0-TAUU(K,J))
            QLLAY = QLLAY + QK*FIGS(K,J)*TAUU(K,J)*(1.0D0-TAUL(K,J))
  320    CONTINUE
  300 CONTINUE
C
      DO 330 K = NUP+1,NZONE
         QOUT = E(K) - DQDE(K)*(1.0D0-EMIS2(K))
         QK = QOUT*AREA(K)
         DO 340 J = 1,NUP
            QULAY = QULAY + QK*FIGS(K,J)*TAUL(K,J)*(1.0D0-TAUU(K,J))
            QLLAY = QLLAY + QK*FIGS(K,J)*(1.0D0-TAUL(K,J))
  340    CONTINUE
         DO 350 J = NUP+1,NZONE
            QLLAY = QLLAY + QK*FIGS(K,J)*(1.0D0-TAUL(K,J))
  350    CONTINUE
  330 CONTINUE
C
      RETURN
      END

      real*8 FUNCTION RDPARFIG(X,Y,Z)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDPARFIG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  This routine calculates the configuration factor 
C          between two paralell plates a distance z a part.  Each 
C          plate has a dimension of x by y.  the units of x, y and z 
C          are un-important except that they must be consistent.
C
C     Arguments: X
C                Y
C                Z
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"

      SAVE IFIRST, PI, XX0, XX1, XXH
      DATA IFIRST /0/
      IF (IFIRST==0) THEN
        XX1 = 1.0D0
        XXH = 0.5D0
        XX0 = 0.0D0
        PI = 4.0D0 * ATAN(XX1)
        IFIRST = 1
      endif
      RDPARFIG = XX0
      IF (Z==XX0.OR.X==XX0.OR.Y==XX0) RETURN
      XX = X / Z
      YY = Y / Z
      F1 = XXH*LOG((XX1+XX**2)*(XX1+YY**2)/(XX1+XX**2+YY**2))
      YSQ = SQRT(XX1+YY**2)
      F2 = XX * YSQ * ATAN(XX/YSQ)
      XSQ = SQRT(XX1+XX**2)
      F3 = YY * XSQ * ATAN(YY/XSQ)
      F4 = XX * ATAN(XX)
      F5 = YY * ATAN(YY)
      RDPARFIG = 2.0D0 * (F1+F2+F3-F4-F5) / (PI*XX*YY)
      RETURN
      END
      real*8 FUNCTION RDPRPFIG(X,Y,Z)
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDPRPFIG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  this routine calculates the configuration
C          factor between two perpindular plates with a common edge.
C
C     Arguments: X
C                Y
C                Z
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"
C    
      LOGICAL FIRST
      SAVE FIRST, PI
      DATA FIRST /.TRUE./
      XX1 = 1.0D0
      XX0 = 0.0D0
      IF (FIRST) THEN
        PI = 4.0D0 * ATAN(XX1)
        FIRST = .FALSE.
      endif
      RDPRPFIG = XX0
      IF (Y==XX0.OR.X==XX0.OR.Z==XX0) RETURN
      H = X / Y
      W = Z / Y
      F1 = W * ATAN(XX1/W)
      F2 = H * ATAN(XX1/H)
C      
      HWSUM = H ** 2 + W ** 2
      HWNORM = SQRT(HWSUM)
      RHWNORM = 1.0D0/HWNORM
      F3 = HWNORM * ATAN(RHWNORM)
 
      WSUM1 = XX1 + W ** 2
      HSUM1 = XX1 + H ** 2
      HWSUM2 = XX1 + HWSUM
      F4A = WSUM1 * HSUM1 / HWSUM2
      F4B = (W**2*HWSUM2/WSUM1/HWSUM)
      F4C = (H**2*HWSUM2/HSUM1/HWSUM)
      F4 = 0.25D0*(LOG(F4A)+LOG(F4B)*W**2+LOG(F4C)*H**2) 
      RDPRPFIG = (F1+F2-F3+F4) / PI / W
      RETURN
      END

      SUBROUTINE RDFANG(MXFIRE,XROOM,YROOM,ZROOM,HLAY,NFIRE,XFIRE,YFIRE,
     +    ZFIRE,FIRANG)
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDFANG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: MXFIRE
C                XROOM
C                YROOM
C                ZROOM
C                HLAY
C                NFIRE
C                XFIRE
C                YFIRE
C                ZFIRE
C                FIRANG
C
C     Revision History:
C                Modified by gpf 6/28/95:
C                   Changed constant in solid angle identity from 1 to 4 pi 
C                   since the underlying solid angle calculation is not 
C                   normalized to one any more.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"
 
      DIMENSION XFIRE(*), YFIRE(*), ZFIRE(*)
      DIMENSION FIRANG(MXFIRE,*)
      LOGICAL FIRST
      SAVE FIRST, FOURPI
      DATA FIRST/.TRUE./

      IF(FIRST)THEN
         FIRST = .FALSE.
         XX1 = 1.0D0
         PI = 4.0D0*ATAN(XX1)
         FOURPI = 4.0D0*PI
      ENDIF

      DO 10 I = 1, NFIRE
        ARG1 = -XFIRE(I)
        ARG2 = XROOM - XFIRE(I)
        ARG3 = -YFIRE(I)
        ARG4 = YROOM - YFIRE(I)
        F1 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZROOM-ZFIRE(I))
        FD = RDSANG(ARG1,ARG2,ARG3,ARG4,HLAY-ZFIRE(I))
        F4 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZFIRE(I))
        FIRANG(I,1) = F1
        FIRANG(I,4) = F4
        IF (ZFIRE(I)<HLAY) THEN
          FIRANG(I,2) = FD - F1
          FIRANG(I,3) = FOURPI - FD - F4
        ELSE
          FIRANG(I,2) = FOURPI - FD - F1
          FIRANG(I,3) = FD - F4
        endif
   10 CONTINUE
      RETURN
      END
      real*8 FUNCTION RDSANG(X1,X2,Y1,Y2,R)
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDSANG
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: X1
C                X2
C                Y1
C                Y2
C                R
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"

      F1 = SIGN(RDSANG1(ABS(X2),ABS(Y2),R),X2*Y2)
      F2 = SIGN(RDSANG1(ABS(X1),ABS(Y2),R),X1*Y2)
      F3 = SIGN(RDSANG1(ABS(X2),ABS(Y1),R),X2*Y1)
      F4 = SIGN(RDSANG1(ABS(X1),ABS(Y1),R),X1*Y1)
      RDSANG = F1 - F2 - F3 + F4
      RETURN
      END
      real*8 FUNCTION RDSANG1(X,Y,R)
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RDSANG1
C
C     Source File: RDFIGSOL.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: X
C                Y
C                R
C
C     Revision History:
C           gpf 5/24/95  Eliminated a division by 4*pi.  This division was done
C                        elsewhere resulting in a double division by 4*pi.
C                        Now this routine computes a solid angle (maximum
C                        esult 4*pi) rather than a configuration factor
C                        (maximum result 1).
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      include "precis.fi"
C
      LOGICAL FIRST
      SAVE FIRST, PI, PIO2
C     
      DATA FIRST /.TRUE./
C     
      XX0 = 0.0D0
      XX1 = 1.0D0
      IF (FIRST) THEN
        FIRST = .FALSE.
        PI = 4.0D0 * ATAN(XX1)
        PIO2 = PI / 2.0D0
      endif
      IF (X<=XX0.OR.Y<=XX0) THEN
        RDSANG1 = XX0
      ELSE
        XR = X * X + R * R
        XYR = X * X + Y * Y + R * R
        XY = X * X + Y * Y
        YR = Y * Y + R * R
        F1 = MIN(XX1, Y * SQRT(XYR/XY/YR))
        F2 = MIN(XX1, X * SQRT(XYR/XY/XR))
        RDSANG1 = (ASIN(F1)+ASIN(F2)-PIO2)
      endif
      RETURN
      END

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
          IF (ZFIRE(I)>HLAY) THEN
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
          endif
   10   CONTINUE
        DO 20 J = NUP + 1, NZONE
          IF (ZFIRE(I)<=HLAY) THEN
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
          endif
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

      real*8 FUNCTION ABSORB (CMPT, LAYER)
C
C  FUNCTION CALCULATES ABSORBANCE, DUE TO GASES (CO2 AND H2O) AND SOOT,
C  FOR THE SPECIFIED COMPARTMENT AND LAYER.
C
C  ABSORBANCES ARE ASSUMED TO BE EQUAL TO EMISSIVITIES. PER SPFE 
C  HANDBOOK (1988 ED., PAGES 1-99 - 1-101), GAS ABSORBANCE IS
C  CALCULATED AS
C
C  AG = CH2O * EH2O + CCO2 * ECO2 - DELTAE ~ EH2O + 0.5 * ECO2;
C
C  WHERE CH2O AND CCO2 ARE CONCENTRATIONS AND DELTAE IS A CORRECTION
C  FOR OVERLAP OF THE ABSORBANCE BANDS.
C
C  ECO2 AND EH2O ARE INTERPOLATED FROM SPFE HANDBOOK GRAPHS WHICH SHOW 
C  E = F(T,PL), WHERE T IS THE GAS TEMPERATURE (KELVINS) AND PL IS THE
C  PARTIAL PRESSURE-PATH LENGTH PRODUCT (ATM-M). TEMPERATURE AND GAS 
C  PARTIAL PRESSURES ARE BASED ON DATA CALCULATED ELSEWHERE AND STORED IN
C  COMMON BLOCKS. USING HANDBOOK FORMULAE, PATH LENGTH IS ESTIMATED AS
C
C  L = C * 4 * V/A; WHERE C ~ 0.9 FOR TYPICAL GEOMETRIES, V IS THE GAS 
C      VOLUME AND A IS THE SURFACE AREA OF THE GAS VOLUME.
C
C  TOTAL ABSORBANCE IS CALCULATED AS
C
C  AT = AS + AG * TRANS = (1 - EXP(-AS)) + AG * EXP(-AS);
C
C  WHERE AS IS SOOT ABSORPION, AG IS GAS ABSORPTION, TRANS IS SOOT
C  TRANSMISSION, A IS THE EFFECTIVE ABSORBANCE COEFFICIENT FOR SOOT AND
C  S IS THE PHYSICAL PATHLENGTH. S IS APPRXOMINATED BY L, THE MEAN BEAM
C  LENGTH, AND A ~ K*VFS*TG, WHERE VFS IS THE SOOT VOLUME FRACTION, TG THE
C  GAS TEMPERATURE AND K IS A CONSTANT. FOR TYPICAL FUELS, K ~ 1195.5.
C
C  VERSION 1.0.3
C
C  REVISION HISTORY:
C    CREATED 12/09/94 - JBH
C    MODIFIED 08/14/95 - JBH
C       REPLACED GAS MASS CALCULATION (LAYER MASS * GAS MASS FRACTION) WITH
C       REFERENCE TO SPECIES MASS GLOBAL VARIABLE, ZZGSPEC, IN GAS ABSORB-
C       ANCE TERM.
C    MODIFIED 08/17/95 - JBH
C      CORRECTED VALUE OF GAS CONSTANT PER CRC HNDBK OF CHEM. & PHY.; 52ND ED.
C    MODIFIED 09/28/95
C      PER GLENN FORNEY, THE RADX ROUTINES EXPECT AN EXTINCTION COEFFICIENT, RATHER
C      THAN AN ABSORBANCE, THEREFORE, ABSORB WAS MODIFIED TO RETURN [- LOG(1 - ABSORB)/L]
C    MODIFIED 9/12/96  - GPF
C      LOG(TG) was passed in argument list to LINTERP routine.  But LINTERP routine may
C      modify it.  Therefore, a this value is stored in a variable, TGLOG and TGLOG is
C      now passed to LINTERP.
C    MODIFIED 7/3/97 - GPF
C      Eliminated a catastrophic cancellation in the absorbance calculation.  
C      Note new code is equivalent mathematically to the old.  This version is 
C      better because it removes the need for an EXP calculation and eliminates 
C      a cancellation error.  This cancellation error was causing the code bomb.
C    MODIFIED 10/19/97 GPF
C      Converted code to real*8.
C
C  DECLARE COMMON BLOCK VARIABLES (AR, BR, ZZ????, ETC) AND CONSTANTS
C  (UPPER & LOWER). ORDER OF 'INCLUDE' FILES IS CRITICAL.
C  CFAST.INC INVOKES CPARAMS.INC & DSIZE.INC
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
C
C  DECLARE PARAMETERS
C
      INTEGER NOERR, HIERR, LOERR
      PARAMETER (NOERR=0, HIERR=+1, LOERR=-1)
C
      INTEGER CO2XSIZE, CO2YSIZE, H2OXSIZE, H2OYSIZE
      PARAMETER (CO2XSIZE=11, CO2YSIZE=12, H2OXSIZE=11, H2OYSIZE=12)
C
      INTEGER CO2, H2O, SOOT
      PARAMETER (CO2=3, H2O=8, SOOT=9)	
C
C  DECLARE I/O VARIABLES
C
      INTEGER CMPT, LAYER
C
C  DECLARE INTERNAL VARIABLES
C  UNITS:
C    TG = KELVINS; TCO2, TH2O = LOG(KELVINS)
C    PLG = ATM-M; PLCO2, PLH2O = LOG(ATM-M)
C    L = M; NG = MOL; RTV = ATM/MOL
C    AG, ABSORB = NUMBER (ABSORBANCE)
C    ACO2, AH2O, ECO2, EH2O = LOG(EMISS)
C    VFS = NUMBER (SOOT VOLUME FRACTION)
C    RHOS = KG/CUBIC METER (SOOT DENSITY)
C    TRANS = NUMBER (SOOT TRANSMISSION = EXP(-K*VFS*TG*L))
C    K = 1/(KELVIN-METER) (SSOT ABSORPTION CONSTANT)
C    MWCO2, MWH2O = GAS MOLECULAR WEIGHT (KG/GM-MOLE)
C    RG = IDEAL GAS CONSTANT (ATM-M^3/MOL-K)
C
      INTEGER XCO2, YCO2, XH2O, YH2O
      DIMENSION TCO2(CO2XSIZE), PLCO2(CO2YSIZE)
      DIMENSION ECO2(CO2XSIZE,CO2YSIZE)
      DIMENSION TH2O(H2OXSIZE)
      DIMENSION PLH2O(H2OYSIZE), EH2O(H2OXSIZE,H2OYSIZE)
      real*8 MWCO2,MWH2O, K, RHOS, L, NG

C  DECLARE MODULE DATA
C
C  PHYSICAL CONSTANTS [MW IN KG/MOL; RG IN M^3-ATM/MOL-KELVIN]

      DATA MWCO2, MWH2O, RG, K, RHOS
     +  /44.0088D-3, 18.0153D-3, 82.0562D-6, 1195.5D0, 1800.0D0/
C
C  LOG(T) DATA FOR CO2 [T IN K] 
C 
      DATA TCO2  /2.3010D0, 2.4771D0, 2.6021D0, 2.6990D0, 
     +            2.7782D0, 2.8451D0, 2.9031D0, 2.9542D0,
     +            3.0000D0, 3.3010D0, 3.4771D0        /
C
C  LOG(PL) DATA FOR CO2 [PL IN ATM-M]
C
      DATA PLCO2 /-3.0000D0, -2.6990D0, -2.3979D0, -2.0000D0,
     +            -1.6990D0, -1.3979D0, -1.0000D0, -0.6990D0,
     +            -0.3979D0,  0.0000D0,  0.3010D0,  0.6021D0/
C
C  LOG(EMISS) DATA FOR CO2 [STORED IN E(T,PL) FORMAT (ASCENDING ORDER 
C  BY TEMPERATURE, THEN BY PRESSURE-LENGTH)]
C
      DATA ECO2  /-1.8508D0, -1.8416D0, -1.8508D0, -1.7799D0,
     +            -1.6990D0, -1.6799D0, -1.6904D0, -1.6990D0,
     +            -1.7399D0, -2.3706D0, -2.8996D0,
     +            -1.6990D0, -1.6799D0, -1.6904D0, -1.6308D0,
     +            -1.5498D0, -1.5302D0, -1.5302D0, -1.5498D0, 
     +            -1.5800D0, -2.1002D0, -2.6108D0,
     +            -1.5406D0, -1.5200D0, -1.5498D0, -1.4895D0,
     +            -1.4401D0, -1.3904D0, -1.3904D0, -1.4101D0,
     +            -1.4202D0, -1.8894D0, -2.3002D0,
     +            -1.3799D0, -1.3298D0, -1.3497D0, -1.3298D0,
     +            -1.2700D0, -1.2403D0, -1.2403D0, -1.2503D0,
     +            -1.2700D0, -1.6596D0, -2.0400D0,
     +            -1.2403D0, -1.2000D0, -1.2403D0, -1.2104D0,
     +            -1.1599D0, -1.1403D0, -1.1302D0, -1.1403D0,
     +            -1.1500D0, -1.5200D0, -1.8894D0,
     +            -1.1403D0, -1.1002D0, -1.1403D0, -1.1203D0,
     +            -1.0799D0, -1.0400D0, -1.0301D0, -1.0301D0,
     +            -1.0600D0, -1.3799D0, -1.7305D0, 
     +            -1.0400D0, -0.9914D0, -1.0200D0, -1.0200D0,
     +            -0.9706D0, -0.9547D0, -0.9431D0, -0.9355D0,
     +            -0.9431D0, -1.1599D0, -1.4802D0,
     +            -0.9914D0, -0.9431D0, -0.9547D0, -0.9508D0,
     +            -0.9136D0, -0.8827D0, -0.8666D0, -0.8539D0,
     +            -0.8601D0, -1.1002D0, -1.3706D0, 
     +            -0.9355D0, -0.8697D0, -0.8928D0, -0.8827D0,
     +            -0.8477D0, -0.8097D0, -0.7932D0, -0.7852D0,
     +            -0.7932D0, -1.0000D0, -1.2700D0,
     +            -0.8762D0, -0.8013D0, -0.8097D0, -0.8013D0,
     +            -0.7645D0, -0.7352D0, -0.7100D0, -0.6990D0,
     +            -0.6990D0, -0.8962D0, -1.1331D0,
     +            -0.8297D0, -0.7496D0, -0.7645D0, -0.7472D0,
     +            -0.7055D0, -0.6696D0, -0.6421D0, -0.6326D0,
     +            -0.6402D0, -0.8097D0, -1.0301D0,
     +            -0.8013D0, -0.7144D0, -0.7144D0, -0.6840D0,
     +            -0.6478D0, -0.6108D0, -0.5884D0, -0.5817D0,
     +            -0.5817D0, -0.7352D0, -0.9431D0         /
C
C  LOG(T) DATA FOR H2O [T IN K] 
C 
      DATA TH2O  /2.3201D0, 2.4771D0, 2.6021D0, 2.6990D0,
     +            2.7782D0, 2.8451D0, 2.9031D0, 2.9542D0,
     +            3.0000D0, 3.3010D0, 3.4771D0        /
C
C  LOG(PL) DATA FOR H2O [PL IN ATM-M]
C
      DATA PLH2O /-3.0000D0, -2.6990D0, -2.3979D0, -2.0000D0,
     +            -1.6990D0, -1.3979D0, -1.0000D0, -0.6990D0,
     +            -0.3979D0,  0.0000D0,  0.3010D0,  0.6021D0/
C
C  LOG(EMISS) DATA FOR H2O [STORED IN E(T,PL) FORMAT (ASCENDING ORDER 
C  BY TEMPERATURE, THEN BY PRESSURE-LENGTH)]
C
      DATA EH2O  /-1.1500D0, -1.5200D0, -1.7496D0, -1.8996D0,
     +            -2.0000D0, -2.1002D0, -2.1898D0, -2.2798D0,
     +            -2.3706D0, -3.0555D0, -3.4437D0,
     +            -1.0200D0, -1.3298D0, -1.5302D0, -1.6596D0,
     +            -1.7595D0, -1.8416D0, -1.9208D0, -2.0000D0,
     +            -2.0799D0, -2.7496D0, -3.1871D0,
     +            -0.8962D0, -1.1701D0, -1.3242D0, -1.4597D0,
     +            -1.5406D0, -1.6003D0, -1.6596D0, -1.7305D0,
     +            -1.7905D0, -2.4202D0, -2.8794D0,
     +            -0.7696D0, -1.0000D0, -1.1302D0, -1.2204D0,
     +            -1.3002D0, -1.3497D0, -1.4001D0, -1.4401D0,
     +            -1.4802D0, -1.9914D0, -2.5200D0,
     +            -0.6402D0, -0.8729D0, -0.9957D0, -1.0799D0,
     +            -1.1302D0, -1.1701D0, -1.2104D0, -1.2503D0,
     +            -1.2899D0, -1.6904D0, -2.1500D0,
     +            -0.5884D0, -0.7645D0, -0.8729D0, -0.9355D0,
     +            -0.9788D0, -1.0200D0, -1.0400D0, -1.0701D0,
     +            -1.1002D0, -1.4101D0, -1.8210D0,
     +            -0.5003D0, -0.6556D0, -0.7258D0, -0.7545D0,
     +            -0.7932D0, -0.8153D0, -0.8447D0, -0.8665D0,
     +            -0.8894D0, -1.0799D0, -1.4401D0,
     +            -0.4437D0, -0.5670D0, -0.6271D0, -0.6402D0,
     +            -0.6517D0, -0.6696D0, -0.6861D0, -0.6990D0,
     +            -0.7190D0, -0.8729D0, -1.1403D0,
     +            -0.3936D0, -0.5086D0, -0.5302D0, -0.5376D0,
     +            -0.5482D0, -0.5528D0, -0.5670D0, -0.5719D0,
     +            -0.5817D0, -0.7122D0, -0.9431D0,
     +            -0.3458D0, -0.4295D0, -0.4401D0, -0.4365D0,
     +            -0.4401D0, -0.4413D0, -0.4510D0, -0.4535D0,
     +            -0.4584D0, -0.5376D0, -0.7144D0,
     +            -0.2958D0, -0.3686D0, -0.3686D0, -0.3645D0,
     +            -0.3645D0, -0.3686D0, -0.3706D0, -0.3757D0,
     +            -0.3757D0, -0.4510D0, -0.5952D0,
     +            -0.2620D0, -0.3307D0, -0.3233D0, -0.3045D0,
     +            -0.3010D0, -0.3045D0, -0.3045D0, -0.3054D0,
     +            -0.3080D0, -0.3605D0, -0.5086D0         /
C
C  CALCULATE LAYER-SPECIFIC FACTORS
C
      TG = ZZTEMP(CMPT, LAYER)
      RTV = (RG * TG) / ZZVOL(CMPT, LAYER)
      L = ZZBEAM(LAYER,CMPT)

      AG = 0.0D0

C  CALCULATE ABSORBANCE FOR CO2

      NG = ZZGSPEC(CMPT, LAYER, CO2) / MWCO2
      PLG = NG * RTV * L
      IF (PLG>1.0D-3) THEN
        CPLG = LOG10(PLG)
        TGLOG = LOG10(TG)
        CALL LINTERP(CO2XSIZE, CO2YSIZE, TCO2, PLCO2, ECO2, TGLOG,
     +               CPLG, ACO2, XCO2, YCO2)
        AG = AG + 0.50D0*10.0D0**ACO2
      ENDIF


C  CALCULATE ABSORBANCE FOR H2O

      NG = ZZGSPEC(CMPT, LAYER, H2O) / MWH2O
      PLG = NG * RTV * L
      IF (PLG>1.0D-3) THEN
        CPLG = LOG10(PLG)
        TGLOG = LOG10(TG)
        CALL LINTERP(H2OXSIZE, H2OYSIZE, TH2O, PLH2O, EH2O, TGLOG,
     +               CPLG, AH2O, XH2O, YH2O)
        AG = AG + 10.0D0**AH2O
      ENDIF


C  CALCULATE TOTAL ABSORBANCE

      VFS = ZZGSPEC(CMPT,LAYER,SOOT)/(ZZVOL(CMPT,LAYER) * RHOS)
c      TRANS = EXP(-1. * K * VFS * TG * L) 
c      ABSORB = (1 - TRANS) + AG * TRANS
c      ABSORB = - LOG(1 - ABSORB)/L
c
c*** The following line of code is equivalent
c    to the previous three lines and has the virture of
c    eliminating an exp calculation and the cancellation
c    error caused by the subtraction in (1-TRANS) gpf 7/3/97

      ABSORB = MAX(K*VFS*TG - LOG(1.0D0-AG)/L,0.01D0)
      RETURN
 1000 FORMAT ('ERROR IN ',A3,' ABSORBANCE: XERROR = 'I2,
     +    '; YERROR = ',I2)
      END

      SUBROUTINE LINTERP(XDIM, YDIM, X, Y, Z, XVAL, YVAL, ZVAL,
     + XERR, YERR)
C
C  SUBROUTINE CALCULATES A 2-D LINEAR INTERPOLATION OF F(X,Y); WHERE KNOWN
C  F(X,Y) VALUES ARE IN Z, ALLOWED X AND Y VALUES ARE IN X AND Y, THE POINT
C  TO BE INTERPOLATED IS (XVAL,YVAL) AND THE INTERPOLATED RESULT IS RETURNED
C  AS ZVAL. ARRAY DIMENSIONS ARE SPECIFIED BY XDIM AND YDIM, XERR AND YERR
C  ARE ERROR VALUES RETURNED TO THE CALLING FUNCTION.
C
C  THE EQUATION IMPLIMENTED BY THIS FUNCTION IS:
C
C  F(X,Y) = Z(I,J) + {[Z(I+1,J) - Z(I,J)] / [X(I+1) - X(I)]} * [X - X(I)] 
C           + {[Z(I,J+1) - Z(I,J)] / [Y(J+1) - Y(I)]} * [Y - Y(J)]
C
C  VERSION 1.0
C
C  REVISION HISTORY:
C     CREATED 12/07/94 - JBH
C
C      IMPLICIT NONE
      include "precis.fi"
      INTEGER HIERR
      PARAMETER (NOERR=0, HIERR=+1, LOERR=-1) 
C
C  DECLARE I/O PARAMETERS 
C
      INTEGER XDIM, YDIM, XERR, YERR
      DIMENSION X(XDIM), Y(YDIM), Z(XDIM,YDIM)
C
C  DECLARE INTERNAL VARIABLES
C
      INTEGER COUNT
C
C  FIND THE VALUE OF I SUCH THAT X(1) <= XVAL <= X(XDIM).
C  IF XVAL IS OUTSIDE THAT RANGE, SET IT TO THE CLOSEST LEGAL VALUE
C  AND SET THE ERROR VALUE, AS APPROPRIATE.
C
C  CHECK THE SPECIAL CASE OF XVAL < X(1)
C
      IF (XVAL < X(1)) THEN
        XERR = LOERR
        XVAL = X(1)
        I = 1
C
C  CHECK THE SPECIAL CASE OF XVAL > X(XDIM)
C
      ELSE IF (XVAL > X(XDIM)) THEN
        XERR = HIERR
        XVAL = X(XDIM)
        I = XDIM
C
C  CHECK THE CASES WHERE X(1) <= XVAL < X(XDIM)
C
      ELSE
        XERR = NOERR
        DO 10 COUNT=2,XDIM
          IF (XVAL < X(COUNT)) THEN
            I = COUNT - 1
            GO TO 20
          endif 
   10   CONTINUE
C
C  THEN XVAL = X(XDIM)
C
        I = XDIM
   20   CONTINUE
      endif
C
C  CHECK THE SPECIAL CASE OF YVAL < Y(1)
C
      IF (YVAL < Y(1)) THEN
        YERR = LOERR
        YVAL = Y(1)
        J = 1
C
C  CHECK THE SPECIAL CASE OF YVAL > Y(YDIM)
C
      ELSE IF (YVAL > Y(YDIM)) THEN
        YERR = HIERR
        YVAL = Y(YDIM)
        J = YDIM
C
C  CHECK THE CASES OF Y(1) <= YVAL < Y(YDIM)
C
      ELSE
        YERR = NOERR
        DO 30 COUNT=2,YDIM
          IF (YVAL < Y(COUNT)) THEN
            J = COUNT - 1
            GO TO 40
          endif
   30 CONTINUE
C
C  THEN YVAL = Y(YDIM)
C
      J = YDIM
   40 CONTINUE
      endif
C
C  CALCULATE DELTA X, SLOPE X AND THE Z INCREMENT DUE TO A CHANGE IN X.
C  IF XVAL = X(XDIM), THEN (I+1) IS UNDEFINED AND THE SLOPE CAN NOT BE
C  CALCULATED. HOWEVER, IN THOSE CASES, DELTA X IS ZERO, THERE IS NO 
C  CONTRIBUTION DUE TO THE CHANGE IN X AND THE ENTIRE TERM MAY BE SET
C  EQUAL TO ZERO.
C
      DELTAX = XVAL - X(I)
      IF (DELTAX /= 0.0D0) THEN
        DZDX = (Z(I+1,J) - Z(I,J)) / (X(I+1) - X(I))
        DELX = DZDX * DELTAX
      ELSE
        DELX = 0.
      endif
C
C  CALCULATE THE Z INCREMENT DUE TO A CHANGE IN Y AS ABOVE.
C
      DELTAY = YVAL - Y(J)
      IF (DELTAY /= 0.0D0) THEN
        DZDY = (Z(I,J+1) - Z(I,J)) / (Y(J+1) - Y(J))
        DELY = DZDY * DELTAY
      ELSE
        DELY = 0.
      endif
C
C  INTERPOLATE A VALUE FOR F(X,Y)
C
      ZVAL = Z(I,J) + DELX + DELY
      RETURN
      END

      integer function rev_radiation
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_radiation = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_radiation