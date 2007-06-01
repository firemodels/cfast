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
C     Function:   Interface between DSUORC and RAD2 or RAD4.  Loops over
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

      IF (OPTION(FRAD).EQ.OFF) RETURN
      BLACK = .FALSE.
      IF(OPTION(FRAD).EQ.3)BLACK = .TRUE.

C*** Initially assume that we compute radiation transfer in every room

      DO 15 I = 1, NM1
        ROOMFLG(I) = .TRUE.
   15 CONTINUE

      IF(OPTION(FMODJAC).EQ.ON)THEN
        IF(JACCOL.GT.0)THEN

C*** If 2nd modified jacobian is active and dassl is computing a jacobian then
C    only compute the radiation heat transfer in the room where the dassl 
C    solution variable has been perturbed

          DO 16 I = 1, NM1
            ROOMFLG(I) = .FALSE.
   16     CONTINUE
          IEQTYP = IZEQMAP(JACCOL,1)
          IROOM = IZEQMAP(JACCOL,2)
          IF(IEQTYP.EQ.EQVU.OR.IEQTYP.EQ.EQTU.OR.IEQTYP.EQ.EQTL.OR.
     .       IEQTYP.EQ.EQWT)THEN
             IF(IEQTYP.EQ.EQWT)IROOM = IZWALL(IROOM,1)
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
      IF(LFBO.NE.0.AND.OPTION(FRAD).NE.4.AND.LFBT.NE.1)THEN
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
          IF(MOD(IWALL,2).EQ.1)THEN
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
          END IF
   50   CONTINUE
        IFIRE = IFRPNT(I,2)
        NRMFIRE = IFRPNT(I,1)
        IF (NRMFIRE.NE.0) THEN
          IF(.NOT.BLACK)THEN
            IF(OPTION(FRAD).EQ.4.OR.LFBT.EQ.1)THEN
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
            IF(OPTION(FRAD).EQ.2.OR.OPTION(FRAD).EQ.4.OR.
     .         LFBT.EQ.1)THEN
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
        IF (IERROR.NE.0) RETURN
        DO 30 J = 1, NWAL
          FLXRAD(I,J) = QFLXW(MAP(J))
   30   CONTINUE

        FLWRAD(I,1) = QLAY(1)
        FLWRAD(I,2) = QLAY(2)
        QR(1,I) = QLAY(1)
        QR(2,I) = QLAY(2)
   40 CONTINUE

      IF(OPTION(FMODJAC).EQ.ON)THEN
        IF(JACCOL.EQ.0)THEN

C*** if the jacobian option is active and dassl is computing the base vector for
C    the jacobian calculation then save the flow and flux calculation for later use

          DO 55 IROOM = 1, NM1
            DO 60 IWALL = 1, NWAL
              FLXRAD0(IROOM,IWALL) = FLXRAD(IROOM,IWALL)
   60       CONTINUE
            FLWRAD0(IROOM,1) = FLWRAD(IROOM,1)
            FLWRAD0(IROOM,2) = FLWRAD(IROOM,2)
   55     CONTINUE
         ELSEIF(JACCOL.GT.0)THEN

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
