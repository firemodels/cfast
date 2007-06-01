      SUBROUTINE DJET(FLWDJF,DJETFLG)
C*RB
C     Routine:  DJET
C
C     Function: Physical interface routine to calculate the current
C               rates of mass and energy flows into the layers from
C               all door jet fires in the building.
C
C               Note that we presume that this calculation is performed
C               after the normal fires and flow through vents so we
C               have a heat of combustion to use for the burning fuel.
C               At present, this heat of combustion is presumed to be
C               that of the main fire.
C
C     Inputs:   NFIRE   Total number of normal fires
C     Outputs:  FLWDJF  Mass and energy flows into layers due to fires.
C                       Standard source routine data structure.
C     Commons:
C      PASSED:  Vsas     Zzgspec  Zztemp
C        USED:  Izvent   N        Nvents
C
C     Revision History:
C        Modified: 2/7/93 by GPF:
C                  The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have
C                  the number of fires as the first subscript.
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.  Execute this routine only if this 
C                  modeling feature is being used (rather than zeroing out 
C                  the flow vector.)
C*RE
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "flwptrs.fi"
      include "opt.fi"
      include "vents.fi"
C
      DIMENSION FLWDJF(NR,NS+2,2), XNTMS1(2,NS), XNTMS2(2,NS)
      DIMENSION FLWDJF0(NR,NS+2,2)
      SAVE FLWDJF0

      LOGICAL DJETFLG,DJ1FLAG,DJ2FLAG
      LOGICAL VENTFLG(MXVENT), ROOMFLG(NR), anyvents
C
C     INITIALIZE SUMMATIONS AND LOCAL DATA
C
      DJETFLG = .FALSE.
      XX0 = 0.0D0
      IF (OPTION(FDFIRE).NE.ON.OR.NFIRE.LE.0) RETURN


c*** if no vents have a door jet fire then exit

      DO 5 I = 1, NVENTS

c*** is there a door jet fire into room IROOM1

        IROOM1 = IZVENT(I,1)
        IF(ZZTEMP(IROOM1,UPPER).GE.TGIGNT)then
          FLW1TO2 = VSS(1,I)+VSA(1,I)
          IF(VSAS(2,I).GT.XX0.AND.FLW1TO2.GT.XX0)THEN
            DJETFLG = .TRUE.
            GO TO 1
          ENDIF
        ENDIF

c*** is there a door jet fire into room IROOM2

        IROOM2 = IZVENT(I,2)
        IF(ZZTEMP(IROOM2,UPPER).GE.TGIGNT)then
          FLW2TO1 = VSS(2,I)+VSA(2,I)
          IF(VSAS(1,I).GT.XX0.AND.FLW2TO1.GT.XX0)THEN
             DJETFLG = .TRUE.
             GO TO 1
          ENDIF
        ENDIF
    5 CONTINUE
    1 CONTINUE

      IF(.NOT.DJETFLG)RETURN
      DO 10 IFROM = 1, N
        DO 20 LSP = 1, NS + 2
          FLWDJF(IFROM,LSP,LOWER) = XX0
          FLWDJF(IFROM,LSP,UPPER) = XX0
   20   CONTINUE
   10 CONTINUE

      DO 25 I = 1, N
        FQDJ(I) = XX0
   25 CONTINUE
C
      HCOMBT = XFIRE(1,12)
C
C     CALCULATE THE HEAT FOR EACH OF THE DOOR JET FIRES
C
      CALL VENTFLAG(VENTFLG,ROOMFLG,ANYVENTS)
      IF(ANYVENTS)THEN
      DO 40 I = 1, NVENTS
        IF(.NOT.VENTFLG(I))GO TO 40
        IROOM1 = IZVENT(I,1)
        IROOM2 = IZVENT(I,2)
        FLW1TO2 = ZZCSPEC(IROOM1,UPPER,7) * (VSS(1,I)+VSA(1,I))
        FLW2TO1 = ZZCSPEC(IROOM2,UPPER,7) * (VSS(2,I)+VSA(2,I))
        CALL DJFIRE(IROOM2,ZZTEMP(IROOM1,UPPER),
     .    FLW1TO2,VSAS(2,I),HCOMBT,QPYROL2,XNTMS2,DJ2FLAG)
        CALL DJFIRE(IROOM1,ZZTEMP(IROOM2,UPPER),
     .    FLW2TO1,VSAS(1,I),HCOMBT,QPYROL1,XNTMS1,DJ1FLAG)
C
C       SUM THE FLOWS FOR RETURN TO THE SOURCE ROUTINE
C
        IF(DJ1FLAG)THEN
          FLWDJF(IROOM1,Q,UPPER) = FLWDJF(IROOM1,Q,UPPER) + QPYROL1
          DO 30 LSP = 1, NS
            FLWDJF(IROOM1,LSP+2,UPPER) = FLWDJF(IROOM1,LSP+2,UPPER) +
     .                                   XNTMS1(UPPER,LSP)
   30     CONTINUE
        ENDIF
        IF(DJ2FLAG)THEN
          FLWDJF(IROOM2,Q,UPPER) = FLWDJF(IROOM2,Q,UPPER) + QPYROL2
          DO 31 LSP = 1, NS
            FLWDJF(IROOM2,LSP+2,UPPER) = FLWDJF(IROOM2,LSP+2,UPPER) +
     .                                   XNTMS2(UPPER,LSP)
   31     CONTINUE
        ENDIF
   40 CONTINUE
      endif

      IF(OPTION(FMODJAC).EQ.ON)THEN
        IF(JACCOL.EQ.0)THEN

C*** we need to save the solution for later jacobian calculations

          DO 140 IROOM = 1, NM1
            DO 150 LSP = 1, NS + 2
              FLWDJF0(IROOM,LSP,LOWER) = FLWDJF(IROOM,LSP,LOWER)
              FLWDJF0(IROOM,LSP,UPPER) = FLWDJF(IROOM,LSP,UPPER)
  150       CONTINUE
  140     CONTINUE
         ELSEIF(JACCOL.GT.0)THEN

C*** we are computing a jacobian, so get previously saved solution for rooms
C    that are not affected by the perturbed solution variable

          DO 160 IROOM = 1, NM1
            IF(.NOT.ROOMFLG(IROOM))THEN
              DO 170 LSP = 1, NS+2
                FLWDJF(IROOM,LSP,LOWER) = FLWDJF0(IROOM,LSP,LOWER)
                FLWDJF(IROOM,LSP,UPPER) = FLWDJF0(IROOM,LSP,UPPER)
  170         CONTINUE
            ENDIF
  160     CONTINUE
        ENDIF
      ENDIF

      DO 50 I = 1, N
        FQDJ(I) = FLWDJF(I,Q,UPPER) + FLWDJF(I,Q,LOWER)
        HEATVF(I) = FLWDJF(I,Q,UPPER)
   50 CONTINUE
      RETURN
      END
