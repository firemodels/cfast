      SUBROUTINE DOFIRE(IFIRE,IROOM,XEMP,XHR,XBR,XDR,HCOMBT,CCO2T,
     .    COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,XMFIR,STMASS,XFX,XFY,
     .    XFZ,XEME,XEMS,XQPYRL,XNTMS,XQF,XQFC,XQFR,XQLP,XQUP)

C
C     Routine:     DOFIRE

C     Description:  Do heat release from a fire for both main fire and
C               objects
C
C     Inputs:   IFIRE   - fire number (ifire=0 is the main fire)
C               IROOM   - room containing the fire
C               XEMP    - pyrolysis rate of the fire (kg/s)
C               XHR     - height of the room (m)
C               XBR     - breadth of the room (m)
C               HCOMBT  - current heat of combustion (J/kg)
C               CCO2T   - current carbon/CO2 production ratio (kg/kg)
C               COCO2T  - current CO/CO2 production ratio (kg/kg)
C               HCRATT  - current Hydrogen/Carbon ratio in fuel (kg/kg)
C               OCRATT  - current Oxygen/Carbon ratio in fuel (kg/kg)
C               CLFRAT  - current HCl production rate (kg/kg burned)
C               CNFRAT  - current HCN production rate (kg/kg burned)
C               XMFIR   - production rate of a species in fire (kg/s)
C               STMASS   - mass of a species in a layer in the room (kg)
C               XFX     - position of the fire in x direction
C               XFY     - position of the fire in y direction
C               XFZ     - position of the fire in z direction
C     Outputs:  XEME    - plume entrainment rate (kg/s)
C               XEMS    - plume flow rate into the upper layer (kg/s)
C               XQPYRL  - actual heat release rate of the fire (W)
C               XNTMS   - net change in mass of a species in a layer
C               XQF     - net heat generation rate into upper layer (W)
C               XQFC    - net convection into layers (W)
C               XQFR    - net radiation from fire (W)
C               XQLP    - heat release in the lower plume (W)
C               XQUP    - heat release rate in the upper plume (W)
C
C     Revision History:
C     wwj   4/10/98 fixed upper/lower layer separation (depends on where the fire is)
C     gpf    4/9/98 added some more initializations for when the fire
C                   is only in the upper layer
C     gpf  10/19/97 added initialization to XQFC, error only showed when
C                   there was upper but not lower layer burning
C     gpf   2/27/96 fixed definition of XXFIREU (was missing XZ term)
C     gpf   2/5/96  added missing initialization to XQFR
C     gpf   6/30/95 commented out species specific code from a 'type 1' fire
C                   calculation
C     gpf   4/24/95  removed reference to tmass to remove flint complaint
C     RDP   4/8/94    Added multiple pass through fire "chemistry" for
C                     oxygen limited fires so that plume entrainment is
C                     consistent with actual fire size.
C     gpf   10/14/93 added detection/suppression
C     RDP   11/10/91 Modified call to firplm for entrainment type.
C                    Modified so NETMAS returns just amount for this
C                    fire.  Accumulation must be done by calling
C                    routine.  Note new definition of NETMAS.
C                    Standardized calling sequence.
C     WWJ   5/15/91  eliminate general contribution from plume
C                    done in chemie, add in just ct and tuhc
C     WWJ   2/9/91   changed limit on plume entrainment
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fireptrs.fi"

      DIMENSION XMFIR(NS), XNTMS(2,NS), XQFC(2), STMASS(2,NS), XMASS(NS)
      LOGICAL PASS2

      X1 = 1.0D0
      X0 = 0.0D0

C--- note: added upper/lower parameters to following three statements.
C          xtu was incorrectly set to lower layer temp, fixed it

      XZ = ZZHLAY(IROOM,UPPER)
      XTL = ZZTEMP(IROOM,LOWER)
      XTU = ZZTEMP(IROOM,UPPER)
      XQFC(LOWER) = 0.0D0
      XQFC(UPPER) = 0.0D0
      XQLP = 0.0D0
      XEME = 0.0D0

C     THESE ARE THE LENGTHS ("HEIGHTS") IN THE UPPER AND LOWER LAYERS RESPECTIVELY
C     IF IT IS NEGATIVE, THEN THE FIRE IS NOT IN THAT LAYER

      XXFIREL = XHR - XZ - XFZ
	XXFIREU = XHR - XFZ
      XNTFL = X0
      QHEATL = X0
      QHEATU = X0
      XQFR = X0
      XEMS = X0

      DO 10 LSP = 1, NS
        XNTMS(UPPER,LSP) = X0
        XNTMS(LOWER,LSP) = X0
        XMASS(LSP) = X0
   10 CONTINUE

C     DIVVY UP THE PLUME OUTPUT INTO RADIATION AND CONVECTIVE ENERGY.
C     CONVECTION DRIVES THE PLUME ENTRAINMENT

      CHIRAD = MAX(MIN(radconsplit(ifire),X1),X0)
      QHEATL = MAX((XQPYRL+CP*(TE-XTL)*XEMP)*(X1-CHIRAD),X0)

C     CALCULATE THE SPECIES MASS FLOW IN THE PLUME FOR TYPE 1 FIRE,
C     NO KINETICS (UNCONSTRAINED BURNING)

      IF (LFBT.EQ.FREE) THEN
!	We have eliminated unconstrained fires, if we reach this point, the input parser has failed!
	  stop 101

C     CALCULATE THE ENTRAINMENT RATE BUT CONSTRAIN THE ACTUAL AMOUNT
C     OF AIR ENTRAINED TO THAT REQUIRED TO PRODUCE STABLE STRATIFICATION

        CALL FIRPLM(QHEATL,MAX(X0,XXFIREL),XEMP,XEMS,XEME,
     .      MIN(XFX,XbR-XFX),MIN(XFY,XdR-XFY))
        XEME = MIN(XEME,QHEATL/(MAX((XTU-XTL),X1)*CP))
        XEMS = XEMP + XEME

        XQPYRL = QHEATL / (X1-CHIRAD)

        XQFR = XQPYRL * CHIRAD
        XQFC(UPPER) = XQPYRL * (X1-CHIRAD)
        XQFC(LOWER) = X0
        XQLP = XQPYRL
        XQF = XQPYRL
      ELSE

C     NOTE THAT THE COMBINATION OF FIRPLM AND CHEMIE CAN BE CALLED TWICE
C     IN A SINGLE ITERATION TO MAKE SURE THAT THE PLUME ENTRAINMENT IS
C     CONSISTENT WITH THE ACTUAL FIRE SIZE FOR OXYGEN LIMITED FIRES
C     THIS IS DONE BY "RE-PASSING" THE ACTUAL FIRE SIZE TO FIRPLM IN THE
C     SECOND PASS

	  PASS2 = .FALSE.

C     CALCULATE THE ENTRAINMENT RATE BUT CONSTRAIN THE ACTUAL AMOUNT
C     OF AIR ENTRAINED TO THAT REQUIRED TO PRODUCE STABLE STRATIFICATION

   40   CALL FIRPLM(QHEATL,XXFIREL,XEMP,XEMS,XEME,
     .      MIN(XFX,XbR-XFX),MIN(XFY,XdR-XFY))

C     Only do the upper layer (the fire is not in the lower layer)

        IF (XXFIREL.LE.X0) GO TO 90
        XEME = MIN(XEME,QHEATL/(MAX((XTU-XTL),X1)*CP))
        XEMS = XEMP + XEME

        CALL CHEMIE(QSPRAY(IFIRE,LOWER),XEMP,XEME,IROOM,LOWER,HCOMBT,
     .      CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,XQPYRL,XNTFL,XMASS)

C       LIMIT THE AMOUNT ENTRAINED TO THAT ACTUALLY ENTRAINED BY THE
C       FUEL BURNED

        XQPYRL = MAX(X0, (XQPYRL+CP*(TE-XTL)*XEMP)*(X1-CHIRAD))

        IF (XQPYRL.LT.QHEATL) THEN
          XEME = XEME * (XQPYRL/QHEATL)
          QHEATL = XQPYRL
          IF (.NOT.PASS2) THEN
            PASS2 = .TRUE.
            GO TO 40
          END IF
        END IF
        XQPYRL = XQPYRL/(X1-CHIRAD)

        XEMS = XEMP + XEME
        DO 50 I = 1, NS
          XNTMS(UPPER,I) = XMASS(I) + XNTMS(UPPER,I)
   50   CONTINUE
C
C       ADD THE SPECIES FLOW ENTRAINED BY THE PLUME
C
        XTEMP = X0
        DO 60 LSP = 1, 9
          XTEMP = XTEMP + STMASS(LOWER,LSP)
   60   CONTINUE
        IF(XTEMP.EQ.0.0D0)XTEMP = 1.0D0
        DO 70 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            XNET = XEME * STMASS(LOWER,LSP) / XTEMP
            XNTMS(UPPER,LSP) = XNTMS(UPPER,LSP) + XNET
            XNTMS(LOWER,LSP) = XNTMS(LOWER,LSP) - XNET
          END IF
   70   CONTINUE

C       ADD IN THE FUEL AND CT.  EVERYTHING ELSE IS DONE BY CHEMIE.

        XNTMS(UPPER,7) = XNTMS(UPPER,7) + XMFIR(7)
        XNTMS(UPPER,10) = XNTMS(UPPER,10) + XMFIR(10)
        XQFR = XQPYRL * CHIRAD
        XQFC(UPPER) = XQPYRL * (X1-CHIRAD)
        XQLP = XQPYRL
        XQF = XQPYRL

C       ADD BURNING IN THE UPPER LAYER TO THE FIRE. THE HEAT WHICH
C       DRIVES ENTRAINMENT IN THE UPPER LAYER IS THE SUM OF THE
C       HEAT RELEASED IN THE LOWER LAYER AND WHAT CAN BE RELEASED
C       IN THE UPPER LAYER.
C
C       START WITH THE FUEL REMOVED BY LOWER LAYER BURNING, XNTFL
C       UMPLM{EP},{ES},AND {EE} ARE EQUIVALENT TO EMP, EMS AND EME

   90   XQUP = 0.0D0
	  UPLMEP = MAX(X0,XEMP-XNTFL)

        IF (UPLMEP.GT.X0) THEN
          QHEATU = HCOMBT * UPLMEP + QHEATL
          HEIGHT = MAX (X0, MIN(XZ,XXFIREU))
          CALL FIRPLM(QHEATU,HEIGHT,UPLMEP,UPLMES,UPLMEE,
     .        MIN(XFX,XbR-XFX),MIN(XFY,XdR-XFY))
          CALL CHEMIE(QSPRAY(IFIRE,UPPER),UPLMEP,UPLMEE,IROOM,UPPER,
     .        HCOMBT,CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,XQPYRL,
     .        XNTFL,XMASS)
          XQFR = XQPYRL * CHIRAD + XQFR
          XQFC(UPPER) = XQPYRL * (X1-CHIRAD) + XQFC(UPPER)
          XQUP = XQPYRL
          XQF = XQPYRL + XQF
          DO 80 I = 1, NS
            XNTMS(UPPER,I) = XMASS(I) + XNTMS(UPPER,I)
   80     CONTINUE
        END IF
        
      END IF
      RETURN
      END
