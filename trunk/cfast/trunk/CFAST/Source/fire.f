      SUBROUTINE FIRES(TSEC,FLWF,UPDATE)
C
C     Routine:     FIRES
C
C     Description:  Physical interface routine to calculate the current
C                   rates of mass and energy flows into the layers from
C                   all fires in the building.
C
C     Arguments: TSEC   Current simulation time (s)
C                FLWF   Mass and energy flows into layers due to fires.
C                       Standard source routine data structure.
C                NFIRE  Total number of fires
C                IFROOM Room numbers for each of the fires
C                XFIRE  Fire related quantities used by other routines.
C                       (I,1 to 3) = X, Y, and Z position for fire i
C                       (I,4) = Mass into upper layer from fire i (EMS)
C                       (I,5) = Pyrolysis rate from fire i (EMP)
C                       (I,6) = Mass entrained in plume by fire i (EME)
C                       (I,7 & 8) = Convective, and radiative heat into
C                               upper layer, fire i
C                       (I,9) = Total heat released by fire i
C                       (I,10) = Total heat into lower layer by fire i
C                       (I,11) = Total heat into upper layer by fire i
C                       (I,12 to 18) = Heat of combustion, C/CO2,
C                                CO/CO2, H/C, O/C, HCl, HCN yields for
C                                fire i
C					   (I,19) characteristic length of the burning volume
C
!     Revision History:
!          7/93 Created
!         10/93 added detection/suppression
!          4/95 removed references to TMASS to eliminate FLINT complaint
!          8/95 added UPDATE to argument list for FIRES and OBJINT.  Fixed so LFBT = 0 means no main fire.
!          7/96 update hall data structures for rooms that are halls and have a main fire.
!          9/96 changed qf to qfc in fire flow computation (qdot terms in ode's use convective not total
!               heat release, radiative contribution to layers is handled in rdheat.)
!          5/98 Implement fast startup option.  Execute this routine only if this 
!               modeling feature is being used (rather than zeroing out the flow vector.)
!          3/07 modify chemistry to include radiological species. Note that the knockdown from sprinklers is 
!               applied in CHEMIE to the pyrolysis rate and in the interpreter to mass. Since knockdown is 
!               applied to pyrolysis rate, (see notes below) the radiological production is the reduced 
!               pyrolysis rate times the bare crfrat
!          5/08 separate pyrolysis and kinetics to correct error in upper layer burning species generation

      include "precis.fi"
      include "cfast.fi"
      include "flwptrs.fi"
      include "params.fi"
      include "cenviro.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "opt.fi"
      include "fltarget.fi"

      DIMENSION FLWF(NR,NS+2,2), XNTMS(2,NS), STMASS(2,NS)
      INTEGER CJETOPT
      INTEGER UPDATE

C     INITIALIZE SUMMATIONS AND LOCAL DATA

      DO 30 LSP = 1, NS + 2
        DO 20 IROOM = 1, N
          FLWF(IROOM,LSP,UPPER) = 0.0D0
          FLWF(IROOM,LSP,LOWER) = 0.0D0
   20   CONTINUE
   30 CONTINUE
      NFIRE = 0

      IF (OPTION(FFIRE).NE.FCFAST) RETURN

!	Start with the main fire (it is object 0)

      IF (LFBO.GT.0.AND.LFBO.LT.N.AND.LFBT.GT.0) THEN

C       CALCULATE THE FIRE OUTPUTS FOR THE MAIN FIRE

        CALL PYROLS(TSEC,LFBO,EMP(LFBO),APS(LFBO),HF0T,QPYROL,HCOMBT,
     +      CCO2T,COCO2T,HCRATT,OCRATT,
     +      CLFRAT,CNFRAT,crfrat,MFIRET)
        DO 40 LSP = 1, NS
          STMASS(UPPER,LSP) = ZZGSPEC(LFBO,UPPER,LSP)
          STMASS(LOWER,LSP) = ZZGSPEC(LFBO,LOWER,LSP)
   40   CONTINUE

        CALL DOFIRE(0,LFBO,EMP(LFBO),HR(LFBO),BR(LFBO),DR(LFBO),HCOMBT,
     +      CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,crfrat,MFIRET,
     +      STMASS,FPOS(1),FPOS(2),FPOS(3)+HF0T,EME(LFBO),EMS(LFBO),
     +      QPYROL,XNTMS,QF(LFBO),QFC(1,LFBO),XQFR,HEATLP(LFBO),
     +      HEATUP(LFBO),objcl(0))

!     SUM THE FLOWS FOR RETURN TO THE SOURCE ROUTINE

        XTL = ZZTEMP(LFBO,LOWER)
        FLWF(LFBO,M,UPPER) = FLWF(LFBO,M,UPPER) + EMS(LFBO)
        FLWF(LFBO,M,LOWER) = FLWF(LFBO,M,LOWER) - EME(LFBO)
        QEME = CP * EME(LFBO) * XTL
        QEMP = CP * EMP(LFBO) * TE
        FLWF(LFBO,Q,UPPER) = FLWF(LFBO,Q,UPPER) + QFC(UPPER,LFBO)
     .                     + QEME + QEMP
        FLWF(LFBO,Q,LOWER) = FLWF(LFBO,Q,LOWER) - QEME
        DO 50 LSP = 1, NS
          FLWF(LFBO,LSP+2,UPPER) = FLWF(LFBO,LSP+2,UPPER) +
     +        XNTMS(UPPER,LSP)
          FLWF(LFBO,LSP+2,LOWER) = FLWF(LFBO,LSP+2,LOWER) +
     +        XNTMS(LOWER,LSP)
   50   CONTINUE

C       MAKE UP IFROOM AND XFIRE FOR THE MAIN FIRE

        NFIRE = NFIRE + 1
        IFROOM(NFIRE) = LFBO
        XFIRE(NFIRE,1) = FPOS(1)
        XFIRE(NFIRE,2) = FPOS(2)
        XFIRE(NFIRE,3) = FPOS(3) + HF0T
        XFIRE(NFIRE,4) = EMS(LFBO)
        XFIRE(NFIRE,5) = EMP(LFBO)
        XFIRE(NFIRE,6) = EME(LFBO)
        XFIRE(NFIRE,7) = QFC(1,LFBO)
        XFIRE(NFIRE,8) = XQFR
        XFIRE(NFIRE,9) = HEATLP(LFBO) + HEATUP(LFBO)
        XFIRE(NFIRE,10) = HEATLP(LFBO)
        XFIRE(NFIRE,11) = HEATUP(LFBO)
        XFIRE(NFIRE,12) = HCOMBT
        XFIRE(NFIRE,13) = CCO2T
        XFIRE(NFIRE,14) = COCO2T
        XFIRE(NFIRE,15) = HCRATT
        XFIRE(NFIRE,16) = OCRATT
        XFIRE(NFIRE,17) = CLFRAT
        XFIRE(NFIRE,18) = CNFRAT
	  xfire(nfire,19) = objcl(0)
        FROOM(0) = LFBO
        FEMP(0) = EMP(LFBO)
        FEMS(0) = EMS(LFBO)
! note that cnfrat is not reduced by sprinklers, but emp is so femr is
! (see code in chemie and pyrols)
        femr(0) = emp(lfbo) * crfrat
        FQF(0) = HEATLP(LFBO) + HEATUP(LFBO)
        FQFC(0) = QFC(1,LFBO)
        FQLOW(0) = HEATLP(LFBO)
        FQUPR(0) = HEATUP(LFBO)
        FAREA(0) = APS(LFBO)

C*** update HALL data structures for rooms that are halls and have a main
C    fire

        I = LFBO
        IF(UPDATEHALL.AND.IZHALL(I,IHROOM).EQ.1.AND.
     .                    IZHALL(I,IHMODE).NE.IHAFTER)THEN
          IF (SWITCH(1,I)) THEN
            TCEIL = TWJ(1,I,1)
           ELSE
            TCEIL = ZZTEMP(I,UPPER)
          END IF
          IF (SWITCH(3,I)) THEN
            TUWALL = TWJ(1,I,3)
           ELSE
            TUWALL = ZZTEMP(I,UPPER)
          END IF

C*** don't calculate heat transfer

          CJETOPT = 2

          XXFIRE = XFIRE(NFIRE,1)
          YYFIRE = XFIRE(NFIRE,2)
          ZZFIRE = XFIRE(NFIRE,3)

C*** it doesn't matter what ZZLOC is since we are not using the
C    temperature or velocity of the ceiling jet.  we are just interested
C    in the maximum value

          ZZLOC = HR(I) - 0.1D0

          CALL CEILHT(XFIRE(NFIRE,4),XFIRE(NFIRE,7),TCEIL,
     +        ZZTEMP(I,LOWER),ZZTEMP(I,UPPER),TUWALL,bR(I),dR(I),
     +        HR(I),XXFIRE,YYFIRE,ZZFIRE,
     +        ZZHLAY(I,LOWER),ZZRHO(I,LOWER),ZZRHO(I,UPPER),CJETOPT,
     +        XXFIRE,YYFIRE,ZZLOC,
     +        1,QCEIL,QFCLGA,QFWLA,QFWUA,
     +        FTEMP,FVEL,FTMAX,FVMAX,FDMAX)

          WIDTH = MIN(BR(I),DR(I))
          CALL SETHALL(2,-1,I,TSEC,WIDTH,FTMAX,FVMAX,FDMAX)
        ENDIF

      END IF

C     OTHER FIRES COME FROM THE OBJECT DATABASE

      NOBJ = 0
      DO 80 I = 1, NUMOBJL
        IF (OBJPNT(I).GT.0) THEN
          IROOM = OBJRM(I)
          IOBJ = OBJPNT(I)
          CALL OBJINT(I,TSEC,IROOM,OMASST,OAREAT,HF0T,
     +        QPYROL,HCOMBT,CCO2T,COCO2T,HCRATT,MFIRET,OCRATT,
     +        CLFRAT,CNFRAT,crfrat,UPDATE)
          OPLUME(1,IOBJ) = OMASST

          DO 60 LSP = 1, NS
            STMASS(UPPER,LSP) = ZZGSPEC(IROOM,UPPER,LSP)
            STMASS(LOWER,LSP) = ZZGSPEC(IROOM,LOWER,LSP)
   60     CONTINUE

          CALL DOFIRE(I,IROOM,OPLUME(1,IOBJ),HR(IROOM),BR(IROOM),
     +        DR(IROOM),HCOMBT,CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,
     +        crfrat,MFIRET,STMASS,OBJPOS(1,IOBJ),OBJPOS(2,IOBJ),
     +        OBJPOS(3,IOBJ)+HF0T,OPLUME(2,IOBJ),OPLUME(3,IOBJ),QPYROL,
     +        XNTMS,QF(IROOM),QFC(1,IROOM),XQFR,HEATLP(IROOM),
     +        HEATUP(IROOM),objcl(i))

C       SUM THE FLOWS FOR RETURN TO THE SOURCE ROUTINE

          XTL = ZZTEMP(IROOM,LOWER)
          FLWF(IROOM,M,UPPER) = FLWF(IROOM,M,UPPER) + OPLUME(3,IOBJ)
          FLWF(IROOM,M,LOWER) = FLWF(IROOM,M,LOWER) - OPLUME(2,IOBJ)
          Q1 = CP * OPLUME(1,IOBJ) * TE
          Q2 = CP * OPLUME(2,IOBJ) * XTL
          FLWF(IROOM,Q,UPPER) = FLWF(IROOM,Q,UPPER) + QFC(UPPER,IROOM)
     +                          + Q1 + Q2
          FLWF(IROOM,Q,LOWER) = FLWF(IROOM,Q,LOWER) - Q2
          DO 70 LSP = 1, NS
            FLWF(IROOM,LSP+2,UPPER) = FLWF(IROOM,LSP+2,UPPER) +
     +          XNTMS(UPPER,LSP)
            FLWF(IROOM,LSP+2,LOWER) = FLWF(IROOM,LSP+2,LOWER) +
     +          XNTMS(LOWER,LSP)
   70     CONTINUE

!     Put the object information to arrays - xfire and froom, ...
!	Note that we are carrying parallel data structures for the fire information
!	Output uses the unsorted arrays, froom, ..., ordered by object
!	Fire physics uses the sorted arrays, sorted by compartment

          NFIRE = NFIRE + 1
          IFROOM(NFIRE) = IROOM
          XFIRE(NFIRE,1) = OBJPOS(1,IOBJ)
          XFIRE(NFIRE,2) = OBJPOS(2,IOBJ)
          XFIRE(NFIRE,3) = OBJPOS(3,IOBJ) + HF0T
          XFIRE(NFIRE,4) = OPLUME(3,IOBJ)
          XFIRE(NFIRE,5) = OPLUME(1,IOBJ)
          XFIRE(NFIRE,6) = OPLUME(2,IOBJ)
          XFIRE(NFIRE,7) = QFC(1,IROOM)
          XFIRE(NFIRE,8) = XQFR
          XFIRE(NFIRE,9) = HEATLP(IROOM) + HEATUP(IROOM)
          XFIRE(NFIRE,10) = HEATLP(IROOM)
          XFIRE(NFIRE,11) = HEATUP(IROOM)
          XFIRE(NFIRE,12) = HCOMBT
          XFIRE(NFIRE,13) = CCO2T
          XFIRE(NFIRE,14) = COCO2T
          XFIRE(NFIRE,15) = HCRATT
          XFIRE(NFIRE,16) = OCRATT
          XFIRE(NFIRE,17) = CLFRAT
          XFIRE(NFIRE,18) = CNFRAT
	    xfire(nfire,19) = objcl(iobj)
          NOBJ = NOBJ + 1
          FROOM(NOBJ) = IROOM
          FEMP(NOBJ) = OPLUME(1,IOBJ)
          FEMS(NOBJ) = OPLUME(3,IOBJ)
! note that cnfrat is not reduced by sprinklers, but oplume(1) is so femr is
! (see code in chemie and pyrols)
          femr(nobj) = oplume(1,iobj)* crfrat
          FQF(NOBJ) = HEATLP(IROOM) + HEATUP(IROOM)
          FQFC(NOBJ) = QFC(1,IROOM)
          FQLOW(NOBJ) = HEATLP(IROOM)
          FQUPR(NOBJ) = HEATUP(IROOM)
          FAREA(NOBJ) = OAREAT
		do j = 1,3
			fopos (j,nobj) = objpos(j,iobj)
		end do

        END IF
   80 CONTINUE
      RETURN
      END

      SUBROUTINE DOFIRE(IFIRE,IROOM,XEMP,XHR,XBR,XDR,HCOMBT,CCO2T,
     .   COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,crfrat,XMFIR,STMASS,
     .   XFX,XFY,XFZ,XEME,XEMS,XQPYRL,XNTMS,XQF,XQFC,XQFR,XQLP,XQUP,
     .   objectsize)

C
C     Routine:     DOFIRE

!     Description:  Do heat release from a fire for both main fire and objects. Pyrolysis 
!     and kinetics are separate operations.  Pyrolysis: TUHC, HCL, HCN, CT and TS - source 
!     is from pyrol and objint; plume to UL is done below. Combustion kinetics applies to O2, CO2, CO, OD - chemie

!     Inputs:   IFIRE   - fire number (ifire=0 is the main fire)
!               IROOM   - room containing the fire
!               XEMP    - pyrolysis rate of the fire (kg/s)
!               XHR     - height of the room (m)
!               XBR     - breadth of the room (m)
!               HCOMBT  - current heat of combustion (J/kg)
!               CCO2T   - current carbon/CO2 production ratio (kg/kg)
!               COCO2T  - current CO/CO2 production ratio (kg/kg)
!               HCRATT  - current Hydrogen/Carbon ratio in fuel (kg/kg)
!               OCRATT  - current Oxygen/Carbon ratio in fuel (kg/kg)
!               CLFRAT  - current HCl production rate (kg/kg pyrolized)
!               CNFRAT  - current HCN production rate (kg/kg pyrolized)
!               crfrat  - current trace species production rate (kg/kg pyrolized)
!               XMFIR   - production rate of a species in fire (kg/s)
!               STMASS   - mass of a species in a layer in the room (kg)
!               XFX     - position of the fire in x direction
!               XFY     - position of the fire in y direction
!               XFZ     - position of the fire in z direction
!               objectsize - characteristic object diameter for plume models
!     Outputs:  XEME    - plume entrainment rate (kg/s)
!               XEMS    - plume flow rate into the upper layer (kg/s)
!               XQPYRL  - actual heat release rate of the fire (W)
!               XNTMS   - net change in mass of a species in a layer
!               XQF     - net heat generation rate into upper layer (W)
!               XQFC    - net convection into layers (W)
!               XQFR    - net radiation from fire (W)
!               XQLP    - heat release in the lower plume (W)
!               XQUP    - heat release rate in the upper plume (W)
!
!     Revision History:
!     
!          5/08 separate pyrolisys and kinetics so that the the plume model for heat realease can be 
!               modified to a dynamic model.
!          3/07 add trace species (ns = 11)
!          4/98 fixed upper/lower layer separation (depends on where the fire is)
!          9/98 added some more initializations for when the fire is only in the upper layer
!         10/97 added initialization to XQFC, error only showed when there was upper but not lower layer burning
!          2/96 fixed definition of XXFIREU (was missing XZ term)
!          2/96 added missing initialization to XQFR
!          6/95 commented out species specific code from a 'type 1' fire calculation
!          4/95 removed reference to tmass to remove flint complaint
!          4/94 added multiple pass through fire "chemistry" for oxygen limited fires so that plume entrainment is
!               consistent with actual fire size.
!         10/93 added detection/suppression
!         11/91 modified call to firplm for entrainment type so NETMAS returns just amount for this
!               fire.  Accumulation must be done by calling routine.  Note new definition of NETMAS.
!               Standardized calling sequence.
!          5/91 eliminate general contribution from plume done in chemie, add in just ct and tuhc
!          2/91 changed limit on plume entrainment

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
   
!     Deposit pyrolysis material into the upper layer. These are the species which are not affected by combustion.
!     Any combustion related stuff is done by chemie. TUHC is a pyrolysis material whose concentration can be 
!     modified by combustion.

      xntms(upper,5) = xmfir(5)
      xntms(upper,6) = xmfir(6)
      xntms(upper,7) = xmfir(7)
      xntms(upper,10)= xmfir(10)
      xntms(upper,11) = xmfir(11)
      
!     Now do the kinetics scheme
	
C     DIVVY UP THE PLUME OUTPUT INTO RADIATION AND CONVECTIVE ENERGY.
C     CONVECTION DRIVES THE PLUME ENTRAINMENT

      CHIRAD = MAX(MIN(radconsplit(ifire),X1),X0)
      QHEATL = MAX((XQPYRL+CP*(TE-XTL)*XEMP)*(X1-CHIRAD),X0)

      IF (LFBT.EQ.FREE) THEN
!	We have eliminated unconstrained fires, if we reach this point, the input parser has failed!
	  stop 101
      ELSE

C     NOTE THAT THE COMBINATION OF FIRPLM AND CHEMIE CAN BE CALLED TWICE
C     IN A SINGLE ITERATION TO MAKE SURE THAT THE PLUME ENTRAINMENT IS
C     CONSISTENT WITH THE ACTUAL FIRE SIZE FOR OXYGEN LIMITED FIRES
C     THIS IS DONE BY "RE-PASSING" THE ACTUAL FIRE SIZE TO FIRPLM IN THE
C     SECOND PASS

	  PASS2 = .FALSE.

C     CALCULATE THE ENTRAINMENT RATE BUT CONSTRAIN THE ACTUAL AMOUNT
C     OF AIR ENTRAINED TO THAT REQUIRED TO PRODUCE STABLE STRATIFICATION

   40   CALL FIRPLM(fplume(ifire), ifire, objectsize, 
     .  QHEATL,XXFIREL,XEMP,XEMS,XEME,MIN(XFX,XbR-XFX),MIN(XFY,XdR-XFY))

C     Only do the upper layer (the fire is not in the lower layer)

        IF (XXFIREL.LE.X0) GO TO 90
        XEME = MIN(XEME,QHEATL/(MAX((XTU-XTL),X1)*CP))
        XEMS = XEMP + XEME

        CALL CHEMIE(QSPRAY(IFIRE,LOWER),XEMP,XEME,IROOM,LOWER,HCOMBT,
     .      CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,crfrat,
     .      XQPYRL,XNTFL,XMASS)

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
!       include the trace species in mass balance
        xtemp = xtemp + stmass(lower,11)
        IF(XTEMP.EQ.0.0D0)XTEMP = 1.0D0
        DO 70 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            XNET = XEME * STMASS(LOWER,LSP) / XTEMP
            XNTMS(UPPER,LSP) = XNTMS(UPPER,LSP) + XNET
            XNTMS(LOWER,LSP) = XNTMS(LOWER,LSP) - XNET
          END IF
   70   CONTINUE
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
          CALL FIRPLM(fplume(ifire), ifire, objectsize,
     .        QHEATU,HEIGHT,UPLMEP,UPLMES,UPLMEE,
     .        MIN(XFX,XbR-XFX),MIN(XFY,XdR-XFY))
          CALL CHEMIE(QSPRAY(IFIRE,UPPER),UPLMEP,UPLMEE,IROOM,UPPER,
     .        HCOMBT,CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,crfrat,
     .        XQPYRL,XNTFL,XMASS)
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

      SUBROUTINE CHEMIE(QQSPRAY,PYROL,ENTRAIN,SOURCE,LAYER,HCOMBT,CCO2T,
     + COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,crfrat,QPYROL,NETFUEL,XMASS)

!     Routine:     CHEMIE

!     Description:  Do the combustion chemistry - for plumes in both the upper and lower layers.
!     Note that the kinetics scheme is implemented here.  However, applying it to the
!     various pieces, namely the lower layer plume, the upper layer plume, and the door jet fires, is 
!     somewhat complex.

!     Care should be exercised in making changes either here or in the source interface routine.

!     Arguments: QQSPRAY  heat release rate at sprinkler activation time
!                PYROL   pyrolysis rate of the fuel (kg/s)
!                ENTRAIN plume entrainment rate (kg/s)
!                SOURCE  room number from which the mass flux comes
!                LAYER   layer mass is coming from (1=lower, 2=upper)
!                HCOMBT  current heat of combustion (J/kg)
!                CCO2T   current carbon/CO2 production ratio (kg/kg)
!                COCO2T  current CO/CO2 production ratio (kg/kg)
!                HCRATT  current Hydrogen/Carbon ratio in fuel (kg/kg)
!                OCRATT  current Oxygen/Carbon ratio in fuel (kg/kg)
!                CLFRAT  current HCl production rate (kg/kg pyrolized )
!                CNFRAT  current HCN production rate (kg/kg pyrolized)
!                crfrat  current trace species production (kg/kg pyrolized)
!                QPYROL  net heat release rate constrained by available oxygen (W)
!                NETFUEL net burning rate of fuel constrained by available oxygen (kg/s)
!                XMASS   net rate of production of species into layers in the room containing the fire (kg/s)

!     Revision History:
!          5/08 remove pyrolysis calculations 
!          3/07 added trace species tracking
!          3/04 fixed oxygen and hydrogen accounting. The way o/c was being used was as a mass ratio
!               of oxygen to fuel, but the manual specifies mass of oxygen to mass of carbon
!          4/94 moved summing of species production rates to calling routine so CHEMIE can be 
!               called multiple times. Removed EQUIVALENCE to XMASS, now an argument.
!         10/93 added sprinkler attenuation
!         12/92 modified oxygen limit to TANH for smooth cutoff independent of the limit
!         11/91 modified so NETMAS returns just amount for this fire.  Accumulation must be done by calling 
!               routine.  Note new definition of NETMAS. Standardized calling sequence.
!          8/90 remove entrainment calculation. Done by caller
!          3/90 procedure call modified to pass all changed arguments
!          2/90 more complete combustion scheme, account for H, O, HCL, and HCN
!          9/89 correct coefficients - FACTOR and NETCO2
!         11/88 smooth falloff of kinetics near oxygen limit
!         11/87 generalize for upper layer, lower layer, and plume

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"

      DIMENSION XMASS(NS)
      INTEGER SOURCE
      DOUBLE PRECISION NETFUEL, NETFUL
      DOUBLE PRECISION NETH2O, NETCO2, NETCO, NETC, NETO2, NETCL, NETCN
	double precision mdotnet, mdotnetactual
      LOGICAL FIRST
      SAVE FIRST, O2F, O2FI, O2RANGE, XX0, XXFOUR, XXHALF, XXRANGE
      DATA FIRST /.TRUE./
      IF (FIRST) THEN
        O2F = 1.31D+7
        O2FI = 1.0D0 / O2F
        O2RANGE = 0.01D0
        FIRST = .FALSE.
        XX0 = 0.0D0
	  XX1 = 1.0D0
        XXFOUR = 4.0D0
        XXHALF = 0.5D0
        XXRANGE = 8.0D0 / O2RANGE
      END IF

C     CALCULATE THE ACTUAL BURNING RATE CONSTRAINED BY AVAILABLE O2.
 
C     NOTE THE SCALING IN THE TANH FUNCTION.  TANH APPROACHES ~2 AT
C     ABOUT ~4. THE FUNCTION INSIDE THE TANH SCALES THE ORDINATE TO
C     ~O2RANGE.  THE REMAINDER OF THE FUNCTION SCALES THE ABSCISSA 
C     TO 0-1 FOR O2INDEX.
 
      O2FRAC = ZZCSPEC(SOURCE,LAYER,2)
      O2ENTR = ENTRAIN * O2FRAC
      O2INDEX = TANH(XXRANGE*(O2FRAC-LIMO2)-XXFOUR) * XXHALF + XXHALF
      O2MASS = O2ENTR * O2INDEX
      QPYROL = MAX(XX0,MIN(PYROL*HCOMBT,O2MASS*O2F))

C     THIS IS THE KINETICS SCHEME AS DRIVEN BY DIFFUSION

C     FIRST CONVERT CHLORINE AND CYANIDE PRODUCTION TO CARBON BASED 
C     RATIOS

      FACT = (1.0D0+HCRATT+OCRATT) / (1.0D0-CLFRAT-CNFRAT)
      CLCRAT = CLFRAT * FACT
      CNCRAT = CNFRAT * FACT

      FCRATT = (1.0D0+OCRATT+HCRATT+CLCRAT+CNCRAT)

	mdotnet = pyrol
	mdotnetactual = min (mdotnet, o2mass*o2f/hcombt)
      qpyrol = mdotnetactual * hcombt

!	Here we do a reduction for sprinklers if activation has occurred. Otherwise we just save the current value of the HRR

      IF (IDSET.EQ.SOURCE) THEN

C    IF IDSET=SOURCE THEN SAVE VALUE OF FIRE FOR LATER QUENCHING

        QQSPRAY = QPYROL
      ELSE IF (IDSET.EQ.0) THEN

!	A sprinkler reduces the HRR from a fire. The reduction factor is determined by the sprinkler characteristics.
!	This factor is applied to the fire based on HRR at activation.
!	However, the HRR might be reduced for other reasons, so the arithmetic Min function is used.
!	The value of QQSPRAY is the value at activation. TFACT is then a reduction based on time since activation

        ID = IQUENCH(SOURCE)
        IF (ID.NE.0) THEN
          TDRATE = XDTECT(ID,DRATE)
          TIMEF = XDTECT(ID,DTACT)
          TFACT = EXP(-(STIME-TIMEF)/TDRATE)
          IF (QQSPRAY.GT.0.0D0) QPYROL = MIN(QPYROL,TFACT*QQSPRAY)
        END IF
      END IF

      NETFUEL = mdotnetactual
      NETO2 = -QPYROL * O2FI 

C     NOW DO THE "KINETICS SCHEME"

      NETH2O = 9.0D0 * NETFUEL * HCRATT / FCRATT
      FACTOR1 = 1.0D0 + HCOMBT * O2FI - OCRATT / FCRATT
      FACTOR2 = (CLCRAT+CNCRAT+9.D0*HCRATT) / FCRATT
      NETCO2 = (FACTOR1-FACTOR2) * NETFUEL / (1.D0+COCO2T+CCO2T)

      XMASS(2) = NETO2
      XMASS(3) = NETCO2
      XMASS(4) = NETCO2 * COCO2T
      XMASS(7) = -netfuel ! this adjusts TUHC when combustion occurs
      XMASS(8) = 9.0D0 * NETFUEL * HCRATT / FCRATT
      XMASS(9) = NETCO2 * CCO2T

      RETURN
      END


      SUBROUTINE ENTRAIN(DIRS12,YSLAB,XMSLAB,NSLAB,TU,TL,CP,YLAY,CONL,
     +    CONU,PMIX,MXPRD,NPROD,YVBOT,YVTOP,UFLW3,VSAS,VASA)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     ENTRAIN
C
C     Source File: ENTRAIN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: DIRS12
C    INPUT
C    -----
C   DIRS12 - A MEASURE OF THE DIRECTION OF THE ROOM 1 TO ROOM
C                       2 FLOW IN EACH SLAB
C   YSLAB  - SLAB HEIGHTS IN ROOMS 1,2 ABOVE DATUM ELEVATION [M]
C   XMSLAB - MAGNITUDE OF THE MASS FLOW RATE IN SLABS [KG/S]
C   NSLAB  - NUMBER OF SLABS BETWEEN BOTTOM AND TOP OF VENT
C   TU     - UPPER LAYER TEMPERATURE IN EACH ROOM [K]
C   TL     - LOWER LAYER TEMPERATURE IN EACH ROOM [K]
C   YLAY   - HEIGHT OF LAYER IN EACH ROOM ABOVE DATUM ELEVATION [M]
C
C   OUTPUT
C   ------
C   UFLW3(I,1,J), I=1 OR 2, J=1 OR 2 - MASS FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE TO ENTRAINMENT
C   UFLW3(I,2,J), I=1 OR 2, J=1 OR 2 - ENTHALPY FLOW RATE TO UPPER
C            (J=2) OR LOWER (J=1) LAYER OF ROOM I ENTRAINMENT
C   UFLW3(I,2+K,J), I=1 OR 2, K=1 TO NPROD, J=1 OR 2 - PRODUCT K FLOW
C            RATE TO UPPER (J=2) OR LOWER (J=1) LAYER OF ROOM I DUE
C            ENTRAINMENT
C
C        Created:  
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "flwptrs.fi"
      INTEGER DIRS12(10)
      DIMENSION YSLAB(10), XMSLAB(10), PMIX(MXPRD)
      DIMENSION TU(2), TL(2), YLAY(2)
      DIMENSION CONL(MXPRD,2), CONU(MXPRD,2)
      DIMENSION UFLW3(2,MXPRD+2,2)
      DIMENSION VSAS(2), VASA(2)
      PARAMETER (XX0 = 0.0D0)
C
C*** INITIALIZE OUTPUTS
C
      DO 20 I = 1, 2
        DO 10 IPROD = 1, NPROD + 2
          UFLW3(I,IPROD,L) = XX0
          UFLW3(I,IPROD,U) = XX0
   10   CONTINUE
        VSAS(I) = XX0
        VASA(I) = XX0
   20 CONTINUE
C    
C
      DO 60 N = 1, NSLAB
C
C*** ELIMINATE CASES WHERE ENTRAINMENT DOES NOT OCCUR
C    I.E. A SLAB WHICH IS ADJACENT TO THE UPPER LAYER ON BOTH SIDES
C      OR A SLAB WHICH IS ADJACENT TO THE LOWER LAYER ON BOTH SIDES
C
        IF (YSLAB(N).LT.YLAY(1).OR.YSLAB(N).LT.YLAY(2)) THEN
          IF (YSLAB(N).GE.YLAY(1).OR.YSLAB(N).GE.YLAY(2)) THEN
C
C*** SLABS WITH NO FLOW CAUSE NO ENTRAINMENT
C
            IF (XMSLAB(N).NE.XX0) THEN
C
C*** DETERMINE WHAT ROOM FLOW IS COMING FROM
C
              IF (DIRS12(N).EQ.1) THEN
                IFROM = 1
                ITO = 2
              ELSE IF (DIRS12(N).EQ.0) THEN
C
C*** NO FLOW IN THIS SLAB SO WE CAN SKIP IT
C    (WE SHOULD NEVER GET HERE)
C
                GO TO 60
              ELSE IF (DIRS12(N).EQ.-1) THEN
                IFROM = 2
                ITO = 1
              END IF
C
C***  DETERMINE TEMPERATURE AND PRODUCT CONCENTRATIONS
C     OF ENTRAINED FLOW
C
              IF (YSLAB(N).LT.YLAY(ITO)) THEN
                TMIX = TL(ITO)
                DO 30 IPROD = 1, NPROD
                  PMIX(IPROD) = CONL(IPROD,ITO)
   30           CONTINUE
              ELSE
                TMIX = TU(ITO)
                DO 40 IPROD = 1, NPROD
                  PMIX(IPROD) = CONU(IPROD,ITO)
   40           CONTINUE
              END IF
C         
C*** COMPUTE THE SIZE OF THE ENTRAINED MASS FLOW
C
              IF (YSLAB(N).GE.YLAY(IFROM)) THEN
C
C*** INTO UPPER
C
                IF (TU(IFROM).GT.TL(ITO).AND.XMSLAB(N).NE.XX0) THEN
                  ZD = MAX(XX0,YLAY(ITO)-MAX(YVBOT,YLAY(IFROM)))
                  CALL ENTRFL(TU(IFROM),TL(ITO),XMSLAB(N),ZD,
     +                UFLW3(ITO,M,U))
                  UFLW3(ITO,M,L) = -UFLW3(ITO,M,U)
                  VSAS(ITO) = UFLW3(ITO,M,U)
                END IF
              ELSE
C
C*** INTO LOWER
C
                IF (TL(IFROM).LT.TU(ITO).AND.XMSLAB(N).NE.XX0) THEN
C               ZD = MAX(XX0,YLAY(IFROM)-MAX(YVBOT,YLAY(ITO)))

C*** need to re-work distance zd for both into upper and into
C         upper case.  the above doesn't work for all cases

                  ZD = MIN(YVTOP,YLAY(IFROM)) - MAX(YLAY(ITO),YVBOT)
                  CALL ENTRFL(TU(ITO),TL(IFROM),XMSLAB(N),ZD,
     +                UFLW3(ITO,M,L))
C*** The following factor (0.25 as of 10/1/93) now multiplies the lower layer
C*** entrainment to try to approximate the reduced Kelvin-Helmholz type mixing.
C*** This observation arises from the problems encountered in the test
C*** case temper1.dat.  This needs to be researched carefully!

                  UFLW3(ITO,M,L) = UFLW3(ITO,M,L) * 0.25D0
                  VASA(ITO) = UFLW3(ITO,M,L)
                  UFLW3(ITO,M,U) = -UFLW3(ITO,M,L)
                END IF
              END IF
C
C*** COMPUTE ENTHALPY AND PRODUCT FLOW RATES OF ENTRAINED FLOW
C    FROM THE MASS FLOW RATE
C
              UFLW3(ITO,Q,L) = CP * UFLW3(ITO,M,L) * TMIX
              UFLW3(ITO,Q,U) = CP * UFLW3(ITO,M,U) * TMIX
              DO 50 IPROD = 3, 2 + NPROD
                 UFLW3(ITO,IPROD,L) = UFLW3(ITO,M,L) * PMIX(IPROD-2)
                 UFLW3(ITO,IPROD,U) = UFLW3(ITO,M,U) * PMIX(IPROD-2)
   50         CONTINUE
            END IF
          END IF
        END IF
   60 CONTINUE
      RETURN
      END

      SUBROUTINE ENTRFL(TU,TL,FMD,Z,FMZ)
C
C     For the reference for this correlation, see the comments
C     in the routine "firplm."  The offset for the formulation of
C     an equivalent door jet is provided by requiring the plume
C     be long enough to be the appropriate plume for the fire of size
C     QJ.  Note that McCaffrey's units are kilojoules.  Also we assume
C     that the plume is round as in McCaffrey's plume.  This should
C     be modified to account for the flat plume verus the round
C     plume in the theory.
C
C     update history
C
C     July 24, 1990 modified coefiscients so that McCaffrey correlation is
C                   continuous.  the coeff's are calculated the first time
C                   this routine is called.  gpf
C
      include "precis.fi"
      include "cfast.fi"
      LOGICAL FIRSTC
      SAVE FIRSTC, A1, A2, A3, E1, E2, E3, F1, F2
      DATA FIRSTC /.TRUE./
C
C*** DEFINE ASSIGNMENT STATEMENT SUBROUTINES TO COMPUTE THREE PARTS
C    OF CORRELATION
C
      FM1(ZQ) = ZQ ** .566D0
      FM2(ZQ) = ZQ ** .909D0
      FM3(ZQ) = ZQ ** 1.895D0
C
C*** FIRST TIME IN FIRPLM CALCULATE COEFF'S 
C    TO INSURE THAT MCCAFFREY CORRELATION IS CONTINUOUS.
C    THAT IS, FOR A1 = .011, COMPUTE A2, A3 SUCH THAT
C  
C     A1*ZQ**.566 = A2*ZQ**.909  FOR ZQ = .08
C     A2*ZQ**.909 = A3*ZQ**1.895 FOR ZQ = .2
C
      IF (FIRSTC) THEN
C
C*** RESET FLAG SO THIS CODE DOESN'T GET EXECUTED NEXT TIME IN 
C    THIS ROUTINE
C
        FIRSTC = .FALSE.
C
C*** BREAKPOINTS FOR "FORWARD" CORRELATION
C
        T1 = .08D0
        T2 = .20D0
C
C*** COEF'S FOR "FORWARD" CORRELATION
C
        A1 = .011D0
        A2 = A1 * FM1(T1) / FM2(T1)
        A3 = A2 * FM2(T2) / FM3(T2)
C
C*** EXPONENTS FOR "INVERSE" CORRELATION
C
        E1 = 1.0D0 / .566D0
        E2 = 1.0D0 / .909D0
        E3 = 1.0D0 / 1.895D0
C
C*** BREAKPOINTS FOR "INVERSE" CORRELATION
C
        F1 = A1 * FM1(T1)
        F2 = A2 * FM2(T2)
      END IF
C
      XQJ = CP * (TU-TL) * 0.001D0
      QJ = XQJ * FMD
      FMDQJ = 1.D0 / XQJ
      IF (FMDQJ.GE.0.0D0.AND.FMDQJ.LE.F1) THEN
        Z0DQ = (FMDQJ/A1) ** E1
      ELSE IF (FMDQJ.GT.F1.AND.FMDQJ.LE.F2) THEN
        Z0DQ = (FMDQJ/A2) ** E2
      ELSE
        Z0DQ = (FMDQJ/A3) ** E3
      END IF
C
      ZDQ = Z / QJ ** 0.4D0 + Z0DQ
      IF (ZDQ.GT.0.2D0) THEN
        FMZ = A3 * FM3(ZDQ) * QJ
      ELSE IF (ZDQ.GT.0.08D0) THEN
        FMZ = A2 * FM2(ZDQ) * QJ
      ELSE
        FMZ = A1 * FM1(ZDQ) * QJ
      END IF
C
      XX0 = 0.0D0
      FMZ = MAX(XX0,FMZ-FMD)
      RETURN
      END

      SUBROUTINE FIRPLM(plumetype, objectnumber, objectsize, 
     .                  QJL,ZZ,XEMP,XEMS,XEME,XFX,XFY)

!     Physical interface between DOFIRE and the plume models
!     July 30, 2008 supports two plume models
!     1) McCaffrey and
!     2) Heskestad's modified version of Zukoski's model

      include "precis.fi"
      include "cfast.fi"
      integer plumetype, objectnumber
      
      select case (plumetype)
      case (1) !    McCaffrey
        call mccaffrey(QJL,ZZ,XEMP,XEMS,XEME,XFX,XFY,objectsize)
        RETURN        
      case (2) !    Heskestad
        call heskestad (qjl, zz, xemp, xems, xeme, xfx, xfy, objectsize)
        RETURN        
      end select
      stop 'bad case in firplm'
      END

      SUBROUTINE mccaffrey(QJL,ZZ,XEMP,XEMS,XEME,XFX,XFY,od)

!     Function:  Calculates plume entrainment for a fire from 
!                McCaffrey's correlation
!     Inputs:    QJL    fire size (W)
!                ZZ      plume height (m)
!                XEMP  mass loss rate of the fire (kg/s)
!                XFX   position of the fire in x direction (m)
!                XFY   position of the fire in y direction (m)
!                od is the object diameter
!     Outputs:   XEMS  total mass transfer rate at height z (kg/s)
!                XEME  net entrainment rate at height z (kg/s)
!     Algorithm: "Momentum Implications for Buoyant Diffusion Flames", Combustion and Flame 52, 149 (1983)

      include "precis.fi"
      LOGICAL FIRST
      SAVE FIRST, A1, A2, A3, T1, T2
      DATA FIRST /.TRUE./
C
C*** DEFINE ASSIGNMENT STATEMENT SUBROUTINES TO COMPUTE THREE PARTS OF CORRELATION
C
      FM1(ZQ) = ZQ ** .566D0
      FM2(ZQ) = ZQ ** .909D0
      FM3(ZQ) = ZQ ** 1.895D0
C
C*** FIRST TIME IN FIRPLM CALCULATE COEFF'S TO INSURE THAT MCCAFFREY 
C    CORRELATION IS CONTINUOUS.  
C    THAT IS, FOR A1 = .011, COMPUTE A2, A3 SUCH THAT
C  
C     A1*ZQ**.566 = A2*ZQ**.909  FOR ZQ = .08
C     A2*ZQ**.909 = A3*ZQ**1.895 FOR ZQ = .2
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        T1 = .08D0
        T2 = .20D0
        A1 = .011D0
        A2 = A1 * FM1(T1) / FM2(T1)
        A3 = A2 * FM2(T2) / FM3(T2)
      END IF
      X0 = 0.0D0
C
C     DETERMINE WHICH ENTRAINMENT TO USE BY FIRE POSITION.  IF WE'RE ON
C     THE WALL OR IN THE CORNER, ENTRAINMENT IS MODIFIED.
C
      XF = 1.0D0
      IF (XFX.EQ.X0.OR.XFY.EQ.X0) XF = 2.0D0
      IF (XFX.EQ.X0.AND.XFY.EQ.X0) XF = 4.0D0
      QJ = 0.001D0 * QJL
      IF (ZZ.GT.0.D0.AND.QJ.GT.0.0D0) THEN
        ZDQ = ZZ / (XF*QJ) ** 0.4D0
        IF (ZDQ.GT.T2) THEN
          XEMS = A3 * FM3(ZDQ) * QJ
        ELSE IF (ZDQ.GT.T1) THEN
          XEMS = A2 * FM2(ZDQ) * QJ
        ELSE
          XEMS = A1 * FM1(ZDQ) * QJ
        END IF
        XEMS = MAX(XEMP,XEMS/XF)
        XEME = MAX(XEMS-XEMP,X0)
      ELSE
        XEMS = XEMP
        XEME = 0.0D0
      END IF
      RETURN
      END

      subroutine heskestad (q, z, emp, ems, eme, x, y, od)

!     Function:  Calculates plume entrainment for a fire from Heskestad's variant of Zukoski's correlation
!     Inputs:    Q    fire size (W)
!                Z      plume height (m)
!                EMP  mass loss rate of the fire (kg/s)
!                XFX   position of the fire in x direction (m)
!                XFY   position of the fire in y direction (m)
!                od is the characteristic size of the object (diameter)
!     Outputs:   EMS  total mass transfer rate at height z (kg/s)
!                EME  net entrainment rate at height z (kg/s)
    
      include "precis.fi"
    
      double precision q, qj, z, z0, emp, eme, ems, x, y, od, deltaz
    
      qj = 0.001d0 * q
      z0 = -1.02d0 * od + 0.083d0 * qj**0.4
      deltaz = max(0.0001d0, z-z0)
      eme = 0.071 * qj**0.333 * deltaz**1.67 * (1+0.026d0*qj**0.67
     .      * deltaz**(-1.67))
      ems = emp + eme    

      end subroutine heskestad

      SUBROUTINE PYROLS(TIME,IROOM,BFIRET,AFIRET,HFIRET,QFIRET,HCOMBT,
     .      CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,crfrat,ZMFIRE)

!     Routine:  PYROLS

!     Description: Calculate pyrolysis rate of the fuel for the specified main fire
!     Inputs:   TIME    - current time (s)
!               IROOM   - room number containing fire
!     Outputs:  BFIRET  - current pyrolysis rate (kg/s)
!               AFIRET  - current area of the fire (m^2)
!               HFIRET  - current height of the fire (m)
!               QFIRET  - current heat release rate of the fire (W)
!               HCRATT  - current hydrogen/carbon ratio in fuel (kg/kg)
!               CCO2T   - current carbon/CO2 production ratio (kg/kg)
!               COCO2T  - current CO/CO2 production ratio (kg/kg)
!               OCRATT  - current Oxygen/Carbon ratio in fuel (kg/kg)
!               HCOMBT  - current heat of combustion (J/kg)
!               CLFRAT  - current HCl production rate (kg/kg burned)
!               CNFRAT  - current HCN production rate (kg/kg burned)
!               crFRAT  - current trace gas production rate (kg/kg burned)
!               ZMFIRE  - current species production rates (kg/s)

!     Revision History
!         11/91 modified to use new interpolation subroutine
!          3/90 changed the procedure call to pass variables that are changed
!         10/93 added detection/suppression
!          5/08 added initialization for trace species, separated kinetics and chemistry


      include "precis.fi"
      include "cfast.fi"

      DIMENSION ZMFIRE(*)
 
      ID = IQUENCH(IROOM)
      XXTIME = TIME
      IF(ID.EQ.0)THEN
C
C*** IF A SPRINKLER IS NOT ACTIVE THEN INTERPOLATE AT CURRENT TIME
C
        IFACT = 0
       ELSE
C
C*** IF A SPRINKLER IS ACTIVE THEN INTERPOLATE AT CURRENT TIME
C    AND WHEN SPRINKLER FIRST ACTIVATED.  MAKE SURE THAT SPECIFIED
C    HEAT RELEASE RATE IS THE SMALLER OF RATE AT CURRENT TIME
C    AND RATE AT SPRINKLER ACTIVATION TIME * EXP( ...) 
C
        TDRATE = XDTECT(ID,DRATE)
        XXTIMEF = XDTECT(ID,DTACT)
        CALL INTERP(TFIRED,QFIRED,LFMAX,XXTIME,1,QT)
        CALL INTERP(TFIRED,QFIRED,LFMAX,XXTIMEF,1,QTF)
        IFACT = 1
        TFACT = EXP(-(XXTIME-XXTIMEF)/TDRATE)
        IF(QT.LT.TFACT*QTF)THEN
C
C*** CURRENT TIME HEAT RELEASE RATE IS SMALLER THAN SPRINKLERD VALUE
C    SO USE CURRENT TIME AND RESET IFACT TO 0 SO RATES ARE NOT 
C    DECREASED
C
           IFACT = 0
          ELSE
           XXTIME = XXTIMEF
        ENDIF
      ENDIF
C
C     INTERPOLATE THE FIRE OUTPUTS
C
      CALL INTERP(TFIRED,BFIRED,LFMAX,XXTIME,1,BFIRET)
      CALL INTERP(TFIRED,QFIRED,LFMAX,XXTIME,1,QFIRET)
      CALL INTERP(TFIRED,HCLF,LFMAX,XXTIME,1,CLFRAT)
      CALL INTERP(TFIRED,HCNF,LFMAX,XXTIME,1,CNFRAT)
      call interp(tfired,hcrf,lfmax,xxtime,1,crfrat)
 
C*** ATTENUATE MASS AND ENERGY RELEASE RATES IF THERE IS AN ACTIVE SPRINKLER
C    IN THIS ROOM
 
      IF(ID.NE.0.AND.IFACT.EQ.1)THEN
        BFIRET = BFIRET*TFACT
        QFIRET = QFIRET*TFACT
      ENDIF
      CALL INTERP(TFIRED,HOCBMB,LFMAX,XXTIME,1,HCOMBT)
      CALL INTERP(TFIRED,AFIRED,LFMAX,XXTIME,1,AFIRET)
      CALL INTERP(TFIRED,HFIRED,LFMAX,XXTIME,1,HFIRET)
      CALL INTERP(TFIRED,HCRATIO,LFMAX,XXTIME,1,HCRATT)
      XLIM = 0.3333D0
      HCRATT = MIN(XLIM,HCRATT)
      CALL INTERP(TFIRED,CCO2,LFMAX,XXTIME,1,CCO2T)
      CALL INTERP(TFIRED,COCO2,LFMAX,XXTIME,1,COCO2T)
      CALL INTERP(TFIRED,OCRATI,LFMAX,XXTIME,1,OCRATT)
      DO 10 IPROD = 1, NS
        IF (ACTIVS(IPROD)) THEN
          CALL INTERP(TFIRED,MPRODR(1,IPROD),LFMAX,XXTIME,1,XPROD)
          ZMFIRE(IPROD) = XPROD * BFIRET
        END IF
   10 CONTINUE
      RETURN
      END

      subroutine integrate mass (time, deltt)
      
!   Routine to integrate the pyrolosate of objects

!     deltt is the time step
!     we also integrate the trace species release and total for all fires

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"    

      integer i, j
      double precision xx0,xx1,filter,qcifraction,time,deltt
      data xx0/0.0d0/, xx1/1.0d0/
     
      do i = 0, numobjl
        objmaspy(i) = objmaspy(i) + femp(i)*deltt
        radio(i) = radio(i) + femr(i)*deltt
      end do
      
! sum the radiological release from all of the fires

      tradio = xx0
      do i = 0, numobjl
        tradio = tradio + radio(i)
      end do

! sum the hvac flow

! tracet is the trace species which gets through the vent, traces is the mass stopped. Has to be calculated here since
! there is no equivalent to 1-... 

      do irm = 1, n
        DO II = 1, NEXT
            I = HVNODE(1,II)
            J = HVNODE(2,II)
            ISYS = IZHVSYS(J)       
	      filter = (xx1-qcifraction(qcvf,isys,time)) 
	      if (irm.eq.i) then
	         hveflot(upper,ii) = hveflot(upper,ii)+hveflo(upper,ii)*deltt
	         hveflot(lower,ii) = hveflot(lower,ii)+hveflo(lower,ii)*deltt 
	         tracet(upper,ii)  = tracet(upper,ii) + 
     .           hveflo(upper,ii)*hvexcn(ii,11,upper)*filter*deltt
	         tracet(lower,ii)  = tracet(lower,ii) + 
     .           hveflo(lower,ii)*hvexcn(ii,11,lower)*filter*deltt
	         traces(upper,ii)  = traces(upper,ii) + 
     .           hveflo(upper,ii)*hvexcn(ii,11,upper)*(xx1-filter)*deltt
	         traces(lower,ii)  = traces(lower,ii) + 
     .           hveflo(lower,ii)*hvexcn(ii,11,lower)*(xx1-filter)*deltt
	      endif 
        end do
      end do
      
      return
      end
  
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

      SUBROUTINE DJFIRE(ITO,TJET,XXNETFL,SAS,HCOMBT,QPYROL,XNTMS,
     .                  DJFLOWFLG)
C*RB
C     Routine:  DJFIRE
C
C     Function: Calculate heat and combustion chemistry for a door jet
C               fire.
C
C     Inputs:   ITO     Room number door jet is flowing into
C               TJET    Temperature of the door jet gas
C               XXNETFL Net fuel available to be burned
C               SAS     Mass flow rate of entrained air in door jet
C               HCOMBT  Heat of combustion of unburned fuel
C     Outputs:  QPYROL  Total heat released by door jet fire
C               XNTMS   Net change in mass of species in door jet
C     Commons:
C        USED:  Activs   Tgignt   Zzgspec  Zzmass
C
C     Revision History:
C     gpf 5/11/98
C                  Implement fast startup option.  Execute this routine only if this 
C                  modeling feature is being used (rather than zeroing out 
C                  the flow vector.)
C     RDP   4/8/94    moved summing of species production rates from CHEMIE
C                     to calling routine.
C     gpf 10/14/93   In DOFIRE, the routine CHEMIE requires info about
C                    fire size for sprinkler option.  Though a door jet
C                    fire is not attenuated by sprinklers, the call
C                    to CHEMIE had to made consistent with call in
C                    dofire.
C*RE
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
C
      DIMENSION XNTMS(2,NS), XMASS(NS)
      LOGICAL DJFLOWFLG
C
      X0 = 0.0D0
      QPYROL = X0
      DJFLOWFLG = .FALSE.
C
C     WE ONLY WNAT TO DO THE DOOR JET CALCULATION IF THERE IS FUEL,
C     OXYGEN, AND SUFFICIENT TEMPERATURE IN THE DOOR JET
C
      IF (XXNETFL.GT.X0.AND.SAS.GT.X0.AND.TJET.GE.TGIGNT) THEN
C
C     DO COMBUSTION CHEMISTRY ASSUMING COMPLETE COMVERSION TO CO2 & H2O.
C     ALTHOUGH THE REAL CHEMISTRY IS MORE COMPLEX, FOR NOW WE DON'T KNOW
C     HOW TO HANDLE IT.
C
        DUMMY = -1.0D0
        DJFLOWFLG = .TRUE.
        DO 10 i = 1, NS
          XMASS(i) = X0
   10   CONTINUE
        CALL CHEMIE(DUMMY,XXNETFL,SAS,ITO,LOWER,HCOMBT,X0,X0,X0,X0,X0,
     +      X0,x0,QPYROL,XXNETFUE,XMASS)
        DO 20 I = 1, NS
          XNTMS(UPPER,I) = XMASS(I)
          XNTMS(LOWER,I) = X0
   20   CONTINUE
      END IF
      RETURN
      END

      subroutine flamhgt (qdot, area, fheight)

!     Description:  Calculates flame height for a given fire size and area
!
!     Arguments: qdot    Fire Size (W)
!                area    Area of the base of the fire (m^2)
!                fheight Calculated flame height (m)
!
!     Source: SFPE handbook, Section 2, Chapter 1

      implicit none
      character str*10
      real*8, parameter :: zero = 0.0d0, four = 4.0d0, pi = 3.14159d0
      real*8 qdot, area
      real*8 fheight
      real*8 d
      if (area.le.0d0) THEN
        d = 0.3d0
      else
        d = SQRT(four*area/pi)
      end if
	fheight = -1.02*d + 0.235*(qdot/1.0d3)**0.4d0
      fheight = max (zero, fheight)
      return
      end subroutine flamhgt
      
      subroutine PlumeTemp (qdot, xrad, dfire, tu, tl, zfire, zlayer,
     *                      zin, tplume)
    
! Calculates plume centerline temperature at a specified height above
! the fire
!
! Using Heskestad's correlation for the lower layer and Evan's modification
! when position is in the upper layer

! Inputs:
!
! qdot    total heat release rate of the fire (W)
! xrad    fraction of fire HRR released as radiation
! dfire   fire diamater (m)
! tu      upper layer gas temperature (K)
! tl      lower layer gas temperature (K)
! zfire   height of the base of the fire (m)
! zlayer  height of the hot/cold gas layer interface (m)
! z       position to calculate plume centerline temperature (m)

! Output:
!
! tplume  plume centerline temperature

      implicit none
      real*8 qdot, xrad, dfire, tu, tl, zfire, zlayer, zin
      real*8 tplume
      real*8, parameter :: g = 9.8d0, C_T = 9.115d0, Beta = 0.955d0,
     *                     piov4 = (3.14159d0/4.0d0)
      real*8 cp,  rhoamb, z0, qdot_c, z_i1, q_i1star, xi, fheight, z,
     *       q_i2star, z_i2, z_eff, dt

!     z0 = virtual origin, qdot_c = convective HRR
      z0 = -1.02d0*dfire + 0.083d0*(qdot/1000.d0)**0.4d0 + zfire
      qdot_c = qdot*(1.0d0 - xrad)/1000.d0

!     plume temperature correlation is only valid above the mean flame height      
      call flamhgt (qdot,piov4*dfire**2,fheight)
      if (fheight + zfire.lt.zin) then 
        z = zin
      else
        z = fheight + zfire
      end if
!     for the algorithm to work, there has to be a fire, two layers, and a target point about the fire      
      if (qdot.gt.0.0d0.and.tu.ge.tl.and.z.gt.zfire) then

!       fire and target are both in the lower layer
        if (z.le.zlayer) then
            rhoamb = 352.981915d0/tl
            cp = 3.019d-7*tl**2 - 1.217d-4*tl + 1.014d0
            dt = 9.1d0*(tl/(g*cp**2*rhoamb**2))**(1.d0/3.d0)*
     *      qdot_c**(2.d0/3.d0)/(z-z0)**(5.d0/3.d0)
            tplume = tl + dt
     
!       fire and target are both in the upper layer
        else if (zfire.gt.zlayer) then
            rhoamb = 352.981915d0/tu
            cp = 3.019d-7*tu**2 - 1.217d-4*tu + 1.014d0
            dt = 9.1d0*(tu/(g*cp**2*rhoamb**2))**(1.d0/3.d0)*
     *      qdot_c**(2.d0/3.d0)/(z-z0)**(5.d0/3.d0)
            tplume = tu + dt
     
!       fire is in lower layer and target is in upper layer
        else
            rhoamb = 352.981915d0/tl
            cp = 3.019d-7*tl**2 - 1.217d-4*tl + 1.014d0
            z_i1 = zlayer - zfire
            q_i1star = qdot_c/(rhoamb*cp*tl*sqrt(g)*z_i1**(5.d0/2.d0))
            xi = tu/tl
            q_i2star = ((1.d0+C_T*q_i1star**(2.d0/3.d0))/
     *      (xi*C_T)-1.d0/C_T)**(3.d0/2.d0)
            z_i2 = (xi*q_i1star*C_T/(q_i2star**(1.d0/3.d0)*((xi-1.d0)*
     *      (Beta**2+1.d0)+xi*C_T*q_i2star**(2./3.))))**(2.d0/5.d0)*z_i1
            z_eff = (z-zfire)-z_i1+z_i2
            dt = 9.28d0*tu*q_i2star**(2.d0/3.d0)* 
     *      (z_i2/z_eff)**(5.d0/3.d0)
            tplume = tu + dt
        end if
      else
        if (z.le.zlayer) then
            tplume = tl
        else
            tplume = tu
        end if
      end if  
      return
      end subroutine PlumeTemp

      SUBROUTINE OBJINT (OBJN, TIME, IROOM, OMASST, OAREAT, 
     . OHIGHT, OQDOTT, OBJHCT, CCO2T, COCO2T, HCRATT, ZMFIRE, OCRATT,
     . CLFRAT, CNFRAT,crfrat,UPDATE)

!     Routine:     OBJINT

!     Description:  Returns yields for object fires interpolated from user input  

!     Arguments: OBJN   The object pointer number, 
!                TIME   Current simulation time (s)
!                IROOM  Room object is in
!                FLUX   The normal radiative flux to the object
!                SURFT  Surfice temp of object
!                OMASST Pyrolysis rate of object (returned)
!                OAREAT Area of pyrolysis of object (returned)
!                OHIGHT Height of fire (returned)
!                OQDOTT Heat release rate of object
!                OBJHCT Object heat of combustion
!                CCO2T  Carbon to CO2 ratio
!                COCO2T CO to CO2 ration
!                HCRATT Hydrogen to Carbon ratio of fuel
!                ZMFIRE 
!                OCRATT Oxygen to Carbon ration of fuel
!                CLFRAT HCl production rate
!                CNFRAT HCN production rate
!                crFRAT trace gase production rate
!                UPDATE Update varible
!
!     Revision History:
!          8/90 calculate the pyrolysis rate, ... for other objects at this time, this 
!               code is identical to PYROLS, but the intent is to be able to change it.
!         10/93 added detection/suppression by GPF
!          8/95 added code to handle type three fires. Also fixed objects so they can properly
!               ignite by conditions and added header
!          5/08 added initialization for trace species, separated kinetics and chemistry

C     PYROLYSIS RATE OF THE FUEL - HCRATT IS IN COMMON (PARAMS.INC) SINCE
C     IT IS USED IN SEVERAL PLACES

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fireptrs.fi"
      include "fltarget.fi"
      include "objects1.fi"
      include "objects2.fi"

      DIMENSION ZMFIRE(NS)
      INTEGER OBJN, UPDATE

      ONETHRD = 0.3333D0
      IF (.NOT.OBJON(OBJN).OR.OBJSET(OBJN).GT.0) THEN
        XX0 = 0.0D0
        OMASST = XX0
        OAREAT = XX0
        OHIGHT = XX0
        OQDOTT = XX0
        HCRATT = XX0
        CCO2T = XX0
        COCO2T = XX0
        OCRATT = XX0
        OBJHCT = 5.0D7
        DO 14 J = 1, NS
   14     ZMFIRE(J) = XX0
        CLFRAT = XX0
        CNFRAT = XX0
        crfrat = xx0
        IF (FSMTYPE.GT.0) THEN
          DYPDT = XX0
          DXPDT = XX0
          DYBDT = XX0
          DXBDT = XX0
          DQDT  = XX0
        END IF
        RETURN
      ENDIF

      LOBJLFM = OBJLFM(OBJN)
      XXTIME = TIME - OBJCRI(1,OBJN)
      
	ID = IQUENCH(IROOM)
      
	IF(ID.EQ.0)THEN

!	IF A SPRINKLER IS NOT ACTIVE THEN INTERPOLATE AT CURRENT TIME

	    IFACT = 0
	ELSE
C
C*** IF A SPRINKLER IS ACTIVE THEN INTERPOLATE AT CURRENT TIME
C    AND WHEN SPRINKLER FIRST ACTIVATED.  MAKE SURE THAT SPECIFIED
C    HEAT RELEASE RATE IS THE SMALLER OF RATE AT CURRENT TIME
C    AND RATE AT SPRINKLER ACTIVATION TIME * EXP( ...) 
C
	    TDRATE = XDTECT(ID,DRATE)
	    XXTIMEF = XDTECT(ID,DTACT) - OBJCRI(1,OBJN)
	    CALL INTERP(OTIME(1,OBJN),OQDOT(1,OBJN),LOBJLFM,XXTIME,1,
     .                QT)
	    CALL INTERP(OTIME(1,OBJN),OQDOT(1,OBJN),LOBJLFM,XXTIMEF,1,
     .                QTF)
	    IFACT = 1
	    TFACT = EXP(-(XXTIME-XXTIMEF)/TDRATE)
	    IF(QT.LT.TFACT*QTF)THEN
C
C*** CURRENT TIME HEAT RELEASE RATE IS SMALLER THAN SPRINKLERD VALUE
C    SO USE CURRENT TIME AND RESET IFACT TO 0 SO RATES ARE NOT 
C    DECREASED
C
	  		IFACT = 0
	    ELSE
	  		XXTIME = XXTIMEF
	    ENDIF
	ENDIF
      
	CALL INTERP(OTIME(1,OBJN),OMASS(1,OBJN),LOBJLFM,XXTIME,1,
     .            OMASST)
	CALL INTERP(OTIME(1,OBJN),OQDOT(1,OBJN),LOBJLFM,XXTIME,1,
     .  			 OQDOTT)
	CALL INTERP(OTIME(1,OBJN),OMPRODR(1,6,OBJN),LOBJLFM,XXTIME,1,
     +  			 CLFRAT)
	CALL INTERP(OTIME(1,OBJN),OMPRODR(1,5,OBJN),LOBJLFM,XXTIME,1,
     +					 CNFRAT)
!     crfrat is no longer used -  probably can eliminate the following code
      call interp(otime(1,objn),omprodr(1,11,objn),lobjlfm,xxtime,1,
     +                   crfrat)

!     ATTENUATE MASS AND ENERGY RELEASE RATES IF THERE IS AN ACTIVE SPRINKLER IN THIS ROOM

	IF(ID.NE.0.AND.IFACT.EQ.1)THEN
	    OMASST = OMASST*TFACT
	    OQDOTT = OQDOTT*TFACT
	ENDIF
	CALL INTERP(OTIME(1,OBJN),OAREA(1,OBJN),LOBJLFM,XXTIME,1,
     .        OAREAT)
	CALL INTERP(OTIME(1,OBJN),OHIGH(1,OBJN),LOBJLFM,XXTIME,1,
     .        OHIGHT)

	CALL INTERP(OTIME(1,OBJN),OHCR(1,OBJN),LOBJLFM,XXTIME,1,HCRATT)
	HCRATT = MIN (ONETHRD, HCRATT)
	CALL INTERP(OTIME(1,OBJN),OOD(1,OBJN),LOBJLFM,XXTIME,1,CCO2T)
	CALL INTERP(OTIME(1,OBJN),OCO(1,OBJN),LOBJLFM,XXTIME,1,COCO2T)
	CALL INTERP(OTIME(1,OBJN),OOC(1,OBJN),LOBJLFM,XXTIME,1,OCRATT)
	CALL INTERP(OTIME(1,OBJN),OBJHC(1,OBJN),LOBJLFM,XXTIME,1,OBJHCT)
	DO 15 J = 1, NS
	    CALL INTERP(OTIME(1,OBJN),OMPRODR(1,J,OBJN),LOBJLFM,
     .  		XXTIME,1,XPROD)
	    ZMFIRE(J) = XPROD * OMASST
   15	CONTINUE

      RETURN
      END

      SUBROUTINE TOXIC(DELTT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TOXIC
C
C     Source File: TOXIC.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine is used tto calculate species concentrations
C                   (ppm), mass density (kg/m^3), opacity (1/m), 
C                   CT (g-min/m^3), heat flux to target on floor (W)
C
C     Arguments: DELTT  length of the latest time step (s)
C
C     Revision History:
C        5/26/1987 by WWJ, change ct calculation to g-m/m^3
C        11/20/1987 by WWJ, use sum of species to determine total mass used
C                           to calculate mass and volume fractions
C        3/30/1989 by WWJ, fix "ontarget" s*(t-ta)**4->s*(t**4-ta**4)
C        9/12/1989 by WWJ, set ontarget to 0 if < 1
C        11/20/92 by RDP, eliminated MINMAS as the check for minimum molar
C                 count used to calculate molar fraction.  Changed to
C                 1/Avagadro' number so you can't have less than 1 molecule
C                 of gas in a layer.
C	    02/15/02 by WWJ The smoke conversion factor has been changed from 3500 to 3817 
C                  to reflect the new value as reported by Mulholland in Fire and Materials, 24, 227(2000)

C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
C
      DIMENSION AWEIGH(NS), AIR(2), V(2)
      LOGICAL PPMCAL(NS)
C
C     AWEIGH'S ARE MOLAR WEIGHTS OF THE SPECIES, AVAGAD IS THE RECIPROCAL
C     OF AVAGADRO'S NUMBER (SO YOU CAN'T HAVE LESS THAN AN ATOM OF A SPECIES
C
      DATA AWEIGH, AWEIGH7 /28.D0, 32.D0, 44.D0, 28.D0, 27.D0, 37.D0, 
     +    12.D0, 18.D0, 12.D0, 0.D0, 0.0d0, 12.D0/
      DATA AVAGAD /1.66D-24/
      DATA PPMCAL /3 * .FALSE., 3 * .TRUE., 5 * .FALSE./
      AWEIGH(7) = AWEIGH7 * (1.0D0+HCRATT)

      DO 90 I = 1, NM1
C
        V(UPPER) = ZZVOL(I,UPPER)
        V(LOWER) = ZZVOL(I,LOWER)
        DO 20 K = UPPER, LOWER
          AIR(K) = 0.0D0
          DO 10 LSP = 1, 9
            AIR(K) = AIR(K) + ZZGSPEC(I,K,LSP) / AWEIGH(LSP)
   10     CONTINUE
          AIR(K) = MAX(AVAGAD,AIR(K))
   20   CONTINUE
C
C     CALCLUATE THE MASS DENSITY IN KG/M^3
C
        DO 40 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
            DO 30 K = UPPER, LOWER
              PPMDV(K,I,LSP) = ZZGSPEC(I,K,LSP) / V(K)
   30       CONTINUE
          END IF
   40   CONTINUE
C
C     NOW CALCULATE THE MOLAR DENSITY
C
        DO 60 LSP = 1, 8
          IF (ACTIVS(LSP)) THEN
            DO 50 K = UPPER, LOWER
              IF (PPMCAL(LSP)) THEN
                TOXICT(I,K,LSP) = 1.D+6 * ZZGSPEC(I,K,LSP) / (AIR(K)*
     +              AWEIGH(LSP))
              ELSE
                TOXICT(I,K,LSP) = 100.D0 * ZZGSPEC(I,K,LSP) / (AIR(K)*
     +              AWEIGH(LSP))
              END IF
   50       CONTINUE
          END IF
   60   CONTINUE
C
C     OPACITY IS CALCULATED FROM SEDER'S WORK
C	Note: this value was change 2/15/2 from 3500 to 3817 to reflect the new value as reported yb
C     Mulholland in Fire and Materials, 24, 2227(2000)
C
        LSP = 9
        IF (ACTIVS(LSP)) THEN
          DO 70 K = UPPER, LOWER
            TOXICT(I,K,LSP) = PPMDV(K,I,LSP) * 3817.0D0
   70     CONTINUE
        END IF

!     CT is the integration of the total "junk" being transported

        LSP = 10
        IF (ACTIVS(LSP)) THEN
          DO 80 K = UPPER, LOWER
            TOXICT(I,K,LSP) = TOXICT(I,K,LSP) + PPMDV(K,I,LSP) * 
     +          1000.0D0 * DELTT / 60.0D0
   80     CONTINUE
        END IF

!     TS (trace species) is the filtered concentration - this is the total mass. 
!     It is converted to fraction of the total generated by all fires.
!     This step being correct depends on the INTEGRATEMASS routine

        LSP = 11
        IF (ACTIVS(LSP)) THEN
          DO 81 K = UPPER, LOWER
            TOXICT(I,K,LSP) = zzgspec(i,k,lsp) ! / (tradio+1.0d-10)
   81     CONTINUE
        END IF

   90 continue

!     ONTARGET IS THE RADIATION RECEIVED ON A TARGET ON THE FLOOR

	DO 100 I = 1, NM1
        ONTARGET(I) = SIGM * (ZZTEMP(I,UPPER)**4-TAMB(I)**4)
        IF (ONTARGET(I).LT.1.0D0) ONTARGET(I) = 0.0D0
  100 CONTINUE
      RETURN
      END
	SUBROUTINE REMAPFIRES (NFIRES, FLOCAL, FXLOCAL, FYLOCAL, 
     . FZLOCAL, FQLOCAL, FHLOCAL)

C	This routine is to combine the main fire (in lfbo) and any objects into a single list
C	There does not have to be a main fire nor any objects, so NFIRES may be zero

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "smkview.fi"
      include "objects1.fi"
      include "objects2.fi"

C	First, the mainfire if there is one

	IF (LFBO.GT.0) THEN
		nfires = 1
		FLOCAL(1) = FROOM(0)
		FXLOCAL(1) = fopos(1,0)
		FYLOCAL(1) = fopos(2,0)
		FZLOCAL(1) = fopos(3,0)
		CALL FLAMHGT (FQF(0),FAREA(0),FHEIGHT)
		FQLOCAL(1) = FQF(0)
		FHLOCAL(1) = FHEIGHT
	ELSE
		NFIRES = 0
	ENDIF
	
C	Now the other objects

	DO I = 1, NUMOBJL
		NFIRES = NFIRES + 1
		FXLOCAL(NFIRES) = fopos(1,i)
		FYLOCAL(NFIRES) = fopos(2,i)
		FZLOCAL(NFIRES) = fopos(3,i)
        CALL FLAMHGT (fqf(i),FAREA(I),FHEIGHT)
		FQLOCAL(NFIRES) = fqf(i)
		FHLOCAL(NFIRES) = FHEIGHT
		flocal(nfires) = froom(i)
	END DO
	RETURN
	END

	subroutine sethoc (maxint, mdot, qdot, hdot, hinitial)

!	Routine to implement the algorithm to set the heat of combustion for all fires

      include "precis.fi"

	double precision mdot(maxint), qdot(maxint), hdot(maxint)

      data hcmax /5.0D7/, hcmin/1.31D+7/
	
	do 600 i = 1, maxint
	if(i.gt.1) then
	    if (mdot(i)*qdot(i).le.0.d0) then
	  		hdot(i) = hinitial
	    else					
	  		Hdot(I) = min(hcmax,max(Qdot(I)/(mdot(I)),hcmin))
	    endif
	else
	    hdot(1) = hinitial
	endif
  600	continue

	return
	end subroutine sethoc

	SUBROUTINE UPDOBJ(IFLAG, TOLD, DT, IFOBJ, TOBJ, IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPDOBJ
C
C     Source File: UPDOBJ.f
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: IFLAG   Flags if check, set, or update variables
C                TOLD    Time previous to this time step
C                DT      Length of last time step
C                IFOBJ   Object number that ignites (return)
C                TOBJ    Time object ignites
C                IERROR  Returns error codes
C
C     Revision History:
C        Created:  8/15/1995 at 13:08 by PAR
C        Modified: 9/5/1995 at 10:29 by PAR:
C                  Added support for IERROR and returns of stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "fltarget.fi"
      include "opt.fi"

	DIMENSION TMPOB(2,MXOIN)

	IFOBJ = 0
	TOBJ = TOLD + 2.D0*DT
	TNOBJ = TOLD + DT

!!!!! Note that ignition type 1 is time, type 2 is temperature and 3 is flux !!!
!!!!! The critiria for temperature and flux are stored backupwards - this historical
!!!!! See corresponding code in keywordcases
	DO 10 IOBJ = 1, NUMOBJL
	  IF (OBJON(IOBJ)) GOTO 10
	  IGNFLG = OBJIGN(IOBJ)
	  IOBTARG = OBTARG(IOBJ)
	  IF (IGNFLG.EQ.1) THEN
	    IF (OBJCRI(1,IOBJ).LE.TNOBJ) THEN
	      TOBJ = MIN(OBJCRI(1,IOBJ),TOBJ)
		    IFOBJ = IOBJ
	      TMPOB(1,IOBJ) = 1.D0
	      TMPOB(2,IOBJ) = OBJCRI(1,IOBJ)
	    ELSE
	      TMPOB(1,IOBJ) = 0.0D0
	      TMPOB(2,IOBJ) = TNOBJ + DT
	    END IF
	  ELSE IF (IGNFLG.EQ.2) THEN
	    CALL DO_OBJCK(IFLAG, TOLD, DT, XXTARG(TRGTEMPF,IOBTARG), 
     .         OBJCRI(3,IOBJ), OBCOND(OBOTEMP,IOBJ), IOBJ, IFOBJ, TOBJ,
     .         TMPOB(1,IOBJ))
          ELSE IF (IGNFLG.EQ.3) THEN
	    CALL DO_OBJCK(IFLAG, TOLD, DT, XXTARG(TRGTFLUXF,IOBTARG), 
     .         OBJCRI(2,IOBJ), OBCOND(OBOFLUX,IOBJ), IOBJ, IFOBJ, TOBJ,
     .         TMPOB(1,IOBJ))
        ELSE
          CALL XERROR('UPDOBJ - Incorrectly defined object type',0,1,1)
          IERROR = 20
          RETURN
	  ENDIF
   10 CONTINUE
	    
	IF (IFLAG.NE.MDCHK) THEN
	  DO 20 IOBJ = 1, NUMOBJL
	    IF (.NOT.OBJON(IOBJ)) THEN
	      IOBTARG = OBTARG(IOBJ)
	      OBCOND(OBOTEMP,IOBJ) = XXTARG(TRGTEMPF,IOBTARG)
	      OBCOND(OBOFLUX,IOBJ) = XXTARG(TRGTFLUXF,IOBTARG)
	      IF (IFLAG.EQ.MDSET.AND.TMPOB(1,IOBJ).GT.0.0D0) THEN
	        IF (TMPOB(2,IOBJ).LE.TOBJ) THEN
		       OBJON(IOBJ) = .TRUE.
                IF (OPTION(FBTOBJ).EQ.ON) THEN
		          OBJSET(IOBJ) = 1
                ELSE
                   OBJSET(IOBJ) = 0
                END IF
		       OBJCRI(1,IOBJ) = TMPOB(2,IOBJ)
		      END IF
	      END IF
	    END IF
   20   CONTINUE
      END IF

	RETURN
	END

	SUBROUTINE DO_OBJCK(IFLAG,TOLD, DT, COND, TRIP, OLDCOND, IOBJ,
     .                    IFOBJ, TOBJ, TMPOB)

      include "precis.fi"

     	DIMENSION TMPOB(2)

      IF (COND.GT.TRIP) THEN
	  DELTA = (TRIP-OLDCOND)/(COND-OLDCOND)
	  TMPOB(1) = 1.0D0
	  TMPOB(2) = TOLD + DT*DELTA
	  TOBJ = MIN(TOBJ,TMPOB(2))
	  IFOBJ = IOBJ
	ELSE
	  TMPOB(1) = 0.0D0
	  TMPOB(2) = TOLD + 2.D0*DT
	END IF

	RETURN
	END

      SUBROUTINE HCL (FLWHCL, FLXHCL,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HCL
C
C     Source File: HCL.SOR
C
C     Functional Class:  Physical interface routine
C
C     Description:       Physical Interface routine to do HCl deposition
C                        on wall surfaces.
C
C     Arguments: FLWHCL  Mass and energy flows into layers due to HCl
C                        deposition.  Standard source routine data 
C                        structure.
C                FLXHCL  HCl surface concentration flux.
C                IERROR  Returns error codes
C
C     Commons:
C        USED:  Activs   Ar       Br       Dr       Hr       Hwj     
C               Hwjdot   Mass     N        Nm1      Qscnv    Switch  
C               Twj      Zzhlay   Zzrho    Zztemp   Zzvol   
C
C     Revision History:
C        Created:  9/5/1995 at 9:32 by PAR
C        Modified: 9/5/1995 at 9:35 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "opt.fi"
      DIMENSION FLWHCL(NR,NS+2,2), FLXHCL(NR,4)

C     INITIALIZE SUMMATIONS AND LOCAL DATA

      X0 = 0.0D0

C*** only zero out mass (lsp=1) and hcl (lsp=2+6) entries of flwhcl

      DO 10 IROOM = 1, N
      DO 11 J = 1, NS+2
        FLWHCL(IROOM,J,UPPER) = X0
   11   FLWHCL(IROOM,J,LOWER) = X0
        FLXHCL(IROOM,1) = X0
        FLXHCL(IROOM,2) = X0
        FLXHCL(IROOM,3) = X0
        FLXHCL(IROOM,4) = X0
   10 CONTINUE
      IF (OPTION(FHCL).EQ.OFF) RETURN

C     CALCULATE THE HCL "ADDED" TO THE LAYERS FROM EACH SURFACE

      IF (ACTIVS(6)) THEN
        DO 30 IROOM = 1, NM1
          DO 20 IWALL = 1, 4
            IF (SWITCH(IWALL,IROOM)) THEN
              IF (IWALL.EQ.1) THEN
                ARW = AR(IROOM)
                LAYER = UPPER
              ELSE IF (IWALL.EQ.2) THEN
                ARW = AR(IROOM)
                LAYER = LOWER
              ELSE IF (IWALL.EQ.3) THEN
                ARW = (BR(IROOM)+DR(IROOM)) * ZZHLAY(IROOM,UPPER) * 
     +              2.0D0
                LAYER = UPPER
              ELSE IF (IWALL.EQ.4) THEN
                ARW = (BR(IROOM)+DR(IROOM)) * (HR(IROOM)-
     +              ZZHLAY(IROOM,UPPER)) * 2.0D0
                ARW = MAX(X0,ARW)
                LAYER = LOWER
              END IF
C              Hclg = Mass(Layer,iroom,6) / Zzvol(iroom,Layer)
C              H2o = Mass(Layer,iroom,8) / Zzvol(iroom,Layer)

C*** use environment variables

              HCLG = ZZCSPEC(IROOM,LAYER,6)
              H2O = ZZCSPEC(IROOM,LAYER,8)
              RHO = ZZRHO(IROOM,LAYER)
              TG = ZZTEMP(IROOM,LAYER)
              HCLW = ZZWSPEC(IROOM,IWALL)
              FLUX = QSCNV(IWALL,IROOM)
              TW = TWJ(1,IROOM,IWALL)
              CALL HCLTRAN(IROOM,IWALL,ARW,HCLG,H2O,RHO,TG,HCLW,FLUX,TW,
     +            HWDOT,HNET,IERROR)
              IF (IERROR.NE.0) RETURN

C             SUM UP THE FLOWS AND FLUXES FOR THE SOURCE ROUTINE

              FLWHCL(IROOM,1,LAYER) = FLWHCL(IROOM,1,LAYER) + HNET
              FLWHCL(IROOM,2+6,LAYER) = FLWHCL(IROOM,2+6,LAYER) + HNET
              FLXHCL(IROOM,IWALL) = HWDOT

            END IF
   20     CONTINUE
   30   CONTINUE
      END IF
      RETURN
      END

      SUBROUTINE HCLTRAN(ICOMP,IWALL,ARW,HCLG,H2O,RHO,TG,HCLW,FLUX,TW,
     +    HWDOT,HNET,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     HCLTRAN
C
C     Source File: HCLTRAN.SOR
C
C     Functional Class:  
C
C     Description: routine to calculate the hydrogen chloride balance 
C                  in the gas and on the wall surface.
C
C     Arguments: ICOMP   Compartment number (input)
C                IWALL   Wall surface number (input)
C                ARW     Area of the wall surface (m^2) (input)
C                HCLG    Current HCL gas concentration (kg/m^3) (input)
C                H2O     Current H2O gas concentration (kg/m^3) (input)
C                RHO     Current gas density (kg/m^3) (input)
C                TG      Gas layer temperature (K) (input)
C                HCLW    Current HCL wall density (kg/m^2) (input)
C                FLUX    Current convective heat flux on wall (W/m^2) (input)
C                TW      Corresponding wall temperature (K) (input)
C                HWDOT   Time derivative of the HCl wall concentration (output)
C                HNET    Time derivative of the HCL gas concentration (output)
C                IERROR  Returns error codes (output)
C
C     Commons:
C        USED:  Cp       Hclbf   
C
C     Revision History:
C        Created:  10/29/1989 at 9:36 by WWJ:
C        Modified: 10/29/1989 at 9:46 by WWJ:
C                  fix the coefficient for deposition, 
C                  optimize the numerics
C        Modified: 2/27/1990 at 9:46 by WWJ:
C                  fix the constants, and add wall absorption 
C                  coefficents to the thermal database
C        Modified: 9/5/1995 at 9:46 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"

      XX0 = 0.0D0
      HWDOT = XX0
      HNET = XX0
      IF ((HCLG.EQ.0.).AND.(HCLW.EQ.0.)) RETURN
C
C     NOTE THAT WE CALCULATE DENSITY ON THE FLY, SINCE PPMDV IS NOT UPDATED
C     OFTEN ENOUGH
C
      XHCLF = HCLG * TG * 2.25D-3
      HCLP = XHCLF * 1.0D6
      TWC = TW - 273.0D0
C
C     SPECIFIC VALUES FOR PAINTED GYPSUM - B1 AND B2 ARE FOR GAS PHASE
C     REACTIONS, AND B3 AND B4 ARE FOR THE WALL ITSELF
C
      B1 = HCLBF(1,IWALL,ICOMP)
      B2 = HCLBF(2,IWALL,ICOMP)
      B3 = HCLBF(3,IWALL,ICOMP)
      B4 = HCLBF(4,IWALL,ICOMP)
      B5 = HCLBF(5,IWALL,ICOMP)
      B6 = HCLBF(6,IWALL,ICOMP)
      B7 = HCLBF(7,IWALL,ICOMP)

      IF (B1.LE.0) RETURN

C     CALCULATE HCL GAS-SURFACE PARTITION COEFFICIENT
C     H2OS IS THE SATURATION CONCENTRATION OF WATER.

      IF (TWC.LE.40.D0) THEN
        IF (HCLP.GT.10.D0) THEN
          H2OS = (1.8204D0-0.18890D0*LOG(HCLP)+0.06466D0*TWC+1.650D-3*
     +        TWC**2+7.408D-5*TWC**3) / TW
        ELSE
          XTEMP = 17.64262D0 - 5164.1D0 / TW
          EXPTW = EXP(XTEMP)
          BCOEF = (7.696D-5+3.5920D-6*TWC+9.166D-8*TWC**2+4.116D-9*TWC
     +        **3) / TW - 1.D-7 * EXPTW
          H2OS = 0.018D0 * EXPTW + 1.8D4 * BCOEF * HCLP
        END IF
      ELSE IF ((TWC.GT.40.0D0).AND.(TWC.LE.60.0D0)) THEN
        H2OS = (7.044D0-2.2416D3*XHCLF-3.874D-3*TWC**2+2.328D-4*TWC**3
     +      +2.376D6*XHCLF**2-5.527D8*XHCLF**3+4.918D10*XHCLF**4-
     +      1.359D12*XHCLF**5-1.4033D2*TWC*XHCLF+2.431D4*TWC*XHCLF**2-
     +      1.6023D6*TWC*XHCLF**3) / TW
      ELSE IF ((TWC.GT.60.0D0).AND.(TWC.LE.80.0D0)) THEN
        H2OS = (107.46D0-4.129D0*TWC+5.096D-2*TWC**2-3.1915D8*XHCLF**3
     +      +1.0408D10*XHCLF**4-2.2793D11*XHCLF**5-5.8194D0*TWC**2*
     +      XHCLF+7.6883D4*TWC*XHCLF**2-7.4363D2*TWC**2*XHCLF**2+
     +      .059067D0*TWC**3*XHCLF+1.8132D6*TWC*XHCLF**3) / TW
      ELSE IF ((TWC.GT.80.0D0).AND.(TWC.LE.95.0D0)) THEN
        H2OS = (2.583D2-8.0386D0*TWC+1.739D5*XHCLF+7.608D-2*TWC**2-
     +      1.5492D7*XHCLF**2+3.956D9*XHCLF**3-2.065D11*XHCLF**4+
     +      1.3747D13*XHCLF**5-4.086D3*TWC*XHCLF+24.06D0*TWC**2*XHCLF+
     +      1.3558D5*TWC*XHCLF**2-3.076D7*TWC*XHCLF**3) / TW
      ELSE IF ((TWC.GT.95.0D0).AND.(TWC.LE.110.0D0)) THEN
        H2OS = (6.431D2-16.374D0*TWC+2.822D5*XHCLF+0.12117D0*TWC**2-
     +      8.224D7*XHCLF**2-7.387D6*XHCLF**3-5.247D3*TWC*XHCLF+
     +      24.30D0*TWC**2*XHCLF+1.5465D6*TWC*XHCLF**2-7.250D3*TWC**2*
     +      XHCLF**2) / TW
      ELSE IF (TWC.GT.110.0D0) THEN
        XTEMP = 18.3036D0 - 3816.44D0 / (TW-46.13D0)
        H2OS = 0.2885D0 * EXP(XTEMP) / TW
      ELSE
C        STOP 'Error in hcltran - H2O out of range'
         CALL XERROR('HCLTRAN - H2O out of range',0,1,1)
         IERROR = 12
         RETURN
      END IF
C
C     CALCULATE THE COEFFICIENTS
C
C     RK IS THE CONSTANT "kc" WHICH IS THE DEPOSITION COEFFICIENT (M/S)
C     RKE IS THE EQUILIBRIUM COEFFIENT BETWEEN THE GAS AND SOLID PHASE
C
      IF (TW.GE.TG) THEN
        RK = 8.33D-3
      ELSE
        X001 = .001D0
        RK = ABS(FLUX/(MAX(X001,TG-TW)*RHO*CP))
      END IF
      IF (H2OS.GT.H2O) THEN
        XTEMP = 1500.0D0 / TW
        EXPTW = EXP(XTEMP)
        RKE = B1 * EXPTW / (1.0D0+B2*EXPTW*HCLG) * (1.0D0+B5*H2O**B6/(
     +      (H2OS-H2O)**B7))
      ELSE
        RKE = 1.0D4
      END IF
C
C     CALCULATE THE DERIVATIVES
C
      HCLCOF = RK * (HCLG-HCLW/(RKE+1.0D-20))
      HNET = -HCLCOF * ARW
      XTEMP = -B4 / (8.31D0*TW)
      HWDOT = HCLCOF - B3 * EXP(XTEMP) * HCLW
      RETURN
      END
      integer function rev_fire
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_fire = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_fire