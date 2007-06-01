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
C					  (I,19) characteristic length of the burning volume
C
C     Revision History:
C        Created:  7/8/1993   at 15:36 by RDP
C        Modified  10/14/1993 added detection/suppression by GPF
C                  4/24/1995  removed references to TMASS to eliminate FLINT
C                             complaint
C                  8/15/1995  at 13:39 by PAR
C                             added UPDATE to argument list for FIRES and
C                               OBJINT.  Fixed so LFBT = 0 means no main fire.
C                  7/22/1996  gpf
C                             update hall data structures for rooms that are
C                             halls and have a main fire.
C                  9/10/1996  by PAR
C                             changed qf to qfc in fire flow computation
C                             (qdot terms in ode's use convective not total
C                              heat release, radiative contribution to
C                              layers is handled in rdheat.)
C                  5/11/98 by GPF:
C                  Implement fast startup option.  Execute this routine only if this 
C                  modeling feature is being used (rather than zeroing out the flow vector.)
C

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
     +      CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,MFIRET)
        DO 40 LSP = 1, NS
          STMASS(UPPER,LSP) = ZZGSPEC(LFBO,UPPER,LSP)
          STMASS(LOWER,LSP) = ZZGSPEC(LFBO,LOWER,LSP)
   40   CONTINUE

        CALL DOFIRE(0,LFBO,EMP(LFBO),HR(LFBO),BR(LFBO),DR(LFBO),HCOMBT,
     +      CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,MFIRET,
     +      STMASS,FPOS(1),FPOS(2),FPOS(3)+HF0T,EME(LFBO),EMS(LFBO),
     +      QPYROL,XNTMS,QF(LFBO),QFC(1,LFBO),XQFR,HEATLP(LFBO),
     +      HEATUP(LFBO))

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
	  xfire(nfire,19) = 1.0d-1
        FROOM(0) = LFBO
        FEMP(0) = EMP(LFBO)
        FEMS(0) = EMS(LFBO)
        FQF(0) = HEATLP(LFBO) + HEATUP(LFBO)
        FQFC(0) = QFC(1,LFBO)
        FQLOW(0) = HEATLP(LFBO)
        FQUPR(0) = HEATUP(LFBO)
        FAREA(0) = APS(LFBO)
        FPOSX(0) = FPOS(1)
        FPOSY(0) = FPOS(2)
        FPOSZ(0) = FPOS(3)

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
     +        CLFRAT,CNFRAT,UPDATE)
          OPLUME(1,IOBJ) = OMASST

          DO 60 LSP = 1, NS
            STMASS(UPPER,LSP) = ZZGSPEC(IROOM,UPPER,LSP)
            STMASS(LOWER,LSP) = ZZGSPEC(IROOM,LOWER,LSP)
   60     CONTINUE

          CALL DOFIRE(I,IROOM,OPLUME(1,IOBJ),HR(IROOM),BR(IROOM),
     +        DR(IROOM),HCOMBT,CCO2T,COCO2T,HCRATT,OCRATT,CLFRAT,CNFRAT,
     +        MFIRET,STMASS,OBJPOS(1,IOBJ),OBJPOS(2,IOBJ),
     +        OBJPOS(3,IOBJ)+HF0T,OPLUME(2,IOBJ),OPLUME(3,IOBJ),QPYROL,
     +        XNTMS,QF(IROOM),QFC(1,IROOM),XQFR,HEATLP(IROOM),
     +        HEATUP(IROOM))

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

C         MAKE UP IFROOM AND XFIRE FOR THIS OBJECT FIRE

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
          FQF(NOBJ) = HEATLP(IROOM) + HEATUP(IROOM)
          FQFC(NOBJ) = QFC(1,IROOM)
          FQLOW(NOBJ) = HEATLP(IROOM)
          FQUPR(NOBJ) = HEATUP(IROOM)
          FAREA(NOBJ) = OAREAT
          FPOSX(NOBJ) = OBJPOS(1,IOBJ)
          FPOSY(NOBJ) = OBJPOS(2,IOBJ)
          FPOSZ(NOBJ) = OBJPOS(3,IOBJ)
        END IF
   80 CONTINUE
      RETURN
      END
