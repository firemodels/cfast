      SUBROUTINE RESID (TSEC,X,XPSOLVE,DELTA,IRES,RPAR,IPAR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RESID
C
C     Functional Class:
C
C     Description:  Calculates the residual F(t,y,dy/dt) for CFAST
C                   differential and algebraic equations.  For the gas
C                   differential equations (pressure, layer volume,
C                   upper/lower layer temperature) F(t,y,dy/dt) takes
C                   the form F(t,y,dy/dt) = dy/dt - f(t,y) where f(t,y) is
C                   related to the conservation of mass and and energy.
C                   For the wall temperature equations, F is just Fourier's
C                   law taking the form of
C                   F(t,y,dy/dt) = q''(t,y) + K dT/dx
C                   where q'' is the flux striking the wall, K is the wall's
C                   thermal conductivity and dT/dx is the surface wall
C                   temperature gradient.
C
C     Arguments: TSEC    Current simulation time (T above in s)
C                X       Current guess at solution vector (Y above)
C                XPSOLVE XPSOLVE Current guess at derivative of solution
C                        vector (Y' above)
C                DELTA   Residual or value of F(t,y,dy/dt)
C                IRES    Outputs:  IRES    Integer flag which is always equal to
C                        zero on input. RESID should alter IRES
C                        only if it encounters an illegal value of Y or
C                        a stop condition. Set IRES = -1 if an input
C                        value is illegal, and DDASSL will try to solve
C                        the problem without getting IRES = -1. If
C                        IRES = -2, DASSL return control to the calling
C                        program with IDID = -11.
C                RPAR    real parameter arrays
C                        are used for communication between SOLVE and
C                        RESID via DASSL. They are not altered by DASSL.
C                        Currently, only IPAR is used in RESID to pass
C                        a partial / total flag for solution of the
C                        species equations.
C                IPAR
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 06/14/1992 at 10:07 by GPF:
C                  Installed HVAC (solved by DASSL instead of by time splitting)
C        Modified: 2/7/93 by GPF:
C                  The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have
C                  the number of fires as the first subscript.
C        Modified: 10/14/93 by GPF:
C                  added detection/suppression
C                  Defined STIME
C        Modified: 6/10/1994 by GPF:
C                  added shaft option (combine two layers into one)
C        Modified: 10/10/94 & 3/10/95
C                  the array xfire was formerly a local variable in
C                  resid.  the target calculation routines are not
C                  called by resid and needed access to xfire.
C                  therefor xfire was moved to main common block.
C        Modified: 4/26/1995 gpf:
C                  added call to TRHEAT to calculate TARGET residuals for targets
C                  temperatures computed by DASSL
C        Modified: 6/30/1995 gpf:
C                  added equations for oxygen
C        Modified: 7/13/1995 gpf:
C                  changed MXIRM to NR in several dimension statements
C          Modified: 8/15/1995 par:
C                    changed the argument list for FIRES to match the changed
C                    FIRES subroutine.  Added calculating the residuals for
C                    flame spread objects.  Added OBJECTS2.INC to get resid.
C        Modified: 9/5/1995 at 10:23 by PAR:
C                  Added support for IERROR and returning error codes to DASSL
C        Modified: 7/22/1996 by GPF
C                  changed product reference from IP to M.  (product
C                  flow logic depends on where mass is flowing, IP was not
C                  correct)
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C     Commons:

C     WRITTEN:  Dt       Qc       Qf       Qscnv
C      PASSED:  Dt
C        USED:  Activs   Ar       Cp       Gamma    Hr       Izpmap
C               N        Nlspct   Nm1      Option   Qf       Switch
C               Told     Zzhlay   Zzmass   Zzpabs   Zzrho    Zztemp
C               Zzvol


      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "sizes.fi"
      include "opt.fi"
      include "params.fi"
      include "dervs.fi"
      include "wnodes.fi"
      include "flwptrs.fi"
      include "fltarget.fi"
      include "objects2.fi"

C     TEMPORAY DECLARATIONS AND ASSIGNMENTS

      INTEGER ALL, SOME, UU, LL
      PARAMETER (ALL = 1,SOME = 0, UU = UPPER,LL = LOWER)

C     DATA STRUCTURES FOR DASSL, THE NUMERICAL SOLVER

      DIMENSION IPAR(*), RPAR(*)
      DIMENSION X(*), XPSOLVE(*), XPRIME(MAXTEQ), DELTA(*)

C     DATA STRUCTURE FOR TOTAL FLOWS AND FLUXES

      DIMENSION FLWTOT(NR,MXPRD+2,2), FLXTOT(NR,NWAL)

C     DATA STRUCTURES FOR FLOW THROUGH VENTS

      DIMENSION FLWNVNT(NR,MXPRD+2,2)
      DIMENSION FLWHVNT(NR,NS+2,2)

C     DATA STRUCTURES FOR FIRES

      DIMENSION FLWF(NR,NS+2,2)

C     DATA STRUCTURES FOR CONVECTION, RADIATION, AND CEILING JETS

      DIMENSION FLWCV(NR,2), FLXCV(NR,NWAL)
      DIMENSION FLWRAD(NR,2), FLXRAD(NR,NWAL)
      DIMENSION FLWCJET(NR,2), FLXCJET(NR,NWAL)

C     DATA STRUCTURES FOR MECHANICAL VENTS

      DIMENSION FLWMV(NR,NS+2,2), filtered(nr,ns+2,2)

C     DATA STRUCTURES FOR HCL DEPOSITION

      DIMENSION FLWHCL(NR,NS+2,2), FLXHCL(NR,4)

C     DATA STRUCTURES FOR DOOR JET FIRES

      DIMENSION FLWDJF(NR,NS+2,2)
      INTEGER UPDATE

      LOGICAL VFLOWFLG, HVACFLG, DJETFLG

      IERROR = 0
      XX0 = 0.0D0
      NPROD = NLSPCT
      DT = TSEC - TOLD
      NUMRESD = NUMRESD + 1
      STIME = TSEC

      NIRM = NM1

      CALL DATACOPY(X,ODEVARA)
      CALL DATACOPY(X,ODEVARB)

C***  If RESID is called by SOLVE THEN IPAR(2)==ALL all residuals
C     are computed.  If RESID is called by DASSL residuals are not
C     computed for species.  Further, temperature profiles are only
C     updated when RESID is called by SOLVE.

      IF (IPAR(2).EQ.SOME) THEN
        UPDATE = 0
      ELSE
        UPDATE = 1
      END IF

      EPSP = RPAR(1)

      DO 1 I = 1, N
    1 QF(I) = XX0

C     CALCULATE FLOW DUE TO UNFORCED VENTS (HFLOW FOR HORIZONTAL FLOW
C     THROUGH VERTICAL VENTS AND VFLOW FOR VERTICAL FLOW THROUGH
C     HORIZONTAL VENTS)
      CALL HFLOW (TSEC,EPSP,NPROD,FLWNVNT)
      CALL VFLOW (TSEC,FLWHVNT,VFLOWFLG)
      CALL MVENT (TSEC,X(NOFPMV+1),X(NOFTMV+1),XPSOLVE(NOFTMV+1),
     .            FLWMV,DELTA(NOFPMV+1),DELTA(NOFTMV+1),
     .            XPRIME(NOFHVPR+1),NPROD,IERROR,HVACFLG,filtered)

      IF (IERROR.NE.0) THEN
        IRES = -2
        RETURN
      END IF
C     CALCULATE HEAT AND MASS FLOWS DUE TO FIRES

      CALL FIRES (TSEC,FLWF,UPDATE)
      CALL SORTFR (NFIRE,IFROOM,XFIRE,IFRPNT,NM1)
      CALL DJET (FLWDJF,DJETFLG)

C     CALCULATE FLOW AND FLUX DUE TO HEAT TRANSFER (CEILING JETs, CONVECTION AND RADIATION

      CALL CJET (FLWCJET,FLXCJET)
      CALL CVHEAT (FLWCV,FLXCV)
      CALL RDHEAT (FLWRAD,FLXRAD,IERROR)
      IF (IERROR.NE.0) THEN
        IRES = -2
        RETURN
      END IF

C     CALCULATE HCL DEPOSITION TO WALLS

      CALL HCL (FLWHCL, FLXHCL,IERROR)
      IF (IERROR.NE.0) THEN
        IRES = -2
        RETURN
      END IF

C     RESET PARALLEL DATA STRUCTURES

      DO 20 I = 1, NM1
         QC(LL,I) = FLWCJET(I,LL) + FLWCV(I,LL)
         QC(UU,I) = FLWCJET(I,UU) + FLWCV(I,UU)
C     ADD IN VENT FIRES TO THE TOTAL.  DOFIRE DOES THE TOTAL OF
C     QF FOR NORMAL FIRES, BUT VENT FIRES ARE DONE AFTERWARDS WITH DJET
         DO 10 J = 1, NWAL
            QSCNV(J,I) = FLXCJET(I,J) + FLXCV(I,J)
   10    CONTINUE
   20 CONTINUE
      IF(DJETFLG)THEN
        DO 21 I = 1, NM1
         QF(I) = QF(I) + FLWDJF(I,Q,LL) + FLWDJF(I,Q,UU)
   21   CONTINUE
      ENDIF


C     SUM FLOW FOR INSIDE ROOMS

      DO 40 IROOM = 1, NIRM

        DO 30 IPROD = 1, NPROD + 2
          IP = IZPMAP(IPROD)
          FLWTOT(IROOM,IPROD,LL) = FLWNVNT(IROOM,IPROD,LL) +
     +        FLWF(IROOM,IP,LL)
          FLWTOT(IROOM,IPROD,UU) = FLWNVNT(IROOM,IPROD,UU) +
     +        FLWF(IROOM,IP,UU)
   30   CONTINUE
        IF(VFLOWFLG)THEN
          DO 31 IPROD = 1, NPROD + 2
          IP = IZPMAP(IPROD)
            FLWTOT(IROOM,IPROD,LL) = FLWTOT(IROOM,IPROD,LL) +
     +          FLWHVNT(IROOM,IP,LL)
            FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     +          FLWHVNT(IROOM,IP,UU)
   31     CONTINUE
        ENDIF
        IF(HVACFLG)THEN
          DO 32 IPROD = 1, NPROD + 2
            IP = IZPMAP(IPROD)
            FLWTOT(IROOM,IPROD,LL) = FLWTOT(IROOM,IPROD,LL) +
     +          FLWMV(IROOM,IP,LL) - filtered(iroom,iprod,ll)
            FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     +          FLWMV(IROOM,IP,UU) - filtered(iroom,iprod,uu)
   32     CONTINUE
        ENDIF
        IF(DJETFLG)THEN
          DO 33 IPROD = 1, NPROD + 2
            IP = IZPMAP(IPROD)
            FLWTOT(IROOM,IPROD,LL) = FLWTOT(IROOM,IPROD,LL) +
     +          FLWDJF(IROOM,IP,LL)
            FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     +          FLWDJF(IROOM,IP,UU)
   33     CONTINUE
        ENDIF

C     ADD IN HCL CONTRIBUTION TO FLWTOT

        IF (ACTIVS(6)) THEN
           FLWTOT(IROOM,1,LL) = FLWTOT(IROOM,1,LL) + FLWHCL(IROOM,1,LL)
           FLWTOT(IROOM,1,UU) = FLWTOT(IROOM,1,UU) + FLWHCL(IROOM,1,UU)
           FLWTOT(IROOM,8,LL) = FLWTOT(IROOM,8,LL) + FLWHCL(IROOM,8,LL)
           FLWTOT(IROOM,8,UU) = FLWTOT(IROOM,8,UU) + FLWHCL(IROOM,8,UU)
        END IF

        FLWTOT(IROOM,Q,LL) = FLWTOT(IROOM,Q,LL) + FLWCV(IROOM,LL) +
     +      FLWRAD(IROOM,LL) + FLWCJET(IROOM,LL)
        FLWTOT(IROOM,Q,UU) = FLWTOT(IROOM,Q,UU) + FLWCV(IROOM,UU) +
     +      FLWRAD(IROOM,UU) + FLWCJET(IROOM,UU)

C*** IF THIS ROOM IS A SHAFT THEN SOLVE FOR ONLY ONE ZONE.
C    THIS IS DONE BY COMBINING FLOWS FROM TO BOTH
C    LAYERS INTO UPPER LAYER FLOW AND SETTING LOWER LAYER FLOW TO
C    ZERO.

        IF(IZSHAFT(IROOM).EQ.1)THEN
           DO 45 IPROD = 1, NPROD + 2
              FLWTOT(IROOM,IPROD,UU) = FLWTOT(IROOM,IPROD,UU) +
     .                                 FLWTOT(IROOM,IPROD,LL)
              FLWTOT(IROOM,IPROD,LL) = XX0
   45      CONTINUE
        ENDIF

c*** calculate temperature of flow going into the upper layer
c    of each room

        IF(JACCOL.LE.0)THEN
          XQU = FLWTOT(IROOM,Q,UPPER)
          XMU = FLWTOT(IROOM,M,UPPER)
          IF(XMU.NE.0.0D0)THEN
            ZZFTEMP(IROOM,UPPER) = XQU/(CP*XMU)
           ELSE
            ZZFTEMP(IROOM,UPPER) = TAMB(IROOM)
          ENDIF
        ENDIF


   40 CONTINUE

C     SUM FLUX FOR INSIDE ROOMS

      DO 60 IROOM = 1, NIRM
        DO 50 IWALL = 1, NWAL
          IF (SWITCH(IWALL,IROOM)) THEN
            FLXTOT(IROOM,IWALL) = FLXCV(IROOM,IWALL) +
     +          FLXRAD(IROOM,IWALL) + FLXCJET(IROOM,IWALL)
          END IF
   50   CONTINUE
   60 CONTINUE

C     SET NPROD TO ZERO WHEN WE ARE ONLY SOLVING "SOME" OF THE ODE'S

      IF (IPAR(2).EQ.SOME) THEN
         NPRODSV = NPROD
         NPROD = 0
      END IF

C     CALCULATE RHS OF ODE'S FOR EACH ROOM

      DO 70 IROOM = 1, NIRM
         AROOM = AR(IROOM)
         HCEIL = HR(IROOM)
         PABS = ZZPABS(IROOM)
         HINTER = ZZHLAY(IROOM,LL)
         QL = FLWTOT(IROOM,Q,LL)
         QU = FLWTOT(IROOM,Q,UU)
         TMU = FLWTOT(IROOM,M,UU)
         TML = FLWTOT(IROOM,M,LL)

         IF(OPTION(FOXYGEN).EQ.ON)THEN
           OXYDU = FLWTOT(IROOM,4,UU)
           OXYDL = FLWTOT(IROOM,4,LL)
         ENDIF

C     PRESSURE EQUATION

         PDOT = (GAMMA-1.0D0) * (QL + QU) / (AROOM*HCEIL)

C     UPPER LAYER TEMPERATURE EQUATION

         TLAYDU = (QU-CP*TMU*ZZTEMP(IROOM,UU)) / (CP*ZZMASS(IROOM,UU))
         IF (OPTION(FODE).EQ.ON) THEN
            TLAYDU = TLAYDU + PDOT / (CP*ZZRHO(IROOM,UU))
         END IF

C     UPPER LAYER VOLUME EQUATION

         VLAYD = (GAMMA-1.0D0) * QU / (GAMMA*PABS)
         IF (OPTION(FODE).EQ.ON) THEN
            VLAYD = VLAYD - ZZVOL(IROOM,UU) * PDOT / (GAMMA*PABS)
         END IF
         IF(IZSHAFT(IROOM).EQ.1)VLAYD = XX0

C     LOWER LAYER TEMPERATURE EQUATION

         TLAYDL = (QL-CP*TML*ZZTEMP(IROOM,LL)) / (CP*ZZMASS(IROOM,LL))
         IF (OPTION(FODE).EQ.ON) THEN
            TLAYDL = TLAYDL + PDOT / (CP*ZZRHO(IROOM,LL))
         END IF
C
C*** IF THE LOWER LAYER IS SMALL AND GETTING SMALLER THEN WE MAKE IT STOP
C
C         IF(ZZVOL(IROOM,LL).LE.ZZVMIN(IROOM).AND.VLAYD.GT.0.0D0)THEN
C            VLAYD = 0.0D0
C            TLAYDL = 0.0D0
C         ENDIF
C
C*** IF THE UPPER LAYER IS BIG AND GETTING BIGGER THEN WE MAKE IT STOP
C
C         IF(ZZVOL(IROOM,UU).GT.ZZVMAX(IROOM).AND.VLAYD.LT.0.0D0)THEN
C            VLAYD = 0.0D0
C            TLAYDU = 0.0D0
C         ENDIF
C
         XPRIME(IROOM) = PDOT
         XPRIME(IROOM+NOFTL) = TLAYDL
         XPRIME(IROOM+NOFVU) = VLAYD
         XPRIME(IROOM+NOFTU) = TLAYDU

         IF(OPTION(FOXYGEN).EQ.ON)THEN
           XPRIME(IROOM+NOFOXYU) = OXYDU
           XPRIME(IROOM+NOFOXYL) = OXYDL
         ENDIF
   70 CONTINUE

C     COMPUTE PRODUCT OF COMBUSTION TERMS

      IF (NPROD.GT.0.AND.IPAR(2).EQ.ALL) THEN
         IPRODU = NOFPRD - 1
         DO 90 IPROD = 1, NPROD
            DO 80 IROOM = 1, NM1
            HCEIL = HR(IROOM)
            HINTER = ZZHLAY(IROOM,LL)
            IPRODU = IPRODU + 2
            IPRODL = IPRODU + 1
            PRODL = FLWTOT(IROOM,IPROD+2,LL)

C*** If this room is a hall and the jet has not reached the end
C    of the hall then don't solve for it using dassl

            PRODU = FLWTOT(IROOM,IPROD+2,UU)

            IF (HINTER.LT.HCEIL) THEN
               XPRIME(IPRODU) = PRODU
            ELSE IF (HINTER.GE.HCEIL.AND.FLWTOT(IROOM,M,UU).LT.XX0)
     +       THEN
               XPRIME(IPRODU) = PRODU
            ELSE
               XPRIME(IPRODU) = XX0
            END IF
            IF (HINTER.GT.XX0) THEN
               XPRIME(IPRODL) = PRODL
            ELSE IF (HINTER.LE.XX0.AND.FLWTOT(IROOM,M,LL).GT.XX0)
     +          THEN
               XPRIME(IPRODL) = PRODL
            ELSE
               XPRIME(IPRODL) = XX0
            END IF
   80     CONTINUE
   90   CONTINUE

C     HCL DEPOSITION.  NOTE THAT THESE ARE DONE ONLY IF HCLDEP IS SET

        IF (HCLDEP.NE.0) THEN
           IWHCL = NOFHCL
           DO 92 IROOM = 1, NM1
              DO 91 IWALL = 1, NWAL
                IWHCL = IWHCL + 1
                XPRIME(IWHCL) = FLXHCL(IROOM,IWALL)
  91          CONTINUE
  92       CONTINUE
        END IF

C     SMOKE DEPOSITION AND AGGLOMERATION.
C     NOTE THAT THESE ARE DONE ONLY IF SMKAGL IS SET

        DO 93 I = NOFSMKW + 1, NOFSMKW + 4 * NM1 * (SMKAGL+SMKAGL)
          XPRIME(I) = XX0
  93    CONTINUE
      END IF

C     RESIDUALS FOR PRESSURE

      DO 100 I = NOFP + 1, NOFP + NM1
         DELTA(I) = XPRIME(I) - XPSOLVE(I)
  100 CONTINUE

C       RESIDUALS FOR FLAME SPREAD

      IF (FSMTYPE.NE.0) THEN
         DELTA(NOFFSM+1) = DYPDT - XPSOLVE(NOFFSM+1)
         DELTA(NOFFSM+2) = DXPDT - XPSOLVE(NOFFSM+2)
         DELTA(NOFFSM+3) = DYBDT - XPSOLVE(NOFFSM+3)
         DELTA(NOFFSM+4) = DXBDT - XPSOLVE(NOFFSM+4)
         DELTA(NOFFSM+5) = DQDT -  XPSOLVE(NOFFSM+5)
      END IF

C     RESIDUALS FOR LAYER VOLUME, AND LAYER TEMPERATURES

      DO 105 I = NOFTU + 1, NOFTU + 3*NM1
         DELTA(I) = XPRIME(I) - XPSOLVE(I)
  105 CONTINUE

C     RESIDUAL FOR OXYGEN

      IF(OPTION(FOXYGEN).EQ.ON)THEN
        DO 106 I = 1, NM1
           DELTA(I+NOFOXYU) = XPRIME(I+NOFOXYU) - XPSOLVE(I+NOFOXYU)
           DELTA(I+NOFOXYL) = XPRIME(I+NOFOXYL) - XPSOLVE(I+NOFOXYL)
  106   CONTINUE
      ENDIF

C     CONDUCTION RESIDUAL

      CALL CNHEAT(UPDATE,DT,FLXTOT,DELTA)

C     target residual

      CALL TRHEAT(0,MPLICIT,DT,XPSOLVE,DELTA)

C     RESIDUALS FOR STUFF THAT IS SOLVED IN SOLVE ITSELF, AND NOT BY DASSL

      IF (NPROD.NE.0) THEN

C       RESIDUALS FOR GAS LAYER SPECIES
        DO 110 I = NOFPRD + 1, NOFPRD + 2*NPROD*NM1
          DELTA(I) = XPRIME(I) - XPSOLVE(I)
  110   CONTINUE

C       RESIDUALS FOR HCL DEPOSITION, SMOKE DEPOSITION AND SMOKE AGGLOMERATION
        DO 111 I = NOFHCL+1, NOFHCL + 4*NM1*(HCLDEP+SMKAGL+SMKAGL)
           DELTA(I) = XPRIME(I) - XPSOLVE(I)
  111   CONTINUE

C       RESIDUAL FOR hvac SPECIES
        DO 112 I = NOFHVPR+1, NOFHVPR+NLSPCT*NHVSYS
            DELTA(I) = XPRIME(I) - XPSOLVE(I)
  112   CONTINUE
      ENDIF

      IF (IPAR(2).EQ.SOME) THEN
        NPROD = NPRODSV
      END IF

      RETURN
      END
