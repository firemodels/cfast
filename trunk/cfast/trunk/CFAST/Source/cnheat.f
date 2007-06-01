      SUBROUTINE CNHEAT(UPDATE,DT,FLXTOT,DELTA)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CNHEAT
C
C     Source File: CNHEAT.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Interface between RESID and the conduction calculation.
C                   For each active wall surface in each routine this
C                   routine calculates the residual function 
C                   q'' + K dT/dx, which when zero is simply Fourier's
C                   law of heat conduction.
C
C     Arguments: UPDATE  We don't keep solution unless UPDATE is 1 or 2
C                        If UPDATE is 2 then we don't calculate DELTA or
C                        use FLXTOT
C                DT Time step interval from last valid solution point
C                FLXTOT  Total flux striking walls
C                DELTA   The residual of q'' + K DT/dx
C
C     Revision History:
C        Created:  by GPF
C        Modified: 2/5/93 BY GPF
C                  added partial derivative calculations for the
C                  reduced Jacobian option
C        Modified: 2/25/94 BY GPF
C                  added external temperature to CNDUCT call for
C                  the room to room heat transfer option
C        Modified by GPF 10/10/94
C               Added fourth boundary condition option, flux on both
C               sides of wall.  This is needed when we do a full 
C               numerical conduction calculation of targets.  Currently,
C               we assume that targets are thin surfaces, ie we calculate
C               a temperature that results in a net flux of zero
C               striking the target.
C        Modified by GPF 4/26/95
C               Removed ETA from CNDUCT's argument list
C        Modified by GPF 7/13/95
C               Reduced the number of conduction calculations performed during
C               a Jacobian calculation (OPTION(FMODJAC)==2)
C        Modified by GPF 2/5/96   removed reduced jacobian option added on 2/5/93.
C        Modified by GPF 2/5/99   added wall to wall heat transfer
C
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "wdervs.fi"
      include "opt.fi"

      DIMENSION TGRAD(2), DELTA(*)
      DIMENSION FLXTOT(NR,NWAL), IREVWC(NWAL)
      DIMENSION VTGRAD0(4*NR), VTGRAD(4*NR)
      INTEGER UPDATE
      SAVE VTGRAD0
      DATA IREVWC/2,1,3,4/

C
      SIGMA = 5.67D-8

C*** SOLVE CONDUCTION PROBLEM FOR ALL WALLS

      IBEG = 1
      IEND = NWALLS

C*** If the reduced jacobian option is on and dassl is computing a jacobian
C    then solve a conduction problem only if dassl is varying a wall temperature

      IF(OPTION(FMODJAC).EQ.ON.AND.JACCOL.GT.0)THEN
        IF(IZEQMAP(JACCOL,1).EQ.EQWT)THEN

C  a wall temperatuer is being perturbed so solve that walls conduction problem

          IBEG = JACCOL - NOFWT
          IEND = IBEG
         ELSE

C   some other variable is being perturbed, so don't solve any 
C   conduction problems

          IBEG = 1
          IEND = 0
        ENDIF
      ENDIF

      DO 10 IW = IBEG, IEND
            IROOM = IZWALL(IW,1)
            IWALL = IZWALL(IW,2)
            ICOND = NOFWT + IW

C*** use exterior wall temperature from last time step to ...

            TWINT = ZZWTEMP(IROOM,IWALL,1)
            TWEXT = ZZWTEMP(IROOM,IWALL,2)
            TGAS = ETA(IROOM)
            IWEQ = IZWMAP2(IWALL,IROOM) - NOFWT
            IWB = IZWALL(IWEQ,5)

C*** compute flux seen by exterior of wall

            IF (IWB.EQ.3) THEN

c*** back wall is connected to the outside

              CALL CONVEC(IREVWC(IWALL),TGAS,TWEXT,WFLUXOUT)
              WFLUXOUT = WFLUXOUT + SIGMA * (TGAS**4-TWEXT**4)
              WFLUXSAVE = WFLUXOUT
              IF(IZHEAT(IROOM).NE.0.AND.IWALL.NE.1.AND.IWALL.NE.2)THEN

c*** back wall is connected to rooms defined by izhtfrac with fractions
c         defined by zzhtfrac.  If izheat(iroom) is not zero then
c         nwroom better not be zero!  nwroom should always be zero
c         for iwall=3 and iwall=4

                WFLUXOUT = 0.0D0
                NWROOM = IZHTFRAC(IROOM,0)
                DO 50 JJ = 1, NWROOM
                  J = IZHTFRAC(IROOM,JJ)
                  FRAC = ZZHTFRAC(IROOM,J)
                  IF(IWALL.EQ.3)THEN
                    YB = ZZHLAY(IROOM,LOWER)
                    YT = ZZYCEIL(IROOM)
                   ELSEIF(IWALL.EQ.4)THEN
                    YB = 0.0D0
                    YT = ZZHLAY(IROOM,LOWER)
                  ENDIF
                  DFLOR = ZZYFLOR(J) - ZZYFLOR(IROOM)
                  YY = ZZHLAY(J,LOWER) + DFLOR
                  IF(J.NE.NM1+1)THEN
                    IF(YY.GT.YT)THEN
                      FU = 0.0D0
                     ELSEIF(YY.LT.YB)THEN
                      FU = 1.0D0
                     ELSE
                      IF(YB.NE.YT)THEN
                        FU = (YT-YY)/(YT-YB)
                       ELSE
                        FU = 0.0D0
                      ENDIF
                    ENDIF
                    FLUXU = FU*FLXTOT(J,3)
                    FLUXL = (1.0D0-FU)*FLXTOT(J,4)
                   ELSE
                    FLUXU = WFLUXSAVE
                    FLUXL = 0.0D0
                  ENDIF
                  WFLUXOUT = WFLUXOUT + FRAC*(FLUXU + FLUXL)
   50           CONTINUE
              ENDIF
            END IF
            CALL CNDUCT(UPDATE,TWINT,TWEXT,DT,
     +          FKW(1,IWALL,IROOM),CW(1,IWALL,IROOM),RW(1,IWALL,IROOM),
     +          TWJ(1,IROOM,IWALL),WALLDX(1,IROOM,IWALL),
     +          NUMNODE(1,IWALL,IROOM),NSLB(IWALL,IROOM),
     +          WFLUXIN,WFLUXOUT,IWB,TGRAD,TDERV)

C*** store wall gradient

            VTGRAD(IW) = TGRAD(2)

C*** COMPUTE PARTIAL OF WALL TEMPERATURE EQUATION WITH
C    RESPECT TO THE WALL TEMPERATURE.  WE ASSUME THAT
C    THE PARTIALS OF CONVECTIVE HEAT FLUX AND RADIATIVE HEAT
C    FLUX WITH RESPECT TO WALL TEMPERATURE HAVE ALREADY
C    BEEN COMPUTED.  (IF THEY WERE NOT THEN WE WOULDN'T KNOW
C    HEAT FLUX STRIKING THE WALL!  

   10 CONTINUE

C*** save wall gradients during base call to resid (cnduct)

      IF(OPTION(FMODJAC).EQ.ON)THEN

c*** store wall gradient for later use

        IF(JACCOL.EQ.0)THEN
          DO 20 IW = 1, NWALLS
            VTGRAD0(IW) = VTGRAD(IW)
   20     CONTINUE
         ELSEIF(JACCOL.GT.0)THEN

c*** use saved wall temperature gradient except for conduction problem
c    corresponding to the perturbed wall temperature

          IF(IZEQMAP(JACCOL,1).EQ.EQWT)THEN
            IEQ = JACCOL - NOFWT
           ELSE
            IEQ = 0
          ENDIF
          DO 30 IW = 1, NWALLS
            IF(IW.NE.IEQ)VTGRAD(IW) = VTGRAD0(IW)
   30     CONTINUE
        ENDIF
      ENDIF                 

C*** DASSL WILL TRY TO FORCE DELTA TO BE ZERO, SO THAT FOURIER'S
C    LAW, Q = -K DT/DX, IS SATISFIED AT THE WALL SURFACE 

      IF(UPDATE.NE.2)THEN
        DO 40 IW = 1, NWALLS
          ICOND = NOFWT + IW
          IROOM = IZWALL(IW,1)
          IWALL = IZWALL(IW,2)
          DELTA(ICOND) = FLXTOT(IROOM,IWALL) + VTGRAD(IW) * 
     +    FKW(1,IWALL,IROOM)
   40   CONTINUE
      ENDIF

      RETURN
#ifdef pp_gui
        ENTRY INCNHEAT
         IREVWC(1) = 2
         IREVWC(2) = 1
         IREVWC(3) = 3
         IREVWC(4) = 4
        RETURN
#endif
      END
