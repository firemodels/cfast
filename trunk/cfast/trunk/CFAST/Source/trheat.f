      SUBROUTINE TRHEAT(UPDATE,METHOD,DT,XPSOLVE,DELTA)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TRHEAT
C
C     Source File: TRHEAT.SOR
C
C     Functional Class:  
C
C     Description:  Compute dassl residuals associated with targets
C
C     Arguments: UPDATE   variable indicating whether temperature profile
C                         should be updated
C                METHOD   one of steady, mplicit or xplicit (note: these
C                         are parameter values, not mis-pelld)
C                DT       time step
C                XPSOLVE  dassl derivative estimate
C                DELTA    dassl residual array
C
C     Revision History:
C        Created:  5/5/1995 at 13:51 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "fltarget.fi"
      include "cenviro.fi"
      DIMENSION TMP(TRGTNUM), WALLDX(TRGTNUM), NMNODE(2), DELTA(*)
      DIMENSION XPSOLVE(*), TGRAD(2)
      LOGICAL FIRST
      INTEGER UPDATE
      DIMENSION WK(1),WSPEC(1),WRHO(1)
      SAVE FIRST,TMP
      DATA FIRST/.TRUE./
      
      IF(METHOD.EQ.STEADY)RETURN

C*** INITIALIZE NON-DIMENSIONAL TARGET NODE LOCATIONS THE FIRST TIME TRHEAT IS CALLED

      IF(FIRST)THEN
        FIRST = .FALSE.
        NNN = TRGTNUM - 1
        TMP(1) = 1.0D0
        TMP(NNN) = 1.0D0
        DO 10 I = 2, NNN/2 
          TMP(I) = TMP(I-1)*1.50D0
          TMP(NNN+1-I) = TMP(I)
   10   CONTINUE
        IF(MOD(NNN,2).EQ.1)TMP(NNN/2+1)=TMP(NNN/2)*1.50D0
        SUM = 0.0D0
        DO 20 I = 1, NNN
          SUM = SUM + TMP(I)
   20   CONTINUE
        XFACT = 1.0D0/SUM
        DO 30 I = 1, NNN
          TMP(I) = TMP(I)*XFACT
   30   CONTINUE
      ENDIF

C*** CACLULATE NET FLUX STRIKING EACH SIDE OF TARGET

      CALL TARGET(METHOD)

C*** FOR EACH TARGET CALCULATE ODE OR PDE RESIDUAL AND UPDATE TARGET
C    TEMPERATURE (IF UPDATE=1 OR 1)

      DO 40 ITARG = 1, NTARG
         IF(IXTARG(TRGMETH,ITARG).NE.METHOD)GO TO 40
         WFLUXIN = XXTARG(TRGNFLUXF,ITARG)
         WFLUXOUT = XXTARG(TRGNFLUXB,ITARG)
         WSPEC(1) = XXTARG(TRGCP,ITARG)
         WRHO(1) =  XXTARG(TRGRHO,ITARG)
         WK(1) =  XXTARG(TRGK,ITARG)
         XL = XXTARG(TRGL,ITARG)
         IIMETH = IXTARG(TRGMETH,ITARG)
         IIEQ = IXTARG(TRGEQ,ITARG)

C*** COMPUTE THE PDE RESIDUAL 

         IF(IIEQ.EQ.PDE)THEN
           IF(IIMETH.EQ.MPLICIT)THEN
             TEMPIN = XXTARG(TRGTEMPF,ITARG)
             IWBOUND = 3
            ELSE
             IWBOUND = 4
           ENDIF
           DO 60 I = 1, TRGTNUM - 1
             WALLDX(I) = XL*TMP(I)
   60      CONTINUE
           NMNODE(1) = TRGTNUM
           NMNODE(2) = TRGTNUM - 2
           NSLAB = 1
           CALL CNDUCT(UPDATE,TEMPIN,TEMPOUT,DT,WK,WSPEC,WRHO,
     +      XXTARG(TRGTEMPF,ITARG),WALLDX,NMNODE,NSLAB,
     +      WFLUXIN,WFLUXOUT,IWBOUND,TGRAD,TDERV)
           IF(IIMETH.EQ.MPLICIT)THEN
             IEQ = IZTARG(ITARG)
             DELTA(NOFTT+IEQ) = XXTARG(TRGNFLUXF,ITARG)+WK(1)*TGRAD(1)
           ENDIF

C*** COMPUTE THE ODE RESIDUAL

          ELSEIF(IIEQ.EQ.ODE)THEN
            DDTEMP = (WFLUXIN+WFLUXOUT)/(WSPEC(1)*WRHO(1)*XL)
            IF(IIMETH.EQ.MPLICIT)THEN
              IEQ = IZTARG(ITARG)
              DELTA(NOFTT+IEQ) = DDTEMP - XPSOLVE(NOFTT+IEQ) 
             ELSEIF(IIMETH.EQ.XPLICIT)THEN
              IF(UPDATE.NE.0)THEN
                TTOLD = XXTARG(TRGTEMPF,ITARG)
                TTNEW = TTOLD + DT*DDTEMP
                XXTARG(TRGTEMPF,ITARG) = TTNEW
              ENDIF
            ENDIF

C*** ERROR, THE EQUATION TYPE CAN HAS TO BE EITHER PDE OR ODE
C    IF THE METHOD IS NOT STEADY

          ELSE

         ENDIF
            
   40 CONTINUE
      RETURN
      END
