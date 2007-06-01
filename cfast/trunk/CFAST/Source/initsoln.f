      SUBROUTINE INITSOLN(T,PDOLD,PDZERO,RPAR,IPAR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITSOLN
C
C     Source File: INITSOLN.SOR
C
C     Functional Class:
C
C     Description:  This routine determines an initial solution to
C                   the zone fire modeling equations.  A non-linear
C                   algebraic solver (SNSQE) is used to calculate initial
C                   room pressures that make dP/dt zero.  If an HVAC system
C                   is modeled then HVAC node pressures and hvac duct
C                   temperatures are also determined to force mass and energy
C                   conservation.
C
C     Arguments: T
C                PDOLD
C                PDZERO
C                RPAR
C                IPAR
C
C     Revision History:
C        Created:  06/14/1992 at 12:43 by GPF
C        Modified: 12/1/1992 at 12:45 by PAR
C                  RESYNC was added so that species mass adds up to total mass
C                  at new pressure
C        Modified: 8/15/1995 at 11:13 by PAR
C                    Added initalization for flame spread dif eqs if we have a
C                    flame spread object
C        Modified: 2/5/95: GPF
C                    Ignore pressure solution for rooms that are isolated from outside.
C        Modified: 10/20/97: GPF
C                    retry solution solve if the first try used the "steady"
C                    option and it failed.
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "params.fi"
      include "solvprm.fi"
      include "opt.fi"
      include "objects1.fi"
      include "objects2.fi"

      DIMENSION IPAR(*), PDOLD(*), PDZERO(*), RPAR(*)
      EXTERNAL GRES, GRES2, GRES3
      PARAMETER (MXALG = 4*NR+MNODE+MBR)
      DIMENSION DELTAMV(MXALG), HHVP(MXALG)
      PARAMETER (LRW = (3*MXALG**2+13*MXALG)/2)
      DIMENSION WORK(LRW)
      CHARACTER*132 MESSG
      EXTERNAL GJAC


    1 CONTINUE

      CALL ROOMCON(T)

      XX0 = 0.0D0
      RPAR2(1) = RPAR(1)
      IPAR2(1) = IPAR(1)
      IPAR2(2) = IPAR(2)
      CALL SETDERV(-1)
      CALL RESID(T,P,PDZERO,PDOLD,IRES,RPAR2,IPAR2)
      IOPT = 2
      TOL = ALGTOL
      NHVALG = NHVPVAR + NHVTVAR
      NALG0 = NHVALG
      NALG1 = NM1 + NHVALG
      NPRINT = -1

c*** room pressures

      DO 10 I = 1, NM1
        HHVP(I) = P(I+NOFP)
   10 CONTINUE

c*** hvac pressures

      DO 20 I = 1, NHVPVAR
        HHVP(I+NM1) = P(I+NOFPMV)
   20 CONTINUE

c*** hvac temperatures

      DO 30 I = 1, NHVTVAR
        HHVP(I+NM1+NHVPVAR) = P(I+NOFTMV)
   30 CONTINUE

      DO 40 I = 1, NEQUALS
        PINIT(I) = P(I)
   40 CONTINUE
      IF (OPTION(FPSTEADY).EQ.1) THEN
        CALL SNSQE(GRES,GJAC,IOPT,NALG1,HHVP,DELTAMV,TOL,NPRINT,INFO,
     *      WORK,LRW)
       ELSEIF (OPTION(FPSTEADY).EQ.2) THEN
        IOFF0 = NALG1

c*** upper layer temperatures

        NALG2 = NALG1 + 1
        HHVP(1+IOFF0) = ZZFTEMP(LFBO,UPPER)

c*** wall temperatures

c*** copy wall temperatures

        II = 0
        IEQ1 = IZWMAP2(1,LFBO)
        IEQ2 = IZWMAP2(3,LFBO)
        IF(IEQ1.NE.0)THEN
          II = II + 1
          NALG2 = NALG2 + 1
          HHVP(II+IOFF0+1) = P(IEQ1)
        ENDIF
        IF(IEQ2.NE.0)THEN
          II = II + 1
          NALG2 = NALG2 + 1
          HHVP(II+IOFF0+1) = P(IEQ2)
        ENDIF

        CALL SNSQE(GRES3,GJAC,IOPT,NALG2,HHVP,DELTAMV,TOL,NPRINT,INFO,
     *      WORK,LRW)
       ELSE
        IF (NHVALG.GT.0) THEN
          CALL SNSQE(GRES2,GJAC,IOPT,NALG0,HHVP(1+NM1),DELTAMV(1+NM1),
     *               TOL,NPRINT,INFO,WORK,LRW)
         ELSE
          INFO = 1
        ENDIF
      ENDIF

C*** couldn't find a solution.  either try to recover or stop

      IF (INFO.NE.1) THEN
        IF(OPTION(FPSTEADY).NE.OFF)THEN
          OPTION(FPSTEADY) = OFF
          CALL XERROR('Trying non-steady initial guess' ,
     .              0,101,1)
          GO TO 1
        ENDIF
        CALL XERROR('Solver could not find an initial solution' ,
     .              0,102,2)
      ENDIF

C*** if a room is not connected to any other room via a horizontal or
C    vertical vent then do not use the SNSQE pressure solution,
C    use the original pressure solution that was based on rho*g*h.

      DO 50 I = 1, NM1
        IF(IZCON(I))P(I+NOFP) = HHVP(I)
   50 CONTINUE
      DO 60 I = 1, NHVPVAR
        P(I+NOFPMV) = HHVP(I+NM1)
   60 CONTINUE
      DO 70 I = 1, NHVTVAR
        P(I+NOFTMV) = HHVP(I+NM1+NHVPVAR)
   70 CONTINUE
      IF (OPTION(FPSTEADY).EQ.2) THEN
        P(LFBO+NOFTU) = HHVP(1+IOFF0)
        II = 0
        IF(IEQ1.NE.0)THEN
          II = II + 1
          P(IEQ1) = HHVP(II+IOFF0+1)
        ENDIF
        IF(IEQ2.NE.0)THEN
          II = II + 1
          P(IEQ2) = HHVP(II+IOFF0+1)
        ENDIF
      ENDIF
      IF (FSMTYPE.GT.0) THEN
        DO 75 I = 1, NUMOBJL
          IF (OBJTYP(I).EQ.3) THEN
            P(NOFFSM+1+(I-1)*5) = 0.0D0
            P(NOFFSM+2+(I-1)*5) = 0.0D0
            P(NOFFSM+3+(I-1)*5) = 0.0D0
            P(NOFFSM+4+(I-1)*5) = 0.0D0
            P(NOFFSM+5+(I-1)*5) = objqarea
          END IF
   75   CONTINUE
      END IF
      CALL RESID(T,P,PDZERO,PDOLD,IRES,RPAR,IPAR)

C     Added to resync the species mass with the total mass of each layer at
C     the new pressure  12/01/92
C
      NODES = NOFPRD+1
      CALL RESYNC(P,NODES)
      DO 80 I = 1, NHVPVAR
        PDOLD(I+NOFPMV) = XX0
   80 CONTINUE
      DO 90 I = 1, NHVTVAR
        PDOLD(I+NOFTMV) = XX0
   90 CONTINUE
      DO 100 I = 1, NWALLS
        PDOLD(I+NOFWT) = XX0
  100 CONTINUE
      RETURN
      END
