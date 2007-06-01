      SUBROUTINE GRES3(NNN,HVPSOLV,DELTAMV,IFLAG)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GRES3
C
C     Source File: GRES3.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Calculates residuals for initial solution by SNSQE
C                   This routine finds initial upper layer temperatures,
C                   upper wall and ceiling surface temperatures
C                   in addition to room pressures and hvac pressures and
C                   temperatures.
C
C     Arguments: NNN
C                HVPSOLV
C                DELTAMV
C                IFLAG
C
C     Revision History:
C        Created:  6/23/97 by GPF
C        Modified: 5/11/98 by GPF:
C                  Implement fast startup option.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "solvprm.fi"
      include "params.fi"
      include "cshell.fi"
      include "wnodes.fi"
      include "opt.fi"

      DIMENSION HVPSOLV(*), DELTAMV(*), P2(MAXTEQ), DELTA(MAXTEQ),
     +    PDZERO(MAXTEQ)
      DATA PDZERO /MAXTEQ * 0.0D0/
      NALG = NM1 + NHVPVAR + NHVTVAR

      DO 10 I = 1, NEQUALS
        P2(I) = PINIT(I)
   10 CONTINUE

c*** copy pressures, hvac pressures and temps

      DO 20 I = 1, NALG
        P2(I) = HVPSOLV(I)
   20 CONTINUE

c*** copy upper layer temperatures in fire room

      P2(LFBO + NOFTU) = HVPSOLV(1+NALG)

c*** copy wall temperatures

      II = 0
      IEQ1 = IZWMAP2(1,LFBO)
      IEQ2 = IZWMAP2(3,LFBO)
      IF(IEQ1.NE.0)THEN
        II = II + 1
        P2(IEQ1) = HVPSOLV(II+NALG+1)
      ENDIF
      IF(IEQ2.NE.0)THEN
        II = II + 1
        P2(IEQ2) = HVPSOLV(II+NALG+1)
      ENDIF

      IF(IPRTALG.NE.0)THEN
        WRITE(IOFILO,*)' *** GUESSES ***'
        WRITE(IOFILO,*)'Room pressures'
        DO 25 I = 1, NM1
          WRITE(IOFILO,26)I,P2(I)
   26     FORMAT(1x,I3,1x,e23.16)
   25   CONTINUE
        WRITE(IOFILO,*)'HVAC pressure and temperatures'
        DO 27 I = NM1+1,NALG
          WRITE(IOFILO,26)I,P2(I)
   27   CONTINUE
        II = 1
        WRITE(IOFILO,*)'Upper Layer Temperature in Fire Room'
        WRITE(IOFILO,26)NALG+II,P2(LFBO+NOFTU)
        IEQ1 = IZWMAP2(1,LFBO)
        IEQ3 = IZWMAP2(3,LFBO)
        IF(IEQ1.NE.0.OR.IEQ3.NE.0)THEN
          WRITE(IOFILO,*)'Wall temperatures'
          IF(IEQ1.NE.0)THEN
            II = II + 1
            WRITE(IOFILO,26)NALG+II,P2(IEQ1)
          ENDIF
          IF(IEQ3.NE.0)THEN
            II = II + 1
            WRITE(IOFILO,26)NALG+II,P2(IEQ3)
          ENDIF
        ENDIF
      ENDIF
      T = STIME
      CALL RESID(T,P2,PDZERO,DELTA,IRES,RPAR2,IPAR2)
      DO 30 I = 1, NALG
        DELTAMV(I) = DELTA(I)
   30 CONTINUE
      DO 31 I = 1, NM1
        IF(.NOT.IZCON(I))DELTAMV(I) = 0.0D0
   31 CONTINUE
      DELTAMV(1+NALG) = DELTA(LFBO+NOFTU)
      II = 0
      IF(IEQ1.NE.0)THEN
        II = II + 1
        DELTAMV(II+1+NALG) = DELTA(IEQ1)
      ENDIF
      IF(IEQ2.NE.0)THEN
        II = II + 1
        DELTAMV(II+1+NALG) = DELTA(IEQ2)
      ENDIF
      IF(IPRTALG.NE.0)THEN
        WRITE(IOFILO,*)' '
        WRITE(IOFILO,*)' *** Residuals ***'
        WRITE(IOFILO,*)'Room pressure'
        DO 35 I = 1, NM1
          WRITE(IOFILO,26)I,delta(I)
   35   CONTINUE
        WRITE(IOFILO,*)'HVAC pressure and temperatures'
        DO 37 I = NM1+1,NALG
          WRITE(IOFILO,26)I,delta(I)
   37   CONTINUE
        WRITE(IOFILO,*)'Upper Layer Temperature in Fire Room'
        WRITE(IOFILO,26)NALG+II,DELTA(LFBO+NOFTU)
        IEQ1 = IZWMAP2(1,LFBO)
        IEQ3 = IZWMAP2(3,LFBO)
        IF(IEQ1.NE.0.OR.IEQ3.NE.0)THEN
          WRITE(IOFILO,*)'Wall temperatures'
          IF(IEQ1.NE.0)THEN
            II = II + 1
            WRITE(IOFILO,26)NALG+II,DELTA(IEQ1)
          ENDIF
          IF(IEQ3.NE.0)THEN
            II = II + 1
            WRITE(IOFILO,26)NALG+II,DELTA(IEQ3)
          ENDIF
        ENDIF
        WRITE(IOFILO,*)' '
        PAUSE
      ENDIF
      RETURN
#ifdef pp_lite
      ENTRY INGRES3
      DO 1000 I = 1, MAXTEQ
        PDZERO(I) = 0.0D0
 1000 CONTINUE
      RETURN
#endif
      END
