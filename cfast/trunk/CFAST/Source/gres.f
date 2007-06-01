      SUBROUTINE GRES(NNN,HVPSOLV,DELTAMV,IFLAG)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GRES
C
C     Source File: GRES.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Calculates residuals for initial solution by SNSQE
C
C     Arguments: NNN
C                HVPSOLV
C                DELTAMV
C                IFLAG
C
C     Revision History:
C        Created:  6/14/92 by GPF
C        Modified: 2/5/1993 at 13:18 by RDP:
C                  Changed initial solution time from 0 to STIME for restart 
C                  case.
C        Modified: 2/12/1993 by GPF: added debug print
C        Modified: 2/5/95 by GPF:  ignore rooms that are isolated from outside
C                                  (set pressure equation residual to zero)
C        Modified: 2/1/97 by GPF:  added write before PAUSE so that output is
C                                  not lost
C        Modified: 10/9/97 by GPF: improved debug printout
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "solvprm.fi"
      include "params.fi"
      include "cshell.fi"
      include "opt.fi"

      DIMENSION HVPSOLV(*), DELTAMV(*), P2(MAXTEQ), DELTA(MAXTEQ), 
     +    PDZERO(MAXTEQ)
#ifdef pp_double
      DATA PDZERO /MAXTEQ * 0.0D0/
#else
      DATA PDZERO /MAXTEQ * 0.0E0/
#endif
      NALG = NM1 + NHVPVAR + NHVTVAR
      DO 10 I = 1, NALG
        P2(I) = HVPSOLV(I)
   10 CONTINUE
      DO 20 I = NALG + 1, NEQUALS
        P2(I) = PINIT(I)
   20 CONTINUE
      IF(IPRTALG.NE.0)THEN
        WRITE(IOFILO,*)'Room pressures'
        DO 25 I = 1, NM1
          WRITE(IOFILO,*)I,P2(I)
   25   CONTINUE
        IF(NHVPVAR.GT.0)WRITE (IOFILO,*) 'HVAC pressures'
        DO 26 I = 1, NHVPVAR
          WRITE(IOFILO,*)I,P2(I+NOFPMV)
   26   CONTINUE
        IF(NHVTVAR.GT.0)WRITE (IOFILO,*) 'HVAC temperatures'
        DO 27 I = 1, NHVTVAR
          WRITE(IOFILO,*)I,P2(I+NOFTMV)
   27   CONTINUE
      ENDIF
      T = STIME
      CALL RESID(T,P2,PDZERO,DELTA,IRES,RPAR2,IPAR2)
      DO 30 I = 1, NALG
        DELTAMV(I) = DELTA(I)
   30 CONTINUE
      DO 31 I = 1, NM1
        IF(.NOT.IZCON(I))DELTAMV(I) = 0.0D0
   31 CONTINUE
      IF(IPRTALG.NE.0)THEN
        WRITE(IOFILO,*)'Room pressure Residuals'
        DO 35 I = 1, NM1
          WRITE(IOFILO,*)I,DELTA(I)
   35   CONTINUE
        IF(NHVpvar.GT.0)WRITE (IOFILO,*) 'HVAC pressure Residuals'
        DO 36 I = 1, NHVPVAR
          WRITE(IOFILO,*)I,DELTA(I+NOFPMV)
   36   CONTINUE
        IF(NHVTVAR.GT.0)WRITE (IOFILO,*) 'HVAC temperature Residuals'
        DO 37 I = 1, NHVTVAR
          WRITE(IOFILO,*)I,DELTA(I+NOFTMV)
   37   CONTINUE
        WRITE(IOFILO,*)' '
        PAUSE
      ENDIF
      RETURN
#ifdef pp_gui
      ENTRY INGRES
      DO 1000 I = 1, MAXTEQ
        PDZERO(I) = 0.0D0
 1000 CONTINUE
      RETURN
#endif
      END
