      SUBROUTINE GRES2(NNN,HVSOLV,DELTAMV,IFLAG)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GRES2
C
C     Source File: GRES2.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Calculates residuals for initial solution by SNSQE
C                   (HVAC pressure and temperature)
C
C     Arguments: NNN
C                HVSOLV
C                DELTAMV
C                IFLAG
C
C     Revision History:
C        Created:  6/14/92 by GPF
C        Modified: 2/12/1993 by GPF:
C                  Changed initial solution time from 0 to STIME for restart 
C                  case.  Added debug print
C        Modified: 2/1/97 by GPF:  added write before PAUSE so that output is
C                                  not lost
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "solvprm.fi"
      include "params.fi"
      include "opt.fi"
      include "cshell.fi"

      DIMENSION HVSOLV(*), DELTAMV(*), P2(MAXTEQ), DELTA(MAXTEQ)
      DIMENSION PDZERO(MAXTEQ)
#ifdef pp_double
      DATA PDZERO /MAXTEQ * 0.0D0/
#else
      DATA PDZERO /MAXTEQ * 0.0E0/
#endif
      DO 10 I = 1, NEQUALS
        P2(I) = PINIT(I)
   10 CONTINUE
      DO 20 I = 1, NHVPVAR
        P2(I+NOFPMV) = HVSOLV(I)
   20 CONTINUE
      DO 30 I = 1, NHVTVAR
        P2(I+NOFTMV) = HVSOLV(NHVPVAR+I)
   30 CONTINUE
      IF (IPRTALG.NE.0) THEN
        IF(NHVPVAR.GT.0)WRITE (IOFILO,*) 'HVAC PRESSURES'
        DO 40 I = 1, NHVPVAR
          WRITE (IOFILO,*) I, HVSOLV(I)
   40   CONTINUE
        IF(NHVTVAR.GT.0)WRITE (IOFILO,*) 'HVAC TEMPERATURES'
        DO 50 I = 1, NHVTVAR
          WRITE (IOFILO,*) I, HVSOLV(NHVPVAR+I)
   50   CONTINUE
      END IF
      T = STIME
      CALL RESID(T,P2,PDZERO,DELTA,IRES,RPAR2,IPAR2)
      DO 60 I = 1, NHVPVAR
        DELTAMV(I) = DELTA(I+NOFPMV)
   60 CONTINUE
      DO 70 I = 1, NHVTVAR
        DELTAMV(I+NHVPVAR) = DELTA(I+NOFTMV)
   70 CONTINUE
      IF (IPRTALG.NE.0) THEN
        WRITE (IOFILO,*) ' '
        IF(NHVPVAR.GT.0)WRITE (IOFILO,*) 'HVAC PRESSURE RESIDUALS'
        DO 80 I = 1, NHVPVAR
          WRITE (IOFILO,*) I, DELTAMV(I)
   80   CONTINUE
        IF(NHVTVAR.GT.0)WRITE (IOFILO,*) 'HVAC TEMPERATURE RESIDUALS'
        DO 90 I = 1, NHVTVAR
          WRITE (IOFILO,*) I, DELTAMV(I+NHVPVAR)
   90   CONTINUE
        WRITE(IOFILO,*)' '
        PAUSE
      END IF
      RETURN
#ifdef pp_gui
        ENTRY INGRES2
        DO 1000 I = 1, MAXTEQ
          PDZERO(I) = 0.D0
 1000 CONTINUE
        RETURN
#endif
      END
