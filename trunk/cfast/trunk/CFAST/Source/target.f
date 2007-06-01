      SUBROUTINE TARGET(METHOD)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     TARGET
C
C     Source File: TARGET.SOR
C
C     Functional Class:  
C
C     Description:  
C         Routine to calculate total flux striking a target.
C         This flux is used to calculate a target temperature,
C         assuming that the sum of incoming and outgoing flux is zero,
C         ie, assuming that the target is at steady state.
C
C     Arguments: METHOD
C
C     Revision History:
C     Created by GPF 9/26/94
C        Modified: 4/26/1995 gpf:
C                  expanded this routine to compute transient in addition to
C                  steady state target temperatures
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "fltarget.fi"

      LOGICAL FIRST
      DIMENSION FLUX(2), DFLUX(2), TTARG(2)
      SAVE FIRST, SIGMA

      DATA FIRST/.TRUE./

      IF(FIRST)THEN
         FIRST = .FALSE.
         SIGMA = 5.67D-8
      ENDIF

C     CALCULATE FLUX TO USER SPECIFIED TARGETS, ASSUMING TARGET IS AT THERMAL EQUILIBRIUM
      IF (NTARG.GT.NM1) THEN
        DO 10 ITARG = 1, NTARG
          METHTARG = IXTARG(TRGMETH,ITARG)
          IF(METHOD.NE.METHTARG)GO TO 10
          IROOM = IXTARG(TRGROOM,ITARG)
          IF(METHTARG.EQ.STEADY)THEN
             NITER = 10
            ELSE
             NITER = 1
          ENDIF
          TTARG(1) = XXTARG(TRGTEMPF,ITARG)
          TTARG(2) = XXTARG(TRGTEMPB,ITARG)
          DO 20 ITER = 1, NITER
            CALL TARGFLUX(ITER,ITARG,TTARG,FLUX,DFLUX)
            IF(DFLUX(1).NE.0.0D0.AND.METHTARG.EQ.STEADY)THEN
               DDIF = FLUX(1)/DFLUX(1)
               TTARG(1) = TTARG(1) - DDIF
               IF(ABS(DDIF).LE.1.0D-5*TTARG(1))GO TO 30
            ENDIF
   20     CONTINUE
   30     CONTINUE
          IF(METHTARG.EQ.STEADY)THEN
            XXTARG(TRGTEMPF,ITARG) = TTARG(1)
            XXTARG(TRGTEMPB,ITARG) = TTARG(2)
          ENDIF
          XXTARG(TRGTFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *        + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1)
          XXTARG(TRGTFLUXB,ITARG) = QTWFLUX(ITARG,2) + QTFFLUX(ITARG,2)
     *        + QTCFLUX(ITARG,2) + QTGFLUX(ITARG,2)
          CALL TARGFLUX(NITER+1,ITARG,TTARG,FLUX,DFLUX)
          XXTARG(TRGNFLUXF,ITARG) = FLUX(1)
          XXTARG(TRGNFLUXB,ITARG) = FLUX(2)
   10   CONTINUE
      END IF

C     CALCULATE FLUX TO FLOOR TARGETS FOR THE PRE-EXISTING DATA STRUCTURE, 
C     ONTARGET, AND A FLASHOVER INDICATOR ON THE FLOOR
      IF(METHOD.EQ.STEADY)THEN
      DO 40 IROOM = 1, NM1
        ITARG = NTARG - NM1 + IROOM

C     AMBIENT TARGET

        TTARG(1) = TAMB(IROOM)
        TTARG(2) = ETA(IROOM)
        XXTARG(TRGTEMPF,ITARG) = TTARG(1)
        CALL TARGFLUX(1,ITARG,TTARG,FLUX,DFLUX)
        XXTARG(TRGTFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *      + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1)
        ONTARGET(IROOM) = XXTARG(TRGTFLUXF,ITARG)-SIGMA*TTARG(1)**4

C     FLASHOVER INDICATOR

        TTARG(1) = ZZWTEMP(IROOM,2,1)
        XXTARG(TRGTEMPF,ITARG) = TTARG(1)
        CALL TARGFLUX(1,ITARG,TTARG,FLUX,DFLUX)
        XXTARG(TRGTFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *      + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1)
        XXTARG(TRGNFLUXF,ITARG) = QTWFLUX(ITARG,1) + QTFFLUX(ITARG,1)
     *      + QTCFLUX(ITARG,1) + QTGFLUX(ITARG,1) - SIGMA*TTARG(1)**4
   40 CONTINUE
      ENDIF

      RETURN
      END

