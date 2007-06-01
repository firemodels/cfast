      SUBROUTINE CPTIME(CPUTIM)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CPTIME
C
C     Source File: CPTIME.SOR
C
C     Functional Class:  Utility
C
C     Description:  ROUTINE TO CALCULATE AMOUNT OF COMPUTER
C                   TIME (CPUTIM) IN SECONDS USED SO FAR.  THIS ROUTINE 
C                   WILL GENERALLY BE DIFFERENT FOR EACH COMPUTER.
C
C     Arguments: CPUTIM
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      DOUBLE PRECISION CPUTIM
#ifdef pp_ibmpc
      INTEGER*2 HRS, MINS, SECS, HSECS
      CALL GETTIM(HRS,MINS,SECS,HSECS)
      CPUTIM = HRS * 3600 + MINS * 60 + SECS + HSECS / 100.0
#elif unix
      REAL T(2)
      XXX = ETIME(T)
      CPUTIM = T(1)
#elif mac
      CPUTIM = LONG(Z'16A') / 60.0D0
#else
      CPUTIM = 0.0D0
#endif
      RETURN
      END
