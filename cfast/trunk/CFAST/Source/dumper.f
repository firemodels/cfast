      SUBROUTINE DUMPER(ISTEP,IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DUMPER
C
C     Source File: DUMPER.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Saves the data files in packed binary format
C
C     Arguments: ISTEP  Current time step
C                IERROR Returns error code
C
C     Revision History:
C        8/5/1993  by RDP, change file opens to remove machine dependence of
C                          history file
C        3/11/1987 by WWJ, change from LOCATE to LENOCO to find COMMON size
C        2/22/1989 by WWJ, increase speed of write with implied array write
C        7/26/1990 by WWJ, changed file specifications to ASCII from ASCIIZ
C        4/28/1995 by GPF, removed unused FORMAT, 5010
C        Modified: 9/5/1995 at 9:29 by PAR:
C                  Added support for IERROR and returning errors and stops to 
C                  main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cfin.fi"
      include "iofiles77.fi"

      INTEGER IOUNIT, ITOT, OUTPUT(4096), ld
      LOGICAL FIRSTC, EXISTS
      EQUIVALENCE (GAMMA,OUTPUT)
      DATA IOUNIT /11/, FIRSTC /.TRUE./
      SAVE ITOT, FIRSTC

      IF (NDUMPR.EQ.0) stop 106
 
      TERMXX = ITMSTP - 1
      ITERMXX = ITMSTP - 1
      CALL LENOCO(version/10,ITOT,IFLT,IINT)
      CALL WRITEOT(OUTPUT,ITOT,IOUNIT,IERR,version)
      IF (IERR.EQ.0) THEN
        if (debugging) WRITE (LOGERR,5020) ISTEP, ITOT * 4
        RETURN
      END IF
	 
C     ERROR PROCESSING
 
   10 WRITE (LOGERR,5030) MOD(ierr,256), historyfile
      STOP

 5020 FORMAT ('Write to the history file at',I5,I7)
 5030 FORMAT ('From dumper, error in accessing history file, error = ',I5,
     +    /,' File = ',A256)
	END
