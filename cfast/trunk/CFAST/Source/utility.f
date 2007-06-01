      INTEGER FUNCTION FUNIT (IO)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FUNIT
C
C     Source File: UTILITY.SOR
C
C     Functional Class: UTILTY
C
C     Description: finds first avalable unit starting at IO
C
C     Arguments: IO
C
C     Revision History:
C        Created:  12/3/1992 at 10:37 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      PARAMETER(MXIO=32767)
      LOGICAL OPEND

      ITMP = IO-1
   10 CONTINUE
      ITMP = ITMP+1
      IF (ITMP.GT.MXIO) THEN
         FUNIT = -1
         RETURN
      END IF
      INQUIRE(UNIT=ITMP,OPENED = OPEND)
      IF (OPEND) GO TO 10
      FUNIT = ITMP
      RETURN
      END

      SUBROUTINE OPNOTPT (FILNAME, IOUNIT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OPNOTPT
C
C     Source File: UTILITY.SOR
C
C     Functional Class: UTILITY
C
C     Description: opens a file using the extension to distinguish previous 
C                  open files
C
C     Arguments: FILNAME
C                IOUNIT
C
C     Revision History:
C        Created:  12/3/1992 at 9:37 by PAR
C        Modified:  2/12/1993 by GPF to use FNDBLANK routine.
C                   Necessary because UNIX treated leading blanks
C                   as significant   
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      CHARACTER FILNAME*(*)
      INTEGER IOUNIT, FIRST, LAST
      LOGICAL EXISTED, VALID
      CHARACTER NAMEFIL*60, WORKFIL*60, FMT*14

      LENGTH = LEN (FILNAME)
      CALL SSTRNG (FILNAME, LENGTH, 1, FIRST, LAST, VALID)
      IF (.NOT.VALID) STOP 'CANNOT OPEN DEBUG FILE'
      ILEN = LAST - FIRST + 1
      NAMEFIL = ' '
      NAMEFIL(1:ILEN) = FILNAME(FIRST:LAST)

      ITMP = 0
   30 CONTINUE
      ITMP = ITMP + 1
      WRITE(FMT,10) ILEN
   10 FORMAT('(A',I2.2,',',1H','.',1H',',I3.3)')
      WRITE(WORKFIL,FMT) NAMEFIL, ITMP
      INQUIRE (FILE = WORKFIL, EXIST = EXISTED)
      IF (EXISTED) GO TO 30
      OPEN (UNIT = IOUNIT, FILE = WORKFIL,RECL=255)
      RETURN
      END
