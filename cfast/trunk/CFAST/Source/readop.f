      SUBROUTINE READOP
      use ifport

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READOP
C
C     Description:  retrieve and process command line options and date
C
C     Arguments: none
C
C     Revision History:
C        Written:  7/26/1990 by WWJ: implement date on UNIX and PC
C        Modified: 2/8/1993 RDP:
C                  Rewritten to to command parsing format to remove bug when 
C                  options and filename were not separated with a space.
C
C        Modified: 10/21/97 GPF:
C                  added -s option to create a "backdoor" solver input file
C                  aka SOLVER.INI .  note that the default name assigned by
C                  this option is SOLVE.INI not SOLVER.INI so as to not overwrite
C                  this file if already present.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
!
!	Unit numbers defined in readop, openoutputfiles, readinputfiles
!
!      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
!      3 is for the log file  (LOGERR)
!      6 is output (IOFILO)
!     11 is the history file
!     12 is used to write the status file (project.status)
!     13 smokeview output
!     14 spreadsheet output
!
      include "cparams.fi"
      include "cshell.fi"

      INTEGER (2) YEAR, MONTH, DAY
      LOGICAL EXISTS
      CHARACTER STRS(8)*60, IC, TOUPPER*1, LOGFILE*60
      CHARACTER*60 SOLVEINI
      INTEGER IARG(8), IOPT(26), OPTION
      OPTION(IC) = IOPT(ICHAR(IC)-ICHAR('A')+1)

C     CURRENT DATE

      CALL GETDAT(YEAR,MONTH,DAY)
      RUNDAT(3) = DAY
      RUNDAT(2) = MONTH
      RUNDAT(1) = YEAR
      WRITE (MPSDATC,5010) RUNDAT(1), RUNDAT(2), RUNDAT(3)

C     COMMAND LINE ARGUMENTS

      NARGS = 8
      CALL CMDLINE(NARGS,STRS,IARG,IOPT)

!     OPTIONS
!     K = DO NOT ACCESS KEYBOARD
!     F/C = OUTPUT OPTIONS
!     S = Output "SOLVER.INI" options into the file SOLVE.INI
!	I = do initialization only
!     H to include the header in the output file
!     D to turn on debugging writes
!     T to output trace species mass

      IF (OPTION('H').NE.0) HEADER = .TRUE.
      IF (OPTION('K').NE.0) NOKBD = .TRUE.
	IF (OPTION('I').NE.0) INITIALIZEONLY = .TRUE.
	IF (OPTION('D').NE.0) DEBUGGING = .TRUE.
	if (option('T').ne.0) trace = .true.
      LOGERR = 3

      IF (OPTION('F').ne.0.and.option('C').ne.0) stop 107
	if (option('C').ne.0) outputformat = 1
	if (option('F').ne.0) outputformat = 2

      IF (OPTION('S').NE.0) THEN
         IF (STRS(OPTION('S')).NE.' ') THEN
            SOLVEINI = STRS(OPTION('S'))
         ELSE
            SOLVEINI = 'SOLVE.INI'
         ENDIF
         CALL WRITEINI(SOLVEINI)
      ENDIF

      RETURN

 5010 FORMAT (I4.4,'/',I2.2,'/',I2.2)
      END
