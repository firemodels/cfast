	LOGICAL FUNCTION DOESTHEFILEEXIST (CHECKFILE)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DOESTHEFILEEXIST - we are checking the existence of required files
C
C     Source File: doesthefileexist.f
C
C     Functional Class: error checking
C
C     Description:  Part of the panel project output
C
C     Revision History: new 12/16/99
C---------------------------- ALL RIGHTS RESERVED ----------------------------
      include "cparams.fi"
      include "cshell.fi"
      include "cfin.fi"

	CHARACTER (*) CHECKFILE
	INTEGER LOGERR
	LOGICAL YESORNO
	
C	"checkfile" is the file to be checked for existence
C	We look in the path specified as the datapath (path)
C	Be very careful; we are using lbuf as input as well as a string for constructing the file

	INQUIRE (FILE=CHECKFILE, EXIST=YESORNO)
	IF (YESORNO) THEN
		DOESTHEFILEEXIST = .TRUE.
	else
		doesthefileexist = .false.
	ENDIF

	RETURN

	END
