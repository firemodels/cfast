	SUBROUTINE SETP0(P0, IP0, PMXMN, IPMXMN, iounit, IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SETP0
C
C     Source File: SETP0.SOR
C
C     Functional Class:  
C
C     Description:  
C
C      Arguments: P0      Array containing new set values for p vector
C                 IP0     Array of flags for variables that have changes in P0
C                 IERROR  Error flag
C
C     Revision History:
C        Created:  1/28/1998 at 9:57 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cfin.fi"
      include "opt.fi"
      include "iofiles77.fi"

      DIMENSION P0(*), IP0(0:*)
      DIMENSION PMXMN(MAXTEQ,2), IPMXMN(0:MAXTEQ,2)
      INTEGER FUNIT, iounit
      CHARACTER LABEL*5, TESTFILE*128, PLACE*1, MXMN*1, TOUPPER*1
	character testpath*256
      INTEGER ILOCAL(2)
	logical exists, doesthefileexist, eof

      DOUBLE PRECISION LOCAL(2)

      IP0(0) = OFF
      IERROR = 0
      DO 10 I = 1, MAXTEQ
      	P0(I) = 0.D0
	IP0(I) = OFF
   10 CONTINUE

      IF (SETPFILE.EQ.'   ') RETURN

! First we try the local directory

	testpath = trim (datapath) // trim(setpfile)
	exists = DoesTheFileExist(testpath)

! Then we try the executable root directory

	if (.not.exists) then
	  testpath = trim (exepath) // trim(setpfile)
	  exists = DoesTheFileExist(testpath)

! All is lost

	  if (.not.exists) then
			write (logerr, 5000) trim(setpfile)
			errorcode = 217
			return
	  endif
	endif

! All is not lost

	write (logerr, 5001) testpath
	
	close (iounit)
	open (unit=iounit,file=testpath,form='formatted')

      CALL READBF(IO, LABEL, eof)
      DO 30 I = 1, 5
        LABEL(I:I) = TOUPPER(LABEL(I:I))
   30 CONTINUE
      IF (LABEL(1:4).NE.'FILE') THEN
		IERROR = 75
		CLOSE(IO)
		RETURN
      END IF
      CALL READFL(TESTFILE)
      IF (TESTFILE.NE.NNFILE) THEN
        IERROR = 50
	CLOSE(IO)
	RETURN
      END IF

   20 CONTINUE

      CALL READBF(IO, LABEL, eof)
      DO 40 I = 1, 5
          LABEL(I:I) = TOUPPER(LABEL(I:I))
   40 CONTINUE

      CALL READIN(2,NRET,ILOCAL,LOCAL)
		IROOM = ILOCAL(1)
		X = LOCAL(2)
      CALL READFL(PLACE)
		PLACE = TOUPPER(PLACE)
      CALL READFL(MXMN)
		MXMN = TOUPPER(MXMN)
	
	IF (LABEL(1:4).EQ.'TEMP') THEN
	  IF (PLACE.EQ.'U') THEN
	    CALL DOP0(NOFTU,IROOM,MXMN,X,P0,IP0,PMXMN,IPMXMN)
	  ELSE IF (PLACE.EQ.'L') THEN
	    CALL DOP0(NOFTL,IROOM,MXMN,X,P0,IP0,PMXMN,IPMXMN)
	  ELSE
		 write(logerr,*) 'Parameter not supported by SETP'
		 ierror = 77
		 CLOSE(IO)
		 return
	  END IF
	ELSE IF (LABEL.EQ.'PRESS') THEN
		MXMN = PLACE
		CALL DOP0(NOFP,IROOM,MXMN,X,P0,IP0,PMXMN,IPMXMN)
	ELSE IF (LABEL.EQ.'INTER') THEN
		MXMN = PLACE
		X = (HR(IROOM) - X)*AR(IROOM)
		CALL DOP0(NOFVU,IROOM,MXMN,X,P0,IP0,PMXMN,IPMXMN)
	ELSE
		CLOSE(IO)
		RETURN
	END IF

      GOTO 20

 1000 CONTINUE
      write(logerr,*)'Error Reading the "SETP" file'
      IERROR = 76
      RETURN
 5000 format ('Cannot find the object fire file in either the' 
     . ' executable path or the local directory ',/,a)
 5001 format ('Open the SETPARAMETER file ',a)

      END

      SUBROUTINE DOP0(NOFLG, IROOM, MXMN, X, P0, IP0, PMXMN, IPMXMN)
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DOP0
C
C     Source File: SETP0.SOR
C
C     Functional Class:  
C
C     Description:  
C
C      Arguments: NOFLG   Index into the P vector
C                 IROOM   Index of room
C                 MXMN    Character flag of max or min of value
C                 P0      Array containing new set values for p vector
C                 IP0     Array of flags for variables that have changes in P0
C                 PMXMN   Array containing new limits of values for p vector
C                 IPMXMN  Array of flags for limits that have
C                         been set in PMXMN
C
C     Revision History:
C        Created:  1/28/1998 at 9:57 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cfin.fi"
      include "opt.fi"

      DIMENSION P0(*), IP0(0:*)
      DIMENSION PMXMN(MAXTEQ,2), IPMXMN(0:MAXTEQ,2)
      CHARACTER MXMN*1

      IF (MXMN.EQ.'X') THEN
        IPMXMN(0,1) = ON
        PMXMN(NOFLG+IROOM,1) = X
        IPMXMN(NOFLG+IROOM,1) = ON
      ELSE IF (MXMN.EQ.'M') THEN
        IPMXMN(0,2) = ON
        PMXMN(NOFLG+IROOM,2) = X
        IPMXMN(NOFLG+IROOM,2) = ON
      ELSE
        IP0(0) = ON
        P0(NOFLG+IROOM) = X
        IP0(NOFLG+IROOM) = ON
      END IF

      RETURN
      END

