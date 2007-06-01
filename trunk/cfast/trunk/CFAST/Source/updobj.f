	SUBROUTINE UPDOBJ(IFLAG, TOLD, DT, IFOBJ, TOBJ, IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPDOBJ
C
C     Source File: UPDOBJ.f
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: IFLAG   Flags if check, set, or update variables
C                TOLD    Time previous to this time step
C                DT      Length of last time step
C                IFOBJ   Object number that ignites (return)
C                TOBJ    Time object ignites
C                IERROR  Returns error codes
C
C     Revision History:
C        Created:  8/15/1995 at 13:08 by PAR
C        Modified: 9/5/1995 at 10:29 by PAR:
C                  Added support for IERROR and returns of stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "fltarget.fi"
      include "opt.fi"

	DIMENSION TMPOB(2,MXOIN)

	IFOBJ = 0
	TOBJ = TOLD + 2.D0*DT
	TNOBJ = TOLD + DT

!!!!! Note that ignition type 1 is time, type 2 is temperature and 3 is flux !!!
!!!!! The critiria for temperature and flux are stored backupwards - this historical
!!!!! See corresponding code in keywordcases
	DO 10 IOBJ = 1, NUMOBJL
	  IF (OBJON(IOBJ)) GOTO 10
	  IGNFLG = OBJIGN(IOBJ)
	  IOBTARG = OBTARG(IOBJ)
	  IF (IGNFLG.EQ.1) THEN
	    IF (OBJCRI(1,IOBJ).LE.TNOBJ) THEN
	      TOBJ = MIN(OBJCRI(1,IOBJ),TOBJ)
		    IFOBJ = IOBJ
	      TMPOB(1,IOBJ) = 1.D0
	      TMPOB(2,IOBJ) = OBJCRI(1,IOBJ)
	    ELSE
	      TMPOB(1,IOBJ) = 0.0D0
	      TMPOB(2,IOBJ) = TNOBJ + DT
	    END IF
	  ELSE IF (IGNFLG.EQ.2) THEN
	    CALL DO_OBJCK(IFLAG, TOLD, DT, XXTARG(TRGTEMPF,IOBTARG), 
     .         OBJCRI(3,IOBJ), OBCOND(OBOTEMP,IOBJ), IOBJ, IFOBJ, TOBJ,
     .         TMPOB(1,IOBJ))
          ELSE IF (IGNFLG.EQ.3) THEN
	    CALL DO_OBJCK(IFLAG, TOLD, DT, XXTARG(TRGTFLUXF,IOBTARG), 
     .         OBJCRI(2,IOBJ), OBCOND(OBOFLUX,IOBJ), IOBJ, IFOBJ, TOBJ,
     .         TMPOB(1,IOBJ))
        ELSE
          CALL XERROR('UPDOBJ - Incorrectly defined object type',0,1,1)
          IERROR = 20
          RETURN
	  ENDIF
   10 CONTINUE
	    
	IF (IFLAG.NE.MDCHK) THEN
	  DO 20 IOBJ = 1, NUMOBJL
	    IF (.NOT.OBJON(IOBJ)) THEN
	      IOBTARG = OBTARG(IOBJ)
	      OBCOND(OBOTEMP,IOBJ) = XXTARG(TRGTEMPF,IOBTARG)
	      OBCOND(OBOFLUX,IOBJ) = XXTARG(TRGTFLUXF,IOBTARG)
	      IF (IFLAG.EQ.MDSET.AND.TMPOB(1,IOBJ).GT.0.0D0) THEN
	        IF (TMPOB(2,IOBJ).LE.TOBJ) THEN
		       OBJON(IOBJ) = .TRUE.
                IF (OPTION(FBTOBJ).EQ.ON) THEN
		          OBJSET(IOBJ) = 1
                ELSE
                   OBJSET(IOBJ) = 0
                END IF
		       OBJCRI(1,IOBJ) = TMPOB(2,IOBJ)
		      END IF
	      END IF
	    END IF
   20   CONTINUE
      END IF

	RETURN
	END

	SUBROUTINE DO_OBJCK(IFLAG,TOLD, DT, COND, TRIP, OLDCOND, IOBJ,
     .                    IFOBJ, TOBJ, TMPOB)

      include "precis.fi"

     	DIMENSION TMPOB(2)

      IF (COND.GT.TRIP) THEN
	  DELTA = (TRIP-OLDCOND)/(COND-OLDCOND)
	  TMPOB(1) = 1.0D0
	  TMPOB(2) = TOLD + DT*DELTA
	  TOBJ = MIN(TOBJ,TMPOB(2))
	  IFOBJ = IOBJ
	ELSE
	  TMPOB(1) = 0.0D0
	  TMPOB(2) = TOLD + 2.D0*DT
	END IF

	RETURN
	END
