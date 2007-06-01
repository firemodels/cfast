      SUBROUTINE SETOBTRG (ITARG,IOBJ,IERROR)

C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SETOBTRG
C
C     Source File: SETOBTRG.SOR
C
C     Functional Class:  INPUT
C
C     Description:  Takes information from OBJECTS and sets a target
C                   for each.
C
C     Arguments: ITARG
C                IOBJ
C                IERROR  Returns error codes
C
C     Revision History:
C        Created:  8/15/1995 at 14:54 by PAR
C        Modified: 9/5/1995 at 10:26 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "objects1.fi"
      include "objects2.fi"
      include "fltarget.fi"

	IXTARG(TRGROOM,ITARG) = OBJRM(IOBJ)
	DO 10 I = 0,2
	  XXTARG(TRGCENX+I,ITARG) = OBJPOS(1+I,IOBJ)
	  XXTARG(TRGNORMX+I,ITARG) = OBJORT(1+I,IOBJ)
   10 CONTINUE
   	IXTARG(TRGWALL,ITARG) = 0
   	IXTARG(TRGMETH,ITARG) = MPLICIT
!	Using ODE because of problems with PDE
!	IXTARG(TRGEQ,ITARG) = PDE
	IXTARG(TRGEQ,ITARG) = ODE
	RETURN
	END
