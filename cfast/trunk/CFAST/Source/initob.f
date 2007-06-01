      SUBROUTINE INITOB
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITOB
C
C     Source File: INITOB.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Initialize object data
C
C     Arguments: none
C
C     Revision History:
C        Created:  3/12/1993 at 9:34 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "objects1.fi"
      include "objects2.fi"
C
C     TURN OFF OBJECTS
C
      NUMOBJL = 0
	DO 10 I = 0, MXOIN
        OTFIRET(I) = 0.0
        OBJON(I) = .FALSE.
        OBJPOS(1,I) = -1.0
        OBJPOS(2,I) = -1.0
        OBJPOS(3,I) = -1.0
        OBJRM(I) = 0
        OBJNIN(I) = ' '
        OBJLD(I) = .FALSE.
        OBJPNT(I) = 0
        OBJCRI(1,I) = 0.0
        OBJCRI(2,I) = 0.0
        OBJCRI(3,I) = 0.0
        OBJDEF(I) = .FALSE.
        ODBNAM(I) = ' '
   10 CONTINUE
      RETURN
      END
