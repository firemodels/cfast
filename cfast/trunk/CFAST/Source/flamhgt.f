      SUBROUTINE FLAMHGT (IROOM, XFQF, XFAREA, FHEIGHT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FLAMHGT
C
C     Description:  Calculates flame height for a given fire size and area
C
C     Arguments: IROOM   Compartment number where fire is located
C                XFQF    Fire Size (W)
C                XFAREA  Area of the base of the fire (m^2)
C                FHEIGHT Calculated flame height (m)
C
C     Source: SFPE handbook, Section 2, Chapter 1
C
C     Revision History:
C        Created:  3/6/1997 at 13:45 by RDP
!        Modified  7/11/2005 by wwj to change from Heskestaad equation 2 to equation 8 in the third edition
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      CHARACTER STR*10
      ZERO = 0.0D0
      FOUR = 4.0D0
      PI = 3.14159
      IF (XFAREA.LE.0) THEN
        D = 0.3
      ELSE
        D = SQRT(FOUR*XFAREA/PI)
      END IF
	fheight = -1.02*d + 0.235*(xfqf/1.0d+3)**0.4
      fheight = max (zero, fheight)
      RETURN
      END
