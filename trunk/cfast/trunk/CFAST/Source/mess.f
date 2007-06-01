      SUBROUTINE MESS(P,Z)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     MESS
C
C     Source File: MESS.SOR
C
C     Functional Class:  I/O
C
C     Description:  Write a literal string of characters.  This is a write
C                   to STDOUT with a return/linefeed at the end
C
C     Arguments: PHRASE Character string to be written
C                Z      Length of the string
C
C     Revision History:
C        Created:  9/14/1993 at 14:50 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "cparams.fi"
      include "cshell.fi"

      INTEGER Z, L
      CHARACTER(len=z) P
	IF (Z.GT.2048) STOP 'Error in message handler'
      WRITE (IOFILO,'(1x,2048A1)') (P(I:I),I=1,Z)
      RETURN
C Note that both format statements have to be present
 4000 FORMAT('(1X,A',I2.2,',$)')
 5000 FORMAT('(1X,A',I2.2,',$)')
      END
C 
      SUBROUTINE MESSNRF (STRING, L)
      include "cparams.fi"
      include "cshell.fi"

      CHARACTER STRING*(*), FORMATT*100
      WRITE (FORMATT,4000) L
      WRITE (IOFILO,FORMATT) STRING(1:L)
      RETURN
 
C Note that both format statements have to be present
 4000 FORMAT('(1X,A',I2.2,',$)')
 5000 FORMAT('(1X,A',I2.2,',$)')
      END
