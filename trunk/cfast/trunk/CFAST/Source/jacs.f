      SUBROUTINE GJAC

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GJAC
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  DUMMY ROUTINE TO KEEP THE LINKER HAPPY
C
C     Arguments: 
C
C     Revision History:
C        Created:  12/2/1992 at 11:28 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

C     DUMMY ROUTINE TO KEEP THE LINKER HAPPY

      STOP 'internal error in dassl - gjac not instantiated'
      END

      SUBROUTINE JAC

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     JAC
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  DUMMY ROUTINE TO KEEP THE LINKER HAPPY
C
C     Arguments: 
C
C     Revision History:
C        Created:  12/2/1992 at 11:28 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------


C     DUMMY ROUTINE TO KEEP THE LINKER HAPPY

      STOP 'internal error in dassl - jac not instantiated'
      END

      FUNCTION JACD()

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     JACD
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  Used by DASSL to determine how many columns of the
C                   Jacobian are going to be computed the "hard way".  
C                   This method avoids adding CFAST common blocks to DASSL.
C
C     Arguments: 
C
C     Revision History:
C        Created:  2/4/1993 at 17:13 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cparams.fi"
      include "wdervs.fi"
      JACD = JACDIM
      RETURN
      END

      SUBROUTINE SETDERV(J)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SETDERV
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description: This routine sets the value of JACCOL for use by DASSL. 
C                                                                   
C     Arguments: J       Value to be copied into JACCOL
C
C     Revision History:
C        Created:  2/14/1993 by GPF
C        Modified: 2/2/1995 by gpf  Removed IDERV set option
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "wdervs.fi"
      include "cenviro.fi"
      include "opt.fi"
C
      IF(J.GT.-10)JACCOL = J
      RETURN
      END
      SUBROUTINE INCJAC
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INJAC
C
C     Source File: JACS.SOR
C
C     Functional Class:  
C
C     Description:  Used to increment the Jacobian counter from within
C                   DASSL.  This method avoids adding CFAST common blocks 
C                   to DASSL.
C
C
C     Revision History:
C        Created:  2/14/1993 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "opt.fi"
C
      NUMJAC = NUMJAC + 1
      RETURN
      END
