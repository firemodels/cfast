      SUBROUTINE LENOCO(IV,ITOT,IFLT,IINT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     LENOCO
C
C     Source File: LENOCO.SOR
C
C     Functional Class:  CFAST
C
C     Description:  To calculation the length of the numeric common block
C
C     Arguments: IV     CFAST reduced version number (VERSION -1800) / 10
C                ITOT   Total length of the common block in storage units
C                IFLT   Length of the floating portion in numeric storage units
C                IINT   Length of the integer portion in numeric storage units
C
C     Revision History:
C        2/3/1990:  add a pseudo entry for 18.5
C        3/3/1990:  change common block size for extra hcl constants
C                   delete fuel ratios from unlabelled common
C        4/26/1990:  change the numbering system to start back at ten for CFA
C        9/10/1990:  add 8*mxoin for the object stuff
C        2/8/1992:   fixed count, at least for single precision for 1.4
C        2/20/1992:  add count for 1.5, for both double and single precision
C        9/13/1993:  added access to integer and floating counts
C        3/4/1994:   added count for detectors
C        6/30/1995:  added count for oxygen offsets
C        8/17/1995:  added count for fsm offsets and pointers and data
C                    structures for fsm.  PAR
C        7/22/1996:  added space to total for variables used
C                    by hall option
C        2/10/1997   take away space resulting from vent reduction
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"

      IINT = (LOC(ITERMXX) - LOC(NEUTRAL))/4 + 1
      IFLT = (LOC(TERMXX) - LOC(GAMMA))/4

      IFLT = IFLT + 2
      ITOT = IINT + IFLT

      RETURN
      END
