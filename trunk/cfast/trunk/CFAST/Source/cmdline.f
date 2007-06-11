      SUBROUTINE CMDLINE (NARGS,STRS,IARG,IOPT)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CMDLINE
C
C     Source File: CMDLINE.SOR
C
C     Functional Class:  Utility
C
C     Description:  Gets argument list and options from command line.
C                   Options may be of the form C:<string> where <C> is
C                   the desired option A-Z and <string> is a character
C                   string associated with that option.
C
C     Arguments: NARGS Maximum number of arguments expected (dimension limit
C                      on input, actual number on output.
C                STRS  Returned strings of arguments and options
C                IARG  Returned list of pointers to elements in STRS
C                      corresponding to arguments 1..NARGS
C                IOPT  Returned list of pointers to elements in STRS
C                      corresponding to options A-Z
C
C     Revision History:
C        Created:  01/26/90 by WWJ
C        Revised:   2/??/91 by WWJ to add unix variant
C        Revised:   2/19/93 by the group
C        Revised:  11/11/92 by RDP for Watcom
C        Revised:  4/28/95  by GPF to eliminate COUNT and ICOUNT; declared but unused
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
      CHARACTER STRS(NARGS)*(*), CMDLIN*127, OPTSEP
      INTEGER IARG(NARGS), IOPT(26)

 	OPTSEP = '/'

      DO 10 IC = 1, MAX0(NARGS,26)
        IF (IC.LE.NARGS) THEN
          STRS(IC) = ' '
          IARG(IC) = 0
        END IF
        IOPT(IC) = 0
   10 CONTINUE

C   GET THE COMMAND LINE TO DECIPHER
 
      CALL GETCL(CMDLIN)
      IF (CMDLIN.NE.' ') THEN
	 
C   GET RID OF EXTRA SPACES IN THE COMMAND LINE
 
        IC = 1
   20   IF (CMDLIN(IC:IC+1).EQ.'  ') THEN
          CALL CMOVE(CMDLIN,IC,126,IC+1,127,127,' ')
        ELSE
          IC = IC + 1
        END IF
        IF (CMDLIN(IC:127).NE.' '.AND.IC.LE.126) GO TO 20
        IF (CMDLIN(1:1).EQ.' ') THEN
          CALL CMOVE(CMDLIN,1,126,2,127,127,' ')
        END IF
 
C   PUT IN COMMAS WHERE APPROPRIATE TO DELIMIT ALL FIELDS
 
        IC = 2
   30   IF (CMDLIN(IC:IC).EQ.' ') THEN
          IF (CMDLIN(IC-1:IC-1).NE.','.AND.CMDLIN(IC+1:IC+1).NE.',') 
     +        THEN
            CMDLIN(IC:IC) = ','
            IC = IC + 1
          ELSE
            CALL CMOVE(CMDLIN,IC,126,IC+1,127,127,' ')
          END IF
        ELSE IF ((CMDLIN(IC:IC).EQ.OPTSEP).AND.CMDLIN(IC-1:IC-1).NE.',')
     +      THEN
          CALL CMOVE(CMDLIN,IC+1,127,IC,126,IC,',')
          IC = IC + 2
        ELSE
          IC = IC + 1
        END IF
        IF (CMDLIN(IC:127).NE.' '.AND.IC.LE.126) GO TO 30
      END IF
 
C   PARSE COMMAND LINE INTO SEPARATE FIELDS AND PROCESS OPTIONS
 
      IA = 0
   40 IC = INDEX(CMDLIN,',')
      IF (IC.EQ.0.AND.CMDLIN.NE.' ') IC = INDEX(CMDLIN,' ')
      IF (IC.NE.0) THEN
        IA = IA + 1
        STRS(IA) = ' '
        IF (IC.GT.1) STRS(IA) = CMDLIN(1:IC-1)
        CALL CMOVE(CMDLIN,1,127,IC+1,127,127,' ')
        GO TO 40
      END IF
 
C   ASSIGN THE PARSED FIELDS TO APPROPRIATE ARGUMENTS AND OPTIONS
 
      NARGS = 0
      IF (IA.GT.0) THEN
        DO 50 I = 1, IA
          IF (STRS(I)(1:1).EQ.OPTSEP) THEN
            IF (STRS(I)(2:2).GE.'A'.AND.STRS(I)(2:2).LE.'Z') THEN
              IOPT(ICHAR(STRS(I)(2:2))-ICHAR('A')+1) = I
            ELSE IF (STRS(I)(2:2).GE.'a'.AND.STRS(I)(2:2).LE.'z') THEN
              IOPT(ICHAR(STRS(I)(2:2))-ICHAR('a')+1) = I
            END IF
            CMDLIN = STRS(I)
            CALL CMOVE(CMDLIN,1,127,3,127,127,' ')
            IF (CMDLIN(1:1).EQ.':') CALL 
     +          CMOVE(CMDLIN,1,127,2,127,127,' ')
            STRS(I) = CMDLIN
          ELSE
            NARGS = NARGS + 1
            IARG(NARGS) = I
          END IF
   50   CONTINUE
      END IF
      RETURN
      END

      SUBROUTINE CMOVE(CMDLIN,I1,I2,I3,I4,I5,CHR)
 
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CMOVE
C
C     Source File: CMDLINE.FOR
C
C     Functional Class:  Utility
C
C     Description:  Move a substring in the command line to remove spaces.
C
C     Arguments: CMDLIN Command line string
C                I1     Beginning of substring to be moved
C                I2     End of substring to be moved
C                I3     Beginning of destination
C                I4     End of destination
C                I5     Position of newly vacated space in the string
C                CHR    Character to fill that space
C
C     Revision History:
C        Created:  2/8/1993 at 15:31 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
 
      CHARACTER TEMP*127, CMDLIN*(*), CHR
      TEMP = CMDLIN
      TEMP(I1:I2) = CMDLIN(I3:I4)
      TEMP(I5:I5) = CHR
      CMDLIN = TEMP
      RETURN
      END

      SUBROUTINE GETCL(CMDLIN)
      use ifport

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     GETCL
C
C     Source File: CMDLINE.SOR
C
C     Functional Class: UTILITY
C
C     Description: Get command line as a single string on UNIX machines
C
C     Arguments: CMDLIN  Command line
C
C     Revision History:
C        Created:  11/16/1992 at 10:06 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "cfin.fi"
      CHARACTER CMDLIN*127
      INTEGER FIRST, LAST, LPOINT
      LOGICAL VALID
C
      MAXARG = 5 + 2
      LPOINT = 0
      IAR = IARGC()
      IF (IAR.EQ.0) THEN
        CMDLIN = ' '
      ELSE
        CMDLIN = ' '
        DO 10 I = 1, MIN(IAR,MAXARG)
          CALL GETARG(I,LBUF)
          CALL SSTRNG(LBUF,60,1,FIRST,LAST,VALID)
          IF (VALID) THEN
            IC = LAST - FIRST + 1
            LPOINT = LPOINT + 1
            CMDLIN(LPOINT:LPOINT+IC) = LBUF(FIRST:LAST)
            LPOINT = LPOINT + IC
          END IF
   10   CONTINUE
      END IF
      RETURN
      END


