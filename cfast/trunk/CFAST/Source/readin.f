      SUBROUTINE READIN (NREQ, NRET, FIXED, FLTING)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READIN
C
C     Source File: READIN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: NREQ
C                NRET
C                FIXED
C                FLTING
C
C     Revision History:
C        Created:  1/24/1996 at 14:52 by RWP
C        Modified: 1/24/1996 at 14:48 by RWP:
C                  Include parameter LBUFLN
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

C     Update history August 10, 1989 place limit on fixed number (line 33)
C                    April 24, 1991 the three versions (input, shell and model
C                                   were consolidated into this version

C     READ IN A STRING AND PROCESS IT INTO THE INTEGER AND FLOATING VARIABLES "FIXED" AND "FLTNG"

      include "precis.fi"
      include "cparams.fi"
      include "cfin.fi"
      include "cfio.fi"
      include "cshell.fi"

      DIMENSION FLTING(*)
      INTEGER FIXED(*)
      REAL X0, XXBIG
      LOGICAL MULTI, eof
      CHARACTER LABEL*5, LABLE*5, SLASH*1, FILE*(*)
      SAVE INPUT, LSTART

      DATA INPUT/0/, LSTART/0/, SLASH/'/'/

      NRET = 0
      MULTI = .FALSE.
      XXBIG = 10 000 000.0D0
      DO 4 I = 1, NREQ
         CALL SSTRNG (INBUF, COUNT, START, FIRST, LAST, VALID)
         IF (.NOT.VALID) THEN
            START = 257
            RETURN
         ENDIF
    1    DO 2 J = FIRST, LAST
            IF (INBUF(J:J).EQ.SLASH) THEN
               LLAST = J - 1
               MULTI = .TRUE.
               GO TO 3
            ENDIF
    2    CONTINUE
         LLAST = LAST
    3    CALL CONVRT (INBUF, FIRST, LLAST, TYPE, I0, X0)
         IF (TYPE.EQ.1) THEN
            FLTING(NRET+1) = X0
            FIXED(NRET+1) = IFIX (MIN (X0, XXBIG))
         ELSE IF (TYPE.EQ.2) THEN
            FIXED(NRET+1) = I0
            FLTING(NRET+1) = FLOAT(I0)
         ELSE
            IF (LOGERR.GT.0) THEN
               WRITE(LOGERR,12) FIRST,LLAST,(INBUF(J:J),J=1,50)
   12       FORMAT(' WARNING!! NON-NUMERIC DATA IN ',2I3,50A1)
            ENDIF
            NRET = NRET - 1
         ENDIF
         IF (MULTI) THEN

C     LOOP FOR A CONTINUATION

            FIRST = LLAST + 2
            MULTI = .FALSE.
            NRET = NRET + 1
            GO TO 1
         ENDIF
         COUNT = COUNT - (LAST-START+1)
         START = LAST + 1
         NRET = NRET + 1
    4 CONTINUE

      RETURN

C     READ IN A BUFFER

      ENTRY READBF(IU,LABLE, eof)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READBF
C
C     Source File: READIN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: IU
C                LABLE
C
C     Revision History:
C        Created:  1/24/1996 at 14:52 by RWP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

C     CHECK TO SEE IF THE BUFFER IS CURRENT AND FULL

      IF (IU.EQ.INPUT.AND.START.EQ.LSTART) THEN
         LABLE = LABEL
         RETURN
      ENDIF
      INPUT = IU
      CALL READAS (IU, INBUF, COUNT, START, VALID)
      IF (.NOT.VALID) GO TO 11
      CALL SSTRNG (INBUF, COUNT, START, FIRST, LAST, VALID)
      IF (.NOT.VALID) GO TO 11
      LLAST = MIN(LAST, FIRST+5)
      LABEL = ' '
      LABEL = INBUF(FIRST:LLAST)
      LABLE = LABEL
      COUNT = COUNT - (LAST-START+1)
      START = LAST + 1
      LSTART = START
	eof = .false.
	write(logerr,14) label
   14 format ('Label = ',a5)
      RETURN

   11 LABEL = '     '
      LABLE = LABEL
      START = 1
      COUNT = 0
	eof = .true.
      RETURN

C     ENTRY TO FORCE A CONTEXT SWITCH

      ENTRY READRS
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READRS
C
C     Source File: READIN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: 
C
C     Revision History:
C        Created:  1/24/1996 at 14:52 by RWP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

C     SET START TO ITS INITIAL VALUE

      START = 257
      RETURN

C     READ IN A FILE NAME

      ENTRY READFL (FILE)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READFL
C
C     Source File: READIN.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: FILE
C
C     Revision History:
C        Created:  1/24/1996 at 14:52 by RWP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      CALL SSTRNG (INBUF, COUNT, START, FIRST, LAST, VALID)
      IF (.NOT.VALID) THEN
         START = 257
         FILE = ' '
         RETURN
      ENDIF
      LENOFCH = LEN (FILE)
      LLAST = MIN(LAST, FIRST+LENOFCH)
      FILE = INBUF(FIRST:LLAST)
      COUNT = COUNT - (LAST-START+1)
      START = LAST + 1
      RETURN
      END

      SUBROUTINE READAS(INFILE, INBUF, COUNT, START, VALID)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READAS
C
C     Source File: READAS.SOR
C
C     Functional Class:  UTILITY
C
C     Description:  Read in a string from the input file, filtering out
C                   comments
C
C     Arguments: INFILE
C                INBUF
C                COUNT
C                START
C                VALID
C
C     Revision History:
C        5/16/1991 by WWJ, include the precision file PRECIS.INC
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"
C
C     READ IN A STRING
C
      INTEGER START, COUNT, IREC
      CHARACTER INBUF*(*), CMT1*1, CMT2*1, frmt*30
      LOGICAL VALID
      DATA CMT1/'!'/, CMT2/'#'/, IREC/0/
      SAVE IREC
C
C     IF WE HAVE REACHED AN END OF FILE, RETURN NOTHING
C
10    INBUF = ' '
      IF (IREC.EQ.0) THEN
         READ(INFILE,1,END=2,ERR=2) INBUF
      ELSE
         READ(INFILE,1,REC=IREC,ERR=2) INBUF
         IREC = IREC + 1
      END IF
    1 FORMAT(A)
	ls = len_trim (inbuf)
	write (frmt, 9) ls
      WRITE (LOGERR,frmt) INBUF
    9 format('(''Buffer input = '',A',i3,')')

C     FILTER COMMENTS

      IF (INBUF(1:1).EQ.CMT1.OR.INBUF(1:1).EQ.CMT2) GO TO 10

C     OK

      COUNT = LEN(INBUF)
      START = 1
      VALID = .TRUE.
      RETURN
2     VALID = .FALSE.
      COUNT = 0
      START = 1
      WRITE(LOGERR,*) 'END OF FILE FOR UNIT ',INFILE
      RETURN
      END
