      SUBROUTINE XERROR(MESSG,NMESSG,NERR,LEVEL)
C
C     Routine:     XERROR - This routine, a standard error
C        handling routine was modified for use with CFAST,
C        ie in addition to printing out error messages,
C        it also prints out to CFAST's log file.
C
C     Revision History:
C        Adapted:  gpf
C        Modified by GPF 10/10/94
C               Fixed subscript index problem with NMESSG=0
C
C***BEGIN PROLOGUE  XERROR
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  870930   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Processes an error (diagnostic) message.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        XERROR processes a diagnostic message. It is a stub routine
C        written for the book above. Actually, XERROR is a sophisticated
C        error handling package with many options, and is described
C        in the reference below. Our version has the same calling sequence
C        but only prints an error message and either returns (if the
C        input value of ABS(LEVEL) is less than 2) or stops (if the
C        input value of ABS(LEVEL) equals 2).
C
C     Description of Parameters
C      --Input--
C        MESSG - the Hollerith message to be processed.
C        NMESSG- the actual number of characters in MESSG.
C                (this is ignored in this stub routine)
C        NERR  - the error number associated with this message.
C                NERR must not be zero.
C                (this is ignored in this stub routine)
C        LEVEL - error category.
C                =2 means this is an unconditionally fatal error.
C                =1 means this is a recoverable error.  (I.e., it is
C                   non-fatal if XSETF has been appropriately called.)
C                =0 means this is a warning message only.
C                =-1 means this is a warning message which is to be
C                   printed at most once, regardless of how many
C                   times this call is executed.
C                 (in this stub routine
C                       LEVEL=2 causes a message to be printed and then a
C                                         stop.
C                       LEVEL=-1,0,1 causes a message to be printed and then a
C                                         return.
C                       LEVEL=-2 message is only printed to log file
C                                (if logerr.ne.0)
C
C     Examples
C        CALL XERROR('SMOOTH -- NUM WAS ZERO.',23,1,2)
C        CALL XERROR('INTEG  -- LESS THAN FULL ACCURACY ACHIEVED.',
C                    43,2,1)
C        CALL XERROR('ROOTER -- ACTUAL ZERO OF F FOUND BEFORE INTERVAL F
C    1ULLY COLLAPSED.',65,3,0)
C        CALL XERROR('EXP    -- UNDERFLOWS BEING SET TO ZERO.',39,1,-1)
C
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C   MODIFICATION HISTORY
C 
C     10/14/93   ADAPTED FOR USE WITH CFAST (WRITE OUT TO LOG FILE) GPF
C
C***ROUTINES CALLED  XERRWV
C***END PROLOGUE  XERROR
      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"
      CHARACTER*(*) MESSG
C***FIRST EXECUTABLE STATEMENT  XERROR
      XX0 = 0.0D0
      IF(NMESSG.EQ.0)THEN
        NMESS = LENG(MESSG)
       ELSE
        NMESS = NMESSG
      ENDIF
      NMESS = MAX(1,NMESS)
      IF(LOGERR.GT.0 .AND. LEVEL.NE.0) WRITE(LOGERR,*) MESSG(1:NMESS)
      IF(LEVEL.NE.-2)THEN
         CALL XERRWV(MESSG,NMESS,NERR,LEVEL,0,0,0,0,XX0,XX0)
      ENDIF
      RETURN
C***TAKEN FROM KAHANER LIBRARY, BILL MOSS 2-91
      END
      SUBROUTINE XERRWV(MSG,NMES,NERR,LEVEL,NI,I1,I2,NNR,R1,R2)
      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"
      INTEGER NMES, NERR, LEVEL, NI, I1, I2, NNR
      DOUBLE PRECISION R1, R2
      CHARACTER*1 MSG(NMES)
C-----------------------------------------------------------------------
C Subroutine XERRWV, as given here, constitutes a simplified version of
C the SLATEC error handling package.
C Written by A. C. Hindmarsh and P. N. Brown at LLNL.
C Modified 1/8/90 by Clement Ulrich at LLNL.
C Version of 8 January, 1990.
C
C All arguments are input arguments.
C
C MSG    = The message (character array).
C NMES   = The length of MSG (number of characters).
C NERR   = The error number (not used).
C LEVEL  = The error level..
C          0 or 1 means recoverable (control returns to caller).
C          2 means fatal (run is aborted--see note below).
C NI     = Number of integers (0, 1, or 2) to be printed with message.
C I1,I2  = Integers to be printed, depending on NI.
C NNR     = Number of reals (0, 1, or 2) to be printed with message.
C R1,R2  = Reals to be printed, depending on NNR.
C
C Note..  this routine is compatible with ANSI-77; however the
C following assumptions may not be valid for some machines:
C
C 1. The argument MSG is assumed to be of type CHARACTER, and
C    the message is printed with a format of (1X,80A1).
C 2. The message is assumed to take only one line.
C    Multi-line messages are generated by repeated calls.
C 3. If LEVEL = 2, control passes to the statement STOP
C    to abort the run.  For a different run-abort command,
C    change the statement following statement 100 at the end.
C 4. R1 and R2 are assumed to be in double precision and are printed
C    in E21.13 format.
C 5. The logical unit number 6 is standard output.
C    For a different default logical unit number, change the assignment
C    statement for LUNIT below.
C
C-----------------------------------------------------------------------
C Subroutines called by XERRWV.. None
C Function routines called by XERRWV.. None
C-----------------------------------------------------------------------
C
      INTEGER I, LUNIT, MESFLG
      CHARACTER*1  CC
      CHARACTER*60 FOUTNM
      INTEGER IMESSG(2048)
C
C Define message print flag and logical unit number. -------------------
      MESFLG = 1
      LUNIT = logerr
      IF (MESFLG.NE.0) THEN
C Write the message. ---------------------------------------------------
        WRITE (LUNIT,5000) (MSG(I),I = 1,NMES)
        IF (NI.EQ.1) WRITE (LUNIT,5010) nerr,I1
        IF (NI.EQ.2) WRITE (LUNIT,5020) nerr,I1, I2
        IF (NNR.EQ.1) WRITE (LUNIT,5030) NERR,R1
        IF (NNR.EQ.2) WRITE (LUNIT,5040) NERR,R1, R2
      END IF
C Abort the run if LEVEL = 2. ------------------------------------------
      IF (LEVEL.NE.2) RETURN
      STOP
 5000 FORMAT (80A1)
 5010 FORMAT ('NERR, I1 =',2I10)
 5020 FORMAT ('NERR,I1,I2 =',3I10)
 5030 FORMAT ('NERR,R1 =',D21.13)
 5040 FORMAT ('NERR,R1,R2 =',I10,2E21.13)
C----------------------- End of Subroutine XERRWV ----------------------
      END
      INTEGER FUNCTION LENG(C)

C     Routine:  LENG
C
C     Function: Determines the length (last non-blank character)
C               of the character string c.
C
C     Inputs:   C       character string
C     Outputs:  LENG    last non-blank character in C
C
C     Revision History:
C        Created: 10/14/93 by GPF:

      CHARACTER*(*) C
      NC = LEN(C)
      LENG = 0
      DO 10 I = NC, 1, -1
         IF(C(I:I).NE.' ')THEN
            LENG = I
            RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END

      DOUBLE PRECISION FUNCTION D1MACH(I)
C***BEGIN PROLOGUE  D1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  831014   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  Returns double precision machine dependent constants
C***DESCRIPTION
C     From the book, "Numerical Methods and Software" by
C                D. Kahaner, C. Moler, S. Nash
C                Prentice Hall, 1988
C
C
C     D1MACH can be used to obtain machine-dependent parameters
C     for the local machine environment.  It is a function
C     subprogram with one (input) argument, and can be called
C     as follows, for example
C
C          D = D1MACH(I)
C
C     where I=1,...,5.  The (output) value of D above is
C     determined by the (input) value of I.  The results for
C     various values of I are discussed below.
C
C  Double-precision machine constants
C  D1MACH( 1) = B**(EMIN-1), the smallest positive magnitude.
C  D1MACH( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C  D1MACH( 3) = B**(-T), the smallest relative spacing.
C  D1MACH( 4) = B**(1-T), the largest relative spacing.
C  D1MACH( 5) = LOG10(B)
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  D1MACH
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
C
      DOUBLE PRECISION DMACH(5)
C
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 170 SERIES (FTN5).
C
C      DATA SMALL(1) / O"00604000000000000000" /
C      DATA SMALL(2) / O"00000000000000000000" /
C
C      DATA LARGE(1) / O"37767777777777777777" /
C      DATA LARGE(2) / O"37167777777777777777" /
C
C      DATA RIGHT(1) / O"15604000000000000000" /
C      DATA RIGHT(2) / O"15000000000000000000" /
C
C      DATA DIVER(1) / O"15614000000000000000" /
C      DATA DIVER(2) / O"15010000000000000000" /
C
C      DATA LOG10(1) / O"17164642023241175717" /
C      DATA LOG10(2) / O"16367571421742254654" /
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 200 SERIES
C
C     DATA SMALL(1) / X'9000400000000000' /
C     DATA SMALL(2) / X'8FD1000000000000' /
C
C     DATA LARGE(1) / X'6FFF7FFFFFFFFFFF' /
C     DATA LARGE(2) / X'6FD07FFFFFFFFFFF' /
C
C     DATA RIGHT(1) / X'FF74400000000000' /
C     DATA RIGHT(2) / X'FF45000000000000' /
C
C     DATA DIVER(1) / X'FF75400000000000' /
C     DATA DIVER(2) / X'FF46000000000000' /
C
C     DATA LOG10(1) / X'FFD04D104D427DE7' /
C     DATA LOG10(2) / X'FFA17DE623E2566A' /
C
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C     DATA SMALL(1) / 00564000000000000000B /
C     DATA SMALL(2) / 00000000000000000000B /
C
C     DATA LARGE(1) / 37757777777777777777B /
C     DATA LARGE(2) / 37157777777777777777B /
C
C     DATA RIGHT(1) / 15624000000000000000B /
C     DATA RIGHT(2) / 00000000000000000000B /
C
C     DATA DIVER(1) / 15634000000000000000B /
C     DATA DIVER(2) / 00000000000000000000B /
C
C     DATA LOG10(1) / 17164642023241175717B /
C     DATA LOG10(2) / 16367571421742254654B /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA SMALL(1) / 201354000000000000000B /
C     DATA SMALL(2) / 000000000000000000000B /
C
C     DATA LARGE(1) / 577767777777777777777B /
C     DATA LARGE(2) / 000007777777777777774B /
C
C     DATA RIGHT(1) / 376434000000000000000B /
C     DATA RIGHT(2) / 000000000000000000000B /
C
C     DATA DIVER(1) / 376444000000000000000B /
C     DATA DIVER(2) / 000000000000000000000B /
C
C     DATA LOG10(1) / 377774642023241175717B /
C     DATA LOG10(2) / 000007571421742254654B /
C
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA SMALL(1),SMALL(2) / Z00100000, Z00000000 /
C     DATA LARGE(1),LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C     DATA RIGHT(1),RIGHT(2) / Z33100000, Z00000000 /
C     DATA DIVER(1),DIVER(2) / Z34100000, Z00000000 /
C     DATA LOG10(1),LOG10(2) / Z41134413, Z509F79FF /
C
C     MACHINE CONSTATNS FOR THE IBM PC FAMILY (D. KAHANER NBS)
C
C     DATA DMACH/2.23D-308,1.79D+308,1.11D-16,2.22D-16,
C    *  0.301029995663981195D0/
C
C  MACHINE CONSTATNS FOR THE APPLE MACINTOSH II ABSOFT 2.4 (D. KAHANER NBS)
      DATA DMACH(1) /1.0D-307/
      DATA DMACH(2) /1.79D+308/
      DATA DMACH(3) /1.11D-16/
      DATA DMACH(4) /2.22D-16/
      DATA DMACH(5) /.301029995663981195/
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C     DATA SMALL(1),SMALL(2) / "033400000000, "000000000000 /
C     DATA LARGE(1),LARGE(2) / "377777777777, "344777777777 /
C     DATA RIGHT(1),RIGHT(2) / "113400000000, "000000000000 /
C     DATA DIVER(1),DIVER(2) / "114400000000, "000000000000 /
C     DATA LOG10(1),LOG10(2) / "177464202324, "144117571776 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C     DATA SMALL(1),SMALL(2) / "000400000000, "000000000000 /
C     DATA LARGE(1),LARGE(2) / "377777777777, "377777777777 /
C     DATA RIGHT(1),RIGHT(2) / "103400000000, "000000000000 /
C     DATA DIVER(1),DIVER(2) / "104400000000, "000000000000 /
C     DATA LOG10(1),LOG10(2) / "177464202324, "476747767461 /
C
C
C     MACHINE CONSTANTS FOR THE SUN-3 (INCLUDES THOSE WITH 68881 CHIP,
C       OR WITH FPA BOARD. ALSO INCLUDES SUN-2 WITH SKY BOARD. MAY ALSO
C       WORK WITH SOFTWARE FLOATING POINT ON EITHER SYSTEM.)
C
C      DATA SMALL(1),SMALL(2) / X'00100000', X'00000000' /
C      DATA LARGE(1),LARGE(2) / X'7FEFFFFF', X'FFFFFFFF' /
C      DATA RIGHT(1),RIGHT(2) / X'3CA00000', X'00000000' /
C      DATA DIVER(1),DIVER(2) / X'3CB00000', X'00000000' /
C      DATA LOG10(1),LOG10(2) / X'3FD34413', X'509F79FF' /
C
C
C     MACHINE CONSTANTS FOR VAX 11/780
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1), SMALL(2) /        128,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /       9344,           0 /
C     DATA DIVER(1), DIVER(2) /       9472,           0 /
C     DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /
C
C    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C     DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C   MACHINE CONSTANTS FOR VAX 11/780 (G-FLOATING)
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1), SMALL(2) /         16,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /      15552,           0 /
C     DATA DIVER(1), DIVER(2) /      15568,           0 /
C     DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
C
C    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C     DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
C
C
C***FIRST EXECUTABLE STATEMENT  D1MACH
      IF (I.LT.1.OR.I.GT.5) CALL 
     +    XERROR('D1MACH -- I OUT OF BOUNDS',25,1,2)
C
      D1MACH = DMACH(I)
      RETURN
C***TAKEN FROM KAHANER LIBRARY, BILL MOSS, 2-91
      END

      SUBROUTINE XERRmod(mesg,NMES,NERR,NNR,R1,R2)

      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"

      INTEGER NMES, NERR, NNR
      DOUBLE PRECISION R1, R2
      CHARACTER mesg*(*)

C-----------------------------------------------------------------------
C Subroutine XERRmod, as given here, constitutes a simplified version of
C the SLATEC error handling package. 
! Written by A. C. Hindmarsh and P. N. Brown at LLNL.
C-----------------------------------------------------------------------
C Subroutines called by XERRmod.. None
C Function routines called by XERRmod.. None
C-----------------------------------------------------------------------
C
      INTEGER I, LUNIT, MESFLG
      CHARACTER*1  CC
      CHARACTER*60 FOUTNM
C
	logerr = 3
	lm = len_trim(mesg)
C Write the message. ---------------------------------------------------
	write(logerr,5000) mesg(1:lm)
	IF (NNR.EQ.1) write(logerr,5001) nerr,r1
	IF (NNR.EQ.2) write(logerr,5002) nerr,r1,r2
	return

 5000 FORMAT(a)
 5001 FORMAT('ierror,r1 =',i5,2d14.4)
 5002 FORMAT('ierror,r1,r2 =',i5,2d14.4)
      END subroutine xerrmod

      SUBROUTINE BUBBLE (PRSNID,HAZARD,NRET)
C
C   PERFORMS A BUBBLE SORT OF PERSONS INPUT WITH TENAB COMMAND
C
      INTEGER PRSNID(NRET),HAZARD(NRET)
      NRETM1=NRET-1
      DO 20 I=1,NRETM1
      IP1=I+1
      DO 10 J=IP1,NRET
      IF (PRSNID(I)*10+HAZARD(I).GT.PRSNID(J)*10+HAZARD(J)) THEN
        IT=PRSNID(I)
        PRSNID(I)=PRSNID(J)
        PRSNID(J)=IT
        IT=HAZARD(I)
        HAZARD(I)=HAZARD(J)
        HAZARD(J)=IT
      END IF
10    CONTINUE
20    CONTINUE
      RETURN
      END
	SUBROUTINE CFASTEXIT (NAME, errorcode)
	
      include "cparams.fi"
      include "cshell.fi"
      include "iofiles77.fi"

	CHARACTER NAME*(*)
	integer errorcode
	logical doesthefileexist

C	SET THE APPROPRIATE EXIT FORMAT

	LEN= MIN(LEN_TRIM (NAME),32)

	if (errorcode.eq.0) then
		WRITE(LOGERR, 1) (NAME(i:i),i=1,LEN)
	else
		 write(logerr,2) errorcode
	ENDIF

	STOP

1     FORMAT ('Normal exit from ',32a1)
2     format ('Error exit, code = ',i5)

	END
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


      SUBROUTINE CONVRT(COORD,FIRST,LAST,TYPE,I0,X0)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CONVRT
C
C     Source File: CONVRT.SOR
C
C     Functional Class:  UTILITY
C
C     Description:  Convert next entry in string COORD to a number of
C                   correct type.
C
C     Arguments: COORD  String to be parsed
C                FIRST  Beginning position of substring to be parsed
C                LAST   End of substring
C                TYPE   Type of number (1=integer, 2=real)
C                I0     Value if integer
C                X0     Value if real
C
C     Revision History:
C        11/16/1992 by RDP added routines INUM and RNUM to parse string into
C                   number by hand.
C        Modified: 1/24/1996 at 14:48 by RWP: Include parameter LBUFLN
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
C
C     CONVERT NEXT ENTRY IN STRING COORD TO A NUMBER OF CORRECT TYPE
C
C
      include "cfin.fi"

      CHARACTER COORD*(*)
      CHARACTER*20 DECOD
      INTEGER FIRST, LAST, TYPE, I0
      REAL X0
C
C     GET DATA TYPE
C
      CALL DATYPE(COORD,FIRST,LAST,TYPE)
      DECOD = ' '
      LFIRST = MIN(FIRST,LBUFLN)
      LLAST = MIN(LAST,FIRST+20,LBUFLN)
      DECOD = COORD(LFIRST:LLAST)
C
C     DECODE BY TYPE
C
      IF (TYPE.EQ.1) THEN
        X0 = RNUM(DECOD)
      ELSE IF (TYPE.EQ.2) THEN
        I0 = INUM(DECOD)
      END IF
      RETURN
      END
      INTEGER FUNCTION INUM(STRNG)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INUM
C
C     Source File: CONVRT.SOR
C
C     Functional Class:  UTILITY
C
C     Description:  Convert string into an integer
C
C     Arguments: STRNG  String containing number to be converted.
C
C     Revision History:
C        Created:  11/16/1992 at 10:07 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      CHARACTER STRNG*(*)
      IVAL = 0
      ISGN = 1
      DO 10 I = 1, LEN(STRNG)
        IF (STRNG(I:I).EQ.'-') ISGN = -1
        IC = ICHAR(STRNG(I:I)) - ICHAR('0')
        IF (IC.GE.0.AND.IC.LE.9) IVAL = IVAL * 10 + IC
   10 CONTINUE
      INUM = ISGN * IVAL
      RETURN
      END
      REAL FUNCTION RNUM(STRNG)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     RNUM
C
C     Source File: CONVRT.SOR
C
C     Functional Class:  UTILITY
C
C     Description:  Convert string into a real number
C
C     Arguments: STRNG  String containing number to be converted.
C
C     Revision History:
C        Created:  11/16/1992 at 10:07 by RDP
C        Modified: 11/19/1993 at 14:55 by RDP:
C                  Calculation of exponent value from integer to real to fix
C                  invalid integer numbers for large ABS(exponents)
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      CHARACTER STRNG*(*), CHR
      RVAL = 0.0
      ISGN = 0
      IDEC = 0
      IESGN = 0
      IEXP = 0
      IP = 1
   10 CHR = STRNG(IP:IP)
C
C     FIRST COMES A SIGN OR MANTISSA
C
      IC = ICHAR(CHR) - ICHAR('0')
      IF (ISGN.EQ.0) THEN
        ISGN = 1
        IF (CHR.EQ.'-') ISGN = -1
        IF (CHR.EQ.'.') IDEC = 1
        IF (IC.GE.0.AND.IC.LE.9) THEN
          RVAL = RVAL * 10. + IC
        END IF
C
C     IF WE'VE FOUND THE MANTISSA, CHECK FOR EXPONENT
C
      ELSE IF (CHR.EQ.'E'.OR.CHR.EQ.'e'.OR.CHR.EQ.'D'.OR.CHR.EQ.'d'
     +    .OR.CHR.EQ.'+'.OR.CHR.EQ.'-') THEN
        IESGN = 1
        IF (CHR.EQ.'-') IESGN = -1
      ELSE
C
C     IF NO EXPONENT, KEEP TRACK OF DECIMAL POINT
C
        IF (IESGN.EQ.0) THEN
          IF (CHR.EQ.'.') THEN
            IDEC = 1
          ELSE IF (IC.GE.0.AND.IC.LE.9) THEN
            RVAL = RVAL * 10. + IC
            IF (IDEC.NE.0) IDEC = IDEC + 1
          END IF
C     
C     IF EXPONENT JUST KEEP TRACK OF IT
C
        ELSE
          IF (IC.GE.0.AND.IC.LE.9) IEXP = IEXP * 10 + IC
        END IF
      END IF
      IP = IP + 1
      IF (IP.LT.LEN(STRNG)) GO TO 10
      IF (IDEC.NE.0) IDEC = IDEC - 1
      EVAL = 10. ** (ABS(IESGN*IEXP-IDEC))
      IESGN = ISIGN(1,IESGN*IEXP-IDEC)
      IF (IESGN.EQ.1) RNUM = ISGN * RVAL * EVAL
      IF (IESGN.EQ.-1) RNUM = ISGN * RVAL / EVAL
      RETURN
      END
	logical function countargs (label,tocount,lcarray,numc,nret)

!	Count the number of arguements on the input line. 
!	Should be tocount. If not, then return an error (logical false)

      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"

	integer tocount, i,numc,nret
	character lcarray*128(numc), label*5

	countargs = .false.
	nret = 0.

	do 1 i = 1, tocount
	if (lcarray(i).eq.' ') then
		 if (i.eq.1.) then
			  write(logerr,5003) label, tocount
		 else if (i.eq.2) then
			  write(logerr,5004) label, tocount
		 else
			  write(logerr,5000) label, tocount, nret
		 endif
		 return
	endif
	nret = nret + 1
    1 continue

	countargs = .true.

	do 2 i = tocount+1, numc
	if (lcarray(i).ne.' ') nret = nret + 1
2	continue	

	if(nret.eq.tocount) then
		 write(logerr,5001) label, (lcarray(i),i=1,nret)
	else
		 write(logerr,5002) label, (lcarray(i),i=1,nret)
	endif

	return

 5000	FORMAT('Error for key word: ',a5,' Required count = ',i2,
     + ' and there were only ',i2,' entries')
 5001 FORMAT('Key word ',a5,' parameter(s) = ',20a10)
 5002 FORMAT('Key word (ext) ',a5,' parameter(s) = ',128a10)
 5003	FORMAT('Error for key word: ',a5,' Required count = ',i2,
     + ' and there were no entries')
 5004	FORMAT('Error for key word: ',a5,' Required count = ',i2,
     + ' and there was only 1 entry')
	end

      SUBROUTINE CPTIME(CPUTIM)
      use ifport
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CPTIME
C
C     Source File: CPTIME.SOR
C
C     Functional Class:  Utility
C
C     Description:  ROUTINE TO CALCULATE AMOUNT OF COMPUTER
C                   TIME (CPUTIM) IN SECONDS USED SO FAR.  THIS ROUTINE 
C                   WILL GENERALLY BE DIFFERENT FOR EACH COMPUTER.
C
C     Arguments: CPUTIM
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      DOUBLE PRECISION CPUTIM
      INTEGER*2 HRS, MINS, SECS, HSECS
      CALL GETTIM(HRS,MINS,SECS,HSECS)
      CPUTIM = HRS * 3600 + MINS * 60 + SECS + HSECS / 100.0
      RETURN
      END
      SUBROUTINE DATYPE(CRD,N1,N2,DTYPE)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DATYPE
C
C     Source File: DATYPE.SOR
C
C     Functional Class:  UTILITY
C
C     Description:  This routine determines the data type of a string.
C                   The character string is examined between given starting and
C                   ending positions to determine if the substring is an
C                   integer, a real number, or a non-numeric string.
C
C     Arguments: CRD    String containing number to be typed
C                N1     Starting position
C                N2     Ending position
C                DTYPE  Returned type (1=real, 2=integer, 3=non-numeric)
C
C     Revision History:
C        11/16/1992 by RDP, changed specification of CRD to character string.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      LOGICAL PERIOD, EFMT
      INTEGER N1, N2, DTYPE
      CHARACTER CRD*(*), NUM(12)*1
      DATA NUM /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', 
     +    '-'/
      PERIOD = .FALSE.
      EFMT = .FALSE.
C
C  DETERMINE DATA TYPE.  ASSUME A NUMERIC ENTRY UNTIL A NON-NUMERIC
C  CHARACTER IS FOUND.
C
      DO 20 I = N1, N2
        IF (CRD(I:I).EQ.'.') THEN
          IF (PERIOD) THEN
C
C  PERIOD IN STRING - NON NUMERIC
C
            GO TO 30
          ELSE
            PERIOD = .TRUE.
          END IF
          GO TO 20
        END IF
C
C     CHECK FOR DIGITS
C
        DO 10 J = 1, 12
          IF (CRD(I:I).EQ.NUM(J)) GO TO 20
   10   CONTINUE
        IF (INDEX('EeDd',CRD(I:I)).EQ.0.OR.EFMT) GO TO 30
        EFMT = .TRUE.
   20 CONTINUE
C
C  DETERMINE TYPE OF NUMERIC ENTRY
C
      IF (PERIOD.OR.EFMT) THEN
C
C  REAL
C
        DTYPE = 1
      ELSE
C
C  INTEGER
C
        DTYPE = 2
      END IF
      RETURN
C
C  NON-NUMERIC
C
   30 DTYPE = 3
      RETURN
      END
	LOGICAL FUNCTION DOESTHEFILEEXIST (CHECKFILE)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     DOESTHEFILEEXIST - we are checking the existence of required files
C
C     Source File: doesthefileexist.f
C
C     Functional Class: error checking
C
C     Description:  Part of the panel project output
C
C     Revision History: new 12/16/99
C---------------------------- ALL RIGHTS RESERVED ----------------------------
      include "cparams.fi"
      include "cshell.fi"
      include "cfin.fi"

	CHARACTER (*) CHECKFILE
	INTEGER LOGERR
	LOGICAL YESORNO
	
C	"checkfile" is the file to be checked for existence
C	We look in the path specified as the datapath (path)
C	Be very careful; we are using lbuf as input as well as a string for constructing the file

	INQUIRE (FILE=CHECKFILE, EXIST=YESORNO)
	IF (YESORNO) THEN
		DOESTHEFILEEXIST = .TRUE.
	else
		doesthefileexist = .false.
	ENDIF

	RETURN

	END
      subroutine exehandle (exepath, datapath, project, errorcode)

!--------------------------------- NIST/BFRL ---------------------------------
!
!     Routine:     EXEHANDLE
!
!     Source File: EXEHANDEL.F90 (free form)
!
!     Functional Class:
!
!     Description: get the arguments used to call the main programs (cfast, report, reportss, cplot, tosmokeview, topanel, compare)
!				   
!	  General format for the command line [folder1\executable folder2\data]
!
!     Arguments: exepath is the path (without the name) to the folder where the executable resides
!				 datapath is the path (without a file name) to the folder where the project data file resides
!				 project is the name of the project - this name cannot exceed 64 charcters 
!                the total lenght of datapath + project cannot exceed 256 characters
!                errorcode   (i*4)
!					100 program called with no arguments
!					101 the filename has an extension
!					102 project files does not exist
!					103 total file name length includ path >256
!					0 okay
!
!     Revision History:
!        Created:  12/11/2004 by WWJ
!
!---------------------------- ALL RIGHTS RESERVED ----------------------------

      INTEGER(2) n, status, loop, ld(2), li(2), ln(2), le(2), lb
      CHARACTER(256) buf, xname
      character *(*) exepath, datapath, project
      character (64) name(2)
      logical exists, doesthefileexist

      CHARACTER(3) drive(2)
      CHARACTER(256) dir(2)
      CHARACTER(64) ext(2)
      INTEGER(4) length, errorcode, pathcount, splitpathqq

      n = nargs ()
      project = ' '
      exepath = ' '
      datapath = ' '

      if (n.lt.2) then
	    errorcode = 100
	    return
      endif

      errorcode = 0

! get the calling program and arguments

      exepath = ' '
      datapath = ' '

      do 1 i = 1, 2

        loop = i - 1
        call getarg (loop, buf, status)

        if(status.gt.0) then
	      xname = buf

!	Split out the components

	      length = SPLITPATHQQ(xname, drive(i), dir(i), name(i), ext(i))
	      ld(i) = len_trim(drive(i))
	      li(i) = len_trim(dir(i))
	      ln(i) = len_trim(name(i))
	      le(i) = len_trim(ext(i))

	      pathcount = 5 + ln(i) + li(i) +ld(i) + le(i)

	      if (pathcount.gt.255.or.ln(i).gt.64) then
		      errorcode = 103
		    return
	      endif

        endif

1     continue

! Now check that the project.in file exists - this is the data file

      buf = ' '
      if (le(2).ne.0) then
	    buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // 
     *          ext(2)(1:le(2)) // '.in'
      else
	    buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) //
     *         '.in'
      endif

      lb = len_trim(buf)

! buf(1:lb) is the data file to check

      if (DoesTheFileExist(buf(1:lb))) then

!	The project file exists
	    exepath = drive(1)(1:ld(1)) // dir(1)(1:li(1))
	    datapath = drive(2)(1:ld(2)) // dir(2)(1:li(2))
	    project = name(2)(1:ln(2)) // ext(2)(1:le(2))
	    return
      else
! Note that we do not yet have the logerr file open, so write to the console
	    write(*,*) 'The data file does not exist'
	    errorcode = 102
      endif
      return

      end
      SUBROUTINE GRABKY(ICH,IT)

      USE ifport
      CHARACTER*1 CH, getcharqq
      INTEGER*2 ICH, IT
      logical peekcharqq
      
      ICH = 0
      IT = 0
	
	IF (PEEKCHARQQ()) THEN
		CH = GETCHARQQ()
		ICH = ICHAR(CH)
		IF (ICH.EQ.0) THEN
			CH = GETCHARQQ()
			ICH = ICHAR (CH)
			IT = 2
		ELSE
			IT = 1
		ENDIF
	ENDIF

      RETURN
      END

      SUBROUTINE INDEXI(N,ARRIN,INDX)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INDEXI
C
C     Source File: INDEXI.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routines sorts the array ARRIN passively via
C                   the permuation array INDX.  The elements
C                   ARRIN(INDX(I)), I=1, ..., N are in increasing order.
C                   This routine uses a bubble sort.  It should not be used
C                   for large N (N>30), since bubble sorts are not
C                   efficient.  The original INDEXI subroutine was deleted
C                   for two reasons.  1) it was based on copyrighted 
C                   numerical recipes code, 2) it gave correct but
C                   unexpected results in some cases.  (sorted arrays 
C                   containing constant values were reordered)
C
C     Arguments: 
C     INPUT:
C            N     number of elements in N
C            ARRIN array to be passively sorted
C     OUTPUT
C            INDX  permuation vector containing ordering such that
C                  ARRIN(INDX) is in increasing order.
C
C     Revision History:
C        Created:  09/30/1993 gpf
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      INTEGER ARRIN(*), INDX(*), AI, AIP1
      DO 10 I = 1, N
         INDX(I) = I
   10 CONTINUE
    5 CONTINUE
      ISWITCH = 0
      DO 20 I = 1, N-1, 2
         AI = ARRIN(INDX(I))
         AIP1 = ARRIN(INDX(I+1))
         IF(AI.LE.AIP1)GO TO 20
         ISWITCH = 1
         ITEMP = INDX(I)
         INDX(I) = INDX(I+1)
         INDX(I+1) = ITEMP
   20 CONTINUE
      DO 30 I = 2, N-1, 2
         AI = ARRIN(INDX(I))
         AIP1 = ARRIN(INDX(I+1))
         IF(AI.LE.AIP1)GO TO 30
         ISWITCH = 1
         ITEMP = INDX(I)
         INDX(I) = INDX(I+1)
         INDX(I+1) = ITEMP
   30 CONTINUE
      IF(ISWITCH.EQ.1)GO TO 5
      RETURN
      END
      SUBROUTINE INTERP(X,Y,N,T,ICODE,YINT)
      include "precis.fi"
C*BEG
C
C***  INTERP  GENERIC - THIS ROUTINE INTERPOLATES A TABLE OF
C             NUMBERS FOUND IN THE ARRAYS, X AND Y.
C
C*** SUBROUTINE ARGUMENTS
C
C  INPUT
C  -----
C  X,Y    -  ARRAYS OF SIZE N TO BE INTERPOLATED AT X=T
C  ICODE  -  CODE TO SELECT HOW TO EXTRAPOLATE VALUES IF T
C               IS LESS THAN X(1) OR GREATER THAN X(N).
C            IF ICODE = 1 THEN YINT = Y(1) FOR T < X(1)
C                          AND YINT = Y(N) FOR T > X(N).
C            IF ICODE = 2 THEN YINT IS EVALUATED BY INTERPOLATION
C            IF X(1) < T < X(N) AND BY EXTRAPOLATION IF T < X(1)
C            OR    T > X(N)
C
C  OUTPUT
C  ------
C  YINT   -  INTERPOLATED VALUE OF THE Y ARRAY AT T
C*END
      DIMENSION X(*), Y(*)
      SAVE
      DATA ILAST /1/
      IF (N.EQ.1) THEN
        YINT = Y(1)
        RETURN
      END IF
      IF (T.LE.X(1)) THEN
        IF (ICODE.EQ.1) THEN
          YINT = Y(1)
          RETURN
        ELSE
          IMID = 1
          GO TO 20
        END IF
      END IF
      IF (T.GE.X(N)) THEN
        IF (ICODE.EQ.1) THEN
          YINT = Y(N)
          RETURN
        ELSE
          IMID = N - 1
          GO TO 20
        END IF
      END IF
      IF (ILAST+1.LE.N) THEN
        IMID = ILAST
        IF (X(IMID).LE.T.AND.T.LE.X(IMID+1)) GO TO 20
      END IF
      IF (ILAST+2.LE.N) THEN
        IMID = ILAST + 1
        IF (X(IMID).LE.T.AND.T.LE.X(IMID+1)) GO TO 20
      END IF
      IA = 1
      IZ = N - 1
   10 CONTINUE
      IMID = (IA+IZ) / 2
      IF (T.LT.X(IMID)) THEN
        IZ = IMID - 1
        GO TO 10
      END IF
      IF (T.GE.X(IMID+1)) THEN
        IA = IMID + 1
        GO TO 10
      END IF
   20 CONTINUE
      DYDX = (Y(IMID+1)-Y(IMID)) / (X(IMID+1)-X(IMID))
      YINT = Y(IMID) + DYDX * (T-X(IMID))
      ILAST = IMID
      RETURN
      END
      INTEGER FUNCTION LENGTH(STRING)
      CHARACTER STRING*(*)
      IF (LEN(STRING).NE.0) THEN
        DO 10 I=LEN(STRING),1,-1
        IF (STRING(I:I).NE.' ') THEN
          LENGTH=I
          RETURN
        END IF
10      CONTINUE
      END IF
      LENGTH=0
      RETURN
      END
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
      SUBROUTINE READASTU (IN, COUNT, START, MAX, VALID)

C     READ IN A STRING FROM THE STANDARD INPUT DEVICE

C     THIS VERSION OF READAS(TU) IS SIMILAR TO THE ONE USED BY EVERONE ELSE
C     (READAS) EXCEPT THAT THIS CONTAINS AUTOMATIC CONVERSION TO UPPER CASE.  
C     THIS IS TO FILTER COMMANDS FROM THE CONSOLE SO THAT THEY ARE NOT CASE
C     SENSITIVE.

      include "precis.fi"
      include "cparams.fi"
      include "cshell.fi"

      INTEGER START, COUNT
      LOGICAL VALID
      CHARACTER CH,INN*256,IN(MAX),C

10    INN = ' '
      READ(IOFILI,4,END=5) INN
4     FORMAT(A256)

C     FILTER COMMENTS

      C = INN(1:1)
      IF (C.EQ.'!'.OR.C.EQ.'#') GO TO 10

C     OK

      DO 7 I = 255,1,-1
      COUNT = I
      IF (INN(I:I).NE.' ') GO TO 6
7     CONTINUE
5     COUNT = 0
      VALID = .FALSE.
      RETURN
C
C   CHECK FOR CONTROL CHARACTERS
C
6     NC = 0
      COUNT = MIN(MAX,COUNT)
      DO 1 I = 1,COUNT
      ICH = ICHAR(INN(I:I))
      IF (ICH.LT.32.OR.ICH.GT.125) GO TO 3
      NC = I
1     CONTINUE
3     COUNT = NC
C
C   CONVERT TO UPPER CASE
C
      DO 2 I = 1,COUNT
      CH = INN(I:I)
      ICH = ICHAR(CH)
      IF (ICH.GT.96.AND.ICH.LT.123) ICH = ICH - 32
      IN(I) = CHAR(ICH)
2     CONTINUE
      START=1
      IF (COUNT.GT.0) THEN
        VALID = .TRUE.
      ELSE
        VALID = .FALSE.
      END IF
      RETURN
      END
      SUBROUTINE READCV1 (IN,COUNT,START,IX,XI,TYPE,VALID)
C
C   DO THE ACTUAL STRING CONVERSION
C
      INTEGER START,FIRST,LAST,COUNT,TYPE
      LOGICAL VALID
      CHARACTER*128 IN
      REAL XI

      CALL SSTRNGP (IN,COUNT,START,FIRST,LAST,VALID)
      IF (.NOT.VALID) THEN
        GO TO 5
      END IF
      CALL CONVRT (IN,FIRST,LAST,TYPE,IX,XI)
      COUNT = COUNT - (LAST-START+1)
      START = LAST + 1
5     RETURN
      END
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
      SUBROUTINE READOP
      use ifport

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READOP
C
C     Description:  retrieve and process command line options and date
C
C     Arguments: none
C
C     Revision History:
C        Written:  7/26/1990 by WWJ: implement date on UNIX and PC
C        Modified: 2/8/1993 RDP:
C                  Rewritten to to command parsing format to remove bug when 
C                  options and filename were not separated with a space.
C
C        Modified: 10/21/97 GPF:
C                  added -s option to create a "backdoor" solver input file
C                  aka SOLVER.INI .  note that the default name assigned by
C                  this option is SOLVE.INI not SOLVER.INI so as to not overwrite
C                  this file if already present.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
!
!	Unit numbers defined in readop, openoutputfiles, readinputfiles
!
!      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
!      3 is for the log file  (LOGERR)
!      6 is output (IOFILO)
!     11 is the history file
!     12 is used to write the status file (project.status)
!     13 smokeview output
!     14 spreadsheet output
!
      include "cparams.fi"
      include "cshell.fi"

      INTEGER (2) YEAR, MONTH, DAY
      LOGICAL EXISTS
      CHARACTER STRS(8)*60, IC, TOUPPER*1, LOGFILE*60
      CHARACTER*60 SOLVEINI
      INTEGER IARG(8), IOPT(26), OPTION
      OPTION(IC) = IOPT(ICHAR(IC)-ICHAR('A')+1)

C     CURRENT DATE

      CALL GETDAT(YEAR,MONTH,DAY)
      RUNDAT(3) = DAY
      RUNDAT(2) = MONTH
      RUNDAT(1) = YEAR
      WRITE (MPSDATC,5010) RUNDAT(1), RUNDAT(2), RUNDAT(3)

C     COMMAND LINE ARGUMENTS

      NARGS = 8
      CALL CMDLINE(NARGS,STRS,IARG,IOPT)

!     OPTIONS
!     K = DO NOT ACCESS KEYBOARD
!     F/C = OUTPUT OPTIONS
!     S = Output "SOLVER.INI" options into the file SOLVE.INI
!	I = do initialization only
!     H to include the header in the output file
!     D to turn on debugging writes
!     T to output trace species mass

      IF (OPTION('H').NE.0) HEADER = .TRUE.
      IF (OPTION('K').NE.0) NOKBD = .TRUE.
	IF (OPTION('I').NE.0) INITIALIZEONLY = .TRUE.
	IF (OPTION('D').NE.0) DEBUGGING = .TRUE.
	if (option('T').ne.0) trace = .true.
      LOGERR = 3

      IF (OPTION('F').ne.0.and.option('C').ne.0) stop 107
	if (option('C').ne.0) outputformat = 1
	if (option('F').ne.0) outputformat = 2

      IF (OPTION('S').NE.0) THEN
         IF (STRS(OPTION('S')).NE.' ') THEN
            SOLVEINI = STRS(OPTION('S'))
         ELSE
            SOLVEINI = 'SOLVE.INI'
         ENDIF
         CALL WRITEINI(SOLVEINI)
      ENDIF

      RETURN

 5010 FORMAT (I4.4,'/',I2.2,'/',I2.2)
      END
	 subroutine shellsort (ra, n)

	 implicit none
	 integer n, j, i, inc
	 double precision ra(n), rra
	 
 	 inc = 1
1	 inc = 3*inc+1
	 if (inc.le.n) go to 1
2	 continue
		  inc = inc / 3
		  do i = inc+1, n
				rra = ra(i)
				j = i
3				if(ra(j-inc).gt.rra) then
					 ra(j) = ra(j-inc)
					 j = j - inc
					 if(j.le.inc) go to 4
				go to 3
				endif
4				ra(j) = rra
		  enddo
	 if(inc.gt.1) go to 2
	 return
	 end
      SUBROUTINE SORTBRM(
     I                   X,LX,IX,LIX,NROW,NCOLX,NCOLIX,ISORT,LDP,NROOM,
     O                   IPOINT)

C     Function: Sort the two arrays X and IX by the ISORT'th column of
C               IX which contains room data.  This routine is based on the
C               now obsolete routine SORTFR.  This routine is used
C               to sort fire and detector data structures by room 
C               number.
C
C     Inputs:   X       Floating point info to be sorted
C               LX      Leading dimension of X 
C               IX      Integer info to be sorted
C               LIX     Leading dimension of IX in calling routine
C               NROW    Number of rows in X and IX
C               NCOLX   Number of columns in X
C               NCOLIX  Number of columns in IX
C               ISORT   Column in IX to sort on (usually contains room numbers)
C               LDP     Leading dimension of IPOINT
C               NROOM   number of elements for which ipoint is defined, also
C                       the number of rooms
C
C     Outputs:  IPOINT  Pointer array for sorted X and IX list.
C                       (r,1) = Number of items (fires or detectors so far)
C                               in room r
C                       (r,2) = Pointer to beginning element in IX
C                               and X for fire or detector in room r
C
C     Revision History:
C        Created: 9/3/93 by GPF:
!	   Modified 10/31/2006 by WWJ
!
!				 SORTBRM is used to sort the detector information.
!
C                  This routine was created using SORTFR as a template.
C                  SORTFR was designed specifically to sort fire data 
C                  structures.  This routine is more general.  It is designed
C                  to sort any floating point and integer data structures
C                  by the data given in the ISORT'th colum of IX (usually
C                  room numbers).

      include "precis.fi"
      DIMENSION X(LX,NCOLX), IX(LIX,NCOLIX)
C  
C    IF THE NUMBER OF FIRES, DETECTORS OR ROOMS EVER EXCEEDS 200 THEN 
C    THE FOLLOWING DIMENSION STATEMENT NEEDS TO BE CHANGED
C
      PARAMETER (LWORK=100)
      DIMENSION IPOINT(LDP,*), WORK(LWORK), IWORK(LWORK), IPERM(LWORK)
C
C     CREATE A PERMUTATION VECTOR USING THE ISORT'TH COLUMN OF IX
C
      IF(NROW.GT.LWORK)THEN
              CALL XERROR('NOT ENOUGH WORK SPACE IN SORTBRM',
     .                    0,1,2)
      ENDIF
      DO 2 I = 1, NROW
    2    IPERM(I) = I
      CALL INDEXI(NROW,IX(1,ISORT),IPERM)
C
C     REORDER INTEGER ARRAY USING THE PERMUTATION VECTOR
C
      DO 5 J = 1, NCOLIX
        DO 10 I = 1, NROW
          IWORK(I) = IX(IPERM(I),J)
   10   CONTINUE
        DO 20 I = 1, NROW
          IX(I,J) = IWORK(I)
   20   CONTINUE
    5 CONTINUE
C
C    REORDER THE FLOATING POINT ARRAYS USING THE PERMUTATION VECTOR
C
      DO 50 J = 1, NCOLX
        DO 30 I = 1, NROW
          WORK(I) = X(IPERM(I),J)
   30   CONTINUE
        DO 40 I = 1, NROW
          X(I,J) = WORK(I)
   40   CONTINUE
   50 CONTINUE
C
C     CONSTRUCT THE POINTER ARRAY
C
      DO 60 I = 1, NROOM
        IPOINT(I,1) = 0
        IPOINT(I,2) = 0
   60 CONTINUE
      DO 70 I = 1, NROW
        IROOM = IX(I,ISORT)
        IPOINT(IROOM,1) = IPOINT(IROOM,1) + 1
        IF (IPOINT(IROOM,2).EQ.0) IPOINT(IROOM,2) = I
   70 CONTINUE
      DO 80 I = 1, NROOM
        IF (IPOINT(I,2).EQ.0) IPOINT(I,2) = 1
   80 CONTINUE
      RETURN
      END
      SUBROUTINE SORTFR(NFIRE,IFROOM,XFIRE,IFRPNT,NM1)

C     Function: Sort the two arrays IFROOM and XFIRE into increasing
C               room number in IFROOM.  These are used in this order
C               by the ceiling jet and radiation algorithms
C
C     Inputs:   NFIRE   Number of fires
C     Outputs:  IFROOM  Room numbers for each of the fires
C               XFIRE   Fire related quantities used by other routines.
C                       See routine FIRES for definition.
C               IFRPNT  Pointer array for sorted fire list.
C                       (r,1) = Number of fires in room r
C                       (r,2) = Pointer to beginning element in IFROOM
C                               and XFIRE for fires in room r
C
C     Revision History:
C        Modified: 2/7/93 by GPF:
C                  The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have 
C                  the number of fires as the first subscript.

      include "precis.fi"
      include "cparams.fi"
      DIMENSION IFROOM(MXFIRE), XFIRE(MXFIRE,MXFIRP), IPERM(MXFIRE), 
     +    IWORK(MXFIRE), WORK(MXFIRE), IFRPNT(NR,2)

C     CREATE A PERMUTATION VECTOR FROM THE LIST OF FIRE ROOMS WHICH IS 
C     ORDERED BY INCREASING ROOM NUMBER

      DO 2 I = 1, NFIRE
    2    IPERM(I) = I
      CALL INDEXI(NFIRE,IFROOM,IPERM)
C
C     REORDER THE TWO ARRAYS WITH THE PERMUTATION VECTOR
C
      DO 10 I = 1, NFIRE
        IWORK(I) = IFROOM(IPERM(I))
   10 CONTINUE
      DO 20 I = 1, NFIRE
        IFROOM(I) = IWORK(I)
   20 CONTINUE
C
      DO 50 J = 1, MXFIRP
        DO 30 I = 1, NFIRE
          WORK(I) = XFIRE(IPERM(I),J)
   30   CONTINUE
        DO 40 I = 1, NFIRE
          XFIRE(I,J) = WORK(I)
   40   CONTINUE
   50 CONTINUE
C
C     DO THE POINTER ARRAYS FOR THE RADIATION AND CEILING JET ROUTINES
C
      DO 60 I = 1, NM1
        IFRPNT(I,1) = 0
        IFRPNT(I,2) = 0
   60 CONTINUE
      DO 70 I = 1, NFIRE
        IRM = IFROOM(I)
        IFRPNT(IRM,1) = IFRPNT(IRM,1) + 1
        IF (IFRPNT(IRM,2).EQ.0) IFRPNT(IRM,2) = I
   70 CONTINUE
      DO 80 I = 1, NM1
        IF (IFRPNT(I,2).EQ.0) IFRPNT(I,2) = 1
   80 CONTINUE
      RETURN
      END
      SUBROUTINE SSTRNG(STRING,WCOUNT,SSTART,SFIRST,SLAST,SVALID)
C
C  THIS ROUTINE FINDS POSITIONS OF SUBSTRINGS WITHIN A CHARACTER
C  STRING.  A SPACE, COMMA, - , (, OR ) INDICATES THE BEGINNING OR
C  END OF A SUBSTRING.  WHEN CALLED, THE STRING IS PASSED
C  AS AN INTEGER(CHOOSE) ALONG WITH THE NUMBER OF CHARACTERS IN THE
C  STRING(WCOUNT) AND A STARTING POSITION(SSTART).  BEGINNING AT
C  "SSTART", THE ROUTINE SEARCHES FOR A SUBSTRING. IF A SUBSTRING IS
C  FOUND, ITS FIRST AND LAST CHARACTER POSITIONS ARE RETURNED ALONG
C  WITH A TRUE VALUE IN "SVALID"; OTHERWISE "SVALID" IS SET FALSE.
C
      LOGICAL SVALID
      INTEGER SFIRST, SLAST, SSTART, WCOUNT, ENDSTR
      CHARACTER*1 STRING(*), SPACE, COMMA
C
      DATA SPACE/' '/, COMMA/','/
C
      SVALID = .TRUE.
C
C  INVALID STARTING POSITION - PAST END OF STRING
C
      ENDSTR = SSTART + WCOUNT - 1
C
C  FIND POSITION OF FIRST ELEMENT OF SUBSTRING
C
      DO 20 I = SSTART, ENDSTR
C
C     MOVE TO THE BEGINNING OF THE SUBSTRING
C
      SFIRST = I
      IF((STRING(I).NE.SPACE).AND.(STRING(I).NE.COMMA)) GOTO 60
20    CONTINUE
C
C  NO SUBSTRING FOUND - ONLY DELIMITER
C
      GO TO 40
C
C  FIND POSITION OF LAST CHARACTER OF SUBSTRING
C
60    DO 50 J = SFIRST, ENDSTR
C
C  POSITION OF LAST ELEMENT OF SUBSTRING
C
      SLAST = J-1
      IF((STRING(J).EQ.SPACE).OR.(STRING(J).EQ.COMMA)) GO TO 100
50    CONTINUE
C
C  NO SUBSTRING DELIMITER => LAST CHARACTER OF SUBSTRING IS THE
C  LAST CHARACTER OF THE STRING
C
      SLAST = ENDSTR
      RETURN
C
C  NO SUBSTRING FOUND
C
40    SVALID = .FALSE.
100   RETURN
      END
      SUBROUTINE SSTRNGP (STRING,WCOUNT,SSTART,SFIRST,SLAST,SVALID)
C
C   THIS ROUTINE FINDS POSITIONS OF SUBSTRINGS WITHIN A CHARACTER
C   STRING.  A SPACE, COMMA, - , (, OR ) INDICATES THE BEGINNING OR
C   END OF A SUBSTRING.  WHEN CALLED, THE STRING IS PASSED
C   AS AN INTEGER(CHOOSE) ALONG WITH THE NUMBER OF CHARACTERS IN THE
C   STRING(WCOUNT) AND A STARTING POSITION(SSTART).  BEGINNING AT
C   "SSTART", THE ROUTINE SEARCHES FOR A SUBSTRING. IF A SUBSTRING IS
C   FOUND, ITS FIRST AND LAST CHARACTER POSITIONS ARE RETURNED ALONG
C   WITH A TRUE VALUE IN "SVALID"; OTHERWISE "SVALID" IS SET FALSE.
C   SIMILAR TO SSTRNG, EXCEPT ENTRIES CAN BE GROUPED WITH PARENTHESIS
C
      LOGICAL SVALID
      INTEGER SFIRST,SLAST,SSTART,WCOUNT,ENDSTR
      CHARACTER*1 STRING(128),SPACE,COMMA,RPAREN,LPAREN
      DATA SPACE /' '/, COMMA /','/,RPAREN /')'/, LPAREN /'('/
c
      SVALID = .TRUE.
      ENDSTR = SSTART + WCOUNT - 1
C
C  FIND POSITION OF FIRST ELEMENT OF SUBSTRING
C
      DO 20 I = SSTART, ENDSTR
C
C   MOVE TO THE BEGINNING OF THE SUBSTRING
C
      SFIRST = I
      IF ((STRING(I).NE.SPACE).AND.(STRING(I).NE.COMMA).AND.
     2 (STRING(I).NE.RPAREN).AND.(STRING(I).NE.LPAREN)) GO TO 60
20    CONTINUE
C
C  NO SUBSTRING FOUND - ONLY DELIMITER
C
      GO TO 40
C
C  FIND POSITION OF LAST CHARACTER OF SUBSTRING
C
60    DO 50 J = SFIRST, ENDSTR
C
C  POSITION OF LAST ELEMENT OF SUBSTRING
C
      SLAST = J-1
      IF ((STRING(J).EQ.SPACE).OR.(STRING(J).EQ.COMMA).OR.
     . (STRING(J).EQ.RPAREN).OR.(STRING(J).EQ.LPAREN)) GO TO 100
50    CONTINUE
C
C  NO SUBSTRING DELIMITER => LAST CHARACTER OF SUBSTRING IS THE
C  LAST CHARACTER OF THE STRING
C
      SLAST = ENDSTR
      RETURN
C
C  NO SUBSTRING FOUND
C
40    SVALID = .FALSE.
      RETURN
100   IF (SLAST.LT.SFIRST) SVALID = .FALSE.
      END
      CHARACTER FUNCTION TOUPPER(CH)
      CHARACTER*1 CH, TOLOWER
C
C     CONVERT TO UPPER CASE
C
      ICH = ICHAR(CH)
      IF (ICH.GT.96.AND.ICH.LT.123) ICH = ICH - 32
      TOUPPER = CHAR(ICH)
      RETURN
C
C     COVERT TO LOWER CASE
C
      ENTRY TOLOWER(CH)
      ICH = ICHAR(CH)
      IF (ICH.GT.64.AND.ICH.LT.91) ICH = ICH + 32
      TOLOWER = CHAR(ICH)
      RETURN
      END
      SUBROUTINE UPPERALL(FROM,TO)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPPERALL
C
C     Source File: UPPERALL.SOR
C
C     Functional Class:  
C
C     Description:  Convert a character string to upper case
C
C     Arguments: FROM
C                TO
C
C     Revision History:
C        Created:  5/5/1995 at 13:51 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      CHARACTER*(*) FROM, TO
      CHARACTER*1 C
      EXTERNAL LENGTH
      NFROM = LENGTH (FROM)
      NTO = LEN(TO)
      NN = MIN(NFROM,NTO)
      DO 10 I = 1, NN
         C = FROM(I:I)
         IF(C.GE.'a'.AND.C.LE.'z')THEN
            C = CHAR(ICHAR(C) + ICHAR('A')-ICHAR('a'))
         ENDIF
         TO(I:I) = C
   10 CONTINUE
      IF(NTO.GT.NN)TO(NN+1:NTO)=' '
      RETURN
      END
      INTEGER FUNCTION FUNIT (IO)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FUNIT
C
C     Source File: UTILITY.SOR
C
C     Functional Class: UTILTY
C
C     Description: finds first avalable unit starting at IO
C
C     Arguments: IO
C
C     Revision History:
C        Created:  12/3/1992 at 10:37 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      PARAMETER(MXIO=32767)
      LOGICAL OPEND

      ITMP = IO-1
   10 CONTINUE
      ITMP = ITMP+1
      IF (ITMP.GT.MXIO) THEN
         FUNIT = -1
         RETURN
      END IF
      INQUIRE(UNIT=ITMP,OPENED = OPEND)
      IF (OPEND) GO TO 10
      FUNIT = ITMP
      RETURN
      END

      SUBROUTINE OPNOTPT (FILNAME, IOUNIT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OPNOTPT
C
C     Source File: UTILITY.SOR
C
C     Functional Class: UTILITY
C
C     Description: opens a file using the extension to distinguish previous 
C                  open files
C
C     Arguments: FILNAME
C                IOUNIT
C
C     Revision History:
C        Created:  12/3/1992 at 9:37 by PAR
C        Modified:  2/12/1993 by GPF to use FNDBLANK routine.
C                   Necessary because UNIX treated leading blanks
C                   as significant   
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      CHARACTER FILNAME*(*)
      INTEGER IOUNIT, FIRST, LAST
      LOGICAL EXISTED, VALID
      CHARACTER NAMEFIL*60, WORKFIL*60, FMT*14

      LENGTH = LEN (FILNAME)
      CALL SSTRNG (FILNAME, LENGTH, 1, FIRST, LAST, VALID)
      IF (.NOT.VALID) STOP 'CANNOT OPEN DEBUG FILE'
      ILEN = LAST - FIRST + 1
      NAMEFIL = ' '
      NAMEFIL(1:ILEN) = FILNAME(FIRST:LAST)

      ITMP = 0
   30 CONTINUE
      ITMP = ITMP + 1
      WRITE(FMT,10) ILEN
   10 FORMAT('(A',I2.2,',',1H','.',1H',',I3.3)')
      WRITE(WORKFIL,FMT) NAMEFIL, ITMP
      INQUIRE (FILE = WORKFIL, EXIST = EXISTED)
      IF (EXISTED) GO TO 30
      OPEN (UNIT = IOUNIT, FILE = WORKFIL,RECL=255)
      RETURN
      END
      SUBROUTINE XERBLA ( SRNAME, INFO )
*     ..    Scalar Arguments ..
      INTEGER            INFO
      CHARACTER*6        SRNAME
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the Level 2 BLAS routines.
*
*  It is called by the Level 2 BLAS routines if an input parameter is
*  invalid.
*
*  Installers should consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Parameters
*  ==========
*
*  SRNAME - CHARACTER*6.
*           On entry, SRNAME specifies the name of the routine which
*           called XERBLA.
*
*  INFO   - INTEGER.
*           On entry, INFO specifies the position of the invalid
*           parameter in the parameter-list of the calling routine.
*
*
*  Auxiliary routine for Level 2 Blas.
*
*  Written on 20-July-1986.
*
*     .. Executable Statements ..
*
      WRITE (*,99999) SRNAME, INFO
*
      STOP
*
99999 FORMAT ( ' ** On entry to ', A6, ' parameter number ', I2,
     $         ' had an illegal value' )
*
*     End of XERBLA.
*
      END

      LOGICAL FUNCTION LSAME ( CA, CB )
*     .. Scalar Arguments ..
      CHARACTER*1            CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME  tests if CA is the same letter as CB regardless of case.
*  CB is assumed to be an upper case letter. LSAME returns .TRUE. if
*  CA is either the same as CB or the equivalent lower case letter.
*
*  N.B. This version of the routine is only correct for ASCII code.
*       Installers must modify the routine for other character-codes.
*
*       For EBCDIC systems the constant IOFF must be changed to -64.
*       For CDC systems using 6-12 bit representations, the system-
*       specific code in comments must be activated.
*
*  Parameters
*  ==========
*
*  CA     - CHARACTER*1
*  CB     - CHARACTER*1
*           On entry, CA and CB specify characters to be compared.
*           Unchanged on exit.
*
*
*  Auxiliary routine for Level 2 Blas.
*
*  -- Written on 20-July-1986
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, Nag Central Office.
*
*     .. Parameters ..
      INTEGER                IOFF
      PARAMETER            ( IOFF=32 )
*     .. Intrinsic Functions ..
      INTRINSIC              ICHAR
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA .EQ. CB
*
*     Now test for equivalence
*
      IF ( .NOT.LSAME ) THEN
         LSAME = ICHAR(CA) - IOFF .EQ. ICHAR(CB)
      END IF
*
      RETURN
*
*  The following comments contain code for CDC systems using 6-12 bit
*  representations.
*
*     .. Parameters ..
*     INTEGER                ICIRFX
*     PARAMETER            ( ICIRFX=62 )
*     .. Scalar Arguments ..
*     CHARACTER*1            CB
*     .. Array Arguments ..
*     CHARACTER*1            CA(*)
*     .. Local Scalars ..
*     INTEGER                IVAL
*     .. Intrinsic Functions ..
*     INTRINSIC              ICHAR, CHAR
*     .. Executable Statements ..
*
*     See if the first character in string CA equals string CB.
*
*     LSAME = CA(1) .EQ. CB .AND. CA(1) .NE. CHAR(ICIRFX)
*
*     IF (LSAME) RETURN
*
*     The characters are not identical. Now check them for equivalence.
*     Look for the 'escape' character, circumflex, followed by the
*     letter.
*
*     IVAL = ICHAR(CA(2))
*     IF (IVAL.GE.ICHAR('A') .AND. IVAL.LE.ICHAR('Z')) THEN
*        LSAME = CA(1) .EQ. CHAR(ICIRFX) .AND. CA(2) .EQ. CB
*     END IF
*
*     RETURN
*
*     End of LSAME.
*
      END
      integer function rev_auxilliary
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_auxilliary = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_auxilliary