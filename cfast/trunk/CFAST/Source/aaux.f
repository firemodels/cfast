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
#ifdef pp_double
C This version is in double precision.
#else
C This version is in single precision.
#endif
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
#ifdef pp_gui
      ELSE
      DO 100 I = 1, NMES
         CC = MSG(I)
         IMESSG(I) = ICHAR(CC)
  100 CONTINUE
      DO 150 I = NMES+1, 2048
         IMESSG(I) = 0
  150 CONTINUE
      CALL XERRWND_C(IMESSG,NMES,NERR,LEVEL,N1,I1,I2,NNR,R1,R2) 
      END IF
#endif
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
C*RB
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
C*RE
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

