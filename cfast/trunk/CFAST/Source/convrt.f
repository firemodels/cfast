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
