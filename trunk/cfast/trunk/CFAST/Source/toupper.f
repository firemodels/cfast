      CHARACTER FUNCTION TOUPPER(CH)
      CHARACTER*1 CH, TOLOWER
C
C     CONVERT TO UPPER CASE
C
#ifdef pp_ibmpc
      ICH = ICHAR(CH)
      IF (ICH.GT.96.AND.ICH.LT.123) ICH = ICH - 32
      TOUPPER = CHAR(ICH)
#else
      IF(CH.GE.'a'.and.CH.LE.'z')THEN
         TOUPPER = CHAR(ICHAR(CH) + ICHAR('A') - ICHAR('a'))
      ELSE 
         TOUPPER = CH
      ENDIF
#endif
      RETURN
C
C     COVERT TO LOWER CASE
C
      ENTRY TOLOWER(CH)
#ifdef pp_ibmpc
      ICH = ICHAR(CH)
      IF (ICH.GT.64.AND.ICH.LT.91) ICH = ICH + 32
      TOLOWER = CHAR(ICH)
#else
      IF(CH.GE.'A'.and.CH.LE.'Z')THEN
         TOLOWER = CHAR(ICHAR(CH) + ICHAR('a') - ICHAR('A'))
      ELSE 
         TOLOWER = CH
      ENDIF
#endif
      RETURN
      END
