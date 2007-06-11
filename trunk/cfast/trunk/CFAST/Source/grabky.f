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

