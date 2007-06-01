      SUBROUTINE GRABKY(ICH,IT)

      USE DFLIB

      CHARACTER*1 CH

      INTEGER*2 ICH, IT
      
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

