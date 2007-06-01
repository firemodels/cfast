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
