subroutine deleteoutputfiles (outputfile)

use dflib

character (*) outputfile
logical exists, doesthefileexist
integer (2) filecount

if (doesthefileexist(outputfile)) then
	filecount = delfilesqq(outputfile)
	if (filecount.lt.1) stop 104
endif

return
end
