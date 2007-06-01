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
CHARACTER(128) dir(2)
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

1 continue

! Now check that the project.in file exists - this is the data file

buf = ' '
if (le(2).ne.0) then
	 buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // ext(2)(1:le(2)) // '.in'
else
	 buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // '.in'
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

END
