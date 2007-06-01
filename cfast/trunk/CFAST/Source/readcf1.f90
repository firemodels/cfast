subroutine readcf1 (errorcode)

!     Routines:    READCF1, 2 AND 3
!
!     Source File: READCF1.F90 (free form)
!
!     Functional Class:

! 	get the paths and project base name
! 	open the input file for reading (1)
! 	delete the output files
! 	open the log file (3)
! 	call the input routines

integer errorcode , lp, ld, lf, ios
integer(2) filecount
logical exists

character*256 testpath, testproj 

include "iofiles.fi"

! get the path and project names

errorcode = 0
call exehandle (exepath, datapath, project, errorcode)
if (errorcode.gt.0) return

! form the file names

! datafiles: inputfile, outputfile, smvhead, smvdata, ssflow, ssnormal, ssspecies, sswall

	testpath = trim (datapath)
	lp = len_trim (testpath)
	testproj = trim (project)
	ld = len_trim (testproj)
	inputfile = testpath(1:lp) // testproj(1:ld) // '.in'
	outputfile = testpath(1:lp) // testproj(1:ld) // '.out'
	smvhead = testpath(1:lp) // testproj(1:ld) // '.smv'
	smvdata = testpath(1:lp) // testproj(1:ld) // '.plt'
	ssflow = testpath(1:lp) // testproj(1:ld) // '.f.csv'
	ssnormal = testpath(1:lp) // testproj(1:ld) // '.n.csv'
	ssspecies = testpath(1:lp) // testproj(1:ld) // '.s.csv'
	sswall = testpath(1:lp) // testproj(1:ld) // '.w.csv'
	errorlogging = testpath(1:lp) // testproj(1:ld) // '.log'
	stopfile = testpath(1:lp) // testproj(1:ld) // '.stop'
	historyfile = testpath(1:lp) // testproj(1:ld) // '.hi'
	queryfile = testpath(1:lp) // testproj(1:ld) // '.query'
	statusfile = testpath(1:lp) // testproj(1:ld) // '.status'
	kernelisrunning = testpath(1:lp) // testproj(1:ld) // '.kernelisrunning'

	testpath = trim (exepath)
	lp = len_trim (testpath)
	solverini = testpath(1:lp) // 'solver.ini'

open (unit=1, file=inputfile, action='read', status='old', iostat=ios)

call deleteoutputfiles (outputfile)
call deleteoutputfiles (smvhead)	
call deleteoutputfiles (smvdata)
call deleteoutputfiles (ssflow)
call deleteoutputfiles (ssnormal)
call deleteoutputfiles (ssspecies)
call deleteoutputfiles (sswall)
call deleteoutputfiles (errorlogging)
call deleteoutputfiles (stopfile)
call deleteoutputfiles (historyfile)
call deleteoutputfiles (statusfile)
call deleteoutputfiles (queryfile)
call deleteoutputfiles (kernelisrunning)

! since we have reached this point, the output files are avaiable and stop has been turned off.
! open the log file and return the correct project name

open (unit=3, file=errorlogging, action='write', iostat=ios, status='new')

	project = testproj (1:ld)
	errorcode = ios

return

end
