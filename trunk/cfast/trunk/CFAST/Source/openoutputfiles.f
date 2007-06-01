	subroutine openoutputfiles

!	Now that we know what output is needed, open the appropriate files
!	Note that the sign of lprint determines whether we write to the console or  file
!	Unit numbers defined here and readinputfiles

!	Unit numbers defined in readop, openoutputfiles, readinputfiles
!
!      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
!      3 is for the log file  (LOGERR)
!	 4 is for the indicator that the model is running (kernelisrunning)
!      6 is output (IOFILO)
!     11 is the history file
!     12 is used to write the status file (project.status)
!     13 smokeview output (header) - note this is rewound each time the plot data is written)
!     14 smokeview output (plot data)
!     15 spreadsheet output (normal)
!     16 spreadsheet output (flow field)
!     17 spreadsheet output (species)
!     18 spreadsheet output (walls and targets)

!!!! Note that we assume that the default carraigecontrol for formatted files is of type LIST (no fortran controls)

      include "iofiles77.fi"
      include "cfast.fi"
      include "cshell.fi"

! first the file for "printed" output
	if (lprint.lt.0) then
		 open (unit=iofilo,file=outputfile,status='new',
     .		  carriagecontrol='fortran')
		 lprint = abs(lprint)
		 WRITE (LOGERR,5002) trim(outputfile)
		 if (outputformat.eq.0) outputformat = 2
	else
		 OPEN (UNIT=IOFILO,FILE='CON',CARRIAGECONTROL='FORTRAN')
		 write (logerr,5004)
		 if (outputformat.eq.0) outputformat = 1
	END IF

! next the history file
      IF (ldiago.gt.0) THEN
	   write(logerr,5001) trim(historyfile)
         OPEN (UNIT=11,FILE=historyfile,ERR=10,
     +		IOSTAT=IOS,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
      END IF 

! Next create the status file
	open (12,file=statusfile,ACCESS='APPEND',ERR=81,iostat=ios)

! Now the smokeview files
	if (ldiagp.gt.0) then
		 write(logerr,5003) trim(smvhead),trim(smvdata)
	    OPEN (UNIT=13,FILE=smvhead,form='formatted',err=11,
     .          iostat=ios)
		 OPEN (UNIT=14,FILE=smvdata,FORM="UNFORMATTED",err=11,
     .          iostat=ios)
	endif
 
! Next the spread sheet files
	if (lcopyss.gt.0) then
		 write(logerr,5005) trim(ssnormal),trim(ssflow),
     .                       trim(ssspecies),trim(sswall)
		 open (unit=15, file=ssnormal,form='formatted')
		 open (unit=16, file=ssflow,form='formatted')
		 open (unit=17, file=ssspecies,form='formatted')
		 open (unit=18, file=sswall,form='formatted')
	endif

! And finally we create a file to indicate that the model is running.

	open (unit=4, file=kernelisrunning, dispose='DELETE')

	return

C ERROR PROCESSING
 
!	History file
   10 WRITE (LOGERR,5030) MOD(IOS,256), trim(historyfile)
      STOP 105
!	Smokeview file
   11 write(logerr,5040) mod(ios,256),trim(smvhead),trim(smvdata)
	stop 105
!	This one comes from writing to the status file
   81 write(logerr,*) 'Fatal error writing to the status file ',ios
	STOP 106

 5001 format ('Open the history file ',a)
 5002 format ('Open the output file ',a)
 5003 format ('Open the smokeview files - ',a,2x,a)
 5004 format ('Send output to the consol')
 5005 format ('Open the spreadsheet files - ',4(a,2x))
 5030 FORMAT ('Error ',i4,' while accessing history, file = ',A)
 5040 FORMAT ('Error ',i4,' while processing smokeview files -'
     . ,i3,2x,a,2x,a)

	end
