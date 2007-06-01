	subroutine inputtpp (tppname, errorcode)

!  This routine reads the thermophysical properties file either from the current directory
!  or the directory for the executable. If it does not exist in either place, then we quit


      include "precis.fi"
      include "cfast.fi"
      include "iofiles77.fi"
      include "objects2.fi"
      include "fltarget.fi"
      include "thermp.fi"

	character testpath*256, tppname*(*)
	integer numr, numc, i, j, iounit, logerr/3/, errorcode
	logical exists

! First we try the local directory
	testpath = trim (datapath) // trim(tppname) // '.csv'
	inquire (file = testpath, exist = exists)
! Then we try the executable root directory
	if (.not.exists) then
	  testpath = trim (exepath) // trim(tppname) // '.csv'
	  inquire (file=testpath, exist=exists)
	  if (.not.exists) then
! All is lost
			write (logerr, 5000) trim(testpath)
			errorcode = 202
			return
	  endif
	endif
	write (logerr, 5001) testpath
	
	close (iounit)
	open (unit=iounit,file=testpath,form='formatted')

! The first entry is the first property, so start at the beginning

	call readcsvformat (iounit,rarray,carray,nrow,ncol,1,numr,numc,
     . errorcode)
      if (errorcode.gt.0) return

! Check to make sure we do not overwrite the tpp data structures

! Too many TPPs.
	if (numr.gt.nthmx) then
		 ierror = 203
		 return
	endif
! Data format is not correct
	if (numc.lt.14) then
		 ierror = 204
		 return
	endif
		
! Copy the data into the appropriate arrays

	do 10 i = 1, numr
	nlist(i) = carray(i,1)
	lnslb(i) = 1
	DO 30 K = 1, lNSLB(I)
	 	lFKW(K,I) = rarray(i,2)
	 	lCW(K,I) = rarray(i,3)
	 	lRW(K,I) = rarray(i,4)
	 	lFLW(K,I) = rarray(i,5)
   30 CONTINUE
      lEPW(I) = rarray(i,6)
      DO 40 K = 1, 7
   40 lHCLBF(K,I) = rarray(i,6+k)

   10 CONTINUE

! Finally we put in the default properties; this also becomes the size of the database
	maxct = numr + 1

      NLIST(maxct) = 'DEFAULT'
      LNSLB(maxct) = 1
      LFKW(1,maxct) = 0.120D0
      LCW(1,maxct) = 900.0D0
      LRW(1,maxct) = 800.0D0
      LFLW(1,maxct) = 0.0120D0
      LEPW(maxct) = 0.90D0
      DO 50 I = 1, 7
        LHCLBF(I,maxct) = 0.00D0
   50 CONTINUE

	close (iounit)	
	return

 5000 format ('Cannot find the thermophysical properties file in '
     . 'either the executable path or the local directory ',/,a)
 5001 format ('Open the thermophysical properties file ',a256)

	end subroutine inputtpp
