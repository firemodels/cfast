	subroutine inputmainfire (iounit,errorcode)

!  This routine reads the "mainfire.o" file either from the current directory
!  or the directory for the executable. If it does not exist in either place, then
!  we quit

!  The order of species is 'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O','OD', 'CT', 'TS'

      include "precis.fi"
      include "cfast.fi"
      include "iofiles77.fi"

	character*10 data_file /'mainfire.o' /, testpath*256 
	integer numr, numc, i, j, iounit, logerr/3/, errorcode
	logical exists, doesthefileexist
      data hcmax /5.0D7/, hcmin/1.31D+7/

! First we try the local directory
	testpath = trim (datapath) // data_file 
	exists = DoesTheFileExist(testpath)

! Then we try the executable root directory
	if (.not.exists) then
	  testpath = trim (exepath) // data_file 
	  exists = DoesTheFileExist(testpath)
	  if (.not.exists) then
! All is lost
			write (logerr, 5000) trim(testpath)
			errorcode = 200
			return
	  endif
	endif
	write (logerr, 5001) testpath
	
	close (iounit)
	open (unit=iounit,file=testpath,form='formatted')

!	ignore the header line and start with row 2

	call readcsvformat (iounit,rarray,carray,nrow,ncol,2,numr,numc,
     . errorcode)
	if (errorcode.gt.0) return

!	First, get the scalar data; for the mainfire, we do not check the name

	lfmax = rarray (1,1)
	xx1 = 1.0d0

!	Check array limits
	if (lfmax.gt.nv) then
		 ierror = 209
		 write(logerr,5002)
		 return
	endif

!	Note that we are fudging this; gmwf is in m/kg, but cfast is expecting m/g
	GMWF = rarray(2,1) * 1.0D3
	te = rarray(4,1)
!	Note that we are not using the heat of gasification at the moment

	radconsplit(0) = rarray(6,1)
!	Fires do not radiate below a fraction of 0.8
	if(radconsplit(0).gt.0.8d0) then
		write(logerr,5003) radconsplit(0)
		errorcode = 219
		return
	else if (radconsplit(0).gt.0.5d0) then
		write(logerr,5003) radconsplit(0)
	endif

	HCOMBA = rarray(11,1)

! This should not be important, but just in case

      do 300 i = 1, nv
      DO 300 II = 1, ns
      	MPRODR(I,II) = xx0
  300 CONTINUE
	
      DO 400 I = 1, NV
        HOCBMB(I) = HCOMBA
  400 CONTINUE

	do 1 i = 1, lfmax
	tfired(i) = rarray(i,2)
	qfired(i) = rarray(i,3)
	bfired(i) = rarray(i,4)
	hfired(i) = rarray(i,5)
	afired(i) = rarray(i,6)
      OCRATI(I) = rarray(i,7)
	hcratio(i) = rarray(i,8)
	COCO2(I) = rarray(i,9)
      CCO2(I) = rarray(i,10)
      HCNF(I) = rarray(i,11)
      HCLF(I) = rarray(i,12)
      hcrf(i) = rarray(i,14)
!	Note that CT, TUHC and TS are carried in the mprodr array - all other species have their own array
	mprodr(i,7) = xx1
	mprodr(i,10) = rarray(i,13)
	mprodr(i,11) = rarray(i,14)

    1	continue

	call sethoc (lfmax, bfired, qfired, hocbmb, hcomba)

	close (iounit)	

 5001 format ('Open mainfire object file ',a256)
 5000 format ('Cannot find the main object file in either the' 
     . ' executable path or the local directory ',/,a)
 5002	format ('Too many entries for the main fire')
 5003 format (
     . 'Radiation fraction for main fire is outside of a normal range',
     .  f7.2)

	end subroutine inputmainfire

