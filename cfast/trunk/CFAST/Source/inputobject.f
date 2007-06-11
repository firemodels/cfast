	subroutine inputobject (objname, iobj, iounit, errorcode)

!  This routine reads the object files either from the current directory
!	or the directory for the executable. If it does not exist in either place, then
!	we quit

!  The object name is objname.  iobj is the counter in the main list of objects

!  The order of species is 'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O','OD', 'CT', 'TS'

!  The minimum separation between an object an a wall is 1mm (minimumseparation)
!  The maximum heat release rate (see reference in the UG and TR) is 4 MW/m^3 (the actual value should be 2, 
!     but where we cut the user some slack

      include "precis.fi"
      include "cfast.fi"
      include "iofiles77.fi"
      include "objects2.fi"
      include "fltarget.fi"

	character testpath*256 , objname*(*)
	integer numr, numc, i, j, iounit, logerr/3/, errorcode, 
     . midpoint/1/, base/2/
	logical exists, doesthefileexist
      data hcmax /5.0D8/, hcmin/1.31D+7/
	double precision minimumheight/1.d-3/, maximumhrr, xx0, hrrpm3

! First we try the local directory

	testpath = trim (datapath) // trim(objname) // '.o'
	exists = DoesTheFileExist(testpath)

! Then we try the executable root directory

	if (.not.exists) then
	  testpath = trim (exepath) // trim(objname) // '.o'
	  exists = DoesTheFileExist(testpath)

! All is lost

	  if (.not.exists) then
			write (logerr, 5000) trim(testpath)
			errorcode = 215
			return
	  endif
	endif

! All is not lost

	write (logerr, 5001) testpath
	
	close (iounit)
	open (unit=iounit,file=testpath,form='formatted')

!	Unlike the main fire, we need the header information

	call readcsvformat (iounit,rarray,carray,nrow,ncol,1,numr,numc,
     . errorcode)
	if (errorcode.gt.0) return
! Make sure we are reading the object we think we should have
	if(carray(1,1).ne.objname) then
		 errorcode = 210
		 write(logerr,5004) objname, carray(1,1)
		 return
	endif
	if(numc.lt.14) then
		write(logerr,5108) numc
	endif

! Copy the data into the appropriate arrays
!!!!!! Note, we assume that there cannot be more objects to initialize than there are objects in a calculation 

	xx0 = 0.0d0
	xx1 = 1.0d0

      OBJLFM(IOBJ) = rarray(2,1)
      OBJGMW(IOBJ) = rarray(3,1) * 1.0D+3
      OBJVT(IOBJ) = rarray(5,1)
	radconsplit(iobj) = rarray(7,1)
	if(radconsplit(iobj).gt.0.8d0) then
		write(logerr,5103) radconsplit(iobj)
		errorcode = 219
		return
	else if (radconsplit(iobj).gt.0.5d0) then
		write(logerr,5103) radconsplit(iobj)
	endif

      OHCOMB = rarray(12,1)
      NTARG = NTARG + 1
      IF (NTARG.GT.MXTARG) THEN
      	write(logerr, 5002) 
      	IERROR = 201
      	RETURN
      END IF
      OBTARG(IOBJ) = NTARG
      CXTARG(NTARG) = carray(13,1)

      OBJMAS(IOBJ) = rarray(8,1)
      OBJXYZ(1,IOBJ) = rarray(9,1)
      OBJXYZ(2,IOBJ) = rarray(10,1)
      OBJXYZ(3,IOBJ) = rarray(11,1)

! Calculate the characteristic size of an object.
! This is a basic conceptual model for the physical extent for the heat release
	objcl(iobj) = objxyz(1,iobj) * objxyz(2,iobj) * objxyz(3,iobj)
	if(objcl(iobj).lt.1.0d-6) then
		write(logerr,5005) iobj,objcl(iobj)
		errorcode = 220
		return
	endif
	objcl(iobj) = objcl(iobj)**0.333d0

      OTIME(1,IOBJ) = xx0

	if(objlfm(iobj).gt.nv) then
		 errorcode = 208
		 write (logerr,5003)
		 return
	endif

! This should not be important, but just in case

      do 300 i = 1, nv
      DO 300 II = 1, ns
      	OMPRODR(I,II,IOBJ) = xx0
  300 CONTINUE

! Move the array data into the object arrays

	maximumhrr = xx0
      DO 400 II = 1, OBJLFM(IOBJ)
         OTIME(II,IOBJ) = rarray(ii+1,2)
         OQDOT(II,IOBJ) = rarray(ii+1,3)
	   maximumhrr = max(maximumhrr, oqdot(ii,iobj))
         OMASS(II,IOBJ) = rarray(ii+1,4)
! This is to stop dassl from an floating point underflow when it tries to extrapolate back.
! It only occurs for objects which are on the floor and ignite after t=0
         OHIGH(II,IOBJ) = rarray(ii+1,5)
         OAREA(II,IOBJ) = rarray(ii+1,6)
         OOC(II,IOBJ) = rarray(ii+1,7)
         OHCR(II,IOBJ) = rarray(ii+1,8)
         OCO(II,IOBJ) = rarray(ii+1,9)
         OOD(II,IOBJ) = rarray(ii+1,10)
         OMPRODR(II,5,IOBJ) = rarray(ii+1,11)
         OMPRODR(II,6,IOBJ) = rarray(ii+1,12)
	   omprodr(ii,7,iobj) = xx1
         OMPRODR(II,10,IOBJ) = rarray(ii+1,13)
	   omprodr(ii,11,iobj) = rarray(ii+1,14)
  400 CONTINUE

	OTFMAXT(IOBJ) = OTIME(OBJLFM(IOBJ),IOBJ)

!	set the heat of combustion - this is a problem if the qdot is zero and the mdot is zero as well
	call sethoc (objlfm(iobj), omass(1,iobj), oqdot(1,iobj), 
     +             objhc(1,iobj), ohcomb)

!	Position the object

	call positionobject(objpos,1,iobj,objrm(iobj),br,
     . midpoint,minimumheight,errorcode)
	if (errorcode.ne.0) return
	call positionobject(objpos,2,iobj,objrm(iobj),dr,
     . midpoint,minimumheight,errorcode)
	if (errorcode.ne.0) return
	call positionobject(objpos,3,iobj,objrm(iobj),hr,
     . base,minimumheight,errorcode)
	if (errorcode.ne.0) return
	
! Diagnostic - check for the maximum heat release per unit volume.
 
!	First, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
	area = objxyz(1,iobj) * objxyz(2,iobj)
	d = max(0.33d0,sqrt(4.0/3.14*area))
	flamelength = d * (0.235d0*(maximumhrr/1.0d3)**0.4 - 1.02)
	flamelenght = max (xx0, flamelength)
!	Now the heat realease per cubic meter - we know that the size is larger than 1.0d-6 m^3 - enforced above
	hrrpm3 = maximumhrr/(area*(objxyz(3,iobj)+flamelength))
	if (hrrpm3.gt.4.0e+6) then
	  WRITE (LOGERR,5106) trim(objname),(OBJPOS(i,IOBJ),i=1,3),hrrpm3
	  errorcode = 221
	  return
	else if (hrrpm3.gt.2.0d+6) then
	  WRITE (LOGERR,5107) trim(objname),(OBJPOS(i,IOBJ),i=1,3),hrrpm3
	else 
	  WRITE (LOGERR,5100) trim(objname),(OBJPOS(i,IOBJ),i=1,3),hrrpm3
	endif

!	Initialize object target position

	CALL SETOBTRG (NTARG,IOBJ,IERROR)

	close (iounit)	

	return

 5000 format ('Cannot find the object fire file in either the' 
     . ' executable path or the local directory ',/,a)
 5001 format ('Open the object fire file ',a256)
 5002 FORMAT ('Too many targets are being defined in inputobject')
 5003 format ('Too many entries for the object file')
 5004 format ('Names do not match ',a8,2x,a8)
 5005 format ('Object # ',i3,' is too small. Volume = ',g10.3)
 5100 FORMAT ('Object ',a,' position set to ',3F7.3, 
     . '; Maximum HRR per m^3 is ',1pg10.3)
 5106 FORMAT ('Object ',a,' position set to ',3F7.3,
     . '; Maximum HRR per m^3 = ',1pg10.3,' exceeds physical limits')
 5107 FORMAT ('Object ',a,' position set to ',3F7.3,
     . '; Maximum HRR per m^3 = ',1pg10.3,' exceeds nominal limits')
 5103 format (
     . 'Radiation fraction for object fire is outside of a normal range'
     . f7.3)
 5108 format ('>>>Old fire object format - please update, count =',i3)


	end subroutine inputobject

