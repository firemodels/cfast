	SUBROUTINE VERSIONOUT (IV)

!	A routine to put the header information in the output file. 
!	We assume the file is open

      include "precis.fi"
      include "cfast.fi"
      include "cfin.fi"
      include "cshell.fi"

!>>>>> IV should be the same as version
      call splitversion(version,imajor,iminor,iminorrev)
	
      if (iminorrev.ge.10) then
        WRITE (logerr,10) imajor, iminor, iminorrev, CRDATE(1), 
     +      CRDATE(2), CRDATE(3)
      else
        WRITE (logerr,20) imajor, iminor, iminorrev, CRDATE(1), 
     +      CRDATE(2), CRDATE(3)
      end if
	RETURN

10    FORMAT ('Version ',i1,'.',i1,'.',I2,' Created ',I4.4,'/',I2.2,
     +        '/',I2.2)
20    FORMAT ('Version ',i1,'.',i1,'.',I1,' Created ',I4.4,'/',I2.2,
     +        '/',I2.2)
	END

	subroutine splitversion(version,imajor,iminor,iminorrev)
	integer version,imajor,iminor,iminorrev
	if (version.ge.1000) then
	  imajor = version / 1000
	  iminor = mod(version,1000) / 100
	  iminorrev = mod(version,100)
	else
	  imajor = version / 100
	  iminor = mod(version,100) / 10
	  iminorrev = mod(version,10)
	end if
	return
	end
