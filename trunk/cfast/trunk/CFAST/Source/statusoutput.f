	subroutine StatusOutput (T, dT, errorcode)
	
!  Write the status information to the "statusfile"

      include "precis.fi"
      include "cfast.fi"

	integer errocode	
	double precision T, dT

	rewind (12)
	write(12,5001) t, dt
     	call rsltcmp (12)
	errorcode = 0
	return
	
5001	FORMAT('Status at T = ',1PG11.2, ' DT = ',G11.3)
	end

