	subroutine sethoc (maxint, mdot, qdot, hdot, hinitial)

!	Routine to implement the algorithm to set the heat of combustion for all fires

      include "precis.fi"

	double precision mdot(maxint), qdot(maxint), hdot(maxint)

      data hcmax /5.0D7/, hcmin/1.31D+7/
	
	do 600 i = 1, maxint
	if(i.gt.1) then
	    if (mdot(i)*qdot(i).le.0.d0) then
	  		hdot(i) = hinitial
	    else					
	  		Hdot(I) = min(hcmax,max(Qdot(I)/(mdot(I)),hcmin))
	    endif
	else
	    hdot(1) = hinitial
	endif
  600	continue

	return
	end subroutine sethoc
