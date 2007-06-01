        subroutine getvventinfo(iinvvent,itop,ibot,harea,hshape,hface)

!       This is a routine to get the shape data for vertical flow (horizontal) vents

      include "precis.fi"
      include "cfast.fi"
      include "vents.fi"

	integer itop, ibot, hshape, hface, iinvvent
	double precision harea

	itop = ivvent(iinvvent,1)
	ibot = ivvent(iinvvent,2)
	harea = vvarea(itop,ibot)
	hshape = vshape(itop,ibot)
	if (itop.gt.nm1) then
		hface = 6
	else
		hface = 5
	endif

      RETURN
      END
