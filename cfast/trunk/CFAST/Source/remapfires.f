	subroutine remapfires (nfires, flocal, fxlocal, fylocal, 
     . fzlocal, fqlocal, fhlocal)

C	This routine is to combine the main fire (in lfbo) and any objects into a single list
C	There does not have to be a main fire nor any objects, so NFIRES may be zero

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "smkview.fi"
      include "objects1.fi"
      include "objects2.fi"

C	First, the mainfire if there is one

	if (lfbo.gt.0) then
		nfires = 1
		flocal(1) = froom(0)
		fxlocal(1) = fposx(0)
		fylocal(1) = fposy(0)
		fzlocal(1) = fposz(0)
		call flamhgt (froom(0),fqf(0),farea(0),fheight)
		fqlocal(1) = fqf(0)
		fhlocal(1) = fheight
	else
		nfires = 0
	endif
	
C	Now the other objects

	do i = 1, numobjl
		nfires = nfires + 1
		fxlocal(nfires) = fposx(i)
		fylocal(nfires) = fposy(i)
		fzlocal(nfires) = fposz(i)
          call flamhgt (froom(i),fqf(i),farea(i),fheight)
		fqlocal(nfires) = fqf(i)
		fhlocal(nfires) = fheight
		flocal(nfires) = froom(i)
	end do

	return
	end
