      subroutine getabstarget(targetnumber, positionvector)

!	Routine to get the absolute position of a target in the computational space

!	This is the protocol between cfast and smokeview

      include "precis.fi"
      include "cfast.fi"
      include "fltarget.fi"

      double precision positionvector(*)
      integer targetnumber

      do i = 1, 6
      positionvector (i) = xxtarg(i,targetnumber)
      end do

      positionvector(1) = positionvector(1) + 
     .                    cxabs(ixtarg(trgroom,targetnumber))
      positionvector(2) = positionvector(2) + 
     .                    cyabs(ixtarg(trgroom,targetnumber))
      positionvector(3) = positionvector(3) + 
     .                    hrl(ixtarg(trgroom,targetnumber))

      return

      end subroutine getabstarget
