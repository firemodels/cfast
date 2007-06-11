      subroutine integrate mass (time, deltt)
      
!   Routine to integrate the pyrolosate of objects

!     deltt is the time step
!     we also integrate the trace species release and total for all fires

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"    

      integer i, j
      double precision xx0,xx1,filter,qcifraction,time,deltt
      data xx0/0.0d0/, xx1/1.0d0/
     
      do i = 0, numobjl
        objmaspy(i) = objmaspy(i) + femp(i)*deltt
        radio(i) = radio(i) + femr(i)*deltt
      end do
      
! sum the radiological release from all of the fires

      tradio = xx0
      do i = 0, numobjl
        tradio = tradio + radio(i)
      end do

! sum the hvac flow

      do irm = 1, n
        DO II = 1, NEXT
            I = HVNODE(1,II)
            J = HVNODE(2,II)
            ISYS = IZHVSYS(J)       
	      filter = (xx1-qcifraction(qcvf,isys,time)) 
	      if (irm.eq.i) then
	          hveflot(upper,ii) = hveflot(upper,ii) + hveflo(upper,ii)
	          hveflot(lower,ii) = hveflot(lower,ii) + hveflo(lower,ii)	  	 
	          tracet(upper,ii)  = tracet(upper,ii) + 
     .                       hveflo(upper,ii)*hvexcn(ii,11,upper)*filter
	          tracet(lower,ii)  = tracet(lower,ii) + 
     .                       hveflo(lower,ii)*hvexcn(ii,11,lower)*filter
	      endif 
        end do
      end do
      
      return
      end
  
