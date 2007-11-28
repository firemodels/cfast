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

! tracet is the trace species which gets through the vent, traces is the mass stopped. Has to be calculated here since
! there is no equivalent to 1-... 

      do irm = 1, n
        DO II = 1, NEXT
            I = HVNODE(1,II)
            J = HVNODE(2,II)
            ISYS = IZHVSYS(J)       
	      filter = (xx1-qcifraction(qcvf,isys,time)) 
	      if (irm.eq.i) then
	         hveflot(upper,ii) = hveflot(upper,ii)+hveflo(upper,ii)*deltt
	         hveflot(lower,ii) = hveflot(lower,ii)+hveflo(lower,ii)*deltt 
	         tracet(upper,ii)  = tracet(upper,ii) + 
     .           hveflo(upper,ii)*hvexcn(ii,11,upper)*filter*deltt
	         tracet(lower,ii)  = tracet(lower,ii) + 
     .           hveflo(lower,ii)*hvexcn(ii,11,lower)*filter*deltt
	         traces(upper,ii)  = traces(upper,ii) + 
     .           hveflo(upper,ii)*hvexcn(ii,11,upper)*(xx1-filter)*deltt
	         traces(lower,ii)  = traces(lower,ii) + 
     .           hveflo(lower,ii)*hvexcn(ii,11,lower)*(xx1-filter)*deltt
	      endif 
        end do
      end do
      
      return
      end
  
