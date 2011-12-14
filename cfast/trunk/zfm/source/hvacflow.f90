
! --------------- hvacflow ----------------------

subroutine hvacflow
  use precision
  use zonedata
  implicit none

  integer :: ihvac
  type(hvac_data), pointer :: h

 ! calculate horizontal flows through vents

  call gethvacslabs

 ! determine where these flows go

  do ihvac = 1, nhvacs
    h => hvacs(ihvac)
    call flowgo(h%fromslab,2,h%toslab,2,hvflow)
  end do

end subroutine hvacflow

! --------------- gethvacslabs ----------------------

subroutine gethvacslabs
  use precision
  use zonedata
  implicit none

  integer :: ihvac, layer
  type(hvac_data), pointer :: h
  type(room_data), pointer :: r
  type(flow_data), pointer :: slab_flow, totalslab_flow
  type(slab_data), pointer :: slab
  real(kind=dd) :: f_lower, f_upper, slabtemp, slabdensity
  
  do ihvac = 1, nhvacs
    h => hvacs(ihvac)
  	r => rooms(h%fromroom) 

	! compute fraction coming out of lower and upper layers in the from room

  	call getfrac(h%rel_frombot,h%rel_fromtop,r%rel_layer_height,f_lower,f_upper)

    h%fromslab(lower)%bot = h%abs_frombot
    h%fromslab(lower)%top = min(h%abs_fromtop,max(r%abs_layer_height,h%abs_frombot))
    h%fromslab(upper)%bot = min(h%abs_fromtop,max(r%abs_layer_height,h%abs_frombot))
    h%fromslab(upper)%top = h%abs_fromtop
    h%fromslab(lower)%height = (h%fromslab(lower)%bot + h%fromslab(lower)%top)/2.0_dd
    h%fromslab(upper)%height = (h%fromslab(upper)%bot + h%fromslab(upper)%top)/2.0_dd


	! compute "from slabs"

  	do layer = lower, upper
      slab_flow => h%fromslab(layer)%slab_flow
      slab => h%fromslab(layer)
  	  slab_flow=zeroflow
      slab_flow%fromlower=.false.
      slab_flow%fromupper = .false.
  	  if((layer.eq.lower.and.f_lower.ne.zero).or.(layer.eq.upper.and.f_upper.ne.zero))then
        if(layer.eq.lower)slab_flow%fromlower=.true.
        if(layer.eq.upper)slab_flow%fromupper=.true.
        if(h%specifiedtemp)then
          slabtemp=h%tfan
          slabdensity=h%rhofan
         else
          slabtemp=r%layer(layer)%temperature
          slabdensity=r%layer(layer)%density
        endif
        slab_flow%density = slabdensity
        slab_flow%temperature = slabtemp
  	    slab_flow%mdot = slabdensity*abs(h%vfan)
        if(h%vfan.gt.0)then
          slab%from=h%fromroom
          slab%to=h%toroom
         else
          slab%from=h%toroom
          slab%to=h%fromroom
        endif
    		slab_flow%qdot =  slabtemp*cp*slab_flow%mdot
        slab_flow%sdot =  r%layer(layer)%s_con*slab_flow%mdot
        slab_flow%zeroflowflag=.false.
  	  endif
  	end do

	! compute total slab

  	totalslab_flow => h%totalslab%slab_flow
  	totalslab_flow = f_lower*h%fromslab(lower)%slab_flow + f_upper*h%fromslab(upper)%slab_flow

	! compute fraction going into lower and upper layers in the to room

  	r=> rooms(h%toroom)

    h%toslab(lower)%bot = h%abs_frombot
    h%toslab(lower)%top = min(h%abs_fromtop,max(r%abs_layer_height,h%abs_frombot))
    h%toslab(upper)%bot = min(h%abs_fromtop,max(r%abs_layer_height,h%abs_frombot))
    h%toslab(upper)%top = h%abs_fromtop
    h%toslab(lower)%height = (h%toslab(lower)%bot + h%toslab(lower)%top)/2.0_dd
    h%toslab(upper)%height = (h%toslab(upper)%bot + h%toslab(upper)%top)/2.0_dd

  	call getfrac(h%rel_tobot,h%rel_totop,r%rel_layer_height,f_lower,f_upper)

  	do layer = lower, upper
      slab => h%toslab(layer)
      if(h%vfan.gt.0)then
        slab%from=h%fromroom
        slab%to=h%toroom
       else
        slab%from=h%toroom
        slab%to=h%fromroom
      endif
  	end do

	! compute "to slabs"

    slab_flow => h%toslab(lower)%slab_flow
  	slab_flow = zeroflow
  	if(f_lower.ne.zero)slab_flow = f_lower*totalslab_flow

    slab_flow => h%toslab(upper)%slab_flow
  	slab_flow = zeroflow
  	if(f_upper.ne.zero)slab_flow = f_upper*totalslab_flow


  end do
end subroutine gethvacslabs

! --------------- getfrac ----------------------

subroutine getfrac(hbot, htop, ylay, f_lower, f_upper)
  use precision
  use zonedata
  implicit none
  real(kind=dd), intent(in) :: hbot, htop, ylay 
  real(kind=dd), intent(out) :: f_lower, f_upper
  if(ylay.lt.hbot)then
    f_upper = 1.0_dd
   elseif(ylay.gt.htop)then
    f_upper = zero
   else
    f_upper = (htop - ylay)/(htop - hbot)
  endif
  f_lower = 1.0_dd - f_upper
  return
end subroutine getfrac

