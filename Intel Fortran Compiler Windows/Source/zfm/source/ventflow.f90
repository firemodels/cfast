
! --------------- hventflow ----------------------
   
subroutine hventflow
  use precision
  use zonedata
  use zoneinterfaces

  implicit none
  type(vent_data), pointer :: v
  integer :: ivent

 ! calculate horizontal flows through vents

  call getventslabs

 ! determine where these flows go

  do ivent = 1, nvents
    v => vents(ivent)
    call flowgo(v%slab,v%nslabs,v%slab,v%nslabs,hflow)
  end do

end subroutine hventflow

! --------------- flowgo ----------------------

subroutine flowgo(slabfrom,nfromslab,slabto,ntoslab,odeflow)
  use precision
  use zonedata
  use zoneinterfaces

  implicit none
  integer, intent(in) :: nfromslab, ntoslab
  type(slab_data), target, dimension(nfromslab) :: slabfrom
  type(slab_data), target, dimension(ntoslab) :: slabto
  type(flow_data), dimension(0:nrooms,2) :: odeflow

  type(room_data), pointer :: toroom
  type(slab_data), pointer :: slab
  type(flow_data), pointer :: slab_flow, entrain_flow
  real(kind=eb), pointer :: tslab
  real(kind=eb) :: tlower, tupper, f_lower, f_upper
  integer :: islab, from, to
  real(kind=eb), parameter :: dtempmin=1.0_eb

  do islab = 1, nfromslab
    slab => slabfrom(islab)
    slab_flow => slab%slab_flow
                                             ! take flow out of "from" room according to slab elevation

    from = slab%from
    if(slab_flow%zero)cycle
    if(from.eq.0)cycle
    to = slab%to
	  toroom => rooms(to)

    if(slab%height.lt.rooms(from)%abs_layer_height)then
      odeflow(from,lower) = odeflow(from,lower) - slab_flow

     else
      odeflow(from,upper) = odeflow(from,upper) - slab_flow
    endif
  end do

  do islab = 1, ntoslab
    slab => slabto(islab) 
    to = slab%to
	  toroom => rooms(to)


    slab_flow => slab%slab_flow
    entrain_flow => slab%entrain_flow

	  entrain_flow = zeroflow
    if(to.eq.0)cycle
	  if(slab_flow%zero)cycle

! compute entrained vent flow

    call v_entrain(toroom,slab_flow,entrain_flow)

! put slab flow into "to" flow according to slab temperature

    tslab => slab_flow%temperature
    tupper = rooms(to)%layer(upper)%temperature
    tlower = rooms(to)%layer(lower)%temperature

    if(tslab.ge.tupper+dtempmin)then
      f_upper = 1.0_eb
     elseif(tslab.le.tlower-dtempmin)then
      f_upper = zero
     else
      f_upper = (tslab - (tlower-dtempmin))/(tupper-tlower+2.0_eb*dtempmin)
    endif

    f_lower = 1.0_eb - f_upper
    if(f_upper.ne.zero)hflow(to,upper) = hflow(to,upper) + f_upper*slab_flow
    if(f_lower.ne.zero)hflow(to,lower) = hflow(to,lower) + f_lower*slab_flow

    if(.not.entrain_flow%zero)then
      if(entrain_flow%fromlower)then
        hflow(to,upper) = hflow(to,upper) + entrain_flow
        hflow(to,lower) = hflow(to,lower) - entrain_flow
      endif
      if(entrain_flow%fromupper)then
        hflow(to,upper) = hflow(to,upper) - entrain_flow
        hflow(to,lower) = hflow(to,lower) + entrain_flow
      endif
    endif

       
  end do

end subroutine flowgo

! --------------- getventslabs ----------------------

subroutine getventslabs
  use precision
  use zonedata
  implicit none

  type(vent_data), pointer :: v
  real(kind=eb), dimension(7) :: yelev1, dp1
  real(kind=eb), dimension(6), pointer :: dpslab(:)
  real(kind=eb), dimension(6) :: abs_slab_height
  type(flow_data), pointer :: slab_flow
  type(zone_data), pointer :: fromlayer
  type(slab_data), pointer :: slab
  real(kind=eb), pointer :: area
  real(kind=eb) :: yb, y1, y2, yt, y12min, y12max
  real(kind=eb) :: yy, dptop, dpbot, dptopsq, dpbotsq, dpavg
  real(kind=eb) :: getdp
  real(kind=eb) :: f1, f2
  integer :: ivent, nslab, nelev2, islab
  integer :: ielev
  integer :: from, to, fromlayer_index, tolayer_index

  do ivent = 1, nvents

  ! set pointers for vents, elevation and dp arrays
    v => vents(ivent)
    dpslab => v%dpslab

  ! find slabs (ignoring neutral planes)

    nslab = 1
    yb = v%absbot
    y1 = rooms(v%from)%abs_layer_height
    y2 = rooms(v%to)%abs_layer_height
    yt = v%abstop

    yelev1(1) = yb
    y12min = min(y1,y2)
    y12max = max(y1,y2)
    if(y12min.gt.yb.and.y12min.lt.yt)then
      nslab = nslab + 1
      yelev1(nslab) = y12min
    end if
    if(y12max.gt.yb.and.y12max.lt.yt.and.y12min.ne.y12max)then
      nslab = nslab + 1
      yelev1(nslab) = y12max
    end if
    yelev1(nslab + 1) = yt

  !  now account for neutral planes

    nelev2 = 1
    dp1(nelev2) = getdp(yelev1(nelev2),ivent)
    v%dpelev(nelev2) = dp1(nelev2)
    v%abs_yelev(nelev2) = yb
	  v%nneutrals = 0
    do ielev = 2, nslab + 1
      dp1(ielev) = getdp(yelev1(ielev),ivent)
      if(dp1(ielev)*dp1(ielev-1).lt.zero)then
	      v%nneutrals = v%nneutrals + 1
        nelev2 = nelev2 + 1
        v%dpelev(nelev2) = zero
		    f1 = dp1(ielev)/(dp1(ielev)-dp1(ielev-1))
		    f2 = -dp1(ielev-1)/(dp1(ielev)-dp1(ielev-1))
		    yy = f1*yelev1(ielev-1) + f2*yelev1(ielev)
        v%abs_yelev(nelev2) = yy
		    v%yneutral(v%nneutrals) = yy
      end if
      nelev2 = nelev2 + 1
      v%abs_yelev(nelev2) = yelev1(ielev)
      v%dpelev(nelev2) = dp1(ielev)
    end do

    nslab = nelev2 - 1
	  v%nslabs = nslab

    do islab = 1, nslab

	    slab => v%slab(islab)
	    slab%bot = v%abs_yelev(islab)
	    slab%top = v%abs_yelev(islab+1)

    ! define pointers to zone modeling objects

      area => slab%area
      area = v%width*(slab%top-slab%bot)
      slab_flow => slab%slab_flow

    ! define slab delta pressures and elevations

      dpslab(islab) = (v%dpelev(islab)+v%dpelev(islab+1))/2.
      abs_slab_height(islab) = (v%abs_yelev(islab) + v%abs_yelev(islab+1))/2.
	    slab%height = abs_slab_height(islab)

    ! determine where flow is coming

      if(dpslab(islab).gt.zero)then      ! which room
        from = v%from
        to = v%to
       else
        from = v%to
        to = v%from
      end if

	    slab%from = from
	    slab%to = to
	    slab_flow%abs_height = abs_slab_height(islab)
	    slab_flow%rel_height = abs_slab_height(islab) - rooms(to)%z0

      if(abs_slab_height(islab).gt.rooms(from)%abs_layer_height)then ! which layer
        fromlayer_index = upper
        slab_flow%fromupper = .true.
        slab_flow%fromlower = .false.
       else
        fromlayer_index = lower
        slab_flow%fromupper = .false.
        slab_flow%fromlower = .true.
      end if

      fromlayer => rooms(from)%layer(fromlayer_index)

      if(abs_slab_height(islab).gt.rooms(to)%abs_layer_height)then ! which layer
        tolayer_index = upper
       else
        tolayer_index = lower
      end if

      ! define slab properties

      slab_flow%temperature = fromlayer%temperature
      slab_flow%density = fromlayer%density
      if(slab_flow%density.lt.0.0_eb)then
        write(6,*)"ut oh from=",from,fromlayer%temperature
      endif

      dptop = abs(v%dpelev(islab))
      dpbot = abs(v%dpelev(islab+1))
      dptopsq = sqrt(dptop)
      dpbotsq = sqrt(dpbot)
      if(dpbotsq+dptopsq.eq.0.0_eb)then
        dpavg = 0.0_eb
       else
        dpavg = twothirds*(dptop+dptopsq*dpbotsq+dpbot)/(dpbotsq+dptopsq)
      endif
      if(dpavg.eq.0.0_eb)then
        slab_flow%zero = .true.
       else
        slab_flow%zero = .false.
      endif
      if(slab_flow%density.lt.0.0)then
        write(6,*)"ut oh"
      endif
      slab_flow%mdot = cvent*area*sqrt(slab_flow%density)*dpavg
      slab_flow%qdot =  cp*slab_flow%temperature*slab_flow%mdot
      slab_flow%sdot(1:nspecies) = fromlayer%s_con(1:nspecies)*slab_flow%mdot
    end do
  end do
end subroutine getventslabs
