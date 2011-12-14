
! --------------- datacopy ----------------------

subroutine datacopy(tsec,psolve)
  use precision
  use zonedata
  implicit none

  real(kind=dd) :: relp, pabs
  real(kind=dd), intent(in) :: tsec
  real(kind=dd), dimension(*) :: psolve
  real(kind=dd), dimension(2) :: VOL, TEMP, RHO, OXY
  real(kind=dd) :: tanharg
  type(room_data), pointer :: room
  type(zone_data), pointer :: layer
  integer :: iroom, ilayer, iwall

  do iroom = 1, nrooms
	  room => rooms(iroom)  ! get room pointer

  	relp = psolve(iroom+offset_p) ! copy data out of solver array
!  	VOL(upper) = max(psolve(iroom+offset_vu),room%volmin)
!    VOL(upper) = min(VOL(upper),room%volmax)
!  	VOL(lower) = max(room%volume - VOL(upper),room%volmin)
!    VOL(lower) = min(VOL(lower),room%volmax)
!  	TEMP(lower) = psolve(iroom+offset_tl)
!  	TEMP(upper) = psolve(iroom+offset_tu)

!    if(solveoxy)then
!     OXY(lower) = max(psolve(iroom+offset_oxyl),0.0_dd)
!     OXY(upper) = max(psolve(iroom+offset_oxyu),0.0_dd)
!    endif

  	pabs = pabs_ref + relp     ! copy data into zone modeling data structures

    room%rel_pressure = relp
    room%abs_pressure = pabs

    VOL(upper) = psolve(iroom+offset_vu)
    if(VOL(upper).lt.room%volmin)then
      VOL(upper) = room%volmin
      VOL(lower) = room%volmax
    	TEMP(lower) = psolve(iroom+offset_tl)
    	TEMP(upper) = TEMP(lower)
    	RHO(lower) = pabs/(rgas*TEMP(lower))
    	RHO(upper) = RHO(lower)
      if(solveoxy)then
        OXY(lower) = max(psolve(iroom+offset_oxyl),0.0_dd)
        OXY(upper) = (OXY(lower)/(RHO(lower)*VOL(lower)))*RHO(upper)*VOL(upper)
      endif
     elseif(VOL(upper).gt.room%volmax)then
      VOL(upper) = room%volmax
      VOL(lower) = room%volmin
    	TEMP(upper) = psolve(iroom+offset_tu)
    	TEMP(lower) = TEMP(upper)
    	RHO(upper) = pabs/(rgas*TEMP(upper))
    	RHO(lower) = RHO(upper)
      if(solveoxy)then
        OXY(upper) = max(psolve(iroom+offset_oxyu),0.0_dd)
        OXY(lower) = (OXY(upper)/(RHO(upper)*VOL(upper)))*RHO(lower)*VOL(lower)
      endif
     else
    	TEMP(lower) = psolve(iroom+offset_tl)
    	TEMP(upper) = psolve(iroom+offset_tu)
      VOL(lower) = room%volume - VOL(upper)
    	RHO(lower) = pabs/(rgas*TEMP(lower))
    	RHO(upper) = pabs/(rgas*TEMP(upper))
      if(solveoxy)then
        OXY(lower) = max(psolve(iroom+offset_oxyl),0.0_dd)
        OXY(upper) = max(psolve(iroom+offset_oxyu),0.0_dd)
      endif
    endif

  	room%VU = VOL(upper)
    room%VL = VOL(lower)

    do ilayer = 1, 2
      layer => room%layer(ilayer)
!    	RHO(ilayer) = pabs/(rgas*TEMP(ilayer))
    	layer%temperature = TEMP(ilayer)
    	layer%density = RHO(ilayer)
    	layer%mass = RHO(ilayer)*VOL(ilayer)
      if(solveoxy)then
        layer%s_mass(oxygen) = OXY(ilayer)
        layer%s_con(oxygen) = OXY(ilayer)/layer%mass
      endif
    end do

  	room%rel_layer_height = room%dz - VOL(upper)/room%floor_area
  	room%abs_layer_height = room%z0 + room%rel_layer_height

    room%wall(3)%area = 2.0_dd*(room%dx+room%dy)*(room%dz-room%rel_layer_height)
    room%wall(4)%area = 2.0_dd*(room%dx+room%dy)*room%rel_layer_height
    do iwall=1,4
      room%wall(iwall)%temp=tamb
    end do

  end do

  call setfireflow(tsec)

end subroutine datacopy
