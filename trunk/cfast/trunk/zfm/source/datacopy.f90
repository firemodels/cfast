
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
  	VOL(upper) = max(psolve(iroom+offset_vu),room%volmin)
    VOL(upper) = min(VOL(upper),room%volmax)
  	VOL(lower) = max(room%volume - VOL(upper),room%volmin)
    VOL(lower) = min(VOL(lower),room%volmax)
  	TEMP(lower) = psolve(iroom+offset_tl)
  	TEMP(upper) = psolve(iroom+offset_tu)

    if(solveoxy)then
     OXY(lower) = psolve(iroom+offset_oxyl)
     OXY(upper) = psolve(iroom+offset_oxyu)
    endif

  	pabs = pabs_ref + relp     ! copy data into zone modeling data structures

    room%rel_pressure = relp
    room%abs_pressure = pabs
  	room%VU = VOL(upper)
    room%VL = VOL(lower)

    do ilayer = 1, 2
      layer => room%layer(ilayer)
    	RHO(ilayer) = pabs/(rgas*TEMP(ilayer))
    	layer%temperature = TEMP(ilayer)
    	layer%density = RHO(ilayer)
    	layer%mass = RHO(ilayer)*VOL(ilayer)
      if(solveoxy)then
        layer%s_mass(oxygen) = OXY(ilayer)
        layer%s_con(oxygen) = OXY(ilayer)/layer%mass
        tanharg = layer%s_con(oxygen)-o2limit
        tanharg = 4*tanharg/0.01_dd
        layer%o2index = 0.50_dd*(1.0_dd+tanh(tanharg))
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
