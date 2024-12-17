
! --------------- datacopy ----------------------

subroutine datacopy(tsec,psolve)
  use precision
  use zonedata
  implicit none

  real(kind=eb) :: relp, pabs
  real(kind=eb), intent(in) :: tsec
  real(kind=eb), dimension(*) :: psolve
  real(kind=eb), dimension(2) :: VOL, TEMP, RHO, OXY
  real(kind=eb), dimension(maxspecies,2) :: vSPECIES
  type(room_data), pointer :: room
  type(zone_data), pointer :: layer
  integer :: iiroom, iroom, ilayer, iwall, ispec

  iiroom = 0
  do iroom = 1, nrooms
	  room => rooms(iroom)  ! get room pointer

  	relp = psolve(iroom+offset_p) ! copy data out of solver array
  	pabs = pabs_ref + relp     ! copy data into zone modeling data structures

    room%rel_pressure = relp
    room%abs_pressure = pabs

    if(room%singlezone.eq.0)then
      iiroom = iiroom + 1
      VOL(upper) = psolve(iiroom+offset_vu)
     else
      VOL(upper) = room%volmax
    endif
    if(VOL(upper).lt.room%volmin)then
      VOL(upper) = room%volmin
      VOL(lower) = room%volmax
      TEMP(lower) = psolve(iroom+offset_tl)
    	TEMP(upper) = TEMP(lower)
    	RHO(lower) = pabs/(rgas*TEMP(lower))
    	RHO(upper) = RHO(lower)
      if(solveoxy)then
        OXY(lower) = max(psolve(iroom+offset_oxyl),0.0_eb)
        OXY(upper) = (OXY(lower)/(RHO(lower)*VOL(lower)))*RHO(upper)*VOL(upper)
      endif
      if(solveprods)then
        do ispec = 2, maxspecies
          vSPECIES(ispec,lower) = max(psolve(iroom+offset_SPECIES(ispec,lower)),0.0_eb)
          vSPECIES(ispec,upper) = (vSPECIES(ispec,lower)/(RHO(lower)*VOL(lower)))*RHO(upper)*VOL(upper)
        end do
      endif

     elseif(VOL(upper).gt.room%volmax)then
      VOL(upper) = room%volmax
      VOL(lower) = room%volmin
    	TEMP(upper) = psolve(iroom+offset_tu)
    	TEMP(lower) = TEMP(upper)
    	RHO(upper) = pabs/(rgas*TEMP(upper))
    	RHO(lower) = RHO(upper)
      if(solveoxy)then
        OXY(upper) = max(psolve(iroom+offset_oxyu),0.0_eb)
        OXY(lower) = (OXY(upper)/(RHO(upper)*VOL(upper)))*RHO(lower)*VOL(lower)
      endif
      if(solveprods)then
        do ispec = 2, maxspecies
          vSPECIES(ispec,upper) = max(psolve(iroom+offset_SPECIES(ispec,upper)),0.0_eb)
          vSPECIES(ispec,lower) = (vSPECIES(ispec,upper)/(RHO(upper)*VOL(upper)))*RHO(lower)*VOL(lower)
        end do
      endif
     else
    	TEMP(upper) = psolve(iroom+offset_tu)
    	RHO(upper) = pabs/(rgas*TEMP(upper))
      if(solveoxy)OXY(upper) = max(psolve(iroom+offset_oxyu),0.0_eb)
      if(solveprods)then
        do ispec = 2, maxspecies
          vSPECIES(ispec,upper) = max(psolve(iroom+offset_SPECIES(ispec,upper)),0.0_eb)
        end do
      endif


      VOL(lower) = room%volume - VOL(upper)
      if(room%singlezone.eq.0)then
      	TEMP(lower) = psolve(iiroom+offset_tl)
      	RHO(lower) = pabs/(rgas*TEMP(lower))
        if(solveoxy)OXY(lower) = max(psolve(iiroom+offset_oxyl),0.0_eb)
        if(solveprods)then
          do ispec = 2, maxspecies
            vSPECIES(ispec,lower) = max(psolve(iiroom+offset_SPECIES(ispec,lower)),0.0_eb)
          end do
        endif
       else
      	TEMP(lower) = TEMP(upper)
      	RHO(lower) = RHO(upper)
        if(solveoxy)OXY(lower) = OXY(upper)
        if(solveprods)then
          do ispec = 2, maxspecies
            vSPECIES(ispec,lower) = vSPECIES(ispec,upper)
          end do
        endif
      endif
    endif

  	room%VU = VOL(upper)
    room%VL = VOL(lower)

    do ilayer = 1, 2
      layer => room%layer(ilayer)
    	layer%temperature = TEMP(ilayer)
    	layer%density = RHO(ilayer)
    	layer%mass = RHO(ilayer)*VOL(ilayer)
      if(solveoxy)then
        layer%s_mass(oxygen) = OXY(ilayer)
        layer%s_con(oxygen) = OXY(ilayer)/layer%mass
      endif
      if(solveprods)then
        do ispec = 2, maxspecies
          layer%s_mass(ispec) = vSPECIES(ispec,ilayer)
          layer%s_con(ispec) = vSPECIES(ispec,ilayer)/layer%mass
        end do
      endif
    end do

  	room%rel_layer_height = room%dz - VOL(upper)/room%floor_area
  	room%abs_layer_height = room%z0 + room%rel_layer_height

    room%wall(3)%area = 2.0_eb*(room%dx+room%dy)*(room%dz-room%rel_layer_height)
    room%wall(4)%area = 2.0_eb*(room%dx+room%dy)*room%rel_layer_height
    do iwall=1,4
      room%wall(iwall)%temp=tamb
    end do

  end do

  call setfireflow(tsec)

end subroutine datacopy
