subroutine init0
  use precision
  use zonedata
  implicit none

  noldfires=0

return
end subroutine init0

! --------------- initmm ----------------------

subroutine initmm
  use precision
  use zonedata
  implicit none

  smvfile = ""
  plotfile = ""
  zeroflow%mdot = zero       ! define the zeroflow data type
  zeroflow%qdot = zero
  zeroflow%temperature = tamb
  zeroflow%density = rhoamb
  zeroflow%sdot = 0.0_dd
  zeroflow%fromlower = .false.
  zeroflow%fromupper = .false.
  zeroflow%zeroflowflag = .true.
  
  amb_oxy_con = 0.23_dd
  heat_c=18000000.0_dd
  heat_o2 = 13200000.0_dd
  o2limit = 0.10_dd
  chi_rad = 0.35_dd
  tprint = 0.0_dd
  tdump = 0.0_dd
  tplot = 0.0_dd
  iplot = 0
  iprint = 0
  idump = 0
  debugprint = .false.
  call setstate(.true.)
end subroutine initmm

! --------------- initamb ----------------------

subroutine initamb
  use precision
  use zonedata
  implicit none
  integer :: iroom, ihvac
  type(room_data), pointer :: r
  type(hvac_data), pointer :: h
  type(vent_data), pointer :: v
  type(zone_data), pointer :: layer
  type(fire_data), pointer :: f
  type(wall_data), pointer :: w
  integer :: error, ilay , ivent, ifire, iwall
  
  offset_p = 0  ! define offsets used to access solver array
  offset_vu = offset_p + nrooms
  offset_tl = offset_vu + nrooms
  offset_tu = offset_tl + nrooms
  offset_oxyl = offset_tu + nrooms
  offset_oxyu = offset_oxyl + nrooms
  neq = 4*nrooms
#ifdef pp_solveoxy
  neq = 6*nrooms
#endif

  ! allocate space for solver arrays and error tolerance arrays

  if(allocated(vatol))deallocate(vatol)
  if(allocated(vrtol))deallocate(vrtol)
  if(allocated(pprime))deallocate(pprime)
  if(allocated(p))deallocate(p)
  if(allocated(pdzero))deallocate(pdzero)
  if(allocated(delta))deallocate(delta)
  if(allocated(xpsolve))deallocate(xpsolve)
  if(allocated(zerosoln))deallocate(zerosoln)
  if(allocated(dummysoln))deallocate(dummysoln)

  allocate(vatol(neq),vrtol(neq), pprime(neq), p(neq), & 
           pdzero(neq), delta(neq), xpsolve(neq),&
           zerosoln(neq), dummysoln(neq), stat=error)
  zerosoln = 0.0_dd
  if(error.ne.0)then
    write(6,*)"error allocating solver arrays"
    stop
  endif
  do iroom = 1, nrooms
    r => rooms(iroom)

  	r%floor_area = r%dx*r%dy
  	r%abs_pressure = pabs_ref + r%rel_pressure
  	r%abs_layer_height = r%z0 + r%rel_layer_height
  	r%VU = r%floor_area*(r%dz - r%rel_layer_height)
  	r%volume = r%floor_area*r%dz

  	r%layer(lower)%volume = r%volume - r%VU
  	r%layer(upper)%volume = r%VU
    do ilay = 1, 2
      layer=>r%layer(ilay)
    	layer%density = r%abs_pressure/(rgas*layer%temperature)
    	layer%mass = layer%density*layer%volume
      layer%s_mass(oxygen) = layer%mass*amb_oxy_con
      layer%s_con(oxygen) = amb_oxy_con
    end do

    do iwall=1,4
      w=>r%wall(iwall)
      w%temp=tamb
      w%dir=iwall
      w%qdot=0.0_dd
      if(iwall.eq.1.or.iwall.eq.2)w%area=r%floor_area

    end do
      
    r%volmax = r%volume*0.999_dd
    r%volmin = r%volume*0.001_dd

  end do

  do ihvac = 1, nhvacs
    h => hvacs(ihvac)
    r=>rooms(h%fromroom)
    h%abs_frombot = h%rel_frombot + r%z0
    h%abs_fromtop = h%rel_fromtop + r%z0

    r=>rooms(h%toroom)
    h%abs_tobot = h%rel_tobot + r%z0
    h%abs_totop = h%rel_totop + r%z0
  end do

  if(plotfile.ne."")then
    open(unit=plotunit,file=plotfile,form="unformatted")
    open(unit=smvunit,file=smvfile)
    write(smvunit,"(a)")"ZONE"
    write(smvunit,"(a)")plotfile
    write(smvunit,"(a)")"P"
    write(smvunit,"(a)")"Pa"
    write(smvunit,"(a)")"Layer Height"
    write(smvunit,"(a)")"ylay"
    write(smvunit,"(a)")"m"
    write(smvunit,"(a)")"TEMPERATURE"                   
    write(smvunit,"(a)")"TEMP"                          
    write(smvunit,"(a)")"C"                             
    write(smvunit,"(a)")"TEMPERATURE"
    write(smvunit,"(a)")"TEMP"
    write(smvunit,"(a)")"C"
    write(smvunit,"(a)")"AMBIENT"
    write(smvunit,"(e13.6,1x,e13.6,1x,e13.6)")pabs_ref,pamb,tamb
    do iroom = 1, nrooms
      r=>rooms(iroom)
      write(smvunit,"(a)")"ROOM"
      write(smvunit,"(e11.4,1x,e11.4,1x,e11.4)")r%dx,r%dy,r%dz
      write(smvunit,"(e11.4,1x,e11.4,1x,e11.4)")r%x0,r%y0,r%z0
    end do
    do ivent = 1, nvents
      v=>vents(ivent)
      write(smvunit,"(a)")"VENTGEOM"
      write(smvunit,"(i3,1x,i3,1x,i3,1x,3(e11.4,1x),e11.4)")v%from,v%to,v%face,v%width,v%offset,v%relbot,v%reltop
    end do
    do ifire = 1, nfires
      f=>fires(ifire)
      write(smvunit,"(a)")"FIRE"
      write(smvunit,"(i3,1x,e11.4,1x,e11.4,1x,e11.4)")f%room_number,f%x0,f%y0,f%z0
    end do

    close(smvunit)

    open(unit=csvunit,file=csvfile)


  endif


  
end subroutine initamb    

