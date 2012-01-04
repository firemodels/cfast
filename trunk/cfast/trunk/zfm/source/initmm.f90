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
  zeroflow%sdot = 0.0_eb
  zeroflow%fromlower = .false.
  zeroflow%fromupper = .false.
  zeroflow%zero = .true.
  
  nspecies = 0
  n_single = 0
  amb_oxy_con = 0.23_eb
  heat_c  = 45000000.0_eb
  heat_o2 = 13200000.0_eb
  yield_SPECIES(co) = 1.0
  yield_SPECIES(co2) = 1.0
  yield_SPECIES(smoke) = 1.0
  o2limit = 0.10_eb
  chi_rad = 0.35_eb
  tprint = 0.0_eb
  tdump = 0.0_eb
  tplot = 0.0_eb
  iplot = 0
  iprint = 0
  idump = 0
  debugprint = .false.
  solveoxy = .false.
  solveprods = .false.
  allwalls = .false.
  allwallsmat = ""
  plotfilebase = ""
  smvfile=""
  plotfile=""
  csvfile=""
  dumpfile=""
  pi = 4.0d0 * atan(1.0_eb)
  call setstate(.true.)
end subroutine initmm

! --------------- initamb ----------------------

subroutine initamb
  use precision
  use zonedata
  implicit none
  integer :: iroom, ihvac
  type(room_data), pointer :: r
  type(zone_data), pointer :: llay, ulay
  type(hvac_data), pointer :: h
  type(vent_data), pointer :: v
  type(zone_data), pointer :: layer
  type(fire_data), pointer :: f
  type(wall_data), pointer :: w
  integer :: error, ilay , ivent, ifire, iwall, i, ispec

  real, dimension(nrooms) :: x0, dx, y0, dy, z0, dz
  integer, dimension(nvents) :: vfrom, vto, vface
  real, dimension(nvents) :: vwidth, voffset, vrelbot, vreltop
  integer, dimension(nfires) :: froom_number
  real, dimension(nfires) :: fx0, fy0, fz0

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

  neq = 4*nrooms
  if(solveoxy)neq = neq + 2*nrooms
  if(solveprods)neq = neq + 6*nrooms

  allocate(vatol(neq),vrtol(neq), pprime(neq), p(neq), & 
           pdzero(neq), delta(neq), xpsolve(neq),&
           zerosoln(neq), dummysoln(neq), stat=error)
  i=0
  n_single = 0
  do iroom= 1, nrooms
    r => rooms(iroom)
    if(r%singlezone.eq.1)n_single = n_single + 1
  end do

  offset_p = 0  ! define offsets used to access solver array
  offset_vu = offset_p + nrooms
  offset_tl = offset_vu + nrooms - n_single
  offset_tu = offset_tl + nrooms - n_single
  offset_oxyl = offset_tu + nrooms 
  offset_oxyu = offset_oxyl + nrooms - n_single
  offset_col = offset_oxyu + nrooms 
  offset_cou = offset_col + nrooms - n_single
  offset_co2l = offset_cou + nrooms 
  offset_co2u = offset_co2l + nrooms - n_single
  offset_smokel = offset_co2u + nrooms 
  offset_smokeu = offset_smokel + nrooms - n_single

  offset_SPECIES(oxygen,upper)=offset_oxyu
  offset_SPECIES(co,upper)    =offset_cou
  offset_SPECIES(co2,upper)   =offset_co2u
  offset_SPECIES(smoke,upper) =offset_smokeu
  offset_SPECIES(oxygen,lower)=offset_oxyl
  offset_SPECIES(co,lower)    =offset_col
  offset_SPECIES(co2,lower)   =offset_co2l
  offset_SPECIES(smoke,lower) =offset_smokel





  neq = neq - 2*n_single
  if(solveoxy)neq = neq - n_single
  if(solveprods)neq = neq - (maxspecies-1)*n_single


  zerosoln = 0.0_eb
  if(error.ne.0)then
    write(6,*)"error allocating solver arrays"
    stop
  endif
  
      ! define room 0 using data read in or in readini

      r => rooms(0)
      r%z0 = 0.0_eb
      r%dx = 1000._eb
      r%dy = 1000._eb
      r%dz = 1000._eb
      r%rel_layer_height = 1000._eb
      r%floor_area = r%dx*r%dy
      r%abs_pressure = pabs_ref
      r%rel_pressure = 0.0_eb
      r%abs_layer_height = r%z0 + r%rel_layer_height
      r%volume = r%floor_area*r%dz
      r%VU = 0.0_eb

      llay => r%layer(lower)
      ulay => r%layer(upper)

      llay%temperature = tamb
      ulay%temperature = tamb
      llay%density = rhoamb
      ulay%density = rhoamb
      llay%volume = r%floor_area*r%dz
      llay%mass = rhoamb*r%volume
      llay%s_mass(oxygen) = llay%mass*amb_oxy_con
      llay%s_con(oxygen) = amb_oxy_con
      ulay%s_mass(oxygen) = ulay%mass*amb_oxy_con
      ulay%s_con(oxygen) = amb_oxy_con

      do ispec = 2, maxspecies
        llay%s_mass(ispec) = 0.0_eb
        llay%s_con(ispec) = 0.0_eb
        ulay%s_mass(ispec) = 0.0_eb
        ulay%s_con(ispec) = 0.0_eb
      end do

      ulay%volume = 0.0_eb
      ulay%mass = 0.0_eb

  absorb(lower) = 0.01
  absorb(upper) = 0.50
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
      do ispec = 2, maxspecies
        layer%s_mass(ispec) = 0.0
        layer%s_con(ispec) = 0.0
      end do
    end do

    do iwall=1,4
      w=>r%wall(iwall)
      w%temp=tamb
      w%dir=iwall
      w%qdot=0.0_eb
      if(iwall.eq.1.or.iwall.eq.2)w%area=r%floor_area

    end do
      
    r%volmax = r%volume*0.999_eb
    r%volmin = r%volume*0.001_eb

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
    do iroom = 1, nrooms
      r=>rooms(iroom)
      x0(iroom) = r%x0
      y0(iroom) = r%y0
      z0(iroom) = r%z0
      dx(iroom) = r%dx
      dy(iroom) = r%dy
      dz(iroom) = r%dz
    end do
    do ivent = 1, nvents
      v=>vents(ivent)
      vfrom(ivent) = v%from
      vto(ivent) = v%to
      vface(ivent) = v%face
      vwidth(ivent) = v%width
      voffset(ivent) = v%offset
      vrelbot(ivent) = v%relbot
      vreltop(ivent) = v%reltop
    end do
    do ifire = 1, nfires
      f=>fires(ifire)
      froom_number(ifire) = f%room_number
      fx0(ifire) = f%x0
      fy0(ifire) = f%y0
      fz0(ifire) = f%z0
    end do
    call smvout(smvfile,plotfile,real(pabs_ref),real(pamb),real(tamb),nrooms,x0,y0,z0,dx,dy,dz, &
                          nvents,vfrom,vto,vface,vwidth,voffset,vrelbot,vreltop, &
                          nfires,froom_number,fx0,fy0,fz0)
    open(unit=csvunit,file=csvfile)


  endif


  
end subroutine initamb    

