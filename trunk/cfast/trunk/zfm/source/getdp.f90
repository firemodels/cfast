
! --------------- getdp ----------------------

real(kind=dd) function getdp(y,ivent)
  use precision
  use zonedata
  implicit none

  type(vent_data), pointer :: v
  type(room_data), pointer :: r1,r2
  integer :: ivent
  real(kind=dd) :: p1, p2, y, zz, epscut, dpold, epsp
  real(kind=dd) :: absold, expold

  v => vents(ivent)
  r1 => rooms(v%from)
  r2 => rooms(v%to)

  p1 = r1%rel_pressure - g*min(r1%rel_layer_height,y-r1%z0)*r1%layer(lower)%density
  p1 = p1 - g*max(y-r1%abs_layer_height,zero)*r1%layer(upper)%density

  p2 = r2%rel_pressure - g*min(r2%rel_layer_height,y-r2%z0)*r2%layer(lower)%density
  p2 = p2 - g*max(y-r2%abs_layer_height,zero)*r2%layer(upper)%density

  epsp = rptol
  epscut = 10.0_dd*epsp*max(1.0_dd,abs(p1),abs(p2))
  dpold = p1 - p2
  absold = abs(p1-p2)/epscut
  if(absold.le.37.0_dd)then      ! absold < 16*ln(10) < 37
   ! write(6,*)"fudge active!!",p1,p2,dpold
    expold = exp(-absold)
    zz = 1.0_dd - expold       
    getdp = dpold*zz
   else
    getdp = dpold
  endif

end function getdp
