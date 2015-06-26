
! --------------- convecflow
   
subroutine convecflow
  use precision
  use zonedata
  implicit none
  type(room_data), pointer :: r
  integer :: iroom
  real(kind=eb) :: tg, tw
  real(kind=eb) :: qceil, quwall, qlwall, qfloor

  do iroom = 1, nrooms
    r => rooms(iroom)

    tg = r%layer(upper)%temperature
    if(r%wall(1)%wallmatindex.ne.p_nowall)then
      tw = r%wall(1)%temp
      call convec(1,tg,tw,qceil)
    else
      qceil = 0.0_eb
    endif

    if(r%wall(3)%wallmatindex.ne.p_nowall)then
      tw = r%wall(3)%temp
      call convec(3,tg,tw,quwall)
     else
      quwall = 0.0_eb
    endif

    tg = r%layer(lower)%temperature
    if(r%wall(4)%wallmatindex.ne.p_nowall)then
      tw = r%wall(2)%temp
      call convec(2,tg,tw,qlwall)
     else
      qlwall = 0.0_eb
    endif

    if(r%wall(2)%wallmatindex.ne.p_nowall)then
      tw = r%wall(4)%temp
      call convec(4,tg,tw,qfloor)
     else
      qfloor = 0.0_eb
    endif

    cflow(iroom,upper)%qdot = - qceil*r%wall(1)%area - quwall*r%wall(3)%area
    cflow(iroom,upper)%zero = .false.

    cflow(iroom,lower)%zero = .false.
    cflow(iroom,lower)%qdot =  -qfloor*r%wall(2)%area - qlwall*r%wall(4)%area
  end do
end subroutine convecflow
