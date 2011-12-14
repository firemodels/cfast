subroutine smvout(smvfile,plotfile,pabs_ref,pamb,tamb,nrooms,x0,y0,z0,dx,dy,dz, &
                          nvents,vfrom,vto,vface,vwidth,voffset,vrelbot,vreltop, &
                          nfires,froom_number,fx0,fy0,fz0)
! 
! This routine creates the .smv file used by smokeview to determine size and location of
! rooms, vents, fires etc
!
! This routine is called only once 
!
!  smvfile -    name of .smv file
!  plotfile -   name of file containing zone fire data
!  pabs_ref -   reference absolute pressure
!  pamb -       ambient pressure
!  tamb -       ambient temperature
!  nrooms -     number of rooms
!    x0,y0,z0 -   room origin
!    dx,dy,dz -   room dimensions
!  nvents -     number of vents
!    vfrom -      from room number
!    vto =        to room number 
!    vface -      face number
!    vwidth -     vent width
!    vrelbot -    bottom elevation w.r.t. floor of from room
!    vreltop -    top elevation w.r.t. floor of from room
!  nfires -     number of fires
!    froom_number - room containing fire
!    fx0,fy0,fz0 - location of fire base

  implicit none
  character(len=*), intent(in) :: smvfile, plotfile
  real, intent(in) :: pabs_ref, pamb, tamb
  integer, intent(in) :: nrooms
  real, dimension(nrooms), intent(in) :: x0, y0, z0, dx, dy, dz
  integer, intent(in) :: nvents
  integer, intent(in), dimension(nvents) :: vfrom, vto, vface
  real, intent(in), dimension(nvents) :: vwidth, voffset, vrelbot, vreltop
  integer, intent(in) :: nfires
  integer, intent(in), dimension(nfires) :: froom_number
  real, intent(in), dimension(nfires) :: fx0, fy0, fz0

  integer :: i, smvunit, funit

  smvunit=funit(70)

  open(unit=smvunit, file=trim(smvfile))

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

  do i = 1, nrooms
    write(smvunit,"(a)")"ROOM"
    write(smvunit,10)dx(i), dy(i), dz(i)
    write(smvunit,10)x0(i), y0(i), z0(i)
10  format(e11.4,1x,e11.4,1x,e11.4)
  end do
  do i = 1, nvents
    write(smvunit,"(a)")"VENTGEOM"
    write(smvunit,20)vfrom(i),vto(i),vface(i),vwidth(i),voffset(i),vrelbot(i),vreltop(i)
20  format(i3,1x,i3,1x,i3,1x,3(e11.4,1x),e11.4)
  end do
  do i = 1, nfires
    write(smvunit,"(a)")"FIRE"
    write(smvunit,30)froom_number(i),fx0(i),fy0(i),fz0(i)
30  format(i3,1x,e11.4,1x,e11.4,1x,e11.4)
  end do

  close(smvunit)

  return
end subroutine smvout

subroutine svplothdr(plotfile,version,nrooms,nfires)

!
! This routine prints out a header for the smokeview zone fire data file
!
! This routine is called once
!
! plotfile - name of file containing zone fire modeling plot data to 
!            be visualized by smokeview
! version  - Presently smokeview only supports version=1 .  In the future
!            if the file format changes then change version to allow
!            smokeview to determine how the data file is organized
!  nrooms  - number of rooms in simulation
!  nfires  - number of fires in simulation
!              
  implicit none
  integer, intent(in) :: version,nrooms,nfires
  character(len=*), intent(in) :: plotfile
  integer :: plotunit, funit

  plotunit=funit(70)

  open(unit=plotunit,file=plotfile,form="unformatted")

  write(plotunit)version
  write(plotunit)nrooms
  write(plotunit)nfires

  close(plotunit)
  return

end subroutine svplothdr

subroutine  svplotdata(plotfile,time,nrooms,pr,ylay,tl,tu,nfires,qdot,height)

!
! This routine records data for the current time step into the smokeview zone fire data file
!
! plotfile - name of file containing zone fire modeling plot data to 
!            be visualized by smokeview
!     time - current time
!   nrooms   number of rooms
!       pr - real array of size nrooms of room pressures
!     ylay - real array of size nrooms of layer interface heights
!       tl - real array of size nrooms of lower layer temperatures 
!       tu - real array of size nrooms of upper layer temperatures 
!   nfires - number of fires
!     qdot - real array of size nfires of fire heat release rates
!   height - real array of size nfires of fire heights
!
  implicit none

  character(len=*), intent(in) :: plotfile
  real, intent(in) :: time
  integer, intent(in) :: nrooms
  real, intent(in), dimension(nrooms) :: pr, ylay, tl, tu
  integer, intent(in) :: nfires
  real, intent(in), dimension(nfires) :: qdot, height

  integer :: plotunit, funit, i

  plotunit=funit(70)
  open(unit=plotunit,file=plotfile,form="unformatted",position="APPEND")

  write(plotunit)time
  do i = 1, nrooms
    write(plotunit)pr(i),ylay(i),tl(i),tu(i)
  end do

  do i = 1, nfires
    write(plotunit)height(i),qdot(i)
  end do
  close(plotunit)

end subroutine svplotdata



integer function funit(unit)

! return an unused I/O unit

  implicit none

  integer, intent(in) :: unit
  integer, parameter :: maxio=32767
  logical :: opened
  integer :: itemp

  itemp = unit
  opened = .false.
  do
    inquire(unit=itemp,opened=opened)
    if(.not.opened)exit
    itemp = itemp + 1
  end do
  funit=itemp
  return
end function funit
