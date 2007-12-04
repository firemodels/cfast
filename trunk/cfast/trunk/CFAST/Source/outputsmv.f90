subroutine svout(plotfile, pabs_ref,pamb,tamb,nrooms,x0,y0,z0,dx,dy,dz, &
                          nvents,vfrom,vto,vface,vwidth,voffset,vrelbot,vreltop, &
						  nvvent, &
                          nfires,froom_number,fx0,fy0,fz0, &
						  ntarg, stime, nscount)
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

      include "iofiles.fi"

  DOUBLE PRECISION, intent(in) :: pabs_ref, pamb, tamb, stime
  integer, intent(in) :: nrooms, nscount, nvents, nfires, nvvent, ntarg
  DOUBLE PRECISION, dimension(nrooms), intent(in) :: x0, y0, z0, dx, dy, dz
  integer, intent(in), dimension(nvents) :: vfrom, vto, vface
  DOUBLE PRECISION, intent(in), dimension(nvents) :: vwidth, voffset, vrelbot, vreltop
  integer, intent(in), dimension(nfires) :: froom_number
  DOUBLE PRECISION, intent(in), dimension(nfires) :: fx0, fy0, fz0
  character(len=*), intent(in) :: plotfile
  double precision  harea, targetvector(6)
  integer i, hface, ibot, itop, hshape
  character(128) dir
  CHARACTER(64) smokeviewplotfilename, drive, ext, name ! the extension is .plt
  INTEGER(4) length, splitpathqq

! This code is to trim the file name to the name itself along with the extension
! for compatibility with version 4 and later of smokeview
  length = SPLITPATHQQ(smvdata, drive, dir, name, ext)
  smokeviewplotfilename = trim(name) // trim(ext)
  
  rewind (13)
  write(13,"(a)") "ZONE"
  write(13,"(a)") trim(smokeviewplotfilename)
  write(13,"(a)") "P"
  write(13,"(a)") "Pa"
  write(13,"(a)") "Layer Height"
  write(13,"(a)") "ylay"
  write(13,"(a)") "m"
  write(13,"(a)") "TEMPERATURE"                   
  write(13,"(a)") "TEMP"                          
  write(13,"(a)") "K"                             
  write(13,"(a)") "TEMPERATURE"
  write(13,"(a)") "TEMP"
  write(13,"(a)") "K"
  write(13,"(a)") "AMBIENT"
  write(13,"(e13.6,1x,e13.6,1x,e13.6)") pabs_ref,pamb,tamb

  do i = 1, nrooms
    write(13,"(a,1x,i3)")"ROOM",i
    write(13,10)dx(i), dy(i), dz(i)
    write(13,10)x0(i), y0(i), z0(i)
10  format(e11.4,1x,e11.4,1x,e11.4)
  end do
  
  do i = 1, nvents
    write(13,"(a)")"VENTGEOM"
    write(13,20) vfrom(i),vto(i),vface(i),vwidth(i),voffset(i),vrelbot(i),vreltop(i)
20  format(i3,1x,i3,1x,i3,1x,3(e11.4,1x),e11.4)
  end do
  do i = 1, nfires
    write(13,"(a)")"FIRE"
    write(13,30) froom_number(i),fx0(i),fy0(i),fz0(i)
30  format(i3,1x,e11.4,1x,e11.4,1x,e11.4)
  end do

  do i = 1, nvvent
	write(13,"(a)") "VFLOWGEOM"
	call getvventinfo(i,itop,ibot,harea,hshape,hface)
	write(13,35) itop,ibot,hface,harea,hshape
35  format(3i3,1x,e11.4,1x,i3)
  end do

  if (ntarg.gt.nrooms) then
    write(13,"(a)") "THCP"
    write(13,"(i3)") ntarg-nrooms
      do i = 1, ntarg-nrooms
	  call getabstarget(i,targetvector)
	  write(13,36) targetvector
36    format(6f10.2)
    end do
  endif

    write(13,"(a)") "TIME"
    write(13,40) nscount, stime
40  format(i6,1x,f11.0)

  return
end subroutine svout

subroutine  svplotdata(time,nrooms,pr,ylay,tl,tu,nfires,qdot,height)

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

  DOUBLE PRECISION, intent(in) :: time
  integer, intent(in) :: nrooms
  DOUBLE PRECISION, intent(in), dimension(nrooms) :: pr, ylay, tl, tu
  integer, intent(in) :: nfires
  DOUBLE PRECISION, intent(in), dimension(nfires) :: qdot, height
  REAL XXTIME, XXPR, XXYLAY, XXTL, XXTU, XXHEIGHT, XXQDOT

  integer :: i

  xxtime = time
  write(14) xxtime

  do i = 1, nrooms
	XXPR = pr(i)
	XXYLAY = ylay(i)
	XXTL = tl(i)
	XXTU = tu(i)
    write(14) XXPR, XXYLAY, XXTL, XXTU
  end do

  do i = 1, nfires
	XXHEIGHT = height(i)
	XXQDOT = qdot(i)
    write(14) XXHEIGHT, XXQDOT
  end do

End subroutine svplotdata


subroutine svplothdr (version, nrooms, nfires)

!
! This routine prints out a header for the smokeview zone fire data file
!
! This routine is called once
!
!  version  - Presently smokeview only supports version=1 .  In the future
!            if the file format changes then change version to allow
!            smokeview to determine how the data file is organized
!  nrooms  - number of rooms in simulation
!  nfires  - number of fires in simulation
!              
  implicit none
  integer, intent(in) :: version, nrooms, nfires

  write(14) version
  write(14) nrooms
  write(14) nfires
  return

end subroutine svplothdr

