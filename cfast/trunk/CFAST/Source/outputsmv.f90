
! --------------------------- svout -------------------------------------------

subroutine svout(pabs_ref,pamb,tamb,nrooms,x0,y0, nvents, nvvent, nfires,froom_number,fx0,fy0,fz0, ntarg, stime, nscount)
    ! 
    ! this routine creates the .smv file used by smokeview to determine size and location of
    ! rooms, vents, fires etc
    !
    ! this routine is called only once 
    !
    !  smvfile -    name of .smv file
    !  plotfile -   name of file containing zone fire data
    !  pabs_ref -   reference absolute pressure
    !  pamb -       ambient pressure
    !  tamb -       ambient temperature
    !  nrooms -     number of rooms
    !  x0,y0,z0 -   room origin
    !  dx,dy,dz -   room dimensions
    !  nvents -     number of vents
    !  vfrom -      from room number
    !  vto =        to room number 
    !  vface -      face number
    !  vwidth -     vent width
    !  vrelbot -    bottom elevation w.r.t. floor of from room
    !  vreltop -    top elevation w.r.t. floor of from room
    !  nfires -     number of fires
    !  froom_number - room containing fire
    !  fx0,fy0,fz0 - location of fire base

    use precision_parameters
    use iofiles
    use cenviro
    implicit none

    real(eb), intent(in) :: pabs_ref, pamb, tamb, stime
    integer, intent(in) :: nrooms, nscount, nvents, nfires, nvvent, ntarg
    real(eb), dimension(nrooms), intent(in) :: x0, y0
    integer, intent(in), dimension(nfires) :: froom_number
    real(eb), intent(in), dimension(nfires) :: fx0, fy0, fz0
    
    real(eb) :: vwidth, vbottom, vtop, voffset, vred, vgreen, vblue
    real(eb) :: harea, targetvector(6)
    integer ::i, hface, ibot, itop, hshape
    character(128) :: dir
    character(64) :: smokeviewplotfilename, drive, ext, name ! the extension is .plt
    integer(4) :: length, splitpathqq
    integer :: ifrom, ito, iface
    type(room_type), pointer :: roomi

    ! this code is to trim the file name to the name itself along with the extension
    ! for compatibility with version 4 and later of smokeview
    length = splitpathqq(smvcsv, drive, dir, name, ext)
    smokeviewplotfilename = trim(name) // trim(ext)

    rewind (13)
    write(13,"(a)") "ZONE"
    write(13,"(1x,a)") trim(smokeviewplotfilename)
    write(13,"(1x,a)") "PRESSURE"
    write(13,"(1x,a)") "P"
    write(13,"(1x,a)") "Pa"
    write(13,"(1x,a)") "Layer Height"
    write(13,"(1x,a)") "ylay"
    write(13,"(1x,a)") "m"
    write(13,"(1x,a)") "TEMPERATURE"                   
    write(13,"(1x,a)") "TEMP"                          
    write(13,"(1x,a)") "C"                             
    write(13,"(1x,a)") "TEMPERATURE"
    write(13,"(1x,a)") "TEMP"
    write(13,"(1x,a)") "C"
    write(13,"(a)") "AMBIENT"
    write(13,"(1x,e13.6,1x,e13.6,1x,e13.6)") pabs_ref,pamb,tamb

    do i = 1, nrooms
        roomi=>roominfo(i)
        
        write(13,"(a,1x)")"ROOM"
        write(13,10) roomi%br, roomi%dr, roomi%hr
        write(13,10) x0(i), y0(i), roomi%hrl
10      format(1x,e11.4,1x,e11.4,1x,e11.4)
    end do

    do i = 1, nvents
        write(13,"(a)")"VENTGEOM"
        call getventinfo(i,ifrom, ito, iface, vwidth, vbottom, vtop, voffset, vred, vgreen, vblue)
        write(13,20) ifrom, ito, iface, vwidth, voffset, vbottom, vtop, vred, vgreen, vblue
        !write(13,20) vfrom(i),vto(i),vface(i),vwidth(i),voffset(i),vrelbot(i),vreltop(i),1.0,0.0,1.0
20      format(1x,i3,1x,i3,1x,i3,1x,6(e11.4,1x),e11.4)
    end do
    do i = 1, nfires
        write(13,"(a)")"FIRE"
        write(13,30) froom_number(i),fx0(i),fy0(i),fz0(i)
30      format(1x,i3,1x,e11.4,1x,e11.4,1x,e11.4)
    end do

    do i = 1, nvvent
        write(13,"(a)") "VFLOWGEOM"
        call getvventinfo(i,itop,ibot,harea,hshape,hface)
        write(13,35) itop,ibot,hface,harea,hshape
35      format(1x,3i3,1x,e11.4,1x,i3)
    end do

    if (ntarg>nrooms) then
        write(13,"(a)") "THCP"
        write(13,"(1x,i3)") ntarg-nrooms
        do i = 1, ntarg-nrooms
            call getabstarget(i,targetvector)
            write(13,36) targetvector
36          format(1x,6f10.2)
        end do
    endif

    write(13,"(a)") "TIME"
    write(13,40) nscount, stime
40  format(1x,i6,1x,f11.0)

    call ssheaderssmv(.false.)

    return
end subroutine svout

! --------------------------- svplotdata -------------------------------------------

subroutine  svplotdata(time,nrooms,nfires,qdot,height)

!
! this routine records data for the current time step into the smokeview zone fire data file
!
!            be visualized by smokeview
!     time - current time
!   nrooms   number of rooms
!   nfires - number of fires
!     qdot - real array of size nfires of fire heat release rates
!   height - real array of size nfires of fire heights
!
    use precision_parameters
    use cenviro
    implicit none

    real(eb), intent(in) :: time
    integer, intent(in) :: nrooms
    integer, intent(in) :: nfires
    real(eb), intent(in), dimension(nfires) :: qdot, height
    real xxtime, xxheight, xxqdot
    
    type(room_type), pointer :: roomi

    integer :: i

    xxtime = time
    write(14) xxtime

    do i = 1, nrooms
        roomi=>roominfo(i)
        
        write(14) roomi%zzrelp, roomi%zzhlay(lower), roomi%zztemp(lower), roomi%zztemp(upper)
    end do

    do i = 1, nfires
        xxheight = height(i)
        xxqdot = qdot(i)
        write(14) xxheight, xxqdot
    end do

end subroutine svplotdata

! --------------------------- svplothdr -------------------------------------------

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

! --------------------------- rev_outputsmv -------------------------------------------

integer function rev_outputsmv ()
          
      integer :: module_rev
      character(255) :: module_date 
      character(255), parameter :: mainrev='$Revision$'
      Character(255), parameter :: maindate='$Date$'
      
      write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
      read (module_date,'(i5)') module_rev
      rev_outputsmv = module_rev
      write(module_date,'(a)') maindate
      return
      
end function rev_outputsmv