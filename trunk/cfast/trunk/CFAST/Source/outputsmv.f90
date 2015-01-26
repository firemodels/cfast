
! --------------------------- output_smokeview -------------------------------------------

subroutine output_smokeview(pabs_ref,pamb,tamb,nrooms,x0,y0,z0,dx,dy,dz, n_hvents, n_vvents, nfires,froom_number,fx0,fy0,fz0, ntarg, stime, nscount)
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
    !  n_hvents -     number of vents
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
    use cfast_types
    use cenviro
    implicit none

    real(eb), intent(in) :: pabs_ref, pamb, tamb, stime
    integer, intent(in) :: nrooms, nscount, n_hvents, nfires, n_vvents, ntarg
    real(eb), dimension(nrooms), intent(in) :: x0, y0, z0, dx, dy, dz
    integer, intent(in), dimension(nfires) :: froom_number
    real(eb), intent(in), dimension(nfires) :: fx0, fy0, fz0
    
    real(eb) :: vwidth, vbottom, vtop, voffset, vred, vgreen, vblue
    real(eb) :: harea, targetvector(6)
    integer ::i, hface, ibot, itop, hshape
    character(128) :: dir
    character(64) :: smokeviewplotfilename, drive, ext, name ! the extension is .plt
    integer(4) :: length, splitpathqq
    integer :: ifrom, ito, iface

    integer ibar, jbar, kbar
    real(eb) :: x1, y1, z1
    integer :: j
    real(eb) :: xj, yj, zj
    type(room_type), pointer :: rm
    type(slice_type), pointer :: sf

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

    ! ibar, jbar and kbar are placeholders for now
    ibar = 10
    jbar = 10
    kbar = 10
    do i = 1, nrooms
       rm=>roominfo(i)
       
       ibar = rm%ibar
       jbar = rm%jbar
       kbar = rm%kbar
        
        write(13,"(a,1x)")"ROOM"
        write(13,10) dx(i), dy(i), dz(i)
        write(13,10) x0(i), y0(i), z0(i)
10      format(1x,e11.4,1x,e11.4,1x,e11.4)

        write(13,"(a,1x)")"GRID"
        write(13,"(1x,i5,1x,i5,1x,i5,1x,i5)")ibar,jbar,kbar,0
        x1 = x0(i) + dx(i)
        y1 = y0(i) + dy(i)
        z1 = z0(i) + dz(i)
        write(13,"(a,1x)")"PDIM"
        write(13,"(9(f14.5,1x))")x0(i),x1,y0(i),y1,z0(i),z1,0.0_eb,0.0_eb,0.0_eb
        write(13,"(a,1x)")"TRNX"
        write(13,"(1x,i1)")0
        do j = 0, ibar
           xj = (x0(i)*(ibar-j) + x1*j)/real(ibar,eb)
           write(13,"(i5,1x,f14.5)")j,xj
        end do
        write(13,"(a,1x)")"TRNY"
        write(13,"(1x,i1)")0
        do j = 0, jbar
           yj = (y0(i)*(jbar-j) + y1*j)/real(jbar,eb)
           write(13,"(i5,1x,f14.5)")j,yj
        end do
        write(13,"(a,1x)")"TRNZ"
        write(13,"(1x,i1)")0
        do j = 0, kbar
           zj = (z0(i)*(kbar-j) + z1*j)/real(kbar,eb)
           write(13,"(i5,1x,F14.5)")j,zj
        end do
        write(13,"(a,1x)")"OBST"
        write(13,"(1x,i1)")0
        write(13,"(a,1x)")"VENT"
        write(13,"(1x,i1,1x,i1)")0,0
    end do
    
    do i = 1, nsliceinfo
       sf=>sliceinfo(i)
       
       write(13,"(a,1x,i3,'&',6(i4,1x))")"SLCF",sf%roomnum,sf%ijk(1),sf%ijk(2),sf%ijk(3),sf%ijk(4),sf%ijk(5),sf%ijk(6)
       write(13,"(1x,a)")trim(sf%filename)
       write(13,"(1x,a)")trim(sf%menu_label)
       write(13,"(1x,a)")trim(sf%colorbar_label)
       write(13,"(1x,a)")trim(sf%unit_label)
    end do

    do i = 1, n_hvents
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

    do i = 1, n_vvents
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
end subroutine output_smokeview

! --------------------------- output_smokeview_plot_data -------------------------------------------

subroutine  output_smokeview_plot_data(time,nrooms,pr,ylay,tl,tu,nfires,qdot,height)

!
! this routine records data for the current time step into the smokeview zone fire data file
!
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
    use precision_parameters
    implicit none

    real(eb), intent(in) :: time
    integer, intent(in) :: nrooms
    real(eb), intent(in), dimension(nrooms) :: pr, ylay, tl, tu
    integer, intent(in) :: nfires
    real(eb), intent(in), dimension(nfires) :: qdot, height
    real xxtime, xxpr, xxylay, xxtl, xxtu, xxheight, xxqdot

    integer :: i

    xxtime = time
    write(14) xxtime

    do i = 1, nrooms
        xxpr = pr(i)
        xxylay = ylay(i)
        xxtl = tl(i)
        xxtu = tu(i)
        write(14) xxpr, xxylay, xxtl, xxtu
    end do

    do i = 1, nfires
        xxheight = height(i)
        xxqdot = qdot(i)
        write(14) xxheight, xxqdot
    end do

   end subroutine output_smokeview_plot_data

! --------------------------- finterp -------------------------------------------

real(eb) function fmix(fbeg,fend,i,n)
use precision_parameters
implicit none

real(eb), intent(in) :: fbeg, fend
integer, intent(in) :: i, n

if(n.le.0)then
   fmix = fbeg
else
   fmix = (fbeg*real(n-i,eb) + fend*real(i,eb))/real(n,eb)
endif
end function fmix

! --------------------------- output_slicedata -------------------------------------------

subroutine output_slicedata(time,first_time)
   use precision_parameters
   use iofiles
   use cenviro
   use cfast_main
   implicit none

   real(eb), intent(in) :: time
   integer, intent(in) :: first_time
   real(fb), allocatable, dimension(:,:,:) :: slicedata
   integer :: nx, ny, nz
   type(slice_type), pointer :: sf
   type(room_type), pointer :: rm
   integer :: i, ii, jj, kk, roomnum
   real(eb) :: xx, yy, zz, tgas
   integer :: funit,unit
   real(eb) :: fmix
   
   do i = 1, nsliceinfo
      sf => sliceinfo(i)
      
      nx = sf%ijk(2) + 1 - sf%ijk(1)
      ny = sf%ijk(4) + 1 - sf%ijk(3)
      nz = sf%ijk(6) + 1 - sf%ijk(5)
      roomnum = sf%roomnum
      rm => roominfo(roomnum)
      if(nx.le.0.or.ny.le.0.or.nz.le.0)cycle
      allocate(slicedata(0:nx-1,0:ny-1,0:nz-1))
      do ii = 0, nx-1
         xx = fmix(sf%xb(1),sf%xb(2),ii,nx-1) - rm%x0
         do jj = 0, ny-1
            yy = fmix(sf%xb(3),sf%xb(4),jj,ny-1) - rm%y0
            do kk = 0, nz-1
               zz = fmix(sf%xb(5),sf%xb(6),kk,nz-1) - rm%z0
               call gettgas(roomnum,xx,yy,zz,tgas)
               slicedata(ii,jj,kk) = real(tgas-273.15_eb,fb)
            end do
         end do
      end do
      unit=funit(14)
      if(first_time.eq.1)then
         open(unit,file=sf%filename,form='unformatted',status='replace')
         write(unit) sf%menu_label(1:30)
         write(unit) sf%colorbar_label(1:30)
         write(unit) sf%unit_label(1:30)
         write(unit) (sf%ijk(ii),ii=1,6)
      else
         open(unit,FILE=sf%filename,form='unformatted',status='old',position='append')
      endif
      write(unit) real(time,fb)
      write(unit) (((slicedata(ii,jj,kk),ii=0,nx-1),jj=0,ny-1),kk=0,nz-1)
      deallocate(slicedata)
      close(unit)
   end do
   

end subroutine output_slicedata

! --------------------------- output_smokeview_header -------------------------------------------

subroutine output_smokeview_header (version, nrooms, nfires)

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

end subroutine output_smokeview_header

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