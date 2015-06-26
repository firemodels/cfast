
! --------------------------- grabky -------------------------------------------

    subroutine grabky (ich,it)

    implicit none
    
    integer(2), intent(out) :: ich, it

    ich = 0
    it = 0

    return
    end subroutine grabky

! ---------------------------  -------------------------------------------

    integer function splitpathqq(path,drive,dir,name,ext)
    character*(*), intent(IN) :: path
    character*(*), intent(out) :: drive, dir, name, ext
   
    character*1 dirsep
    integer :: i, len, first, next

    dirsep = '/'
    drive=""
    dir=""
    name=""
    ext=""
    len = length(trim(path))
    splitpathqq=len
    if(len.eq.0)return
    ! look for first non-blank character
    do i = 1, len
       first=i
       if(path(i:i).ne.' ')exit
    end do

    ! look for extension
    next=len
    do i = next, first, -1
       if(path(i:i)=='.')then
          ext=path(i:len)
          next=i-1
          exit
       endif
    end do

    ! look for filename
    do i = next, first, -1
       if(path(i:i)==dirsep)then
          name=path(i+1:next)
          next=i
          exit
       endif
    end do
    ! directory
    dir=path(first:next)

! debug print
!    write(6,*)"path=",trim(path)
!    write(6,*)"drive=",trim(drive)
!    write(6,*)"dir=",trim(dir)
!    write(6,*)"name=",trim(name)
!    write(6,*)"ext=",trim(ext)
!    stop    
    end function splitpathqq
