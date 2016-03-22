
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
    len = len_trim(trim(path))
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
            if(path(i+1:i+1)==dirsep)then
                ext=''
                next=len
            else
                ext=path(i:len)
                next=i-1
            endif
            exit
        endif
    end do

    ! look for filename
    do i = next, first, -1
        if(path(i:i)==dirsep)then
            name=path(i+1:next)
            next=i
            dir=path(first:next)
            exit
        endif
        if (i==first) then
            name = path(first:next)
            next=first
            dir =''
            exit
        endif
    end do

    ! debug print
    !write(*,*)"path=",trim(path)
    !write(*,*)"drive=",trim(drive)
    !write(*,*)"dir=",trim(dir)
    !write(*,*)"name=",trim(name)
    !write(*,*)"ext=",trim(ext)
    !    stop
    end function splitpathqq
