
! --------------------------- grabky -------------------------------------------

    subroutine grabky (ich,it)

    implicit none
    
    integer(2), intent(out) :: ich, it

    character(1) :: ch, getcharqq
    logical :: peekcharqq

    ich = 0
    it = 0

    if (peekcharqq()) then
        ch = getcharqq()
        ich = ichar(ch)
        if (ich==0) then
            ch = getcharqq()
            ich = ichar (ch)
            it = 2
        else
            it = 1
        endif
    endif

    return
    end subroutine grabky

