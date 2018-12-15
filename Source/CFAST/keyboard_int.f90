
! --------------------------- grabky -------------------------------------------

    subroutine grabky (ich, hit)
    
    ! interogates keyboard for a key press
    
    ! outputs   ich     ASCII character number of key pressed
    !           hit     =2 if new key pressed; =1 otherwise

    implicit none

    integer(2), intent(out) :: ich, hit

    character(1) :: ch, getcharqq
    logical :: peekcharqq

    ich = 0
    hit = 0

    if (peekcharqq()) then
        ch = getcharqq()
        ich = ichar(ch)
        if (ich==0) then
            ch = getcharqq()
            ich = ichar (ch)
            hit = 2
        else
            hit = 1
        end if
    end if

    return
    end subroutine grabky

