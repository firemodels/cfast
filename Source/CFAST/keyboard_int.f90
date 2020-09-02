
! --------------------------- grabky -------------------------------------------
    
!> \brief   interogates keyboard for a key press
    
!> \param   ich (output): ASCII character number of key pressed
!> \param   hit (output): =2 if new key pressed; =1 otherwise

    subroutine grabky (ich, hit)

    implicit none

    integer(2), intent(out) :: ich, hit

    character(len=1) :: ch, getcharqq
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

