    subroutine get_revision (revision, date)
    implicit none
    character(len=256), intent(out) :: revision, date
    revision = trim(adjustl("unknown"))
    date = trim(adjustl("unknown"))
    return
    end subroutine get_revision
