    subroutine get_revision (revision)
    implicit none
    character(len=256), intent(out) :: revision
    revision = trim("unknown")
    return
    end subroutine get_revision
