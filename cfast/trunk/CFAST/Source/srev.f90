    subroutine get_info (revision,revision_date,compile_date)
    implicit none
    character(len=256), intent(out) :: revision, revision_date, compile_date
    revision = trim(adjustl("unknown"))
    revision_date = trim(adjustl("unknown"))
    compile_date = trim(adjustl("uknown"))
    return
    end subroutine get_revision
