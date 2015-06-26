    subroutine get_info (revision,revision_date,compile_date)
    implicit none
    character(len=256), intent(out) :: revision, revision_date, compile_date

    character(255), parameter :: grevision='$Revision: unknown $'
    character(255), parameter :: grevision_date='$RevisionDate: unknown $'
    character(255), parameter :: gcompile_date='$CompileDate: unknown $'

    write(revision,'(A)')      grevision(index(grevision,':')+2:len_trim(grevision)-2)
    write(revision_date,'(A)') grevision_date(index(grevision_date,':')+2:len_trim(grevision_date)-2)
    write(compile_date,'(A)')  gcompile_date(index(gcompile_date,':')+2:len_trim(gcompile_date)-2)
    return
    end subroutine get_info
