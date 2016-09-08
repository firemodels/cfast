#ifndef GITHASH_PP
#define GITHASH_PP "unknown"
#endif
#ifndef GITDATE_PP
#define GITDATE_PP "unknown"
#endif
#ifndef BUILDDATE_PP
#define BUILDDATE_PP "unknown"
#endif
    subroutine get_info (revision,revision_date,compile_date)

    implicit none
    character(len=256), intent(out) :: revision, revision_date, compile_date

    write (revision,'(A)')      TRIM(GITHASH_PP)
    write (revision_date,'(A)') TRIM(GITDATE_PP)
    write (compile_date,'(A)')  TRIM(BUILDDATE_PP)

    return

    end subroutine get_info
