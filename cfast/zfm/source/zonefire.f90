subroutine zonefire(filein,error)
  integer :: error
  character(len=*) :: filein
  logical :: firstcall
  data firstcall/.true./
  save firstcall

  if(firstcall)then
    call init0
    firstcall = .false.
  endif
  call initmm
  call readini
  call loadcase(filein,error)
  if(error.ne.0)return
  call initamb
!  call writedata
  call initsolve
  call solve
  return
end subroutine zonefire
