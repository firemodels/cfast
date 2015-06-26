subroutine interp(tsec,t,q,n,qt)
  use precision
  implicit none
  integer, intent(in) :: n
  real(kind=eb), dimension(n) :: t, q
  real(kind=eb), intent(out) :: qt
  real(kind=eb), intent(in) :: tsec
  real(kind=eb) :: qlow, qhigh, tlow, thigh
  real(kind=eb) :: f1, f2
  integer :: istart, iend, imid


  istart=0
  iend=n+1
  if(tsec.le.t(1))then
    qt = q(1)
    return
  endif
  if(tsec.ge.t(n))then
    qt = q(n)
    return
  endif
  do
   if(iend-istart.le.1)exit
   imid = (istart+iend)/2
   if(tsec.ge.t(imid))then
     istart = imid
    else
     iend = imid
   endif
  end do
  if(tsec.lt.t(istart).or.tsec.gt.t(istart+1))then
    write(6,*)t(istart),tsec,t(istart+1)
    stop
  endif
  qlow = q(istart)
  qhigh = q(istart+1)
  tlow = t(istart)
  thigh = t(istart+1)
  f1 = (thigh-tsec)/(thigh-tlow)
  f2 = 1.0_eb - f1
  qt = qlow*f1 + qhigh*f2
return
end subroutine interp

