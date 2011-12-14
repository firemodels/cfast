
! --------------- solve ----------------------

subroutine solve
  use precision
  use zonedata
  implicit none

  integer, dimension(15) :: info
  integer :: idid
  integer :: ipar,error
  real(kind=dd) :: rpar
  integer :: ires
  external resid, jac


  call setinfo(info)
  call dumpcase(dumpfile,error,.false.)
  call output
  do while (tnow .lt. tfinal)
    tout = min(tfinal, tprint, tdump, tplot)
    rwork(1)=tout
10  continue
    call setstate(.false.)
    call ddassl (resid, neq, tnow, p, pprime, tout, info, vrtol, vatol, &
                 idid, rwork, lrw, iwork, liw, rpar, ipar, jac)
    call setstate(.true.)
    zerosoln(1:neq) = zero
    call resid (tnow,p,zerosoln,dummysoln,ires,rpar,ipar)
    if(idid.eq.-1)then
      write(6,*)"too many steps"
      info(1) = 1
      go to 10
    endif
    call output
  end do
  return
end subroutine solve

subroutine output
  use precision
  use zonedata
  implicit none
  integer :: error
  if(abs(tprint-tnow).lt.0.001.or.tnow.ge.tfinal)then
    iprint = iprint + 1
    tprint = tstartprint + iprint*dprint
#ifndef pp_inverse
    call result
#endif
  end if
  if(abs(tnow-tdump).lt.0.001.or.tnow.ge.tfinal)then
    idump = idump + 1
    tdump = tstartdump + idump*ddump
    call dumpcase(dumpfile,error,.true.)
  end if
  if(abs(tnow-tplot).lt.0.001.or.tnow.ge.tfinal)then
    iplot = iplot + 1
    tplot = tstartplot + iplot*dplot
    call plot
  end if
  return
end subroutine output

subroutine gettemp(templ,tempu)
  use precision
  use zonedata
  implicit none
  real(kind=dd), dimension(*) :: templ, tempu
  type(room_data), pointer :: r
  integer :: iroom

  do iroom = 1, nrooms
	  r => rooms(iroom)  ! get room pointer
    templ(iroom) = r%layer(lower)%temperature
    tempu(iroom) = r%layer(upper)%temperature
  end do

  return
end subroutine gettemp

subroutine initsolve
  use precision
  use zonedata
  implicit none
  integer :: ipar
  real(kind=dd) :: rpar
  integer :: i, ires
  integer :: error
  integer :: iiroom,ispec

  iiroom = 0
  do i = 1, nrooms
    vatol(i+offset_p) = aptol
    vrtol(i+offset_p) = rptol
    vatol(i+offset_tu) = atol
    vrtol(i+offset_tu) = rtol
    if(solveoxy)then
      vatol(i+offset_oxyu) = atol
      vrtol(i+offset_oxyu) = rtol
    endif
    if(solveprods)then
      do ispec = 2, maxspecies
        vatol(i+offset_SPECIES(ispec,upper)) = atol
        vrtol(i+offset_SPECIES(ispec,upper)) = rtol
      end do
    endif


    if(rooms(i)%singlezone.eq.0)then
      iiroom = iiroom + 1
      vatol(iiroom+offset_vu) = atol
      vrtol(iiroom+offset_vu) = rtol
      vatol(iiroom+offset_tl) = atol
      vrtol(iiroom+offset_tl) = rtol
      if(solveoxy)then
        vatol(iiroom+offset_oxyl) = atol
        vrtol(iiroom+offset_oxyl) = rtol
      endif
      if(solveprods)then
        do ispec = 2, maxspecies
          vatol(iiroom+offset_SPECIES(ispec,lower)) = atol
          vrtol(iiroom+offset_SPECIES(ispec,lower)) = rtol
        end do
      endif
    endif
  end do

  call initsoln
  lrw = 40 + 9*neq + neq**2
  liw = 20 + neq
  if(allocated(rwork))deallocate(rwork)
  if(allocated(iwork))deallocate(iwork)
  allocate(rwork(lrw), iwork(liw),stat=error)
  if(error.ne.0)then
    write(6,*)"error allocating work arrays"
    stop
  endif

  xpsolve(1:neq) = zero
  call resid (tnow,p,xpsolve,delta,ires,rpar,ipar)
  pprime(1:neq) = delta(1:neq)


end subroutine initsolve
subroutine initsoln
  use precision
  use zonedata
  implicit none
  integer :: iroom,iiroom,ispec
  type(room_data), pointer :: r
  iiroom=0
  do iroom = 1, nrooms
    r => rooms(iroom)
    p(offset_p + iroom) = r%rel_pressure
    if(r%singlezone.eq.0)then
      iiroom = iiroom + 1
    	p(offset_vu + iiroom) = r%VU
    	p(offset_tl + iiroom) = r%layer(lower)%temperature
    endif
  	p(offset_tu + iroom) = r%layer(upper)%temperature
    if(solveoxy)then
    	if(r%singlezone.eq.0)p(offset_oxyl + iiroom) = r%layer(lower)%s_mass(oxygen)
    	p(offset_oxyu + iroom) = r%layer(upper)%s_mass(oxygen)
    endif
    if(solveprods)then
      do ispec = 2, maxspecies
    	  if(r%singlezone.eq.0)p(offset_SPECIES(ispec,lower) + iiroom) = r%layer(lower)%s_mass(ispec)
    	  p(offset_SPECIES(ispec,upper) + iroom) = r%layer(upper)%s_mass(ispec)
      end do
    endif
  end do
  if(smvfile.ne."")call svplothdr(plotfile,1,nrooms,nfires)
end subroutine initsoln

subroutine result
  use precision
  use zonedata
  implicit none
  integer :: iroom
  type(room_data), pointer :: r
  type(zone_data), pointer :: llay, ulay
  integer :: ispec

  do iroom = 1, nrooms
    r => rooms(iroom)
    llay => r%layer(lower)
    ulay => r%layer(upper)
    if(solveprods)then
  	  write(6,10)tnow,iroom,r%rel_pressure,r%rel_layer_height,&
                 llay%temperature,ulay%temperature,fires(1)%qtotal,&
                 llay%s_con(oxygen),ulay%s_con(oxygen),&
                 (llay%s_con(ispec),ulay%s_con(ispec),ispec=2,maxspecies)
    else if(solveoxy.and..not.solveprods)then
  	  write(6,10)tnow,iroom,r%rel_pressure,r%rel_layer_height,&
                 llay%temperature,ulay%temperature,fires(1)%qtotal,&
                 llay%s_con(oxygen),ulay%s_con(oxygen)
     else
  	  write(6,10)tnow,iroom,r%rel_pressure,r%rel_layer_height,&
                 llay%temperature,ulay%temperature,fires(1)%qtotal
    endif
10  format(1x,e11.4,",",i3,13(",",e11.4)) 
  end do
  write(6,*)""
end subroutine result

subroutine dump
end subroutine dump

subroutine plot
  use precision
  use zonedata
  implicit none
  integer :: iroom, ifire
  real, dimension(nrooms) :: ylay, tl, tu,pr
  real :: tt
  real, dimension(nfires) :: qdot,height
  type(room_data), pointer :: r
  type(fire_data), pointer :: f
  if(smvfile.ne."")then
    tt = tnow
    do iroom = 1, nrooms
      r => rooms(iroom)
      pr(iroom) = r%rel_pressure
    	ylay(iroom) = r%abs_layer_height
      tl(iroom) = r%layer(lower)%temperature
      tu(iroom) = r%layer(upper)%temperature 
    end do
    do ifire=1, nfires
      f=>fires(ifire)
      qdot(ifire) = f%qconvec
      height(ifire) = f%dz
    end do
     write(csvunit,10)tnow,(rooms(iroom)%layer(upper)%temperature,iroom=1,nrooms)
10  format(e11.4,",",10(1(e11.4,",")))

    call svplotdata(plotfile,tt,nrooms,pr,ylay,tl,tu,nfires,qdot,height)

  endif

end subroutine plot

! --------------- setinfo ----------------------

subroutine setinfo(info)
  implicit none
  integer, dimension(15), intent(out) :: info

  info = 0
  info(3) = 1
  info(2) = 1
  info(4) = 1

!     setting jacobian flag

  info(5) = 0
  info(11) = 0
  return
end subroutine setinfo
subroutine jac
end subroutine jac

subroutine setstate(state)
  use precision
  use zonedata
  implicit none

  logical, intent(in) :: state

  printresid = state
  return
end subroutine setstate

