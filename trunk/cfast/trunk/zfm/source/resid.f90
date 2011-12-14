
! --------------- resid ----------------------

subroutine resid (tsec,x,xpsolvesub,deltasub,ires,rpar,ipar)
  use precision
  use zonedata
  implicit none

! declarations for the argument list

  real(kind=dd), intent(in) :: tsec
  real(kind=dd), dimension(neq), intent(in) :: x, xpsolvesub
  real(kind=dd), dimension(neq), intent(out) :: deltasub
  real(kind=dd), dimension(maxspecies,2) :: speciesdot
  integer :: ires
  real(kind=dd), dimension(*) :: rpar
  integer,  dimension(*) :: ipar
  integer :: iiroom

  real(kind=dd), dimension(neq) :: xprime
  type(room_data), pointer :: r
  integer :: iroom, ispec
  real(kind=dd) :: vroom, pabs, hinter, qldot, qudot, mldot, mudot, &
                          tu, tl, rhou, rhol, massu, massl, volu, &
                          pdot, tlaydu, vlayd, tlaydl
  real(kind=dd) :: oxyldot, oxyudot

  totalflow(0:nrooms,1:2) = zeroflow
  fflow(0:nrooms,1:2) = zeroflow
  hvflow(0:nrooms,1:2) = zeroflow
  hflow(0:nrooms,1:2) = zeroflow
  fflow(0:nrooms,1:2) = zeroflow
  cflow(0:nrooms,1:2) = zeroflow

! convert solver data, x, to f90 data structures

  call datacopy(tsec,x)

! compute mass, energy and species flows for each sub-model type

  call hventflow
  call fireflow
  call hvacflow
  call convecflow

! sum flows

  do iroom = 1, nrooms
    totalflow(iroom,lower) =  hflow(iroom,lower) + fflow(iroom,lower) + hvflow(iroom,lower) + cflow(iroom,lower)
    totalflow(iroom,upper) =  hflow(iroom,upper) + fflow(iroom,upper) + hvflow(iroom,upper) + cflow(iroom,upper)
  end do

! calculate rhs of ode's for each room
  if(printresid.and.debugprint)write(6,*)"tsec=",tsec
  iiroom = 0
  do iroom = 1, nrooms
    r => rooms(iroom)
    vroom = r%volume
    pabs = r%abs_pressure
    hinter = r%rel_layer_height
    qldot = totalflow(iroom,lower)%qdot
    qudot = totalflow(iroom,upper)%qdot
    mldot = totalflow(iroom,lower)%mdot
    mudot = totalflow(iroom,upper)%mdot
    if(r%singlezone.eq.0)then
      iiroom = iiroom + 1
     else
      qudot = qldot + qudot
      qldot = 0.0
      mudot = mldot + mudot
      mldot = 0.0
    endif
    if(solveoxy)then
      oxyldot = totalflow(iroom,lower)%sdot(oxygen)
      oxyudot = totalflow(iroom,upper)%sdot(oxygen)
      if(r%singlezone.eq.1)then
        oxyudot = oxyudot + oxyldot 
        oxyldot = 0.0
      endif
    endif
    if(solveprods)then
      do ispec = 2, maxspecies
        speciesdot(ispec,lower) = totalflow(iroom,lower)%sdot(ispec)
        speciesdot(ispec,upper) = totalflow(iroom,upper)%sdot(ispec)
        if(r%singlezone.eq.1)then
          speciesdot(ispec,upper) = speciesdot(ispec,lower) + speciesdot(ispec,upper)
          speciesdot(ispec,lower) = 0.0
        endif
      end do
    endif
    tu = r%layer(upper)%temperature
    tl = r%layer(lower)%temperature
    if(printresid.and.debugprint)then
      write(6,"(i2,1x,3(e15.8,1x))")iroom,qldot,qudot,qldot+qudot
      write(6,"(3x,3(e15.8,1x))")hinter,tl,tu
    endif
    rhou = r%layer(upper)%density
    rhol = r%layer(lower)%density
    massu = r%layer(upper)%mass
    massl = r%layer(lower)%mass
    volu = r%VU

!     pressure equation

    pdot = (gamma - 1.0_dd)*(qldot + qudot)/vroom

!     upper layer temperature equation

    tlaydu = (qudot - cp*mudot*tu)/(cp*massu)
    tlaydu = tlaydu + pdot / (cp*rhou)

!     upper layer volume equation

    if(r%singlezone.eq.0)then
      vlayd = (gamma - 1.0_dd)*qudot/(gamma*pabs)
      vlayd = vlayd - volu*pdot/(gamma*pabs)
    endif

!     lower layer temperature equation

    if(r%singlezone.eq.0)then
      tlaydl = (qldot - cp*mldot*tl)/(cp*massl)
      tlaydl = tlaydl + pdot / (cp*rhol)
    endif

    xprime(iroom + offset_p) = pdot
    if(r%singlezone.eq.0)then
      xprime(iiroom + offset_tl) = tlaydl
      xprime(iiroom + offset_vu) = vlayd
    endif
    xprime(iroom + offset_tu) = tlaydu
    if(solveoxy)then
      if(r%singlezone.eq.0)xprime(iiroom + offset_oxyl) = oxyldot
      xprime(iroom + offset_oxyu) = oxyudot
    endif
    if(solveprods)then
      do ispec = 2, maxspecies
        if(r%singlezone.eq.0)xprime(iiroom + offset_SPECIES(ispec,lower)) = speciesdot(ispec,lower)
        xprime(iroom + offset_SPECIES(ispec,upper)) = speciesdot(ispec,upper)
      end do
    endif


  end do

  deltasub(1:neq) = xprime(1:neq) - xpsolvesub(1:neq)

  return
  end subroutine resid
