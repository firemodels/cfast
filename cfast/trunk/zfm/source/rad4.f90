  subroutine rad4(iroom,qflux,qlay)
  use precision
  use zonedata
  implicit none

  integer, intent(in) :: iroom
  real(kind=dd), intent(out), dimension(2) :: qlay
  real(kind=dd), intent(out), dimension(4) :: qflux

  type(room_data), pointer :: r

  real(kind=dd) :: sigma, f14, rdparfig, firang, ddot, f1d, ff14, f4d, twall
  real(kind=dd) :: dx2, dy2, dz2, dh2, x2, y2, aij, qllay, qulay, emis
  real(kind=dd) :: xroom, yroom, zroom, hlay


  real(kind=dd), dimension(4,4) :: tauu, taul, a, b, beam, figs
  real(kind=dd), dimension(4) :: area, dqde, qout, dq, zz, e, c, rhs

  integer, dimension(4) :: ipvt
  integer :: info
  integer :: ierror
  integer :: i, j, k, ifire

  logical :: black

! define local constants first time rad4 is called

  black = .false.
  sigma = 5.67d-8
  r => rooms(iroom)
  xroom=r%dx
  yroom=r%dy
  zroom=r%dz
  hlay=r%rel_layer_height



! define areas

  area(1) = xroom*yroom
  area(2) = 2.0d0*(zroom-hlay)*(xroom+yroom)
  area(3) = 2.0d0*hlay*(xroom+yroom)
  area(4) = area(1)

! define configuration factors

  f1d = rdparfig(xroom,yroom,zroom-hlay)
  ff14 = r%fig14
  f4d = rdparfig(xroom,yroom,hlay)

  figs(1,1) = 0.0d0
  figs(1,2) = 1.0d0-f1d
  figs(2,1) = area(1)*figs(1,2)/area(2)
  figs(2,2) = 1.0d0-2.0d0*figs(2,1)
  figs(1,4) = ff14
  figs(4,1) = figs(1,4)

  figs(4,4) = 0.0d0
  figs(4,3) = 1.0d0-f4d
  figs(3,4) = area(4)*figs(4,3)/area(3)
  figs(3,3) = 1.0d0-2.0d0*figs(3,4)

  figs(1,3) = 1.0d0-figs(1,4)-figs(1,2)
  figs(3,1) = area(1)*figs(1,3)/area(3)

  figs(3,2) = 1.0d0-figs(3,4)-figs(3,3)-figs(3,1)
  figs(2,3) = area(3)*figs(3,2)/area(2)

  figs(2,4) = 1.0d0-figs(2,3)-figs(2,2)-figs(2,1)
  figs(4,2) = area(2)*figs(2,4)/area(4)

! define transmission factors for surfaces

!    but first define beam lengths

  zz(1) = zroom
  zz(2) = (hlay+zroom) *.50d0
  zz(3) = hlay*.50d0
  zz(4) = 0.0d0
  dx2 = (xroom*.50d0) ** 2
  dy2 = (yroom*.50d0) ** 2
  x2 = xroom ** 2
  y2 = yroom ** 2

  beam(1,1) = 0.0d0

  dz2 = (zz(1)-zz(2)) ** 2
  beam(1,2) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*.50d0

  dz2 = (zz(1)-zz(3)) ** 2
  beam(1,3) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*.50d0

  beam(1,4) = zroom
  beam(2,2) = (xroom+yroom)*.50d0
  dz2 = (zroom*.50d0) ** 2
  beam(2,3) = (sqrt(dz2+x2)+sqrt(dz2+y2))*.50d0
  dz2 = ((zroom+hlay)*0.50d0) ** 2
  beam(2,4) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*.50d0
  beam(3,3) = beam(2,2)
  dh2 = (hlay*.50d0) ** 2
  beam(3,4) = (sqrt(dh2+dx2)+sqrt(dh2+dy2))*.50d0
  beam(4,4) = 0.0d0
  do i = 1, 4
    do j = i+1, 4
       beam(j,i) = beam(i,j)
    end do
  end do

  call rdrtran(4,2,absorb,beam,hlay,zz,tauu,taul,black)

! define transmission factors for fires

  if (nfires.ne.0) then
    call rdftran(4,2,absorb,hlay,zz,black)
  end if

! define solid angles for fires

  if (nfires.ne.0) then
    do ifire = 1, nfires
      call rdfang(xroom,yroom,zroom,hlay,firang)
      fires(ifire)%angle=firang
    end do
  end if

! note: we want to solve the linear system
!          a*dq = b*e+c
!   where a and b are nxn matrices, q, e and c are n vectors
!
! define e vector

  do i = 1, 4
    twall = r%wall(i)%temp
    e(i) = sigma*twall**4
  end do

! define 'a' and 'b' coefficient matrix

  do k = 1, 4
    do j = 1, 4
      aij = figs(k,j)*taul(k,j)*tauu(k,j)
      emis = r%wall(j)%emis
      a(k,j) = -aij*(1.0d0-emis)
      b(k,j) = -aij
    end do
    a(k,k) = a(k,k)+1.0d0
    b(k,k) = b(k,k)+1.0d0
  end do
      

! define c vector
!    also, calculate energy absorbed by upper, lower layer gases
!          due to fires and gas layer emission

      call rdflux(iroom,4,2,area,hlay,figs,taul,tauu,qllay,qulay,c)

! construct right hand side (rhs) of linear system to be solved
!    i.e. compute b*e-c
  do k = 1, 4
    rhs(k) = ddot(4,b(k,1),4,e(1),1)-c(k)
  end do

! solve the linear system

  call dgefa(a,4,4,ipvt,info)
  if(info.ne.0) then
    call xerror('rad4-singular matrix',0,1,1)
    ierror = 18
    do k = 1, 4
      rhs(k) = 0.0d0
    end do
   else
    call dgesl(a,4,4,ipvt,rhs,0)
  endif

! note: each row k of the a matrix, as defined by seigal and howell 
!    was divided by emis(k) (in order to insure that this new 'a' was
!    diagonally dominant.  now we have to multiply the solution to the
!    modified problem by emis(i) to get the answer to the original
!    problem

  do k = 1, 4
    dqde(k) = rhs(k)
    emis = r%wall(k)%emis
    qout(k) = e(k)-(1.0d0-emis)*dqde(k)
    dq(k) = rhs(k)*emis
  end do

! take solution and compute energy gain or loss to each panel
!    and each layer.  also compute fluxes.  change sign so that
!    a postive flux means that heat is flowing to the wall

  do i = 1, 4
    qflux(i) = -dq(i)
  end do

! compute radiation absorbed by each layer

  call rdabs(4,2,e,dqde,emis,area,figs,tauu,taul,qllay,qulay)

  qlay(upper) = qulay
  qlay(lower) = qllay

  return
end subroutine rad4
