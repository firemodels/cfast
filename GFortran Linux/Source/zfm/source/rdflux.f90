subroutine rdflux(iroom,nzone,nup,figs,taul,tauu,qllay,qulay,c)

  use precision
  use zonedata
  implicit none

  type(room_data), pointer :: r
  type(fire_data), pointer :: f
  integer, intent(in) :: iroom
  integer, intent(in) :: nzone, nup
  real(kind=dd), intent(in), dimension(4,4) :: taul, tauu

  real(kind=dd) :: hlay
  real(kind=dd) :: sigma
  real(kind=dd), intent(out) :: qllay,qulay
  real(kind=dd) :: eu, el, qugas
  real(kind=dd), dimension(4) :: c, area
  real(kind=dd), dimension(4,4) :: figs
  real(kind=dd) :: wf,qlgas
  real(kind=dd) :: qfflux
  real(kind=dd) :: factl, factu
  integer :: ifire

  integer :: j, k


  r => rooms(iroom)
  sigma = 5.67d-8
  hlay=r%rel_layer_height


! define c vector

  qulay = 0.d0
  qllay = 0.d0
  eu = sigma*r%layer(upper)%temperature ** 4
  el = sigma*r%layer(lower)%temperature ** 4
  do k = 1, nup
    c(k) = 0.d0

! case: upper to upper

    do j = 1, nup
      qugas = (1.0_dd-tauu(k,j))*eu
      c(k) = c(k) + figs(k,j)*qugas
      qulay = qulay - area(k)*figs(k,j)*qugas
    end do

! case: lower to upper

    do j = nup + 1, nzone
      qugas = (1.0_dd-tauu(k,j))*eu
      qlgas = (1.0_dd-taul(k,j))*el
      c(k) = c(k) + figs(k,j)*(qugas+qlgas*tauu(k,j))
      wf = area(k)*figs(k,j)
      qulay = qulay + qlgas*wf*(1.0_dd-tauu(k,j)) - qugas*wf
      qllay = qllay - qlgas*wf
    end do

! case: fire to upper layer

    do ifire = 1, nfires
      f=>fires(ifire)
      qfflux = 0.25d0*f%qtotal*f%angle(k) / (pi*area(k))
      c(k) = c(k) + qfflux*f%taufl(k)*f%taufu(k)
      if (f%dz.gt.hlay) then
        factu = 1.0_dd - f%taufu(k)
        factl = 0.0d0
      else
        factu = (1.0_dd-f%taufu(k))*f%taufl(k)
        factl = 1.0_dd - f%taufl(k)
      end if
      qulay = qulay + factu*qfflux*area(k)
      qllay = qllay + factl*qfflux*area(k)
    end do
  end do

  do k = nup + 1, nzone
    c(k) = 0.0d0

! case: upper to lower

    do j = 1, nup
      qugas = (1.0_dd-tauu(k,j))*eu
      qlgas = (1.0_dd-taul(k,j))*el
      c(k) = c(k) + figs(k,j)*(qugas*taul(k,j)+qlgas)
      wf = area(k)*figs(k,j)
      qulay = qulay - qugas*wf
      qllay = qllay + qugas*wf*(1.0_dd-taul(k,j)) - qlgas*wf
    end do

! case: lower to lower

    do j = nup + 1, nzone
      qlgas = (1.0d0-taul(k,j))*el
      c(k) = c(k) + figs(k,j)*qlgas
      qllay = qllay - qlgas*area(k)*figs(k,j)
    end do

! case: fire to lower layer

    do ifire = 1, nfires
      f=>fires(ifire)
      qfflux = 0.25d0*f%qtotal*f%angle(k) / (pi*area(k))
      c(k) = c(k) + qfflux*f%taufl(k)*f%taufu(k)
      if (f%dz.gt.hlay) then
        factu = 1.0_dd - f%taufu(k)
        factl = (1.0_dd-f%taufl(k))*f%taufu(k)
      else
        factu = 0.0d0
        factl = 1.0_dd - f%taufl(k)
      end if
      qulay = qulay + factu*qfflux*area(k)
      qllay = qllay + factl*qfflux*area(k)
    end do
  end do
  return
end subroutine rdflux
