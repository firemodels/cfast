subroutine convec(iw,tg,tw,qdinl)
  use precision
  use zonedata
  implicit none


!     Description:  Calculate convective heat transfer for a wall segment
!                   Note that we have simplified the convection calculation
!                   by assuming turbulent flow.  This allows us to remove the
!                   dependency on the characterisitic length and avoid a divide
!                   by zero as the surface vanishes.  If a more general
!                   relationship is desired, the code will have to be reworked
!                   to include the characteristic length in the calculation.
!
!     Arguments: IW     Wall number, standand CFAST numbering convention
!                TG     Temperature of gas layer adjacent to wall surface
!                TW     Wall surface temperature
!                QDINL  Convective flux into wall surface IW
!
!     Revision History:
!        Created:  by WWJ
!        Modified: 1/2/1987 at 16:19 by WWJ:
!                  change number of surfaces to be consistent with four-wall 
!                  numbering. QDINAL is now multiplied by (TG-TW). This was 
!                  missing.
!        Modified: 1/4/1993 at 16:20 by RDP & GPF:
!                  changed calculation to be consistent with SPFE Handbook 
!                  chapter on convective heat transfer. Calculation of thermal 
!                  properties and correlations come from there.  Smooth
!                  transition between correlations added with TANH.  
!                  Vertical tangent in Nusselt number was eliminated (when
!                  gas and wall temperatures are close) in order to improve
!                  numerical characteristics.  Note this has little effect
!                  on flux calculation.
!

  real(kind=eb), intent(in) :: tg, tw
  integer, intent(in) :: iw
  real(kind=eb), intent(out) :: qdinl
  real(kind=eb) :: nuoverl, k, tdel, x1del, xthird, tf
  real(kind=eb) :: t3000, tff, alpha, pr, cup, cdown, c, abstwtg

  tdel = 5.0_eb
  x1del = 1.0_eb / tdel
  xthird = 1.0_eb / 3.0_eb

  qdinl = 0.0_eb
  tf = (tw+tg) * 0.50_eb
!
!*** To keep K positive, make sure TF is below 3000.  Of course the
!    calculation will have long since lost any semblance to reality.
!
  t3000 = 3000.0_eb
  tff = min(tf,t3000)
  tf = tff
  if (tf.le.0.0_eb) return
  alpha = (tff ** (1.75_eb))/1000000000.0_eb
  k = (0.0209_eb+2.33_eb*tff/100000.0_eb) / (1.0_eb-2.67_eb*tff/10000.0_eb)
  pr = 0.72_eb
!
!     CEILINGS AND FLOORS
!     Use the hyperbolic tangent to smooth the coefficient C 
!     from CUP to CDOWN over a temperature range of TDEL degress. 
!     Note: Tanh(x>>0)=1 and Tanh(x<<0)=-1 .
!
  cup = 0.16_eb
  cdown = 0.13_eb
  if (iw.eq.1) then
    c = (cup+cdown+(cup-cdown)*tanh((tg-tw)*x1del)) * 0.50_eb
  else if (iw.eq.2) then
    c = (cup+cdown-(cup-cdown)*tanh((tg-tw)*x1del)) * 0.50_eb

!     VERTICAL SURFACES

  else
   c = 0.121_eb
  end if
!
!     Prevent the vertical tangent in the calculation of NUOVERL
!     by keeping ABSTWTG from going to zero.  
!
  abstwtg = max(abs(tw-tg),tdel)
  abstwtg=300.0_eb

  nuoverl = c * (g*abstwtg*pr/(tf*alpha**2)) ** xthird
!
  qdinl = nuoverl * k * (tg-tw)
  return
end subroutine convec
