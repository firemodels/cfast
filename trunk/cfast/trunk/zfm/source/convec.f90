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

  real(kind=dd), intent(in) :: tg, tw
  integer, intent(in) :: iw
  real(kind=dd), intent(out) :: qdinl
  real(kind=dd) :: nuoverl, k, tdel, x1del, xthird, tf
  real(kind=dd) :: t3000, tff, alpha, pr, cup, cdown, c, abstwtg

  tdel = 5.0_dd
  x1del = 1.0_dd / tdel
  xthird = 1.0_dd / 3.0_dd

  qdinl = 0.0_dd
  tf = (tw+tg) * 0.50_dd
!
!*** To keep K positive, make sure TF is below 3000.  Of course the
!    calculation will have long since lost any semblance to reality.
!
  t3000 = 3000.0_dd
  tff = min(tf,t3000)
  tf = tff
  if (tf.le.0.0_dd) return
  alpha = (tff ** (1.75_dd))/1000000000.0_dd
  k = (0.0209_dd+2.33_dd*tff/100000.0_dd) / (1.0_dd-2.67_dd*tff/10000.0_dd)
  pr = 0.72_dd
!
!     CEILINGS AND FLOORS
!     Use the hyperbolic tangent to smooth the coefficient C 
!     from CUP to CDOWN over a temperature range of TDEL degress. 
!     Note: Tanh(x>>0)=1 and Tanh(x<<0)=-1 .
!
  cup = 0.16_dd
  cdown = 0.13_dd
  if (iw.eq.1) then
    c = (cup+cdown+(cup-cdown)*tanh((tg-tw)*x1del)) * 0.50_dd
  else if (iw.eq.2) then
    c = (cup+cdown-(cup-cdown)*tanh((tg-tw)*x1del)) * 0.50_dd

!     VERTICAL SURFACES

  else
   c = 0.121_dd
  end if
!
!     Prevent the vertical tangent in the calculation of NUOVERL
!     by keeping ABSTWTG from going to zero.  
!
  abstwtg = max(abs(tw-tg),tdel)
  abstwtg=300.0_dd

  nuoverl = c * (g*abstwtg*pr/(tf*alpha**2)) ** xthird
!
  qdinl = nuoverl * k * (tg-tw)
  return
end subroutine convec
