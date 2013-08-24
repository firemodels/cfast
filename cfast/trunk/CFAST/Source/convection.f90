
! --------------------------- convec -------------------------------------------

    subroutine convec (iw,tg,tw,qdinl)

    !     routine: convec
    !     purpose: calculate convective heat transfer for a wall segment. note that we have simplified the convection calculation
    !              by assuming turbulent flow.  this allows us to remove the dependency on the characterisitic length and avoid a divide
    !              by zero as the surface vanishes.  if a more general relationship is desired, the code will have to be reworked
    !              to include the characteristic length in the calculation.
    !     arguments:  iw     wall number, standand cfast numbering convention
    !                 tg     temperature of gas layer adjacent to wall surface
    !                 tw     wall surface temperature
    !                 qdinl  convective flux into wall surface iw

    use precision_parameters
    implicit none
    
    integer, intent(in) :: iw
    real(eb), intent(in) :: tg, tw
    real(eb), intent(out) :: qdinl
    
    real(eb) nuoverl, k, tdel, x1del, tf,   t3000, tff, alpha, pr, cup, cdown, c, abstwtg

    tdel = 5.0_eb
    x1del = 1.0_eb/tdel
    qdinl = 0.0_eb
    tf = (tw + tg)/2.0_eb

    ! to keep k positive, make sure tf is below 3000.  of course the calculation will have long since lost any semblance to reality.
    
    t3000 = 3000.0_eb
    tff = min(tf,t3000)
    if (tf<=0.0_eb) return
    alpha = 1.e-9_eb*tf**1.75_eb
    k = (0.0209_eb+2.33e-5_eb*tff)/(1._eb-2.67e-4_eb*tff)
    pr = 0.72_eb

    ! ceilings and floors
    ! use the hyperbolic tangent to smooth the coefficient c from cup to cdown over a temperature range of tdel degress. 
    ! note: tanh(x>>0)=1 and tanh(x<<0)=-1
    
    cup = 0.16_eb
    cdown = 0.13_eb
    if (iw==1) then
        c = (cup+cdown+(cup-cdown)*tanh((tg-tw)*x1del))/2.0_eb
    else if (iw==2) then
        c = (cup+cdown-(cup-cdown)*tanh((tg-tw)*x1del))/2.0_eb

        ! vertical surfaces
    else
        c = 0.121_eb
    endif

    ! prevent the vertical tangent in the calculation of nuoverl by keeping abstwtg from going to zero. 
     
    abstwtg = abs(tw-tg)
    if (abstwtg<tdel) abstwtg = tdel
    nuoverl = c*(g*abstwtg*pr/(tf*alpha**2))**third
    qdinl = nuoverl*k*(tg-tw)
    return
    end subroutine convec

! --------------------------- cvheat -------------------------------------------

   subroutine cvheat (flwcv,flxcv)

   !     routine:    cfcnvc
   !     function:   interface between resid and convec.  loops over rooms
   !                 setting up varibles.  passes to convec if ceiling jet for
   !                 a surface is off, otherwise sets flxcv to 0.0 and then
   !                 solves for flwcv
   !     outputs:    flwcv       net enthalphy into each layer 
   !                 flxcv       net heat flux onto surface

   use precision_parameters
   use cparams
   use cenviro
   use cfast_main
   use opt
   use wnodes
   implicit none

   real(eb), intent(out) :: flwcv(nr,2), flxcv(nr,nwal)
    
   real(eb) :: flwcv0(nr,2), flxcv0(nr,nwal)
   integer cjetopt, i, j, ieqtyp, iroom, iwall, iw, nrmfire, ilay
   logical roomflg(nr), wallflg(4*nr)
   save flwcv0, flxcv0

   flwcv(1:nm1,1:2) = 0.0_eb
   flxcv(1:nm1,1:nwal) = 0.0_eb
    
   if (option(fconvec)/=on) return

   cjetopt = option(fcjet)

   roomflg(1:nm1) = .true.
   wallflg(1:nwalls) = .true.

   if (option(fmodjac)==on) then
      if (jaccol>0) then

      ! if 2nd modified jacobian is active and dassl is computing a jacobian then
      ! only compute convection heat transfer in the room where the dassl 
      ! solution variable has been perturbed

         roomflg(1:nm1) = .false.
         wallflg(1:nwalls) = .false.

         ieqtyp = izeqmap(jaccol,1)
         iroom = izeqmap(jaccol,2)
         roomflg(iroom) = .true.
         if (ieqtyp==eqtu.or.ieqtyp==eqvu.or.ieqtyp==eqtl.or.ieqtyp==eqwt) then
            if (ieqtyp==eqwt)iroom = izwall(iroom,1)
            do iwall = 1, 4
               if (switch(iwall,iroom)) then
                  iw = izwmap2(iwall,iroom) - nofwt
                  wallflg(iw) = .true.
               endif
            end do
         endif
      endif
   endif

   do iw = 1, nwalls
      if (wallflg(iw)) then
         i = izwall(iw,1)
         iwall = izwall(iw,2)
         nrmfire = ifrpnt(i,1)
         if (mod(iwall,2)==1) then
            ilay = upper
         else
            ilay = lower
         endif

      ! ceiling jet heat transfer is not active if cjetopt=2.  use normal (call convec) instead
      
         if (cjetopt/=2.and.cjeton(iwall).and.nrmfire/=0) then
            flxcv(i,iwall) = 0.0_eb
         else
            call convec(iwall,zztemp(i,ilay),zzwtemp(i,iwall,1),flxcv(i,iwall))
         endif
         flwcv(i,ilay) = flwcv(i,ilay) - zzwarea(i,iwall)*flxcv(i,iwall)
      endif
   end do

   if (option(fmodjac)==on) then
      if (jaccol==0) then

      ! save the flux and flow vectors when we are about to compute a jacobian
            
         flwcv0(1:nm1,1:2) = flwcv(1:nm1,1:2)
         flxcv0(1:nm1,1:4) = flxcv(1:nm1,1:4)
      elseif (jaccol>0) then

      ! we are computing the jaccol'th column of the jacobian.  if the solution hasn't changed then get it from the vectors saved above.
         do iroom = 1, nm1
            if (.not.roomflg(iroom)) then
               flwcv(iroom,1:2) = flwcv0(iroom,1:2)
               flxcv(iroom,1:4) = flxcv0(iroom,1:4)
            endif
         end do
      endif
   endif      
   return
   end subroutine cvheat

! --------------------------- ceilht -------------------------------------------

   subroutine ceilht(mplume,qconv,atc,tl,tu,tw,xw,yw,zc,axf,ayf,zf,zlay,rhol,rhou,cjetopt,xd,yd,zd,nd,qceil,qfclga, &
    qfwla,qfwua,td,vd,tdmax,vdmax,ddmax)
    !
    !     routine:     ceilht
    !
    !     functional class:  
    !
    !     description:  
    !       this subroutine calculates convective heat transfer to
    !	the uniform temperature ceiling above a fire in a parallel-
    !	opiped room with a two-layer fire environment.  also calcu-
    !	lated is the total rate of heat transfer to the walls and
    !	the average flux to the upper and lower portions of the
    !	walls.

    !   	input
    !	mplume		mass flow rate in the plume at zlay if zf < zlay [kg/s]
    !         nd    number of detectors
    !	 qconv		portion of fire energy release rate convected in plume [w]
    !	    tc    average temperature of ceiling [k]
    !	 tl,tu    temperature of lower, upper layer [k]
    !	    tw    average temperature of wall surfaces [k]
    !   xd,yd,zd    x, y, and z positions of the detectors
    !   xw,yw,zc    co-ordinates of room corner diagonally opposite to origin 
    !               of axes (origin is at a corner on the floor, with x, y 
    !               axes along wall/floor junction and z axes upward) [m]
    !   xf,yf,zf    co-ordinates of center of base of fire [m]
    !       zlay    elevation above floor of layer interface [m]
    !  rhol,rhou    density of lower, upper layer [kg/m**3]
    !    cjetopt    heat transfer is not calculated if cjetopt=2
    !
    !	output
    !	-----
    !
    !      qceil    rate of heat transfer to ceiling [w]
    !     qfclga		average flux of heat transfer to ceiling [w/m**2]
    ! qfwla,qfwua   average flux of heat transfer to lower and
    !               upper portions of the wall sufaces [w/m**2]
    !         td    temperature of jet at xd,yd,zd locations
    !         vd    velocity of jet at xd,yd,zd locations
    !
    !***	some other definitions and fixed input in this subroutine

    !      alpha    tu/tl
    !     awl(n)    area of lower-layer portion of wall segment n
    !     awu(n)    area of upper-layer portion of wall segment n
    !         cp    specific heat of air at constant pressure [kj/(kg*k)]
    !         ct    9.115, constant in point source plume eqn.
    !          g    9.8, acceleration of gravity [m/s**2]
    !          h    if layer interface is below ceiling: distance
    !               of continuation source below ceiling; if layer interface 
    !               is at ceiling: distance of equivalent source below 
    !               ceiling [m]
    !      qcont    strength of continuation source [w]
    !        qeq    dimensionless strength of plume at zlay
    !         qh    dimensionless strength of plume at zc
    !   qfwcl(n)    average heat flux to wall near corner n in the 
    !               lower layer [w/m**2]
    !   qfwcu(n)    average heat flux to wall near corner n in
    !               the upper layer [w/m**2]
    !  qfwlan(n)    average heat flux to portion of wall segment
    !               n in the lower layer [w/m**2]
    !  qfwuan(n)    average heat flux to portion of wall segment
    !               n in the upper layer [w/m**2]
    !   qfwsl(n)    average heat flux to portion of wall in the lower layer 
    !               along the line passing through stagnation point n [w/m**2]
    !   qfwsu(n)    average heat flux to portion of wall in the upper layer 
    !               along the line passing through stagnation point n [w/m**2]
    !      qfwst    heat transfer flux to wall at a wall/ceiling-jet stagnation 
    !               point [w/m**2]
    !         pr    prandtl number
    !      rc(n)    distance from plume/ceiling impingement point to corner n [m]
    !      rs(n)    distance from plume/ceiling impingement point to wall 
    !               stagnation point n [m]
    !        tht    temperature of ambient in unconfined ceiling heat transfer 
    !               problem, either tl or tu [k] 
    !        zeq    elevation above floor of equivalent source in lower layer [m]
    !         zs    elevation above floor of continuation source [m]
    !       tdmax   maximum temperature of the ceiling jet
    !       vdmax   maximum velocity of the ceiling jet
    !       ddmax   estimate of ceiling jet depth at r/h = .2
    !               (given by:  ddmax/(.23*delta) = 2)


   use precision_parameters
   implicit none
    
    ! 
    !********************************************************************
    !   	this subroutine and the function qfclg used in its called subroutines use the common blocks aintch
    !********************************************************************
    !
   external qfclg

   integer, intent(in) :: nd,  cjetopt
   real(eb), intent(in) :: mplume, qconv, atc, tl, tu, tw, xd(*), yd(*), zd(*), xw, yw, zc, axf, ayf, zf,  zlay, rhol, rhou
   
   real(eb), intent(out) :: qceil, qfclga, qfwla, qfwua, td(*), vd(*), tdmax, vdmax, ddmax
    
   integer id, ntab, n
   real(eb) :: awl(8), awu(8), qfwcl(4), qfwcu(4), qfwlan(8), qfwuan(8), qfwsl(4), qfwsu(4), rc(4), rs(4)
   real(eb) :: ct, cp, pr, rk1, rfmin, wy, alpha, qeq, zeq, alfm1, a1, ssq, top, bottom
   real(eb) :: mfrac, qcont, zs, rhoht, sqrtgh, qh, qhp, anu, re, prp
   real(eb) :: rmax, rd, rdh, v, vmax, delta, dz, zdel, vcj, arg
   real(eb) :: rlamr, tmaxmtu, ths, thta, tcj, sumaql, sumaqu, sumal, sumau, qfclg
   
   real(eb) :: h, htct, tht, thtqhp, c1, c2, c3, c4, xf, yf, tc
   common /aintch/ h, htct, tht, thtqhp, c1, c2, c3, xf, yf, tc
   save /aintch/
 
   qceil = 0.0_eb
   qfclga = 0.0_eb
   qfwla = 0.0_eb
   qfwua = 0.0_eb
   if (mplume==0.0_eb.or.qconv==0.0_eb) then
      do id = 1, nd
         td(id) = max(tu,td(id))
         vd(id) = max(0.0_eb,vd(id))
      end do
      return
   endif

   ct = 9.115_eb
   cp = 1000.0_eb
   pr = 0.70_eb
   rk1 = (0.23_eb/0.77_eb)*log(sqrt(2.0_eb)-1.0_eb)
        
   xf = axf
   yf = ayf
   rfmin = .20_eb*(zc-zf)
   if (rfmin<xw/2.0_eb) then
      if (xf<rfmin) xf = rfmin
      if (xf>xw-rfmin) xf = xw - rfmin
   else
      xf = xw/2.0_eb
   endif
   if (rfmin<yw/2.0_eb) then
      if (yf<rfmin) yf = rfmin
      if (yf>yw-rfmin) yf = yw - rfmin
   else
      yf = yw/2.0_eb
   endif
   tc = atc
   alpha = tu/tl
   if (zf<zlay) then

   ! fire is below layer interface
   
      qeq = (0.21_eb*qconv/(cp*tl*mplume))**1.5_eb
      zeq = zlay - (qconv/(qeq*rhol*cp*tl*gsqrt))**(.4_eb)
      if (zlay<zc) then

      ! layer interface is below ceiling
      
         alfm1 = alpha - 1.0_eb
         if (alfm1/=0.0_eb) then
            sigma = -1.0_eb + ct*qeq**twothirds/alfm1
            a1 = sigma/(sigma+1.0_eb)
            if (sigma>0.0_eb) then
               ssq = sigma**2
               top = 1.04599_eb*sigma + 0.360391_eb*ssq
               bottom = 1._eb + 1.37748_eb*sigma + 0.360391_eb*ssq
               mfrac = top/bottom
            else
               mfrac = 0.0_eb
               qceil = 0.0_eb
               qfclga = 0.0_eb
               qfwla = 0.0_eb
               qfwua = 0.0_eb
               do id = 1, nd
                  td(id) = max(tu,td(id))
                  vd(id) = max(0.0_eb,vd(id))
               end do
               return
            endif
         else
            a1 = 1.0_eb
            mfrac = 1.0_eb
         endif
         qcont = qconv*a1*mfrac
         zs = zlay - (zlay-zeq)*alpha**.6_eb*mfrac**.4_eb/a1**0.2_eb
         tht = tu
         rhoht = rhou
      else

      ! layer interface is at ceiling
      
         qcont = qconv
         zs = zeq
         tht = tl
         rhoht = rhol
      endif
   else

   ! fire is at or above layer interface
   
      if (zf<zc) then

      ! fire is below ceiling
      
         qcont = qconv
         zs = zf
         tht = tu
         rhoht = rhou
      else

       ! fire is at ceiling
       
         qceil = 0.0_eb
         qfclga = 0.0_eb
         qfwla = 0.0_eb
         qfwua = 0.0_eb
         return
      endif
   endif
   h = zc - zs
   sqrtgh = sqrt(g*h)
   qh = qcont/(rhoht*cp*tht*sqrtgh*h**2)
   qhp = (qh**third)
   htct = rhoht*cp*sqrtgh*qhp
   anu = (0.04128e-7_eb*tht**2.5_eb)/(tht+110.4_eb)
   re = sqrtgh*h*qhp/anu
   thtqhp = tht*qhp**2
   prp = pr**twothirds
   c1 = 8.82_eb/(sqrt(re)*prp)
   c2 = 5._eb - 0.284_eb*re**0.2_eb
   c3 = 0.283032655_eb/(re**0.3_eb*prp)
   c4 = 0.94_eb/((re**0.42_eb)*pr)

   rmax = sqrt(max(yf,yw-yf)**2+max(xf,xw-xf)**2)

    ! make an integral table of size ntab from zero to rmax.
    
   ntab = 20
   call maktabl(rmax,ntab,qfclg)

    ! don't need to compute the following if we aren't computing ceiling jet heat transfer
    
   if (cjetopt/=2) then
      call int2d(xf,yf,xw,yw,rmax,qceil)
      qfclga = qceil/(xw*yw)
   endif

    ! now calculate wall heat transfer: ***	step 1. calculate radii at wall stagnation points
    
   rs(1) = yf
   rs(2) = xw - xf
   rs(3) = yw - yf
   rs(4) = xf

   !calculate velocity and temperatures of the ceiling jet at various locations
   
   do id = 1, nd
      rd = sqrt((axf-xd(id))**2+(ayf-yd(id))**2)
      rdh = rd/h
      if (rdh<=.20_eb) then
         rdh = .20_eb
         rd = rdh*h
      endif
      v = sqrtgh*qhp
      vmax = .85_eb*v/rdh**1.1_eb
      vdmax = vmax
      delta = .1_eb*h*rdh**.9_eb
      dz = zc - zd(id)
      zdel = dz/(.23_eb*delta)
      ddmax = 2.0_eb*0.23_eb*delta
      if (zdel<=1.0_eb) then
         vcj = vmax*zdel**(1.0_eb/7.0_eb)*(8.0_eb-zdel)/7.0_eb
      else
         arg = rk1*(zdel-1.0_eb)
         vcj = vmax/cosh(arg)**2
      endif

      call inttabl(rd,rlamr)
      rlamr = 2.0_eb*pi*rlamr/qconv
      tmaxmtu = 2.6_eb*(1.0_eb-rlamr)*qh**twothirds*tu/rdh**.80_eb - .90_eb*(tc-tu)
      ths = (tc-tu)/tmaxmtu
      if (zdel<=1.0_eb) then
         thta = ths + 2.0_eb*(1.0_eb-ths)*zdel - (1.0_eb-ths)*zdel** 2
      else
         thta = vcj/vmax
      endif
      tcj = tu + thta*tmaxmtu
      tdmax = tu + tmaxmtu
      td(id) = max(tcj,td(id))
      vd(id) = max(vcj,vd(id))
   end do

   ! heat transfer between ceiling jet and ceiling is not calculated if the c.j. option is set to 2 in the file solver.ini
    
   if (cjetopt==2) return

   ! calculate average heat transfer fluxes to lower and upper walls along the vertical lines passing through the four wall/ceiling-jet stagnation points:
    
   do n = 1, 4
      rdh = rs(n)/h
      call sqfwst(rdh,h,c4,tht,htct,thtqhp,tw,qfwsl(n),qfwsu(n),zc,zlay)
   end do

   ! step 2. calculate radii at room corners:
   rc(1) = sqrt(xf**2+yf**2)
   rc(2) = sqrt((xw-xf)**2+yf**2)
   rc(3) = sqrt((xw-xf)**2+(yw-yf)**2)
   rc(4) = sqrt(xf**2+(yw-yf)**2)

   ! calculate average heat transfer fluxes to lower and upper walls along the vertical lines passing through the four room
   ! corners by assuming that the heat transfer there is as along a line passing through a point of normal ceiling-jet/wall impingement.
   
   do n = 1, 4
      rdh = rc(n)/h
      call sqfwst(rdh,h,c4,tht,htct,thtqhp,tw,qfwcl(n),qfwcu(n),zc,zlay)
   end do

    ! step 3. calculate the average heat transfer fluxes to the lower and upper portions of the eight wall segments bounded by the room
    ! corners and the the vertical lines passing through the points of normal wall/ceiling-jet impingement.
    
   qfwuan(1) = (qfwcu(1)+qfwsu(1))/2.0_eb
   qfwlan(1) = (qfwcl(1)+qfwsl(1))/2.0_eb
   qfwuan(2) = (qfwcu(2)+qfwsu(1))/2.0_eb
   qfwlan(2) = (qfwcl(2)+qfwsl(1))/2.0_eb
   qfwuan(3) = (qfwcu(2)+qfwsu(2))/2.0_eb
   qfwlan(3) = (qfwcl(2)+qfwsl(2))/2.0_eb
   qfwuan(4) = (qfwcu(3)+qfwsu(2))/2.0_eb
   qfwlan(4) = (qfwcl(3)+qfwsl(2))/2.0_eb
   qfwuan(5) = (qfwcu(3)+qfwsu(3))/2.0_eb
   qfwlan(5) = (qfwcl(3)+qfwsl(3))/2.0_eb
   qfwuan(6) = (qfwcu(4)+qfwsu(3))/2.0_eb
   qfwlan(6) = (qfwcl(4)+qfwsl(3))/2.0_eb
   qfwuan(7) = (qfwcu(4)+qfwsu(4))/2.0_eb
   qfwlan(7) = (qfwcl(4)+qfwsl(4))/2.0_eb
   qfwuan(8) = (qfwcu(1)+qfwsu(4))/2.0_eb
   qfwlan(8) = (qfwcl(1)+qfwsl(4))/2.0_eb

    ! step 4. for each of the upper layer segments use the area of the segment and the previously calculated average heat transfer
    ! flux to calculate the eight contributions to theto the total rate of upper-layer wall heat transfer.  sum these contributions 
    ! and obtain finally the average rate of heat transfer to the upper-layer portions of the walls.  carry out analogous
    ! calculations for the lower wall surfaces.  add rates of heat transfer to all 16 wall surface segments and obtain total rate of heat transfer to the wall.
    
   awl(1) = xf*zlay
   awu(1) = xf*(zc-zlay)
   awl(2) = (xw-xf)*zlay
   awu(2) = (xw-xf)*(zc-zlay)
   awl(3) = yf*zlay
   awu(3) = yf*(zc-zlay)
   awl(4) = (yw-yf)*zlay
   awu(4) = (yw-yf)*(zc-zlay)
   awl(5) = awl(2)
   awu(5) = awu(2)
   awl(6) = awl(1)
   awu(6) = awu(1)
   awl(7) = awl(4)
   awu(7) = awu(4)
   awl(8) = awl(3)
   awu(8) = awu(3)
   sumaql = 0.0_eb
   sumaqu = 0.0_eb
   sumal = 0.0_eb
   sumau = 0.0_eb
   do n = 1, 8
      sumaql = awl(n)*qfwlan(n) + sumaql
      sumaqu = awu(n)*qfwuan(n) + sumaqu
      sumal = awl(n) + sumal
      sumau = awu(n) + sumau
   end do

    ! turn off heat transfer to lower wall surfaces
    
   sumaql = 0.0_eb
   if (sumal<=0.0_eb) then
      qfwla = 0.0_eb
   else
      qfwla = sumaql/sumal
   endif
   if (sumau<=0.0_eb) then
      qfwua = 0.0_eb
   else
      qfwua = sumaqu/sumau
   endif

   return
   end subroutine ceilht

! --------------------------- int2d -------------------------------------------

   subroutine int2d(xc,yc,xrect,yrect,r,ans)
    !
    !     Routine:     INT2D
    !
    !     Source File: CEILHT.SOR
    !
    !     Functional Class:  
    !
    !     Description:  Integrates a function over a region formed by 
    !         intersecting a rectangle with dimension (xrect,yrect) 
    !         and a circle with center (xc,yc) and radius r.  
    !
    !     Arguments: XC
    !                YC
    !                XRECT
    !                YRECT
    !                R
    !                ANS

   use precision_parameters
   implicit none
   
   real(eb), intent(in) :: xc, yc, xrect, yrect, r
   real(eb), intent(out) :: ans
    
   real(eb) x1, x2, y1, y2, frint, ans1, ans2, ans3, ans4

   x1 = xrect - xc
   x2 = xc
   y1 = yrect - yc
   y2 = yc

   if (r<min(x1,x2,y1,y2)) then
      call inttabl(r,frint)
      ans = 2.0_eb*pi*frint
   else
      call intsq(x1,y1,r,ans1)
      call intsq(x2,y1,r,ans2)
      call intsq(x1,y2,r,ans3)
      call intsq(x2,y2,r,ans4)
      ans = ans1 + ans2 + ans3 + ans4
   endif
   return
   end subroutine int2d

! --------------------------- intsq -------------------------------------------

   subroutine intsq (s1,s2,r,ans)

    !     routine:     intsq

   use precision_parameters
   implicit none
   
   real(eb), intent(in) :: s1, s2, r
   real(eb), intent(out) :: ans
    
   real(eb) :: frint, ans1, ans2

   if (r<=min(s1,s2)) then
      call inttabl(r,frint)
      ans = pio2*frint
   else
      call inttri(s1,s2,r,ans1)
      call inttri(s2,s1,r,ans2)
      ans = ans1 + ans2
   endif
   return
   end subroutine intsq

! --------------------------- inttri -------------------------------------------

   subroutine inttri (x,y,r,ans)
    !
    !     routine:     inttri
    !

   use precision_parameters
   implicit none
   
   real(eb), intent(in) :: x, y, r
   real(eb), intent(out) :: ans
    
   integer :: n, j  
   real(eb) :: frint, frintu, diag, yl, thetal, thetau, xxn, dth, xxjm1, theatj, thetaj, rj, arj, theta


   if (abs(x)<1.e-5_eb.or.abs(y)<1.e-5_eb) then
      ans = 0.0_eb
      return
   endif
   theta = atan(y/x)
   if (r<x) then
      call inttabl(r,frint)
      ans = frint*theta
      return
   else
      diag = sqrt(x**2+y**2)
      if (r>diag) then
         yl = y
      else
         yl = sqrt(r**2-x**2)
      endif
      thetal = atan(yl/x)
      n = 1
      xxn = n
      dth = thetal/xxn
      ans = 0.0_eb
      do j = 1, n
         xxjm1 = j - 1
         thetaj = dth/2.0_eb + xxjm1*dth
         rj = x/cos(thetaj)
         call inttabl(rj,arj)
         ans = ans + arj
      end do
      ans = ans*dth
      thetau = theta - thetal
      call inttabl(r,frintu)
      ans = ans + thetau*frintu
   endif
   return
   end subroutine inttri

! --------------------------- maktabl -------------------------------------------

   subroutine maktabl (r,n,func)

    !     Routine:     MAKTABL

   use precision_parameters
   implicit none

   external func
   
   integer, intent(in) :: n
   real(eb), intent(in) :: r
   
   real(eb) :: fun(100), rmax, xxntabm1, dr, dr2, func, rr
   integer :: i
   
   integer :: ntab
   real(eb) :: tabl(100)
   common /trptabl/ tabl, rmax, ntab
   save /trptabl/

   ntab = n
   rmax = r
   xxntabm1 = ntab - 1
   dr = rmax/xxntabm1
   dr2 = dr/2.0_eb
   tabl(1) = 0.0_eb
   fun(1) = 0.0_eb
   do i = 2, ntab
      rr = real(i-1,eb)*dr
      fun(i) = rr*func(rr)
      tabl(i) = tabl(i-1) + (fun(i)+fun(i-1))*dr2
   end do
   return
   end subroutine maktabl

! --------------------------- inttabl -------------------------------------------

   subroutine inttabl (r,ans)

    !     Routine:     INTTABL

   use precision_parameters
   implicit none
   
   real(eb), intent(in) :: r
   real(eb), intent(out) :: ans

   real(eb) :: xxntabm1, dr, tab1, tab2, rr1, rr2
   integer :: ir
   
   integer :: ntab
   real(eb) :: tabl(100), rmax
   common /trptabl/ tabl, rmax, ntab
   save /trptabl/
   
   xxntabm1 = ntab - 1
   dr = rmax/xxntabm1 
   ir = 1.0_eb + r/dr
   if (ir<1) ir = 1
   if (ir>ntab-1) ir = ntab - 1
   tab1 = tabl(ir)
   tab2 = tabl(ir+1)
   rr1 = (real(ir,eb)-1.0_eb)*dr
   rr2 = real(ir,eb)*dr
   ans = (tab1*(rr2-r)+tab2*(r-rr1))/dr
   return
   end subroutine inttabl

! --------------------------- qfclg -------------------------------------------

   real(eb) function qfclg (r)

    !     Description: This function computes the convective heat transfer 
    !                  flux to the ceiling at location (X,Y)=(Z(1),Z(2)) 
    !
   use precision_parameters
   implicit none
   
   real(eb), intent(in) :: r

   real(eb) :: t0, t1, t2, t3, ff, htcldh, taddim, htcl, tad

   real(eb) :: rdh, h, htct, tht, thtqhp, c1, c2, c3, xf, yf, tc
   common /aintch/ h, htct, tht, thtqhp, c1, c2, c3, xf, yf, tc
   save /aintch/
   
   rdh = r/h
   t0 = rdh**(.8_eb)
   t1 = 1._eb - 1.1_eb*t0
   t2 = t0**2
   t3 = t0**3
   ff = (t1+0.808_eb*t2)/(t1+2.2_eb*t2+0.69_eb*t3)
   if (rdh<0.2_eb) then
      htcldh = c1*(1._eb-c2*rdh)
      taddim = 10.22_eb - 14.9_eb*rdh
   else
      htcldh = c3*(rdh-0.0771_eb)/((rdh+0.279_eb)*rdh**1.2_eb)
      taddim = 8.390913361_eb*ff
   endif
   htcl = htcldh*htct
   tad = taddim*thtqhp + tht
   qfclg = htcl*(tad-tc)
   return
   end function qfclg

! --------------------------- sqfwst -------------------------------------------

   subroutine sqfwst(rdh,h,c4,tht,htct,thtqhp,tw,qfwlow,qfwup,zc,zlay)
    !
    !     routine: sqfwst
    !   description: calculate average heat transfer fluxes to lower and upper walls along a vertical line passing through a 
    !                wall/ceiling-jet stagnation point

   use precision_parameters
   implicit none
   
   real(eb), intent(in) :: rdh, h, c4, tht, htct, thtqhp, tw
   real(eb), intent(out) :: qfwlow, qfwup

   real(eb) :: t1, t2, t3, f, taddim, htcl, tad, qfwst,  h8, h16, zc,  zlay 

   t1 = rdh**(.8_eb)
   t2 = t1*t1
   t3 = t2*t1
   f = (1._eb-1.1_eb*t1+0.808_eb*t2)/(1._eb-1.1_eb*t1+2.2_eb*t2+0.69_eb*t3)
   if (rdh<0.2_eb) then
      taddim = 10.22_eb - 14.9_eb*rdh
   else
      taddim = 8.39_eb*f
   endif
   htcl = c4*htct/rdh
   tad = taddim*thtqhp + tht
   qfwst = htcl*(tad-tw)
   h8 = .80_eb*h
   h16 = h8 + h8
   if (zc<=h8) then
      qfwlow = qfwst*(h16-(zc-zlay)-zc)/h16
      qfwup = qfwst*(1._eb-(zc-zlay)/h16)
    else
      if ((zc-zlay)>h8) then
         qfwlow = 0.0_eb
         qfwup = qfwst*(zc-h8)/(2.0_eb*(zc-zlay))
      else
         qfwlow = qfwst*(h8-(zc-zlay))/(2.0_eb*zlay)
         qfwup = qfwst*(1._eb-(zc-zlay)/h16)
     endif
   endif
   return
   end subroutine sqfwst

! --------------------------- cjet -------------------------------------------

   subroutine cjet (flwcjt,flxcjt)

    !     routine:     cjet

    !     description:  interface between resid and ceilht.  loops over
    !                 rooms setting up varibles to pass.  calls ceilht
    !                 only when fires are in a room otherwise sets zeros
    !                 for flxcjt.  then uses flxcjt to figure flwcjt.
    !

   use precision_parameters
   use cenviro
   use cfast_main
   use opt
   use wdervs

   implicit none
   
   real(eb), intent(out) :: flwcjt(nr,2), flxcjt(nr,nwal)

   real(eb) :: dummy(100), zloc, tceil, tuwall, qceil, qfclga, qfwla, qfwua, ftmax, fvmax, fdmax
   integer :: cjetopt, i, id, iroom, nrmfire, nd, ifire, ifpnt, iwall, ilay

   flxcjt(1:nm1,1:4) = 0.0_eb
   flwcjt(1:nm1,1:2) = 0.0_eb
   
   do id = 1, ndtect
      iroom = ixdtect(id,droom)
      xdtect(id,dvel) = 0.0_eb
      zloc = xdtect(id,dzloc)
      if (zloc>zzhlay(iroom,lower)) then
         xdtect(id,dtjet) = zztemp(iroom,upper)
      else
         xdtect(id,dtjet) = zztemp(iroom,lower)
      endif
   end do
   if (option(fcjet)==off) return
   cjetopt = option(fcjet)

   do i = 1, nm1
      nrmfire = ifrpnt(i,1)
      id = idtpnt(i,2)
      nd = idtpnt(i,1)

        ! handle ceiling jets that are not in active halls

      if (cjeton(nwal+1).and.nrmfire>0.and.izhall(i,ihmode)/=ihduring) then
         do ifire = 1, nrmfire
            ifpnt = ifrpnt(i,2) + ifire - 1
               if (switch(1,i)) then
                  tceil = twj(1,i,1)
               else
                  tceil = zztemp(i,upper)
               endif
               if (switch(3,i)) then
                  tuwall = twj(1,i,3)
               else
                  tuwall = zztemp(i,upper)
               endif
               call ceilht(xfire(ifpnt,4),xfire(ifpnt,7),tceil,zztemp(i,lower),zztemp(i,upper),tuwall,br(i),dr(i), &
               hr(i),xfire(ifpnt,1),xfire(ifpnt,2),xfire(ifpnt,3),zzhlay(i,lower),zzrho(i,lower),zzrho(i,upper),cjetopt, &
               xdtect(id,dxloc),xdtect(id,dyloc),xdtect(id,dzloc),nd,qceil,qfclga,qfwla,qfwua,xdtect(id,dtjet),xdtect(id,dvel),ftmax,fvmax,fdmax)
               flxcjt(i,1) = flxcjt(i,1) + qfclga
               flxcjt(i,3) = flxcjt(i,3) + qfwua
               flxcjt(i,4) = flxcjt(i,4) + qfwla
            end do
         endif

        ! handle ceiling jets that are in active halls
        
         if (izhall(i,ihmode)==ihduring)call hallht(i,id,nd)

         do iwall = 1, 4
            if (mod(iwall,2)==1) then
               ilay = upper
            else
               ilay = lower
            endif

           ! if (.not.(ceiling jet in fire room)) then flux to iwall = 0.

            if (.not.(switch(iwall,i).and.cjeton(iwall).and.nrmfire>0)) then
               flxcjt(i,iwall) = 0.0_eb
            endif
            flwcjt(i,ilay) = flwcjt(i,ilay) - zzwarea(i,iwall)*flxcjt(i,iwall)
         end do
      end do
      return
     end subroutine cjet

! --------------------------- rev_convection -------------------------------------------

   integer function rev_convection ()

   integer :: module_rev
   character(255) :: module_date 
   character(255), parameter :: mainrev='$Revision$'
   character(255), parameter :: maindate='$Date$'

   write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
   read (module_date,'(i5)') module_rev
   rev_convection = module_rev
   write(module_date,'(a)') maindate
   return
   end function rev_convection