      subroutine convec(iw,tg,tw,qdinl)

!     routine: convec
!     purpose: calculate convective heat transfer for a wall segment. note that we have simplified the convection calculation
!              by assuming turbulent flow.  this allows us to remove the dependency on the characterisitic length and avoid a divide
!              by zero as the surface vanishes.  if a more general relationship is desired, the code will have to be reworked
!              to include the characteristic length in the calculation.
!     arguments:  iw     wall number, standand cfast numbering convention
!                 tg     temperature of gas layer adjacent to wall surface
!                 tw     wall surface temperature
!                 qdinl  convective flux into wall surface iw

      include "precis.fi"
      real*8 nuoverl, k
      logical first
      save first, g, x1del, xthird, tdel, xxhalf
      data first /.true./

      if (first) then
          first = .false.
          g = 9.80d0
          tdel = 5.0d0
          x1del = 1.0d0 / tdel
          xthird = 1.0d0 / 3.0d0
          xxhalf = 0.50d0
      endif

      qdinl = 0.0d0
      tf = (tw+tg) * xxhalf

      ! to keep k positive, make sure tf is below 3000.  of course the calculation will have long since lost any semblance to reality.
      t3000 = 3000.0d0
      tff = min(tf,t3000)
      if (tf<=0.0d0) return
      alpha = 1.d-9 * tf ** (1.75d0)
      k = (0.0209d0+2.33d-5*tff) / (1.d0-2.67d-4*tff)
      pr = 0.72d0

      ! ceilings and floors
      ! use the hyperbolic tangent to smooth the coefficient c from cup to cdown over a temperature range of tdel degress. 
      ! note: tanh(x>>0)=1 and tanh(x<<0)=-1
      cup = 0.16d0
      cdown = 0.13d0
      if (iw==1) then
          c = (cup+cdown+(cup-cdown)*tanh((tg-tw)*x1del)) * xxhalf
      else if (iw==2) then
          c = (cup+cdown-(cup-cdown)*tanh((tg-tw)*x1del)) * xxhalf

          ! vertical surfaces
      else
          c = 0.121d0
      endif

      ! prevent the vertical tangent in the calculation of nuoverl by keeping abstwtg from going to zero.  
      abstwtg = abs(tw-tg)
      if (abstwtg<tdel) abstwtg = tdel
      nuoverl = c * (g*abstwtg*pr/(tf*alpha**2)) ** xthird
      qdinl = nuoverl * k * (tg-tw)
      return
      end

      subroutine cvheat(flwcv,flxcv)
c*rb
c
c     routine:    cfcnvc
c     function:   interface between resid and convec.  loops over rooms
c                 setting up varibles.  passes to convec if ceiling jet for
c                 a surface is off, otherwise sets flxcv to 0.0 and then
c                 solves for flwcv
c     outputs:    flwcv       net enthalphy into each layer 
c                 flxcv       net heat flux onto surface
c     revision history:
c     par   11/??/91 created.
c     gpf   2/5/93  added partial derivative calculations for use
c                   with the reduced jacobian option
c     gpf   7/13/95
c               reduced the number of convection calculations performed during
c               a jacobian calculation (option(fmodjac)==2)
c     gpf 2/5/96   removed reduced jacobian option added on 2/5/93.
c*re

      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "wdervs.fi"
      include "opt.fi"
      include "wnodes.fi"

      dimension flwcv(nr,2), flxcv(nr,nwal)
      dimension flwcv0(nr,2), flxcv0(nr,nwal)
      integer cjetopt
      logical roomflg(nr), wallflg(4*nr)
      save flwcv0, flxcv0

      xx0 = 0.0d0
      do i = 1, nm1
          flwcv(i,upper) = xx0
          flwcv(i,lower) = xx0
          do j = 1, nwal
              flxcv(i,j) = xx0
          end do
      end do
      if (option(fconvec)/=on) return

      cjetopt = option(fcjet)

      do i = 1, nm1
          roomflg(i) = .true.
      end do
      do i = 1, nwalls
          wallflg(i) = .true.
      end do

      if(option(fmodjac)==on)then
          if(jaccol>0)then

              ! if 2nd modified jacobian is active and dassl is computing a jacobian then
              ! only compute convection heat transfer in the room where the dassl 
              ! solution variable has been perturbed
              do i = 1, nm1
                  roomflg(i) = .false.
              end do
              do i = 1, nwalls
                  wallflg(i) = .false.
              end do

              ieqtyp = izeqmap(jaccol,1)
              iroom = izeqmap(jaccol,2)
              if(ieqtyp==eqtu.or.ieqtyp==eqvu.or.ieqtyp==eqtl.or.
     .        ieqtyp==eqwt)then
                  if(ieqtyp==eqwt)iroom = izwall(iroom,1)
                  do iwall = 1, 4
                      roomflg(iroom) = .true.
                      if(switch(iwall,iroom))then
                          iw = izwmap2(iwall,iroom) - nofwt
                          wallflg(iw) = .true.
                      endif
                  end do
              endif
          endif
      endif

      do iw = 1, nwalls
          if(wallflg(iw)) then
              i = izwall(iw,1)
              iwall = izwall(iw,2)
              nrmfire = ifrpnt(i,1)
              if(mod(iwall,2)==1)then
                  ilay = upper
              else
                  ilay = lower
              endif

              ! ceiling jet heat transfer is not active if cjetopt=2.  use normal (call convec) instead
              if (cjetopt/=2.and.cjeton(iwall).and.nrmfire/=0) then
                  flxcv(i,iwall) = xx0
              else
                  call convec(iwall,zztemp(i,ilay),zzwtemp(i,iwall,1),
     .            flxcv(i,iwall))
              endif
              flwcv(i,ilay) = flwcv(i,ilay) - 
     .        zzwarea(i,iwall)*flxcv(i,iwall)
          endif
      end do

      if (option(fmodjac)==on) then
          if (jaccol==0) then

              ! save the flux and flow vectors when we are about to compute a jacobian
              do iroom = 1, nm1
                  flwcv0(iroom,1) = flwcv(iroom,1)
                  flwcv0(iroom,2) = flwcv(iroom,2)
                  do iwall = 1, 4
                      flxcv0(iroom,iwall) = flxcv(iroom,iwall)
                  end do
              end do
          elseif (jaccol>0) then

              ! we are computing the jaccol'th column of the jacobian.  if the solution hasn't changed then get it from the vectors saved above.
              do iroom = 1, nm1
                  if(.not.roomflg(iroom)) then
                      flwcv(iroom,1) = flwcv0(iroom,1)
                      flwcv(iroom,2) = flwcv0(iroom,2)
                      do iwall = 1, 4
                          flxcv(iroom,iwall) = flxcv0(iroom,iwall)
                      end do
                  endif
              end do
          endif
      endif      
      return
      end

      subroutine ceilht(mplume,qconv,atc,tl,tu,tw,xw,yw,zc,axf,ayf,zf,
     +zlay,rhol,rhou,cjetopt,xd,yd,zd,nd,qceil,qfclga,
     +qfwla,qfwua,td,vd,tdmax,vdmax,ddmax)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     CEILHT
C
C     Functional Class:  
C
C     Description:  
C       THIS SUBROUTINE CALCULATES CONVECTIVE HEAT TRANSFER TO
C	THE UNIFORM TEMPERATURE CEILING ABOVE A FIRE IN A PARALLEL-
C	OPIPED ROOM WITH A TWO-LAYER FIRE ENVIRONMENT.  ALSO CALCU-
C	LATED IS THE TOTAL RATE OF HEAT TRANSFER TO THE WALLS AND
C	THE AVERAGE FLUX TO THE UPPER AND LOWER PORTIONS OF THE
C	WALLS.

C   	INPUT
C
C	MPLUME		MASS FLOW RATE IN THE PLUME AT ZLAY IF ZF < ZLAY [KG/S]
C         ND    NUMBER OF DETECTORS
C	 QCONV		PORTION OF FIRE ENERGY RELEASE RATE CONVECTED IN PLUME [W]
C	    TC    AVERAGE TEMPERATURE OF CEILING [K]
C	 TL,TU    TEMPERATURE OF LOWER, UPPER LAYER [K]
C	    TW    AVERAGE TEMPERATURE OF WALL SURFACES [K]
C   XD,YD,ZD    X, Y, AND Z POSITIONS OF THE DETECTORS
C   XW,YW,ZC    CO-ORDINATES OF ROOM CORNER DIAGONALLY OPPOSITE TO ORIGIN 
C               OF AXES (ORIGIN IS AT A CORNER ON THE FLOOR, WITH X, Y 
C               AXES ALONG WALL/FLOOR JUNCTION AND Z AXES UPWARD) [M]
C   XF,YF,ZF    CO-ORDINATES OF CENTER OF BASE OF FIRE [M]
C       ZLAY    ELEVATION ABOVE FLOOR OF LAYER INTERFACE [M]
C  RHOL,RHOU    DENSITY OF LOWER, UPPER LAYER [KG/M**3]
C    CJETOPT    HEAT TRANSFER IS NOT CALCULATED IF CJETOPT=2
C
C	OUTPUT
C	-----
C
C      QCEIL    RATE OF HEAT TRANSFER TO CEILING [W]
C     QFCLGA		AVERAGE FLUX OF HEAT TRANSFER TO CEILING [W/M**2]
C QFWLA,QFWUA   AVERAGE FLUX OF HEAT TRANSFER TO LOWER AND
C               UPPER PORTIONS OF THE WALL SUFACES [W/M**2]
C         TD    TEMPERATURE OF JET AT XD,YD,ZD LOCATIONS
C         VD    VELOCITY OF JET AT XD,YD,ZD LOCATIONS
C
C***	SOME OTHER DEFINITIONS AND FIXED INPUT IN THIS SUBROUTINE

C      ALPHA    TU/TL
C     AWL(N)    AREA OF LOWER-LAYER PORTION OF WALL SEGMENT N
C     AWU(N)    AREA OF UPPER-LAYER PORTION OF WALL SEGMENT N
C         CP    SPECIFIC HEAT OF AIR AT CONSTANT PRESSURE [KJ/(KG*K)]
C         CT    9.115, CONSTANT IN POINT SOURCE PLUME EQN.
C          G    9.8, ACCELERATION OF GRAVITY [M/S**2]
C          H    IF LAYER INTERFACE IS BELOW CEILING: DISTANCE
C               OF CONTINUATION SOURCE BELOW CEILING; IF LAYER INTERFACE 
C               IS AT CEILING: DISTANCE OF EQUIVALENT SOURCE BELOW 
C               CEILING [M]
C      QCONT    STRENGTH OF CONTINUATION SOURCE [W]
C        QEQ    DIMENSIONLESS STRENGTH OF PLUME AT ZLAY
C         QH    DIMENSIONLESS STRENGTH OF PLUME AT ZC
C   QFWCL(N)    AVERAGE HEAT FLUX TO WALL NEAR CORNER N IN THE 
C               LOWER LAYER [W/M**2]
C   QFWCU(N)    AVERAGE HEAT FLUX TO WALL NEAR CORNER N IN
C               THE UPPER LAYER [W/M**2]
C  QFWLAN(N)    AVERAGE HEAT FLUX TO PORTION OF WALL SEGMENT
C               N IN THE LOWER LAYER [W/M**2]
C  QFWUAN(N)    AVERAGE HEAT FLUX TO PORTION OF WALL SEGMENT
C               N IN THE UPPER LAYER [W/M**2]
C   QFWSL(N)    AVERAGE HEAT FLUX TO PORTION OF WALL IN THE LOWER LAYER 
C               ALONG THE LINE PASSING THROUGH STAGNATION POINT N [W/M**2]
C   QFWSU(N)    AVERAGE HEAT FLUX TO PORTION OF WALL IN THE UPPER LAYER 
C               ALONG THE LINE PASSING THROUGH STAGNATION POINT N [W/M**2]
C      QFWST    HEAT TRANSFER FLUX TO WALL AT A WALL/CEILING-JET STAGNATION 
C               POINT [W/M**2]
C         PR    PRANDTL NUMBER
C      RC(N)    DISTANCE FROM PLUME/CEILING IMPINGEMENT POINT TO CORNER N [M]
C      RS(N)    DISTANCE FROM PLUME/CEILING IMPINGEMENT POINT TO WALL 
C               STAGNATION POINT N [M]
C        THT    TEMPERATURE OF AMBIENT IN UNCONFINED CEILING HEAT TRANSFER 
C               PROBLEM, EITHER TL OR TU [K] 
C        ZEQ    ELEVATION ABOVE FLOOR OF EQUIVALENT SOURCE IN LOWER LAYER [M]
C         ZS    ELEVATION ABOVE FLOOR OF CONTINUATION SOURCE [M]
C       TDMAX   MAXIMUM TEMPERATURE OF THE CEILING JET
C       VDMAX   MAXIMUM VELOCITY OF THE CEILING JET
C       DDMAX   ESTIMATE OF CEILING JET DEPTH AT R/H = .2
C               (GIVEN BY:  DDMAX/(.23*DELTA) = 2)
C       
C
C     Revision History:
C     GPF 10/14/93   CALCULATED VELOCITY AND TEMPERATURES AT DETECTOR
C                    LOCATIONS. NOTE, WHEN THERE IS MORE THAN ONE FIRE
C                    IN A ROOM WE SAVE THE 'WORST CASE', IE WE STORE
C                    THE CEILING JET VELOCITY AND TEMPERATURE FROM
C                    THE FIRE THAT CAUSES THESE VALUES TO BE THE LARGEST.
C                    ADDED OPTION FLAG, CJETOPT.  
C
C                    HEAT TRANSFER BETWEEN CEILING JET AND WALLS IS 
C                    NOT CALCULATED IF IF CJETOPT=2.
C     GPF 3/29/95    Disabled heat transfer to lower wall surfaces.  Previously,
C                    heat from the ceiling jet was added to the lower walls but
C                    not to the lower layer.  This inconsistency resulted in an
C                    unrealistic drop in lower layer temperatures.
C     GPF 4/24/95    removed references to qwall, since it was not
C                    being used
C     GPF 7/22/96    added maximum temperature, velocity and depth for hybrid option
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      implicit real*8 (a-h,m,o-z)
C 
C********************************************************************
C   	This subroutine and the function QFCLG used in its called subroutines use the common blocks AINTCH
C********************************************************************
C
      external qfclg
      integer cjetopt
      dimension awl(8), awu(8), qfwcl(4), qfwcu(4), qfwlan(8), 
     +qfwuan(8), qfwsl(4), qfwsu(4), rc(4), rs(4)
      dimension xd(*), yd(*), zd(*), td(*), vd(*)
      common /aintch/ h, htct, tht, thtqhp, c1, c2, c3, xf, yf, tc
      save /aintch/
      logical first
      save first, pi, g, ct, cp, pr, gsqrt, x2d3, x1d3, rk1
      data first /.true./

      xx0 = 0.0d0
      qceil = xx0
      qfclga = xx0
      qfwla = xx0
      qfwua = xx0
      if (mplume==xx0.or.qconv==xx0) then
          do id = 1, nd
              td(id) = max(tu,td(id))
              vd(id) = max(xx0,vd(id))
          end do
          return
      endif

      if (first) then
          first = .false.
          one = 1.0d0
          pi = 4.0d0 * atan(one)
          g = 9.80d0
          ct = 9.115d0
          cp = 1000.0d0
          pr = .70d0
          gsqrt = sqrt(g)
          x2d3 = 2.0d0 / 3.0d0
          x1d3 = 1.0d0 / 3.0d0
          two = 2.0d0
          rk1 = (.23d0/.77d0) * log(sqrt(two)-one)
      endif
      xf = axf
      yf = ayf
      rfmin = .20d0 * (zc-zf)
      if (rfmin<xw/2.0d0) then
          if (xf<rfmin) xf = rfmin
          if (xf>xw-rfmin) xf = xw - rfmin
      else
          xf = xw / 2.0d0
      endif
      if (rfmin<yw/2.0d0) then
          if (yf<rfmin) yf = rfmin
          if (yf>yw-rfmin) yf = yw - rfmin
      else
          yf = yw / 2.0d0
      endif
      tc = atc
      alpha = tu / tl
      if (zf<zlay) then

          ! fire is below layer interface
          qeq = (0.21d0*qconv/(cp*tl*mplume)) ** 1.5d0
          zeq = zlay - (qconv/(qeq*rhol*cp*tl*gsqrt)) ** (.4d0)
          if (zlay<zc) then

              ! layer interface is below ceiling
              alfm1 = alpha - 1.0d0
              if (alfm1/=xx0) then
                  sigma = -1.0d0 + ct * qeq ** x2d3 / alfm1
                  a1 = sigma / (sigma+1.0d0)
                  if (sigma>xx0) then
                      ssq = sigma ** 2
                      top = 1.04599d0 * sigma + 0.360391d0 * ssq
                      bottom = 1.d0 + 1.37748d0 * sigma + 0.360391d0*ssq
                      mfrac = top / bottom
                  else
                      mfrac = xx0
                      qceil = xx0
                      qfclga = xx0
                      qfwla = xx0
                      qfwua = xx0
                      do id = 1, nd
                          td(id) = max(tu,td(id))
                          vd(id) = max(xx0,vd(id))
                      end do
                      return
                  endif
              else
                  a1 = 1.0d0
                  mfrac = 1.0d0
              endif
              qcont = qconv * a1 * mfrac
              zs = zlay - (zlay-zeq) * alpha**.6d0 * mfrac**.4d0 / a1 **
     +        .2d0
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
              qceil = xx0
              qfclga = xx0
              qfwla = xx0
              qfwua = xx0
              return
          endif
      endif
      h = zc - zs
      sqrtgh = sqrt(g*h)
      qh = qcont / (rhoht*cp*tht*sqrtgh*h**2)
      qhp = (qh**x1d3)
      htct = rhoht * cp * sqrtgh * qhp
      anu = (0.04128d-7*tht**2.5d0) / (tht+110.4d0)
      re = sqrtgh * h * qhp / anu
      thtqhp = tht * qhp ** 2
      prp = pr ** x2d3
      c1 = 8.82d0 / (sqrt(re)*prp)
      c2 = 5.d0 - 0.284d0 * re ** 0.2d0
      c3 = 0.283032655d0 / (re**0.3d0*prp)
      c4 = 0.94d0 / ((re**0.42d0)*pr)

      rmax = sqrt(max(yf,yw-yf)**2+max(xf,xw-xf)**2)

      ! make an integral table of size ntab from zero to rmax.
      ntab = 20
      call maktabl(rmax,ntab,qfclg)

      ! don't need to compute the following if we aren't computing ceiling jet heat transfer
      if (cjetopt/=2) then
          call int2d(xf,yf,xw,yw,rmax,qceil)
          qfclga = qceil / (xw*yw)
      endif

      ! now calculate wall heat transfer: ***	step 1. calculate radii at wall stagnation points
      rs(1) = yf
      rs(2) = xw - xf
      rs(3) = yw - yf
      rs(4) = xf

      !calculate velocity and temperatures of the ceiling jet at various locations
      do id = 1, nd
          rd = sqrt((axf-xd(id))**2+(ayf-yd(id))**2)
          rdh = rd / h
          if (rdh<=.20d0) then
              rdh = .20d0
              rd = rdh * h
          endif
          v = sqrtgh * qhp
          vmax = .85d0 * v / rdh ** 1.1d0
          vdmax = vmax
          delta = .1d0 * h * rdh ** .9d0
          dz = zc - zd(id)
          zdel = dz / (.23d0*delta)
          ddmax = 2.0d0*0.23d0*delta
          if (zdel<=1.0d0) then
              vcj = vmax * zdel ** (1.0d0/7.0d0) * (8.0d0-zdel) / 7.0d0
          else
              arg = rk1 * (zdel-1.0d0)
              vcj = vmax / cosh(arg) ** 2
          endif

          call inttabl(rd,rlamr)
          rlamr = 2.0d0 * pi * rlamr / qconv
          tmaxmtu = 2.6d0 * (1.0d0-rlamr) * qh ** x2d3 * tu / rdh**.80d0
     +    - .90d0 * (tc-tu)
          ths = (tc-tu) / tmaxmtu
          if (zdel<=1.0d0) then
              thta = ths + 2.0d0 * (1.0d0-ths) * zdel - (1.0d0-ths)*zdel
     +        ** 2
          else
              thta = vcj / vmax
          endif
          tcj = tu + thta * tmaxmtu
          tdmax = tu + tmaxmtu
          td(id) = max(tcj,td(id))
          vd(id) = max(vcj,vd(id))
      end do

      ! heat transfer between ceiling jet and ceiling is not calculated if the c.j. option is set to 2 in the file solver.ini
      if (cjetopt==2) return

      ! calculate average heat transfer fluxes to lower and upper walls along the vertical lines passing through the four wall/ceiling-jet stagnation points:
      do n = 1, 4
          rdh = rs(n) / h
          call sqfwst(rdh,h,c4,tht,htct,thtqhp,tw,qfwsl(n),qfwsu(n),zc,
     +    zlay)
      end do

      ! step 2. calculate radii at room corners:
      rc(1) = sqrt(xf**2+yf**2)
      rc(2) = sqrt((xw-xf)**2+yf**2)
      rc(3) = sqrt((xw-xf)**2+(yw-yf)**2)
      rc(4) = sqrt(xf**2+(yw-yf)**2)

      ! calculate average heat transfer fluxes to lower and upper walls along the vertical lines passing through the four room
      ! corners by assuming that the heat transfer there is as along a line passing through a point of normal ceiling-jet/wall impingement.
      do n = 1, 4
          rdh = rc(n) / h
          call sqfwst(rdh,h,c4,tht,htct,thtqhp,tw,qfwcl(n),qfwcu(n),zc,
     +    zlay)
      end do

      ! step 3. calculate the average heat transfer fluxes to the lower and upper portions of the eight wall segments bounded by the room
      ! corners and the the vertical lines passing through the points of normal wall/ceiling-jet impingement.
      qfwuan(1) = (qfwcu(1)+qfwsu(1)) * .5d0
      qfwlan(1) = (qfwcl(1)+qfwsl(1)) * .5d0
      qfwuan(2) = (qfwcu(2)+qfwsu(1)) * .5d0
      qfwlan(2) = (qfwcl(2)+qfwsl(1)) * .5d0
      qfwuan(3) = (qfwcu(2)+qfwsu(2)) * .5d0
      qfwlan(3) = (qfwcl(2)+qfwsl(2)) * .5d0
      qfwuan(4) = (qfwcu(3)+qfwsu(2)) * .5d0
      qfwlan(4) = (qfwcl(3)+qfwsl(2)) * .5d0
      qfwuan(5) = (qfwcu(3)+qfwsu(3)) * .5d0
      qfwlan(5) = (qfwcl(3)+qfwsl(3)) * .5d0
      qfwuan(6) = (qfwcu(4)+qfwsu(3)) * .5d0
      qfwlan(6) = (qfwcl(4)+qfwsl(3)) * .5d0
      qfwuan(7) = (qfwcu(4)+qfwsu(4)) * .5d0
      qfwlan(7) = (qfwcl(4)+qfwsl(4)) * .5d0
      qfwuan(8) = (qfwcu(1)+qfwsu(4)) * .5d0
      qfwlan(8) = (qfwcl(1)+qfwsl(4)) * .5d0

      ! Step 4. For each of the upper layer segments use the area of the segment and the previously calculated average heat transfer
      ! flux to calculate the eight contributions to theto the total rate of upper-layer wall heat transfer.  Sum these contributions 
      ! and obtain finally the average rate of heat transfer to the upper-layer portions of the walls.  Carry out analogous
      ! calculations for the lower wall surfaces.  add rates of heat transfer to all 16 wall surface segments and obtain total rate of heat transfer to the wall.
      awl(1) = xf * zlay
      awu(1) = xf * (zc-zlay)
      awl(2) = (xw-xf) * zlay
      awu(2) = (xw-xf) * (zc-zlay)
      awl(3) = yf * zlay
      awu(3) = yf * (zc-zlay)
      awl(4) = (yw-yf) * zlay
      awu(4) = (yw-yf) * (zc-zlay)
      awl(5) = awl(2)
      awu(5) = awu(2)
      awl(6) = awl(1)
      awu(6) = awu(1)
      awl(7) = awl(4)
      awu(7) = awu(4)
      awl(8) = awl(3)
      awu(8) = awu(3)
      sumaql = xx0
      sumaqu = xx0
      sumal = xx0
      sumau = xx0
      do n = 1, 8
          sumaql = awl(n) * qfwlan(n) + sumaql
          sumaqu = awu(n) * qfwuan(n) + sumaqu
          sumal = awl(n) + sumal
          sumau = awu(n) + sumau
      end do

      ! turn off heat transfer to lower wall surfaces
      sumaql = xx0
      if (sumal<=xx0) then
          qfwla = xx0
      else
          qfwla = sumaql / sumal
      endif
      if (sumau<=xx0) then
          qfwua = xx0
      else
          qfwua = sumaqu / sumau
      endif

      return
      end

      subroutine int2d(xc,yc,xrect,yrect,r,ans)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INT2D
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  Integrates a function over a region formed by 
C         intersecting a rectangle with dimension (xrect,yrect) 
C         and a circle with center (xc,yc) and radius r.  
C
C     Arguments: XC
C                YC
C                XRECT
C                YRECT
C                R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      logical first
      save pi
      data first /.true./

      if (first) then
          first = .false.
          one = 1.0d0
          pi = 4.0d0 * atan(one)
      endif

      x1 = xrect - xc
      x2 = xc
      y1 = yrect - yc
      y2 = yc

      if (r<min(x1,x2,y1,y2)) then
          call inttabl(r,frint)
          ans = 2.0d0 * pi * frint
      else
          call intsq(x1,y1,r,ans1)
          call intsq(x2,y1,r,ans2)
          call intsq(x1,y2,r,ans3)
          call intsq(x2,y2,r,ans4)
          ans = ans1 + ans2 + ans3 + ans4
      endif
      return
      end subroutine int2d
      
      SUBROUTINE INTSQ(S1,S2,R,ANS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INTSQ
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: S1
C                S2
C                R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      LOGICAL FIRST
      SAVE FIRST, PI
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
          FIRST = .FALSE.
          ONE = 1.0D0
          PI = 4.0D0 * ATAN(ONE)
      endif
C
      IF (R<=MIN(S1,S2)) THEN
          CALL INTTABL(R,FRINT)
          ANS = PI * FRINT / 2.0D0
      ELSE
          CALL INTTRI(S1,S2,R,ANS1)
          CALL INTTRI(S2,S1,R,ANS2)
          ANS = ANS1 + ANS2
      endif
      RETURN
      END
      SUBROUTINE INTTRI(X,Y,R,ANS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INTTRI
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: X
C                Y
C                R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
C
      XX0 = 0.0D0
      IF (ABS(X)<1.D-5.OR.ABS(Y)<1.D-5) THEN
          ANS = XX0
          RETURN
      endif
      THETA = ATAN(Y/X)
      IF (R<X) THEN
          CALL INTTABL(R,FRINT)
          ANS = FRINT * THETA
          RETURN
      ELSE
          DIAG = SQRT(X**2+Y**2)
          IF (R>DIAG) THEN
              YL = Y
          ELSE
              YL = SQRT(R**2-X**2)
          endif
          THETAL = ATAN(YL/X)
          N = 1
          XXN = N
          DTH = THETAL / XXN
          ANS = XX0
          DO 10 J = 1, N
              XXJM1 = J - 1
              THETAJ = DTH / 2.0D0 + XXJM1 * DTH
              RJ = X / COS(THETAJ)
              CALL INTTABL(RJ,ARJ)
              ANS = ANS + ARJ
   10     CONTINUE
          ANS = ANS * DTH
          THETAU = THETA - THETAL
          CALL INTTABL(R,FRINTU)
          ANS = ANS + THETAU * FRINTU
      endif
      RETURN
      END
      SUBROUTINE MAKTABL(R,N,FUNC)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     MAKTABL
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: R
C                N
C                FUNC
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"

      EXTERNAL FUNC
      DIMENSION TABL(100), FUN(100)
      COMMON /TRPTABL/ TABL, RMAX, NTAB
      SAVE /TRPTABL/

      XX0 = 0.0D0
      NTAB = N
      RMAX = R
      XXNTABM1 = NTAB - 1
      DR = RMAX / XXNTABM1
      DR2 = DR / 2.0D0
      TABL(1) = XX0
      FUN(1) = XX0
      DO 10 I = 2, NTAB
          XXIM1 = I - 1
          RR = XXIM1 * DR
          FUN(I) = RR * FUNC(RR)
          TABL(I) = TABL(I-1) + (FUN(I)+FUN(I-1)) * DR2
   10 CONTINUE
      RETURN
      END
      SUBROUTINE INTTABL(R,ANS)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INTTABL
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: R
C                ANS
C
C     Revision History:
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
      DIMENSION TABL(100)
      COMMON /TRPTABL/ TABL, RMAX, NTAB
      SAVE /TRPTABL/
      XXNTABM1 = NTAB - 1
      DR = RMAX / XXNTABM1 
      IR = 1.0D0 + R / DR
      IF (IR<1) IR = 1
      IF (IR>NTAB-1) IR = NTAB - 1
      TAB1 = TABL(IR)
      TAB2 = TABL(IR+1)
      XXIR = IR
      RR1 = (XXIR-1.0D0) * DR
      RR2 = XXIR * DR
      ANS = (TAB1*(RR2-R)+TAB2*(R-RR1)) / DR
      RETURN
      END
      real*8 FUNCTION QFCLG(R)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     QFCLG
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description: This function computes the convective heat transfer 
C                  flux to the ceiling at location (X,Y)=(Z(1),Z(2)) 
C
C     Arguments: R
C
C     Revision History:
C        Created:  5/5/1995 at 14:18 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      COMMON /AINTCH/ H, HTCT, THT, THTQHP, C1, C2, C3, XF, YF, TC
      SAVE /AINTCH/
      RDH = R / H
      T0 = RDH ** (.8D0)
      T1 = 1.D0 - 1.1D0 * T0
      T2 = T0 ** 2
      T3 = T0 ** 3
      FF = (T1+0.808D0*T2) / (T1+2.2D0*T2+0.69D0*T3)
      IF (RDH<0.2D0) THEN
          HTCLDH = C1 * (1.D0-C2*RDH)
          TADDIM = 10.22D0 - 14.9D0 * RDH
      ELSE
          HTCLDH = C3 * (RDH-0.0771D0) / ((RDH+0.279D0)*RDH**1.2D0)
          TADDIM = 8.390913361D0 * FF
      endif
      HTCL = HTCLDH * HTCT
      TAD = TADDIM * THTQHP + THT
      QFCLG = HTCL * (TAD-TC)
      RETURN
      END
      SUBROUTINE SQFWST(RDH,H,C4,THT,HTCT,THTQHP,TW,QFWLOW,QFWUP,ZC,ZLAY
     +)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     SQFWST
C
C     Source File: CEILHT.SOR
C
C     Functional Class:  
C
C     Description:  Calculate average heat transfer fluxes to lower and 
C            upper walls along a vertical line passing through a 
C            wall/ceiling-jet stagnation point
C
C
C     Arguments: RDH
C                H
C                C4
C                THT
C                HTCT
C                THTQHP
C                TW
C                QFWLOW
C                QFWUP
C                ZC
C                ZLAY 
C
C     Revision History:
C        Created:  5/5/1995 at 14:18 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      T1 = RDH ** (.8D0)
      T2 = T1 * T1
      T3 = T2 * T1
      F = (1.D0-1.1D0*T1+0.808D0*T2) / (1.D0-1.1D0*T1+2.2D0*T2+0.69D0*T3
     +)
      IF (RDH<0.2D0) THEN
          TADDIM = 10.22D0 - 14.9D0 * RDH
      ELSE
          TADDIM = 8.39D0 * F
      endif
      HTCL = C4 * HTCT / RDH
      TAD = TADDIM * THTQHP + THT
      QFWST = HTCL * (TAD-TW)
      H8 = .80D0 * H
      H16 = H8 + H8
      IF (ZC<=H8) THEN
          QFWLOW = QFWST * (H16-(ZC-ZLAY)-ZC) / H16
          QFWUP = QFWST * (1.D0-(ZC-ZLAY)/H16)
      ELSE
          IF ((ZC-ZLAY)>H8) THEN
              QFWLOW = 0.0D0
              QFWUP = QFWST * (ZC-H8) / (2.0D0*(ZC-ZLAY))
          ELSE
              QFWLOW = QFWST * (H8-(ZC-ZLAY)) / (2.0D0*ZLAY)
              QFWUP = QFWST * (1.D0-(ZC-ZLAY)/H16)
          endif
      endif
      RETURN
      END

      SUBROUTINE CJET(FLWCJT,FLXCJT)
C
C     Routine:     CJET
C
C     Description:  Interface between RESID and CEILHT.  Loops over
C                 rooms setting up varibles to pass.  Calls CEILHT
C                 only when fires are in a room otherwise sets zeros
C                 for FLXCJT.  Then uses FLXCJT to figure FLWCJT.
C
C     Arguments: FLWCJT  Net enthalphy into each layer
C                FLXCJT  Net enthalphy flux onto surface
C
C     Revision History:
C     PAR 11/91    Created.
C     gpf 2/5/93   calculated partial derivatives needed for
C                  reduced Jacobian calculation
C     gpf 2/7/93   The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have 
C                  the number of fires as the first subscript.
C     gpf 10/14/93 Added detector/sprinkler option.  changed code so
C                  jacobian elements are not computed if wall is not active.
C    
C     GPF 4/24/95    removed references to qwall, since it was not
C                    being used
C     GPF 2/5/96   removed reduced jacobian option added on 2/5/93.
C     GPF 7/22/96  handle ceiling jets in active halls.  a hall is active
C                  if the ceiling jet has started but not reached the end of 
C                  the hall.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      use cparams
      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "cshell.fi"
      include "opt.fi"
      include "wdervs.fi"
C
      DIMENSION FLWCJT(NR,2), FLXCJT(NR,NWAL)
      INTEGER CJETOPT
      DIMENSION DUMMY(100)
C
      XX0 = 0.0D0
      DO 10 I = 1, NM1
          FLXCJT(I,1) = XX0
          FLXCJT(I,2) = XX0
          FLXCJT(I,3) = XX0
          FLXCJT(I,4) = XX0
          FLWCJT(I,1) = XX0
          FLWCJT(I,2) = XX0
   10 CONTINUE
      DO 15 ID = 1, NDTECT
          IROOM = IXDTECT(ID,DROOM)
          XDTECT(ID,DVEL) = 0.0D0
          ZLOC = XDTECT(ID,DZLOC)
          IF(ZLOC>ZZHLAY(IROOM,LOWER))THEN
              XDTECT(ID,DTJET) = ZZTEMP(IROOM,UPPER)
          ELSE
              XDTECT(ID,DTJET) = ZZTEMP(IROOM,LOWER)
          ENDIF
   15 CONTINUE
      IF (OPTION(FCJET)==OFF) RETURN
      CJETOPT = OPTION(FCJET)
C
      DO 30 I = 1, NM1
          NRMFIRE = IFRPNT(I,1)
          ID = IDTPNT(I,2)
          ND = IDTPNT(I,1)

C*** handle ceiling jets that are not in active halls

          IF (CJETON(NWAL+1).AND.NRMFIRE>0.AND.
     .    IZHALL(I,IHMODE)/=IHDURING) THEN
              DO 20 IFIRE = 1, NRMFIRE
                  IFPNT = IFRPNT(I,2) + IFIRE - 1
                  IF (SWITCH(1,I)) THEN
                      TCEIL = TWJ(1,I,1)
                  ELSE
                      TCEIL = ZZTEMP(I,UPPER)
                  endif
                  IF (SWITCH(3,I)) THEN
                      TUWALL = TWJ(1,I,3)
                  ELSE
                      TUWALL = ZZTEMP(I,UPPER)
                  endif
                  CALL CEILHT(XFIRE(IFPNT,4),XFIRE(IFPNT,7),TCEIL,
     +            ZZTEMP(I,LOWER),ZZTEMP(I,UPPER),TUWALL,bR(I),dR(I),
     +            HR(I),XFIRE(IFPNT,1),XFIRE(IFPNT,2),XFIRE(IFPNT,3),
     +            ZZHLAY(I,LOWER),ZZRHO(I,LOWER),ZZRHO(I,UPPER),CJETOPT,
     +            XDTECT(ID,DXLOC),XDTECT(ID,DYLOC),XDTECT(ID,DZLOC),
     +            ND,QCEIL,QFCLGA,QFWLA,QFWUA,
     +            XDTECT(ID,DTJET),XDTECT(ID,DVEL),FTMAX,FVMAX,FDMAX)
                  FLXCJT(I,1) = FLXCJT(I,1) + QFCLGA
                  FLXCJT(I,3) = FLXCJT(I,3) + QFWUA
                  FLXCJT(I,4) = FLXCJT(I,4) + QFWLA
   20         CONTINUE
          ENDIF

C*** handle ceiling jets that are in active halls

          IF(IZHALL(I,IHMODE)==IHDURING)CALL HALLHT(I,ID,ND)

          DO 50 IWALL = 1, 4
              IF(MOD(IWALL,2)==1)THEN
                  ILAY = UPPER
              ELSE
                  ILAY = LOWER
              ENDIF

C     if (.not.(ceiling jet in fire room)) then flux to IWALL = 0.

              IF (.NOT.(SWITCH(IWALL,I).AND.CJETON(IWALL)
     .        .AND.NRMFIRE>0)) THEN
                  FLXCJT(I,IWALL) = XX0
              endif
              FLWCJT(I,ILAY) = FLWCJT(I,ILAY) - 
     .        ZZWAREA(I,IWALL)*FLXCJT(I,IWALL)
   50     CONTINUE
   30 CONTINUE
      RETURN
      END
      integer function rev_convection

      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     *mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     *maindate='$Date$'

      WRITE(module_date,'(A)') 
     *mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_convection = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_convection