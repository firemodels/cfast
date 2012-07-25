      SUBROUTINE RDHEAT(FLWRAD,FLXRAD,IERROR)

!     routine: rdheat
!     purpose: Interface between RESID and RAD2 or RAD4.  Loops over
!              rooms setting up varibles to pass.  If one or more fires
!              are in a room calls RAD4 otherwise RAD2.
!     Revision: $Revision: 484 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments: flwrad      net enthalphy into each layer
!                flxrad      net enthalphy flux into surface
!                ierror      returns error codes

      use cenviro
      use cfast_main
      use fltarget
      use opt
      include "precis.fi"

      dimension flwrad(nr,2), flxrad(nr,nwal), qlay(2), qflxw(nwal)

      dimension twall(nwal), map(nwal), emis(nwal)
      dimension tg(2)
      logical black

      ! work and dummy arrays passed to rad2 and rad4
      dimension taufl(mxfire,nwal), taufu(mxfire,nwal), firang(mxfire,nwal)
      dimension dummy(4)
      dimension flxrad0(nr,nwal), flwrad0(nr,2)
      logical roomflg(nr)
      save flxrad0, flwrad0
      data map /1, 4, 2, 3/

      do i = 1, nm1
          do j = 1, nwal
              flxrad(i,j) = 0.0d0
          end do
          flwrad(i,1) = 0.0d0
          flwrad(i,2) = 0.0d0
      end do

      IF (OPTION(FRAD)==OFF) RETURN
      BLACK = .FALSE.
      IF(OPTION(FRAD)==3)BLACK = .TRUE.

      ! initially assume that we compute radiation transfer in every room
      do i = 1, nm1
          roomflg(i) = .true.
      end do

      if(option(fmodjac)==on)then
          if(jaccol>0)then

              ! if 2nd modified jacobian is active and dassl is computing a jacobian then
              ! only compute the radiation heat transfer in the room where the dassl 
              ! solution variable has been perturbed
              do i = 1, nm1
                  roomflg(i) = .false.
              end do
              ieqtyp = izeqmap(jaccol,1)
              iroom = izeqmap(jaccol,2)
              if(ieqtyp==eqvu.or.ieqtyp==eqtu.or.ieqtyp==eqtl.or.
     .        ieqtyp==eqwt)then
                  if(ieqtyp==eqwt)iroom = izwall(iroom,1)
                  roomflg(iroom) = .true.
              endif
          endif
      endif

      do i = 1, nm1
          zzbeam(lower,i) = (1.8d0 * zzvol(i, lower)) / (ar(i) + zzhlay(i, lower) * (dr(i) + br(i)))
          zzbeam(upper,i) = (1.8 * zzvol(i, upper)) / (ar(i) + zzhlay(i, upper) * (dr(i) + br(i)))
      end do

      defabsup = 0.5d0
      defabslow = 0.01d0
      if(lfbo/=0.and.option(frad)/=4.and.lfbt/=1)then
          defabsup = absorb(lfbo,upper)
      endif

      do i = 1, nm1
          if(roomflg(i)) then
              tg(upper) = zztemp(i,upper)
              tg(lower) = zztemp(i,lower)
              zzbeam(lower,i) = (1.8d0 * zzvol(i, lower)) / (ar(i) + zzhlay(i, lower) * (dr(i) + br(i)))
              zzbeam(upper,i) = (1.8 * zzvol(i, upper)) / (ar(i) + zzhlay(i, upper) * (dr(i) + br(i)))
              do iwall = 1, 4
                  if(mod(iwall,2)==1)then
                      ilay = upper
                  else
                      ilay = lower
                  endif
                  imap = map(iwall)
                  if (switch(iwall,i)) then
                      twall(imap) = zzwtemp(i,iwall,1)
                      emis(imap) = epw(iwall,i)
                  else
                      twall(imap) = zztemp(i,ilay)
                      emis(imap) = 1.0d0
                  endif
              end do
              ifire = ifrpnt(i,2)
              nrmfire = ifrpnt(i,1)
              if (nrmfire/=0) then
                  if(.not.black)then
                      if(option(frad)==4.or.lfbt==1)then
                          zzabsb(upper,i) = defabsup
                          zzabsb(lower,i) = defabslow
                      else
                          zzabsb(upper,i) = absorb(i, upper)
                          zzabsb(lower,i) = absorb(i, lower)
                      endif
                  endif
                  call rad4(twall,tg,emis,zzabsb(1,i),i,br(i),dr(i),hr(i),zzhlay(i,lower),xfire(ifire,8),xfire(ifire,1),xfire(ifire,2),xfire(ifire,3),nrmfire, &
                  qflxw,qlay,mxfire,taufl,taufu,firang,rdqout(1,i),black,ierror)
              else
                  if(.not.black)then
                      if(option(frad)==2.or.option(frad)==4.or.
     .                lfbt==1)then
                          zzabsb(upper,i) = defabsup
                          zzabsb(lower,i) = defabslow
                      else
                          zzabsb(upper,i) = absorb(i, upper)
                          zzabsb(lower,i) = absorb(i, lower)
                      endif
                  endif
                  call rad2(twall,tg,emis,zzabsb(1,i),i,br(i),dr(i),hr(i),zzhlay(i,lower),xfire(ifire,8),xfire(ifire,1),xfire(ifire,2),xfire(ifire,3),nrmfire, &
                  qflxw,qlay,mxfire,taufl,taufu,firang,rdqout(1,i),black,ierror)

              endif
              if (ierror/=0) return
              do j = 1, nwal
                  flxrad(i,j) = qflxw(map(j))
              end do

              flwrad(i,1) = qlay(1)
              flwrad(i,2) = qlay(2)
              qr(1,i) = qlay(1)
              qr(2,i) = qlay(2)
          endif
      end do

      if(option(fmodjac)==on)then
          if(jaccol==0)then

              ! if the jacobian option is active and dassl is computing the base vector for
              ! the jacobian calculation then save the flow and flux calculation for later use
              do iroom = 1, nm1
                  do iwall = 1, nwal
                      flxrad0(iroom,iwall) = flxrad(iroom,iwall)
                  end do
                  flwrad0(iroom,1) = flwrad(iroom,1)
                  flwrad0(iroom,2) = flwrad(iroom,2)
              end do
          elseif(jaccol>0)then

              ! dassl is computing the jaccol'th column of a jacobian.  copy values into
              ! the flow and flux vectors that have not changed from the base vector
              do iroom = 1, nm1
                  if(.not.roomflg(iroom))then
                  do iwall = 1, nwal
                      flxrad(iroom,iwall) = flxrad0(iroom,iwall)
                  end do
                  flwrad(iroom,1) = flwrad0(iroom,1)
                  flwrad(iroom,2) = flwrad0(iroom,2)
                  end if
              end do
          endif
      endif
      return
      end subroutine rdheat

      subroutine rad2(twall,tlay,emis,absorb,iroom,xroom,yroom,zroom,hlay,qfire,xfire,yfire,zfire,nfire,qflux,qlay,mxfire,taufl,taufu,firang,qout,black,ierror)

!     routine: rad2
!     purpose: This routine computes the radiative heat flux to 
!              the extended ceiling (ceiling + upper wall) and the extended 
!              floor (floor + lower wall) due to a point source fire, emitting 
!              absorbing gas layers (upper and lower) and heat emitting wall 
!              segments.  This routine also computes the heat absorbed by the lower and upper layers.
!     arguments: twall(i): twall(i) is the temperature of the i'th surface [k] . where
!                          i=1,2,3,4 denotes the ceiling, the upper wall, the lower wall and the floor respectively.  
!                tlay: tlay(i) is the temperature of the i'th layer [k] where i=1,2 denotes the upper, lower layers respectively
!                emis: emis(i) is the emisivity of the ceiling (i=1), walls (i=2) and floor (i=3)
!                absorb: absorb(i) is the absorbivity [1/m] of the upper (i=1), lower layer (i=2)
!                xroom: size of the room [m] in the x'th coordinate direction.
!                yroom: size of the room [m] in the y'th coordinate direction.
!                zroom: size of the room [m] in the z'th coordinate direction.
!                hlay: height of smoke layer interface above the floor [m]
!                qfire: array of length nfire, qfire(ifire) is the energy release rate due to radiation of the ifire'th fire [w]
!                xfire: x coordinate of fire location [m]
!                yfire: y coordinate of fire location [m]
!                zfire: z coordinate of fire location [m]
!                qflux (ouptut): qflux(i) is the radiant heat flux [w/m**2] to the i'th surfaces where i=1,2,3,4 denotes the ceiling, the upper wall,
!                                the lower wall and the floor respectively.  note that qflux(1)=qflux(2) and qflux(3)=qflux(4)
!                qlay (output): qlay(i) is the heat absorbed by the i'th layer where i=1,2 denotes the upper, lower layers respectively
!                qout (output): qout(i) is the output flux from the i'th wall
!                ierror (output): returns error codes

      include "precis.fi"

      dimension tlay(2), twall(4), emis(4), absorb(2)
      dimension xfire(*), yfire(*), zfire(*)
      dimension qlay(2), qflux(4), qfire(*)
      dimension taul(2,2), tauu(2,2), beam(2,2)
      dimension taufl(mxfire,*), taufu(mxfire,*)
      dimension firang(mxfire,*), area(2), area4(4)
      dimension figs(2,2), emis2(2) 
      dimension qout(4), qqout(2)
      dimension xxl(2), xxu(2)
      logical black

      dimension a(2,2), b(2,2), e(2), c(2)
      dimension ipvt(2), rhs(2), dq(2), dqde(2)

      logical first
      integer l, u
      parameter (u = 1,l = 2)
      save sigma,first,x1,pi,third,one

      data first /.true./

      ! define local constants first time rad2 is called
      if (first) then
          sigma = 5.67d-8
          first = .false.
          x1 = 1.0d0
          pi = 4.0d0 * atan(x1)
          third = 1.0d0 / 3.0d0
          one = 1.0d0
      endif

      ! define areas of upper and lower plates
      area4(1) = xroom * yroom
      area4(2) = 2.0d0 * (zroom-hlay) * (xroom+yroom)
      area4(3) = 2.0d0 * hlay * (xroom+yroom)
      area4(4) = area4(1)
      area(1) = area4(1) + area4(2)
      area(2) = area4(3) + area4(4)
      aread = area4(1)

      ! define configuration factors 
      figs(1,1) = 1.0d0 - aread / area(1)
      figs(2,2) = 1.0d0 - aread / area(2)
      figs(1,2) = aread / area(1)
      figs(2,1) = aread / area(2)

      ! define transmission factors for surfaces with respect to themselves

      beam(1,1) = (6.0d0*xroom*yroom*(zroom-hlay)/pi) ** third
      beam(2,2) = (6.0d0*xroom*yroom*hlay/pi) ** third
      beam(1,2) = zroom
      beam(2,1) = zroom
      fl = hlay / zroom
      fu = 1.0d0 - fl
      if(.not.black)then
          tauu(1,1) = exp(-beam(1,1)*absorb(1))
          taul(2,2) = exp(-beam(2,2)*absorb(2))
      else
          tauu(1,1) = 0.0d0
          taul(2,2) = 0.0d0
      endif
      tauu(2,2) = 1.0d0
      taul(1,1) = 1.0d0

      if(.not.black)then
          tauu(1,2) = exp(-fu*beam(1,2)*absorb(1))
          taul(1,2) = exp(-fl*beam(1,2)*absorb(2))
      else
          tauu(1,2) = 0.0d0
          taul(1,2) = 0.0d0
      endif
      tauu(2,1) = tauu(1,2)
      taul(2,1) = taul(1,2)

      ! define tranmission factors for surfaces with respect to fire
      do ifire = 1, nfire
          if (zfire(ifire)>hlay) then
              xxu(1) = zroom - zfire(ifire)
              xxu(2) = zfire(ifire) - hlay
              xxl(1) = 0.0d0
              xxl(2) = hlay
          else
              xxu(1) = zroom - hlay
              xxu(2) = 0.0d0
              xxl(1) = hlay - zfire(ifire)
              xxl(2) = zfire(ifire)
          endif
          do i = 1, 2
              if(.not.black)then 
                  taufu(ifire,i) = exp(-absorb(1)*xxu(i))
                  taufl(ifire,i) = exp(-absorb(2)*xxl(i))
              else
                  taufu(ifire,i) = 0.0d0
                  taufl(ifire,i) = 0.0d0
              endif
          end do
      end do

      ! compute solid angles
      do ifire = 1, nfire
          xf = xfire(ifire)
          yf = yfire(ifire)
          zf = zfire(ifire)
          firang(ifire,1) = rdsang(-xf,xroom-xf,-yf,yroom-yf,hlay-zf)
          firang(ifire,2) = 4.0d0*pi - firang(ifire,1)
      end do
      f1d = rdparfig(xroom,yroom,zroom-hlay)
      f2d = rdparfig(xroom,yroom,hlay)

      ! define e vector
      tupper4 = (twall(1)**4*f1d+twall(2)**4*(1.0d0-f1d))
      tlower4 = (twall(4)**4*f2d+twall(3)**4*(1.0d0-f2d))
      e(1) = sigma * tupper4
      e(2) = sigma * tlower4

      ! re-map emissivity vector
      emis2(1) = (emis(1)*area4(1) + emis(2)*area4(2))/area(1)
      emis2(2) = (emis(4)*area4(4) + emis(3)*area4(3))/area(2)

      ! define 'a' and 'b' coefficicnt matrix
      do k = 1, 2
          do j = 1, 2
              aij = figs(k,j) * taul(k,j) * tauu(k,j)
              a(k,j) = -aij * (1.0d0-emis2(j))
              b(k,j) = -aij
          end do
          a(k,k) = a(k,k) + 1.0d0
          b(k,k) = b(k,k) + 1.0d0
      end do

      ! define c vector
      call rdflux(mxfire,2,1,area,hlay,tlay,zfire,qfire,figs,taul,tauu,
     +taufl,taufu,firang,nfire,qllay,qulay,c)

      ! construct right hand side (rhs) of linear system to be solved
      rhs(1) = b(1,1) * e(1) + b(1,2) * e(2) - c(1)
      rhs(2) = b(2,1) * e(1) + b(2,2) * e(2) - c(2)
      call dgefa(a,2,2,ipvt,info)
      call dgesl(a,2,2,ipvt,rhs,0)
      if(info/=0) then
          call xerror('RAD2 - singular matrix',0,1,1)
          ierror = 17
          return
      endif

      ! note: each row k of the a matrix as defined by seigal and howell was divided by emis2(k) (in order to insure that this new 'a' was
      ! diagonally dominant.  now we have to multiply the solution to the modified problem by emis2(i) to get the original answers
      do k = 1, 2
          dqde(k) = rhs(k)
          qqout(k) = e(k) - (one - emis(k))*dqde(k)
          dq(k) = rhs(k) * emis2(k)
      end do

      ! take solution and compute energy gain or loss to each panel and each layer.  also compute fluxes.  change sign so that
      ! a postive flux means that heat is flowing to the wall
      qflux(1) = -dq(1)
      qflux(2) = -dq(1)
      qflux(3) = -dq(2)
      qflux(4) = -dq(2)

      qout(1) = qqout(1)
      qout(2) = qqout(1)
      qout(3) = qqout(2)
      qout(4) = qqout(2)

      ! compute radiation absorbed by each layer
      call rdabs(2,1,e,dqde,emis2,area,figs,tauu,taul,qllay,qulay)

      qlay(u) = qulay
      qlay(l) = qllay

      return
      end subroutine rad2

      subroutine rad4(twall,tlay,emis,absorb,iroom,xroom,yroom,zroom,hlay,qfire,xfire,yfire,zfire,nfire,qflux,qlay,mxfire,taufl,taufu,firang,qout,black,ierror)

!     routine: rad4
!     purpose: this routine computes the radiative heat flux to the ceiling, upper wall, lower wall and floor due to 
!              a point source fire, emitting absorbing gas layers (upper and lower) and heat emitting wall segments. this routine 
!              also computes the heat absorbed by the lower and upper layers.
!     arguments: twall(i): twall(i) is the temperature of the i'th surface [k] . where
!                          i=1,2,3,4 denotes the ceiling, the upper wall, the lower wall and the floor respectively
!                tlay: tlay(i) is the temperature of the i'th layer [k] where i=1,2 denotes the upper, lower layers respectively
!                emis: emis(i) is the emisivity of the ceiling (i=1), walls (i=2) and floor (i=3)
!                absorb: absorb(i) is the absorbivity [1/m] of the upper (i=1), lower layer (i=2)
!                xroom: size of the room [m] in the x'th coordinate direction.
!                yroom: size of the room [m] in the y'th coordinate direction.
!                zroom: size of the room [m] in the z'th coordinate direction.
!                hlay: height of smoke layer interface above the floor [m]
!                qfire: array of length nfire, qfire(ifire) is the energy release rate due to radiation of the ifire'th fire [w]
!                xfire: x coordinate of fire location [m]
!                yfire: y coordinate of fire location [m]
!                zfire: z coordinate of fire location [m]
!                qflux (output): qflux(i) is the radiant heat flux [w/m**2] to the i'th surfaces where i=1,2,3,4 denotes the ceiling, the upper wall,
!                                the lower wall and the floor respectively
!                qlay (output): qlay(i) is the heat absorbed by the i'th layer where i=1,2 denotes the upper, lower layers respectively
!                qout (output): qout(i) is the output flux from the i'th wall
!                ierror - returns error codes

      include "precis.fi"

      dimension tlay(2), twall(4), emis(4), absorb(2)
      dimension xfire(*), yfire(*), zfire(*)
      dimension qlay(2), qflux(4), qfire(*)
      dimension taul(4,4), tauu(4,4), beam(4,4)
      dimension taufl(mxfire,*), taufu(mxfire,*)
      dimension firang(mxfire,*), area(4)
      dimension figs(4,4), qout(4)
      dimension zz(4)
      logical black

      dimension a(4,4), b(4,4), e(4), c(4)
      dimension ipvt(4), rhs(4), dq(4), dqde(4)

      integer l, u
      parameter (u = 1,l = 2,mxroom = 100)
      dimension iflag(mxroom), f14(mxroom)

      logical first

      save first,sigma

      data first /.true./, iflag /mxroom * 0/

      ! define local constants first time rad4 is called
      if (first) then
          sigma = 5.67d-8
          first = .false.
      endif
      if (iflag(iroom)==0) then
          f14(iroom) = rdparfig(xroom,yroom,zroom)
          iflag(iroom) = 1
      endif
      f14(iroom) = rdparfig(xroom,yroom,zroom)

      ! define areas
      area(1) = xroom * yroom
      area(2) = 2.0d0 * (zroom-hlay) * (xroom+yroom)
      area(3) = 2.0d0 * hlay * (xroom+yroom)
      area(4) = area(1)

      ! define configuration factors
      f1d = rdparfig(xroom,yroom,zroom-hlay)
      ff14 = f14(iroom)
      f4d = rdparfig(xroom,yroom,hlay)

      figs(1,1) = 0.0d0
      figs(1,2) = 1.0d0 - f1d
      figs(2,1) = area(1) * figs(1,2) / area(2)
      figs(2,2) = 1.0d0 - 2.0d0 * figs(2,1)
      figs(1,4) = ff14
      figs(4,1) = figs(1,4)

      figs(4,4) = 0.0d0
      figs(4,3) = 1.0d0 - f4d
      figs(3,4) = area(4) * figs(4,3) / area(3)
      figs(3,3) = 1.0d0 - 2.0d0 * figs(3,4)

      figs(1,3) = 1.0d0 - figs(1,4) - figs(1,2)
      figs(3,1) = area(1) * figs(1,3) / area(3)

      figs(3,2) = 1.0d0 - figs(3,4) - figs(3,3) - figs(3,1)
      figs(2,3) = area(3) * figs(3,2) / area(2)

      figs(2,4) = 1.0d0 - figs(2,3) - figs(2,2) - figs(2,1)
      figs(4,2) = area(2) * figs(2,4) / area(4)

      ! define transmission factors for surfaces, but first define beam lengths
      zz(1) = zroom
      zz(2) = (hlay+zroom) *.50d0
      zz(3) = hlay * .50d0
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
          do j = i + 1, 4
              beam(j,i) = beam(i,j)
          end do
      end do

      call rdrtran(4,2,absorb,beam,hlay,zz,tauu,taul,black)

      ! define transmission factors for fires
      if (nfire/=0) then
          call rdftran(mxfire,4,2,absorb,hlay,zz,nfire,zfire,taufu,taufl,black)
      endif

      ! define solid angles for fires
      if (nfire/=0) then
          call rdfang(mxfire,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang)
      endif

    !     note: we want to solve the linear system
    !         a*dq = b*e + c
    !         where a and b are nxn matrices, q, e and c are n vectors

      ! define e vector
      do i = 1, 4
          e(i) = sigma * twall(i) ** 4
      end do

      ! define 'a' and 'b' coefficient matrix
      do k = 1, 4
          do j = 1, 4
              aij = figs(k,j) * taul(k,j) * tauu(k,j)
              a(k,j) = -aij * (1.0d0-emis(j))
              b(k,j) = -aij
          end do
          a(k,k) = a(k,k) + 1.0d0
          b(k,k) = b(k,k) + 1.0d0
      end do


      ! define c vector
      ! also, calculate energy absorbed by upper, lower layer gases due to fires and gas layer emission
      call rdflux(mxfire,4,2,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

      ! construct right hand side (rhs) of linear system to be solved, i.e. compute b*e - c
      do k = 1, 4
          rhs(k) = ddot(4,b(k,1),4,e(1),1) - c(k)
      end do

      ! solve the linear system
      call dgefa(a,4,4,ipvt,info)
      if(info/=0) then
          call xerror('RAD4 - singular matrix',0,1,1)
          ierror = 18
          do k = 1, 4
              rhs(k) = 0.0d0
          end do
      else
          call dgesl(a,4,4,ipvt,rhs,0)
      endif

      ! note: each row k of the a matrix, as defined by seigal and howell was divided by emis(k) (in order to insure that this new 'a' was
      ! diagonally dominant.  now we have to multiply the solution to the modified problem by emis(i) to get the answer to the original problem
      do k = 1, 4
          dqde(k) = rhs(k)
          qout(k) = e(k) - (1.0d0 - emis(k))*dqde(k)
          dq(k) = rhs(k) * emis(k)
      end do

      ! take solution and compute energy gain or loss to each panel and each layer.  also compute fluxes.  change sign so that
      ! a postive flux means that heat is flowing to the wall
      do i = 1, 4
          qflux(i) = -dq(i)
      end do

      ! compute radiation absorbed by each layer
      call rdabs(4,2,e,dqde,emis,area,figs,tauu,taul,qllay,qulay)

      qlay(u) = qulay
      qlay(l) = qllay

      return
      end subroutine rad4

      subroutine rdflux(mxfire,nzone,nup,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

!     routine: rad4
!     purpose: this routine calculates the 'c' vector in the net radiation equations of seigel and howell and the 
!        heat absorbed by the lower and upper layer fires due to gas layer emission and fires..
!     arguments: mxfire
!                nzone
!                nup
!                area
!                hlay
!                tlay
!                zfire
!                qfire
!                figs
!                taul
!                tauu
!                taufl
!                taufu
!                firang
!                nfire
!                qllay
!                qulay
!                c

      include "precis.fi"
      
      integer l, u
      parameter (u = 1,l = 2)
      dimension c(*)
      dimension figs(nzone,*), taul(nzone,*), tauu(nzone,*)
      dimension taufl(mxfire,*), taufu(mxfire,*)
      dimension firang(mxfire,*), zfire(*)
      dimension area(*), qfire(mxfire), tlay(2)
      logical first
      save first, pi, sigma
      data first /.true./
      xx1 = 1.0d0
      if (first) then
          first = .false.
          pi = 4.0d0 * atan(xx1)
          sigma = 5.67d-8
      endif

      ! define c vector
      qulay = 0.d0
      qllay = 0.d0
      eu = sigma * tlay(u) ** 4
      el = sigma * tlay(l) ** 4
      do k = 1, nup
          c(k) = 0.d0

          ! case: upper to upper
          do j = 1, nup
              qugas = (xx1-tauu(k,j)) * eu
              c(k) = c(k) + figs(k,j) * qugas
              qulay = qulay - area(k) * figs(k,j) * qugas
          end do

          ! case: lower to upper
          do j = nup + 1, nzone
              qugas = (xx1-tauu(k,j)) * eu
              qlgas = (xx1-taul(k,j)) * el
              c(k) = c(k) + figs(k,j) * (qugas+qlgas*tauu(k,j))
              wf = area(k) * figs(k,j)
              qulay = qulay + qlgas * wf * (xx1-tauu(k,j)) - qugas * wf
              qllay = qllay - qlgas * wf
          end do

          ! case: fire to upper layer
          do ifire = 1, nfire
              qfflux = 0.25d0*qfire(ifire)*firang(ifire,k)/(pi*area(k))
              c(k) = c(k) + qfflux * taufl(ifire,k) * taufu(ifire,k)
              if (zfire(ifire)>hlay) then
                  factu = xx1 - taufu(ifire,k)
                  factl = 0.0d0
              else
                  factu = (xx1-taufu(ifire,k)) * taufl(ifire,k)
                  factl = xx1 - taufl(ifire,k)
              endif
              qulay = qulay + factu * qfflux * area(k)
              qllay = qllay + factl * qfflux * area(k)
          end do
      end do

      do k = nup + 1, nzone
          c(k) = 0.0d0

          ! case: upper to lower
          do j = 1, nup
              qugas = (xx1-tauu(k,j)) * eu
              qlgas = (xx1-taul(k,j)) * el
              c(k) = c(k) + figs(k,j) * (qugas*taul(k,j)+qlgas)
              wf = area(k) * figs(k,j)
              qulay = qulay - qugas * wf
              qllay = qllay + qugas * wf * (xx1-taul(k,j)) - qlgas * wf
          end do

          !case: lower to lower
          do j = nup + 1, nzone
              qlgas = (1.0d0-taul(k,j)) * el
              c(k) = c(k) + figs(k,j) * qlgas
              qllay = qllay - qlgas * area(k) * figs(k,j)
          end do

          ! case: fire to lower layer
          do ifire = 1, nfire
              qfflux = 0.25d0*qfire(ifire)*firang(ifire,k)/(pi*area(k))
              c(k) = c(k) + qfflux * taufl(ifire,k) * taufu(ifire,k)
              if (zfire(ifire)>hlay) then
                  factu = xx1 - taufu(ifire,k)
                  factl = (xx1-taufl(ifire,k)) * taufu(ifire,k)
              else
                  factu = 0.0d0
                  factl = xx1 - taufl(ifire,k)
              endif
              qulay = qulay + factu * qfflux * area(k)
              qllay = qllay + factl * qfflux * area(k)
          end do
      end do
      return
      end subroutine rdflux

      SUBROUTINE RDABS(NZONE,NUP,E,DQDE,EMIS2,AREA,FIGS,TAUU,TAUL,QLLAY,QULAY)

!     routine: rdabs
!     purpose: This routine computes the energy absorbed by the upper and lower layer due to radiation given off by heat emiiting rectangles
!              forming the enclosure.  Coming into this routine, qllay and qulay were previously defined to be the heat absorbed by the lower and
!              upper layers due to gas emissions and fires.  this routine just adds onto these values.
!     arguments: NZONE
!                NUP
!                E
!                DQDE
!                EMIS2
!                AREA
!                FIGS
!                TAUU
!                TAUL
!                QLLAY(output)
!                QULAY (output)

      include "precis.fi"

      dimension e(*), emis2(*), area(*),dqde(*)
      dimension figs(nzone,*), tauu(nzone,*),taul(nzone,*)

      do k = 1, nup
          qout = e(k) - dqde(k)*(1.0d0-emis2(k))
          qk = qout*area(k)
          do j = 1,nup
              qulay = qulay + qk*figs(k,j)*(1.0d0-tauu(k,j))
          end do
          do j = nup + 1, nzone
              qulay = qulay + qk*figs(k,j)*(1.0d0-tauu(k,j))
              qllay = qllay + qk*figs(k,j)*tauu(k,j)*(1.0d0-taul(k,j))
          end do
      end do

      do k = nup+1,nzone
          qout = e(k) - dqde(k)*(1.0d0-emis2(k))
          qk = qout*area(k)
          do j = 1,nup
              qulay = qulay + qk*figs(k,j)*taul(k,j)*(1.0d0-tauu(k,j))
              qllay = qllay + qk*figs(k,j)*(1.0d0-taul(k,j))
          end do
          do j = nup+1,nzone
              qllay = qllay + qk*figs(k,j)*(1.0d0-taul(k,j))
          end do
      end do

      return
      end subroutine rdabs

      real*8 FUNCTION RDPARFIG(X,Y,Z)

!     routine: rdparfig
!     purpose: This routine calculates the configuration factor between two paralell plates a distance z a part.  Each 
!          plate has a dimension of x by y.  the units of x, y and z are un-important except that they must be consistent.
!     arguments: X
!                Y
!                Z

      include "precis.fi"

      SAVE IFIRST, PI, XX0, XX1, XXH
      DATA IFIRST /0/
      IF (IFIRST==0) THEN
          XX1 = 1.0D0
          XXH = 0.5D0
          XX0 = 0.0D0
          PI = 4.0D0 * ATAN(XX1)
          IFIRST = 1
      endif
      RDPARFIG = XX0
      IF (Z==XX0.OR.X==XX0.OR.Y==XX0) RETURN
      XX = X / Z
      YY = Y / Z
      F1 = XXH*LOG((XX1+XX**2)*(XX1+YY**2)/(XX1+XX**2+YY**2))
      YSQ = SQRT(XX1+YY**2)
      F2 = XX * YSQ * ATAN(XX/YSQ)
      XSQ = SQRT(XX1+XX**2)
      F3 = YY * XSQ * ATAN(YY/XSQ)
      F4 = XX * ATAN(XX)
      F5 = YY * ATAN(YY)
      RDPARFIG = 2.0D0 * (F1+F2+F3-F4-F5) / (PI*XX*YY)
      RETURN
      END
      real*8 FUNCTION RDPRPFIG(X,Y,Z)

!     routine: rdparfig
!     purpose: this routine calculates the configuration factor between two perpindular plates with a common edge.
!     arguments: X
!                Y
!                Z

      include "precis.fi"
    
      LOGICAL FIRST
      SAVE FIRST, PI
      DATA FIRST /.TRUE./
      XX1 = 1.0D0
      XX0 = 0.0D0
      IF (FIRST) THEN
          PI = 4.0D0 * ATAN(XX1)
          FIRST = .FALSE.
      endif
      RDPRPFIG = XX0
      IF (Y==XX0.OR.X==XX0.OR.Z==XX0) RETURN
      H = X / Y
      W = Z / Y
      F1 = W * ATAN(XX1/W)
      F2 = H * ATAN(XX1/H)
      
      HWSUM = H ** 2 + W ** 2
      HWNORM = SQRT(HWSUM)
      RHWNORM = 1.0D0/HWNORM
      F3 = HWNORM * ATAN(RHWNORM)

      WSUM1 = XX1 + W ** 2
      HSUM1 = XX1 + H ** 2
      HWSUM2 = XX1 + HWSUM
      F4A = WSUM1 * HSUM1 / HWSUM2
      F4B = (W**2*HWSUM2/WSUM1/HWSUM)
      F4C = (H**2*HWSUM2/HSUM1/HWSUM)
      F4 = 0.25D0*(LOG(F4A)+LOG(F4B)*W**2+LOG(F4C)*H**2) 
      RDPRPFIG = (F1+F2-F3+F4) / PI / W
      RETURN
      END

      SUBROUTINE RDFANG(MXFIRE,XROOM,YROOM,ZROOM,HLAY,NFIRE,XFIRE,YFIRE,ZFIRE,FIRANG)

!     routine: rdfang
!     purpose: 
!     arguments: MXFIRE
!                XROOM
!                YROOM
!                ZROOM
!                HLAY
!                NFIRE
!                XFIRE
!                YFIRE
!                ZFIRE
!                FIRANG

      include "precis.fi"

      DIMENSION XFIRE(*), YFIRE(*), ZFIRE(*)
      DIMENSION FIRANG(MXFIRE,*)
      LOGICAL FIRST
      SAVE FIRST, FOURPI
      DATA FIRST/.TRUE./

      IF(FIRST)THEN
          FIRST = .FALSE.
          XX1 = 1.0D0
          PI = 4.0D0*ATAN(XX1)
          FOURPI = 4.0D0*PI
      ENDIF

      DO I = 1, NFIRE
          ARG1 = -XFIRE(I)
          ARG2 = XROOM - XFIRE(I)
          ARG3 = -YFIRE(I)
          ARG4 = YROOM - YFIRE(I)
          F1 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZROOM-ZFIRE(I))
          FD = RDSANG(ARG1,ARG2,ARG3,ARG4,HLAY-ZFIRE(I))
          F4 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZFIRE(I))
          FIRANG(I,1) = F1
          FIRANG(I,4) = F4
          IF (ZFIRE(I)<HLAY) THEN
              FIRANG(I,2) = FD - F1
              FIRANG(I,3) = FOURPI - FD - F4
          ELSE
              FIRANG(I,2) = FOURPI - FD - F1
              FIRANG(I,3) = FD - F4
          endif
      end do
      RETURN
      END
      real*8 FUNCTION RDSANG(X1,X2,Y1,Y2,R)

!     routine: rdsang
!     purpose: 
!     arguments: X1
!                X2
!                Y1
!                Y2
!                R

      include "precis.fi"

      f1 = sign(rdsang1(abs(x2),abs(y2),r),x2*y2)
      f2 = sign(rdsang1(abs(x1),abs(y2),r),x1*y2)
      f3 = sign(rdsang1(abs(x2),abs(y1),r),x2*y1)
      f4 = sign(rdsang1(abs(x1),abs(y1),r),x1*y1)
      rdsang = f1 - f2 - f3 + f4
      return
      end
      
      real*8 FUNCTION RDSANG1(X,Y,R)

!     routine: rdsang1
!     purpose: 
!     arguments: X
!                Y
!                R

      include "precis.fi"

      LOGICAL FIRST
      SAVE FIRST, PI, PIO2
     
      DATA FIRST /.TRUE./
     
      XX0 = 0.0D0
      XX1 = 1.0D0
      IF (FIRST) THEN
          FIRST = .FALSE.
          PI = 4.0D0 * ATAN(XX1)
          PIO2 = PI / 2.0D0
      endif
      IF (X<=XX0.OR.Y<=XX0) THEN
          RDSANG1 = XX0
      ELSE
          XR = X * X + R * R
          XYR = X * X + Y * Y + R * R
          XY = X * X + Y * Y
          YR = Y * Y + R * R
          F1 = MIN(XX1, Y * SQRT(XYR/XY/YR))
          F2 = MIN(XX1, X * SQRT(XYR/XY/XR))
          RDSANG1 = (ASIN(F1)+ASIN(F2)-PIO2)
      endif
      RETURN
      END

      SUBROUTINE RDFTRAN(MXFIRE,NZONE,NUP,ABSORB,HLAY,ZZ,NFIRE,ZFIRE,TAUFU,TAUFL,BLACK)

!     routine: rdftran
!     purpose: 
!     arguments: MXFIRE
!                NZONE
!                NUP
!                ABSORB
!                HLAY
!                ZZ
!                NFIRE
!                ZFIRE
!                TAUFU
!                TAUFL

      include "precis.fi"
    
      DIMENSION ABSORB(*), ZZ(*), ZFIRE(*)
      DIMENSION TAUFU(MXFIRE,*), TAUFL(MXFIRE,*)
      LOGICAL BLACK
      DO I = 1, NFIRE
          DO J = 1, NUP
              IF (ZFIRE(I)>HLAY) THEN
                  BEAM = ABS(ZZ(J)-ZFIRE(I))
                  TAUFL(I,J) = 1.0D0
                  IF(.NOT.BLACK)THEN
                      TAUFU(I,J) = EXP(-ABSORB(1)*BEAM)
                  ELSE
                      TAUFU(I,J) = 0.0D0
                      TAUFL(I,J) = 0.0D0
                  ENDIF
              ELSE
                  BEAMU = ZZ(J) - HLAY
                  BEAML = HLAY - ZFIRE(I)
                  IF(.NOT.BLACK)THEN
                      TAUFU(I,J) = EXP(-ABSORB(1)*BEAMU)
                      TAUFL(I,J) = EXP(-ABSORB(2)*BEAML)
                  ELSE
                      TAUFU(I,J) = 0.0D0
                      TAUFL(I,J) = 0.0D0
                  ENDIF
              endif
          end do
          DO J = NUP + 1, NZONE
              IF (ZFIRE(I)<=HLAY) THEN
                  BEAM = ABS(ZZ(J)-ZFIRE(I))
                  TAUFU(I,J) = 1.0D0
                  IF(.NOT.BLACK)THEN
                      TAUFL(I,J) = EXP(-ABSORB(2)*BEAM)
                  ELSE
                      TAUFL(I,J) = 0.0D0
                      TAUFU(I,J) = 0.0D0
                  ENDIF
              ELSE
                  BEAMU = ZFIRE(I) - HLAY
                  BEAML = HLAY - ZZ(J)
                  IF(.NOT.BLACK)THEN
                      TAUFU(I,J) = EXP(-ABSORB(1)*BEAMU)
                      TAUFL(I,J) = EXP(-ABSORB(2)*BEAML)
                  ELSE
                      TAUFU(I,J) = 0.0D0
                      TAUFL(I,J) = 0.0D0
                  ENDIF
              endif
          end do
      end do
      RETURN
      END

      SUBROUTINE RDRTRAN(NZONE,NUP,ABSORB,BEAM,HLAY,ZZ,TAUU,TAUL,BLACK)

!     routine: rdftran
!     purpose: 
!     arguments: NZONE
!                NUP
!                ABSORB
!                BEAM
!                HLAY
!                ZZ
!                TAUU
!                TAUL

      include "precis.fi"
      DIMENSION ABSORB(*), BEAM(NZONE,NZONE), ZZ(*)
      DIMENSION TAUU(NZONE,NZONE), TAUL(NZONE,NZONE)
      LOGICAL BLACK

      ! DEFINE UPPER LAYER TRANSMISSION FACTORS
      ! UPPER TO UPPER
      DO I = 1, NUP
          DO J = I + 1, NUP
              IF(.NOT.BLACK)THEN
                  TAUU(I,J) = EXP(-ABSORB(1)*BEAM(I,J))
              ELSE
                  TAUU(I,J) = 0.0D0
              ENDIF
              TAUU(J,I) = TAUU(I,J)
          end do
          IF(.NOT.BLACK)THEN
              TAUU(I,I) = EXP(-ABSORB(1)*BEAM(I,I))
          ELSE
              TAUU(I,I) = 0.0D0
          ENDIF
      end do

      ! UPPER TO LOWER AND LOWER TO UPPER
      DO I = 1, NUP
          DO J = NUP + 1, NZONE
              FU = (ZZ(I)-HLAY) / (ZZ(I)-ZZ(J))
              IF(.NOT.BLACK)THEN
                  TAUU(I,J) = EXP(-ABSORB(1)*BEAM(I,J)*FU)
              ELSE
                  TAUU(I,J) = 0.0D0
              ENDIF
              TAUU(J,I) = TAUU(I,J)
          end do
      end do

      ! LOWER TO LOWER
      DO I = NUP + 1, NZONE
          DO J = NUP + 1, NZONE
              IF(.NOT.BLACK)THEN
                  TAUU(I,J) = 1.0D0
              ELSE
                  TAUU(I,J) = 0.0D0
              ENDIF
          end do
      end do

      ! DEFINE LOWER LAYER TRANSMISSION FACTORS
      ! LOWER TO LOWER
      DO I = NUP + 1, NZONE
          DO J = I + 1, NZONE
              IF(.NOT.BLACK)THEN
                  TAUL(I,J) = EXP(-ABSORB(2)*BEAM(I,J))
              ELSE
                  TAUL(I,J) = 0.0D0
              ENDIF
              TAUL(J,I) = TAUL(I,J)
          end do
          IF(.NOT.BLACK)THEN
              TAUL(I,I) = EXP(-ABSORB(2)*BEAM(I,I))
          ELSE
              TAUL(I,I) = 0.0D0
          ENDIF
      end do

      ! UPPER TO UPPER
      DO I = 1, NUP
          DO J = 1, NUP
              IF(.NOT.BLACK)THEN
                  TAUL(I,J) = 1.0D0
              ELSE
                  TAUL(I,J) = 0.0D0
              ENDIF
          end do
      end do

      ! UPPER TO LOEWR AND LOWER TO UPPER
      DO I = NUP + 1, NZONE
          DO J = 1, NUP
              FL = (HLAY-ZZ(I)) / (ZZ(J)-ZZ(I))
              IF(.NOT.BLACK)THEN
                  TAUL(I,J) = EXP(-ABSORB(2)*BEAM(I,J)*FL)
              ELSE
                  TAUL(I,J) = 0.0D0
              ENDIF
              TAUL(J,I) = TAUL(I,J)
          end do
      end do
      RETURN
      END

      real*8 FUNCTION ABSORB (CMPT, LAYER)

!  FUNCTION CALCULATES ABSORBANCE, DUE TO GASES (CO2 AND H2O) AND SOOT, FOR THE SPECIFIED COMPARTMENT AND LAYER.

!  ABSORBANCES ARE ASSUMED TO BE EQUAL TO EMISSIVITIES. PER SPFE HANDBOOK (1988 ED., PAGES 1-99 - 1-101), GAS ABSORBANCE ISCALCULATED AS

!  AG = CH2O * EH2O + CCO2 * ECO2 - DELTAE ~ EH2O + 0.5 * ECO2;

!  WHERE CH2O AND CCO2 ARE CONCENTRATIONS AND DELTAE IS A CORRECTION
!  FOR OVERLAP OF THE ABSORBANCE BANDS.

!  ECO2 AND EH2O ARE INTERPOLATED FROM SPFE HANDBOOK GRAPHS WHICH SHOW E = F(T,PL), WHERE T IS THE GAS TEMPERATURE (KELVINS) AND PL IS THE
!  PARTIAL PRESSURE-PATH LENGTH PRODUCT (ATM-M). TEMPERATURE AND GAS PARTIAL PRESSURES ARE BASED ON DATA CALCULATED ELSEWHERE AND STORED IN
!  COMMON BLOCKS. USING HANDBOOK FORMULAE, PATH LENGTH IS ESTIMATED AS

!  L = C * 4 * V/A; WHERE C ~ 0.9 FOR TYPICAL GEOMETRIES, V IS THE GAS VOLUME AND A IS THE SURFACE AREA OF THE GAS VOLUME.

!  TOTAL ABSORBANCE IS CALCULATED AS

!  AT = AS + AG * TRANS = (1 - EXP(-AS)) + AG * EXP(-AS);

!  WHERE AS IS SOOT ABSORPION, AG IS GAS ABSORPTION, TRANS IS SOOT TRANSMISSION, A IS THE EFFECTIVE ABSORBANCE COEFFICIENT FOR SOOT AND
!  S IS THE PHYSICAL PATHLENGTH. S IS APPRXOMINATED BY L, THE MEAN BEAM LENGTH, AND A ~ K*VFS*TG, WHERE VFS IS THE SOOT VOLUME FRACTION, TG THE
!  GAS TEMPERATURE AND K IS A CONSTANT. FOR TYPICAL FUELS, K ~ 1195.5.

!  VERSION 1.0.3

      use cenviro
      include "precis.fi"

      ! DECLARE PARAMETERS
      INTEGER NOERR, HIERR, LOERR
      PARAMETER (NOERR=0, HIERR=+1, LOERR=-1)

      INTEGER CO2XSIZE, CO2YSIZE, H2OXSIZE, H2OYSIZE
      PARAMETER (CO2XSIZE=11, CO2YSIZE=12, H2OXSIZE=11, H2OYSIZE=12)

      INTEGER CO2, H2O, SOOT
      PARAMETER (CO2=3, H2O=8, SOOT=9)	

      ! DECLARE I/O VARIABLES

      INTEGER CMPT, LAYER

!  DECLARE INTERNAL VARIABLES
!  UNITS:
!    TG = KELVINS; TCO2, TH2O = LOG(KELVINS)
!    PLG = ATM-M; PLCO2, PLH2O = LOG(ATM-M)
!    L = M; NG = MOL; RTV = ATM/MOL
!    AG, ABSORB = NUMBER (ABSORBANCE)
!    ACO2, AH2O, ECO2, EH2O = LOG(EMISS)
!    VFS = NUMBER (SOOT VOLUME FRACTION)
!    RHOS = KG/CUBIC METER (SOOT DENSITY)
!    TRANS = NUMBER (SOOT TRANSMISSION = EXP(-K*VFS*TG*L))
!    K = 1/(KELVIN-METER) (SSOT ABSORPTION CONSTANT)
!    MWCO2, MWH2O = GAS MOLECULAR WEIGHT (KG/GM-MOLE)
!    RG = IDEAL GAS CONSTANT (ATM-M^3/MOL-K)

      INTEGER XCO2, YCO2, XH2O, YH2O
      DIMENSION TCO2(CO2XSIZE), PLCO2(CO2YSIZE)
      DIMENSION ECO2(CO2XSIZE,CO2YSIZE)
      DIMENSION TH2O(H2OXSIZE)
      DIMENSION PLH2O(H2OYSIZE), EH2O(H2OXSIZE,H2OYSIZE)
      real*8 MWCO2,MWH2O, K, RHOS, L, NG

      ! DECLARE MODULE DATA

      ! PHYSICAL CONSTANTS [MW IN KG/MOL; RG IN M^3-ATM/MOL-KELVIN]
      DATA MWCO2, MWH2O, RG, K, RHOS /44.0088D-3, 18.0153D-3, 82.0562D-6, 1195.5D0, 1800.0D0/

      ! LOG(T) DATA FOR CO2 [T IN K] 
      DATA TCO2  /2.3010D0, 2.4771D0, 2.6021D0, 2.6990D0, 2.7782D0, 2.8451D0, 2.9031D0, 2.9542D0,3.0000D0, 3.3010D0, 3.4771D0        /

      ! LOG(PL) DATA FOR CO2 [PL IN ATM-M]
      DATA PLCO2 /-3.0000D0, -2.6990D0, -2.3979D0, -2.0000D0, -1.6990D0, -1.3979D0, -1.0000D0, -0.6990D0, -0.3979D0,  0.0000D0,  0.3010D0,  0.6021D0/

      ! LOG(EMISS) DATA FOR CO2 [STORED IN E(T,PL) FORMAT (ASCENDING ORDER BY TEMPERATURE, THEN BY PRESSURE-LENGTH)]
      DATA ECO2  /-1.8508D0, -1.8416D0, -1.8508D0, -1.7799D0, -1.6990D0, -1.6799D0, -1.6904D0, -1.6990D0, -1.7399D0, -2.3706D0, -2.8996D0, &
                  -1.6990D0, -1.6799D0, -1.6904D0, -1.6308D0, -1.5498D0, -1.5302D0, -1.5302D0, -1.5498D0, -1.5800D0, -2.1002D0, -2.6108D0, &
                  -1.5406D0, -1.5200D0, -1.5498D0, -1.4895D0, -1.4401D0, -1.3904D0, -1.3904D0, -1.4101D0, -1.4202D0, -1.8894D0, -2.3002D0, &
     +-1.3799D0, -1.3298D0, -1.3497D0, -1.3298D0,
     +-1.2700D0, -1.2403D0, -1.2403D0, -1.2503D0,
     +-1.2700D0, -1.6596D0, -2.0400D0,
     +-1.2403D0, -1.2000D0, -1.2403D0, -1.2104D0,
     +-1.1599D0, -1.1403D0, -1.1302D0, -1.1403D0,
     +-1.1500D0, -1.5200D0, -1.8894D0,
     +-1.1403D0, -1.1002D0, -1.1403D0, -1.1203D0,
     +-1.0799D0, -1.0400D0, -1.0301D0, -1.0301D0,
     +-1.0600D0, -1.3799D0, -1.7305D0, 
     +-1.0400D0, -0.9914D0, -1.0200D0, -1.0200D0,
     +-0.9706D0, -0.9547D0, -0.9431D0, -0.9355D0,
     +-0.9431D0, -1.1599D0, -1.4802D0,
     +-0.9914D0, -0.9431D0, -0.9547D0, -0.9508D0,
     +-0.9136D0, -0.8827D0, -0.8666D0, -0.8539D0,
     +-0.8601D0, -1.1002D0, -1.3706D0, 
     +-0.9355D0, -0.8697D0, -0.8928D0, -0.8827D0,
     +-0.8477D0, -0.8097D0, -0.7932D0, -0.7852D0,
     +-0.7932D0, -1.0000D0, -1.2700D0,
     +-0.8762D0, -0.8013D0, -0.8097D0, -0.8013D0,
     +-0.7645D0, -0.7352D0, -0.7100D0, -0.6990D0,
     +-0.6990D0, -0.8962D0, -1.1331D0,
     +-0.8297D0, -0.7496D0, -0.7645D0, -0.7472D0,
     +-0.7055D0, -0.6696D0, -0.6421D0, -0.6326D0,
     +-0.6402D0, -0.8097D0, -1.0301D0,
     +-0.8013D0, -0.7144D0, -0.7144D0, -0.6840D0,
     +-0.6478D0, -0.6108D0, -0.5884D0, -0.5817D0,
     +-0.5817D0, -0.7352D0, -0.9431D0         /

C     !OG(T) DATA FOR H2O [T IN K] 
      DATA TH2O  /2.3201D0, 2.4771D0, 2.6021D0, 2.6990D0,
     +2.7782D0, 2.8451D0, 2.9031D0, 2.9542D0,
     +3.0000D0, 3.3010D0, 3.4771D0        /

      ! LOG(PL) DATA FOR H2O [PL IN ATM-M]
      DATA PLH2O /-3.0000D0, -2.6990D0, -2.3979D0, -2.0000D0,
     +-1.6990D0, -1.3979D0, -1.0000D0, -0.6990D0,
     +-0.3979D0,  0.0000D0,  0.3010D0,  0.6021D0/

      ! LOG(EMISS) DATA FOR H2O [STORED IN E(T,PL) FORMAT (ASCENDING ORDER BY TEMPERATURE, THEN BY PRESSURE-LENGTH)]
      DATA EH2O  /-1.1500D0, -1.5200D0, -1.7496D0, -1.8996D0,
     +-2.0000D0, -2.1002D0, -2.1898D0, -2.2798D0,
     +-2.3706D0, -3.0555D0, -3.4437D0,
     +-1.0200D0, -1.3298D0, -1.5302D0, -1.6596D0,
     +-1.7595D0, -1.8416D0, -1.9208D0, -2.0000D0,
     +-2.0799D0, -2.7496D0, -3.1871D0,
     +-0.8962D0, -1.1701D0, -1.3242D0, -1.4597D0,
     +-1.5406D0, -1.6003D0, -1.6596D0, -1.7305D0,
     +-1.7905D0, -2.4202D0, -2.8794D0,
     +-0.7696D0, -1.0000D0, -1.1302D0, -1.2204D0,
     +-1.3002D0, -1.3497D0, -1.4001D0, -1.4401D0,
     +-1.4802D0, -1.9914D0, -2.5200D0,
     +-0.6402D0, -0.8729D0, -0.9957D0, -1.0799D0,
     +-1.1302D0, -1.1701D0, -1.2104D0, -1.2503D0,
     +-1.2899D0, -1.6904D0, -2.1500D0,
     +-0.5884D0, -0.7645D0, -0.8729D0, -0.9355D0,
     +-0.9788D0, -1.0200D0, -1.0400D0, -1.0701D0,
     +-1.1002D0, -1.4101D0, -1.8210D0,
     +-0.5003D0, -0.6556D0, -0.7258D0, -0.7545D0,
     +-0.7932D0, -0.8153D0, -0.8447D0, -0.8665D0,
     +-0.8894D0, -1.0799D0, -1.4401D0,
     +-0.4437D0, -0.5670D0, -0.6271D0, -0.6402D0,
     +-0.6517D0, -0.6696D0, -0.6861D0, -0.6990D0,
     +-0.7190D0, -0.8729D0, -1.1403D0,
     +-0.3936D0, -0.5086D0, -0.5302D0, -0.5376D0,
     +-0.5482D0, -0.5528D0, -0.5670D0, -0.5719D0,
     +-0.5817D0, -0.7122D0, -0.9431D0,
     +-0.3458D0, -0.4295D0, -0.4401D0, -0.4365D0,
     +-0.4401D0, -0.4413D0, -0.4510D0, -0.4535D0,
     +-0.4584D0, -0.5376D0, -0.7144D0,
     +-0.2958D0, -0.3686D0, -0.3686D0, -0.3645D0,
     +-0.3645D0, -0.3686D0, -0.3706D0, -0.3757D0,
     +-0.3757D0, -0.4510D0, -0.5952D0,
     +-0.2620D0, -0.3307D0, -0.3233D0, -0.3045D0,
     +-0.3010D0, -0.3045D0, -0.3045D0, -0.3054D0,
     +-0.3080D0, -0.3605D0, -0.5086D0         /

      ! CALCULATE LAYER-SPECIFIC FACTORS
      TG = ZZTEMP(CMPT, LAYER)
      RTV = (RG * TG) / ZZVOL(CMPT, LAYER)
      L = ZZBEAM(LAYER,CMPT)

      AG = 0.0D0

      ! CALCULATE ABSORBANCE FOR CO2
      NG = ZZGSPEC(CMPT, LAYER, CO2) / MWCO2
      PLG = NG * RTV * L
      IF (PLG>1.0D-3) THEN
          CPLG = LOG10(PLG)
          TGLOG = LOG10(TG)
          CALL LINTERP(CO2XSIZE, CO2YSIZE, TCO2, PLCO2, ECO2, TGLOG,
     +    CPLG, ACO2, XCO2, YCO2)
          AG = AG + 0.50D0*10.0D0**ACO2
      ENDIF

      ! CALCULATE ABSORBANCE FOR H2O
      NG = ZZGSPEC(CMPT, LAYER, H2O) / MWH2O
      PLG = NG * RTV * L
      IF (PLG>1.0D-3) THEN
          CPLG = LOG10(PLG)
          TGLOG = LOG10(TG)
          CALL LINTERP(H2OXSIZE, H2OYSIZE, TH2O, PLH2O, EH2O, TGLOG,
     +    CPLG, AH2O, XH2O, YH2O)
          AG = AG + 10.0D0**AH2O
      ENDIF

      ! CALCULATE TOTAL ABSORBANCE
      VFS = ZZGSPEC(CMPT,LAYER,SOOT)/(ZZVOL(CMPT,LAYER) * RHOS)
      ABSORB = MAX(K*VFS*TG - LOG(1.0D0-AG)/L,0.01D0)
      RETURN
 1000 FORMAT ('ERROR IN ',A3,' ABSORBANCE: XERROR = 'I2,
     +'; YERROR = ',I2)
      END

      SUBROUTINE LINTERP(XDIM, YDIM, X, Y, Z, XVAL, YVAL, ZVAL,
     +XERR, YERR)

!     routine: linterp
!     purpose: SUBROUTINE CALCULATES A 2-D LINEAR INTERPOLATION OF F(X,Y); WHERE KNOWN F(X,Y) VALUES ARE IN Z, ALLOWED X AND Y VALUES ARE IN X AND Y, THE POINT
!              TO BE INTERPOLATED IS (XVAL,YVAL) AND THE INTERPOLATED RESULT IS RETURNED AS ZVAL. ARRAY DIMENSIONS ARE SPECIFIED BY XDIM AND YDIM, XERR AND YERR
!              ARE ERROR VALUES RETURNED TO THE CALLING FUNCTION.

!  THE EQUATION IMPLIMENTED BY THIS FUNCTION IS:

!  F(X,Y) = Z(I,J) + {[Z(I+1,J) - Z(I,J)] / [X(I+1) - X(I)]} * [X - X(I)] 
!          + {[Z(I,J+1) - Z(I,J)] / [Y(J+1) - Y(I)]} * [Y - Y(J)]
!     arguments: 

      include "precis.fi"
      INTEGER HIERR
      PARAMETER (NOERR=0, HIERR=+1, LOERR=-1) 

      ! DECLARE I/O PARAMETERS 
      INTEGER XDIM, YDIM, XERR, YERR
      DIMENSION X(XDIM), Y(YDIM), Z(XDIM,YDIM)

      ! DECLARE INTERNAL VARIABLES
      INTEGER COUNT

      ! FIND THE VALUE OF I SUCH THAT X(1) <= XVAL <= X(XDIM). IF XVAL IS OUTSIDE THAT RANGE, SET IT TO THE CLOSEST LEGAL VALUE AND SET THE ERROR VALUE, AS APPROPRIATE.

      ! CHECK THE SPECIAL CASE OF XVAL < X(1)
      IF (XVAL < X(1)) THEN
          XERR = LOERR
          XVAL = X(1)
          I = 1

      ! CHECK THE SPECIAL CASE OF XVAL > X(XDIM)
      ELSE IF (XVAL > X(XDIM)) THEN
          XERR = HIERR
          XVAL = X(XDIM)
          I = XDIM

      ! CHECK THE CASES WHERE X(1) <= XVAL < X(XDIM)
      ELSE
          XERR = NOERR
          DO COUNT=2,XDIM
              IF (XVAL < X(COUNT)) THEN
                  I = COUNT - 1
                  GO TO 20
              endif 
          end do
      ! THEN XVAL = X(XDIM)
          I = XDIM
   20     CONTINUE
      endif

      ! CHECK THE SPECIAL CASE OF YVAL < Y(1)
      IF (YVAL < Y(1)) THEN
          YERR = LOERR
          YVAL = Y(1)
          J = 1

      ! CHECK THE SPECIAL CASE OF YVAL > Y(YDIM)
      ELSE IF (YVAL > Y(YDIM)) THEN
          YERR = HIERR
          YVAL = Y(YDIM)
          J = YDIM

      ! CHECK THE CASES OF Y(1) <= YVAL < Y(YDIM)
      ELSE
          YERR = NOERR
          DO COUNT=2,YDIM
              IF (YVAL < Y(COUNT)) THEN
                  J = COUNT - 1
                  GO TO 40
              endif
          end do

      ! THEN YVAL = Y(YDIM)
          J = YDIM
   40     CONTINUE
      endif

! CALCULATE DELTA X, SLOPE X AND THE Z INCREMENT DUE TO A CHANGE IN X. IF XVAL = X(XDIM), THEN (I+1) IS UNDEFINED AND THE SLOPE CAN NOT BE
! CALCULATED. HOWEVER, IN THOSE CASES, DELTA X IS ZERO, THERE IS NO CONTRIBUTION DUE TO THE CHANGE IN X AND THE ENTIRE TERM MAY BE SET EQUAL TO ZERO.
      DELTAX = XVAL - X(I)
      IF (DELTAX /= 0.0D0) THEN
          DZDX = (Z(I+1,J) - Z(I,J)) / (X(I+1) - X(I))
          DELX = DZDX * DELTAX
      ELSE
          DELX = 0.
      endif

      ! CALCULATE THE Z INCREMENT DUE TO A CHANGE IN Y AS ABOVE.
      DELTAY = YVAL - Y(J)
      IF (DELTAY /= 0.0D0) THEN
          DZDY = (Z(I,J+1) - Z(I,J)) / (Y(J+1) - Y(J))
          DELY = DZDY * DELTAY
      ELSE
          DELY = 0.
      endif

      ! INTERPOLATE A VALUE FOR F(X,Y)
      ZVAL = Z(I,J) + DELX + DELY
      RETURN
      END

      integer function rev_radiation

      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     *mainrev='$Revision: 484 $'
      CHARACTER(255), PARAMETER :: 
     *maindate='$Date: 2012-07-23 14:49:22 -0400 (Mon, 23 Jul 2012) $'

      WRITE(module_date,'(A)') 
     *mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_radiation = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_radiation