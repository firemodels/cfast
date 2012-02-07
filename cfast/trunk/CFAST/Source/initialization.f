      subroutine gettpp (name, tp, errorcode)

!     Routine: gettpp
!     Purpose: check for and return index to a thermal property
!     Revision: $Revision$
!     Revision Date: $Date$

      include "cfast.fi"
      include "thermp.fi"

      character name*(*), missingtpp*64
      integer tp, errorcode

      errorcode = 0
      do i = 1, maxct
          tp = i
          if (name.eq.nlist(i)) return
      end do
      missingtpp = name
      errorcode = 205
      write(3,'(''Missing tpp = '',a)') missingtpp
      return
      end

      subroutine gres(nnn,hvpsolv,deltamv,iflag)

!     routine: gres
!     purpose: calculates residuals for initial solution by snsqe
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments: nnn
!                hvpsolv
!                deltamv
!                iflag

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "solvprm.fi"
      include "params.fi"
      include "cshell.fi"
      include "opt.fi"

      dimension hvpsolv(*), deltamv(*), p2(maxteq), delta(maxteq), 
     +pdzero(maxteq)
      data pdzero /maxteq * 0.0d0/
      nalg = nm1 + nhvpvar + nhvtvar
      do i = 1, nalg
          p2(i) = hvpsolv(i)
      end do
      do i = nalg + 1, nequals
          p2(i) = pinit(i)
      end do
      if(iprtalg.ne.0)then
          write(iofilo,*)'room pressures'
          do i = 1, nm1
              write(iofilo,*)i,p2(i)
          end do
          if(nhvpvar.gt.0)write (iofilo,*) 'hvac pressures'
          do i = 1, nhvpvar
              write(iofilo,*)i,p2(i+nofpmv)
          end do
          if(nhvtvar.gt.0)write (iofilo,*) 'hvac temperatures'
          do i = 1, nhvtvar
              write(iofilo,*)i,p2(i+noftmv)
          end do
      endif
      t = stime
      call resid(t,p2,pdzero,delta,ires,rpar2,ipar2)
      do i = 1, nalg
          deltamv(i) = delta(i)
      end do
      do i = 1, nm1
          if(.not.izcon(i))deltamv(i) = 0.0d0
      end do
      if(iprtalg.ne.0)then
          write(iofilo,*)'room pressure residuals'
          do i = 1, nm1
              write(iofilo,*)i,delta(i)
          end do
          if(nhvpvar.gt.0)write (iofilo,*) 'hvac pressure residuals'
          do i = 1, nhvpvar
              write(iofilo,*)i,delta(i+nofpmv)
          end do
          if(nhvtvar.gt.0)write (iofilo,*) 'hvac temperature residuals'
          do i = 1, nhvtvar
              write(iofilo,*)i,delta(i+noftmv)
          end do
          write(iofilo,*)' '
          pause
      endif
      return
      end

      subroutine gres2(nnn,hvsolv,deltamv,iflag)

!     routine: gres2
!     purpose: calculates residuals for initial solution by snsqe
!              (HVAC pressure and temperature)
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     Arguments: NNN
!                HVSOLV
!                DELTAMV
!                IFLAG

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "solvprm.fi"
      include "params.fi"
      include "opt.fi"
      include "cshell.fi"

      dimension hvsolv(*), deltamv(*), p2(maxteq), delta(maxteq)
      dimension pdzero(maxteq)
      data pdzero /maxteq * 0.0d0/
      do i = 1, nequals
          p2(i) = pinit(i)
      end do
      do i = 1, nhvpvar
          p2(i+nofpmv) = hvsolv(i)
      end do
      do i = 1, nhvtvar
          p2(i+noftmv) = hvsolv(nhvpvar+i)
      end do
      if (iprtalg.ne.0) then
          if(nhvpvar.gt.0)write (iofilo,*) 'hvac pressures'
          do i = 1, nhvpvar
              write (iofilo,*) i, hvsolv(i)
          end do
          if(nhvtvar.gt.0)write (iofilo,*) 'hvac temperatures'
          do i = 1, nhvtvar
              write (iofilo,*) i, hvsolv(nhvpvar+i)
          end do
      end if
      t = stime
      call resid(t,p2,pdzero,delta,ires,rpar2,ipar2)
      do i = 1, nhvpvar
          deltamv(i) = delta(i+nofpmv)
      end do
      do i = 1, nhvtvar
          deltamv(i+nhvpvar) = delta(i+noftmv)
      end do
      if (iprtalg.ne.0) then
          write (iofilo,*) ' '
          if(nhvpvar.gt.0)write (iofilo,*) 'hvac pressure residuals'
          do i = 1, nhvpvar
              write (iofilo,*) i, deltamv(i)
          end do
          if(nhvtvar.gt.0)write (iofilo,*) 'hvac temperature residuals'
          do i = 1, nhvtvar
              write (iofilo,*) i, deltamv(i+nhvpvar)
          end do
          write(iofilo,*)' '
          pause
      end if
      return
      end

      subroutine gres3(nnn,hvpsolv,deltamv,iflag)

!     routine: gres2
!     purpose: calculates residuals for initial solution by snsqe
!                 this routine finds initial upper layer temperatures,
!                 upper wall and ceiling surface temperatures
!                 in addition to room pressures and hvac pressures and
!                 temperatures.
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments: nnn
!                hvpsolv
!                deltamv
!                iflag

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "solvprm.fi"
      include "params.fi"
      include "cshell.fi"
      include "wnodes.fi"
      include "opt.fi"

      dimension hvpsolv(*), deltamv(*), p2(maxteq), delta(maxteq),
     +pdzero(maxteq)
      data pdzero /maxteq * 0.0d0/
      nalg = nm1 + nhvpvar + nhvtvar

      do i = 1, nequals
          p2(i) = pinit(i)
      end do

      ! copy pressures, hvac pressures and temps
      do i = 1, nalg
          p2(i) = hvpsolv(i)
      end do

      ! copy upper layer temperatures in fire room
      p2(lfbo + noftu) = hvpsolv(1+nalg)

      ! copy wall temperatures
      ii = 0
      ieq1 = izwmap2(1,lfbo)
      ieq2 = izwmap2(3,lfbo)
      if(ieq1.ne.0)then
          ii = ii + 1
          p2(ieq1) = hvpsolv(ii+nalg+1)
      endif
      if(ieq2.ne.0)then
          ii = ii + 1
          p2(ieq2) = hvpsolv(ii+nalg+1)
      endif

      if(iprtalg.ne.0)then
          write(iofilo,*)' *** guesses ***'
          write(iofilo,*)'room pressures'
          do i = 1, nm1
              write(iofilo,'(1x,i3,1x,e23.16)')i,p2(i)
          end do
          write(iofilo,*)'hvac pressure and temperatures'
          do i = nm1+1,nalg
              write(iofilo,'(1x,i3,1x,e23.16)')i,p2(i)
          end do
          ii = 1
          write(iofilo,*)'upper layer temperature in fire room'
          write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,p2(lfbo+noftu)
          ieq1 = izwmap2(1,lfbo)
          ieq3 = izwmap2(3,lfbo)
          if(ieq1.ne.0.or.ieq3.ne.0)then
              write(iofilo,*)'wall temperatures'
              if(ieq1.ne.0)then
                  ii = ii + 1
                  write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,p2(ieq1)
              endif
              if(ieq3.ne.0)then
                  ii = ii + 1
                  write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,p2(ieq3)
              endif
          endif
      endif
      t = stime
      call resid(t,p2,pdzero,delta,ires,rpar2,ipar2)
      do i = 1, nalg
          deltamv(i) = delta(i)
      end do
      do i = 1, nm1
          if(.not.izcon(i))deltamv(i) = 0.0d0
      end do
      deltamv(1+nalg) = delta(lfbo+noftu)
      ii = 0
      if(ieq1.ne.0)then
          ii = ii + 1
          deltamv(ii+1+nalg) = delta(ieq1)
      endif
      if(ieq2.ne.0)then
          ii = ii + 1
          deltamv(ii+1+nalg) = delta(ieq2)
      endif
      if(iprtalg.ne.0)then
          write(iofilo,*)' '
          write(iofilo,*)' *** residuals ***'
          write(iofilo,*)'room pressure'
          do i = 1, nm1
              write(iofilo,'(1x,i3,1x,e23.16)')i,delta(i)
          end do
          write(iofilo,*)'hvac pressure and temperatures'
          do i = nm1+1,nalg
              write(iofilo,'(1x,i3,1x,e23.16)')i,delta(i)
          end do
          write(iofilo,*)'upper layer temperature in fire room'
          write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,delta(lfbo+noftu)
          ieq1 = izwmap2(1,lfbo)
          ieq3 = izwmap2(3,lfbo)
          if(ieq1.ne.0.or.ieq3.ne.0)then
              write(iofilo,*)'wall temperatures'
              if(ieq1.ne.0)then
                  ii = ii + 1
                  write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,delta(ieq1)
              endif
              if(ieq3.ne.0)then
                  ii = ii + 1
                  write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,delta(ieq3)
              endif
          endif
          write(iofilo,*)' '
          pause
      endif
      return
      end

      subroutine hvinit (ierror)

!     routine: hvinit
!     purpose: this routine sets up the arrays needed to for hvac
!                 simulation and initializes temperatures and concentrations
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments: ierror  returns error codes

!     this function has been modified to prevent negative flow.  a max function
!     was inserted just after the calculation off, the flow to do this.  if
!     the flow is allowed to be negative (flow reversal) then this statement
!     must be removed.

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "cshell.fi"

      dimension c3(ns)

!    calculate min & max values for fan curve
      x1 = 1.0d0
      x0 = 0.0d0
      pi = 4.0d0 * atan(x1)
          
      do k = 1, nfan
          f = hvbco(k,1)
          df = x0
          xx = x1
          do j = 2, nfc(k)
              xxjm1 = j - 1
              df = df + xxjm1 * hvbco(k,j) * xx
              xx = xx * hmin(k)
              f = f + hvbco(k,j) * xx
          end do
          ! prevent negative flow
          qmin(k) = max(x0,f)
          dfmin(k) = df
      end do
      do k = 1, nfan
          f = hvbco(k,1)
          df = x0
          xx = x1
          do j = 2, nfc(k)
              xxjm1 = j - 1
              df = df + xxjm1 * hvbco(k,j) * xx
              xx = xx * hmax(k)
              f = f + hvbco(k,j) * xx
          end do
          ! prevent negative flow
          qmax(k) = max(x0,f)
          dfmax(k) = df
      end do

      ! if there are no connections between the hvac system and the
      ! outside world, we do not need to go any further
      if (next.le.0) return

      ! arrange data on node basis
      do i = 1, nnode
          k = 0
          do ib = 1, nbr
              if (i.eq.na(ib)) then
                  k = k + 1
                  icmv(i,k) = ib
                  mvintnode(i,k) = ne(ib)
              else if (i.eq.ne(ib)) then
                  k = k + 1
                  icmv(i,k) = ib
                  mvintnode(i,k) = na(ib)
              end if
          end do
          ncnode(i) = k
      end do

      ! check interior nodes
      do i = 1, nnode
          if (ncnode(i).lt.1.or.ncnode(i).gt.mcon) then
              call xerror('hvinit - interior node has too many ',
     .        'or too few connections',0,1,1)
              ierror = 223
              return
          end if
      end do

      ! limit the range of hvelxt and set the absolute height of the interior node
      do ii = 1, next
          i = hvnode(1,ii)
          j = hvnode(2,ii)
          if (ncnode(j).gt.1) then
              ierror = 223
              return
          endif
          hvelxt(ii) = min(hr(i),max(x0,hvelxt(ii)))
          hvght(j) = hvelxt(ii) + hflr(i)
      end do

      ! assign compartment pressure & temperature data to exterior nodes of the hvac network
      do i = 1, nnode
          hvp(i) = -1.0d0
      end do
      do i = 1, nbr
          hvdara(i) = x0
          hvdvol(i) = x0
          hvconc(i,1) = -x1
          tbr(i) = -x1
      end do

      s1 = x0
      s2 = x0
      do lsp = 1, ns
          c3(lsp) = x0
      end do
      do ii = 1, next
          i = hvnode(1,ii)
          j = hvnode(2,ii)
          ib = icmv(j,1)
          ! the outside is defined to be at the base of the structure for mv
          if (i.lt.n) then
              hvextt(ii,upper) = tamb(i)
              hvextt(ii,lower) = tamb(i)
              hvp(j) = zzrelp(i) - hvgrav * ramb(i) * hvelxt(ii)
          else
              hvextt(ii,upper) = exta
              hvextt(ii,lower) = exta
              hvp(j) = expa - hvgrav * exra * hvelxt(ii)
          end if
          tbr(ib) = hvextt(ii,upper)
          s1 = s1 + hvp(j)
          s2 = s2 + tbr(ib)
          do lsp = 1, ns
              ! the outside is defined to be at the base of the structure for mv
              if (i.lt.n) then
                  hvexcn(ii,lsp,upper) = o2n2(lsp) * ramb(i)
                  hvexcn(ii,lsp,lower) = o2n2(lsp) * ramb(i)
              else
                  hvexcn(ii,lsp,upper) = o2n2(lsp) * exra
                  hvexcn(ii,lsp,lower) = o2n2(lsp) * exra
              end if
              hvconc(j,lsp) = hvexcn(ii,lsp,upper)
              c3(lsp) = c3(lsp) + hvexcn(ii,lsp,upper)
          end do
      end do
      
      ! this is to initialize the nodes and branches to something
      ! we will then let the system equilibrate to give us the true answer
      xnext = next
      pav = s1 / xnext
      tav = s2 / xnext
      do lsp = 1, ns
          c3(lsp) = c3(lsp) / xnext
      end do
      do i = 1, nnode
          if (hvp(i).lt.x0) then
              hvp(i) = pav
          end if
      end do
      do i = 1, nbr
          if (tbr(i).le.x0) tbr(i) = tav
          if (hvconc(i,1).lt.x0) then
              do lsp = 1, ns
                  hvconc(i,lsp) = c3(lsp)
              end do
          end if
      end do

      ! calculate area, relative roughness, effective diameter and volume of ducts
      ! volume and roughness
      do id = 1, ndt
          da(id) = (pi*de(id)**2) / 4.0d0
          rr(id) = ductar(id) / de(id)
          ib = ibrd(id)
          hvdvol(ib) = hvdvol(ib) + da(id) * dl(id)
          hvdara(ib) = hvdara(ib) + pi*de(id)*dl(id)
      end do


      ! construct hvmap arrays
      call hvmap

      ! define total mass for each hvac system
      do isys = 1, nhvsys
          hvtm(isys) = x0
      end do     
      do ib = 1, nbr
          isys = izhvbsys(ib)
          rden = (pofset+pav)/(hvrgas*tbr(ib))
          hvtm(isys) = hvtm(isys) + rden*hvdvol(ib)
      end do

      ! now that everything is ok, we can turn on ventilation
      mvcalc = .true.
      return
      end

      subroutine hvmap

!     routine: hvmap
!     purpose: this routine maps all the hvac nodes into a single mapping array for dassl and creates
!              a mapping from those to exterior ones
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments: 

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"

      dimension istack(100)

      ! construct the array that maps between interior nodes (nodes that dassl solves for) and the entire node array
      do i = 1, nnode
          izhvmapi(i) = i
      end do

      ! DASSL only solve interior nodes so zero out exterior nodes
      do ii = 1, next
          i = hvnode(2,ii)
          izhvmapi(i) = 0
      end do

      ! and fill in the holes vacated by the exterior nodes
      ii = 0
      do i = 1, nnode
          if (izhvmapi(i).ne.0) then
              ii = ii + 1
              izhvmapi(ii) = izhvmapi(i)
          end if
      end do

      ! construct inverse of izhvmapi
      do i = 1, nnode
          izhvmape(i) = -1
      end do
      do i = 1, nnode - next
          izhvmape(izhvmapi(i)) = i
      end do

      ! construct array that maps between all nodes and exterior nodes
      do i = 1, nnode
          izhvie(i) = 0
      end do
      do ii = 1, next
          i = hvnode(2,ii)
          izhvie(i) = ii
      end do

      ! construct array that maps between all nodes and hvac system number to which they belong
      do i = 1, nnode
          izhvsys(i) = 0
      end do
      icursys = 0
      iptr = 0
   90 continue
      icurnod = 0
      do i = 1, nnode
          if (izhvsys(i).eq.0) then
              icurnod = i
              exit
          end if
      end do
      if (icurnod.ne.0) then
          icursys = icursys + 1
          iptr = iptr + 1
          istack(iptr) = icurnod
  120     continue
          if (iptr.eq.0) go to 90
          icurnod = istack(iptr)
          iptr = iptr - 1
          izhvsys(icurnod) = icursys
          do j = 1, ncnode(icurnod)
              nxtnode = mvintnode(icurnod,j)
              if (izhvsys(nxtnode).eq.0) then
                  iptr = iptr + 1
                  istack(iptr) = nxtnode
              end if
          end do
          go to 120
      end if
      nhvsys = icursys

      ! we have to update nequals.  nequals was originally defined in 
      ! offset but offset was called before nhvsys was defined.
      nequals = nofhvpr + nhvsys*nlspct

      do i = 1, nnode
          isys = izhvsys(i)
          do j = 1, ncnode(i)
              ib = icmv(i,j)
              izhvbsys(ib) = isys
          end do
      end do
      return
      end

      SUBROUTINE INITAMB(YINTER,IFLAG)

!     routine: hvmap
!     purpose: this routine computes initializations for varialbes
!     related to ambient conditions.  when iflag=1 the array
!     yinter is used to compute upper layer volumes.  otherwise,
!     upper layer volumes are not computed.  if iflag is set to 1
!     then yinter must be a floating point array of at least size nr
!     (nr = number of rooms) in the calling routine.
!     revision: $revision: 352 $
!     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
!     arguments: yinter, iflag

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "fltarget.fi"
      include "opt.fi"

      dimension yinter(*), dummy(1)

      xx0 = 0.0d0
      xx2 = 2.0d0

      ! simplify and make initial pressure calculations consistent.  Inside pressures
      ! were calculated using rho*g*h .  But outside pressures were calculated using
      ! ATMOSP.  Fictional flows resulted making  SNSQE work a log harder to get
      ! an initial solution.  The initial temperature values calculated by ATMOSP
      ! at the top of the empire state building (about 400 M above base) is only
      ! about 0.2 K different that at the base.  
      do i = 1, nm1
          pamb(i) = -ra*g*hflr(i)
          tamb(i) = ta
          ramb(i) = ra
          epa(i) = -exra*g*hflr(i)
          eta(i) = exta
          era(i) = exra
      end do
      eta(n) = exta
      era(n) = exra
      epa(n) = xx0


      ! normalize pressures so that the smallest pressure is zero
      xxpmin = pamb(1)
      do i = 1, nm1
          xxpmin = min(xxpmin,pamb(i),epa(i))
      end do
      do i = 1, nm1
          epa(i) = epa(i) - xxpmin
          pamb(i) = pamb(i) - xxpmin
      end do
      pofset = pofset + xxpmin
      pa = pa + xxpmin - pofset
      expa = expa + xxpmin - pofset

      ! copy all of the variables from the initial values into the data arrays
      call datacopy(dummy,constvar)

      ! define the p array, the solution to the ode
      do i = 1, nm1
          p(i) = pamb(i)
          p(i+noftu) = tamb(i)

          ! check for a special setting of the interface height
          if (iflag.eq.1) then
              if (yinter(i).lt.0.d0) then
                  p(i+nofvu) = zzvmin(i)
              else
                  p(i+nofvu) = 
     .            min(zzvmax(i),max(zzvmin(i),yinter(i)*ar(i)))
              end if
              yinter(i) = xx0
          end if
          if(izshaft(i).eq.1)p(i+nofvu) = zzvmax(i)
          p(i+noftl) = tamb(i)
      end do

      ! define hvac pressures and temperatures.  these values are later refined by 
      ! snsqe so that each hvac node conserves mass and energy
      do i = 1, nhvpvar
          p(i+nofpmv) = xx0
      end do
      do i = 1, nhvtvar
          p(i+noftmv) = tamb(1)
      end do

      ! define interior surface wall temperatures
      ii =nofwt 
      do i = 1, nm1
          do iwall = 1, nwal
              if (switch(iwall,i)) then
                  ii = ii + 1
                  p(ii) = tamb(i)
              end if
          end do
      end do

      ! establish default values for detector data
      do i = 1, ndtect
          iroom=ixdtect(i,droom)
          if(xdtect(i,dxloc).lt.0.0d0)xdtect(i,dxloc)=br(iroom)*.5d0
          if(xdtect(i,dyloc).lt.0.0d0)xdtect(i,dyloc)=dr(iroom)*.5d0
          if(xdtect(i,dzloc).lt.0.0d0)then
              xdtect(i,dzloc)=hrp(iroom)+xdtect(i,dzloc)
          endif
          tdspray = xdtect(i,dspray)

          ! if tdspray>0 then interpret it as a spray density and convert
          ! to a characteristic quenching time
          ! if tdspray < 0 then interpret abs(tdspray) as the time
          ! required to reduce the fire size by 50 per cent
          ! if tdspray = 0 then turn the sprinkler off
          if(tdspray.gt.0.0d0)then
              tdrate = 3.0d0/tdspray**1.8d0
          elseif(tdspray.lt.0.0d0)then
              tdrate = abs(tdspray)/log(xx2)
              tdspray = (3.0d0/tdrate)**(1.0d0/1.8d0)
          else
              tdspray = 0.0d0
              tdrate = 1.0d10
              ixdtect(i,dquench) = 0
          endif

          ! set initial ceiling jet and detector link temperatures to ambient
          xdtect(i,dspray) = tdspray
          xdtect(i,dthalf) = tdrate*log(xx2)
          xdtect(i,drate) = tdrate
          xdtect(i,dtemp) = tamb(iroom)
          xdtect(i,dtempo) = tamb(iroom)
          xdtect(i,dtjet) = tamb(iroom)
          xdtect(i,dtjeto) = tamb(iroom)
      end do

      call sortbrm(xdtect,mxdtect,ixdtect,mxdtect,
     .ndtect,dtxcol,dticol,droom,nr,nm1,idtpnt)

      ! p's for pressure, volume and temperature are defined
      ! we can now copy these values to the environment variables
      call datacopy (p, odevara)

      ! initialize target temperatures
      do itarg = 1, ntarg
          iroom = ixtarg(trgroom,itarg)
          if(ixtarg(trgmeth,itarg).eq.mplicit)then
              ieq = iztarg(itarg)
              p(noftt+ieq) = tamb(iroom)
          endif  
          do i=trgtempf,trgtempb
              xxtarg(i,itarg)=tamb(iroom)
          end do
          tgtarg(itarg) = tamb(iroom)

          ! scale normal vectors to have length 1
          scale = 1.0d0/dnrm2(3,xxtarg(trgnormx,itarg),1)
          call dscal(3,scale,xxtarg(trgnormx,itarg),1)
      end do

      ! initialize solver oxygen values if required.   (must be initialized
      ! after zzmass is defined)
      if(option(foxygen).eq.on)then
          do iroom = 1, nm1
              p(iroom+nofoxyu)=0.23d0*zzmass(iroom,upper)
              p(iroom+nofoxyl)=0.23d0*zzmass(iroom,lower)
          end do
      endif

      ! define ihxy in izhall (dimension that is longest)
      do i = 1, nm1
          if(izhall(i,ihroom).eq.1)then
              if(dr(i).gt.br(i))then
                  izhall(i,ihxy) = 1
              else
                  izhall(i,ihxy) = 2
              endif
          endif
      end do

      return
      end

      SUBROUTINE INITMM
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITMM
C
C     Source File: INITMM.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine initializes the main memory - must be used by 
C                   all modules that will run the model kernel
C
C     Arguments: none
C
C     Revision History:
C        Created:  06/20/1990 at 9:57 by WWJ
C        Modified: 05/15/1991 at 10:27 by WWJ:
C                  initialize vertical flow data
C        Modified: 11/30/1992 at 10:56 by RDP:
C                  Set MINMAS to 0.0 (with change in TOXIC to eliminate its
C                  need)
C        Modified: 3/12/1993 at 9:36 by RDP:
C                  Moved object initialization to new routine INITOB
C        gpf 10/14/93 initialized variables involved in the 
C                     detection/suppression option
C        gpf 2/25/94 initialized variables involved in the
C                    room to room heat transfer option
C        Modified: 6/10/1994 by GPF:
C                  added shaft option (combine two layers into one)
C        Modified: 10/10/1994 by gpf:
C                  initialize ntarg, a target counter
C        Modified: 4/26/95
C                  give default values for target data structures, corrected
C                  value of sigma
C        Modified: 7/13/95
C                  defined JACCOL, NEQOFF for use in the new Jacobian speedup
C                  option.
C        Modified: 9/12/96
C                  initialized IXDTECT(I,DQUENCH) to 0 rather than 1 .  That is,
C                  sprinklers will now not go off by default.  
C        Modified: 7/22/96
C                  Initialized variables for use by hall option
C        Modified: 11/25/96
C                  Initialized variables used by non-rectangular room option
C        Modified: 4/21/97 by gpf
C                  Added BLACK option for the gas to allow better simulations
C                  for furnaces
C        Modified: 7/18/97 by par
C                  Initialized GUISELCT for gui graphics output
C        Modified: 10/21/97 by gpf
C                  Changed default fire size to 0 to be consistent with FAST
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cenviro.fi"
      include "cfin.fi"
      include "params.fi"
      include "thermp.fi"
      include "fltarget.fi"
      include "vents.fi"

C     SET SOME INITIALIZATION - SIMPLE CONTROL STUFF

      EXSET = .FALSE.
      debugging = .false.
      XX0 = 0.0D0
      xx1 = 1.0d0
      xm1 = -1.0d0

C     INITIALIZE THE COMMON BLOCK

      DO 10 I = 1, NS
          O2N2(I) = XX0
          ALLOWED(I) = .FALSE.
          ACTIVS(I) = .TRUE.
   10 CONTINUE
      DO 30 I = 1, NR
          DO 20 J = 1, NWAL
              CNAME(J,I) = 'OFF'
              SWITCH(J,I) = .FALSE.
   20     CONTINUE
   30 CONTINUE
      DO 40 I = 1, NR
          SWITCH(1,I) = .TRUE.
          CNAME(1,I) = 'DEFAULT'
   40 CONTINUE
      NCONFG = 0
      NDUMPR = 0
      NLSPCT = 0
      NRESTR = 0
      NUMTHRM = 0
      MAPLTW(1) = 1
      MAPLTW(2) = 2
      MAPLTW(3) = 1
      MAPLTW(4) = 2
      HCLDEP = 0
      SMKAGL = 0
      N = 0
      DO 50 I = 1, NWAL + 1
          CJETON(I) = .FALSE.
   50 CONTINUE

C     INITIALIZE THE FLOW VARIABLES

      DO 80 I = 1, NR
          IZSHAFT(I) = 0
          HEATUP(I) = XX0
          HEATLP(I) = XX0
          HEATVF(I) = XX0
          DO 70 J = 1, NR
C     DO VERTICAL VENTS (VVENT,...)
              VSHAPE(I,J) = 0
              NWV(I,J) = 0
              VVAREA(I,J) = XX0
C     DO HORIZONTAL VENTS (HVENT,...)
              NW(I,J) = 0
              NEUTRAL(I,J) = 0
   70     CONTINUE
   80 CONTINUE

      DO 60 IVENT = 1, MXVENTS
          SS1(IVENT) = XX0
          SS2(IVENT) = XX0
          SA1(IVENT) = XX0
          SA2(IVENT) = XX0
          AS1(IVENT) = XX0
          AS2(IVENT) = XX0
          AA1(IVENT) = XX0
          AA2(IVENT) = XX0
          SAU1(IVENT) = XX0
          SAU2(IVENT) = XX0
          ASL1(IVENT) = XX0
          ASL2(IVENT) = XX0
   60 CONTINUE

      do i = 1, mext
          hveflot(upper,i) = xx0
          hveflot(lower,i) = xx0
          tracet(upper,i) = xx0
          tracet(lower,i) = xx0
      end do

C     INITIALIZE THE FORCING FUNCTIONS

      DO 100 I = 1, NR
          EMP(I) = XX0
          EMS(I) = XX0
          EME(I) = XX0
          APS(I) = XX0
          DO 90 K = UPPER, LOWER
              QR(K,I) = XX0
              QC(K,I) = XX0
              QFC(K,I) = XX0
   90     CONTINUE
  100 CONTINUE
      DO 110 I = 1, MXFIRE
          QFR(I) = XX0
  110 CONTINUE
      do i = 1, maxteq
          p(i) = xx0
      end do

C     DEFINE THE OUTSIDE WORLD AS INFINITY

      XLRG = 1.D+5
      DO 150 I = 1, NR
          DR(I) = XLRG
          BR(I) = XLRG
          HR(I) = XLRG
          HRP(I) = XLRG
          HRL(I) = XX0
          HFLR(I) = XX0
          CXABS(I) = XX0
          CYABS(I) = XX0
          AR(I) = BR(I) * DR(I)
          VR(I) = HR(I) * AR(I)
          DO 120 J = 1, NWAL
              EPW(J,I) = XX0
              QSRADW(J,I) = XX0
              QSCNV(J,I) = XX0
  120     CONTINUE
          DO 140 J = 1, NR
              NW(I,J) = 0
  140     CONTINUE
  150 CONTINUE
      DO 130 IVENT = 1, MXVENTS
          BW(IVENT) = XX0
          HH(IVENT) = XX0
          HL(IVENT) = XX0
          HHP(IVENT) = XX0
          HLP(IVENT) = XX0
          VFACE(IVENT) = 1
  130 CONTINUE

C     SET THE TIME STEP AND INNER STEP DIVISION FOR TIME SPLITTING
C     WE DO NOT LET THE USER CHOOSE THESE

      DELTAT = 1.0D0

C     DEFINE ALL THE "UNIVERSAL CONSTANTS

      SIGM = 5.67D-8
      CP = 1012.0D0
      GAMMA = 1.40D0
      RGAS = (GAMMA-1.0D0) / GAMMA * CP
      MINMAS = 0.0D0
      G = 9.80D0
      STIME = XX0
      TREF = 288.D0
      LIMO2 = 0.10D0
      GMWF = 16.0D0
      HCOMBA = 50000000.0D0
      PREF = 1.013D+5
      PA = PREF
      POFSET = PREF
      SAL = XX0
      SAL2 = -1.0D0
      TE = TREF
      TA = TREF
      TGIGNT = TE + 200.D0
      EXTA = TA
      EXPA = PA
      EXSAL = SAL
      WINDV = XX0
      WINDRF = 10.D0
      WINDPW = 0.16D0
      do i = 0, mxfire
          objmaspy(i) = xx0
          radio(i) = xx0
          radconsplit(i) = 0.15d0
      end do
      tradio = xx0
      QRADRL = 0.15D0

C     NORMAL AIR

      O2N2(1) = 0.77D0
      O2N2(2) = 0.23D0

C     A SPECIFIED FIRE IN THE CENTER OF THE ROOM

      LFBT = 2
      LFBO = 0
      LFMAX = 1
      heatfl = .false.
      heatfq = 0.0
      heatfp(1) = xm1
      heatfp(2) = xm1
      heatfp(3) = xm1

C     SET TO -1 AS A FLAG FOR NPUTP INITIALIZATION - ANY VALUE NOT SET
C     WILL BE SET TO THE DEFAULT WHICH IS THE CENTER OF THE RESPECTIVE WALL

      FPOS(1) = xm1
      FPOS(2) = xm1
      FPOS(3) = xm1

C     SET UP DEFAULT VALUES FOR THE CHEMISTRY

      DO 190 I = 1, NV

C     DEFINE THE VENTS AS BEING OPEN

          DO 180 IVENT=1, MXVENTS
              QCVENT(IVENT,I) = 1.0D0
  180     CONTINUE
          TFIRED(I) = 86400.D0
          HFIRED(I) = XX0
          AFIRED(I) = XX0
          BFIRED(I) = 0.000D0
          QFIRED(I) = BFIRED(I) * HCOMBA
          HCRATIO(I) = 0.3333333D0
          HOCBMB(I) = HCOMBA
          COCO2(I) = XX0
          CCO2(I) = XX0
  190 CONTINUE

!	Start with vents open: h for hvent, v for vvent, and m for mvent

      do 191 i = 1,mxvents
          qcvh(1,i) = xx0
          qcvh(2,i) = xx1
          qcvh(3,i) = xx0
  191     qcvh(4,j) = xx1

          do 193 i = 1, nr
              qcvv(1,i) = xx0
              qcvv(2,i) = xx1
              qcvv(3,i) = xx0
  193         qcvv(4,i) = xx1

!	Note that the fan fraction is unity = on, whereas the filter fraction is unity = 100% filtering
!	Since there is not "thing" associated with a filter, there is no (as of 11/21/2006) 
!		way to have an intial value other than 0 (no filtering).
              do 194 i = 1, mfan
                  qcvf(1,i) = xx0
                  qcvf(2,i) = xx0
                  qcvf(3,i) = xx0
                  qcvf(4,i) = xx0
                  qcvm(1,i) = xx0
                  qcvm(2,i) = xx1
                  qcvm(3,i) = xx0
  194             qcvm(4,i) = XX1
  192             continue

                  HCRATT = HCRATIO(1)

C     TURN HVAC OFF INITIALLY

                  NNODE = 0
                  NFT = 0
                  NFAN = 0
                  nfilter = 0
                  NBR = 0
                  NEXT = 0
                  HVGRAV = G
                  HVRGAS = RGAS
                  MVCALC = .FALSE.
                  DO 200 I = 1, MNODE
                      HVGHT(I) = XX0
  200             CONTINUE

C*** INITIALIZE DETECTORS

                  DO 210 I = 1, MXDTECT
                      XDTECT(I,DRTI) = 50.0D0
                      XDTECT(I,DSPRAY) = -300.D0
                      XDTECT(I,DXLOC) = -1.0D0
                      XDTECT(I,DYLOC) = -1.0D0
                      XDTECT(I,DZLOC) = -3.0D0/39.37D0
                      XDTECT(I,DTRIG) = 330.3722D0
                      XDTECT(I,DVEL) = 0.D0
                      XDTECT(I,DVELO) = 0.D0
                      XDTECT(I,DTACT) = 99999.D0
                      IXDTECT(I,DTYPE) = 2
                      IXDTECT(I,DROOM) = 1
                      IXDTECT(I,DQUENCH) = 0
                      IXDTECT(I,DACT) = 0
  210             CONTINUE
                  NDTECT = 0
                  DO 220 I = 1, NR
                      IQUENCH(I) = 0
  220             CONTINUE

C*** initialize room to room heat transfer data structures

                  NSWAL = 0

C*** initialize target counter

                  NTARG = 0

                  DO 230 ITARG = 1, MXTARG
                      IXTARG(TRGMETH,ITARG) = XPLICIT
                      IXTARG(TRGEQ,ITARG) = PDE
                      IXTARG(TRGBACK,ITARG) = INT
                      CXTARG(ITARG) = 'DEFAULT'
  230             CONTINUE

C*** initialize JACCOL  

                  JACCOL = -2
                  NEQOFF = 10

C*** initialize hall start time

                  DO 240 I = 1, NR
                      ZZHALL(I,IHTIME0) = -1.0D0
                      ZZHALL(I,IHVEL) = -1.0D0
                      ZZHALL(I,IHDEPTH) = -1.0D0
                      ZZHALL(I,IHMAXLEN) = -1.0D0
                      ZZHALL(I,IHHALF) = -1.0D0
                      ZZHALL(I,IHTEMP) = 0.0D0
                      ZZHALL(I,IHORG) = -1.0D0
                      IZHALL(I,IHDEPTHFLAG) = 0
                      IZHALL(I,IHHALFFLAG) = 0
                      IZHALL(I,IHMODE) = IHAFTER
                      IZHALL(I,IHROOM) = 0
                      IZHALL(I,IHVELFLAG) = 0
                      IZHALL(I,IHVENTNUM) = 0
                      IZHALL(I,IHXY) = 0
                      DO 250 IVENT = 1, MXVENT
                          ZZVENTDIST(I,IVENT) = -1.
  250                 CONTINUE
  240             CONTINUE
                  UPDATEHALL = .FALSE.

                  DO 260 I = 1, NR
                      DO 260 J = 1, NR
                          DO 260 K = 1, 4
                              IJK(I,J,K) = 0
  260             CONTINUE
                  NVENTIJK = 0

                  DO 270 I = 1, NR
                      IZRVOL(I) = 0
                      DO 280 J = 1, MXPTS
                          ZZRVOL(J,I)=0.0D0
                          ZZRAREA(J,I)=0.0D0
                          ZZRHGT(J,I)=0.0D0
  280                 CONTINUE
  270             CONTINUE

C*** initialzie time step checking

                  ZZDTCRIT = 1.0D-09
                  IZDTNUM = 0
                  IZDTMAX = 100
                  IZDTFLAG = .TRUE.

C*** initialize inter-compartment heat transfer fractions

                  DO 300 I = 1, NR
                      DO 310 J = 1, NR
                          ZZHTFRAC(I,J) = 0.0D0
  310                 CONTINUE
  300             CONTINUE

                  do 320 j = 0, nr
                      izheat(j) = 0
                      do 330 i = 1, nr
                          izhtfrac(i,j) = 0
  330                 continue
  320             continue

                  do lsp = 1, ns
                      do j = upper, lower
                          do i = 1, nr
                              zzgspec(i,j,lsp) = xx0
                              zzcspec(i,j,lsp) = xx0            
                          end do
                      end do
                  end do

C     initialize number of furnace temperature nodes

                  nfurn=0

                  RETURN
      END

      SUBROUTINE INITOB
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITOB
C
C     Source File: INITOB.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Initialize object data
C
C     Arguments: none
C
C     Revision History:
C        Created:  3/12/1993 at 9:34 by RDP
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "objects1.fi"
      include "objects2.fi"
C
C     TURN OFF OBJECTS
C
      NUMOBJL = 0
      DO 10 I = 0, MXOIN
          OBJON(I) = .FALSE.
          OBJPOS(1,I) = -1.0
          OBJPOS(2,I) = -1.0
          OBJPOS(3,I) = -1.0
          OBJRM(I) = 0
          OBJNIN(I) = ' '
          OBJLD(I) = .FALSE.
          OBJPNT(I) = 0
          OBJCRI(1,I) = 0.0
          OBJCRI(2,I) = 0.0
          OBJCRI(3,I) = 0.0
          OBJDEF(I) = .FALSE.
          ODBNAM(I) = ' '
   10 CONTINUE
      RETURN
      END

      SUBROUTINE INITSLV
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITSLV
C
C     Source File: INITSLV.SOR
C
C     Functional Class:  
C
C     Description:  
C
C     Arguments: 
C
C     Revision History:
C        Created:  11/28/1992 at 9:57 by WWJ
C        Modified: 06/14/1992 at 10:28 by GPF:
C                  Added parameters for new initialization
C                  heuristic and various parameters for hvac
C                  (error tolerances, convective loss through
C                  duct walls)
C        Modified: 2/3/93 by GPF
C                  Added parameters in support of the reduced Jacobian
C                  option.  Added debug print option to print out
C                  SNSQE progress
C        Modified: 6/30/95 by GPF
C                  Reallocated points in wall, adding more points to the rear.
C                  Added oxygen dassl option flag.
C        Modified: 10/16/97 by gpf
C                  made STPMAX initializations consistent
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      use iofiles
      include "precis.fi"
      include "cfast.fi"
      include "opt.fi"
      include "wnodes.fi"
      include "solvprm.fi"
      include "cfin.fi"
      include "cshell.fi"
      include "params.fi"

      LOGICAL EXISTED

C     THIS GUY IS IN UNLABELED COMMON SO WE CAN NOT PUT IT INTO A DATA STATEMENT
      DUCTCV = 0.0D0

      INQUIRE (FILE=solverini,EXIST=EXISTED)
      IF (.NOT.EXISTED) return
      CLOSE (IOFILI)
      write (logerr, 1) solverini
      open (unit=iofili,file=solverini)

C*** READ IN SOLVER ERROR TOLERANCES

      READ (IOFILI,*)
      READ (IOFILI,*) APTOL, RPTOL, ATOL, RTOL
      READ (IOFILI,*)
      READ (IOFILI,*) AWTOL, RWTOL, ALGTOL
      READ (IOFILI,*)
      READ (IOFILI,*) AHVPTOL, RHVPTOL, AHVTTOL, RHVTTOL

C     READ IN PHYSICAL SUB-MODEL OPTION LIST

      READ (IOFILI,*)
      READ (IOFILI,*) NOPT
      NOPT = MAX(0, MIN(MXOPT, NOPT))
      DO 10 I = 1, (NOPT-1) / 5 + 1
          IBEG = 1 + (I-1) * 5
          IEND = MIN(IBEG+4,NOPT)
          READ (IOFILI,*)
          READ (IOFILI,*) (OPTION(J),J = IBEG,IEND)
   10 CONTINUE
C     SINCE THE SOLVER.INI FILE IS ON, TURN ON DEBUG HELP
      OPTION(FKEYEVAL) = 1

C     SET DEBUG PRINT

      IF (OPTION(FDEBUG).EQ.2) THEN
          OPTION(FDEBUG) = OFF
          SWITCH(1,NR) = .TRUE.
      ELSE IF (OPTION(FDEBUG).GE.3) THEN
          OPTION(FDEBUG) = ON
          SWITCH(1,NR) = .TRUE.
      END IF

C     READ IN WALL INFO

      READ (IOFILI,*)
      READ (IOFILI,*) NWPTS, FRACT1, FRACT2, FRACT3
      READ (IOFILI,*)
      READ (IOFILI,*) IWBOUND
      FSUM = ABS(FRACT1) + ABS(FRACT2) + ABS(FRACT3)
      WSPLIT(1) = ABS(FRACT1) / FSUM
      WSPLIT(2) = ABS(FRACT2) / FSUM
      WSPLIT(3) = ABS(FRACT3) / FSUM

C     READ IN MAXIMUM DESIRED SOLVE STEP SIZE, 
C     IF NEGATIVE THEN THEN SOLVE WILL DECIDE

      READ (IOFILI,*)
      READ (IOFILI,*) STPMAX, DASSLFTS

C*** read in hvac convection coefficient

      READ(IOFILI,*)
      READ(IOFILI,*) DUCTCV

C*** READ IN JACOBIAN AND SNSQE PRINT FLAGS

      READ(IOFILI,*)
      READ(IOFILI,*) JACCHK, CUTJAC, IPRTALG
      CLOSE (IOFILI)

      RETURN

    1 FORMAT ('***** Modify dassl tolerances with ',a256)
      END

      BLOCKDATA INITSLVB

C     THIS INITIALIZES THE SOLVER VARIABLES

          include "precis.fi"
          include "cparams.fi"
          include "opt.fi"
          include "wnodes.fi"
          include "solvprm.fi"
          include "cfin.fi"
          include "cshell.fi"
          include "params.fi"

C     ABS PRESSURE TOL, REL PRESSURE TOL, ABS OTHER TOL, REL OTHER TOL
          DATA APTOL, RPTOL, ATOL, RTOL/1.0D-6, 1.0D-6, 1.0D-5, 1.0D-5/
C     ABS WALL TOL, REL WALL TOL
          DATA AWTOL, RWTOL, ALGTOL/1.0D-2, 1.0D-2, 1.0D-8/
C     ABS HVAC PRESS, REL HVAC PRESS, ABS HVAC TEMP, REL HVAC TEMP
          DATA AHVPTOL,RHVPTOL,AHVTTOL,RHVTTOL/2*1.0D-6,2*1.0D-5/
C     OPTIONS FIRE, HFLOW, ENTRAIN, VFLOW, CJET, DOOR-FIRE, CONVEC, RAD,
          DATA (OPTION(J),J=1,21)/ 2, 1, 1, 1, 2, 1, 1, 2, 
C         CONDUCT, DEBUG, EXACT ODE,  HCL , MFLOW, KEYBOARD, 
     +    1,     0,     1,         1,     1,      1,
C         TYPE OF INITIALIZATION,   MV HEAT LOSS, MOD JAC, DASSL DEBUG
     +    1,                       0,          1,     0,
C         OXYGEN DASSL SOLVE, BACK TRACK ON DTECT,  BACK TRACK ON OBJECTS
     .    0,                       0,                 0    /
C     NUMBER OF WALL NODES, FRACTIONS FOR FIRST, MIDDLE AND LAST WALL SLAB
          DATA NWPTS /30/
C     BOUNDARY CONDITION TYPE (1=CONSTANT TEMPERATURE, 2=INSULATED 3=FLUX)
          DATA IWBOUND /3/
C     COMPUTED VALUES FOR BOUNDARY THICKNESS
          DATA (WSPLIT(J),J=1,3)  /0.50, 0.17, 0.33/
C     TURN DEBUGGING OPTIONS OFF - THIS IS NOT CURRENTLY USED
          DATA DEBUG /MXOPT*0/
C     MAXIMUM STEP SIZE, IF NEGATIVE THEN SOLVER WILL DECIDE
          DATA STPMAX /1.0D0/, DASSLFTS/0.005D0/
C
          DATA JACCHK/0/, CUTJAC/0.0D0/, IPRTALG/0/
      END

      SUBROUTINE INITSPEC
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITSPEC
C
C     Source File: INITSPEC.SOR
C
C     Functional Class:  
C
C     Description:  This routine initializes variables associated with 
C         species it originally occured in CFAST and INITFS.  It was moved
C         to one subroutine to make maintenance easier
C
C     Arguments: 
C
C     Revision History:
C     July 26, 1990 modified mapping to eliminate referene to outside room
c     June 14, 1992 added initialization for species occuring in HVAC systems
c     4/24/95  removed reference to xx0 to eliminate flint complaint
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "params.fi"
      include "thermp.fi"
      include "cenviro.fi"
      include "wnodes.fi"
C
      DIMENSION XM(2)
C
C     WE MUST SET THE NITROGEN/OXYGEN FOR AMBIENT AIR
C
C     NOTE THAT THE FIRST CALL TO "TOXIC" IS WITH ZERO INTERVAL
C     THIS IS JUST TO SET THE PPM AND PPMDV VALUES FOR OUTPUT
C     IT SERVES NO REAL PURPOSE
C
      DO 50 I = 1, NM1
          XM(1) = RAMB(I) * ZZVOL(I,UPPER)
          XM(2) = RAMB(I) * ZZVOL(I,LOWER)
C
C     SET THE WATER CONTENT TO RELHUM - THE POLYNOMIAL FIT IS TO (T-273), AND
C     IS FOR SATURATION PRESSURE OF WATER.  THIS FIT COMES FROM THE STEAM
C     TABLES IN THE HANDBOOK OF PHYSICS AND CHEMISTRY.  WE ARE BEING CLEVER
C     HERE.  THE FINAL RESULT IN O2N2 SHOULD BE THE VALUE USED IN STPORT FOR
C     THE OUTSIDE AMBIENT.
C
          XT = TAMB(I)
          XTEMP = 23.2D0 - 3.816D3 / (XT-46.D0)
          XH2O = EXP(XTEMP) / 101325.0D0 * (18.D0/28.4D0)
          O2N2(8) = RELHUM * XH2O
C
C     NORMALIZE THE ATMOSPHERE
C
          TOTO2N2 = 0.0D0
          DO 10 J = 1, NS
              TOTO2N2 = TOTO2N2 + O2N2(J)
   10     CONTINUE
          DO 20 J = 1, NS
              O2N2(J) = O2N2(J) / TOTO2N2
   20     CONTINUE
C
          DO 40 K = UPPER, LOWER
              DO 30 LSP = 1, NS
                  TOXICT(I,K,LSP) = 0.0D0
                  MASS(K,I,LSP) = O2N2(LSP) * XM(K)
   30         CONTINUE
   40     CONTINUE
   50 CONTINUE

      ISOF = NOFPRD
      DO 60 LSP = 1, NS
          IF (ACTIVS(LSP)) THEN
              DO 70 I = 1, NM1
                  DO 80 K = UPPER, LOWER
                      ISOF = ISOF + 1
                      P(ISOF) = MASS(K,I,LSP) + MINMAS
   80             CONTINUE
   70         CONTINUE
          END IF
   60 CONTINUE

C     HVINIT DEFINE INITIAL PRODUCTS FOR HVAC SYSTEMS

      IF(NHVSYS.NE.0)THEN
          ISOF = NOFHVPR
          DO 220 LSP = 1, MIN(NS,9)
              IF(ACTIVS(LSP))THEN
                  DO 230 ISYS = 1, NHVSYS
                      ISOF = ISOF + 1
                      P(ISOF) = O2N2(LSP)*HVTM(ISYS)
  230             CONTINUE
              ENDIF
  220     CONTINUE
      ENDIF

C     ADD IN HYDROGEN CHLORIDE DEPOSITION ONTO THE WALLS IF HCL IS TRACKED

      IF (ACTIVS(6)) THEN
          DO 90 I = 1, NM1
              DO 91 K = 1, NWAL
                  ISOF = ISOF + 1
                  P(ISOF) = MINMAS
   91         CONTINUE
   90     CONTINUE
      END IF

C     ADD SMOKE AGGLOMERATION IF SMOKE IS TRACKED

      IF (ACTIVS(9)) THEN
      END IF

c     the following line was commented out 8/27/92 because wall initialization
c     is now done by offset, nputt and initwall (wset)
c      CALL DATACOPY(P,ODEVARB)

C     CONNECT HVAC TO THE REST OF THE WORLD

      HVDELT = DELTAT

C     DEFINE PRODUCT MAP ARRAY

      IZPMAP(1) = 1
      IZPMAP(2) = 2
      IP = 2
      DO 100 IPROD = 1, NS
          IF (ACTIVS(IPROD)) THEN
              IP = IP + 1
              IZPMAP(IP) = IPROD + 2
          END IF
  100 CONTINUE
      RETURN
      END

      SUBROUTINE INITTARG (IERROR)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITTARG
C
C     Source File: INITTARG.SOR
C
C     Functional Class:  
C
C     Description:  Initialize target data structures
C
C     Arguments: IERROR  Returns error codes
C
C     Revision History:
C        Created:  5/5/1995 at 13:51 by GPF
C       Modified:  8/15/1995 at 14:00 by PAR
C	             Changed intialization of wall targets so that the X,
C                  Y given in the keyword are using the same origin as 
C                  the general room geometry.
C        Modified: 9/5/1995 at 9:55 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      include "precis.fi"
      include "cfast.fi"
      include "fltarget.fi"
      include "thermp.fi"
      include "cshell.fi"
      CHARACTER*133 MESSG
      INTEGER MAP6(6)
      DATA MAP6/1,3,3,3,3,2/

      IFAIL = 0
      xm1 = -1.0d0
      x0 = 0.0d0
      DO 210 ITARG = 1, NTARG

C*** ROOM NUMBER MUST BE BETWEEN 1 AND NM1

          IROOM = IXTARG(TRGROOM,ITARG)
          IF(IROOM.LT.1.OR.IROOM.GT.NM1)THEN
              write(logerr,5000) iroom
              ierror = 213
              return
          ENDIF
          IWALL = IXTARG(TRGWALL,ITARG)
          XLOC = XXTARG(TRGCENX,ITARG)
          YLOC = XXTARG(TRGCENY,ITARG)
          ZLOC = XXTARG(TRGCENZ,ITARG)
          XXNORM = XXTARG(TRGNORMX,ITARG)
          YYNORM = XXTARG(TRGNORMY,ITARG)
          ZZNORM = XXTARG(TRGNORMZ,ITARG)
          XSIZE = BR(IROOM)
          YSIZE = DR(IROOM)
          ZSIZE = HRP(IROOM)
          !*** If the locator is -1, set to center of room on the floor
          if(xloc.eq.xm1) xloc = 0.5 * xsize
          if(yloc.eq.xm1) yloc = 0.5 * ysize
          if(zloc.eq.xm1) zloc = x0
          IF(IWALL.NE.0)THEN
              XXNORM = 0.0D0
              YYNORM = 0.0D0
              ZZNORM = 0.0D0
          ENDIF
          IF(IWALL.EQ.1)THEN
              ZZNORM = -1.0D0
              XX = XLOC
              YY = YLOC
              ZZ = ZSIZE
          ELSEIF(IWALL.EQ.2)THEN
              YYNORM = -1.0D0
C          XX = XSIZE - XLOC
              XX = XSIZE
              YY = YSIZE
              ZZ = YLOC
          ELSEIF(IWALL.EQ.3)THEN
              XXNORM = -1.0D0
              XX = XSIZE
              YY = XLOC
              ZZ = YLOC
          ELSEIF(IWALL.EQ.4)THEN
              YYNORM = 1.0D0
              XX = XLOC
              YY = 0.0D0
              ZZ = YLOC
          ELSEIF(IWALL.EQ.5)THEN
              XXNORM = 1.0D0
              XX = 0.0D0
C          YY = YSIZE - XLOC
              YY = YSIZE
              ZZ = YLOC
          ELSEIF(IWALL.EQ.6)THEN
              ZZNORM = 1.0D0
              XX = XLOC
C          YY = YSIZE - YLOC
              YY = YSIZE
              ZZ = 0.0D0
          ENDIF
          IF(IWALL.NE.0)THEN
              XXTARG(TRGCENX,ITARG) = XX
              XXTARG(TRGCENY,ITARG) = YY
              XXTARG(TRGCENZ,ITARG) = ZZ
              XXTARG(TRGNORMX,ITARG) = XXNORM
              XXTARG(TRGNORMY,ITARG) = YYNORM
              XXTARG(TRGNORMZ,ITARG) = ZZNORM
              XLOC = XX
              YLOC = YY
              ZLOC = ZZ
              IWALL2 = MAP6(IWALL)
              IF(SWITCH(IWALL2,IROOM))THEN
                  CXTARG(ITARG) = CNAME(IWALL2,IROOM)
              ELSE
                  CXTARG(ITARG) = ' '
              ENDIF
          ENDIF

C***    CENTER COORDINATES NEED TO BE WITHIN ROOM

          IF(XLOC.LT.0.0D0.OR.XLOC.GT.XSIZE.OR.
     .    YLOC.LT.0.0D0.OR.YLOC.GT.YSIZE.OR.
     .    ZLOC.LT.0.0D0.OR.ZLOC.GT.ZSIZE)THEN
              write(logerr,5001) iroom,xloc,yloc,zloc
              ierror = 214
              return
          ENDIF
  210 CONTINUE

C*** put a target in the center of the floor of each room

      DO 216 IROOM = 1, NM1
          NTARG = NTARG + 1
          IXTARG(TRGROOM,NTARG) = IROOM
          IXTARG(TRGMETH,NTARG) = STEADY
          IXTARG(TRGBACK,NTARG) = EXT

          XX = BR(IROOM)*0.50D0
          YY = DR(IROOM)*0.50D0
          ZZ = x0
          XXTARG(TRGCENX,NTARG) = XX
          XXTARG(TRGCENY,NTARG) = YY
          XXTARG(TRGCENZ,NTARG) = ZZ
          XXTARG(TRGNORMX,NTARG) = x0
          XXTARG(TRGNORMY,NTARG) = x0
          XXTARG(TRGNORMZ,NTARG) = 1.0D0
          XXTARG(TRGINTERIOR,NTARG) = 0.5

          IF(SWITCH(2,IROOM))THEN
              CXTARG(NTARG) = CNAME(2,IROOM)
          ELSE
              CXTARG(NTARG) = ' '
          ENDIF
  216 CONTINUE

      RETURN
 5000 format("Target assigned to non-existent compartment",i3)
 5001 format("Target located outside of compartment",i3,1x,3f10.3)
      END

      SUBROUTINE INITWALL(TSTOP,IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     INITWALL
C
C     Description:  This routine initializes data structures associated
C                   with walls and targets
C
C     Arguments: TSTOP
C                IERROR  Returns error codes
C
C     Revision History:
C        Created:  by gpf
C        Modified: 2/25/1994 by gpf:
C          Redefine wall slab properties for walls that connect 
C          interior rooms.  For example, if the ceiling in room 1
C          is connected to the floor of room 2 then this routine
C          will concatenate the slab property variables for conductivity,
C          specific heat etc.  The heat transfer can then be calculated
C          using the same routine, CNDUCT, but with the new slab variables.
C        Modified: 10/10/1994 by gpf:
C          Added initialization of target data structures.  Targets
C          are just thin walls.
C        Modified: 6/14/1995 by gpf:
C          Added exterior temperature parameter to WSET so that the temperature
C          profile is defined to be a linear ramp between the interior and exterior
C          wall surfaces temperature.
C        Modified: 6/30/1995 by gpf:
C          added CSHELL include file
C        Modified: 9/5/1995 at 9:57 by PAR:
C                  Added support for IERROR and returning stops to main
!		Modified 2/25/5 to use direct properties lookup
C
C        FKW = THERMAL CONDUCTIVITY
C        CW = SPECIFIC HEAT (J/KG)
C        RW = DENSITY OF THE WALL (KG/M**3)
C        FLW = THICKNESS OF THE WALL (M)
C        EPW = EMMISIVITY OF THE WALL
C        NSLB = DISCRETIZATION OF THE WALL SLABS (NUMBER OF NODES)
C        CNAME CONTAINS THE NAME OF THE THERMAL DATA SUBSET IN THE tpp datafile 
C        SWITCH IS A LOGICAL FOR THERMAL CALCULATION ON/OFF
C        THSET IS A SWITCH FOR A PROPERLY TRANSFERRED DATA SET
C        MAXCT IS A COUNT OF THE NUMBER OF tpp DATA SETs in the database
C        SWITCH IS SET IF CALCULATION IS CALLED FOR
C        THSET IS SET IF A NAME IN THE LIST OF REQUESTED DATA SETS matches ONE OF THE NAMES IN THE LIST OF DATA SET NAMES (NLIST).
C        THE DATA FROM THE DATA BASE IS STORED IN THE LOCAL VARIABLES LFKW,LCW,LRS,LFLW AND LEPW AND IS TRANSFERRED TO FKW...

      include "precis.fi"
      include "cfast.fi"
      include "cshell.fi"
      include "cenviro.fi"
      include "wnodes.fi"
      include "thermp.fi"
      include "fltarget.fi"

      CHARACTER OFF*8, NONE*8, tcname*8
!	TP is the pointer into the data base for each material
      INTEGER tp

      DATA OFF /'OFF'/, NONE /'NONE'/

C     MAP THE THERMAL DATA INTO ITS APPROPRIATE WALL SPECIFICATION
C     IF NAME IS "OFF" OR "NONE" THEN JUST TURN ALL OFF

      DO 170 I = 1, NWAL
          DO 160 J = 1, NM1
              THSET(I,J) = .FALSE.
              IF (SWITCH(I,J)) THEN
                  IF (CNAME(I,J).EQ.OFF.OR.CNAME(I,J).EQ.NONE) THEN
                      SWITCH(I,J) = .FALSE.
                      GO TO 160
                  END IF
                  CALL GETTPP(CNAME(I,J),tp,IERROR)
                  IF (IERROR.NE.0) RETURN
                  NSLB(I,J) = lnslb(tp)
                  DO 140 K = 1, NSLB(I,J)
                      FKW(K,I,J) = lfkw(k,tp)
                      CW(K,I,J) = lcw(k,tp)
                      RW(K,I,J) = lrw(k,tp)
                      FLW(K,I,J) = lflw(k,tp)
  140             CONTINUE
                  EPW(I,J) = lepw(tp)
                  DO 150 K = 1, 7
                      HCLBF(K,I,J) = lhclbf(k,tp)
  150             CONTINUE
              END IF
  160     CONTINUE
  170 CONTINUE

!	Initialize the interior temperatures to the interior ambient

      DO 270 I = 1, NM1
          DO 260 J = 1, NWAL
              TWE(J,I) = ETA(I)
              DO 250 K = 1, NN 
                  TWJ(K,I,J) = TAMB(I)
  250         CONTINUE
  260     CONTINUE
  270 CONTINUE

C*** initialize temperature profile data structures

      DO 20 I = 1, NM1
          DO 10 J = 1, NWAL
              IF (SWITCH(J,I)) THEN
                  CALL WSET(NUMNODE(1,J,I),NSLB(J,I),TSTOP,WALLDX(1,I,J)
     +            ,WSPLIT,FKW(1,J,I),CW(1,J,I),RW(1,J,I),FLW(1,J,I),
     +            WLENGTH(I,J),TWJ(1,I,J),TAMB(I),ETA(I))
              END IF
   10     CONTINUE
   20 CONTINUE

C*** concatenate slab properties of wall nodes that are connected
C    to each other

      DO 30 I = 1, NSWAL
          IFROMR = IZSWAL(I,1)
          IFROMW = IZSWAL(I,2)
          ITOR = IZSWAL(I,3)
          ITOW = IZSWAL(I,4)

          NSLABF = NSLB(IFROMW,IFROMR)
          NSLABT = NSLB(ITOW,ITOR)
          NSLB(IFROMW,IFROMR) = NSLABF + NSLABT
          NSLB(ITOW,ITOR) = NSLABF + NSLABT

          NPTSF = NUMNODE(1,IFROMW,IFROMR)
          NPTST = NUMNODE(1,ITOW,ITOR)
          NUMNODE(1,ITOW,ITOR) = NPTSF + NPTST - 1
          NUMNODE(1,IFROMW,IFROMR) = NPTSF + NPTST - 1

          WFROM = WLENGTH(IFROMR,IFROMW)
          WTO = WLENGTH(ITOR,ITOW)
          WLENGTH(IFROMR,IFROMW) = WFROM + WTO
          WLENGTH(ITOR,ITOW) = WFROM + WTO

          JJ = NSLABT + 1
          DO 40 J = NSLABF+1, NSLABF+NSLABT
              JJ = JJ - 1
              FKW(J,IFROMW,IFROMR) = FKW(JJ,ITOW,ITOR)
              CW(J,IFROMW,IFROMR) =  CW(JJ,ITOW,ITOR)
              RW(J,IFROMW,IFROMR) =  RW(JJ,ITOW,ITOR)
              FLW(J,IFROMW,IFROMR) = FLW(JJ,ITOW,ITOR)
              NUMNODE(J+1,IFROMW,IFROMR) = NUMNODE(JJ+1,ITOW,ITOR)
   40     CONTINUE

          JJ = NSLABF + 1
          DO 50 J = NSLABT+1, NSLABT+NSLABF
              JJ = JJ - 1
              FKW(J,ITOW,ITOR) = FKW(JJ,IFROMW,IFROMR)
              CW(J,ITOW,ITOR) =  CW(JJ,IFROMW,IFROMR)
              RW(J,ITOW,ITOR) =  RW(JJ,IFROMW,IFROMR)
              FLW(J,ITOW,ITOR) = FLW(JJ,IFROMW,IFROMR)
              NUMNODE(J+1,ITOW,ITOR) = NUMNODE(JJ+1,IFROMW,IFROMR)
   50     CONTINUE

          JJ = NPTST 
          DO 60 J = NPTSF+1,NPTSF+NPTST - 1
              JJ = JJ - 1
              TWJ(J,IFROMR,IFROMW) = TWJ(JJ,ITOR,ITOW)
              WALLDX(J-1,IFROMR,IFROMW) = WALLDX(JJ,ITOR,ITOW)
   60     CONTINUE

          JJ = NPTSF 
          DO 70 J = NPTST+1,NPTST+NPTSF - 1
              JJ = JJ - 1
              TWJ(J,ITOR,ITOW) = TWJ(JJ,IFROMR,IFROMW)
              WALLDX(J-1,ITOR,ITOW) = WALLDX(JJ,IFROMR,IFROMW)
   70     CONTINUE

   30 CONTINUE

C*** INITIALIZE TARGET DATA STRUCTURES

      DO 100 ITARG = 1, NTARG
          TCNAME = CXTARG(ITARG)
          IF(TCNAME.EQ.' ')THEN
              TCNAME = 'DEFAULT'
              CXTARG(ITARG) = TCNAME
          ENDIF
          ICODE = 0
          CALL GETTPP(TCNAME,tp,IERROR)
          IF (IERROR.NE.0) RETURN
          XXTARG(TRGK,ITARG) = lfkw(1,tp)
          XXTARG(TRGCP,ITARG) = lcw(1,tp)
          XXTARG(TRGRHO,ITARG) = lrw(1,tp)
          XXTARG(TRGL,ITARG) = lflw(1,tp)
          XXTARG(TRGEMIS,ITARG) = lepw(tp)
  100 CONTINUE

      RETURN
      END

      SUBROUTINE OFFSET (IERROR)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OFFSET
C
C     Source File: OFFSET.SOR
C
C     Functional Class:  
C
C     Description:  
C     Offset in the following context is the beginning of the vector for
C     that particular variable minus one.  Thus, the actual pressure array
C     goes from NOFP+1 to NOFP+nm1.  The total number of equations to be
C     considered is NEQUALS, and is the last element in the last vector.
C     Each physical interface routine is responsible for the COUNT of the
C     number of elements in the vector for which it is resonsible.

C     This set of parameters is set by NPUTP and is kept in the environment
C     common block CENVIRO.INC.  To index a variable, the list is something
C     like (for temperature in this case)

C     NOFTU+1, NOFTU+NM1

C     The structure of the solver array is

C     NOFP = offset for the main pressure; the array of base pressures for each compartment
C     NOFPMV = offset for HVAC node pressuers
C     NOFTMV = offset for HVAC branch temperatures
C     NOFTU = upper layer temperature
C     NOFVU = upper layer volume
C     NOFTL = lower layer temperature
C     NOFTT = target temperatures
C     NOFWT = wall surface temperatures (equivalent to the number of profiles)
C     NOFPRD = species
C     NOFHCL = surface deposition of hydrogen chloride
C     NOFSMKW = surface deposition of soot
C     NOFSMK = gas phase agglomeration of soot
C     NEQUALS = last element in the array.

C     The arrays which use this structure are VATOL, VRTOL, P, PDOLD, PPRIME and PDZERO

C     An important note - solve sets the last variable to be solved to NOFPRD
C     which is the beginning of the species (-1) and the end of the array which
C     is presently used by DASSL.
C
C     Arguments: IERROR  Returns error codes
C
C     Revision History:
C
C     created May 19, 1992
C             June 14, 1992 added offsets for HVAC duct temperatures
C        Modified: 4/26/1995 gpf:
C                  added offset parameter, NOFTT, for implicit targets
C        Modified: 6/30/1995 gpf:
C                  added oxygen offsets
C        Modified: 8/15/1995 par:
C                  added flame spread offsets.  Fixed for LFBT = 0 to
C	             treated as a type 2 fire.
C        Modified: 9/5/1995 at 10:12 by PAR:
C                  Added support for IERROR and returning stops to main
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "cenviro.fi"
      include "params.fi"
      include "wnodes.fi"
      include "fltarget.fi"
      include "opt.fi"
      include "objects2.fi"

C     COUNT THE OF NODES (LARGEST OF NS AND NE)

      NNODE = MAX(NA(1),NE(1))
      DO 50 IB = 2, NBR
          NNODE = MAX(NNODE,NA(IB),NE(IB))
   50 CONTINUE
      IF (NNODE.GT.MNODE) THEN
          CALL XERROR('OFFSET - Node range exceeded for HVAC',0,1,1)
          IERROR = 16
          RETURN
      END IF

C     SET THE NUMBER OF COMPARTMENTS AND OFFSETS

      NM1 = N - 1

C     COUNT THE SPECIES 

      NLSPCT = 0

      IF (LFBT.EQ.1) THEN
          DO 90 I = 1, NS
              IF (ALLOWED(I).AND.ACTIVS(I)) THEN
                  NLSPCT = NLSPCT + 1
              END IF
   90     CONTINUE
      ELSE IF (LFBT.EQ.2.OR.LFBT.EQ.0) THEN
          DO 110 I = 1, NS
              IF (ALLOWED(I)) THEN
                  IF (ACTIVS(I)) THEN
                      NLSPCT = NLSPCT + 1
                  END IF
              ELSE IF (I.NE.7) THEN
                  NLSPCT = NLSPCT + 1
              END IF
  110     CONTINUE
          NLSPCT = NLSPCT + 1
      ELSE
          STOP ' NOT AN ALLOWED FIRE TYPE'
      ENDIF

C     COUNT THE NUMBER OF WALLS

      NWALLS = 0
      DO 270 I = 1, NM1
          DO 260 J = 1, NWAL
              IF (SWITCH(J,I)) THEN
                  NWALLS = NWALLS + 1
              END IF
              IF (NWPTS.NE.0) NUMNODE(1,J,I) = NWPTS
  260     CONTINUE
  270 CONTINUE

C     count the number of implicit targets

      NIMTARG = 0
      NEQTARG(MPLICIT) = 0
      NEQTARG(STEADY) = 0
      NEQTARG(XPLICIT) = 0
      DO 300 ITARG = 1, NTARG
          IF(IXTARG(TRGMETH,ITARG).EQ.MPLICIT)THEN
              NIMTARG = NIMTARG + 1
              NEQTARG(MPLICIT) = NEQTARG(MPLICIT) + 1
          ELSEIF(IXTARG(TRGMETH,ITARG).EQ.STEADY)THEN
              NEQTARG(STEADY) = NEQTARG(STEADY) + 1
          ELSEIF(IXTARG(TRGMETH,ITARG).EQ.XPLICIT)THEN
              NEQTARG(XPLICIT) = NEQTARG(STEADY) + 1
          ENDIF
  300 CONTINUE

C    SET NUMBER OF IMPLICIT OXYGEN VARIABLES

C*** note we never let dassl solve for oxygen when we have a type 1 fire

      IF(LFBT.EQ.1)OPTION(FOXYGEN) = OFF
      IF(OPTION(FOXYGEN).EQ.ON)THEN
          NOXYGEN = NM1
      ELSE
          NOXYGEN = 0
      ENDIF

C     NOW DO ALL THE EQUATION OFFSETS

      NHVPVAR = NNODE - NEXT
      NHVTVAR = NBR
      NOFP = 0
      NOFPMV = NOFP + NM1
      NOFTMV = NOFPMV + NHVPVAR
      NOFFSM = NOFTMV + NHVTVAR
      NOFTU = NOFFSM
      NOFVU = NOFTU + NM1
      NOFTL = NOFVU + NM1
      NOFOXYL = NOFTL + NM1
      NOFOXYU = NOFOXYL + NOXYGEN
      NOFTT = NOFOXYU + NOXYGEN
      NOFWT = NOFTT + NIMTARG
      NOFPRD = NOFWT + NWALLS
      NOFHCL = NOFPRD + 2 * NM1 * NLSPCT
      NOFSMKW = NOFHCL + 4 * NM1 * HCLDEP
      NOFSMK = NOFSMKW + 4 * NM1 * SMKAGL
      NOFHVPR = NOFSMK + 4 * NM1 * SMKAGL

C     If the hvac model is used then nequals needs to be redefined in 
C     HVMAP since the variable NHVSYS is not defined yet.  After NHVSYS 
C     is defined the following statement can be used to define nequals

C     NEQUALS = NOFHVPR + NHVSYS*NLSPCT

      NEQUALS = NOFHVPR

      RETURN
      END

      SUBROUTINE ROOMCON(TSEC)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     ROOMCON
C
C     Source File: ROOMCON.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine determines whether flow from each room can
C                   reach the outside (perhaps through intermediate rooms)
C                   via horizontal or vertical vents.  If a room is 
C                   isolated from the outside then SNSQE has trouble finding
C                   an initial pressure solution.
C
C     Revision History:
C        Created:  1/31/96 by GPF
C        Modified: 2/10/97 by gpf
C                  use o(n) vent datastructures instead of o(n**2)
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      include "precis.fi"
      include "cfast.fi"
      include "params.fi"
      include "cenviro.fi"
      include "vents.fi"

      DIMENSION VNTOPN(NV)
      INTEGER ROOMC(NR,NR), TEMPMAT(NR,NR)
      INTEGER TOPRM, BOTRM
      DATA TOPRM /1/, BOTRM /2/

C*** initially assume that no rooms are connected
      DO 10 I = 1, N
          DO 20 J = 1, N
              ROOMC(I,J) = 0
   20     CONTINUE
          ROOMC(I,I) = 1
   10 CONTINUE

C*** check horizontal vent flow

      DO 30 I = 1, NVENTS
          IROOM1 = IZVENT(I,1)
          IROOM2 = IZVENT(I,2)
          IK = IZVENT(I,3)
          IM = MIN(IROOM1,IROOM2)
          IX = MAX(IROOM1,IROOM2)
          factor2 = qchfraction(qcvh,ijk(im,ix,ik),tsec)
          HEIGHT = ZZVENT(I,2) - ZZVENT(I,1)
          WIDTH = ZZVENT(I,3)
          avent = factor2 * height * width
          IF(AVENT.NE.0.0D0)THEN
              ROOMC(IROOM1,IROOM2) = 1
              ROOMC(IROOM2,IROOM1) = 1
          ENDIF
   30 CONTINUE

C*** check vertical vent flow

      DO 50 I = 1, NVVENT
          IROOM1 = IVVENT(I,TOPRM)
          IROOM2 = IVVENT(I,BOTRM)
          IF(VVAREA(IROOM1,IROOM2).NE.0.0D0)THEN
              ROOMC(IROOM1,IROOM2) = 1
              ROOMC(IROOM2,IROOM1) = 1
          ENDIF
   50 CONTINUE


C*** construct ROOMC**MATITER where MATITER > N
C    Note:  ROOMC is a transitiion matrix (from markov chain theory).
C           That is, ROOMC(i,j) is zero if there no connection between
C           room and room j.  Similarly, ROOMC(i,j) is one if there
C           is a connection between these two rooms.  ROOMC is symmetric.
C           The matrix ROOMC**2 is tells us whether flow can get from
C           room i to room j in two steps.  Since there are only N rooms,
C           ROOMC**N tells us whether any given room is connected to
C           any other room in N steps.  The entries ROOMC**N(i,n) then
C           indicates whether a room is connected to the outside (perhaps
C           through several other intermediate rooms).

      MATITER = 1
      DO 60 I = 1, N
          IF(N.LE.MATITER)GO TO 70
          CALL MAT2MULT(ROOMC,TEMPMAT,NR,N,matiter)
          MATITER = MATITER*2
   60 CONTINUE
   70 CONTINUE

      DO 80 I = 1, NM1
          IF(ROOMC(I,N).NE.0)THEN
              IZCON(I) = .TRUE.
          ELSE
              IZCON(I) = .FALSE.
          ENDIF
   80 CONTINUE


      RETURN
      END
      SUBROUTINE MAT2MULT(MAT1,MAT2,IDIM,N,MATITER)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     MAT2MULT
C
C     Source File: ROOMCON.SOR
C
C     Functional Class:  CFAST
C
C     Description:  Given an NxN matrix MAT1 whose elements are either 0 or 1,
C                   this routine computes the matrix MAT1**2 and 
C                   returns the results in MAT1 (after scaling non-zero entries
C                   to 1).
C
C     Revision History:
C        Created:  1/31/96 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      DIMENSION MAT1(IDIM,N),MAT2(IDIM,N)
      DO 10 I = 1, N
          DO 20 J = 1, N
              MAT2(I,J) = IDOT(MAT1(I,1),IDIM,MAT1(1,J),1,N)
              IF(MAT2(I,J).GE.1)MAT2(I,J) = 1
   20     CONTINUE
   10 CONTINUE
      DO 30 I = 1, N
          DO 40 J = 1, N
              MAT1(I,J) = MAT2(I,J)
   40     CONTINUE
   30 CONTINUE
      RETURN
      END
      INTEGER FUNCTION IDOT(IX,INX,IY,INY,N)

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     IDOT
C
C     Source File: ROOMCON.SOR
C
C     Functional Class:  CFAST
C
C     Description:  This routine computes the integer dot product of two
C                   integer vectors.
C
C     Revision History:
C        Created:  1/31/96 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      INTEGER IX(*), IY(*)
      IDOT = 0
      II = 1 - INX
      JJ = 1 - INY
      DO 10 I = 1, N
          II = II + INX
          JJ = JJ + INY
          IDOT = IDOT + IX(II)*IY(JJ)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE WSET(NUMNODE,NSLAB,TSTOP,WALLDX,WSPLIT,WK,WSPEC,WRHO,
     +WTHICK,WLEN,WTEMP,TAMB,TEXT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     WSET
C
C     Source File: WSET.SOR
C
C     Functional Class:  
C
C     Description:  Initializes temperature profiles, breakpoints used
C                   in wall conduction calculations.
C
C     Arguments: NUMNODE  Number of nodes in each slab
C                NSLAB    Number of slabs
C                TSTOP    Final simulation time
C                WALLDX   Wall position points
C                WSPLIT   fraction of points assigned to slabs 1, 2 and 3
C                WK       Wall thermal conductivity
C                WSPEC    Wall specific heat
C                WRHO     Wall density
C                WTHICK   Thickness of each slab
C                WLEN     Length of wall
C                WTEMP    Wall temperature profile
C                TAMB     Ambient temperature seen by interior wall
C                TEXT     Ambient temperature seen by exterior wall
C
C     Revision History:
C        Created:  by gpf
C        Modified: 02/7/1993 GPF:
C                  Fixed wall point allocations.
C                  06/14/1995 GPF:
C                  Wall temperature was defined to have a constant value
C                  of TAMB.  Now it is defined to change from TAMB to TEXT
C                  from the interior to exterior of wall. 
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      include "precis.fi"
C
      DIMENSION WALLDX(*), XWALL(100)
      DIMENSION NUMNODE(*), WK(*), WSPEC(*), WRHO(*), WTHICK(*)
      DIMENSION WTEMP(*)
      INTEGER CUMPTS
      DIMENSION NUMPTS(10), CUMPTS(10), XPOS(10)
      DIMENSION WSPLIT(*)
C
      NX = NUMNODE(1)
      XXNX = NX
C      
      NINTX = NX - (NSLAB+1)
      IF (NSLAB.LE.2) THEN
          NSPLIT = (WSPLIT(1)+WSPLIT(2)) * XXNX
      ELSE
          NSPLIT = WSPLIT(1) * XXNX
      END IF
C
C*** calculate total walldepth
C
      XPOS(1) = 0.0D0
      DO 20 ISLAB = 1, NSLAB
          XPOS(ISLAB+1) = XPOS(ISLAB) + WTHICK(ISLAB)
   20 CONTINUE
      WLEN = XPOS(NSLAB+1)
C
C*** calculate break point based on first slab's properties
C
      ERRFC05 = 1.30D0
      XKRHOC = WK(1) / (WSPEC(1)*WRHO(1))
      ALPHA = SQRT(XKRHOC)
      XB = 2.0D0 * ALPHA * SQRT(TSTOP) * ERRFC05 * WLEN
      IF (XB.GT..50D0*WLEN) XB = .5D0 * WLEN
      IF (NSLAB.EQ.1) THEN
C
C*** SET UP WALL NODE LOCATIONS for 1 slab case
C    bunch points at interior and exterior boundary
C
          XXNSPLIT = NSPLIT
          W = 1.0D0 / XXNSPLIT 
          DO 30 I = 1, NSPLIT + 1 
              XXIM1 = I - 1
              XWALL(I) = XB * (XXIM1*W) ** 2
   30     CONTINUE
          W = 1.0D0 / (XXNX-(XXNSPLIT+1.0D0))
          DO 40 I = NSPLIT +2, NX
              II = NX + 1 - I 
              XXIIM1 = II - 1
              XWALL(I) = WLEN - (WLEN-XB) * (XXIIM1*W) ** 2
   40     CONTINUE
          NUMNODE(1+NSLAB) = NINTX
      ELSE
C
C*** SET UP WALL NODE LOCATIONS for multi-slab case.
C    bunch points at interior boundary of first slab,
C    exterior boundary of last slab and uniformly in middle slabs
C
C
C*** calculate number of points interior to each slab
C
          XXNINTX = NINTX
          NUMPTS(1) = WSPLIT(1) * XXNINTX * MIN(XB,WTHICK(1)) / WLEN
          IF (NUMPTS(1).LT.1) NUMPTS(1) = 1
          WMXB = WLEN - XB
          NUMPTS(NSLAB) = WSPLIT(3) * XXNINTX * MIN(WMXB,WTHICK(NSLAB))/ 
     +    WLEN
          IF (NUMPTS(NSLAB).LT.1) NUMPTS(NSLAB) = 1
          ISUM = NINTX - NUMPTS(1) - NUMPTS(NSLAB)
          XXNSLABM2 = NSLAB - 2
          DO 50 I = 2, NSLAB - 1
              NUMPTS(I) = XXNX * WSPLIT(2)*WTHICK(NSLAB)/XXNSLABM2/WLEN
              IF (NUMPTS(I).LT.1) NUMPTS(I) = 1
              ISUM = ISUM - NUMPTS(I)
   50     CONTINUE
          NUMPTS(1) = NUMPTS(1) + (ISUM-ISUM/2)
          NUMPTS(NSLAB) = NUMPTS(NSLAB) + ISUM / 2
          IF (NUMPTS(NSLAB).LT.1) THEN
              NUMPTS(1) = NUMPTS(1) + NUMPTS(NSLAB) - 1
              NUMPTS(NSLAB) = 1
          END IF
C
C*** copy numpts data into numnode and keep a running total
C
          CUMPTS(1) = 1
          DO 60 ISLAB = 1, NSLAB
              NUMNODE(1+ISLAB) = NUMPTS(ISLAB)
              CUMPTS(ISLAB+1) = CUMPTS(ISLAB) + NUMPTS(ISLAB) + 1
   60     CONTINUE
C
C*** calculate wall positions for first slab (bunched near left)
C
          NINT = NUMPTS(1) + 1
          XXNINT = NINT
          DO 70 I = 1, NINT
              XXIM1 = I - 1
              XWALL(I) = XXIM1 ** 2 * XPOS(2) / XXNINT**2
   70     CONTINUE
C
C*** calculate wall positions for middle slabs (uniform)
C
          DO 90 ISLAB = 2, NSLAB - 1
              IBEG = CUMPTS(ISLAB)
              IEND = CUMPTS(ISLAB+1) - 1
              XXI3 = IEND+1-IBEG
              DO 80 I = IBEG, IEND
                  XXI1 = IEND+1-I
                  XXI2 = I-IBEG
                  XWALL(I) = (XPOS(ISLAB)*XXI1+XPOS(ISLAB+1)*XXI2) / XXI3
   80         CONTINUE
C
C*** keep track of break points
C
   90     CONTINUE
C
C*** calculate wall positions for last slab (bunched near right)
C
          IF (NSLAB.GE.2) THEN
              IBEG = CUMPTS(NSLAB)

!*** include last point for last slab

              IEND = CUMPTS(NSLAB+1)
              XXI3 = IEND - IBEG
              DO 100 I = IBEG, IEND
                  XXI1 = IEND - I
                  XWALL(I) = XPOS(NSLAB+1) - XXI1 ** 2 * (XPOS(NSLAB+1)-
     +            XPOS(NSLAB)) / XXI3 ** 2
  100         CONTINUE
          END IF
      END IF
C
C*** finally calculate distances between each point
C    these distances are used by cnduct to setup discretization
C    tri-diagonal matrix
C
      DO 110 I = 1, NX - 1
          WALLDX(I) = XWALL(I+1) - XWALL(I)
  110 CONTINUE
C
C*** initialize temperature profile.  Note, WTEMP(1)=WTEMP(2) and
C    WTEMP(NX)=WTEMP(NX-1) so DASSL will think that no heat transfer
C    needs to occur to the wall (since dT/dx=0 here)
C
      WTEMP(1) = TAMB
      WTEMP(NX) = TEXT
      DTDW = (TEXT-TAMB)/(XWALL(NX-1)-XWALL(2))
      DO 10 I = 2, NX-1
          WTEMP(I) = TAMB + (XWALL(I)-XWALL(2))*DTDW
   10 CONTINUE
      RETURN
      END

      integer function rev_initialization

      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     *mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     *maindate='$Date$'

      WRITE(module_date,'(A)') 
     *mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_initialization = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_initialization