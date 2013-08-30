
    subroutine gettpp (name, tp, errorcode)

    !     Routine: gettpp
    !     Purpose: check for and return index to a thermal property
    !     Revision: $Revision$
    !     Revision Date: $Date$

    use thermp

    implicit none
    character name*(*), missingtpp*64
    integer tp, errorcode, i

    errorcode = 0
    do i = 1, maxct
        tp = i
        if (name==nlist(i)) return
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

    use cenviro
    use cfast_main
    use cshell
    use opt
    use params
    use solver_parameters
    implicit none

    integer :: nalg, i, ires, nnn, iflag
    real(8) :: hvpsolv(*), deltamv(*), p2(maxteq), delta(maxteq), pdzero(maxteq), T
    data pdzero /maxteq * 0.0d0/
    
    nalg = nm1 + nhvpvar + nhvtvar
    do i = 1, nalg
        p2(i) = hvpsolv(i)
    end do
    do i = nalg + 1, nequals
        p2(i) = pinit(i)
    end do
    if(iprtalg/=0)then
        write(iofilo,*)'room pressures'
        do i = 1, nm1
            write(iofilo,*)i,p2(i)
        end do
        if(nhvpvar>0)write (iofilo,*) 'hvac pressures'
        do i = 1, nhvpvar
            write(iofilo,*)i,p2(i+nofpmv)
        end do
        if(nhvtvar>0)write (iofilo,*) 'hvac temperatures'
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
    if(iprtalg/=0)then
        write(iofilo,*)'room pressure residuals'
        do i = 1, nm1
            write(iofilo,*)i,delta(i)
        end do
        if(nhvpvar>0)write (iofilo,*) 'hvac pressure residuals'
        do i = 1, nhvpvar
            write(iofilo,*)i,delta(i+nofpmv)
        end do
        if(nhvtvar>0)write (iofilo,*) 'hvac temperature residuals'
        do i = 1, nhvtvar
            write(iofilo,*)i,delta(i+noftmv)
        end do
        write(iofilo,*)' '
        read (*,*)
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

    use cenviro
    use cfast_main
    use cshell
    use opt
    use params
    use solver_parameters
    implicit none

    real(8) :: hvsolv(*), deltamv(*), p2(maxteq), delta(maxteq), pdzero(maxteq), t
    integer :: i, ires, nnn, iflag
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
    if (iprtalg/=0) then
        if(nhvpvar>0)write (iofilo,*) 'hvac pressures'
        do i = 1, nhvpvar
            write (iofilo,*) i, hvsolv(i)
        end do
        if(nhvtvar>0)write (iofilo,*) 'hvac temperatures'
        do i = 1, nhvtvar
            write (iofilo,*) i, hvsolv(nhvpvar+i)
        end do
    endif
    t = stime
    call resid(t,p2,pdzero,delta,ires,rpar2,ipar2)
    do i = 1, nhvpvar
        deltamv(i) = delta(i+nofpmv)
    end do
    do i = 1, nhvtvar
        deltamv(i+nhvpvar) = delta(i+noftmv)
    end do
    if (iprtalg/=0) then
        write (iofilo,*) ' '
        if(nhvpvar>0)write (iofilo,*) 'hvac pressure residuals'
        do i = 1, nhvpvar
            write (iofilo,*) i, deltamv(i)
        end do
        if(nhvtvar>0)write (iofilo,*) 'hvac temperature residuals'
        do i = 1, nhvtvar
            write (iofilo,*) i, deltamv(i+nhvpvar)
        end do
        write(iofilo,*)' '
        read(*,*)
    endif
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

    use cenviro
    use cfast_main
    use cshell
    use opt
    use params
    use solver_parameters
    implicit none

    real(8) :: hvpsolv(*), deltamv(*), p2(maxteq), delta(maxteq), pdzero(maxteq), t
    integer nalg, i, ii, ieq1, ieq2, ieq3, ires, nnn, iflag
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
    if(ieq1/=0)then
        ii = ii + 1
        p2(ieq1) = hvpsolv(ii+nalg+1)
    endif
    if(ieq2/=0)then
        ii = ii + 1
        p2(ieq2) = hvpsolv(ii+nalg+1)
    endif

    if(iprtalg/=0)then
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
        if(ieq1/=0.or.ieq3/=0)then
            write(iofilo,*)'wall temperatures'
            if(ieq1/=0)then
                ii = ii + 1
                write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,p2(ieq1)
            endif
            if(ieq3/=0)then
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
    if(ieq1/=0)then
        ii = ii + 1
        deltamv(ii+1+nalg) = delta(ieq1)
    endif
    if(ieq2/=0)then
        ii = ii + 1
        deltamv(ii+1+nalg) = delta(ieq2)
    endif
    if(iprtalg/=0)then
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
        if(ieq1/=0.or.ieq3/=0)then
            write(iofilo,*)'wall temperatures'
            if(ieq1/=0)then
                ii = ii + 1
                write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,delta(ieq1)
            endif
            if(ieq3/=0)then
                ii = ii + 1
                write(iofilo,'(1x,i3,1x,e23.16)')nalg+ii,delta(ieq3)
            endif
        endif
        write(iofilo,*)' '
        read(*,*)
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

    use cenviro
    use cfast_main
    use params
    implicit none

    real(8) :: c3(ns), f, xxjm1, s1, s2, xnext, pav, tav, df, xx, rden
    integer :: i, ii, j, k, ib, id, isys, ierror, lsp
    real(8), parameter :: x1 = 1.0d0, x0 = 0.0d0, pi = 4.0d0 * atan(x1)

    !    calculate min & max values for fan curve

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
    if (next<=0) return

    ! arrange data on node basis
    do i = 1, nnode
        k = 0
        do ib = 1, nbr
            if (i==na(ib)) then
                k = k + 1
                icmv(i,k) = ib
                mvintnode(i,k) = ne(ib)
            else if (i==ne(ib)) then
                k = k + 1
                icmv(i,k) = ib
                mvintnode(i,k) = na(ib)
            endif
        end do
        ncnode(i) = k
    end do

    ! check interior nodes
    do i = 1, nnode
        if (ncnode(i)<1.or.ncnode(i)>mcon) then
            call xerror('hvinit - interior node has too many or too few connections',0,1,1)
            ierror = 223
            return
        endif
    end do

    ! limit the range of hvelxt and set the absolute height of the interior node
    do ii = 1, next
        i = hvnode(1,ii)
        j = hvnode(2,ii)
        if (ncnode(j)>1) then
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
        if (i<n) then
            hvextt(ii,upper) = tamb(i)
            hvextt(ii,lower) = tamb(i)
            hvp(j) = zzrelp(i) - hvgrav * ramb(i) * hvelxt(ii)
        else
            hvextt(ii,upper) = exta
            hvextt(ii,lower) = exta
            hvp(j) = expa - hvgrav * exra * hvelxt(ii)
        endif
        tbr(ib) = hvextt(ii,upper)
        s1 = s1 + hvp(j)
        s2 = s2 + tbr(ib)
        do lsp = 1, ns
            ! the outside is defined to be at the base of the structure for mv
            if (i<n) then
                hvexcn(ii,lsp,upper) = o2n2(lsp) * ramb(i)
                hvexcn(ii,lsp,lower) = o2n2(lsp) * ramb(i)
            else
                hvexcn(ii,lsp,upper) = o2n2(lsp) * exra
                hvexcn(ii,lsp,lower) = o2n2(lsp) * exra
            endif
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
        if (hvp(i)<x0) then
            hvp(i) = pav
        endif
    end do
    do i = 1, nbr
        if (tbr(i)<=x0) tbr(i) = tav
        if (hvconc(i,1)<x0) then
            do lsp = 1, ns
                hvconc(i,lsp) = c3(lsp)
            end do
        endif
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

    use cfast_main
    use params
    implicit none

    integer :: istack(100), i, ii, j, icursys, iptr, icurnod, nxtnode, isys, ib

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
        if (izhvmapi(i)/=0) then
            ii = ii + 1
            izhvmapi(ii) = izhvmapi(i)
        endif
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
90  continue
    icurnod = 0
    do i = 1, nnode
        if (izhvsys(i)==0) then
            icurnod = i
            exit
        endif
    end do
    if (icurnod/=0) then
        icursys = icursys + 1
        iptr = iptr + 1
        istack(iptr) = icurnod
120     continue
        if (iptr==0) go to 90
        icurnod = istack(iptr)
        iptr = iptr - 1
        izhvsys(icurnod) = icursys
        do j = 1, ncnode(icurnod)
            nxtnode = mvintnode(icurnod,j)
            if (izhvsys(nxtnode)==0) then
                iptr = iptr + 1
                istack(iptr) = nxtnode
            endif
        end do
        go to 120
    endif
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

    subroutine initamb(yinter,iflag)

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

    use cenviro
    use cfast_main
    use fltarget
    use opt
    use params
    implicit none

    real(8) :: yinter(*), dummy(1), xxpmin, tdspray, tdrate, scale, dnrm2
    real(8), parameter :: xx0 = 0.0d0, xx2= 2.0d0
    integer i, ii, iflag, iwall, iroom, itarg, ieq

    ! simplify and make initial pressure calculations consistent.  inside pressures
    ! were calculated using rho*g*h .  but outside pressures were calculated using
    ! atmosp.  fictional flows resulted making  snsqe work a log harder to get
    ! an initial solution.  the initial temperature values calculated by atmosp
    ! at the top of the empire state building (about 400 m above base) is only
    ! about 0.2 k different that at the base.  
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
        if (iflag==1) then
            if (yinter(i)<0.d0) then
                p(i+nofvu) = zzvmin(i)
            else
                p(i+nofvu) = min(zzvmax(i),max(zzvmin(i),yinter(i)*ar(i)))
            endif
            yinter(i) = xx0
        endif
        if(izshaft(i)==1)p(i+nofvu) = zzvmax(i)
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
            endif
        end do
    end do

    ! establish default values for detector data
    do i = 1, ndtect
        iroom=ixdtect(i,droom)
        if(xdtect(i,dxloc)<0.0d0)xdtect(i,dxloc)=br(iroom)*.5d0
        if(xdtect(i,dyloc)<0.0d0)xdtect(i,dyloc)=dr(iroom)*.5d0
        if(xdtect(i,dzloc)<0.0d0)then
            xdtect(i,dzloc)=hrp(iroom)+xdtect(i,dzloc)
        endif
        tdspray = xdtect(i,dspray)

        ! if tdspray>0 then interpret it as a spray density and convert
        ! to a characteristic quenching time
        ! if tdspray < 0 then interpret abs(tdspray) as the time
        ! required to reduce the fire size by 50 per cent
        ! if tdspray = 0 then turn the sprinkler off
        if(tdspray>0.0d0)then
            tdrate = 3.0d0/tdspray**1.8d0
        elseif(tdspray<0.0d0)then
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

    call sortbrm(xdtect,mxdtect,ixdtect,mxdtect,ndtect,dtxcol,dticol,droom,nr,nm1,idtpnt)

    ! p's for pressure, volume and temperature are defined
    ! we can now copy these values to the environment variables
    call datacopy (p, odevara)

    ! initialize target temperatures
    do itarg = 1, ntarg
        iroom = ixtarg(trgroom,itarg)
        if(ixtarg(trgmeth,itarg)==mplicit)then
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
    if(option(foxygen)==on)then
        do iroom = 1, nm1
            p(iroom+nofoxyu)=0.23d0*zzmass(iroom,upper)
            p(iroom+nofoxyl)=0.23d0*zzmass(iroom,lower)
        end do
    endif

    ! define ihxy in izhall (dimension that is longest)
    do i = 1, nm1
        if(izhall(i,ihroom)==1)then
            if(dr(i)>br(i))then
                izhall(i,ihxy) = 1
            else
                izhall(i,ihxy) = 2
            endif
        endif
    end do

    return
    end

    subroutine initmm

    !     routine: initmm
    !     purpose: This routine initializes the main memory - must be used by 
    !              all modules that will run the model kernel
    !     Arguments: none

    use cenviro
    use cfast_main
    use cfin
    use cshell
    use fltarget
    use params
    use thermp
    use vents
    implicit none
    
    real(8) :: xlrg
    real(8), parameter :: xx0 = 0.0d0, xx1 = 1.0d0, xm1 = -1.0d0
    integer :: i, j, k, ivent, itarg, lsp, nfurn

    ! set some initialization - simple control stuff
    exset = .false.
    debugging = .false.

    ! initialize the common block
    do i = 1, ns
        o2n2(i) = xx0
        allowed(i) = .false.
        activs(i) = .true.
    end do
    do i = 1, nr
        do j = 1, nwal
            cname(j,i) = 'OFF'
            switch(j,i) = .false.
        end do
    end do
    do i = 1, nr
        switch(1,i) = .true.
        cname(1,i) = 'DEFAULT'
    end do
    nconfg = 0
    ndumpr = 0
    nlspct = 0
    nrestr = 0
    numthrm = 0
    mapltw(1) = 1
    mapltw(2) = 2
    mapltw(3) = 1
    mapltw(4) = 2
    hcldep = 0
    smkagl = 0
    n = 0
    do i = 1, nwal + 1
        cjeton(i) = .false.
    end do

    ! initialize the flow variables
    do i = 1, nr
        izshaft(i) = 0
        heatup(i) = xx0
        heatlp(i) = xx0
        heatvf(i) = xx0
        do j = 1, nr
            ! do vertical vents (vvent,...)
            vshape(i,j) = 0
            nwv(i,j) = 0
            vvarea(i,j) = xx0
            ! do horizontal vents (hvent,...)
            nw(i,j) = 0
        end do
    end do

    do ivent = 1, mxvents
        ss1(ivent) = xx0
        ss2(ivent) = xx0
        sa1(ivent) = xx0
        sa2(ivent) = xx0
        as1(ivent) = xx0
        as2(ivent) = xx0
        aa1(ivent) = xx0
        aa2(ivent) = xx0
        sau1(ivent) = xx0
        sau2(ivent) = xx0
        asl1(ivent) = xx0
        asl2(ivent) = xx0
    end do

    do i = 1, mext
        hveflot(upper,i) = xx0
        hveflot(lower,i) = xx0
        tracet(upper,i) = xx0
        tracet(lower,i) = xx0
    end do

    ! initialize the forcing functions
    do i = 1, nr
        emp(i) = xx0
        ems(i) = xx0
        eme(i) = xx0
        aps(i) = xx0
        do k = upper, lower
            qr(k,i) = xx0
            qc(k,i) = xx0
            qfc(k,i) = xx0
        end do
    end do
    do i = 1, mxfire
        qfr(i) = xx0
    end do
    do i = 1, maxteq
        p(i) = xx0
    end do

    ! define the outside world as infinity
    xlrg = 1.d+5
    do i = 1, nr
        dr(i) = xlrg
        br(i) = xlrg
        hr(i) = xlrg
        hrp(i) = xlrg
        hrl(i) = xx0
        hflr(i) = xx0
        cxabs(i) = xx0
        cyabs(i) = xx0
        ar(i) = br(i) * dr(i)
        vr(i) = hr(i) * ar(i)
        do  j = 1, nwal
            epw(j,i) = xx0
            qsradw(j,i) = xx0
            qscnv(j,i) = xx0
        end do
        do j = 1, nr
            nw(i,j) = 0
        end do
    end do

    ! initialize all vents to zero size
    do ivent = 1, mxvents
        bw(ivent) = xx0
        hh(ivent) = xx0
        hl(ivent) = xx0
        hhp(ivent) = xx0
        hlp(ivent) = xx0
        vface(ivent) = 1
    end do

    ! set the time step and inner step division for time splitting
    ! we do not let the user choose these
    deltat = 1.0d0

    ! define all the "universal constants
    sigm = 5.67d-8
    cp = 1012.0d0
    gamma = 1.40d0
    rgas = (gamma-1.0d0) / gamma * cp
    minmas = 0.0d0
    g = 9.80d0
    stime = xx0
    tref = 288.d0
    limo2 = 0.10d0
    gmwf = 16.0d0
    hcomba = 50000000.0d0
    pref = 1.013d+5
    pa = pref
    pofset = pref
    sal = xx0
    sal2 = -1.0d0
    te = tref
    ta = tref
    tgignt = te + 200.d0
    exta = ta
    expa = pa
    exsal = sal
    windv = xx0
    windrf = 10.d0
    windpw = 0.16d0
    do i = 0, mxfire
        objmaspy(i) = xx0
        radio(i) = xx0
        radconsplit(i) = 0.15d0
    end do
    tradio = xx0
    qradrl = 0.15d0

    ! normal air
    o2n2(1) = 0.77d0
    o2n2(2) = 0.23d0

    ! a specified fire in the center of the room
    lfbt = 2
    lfbo = 0
    lfmax = 1
    heatfl = .false.
    heatfq = 0.0
    heatfp(1) = xm1
    heatfp(2) = xm1
    heatfp(3) = xm1

    ! set to -1 as a flag for nputp initialization - any value not set will be set to the default which is the center of the respective wall
    fpos(1) = xm1
    fpos(2) = xm1
    fpos(3) = xm1

    ! set up default values for the chemistry
    do i = 1, nv

        ! define the vents as being open
        do ivent=1, mxvents
            qcvent(ivent,i) = 1.0d0
        end do
        tfired(i) = 86400.d0
        hfired(i) = xx0
        afired(i) = xx0
        bfired(i) = 0.000d0
        qfired(i) = bfired(i) * hcomba
        hcratio(i) = 0.3333333d0
        hocbmb(i) = hcomba
        coco2(i) = xx0
        cco2(i) = xx0
    end do

    ! Start with vents open: h for hvent, v for vvent, and m for mvent
    do i = 1,mxvents
        qcvh(1,i) = xx0
        qcvh(2,i) = xx1
        qcvh(3,i) = xx0
        qcvh(4,j) = xx1
    end do

    do i = 1, nr
        qcvv(1,i) = xx0
        qcvv(2,i) = xx1
        qcvv(3,i) = xx0
        qcvv(4,i) = xx1
    end do

    ! note that the fan fraction is unity = on, whereas the filter fraction is unity = 100% filtering since there is not "thing" associated with a filter, there is no (as of 11/21/2006) 
    ! way to have an intial value other than 0 (no filtering).
    do i = 1, mfan
        qcvf(1,i) = xx0
        qcvf(2,i) = xx0
        qcvf(3,i) = xx0
        qcvf(4,i) = xx0
        qcvm(1,i) = xx0
        qcvm(2,i) = xx1
        qcvm(3,i) = xx0
        qcvm(4,i) = xx1
    end do

    hcratt = hcratio(1)

    ! turn hvac off initially

    nnode = 0
    nft = 0
    nfan = 0
    nfilter = 0
    nbr = 0
    next = 0
    hvgrav = g
    hvrgas = rgas
    mvcalc = .false.
    do i = 1, mnode
        hvght(i) = xx0
    end do

    ! initialize detectors
    do i = 1, mxdtect
        xdtect(i,drti) = 50.0d0
        xdtect(i,dspray) = -300.d0
        xdtect(i,dxloc) = -1.0d0
        xdtect(i,dyloc) = -1.0d0
        xdtect(i,dzloc) = -3.0d0/39.37d0
        xdtect(i,dtrig) = 330.3722d0
        xdtect(i,dvel) = 0.d0
        xdtect(i,dvelo) = 0.d0
        xdtect(i,dtact) = 99999.d0
        ixdtect(i,dtype) = 2
        ixdtect(i,droom) = 1
        ixdtect(i,dquench) = 0
        ixdtect(i,dact) = 0
    end do
    ndtect = 0
    do i = 1, nr
        iquench(i) = 0
    end do

    ! initialize room to room heat transfer data structures
    nswal = 0

    ! initialize target counter
    ntarg = 0

    do itarg = 1, mxtarg
        ixtarg(trgmeth,itarg) = xplicit
        ixtarg(trgeq,itarg) = pde
        ixtarg(trgback,itarg) = int
        cxtarg(itarg) = 'DEFAULT'
    end do

    ! initialize jaccol  
    jaccol = -2
    neqoff = 10

    ! initialize hall start time
    do i = 1, nr
        zzhall(i,ihtime0) = -1.0d0
        zzhall(i,ihvel) = -1.0d0
        zzhall(i,ihdepth) = -1.0d0
        zzhall(i,ihmaxlen) = -1.0d0
        zzhall(i,ihhalf) = -1.0d0
        zzhall(i,ihtemp) = 0.0d0
        zzhall(i,ihorg) = -1.0d0
        izhall(i,ihdepthflag) = 0
        izhall(i,ihhalfflag) = 0
        izhall(i,ihmode) = ihafter
        izhall(i,ihroom) = 0
        izhall(i,ihvelflag) = 0
        izhall(i,ihventnum) = 0
        izhall(i,ihxy) = 0
        do ivent = 1, mxvent
            zzventdist(i,ivent) = -1.
        end do
    end do
    updatehall = .false.

    do i = 1, nr
        do j = 1, nr
            do k = 1, 4
                ijk(i,j,k) = 0
            end do
        end do
    end do
    nventijk = 0

    ! initialize variable cross sectional area to none
    do i = 1, nr
        izrvol(i) = 0
        do j = 1, mxpts
            zzrvol(j,i) = xx0
            zzrarea(j,i) = xx0
            zzrhgt(j,i) = xx0
        end do
    end do

    ! initialzie time step checking
    zzdtcrit = 1.0d-09
    izdtnum = 0
    izdtmax = 100
    izdtflag = .true.

    ! initialize inter-compartment heat transfer fractions
    do i = 1, nr
        do j = 1, nr
            zzhtfrac(i,j) = xx0
        end do
    end do

    do j = 0, nr
        izheat(j) = 0
        do i = 1, nr
            izhtfrac(i,j) = 0
        end do
    end do

    do lsp = 1, ns
        do j = upper, lower
            do i = 1, nr
                zzgspec(i,j,lsp) = xx0
                zzcspec(i,j,lsp) = xx0            
            end do
        end do
    end do

    ! initialize number of furnace temperature nodes
    nfurn=0

    return
    end

    subroutine initob

    !     routine: initob
    !     purpose: this routine initializes the fire objects
    !     arguments: none

    use cfast_main
    use objects1
    use objects2
    implicit none
    
    integer :: i

    ! turn off objects
    numobjl = 0
    do i = 0, mxoin
        objon(i) = .false.
        objpos(1,i) = -1.0
        objpos(2,i) = -1.0
        objpos(3,i) = -1.0
        objrm(i) = 0
        objnin(i) = ' '
        objld(i) = .false.
        objpnt(i) = 0
        objcri(1,i) = 0.0
        objcri(2,i) = 0.0
        objcri(3,i) = 0.0
        objdef(i) = .false.
        odbnam(i) = ' '
    end do
    return
    end

    subroutine initslv

    !     routine: initslv
    !     purpose: this routine initializes the solver variables from solver.ini if it exists
    !     arguments: none

    use cfast_main
    use cfin
    use cshell
    use iofiles
    use opt
    use params
    use solver_parameters
    use wnodes
    implicit none

    real(8) :: fract1, fract2, fract3, fsum
    integer :: nopt, i, j, ibeg, iend
    logical existed

    ductcv = 0.0d0

    inquire (file=solverini,exist=existed)
    if (.not.existed) return
    close (iofili)
    write (logerr, '(2a)') '***** modify dassl tolerances with ', solverini
    open (unit=iofili,file=solverini)

    ! read in solver error tolerances
    read (iofili,*)
    read (iofili,*) aptol, rptol, atol, rtol
    read (iofili,*)
    read (iofili,*) awtol, rwtol, algtol
    read (iofili,*)
    read (iofili,*) ahvptol, rhvptol, ahvttol, rhvttol

    ! read in physical sub-model option list
    read (iofili,*)
    read (iofili,*) nopt
    nopt = max(0, min(mxopt, nopt))
    do i = 1, (nopt-1) / 5 + 1
        ibeg = 1 + (i-1) * 5
        iend = min(ibeg+4,nopt)
        read (iofili,*)
        read (iofili,*) (option(j),j = ibeg,iend)
    end do
    ! since the solver.ini file is on, turn on debug help
    option(fkeyeval) = 1

    ! set debug print
    if (option(fdebug)==2) then
        option(fdebug) = off
        switch(1,nr) = .true.
    else if (option(fdebug)>=3) then
        option(fdebug) = on
        switch(1,nr) = .true.
    endif

    ! read in wall info
    read (iofili,*)
    read (iofili,*) nwpts, fract1, fract2, fract3
    read (iofili,*)
    read (iofili,*) iwbound
    fsum = abs(fract1) + abs(fract2) + abs(fract3)
    wsplit(1) = abs(fract1) / fsum
    wsplit(2) = abs(fract2) / fsum
    wsplit(3) = abs(fract3) / fsum

    ! read in maximum desired solve step size, if negative then then solve will decide
    read (iofili,*)
    read (iofili,*) stpmax, dasslfts

    ! read in hvac convection coefficient
    read(iofili,*)
    read(iofili,*) ductcv

    ! read in jacobian and snsqe print flags
    read(iofili,*)
    read(iofili,*) jacchk, cutjac, iprtalg
    close (iofili)

    return
    end subroutine initslv

    subroutine initspec

    !     routine: initspec
    !     purpose: This routine initializes variables associated with 
    !              species it originally occured in CFAST and INITFS.  It was moved
    !              to one subroutine to make maintenance easier
    !     Arguments: none

    use cenviro
    use cfast_main
    use params
    use thermp
    implicit none

    real(8) :: xm(2), xt, xtemp, xh2o, toto2n2
    integer i, j, k, ip, iprod, isof, isys, lsp


    do i = 1, nm1
        xm(1) = ramb(i) * zzvol(i,upper)
        xm(2) = ramb(i) * zzvol(i,lower)

        !  set the water content to relhum - the polynomial fit is to (t-273), and
        ! is for saturation pressure of water.  this fit comes from the steam
        ! tables in the handbook of physics and chemistry.  we are being clever
        ! here.  the final result in o2n2 should be the value used in stport for
        ! the outside ambient.
        xt = tamb(i)
        xtemp = 23.2d0 - 3.816d3 / (xt-46.d0)
        xh2o = exp(xtemp) / 101325.0d0 * (18.d0/28.4d0)
        o2n2(8) = relhum * xh2o

        ! normalize the atmosphere
        toto2n2 = 0.0d0
        do j = 1, ns
            toto2n2 = toto2n2 + o2n2(j)
        end do
        do j = 1, ns
            o2n2(j) = o2n2(j) / toto2n2
        end do

        do k = upper, lower
            do lsp = 1, ns
                toxict(i,k,lsp) = 0.0d0
                mass(k,i,lsp) = o2n2(lsp) * xm(k)
            end do
        end do
    end do

    isof = nofprd
    do lsp = 1, ns
        if (activs(lsp)) then
            do i = 1, nm1
                do k = upper, lower
                    isof = isof + 1
                    p(isof) = mass(k,i,lsp) + minmas
                end do
            end do
        endif
    end do

    ! hvinit define initial products for hvac systems (if any)
    if(nhvsys/=0)then
        isof = nofhvpr
        do lsp = 1, min(ns,9)
            if(activs(lsp))then
                do isys = 1, nhvsys
                    isof = isof + 1
                    p(isof) = o2n2(lsp)*hvtm(isys)
                end do
            endif
        end do
    endif

    ! add in hydrogen chloride deposition onto the walls if hcl is tracked
    if (activs(6)) then
        do i = 1, nm1
            do k = 1, nwal
                isof = isof + 1
                p(isof) = minmas
            end do
        end do
    endif

    ! placeholder for smoke agglomeration if smoke is tracked
    if (activs(9)) then
    endif

    ! connect hvac to the rest of the world
    hvdelt = deltat

    ! define product map array
    izpmap(1) = 1
    izpmap(2) = 2
    ip = 2
    do iprod = 1, ns
        if (activs(iprod)) then
            ip = ip + 1
            izpmap(ip) = iprod + 2
        endif
    end do

    return
    end subroutine initspec

    subroutine inittarg (ierror)

    !     routine: inittarg
    !     purpose: Initialize target data structures
    !     Arguments: ierror:  Returns error codes

    use cfast_main
    use cshell
    use fltarget
    use thermp
    implicit none
    
    real(8), parameter :: xm1 = -1.0d0, x0 = 0.0d0
    real(8) :: xloc, yloc, zloc, xxnorm, yynorm, zznorm, xsize, ysize, zsize, xx, yy, zz
    integer :: ifail, itarg, iroom, iwall, iwall2, ierror
    integer :: map6(6) = (/1,3,3,3,3,2/)

    ifail = 0
    do itarg = 1, ntarg

        ! room number must be between 1 and nm1
        iroom = ixtarg(trgroom,itarg)
        if(iroom<1.or.iroom>nm1)then
            write(logerr,'(a,i3)') 'Target assigned to non-existent compartment',iroom
            ierror = 213
            return
        endif
        iwall = ixtarg(trgwall,itarg)
        xloc = xxtarg(trgcenx,itarg)
        yloc = xxtarg(trgceny,itarg)
        zloc = xxtarg(trgcenz,itarg)
        xxnorm = xxtarg(trgnormx,itarg)
        yynorm = xxtarg(trgnormy,itarg)
        zznorm = xxtarg(trgnormz,itarg)
        xsize = br(iroom)
        ysize = dr(iroom)
        zsize = hrp(iroom)

        ! if the locator is -1, set to center of room on the floor
        if(xloc==xm1) xloc = 0.5 * xsize
        if(yloc==xm1) yloc = 0.5 * ysize
        if(zloc==xm1) zloc = x0
        if(iwall/=0)then
            xxnorm = 0.0d0
            yynorm = 0.0d0
            zznorm = 0.0d0
        endif
        if(iwall==1)then
            zznorm = -1.0d0
            xx = xloc
            yy = yloc
            zz = zsize
        elseif(iwall==2)then
            yynorm = -1.0d0
            xx = xsize
            yy = ysize
            zz = yloc
        elseif(iwall==3)then
            xxnorm = -1.0d0
            xx = xsize
            yy = xloc
            zz = yloc
        elseif(iwall==4)then
            yynorm = 1.0d0
            xx = xloc
            yy = 0.0d0
            zz = yloc
        elseif(iwall==5)then
            xxnorm = 1.0d0
            xx = 0.0d0
            yy = ysize
            zz = yloc
        elseif(iwall==6)then
            zznorm = 1.0d0
            xx = xloc
            yy = ysize
            zz = 0.0d0
        endif
        if(iwall/=0)then
            xxtarg(trgcenx,itarg) = xx
            xxtarg(trgceny,itarg) = yy
            xxtarg(trgcenz,itarg) = zz
            xxtarg(trgnormx,itarg) = xxnorm
            xxtarg(trgnormy,itarg) = yynorm
            xxtarg(trgnormz,itarg) = zznorm
            xloc = xx
            yloc = yy
            zloc = zz
            iwall2 = map6(iwall)
            if(switch(iwall2,iroom))then
                cxtarg(itarg) = cname(iwall2,iroom)
            else
                cxtarg(itarg) = ' '
            endif
        endif

        ! center coordinates need to be within room
        if(xloc<0.0d0.or.xloc>xsize.or.yloc<0.0d0.or.yloc>ysize.or.zloc<0.0d0.or.zloc>zsize)then
            write(logerr,'(a,i3,1x,3f10.3)') 'Target located outside of compartment', iroom, xloc, yloc, zloc
            ierror = 214
            return
        endif
    end do

    ! add a target in the center of the floor of each room
    do iroom = 1, nm1
        ntarg = ntarg + 1
        ixtarg(trgroom,ntarg) = iroom
        ixtarg(trgmeth,ntarg) = steady
        ixtarg(trgback,ntarg) = ext

        xx = br(iroom)*0.50d0
        yy = dr(iroom)*0.50d0
        zz = x0
        xxtarg(trgcenx,ntarg) = xx
        xxtarg(trgceny,ntarg) = yy
        xxtarg(trgcenz,ntarg) = zz
        xxtarg(trgnormx,ntarg) = x0
        xxtarg(trgnormy,ntarg) = x0
        xxtarg(trgnormz,ntarg) = 1.0d0
        xxtarg(trginterior,ntarg) = 0.5

        if(switch(2,iroom))then
            cxtarg(ntarg) = cname(2,iroom)
        else
            cxtarg(ntarg) = ' '
        endif
    end do

    return
    end

    subroutine initwall(tstop,ierror)

    !     routine: initspec
    !     purpose: This routine initializes data structures associated
    !             with walls and targets
    !     Arguments: TSTOP
    !                IERROR  Returns error codes

    !        fkw = thermal conductivity
    !        cw = specific heat (j/kg)
    !        rw = density of the wall (kg/m**3)
    !        flw = thickness of the wall (m)
    !        epw = emmisivity of the wall
    !        nslb = discretization of the wall slabs (number of nodes)
    !        cname contains the name of the thermal data subset in the tpp datafile 
    !        switch is a logical for thermal calculation on/off
    !        thset is a switch for a properly transferred data set
    !        maxct is a count of the number of tpp data sets in the database
    !        switch is set if calculation is called for
    !        thset is set if a name in the list of requested data sets matches one of the names in the list of data set names (nlist).
    !        the data from the data base is stored in the local variables lfkw,lcw,lrs,lflw and lepw and is transferred to fkw...

    use cenviro
    use cfast_main
    use fltarget
    use thermp
    use wnodes
    implicit none

    real(8) :: tstop
    integer :: i, j, jj, k, icode, itarg, ifromr, itor, ifromw, itow, ierror, nslabf, nslabt, nptsf, nptst, wfrom, wto
    character off*8, none*8, tcname*8

    ! tp is the pointer into the data base for each material
    integer tp

    data off /'OFF'/, none /'NONE'/

    ! map the thermal data into its appropriate wall specification
    ! if name is "OFF" or "NONE" then just turn all off
    do i = 1, nwal
        do j = 1, nm1
            thset(i,j) = .false.
            if (switch(i,j)) then
                if (cname(i,j)==off.or.cname(i,j)==none) then
                    switch(i,j) = .false.
                else
                    call gettpp(cname(i,j),tp,ierror)
                    if (ierror/=0) return
                    nslb(i,j) = lnslb(tp)
                    do k = 1, nslb(i,j)
                        fkw(k,i,j) = lfkw(k,tp)
                        cw(k,i,j) = lcw(k,tp)
                        rw(k,i,j) = lrw(k,tp)
                        flw(k,i,j) = lflw(k,tp)
                    end do
                    epw(i,j) = lepw(tp)
                    do k = 1, 7
                        hclbf(k,i,j) = lhclbf(k,tp)
                    end do
                endif
            endif
        end do
    end do

    ! Initialize the interior temperatures to the interior ambient
    do i = 1, nm1
        do j = 1, nwal
            twe(j,i) = eta(i)
            do k = 1, nn 
                twj(k,i,j) = tamb(i)
            end do
        end do
    end do

    ! initialize temperature profile data structures
    do i = 1, nm1
        do j = 1, nwal
            if (switch(j,i)) then
                call wset(numnode(1,j,i),nslb(j,i),tstop,walldx(1,i,j),wsplit,fkw(1,j,i),cw(1,j,i),rw(1,j,i),flw(1,j,i),wlength(i,j),twj(1,i,j),tamb(i),eta(i))
            endif
        end do
    end do

    ! concatenate slab properties of wall nodes that are connected to each other
    do i = 1, nswal
        ifromr = izswal(i,1)
        ifromw = izswal(i,2)
        itor = izswal(i,3)
        itow = izswal(i,4)

        nslabf = nslb(ifromw,ifromr)
        nslabt = nslb(itow,itor)
        nslb(ifromw,ifromr) = nslabf + nslabt
        nslb(itow,itor) = nslabf + nslabt

        nptsf = numnode(1,ifromw,ifromr)
        nptst = numnode(1,itow,itor)
        numnode(1,itow,itor) = nptsf + nptst - 1
        numnode(1,ifromw,ifromr) = nptsf + nptst - 1

        wfrom = wlength(ifromr,ifromw)
        wto = wlength(itor,itow)
        wlength(ifromr,ifromw) = wfrom + wto
        wlength(itor,itow) = wfrom + wto

        jj = nslabt + 1
        do j = nslabf+1, nslabf+nslabt
            jj = jj - 1
            fkw(j,ifromw,ifromr) = fkw(jj,itow,itor)
            cw(j,ifromw,ifromr) =  cw(jj,itow,itor)
            rw(j,ifromw,ifromr) =  rw(jj,itow,itor)
            flw(j,ifromw,ifromr) = flw(jj,itow,itor)
            numnode(j+1,ifromw,ifromr) = numnode(jj+1,itow,itor)
        end do

        jj = nslabf + 1
        do j = nslabt+1, nslabt+nslabf
            jj = jj - 1
            fkw(j,itow,itor) = fkw(jj,ifromw,ifromr)
            cw(j,itow,itor) =  cw(jj,ifromw,ifromr)
            rw(j,itow,itor) =  rw(jj,ifromw,ifromr)
            flw(j,itow,itor) = flw(jj,ifromw,ifromr)
            numnode(j+1,itow,itor) = numnode(jj+1,ifromw,ifromr)
        end do

        jj = nptst 
        do j = nptsf+1,nptsf+nptst - 1
            jj = jj - 1
            twj(j,ifromr,ifromw) = twj(jj,itor,itow)
            walldx(j-1,ifromr,ifromw) = walldx(jj,itor,itow)
        end do

        jj = nptsf 
        do j = nptst+1,nptst+nptsf - 1
            jj = jj - 1
            twj(j,itor,itow) = twj(jj,ifromr,ifromw)
            walldx(j-1,itor,itow) = walldx(jj,ifromr,ifromw)
        end do
    end do

    ! initialize target data structures
    do itarg = 1, ntarg
        tcname = cxtarg(itarg)
        if(tcname==' ')then
            tcname = 'DEFAULT'
            cxtarg(itarg) = tcname
        endif
        icode = 0
        call gettpp(tcname,tp,ierror)
        if (ierror/=0) return
        xxtarg(trgk,itarg) = lfkw(1,tp)
        xxtarg(trgcp,itarg) = lcw(1,tp)
        xxtarg(trgrho,itarg) = lrw(1,tp)
        xxtarg(trgl,itarg) = lflw(1,tp)
        xxtarg(trgemis,itarg) = lepw(tp)
    end do

    return
    end

    subroutine offset (ierror)

    ! routine: initspec
    ! purpose: offset in the following context is the beginning of the vector for that particular variable minus one.  thus, the actual pressure array
    !          goes from nofp+1 to nofp+nm1.  the total number of equations to be considered is nequals, and is the last element in the last vector.
    !          each physical interface routine is responsible for the count of the number of elements in the vector for which it is resonsible.
    ! arguments:  ierror  returns error codes

    ! this set of parameters is set by nputp and is kept in the environment module cenviro.  to index a variable, the list is something
    ! like (for temperature in this case)

    ! noftu+1, noftu+nm1

    ! the structure of the solver array is

    ! nofp = offset for the main pressure; the array of base pressures for each compartment
    ! nofpmv = offset for hvac node pressuers
    ! noftmv = offset for hvac branch temperatures
    ! noftu = upper layer temperature
    ! nofvu = upper layer volume
    ! noftl = lower layer temperature
    ! noftt = target temperatures
    ! nofwt = wall surface temperatures (equivalent to the number of profiles)
    ! nofprd = species
    ! nofhcl = surface deposition of hydrogen chloride
    ! nofsmkw = surface deposition of soot
    ! nofsmk = gas phase agglomeration of soot
    ! nequals = last element in the array.

    ! the arrays which use this structure are vatol, vrtol, p, pdold, pprime and pdzero

    ! an important note - solve sets the last variable to be solved to nofprd which is the beginning of the species (-1) and the end of the array which is presently used by dassl
    use cenviro
    use cfast_main
    use fltarget
    use opt
    use params
    use wnodes
    implicit none
    
    integer :: i, j, ib, ierror, itarg, nimtarg, noxygen

    ! count the of nodes (largest of ns and ne)
    nnode = max(na(1),ne(1))
    do ib = 2, nbr
        nnode = max(nnode,na(ib),ne(ib))
    end do
    if (nnode>mnode) then
        call xerror('offset - node range exceeded for hvac',0,1,1)
        ierror = 16
        return
    endif

    ! set the number of compartments and offsets
    nm1 = n - 1

    ! count the species 
    nlspct = 0

    if (lfbt==1) then
        do i = 1, ns
            if (allowed(i).and.activs(i)) then
                nlspct = nlspct + 1
            endif
        end do
    else if (lfbt==2.or.lfbt==0) then
        do i = 1, ns
            if (allowed(i)) then
                if (activs(i)) then
                    nlspct = nlspct + 1
                endif
            else if (i/=7) then
                nlspct = nlspct + 1
            endif
        end do
        nlspct = nlspct + 1
    else
        stop ' not an allowed fire type'
    endif

    ! count the number of walls
    nwalls = 0
    do i = 1, nm1
        do j = 1, nwal
            if (switch(j,i)) then
                nwalls = nwalls + 1
            endif
            if (nwpts/=0) numnode(1,j,i) = nwpts
        end do
    end do

    ! count the number of implicit targets
    nimtarg = 0
    neqtarg(mplicit) = 0
    neqtarg(steady) = 0
    neqtarg(xplicit) = 0
    do itarg = 1, ntarg
        if(ixtarg(trgmeth,itarg)==mplicit)then
            nimtarg = nimtarg + 1
            neqtarg(mplicit) = neqtarg(mplicit) + 1
        elseif(ixtarg(trgmeth,itarg)==steady)then
            neqtarg(steady) = neqtarg(steady) + 1
        elseif(ixtarg(trgmeth,itarg)==xplicit)then
            neqtarg(xplicit) = neqtarg(steady) + 1
        endif
    end do

    ! set number of implicit oxygen variables
    if(lfbt==1)option(foxygen) = off
    if(option(foxygen)==on)then
        noxygen = nm1
    else
        noxygen = 0
    endif

    ! now do all the equation offsets
    nhvpvar = nnode - next
    nhvtvar = nbr
    nofp = 0
    nofpmv = nofp + nm1
    noftmv = nofpmv + nhvpvar
    noffsm = noftmv + nhvtvar
    noftu = noffsm
    nofvu = noftu + nm1
    noftl = nofvu + nm1
    nofoxyl = noftl + nm1
    nofoxyu = nofoxyl + noxygen
    noftt = nofoxyu + noxygen
    nofwt = noftt + nimtarg
    nofprd = nofwt + nwalls
    nofhcl = nofprd + 2 * nm1 * nlspct
    nofsmkw = nofhcl + 4 * nm1 * hcldep
    nofsmk = nofsmkw + 4 * nm1 * smkagl
    nofhvpr = nofsmk + 4 * nm1 * smkagl

    ! if the hvac model is used then nequals needs to be redefined in hvmap since the variable nhvsys is not defined yet.  after nhvsys is defined the following statement can be used to define nequals
    ! nequals = nofhvpr + nhvsys*nlspct
    nequals = nofhvpr

    return
    end subroutine offset

    subroutine roomcon(tsec)

    ! routine: roomcon
    ! purpose: this routine determines whether flow from each room can reach the outside (perhaps through intermediate rooms) via horizontal or vertical vents.  if a room is 
    !            isolated from the outside then snsqe has trouble finding an initial pressure solution.
    ! arguments: tsec: current simulation time 

    use cenviro
    use cfast_main
    use params
    use vents
    implicit none

    real(8) :: factor2, qchfraction, height, width, tsec, avent
    integer roomc(nr,nr), tempmat(nr,nr), i, j, iroom1, iroom2, ik, im, ix, matiter
    integer, parameter :: toprm = 1, botrm = 2

    ! initially assume that no rooms are connected
    do i = 1, n
        do j = 1, n
            roomc(i,j) = 0
        end do
        roomc(i,i) = 1
    end do

    ! check horizontal vent flow
    do i = 1, nvents
        iroom1 = izvent(i,1)
        iroom2 = izvent(i,2)
        ik = izvent(i,3)
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        factor2 = qchfraction(qcvh,ijk(im,ix,ik),tsec)
        height = zzvent(i,2) - zzvent(i,1)
        width = zzvent(i,3)
        avent = factor2 * height * width
        if(avent/=0.0d0)then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        endif
    end do

    ! check vertical vent flow
    do i = 1, nvvent
        iroom1 = ivvent(i,toprm)
        iroom2 = ivvent(i,botrm)
        if(vvarea(iroom1,iroom2)/=0.0d0)then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        endif
    end do

    ! construct roomc**matiter where matiter > n
    ! note:  roomc is a transitiion matrix (from markov chain theory). that is, roomc(i,j) is zero if there no connection between room and room j.  similarly, roomc(i,j) is one if there
    !        is a connection between these two rooms.  roomc is symmetric. the matrix roomc**2 is tells us whether flow can get from room i to room j in two steps.  since there are only n rooms,
    !        roomc**n tells us whether any given room is connected to any other room in n steps.  the entries roomc**n(i,n) then indicates whether a room is connected to the outside (perhaps
    !        through several other intermediate rooms).
    matiter = 1
    do i = 1, n
        if(n<=matiter) exit
        call mat2mult(roomc,tempmat,nr,n)
        matiter = matiter*2
    end do

    do i = 1, nm1
        if(roomc(i,n)/=0)then
            izcon(i) = .true.
        else
            izcon(i) = .false.
        endif
    end do

    return
    end subroutine roomcon

    subroutine wset(numnode,nslab,tstop,walldx,wsplit,wk,wspec,wrho,wthick,wlen,wtemp,tamb,text)

    ! routine: wset
    ! purpose: initializes temperature profiles, breakpoints used in wall conduction calculations.
    ! arguments: numnode  number of nodes in each slab
    !            nslab    number of slabs
    !            tstop    final simulation time
    !            walldx   wall position points
    !            wsplit   fraction of points assigned to slabs 1, 2 and 3
    !            wk       wall thermal conductivity
    !            wspec    wall specific heat
    !            wrho     wall density
    !            wthick   thickness of each slab
    !            wlen     length of wall
    !            wtemp    wall temperature profile
    !            tamb     ambient temperature seen by interior wall
    !            text     ambient temperature seen by exterior wall

    implicit none

    real(8) :: walldx(*), wk(*), wspec(*), wrho(*), wthick(*), wtemp(*), wsplit(*), xwall(100), xpos(10), xxnx, wlen, errfc05, xkrhoc, alpha, xb, tstop, xxnsplit, w, xxim1, xxiim1, &
        wmxb, xxnslabm2, xxnint, xxi1, xxi2, xxi3, xxnintx, tamb, text, dtdw
    integer :: numnode(*), cumpts(10), numpts(10), i, ii, nx, nintx, nslab, nsplit, islab, isum, nint, ibeg, iend

    nx = numnode(1)
    xxnx = nx

    nintx = nx - (nslab+1)
    if (nslab<=2) then
        nsplit = (wsplit(1)+wsplit(2)) * xxnx
    else
        nsplit = wsplit(1) * xxnx
    endif

    ! calculate total walldepth
    xpos(1) = 0.0d0
    do islab = 1, nslab
        xpos(islab+1) = xpos(islab) + wthick(islab)
    end do
    wlen = xpos(nslab+1)

    ! calculate break point based on first slab's properties
    errfc05 = 1.30d0
    xkrhoc = wk(1) / (wspec(1)*wrho(1))
    alpha = sqrt(xkrhoc)
    xb = 2.0d0 * alpha * sqrt(tstop) * errfc05 * wlen
    if (xb>.50d0*wlen) xb = .5d0 * wlen
    if (nslab==1) then

        ! set up wall node locations for 1 slab case, bunch points at interior and exterior boundary
        xxnsplit = nsplit
        w = 1.0d0 / xxnsplit 
        do i = 1, nsplit + 1 
            xxim1 = i - 1
            xwall(i) = xb * (xxim1*w) ** 2
        end do
        w = 1.0d0 / (xxnx-(xxnsplit+1.0d0))
        do i = nsplit +2, nx
            ii = nx + 1 - i 
            xxiim1 = ii - 1
            xwall(i) = wlen - (wlen-xb) * (xxiim1*w) ** 2
        end do
        numnode(1+nslab) = nintx
    else

        ! set up wall node locations for multi-slab case, bunch points at interior boundary of first slab, exterior boundary of last slab and uniformly in middle slabs

        ! calculate number of points interior to each slab
        xxnintx = nintx
        numpts(1) = wsplit(1) * xxnintx * min(xb,wthick(1)) / wlen
        if (numpts(1)<1) numpts(1) = 1
        wmxb = wlen - xb
        numpts(nslab) = wsplit(3) * xxnintx * min(wmxb,wthick(nslab))/ wlen
        if (numpts(nslab)<1) numpts(nslab) = 1
        isum = nintx - numpts(1) - numpts(nslab)
        xxnslabm2 = nslab - 2
        do i = 2, nslab - 1
            numpts(i) = xxnx * wsplit(2)*wthick(nslab)/xxnslabm2/wlen
            if (numpts(i)<1) numpts(i) = 1
            isum = isum - numpts(i)
        end do
        numpts(1) = numpts(1) + (isum-isum/2)
        numpts(nslab) = numpts(nslab) + isum / 2
        if (numpts(nslab)<1) then
            numpts(1) = numpts(1) + numpts(nslab) - 1
            numpts(nslab) = 1
        endif

        ! copy numpts data into numnode and keep a running total
        cumpts(1) = 1
        do islab = 1, nslab
            numnode(1+islab) = numpts(islab)
            cumpts(islab+1) = cumpts(islab) + numpts(islab) + 1
        end do

        ! calculate wall positions for first slab (bunched near left)
        nint = numpts(1) + 1
        xxnint = nint
        do i = 1, nint
            xxim1 = i - 1
            xwall(i) = xxim1 ** 2 * xpos(2) / xxnint**2
        end do

        ! calculate wall positions for middle slabs (uniform)
        do islab = 2, nslab - 1
            ibeg = cumpts(islab)
            iend = cumpts(islab+1) - 1
            xxi3 = iend+1-ibeg
            do i = ibeg, iend
                xxi1 = iend+1-i
                xxi2 = i-ibeg
                xwall(i) = (xpos(islab)*xxi1+xpos(islab+1)*xxi2) / xxi3
            end do
        end do

        ! calculate wall positions for last slab (bunched near right)
        if (nslab>=2) then
            ibeg = cumpts(nslab)

            ! include last point for last slab
            iend = cumpts(nslab+1)
            xxi3 = iend - ibeg
            do i = ibeg, iend
                xxi1 = iend - i
                xwall(i) = xpos(nslab+1) - xxi1 ** 2 * (xpos(nslab+1) - xpos(nslab)) / xxi3 ** 2
            end do
        endif
    endif

    ! finally calculate distances between each point these distances are used by cnduct to setup discretization tri-diagonal matrix
    do i = 1, nx - 1
        walldx(i) = xwall(i+1) - xwall(i)
    end do

    ! initialize temperature profile.  note, wtemp(1)=wtemp(2) and wtemp(nx)=wtemp(nx-1) so dassl will think that no heat transfer needs to occur to the wall (since dt/dx=0 here)
    wtemp(1) = tamb
    wtemp(nx) = text
    dtdw = (text-tamb)/(xwall(nx-1)-xwall(2))
    do i = 2, nx-1
        wtemp(i) = tamb + (xwall(i)-xwall(2))*dtdw
    end do
    return
    end

    integer function rev_initialization ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_initialization = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_initialization