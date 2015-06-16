
! --------------------------- gettpp -------------------------------------------

    subroutine gettpp (name, tp, errorcode)

    !     Routine: gettpp
    !     Purpose: check for and return index to a thermal property
    !     Revision: $Revision$
    !     Revision Date: $Date$

    use thermp

    implicit none
    character, intent(in) :: name*(*)
    integer, intent(out) :: errorcode
    
    character(mxthrmplen) missingtpp
    integer tp, i

    errorcode = 0
    do i = 1, maxct
        tp = i
        if (name==nlist(i)) return
    end do
    missingtpp = name
    errorcode = 205
    write(3,'(''A thermal property was not found in the input file. Missing material: '',a)') missingtpp
    return
    end

! --------------------------- gres -------------------------------------------

    subroutine gres(nnn,hvpsolv,deltamv,iflag)

    !     routine: gres
    !     purpose: calculates residuals for initial solution by snsqe
    !     revision: $revision: 352 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: nnn
    !                hvpsolv
    !                deltamv
    !                iflag

    use precision_parameters
    use cenviro
    use cfast_main
    use cshell
    use opt
    use params
    use solver_parameters
    implicit none

    integer, intent(in) :: nnn
    real(eb), intent(in) :: hvpsolv(nnn)
    integer, intent(out) :: iflag
    real(eb), intent(out) :: deltamv(*)
    
    integer :: nalg, i, ires
    real(eb) :: p2(maxteq), delta(maxteq), pdzero(maxteq), T
    
    data pdzero /maxteq*0.0_eb/
    
    if(1.eq.2)iflag=-1 ! dummy statement to eliminate compiler warnings
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
    call calculate_residuals(t,p2,pdzero,delta,ires,rpar2,ipar2)
    do i = 1, nalg
        deltamv(i) = delta(i)
    end do
    do i = 1, nm1
        if(.not.izcon(i))deltamv(i) = 0.0_eb
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

! --------------------------- gres2 -------------------------------------------

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

    use precision_parameters
    use cfast_main
    use cshell
    use opt
    use params
    use solver_parameters
    implicit none

    integer, intent(in) :: nnn
    real(eb), intent(in) :: hvsolv(nnn)
    integer, intent(out) :: iflag
    real(eb), intent(out) :: deltamv(*)

    real(eb) :: p2(maxteq), delta(maxteq), pdzero(maxteq), t
    integer :: i, ires
    data pdzero /maxteq*0.0_eb/
    
    if(1.eq.2)iflag=-1 ! dummy statement to eliminate compiler warnings
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
    call calculate_residuals(t,p2,pdzero,delta,ires,rpar2,ipar2)
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

! --------------------------- hvinit -------------------------------------------

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

    use precision_parameters
    use cenviro
    use cfast_main
    use params
    implicit none

    integer, intent(out) :: ierror
    
    real(eb) :: c3(ns), f, xxjm1, s1, s2, xnext, pav, tav, df, xx, rden
    integer :: i, ii, j, k, ib, id, isys, lsp

    !    calculate min & max values for fan curve

    do k = 1, nfan
        f = hvbco(k,1)
        df = 0.0_eb
        xx = 1.0_eb
        do j = 2, nfc(k)
            xxjm1 = j - 1
            df = df + xxjm1*hvbco(k,j)*xx
            xx = xx*hmin(k)
            f = f + hvbco(k,j)*xx
        end do
    end do
    do k = 1, nfan
        f = hvbco(k,1)
        df = 0.0_eb
        xx = 1.0_eb
        do j = 2, nfc(k)
            xxjm1 = j - 1
            df = df + xxjm1*hvbco(k,j)*xx
            xx = xx*hmax(k)
            f = f + hvbco(k,j)*xx
        end do
        ! prevent negative flow
        qmax(k) = max(0.0_eb,f)
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
        if (ncnode(i)<1.or.ncnode(i)>mxcon) then
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
            call xerror('hvinit - interior node has too many or too few connections',0,1,1)
            ierror = 223
            return
        endif
        hvelxt(ii) = min(room_height(i),max(0.0_eb,hvelxt(ii)))
        hvght(j) = hvelxt(ii) + floor_height(i)
    end do

    ! assign compartment pressure & temperature data to exterior nodes of the hvac network
    do i = 1, nnode
        hvp(i) = -1.0_eb
    end do
    do i = 1, nbr
        hvdara(i) = 0.0_eb
        hvdvol(i) = 0.0_eb
        hvconc(i,1) = -1.0_eb
        tbr(i) = -1.0_eb
    end do

    s1 = 0.0_eb
    s2 = 0.0_eb
    do lsp = 1, ns
        c3(lsp) = 0.0_eb
    end do
    do ii = 1, next
        i = hvnode(1,ii)
        j = hvnode(2,ii)
        ib = icmv(j,1)
        ! the outside is defined to be at the base of the structure for mv
        if (i<n) then
            hvextt(ii,upper) = interior_temperature
            hvextt(ii,lower) = interior_temperature
            hvp(j) = zzrelp(i) - grav_con*interior_density*hvelxt(ii)
        else
            hvextt(ii,upper) = exterior_temperature
            hvextt(ii,lower) = exterior_temperature
            hvp(j) = exterior_abs_pressure - grav_con*exterior_density*hvelxt(ii)
        endif
        tbr(ib) = hvextt(ii,upper)
        s1 = s1 + hvp(j)
        s2 = s2 + tbr(ib)
        do lsp = 1, ns
            ! the outside is defined to be at the base of the structure for mv
            if (i<n) then
                hvexcn(ii,lsp,upper) = o2n2(lsp)*interior_density
                hvexcn(ii,lsp,lower) = o2n2(lsp)*interior_density
            else
                hvexcn(ii,lsp,upper) = o2n2(lsp)*exterior_density
                hvexcn(ii,lsp,lower) = o2n2(lsp)*exterior_density
            endif
            hvconc(j,lsp) = hvexcn(ii,lsp,upper)
            c3(lsp) = c3(lsp) + hvexcn(ii,lsp,upper)
        end do
    end do

    ! this is to initialize the nodes and branches to something
    ! we will then let the system equilibrate to give us the true answer
    xnext = next
    pav = s1/xnext
    tav = s2/xnext
    do lsp = 1, ns
        c3(lsp) = c3(lsp)/xnext
    end do
    do i = 1, nnode
        if (hvp(i)<0.0_eb) then
            hvp(i) = pav
        endif
    end do
    do i = 1, nbr
        if (tbr(i)<=0.0_eb) tbr(i) = tav
        if (hvconc(i,1)<0.0_eb) then
            do lsp = 1, ns
                hvconc(i,lsp) = c3(lsp)
            end do
        endif
    end do

    ! calculate area, relative roughness, effective diameter and volume of ducts
    do id = 1, ndt
        duct_area(id) = (pi*eff_duct_diameter(id)**2)/4.0_eb
        ib = ibrd(id)
        hvdvol(ib) = hvdvol(ib) + duct_area(id)*duct_length(id)
        hvdara(ib) = hvdara(ib) + pi*eff_duct_diameter(id)*duct_length(id)
    end do


    ! construct hvmap arrays
    call hvmap

    ! define total mass for each hvac system
    do isys = 1, nhvsys
        hvtm(isys) = 0.0_eb
    end do     
    do ib = 1, nbr
        isys = izhvbsys(ib)
        rden = (pofset+pav)/(rgas*tbr(ib))
        hvtm(isys) = hvtm(isys) + rden*hvdvol(ib)
    end do

    ! now that everything is ok, we can turn on ventilation
    mvcalc = .true.
    return
    end

! --------------------------- hvmap -------------------------------------------

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

    ! DASSL only solves interior nodes so zero out exterior nodes
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

! --------------------------- initamb -------------------------------------------

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

    use precision_parameters
    use cenviro
    use cfast_main
    use fltarget
    use opt
    use params
    implicit none

    integer, intent(in) :: iflag
    real(eb), intent(out) :: yinter(*)
    
    real(eb) :: dummy(1), xxpmin, tdspray, tdrate, scale, dnrm2
    integer i, ii, iwall, iroom, itarg, ieq

    ! simplify and make initial pressure calculations consistent.  inside pressures
    ! were calculated using rho*g*h .  but outside pressures were calculated using
    ! atmosp.  fictional flows resulted making  snsqe work a log harder to get
    ! an initial solution.  the initial temperature values calculated by atmosp
    ! at the top of the empire state building (about 400 m above base) is only
    ! about 0.2 k different that at the base.  
    do i = 1, nm1
        interior_rel_pressure(i) = -interior_density*grav_con*floor_height(i)
        exterior_rel_pressure(i) = -exterior_density*grav_con*floor_height(i)
    end do
    exterior_rel_pressure(n) = 0.0_eb


    ! normalize pressures so that the smallest pressure is zero
    xxpmin = min(interior_rel_pressure(1),exterior_rel_pressure(1))
    do i = 2, nm1
        xxpmin = max(xxpmin,interior_rel_pressure(i),exterior_rel_pressure(i))
    end do
    do i = 1, nm1
        exterior_rel_pressure(i) = exterior_rel_pressure(i) - xxpmin
        interior_rel_pressure(i) = interior_rel_pressure(i) - xxpmin
    end do
    pofset = pofset + xxpmin
    interior_abs_pressure = interior_abs_pressure + xxpmin - pofset
    exterior_abs_pressure = exterior_abs_pressure + xxpmin - pofset

    ! copy all of the variables from the initial values into the data arrays
    call update_data (dummy,constvar)

    ! define the p array, the solution to the ode
    do i = 1, nm1
        p(i) = interior_rel_pressure(i)
        p(i+noftu) = interior_temperature

        ! check for a special setting of the interface height
        if (iflag==1) then
            if (yinter(i)<0.0_eb) then
                p(i+nofvu) = zzvmin(i)
            else
                p(i+nofvu) = min(zzvmax(i),max(zzvmin(i),yinter(i)*room_area(i)))
            endif
            yinter(i) = 0.0_eb
        endif
        if(izshaft(i)==1)p(i+nofvu) = zzvmax(i)
        p(i+noftl) = interior_temperature
    end do

    ! define hvac pressures and temperatures.  these values are later refined by 
    ! snsqe so that each hvac node conserves mass and energy
    do i = 1, nhvpvar
        p(i+nofpmv) = 0.0_eb
    end do
    do i = 1, nhvtvar
        p(i+noftmv) = interior_temperature
    end do

    ! define interior surface wall temperatures
    ii =nofwt 
    do i = 1, nm1
        do iwall = 1, nwal
            if (switch(iwall,i)) then
                ii = ii + 1
                p(ii) = interior_temperature
            endif
        end do
    end do

    ! establish default values for detector data
    do i = 1, ndtect
        iroom=ixdtect(i,droom)
        if(xdtect(i,dxloc)<0.0_eb)xdtect(i,dxloc)=room_width(iroom)*.5_eb
        if(xdtect(i,dyloc)<0.0_eb)xdtect(i,dyloc)=room_depth(iroom)*.5_eb
        if(xdtect(i,dzloc)<0.0_eb)then
            xdtect(i,dzloc)=ceiling_height(iroom)+xdtect(i,dzloc)
        endif
        tdspray = xdtect(i,dspray)

        ! if tdspray>0 then interpret it as a spray density and convert
        ! to a characteristic quenching time
        ! if tdspray < 0 then interpret abs(tdspray) as the time
        ! required to reduce the fire size by 50 per cent
        ! if tdspray = 0 then turn the sprinkler off
        if(tdspray>0.0_eb)then
            tdrate = 3.0_eb/tdspray**1.8_eb
        elseif(tdspray<0.0_eb)then
            tdrate = abs(tdspray)/log(2.0_eb)
            tdspray = (3.0_eb/tdrate)**(1.0_eb/1.8_eb)
        else
            tdspray = 0.0_eb
            tdrate = 1.0e10_eb
            ixdtect(i,dquench) = 0
        endif

        ! set initial ceiling jet and detector link temperatures to ambient
        xdtect(i,dspray) = tdspray
        xdtect(i,dthalf) = tdrate*log(2.0_eb)
        xdtect(i,drate) = tdrate
        xdtect(i,dtemp) = interior_temperature
        xdtect(i,dtempo) = interior_temperature
        xdtect(i,dtjet) = interior_temperature
        xdtect(i,dtjeto) = interior_temperature
    end do

    call sortbrm(xdtect,mxdtect,ixdtect,mxdtect,ndtect,dtxcol,dticol,droom,nr,nm1,idtpnt)

    ! p's for pressure, volume and temperature are defined
    ! we can now copy these values to the environment variables
    call update_data (p, odevara)

    ! initialize target temperatures
    do itarg = 1, ntarg
        iroom = ixtarg(trgroom,itarg)
        if(ixtarg(trgmeth,itarg)==mplicit)then
            ieq = iztarg(itarg)
            p(noftt+ieq) = interior_temperature
        endif  
        do i=idxtempf_trg,idx_tempb_trg
            xxtarg(i,itarg)=interior_temperature
        end do
        tgtarg(itarg) = interior_temperature

        ! scale normal vectors to have length 1
        scale = 1.0_eb/dnrm2(3,xxtarg(trgnormx,itarg),1)
        call dscal(3,scale,xxtarg(trgnormx,itarg),1)
    end do

    ! initialize solver oxygen values if required.   (must be initialized
    ! after zzmass is defined)
    if(option(foxygen)==on)then
        do iroom = 1, nm1
            p(iroom+nofoxyu)=0.23_eb*zzmass(iroom,upper)
            p(iroom+nofoxyl)=0.23_eb*zzmass(iroom,lower)
        end do
    endif

    ! define ihxy in izhall (dimension that is longest)
    do i = 1, nm1
        if(izhall(i,ishall)==1)then
            if(room_depth(i)>room_width(i))then
                izhall(i,ihxy) = 1
            else
                izhall(i,ihxy) = 2
            endif
        endif
    end do

    return
    end

! --------------------------- initialize_memory -------------------------------------------

    subroutine initialize_memory

    !     routine: initialize_memory
    !     purpose: This routine initializes the main memory
    !     Arguments: none

    use precision_parameters
    use cenviro
    use cfast_main
    use cshell
    use fltarget
    use params
    use thermp
    use vents
    implicit none
    
    real(eb) :: xlrg
    integer :: i, j, k, ivent, itarg, lsp, nfurn

    ! set some initialization - simple control stuff
    exset = .false.
    debugging = .false.

    ! initialize the common block
    do i = 1, ns
        o2n2(i) = 0.0_eb
        allowed(i) = .false.
        activs(i) = .true.
    end do
    do i = 1, nr
        do j = 1, nwal
            cname(j,i) = 'OFF'
            switch(j,i) = .false.
        end do
    end do
    adiabatic_wall = .false.
    do i = 1, nr
        deadroom(i) = 0
        izhall(i,ishall) = 0
    end do
    do i = 1, nr
        switch(1,i) = .true.
        cname(1,i) = 'DEFAULT'
    end do
    nconfg = 0
    nlspct = 0
    nrestr = 0
    numthrm = 0
    n = 0

    ! initialize the flow variables
    do i = 1, nr
        izshaft(i) = 0
        heatup(i) = 0.0_eb
        heatlp(i) = 0.0_eb
        do j = 1, nr
            ! do vertical vents (vvent,...)
            vshape(i,j) = 0
            nwv(i,j) = 0
            vvarea(i,j) = 0.0_eb
            ! do horizontal vents (hvent,...)
            nw(i,j) = 0
        end do
    end do

    do i = 1, mxext
        hveflot(upper,i) = 0.0_eb
        hveflot(lower,i) = 0.0_eb
        tracet(upper,i) = 0.0_eb
        tracet(lower,i) = 0.0_eb
    end do

    ! initialize the forcing functions
    do i = 1, nr
        do k = upper, lower
            qfc(k,i) = 0.0_eb
        end do
    end do
    do i = 1, maxteq
        p(i) = 0.0_eb
    end do

    ! define the outside world as infinity
    xlrg = 1.0e+5_eb
    do i = 1, nr
        room_depth(i) = xlrg
        room_width(i) = xlrg
        room_height(i) = xlrg
        ceiling_height(i) = xlrg
        floor_height(i) = 0.0_eb
        floor_height(i) = 0.0_eb
        cxabs(i) = 0.0_eb
        cyabs(i) = 0.0_eb
        cxgrid(i) = 50
        cygrid(i) = 50
        czgrid(i) = 50
        room_area(i) = room_width(i)*room_depth(i)
        room_volume(i) = room_height(i)*room_area(i)
        do  j = 1, nwal
            epw(j,i) = 0.0_eb
            qscnv(j,i) = 0.0_eb
        end do
        do j = 1, nr
            nw(i,j) = 0
        end do
    end do

    ! initialize all vents to zero size
    do ivent = 1, mxhvents
        bw(ivent) = 0.0_eb
        hh(ivent) = 0.0_eb
        hl(ivent) = 0.0_eb
        hhp(ivent) = 0.0_eb
        hlp(ivent) = 0.0_eb
        vface(ivent) = 1
    end do

    ! set the time step and inner step division for time splitting
    ! we do not let the user choose these
    deltat = 1.0_eb

    ! define all the "universal constants
    cp = 1012.0_eb
    gamma = 1.40_eb
    rgas = (gamma-1.0_eb)/gamma*cp
    minmas = 0.0_eb
    stime = 0.0_eb
    tref = 293.15_eb
    limo2 = 0.10_eb
    hcomba = 50000000.0_eb
    pref = 101325.0_eb
    interior_abs_pressure = pref
    pofset = pref
    te = tref
    interior_temperature = tref
    tgignt = te + 200.0_eb
    exterior_temperature = interior_temperature
    exterior_abs_pressure = interior_abs_pressure
    relhum = 0.5_eb
    do i = 0, mxfire
        objmaspy(i) = 0.0_eb
        radio(i) = 0.0_eb
        radconsplit(i) = 0.15_eb
    end do
    tradio = 0.0_eb

    ! normal air
    o2n2(1) = 0.77_eb
    o2n2(2) = 0.23_eb

    ! a specified fire in the center of the room
    lfbt = 2
    lfmax = 1
    heatfl = .false.
    heatfp(1) = -1.0_eb
    heatfp(2) = -1.0_eb
    heatfp(3) = -1.0_eb

    ! set to -1 as a flag for nputp initialization - any value not set will be set to the 
    ! default which is the center of the respective wall
    fpos(1) = -1.0_eb
    fpos(2) = -1.0_eb
    fpos(3) = -1.0_eb

    ! set up default values for the chemistry
    do i = 1, mxpts
        hcratio(i) = onethird
    end do

    ! Start with vents open: h for hvent, v for vvent, and m for mvent
    do i = 1,mxhvents
        qcvh(1,i) = 0.0_eb
        qcvh(2,i) = 1.0_eb
        qcvh(3,i) = 0.0_eb
        qcvh(4,j) = 1.0_eb
    end do

    do i = 1, nr
        qcvv(1,i) = 0.0_eb
        qcvv(2,i) = 1.0_eb
        qcvv(3,i) = 0.0_eb
        qcvv(4,i) = 1.0_eb
    end do

    ! note that the fan fraction is unity = on, whereas the filter fraction is unity = 100% filtering since 
    ! there is not "thing" associated with a filter, there is no (as of 11/21/2006) 
    ! way to have an intial value other than 0 (no filtering).
    do i = 1, mxfan
        qcvf(1,i) = 0.0_eb
        qcvf(2,i) = 0.0_eb
        qcvf(3,i) = 0.0_eb
        qcvf(4,i) = 0.0_eb
        qcvm(1,i) = 0.0_eb
        qcvm(2,i) = 1.0_eb
        qcvm(3,i) = 0.0_eb
        qcvm(4,i) = 1.0_eb
    end do

    hcratt = hcratio(1)

    ! turn hvac off initially

    nnode = 0
    nft = 0
    nfan = 0
    nfilter = 0
    nbr = 0
    next = 0
    mvcalc = .false.
    do i = 1, mxnode
        hvght(i) = 0.0_eb
    end do

    ! initialize detectors
    do i = 1, mxdtect
        xdtect(i,drti) = 50.0_eb
        xdtect(i,dspray) = -300.0_eb
        xdtect(i,dxloc) = -1.0_eb
        xdtect(i,dyloc) = -1.0_eb
        xdtect(i,dzloc) = -3.0_eb/39.37_eb
        xdtect(i,dtrig) = 330.3722_eb
        xdtect(i,dvel) = 0.0_eb
        xdtect(i,dvelo) = 0.0_eb
        xdtect(i,dtact) = 99999.0_eb
        ixdtect(i,dtype) = 2
        ixdtect(i,droom) = 1
        ixdtect(i,dquench) = 0
        ixdtect(i,dact) = 0
        ixdtect(i,dactreported) = 0
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
        ixtarg(trgback,itarg) = interior
        cxtarg(itarg) = 'DEFAULT'
    end do

    ! initialize jaccol  
    jaccol = -2
    neqoff = 10

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
        do j = 1, mxcross
            zzrvol(j,i) = 0.0_eb
            zzrarea(j,i) = 0.0_eb
            zzrhgt(j,i) = 0.0_eb
        end do
    end do

    ! initialzie time step checking
    zzdtcrit = 1.0e-09_eb
    izdtnum = 0
    izdtmax = 100
    izdtflag = .true.

    ! initialize inter-compartment heat transfer fractions
    do i = 1, nr
        do j = 1, nr
            zzhtfrac(i,j) = 0.0_eb
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
                zzgspec(i,j,lsp) = 0.0_eb
                zzcspec(i,j,lsp) = 0.0_eb            
            end do
        end do
    end do

    ! initialize number of furnace temperature nodes
    nfurn=0

    return
    end

! --------------------------- initialize_fire_objects -------------------------------------------

    subroutine initialize_fire_objects

    !     routine: initialize_fire_objects
    !     purpose: this routine initializes the fire objects
    !     arguments: none

    use cfast_main
    use objects1
    use objects2
    implicit none
    
    integer :: i

    ! turn off objects
    numobjl = 0
    do i = 0, mxfires
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

! --------------------------- read_solver_ini -------------------------------------------

    subroutine read_solver_ini

    !     routine: read_solver_ini
    !     purpose: this routine initializes the solver variables from solver.ini if it exists
    !     arguments: none

    use precision_parameters
    use cfast_main
    use cshell
    use iofiles
    use opt
    use params
    use solver_parameters
    use wnodes
    implicit none

    real(eb) :: fract1, fract2, fract3, fsum
    integer :: nopt, i, j, ibeg, iend
    logical existed

    ductcv = 0.0_eb

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
    do i = 1, (nopt-1)/5 + 1
        ibeg = 1 + (i-1)*5
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
    wsplit(1) = abs(fract1)/fsum
    wsplit(2) = abs(fract2)/fsum
    wsplit(3) = abs(fract3)/fsum

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
    end subroutine read_solver_ini

! --------------------------- initspecc -------------------------------------------

    subroutine initialize_species

    !     routine: initialize_species
    !     purpose: This routine initializes variables associated with 
    !              species it originally occured in CFAST and INITFS.  It was moved
    !              to one subroutine to make maintenance easier
    !     Arguments: none

    use precision_parameters
    use cenviro
    use cfast_main
    use params
    use thermp
    implicit none

    real(eb) :: xm(2), xt, xtemp, xh2o, toto2n2
    integer i, j, k, ip, iprod, isof, isys, lsp


    do i = 1, nm1
        xm(1) = interior_density*zzvol(i,upper)
        xm(2) = interior_density*zzvol(i,lower)

        !  set the water content to relhum - the polynomial fit is to (t-273), and
        ! is for saturation pressure of water.  this fit comes from the steam
        ! tables in the handbook of physics and chemistry.  we are being clever
        ! here.  the final result in o2n2 should be the value used in stport for
        ! the outside ambient.
        xt = interior_temperature
        xtemp = 23.2_eb - 3.816e3_eb/(xt-46.0_eb)
        xh2o = exp(xtemp)/101325.0_eb*(18.0_eb/28.4_eb)
        o2n2(8) = relhum*xh2o

        ! normalize the atmosphere
        toto2n2 = 0.0_eb
        do j = 1, ns
            toto2n2 = toto2n2 + o2n2(j)
        end do
        do j = 1, ns
            o2n2(j) = o2n2(j)/toto2n2
        end do

        do k = upper, lower
            do lsp = 1, ns
                toxict(i,k,lsp) = 0.0_eb
                mass(k,i,lsp) = o2n2(lsp)*xm(k)
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
    end subroutine initialize_species

! --------------------------- inittarg -------------------------------------------

    subroutine inittarg (ierror)

    !     routine: inittarg
    !     purpose: Initialize target data structures
    !     Arguments: ierror:  Returns error codes

    use precision_parameters
    use cfast_main
    use cshell
    use fltarget
    use thermp
    implicit none
    
    integer, intent(out) :: ierror
    
    real(eb) :: xloc, yloc, zloc, xxnorm, yynorm, zznorm, xsize, ysize, zsize, xx, yy, zz
    integer :: ifail, itarg, iroom, iwall, iwall2
    integer :: map6(6) = (/1,3,3,3,3,2/)

    ifail = 0
    do itarg = 1, ntarg

        ! room number must be between 1 and nm1
        iroom = ixtarg(trgroom,itarg)
        if(iroom<1.or.iroom>nm1)then
            write(logerr,'(a,i3)') '***Error: Target assigned to non-existent compartment',iroom
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
        xsize = room_width(iroom)
        ysize = room_depth(iroom)
        zsize = ceiling_height(iroom)

        ! if the locator is -1, set to center of room on the floor
        if(xloc==-1.0_eb) xloc = 0.5_eb*xsize
        if(yloc==-1.0_eb) yloc = 0.5_eb*ysize
        if(zloc==-1.0_eb) zloc = 0.0_eb
        if(iwall/=0)then
            xxnorm = 0.0_eb
            yynorm = 0.0_eb
            zznorm = 0.0_eb
        endif
        if(iwall==1)then
            zznorm = -1.0_eb
            xx = xloc
            yy = yloc
            zz = zsize
        elseif(iwall==2)then
            yynorm = -1.0_eb
            xx = xsize
            yy = ysize
            zz = yloc
        elseif(iwall==3)then
            xxnorm = -1.0_eb
            xx = xsize
            yy = xloc
            zz = yloc
        elseif(iwall==4)then
            yynorm = 1.0_eb
            xx = xloc
            yy = 0.0_eb
            zz = yloc
        elseif(iwall==5)then
            xxnorm = 1.0_eb
            xx = 0.0_eb
            yy = ysize
            zz = yloc
        elseif(iwall==6)then
            zznorm = 1.0_eb
            xx = xloc
            yy = ysize
            zz = 0.0_eb
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
        if(xloc<0.0_eb.or.xloc>xsize.or.yloc<0.0_eb.or.yloc>ysize.or.zloc<0.0_eb.or.zloc>zsize)then
            write(logerr,'(a,i3,1x,3f10.3)') '***Error: Target located outside of compartment', iroom, xloc, yloc, zloc
            ierror = 214
            return
        endif
    end do

    ! add a target in the center of the floor of each room
    do iroom = 1, nm1
        ntarg = ntarg + 1
        ixtarg(trgroom,ntarg) = iroom
        ixtarg(trgmeth,ntarg) = steady
        ixtarg(trgback,ntarg) = exterior

        xx = room_width(iroom)*0.50_eb
        yy = room_depth(iroom)*0.50_eb
        zz = 0.0_eb
        xxtarg(trgcenx,ntarg) = xx
        xxtarg(trgceny,ntarg) = yy
        xxtarg(trgcenz,ntarg) = zz
        xxtarg(trgnormx,ntarg) = 0.0_eb
        xxtarg(trgnormy,ntarg) = 0.0_eb
        xxtarg(trgnormz,ntarg) = 1.0_eb
        xxtarg(trginterior,ntarg) = 0.5

        if(switch(2,iroom))then
            cxtarg(ntarg) = cname(2,iroom)
        else
            cxtarg(ntarg) = ' '
        endif
    end do

    return
    end

! --------------------------- initialize_walls  -------------------------------------------

    subroutine initialize_walls (tstop,ierror)

    !     routine: initialize_species
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
    !        thset is set if a name in the list of requested data sets matches one of the names in the 
    !              list of data set names (nlist). the data from the data base is stored in the local variables 
    !              lfkw,lcw,lrs,lflw and lepw and is transferred to fkw...

    use precision_parameters
    use wallptrs
    use cenviro
    use cfast_main
    use fltarget
    use thermp
    use wnodes
    implicit none

    integer, intent(out) :: ierror
    real(eb), intent(in) :: tstop
    integer :: i, j, jj, k, icode, itarg, ifromr, itor, ifromw, itow, nslabf, nslabt, nptsf, nptst, wfrom, wto
    character(mxthrmplen) off, none, tcname

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
                endif
            endif
        end do
    end do

    ! Initialize the interior temperatures to the interior ambient
    do i = 1, nm1
        do j = 1, nwal
            do k = 1, nnodes 
                twj(k,i,j) = interior_temperature
            end do
        end do
    end do

    ! initialize temperature profile data structures
    do i = 1, nm1
        do j = 1, nwal
            if (switch(j,i)) then
                call wset(numnode(1,j,i),nslb(j,i),tstop,walldx(1,i,j),wsplit,fkw(1,j,i),cw(1,j,i),rw(1,j,i),flw(1,j,i),&
                   wlength(i,j),twj(1,i,j),interior_temperature,exterior_temperature)
            endif
        end do
    end do

    ! concatenate slab properties of wall nodes that are connected to each other
    do i = 1, nswal
        ifromr = izswal(i,w_from_room)
        ifromw = izswal(i,w_from_wall)
        itor = izswal(i,w_to_room)
        itow = izswal(i,w_to_wall)

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

        do j = 1,nptsf
            twj(j,ifromr,ifromw) = interior_temperature
            twj(j,itor,itow) = interior_temperature
        end do
        jj = nptst 
        do j = nptsf+1,nptsf+nptst - 1
            jj = jj - 1
            twj(j,ifromr,ifromw) = interior_temperature
            walldx(j-1,ifromr,ifromw) = walldx(jj,itor,itow)
        end do

        jj = nptsf 
        do j = nptst+1,nptst+nptsf - 1
            jj = jj - 1
            twj(j,itor,itow) = interior_temperature
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

! --------------------------- offset -------------------------------------------

    subroutine offset (ierror)

    ! routine: initialize_species
    ! purpose: offset in the following context is the beginning of the vector for that particular variable minus one.  
    !          thus, the actual pressure array goes from nofp+1 to nofp+nm1.  the total number of equations to be considered
    !          is nequals, and is the last element in the last vector. each physical interface routine is responsible for 
    !          the count of the number of elements in the vector for which it is resonsible.
    ! arguments:  ierror  returns error codes

    ! this set of parameters is set by nputp and is kept in the environment module cenviro.  
    ! to index a variable, the list is something like (for temperature in this case)

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
    ! nequals = last element in the array.

    ! the arrays which use this structure are vatol, vrtol, p, pdold, pprime and pdzero

    ! an important note - solve_simulation sets the last variable to be solved to nofprd which is the 
    ! beginning of the species (-1) and the end of the array which is presently used by dassl
    
    use cfast_main
    use fltarget
    use opt
    use params
    use wnodes
    implicit none
    
    integer, intent(out) :: ierror
    
    integer :: i, j, ib, itarg, nimtarg, noxygen

    ! count the of nodes (largest of ns and ne)
    nnode = max(na(1),ne(1))
    do ib = 2, nbr
        nnode = max(nnode,na(ib),ne(ib))
    end do
    if (nnode>mxnode) then
        call xerror('offset - node range exceeded for hvac specification',0,1,1)
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
    nofhvpr = nofprd + 2*nm1*nlspct

    ! if the hvac model is used then nequals needs to be redefined in hvmap since the variable nhvsys is not defined yet.  
    ! after nhvsys is defined the following statement can be used to define nequals
    ! nequals = nofhvpr + nhvsys*nlspct
    nequals = nofhvpr

    return
    end subroutine offset

! --------------------------- room_connections -------------------------------------------

    subroutine room_connections (tsec)

    ! routine: room_connections
    ! purpose: this routine determines whether flow from each room can reach the outside (perhaps through intermediate rooms) 
    !           via horizontal or vertical vents.  if a room is isolated from the outside then snsqe has trouble finding an 
    !           initial pressure solution.
    ! arguments: tsec: current simulation time 

    use precision_parameters
    use cenviro
    use cfast_main
    use vents
    implicit none

    real(eb), intent(in) :: tsec
    
    real(eb) :: factor2, qchfraction, height, width, avent
    integer roomc(nr,nr), tempmat(nr,nr), i, j, iroom1, iroom2, ik, im, ix, matiter
    integer, parameter :: toprm = 1, botrm = 2
    
    type(vent_type), pointer :: ventptr

    ! initially assume that no rooms are connected
    do i = 1, n
        do j = 1, n
            roomc(i,j) = 0
        end do
        roomc(i,i) = 1
    end do

    ! check horizontal vent flow
    do i = 1, n_hvents
        ventptr=>hventinfo(i)
        
        iroom1 = ventptr%from
        iroom2 = ventptr%to
        ik = ventptr%counter
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        factor2 = qchfraction(qcvh,ijk(im,ix,ik),tsec)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = factor2*height*width
        if(avent/=0.0_eb)then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        endif
    end do

    ! check vertical vent flow
    do i = 1, n_vvents
        iroom1 = ivvent(i,toprm)
        iroom2 = ivvent(i,botrm)
        if(vvarea(iroom1,iroom2)/=0.0_eb)then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        endif
    end do

    ! construct roomc**matiter where matiter > n
    ! note:  roomc is a transitiion matrix (from markov chain theory). that is, roomc(i,j) is zero if there no connection 
    !        between room and room j.  similarly, roomc(i,j) is one if there is a connection between these two rooms.  
    !        roomc is symmetric. the matrix roomc**2 is tells us whether flow can get from room i to room j in two steps.  
    !        since there are only n rooms, roomc**n tells us whether any given room is connected to any other room in n steps.  
    !        the entries roomc**n(i,n) then indicates whether a room is connected to the outside (perhaps through several other 
    !        intermediate rooms).
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
    end subroutine room_connections

! --------------------------- wset -------------------------------------------

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

    use precision_parameters
    implicit none

    integer, intent(in) :: nslab
    integer, intent(inout) :: numnode(*) 
    real(eb), intent(in) :: tstop, wsplit(*), wk(*), wspec(*), wrho(*), wthick(*), tamb, text
    real(eb), intent(out) :: wlen, walldx(*)
    
    integer :: cumpts(10), numpts(10), i, ii, nx, nintx, nsplit, islab, isum, nint, ibeg, iend
    real(eb) :: wtemp(*), xwall(100), xpos(10), xxnx, errfc05, xkrhoc, alpha, xb, xxnsplit, w, xxim1, xxiim1
    real(eb) :: wmxb, xxnslabm2, xxnint, xxi1, xxi2, xxi3, xxnintx, dtdw

    nx = numnode(1)
    xxnx = nx

    nintx = nx - (nslab+1)
    if (nslab<=2) then
        nsplit = (wsplit(1)+wsplit(2))*xxnx
    else
        nsplit = wsplit(1)*xxnx
    endif

    ! calculate total walldepth
    xpos(1) = 0.0_eb
    do islab = 1, nslab
        xpos(islab+1) = xpos(islab) + wthick(islab)
    end do
    wlen = xpos(nslab+1)

    ! calculate break point based on first slab's properties
    errfc05 = 1.30_eb
    xkrhoc = wk(1)/(wspec(1)*wrho(1))
    alpha = sqrt(xkrhoc)
    xb = 2.0_eb*alpha*sqrt(tstop)*errfc05*wlen
    if (xb>.50_eb*wlen) xb = 0.5_eb*wlen
    if (nslab==1) then

        ! set up wall node locations for 1 slab case, bunch points at interior and exterior boundary
        xxnsplit = nsplit
        w = 1.0_eb/xxnsplit 
        do i = 1, nsplit + 1 
            xxim1 = i - 1
            xwall(i) = xb*(xxim1*w)**2
        end do
        w = 1.0_eb/(xxnx-(xxnsplit+1.0_eb))
        do i = nsplit +2, nx
            ii = nx + 1 - i 
            xxiim1 = ii - 1
            xwall(i) = wlen - (wlen-xb)*(xxiim1*w)**2
        end do
        numnode(1+nslab) = nintx
    else

        ! set up wall node locations for multi-slab case, bunch points at interior boundary of first slab, exterior 
        ! boundary of last slab and uniformly in middle slabs

        ! calculate number of points interior to each slab
        xxnintx = nintx
        numpts(1) = wsplit(1)*xxnintx*min(xb,wthick(1))/wlen
        if (numpts(1)<1) numpts(1) = 1
        wmxb = wlen - xb
        numpts(nslab) = wsplit(3)*xxnintx*min(wmxb,wthick(nslab))/ wlen
        if (numpts(nslab)<1) numpts(nslab) = 1
        isum = nintx - numpts(1) - numpts(nslab)
        xxnslabm2 = nslab - 2
        do i = 2, nslab - 1
            numpts(i) = xxnx*wsplit(2)*wthick(nslab)/xxnslabm2/wlen
            if (numpts(i)<1) numpts(i) = 1
            isum = isum - numpts(i)
        end do
        numpts(1) = numpts(1) + (isum-isum/2)
        numpts(nslab) = numpts(nslab) + isum/2
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
            xwall(i) = xxim1**2*xpos(2)/xxnint**2
        end do

        ! calculate wall positions for middle slabs (uniform)
        do islab = 2, nslab - 1
            ibeg = cumpts(islab)
            iend = cumpts(islab+1) - 1
            xxi3 = iend+1-ibeg
            do i = ibeg, iend
                xxi1 = iend+1-i
                xxi2 = i-ibeg
                xwall(i) = (xpos(islab)*xxi1+xpos(islab+1)*xxi2)/xxi3
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
                xwall(i) = xpos(nslab+1) - xxi1**2*(xpos(nslab+1) - xpos(nslab))/xxi3**2
            end do
        endif
    endif

    ! finally calculate distances between each point these distances are used by conductive_flux to setup 
    ! discretization tri-diagonal matrix
    do i = 1, nx - 1
        walldx(i) = xwall(i+1) - xwall(i)
    end do

    ! initialize temperature profile.  note, wtemp(1)=wtemp(2) and wtemp(nx)=wtemp(nx-1) so dassl will think that no heat 
    ! transfer needs to occur to the wall (since dt/dx=0 here)
    wtemp(1) = tamb
    wtemp(nx) = text
    dtdw = (text-tamb)/(xwall(nx-1)-xwall(2))
    do i = 2, nx-1
        wtemp(i) = tamb + (xwall(i)-xwall(2))*dtdw
    end do
    return
    end
