module initialization_routines

    use precision_parameters

    use numerics_routines, only: dnrm2, dscal
    use opening_fractions, only : qchfraction
    use output_routines, only : deleteoutputfiles
    use solve_routines, only : update_data
    use utility_routines, only: indexi, xerror

    use cenviro
    use cfast_main
    use cparams
    use cshell
    use solver_data
    use fire_data
    use params
    use opt
    use target_data
    use thermp
    use vent_data
    use wallptrs
    use wnodes

    implicit none

    private

    public get_thermal_property, inittarg, initamb, offset, hvinit, initialize_memory, initialize_fire_objects, &
        initialize_species, initialize_walls

    contains


! --------------------------- get_thermal_property -------------------------------------------

    subroutine get_thermal_property (name, tp)

    !     Routine: get_thermal_Property
    !     Purpose: check for and return index to a thermal property
    !     Revision: $Revision$
    !     Revision Date: $Date$

    implicit none
    character, intent(in) :: name*(*)

    character(mxthrmplen) missingtpp
    integer tp, i

    do i = 1, maxct
        tp = i
        if (name==nlist(i)) return
    end do
    missingtpp = name
    write(3,'(''***Error: A thermal property was not found in the input file. Missing material: '',a)') missingtpp
    stop

    end subroutine get_thermal_property

! --------------------------- hvinit -------------------------------------------

    subroutine hvinit ()

    !     routine: hvinit
    !     purpose: this routine sets up the arrays needed to for hvac
    !                 simulation and initializes temperatures and concentrations
    !     revision: $revision: 352 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $

    !     this function has been modified to prevent negative flow.  a max function
    !     was inserted just after the calculation off, the flow to do this.  if
    !     the flow is allowed to be negative (flow reversal) then this statement
    !     must be removed.

    real(eb) :: c3(ns), f, xxjm1, s1, s2, xnext, pav, tav, df, xx, rden
    integer :: i, ii, j, k, ib, id, isys, lsp
    type(room_type), pointer :: roomptr

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
            end if
        end do
        ncnode(i) = k
    end do

    ! check interior nodes
    do i = 1, nnode
        if (ncnode(i)<1.or.ncnode(i)>mxcon) then
            write (logerr,*) '***Error: HVINIT - interior node has too many or too few connections'
            stop
        end if
    end do

    ! limit the range of hvelxt and set the absolute height of the interior node
    do ii = 1, next
        i = hvnode(1,ii)
        j = hvnode(2,ii)
        if (ncnode(j)>1) then
            write (logerr,*) '***Error: HVINIT - exterior node has too many or too few connections'
            stop
        end if
        roomptr => roominfo(i)
        hvelxt(ii) = min(roomptr%height,max(0.0_eb,hvelxt(ii)))
        hvght(j) = hvelxt(ii) + roomptr%z0
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
        end if
        tbr(ib) = hvextt(ii,upper)
        s1 = s1 + hvp(j)
        s2 = s2 + tbr(ib)
        do lsp = 1, ns
            ! the outside is defined to be at the base of the structure for mv
            if (i<n) then
                hvexcn(ii,lsp,upper) = initial_mass_fraction(lsp)*interior_density
                hvexcn(ii,lsp,lower) = initial_mass_fraction(lsp)*interior_density
            else
                hvexcn(ii,lsp,upper) = initial_mass_fraction(lsp)*exterior_density
                hvexcn(ii,lsp,lower) = initial_mass_fraction(lsp)*exterior_density
            end if
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
        end if
    end do
    do i = 1, nbr
        if (tbr(i)<=0.0_eb) tbr(i) = tav
        if (hvconc(i,1)<0.0_eb) then
            do lsp = 1, ns
                hvconc(i,lsp) = c3(lsp)
            end do
        end if
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
        rden = (pressure_offset+pav)/(rgas*tbr(ib))
        hvtm(isys) = hvtm(isys) + rden*hvdvol(ib)
    end do

    ! now that everything is ok, we can turn on ventilation
    mvcalc_on = .true.
    return
    end subroutine hvinit

! --------------------------- hvmap -------------------------------------------

    subroutine hvmap

    !     routine: hvmap
    !     purpose: this routine maps all the hvac nodes into a single mapping array for dassl and creates
    !              a mapping from those to exterior ones
    !     revision: $revision: 352 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments:


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
90  continue
    icurnod = 0
    do i = 1, nnode
        if (izhvsys(i)==0) then
            icurnod = i
            exit
        end if
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
            end if
        end do
        go to 120
    end if
    nhvsys = icursys

    ! we have to update nequals.  nequals was originally defined in
    ! offset but offset was called before nhvsys was defined.
    nequals = nofhvpr + nhvsys*n_species

    do i = 1, nnode
        isys = izhvsys(i)
        do j = 1, ncnode(i)
            ib = icmv(i,j)
            izhvbsys(ib) = isys
        end do
    end do
    return
    end subroutine hvmap

! --------------------------- initamb -------------------------------------------

    subroutine initamb (yinter,iflag)

    !     purpose: this routine computes initializations for varialbes
    !     related to ambient conditions.  when iflag=1 the array
    !     yinter is used to compute upper layer volumes.  otherwise,
    !     upper layer volumes are not computed.  if iflag is set to 1
    !     then yinter must be a floating point array of at least size nr
    !     (nr = number of rooms) in the calling routine.

    integer, intent(in) :: iflag
    real(eb), intent(out) :: yinter(*)

    real(eb) :: dummy(1) = (/0.0_eb/), xxpmin, tdspray, tdrate, scale
    integer i, ii, iwall, iroom, itarg

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    ! simplify and make initial pressure calculations consistent.  inside pressures
    ! were calculated using rho*g*h .  but outside pressures were calculated using
    ! atmosp.  fictional flows resulted making  snsqe work a log harder to get
    ! an initial solution.  the initial temperature values calculated by atmosp
    ! at the top of the empire state building (about 400 m above base) is only
    ! about 0.2 k different that at the base.
    do i = 1, nm1
        roomptr => roominfo(i)
        interior_rel_pressure(i) = -interior_density*grav_con*roomptr%z0
        exterior_rel_pressure(i) = -exterior_density*grav_con*roomptr%z0
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
    pressure_offset = pressure_offset + xxpmin
    interior_abs_pressure = interior_abs_pressure + xxpmin - pressure_offset
    exterior_abs_pressure = exterior_abs_pressure + xxpmin - pressure_offset

    ! copy all of the variables from the initial values into the data arrays
    call update_data (dummy,constvar)

    ! define the p array, the solution to the ode
    do i = 1, nm1
        roomptr => roominfo(i)
        p(i) = interior_rel_pressure(i)
        p(i+noftu) = interior_temperature

        ! check for a special setting of the interface height
        if (iflag==1) then
            if (yinter(i)<0.0_eb) then
                p(i+nofvu) = roomptr%vmin
            else
                p(i+nofvu) = min(roomptr%vmax,max(roomptr%vmin,yinter(i)*roomptr%area))
            end if
            yinter(i) = 0.0_eb
        end if
        if(roomptr%shaft) p(i+nofvu) = roomptr%vmax
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
    ii = nofwt
    do i = 1, nm1
        roomptr => roominfo(i)
        do iwall = 1, nwal
            if (roomptr%surface_on(iwall)) then
                ii = ii + 1
                p(ii) = interior_temperature
            end if
        end do
    end do

    ! establish default values for detector data
    do i = 1, ndtect
        dtectptr => detectorinfo(i)
        iroom=dtectptr%room
        roomptr => roominfo(iroom)
        if (dtectptr%center(1)<0.0_eb) dtectptr%center(1) = roomptr%width*0.5_eb
        if (dtectptr%center(2)<0.0_eb) dtectptr%center(2) = roomptr%depth*0.5_eb
        if (dtectptr%center(3)<0.0_eb) dtectptr%center(3) = roomptr%height-mx_vsep

        ! if tdspray>0 then interpret it as a spray density and convert
        ! to a characteristic quenching time
        ! if tdspray < 0 then interpret abs(tdspray) as the time
        ! required to reduce the fire size by 50 per cent
        ! if tdspray = 0 then turn the sprinkler off
        tdspray = dtectptr%spray_density
        if(tdspray>0.0_eb)then
            tdrate = 3.0_eb/tdspray**1.8_eb
        elseif(tdspray<0.0_eb)then
            tdrate = abs(tdspray)/log(2.0_eb)
            tdspray = (3.0_eb/tdrate)**(1.0_eb/1.8_eb)
        else
            tdspray = 0.0_eb
            tdrate = 1.0e10_eb
            dtectptr%quench = .false.
        end if
        dtectptr%spray_density = tdspray
        dtectptr%half_life = tdrate*log(2.0_eb)
        dtectptr%tau = tdrate

        ! set initial ceiling jet and detector link temperatures to ambient
        dtectptr%value = interior_temperature
        dtectptr%value_o = interior_temperature
        dtectptr%temp_gas = interior_temperature
        dtectptr%temp_gas_o = interior_temperature
    end do

    ! p's for pressure, volume and temperature are defined
    ! we can now copy these values to the environment variables
    call update_data (p, odevara)

    ! initialize target temperatures
    do itarg = 1, ntarg
        targptr => targetinfo(itarg)
        iroom = targptr%room
        targptr%temperature(idx_tempf_trg:idx_tempb_trg) = interior_temperature
        targptr%tgas = interior_temperature

        ! scale normal vectors to have length 1
        scale = 1.0_eb/dnrm2(3,targptr%normal,1)
        call dscal(3,scale,targptr%normal,1)
    end do

    ! initialize solver oxygen values if required.   (must be initialized
    ! after zzmass is defined)
    if(option(foxygen)==on)then
        do iroom = 1, nm1
            p(iroom+nofoxyu)=0.23_eb*zzmass(iroom,upper)
            p(iroom+nofoxyl)=0.23_eb*zzmass(iroom,lower)
        end do
    end if

    return
    end subroutine initamb

! --------------------------- sortbrm -------------------------------------------

    subroutine sortbrm (x,lx,ix,lix,nrow,ncolx,ncolix,isort,ldp,nroom,ipoint)

    !     routine: sortbrm
    !     purpose:  sort the two arrays x and ix by the isort'th column of ix which contains room data.  this routine is used to
    !               sort fire and detector data structures by room number.
    !     arguments: x       floating point info to be sorted
    !                lx      leading dimension of x
    !                ix      integer info to be sorted
    !                lix     leading dimension of ix in calling routine
    !                nrow    number of rows in x and ix
    !                ncolx   number of columns in x
    !                ncolix  number of columns in ix
    !                isort   column in ix to sort on (usually contains room numbers)
    !                ldp     leading dimension of ipoint
    !                nroom   number of elements for which ipoint is defined, also the number of rooms
    !                ipoint (output)  pointer array for sorted x and ix list.
    !                                 (r,1) = number of items (fires or detectors so far) in room r
    !                                 (r,2) = pointer to beginning element in ix and x for fire or detector in room r

    ! if the number of fires, detectors or rooms ever exceeds 100 then the following dimension statement needs to be changed
    integer, parameter :: lwork = nr + mxfires + mxdtect


    integer, intent(in) :: lix, ncolix, ncolx
    integer, intent(in) :: nrow, isort, nroom, lx, ldp
    integer, intent(inout) :: ix(lix,ncolix)
    integer, intent(out) :: ipoint(ldp,*)
    real(eb), intent(inout) :: x(lx,ncolx)

    integer :: i, j, iroom, iwork(lwork), iperm(lwork)
    real(eb) :: work(lwork)

    if(nrow>lwork)then
        call xerror('Error: Internal error sorting detectors. Not enough work space in sortbrm',0,1,2)
    end if

    ! create a permutation vector using the isort'th column of ix
    iperm(1:nrow) = (/(i,i=1,nrow)/)
    call indexi(nrow,ix(1,isort),iperm)

    ! reorder integer array using the permutation vector
    do j = 1, ncolix
        iwork(1:nrow) = ix(iperm(1:nrow),j)
        ix(1:nrow,j) = iwork(1:nrow)
    end do

    ! reorder the floating point arrays using the permutation vector
    do j = 1, ncolx
        work(1:nrow) = x(iperm(1:nrow),j)
        x(1:nrow,j) = work(1:nrow)
    end do

    ! construct the pointer array
    ipoint(1:nroom,1:2) = 0
    do i = 1, nrow
        iroom = ix(i,isort)
        ipoint(iroom,1) = ipoint(iroom,1) + 1
        if (ipoint(iroom,2)==0) ipoint(iroom,2) = i
    end do
    do i = 1, nroom
        if (ipoint(i,2)==0) ipoint(i,2) = 1
    end do
    return

    end subroutine sortbrm

! --------------------------- initialize_memory -------------------------------------------

    subroutine initialize_memory

    !     routine: initialize_memory
    !     purpose: This routine initializes the main memory
    !     Arguments: none

    integer i
    type(room_type), pointer :: roomptr

    ! simple control stuff
    exset = .false.
    debugging = .false.
    jaccol = -2
    neqoff = 10

    ! DASSL forcing functions
    p(1:maxteq) = 0.0_eb

    ! set the time step and inner step division for time splitting
    ! we do not let the user choose these
    deltat = 1.0_eb

    ! time step checking
    zzdtcrit = 1.0e-09_eb
    izdtnum = 0
    izdtmax = 100
    izdtflag = .true.

    ! define universal constants
    cp = 1012.0_eb
    gamma = 1.40_eb
    rgas = (gamma-1.0_eb)/gamma*cp
    stime = 0.0_eb
    t_ref = 293.15_eb
    lower_o2_limit = 0.15_eb
    pressure_ref = 101325.0_eb
    interior_abs_pressure = pressure_ref
    pressure_offset = pressure_ref
    interior_temperature = t_ref
    tgignt = t_ref + 200.0_eb
    exterior_temperature = interior_temperature
    exterior_abs_pressure = interior_abs_pressure
    relative_humidity = 0.5_eb

    ! species
    allowed(1:ns) = .false.
    activs(1:ns) = .true.
    initial_mass_fraction(1:ns) = 0.0_eb
    ! normal air
    initial_mass_fraction(1) = 0.77_eb
    initial_mass_fraction(2) = 0.23_eb
    zzgspec(1:nr,upper:lower,1:ns) = 0.0_eb
    zzcspec(1:nr,upper:lower,1:ns) = 0.0_eb

    ! rooms
    roominfo(1:nr)%width = xlrg
    roominfo(1:nr)%depth = xlrg
    roominfo(1:nr)%height = xlrg
    roominfo(1:nr)%x0 = 0.0_eb
    roominfo(1:nr)%y0 = 0.0_eb
    roominfo(1:nr)%z0 = 0.0_eb
    roominfo(1:nr)%x1 = xlrg
    roominfo(1:nr)%y1 = xlrg
    roominfo(1:nr)%z1 = xlrg
    roominfo(1:nr)%ibar = 50
    roominfo(1:nr)%jbar = 50
    roominfo(1:nr)%kbar = 50
    do i = 1, nr
        roomptr => roominfo(i)
        roomptr%area = roomptr%width*roomptr%depth
        roomptr%volume = roomptr%height*roomptr%area
        roomptr%matl(1:nwal) = 'OFF'
        roomptr%surface_on(1:nwal) = .false.
    end do
    epw(1:nwal,1:nr) = 0.0_eb
    adiabatic_wall = .false.
    roominfo(1:nr)%deadroom = 0
    roominfo(1:nr)%hall = .false.
    roominfo(1:nr)%shaft = .false.
    n_species = 0
    numthrm = 0
    n = 0
    ! room to room heat transfer
    nswal = 0

    ! variable cross sectional area
    izrvol(1:nr) = 0
    zzrvol(1:mxcross,1:nr) = 0.0_eb
    zzrarea(1:mxcross,1:nr) = 0.0_eb
    zzrhgt(1:mxcross,1:nr) = 0.0_eb

    ! initialize inter-compartment heat transfer fractions
    zzhtfrac(1:nr,1:nr) = 0.0_eb
    izheat(1:nr) = 0
    izhtfrac(1:nr,1:nr) = 0

    ! initialize number of furnace temperature nodes
    nfurn=0

    ! flow variables
    heatup(1:nr) = 0.0_eb
    heatlp(1:nr) = 0.0_eb
    qfc(upper:lower,1:nr) = 0.0_eb

    ! horizontal vents
    ihvent_connections(1:nr,1:nr) = 0.0_eb
    bw(1:mxhvents) = 0.0_eb
    hh(1:mxhvents) = 0.0_eb
    hl(1:mxhvents) = 0.0_eb
    hhp(1:mxhvents) = 0.0_eb
    hlp(1:mxhvents) = 0.0_eb
    vface(1:mxhvents) = 1
    ! start with vents open
    qcvh(1,1:mxhvents) = 0.0_eb
    qcvh(2,1:mxhvents) = 1.0_eb
    qcvh(3,1:mxhvents) = 0.0_eb
    qcvh(4,1:mxhvents) = 1.0_eb
    ijk(1:nr,1:nr,1:mxccv) = 0
    nventijk = 0

    ! vertical vents
    vshape(1:nr,1:nr) = 0
    ivvent_connections(1:nr,1:nr) = 0
    vvarea(1:nr,1:nr) = 0.0_eb
    ! start with vents open
    qcvv(1,1:nr) = 0.0_eb
    qcvv(2,1:nr) = 1.0_eb
    qcvv(3,1:nr) = 0.0_eb
    qcvv(4,1:nr) = 1.0_eb

    ! mechanical vents
    nnode = 0
    nfan = 0
    nfilter = 0
    nbr = 0
    next = 0
    mvcalc_on = .false.
    hvght(1:mxnode) = 0.0_eb
    hveflot(upper:lower,1:mxext) = 0.0_eb
    tracet(upper:lower,1:mxext) = 0.0_eb
    ! note that the fan fraction is unity = on, whereas the filter fraction is unity = 100% filtering since
    ! there is not "thing" associated with a filter, there is no (as of 11/21/2006)
    ! way to have an intial value other than 0 (no filtering).
    qcvf(1,1:mxfan) = 0.0_eb
    qcvf(2,1:mxfan) = 0.0_eb
    qcvf(3,1:mxfan) = 0.0_eb
    qcvf(4,1:mxfan) = 0.0_eb
    qcvm(1,1:mxfan) = 0.0_eb
    qcvm(2,1:mxfan) = 1.0_eb
    qcvm(3,1:mxfan) = 0.0_eb
    qcvm(4,1:mxfan) = 1.0_eb

    ! set to -1 as a flag for nputp initialization - any value not set will be set to the
    ! default which is the center of the respective wall
    fpos(1:3) = -1.0_eb
    fqdj(1:nr) = 0.0_eb

    ! detectors
    ndtect = 0
    detectorinfo(1:mxdtect)%rti = 50.0_eb
    detectorinfo(1:mxdtect)%spray_density = -300.0_eb
    detectorinfo(1:mxdtect)%center(1) = -1.0_eb
    detectorinfo(1:mxdtect)%center(2) = -1.0_eb
    detectorinfo(1:mxdtect)%center(3) = -3.0_eb/39.37_eb
    detectorinfo(1:mxdtect)%trigger = 330.3722_eb
    detectorinfo(1:mxdtect)%velocity = 0.0_eb
    detectorinfo(1:mxdtect)%velocity_o = 0.0_eb
    detectorinfo(1:mxdtect)%activation_time = 99999.0_eb
    detectorinfo(1:mxdtect)%dtype = 2
    detectorinfo(1:mxdtect)%room = 1
    detectorinfo(1:mxdtect)%quench = .false.
    detectorinfo(1:mxdtect)%activated = .false.
    detectorinfo(1:mxdtect)%reported = .false.

    iquench(1:nr) = 0

    ! targets
    ntarg = 0
    targetinfo(1:mxtarg)%equaton_type = pde
    targetinfo(1:mxtarg)%back = interior
    targetinfo(1:mxtarg)%material = 'DEFAULT'

    return
    end subroutine initialize_memory

! --------------------------- initialize_fire_objects -------------------------------------------

    subroutine initialize_fire_objects

    !     routine: initialize_fire_objects
    !     purpose: this routine initializes the fire objects
    !     arguments: none

    ! a specified fire in the center of the room
    heatfl = .false.
    heatfp(1:3) = -1.0_eb

    ! turn off objects
    numobjl = 0
    objon(0:mxfires) = .false.
    objpos(1:3,0:mxfires) = -1.0
    objrm(0:mxfires) = 0
    objnin(0:mxfires) = ' '
    objld(0:mxfires) = .false.
    objpnt(0:mxfires) = 0
    objcri(1:3,0:mxfires) = 0.0
    objdef(0:mxfires) = .false.
    odbnam(0:mxfires) = ' '

    ! trace species stuff
    objmaspy(0:mxfire) = 0.0_eb
    radio(0:mxfire) = 0.0_eb
    radconsplit(0:mxfire) = 0.35_eb
    tradio = 0.0_eb

    return
    end subroutine initialize_fire_objects

! --------------------------- initspecc -------------------------------------------

    subroutine initialize_species

    !     routine: initialize_species
    !     purpose: This routine initializes variables associated with
    !              species it originally occured in CFAST and INITFS.  It was moved
    !              to one subroutine to make maintenance easier
    !     Arguments: none

    real(eb) :: xt, xtemp, xh2o, totmass, initialmass(2,nr,ns)
    integer i, j, k, ip, iprod, isof, isys, lsp


    do i = 1, nm1

        !  set the water content to relative_humidity - the polynomial fit is to (t-273), and
        ! is for saturation pressure of water.  this fit comes from the steam
        ! tables in the handbook of physics and chemistry.  we are being clever
        ! here.  the final result in initial_mass_fraction should be the value used in stport for
        ! the outside ambient.
        xt = interior_temperature
        xtemp = 23.2_eb - 3.816e3_eb/(xt-46.0_eb)
        xh2o = exp(xtemp)/101325.0_eb*(18.0_eb/28.4_eb)
        initial_mass_fraction(8) = relative_humidity*xh2o

        ! normalize the atmosphere
        totmass = 0.0_eb
        do j = 1, ns
            totmass = totmass + initial_mass_fraction(j)
        end do
        initial_mass_fraction(1:ns) = initial_mass_fraction(1:ns)/totmass

        do k = upper, lower
            do lsp = 1, ns
                toxict(i,k,lsp) = 0.0_eb
                initialmass(k,i,lsp) = initial_mass_fraction(lsp)*interior_density*zzvol(i,k)
            end do
        end do
    end do

    isof = nofprd
    do lsp = 1, ns
        if (activs(lsp)) then
            do i = 1, nm1
                do k = upper, lower
                    isof = isof + 1
                    p(isof) = initialmass(k,i,lsp)
                end do
            end do
        end if
    end do

    ! hvinit define initial products for hvac systems (if any)
    if(nhvsys/=0)then
        isof = nofhvpr
        do lsp = 1, min(ns,9)
            if(activs(lsp))then
                do isys = 1, nhvsys
                    isof = isof + 1
                    p(isof) = initial_mass_fraction(lsp)*hvtm(isys)
                end do
            end if
        end do
    end if

    ! define product map array
    izpmap(1) = 1
    izpmap(2) = 2
    ip = 2
    do iprod = 1, ns
        if (activs(iprod)) then
            ip = ip + 1
            izpmap(ip) = iprod + 2
        end if
    end do

    return
    end subroutine initialize_species

! --------------------------- inittarg -------------------------------------------

    subroutine inittarg ()

    !     routine: inittarg
    !     purpose: Initialize target data structures

    real(eb) :: xloc, yloc, zloc, xxnorm, yynorm, zznorm, xsize, ysize, zsize, xx, yy, zz
    integer :: itarg, iroom, iwall, iwall2
    integer :: map6(6) = (/1,3,3,3,3,2/)

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr

    do itarg = 1, ntarg

        ! room number must be between 1 and nm1
        targptr => targetinfo(itarg)
        iroom = targptr%room
        if(iroom<1.or.iroom>nm1)then
            write(logerr,'(a,i0)') '***Error: Target assigned to non-existent compartment',iroom
            stop
        end if
        roomptr => roominfo(iroom)
        iwall = targptr%wall
        xloc = targptr%center(1)
        yloc = targptr%center(2)
        zloc = targptr%center(3)
        xxnorm = targptr%normal(1)
        yynorm = targptr%normal(2)
        zznorm = targptr%normal(3)
        xsize = roomptr%width
        ysize = roomptr%depth
        zsize = roomptr%height

        ! if the locator is -1, set to center of room on the floor
        if(xloc==-1.0_eb) xloc = 0.5_eb*xsize
        if(yloc==-1.0_eb) yloc = 0.5_eb*ysize
        if(zloc==-1.0_eb) zloc = 0.0_eb
        if(iwall/=0)then
            xxnorm = 0.0_eb
            yynorm = 0.0_eb
            zznorm = 0.0_eb
        end if
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
        end if
        if(iwall/=0)then
            targptr%center(1) = xx
            targptr%center(2) = yy
            targptr%center(3) = zz
            targptr%normal(1) = xxnorm
            targptr%normal(2) = yynorm
            targptr%normal(3) = zznorm
            xloc = xx
            yloc = yy
            zloc = zz
            iwall2 = map6(iwall)
            if(roomptr%surface_on(iwall2))then
                targptr%material = roomptr%matl(iwall2)
            else
                targptr%material = ' '
            end if
        end if

        ! center coordinates need to be within room
        if(xloc<0.0_eb.or.xloc>xsize.or.yloc<0.0_eb.or.yloc>ysize.or.zloc<0.0_eb.or.zloc>zsize)then
            write(logerr,'(a,i0,1x,3f10.3)') '***Error: Target located outside of compartment', iroom, xloc, yloc, zloc
            stop
        end if
    end do

    return
    end subroutine inittarg

! --------------------------- initialize_walls  -------------------------------------------

    subroutine initialize_walls (tstop)

    !     purpose: This routine initializes data structures associated
    !             with walls and targets
    !     Arguments: TSTOP

    !        fkw = thermal conductivity
    !        cw = specific heat (j/kg)
    !        rw = density of the wall (kg/m**3)
    !        flw = thickness of the wall (m)
    !        epw = emmisivity of the wall
    !        nslb = discretization of the wall slabs (number of nodes)
    !        matl contains the name of the thermal data subset in the tpp datafile
    !        maxct is a count of the number of tpp data sets in the database

    real(eb), intent(in) :: tstop
    integer :: i, j, jj, k, itarg, ifromr, itor, ifromw, itow, nslabf, nslabt, nptsf, nptst, wfrom, wto
    character(mxthrmplen) off, none, tcname

    ! tp is the pointer into the data base for each material
    integer tp

    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr

    data off /'OFF'/, none /'NONE'/

    ! map the thermal data into its appropriate wall specification
    ! if name is "OFF" or "NONE" then just turn all off
    do i = 1, nwal
        do j = 1, nm1
            roomptr => roominfo(j)
            if (roomptr%surface_on(i)) then
                if (roomptr%matl(i)==off.or.roomptr%matl(i)==none) then
                    roomptr%surface_on(i) = .false.
                else
                    call get_thermal_property(roomptr%matl(i),tp)
                    nslb(i,j) = lnslb(tp)
                    do k = 1, nslb(i,j)
                        fkw(k,i,j) = lfkw(k,tp)
                        cw(k,i,j) = lcw(k,tp)
                        rw(k,i,j) = lrw(k,tp)
                        flw(k,i,j) = lflw(k,tp)
                    end do
                    epw(i,j) = lepw(tp)
                end if
            end if
        end do
    end do

    ! Initialize the interior temperatures to the interior ambient
    twj(1:nnodes,1:nm1,1:nwal) = interior_temperature

    ! initialize temperature profile data structures
    do i = 1, nm1
        roomptr => roominfo(i)
        do j = 1, nwal
            if (roomptr%surface_on(j)) then
                call wset(numnode(1,j,i),nslb(j,i),tstop,walldx(1,i,j),wsplit,fkw(1,j,i),cw(1,j,i),rw(1,j,i),flw(1,j,i),&
                   wlength(i,j),twj(1,i,j),interior_temperature,exterior_temperature)
            end if
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
        targptr => targetinfo(itarg)
        tcname = targptr%material
        if (tcname==' ') then
            tcname = 'DEFAULT'
            targptr%material = tcname
        end if
        call get_thermal_property(tcname,tp)
        targptr%k = lfkw(1,tp)
        targptr%cp = lcw(1,tp)
        targptr%rho = lrw(1,tp)
        targptr%thickness = lflw(1,tp)
        targptr%depth_loc = max(0.0_eb,min(targptr%thickness*targptr%depth_loc,targptr%thickness))
        targptr%emissivity = lepw(tp)
    end do

    return
    end subroutine initialize_walls

! --------------------------- offset -------------------------------------------

    subroutine offset ()

    ! purpose: offset in the following context is the beginning of the vector for that particular variable minus one.
    !          thus, the actual pressure array goes from nofp+1 to nofp+nm1.  the total number of equations to be considered
    !          is nequals, and is the last element in the last vector. each physical interface routine is responsible for
    !          the count of the number of elements in the vector for which it is resonsible.

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
    ! nofwt = wall surface temperatures (equivalent to the number of profiles)
    ! nofprd = species
    ! nequals = last element in the array.

    ! the arrays which use this structure are vatol, vrtol, p, pdold, pprime and pdzero

    ! an important note - solve_simulation sets the last variable to be solved to nofprd which is the
    ! beginning of the species (-1) and the end of the array which is presently used by dassl

    integer :: i, j, ib, noxygen
    type(room_type), pointer :: roomptr

    ! count the of nodes (largest of ns and ne)
    nnode = max(na(1),ne(1))
    do ib = 2, nbr
        nnode = max(nnode,na(ib),ne(ib))
    end do
    if (nnode>mxnode) then
        write (logerr,*) '***Error: offset - Too many nodes in hvac specification'
        stop
    end if

    ! set the number of compartments and offsets
    nm1 = n - 1

    ! count the species
    n_species = 0

    do i = 1, ns
        if (allowed(i)) then
            if (activs(i)) then
                n_species = n_species + 1
            end if
        else if (i/=7) then
            n_species = n_species + 1
        end if
    end do
    n_species = n_species + 1

    ! count the number of walls
    nwalls = 0
    do i = 1, nm1
        roomptr => roominfo(i)
        do j = 1, nwal
            if (roomptr%surface_on(j)) then
                nwalls = nwalls + 1
            end if
            if (nwpts/=0) numnode(1,j,i) = nwpts
        end do
    end do

    ! set number of implicit oxygen variables
    if(option(foxygen)==on)then
        noxygen = nm1
    else
        noxygen = 0
    end if

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
    nofwt = nofoxyu + noxygen
    nofprd = nofwt + nwalls
    nofhvpr = nofprd + 2*nm1*n_species

    ! if the hvac model is used then nequals needs to be redefined in hvmap since the variable nhvsys is not defined yet.
    ! after nhvsys is defined the following statement can be used to define nequals
    ! nequals = nofhvpr + nhvsys*n_species
    nequals = nofhvpr

    return
    end subroutine offset

! --------------------------- wset -------------------------------------------

    subroutine wset (numnode,nslab,tstop,walldx,wsplit,wk,wspec,wrho,wthick,wlen,wtemp,tamb,text)

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
    end if

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
        end if

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
        end if
    end if

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
    end subroutine wset

end module initialization_routines
