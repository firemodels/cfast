module initialization_routines

    use precision_parameters

    use numerics_routines, only: dnrm2, dscal
    use opening_fractions, only : qchfraction
    use output_routines, only : deleteoutputfiles
    use solve_routines, only : update_data
    use utility_routines, only: indexi, xerror

    use cenviro
    use ramp_data
    use cparams
    use setup_data
    use solver_data
    use fire_data
    use cparams
    use option_data
    use target_data
    use thermal_data
    use vent_data
    use wallptrs
    use room_data

    implicit none

    private

    public get_thermal_property, inittarg, initamb, offset, hvinit, initialize_memory, initialize_fire_objects, &
        initialize_species, initialize_walls

    contains


! --------------------------- get_thermal_property -------------------------------------------

    subroutine get_thermal_property (name, tp)

    !     Routine: get_thermal_Property
    !     Purpose: check for and return index to a thermal property
    
    implicit none
    character, intent(in) :: name*(*)

    character(mxthrmplen) missingtpp
    integer tp, i
    type(thermal_type), pointer :: thrmpptr

    do i = 1, nthrmp
        thrmpptr => thermalinfo(i)
        if (name==thrmpptr%name) then
            tp = i
            return
        end if
    end do
    missingtpp = name
    write(*,'(''***Error: A thermal property was not found in the input file. Missing material: '',a)') missingtpp
    write(logerr,'(''***Error: A thermal property was not found in the input file. Missing material: '',a)') missingtpp
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
            write (*,*) '***Error: HVINIT - interior node has too many or too few connections'
            write (logerr,*) '***Error: HVINIT - interior node has too many or too few connections'
            stop
        end if
    end do

    ! limit the range of hvelxt and set the absolute height of the interior node
    do ii = 1, next
        i = hvnode(1,ii)
        j = hvnode(2,ii)
        if (ncnode(j)>1) then
            write (*,*) '***Error: HVINIT - exterior node has too many or too few connections'
            write (logerr,*) '***Error: HVINIT - exterior node has too many or too few connections'
            stop
        end if
        roomptr => roominfo(i)
        hvelxt(ii) = min(roomptr%cheight,max(0.0_eb,hvelxt(ii)))
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
        roomptr => roominfo(i)
        j = hvnode(2,ii)
        ib = icmv(j,1)
        ! the outside is defined to be at the base of the structure for mv
        if (i<nr) then
            hvextt(ii,u) = interior_temperature
            hvextt(ii,l) = interior_temperature
            hvp(j) = roomptr%relp - grav_con*interior_rho*hvelxt(ii)
        else
            hvextt(ii,u) = exterior_temperature
            hvextt(ii,l) = exterior_temperature
            hvp(j) = exterior_abs_pressure - grav_con*exterior_rho*hvelxt(ii)
        end if
        tbr(ib) = hvextt(ii,u)
        s1 = s1 + hvp(j)
        s2 = s2 + tbr(ib)
        do lsp = 1, ns
            ! the outside is defined to be at the base of the structure for mv
            if (i<nr) then
                hvexcn(ii,lsp,u) = initial_mass_fraction(lsp)*interior_rho
                hvexcn(ii,lsp,l) = initial_mass_fraction(lsp)*interior_rho
            else
                hvexcn(ii,lsp,u) = initial_mass_fraction(lsp)*exterior_rho
                hvexcn(ii,lsp,l) = initial_mass_fraction(lsp)*exterior_rho
            end if
            hvconc(j,lsp) = hvexcn(ii,lsp,u)
            c3(lsp) = c3(lsp) + hvexcn(ii,lsp,u)
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
    !     then yinter must be a floating point array of at least size mxrooms
    !     (mxrooms = number of rooms) in the calling routine.

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
    do i = 1, nrm1
        roomptr => roominfo(i)
        roomptr%interior_relp_initial = -interior_rho*grav_con*roomptr%z0
        roomptr%exterior_relp_initial = -exterior_rho*grav_con*roomptr%z0
    end do
    roomptr => roominfo(nr)
    roomptr%exterior_relp_initial = 0.0_eb


    ! normalize pressures so that the smallest pressure is zero
    roomptr => roominfo(1)
    xxpmin = min(roomptr%interior_relp_initial,roomptr%exterior_relp_initial)
    do i = 2, nrm1
        roomptr => roominfo(i)
        xxpmin = max(xxpmin,roomptr%interior_relp_initial,roomptr%exterior_relp_initial)
    end do
    do i = 1, nrm1
        roomptr => roominfo(i)
        roomptr%interior_relp_initial = roomptr%interior_relp_initial - xxpmin
        roomptr%exterior_relp_initial = roomptr%exterior_relp_initial - xxpmin
    end do
    pressure_offset = pressure_offset + xxpmin
    interior_abs_pressure = interior_abs_pressure + xxpmin - pressure_offset
    exterior_abs_pressure = exterior_abs_pressure + xxpmin - pressure_offset

    ! copy all of the variables from the initial values into the data arrays
    call update_data (dummy,constvar)

    ! define the p array, the solution to the ode
    do i = 1, nrm1
        roomptr => roominfo(i)
        p(i) = roomptr%interior_relp_initial
        p(i+noftu) = interior_temperature

        ! check for a special setting of the interface height
        if (iflag==1) then
            if (yinter(i)<0.0_eb) then
                p(i+nofvu) = roomptr%vmin
            else
                p(i+nofvu) = min(roomptr%vmax,max(roomptr%vmin,yinter(i)*roomptr%floor_area))
            end if
            yinter(i) = 0.0_eb
        end if
        if (roomptr%shaft) p(i+nofvu) = roomptr%vmax
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
    do i = 1, nrm1
        roomptr => roominfo(i)
        do iwall = 1, nwal
            if (roomptr%surface_on(iwall)) then
                ii = ii + 1
                p(ii) = interior_temperature
            end if
        end do
    end do

    ! establish default values for detector data
    do i = 1, n_detectors
        dtectptr => detectorinfo(i)
        iroom=dtectptr%room
        roomptr => roominfo(iroom)
        if (dtectptr%center(1)<0.0_eb) dtectptr%center(1) = roomptr%cwidth*0.5_eb
        if (dtectptr%center(2)<0.0_eb) dtectptr%center(2) = roomptr%cdepth*0.5_eb
        if (dtectptr%center(3)<0.0_eb) dtectptr%center(3) = roomptr%cheight-mx_vsep

        ! if tdspray>0 then interpret it as a spray density and convert
        ! to a characteristic quenching time
        ! if tdspray < 0 then interpret abs(tdspray) as the time
        ! required to reduce the fire size by 50 per cent
        ! if tdspray = 0 then turn the sprinkler off
        tdspray = dtectptr%spray_density
        if (tdspray>0.0_eb) then
            tdrate = 3.0_eb/tdspray**1.8_eb
        else if (tdspray<0.0_eb) then
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
    do itarg = 1, n_targets
        targptr => targetinfo(itarg)
        iroom = targptr%room
        targptr%temperature(idx_tempf_trg:idx_tempb_trg) = interior_temperature
        targptr%tgas = interior_temperature

        ! scale normal vectors to have length 1
        scale = 1.0_eb/dnrm2(3,targptr%normal,1)
        call dscal(3,scale,targptr%normal,1)
    end do

    ! initialize solver oxygen values if required.   (must be initialized
    ! after layer mass is defined)
    if (option(foxygen)==on) then
        do iroom = 1, nrm1
            roomptr => roominfo(iroom)
            p(iroom+nofoxyu)=0.23_eb*roomptr%mass(u)
            p(iroom+nofoxyl)=0.23_eb*roomptr%mass(l)
        end do
    end if

    return
    end subroutine initamb

! --------------------------- initialize_memory -------------------------------------------

    subroutine initialize_memory

    !     routine: initialize_memory
    !     purpose: This routine initializes the main memory
    !     Arguments: none

    integer i
    type(room_type), pointer :: roomptr

    ! simple control stuff
    debugging = .false.
    jaccol = -2

    ! DASSL forcing functions
    p(1:maxteq) = 0.0_eb

    ! set the time step and inner step division for time splitting
    ! we do not let the user choose these
    deltat = 1.0_eb

    ! time step checking
    stpmin = 1.0e-09_eb
    stpmin_cnt = 0
    stpmin_cnt_max = 100
    stpminflag = .true.

    ! define universal constants

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

    ! rooms
    n_species = 0
    nr = 0
    roominfo(1:mxrooms)%cwidth = xlrg
    roominfo(1:mxrooms)%cdepth = xlrg
    roominfo(1:mxrooms)%cheight = xlrg
    roominfo(1:mxrooms)%x0 = 0.0_eb
    roominfo(1:mxrooms)%y0 = 0.0_eb
    roominfo(1:mxrooms)%z0 = 0.0_eb
    roominfo(1:mxrooms)%x1 = xlrg
    roominfo(1:mxrooms)%y1 = xlrg
    roominfo(1:mxrooms)%z1 = xlrg
    roominfo(1:mxrooms)%ibar = 50
    roominfo(1:mxrooms)%jbar = 50
    roominfo(1:mxrooms)%kbar = 50
    adiabatic_walls = .false.
    roominfo(1:mxrooms)%deadroom = 0
    roominfo(1:mxrooms)%hall = .false.
    roominfo(1:mxrooms)%shaft = .false.
    roominfo(1:mxrooms)%sprinkler_activated = 0
    roominfo(1:mxrooms)%qdot_doorjet = 0.0_eb
    do i = 1, mxrooms
        roomptr => roominfo(i)
        roomptr%floor_area = roomptr%cwidth*roomptr%cdepth
        roomptr%cvolume = roomptr%cheight*roomptr%floor_area
        roomptr%matl(1:nwal) = 'OFF'
        roomptr%surface_on(1:nwal) = .false.
        roomptr%eps_w(1:nwal) = 0.0_eb
    end do
    
    ! room to room heat transfer
    nvcons = 0

    do i = 1, mxrooms
        roomptr => roominfo(i)
        
        ! variable cross sectional area
        roomptr%nvars = 0
        roomptr%var_volume(1:mxcross) = 0.0_eb
        roomptr%var_area(1:mxcross) = 0.0_eb
        roomptr%var_height(1:mxcross) = 0.0_eb
        
        ! initialize inter-compartment heat transfer fractions
        roomptr%iheat = 0
        roomptr%hheat_connections(1:mxrooms) = 0
        roomptr%heat_frac(1:mxrooms) = 0
    end do

    ! initialize number of furnace temperature nodes
    nfurn=0

    ! horizontal vents
    ihvent_connections(1:mxrooms,1:mxrooms) = 0.0_eb
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
    ijk(1:mxrooms,1:mxrooms,1:mxccv) = 0
    nventijk = 0

    ! vertical vents
    vshape(1:mxrooms,1:mxrooms) = 0
    ivvent_connections(1:mxrooms,1:mxrooms) = 0
    vvarea(1:mxrooms,1:mxrooms) = 0.0_eb
    ! start with vents open
    qcvv(1,1:mxrooms) = 0.0_eb
    qcvv(2,1:mxrooms) = 1.0_eb
    qcvv(3,1:mxrooms) = 0.0_eb
    qcvv(4,1:mxrooms) = 1.0_eb

    ! mechanical vents
    nnode = 0
    nfan = 0
    nfilter = 0
    nbr = 0
    next = 0
    mvcalc_on = .false.
    hvght(1:mxnode) = 0.0_eb
    hveflot(u:l,1:mxext) = 0.0_eb
    tracet(u:l,1:mxext) = 0.0_eb
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

    ! detectors
    n_detectors = 0
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

    ! targets
    n_targets = 0
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

    ! turn off objects
    n_fires = 0
    fireinfo(1:mxfires)%x_position = -1.0_eb
    fireinfo(1:mxfires)%y_position = -1.0_eb
    fireinfo(1:mxfires)%z_position = -1.0_eb
    fireinfo(1:mxfires)%room = 0
    fireinfo(1:mxfires)%name = ' '
    fireinfo(1:mxfires)%chemistry_type = 2
    fireinfo(1:mxfires)%ignition_type = 1
    fireinfo(1:mxfires)%ignition_criterion = 0.0_eb
    fireinfo(1:mxfires)%ignition_time = 0.0_eb
    fireinfo(1:mxfires)%ignited = .false.
    fireinfo(1:mxfires)%backtrack = .false.
    fireinfo(1:mxfires)%modified_plume = 1
    fireinfo(1:mxfires)%chirad = 0.35_eb

    fireinfo(1:mxfires)%qdot_at_activation(u) = 0.0_eb
    fireinfo(1:mxfires)%qdot_at_activation(l) = 0.0_eb
    fireinfo(1:mxfires)%qdot_layers(u) = 0.0_eb
    fireinfo(1:mxfires)%qdot_layers(l) = 0.0_eb

    ! trace species stuff
    fireinfo(1:mxfires)%total_pyrolysate = 0.0_eb
    fireinfo(1:mxfires)%total_trace = 0.0_eb
    tradio = 0.0_eb

    return
    end subroutine initialize_fire_objects

! --------------------------- initspecc -------------------------------------------

    subroutine initialize_species

    !     routine: initialize_species
    !     purpose: This routine initializes variables associated with species 
    !     Arguments: none

    real(eb) :: xt, xtemp, xh2o, totmass, initialmass(2,mxrooms,ns)
    integer i, j, k, ip, iprod, isof, isys, lsp
    type(room_type), pointer :: roomptr
    
    initial_mass_fraction(1:ns) = 0.0_eb

    ! normal air
    initial_mass_fraction(1) = 0.77_eb
    initial_mass_fraction(2) = 0.23_eb
    
    do i = 1, nrm1
        roomptr => roominfo(i)
        roomptr%species_mass(u:l,1:ns) = 0.0_eb
        roomptr%species_fraction(u:l,1:ns) = 0.0_eb

        !  set the water content to relative_humidity - the polynomial fit is to (t-273), and
        ! is for saturation pressure of water.  this fit comes from the steam
        ! tables in the handbook of physics and chemistry.
        xt = interior_temperature
        xtemp = 23.2_eb - 3.816e3_eb/(xt-46.0_eb)
        xh2o = exp(xtemp)/101325.0_eb*(18.016_eb/28.584_eb)
        initial_mass_fraction(h2o) = relative_humidity*xh2o

        ! normalize the atmosphere
        totmass = 0.0_eb
        do j = 1, ns
            totmass = totmass + initial_mass_fraction(j)
        end do
        initial_mass_fraction(1:ns) = initial_mass_fraction(1:ns)/totmass

        do k = u, l
            do lsp = 1, ns
                roomptr%species_output(k,lsp) = 0.0_eb
                initialmass(k,i,lsp) = initial_mass_fraction(lsp)*interior_rho*roomptr%volume(k)
            end do
        end do
    end do

    isof = nofprd
    do lsp = 1, ns
        do i = 1, nrm1
            do k = u, l
                isof = isof + 1
                p(isof) = initialmass(k,i,lsp)
            end do
        end do
    end do

    ! hvinit define initial products for hvac systems (if any)
    if (nhvsys/=0) then
        isof = nofhvpr
        do lsp = 1, min(ns,9)
            do isys = 1, nhvsys
                isof = isof + 1
                p(isof) = initial_mass_fraction(lsp)*hvtm(isys)
            end do
        end do
    end if

    ! define product map array
    i_speciesmap(1) = 1
    i_speciesmap(2) = 2
    ip = 2
    do iprod = 1, ns
        ip = ip + 1
        i_speciesmap(ip) = iprod + 2
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

    do itarg = 1, n_targets

        ! room number must be between 1 and nrm1
        targptr => targetinfo(itarg)
        iroom = targptr%room
        if (iroom<1.or.iroom>nrm1) then
            write(*,'(a,i0)') '***Error: Target assigned to non-existent compartment',iroom
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
        xsize = roomptr%cwidth
        ysize = roomptr%cdepth
        zsize = roomptr%cheight

        ! if the locator is -1, set to center of room on the floor
        if (xloc==-1.0_eb) xloc = 0.5_eb*xsize
        if (yloc==-1.0_eb) yloc = 0.5_eb*ysize
        if (zloc==-1.0_eb) zloc = 0.0_eb
        if (iwall/=0) then
            xxnorm = 0.0_eb
            yynorm = 0.0_eb
            zznorm = 0.0_eb
        end if
        if (iwall==1) then
            zznorm = -1.0_eb
            xx = xloc
            yy = yloc
            zz = zsize
        else if (iwall==2) then
            yynorm = -1.0_eb
            xx = xsize
            yy = ysize
            zz = yloc
        else if (iwall==3) then
            xxnorm = -1.0_eb
            xx = xsize
            yy = xloc
            zz = yloc
        else if (iwall==4) then
            yynorm = 1.0_eb
            xx = xloc
            yy = 0.0_eb
            zz = yloc
        else if (iwall==5) then
            xxnorm = 1.0_eb
            xx = 0.0_eb
            yy = ysize
            zz = yloc
        else if (iwall==6) then
            zznorm = 1.0_eb
            xx = xloc
            yy = ysize
            zz = 0.0_eb
        end if
        if (iwall/=0) then
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
            if (roomptr%surface_on(iwall2)) then
                targptr%material = roomptr%matl(iwall2)
            else
                targptr%material = ' '
            end if
        end if

        ! center coordinates need to be within room
        if (xloc<0.0_eb.or.xloc>xsize.or.yloc<0.0_eb.or.yloc>ysize.or.zloc<0.0_eb.or.zloc>zsize) then
            write(*,'(a,i0,1x,3f10.3)') '***Error: Target located outside of compartment', iroom, xloc, yloc, zloc
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

    !        kw = thermal conductivity
    !        cw = specific heat (j/kg)
    !        rhow = density of the wall (kg/m**3)
    !        thickw = thickness of the wall (m)
    !        epsw = emmisivity of the wall
    !        nslb = discretization of the wall slabs (number of nodes)
    !        matl contains the name of the thermal data subset in the tpp datafile
    !        nthrmp is a count of the number of tpp data sets in the database

    real(eb), intent(in) :: tstop
    integer :: i, j, jj, k, itarg, ifromr, itor, ifromw, itow, nslabf, nslabt, nptsf, nptst, wfrom, wto
    real(eb) :: k_w(mxslb), c_w(mxslb), rho_w(mxslb), thick_w(mxslb), thick, wtemps(nnodes), walldx(nnodes)
    integer nslab, numnode(mxslb+1)
    character(mxthrmplen):: off = 'OFF', none = 'NONE', tcname

    ! tp is the pointer into the data base for each material
    integer tp

    type(room_type), pointer :: roomptr, from_roomptr, to_roomptr
    type(target_type), pointer :: targptr
    type(thermal_type), pointer :: thrmpptr

    ! map the thermal data into its appropriate wall specification
    ! if name is "OFF" or "NONE" then just turn all off
    do i = 1, nwal
        do j = 1, nrm1
            roomptr => roominfo(j)
            if (roomptr%surface_on(i)) then
                if (roomptr%matl(i)==off.or.roomptr%matl(i)==none) then
                    roomptr%surface_on(i) = .false.
                else
                    call get_thermal_property(roomptr%matl(i),tp)
                    thrmpptr => thermalinfo(tp)
                    roomptr%eps_w(i) = thrmpptr%eps
                    roomptr%nslab_w(i) = thrmpptr%nslab
                    do k = 1, roomptr%nslab_w(i)
                        roomptr%k_w(k,i) = thrmpptr%k(k)
                        roomptr%c_w(k,i) = thrmpptr%c(k)
                        roomptr%rho_w(k,i) = thrmpptr%rho(k)
                        roomptr%thick_w(k,i) = thrmpptr%thickness(k)
                    end do
                end if
            end if
        end do
    end do

    ! initialize temperature profile data structures
    do i = 1, nrm1
        roomptr => roominfo(i)
        do j = 1, nwal
            roomptr%t_profile(1:nnodes,j) = interior_temperature
            if (roomptr%surface_on(j)) then
                k_w(1:mxslb) = roomptr%k_w(1:mxslb,j)
                c_w(1:mxslb) = roomptr%c_w(1:mxslb,j)
                rho_w(1:mxslb) = roomptr%rho_w(1:mxslb,j)
                thick_w(1:mxslb) = roomptr%thick_w(1:mxslb,j)
                nslab = roomptr%nslab_w(j)
                thick = roomptr%total_thick_w(j)
                numnode = roomptr%nodes_w(1:mxslb+1,j)
                wtemps = roomptr%t_profile(1:nnodes,j)
                walldx = roomptr%walldx(1:nnodes,j)
                call wset(numnode,nslab,tstop,walldx,wsplit,k_w,c_w,rho_w,thick_w,&
                   thick,wtemps,interior_temperature,exterior_temperature)
                roomptr%nodes_w(1:mxslb+1,j) = numnode
                roomptr%t_profile(1:nnodes,j) = wtemps
                roomptr%walldx(1:nnodes,j) = walldx
            end if
        end do
    end do

    ! concatenate slab properties of wall nodes that are connected to each other
    do i = 1, nvcons
        ifromr = i_vconnections(i,w_from_room)
        ifromw = i_vconnections(i,w_from_wall)
        itor = i_vconnections(i,w_to_room)
        itow = i_vconnections(i,w_to_wall)
        from_roomptr => roominfo(ifromr)
        to_roomptr => roominfo(itor)

        nslabf = from_roomptr%nslab_w(ifromw)
        nslabt = to_roomptr%nslab_w(itow)
        from_roomptr%nslab_w(ifromw) = nslabf + nslabt
        to_roomptr%nslab_w(itow) = nslabf + nslabt

        nptsf = from_roomptr%nodes_w(1,ifromw)
        nptst = to_roomptr%nodes_w(1,itow)
        to_roomptr%nodes_w(1,itow) = nptsf + nptst - 1
        from_roomptr%nodes_w(1,ifromw) = nptsf + nptst - 1

        wfrom = from_roomptr%total_thick_w(ifromw)
        wto = to_roomptr%total_thick_w(itow)
        from_roomptr%total_thick_w(ifromw) = wfrom + wto
        to_roomptr%total_thick_w(itow) = wfrom + wto

        jj = nslabt + 1
        do j = nslabf+1, nslabf+nslabt
            jj = jj - 1
            from_roomptr%k_w(j,ifromw) = to_roomptr%k_w(jj,itow)
            from_roomptr%c_w(j,ifromw) = to_roomptr%c_w(jj,itow)
            from_roomptr%rho_w(j,ifromw) = to_roomptr%rho_w(jj,itow)
            from_roomptr%thick_w(j,ifromw) = to_roomptr%thick_w(jj,itow)
            from_roomptr%nodes_w(j+1,ifromw) = to_roomptr%nodes_w(jj+1,itow)
        end do

        jj = nslabf + 1
        do j = nslabt+1, nslabt+nslabf
            jj = jj - 1
            to_roomptr%k_w(j,itow) = from_roomptr%k_w(jj,ifromw)
            to_roomptr%c_w(j,itow) = from_roomptr%c_w(jj,ifromw)
            to_roomptr%rho_w(j,itow) = from_roomptr%rho_w(jj,ifromw)
            to_roomptr%thick_w(j,itow) = from_roomptr%thick_w(jj,ifromw)
            to_roomptr%nodes_w(j+1,itow) = from_roomptr%nodes_w(jj+1,ifromw)
        end do

        do j = 1,nptsf
            from_roomptr%t_profile(j,ifromw) = interior_temperature
            to_roomptr%t_profile(j,itow) = interior_temperature
        end do
        jj = nptst
        do j = nptsf+1,nptsf+nptst - 1
            jj = jj - 1
            from_roomptr%t_profile(j,ifromw) = interior_temperature
            from_roomptr%walldx(j-1,ifromw) = to_roomptr%walldx(jj,itow)
        end do

        jj = nptsf
        do j = nptst+1,nptst+nptsf - 1
            jj = jj - 1
            to_roomptr%t_profile(j,itow) = interior_temperature
            to_roomptr%walldx(j-1,itow) = from_roomptr%walldx(jj,ifromw)
        end do
    end do

    ! initialize target data structures
    do itarg = 1, n_targets
        targptr => targetinfo(itarg)
        tcname = targptr%material
        if (tcname==' ') then
            tcname = 'DEFAULT'
            targptr%material = tcname
        end if
        call get_thermal_property(tcname,tp)
        thrmpptr => thermalinfo(tp)
        targptr%k = thrmpptr%k(1)
        targptr%c = thrmpptr%c(1)
        targptr%rho = thrmpptr%rho(1)
        targptr%thickness = thrmpptr%thickness(1)
        targptr%depth_loc = max(0.0_eb,min(targptr%thickness*targptr%depth_loc,targptr%thickness))
        targptr%emissivity = thrmpptr%eps
    end do

    return
    end subroutine initialize_walls

! --------------------------- offset -------------------------------------------

    subroutine offset ()

    ! purpose: offset in the following context is the beginning of the vector for that particular variable minus one.
    !          thus, the actual pressure array goes from nofp+1 to nofp+nrm1.  the total number of equations to be considered
    !          is nequals, and is the last element in the last vector. each physical interface routine is responsible for
    !          the count of the number of elements in the vector for which it is resonsible.

    ! this set of parameters is set by nputp and is kept in the environment module cenviro.
    ! to index a variable, the list is something like (for temperature in this case)

    ! noftu+1, noftu+nrm1

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
        write (*,*) '***Error: offset - Too many nodes in hvac specification'
        write (logerr,*) '***Error: offset - Too many nodes in hvac specification'
        stop
    end if

    ! set the number of compartments and offsets
    nrm1 = nr - 1

    ! count the species
    n_species = 0

    do i = 1, ns
        if  (i/=7) then
            n_species = n_species + 1
        end if
    end do
    n_species = n_species + 1

    ! count the number of walls
    nhcons = 0
    do i = 1, nrm1
        roomptr => roominfo(i)
        do j = 1, nwal
            if (roomptr%surface_on(j)) then
                nhcons = nhcons + 1
            end if
            if (nwpts/=0) roomptr%nodes_w(1,j) = nwpts
        end do
    end do

    ! set number of implicit oxygen variables
    if (option(foxygen)==on) then
        noxygen = nrm1
    else
        noxygen = 0
    end if

    ! now do all the equation offsets
    nhvpvar = nnode - next
    nhvtvar = nbr
    nofp = 0
    nofpmv = nofp + nrm1
    noftmv = nofpmv + nhvpvar
    noffsm = noftmv + nhvtvar
    noftu = noffsm
    nofvu = noftu + nrm1
    noftl = nofvu + nrm1
    nofoxyl = noftl + nrm1
    nofoxyu = nofoxyl + noxygen
    nofwt = nofoxyu + noxygen
    nofprd = nofwt + nhcons
    nofhvpr = nofprd + 2*nrm1*n_species

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
    real(eb), intent(in) :: tstop, wsplit(*), wk(*), wspec(*), wrho(*), wthick(*), tamb, text
    integer, intent(inout) :: numnode(*)
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
            iend = cumpts(nslab+1) ! include last point for last slab
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
