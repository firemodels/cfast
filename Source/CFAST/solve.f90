module solve_routines

    use precision_parameters

    use conduction_routines, only: conduction
    use convection_routines, only: convection
    use debug_routines, only: output_spreadsheet_residuals
    use fire_routines, only: fire, door_jet, integrate_mass, update_species, remap_fires, update_fire_objects
    use isosurface, only: output_isodata
    use hflow_routines, only: horizontal_flow
    use mflow_routines, only: mechanical_flow
    use numerics_routines, only : ddassl, jac, setderv, snsqe, gjac
    use opening_fractions, only : findwhichvent, getventfraction
    use output_routines, only: output_results, output_status, output_debug, deleteoutputfiles, find_error_component
    use radiation_routines, only: radiation
    use smokeview_routines, only: output_smokeview, output_smokeview_header, output_smokeview_plot_data, output_slicedata
    use spreadsheet_routines, only: output_spreadsheet, output_spreadsheet_smokeview
    use target_routines, only: target, update_detectors, get_detector_temp_and_velocity
    use utility_routines, only: mat2mult, interp, shellsort, cptime, xerror, funit
    use vflow_routines, only: vertical_flow

    use cenviro
    use ramp_data
    use setup_data
    use solver_data
    use debug_data
    use cparams
    use target_data
    use fire_data
    use option_data
    use smkview_data
    use vent_data
    use wallptrs
    use room_data

    implicit none

    private

    public solve_simulation, calculate_residuals, output_interactive_help, update_data

    contains

! --------------------------- initial_solution -------------------------------------------

    subroutine initial_solution(t,pdold,pdzero,rpar,ipar)

    !     Routine: Initial_solution
    !     Purpose: This routine determines an initial solution to
    !              the zone fire modeling equations.  A non-linear
    !              algebraic solver (SNSQE) is used to calculate initial
    !              room pressures that make dP/dt zero.  If an HVAC system
    !              is modeled then HVAC node pressures and hvac duct
    !              temperatures are also determined to force mass and energy
    !              conservation.

    integer, intent(in) :: ipar(*)
    real(eb), intent(in) :: t,pdzero(*), rpar(*)
    real(eb), intent(out) :: pdold(*)

    integer, parameter :: mxalg = 4*mxrooms+mxnode+mxbranch
    real(eb) deltamv(mxalg), hhvp(mxalg)
    integer, parameter :: lrwork = (3*mxalg**2+13*mxalg)/2
    real(eb) :: work(lrwork)
    integer :: ires, iopt, nhvalg, nalg0, nalg1, nprint, i, info, nodes
    real(eb) :: tol

    type(room_type), pointer :: roomptr

    ires = 0
1   continue

    call room_connections (t)

    rpar2(1) = rpar(1)
    ipar2(1) = ipar(1)
    ipar2(2) = ipar(2)
    call setderv(-1)
    call calculate_residuals(t,p,pdzero,pdold,ires,rpar2,ipar2)
    iopt = 2
    tol = algtol
    nhvalg = nhvpvar + nhvtvar
    nalg0 = nhvalg
    nalg1 = nrm1 + nhvalg
    nprint = -1

    ! room pressures
    do i = 1, nrm1
        hhvp(i) = p(i+nofp)
    end do

    ! hvac pressures
    do i = 1, nhvpvar
        hhvp(i+nrm1) = p(i+nofpmv)
    end do

    ! hvac temperatures
    do i = 1, nhvtvar
        hhvp(i+nrm1+nhvpvar) = p(i+noftmv)
    end do

    do i = 1, nequals
        pinit(i) = p(i)
    end do
    if (option(fpsteady)==1) then
        call snsqe(gres,gjac,iopt,nalg1,hhvp,deltamv,tol,nprint,info, work,lrwork)
    else
        if (nhvalg>0) then
            call snsqe(gres2,gjac,iopt,nalg0,hhvp(1+nrm1),deltamv(1+nrm1),tol,nprint,info,work,lrwork)
        else
            info = 1
        end if
    end if

    ! couldn't find a solution.  either try to recover or stop
    if (info/=1) then
        if (option(fpsteady)/=off) then
            option(fpsteady) = off
            call xerror('Trying non-steady initial guess' ,0,101,0)
            go to 1
        end if
        call xerror('Solver could not find an initial solution' ,0,102,2)
    end if

    ! if a room is not connected to any other room via a horizontal or
    ! vertical vent then do not use the snsqe pressure solution,
    ! use the original pressure solution that was based on rho*g*h.
    do i = 1, nrm1
        roomptr => roominfo(i)
        if (roomptr%is_connection) p(i+nofp) = hhvp(i)
    end do
    do i = 1, nhvpvar
        p(i+nofpmv) = hhvp(i+nrm1)
    end do
    do i = 1, nhvtvar
        p(i+noftmv) = hhvp(i+nrm1+nhvpvar)
    end do

    call calculate_residuals(t,p,pdzero,pdold,ires,rpar,ipar)

    ! Added to synchronize_species_mass the species mass with the total mass of each layer at the new pressure
    nodes = nofprd+1
    call synchronize_species_mass (p,nodes)
    do i = 1, nhvpvar
        pdold(i+nofpmv) = 0.0_eb
    end do
    do i = 1, nhvtvar
        pdold(i+noftmv) = 0.0_eb
    end do
    do i = 1, nhcons
        pdold(i+nofwt) = 0.0_eb
    end do
    return
    end subroutine initial_solution

! --------------------------- room_connections -------------------------------------------

    subroutine room_connections (tsec)

    ! routine: room_connections
    ! purpose: this routine determines whether flow from each room can reach the outside (perhaps through intermediate rooms)
    !           via horizontal or vertical vents.  if a room is isolated from the outside then snsqe has trouble finding an
    !           initial pressure solution.
    ! arguments: tsec: current simulation time

    real(eb), intent(in) :: tsec

    real(eb) :: fraction, height, width, avent
    integer roomc(mxrooms,mxrooms), tempmat(mxrooms,mxrooms), i, iroom1, iroom2, ik, im, ix, matiter
    integer, parameter :: toprm = 1, botrm = 2

    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    ! initially assume that no rooms are connected
    roomc(1:nr,1:nr) = 0
    do i = 1, nr
        roomc(i,i) = 1
    end do

    ! check horizontal vent flow
    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        iroom1 = ventptr%room1
        iroom2 = ventptr%room2
        ik = ventptr%counter
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        call getventfraction ('H',iroom1,iroom2,ik,i,tsec,fraction,ventptr)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = fraction*height*width
        if (avent/=0.0_eb) then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        end if
    end do

    ! check vertical vent flow
    do i = 1, n_vvents
        iroom1 = ivvent(i,toprm)
        iroom2 = ivvent(i,botrm)
        if (vvarea(iroom1,iroom2)/=0.0_eb) then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        end if
    end do

    ! construct roomc**matiter where matiter > nr
    ! note:  roomc is a transitiion matrix (from markov chain theory). that is, roomc(i,j) is zero if there no connection
    !        between room and room j.  similarly, roomc(i,j) is one if there is a connection between these two rooms.
    !        roomc is symmetric. the matrix roomc**2 is tells us whether flow can get from room i to room j in two steps.
    !        since there are only nr rooms, roomc**nr tells us whether any given room is connected to any
    !        other room in nr steps. the entries roomc**nr(i,nr) then indicates whether a room is connected
    !        to the outside (perhaps through several other intermediate rooms).
    matiter = 1
    do i = 1, nr
        if (nr<=matiter) exit
        call mat2mult(roomc,tempmat,mxrooms,nr)
        matiter = matiter*2
    end do

    do i = 1, nrm1
        roomptr => roominfo(i)
        if (roomc(i,nr)/=0) then
            roomptr%is_connection = .true.
        else
            roomptr%is_connection = .false.
        end if
    end do

    return
    end subroutine room_connections

! --------------------------- gres -------------------------------------------

    subroutine gres (nnn,hvpsolv,deltamv,iflag)

    !     routine: gres
    !     purpose: calculates residuals for initial solution by snsqe
    !     revision: $revision: 352 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: nnn
    !                hvpsolv
    !                deltamv
    !                iflag

    integer, intent(in) :: nnn
    real(eb), intent(in) :: hvpsolv(nnn)
    integer, intent(out) :: iflag
    real(eb), intent(out) :: deltamv(*)

    integer :: nalg, i, ires
    real(eb) :: p2(maxteq), delta(maxteq), pdzero(maxteq), T

    type(room_type), pointer :: roomptr

    data pdzero /maxteq*0.0_eb/

    if (1.eq.2) iflag=-1 ! dummy statement to eliminate compiler warnings
    nalg = nrm1 + nhvpvar + nhvtvar
    do i = 1, nalg
        p2(i) = hvpsolv(i)
    end do
    do i = nalg + 1, nequals
        p2(i) = pinit(i)
    end do
    if (iprtalg/=0) then
        write(iofilo,*) 'room pressures'
        do i = 1, nrm1
            write(iofilo,*) i,p2(i)
        end do
        if (nhvpvar>0) write (iofilo,*) 'hvac pressures'
        do i = 1, nhvpvar
            write(iofilo,*)i,p2(i+nofpmv)
        end do
        if (nhvtvar>0) write (iofilo,*) 'hvac temperatures'
        do i = 1, nhvtvar
            write(iofilo,*)i,p2(i+noftmv)
        end do
    end if
    t = stime
    ires = 0
    call calculate_residuals(t,p2,pdzero,delta,ires,rpar2,ipar2)
    do i = 1, nalg
        deltamv(i) = delta(i)
    end do
    do i = 1, nrm1
        roomptr => roominfo(i)
        if (.not.roomptr%is_connection) deltamv(i) = 0.0_eb
    end do
    if (iprtalg/=0) then
        write(iofilo,*)'room pressure residuals'
        do i = 1, nrm1
            write(iofilo,*)i,delta(i)
        end do
        if (nhvpvar>0)write (iofilo,*) 'hvac pressure residuals'
        do i = 1, nhvpvar
            write(iofilo,*)i,delta(i+nofpmv)
        end do
        if (nhvtvar>0)write (iofilo,*) 'hvac temperature residuals'
        do i = 1, nhvtvar
            write(iofilo,*)i,delta(i+noftmv)
        end do
        write(iofilo,*)' '
        read (*,*)
    end if
    return
    end subroutine gres

! --------------------------- gres2 -------------------------------------------

    subroutine gres2 (nnn,hvsolv,deltamv,iflag)

    !     routine: gres2
    !     purpose: calculates residuals for initial solution by snsqe
    !              (HVAC pressure and temperature)
    !     revision: $revision: 352 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     Arguments: NNN
    !                HVSOLV
    !                DELTAMV
    !                IFLAG

    integer, intent(in) :: nnn
    real(eb), intent(in) :: hvsolv(nnn)
    integer, intent(out) :: iflag
    real(eb), intent(out) :: deltamv(*)

    real(eb) :: p2(maxteq), delta(maxteq), pdzero(maxteq), t
    integer :: i, ires
    data pdzero /maxteq*0.0_eb/

    if (1.eq.2)iflag=-1 ! dummy statement to eliminate compiler warnings
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
        if (nhvpvar>0)write (iofilo,*) 'hvac pressures'
        do i = 1, nhvpvar
            write (iofilo,*) i, hvsolv(i)
        end do
        if (nhvtvar>0)write (iofilo,*) 'hvac temperatures'
        do i = 1, nhvtvar
            write (iofilo,*) i, hvsolv(nhvpvar+i)
        end do
    end if
    t = stime
    ires = 0
    call calculate_residuals(t,p2,pdzero,delta,ires,rpar2,ipar2)
    do i = 1, nhvpvar
        deltamv(i) = delta(i+nofpmv)
    end do
    do i = 1, nhvtvar
        deltamv(i+nhvpvar) = delta(i+noftmv)
    end do
    if (iprtalg/=0) then
        write (iofilo,*) ' '
        if (nhvpvar>0)write (iofilo,*) 'hvac pressure residuals'
        do i = 1, nhvpvar
            write (iofilo,*) i, deltamv(i)
        end do
        if (nhvtvar>0)write (iofilo,*) 'hvac temperature residuals'
        do i = 1, nhvtvar
            write (iofilo,*) i, deltamv(i+nhvpvar)
        end do
        write(iofilo,*)' '
        read(*,*)
    end if
    return
    end subroutine gres2

! --------------------------- solve_simulation -------------------------------------------

    subroutine solve_simulation (tstop)

    !     Routine: solve_simulaiton
    !     Purpose: main solution loop for the model
    !     Arguments: TSTOP   The final time to which CFAST should run

    !     Offset in the following context is the beginning of the vector for
    !     that particular variable minus one.  Thus, the actual pressure array
    !     goes from NOFP+1 to NOFP+nrm1.  The total number of equations to be
    !     considered is NEQUALS, and is the last element in the last vector.
    !     Each physical interface routine is responsible for the COUNT of the
    !     number of elements in the vector for which it is resonsible.

    !     This set of parameters is set by NPUTP and is kept in the environment
    !     common block CFAST.INC.  To index a variable, the list is something
    !     like (for temperature in this case)

    !     NOFTU+1, NOFTU+nrm1

    !     The structure of the solver array is

    !     NOFP = offset for the main pressure; the array of base pressures for each compartment
    !     NOFPMV = offset for HVAC node pressuers
    !     NOFTMV = offset for HVAC branch temperatures
    !     NOFTU = upper layer temperature
    !     NOFVU = upper layer volume
    !     NOFTL = lower layer temperature
    !     NOFWT = wall surface temperatures (equivalent to the number of profiles)
    !     NOFPRD = species
    !     NEQUALS = last element in the array.

    !     The arrays which use this structure are VATOL, VRTOL, P, PDOLD, PPRIME and PDZERO

    !     An important note - solve sets the last variable to be solved to NOFPRD
    !     which is the beginning of the species (-1) and the end of the array which
    !     is presently used by DASSL. The important point is that NODES is set to
    !     NOFPRD which is the equivalent to NOFWT+nhcons

    real(eb), intent(in) :: tstop

    integer, parameter :: maxord = 5
    integer, parameter :: lrwork = 40+(maxord+4)*maxeq+maxeq**2
    integer, parameter :: liw = 20+maxeq
    integer, parameter :: all = 1, some = 0

    real(eb) :: rwork(lrwork), rpar(1)
    integer :: iwork(liw), info(15), ipar(3), info2(15)
    real(eb) :: pprime(maxteq), pdnew(maxteq), vatol(maxeq), vrtol(maxeq)
    real(eb) :: pdzero(maxteq) = 0.0_eb
    logical :: iprint, ismv, exists, ispread,firstpassforsmokeview
    integer :: idid, i, nodes, nfires, icode, ieqmax, idisc, ires, idsave, ifdtect, ifobj, isensor
    real(eb) :: ton, toff, tpaws, tstart, tdout, dprint, dplot, dspread, t, tprint, td, tsmv, tspread, tout,  &
        ostptime, tdtect, tobj
    integer :: first_time
    integer :: stopunit, stopiter, ios
    
    type(fire_type), pointer :: fireptr

    call cptime(toff)
    ires = 0
    tpaws = tstop + 1.0_eb
    tstart = i_time_step - 1
    told = tstart
    dt = tstop - tstart
    dprint = abs(print_out_interval)
    dplot = abs(smv_out_interval)
    dspread = abs(ss_out_interval)
    rpar(1) = rptol

    ! initialize print and output times
    t = tstart
    tprint = t
    tsmv = t
    tspread = t
    idid = 1
    firstpassforsmokeview = .true.
    first_time = 1

    ! Output options
    if (dprint<0.0001_eb.or.print_out_interval==0) then
        iprint = .false.
        tprint = tstop + 1.0_eb
    else
        iprint = .true.
    end if

    if (smv_out_interval<=0) then
        ismv = .false.
        tsmv = tstop + 1.0_eb
    else
        ismv = .true.
    end if

    if (dspread<0.0001_eb.or.ss_out_interval<=0) then
        ispread = .false.
        tspread = tstop + 1.0_eb
    else
        ispread = .true.
    end if

    call set_info_flags (info, rwork)

    ! copy error tolerances into arrays. if the location of pressure is
    ! changed in the solver array then the following code has to be changed
    do i = 1, nrm1
        vatol(i+nofp) = aptol
        vrtol(i+nofp) = rptol
        vatol(i+noftu) = atol
        vrtol(i+noftu) = rtol
        vatol(i+nofvu) = atol
        vrtol(i+nofvu) = rtol
        vatol(i+noftl) = atol
        vrtol(i+noftl) = rtol
        if (option(foxygen)==on) then
            vatol(i+nofoxyu)=atol
            vrtol(i+nofoxyu)=rtol
            vatol(i+nofoxyl)=atol
            vrtol(i+nofoxyl)=rtol
        end if
    end do
    do i = 1, nhvpvar
        vatol(i+nofpmv) = ahvptol
        vrtol(i+nofpmv) = rhvptol
    end do
    do i = 1, nhvtvar
        vatol(i+noftmv) = ahvttol
        vrtol(i+noftmv) = rhvttol
    end do
    do i = 1, nhcons
        vatol(i+nofwt) = awtol
        vrtol(i+nofwt) = rwtol
    end do

    ovtime = 0.0_eb
    tovtime = 0.0_eb
    tottime = 0.0_eb
    prttime = 0.0_eb

    ! See note in comments about the nodes=nofprd line below
    nodes = nofprd
    ipar(1) = nodes
    ipar(2) = all
    idset = 0

    ! construct initial solution
    do i = 1, nequals
        pdold(i) = 0.0_eb
        pold(i) = p(i)
    end do
    call initial_solution(t,pdold,pdzero,rpar,ipar)
    do i = 1, nequals
        pprime(i) = pdold(i)
        pold(i) = p(i)
    end do

    ! Calculate the mass of objects that have been pyrolized
    ! at the moment we do only the total and the radiological species
    ! make sure that the INTEGRATE routine is called before update_species
    call integrate_mass (t, dt)
    call update_species (0.0_eb)

    ! If we are running only an initialization test then we do not need to solve anything
    if (initializeonly) then
        ! normally, this only needs to be done while running. however, if we are doing an initialonly run
        ! then we need the output now
        call remap_fires (nfires)
        call output_smokeview(pressure_ref, exterior_abs_pressure, exterior_temperature, nrm1,  &
             n_hvents, n_vvents, nfires, smv_room, smv_xfire, smv_yfire, smv_zfire, n_targets, 0.0_eb, 1)
        icode = 0
        write (*, '(a)') 'Initialize only'
        write (logerr, '(a)') 'Initialize only'
        return
    end if

    ! main solve loop
    numjac = 0
    numstep = 0
    numresd = 0

10  continue

    ! DASSL equation with most error
    ieqmax = 0

    ! Check for interactive commands
    ! if a key has been pressed (and we are watching the keyboard) figure out what to do
    ! The escape key returns a code of 1
    if (.not.nokbd) call keyboard_interaction (t,icode,tpaws,tout,ieqmax)
    inquire (file=stopfile, exist =exists)
    stopiter=-1
    if (exists) then
       stopunit=funit(14)
       open(unit=stopunit,file=stopfile)
       read(stopunit,*,iostat=ios) stopiter
       if (ios.ne.0) stopiter=0
       close(unit=stopunit)
       icode = 1
    end if
    ! If the stop file exists or the esc key has been pressed, then quit
    if (icode==1.and.stopiter.eq.0) then
        call deleteoutputfiles (stopfile)
        write (*,'(a,1pg11.3,a,g11.3)') 'Stopped by request at T = ', t, ' DT = ', dt
        write (logerr,'(a,1pg11.3,a,g11.3)') 'Stopped by request at T = ', t, ' DT = ', dt
        return
    end if

    ! Check the .query file. If it does not exist, do nothing. If if DOES exist, then
    ! rewind/write the status file and delete the query file (in that order).
    ! Ignore errors from deleting the file. It may not exist
    inquire (file=queryfile, exist = exists)
    if (exists) then
        call output_status (T, dT)
        call deleteoutputfiles (queryfile)
    end if

    !Check to see if diagnostic files .resid and .jac exist. If they do exist
    !set flags and open file, if needed, to print diagnositic information.
    inquire (file=residfile, exist=exists)
    if (exists) then
        residprn = .true.
        if (residfirst) then
            residfirst = .false.
            ioresid = funit(150)
            open(unit=ioresid,file=residcsv)
            ioslab = funit(150)
            open(unit = ioslab, file=slabcsv)
        end if
        inquire (file=jacfile,exist=exists)
        if (exists) then
            if (jacfirst) then
                jacfirst = .false.
                iojac = funit(150)
                open(unit=iojac,file=jaccsv)
            end if
            jacprn = .true.
        else
            jacprn = .false.
        end if
    else
        residprn = .false.
        jacprn = .false.
    end if

    ! now do normal output (printout, spreadsheets, ...)
    if (idid>0) then

        ! printed output
        if (t+0.0001_eb>min(tprint,tstop).and.iprint) then
            i_time_step = tprint
            call output_results (t)
            call output_status (t, dt)
            tprint = tprint + dprint
            numjac = 0
            numstep = 0
            numresd = 0
            prttime = 0.0_eb
        end if

        ! smokeview output
        if (t+0.0001_eb>min(tsmv,tstop).and.ismv) then
            i_time_step = tsmv
            call remap_fires (nfires)
            if (firstpassforsmokeview) then
                firstpassforsmokeview = .false.
                ! note: output_smokeview writes the .smv file. we do not close the file but only rewind so that smokeview
                ! can have the latest time step information. remap_fires just puts all of the information in a single list
                call output_smokeview (pressure_ref, exterior_abs_pressure, exterior_temperature, nrm1, &
                    n_hvents, n_vvents, nfires, smv_room, smv_xfire, smv_yfire, smv_zfire, n_targets, t, i_time_step)
                call output_smokeview_header (version,nrm1,nfires)
            end if
            smv_relp(1:nrm1) = roominfo(1:nrm1)%relp
            smv_zlay(1:nrm1) = roominfo(1:nrm1)%depth(l)
            smv_tu(1:nrm1) = roominfo(1:nrm1)%temp(u)
            smv_tl(1:nrm1) = roominfo(1:nrm1)%temp(l)
            call output_smokeview_plot_data(t,nrm1,smv_relp,smv_zlay,smv_tl,smv_tu,nfires, smv_qdot,smv_height)
            call output_spreadsheet_smokeview(t)
            tsmv = tsmv + dplot
            call output_status (t, dt)
            call output_slicedata(t,first_time)
            call output_isodata(t,first_time)
            first_time = 0
        end if

        ! spreadsheet output
        if (t+0.0001_eb>min(tspread,tstop).and.ispread) then
            call output_spreadsheet(t)
            i_time_step = tspread
            tspread =tspread + dspread
            call output_status (t, dt)
        end if

        ! diagnostic output
        if (t+0.0001_eb>tpaws) then
            i_time_step = tpaws
            call output_results (t)
            call output_debug (1,t,dt,ieqmax)
            tpaws = tstop + 1.0_eb
            call output_status (t, dt)
        end if

        ! find the interval next discontinuity is in
        idisc = 0
        do i = 1, ndisc
            if (t>=discon(i-1).and.t<discon(i)) then
                idisc = i
                exit
            end if
        end do
        tout = min(tprint, tsmv, tspread, tpaws, tstop)

        ! if there is a discontinuity then tell DASSL
        if (idisc/=0) then
            tout = min(tout,discon(idisc))
            rwork(1) = discon(idisc)
            info(4) = 1
        else
            info(4) = 0
        end if
    end if

    if (t<tstop) then
        idset = 0
        ipar(2) = some
        told = t
        call setderv(-1)
        call cptime(ton)
        call ddassl (calculate_residuals,nodes,t,p,pprime,tout,info,vrtol,vatol,idid,rwork,lrwork,iwork,liw,rpar,ipar,jac)
        ! call cpu timer and measure, solver time within dassl and overhead time (everything else).
        call setderv(-2)
        ieqmax = ipar(3)
        if (option(fpdassl)==on) call output_debug (3,t,dt,ieqmax)
        ostptime = ton - toff
        call cptime(toff)
        stime = t
        stptime = toff - ton
        prttime = prttime + stptime
        tottime = tottime + stptime
        ovtime = ovtime + ostptime
        tovtime = tovtime + ostptime

        ! make sure dassl is happy
        if (idid<0) then
            call find_error_component (ieqmax)
            write (*,'(a,i0)') '***Error, dassl - idid = ', idid
            write (logerr,'(a,i0)') '***Error, dassl - idid = ', idid
            call cfastexit ('CFAST', idid)
            stop
        end if

        dt = t - told
        if (stpminflag) then
            if (dt<stpmin) then
                stpmin_cnt = stpmin_cnt + 1
                if (stpmin_cnt>stpmin_cnt_max) then
                    ! model has hung (stpmin_cnt_max consective time step sizes were below stpmin)
                    write(*,'(i0,a,e11.4,a,e11.4)') &
                        '***Error: Consecutive time steps with size below ', stpmin_cnt_max, stpmin, ' at t = ', t
                    write(logerr,'(i0,a,e11.4,a,e11.4)') &
                        '***Error: Consecutive time steps with size below ', stpmin_cnt_max, stpmin, ' at t = ', t
                    call cfastexit ('CFAST',1)
                    stop
                end if
            else
                ! this time step is above the critical size so reset counter
                stpmin_cnt = 0
            end if
        end if

        ipar(2) = all
        call calculate_residuals (t,p,pdzero,pdnew,ires,rpar,ipar)
        call update_solution (nodes, nequals, n_species, t, told, p, pold, pdnew, pdold)

        ! advance the detector temperature solutions and check for object ignition
        idsave = 0
        call get_detector_temp_and_velocity
        call update_detectors (check_detector_state,told,dt,n_detectors,idset,ifdtect,tdtect)
        call update_fire_objects (check_detector_state,told,dt,ifobj,tobj)
        td = min(tdtect,tobj)

        ! a detector is the first one that went off
        if (ifdtect>0.and.tdtect<=td) then
            isensor = ifdtect
            call update_detectors (set_detector_state,told,dt,n_detectors,idset,ifdtect,tdtect)
            ! check to see if we are backing up for detectors going off
            if (option(fbtdtect)==on) then
                idsave = idset
            else
                idsave = ifobj
                td = tobj
                call calculate_residuals (t, p, pdzero, pdnew, ires, rpar, ipar)
                idset = 0
            end if
        else
            call update_detectors (update_detector_state,told,dt,n_detectors,idset,ifdtect,tdtect)
        end if

        ! object ignition is the first thing to happen
        if (ifobj>0.and.tobj<=td) then
            fireptr => fireinfo(ifobj)
            call update_fire_objects (set_detector_state,told,dt,ifobj,tobj)
            write(iofilo,'(/,a,i0,3a,i0,a)') 'Object #',ifobj,' (',trim(fireptr%name),') ignited at ', &
                int(max(tobj+0.5_eb,0.0_eb)),' seconds'
            write(*,'(a,i0,3a,i0,a)') 'Object #',ifobj,' (',trim(fireptr%name),') ignited at ', &
                int(max(tobj+0.5_eb,0.0_eb)),' seconds'
            write(logerr,'(a,i0,3a,i0,a)') 'Object #',ifobj,' (',trim(fireptr%name),') ignited at ', &
                int(max(tobj+0.5_eb,0.0_eb)),' seconds'
            ! check to see if we are backing up objects igniting
            if (option(fbtobj)==on) then
                idsave = ifobj
            else
                idsave = idset
                td = tdtect
                fireptr%ignited = .true.
                fireptr%backtrack = .false.
                call set_info_flags(info,rwork)
                ifobj = 0
            end if
        else
            call update_fire_objects (update_detector_state,told,dt,ifobj,tobj)
        end if

        if (idsave/=0) then

            ! a detector has activated so call dassl to integrate backwards
            ! in time to t=td.  this is better than using simple linear interpolation
            ! because in general dassl could be taking very big time steps
            if (told<=td.and.td<t) then
                call output_results (t)
                ipar(2) = some
                tdout = td
                do i = 1, 11
                    info2(i) = 0
                end do
                info2(2) = 1
                told = t
                call ddassl (calculate_residuals,nodes,t,p,pprime,tdout,info2,vrtol,vatol,idid,rwork,lrwork,iwork,liw,rpar,ipar,jac)

                ! make sure dassl is happy (again)
                if (idid<0) then
                    call find_error_component (ipar(3))
                    write (*,'(a,i0)') '***Error, dassl - idid = ', idid
                    write (*,'(a,f10.5,1x,a,f10.5)') '***Error: Problem in DASSL backing from ',t,'to time ',tdout
                    write (logerr,'(a,i0)') '***Error, dassl - idid = ', idid
                    write (logerr,'(a,f10.5,1x,a,f10.5)') '***Error: Problem in DASSL backing from ',t,'to time ',tdout
                    call cfastexit ('CFAST', idid)
                    stop
                end if

                ! reset dassl flags to integrate forward from t=td and
                ! call calculate_residuals to get product info at sprinkler activation time
                if (ifdtect>0) idset = idsave
                dt = t - told
                ipar(2) = all

                ! call calculate_residuals to get product info at the correct time and
                ! to save fire release rates in room where detector has
                ! activated.  (this happens because idset /= 0)
                call calculate_residuals (t, p, pdzero, pdnew, ires, rpar, ipar)
                call update_solution (nodes, nequals, n_species, t, told, p, pold, pdnew, pdold)
                call set_info_flags (info,rwork)
            else if (td==t) then
                call set_info_flags (info,rwork)
                call calculate_residuals (t, p, pdzero, pdnew, ires, rpar, ipar)
            else
                ! update_detectors said that a sprinkler has gone off but the time is wrong!!
                write(*,'(a,f10.5,a,f10.5,a,f10.5)') '***Error: Back step too large in DASSL, Time = ', &
                    t,' Last time = ',told,' need to back step to ',td
                write(logerr,'(a,f10.5,a,f10.5,a,f10.5)') '***Error: Back step too large in DASSL, Time = ', &
                    t,' Last time = ',told,' need to back step to ',td
                call cfastexit ('CFAST', idid)
                stop
            end if
            fireinfo(1:mxfires)%backtrack = .false.
        end if

        ! calculate the mass of objects that have been pyrolized
        ! at the moment we do only the total and the radiological species
        ! It is important to call the routine to integrate the mass before call the toxicology calculation
        call integrate_mass (t, dt)

        ! calculate gas dosage
        call update_species (dt)

        if (option(fdebug)==on) call output_debug (2,t,dt,ieqmax)
        numstep = numstep + 1
        total_steps = total_steps + 1
        if (stopiter>=0.and.total_steps>stopiter) then
            call deleteoutputfiles (stopfile)
            write (logerr,'(a,1pg11.3,a,g11.3)') 'Stopped by request at T = ', t, ' DT = ', dt
            call cfastexit ('CFAST', 0)
        end if
        go to 10
    end if

    return

    end subroutine solve_simulation

! --------------------------- update_solution -------------------------------------------

    subroutine update_solution(nodes, nequals, n_species,  t, told, p, pold, pdnew, pdold)

    !     routine: update_solution
    !     purpose: update solution returned by dassl

    integer, intent(in) :: nodes, nequals, n_species
    real(eb), intent(in) :: t, told, pdnew(*)

    real(eb), intent(inout) :: p(*), pdold(*)
    real(eb), intent(out) :: pold(*)

    integer :: i
    real(eb) :: dt

    dt = t - told

    ! advance species
    do i = nodes + 1, nequals
        p(i) = p(i) + dt*pdold(i)
        p(i) = max (0.0_eb, p(i))
        pdold(i) = pdnew(i)
    end do

    ! advance target temperatures
    call target (1,dt)

    ! make sure species mass adds up to total mass
    if (n_species>0) call synchronize_species_mass (p,nodes+1)

    pold(1:nequals) = p(1:nequals)

    return

    end subroutine update_solution

! --------------------------- keyboard_interaction -------------------------------------------

    subroutine keyboard_interaction (t,icode,tpaws,tout,ieqmax)

    !     routine: keyboard_interaction
    !     purpose: keyboard routine for user interaction during simulation

    integer, intent(in) :: ieqmax
    real(eb), intent(in) :: t

    integer, intent(out) :: icode
    real(eb), intent(out) :: tpaws
    real(eb), intent(inout) :: tout

    integer(2) :: ch, hit
    real(eb) :: rcode

    icode = 0
    call grabky(ch,hit)
    if (hit>0) then
        if (ch==27) then
            icode = 1
            return
        else if (hit>1) then
            if (option(fkeyeval)==on) then
                if (ch==59) then
                    write (*,5010) t, dt
                    if (output_interactive_help ()) icode = 1
                else if (ch==60) then
                    if (option(fdebug)==on) then
                        option(fdebug) = off
                        write (*,*) 'debug is now off'
                        write (*,*)
                    else
                        option(fdebug) = on
                    end if
                else if (ch==62) then
                    call output_debug(1,t,dt,ieqmax)
                else if (ch==63) then
                    write (*,5010) t, dt
                else if (ch==64) then
                    write (*,5010) t, dt
                    write (*,*) 'enter time at which to pause: '
                    read (*,*) rcode
                    tpaws = rcode
                    tout = min(tpaws,tout)
                else if (ch==65) then
                    if (option(fpdassl)==on) then
                        option(fpdassl) = off
                        write (*,*) 'dassl debug is now off'
                    else
                        option(fpdassl) = on
                    end if
                end if
            else
                write (*,5010) t, dt
            end if
        end if
    end if

    return
5010 format (' time = ',1pg12.4,', dt = ',1pg12.4)
    end subroutine keyboard_interaction

! --------------------------- output_interactive_help -------------------------------------------

    logical function output_interactive_help()

    !     Routine: output_interactive_help
    !     Purpose: quick output of keyboard shortcuts available during simulaiton

    integer(2) :: ch, hit
    integer :: ii

    write (iofilo,*) '***Options Set***'
    write (iofilo,'(1x,20i3)') (option(ii),ii = 1,mxopt)
    write (iofilo,*) '************************************************************'
    write (iofilo,*) '1=Help,2=debug,3=flow,4=pause,5=time,6=pause time,7=dassl(t)'
    write (iofilo,*) 'Press <esc> to quit, any other key to continue'
    write (iofilo,*) '************************************************************'

10  call grabky(ch,hit)
    if (hit==0) go to 10
    if (ch==27) then
        output_interactive_help = .true.
        write (iofilo,*) 'Run terminated at user request'
    else
        output_interactive_help = .false.
        write (iofilo,*) 'continuing'
        write (iofilo,*)
    end if
    return

    end function output_interactive_help

! --------------------------- set_info_flags -------------------------------------------

    subroutine set_info_flags (info,rwork)

    !     routine: set_info_flags
    !     purpose: update solution flags for dassl solver

    integer, intent(out) :: info(*)
    real(eb), intent(out) :: rwork(*)

    info(1:11) = 0
    info(3) = 1
    info(2) = 1
    if (stpmax<=0.0_eb) then
        info(7) = 0
    else
        info(7) = 1
        rwork(2) = stpmax
    end if
    if (stpfirst<0.0_eb) then
        info(8) = 0
    else
        info(8) = 1
        rwork(3) = stpfirst
    end if

    ! setting jacobian flag
    info(5) = 0
    info(11) = 1
    return

    end subroutine set_info_flags

! --------------------------- calculate_residuals (resid) -------------------------------------------

    subroutine calculate_residuals (tsec,y_vector,yprime_vector,f_vector,ires,rpar,ipar)


    !     Routine: cfast calculate_residuals
    !     Purpose: Calculates the residual F(t,y,dy/dt) for CFAST
    !              differential and algebraic equations.  For the gas
    !              differential equations (pressure, layer volume,
    !              upper/lower layer temperature) F(t,y,dy/dt) takes
    !              the form F(t,y,dy/dt) = dy/dt - f(t,y) where f(t,y) is
    !              related to the conservation of mass and and energy.
    !              For the wall temperature equations, F is just Fourier's
    !              law taking the form of
    !              F(t,y,dy/dt) = q''(t,y) + K dT/dx
    !              where q'' is the flux striking the wall, K is the wall's
    !              thermal conductivity and dT/dx is the surface wall
    !              temperature gradient.
    !     Revision: $Revision$
    !     Revision Date: $Date$
    !     Arguments: TSEC    Current simulation time (T above in s)
    !                Y_VECTOR Current guess at solution vector (Y above)
    !                YPRIME_VECTOR Current guess at derivative of solution
    !                        vector (Y' above)
    !                F_VECTOR Residual or value of F(t,y,dy/dt)
    !                IRES    Outputs:  IRES    Integer flag which is always equal to
    !                        zero on input. calculate_residuals should alter IRES
    !                        only if it encounters an illegal value of Y or
    !                        a stop condition. Set IRES = -1 if an input
    !                        value is illegal, and DDASSL will try to solve
    !                        the problem without getting IRES = -1. If
    !                        IRES = -2, DASSL return control to the calling
    !                        program with IDID = -11.
    !                RPAR    real parameter arrays
    !                IPAR    integer parameter arrays
    !                        These are used for communication between solve_simulation and
    !                        calculate_residuals via DASSL. They are not altered by DASSL.
    !                        Currently, only IPAR is used in calculate_residuals to pass
    !                        a partial/total flag for solution of the
    !                        species equations.

    real(eb), intent(in) :: tsec, y_vector(*), yprime_vector(*), rpar(*)
    integer, intent(in) :: ipar(*)

    integer, intent(inout) :: ires
    real(eb), intent(out) :: f_vector(*)

    integer, parameter :: all = 1, some = 0

    ! data structures for dassl, the numerical solver
    real(eb) :: yhatprime_vector(maxteq)

    ! data structures for rooms
    type(room_type), pointer :: roomptr

    ! data structure for total flows and fluxes
    real(eb) :: flwtot(mxrooms,mxfprd+2,2), flxtot(mxrooms,nwal)

    ! data structures for flow through vents
    real(eb) :: flwnvnt(mxrooms,mxfprd+2,2)
    real(eb) :: flwhvnt(mxrooms,ns+2,2)

    ! data structures for fires
    real(eb) :: flwf(mxrooms,ns+2,2)

    ! data structures for convection, radiation, and ceiling jets
    real(eb) :: flwcv(mxrooms,2), flxcv(mxrooms,nwal)
    real(eb) :: flwrad(mxrooms,2), flxrad(mxrooms,nwal)

    ! data structures for mechanical vents
    real(eb) :: flwmv(mxrooms,ns+2,2), filtered(mxrooms,ns+2,2)

    ! data structures for door jet fires
    real(eb) :: flwdjf(mxrooms,ns+2,2)
    integer :: update

    logical :: vflowflg, hvacflg, djetflg
    integer :: nprod, i, iroom, iprod, ip, iwall, nprodsv, iprodu, iprodl
    real(eb) :: epsp, aroom, hceil, pabs, hinter, ql, qu, tmu, tml
    real(eb) :: oxydu, oxydl, pdot, tlaydu, tlaydl, vlayd, prodl, produ

    ires = ires ! just to get rid of a warning message
    nprod = n_species
    dt = tsec - told
    numresd = numresd + 1
    stime = tsec

    call update_data (y_vector,odevara)
    call update_data (y_vector,odevarb)

    ! If calculate_residuals is called by solve_simulation then IPAR(2)==ALL all residuals
    ! are computed.  If calculate_residuals is called by DASSL residuals are not
    ! computed for species.  Further, temperature profiles are only
    ! updated when calculate_residuals is called by solve_simulation.

    if (ipar(2)==some) then
        update = 0
        prnslab = .false.
    else
        update = 1
        if (residprn) then
            prnslab = .true.
        else
            prnslab = .false.
        end if
        dbtime = tsec
    end if

    epsp = rpar(1)

    ! calculate flow due to unforced vents (horizontal_flow for doors/windows
    ! and vertical_flow for ceiling/floor vents
    call horizontal_flow (tsec,epsp,nprod,flwnvnt)
    call vertical_flow (tsec,flwhvnt,vflowflg)
    call mechanical_flow (tsec,y_vector(nofpmv+1),y_vector(noftmv+1),yprime_vector(noftmv+1),flwmv,&
        f_vector(nofpmv+1),f_vector(noftmv+1),&
        yhatprime_vector(nofhvpr+1),nprod,hvacflg,filtered)

    ! calculate heat and mass flows due to fires
    call fire (tsec,flwf)
    call door_jet (flwdjf,djetflg)

    ! calculate flow and flux due to heat transfer (ceiling jets, convection and radiation)
    call convection (flwcv,flxcv)
    call radiation (flwrad,flxrad)

    ! sum flow for inside rooms
    do iroom = 1, nrm1
        roomptr => roominfo(iroom)

        do iprod = 1, nprod + 2
            ip = i_speciesmap(iprod)
            flwtot(iroom,iprod,l) = flwnvnt(iroom,iprod,l) + flwf(iroom,ip,l)
            flwtot(iroom,iprod,u) = flwnvnt(iroom,iprod,u) + flwf(iroom,ip,u)
        end do
        if (vflowflg) then
            do iprod = 1, nprod + 2
                ip = i_speciesmap(iprod)
                flwtot(iroom,iprod,l) = flwtot(iroom,iprod,l) + flwhvnt(iroom,ip,l)
                flwtot(iroom,iprod,u) = flwtot(iroom,iprod,u) + flwhvnt(iroom,ip,u)
            end do
        end if
        if (hvacflg) then
            do iprod = 1, nprod + 2
                ip = i_speciesmap(iprod)
                flwtot(iroom,iprod,l) = flwtot(iroom,iprod,l) + flwmv(iroom,ip,l) - filtered(iroom,iprod,l)
                flwtot(iroom,iprod,u) = flwtot(iroom,iprod,u) + flwmv(iroom,ip,u) - filtered(iroom,iprod,u)
            end do
        end if
        if (djetflg) then
            do iprod = 1, nprod + 2
                ip = i_speciesmap(iprod)
                flwtot(iroom,iprod,l) = flwtot(iroom,iprod,l) + flwdjf(iroom,ip,l)
                flwtot(iroom,iprod,u) = flwtot(iroom,iprod,u) + flwdjf(iroom,ip,u)
            end do
        end if

        flwtot(iroom,q,l) = flwtot(iroom,q,l) + flwcv(iroom,l) + flwrad(iroom,l)
        flwtot(iroom,q,u) = flwtot(iroom,q,u) + flwcv(iroom,u) + flwrad(iroom,u)


        ! if this room is a shaft then solve for only one zone.
        ! this is done by combining flows from to both
        ! layers into upper layer flow and setting lower layer flow to
        ! zero.
        if (roomptr%shaft) then
            do iprod = 1, nprod + 2
                flwtot(iroom,iprod,u) = flwtot(iroom,iprod,u) + flwtot(iroom,iprod,l)
                flwtot(iroom,iprod,l) = 0.0_eb
            end do
        end if
    end do

    if (update==all) then
        if (residprn) then
            call output_spreadsheet_residuals (tsec, flwtot, flwnvnt, flwf, flwhvnt, flwmv, filtered, flwdjf, flwcv, flwrad)
        end if
    end if
    ! sum flux for inside rooms
    do iroom = 1, nrm1
        roomptr => roominfo(iroom)
        do iwall = 1, nwal
            if (roomptr%surface_on(iwall)) then
                flxtot(iroom,iwall) = flxcv(iroom,iwall) + flxrad(iroom,iwall)
            end if
        end do
    end do

    ! set nprod to zero when we are only solving "some" of the ode's
    if (ipar(2)==some) then
        nprodsv = nprod
        nprod = 0
    end if

    ! calculate rhs of ode's for each room
    do iroom = 1, nrm1
        roomptr => roominfo(iroom)
        aroom = roomptr%floor_area
        hceil = roomptr%cheight
        pabs = roomptr%absp
        hinter = roomptr%depth(l)
        ql = flwtot(iroom,q,l)
        qu = flwtot(iroom,q,u)
        tmu = flwtot(iroom,m,u)
        tml = flwtot(iroom,m,l)

        if (option(foxygen)==on) then
            oxydu = flwtot(iroom,4,u)
            oxydl = flwtot(iroom,4,l)
        end if

        ! pressure equation
        if (roomptr%deadroom.eq.0) then
            pdot = (gamma-1.0_eb)*(ql + qu)/(aroom*hceil)
        else
            pdot = 0.0_eb
        end if

        ! upper layer temperature equation
        tlaydu = (qu-cp*tmu*roomptr%temp(u))/(cp*roomptr%mass(u))
        if (option(fode)==on) then
            tlaydu = tlaydu + pdot/(cp*roomptr%rho(u))
        end if

        ! upper layer volume equation
        vlayd = (gamma-1.0_eb)*qu/(gamma*pabs)
        if (option(fode)==on) then
            vlayd = vlayd - roomptr%volume(u)*pdot/(gamma*pabs)
        end if
        if (roomptr%shaft) vlayd = 0.0_eb

        ! lower layer temperature equation
        tlaydl = (ql-cp*tml*roomptr%temp(l))/(cp*roomptr%mass(l))
        if (option(fode)==on) then
            tlaydl = tlaydl + pdot/(cp*roomptr%rho(l))
        end if

        yhatprime_vector(iroom) = pdot
        yhatprime_vector(iroom+noftl) = tlaydl
        yhatprime_vector(iroom+nofvu) = vlayd
        yhatprime_vector(iroom+noftu) = tlaydu

        if (option(foxygen)==on) then
            yhatprime_vector(iroom+nofoxyu) = oxydu
            yhatprime_vector(iroom+nofoxyl) = oxydl
        end if
    end do

    ! compute product of combustion terms
    if (nprod>0.and.ipar(2)==all) then
        iprodu = nofprd - 1
        do iprod = 1, nprod
            do iroom = 1, nrm1
                roomptr => roominfo(iroom)
                hceil = roomptr%cheight
                hinter = roomptr%depth(l)
                iprodu = iprodu + 2
                iprodl = iprodu + 1
                prodl = flwtot(iroom,iprod+2,l)

                ! if this room is a hall and the jet has not reached the end
                ! of the hall then don't solve for it using dassl
                produ = flwtot(iroom,iprod+2,u)

                if (hinter<hceil) then
                    yhatprime_vector(iprodu) = produ
                else if (hinter>=hceil.and.flwtot(iroom,m,u)<0.0_eb)  then
                    yhatprime_vector(iprodu) = produ
                else
                    yhatprime_vector(iprodu) = 0.0_eb
                end if
                if (hinter>0.0_eb) then
                    yhatprime_vector(iprodl) = prodl
                else if (hinter<=0.0_eb.and.flwtot(iroom,m,l)>0.0_eb) then
                    yhatprime_vector(iprodl) = prodl
                else
                    yhatprime_vector(iprodl) = 0.0_eb
                end if
            end do
        end do
    end if

    ! residuals for pressure
    do i = nofp + 1, nofp + nrm1
        f_vector(i) = yhatprime_vector(i) - yprime_vector(i)
    end do

    ! residuals for layer volume, and layer temperatures
    do i = noftu + 1, noftu + 3*nrm1
        f_vector(i) = yhatprime_vector(i) - yprime_vector(i)
    end do

    ! residual for oxygen
    if (option(foxygen)==on) then
        do i = 1, nrm1
            f_vector(i+nofoxyu) = yhatprime_vector(i+nofoxyu) - yprime_vector(i+nofoxyu)
            f_vector(i+nofoxyl) = yhatprime_vector(i+nofoxyl) - yprime_vector(i+nofoxyl)
        end do
    end if

    ! conduction residual
    call conduction (update,dt,flxtot,f_vector)

    ! residuals for stuff that is solved in solve_simulation itself, and not by dassl
    if (nprod/=0) then

        ! residuals for gas layer species
        do i = nofprd + 1, nofprd + 2*nprod*nrm1
            f_vector(i) = yhatprime_vector(i) - yprime_vector(i)
        end do

        ! residual for hvac species
        do i = nofhvpr+1, nofhvpr+n_species*nhvsys
            f_vector(i) = yhatprime_vector(i) - yprime_vector(i)
        end do
    end if

    if (ipar(2)==some) nprod = nprodsv

    return

    end subroutine calculate_residuals

! --------------------------- update_data -------------------------------------------

    subroutine update_data (y_vector,iflag)

    !     routine: update_data (datacopy)
    !     purpose: calculate environment variables from the solver vector

    !     arguments: y_vector   solver vector
    !                iflag  action flag:
    !     iflag = constvar ==> constant data (data that does not change
    !                          with time)
    !     iflag = odevara  ==> ode variables: pressure, temperature and upper
    !                          layer volume
    !     iflag = odevarb  ==> species data and wall temperature profile.
    !                          use yvector_old and pdold to estimate species
    !     iflag = odevarc  ==> species data and wall temperature profile.
    !                          use pdif array for species

    integer, intent(in) :: iflag
    real(eb), intent(in) :: y_vector(*)


    integer :: iroom, lsp, layer, i, j, itstop, ieq, iwall, ii
    integer :: iwfar, ifromr, ifromw, itor, itow, ieqfrom, ieqto, itarg
    integer :: npts, iwalleq, iwalleq2, iinode, ilay, isys, isof
    real(eb) :: wtemp
    real(eb) :: xdelt, tstop, zzu, zzl
    real(eb) :: zlay, ztarg, ppgas, totl, totu, rtotl, rtotu, oxyl, oxyu, pphv
    real(eb) :: xt, xtemp, xh2o, ptemp, epscut
    real(eb) :: xmax, xmid, ymax, ymid, zmax

    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr, deadroomptr
    type(target_type), pointer :: targptr

    if (nfurn>0.and.iflag/=constvar) then
        call interp(furn_time,furn_temp,nfurn,stime,1,wtemp)
        wtemp = wtemp + kelvin_c_offset
        qfurnout=sigma*wtemp**4
    end if

    if (iflag==constvar) then
        do iroom = 1, nr
            roomptr => roominfo(iroom)
            roomptr%vmin = min(vminfrac*roomptr%cvolume, 1.0_eb)
            roomptr%vmax = roomptr%cvolume - roomptr%vmin
        end do
        do iroom = 1, nrm1
            roomptr=>roominfo(iroom)

            roomptr%x1 = roomptr%x0 + roomptr%cwidth
            roomptr%y1 = roomptr%y0 + roomptr%cdepth
            roomptr%z1 = roomptr%z0 + roomptr%cheight

            ! define wall centers
            xmax = roomptr%cwidth
            xmid = xmax/2.0_eb
            ymax = roomptr%cdepth
            ymid = ymax/2.0_eb
            zmax = roomptr%z1

            ! ceiling
            roomptr%wall_center(1,1) = xmid
            roomptr%wall_center(2,1) = ymid
            roomptr%wall_center(3,1) = zmax

            ! upper back
            roomptr%wall_center(1,2) = xmid
            roomptr%wall_center(2,2) = ymax

            ! upper right
            roomptr%wall_center(1,3) = xmax
            roomptr%wall_center(2,3) = ymid

            ! upper front
            roomptr%wall_center(1,4) = xmid
            roomptr%wall_center(2,4) = 0.0_eb

            ! upper left
            roomptr%wall_center(1,5) = 0.0_eb
            roomptr%wall_center(2,5) = ymid

            ! lower back
            roomptr%wall_center(1,6) = xmid
            roomptr%wall_center(2,6) = ymax

            ! lower right
            roomptr%wall_center(1,7) = xmax
            roomptr%wall_center(2,7) = ymid

            ! lower front
            roomptr%wall_center(1,8) = xmid
            roomptr%wall_center(2,8) = 0.0_eb

            ! lower left
            roomptr%wall_center(1,9) = 0.0_eb
            roomptr%wall_center(2,9) = ymid

            ! floor
            roomptr%wall_center(1,10) = xmid
            roomptr%wall_center(2,10) = ymid
            roomptr%wall_center(3,10) = 0.0_eb
        end do

        roomptr=>roominfo(nr)

        roomptr%z0 = 0.0_eb
        roomptr%z1 = 100000.0_eb

        roomptr%volume(u) = 0.0_eb
        roomptr%volume(l) = 100000.0_eb
        roomptr%depth(u) = 0.0_eb
        roomptr%depth(l) = 100000.0_eb
        roomptr%relp = 0.0_eb
        roomptr%absp = pressure_offset
        roomptr%temp(u) = exterior_temperature
        roomptr%temp(l) = exterior_temperature
        roomptr%species_fraction(u,3:ns) = 0.0_eb
        roomptr%species_fraction(l,3:ns) = 0.0_eb
        roomptr%species_mass(l,3:ns) = 0.0_eb
        roomptr%species_mass(u,3:ns) = 0.0_eb
        roomptr%species_fraction(u,n2) = 0.770_eb
        roomptr%species_fraction(l,n2) = 0.770_eb
        roomptr%species_fraction(u,o2) = 0.230_eb
        roomptr%species_fraction(l,o2) = 0.230_eb

        ! set the water content to relative_humidity - the polynomial fit is to (t-273), and
        ! is for saturation pressure of water.  this fit comes from the steam
        ! tables in the handbook of physics and chemistry.
        xt = exterior_temperature
        xtemp = 23.2_eb - 3.816e3_eb/(xt-46.0_eb)
        xh2o = exp(xtemp)/101325.0_eb*(18.016_eb/28.584_eb)
        roomptr%species_fraction(u,h2o) = relative_humidity*xh2o
        roomptr%species_fraction(l,h2o) = relative_humidity*xh2o

        roomptr%rho(u:l) = roomptr%absp/rgas/roomptr%temp(u:l)
        roomptr%mass(u:l) = roomptr%rho(u:l)*roomptr%volume(u:l)

        !define vents for vertical flow
        n_vvents = 0
        do i = 1, nr
            do j = 1, nr
                if (ivvent_connections(i,j)/=0) then
                    n_vvents = n_vvents + 1
                    ivvent(n_vvents,1) = i
                    ivvent(n_vvents,2) = j
                    qcvv(1,n_vvents) = qcvpp(1,i,j)
                    qcvv(2,n_vvents) = qcvpp(2,i,j)
                    qcvv(3,n_vvents) = qcvpp(3,i,j)
                    qcvv(4,n_vvents) = qcvpp(4,i,j)

                    ventptr => vventinfo(n_vvents)
                    ventptr%top = ivvent(n_vvents,1)
                    ventptr%bottom = ivvent(n_vvents,2)
                end if
            end do
        end do

        ! define discontinuity array.  first we look at vent openings

        xdelt = time_end/deltat
        itstop = xdelt + 1
        tstop = itstop - 1

        discon(0) = 0.0_eb
        discon(1) = tstop
        ndisc = 1

        ! add each of the change arrays to the discontinuity list
        do  i = 1, n_hvents
            ventptr => hventinfo(i)
            if (ventptr%initial_open_time/=0.0_eb) then
                ndisc = ndisc + 1
                discon(ndisc) = ventptr%initial_open_time
            end if
            if (ventptr%final_open_time/=0.0_eb) then
                ndisc = ndisc + 1
                discon(ndisc) = ventptr%final_open_time
            end if
        end do
        do  i = 1, n_vvents
            ndisc = ndisc + 1
            discon(ndisc) = qcvv(1,i)
            ndisc = ndisc + 1
            discon(ndisc) = qcvv(3,i)
        end do
        do  i = 1, nfan
            ndisc = ndisc + 1
            discon(ndisc) = qcvm(1,i)
            ndisc = ndisc + 1
            discon(ndisc) = qcvm(3,i)
        end do
        do i = 1, nfilter
            ndisc = ndisc + 1
            discon(ndisc) = qcvf(1,i)
            ndisc = ndisc + 1
            discon(ndisc) = qcvf(3,i)
        end do

        ! put the discontinuity array into order
        call shellsort (discon(0), ndisc+1)

        ! define i_wallmap for jac and other constants for dassl and the conduction routine
        ieq = nofwt
        i_wallmap(nr,1:4) = 0
        do iroom = 1, nrm1
            roomptr => roominfo(iroom)
            do iwall = 1, 4
                if (roomptr%surface_on(iwall)) then
                    ieq = ieq + 1
                    i_wallmap(iroom,iwall) = ieq

                    ! define i_hconnections, to describe ceiling-floor connections
                    ! first assume that walls are connected to the outside
                    ii = ieq - nofwt
                    i_hconnections(ii,w_from_room) = iroom
                    i_hconnections(ii,w_from_wall) = iwall
                    i_hconnections(ii,w_to_room) = nrm1 + 1
                    if (iwall==1.or.iwall==2) then
                        iwfar = 3 - iwall
                    else
                        iwfar = iwall
                    end if
                    i_hconnections(ii,w_to_wall) = iwfar
                    i_hconnections(ii,w_boundary_condition) = iwbound

                else
                    i_wallmap(iroom,iwall) = 0
                end if
            end do
        end do

        ! update i_hconnections for ceiling/floors that are connected
        do i = 1, nvcons
            ifromr = i_vconnections(i,w_from_room)
            ifromw = i_vconnections(i,w_from_wall)
            itor = i_vconnections(i,w_to_room)
            itow = i_vconnections(i,w_to_wall)
            ieqfrom = i_wallmap(ifromr,ifromw) - nofwt
            ieqto = i_wallmap(itor,itow) - nofwt

            i_hconnections(ieqfrom,w_to_room) = itor
            i_hconnections(ieqfrom,w_to_wall) = itow
            i_hconnections(ieqfrom,w_boundary_condition) = 1

            i_hconnections(ieqto,w_to_room) = ifromr
            i_hconnections(ieqto,w_to_wall) = ifromw
            i_hconnections(ieqto,w_boundary_condition) = 1
        end do

        jacn1 = nofpmv - nofp
        jacn2 = nofwt - nofpmv
        jacn3 = nofprd - nofwt
        jacdim = jacn1 + jacn2 + jacn3

        ! indicate which rooms are connected to an hvac system
        roominfo(1:nrm1)%is_hvac = .false.
        do ii = 1, next
            i = hvnode(1,ii)
            roomptr => roominfo(i)
            roomptr%is_hvac = .true.
        end do

    else if (iflag==odevara) then
        do iroom = 1, nrm1
            roomptr=>roominfo(iroom)

            roomptr%volume(u) = max(y_vector(iroom+nofvu),roomptr%vmin)
            roomptr%volume(u) = min(roomptr%volume(u),roomptr%vmax)
            roomptr%volume(l) = max(roomptr%cvolume-roomptr%volume(u),roomptr%vmin)
            roomptr%volume(l) = min(roomptr%volume(l),roomptr%vmax)

            ! calculate layer height for non-rectangular rooms
            npts = roomptr%nvars
            if (npts==0) then
                roomptr%depth(u) = roomptr%volume(u)/roomptr%floor_area
                roomptr%depth(l) = roomptr%volume(l)/roomptr%floor_area
            else
                call interp(roomptr%var_volume,roomptr%var_height,npts,roomptr%volume(l),1,roomptr%depth(l))
                roomptr%depth(u) = roomptr%cheight - roomptr%depth(l)
            end if

            roomptr%relp = y_vector(iroom)
            roomptr%absp = y_vector(iroom) + pressure_offset
            if (nfurn>0) then
              roomptr%temp(u) = wtemp
              roomptr%temp(l) = wtemp
            else
              roomptr%temp(u) = y_vector(iroom+noftu)
              roomptr%temp(l) = y_vector(iroom+noftl)
            end if

            ! there is a problem with how flow is being withdrawn from layers
            ! when the layers are small and the flow is large (for example with
            ! ceiling vents.  as a result, dassl, can predict a negative temperature
            ! (because the rhs of the temperature equation is wrong).  the following
            ! code causes the temperature of the opposite layer to be used in these
            ! situations.
            if (roomptr%temp(u)<0.0_eb) then
                roomptr%temp(u)=roomptr%temp(l)
            end if
            if (roomptr%temp(l)<0.0_eb) then
                roomptr%temp(l)=roomptr%temp(u)
            end if
            if (roomptr%shaft) then
                roomptr%temp(l) = roomptr%temp(u)
            end if

            ! compute area of 10 wall segments
            xmax = roomptr%cwidth
            ymax = roomptr%cdepth
            zzu = roomptr%depth(u)
            zzl = roomptr%depth(l)
            roomptr%wall_area10(1) = roomptr%floor_area
            roomptr%wall_area10(2) = zzu*xmax
            roomptr%wall_area10(3) = zzu*ymax
            roomptr%wall_area10(4) = zzu*xmax
            roomptr%wall_area10(5) = zzu*ymax
            roomptr%wall_area10(6) = zzl*xmax
            roomptr%wall_area10(7) = zzl*ymax
            roomptr%wall_area10(8) = zzl*xmax
            roomptr%wall_area10(9) = zzl*ymax
            roomptr%wall_area10(10) = roomptr%floor_area

            ! compute area of 4 wall segments
            roomptr%wall_area4(1) = roomptr%floor_area
            roomptr%wall_area4(2) = roomptr%floor_area
            roomptr%wall_area4(3) = (ymax + xmax)*zzu*2.0_eb
            roomptr%wall_area4(4) = max(0.0_eb,(ymax+xmax)*zzl*2.0_eb)

            ! define z wall centers (the z coordinate changes with time)
            ! (other coordinates are static and are defined earlier)

            do i = 1, 4
                zlay = roomptr%depth(l)
                roomptr%wall_center(3,i+1) =  (roomptr%z1+zlay)/2.0_eb
                roomptr%wall_center(3,i+5) = zlay/2.0_eb
            end do

            ! Eliminate very small noise in the pressure equation. This was added to correct
            ! phantom flows with very small pressure differences. This is the same algorithm
            ! used in hrozontal and vertical flow
            epscut = 1.0e-5_eb*max(1.0_eb,abs(roomptr%relp))
            ! test for underflow
            if (abs(roomptr%relp/epscut)<=130.0_eb) then
                ptemp = roomptr%relp*(1.0_eb - exp(-abs(roomptr%relp/epscut))) + pressure_offset
            else
                ptemp = roomptr%absp
            end if

            do layer = u, l
                roomptr%rho(layer) = ptemp/rgas/roomptr%temp(layer)
                roomptr%mass(layer) = roomptr%rho(layer)*roomptr%volume(layer)
            end do
        end do

        do i = 1, nrm1
            roomptr => roominfo(i)
            if (roomptr%deadroom.eq.0) cycle
            deadroomptr => roominfo(roomptr%deadroom)
            roomptr%relp = deadroomptr%relp
            roomptr%absp = deadroomptr%absp
        end do

        ! record which layer target is in
        do itarg = 1, n_targets
            targptr => targetinfo(itarg)
            iroom = targptr%room
            roomptr => roominfo(iroom)
            zlay = roomptr%depth(l)
            ztarg = targptr%center(3)
            if (ztarg>=zlay) then
                targptr%layer = u
            else
                targptr%layer = l
            end if
        end do

        ! define surface wall temperatures (interior=1,exterior=2)
    else if (iflag==odevarb.or.iflag==odevarc) then
        isof = nofwt
        do iroom = 1, nrm1
            roomptr => roominfo(iroom)
            do iwall = 1, nwal
                iwalleq = i_wallmap(iroom,iwall)
                if (iwalleq/=0) then
                    if (nfurn.gt.0) then
                        roomptr%t_surfaces(1,iwall) = wtemp
                        roomptr%t_surfaces(2,iwall) = wtemp
                    else
                        roomptr%t_surfaces(1,iwall) = y_vector(iwalleq)
                        ieqfrom = iwalleq - nofwt
                        itor = i_hconnections(ieqfrom,w_to_room)
                        itow = i_hconnections(ieqfrom,w_to_wall)
                        iwalleq2 = i_wallmap(itor,itow)
                        if (iwalleq2==0) then
                            iinode = roomptr%nodes_w(1,iwall)
                            roomptr%t_surfaces(2,iwall) = roomptr%t_profile(iinode,iwall)
                        else
                            roomptr%t_surfaces(2,iwall) = y_vector(iwalleq2)
                        end if
                    end if
                else

                    ! if we're not solving for the wall temperature then set it
                    ! to the layer temperature that it is adjacent too.  note,
                    ! ...%t_surfaces(2,iwall) is only referenced if the iwall'th
                    ! wall in room iroom is being solved with the heat equation
                    if (iwall==1.or.iwall==3) then
                        ilay = u
                    else
                        ilay = l
                    end if
                    if (nfurn.gt.0) then
                        roomptr%t_surfaces(1,iwall) = wtemp
                    else
                        roomptr%t_surfaces(1,iwall) = roomptr%temp(ilay)
                    end if
                end if
            end do
        end do

        ! define species masses
        isof = nofprd
        do lsp = 1, ns
            do iroom = 1, nrm1
                roomptr => roominfo(iroom)
                isof = isof + 1
                if (iflag==odevarb) then
                    ppgas = pold(isof) + dt*pdold(isof)
                else
                    ppgas = y_vector(isof)
                end if
                roomptr%species_mass(u,lsp) = max(ppgas,0.0_eb)
                isof = isof + 1
                if (iflag==odevarb) then
                    ppgas = pold(isof) + dt*pdold(isof)
                else
                    ppgas = y_vector(isof)
                end if
                roomptr%species_mass(l,lsp) = max(ppgas,0.0_eb)
            end do
        end do

        ! define species mass fractions: normalize to total product mass
        ! rather than total mass (this is equivalent to what was being done
        ! in chemistry)
        do iroom = 1, nrm1
            roomptr => roominfo(iroom)
            totl = 0.0_eb
            totu = 0.0_eb
            do lsp = 1, min(9,ns)
                totu = totu + roomptr%species_mass(u,lsp)
                totl = totl + roomptr%species_mass(l,lsp)
            end do
            rtotl = 1.0_eb
            rtotu = 1.0_eb
            if (totl>0.0_eb) rtotl = 1.0_eb/totl
            if (totu>0.0_eb) rtotu = 1.0_eb/totu
            do lsp = 1, ns
                roomptr%species_fraction(u,lsp) = roomptr%species_mass(u,lsp)*rtotu
                roomptr%species_fraction(l,lsp) = roomptr%species_mass(l,lsp)*rtotl
                if (roomptr%shaft) roomptr%species_fraction(l,lsp) = roomptr%species_fraction(u,lsp)
            end do

            ! if oxygen is a dassl variable then use dassl solution array to define
            ! ...%species_mass and ...%species_fraction values for oxygen.
            ! make sure oxygen never goes negative
            if (option(foxygen)==on) then
                oxyl = max(p(iroom+nofoxyl),0.0_eb)
                oxyu = max(p(iroom+nofoxyu),0.0_eb)
                roomptr%species_mass(l,o2) = oxyl
                roomptr%species_mass(u,o2) = oxyu
                roomptr%species_fraction(l,o2) = oxyl/roomptr%mass(l)
                roomptr%species_fraction(u,o2) = oxyu/roomptr%mass(u)
                if (roomptr%shaft) roomptr%species_fraction(l,o2) = roomptr%species_fraction(u,2)
            end if
        end do
    end if

    ! copy hvac product values for each hvac system
    if (nhvsys/=0.and.ns/=0) then
        isof = nofhvpr
        zzhvm(1:nhvsys) = 0.0_eb

        do lsp = 1, ns
            do isys = 1, nhvsys
                isof = isof + 1
                if (iflag==odevarb) then
                    pphv = max(0.0_eb,pold(isof)+dt*pdold(isof))
                else
                    pphv = max(0.0_eb,y_vector(isof))
                end if
                zzhvspec(isys,lsp) = pphv
                zzhvm(isys) = zzhvm(isys) + zzhvspec(isys,lsp)
            end do
        end do
    end if
    return

    end subroutine update_data

! --------------------------- synchronize_species_mass -------------------------------------------

    subroutine synchronize_species_mass (pdif,ibeg)

    !     routine: synchronize_species_mass
    !     purpose: resyncronize the total mass of the
    !              species with that of the total mass to insure overall and individual mass balance

    !     arguments: pdif   the p array to synchronize_species_mass
    !                ibeg   the point at which species are started in p array

    integer, intent(in) :: ibeg
    real(eb), intent(out) :: pdif(*)

    real(eb) :: factor(mxrooms,2)
    integer :: iroom, isof, iprod
    type(room_type), pointer :: roomptr

    factor(1:nrm1,u) = 0.0_eb
    factor(1:nrm1,l) = 0.0_eb

    isof = ibeg
    do iprod = 1, min(ns,9)
        do iroom = 1, nrm1
            factor(iroom,u) = factor(iroom,u) + pdif(isof)
            isof = isof + 1
            factor(iroom,l) = factor(iroom,l) + pdif(isof)
            isof = isof + 1
        end do
    end do

    do iroom = 1, nrm1
        roomptr => roominfo(iroom)
        if (factor(iroom,u)>0.0_eb.and.roomptr%mass(u)>0.0_eb) then
            factor(iroom,u) = roomptr%mass(u)/factor(iroom,u)
        else
            factor(iroom,u) = 1.0_eb
        end if
        if (factor(iroom,l)>0.0_eb.and.roomptr%mass(l)>0.0_eb) then
            factor(iroom,l) = roomptr%mass(l)/factor(iroom,l)
        else
            factor(iroom,l) = 1.0_eb
        end if
    end do

    isof = ibeg
    do iprod = 1, min(ns,9)
        do iroom = 1, nrm1
            pdif(isof) = pdif(isof)*factor(iroom,u)
            isof = isof + 1
            pdif(isof) = pdif(isof)*factor(iroom,l)
            isof = isof + 1
        end do
    end do

    return

    end subroutine synchronize_species_mass

end module solve_routines