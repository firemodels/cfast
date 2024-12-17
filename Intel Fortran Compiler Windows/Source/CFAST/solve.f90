module solve_routines

    use precision_parameters

    use ws2_32
    use conduction_routines, only: conduction
    use convection_routines, only: convection
    use debug_routines, only: output_spreadsheet_residuals
    use exit_routines, only: cfastexit, delete_output_files
    use fire_routines, only: fire, vent_jets, integrate_mass, update_species, collect_fire_data_for_smokeview, update_fire_ignition
    use isosurface, only: output_isodata
    use hflow_routines, only: wall_flow, leakage_flow
    use mflow_routines, only: mechanical_flow
    use numerics_routines, only : ddassl, jac, setderv, snsqe, gjac
    use opening_fractions, only : get_vent_opening
    use output_routines, only: output_results, output_status, output_debug, write_error_component
    use radiation_routines, only: radiation
    use smokeview_routines, only: output_smokeview, output_smokeview_header, output_smokeview_plot_data, output_slicedata
    use spreadsheet_routines, only: output_spreadsheet, output_spreadsheet_smokeview
    use target_routines, only: target, update_detectors, get_detector_temp_and_velocity
    use utility_routines, only: mat2mult, interp, shellsort, cptime, get_filenumber
    use vflow_routines, only: vertical_flow
    use compartment_routines, only: layer_mixing, synchronize_species_mass, room_connections, wall_opening_fraction

    use cfast_types, only: fire_type, ramp_type, room_type, target_type, vent_type
    
    use cenviro, only: odevara, odevarb, odevarc, constvar, cp, rgas, gamma
    use cparams, only: u, l, m, q, mxrooms, mxtarg, mxnode, mxbranch, mxdiscon, maxeq, ns, check_state, set_state, update_state, &
        nwal, ns_mass, vminfrac, n2, o2, co2, co, h2o, w_from_room, w_to_room, w_from_wall, w_to_wall, w_boundary_condition, &
        radiation_fix
    
    use devc_data, only: n_detectors, n_targets, targetinfo, idset
    use diag_data, only: radi_verification_flag, verification_time_step, upper_layer_thickness, dbtime, gas_temperature, &
        partial_pressure_co2, partial_pressure_h2o, residfile, ioresid, residcsv, residfirst, residprn, ioslab, slabcsv, prnslab
    use fire_data, only: n_fires, fireinfo, n_furn, furn_time, furn_temp, qfurnout
    use option_data, only: option, mxopt, on, off, iprtalg, ovtime, tovtime, tottime, prttime, numjac, numstep, numresd, fpdassl, &
        stptime, total_steps, fpsteady, foxygen, fdebug, fresidprn, fkeyeval
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: n_rooms, roominfo, n_cons, surface_connections, n_vcons, vertical_connections, &
        exterior_ambient_temperature, exterior_abs_pressure, pressure_ref, pressure_offset, relative_humidity, iwbound
    use setup_data, only: iofilo, iofill, initializeonly, stime, i_time_step, time_end, deltat, print_out_interval, &
        smv_out_interval, ss_out_interval, nokbd, stopfile, queryfile, cfast_version, errormessage, datapath
    use smkview_data, only: smv_room, smv_xfire, smv_yfire, smv_zfire, smv_relp, smv_zlay, smv_tu, smv_tl, smv_qdot, smv_height
    use solver_data, only: maxteq, rpar2, ipar2, p, pold, pdold, pinit, told, dt, aptol, atol, rtol, rptol, awtol, rwtol, algtol, &
        nofp, nequals, nofprd, nofwt, noftu, noftl, nofvu, nofoxyu, nofoxyl, ndisc, discon, stpmin, stpminflag, stpmin_cnt, &
        stpmin_cnt_max, stpmax, stpfirst, jacdim, i_speciesmap, I_wallmap, stp_cnt_max
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo

    implicit none
    external grabky

    private

    public solve_simulation, calculate_residuals, output_interactive_help, update_data

    contains

! --------------------------- initial_solution -------------------------------------------
    
    !SECTION runtime ventptr change subroutines
    SUBROUTINE GetConnection(connection)

        INTEGER WINSOCK_V2_2

        INTEGER SUCCESS

        PARAMETER(WINSOCK_V2_2 = X'202', SUCCESS = 0)

        INTEGER connection

        TYPE(T_SOCKADDR_IN) connectionInfo

        TYPE(T_WSADATA) wsaInfo

        CHARACTER*15 host
        
        CHARACTER(len=100) :: cfastSocketPortFilename
        
        CHARACTER(len=400) :: cfastSocketPortPath
        
        CHARACTER(len=100) line
        
        INTEGER :: ios
          
        INTEGER port
        

        INTEGER status
        cfastSocketPortFilename = 'cfast_evac_socket_port.txt'

        cfastSocketPortPath = trim(datapath) // trim(cfastSocketPortFilename)
        ! Initialize Winsock v2.
        status = WSAStartup(WINSOCK_V2_2, wsaInfo)

        connection = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        IF (connection .EQ. INVALID_SOCKET) THEN

            status = WSACleanup()

            RETURN
        ENDIF

        host = '127.0.0.1'
        open(unit=10, file=trim(cfastSocketPortPath), status='old', action='read')
        read(10, '(A100)', iostat=ios) line
        read(line, *, iostat=ios) port
        close(10)
        connectionInfo%sin_family = AF_INET
        connectionInfo%sin_port = htons(port)
        connectionInfo%sin_addr%s_addr = inet_addr(host(1:LEN_TRIM(host)))

        status = connect(connection, %REF(connectionInfo), SIZEOF(connectionInfo))

        RETURN
    END SUBROUTINE GetConnection
    
    SUBROUTINE SendMsg(connection, buffer, size, status, bytesSentTotal)

        use ws2_32

        IMPLICIT NONE 

        integer, intent(out) :: status, bytesSentTotal

        INTEGER connection

        INTEGER size

        CHARACTER*(size) buffer

        INTEGER SUCCESS

        PARAMETER(SUCCESS = 0)
        
        status = SUCCESS

        bytesSentTotal = 0
        bytesSentTotal = send(connection,buffer(bytesSentTotal + 1:bytesSentTotal + 1), (size - bytesSentTotal), 0)
        
        IF (bytesSentTotal .EQ. SOCKET_ERROR) THEN
            status = WSAGetLastError()
            RETURN
        ENDIF      
        
        RETURN
    END SUBROUTINE SendMsg
    
    SUBROUTINE ReceiveMsg(connection, buffer, size, status, bytesReceivedTotal)
        use ws2_32

        IMPLICIT NONE 
            
        integer, intent(out) :: status, bytesReceivedTotal
            
        INTEGER CONNECTION_DROPPED_BY_REMOTE_PARTY

        INTEGER SUCCESS

        PARAMETER(CONNECTION_DROPPED_BY_REMOTE_PARTY = X'05050505', SUCCESS = 0)

        INTEGER connection

        INTEGER size

        CHARACTER*(size) buffer

        bytesReceivedTotal = 0

        bytesReceivedTotal = recv(connection, buffer(bytesReceivedTotal + 1: bytesReceivedTotal + 1), (size - bytesReceivedTotal), 0)
        IF (bytesReceivedTotal .EQ. SOCKET_ERROR) THEN
            status = WSAGetLastError()
            RETURN
        ELSEIF (bytesReceivedTotal .EQ. 0) THEN
            status = CONNECTION_DROPPED_BY_REMOTE_PARTY
            RETURN
        ENDIF

        RETURN
    END SUBROUTINE ReceiveMsg 
    

    SUBROUTINE CloseConnection(connection)
        use ws2_32

        IMPLICIT NONE 

        INTEGER SUCCESS

        PARAMETER(SUCCESS = 0)

        INTEGER connection

        INTEGER status

        status = closesocket(connection)

        status = WSACleanup()

        RETURN
    END SUBROUTINE CloseConnection
        
    !END SECTION runtime ventptr change subroutines
    
    subroutine initial_solution(t,pdold,pdzero,rpar,ipar)

    ! determines an initial solution to the zone fire modeling equations.  A non-linear
    ! algebraic solver (SNSQE) is used to calculate initial room pressures that make dP/dt zero.  If an HVAC system
    ! is modeled then HVAC node pressures and hvac duct temperatures are also determined to force mass and energy conservation.

    integer, intent(in) :: ipar(*)
    real(eb), intent(in) :: t,pdzero(*), rpar(*)
    real(eb), intent(out) :: pdold(*)

    integer, parameter :: mxalg = 4*mxrooms+mxnode+mxbranch
    real(eb) deltamv(mxalg), hhvp(mxalg)
    integer, parameter :: lrwork = (3*mxalg**2+13*mxalg)/2
    real(eb) :: work(lrwork)
    integer :: ires, iopt, nalg1, nprint, i, info, n_odes
    real(eb) :: tol

    type(room_type), pointer :: roomptr

    ires = 0
1   continue

    call room_connections

    rpar2(1) = rpar(1)
    ipar2(1) = ipar(1)
    ipar2(2) = ipar(2)
    call setderv(-1)
    call calculate_residuals(t,p,pdzero,pdold,ires,rpar2,ipar2)
    iopt = 2
    tol = algtol
    nalg1 = n_rooms
    nprint = -1

    ! room pressures
    do i = 1, n_rooms
        hhvp(i) = p(i+nofp)
    end do

    do i = 1, nequals
        pinit(i) = p(i)
    end do
    if (option(fpsteady)==1) then
        call snsqe(gres,gjac,iopt,nalg1,hhvp,deltamv,tol,nprint,info, work,lrwork)
    else
        info = 1
    end if

    ! couldn't find a solution.  either try to recover or stop
    if (info/=1) then
        if (option(fpsteady)/=off) then
            option(fpsteady) = off
            write(iofill, '(a)') '***Error in initial_solution: Trying non-steady initial guess.'
            go to 1
        end if
        write(errormessage, '(a)') '***Error in initial_solution: Solver could not find an initial solution.'
        call cfastexit('initial_solution',1)
    end if

    ! if a room is not connected to any other room via a horizontal or
    ! vertical vent then do not use the snsqe pressure solution,
    ! use the original pressure solution that was based on rho*g*h.
    do i = 1, n_rooms
        roomptr => roominfo(i)
        if (roomptr%is_connection) p(i+nofp) = hhvp(i)
    end do

    call calculate_residuals(t,p,pdzero,pdold,ires,rpar,ipar)

    ! Added to synchronize_species_mass the species mass with the total mass of each layer at the new pressure
    n_odes = nofprd+1
    call synchronize_species_mass (p,n_odes)

    do i = 1, n_cons
        pdold(i+nofwt) = 0.0_eb
    end do
    return
    end subroutine initial_solution

! --------------------------- gres -------------------------------------------

    subroutine gres (nnn,hvpsolv,deltamv,iflag)

    !  calculates residuals for initial solution by snsqe
    !     arguments: nnn
    !                hvpsolv
    !                deltamv
    !                iflag

    integer, intent(in) :: nnn
    real(eb), intent(in) :: hvpsolv(nnn)
    integer, intent(out) :: iflag
    real(eb), intent(out) :: deltamv(*)

    integer :: nalg, i, ires
    real(eb) :: p2(maxteq), delta(maxteq), pdzero(maxteq), t

    type(room_type), pointer :: roomptr

    data pdzero /maxteq*0.0_eb/

    if (1.eq.2) iflag=-1 ! dummy statement to eliminate compiler warnings
    nalg = n_rooms
    do i = 1, nalg
        p2(i) = hvpsolv(i)
    end do
    do i = nalg + 1, nequals
        p2(i) = pinit(i)
    end do
    if (iprtalg/=0) then
        write (iofilo,*) 'room pressures'
        do i = 1, n_rooms
            write (iofilo,*) i,p2(i)
        end do
    end if
    t = stime
    ires = 0
    call calculate_residuals(t,p2,pdzero,delta,ires,rpar2,ipar2)
    do i = 1, nalg
        deltamv(i) = delta(i)
    end do
    do i = 1, n_rooms
        roomptr => roominfo(i)
        if (.not.roomptr%is_connection) deltamv(i) = 0.0_eb
    end do
    if (iprtalg/=0) then
        write (iofilo,*)'room pressure residuals'
        do i = 1, n_rooms
            write (iofilo,*)i,delta(i)
        end do
        write (iofilo,*)' '
        read (*,*)
    end if
    return
    end subroutine gres
    
! --------------------------- solve_simulation -------------------------------------------

    subroutine solve_simulation (tstop)

    ! main solution loop for the model
    ! Arguments: TSTOP   The final time to which CFAST should run

    !     The structure of the solver array is

    !     NOFP = offset for the main pressure; the array of base pressures for each compartment
    !     NOFTU = upper layer temperature
    !     NOFVU = upper layer volume
    !     NOFTL = lower layer temperature
    !     NOFWT = wall surface temperatures (equivalent to the number of profiles)
    !     NOFPRD = species
    !     NEQUALS = last element in the array.

    !     The arrays which use this structure are VATOL, VRTOL, P, PDOLD, PPRIME and PDZERO

    !     An important note - solve sets the last variable to be solved to NOFPRD
    !     which is the beginning of the species (-1) and the end of the array which
    !     is presently used by DASSL. The important point is that N_ODES is set to
    !     NOFPRD

    external post_process
    real(eb), intent(in) :: tstop

    integer, parameter :: maxord = 5
    integer, parameter :: lrwork = 40+(maxord+4)*maxeq+maxeq**2
    integer, parameter :: liw = 20+maxeq
    integer, parameter :: all = 1, some = 0
    ! character(len=80), parameter :: filename = 'times.txt'
    character(len=2), parameter :: valid_ids(3) = (/"d2", "d4", "d5"/)
    logical :: is_valid
    integer :: unit_number

    real(eb) :: rwork(lrwork), rpar(1)
    integer :: iwork(liw), info(15), ipar(3), info2(15)
    real(eb) :: pprime(maxteq), pdnew(maxteq), vatol(maxeq), vrtol(maxeq)
    real(eb) :: pdzero(maxteq) = 0.0_eb
    logical :: iprint, ismv, exists, ispread,firstpassforsmokeview
    integer :: idid, i, n_odes, nfires, icode, ieqmax, idisc, ires, idsave, ifdtect, ifobj, n, ipts
    real(eb) :: ton, toff, tpaws, tstart, tdout, dprint, dplot, dspread, t, tprint, td, tsmv, tspread, tout,  &
        ostptime, tdtect, tobj
    integer :: first_time
    integer :: stopunit, ios
    
    type(fire_type), pointer :: fireptr
    
    
    ! SECTION runtime ventptr change variables definition and establishing socket connection
    INTEGER  :: CONNECTION_DROPPED_BY_REMOTE_PARTY
    integer :: j, k
    INTEGER :: SUCCESS
    PARAMETER(CONNECTION_DROPPED_BY_REMOTE_PARTY = X'05050505', SUCCESS = 0)
    INTEGER :: connection
    TYPE :: T_CLIENT_SERVER_MESSAGE
    SEQUENCE
        ! The fields in this TYPE must share memory space with a dummy CHARACTER variable
        ! In this instance the ClientSendCount field from the first UNION MAP and the buffer CHARACTER variable from the
        ! second UNION map both start at the same memory address (i.e. they overlay one another in memory)
        ! The buffer CHARACTER variable is required because the Win32 send and recv routines accept a
        ! CHARACTER variable whose address is the starting memory address of the message to be sent or received,
        ! respectively
        UNION
            MAP
                INTEGER ClientSendCount
                INTEGER ServerSendCount
            END MAP
            MAP
                CHARACTER(len=3072) buffer
            END MAP
        END UNION
    END TYPE
    TYPE(T_CLIENT_SERVER_MESSAGE) clientServerMessage
    INTEGER status
    INTEGER bytesReceivedTotal
    INTEGER bytesSentTotal
    INTEGER idx
    INTEGER maxBufferSize
    character(len=3072), dimension(3072) :: keys
    real :: values(3072)
    integer :: num_entries, comma_index
    character(len=3072) :: key, value
    type(vent_type), pointer :: ventptr
    character(len=100) :: doorsOpeningLevelFileName
    character(len=400) :: doorsOpeningLevelFile
    character(len=3072) message
    character(len=100) line
    num_entries = 0
    maxBufferSize = 3072
    bytesReceivedTotal = 2
    status = 0
    bytesSentTotal = 2
    
    doorsOpeningLevelFileName = 'doors_opening_level_frame.txt'
    doorsOpeningLevelFile = trim(datapath) // trim(doorsOpeningLevelFileName)
    call GetConnection(connection)
    
    ! SECTION runtime ventptr change variables definition and establishing socket connection
    
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
    
    ! specific verification cases just do a calculation and stop then exit
    if (radi_verification_flag) then
        if (verification_time_step /= 0._eb) then
            dt = verification_time_step
        else
            dt = tstop
        end if 
        if (upper_layer_thickness /= -1001._eb) then
            do while (t<=tstop)
                call wall_opening_fraction(t)
                call output_spreadsheet(t)
            t = t + dt
            end do
            return
        end if
    end if

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
    do i = 1, n_rooms
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
    do i = 1, n_cons
        vatol(i+nofwt) = awtol
        vrtol(i+nofwt) = rwtol
    end do

    ovtime = 0.0_eb
    tovtime = 0.0_eb
    tottime = 0.0_eb
    prttime = 0.0_eb

    ! Set number of equations solved by DASSL
    n_odes = nofprd
    ipar(1) = n_odes
    ipar(2) = all
    idset = 0

    ! construct initial solution
    pdold(1:nequals) = 0.0_eb
    pold(1:nequals) = p(1:nequals)
    call initial_solution(t,pdold,pdzero,rpar,ipar)
    pprime(1:nequals) = pdold(1:nequals)
    pold(1:nequals) = p(1:nequals)

    ! Calculate the mass of objects that have been pyrolized
    ! at the moment we do only the total and the radiological species
    ! make sure that the INTEGRATE routine is called before update_species
    call integrate_mass (dt)
    call update_species (0.0_eb)

    ! If we are running only an initialization test then we do not need to solve anything
    if (initializeonly) then
        ! normally, this only needs to be done while running. however, if we are doing an initialonly run
        ! then we need the output now
        call collect_fire_data_for_smokeview (nfires)
        call output_smokeview(pressure_ref, exterior_abs_pressure, exterior_ambient_temperature, n_rooms,  &
             nfires, smv_room, smv_xfire, smv_yfire, smv_zfire, 0.0_eb, 1)
        icode = 0
        write (*, '(a)') 'Initialize only'
        write (iofill, '(a)') 'Initialize only'
        return
    end if

    ! main solve loop
    numjac = 0
    numstep = 0
    numresd = 0

    do while (idid>=0 .and. t+0.000001_eb<=tstop)
        
        if (t+0.0001_eb>min(tspread,tstop).and.ispread) then
          
            ! SECTION send message via socket
            if (t .gt. 0) then
                clientServerMessage%buffer = "ok"
                call SendMsg(connection, clientServerMessage%buffer, bytesReceivedTotal, status, bytesSentTotal)
            end if
            ! END SECTION send message via socket
                
            ! SECTION receive message containing hole opening % from socket and change ventptr%f holes opening
            if (t .gt. 0) then
                call ReceiveMsg(connection, clientServerMessage%buffer, maxBufferSize, status, bytesReceivedTotal)
                    
                open(unit=10, file=trim(doorsOpeningLevelFile), status='old', action='read')
                read(10, '(A)', iostat=ios) message
                close(10)
                    
                    
                do while (len(trim(message)) > 0)
                    comma_index = index(message, ',')
                    if (comma_index > 0) then
                        key = trim(adjustl(message(:comma_index-1)))
                        message = adjustl(message(comma_index+1:))
                    else
                        if (index(message, '.') .EQ. 0) then 
                            ! integer type - for example 0 or 1 
                            key = adjustl(message(:index(message, '=')+1))
                        else
                            !decimal type- for example 0.5 or 0.8 
                            key = adjustl(message(:index(message, '=')+3))
                        endif  
                        message = ""
                    end if
                
                    comma_index = index(key, '=')
                    if (comma_index > 0) then
                        value = trim(key(comma_index+1:))
                        key = trim(key(:comma_index-1))
                    else
                        value = ""
                    end if
        
                    ! We add a key-value pair to the dictionary
                    num_entries = num_entries + 1
                    keys(num_entries) = key
                    read(value, *) values(num_entries)
                                         
                end do
            
                ! Display the contents of the dictionary
                print *, "Current time:"
                print *, t

                print *, "Opening of the following doors will be changed to:"
                do i = 1, num_entries
                    print *, trim(keys(i)), "=", values(i)
                end do
                 
                !change ventptr%f array (hole oppening percentage)
                do i = 1, n_hvents
                    ventptr=>hventinfo(i)
                    do j = 1, num_entries
                        if (ventptr%CFAST_TYPE%ID == trim(keys(j))) then
                            do k = 1, ventptr%npoints
                                if (nint(t) == ventptr%t(k)) then
                                    if (k /= ventptr%npoints) then
                                        !ventptr%f(k) = values(j)
                                        ventptr%f(k+1) = values(j)
                                    !else
                                        !k is the last element of f() table (k==ventptr%npoints)
                                        !so we dont want to change f(k+1) element
                                        !get_vent_opening() subroutine in utilities.f90 will work fine.
                                    !    ventptr%f(k) = values(j)
                                    end if
                                end if
                            end do
                        end if
                    end do
                end do
            
                do i = 1, num_entries
                    values(i) = 0
                    keys(i) = ''
                end do
            
                num_entries=0
            
            end if
            ! END SECTION receive message containing hole opening % from socket and change ventptr%f holes opening
        end if        
                
                
                
        !unit_number = 10
        !open(unit=unit_number, file=filename, status='old', position='append', action='write')
        !write(unit_number, '(E24.16, A)') t, ' :timeeeee'


        !do i = 1, n_hvents
        !    ventptr=>hventinfo(i)
        !    is_valid = any(ventptr%CFAST_TYPE%ID == valid_ids)

        !    if (is_valid) then
        !        write(unit_number, '(A, A)') ventptr%id, ' :ventptr%id'
        !        do ipts = 1,30
        !            write(unit_number, '(E24.16, A)') ventptr%t(ipts), ' :time'
        !            write(unit_number, '(E24.16, A)') ventptr%f(ipts), ' :fraction'
        !        end do
        !    end if
        !end do

        !close(unit_number)

        
        ! DASSL equation with most error
        ieqmax = 0

        ! Check for interactive commands
        ! if a key has been pressed (and we are watching the keyboard) figure out what to do
        ! The escape key returns a code of 1
        if (.not.nokbd) call keyboard_interaction (t,icode,tpaws,tout,ieqmax)

        ! Check for stop file that overrides maximum iteration count
        inquire (file=stopfile, exist=exists)
        icode = 0
        n = stp_cnt_max
        if (exists) then
            stopunit = get_filenumber()
            open (stopunit,file=stopfile)
            read (stopunit,*,iostat=ios) n
            if (ios==0) then 
                stp_cnt_max = n
            else
                stp_cnt_max = 0
                icode = 1
            end if
            close(unit=stopunit)
        end if
        
        ! If the stop file exists or the esc key has been pressed, then quit
        if (icode==1.and.stp_cnt_max.eq.0) then
            call delete_output_files (stopfile)
            write (*,'(a,1pg11.3,a,g11.3)') 'Stopped by request at T = ', t, ' DT = ', dt
            write (iofill,'(a,1pg11.3,a,g11.3)') 'Stopped by request at T = ', t, ' DT = ', dt
            return
        end if

        ! Check the .query file. If it does not exist, do nothing. If if DOES exist, then
        ! rewind/write the status file and delete the query file (in that order).
        ! Ignore errors from deleting the file. It may not exist
        inquire (file=queryfile, exist = exists)
        if (exists) then
            call output_status (t, dt)
            call delete_output_files (queryfile)
        end if

        !Check to see if diagnostic files .resid and .jac exist. If they do exist
        !set flags and open file, if needed, to print diagnositic information.
        inquire (file=residfile, exist=exists)
        if (exists .or. option(fresidprn) == on) then
            residprn = .true.
            if (residfirst) then
                residfirst = .false.
                ioresid = get_filenumber()
                open (ioresid,file=residcsv)
                ioslab = get_filenumber()
                open (ioslab, file=slabcsv)
            end if
        else
            residprn = .false.
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
                ! collect_fire_data_for_smokeview just puts all of the fire information in a single list
                call collect_fire_data_for_smokeview (nfires)
                if (firstpassforsmokeview) then
                    firstpassforsmokeview = .false.
                    ! note: output_smokeview writes the .smv file. we do not close the file but only rewind so that smokeview
                    ! can have the latest time step information.
                    call output_smokeview (pressure_ref, exterior_abs_pressure, exterior_ambient_temperature, n_rooms, &
                        nfires, smv_room, smv_xfire, smv_yfire, smv_zfire, t, i_time_step)
                    call output_smokeview_header (cfast_version,n_rooms,nfires)
                end if
                ! Using the absolute room pressure minus the absolute ref pressure makes sure that the relp values going to
                ! smokeview are what is expected.
                smv_relp(1:n_rooms) = roominfo(1:n_rooms)%absp - pressure_ref
                smv_zlay(1:n_rooms) = roominfo(1:n_rooms)%depth(l)
                smv_tu(1:n_rooms) = roominfo(1:n_rooms)%temp(u)
                smv_tl(1:n_rooms) = roominfo(1:n_rooms)%temp(l)
                call output_smokeview_plot_data(t,n_rooms,smv_relp,smv_zlay,smv_tl,smv_tu,nfires, smv_qdot,smv_height)
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

                ! reset incremental FED data
                targetinfo(1:mxtarg)%dfed_gas = 0.0_eb
                targetinfo(1:mxtarg)%dfed_heat = 0.0_eb

              
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

        ! Special case for radiation verification
        if (radi_verification_flag) then
            t = tstop
            call target (1,t)
            call output_spreadsheet(t)
            return
        end if
        
        
        if (t<tstop) then
            idset = 0
            ipar(2) = some
            told = t
            call setderv(-1)
            call cptime(ton)
            call ddassl (calculate_residuals,n_odes,t,p,pprime,tout,info,vrtol,vatol,idid,rwork,lrwork,iwork,liw,rpar,ipar,jac)
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
                call write_error_component (ieqmax)
                write (*,'(a,i0)') '***Error, dassl - idid = ', idid
                write (iofill,'(a,i0)') '***Error, dassl - idid = ', idid
                call post_process
                stop
            end if

            dt = t - told
            if (stpminflag) then
                if (dt<stpmin) then
                    stpmin_cnt = stpmin_cnt + 1
                    if (stpmin_cnt>stpmin_cnt_max) then
                        ! model has hung (stpmin_cnt_max consective time step sizes were below stpmin)
                        write (*,'(a,i0,a,e11.4,a,e11.4)') &
                            '***Error, ', stpmin_cnt_max, 'Consecutive time steps with size below ', stpmin, ' at t = ', t
                        write (iofill,'(a,i0,a,e11.4,a,e11.4)') &
                            '***Error, ', stpmin_cnt_max, 'Consecutive time steps with size below ', stpmin, ' at t = ', t
                        stop
                    end if
                else
                    ! this time step is above the critical size so reset counter
                    stpmin_cnt = 0
                end if
            end if

            ipar(2) = all
            call calculate_residuals (t,p,pdzero,pdnew,ires,rpar,ipar)
            call update_solution (n_odes, nequals, t, told, p, pold, pdnew, pdold)

            ! advance the detector temperature solutions and check for object ignition
            idsave = 0
            call get_detector_temp_and_velocity
            call update_detectors (check_state,told,dt,n_detectors,idset,ifdtect,tdtect)
            call update_fire_ignition (check_state,told,dt,ifobj,tobj)
            td = min(tdtect,tobj)

            ! a detector is the first one that went off
            if (ifdtect>0.and.tdtect<=td) then
                call update_detectors (set_state,told,dt,n_detectors,idset,ifdtect,tdtect)
                idsave = ifobj
                td = tobj
                call calculate_residuals (t, p, pdzero, pdnew, ires, rpar, ipar)
                idset = 0
            else
                call update_detectors (update_state,told,dt,n_detectors,idset,ifdtect,tdtect)
            end if

            ! object ignition is the first thing to happen
            if (ifobj>0.and.ifobj <=n_fires.and.tobj<=td) then
                fireptr => fireinfo(ifobj)
                call update_fire_ignition (set_state,told,dt,ifobj,tobj)
                idsave = idset
                td = tdtect
                fireptr%ignited = .true.
                call set_info_flags(info,rwork)
                ifobj = 0
            else
                call update_fire_ignition (update_state,told,dt,ifobj,tobj)
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
                    call ddassl (calculate_residuals, &
                        n_odes,t,p,pprime,tdout,info2,vrtol,vatol,idid,rwork,lrwork,iwork,liw,rpar,ipar,jac)

                    ! make sure dassl is happy (again)
                    if (idid<0) then
                        call write_error_component (ipar(3))
                        write (*,'(a,i0)') '***Error, dassl - idid = ', idid
                        write (*,'(a,f10.5,1x,a,f10.5)') '***Error, Problem in DASSL backing from ',t,'to time ',tdout
                        write (iofill,'(a,i0)') '***Error, dassl - idid = ', idid
                        write (iofill,'(a,f10.5,1x,a,f10.5)') '***Error, Problem in DASSL backing from ',t,'to time ',tdout
                        call post_process
                        write (errormessage,'(a)') '***Error, Equation solver could not find a solution.'
                        call cfastexit ('solve_simulation', 3)
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
                    call update_solution (n_odes, nequals, t, told, p, pold, pdnew, pdold)
                    call set_info_flags (info,rwork)
                else if (td==t) then
                    call set_info_flags (info,rwork)
                    call calculate_residuals (t, p, pdzero, pdnew, ires, rpar, ipar)
                else
                    ! update_detectors said that a sprinkler has gone off but the time is wrong!!
                    write (*,'(a,f10.5,a,f10.5,a,f10.5)') '***Error, Back step too large in DASSL, Time = ', &
                        t,' Last time = ',told,' need to back step to ',td
                    write (iofill,'(a,f10.5,a,f10.5,a,f10.5)') '***Error, Back step too large in DASSL, Time = ', &
                        t,' Last time = ',told,' need to back step to ',td
                    write (errormessage,'(a)') '***Error, Equation solver could not find a solution.'
                    call cfastexit ('solve_simulation', 4)
                    stop
                end if
            end if

            ! calculate the mass of objects that have been pyrolized
            ! at the moment we do only the total and the radiological species
            ! It is important to call the routine to integrate the mass before call the toxicology calculation
            call integrate_mass (dt)

            ! calculate gas dosage
            call update_species (dt)

            if (option(fdebug)==on) call output_debug (2,t,dt,ieqmax)
            numstep = numstep + 1
            total_steps = total_steps + 1
            if (stp_cnt_max>=0.and.total_steps>stp_cnt_max) then
                call delete_output_files (stopfile)
                write (errormessage,'(a,1pg11.3,a,g11.3)') 'Stopped by user request at T = ', t, ' DT = ', dt
                call cfastexit ('solve_simulation', 5)
            end if
        end if
    end do
    
    ! SECTION send message via socket
    if (t .gt. 0) then
        clientServerMessage%buffer = "ok"
        call SendMsg(connection, clientServerMessage%buffer, bytesReceivedTotal, status, bytesSentTotal)
    end if
    ! END SECTION send message via socket  
                
    return

    end subroutine solve_simulation

! --------------------------- update_solution -------------------------------------------

    subroutine update_solution(n_odes, nequals,  t, told, p, pold, pdnew, pdold)

    ! update solution returned by dassl

    integer, intent(in) :: n_odes, nequals
    real(eb), intent(in) :: t, told, pdnew(*)

    real(eb), intent(inout) :: p(*), pdold(*)
    real(eb), intent(out) :: pold(*)

    integer :: i
    real(eb) :: dt

    dt = t - told

    ! advance species
    do i = n_odes + 1, nequals
        p(i) = p(i) + dt*pdold(i)
        p(i) = max (0.0_eb, p(i))
        pdold(i) = pdnew(i)
    end do

    ! advance target temperatures
    call target (1,dt)

    ! make sure species mass adds up to total mass
    if (ns>0) call synchronize_species_mass (p,n_odes+1)

    pold(1:nequals) = p(1:nequals)

    return

    end subroutine update_solution

! --------------------------- keyboard_interaction -------------------------------------------

    subroutine keyboard_interaction (t,icode,tpaws,tout,ieqmax)

    ! keyboard routine for user interaction during simulation

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

    ! quick output of keyboard shortcuts available during simulaiton

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

    ! update solution flags for dassl solver

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

    ! Calculates the residual F(t,y,dy/dt) for CFAST differential and algebraic equations.  
    ! For the gas differential equations (pressure, layer volume, upper/lower layer temperature) F(t,y,dy/dt) takes
    ! the form F(t,y,dy/dt) = dy/dt - f(t,y) where f(t,y) is related to the conservation of mass and and energy.
    
    ! For the wall temperature equations, F is just Fourier's law taking the form of
    !     F(t,y,dy/dt) = q''(t,y) + K dT/dx
    ! where q'' is the flux striking the wall, K is the wall's thermal conductivity and dT/dx is the surface wall
    ! temperature gradient.
    ! arguments: tsec    Current simulation time (T above in s)
    !            y_vector Current guess at solution vector (Y above)
    !            yprime_vector Current guess at derivative of solution vector (Y' above)
    !            f_vector Residual or value of F(t,y,dy/dt)
    !            ires    
    ! outputs   ires Integer flag which is always equal to zero on input. calculate_residuals should alter IRES
    !                   only if it encounters an illegal value of Y or a stop condition. Set IRES = -1 if an input
    !                   value is illegal, and DDASSL will try to solve the problem without getting IRES = -1. If
    !                   IRES = -2, DASSL return control to the calling program with IDID = -11.
    !           rpar    real parameter arrays
    !           ipar    integer parameter arrays
    !                   These are used for communication between solve_simulation and
    !                   calculate_residuals via DASSL. They are not altered by DASSL.
    !                   Currently, only IPAR is used in calculate_residuals to pass
    !                   a partial/total flag for solution of the species equations.

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
    real(eb) :: flows_total(mxrooms,ns+2,2), fluxes_total(mxrooms,nwal)

    ! data structures for flow through vents
    real(eb) :: flows_hvents(mxrooms,ns+2,2)
    real(eb) :: flows_leaks(mxrooms,ns+2,2)
    real(eb) :: flows_vvents(mxrooms,ns+2,2)
    real(eb) :: flows_mvents(mxrooms,ns+2,2), filtered(mxrooms,ns+2,2)

    ! data structures for fires
    real(eb) :: flows_fires(mxrooms,ns+2,2)
    real(eb) :: flows_doorjets(mxrooms,ns+2,2)
    integer :: update

    ! data structures for convection, radiation, and ceiling jets
    real(eb) :: flows_convection(mxrooms,2), fluxes_convection(mxrooms,nwal)
    real(eb) :: flows_radiation(mxrooms,2), fluxes_radiation(mxrooms,nwal)
    
    ! data structures for heat and mass transfer between layers
    real(eb) :: flows_layer_mixing(mxrooms, ns+2, 2)

    logical :: djetflg
    integer :: nprod, i, iroom, iprod, ip, iwall, nprodsv, iprodu, iprodl
    real(eb) :: epsp, aroom, hceil, pabs, hinter, ql, qu, tmu, tml
    real(eb) :: oxydu, oxydl, pdot, tlaydu, tlaydl, vlayd, prodl, produ

    ires = ires ! just to get rid of a warning message
    nprod = ns
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

    ! calculate flow due to unforced vents (wall_flow for doors/windows
    ! and vertical_flow for ceiling/floor vents
    call wall_flow (tsec,epsp,flows_hvents)
    call leakage_flow (epsp,flows_leaks)
    call vertical_flow (tsec,epsp,flows_vvents)
    call mechanical_flow (tsec,epsp,flows_mvents,filtered)

    ! calculate heat and mass flows due to fires
    call fire (tsec,flows_fires)
    call vent_jets (flows_doorjets,djetflg)
    
    ! calculation opening fraction for radiation loss and etc
    call wall_opening_fraction (tsec)

    ! calculate flow and flux due to heat transfer (ceiling jets, convection and radiation)
    call convection (flows_convection,fluxes_convection)
    call radiation (flows_radiation,fluxes_radiation)
    
    call layer_mixing(flows_layer_mixing)

    ! sum flow for inside rooms
    do iroom = 1, n_rooms
        roomptr => roominfo(iroom)

        do iprod = 1, nprod + 2
            ip = i_speciesmap(iprod)
            flows_total(iroom,iprod,l) = flows_hvents(iroom,iprod,l) + flows_leaks(iroom,iprod,l) + flows_fires(iroom,ip,l)
            flows_total(iroom,iprod,u) = flows_hvents(iroom,iprod,u) + flows_leaks(iroom,iprod,u) + flows_fires(iroom,ip,u)
        end do
        do iprod = 1, nprod + 2
            ip = i_speciesmap(iprod)
            flows_total(iroom,iprod,l) = flows_total(iroom,iprod,l) + flows_vvents(iroom,ip,l)
            flows_total(iroom,iprod,u) = flows_total(iroom,iprod,u) + flows_vvents(iroom,ip,u)
        end do
        do iprod = 1, nprod + 2
            ip = i_speciesmap(iprod)
            flows_total(iroom,iprod,l) = flows_total(iroom,iprod,l) + flows_mvents(iroom,ip,l) - filtered(iroom,iprod,l)
            flows_total(iroom,iprod,u) = flows_total(iroom,iprod,u) + flows_mvents(iroom,ip,u) - filtered(iroom,iprod,u)
        end do
        do iprod = 1, nprod + 2
            ip = i_speciesmap(iprod)
            flows_total(iroom,iprod,l) = flows_total(iroom,iprod,l) + flows_layer_mixing(iroom,iprod,l)
            flows_total(iroom,iprod,u) = flows_total(iroom,iprod,u) + flows_layer_mixing(iroom,iprod,u)
        end do
        if (djetflg) then
            do iprod = 1, nprod + 2
                ip = i_speciesmap(iprod)
                flows_total(iroom,iprod,l) = flows_total(iroom,iprod,l) + flows_doorjets(iroom,ip,l)
                flows_total(iroom,iprod,u) = flows_total(iroom,iprod,u) + flows_doorjets(iroom,ip,u)
            end do
        end if

        flows_total(iroom,q,l) = flows_total(iroom,q,l) + flows_convection(iroom,l) + flows_radiation(iroom,l)
        flows_total(iroom,q,u) = flows_total(iroom,q,u) + flows_convection(iroom,u) + flows_radiation(iroom,u)


        ! if this room is a shaft then solve for only one zone.
        ! this is done by combining flows from to both
        ! layers into upper layer flow and setting lower layer flow to
        ! zero.
        if (roomptr%shaft) then
            do iprod = 1, nprod + 2
                flows_total(iroom,iprod,u) = flows_total(iroom,iprod,u) + flows_total(iroom,iprod,l)
                flows_total(iroom,iprod,l) = 0.0_eb
            end do
        end if
    end do

    if (update==all) then
        if (residprn) then
            call output_spreadsheet_residuals (tsec, flows_total, flows_hvents, flows_fires, flows_vvents, flows_mvents, &
                filtered, flows_doorjets, flows_layer_mixing, flows_convection, flows_radiation, fluxes_convection, &
                fluxes_radiation)
        end if
    end if
    ! sum flux for inside rooms
    do iroom = 1, n_rooms
        roomptr => roominfo(iroom)
        do iwall = 1, nwal
            if (roomptr%surface_on(iwall)) then
                fluxes_total(iroom,iwall) = fluxes_convection(iroom,iwall) + fluxes_radiation(iroom,iwall)
            end if
        end do
    end do

    ! set nprod to zero when we are only solving "some" of the ode's
    if (ipar(2)==some) then
        nprodsv = nprod
        nprod = 0
    end if

    ! calculate rhs of ode's for each room
    do iroom = 1, n_rooms
        roomptr => roominfo(iroom)
        aroom = roomptr%floor_area
        hceil = roomptr%cheight
        pabs = roomptr%absp
        hinter = roomptr%depth(l)
        ql = flows_total(iroom,q,l)
        qu = flows_total(iroom,q,u)
        tmu = flows_total(iroom,m,u)
        tml = flows_total(iroom,m,l)

        if (option(foxygen)==on) then
            oxydu = flows_total(iroom,4,u)
            oxydl = flows_total(iroom,4,l)
        end if

        ! pressure equation
        if (roomptr%deadroom.eq.0) then
            pdot = (gamma-1.0_eb)*(ql + qu)/(aroom*hceil)
        else
            pdot = 0.0_eb
        end if

        ! upper layer temperature equation
        tlaydu = (qu-cp*tmu*roomptr%temp(u))/(cp*roomptr%mass(u)) + pdot/(cp*roomptr%rho(u))

        ! upper layer volume equation
        vlayd = (gamma-1.0_eb)*qu/(gamma*pabs) - roomptr%volume(u)*pdot/(gamma*pabs)
        if (roomptr%shaft) vlayd = 0.0_eb

        ! lower layer temperature equation
        tlaydl = (ql-cp*tml*roomptr%temp(l))/(cp*roomptr%mass(l)) + pdot/(cp*roomptr%rho(l))

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
            do iroom = 1, n_rooms
                roomptr => roominfo(iroom)
                hceil = roomptr%cheight
                hinter = roomptr%depth(l)
                iprodu = iprodu + 2
                iprodl = iprodu + 1
                prodl = flows_total(iroom,iprod+2,l)

                ! if this room is a hall and the jet has not reached the end
                ! of the hall then don't solve for it using dassl
                produ = flows_total(iroom,iprod+2,u)

                if (hinter<hceil) then
                    yhatprime_vector(iprodu) = produ
                else if (hinter>=hceil.and.flows_total(iroom,m,u)<0.0_eb)  then
                    yhatprime_vector(iprodu) = produ
                else
                    yhatprime_vector(iprodu) = 0.0_eb
                end if
                if (hinter>0.0_eb) then
                    yhatprime_vector(iprodl) = prodl
                else if (hinter<=0.0_eb.and.flows_total(iroom,m,l)>0.0_eb) then
                    yhatprime_vector(iprodl) = prodl
                else
                    yhatprime_vector(iprodl) = 0.0_eb
                end if
            end do
        end do
    end if

    ! residuals for pressure
    do i = nofp + 1, nofp + n_rooms
        f_vector(i) = yhatprime_vector(i) - yprime_vector(i)
    end do

    ! residuals for layer volume, and layer temperatures
    do i = noftu + 1, noftu + 3*n_rooms
        f_vector(i) = yhatprime_vector(i) - yprime_vector(i)
    end do

    ! residual for oxygen
    if (option(foxygen)==on) then
        do i = 1, n_rooms
            f_vector(i+nofoxyu) = yhatprime_vector(i+nofoxyu) - yprime_vector(i+nofoxyu)
            f_vector(i+nofoxyl) = yhatprime_vector(i+nofoxyl) - yprime_vector(i+nofoxyl)
        end do
    end if

    ! conduction residual
    call conduction (update,dt,fluxes_total,f_vector)

    ! residuals for stuff that is solved in solve_simulation itself, and not by dassl
    if (nprod/=0) then
        ! residuals for gas layer species
        do i = nofprd + 1, nofprd + 2*nprod*n_rooms
            f_vector(i) = yhatprime_vector(i) - yprime_vector(i)
        end do
    end if

    if (ipar(2)==some) nprod = nprodsv

    return

    end subroutine calculate_residuals

! --------------------------- update_data -------------------------------------------

    subroutine update_data (y_vector,iflag)

    ! calculate environment variables from the solver vector

    ! arguments: y_vector   solver vector
    !            iflag  action flag:
    ! iflag = constvar ==> constant data (data that does not change with time)
    ! iflag = odevara  ==> ode variables: pressure, temperature and upper layer volume
    ! iflag = odevarb  ==> species data and wall temperature profile. use yvector_old and pdold to estimate species
    ! iflag = odevarc  ==> species data and wall temperature profile. use pdif array for species

    integer, intent(in) :: iflag
    real(eb), intent(in) :: y_vector(*)


    integer :: iroom, lsp, layer, i, itstop, ieq, iwall, ii
    integer :: iwfar, ifromr, ifromw, itor, itow, ieqfrom, ieqto, itarg
    integer :: npts, iwalleq, iwalleq2, iinode, ilay, isof
    real(eb) :: wtemp
    real(eb) :: xdelt, tstop, zzu, zzl
    real(eb) :: zlay, ztarg, ppgas, totl, totu, rtotl, rtotu, oxyl, oxyu
    real(eb) :: xt, xtemp, xh2o, ptemp, epscut
    real(eb) :: xmax, xmid, ymax, ymid, zmax

    type(room_type), pointer :: roomptr, deadroomptr
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
    type(ramp_type), pointer :: rampptr

    if (n_furn>0.and.iflag/=constvar) then
        call interp(furn_time,furn_temp,n_furn,stime,1,wtemp)
        wtemp = wtemp + kelvin_c_offset
        qfurnout=sigma*wtemp**4
    end if

    if (iflag==constvar) then
        do iroom = 1, n_rooms+1
            roomptr => roominfo(iroom)
            roomptr%vmin = min(vminfrac*roomptr%cvolume, 1.0_eb)
            roomptr%vmax = roomptr%cvolume - roomptr%vmin
        end do
        do iroom = 1, n_rooms
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

        roomptr=>roominfo(n_rooms+1)

        roomptr%z0 = 0.0_eb
        roomptr%z1 = 100000.0_eb

        roomptr%volume(u) = 0.0_eb
        roomptr%volume(l) = 100000.0_eb
        roomptr%depth(u) = 0.0_eb
        roomptr%depth(l) = 100000.0_eb
        roomptr%relp = 0.0_eb
        roomptr%absp = pressure_offset
        roomptr%temp(u) = exterior_ambient_temperature
        roomptr%temp(l) = exterior_ambient_temperature
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
        xt = exterior_ambient_temperature
        xtemp = 23.2_eb - 3.816e3_eb/(xt-46.0_eb)
        xh2o = exp(xtemp)/101325.0_eb*(18.016_eb/28.584_eb)
        roomptr%species_fraction(u,h2o) = relative_humidity*xh2o
        roomptr%species_fraction(l,h2o) = relative_humidity*xh2o

        roomptr%rho(u:l) = roomptr%absp/rgas/roomptr%temp(u:l)
        roomptr%mass(u:l) = roomptr%rho(u:l)*roomptr%volume(u:l)

        ! define discontinuity array.  first we look at vent openings
        xdelt = time_end/deltat
        itstop = xdelt + 1
        tstop = itstop - 1

        discon = 0.0_eb
        discon(1) = tstop
        ndisc = 2

        ! add each of the change arrays to the discontinuity list
        if (ndisc+2*n_hvents+2*n_vvents+4*n_mvents<=mxdiscon) then
            do  i = 1, n_hvents
                ventptr => hventinfo(i)
                discon(ndisc) = ventptr%opening_initial_time
                discon(ndisc+1) = ventptr%opening_final_time
                ndisc = ndisc + 2
            end do
            do  i = 1, n_vvents
                ventptr => vventinfo(i)
                discon(ndisc) = ventptr%opening_initial_time
                discon(ndisc+1) = ventptr%opening_final_time
                ndisc = ndisc + 2
            end do
            do i = 1, n_mvents
                ventptr => mventinfo(i)
                discon(ndisc) = ventptr%opening_initial_time
                discon(ndisc+1) = ventptr%opening_final_time
                discon(ndisc+2) = ventptr%filter_initial_time
                discon(ndisc+3) = ventptr%filter_final_time
                ndisc = ndisc + 4
            end do
        else
            write (*,10) ndisc+2*n_hvents+2*n_vvents+4*n_mvents, mxdiscon
            write (iofill,10) ndisc+2*n_hvents+2*n_vvents+4*n_mvents, mxdiscon
10          format('***Error, Insufficient space in discontinuity array. Required: ',i0,'. Allocated: ',i0)
        end if
        
        do i = 1, n_ramps
            rampptr => rampinfo(i)
            do ii = 1, rampptr%npoints
                discon(ndisc+ii-1) = rampptr%x(ii)
            end do
            ndisc = ndisc + rampptr%npoints
        end do

        ! put the discontinuity array into order
        call shellsort (discon(0), ndisc+1)
        
        ! define i_wallmap for jac and other constants for dassl and the conduction routine
        ieq = nofwt
        i_wallmap(n_rooms+1,1:4) = 0
        do iroom = 1, n_rooms
            roomptr => roominfo(iroom)
            do iwall = 1, 4
                if (roomptr%surface_on(iwall)) then
                    ieq = ieq + 1
                    i_wallmap(iroom,iwall) = ieq

                    ! define surface_connections, to describe ceiling-floor connections
                    ! first assume that walls are connected to the outside
                    ii = ieq - nofwt
                    surface_connections(ii,w_from_room) = iroom
                    surface_connections(ii,w_from_wall) = iwall
                    surface_connections(ii,w_to_room) = n_rooms + 1
                    if (iwall==1.or.iwall==2) then
                        iwfar = 3 - iwall
                    else
                        iwfar = iwall
                    end if
                    surface_connections(ii,w_to_wall) = iwfar
                    surface_connections(ii,w_boundary_condition) = iwbound

                else
                    i_wallmap(iroom,iwall) = 0
                end if
            end do
        end do

        ! update surface_connections for ceiling/floors that are connected
        do i = 1, n_vcons
            ifromr = vertical_connections(i,w_from_room)
            ifromw = vertical_connections(i,w_from_wall)
            itor = vertical_connections(i,w_to_room)
            itow = vertical_connections(i,w_to_wall)
            ieqfrom = i_wallmap(ifromr,ifromw) - nofwt
            ieqto = i_wallmap(itor,itow) - nofwt

            surface_connections(ieqfrom,w_to_room) = itor
            surface_connections(ieqfrom,w_to_wall) = itow
            surface_connections(ieqfrom,w_boundary_condition) = 1

            surface_connections(ieqto,w_to_room) = ifromr
            surface_connections(ieqto,w_to_wall) = ifromw
            surface_connections(ieqto,w_boundary_condition) = 1
        end do

        jacdim = nofprd - nofp

        ! indicate which rooms are connected to an hvac system
        roominfo(1:n_rooms)%is_hvac = .false.
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            ii = ventptr%room1
            roomptr => roominfo(ii)
            roomptr%is_hvac = .true.
            ii = ventptr%room2
            roomptr => roominfo(ii)
            roomptr%is_hvac = .true.
        end do

    else if (iflag==odevara) then
        do iroom = 1, n_rooms
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
            if(radiation_fix==1.and.roomptr%depth(l)<0.1_eb)then
                roomptr%depth(l)  = 0.1_eb
                roomptr%depth(u)  = roomptr%cheight - 0.1_eb
                roomptr%volume(l) = 0.1_eb*roomptr%floor_area
                roomptr%volume(u) = roomptr%cvolume - roomptr%volume(l)
            endif

            roomptr%relp = y_vector(iroom)
            roomptr%absp = y_vector(iroom) + pressure_offset
            if (n_furn>0) then
              roomptr%temp(u) = wtemp
              roomptr%temp(l) = wtemp
            else if (radi_verification_flag) then
              roomptr%temp(u) = gas_temperature
              roomptr%temp(l) = gas_temperature
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

        do i = 1, n_rooms
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
        do iroom = 1, n_rooms
            roomptr => roominfo(iroom)
            do iwall = 1, nwal
                iwalleq = i_wallmap(iroom,iwall)
                if (iwalleq/=0) then
                    if (n_furn.gt.0) then
                        roomptr%t_surfaces(1,iwall) = wtemp
                        roomptr%t_surfaces(2,iwall) = wtemp
                    else
                        roomptr%t_surfaces(1,iwall) = y_vector(iwalleq)
                        ieqfrom = iwalleq - nofwt
                        itor = surface_connections(ieqfrom,w_to_room)
                        itow = surface_connections(ieqfrom,w_to_wall)
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
                    if (n_furn.gt.0) then
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
            do iroom = 1, n_rooms
                roomptr => roominfo(iroom)
                isof = isof + 1
                if (radi_verification_flag) then
                    if (partial_pressure_co2+partial_pressure_h2o == 101325._eb) then ! Only H2O and CO2 exist
                        !if (lsp == 1 .or. lsp == 2) then ! N2 and O2 will have to be zero 
                        if (lsp == n2 .or. lsp == o2) then ! N2 and O2 will have to be zero 
                            ppgas = 0._eb
                        !else if (lsp == 3) then
                        else if (lsp == co2) then
                            ! partical pressure = species_mass/its molecular weight*ideal gas constant*gas temperature/volume of the medium
                            roomptr%species_mass(u,lsp) = &
                            partial_pressure_co2/101325._eb*44.0088e-3_eb/82.0562e-6_eb/roomptr%temp(u)*roomptr%volume(u)
                        !else if (lsp == 8) then
                        else if (lsp == h2o) then
                            roomptr%species_mass(u,lsp) = &
                            partial_pressure_h2o/101325._eb*18.0153e-3_eb/82.0562e-6_eb/roomptr%temp(u)*roomptr%volume(u)
                        else 
                            ppgas = 0._eb
                        end if
                    end if
                else
                    if (iflag==odevarb) then
                        ppgas = pold(isof) + dt*pdold(isof)
                    else
                        ppgas = y_vector(isof)
                    end if
                    roomptr%species_mass(u,lsp) = max(ppgas,0.0_eb)
                end if
                                
                isof = isof + 1
                if (radi_verification_flag) then
                    if (partial_pressure_co2+partial_pressure_h2o == 101325._eb) then
                        !if (lsp == 1 .or. lsp == 2) then 
                        if (lsp == n2 .or. lsp == o2) then 
                            ppgas = 0._eb
                        !else if (lsp == 3) then
                        else if (lsp == co2) then
                            roomptr%species_mass(l,lsp) = &
                            partial_pressure_co2/101325._eb*44.0088e-3_eb/82.0562e-6_eb/roomptr%temp(l)*roomptr%volume(l)
                        !else if (lsp == 8) then
                        else if (lsp == h2o) then
                            roomptr%species_mass(l,lsp) = &
                            partial_pressure_h2o/101325._eb*18.0153e-3_eb/82.0562e-6_eb/roomptr%temp(l)*roomptr%volume(l)
                        else 
                            ppgas = 0._eb
                        end if
                    end if
                else
                    if (iflag==odevarb) then
                        ppgas = pold(isof) + dt*pdold(isof)
                    else
                        ppgas = y_vector(isof)
                    end if
                    roomptr%species_mass(l,lsp) = max(ppgas,0.0_eb)
                end if
            end do
        end do

        ! define species mass fractions: normalize to total product mass
        ! rather than total mass (this is equivalent to what was being done
        ! in chemistry)
        do iroom = 1, n_rooms
            roomptr => roominfo(iroom)
            totl = 0.0_eb
            totu = 0.0_eb
            !do lsp = 1, min(9,ns)
            do lsp = 1, ns_mass
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
    return

    end subroutine update_data
    

        
    
end module solve_routines