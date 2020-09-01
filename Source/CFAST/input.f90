    module input_routines

    use precision_parameters

    use exit_routines, only: cfastexit, delete_output_files
    use fire_routines, only: flame_height
    use initialization_routines, only : initialize_leakage, initialize_targets, initialize_ambient, initialize_solver_vector
    use namelist_input_routines, only: namelist_input, read_misc
    use numerics_routines, only : dnrm2
    use output_routines, only: open_output_files
    use utility_routines, only: emix
    
    use cfast_types, only: detector_type, fire_type, iso_type, room_type, slice_type, target_type, material_type, vent_type, &
        visual_type

    use cenviro, only: rgas
    use cparams, only: mxpts, mxrooms, mx_hsep, mx_vsep, smoked, w_from_room, w_to_room, w_from_wall, w_to_wall, &
        mx_dumps, interior, exterior
    use diag_data, only: radi_verification_flag, residfile, residcsv, slabcsv
    use fire_data, only: n_fires, fireinfo, lower_o2_limit
    use namelist_data, only: nmlflag
    use setup_data, only: iofili, iofilg, iofill, inputfile, outputfile, exepath, datapath, project, extension, smvhead, smvdata, &
        smvcsv, smvsinfo, sscompartment, ssdevice, sswall, ssmasses, ssvent, &
        ssdiag, sscalculation, heading, validation_flag, gitfile, errorlogging, stopfile, queryfile, statusfile, &
        overwrite_testcase, cfast_input_file_position
    use smkview_data, only: n_slice, n_iso, n_visual, isoinfo, sliceinfo, visualinfo
    use devc_data, only: n_detectors, detectorinfo, n_targets, targetinfo
    use material_data, only: n_matl, material_info
    use vent_data, only: n_hvents, n_vvents, hventinfo, vventinfo
    use room_data, only: n_rooms, roominfo, exterior_ambient_temperature, interior_ambient_temperature, exterior_abs_pressure, &
        interior_abs_pressure, pressure_ref, pressure_offset, exterior_rho, interior_rho, n_vcons, vertical_connections
    use dump_data, only: n_dumps, dumpinfo

    implicit none
    external get_info

    private

    public read_input_file, open_files, exehandle

    contains

    ! --------------------------- read_input_file -------------------------------------------

!> \brief   read the input file and set up the data for processing

    subroutine read_input_file ()

    implicit none

    real(eb) :: temparea(mxpts), temphgt(mxpts), deps1, dwall1, dwall2, rti
    real(eb) :: xloc, yloc, zloc, zbot, ztop, pyramid_height, dheight, xx, sum
    integer :: ios, i, ii, j, itop, ibot, nswall2, iroom, iroom1, iroom2
    integer :: iwall1, iwall2, itype, npts, ioff, ioff2

    type(detector_type), pointer :: dtectptr
    type(fire_type), pointer :: fireptr
    type(room_type), pointer :: roomptr, roomptr2
    type(target_type), pointer :: targptr
    type(material_type), pointer :: thrmpptr
    type(vent_type), pointer :: ventptr    

    ! deal with opening the data file and assuring ourselves that it is compatible
    close (iofili)
    open (newunit=iofili,file=inputfile,status='OLD',iostat=ios)
    if (ios/=0) then
        if (iofill>0) then
            write (*,5050) modulo(ios,256)
            write (iofill,5050) modulo(ios,256)
        else
            write (*,5050) modulo(ios,256)
        end if
        call cfastexit('readinputfile',1)
        stop
    end if

    n_matl = 0
 
    ! read in the input file in new (namelist) or old (.csv) format
    if (nmlflag) then
        call namelist_input
    else
        write(*, '(a,i3)') '***Error: This version of CFAST will only read the newer namelist input format' 
        write(iofill, '(a,i3)') '***Error: This version of CFAST will only read the newer namelist input format'
        call cfastexit('readinputfile',2)
        stop
    end if

    ! add the default thermal property
    n_matl = n_matl + 1
    thrmpptr => material_info(n_matl)
    thrmpptr%id = 'DEFAULT'
    thrmpptr%material = 'Default values assigned to surfaces that do no have a material assigned'
    thrmpptr%fyi = 'It is best to not use this material '
    thrmpptr%eps = 0.90_eb
    thrmpptr%nslab = 1
    thrmpptr%k(1) = 0.120_eb
    thrmpptr%c(1) = 900.0_eb
    thrmpptr%rho(1) = 800.0_eb
    thrmpptr%thickness(1) = 0.0120_eb
   
    ! now we can check the input data for consistency

    ! check for temperature outside reasonable limits
    if (.not.radi_verification_flag)  then
        if (exterior_ambient_temperature>373.15_eb.or.exterior_ambient_temperature<223.15_eb) then
            write (*,5022) exterior_ambient_temperature
            write (iofill,5022) exterior_ambient_temperature
            call cfastexit('readinputfile',3)
            stop
        end if
        if (interior_ambient_temperature>373.15_eb.or.interior_ambient_temperature<223.15_eb) then
            write (*,5022) interior_ambient_temperature
            write (iofill,5022) interior_ambient_temperature
            if (.not.radi_verification_flag) then
                call cfastexit('readinputfile',4)
                stop
            end if
        end if
    end if

    ! make pressures consistent with temperatures
    interior_abs_pressure = exterior_abs_pressure*interior_ambient_temperature/exterior_ambient_temperature
    pressure_ref = exterior_abs_pressure
    pressure_offset = pressure_ref

    ! compartment geometry related data
    do i = 1, n_rooms
        roomptr => roominfo(i)
        roomptr%x1 = roomptr%x0 + roomptr%cwidth
        roomptr%y1 = roomptr%y0 + roomptr%cdepth
        roomptr%z1 = roomptr%z0 + roomptr%cheight
    end do

    ! we now know what output is going to be generated, so create the files
    call open_output_files

    interior_rho = interior_abs_pressure/interior_ambient_temperature/rgas
    exterior_rho = exterior_abs_pressure/exterior_ambient_temperature/rgas

    ! initialize the targets.
    call initialize_targets

    ! now calculate the offsets - the order is important
    call initialize_solver_vector

    ! check fire objects
    do i = 1, n_fires
        fireptr => fireinfo(i)
        roomptr => roominfo(fireptr%room)
        if ((fireptr%x_position<0.0_eb).or.(fireptr%x_position>roomptr%cwidth)) fireptr%x_position = roomptr%cwidth/2.0_eb
        if ((fireptr%y_position<0.0_eb).or.(fireptr%y_position>roomptr%cdepth)) fireptr%y_position = roomptr%cdepth/2.0_eb
        if ((fireptr%z_position<0.0_eb).or.(fireptr%z_position>roomptr%cheight)) fireptr%z_position = 0.0_eb
    end do
    if (lower_o2_limit<0.0_eb.or.lower_o2_limit>0.230_eb) then
        write (*,*) '***Error: Specified LOI is less than zero or greater than ambient. Default of 0.15 used'
        write (iofill,*) '***Error: Specified LOI is less than zero or greater than ambient. Default of 0.15 used'
        lower_o2_limit = 0.15_eb
    end if
    
    !make sure wall vent specifications are correct
    do i = 1,n_hvents
        ventptr => hventinfo(i)
        roomptr => roominfo(ventptr%room1)
        ! wall vent must be completely within the wall of the from compartment
        zbot = ventptr%sill
        ztop = ventptr%soffit
        if (zbot<0.0_eb.or.zbot>roomptr%cheight.or.ztop<0.0_eb.or.ztop>roomptr%cheight.or.ztop<zbot) then
            write (*,'(a,2e11.4,a)') '***Error: Invalid HVENT specification. sill and/or soffit height =',&
                zbot, ztop,' out of bounds'
            write (iofill,'(a,2e11.4,a)') '***Error: Invalid HVENT specification. sill and/or soffit height =',&
                zbot, ztop,' out of bounds'
                call cfastexit('readinputfile',5)
            stop
        end if
        ! outside must always be second compartment
        if (ventptr%room1==n_rooms+1) then
            write (*,'(a)') '***Error: Compartment order is incorrect. Outside must always be second compartment.'
            write (iofill,'(a)') '***Error: Compartment order is incorrect. Outside must always be second compartment.'
            call cfastexit('readinputfile',6)
            stop
        end if
    end do
    
    ! add leakage if it's specified
    call initialize_leakage

    ! make sure ceiling/floor vent specifications are correct -  we have to do this
    ! here rather than right after keywordcases because floor_height and ceiling_height were just defined
    ! above
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        if (ventptr%room1==ventptr%room2) then
            write (*,*) '***Error: A room can not be connected to itself with a vertical vent'
            write (iofill,*) '***Error: A room can not be connected to itself with a vertical vent'
            call cfastexit('readinputfile',7)
            stop
        end if
        itop = ventptr%room1
        ibot = ventptr%room2
        deps1 = roominfo(itop)%z0 - roominfo(ibot)%z1
        if (itop/=n_rooms+1.and.ibot/=n_rooms+1.and.abs(deps1)>=mx_vsep) then
            write (*,*) '***Error: Vertical vent from ', itop,' to ', ibot, 'Separation between floor and ceiling too large.'
            write (iofill,*) '***Error: Vertical vent from ', itop,' to ', ibot, 'Separation between floor and ceiling too large.'
            call cfastexit('readinputfile',8)
            stop
        end if
    end do

    ! Compartment area and volume
    do i = 1, n_rooms
        roomptr => roominfo(i)
        roomptr%floor_area = roomptr%cwidth*roomptr%cdepth
        roomptr%cvolume = roomptr%floor_area*roomptr%cheight
    end do


    ! check room to room heat transfer parameters (VHEAT command)
    nswall2 = n_vcons
    ii = 0
    do i = 1, n_vcons
        iroom1 = vertical_connections(i,w_from_room)
        iroom2 = vertical_connections(i,w_to_room)

        ! room numbers must be between 1 and n_rooms. outside is n_rooms+1
        if (iroom1<1.or.iroom2<1.or.iroom1>n_rooms+1.or.iroom2>n_rooms+1) then
            write (*,'(a,i0,a,i0,a)')  '***Error: Invalid VHEAT specification:',' one or both of rooms ', &
                iroom1,'-',iroom2,' do not exist'
            write (iofill,'(a,i0,a,i0,a)')  '***Error: Invalid VHEAT specification:',' one or both of rooms ', &
                iroom1,'-',iroom2,' do not exist'
            call cfastexit('readinputfile',9)
            stop
        end if

        ! if room is connected to the outside then ignore it
        if (iroom1==n_rooms+1.or.iroom2==n_rooms+1) then
            nswall2 = nswall2 - 1
            cycle
        else
            ii = ii + 1
            if (i/=ii) then
                vertical_connections(ii,w_from_room) = vertical_connections(i,w_from_room)
                vertical_connections(ii,w_from_wall) = vertical_connections(i,w_from_wall)
                vertical_connections(ii,w_to_room) = vertical_connections(i,w_to_room)
                vertical_connections(ii,w_to_wall) = vertical_connections(i,w_to_wall)
            end if
        end if

        ! floor of one room must be adjacent to ceiling of the other
        dwall1 = abs(roominfo(iroom1)%z0 - roominfo(iroom2)%z1)
        dwall2 = abs(roominfo(iroom2)%z0 - roominfo(iroom1)%z1)
        if (dwall1<mx_vsep.or.dwall2<=mx_vsep) then
            if (dwall1<mx_vsep) then
                vertical_connections(ii,w_from_wall) = 2
                vertical_connections(ii,w_to_wall) = 1
            else
                vertical_connections(ii,w_from_wall) = 1
                vertical_connections(ii,w_to_wall) = 2
            end if
        else
            write (*,'(a,i0,a,i0,a)') '***Error: Invalid VHEAT specification: ceiling and floor of rooms', &
                iroom1,'-',iroom2,' are not connected'
            write (iofill,'(a,i0,a,i0,a)') '***Error: Invalid VHEAT specification: ceiling and floor of rooms', &
                iroom1,'-',iroom2,' are not connected'
                call cfastexit('readinputfile',10)
            stop
        end if

        ! walls must be turned on, ie surface_on must be set
        ! for the ceiling in the lower room and the floor of the upper room
        iwall1 = vertical_connections(ii,w_from_wall)
        iwall2 = vertical_connections(ii,w_to_wall)
        if (.not.roominfo(iroom1)%surface_on(iwall1).or..not.roominfo(iroom2)%surface_on(iwall2)) then
            if (.not.roominfo(iroom1)%surface_on(iwall1)) then
                write (*,'(a,i0,a,i0,a)') '***Error: Invalid VHEAT specification. Wall ',iwall1,' of room ', &
                    iroom1,' is adiabatic'
                write (iofill,'(a,i0,a,i0,a)') '***Error: Invalid VHEAT specification. Wall ',iwall1,' of room ', &
                    iroom1,' is adiabatic'
            else
                write (*,'(a,i0,a,i0,a)') '***Error: Invalid VHEAT specification. Wall ',iwall2,' of room ', &
                    iroom2,' is adiabatic'
                write (iofill,'(a,i0,a,i0,a)') '***Error: Invalid VHEAT specification. Wall ',iwall2,' of room ', &
                    iroom2,' is adiabatic'
            end if
            call cfastexit('readinputfile',11)
            stop
        end if
    end do
    n_vcons = nswall2

    ! check variable cross-sectional area specs and convert to volume
    do i = 1, n_rooms
        roomptr => roominfo(i)
        npts = roomptr%nvars
        if (npts/=0) then

            ! force first elevation to be at the floor; add a data point if necessary (same area as first entered data point)
            if (roomptr%var_height(1)/=0.0_eb) then
                temparea(1) = roomptr%var_area(1)
                temphgt(1) = 0.0_eb
                ioff = 1
            else
                ioff = 0
            end if

            ! copy data to temporary arrays
            do j = 1, npts
                temparea(j+ioff) = roomptr%var_area(j)
                temphgt(j+ioff) = roomptr%var_height(j)
            end do

            ! force last elevation to be at the ceiling (as defined by room_height(i)
            if (roomptr%cheight/=roomptr%var_height(npts)) then
                ioff2 = 1
                temparea(npts+ioff+ioff2) = roomptr%var_area(npts)
                temphgt(npts+ioff+ioff2) = roomptr%cheight
            else
                ioff2 = 0
            end if

            npts = npts + ioff + ioff2
            roomptr%nvars = npts

            ! copy temporary arrays to var_height and var_area; define volume by integrating areas
            roomptr%var_height(1) = 0.0_eb
            roomptr%var_volume(1) = 0.0_eb
            roomptr%var_area(1) = temparea(1)
            j = 1
            do j = 2, npts
                roomptr%var_height(j) = temphgt(j)
                roomptr%var_area(j) = temparea(j)
                dheight = roomptr%var_height(j) - roomptr%var_height(j-1)
                if (roomptr%var_area(j)/=roomptr%var_area(j-1)) then
                    ! if the area changes, we assume it's a pyramid
                    pyramid_height = dheight/(1.0_eb-sqrt(roomptr%var_area(j)/roomptr%var_area(j-1)))
                    roomptr%var_volume(j) = roomptr%var_volume(j-1) + &
                        (roomptr%var_area(j-1)*pyramid_height-roomptr%var_area(j)*(pyramid_height-dheight))/3.0_eb
                else
                    roomptr%var_volume(j) = roomptr%var_volume(j-1) + roomptr%var_area(j)*dheight
                end if
            end do

            ! re-define volume, area, breadth and depth arrays
            ! (room_volume, room_area, room_width and room_depth ) according to room area - height
            ! data read in.  room_height remains the same, room_volume is defined
            ! by integrating areas specified on the roomarea command,
            ! room_area is then room_volume/room_height, room_width and room_depth are defined so that
            ! room_width*room_depth=room_area and room_width/room_depth remain the same as entered on
            ! the width and depth  commands.

            roomptr%cvolume = roomptr%var_volume(npts)
            roomptr%floor_area = roomptr%cvolume/roomptr%cheight
            xx = roomptr%cwidth/roomptr%cdepth
            roomptr%cwidth = sqrt(roomptr%floor_area*xx)
            roomptr%cdepth = sqrt(roomptr%floor_area/xx)
        end if
    end do

    ! initialize variables that will change when ambient conditions change
    call initialize_ambient
    
    ! check targets to set flag for targets exposed to the exterior
    do i = 1, n_targets
        targptr => targetinfo(i)
        iroom = targptr%room
        if (iroom<1.or.iroom>n_rooms) then
            write (*,110) iroom
            write (iofill,110) iroom
110         format('***Error: Invalid TARGET specification. Room ',i3, ' is not a valid')
            call cfastexit('readinputfile',12)
            stop
        end if
        roomptr => roominfo(iroom)
        targptr%back = interior
        if (targptr%center(1)==0.0_eb.or.targptr%center(1)==roomptr%cwidth) targptr%back=exterior
        if (targptr%center(2)==0.0_eb.or.targptr%center(2)==roomptr%cdepth) targptr%back=exterior
        if (targptr%center(3)==0.0_eb.or.targptr%center(3)==roomptr%cheight) targptr%back=exterior
    end do

    ! check detectors
    do i = 1, n_detectors
        dtectptr => detectorinfo(i)
        iroom = dtectptr%room
        if (iroom<1.or.iroom>n_rooms) then
            write (*,104) iroom
            write (iofill,104) iroom
104         format('***Error: Invalid DETECTOR specification. Room ',i3, ' is not a valid')
            call cfastexit('readinputfile',13)
            stop
        end if
        roomptr => roominfo(iroom)

        rti = dtectptr%rti
        itype = dtectptr%dtype
        if (rti<=0.0_eb.and.itype/=smoked) then
            write (*,101) rti
            write (iofill,101) rti
101         format('***Error: Invalid DETECTOR specification. RTI = ',e11.4, ' is not a valid.')
            call cfastexit('readinputfile',14)
            stop
        end if

        xloc = dtectptr%center(1)
        yloc = dtectptr%center(2)
        zloc = dtectptr%center(3)
        if (xloc<0.0_eb.or.xloc>roomptr%cwidth.or.yloc<0.0_eb.or.yloc>roomptr%cdepth.or.zloc<0.0_eb.or.zloc>roomptr%cheight) then
            write (*,102) xloc,yloc,zloc
            write (iofill,102) xloc,yloc,zloc
102         format('***Error: Invalid DETECTOR specification. X,Y,Z,location =',3e11.4,' is out of bounds')
            call cfastexit('readinputfile',15)
            stop
        end if

        itype = dtectptr%dtype
        if (itype<1.or.itype>3) then
            write (*,103) itype
            write (iofill,103) itype
103         format('***Error: Invalid DETECTOR specification - type= ',i2,' is not a valid')
            call cfastexit('readinputfile',16)
            stop
        end if
    end do

    ! check room to room heat transfer

    !iheat may have one of three values, 0, 1, 2.
    ! 0 = no room to room heat transfer
    ! 1 = fractions are determined by what rooms are connected by vents
    ! For example, if room 1 is connected to rooms 2, 3, 4 and the outside
    ! by vents then the first row of heat_frac will have the values
    ! 0. .25 .25 .25 .25

    do i = 1, n_rooms
        roomptr => roominfo(i)
        ! force heat transfer between rooms connected by vents.
        if (roomptr%iheat==1) then
            do j = 1, n_hvents
                ventptr => hventinfo(j)
                if (ventptr%room2==j) then
                    roomptr2 => roominfo(j)
                    ! if the back wall of to room is not active then don't consider its contribution
                    if (j<n_rooms+1.and.roomptr2%surface_on(3)) roomptr%heat_frac(j) = 1.0_eb
                end if
            end do
        end if

        ! normalize heat_frac fraction matrix so that rows sum to one
        if (roomptr%iheat/=0) then
            sum = 0.0_eb
            do j = 1, n_rooms+1
                sum = sum + roomptr%heat_frac(j)
            end do
            if (sum<1.e-5_eb) then
                roomptr%heat_frac(1:n_rooms) = 0.0_eb
                roomptr%heat_frac(n_rooms+1) = 1.0_eb
            else
                roomptr%heat_frac(1:n_rooms+1) = roomptr%heat_frac(1:n_rooms+1)/sum
            end if
            roomptr%nheats = 0
            do j = 1, n_rooms
                if (roomptr%heat_frac(j)/=0.0_eb) then
                    roomptr%nheats = roomptr%nheats + 1
                    roomptr%hheat_connections(roomptr%nheats) = j
                end if
            end do
        end if
    end do

    ! set up any specified slice or iso files
    call setup_slice_iso

    close (iofili)
    return

5022 format ('***Error: Initial temperature outside of allowable range (-50 to +100)',f5.2)

    ! read format list
5050 format ('***Error: Error opening the input file = ',I6)

    end subroutine read_input_file

    ! --------------------------- open_files -------------------------------------------

!> \brief   get the paths and project base name open the input file for reading, delete the output files, and open the log file

    subroutine open_files ()
    
    use namelist_data

    logical ex
    integer :: lp, ld, le, ios
    character(len=256) :: revision, revision_date, compile_date, buf
    
    ! get the input file parts
    call exehandle (exepath, datapath, project, extension)
    
    ! form the file names for datafiles
    lp = len_trim (datapath)
    ld = len_trim (project)
    le = len_trim (extension)
    inputfile = datapath(1:lp) // project(1:ld) // extension(1:le)
    outputfile = datapath(1:lp) // project(1:ld) // '.out'
    smvhead = datapath(1:lp) // project(1:ld) // '.smv'
    smvdata = datapath(1:lp) // project(1:ld) // '.plt'
    smvcsv = datapath(1:lp) // project(1:ld) // '_zone.csv'
    smvsinfo = datapath(1:lp) // project(1:ld) // '.sinfo'
    
    ! spreadsheet output files
    sscompartment = datapath(1:lp) // project(1:ld) // '_compartments.csv'
    ssdevice = datapath(1:lp) // project(1:ld) // '_devices.csv'
    sswall = datapath(1:lp) // project(1:ld) // '_walls.csv'
    ssmasses = datapath(1:lp) // project(1:ld) // '_masses.csv'
    ssvent = datapath(1:lp) // project(1:ld) // '_vents.csv'
    
    ssdiag = datapath(1:lp) // project(1:ld) // '_diagnostics.csv'
    gitfile = datapath(1:lp) // project(1:ld) // '_git.txt'
    errorlogging = datapath(1:lp) // project(1:ld) // '.log'
    stopfile = datapath(1:lp) // project(1:ld) // '.stop'
    residfile = datapath(1:lp) // project(1:ld) // '.debug'
    residcsv = datapath(1:lp) // project(1:ld) // '_resid.csv'
    queryfile = datapath(1:lp) // project(1:ld) // '.query'
    statusfile = datapath(1:lp) // project(1:ld) // '.status'
    slabcsv = datapath(1:lp) // project(1:ld) // '_slab.csv'
    sscalculation = datapath(1:lp) // project(1:ld) // '_calculations.csv'

    !open input file and check to see if it's a new (namelist) format file
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)
    read (unit=iofili,fmt='(a)') buf
    rewind (unit=iofili)
    if (buf(1:5)==heading) then
        nmlflag = .false.
    else if (buf(1:1)=='&') then
        nmlflag = .true.
        rewind (unit=iofili)
        call read_misc (iofili)
        if (.not.overwrite_testcase) then
            inquire (file=outputfile,exist=ex)
            if (ex) then
                write (*,'(a,a,a)') '***Error in &MISC, OVERWRITE=.FALSE. and the file ',trim(outputfile),' exists.'
                call cfastexit('openfiles',1)
                stop
            end if
        end if
    else
        write (*,*) ' Input file format not recognized. Check first line of input file.'
        call cfastexit('openfiles',2)
        stop
    end if

    ! output the revision for later identification of validation plots
    if (validation_flag) then
        call delete_output_files (gitfile)
        open (newunit=iofilg, file=gitfile, action='write', iostat=ios, status='new')
        if (ios==0) then
            call get_info(revision, revision_date, compile_date)
            write (iofilg,'(a)') revision
            close (unit=iofilg)
        end if
    end if

    ! open the log file to write error messages and such
    call delete_output_files (errorlogging)
    open (newunit=iofill, file=errorlogging, action='write', iostat=ios, status='new')
    if (ios/=0) then
        write (*,'(a,i0,a)') '***Error opening log file, returned status = ', ios, &
            '. Log file may be in use by another application.'
        call cfastexit('openfiles',3)
        stop
    end if

    call delete_output_files (outputfile)
    call delete_output_files (smvhead)
    call delete_output_files (smvdata)
    call delete_output_files (smvcsv)
    call delete_output_files (smvsinfo)
    call delete_output_files (sscompartment)
    call delete_output_files (ssdevice)
    call delete_output_files (sswall)
    call delete_output_files (ssmasses)
    call delete_output_files (ssvent)
    call delete_output_files (statusfile)
    call delete_output_files (queryfile)
    call delete_output_files (residcsv)
    call delete_output_files (slabcsv)

    return

    end subroutine open_files

    ! --------------------------- exehandle -------------------------------------------)

!> \brief   get the arguments used to call the main program

!> \param   exepath (output): path (without the name) to the folder where the executable resides
!> \param   datapath (output): path (without a file name) to the folder where the project data file resides
!> \param   project  (output): name of the project - this name cannot exceed 64 charcters. the total length
!>          of datapath + project cannot exceed 256 characters
!> \param   extension (output): file extension of input file
    
    subroutine exehandle (exepath, datapath, project, extension)

    character(len=*), intent(out) :: exepath, datapath, project, extension

    integer :: i, loop, status, nargs, ld(2), li(2), ln(2), le(2), idx(2)
    character(len=256) :: buf, xname
    character (len=64) :: name(2)
    character(len=3) :: drive(2)
    character(len=256) :: dir(2)
    character(len=64) :: ext(2)
    integer(kind=4) :: length, pathcount, splitpathqq, ilen
    logical :: DoesTheFileExist

    nargs = command_argument_count() + 1

    if (nargs<cfast_input_file_position) then
        if (cfast_input_file_position == 2) then
            write (*,*) 'CFAST was called with no arguments on the command line.  At least an input file is required.'
            call cfastexit('exehandle',1)
        else
            write(*,*) 'CData was called with insufficent arguments on the command line.', &
                'Input file should be in position ', cfast_input_file_position
            call cfastexit('exehandle', 2)
        end if 
    end if

    ! get the calling program and arguments

    exepath = ' '
    datapath = ' '
    project = ' '
    extension = ' '
    idx(1) = 1
    idx(2) = cfast_input_file_position

    ! only look at the first two arguments (1 = executable name, 2 =cfast input file name)
    do i = 1, 2
        loop = idx(i) - 1
        call get_command_argument(loop, buf, ilen, status)
        if (ilen>0) then
            xname = buf

            !	Split out the components
            drive(i) = ' '
            dir(i) = ' '
            name(i) = ' '
            ext(i) = ' '
            length = splitpathqq(xname, drive(i), dir(i), name(i), ext(i))
            ld(i) = len_trim(drive(i))
            li(i) = len_trim(dir(i))
            ln(i) = len_trim(name(i))
            le(i) = len_trim(ext(i))

            pathcount = 5 + ln(i) + li(i) +ld(i) + le(i)

            if (pathcount>255.or.ln(i)>64) then
                write (*,'(a,/,a)') 'Total file name length including path must be less than 256 characters.', &
                    'Individual filenames must be less than 64 characters.'
                call cfastexit('exehandle',3)
            end if
        end if
    end do

    ! Now check that the cfast input file exists = this is the data file
    buf = ' '
    if (le(2)/=0) then
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // ext(2)(1:le(2))
    else
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // '.in'
    end if

    inquire (file=buf(1:len_trim(buf)), exist=doesthefileexist)
    if (doesthefileexist) then
        !	The project file exists
        exepath = drive(1)(1:ld(1)) // dir(1)(1:li(1))
        datapath = drive(2)(1:ld(2)) // dir(2)(1:li(2))
        project = name(2)(1:ln(2))
        if (le(2)/=0) then
            extension = ext(2)(1:le(2))
        else
            extension = '.in'
        end if
    else
        write (*,*) ' Input file does not exist: ', trim(buf)
        call cfastexit('exehandle',4)
        stop
    end if
    
    return

    end subroutine exehandle

    ! --------------------------- setup_slice_iso -------------------------------------------
    
!> \brief   allocate and initialize slice file and iso surface variables after reading input file

    subroutine setup_slice_iso

    integer :: nrm

    integer :: i,j,k,iroom,islice
    type(slice_type), pointer :: sliceptr
    type(room_type), pointer :: roomptr
    real(eb) :: xb(6)
    character(len=256) :: slicefilename
    integer :: ijkslice(6)
    real(eb), parameter :: dxyz=0.01_eb
    character(len=60) :: menu_label, colorbar_label, unit_label
    integer :: i_iso
    type(iso_type), pointer :: isoptr
    character(len=256) :: isofilename
    real(eb) :: ceiljet_depth
    type(visual_type), pointer :: vptr
    integer :: ntypes, ir, ibeg, iend
    real(eb) :: position_offset
    integer skipslice


    n_slice = 0
    n_iso = 0
    if (n_visual.eq.0)return

    nrm = n_rooms

    ! count number of isosurfaces and slices

    do i = 1, n_visual
        vptr=>visualinfo(i)
        if (vptr%roomnum.eq.0) then
            ntypes=nrm
        else
            ntypes=1
        end if
        if (vptr%vtype.eq.1.or.vptr%vtype.eq.2) then
            n_slice = n_slice + ntypes
        else
            n_iso = n_iso + ntypes
        end if
    end do
    n_slice = 5*n_slice
    allocate(sliceinfo(n_slice))
    allocate(isoinfo(n_iso))

    ! setup grid locations for each compartment

    do iroom = 1, nrm
        roomptr=>roominfo(iroom)
        roomptr%ibar = min(max(2,int(roomptr%cwidth/dxyz)),roomptr%ibar)

        ceiljet_depth = 0.2_eb * roomptr%z1 ! placeholder now, change to a calculation

        roomptr%ibar = min(max(2,int(roomptr%cwidth/dxyz)),roomptr%ibar)
        allocate(roomptr%xplt(0:roomptr%ibar))
        allocate(roomptr%xpltf(0:roomptr%ibar))
        call set_grid(roomptr%xplt,roomptr%ibar+1,roomptr%x0,roomptr%x1,roomptr%x1,0)
        do i = 0, roomptr%ibar
            roomptr%xpltf(i) = real(roomptr%xplt(i),fb)
        end do

        roomptr%jbar = min(max(2,int(roomptr%cdepth/dxyz)),roomptr%jbar)
        allocate(roomptr%yplt(0:roomptr%jbar))
        allocate(roomptr%ypltf(0:roomptr%jbar))
        call set_grid(roomptr%yplt,roomptr%jbar+1,roomptr%y0,roomptr%y1,roomptr%y1,0)
        do j = 0, roomptr%jbar
            roomptr%ypltf(j) = real(roomptr%yplt(j),fb)
        end do

        roomptr%kbar = min(max(2,int(roomptr%cheight/dxyz)),roomptr%kbar)
        allocate(roomptr%zplt(0:roomptr%kbar))
        allocate(roomptr%zpltf(0:roomptr%kbar))
        call set_grid(roomptr%zplt,roomptr%kbar+1,roomptr%z0,roomptr%z1-ceiljet_depth,roomptr%z1,roomptr%kbar/3)
        do k = 0, roomptr%kbar
            roomptr%zpltf(k) = real(roomptr%zplt(k),fb)
        end do
    end do

    ! setup slice file data structures

    islice = 1

    do i = 1, n_visual
        vptr=>visualinfo(i)
        if (vptr%vtype.eq.3)cycle
        ir = vptr%roomnum
        if (ir.eq.0) then
            ibeg=1
            iend=nrm
        else
            ibeg=ir
            iend=ir
        end if
        do iroom=ibeg,iend
            roomptr=>roominfo(iroom)
            xb(1) = roomptr%x0
            xb(2) = roomptr%x1
            xb(3) = roomptr%y0
            xb(4) = roomptr%y1
            xb(5) = roomptr%z0
            xb(6) = roomptr%z1
            ijkslice(1) = 0
            ijkslice(2) = roomptr%ibar
            ijkslice(3) = 0
            ijkslice(4) = roomptr%jbar
            ijkslice(5) = 0
            ijkslice(6) = roomptr%kbar
            skipslice=0
            if (vptr%vtype.eq.1) then
                position_offset = 0.0_eb
                if (vptr%axis.eq.1) then
                    if (ir/=0) position_offset = roomptr%x0
                    xb(1) = vptr%position + position_offset
                    xb(2) = vptr%position + position_offset
                    ijkslice(1) = get_igrid(xb(1),roomptr%xplt,roomptr%ibar)
                    if (ijkslice(1)<0)skipslice=1
                    ijkslice(2) = ijkslice(1)
                else if (vptr%axis.eq.2) then
                    if (ir/=0) position_offset = roomptr%y0
                    xb(3) = vptr%position + position_offset
                    xb(4) = vptr%position + position_offset
                    ijkslice(3) = get_igrid(xb(3),roomptr%yplt,roomptr%jbar)
                    if (ijkslice(3)<0)skipslice=1
                    ijkslice(4) = ijkslice(3)
                else if (vptr%axis.eq.3) then
                    if (ir/=0) position_offset = roomptr%z0
                    xb(5) = vptr%position + position_offset
                    xb(6) = vptr%position + position_offset
                    ijkslice(5) = get_igrid(xb(5),roomptr%zplt,roomptr%kbar)
                    if (ijkslice(5)<0)skipslice=1
                    ijkslice(6) = ijkslice(5)
                end if
            end if
            do j = 1, 5
                sliceptr => sliceinfo(islice)

                sliceptr%skip=skipslice
                write (slicefilename,'(A,A,I4.4,A)') trim(project),'_',islice,'.sf'
                if (j.eq.1) then
                    menu_label="Temperature"
                    colorbar_label="TEMP"
                    unit_label="C"
                else if (j.eq.2) then
                    menu_label="U-VELOCITY"
                    colorbar_label="U-VEL"
                    unit_label="m/s"
                else if (j.eq.3) then
                    menu_label="V-VELOCITY"
                    colorbar_label="V-VEL"
                    unit_label="m/s"
                else if (j.eq.4) then
                    menu_label="W-VELOCITY"
                    colorbar_label="W-VEL"
                    unit_label="m/s"
                else
                    menu_label="Speed"
                    colorbar_label="Speed"
                    unit_label="m/s"
                end if

                sliceptr%filename = trim(slicefilename)
                sliceptr%roomnum = iroom
                sliceptr%menu_label = trim(menu_label)
                sliceptr%colorbar_label = trim(colorbar_label)
                sliceptr%unit_label = trim(unit_label)
                sliceptr%xb = xb
                sliceptr%ijk = ijkslice
                islice = islice + 1
            end do
        end do
    end do

    ! setup isosurface data structures

    i_iso = 1
    do i = 1, n_visual
        vptr=>visualinfo(i)
        if (vptr%vtype.ne.3)cycle
        ir = vptr%roomnum
        if (ir.eq.0) then
            ibeg=1
            iend=nrm
        else
            ibeg=ir
            iend=ir
        end if
        do iroom=ibeg,iend
            roomptr=>roominfo(iroom)
            isoptr => isoinfo(i_iso)

            write (isofilename,'(A,A,I4.4,A)') trim(project),'_',i_iso,'.iso'
            menu_label="Temperature"
            colorbar_label="TEMP"
            unit_label="C"

            isoptr%filename = trim(isofilename)
            isoptr%roomnum = iroom
            isoptr%value = vptr%value
            isoptr%menu_label = trim(menu_label)
            isoptr%colorbar_label = trim(colorbar_label)
            isoptr%unit_label = trim(unit_label)
            i_iso = i_iso + 1
        end do
    end do

    end subroutine setup_slice_iso

    ! ------------------ get_igrid ------------------------

    integer function get_igrid (x,xgrid,n)

    integer, intent(in) :: n
    real(eb), intent(in), dimension(0:n) :: xgrid
    real(eb), intent(in) :: x

    integer :: i

    do i = 0, n-1
        if (xgrid(i).le.x.and.x.lt.xgrid(i+1)) then
            get_igrid=i
            return
        end if
    end do
    if (xgrid(n).eq.x) then
        get_igrid=n
    else
        get_igrid=-1
    end if
    return
    end function get_igrid

    ! --------------------------- set_grid -------------------------------------------

    subroutine set_grid (xgrid,n,xmin,xsplit,xmax,nsplit)

    integer, intent(in) :: n, nsplit
    real(eb), dimension(n), intent(out) :: xgrid
    real(eb), intent(in) :: xmin, xsplit, xmax

    real(eb) :: factor
    integer :: i

    !   1            n-nsplit          n
    !  xmin          xsplit          xmax

    do i = 1, n-nsplit
        factor = real(i-1,eb)/real(n-nsplit-1,eb)
        xgrid(i) = emix(factor,xmin,xsplit)
    end do

    do i = n-nsplit+1, n
        factor = real(i-(n-nsplit),eb)/real(nsplit,eb)
        xgrid(i) = emix(factor,xsplit,xmax)
    end do

    end subroutine set_grid

    end module input_routines