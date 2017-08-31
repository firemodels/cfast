    module input_routines

    use precision_parameters

    use fire_routines, only: flame_height
    use initialization_routines, only : inittarg, initamb, offset
    use numerics_routines, only : dnrm2
    use output_routines, only: openoutputfiles, deleteoutputfiles
    use utility_routines, only: countargs, get_igrid, upperall, exehandle, emix
    use namelist_routines
    use conversion_routines

    use wallptrs
    use cenviro
    use ramp_data
    use cparams, only: mx_hsep
    use setup_data
    use debug_data
    use detectorptrs
    use target_data
    use fire_data
    use option_data
    use solver_data
    use smkview_data
    use thermal_data
    use vent_data
    use room_data
    use namelist_data

    implicit none

    logical :: exset = .false.

    private

    public read_input_file, open_files, read_solver_ini

    contains

    ! --------------------------- read_input_file -------------------------------------------

    subroutine read_input_file ()

    !	Read the input file and set up the data for processing

    implicit none

    real(eb) :: yinter(mxrooms), temparea(mxcross), temphgt(mxcross), deps1, dwall1, dwall2, rti
    real(eb) :: xloc, yloc, zloc, zbot, ztop, pyramid_height, dheight, xx, sum
    integer :: numr, numc, ios, iversion, i, ii, j, itop, ibot, nswall2, iroom, iroom1, iroom2
    integer :: iwall1, iwall2, itype, npts, ioff, ioff2, ivers
    character :: aversion*5
    type(room_type), pointer :: roomptr, roomptr2
    type(fire_type), pointer :: fireptr
    type(detector_type), pointer :: dtectptr
    type(vent_type), pointer :: ventptr
    
    
    !	Unit numbers defined in read_command_options, openoutputfiles, readinputfiles
    !
    !      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
    !      3 is for the log file  (iofill)
    !      6 is output (IOFILO)
    !     11 is the history file
    !     12 is used to write the status file (project.status)
    !     13 smokeview output (header) - note this is rewound each time the plot data is written)
    !     14 smokeview output (plot data)
    !     15 spreadsheet output (normal)
    !     16 spreadsheet output (flow field)
    !     17 spreadsheet output (species)
    !     18 spreadsheet output (walls and targets)
    !
    !     surface_on (1) = ceiling properties are defined
    !                (2) = floor properties are defined
    !                (3) = side wall properties are defined for upper walls
    !                (4) = side wall properties are defined for lower walls

    ! deal with opening the data file and assuring ourselves that it is compatible
    close (iofili)
    open (unit=iofili,file=inputfile,status='OLD',iostat=ios)
    if (ios/=0) then
        if (iofill>0) then
            write (*,5050) mod(ios,256)
            write (iofill,5050) mod(ios,256)
        else
            write (*,5050) mod(ios,256)
        end if
        stop
    end if

    if (.not. nmlflag) then
        ! read in the entire input file as a spreadsheet array of numbers and/or character strings
        call readcsvformat (iofili, rarray, carray, nrow, ncol, 1, numr, numc, iofill)
    
        close (iofili)
    
        ! aversion is the header name, ivers is the major version number read in, iversion is the major version number
        ! from the internal version data. these need to be compatible
        aversion = carray(1,1)
        ivers = rarray(1,2)
        ! new version numbering 600->6000, so current version is 7000
        if (version>=1000) then
            iversion = version/1000
        else
            iversion = version/100
        end if
    
        if (aversion==heading.and.ivers==iversion-1) then
            write (*,5004) ivers, iversion
            write (iofill,5004) ivers, iversion
        else if (aversion/=heading.or.ivers/=iversion) then
            write (*,5002) aversion,heading,ivers,iversion
            write (iofill,5002) aversion,heading,ivers,iversion
            stop
        end if
        title = carray(1,3)
        versnflag=.true.
    end if

    do i = 1, mxrooms
        yinter(i) = -1.0_eb
    end do

    n_thrmp = 0

    if (nmlflag) then
        call namelist_input
    else
        ! read in data file
        call keywordcases (numr, numc)
        call namelist_conversion(aversion,ivers)
    end if

    !	wait until the input file is parsed before we die on temperature outside reasonable limits
    if (exterior_temperature>373.15_eb.or.exterior_temperature<223.15_eb) then
        write (*,5022) exterior_temperature
        write (iofill,5022) exterior_temperature
        stop
    end if
    if (interior_temperature>373.15_eb.or.interior_temperature<223.15_eb) then
        write (*,5022) interior_temperature
        write (iofill,5022) interior_temperature
        stop
    end if

    ! Compartment geometry related data
    nrm1 = nr - 1
    do i = 1, nrm1
        roomptr => roominfo(i)
        roomptr%x1 = roomptr%x0 + roomptr%cwidth
        roomptr%y1 = roomptr%y0 + roomptr%cdepth
        roomptr%z1 = roomptr%z0 + roomptr%cheight
    end do

    ! We now know what output is going to be generated, so create the files
    call openoutputfiles

    interior_rho = interior_abs_pressure/interior_temperature/rgas
    exterior_rho = exterior_abs_pressure/exterior_temperature/rgas

    ! initialize the targets.
    call inittarg

    ! now calculate the offsets - the order is important
    call offset

    ! check fire objects
    do i = 1, n_fires
        fireptr => fireinfo(i)
        roomptr => roominfo(fireptr%room)
        if ((fireptr%x_position<0.0_eb).or.(fireptr%x_position>roomptr%cwidth)) fireptr%x_position = roomptr%cwidth/2.0_eb
        if ((fireptr%y_position<0.0_eb).or.(fireptr%y_position>roomptr%cdepth)) fireptr%y_position = roomptr%cdepth/2.0_eb
        if ((fireptr%z_position<0.0_eb).or.(fireptr%z_position>roomptr%cheight)) fireptr%z_position = 0.0_eb
    end do
    if (lower_o2_limit<0.0_eb.or.lower_o2_limit>0.230_eb) then
        write (*,*) '***Error: Specified LOI is less than zero or greater than ambient. Default of 0..15 used'
        write (iofill,*) '***Error: Specified LOI is less than zero or greater than ambient. Default of 0..15 used'
        lower_o2_limit = 0.15_eb
    end if
    
    !make sure wall vent specifications are correct
    do i = 1,n_hvents
        ventptr => hventinfo(i)
        roomptr => roominfo(ventptr%room1)
        zbot = ventptr%sill
        ztop = ventptr%soffit
        if (zbot<0.0_eb.or.zbot>roomptr%cheight.or.ztop<0.0_eb.or.ztop>roomptr%cheight.or.ztop<zbot) then
            write (*,203) zbot, ztop
            write (iofill,203) zbot, ztop
203         format('***Error: Invalid HVENT specification. sill and/or soffit height =',2e11.4,' out of bounds')
            stop
        end if
    end do

    ! make sure ceiling/floor vent specifications are correct -  we have to do this
    ! here rather than right after keywordcases because floor_height and ceiling_height were just defined
    ! above
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        if (ventptr%room1==ventptr%room2) then
            write (*,*) '***Error: A room can not be connected to itself with a vertical vent'
            write (iofill,*) '***Error: A room can not be connected to itself with a vertical vent'
            stop
        end if
        itop = ventptr%room1
        ibot = ventptr%room2
        deps1 = roominfo(itop)%z0 - roominfo(ibot)%z1
        if (itop/=nr.and.ibot/=nr.and.abs(deps1)>=mx_vsep) then
            write (*,*) '***Error: Vertical vent from ', itop,' to ', ibot, 'Separation between floor and ceiling too large.'
            write (iofill,*) '***Error: Vertical vent from ', itop,' to ', ibot, 'Separation between floor and ceiling too large.'
            stop
        end if
    end do

    ! Compartment area and volume
    do i = 1, nrm1
        roomptr => roominfo(i)
        roomptr%floor_area = roomptr%cwidth*roomptr%cdepth
        roomptr%cvolume = roomptr%floor_area*roomptr%cheight
    end do


    ! check room to room heat transfer parameters (VHEAT command)
    nswall2 = nvcons
    ii = 0
    do i = 1, nvcons
        iroom1 = i_vconnections(i,w_from_room)
        iroom2 = i_vconnections(i,w_to_room)

        ! room numbers must be between 1 and nrm1
        if (iroom1<1.or.iroom2<1.or.iroom1>nrm1+1.or.iroom2>nrm1+1) then
            write (*,201) iroom1, iroom2
            write (iofill,201) iroom1, iroom2
201         format('***Error: Invalid VHEAT specification:',' one or both of rooms ',i0,'-',i0,' do not exist')
            stop
        end if

        ! if room is connected to the outside then ignore it
        if (iroom1==nrm1+1.or.iroom2==nrm1+1) then
            nswall2 = nswall2 - 1
            cycle
        else
            ii = ii + 1
            if (i/=ii) then
                i_vconnections(ii,w_from_room) = i_vconnections(i,w_from_room)
                i_vconnections(ii,w_from_wall) = i_vconnections(i,w_from_wall)
                i_vconnections(ii,w_to_room) = i_vconnections(i,w_to_room)
                i_vconnections(ii,w_to_wall) = i_vconnections(i,w_to_wall)
            end if
        end if

        ! floor of one room must be adjacent to ceiling of the other
        dwall1 = abs(roominfo(iroom1)%z0 - roominfo(iroom2)%z1)
        dwall2 = abs(roominfo(iroom2)%z0 - roominfo(iroom1)%z1)
        if (dwall1<mx_vsep.or.dwall2<=mx_vsep) then
            if (dwall1<mx_vsep) then
                i_vconnections(ii,w_from_wall) = 2
                i_vconnections(ii,w_to_wall) = 1
            else
                i_vconnections(ii,w_from_wall) = 1
                i_vconnections(ii,w_to_wall) = 2
            end if
        else
            write (*,202) iroom1, iroom2
            write (iofill,202) iroom1, iroom2
202         format('***Error: Invalid VHEAT specification:'' ceiling and floor of rooms',i0,'-',i0,' are not connectetd')
            stop
        end if

        ! walls must be turned on, ie surface_on must be set
        ! for the ceiling in the lower room and the floor of the upper room
        iwall1 = i_vconnections(ii,w_from_wall)
        iwall2 = i_vconnections(ii,w_to_wall)
        if (.not.roominfo(iroom1)%surface_on(iwall1).or..not.roominfo(iroom2)%surface_on(iwall2)) then
            if (.not.roominfo(iroom1)%surface_on(iwall1)) then
                write (*,204) iwall1, iroom1
                write (iofill,204) iwall1, iroom1
204             format('***Error: Invalid VHEAT specification. Wall ',i0,' of room ',i0,' is adiabatic')
            else
                write (*,204) iwall2, iroom2
                write (iofill,204) iwall2, iroom2
            end if
            stop
        end if
    end do
    nvcons = nswall2

    ! check shafts
    do iroom = nrm1 + 1, mxrooms
        roomptr => roominfo(iroom)
        if (roomptr%shaft) then
            write (*,'(a,i0,a,i0)') &
                '***Error: Invalid SHAFT specification. Room',iroom,'must be less than or equal to ',nrm1
            write (iofill,'(a,i0,a,i0)') &
                '***Error: Invalid SHAFT specification. Room',iroom,'must be less than or equal to ',nrm1
            stop
        end if
    end do

    ! check variable cross-sectional area specs and convert to volume
    do i = 1, nrm1
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
    call initamb(yinter,1)

    ! check detectors
    do i = 1, n_detectors
        dtectptr => detectorinfo(i)
        iroom = dtectptr%room
        if (iroom<1.or.iroom>nrm1) then
            write (*,104) iroom
            write (iofill,104) iroom
104         format('***Error: Invalid DETECTOR specification. Room ',i3, ' is not a valid')
            stop
        end if
        roomptr => roominfo(iroom)

        rti = dtectptr%rti
        itype = dtectptr%dtype
        if (rti<=0.0_eb.and.itype/=smoked) then
            write (*,101) rti
            write (iofill,101) rti
101         format('***Error: Invalid DETECTOR specification. RTI = ',e11.4, ' is not a valid.')
            stop
        end if

        xloc = dtectptr%center(1)
        yloc = dtectptr%center(2)
        zloc = dtectptr%center(3)
        if (xloc<0.0_eb.or.xloc>roomptr%cwidth.or.yloc<0.0_eb.or.yloc>roomptr%cdepth.or.zloc<0.0_eb.or.zloc>roomptr%cheight) then
            write (*,102) xloc,yloc,zloc
            write (iofill,102) xloc,yloc,zloc
102         format('***Error: Invalid DETECTOR specification. X,Y,Z,location =',3e11.4,' is out of bounds')
            stop
        end if

        itype = dtectptr%dtype
        if (itype<1.or.itype>3) then
            write (*,103) itype
            write (iofill,103) itype
103         format('***Error: Invalid DETECTOR specification - type= ',i2,' is not a valid')
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

    do i = 1, nrm1
        roomptr => roominfo(i)
        ! force heat transfer between rooms connected by vents.
        if (roomptr%iheat==1) then
            do j = 1, n_hvents
                ventptr => hventinfo(j)
                if (ventptr%room2==j) then
                    roomptr2 => roominfo(j)
                    ! if the back wall of to room is not active then don't consider its contribution
                    if (j<nr.and.roomptr2%surface_on(3)) roomptr%heat_frac(j) = 1.0_eb
                end if
            end do
        end if

        ! normalize heat_frac fraction matrix so that rows sum to one
        if (roomptr%iheat/=0) then
            sum = 0.0_eb
            do j = 1, nrm1+1
                sum = sum + roomptr%heat_frac(j)
            end do
            if (sum<1.e-5_eb) then
                roomptr%heat_frac(1:nrm1) = 0.0_eb
                roomptr%heat_frac(nr) = 1.0_eb
            else
                roomptr%heat_frac(1:nr) = roomptr%heat_frac(1:nr)/sum
            end if
            roomptr%nheats = 0
            do j = 1, nrm1
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

5002 format ('***Error: Not a compatible version ',2a8,2x,2i10)
5004 format ('Opening a version ',i2,' file with version ',i2,'. Fire inputs may need to be updated.')
5022 format ('***Error: Initial temperature outside of allowable range (-50 to +100)',f5.2)

    ! read format list
5050 format ('***Error: Error opening the input file = ',I6)

    end subroutine read_input_file

    ! --------------------------- keywordcases -------------------------------------------

    subroutine keywordcases(inumr,inumc)

    !     routine:  keywordcases (remaned from NPUTQ)
    !     purpose: Handles CFAST datafile keywords
    !     Arguments: inumr    number of rows in input file spreadsheet
    !                inumc    number of columns in input file spreadsheet

    integer, parameter :: maxin = 37

    integer, intent(in) :: inumr, inumc

    integer :: i1, i2, fannumber, i, j, k, ir, icarea, icshape, icfraction
    integer :: iijk, jmax, npts, nto, ifrom, ito, imin, iroom, iramp, ncomp
    real(eb) :: initialopening, lrarray(ncol)
    real(eb) :: frac, tmpcond
    character :: label*5, tcname*64, eqtype*3, venttype
    character(128) :: lcarray(ncol)
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(detector_type), pointer :: dtectptr
    type(ramp_type), pointer :: rampptr
    type(visual_type), pointer :: sliceptr
    type(thermal_type), pointer :: thrmpptr
    type(fire_type), pointer :: fireptr
    type(vent_type), pointer :: ventptr

    !	Start with a clean slate

    do i = 1, mxrooms
        roomptr => roominfo(i)
        do j = 1, 4
            roomptr%matl(j) = 'OFF'
            roomptr%surface_on(j) = .false.
        end do
    end do
    ncomp = 0

    ! First check for a maximum time step. This may be modified by fires, vents, or detectors
    do ir = 2, inumr
        label = carray(ir,1)
        if (label==' ') cycle
        lrarray = 0.0_eb
        lcarray = ' '
        do i = 2, inumc
            lcarray(i-1) = carray(ir,i)
            lrarray(i-1) = rarray(ir,i)
        end do
        if (label=='STPMA') then
            if (countargs(lcarray)>=1) then
                stpmax = lrarray(1)
            else
                write (*,*) '***Error: Bad STPMA input. At least 1 argument required.'
                write (iofill,*) '***Error: Bad STPMA input. At least 1 argument required.'
                stop
            end if
            stpmaflag=.true.
        end if
    end do

    ! Then do thermal properties
    do ir = 2, inumr
        label = carray(ir,1)
        if (label==' ') cycle
        lrarray = 0.0_eb
        lcarray = ' '
        do i = 2, inumc
            lcarray(i-1) = carray(ir,i)
            lrarray(i-1) = rarray(ir,i)
        end do

        if (label=='MATL') then
            if (countargs(lcarray)>=7) then
                n_thrmp = n_thrmp + 1
                if (n_thrmp>mxthrmp) then
                    write (*,'(a,i3)') '***Error: Bad MATL input. Too many thermal properties in input data file. Limit is ', &
                        mxthrmp
                    write (iofill,'(a,i3)') '***Error: Bad MATL input. Too many thermal properties in input data file. Limit is ', &
                        mxthrmp
                    stop
                end if
                thrmpptr => thermalinfo(n_thrmp)
                thrmpptr%name = lcarray(1)
                thrmpptr%nslab = 1
                thrmpptr%k(1) = lrarray(2)
                thrmpptr%c(1) = lrarray(3)
                thrmpptr%rho(1) = lrarray(4)
                thrmpptr%thickness(1) = lrarray(5)
                thrmpptr%eps = lrarray(6)
            else
                write (*,*) '***Error: Bad MATL input. At least 7 arguments required.'
                write (iofill,*) '***Error: Bad MATL input. At least 7 arguments required.'
                stop
            end if
            matrlflag=.true.
        end if
    end do

    ! Then do compartments
    do ir = 2, inumr
        label = carray(ir,1)
        if (label==' ') cycle
        lrarray = 0.0_eb
        lcarray = ' '
        do i = 2, inumc
            lcarray(i-1) = carray(ir,i)
            lrarray(i-1) = rarray(ir,i)
        end do

        ! COMPA	name(c), width(f), depth(f), height(f), absolute position (f) (3), ceiling_material(c),
        ! floor_material(c), wall_material (c)
        if (label=='COMPA') then
            if (countargs(lcarray)>=10) then

                ncomp = ncomp + 1
                if (ncomp>mxrooms) then
                    write (*, 5062) ncomp
                    write (iofill, 5062) ncomp
                    stop
                end if

                roomptr => roominfo(ncomp)
                ! Name
                roomptr%name = lcarray(1)

                ! Size
                roomptr%cwidth = lrarray(2)
                roomptr%cdepth = lrarray(3)
                roomptr%cheight = lrarray(4)
                roomptr%x0 = lrarray(5)
                roomptr%y0 = lrarray(6)
                roomptr%z0 = lrarray(7)

                ! Ceiling
                tcname = lcarray(8)
                if (tcname/='OFF') then
                    roomptr%surface_on(1) = .true.
                    roomptr%matl(1) = tcname
                end if

                ! floor
                tcname = lcarray(9)
                if (tcname/='OFF') then
                    roomptr%surface_on(2) = .true.
                    roomptr%matl(2) = tcname
                end if

                ! walls
                tcname = lcarray(10)
                if (tcname/='OFF') then
                    roomptr%surface_on(3) = .true.
                    roomptr%matl(3) = tcname
                    roomptr%surface_on(4) = .true.
                    roomptr%matl(4) = tcname
                end if

                ! If there are more than 10 arguments, it's the new format that includes grid spacing
                if (countargs(lcarray)==13) then
                    roomptr%ibar = lrarray(11)
                    roomptr%jbar = lrarray(12)
                    roomptr%kbar = lrarray(13)
                end if

                ! Reset this each time in case this is the last entry
                nr = ncomp + 1
            else
                write (*,*) '***Error: Bad COMPA input. At least 10 arguments required.'
                write (iofill,*) '***Error: Bad COMPA input. At least 10 arguments required.'
                stop
            end if
            compaflag=.true.
        end if
    end do

    ! Then do targets
    do ir = 2, inumr
        label = carray(ir,1)
        if (label==' ') cycle
        lrarray = 0.0_eb
        lcarray = ' '
        do i = 2, inumc
            lcarray(i-1) = carray(ir,i)
            lrarray(i-1) = rarray(ir,i)
        end do

        !	TARGET - Compartment position(3) normal(3) Material Method Equation_Type
        if (label=='TARGE') then
            if (countargs(lcarray)>=10) then
                if (n_targets+1>mxtarg) then
                    write (*,5002)
                    write (iofill,5002)
                    stop
                end if

                ! The target can exist, now for the compartment
                n_targets = n_targets + 1
                iroom = lrarray(1)
                if (iroom<1.or.iroom>nr) then
                    write (*,5003) iroom
                    write (iofill,5003) iroom
                    stop
                end if
                targptr => targetinfo(n_targets)
                targptr%room = iroom

                ! position and normal vector
                targptr%center(1:3) = lrarray(2:4)
                targptr%normal(1:3) = lrarray(5:7)

                if (countargs(lcarray)>=11) then
                    targptr%depth_loc = lrarray(11)
                else
                    targptr%depth_loc = 0.5
                end if

                ! target name
                if (countargs(lcarray)>=12) then
                    targptr%name = lcarray(12)
                else
                    write (targptr%name,'(a5,i0)') 'Targ ', n_targets
                end if

                ! material type
                tcname = lcarray(8)
                if (tcname==' ') tcname='DEFAULT'
                targptr%material = tcname
                targptr%wall = 0

                ! equation type, PDE or CYL.  ODE is outdated and changed to PDE if it's in an input file.
                eqtype = ' '
                eqtype = lcarray(10)
                call upperall(eqtype)
                if (eqtype/=' ') then
                    if (eqtype(1:3)=='ODE') then
                        targptr%equaton_type = pde
                        write (*,913) 'Warning', eqtype
                        write (iofill,913) 'Warning', eqtype
                    else if (eqtype(1:3)=='PDE') then
                        targptr%equaton_type = pde
                    else if (eqtype(1:3)=='CYL') then
                        targptr%equaton_type = cylpde
                    else
                        write (*,913) 'Error',eqtype
                        write (iofill,913) 'Error',eqtype
                        stop
                    end if
                end if
            else
                write (*,*) '***Error: Bad TARGE input. At least 10 arguments required.'
                write (iofill,*) '***Error: Bad TARGE input. At least 10 arguments required.'
                stop
            end if
            targeflag=.true.
        end if
    end do

    ! Then do fires
    do ir = 2, inumr
        label = carray(ir,1)
        if (label==' ') cycle
        lrarray = 0.0_eb
        lcarray = ' '
        do i = 2, inumc
            lcarray(i-1) = carray(ir,i)
            lrarray(i-1) = rarray(ir,i)
        end do

        ! FIRE room pos(3) plume ignition_type ignition_criterion normal(3) name
        ! This is almost the same as the older OBJEC keyword (name is moved to the end to make it more
        ! consistent with other keywords
        ! With the FIRE keyword, the rest of the fire definition follows in CHEMI, TIME, HRR, SOOT, CO, and TRACE keywords
        ! For now, we assume that the input file was written correctly by the GUI and just set an index for the forthcoming keywords
        if (label=='FIRE') then
            if (countargs(lcarray)/=11) then
                write (*,*) '***Error: Bad FIRE input. 11 arguments required.'
                write (iofill,*) '***Error: Bad FIRE input. 11 arguments required.'
                stop
            end if
            if (n_fires>=mxfires) then
                write (*,5300)
                write (iofill,5300)
                stop
            end if
            iroom = lrarray(1)
            if (iroom<1.or.iroom>nr-1) then
                write (*,5320) iroom
                write (iofill,5320) iroom
                stop
            end if
            roomptr => roominfo(iroom)
            n_fires = n_fires + 1
            fireptr => fireinfo(n_fires)

            ! Only constrained fires
            fireptr%chemistry_type = 2
            if (fireptr%chemistry_type>2) then
                write (*,5321) fireptr%chemistry_type
                write (iofill,5321) fireptr%chemistry_type
                stop
            end if

            fireptr%x_position = lrarray(2)
            fireptr%y_position = lrarray(3)
            fireptr%z_position = lrarray(4)
            if (fireptr%x_position>roomptr%cwidth.or.fireptr%y_position>roomptr%cdepth.or.fireptr%z_position>roomptr%cheight) then
                write (*,5323) n_fires
                write (iofill,5323) n_fires
                stop
            end if
            fireptr%modified_plume = 1
            if (min(fireptr%x_position,roomptr%cwidth-fireptr%x_position)<=mx_hsep .or. &
                min(fireptr%y_position,roomptr%cdepth-fireptr%y_position)<=mx_hsep) fireptr%modified_plume = 2
            if (min(fireptr%x_position,roomptr%cwidth-fireptr%x_position)<=mx_hsep .and. &
                min(fireptr%y_position,roomptr%cdepth-fireptr%y_position)<=mx_hsep) fireptr%modified_plume = 3

            if (lcarray(6)=='TIME' .or. lcarray(6)=='TEMP' .or. lcarray(6)=='FLUX') then
                ! it's a new format fire line that point to an existing target rather than to one created for the fire
                if (lcarray(6)=='TIME') fireptr%ignition_type = trigger_by_time
                if (lcarray(6)=='TEMP') fireptr%ignition_type = trigger_by_temp
                if (lcarray(6)=='FLUX') fireptr%ignition_type = trigger_by_flux
                tmpcond = lrarray(7)
                fireptr%ignition_target = 0
                if (lcarray(6)=='TEMP' .or. lcarray(6)=='FLUX') then
                    do i = 1,n_targets
                        targptr => targetinfo(i)
                        if (targptr%name==lcarray(8)) fireptr%ignition_target = i
                    end do
                    if (fireptr%ignition_target==0) then
                        write (*,5324) n_fires
                        write (iofill,5324) n_fires
                        stop
                    end if
                end if
            else
                write (*,5322)
                write (iofill,5322)
                stop
            end if
            fireptr%room = iroom
            fireptr%name = lcarray(11)
            ! Note that ignition type 1 is time, type 2 is temperature and 3 is flux
            if (tmpcond>0.0_eb) then
                fireptr%ignited = .false.
                if (fireptr%ignition_type==trigger_by_time) then
                    fireptr%ignition_time = tmpcond
                    fireptr%ignition_criterion = 1.0e30_eb
                else if (fireptr%ignition_type==trigger_by_temp.or.fireptr%ignition_type==trigger_by_flux) then
                    fireptr%ignition_time = 1.0e30_eb
                    fireptr%ignition_criterion = tmpcond
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                else
                    write (*,5358) fireptr%ignition_type
                    write (iofill,5358) fireptr%ignition_type
                    stop
                end if
            else
                fireptr%ignited = .true.
                fireptr%reported = .true.
            end if
            cfireflag=.true.

            ! read and set the other stuff for this fire
            call inputembeddedfire (fireptr, ir, inumc)
        end if
    end do

    ! Then do everything else
    do ir = 2, inumr

        label = carray(ir,1)
        if (label==' ') cycle
        lrarray = 0.0_eb
        lcarray = ' '
        do i = 2, inumc
            lcarray(i-1) = carray(ir,i)
            lrarray(i-1) = rarray(ir,i)
        end do

        !	Start the case statement for key words

        select case (label)

            ! TIMES total_simulation, print interval, smokeview interval, spreadsheet interval
        case ("TIMES")
            if (countargs(lcarray)>=5) then
                time_end =  lrarray(1)
                print_out_interval = abs(lrarray(2))
                smv_out_interval = lrarray(4)
                ss_out_interval =  lrarray(5)
            else if (countargs(lcarray)>=4) then
                time_end =  lrarray(1)
                print_out_interval = abs(lrarray(2))
                smv_out_interval = lrarray(3)
                ss_out_interval =  lrarray(4)
            else
                write (*,*) '***Error: Bad TIMES input. At least 4 arguments required.'
                write (iofill,*) '***Error: Bad TIMES input. At least 4 arguments required.'
                stop
            end if
            ctimeflag=.true.

            ! TAMB reference ambient temperature (c), reference ambient pressure, reference pressure, relative humidity
        case ("TAMB")
            if (countargs(lcarray)>=4) then
                interior_temperature = lrarray(1)
                interior_abs_pressure = lrarray(2)
                relative_humidity = lrarray(4)*0.01_eb
            else if (countargs(lcarray)>=3) then
                interior_temperature = lrarray(1)
                interior_abs_pressure = lrarray(2)
                relative_humidity = lrarray(3)*0.01_eb
            else
                write (*,*) '***Error: Bad TAMB input. At least 3 arguments required.'
                write (iofill,*) '***Error: Bad TAMB input. At least 3 arguments required.'
                stop
            end if
            if (.not.exset) then
                exterior_temperature = interior_temperature
                exterior_abs_pressure = interior_abs_pressure
                exterior_rho = interior_rho
            end if
            tgignt = interior_temperature + 200.0_eb
            tambiflag=.true.

            ! EAMB reference external ambient temperature (c), reference external ambient pressure
        case ("EAMB")
            if (countargs(lcarray)/=3) then
                write (*,*) '***Error: Bad EAMB input. 3 arguments required.'
                write (iofill,*) '***Error: Bad EAMB input. 3 arguments required.'
                stop
            end if
            exterior_temperature = lrarray(1)
            exterior_abs_pressure = lrarray(2)
            exset = .true.
            eambiflag=.true.
            
            ! LIMO2 lower oxygen limit for combustion. This is a global value
        case ("LIMO2")
            if (countargs(lcarray)/=1) then
                write (*,*) '***Error: Bad LIMO2 input. Only 1 argument allowed.'
                write (iofill,*) '***Error: Bad LIMO2 input. Only 1 argument allowed.'
                stop
            end if
            lower_o2_limit = lrarray(1)
            limo2flag=.true.

            ! HVENT 1st, 2nd, which_vent, width, soffit, sill, wind_coef, hall_1, hall_2, face, opening_fraction,
            !           width, soffit, sill
            !		    absolute height of the soffit, absolute height of the sill,
            !           floor_height = absolute height of the floor (not set here)
            !		    compartment offset for the hall command (2 of these)
            !		    face = the relative face of the vent: 1-4 for x plane (-), y plane (+), x plane (+), y plane (-)
            !		    initial open fraction
        case ('HVENT')
            if (countargs(lcarray)<7) then
                write (*,*) '***Error: Bad HVENT input. At least 7 arguments required.'
                write (iofill,*) '***Error: Bad HVENT input. At least 7 arguments required.'
                stop
            else
                i = lrarray(1)
                j = lrarray(2)
                k = lrarray(3)
                imin = min(i,j)
                jmax = max(i,j)
                if (imin>mxrooms-1.or.jmax>mxrooms.or.imin==jmax) then
                    write (*,5070) i, j
                    write (iofill,5070) i, j
                    stop
                end if
                if (k>mxperrm) then
                    write (*,5080) i, j, k
                    write (iofill,5080) i, j, k
                    stop
                end if

                n_hvents = n_hvents + 1
                ventptr => hventinfo(n_hvents)
                ventptr%room1 = imin
                ventptr%room2 = jmax
                ventptr%counter = lrarray(3)

                if (n_hvents>mxhvents) then
                    write (*,5081) i,j,k
                    write (iofill,5081) i,j,k
                    stop
                end if

                ventptr%width = lrarray(4)
                ventptr%soffit = lrarray(5)
                ventptr%sill = lrarray(6)
            end if
            if (lcarray(10)=='TIME' .or. lcarray(10)=='TEMP' .or. lcarray(10)=='FLUX') then
                ventptr%offset(1) = lrarray(7)
                ventptr%offset(2) = 0.0_eb
                ventptr%face = lrarray(9)
                if (lcarray(10)=='TIME') then
                    ventptr%opening_type = trigger_by_time
                    ventptr%opening_initial_time = lrarray(13)
                    ventptr%opening_initial_fraction = lrarray(14)
                    ventptr%opening_final_time = lrarray(15)
                    ventptr%opening_final_fraction = lrarray(16)
                else
                    if (lcarray(10)=='TEMP') ventptr%opening_type = trigger_by_temp
                    if (lcarray(10)=='FLUX') ventptr%opening_type = trigger_by_flux
                    ventptr%opening_criterion = lrarray(11)
                    ventptr%opening_target = 0
                    do i = 1,n_targets
                        targptr => targetinfo(i)
                        if (targptr%name==lcarray(12)) ventptr%opening_target = i
                    end do
                    if (ventptr%opening_target==0) then
                        write (*,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                        write (iofill,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                        stop
                    end if   
                    ventptr%opening_initial_fraction = lrarray(14)
                    ventptr%opening_final_fraction = lrarray(16)
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                end if
            else if (countargs(lcarray)>=11) then
                ventptr%offset(1) = lrarray(8)
                ventptr%offset(2) = lrarray(9)
                ventptr%face = lrarray(10)
                initialopening = lrarray(11)
                ventptr%opening_type = trigger_by_time
                ventptr%opening_initial_fraction = initialopening
                ventptr%opening_final_fraction = initialopening
            else if (countargs(lcarray)>=9) then
                ventptr%offset(1) = lrarray(7)
                ventptr%offset(2) = 0.0_eb
                ventptr%opening_type = trigger_by_time
                ventptr%face = lrarray(8)
                initialopening = lrarray(9)
                ventptr%opening_initial_fraction = initialopening
                ventptr%opening_final_fraction = initialopening
            else
                write (*,*) '***Error: Bad HVENT input. At least 7 arguments required.'
                write (iofill,*) '***Error: Bad HVENT input. At least 7 arguments required.'
                stop
            end if
            roomptr => roominfo(ventptr%room1)
            ventptr%absolute_soffit = ventptr%soffit + roomptr%z0
            ventptr%absolute_sill = ventptr%sill + roomptr%z0
            
            hventflag=.true.

            ! DEADROOM dead_room_num connected_room_num
            ! pressure in dead_room_num is not solved.  pressure for this room
            ! is obtained from connected_room_num
        case ('DEADR')
            i = lrarray(1)
            j = lrarray(2)
            if (i.ge.1.and.i.le.mxrooms.and.j.le.1.and.j.le.mxrooms.and.i.ne.j) then
                roomptr => roominfo(i)
                roomptr%deadroom = j
            end if
            
            deadrflag=.true.

            ! EVENT keyword, the four possible formats are:
            ! EVENT   H     First_Compartment   Second_Compartment	 Vent_Number    Time   Final_Fraction   decay_time
            ! EVENT   V     First_Compartment   Second_Compartment	 Not_Used	    Time   Final_Fraction   decay_time
            ! EVENT   M        Not_Used             Not_used            M_ID        Time   Final_Fraction   decay_time
            ! EVENT   F        Not_Used             Not_used            M_ID        Time   Final_Fraction   decay_time
        case ('EVENT')
            if (countargs(lcarray)>=7) then
                !	        Sort by event type, h, v, m, or f
                venttype = lcarray(1)

                if (lrarray(6)<0.0_eb.or.lrarray(6)>1.0_eb) then
                    write (3,*) '****Error: Bad EVENT input. Final_Fraction (6th argument) must be between 0 and 1 inclusive.'
                    stop
                end if

                select case (venttype)
                case ('H')
                    i = lrarray(2)
                    j = lrarray(3)
                    k = lrarray(4)
                    do iijk = 1, n_hvents
                        ventptr => hventinfo(iijk)
                        if (ventptr%room1==i.and.ventptr%room2==j.and.ventptr%counter==k) then
                            ventptr%opening_initial_time = lrarray(5)
                            ventptr%opening_final_time = lrarray(5) + lrarray(7)
                            ventptr%opening_final_fraction = lrarray(6)
                            hflag(i,j,k,iijk)=.true.
                        end if
                    end do
                    event_hflag=.true.
                case ('V')
                    i = lrarray(2)
                    j = lrarray(3)
                    k = lrarray(4)
                    do iijk = 1, n_vvents
                        ventptr => vventinfo(iijk)
                        if (ventptr%room1==i.and.ventptr%room2==j.and.ventptr%counter==k) then
                            ventptr%opening_initial_time = lrarray(5)
                            ventptr%opening_final_time = lrarray(5) + lrarray(7)
                            ventptr%opening_final_fraction = lrarray(6)
                            vflag(i,j,k,iijk)=.true.
                        end if
                    end do
                    event_vflag=.true.
                case ('M')
                    i = lrarray(2)
                    j = lrarray(3)
                    k = lrarray(4)
                    do iijk = 1, n_mvents
                        ventptr => mventinfo(iijk)
                        if (ventptr%room1==i.and.ventptr%room2==j.and.ventptr%counter==k) then
                            ventptr%opening_initial_time = lrarray(5)
                            ventptr%opening_final_time = lrarray(5) + lrarray(7)
                            ventptr%opening_final_fraction = lrarray(6)
                            mflag(i,j,k,iijk)=.true.
                        end if
                    end do
                    event_mflag=.true.
                case ('F')
                    i = lrarray(2)
                    j = lrarray(3)
                    fannumber = lrarray(4)
                    if (fannumber>n_mvents) then
                        write (*,5196) fannumber
                        write (iofill,5196) fannumber
                        stop
                    end if
                    ventptr => mventinfo(fannumber)
                    ventptr%filter_initial_time = lrarray(5)
                    ventptr%filter_final_time = lrarray(5) + lrarray(7)
                    ventptr%filter_final_fraction = lrarray(6)
                    fflag(i,j,fannumber)=.true.
                    event_fflag=.true.
                    case default
                    write (*,*) '***Error: Bad EVENT input. Type (1st arguement) must be H, V, M, or F.'
                    write (iofill,*) '***Error: Bad EVENT input. Type (1st arguement) must be H, V, M, or F.'
                    stop
                end select
            else
                write (*,*) '***Error: Bad EVENT input. At least 7 arguments required.'
                write (iofill,*) '***Error: Bad EVENT input. At least 7 arguments required.'
                stop
            end if

            ! RAMP - from_compartment (or 0) to_compartment (or 0) vent_or_fire_number number_of_xy_pairs x1 y1 x2 y2 ... xn yn
        case ('RAMP')
            if (countargs(lcarray)<9) then
                write (*,*) '***Error: Bad RAMP input. At least 9 arguments required.'
                write (iofill,*) '***Error: Bad RAMP input. At least 9 arguments required.'
                stop
            else if (lrarray(5)<=1) then
                write (*,*) '***Error: Bad RAMP input. At least 1 time point must be specified.'
                write (iofill,*) '***Error: Bad RAMP input. At least 1 time point must be specified.'
                stop
            else if (countargs(lcarray)/=5+2*lrarray(5)) then
                write (*,*) '***Error: Bad RAMP input. Inputs must be in pairs.'
                write (iofill,*) '***Error: Bad RAMP input. Inputs must be in pairs.'
                stop
            end if
            if (nramps<=mxramps) then
                nramps = nramps + 1
                rampptr=>rampinfo(nramps)
                rampptr%type = lcarray(1)
                rampptr%room1 = lrarray(2)
                rampptr%room2 = lrarray(3)
                rampptr%counter = lrarray(4)
                rampptr%npoints = lrarray(5)
                do iramp = 1,rampptr%npoints
                    rampptr%time(iramp) = lrarray(4+2*iramp)
                    rampptr%value(iramp) = lrarray(5+2*iramp)
                end do
            end if
            crampflag=.true.

            ! VVENT - from_compartment to_compartment area shape initial_fraction
        case ('VVENT')
            if (countargs(lcarray)>=5) then
                i = lrarray(1)
                j = lrarray(2)
                if (countargs(lcarray)==5) then
                    ! oldest format that only allows one vent per compartment pair
                    k = 1
                    icarea = 3
                    icshape = 4
                    icfraction = 5
                else
                    ! newer format that allows more than one vent per compartment pair
                    k = lrarray(3)
                    icarea = 4
                    icshape = 5
                    icfraction = 6
                end if
                ! check for outside of compartment space; self pointers are covered in read_input_file
                if (i>mxrooms.or.j>mxrooms) then
                    write (iofill,5070) i, j
                    write (iofill,5070) i, j
                    stop
                end if
                n_vvents = n_vvents + 1
                ventptr => vventinfo(n_vvents)
                ventptr%room1 = i
                ventptr%room2 = j
                ventptr%counter = k
                ! read_input_file will verify the orientation (i is on top of j)
                ventptr%area = lrarray(icarea)
                ! check the shape parameter. the default (1) is a circle)
                if (lrarray(icshape)<1.or.lrarray(icshape)>2) then
                    ventptr%shape = 1
                else
                    ventptr%shape = lrarray(icshape)
                end if
                if (lcarray(6)=='TIME' .or. lcarray(6)=='TEMP' .or. lcarray(6)=='FLUX') then
                    if (lcarray(6)=='TIME') then
                        ventptr%opening_type = trigger_by_time
                        ventptr%opening_initial_time = lrarray(9)
                        ventptr%opening_initial_fraction = lrarray(10)
                        ventptr%opening_final_time = lrarray(11)
                        ventptr%opening_final_fraction = lrarray(12)
                    else
                        if (lcarray(6)=='TEMP') ventptr%opening_type = trigger_by_temp
                        if (lcarray(6)=='FLUX') ventptr%opening_type = trigger_by_flux
                        ventptr%opening_criterion = lrarray(7)
                        ventptr%opening_target = 0
                        do i = 1,n_targets
                            targptr => targetinfo(i)
                            if (targptr%name==lcarray(8)) ventptr%opening_target = i
                        end do
                        if (ventptr%opening_target==0) then
                            write (*,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                            write (iofill,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                            stop
                        end if  
                        ventptr%opening_initial_fraction = lrarray(10)
                        ventptr%opening_final_fraction = lrarray(12)
                        if (stpmax>0) then
                            stpmax = min(stpmax,1.0_eb)
                        else
                            stpmax = 1.0_eb
                        end if
                    end if
                    ventptr%xoffset = lrarray(13)
                    ventptr%yoffset = lrarray(14)
                else
                    ventptr%opening_type = trigger_by_time
                    ventptr%opening_initial_fraction = lrarray(icfraction)
                    ventptr%opening_final_fraction = lrarray(icfraction)
                    if (ventptr%room1<=nr-1) then
                        roomptr => roominfo(ventptr%room1)
                        ventptr%xoffset = roomptr%cwidth/2
                        ventptr%yoffset = roomptr%cdepth/2
                    else
                        roomptr => roominfo(ventptr%room2)
                        ventptr%xoffset = roomptr%cwidth/2
                        ventptr%yoffset = roomptr%cdepth/2
                    end if

                end if
            else
                write (*,*) '***Error: Bad VVENT input. At least 5 arguments required.'
                write (iofill,*) '***Error: Bad VVENT input. At least 5 arguments required.'
                stop
            end if
            vventflag=.true.

            ! MVENT - simplified mechanical ventilation

            ! (1) From_Compartment, (2) To_Compartment, (3) ID_Number
            ! (4-6) From_Opening_Orientation From_Center_Height From_Opening_Area
            ! (7-9) To_Opening_Orientation To_Center_Height To_Opening_Area
            ! (10-12) Flow Flow_Begin_Dropoff_Pressure Zero_Flow_Pressure
            ! (13) Initial fraction of the fan speed
        case ('MVENT')
            if (countargs(lcarray)>=13) then
                i = lrarray(1)
                j = lrarray(2)
                k = lrarray(3)
                if (i>nr.or.j>nr) then
                    write (*,5191) i, j
                    write (iofill,5191) i, j
                    stop
                end if
                n_mvents = n_mvents + 1
                ventptr => mventinfo(n_mvents)
                ventptr%room1 = i
                ventptr%room2 = j
                ventptr%counter = k

                if (lcarray(4)=='V') then
                    ventptr%orientation(1) = 1
                else
                    ventptr%orientation(1) = 2
                end if
                ventptr%height(1) = lrarray(5)
                ventptr%diffuser_area(1) = lrarray(6)

                if (lcarray(7)=='V') then
                    ventptr%orientation(2) = 1
                else
                    ventptr%orientation(2) = 2
                end if
                ventptr%height(2) = lrarray(8)
                ventptr%diffuser_area(2) = lrarray(9)

                ventptr%n_coeffs = 1
                ventptr%coeff = 0.0_eb
                ventptr%coeff(1) = lrarray(10)
                ventptr%maxflow = lrarray(10)
                ventptr%min_cutoff_relp = lrarray(11)
                ventptr%max_cutoff_relp = lrarray(12)

                if (lcarray(13)=='TIME' .or. lcarray(13)=='TEMP' .or. lcarray(13)=='FLUX') then
                    if (lcarray(13)=='TIME') then
                        ventptr%opening_type = trigger_by_time
                        ventptr%opening_initial_time = lrarray(16)
                        ventptr%opening_initial_fraction = lrarray(17)
                        ventptr%opening_final_time = lrarray(18)
                        ventptr%opening_final_fraction = lrarray(19)
                    else
                        if (lcarray(13)=='TEMP') ventptr%opening_type = trigger_by_temp
                        if (lcarray(13)=='FLUX') ventptr%opening_type = trigger_by_flux
                        ventptr%opening_criterion = lrarray(14)
                        ventptr%opening_target = 0
                        do i = 1,n_targets
                            targptr => targetinfo(i)
                            if (targptr%name==lcarray(15)) ventptr%opening_target = i
                        end do
                        if (ventptr%opening_target==0) then
                            write (*,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                            write (iofill,*) '***Error: Bad HVENT input. Vent opening specification requires an associated target.'
                            stop
                        end if  
                        ventptr%opening_initial_fraction = lrarray(17)
                        ventptr%opening_final_fraction = lrarray(19)
                    end if
                    ventptr%xoffset = lrarray(20)
                    ventptr%yoffset = lrarray(21)
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                else
                    ventptr%opening_type = trigger_by_time
                    ventptr%opening_initial_fraction = lrarray(13)
                    ventptr%opening_final_fraction = lrarray(13)
                    if (ventptr%room1<=nr-1) then
                        roomptr => roominfo(ventptr%room1)
                        ventptr%xoffset = 0.0_eb
                        ventptr%yoffset = roomptr%cdepth/2
                    else
                        roomptr => roominfo(ventptr%room2)
                    end if
                    ventptr%xoffset = 0.0_eb
                    ventptr%yoffset = roomptr%cdepth/2
                end if
            else
                write (*,*) '***Error: Bad MVENT input. 13 arguments required.'
                write (iofill,*) '***Error: Bad MVENT input. 13 arguments required.'
                stop
            end if
            mventflag=.true.

            ! DETECT Type Compartment Activation_Value Width Depth Height RTI Suppression Spray_Density
        case ('DETEC')
            if (countargs(lcarray)>=9) then
                n_detectors = n_detectors + 1

                if (n_detectors>mxdtect) then
                    write (*, 5338)
                    write (iofill, 5338)
                    stop
                end if

                dtectptr => detectorinfo(n_detectors)
                if (lcarray(1)=='SMOKE') then
                    i1 = smoked
                else if (lcarray(1)=='HEAT') then
                    i1 = heatd
                else if (lcarray(1)=='SPRINKLER') then
                    i1 = sprinkd
                else
                    i1 = lrarray(1)
                    ! force to heat detector if out of range
                    if (i1>3) i1 = heatd
                end if
                dtectptr%dtype = i1

                i2 = lrarray(2)
                iroom = i2
                dtectptr%room = iroom
                if (iroom<1.or.iroom>mxrooms) then
                    write (*,5342) i2
                    write (iofill,5342) i2
                    stop
                end if

                dtectptr%trigger = lrarray(3)
                dtectptr%center(1) = lrarray(4)
                dtectptr%center(2) = lrarray(5)
                dtectptr%center(3) = lrarray(6)
                dtectptr%rti =  lrarray(7)
                if (lrarray(8)/=0) then
                    dtectptr%quench = .true.
                else
                    dtectptr%quench = .false.
                end if
                dtectptr%spray_density = lrarray(9)*1000.0_eb
                ! if spray density is zero, then turn off the sprinkler
                if (dtectptr%spray_density==0.0_eb) then
                    dtectptr%quench = .false.
                end if
                ! if there's a sprinkler that can go off, then make sure the time step is small enough to report it accurately
                if (dtectptr%quench) then
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                end if
                roomptr => roominfo(iroom)
                if (roomptr%name==' ') then
                    write (*,5344) i2
                    write (iofill,5344) i2
                    stop
                end if

                if (dtectptr%center(1)>roomptr%cwidth.or. &
                    dtectptr%center(2)>roomptr%cdepth.or.dtectptr%center(3)>roomptr%cheight) then
                write (*,5339) n_detectors,roomptr%name
                write (iofill,5339) n_detectors,roomptr%name
                stop
                end if

            else
                write (*,*) '***Error: Bad DETEC input. At least 9 arguments required.'
                write (iofill,*) '***Error: Bad DETEC input. At least 9 arguments required.'
                stop
            end if
            detecflag=.true.

            !  VHEAT top_compartment bottom_compartment
        case ('VHEAT')
            if (countargs(lcarray)>=2) then
                i1 = lrarray(1)
                i2 = lrarray(2)
                if (i1<1.or.i2<1.or.i1>nr.or.i2>nr) then
                    write (*,5345) i1, i2
                    write (iofill,5345) i1, i2
                    stop
                end if

                nvcons = nvcons + 1
                i_vconnections(nvcons,w_from_room) = i1
                i_vconnections(nvcons,w_from_wall) = 2
                i_vconnections(nvcons,w_to_room) = i2
                i_vconnections(nvcons,w_to_wall) = 1
            else
                write (*,*) '***Error: Bad VHEAT input. At least 2 arguments required.'
                write (iofill,*) '***Error: Bad VHEAT input. At least 2 arguments required.'
                stop
            end if
            vheatflag=.true.

            ! ONEZ compartment number - This turns the compartment into a single zone
        case ('ONEZ')
            if (countargs(lcarray)>=1) then
                iroom = lrarray(1)
                if (iroom<1.or.iroom>nr) then
                    write (*, 5001) i1
                    write (iofill, 5001) i1
                    stop
                end if
                roomptr => roominfo(iroom)
                roomptr%shaft = .true.
            else
                write (*,*) '***Error: Bad ONEZ input. At least 1 compartment must be specified.'
                write (iofill,*) '***Error: Bad ONEZ input. At least 1 compartment must be specified.'
                stop
            end if
            conezflag=.true.

            ! HALL Compartment Velocity Depth Decay_Distance
        case ('HALL')
            if (countargs(lcarray)>=1) then
                iroom = lrarray(1)

                ! check that specified room is valid
                if (iroom<0.or.iroom>nr) then
                    write (*,5346) iroom
                    write (iofill,5346) iroom
                    stop
                end if

                roomptr => roominfo(iroom)
                roomptr%hall = .true.
                if (countargs(lcarray)>1) then
                    write (*,5406) iroom
                    write (iofill,5406) iroom
                end if
            else
                write (*,*) '***Error: Bad HALL input. At least 1 compartment must be specified.'
                write (iofill,*) '***Error: Bad HALL input. At least 1 compartment must be specified.'
                stop
            end if
            challflag=.true.

            ! ROOMA Compartment Number_of_Area_Values Area_Values
            ! This provides for variable compartment floor areas; this should be accompanied by the roomh command
        case ('ROOMA')
            if (countargs(lcarray)>=2) then
                iroom = lrarray(1)
                roomptr => roominfo(iroom)

                ! make sure the room number is valid
                if (iroom<1.or.iroom>nr) then
                    write (*,5347) iroom
                    write (iofill,5347) iroom
                    stop
                end if

                ! make sure the number of points is valid
                npts = lrarray(2)
                if (npts>mxcross.or.npts<=0.or.npts/=countargs(lcarray)-2) then
                    write (*,5347) npts
                    write (iofill,5347) npts
                    stop
                end if
                if (roomptr%nvars/=0) npts = min(roomptr%nvars,npts)
                roomptr%nvars = npts

                ! make sure all data is positive
                do  i = 1, npts
                    if (lrarray(i+2)<0.0_eb) then
                        write (*,5348) lrarray(i+2)
                        write (iofill,5348) lrarray(i+2)
                        stop
                    end if
                end do

                ! put the data in its place
                do i = 1, npts
                    roomptr%var_area(i) = lrarray(i+2)
                end do
            else
                write (*,*) '***Error: Bad ROOMA input. At least 2 arguments must be specified.'
                write (iofill,*) '***Error: Bad ROOMA input. At least 2 arguments must be specified.'
                stop
            end if
            roomaflag=.true.

            ! ROOMH Compartment Number_of_Height_Values Height_Values
            ! This companion to ROOMA, provides for variable compartment floor areas; this should be accompanied by the ROOMA command
        case ('ROOMH')
            if (countargs(lcarray)>=2) then
                iroom = lrarray(1)
                roomptr => roominfo(iroom)

                ! make sure the room number is valid
                if (iroom<1.or.iroom>nr) then
                    write (*,5349) iroom
                    write (iofill,5349) iroom
                    stop
                end if

                ! make sure the number of points is valid
                npts = lrarray(2)
                if (npts>mxcross.or.npts<0.or.npts/=countargs(lcarray)-2) then
                    write (*,5350) npts
                    write (iofill,5350) npts
                    stop
                end if
                if (roomptr%nvars/=0)npts = min(roomptr%nvars,npts)
                roomptr%nvars = npts

                ! make sure all data is positive
                do i = 1, npts
                    if (lrarray(i+2)<0.0_eb) then
                        write (*,5348) lrarray(i+2)
                        write (iofill,5348) lrarray(i+2)
                        stop
                    end if
                end do

                ! put the data in its place
                do i = 1, npts
                    roomptr%var_height(i) = lrarray(i+2)
                end do

            else
                write (*,*) '***Error: Bad ROOMH input. At least 2 arguments must be specified.'
                write (iofill,*) '***Error: Bad ROOMH input. At least 2 arguments must be specified.'
                stop
            end if
            roomhflag=.true.

            ! DTCHE Minimum_Time_Step Maximum_Iteration_Count
        case ('DTCHE')
            if (countargs(lcarray)>=2) then
                stpmin = abs(lrarray(1))
                stpmin_cnt_max = abs(lrarray(2))
                ! a negative turns off the check
                if (lrarray(2)<=0) stpminflag = .false.
            else
                write (*,*) '***Error: Bad DTCHE input. At least 2 arguments must be specified.'
                write (iofill,*) '***Error: Bad DTCHE input. At least 2 arguments must be specified.'
                stop
            end if
            dtcheflag=.true.

            ! Horizontal heat flow, HHEAT First_Compartment Number_of_Parts nr pairs of {Second_Compartment, Fraction}

            ! There are two forms of the command
            !   The first (single entry of the room number) - all connections based on horizontal flow
            !   The second is the compartment number followed by nr pairs of compartments to which the heat
            !   will flow and the fraction of the vertical surface of the compartment that loses heat
        case ('HHEAT')
            if (countargs(lcarray)>=1) then
                nto = 0
                ifrom = lrarray(1)
                roomptr => roominfo(ifrom)
                if (countargs(lcarray)==1) then
                    roomptr%iheat = 1
                    cycle
                else
                    nto = lrarray(2)
                    if (nto<1.or.nto>nr) then
                        write (*,5354) nto
                        write (iofill,5354) nto
                        stop
                    end if
                    roomptr%iheat = 2
                end if

                if (2*nto==(countargs(lcarray)-2)) then
                    do i = 1, nto
                        i1 = 2*i+1
                        i2 = 2*i+2
                        ito = lrarray(i1)
                        frac = lrarray(i2)
                        if (ito<1.or.ito==ifrom.or.ito>nr) then
                            write (*, 5356) ifrom,ito
                            write (iofill, 5356) ifrom,ito
                            stop
                        end if
                        if (frac<0.0_eb.or.frac>1.0_eb) then
                            write (*, 5357) ifrom,ito,frac
                            write (iofill, 5357) ifrom,ito,frac
                            stop
                        end if
                        roomptr%heat_frac(ito) = frac
                        hheat_comp(i)=ito
                    end do
                else
                    write (*,5355) ifrom, nto
                    write (iofill,5355) ifrom, nto
                    stop
                end if
            else
                write (*,*) '***Error: Bad HHEAT input. At least 1 arguments must be specified.'
                write (iofill,*) '***Error: Bad HHEAT input. At least 1 arguments must be specified.'
                stop
            end if
            hheatflag=.true.
            hheatnto=nto

            ! FURN - no fire, heat walls according to a prescribed time temperature curve
        case ('FURN')
            nfurn=lrarray(1)+0.5
            do i = 1, nfurn
                furn_time(i)=lrarray(2*i)
                furn_temp(i)=lrarray(2*i+1)
            end do
            furncflag=.true.
            nfurncount=nfurn

            ! ADIAB - all surfaces are adiabatic so that dT/dx at the surface = 0
        case ('ADIAB')
            adiabatic_walls = .true.
            
            adiabflag=.true.

            ! SLCF 2-D and 3-D slice files
        case ('SLCF')
            if (countargs(lcarray)>=1) then
                nvisualinfo = nvisualinfo + 1
                sliceptr => visualinfo(nvisualinfo)
                if (lcarray(1)=='2-D') then
                    sliceptr%vtype = 1
                else if (lcarray(1)=='3-D') then
                    sliceptr%vtype = 2
                else
                    write (*, 5403) nvisualinfo
                    write (iofill, 5403) nvisualinfo
                    stop
                end if
                ! 2-D slice file
                if (sliceptr%vtype==1) then
                    ! get position (required) and compartment (optional) first so we can check to make sure
                    ! desired position is within the compartment(s)
                    if (countargs(lcarray)>2) then
                        sliceptr%position = lrarray(3)
                        if (countargs(lcarray)>3) then
                            sliceptr%roomnum = lrarray(4)
                        else
                            sliceptr%roomnum = 0
                        end if
                        if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
                            write (*, 5403) nvisualinfo
                            write (iofill, 5403) nvisualinfo
                            stop
                        end if
                        if (lcarray(2) =='X') then
                            sliceptr%axis = 1
                            if (sliceptr%roomnum>0) then
                                roomptr => roominfo(sliceptr%roomnum)
                                if (sliceptr%position>roomptr%cwidth.or.sliceptr%position<0.0_eb) then
                                    write (*, 5403) nvisualinfo
                                    write (iofill, 5403) nvisualinfo
                                    stop
                                end if
                            end if
                        else if (lcarray(2) =='Y') then
                            sliceptr%axis = 2
                            if (sliceptr%roomnum>0) then
                                roomptr => roominfo(sliceptr%roomnum)
                                if (sliceptr%position>roomptr%cdepth.or.sliceptr%position<0.0_eb) then
                                    write (*, 5403) nvisualinfo
                                    write (iofill, 5403) nvisualinfo
                                    stop
                                end if
                            end if
                        else if (lcarray(2) =='Z') then
                            sliceptr%axis = 3
                            if (sliceptr%roomnum>0) then
                                roomptr => roominfo(sliceptr%roomnum)
                                if (sliceptr%position>roomptr%cheight.or.sliceptr%position<0.0_eb) then
                                    write (*, 5403) nvisualinfo
                                    write (iofill, 5403) nvisualinfo
                                    stop
                                end if
                            end if
                        else
                            write (*, 5403) nvisualinfo
                            write (iofill, 5403) nvisualinfo
                            stop
                        end if
                    else
                        write (*, 5403) nvisualinfo
                        write (iofill, 5403) nvisualinfo
                        stop
                    end if
                    ! 3-D slice
                else if (sliceptr%vtype==2) then
                    if (countargs(lcarray)>1) then
                        sliceptr%roomnum = lrarray(2)
                    else
                        sliceptr%roomnum = 0
                    end if
                    if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
                        write (*, 5403) nvisualinfo
                        write (iofill, 5403) nvisualinfo
                        stop
                    end if
                end if
            else
                write (*,*) '***Error: Bad SLCF input. At least 1 arguments must be specified.'
                write (iofill,*) '***Error: Bad SLCF input. At least 1 arguments must be specified.'
                stop
            end if
            cslcfflag=.true.

            ! ISOF isosurface of specified temperature in one or all compartments
        case ('ISOF')
            if (countargs(lcarray)>=1) then
                nvisualinfo = nvisualinfo + 1
                sliceptr => visualinfo(nvisualinfo)
                sliceptr%vtype = 3
                sliceptr%value = lrarray(1)
                if (countargs(lcarray)>1) then
                    sliceptr%roomnum = lrarray(2)
                else
                    sliceptr%roomnum = 0
                end if
                if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
                    write (*, 5404) nvisualinfo
                    write (iofill, 5404) nvisualinfo
                    stop
                end if
            else
                write (*,*) '***Error: Bad SLCF input. At least 1 arguments must be specified.'
                write (iofill,*) '***Error: Bad SLCF input. At least 1 arguments must be specified.'
                stop
            end if
            cisofflag=.true.

            ! Outdated keywords
        case ('CJET','WIND','GLOBA','DJIGN') ! Just ignore these inputs ... they shouldn't be fatal
            write (*,5407) label
            write (iofill,5407) label
        case ('OBJFL','MVOPN','MVFAN','MAINF','INTER','SETP','THRMF','OBJEC') ! these are clearly outdated and produce errors
            write (*,5405) label
            write (iofill,5405) label
            stop
        case ('MATL','COMPA','TARGE','HEIGH','AREA','TRACE','CO','SOOT',&
            'HRR','TIME','CHEMI','FIRE','STPMA') ! these are already handled above

        case default
        write (*, 5051) label
        write (iofill, 5051) label
        stop
        end select
    end do

913 format('***',a,': BAD TARGE input. Invalid equation type:',A3,' Valid choices are: PDE or CYL')
5001 format ('***Error: Bad ONEZ input. Referenced compartment is not defined ',i0)
5002 format ('***Error: BAD TARGE input. Too many targets are being defined')
5003 format ('***Error: BAD TARGE input. The compartment specified by TARGET does not exist ',i0)
5051 format ('***Error: The key word ',a5,' is not recognized')
5062 format ('***Error: Bad COMPA input. Compartment number outside of allowable range ',i0)
5070 format ('***Error: Bad VENT input. Parameter(s) outside of allowable range',2I4)
5080 format ('***Error: Bad HVENT input. Too many pairwise horizontal connections',3I5)
5081 format ('***Error: Too many horizontal connections ',3i5)
5191 format ('***Error: Bad MVENT input. Compartments specified in MVENT have not been defined ',2i3)
5192 format ('***Error: Bad MVENT input. Exceeded maximum number of nodes/openings in MVENT ',2i3)
5193 format ('***Error: Bad MVENT input. MVENT(MID) is not consistent and should be a fan ',2i3)
5194 format ('***Error: Bad MVENT input. Pressure for zero flow must exceed the lower limit',f10.2)
5195 format ('***Error: Bad MVENT input. Too many fan systems ',i0)
5196 format ('***Error: Bad EVENT input. Fan has not been defined for this filter ',i0)
5300 format ('***Error: Bad FIRE input. Too many objects defined in datafile')
5310 format ('***Error: Bad FIRE input. Incorrect number of parameters for OBJECT')
5320 format ('***Error: Bad FIRE input. Fire specification error, room ',i0,' out of range')
5321 format ('***Error: Bad FIRE input. Fire specification error, not an allowed fire type',i0)
5322 format ('***Error: Bad FIRE input. Fire specification is outdated and must include target for ignition')
5323 format ('***Error: Bad FIRE input. Fire location ',i0,' is outside its compartment')
5324 format ('***Error: Bad FIRE input. Target specified for fire ',i0, ' does not exist')
5338 format ('***Error: Bad DETEC input. Exceed allowed number of detectors')
5339 format ('***Error: Bad DETEC input. Detector ',i0,' is outside of compartment ',a)
5342 format ('***Error: Bad DETEC input. Invalid DETECTOR specification - room ',i0)
5344 format ('***Error: Bad DETEC input. A referenced compartment is not yet defined ',i0)
5345 format ('***Error: Bad VHEAT input. A referenced compartment does not exist')
5346 format ('***Error: Bad HALL input. A referenced compartment does not exist ',i0)
5347 format ('***Error: Bad ROOMA input. Compartment specified by ROOMA does not exist ',i0)
5348 format ('***Error: Bad ROOMA or ROOMH input. Data on the ROOMA (or H) line must be positive ',1pg12.3)
5349 format ('***Error: Bad ROOMH input. Compartment specified by ROOMH is not defined ',i0)
5350 format ('***Error: Bad ROOMH input. ROOMH error on data line ',i0)
5354 format ('***Error: Bad HHEAT input. HHEAT to compartment out of bounds or not defined - ',i0)
5355 format ('***Error: Bad HHEAT input. HHEAT fraction pairs are not consistent ',2i3)
5356 format ('***Error: Bad HHEAT input. HHEAT specification error in compartment pairs: ',2i3)
5357 format ('***Error: Bad HHEAT input. Error in fraction for HHEAT:',2i3,f6.3)
5358 format ('***Error: Bad FIRE input. Not a valid ignition criterion ',i0)
5403 format ('***Error: Bad SLCF input. Invalid SLCF specification in visualization input ',i0)
5404 format ('***Error: Bad ISOF input. Invalid ISOF specification in visualization input ',i0)
5405 format ('***Error: Invalid or outdated keyword in CFAST input file ',a)
5406 format ('***Error: Bad HALL input. Outdated HALL command for compartment ',i0,' Flow inputs are no longer used')
5407 format ('***Warning: Outdated keyword in CFAST input file ignored ',a)

    end subroutine keywordcases

    ! --------------------------- inputembeddedfire -------------------------------------------

    subroutine inputembeddedfire(fireptr, lrowcount, inumc)

    !     routine: inputembeddedfire
    !     purpose: This routine reads a new format fire definition that begins with a FIRE keyword (already read in keywordcases)
    !              followed by CHEMI, TIME, HRR, SOOT, CO, TRACE, AREA, and HEIGH keywords (read in here)
    !     Arguments: fireptr: pointer to data for this fire object
    !                lrowcount: current row in the input file.  We begin one row after this one
    !                inumc:   number of columns in the input file
    !                iobj:    pointer to the fire object that will contain all the data we read in here

    integer, intent(in) :: inumc, lrowcount
    type(fire_type), intent(inout), pointer :: fireptr

    character(128) :: lcarray(ncol)
    character(5) :: label
    integer :: iofill = 3, midpoint = 1, base = 2, ir, i, nret
    real(eb) :: lrarray(ncol), ohcomb, max_area, max_hrr, hrrpm3, flamelength
    type(room_type), pointer :: roomptr

    ! there are eight required inputs for each fire
    lrarray = 0.0_eb
    lcarray = ' '
    do ir = 1, 8
        label = carray(lrowcount+ir,1)
        if (label==' ') cycle
        do i = 2, inumc
            lcarray(i-1) = carray(lrowcount+ir,i)
            lrarray(i-1) = rarray(lrowcount+ir,i)
        end do

        select case (label)

            ! The new CHEMIE line defines chemistry for the current fire object.  This includes chemical formula,
            !  radiative fraction, heat of combustion, and material
        case ('CHEMI')
            if (countargs(lcarray)>=7) then
                ! define chemical formula
                fireptr%n_C = lrarray(1)
                fireptr%n_H = lrarray(2)
                fireptr%n_O = lrarray(3)
                fireptr%n_N = lrarray(4)
                fireptr%n_Cl = lrarray(5)
                fireptr%molar_mass = (12.01*fireptr%n_C + 1.008*fireptr%n_H + 16.0*fireptr%n_O + &
                    14.01*fireptr%n_N + 35.45*fireptr%n_Cl)/1000.0
                fireptr%chirad = lrarray(6)
                ohcomb = lrarray(7)
                if (ohcomb<=0.0_eb) then
                    write (*,5001) ohcomb
                    write (iofill,5001) ohcomb
                    stop
                end if
            else
                write (*,*) '***Error: At least 7 arguments required on CHEMI input'
                write (iofill,*) '***Error: At least 7 arguments required on CHEMI input'
                stop
            end if
            chemiflag=.true.
        case ('TIME')
            nret = countargs(lcarray)
            fireptr%npoints = nret
            do i = 1, nret
                fireptr%time(i) = lrarray(i)
            end do
        case ('HRR')
            max_hrr = 0.0_eb
            do i = 1, nret
                fireptr%qdot(i) = lrarray(i)
                max_hrr = max(max_hrr, fireptr%qdot(i))
                fireptr%mdot(i) = fireptr%qdot(i)/ohcomb
            end do
        case ('SOOT')
            do i = 1, nret
                fireptr%y_soot(i) = lrarray(i)
            end do
        case ('CO')
            do i = 1, nret
                fireptr%y_co(i) = lrarray(i)
            end do
        case ('TRACE')
            ! Note that CT, TUHC and TS are carried in the mprodr array - all other species have their own array
            do i = 1, nret
                fireptr%y_trace(i) = lrarray(i)
            end do
        case ('AREA')
            max_area = 0.0_eb
            do i = 1, nret
                ! The minimum area is to stop dassl from a floating point underflow when it tries to extrapolate back to the
                ! ignition point. It only occurs for objects which are on the floor and ignite after t=0. The assumed minimum fire
                ! diameter of 0.2 m below is the minimum valid fire diameter for Heskestad's plume correlation
                ! (from SFPE Handbook chapter)
                if (lrarray(i)==0.0_eb) then
                    write (*,5002)
                    write (iofill,5002)
                    stop
                end if
                fireptr%area(i) = max(lrarray(i),pio4*0.2_eb**2)
                max_area = max(max_area,fireptr%area(i))
            end do

            ! calculate a characteristic length of an object (we assume the diameter).
            ! This is used for point source radiation fire to target calculation as a minimum effective
            ! distance between the fire and the target which only impact very small fire to target distances
            fireptr%characteristic_length = sqrt(max_area/pio4)
        case ('HEIGH')
            do i = 1, nret
                fireptr%height(i) = max(lrarray(i),0.0_eb)
            end do
            case default
            write (*, 5000) label
            write (iofill, 5000) label
            stop
        end select

    end do

    ! set the heat of combustion - this is a problem if the qdot is zero and the mdot is zero as well
    call set_heat_of_combustion (fireptr%npoints, fireptr%mdot, fireptr%qdot, fireptr%hoc, ohcomb)

    ! Position the object
    roomptr => roominfo(fireptr%room)
    call positionobject(fireptr%x_position,roomptr%cwidth,midpoint,mx_hsep)
    call positionobject(fireptr%y_position,roomptr%cdepth,midpoint,mx_hsep)
    call positionobject(fireptr%z_position,roomptr%cheight,base,mx_hsep)

    ! diagnostic - check for the maximum heat release per unit volume.
    ! first, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
    call flame_height(max_hrr, max_area, flamelength)
    flamelength = max (0.0_eb, flamelength)
    ! now the heat realease per cubic meter of the flame - we know that the size is larger than 1.0d-6 m^3 - enforced above
    hrrpm3 = max_hrr/(pio4*fireptr%characteristic_length**2*(fireptr%characteristic_length+flamelength))
    if (hrrpm3>4.0e6_eb) then
        write (*,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
        write (*, 5108)
        write (iofill,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
        write (iofill, 5108)
        stop
    else if (hrrpm3>2.0e6_eb) then
        write (*,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
        write (*, 5108)
        write (iofill,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
        write (iofill, 5108)
    end if

    return
5001 format ('***Error: Invalid heat of combustion, must be greater than zero, ',1pg12.3)
5002 format ('***Error: Invalid fire area. All input values must be greater than zero')
5106 format ('***Error: Object ',a,' position set to ',3F7.3,'; Maximum HRR per m^3 = ',1pg10.3,' exceeds physical limits')
5107 format ('Object ',a,' position set to ',3F7.3,'; Maximum HRR per m^3 = ',1pg10.3,' exceeds nominal limits')
5108 format ('Typically, this is caused by too small fire area inputs. Check HRR and fire area inputs')
5000 format ('***Error: The key word ',a5,' is not part of a fire definition. Fire keyword are likely out of order')

    end subroutine inputembeddedfire

    ! --------------------------- set_heat_of_combustion -------------------------------------------

    subroutine set_heat_of_combustion (maxint, mdot, qdot, hdot, hinitial)

    !	Routine to implement the algorithm to set the heat of combustion for all fires

    integer, intent(in) :: maxint
    real(eb), intent(in) :: qdot(maxint), hinitial
    real(eb), intent(out) :: mdot(maxint), hdot(maxint)

    integer :: i
    real(eb) :: hcmax = 1.0e8_eb, hcmin = 1.0e6_eb

    do i = 1, maxint
        if (i>1) then
            if (mdot(i)*qdot(i)<=0.0_eb) then
                hdot(i) = hinitial
            else
                hdot(i) = min(hcmax,max(qdot(i)/mdot(i),hcmin))
                mdot(i) = qdot(i)/hdot(i)
            end if
        else
            hdot(1) = hinitial
        end if
    end do

    return

    end subroutine set_heat_of_combustion


    ! --------------------------- open_files -------------------------------------------

    subroutine open_files ()
    
    use namelist_data

    !     routine: open_files
    !     purpose: get the paths and project base name open the input file for reading (1)
    ! 	         delete the output files
    ! 	         open the log file (3)
    ! 	         call the input routines
    !     arguments: errorcode: return error indication if non-zero

    integer :: lp, ld, ios
    character(256) :: testpath, testproj, revision, revision_date, compile_date
    
    ! get the path and project names
    call exehandle (exepath, datapath, project)
    
    ! form the file names for datafiles
    testpath = trim (datapath)
    lp = len_trim (testpath)
    testproj = trim (project)
    ld = len_trim (testproj)
    if (nmlflag) then
        inputfile = testpath(1:lp) // testproj(1:ld) // '.cfast'
    else
        inputfile = testpath(1:lp) // testproj(1:ld) // '.in'
        nmlconfile = testpath(1:lp) // testproj(1:ld) // '.cfast'
        ! the input format conversion files
        if (dotinflag) then
            open (unit=31,file=nmlconfile,delim='apostrophe')
        end if
    end if
    outputfile = testpath(1:lp) // testproj(1:ld) // '.out'
    smvhead = testpath(1:lp) // testproj(1:ld) // '.smv'
    smvdata = testpath(1:lp) // testproj(1:ld) // '.plt'
    smvcsv = testpath(1:lp) // testproj(1:ld) // '_zone.csv'
    ssflow = testpath(1:lp) // testproj(1:ld) // '_f.csv'
    ssnormal = testpath(1:lp) // testproj(1:ld) // '_n.csv'
    ssspecies = testpath(1:lp) // testproj(1:ld) // '_s.csv'
    ssspeciesmass = testpath(1:lp) // testproj(1:ld) // '_m.csv'
    sswall = testpath(1:lp) // testproj(1:ld) // '_w.csv'
    gitfile = testpath(1:lp) // testproj(1:ld) // '_git.txt'
    errorlogging = testpath(1:lp) // testproj(1:ld) // '.log'
    stopfile = testpath(1:lp) // testproj(1:ld) // '.stop'
    residfile = testpath(1:lp) // testproj(1:ld) // '.debug'
    residcsv = testpath(1:lp) // testproj(1:ld) // '_resid.csv'
    queryfile = testpath(1:lp) // testproj(1:ld) // '.query'
    statusfile = testpath(1:lp) // testproj(1:ld) // '.status'
    slabcsv = testpath(1:lp) // testproj(1:ld) // '_slab.csv'
    kernelisrunning = testpath(1:lp) // testproj(1:ld) // '.kernelisrunning'

    testpath = trim (exepath)
    lp = len_trim (testpath)
    solverini = testpath(1:lp) // 'solver.ini'

    open (unit=1, file=inputfile, action='read', status='old', iostat=ios)

    ! output the revision for later identification of validation plots
    if (validate) then
        call deleteoutputfiles (gitfile)
        open (unit=3, file=gitfile, action='write', iostat=ios, status='new')
        if (ios==0) then
            call get_info(revision, revision_date, compile_date)
            write (3,'(a)') revision
            close (unit=3)
        end if
    end if

    ! open the log file to write error messages and such
    call deleteoutputfiles (errorlogging)
    open (unit=3, file=errorlogging, action='write', iostat=ios, status='new')
    if (ios/=0) then
        write (*,'(a,i0,a)') 'Error opening log file, returned status = ', ios, '. Log file may be in use by another application.'
        stop
    end if

    call deleteoutputfiles (outputfile)
    call deleteoutputfiles (smvhead)
    call deleteoutputfiles (smvdata)
    call deleteoutputfiles (smvcsv)
    call deleteoutputfiles (ssflow)
    call deleteoutputfiles (ssnormal)
    call deleteoutputfiles (ssspecies)
    call deleteoutputfiles (ssspeciesmass)
    call deleteoutputfiles (sswall)
    call deleteoutputfiles (statusfile)
    call deleteoutputfiles (queryfile)
    call deleteoutputfiles (residcsv)
    call deleteoutputfiles (slabcsv)
    call deleteoutputfiles (kernelisrunning)

    ! since we have reached this point, the output files are available and stop has been turned off.
    project = testproj (1:ld)
    return

    end subroutine open_files

    ! --------------------------- read_solver_ini -------------------------------------------

    subroutine read_solver_ini

    !     routine: read_solver_ini
    !     purpose: this routine initializes the solver variables from solver.ini if it exists
    !     arguments: none

    real(eb) :: fract1, fract2, fract3, fsum, ductcv
    integer :: nopt, i, j, ibeg, iend
    logical existed

    ductcv = 0.0_eb

    inquire (file=solverini,exist=existed)
    if (.not.existed) return
    close (iofili)
    write (*, '(2a)') '***** modify dassl tolerances with ', solverini
    write (iofill, '(2a)') '***** modify dassl tolerances with ', solverini
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
    else if (option(fdebug)>=3) then
        option(fdebug) = on
    end if

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
    read (iofili,*) stpmax, stpfirst

    ! read in hvac convection coefficient (not currently used)
    read (iofili,*)
    read (iofili,*) ductcv

    ! read in jacobian and snsqe print flags
    read (iofili,*)
    read (iofili,*) jacchk, cutjac, iprtalg
    close (iofili)

    return
    end subroutine read_solver_ini

    ! --------------------------- positionobject -------------------------------------------

    subroutine positionobject (xyorz,pos_max,defaultposition,minimumseparation)

    !     routine: positionobject
    !     purpose: Position an object in a compartment
    !     arguments: xyorz: object position in the x, y, or z direction
    !		         pos_max: the maximum extent
    !		         defaultposition: to set to zero (base)(2) or midpoint(1)
    !		         minimumseparation: the closest the object can be to a wall

    integer, intent(in) :: defaultposition
    real(eb), intent(in) :: minimumseparation, pos_max
    real(eb), intent(inout) :: xyorz

    if ((xyorz<0.0_eb).or.(xyorz>pos_max)) then
        select case (defaultposition)
        case (1)
            xyorz = pos_max / 2.0_eb
        case (2)
            xyorz = minimumseparation
            case default
            write (*,*) 'Fire objects positioned specified outside compartment bounds.'
            write (iofill,*) 'Fire objects positioned specified outside compartment bounds.'
            stop
        end select
    else if (xyorz==0.0_eb) then
        xyorz = minimumseparation
    else if (xyorz==pos_max) then
        xyorz = pos_max - minimumseparation
    end if

    return

    end subroutine positionobject

    ! --------------------------- readcsvformat -------------------------------------------

    subroutine readcsvformat (iunit, x, c, numr, numc, nstart, maxrow, maxcol, iofill)

    !     routine: readcsvformat
    !     purpose: reads a comma-delimited file as generated by Micorsoft Excel, assuming that all
    !              the data is in the form of real numbers
    !     arguments: iunit  = logical unit, already open to .csv file
    !                x      = array of dimension (numr,numc) for values in spreadsheet
    !                c      = character array of same dimenaion as x for character values in spreadsheet
    !                numr   = # of rows of array x
    !                numc   = # of columns of array x
    !                nstart = starting row of spreadsheet to read
    !                maxrow   = actual number of rows read
    !                maxcol   = actual number of columns read
    !                iofill   = logical unit number for writing error messages (if any)

    integer, intent(in) :: iunit, numr, numc, nstart, iofill

    integer, intent(out) :: maxrow, maxcol
    real(eb), intent(out) :: x(numr,numc)
    character, intent(out) :: c(numr,numc)*(*)

    character :: in*10000, token*128
    integer :: i, j, nrcurrent, ic, icomma, ios, nc

    maxrow = 0
    maxcol = 0
    do i=1,numr
        do j=1,numc
            x(i,j) = 0.0_eb
            c(i,j) = ' '
        end do
    end do

    ! if we have header rows, then skip them
    if (nstart>1) then
        do  i=1,nstart-1
            read (iunit,'(A)') in
        end do
    end if

    ! read the data
    nrcurrent = 0
20  read (iunit,'(A)',end=100) in

    ! Skip comments and blank lines
    if (in(1:1)=='!'.or.in(1:1)=='#'.or.in==' ') then
        go to 20
    end if

    nrcurrent = nrcurrent+1
    maxrow = max(maxrow,nrcurrent)

    ! Cannot exceed work array
    if (maxrow>numr) then
        write (*,'(a,i0,1x,i0)') '***Error: Too many rows or columns in input file, r,c = ', maxrow, maxcol
        write (iofill,'(a,i0,1x,i0)') '***Error: Too many rows or columns in input file, r,c = ', maxrow, maxcol
        stop
    end if

    nc=0
    ic=1
30  icomma=index(in,',')
    if (icomma/=0) then
        if (icomma==ic) then
            token=' '
        else
            token=in(ic:icomma-1)
        end if
        ic = icomma+1
        nc = nc + 1
        in(1:ic-1)=' '
        if (nrcurrent<=numr.and.nc<=numc) then
            c(nrcurrent,nc) = token
            read (token,'(f128.0)',iostat=ios) x(nrcurrent,nc)
            if (ios/=0) x(nrcurrent,nc) = 0
        else
            write (*,'(a,i0,a,i0)') 'Too many rows or columns in input file, r,c=', nrcurrent, ' ', nc
            write (iofill,'(a,i0,a,i0)') 'Too many rows or columns in input file, r,c=', nrcurrent, ' ', nc
            stop
        end if
        go to 30
    end if
    nc = nc + 1
    maxcol=max(maxcol,nc)
    token = in(ic:ic+100)
    c(nrcurrent,nc) = token
    read (token,'(f128.0)',iostat=ios) x(nrcurrent,nc)
    if (ios/=0) x(nrcurrent,nc) = 0
    go to 20

100 continue

    return
    end subroutine readcsvformat

    ! --------------------------- setup_slice_iso -------------------------------------------

    subroutine setup_slice_iso

    integer :: nrm

    integer :: i,j,k,iroom,islice
    type(slice_type), pointer :: sliceptr
    type(room_type), pointer :: roomptr
    real(eb) :: xb(6)
    character(256) :: slicefilename
    integer :: ijkslice(6)
    real(eb), parameter :: dxyz=0.01_eb
    character(60) :: menu_label, colorbar_label, unit_label
    integer :: i_iso
    type(iso_type), pointer :: isoptr
    character(256) :: isofilename
    real(eb) :: ceiljet_depth
    type(visual_type), pointer :: vptr
    integer :: ntypes, ir, ibeg, iend
    real(eb) :: position_offset
    integer skipslice


    nsliceinfo = 0
    nisoinfo = 0
    if (nvisualinfo.eq.0)return

    nrm = nrm1

    ! count number of isosurfaces and slices

    do i = 1, nvisualinfo
        vptr=>visualinfo(i)
        if (vptr%roomnum.eq.0) then
            ntypes=nrm
        else
            ntypes=1
        end if
        if (vptr%vtype.eq.1.or.vptr%vtype.eq.2) then
            nsliceinfo = nsliceinfo + ntypes
        else
            nisoinfo = nisoinfo + ntypes
        end if
    end do
    nsliceinfo = 5*nsliceinfo
    allocate(sliceinfo(nsliceinfo))
    allocate(isoinfo(nisoinfo))

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

    do i = 1, nvisualinfo
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
    do i = 1, nvisualinfo
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

    ! --------------------------- set_grid -------------------------------------------

    subroutine set_grid (xgrid,nr,xmin,xsplit,xmax,nsplit)

    integer, intent(in) :: nr, nsplit
    real(eb), dimension(nr), intent(out) :: xgrid
    real(eb), intent(in) :: xmin, xsplit, xmax

    real(eb) :: factor
    integer :: i

    !   1            nr-nsplit          nr
    !  xmin          xsplit          xmax

    do i = 1, nr-nsplit
        factor = real(i-1,eb)/real(nr-nsplit-1,eb)
        xgrid(i) = emix(factor,xmin,xsplit)
    end do

    do i = nr-nsplit+1, nr
        factor = real(i-(nr-nsplit),eb)/real(nsplit,eb)
        xgrid(i) = emix(factor,xsplit,xmax)
    end do

    end subroutine set_grid

    end module input_routines
