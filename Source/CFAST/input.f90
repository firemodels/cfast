    module input_routines

    use precision_parameters

    use fire_routines, only: flame_height
    use initialization_routines, only : initialize_targets, initialize_ambient, offset
    use numerics_routines, only : dnrm2
    use output_routines, only: openoutputfiles, deleteoutputfiles
    use utility_routines, only: countargs, upperall, exehandle, emix
    use namelist_input_routines, only: namelist_input
    use spreadsheet_input_routines, only: spreadsheet_input

    use wallptrs
    use cenviro
    use ramp_data
    use cparams, only: mx_hsep
    use setup_data
    use diag_data
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

    private

    public read_input_file, open_files

    contains

    ! --------------------------- read_input_file -------------------------------------------

    subroutine read_input_file ()

    ! read the input file and set up the data for processing

    implicit none

    real(eb) :: temparea(mxpts), temphgt(mxpts), deps1, dwall1, dwall2, rti
    real(eb) :: xloc, yloc, zloc, zbot, ztop, pyramid_height, dheight, xx, sum
    integer :: ios, i, ii, j, itop, ibot, nswall2, iroom, iroom1, iroom2
    integer :: iwall1, iwall2, itype, npts, ioff, ioff2
    
    type(room_type), pointer :: roomptr, roomptr2
    type(fire_type), pointer :: fireptr
    type(detector_type), pointer :: dtectptr
    type(vent_type), pointer :: ventptr
    type(thermal_type), pointer :: thrmpptr

    ! deal with opening the data file and assuring ourselves that it is compatible
    close (iofili)
    open (unit=iofili,file=inputfile,status='OLD',iostat=ios)
    if (ios/=0) then
        if (iofill>0) then
            write (*,5050) modulo(ios,256)
            write (iofill,5050) modulo(ios,256)
        else
            write (*,5050) modulo(ios,256)
        end if
        stop
    end if

    n_thrmp = 0
 
    ! read in the input file in new (namelist) or old (.csv) format
    if (nmlflag) then
        call namelist_input
    else
        call spreadsheet_input
    end if

    ! add the default thermal property
    n_thrmp = n_thrmp + 1
    thrmpptr => thermalinfo(n_thrmp)
    thrmpptr%name = 'DEFAULT'
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
            stop
        end if
        if (interior_ambient_temperature>373.15_eb.or.interior_ambient_temperature<223.15_eb) then
            write (*,5022) interior_ambient_temperature
            write (iofill,5022) interior_ambient_temperature
            if (.not.radi_verification_flag) stop
        end if
    end if

    ! make pressures consistent with temperatures
    interior_abs_pressure = exterior_abs_pressure*interior_ambient_temperature/exterior_ambient_temperature
    pressure_ref = exterior_abs_pressure
    pressure_offset = pressure_ref

    ! compartment geometry related data
    nrm1 = nr - 1
    do i = 1, nrm1
        roomptr => roominfo(i)
        roomptr%x1 = roomptr%x0 + roomptr%cwidth
        roomptr%y1 = roomptr%y0 + roomptr%cdepth
        roomptr%z1 = roomptr%z0 + roomptr%cheight
    end do

    ! we now know what output is going to be generated, so create the files
    call openoutputfiles

    interior_rho = interior_abs_pressure/interior_ambient_temperature/rgas
    exterior_rho = exterior_abs_pressure/exterior_ambient_temperature/rgas

    ! initialize the targets.
    call initialize_targets

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
        write (*,*) '***Error: Specified LOI is less than zero or greater than ambient. Default of 0.15 used'
        write (iofill,*) '***Error: Specified LOI is less than zero or greater than ambient. Default of 0.15 used'
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
    call initialize_ambient

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

5022 format ('***Error: Initial temperature outside of allowable range (-50 to +100)',f5.2)

    ! read format list
5050 format ('***Error: Error opening the input file = ',I6)

    end subroutine read_input_file

    ! --------------------------- open_files -------------------------------------------

    subroutine open_files ()
    
    use namelist_data

    ! get the paths and project base name open the input file for reading
    ! delete the output files
    ! open the log file

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
    ssconnections = datapath(1:lp) // project(1:ld) // '_c.csv'
    ssflow = datapath(1:lp) // project(1:ld) // '_f.csv'
    ssnormal = datapath(1:lp) // project(1:ld) // '_n.csv'
    ssspecies = datapath(1:lp) // project(1:ld) // '_s.csv'
    ssspeciesmass = datapath(1:lp) // project(1:ld) // '_m.csv'
    sswall = datapath(1:lp) // project(1:ld) // '_w.csv'
    ssdiag = datapath(1:lp) // project(1:ld) // '_d.csv'
    gitfile = datapath(1:lp) // project(1:ld) // '_git.txt'
    errorlogging = datapath(1:lp) // project(1:ld) // '.log'
    stopfile = datapath(1:lp) // project(1:ld) // '.stop'
    residfile = datapath(1:lp) // project(1:ld) // '.debug'
    residcsv = datapath(1:lp) // project(1:ld) // '_resid.csv'
    queryfile = datapath(1:lp) // project(1:ld) // '.query'
    statusfile = datapath(1:lp) // project(1:ld) // '.status'
    slabcsv = datapath(1:lp) // project(1:ld) // '_slab.csv'
    kernelisrunning = datapath(1:lp) // project(1:ld) // '.kernelisrunning'

    lp = len_trim (exepath)
    solverini = datapath(1:lp) // 'solver.ini'

    ! open input file and check to see if it's a new (namelist) format file
    open (unit=1, file=inputfile, action='read', status='old', iostat=ios)
    read (unit=1,fmt='(a)') buf
    rewind (unit=1)
    if (buf(1:5)==heading) then
        nmlflag = .false.
    else if (buf(1:1)=='&') then
        nmlflag = .true.
    else
        write (*,*) ' Input file format not recognized. Check first line of input file.'
    end if
 
    ! output the revision for later identification of validation plots
    if (validation_flag) then
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
    call deleteoutputfiles (smvsinfo)
    call deleteoutputfiles (ssflow)
    call deleteoutputfiles (ssconnections)
    call deleteoutputfiles (ssnormal)
    call deleteoutputfiles (ssspecies)
    call deleteoutputfiles (ssspeciesmass)
    call deleteoutputfiles (sswall)
    call deleteoutputfiles (statusfile)
    call deleteoutputfiles (queryfile)
    call deleteoutputfiles (residcsv)
    call deleteoutputfiles (slabcsv)
    call deleteoutputfiles (kernelisrunning)

    return

    end subroutine open_files

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

    ! ------------------ get_igrid ------------------------

    integer function get_igrid (x,xgrid,nr)

    integer, intent(in) :: nr
    real(eb), intent(in), dimension(0:nr) :: xgrid
    real(eb), intent(in) :: x

    integer :: i

    do i = 0, nr-1
        if (xgrid(i).le.x.and.x.lt.xgrid(i+1)) then
            get_igrid=i
            return
        end if
    end do
    if (xgrid(nr).eq.x) then
        get_igrid=nr
    else
        get_igrid=-1
    end if
    return
    end function get_igrid

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