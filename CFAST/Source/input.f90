module input_routines

    use precision_parameters

    use fire_routines, only: flame_height
    use initialization_routines, only : inittarg, initamb, offset, hvinit
    use numerics_routines, only : dnrm2
    use output_routines, only: openoutputfiles, deleteoutputfiles
    use utility_routines, only: countargs, get_igrid, upperall, exehandle, emix

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

    implicit none
    
    logical :: exset = .false.

    private

    public read_input_file, open_files, read_solver_ini

    contains

! --------------------------- read_input_file -------------------------------------------

    subroutine read_input_file ()

    !	Read the input file and set up the data for processing

    implicit none

    real(eb) :: yinter(mxrooms), temparea(mxcross), temphgt(mxcross), deps1, deps2, dwall1, dwall2, rti
    real(eb) :: xloc, yloc, zloc, pyramid_height, dheight, xx, sum
    integer :: numr, numc, ios, iversion, i, ii, j, jj, k, itop, ibot, nswall2, iroom, iroom1, iroom2
    integer :: iwall1, iwall2, itype, npts, ioff, ioff2, nventij, ivers
    character :: aversion*5
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    !	Unit numbers defined in read_command_options, openoutputfiles, readinputfiles
    !
    !      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
    !      3 is for the log file  (LOGERR)
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
        if (logerr>0) then
            write (logerr,5050) mod(ios,256)
        else
            write (*,5050) mod(ios,256)
        end if
        stop
    end if

    ! read in the entire input file as a spreadsheet array of numbers and/or character strings
    call readcsvformat (iofili, rarray, carray, nrow, ncol, 1, numr, numc, logerr)

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
        write (logerr,5004) ivers, iversion
    elseif (aversion/=heading.or.ivers/=iversion) then
        write (logerr,5002) aversion,heading,ivers,iversion
        stop
    end if
    title = carray(1,3)

    do i = 1, mxrooms
        yinter(i) = -1.0_eb
    end do

    nthrmp = 0

    ! read in data file
    call keywordcases (numr, numc)

    !	wait until the input file is parsed before we die on temperature outside reasonable limits
    if (exterior_temperature>373.15_eb.or.exterior_temperature<223.15_eb) then
        write(logerr,5022) exterior_temperature
        stop
    end if
    if (interior_temperature>373.15_eb.or.interior_temperature<223.15_eb) then
        write(logerr,5022) interior_temperature
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

    ! check and/or set heat source fire position
    if (heatfl) then
        roomptr => roominfo(heatfr)
        if ((heatfp(1)<0.0_eb).or.(heatfp(1)>roomptr%cwidth)) heatfp(1) = roomptr%cwidth/2.0_eb
        if ((heatfp(2)<0.0_eb).or.(heatfp(2)>roomptr%cdepth)) heatfp(2) = roomptr%cdepth/2.0_eb
        if ((heatfp(3)<0.0_eb).or.(heatfp(3)>roomptr%cheight)) heatfp(3) = 0.0_eb
    end if

    ! check and/or set position of fire objects
    do i = 1, numobjl
        roomptr => roominfo(objrm(i))
        if((objpos(1,i)<0.0_eb).or.(objpos(1,i)>roomptr%cwidth)) objpos(1,i) = roomptr%cwidth/2.0_eb
        if((objpos(2,i)<0.0_eb).or.(objpos(2,i)>roomptr%cdepth)) objpos(2,i) = roomptr%cdepth/2.0_eb
        if((objpos(3,i)<0.0_eb).or.(objpos(3,i)>roomptr%cheight)) objpos(3,i) = 0.0_eb
    end do

    ! make sure ceiling/floor vent specifications are correct -  we have to do this
    ! here rather than right after keywordcases because floor_height and ceiling_height were just defined
    ! above
    do itop = 1, nrm1
        if (ivvent_connections(itop,itop)/=0) then
            write (logerr,*) '***Error: A room can not be connected to itself with a vertical vent'
            stop
        end if
        do ibot = 1, itop - 1
            if (ivvent_connections(itop,ibot)/=0.or.ivvent_connections(ibot,itop)/=0) then

                ! see which room is on top (if any) - this is like a bubble sort
                deps1 = roominfo(itop)%z0 - roominfo(ibot)%z1
                deps2 = roominfo(ibot)%z0 - roominfo(itop)%z1
                if (ivvent_connections(itop,ibot)/=1.or.abs(deps1)>=mx_vsep) then
                    if (ivvent_connections(ibot,itop)/=1.or.abs(deps2)>=mx_vsep) then
                        if (ivvent_connections(itop,ibot)==1.and.abs(deps2)<mx_vsep) then
                            if (ivvent_connections(ibot,itop)/=0) then
                                write (logerr,*) '***Error: Vertical vent ', ibot, itop, ' is being redefined'
                            end if
                            ivvent_connections(itop,ibot) = 0
                            ivvent_connections(ibot,itop) = 1
                            vvarea(ibot,itop) = vvarea(itop,ibot)
                            vshape(ibot,itop) = vshape(itop,ibot)
                            cycle
                        end if
                        if (ivvent_connections(ibot,itop)==1.and.abs(deps1)<mx_vsep) then
                            if (ivvent_connections(itop,ibot)/=0) then
                                write (logerr,*) '***Error: Vertical vent ', itop, ibot, ' is being redefined'
                            end if
                            ivvent_connections(itop,ibot) = 1
                            ivvent_connections(ibot,itop) = 0
                            vvarea(itop,ibot) = vvarea(ibot,itop)
                            vshape(itop,ibot) = vshape(ibot,itop)
                            cycle
                        end if
                        ivvent_connections(itop,ibot) = 0
                        ivvent_connections(ibot,itop) = 0
                    end if
                end if
            end if
        end do
    end do

    ! Compartment area and volume
    do i = 1, nrm1
        roomptr => roominfo(i)
        roomptr%floor_area = roomptr%cwidth*roomptr%cdepth
        roomptr%cvolume = roomptr%floor_area*roomptr%cheight
    end do


    ! check room to room heat transfer parameters (cfcon command)
    nswall2 = nswal
    ii = 0
    do i = 1, nswal
        iroom1 = izswal(i,w_from_room)
        iroom2 = izswal(i,w_to_room)

        ! room numbers must be between 1 and nrm1
        if(iroom1<1.or.iroom2<1.or.iroom1>nrm1+1.or.iroom2>nrm1+1)then
            write (logerr,201) iroom1, iroom2
201         format('***Error: Invalid CFCON specification:',' one or both of rooms ',i0,'-',i0,' do not exist')
            stop
        end if

        ! if room is connected to the outside then ignore it
        if(iroom1==nrm1+1.or.iroom2==nrm1+1)then
            nswall2 = nswall2 - 1
            cycle
        else
            ii = ii + 1
            if(i/=ii)then
                izswal(ii,w_from_room) = izswal(i,w_from_room)
                izswal(ii,w_from_wall) = izswal(i,w_from_wall)
                izswal(ii,w_to_room) = izswal(i,w_to_room)
                izswal(ii,w_to_wall) = izswal(i,w_to_wall)
            end if
        end if

        ! floor of one room must be adjacent to ceiling of the other
        dwall1 = abs(roominfo(iroom1)%z0 - roominfo(iroom2)%z1)
        dwall2 = abs(roominfo(iroom2)%z0 - roominfo(iroom1)%z1)
        if(dwall1<mx_vsep.or.dwall2<=mx_vsep)then
            if(dwall1<mx_vsep)then
                izswal(ii,w_from_wall) = 2
                izswal(ii,w_to_wall) = 1
            else
                izswal(ii,w_from_wall) = 1
                izswal(ii,w_to_wall) = 2
            end if
        else
            write (logerr,202) iroom1, iroom2
202         format('***Error: Invalid CFCON specification:'' ceiling and floor of rooms',i0,'-',i0,' are not connectetd')
            stop
        end if

        ! walls must be turned on, ie surface_on must be set
        ! for the ceiling in the lower room and the floor of the upper room
        iwall1 = izswal(ii,w_from_wall)
        iwall2 = izswal(ii,w_to_wall)
        if(.not.roominfo(iroom1)%surface_on(iwall1).or..not.roominfo(iroom2)%surface_on(iwall2))then
            if(.not.roominfo(iroom1)%surface_on(iwall1))then
                write(logerr,204) iwall1, iroom1
204             format('***Error: Invalid CFCON specification. Wall ',i0,' of room ',i0,' is adiabatic')
            else
                write(logerr,204)iwall2, iroom2
            end if
            stop
        end if
    end do
    nswal = nswall2

    ! check shafts
    do iroom = nrm1 + 1, mxrooms
        roomptr => roominfo(iroom)
        if(roomptr%shaft)then
            write (logerr,'(a,i0,a,i0)') &
                '***Error: Invalid SHAFT specification. Room',iroom,'must be less than or equal to ',nrm1
            stop
        end if
    end do

    ! check variable cross-sectional area specs and convert to volume
    do i = 1, nrm1
        roomptr => roominfo(i)
        npts = roomptr%nvars
        if(npts/=0)then

            ! force first elevation to be at the floor; add a data point if necessary (same area as first entered data point)
            if(roomptr%var_height(1)/=0.0_eb)then
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
            if(roomptr%cheight/=roomptr%var_height(npts))then
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
                if(roomptr%var_area(j)/=roomptr%var_area(j-1)) then
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

    ! initialize the mechanical ventilation
    call hvinit

    ! check detectors
    do i = 1, ndtect
        dtectptr => detectorinfo(i)
        iroom = dtectptr%room
        if(iroom<1.or.iroom>nrm1)then
            write (logerr,104) iroom
104         format('***Error: Invalid DETECTOR specification. Room ',i3, ' is not a valid')
            stop
        end if
        roomptr => roominfo(iroom)

        rti = dtectptr%rti
        itype = dtectptr%dtype
        if(rti<=0.0_eb.and.itype/=smoked)then
            write (logerr,101) rti
101         format('***Error: Invalid DETECTOR specification. RTI = ',e11.4, ' is not a valid.')
            stop
        end if

        xloc = dtectptr%center(1)
        yloc = dtectptr%center(2)
        zloc = dtectptr%center(3)
        if(xloc<0.0_eb.or.xloc>roomptr%cwidth.or.yloc<0.0_eb.or.yloc>roomptr%cdepth.or.zloc<0.0_eb.or.zloc>roomptr%cheight) then
            write(logerr,102) xloc,yloc,zloc
102         format('***Error: Invalid DETECTOR specification. X,Y,Z,location =',3e11.4,' is out of bounds')
            stop
        end if

        itype = dtectptr%dtype
        if(itype<1.or.itype>3)then
            write(logerr,103) itype
103         format('***Error: Invalid DETECTOR specification - type= ',i2,' is not a valid')
            stop
        end if
    end do

    ! check room to room heat transfer

    ! The array iheat may have one of three values, 0, 1, 2.
    ! 0 = no room to room heat transfer
    ! 1 = fractions are determined by what rooms are connected by vents
    ! For example, if room 1 is connected to rooms 2, 3, 4 and the outside
    ! by vents then the first row of heat_frac will have the values
    ! 0. .25 .25 .25 .25

    ! force all rooms to transfer heat between connected rooms
    if(iheat(0)==1)then
        do i = 1, nrm1
            iheat(i) = 1
        end do
    end if

    do i = 1, nrm1

        ! force heat transfer between rooms connected by vents.
        if(iheat(i)==1)then
            do j = 1, nr
                roomptr => roominfo(j)
                nventij = 0
                do k = 1, 4
                    nventij = nventij + ijk(i,j,k)
                end do
                if(nventij/=0)heat_frac(i,j) = 1.0_eb

                ! if the back wall is not active then don't consider its contribution
                if(j<=nrm1.and..not.roomptr%surface_on(3)) heat_frac(i,j) = 0.0_eb
            end do
        end if

        ! normalize heat_frac fraction matrix so that rows sum to one
        if(iheat(i)/=0)then
            sum = 0.0_eb
            do j = 1, nrm1+1
                sum = sum + heat_frac(i,j)
            end do
            if(sum<1.e-5_eb)then
                do j = 1, nrm1
                    heat_frac(i,j) = 0.0_eb
                end do
                heat_frac(i,nrm1+1) = 1.0_eb
            else
                do j = 1, nrm1+1
                    heat_frac(i,j) = heat_frac(i,j)/sum
                end do
            end if
            jj = 0
            do j = 1, nrm1
                if(heat_frac(i,j)/=0.0_eb)then
                    iheat_connections(i,0) = iheat_connections(i,0) + 1
                    jj = jj + 1
                    iheat_connections(i,jj) = j
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

    integer :: obpnt, i1, i2, fannumber, iecfrom, iecto, mid, i, j, k, ir
    integer :: iijk, jik, koffst, jmax, itop, ibot, npts, nto, ifrom, ito, imin, iroom, iramp, ncomp
    real(eb) :: initialopening, lrarray(ncol),minpres, maxpres, heightfrom, heightto, areafrom, areato
    real(eb) :: frac, tmpcond
    character :: label*5, tcname*64, eqtype*3, venttype,orientypefrom*1, orientypeto*1
    character(128) :: lcarray(ncol)
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(detector_type), pointer :: dtectptr
    type(ramp_type), pointer :: rampptr
    type(visual_type), pointer :: sliceptr
    type(thermal_type), pointer :: thrmpptr

    !	Start with a clean slate

    do i = 1, mxrooms
        roomptr => roominfo(i)
        do j = 1, 4
            roomptr%matl(j) = 'OFF'
            roomptr%surface_on(j) = .false.
        end do
    end do
    ncomp = 0

    ! Read in thermal properties first
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
            if(countargs(lcarray)>=7) then
                nthrmp = nthrmp + 1
                if (nthrmp>mxthrmp) then
                    write (logerr,'(a,i3)') '***Error: Bad MATL input. Too many thermal properties in input data file. Limit is ', &
                        mxthrmp
                    stop
                end if
                thrmpptr => thermalinfo(nthrmp)
                thrmpptr%name = lcarray(1)
                thrmpptr%nslab = 1
                thrmpptr%k(1) = lrarray(2)
                thrmpptr%c(1) = lrarray(3)
                thrmpptr%rho(1) = lrarray(4)
                thrmpptr%thickness(1) = lrarray(5)
                thrmpptr%eps = lrarray(6)
            else
                write (logerr,*) '***Error: Bad MATL input. At least 7 arguments required.'
                stop
            end if
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
                    write (logerr, 5062) ncomp
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
                write (logerr,*) '***Error: Bad COMPA input. At least 10 arguments required.'
                stop
            end if
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
                if(ntarg+1>mxtarg)then
                    write(logerr,5002)
                    stop
                end if

                ! The target can exist, now for the compartment
                ntarg = ntarg + 1
                iroom = lrarray(1)
                if(iroom<1.or.iroom>nr)then
                    write(logerr,5003) iroom
                    stop
                end if
                targptr => targetinfo(ntarg)
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
                    write (targptr%name,'(a5,i0)') 'Targ ', ntarg
                end if

                ! material type
                tcname = lcarray(8)
                if(tcname==' ') tcname='DEFAULT'
                targptr%material = tcname
                targptr%wall = 0

                ! equation type, PDE or CYL.  ODE is outdated and changed to PDE if it's in an input file.
                eqtype = ' '
                eqtype = lcarray(10)
                call upperall(eqtype)
                if(eqtype/=' ')then
                    if (eqtype(1:3)=='ODE') then
                        targptr%equaton_type = pde
                        write (logerr,913) 'Warning', eqtype
                    elseif (eqtype(1:3)=='PDE') then
                        targptr%equaton_type = pde
                    elseif (eqtype(1:3)=='CYL') then
                        targptr%equaton_type = cylpde
                    else
                        write(logerr,913) 'Error',eqtype
                        stop
                    end if
                end if
            else
                write (logerr,*) '***Error: Bad TARGE input. At least 10 arguments required.'
                stop
            end if
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
                write (logerr,*) '***Error: Bad FIRE input. 11 arguments required.'
                stop
            end if
            if (numobjl>=mxfires) then
                write(logerr,5300)
                stop
            end if
            iroom = lrarray(1)
            if (iroom<1.or.iroom>nr-1) then
                write(logerr,5320)iroom
                stop
            end if
            obpnt = numobjl + 1
            numobjl = obpnt
            roomptr => roominfo(iroom)

            ! Only constrained fires
            objtyp(numobjl) = 2
            if (objtyp(numobjl)>2) then
                write(logerr,5321) objtyp(numobjl)
                stop
            end if

            objpos(1,obpnt) = lrarray(2)
            objpos(2,obpnt) = lrarray(3)
            objpos(3,obpnt) = lrarray(4)
            if (objpos(1,obpnt)>roomptr%cwidth.or.objpos(2,obpnt)>roomptr%cdepth.or.objpos(3,obpnt)>roomptr%cheight) then
                write(logerr,5323) obpnt
                stop
            end if
            obj_fpos(obpnt) = 1
            if (min(objpos(1,obpnt),roomptr%cwidth-objpos(1,obpnt))<=mx_hsep .or. &
                min(objpos(2,obpnt),roomptr%cdepth-objpos(2,obpnt))<=mx_hsep) obj_fpos(obpnt) = 2
            if (min(objpos(1,obpnt),roomptr%cwidth-objpos(1,obpnt))<=mx_hsep .and. &
                min(objpos(2,obpnt),roomptr%cdepth-objpos(2,obpnt))<=mx_hsep) obj_fpos(obpnt) = 3

            if (lcarray(6)=='TIME' .or. lcarray(6)=='TEMP' .or. lcarray(6)=='FLUX') then
                ! it's a new format fire line that point to an existing target rather than to one created for the fire
                if (lcarray(6)=='TIME') objign(obpnt) = 1
                if (lcarray(6)=='TEMP') objign(obpnt) = 2
                if (lcarray(6)=='FLUX') objign(obpnt) = 3
                tmpcond = lrarray(7)
                obtarg(obpnt) = 0
                if (lcarray(6)=='TEMP' .or. lcarray(6)=='FLUX') then
                    do i = 1,ntarg
                        targptr => targetinfo(i)
                        if (targptr%name==lcarray(8)) obtarg(obpnt) = i
                    end do
                    if (obtarg(obpnt)==0) then
                        write (logerr,5324) obpnt
                        stop
                    end if
                end if
            else
                ! it's the old format fire line that creates a target for each fire
                objign(obpnt) =   lrarray(6)
                tmpcond =         lrarray(7)
                objort(1,obpnt) = lrarray(8)
                objort(2,obpnt) = lrarray(9)
                objort(3,obpnt) = lrarray(10)

                ! Enforce sanity; normal pointing vector must be non-zero (blas routine)
                if (dnrm2(3,objort(1,obpnt),1)<=0.0) then
                    write(logerr,5322)
                    stop
                end if
            end if
            objrm(obpnt) = iroom
            objnin(obpnt) = lcarray(11)
            objld(obpnt) = .true.
            objon(obpnt) = .false.
            ! This is redudant but needed to be compatible with the object database format
            objpnt(obpnt) = obpnt
            ! Note that ignition type 1 is time, type 2 is temperature and 3 is flux
            ! The critiria for temperature and flux are stored backupwards - this is historical
            ! See corresponding code in update_fire_objects
            if (tmpcond>0.0_eb) then
                if (objign(obpnt)==1) then
                    objcri(1,obpnt) = tmpcond
                    objcri(2,obpnt) = 1.0e30_eb
                    objcri(3,obpnt) = 1.0e30_eb
                else if (objign(obpnt)==2) then
                    objcri(1,obpnt) = 1.0e30_eb
                    objcri(2,obpnt) = 1.0e30_eb
                    objcri(3,obpnt) = tmpcond
                else if (objign(obpnt)==3) then
                    objcri(1,obpnt) = 1.0e30_eb
                    objcri(2,obpnt) = tmpcond
                    objcri(3,obpnt) = 1.0e30_eb
                else
                    write(logerr,5358) objign(obpnt)
                    stop
                end if
            else
                objon(obpnt) = .true.
            end if
            if (option(fbtobj)==off.and.objign(obpnt)/=1.0_eb) then
                if (stpmax>0.0_eb) then
                    stpmax = min(stpmax,1.0_eb)
                else
                    stpmax = 1.0_eb
                end if
            end if

            ! read and set the other stuff for this fire
            call inputembeddedfire (objnin(obpnt), ir, inumc, obpnt)
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
                write (logerr,*) '***Error: Bad TIMES input. At least 4 arguments required.'
                stop
            end if

            ! TAMB reference ambient temperature (c), reference ambient pressure, reference pressure, relative humidity
        case ("TAMB")
            if (countargs(lcarray)>=4) then
                interior_temperature = lrarray(1)
                interior_abs_pressure = lrarray(2)
                relative_humidity = lrarray(4)*0.01_eb
            elseif (countargs(lcarray)>=3) then
                interior_temperature = lrarray(1)
                interior_abs_pressure = lrarray(2)
                relative_humidity = lrarray(3)*0.01_eb
            else
                write (logerr,*) '***Error: Bad TAMB input. At least 3 arguments required.'
                stop
            end if
            if (.not.exset) then
                exterior_temperature = interior_temperature
                exterior_abs_pressure = interior_abs_pressure
                exterior_rho = interior_rho
            end if
            tgignt = interior_temperature + 200.0_eb

            ! EAMB reference external ambient temperature (c), reference external ambient pressure
        case ("EAMB")
            if (countargs(lcarray)/=3) then
                write (logerr,*) '***Error: Bad EAMB input. 3 arguments required.'
                stop
            end if
            exterior_temperature = lrarray(1)
            exterior_abs_pressure = lrarray(2)
            exset = .true.

            ! Rename the thermal data file
        case ("THRMF")
            if (countargs(lcarray)>=1) then
                thrmfile = lcarray(1)
            else
                write (logerr,*) '***Error: Bad THRMF input. 1 argument required.'
                stop
            end if

            ! HVENT 1st, 2nd, which_vent, width, soffit, sill, wind_coef, hall_1, hall_2, face, opening_fraction
            !		    bw = width, hh = soffit, hl = sill,
            !		    hhp = absolute height of the soffit,hlp = absolute height of the sill,
            !           floor_height = absolute height of the floor (not set here)
            !		    compartment offset for the hall command (2 of these)
            !		    vface = the relative face of the vent: 1-4 for x plane (-), y plane (+), x plane (+), y plane (-)
            !		    initial open fraction
        case ('HVENT')
            if (countargs(lcarray)<7) then
                write (logerr,*) '***Error: Bad HVENT input. At least 7 arguments required.'
                stop
            else
                i = lrarray(1)
                j = lrarray(2)
                k = lrarray(3)
                imin = min(i,j)
                jmax = max(i,j)
                if (imin>mxrooms-1.or.jmax>mxrooms.or.imin==jmax) then
                    write (logerr,5070) i, j
                    stop
                end if
                if (k>mxccv) then
                    write (logerr,5080) i, j, k, ihvent_connections(i,j)
                    stop
                end if
                nventijk = nventijk + 1
                if (nventijk>mxhvents) then
                    write(logerr,5081) i,j,k
                    stop
                end if
                ijk(i,j,k) = nventijk
                ijk(j,i,k) = ijk(i,j,k)
                iijk = ijk(i,j,k)
                jik = iijk
                koffst = 2**k
                ihvent_connections(i,j) = ior(ihvent_connections(i,j),koffst)
                bw(iijk) = lrarray(4)
                hh(iijk) = lrarray(5)
                hl(iijk) = lrarray(6)
            end if
            if (countargs(lcarray)>=11) then
                ventoffset(iijk,1) = lrarray(8)
                ventoffset(iijk,2) = lrarray(9)
                vface(iijk) = lrarray(10)
                initialopening = lrarray(11)
            else if (countargs(lcarray)>=9) then
                ventoffset(iijk,1) = lrarray(7)
                ventoffset(iijk,2) = 0.0_eb
                vface(iijk) = lrarray(8)
                initialopening = lrarray(9)
            else
                write (logerr,*) '***Error: Bad HVENT input. At least 7 arguments required.'
                stop
            end if

            qcvh(2,iijk) = initialopening
            qcvh(4,iijk) = initialopening

            roomptr => roominfo(i)
            hhp(iijk) = hh(iijk) + roomptr%z0
            hlp(iijk) = hl(iijk) + roomptr%z0

            ! connections are bidirectional

            ihvent_connections(j,i) = ihvent_connections(i,j)
            roomptr => roominfo(j)
            hh(jik) = min(roomptr%cheight,max(0.0_eb,hhp(jik)-roomptr%z0))
            hl(jik) = min(hh(jik),max(0.0_eb,hlp(jik)-roomptr%z0))

            ! assure ourselves that the connections are symmetrical

            hhp(jik) = hh(jik) + roomptr%z0
            hlp(jik) = hl(jik) + roomptr%z0
            roomptr => roominfo(i)
            hh(iijk) = min(roomptr%cheight,max(0.0_eb,hhp(iijk)-roomptr%z0))
            hl(iijk) = min(hh(iijk),max(0.0_eb,hlp(iijk)-roomptr%z0))

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

            ! EVENT keyword, the four possible formats are:
            ! EVENT   H     First_Compartment   Second_Compartment	 Vent_Number    Time   Final_Fraction   decay_time
            ! EVENT   V     First_Compartment   Second_Compartment	 Not_Used	    Time   Final_Fraction   decay_time
            ! EVENT   M        Not_Used             Not_used            M_ID        Time   Final_Fraction   decay_time
            ! EVENT   F        Not_Used             Not_used            M_ID        Time   Final_Fraction   decay_time
        case ('EVENT')
            if (countargs(lcarray)>=7) then
                !	        Sort by event type, h, v, m, or f
                venttype = lcarray(1)

                if(lrarray(6)<0.0_eb.or.lrarray(6)>1.0_eb) then
                    write(3,*) '****Error: Bad EVENT input. Final_Fraction (6th argument) must be between 0 and 1 inclusive.'
                    stop
                end if

                select case (venttype)
                case ('H')
                    i = lrarray(2)
                    j = lrarray(3)
                    k = lrarray(4)
                    iijk = ijk(i,j,k)
                    qcvh(1,iijk) = lrarray(5)
                    qcvh(3,iijk) = lrarray(5) + lrarray(7)
                    qcvh(4,iijk) = lrarray(6)
                case ('V')
                    ! Sort these out in update_data; we duplicate here so that read_input_file does not have to sort these as well
                    itop = lrarray(2)
                    ibot = lrarray(3)
                    qcvpp(1,itop,ibot) = lrarray(5)
                    qcvpp(3,itop,ibot) = lrarray(5) + lrarray(7)
                    qcvpp(4,itop,ibot) = lrarray(6)
                    qcvpp(1,ibot,itop) = lrarray(5)
                    qcvpp(3,ibot,itop) = lrarray(5) + lrarray(7)
                    qcvpp(4,ibot,itop) = lrarray(6)
                case ('M')
                    fannumber = lrarray(4)
                    qcvm(1,fannumber) = lrarray(5)
                    qcvm(3,fannumber) = lrarray(5) + lrarray(7)
                    qcvm(4,fannumber) = lrarray(6)
                case ('F')
                    fannumber = lrarray(4)
                    if (fannumber>nfan) then
                        write(logerr,5196) fannumber
                        stop
                    end if
                    nfilter = nfilter + 1
                    qcvf(1,fannumber) = lrarray(5)
                    qcvf(3,fannumber) = lrarray(5) + lrarray(7)
                    qcvf(4,fannumber) = lrarray(6)
                    case default
                    write (logerr,*) '***Error: Bad EVENT input. Type (1st arguement) must be H, V, M, or F.'
                    stop
                end select
            else
                write (logerr,*) '***Error: Bad EVENT input. At least 7 arguments required.'
                stop
            end if

            ! RAMP - from_compartment (or 0) to_compartment (or 0) vent_or_fire_number number_of_xy_pairs x1 y1 x2 y2 ... xn yn
        case ('RAMP')
            if (countargs(lcarray)<9) then
                write (logerr,*) '***Error: Bad RAMP input. At least 9 arguments required.'
                stop
            else if (lrarray(5)<=1) then
                write (logerr,*) '***Error: Bad RAMP input. At least 1 time point must be specified.'
                stop
            else if (countargs(lcarray)/=5+2*lrarray(5)) then
                write (logerr,*) '***Error: Bad RAMP input. Inputs must be in pairs.'
                stop
            end if
            if (nramps<=mxramps) then
                nramps = nramps + 1
                rampptr=>rampinfo(nramps)
                rampptr%type = lcarray(1)
                rampptr%from_room = lrarray(2)
                rampptr%to_room = lrarray(3)
                rampptr%vent_number = lrarray(4)
                rampptr%npoints = lrarray(5)
                do iramp = 1,rampptr%npoints
                    rampptr%time(iramp) = lrarray(4+2*iramp)
                    rampptr%value(iramp) = lrarray(5+2*iramp)
                end do
            end if

            ! VVENT - from_compartment to_compartment area shape initial_fraction
        case ('VVENT')
            if (countargs(lcarray)>=5) then
                i = lrarray(1)
                j = lrarray(2)
                ! check for outside of compartment space; self pointers are covered in read_input_file
                if (i>mxrooms.or.j>mxrooms) then
                    write (logerr,5070) i, j
                    stop
                end if

                ! read_input_file will verify the orientation (i is on top of j)
                ivvent_connections(i,j) = 1
                vvarea(i,j) = lrarray(3)
                ! check the shape parameter. the default (1) is a circle)
                if (lrarray(4)<1.or.lrarray(4)>2) then
                    vshape(i,j) = 1
                else
                    vshape(i,j) = lrarray(4)
                end if
                qcvpp(2,i,j) = lrarray(5)
                qcvpp(2,j,i) = lrarray(5)
                qcvpp(4,i,j) = lrarray(5)
                qcvpp(4,j,i) = lrarray(5)
            else
                write (logerr,*) '***Error: Bad VVENT input. At least 5 arguments required.'
                stop
            end if

            ! MVENT - simplified mechanical ventilation

            ! (1) From_Compartment, (2) To_Compartment, (3) ID_Number
            ! (4-6) From_Opening_Orientation From_Center_Height From_Opening_Area
            ! (7-9) To_Opening_Orientation To_Center_Height To_Opening_Area
            ! (10-12) Flow Flow_Begin_Dropoff_Pressure Zero_Flow_Pressure
            ! (13) Initial fraction of the fan speed
        case ('MVENT')
            if (countargs(lcarray)/=13) then
                write (logerr,*) '***Error: Bad MVENT input. 13 arguments required.'
                stop
            end if
            mid = lrarray(3)
            iecfrom = lrarray(1)
            iecto = lrarray(2)
            if (iecfrom>nr.or.iecto>nr) then
                write(logerr,5191) iecfrom, iecto
                stop
            end if

            orientypefrom = lcarray(4)
            heightfrom = lrarray(5)
            areafrom = lrarray(6)
            orientypeto = lcarray(7)
            heightto = lrarray(8)
            areato = lrarray(9)
            minpres = lrarray(11)
            maxpres = lrarray(12)

            ! We start with two new nodes for the openings into the compartments for connections to the fan

            ! first compartment/node opening
            next = next + 1
            nnode = nnode + 1
            if (next>mxext.or.nnode>mxnode) then
                write (logerr,5192) next,nnode
                stop
            end if
            if (orientypefrom=='V') then
                hvorien(next) = 1
            else
                hvorien(next) = 2
            end if
            hvnode(1,next) = iecfrom
            hvnode(2,next) = nnode
            hvelxt(next) = heightfrom
            arext(next) = areafrom

            ! second compartment/node opening
            next = next + 1
            nnode = nnode + 1
            if (next>mxext.or.nnode>mxnode) then
                write (logerr,5192) next,nnode
                stop
            end if
            if (orientypeto=='V') then
                hvorien(next) = 1
            else
                hvorien(next) = 2
            end if
            hvnode(1,next) = iecto
            hvnode(2,next) = nnode
            hvelxt(next) = heightto
            arext(next) = areato

            ! now connect nodes 1 and 2 with a fan

            if (minpres>maxpres) then
                write (logerr,5194) minpres,maxpres
                stop
            end if

            nfan = nfan + 1
            if (mid/=nfan) then
                write(logerr,5193) mid,nfan
                stop
            end if

            nbr = nbr + 1
            if (nfan>mxfan.or.nbr>mxbranch) then
                write (iofilo,5195) mxfan
                stop
            end if

            nf(nbr) = nfan
            nfc(nfan) = 1
            na(nbr) = hvnode(2,next-1)
            ne(nbr) = hvnode(2,next)
            hvdvol(nbr) = 0.0_eb
            hmin(nfan) = minpres
            hmax(nfan) = maxpres
            hvbco(nfan,1) = lrarray(10)

            ! add a simple duct to connect the two nodes/fan - this is artificial since we do not
            ! worry about the species within the system
            ndt = ndt + 1

            ! to change from the zero volume calculation to a finite volume, use 1.0d1 (1 meter duct)
            ! the effect is in hvfrex. case 1 is the finite volume and case 2, the zero volume calculation
            ! for flow through the external nodes
            duct_length(ndt) = 0.0_eb
            eff_duct_diameter(ndt) = lrarray(6)
            ibrd(ndt) = nbr

            ! finally, we set the initial fraction opening
            qcvm(2,mid) = lrarray(13)
            qcvm(4,mid) = lrarray(13)

            ! OBJEC name room pos(3) plume ignition_type ignition_criterion normal(3)
            ! This is the old format fire object specification
        case ('OBJEC')

            if (countargs(lcarray)/=11) then
                write(logerr,5310)
                stop
            end if
            if (numobjl>=mxfires) then
                write(logerr,5300)
                stop
            end if
            tcname = lcarray(1)
            iroom = lrarray(2)
            if (iroom<1.or.iroom>nr-1) then
                write(logerr,5320)iroom
                stop
            end if
            obpnt = numobjl + 1
            numobjl = obpnt
            roomptr => roominfo(iroom)

            ! Only constrained fires
            objtyp(numobjl) = 2
            if (objtyp(numobjl)>2) then
                write(logerr,5321) objtyp(numobjl)
                stop
            end if

            objpos(1,obpnt) = lrarray(3)
            objpos(2,obpnt) = lrarray(4)
            objpos(3,obpnt) = lrarray(5)
            if (objpos(1,obpnt)>roomptr%cwidth.or.objpos(2,obpnt)>roomptr%cdepth.or.objpos(3,obpnt)>roomptr%cheight) then
                write(logerr,5323) obpnt
                stop
            end if
            obj_fpos(obpnt) = 1
            if (min(objpos(1,obpnt),roomptr%cwidth-objpos(1,obpnt))<=mx_hsep .or. &
                min(objpos(2,obpnt),roomptr%cdepth-objpos(2,obpnt))<=mx_hsep) obj_fpos(obpnt) = 2
            if (min(objpos(1,obpnt),roomptr%cwidth-objpos(1,obpnt))<=mx_hsep .and. &
                min(objpos(2,obpnt),roomptr%cdepth-objpos(2,obpnt))<=mx_hsep) obj_fpos(obpnt) = 3

            objign(obpnt) =   lrarray(7)
            tmpcond =         lrarray(8)
            objort(1,obpnt) = lrarray(9)
            objort(2,obpnt) = lrarray(10)
            objort(3,obpnt) = lrarray(11)
            ! Enforce sanity; normal pointing vector must be non-zero (blas routine)
            if (dnrm2(3,objort(1,obpnt),1)<=0.0) then
                write(logerr,5322)
                stop
            end if
            objrm(obpnt) = iroom
            objnin(obpnt) = tcname
            objld(obpnt) = .true.
            objon(obpnt) = .false.
            ! This is redudant but needed to be compatible with the object database format
            objpnt(obpnt) = obpnt

            !!!!! Note that ignition type 1 is time, type 2 is temperature and 3 is flux !!!
            !!!!! The critiria for temperature and flux are stored backupwards - this is historical
            !!!!! See corresponding code in update_fire_objects
            if (tmpcond>0.0_eb) then
                if (objign(obpnt)==1) then
                    objcri(1,obpnt) = tmpcond
                    objcri(2,obpnt) = 1.0e30_eb
                    objcri(3,obpnt) = 1.0e30_eb
                else if (objign(obpnt)==2) then
                    objcri(1,obpnt) = 1.0e30_eb
                    objcri(2,obpnt) = 1.0e30_eb
                    objcri(3,obpnt) = tmpcond
                else if (objign(obpnt)==3) then
                    objcri(1,obpnt) = 1.0e30_eb
                    objcri(2,obpnt) = tmpcond
                    objcri(3,obpnt) = 1.0e30_eb
                else
                    write(logerr,5358) objign(obpnt)
                    stop
                end if
            else
                objon(obpnt) = .true.
            end if
            if (option(fbtobj)==off.and.objign(obpnt)/=1.0_eb) then
                if (stpmax>0.0_eb) then
                    stpmax = min(stpmax,1.0_eb)
                else
                    stpmax = 1.0_eb
                end if
            end if

            ! STPMAX # - set the maximum time step to #
        case ('STPMA')
            if (countargs(lcarray)>=1) then
                stpmax = lrarray(1)
            else
                write (logerr,*) '***Error: Bad STPMA input. At least 1 argument required.'
                stop
            end if

            ! DETECT Type Compartment Activation_Value Width Depth Height RTI Suppression Spray_Density
        case ('DETEC')
            if (countargs(lcarray)>=9) then
                ndtect = ndtect + 1

                if (ndtect>mxdtect) then
                    write (logerr, 5338)
                    stop
                end if

                dtectptr => detectorinfo(ndtect)
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
                if(iroom<1.or.iroom>mxrooms)then
                    write (logerr,5342) i2
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
                if(dtectptr%spray_density==0.0_eb)then
                    dtectptr%quench = .false.
                end if
                if(option(fbtdtect)==off.and.dtectptr%quench)then
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                end if
                roomptr => roominfo(ir)
                if (roomptr%name==' ') then
                    write(logerr,5344) i2
                    stop
                end if

                if(dtectptr%center(1)>roomptr%cwidth.or. &
                    dtectptr%center(2)>roomptr%cdepth.or.dtectptr%center(3)>roomptr%cheight) then
                    write(logerr,5339) ndtect,roomptr%name
                    stop
                end if

            else
                write (logerr,*) '***Error: Bad DETEC input. At least 9 arguments required.'
                stop
            end if

            !  VHEAT top_compartment bottom_compartment
        case ('VHEAT')
            if (countargs(lcarray)>=2) then
                i1 = lrarray(1)
                i2 = lrarray(2)
                if (i1<1.or.i2<1.or.i1>nr.or.i2>nr) then
                    write(logerr,5345) i1, i2
                    stop
                end if

                nswal = nswal + 1
                izswal(nswal,w_from_room) = i1
                izswal(nswal,w_from_wall) = 2
                izswal(nswal,w_to_room) = i2
                izswal(nswal,w_to_wall) = 1
            else
                write (logerr,*) '***Error: Bad VHEAT input. At least 2 arguments required.'
                stop
            end if

            ! ONEZ compartment number - This turns the compartment into a single zone
        case ('ONEZ')
            if (countargs(lcarray)>=1) then
                iroom = lrarray(1)
                if(iroom<1.or.iroom>nr)then
                    write(logerr, 5001) i1
                    stop
                end if
                roomptr => roominfo(iroom)
                roomptr%shaft = .true.
            else
                write (logerr,*) '***Error: Bad ONEZ input. At least 1 compartment must be specified.'
                stop
            end if

            ! HALL Compartment Velocity Depth Decay_Distance
        case ('HALL')
            if (countargs(lcarray)>=1) then
                iroom = lrarray(1)

                ! check that specified room is valid
                if(iroom<0.or.iroom>nr)then
                    write(logerr,5346) iroom
                    stop
                end if

                roomptr => roominfo(iroom)
                roomptr%hall = .true.
                if (countargs(lcarray)>1) write (logerr,5406) iroom
            else
                write (logerr,*) '***Error: Bad HALL input. At least 1 compartment must be specified.'
                stop
            end if

            ! ROOMA Compartment Number_of_Area_Values Area_Values
            ! This provides for variable compartment floor areas; this should be accompanied by the roomh command
        case ('ROOMA')
            if (countargs(lcarray)>=2) then
                iroom = lrarray(1)
                roomptr => roominfo(iroom)

                ! make sure the room number is valid
                if(iroom<1.or.iroom>nr)then
                    write(logerr,5347) iroom
                    stop
                end if

                ! make sure the number of points is valid
                npts = lrarray(2)
                if(npts>mxcross.or.npts<=0.or.npts/=countargs(lcarray)-2) then
                    write (logerr,5347) npts
                    stop
                end if
                if(roomptr%nvars/=0) npts = min(roomptr%nvars,npts)
                roomptr%nvars = npts

                ! make sure all data is positive
                do  i = 1, npts
                    if(lrarray(i+2)<0.0_eb)then
                        write(logerr,5348) lrarray(i+2)
                        stop
                    end if
                end do

                ! put the data in its place
                do i = 1, npts
                    roomptr%var_area(i) = lrarray(i+2)
                end do
            else
                write (logerr,*) '***Error: Bad ROOMA input. At least 2 arguments must be specified.'
                stop
            end if

            ! ROOMH Compartment Number_of_Height_Values Height_Values
            ! This companion to ROOMA, provides for variable compartment floor areas; this should be accompanied by the ROOMA command
        case ('ROOMH')
            if (countargs(lcarray)>=2) then
                iroom = lrarray(1)
                roomptr => roominfo(iroom)

                ! make sure the room number is valid
                if(iroom<1.or.iroom>nr)then
                    write(logerr,5349) iroom
                    stop
                end if

                ! make sure the number of points is valid
                npts = lrarray(2)
                if(npts>mxcross.or.npts<0.or.npts/=countargs(lcarray)-2)then
                    write(logerr,5350) npts
                    stop
                end if
                if(roomptr%nvars/=0)npts = min(roomptr%nvars,npts)
                roomptr%nvars = npts

                ! make sure all data is positive
                do i = 1, npts
                    if(lrarray(i+2)<0.0_eb)then
                        write(logerr,5348) lrarray(i+2)
                        stop
                    end if
                end do

                ! put the data in its place
                do i = 1, npts
                    roomptr%var_height(i) = lrarray(i+2)
                end do

            else
                write (logerr,*) '***Error: Bad ROOMH input. At least 2 arguments must be specified.'
                stop
            end if

            ! DTCHE Minimum_Time_Step Maximum_Iteration_Count
        case ('DTCHE')
            if (countargs(lcarray)>=2) then
                stpmin = abs(lrarray(1))
                stpmin_cnt_max = abs(lrarray(2))
                ! a negative turns off the check
                if (lrarray(2)<=0) stpminflag = .false.
            else
                write (logerr,*) '***Error: Bad DTCHE input. At least 2 arguments must be specified.'
                stop
            end if

            ! Horizontal heat flow, HHEAT First_Compartment Number_of_Parts nr pairs of {Second_Compartment, Fraction}

            ! There are two forms of the command
            !   The first (single entry of the room number) - all connections based on horizontal flow
            !   The second is the compartment number followed by nr pairs of compartments to which the heat
            !   will flow and the fraction of the vertical surface of the compartment that loses heat
        case ('HHEAT')
            if (countargs(lcarray)>=1) then
                nto = 0
                ifrom = lrarray(1)

                if (countargs(lcarray)>=1) then
                    iheat(ifrom) = 1
                    cycle
                else
                    nto = lrarray(2)
                    if(nto<1.or.nto>nr)then
                        write(logerr,5354) nto
                        stop
                    end if
                    iheat(ifrom) = 2
                    iheat(ifrom) = 2
                end if

                if (2*nto==(countargs(lcarray)-2)) then
                    do i = 1, nto
                        i1 = 2*i+1
                        i2 = 2*i+2
                        ito = lrarray(i1)
                        frac = lrarray(i2)
                        if(ito<1.or.ito==ifrom.or.ito>nr)then
                            write(logerr, 5356) ifrom,ito
                            stop
                        end if
                        if(frac<0.0_eb.or.frac>1.0_eb)then
                            write(logerr, 5357) ifrom,ito,frac
                            stop
                        end if
                        heat_frac(ifrom,ito) = frac
                    end do
                else
                    write(logerr,5355) ifrom, nto
                    stop
                end if
            else
                write (logerr,*) '***Error: Bad HHEAT input. At least 1 arguments must be specified.'
                stop
            end if

            ! FURN - no fire, heat walls according to a prescribed time temperature curve
        case ('FURN')
            nfurn=lrarray(1)+0.5
            do i = 1, nfurn
                furn_time(i)=lrarray(2*i)
                furn_temp(i)=lrarray(2*i+1)
            end do

            ! ADIAB - all surfaces are adiabatic so that dT/dx at the surface = 0
        case ('ADIAB')
            adiabatic_wall = .true.

            !  HEATF Special fire - heat source only; no mass
        case ('HEATF')
            if (countargs(lcarray)>=5) then
                heatfr = lrarray(1)
                if(heatfr<1.or.heatfr>nr-1) then
                    stop
                end if
                heatfl = .true.
                heatfp(1) = lrarray(2)
                heatfp(2) = lrarray(3)
                heatfp(3) = lrarray(4)
            else
                write (logerr,*) '***Error: Bad HEATF input. At least 5 arguments must be specified.'
                stop
            end if

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
                    write (logerr, 5403) nvisualinfo
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
                            write (logerr, 5403) nvisualinfo
                            stop
                        end if
                        if (lcarray(2) =='X') then
                            sliceptr%axis = 1
                            if (sliceptr%roomnum>0) then
                                roomptr => roominfo(sliceptr%roomnum)
                                if (sliceptr%position>roomptr%cwidth.or.sliceptr%position<0.0_eb) then
                                    write (logerr, 5403) nvisualinfo
                                    stop
                                end if
                            end if
                        else if (lcarray(2) =='Y') then
                            sliceptr%axis = 2
                            if (sliceptr%roomnum>0) then
                                roomptr => roominfo(sliceptr%roomnum)
                                if (sliceptr%position>roomptr%cdepth.or.sliceptr%position<0.0_eb) then
                                    write (logerr, 5403) nvisualinfo
                                    stop
                                end if
                            end if
                        else if (lcarray(2) =='Z') then
                            sliceptr%axis = 3
                            if (sliceptr%roomnum>0) then
                                roomptr => roominfo(sliceptr%roomnum)
                                if (sliceptr%position>roomptr%cheight.or.sliceptr%position<0.0_eb) then
                                    write (logerr, 5403) nvisualinfo
                                    stop
                                end if
                            end if
                        else
                            write (logerr, 5403) nvisualinfo
                            stop
                        end if
                    else
                        write (logerr, 5403) nvisualinfo
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
                        write (logerr, 5403) nvisualinfo
                        stop
                    end if
                end if
            else
                write (logerr,*) '***Error: Bad SLCF input. At least 1 arguments must be specified.'
                stop
            end if

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
                    write (logerr, 5404) nvisualinfo
                    stop
                end if
            else
                write (logerr,*) '***Error: Bad SLCF input. At least 1 arguments must be specified.'
                stop
            end if

            ! Outdated keywords
        case ('CJET','WIND','LIMO2','GLOBA','DJIGN')            ! Just ignore these inputs ... they shouldn't be fatal
            write (logerr,5407) label
        case ('OBJFL','MVOPN','MVFAN','MAINF','INTER','SETP')   ! these are clearly outdated and should produce errors
            write (logerr,5405) label
            stop
        case ('MATL','COMPA','TARGE','HEIGH','AREA','TRACE','CO','SOOT',&
              'HRR','TIME','CHEMI','FIRE') ! these are already handled above

            case default
            write(logerr, 5051) label
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
5080 format ('***Error: Bad HVENT input. Too many pairwise horizontal connections',4I5)
5081 format ('***Error: Too many horizontal connections ',3i5)
5191 format ('***Error: Bad MVENT input. Compartments specified in MVENT have not been defined ',2i3)
5192 format ('***Error: Bad MVENT input. Exceeded maximum number of nodes/openings in MVENT ',2i3)
5193 format ('***Error: Bad MVENT input. MVENT(MID) is not consistent and should be a fan ',2i3)
5194 format ('***Error: Bad MVENT input. Pressure for zero flow must exceed the lower limit',f10.2)
5195 format ('***Error: Bad MVENT input. Too many fan systems ',i0)
5196 format ('***Error: Bad EVENT input. Fan has not been defined for this filter ',i0)
5300 format ('***Error: Bad FIRE input. Too many objects defined in datafile')
5310 format ('***Error: Bad FIRE input. Incorrect number of parameters for OBJECT')
5320 format ('***Error: Bad FIRE input. Object specification error, room ',i0,' out of range')
5321 format ('***Error: Bad FIRE input. Object specification error, not an allowed fire type',i0)
5322 format ('***Error: Bad FIRE input. Object normal vector must be non-zero')
5323 format ('***Error: Bad FIRE input. Object ',i0,' is outside its compartment')
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
5405 format ('***Error: Invalid keyword in CFAST input file ',a)
5406 format ('***Error: Bad HALL input. Outdated HALL command for compartment ',i0,' Flow inputs are no longer used')
5407 format ('***Warning: Outdated keyword in CFAST input file ignored ',a)

    end subroutine keywordcases

! --------------------------- inputembeddedfire -------------------------------------------

    subroutine inputembeddedfire(objname, lrowcount, inumc, iobj)

    !     routine: inputembeddedfire
    !     purpose: This routine reads a new format fire definition that begins with a FIRE keyword (already read in keywordcases)
    !              followed by CHEMI, TIME, HRR, SOOT, CO, TRACE, AREA, and HEIGH keywords (read in here)
    !     Arguments: objname: name of this fire object
    !                iroom:   compartment where this fire is located
    !                lrowcount: current row in the input file.  We begin one row after this one
    !                inumc:   number of columns in the input file
    !                iobj:    pointer to the fire object that will contain all the data we read in here

    integer, intent(in) :: inumc, iobj, lrowcount
    character(*), intent(in) :: objname

    character(128) :: lcarray(ncol)
    character(5) :: label
    integer :: logerr = 3, midpoint = 1, base = 2, ir, i, ii, nret
    real(eb) :: lrarray(ncol), ohcomb, max_area, max_hrr, hrrpm3, area, flamelength
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
                obj_c(iobj) = lrarray(1)
                obj_h(iobj) = lrarray(2)
                obj_o(iobj) = lrarray(3)
                obj_n(iobj) = lrarray(4)
                obj_cl(iobj) = lrarray(5)
                objgmw(iobj) = (12.01*obj_c(iobj) + 1.008*obj_h(iobj) + 16.0*obj_o(iobj) + &
                    14.01*obj_n(iobj) + 35.45*obj_cl(iobj))/1000.0
                radconsplit(iobj) = lrarray(6)
                ohcomb = lrarray(7)
                if (ohcomb<=0.0_eb) then
                    write(logerr,5001) ohcomb
                    stop
                end if
                omatl(iobj) = lcarray(8)
            else
                write (logerr,*) '***Error: At least 7 arguments required on CHEMI input'
                stop
            end if
        case ('TIME')
            nret = countargs(lcarray)
            objlfm(iobj) = nret
            do ii = 1, nret
                otime(ii,iobj) = lrarray(ii)
            end do
        case ('HRR')
            max_hrr = 0.0_eb
            do ii = 1, nret
                oqdot(ii,iobj) = lrarray(ii)
                max_hrr = max(max_hrr, oqdot(ii,iobj))
                omass(ii,iobj) = oqdot(ii,iobj)/ohcomb
            end do
        case ('SOOT')
            do ii = 1, nret
                ood(ii,iobj) = lrarray(ii)
            end do
        case ('CO')
            do ii = 1, nret
                oco(ii,iobj) = lrarray(ii)
            end do
        case ('TRACE')
            ! Note that CT, TUHC and TS are carried in the mprodr array - all other species have their own array
            do ii = 1, nret
                omprodr(ii,7,iobj) = 0.0_eb
                omprodr(ii,10,iobj) = 1.0_eb
                omprodr(ii,11,iobj) = lrarray(ii)
            end do
        case ('AREA')
            max_area = 0.0_eb
            do ii = 1, nret
                ! The minimum area is to stop dassl from an floating point underflow when it tries to extrapolate back to the
                ! ignition point. It only occurs for objects which are on the floor and ignite after t=0 The assumed minimum fire
                ! diameter of 0.2 m below is the minimum valid fire diameter for Heskestad's plume correlation
                ! (from SFPE Handbook chapter)
                if (lrarray(ii)==0.0_eb) then
                    write (logerr,5002)
                    stop
                end if
                oarea(ii,iobj) = max(lrarray(ii),pio4*0.2_eb**2)
                max_area = max(max_area,oarea(ii,iobj))
            end do

            ! calculate size of the object based on the maximum area with a thickness assuming it's a cube
            ! as with the flame height calculation, the minimum area is 0.09 m^2 (about 1 ft^2)
            objxyz(1,iobj) = sqrt(max(max_area,pio4*0.2_eb**2))
            objxyz(2,iobj) = objxyz(1,iobj)
            objxyz(3,iobj) = objxyz(1,iobj)

            ! calculate a characteristic length of an object (we assume the diameter).
            ! This is used for point source radiation fire to target calculation as a minimum effective
            ! distance between the fire and the target which only impact very small fire to target distances
            objclen(iobj) = sqrt(max_area/pio4)
        case ('HEIGH')
            do ii = 1, nret
                ohigh(ii,iobj) = max(lrarray(ii),0.0_eb)
            end do
        case default
            write(logerr, 5000) label
            stop
        end select

    end do

    ! set the heat of combustion - this is a problem if the qdot is zero and the mdot is zero as well
    call set_heat_of_combustion (objlfm(iobj), omass(1,iobj), oqdot(1,iobj), objhc(1,iobj), ohcomb)

    ! Position the object
    roomptr => roominfo(objrm(iobj))
    call positionobject(objpos,1,iobj,roomptr%cwidth,midpoint,mx_hsep)
    call positionobject(objpos,2,iobj,roomptr%cdepth,midpoint,mx_hsep)
    call positionobject(objpos,3,iobj,roomptr%cheight,base,mx_hsep)

    ! diagnostic - check for the maximum heat release per unit volume.
    ! first, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
    area = objxyz(1,iobj)*objxyz(2,iobj)
    call flame_height(max_hrr, area, flamelength)
    flamelength = max (0.0_eb, flamelength)
    ! now the heat realease per cubic meter of the flame - we know that the size is larger than 1.0d-6 m^3 - enforced above
    hrrpm3 = max_hrr/(area*(objxyz(3,iobj)+flamelength))
    if (hrrpm3>4.0e6_eb) then
        write (logerr,5106)trim(objname),(objpos(i,iobj),i=1,3),hrrpm3
        write (logerr, 5108)
        stop
    else if (hrrpm3>2.0e6_eb) then
        write (logerr,5107)trim(objname),(objpos(i,iobj),i=1,3),hrrpm3
        write (logerr, 5108)
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
        if(i>1) then
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

    !     routine: open_files
    !     purpose: get the paths and project base name open the input file for reading (1)
    ! 	         delete the output files
    ! 	         open the log file (3)
    ! 	         call the input routines
    !     arguments: errorcode: return error indication if non-zero

    integer :: lp, ld, ios
    character(256) :: testpath, testproj

    ! get the path and project names
    call exehandle (exepath, datapath, project)

    ! form the file names for datafiles: inputfile, outputfile, smvhead, smvdata, smvcsv, ssflow, ssnormal, ssspecies, sswall
    testpath = trim (datapath)
    lp = len_trim (testpath)
    testproj = trim (project)
    ld = len_trim (testproj)
    inputfile = testpath(1:lp) // testproj(1:ld) // '.in'
    outputfile = testpath(1:lp) // testproj(1:ld) // '.out'
    smvhead = testpath(1:lp) // testproj(1:ld) // '.smv'
    smvdata = testpath(1:lp) // testproj(1:ld) // '.plt'
    smvcsv = testpath(1:lp) // testproj(1:ld) // '_zone.csv'
    ssflow = testpath(1:lp) // testproj(1:ld) // '_f.csv'
    ssnormal = testpath(1:lp) // testproj(1:ld) // '_n.csv'
    ssspecies = testpath(1:lp) // testproj(1:ld) // '_s.csv'
    sswall = testpath(1:lp) // testproj(1:ld) // '_w.csv'
    errorlogging = testpath(1:lp) // testproj(1:ld) // '.log'
    stopfile = testpath(1:lp) // testproj(1:ld) // '.stop'
    residfile = testpath(1:lp) // testproj(1:ld) // '.debug'
    residcsv = testpath(1:lp) // testproj(1:ld) // '_resid.csv'
    jacfile = testpath(1:lp) // testproj(1:ld) // '.jac'
    jaccsv = testpath(1:lp) // testproj(1:ld) // '_jac.csv'
    historyfile = testpath(1:lp) // testproj(1:ld) // '.hi'
    queryfile = testpath(1:lp) // testproj(1:ld) // '.query'
    statusfile = testpath(1:lp) // testproj(1:ld) // '.status'
    kernelisrunning = testpath(1:lp) // testproj(1:ld) // '.kernelisrunning'

    slabcsv = testpath(1:lp) // testproj(1:ld) // '_slab.csv'

    testpath = trim (exepath)
    lp = len_trim (testpath)
    solverini = testpath(1:lp) // 'solver.ini'

    open (unit=1, file=inputfile, action='read', status='old', iostat=ios)

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
    call deleteoutputfiles (sswall)
    call deleteoutputfiles (historyfile)
    call deleteoutputfiles (statusfile)
    call deleteoutputfiles (queryfile)
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

    ! read in hvac convection coefficient
    read(iofili,*)
    read(iofili,*) ductcv

    ! read in jacobian and snsqe print flags
    read(iofili,*)
    read(iofili,*) jacchk, cutjac, iprtalg
    close (iofili)

    return
    end subroutine read_solver_ini

! --------------------------- positionobject -------------------------------------------

    subroutine positionobject (xyz,index,opoint,pos_max,defaultposition,minimumseparation)

    !     routine: positionobject
    !     purpose: Position an object in a compartment
    !     arguments: xyz: objposition (objpos)
    !                index: 1, 2 or 3 for x, y or z
    !		         opoint: the object pointer
    !		         rpoint: the compartment
    !		         pos_max: the maximum extent
    !		         defaultposition: to set to zero (base)(2) or midpoint(1)
    !		         minimumseparation: the closest the object can be to a wall

    integer, intent(in) :: index, defaultposition, opoint
    real(eb), intent(in) :: minimumseparation, pos_max
    real(eb), intent(inout) :: xyz(3,0:*)

    if ((xyz(index,opoint)<0.0_eb).or.(xyz(index,opoint)>pos_max)) then
        select case (defaultposition)
        case (1)
            xyz(index,opoint) = pos_max/2.0_eb
        case (2)
            xyz(index,opoint) = minimumseparation
        case default
            write (logerr,*) 'Fire objects positioned specified outside compartment bounds.'
            stop
        end select
    else if (xyz(index,opoint)==0.0_eb) then
        xyz(index,opoint) = minimumseparation
    else if (xyz(index,opoint)==pos_max) then
        xyz(index,opoint) = pos_max-minimumseparation
    end if

    return

    end subroutine positionobject

! --------------------------- readcsvformat -------------------------------------------

    subroutine readcsvformat (iunit, x, c, numr, numc, nstart, maxrow, maxcol, logerr)

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
    !                logerr   = logical unit number for writing error messages (if any)

    integer, intent(in) :: iunit, numr, numc, nstart, logerr

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

    ! Skip comments
    if (in(1:1)=='!'.or.in(1:1)=='#') then
        go to 20
    end if

    nrcurrent = nrcurrent+1
    maxrow = max(maxrow,nrcurrent)

    ! Cannot exceed work array
    if(maxrow>numr) then
        write (logerr,'(a,i0,1x,i0)') '***Error: Too many rows or columns in input file, r,c = ', maxrow, maxcol
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
            write (logerr,'(a,i0,a,i0)') 'Too many rows or columns in input file, r,c=', nrcurrent, ' ', nc
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
   if(nvisualinfo.eq.0)return

   nrm = nrm1

   ! count number of isosurfaces and slices

   do i = 1, nvisualinfo
       vptr=>visualinfo(i)
       if(vptr%roomnum.eq.0)then
           ntypes=nrm
       else
           ntypes=1
       end if
       if(vptr%vtype.eq.1.or.vptr%vtype.eq.2)then
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
       if(vptr%vtype.eq.3)cycle
       ir = vptr%roomnum
       if(ir.eq.0)then
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
           if(vptr%vtype.eq.1)then
               position_offset = 0.0_eb
               if(vptr%axis.eq.1)then
                   if(ir/=0) position_offset = roomptr%x0
                   xb(1) = vptr%position + position_offset
                   xb(2) = vptr%position + position_offset
                   ijkslice(1) = get_igrid(xb(1),roomptr%xplt,roomptr%ibar)
                   if(ijkslice(1)<0)skipslice=1
                   ijkslice(2) = ijkslice(1)
               else if(vptr%axis.eq.2)then
                   if(ir/=0) position_offset = roomptr%y0
                   xb(3) = vptr%position + position_offset
                   xb(4) = vptr%position + position_offset
                   ijkslice(3) = get_igrid(xb(3),roomptr%yplt,roomptr%jbar)
                   if(ijkslice(3)<0)skipslice=1
                   ijkslice(4) = ijkslice(3)
               else if(vptr%axis.eq.3)then
                   if(ir/=0) position_offset = roomptr%z0
                   xb(5) = vptr%position + position_offset
                   xb(6) = vptr%position + position_offset
                   ijkslice(5) = get_igrid(xb(5),roomptr%zplt,roomptr%kbar)
                   if(ijkslice(5)<0)skipslice=1
                   ijkslice(6) = ijkslice(5)
               end if
           end if
           do j = 1, 5
               sliceptr => sliceinfo(islice)

               sliceptr%skip=skipslice
               write(slicefilename,'(A,A,I4.4,A)') trim(project),'_',islice,'.sf'
               if(j.eq.1)then
                   menu_label="Temperature"
                   colorbar_label="TEMP"
                   unit_label="C"
               else if(j.eq.2)then
                   menu_label="U-VELOCITY"
                   colorbar_label="U-VEL"
                   unit_label="m/s"
               else if(j.eq.3)then
                   menu_label="V-VELOCITY"
                   colorbar_label="V-VEL"
                   unit_label="m/s"
               else if(j.eq.4)then
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
       if(vptr%vtype.ne.3)cycle
       ir = vptr%roomnum
       if(ir.eq.0)then
           ibeg=1
           iend=nrm
       else
           ibeg=ir
           iend=ir
       end if
       do iroom=ibeg,iend
           roomptr=>roominfo(iroom)
           isoptr => isoinfo(i_iso)

           write(isofilename,'(A,A,I4.4,A)') trim(project),'_',i_iso,'.iso'
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