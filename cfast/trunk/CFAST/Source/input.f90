
! --------------------------- readinputfile -------------------------------------------

    subroutine readinputfile (ierror)

    !	Read the input file and set up the data for processing

    use precision_parameters
    use wallptrs
    use cenviro
    use cfast_main
    use cshell
    use iofiles
    use params
    use thermp
    implicit none

    integer, intent(out) :: ierror
    
    real(eb) :: yinter(nr), temparea(mxpts), temphgt(mxpts), deps1, deps2, dwall1, dwall2, rti, xloc, yloc, zloc, darea, dheight, xx, sum
    integer numr, numc, ifail, ios, iversion, i, ii, j, jj, k, itop, ibot, nswall2, iroom, iroom1, iroom2, iwall1, iwall2, idtype, npts, ioff, ioff2, nventij
    character :: messg*133, aversion*5

    !	Unit numbers defined in readop, openoutputfiles, readinputfiles
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
    !     switch (1) = ceiling properties are defined
    !            (2) = floor properties are defined
    !            (3) = side wall properties are defined for upper walls
    !            (4) = side wall properties are defined for lower walls

    ifail = 0

    ! deal with opening the data file and assuring ourselves that it is compatible
    close (iofili)
    open (unit=iofili,file=inputfile,status='OLD',iostat=ios)
    if (ios/=0) then
        if (logerr>0) write (logerr,5050) mod(ios,256)
        ierror = 99
        return
    endif

    ! read in the entire input file as a spreadsheet array of numbers and/or character strings
    call readcsvformat(iofili,rarray,carray,nrow,ncol,1,numr,numc,ierror)
    if (ierror>0) then
        write(logerr,5003)
        return
    endif

    close (iofili)

    ! aversion is the header name, ivers is the major version number read in, iversion is the major version number
    ! from the internal version data. these need to be compatible
    aversion = carray(1,1)
    ivers = rarray(1,2)
    ! new version numbering 600->6000, so current version is 6100
    if (version>=1000) then
        iversion = version/1000
    else
        iversion = version/100
    endif
    if (aversion==heading.and.ivers==iversion) then
        write (logerr,5001) ivers
    else
        write (logerr,5002) aversion,heading,ivers,iversion
        ierror = 206
        return
    endif
    title = carray(1,3)

    do i = 1, nr
        yinter(i) = -1.0_eb
    end do

    maxct = 0

    ! read in data file
    call keywordcases (numr, numc, ierror)

    !	wait until the input file is parsed before dieing on temperature outside reasonable limits
    if (exterior_temperature>373.15_eb.or.exterior_temperature<223.15_eb) then
        write(logerr,5022) exterior_temperature
        ierror = 218
    endif
    if (interior_temperature>373.15_eb.or.interior_temperature<223.15_eb) then
        write(logerr,5022) interior_temperature
        ierror = 218
    endif

    if (ierror/=0) return

    ! We now know what output is going to be generated, so create the files
    call openoutputfiles

    interior_density = interior_abs_pressure/interior_temperature/rgas
    exterior_density = exterior_abs_pressure/exterior_temperature/rgas

    ! turn on the subsidiary equations if they are neeeded - this is always true
    if (activs(6)) hcldep = 1

    ! initialize the targets. add targets for each fire
    nm1 = n - 1
    if (numobjl>0) then
        do i = 1, numobjl 
            call initfireobject(i,ierror)
        end do
    endif
    call inittarg (ierror)
    if (ierror/=0) return

    ! now calculate the offsets - the order is important
    call offset (ierror)
    if (ierror/=0) return

    ! floor plan dependent parameters
    do i = 1, nm1
        hrl(i) = hflr(i)
        hrp(i) = hr(i) + hflr(i)
    end do

    ! check and/or set heat source fire position
    if (heatfl) then
        if ((heatfp(1)<0.0_eb).or.(heatfp(1)>br(heatfr))) then
            heatfp(1) = br(heatfr)/2.0_eb
        endif
        if ((heatfp(2)<0.0_eb).or.(heatfp(2)>dr(heatfr))) then
            heatfp(2) = dr(heatfr)/2.0_eb
        endif
        if ((heatfp(3)<0.0_eb).or.(heatfp(3)>hr(heatfr))) then
            heatfp(3) = 0.0_eb
        endif
        write(logerr,5021) heatfr,heatfp
    endif

    ! check and/or set position of fire objects
    do i = 1, numobjl
        if((objpos(1,i)<0.0_eb).or.(objpos(1,i)>br(objrm(i)))) then
            objpos(1,i) = br(objrm(i))/2.0_eb
            if (logerr>0) write (logerr,5080) i, objpos(1,i)
        endif
        if((objpos(2,i)<0.0_eb).or.(objpos(2,i)>dr(objrm(i)))) then
            objpos(2,i) = dr(objrm(i))/2.0_eb
            if (logerr>0) write (logerr,5090) i, objpos(2,i)
        endif
        if((objpos(3,i)<0.0_eb).or.(objpos(3,i)>hr(objrm(i)))) then
            objpos(3,i) = 0.0_eb
            if (logerr>0) write (logerr,5100) i, objpos(3,i)
        endif
    end do

    ! make sure horizontal vent specifications are correct -  we have to do this
    ! here rather than right after keywordcases because hrl and hrp were just defined
    ! above
    do itop = 1, nm1
        if (nwv(itop,itop)/=0) then
            if (logerr>0) write (logerr,*) ' A room can not be connected to itself'
            nwv(itop,itop) = 0
        endif
        do ibot = 1, itop - 1
            if (nwv(itop,ibot)/=0.or.nwv(ibot,itop)/=0) then

                ! see which room is on top (if any) - this is like a bubble sort
                deps1 = hrl(itop) - hrp(ibot)
                deps2 = hrl(ibot) - hrp(itop)
                if (nwv(itop,ibot)/=1.or.abs(deps1)>=vfmaxdz) then
                    if (nwv(ibot,itop)/=1.or.abs(deps2)>=vfmaxdz) then
                        if (nwv(itop,ibot)==1.and.abs(deps2)<vfmaxdz) then
                            if (nwv(ibot,itop)/=0) then
                                write (logerr,*) 'Vent ', ibot, itop, ' is being redefined'
                            endif
                            nwv(itop,ibot) = 0
                            nwv(ibot,itop) = 1
                            vvarea(ibot,itop) = vvarea(itop,ibot)
                            vshape(ibot,itop) = vshape(itop,ibot)
                            cycle
                        endif
                        if (nwv(ibot,itop)==1.and.abs(deps1)<vfmaxdz) then
                            if (nwv(itop,ibot)/=0) then
                                write (logerr,*) 'Vent ', itop, ibot, ' is being redefined'
                            endif
                            nwv(itop,ibot) = 1
                            nwv(ibot,itop) = 0
                            vvarea(itop,ibot) = vvarea(ibot,itop)
                            vshape(itop,ibot) = vshape(ibot,itop)
                            cycle
                        endif
                        nwv(itop,ibot) = 0
                        nwv(ibot,itop) = 0
                    endif
                endif
            endif
        end do
    end do

    ! Compartment area and volume
    do i = 1, nm1
        ar(i) = br(i)*dr(i)
        vr(i) = ar(i)*hr(i)
    end do


    ! check room to room heat transfer parameters (cfcon command)
    nswall2 = nswal
    ii = 0
    do i = 1, nswal
        iroom1 = izswal(i,w_from_room)
        iroom2 = izswal(i,w_to_room)

        ! room numbers must be between 1 and nm1
        if(iroom1<1.or.iroom2<1.or.iroom1>nm1+1.or.iroom2>nm1+1)then
            ifail = 39
            write (messg,201)iroom1,iroom2 
201         format(' Invalid CFCON specification:',' one or both of rooms ',2i3, ' is out of bounds')
            call xerror(messg,0,1,1)
        endif

        ! if room is connected to the outside then ignore it
        if(iroom1==nm1+1.or.iroom2==nm1+1)then
            nswall2 = nswall2 - 1
            cycle
        else
            ii = ii + 1
            if(i/=ii)then
                izswal(ii,w_from_room) = izswal(i,w_from_room)
                izswal(ii,w_from_wall) = izswal(i,w_from_wall)
                izswal(ii,w_to_room) = izswal(i,w_to_room)
                izswal(ii,w_to_wall) = izswal(i,w_to_wall)
            endif
        endif

        ! floor of one room must be adjacent to ceiling of the other
        dwall1 = abs(hrl(iroom1) - hrp(iroom2))
        dwall2 = abs(hrl(iroom2) - hrp(iroom1))
        if(dwall1<vfmaxdz.or.dwall2<=vfmaxdz)then
            if(dwall1<vfmaxdz)then
                izswal(ii,w_from_wall) = 2
                izswal(ii,w_to_wall) = 1
            else
                izswal(ii,w_from_wall) = 1
                izswal(ii,w_to_wall) = 2
            endif
        else
            ifail = 40
            write (messg,202) iroom1,iroom2 
202         format(' Invalid CFCON specification:'' ceiling and floor of rooms',2i3, ' are not connectetd')
        endif

        ! walls must be turned on, ie switch must be set
        ! for the ceiling in the lower room and the floor of
        ! the upper room
        iwall1 = izswal(ii,w_from_wall)
        iwall2 = izswal(ii,w_to_wall)
        if(.not.switch(iwall1,iroom1).or..not.switch(iwall2,iroom2))then
            write (messg,203)
203         format(' Invalid CFCON specification:')
            call xerror(messg,0,1,1)
            if(.not.switch(iwall1,iroom1))then
                write(messg,204) iwall1,iroom1
204             format(' Wall ',i2,' of room ',i2,' is not turned on')
                call xerror(messg,0,1,1)
            endif
            if(.not.switch(iwall2,iroom2))then
                write(messg,204)iwall2,iroom2
                call xerror(messg,0,1,1)
            endif
            ifail = 41
        endif
    end do
    nswal = nswall2

    ! check shafts
    do iroom = nm1 + 1, nr
        if(izshaft(iroom)/=0)then
            call xerror(' invalid SHAFT specification:',0,1,1)
            ifail = 42
            write (messg,206)iroom,nm1
206         format(' room ',i3,' must be less than or equal to ',i3)
            call xerror(messg,0,1,1)
        endif
    end do

    ! initialize variables that will change when ambient conditions change
    call initamb(yinter,1)

    ! initialize the mechanical ventilation
    call hvinit (ierror)
    if (ierror/=0) return

    ! check detectors
    do i = 1, ndtect
        iroom = ixdtect(i,droom)
        if(iroom<1.or.iroom>nm1)then
            write (messg,104)iroom 
104         format('Invalid DETECTOR specification: room ',i3, ' is not a valid')
            ifail = 43
            call xerror(messg,0,1,1)
        endif
        rti = xdtect(i,drti)
        if(rti<=0.0_eb.and.ixdtect(i,dtype)/=smoked)then
            write (messg,101)rti 
101         format('Invalid DETECTOR specification - rti= ',e11.4, ' is not a valid.')
            ifail = 44
        endif
        xloc = xdtect(i,dxloc)
        yloc = xdtect(i,dyloc)
        zloc = xdtect(i,dzloc)
        if(xloc<0.0_eb.or.xloc>br(iroom).or.yloc<0.0_eb.or.yloc>dr(iroom).or.zloc<0.0_eb.or.zloc>hrp(iroom))then
            write(messg,102)xloc,yloc,zloc
102         format('Invalid DETECTOR specification - x,y,z,location','x,y,z=',3e11.4,' is out of bounds')
            ifail = 45
        endif
        idtype = ixdtect(i,dtype)
        if(idtype<1.or.idtype>3)then
            write(messg,103)idtype
103         format('Invalid DETECTOR specification - type= ',i2,' is not a valid')
            ifail = 46
        endif
    end do

    ! check variable cross-sectional area specs and convert to volume
    do i = 1, nm1
        npts = izrvol(i)
        if(npts/=0)then

            ! force first elevation to be at the floor; add a data point if necessary (same area as first entered data point)
            if(zzrhgt(1,i)/=0.0_eb)then
                temparea(1) = zzrarea(1,i)
                temphgt(1) = 0.0_eb
                ioff = 1
            else
                ioff = 0
            endif

            ! copy data to temporary arrays
            do j = 1, npts
                temparea(j+ioff) = zzrarea(j,i)
                temphgt(j+ioff) = zzrhgt(j,i)
            end do

            ! force last elevation to be at the ceiling (as defined by hr(i)
            if(hr(i)/=zzrhgt(npts,i))then
                ioff2 = 1
                temparea(npts+ioff+ioff2) = zzrarea(npts,i)
                temphgt(npts+ioff+ioff2) = hr(i)
            else
                ioff2 = 0
            endif

            npts = npts + ioff + ioff2
            izrvol(i) = npts

            ! copy temporary arrays to zzrhgt and zzrarea; define volume by integrating areas
            zzrhgt(1,i) = 0.0_eb
            zzrvol(1,i) = 0.0_eb
            zzrarea(1,i) = temparea(1)
            j = 1
            do j = 2, npts
                zzrhgt(j,i) = temphgt(j)
                zzrarea(j,i) = temparea(j)
                darea = (zzrarea(j,i)+zzrarea(j-1,i))/2.0_eb
                dheight = zzrhgt(j,i) - zzrhgt(j-1,i)
                zzrvol(j,i) = zzrvol(j-1,i) + darea*dheight
            end do

            ! re-define volume, area, breadth and depth arrays 
            ! (vr, ar, br and dr ) according to room area - height
            ! data read in.  hr remains the same, vr is defined
            ! by integrating areas specified on the roomarea command,
            ! ar is then vr/hr, br and dr are defined so that
            ! br*dr=ar and br/dr remain the same as entered on
            ! the width and depth  commands.

            vr(i) = zzrvol(npts,i)
            ar(i) = vr(i)/hr(i)
            xx = br(i)/dr(i)
            br(i) = sqrt(ar(i)*xx)
            dr(i) = sqrt(ar(i)/xx)
        endif
    end do


    ! check room to room heat transfer 

    ! The array IZHEAT may have one of three values, 0, 1, 2.  
    ! 0 = no room to room heat transfer
    ! 1 = fractions are determined by what rooms are connected by vents
    ! For example, if room 1 is connected to rooms 2, 3, 4 and the outside
    ! by vents then the first row of ZZHTFRAC will have the values
    ! 0. .25 .25 .25 .25

    ! force all rooms to transfer heat between connected rooms
    if(izheat(0)==1)then
        do i = 1, nm1
            izheat(i) = 1
        end do
    endif

    do i = 1, nm1

        ! force heat transfer between rooms connected by vents.
        if(izheat(i)==1)then
            do j = 1, nm1+1
                nventij = 0
                do k = 1, 4
                    nventij = nventij + ijk(i,j,k)
                end do
                if(nventij/=0)zzhtfrac(i,j) = 1.0_eb

                ! if the back wall is not active then don't consider its contribution
                if(j<=nm1.and..not.switch(3,j))zzhtfrac(i,j) = 0.0_eb
            end do
        endif

        ! normalize zzhtfrac fraction matrix so that rows sum to one
        if(izheat(i)/=0)then
            sum = 0.0_eb
            do j = 1, nm1+1
                sum = sum + zzhtfrac(i,j)
            end do
            if(sum<1.e-5_eb)then
                do j = 1, nm1
                    zzhtfrac(i,j) = 0.0_eb
                end do
                zzhtfrac(i,nm1+1) = 1.0_eb
            else
                do j = 1, nm1+1
                    zzhtfrac(i,j) = zzhtfrac(i,j)/sum
                end do
            endif
            jj = 0
            do j = 1, nm1
                if(zzhtfrac(i,j)/=0.0_eb)then
                    izhtfrac(i,0) = izhtfrac(i,0) + 1
                    jj = jj + 1
                    izhtfrac(i,jj) = j
                endif
            end do
        endif
    end do

    if(ifail>0) then
        call xerror('Input error in readinputfile',0,1,1)
        ierror = ifail
        return
    endif
    close (iofili)
    return

5001 format ('Opening a version ',i2,' file in normal mode')
5002 format ('***Error: Not a compatible version ',2a8,2x,2i10)
5003 format ('***Error: Too many lines in the main data file')
5021 format ('The constant heat source (heatf) is in compartment ',i3,' at ',3f12.5)
5022 format ('***Error: Initial temperature outside of allowable range (-50 to +100)',f5.2)

    ! read format list
5050 format ('***Error: Error opening the input file = ',I6)

    ! output for objects
5080 format (' Object no. ',I3,' X cood. set to ',F12.5)
5090 format (' Object no. ',I3,' Y cood. set to ',F12.5)
5100 format (' Object no. ',I3,' Z cood. set to ',F12.5)

    end subroutine readinputfile

! --------------------------- keywordcases -------------------------------------------

    subroutine keywordcases(xnumr,xnumc,ierror)

    !     routine:  keywordcases (remaned from NPUTQ)
    !     purpose: Handles CFAST datafile keywords
    !     Arguments: xnumr    number of rows in input file spreadsheet
    !                xnumc    number of columns in input file spreadsheet
    !                ierror  Returns error codes

    use precision_parameters
    use wallptrs
    use cenviro
    use cfast_main
    use cshell
    use iofiles
    use fltarget
    use objects1
    use objects2
    use opt
    use params
    use solver_parameters
    use thermp
    use wnodes
    implicit none

    integer, parameter :: maxin = 37
    
    integer, intent(in) :: xnumr, xnumc
    integer, intent(out) :: ierror
    
    logical :: lfupdat, countargs, lstat
    integer :: obpnt, compartment, lrowcount, nx, i1, i2, fannumber, iecfrom, iecto, mid, i, j, k, iijk, jik, koffst, iflgsetp, jmax, itop, ibot, npts, nto, ifrom, ito, nret, imin, iroom, iramp
    real(eb) :: initialopening, lrarray(ncol),minpres, maxpres, heightfrom, heightto, areafrom, areato, fanfraction, heatfplume, frac, tmpcond, dnrm2
    character :: cjtype*1,label*5, tcname*64, method*8, eqtype*3, venttype,orientypefrom*1, orientypeto*1
    character(128) :: lcarray(ncol)
    character(10) :: plumemodel(2)
    data plumemodel /'McCaffrey', 'Heskestad'/
    type(ramp_type), pointer :: rampptr

    !	Start with a clean slate

    lfupdat=.false.
    iflgsetp = 0
    setpfile = ' '
    do i = 1, nr
        do j = 1, 4
            cname(j,i) = 'OFF'
            switch(j,i) = .false.
        end do
        compartment = 0
        ierror = 0
        lrowcount = 1
    end do

10  CONTINUE

40  lrowcount = lrowcount + 1
    !	If we reach the end of the file, then we are done
    if (lrowcount>xnumr) return

    !	Copy a single row into local arrays for processing in readin; start with column two, assuming that the key word is the first entry!

    label = carray(lrowcount,1)
    if (label==' ') go to 40
    do i = 2, xnumc
        lcarray(i-1) = carray(lrowcount,i)
        lrarray(i-1) = rarray(lrowcount,i)
    end do

    !	Start the case statement for key words

    select case (label)

        ! TIMES total_simulation, print interval, smokeview interval, spreadsheet interval
    case ("TIMES")
        if (countargs(5,lcarray, xnumc-1, nret)) then
            nsmax =  lrarray(1)
            lprint = lrarray(2)
            ldiago = lrarray(3)
            if (ldiago>0) ndumpr = 1
            ldiagp = lrarray(4)
            lcopyss =  lrarray(5)
        else if (countargs(4,lcarray, xnumc-1, nret)) then
            nsmax =  lrarray(1)
            lprint = lrarray(2)
            ldiagp = lrarray(3)
            lcopyss =  lrarray(4)
        else 
            ierror = 1
            return
        endif

        ! TAMB REFERENCE AMBIENT TEMPERATURE (C), REFERENCE AMBIENT PRESSURE, REFERENCE PRESSURE, relative humidity
    case ("TAMB")
        if (.not.countargs(4,lcarray, xnumc-1, nret)) then
            ierror = 2
            return
        endif
        interior_temperature = lrarray(1)
        interior_abs_pressure = lrarray(2)
        relhum = lrarray(4)*0.01_eb
        if (.not.exset) then
            exterior_temperature = interior_temperature
            exterior_abs_pressure = interior_abs_pressure
            exterior_density = interior_density
        endif

        ! EAMB REFERENCE EXTERNAL AMBIENT TEMPERATURE (C), REFERENCE EXTERNAL AMBIENT PRESSURE
    case ("EAMB")
        if (.not.countargs(3,lcarray, xnumc-1, nret)) then
            ierror = 3
            return
        endif
        exterior_temperature = lrarray(1)
        exterior_abs_pressure = lrarray(2)
        exset = .true.

        ! Limiting oxygen index
    case ("LIMO2")
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 4
            return
        endif
        limo2 = lrarray(1)*0.01_eb 

        ! Rename the THERMAL DATA FILE
    case ("THRMF")
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 6
            return
        endif
        thrmfile = lcarray(1)

        ! Set the gaseous ignition temperature - this is a global parameter DJIGN
    case ('DJIGN')
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 4
            return
        endif
        tgignt = lrarray(2)

        ! Set global chemistry parameters.  With 2 parameters it's redundant with DJIGN and LIMO2. With more, it's part of a fire definition
    case ('GLOBA')
        if (countargs(2,lcarray,xnumc-1, nret)) then
            limo2 = lrarray(1)*0.01_eb
            tgignt = lrarray(2)
        else
            ierror = 4
            return
        endif

        ! MATL short_name conductivity specific_heat density thickness emissivity long_name
        ! HCl deposition constants are only available for gypsum so we just add the automatically if the name of the material contains gypsum
    case ('MATL')
        if(.not.countargs(7,lcarray,xnumc-1,nret)) then
            ierror = 6
            return
        endif
        maxct = maxct + 1
        if (maxct>nthmax) then
            write (logerr,'(a,i3)') 'Too many thermal properties',' in input data file. Limit is ',nthmax
            ierror = 203
            return
        endif

        nlist(maxct) = lcarray(1)
        lnslb(maxct) = 1
        lfkw(1,maxct) = lrarray(2)
        lcw(1,maxct) = lrarray(3)
        lrw(1,maxct) = lrarray(4)
        lflw(1,maxct) = lrarray(5)
        lepw(maxct) = lrarray(6)
        if (index(lcarray(1),'gypsum')/=0.or.index(lcarray(1),'GYPSUM')/=0.or.index(lcarray(7),'gypsum')/=0.or.index(lcarray(7),'GYPSUM')/=0) then
            lhclbf(1,maxct) = 0.0063_eb
            lhclbf(2,maxct) = 191.8_eb
            lhclbf(3,maxct) = 0.0587_eb
            lhclbf(4,maxct) = 7476.0_eb
            lhclbf(5,maxct) = 193.0_eb
            lhclbf(6,maxct) = 1.021_eb
            lhclbf(7,maxct) = 0.431_eb
        else
            do i = 1, 7
                lhclbf(i,maxct) = 0.0_eb
            end do
        endif

        ! COMPA	name(c), width(f), depth(f), height(f), absolute position (f) (3), ceiling_material(c), floor_material(c), wall_material (c) 
    case ('COMPA')
        if (.not.countargs(10,lcarray,xnumc-1,nret)) then
            ierror = 8
            return
        endif

        compartment = compartment + 1
        if (compartment>nr) then
            write (logerr, 5062) compartment
            ierror = 9
            return
        endif

        ! Name
        compartmentnames(compartment) = lcarray(1)

        ! Size
        br(compartment) = lrarray(2)
        dr(compartment) = lrarray(3)
        hr(compartment) = lrarray(4)
        cxabs(compartment) = lrarray(5)
        cyabs(compartment) = lrarray(6)
        hflr(compartment) = lrarray(7)

        ! Ceiling
        tcname = lcarray(8)
        if (tcname/='OFF') then
            switch(1,compartment) = .true.
            cname(1,compartment) = tcname
            ! keep track of the total number of thermal properties used
            numthrm = numthrm + 1
        endif

        ! floor
        tcname = lcarray(9)
        if (tcname/='OFF') then
            switch(2,compartment) = .true.
            cname(2,compartment) = tcname   
            ! keep track of the total number of thermal properties used
            numthrm = numthrm + 1
        endif

        ! walls
        tcname = lcarray(10)
        if (tcname/='OFF') then
            switch(3,compartment) = .true.
            cname(3,compartment) = tcname
            switch(4,compartment) = .true.
            cname(4,compartment) = tcname
            ! keep track of the total number of thermal properties used
            numthrm = numthrm + 1
        endif

        ! Reset this each time in case this is the last entry
        n = compartment+1
        nx = compartment

        write (logerr,5063) compartment, compartmentnames(nx), br(nx),dr(nx), hr(nx),cxabs(nx),cyabs(nx),hflr(nx),(switch(i,nx),i=1,4),(cname(i,nx),i=1,4)

        ! HVENT 1st, 2nd, which_vent, width, soffit, sill, wind_coef, hall_1, hall_2, face, opening_fraction
        !		    BW = width, HH = soffit, HL = sill, 
        !		    HHP = ABSOLUTE HEIGHT OF THE soffit,HLP = ABSOLUTE HEIGHT OF THE sill, HFLR = ABSOLUTE HEIGHT OF THE FLOOR (not set here)
        !		    WINDC = a wind coefficient which varies from -1 to +1 and is dimensionless
        !		    Compartment offset for the HALL command (2 of these)
        !		    VFACE = THE RELATIVE FACE OF THE VENT: 1-4 FOR X PLANE (-), Y PLANE (+), X PLANE (+), Y PLANE (-)
        !		    Initial open fraction
    case ('HVENT')
        if (.not.countargs(11,lcarray,xnumc-1,nret)) then
            ierror = 10
            return
        endif

        i = lrarray(1)
        j = lrarray(2)
        k = lrarray(3)
        imin = min(i,j)
        jmax = max(i,j)
        if (imin>nr-1.or.jmax>nr.or.imin==jmax) then
            write (logerr,5070) i, j
            ierror = 78
            return
        endif
        if (k>mxccv) then
            write (logerr,5080) i, j, k, nw(i,j)
            ierror = 78
            return
        endif
        nventijk = nventijk + 1
        if (nventijk>mxvents) then
            write(logerr,5081) i,j,k
            ierror = 78
            return
        endif
        ijk(i,j,k) = nventijk
        ijk(j,i,k) = ijk(i,j,k)
        iijk = ijk(i,j,k)
        jik = iijk
        koffst = 2**k
        if (iand(koffst,nw(i,j))/=0) write (iofilo,5090) i, j, k
        nw(i,j) = ior(nw(i,j),koffst)
        bw(iijk) = lrarray(4)
        hh(iijk) = lrarray(5)
        hl(iijk) = lrarray(6)
        windc(iijk) = lrarray(7)
        halldist(iijk,1) = lrarray(8)
        halldist(iijk,2) = lrarray(9)
        vface(iijk) = lrarray(10)
        initialopening = lrarray(11)

        qcvh(2,iijk) = initialopening
        qcvh(4,iijk) = initialopening

        hhp(iijk) = hh(iijk) + hflr(i)
        hlp(iijk) = hl(iijk) + hflr(i)

        ! connections are bidirectional

        nw(j,i) = nw(i,j)
        hh(jik) = min(hr(j),max(0.0_eb,hhp(jik)-hflr(j)))
        hl(jik) = min(hh(jik),max(0.0_eb,hlp(jik)-hflr(j)))

        ! assure ourselves that the connections are symmetrical

        hhp(jik) = hh(jik) + hflr(j)
        hlp(jik) = hl(jik) + hflr(j)
        hh(iijk) = min(hr(i),max(0.0_eb,hhp(iijk)-hflr(i)))
        hl(iijk) = min(hh(iijk),max(0.0_eb,hlp(iijk)-hflr(i)))
        
       ! DEADROOM dead_room_num connected_room_num
       ! pressure in dead_room_num is not solved.  pressure for this room
       ! is obtained from connected_room_num

    case ('DEADR')  
        i = lrarray(1)
        j = lrarray(2)
        if (i.ge.1.and.i.le.nr.and.j.le.1.and.j.le.nr.and.i.ne.j) then
           deadroom(i) = j
        endif

        ! EVENT - H First_Compartment     Second_Compartment	 Vent_Number Time Final_Fraction decay_time
        ! EVENT - V First_Compartment     Second_Compartment	 Not_Used	 Time Final_Fraction decay_time
        ! EVENT - M Not_Used				  Not_used				 M_ID        Time Final_Fraction decay_time
        ! EVENT - F Not_Used				  Not_used				 M_ID        Time Final_Fraction decay_time    
    case ('EVENT')
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 11
            return
        endif

        !	        Sort by event type, h, v, m, or f
        venttype = lcarray(1)

        select case (venttype)
        case ('H')
            if (.not.countargs(7,lcarray, xnumc-1, nret)) then
                ierror = 11
                return
            endif
            i = lrarray(2)
            j = lrarray(3)
            k = lrarray(4)
            iijk = ijk(i,j,k)
            qcvh(1,iijk) = lrarray(5)
            qcvh(3,iijk) = lrarray(5) + lrarray(7)
            qcvh(4,iijk) = lrarray(6)
        case ('V')
            if (.not.countargs(7,lcarray, xnumc-1, nret)) then
                ierror = 11
                return
            endif
            ! Sort these out in datacopy; we duplicate here so that readinputfile does not have to sort these as well
            itop = lrarray(2)
            ibot = lrarray(3)
            qcvpp(1,itop,ibot) = lrarray(5)
            qcvpp(3,itop,ibot) = lrarray(5) + lrarray(7)
            qcvpp(4,itop,ibot) = lrarray(6)
            qcvpp(1,ibot,itop) = lrarray(5)
            qcvpp(3,ibot,itop) = lrarray(5) + lrarray(7)
            qcvpp(4,ibot,itop) = lrarray(6)
        case ('M')
            if (.not.countargs(7,lcarray, xnumc-1, nret)) then
                ierror = 11
                return
            endif
            fannumber = lrarray(4)
            qcvm(1,fannumber) = lrarray(5)
            qcvm(3,fannumber) = lrarray(5) + lrarray(7)
            qcvm(4,fannumber) = lrarray(6)
        case ('F')
            if (.not.countargs(7,lcarray, xnumc-1, nret)) then
                ierror = 11
                return
            endif
            fannumber = lrarray(4)
            if (fannumber>nfan) then
                ierror = 82
                write(logerr,5196) fannumber
                return
            endif
            nfilter = nfilter + 1
            qcvf(1,fannumber) = lrarray(5)
            qcvf(3,fannumber) = lrarray(5) + lrarray(7)
            qcvf(4,fannumber) = lrarray(6)
        case default
            ierror = 71
            return
        end select 
        
        ! RAMP - from_compartment (or 0) to_compartment (or 0) vent_or_fire_number number_of_xy_pairs x1 y1 x2 y2 ... xn yn
    case ('RAMP')
        lstat = countargs(2,lcarray,xnumc-1,nret)
        if (nret<9) then
            ierror=11
            return
        else if (lrarray(5)<=1) then
            ierror=11
            return
        else if (nret/=5+2*lrarray(5)) then
            ierror=11
            return
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
        if (.not.countargs(5,lcarray, xnumc-1, nret)) then
            ierror = 23
            return
        endif
        i = lrarray(1)
        j = lrarray(2)
        ! check for outside of compartment space; self pointers are covered in readinputfile
        if (i>nr.or.j>nr) then
            write (logerr,5070) i, j
            ierror = 79
            return
        endif

        ! readinputfile will verify the orientation (i is on top of j)
        nwv(i,j) = 1
        vvarea(i,j) = lrarray(3)
        ! check the shape parameter. the default (1) is a circle)
        if (lrarray(4)<1.or.lrarray(4)>2) then
            vshape(i,j) = 1
        else
            vshape(i,j) = lrarray(4)
        endif
        qcvpp(2,i,j) = lrarray(5)
        qcvpp(2,j,i) = lrarray(5)
        qcvpp(4,i,j) = lrarray(5)
        qcvpp(4,j,i) = lrarray(5)

        ! WIND - VELOCITY AT REFERENCE HEIGHT and EXPONENTIAL LAPSE RATE
    case ('WIND')
        if (.not.countargs(3,lcarray, xnumc-1, nret)) then
            ierror = 24
            return
        endif
        windv = lrarray(1)
        windrf = lrarray(2)
        windpw = lrarray(3)

        ! MVENT - simplified mechanical ventilation

        ! (1) From_Compartment, (2) To_Compartment, (3) ID_Number
        ! (4-6) From_Opening_Orientation From_Center_Height From_Opening_Area 
        ! (7-9) To_Opening_Orientation To_Center_Height To_Opening_Area 
        ! (10-12) Flow Flow_Begin_Dropoff_Pressure Zero_Flow_Pressure
        ! (13) Initial fraction of the fan speed
    case ('MVENT')
        if (.not.countargs(13,lcarray,xnumc-1,nret)) then 
            ierror = 12
            return
        endif
        mid = lrarray(3)
        iecfrom = lrarray(1)
        iecto = lrarray(2)
        if (iecfrom>n.or.iecto>n) then
            write(logerr,5191) iecfrom, iecto
            ierror = 67
            return
        endif

        orientypefrom = lcarray(4)
        heightfrom = lrarray(5)
        areafrom = lrarray(6)
        orientypeto = lcarray(7)
        heightto = lrarray(8)
        areato = lrarray(9)
        minpres = lrarray(11)
        maxpres = lrarray(12)
        fanfraction = lrarray(13)

        ! We start with two new nodes for the openings into the compartments for connections to the fan

        ! first compartment/node opening
        next = next + 1
        nnode = nnode + 1
        if (next>mext.or.nnode>mnode) then
            write (logerr,5192) next,nnode
            ierror = 68
            return
        endif
        if (orientypefrom=='V') then
            hvorien(next) = 1
        else
            hvorien(next) = 2
        endif
        hvnode(1,next) = iecfrom
        hvnode(2,next) = nnode
        hvelxt(next) = heightfrom
        arext(next) = areafrom

        ! second compartment/node opening
        next = next + 1
        nnode = nnode + 1
        if (next>mext.or.nnode>mnode) then
            write (logerr,5192) next,nnode
            ierror = 68
            return
        endif
        if (orientypeto=='V') then
            hvorien(next) = 1
        else
            hvorien(next) = 2
        endif
        hvnode(1,next) = iecto
        hvnode(2,next) = nnode
        hvelxt(next) = heightto
        arext(next) = areato

        ! now connect nodes 1 and 2 with a fan

        if (minpres>maxpres) then
            write (logerr,5194) minpres,maxpres
            ierror = 70
            return
        endif

        nfan = nfan + 1
        if (mid/=nfan) then
            write(logerr,5193) mid,nfan
            ierror = 68
            return
        endif

        nbr = nbr + 1
        if (nfan>mfan.or.nbr>mbr) then
            write (iofilo,5195) mfan
            ierror = 70
            return
        endif

        nf(nbr) = nfan
        nfc(nfan) = 1
        na(nbr) = hvnode(2,next-1)
        ne(nbr) = hvnode(2,next)
        hvdvol(nbr) = 0.0_eb
        hmin(nfan) = minpres
        hmax(nfan) = maxpres
        hvbco(nfan,1) = lrarray(10)

        ! add a simple duct to connect the two nodes/fan - this is artificial since we do not worry about the species in the system
        ndt = ndt + 1

        ! to change from the zero volume calculation to a finite volume, use 1.0d1 (10 meter duct)
        ! the effect is in hvfrex. case 1 is the finite volume and case 2, the zero volume calculation for flow through the external nodes
        dl(ndt) = 0.0_eb ! 1.0d1
        de(ndt) = lrarray(6)
        ibrd(ndt) = nbr

        ! finally, we set the initial fraction opening
        qcvm(2,mid) = lrarray(13)
        qcvm(4,mid) = lrarray(13)

        ! FIRE room pos(3) plume ignition_type ignition_criterion normal(3) name
        ! This is almost the same as the older OBJEC keyword (name is moved to the end to make it more consistent with other keywords
        ! With the FIRE keyword, the rest of the fire definition follows in CHEMI, TIME, HRR, SOOT, CO, and TRACE keywords
        ! For now, we assume that the input file was written correctly by the GUI and just set an index for the forthcoming keywords
    case ('FIRE')
        if (.not.countargs(11,lcarray, xnumc-1, nret)) then
            ierror = 32
            return
        endif
        if (numobjl>=mxoin) then
            write(logerr,5300)
            go to 10
        endif
        iroom = lrarray(1)
        if (iroom<1.or.iroom>n-1) then
            write(logerr,5320)iroom
            ierror = 33
            return
        endif
        obpnt = numobjl + 1
        numobjl = obpnt

        ! Only constrained fires
        objtyp(numobjl) = 2
        if (objtyp(numobjl)>2) then
            write(logerr,5321) objtyp(numobjl)
            ierror = 63
            return
        endif

        objpos(1,obpnt) = lrarray(2)
        objpos(2,obpnt) = lrarray(3)
        objpos(3,obpnt) = lrarray(4)
        if (objpos(1,obpnt)>br(iroom).or.objpos(2,obpnt)>dr(iroom).or.objpos(3,obpnt)>hr(iroom)) then
            write(logerr,5323) obpnt
            ierror = 82
            return
        endif

        fplume(numobjl) = lrarray(5)
        if(fplume(numobjl)<1.or.fplume(numobjl)>2) then
            write(logerr,5402) fplume(numobjl)
            ierror = 78
            return 
        endif
        write(logerr,5403) plumemodel(fplume(numobjl))
        objign(obpnt) =   lrarray(6)
        tmpcond =         lrarray(7)
        objort(1,obpnt) = lrarray(8)
        objort(2,obpnt) = lrarray(9)
        objort(3,obpnt) = lrarray(10)

        ! Enforce sanity; normal pointing vector must be non-zero (blas routine)
        if (dnrm2(3,objort(1,obpnt),1)<=0.0) then
            write(logerr,5322)
            ierror = 216
            return
        endif

        objrm(obpnt) = iroom
        objnin(obpnt) = lcarray(11)
        objld(obpnt) = .true.
        objon(obpnt) = .false.
        ! This is redudant but needed to be compatible with the object database format
        objpnt(obpnt) = obpnt

        ! Note that ignition type 1 is time, type 2 is temperature and 3 is flux
        ! The critiria for temperature and flux are stored backupwards - this is historical
        ! See corresponding code in updobj
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
                ierror = 13
                return
            endif
        else
            objon(obpnt) = .true.
        endif
        if (option(fbtobj)==off.and.objign(obpnt)/=1.0_eb) then
            if (stpmax>0.0_eb) then
                stpmax = min(stpmax,1.0_eb)
            else
                stpmax = 1.0_eb
            endif
        endif 

        ! read and set the other stuff for this fire
        call inputembeddedfire(objnin(obpnt), lrowcount, xnumc, obpnt, ierror)
        if (ierror/=0) return

        ! OBJEC name room pos(3) plume ignition_type ignition_criterion normal(3)
    case ('OBJEC')

        if (.not.countargs(11,lcarray, xnumc-1, nret)) then
            write(logerr,5310)
            ierror = 32
            return
        endif
        if (numobjl>=mxoin) then
            write(logerr,5300)
            go to 10
        endif
        tcname = lcarray(1)
        iroom = lrarray(2)
        if (iroom<1.or.iroom>n-1) then
            write(logerr,5320)iroom
            ierror = 33
            return
        endif
        obpnt = numobjl + 1
        numobjl = obpnt

        ! Only constrained fires
        objtyp(numobjl) = 2
        if (objtyp(numobjl)>2) then
            write(logerr,5321) objtyp(numobjl)
            ierror = 63
            return
        endif

        objpos(1,obpnt) = lrarray(3)
        objpos(2,obpnt) = lrarray(4)
        objpos(3,obpnt) = lrarray(5)
        if (objpos(1,obpnt)>br(iroom).or.objpos(2,obpnt)>dr(iroom).or.objpos(3,obpnt)>hr(iroom)) then
            write(logerr,5323) obpnt
            ierror = 82
            return
        endif

        fplume(numobjl) = lrarray(6)
        if(fplume(numobjl)<1.or.fplume(numobjl)>2) then
            write(logerr,5402) fplume(numobjl)
            ierror = 78
            return 
        endif
        write(logerr,5403) plumemodel(fplume(numobjl))
        objign(obpnt) =   lrarray(7)
        tmpcond =         lrarray(8)
        objort(1,obpnt) = lrarray(9)
        objort(2,obpnt) = lrarray(10)
        objort(3,obpnt) = lrarray(11)
        ! Enforce sanity; normal pointing vector must be non-zero (blas routine)
        if (dnrm2(3,objort(1,obpnt),1)<=0.0) then
            write(logerr,5322)
            ierror = 216
            return
        endif
        objrm(obpnt) = iroom
        objnin(obpnt) = tcname
        objld(obpnt) = .true.
        objon(obpnt) = .false.
        ! This is redudant but needed to be compatible with the object database format
        objpnt(obpnt) = obpnt

        !!!!! Note that ignition type 1 is time, type 2 is temperature and 3 is flux !!!
        !!!!! The critiria for temperature and flux are stored backupwards - this is historical
        !!!!! See corresponding code in updobj
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
                ierror = 13
                return
            endif
        else
            objon(obpnt) = .true.
        endif
        if (option(fbtobj)==off.and.objign(obpnt)/=1.0_eb) then
            if (stpmax>0.0_eb) then
                stpmax = min(stpmax,1.0_eb)
            else
                stpmax = 1.0_eb
            endif
        endif

        ! CJET - Ceiling jet for walls, ceiling, all, or off
    case ('CJET')
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 34
            return
        endif
        DO  I = 1, 5
            cjeton(i) = .false.
        end do
        cjtype = lcarray(1)(1:1)
        if (cjtype/=' ') then
            if (cjtype=='C') then
                cjeton(1) = .true.
                cjeton(5) = .true.
            else if (cjtype=='W') then
                cjeton(1) = .true.
                cjeton(3) = .true.
                cjeton(5) = .true.
            else if (cjtype=='A') then
                cjeton(1) = .true.
                cjeton(3) = .true.
                cjeton(4) = .true.
                cjeton(5) = .true.
            endif
        endif
        write(logerr,5341) cjeton

        ! STPMAX # - set the maximum time step to #
    case ('STPMA')
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 35
            return
        endif
        stpmax = lrarray(1)

        ! DETECT Type Compartment Activation_Temperature Width Depth Height RTI Suppression Spray_Density
    case ('DETEC')
        if (.not.countargs(9,lcarray, xnumc-1, nret)) then
            ierror = 34
            return
        endif

        ndtect = ndtect + 1
        if (ndtect>mxdtect) then
            write (logerr, 5338)
            ierror = 81
            return
        endif

        i1 = lrarray(1)
        i2 = lrarray(2)
        ! force to heat detector if out of range
        if (i1>3) i1 = heatd
        ixdtect(ndtect,dtype) = i1
        iroom = i2
        ixdtect(ndtect,droom) = iroom
        if(iroom<1.or.iroom>nr)then
            write (logerr,5342) i2
            ierror = 35
            return
        endif

        xdtect(ndtect,dtrig) = lrarray(3)
        xdtect(ndtect,dxloc) = lrarray(4)
        xdtect(ndtect,dyloc) = lrarray(5)
        xdtect(ndtect,dzloc) = lrarray(6)
        xdtect(ndtect,drti) =  lrarray(7)
        ixdtect(ndtect,dquench) = lrarray(8)
        xdtect(ndtect,dspray) = lrarray(9)*1000.0_eb
        ! if spray density is zero, then turn off the sprinkler
        if(xdtect(ndtect,dspray)==0.0_eb)then
            ixdtect(ndtect,dquench) = 0
        endif
        if(option(fbtdtect)==off.and.ixdtect(ndtect,dquench)>0)then
            if (stpmax>0) then
                stpmax = min(stpmax,1.0_eb)
            else
                stpmax = 1.0_eb
            endif
        endif
        if (compartmentnames(i2)==' ') then
            write(logerr,5344) i2
            ierror = 36
            return
        else
            write(logerr, 5343) i1,compartmentnames(i2)
        endif

        if (debugging) then
            write(*,5400) (xdtect(ndtect,i),i=1,dtxcol)
            write(*,5401) (ixdtect(ndtect,i),i=1,dticol)
            write(*,*)
        endif

        if(xdtect(ndtect,dxloc)>br(i2).or.xdtect(ndtect,dyloc)>dr(i2).or.xdtect(ndtect,dzloc)>hr(i2)) then
            write(logerr,5339) ndtect,compartmentnames(i2)
            ierror = 80
            return
        endif

        !     VHEAT top_compartment bottom_compartment
    case ('VHEAT')
        if (.not.countargs(2,lcarray, xnumc-1, nret)) then
            ierror = 37
            return
        endif

        i1 = lrarray(1)
        i2 = lrarray(2)
        if (i1<1.or.i2<1.or.i1>n.or.i2>n) then
            write(logerr,5345) i1, i2
            ierror = 38
            return
        endif

        nswal = nswal + 1
        izswal(nswal,w_from_room) = i1
        izswal(nswal,w_from_wall) = 1
        izswal(nswal,w_to_room) = i2
        izswal(nswal,w_to_wall) = 3

        ! ONEZ compartment number - This turns the compartment into a single zone
    case ('ONEZ')
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 39
            return
        endif

        iroom = lrarray(1)
        if(iroom<1.or.iroom>n)then
            write(logerr, 5001) i1
            ierror = 40
            return
        endif
        izshaft(iroom) = 1
        !	TARGET - Compartment position(3) normal(3) Material Method Equation_Type
    case ('TARGE')
        if (countargs(10,lcarray, xnumc-1, nret).or.countargs(11,lcarray, xnumc-1, nret)) then
            if(ntarg+1>mxtarg)then
                write(logerr,5002) 
                ierror = 42
                return
            else
                ntarg = ntarg + 1
            endif

            ! The target can exist, now for the compartment
            IROOM = lrarray(1)
            IF(IROOM<1.OR.IROOM>N)THEN
                write(logerr,5003) iroom
                ierror = 43
                return
            endif

            ! position and normal
            ixtarg(trgroom,ntarg)=iroom
            do i = 0, 2
                xxtarg(trgcenx+i,ntarg) = lrarray(2+i)
                xxtarg(trgnormx+i,ntarg) = lrarray(5+i)
            end do
            if (countargs(11,lcarray, xnumc-1, nret)) then
                xxtarg(trginterior,ntarg) = lrarray(11)
            else
                xxtarg(trginterior,ntarg) = 0.5
            endif

            ! material type
            tcname = lcarray(8)
            if(tcname==' ') tcname='DEFAULT'
            cxtarg(ntarg) = tcname
            ixtarg(trgwall,ntarg) = 0

            ! solution method
            method = ' '
            method = lcarray(9)
            call upperall(method,method)
            if(method/=' ')then
                if(method(1:3)=='STE') then
                    ixtarg(trgmeth,ntarg) = STEADY
                    method = ' '
                elseif (method(1:3)=='IMP') then
                    ixtarg(trgmeth,ntarg) = MPLICIT
                elseif (method(1:3)=='EXP') then
                    ixtarg(trgmeth,ntarg) = XPLICIT
                else
                    write(logerr,912) method
                    ierror = 44
                    return
                endif
            endif

            ! equation type
            eqtype = ' '
            eqtype = lcarray(10)
            call upperall(eqtype,eqtype)
            if(eqtype/=' '.and.method/=' ')then
                if (eqtype(1:3)=='ODE') then
                    ixtarg(trgeq,ntarg) = ODE
                elseif (eqtype(1:3)=='PDE') then
                    ixtarg(trgeq,ntarg) = PDE
                elseif (eqtype(1:3)=='CYL') then
                    ixtarg(trgeq,ntarg) = CYLPDE
                else
                    write(logerr,913) eqtype
                    ierror = 45
                    return
                endif
            endif
        else
            ierror = 41
            return
        endif
        ! HALL Compartment Velocity Depth Decay_Distance
    case ('HALL')
        if (.not.countargs(4,lcarray, xnumc-1, nret)) then
            ierror = 46
            return
        endif

        IROOM = lrarray(1)

        ! check that specified room is valid
        if(iroom<0.or.iroom>n)then
            write(logerr,5346) iroom
            ierror = 63
            return
        endif

        izhall(iroom,ihroom) = 1
        izhall(iroom,ihvelflag) = 0
        izhall(iroom,ihdepthflag) = 0
        izhall(iroom,ihventnum) = 0
        zzhall(iroom,ihtime0) = -1.0_eb
        zzhall(iroom,ihvel) = 0.0_eb
        zzhall(iroom,ihdepth) = -1.0_eb
        zzhall(iroom,ihhalf) = -1.0_eb

        ! HALL velocity; not set if negative
        if(lrarray(2)>=0) then
            zzhall(iroom,ihvel) = lrarray(2)
            izhall(iroom,ihvelflag) = 1
        endif

        ! HALL layer depth; not set if negative
        if (lrarray(3)>=0) then
            zzhall(iroom,ihdepth) = lrarray(3)
            izhall(iroom,ihdepthflag) = 1
        endif

        ! HALL temperature decay distance (temperature decays by 0.50); if negative, not set
        if (lrarray(4)>=0) then
            zzhall(iroom,ihhalf) = lrarray(4)
            izhall(iroom,ihhalfflag) = 1
            izhall(iroom,ihmode) = ihbefore
        endif

        ! ROOMA Compartment Number_of_Area_Values Area_Values
        ! This provides for variable compartment floor areas; this should be accompanied by the roomh command
    case ('ROOMA')
        if (.not.countargs(2,lcarray, xnumc-1, nret)) then
            ierror = 47
            return
        endif

        IROOM = lrarray(1)

        ! make sure the room number is valid
        if(iroom<1.or.iroom>n)then
            write(logerr,5347) iroom
            ierror = 48
            return
        endif

        ! make sure the number of points is valid
        npts = lrarray(2)
        if(npts>mxpts.or.npts<=0.or.npts/=nret-2) then
            write (logerr,5347) npts
            ierror = 49
            return
        endif
        if(izrvol(iroom)/=0) npts = min(izrvol(iroom),npts)
        izrvol(iroom) = npts

        ! make sure all data is positive 
        do  i = 1, npts
            if(lrarray(i+2)<0.0_eb)then
                write(logerr,5348) lrarray(i+2)
                ierror = 50
                return
            endif
        end do

        ! put the data in its place
        do i = 1, npts
            zzrarea(i,iroom) = lrarray(i+2)
        end do
        write(logerr,5351) iroom, (zzrarea(i,iroom),i=1,npts)

        ! ROOMH Compartment Number_of_Height_Values Height_Values
        ! This companion to ROOMA, provides for variable compartment floor areas; this should be accompanied by the ROOMA command
    case ('ROOMH')
        if (.not.countargs(2,lcarray, xnumc-1, nret)) then
            ierror = 51
            return
        endif

        iroom = lrarray(1)

        ! make sure the room number is valid
        if(iroom<1.or.iroom>n)then
            write(logerr,5349) iroom
            ierror = 52
            return
        endif

        ! make sure the number of points is valid
        npts = lrarray(2)
        if(npts>mxpts.or.npts<0.or.npts/=nret-2)then
            write(logerr,5350) npts
            ierror = 53
            return
        endif
        if(izrvol(iroom)/=0)npts = min(izrvol(iroom),npts)
        izrvol(iroom) = npts

        ! make sure all data is positive 
        do i = 1, npts
            if(lrarray(i+2)<0.0_eb)then
                write(logerr,5348) lrarray(i+2)
                ierror = 54
                return
            endif
        end do

        ! put the data in its place
        do i = 1, npts
            zzrhgt(i,iroom) = lrarray(i+2)
        end do
        write(logerr,5352) iroom, (zzrhgt(i,iroom),i=1,npts)

        ! DTCHE Minimum_Time_Step Maximum_Iteration_Count
    case ('DTCHE')
        if (.not.countargs(2,lcarray, xnumc-1, nret)) then
            ierror = 55
            return
        endif

        zzdtcrit = abs(lrarray(1))
        izdtmax = abs(lrarray(2))
        ! a negative turns off the check
        if(lrarray(2)<=0)izdtflag = .false.

        ! SETP file_name
    case ('SETP')
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 56
            return
        endif

        if (iflgsetp>0) then
            ierror = 57
            write (logerr,5353) setpfile
            return
        else
            iflgsetp = 1
            setpfile = lcarray(1)
            write (logerr,5340) setpfile
        endif

        ! Horizontal heat flow, HHEAT First_Compartment Number_of_Parts N pairs of {Second_Compartment, Fraction}

        ! There are two forms of the command
        !   The first (single entry of the room number) - all connections based on horizontal flow
        !   The second is the compartment number followed by N pairs of compartments to which the heat will flow and the fraction of the vertical surface of the compartment that loses heat
    case ('HHEAT')
        if (.not.countargs(1,lcarray, xnumc-1, nret)) then
            ierror = 58
            return
        endif

        nto = 0
        ifrom = lrarray(1)

        if (nret==1) then
            izheat(ifrom) = 1
            go to 10
        else
            nto = lrarray(2)
            if(nto<1.or.nto>n)then
                write(logerr,5354) nto
                ierror = 59
                return
            endif
            izheat(ifrom) = 2
            izheat(ifrom) = 2
        endif

        if (2*nto/=(nret-2)) then
            write(logerr,5355) ifrom, nto
            ierror = 60
            return
        endif

        do i = 1, nto
            i1 = 2*i+1
            i2 = 2*i+2
            ito = lrarray(i1)
            frac = lrarray(i2)
            if(ito<1.or.ito==ifrom.or.ito>n)then
                write(logerr, 5356) ifrom,ito
                ierror = 61
                return
            endif
            if(frac<0.0_eb.or.frac>1.0_eb)then
                write(logerr, 5357) ifrom,ito,frac
                ierror = 62
                return
            endif
            zzhtfrac(ifrom,ito) = frac
        end do

        ! FURN - no fire, heat walls according to a prescribed time temperature curve
    case ('FURN')
        nfurn=lrarray(1)+0.5
        do i = 1, nfurn
            furn_time(i)=lrarray(2*i)
            furn_temp(i)=lrarray(2*i+1)
        end do

        !  HEATF Special fire - heat source only; no mass
    case ('HEATF')
        if (.not.countargs(6,lcarray, xnumc-1, nret)) then
            ierror = 65
            return
        endif
        heatfr = lrarray(1)
        if(heatfr<1.or.heatfr>n-1) then
            ierror = 66
            return
        endif
        heatfl = .true.
        heatfp(1) = lrarray(2)
        heatfp(2) = lrarray(3)
        heatfp(3) = lrarray(4)
        heatfplume =  lrarray(5)

        ! Outdated keywords
    case ('OBJFL','MVOPN','MVFAN','MAINF','INTER')
        ierror = 5
        return


    case default
        write(logerr, 5051) label
    end select
    go to 10 

912 format ('***Error: Invalid TARGET METHOD:',A8,'. Valid choices are: ','STEADY, IMPLICIT OR EXPLICIT')
913 format('***Error: Invalid equation type specified in TARGET:',A3,' Valid choices are:ODE, PDE or CYL')
5001 format ('***Error: ONEZ requires a defined compartment ',i3)
5002 format ('***Error: Too many targets are being defined')
5003 format ('***Error: The compartment specified by TARGET does not exist',i3)
5051 format ('***Error: The key word ',a5,' is not recognized')
5062 format ('***Error: Compartment number outside of allowable range',i5)
5063 format ('Compartment ',i3,1x,a8,1x,6f6.1,4l1,1x,4a10)
5070 format ('***Error: VENT parameter(s) outside of allowable range',2I4)
5080 format ('***Error: Too many pairwise horizontal connections',4I5)
5081 format ('***Error: Too many horizontal connections ',3i5)
5090 format ('The connection',3I3,' is being redefined')
5191 format ('***Error: Compartments specified in MVENT have not been defined ',2i3)
5192 format ('***Error: Exceeded maximum number of nodes/openings in MVENT ',2i3)
5193 format ('***Error: MVENT(MID) is not consistent ',2i3)
5194 format ('***Error: Pressure for zero flow must exceed the lower limit',f10.2)
5195 format ('***Error: Too many fan systems',i3)
5196 format ('***Error: Fan (MID) has not been defined for this filter ',i3)
5300 format ('***Error: Too many objects defined in datafile')
5310 format ('***Error: Incorrect number of parameters for OBJECT')
5320 format ('***Error: Object specification error, room ',I4,' out of range')
5321 format ('***Error: Object specification error, not an allowed fire type',i3)
5322 format ('***Error: Object normal vector must be non-zero')
5323 format ('***Error: Object ',i3,' is outside its compartment')
5338 format ('***Error: Exceed allowed number of detectors')
5339 format ('***Error: Detector ',i3,' is outside of compartment ',a)
5340 format ('Set point file name is - ',A)
5341 format ('Ceiling jet calculation has been set: ',5l2)
5342 format ('***Error: Invalid DETECTOR specification - room ',i3)
5343 format ('A type ',i3,' detector has been placed in ',a128)
5344 format ('***Error: A referenced compartment is not yet defined ',i3)
5345 format ('***Error: VHEAT has specified a non-existent compartment')
5346 format ('***Error: HALL has specified a non-existent compartment',i3)
5347 format ('***Error: Compartment specified by ROOMA does not exist ',i3)
5348 format ('***Error: Data on the ROOMA (or H) line must be positive ',1pg12.3)
5349 format ('***Error: Compartment specified by ROOMH is not defined ',i3)
5350 format ('***Error: ROOMH error on data line ',i3)
5351 format ('Compartment',i3,' has been redefined as a variable space' ,'- area: ',20f8.1)
5352 format ('Compartment',i3,' has been redefined as a variable space' ,'- height: ',20f8.1)
5353 format ('Trying to reset the point file name is - ',A)
5354 format ('***Error: HHEAT to compartment out of bounds or not defined - ',i3)
5355 format ('***Error: HHEAT fraction pairs is not consistent ',2i3)
5356 format ('***Error: HHEAT specification error in compartment pairs: ',2i3)
5357 format ('***Error: Error in fraction for HHEAT:',2i3,f6.3)
5358 format ('***Error: Not a valid ignition criterion ',I5)
5400 format ('xdtect = ',15f8.1)
5401 format ('ixdtect = ',4i5)
5402 format ('***Error: Plume index out of range ',i3)
5403 format ('Plume model for this fire: ',a10)

    end subroutine keywordcases

! --------------------------- inputembeddedfire -------------------------------------------

    subroutine inputembeddedfire(objname, lrowcount, xnumc, iobj, ierror)

    !     routine: inputembeddedfire
    !     purpose: This routine reads a new format fire definition that begins with a FIRE keyword (already read in keywordcases)
    !              followed by CHEMI, TIME, HRR, SOOT, CO, TRACE, AREA, and HEIGH keywords (read in here)
    !     Arguments: objname: name of this fire object
    !                iroom: compartment where this fire is located
    !                lrowcount: current row in the input file.  We begin one row after this one
    !                xnumr:   number of rows in the input file
    !                xnumc:   number of columns in the input file
    !                iobj:    pointer to the fire object that will contain all the data we read in here
    !                ierror:  error return index

    use precision_parameters
    use cfast_main
    use iofiles
    use objects2
    implicit none

    integer, intent(in) :: xnumc, iobj
    character(*), intent(in) :: objname
    integer, intent(out) :: lrowcount, ierror
    
    logical :: countargs, lstat
    character(128) :: lcarray(ncol)
    character(5) :: label
    integer :: logerr = 3, midpoint = 1, base = 2, errorcode, ir, i, ii, nret
    real(eb) :: lrarray(ncol), ohcomb, max_area, max_hrr, hrrpm3, minimumheight = 1.e-3_eb, area, d, flamelength

    ! there are eight required inputs for each fire
    do ir = 1, 8
        lrowcount = lrowcount + 1
        label = carray(lrowcount,1)
        if (label==' ') cycle
        do i = 2, xnumc
            lcarray(i-1) = carray(lrowcount,i)
            lrarray(i-1) = rarray(lrowcount,i)
        end do

        select case (label)

            ! The new CHEMIE line defines chemistry for the current fire object.  This includes chemical formula, radiative fraction, heat of combustion, and material
        case ('CHEMI')
            if (.not.countargs(8,lcarray,xnumc-1,nret)) then
                ierror = 4
                return
            endif

            ! define chemical formula
            obj_c(iobj) = lrarray(1)
            obj_h(iobj) = lrarray(2)
            obj_o(iobj) = lrarray(3)
            obj_n(iobj) = lrarray(4)
            obj_cl(iobj) = lrarray(5)
            objgmw(iobj) = (12.01*obj_c(iobj) + 1.008*obj_h(iobj) + 16.0*obj_o(iobj) + 14.01*obj_n(iobj) + 35.45*obj_cl(iobj))/1000.0
            objvt(iobj) = te

            radconsplit(iobj) = lrarray(6)
            ohcomb = lrarray(7)
            if (ohcomb<=0.0_eb) then
                write(logerr,5001) ohcomb
                ierror = 32
                return
            endif  
            omatl(iobj) = lcarray(8)
        case ('TIME')
            lstat = countargs(2,lcarray,xnumc-1,nret)
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
                ! The minimum area is to stop dassl from an floating point underflow when it tries to extrapolate back to the ignition point.
                ! It only occurs for objects which are on the floor and ignite after t=0
                oarea(ii,iobj) = max(lrarray(ii),0.09_eb)
                max_area = max(max_area,oarea(ii,iobj))
            end do

            ! calculate size of the object based on the maximum area with a thickness assuming it's a cube
            ! as with the flame height calculation, the minimum area is 0.09 m^2 (about 1 ft^2)
            objxyz(1,iobj) = sqrt(max(max_area,0.09_eb))
            objxyz(2,iobj) = objxyz(1,iobj)
            objxyz(3,iobj) = objxyz(1,iobj)

            ! calculate a characteristic length of an object (we assume the diameter). 
            ! This is used for point source radiation fire to target calculation as a minimum effective distance between the fire and the target
            ! which only impact very small fire to target distances
            objclen(iobj) = 2.0_eb*sqrt(max_area/pi)
        case ('HEIGH')
            do ii = 1, nret
                ohigh(ii,iobj) = max(lrarray(ii),0.0_eb)
            end do
        case default
            write(logerr, 5000) label
        end select

    end do

    ! set the heat of combustion - this is a problem if the qdot is zero and the mdot is zero as well
    call sethoc (objlfm(iobj), omass(1,iobj), oqdot(1,iobj), objhc(1,iobj), ohcomb)

    ! Position the object
    call positionobject(objpos,1,iobj,objrm(iobj),br,midpoint,minimumheight,errorcode)
    if (errorcode/=0) return
    call positionobject(objpos,2,iobj,objrm(iobj),dr,midpoint,minimumheight,errorcode)
    if (errorcode/=0) return
    call positionobject(objpos,3,iobj,objrm(iobj),hr,base,minimumheight,errorcode)
    if (errorcode/=0) return

    ! diagnostic - check for the maximum heat release per unit volume.
    ! first, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
    area = objxyz(1,iobj)*objxyz(2,iobj)
    d = max(0.33_eb,sqrt(4.0/3.14_eb*area))
    flamelength = d*(0.235_eb*(max_hrr/1.0e3_eb)**0.4_eb - 1.02_eb)
    flamelength = max (0.0_eb, flamelength)
    ! now the heat realease per cubic meter of the flame - we know that the size is larger than 1.0d-6 m^3 - enforced above
    hrrpm3 = max_hrr/(area*(objxyz(3,iobj)+flamelength))
    if (hrrpm3>4.0e6_eb) then
        write (logerr,5106)trim(objname),(objpos(i,iobj),i=1,3),hrrpm3
        errorcode = 221
        return
    else if (hrrpm3>2.0e6_eb) then
        write (logerr,5107)trim(objname),(objpos(i,iobj),i=1,3),hrrpm3
    else 
        write (logerr,5100)trim(objname),(objpos(i,iobj),i=1,3),hrrpm3
    endif

    return
5001 format ('Invalid heat of combustion, must be greater than zero, ',1pg12.3)
5100 format ('Object ',a,' position set to ',3F7.3, '; Maximum HRR per m^3 is ',1pg10.3)
5106 format ('Object ',a,' position set to ',3F7.3,'; Maximum HRR per m^3 = ',1pg10.3,' exceeds physical limits')
5107 format ('Object ',a,' position set to ',3F7.3,'; Maximum HRR per m^3 = ',1pg10.3,' exceeds nominal limits')
5000 format ('The key word ',a5,' is not part of a fire definition')

    end subroutine inputembeddedfire

! --------------------------- initfireobject -------------------------------------------

    subroutine initfireobject (iobj, ierror)

    !     routine: initfireobject
    !     purpose: This routine sets default values for new fire object targets created to monitor temperature and flux of the object prior to ignition
    !     Arguments: iobj: fire object number
    !                ierror: non zero on output if we exceed the maximum number of targets creating this target.

    use cfast_main
    use fltarget
    use objects2
    use cshell, only : logerr
    implicit none
    
    integer, intent(in) :: iobj
    integer, intent(out) :: ierror

    ntarg = ntarg + 1
    if (ntarg>mxtarg) then
        write(logerr, *) '***Error: Too many targets created for fire objects'
        ierror = 201
        return
    endif
    obtarg(iobj) = ntarg
    cxtarg(ntarg) = omatl(iobj)

    ! Initialize object target
    call set_target_object (ntarg,iobj)
    return
    end subroutine initfireobject

! --------------------------- readcfl -------------------------------------------

    subroutine readcf1 (errorcode)

    !     routine: readcf1
    !     purpose: get the paths and project base name open the input file for reading (1)
    ! 	         delete the output files
    ! 	         open the log file (3)
    ! 	         call the input routines
    !     arguments: errorcode: return error indication if non-zero

    use iofiles
    use debug
    implicit none

    integer, intent(out) :: errorcode
    
    integer :: lp, ld, ios
    character(256) :: testpath, testproj 

    ! get the path and project names
    errorcode = 0
    call exehandle (exepath, datapath, project, errorcode)
    if (errorcode>0) return

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

    call deleteoutputfiles (outputfile)
    call deleteoutputfiles (smvhead)
    call deleteoutputfiles (smvdata)
    call deleteoutputfiles (smvcsv)
    call deleteoutputfiles (ssflow)
    call deleteoutputfiles (ssnormal)
    call deleteoutputfiles (ssspecies)
    call deleteoutputfiles (sswall)
    call deleteoutputfiles (errorlogging)
    call deleteoutputfiles (stopfile)
    call deleteoutputfiles (historyfile)
    call deleteoutputfiles (statusfile)
    call deleteoutputfiles (queryfile)
    call deleteoutputfiles (kernelisrunning)

    ! since we have reached this point, the output files are available and stop has been turned off.
    ! open the log file and return the correct project name
    open (unit=3, file=errorlogging, action='write', iostat=ios, status='new')

    project = testproj (1:ld)
    errorcode = ios

    return
    end subroutine readcf1

! --------------------------- setp0 -------------------------------------------

    subroutine setp0 (p0, ip0, pmxmn, ipmxmn, iounit, ierror)

    !     routine: setp0
    !     purpose: 
    !     arguments: p0      array containing new set values for p vector
    !                ip0     array of flags for variables that have changes in p0
    !                ierror  error flag

    use precision_parameters
    use cfast_main
    use cshell
    use iofiles
    use opt
    implicit none

    integer, intent(in) :: iounit
    real(eb), intent(out) :: pmxmn(maxteq,2)
    
    real(eb), intent(out) :: p0(*)
    integer, intent(out) :: ipmxmn(0:maxteq,2), ip0(0:*), ierror
    
    real(eb) :: local(2), x

    integer :: ilocal(2), i, io, nret, iroom
    character :: label*5, testfile*128, place*1, mxmn*1, toupper*1, testpath*256
    logical exists, doesthefileexist, eof

    ip0(0) = off
    ierror = 0
    do i = 1, maxteq
        p0(i) = 0.0_eb
        ip0(i) = off
    end do

    if (setpfile=='   ') return

    ! first we try the local directory
    testpath = trim (datapath) // trim(setpfile)
    exists = doesthefileexist(testpath)

    ! then we try the executable root directory
    if (.not.exists) then
        testpath = trim (exepath) // trim(setpfile)
        exists = doesthefileexist(testpath)

        ! all is lost
        if (.not.exists) then
            write (logerr, 5000) trim(setpfile)
            ierror = 217
            return
        endif
    endif

    ! all is not lost
    write (logerr, 5001) testpath

    close (iounit)
    open (unit=iounit,file=testpath,form='formatted')

    call readbf(io, label, eof)
    do i = 1, 5
        label(i:i) = toupper(label(i:i))
    end do
    if (label(1:4)/='file') then
        ierror = 75
        close(io)
        return
    endif
    call readfl(testfile)
    if (testfile/=nnfile) then
        ierror = 50
        close(io)
        return
    endif

20  continue

    call readbf(io, label, eof)
    do i = 1, 5
        label(i:i) = toupper(label(i:i))
    end do

    call readin(2,nret,ilocal,local)
    iroom = ilocal(1)
    x = local(2)
    call readfl(place)
    place = toupper(place)
    call readfl(mxmn)
    mxmn = toupper(mxmn)

    if (label(1:4)=='TEMP') then
        if (PLACE=='U') then
            call dop0(noftu,iroom,mxmn,x,p0,ip0,pmxmn,ipmxmn)
        else if (place=='L') then
            call dop0(noftl,iroom,mxmn,x,p0,ip0,pmxmn,ipmxmn)
        else
            write(logerr,*) '***Error: parameter not supported by setp'
            ierror = 77
            close(io)
            return
        endif
    else if (labeL=='PRESS') then
        mxmn = place
        call dop0(nofp,iroom,mxmn,x,p0,ip0,pmxmn,ipmxmn)
    else if (label=='INTER') then
        mxmn = place
        x = (hr(iroom) - x)*ar(iroom)
        call dop0(nofvu,iroom,mxmn,x,p0,ip0,pmxmn,ipmxmn)
    else
        close(io)
        return
    endif

    goto 20

5000 format ('***Error: Cannot find the object fire file in either the executable path or the local directory ',/,a)
5001 format ('Open the SETPARAMETER file ',a)

    end subroutine setp0

! --------------------------- dop0 -------------------------------------------

    subroutine dop0(noflg, iroom, mxmn, x, p0, ip0, pmxmn, ipmxmn)

    !     routine: dop0
    !     purpose: 
    !     arguments: noflg   index into the p vector
    !                iroom   index of room
    !                mxmn    character flag of max or min of value
    !                p0      array containing new set values for p vector
    !                ip0     array of flags for variables that have changes in p0
    !                pmxmn   array containing new limits of values for p vector
    !                ipmxmn  array of flags for limits that have been set in pmxmn

    use precision_parameters
    use opt
    implicit none

    integer, intent(in) :: noflg, iroom
    real(eb), intent(in) ::  x
    character, intent(in) :: mxmn*1

    integer, intent(out) :: ipmxmn(0:maxteq,2), ip0(0:*)
    real(eb), intent(out) :: pmxmn(maxteq,2), p0(*)

    if (mxmn=='X') then
        ipmxmn(0,1) = on
        pmxmn(noflg+iroom,1) = x
        ipmxmn(noflg+iroom,1) = on
    else if (mxmn=='M') then
        ipmxmn(0,2) = on
        pmxmn(noflg+iroom,2) = x
        ipmxmn(noflg+iroom,2) = on
    else
        ip0(0) = on
        p0(noflg+iroom) = x
        ip0(noflg+iroom) = on
    endif

    return
    end subroutine dop0

! --------------------------- positionobject -------------------------------------------

    subroutine positionobject (xyz,index,opoint,rpoint,criterion,defaultposition,minimumseparation,errorcode)

    !     routine: positionobject
    !     purpose: Position an object in a compartment
    !     arguments: xyz: objposition (objpos)
    !                index: 1, 2 or 3 for x, y or z
    !		         opoint: the object pointer
    !		         rpoint: the compartment
    !		         criterion: the maximum extent
    !		         defaultposition: to set to zero (base)(2) or midpoint(1)
    !		         minimumseparation: the closest the object can be to a wall

    use precision_parameters
    implicit none
    
    integer, intent(in) :: index, defaultposition, opoint,rpoint
    real(eb), intent(in) :: minimumseparation, criterion(*)
    real(eb), intent(inout) :: xyz(3,0:*)
    integer, intent(out) :: errorcode
    
    if ((xyz(index,opoint)<0.0_eb).or.(xyz(index,opoint)>criterion(rpoint))) then
        select case (defaultposition)
        case (1) 
            xyz(index,opoint) = criterion(rpoint)/2.0_eb
        case (2) 
            xyz(index,opoint) = minimumseparation
        case default
            errorcode = 222
        end select
    else if (xyz(index,opoint)==0.0_eb) then
        xyz(index,opoint) = minimumseparation
    else if (xyz(index,opoint)==criterion(rpoint)) then
        xyz(index,opoint) = criterion(rpoint)-minimumseparation
    endif

    return

    end subroutine positionobject

! --------------------------- set_target_object -------------------------------------------

    subroutine set_target_object (itarg,iobj)

    !     routine: setobjtrg
    !     purpose: takes information from objects and sets a target for each.
    !     arguments: itarg
    !                iobj
    !                ierror  returns error codes

    use cfast_main
    use fltarget
    use objects2
    implicit none
    
    integer, intent(in) :: itarg, iobj

    integer :: i

    ixtarg(trgroom,itarg) = objrm(iobj)
    do i = 0,2
        xxtarg(trgcenx+i,itarg) = objpos(1+i,iobj)
        xxtarg(trgnormx+i,itarg) = objort(1+i,iobj)
    end do
    ixtarg(trgwall,itarg) = 0
    ixtarg(trgmeth,itarg) = mplicit
    ixtarg(trgeq,itarg) = ode
    return
    end subroutine set_target_object

! --------------------------- readcsvformat -------------------------------------------

    subroutine readcsvformat (iunit,x,c,numr,numc,nstart,maxr,maxc,ierror)

    !     routine: readcsvformat
    !     purpose: reads a comma-delimited file as generated by Micorsoft Excel, assuming that all the data is in the form of real numbers
    !     arguments: iunit  = logical unit, already open to .csv file
    !                x      = array of dimension (numr,numc) for values in spreadsheet
    !                c      = character array of same dimenaion as x for character values in spreadsheet
    !                numr   = # of rows of array x
    !                numc   = # of columns of array x
    !                nstart = starting row of spreadsheet to read
    !                maxr     = actual number of rows read
    !                maxcc    = actual number of columns read

    use precision_parameters
    use cshell
    implicit none

    integer, intent(in) :: iunit, numr, numc, nstart

    integer, intent(out) :: maxr, maxc, ierror
    real(eb), intent(out) :: x(numr,numc)
    character, intent(out) :: c(numr,numc)*(*)

    character :: in*10000, token*128
    integer :: i, j, nrcurrent, ic, icomma, ios, nc

    maxr = 0
    maxc = 0
    ierror = 0
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
    endif

    ! read the data
    nrcurrent=0
20  read (iunit,'(A)',end=100) in

    ! Skip comments
    if (in(1:1)=='!'.or.in(1:1)=='#') then
        go to 20
    endif

    nrcurrent=nrcurrent+1
    maxr=max(maxr,nrcurrent)

    ! Cannot exceed work array
    if(maxr>numr) then
        ierror = 207
        return
    endif

    nc=0
    ic=1
30  icomma=index(in,',')
    if (icomma/=0) then
        if (icomma==ic) then
            token=' '
        else
            token=in(ic:icomma-1)
        endif
        ic = icomma+1
        nc = nc + 1
        in(1:ic-1)=' '
        if (nrcurrent<=numr.and.nc<=numc) then
            c(nrcurrent,nc) = token
            read (token,'(f128.0)',iostat=ios) x(nrcurrent,nc)
            if (ios/=0) x(nrcurrent,nc) = 0
        else
            write (logerr,*) 'Array exceeded (readcsv), r,c=',nrcurrent,nc
            ierror = 207
            return
        endif
        go to 30
    endif
    nc = nc + 1
    maxc=max(maxc,nc)
    token = in(ic:ic+100)
    c(nrcurrent,nc) = token
    read (token,'(f128.0)',iostat=ios) x(nrcurrent,nc)
    if (ios/=0) x(nrcurrent,nc) = 0
    go to 20

100 continue

    return
    end subroutine readcsvformat

! --------------------------- rev_input -------------------------------------------

    integer function rev_input ()

    implicit none
    
    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_input = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_input