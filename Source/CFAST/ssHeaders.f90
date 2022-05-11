module spreadsheet_header_routines

    use precision_parameters
    
    use utility_routines, only: tointstring

    use cfast_types, only: target_type, detector_type, vent_type
    
    use cparams, only: u, l, soot, soot_flaming, soot_smolder, mxrooms, mxfires, mxtarg, mxdtect, mxfires, mxhvents, &
        mxvvents, mxmvents, mxext, mxleaks, mxfslab, ns, smoked
    
    use devc_data, only: n_detectors, detectorinfo, n_targets, targetinfo
    use diag_data, only: ioresid, ioslab
    use fire_data, only: n_fires, fireinfo, fire_type
    use room_data, only: n_rooms, roominfo, room_type
    use setup_data, only: validation_flag, iofilsmvzone, iofilsmv, iofilssdiag
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo

    implicit none

    private

    public ssheaders_smv, ssHeaders_resid, ssHeaders_fslabs, ssheaders_diagnosis

    contains

! --------------------------- ssheaders_smv -------------------------------------------

    subroutine ssheaders_smv(lMode)

    ! header information for the smokeview spreadsheet output

    logical, intent(in) :: lmode

    integer, parameter :: maxhead = 1+8*mxrooms+4*mxfires+2*mxhvents+3*mxfslab*mxhvents+2*mxvvents+2*mxext
    character(len=35) :: headertext(2,maxhead), cRoom, cFire, cVent, cSlab, cTarg, LabelsShort(36), LabelUnits(36)
    integer position, i, j, iv
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: ventptr

    data LabelsShort / 'Time', 'ULT_', 'LLT_', 'HGT_', 'PRS_', 'RHOU_', 'RHOL_', 'ULOD_', 'LLOD_', &
        'CLT_', 'UWT_', 'LWT_', 'FLT_', &
        'HRR_', 'FLHGT_', 'FBASE_', 'FAREA_', &
        'HVENT_','HSLAB_','HSLABT_','HSLABF_','HSLABYB_','HSLABYT_', &
        'VVENT_','VSLAB_','VSLABT_','VSLABF_','VSLABYB_','VSLABYT_', &
        'MVENT_','MSLAB_','MSLABT_','MSLABF_','MSLABYB_','MSLABYT_', 'TARGET_'  /
    data LabelUnits / 's', 'C', 'C', 'm', 'Pa', 'kg/m^3', 'kg/m^3', '1/m', '1/m', &
        'C', 'C', 'C', 'C', &
        'kW', 'm', 'm', 'm^2', &
        'm^2', ' ', 'K', 'kg/s', 'm', 'm', &
        'm^2', ' ', 'K', 'kg/s', 'm', 'm', &
        'm^2', ' ', 'K', 'kg/s', 'm', 'm', 'C' /

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelUnits(1)
    headertext(2,1) = LabelsShort(1)
    call smvDeviceTag('TIME')
    position = 1

    ! Compartment variables
    do j = 1, n_rooms
        roomptr => roominfo(j)
        do i = 1, 12
            if (i==1.or.i==4.or.i==5.or.i==7.or.i>8.or..not.roomptr%shaft) then
                position = position + 1
                call toIntString(j,cRoom)
                headertext(1,position) = LabelUnits(i+1)
                headertext(2,position) = trim(LabelsShort(i+1)) // trim(cRoom)
                call smvDeviceTag(headertext(2,position))
            end if
        end do
    end do

    ! Fire variables.
    do j = 1, n_fires
        do i = 1, 4
            position = position + 1
            call toIntString(j,cFire)
            headertext(1,position) = LabelUnits(i+13)
            headertext(2,position) = trim(LabelsShort(i+13))//trim(cFire)
            call smvDeviceTag(headertext(2,position))
        end do
    end do

    ! Wall vent variables
    do j = 1, n_hvents
        position = position + 1
        call toIntString(j,cVent)
        headertext(1,position) = LabelUnits(18) ! Vent area
        headertext(2,position) = trim(LabelsShort(18))//trim(cVent)
        call smvDeviceTag(headertext(2,position))
        position = position + 1
        headertext(1,position) = LabelUnits(19) ! number of slabs
        headertext(2,position) = trim(LabelsShort(19))//trim(cVent)
        call smvDeviceTag(headertext(2,position))
        do i = 1,mxfslab
            call toIntString(i,cSlab)
            position = position + 1
            headertext(1,position) = LabelUnits(20) ! slab temperature
            headertext(2,position) = trim(LabelsShort(20))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(21) ! slab flow
            headertext(2,position) = trim(LabelsShort(21))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(22) ! slab bottom
            headertext(2,position) = trim(LabelsShort(22))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(23) ! slab top
            headertext(2,position) = trim(LabelsShort(23))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
        end do
    end do

    ! Ceiling / floor vent variables
    do j = 1, n_vvents
        position = position + 1
        call toIntString(j,cVent)
        headertext(1,position) = LabelUnits(24)
        headertext(2,position) = trim(LabelsShort(24))//trim(cVent)
        call smvDeviceTag(headertext(2,position))
        position = position + 1
        headertext(1,position) = LabelUnits(25) ! number of slabs
        headertext(2,position) = trim(LabelsShort(25))//trim(cVent)
        call smvDeviceTag(headertext(2,position))
        do i = 1,2
            call toIntString(i,cSlab)
            position = position + 1
            headertext(1,position) = LabelUnits(26) ! slab temperature
            headertext(2,position) = trim(LabelsShort(26))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(27) ! slab flow
            headertext(2,position) = trim(LabelsShort(27))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(28) ! slab bottom
            headertext(2,position) = trim(LabelsShort(28))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(29) ! slab top
            headertext(2,position) = trim(LabelsShort(29))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
        end do
    end do

    ! Mechanical vent variables
    iv = 0
    do j = 1, n_mvents
        ventptr => mventinfo(j)
        iv = iv + 1
        position = position + 1
        call toIntString(iv,cVent)
        headertext(1,position) = LabelUnits(30)
        headertext(2,position) = trim(LabelsShort(30))//trim(cVent)
        call smvDeviceTag(headertext(2,position))
        position = position + 1
        headertext(1,position) = LabelUnits(31) ! number of slabs
        headertext(2,position) = trim(LabelsShort(31))//trim(cVent)
        call smvDeviceTag(headertext(2,position))
        do i = 1,2
            call toIntString(i,cSlab)
            position = position + 1
            headertext(1,position) = LabelUnits(32) ! slab temperature
            headertext(2,position) = trim(LabelsShort(32))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(33) ! slab flow
            headertext(2,position) = trim(LabelsShort(33))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(34) ! slab bottom
            headertext(2,position) = trim(LabelsShort(34))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
            position = position + 1
            headertext(1,position) = LabelUnits(35) ! slab top
            headertext(2,position) = trim(LabelsShort(35))//trim(cVent)//'_'//trim(cSlab)
            call smvDeviceTag(headertext(2,position))
        end do
    end do
    
    ! target information
    do i = 1, n_targets
        call toIntString(i,cTarg)
        position = position + 1
        headertext(1,position) = LabelUnits(36) ! target temperature
        headertext(2,position) = trim(LabelsShort(36))//trim(cTarg)
        call smvDeviceTag(headertext(2,position))
    end do


    ! write out header if called from outputspreadsheet
    ! (this is only one once, but smokeview device tags are done each time)
    if (lMode) then
        write (iofilsmvzone,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
        write (iofilsmvzone,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    end if

    end subroutine ssheaders_smv

! --------------------------- smvDeviceTag -------------------------------------------

    subroutine smvDeviceTag(string)

    character(len=*), intent(in) :: string

    write (iofilsmv,'(a)') 'DEVICE'
    write (iofilsmv,'(4x,a)') trim(string)
    write (iofilsmv,'(1x,3f6.1)') 0.,0.,0.
    return
    end subroutine smvDeviceTag

! --------------------------- ssHeaders_resid -------------------------------------------

 subroutine ssHeaders_resid

    ! header information for the calculate_residuals spreadsheet output

    integer, parameter :: maxhead = 1+2*(8*(ns+2)+3)*mxrooms + 4*mxrooms + 8*mxrooms
    character(len=35) :: headertext(3,maxhead), Labels(23), LabelUnits(8), Layers(2), Species(9)
    integer position, i, j, k, l, tmp
    type(room_type), pointer :: roomptr

    data Labels / 'Time','Delta P', 'Vol Upper', 'Temp UP', 'Temp Low', &
        'Ceiling Temp (Interior)', 'Ceiling Temp (Exterior)', 'Upper Wall Temp (Interior)', &
        'Upper Wall Temp (Exterior)', 'Lower Wall (Interior)', 'Lower Wall (Exterior)', &
        'Floor Temp (Interior)', 'Floor Temp (Exterior)', 'Total Flow', 'Natural Vent Flow', 'Fire Flow', &
        'Vertical Flow', 'Mechanical Flow', 'Filtered Mass', 'vent jet Fire Flow', 'Mixing Between Layers', &
        'Convective Flow', 'Radiative Flow'/
    data LabelUnits / 'sec', 'Pa', 'm^3', 'K', 'K', 'kg/s','w', 'kg/s' /
    data Layers /'upper', 'lower'/
    data Species /'N2','O2','CO2','CO','HCN','HCL','FUEL','H2O','Soot'/

    !  spreadsheet header.  Add time first
    headertext(1,1) = Labels(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelUnits(1)
    tmp = maxhead

    position = 1

    ! Compartment variables
    do j = 1, n_rooms
        roomptr => roominfo(j)
        position = position + 1
        headertext(1,position) = trim(Labels(2))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(2)
        position = position + 1
        headertext(1,position) = trim(Labels(3))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(3)
        position = position + 1
        headertext(1,position) = trim(Labels(4))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(4)
        position = position + 1
        headertext(1,position) = trim(Labels(5))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(6))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(7))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(8))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(9))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(10))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(11))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(12))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        position = position + 1
        headertext(1,position) = trim(Labels(13))
        headertext(2,position) = roomptr%id
        headertext(3,position) = LabelUnits(5)
        do i = 1, 2
            do k = 1, 2
                do l = 2, 9
                    position = position + 1
                    headertext(1,position) = trim(Labels(l+12))//trim(Layers(i))
                    headertext(2,position) = roomptr%id
                    headertext(3,position) = LabelUnits(k+5)
                end do
            end do
            position = position + 1
            headertext(1,position) = trim(Labels(22))//trim(Layers(i))
            headertext(2,position) = roomptr%id
            headertext(3,position) = LabelUnits(7)
            position = position + 1
            headertext(1,position) = trim(Labels(23))//trim(Layers(i))
            headertext(2,position) = roomptr%id
            headertext(3,position) = LabelUnits(7)
        end do
    end do

    ! Species
    do j = 1, n_rooms
        roomptr => roominfo(j)
        do i = 1, 2
            do k = 1, 9
                position = position + 1
                headertext(1,position) = trim(Species(k))//trim(Layers(i))
                headertext(2,position) = roomptr%id
                headertext(3,position) = LabelUnits(8)
            end do
        end do
    end do

    ! write out header
    write (ioresid,"(44664a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (ioresid,"(44664a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (ioresid,"(44664a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))

 end subroutine ssHeaders_resid

! --------------------------- ssHeaders_fslabs -------------------------------------------

  subroutine ssHeaders_fslabs

    ! header information for the flow slabs spreadsheet output

    integer, parameter :: maxhead = 1 + mxhvents*(4 + mxfslab)
    character(len=35) :: headertext(3,maxhead), Labels(6), LabelUnits(2)
    integer :: position, i, j

    data Labels / 'time', 'Room 1','Room 2', 'Vent Num', 'Num Slabs', 'Slab'/
    data LabelUnits / 'sec', 'w'/

    !  spreadsheet header.  Add time first
    headertext(1,1) = Labels(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelUnits(1)

    position = 1

    do i = 1, n_hvents
        do j = 1, 4
            position = position + 1
            headertext(1,position) = trim(Labels(j+1))
            headertext(2,position) = ' '
            headertext(3,position) = ' '
        end do
        do j = 1, mxfslab
            position = position + 1
            headertext(1,position) = trim(Labels(6))
            call toIntString(j,headertext(2,position))
            headertext(3,position) = trim(LabelUnits(2))
        end do
    end do

    ! write out header
    write (ioslab,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (ioslab,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (ioslab,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))

    end subroutine ssHeaders_fslabs

! --------------------------- ssheaders_diagnosis -------------------------------------------

    subroutine ssheaders_diagnosis

    ! header information for the diagnostic spreadsheet output

    integer, parameter :: maxhead = 1+10*mxrooms
    character(len=35) :: headertext(4,maxhead), cRoom, Labels(11), LabelsShort(11), LabelUnits(11)
    integer :: position, i, j
    type(room_type), pointer :: roomptr

    data Labels / 'Time','Ceiling Opening Fraction', 'Upper Front Opening Fraction', 'Upper Right Opening Fraction', &
    'Upper Back Opening Fraction', 'Upper Left Opening Fraction', 'Lower Front Opening Fraction', 'Lower Right Opening Fraction', &
    'Lower Back Opening Fraction', 'Lower Left Opening Fraction', 'Floor Opening Fraction' /
    data LabelsShort / 'Time', 'COF_', 'UFOF_', 'UROF_', 'UBOF_', 'ULOF_', 'LFOF_', 'LROF_', 'LBOF_', 'LLOF_', &
    'FOF_' /
    data LabelUnits / 's', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' /

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelsShort(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = Labels(1)
    headertext(4,1) = LabelUnits(1)

    position = 1

    ! Compartment variables
    do j = 1, n_rooms
        roomptr => roominfo(j)
        do i = 1, 10
            position = position + 1
            call toIntString(j,cRoom)
            headertext(1,position) = trim(LabelsShort(i+1)) // trim(cRoom)
            headertext(2,position) = Labels(i+1)
            headertext(3,position) = roomptr%id
            headertext(4,position) = LabelUnits(i+1)
        end do
    end do

    ! write out header
    write (iofilssdiag,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (iofilssdiag,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (iofilssdiag,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))
    write (iofilssdiag,"(16384a)") (trim(headertext(4,i)) // ',',i=1,position-1),trim(headertext(4,position))

    end subroutine ssheaders_diagnosis

end module spreadsheet_header_routines
