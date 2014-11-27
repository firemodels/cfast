
! --------------------------- ssHeadersNormal -------------------------------------------

    subroutine ssHeadersNormal

    ! This is the header information for the normal spreadsheet output

    use cenviro
    use cfast_main
    use cshell
    use objects1
    implicit none

    ! local variables     
    integer, parameter :: maxhead = 1+8*nr+5+9*mxfire
    character(35) :: headertext(3,maxhead), cTemp, cRoom, cFire, Labels(18), LabelsShort(18), LabelUnits(18)
    integer :: position, i, j

    data Labels / 'Time','Upper Layer Temperature', 'Lower Layer Temperature', 'Layer Height', 'Upper Layer Volume', 'Pressure', 'Ambient Temp Target Flux', 'Floor Temp Target Flux', &
    'HRR Door Jet Fires', 'Plume Entrainment Rate', 'Pyrolysis Rate', 'HRR', 'HRR Lower', 'HRR Upper','Flame Height', 'Convective HRR', 'Total Pyrolysate Released', 'Total Trace Species Released' /
    data LabelsShort / 'Time', 'ULT_', 'LLT_', 'HGT_', 'VOL_', 'PRS_', 'ATARG_', 'FTARG_', 'DJET_', 'PLUM_', 'PYROL_', 'HRR_', 'HRRL_', 'HRRU_', 'FLHGT_', 'HRR_C_', 'PYROL_T_', 'TRACE_T_' /
    data LabelUnits / 's', 'C', 'C', 'm', 'm^3', 'Pa', 'W/m^2', 'W/m^2', 'W', 'kg/s', 'kg/s', 'W', 'W', 'W', 'm', 'W', 'kg', 'kg' /

    !  spreadsheet header.  Add time first
    if (validate) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
    else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
    endif
    position = 1

    ! Compartment variables
    do j = 1, nm1
        do i = 1, 7
            if (i/=2.or.izshaft(j)==0) then
                if (i/=3.or.izshaft(j)==0) then
                    position = position + 1
                    if (validate) then
                        call toIntString(j,cRoom)
                        headertext(1,position) = trim(LabelsShort(i+1)) // trim(cRoom)
                        headertext(2,position) = LabelUnits(i+1)
                        headertext(3,position) = ' '
                    else
                        headertext(1,position) = Labels(i+1)
                        headertext(2,position) = compartmentnames(j)
                        headertext(3,position) = LabelUnits(i+1)
                    endif
                endif
            endif
        end do
    end do

    ! Door jet fires
    do i = 1, n
        position = position + 1
        if (validate) then
            call toIntString(i,cRoom)
            if (i==n) then
                headertext(1,position) = trim(LabelsShort(9)) // 'Out'
            else
                headertext(1,position) = trim(LabelsShort(9)) // trim(cRoom)
            endif
            headertext(2,position) = LabelUnits(9)
            headertext(3,position) = ' '
        else
            headertext(1,position) = Labels(9)
            if (i==n) then
                headertext(2,position) = 'Outside'
            else
                headertext(2,position) = compartmentnames(i)
            end if
            headertext(3,position) = LabelUnits(9)
        endif
    end do

    ! Fire variables. Main fire first, then object fires
    if (lfbo>0) then
        do i = 1, 9
            position = position + 1
            if (validate) then
                write (cTemp,'(a,i1)') trim(LabelsShort(i+9)), 0
                headertext(1,position) = cTemp
                headertext(2,position) = LabelUnits(i+9)
                headertext(3,1) = ' '
            else
                headertext(1,position) = Labels(i+9)
                headertext(2,position) = 'Mainfire'
                headertext(3,position) = LabelUnits(i+9)
            endif  
        end do
    endif
    do j = 1, numobjl
        do i = 1, 9
            position = position + 1
            if (validate) then
                call toIntString(j,cFire)
                headertext(1,position) = trim(LabelsShort(i+9))//trim(cFire)
                headertext(2,position) = LabelUnits(i+9)
                headertext(3,1) = ' '
            else
                headertext(1,position) = Labels(i+9)
                headertext(2,position) = objnin(j)
                headertext(3,position) = LabelUnits(i+9)
            endif
        end do
    end do

    ! write out header
    write(21,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
    write(21,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
    if (.not.validate) write(21,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)

    end subroutine ssHeadersNormal

! --------------------------- ssHeadersSpeccies -------------------------------------------

    subroutine ssHeadersSpecies

    ! This is the header information for the spreadsheet output

    use cenviro
    use cfast_main
    use cshell
    implicit none

    ! local variables     
    integer, parameter :: maxhead = 1+7*nr+5+7*mxfire
    character(35) :: headertext(3,maxhead), cRoom, Labels(23), LabelsShort(23), LabelUnits(23)
    logical tooutput(ns), molfrac(ns)
    data tooutput /.false.,5*.true.,.false.,4*.true./ 
    data molfrac /3*.true.,3*.false.,2*.true.,3*.false./
    integer position, i, j, lsp

    data Labels / 'Time', 'N2 Upper Layer', 'O2 Upper Layer', 'CO2 Upper Layer', 'CO Upper Layer', 'HCN Upper Layer', 'HCL Upper Layer', 'Unburned Hydrocarbons Upper Layer', 'H2O Upper Layer', 'Optical Density Upper Layer', 'C-T Product Upper Layer', 'Trace Species Upper Layer',&
    'N2 Lower Layer', 'O2 Lower Layer', 'CO2 Lower Layer', 'CO Lower Layer', 'HCN Lower Layer', 'HCL Lower Layer', 'Unburned Hydrocarbons Lower Layer', 'H2O Lower Layer', 'Optical Density Lower Layer', 'C-T Product Lower Layer', 'Trace Species Lower Layer' / 
    data LabelsShort / 'Time', 'ULN2', 'ULO2_', 'ULCO2_', 'ULCO_', 'ULHCN_', 'ULHCL_', 'ULTUHC_', 'ULH2O_', 'ULOD_', 'ULCT_', 'ULTS_', 'LLN2', 'LLO2_', 'LLCO2_', 'LLCO_', 'LLHCN_', 'LLHCL_', 'LLTUHC_', 'LLH2O_', 'LLOD_', 'LLCT_', 'LLTS_'/
    data LabelUnits / 's', 'mol %', 'mol %', 'mol %', 'PPM', 'PPM', 'PPM', 'mol %', 'mol %', '1/m', 'g-min/m^3', 'kg', 'mol %', 'mol %', 'mol %', 'PPM', 'PPM', 'PPM', 'mol %', 'mol %', '1/m', 'g-min/m^3', 'kg' /

    !  spreadsheet header.  Add time first
    if (validate) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
    else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
    endif
    position = 1

    ! Species by compartment, then layer, then species type
    do i = 1, nm1
        do j = upper, lower
            if (j==upper.or.izshaft(i)==0) then
                do lsp = 1, NS
                    if(tooutput(lsp)) then
                        position = position + 1
                        if (validate) then
                            call toIntString(i,cRoom)
                            headertext(1,position) = trim(LabelsShort((j-1)*11+lsp+1)) // trim(cRoom)
                            headertext(2,position) = LabelUnits((j-1)*11+lsp+1)
                            if (molfrac(lsp)) headertext(2,position) = 'mol frac'
                            if (lsp==9) headertext(2,position) = 'mg/m^3'
                            headertext(3,1) = ' '
                        else
                            headertext(1,position) = Labels((j-1)*11+lsp+1)
                            headertext(2,position) = compartmentnames(i)
                            headertext(3,position) = LabelUnits((j-1)*11+lsp+1)
                        endif
                    endif
                end do
            endif
        end do
    end do

    ! write out header
    write(23,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
    write(23,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
    if (.not.validate) write(23,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)

    end subroutine ssHeadersSpecies

! --------------------------- ssHeadersFlux -------------------------------------------

    subroutine ssHeadersFlux

    ! This routine spools the headers for the surface temperature and flux results.

    ! Format

    !blank     c1     c1      c1    c1      c1   c1    c1      c1   c1         c2     c2      c2    c2       c2   c2   c2     c2   c2       ....
    !time   ceiling	u-wall  l-wall floor  flux  fire surface gas convect   ceiling u-wall  l-wall floor  flux  fire surface gas convect    ....


    !.....  target number
    !.....  gas temp, surface temp, center temp, flux, fire, surface, gas, convect


    !.....  sensor number
    !.....  compartment name, type, sensor temperature, activated, smoke temperature, smoke velocity


    use cfast_main
    use cshell
    use fltarget
    implicit none

    ! local variables     
    integer, parameter :: maxhead = 1+9*nr+8*mxtarg+4*mxdtect
    character(35) :: headertext(3,maxhead), cTemp, cType, cDet, cRoom, Labels(18), LabelsShort(18), LabelUnits(18)
    integer position, i, j, itarg, itype

    data Labels / 'Time', 'Ceiling Temperature', 'Upper Wall Temperature', 'Lower Wall Temperature', 'Floor Temperature', 'Target Surrounding Gas Temperature', 'Target Surface Temperature', 'Target Center Temperature', 'Target Total Flux', 'Target Convective Flux', &
    'Target Radiative Flux', 'Target Fire Radiative Flux', 'Target Surface Radiative Flux', 'Target Gas Radiative Flux', 'Sensor Temperature', 'Sensor Activation', 'Sensor Surrounding Gas Temperature', 'Sensor Surrounding Gas Velocity' /

    data LabelsShort /'Time', 'CEILT_', 'UWALLT_', 'LWALLT_', 'FLOORT_', 'TARGGAST_', 'TARGSURT_', 'TARGCENT_', 'TARGFLUXT_', 'TARGFLUXC_', 'TARGFLUXR_','TARGFLUXF_', 'TARGFLUXS_', 'TARGFLUXG_',  'SENST_', 'SENSACT_', 'SENSGAST_', 'SENSGASVEL_' /

    data LabelUnits / 's', 7*'C', 6*'W/m^2', 'C', '1=yes', 'C', 'm/s' /

    !  spreadsheet header.  Add time first
    if (validate) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
    else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
    endif
    position = 1

    ! Compartment surfaces temperatures
    do i = 1, nm1
        do j = 1, 4
            position = position + 1
            if (validate) then
                call toIntString(i,cRoom)
                headertext(1,position) = trim(LabelsShort(j+1))//trim(cRoom)
                headertext(2,position) = LabelUnits(j+1)
                headertext(3,position) = ' '
            else
                headertext(1,position) = Labels(j+1)
                headertext(2,position) = compartmentnames(i)
                headertext(3,position) = LabelUnits(j+1)
            endif
        end do
    end do

    !	All the additional targets
    do i = 1, nm1
        if (ntarg>nm1) then
            do itarg = 1, ntarg-nm1
                if (ixtarg(trgroom,itarg)==i) then
                    call toIntString(itarg,cDet)
                    do j = 1, 9
                        position = position + 1
                        if (validate) then
                            headertext(1,position) = trim(LabelsShort(j+5)) // trim(cDet)
                            if (LabelUnits(j+5)=='W') LabelUnits(j+5) = 'kW'
                            headertext(2,position) = LabelUnits(j+5)
                            headertext(3,position) = ' '
                        else
                            headertext(1,position) = Labels(j+5)
                            headertext(2,position) = 'Target ' // trim(cDet)
                            headertext(3,position) = LabelUnits(j+5)
                        endif
                    end do  
                endif
            end do
        endif
    end do

    !	Hall flow needs to go here

    !	Detectors
    do i = 1, ndtect
        call toIntString(i,cDet)
        itype = ixdtect(i,dtype)
        if (itype==smoked) then
            cType = 'Smoke'
        elseif (itype==heatd) then
            cType = 'Heat'
        else
            cType = 'Other'
        endif
        do j = 1, 4
            position = position + 1
            if (validate) then
                headertext(1,position) = trim(LabelsShort(j+14))//trim(cDet)
                headertext(2,position) = LabelUnits(j+14)
                headertext(3,position) = ' '
            else
                headertext(1,position) = Labels(j+14)
                write (cTemp,'(a,1x,a,1x,a)') trim(cType),'Sensor',trim(cDet)
                headertext(2,position) = cTemp
                headertext(3,position) = LabelUnits(j+14)
            endif
        end do
    end do

    ! write out header
    write(24,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
    write(24,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
    if (.not.validate) write(24,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)

    return
    end subroutine ssHeadersFlux

! --------------------------- ssHeadersFlow -------------------------------------------

    subroutine ssHeadersFlow

    !	This is the header information for the flow spreadsheet and is called once
    !	The logic is identical to SpreadSheetFlow so the output should be parallel

    use cfast_main
    use cshell
    use vents
    implicit none

    ! local variables
    integer, parameter :: maxhead = mxvents+2*mxvv+2*mxhvsys+mfan
    character(35) :: headertext(3,maxhead), cTemp, ciFrom, ciTo, cVent, Labels(7), LabelsShort(7), LabelUnits(7)
    integer :: position, i, ih, ii, inode, ifrom, ito, toprm = 1, botrm = 2
    type(vent_type), pointer :: ventptr

    data Labels / 'Time', 'HVENT Net Inflow', 'HVENT Net Mixing to Upper Layer', 'VVENT Net Inflow', 'MVENT Net Inflow', 'MVENT Trace Species Flow', 'MVENT Trace Species Filtered' /

    data LabelsShort /'Time', 'H_', 'H_MIX_', 'V_', 'MV_', 'MV_TRACE_', 'MV_FILTERED_' /

    data LabelUnits / 's', 4*'kg/s', 2*'kg' /

    !  spreadsheet header.  Add time first
    if (validate) then
        headertext(1,1) = LabelsShort(1)
        headertext(2,1) = LabelUnits(1)
        headertext(3,1) = ' '
    else
        headertext(1,1) = Labels(1)
        headertext(2,1) = ' '
        headertext(3,1) = LabelUnits(1)
    endif
    position = 1

    !	Do the output by compartments
    do i = 1, n_hvents
        ventptr=>ventinfo(i)

        ifrom = ventptr%from
        call tointstring(ifrom,cifrom)
        if (ifrom==n) cifrom = 'Outside'
        ito = ventptr%to
        call tointstring(ito,cito)
        if (ito==n) cito = 'Outside'

        position = position + 1
        call tointstring(ventptr%counter,cvent)
        if (validate) then
            write (ctemp,'(6a)') trim(labelsshort(2)),trim(cifrom),'>',trim(cito),'_#',trim(cvent)
            headertext(1,position) = ctemp
            headertext(2,position) = labelunits(2)
            headertext(3,position) = ' '
            position = position + 1
            write (ctemp,'(6a)') trim(labelsshort(2)),trim(cito),'>',trim(cifrom),'_#',trim(cvent)
            headertext(1,position) = ctemp
            headertext(2,position) = labelunits(2)
            headertext(3,position) = ' '
        else
            headertext(1,position) = labels(2)
            write (ctemp,'(a,1x,a,1x,3a)') 'Vent #',trim(cvent),trim(cifrom),'>',trim(cito)
            headertext(2,position) = ctemp
            headertext(3,position) = labelunits(2)
            position = position + 1
            headertext(1,position) = labels(2)
            write (ctemp,'(a,1x,a,1x,3a)') 'Vent #',trim(cvent),trim(cito),'>',trim(cifrom)
            headertext(2,position) = ctemp
            headertext(3,position) = labelunits(2)
        endif
    end do

    ! Natural flow through horizontal vents (vertical flow)
    do i = 1,n_vvents

        ifrom = ivvent(i,botrm)
        call tointstring(ifrom,ciFrom)
        if (ifrom==n) cifrom = 'Outside'
        ito = ivvent(i,toprm)
        call tointstring(ito,cito)
        if (ito==n) cito = 'Outside'
        position = position + 1
        
        if (validate) then
            write (ctemp,'(5a)') trim(labelsshort(4)),trim(cifrom),'>',trim(cito)
            headertext(1,position) = ctemp
            headertext(2,position) = labelunits(4)
            headertext(3,position) = ' '
            position = position + 1
            write (ctemp,'(5a)') trim(labelsshort(4)),trim(cito),'>',trim(cifrom)
            headertext(1,position) = ctemp
            headertext(2,position) = labelunits(4)
            headertext(3,position) = ' '
        else
            headertext(1,position) = labels(4)
            write (ctemp,'(a,1x,3a)') 'Vent',trim(cifrom),'>',trim(cito)
            headertext(2,position) = ctemp
            headertext(3,position) = labelunits(4)
            position = position + 1
            headertext(1,position) = labels(4)
            write (ctemp,'(a,1x,3a)') 'Vent',trim(cito),'>',trim(cifrom)
            headertext(2,position) = cTemp
            headertext(3,position) = LabelUnits(4)
        endif
        
    end do

    ! Mechanical ventilation
    if (nnode/=0.and.next/=0) then
        do i = 1, next
            ii = hvnode(1,i)
            inode = hvnode(2,i)
            call toIntString(ii,ciFrom)
            if (ii==n) cifrom = 'Outside'
            call toIntString(inode,ciTo)
            do ih = 1,3
                position = position + 1
                if (validate) then
                    if (ih==1) then
                        if (ciFrom=='Outside') then
                            headertext(1,position) = trim(LabelsShort(ih+4)) // trim(ciFrom) // '>N' // trim(ciTo)
                        else
                            headertext(1,position) = trim(LabelsShort(ih+4)) //'C' // trim(ciFrom) // '>N' // trim(ciTo)
                        end if
                    else
                        headertext(1,position) = trim(LabelsShort(ih+4)) // 'Fan_N' // trim(ciTo)
                    endif
                    headertext(2,position) = LabelUnits(ih+4)
                    headertext(3,position) = ' '
                else
                    headertext(1,position) = Labels(ih+4)
                    if (ih==1) then
                        headertext(2,position) = 'Vent ' // trim(ciFrom) // '> Node ' // trim(ciTo)
                    else
                        headertext(2,position) = 'Fan at Node ' // trim(ciTo)
                    endif
                    headertext(3,position) = LabelUnits(ih+4)
                endif
            end do
        end do
    endif

    ! write out header
    write(22,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
    write(22,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
    if (.not.validate) write(22,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)

    return

    end subroutine ssHeadersFlow

! --------------------------- ssHeadersSMV -------------------------------------------

    subroutine ssHeadersSMV(lMode)

    ! This is the header information for the smokeview spreadsheet output

    use cenviro
    use cfast_main
    use vents
    implicit none

    logical, intent(in) :: lmode

    integer, parameter :: maxhead = 1+6*nr+5+2*mxfire
    character(35) :: headertext(2,maxhead), cTemp, cRoom, cFire, cVent, LabelsShort(15), LabelUnits(15)
    integer position, i, j

    data LabelsShort / 'Time', 'ULT_', 'LLT_', 'HGT_', 'PRS_', 'ULOD_', 'LLOD_', 'HRR_', 'FLHGT_', 'FBASE_', 'FAREA_', 'HVENT_', 'VVENT_', 'VVENTIN_',' VVENT_OUT_' /
    data LabelUnits / 's', 'C', 'C', 'm', 'Pa', '1/m', '1/m', 'kW', 'm', 'm', 'm^2', 'm^2', 'm^2', 'kg/s', 'kg/s' /

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelUnits(1)
    headertext(2,1) = LabelsShort(1)
    call smvDeviceTag('TIME')
    position = 1

    ! Compartment variables
    do j = 1, nm1
        do i = 1, 6
            if (i==1.or.i==4.or.i==5.or.izshaft(j)==0) then
                position = position + 1
                call toIntString(j,cRoom)
                headertext(1,position) = LabelUnits(i+1)
                headertext(2,position) = trim(LabelsShort(i+1)) //trim(cRoom)
                call smvDeviceTag(headertext(2,position))

            endif
        end do
    end do

    ! Fire variables. Main fire first, then object fires
    if (lfbo>0) then
        do i = 1, 4
            position = position + 1
            write (cTemp,'(a,i1)') trim(LabelsShort(i+7)), 0
            headertext(1,position) = LabelUnits(i+7)
            headertext(2,position) = cTemp
            call smvDeviceTag(headertext(2,position))
        end do
    endif
    do j = 1, numobjl
        do i = 1, 4
            position = position + 1
            call toIntString(j,cFire)
            headertext(1,position) = LabelUnits(i+7)
            headertext(2,position) = trim(LabelsShort(i+7))//trim(cFire)
            call smvDeviceTag(headertext(2,position))
        end do
    end do

    ! Vent variables
    do i = 1, n_hvents
        position = position + 1
        call toIntString(i,cVent)
        headertext(1,position) = LabelUnits(12)
        headertext(2,position) = trim(LabelsShort(12))//trim(cVent)
        call smvDeviceTag(headertext(2,position))
    end do
    do i = 1, n_vvents
        do j = 1,3
            position = position + 1
            call toIntString(i,cVent)
            headertext(1,position) = LabelUnits(13+j-1)
            headertext(2,position) = trim(LabelsShort(13+j-1))//trim(cVent)
            call smvDeviceTag(headertext(2,position))
        end do
    end do

    ! write out header if called from outputspreadsheet 
    ! (this is only one once, but smokeview device tags are done each time)
    if(lMode) then
        write(15,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
        write(15,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
    endif

    end subroutine ssHeadersSMV

! --------------------------- smvDeviceTag -------------------------------------------

    subroutine smvDeviceTag(string)
    implicit none
    character, intent(in) :: string*(*)
    
    write (13,'(a)') 'DEVICE'
    write (13,'(4x,a)') trim(string)
    write (13,'(1x,3f6.1)') 0.,0.,0.
    return
    end subroutine smvDeviceTag

! --------------------------- toIntString -------------------------------------------

    subroutine toIntString(i,istring)
    implicit none
    
    integer, intent(in) :: i
    character(len=*), intent(out) :: istring
    
    character :: string*256
    
    if (i<10) then
        write (string,'(i1)') i
    else if (i<100) then
        write (string,'(i2)') i
    else if (i<1000) then
        write (string,'(i3)') i
    else if (i<10000) then
        write (string,'(i4)') i
    else if (i<100000) then
        write (string,'(i5)') i
    else if (i<1000000) then
        write (string,'(i6)') i
    else
        string = 'error'
    endif
    istring = trim(string)
    return
    end subroutine toIntString

! --------------------------- ssHeadersResid -------------------------------------------

 subroutine ssHeadersResid

    ! This is the header information for the normal spreadsheet output

    use cfast_main
    use objects1
    use debug
    implicit none

    ! local variables     
    integer, parameter :: maxhead = 1+2*(7*(ns+2)+3)*nr + 4*nr
    character(35) :: headertext(3,maxhead), Labels(15), LabelUnits(8), Layers(2)
    integer position, i, j, k, l, nprod

    data Labels / 'Time','Delta P', 'Vol Upper', 'Temp UP', 'Temp Low', 'Total Flow', 'Natural Vent Flow', 'Fire Flow', 'Vertical Flow', 'Mechanical Flow', 'Filtered Mass', 'Door Jet Fire Flow', &
    'Convecxtive Flow', 'Radiative Flow', 'Ceiling Jet Flow'/
    data LabelUnits / 'sec', 'Pa', 'm^3', 'C', 'C', 'kg/s','w', 'kg/s' /
    data Layers /'upper', 'lower'/

    !  spreadsheet header.  Add time first
    headertext(1,1) = Labels(1)
    headertext(2,1) = ' '
    headertext(3,1) = LabelUnits(1)
    nprod = nlspct
 
    position = 1

    ! Compartment variables
    do j = 1, nm1
        position = position + 1
        headertext(1,position) = trim(Labels(2))
        headertext(2,position) = compartmentnames(j)
        headertext(3,position) = LabelUnits(2)
        position = position + 1
        headertext(1,position) = trim(Labels(3))
        headertext(2,position) = compartmentnames(j)
        headertext(3,position) = LabelUnits(3)
        position = position + 1
        headertext(1,position) = trim(Labels(4))
        headertext(2,position) = compartmentnames(j)
        headertext(3,position) = LabelUnits(4)
        position = position + 1
        headertext(1,position) = trim(Labels(5))
        headertext(2,position) = compartmentnames(j)
        headertext(3,position) = LabelUnits(5)
        do i = 1, 2
            do k = 1, 2
                do l = 2, 8
                    position = position + 1
                    headertext(1,position) = trim(Labels(l+4))//trim(Layers(i))
                    headertext(2,position) = compartmentnames(j)
                    headertext(3,position) = LabelUnits(k+5)
                end do
            end do
            position = position + 1
            headertext(1,position) = trim(Labels(13))//trim(Layers(i))
            headertext(2,position) = compartmentnames(j)
            headertext(3,position) = LabelUnits(7)
            position = position + 1
            headertext(1,position) = trim(Labels(14))//trim(Layers(i))
            headertext(2,position) = compartmentnames(j)
            headertext(3,position) = LabelUnits(7)
            position = position + 1
            headertext(1,position) = trim(Labels(15))//trim(Layers(i))
            headertext(2,position) = compartmentnames(j)
            headertext(3,position) = LabelUnits(7)
        end do
    end do
        
    ! Species 
    !do j = 1, nm1
    !    do i = 1, 2
    !        do k = 3, nprod+2
    !            position = position + 1
    !            headertext(1,position) = trim(Species(k-2))//trim(Layers(i))
    !            headertext(2,position) = compartmentnames(j)
    !            headertext(3,position) = LabelUnits(3)
    !        end do
    !    end do
    !end do
    
    ! write out header
    write(ioresid,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
    write(ioresid,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
    write(ioresid,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)

 end subroutine ssHeadersResid

! --------------------------- ssHeadersFSlabs -------------------------------------------

  subroutine ssHeadersFSlabs

    ! This is the header information for the normal spreadsheet output

    use cparams
    use debug
    use vents
    implicit none

    ! local variables     
    integer, parameter :: maxhead = 1 + mxvents*(4 + mxslab)
    character(35) :: headertext(3,maxhead), Labels(6), LabelUnits(2)
    integer :: position, i, j

    data Labels / 'time', 'Room 1','Room 2', 'Vent Num', 'Num Slabs', 'Slab'/
    data LabelUnits / 'sec', 'w'/

    !  spreadsheet header.  Add time first
    headertext(1,1) = Labels(1)
    headertext(2,1) = ' '
    headertext(3,1) = LabelUnits(1)
 
    position = 1
    
    do i = 1, n_hvents
        do j = 1, 4
            position = position + 1
            headertext(1,position) = trim(Labels(j+1))
            headertext(2,position) = ' '
            headertext(3,position) = ' '
        end do
        do j = 1, mxslab
            position = position + 1
            headertext(1,position) = trim(Labels(6))
            call toIntString(j,headertext(2,position))
            headertext(3,position) = trim(LabelUnits(2))
        end do
    end do 
    
    ! write out header
    write(ioslab,"(1024(a,','))") (trim(headertext(1,i)),i=1,position)
    write(ioslab,"(1024(a,','))") (trim(headertext(2,i)),i=1,position)
    write(ioslab,"(1024(a,','))") (trim(headertext(3,i)),i=1,position)

    end subroutine ssHeadersFSlabs
    
! --------------------------- rev_ssHeaders -------------------------------------------

    integer function rev_ssHeaders ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(A)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_ssHeaders = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_ssHeaders
    