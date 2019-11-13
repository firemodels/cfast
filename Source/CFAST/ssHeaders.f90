module spreadsheet_header_routines

    use precision_parameters
    
    use utility_routines, only: tointstring

    use cfast_types, only: target_type, detector_type, vent_type
    
    use cparams, only: u, l, soot, soot_flaming, soot_smolder, mxrooms, mxfires, mxtarg, mxdtect, mxfires, mxhvents, &
        mxvvents, mxmvents, mxext, mxleaks, mxfslab, ns, smoked
    use diag_data, only: ioresid, ioslab
    use fire_data, only: n_fires, fireinfo, fire_type
    use room_data, only: nr, nrm1, roominfo, room_type
    use setup_data, only: validation_flag, iofilsmvzone, iofilsmv, iofilssn, iofilssf, iofilssm, iofilsss, iofilssw, iofilssd
    use target_data, only: n_detectors, detectorinfo, n_targets, targetinfo
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo

    implicit none

    private

    public ssheaders_normal, ssheaders_species, ssheaders_speciesmass, ssheaders_flow, ssheaders_target, &
        ssheaders_smv, ssHeaders_resid, ssHeaders_fslabs, ssheaders_diagnosis

    contains

! --------------------------- ssheaders_normal -------------------------------------------

    subroutine ssheaders_normal

    ! header information for the normal spreadsheet output

    integer, parameter :: maxhead = 1+8*mxrooms+5+11*mxfires
    character(35) :: headertext(4,maxhead), cRoom, cFire, Labels(18), LabelsShort(18), LabelUnits(18)
    integer :: position, i, j
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    data Labels / 'Simulation Time','Upper Layer Temperature', 'Lower Layer Temperature', 'Layer Height', &
        'Upper Layer Volume', 'Pressure', 'HRR Door Jet Fires', 'Ignition', 'Plume Entrainment Rate', 'Pyrolysis Rate', &
        'HRR Expected', 'HRR', 'HRR Lower', 'HRR Upper','Flame Height', 'Convective HRR', 'Total Pyrolysate Released', &
        'Total Trace Species Released' /
    data LabelsShort / 'Time', 'ULT_', 'LLT_', 'HGT_', 'VOL_', 'PRS_', 'DJET_', 'IGN_', 'PLUM_', 'PYROL_', &
       'HRR_E', 'HRR_', 'HRRL_', 'HRRU_', 'FLHGT_', 'HRR_C_', 'PYROL_T_', 'TRACE_T_' /
    data LabelUnits / 's', 'C', 'C', 'm', 'm^3', 'Pa', 'W', ' ', 'kg/s', 'kg/s', 'W', 'W', 'W', 'W', 'm', 'W', 'kg', 'kg' /

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelsShort(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelsShort(1)
    headertext(4,1) = LabelUnits(1)

    position = 1

    ! Compartment variables
    do j = 1, nrm1
        roomptr => roominfo(j)
        do i = 1, 5
            if (i/=2.or..not.roomptr%shaft) then
                if (i/=3.or..not.roomptr%shaft) then
                    position = position + 1
                    call toIntString(j,cRoom)
                    headertext(1,position) = trim(LabelsShort(i+1)) // trim(cRoom)
                    headertext(2,position) = Labels(i+1)
                    headertext(3,position) = roomptr%name
                    headertext(4,position) = LabelUnits(i+1)
                end if
            end if
        end do
    end do

    ! Door jet fires
    do i = 1, nr
        roomptr => roominfo(i)
        position = position + 1
        call toIntString(i,cRoom)
        if (i==nr) then
            headertext(1,position) = trim(LabelsShort(7)) // 'Outside'
        else
            headertext(1,position) = trim(LabelsShort(7)) // trim(cRoom)
        end if
        headertext(2,position) = Labels(7)
        if (i==nr) then
            headertext(3,position) = 'Outside'
        else
            headertext(3,position) = roomptr%name
        end if
        headertext(4,position) = LabelUnits(7)
    end do

    ! Fire variables.
    do j = 1, n_fires
        fireptr => fireinfo(j)
        do i = 1, 11
            position = position + 1
            call toIntString(j,cFire)
            headertext(1,position) = trim(LabelsShort(i+7))//trim(cFire)
            headertext(2,position) = Labels(i+7)
            headertext(3,position) = fireptr%name
            headertext(4,position) = LabelUnits(i+7)
        end do
    end do

    ! write out header
    write (iofilssn,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (iofilssn,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (iofilssn,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))
    write (iofilssn,"(16384a)") (trim(headertext(4,i)) // ',',i=1,position-1),trim(headertext(4,position))

    end subroutine ssheaders_normal

! --------------------------- ssheaders_species -------------------------------------------

    subroutine ssheaders_species

    ! header information for the species spreadsheet output

    ! local variables
    integer, parameter :: maxhead = 1+2*ns*mxrooms
    character(45) :: headertext(4,maxhead), cRoom, Labels(2*ns+1), LabelsShort(2*ns+1), LabelUnits(2*ns+1)
    logical, dimension(ns), parameter :: tooutput = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.true., &
          .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false./)
    logical, dimension(ns), parameter :: molfrac = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.false.,.false.,.false.,.false., &
          .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false./)
    integer position, i, j, lsp
    type(room_type), pointer :: roomptr

    data LabelsShort / 'Time', 'ULN2_', 'ULO2_', 'ULCO2_', 'ULCO_', 'ULHCN_', 'ULHCL_', 'ULTUHC_', 'ULH2O_', 'ULOD_','ULODF_', &
        'ULODS_', 'ULCT_', 'ULTS_', 'ULFM_','ULPQ_','ULPN2_','ULPO2_','ULPCO2_','ULPCO_','ULPHCN_','ULPHCL_','ULPH2O_','ULPSOOT_', &
        'LLN2_', 'LLO2_', 'LLCO2_', 'LLCO_', 'LLHCN_', 'LLHCL_', 'LLTUHC_', 'LLH2O_', 'LLOD_', 'LLODF_','LLODS_', 'LLCT_', &
        'LLTS_', 'LLFM_','LLPQ_','LLPN2_','LLPO2_','LLPCO2_','LLPCO_','LLPHCN_','LLPHCL_','LLPH2O_','LLPSOOT_'/
    data Labels / 'Simulation Time', 'N2 Upper Layer', 'O2 Upper Layer', 'CO2 Upper Layer', 'CO Upper Layer', 'HCN Upper Layer', &
       'HCL Upper Layer', 'Unburned Hydrocarbons Upper Layer', 'H2O Upper Layer', 'Optical Density Upper Layer', &
       'Optical Density Flaming Smoke Upper Layer','Optical Density Smoldering Smoke Upper Layer', &
       'C-T Product Upper Layer', 'Trace Species Upper Layer', 'Moles Fuel Upper Layer', 'Potential Total Heat Upper Layer', &
       'Potential N2 Upper Layer', 'Potential O2 Upper Layer', 'Potential CO2 Upper Layer', 'Potential CO Upper Layer', &
       'Potential HCN Upper Layer', 'Potential HCl Upper Layer', 'Potential H2O Upper Layer', 'Potential Soot Upper Layer',& 
       'N2 Lower Layer', 'O2 Lower Layer', 'CO2 Lower Layer', 'CO Lower Layer', 'HCN Lower Layer', &
       'HCL Lower Layer', 'Unburned Hydrocarbons Lower Layer', 'H2O Lower Layer', 'Optical Density Lower Layer',&
       'Optical Density Flaming Smoke Lower Layer','Optical Density Smoldering Smoke Lower Layer', &
       'C-T Product Lower Layer', 'Trace Species Lower Layer', 'Moles Fuel Lower Layer', 'Potential Total Heat Lower Layer', &
       'Potential N2 Lower Layer', 'Potential O2 Lower Layer', 'Potential CO2 Lower Layer', 'Potential CO Lower Layer', &
       'Potential HCN Lower Layer', 'Potential HCl Lower Layer', 'Potential H2O Lower Layer', 'Potential Soot Lower Layer' /
    data LabelUnits / 's', 'mol %', 'mol %', 'mol %', 'mol %', 'mol %', 'mol %', 'mol %', 'mol %', '1/m', '1/m','1/m','g-min/m^3',&
       'kg', 'mole', 'j', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'mol %', 'mol %', 'mol %', 'mol %', 'mol %', 'mol %', &
       'mol %', 'mol %', '1/m', '1/m', '1/m', 'g-min/m^3', 'kg', 'mole', 'j', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg' /

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelsShort(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelsShort(1)
    headertext(4,1) = LabelUnits(1)
    position = 1

    ! Species by compartment, then layer, then species type
    do i = 1, nrm1
        roomptr => roominfo(i)
        do j = u, l
            if (j==u.or..not.roomptr%shaft) then
                do lsp = 1, ns
                    if (tooutput(lsp)) then
                        position = position + 1
                        call toIntString(i,cRoom)
                        headertext(1,position) = trim(LabelsShort((j-1)*ns+lsp+1)) // trim(cRoom)
                        headertext(2,position) = Labels((j-1)*ns+lsp+1)
                        headertext(3,position) = roomptr%name
                        headertext(4,position) = LabelUnits((j-1)*ns+lsp+1)
                        if (validation_flag.and.molfrac(lsp)) headertext(4,position) = 'mol frac'
                        if (validation_flag.and.lsp==soot) headertext(4,position) = 'mg/m^3'
                        if (validation_flag.and.lsp==soot_flaming) headertext(4,position) = 'mg/m^3'
                        if (validation_flag.and.lsp==soot_smolder) headertext(4,position) = 'mg/m^3'
                    end if
                end do
            end if
        end do
    end do

    ! write out header
    write (iofilsss,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (iofilsss,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (iofilsss,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))
    write (iofilsss,"(16384a)") (trim(headertext(4,i)) // ',',i=1,position-1),trim(headertext(4,position))

    end subroutine ssheaders_species

! --------------------------- ssheaders_speciesMass -------------------------------------------

    subroutine ssheaders_speciesMass

    ! header information for the species mass spreadsheet output

    ! local variables
    integer, parameter :: maxhead = 1+2*ns*mxrooms
    character(45) :: headertext(4,maxhead), cRoom, Labels(2*ns+1), LabelsShort(2*ns+1), LabelUnits(2*ns+1)
    logical, dimension(ns), parameter :: tooutput = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.true., &
          .true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true./)
    integer position, i, j, lsp
    type(room_type), pointer :: roomptr

    data LabelsShort / 'Time', 'ULN2_', 'ULO2_', 'ULCO2_', 'ULCO_', 'ULHCN_', 'ULHCL_','ULTUHC_','ULH2O_','ULOD_','ULODF_', &
       'ULODS_', 'ULCT_', 'ULTS_', 'ULFM_','ULPQ_','ULPN2','ULPO2_','ULPCO2_','ULPCO_','ULPHCN_','ULPHCL_','ULPH2O_','ULPSOOT_',&
       'LLN2_', 'LLO2_', 'LLCO2_', 'LLCO_', 'LLHCN_', 'LLHCL_', 'LLTUHC_', 'LLH2O_', 'LLOD_', 'LLODF_', 'LLODS_', 'LLCT_', &
       'LLTS_', 'LLFM_','LLPQ_','LLPN2','LLPO2_','LLPCO2_','LLPCO_','LLPHCN_','LLPHCL_','LLPH2O_','LLPSOOT_'/
    data Labels / 'Simulation Time', 'N2 Upper Layer', 'O2 Upper Layer', 'CO2 Upper Layer', 'CO Upper Layer', 'HCN Upper Layer', &
       'HCL Upper Layer', 'Unburned Hydrocarbons Upper Layer', 'H2O Upper Layer', 'Optical Density Upper Layer', &
       'Optical Density Flaming Smoke Upper Layer','Optical Density Smoldering Smoke Upper Layer', &
       'C-T Product Upper Layer', 'Trace Species Upper Layer', 'Moles Fuel Upper Layer', 'Potential Total Heat Upper Layer', &
       'Potential N2 Upper Layer', 'Potential O2 Upper Layer', 'Potential CO2 Upper Layer', 'Potential CO Upper Layer', &
        'Potential HCN Upper Layer', 'Potential HCl Upper Layer', 'Potential H2O Upper Layer', 'Potential Soot Upper Layer',&
       'N2 Lower Layer', 'O2 Lower Layer', 'CO2 Lower Layer', 'CO Lower Layer', 'HCN Lower Layer', &
       'HCL Lower Layer', 'Unburned Hydrocarbons Lower Layer', 'H2O Lower Layer', 'Optical Density Lower Layer',&
       'Optical Density Flaming Smoke Lower Layer','Optical Density Smoldering Smoke Lower Layer', &
       'C-T Product Lower Layer', 'Trace Species Lower Layer', 'Moles Fuel Lower Layer', 'Potential Total Heat Lower Layer', &
       'Potential N2 Lower Layer', 'Potential O2 Lower Layer', 'Potential CO2 Lower Layer', 'Potential CO Lower Layer', &
       'Potential HCN Lower Layer', 'Potential HCl Lower Layer', 'Potential H2O Lower Layer', 'Potential Soot Lower Layer' /
    data LabelUnits / 's', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'g-min/m^3', 'kg', 'mole', &
       'j', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', &
       'kg', 'g-min/m^3', 'kg', &
       'mole', 'j', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg', 'kg' /

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelsShort(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelsShort(1)
    headertext(4,1) = LabelUnits(1)
    position = 1

    ! Species by compartment, then layer, then species type
    do i = 1, nrm1
        roomptr => roominfo(i)
        do j = u, l
            if (j==u.or..not.roomptr%shaft) then
                do lsp = 1, ns
                    if (tooutput(lsp)) then
                        if (lsp<13 .or. (lsp>=13.and.validation_flag)) then
                            position = position + 1
                            call toIntString(i,cRoom)
                            headertext(1,position) = trim(LabelsShort((j-1)*ns+lsp+1)) // trim(cRoom)
                            headertext(2,position) = Labels((j-1)*ns+lsp+1)
                            headertext(3,position) = roomptr%name
                            headertext(4,position) = LabelUnits((j-1)*ns+lsp+1)
                        end if
                    end if
                end do
            end if
        end do
    end do

    ! write out header
    write (iofilssm,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (iofilssm,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (iofilssm,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))
    write (iofilssm,"(16384a)") (trim(headertext(4,i)) // ',',i=1,position-1),trim(headertext(4,position))

    end subroutine ssheaders_speciesMass

! --------------------------- ssheaders_target -------------------------------------------

    subroutine ssheaders_target

    ! header information for the surface temperature and flux results.

    integer, parameter :: maxhead = 1+9*mxrooms+16*mxtarg+4*mxdtect
    character(35) :: headertext(4,maxhead), cDet, cRoom, Labels(29), LabelsShort(29), LabelUnits(29), frontorback(2)
    integer position, i, j, itarg
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(detector_type), pointer :: dtectptr

    data LabelsShort /'Time', 'CEILT_', 'UWALLT_', 'LWALLT_', 'FLOORT_', &
        'TRGGAST_', 'TRGSURT_', 'TRGCENT_', 'TRGFLXI_', 'TRGFLXT_', 'TRGFLXR_', &
        'TRGFLXC_','TRGFLXF_', 'TRGFLXS_', 'TRGFLXG_', 'TRGFLXRE_', 'TRGFLXTG_', 'TRGFLXRG_', 'TRGFLXCG_', 'TRGFLXREG_',  &
        'SENST_', 'SENSACT_', 'SENSGAST_', 'SENSGASVEL_', &
        'TRGFEDG_','TRGDFEDG_','TRGFEDH_','TRGDFEDH_','TRG_OBS' /
    data Labels / 'Simulation Time', 'Ceiling Temperature', 'Upper Wall Temperature', 'Lower Wall Temperature', &
        'Floor Temperature', 'Target Surrounding Gas Temperature', 'Target Surface Temperature', 'Target Center Temperature', &
        'Target Incident Flux','Target Net Flux', 'Target Radiative Flux', 'Target Convective Flux', &
        'Target Fire Radiative Flux', 'Target Surface Radiative Flux', 'Target Gas Radiative Flux', &
        'Target Radiative Loss Flux', 'Target Total Gauge Flux', 'Target Radiative Gauge Flux', 'Target Convective Gauge Flux', &
        'Target Radiative Loss Gauge Flux',  &
        'Sensor Temperature', 'Sensor Activation', 'Sensor Surrounding Gas Temperature', 'Sensor Surrounding Gas Velocity', &
        'Target Gas FED','Target GasFED Increment','Target Heat FED','Target Heat FED Increment','Target Smoke Obscuration'/
    data LabelUnits / 's', 7*'C', 12*'KW/m^2', 'C', '1=yes', 'C', 'm/s', 4*' ','1/m' /
    data frontorback / '','B_'/

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelsShort(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelsShort(1)
    headertext(4,1) = LabelUnits(1)
    position = 1

    ! Compartment surfaces temperatures
    do i = 1, nrm1
        roomptr => roominfo(i)
        do j = 1, 4
            position = position + 1
            call toIntString(i,cRoom)
            headertext(1,position) = trim(LabelsShort(j+1))//trim(cRoom)
            headertext(2,position) = Labels(j+1)
            headertext(3,position) = roomptr%name
            headertext(4,position) = LabelUnits(j+1)
        end do
    end do

    ! Targets
    do itarg = 1, n_targets
        call toIntString(itarg,cDet)
        targptr => targetinfo(itarg)
        ! front surface
        do j = 1, 15
            if (j<6.or.validation_flag) then
                position = position + 1
                headertext(1,position) = trim(frontorback(1)) // trim(LabelsShort(j+5)) // trim(cDet)
                headertext(2,position) = Labels(j+5)
                headertext(3,position) = targptr%name
                headertext(4,position) = LabelUnits(j+5)
            end if
        end do
        ! back surface
        if (validation_flag) then
            do j = 3, 15
                if (j==3) cycle
                position = position + 1
                headertext(1,position) = trim(frontorback(2)) // trim(LabelsShort(j+5)) // trim(cDet)
                headertext(2,position) = 'Back ' // Labels(j+5)
                headertext(3,position) = targptr%name
                headertext(4,position) = LabelUnits(j+5)
            end do
        end if
        do j = 1, 5
            position = position + 1
            headertext(1,position) = trim(LabelsShort(j+24)) // trim(cDet)
            headertext(2,position) = Labels(j+24)
            headertext(3,position) = targptr%name
            headertext(4,position) = LabelUnits(j+24)
        end do
    end do

    ! Detectors
    do i = 1, n_detectors
        dtectptr => detectorinfo(i)
        call toIntString(i,cDet)
        do j = 1, 4
            position = position + 1
            if (dtectptr%dtype==smoked .and. j == 1) then
                headertext(1,position) = trim(LabelsShort(j+20))//trim(cDet)
                headertext(2,position) = 'Sensor Obscuration'
                headertext(3,position) = dtectptr%name
                headertext(4,position) = '1/m'
            else
                headertext(1,position) = trim(LabelsShort(j+20))//trim(cDet)
                headertext(2,position) = Labels(j+20)
                headertext(3,position) = dtectptr%name
                headertext(4,position) = LabelUnits(j+20)
            end if
        end do
    end do

    ! write out header
    write (iofilssw,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (iofilssw,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (iofilssw,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))
    write (iofilssw,"(16384a)") (trim(headertext(4,i)) // ',',i=1,position-1),trim(headertext(4,position))

    return
    end subroutine ssheaders_target

! --------------------------- ssheaders_flow -------------------------------------------

    subroutine ssheaders_flow

    ! header information for the flow spreadsheet

    integer, parameter :: maxhead = 1 + 11*mxhvents + 11*mxvvents + 11*mxmvents + mxleaks
    
    character(35) :: headertext(4,maxhead), cTemp, ciFrom, ciTo, cVent, Labels(22), LabelsShort(13), LabelUnits(13)
    integer :: position, i, ii, ifrom, ito, ih
    type(vent_type), pointer :: ventptr

    data LabelsShort /'Time', 'H_', 'V_', 'MV_', 'MV_TRACE_', 'MV_FILTERED_', 'HT_', 'VT_', 'MVT_', 'HF_', 'VF_', 'MF_', 'L_' /
    data Labels / 'Simulation Time', 'HVENT Net Inflow', 'VVENT Net Inflow', 'MVENT Net Inflow', 'MVENT Trace Species Flow', &
       'MVENT Trace Species Filtered', 'HVENT Total Inflow Upper', 'HVENT Total Outflow Upper', &
       'HVENT Total Inflow Lower', 'HVENT Total Outflow Lower', 'VVENT Total Inflow Upper', 'VVENT Total Outflow Upper', &
       'VVENT Total Inflow Lower', 'VVENT Total Outflow Lower', 'MVENT Total Inflow Upper', 'MVENT Total Outflow Upper', &
       'MVENT Total Inflow Lower', 'MVENT Total Outflow Lower', 'HVENT Opening Fraction', 'VVENT Opening Fraction', &
        'MVENT Opening Fraction', 'LEAK Net Inflow'/
    data LabelUnits / 's', 3*'kg/s', 2*'kg', 3*'kg/s', 3*' ', 'kg/s'/

    !  spreadsheet header.  Add time first
    headertext(1,1) = LabelsShort(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelsShort(1)
    headertext(4,1) = LabelUnits(1)
    position = 1

    !	natural flow through vents in walls (doors / windows)
    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        ifrom = ventptr%room1
        call tointstring(ifrom,cifrom)
        if (ifrom==nr) cifrom = 'Outside'
        ito = ventptr%room2
        call tointstring(ito,cito)
        if (ito==nr) cito = 'Outside'

        position = position + 1
        call tointstring(ventptr%counter,cvent)
        write (ctemp,'(6a)') trim(labelsshort(2)),trim(cifrom),'_',trim(cito),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(2)
        write (ctemp,'(a,1x,a,1x,4a)') 'Vent ',trim(cvent),' from ', trim(cifrom),' to ',trim(cito)
        headertext(3,position) = ctemp
        headertext(4,position) = labelunits(2)
        position = position + 1
        write (ctemp,'(6a)') trim(labelsshort(2)),trim(cito),'_',trim(cifrom),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(2)
        write (ctemp,'(a,1x,a,1x,4a)') 'Vent ',trim(cvent),' from ',trim(cito),' to ',trim(cifrom)
        headertext(3,position) = ctemp
        headertext(4,position) = labelunits(2)
        
        if (validation_flag) then
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cifrom),'_u_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(7)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cifrom),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cifrom),'_u_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(8)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cifrom),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cifrom),'_l_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(9)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cifrom),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cifrom),'_l_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(10)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cifrom),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cito),'_u_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(7)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cito),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cito),'_u_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(8)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cito),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cito),'_l_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(9)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cito),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(7)),trim(cito),'_l_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(10)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cito),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(7)
        end if
        
        position = position + 1
        call tointstring(ventptr%counter,cvent)
        write (ctemp,'(6a)') trim(labelsshort(10)),trim(cifrom),'_',trim(cito),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(19)
        write (ctemp,'(a,1x,a,1x,4a)') 'Vent ',trim(cvent),' from ', trim(cifrom),' to ',trim(cito)
        headertext(3,position) = ctemp
        headertext(4,position) = labelunits(10)
        
    end do

    ! natural flow through  vents in ceilings / floors
    do i = 1,n_vvents

        call tointstring(i,cvent)
        ventptr => vventinfo(i)
        ifrom = ventptr%room2
        call tointstring(ifrom,ciFrom)
        if (ifrom==nr) cifrom = 'Outside'
        ito = ventptr%room1
        call tointstring(ito,cito)
        if (ito==nr) cito = 'Outside'
        position = position + 1
        write (ctemp,'(7a)') trim(labelsshort(3)),trim(cifrom),'_',trim(cito),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(3)
        write (ctemp,'(3a,1x,3a)') 'Vent ',trim(cvent),' from ',trim(cifrom),' to ',trim(cito)
        headertext(3,position) = ctemp
        headertext(4,position) = labelunits(3)
        position = position + 1
        write (ctemp,'(7a)') trim(labelsshort(3)),trim(cito),'_',trim(cifrom),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(3)
        write (ctemp,'(3a,1x,3a)') 'Vent ',trim(cvent),' from ',trim(cito),' to ',trim(cifrom)
        headertext(3,position) = cTemp
        headertext(4,position) = LabelUnits(3)
        
        if (validation_flag) then
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cifrom),'_u_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(11)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cifrom),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cifrom),'_u_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(12)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cifrom),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cifrom),'_l_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(13)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cifrom),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cifrom),'_l_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(14)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cifrom),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cito),'_u_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(11)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cito),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cito),'_u_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(12)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cito),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cito),'_l_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(13)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' to ',trim(cito),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(8)),trim(cito),'_l_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(14)
            write(ctemp,'(5a)') 'Vent ',trim(cvent),' from ',trim(cito),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(8)
        end if
        
        position = position + 1
        call tointstring(ventptr%counter,cvent)
        write (ctemp,'(6a)') trim(labelsshort(11)),trim(cifrom),'_',trim(cito),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(20)
        write (ctemp,'(a,1x,a,1x,4a)') 'Vent ',trim(cvent),' from ', trim(cifrom),' to ',trim(cito)
        headertext(3,position) = ctemp
        headertext(4,position) = labelunits(11)
        
    end do

    ! Mechanical vents
    do i = 1, n_mvents
        call tointstring(i,cvent)
        ventptr => mventinfo(i)
        ii = ventptr%room1
        call toIntString(ii,ciFrom)
        if (ii==nr) cifrom = 'Outside'
        ii = ventptr%room2
        call toIntString(ii,ciTo)
        if (ii==nr) cito = 'Outside'
        do ih = 1,3
            position = position + 1
            headertext(1,position) = trim(LabelsShort(ih+3)) // trim(cifrom) // '_' // trim(cito) // '_' // trim(cvent)
            headertext(2,position) = Labels(ih+3)
            headertext(3,position) = 'Fan ' // trim(cvent) // ' from ' // trim(ciFrom) // ' to ' // trim(ciTo)
            headertext(4,position) = LabelUnits(ih+3)
        end do

        if (validation_flag) then
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cifrom),'_u_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(15)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' to ',trim(cifrom),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cifrom),'_u_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(16)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' from ',trim(cifrom),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cifrom),'_l_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(17)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' to ',trim(cifrom),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cifrom),'_l_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(18)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' from ',trim(cifrom),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cito),'_u_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(15)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' to ',trim(cito),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cito),'_u_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(16)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' from ',trim(cito),' upper layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cito),'_l_inflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(17)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' to ',trim(cito),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
            position = position + 1
            write(ctemp,'(3a)')trim(labelsshort(9)),trim(cito),'_l_outflow'
            headertext(1,position) = ctemp
            headertext(2,position) = labels(18)
            write(ctemp,'(5a)') 'Fan ',trim(cvent),' from ',trim(cito),' lower layer '
            headertext(3,position) = ctemp
            headertext(4,position) = labelunits(9)
        end if

        position = position + 1
        call tointstring(ventptr%counter,cvent)
        write (ctemp,'(6a)') trim(labelsshort(12)),trim(cifrom),'_',trim(cito),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(21)
        write (ctemp,'(a,1x,a,1x,4a)') 'Vent ',trim(cvent),' from ', trim(cifrom),' to ',trim(cito)
        headertext(3,position) = ctemp
        headertext(4,position) = labelunits(12)
    end do
    
    ! leakage
    do i = 1, n_leaks
        ventptr=>leakinfo(i)

        ifrom = ventptr%room1
        call tointstring(ifrom,cifrom)
        if (ifrom==nr) cifrom = 'Outside'
        ito = ventptr%room2
        call tointstring(ito,cito)
        if (ito==nr) cito = 'Outside'

        position = position + 1
        call tointstring(ventptr%counter,cvent)
        write (ctemp,'(6a)') trim(labelsshort(13)),trim(cifrom),'_',trim(cito),'_',trim(cvent)
        headertext(1,position) = ctemp
        headertext(2,position) = labels(22)
        write (ctemp,'(a,1x,a,1x,4a)') 'Leak ',trim(cvent),' from ', trim(cifrom),' to ',trim(cito)
        headertext(3,position) = ctemp
        headertext(4,position) = labelunits(13)
    end do

    ! write out header
    write (iofilssf,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (iofilssf,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (iofilssf,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))
    write (iofilssf,"(16384a)") (trim(headertext(4,i)) // ',',i=1,position-1),trim(headertext(4,position))

    return

    end subroutine ssheaders_flow

! --------------------------- ssheaders_smv -------------------------------------------

    subroutine ssheaders_smv(lMode)

    ! header information for the smokeview spreadsheet output

    logical, intent(in) :: lmode

    integer, parameter :: maxhead = 1+8*mxrooms+4*mxfires+2*mxhvents+3*mxfslab*mxhvents+2*mxvvents+2*mxext
    character(35) :: headertext(2,maxhead), cRoom, cFire, cVent, cSlab, cTarg, LabelsShort(36), LabelUnits(36)
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
    do j = 1, nrm1
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

    character, intent(in) :: string*(*)

    write (iofilsmv,'(a)') 'DEVICE'
    write (iofilsmv,'(4x,a)') trim(string)
    write (iofilsmv,'(1x,3f6.1)') 0.,0.,0.
    return
    end subroutine smvDeviceTag

! --------------------------- ssHeaders_resid -------------------------------------------

 subroutine ssHeaders_resid

    ! header information for the calculate_residuals spreadsheet output

    integer, parameter :: maxhead = 1+2*(8*(ns+2)+3)*mxrooms + 4*mxrooms
    character(35) :: headertext(3,maxhead), Labels(15), LabelUnits(8), Layers(2), Species(9)
    integer position, i, j, k, l
    type(room_type), pointer :: roomptr

    data Labels / 'Time','Delta P', 'Vol Upper', 'Temp UP', 'Temp Low', 'Total Flow', 'Natural Vent Flow', 'Fire Flow',&
       'Vertical Flow', 'Mechanical Flow', 'Filtered Mass', 'Door Jet Fire Flow', 'Mixing Between Layers', &
    'Convective Flow', 'Radiative Flow'/
    data LabelUnits / 'sec', 'Pa', 'm^3', 'K', 'K', 'kg/s','w', 'kg/s' /
    data Layers /'upper', 'lower'/
    data Species /'N2','O2','CO2','CO','HCN','HCL','FUEL','H2O','Soot'/

    !  spreadsheet header.  Add time first
    headertext(1,1) = Labels(1)
    headertext(2,1) = Labels(1)
    headertext(3,1) = LabelUnits(1)

    position = 1

    ! Compartment variables
    do j = 1, nrm1
        roomptr => roominfo(j)
        position = position + 1
        headertext(1,position) = trim(Labels(2))
        headertext(2,position) = roomptr%name
        headertext(3,position) = LabelUnits(2)
        position = position + 1
        headertext(1,position) = trim(Labels(3))
        headertext(2,position) = roomptr%name
        headertext(3,position) = LabelUnits(3)
        position = position + 1
        headertext(1,position) = trim(Labels(4))
        headertext(2,position) = roomptr%name
        headertext(3,position) = LabelUnits(4)
        position = position + 1
        headertext(1,position) = trim(Labels(5))
        headertext(2,position) = roomptr%name
        headertext(3,position) = LabelUnits(5)
        do i = 1, 2
            do k = 1, 2
                do l = 2, 9
                    position = position + 1
                    headertext(1,position) = trim(Labels(l+4))//trim(Layers(i))
                    headertext(2,position) = roomptr%name
                    headertext(3,position) = LabelUnits(k+5)
                end do
            end do
            position = position + 1
            headertext(1,position) = trim(Labels(14))//trim(Layers(i))
            headertext(2,position) = roomptr%name
            headertext(3,position) = LabelUnits(7)
            position = position + 1
            headertext(1,position) = trim(Labels(15))//trim(Layers(i))
            headertext(2,position) = roomptr%name
            headertext(3,position) = LabelUnits(7)
        end do
    end do

    ! Species
    do j = 1, nrm1
        roomptr => roominfo(j)
        do i = 1, 2
            do k = 1, 9
                position = position + 1
                headertext(1,position) = trim(Species(k))//trim(Layers(i))
                headertext(2,position) = roomptr%name
                headertext(3,position) = LabelUnits(8)
            end do
        end do
    end do

    ! write out header
    write (ioresid,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (ioresid,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (ioresid,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))

 end subroutine ssHeaders_resid

! --------------------------- ssHeaders_fslabs -------------------------------------------

  subroutine ssHeaders_fslabs

    ! header information for the flow slabs spreadsheet output

    integer, parameter :: maxhead = 1 + mxhvents*(4 + mxfslab)
    character(35) :: headertext(3,maxhead), Labels(6), LabelUnits(2)
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
    character(35) :: headertext(4,maxhead), cRoom, Labels(11), LabelsShort(11), LabelUnits(11)
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
    do j = 1, nrm1
        roomptr => roominfo(j)
        do i = 1, 10
            position = position + 1
            call toIntString(j,cRoom)
            headertext(1,position) = trim(LabelsShort(i+1)) // trim(cRoom)
            headertext(2,position) = Labels(i+1)
            headertext(3,position) = roomptr%name
            headertext(4,position) = LabelUnits(i+1)
        end do
    end do

    ! write out header
    write (iofilssd,"(16384a)") (trim(headertext(1,i)) // ',',i=1,position-1),trim(headertext(1,position))
    write (iofilssd,"(16384a)") (trim(headertext(2,i)) // ',',i=1,position-1),trim(headertext(2,position))
    write (iofilssd,"(16384a)") (trim(headertext(3,i)) // ',',i=1,position-1),trim(headertext(3,position))
    write (iofilssd,"(16384a)") (trim(headertext(4,i)) // ',',i=1,position-1),trim(headertext(4,position))

    end subroutine ssheaders_diagnosis

end module spreadsheet_header_routines
