module spreadsheet_routines

    use precision_parameters

    use fire_routines, only : flame_height
    use target_routines, only: get_target_temperatures
    use opening_fractions, only : get_vent_opening
    use spreadsheet_header_routines
    use utility_routines, only: ssaddtolist, readcsvformat
    
    use cfast_types, only: fire_type, ramp_type, room_type, detector_type, target_type, vent_type, montecarlo_type

    use cparams, only: u, l, mxrooms, mxfires, mxdtect, mxtarg, mxhvents, mxfslab, mxvvents, mxmvents, &
        ns, soot, soot_flaming, soot_smolder, smoked, mx_monte_carlo
    use diag_data, only: radi_verification_flag
    use fire_data, only: n_fires, fireinfo
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: nr, nrm1, roominfo
    use setup_data, only: validation_flag, iofilsmvzone, iofilssn, iofilssf, iofilssm, iofilsss, iofilssw, iofilssd, &
        iofilssmc, iofill, ss_out_interval, project, extension
    use target_data, only: n_detectors, detectorinfo, n_targets, targetinfo
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo
    use Monte_Carlo_data, only: n_mcarlo, mcarloinfo, csvnames, num_csvfiles, iocsv

    implicit none
    
    integer, dimension(4), parameter :: iwptr = (/1, 3, 4, 2/)

    private

    public output_spreadsheet, output_spreadsheet_smokeview, output_spreadsheet_montecarlo

    contains
    

! --------------------------- output_spreadsheet -------------------------------------------

    subroutine output_spreadsheet(time)

    real(eb), intent(in) :: time

    call output_spreadsheet_normal (time)
    call output_spreadsheet_species (time)
    call output_spreadsheet_species_mass (time)
    call output_spreadsheet_flow (time)
    call output_spreadsheet_target (time)
    if (radi_verification_flag) call output_spreadsheet_diag(time)

    return

    end subroutine output_spreadsheet

! --------------------------- output_spreadsheet_normal ------------------------------------

    subroutine output_spreadsheet_normal (time)

    !  writes to the {project}_n.csv file, the compartment information and the fires

    real(eb), intent(in) :: time

    integer, parameter :: maxhead = 1+8*mxrooms+5+10*mxfires
    real(eb) :: outarray(maxhead), f_height, fire_ignition
    logical :: firstc = .true.
    integer :: position, i
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    save firstc

    ! headers
    if (firstc) then
        call ssheaders_normal
        firstc = .false.
    end if

    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nrm1
        roomptr => roominfo(i)
        call ssaddtolist (position,roomptr%temp(u)-kelvin_c_offset,outarray)
        if (.not.roomptr%shaft) then
            call ssaddtolist(position,roomptr%temp(l)-kelvin_c_offset,outarray)
            call ssaddtolist (position,roomptr%depth(l),outarray)
        end if
        call ssaddtolist (position,roomptr%volume(u),outarray)
        call ssaddtolist (position,roomptr%relp - roomptr%interior_relp_initial ,outarray)
    end do

    ! Fires
    do i = 1,nr
        roomptr => roominfo(i)
        call ssaddtolist (position,roomptr%qdot_doorjet,outarray)
    end do

    if (n_fires/=0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            f_height = flame_height (fireptr%qdot_actual,fireptr%firearea)
            if (fireptr%ignited) then
                fire_ignition = 1
            else
                fire_ignition = 0
            end if
            call ssaddtolist (position,fire_ignition,outarray)
            call ssaddtolist (position,fireptr%mdot_entrained,outarray)
            call ssaddtolist (position,fireptr%mdot_pyrolysis,outarray)
            call ssaddtolist (position,fireptr%qdot_theoretical,outarray)
            call ssaddtolist (position,fireptr%qdot_actual,outarray)
            call ssaddtolist (position,fireptr%qdot_layers(l),outarray)
            call ssaddtolist (position,fireptr%qdot_layers(u),outarray)
            call ssaddtolist (position,f_height,outarray)
            call ssaddtolist (position,fireptr%qdot_convective,outarray)
            call ssaddtolist (position,fireptr%total_pyrolysate,outarray)
            call ssaddtolist (position,fireptr%total_trace,outarray)
        end do
    end if

    call ssprintresults (iofilssn, position, outarray)

    return
    end subroutine output_spreadsheet_normal

    subroutine ssprintresults (iounit,ic,array)

    real(eb), intent(in) :: array(*)
    integer, intent(in) :: iounit, ic

    integer i
    character(35), dimension(16384) :: out
    
    out = ' '
    do i = 1, ic
        if (validation_flag) then
            write (out(i),"(e19.12)" ) array(i)
        else
            write (out(i),"(e13.6)" ) array(i)
        end if
    end do
    write (iounit,"(16384a)") (trim(out(i)) // ',',i=1,ic-1),out(ic)
    
    return

    end subroutine ssprintresults

! --------------------------- output_spreadsheet_flow -------------------------------------------

    subroutine output_spreadsheet_flow (time)

    ! output the flow data to the flow spreadsheet {project}_f.csv

    integer, parameter :: maxoutput = mxhvents*4

    real(eb), intent(in) :: time

    real(eb) :: outarray(maxoutput),flow(8), sumin, sumout, netflow, trace, tracefiltered
    integer :: position, i, ifrom, ito, j
    type(vent_type), pointer :: ventptr
    logical :: firstc = .true.
    save firstc

    if (firstc) then
        call ssheaders_flow
        firstc = .false.
    end if

    position = 0

    ! first the time
    call ssaddtolist (position,time,outarray)

    ! next the horizontal flow through vertical vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)
        ifrom = ventptr%room1
        ito = ventptr%room2
        netflow = ventptr%h_mflow(2,1,1) - ventptr%h_mflow(2,1,2) + ventptr%h_mflow(2,2,1) - ventptr%h_mflow(2,2,2)
        call ssaddtolist (position,netflow,outarray)
        netflow = ventptr%h_mflow(1,1,1) - ventptr%h_mflow(1,1,2) + ventptr%h_mflow(1,2,1) - ventptr%h_mflow(1,2,2)
        call ssaddtolist (position,netflow,outarray)
        
        if (validation_flag) then
            call ssaddtolist(position,ventptr%h_mflow(1,1,1),outarray)
            call ssaddtolist(position,ventptr%h_mflow(1,1,2),outarray)
            call ssaddtolist(position,ventptr%h_mflow(1,2,1),outarray)
            call ssaddtolist(position,ventptr%h_mflow(1,2,2),outarray)
            call ssaddtolist(position,ventptr%h_mflow(2,1,1),outarray)
            call ssaddtolist(position,ventptr%h_mflow(2,1,2),outarray)
            call ssaddtolist(position,ventptr%h_mflow(2,2,1),outarray)
            call ssaddtolist(position,ventptr%h_mflow(2,2,2),outarray)
        end if 
        
    end do

    ! next natural flow through horizontal vents (vertical flow)
    do i = 1, n_vvents

        ventptr => vventinfo(i)
        ifrom = ventptr%room2
        ito = ventptr%room1

        flow = 0.0_eb
        if (ventptr%mflow(2,u)>=0.0_eb) flow(5) = ventptr%mflow(2,u)
        if (ventptr%mflow(2,u)<0.0_eb) flow(6) = -ventptr%mflow(2,u)
        if (ventptr%mflow(2,l)>=0.0_eb) flow(7) = ventptr%mflow(2,l)
        if (ventptr%mflow(2,l)<0.0_eb) flow(8) = -ventptr%mflow(2,l)
        if (ventptr%mflow(1,u)>=0.0_eb) flow(1) = ventptr%mflow(1,u)
        if (ventptr%mflow(1,u)<0.0_eb) flow(2) = -ventptr%mflow(1,u)
        if (ventptr%mflow(1,l)>=0.0_eb) flow(3) = ventptr%mflow(1,l)
        if (ventptr%mflow(1,l)<0.0_eb) flow(4) = -ventptr%mflow(1,l)
        
        sumin = flow(5) + flow(7)
        sumout = flow(6) + flow(8)
        netflow = sumin - sumout
        call ssaddtolist (position,netflow,outarray)
        sumin = flow(1) + flow(3)
        sumout = flow(2) + flow(4)
        netflow = sumin - sumout
        call ssaddtolist (position,netflow,outarray)
        
        if(validation_flag) then
            do j = 1, 8
                call ssaddtolist(position, flow(j), outarray)
            end do
        end if 
    end do

    ! finally, mechanical ventilation
    if (n_mvents/=0) then
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            flow = 0.0_eb

            flow = 0.0_eb
            if (ventptr%mflow(2,u)>=0.0_eb) flow(5) = ventptr%mflow(2,u)
            if (ventptr%mflow(2,u)<0.0_eb) flow(6) = -ventptr%mflow(2,u)
            if (ventptr%mflow(2,l)>=0.0_eb) flow(7) = ventptr%mflow(2,l)
            if (ventptr%mflow(2,l)<0.0_eb) flow(8) = -ventptr%mflow(2,l)
            if (ventptr%mflow(1,u)>=0.0_eb) flow(1) = ventptr%mflow(1,u)
            if (ventptr%mflow(1,u)<0.0_eb) flow(2) = -ventptr%mflow(1,u)
            if (ventptr%mflow(1,l)>=0.0_eb) flow(3) = ventptr%mflow(1,l)
            if (ventptr%mflow(1,l)<0.0_eb) flow(4) = -ventptr%mflow(1,l)

            sumin = flow(5) + flow(7)
            sumout = flow(6) + flow(8)
            netflow = sumin - sumout
            call ssaddtolist (position,netflow,outarray)
            trace =abs(ventptr%total_trace_flow(u))+abs(ventptr%total_trace_flow(l))
            tracefiltered =abs(ventptr%total_trace_filtered(u))+abs(ventptr%total_trace_filtered(l))
            call ssaddtolist (position, trace, outarray)
            call ssaddtolist (position, tracefiltered, outarray)
        
            if(validation_flag) then
                do j = 1, 8
                    call ssaddtolist(position, flow(j), outarray)
                end do
            end if 
        end do
    end if

    call ssprintresults(iofilssf, position, outarray)
    return

    end subroutine output_spreadsheet_flow

! --------------------------- output_spreadsheet_target -------------------------------------------

    subroutine output_spreadsheet_target (time)

    ! output the temperatures and fluxes on surfaces and targets at the current time

    integer, parameter :: maxoutput=4*mxrooms+27*mxtarg+4*mxdtect
    real(eb), intent(in) :: time

    real(eb) :: outarray(maxoutput), zdetect, tjet, vel, value, xact
    real(eb) :: tttemp, tctemp, tlay, tgtemp, cjetmin 
    integer :: position, i, iw, itarg, iroom

    type(target_type), pointer :: targptr
    type(detector_type), pointer ::dtectptr
    type(room_type), pointer :: roomptr

    logical :: firstc = .true.
    save firstc

    if (firstc) then
        call ssheaders_target
        firstc = .false.
    end if

    position = 0

    !	First the time

    call ssaddtolist (position,time,outarray)

    !     First the surface temperatures for each compartment

    do i=1,nrm1
        roomptr => roominfo(i)
        do iw = 1, 4
            call ssaddtolist (position,roomptr%t_surfaces(1,iwptr(iw))-kelvin_c_offset,outarray)
        end do
    end do

    call get_target_temperatures

    ! now do targets if defined
    do itarg = 1, n_targets
        targptr => targetinfo(itarg)
        tgtemp = targptr%tgas
        tttemp = targptr%tfront
        tctemp = targptr%tinternal

        call ssaddtolist (position, tgtemp-kelvin_c_offset, outarray)
        call ssaddtolist (position, tttemp-kelvin_c_offset, outarray)
        call ssaddtolist (position, tctemp-kelvin_c_offset, outarray)
        ! front surface
        call ssaddtolist (position, targptr%flux_incident_front / 1000._eb, outarray)
        call ssaddtolist (position, targptr%flux_net(1) / 1000._eb, outarray)
        
        !much more detailed output for validation_flag option
        if (validation_flag) then
            call ssaddtolist (position, targptr%flux_radiation(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_convection(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_fire(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_surface(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_gas(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_target(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_net_gauge(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_radiation_gauge(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_convection_gauge(1) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_target_gauge(1) / 1000._eb, outarray)
            ! back surface
            tttemp = targptr%tback
            call ssaddtolist (position, tttemp-kelvin_c_offset, outarray)
            call ssaddtolist (position, targptr%flux_net(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_radiation(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_convection(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_fire(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_surface(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_gas(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_target(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_net_gauge(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_radiation_gauge(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_convection_gauge(2) / 1000._eb, outarray)
            call ssaddtolist (position, targptr%flux_target_gauge(2) / 1000._eb, outarray)
        end if
        
        ! tenability at target location
        call ssaddtolist (position, targptr%fed_gas, outarray)
        call ssaddtolist (position, targptr%dfed_gas, outarray)
        call ssaddtolist (position, targptr%fed_heat, outarray)
        call ssaddtolist (position, targptr%dfed_heat, outarray)
        call ssaddtolist (position, targptr%fed_obs, outarray)
    end do

    ! detectors (including sprinklers)
    cjetmin = 0.10_eb
    do i = 1, n_detectors
        dtectptr => detectorinfo(i)
        zdetect = dtectptr%center(3)
        iroom = dtectptr%room
        roomptr => roominfo(iroom)
        if (zdetect>roomptr%depth(l)) then
            tlay = roomptr%temp(u)
        else
            tlay = roomptr%temp(l)
        end if
        if (dtectptr%activated) then
            xact = 1.0_eb
        else
            xact = 0.0_eb
        end if
        tjet = max(dtectptr%temp_gas,tlay)
        vel = max(dtectptr%velocity,cjetmin)
        value =  dtectptr%value
        if (dtectptr%dtype/=smoked) value = value - kelvin_c_offset
        call ssaddtolist(position, value, outarray)
        call ssaddtolist(position, xact, outarray)
        call ssaddtolist(position, tjet-kelvin_c_offset, outarray)
        call ssaddtolist(position, vel, outarray)
    end do

    call ssprintresults (iofilssw, position, outarray)
    return

    end subroutine output_spreadsheet_target

! --------------------------- output_spreadsheet_species -------------------------------------------

    subroutine output_spreadsheet_species (time)

    ! write out the species to the spreadsheet file

    integer, parameter :: maxhead = 1+2*ns*mxrooms
    real(eb), intent(in) :: time

    real(eb) :: outarray(maxhead), ssvalue
    integer :: position, i, lsp, layer
    logical, dimension(ns), parameter :: tooutput = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.true., &
          .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false./)
    logical, dimension(ns), parameter :: molfrac = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.false.,.false.,.false.,.false., &
          .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false.,.false./)
    logical :: firstc = .true.
    type(room_type), pointer :: roomptr

    save outarray, firstc

    ! If there are no species, then don't do the output
    if (ns==0) return

    ! Set up the headings
    if (firstc) then
        call ssheaders_species
        firstc = .false.
    end if

    ! From now on, just the data, please
    position = 0
    call ssaddtolist (position,time,outarray)

    do i = 1, nrm1
        roomptr => roominfo(i)
        do layer = u, l
            do lsp = 1, ns
                if (layer==u.or..not.roomptr%shaft) then
                    if (tooutput(lsp)) then
                        ssvalue = roomptr%species_output(layer,lsp)
                        if (validation_flag.and.molfrac(lsp)) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                        if (validation_flag.and.lsp==soot) ssvalue = ssvalue*264.6903_eb ! converts od to mg/m^3
                        if (validation_flag.and.lsp==soot_flaming) ssvalue =ssvalue*264.6903_eb !converts od to mg/m^3
                        if (validation_flag.and.lsp==soot_smolder) ssvalue =ssvalue*264.6903_eb !converts od to mg/m^3
                        call ssaddtolist (position,ssvalue,outarray)
                        ! we can only output to the maximum array size; this is not deemed to be a fatal error!
                        if (position>=maxhead) go to 90
                    end if
                end if
            end do
        end do
    end do

90  call SSprintresults (iofilsss ,position, outarray)

    return

    end subroutine output_spreadsheet_species

! --------------------------- output_spreadsheet_species_mass -------------------------------------------

    subroutine output_spreadsheet_species_mass (time)

    ! write out the species mass to the spreadsheet file

    integer, parameter :: maxhead = 1+2*ns*mxrooms
    real(eb), intent(in) :: time

    real(eb) :: outarray(maxhead), ssvalue
    integer :: position, i, lsp, layer
    logical, dimension(ns), parameter :: tooutput(ns) = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.true., &
          .true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true./)
    logical :: firstc = .true.
    type(room_type), pointer :: roomptr

    save outarray, firstc

    ! If there are no species, then don't do the output
    if (ns==0) return

    ! Set up the headings
    if (firstc) then
        call ssheaders_speciesmass
        firstc = .false.
    end if

    ! From now on, just the data, please
    position = 0
    call ssaddtolist (position,time,outarray)

    do i = 1, nrm1
        roomptr => roominfo(i)
        do layer = u, l
            do lsp = 1, ns
                if (layer==u.or..not.roomptr%shaft) then
                    if (tooutput(lsp)) then
                        if (lsp<13 .or. (lsp>=13.and.validation_flag)) then
                            ssvalue = roomptr%species_mass(layer,lsp)
                            call ssaddtolist (position,ssvalue,outarray)
                            ! we can only output to the maximum array size; this is not deemed to be a fatal error!
                            if (position>=maxhead) go to 90
                        end if
                    end if
                end if
            end do
        end do
    end do

90  call SSprintresults (iofilssm, position, outarray)

    return

    end subroutine output_spreadsheet_species_mass

! --------------------------- output_spreadsheet_smokeview -------------------------------------------

    subroutine output_spreadsheet_smokeview (time)

    ! writes to the {project}_zone.csv file, the smokeview information

    integer, parameter :: maxhead = 1+7*mxrooms+5+7*mxfires+mxhvents*(4+10*mxfslab)+10*mxvvents+12*mxmvents
    real(eb), intent(in) :: time

    real(eb) :: outarray(maxhead), f_height, fraction, height, width, avent, slabs, vflow
    logical :: firstc
    integer :: position
    integer :: i, j, iroom1, iroom2, ik, im, ix
    character(64) :: rampid


    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr
    type(target_type), pointer :: targptr

    data firstc/.true./
    save firstc

    ! Headers
    if (firstc) then
        call ssheaders_smv(.true.)
        firstc = .false.
    end if

    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nrm1
        roomptr => roominfo(i)
        call ssaddtolist(position,roomptr%temp(u)-kelvin_c_offset,outarray)
        if (.not.roomptr%shaft) then
            call ssaddtolist(position,roomptr%temp(l)-kelvin_c_offset,outarray)
            call ssaddtolist(position,roomptr%depth(l),outarray)
        end if
        call ssaddtolist(position,roomptr%relp,outarray)
        call ssaddtolist(position,roomptr%rho(u),outarray)
        if (.not.roomptr%shaft) call ssaddtolist(position,roomptr%rho(l),outarray)
        call ssaddtolist(position,roomptr%species_output(u,soot),outarray)
        if (.not.roomptr%shaft) call ssaddtolist(position,roomptr%species_output(l,soot),outarray)
        do j = 1, 4
            call ssaddtolist(position,roomptr%t_surfaces(1,iwptr(j))-kelvin_c_offset,outarray)
        end do
    end do

    ! fires
    if (n_fires/=0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            f_height = flame_height (fireptr%qdot_actual,fireptr%firearea)
            call ssaddtolist (position,fireptr%qdot_actual/1000.,outarray)
            call ssaddtolist (position,f_height,outarray)
            call ssaddtolist (position,fireptr%z_position+fireptr%z_offset,outarray)
            call ssaddtolist (position,fireptr%firearea,outarray)
        end do
    end if

    ! horizontal vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        iroom1 = ventptr%room1
        iroom2 = ventptr%room2
        ik = ventptr%counter
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        rampid = ventptr%ramp_id
        call get_vent_opening (rampid,'H',im,ix,ik,i,time,fraction)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = fraction*height*width
        ! first column is just vent area ... it's for backwards compatibility with old vent flow visualization
        call ssaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call ssaddtolist (position,slabs,outarray)
        do j = 1, mxfslab
            call ssaddtolist(position,ventptr%temp_slab(j),outarray)
            call ssaddtolist(position,ventptr%flow_slab(j),outarray)
            call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
            call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
        end do
    end do

    ! vertical vents
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        avent = ventptr%current_area
        call ssaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call ssaddtolist (position,slabs,outarray)
        do j = 2, 1, -1
            vflow = ventptr%flow_slab(j)
            if (ventptr%room1<=nrm1.and.j==1) vflow = -vflow
            call ssaddtolist(position,ventptr%temp_slab(j),outarray)
            call ssaddtolist(position,vflow,outarray)
            call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
            call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
        end do
    end do

    ! mechanical vents (note sign of flow is different here to make it relative to compartment instead of hvac system
    if (n_mvents/=0) then
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            avent = ventptr%diffuser_area(1)
            call ssaddtolist (position,avent,outarray)
            ! flow slabs for the vent
            slabs = ventptr%n_slabs
            call ssaddtolist (position,slabs,outarray)
            do j = 1, 2
                call ssaddtolist(position,ventptr%temp_slab(j),outarray)
                if (ventptr%room1<=nrm1) then
                call ssaddtolist(position,-ventptr%flow_slab(j),outarray)
                else
                call ssaddtolist(position,ventptr%flow_slab(j),outarray)
                end if
                call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
                call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
            end do
        end do
    end if
    
    ! target temperature
    if (n_targets/=0) then
        do i = 1, n_targets
            targptr => targetinfo(i)
            call ssaddtolist(position,targptr%tinternal-kelvin_c_offset,outarray)
        end do
    end if
    call ssprintresults (iofilsmvzone, position, outarray)

    return
    end subroutine output_spreadsheet_smokeview
    
    ! --------------------------- output_spreadsheet_diag -------------------------------------------

    subroutine output_spreadsheet_diag (time)

    ! writes to the {project}_d.csv file, the diagnostic parameters

    real(eb), intent(in) :: time

    integer, parameter :: maxhead = 1+10*mxrooms
    real(eb) :: outarray(maxhead)
    logical :: firstc = .true.
    integer :: position, i, j
    type(room_type), pointer :: roomptr

    save firstc

    ! headers
    if (firstc) then
        call ssheaders_diagnosis
        firstc = .false.
    end if

    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nrm1
        roomptr => roominfo(i)
        do j = 1, 10
            call ssaddtolist (position,roomptr%chi(j),outarray)
        end do
    end do

    call ssprintresults (iofilssd, position, outarray)
    
    return
    end subroutine output_spreadsheet_diag
    
    !--------------------------output_spreadsheet_montecarlo-----------------------------------------------------------
    
    subroutine output_spreadsheet_montecarlo 
    
    integer, parameter :: nr = 2, nc = mx_monte_carlo+1
    real(eb) :: mcrarray(nr, nc)
    character(128) :: mccarray(nr, nc)
    integer :: i, icount, mxcol
    
    mcrarray(1, 1:nc) = 0.0
    mcrarray(2, 2:nc) = -1001
    mccarray(1:nr, 2:nc) = 'NO VALUE ASSIGNED'
    mcrarray(2,1) = 0.0
    mccarray(1,1) = 'File Name'
    mccarray(2,1) = trim(project) // trim(extension)
    mxcol = 0 
    if (ss_out_interval>0 .and. n_mcarlo > 0) then 
        icount = n_mcarlo
        do i = 1, num_csvfiles
            if (icount>0) then
                call do_csvfile(nr, nc, mcrarray, mccarray, i, icount, mxcol)
            else 
                exit
            end if
        end do
        call writecsvformat(iofilssmc, mcrarray, mccarray, nr, nc, 1, 2, mxcol)
    end if      
    
    return
    end subroutine output_spreadsheet_montecarlo
    
    !--------------------do_csvfile---------------------------------------
    
    subroutine do_csvfile(nr, nc, mcrarray, mccarray, idx, icount, mxcol)
    
    integer, intent(in) :: nr, nc, idx
    integer, intent(inout) :: mxcol, icount
    real(eb), intent(inout) :: mcrarray(nr, nc)
    character(*), intent(inout) :: mccarray(nr, nc)
    
    integer :: i
    type(montecarlo_type), pointer :: monteptr
    logical :: first, lend
    
    integer, parameter :: numr = 3, numc = 500
    real(eb) :: lastval(2, mx_monte_carlo), lasttime(mx_monte_carlo), x(numr, numc)
    character(128) :: header(numr, numc), c(numr, numc)
    
    integer :: relcol, mxhr, mxhc, ic, cols(mx_monte_carlo), icol, num_entries
    integer :: primecol(mx_monte_carlo), seccol(2, mx_monte_carlo), mxr, mxc
    real(eb) :: dummy(2, mx_monte_carlo)
    
    first = .true.
    num_entries = 0
    icol = 0
    do i = 1, n_mcarlo
        if (icount>0)  then
            monteptr => mcarloinfo(i)
            monteptr%found = .false.
            if (monteptr%file_type==csvnames(idx)) then
                num_entries = num_entries + 1
                relcol = monteptr%relative_column + 1
                icount = icount - 1
                icol = icol + 1
                cols(icol) = i
                mxcol = max(mxcol, relcol)
                mccarray(1,relcol) = monteptr%column_title
                if (first) then
                    rewind(iocsv(idx))
                    call readcsvformat(iocsv(idx), x, header, numr, numc, 2, 3, mxhr, mxhc, lend, iofill)
                    first = .false. 
                    if (lend) then 
                        return
                    end if
                end if 
                call fnd_col(ic, header, numr, numc, mxhr, mxhc, monteptr%prime_instrument, monteptr%prime_measurement)
                primecol(cols(icol)) = ic
                if (ic>0) then
                    monteptr%found = .true.
                end if
                if ((monteptr%type_of_analysis(1:8) == 'TRIGGER_' .or. &
                        monteptr%type_of_analysis(1:9) == 'INTEGRATE').and.monteptr%found) then 
                    call fnd_col(ic, header, numr, numc, mxhr, mxhc, monteptr%second_instrument, &
                                    monteptr%second_measurement)
                    seccol(1,cols(icol)) = ic
                    if (ic<1) then
                        monteptr%found = .false.
                    end if
                else if (monteptr%type_of_analysis(1:15) == 'CHECK_TOTAL_HRR'.and.monteptr%found) then
                    call fnd_col(ic, header, numr, numc, mxhr, mxhc, monteptr%second_instrument, &
                                    monteptr%second_measurement)
                    seccol(1,cols(icol)) = ic
                    if (ic<1) then
                        monteptr%found = .false.
                    end if
                    call fnd_col(ic, header, numr, numc, mxhr, mxhc, monteptr%second_instrument, &
                                    'HRR Expected')
                    seccol(2,cols(icol)) = ic
                    if (ic<1) then
                        monteptr%found = .false.
                    end if
                end if
            end if
        end if
    end do
    
    call readcsvformat(iocsv(idx), x, c, numr, numc, 2, 2, mxr, mxc, lend, iofill) 
    if (.not.lend) then
        do i = 1, icol
            monteptr => mcarloinfo(cols(i))
            if (monteptr%found) then
                relcol = monteptr%relative_column + 1
                if (monteptr%type_of_analysis(1:1) == 'M') then
                    mcrarray(2,relcol) = x(1, primecol(cols(i)))
                else if (monteptr%type_of_analysis(1:8) == 'TRIGGER_') then
                    mcrarray(2,relcol) = -1
                else if (monteptr%type_of_analysis(1:9) == 'INTEGRATE') then
                    mcrarray(2,relcol) = -1
                    lasttime(i) = x(1, primecol(cols(i)))
                    lastval(1,i) = x(1, seccol(1,cols(i)))
                else if (monteptr%type_of_analysis(1:15) == 'CHECK_TOTAL_HRR') then
                    mcrarray(2,relcol) = -1
                    dummy(1:2,i) = 0
                    lasttime(i) = x(1, primecol(cols(i)))
                    lastval(1:2,i) = x(1, seccol(1:2,cols(i)))
                else
                    mcrarray(2,relcol) = -1001
                end if
            end if 
        end do 
    else
        return
    end if
    
    do while (.not.lend)
        call readcsvformat(iocsv(idx), x, c, numr, numc, 1, 1, mxr, mxc, lend, iofill)
        if (.not.lend) then
            do i = 1, icol
                monteptr => mcarloinfo(cols(i))
                if (monteptr%found) then
                    relcol = monteptr%relative_column + 1
                    if (monteptr%type_of_analysis(1:3) == 'MAX') then
                        mcrarray(2,relcol) = max(mcrarray(2,relcol),x(1, primecol(i)))
                    else if (monteptr%type_of_analysis(1:3) == 'MIN') then
                        mcrarray(2,relcol) = min(mcrarray(2,relcol),x(1, primecol(i)))
                    else if (monteptr%type_of_analysis(1:15) == 'TRIGGER_GREATER') then
                        if (x(1, seccol(1,i))>=monteptr%criteria.and.mcrarray(2,relcol)== -1) then
                            mcrarray(2,relcol) = x(1, primecol(i))
                        end if
                    else if (monteptr%type_of_analysis(1:14) == 'TRIGGER_LESSER') then
                        if (x(1, seccol(1,i))<=monteptr%criteria.and.mcrarray(2,relcol)== -1) then
                            mcrarray(2,relcol) = x(1, primecol(i))
                        end if
                    else if (monteptr%type_of_analysis(1:9) == 'INTEGRATE') then
                        mcrarray(2,relcol) = mcrarray(2,relcol) + &
                            (x(1, seccol(1,cols(i)))+lastval(1,i))/2*(x(1, primecol(cols(i)))-lasttime(i))
                        lasttime(i) = x(1, primecol(cols(i)))
                        lastval(1,i) = x(1, seccol(1,cols(i)))
                    else if (monteptr%type_of_analysis(1:15) == 'CHECK_TOTAL_HRR') then
                        dummy(1:2,i) = dummy(1:2,i) + &
                            (x(1, seccol(1:2,cols(i)))+lastval(1:2,i))/2*(x(1, primecol(cols(i)))-lasttime(i))
                        if (dummy(2,i)>0) then
                            mcrarray(2,relcol) = dummy(1,i)/dummy(2,i)*100.0
                        end if 
                        lasttime(i) = x(1, primecol(cols(i)))
                        lastval(1:2,i) = x(1, seccol(1:2,cols(i)))
                    else
                        mcrarray(2,relcol) = -1001
                    end if
                end if
            end do
        end if
    end do 
    
    return
    end subroutine do_csvfile
    
    !-----------------------do_minmax (val, analysis, nr, nc, mxr, mxc, primei, primem)---------------------------------
    
    subroutine do_minmax(val, analysis, nr, nc, mxr, mxc, x, c, primei, primem)
    
    integer, intent(in) :: nr, nc, mxr, mxc
    character(*), intent(in) :: analysis, primei, primem, c(nr, nc)
    real(eb), intent(in) :: x(nr,nc)
    real(eb), intent(out) :: val
    
    integer, parameter :: startrow = 5
    integer :: i, ic, itime
    
    call fnd_col(ic, c, nr, nc, mxr, mxc, primei, primem)
    
    itime = mxr
    val = x(startrow, ic)
    
    do i = startrow+1, itime
        if (trim(analysis) == 'MAX') then
            val = max(val, x(i, ic))
        elseif (trim(analysis) == 'MIN') then
            val = min(val, x(i, ic))
        endif
    end do  
    
    return
    end subroutine do_minmax
    
    !---------------------------do_trigger (val, analysis, nr, nc, mxr, mxc, x, c, primei, primem, seci, secm, crit)------------
    
    subroutine do_trigger(val, analysis, nr, nc, mxr, mxc, x, c, primei, primem, &
                            seci, secm, crit)
    
    integer, intent(in) :: nr, nc, mxr, mxc
    character(*), intent(in) :: analysis, primei, primem, c(nr, nc)
    character(*), intent(in) :: seci, secm
    real(eb), intent(in) :: x(nr,nc), crit
    real(eb), intent(out) :: val
    
    integer :: i, ic, itime, ix
    
    call fnd_col(ix, c, nr, nc, mxr, mxc, primei, primem)
    call fnd_col(ic, c, nr, nc, mxr, mxc, seci, secm)
    
    itime = mxr
    val = -1
    
    if (ic>0.and.ix>0) then
        do i = 5, itime
            if (trim(analysis) == 'TRIGGER_GREATER') then
                if (x(i, ic) >= crit) then
                    val = x(i, ix)
                    itime = i
                    exit
                end if
            elseif (trim(analysis) == 'TRIGGER_LESSER') then
                if (x(i, ic) <= crit) then
                    val = x(i, ix)
                    itime = i
                    exit
                end if
            else
                write(*,*) 'failure in DO_TRIGGER, ANALYSIS invalid ',trim(analysis)
                write(iofill,*) 'failure in DO_TRIGGER, ANALYSIS invalid ',trim(analysis)
                call cfastexit('SPREADSHEET_ROUTINES:DO_TRIGGER',1)
            end if
        end do
    end if
    
    return
    end subroutine do_trigger
                            
    subroutine do_integrate(val, analysis, nr, nc, mxr, mxc, x, c, primei, primem, &
                            seci, secm)         
    
    integer, intent(in) :: nr, nc, mxr, mxc
    character(*), intent(in) :: analysis, primei, primem, c(nr, nc)
    character(*), intent(in) :: seci, secm
    real(eb), intent(in) :: x(nr,nc)
    real(eb), intent(out) :: val
    
    integer :: i, ic, itime, ix
    
    call fnd_col(ix, c, nr, nc, mxr, mxc,seci, secm)
    if(trim(primei)=='Time') then
        ic = 1
    else
        write(*,*)'Currently INTEGRATE only works with PRIME_INSTRUMENT = Time'
        write(iofill,*)'Currently INTEGRATE only works with PRIME_INSTRUMENT = Time'
        call cfastexit('SPREADSHEET_ROUTINES:DO_INTEGRATE',1)
    end if
    
    itime = mxr
    val = -1
    
    if (ic>0.and.ix>0) then
        val = 0
        do i = 6, itime
            val = val + (x(ix,i)-x(ix,i-1))/2*(x(ic,i) - x(ic,i-1))
        end do
    end if
    
    return
    end subroutine do_integrate    
    
    !-------------------get_csvfile (nr, nc, mxr, mxc, x, c, idx)
                            
    subroutine get_csvfile (nr, nc, mxr, mxc, x, c, idx)
    
    integer, intent(in) :: nr, nc, idx
    integer, intent(out) :: mxr, mxc
    character(*), intent(out) :: c(nr, nc)
    real(eb), intent(out) :: x(nr, nc)
    logical :: lend
    
    rewind(iocsv(idx))
    call readcsvformat (iocsv(idx), x, c, nr, nc, 1, -1, mxr, mxc, lend, iofill)
    
    return
    end subroutine get_csvfile
        
    !-----------------------------fnd_col(ic, c, nr, nc, mxr, mxc, instrument, measurement)-----------------------------------
    
    subroutine fnd_col(ic, c, nr, nc, mxr, mxc, instrument, measurement)

    integer, intent(out) :: ic
    integer, intent(in) :: nr, nc, mxr, mxc
    character, intent(in) :: c(nr, nc)*(*), instrument*(*), measurement*(*)
    
    integer, parameter :: instrumentRow = 2, measurementRow = 1, timeColumn = 1
    integer :: i
    
    ic = -1
    if (trim(instrument)=='Time') then
        ic = timeColumn
        return
    end if 
    
    do i = 1, mxc
        if (trim(instrument) == trim(c(instrumentRow,i))) then
            if (trim(measurement) == trim(c(measurementRow,i))) then
                ic = i
                return
            end if
        end if
    end do
    
    return
    
    end subroutine fnd_col
    
    
    ! --------------------------- writecsvformat -------------------------------------------

    subroutine writecsvformat (iunit, x, c, nr, nc, nstart, mxr, mxc)

    !     routine: writecsvformat
    !     purpose:writess a comma-delimited file as generated by Micorsoft Excel, assuming that all
    !              the data is in the form of real numbers
    !     arguments: iunit  = logical unit, already open to .csv file
    !                x      = array of dimension (numr,numc) for values in spreadsheet
    !                c      = character array of same dimenaion as x for character values in spreadsheet
    !                nr     = # of rows of arrays x and c
    !                nc     = # of columns of arrays x and c
    !                nstart = starting row of spreadsheet to read
    !                mxr    = actual number of rows read
    !                mxc    = actual number of columns read
    
    integer, intent(in) :: iunit, nr, nc, nstart, mxr, mxc

    real(eb), intent(in) :: x(nr,nc)
    character, intent(inout) :: c(nr,nc)*(*)

    character :: buf*204800
    integer :: i, j, ic, ie
    
    do i = nstart, mxr
        buf = '                    '
        ic = 1
        do j = 1, mxc
            if (x(i,j) /= 0.0) then
                write(c(i,j),'(e16.9)') x(i,j)
            end if
            ie = ic + len_trim(c(i,j))
            buf(ic:ie) = trim(c(i,j))
            ic = ie+1
            buf(ic:ic) = ','
            ic = ic+1
        end do
        write(iunit,'(A)') buf(1:ic)
    end do
    
    return
    end subroutine writecsvformat

end module spreadsheet_routines
