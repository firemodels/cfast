module spreadsheet_routines

    use precision_parameters

    use fire_routines, only : flame_height
    use target_routines, only: get_target_temperatures
    use opening_fractions, only : qchfraction
    use spreadsheet_header_routines
    use utility_routines, only: ssaddtolist

    use precision_parameters
    use cenviro
    use ramp_data
    use setup_data
    use target_data
    use fire_data
    use cparams
    use vent_data
    use room_data

    implicit none

    private

    public output_spreadsheet, output_spreadsheet_smokeview

    contains

! --------------------------- output_spreadsheet -------------------------------------------

    subroutine output_spreadsheet(time)

    real(eb), intent(in) :: time

    call output_spreadsheet_normal (time)
    call output_spreadsheet_species (time)
    call output_spreadsheet_flow (time)
    call output_spreadsheet_flux (time)

    return

    end subroutine output_spreadsheet

! --------------------------- output_spreadsheet_normal -------------------------------------------

    subroutine output_spreadsheet_normal (time)

    ! This routine writes to the {project}_n.csv file, the compartment information and the fires

    real(eb), intent(in) :: time

    integer, parameter :: maxhead = 1+8*mxrooms+5+9*mxfires
    real(eb) :: outarray(maxhead), fheight, fire_ignition
    logical :: firstc = .true.
    integer :: position, i
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    save firstc

    ! headers
    if (firstc) then
        call ssHeadersNormal
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
            call flame_height (fireptr%qdot_actual,fireptr%firearea,fheight)
            if (fireptr%ignited) then
                fire_ignition = 1
            else
                fire_ignition = 0
            end if
            call ssaddtolist (position,fire_ignition,outarray)
            call ssaddtolist (position,fireptr%mdot_plume,outarray)
            call ssaddtolist (position,fireptr%mdot_pyrolysis,outarray)
            call ssaddtolist (position,fireptr%qdot_actual,outarray)
            call ssaddtolist (position,fireptr%qdot_layers(l),outarray)
            call ssaddtolist (position,fireptr%qdot_layers(u),outarray)
            call ssaddtolist (position,fheight,outarray)
            call ssaddtolist (position,fireptr%qdot_convective,outarray)
            call ssaddtolist (position,fireptr%total_pyrolysate,outarray)
            call ssaddtolist (position,fireptr%total_trace,outarray)
        end do
    end if

    call ssprintresults (21, position, outarray)

    return
    end subroutine output_spreadsheet_normal

    subroutine ssprintresults (iounit,ic,array)

    real(eb), intent(in) :: array(*)
    integer, intent(in) :: iounit, ic

    integer i

    if (validate) then
        write (iounit,"(16384(e19.12,','))" ) (array(i),i=1,ic)
    else
        write (iounit,"(16384(e13.6,','))" ) (array(i),i=1,ic)
    end if
    return

    end subroutine ssprintresults

! --------------------------- output_spreadsheet_flow -------------------------------------------

    subroutine output_spreadsheet_flow (time)

    !	Routine to output the flow data to the flow spreadsheet {project}_f.csv

    integer, parameter :: maxoutput = mxhvents*4

    real(eb), intent(in) :: time

    real(eb) :: outarray(maxoutput),flow(8), sumin, sumout, netflow
    integer :: position, i, ifrom, ito
    type(vent_type), pointer :: ventptr
    logical :: firstc = .true.
    save firstc

    if (firstc) then
        call ssheadersflow
        firstc = .false.
    end if

    position = 0

    ! first the time
    call SSaddtolist (position,time,outarray)

    ! next the horizontal flow through vertical vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        ifrom = ventptr%room1
        ito = ventptr%room2
        netflow = ventptr%h_mflow(2,1,1) - ventptr%h_mflow(2,1,2) + ventptr%h_mflow(2,2,1) - ventptr%h_mflow(2,2,2)
        call SSaddtolist (position,netflow,outarray)
        netflow = ventptr%h_mflow(1,1,1) - ventptr%h_mflow(1,1,2) + ventptr%h_mflow(1,2,1) - ventptr%h_mflow(1,2,2)
        call SSaddtolist (position,netflow,outarray)
    end do

    ! next natural flow through horizontal vents (vertical flow)
    do i = 1, n_vvents

        ventptr => vventinfo(i)
        ifrom = ventptr%bottom
        ito = ventptr%top

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
        call SSaddtolist (position,netflow,outarray)
        sumin = flow(1) + flow(3)
        sumout = flow(2) + flow(4)
        netflow = sumin - sumout
        call SSaddtolist (position,netflow,outarray)
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

            sumin = flow(5) + flow(7)
            sumout = flow(6) + flow(8)
            netflow = sumin - sumout
            call SSaddtolist (position,netflow,outarray)
            flow(5) =abs(ventptr%total_trace_flow(u))+abs(ventptr%total_trace_flow(l))
            flow(6) =abs(ventptr%total_trace_filtered(u))+abs(ventptr%total_trace_filtered(l))
            call SSaddtolist (position, flow(5), outarray)
            call SSaddtolist (position, flow(6), outarray)
        end do
    end if

    call ssprintresults(22, position, outarray)
    return

    end subroutine output_spreadsheet_flow

! --------------------------- output_spreadsheet_flux -------------------------------------------

    subroutine output_spreadsheet_flux (time)

    !     Output the temperatures and fluxes on surfaces and targets at the current time

    integer, parameter :: maxoutput=4*mxrooms+27*mxtarg+4*mxdtect
    real(eb), intent(in) :: time

    real(eb) :: outarray(maxoutput), zdetect, tjet, vel, tlink, xact
    real(eb) :: tttemp, tctemp, tlay, tgtemp, cjetmin
    integer, dimension(4), parameter :: iwptr = (/1, 3, 4, 2/) 
    integer :: position, i, iw, itarg, iroom

    type(target_type), pointer :: targptr
    type(detector_type), pointer ::dtectptr
    type(room_type), pointer :: roomptr

    logical :: firstc = .true.
    save firstc

    if (firstc) then
        call ssHeadersFlux
        firstc = .false.
    end if

    position = 0

    !	First the time

    call SSaddtolist (position,time,outarray)

    !     First the surface temperatures for each compartment

    do i=1,nrm1
        roomptr => roominfo(i)
        do iw = 1, 4
            call SSaddtolist (position,roomptr%t_surfaces(1,iwptr(iw))-kelvin_c_offset,outarray)
        end do
    end do

    call get_target_temperatures

    ! now do targets if defined
    do itarg = 1, n_targets
        targptr => targetinfo(itarg)
        tgtemp = targptr%tgas
        tttemp = targptr%tfront
        tctemp = targptr%tinternal

        call SSaddtolist (position, tgtemp-kelvin_c_offset, outarray)
        call SSaddtolist (position, tttemp-kelvin_c_offset, outarray)
        call SSaddtolist (position, tctemp-kelvin_c_offset, outarray)
        ! front surface
        call SSaddtolist (position, targptr%flux_incident_front / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_net(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_radiation(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_convection(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_fire(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_surface(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_gas(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_target(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_net_gauge(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_radiation_gauge(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_convection_gauge(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_target_gauge(1) / 1000._eb, outarray)
        ! back surface
        if (validate) then
            tttemp = targptr%tback
            call SSaddtolist (position, tttemp-kelvin_c_offset, outarray)
            call SSaddtolist (position, targptr%flux_net(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_radiation(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_convection(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_fire(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_surface(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_gas(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_target(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_net_gauge(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_radiation_gauge(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_convection_gauge(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_target_gauge(2) / 1000._eb, outarray)
        end if
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
        tlink =  dtectptr%value
        call SSaddtolist(position, tlink-kelvin_c_offset, outarray)
        call SSaddtolist(position, xact, outarray)
        call SSaddtolist(position, tjet-kelvin_c_offset, outarray)
        call SSaddtolist(position, vel, outarray)
    end do

    call ssprintresults (24, position, outarray)
    return

    end subroutine output_spreadsheet_flux

! --------------------------- output_spreadsheet_species -------------------------------------------

    subroutine output_spreadsheet_species (time)

    !	Write out the species to the spread sheet file

    integer, parameter :: maxhead = 1+22*mxrooms
    real(eb), intent(in) :: time

    real(eb) :: outarray(maxhead), ssvalue
    integer :: position, i, lsp, layer
    logical, dimension(ns), parameter :: tooutput(ns) = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.true./)
    logical, dimension(ns), parameter :: molfrac(ns) = &
        (/.true.,.true.,.true.,.true.,.true.,.true.,.true.,.true.,.false.,.false.,.false./)
    logical :: firstc = .true.
    type(room_type), pointer :: roomptr

    save outarray, firstc

    ! If there are no species, then don't do the output
    if (n_species==0) return

    ! Set up the headings
    if (firstc) then
        call ssHeadersSpecies
        firstc = .false.
    end if

    ! From now on, just the data, please
    position = 0
    call SSaddtolist (position,time,outarray)

    do i = 1, nrm1
        roomptr => roominfo(i)
        do layer = u, l
            do lsp = 1, ns
                if (layer==u.or..not.roomptr%shaft) then
                    if (tooutput(lsp)) then
                        ssvalue = roomptr%species_output(layer,lsp)
                        if (validate.and.molfrac(lsp)) ssvalue = ssvalue*0.01_eb ! converts ppm to  molar fraction
                        if (validate.and.lsp==9) ssvalue = ssvalue *264.6903_eb ! converts od to mg/m^3 (see od calculation)
                        call SSaddtolist (position,ssvalue,outarray)
                        ! we can only output to the maximum array size; this is not deemed to be a fatal error!
                        if (position>=maxhead) go to 90
                    end if
                end if
            end do
        end do
    end do

90  call SSprintresults (23,position, outarray)

    return

    end subroutine output_spreadsheet_species

! --------------------------- output_spreadsheet_smokeview -------------------------------------------

    subroutine output_spreadsheet_smokeview (time)

    ! This routine writes to the {project}_zone.csv file, the smokeview information

    integer, parameter :: maxhead = 1+7*mxrooms+5+7*mxfires
    real(eb), intent(in) :: time

    real(eb) :: outarray(maxhead), fheight, fraction, height, width, avent, slabs, vflow
    logical :: firstc
    integer :: position
    integer :: i, j, iroom1, iroom2, ik, im, ix


    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    data firstc/.true./
    save firstc

    ! Headers
    if (firstc) then
        call ssHeadersSMV(.true.)
        firstc = .false.
    end if

    position = 0
    call SSaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nrm1
        roomptr => roominfo(i)
        call SSaddtolist(position,roomptr%temp(u)-kelvin_c_offset,outarray)
        if (.not.roomptr%shaft) then
            call SSaddtolist(position,roomptr%temp(l)-kelvin_c_offset,outarray)
            call SSaddtolist(position,roomptr%depth(l),outarray)
        end if
        call SSaddtolist(position,roomptr%relp,outarray)
        call SSaddtolist(position,roomptr%rho(u),outarray)
        if (.not.roomptr%shaft) call SSaddtolist(position,roomptr%rho(l),outarray)
        call SSaddtolist(position,roomptr%species_output(u,9),outarray)
        if (.not.roomptr%shaft) call SSaddtolist(position,roomptr%species_output(l,9),outarray)
    end do

    ! fires
    if (n_fires/=0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            call flame_height (fireptr%qdot_actual,fireptr%firearea,fheight)
            call SSaddtolist (position,fireptr%qdot_actual/1000.,outarray)
            call SSaddtolist (position,fheight,outarray)
            call SSaddtolist (position,fireptr%z_position+fireptr%z_offset,outarray)
            call SSaddtolist (position,fireptr%firearea,outarray)
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
        fraction = qchfraction (qcvh,i,time)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = fraction*height*width
        ! first column is just vent area ... it's for backwards compatibility with old vent flow visualization
        call SSaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call SSaddtolist (position,slabs,outarray)
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
        avent = ventptr%area
        call SSaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call SSaddtolist (position,slabs,outarray)
        do j = 2, 1, -1
            vflow = ventptr%flow_slab(j)
            if (ventptr%top<=nrm1.and.j==1) vflow = -vflow
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
            call SSaddtolist (position,avent,outarray)
            ! flow slabs for the vent
            slabs = ventptr%n_slabs
            call SSaddtolist (position,slabs,outarray)
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
    call ssprintresults (15, position, outarray)

    return
    end subroutine output_spreadsheet_smokeview

end module spreadsheet_routines
