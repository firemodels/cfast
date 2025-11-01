module output_routines
    
    use precision_parameters

    use utility_routines, only: get_filenumber
    use exit_routines, only: cfastexit
    use fire_routines, only : flame_height
    use target_routines, only: get_target_temperatures
    use opening_fractions, only: find_vent_opening_ramp

    use cfast_types, only: detector_type, fire_type, ramp_type, room_type, target_type, material_type, vent_type
    
    use cparams, only: u, l, lbufln, ns, ns_mass, nwal, interior, smoked, heatd, ct, trigger_by_time, trigger_by_temp, &
        w_from_room, w_from_wall, idx_tempf_trg, mx_dumps, cjetvelocitymin
    use diag_data, only: radi_verification_flag, upper_layer_thickness
    use fire_data, only: n_fires, fireinfo, lower_o2_limit
    use option_data, only: on, option, total_steps, foxygen
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: n_rooms, roominfo, exterior_ambient_temperature, interior_ambient_temperature, exterior_abs_pressure, &
        interior_abs_pressure, pressure_offset, relative_humidity, adiabatic_walls, n_cons, surface_connections, &
        interior_ambient_o2_mass_fraction, exterior_ambient_o2_mass_fraction
    use setup_data, only: cfast_version, iofill, iofilo, iofilstat, iofilsmv, iofilsmvplt, iofilsmvzone, &
        iofilssc, iofilssd, iofilssw, iofilssm, iofilssv, &
        iofilssdiag, inputfile, iofilcalc, listoutput, &
        outputfile, statusfile, title, outputformat, validation_flag, netheatflux, time_end, print_out_interval, &
        smv_out_interval, ss_out_interval, smvhead, smvdata, smvcsv, &
        ssdiag, sscalculation, sscompartment, ssdevice, sswall, ssmasses, ssvent, ssoutoptions, errormessage
    use solver_data, only: atol, nofp, noftu, noftl, nofvu, nofwt, nofoxyl, nofprd
    use devc_data, only: n_detectors, detectorinfo, n_targets, targetinfo
    use material_data, only: n_matl, material_info
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo
    use dump_data, only: n_dumps, dumpinfo, iocsv, iocsv_walls, iocsv_compartments, iocsv_vents, iocsv_masses, iocsv_devices

    implicit none
    external grabky, get_info
    
    integer, dimension(4), parameter :: iwptr = (/1, 3, 4, 2/)

    character(len=lbufln) :: lbuf, cbuf

    private

    public output_version, output_initial_conditions, output_results, open_output_files, &
        output_status, output_debug, write_error_component

    contains

! --------------------------- output_version -------------------------------------------

!> \brief   put the header information in the output file. we assume the file is open
    
!> \param   iunit (input): logical unit number to write output

    subroutine output_version (iunit, program_name, program_version)

    integer, intent(in) :: iunit, program_version
    character(len=5) :: program_name
    
    integer imajor, iminor, iminorrev
    character(len=256) :: revision, revision_date, compile_date

    call get_info(revision, revision_date, compile_date)

    call splitversion(program_version,imajor,iminor,iminorrev)

    write (iunit,'(/a/)') program_name
#ifndef VERSION_PP
#define VERSION_PP "Test Version     :"
#endif
    write (iunit,'(a,1x,a,1x,i0,".",i0,".",i0)')     TRIM(VERSION_PP),program_name,imajor, iminor, iminorrev
    write (iunit,'(A,A)')                    'Revision         : ',TRIM(revision)
    write (iunit,'(A,A)')                    'Revision Date    : ',TRIM(revision_date)
    write (iunit,'(A,A/)')                   'Compilation Date : ',TRIM(compile_date)
    return

    end subroutine output_version

! --------------------------- splitversion -------------------------------------------
    
!> \brief   parse program version as integers
    
!> \param (input):  version     integer version number of the code
!> \param   imajor (output): major version number
!> \param   iminor (output): minor version number
!> \param   iminorrev (output): minor revision number

    subroutine splitversion (version,imajor,iminor,iminorrev)

    integer, intent(in) :: version
    integer, intent(out) :: imajor,iminor,iminorrev

    if (version>=1000) then
        imajor = version/1000
        iminor = modulo(version,1000)/100
        iminorrev = modulo(version,100)
    else
        imajor = version/100
        iminor = modulo(version,100)/10
        iminorrev = modulo(version,10)
    end if
    return

    end subroutine splitversion

! --------------------------- output_initial_conditions -------------------------------------------

!> \brief   output initial test case description

    subroutine output_initial_conditions

    call output_version (iofilo,'CFAST',cfast_version)

    write (iofilo,5000) trim(inputfile), trim(title)
    if (outputformat>1) then
        call output_initial_overview
        call output_initial_ambient_conditions
        call output_initial_thermal_properties
        call output_initial_compartments
        call output_initial_vents
        call output_initial_fires
        call output_initial_targets
        call output_initial_detectors
    end if

    return

5000 format ('Data file: ',a,/,'Title: ',a)
    end subroutine output_initial_conditions

! --------------------------- output_results -------------------------------------------

!> \brief   output the results of the simulation at the current time

!> \param   time (input): current simulation time (s)

    subroutine output_results(time)

    !       results_layers      basic environment
    !       results_fires       fires
    !       results_targets     targets and walls - temperature, radiation and convective flux
    !       results_detectors   sprinkler and detector information
    !       results_species     species
    !       results_vent_flows  wall, ceiling/floor, and mechanical vents

    real(eb), intent(in) :: time
    integer :: i

    if (listoutput) then
      write (6,'("time: ", f8.1)') time
      if (n_fires > 0) then
        write (6,'("fire hrr: ", 10(1x,1pg10.3))') (fireinfo(i)%qdot_actual, i=1,MIN(n_fires,10))
      endif
      write (6,'("ul temps: ", 10(1x,f6.1))') (roominfo(i)%temp(u)-kelvin_c_offset, i=1,MIN(n_rooms,10))
      write (6,'("")')
    endif
    write (iofilo,4090)
    write (iofilo,5000) time
    write (iofilo,5010)
    if (outputformat>1) then
        call results_layers
        call results_fires
        call results_targets
        call results_detectors
        call results_species
        call results_vent_flows
    else if (outputformat==1) then
        call results_compressed (iofilo)
    end if
    return

4090 format (//,28('*'))
5000 format ('* Time = ',f8.1,' seconds. *')
5010 format (28('*'))
    end subroutine output_results

! --------------------------- results_layers -------------------------------------------

!> \brief   output the 2 layer environment at the current time

    subroutine results_layers

    integer :: icomp, ivolpercent
    type(room_type), pointer :: roomptr


    write (iofilo,5000)
    write (iofilo,5010)
    write (iofilo,5020)
    write (iofilo,5030)
    write (iofilo,5040)
    do icomp = 1, n_rooms
        roomptr =>roominfo(icomp)
        ivolpercent = roomptr%volume(u)/roomptr%cvolume*100.0_eb+0.5_eb
        if (roomptr%shaft) then
            write (iofilo,5071) roomptr%id, roomptr%temp(u)-kelvin_c_offset, roomptr%volume(u), &
                roomptr%absorb(u),roomptr%relp - roomptr%interior_relp_initial
        else
            write (iofilo,5070) roomptr%id, roomptr%temp(u)-kelvin_c_offset, &
                roomptr%temp(l)-kelvin_c_offset, roomptr%depth(l), roomptr%volume(u), &
                ivolpercent, roomptr%absorb(u),roomptr%absorb(l), roomptr%relp - roomptr%interior_relp_initial
        end if
    end do
    return

5000 format (' ')
5010 format ('Compartment    Upper     Lower      Inter.      Upper           Upper      Lower       Pressure')
5020 format ('               Temp.     Temp       Height      Vol             Absor      Absorb')
5030 FORMAT ('               (C)       (C)        (m)         (m^3)           (1/m)      (1/m)       (Pa)')
5040 format (100('-'))
5070 format (a13,3(1pg11.4),1x,1pg9.2,'(',i3,'%) ',1pg10.3,1x,1pg10.3,3x,1pg10.3)
5071 format (a13,1pg11.4,11(' '),11(' '),1x,1pg9.2,7(' '),1pg10.3,1x,10(' '),3x,1pg10.3)
    end subroutine results_layers

! --------------------------- results_fires -------------------------------------------

!> \brief   output the fire environment at the current time

    subroutine results_fires

    integer i, icomp
    real(eb) :: f_height, xems, pyrolysis_rate, xqf, xqupr, xqlow
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    ! user-specified fires
    write (iofilo,5000)
    if (n_fires/=0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            if (fireptr%ignited) then
                f_height = flame_height (fireptr%qdot_actual,fireptr%firearea)
                write (iofilo,5010) trim(fireptr%id), 'Y', fireptr%mdot_plume, fireptr%mdot_pyrolysis, fireptr%qdot_actual, &
                    f_height, fireptr%qdot_convective, fireptr%qdot_radiative, fireptr%total_pyrolysate, fireptr%total_trace
            else
                write (iofilo,5010) trim(fireptr%id), 'N'
            end if
        end do
    end if

    ! vent jet fires
    write (iofilo,'(a)') ' '
    do icomp = 1, n_rooms+1
        roomptr => roominfo(icomp)
        if (icomp<n_rooms+1) then
            xems = 0.0_eb
            pyrolysis_rate = 0.0_eb
            xqf = 0.0_eb
            xqupr = 0.0_eb
            xqlow = 0.0_eb
            do i = 1, n_fires
                fireptr => fireinfo(i)
                if (icomp==fireptr%room) then
                    xems = xems + fireptr%mdot_plume
                    pyrolysis_rate = pyrolysis_rate + fireptr%mdot_pyrolysis
                    xqf = xqf + fireptr%qdot_actual
                    xqupr = xqupr + fireptr%qdot_layers(u)
                    xqlow = xqlow + fireptr%qdot_layers(l)
                end if
            end do
            xqf = xqf + roomptr%qdot_doorjet
            if (xems+pyrolysis_rate+xqf+xqupr+xqlow+roomptr%qdot_doorjet/=0.0_eb) write (iofilo,5030) roomptr%id, &
                xems, pyrolysis_rate, xqf, xqupr, xqlow, roomptr%qdot_doorjet
        else 
            if (roomptr%qdot_doorjet/=0.0_eb) write (iofilo,5040) roomptr%qdot_doorjet
        end if
    end do
    
    return

5000 format (//,'FIRES',//,&
         'Compartment    Fire          Ign   Plume     Pyrol     Fire      Flame     Fire in   Fire in   Vent      ', &
         'Convec.   Radiat.    Pyrolysate  Trace',/, &
         '                                   Flow      Rate      Size      Height    Upper     Lower     Fire',/, &
         '                                   (kg/s)    (kg/s)    (W)       (m)       (W)       (W)       (W)       ', &
         '(W)       (W)        (kg)        (kg)' ,/,' ',146('-'))
5010 format (14x,a12,4x,a,3x,4(1pg10.3),30x,3(1pg10.3),2x,g10.3)
5020 format (13x,'Object ',i2,2x,4(1pg10.3),30x,3(1pg10.3),2x,g10.3)
5030 format (a14,20x,3(1pg10.3),10x,3(1pg10.3))
5040 format ('Outside',87x,1pg10.3)
    end subroutine results_fires

! --------------------------- results_species -------------------------------------------

!> \brief   output the layer and wall species at the current time

    subroutine results_species 

    character(len=10), dimension(ns_mass+4) :: stype = (/character(len=10) :: &
        'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O','OD', 'OD_F', 'OD_S', 'CT', ' TS'/)
    character(len=10), dimension(ns_mass+4) :: sunits = (/character(len=10) :: &
        '(%)', '(%)', '(%)', '(%)', '(%)', '(%)', '(%)', '(%)', '(1/m)', '(1/m)', '(1/m)', '(g-min/m3)', ' kg '/)
    character(len=5), dimension(2) :: lnames = (/character(len=5) :: 'UPPER', 'LOWER'/)
    character(len=255) :: ciout, cjout
    integer :: i, icomp, layer, ic, lsp
    type(room_type), pointer :: roomptr

    if (ns/=0) then

        do layer = u, l
            write (iofilo,5050) lnames(layer)
            ciout = 'Compartment'
            cjout = ' '
            ic = 16
            do lsp = 1, ns_mass+4
                !if (lsp/=10) then
                if (lsp/=ct) then
                    write (ciout(ic:ic+9),5000) stype(lsp)
                    write (cjout(ic:ic+9),5000) sunits(lsp)
                    ic = ic + 11
                end if
            end do
            write (iofilo,5020) ciout(1:len_trim(ciout))
            write (iofilo,5020) cjout(1:len_trim(cjout))
            write (iofilo,5030) ('-',i = 1,ic)
            write (ciout,5010)
            do icomp = 1, n_rooms
                roomptr => roominfo(icomp)
                write (ciout,5060) roomptr%id
                ic = 14
                if (layer==u.or..not.roomptr%shaft) then
                    do lsp = 1, ns_mass+4
                        !if (lsp/=10) then
                        if (lsp/=ct) then
                            write (ciout(ic:ic+9),5040) roomptr%species_output(layer,lsp)
                            ic = ic + 11
                        end if
                    end do
                end if
                write (iofilo,5020) ciout(1:len_trim(ciout))
            end do
        end do
    end if
    return

5000 format (a10)
5010 format (' ')
5020 format (a)
5030 format (255a1)
5040 format (1pg10.3)
5050 format (//,a5,' LAYER SPECIES',/)
5060 format (a13)
    end subroutine results_species

! --------------------------- results_vent_flows -------------------------------------------

!> \brief   output the vent flow at the current time

    subroutine results_vent_flows

    integer :: i, j, ifrom, ito
    real(eb), dimension(8) :: flow
    logical :: hasleak

    character(len=132) :: outbuf*132
    character(len=12) :: cifrom, cito
    type(vent_type), pointer :: ventptr, leakptr
    type(room_type), pointer :: roomptr

    write (iofilo,5000)

    ! horizontal flow natural vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)
        ifrom = ventptr%room1
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%id
        if (ifrom==n_rooms+1) cifrom = 'Outside'
        ito = ventptr%room2
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%id
        if (ito==n_rooms+1) cito = 'Outside'
        call flwout(outbuf,ventptr%h_mflow(1,1,1),ventptr%h_mflow(1,1,2),ventptr%h_mflow(1,2,1),ventptr%h_mflow(1,2,2),&
           ventptr%h_mflow(2,1,1),ventptr%h_mflow(2,1,2),ventptr%h_mflow(2,2,1),ventptr%h_mflow(2,2,2))
        write (iofilo,5010) 'H', i, cifrom, cito, ventptr%opening_fraction, outbuf
    end do

    ! vertical flow natural vents
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        ifrom = ventptr%room2
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%id
        if (ifrom==n_rooms+1) cifrom = 'Outside'
        ito = ventptr%room1
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%id
        if (ito==n_rooms+1) cito = 'Outside'
        flow = 0.0_eb
        if (ventptr%mflow(2,u)>=0.0_eb) flow(5) = ventptr%mflow(2,u)
        if (ventptr%mflow(2,u)<0.0_eb) flow(6) = -ventptr%mflow(2,u)
        if (ventptr%mflow(2,l)>=0.0_eb) flow(7) = ventptr%mflow(2,l)
        if (ventptr%mflow(2,l)<0.0_eb) flow(8) = -ventptr%mflow(2,l)
        if (ventptr%mflow(1,u)>=0.0_eb) flow(1) = ventptr%mflow(1,u)
        if (ventptr%mflow(1,u)<0.0_eb) flow(2) = -ventptr%mflow(1,u)
        if (ventptr%mflow(1,l)>=0.0_eb) flow(3) = ventptr%mflow(1,l)
        if (ventptr%mflow(1,l)<0.0_eb) flow(4) = -ventptr%mflow(1,l)

        call flwout(outbuf,flow(1),flow(2),flow(3),flow(4),flow(5),flow(6),flow(7),flow(8))
        write (iofilo,5010) 'V', i, cifrom, cito, ventptr%opening_fraction, outbuf
    end do

    ! mechanical vents
    do i = 1, n_mvents
        ventptr => mventinfo(i)
        ifrom = ventptr%room1
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%id
        if (ifrom==n_rooms+1) cifrom = 'Outside'
        ito = ventptr%room2
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%id
        if (ito==n_rooms+1) cito = 'Outside'
        flow = 0.0_eb
        if (ventptr%mflow(2,u)>=0.0_eb) flow(5) = ventptr%mflow(2,u)
        if (ventptr%mflow(2,u)<0.0_eb) flow(6) = -ventptr%mflow(2,u)
        if (ventptr%mflow(2,l)>=0.0_eb) flow(7) = ventptr%mflow(2,l)
        if (ventptr%mflow(2,l)<0.0_eb) flow(8) = -ventptr%mflow(2,l)
        if (ventptr%mflow(1,u)>=0.0_eb) flow(1) = ventptr%mflow(1,u)
        if (ventptr%mflow(1,u)<0.0_eb) flow(2) = -ventptr%mflow(1,u)
        if (ventptr%mflow(1,l)>=0.0_eb) flow(3) = ventptr%mflow(1,l)
        if (ventptr%mflow(1,l)<0.0_eb) flow(4) = -ventptr%mflow(1,l)

        call flwout(outbuf,flow(1),flow(2),flow(3),flow(4),flow(5),flow(6),flow(7),flow(8))
        write (iofilo,5010) 'M', i, cifrom, cito, ventptr%opening_fraction, outbuf
    end do

    ! Total mass flowing through mechanical vents up to current time
    write (iofilo,5030)
    
    do i = 1, n_mvents
        ventptr => mventinfo(i)
        ifrom = ventptr%room1
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%id
        if (ifrom==n_rooms+1) cifrom = 'Outside'
        ito = ventptr%room2
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%id
        if (ito==n_rooms+1) cito = 'Outside'
        flow = 0.0_eb
        if (ventptr%total_flow(u)>=0.0_eb) flow(1) = ventptr%total_flow(u)
        if (ventptr%total_flow(u)<0.0_eb) flow(2) = -ventptr%total_flow(u)
        if (ventptr%total_flow(l)>=0.0_eb) flow(3) = ventptr%total_flow(l)
        if (ventptr%total_flow(l)<0.0_eb) flow(4) = -ventptr%total_flow(l)
        flow(5) = abs(ventptr%total_trace_flow(u)) + abs(ventptr%total_trace_flow(l))
        flow(6) = abs(ventptr%total_trace_filtered(u)) + abs(ventptr%total_trace_filtered(l))
        call flwout(outbuf,flow(1),flow(2),flow(3),flow(4),flow(5),flow(6),0.0_eb,0.0_eb)
        write (iofilo,5050) cifrom, cito, outbuf
    end do

    ! leakage
    if (n_leaks>0) then
        write (iofilo,5060)
        do i = 1, n_rooms
            roomptr => roominfo(i)
            flow = 0.0_eb
            hasleak = .false.
            do j = 1,n_leaks
                leakptr => leakinfo(j)
                if (leakptr%room1==i) then
                    hasleak = .true.
                    flow(1) = flow(1) + leakptr%h_mflow(1,1,1)
                    flow(2) = flow(2) + leakptr%h_mflow(1,1,2)
                    flow(3) = flow(3) + leakptr%h_mflow(1,2,1)
                    flow(4) = flow(4) + leakptr%h_mflow(1,2,2)
                    flow(5) = flow(5) + leakptr%h_mflow(2,1,1)
                    flow(6) = flow(6) + leakptr%h_mflow(2,1,2)
                    flow(7) = flow(7) + leakptr%h_mflow(2,2,1)
                    flow(8) = flow(8) + leakptr%h_mflow(2,2,2)
                end if
            end do
            if (hasleak) then
                write (cifrom,'(a12)') roomptr%id
                cito = 'Outside'
                call flwout(outbuf,flow(1),flow(2),flow(3),flow(4),flow(5),flow(6),flow(7),flow(8))
                write (iofilo,5070) 'Leak', cifrom, cito, outbuf
            end if
        end do
    end if

5000 format (//,'FLOW THROUGH VENTS (kg/s)',//, &
    '                                                    Flow relative to ''From''', &
    '                             Flow Relative to ''To''',/ &
    '                                       Opening      Upper Layer               ', &
    'Lower Layer               Upper Layer               Lower Layer',/, &
    'Vent   From/Bottom    To/Top           Fraction     Inflow       Outflow      ', &
    'Inflow       Outflow      Inflow       Outflow      Inflow       Outflow',/,153('-'))
5010 format (a1,i3,3x,a12,3x,a12,3x,1pg11.4,1x,a)
5030 format (//,'TOTAL MASS FLOW THROUGH MECHANICAL VENTS (kg)',//, &
    'To             Through        ','      Upper Layer           ','    Lower Layer           ','   Trace Species',/, &
    'Compartment    Vent             ',2('Inflow       Outflow      '),' Vented ', '   Filtered',/, 104('-'))
5040 format (' ')
5050 format (a14,1x,a12,1x,a)
5060 format (//,'LEAKAGE (kg/s)',//, &
    '                                                    Flow relative to ''From''', &
    '                             Flow Relative to Outside',/ &
    '                                       Opening      Upper Layer               ', &
    'Lower Layer               Upper Layer               Lower Layer',/, &
    'Vent   From/Bottom    To/Top           Fraction     Inflow       Outflow      ', &
    'Inflow       Outflow      Inflow       Outflow      Inflow       Outflow',/,153('-'))
5070 format (a4,3x,a12,3x,a12,15x,a)

    end subroutine results_vent_flows

! --------------------------- results_compressed -------------------------------------------

!> \brief   output a compressed output for 80 column screens
    
!> \param   iounit (input): logical unit number of opened file for output

    subroutine results_compressed (iounit)

    integer, intent(in) :: iounit

    integer :: i, ir
    real(eb) :: pyrolysis_rate, xqf
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    write (iounit,5000)
    write (iounit,5010)
    do ir = 1, n_rooms+1
        roomptr => roominfo(ir)
        if (ir<n_rooms+1) then
            pyrolysis_rate = 0.0_eb
            xqf = 0.0_eb
            do i = 1, n_fires
                fireptr => fireinfo(i)
                if (ir==fireptr%room) then
                    pyrolysis_rate = pyrolysis_rate + fireptr%mdot_pyrolysis
                    xqf = xqf + fireptr%qdot_actual
                end if
            end do
            xqf = xqf + roomptr%qdot_doorjet
            if (roomptr%shaft) then
                write (iounit,5040) ir, roomptr%temp(u)-kelvin_c_offset, pyrolysis_rate, xqf, &
                    roomptr%relp - roomptr%interior_relp_initial
            else
                write (iounit,5030) ir, roomptr%temp(u)-kelvin_c_offset, roomptr%temp(l)-kelvin_c_offset, &
                    roomptr%depth(l), pyrolysis_rate, xqf, roomptr%relp - roomptr%interior_relp_initial
            end if
        else
            write (iounit,5020) roomptr%qdot_doorjet
        end if
    end do
    return

5000 format (' ')
5010 format (' Compartment   Upper   Lower   Inter.  Pyrol     Fire      Pressure',/, &
    '               Temp.   Temp.   Height  Rate      Size',/, &
    '               (C)     (C)     (m)     (kg/s)    (W)       (Pa)',/,' ',68('-'))
5020 format ('  Outside',39x,1pg10.3)
5030 format (i5,7x,2f8.1,2x,1pg9.2,3(1pg10.3))
5040 format (i5,7x,f8.1,8(' '),2x,8(' '),3(1pg10.3))
    end subroutine results_compressed

! --------------------------- results_targets -------------------------------------------

!> \brief   output the temperatures and fluxes on surfaces and targets at the current time

    subroutine results_targets

    integer :: i, iw, itarg
    real(eb) :: itotal, total, tgtemp, tttemp, tctemp, gasfed, heatfed

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr

    if (n_targets>0) then
        write (iofilo,5000)

        call get_target_temperatures

        do i=1,n_rooms
            roomptr => roominfo(i)
            write (iofilo,5010) roomptr%id, (roomptr%t_surfaces(1,iwptr(iw))-kelvin_c_offset,iw=1,4)

            do itarg = 1, n_targets
                targptr => targetinfo(itarg)
                if (targptr%room==i) then
                    tgtemp = targptr%tgas
                    tttemp = targptr%tfront
                    tctemp = targptr%tinternal
                    gasfed = targptr%fed_gas
                    heatfed = targptr%fed_heat
                    if (validation_flag.or.netheatflux) then
                        itotal = targptr%flux_incident_front
                        total = targptr%flux_net_gauge(1)
                    else
                        itotal = targptr%flux_incident_front
                        total = targptr%flux_net(1)
                    end if
                    if (abs(itotal)<=1.0e-10_eb) itotal = 0.0_eb
                    if (abs(total)<=1.0e-10_eb) total = 0.0_eb
                    if (total/=0.0_eb) then
                        write (iofilo,5020) targptr%id, tgtemp-kelvin_c_offset, tttemp-kelvin_c_offset, &
                            tctemp-kelvin_c_offset, itotal, total, gasfed, heatfed
                    elseif (itotal/=0.0_eb) then
                        write (iofilo,5030) targptr%id, tgtemp-kelvin_c_offset, tttemp-kelvin_c_offset, tctemp-kelvin_c_offset, &
                            itotal, gasfed,heatfed
                    else
                        write (iofilo,5040) targptr%id, tgtemp-kelvin_c_offset, tttemp-kelvin_c_offset, tctemp-kelvin_c_offset, &
                            gasfed, heatfed
                    end if
                end if
            end do

        end do
    end if

    return
    
    5000 format (//,'SURFACES AND TARGETS',//, &
    'Compartment    Ceiling   Up wall   Low wall  Floor    Target        Gas       Surface   Interior Incident     ', &
    'Net          Gas         Heat',/, &
    '               Temp.     Temp.     Temp.     Temp.                  Temp.     Temp.     Temp.    Flux         Flux', &
    '         FED         FED',/, &
    '               (C)       (C)       (C)       (C)                    (C)       (C)       (C)      (W/m^2)      (W/m^2)' &
    ,/,172('-'))
5010 format (a14,4(1pg10.3))
5020 format (54x,a8,4x,3(1pg10.3),1x,6(1pg10.3,3x))
5030 format (54x,a8,4x,3(1pg10.3),15x,3(1pg10.3,3x))
5040 format (54x,a8,4x,3(1pg10.3),25x,2(1pg10.3,3x))  
    end subroutine results_targets

! --------------------------- results_detectors -------------------------------------------

!> \brief   output the conditions of and at a sprinkler location (temperature, velocities etc) at the current time

    subroutine results_detectors

    integer :: i, iroom, itype
    real(eb) :: zdetect, tlay, tjet, vel, obs, tlink

    character(len=3) :: cact
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    if (n_detectors>0) then
        write (iofilo,5000)
        do i = 1, n_detectors
            dtectptr => detectorinfo(i)
            iroom = dtectptr%room
            roomptr => roominfo(iroom)

            zdetect = dtectptr%center(3)
            if (zdetect>roomptr%depth(l)) then
                tlay = roomptr%temp(u)
            else
                tlay = roomptr%temp(l)
            end if

            tjet = max(dtectptr%temp_gas,tlay)-kelvin_c_offset
            vel = max(dtectptr%velocity,cjetvelocitymin)
            obs = dtectptr%obscuration
            tlink =  dtectptr%value-kelvin_c_offset

            cact = 'NO'
            if (dtectptr%activated) cact = 'YES'

            itype = dtectptr%dtype
            if (itype==smoked) then
                write (iofilo,5010) i, roomptr%id, 'SMOKE ', tjet, vel, obs, cact
            else if (itype==heatd) then
                write (iofilo,5020) i, roomptr%id, 'HEAT  ', tlink, tjet, cact
            else
                write (iofilo,5030) i, roomptr%id, 'SPRINK', tlink, tjet, vel, cact
            end if
        end do
    end if

    return

5000 format(//'DETECTORS/ALARMS/SPRINKLERS',/, &
    '                                      Sensor         Smoke',//, &
    'Number  Compartment        Type       Temp (C)       Temp (C)      Vel (m/s)     Obs (1/m)          Activated',/, &
    '--------------------------------------------------------------------------------------------------------------')
5010    format(i3,5x,a14,5x,a6,4x,15x,1pe10.3,4x,1pe10.3,4x,1pe10.3,10x,a3)
5020    format(i3,5x,a14,5x,a6,4x,1pe10.3,5x,1pe10.3,38x,a3)
5030    format(i3,5x,a14,5x,a6,4x,1pe10.3,5x,1pe10.3,4x,1pe10.3,24x,a3)
    end subroutine results_detectors

! --------------------------- output_initial_overview -------------------------------------------

!> \brief   output initial test case overview

    subroutine output_initial_overview

    write (iofilo,5000)
    write (iofilo,5010) n_rooms, n_hvents, n_vvents, n_mvents
    write (iofilo,5020) time_end, print_out_interval, smv_out_interval, ss_out_interval

5000 format (//,'OVERVIEW',/)
5010 FORMAT (/,'Compartments    Doors, ...    Ceil. Vents, ...    MV Connects',/,61('-'),/,i4,12x,i4,10x,i4,17x,i4)
5020 format (/,'Simulation     Output         Smokeview      Spreadsheet',/, &
             'Time           Interval       Interval       Interval',/, &
             '   (s)            (s)            (s)            (s)',/,56('-'),/,4(f10.2,5x))
    end subroutine output_initial_overview

! --------------------------- output_initial_ambient_conditions -------------------------------------------

!> \brief   output initial test case ambient conditions

    subroutine output_initial_ambient_conditions

    write (iofilo,5000) interior_ambient_temperature-kelvin_c_offset, interior_abs_pressure + pressure_offset, &
       exterior_ambient_temperature-kelvin_c_offset, exterior_abs_pressure + pressure_offset, &
       interior_ambient_o2_mass_fraction, exterior_ambient_o2_mass_fraction
    return

5000 format (//,'AMBIENT CONDITIONS',//, &
    'Interior       Interior       Exterior       Exterior       Interior       Exterior',/, &
    'Temperature    Pressure       Temperature    Pressure       O2 mass frac   O2 mass frac',/, &
    '  (C)            (Pa)           (C)            (Pa)         (kg/kg)        (kg/kg)', &
    /,83('-'),/,f7.0,8x,f9.0,6x,f7.0,8x,f9.0,3x,f7.2,8x,f7.2)

    end subroutine output_initial_ambient_conditions

! --------------------------- output_initial_compartments -------------------------------------------

!> \brief   output initial test case geometry

    subroutine output_initial_compartments

    integer i, j, k
    real(eb) :: wallleakarea, floorleakarea
    type(room_type), pointer :: roomptr
    character :: hall, shaft
    character(len=7), parameter :: surfaces(3) = ['Ceiling','Floor  ','Walls  ']

    write (iofilo,5000)
    do i = 1, n_rooms
        roomptr => roominfo(i)
        shaft = '' ; if (roomptr%shaft) shaft = '*'
        hall = '' ; if (roomptr%hall) hall = '*'
        wallleakarea = (2 * (roomptr%cwidth + roomptr%cdepth) * roomptr%cheight) * roomptr%leak_area_ratios(1)
        floorleakarea = (roomptr%cwidth * roomptr%cdepth) * roomptr%leak_area_ratios(2)
        write (iofilo,5010) i, trim(roomptr%id)//repeat(' ',13), roomptr%cwidth, roomptr%cdepth, roomptr%cheight, &
            roomptr%z0, roomptr%z1, shaft, hall, wallleakarea, floorleakarea
    end do
    if (adiabatic_walls) then
        write (iofilo,*) 'All compartment surfaces are adiabatic'
    else
        write (iofilo,5020)
        do i = 1, n_rooms
            roomptr => roominfo(i)
            do j = 1, 3     ! loop over the surface
                do k = 1, roomptr%nslab_w(j) ! loop over the layers
                    if (j==1.and.k==1) then
                        write (iofilo,5030) i, trim(roomptr%id)//repeat(' ',13), trim(surfaces(j))//repeat(' ',7), k, &
                            roomptr%k_w(k,j), roomptr%c_w(k,j), roomptr%rho_w(k,j), roomptr%thick_w(k,j), roomptr%eps_w(j), &
                            trim(roomptr%matl(k,j))
                    else if (k==1) then
                        write (iofilo,5040) trim(surfaces(j))//repeat(' ',7), k, roomptr%k_w(k,j), roomptr%c_w(k,j), &
                            roomptr%rho_w(k,j), roomptr%thick_w(k,j), roomptr%eps_w(j), trim(roomptr%matl(k,j))
                    else
                        write (iofilo,5050) k, roomptr%k_w(k,j), roomptr%c_w(k,j), roomptr%rho_w(k,j), roomptr%thick_w(k,j), &
                            roomptr%eps_w(j), trim(roomptr%matl(k,j))
                    end if
                end do
            end do
        end do
    end if
    
    return
    
5000 format (//,'COMPARTMENTS',//, &
    'Compartment  Name                Width        Depth        Height       Floor        Ceiling    ', &
    'Shaft    Hall   Wall         Floor',/, &
    '                                                                        Height       Height                     ', &
    'Leakage      Leakage',/, 33x,5('(m)',10x),14x,2('(m^2)',8x),/,133('-'))
5010 format (i5,8x,a13,5(f12.2,1x),7x,a1,7x,a1,1x,2(1pG12.2,1x))
     5020 format (//,'COMPARTMENT MATERIALS',//,'Compartment  Name              Surface      Layer      Conductivity    ', &
          'Specific Heat    Density        Thickness     Emissivity      Material',/, &
          '                                                       (kW/(m C))     (kJ/(m C))      (kg/m^3)       (m)',/,146('-'))  
5030 format (i5,8x,a13,5x,a7,5x,i2,6x,5(1pg13.3,3x),2x,a)
5040 format (31x,a7,5x,i2,6x,5(1pg13.3,3x),2x,a)
5050 format (43x,i2,6x,5(1pg13.3,3x),2x,a)
    end subroutine output_initial_compartments

! --------------------------- output_initial_vents -------------------------------------------

!> \brief   output initial test case vent connections

    subroutine output_initial_vents

    integer :: i, j, iramp
    character(len=14) :: ciout, cjout 
    character(len=6) :: csout
    character(len=10) :: crout
    character(len=4) :: ctrigger
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: ventptr
    type(target_type), pointer :: targptr

    iramp = 0
    
    !     horizontal flow vents
    if (n_hvents==0) then
        write (iofilo,5000)
    else
        write (iofilo,5010)
        do i = 1, n_hvents
            ventptr => hventinfo(i)
            roomptr => roominfo(ventptr%room2)
            write (cjout,'(a14)') roomptr%id
            if (ventptr%room2==n_rooms+1) cjout = 'Outside'
            roomptr => roominfo(ventptr%room1)
            if (ventptr%opening_type==trigger_by_time) then
                ctrigger = 'Time'
                if (ventptr%npoints==0) then
                    write (iofilo,5020) roomptr%id, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                        ctrigger, 0.0_eb, 1.0_eb
                else if (ventptr%npoints==1) then
                    write (iofilo,5020) roomptr%id, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                        ctrigger, ventptr%t(1), ventptr%f(1)
                else if (ventptr%npoints==2) then
                    write (iofilo,5020) roomptr%id, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                        ctrigger, ventptr%t(1), ventptr%f(1), ventptr%t(2), ventptr%f(2)
                else
                    iramp = iramp + 1
                    write (crout,'(a6,1x,i0)') 'RAMP #',iramp
                    write (iofilo,5020) roomptr%id, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, crout
                end if
            else if (ventptr%opening_type==trigger_by_temp) then
                ctrigger = 'Temp'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5025) roomptr%id, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                    ctrigger, ventptr%opening_criterion-kelvin_c_offset, targptr%id, ventptr%f(1), ventptr%f(2)
            else
                ctrigger = 'Flux'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5025) roomptr%id, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                    ctrigger, ventptr%opening_criterion, targptr%id, ventptr%f(1), ventptr%f(2)
            end if
        end do
    end if
    
5000 format (//,'VENT CONNECTIONS',//,'There are no horizontal natural flow connections')
5010 format (//,'VENT CONNECTIONS',//,'Wall Vents (Doors, Windows, ...)',//, &
    'From           To              Vent      Width       Sill        Soffit      Open/Close  Trigger', &
    '                 Initial     Initial     Final       Final',/, &
    'Compartment    Compartment     Number                Height      Height      Type        Value  ', &
    '     Target      Time        Fraction    Time        Fraction',/, &
    41X,4('(m)         '),'(C/W/m^2)',15x,'(s)',21x,'(s)',/,157('-'))
5020 format (a14,1x,a14,i3,4x,3(f9.2,3x),5x,a,27x,4(f9.2,3x))
5025 format (a14,1x,a14,i3,4x,3(f9.2,3x),5x,a,6x,f9.2,5x,a10,9x,2(f9.2,15x))
     
    !     vertical flow vents
    if (n_vvents==0) then
        write (iofilo,5030)
    else
        write (iofilo,5040)
        do i = 1, n_vvents
            ventptr => vventinfo(i)
            roomptr => roominfo(ventptr%room1)
            write (ciout,'(a14)') roomptr%id
            if (ventptr%room1==n_rooms+1) ciout = 'Outside'
            roomptr => roominfo(ventptr%room2)
            write (cjout,'(a14)') roomptr%id
            if (ventptr%room2==n_rooms+1) cjout = 'Outside'
            csout = 'Round'
            if (ventptr%shape==2) csout = 'Square'
            roomptr => roominfo(ventptr%room2)
            if (ventptr%opening_type==trigger_by_time) then
                ctrigger = 'Time'
                if (ventptr%npoints<=2) then
                    write (iofilo,5050) ciout, cjout, ventptr%counter, csout, ventptr%area, &
                        ctrigger, ventptr%opening_initial_time, ventptr%opening_initial_fraction, &
                        ventptr%opening_final_time, ventptr%opening_final_fraction
                else
                    iramp = iramp + 1
                    write (crout,'(a6,1x,i0)') 'RAMP #',iramp
                    write (iofilo,5050) ciout, cjout, ventptr%counter, csout, ventptr%area, crout
                end if
            else if (ventptr%opening_type==trigger_by_temp) then
                ctrigger = 'Temp'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5055) ciout, cjout, ventptr%counter, csout, ventptr%area, &
                    ctrigger, ventptr%opening_criterion-kelvin_c_offset, targptr%id, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            else 
                ctrigger = 'Flux'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5055) ciout, cjout, ventptr%counter, csout, ventptr%area, &
                    ctrigger, ventptr%opening_criterion, targptr%id, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            end if
        end do
    end if
5030 format (//,'There are no vertical natural flow connections')
5040 format (//,'Ceiling and Floor Vents',//,&
         'Top            Bottom         Vent    Shape     Area      ', &
         'Open/Close  Trigger                 Initial     Initial     Final       Final',/, &
         'Compartment    Compartment    Number                      Type        Value       Target      Time        ',&
         'Fraction    Time        Fraction',/, &
         48X,'(m^2)',17x,'(C/W/m^2)',15x,'(s)',21x,'(s)',/,138('-'))
5050 format (a14,1x,a14,i3,6x,a6,3x,f5.2,6x,a,27x,4(f9.2,3x))
5055 format (a14,1x,a14,i3,6x,a6,3x,f5.2,6x,a,6x,f9.2,5x,a10,9x,2(f9.2,15x))

    !     mechanical vents
    if (n_mvents==0) then
        write (iofilo,5060)
    else
        write (iofilo,5120)
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            roomptr => roominfo(ventptr%room1)
            write (ciout,'(a14)') roomptr%id
            if (ventptr%room1==n_rooms+1) ciout = 'Outside'
            roomptr => roominfo(ventptr%room2)
            write (cjout,'(a14)') roomptr%id
            if (ventptr%room2==n_rooms+1) cjout = 'Outside'
            if (ventptr%opening_type==trigger_by_time) then
                ctrigger = 'Time'
                if (ventptr%npoints<=2) then
                    write (iofilo,5130) ciout, cjout, ventptr%counter, ventptr%diffuser_area(1), ventptr%coeff(1), &
                        ctrigger, ventptr%opening_initial_time, ventptr%opening_initial_fraction, &
                        ventptr%opening_final_time, ventptr%opening_final_fraction
                else
                    iramp = iramp + 1
                    write (crout,'(a6,1x,i0)') 'RAMP #',iramp
                    write (iofilo,5130) ciout, cjout, ventptr%counter, ventptr%coeff(1), ventptr%diffuser_area(1), crout
                end if
            else if (ventptr%opening_type==trigger_by_temp) then
                ctrigger = 'Temp'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5135) ciout, cjout, ventptr%counter, ventptr%area, ventptr%coeff(1), &
                    ctrigger, ventptr%opening_criterion-kelvin_c_offset, targptr%id, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            else 
                ctrigger = 'Flux'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5135) ciout, cjout, ventptr%counter, ventptr%area, ventptr%coeff(1), &
                    ctrigger, ventptr%opening_criterion, targptr%id, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            end if
        end do
    end if
5060 format (//,'There are no mechanical flow connections')
     5120 format (//,'Mechanical Vents (Fans)',//,&
        'From           To              Fan        Area      Flowrate     Open/Close  Trigger                 ', &
        'Initial     Initial     Final       Final',/, &
        'Compartment    Compartment     Number                            Type        Value       Target      ', &
        'Time        Fraction    Time        Fraction',/, &
        '                                          (m^2)     (m^3/s)                  (C/W/m^2)               ', &
        '(s)                     (s)',/,145('-'))
5130 format (a14,1x,a14,i3,7x,f7.2,3x,f7.2,9x,a,27x,4(f9.2,3x))
5135 format (a14,1x,a14,i3,7x,f7.2,3x,f7.2,9x,a,6x,f9.2,5x,a10,9x,2(f9.2,15x))  

     if (iramp==0) then
         write (iofilo,5150)
     else
         write (iofilo,5160)
    end if
     iramp = 0
     
     ! horizontal flow vents
     if (n_hvents>2) then
         do i = 1, n_hvents
             ventptr => hventinfo(i)
             if (ventptr%npoints>2) then
                 iramp = iramp + 1
                 roomptr => roominfo(ventptr%room2)
                 write (cjout,'(a14)') roomptr%id
                 if (ventptr%room2==n_rooms+1) cjout = 'Outside'
                 roomptr => roominfo(ventptr%room1)
                 write (iofilo,5170) 'H', roomptr%id, cjout, iramp, 'Time      ', (int(ventptr%t(j)),j=1,ventptr%npoints)
                 write (iofilo,5180) 'Fraction', (ventptr%f(j),j=1,ventptr%npoints)
             end if
         end do
     end if
             
    ! vertical flow vents
    if (n_vvents>2) then
         do i = 1, n_vvents
             ventptr => vventinfo(i)
             if (ventptr%npoints>2) then
                 iramp = iramp + 1
                 roomptr => roominfo(ventptr%room2)
                 write (cjout,'(a14)') roomptr%id
                 if (ventptr%room2==n_rooms+1) cjout = 'Outside'
                 roomptr => roominfo(ventptr%room1)
                 write (iofilo,5170) 'V', roomptr%id, cjout, iramp, 'Time      ', (int(ventptr%t(j)),j=1,ventptr%npoints)
                 write (iofilo,5180) 'Fraction', (ventptr%f(j),j=1,ventptr%npoints)
             end if
         end do
    end if
             
    ! mecahnical flow vents
    if (n_mvents>2) then
         do i = 1, n_mvents
             ventptr => mventinfo(i)
             if (ventptr%npoints>2) then
                 iramp = iramp + 1
                 roomptr => roominfo(ventptr%room2)
                 write (cjout,'(a14)') roomptr%id
                 if (ventptr%room2==n_rooms+1) cjout = 'Outside'
                 roomptr => roominfo(ventptr%room1)
                 write (iofilo,5170) 'M', roomptr%id, cjout, iramp, 'Time      ', (int(ventptr%t(j)),j=1,ventptr%npoints)
                 write (iofilo,5180) 'Fraction', (ventptr%f(j),j=1,ventptr%npoints)
             end if
         end do
    end if

     return
  
5150 format (//,'VENT RAMPS',//,'There are no vent opening ramp specifications')
5160 format (//,'VENT RAMPS',//, &
    'Type  From           To              Vent      ',/, &
    '      Compartment    Compartment     Number    ',/, &
    58x,10('(s)       '),/,151('-'))
5170 format (a1,5x,a14,1x,a14,i3,7x,a,10(i6,4x),/,55x,20(10(i6,4x),/))
5180 format (45x,a,10(f8.2,2x),/,53x,20(10(f8.2,2x),/))

    end  subroutine output_initial_vents

! --------------------------- output_initial_thermal_properties -------------------------------------------

!> \brief   output initial test case thermal properties

    subroutine output_initial_thermal_properties

    integer i, j, k
    type(room_type), pointer :: roomptr
    type(material_type), pointer :: thrmpptr

    ! check to see if any heat transfer is on
    if (.not.adiabatic_walls) then
        do i = 1, n_rooms
            roomptr => roominfo(i)
            do j = 1, nwal
                do k = 1,3
                    if (roomptr%surface_on(j).and.roomptr%matl(k,j)/=' ') go to 10
                end do
            end do
        end do
    end if
    write (iofilo,5000)
    return

    ! some surfaces are on, do the printout of the material properties
10  write (iofilo,5010)
    do i = 1, n_matl
        thrmpptr => material_info(i)
        write (iofilo,5040) thrmpptr%id, thrmpptr%k(1), thrmpptr%c(1), thrmpptr%rho(1), thrmpptr%thickness(1), thrmpptr%eps
        do j = 2, thrmpptr%nslab
            write (iofilo,5050) thrmpptr%k(j), thrmpptr%c(j), thrmpptr%rho(j), thrmpptr%thickness(j)
        end do
    end do
    write (iofilo,5060)
    return

5000 format (//,'All compartment surfaces are adiabatic.')
     5010 format (//,'THERMAL PROPERTIES',//,'Name',4X,'Conductivity',6X,'Specific Heat',5X, 'Density',8X,'Thickness',&
          5X,'Emissivity',/,8x, '(kW/(m  C))       (kJ/(m  C))       (kg/m^3)       (m)',/,83('-'))
5040 format (a8,5(1pg13.3,3x),5e10.2)
5050 format (8x,4(1pg13.3,3x))
5060 format (' ')

    end subroutine output_initial_thermal_properties

! --------------------------- output_initial_fires -------------------------------------------

!> \brief   outputs the fire specification for all the object fires

    subroutine output_initial_fires

    integer :: io, i, is
    real(eb) :: y_hcn, y_hcl

    character(len=13), dimension(0:4) :: ftype = &
        (/character(len=13) :: 'Undefined', 'Unconstrained', 'Constrained','Pool Fire', 'Furniture'/)
    character(len=6), dimension(1:3) :: fire_geometry = (/character(6) :: 'Normal', 'Wall', 'Corner'/)

    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    if (n_fires>0) then
        write (iofilo,5080)
        do io = 1, n_fires
            fireptr => fireinfo(io)
            roomptr => roominfo(fireptr%room)
            write (iofilo,5020) trim(fireptr%id), io, fire_geometry(fireptr%modified_plume)
            write (iofilo,5030) roomptr%id, ftype(fireptr%chemistry_type), fireptr%flaming_transition_time, fireptr%x_position, &
                fireptr%y_position, fireptr%z_position+fireptr%height(1), relative_humidity*100., lower_o2_limit*100.,fireptr%chirad
            write (iofilo,5031) fireptr%n_C, fireptr%n_H, fireptr%n_O, fireptr%n_N, fireptr%n_Cl
            write (cbuf,5040)
            is = 103
            write (iofilo,'(a)') cbuf(1:len_trim(cbuf))
            write (iofilo,5000) ('(kg/kg)',i = 1,(is-51)/10)
            write (iofilo,5010) ('-',i = 1,is-1)
            do i = 1, fireptr%n_qdot
                write (cbuf,5060) fireptr%t_qdot(i), fireptr%mdot(i), fireptr%hoc(i), fireptr%qdot(i), fireptr%height(i)
                y_HCN = fireptr%n_N*0.027028_eb/fireptr%molar_mass
                y_HCl = fireptr%n_Cl*0.036458_eb/fireptr%molar_mass
                write (cbuf(51:132),5070) fireptr%y_soot(i), fireptr%y_co(i), y_HCN, y_HCl, fireptr%y_trace(i)
                write (iofilo,'(a)') cbuf(1:len_trim(cbuf))
            end do
        end do
    end if
    return
5000 format ('  (s)       (kg/s)    (J/kg)    (W)       (m)       ',15(A7,3X))
5010 format (255a1)
     5020 format (//,'Name: ',A,'   Referenced as object #',i3,1x,a6,' fire',//,'Compartment    Fire Type    ',&
          '   Time to Flaming      Position (x,y,z)     Relative    Lower O2    Radiative',/ &
          ,73x,'Humidity    Limit       Fraction',/110('-'))
5030 format (a14,1x,a13,3x,f9.1,8x,3(f7.2),2x,f7.1,6x,f7.2,5x,f7.2//)
5031 format ('Chemical formula of the fuel',/,'Carbon     Hydrogen  Oxygen    Nitrogen  Chlorine',/,50('-'),/,5(f7.3,3x),//)
5040 format ('  Time      Mdot      Hcomb     Qdot      Zoffset   Soot      CO        HCN       HCl       TS')
5060 format (F7.0,3X,4(1PG10.2))
5070 format (10(1PG10.2),2x,2g10.2)
5080 format (/,'FIRES')
    end subroutine output_initial_fires

! --------------------------- output_initial_targets -------------------------------------------

!> \brief   output initial test case target specifications

    subroutine output_initial_targets

    integer :: itarg, j
    character(len=8) :: location_type

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr

    if (n_targets/=0) write (iofilo,5000)
5000 format(//,'TARGETS',//,'Target',21x,'Compartment',10x,'Position (x, y, z)',9x,&
         'Back', 11x, 'Direction (x, y, z)',8x,'Int. Temp. At',4x,'Thickness',4x,'Material',/,155('-'))

    do itarg = 1, n_targets
        targptr => targetinfo(itarg)
        roomptr => roominfo(targptr%room)
        if (targptr%back==interior) then
            location_type = 'Interior'
        else
            location_type = 'Exterior'
        end if
        write (iofilo,5010) itarg, targptr%id, roomptr%id, (targptr%center(j),j=1,3), location_type, &
            (targptr%normal(j),j=1,3), targptr%depth_loc,  targptr%thickness, trim(targptr%material)
5010    format(i5,3x,a15,4x,a14,4x,3(f7.2,2x),3x,a8,4x,3(f7.2,2x),f8.3,11x,f8.3,6x,a)
    end do
    return
    end subroutine output_initial_targets

! --------------------------- output_initial_detectors -------------------------------------------

!> \brief   output initial test case target specifications

    subroutine output_initial_detectors

    integer :: idtect, iroom, itype
    character(len=200) :: outbuf

    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    if (n_detectors/=0) write (iofilo,5000)
    5000 format(//'DETECTORS/ALARMS/SPRINKLERS',/ &
         ,'Target  Compartment        Type           Position (x, y, z)            Activation     Flaming        Smoldering',/ &
         ,'                                                                        Obscuration    Obscuration    Obscuration    ', &
         'Temperature   RTI           Spray Density',/ &
         ,'                                         (m)      (m)      (m)          (%/m)          (%/m)          (%/m)        ', &
         '  (C)           (m s)^1/2     (mm/s)',/ &
         ,160('-'))

    do idtect = 1, n_detectors
        dtectptr => detectorinfo(idtect)
        iroom = dtectptr%room
        roomptr => roominfo(iroom)
        itype = dtectptr%dtype
        if (itype==smoked) then
            if (dtectptr%dual_detector) then
                write (outbuf,5015) idtect, roomptr%id, 'SMOKE ', dtectptr%center(1:3), dtectptr%trigger, dtectptr%trigger_smolder
            else
                write (outbuf,5010) idtect, roomptr%id, 'SMOKE ', dtectptr%center(1:3), dtectptr%trigger
            end if
        else if (itype==heatd) then
            write (outbuf,5020) idtect, roomptr%id, 'HEAT  ', dtectptr%center(1:3), dtectptr%trigger-kelvin_c_offset, dtectptr%rti
        else
            write (outbuf,5020) idtect, roomptr%id, 'SPRINK', dtectptr%center(1:3), &
                dtectptr%trigger-kelvin_c_offset, dtectptr%rti, dtectptr%spray_density
        end if
5010    format(i3,5x,a14,5x,a6,4x,3(f7.2,2x),3x,f10.2)
5015    format(i3,5x,a14,5x,a6,4x,3(f7.2,2x),18x,f10.2,5x,f10.2)
5020    format(i3,5x,a14,5x,a6,4x,3(f7.2,2x),45x,2(5x,f10.2),5x,1pe10.2)

        write (iofilo,'(a)') trim(outbuf)
    end do
    return
    end subroutine output_initial_detectors

! --------------------------- flwout -------------------------------------------

!> \brief   format the flow output with appropriate precision eliminating appropriate zeroes
    
!> \param   outbuf (output): text string of formatted flow outputs
!> \param   flow1 (input): upper layer flow into room 1
!> \param   flow2 (input): upper layer flow out of room 1
!> \param   flow3 (input): lower layer flow into room 1
!> \param   flow4 (input): lower layer flow out of room 1
!> \param   flow5 (input): upper layer flow into room 2
!> \param   flow6 (input): upper layer flow out of room 2
!> \param   flow7 (input): lower layer flow into room 2
!> \param   flow8 (input): lower layer flow out of room 2

    subroutine flwout (outbuf,flow1,flow2,flow3,flow4,flow5,flow6,flow7,flow8)

    real(eb), intent(in) :: flow1, flow2, flow3, flow4, flow5, flow6, flow7, flow8
    character(len=*), intent(out) :: outbuf

    real :: flow(8),  x1000,x100,x10,x1,x01

    integer :: i

    outbuf = ' '
    flow(1) = flow1
    flow(2) = flow2
    flow(3) = flow3
    flow(4) = flow4
    flow(5) = flow5
    flow(6) = flow6
    flow(7) = flow7
    flow(8) = flow8
    x1000 = 1000.0_eb
    x100 = 100.0_eb
    x10 = 10.0_eb
    x1 = 1.0_eb
    x01 = 0.1_eb
    do i = 1, 8
        if (flow(i)>=x1000) then
            write (outbuf(13*(i-1)+1:13*i),5000) flow(i)
        else if (flow(i)>=x100) then
            write (outbuf(13*(i-1)+1:13*i),5010) flow(i)
        else if (flow(i)>=x10) then
            write (outbuf(13*(i-1)+1:13*i),5020) flow(i)
        else if (flow(i)>=x1) then
            write (outbuf(13*(i-1)+1:13*i),5030) flow(i)
        else if (flow(i)>=x01) then
            write (outbuf(13*(i-1)+1:13*i),5040) flow(i)
        else
            write (outbuf(13*(i-1)+1:13*i),5000) flow(i)
        end if
        if (flow(i)<=atol) outbuf(13*(i-1)+1:13*i) = ' '
        if (validation_flag.and.flow(i).ne.0.0_eb) write (outbuf(13*(i-1)+1:13*i),5050) flow(i)
    end do
    return

5000 format (2x,1pg11.3)
5010 format (f6.0,7x)
5020 format (f7.1,6x)
5030 format (f8.2,5x)
5040 format (f9.3,4x)
5050 format (2x,e11.4)
    end subroutine flwout

! --------------------------- write_error_component -------------------------------------------

!> \brief   write most important variable causing solution failure
    
!> \param   equation_pointer (input): pointer into P array of variable with the highest error when DASSL solution fails

    subroutine write_error_component (equation_pointer)

    integer, intent(in) :: equation_pointer

    integer :: itmp, irm, iw

    write (*,'(a)')'Solution component with the greatest error is'
    if (equation_pointer<=nofp+n_rooms) then
        write (*,'(a,i2)')' pressure in room ',equation_pointer
    else if (equation_pointer<=noftu) then
        write (*,'(a,i2)')' either hvac or fsm ',equation_pointer-n_rooms
    else if (equation_pointer<=nofvu) then
        write (*,'(a,i2)')' upper layer temp in room ',equation_pointer-noftu
    else if (equation_pointer<=noftl) then
        write (*,'(a,i2)')' upper layer vol in room ',equation_pointer-nofvu
    else if (equation_pointer<=noftl+n_rooms) then
        write (*,'(a,i2)')' lower layer temp in room ',equation_pointer-noftl
    else if (equation_pointer<=nofwt) then
        if (option(foxygen)==on) then
            write (*,'(a,i2)')' oxygen component ',equation_pointer-nofoxyl
        else
            write (*,'(a,i2)')' target number ',equation_pointer
        end if
    else if (equation_pointer<=nofprd) then
        itmp = equation_pointer - nofwt
        irm = surface_connections(itmp,w_from_room)
        iw = surface_connections(itmp,w_from_wall)
        if (iw==1) then
            write (*,'(a18,i2,a9,i1)') ' wall temp in room ',irm,' ceiling '
        else if (iw==2) then
            write (*,'(a18,i2,a9,i1)') ' wall temp in room ',irm,' floor   '
        else if (iw==3) then
            write (*,'(a18,i2,a12,i1)') ' wall temp in room ',irm,' upper wall '
        else if (iw==4) then
            write (*,'(a18,i2,a12,i1)') ' wall temp in room ',irm,' lower wall '
        end if
    end if

    return
    end subroutine write_error_component

! --------------------------- output_debug -------------------------------------------

!> \brief   output requested details of model calculations at the current time
    
!> \param   ikey (input): function key pressed
!> \param   t (input): current simulation time (s)
!> \param   dt (input): current time step (s)
!> \param   ieqmax (input):  number of equations in solution vector

    subroutine output_debug (ikey,t,dt,ieqmax)

    integer, intent(in) :: ikey, ieqmax
    real(eb), intent(in) :: t, dt

    real(eb) :: xqf
    integer :: i, iprod, il, iroom, iobj, itarg
    integer(2) :: ch, hit
    character(len=5) :: spname(ns_mass+4) = (/'  N2%', '  O2%', ' CO2%', '  CO%', ' HCN%', ' HCL%','  TUH', ' H2O%',&
       '   OD', ' OD_f', ' OD_s', '   CT', '   TS'/)
    character(len=3) :: ccc

    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr
    type(detector_type), pointer :: dtectptr

    type(target_type), pointer :: targptr

    if (ikey==1) then
        write (*,*) 'Pause at time = ', T,',  Press any key to continue'
40      call grabky(ch,hit)
        if (hit==0) go to 40
        write (*,*) 'Continuing'
        write (*,*)
    else if (ikey==2) then
        write (iofilo,5000) t, dt
        do i = 1, n_rooms
            roomptr => roominfo(i)
            write (*,5010) i
            write (*,5020) '   Upper temp(K)', roomptr%temp(u)
            write (*,5020) '   Lower temp(K)', roomptr%temp(l)
            write (*,5020) ' Interface ht(m)', roomptr%depth(l)
            write (*,5020) '   Pressure (pa)', roomptr%relp
            if (ns>0) write (*,*) ' Species mass fractions ',' Upper           Lower'
            do iprod = 1, ns_mass+4
                write (*,5030) spname(iprod), (roomptr%species_fraction(il,iprod),il= u,l)
            end do
            if (n_cons/=0) write (*,*) ' Wall temperatures'
            if (roomptr%surface_on(1)) then
                write (*,5040) roomptr%t_surfaces(1,1)
            end if
            if (roomptr%surface_on(3)) then
                write (*,5060) roomptr%t_surfaces(1,3)
            end if
            if (roomptr%surface_on(4)) then
                write (*,5070) roomptr%t_surfaces(1,4)
            end if
            if (roomptr%surface_on(2)) then
                write (*,5050) roomptr%t_surfaces(1,2)
            end if
        end do
        write (*,*) ' '
        if (n_detectors/=0) then
            write (*,*)'Detector info'
            write (*,100)
100         format('  #  ',3X,'D temp',6X,'J temp',6X,' Act')
            do i = 1, n_detectors
                dtectptr => detectorinfo(i)
                iroom = dtectptr%room
                roomptr => roominfo(iroom)
                if (roomptr%sprinkler_activated==i) then
                    ccc='***'
                else
                    ccc = '   '
                end if
                write (*,102)i,dtectptr%value,dtectptr%temp_gas,dtectptr%velocity,dtectptr%activation_time,ccc
102             format(1x,i2,1x,4(e11.4,1x),a3)
            end do
        end if
        write (*,*) ' '
    else if (ikey==3) then
        write (*,5090) t, dt
        call write_error_component (ieqmax)
        write (*,6030)
        do iroom = 1, n_rooms
            roomptr => roominfo(iroom)
            write (*,6000) iroom, roomptr%relp, roomptr%depth(l), roomptr%temp(l), roomptr%temp(u), &
               roomptr%species_fraction(l,2), roomptr%species_fraction(u,2)
        end do
        write (*,6070)
        do iroom = 1, n_rooms
            roomptr => roominfo(iroom)
            xqf = 0.
            do iobj = 1, n_fires
                fireptr => fireinfo(iobj)
                if (iroom==fireptr%room) xqf = xqf + fireptr%qdot_actual
            end do
            xqf = xqf + roomptr%qdot_doorjet
            write (*,6060) iroom, roomptr%t_surfaces(interior,1), roomptr%t_surfaces(interior,3), &
                roomptr%t_surfaces(interior,4), roomptr%t_surfaces(interior,2), xqf
        end do
        if (n_fires>0) then
            write (*,6080)
            do iobj = 1, n_fires
                fireptr => fireinfo(iobj)
                write (*,6085) iobj, fireptr%qdot_layers(l), fireptr%qdot_layers(u)
            end do
        end if
        if (n_targets>0) then
            write (*,6090)
            do itarg = 1, n_targets
                targptr => targetinfo(itarg)
                write (*,6095) itarg, targptr%temperature(idx_tempf_trg)
            end do
        end if
    end if
    return

5000 format (' T = ',1pg12.4,' DT = ',1pg12.4)
5010 format (' For room ',i3,' at time      T ')
5020 format (a16,5x,e14.7,3x,e14.7)
5030 format (15x,a5,1x,2(e14.7,3x))
5040 format ('  Ceiling temp(K) ',f12.2)
5050 format ('  Floor   temp(K) ',f12.2)
5060 format ('  Up wall temp(K) ',f12.2)
5070 format (' Low wall temp(K) ',e12.2)
5080 format (' from ',I2,' pressure ',e10.3,' to ',i2,' pressure ',g10.3,' mass flow is ',g10.3,' temp ',g10.3)
5090 format (' Returned from dassl at T = ',1pg14.6,',  dt = ',1pg12.4)
6000 format (1x,i3,1x,6e13.6)
6010 format (' HVAC pressures:',4e13.6)
6020 format (' HVAC temperatues:',4E13.6)
6030 format (t2,'Room',t9,'Pressure',t20,'Layer height',t35,'L. temp',t48,'U. temp',t62,'L. oxy',t75,'U. oxy')
6040 format (t3,'Nodes',t12,'Delta p',t23,'P at first node',t39,'P at 2nd node',t57,'Height')
6050 format (1x,2i3,1x,4(e13.6,2x))
6060 format (1x,i3,1x,5e13.6)
6070 format (t2,'Room',t11,'Ceiling',t21,'Upper Wall',t36,'Lower Wall',t49,'Floor',t61,'Fire Size')
6080 format (t2,'Object',t11,'Heat in lower ',t26,'Heat in upper')
6085 format (1x,i2,4x,2e13.6)
6090 format(t2,'Target',t11,'Temp')
6095 format(1x,i2,4x,e13.6)

    end subroutine output_debug

    subroutine output_status (T, dT)

    !  Write the status information to the "statusfile"

    real(eb), intent(in) :: T, dT
    rewind (unit=iofilstat)
    write (iofilstat,5001,err=10) t, dt, total_steps
    call results_compressed (iofilstat)
10  continue
    return

5001 FORMAT('Status at T = ',1pg11.2, ' DT = ',g11.3, ' STEPS = ',i0)
    end subroutine output_status
    
! --------------------------- open_output_files -------------------------------------------

!> \brief   open the appropriate files

    subroutine open_output_files

    !	Unit numbers are automatically defined

    !	Unit numbers defined for various I/O purposes
    !
    !     iofili        solver.ini and data files (data file, tpp and objects)
    !     iofill        log file
    !     iofilo        output 
    !     iofilstat     write the status file
    !     iofilsmv      smokeview output (header) - note this is rewound each time the plot data is written)
    !     iofilsmvplt   smokeview output (plot data)
    !     iofilsmvzone  smokeview spreadsheet output 
    !     iofilssdiag      spreadsheet output (various diagnostics for verification)
    !     ioresid       diagnostic file of solution vector
    !     ioslab        diagnostic file of flow slabs
    !     iofilcalc     spreadsheet output (post-run calculations)
    
    ! other units may be opened with newunit keyword in open statement

    !!!! Note that we assume that the default carriage control for formatted files is of type LIST (no fortran controls)
    integer :: ios

    ! first the file for "printed" output
    iofilo = get_filenumber()
    open (iofilo,file=outputfile,status='new')
    if (outputformat==0) outputformat = 2

    ! the status files
    iofilstat = get_filenumber()
    open (iofilstat,file=statusfile,access='append',err=81,iostat=ios)

    ! the smokeview files
    if (smv_out_interval>0) then
        iofilsmv = get_filenumber()
        open (iofilsmv,file=smvhead,form='formatted',err=11,iostat=ios)
        iofilsmvplt = get_filenumber()
        open (iofilsmvplt,file=smvdata,form="unformatted",err=11,iostat=ios)
        iofilsmvzone = get_filenumber()
        open (iofilsmvzone, file=smvcsv,form='formatted')
    end if

    ! the spread sheet files
    if (ss_out_interval>0) then
        iofilssc = get_filenumber()
        iofilssd = get_filenumber()
        iofilssm = get_filenumber()
        iofilssv = get_filenumber()
        iofilssw = get_filenumber()
        if (ssoutoptions(ichar('C')-ichar('A')+1)>0) open(iofilssc, file=sscompartment,form='formatted')
        iocsv(iocsv_compartments) = iofilssc
        if (ssoutoptions(ichar('D')-ichar('A')+1)>0) open(iofilssd, file=ssdevice,form='formatted')
        iocsv(iocsv_devices) = iofilssd
        if (ssoutoptions(ichar('M')-ichar('A')+1)>0) open(iofilssm, file=ssmasses,form='formatted')
        iocsv(iocsv_masses) = iofilssm
        if (ssoutoptions(ichar('V')-ichar('A')+1)>0) open(iofilssv, file=ssvent,form='formatted')
        iocsv(iocsv_vents) = iofilssv
        if (ssoutoptions(ichar('W')-ichar('A')+1)>0) open(iofilssw, file=sswall,form='formatted')
        iocsv(iocsv_walls) = iofilssw
        
        if (n_dumps/=0) then
           iofilcalc = get_filenumber()
            open (iofilcalc, file=sscalculation,form='formatted')
        end if
        
        if (radi_verification_flag) then
            iofilssdiag = get_filenumber()
            open (iofilssdiag, file=ssdiag,form='formatted')
        end if
    end if

    return

    ! error processing

    !	smokeview file
11  write (errormessage,5040) modulo(ios,256),trim(smvhead),trim(smvdata)
    call cfastexit('open_output_files',1)
    !	this one comes from writing to the status file
81  write (errormessage,*) '***Error opening the status file ',ios
    call cfastexit('open_output_files',2)

5040 FORMAT ('***Error ',i0,' while processing smokeview files -',i0,2x,a,2x,a)

    end subroutine open_output_files

end module output_routines
