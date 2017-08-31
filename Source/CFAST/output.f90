module output_routines

    use fire_routines, only : flame_height
    use target_routines, only: get_target_temperatures
    use utility_routines, only: xerror, doesthefileexist, funit
    use opening_fractions, only: find_vent_opening_ramp

    use cenviro
    use setup_data
    use solver_data
    use detectorptrs
    use cparams
    use target_data
    use fire_data
    use option_data
    use thermal_data
    use vent_data
    use wallptrs
    use room_data
    use ramp_data

    implicit none

    character(lbufln) :: lbuf, cbuf

    private

    public output_version, output_initial_conditions, output_results, deleteoutputfiles, openoutputfiles, &
        output_status, output_debug, find_error_component

    contains

! --------------------------- output_version -------------------------------------------

    subroutine output_version (iunit)

    !	a routine to put the header information in the output file.
    !	we assume the file is open

    integer, intent(in) :: iunit
    integer imajor, iminor, iminorrev
    character(256) :: revision, revision_date, compile_date

    call get_info(revision, revision_date, compile_date)

    call splitversion(version,imajor,iminor,iminorrev)

    write (iunit,'(/A/)')                    'CFAST'
#ifndef VERSION_PP
#define VERSION_PP "Test Version     :"
#endif
    write (iunit,'(a,a,i0,".",i0,".",i0)')     TRIM(VERSION_PP),' CFAST ',imajor, iminor, iminorrev
    write (iunit,'(A,A)')                    'Revision         : ',TRIM(revision)
    write (iunit,'(A,A)')                    'Revision Date    : ',TRIM(revision_date)
    write (iunit,'(A,A/)')                   'Compilation Date : ',TRIM(compile_date)
    return

    end subroutine output_version

! --------------------------- splitversion -------------------------------------------

    subroutine splitversion (version,imajor,iminor,iminorrev)

    integer, intent(in) :: version
    integer, intent(out) :: imajor,iminor,iminorrev

    if (version>=1000) then
        imajor = version/1000
        iminor = mod(version,1000)/100
        iminorrev = mod(version,100)
    else
        imajor = version/100
        iminor = mod(version,100)/10
        iminorrev = mod(version,10)
    end if
    return

    end subroutine splitversion

! --------------------------- output_initial_conditions -------------------------------------------

    subroutine output_initial_conditions

    !     Description:  Output initial test case description

    call output_version (iofilo)

    write (iofilo,5000) trim(inputfile), trim(title)
    if (outputformat>1) then
        call output_initial_overview
        call output_initial_ambient_conditions
        call output_initial_compartments
        call output_initial_vents
        call output_initial_thermal_properties
        call output_initial_fires
        call output_initial_targets
        call output_initial_detectors
    end if

    return

5000 format ('Data file: ',a,/,'Title: ',a)
    end subroutine output_initial_conditions

! --------------------------- output_results -------------------------------------------

    subroutine output_results(time)

    !     Description:  Output the results of the simulation at the current time
    !                results_layers is the basic environment
    !                results_fires information on fires
    !                results_targets targets and walls - temperature, radiation and convective flux
    !                results_detectors sprinkler and detector information
    !                RSLTHALL track the nose of the gravity wave
    !                results_species species

    !     Arguments: TIME  Current time (s)

    real(eb), intent(in) :: time

    write (iofilo,5000) time
    if (outputformat>1) then
        call results_layers
        call results_fires
        call results_targets(1)
        call results_detectors
        call results_species
        call results_vent_flows
    else if (outputformat==1) then
        call results_compressed (iofilo)
    end if
    return

5000 format (//,'Time = ',f8.1,' seconds.')
    end subroutine output_results

! --------------------------- results_layers -------------------------------------------

    subroutine results_layers

    !     Description:  Output the 2 layer environment at the current time

    integer :: icomp, izzvol
    type(room_type), pointer :: roomptr


    write (iofilo,5000)
    write (iofilo,5010)
    write (iofilo,5020)
    write (iofilo,5030)
    write (iofilo,5040)
    do icomp = 1, nrm1
        roomptr =>roominfo(icomp)
        izzvol = roomptr%volume(u)/roomptr%cvolume*100.0_eb+0.5_eb
        if (roomptr%shaft) then
            write (iofilo,5071) roomptr%name, roomptr%temp(u)-kelvin_c_offset, roomptr%volume(u), &
                roomptr%absorb(u),roomptr%relp - roomptr%interior_relp_initial
        else
            write (iofilo,5070) roomptr%name, roomptr%temp(u)-kelvin_c_offset, &
                roomptr%temp(l)-kelvin_c_offset, roomptr%depth(l), roomptr%volume(u), &
                izzvol, roomptr%absorb(u),roomptr%absorb(l), roomptr%relp - roomptr%interior_relp_initial
        end if
    end do
    return

5000 format (' ')
5010 format ('Compartment    Upper     Lower      Inter.      Upper           Upper      Lower       Pressure')
5020 format ('               Temp.     Temp       Height      Vol             Absor      Absorb')
5030 FORMAT ('               (C)       (C)        (m)         (m^3)           (m^-1)     (m^-1)      (Pa)')
5040 format (100('-'))
5070 format (a13,3(1pg11.4),1x,1pg9.2,'(',i3,'%) ',1pg10.3,1x,1pg10.3,3x,1pg10.3)
5071 format (a13,1pg11.4,11(' '),11(' '),1x,1pg9.2,7(' '),1pg10.3,1x,10(' '),3x,1pg10.3)
    end subroutine results_layers

! --------------------------- results_fires -------------------------------------------

    subroutine results_fires

    ! Output the fire environment at the current time

    integer i, icomp
    real(eb) :: fheight, xems, xemp, xqf, xqupr, xqlow
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    ! user-specified fires
    write (iofilo,5000)
    if (n_fires/=0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            if (fireptr%ignited) then
                call flame_height (fireptr%qdot_actual,fireptr%firearea,fheight)
                write (iofilo,5010) trim(fireptr%name), 'Y', fireptr%mdot_plume, fireptr%mdot_pyrolysis, fireptr%qdot_actual, &
                    fheight, fireptr%qdot_convective, fireptr%qdot_radiative, fireptr%total_pyrolysate, fireptr%total_trace
            else
                write (iofilo,5010) trim(fireptr%name), 'N'
            end if
        end do
    end if

    ! door jet fires
    write (iofilo,'(a)') ' '
    do icomp = 1, nr
        roomptr => roominfo(icomp)
        if (icomp<nr) then
            xems = 0.0_eb
            xemp = 0.0_eb
            xqf = 0.0_eb
            xqupr = 0.0_eb
            xqlow = 0.0_eb
            do i = 1, n_fires
                fireptr => fireinfo(i)
                if (icomp==fireptr%room) then
                    xems = xems + fireptr%mdot_plume
                    xemp = xemp + fireptr%mdot_pyrolysis
                    xqf = xqf + fireptr%qdot_actual
                    xqupr = xqupr + fireptr%qdot_layers(u)
                    xqlow = xqlow + fireptr%qdot_layers(l)
                end if
            end do
            xqf = xqf + roomptr%qdot_doorjet
            if (xems+xemp+xqf+xqupr+xqlow+roomptr%qdot_doorjet/=0.0_eb) write (iofilo,5030) roomptr%name, &
                xems, xemp, xqf, xqupr, xqlow, roomptr%qdot_doorjet
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
5030 format (a14,10x,3(1pg10.3),10x,3(1pg10.3))
5040 format ('Outside',76x,1pg10.3)
    end subroutine results_fires

! --------------------------- results_species -------------------------------------------

    subroutine results_species

    !     Description:  Output the layer and wall species at the current time

    character(10), dimension(ns) :: stype = &
        (/character(10) :: 'N2', 'O2', 'CO2', 'CO', 'HCN', 'HCL', 'TUHC', 'H2O','OD', 'CT', ' TS'/)
    character(10), dimension(ns) :: sunits = &
        (/character(10) :: '(%)', '(%)', '(%)', '(%)', '(%)', '(%)', '(%)', '(%)', '(1/m)', '(g-min/m3)', ' kg '/)
    character(5), dimension(2) :: lnames = (/character(5) :: 'UPPER', 'LOWER'/)
    character :: ciout*255, cjout*255
    integer :: i, icomp, layer, ic, lsp
    type(room_type), pointer :: roomptr

    if (ns/=0) then

        do layer = u, l
            write (iofilo,5050) lnames(layer)
            ciout = 'Compartment'
            cjout = ' '
            ic = 16
            do lsp = 1, ns
                if (lsp/=10) then
                    write (ciout(ic:ic+9),5000) stype(lsp)
                    write (cjout(ic:ic+9),5000) sunits(lsp)
                    ic = ic + 11
                end if
            end do
            write (iofilo,5020) ciout(1:len_trim(ciout))
            write (iofilo,5020) cjout(1:len_trim(cjout))
            write (iofilo,5030) ('-',i = 1,ic)
            write (ciout,5010)
            do icomp = 1, nrm1
                roomptr => roominfo(icomp)
                write (ciout,5060) roomptr%name
                ic = 14
                if (layer==u.or..not.roomptr%shaft) then
                    do lsp = 1, ns
                        if (lsp/=10) then
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

    subroutine results_vent_flows ()

    !     Description:  Output the vent flow at the current time

    integer :: i, ifrom, ito
    real(eb), dimension(8) :: flow

    character outbuf*132, cifrom*12, cito*12
    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    write (iofilo,5000)

    ! horizontal flow natural vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)
        ifrom = ventptr%room1
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%name
        if (ifrom==nr) cifrom = 'Outside'
        ito = ventptr%room2
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%name
        if (ito==nr) cito = 'Outside'
        call flwout(outbuf,ventptr%h_mflow(1,1,1),ventptr%h_mflow(1,1,2),ventptr%h_mflow(1,2,1),ventptr%h_mflow(1,2,2),&
           ventptr%h_mflow(2,1,1),ventptr%h_mflow(2,1,2),ventptr%h_mflow(2,2,1),ventptr%h_mflow(2,2,2))
        write (iofilo,5010) 'H', i, cifrom, cito, outbuf
    end do

    ! vertical flow natural vents
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        ifrom = ventptr%room2
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%name
        if (ifrom==nr) cifrom = 'Outside'
        ito = ventptr%room1
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%name
        if (ito==nr) cito = 'Outside'
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
        write (iofilo,5010) 'V', i, cifrom, cito, outbuf
    end do

    ! mechanical vents
    do i = 1, n_mvents
        ventptr => mventinfo(i)
        ifrom = ventptr%room1
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%name
        if (ifrom==nr) cifrom = 'Outside'
        ito = ventptr%room2
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%name
        if (ito==nr) cito = 'Outside'
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
        write (iofilo,5010) 'M', i, cifrom, cito, outbuf
    end do

    ! Total mass flowing through mechanical vents up to current time
    write (iofilo,5030)
    
    do i = 1, n_mvents
        ventptr => mventinfo(i)
        ifrom = ventptr%room1
        roomptr => roominfo(ifrom)
        write (cifrom,'(a12)') roomptr%name
        if (ifrom==nr) cifrom = 'Outside'
        ito = ventptr%room2
        roomptr => roominfo(ito)
        write (cito,'(a12)') roomptr%name
        if (ito==nr) cito = 'Outside'
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

5000 format (//,'FLOW THROUGH VENTS (kg/s)',//, &
    '                                       Flow relative to ''From''                             Flow Relative to ''To''',/ &
    '                                      Upper Layer               Lower Layer               Upper Layer',&
    '               Lower Layer',/, &
    'Vent   From/Bottom    To/Top           Inflow       Outflow      Inflow       Outflow      Inflow',&
    '       Outflow      Inflow       Outflow',/,137('-'))
5010 format (a1,i3,3x,a12,3x,a12,1x,a)
5030 format (//,'TOTAL MASS FLOW THROUGH MECHANICAL VENTS (kg)',//, &
    'To             Through        ','      Upper Layer           ','    Lower Layer           ','   Trace Species',/, &
    'Compartment    Vent             ',2('Inflow       Outflow      '),' Vented ', '   Filtered',/, 104('-'))
5040 format (' ')
5050 format (a14,1x,a12,1x,a)

    end subroutine results_vent_flows

! --------------------------- results_compressed -------------------------------------------

    subroutine results_compressed (iounit)

    !     Description:  Output a compressed output for 80 column screens

    integer, intent(in) :: iounit

    integer :: i, ir
    real(eb) :: xemp, xqf
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    write (iounit,5000)
    write (iounit,5010)
    do ir = 1, nr
        roomptr => roominfo(ir)
        if (ir<nr) then
            xemp = 0.0_eb
            xqf = 0.0_eb
            do i = 1, n_fires
                fireptr => fireinfo(i)
                if (ir==fireptr%room) then
                    xemp = xemp + fireptr%mdot_pyrolysis
                    xqf = xqf + fireptr%qdot_actual
                end if
            end do
            xqf = xqf + roomptr%qdot_doorjet
            if (roomptr%shaft) then
                write (iounit,5040) ir, roomptr%temp(u)-kelvin_c_offset, xemp, xqf, &
                    roomptr%relp - roomptr%interior_relp_initial
            else
                write (iounit,5030) ir, roomptr%temp(u)-kelvin_c_offset, roomptr%temp(l)-kelvin_c_offset, &
                    roomptr%depth(l), xemp, xqf, roomptr%relp - roomptr%interior_relp_initial
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

    subroutine results_targets (itprt)

    !     description:  output the temperatures and fluxes on surfaces and targets at the current time
    !                itprt 1 if target printout specifically called for, 0 otherwise

    integer, intent(in) :: itprt

    integer :: i, iw, itarg
    real(eb) :: itotal, total, tgtemp, tttemp, tctemp, gasfed, heatfed

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr

    integer, parameter :: iwptr(4) =(/1, 3, 4, 2/)

    if ((itprt==0.and.n_targets<=nrm1).or.n_targets==0) return
    write (iofilo,5000)

    call get_target_temperatures

    do i=1,nrm1
        roomptr => roominfo(i)
        write (iofilo,5010) roomptr%name, (roomptr%t_surfaces(1,iwptr(iw))-kelvin_c_offset,iw=1,4)

        do itarg = 1, n_targets
            targptr => targetinfo(itarg)
            if (targptr%room==i) then
                tgtemp = targptr%tgas
                tttemp = targptr%tfront
                tctemp = targptr%tinternal
                gasfed = targptr%fed_gas
                heatfed = targptr%fed_heat
                if (validate.or.netheatflux) then
                    itotal = targptr%flux_incident_front
                    total = targptr%flux_net_gauge(1)
                else
                    itotal = targptr%flux_incident_front
                    total = targptr%flux_net(1)
                end if
                if (abs(itotal)<=1.0e-10_eb) itotal = 0.0_eb
                if (abs(total)<=1.0e-10_eb) total = 0.0_eb
                if (total/=0.0_eb) then
                    write (iofilo,5020) targptr%name, tgtemp-kelvin_c_offset, tttemp-kelvin_c_offset, &
                        tctemp-kelvin_c_offset, itotal, total, gasfed, heatfed
                elseif (itotal/=0.0_eb) then
                    write (iofilo,5030) targptr%name, tgtemp-kelvin_c_offset, tttemp-kelvin_c_offset, tctemp-kelvin_c_offset, &
                        itotal, gasfed,heatfed
                else
                    write (iofilo,5040) targptr%name, tgtemp-kelvin_c_offset, tttemp-kelvin_c_offset, tctemp-kelvin_c_offset, gasfed, heatfed
                end if
            end if
        end do

    end do
    return
5000 format (//,'SURFACES AND TARGETS',//, &
    'Compartment    Ceiling   Up wall   Low wall  Floor    Target        Gas       Surface   Interior Incident     Net          Gas         Heat',/, &
    '               Temp.     Temp.     Temp.     Temp.                  Temp.     Temp.     Temp.    Flux         Flux         FED         FED',/, &
    '               (C)       (C)       (C)       (C)                    (C)       (C)       (C)      (W/m^2)      (W/m^2)',/,172('-'))
5010 format (a14,4(1pg10.3))
5020 format (54x,a8,4x,3(1pg10.3),1x,6(1pg10.3,3x))
5030 format (54x,a8,4x,3(1pg10.3),15x,3(1pg10.3,3x))
5040 format (54x,a8,4x,3(1pg10.3),25x,2(1pg10.3,3x))  
    end subroutine results_targets

! --------------------------- results_detectors -------------------------------------------

    subroutine results_detectors

    !     Description:  Output the conditions of and at a sprinkler location (temperature, velocities etc) at the current time

    integer :: i, iroom, itype
    real(eb) :: cjetmin, zdetect, tlay, tjet, vel, obs, tlink

    character(3) :: cact
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    if (n_detectors==0)return
    write (iofilo,5000)
    cjetmin = 0.10_eb
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
        vel = max(dtectptr%velocity,cjetmin)
        obs = dtectptr%obscuration
        tlink =  dtectptr%value-kelvin_c_offset

        cact = 'NO'
        if (dtectptr%activated) cact = 'YES'

        itype = dtectptr%dtype
        if (itype==smoked) then
            write (iofilo,5010) i, roomptr%name, 'SMOKE ', tjet, vel, obs, cact
        else if (itype==heatd) then
            write (iofilo,5020) i, roomptr%name, 'HEAT  ', tlink, tjet, cact
        else
            write (iofilo,5030) i, roomptr%name, 'SPRINK', tlink, tjet, vel, cact
        end if

5000 format(//'DETECTORS/ALARMS/SPRINKLERS',/, &
    '                                      Sensor         Smoke',//, &
    'Number  Compartment        Type       Temp (C)       Temp (C)      Vel (m/s)     Obs (1/m)          Activated',/, &
    '--------------------------------------------------------------------------------------------------------------')
5010    format(i3,5x,a14,5x,a6,4x,15x,1pe10.3,4x,1pe10.3,4x,1pe10.3,10x,a3)
5020    format(i3,5x,a14,5x,a6,4x,1pe10.3,5x,1pe10.3,38x,a3)
5030    format(i3,5x,a14,5x,a6,4x,1pe10.3,5x,1pe10.3,4x,1pe10.3,24x,a3)
    end do
    return
    end subroutine results_detectors

! --------------------------- output_initial_overview -------------------------------------------

    subroutine output_initial_overview

    !     description:  output initial test case overview

    write (iofilo,5000)
    write (iofilo,5010) nrm1, n_hvents, n_vvents, n_mvents
    write (iofilo,5020) time_end, print_out_interval, smv_out_interval, ss_out_interval

5000 format (//,'OVERVIEW',/)
5010 FORMAT (/,'Compartments    Doors, ...    Ceil. Vents, ...    MV Connects',/,61('-'),/,i4,12x,i4,10x,i4,17x,i4)
5020 format (/,'Simulation     Output         Smokeview      Spreadsheet',/, &
             'Time           Interval       Interval       Interval',/, &
             '   (s)            (s)            (s)            (s)',/,56('-'),/,i6,6x,3(i6,9x))
    end subroutine output_initial_overview

! --------------------------- output_initial_ambient_conditions -------------------------------------------

    subroutine output_initial_ambient_conditions

    !     Description:  Output initial test case ambient conditions

    write (iofilo,5000) interior_temperature-kelvin_c_offset, interior_abs_pressure + pressure_offset, &
       exterior_temperature-kelvin_c_offset, exterior_abs_pressure + pressure_offset
    return

5000 format (//,'AMBIENT CONDITIONS',//, &
    'Interior       Interior       Exterior       Exterior',/, &
    'Temperature    Pressure       Temperature    Pressure',/, &
    '  (C)            (Pa)           (C)            (Pa)', &
    /,53('-'),/,2(f7.0,8x,f9.0,6x))

    end subroutine output_initial_ambient_conditions

! --------------------------- output_initial_compartments -------------------------------------------

    subroutine output_initial_compartments

    !     Description:  Output initial test case geometry

    integer i
    type(room_type), pointer :: roomptr

    write (iofilo,5000)
    do i = 1, nrm1
        roomptr => roominfo(i)
        write (iofilo,5010) i, trim(roomptr%name), roomptr%cwidth, roomptr%cdepth, roomptr%cheight, roomptr%z0, roomptr%z1
    end do
    return
5000 format (//,'COMPARTMENTS',//, &
    'Compartment  Name                Width        Depth        Height       Floor        Ceiling   ',/, &
    '                                                                        Height       Height    ',/, &
    33x,5('(m)',10x),/,96('-'))
5010 format (i5,8x,a13,5(f12.2,1x))
    end subroutine output_initial_compartments

! --------------------------- output_initial_vents -------------------------------------------

    subroutine output_initial_vents

    !     Description:  Output initial test case vent connections

    integer :: i, j, iramp
    character :: ciout*14, cjout*14, csout*6, crout*10, ctrigger*4
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: ventptr
    type(ramp_type), pointer :: rampptr
    type(target_type), pointer :: targptr

    !     horizontal flow vents
    if (n_hvents==0) then
        write (iofilo,5000)
    else
        write (iofilo,5010)
        do i = 1, n_hvents
            ventptr => hventinfo(i)
            roomptr => roominfo(ventptr%room2)
            write (cjout,'(a14)') roomptr%name
            if (ventptr%room2==nr) cjout = 'Outside'
            roomptr => roominfo(ventptr%room1)
            if (ventptr%opening_type==trigger_by_time) then
                ctrigger = 'Time'
                iramp = find_vent_opening_ramp('H',ventptr%room1,ventptr%room2,ventptr%counter)
                if (iramp==0) then
                    write (iofilo,5020) roomptr%name, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                        ctrigger, ventptr%opening_initial_time, ventptr%opening_initial_fraction, &
                        ventptr%opening_final_time, ventptr%opening_final_fraction
                else
                    write (crout,'(a6,1x,i0)') 'RAMP #',iramp
                    write (iofilo,5020) roomptr%name, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, crout
                end if
            else if (ventptr%opening_type==trigger_by_temp) then
                ctrigger = 'Temp'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5025) roomptr%name, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                    ctrigger, ventptr%opening_criterion-273.15, targptr%name, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            else
                ctrigger = 'Flux'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5025) roomptr%name, cjout, ventptr%counter, ventptr%width, ventptr%sill, ventptr%soffit, &
                    ctrigger, ventptr%opening_criterion, targptr%name, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            end if
        end do
    end if
5000 format (//,'VENT CONNECTIONS',//,'There are no horizontal natural flow connections')
     5010 format (//,'VENT CONNECTIONS',//,'Horizontal Natural Flow Connections (Doors, Windows, ...)',//, &
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
            write (ciout,'(a14)') roomptr%name
            if (ventptr%room1==nr) ciout = 'Outside'
            roomptr => roominfo(ventptr%room2)
            write (cjout,'(a14)') roomptr%name
            if (ventptr%room2==nr) cjout = 'Outside'
            csout = 'Round'
            if (ventptr%shape==2) csout = 'Square'
            roomptr => roominfo(ventptr%room2)
            if (ventptr%opening_type==trigger_by_time) then
                ctrigger = 'Time'
                iramp = find_vent_opening_ramp('V',ventptr%room1,ventptr%room2,ventptr%counter)
                if (iramp==0) then
                    write (iofilo,5050) ciout, cjout, ventptr%counter, csout, ventptr%area, &
                        ctrigger, ventptr%opening_initial_time, ventptr%opening_initial_fraction, &
                        ventptr%opening_final_time, ventptr%opening_final_fraction
                else
                    write (crout,'(a6,1x,i0)') 'RAMP #',iramp
                    write (iofilo,5050) ciout, cjout, ventptr%counter, csout, ventptr%area, crout
                end if
            else if (ventptr%opening_type==trigger_by_temp) then
                ctrigger = 'Temp'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5055) ciout, cjout, ventptr%counter, csout, ventptr%area, &
                    ctrigger, ventptr%opening_criterion-273.15, targptr%name, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            else 
                ctrigger = 'Flux'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5055) ciout, cjout, ventptr%counter, csout, ventptr%area, &
                    ctrigger, ventptr%opening_criterion, targptr%name, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            end if
        end do
    end if
5030 format (//,'There are no vertical natural flow connections')
5040 format (//,'Vertical Natural Flow Connections (Ceiling, ...)',//,&
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
            write (ciout,'(a14)') roomptr%name
            if (ventptr%room1==nr) ciout = 'Outside'
            roomptr => roominfo(ventptr%room2)
            write (cjout,'(a14)') roomptr%name
            if (ventptr%room2==nr) cjout = 'Outside'
            if (ventptr%opening_type==trigger_by_time) then
                ctrigger = 'Time'
                iramp = find_vent_opening_ramp('M',ventptr%room1,ventptr%room2,ventptr%counter)
                if (iramp==0) then
                    write (iofilo,5130) ciout, cjout, ventptr%counter, ventptr%area, ventptr%coeff(1), &
                        ctrigger, ventptr%opening_initial_time, ventptr%opening_initial_fraction, &
                        ventptr%opening_final_time, ventptr%opening_final_fraction
                else
                    write (crout,'(a6,1x,i0)') 'RAMP #',iramp
                    write (iofilo,5130) ciout, cjout, ventptr%counter, ventptr%coeff(1), ventptr%area, crout
                end if
            else if (ventptr%opening_type==trigger_by_temp) then
                ctrigger = 'Temp'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5135) ciout, cjout, ventptr%counter, ventptr%area, ventptr%coeff(1), &
                    ctrigger, ventptr%opening_criterion-273.15, targptr%name, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            else 
                ctrigger = 'Flux'
                targptr => targetinfo(ventptr%opening_target)
                write (iofilo,5135) ciout, cjout, ventptr%counter, ventptr%area, ventptr%coeff(1), &
                    ctrigger, ventptr%opening_criterion, targptr%name, ventptr%opening_initial_fraction, &
                    ventptr%opening_final_fraction
            end if
        end do
    end if
5060 format (//,'There are no mechanical flow connections')
     5120 format (//,'FANS',//,&
        'From           To              Fan        Area      Flowrate     Open/Close  Trigger                 ', &
        'Initial     Initial     Final       Final',/, &
        'Compartment    Compartment     Number                            Type        Value       Target      ', &
        'Time        Fraction    Time        Fraction',/, &
        '                                          (m^2)     (m^3/s)                  (C/W/m^2)               ', &
        '(s)                     (s)',/,145('-'))
5130 format (a14,1x,a14,i3,7x,f7.2,3x,f7.2,9x,a,27x,4(f9.2,3x))
5135 format (a14,1x,a14,i3,7x,f7.2,3x,f7.2,9x,a,6x,f9.2,5x,a10,9x,2(f9.2,15x))  
    
    ! ramps
    if (nramps==0) then
        write (iofilo,5150)
    else
        write (iofilo,5160)
        do i = 1, nramps
            rampptr => rampinfo(i)
            roomptr => roominfo(rampptr%room2)
            write (cjout,'(a14)') roomptr%name
            if (rampptr%room2==nr) cjout = 'Outside'
            roomptr => roominfo(rampptr%room1)
            write (iofilo,5170) rampptr%type, roomptr%name, cjout, rampptr%counter, 'Time      ', &
                (int(rampptr%time(j)),j=1,rampptr%npoints)
            write (iofilo,5180) 'Fraction', (rampptr%value(j),j=1,rampptr%npoints)
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

    subroutine output_initial_thermal_properties

    !     description:  output initial test case thermal properties

    integer i, j
    type(room_type), pointer :: roomptr
    type(thermal_type), pointer :: thrmpptr

    ! check to see if any heat transfer is on
    if (.not.adiabatic_walls) then
        do i = 1, nrm1
            roomptr => roominfo(i)
            do j = 1, nwal
                if (roomptr%surface_on(j).and.roomptr%matl(j)/=' ') go to 30
            end do
        end do
    end if
    write (iofilo,5000)
    return

    ! some surfaces are on, do the printout of the surfaces
30  write (iofilo,5010)
    do  i = 1, nrm1
        roomptr => roominfo(i)
        write (iofilo,5020) roomptr%name, roomptr%matl(1), roomptr%matl(3), roomptr%matl(2)
    end do

    ! print out the properties of the materials used
    write (iofilo,5030)
    do i = 1, n_thrmp
        thrmpptr => thermalinfo(i)
        write (iofilo,5040) thrmpptr%name, thrmpptr%k(1), thrmpptr%c(1), thrmpptr%rho(1), thrmpptr%thickness(1), thrmpptr%eps
        do j = 2, thrmpptr%nslab
            write (iofilo,5050) thrmpptr%k(j), thrmpptr%c(j), thrmpptr%rho(j), thrmpptr%thickness(j)
        end do
    end do
    write (iofilo,5060)
    return

5000 format (//,'Heat transfer for all surfaces is turned off')
5010 format (//,'THERMAL PROPERTIES',//,'Compartment    Ceiling      Wall         Floor',/,47('-'))
5020 format (a13,3(a10,3x))
5030 format (//,'Name',4X,'Conductivity',6X,'Specific Heat',5X,&
          'Density',8X,'Thickness',5X,'Emissivity',/,83('-'))
5040 format (a8,5(1pg13.3,3x),5e10.2)
5050 format (8x,4(1pg13.3,3x))
5060 format (' ')

    end subroutine output_initial_thermal_properties

! --------------------------- output_initial_fires -------------------------------------------

    subroutine output_initial_fires

    !     routine: output_initial_fires
    !     purpose: This routine outputs the fire specification for all the object fires
    !     Arguments: none

    integer :: io, i, is
    real(eb) :: y_hcn, y_hcl

    character(13), dimension(0:4) :: ftype = &
        (/character(13) :: 'Undefined', 'Unconstrained', 'Constrained','Pool Fire', 'Furniture'/)
    character(6), dimension(1:3) :: fire_geometry = (/character(6) :: 'Normal', 'Wall', 'Corner'/)

    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    if (n_fires>0) then
        write (iofilo,5080)
        do io = 1, n_fires
            fireptr => fireinfo(io)
            roomptr => roominfo(fireptr%room)
            write (iofilo,5020) trim(fireptr%name), io, fire_geometry(fireptr%modified_plume)
            write (iofilo,5030) roomptr%name, ftype(fireptr%chemistry_type), fireptr%x_position, fireptr%y_position, &
                fireptr%z_position, relative_humidity*100., lower_o2_limit*100.,fireptr%chirad
            write (iofilo,5031) fireptr%n_C, fireptr%n_H, fireptr%n_O, fireptr%n_N, fireptr%n_Cl
            write (cbuf,5040)
            is = 103
            write (iofilo,'(a)') cbuf(1:len_trim(cbuf))
            write (iofilo,5000) ('(kg/kg)',i = 1,(is-51)/10)
            write (iofilo,5010) ('-',i = 1,is-1)
            do i = 1, fireptr%npoints
                write (cbuf,5060) fireptr%time(i), fireptr%mdot(i), fireptr%hoc(i), fireptr%qdot(i), fireptr%height(i)
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
          '   Position (x,y,z)     Relative    Lower O2    Radiative',/,52x,'Humidity    Limit       Fraction',/85('-'))
5030 format (a14,1x,a13,3(f7.2),f7.1,6x,f7.2,5x,f7.2//)
5031 format ('Chemical formula of the fuel',/,'Carbon     Hydrogen  Oxygen    Nitrogen  Chlorine',/,50('-'),/,5(f7.3,3x),//)
5040 format ('  Time      Mdot      Hcomb     Qdot      Zoffset   Soot      CO        HCN       HCl       TS')
5060 format (F7.0,3X,4(1PG10.2))
5070 format (10(1PG10.2),2x,2g10.2)
5080 format (/,'FIRES')
    end subroutine output_initial_fires

! --------------------------- output_initial_targets -------------------------------------------

    subroutine output_initial_targets

    !      description:  output initial test case target specifications

    integer :: itarg, j

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr

    if (n_targets/=0) write (iofilo,5000)
5000 format(//,'TARGETS',//,'Target',T29,'Compartment',T44,'Position (x, y, z)',T71,&
         'Direction (x, y, z)',T96,'Material',/,102('-'))

    do itarg = 1, n_targets
        targptr => targetinfo(itarg)
        roomptr => roominfo(targptr%room)
        write (iofilo,5010) itarg, targptr%name, roomptr%name, (targptr%center(j),j=1,3), &
            (targptr%normal(j),j=1,3), trim(targptr%material)
5010    format(i5,3x,a15,t31,a14,t41,6(f7.2,2x),t96,a)
    end do
    return
    end subroutine output_initial_targets

! --------------------------- output_initial_detectors -------------------------------------------

    subroutine output_initial_detectors

    !      description:  output initial test case target specifications

    integer :: idtect, iroom, itype
    character :: outbuf*132

    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    if (n_detectors/=0) write (iofilo,5000)
    5000 format(//'DETECTORS/ALARMS/SPRINKLERS',/ &
         ,'Target  Compartment        Type           Position (x, y, z)            Activation',/ &
         ,'                                                                        Obscuration    ', &
         'Temperature   RTI           Spray Density',/ &
         ,'                                         (m)      (m)      (m)          (%/m)       ', &
         '  (C)           (m s)^1/2     (m/s)',/ &
         ,128('-'))

    do idtect = 1, n_detectors
        dtectptr => detectorinfo(idtect)
        iroom = dtectptr%room
        roomptr => roominfo(iroom)
        itype = dtectptr%dtype
        if (itype==smoked) then
            write (outbuf,5010) idtect, roomptr%name, 'SMOKE ', dtectptr%center(1:3), dtectptr%trigger
        else if (itype==heatd) then
            write (outbuf,5020) idtect, roomptr%name, 'HEAT  ', dtectptr%center(1:3), dtectptr%trigger-273.15, dtectptr%rti
        else
            write (outbuf,5020) idtect, roomptr%name, 'SPRINK', dtectptr%center(1:3), &
                dtectptr%trigger-273.15, dtectptr%rti, dtectptr%spray_density
        end if
5010    format(i3,5x,a14,5x,a6,4x,3(f7.2,2x),3x,f10.2)
5020    format(i3,5x,a14,5x,a6,4x,3(f7.2,2x),13x,2(5x,f10.2),5x,1pe10.2)

        write (iofilo,'(a)') trim(outbuf)
    end do
    return
    end subroutine output_initial_detectors

! --------------------------- flwout -------------------------------------------

    subroutine flwout (outbuf,flow1,flow2,flow3,flow4,flow5,flow6,flow7,flow8)

    !     description:  stuff the flow output after blanking appropriate zeros

    real(eb), intent(in) :: flow1, flow2, flow3, flow4, flow5, flow6, flow7, flow8
    character, intent(out) :: outbuf*(*)

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
        if (validate.and.flow(i).ne.0.0_eb) write (outbuf(13*(i-1)+1:13*i),5050) flow(i)
    end do
    return

5000 format (2x,1pg11.3)
5010 format (f6.0,7x)
5020 format (f7.1,6x)
5030 format (f8.2,5x)
5040 format (f9.3,4x)
5050 format (2x,e11.4)
    end subroutine flwout


! --------------------------- find_error_component -------------------------------------------

    subroutine find_error_component (icomp)

    integer, intent(in) :: icomp

    integer :: itmp, irm, iw

    write (lbuf,*)'Solution component with the greatest error is'
    call xerror(lbuf,0,1,0)
    if (icomp<=nofp+nrm1) then
        write (lbuf,'(a,i2)')' pressure in room ',icomp
        call xerror(lbuf,0,1,0)
    else if (icomp<=noftu) then
        write (lbuf,'(a,i2)')' either hvac or fsm ',icomp-nrm1
        call xerror(lbuf,0,1,0)
    else if (icomp<=nofvu) then
        write (lbuf,'(a,i2)')' upper layer temp in room ',icomp-noftu
        call xerror(lbuf,0,1,0)
    else if (icomp<=noftl) then
        write (lbuf,'(a,i2)')' upper layer vol in room ',icomp-nofvu
        call xerror(lbuf,0,1,0)
    else if (icomp<=noftl+nrm1) then
        write (lbuf,'(a,i2)')' lower layer temp in room ',icomp-noftl
        call xerror(lbuf,0,1,0)
    else if (icomp<=nofwt) then
        if (option(foxygen)==on) then
            write (lbuf,'(a,i2)')' oxygen component ',icomp-nofoxyl
            call xerror(lbuf,0,1,0)
        else
            write (lbuf,'(a,i2)')' target number ',icomp
            call xerror(lbuf,0,1,0)
        end if
    else if (icomp<=nofprd) then
        itmp = icomp - nofwt
        irm = i_hconnections(itmp,w_from_room)
        iw = i_hconnections(itmp,w_from_wall)
        if (iw==1) then
            write (lbuf,'(a18,i2,a9,i1)') ' wall temp in room ',irm,' ceiling '
            call xerror(lbuf,0,1,0)
        else if (iw==2) then
            write (lbuf,'(a18,i2,a9,i1)') ' wall temp in room ',irm,' floor   '
            call xerror(lbuf,0,1,0)
        else if (iw==3) then
            write (lbuf,'(a18,i2,a12,i1)') ' wall temp in room ',irm,' upper wall '
            call xerror(lbuf,0,1,0)
        else if (iw==4) then
            write (lbuf,'(a18,i2,a12,i1)') ' wall temp in room ',irm,' lower wall '
            call xerror(lbuf,0,1,0)
        end if
    end if

    return
    end subroutine find_error_component

! --------------------------- output_debug -------------------------------------------

    subroutine output_debug (ikey,t,dt,ieqmax)

    integer, intent(in) :: ikey, ieqmax
    real(eb), intent(in) :: t, dt

    real(eb) :: xqf
    integer :: i, iprod, il, iroom, iobj, itarg
    integer(2) :: ch, hit
    character(5) :: spname(ns) = (/'  N2%', '  O2%', ' CO2%', '  CO%', ' HCN%', ' HCL%','  TUH', ' H2O%',&
       '   OD', '   CT', '   TS'/), ccc*3

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
        do i = 1, nrm1
            roomptr => roominfo(i)
            write (*,5010) i
            write (*,5020) '   Upper temp(K)', roomptr%temp(u)
            write (*,5020) '   Lower temp(K)', roomptr%temp(l)
            write (*,5020) ' Interface ht(m)', roomptr%depth(l)
            write (*,5020) '   Pressure (pa)', roomptr%relp
            if (ns>0) write (*,*) ' Species mass fractions ',' Upper           Lower'
            do iprod = 1, ns
                write (*,5030) spname(iprod), (roomptr%species_fraction(il,iprod),il= u,l)
            end do
            if (nhcons/=0) write (*,*) ' Wall temperatures'
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
100         format('  nr ',3X,'D temp',6X,'J temp',6X,' Act')
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
        call find_error_component (ieqmax)
        write (*,6030)
        do iroom = 1, nrm1
            roomptr => roominfo(iroom)
            write (*,6000) iroom, roomptr%relp, roomptr%depth(l), roomptr%temp(l), roomptr%temp(u), &
               roomptr%species_fraction(l,2), roomptr%species_fraction(u,2)
        end do
        write (*,6070)
        do iroom = 1, nrm1
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

    rewind (12)
    write (12,5001) t, dt
    call results_compressed (12)
    return

5001 FORMAT('Status at T = ',1PG11.2, ' DT = ',G11.3)
    end subroutine output_status

! --------------------------- openoutputfiles -------------------------------------------

    subroutine openoutputfiles

    !	Now that we know what output is needed, open the appropriate files
    !	Unit numbers defined here and readinputfiles

    !	Unit numbers defined in read_command_options, openoutputfiles, readinputfiles
    !
    !      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
    !      3 is for the log file  (iofill)
    !	   4 is for the indicator that the model is running (kernelisrunning)
    !      6 is output (IOFILO)
    !     11 is the history file
    !     12 is used to write the status file (project.status)
    !     13 smokeview output (header) - note this is rewound each time the plot data is written)
    !     14 smokeview output (plot data)
    !     15 smokeview spreadsheet output
    !     21 spreadsheet output (normal)
    !     22 spreadsheet output (flow field)
    !     23 spreadsheet output (species molar %, etc.)
    !     24 spreadsheet otuput (species mass)
    !     25 spreadsheet output (walls and targets)

    !!!! Note that we assume that the default carriage control for formatted files is of type LIST (no fortran controls)

    integer :: ios

    ! first the file for "printed" output
    open (unit=iofilo,file=outputfile,status='new')
    if (outputformat==0) outputformat = 2

    ! the status files
    open (12,file=statusfile,access='append',err=81,iostat=ios)
    open (unit=4, file=kernelisrunning)

    ! the smokeview files
    if (smv_out_interval>0) then
        open (unit=13,file=smvhead,form='formatted',err=11,iostat=ios)
        open (unit=14,file=smvdata,form="unformatted",err=11,iostat=ios)
        open (unit=15, file=smvcsv,form='formatted')
    end if

    ! the spread sheet files
    if (ss_out_interval>0) then
        open (unit=21, file=ssnormal,form='formatted')
        open (unit=22, file=ssflow,form='formatted')
        open (unit=23, file=ssspecies,form='formatted')
        open (unit=24, file=ssspeciesmass,form='formatted')
        open (unit=25, file=sswall,form='formatted')
    end if

    return

    ! error processing

    !	smokeview file
11  write (*,5040) mod(ios,256),trim(smvhead),trim(smvdata)
    write (iofill,5040) mod(ios,256),trim(smvhead),trim(smvdata)
    stop
    !	this one comes from writing to the status file
81  write (*,*) '***Fatal error writing to the status file ',ios
    write (iofill,*) '***Fatal error writing to the status file ',ios
    stop

5040 FORMAT ('***Error ',i4,' while processing smokeview files -',i3,2x,a,2x,a)

    end subroutine openoutputfiles

! --------------------------- deleteoutputfiles -------------------------------------------

    subroutine deleteoutputfiles (outputfile)

    character(*), intent(in) :: outputfile
    integer fileunit,ios

    if (doesthefileexist(outputfile)) then
        fileunit=funit(14)
        open(unit=fileunit, iostat=ios, file=outputfile, status='old')
        if (ios==0) then
            close(fileunit, status='delete', iostat=ios)
            if (ios/=0) then
                write (iofill,'(a,i0,a)') 'Error opening output file, returned status = ', ios, &
                    '. File may be in use by another application.'
                write (*,'(a,i0,a)') 'Error opening output file, returned status = ', ios, &
                    '. File may be in use by another application.'
                stop
            end if
        end if
    end if

    return
    end subroutine deleteoutputfiles

end module output_routines
