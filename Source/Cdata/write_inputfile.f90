 module write_inputfile_routines

    use precision_parameters

    use fire_routines, only: flame_height

    use cfast_types, only: detector_type, fire_type, ramp_type, room_type, table_type, target_type, material_type, &
        vent_type, visual_type, dump_type

    use cparams, only: mxdtect, mxfires, mxhvents, mxvvents, mxramps, mxrooms, mxtarg, mxmvents, mxtabls, mxtablcols, &
        mxmatl, mx_hsep, default_grid, pde, cylpde, smoked, heatd, sprinkd, trigger_by_time, trigger_by_temp, trigger_by_flux, &
        w_from_room, w_to_room, w_from_wall, w_to_wall, mx_dumps
    use defaults, only: default_version, default_simulation_time, default_print_out_interval, default_smv_out_interval, &
        default_ss_out_interval, default_temperature, default_pressure, default_relative_humidity, default_lower_oxygen_limit, &
        default_sigma_s, default_activation_temperature, default_activation_obscuration, default_rti, default_stpmax, &
        default_min_cutoff_relp, default_max_cutoff_relp
    
    use devc_data, only: n_targets, targetinfo, n_detectors, detectorinfo
    use diag_data, only: rad_solver, partial_pressure_h2o, partial_pressure_co2, gas_temperature, upper_layer_thickness, &
        verification_time_step, verification_fire_heat_flux, radi_radnnet_flag, verification_ast, &
        radiative_incident_flux_ast, radi_verification_flag
    use fire_data, only: n_fires, fireinfo, n_furn, furn_time, furn_temp, tgignt, lower_o2_limit, mxpts, sigma_s, n_tabls, tablinfo
    use material_data, only: n_matl, material_info
    use namelist_data, only: input_file_line_number, headflag, timeflag, initflag, miscflag, matlflag, compflag, devcflag, &
        rampflag, tablflag, insfflag, fireflag, ventflag, connflag, diagflag, slcfflag, isofflag, dumpflag
    use option_data, only: option, on, off, ffire, fhflow, fvflow, fmflow, fentrain, fcjet, fdfire, frad, fconduc, fconvec, &
        fdebug, fkeyeval, fpsteady, fpdassl, fgasabsorb, fresidprn, flayermixing
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: n_rooms, roominfo, exterior_ambient_temperature, interior_ambient_temperature, exterior_abs_pressure, &
        interior_abs_pressure, pressure_ref, pressure_offset, exterior_rho, interior_rho, n_vcons, vertical_connections, &
        relative_humidity, adiabatic_walls
    use setup_data, only: iofili, iofill, cfast_version, heading, title, time_end, iofilcalc, &
        print_out_interval, smv_out_interval, ss_out_interval, validation_flag, overwrite_testcase
    use solver_data, only: stpmax, stpmin, stpmin_cnt_max, stpminflag
    use smkview_data, only: n_visual, visualinfo
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo
    use dump_data, only: n_dumps, dumpinfo, num_csvfiles, csvnames

    implicit none 
    
    private

    public write_cfast_infile

    contains
    !---------------------write_cfast_infile-----------------------------------------------------------------
    subroutine write_cfast_infile(inputfile)

    implicit none
    
    character, intent(in) :: inputfile*(*)
    integer :: ios

    !open new input file 
    open (newunit=iofilcalc, file=inputfile, action='write', iostat=ios)
    
    call write_head (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc,'(a)') '!! Scenario Configuration '
    call write_time (iofilcalc)
    call write_init (iofilcalc)
    call write_misc (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc,'(a)') '!! Material Properties '
    call write_matl (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc, '(a)') '!! Compartments'
    call write_comp (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc, '(a)') '!! Vents'
    call write_vent (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc, '(a)') '!! Devices'
    call write_devc (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc, '(a)') '!! Fire Instances'
    call write_fire (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc, '(a)') '!! Fires'
    call write_chem_tabl(iofilcalc)
    call write_conn (iofilcalc)
    write(iofilcalc,'(a1)')
    write(iofilcalc, '(a)') '!! Dumps'
    call write_dump (iofilcalc)

    close (iofili)
    
    return
    
    end subroutine write_cfast_infile
    
    !------------------------write_head-------------------------------------------------------------------
    subroutine write_head(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    write(iounit,'(a16, i4, a10, a1, a, a2)') '&HEAD VERSION = ', cfast_version, &
        ', TITLE = ', '''', trim(title),'''/'
    
    return
    end subroutine write_head
    
    !------------------------write_time-------------------------------------------------------------------
    subroutine write_time(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    character :: buf*(128), tbuf*(11)
    logical :: doline
    integer :: itmp
    
    doline = .false.
    buf = ' '
    buf(1:6) = '&TIME '
    if (time_end /= default_simulation_time) then
        doline = .true.
        tbuf = ' '
        call format_number(time_end,tbuf)
        buf = trim(buf) // ' SIMULATION = ' // trim(adjustl(tbuf))
    end if
    if (print_out_interval/=default_print_out_interval) then
        doline = .true.
        tbuf = ' '
        call format_number(print_out_interval,tbuf)
        buf = trim(buf) // ' PRINT = ' // trim(adjustl(tbuf))
    end if
    if (smv_out_interval /= default_smv_out_interval) then
        doline = .true.
        tbuf = ' '
        call format_number(smv_out_interval, tbuf)
        buf = trim(buf) // ' SMOKE = ' // trim(adjustl(tbuf))
    end if
    if (ss_out_interval /= default_ss_out_interval) then
        doline = .true.
        tbuf = ' '
        call format_number(ss_out_interval, tbuf)
        buf = trim(buf) // ' SPREADSHEET = ' // trim(adjustl(tbuf))
    end if
    
    if (doline) then
        buf = trim(buf) // '/'
        write(iounit,'(a128)') buf
    end if 
    
    return

5000 format(i5)    
5010 format(f9.2)
    
    end subroutine write_time
    
    !------------------------write_init-------------------------------------------------------------------
    subroutine write_init(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    character :: buf*(128), tbuf*(12)
    logical :: doline
    integer :: i
    real :: xxpmin
    type(room_type), pointer :: roomptr
    
    roomptr => roominfo(1)
    xxpmin = min(roomptr%interior_relp_initial,roomptr%exterior_relp_initial)
    do i = 2, n_rooms
        roomptr => roominfo(i)
        xxpmin = max(xxpmin,roomptr%interior_relp_initial,roomptr%exterior_relp_initial)
    end do
    
    doline = .false.
    buf = ' '
    buf(1:6) = '&INIT '
    if (exterior_abs_pressure - xxpmin + pressure_offset /= default_pressure) then
        doline = .true.
        tbuf = ' '
        call format_number(exterior_abs_pressure - xxpmin + pressure_offset, tbuf)
        buf = trim(buf) // ' PRESSURE = ' // trim(adjustl(tbuf))
    end if
    if (exterior_ambient_temperature /= default_temperature) then
        doline = .true.
        tbuf = ' '
        call format_number(exterior_ambient_temperature - kelvin_c_offset, tbuf)
        buf = trim(buf) // ' EXTERIOR_TEMPERATURE = ' // trim(adjustl(tbuf))
    end if
    if (interior_ambient_temperature /= default_temperature) then
        doline = .true.
        tbuf = ' '
        call format_number(interior_ambient_temperature - kelvin_c_offset, tbuf)
        buf = trim(buf) // ' INTERIOR_TEMPERATURE = ' // trim(adjustl(tbuf))
    end if
    if (relative_humidity /= default_relative_humidity) then
        doline = .true.
        tbuf = ' '
        call format_number(relative_humidity*100._eb, tbuf)
        buf = trim(buf) // ' RELATIVE_HUMIDITY = ' // trim(adjustl(tbuf))
    end if
    
    if (doline) then
        buf = trim(buf) // '/'
        write(iounit,'(a128)') buf
    end if 
    
    return
    
5000 format(f10.2)
    
    end subroutine write_init
    
    !------------------------write_misc-------------------------------------------------------------------
    subroutine write_misc(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    character :: buf*(128)
    logical :: doline
    real(eb) :: val(2)
    
    doline = .false.
    call start_namelist(buf, 'MISC')
    if (adiabatic_walls) then
        doline = .true.
        call add_token_bool(iounit, buf, 'ADIABATIC = ', adiabatic_walls)
    end if
    if (stpmax /= default_stpmax) then
        doline = .true.
        call add_token_val(iounit, buf, 'MAC_TIME_STEP = ',stpmax)
    end if
    if (lower_o2_limit /= default_lower_oxygen_limit) then
        doline = .true.
        call add_token_val(iounit, buf, 'LOWER_OXYGEN_LIMIT = ', lower_o2_limit)
    end if
    if (sigma_s(1) /= default_sigma_s(1) .or. sigma_s(2) /= default_sigma_s(2)) then
        doline = .true.
        call add_token_rarray(iounit, buf, 'SPECIFIC_EXTINCTION = ', sigma_s, 2)
    end if
    
    
    if (doline) then
        call end_namelist(iounit, buf)
    end if 
    
    return
    
5000 format(f10.2)
5010 format(f10.4)     
     
    end subroutine write_misc
    
    !------------------------write_matl-------------------------------------------------------------------
    subroutine write_matl(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    type(material_type), pointer :: thrmpptr
    character :: buf*(128), tbuf*(11)
    integer :: i
    
    if (n_matl>0) then
        do i = 1, n_matl
            call start_namelist(buf, 'MATL')
            thrmpptr => material_info(i)
            if (thrmpptr%id /= 'DEFAULT') then
                call add_token_str(iounit, buf, 'ID = ',thrmpptr%id)
                call add_token_str(iounit, buf, 'MATERIAL = ', thrmpptr%material)
                call add_token_str(iounit, buf, 'FYI = ', thrmpptr%fyi)
                call flush_buffer(iounit, buf)
                call add_token_val(iounit, buf, 'CONDUCTIVITY = ', thrmpptr%k(1))
                call add_token_val(iounit, buf, 'DENSITY = ', thrmpptr%rho(1))
                call add_token_val(iounit, buf, 'SPECIFIC_HEAT = ', thrmpptr%c(1)/1e3)
                call add_token_val(iounit, buf, 'THICKNESS = ', thrmpptr%thickness(1))
                call add_token_val(iounit, buf, 'EMISSIVITY = ', thrmpptr%eps)
                call end_namelist(iounit, buf)
            end if
        end do
    end if
    
    return
    end subroutine write_matl
    
    !------------------------write_comp-------------------------------------------------------------------
    
    subroutine write_comp(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    type(room_type), pointer :: roomptr
    character :: buf*(128), tbuf*(11)
    character(len=64), dimension(3) :: mat
    real(eb), dimension(3) :: thick
    real(eb) :: tmp(3)
    integer :: i, j
    
    if (n_rooms > 0) then
        do i = 1, n_rooms
            roomptr => roominfo(i)
            call start_namelist(buf, 'COMP')
            call add_token_str(iounit, buf, 'ID = ',roomptr%id)
            call add_token_str(iounit, buf, 'FYI = ',roomptr%fyi)
            call flush_buffer(iounit,buf)
            call add_token_val(iounit, buf, 'DEPTH = ', roomptr%cdepth)
            call add_token_val(iounit,buf,'HEIGHT = ', roomptr%cheight)
            call add_token_val(iounit, buf, 'WIDTH =', roomptr%cwidth)
            if (roomptr%surface_on(1)) then
                do j = 1, roomptr%nslab_w(1)
                    mat(j) = " "
                    mat(j) = trim(roomptr%matl(j,1))
                    thick(j) = roomptr%thick_w(j,1)
                end do
                call add_token_carray(iounit, buf, 'CEILING_MATL_ID = ', mat, roomptr%nslab_w(1))
                call add_token_rarray(iounit, buf, 'CEILING_THICKNESS = ',thick, roomptr%nslab_w(1))
            else
                call add_token_str(iounit, buf, 'CEILING_MATL_ID = ', 'OFF')
            end if
            if (roomptr%surface_on(3)) then
                do j = 1, roomptr%nslab_w(3)
                    mat(j) = " "
                    mat(j) = trim(roomptr%matl(j,3))
                    thick(j) = roomptr%thick_w(j,3)
                end do
                call add_token_carray(iounit, buf, 'WALL_MATL_ID = ', mat, roomptr%nslab_w(3))
                call add_token_rarray(iounit, buf, 'WALL_THICKNESS = ',thick, roomptr%nslab_w(3))
            else
                call add_token_str(iounit, buf, 'WALL_MATL_ID = ', 'OFF')
            end if
            if (roomptr%surface_on(2)) then
                do j = 1, roomptr%nslab_w(2)
                    mat(j) = " "
                    mat(j) = trim(roomptr%matl(j,2))
                    thick(j) = roomptr%thick_w(j,2)
                end do
                call add_token_carray(iounit, buf, 'FLOOR_MATL_ID = ', mat, roomptr%nslab_w(2))
                call add_token_rarray(iounit, buf, 'FLOOR_THICKNESS = ',thick, roomptr%nslab_w(2))
            else
                call add_token_str(iounit, buf, 'FLOOR_MATL_ID = ', 'OFF')
            end if
            tmp(1) = roomptr%x0
            tmp(2) = roomptr%y0
            tmp(3) = roomptr%z0
            call add_token_rarray(iounit, buf, 'ORIGIN = ', tmp, 3)
            tmp(1) = roomptr%ibar
            tmp(2) = roomptr%jbar
            tmp(3) = roomptr%kbar
            call add_token_rarray(iounit, buf, 'GRID = ', tmp, 3)
            if ((roomptr%leak_area_ratios(1)+roomptr%leak_area_ratios(2)) > 0.0) then 
                call add_token_rarray(iounit, buf, 'LEAK_AREA_RATIO = ', roomptr%leak_area_ratios, 2)
            else
                call add_token_rarray(iounit, buf, 'LEAK_AREA = ', roomptr%leak_areas, 2)
            end if
            if (roomptr%hall) then
                call add_token_bool(iounit, buf, 'HALL = ', roomptr%hall)
            elseif (roomptr%shaft) then
                call add_token_bool(iounit, buf, 'SHAFT = ', roomptr%shaft)
            endif
            call end_namelist(iounit, buf)
        end do
    end if 
    
    return
    end subroutine write_comp
    
    !------------------------write_devc-------------------------------------------------------------------
    subroutine write_devc (iounit)
    
    implicit none
    integer, intent(in) :: iounit

    integer :: i
    type(target_type), pointer :: targptr
    type(detector_type), pointer :: dtectptr
    character :: buf*(128)
    real(eb) :: tmp(2)

    write(iounit, '(a1)')
    write(iounit, '(a)') '!! Targets'
    if (n_targets > 0) then
        do i = 1, n_targets
            targptr => targetinfo(i)
            call start_namelist(buf, 'DEVC')
            call add_token_str(iounit, buf, 'ID = ', targptr%id)
            call add_token_str(iounit, buf, 'FYI = ', targptr%fyi)
            call add_token_str(iounit, buf, 'COMP_ID = ', roominfo(targptr%room)%id)
            if (targptr%equaton_type == pde) then
                call add_token_str(iounit, buf, 'TYPE = ', 'PLATE')
            elseif (targptr%equaton_type == cylpde) then
                call add_token_str(iounit, buf, 'TYPE = ', 'CYLINDER')
            end if
            call add_token_str(iounit, buf, 'MATL_ID = ', targptr%material)
            call add_token_val(iounit, buf, 'TEMPERATURE_DEPTH = ', targptr%depth_loc)
            if (trim(targptr%depth_units) /= 'FRACTION') then
                call add_token_str(iounit, buf, 'DEPTH_UNITS = ', targptr%depth_units)
            end if
            call add_token_rarray(iounit, buf, 'LOCATION = ', targptr%center, 3)
            if (trim(targptr%front_surface_orientation) == 'NULL') then
                call add_token_rarray(iounit, buf, 'NORMAL = ', targptr%normal, 3)
            else
                call add_token_str(iounit, buf, 'FRONT_SURFACE_OREINTATION = ', targptr%front_surface_orientation)
            end if 
            if (targptr%adiabatic) call add_token_bool(iounit, buf, 'ADIABATIC_TARGET = ', targptr%adiabatic)
            if (targptr%h_conv(1) /= 0._eb .or. targptr%h_conv(2) /= 0._eb) then 
                tmp(1) = targptr%h_conv(1)/1000._eb
                tmp(2) = targptr%h_conv(2)/1000._eb
                call add_token_rarray(iounit, buf, 'CONVECTION_COEFFICENTS = ', tmp, 2)
            end if
            call end_namelist(iounit, buf)
        end do
    end if 
            
    write(iounit, '(a1)')
    write(iounit, '(a)') '!! Detectors'
    if (n_detectors > 0._eb) then
        do i = 1, n_detectors
            dtectptr => detectorinfo(i)
            call start_namelist(buf, 'DEVC')
            call add_token_str(iounit, buf, 'ID = ', dtectptr%id)
            call add_token_str(iounit, buf, 'FYI = ', dtectptr%fyi)
            call add_token_str(iounit, buf, 'COMP_ID = ', roominfo(dtectptr%room)%id)
            if(dtectptr%dtype == smoked) then
                call add_token_str(iounit, buf, 'TYPE = ', 'SMOKE_DETECTOR')
                call add_token_rarray(iounit, buf, 'LOCATION = ', dtectptr%center, 3)
                if (dtectptr%dual_detector) then
                    tmp(1) = dtectptr%trigger
                    tmp(2) = dtectptr%trigger_smolder
                    call add_token_rarray(iounit, buf, 'SETPOINTS = ', tmp, 2)
                else
                    if (dtectptr%trigger /= default_activation_obscuration) then
                        call add_token_val(iounit, buf, 'SETPOINT = ', dtectptr%trigger)
                    end if
                end if
            elseif (dtectptr%dtype == heatd) then
                call add_token_str(iounit, buf, 'TYPE = ', 'HEAT_DETECTOR')
                call add_token_rarray(iounit, buf, 'LOCATION = ', dtectptr%center, 3)
                call add_token_val(iounit, buf, 'RTI = ', dtectptr%rti)
                if (dtectptr%trigger /= default_activation_temperature) then
                    call add_token_val(iounit, buf, 'SETPOINT = ', dtectptr%trigger - kelvin_c_offset)
                end if
            elseif (dtectptr%dtype == sprinkd) then
                call add_token_str(iounit, buf, 'TYPE = ', 'SPRINKLER')
                call add_token_rarray(iounit, buf, 'LOCATION = ', dtectptr%center, 3)
                call add_token_val(iounit, buf, 'RTI = ', dtectptr%rti)
                if (dtectptr%trigger /= default_activation_temperature) then
                    call add_token_val(iounit, buf, 'SETPOINT = ', dtectptr%trigger - kelvin_c_offset)
                end if
                call add_token_val(iounit, buf, 'SPRAY_DENISTY = ', dtectptr%spray_density/1000._eb)
            end if
            call end_namelist(iounit, buf)
        end do
    end if
    
    return
    end subroutine write_devc
    
    !------------------------write_fire-------------------------------------------------------------------
    subroutine write_fire(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    type(room_type),   pointer :: roomptr
    type(fire_type),   pointer :: fireptr
    type(target_type), pointer :: targptr
    
    integer :: i
    character :: buf*(128)
    real(eb) :: tmp(2)

    if (n_fires > 0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            call start_namelist(buf, 'FIRE')
            call add_token_str(iounit, buf, 'ID = ', fireptr%id)
            call add_token_str(iounit, buf, 'FIRE_ID = ',fireptr%fire_id)
            call add_token_str(iounit, buf, 'COMP_ID = ', roominfo(fireptr%room)%id)
            call add_token_str(iounit, buf, 'FYI = ', fireptr%fyi)
            tmp(1) = fireptr%x_position
            tmp(2) = fireptr%y_position
            call add_token_rarray(iounit, buf, 'LOCATION = ', tmp, 2)
            if (fireptr%ignition_type == trigger_by_time) then
                if (fireptr%ignition_time /= 0._eb) then
                    call add_token_str(iounit, buf, 'IGNITION_CRITERION = ', 'TIME')
                    call add_token_val(iounit, buf, 'SETPOINT = ', fireptr%ignition_time)
                end if
            else if (fireptr%ignition_type == trigger_by_temp) then
                call add_token_str(iounit, buf, 'IGNITION_CRITERION = ', 'TEMPERATURE')
                call add_token_val(iounit, buf, 'SETPOINT = ', fireptr%ignition_criterion - kelvin_c_offset)
                call add_token_str(iounit, buf, 'DEVICE = ', targetinfo(fireptr%ignition_target)%id)
            else if (fireptr%ignition_type == trigger_by_flux) then
                call add_token_str(iounit, buf, 'IGNITION_CRITERION = ', 'FLUX')
                call add_token_val(iounit, buf, 'SETPOINT = ', fireptr%ignition_criterion/1000._eb)
                call add_token_str(iounit, buf, 'DEVICE = ', targetinfo(fireptr%ignition_target)%id)
            end if 
            call end_namelist(iounit, buf)
        end do
    end if 
    
    return
    end subroutine write_fire
    
    !------------------------write_chem_tabl-------------------------------------------------------------------
    subroutine write_chem_tabl(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    type(fire_type),   pointer :: fireptr
    integer :: i, j
    character :: buf*(128), lbuf*(256) 
    character :: lbls(8)*(15) = &
        (/ 'TIME            ', 'HRR         ', 'HEIGHT         ', &
           'AREA            ', 'CO_YEILD    ', 'SOOT_YEILD     ', &
           'HCN_YEILD       ', 'TRACE_YEILD ' /)
    logical :: dup
    real(eb) :: vals(8)
    
    if (n_fires > 0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            dup = .false.
            do j = 1, i-1
                if (trim(fireptr%fire_id) == trim(fireinfo(j)%fire_id)) dup = .true.
            end do
            if (.not.dup) then
                call start_namelist(buf, 'CHEM')
                call add_token_str(iounit, buf, 'ID = ', fireptr%fire_id)
                if (fireptr%n_C > 0) then
                    call add_token_val(iounit, buf, 'CARBON = ', fireptr%n_C)
                end if
                if (fireptr%n_H > 0) then
                    call add_token_val(iounit, buf, 'HYDROGEN = ', fireptr%n_H)
                end if
                if (fireptr%n_N > 0) then
                    call add_token_val(iounit, buf, 'NITROGEN = ', fireptr%n_N)
                end if
                if (fireptr%n_O > 0) then
                    call add_token_val(iounit, buf, 'OXYGEN = ', fireptr%n_O)
                end if
                if (fireptr%n_Cl> 0) then
                    call add_token_val(iounit, buf, 'CHLORINE = ', fireptr%n_Cl)
                end if
                if (fireptr%hoc(1) /= 5.0e7) then
                    call add_token_val(iounit, buf, 'HEAT_OF_COMBUSTION = ',fireptr%hoc(1)/1000._eb)
                end if 
                call add_token_val(iounit, buf, 'RADIATIVE_FRACTION = ', fireptr%chirad)
                if (fireptr%flaming_transition_time > 0._eb) then
                    call add_token_val(iounit, buf, 'FLAMING_TRANSITION_TIME = ', fireptr%flaming_transition_time)
                end if
                call end_namelist(iounit, buf)
                
                call start_namelist(lbuf, 'TABL')
                call add_token_str(iounit, lbuf, 'ID = ', fireptr%fire_id)
                call add_token_carray(iounit, lbuf, 'LABELS = ', lbls, 8)
                call end_namelist(iounit, lbuf)
                do j = 1, fireptr%n_qdot
                    vals(1) = fireptr%t_qdot(j)
                    vals(2) = fireptr%qdot(j)/1000._eb
                    vals(3) = fireptr%height(j)
                    vals(4) = fireptr%area(j)
                    vals(5) = fireptr%y_co(j)
                    vals(6) = fireptr%y_soot(j)
                    vals(7) = fireptr%y_hcn(j)
                    vals(8) = fireptr%y_trace(j)
                    call start_namelist(lbuf, 'TABL')
                    call add_token_str(iounit, lbuf, 'ID = ', fireptr%fire_id)
                    call add_token_rarray(iounit, lbuf, 'DATA = ',vals, 8)
                    call end_namelist(iounit, lbuf)
                end do 
                write(iounit, '(a1)')
            end if
        end do 
    end if
    
    return
    end subroutine write_chem_tabl
    
    !------------------------write_vent-------------------------------------------------------------------
    subroutine write_vent(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
    type(ramp_type), pointer :: rampptr
    
    integer :: i, j
    character :: buf*(128), rms(2)*(128)
    real(eb) :: val(2)
    
    if (n_hvents>0) then
        write(iounit,'(a1)')
        write(iounit,'(a)') '!! Wall'
        do I = 1, n_hvents
            ventptr=> hventinfo(i)
            call start_namelist(buf, 'VENT')
            call add_token_str(iounit,buf, 'TYPE = ','WALL')
            call add_token_str(iounit, buf, 'ID = ',ventptr%id)
            call add_token_str(iounit, buf, 'FYI = ',ventptr%fyi)
            rms(1) = ' '
            rms(2) = ' '
            if (ventptr%room1 == n_rooms+1) then
                rms(1) = 'OUTSIDE'
            else
                rms(1) = roominfo(ventptr%room1)%id
            end if
            if (ventptr%room2 == n_rooms+1) then
                rms(2) = 'OUTSIDE'
            else
                rms(2) = roominfo(ventptr%room2)%id
            end if
            call add_token_carray(iounit, buf, 'COMP_IDS = ', rms, 2)
            call add_token_val(iounit, buf, 'TOP = ', ventptr%soffit)
            call add_token_val(iounit, buf, 'BOTTOM = ', ventptr%sill)
            call add_token_val(iounit, buf, 'WIDTH = ', ventptr%width)
            call add_token_val(iounit, buf, 'OFFSET = ',ventptr%offset(1))
            if (ventptr%face == 1) then
                call add_token_str(iounit, buf, 'FACE = ', 'FRONT')
            elseif (ventptr%face == 2) then
                call add_token_str(iounit, buf, 'FACE = ', 'RIGHT')
            elseif (ventptr%face == 3) then
                call add_token_str(iounit, buf, 'FACE = ', 'REAR')
            elseif (ventptr%face == 4) then
                call add_token_str(iounit, buf, 'FACE = ', 'LEFT')
            end if
            call end_namelist(iounit, buf)
        end do
    end if
    
     if (n_vvents>0) then
        write(iounit,'(a1)')
        write(iounit,'(a)') '!! Ceiling and Floor'
        do I = 1, n_vvents
            ventptr=> vventinfo(i)
            call start_namelist(buf, 'VENT')
            call add_token_str(iounit,buf, 'TYPE = ','FLOOR')
            call add_token_str(iounit, buf, 'ID = ',ventptr%id)
            call add_token_str(iounit, buf, 'FYI = ',ventptr%fyi)
            rms(1) = ' '
            rms(2) = ' '
            if (ventptr%room1 == n_rooms+1) then
                rms(1) = 'OUTSIDE'
            else
                rms(1) = roominfo(ventptr%room1)%id
            end if
            if (ventptr%room2 == n_rooms+1) then
                rms(2) = 'OUTSIDE'
            else
                rms(2) = roominfo(ventptr%room2)%id
            end if
            call add_token_carray(iounit, buf, 'COMP_IDS = ', rms, 2)
            call add_token_val(iounit, buf, 'AREA = ', ventptr%area)
            if (ventptr%shape == 1) then 
                call add_token_str(iounit, buf, 'SHAPE = ', 'ROUND')
            else if (ventptr%shape == 2) then
                call add_token_str(iounit, buf, 'SHAPE = ', 'SQUARE') 
            end if
            call add_token_rarray(iounit, buf, 'OFFSETS = ', ventptr%offset, 2)
            call end_namelist(iounit, buf)
        end do
    end if   
    
     if (n_mvents>0) then
        write(iounit,'(a1)')
        write(iounit,'(a)') '!! Mechanical'
        do I = 1, n_mvents
            ventptr=> mventinfo(i)
            call start_namelist(buf, 'VENT')
            call add_token_str(iounit,buf, 'TYPE = ','MECHANICAL')
            call add_token_str(iounit, buf, 'ID = ',ventptr%id)
            call add_token_str(iounit, buf, 'FYI = ',ventptr%fyi)
            rms(1) = ' '
            rms(2) = ' '
            if (ventptr%room1 == n_rooms+1) then
                rms(1) = 'OUTSIDE'
            else
                rms(1) = roominfo(ventptr%room1)%id
            end if
            if (ventptr%room2 == n_rooms+1) then
                rms(2) = 'OUTSIDE'
            else
                rms(2) = roominfo(ventptr%room2)%id
            end if
            call add_token_carray(iounit, buf, 'COMP_IDS = ', rms, 2)
            call add_token_rarray(iounit, buf, 'HEIGHTS = ', ventptr%height, 2)
            do j = 1, 2
                if (ventptr%orientation(j) == 1) then
                    rms(j) = 'VERTICAL'
                elseif (ventptr%orientation(j) == 2) then
                    rms(j) = 'HORIZONTAL'
                end if
            end do
            call add_token_carray(iounit, buf, 'ORIENTATIONS = ', rms, 2)
            call add_token_rarray(iounit, buf, 'AREAS = ', ventptr%diffuser_area, 2)
            call add_token_val(iounit, buf, 'FLOW = ', ventptr%maxflow)
            if (ventptr%min_cutoff_relp /= default_min_cutoff_relp .or. &
               ventptr%max_cutoff_relp /= default_max_cutoff_relp) then
                val(1) = ventptr%min_cutoff_relp
                val(2) = ventptr%max_cutoff_relp
                call add_token_rarray(iounit, buf, 'CUTOFFS = ', val, 2)
            end if
            if (ventptr%maxflow /= 0) then
                call add_token_val(iounit, buf, 'FLOW = ', ventptr%maxflow)
            end if
            if (ventptr%filter_initial_time > 0._eb) then
                call add_token_val(iounit, buf, 'FILTER_TIME = ', ventptr%filter_initial_time)
            end if
            if (ventptr%filter_final_fraction > 0._eb) then
                call add_token_val(iounit, buf, 'FILTER_EFFICIENCY = ', ventptr%filter_final_fraction*100._eb)
            end if
            call end_namelist(iounit, buf)
        end do
    end if   
    
    return
    end subroutine write_vent
    
    !------------------------write_conn-------------------------------------------------------------------
    subroutine write_conn(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    return
    end subroutine write_conn
    
    !------------------------write_dump-------------------------------------------------------------------
    subroutine write_dump(iounit)
    
    implicit none
    integer, intent(in) :: iounit
    
    type(dump_type), pointer :: dumpptr
    integer :: i
    character :: buf*(128)
    
    if (n_dumps > 0) then
        do i = 1, n_dumps
            dumpptr => dumpinfo(i)
            call start_namelist(buf, 'DUMP')
            call add_token_str(iounit, buf, 'ID = ', dumpptr%id)
            call add_token_str(iounit, buf, 'FYI = ', dumpptr%fyi)
            call add_token_str(iounit, buf, 'TYPE = ', dumpptr%type)
            call add_token_str(iounit, buf, 'FILE_TYPE ', dumpptr%file_type)
            call add_token_str(iounit, buf, 'FIRST_DEVICE = ', dumpptr%first_device)
            call add_token_str(iounit, buf, 'FIRST_MEASUREMENT = ', dumpptr%first_measurement)
            call add_token_str(iounit, buf, 'SECOND_DEVICE = ', dumpptr%second_device)
            call add_token_str(iounit, buf, 'SECOND_MEASUREMENT = ', dumpptr%second_measurement)
            if (dumpptr%type(1:3) == 'TRI') then
                call add_token_val(iounit, buf, 'CRITERIA = ', dumpptr%criteria)
            end if
            call end_namelist(iounit, buf)
        end do
    end if
    
    return
    end subroutine write_dump
    
    !--------start_namelist   --------
    
    subroutine start_namelist(buf, str)
    
    character, intent(inout) :: buf*(*)
    character, intent(in) :: str*(*)
    
    buf = ' '
    buf = '&' // trim(adjustl(str))
    
    return
    
    end subroutine start_namelist
    
    !---------flush_buffer-----------
    
    subroutine flush_buffer(iounit, buf)
    
    integer, intent(in) :: iounit
    character, intent(inout) :: buf*(*)
    
    write(iounit, '(a)') buf
    buf = ' '
    
    return
    
    end subroutine flush_buffer
    
    !---------end_namelist---------
    
    subroutine end_namelist(iounit, buf)
    
    integer, intent(in) :: iounit
    character, intent(inout) ::buf*(*)
    
    integer lbuf, lln
    lbuf = len(buf)
    lln = len_trim(buf)
    if (lln == lbuf) then
        call flush_buffer(iounit,buf)
        buf = '/'
        call flush_buffer(iounit,buf)
    else
        buf = trim(buf) // '/'
        call flush_buffer(iounit, buf)
    endif
    
    return
    
    end subroutine end_namelist
    
    !-------format_number---------------
    
    subroutine format_number(value, buf)
    
    character, intent(out) :: buf*(*)
    real(eb), intent(in) :: value
    integer :: itmp
    
    if (floor(value) == value) then
        itmp = value
        write(buf,'(i7)') itmp
    elseif (floor(value*10) == value*10) then
        write(buf,'(f11.1)') value
    elseif (floor(value*100) == value*100) then
        write(buf,'(f11.2)') value
    elseif (value < 100) then
        if (value < 10) then
            if (floor(value*1000) == value*1000) then
                write(buf,'(f11.3)') value
            elseif (value < 0.001) then
                write(buf,'(e10.3)') value
            else
                write(buf,'(f11.4)') value
            end if
        else
            write(buf,'(f11.2)') value
        end if
    else
        write(buf,'(f11.2)') value
    end if
    
    return
    
    end subroutine format_number
    
    !----add_token_val---------------------------------------------------------
    
    subroutine add_token_val(iounit, buf, token, value)
    
    integer, intent(in) :: iounit
    real(eb), intent(in) :: value
    character, intent(in) :: token*(*)
    character, intent(inout) :: buf*(*)
    
    character :: tbuf*(512), vbuf*(15)
    
    call format_number(value, vbuf)
    tbuf = ' '
    tbuf = trim(adjustl(token)) // ' ' // trim(adjustl(vbuf))
    call add_token(iounit, buf, tbuf)
    return
    
    end subroutine add_token_val
    
    !--------add_token_str(iounit, buf, token, str)-----------------------
    
    subroutine add_token_str(iounit, buf, token, str)
    
    integer, intent(in) :: iounit
    character, intent(in) :: token*(*), str*(*)
    character, intent(inout) :: buf*(*)
    integer :: dummy
    
    character :: tbuf*(512)
    
    tbuf = str
    dummy = len_trim(str)
    tbuf = ' '
    ! len_trim has seemed to be inconsistant. It seems to have returned both 0 and 1 for a string of blanks
    ! the current overkill is deal with the problem. Hopefully this can be better debugged in the future. 
    if (dummy == 0) return
    if (len_trim(str) == 1 .and. str(1:1) == ' ') return
    if (len(str) >= 4) then 
        if ((str(1:4) == 'NULL' .or. str(1:4) == 'null' .or. str(1:4) == 'Null')) return
    end if 
    tbuf = trim(adjustl(token)) // ' ''' // trim(adjustl(str)) // ''''
    call add_token(iounit, buf, tbuf)
    return
    
    end subroutine add_token_str

    !----add_token_rarray---------------------------------------------------------
    
    subroutine add_token_rarray(iounit, buf, token, value, idx)
    
    integer, intent(in) :: iounit, idx
    real(eb), intent(in) :: value(*)
    character, intent(in) :: token*(*)
    character, intent(inout) :: buf*(*)
    
    character :: tbuf*(512), vbuf*(15)
    integer :: i
    
    tbuf = ' '
    tbuf = trim(adjustl(token))
    do i = 1, idx
        call format_number(value(i), vbuf)
        tbuf = trim(tbuf) // ' ' // trim(adjustl(vbuf))
    end do 
    call add_token(iounit, buf, tbuf)
    return
    
    end subroutine add_token_rarray
    
    !----add_token_carray---------------------------------------------------------
    
    subroutine add_token_carray(iounit, buf, token, value, idx)
    
    integer, intent(in) :: iounit, idx
    character, intent(in) :: value(idx)*(*)
    character, intent(in) :: token*(*)
    character, intent(inout) :: buf*(*)
    
    character :: tbuf*(512), vbuf*(15)
    integer :: i
    
    tbuf = ' '
    tbuf = trim(adjustl(token))
    do i = 1, idx
        tbuf = trim(tbuf) // ' ''' // trim(adjustl(value(i))) // ''''
    end do 
    call add_token(iounit, buf, tbuf)
    return
    
    end subroutine add_token_carray
    
    !----------add_token_bool------------------
    
    subroutine add_token_bool(iounit, buf, token, bool)
    
    integer, intent(in) :: iounit
    character, intent(in) :: token*(*)
    character, intent(inout) :: buf*(*)
    logical, intent(in) :: bool
    
    character :: tbuf*(512)
    
    tbuf = ' '
    if (bool) then
        tbuf = trim(adjustl(token)) // ' ' // '.TRUE.'
    else
        tbuf = trim(adjustl(token)) // ' ' // '.FALSE.'
    endif
    call add_token(iounit, buf, tbuf)
    return
    
    end subroutine add_token_bool
    
    !-----add_token---------------------
    
    subroutine add_token(iounit, buf, token)
    
    integer, intent(in) :: iounit
    character, intent(in) :: token*(*)
    character, intent(inout) :: buf*(*)
    
    integer :: lbuf, lln, ltok
    
    lbuf = len(buf)
    lln = len_trim(buf)+1
    ltok = len_trim(adjustl(token))
    if (lln == 1) then
        buf = '     ' // trim(adjustl(token)) 
    elseif (lln+ltok > lbuf) then
        call flush_buffer(iounit, buf)
        buf = '     ' // trim(adjustl(token))
    else
        buf = trim(buf) // ' ' // trim(adjustl(token))
    end if
    return
    
    end subroutine add_token
    
    end module write_inputfile_routines