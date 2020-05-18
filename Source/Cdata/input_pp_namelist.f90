   module namelist_input_pp_routines

    use precision_parameters

    use fire_routines, only: flame_height

    use cfast_types, only: detector_type, fire_type, ramp_type, room_type, table_type, target_type, thermal_type, &
        vent_type, visual_type, dump_type, cfast_type

    use cparams, only: mxdtect, mxfires, mxhvents, mxvvents, mxramps, mxrooms, mxtarg, mxmvents, mxtabls, mxtablcols, &
        mxthrmp, mx_hsep, default_grid, pde, cylpde, smoked, heatd, sprinkd, trigger_by_time, trigger_by_temp, trigger_by_flux, &
        w_from_room, w_to_room, w_from_wall, w_to_wall, mx_dumps
    use diag_data, only: rad_solver, partial_pressure_h2o, partial_pressure_co2, gas_temperature, upper_layer_thickness, &
        verification_time_step, verification_fire_heat_flux, radi_radnnet_flag, verification_ast, &
        radiative_incident_flux_ast, radi_verification_flag
    use namelist_data, only: input_file_line_number, headflag, timeflag, initflag, miscflag, matlflag, compflag, devcflag, &
        rampflag, tablflag, insfflag, fireflag, ventflag, connflag, diagflag, slcfflag, isofflag, dumpflag
    use defaults, only: default_version, default_simulation_time, default_print_out_interval, default_smv_out_interval, &
        default_ss_out_interval, default_temperature, default_pressure, default_relative_humidity, default_lower_oxygen_limit, &
        default_sigma_s, default_activation_temperature, default_activation_obscuration, default_rti, default_stpmax, &
        default_min_cutoff_relp, default_max_cutoff_relp
    use fire_data, only: n_fires, fireinfo, n_furn, furn_time, furn_temp, tgignt, lower_o2_limit, mxpts, sigma_s, n_tabls, tablinfo
    use option_data, only: option, on, off, ffire, fhflow, fvflow, fmflow, fentrain, fcjet, fdfire, frad, fconduc, fconvec, &
        fdebug, fkeyeval, fpsteady, fpdassl, fgasabsorb, fresidprn, flayermixing
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: n_rooms, roominfo, exterior_ambient_temperature, interior_ambient_temperature, exterior_abs_pressure, &
        interior_abs_pressure, pressure_ref, pressure_offset, exterior_rho, interior_rho, n_vcons, vertical_connections, &
        relative_humidity, adiabatic_walls
    use setup_data, only: iofili, iofill, cfast_version, heading, title, time_end, &
        print_out_interval, smv_out_interval, ss_out_interval, validation_flag, overwrite_testcase, inputfile
    use solver_data, only: stpmax, stpmin, stpmin_cnt_max, stpminflag
    use smkview_data, only: n_visual, visualinfo
    use target_data, only: n_targets, targetinfo, n_detectors, detectorinfo
    use thermal_data, only: n_thrmp, thermalinfo
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo
    use dump_data, only: n_dumps, dumpinfo, num_csvfiles, csvnames
    use utility_routines, only: d1mach
    
    use pp_params, only: mxgenerators, mxseeds, idx_uniform, rand_dist, mxfields, val_types, idx_real, &
                idx_char, idx_int, idx_logic, rnd_seeds, restart_values
    use preprocessor_types, only: random_generator_type, field_pointer
    use montecarlo_data, only: mc_number_of_cases, generatorinfo, n_generators, n_fields, fieldinfo, mc_write_seeds
    use preprocessor_routines, only: preprocessor_initialize
    
    use namelist_input_routines, only: checkread

    implicit none
    external cfastexit
    
    private

    public namelist_pp_input

    contains
    
    ! --------------------------- namelist_pp_input ----------------------------------
    subroutine namelist_pp_input

    implicit none
    
    integer :: ios
    
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)

    call preprocessor_initialize
    call read_mhdr(iofili)
    call read_mrnd(iofili)
    call read_mfld(iofili)

    close (iofili)
    
    return

    end subroutine namelist_pp_input
    
    
    !--------------------------------------------read_mhdr----------------------
    subroutine read_mhdr(lu)
    
    integer :: ios
    real(eb) :: monte_carlo_number_of_cases
    integer :: seeds(2)
    logical :: mhdrflag, write_seeds
    
    integer, intent(in) :: lu

    namelist /MHDR/ monte_carlo_number_of_cases, seeds, write_seeds

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &HEAD input
    mhdr_loop: do
        call checkread ('MHDR', lu, ios)
        if (ios==0) mhdrflag=.true.
        if (ios==1) then
            exit mhdr_loop
        end if
        read(lu,MHDR,iostat=ios)
        if (ios>0) then
            write(iofill, '(a)') '***Error in &MHDR: Invalid specification for inputs.'
            call cfastexit('read_mhdr',1)
        end if
    end do mhdr_loop

    if (.not.mhdrflag) then
        write (*, '(/, "***Error: &MHDR inputs are required.")')
        write (iofill, '(/, "***Error: &MHDR inputs are required.")')
        call cfastexit('read_mhdr',2)
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    mhdr_flag: if (mhdrflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('MHDR',lu,ios)
        call set_defaults
        read(lu,MHDR)
        
        mc_number_of_cases = monte_carlo_number_of_cases
        rnd_seeds(1:2) = seeds(1:2)
        if (seeds(1) > 0.0_eb .and. seeds(2) > 0.0_eb) then
            call RANDOM_SEED(PUT=seeds)
        end if
        mc_write_seeds = write_seeds

    end if mhdr_flag

    contains

    subroutine set_defaults

    monte_carlo_number_of_cases = -1001
    seeds(1:mxseeds) = -1001
    write_seeds = .false.

    end subroutine set_defaults

    end subroutine read_mhdr
    
    
    !--------------------------------------------read_mrnd----------------------
    subroutine read_mrnd(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii
    logical :: mrndflag
    type(random_generator_type), pointer :: genptr
    
    character(len=128) :: id, fyi
    character(len=9) :: type_dist
    integer, parameter :: no_seed_value = -1001
    real(eb) :: minimum, maximum, mean, stdev, alpha, beta, peak
    integer ::initial_seed_values(mxseeds)

    namelist /MRND/ id, fyi, type_dist, minimum, maximum, mean, stdev, alpha, beta, peak, initial_seed_values
                    
    
    ios = 1
    n_generators = 0

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &HEAD input
    mrnd_loop: do
        call checkread ('MRND', lu, ios)
        if (ios==0) mrndflag=.true.
        if (ios==1) then
            exit mrnd_loop
        end if
        read(lu,MRND,iostat=ios)
        n_generators = n_generators + 1
        if (ios>0) then
            write(iofill, '(a)') '***Error in &MRND: Invalid specification for inputs.'
            call cfastexit('read_mrnd',1)
        end if
    end do mrnd_loop

    if (n_fields>mxfields) then
        write (*,'(a,i3)') '***Error: Too many random generators in input data file. Limit is ', mxgenerators
        write (iofill,'(a,i3)') '***Error: Too many random generators in input data file. Limit is ', mxgenerators
        call cfastexit('read_mrnd',2)
    end if

    if (.not.mrndflag) then
        write (*, '(/, "***Error: &MRND inputs are required.")')
        write (iofill, '(/, "***Error: &MRND inputs are required.")')
        call cfastexit('read_mrnd',3)
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    mrnd_flag: if (mrndflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_mrnd_loop: do ii=1,n_generators

            genptr => generatorinfo(ii)

            call checkread('MRND',lu,ios)
            call set_defaults
            read(lu,mrnd)
            
            genptr%id = id
            genptr%fyi = fyi
        
            if (type_dist == rand_dist(idx_uniform)) then
                if (maximum<minimum) then
                    call cfastexit('read_mrnd',4)
                end if
                genptr%value_type = val_types(idx_real)
                genptr%type_dist = type_dist
                genptr%min = minimum
                genptr%max = maximum
            else
                call cfastexit('read_mrnd',1000)
            end if
            
        end do read_mrnd_loop

    end if mrnd_flag
        

    contains

    subroutine set_defaults

    id = 'NULL'
    fyi = 'NULL'
    type_dist = 'NULL'
    minimum = d1mach(2)
    maximum = -d1mach(2)
    mean = d1mach(2)
    stdev = -1001._eb
    alpha = -1001._eb
    beta = -1001._eb
    initial_seed_values(1:mxseeds) = -1001._eb

    end subroutine set_defaults

    end subroutine read_mrnd
    
    
    !--------------------------------------------read_mfld----------------------
    subroutine read_mfld(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj
    logical :: mfldflag, found, add_to_parameters
    type(field_pointer), pointer :: fldptr
    
    
    character(len=128) :: id, object_id, rand_id, field_name, parameter_header

    namelist /MFLD/ id, object_id, field_name, rand_id, parameter_header, add_to_parameters
                    
    
    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &HEAD input
    mfld_loop: do
        call checkread ('MFLD', lu, ios)
        if (ios==0) mfldflag=.true.
        if (ios==1) then
            exit mfld_loop
        end if
        read(lu,MFLD,iostat=ios)
        n_fields = n_fields + 1
        if (ios>0) then
            write(iofill, '(a)') '***Error in &MFLD: Invalid specification for inputs.'
            call cfastexit('read_mfld',1)
        end if
    end do mfld_loop

    if (n_fields>mxfields) then
        write (*,'(a,i3)') '***Error: Too many fields in input data file. Limit is ', mxfields
        write (iofill,'(a,i3)') '***Error: Too many fields in input data file. Limit is ', mxfields
        call cfastexit('read_mfld',2)
    end if

    if (.not.mfldflag) then
        write (*, '(/, "***Error: &MFLD inputs are required.")')
        write (iofill, '(/, "***Error: &MFLD inputs are required.")')
        call cfastexit('read_mfld',3)
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    mfld_flag: if (mfldflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_mfld_loop: do ii=1,n_fields

            fldptr => fieldinfo(ii)

            call checkread('MFLD',lu,ios)
            call set_defaults
            read(lu,MFLD)
            if (trim(id) == 'NULL') then
                write(*,'(a)') '***Error: ID field requires a value'
                write(iofill,'(a)') '***Error: ID field requires a value'
                call cfastexit('read_mfld',4)
            end if
            fldptr%id = id
            call find_object(object_id, fldptr, found)
            if (found) then
                call find_field(field_name, fldptr%itemptr, fldptr, found)
            else
                call cfastexit('read_mfld',5)
            end if
            test: do jj = 1, n_generators
                if (trim(rand_id) == trim(generatorinfo(jj)%id)) then
                    fldptr%genptr => generatorinfo(jj)
                    if (trim(generatorinfo(jj)%value_type) /= trim(fldptr%realval%value_type)) then
                        call cfastexit('read_mfld',6)
                    end if
                end if
            end do test
            if (add_to_parameters) then
                fldptr%add_to_parameters = add_to_parameters
                if (trim(parameter_header) == 'NULL') then
                    fldptr%parameter_header = trim(object_id) // '_' // trim(field_name)
                else
                    fldptr%parameter_header = parameter_header
                end if
            end if
            
        end do read_mfld_loop

    end if mfld_flag
        

    contains

    subroutine set_defaults

    id = 'NULL'
    object_id = 'NULL'
    field_name = 'NULL'
    rand_id = 'NULL'
    add_to_parameters = .false.
    

    end subroutine set_defaults

    end subroutine read_mfld
    
    !-------------------------------find_object--------------------------------------
    subroutine find_object(id, field, flag)
    character(len=*), intent(in) :: id
    type(field_pointer), intent(out) :: field
    logical, intent(out) :: flag
    
    integer :: i
    
    flag = .false.
    do i = 1, n_rooms
        if (trim(id)==trim(roominfo(i)%id)) then
            flag = .true.
            field%itemptr => roominfo(i)
            return
        end if
    enddo
    do i = 1, n_thrmp
        if (trim(id)==trim(thermalinfo(i)%id)) then
            flag = .true.
            field%itemptr => thermalinfo(i)
            return
        end if
    enddo
    do i = 1, n_hvents
        if (trim(id)==trim(hventinfo(i)%id)) then
            flag = .true.
            field%itemptr => hventinfo(i)
            return
        end if
    enddo
    do i = 1, n_leaks
        if (trim(id)==trim(leakinfo(i)%id)) then
            flag = .true.
            field%itemptr => leakinfo(i)
            return
        end if
    enddo
    do i = 1, n_vvents
        if (trim(id)==trim(vventinfo(i)%id)) then
            flag = .true.
            field%itemptr => vventinfo(i)
            return
    end if
    enddo
    do i = 1, n_mvents
        if (trim(id)==trim(mventinfo(i)%id)) then
            flag = .true.
            field%itemptr => mventinfo(i)
            return
        end if
    enddo
    do i = 1, n_detectors
        if (trim(id)==trim(detectorinfo(i)%id)) then
            flag = .true.
            field%itemptr => detectorinfo(i)
            return
        end if
    enddo
    do i = 1, n_targets
        if (trim(id)==trim(targetinfo(i)%id)) then
            flag = .true.
            field%itemptr => targetinfo(i)
            return
        end if
    enddo
    do i = 1, n_fires
        if (trim(id)==trim(fireinfo(i)%id)) then
            flag = .true.
            field%itemptr => fireinfo(i)
            return
        end if
    enddo
    
    return
    
    end subroutine find_object
    
    subroutine find_field(fieldid, item, fldptr, found)
    
    character(len=*), intent(in) :: fieldid
    class(cfast_type), target, intent(in) :: item
    type(field_pointer), target, intent(inout) :: fldptr
    logical, intent(out) :: found
    
    found = .false.
    select type (item)
    type is (room_type)
        if (trim(fieldid) == 'WIDTH') then
            found = .true.
            fldptr%realval%val => item%cwidth
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        elseif (trim(fieldid) == 'DEPTH') then
            found = .true.
            fldptr%realval%val => item%cdepth
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        elseif (trim(fieldid) == 'HEIGHT') then
            found = .true.
            fldptr%realval%val => item%cheight
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else 
            call cfastexit('find_field',1)
        end if
    class is (thermal_type)
        if (trim(fieldid) == 'CONDUCTIVITY') then
            found = .true.
            fldptr%realval%val => item%k(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        elseif (trim(fieldid) == 'DENSITY') then
            found = .true.
            fldptr%realval%val => item%rho(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        elseif (trim(fieldid) == 'SPECIFIC_HEAT') then
            found = .true.
            fldptr%realval%val => item%c(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        elseif (trim(fieldid) == 'THICKNESS') then
            found = .true.
            fldptr%realval%val => item%thickness(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        elseif (trim(fieldid) == 'EMISSIVITY') then
            found = .true.
            fldptr%realval%val => item%eps
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else 
            call cfastexit('find_field',2)
        end if
    class is (vent_type)
        if (item%width > 0.0_eb) then
            if (trim(fieldid) == 'WIDTH') then
                found = .true.
                fldptr%realval%val => item%width
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            elseif (trim(fieldid) == 'TOP') then
                found = .true.
                fldptr%realval%val => item%soffit
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            elseif (trim(fieldid) == 'BOTTOM') then
                found = .true.
                fldptr%realval%val => item%sill
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            else
                call cfastexit('find_field',3)
            end if 
        else
            call cfastexit('find_field', 4)
        end if
    class default
        call cfastexit('find_field',99)
    end select
    
    return
    end subroutine find_field
    
end module namelist_input_pp_routines