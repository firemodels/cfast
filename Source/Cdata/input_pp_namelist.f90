   module namelist_input_pp_routines

    use precision_parameters

    use fire_routines, only: flame_height
    use exit_routines, only: cfastexit
    use utility_routines, only: d1mach

    use cfast_types, only: detector_type, fire_type, ramp_type, room_type, table_type, target_type, material_type, &
        vent_type, visual_type, dump_type, cfast_type

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
    use dump_data, only: n_dumps, dumpinfo, num_csvfiles, csvnames
    use fire_data, only: n_fires, fireinfo, n_furn, furn_time, furn_temp, tgignt, lower_o2_limit, mxpts, sigma_s, n_tabls, tablinfo
    use namelist_data, only: input_file_line_number, headflag, timeflag, initflag, miscflag, matlflag, compflag, devcflag, &
        rampflag, tablflag, insfflag, fireflag, ventflag, connflag, diagflag, slcfflag, isofflag, dumpflag
    use option_data, only: option, on, off, ffire, fhflow, fvflow, fmflow, fentrain, fcjet, fdfire, frad, fconduc, fconvec, &
        fdebug, fkeyeval, fpsteady, fpdassl, fgasabsorb, fresidprn, flayermixing
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: n_rooms, roominfo, exterior_ambient_temperature, interior_ambient_temperature, exterior_abs_pressure, &
        interior_abs_pressure, pressure_ref, pressure_offset, exterior_rho, interior_rho, n_vcons, vertical_connections, &
        relative_humidity, adiabatic_walls
    use setup_data, only: iofili, iofill, cfast_version, heading, title, time_end, &
        print_out_interval, smv_out_interval, ss_out_interval, validation_flag, overwrite_testcase, inputfile, project
    use solver_data, only: stpmax, stpmin, stpmin_cnt_max, stpminflag
    use smkview_data, only: n_visual, visualinfo
    use material_data, only: n_matl, material_info
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo
    
    use pp_params, only: mxgenerators, mxseeds, idx_uniform, rand_dist, mxfields, val_types, idx_real, &
        idx_char, idx_int, idx_logic, rnd_seeds, restart_values, mxfiresections, mxpntsarray, idx_user_defined_discrete, &
        mxrndfires, idx_firefiles, idx_stagefires, fire_generator_types, mxrndfires, mxfiregens, mxstats, mxanalys, &
        mximgformats, analysis_list, imgformatext_list, imgformat_list, default_img, idx_const, mxrandfires
        
    use preprocessor_types, only: random_generator_type, field_pointer, fire_generator_type
    use analysis_types, only: stat_type
    use montecarlo_data, only: mc_number_of_cases, generatorinfo, n_generators, n_fields, fieldinfo, mc_write_seeds, n_rndfires, &
        randfireinfo, workpath, parameterfile, n_rndfires, randfireinfo
    use analysis_data, only: n_stats, statinfo, outpath
    
    use namelist_input_routines, only: checkread

    implicit none
    
    private

    public namelist_pp_input, namelist_acc_input, namelist_stt_input

    contains
    
    ! --------------------------- namelist_pp_input ----------------------------------
    subroutine namelist_pp_input

    implicit none
    
    integer :: ios
    
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)
    call read_mhdr(iofili)
    call read_mrnd(iofili)
    call read_mfld(iofili)
    !call read_mfir(iofili)

    close (iofili)
    
    return

    end subroutine namelist_pp_input
    
    ! --------------------------- namelist_acc_input ----------------------------------
    subroutine namelist_acc_input

    implicit none
    
    integer :: ios
    
    close(iofili)
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)
    call read_mhdr(iofili)
    close (iofili)
    
    return

    end subroutine namelist_acc_input
    
    ! --------------------------- namelist_stt_input ----------------------------------
    subroutine namelist_stt_input

    implicit none
    
    integer :: ios
    
    close(iofili)
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)
    if (ios == 0) then
        call read_mstt(iofili)
        close (iofili)
    elseif(ios == 29) then
        write(*,*) 'file ', trim(inputfile), ' not found'
        call cfastexit('namelist_stt_input', 1)
    else
        write(*,*) 'error namelist_stt_input, ios = ', ios
        call cfastexit('namelist_stt_input', 2)
    end if 
    
    return

    end subroutine namelist_stt_input
    
    
    !--------------------------------------------read_mhdr----------------------
    subroutine read_mhdr(lu)
    
    integer :: ios
    real(eb) :: monte_carlo_number_of_cases
    integer :: seeds(2)
    logical :: mhdrflag, write_seeds
    character(len=256) :: work_directory, output_directory, parameter_filename
    
    integer, intent(in) :: lu

    namelist /MHDR/ monte_carlo_number_of_cases, seeds, write_seeds, work_directory, &
        output_directory, parameter_filename

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
        workpath = work_directory
        outpath = output_directory
        parameterfile = parameter_filename

    end if mhdr_flag

    contains

    subroutine set_defaults

    monte_carlo_number_of_cases = -1001
    seeds(1:mxseeds) = -1001
    write_seeds = .false.
    work_directory = 'NULL'
    output_directory = 'NULL'
    parameter_filename = 'NULL'

    end subroutine set_defaults

    end subroutine read_mhdr
    
    
    !--------------------------------------------read_mrnd----------------------
    subroutine read_mrnd(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj
    logical :: mrndflag
    type(random_generator_type), pointer :: genptr
    
    character(len=128) :: id, fyi, value_type
    character(len=35) :: distribution_type
    integer, parameter :: no_seed_value = -1001
    real(eb) :: minimum, maximum, mean, stdev, alpha, beta, peak, constant_value
    integer ::initial_seed_values(mxseeds), ndx
    real(eb) :: discrete_probabilities(mxpntsarray), discrete_real_values(mxpntsarray) 
    integer :: discrete_integer_values(mxpntsarray)
    character(len = 128) :: discrete_string_values(mxpntsarray)
    real(eb) :: real_constant_value
    integer :: integer_constant_value
    character(len=128) :: character_constant_value
    logical :: logical_constant_value
    character(len=128) :: minimum_field_id,  maximum_field_id

    namelist /MRND/ id, fyi, distribution_type, value_type, minimum, maximum, mean, stdev, alpha, beta, &
        peak, initial_seed_values, discrete_real_values, discrete_integer_values, discrete_string_values, &
        discrete_probabilities, real_constant_value, integer_constant_value, character_constant_value, &
        logical_constant_value, minimum_field_id, maximum_field_id
                    
    
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
            if (initial_seed_values(1) /= -1001 .and. initial_seed_values(2) /= -1001) then
                genptr%base_seeds = initial_seed_values
                genptr%use_seeds = .true. 
            end if 
        
            if (trim(distribution_type) == trim(rand_dist(idx_uniform))) then
                genptr%type_dist = distribution_type
                if (value_type == val_types(idx_real)) then
                    genptr%value_type = val_types(idx_real)
                else
                    call cfastexit('READ_MRND',4)
                end if 
                if (trim(minimum_field_id) /= 'NULL') then
                    call genptr%set_min_to_use_field(minimum_field_id)
                else
                    call genptr%set_min_value(minimum)
                end if
                if (trim(maximum_field_id) /= 'NULL') then
                    call genptr%set_max_to_use_field(minimum_field_id)
                else
                    call genptr%set_max_value(maximum)
                end if
            else if (trim(distribution_type) == trim(rand_dist(idx_user_defined_discrete))) then
                ndx = 1
                genptr%type_dist = distribution_type
                ndx_loop: do while(ndx <= mxpntsarray)
                    if (discrete_probabilities(ndx) >= 0._eb) then
                        ndx = ndx + 1
                    else
                        exit ndx_loop
                    end if
                end do ndx_loop
                genptr%num_discrete_values = ndx
                genptr%prob_array(1) = discrete_probabilities(1)
                do jj = 2, ndx
                    genptr%prob_array(jj) = genptr%prob_array(jj-1) + discrete_probabilities(jj)
                end do 
                if (discrete_real_values(1) /= -1001._eb) then
                    genptr%value_type = val_types(idx_real)
                    do jj = 1, ndx
                        genptr%real_array(jj) = discrete_real_values(jj)
                    end do
                else if (discrete_integer_values(1) /= -1001) then
                    genptr%value_type = val_types(idx_int)
                    do jj = 1, ndx
                        genptr%int_array(jj) = discrete_integer_values(jj)
                    end do
                else if (trim(discrete_string_values(1)) /= 'NULL') then
                    genptr%value_type = val_types(idx_char)
                    do jj = 1, ndx
                        genptr%char_array(jj) = discrete_string_values(jj)
                    end do 
                else
                    call cfastexit('read_mrnd', 5)
                end if
            else if (trim(distribution_type) == trim(rand_dist(idx_const))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                genptr%constant = constant_value
            else
                call cfastexit('read_mrnd',1000)
            end if
            
        end do read_mrnd_loop

    end if mrnd_flag
        

    contains

    subroutine set_defaults

    id = 'NULL'
    fyi = 'NULL'
    distribution_type = 'NULL'
    value_type = 'NULL'
    minimum = d1mach(2)
    maximum = -d1mach(2)
    mean = d1mach(2)
    stdev = -1001._eb
    alpha = -1001._eb
    beta = -1001._eb
    initial_seed_values(1:mxseeds) = -1001
    discrete_real_values = -1001._eb
    discrete_integer_values = -1001
    discrete_string_values = 'NULL'
    discrete_probabilities = -1001._eb
    real_constant_value = -1001.0_eb
    integer_constant_value = -1001
    character_constant_value = 'NULL'
    logical_constant_value = .false.
    minimum_field_id = 'NULL'
    maximum_field_id = 'NULL'

    end subroutine set_defaults

    end subroutine read_mrnd
    
    
    !--------------------------------------------read_mfld----------------------
    subroutine read_mfld(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj
    logical :: mfldflag, found, found_rand
    type(field_pointer), pointer :: fldptr
    
    
    character(len=128) :: id, object_id, rand_id, field_name, parameter_header, fyi, field_type, type_of_index 
    real(eb) ::base_scaling_value
    integer ::number_in_index
    logical :: add_to_parameters
    real(eb), dimension(mxpntsarray) :: real_array_values
    integer, dimension(mxpntsarray) :: integer_array_values
    logical, dimension(mxpntsarray) :: logical_array_values
    character(len=128), dimension(mxpntsarray) :: character_array_values, scenario_title_array

    namelist /MFLD/ id, fyi, field_type, object_id, field_name, rand_id, parameter_header, add_to_parameters, &
        real_array_values, integer_array_values, character_array_values, logical_array_values, &
        scenario_title_array, type_of_index, number_in_index, base_scaling_value, add_to_parameters
                    
    
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
            test: do jj = 1, n_generators
                found_rand = .false.
                if (trim(rand_id) == trim(generatorinfo(jj)%id)) then
                    fldptr%genptr => generatorinfo(jj)
                    found_rand = .true.
                    exit test
                end if
            end do test
            if (.not.found_rand) then 
                call cfastexit('read_mfld',5)
            end if
            if (trim(field_type) == fldptr%fld_types(fldptr%idx_value)) then
                fldptr%field_type = trim(field_type)
                call find_object(object_id, fldptr, found)
                if (found) then
                    call find_field(field_name, fldptr%itemptr, fldptr, found)
                    if (trim(generatorinfo(jj)%value_type) /= trim(fldptr%value_type)) then
                        call cfastexit('read_mfld',7)
                    end if
                else
                    call cfastexit('read_mfld',6)
                end if
            elseif (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_index))) then
                call find_object(object_id, fldptr, found)
                if (found) then
                    call find_field(field_name, fldptr%itemptr, fldptr, found)
                    if (trim(type_of_index) /= trim(fldptr%value_type)) then
                        call cfastexit('read_mfld',9)
                    end if
                else
                    call cfastexit('read_mfld',8)
                end if
                fldptr%field_type = trim(field_type)
                fldptr%intval%val => fldptr%index
                fldptr%randptr => fldptr%intval
                if (trim(type_of_index) == val_types(idx_real)) then
                    fldptr%nidx = number_in_index
                    do jj = 1, number_in_index
                        fldptr%real_array(jj) = real_array_values(jj)
                    end do
                elseif (trim(type_of_index) == val_types(idx_int)) then
                    fldptr%nidx = number_in_index
                    do jj = 1, number_in_index
                        fldptr%int_array(jj) = integer_array_values(jj)
                    end do
                elseif (trim(type_of_index) == val_types(idx_char)) then
                    fldptr%nidx = number_in_index
                    do jj = 1, number_in_index
                        fldptr%char_array(jj) = character_array_values(jj)
                    end do
                elseif (trim(type_of_index) == val_types(idx_logic)) then
                    fldptr%nidx = number_in_index
                    do jj = 1, number_in_index
                        fldptr%logic_array(jj) = logical_array_values(jj)
                    end do
                else
                    call cfastexit('read_mfld',10)
                end if
            elseif (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_scale))) then
                call find_object(object_id, fldptr, found)
                if (found) then
                    call find_field(field_name, fldptr%itemptr, fldptr, found)
                    if (trim(val_types(idx_real)) /= trim(fldptr%value_type)) then
                        call cfastexit('read_mfld',12)
                    end if
                else
                    call cfastexit('read_mfld',11)
                end if
                fldptr%field_type = trim(field_type)
                fldptr%scaleval%val => fldptr%scale_value
                fldptr%scaleval%value_type = val_types(idx_real)
                fldptr%randptr => fldptr%scaleval
                if (base_scaling_value > 1.0_eb) then
                    fldptr%scale_base_value = base_scaling_value
                else
                    fldptr%scale_base_value = fldptr%realval%val
                end if 
            elseif (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_label))) then
                fldptr%field_type = trim(field_type)
                fldptr%indexval%val => fldptr%index
                fldptr%randptr => fldptr%indexval
                scenaro_loop: do jj = 1, mxpntsarray
                    if (trim(scenario_title_array(jj)) == 'NULL') then
                        fldptr%nlabel = jj - 1
                        exit scenaro_loop
                    else
                        fldptr%char_array(jj) = scenario_title_array(jj)
                    end if    
                    fldptr%nlabel = jj
                end do scenaro_loop
            else
                call cfastexit('read_mfld',13)
            end if
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
    parameter_header = 'NULL'
    base_scaling_value = -1
    

    end subroutine set_defaults

    end subroutine read_mfld
    
    !
    !--------------------------------------------read_mstt----------------------
    !
    subroutine read_mstt(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj, kk, iend, iex, iendname
    integer :: ncnts(mxanalys), idx
    logical :: msttflag, found
    type(stat_type), pointer :: statptr
    character(len=5) :: extension
    
    
    character(len=128) :: id, fyi, analysis_type, input_filename, output_filename, error_filename, &
        log_filename, column_title

    namelist /MSTT/ id, fyi, analysis_type, input_filename, output_filename, error_filename, log_filename, &
        column_title 
                    
    
    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &HEAD input
    mstt_loop: do
        call checkread ('MSTT', lu, ios)
        if (ios==0) msttflag=.true.
        if (ios==1) then
            exit mstt_loop
        end if
        read(lu,MSTT,iostat=ios)
        n_stats = n_stats + 1
        if (ios>0) then
            write(iofill, '(a)') '***Error in &MSTT: Invalid specification for inputs.'
            call cfastexit('read_mstt',1)
        end if
    end do mstt_loop

    if (n_stats>mxstats) then
        write (*,'(a,i3)') '***Error: Too many fields in input data file. Limit is ', mxstats
        write (iofill,'(a,i3)') '***Error: Too many fields in input data file. Limit is ', mxstats
        call cfastexit('read_mstt',2)
    end if

    if (.not.msttflag) then
        write (*, '(/, "***Error: &MSTT inputs are required.")')
        write (iofill, '(/, "***Error: &MTT inputs are required.")')
        call cfastexit('read_mstt',3)
    end if

    mstt_flag: if (msttflag) then

        rewind (lu)

        ! Assign value to CData variables for further calculations
        read_mstt_loop: do ii=1,n_stats

            statptr => statinfo(ii)

            call checkread('MSTT',lu,ios)
            call set_defaults
            read(lu,MSTT)
            
            statptr => statinfo(ii)
            idx = 0
            do jj = 1, mxanalys
                if (trim(analysis_type) == trim(analysis_list(jj))) then
                    idx = jj
                    statptr%analysis_type = analysis_list(jj)
                    ncnts(jj) = ncnts(jj) + 1
                    exit
                end if
            end do
            if (idx == 0) then
                write(*,*) 'analysis_type ',trim(analysis_type),' not recognized'
                call cfastexit('read_mstt', 4)
            end if
            statptr%id = id
            statptr%fyi = fyi
            if (trim(input_filename) == 'NULL') then
                statptr%infile = trim(project) // '_parameters.csv'
            else
                statptr%infile = input_filename
            end if 
            if (trim(output_filename) == 'NULL') then
                write(statptr%outfile, '(a,a1,i0,a1,a)') trim(project), '_', ncnts(idx), '_', trim(analysis_list(idx))
                statptr%img_format = imgformat_list(default_img)
            else
                iend = len_trim(output_filename)
                extension = ' '
                statptr%img_format = 'NULL'
                do jj = iend, 1, -1
                    if (output_filename(jj:jj) == '.') then
                        extension = output_filename(jj+1:jj+3)
                        do kk = 1, mximgformats
                            if (trim(extension) == imgformatext_list(kk)) then
                                statptr%img_format = ' '
                                statptr%img_format = imgformat_list(kk)
                                statptr%outfile = output_filename(1:jj-1)
                                exit
                            end if
                        end do 
                        exit
                    end if
                end do
                if (statptr%img_format(1:4) == 'NULL') then
                    write(*,*) 'extension file format not recognized'
                    call cfastexit('read_mstt', 5)
                end if
            end if
            statptr%col_title = column_title 
                
        end do read_mstt_loop

    end if mstt_flag
        

    contains

    subroutine set_defaults

    id = 'NULL'
    fyi = 'NULL'
    input_filename = 'NULL'
    output_filename = 'NULL'
    error_filename = 'NULL'
    log_filename = 'NULL'
    column_title = 'NULL'

    end subroutine set_defaults
    
    end subroutine read_mstt
    
    !
    !--------read_mfir--------
    !
    
    subroutine read_mfir(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj
    logical :: mfirflag
    character(len=128) :: id, fyi, fire_id, base_fire_id, scaling_fire_hrr_random_generator_id, &
        smolder_random_generator_id, scaling_fire_time_random_generator_id, &
        smolder_time_random_generator_id, parameter_header
    logical :: first_time_point_smoldering, modify_fire_area_to_match_hrr, add_to_parameters
    type(fire_generator_type), pointer :: fire
    

    namelist /MFIR/ id, fyi, fire_id, base_fire_id, scaling_fire_hrr_random_generator_id, &
        smolder_random_generator_id, scaling_fire_time_random_generator_id, &
        smolder_time_random_generator_id, first_time_point_smoldering, &
        modify_fire_area_to_match_hrr, add_to_parameters, parameter_header
    
    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &HEAD input
    mfir_loop: do
        call checkread ('MFIR', lu, ios)
        if (ios==0) mfirflag=.true.
        if (ios==1) then
            exit mfir_loop
        end if
        read(lu,MFIR,iostat=ios)
        n_rndfires = n_rndfires + 1
        if (ios>0) then
            write(iofill, '(a)') '***Error in &MFIR: Invalid specification for inputs.'
            call cfastexit('read_mfir',1)
        end if
    end do mfir_loop

    if (n_rndfires>mxrndfires) then
        write (*,'(a,i3)') '***Error: Too many fire generators in input data file. Limit is ', mxfiregens
        write (iofill,'(a,i3)') '***Error: Too many fire generators in input data file. Limit is ', mxfiregens
        call cfastexit('read_mfir',2)
    end if

    if (.not.mfirflag) then
        write (*, '(/, "***Error: &MFIR inputs are required.")')
        write (iofill, '(/, "***Error: &MFIR inputs are required.")')
        call cfastexit('read_mfir',3)
    end if

     ! we found one. read it (only the first one counts; others are ignored)
    mfir_flag: if (mfirflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_mfir_loop: do ii=1,n_rndfires
            fire => randfireinfo(ii)
            fire%id = id
            fire%fyi = fyi
            fire%fireid = fire_id
            fire%basefireid = base_fire_id
            do jj = 1, n_fires
                if (trim(fireinfo(jj)%id) == trim(fire%fireid)) then
                    fire%fire => fireinfo(jj)
                elseif (trim(fireinfo(jj)%id) == trim(fire%basefireid)) then
                    fire%base => fireinfo(jj)
                end if
            end do
            if (trim(scaling_fire_hrr_random_generator_id) /= 'NULL') then
                do jj = 1, n_generators
                    if (trim(scaling_fire_hrr_random_generator_id)== trim(generatorinfo(jj)%id)) then
                        fire%hrrscale => generatorinfo(jj)
                        fire%scalehrr = .true.
                    end if
                end do
                if (fire%scalehrr) then
                    fire%hrrscaleval%val => fire%hrrscalevalue
                else
                    call cfastexit('MFIR', 4)
                end if 
            end if
            if (trim(scaling_fire_time_random_generator_id) /= 'NULL') then
                do jj = 1, n_generators
                    if (trim(scaling_fire_time_random_generator_id)== trim(generatorinfo(jj)%id)) then
                        fire%timescale => generatorinfo(jj)
                        fire%scaletime = .true.
                    end if
                end do
                if (fire%scaletime) then
                    fire%timescaleval%val => fire%timescalevalue
                else
                    call cfastexit('MFIR', 5)
                end if 
            end if
            if (add_to_parameters) then
                fire%add_to_parameters = add_to_parameters
                if (trim(parameter_header) == 'NULL') then
                    fire%parameter_header = trim(fire_id) // '_' // 'scaling'
                else
                    fire%parameter_header = parameter_header
                end if
            end if
        end do read_mfir_loop
    end if mfir_flag
    
    contains
    
    subroutine set_defaults
    
    id = 'NULL'
    fire_id = 'NULL' 
    base_fire_id = 'NULL'
    scaling_fire_hrr_random_generator_id = 'NULL'
    smolder_random_generator_id = 'NULL'
    scaling_fire_time_random_generator_id = 'NULL'
    smolder_time_random_generator_id = 'NULL'
    first_time_point_smoldering = .false.
    modify_fire_area_to_match_hrr = .false. 
    add_to_parameters = .false. 
    parameter_header = 'NULL'
    
    
    end subroutine set_defaults
    
    end subroutine read_mfir
    
    !-------------------------------find_object--------------------------------------
    
    subroutine find_object(id, field, flag)
    character(len=*), intent(in) :: id
    type(field_pointer), intent(inout) :: field
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
    do i = 1, n_matl
        if (trim(id)==trim(material_info(i)%id)) then
            flag = .true.
            field%itemptr => material_info(i)
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
    class is (material_type)
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
        else if (item%maxflow > 0.0_eb) then
            if (trim(fieldid) == 'FLOW') then 
                found = .true.
                fldptr%realval%val => item%maxflow
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            end if
        else
            call cfastexit('find_field', 4)
        end if
    class is (detector_type)
        if (trim(fieldid) == 'TRIGGER') then
            found = .true.
            fldptr%realval%val => item%trigger
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        elseif (trim(fieldid)  == 'TRIGGER_SMOLDER') then
            found = .true.
            fldptr%realval%val => item%trigger_smolder
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else
            call cfastexit('find_field',5)
        end if
    class is (fire_type)
        if (trim(fieldid) == 'ROOM') then
            found = .true.
            fldptr%intval%val => item%room
            fldptr%value_type = val_types(idx_int)
            fldptr%valptr => fldptr%intval
        end if
    class default
        call cfastexit('find_field',99)
    end select
    
    return
    end subroutine find_field
    
end module namelist_input_pp_routines