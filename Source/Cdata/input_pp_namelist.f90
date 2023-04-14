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
    use setup_data, only: iofili, iofill, cfast_version, title, time_end, print_out_interval, smv_out_interval, &
        ss_out_interval, validation_flag, overwrite_testcase, inputfile, project, datapath, errormessage
    use solver_data, only: stpmax, stpmin, stpmin_cnt_max, stpminflag
    use smkview_data, only: n_visual, visualinfo
    use material_data, only: n_matl, material_info
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo
    
    use pp_params, only: mxgenerators, mxseeds, idx_uniform, rand_dist, mxfields, val_types, idx_real, &
        idx_char, idx_int, idx_logic, rnd_seeds, restart_values, mxfiresections, mxpntsarray, idx_user_defined_discrete, &
        mxrndfires, idx_firefiles, idx_stagefires, fire_generator_types, mxrndfires, mxfiregens, mxstats, mxanalys, &
        mximgformats, analysis_list, imgformatext_list, imgformat_list, default_img, idx_const, mxrandfires, idx_linear, &
        idx_normal, idx_log_normal, idx_trun_normal, idx_trun_log_normal, idx_beta, idx_gamma, &
        idx_user_defined_continous_interval
        
    use preprocessor_types, only: preprocessor_type, random_generator_type, field_pointer, fire_generator_type
    use analysis_types, only: stat_type
    use diagnostic_types, only: diagnostic_type
    use montecarlo_data, only: mc_number_of_cases, generatorinfo, n_generators, n_fields, fieldinfo, mc_write_seeds, n_rndfires, &
        randfireinfo, workpath, parameterfile, n_rndfires, randfireinfo, fieldptr, dummy, validation_output
    use analysis_data, only: n_stats, statinfo, outpath
    use diagnostic_data, only: n_diag, diaginfo, diagptr
    
    use namelist_input_routines, only: checkread, read_time

    implicit none
    
    private

    public namelist_pp_input, namelist_acc_input, namelist_stt_input, namelist_diag_input

    contains
    
    ! --------------------------- namelist_pp_input ----------------------------------
    subroutine namelist_pp_input

    implicit none
    
    integer :: ios
    
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)
    call read_mhdr(iofili)
    call read_mrnd(iofili)
    call read_mfld(iofili)
    call read_mfir(iofili)

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
        write(errormessage,'(3a)') 'Error***, CData input file, ', trim(inputfile), ', not found'
        call cfastexit('namelist_stt_input', 1)
    else
        write(errormessage,'(a,i0)') 'Error***, error reading CData input file. Error code = ', ios
        call cfastexit('namelist_stt_input', 2)
    end if 
    
    return

    end subroutine namelist_stt_input
    
    ! --------------------------- namelist_diag_input ----------------------------------
    subroutine namelist_diag_input

    implicit none
    
    integer :: ios
    
    close(iofili)
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)
    if (ios == 0) then
        call read_time(iofili)
        call read_mdia(iofili)
        close (iofili)
    elseif(ios == 29) then
        write(errormessage,'(3a)') 'Error***, CData input file, ', trim(inputfile), ', not found'
        call cfastexit('namelist_dia_input', 1)
    else
        write(errormessage,'(a,i0)') 'Error***, error reading CData input file. Error code = ', ios
        call cfastexit('namelist_dia_input', 2)
    end if 
    
    return

    end subroutine namelist_diag_input
    
    
    !--------------------------------------------read_mhdr----------------------
    subroutine read_mhdr(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios
    integer :: number_of_cases
    integer :: seeds(2)
    logical :: mhdrflag, write_seeds, write_validation_output
    character(len=256) :: work_folder, output_folder, parameter_file

    namelist /MHDR/ number_of_cases, seeds, write_seeds, work_folder, &
        output_folder, parameter_file, write_validation_output

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
            write(errormessage,'(a)') '***Error in &MHDR: Invalid variable in specification for inputs.'
            call cfastexit('read_mhdr',1)
        end if
    end do mhdr_loop

    if (.not.mhdrflag) then
        write (errormessage, '(a)') '***Error: &MHDR inputs are required. None included in input file'
        call cfastexit('read_mhdr',2)
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    mhdr_flag: if (mhdrflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('MHDR',lu,ios)
        call set_defaults
        read(lu,MHDR)
        
        mc_number_of_cases = number_of_cases
        rnd_seeds(1:2) = seeds(1:2)
        if (seeds(1) /= -1001 .and. seeds(2) /= -1001) then
            call RANDOM_SEED(PUT=seeds)
        end if
        mc_write_seeds = write_seeds
        if (trim(work_folder) == 'NULL') then
            workpath = datapath
        else
            workpath = work_folder
        end if 
        outpath = output_folder
        parameterfile = parameter_file
        validation_output = write_validation_output

    end if mhdr_flag

    contains

    subroutine set_defaults

    number_of_cases = 1
    seeds(1:mxseeds) = -1001
    write_seeds = .true.
    work_folder = 'NULL'
    output_folder = 'NULL'
    parameter_file = 'NULL'
    write_validation_output = validation_flag

    end subroutine set_defaults

    end subroutine read_mhdr
    
    
    !--------------------------------------------read_mrnd----------------------
    subroutine read_mrnd(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj
    logical :: mrndflag
    type(random_generator_type), pointer :: genptr
    
    character(len=128) :: id, fyi
    character(len=35) :: distribution_type
    integer, parameter :: no_seed_value = -1001
    real(eb) :: minimum, maximum, mean, stdev, alpha, beta, peak, minimum_offset, maximum_offset
    integer :: random_seeds(mxseeds), ndx
    real(eb) :: probabilities(mxpntsarray), values(mxpntsarray) 
    real(eb) :: constant
    character(len=128) :: minimum_field,  maximum_field, add_field

    namelist /MRND/ id, fyi, distribution_type, minimum, maximum, mean, stdev, alpha, beta, &
        peak, random_seeds, values, probabilities, constant, minimum_field, maximum_field, &
        minimum_offset, maximum_offset, add_field

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
            write(errormessage,'(a, i0)') '***Error in &MRND: Invalid specification for inputs. Generator ', n_generators
            call cfastexit('read_mrnd',1)
        end if
    end do mrnd_loop

    if (n_generators>mxgenerators) then
        write (errormessage,'(a,i0)') '***Error: Too many random generators in input data file. Limit is ', mxgenerators
        call cfastexit('read_mrnd',2)
    end if

    if (.not.mrndflag) then
        write (errormessage,'(a)') '***Error: At least one &MRND input is required.'
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
            if (random_seeds(1) /= -1001 .and. random_seeds(2) /= -1001) then
                genptr%base_seeds = random_seeds
                genptr%use_seeds = .true. 
            end if 
            genptr%min_offset = minimum_offset
            genptr%max_offset = maximum_offset
            if (trim(minimum_field) /= 'NULL') then
                call genptr%set_min_to_use_field(minimum_field)
            else
                call genptr%set_min_value(minimum)
            end if
            if (trim(maximum_field) /= 'NULL') then
                call genptr%set_max_to_use_field(maximum_field)
            else
                call genptr%set_max_value(maximum)
            end if
            if (trim(add_field) /= 'NULL') then
                call genptr%set_add_to_use_field(add_field)
            else
                call genptr%set_add_value(0.0_eb)
            end if
        
            if (trim(distribution_type) == trim(rand_dist(idx_uniform))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
            else if (trim(distribution_type) == trim(rand_dist(idx_user_defined_discrete))) then
                ndx = 1
                genptr%type_dist = distribution_type
                ndx_loop: do while(ndx <= mxpntsarray)
                    if (probabilities(ndx) >= 0._eb) then
                        ndx = ndx + 1
                    else
                        exit ndx_loop
                    end if
                end do ndx_loop
                genptr%num_discrete_values = ndx - 1
                genptr%prob_array(1) = probabilities(1)
                do jj = 2, ndx
                    genptr%prob_array(jj) = genptr%prob_array(jj-1) + probabilities(jj)
                end do 
                if (values(1) /= -1001._eb) then
                    genptr%value_type = val_types(idx_real)
                    do jj = 1, ndx
                        genptr%real_array(jj) = values(jj)
                    end do
                else
                    write (errormessage,'(2a)') &
                        '***Error, Invalid values specified for discrete values distribution for &MRND ', &
                        trim(genptr%id)
                    call cfastexit('read_mrnd', 5)
                end if 
            else if (trim(distribution_type) == trim(rand_dist(idx_user_defined_continous_interval))) then
                ndx = 0
                genptr%type_dist = distribution_type
                ndx_loop2: do while(ndx <= mxpntsarray)
                    if (probabilities(ndx + 1) >= 0._eb) then
                        ndx = ndx + 1
                    else
                        exit ndx_loop2
                    end if
                end do ndx_loop2
                genptr%prob_array(1) = probabilities(1)
                do jj = 2, ndx
                    genptr%prob_array(jj) = genptr%prob_array(jj-1) + probabilities(jj)
                end do  
                if (genptr%prob_array(ndx) /= 1) then
                    write (errormessage, '(2a)') '***Error, probablities must add up to 1.0, &MRND ', trim(genptr%id)
                    call cfastexit('read_mrnd', 6)
                end if
                if (values(1) /= -1001._eb) then
                    genptr%value_type = val_types(idx_real)
                    genptr%real_array(1) = values(1)
                    do jj = 2, ndx
                        if (values(jj) == -1001._eb) then
                            write (errormessage,'(3a)') '***Error, Must be equal number of values as probabilities' &
                                ,' &MRND ', trim(genptr%id)
                            call cfastexit('read_mrnd', 7)
                        end if
                        genptr%real_array(jj) = values(jj)
                    end do
                else
                    write (errormessage,'(2a)') &
                        '***Error, Invalid values specified for continuous values distribution for &MRND ', &
                        trim(genptr%id)
                    call cfastexit('read_mrnd', 8)
                end if 
            else if (trim(distribution_type) == trim(rand_dist(idx_const))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                genptr%constant = constant
            else if (trim(distribution_type) == trim(rand_dist(idx_linear))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                genptr%linear_delta = (maximum - minimum)/(mc_number_of_cases - 1)
                genptr%constant = minimum - genptr%linear_delta
            else if (trim(distribution_type) == trim(rand_dist(idx_log_normal))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                genptr%mean = mean
                genptr%stdev = stdev
            else if (trim(distribution_type) == trim(rand_dist(idx_trun_normal))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                genptr%mean = mean
                genptr%stdev = stdev
            else if (trim(distribution_type) == trim(rand_dist(idx_trun_log_normal))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                genptr%mean = mean
                genptr%stdev = stdev
            else if (trim(distribution_type) == trim(rand_dist(idx_beta))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                if (alpha <= 0) then 
                    write (errormessage, '(2a)') '***Error, For BETA distribution ALPHA nust be >0 in& MRND ', &
                        trim(genptr%id)
                    call cfastexit('read_mrnd', 9)
                end if 
                if (beta <= 0) then 
                    write (errormessage, '(2a)') '***Error, For BETA distribution BETA nust be >0 in &MRND ', &
                        trim(genptr%id)
                    call cfastexit('read_mrnd', 10)
                end if 
                genptr%alpha = alpha
                genptr%beta = beta
                if (trim(minimum_field) /= 'NULL' .and. trim(maximum_field) /= 'NULL') then
                    if (genptr%minimum() == -d1mach(2) .and. genptr%maximum() == d1mach(2)) then 
                        call genptr%set_min_value(0.0_eb)
                        call genptr%set_max_value(1.0_eb)
                    end if
                end if
            else if (trim(distribution_type) == trim(rand_dist(idx_gamma))) then
                genptr%type_dist = distribution_type
                genptr%value_type = val_types(idx_real)
                if (alpha <= 0) then 
                    write (errormessage, '(2a)') '***Error, For GAMMA distribution ALPHA nust be >0 in &MRND ', &
                        trim(genptr%id)
                    call cfastexit('read_mrnd', 11)
                end if 
                if (beta <= 0) then 
                    write (errormessage, '(2a)') '***Error, For GAMMA distribution BETA nust be >0 in &MRND ', &
                        trim(genptr%id)
                    call cfastexit('read_mrnd', 12)
                end if 
                genptr%alpha = alpha
                genptr%beta = beta
            else
                write (errormessage,'(4a)') '***Error, Invalid distribution type specified in &MRND ', trim(genptr%id), &
                    ' type ', trim(distribution_type)
                call cfastexit('read_mrnd',1000)
            end if
            
        end do read_mrnd_loop

    end if mrnd_flag
        

    contains

    subroutine set_defaults

    id = 'NULL'
    fyi = 'NULL'
    distribution_type = 'NULL'
    minimum = -d1mach(2)
    maximum = d1mach(2)
    mean = 0.0_eb
    stdev = -1001._eb
    alpha = -1001._eb
    beta = -1001._eb
    random_seeds(1:mxseeds) = -1001
    values = -1001._eb
    probabilities = -1001._eb
    constant = -1001.0_eb
    minimum_field = 'NULL'
    maximum_field = 'NULL'
    add_field = 'NULL'
    minimum_offset = 0.0_eb
    maximum_offset = 0.0_eb

    end subroutine set_defaults

    end subroutine read_mrnd
    
    
    !--------------------------------------------read_mfld----------------------
    subroutine read_mfld(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj, idx, idx1, idx2
    logical :: mfldflag, found, found_rand
    type(field_pointer), pointer :: fldptr
    integer, dimension(mxfields) :: tmpptr
    
    
    character(len=128) :: id, rand_id, parameter_column_label, fyi, field_type 
    character(len=128), dimension(2) :: field
    real(eb) :: base_scaling_value
    integer :: position
    logical :: add_to_parameters
    real(eb), dimension(mxpntsarray) :: real_values
    integer, dimension(mxpntsarray) :: integer_values
    logical, dimension(mxpntsarray) :: logical_values
    character(len=128), dimension(mxpntsarray) :: string_values, scenario_titles

    namelist /MFLD/ id, fyi, field_type, field, rand_id, parameter_column_label, add_to_parameters, &
        real_values, integer_values, string_values, logical_values, &
        scenario_titles, base_scaling_value, position
                    
    
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
            write(errormessage,'(a,i0)') '***Error in &MFLD, Invalid variable in specification for inputs. &MFLD input ', n_fields
            call cfastexit('read_mfld', 1)
        end if
    end do mfld_loop

    if (n_fields>mxfields) then
        write (errormessage,'(a,i0)') '***Error: Too many &MFLD inputs in data file. Limit is ', mxfields
        call cfastexit('read_mfld',2)
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
                write(errormessage,'(a)') '***Error, &MFLD ID field requires a value. No input found'
                call cfastexit('read_mfld',4)
            end if
            fldptr%id = id
            found_rand = .false.
            if (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_pointer))) then
                found_rand = .true.
            else 
                test: do jj = 1, n_generators
                    if (trim(rand_id) == trim(generatorinfo(jj)%id)) then
                        fldptr%genptr => generatorinfo(jj)
                        found_rand = .true.
                        exit test
                    end if
                end do test
            end if
            if (.not.found_rand) then 
                write (errormessage,'(2a)') '***Error, No matching &MRND input found for random generator in &MFLD,', trim(rand_id)
                call cfastexit('read_mfld',5)
            end if
            
            ! Value Type
            if (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_value))) then
                fldptr%field_type = trim(field_type)
                fldptr%randptr%val => fldptr%rand_value
                call find_object(field(1), fldptr, found)
                if (found) then
                    call find_field(field(2), fldptr%itemptr, fldptr, found)
                    if (.not.found) then 
                        write(errormessage, '(4a)') '***Error, FIELD(2) not found in &MFLD, ID = ', trim(fldptr%id), &
                            ' FIELD(2) = ',trim(field(2))
                    end if
                else 
                    write (errormessage,'(2a)') &
                        '***Error, FIELD(1) not found in &MFLD, ID = ', trim(fldptr%id), ' FIELD(1) = ', trim(field(1))
                    call cfastexit('read_mfld',6)
                end if
            
            ! Index Type
                
            elseif (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_index))) then
                call find_object(field(1), fldptr, found)
                if (found) then
                    call find_field(field(2), fldptr%itemptr, fldptr, found)
                    if (.not.found) then
                        write (errormessage,'(5a)') '***Error, in &MFLD ',trim(fldptr%id), &
                            ' Entry 2 in FIELD not found,', trim(field(1)),', ', trim(field(2))
                        call cfastexit('read_mfld',9)
                    end if
                else
                    write (errormessage,'(5a)') '***Error,  in &MFLD,', trim(fldptr%id), 'Entry 1 not found', &
                            ' Entry 1 in FIELD not found,', trim(field(1)),', ', trim(field(2))
                    call cfastexit('read_mfld',8)
                end if
                fldptr%field_type = trim(field_type)
                fldptr%randptr%val => fldptr%rand_value
                fldptr%nidx = fldptr%genptr%num_discrete_values
                if (trim(fldptr%value_type) == val_types(idx_real)) then
                    do jj = 1, fldptr%nidx
                        if (real_values(jj) == -1001.0_eb) then
                            write(errormessage, '(3a,i4,a)') '***Error, for &MFLD ',trim(fldptr%id), &
                                ' not enough REAL_VALUES defined, ', fldptr%nidx, ' needed.'
                            call cfastexit('read_mfld', 10)
                        end if
                        fldptr%real_array(jj) = real_values(jj)
                    end do
                elseif (trim(fldptr%value_type) == val_types(idx_int)) then
                    do jj = 1, fldptr%nidx
                        if (integer_values(jj) == -1001) then
                            write(errormessage, '(3a,i4,a)') '***Error, for &MFLD ',trim(fldptr%id), &
                                ' not enough INTEGER_VALUES defined, ', fldptr%nidx, ' needed.'
                            call cfastexit('read_mfld', 11)
                        end if
                        fldptr%int_array(jj) = integer_values(jj)
                    end do
                elseif (trim(fldptr%value_type) == val_types(idx_char)) then
                    do jj = 1, fldptr%nidx
                        if (trim(string_values(jj)) == 'NULL') then
                            write(errormessage, '(3a,i4,a)') '***Error, for &MFLD ',trim(fldptr%id), &
                                ' not enough STRING_VALUES defined, ', fldptr%nidx, ' needed.'
                            call cfastexit('read_mfld', 12)
                        end if
                        fldptr%char_array(jj) = string_values(jj)
                    end do
                elseif (trim(fldptr%value_type) == val_types(idx_logic)) then
                    do jj = 1, fldptr%nidx
                        fldptr%logic_array(jj) = logical_values(jj)
                    end do
                else
                    write (errormessage,'(2a)') '***Error, Invalid value type in &MFLD,', trim(fldptr%value_type)
                    call cfastexit('read_mfld',13)
                end if
                
            ! Scale Type
                
            elseif (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_scale))) then
                call find_object(field(1), fldptr, found)
                if (found) then
                    call find_field(field(2), fldptr%itemptr, fldptr, found)
                    if (trim(val_types(idx_real)) /= trim(fldptr%value_type)) then
                        write (errormessage,'(2a)') &
                            '***Error, Value type in &MRND input does not match type expected in scaling &MFLD,', &
                            trim(fldptr%value_type)
                        call cfastexit('read_mfld',15)
                    end if
                else
                    call cfastexit('read_mfld',14)
                end if
                fldptr%field_type = trim(field_type)
                fldptr%randptr%val => fldptr%scale_value
                if (base_scaling_value > 0.0_eb) then
                    fldptr%scale_base_value = base_scaling_value
                else
                    fldptr%scale_base_value = fldptr%realval%val
                end if 
            
            ! Label Type
                
            elseif (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_label))) then
                fldptr%field_type = trim(field_type)
                fldptr%randptr%val => fldptr%rand_value
                fldptr%charval%val => fldptr%labelval
                fldptr%value_type = val_types(idx_char)
                fldptr%valptr => fldptr%charval
                scenaro_loop: do jj = 1, mxpntsarray
                    if (trim(string_values(jj)) == 'NULL') then
                        fldptr%nlabel = jj - 1
                        exit scenaro_loop
                    else
                        fldptr%char_array(jj) = string_values(jj)
                    end if    
                    fldptr%nlabel = jj
                end do scenaro_loop
            
            ! Pointer Type
            elseif (trim(field_type) == trim(fldptr%fld_types(fldptr%idx_pointer))) then
                fldptr%field_type = trim(field_type)
                call find_object(field(1), fldptr, found)
                if (found) then
                    call find_field(field(2), fldptr%itemptr, fldptr, found)
                else 
                    write (errormessage,'(2a)') '***Error, Invalid value type in &MFLD,', trim(field_type)
                    call cfastexit('read_mfld',16)
                end if
            else
                call cfastexit('read_mfld',17)
            end if
            
            ! Add Parameters
            
            if (add_to_parameters) then
                fldptr%add_to_parameters = add_to_parameters
                if (trim(parameter_column_label) == 'NULL') then
                    fldptr%parameter_column_label = trim(field(1)) // '_' // trim(field(2))
                else
                    fldptr%parameter_column_label = parameter_column_label
                end if
            end if
            
        end do read_mfld_loop
        
        idx = 0
        do ii = 1, n_fields
            fldptr => fieldinfo(ii)
            if (.not. fldptr%dependencies()) then
                tmpptr(ii) = -1001
                idx = idx + 1
                fieldptr(idx) = ii
            else 
                tmpptr(ii) = ii
            end if 
        end do
        if (idx == 0) then
            write (errormessage,'(a)') 'Error***, Missing linked dependency in &MFLD. Cannot resolve linked reference. '
            call cfastexit('read_mfld', 1000000)
        end if
        idx1 = 1
        idx2 = idx
        do while(idx < n_fields)
            found = .false.
            do ii = 1, n_fields
                if (tmpptr(ii) == ii) then
                    fldptr => fieldinfo(ii)
                    if (.not. fldptr%dependencies_set()) then
                        if (.not. fldptr%max_dependency_set()) then
                            do jj = idx1, idx2
                                if (trim(fldptr%max_dependency()) == &
                                    trim(fieldinfo(fieldptr(jj))%id)) then
                                    call fldptr%set_max_field(fieldinfo(fieldptr(jj)))
                                    found = .true.
                                end if
                            end do
                        end if 
                        if (.not. fldptr%min_dependency_set()) then
                            do jj = idx1, idx2
                                if (trim(fldptr%min_dependency()) == &
                                    trim(fieldinfo(fieldptr(jj))%id)) then
                                    call fldptr%set_min_field(fieldinfo(fieldptr(jj)))
                                    found = .true.
                                end if
                            end do
                        end if 
                        if (.not. fldptr%add_dependency_set()) then
                            do jj = idx1, idx2
                                if (trim(fldptr%add_dependency()) == &
                                    trim(fieldinfo(fieldptr(jj))%id)) then
                                    call fldptr%set_add_field(fieldinfo(fieldptr(jj)))
                                    found = .true.
                                end if
                            end do
                        end if 
                    end if 
                    if (fldptr%dependencies_set()) then
                        idx = idx + 1
                        fieldptr(idx) = ii
                        tmpptr(ii) = -1001
                    end if
                end if
            end do
            if (.not. found) then
                write (errormessage,'(a)') 'Error***, Missing linked dependency in &MFLD. Cannot resolve added reference. '
                call cfastexit('READ_MFLD',1000001)
            else
                idx2 = idx
            end if
        end do 

    end if mfld_flag
        

    contains

    subroutine set_defaults

    id = 'NULL'
    field(1) = 'NULL'
    field(2) = 'NULL'
    rand_id = 'NULL'
    add_to_parameters = .true.
    parameter_column_label = 'NULL'
    base_scaling_value = -1
    position = 1
    real_values = -1001.0_eb
    integer_values = -1001
    string_values = 'NULL'
    logical_values = .FALSE.

    end subroutine set_defaults

    end subroutine read_mfld
    
    !
    !--------------------------------------------read_mstt----------------------
    !
    subroutine read_mstt(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj, kk, iend
    integer :: ncnts(mxanalys), idx
    logical :: msttflag
    type(stat_type), pointer :: statptr
    character(len=5) :: extension
    
    
    character(len=128) :: id, fyi, analysis_type, input_filename, output_filename, error_filename, &
        log_filename, column_label

    namelist /MSTT/ id, fyi, analysis_type, input_filename, output_filename, error_filename, log_filename, &
        column_label 
                    
    
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
            write(errormessage,'(a,i0)') '***Error in &MSTT, Invalid variable in specification for inputs. &MSTT input ', n_stats
            call cfastexit('read_mstt',1)
        end if
    end do mstt_loop

    if (n_stats>mxstats) then
        write (errormessage,'(a,i0)') '***Error: Too many fields in input data file. Limit is ', mxstats
        call cfastexit('read_mstt',2)
    end if

    if (.not.msttflag) then
        write (errormessage,'(a)') '***Error: No &MSTT inputs in input file.'
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
                write(errormessage,*) '***Error, MSTT analysis_type ',trim(analysis_type),' not recognized'
                call cfastexit('read_mstt', 4)
            end if
            statptr%id = id
            statptr%fyi = fyi
            if (trim(input_filename) == 'NULL') then
                statptr%infile = trim(project) // '_accumulate.csv'
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
                    write(errormessage,'(a)') '***Error, &MSTT R extension file format not recognized'
                    call cfastexit('read_mstt', 5)
                end if
            end if
            statptr%col_title = column_label
                
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
    column_label = 'NULL'

    end subroutine set_defaults
    
    end subroutine read_mstt

    !
    !--------read_mfir--------
    !
    subroutine read_mfir(lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, ii, jj, kk, idx_firepts
    logical :: mfirflag, found, found2, flameset, smolderset, generator_is_time_to_1054_kW
    character(len=128) :: id, fyi, fire_id, base_fire_id, scaling_fire_hrr_random_generator_id, &
        scaling_fire_time_random_generator_id, hrr_scale_column_label, time_scale_column_label, &
        fire_compartment_random_generator_id, fire_compartment_id_column_label, &
        flaming_smoldering_incipient_random_generator_id, flaming_incipient_delay_random_generator_id, &
        flaming_incipient_peak_random_generator_id, smoldering_incipient_delay_random_generator_id, &
        smoldering_incipient_peak_random_generator_id, fire_label_parameter_column_label
    character(len=128), dimension(mxrooms) :: fire_compartment_ids
    logical :: modify_fire_area_to_match_hrr, add_hrr_scale_to_parameters, &
        add_time_scale_to_parameters, add_fire_compartment_id_to_parameters
    character(len=128), dimension(mxpts) :: fire_hrr_generator_ids, fire_time_generator_ids, hrr_labels, time_labels
    logical, dimension(100) :: add_hrr_to_parameters, add_time_to_parameters 
    integer :: number_of_growth_points, number_of_decay_points
    real(eb) :: growth_exponent, decay_exponent 
    character(len=10) :: type_of_incipient_growth
    character(len=128), dimension(mxpntsarray) :: incipient_fire_types
    character(len=128) :: incipient_type_column_label, smoldering_time_column_label, smoldering_hrr_peak_column_label, &
        flaming_time_column_label, flaming_hrr_peak_column_label
    logical :: add_incipient_type_to_parameters, add_incipient_time_to_parameters, add_incipient_peak_hrr_to_parameters
    logical, dimension(mxpts) :: add_fire_to_parameters
    type(fire_generator_type), pointer :: fire

    namelist /MFIR/ id, fyi, fire_id, base_fire_id, scaling_fire_hrr_random_generator_id, &
        scaling_fire_time_random_generator_id, modify_fire_area_to_match_hrr, &
        add_hrr_scale_to_parameters, add_time_scale_to_parameters, hrr_scale_column_label, time_scale_column_label, &
        fire_compartment_random_generator_id, fire_compartment_ids, add_fire_compartment_id_to_parameters, &
        fire_compartment_id_column_label, flaming_smoldering_incipient_random_generator_id, &
        flaming_incipient_delay_random_generator_id, fire_label_parameter_column_label, &
        flaming_incipient_peak_random_generator_id, smoldering_incipient_delay_random_generator_id, &
        smoldering_incipient_peak_random_generator_id, &
        fire_hrr_generator_ids, fire_time_generator_ids, type_of_incipient_growth, &
        incipient_fire_types, incipient_type_column_label, smoldering_time_column_label, smoldering_hrr_peak_column_label, &
        flaming_time_column_label, flaming_hrr_peak_column_label, add_incipient_type_to_parameters, &
        add_incipient_peak_hrr_to_parameters, add_incipient_time_to_parameters, number_of_growth_points,&
        number_of_decay_points, growth_exponent, decay_exponent, add_hrr_to_parameters, add_time_to_parameters, &
        hrr_labels, time_labels, add_fire_to_parameters, generator_is_time_to_1054_kW
    
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
            write(errormessage,'(a,i0)') '***Error in &MFIR, Invalid variable in specification for inputs. &MFIR input ', n_rndfires
            call cfastexit('read_mfir',1)
        end if
    end do mfir_loop

    if (n_rndfires>mxrndfires) then
        write (errormessage,'(a,i0)') '***Error: Too many fire generators in input data file. Limit is ', mxfiregens
        call cfastexit('read_mfir',2)
    end if

     ! we found one. read it (only the first one counts; others are ignored)
    mfir_flag: if (mfirflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_mfir_loop: do ii=1,n_rndfires
            fire => randfireinfo(ii)

            call checkread('MFIR',lu,ios)
            call set_defaults
            read(lu,MFIR)
            
            fire%id = id
            fire%fyi = fyi
            fire%fireid = fire_id
            fire%basefireid = base_fire_id
            fire%modifyfirearea = modify_fire_area_to_match_hrr
            fire%incipient_growth = fire%incip_typ(fire%idx_none)
            fire%incipient_type = fire%incip_typ(fire%idx_none)
            found = .false.
            do jj = 1, n_fires
                if (trim(fireinfo(jj)%id) == trim(fire%fireid)) then
                    fire%fire => fireinfo(jj)
                    found = .true.
                elseif (trim(fireinfo(jj)%id) == trim(fire%basefireid)) then
                    fire%base => fireinfo(jj)
                end if
            end do
            if (.not.found) then
                write (errormessage,'(2a)') '***Error, Fire specification not found in input file. ', trim(fire%fireid)
                call cfastexit('read_mfir', 4)
            end if
            
            ! Scaling Fire HRR
            
            if (trim(scaling_fire_hrr_random_generator_id) /= 'NULL') then
                fire%fire%CData_modifying_fire_flag = .true.
                do jj = 1, n_generators
                    if (trim(scaling_fire_hrr_random_generator_id)== trim(generatorinfo(jj)%id)) then
                        fire%hrrscale => generatorinfo(jj)
                        fire%scalehrr = .true.
                    end if
                end do
                if (fire%scalehrr) then
                    fire%hrrscaleval%val => fire%hrrscalevalue
                else
                    write (errormessage,'(a)') &
                        '***Error, HRR scaling value found in &MFIR but scaling fire not specified in input file.'
                    call cfastexit('read_mfir', 5)
                end if 
                if (add_hrr_scale_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%hrrscaleval%add_to_parameters = add_hrr_scale_to_parameters
                    if (trim(hrr_scale_column_label) == 'NULL') then
                        fire%hrrscaleval%parameter_column_label = ' '
                        fire%hrrscaleval%parameter_column_label = trim(fire_id) // '_HRR_scaling_factor'
                    else 
                        fire%hrrscaleval%parameter_column_label =  hrr_scale_column_label
                    end if 
                end if
            end if
            
            ! Scaling Fire Time
            
            if (trim(scaling_fire_time_random_generator_id) /= 'NULL') then
                fire%fire%CData_modifying_fire_flag = .true.
                do jj = 1, n_generators
                    if (trim(scaling_fire_time_random_generator_id)== trim(generatorinfo(jj)%id)) then
                        fire%timescale => generatorinfo(jj)
                        fire%scaletime = .true.
                    end if
                end do
                if (fire%scaletime) then
                    fire%timescaleval%val => fire%timescalevalue
                else
                    write (errormessage,'(a)') &
                        '***Error, Time scaling value found in &MFIR but scaling fire not specified in input file.'
                    call cfastexit('READ_MFIR', 6)
                end if 
                if (add_time_scale_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%timescaleval%add_to_parameters = add_time_scale_to_parameters
                    if (trim(time_scale_column_label) == 'NULL') then
                        fire%timescaleval%parameter_column_label = ' '
                        fire%timescaleval%parameter_column_label = trim(fire_id) // '_time_scaling_factor'
                    else 
                        fire%timescaleval%parameter_column_label =  time_scale_column_label
                    end if
                end if
            end if
            
            ! Randomizing Fire Compartment 
            
            if (trim(fire_compartment_random_generator_id) /= 'NULL') then
                compgen_search: do jj = 1, n_generators
                    if (trim(fire_compartment_random_generator_id)== trim(generatorinfo(jj)%id)) then
                        fire%fire_comp%genptr => generatorinfo(jj)
                        fire%fire_label%genptr => generatorinfo(jj)
                        fire%do_fire_comp = .true.
                        exit compgen_search
                    end if
                end do compgen_search
                if (.not.fire%do_fire_comp) then
                    call cfastexit('READ_MFIR', 7)
                end if
                call find_object(fire_id, fire%fire_comp, found)
                if (found) then
                    call find_field('COMPARTMENT', fire%fire_comp%itemptr, fire%fire_comp, found)
                else
                    call cfastexit('READ_MFIR',8)
                end if
                if (.not.found) then
                    call cfastexit('READ_MFIR',9)
                end if
                fire%fire_comp%field_type = trim(fire%fire_comp%fld_types(fire%fire_comp%idx_index))
                fire%fire_comp%randptr%val => fire%fire_comp%rand_value
                fire%fire_comp%add_to_parameters = .false.
                fire%fire_label%field_type = trim(fire%fire_label%fld_types(fire%fire_label%idx_label))
                fire%fire_label%randptr%val => fire%fire_label%rand_value
                fire%fire_label%charval%val => fire%fire_label%labelval
                fire%fire_label%value_type = val_types(idx_char)
                fire%fire_label%valptr => fire%fire_label%charval
                complist_search: do jj = 1, mxrooms
                    if (trim(fire_compartment_ids(jj)) == 'NULL') then
                        exit complist_search
                    end if
                    found = .false.
                    room_search: do kk = 1, n_rooms
                        if (trim(fire_compartment_ids(jj)) == trim(roominfo(kk)%id)) then
                            fire%fire_comp%int_array(jj) = kk
                            fire%fire_comp%nlabel = jj
                            fire%fire_label%char_array(jj) = fire_compartment_ids(jj)
                            fire%fire_label%nlabel = jj
                            found = .true.
                            exit room_search
                        end if
                    end do room_search
                    if (.not.found) then
                        call cfastexit('READ_MFIR', 10)
                    end if
                end do complist_search
                fire%add_to_parameters = add_fire_compartment_id_to_parameters
                if (add_fire_compartment_id_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%fire_label%add_to_parameters = .true. 
                    fire%parameter_field_set = .true. 
                    if (trim(fire_label_parameter_column_label) == 'NULL') then
                        fire%fire_label%parameter_column_label = trim(fire_id) // '_FIRE_COMPARTMENT'
                    else
                        fire%fire_label%parameter_column_label = fire_label_parameter_column_label
                    end if
                end if
            end if
            
            ! Incipient Fire Model
            
            flameset = .false.
            smolderset = .false.
            
            ! Setting up the flaming growth model
            
            if (trim(flaming_incipient_delay_random_generator_id) /= 'NULL' .and. &
                trim(flaming_incipient_peak_random_generator_id) /= 'NULL') then
                fire%fire%CData_modifying_fire_flag = .true.
                found = .false.
                found2 = .false. 
                do jj = 1, n_generators
                    if (trim(flaming_incipient_delay_random_generator_id) == trim(generatorinfo(jj)%id)) then
                        fire%flame_hrr_ptr%genptr => generatorinfo(jj)
                        found = .true.
                    else if (trim(flaming_incipient_peak_random_generator_id) == trim(generatorinfo(jj)%id)) then
                        fire%flame_time_ptr%genptr => generatorinfo(jj)
                        found2 = .true.
                    end if        
                end do
                if (.not.(found.and.found2)) then
                    call cfastexit('MFIR', 11)
                end if
                call find_field('HRR_PT2   ', fire%fire, fire%flame_hrr_ptr, found)
                if (.not.found) then
                    call cfastexit('MFIR', 12)
                else
                    fire%flame_hrr_ptr%randptr%val => fire%flame_hrr_ptr%rand_value
                end if 
                fire%flame_hrr_ptr%field_type = trim(fire%flame_hrr_ptr%fld_types(fire%fire_comp%idx_value))
                call find_field('T_HRR_PT2   ', fire%fire, fire%flame_time_ptr, found)
                if (.not.found) then
                    call cfastexit('MFIR', 13)
                else
                    fire%flame_time_ptr%randptr%val => fire%flame_time_ptr%rand_value
                end if 
                fire%flame_time_ptr%field_type = trim(fire%flame_time_ptr%fld_types(fire%fire_comp%idx_value))
                flameset = .true. 
                fire%incipient_growth = fire%incip_typ(fire%idx_flame)
                fire%incipient_type = fire%incip_typ(fire%idx_flame)
                if (add_incipient_peak_hrr_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%flame_hrr_ptr%add_to_parameters = .true. 
                    if (trim(flaming_hrr_peak_column_label) == 'NULL') then
                        fire%flame_hrr_ptr%parameter_column_label = ' '
                        fire%flame_hrr_ptr%parameter_column_label = trim(fire_id) // '_flaming_peak_hrr_ignition'
                    else 
                        fire%flame_hrr_ptr%parameter_column_label =  flaming_hrr_peak_column_label
                    end if
                end if
                if (add_incipient_time_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%flame_time_ptr%add_to_parameters = .true. 
                    if (trim(flaming_time_column_label) == 'NULL') then
                        fire%flame_time_ptr%parameter_column_label = ' '
                        fire%flame_time_ptr%parameter_column_label = trim(fire_id) // '_flaming_time_ignition'
                    else 
                        fire%flame_time_ptr%parameter_column_label =  flaming_time_column_label
                    end if
                end if
            end if
            
            ! Setting up the smoldering growth model    
            
            if (trim(smoldering_incipient_delay_random_generator_id) /= 'NULL' .and. &
                trim(smoldering_incipient_peak_random_generator_id) /= 'NULL') then
                fire%fire%CData_modifying_fire_flag = .true.
                found = .false.
                found2 = .false. 
                do jj = 1, n_generators
                    if (trim(smoldering_incipient_delay_random_generator_id) == trim(generatorinfo(jj)%id)) then
                        fire%smolder_hrr_ptr%genptr => generatorinfo(jj)
                        found = .true.
                    else if (trim(smoldering_incipient_peak_random_generator_id) == trim(generatorinfo(jj)%id)) then
                        fire%smolder_time_ptr%genptr => generatorinfo(jj)
                        found2 = .true.
                    end if        
                end do
                if (.not.(found.and.found2)) then
                    call cfastexit('MFIR', 15)
                end if
                call find_field('HRR_PT2  ', fire%fire, fire%smolder_hrr_ptr, found)
                if (.not.found) then
                    call cfastexit('MFIR', 16)
                else
                    fire%smolder_hrr_ptr%randptr%val => fire%smolder_hrr_ptr%rand_value
                end if 
                fire%smolder_hrr_ptr%field_type = trim(fire%smolder_hrr_ptr%fld_types(fire%fire_comp%idx_value))
                call find_field('T_HRR_PT2  ', fire%fire, fire%smolder_time_ptr, found)
                if (.not.found) then
                    call cfastexit('MFIR', 17)
                else
                    fire%smolder_time_ptr%randptr%val => fire%smolder_time_ptr%rand_value
                end if 
                fire%smolder_time_ptr%field_type = trim(fire%smolder_time_ptr%fld_types(fire%fire_comp%idx_value))
                smolderset = .true.
                if (flameset) then
                    fire%incipient_growth = fire%incip_typ(fire%idx_random)
                    fire%incipient_type = fire%incip_typ(fire%idx_random)
                else
                    fire%incipient_growth = fire%incip_typ(fire%idx_smolder)
                    fire%incipient_type = fire%incip_typ(fire%idx_smolder)
                end if
                if (add_incipient_peak_hrr_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%smolder_hrr_ptr%add_to_parameters = .true. 
                    if (trim(smoldering_hrr_peak_column_label) == 'NULL') then
                        fire%smolder_hrr_ptr%parameter_column_label = ' '
                        fire%smolder_hrr_ptr%parameter_column_label = trim(fire_id)//'_smolderinging_peak_hrr_ignition'
                    else 
                        fire%smolder_hrr_ptr%parameter_column_label =  smoldering_hrr_peak_column_label
                    end if
                end if
                if (add_incipient_time_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%smolder_time_ptr%add_to_parameters = .true. 
                    if (trim(smoldering_time_column_label) == 'NULL') then
                        fire%smolder_time_ptr%parameter_column_label = ' '
                        fire%smolder_time_ptr%parameter_column_label = trim(fire_id) // '_smoldering_time_ignition'
                    else 
                        fire%smolder_time_ptr%parameter_column_label =  smoldering_time_column_label
                    end if
                end if
            end if
                
            ! Setting up the generator that switches between the two models 
                
            if (flameset .and. smolderset .and. & 
                trim(flaming_smoldering_incipient_random_generator_id) /= 'NULL' ) then
                fire%fire%CData_modifying_fire_flag = .true.
                found = .false.
                do jj = 1, n_generators
                    if (trim(flaming_smoldering_incipient_random_generator_id) == trim(generatorinfo(jj)%id)) then
                        fire%fs_fire_ptr%genptr => generatorinfo(jj)
                        found = .true.
                    end if
                end do
                if (.not.found) then
                    call cfastexit('MFIR', 19)
                end if
                fire%incipient_type = fire%incip_typ(fire%idx_random)
                fire%incipient_growth = fire%incip_typ(fire%idx_random)
                fire%fs_fire_ptr%field_type = trim(fire%fs_fire_ptr%fld_types(fire%fs_fire_ptr%idx_index))
                fire%fs_fire_ptr%value_type = val_types(idx_char)
                fire%fs_fire_ptr%charval%val => fire%incipient_growth
                fire%fs_fire_ptr%valptr => fire%fs_fire_ptr%charval
                fire%fs_fire_ptr%randptr%val => fire%fs_fire_ptr%rand_value
                fs_do:do jj = 1, mxpntsarray
                    if (trim(incipient_fire_types(jj)) == 'FLAMING' .or. & 
                        trim(incipient_fire_types(jj)) == 'SMOLDERING') then
                        fire%fs_fire_ptr%char_array(jj) = incipient_fire_types(jj)
                    else
                        fire%fs_fire_ptr%nidx = jj - 1
                        exit fs_do
                    end if
                end do fs_do
                if (add_incipient_type_to_parameters) then
                    fire%add_to_parameters = .true. 
                    fire%fs_fire_ptr%add_to_parameters = .true. 
                    if (trim(incipient_type_column_label) == 'NULL') then
                        fire%fs_fire_ptr%parameter_column_label = ' '
                        fire%fs_fire_ptr%parameter_column_label = trim(fire_id) // '_INCIPIENT_TYPE'
                    else 
                        fire%fs_fire_ptr%parameter_column_label =  incipient_type_column_label
                    end if
                end if
            else if (flameset .and. smolderset) then
                call cfastexit('READ_MFIR', 21)
            end if   
            
            ! Setting up the fire where all points are determined by generators
            
            If (trim(fire_hrr_generator_ids(1)) /= 'NULL' .and. trim(fire_time_generator_ids(1)) /= 'NULL') then
                fire%fire%CData_modifying_fire_flag = .true.
                fire%generate_fire =   .true. 
                fire%add_to_parameters = .true.
                fire%fire_generator_is_time_to_1054_kW = generator_is_time_to_1054_kW
                if (flameset .or. smolderset) then
                    idx_firepts = 2
                else 
                    idx_firepts = 1
                end if 
                if (number_of_growth_points > 0) then
                    idx_firepts = idx_firepts + number_of_growth_points
                end if 
                outfireloop: do jj = 1, 100
                    if (trim(fire_hrr_generator_ids(jj)) /= 'NULL' .and. trim(fire_time_generator_ids(jj)) /= 'NULL') then
                        fire%n_firegenerators = jj 
                    else if (trim(fire_hrr_generator_ids(jj)) == 'NULL' .and. trim(fire_time_generator_ids(jj)) == 'NULL') then
                        exit outfireloop
                    end if 
                    found = .false.
                    found2 = .false.
                    infireloop: do kk = 1, n_generators
                        if (trim(fire_hrr_generator_ids(jj)) == trim(generatorinfo(kk)%id)) then
                            found = .true.
                            fire%firegenerators(1, idx_firepts + jj)%genptr => generatorinfo(kk)
                            fire%firegenerators(1,idx_firepts + jj)%field_type =  &
                                trim(fire%firegenerators(1, 1)%fld_types(fire%firegenerators(1, 1)%idx_value))
                            fire%firegenerators(1, idx_firepts + jj)%kilo_flag = .true. 
                        else if (trim(fire_time_generator_ids(jj)) == trim(generatorinfo(kk)%id)) then
                            found2 = .true.
                            fire%firegenerators(2, idx_firepts + jj)%genptr => generatorinfo(kk)
                            fire%firegenerators(2, idx_firepts + jj)%field_type =  &
                                trim(fire%firegenerators(1, 1)%fld_types(fire%firegenerators(1, 1)%idx_value))
                        end if 
                        if (found .and. found2) then 
                            exit infireloop
                        end if
                    end do infireloop 
                    if (.not. found .or. .not. found2) then
                        call cfastexit('READ_MFIR', 22)
                    end if
                end do outfireloop
                fire%last_growth_pt = idx_firepts
                idx_firepts = idx_firepts + fire%n_firegenerators
                fire%growth_npts = number_of_growth_points
                fire%decay_npts = number_of_decay_points
                fire%n_firepoints = idx_firepts + number_of_decay_points
                fire%first_decay_pt = idx_firepts
                if (fire%decay_npts > 0) then
                    do kk = 1, n_generators
                        if (trim(fire%firegenerators(1,idx_firepts)%genptr%id) == &
                            trim(generatorinfo(kk)%id)) then
                            fire%firegenerators(1, fire%n_firepoints)%genptr => generatorinfo(kk)
                            fire%firegenerators(1, fire%n_firepoints)%field_type =  &
                                trim(fire%firegenerators(1, 1)%fld_types(fire%firegenerators(1, 1)%idx_value))
                            fire%firegenerators(1, fire%n_firepoints)%kilo_flag = .true. 
                        else if (trim(fire%firegenerators(2,idx_firepts)%genptr%id) == &
                            trim(generatorinfo(kk)%id)) then
                            fire%firegenerators(2, fire%n_firepoints)%genptr => generatorinfo(kk)
                            fire%firegenerators(2, fire%n_firepoints)%field_type =  &
                                trim(fire%firegenerators(1, 1)%fld_types(fire%firegenerators(1, 1)%idx_value))
                        end if 
                    end do
                end if
                do jj = 1, fire%n_firepoints
                    if (add_fire_to_parameters(jj)) fire%add_to_parameters = .true.
                    call connect_to_fire(jj, fire_id, fire%fire, fire%firegenerators(1,jj), &
                        fire%firegenerators(2,jj), hrr_labels(jj), time_labels(jj), add_fire_to_parameters(jj))
                end do
                fire%growthexpo = growth_exponent
                fire%decayexpo = decay_exponent
            end if
            
        end do read_mfir_loop
    end if mfir_flag
    
    contains
    
    subroutine connect_to_fire(idx, fire_id, fire, hrrfield, timefield, hrr_label, time_label, add_to_parameters)
    
        type(fire_type), pointer :: fire
        type(field_pointer) :: hrrfield, timefield
        character(len=128) :: hrr_label, time_label, fire_id
        logical :: add_to_parameters
        integer :: idx
        
        character(len=128) :: tmpbuf
    
        tmpbuf = ' '
        write(tmpbuf,'(''T_HRR_PT'', I3)') idx
        call find_field(tmpbuf, fire, timefield, found)
        if (.not. found) then
            write(*,*) 'READ_MFIR: Error on Time firegenerator '
            call cfastexit('READMFIR:CONNECT_TO_FIRE', 1)
        end if
        if (add_to_parameters) then
            timefield%add_to_parameters = .true.
            if (trim(time_label) == 'NULL') then
                timefield%parameter_column_label = ' '
                timefield%parameter_column_label = trim(fire_id) // '_' // trim(tmpbuf)
            else
                timefield%parameter_column_label = time_label
            end if
        end if
        tmpbuf = ' '
        write(tmpbuf,'(''HRR_PT'', I3)') idx
        call find_field(tmpbuf, fire, hrrfield, found)
        if (.not. found) then
            write(*,*) 'READ_MFIR: Error on HRR firegenerator '
            call cfastexit('READMFIR:CONNECT_TO_FIRE', 2)
        end if
        if (add_to_parameters) then
            hrrfield%add_to_parameters = .true.
            if (trim(hrr_label) == 'NULL') then
                hrrfield%parameter_column_label = ' '
                hrrfield%parameter_column_label = trim(fire_id) // '_' // trim(tmpbuf)
            else
                hrrfield%parameter_column_label = time_label
            end if
        end if
    end subroutine connect_to_fire
    
    subroutine set_defaults
    
    id = 'NULL'
    fire_id = 'NULL' 
    base_fire_id = 'NULL'
    scaling_fire_hrr_random_generator_id = 'NULL'
    scaling_fire_time_random_generator_id = 'NULL'
    fire_label_parameter_column_label = 'NULL'
    modify_fire_area_to_match_hrr = .true. 
    !add_to_parameters = .false. 
    hrr_scale_column_label = 'NULL'
    time_scale_column_label = 'NULL'
    add_hrr_scale_to_parameters = .true.
    add_time_scale_to_parameters = .true.
    fire_compartment_random_generator_id = 'NULL'
    add_fire_compartment_id_to_parameters = .true.
    fire_compartment_id_column_label = 'NULL'
    fire_compartment_ids = 'NULL'
    flaming_smoldering_incipient_random_generator_id = 'NULL'
    flaming_incipient_delay_random_generator_id = 'NULL'
    flaming_incipient_peak_random_generator_id = 'NULL'
    smoldering_incipient_delay_random_generator_id = 'NULL'
    smoldering_incipient_peak_random_generator_id = 'NULL'
    type_of_incipient_growth = 'NONE'
    incipient_fire_types = 'NULL'
    incipient_type_column_label = 'NULL'
    smoldering_time_column_label = 'NULL'
    smoldering_hrr_peak_column_label = 'NULL'
    flaming_time_column_label = 'NULL'
    flaming_hrr_peak_column_label = 'NULL'
    add_incipient_type_to_parameters = .true.
    add_incipient_time_to_parameters = .true.
    add_incipient_peak_hrr_to_parameters = .true.
    fire_hrr_generator_ids = 'NULL'
    fire_time_generator_ids = 'NULL'
    number_of_growth_points = 0
    number_of_decay_points = 0
    growth_exponent = 1
    decay_exponent = 1
    add_hrr_to_parameters = .true. 
    add_time_to_parameters = .true. 
    hrr_labels = 'NULL'
    time_labels = 'NULL'
    add_fire_to_parameters = .true.
    generator_is_time_to_1054_kW = .false.
    
    end subroutine set_defaults
    
    end subroutine read_mfir
    !
    !------------ read_mdia
    !
    subroutine read_mdia(lu)
    
    integer, intent(in) :: lu
    integer :: ios, input_file_line_number, ii, i
    
    logical :: column_output, mdiaflag
    real(eb) :: criterion, cutoffs(2)
    character(128) :: id 
    character(len=256) :: type, test, zero_exceptions(2), first_field(3), &
        second_field(3), test_column, column_headers(5)

    namelist /MDIA/ id, type, first_field, second_field, zero_exceptions, &
            test, criterion, column_output, cutoffs, test_column, &
            column_headers

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0
    mdiaflag = .false.
    n_diag = 0

    ! scan entire file to look for &HEAD input
    mdia_loop: do
        call checkread ('MDIA', lu, ios)
        if (ios==0) mdiaflag=.true.
        if (ios==1) then
            exit mdia_loop
        end if
        read(lu,MDIA,iostat=ios)
        n_diag = n_diag + 1
        if (ios>0) then
            write(errormessage,'(a,i3)') '***Error in &MDIA: Invalid variable in specification for inputs.',n_diag
            call cfastexit('read_mdia',1)
        end if
    end do mdia_loop

    ! we found one. read it (only the first one counts; others are ignored)
    mdia_flag: if (mdiaflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_mdia_loop: do ii=1,n_diag

            diagptr => diaginfo(ii)

            call checkread('MDIA',lu,ios)
            call set_defaults
            read(lu,MDIA)
            
            if (trim(id) == 'NULL') then
                call cfastexit('read_mdia',1)
            end if 
            diagptr%id = trim(id)
            if (trim(type) == 'NULL') then
                call cfastexit('read_mdia',2)
            end if 
            diagptr%diagnostic = trim(type)
            if (trim(test) == 'NULL') then
                call cfastexit('read_mdia',3)
            end if 
            diagptr%test = trim(test)
            if (trim(zero_exceptions(1)) == 'NULL') then
                call cfastexit('read_mdia',4)
            end if 
            diagptr%zero_except(1:2) = zero_exceptions(1:2)
            if (trim(test_column) == 'NULL') then
                call cfastexit('read_mdia',5)
            end if 
            diagptr%test_column = trim(test_column)
            if (criterion == -1001.0_eb) then
                call cfastexit('read_mdia',6)
            end if 
            diagptr%criterion = criterion
            if (cutoffs(1) == -1001.0_eb.or.cutoffs(2)==-1001.0_eb) then
                call cfastexit('read_mdia',7)
            end if 
            diagptr%cutoffs(1:2) = cutoffs(1:2)
            if (trim(first_field(1)) == 'NULL') then
                call cfastexit('read_mdia',8)
            elseif (trim(first_field(2)) == 'NULL') then
                call cfastexit('read_mdia',9)
            elseif (trim(first_field(3)) == 'NULL') then
                call cfastexit('read_mdia',10)
            end if 
            diagptr%fst_fld(1:3) = first_field(1:3)
            if (trim(second_field(1)) == 'NULL') then
                call cfastexit('read_mdia',11)
            elseif (trim(second_field(2)) == 'NULL') then
                call cfastexit('read_mdia',12)
            elseif (trim(second_field(3)) == 'NULL') then
                call cfastexit('read_mdia',13)
            end if 
            diagptr%sec_fld(1:3) = second_field(1:3)
            diagptr%column_skip = 0
            col_loop:do i = 1, diagptr%mx_hdrs
                if (trim(column_headers(i)) == 'NULL') then
                    exit col_loop
                else
                    diagptr%col_hdrs(i) = trim(column_headers(i))
                    diagptr%column_skip = diagptr%column_skip + 1
                end if
            end do col_loop
            if (diagptr%column_skip < 3) then
                call cfastexit('read_mdia',14)
            end if
        end do read_mdia_loop
    end if mdia_flag
    

    contains
    
    subroutine set_defaults
    
    id = 'NULL'
    type = 'NULL'
    test = 'NULL'
    zero_exceptions(1:2) = 'NULL'
    first_field(1:3) = 'NULL'
    second_field(1:3) = 'NULL'
    test_column = 'NULL'
    criterion = -1001.0_eb
    cutoffs(1:2) = -1001.0_eb
    column_output = .false.
    column_headers(1:5) = 'NULL'
    
    end subroutine set_defaults
    
    end subroutine read_mdia
    
    !-------------------------------find_object--------------------------------------
    
    subroutine find_object(id, field, flag)
    character(len=*), intent(in) :: id
    type(field_pointer), intent(inout) :: field
    logical, intent(out) :: flag
    
    integer :: i
    
    flag = .false.
    if (trim(id)=='INIT') then
        flag = .true.
        field%itemptr => dummy
        return
    end if 
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
    class(cfast_type), target, intent(inout) :: item
    type(field_pointer), target, intent(inout) :: fldptr
    logical, intent(out) :: found
    
    integer :: i
    
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
        else if (fieldid(1:18) == 'CEILING_THICKNESS_') then
            found = .true.
            read(fieldid(19:19),'(i1)') i
            fldptr%realval%val => item%thick_w(i,1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (fieldid(1:16) == 'FLOOR_THICKNESS_') then
            found = .true.
            read(fieldid(17:17),'(i1)') i
            fldptr%realval%val => item%thick_w(i,2)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (fieldid(1:15) == 'WALL_THICKNESS_') then
            found = .true.
            read(fieldid(16:16),'(i1)') i
            fldptr%realval%val => item%thick_w(i,3)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (fieldid(1:16) == 'CEILING_MATL_ID_') then
            found = .true.
            read(fieldid(17:17),'(i1)') i
            fldptr%charval%val => item%matl(i,1)
            fldptr%value_type = val_types(idx_char)
            fldptr%valptr => fldptr%charval
        else if (fieldid(1:14) == 'FLOOR_MATL_ID_') then
            found = .true.
            read(fieldid(15:15),'(i1)') i
            fldptr%charval%val => item%matl(i,2)
            fldptr%value_type = val_types(idx_char)
            fldptr%valptr => fldptr%charval
        else if (fieldid(1:13) == 'WALL_MATL_ID_') then
            found = .true.
            read(fieldid(14:14),'(i1)') i
            fldptr%charval%val => item%matl(i,3)
            fldptr%value_type = val_types(idx_char)
            fldptr%valptr => fldptr%charval
        else if (trim(fieldid) == 'WALL_LEAK_AREA_RATIO') then
            found = .true.
            fldptr%realval%val => item%leak_area_ratios(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'FLOOR_LEAK_AREA_RATIO') then
            found = .true.
            fldptr%realval%val => item%leak_area_ratios(2)
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
        if (item%vtype == 'H') then
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
            elseif (fieldid(1:2) == 'T_') then
                found = .true.
                read(fieldid(3:5),'(i3)') i
                item%npoints = max(item%npoints,i)
                fldptr%realval%val => item%t(i)
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            elseif (fieldid(1:2) == 'F_') then
                found = .true.
                read(fieldid(3:5),'(i3)') i
                fldptr%realval%val => item%f(i)
                item%npoints = max(item%npoints,i)
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            elseif (trim(fieldid) == 'SETPOINT') then
                found = .true.
                fldptr%realval%val => item%opening_criterion
                fldptr%value_type = val_types(idx_real)
                if (item%opening_type == 2) then
                    fldptr%temp_flag = .true.
                else if (item%opening_type == 3) then
                    fldptr%kilo_flag = .true.
                else
                    call cfastexit('FIND_FIELD', 3)
                end if
                fldptr%valptr => fldptr%realval
            elseif (trim(fieldid) == 'PRE_ACTIVATION_FRACTION') then
                found = .true.
                fldptr%realval%val => item%f(1)
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            elseif (trim(fieldid) == 'POST_ACTIVATION_FRACTION') then
                found = .true.
                fldptr%realval%val => item%f(2)
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            else
                call cfastexit('find_field', 4)
            end if 
        elseif (trim(fieldid) == 'AREA') then
            found = .true.
            fldptr%realval%val => item%area
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (item%maxflow > 0.0_eb) then
            if (trim(fieldid) == 'FLOW') then 
                found = .true.
                fldptr%realval%val => item%maxflow
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            end if
        else
            call cfastexit('find_field', 5)
        end if
    class is (detector_type)
        if (trim(fieldid) == 'TRIGGER') then
            found = .true.
            fldptr%realval%val => item%trigger
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid)  == 'TRIGGER_SMOLDER') then
            found = .true.
            fldptr%realval%val => item%trigger_smolder
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid)  == 'COMPARTMENT') then
            found = .true.
            fldptr%charval%val => item%room_id
            fldptr%value_type = val_types(idx_char)
            fldptr%valptr => fldptr%charval
        else if (trim(fieldid) == 'X_LOCATION') then
            found = .true.
            fldptr%realval%val => item%center(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Y_LOCATION') then
            found = .true.
            fldptr%realval%val => item%center(2)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Z_LOCATION') then
            found = .true.
            fldptr%realval%val => item%center(3)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'X_POSITION') then
            found = .true.
            fldptr%realval%val => item%center(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Y_POSITION') then
            found = .true.
            fldptr%realval%val => item%center(2)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Z_POSITION') then
            found = .true.
            fldptr%realval%val => item%center(3)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else
            call cfastexit('find_field',6)
        end if
    class is (target_type)
        if (trim(fieldid)  == 'COMPARTMENT') then
            found = .true.
            fldptr%charval%val => item%room_id
            fldptr%value_type = val_types(idx_char)
            fldptr%valptr => fldptr%charval
        else if (trim(fieldid) == 'X_LOCATION') then
            found = .true.
            fldptr%realval%val => item%center(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Y_LOCATION') then
            found = .true.
            fldptr%realval%val => item%center(2)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Z_LOCATION') then
            found = .true.
            fldptr%realval%val => item%center(3)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'X_POSITION') then
            found = .true.
            fldptr%realval%val => item%center(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Y_POSITION') then
            found = .true.
            fldptr%realval%val => item%center(2)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Z_POSITION') then
            found = .true.
            fldptr%realval%val => item%center(3)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'X_NORMAL') then
            found = .true.
            fldptr%realval%val => item%normal(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Y_NORMAL') then
            found = .true.
            fldptr%realval%val => item%normal(2)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Z_NORMAL') then
            found = .true.
            fldptr%realval%val => item%normal(3)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else
            call cfastexit('find_field', 7)
        end if
    class is (fire_type)
        if (trim(fieldid) == 'COMPARTMENT') then
            found = .true.
            fldptr%intval%val => item%room
            fldptr%value_type = val_types(idx_int)
            fldptr%valptr => fldptr%intval
        else if (trim(fieldid) == 'X_LOCATION') then
            found = .true.
            fldptr%realval%val => item%x_position
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Y_LOCATION') then
            found = .true.
            fldptr%realval%val => item%y_position
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'X_POSITION') then
            found = .true.
            fldptr%realval%val => item%x_position
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'Y_POSITION') then
            found = .true.
            fldptr%realval%val => item%y_position
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'FLAMING_TRANSITION_TIME') then
            found = .true. 
            fldptr%realval%val => item%flaming_transition_time
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid(1:6)) == 'HRR_PT') then 
            found = .true.
            read(fieldid(7:9),'(i3)') i
            fldptr%realval%val => item%qdot(i)
            fldptr%kilo_flag = .true.
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid(1:8)) == 'T_HRR_PT') then 
            found = .true.
            read(fieldid(9:11),'(i3)') i
            fldptr%realval%val => item%t_qdot(i)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid(1:7)) == 'FIRE_ID') then 
            found = .true. 
            fldptr%charval%val => item%fire_id
            fldptr%value_type = val_types(idx_char)
            fldptr%valptr => fldptr%charval
        else if (trim(fieldid(1:8)) == 'SETPOINT') then
            if (item%ignition_type == 2) then
                found = .true. 
                fldptr%realval%val => item%ignition_criterion
                fldptr%temp_flag = .true.
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            else if (item%ignition_type == 3) then
                found = .true. 
                fldptr%realval%val => item%ignition_criterion
                fldptr%value_type = val_types(idx_real)
                fldptr%valptr => fldptr%realval
            else
                call cfastexit('find_field', 8)
            end if
        else if (trim(fieldid(1:8)) == 'CO_YIELD') then
            found = .true. 
            item%n_co = 1
            item%t_co(1) = 0.0_eb
            fldptr%realval%val => item%y_co(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid(1:8)) == 'HCN_YIELD') then
            found = .true. 
            item%n_hcn = 1
            item%t_hcn(1) = 0.0_eb
            fldptr%realval%val => item%y_hcn(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid(1:8)) == 'SOOT_YIELD') then
            found = .true. 
            item%n_soot = 1
            item%t_soot(1) = 0.0_eb
            fldptr%realval%val => item%y_soot(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid(1:8)) == 'TRACE_YIELD') then
            found = .true. 
            item%n_trace = 1
            item%t_trace(1) = 0.0_eb
            fldptr%realval%val => item%y_trace(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'CARBON') then
            found = .true. 
            fldptr%realval%val => item%n_c
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'HYDROGEN') then
            found = .true. 
            fldptr%realval%val => item%n_h
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'OXYGEN') then
            found = .true. 
            fldptr%realval%val => item%n_o
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'NITROGEN') then
            found = .true. 
            fldptr%realval%val => item%n_n
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'CHLORINE') then
            found = .true. 
            fldptr%realval%val => item%n_cl
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else if (trim(fieldid) == 'HEAT_OF_COMBUSTION') then
            found = .true. 
            item%n_hoc = 1
            fldptr%realval%val => item%hoc(1)
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else
            call cfastexit('find_field', 9)
        end if 
    class default
        if (trim(fieldid) == 'PRESSURE') then
            fldptr%realval%val => exterior_abs_pressure
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
            fldptr%press_flag = .true.
        elseif (trim(fieldid) == 'EXTERIOR_AMBIENT_TEMPERATURE') then
            fldptr%realval%val =>  exterior_ambient_temperature
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
            fldptr%temp_flag = .true.
        elseif (trim(fieldid) == 'INTERIOR_AMBIENT_TEMPERATURE') then
            fldptr%realval%val =>  interior_ambient_temperature
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
            fldptr%temp_flag = .true.
        elseif (trim(fieldid) == 'RELATIVE_HUMIDITY') then
            fldptr%realval%val =>  relative_humidity
            fldptr%value_type = val_types(idx_real)
            fldptr%valptr => fldptr%realval
        else
            call cfastexit('find_field',99)
        end if
    end select
    
    return
    end subroutine find_field
    
end module namelist_input_pp_routines