module preprocessor_routines
    
    !-------------CFAST data---------------------------------------------
    use precision_parameters
    use cparams

    use dump_data, only: n_dumps
    use namelist_data, only: convert_negative_distances
    use option_data, only: total_steps
    use setup_data, only: cfast_version, stime, iofill, i_time_step, time_end, deltat, i_time_end, validation_flag, &
        ss_out_interval, inputfile, datapath, project, extension, iofili, initializeonly, debugging, outputfile, &
        smvhead, smvdata, smvcsv, smvsinfo, sscompartment, ssdevice, sswall, ssmasses, ssvent, &
        queryfile, statusfile
    use diag_data, only: residcsv, slabcsv
    
    !-----------------------CFAST routines-----------------------------------------
    use exit_routines, only: cfastexit, delete_output_files, closeoutputfiles
    use initialization_routines, only : initialize_memory
    use input_routines, only : open_files, read_input_file
    use namelist_input_routines, only: cdata_preprocessor_rereadinputfile
    use utility_routines, only : read_command_options, d1mach
    
    !------------------------CData data-----------------------------------------------
    use pp_params, only: mxgenerators, mxpntsarray, mxseeds, mxfields, rnd_seeds, restart_values, mxrandfires, &
        mxiterations
    use montecarlo_data, only: generatorinfo, n_generators, fieldinfo, n_fields, mc_write_seeds, mc_number_of_cases, &
        n_rndfires, randfireinfo, mc_max_iterations, workpath, parameterfile, fieldptr, validation_output
    use preprocessor_types, only: random_generator_type
    
    !------------------------CData routines-------------------------------------
    use montecarlo_routines, only: create_mc_filename, process_mc_filename_pattern
    use namelist_input_pp_routines, only: namelist_pp_input
    use preprocessor_output_routines, only: flush_parameters_buffer, setup_col_parameters_output, &
        open_preprocessor_outputfiles, initialize_preprocessor_output_routines, &
        add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer, finish_batch
    use write_inputfile_routines, only: write_cfast_infile
    
    implicit none
    
    private

    public preprocessor_initialize, create_case, initialize_output_files, write_seeds_outputfile, &
        preprocessor 

    contains
    
    !-----------------------------preprocessor----------------------------------------------
    
    subroutine preprocessor

    integer :: i
    character(len=256) :: infilecase
    
     ! initialize the basic memory configuration

    call initialize_memory
    call read_command_options
    call open_files

    call read_input_file
    
    call preprocessor_initialize
    call cdata_preprocessor_rereadinputfile
    call namelist_pp_input
    call process_mc_filename_pattern
    call initialize_output_files
    do i = 1, mc_number_of_cases
        call create_mc_filename(i, infilecase)
        if (debugging) write(*,*)'creating ',trim(infilecase)
        call create_case(infilecase, i)
        if (.not.initializeonly) call write_cfast_infile(infilecase)
        call flush_parameters_buffer
    end do
    call finish_batch
    call write_seeds_outputfile
    if (initializeonly) then
        write (*,'(a,i0,a)') 'Created parameters files for ', mc_number_of_cases, ' CFAST cases'
    else
        write (*,'(a,i0,a)') 'Created ', mc_number_of_cases, ' CFAST input files'
    end if
    
    return
    
    end subroutine preprocessor 
    
    ! --------------------------- preprocessor_initialize ----------------------------------
    
    subroutine preprocessor_initialize
    
    integer :: i, j, k
    integer, parameter :: i1 = 5554803, i2 = 7033951, c = 4*214783398 + 1
    integer :: values(8)
    character(len=10) :: date, time, zone
    
    call DATE_AND_TIME(date, time, zone, values)
    restart_values(9) = c
    restart_values(1) = values(4)
    restart_values(2) = values(1)*10000 + values(2)*100 + values(3)
    restart_values(3) = values(7)
    restart_values(4) = values(8)*1000 + values(6)*100 + values(5)
    rnd_seeds(1) = mod((i1+restart_values(1))*restart_values(2),restart_values(9))
    rnd_seeds(2) = mod((i2+restart_values(3))*restart_values(4),restart_values(9))
    call DATE_AND_TIME(date, time, zone, values)
    restart_values(5) = values(4)
    restart_values(6) = values(1)*10000 + values(3)*100 + values(2)
    restart_values(7) = values(7)
    restart_values(8) = values(5)*10000 + values(6)*100 + values(8)
    rnd_seeds(2) = mod((rnd_seeds(2)+restart_values(5))*restart_values(6),restart_values(9))
    rnd_seeds(1) = mod((rnd_seeds(1)+restart_values(7))*restart_values(8),restart_values(9))
    call RANDOM_SEED(PUT=rnd_seeds)
    mc_write_seeds = .false.
    
    mc_max_iterations = mxiterations
    validation_output = validation_flag
    
    n_generators = 0
    allocate(generatorinfo(mxgenerators))
    generatorinfo(1:mxgenerators)%id = 'NULL'
    generatorinfo(1:mxgenerators)%fyi = 'NULL' 
    generatorinfo(1:mxgenerators)%type_dist = 'NONE'
    generatorinfo(1:mxgenerators)%value_type = 'NONE'
    do i = 1, mxgenerators
        generatorinfo(i)%char_array(1:mxpntsarray) = 'NULL'
        generatorinfo(i)%real_array(1:mxpntsarray) = -1001._eb 
        generatorinfo(i)%int_array(1:mxpntsarray) = -1001  
        generatorinfo(i)%logic_array(1:mxpntsarray) = .false.
        generatorinfo(i)%prob_array(1:mxpntsarray) = -1001._eb 
        generatorinfo(i)%seeds(1:mxseeds) = -1001
        call generatorinfo(i)%set_max_value(d1mach(2))
        call generatorinfo(i)%set_min_value(-d1mach(2))
        call generatorinfo(i)%set_add_value(0.0_eb)
        generatorinfo(i)%max_offset = 0.0_eb
        generatorinfo(i)%min_offset = 0.0_eb
    end do
    generatorinfo(1:mxgenerators)%mean = 0._eb
    generatorinfo(1:mxgenerators)%stdev = 0._eb
    generatorinfo(1:mxgenerators)%alpha = 0._eb
    generatorinfo(1:mxgenerators)%beta = 0._eb
    generatorinfo(1:mxgenerators)%peak = 0._eb
    generatorinfo(1:mxgenerators)%first = .true.
    generatorinfo(1:mxgenerators)%use_seeds = .false.
    
    n_fields = 0
    allocate(fieldinfo(mxfields))
    fieldinfo(1:mxfields)%id = 'NULL'
    fieldinfo(1:mxfields)%fyi = 'NULL'
    fieldinfo(1:mxfields)%add_to_parameters = .false.
    fieldinfo(1:mxfields)%parameter_column_label = 'NULL'
    fieldinfo(1:mxfields)%position = 1
    fieldinfo(1:mxfields)%temp_flag = .false.
    fieldinfo(1:mxfields)%kilo_flag = .false. 
    fieldinfo(1:mxfields)%press_flag = .false. 
    do i = 1, mxfields
        fieldinfo(i)%real_array(1:mxpntsarray) = -1001.0_eb
        fieldinfo(i)%int_array(1:mxpntsarray) = -1001
        fieldinfo(i)%logic_array(1:mxpntsarray) = .false.
        fieldinfo(i)%char_array(1:mxpntsarray) = 'NULL'
        fieldinfo(i)%randptr%val => fieldinfo(i)%rand_value
    end do
    allocate(fieldptr(mxfields))
    fieldptr(1:mxfields) = -1001
    
    
    n_rndfires = 0
    allocate(randfireinfo(mxrandfires))
    randfireinfo(1:mxrandfires)%id = 'NULL'
    randfireinfo(1:mxrandfires)%fireid = 'NULL'
    randfireinfo(1:mxrandfires)%basefireid = 'NULL'
    randfireinfo(1:mxrandfires)%hrrscalegenid = 'NULL'
    randfireinfo(1:mxrandfires)%timescalegenid = 'NULL'
    randfireinfo(1:mxrandfires)%smoldergenid = 'NULL'
    randfireinfo(1:mxrandfires)%smoldertimegenid = 'NULL'
    do i = 1, mxrandfires
        randfireinfo(i)%compname(1:mxrooms) = 'NULL'
        randfireinfo(i)%compindex(1:mxrooms) = -1001
    end do
    randfireinfo(1:mxrandfires)%generate_fire = .false.
    randfireinfo(1:mxrandfires)%first_time_point_smoldering = .false.
    randfireinfo(1:mxrandfires)%modifyfirearea = .false.
    randfireinfo(1:mxrandfires)%scalehrr = .false.
    randfireinfo(1:mxrandfires)%scaletime = .false.
    randfireinfo(1:mxrandfires)%dostime = .false. 
    randfireinfo(1:mxrandfires)%smoldering_fire = .false. 
    randfireinfo(1:mxrandfires)%hrrscalevalue = -1001.0_eb
    randfireinfo(1:mxrandfires)%timescalevalue = -1001.0_eb
    randfireinfo(1:mxrandfires)%stimevalue = -1001.0_eb
    do i = 1, mxrandfires
        do j = 1, 2
            do k = 1, mxpts
                randfireinfo(i)%firegenerators(j, k)%id = 'NULL'
                randfireinfo(i)%firegenerators(j, k)%fyi = 'NULL'
                randfireinfo(i)%firegenerators(j, k)%add_to_parameters = .false.
                randfireinfo(i)%firegenerators(j, k)%parameter_column_label = 'NULL'
                randfireinfo(i)%firegenerators(j, k)%position = 1
                randfireinfo(i)%firegenerators(j, k)%temp_flag = .false.
                randfireinfo(i)%firegenerators(j, k)%kilo_flag = .false.
                randfireinfo(i)%firegenerators(j, k)%real_array(1:mxpntsarray) = -1001.0_eb
                randfireinfo(i)%firegenerators(j, k)%int_array(1:mxpntsarray) = -1001
                randfireinfo(i)%firegenerators(j, k)%logic_array(1:mxpntsarray) = .false.
                randfireinfo(i)%firegenerators(j, k)%char_array(1:mxpntsarray) = 'NULL'
                randfireinfo(i)%firegenerators(j, k)%randptr%val => randfireinfo(i)%firegenerators(j, k)%rand_value
            end do
        end do
    end do
    
    
    return
    
    end subroutine preprocessor_initialize
    
    !
    !-------------------create_case---------------------------
    !
    
    subroutine create_case(filename, iteration)
    
    character(len=*), intent(in) :: filename
    integer :: i, iteration
    
    call add_filename_to_parameters(filename)
    do i = 1, n_rndfires
        call randfireinfo(i)%do_rand(iteration)
        !call randfireinfo(i)%write_value
    end do 
    do i = 1, n_fields
        call fieldinfo(fieldptr(i))%do_rand(iteration)
        !call fieldinfo(fieldptr(i))%write_value
    end do 
    
    end subroutine create_case
    
    !
    !------------------initialize_output_files
    !
    
    subroutine initialize_output_files
    
    integer :: ios, i
    
    call initialize_preprocessor_output_routines(mxfields, mxseeds, mxgenerators)
    call open_preprocessor_outputfiles(datapath, workpath, project, parameterfile, ios)
    if (ios /= 0) then
        call cfastexit('initialize_output_files',1)
    end if
    do i = 1, n_fields
        call setup_col_parameters_output(fieldinfo(i))
    end do
    do i = 1, n_rndfires
        call setup_col_parameters_output(randfireinfo(i))
    end do
    call flush_parameters_buffer
    return
    
    end subroutine initialize_output_files
    
    !
    !---------write_seeds_outputfile-------------------
    !
    
    subroutine write_seeds_outputfile
    
    integer :: i
    type(random_generator_type), pointer :: genptr
    
    do i = 1, n_generators
        genptr => generatorinfo(i)
        call add_seeds_to_seeds_buffer(genptr%id, genptr%base_seeds)
    end do
    call flush_seeds_buffer
    
    end subroutine write_seeds_outputfile
    
    end module preprocessor_routines
        
    