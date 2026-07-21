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
        mxiterations, set_current_rng_seed_from_cdata
    use montecarlo_data, only: generatorinfo, n_generators, fieldinfo, n_fields, mc_write_seeds, mc_number_of_cases, &
        n_rndfires, randfireinfo, mc_max_iterations, workpath, parameterfile, fieldptr, validation_output
    use preprocessor_types, only: field_pointer, fire_generator_type, random_generator_type
    
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
    call set_current_rng_seed_from_cdata(rnd_seeds)
    mc_write_seeds = .false.

    mc_max_iterations = mxiterations
    validation_output = validation_flag

    call allocate_generator_info
    call allocate_field_info
    call allocate_random_fire_info

    return

    end subroutine preprocessor_initialize

    subroutine allocate_generator_info

    integer :: i

    n_generators = 0
    allocate(generatorinfo(mxgenerators))
    do i = 1, mxgenerators
        call initialize_generator(generatorinfo(i))
    end do

    end subroutine allocate_generator_info

    subroutine allocate_field_info

    integer :: i

    n_fields = 0
    allocate(fieldinfo(mxfields))
    do i = 1, mxfields
        call initialize_field(fieldinfo(i))
    end do
    allocate(fieldptr(mxfields))
    fieldptr(1:mxfields) = -1001

    end subroutine allocate_field_info

    subroutine allocate_random_fire_info

    integer :: i

    n_rndfires = 0
    allocate(randfireinfo(mxrandfires))
    do i = 1, mxrandfires
        call initialize_random_fire(randfireinfo(i))
    end do

    end subroutine allocate_random_fire_info

    subroutine initialize_generator(generator)

    type(random_generator_type), target, intent(inout) :: generator

    generator%id = 'NULL'
    generator%fyi = 'NULL'
    generator%type_dist = 'NONE'
    generator%value_type = 'NONE'
    if (.not. allocated(generator%char_array)) allocate(generator%char_array(mxpntsarray))
    generator%char_array(1:mxpntsarray) = 'NULL'
    generator%real_array(1:mxpntsarray) = -1001._eb
    generator%int_array(1:mxpntsarray) = -1001
    generator%logic_array(1:mxpntsarray) = .false.
    generator%prob_array(1:mxpntsarray) = -1001._eb
    generator%seeds = -1001
    call generator%set_max_value(d1mach(2))
    call generator%set_min_value(-d1mach(2))
    call generator%set_add_value(0.0_eb)
    generator%max_offset = 0.0_eb
    generator%min_offset = 0.0_eb
    generator%mean = 0._eb
    generator%stdev = 0._eb
    generator%alpha = 0._eb
    generator%beta = 0._eb
    generator%peak = 0._eb
    generator%first = .true.
    generator%use_seeds = .false.

    end subroutine initialize_generator

    subroutine initialize_field(field)

    type(field_pointer), target, intent(inout) :: field

    field%id = 'NULL'
    field%fyi = 'NULL'
    field%add_to_parameters = .false.
    field%parameter_column_label = 'NULL'
    field%position = 1
    field%temp_flag = .false.
    field%kilo_flag = .false.
    field%press_flag = .false.
    field%real_array(1:mxpntsarray) = -1001.0_eb
    field%int_array(1:mxpntsarray) = -1001
    field%logic_array(1:mxpntsarray) = .false.
    if (.not. allocated(field%char_array)) allocate(field%char_array(mxpntsarray))
    field%char_array(1:mxpntsarray) = 'NULL'
    field%randptr%val => field%rand_value

    end subroutine initialize_field

    subroutine initialize_random_fire(random_fire)

    type(fire_generator_type), target, intent(inout) :: random_fire
    integer :: j, k

    random_fire%id = 'NULL'
    random_fire%fireid = 'NULL'
    random_fire%basefireid = 'NULL'
    random_fire%hrrscalegenid = 'NULL'
    random_fire%timescalegenid = 'NULL'
    random_fire%smoldergenid = 'NULL'
    random_fire%smoldertimegenid = 'NULL'
    random_fire%compname(1:mxrooms) = 'NULL'
    random_fire%compindex(1:mxrooms) = -1001
    random_fire%generate_fire = .false.
    random_fire%first_time_point_smoldering = .false.
    random_fire%modifyfirearea = .false.
    random_fire%scalehrr = .false.
    random_fire%scaletime = .false.
    random_fire%dostime = .false.
    random_fire%smoldering_fire = .false.
    random_fire%hrrscalevalue = -1001.0_eb
    random_fire%timescalevalue = -1001.0_eb
    random_fire%stimevalue = -1001.0_eb
    call initialize_field(random_fire%fire_comp)
    call initialize_field(random_fire%fire_label)
    call initialize_field(random_fire%fs_fire_ptr)
    call initialize_field(random_fire%smolder_hrr_ptr)
    call initialize_field(random_fire%flame_hrr_ptr)
    call initialize_field(random_fire%smolder_time_ptr)
    call initialize_field(random_fire%flame_time_ptr)
    do j = 1, 2
        do k = 1, mxpts
            call initialize_field(random_fire%firegenerators(j, k))
        end do
    end do

    end subroutine initialize_random_fire
    
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
        call add_seeds_to_seeds_buffer(genptr%id, genptr%base_seeds(1:mxseeds))
    end do
    call flush_seeds_buffer
    
    end subroutine write_seeds_outputfile
    
    end module preprocessor_routines
