module preprocessor_routines

    use precision_parameters
    
    use setup_data, only: datapath, project, extension
    
    use pp_params, only: mxgenerators, mxpntsarray, mxseeds, mxfields, rnd_seeds, restart_values
    use montecarlo_data, only: generatorinfo, n_generators, fieldinfo, n_fields, mc_write_seeds
    use preprocessor_types, only: random_generator_type
    use preprocessor_output_routines, only: flush_parameters_buffer, setup_col_parameters_output, &
        open_preprocessor_outputfiles, initialize_preprocessor_output_routines, &
        add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer
    
    implicit none
    external cfastexit
    
    private

    public preprocessor_initialize, create_case, initialize_output_files, write_seeds_outputfile

    contains
    
    ! --------------------------- preprocessor_initialize ----------------------------------
    
    subroutine preprocessor_initialize
    
    integer :: i
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
    end do
    generatorinfo(1:mxgenerators)%max = 0._eb
    generatorinfo(1:mxgenerators)%min = 0._eb
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
    fieldinfo(1:mxfields)%add_to_parameters = .false.
    fieldinfo(1:mxfields)%parameter_header = 'NULL'
    
    return
    
    end subroutine preprocessor_initialize
    
    !
    !-------------------create_case---------------------------
    !
    
    subroutine create_case(filename)
    
    character(len=*), intent(in) :: filename
    integer :: i
    
    call add_filename_to_parameters(filename)
    do i = 1, n_fields
        call fieldinfo(i)%genptr%rand(fieldinfo(i)%valptr)
        call fieldinfo(i)%write_value
    end do 
    
    end subroutine create_case
    
    !
    !------------------initialize_output_files
    !
    
    subroutine initialize_output_files
    
    integer :: ios, i
    
    call initialize_preprocessor_output_routines(mxfields, mxseeds, mxgenerators)
    call open_preprocessor_outputfiles(datapath, project, ios)
    if (ios /= 0) then
        call cfastexit('initialize_output_files',1)
    end if
    do i = 1, n_fields
        call setup_col_parameters_output(fieldinfo(i))
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
        
    