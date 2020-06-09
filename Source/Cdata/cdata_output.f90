module preprocessor_output_routines
    
    use precision_parameters
    use preprocessor_types, only: value_wrapper_type
    
    implicit none
    save
    
    private
    
    character(len=128), allocatable, dimension(:), target :: parameters_array
    character(len=128), allocatable, dimension(:,:), target :: seeds_array
    integer :: mxparam, mxseeds, n_param, n_seeds, mxgen
    integer :: ioparam, ioseeds
    
    public initialize_preprocessor_output_routines, setup_col_parameters_output, open_preprocessor_outputfiles, &
        flush_parameters_buffer, add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer, &
        close_preprocessor_outputfiles
    
    contains
    
    !
    !----------------------initialize_preprocessor_output_routines---------
    !
    
    subroutine initialize_preprocessor_output_routines(tmp_mxparams, tmp_mxseeds, tmp_mxgen)
    
        integer, intent(in) :: tmp_mxparams, tmp_mxseeds, tmp_mxgen
    
        mxparam = tmp_mxparams
        mxseeds = tmp_mxseeds
        mxgen = tmp_mxgen
    
        allocate(parameters_array(mxparam))
        parameters_array(1:mxparam) = ' '
        n_param = 1
        parameters_array(n_param) = 'File Names'
    
        allocate(seeds_array(mxgen, mxseeds+1))
        seeds_array(1,1) = 'Generator IDs'
        seeds_array(1,2) = 'Initialization Seed 1'
        seeds_array(1,3) = 'Initialization Seed 2'
        seeds_array(2:mxseeds, 1:3) = ' '
        n_seeds = 1
    
    end subroutine initialize_preprocessor_output_routines
    
    !
    !--------------setup_col_parameters_output--------------------
    !
    
    subroutine setup_col_parameters_output(field)
    
        class (value_wrapper_type), intent(inout) :: field

        if (field%add_to_parameters) then
            n_param = n_param + 1
            call field%add_header(n_param, parameters_array)
        end if
    
    end subroutine setup_col_parameters_output
    
    !
    !----------------open_preprocessor_outputfiles----------------
    !
    
    subroutine open_preprocessor_outputfiles(path, project, ios)
    
        character(len=*), intent(in) :: path, project
        integer, intent(out) :: ios
        
        character(len=512) :: buf
        
        buf = trim(path) // trim(project) // '_parameters.csv'
        open(newunit=ioparam, file=buf, action='write', iostat=ios)
        if (ios /= 0) return
        
        buf = trim(path) // trim(project) // '_seeds.csv'
        open(newunit=ioseeds, file=buf, action='write', iostat=ios)
    
    end subroutine open_preprocessor_outputfiles
    
    !
    !-----------flush_parameters_puffer
    !
    
    subroutine flush_parameters_buffer
    
        character(len=2000) :: buf
        integer :: i
        
        buf = ' '
        buf = trim(adjustl(parameters_array(1))) // ','
        parameters_array(1) = ' '
        do i = 2, n_param
            buf = trim(buf) // trim(adjustl(parameters_array(i))) // ','
            parameters_array(i) = ' '
        end do
        
        write(ioparam, '(a)') buf
        return
        
    end subroutine flush_parameters_buffer
    
    !
    !--------add_filename_to_parameters
    !
    
    subroutine add_filename_to_parameters(filename)
    
        character(len=*), intent(in) :: filename
    
        integer :: ibeg, iend, i
    
        iend = len_trim(filename)
        ibeg = 1
        search: do i = iend, 1, -1
            if (filename(i:i) == '\' .or. filename(i:i) == '/') then
                ibeg = i+1
                exit search
            end if
        end do search
        parameters_array(1) = filename(ibeg:iend)
    end subroutine add_filename_to_parameters
    
    !
    !--------add_seeds_to_seeds_buffer---------------------------
    !
    
    subroutine add_seeds_to_seeds_buffer(id, seeds)
    
        integer, intent(in) :: seeds(2)
        character(len=*), intent(in) :: id
    
        integer :: i
    
        n_seeds = n_seeds+1
        seeds_array(n_seeds,1) = trim(id)
        do i = 1, mxseeds
            write(seeds_array(n_seeds,i+1),'(i15)') seeds(i)
        end do
    
    end subroutine add_seeds_to_seeds_buffer
    
    !
    !--------flush_seeds_buffer-----------------------------
    !
    
    subroutine flush_seeds_buffer
    
        character(len=1000) :: buf
        integer :: i, j
        
        do i = 1, n_seeds
            buf = ' '
            buf = trim(buf) // trim(seeds_array(i,1)) // ','
            do j = 1, mxseeds
                buf = trim(buf) // trim(seeds_array(i,j+1)) // ','
            end do
            write(ioseeds, '(a)') buf
        end do
    
    end subroutine flush_seeds_buffer
    
    !
    !----------close_preprocessor_outputfiles
    !
    
    subroutine close_preprocessor_outputfiles
    
        close(ioparam)
        close(ioseeds)
        
    end subroutine close_preprocessor_outputfiles
    
    end module preprocessor_output_routines
    
    