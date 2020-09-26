module preprocessor_output_routines
    
    use precision_parameters
    use preprocessor_types, only: value_wrapper_type
    use montecarlo_data, only: mc_max_iterations
    
    implicit none
    save
    
    private
    
    character(len=128), allocatable, dimension(:), target :: parameters_array
    character(len=128), allocatable, dimension(:,:), target :: seeds_array
    character(len=256) :: work_dir
    integer :: mxparam, mxseeds, n_param, n_seeds, mxgen
    integer :: ioparam, ioseeds, iobat, iounix
    
    public initialize_preprocessor_output_routines, setup_col_parameters_output, open_preprocessor_outputfiles, &
        flush_parameters_buffer, add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer, &
        close_preprocessor_outputfiles, finish_batch, open_cfast_inputfile
    
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
            call field%add_header(n_param, parameters_array)
        end if
    
    end subroutine setup_col_parameters_output
    
    !
    !----------------open_preprocessor_outputfiles----------------
    !
    
    subroutine open_preprocessor_outputfiles(inpath, workpath, project, parameterfile, ios)
    
        character(len=*), intent(in) :: inpath, project
        character(len=*), intent(inout) :: workpath, parameterfile
        integer, intent(out) :: ios
        
        character(len=512) :: buf
        
        if (trim(workpath) == 'NULL') then
            workpath = ' '
            workpath = trim(inpath)
        end if
        work_dir = ' '
        work_dir = workpath
        if (trim(parameterfile) == 'NULL') then
            parameterfile = ' '
            parameterfile = trim(project) // '_parameters.csv'
        end if 
        
        buf = ' '
        buf = trim(workpath) // trim(parameterfile)
        open(newunit=ioparam, file=buf, action='write', iostat=ios)
        if (ios /= 0) return
        
        buf = ' '
        buf = trim(workpath) // trim(project) // '_seeds.csv'
        open(newunit=ioseeds, file=buf, action='write', iostat=ios)
        
        buf = ' '
        buf = trim(workpath) // trim(project) // '.bat'
        open(newunit=iobat, file=buf, action='write', iostat=ios)
        
        buf = ' '
        buf = trim(workpath) // trim(project) // '.sh'
        open(newunit=iounix, file=buf, action='write', iostat=ios, recordtype = 'STREAM_LF')
        call start_batch
    
    end subroutine open_preprocessor_outputfiles
    
    !
    !-----------flush_parameters_buffer
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
        call add_filename_to_batch(filename,ibeg,iend)
        
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
    !-----------------------start_batch--------------------------------
    !
    
    subroutine start_batch
    
        write(iobat,'(a)') 'echo off'
        write(iobat,'(a)') 'rem change the path to background.exe and cfast.exe as appropriate. Here we just assume it is in the path'
        write(iobat,'(a)') 'set bgexe=background.exe'
        write(iobat,'(a)') 'set CFAST_EXE=cfast.exe'
        write(iobat,'(a,i0)') 'set MAX_ITER=', mc_max_iterations
        write(iobat,'(a)') ' '
        write(iobat,'(a)') 'rem you should not need to change anything from here on'
        write(iobat,'(a)') 'set bg=%bgexe% -u 85 -d 0.1'
        write(iobat,'(a)') 'set CFAST=%bg% %CFAST_EXE%'
        
        write(iounix,'(a)')'#/bin/bash'
        write(iounix,'(a)') 'CFAST=~/firemodels/cfast/Build/CFAST/intel_linux_64/cfast7_linux_64'
        write(iounix,'(a)') 'DELAY=3'
        write(iounix,'(a)') 'BATCH=batch3'
        write(iounix,'(a)') ''
        write(iounix,'(a)') 'export STOPFDSMAXITER=100000'
        write(iounix,'(a)') ''
    
    end subroutine start_batch
    
    !
    !-------------------add_filename_to_batch
    !
    
    subroutine add_filename_to_batch(filename, ibeg, iend)
    
        character(len=*), intent(in) :: filename
        integer, intent(in) :: ibeg, iend
        
        character(len=256) :: outfilename
        integer :: i, idot, ib, ie
        
        do i = iend, ibeg, -1
            if (filename(i:i) == '.') then
                idot = i
                exit
            end if
        end do
    
        ie = len_trim(filename)
        ib = 1
        search: do i = ie, 1, -1
            if (filename(i:i) == '\' .or. filename(i:i) == '/') then
                ib = i+1
                exit search
            end if
        end do search
        
        outfilename = filename(ibeg:idot) // 'stop'
        write(iobat, '(a18, a)') 'echo %MAX_ITER% > ', trim(outfilename)
        write(iobat,'(a8, a, a3)') '%CFAST% ', filename(ibeg:iend), ' -v'
        
        write(iounix,'(a39,a)') 'qfds.sh -D $DELAY -e $CFAST -q $BATCH  ', filename(ib:iend)
        
    end subroutine add_filename_to_batch 
    
    !
    !---------------finish_batch--------------------------------
    !
    
    subroutine finish_batch
    
    ! finish up Windows batch file'
    write(iobat,'(a)') ' '
    write(iobat,'(a)') ':loop1'
    write(iobat,'(a)') 'tasklist | find /i /c "CFAST" > temp.out'
    write(iobat,'(a)') 'set /p numexe=<temp.out'
    write(iobat,'(a)') 'echo waiting for %numexe% jobs to finish'
    write(iobat,'(a)') 'if %numexe% == 0 goto finished'
    write(iobat,'(a)') 'Timeout /t 30 >nul' 
    write(iobat,'(a)') 'goto loop1'
    write(iobat,'(a)') ':finished'
    close(unit = iobat)

    write(iounix,'(a)') 
    close(unit = iounix)
    
    end subroutine finish_batch
    
    !
    !----------close_preprocessor_outputfiles
    !
    
    subroutine close_preprocessor_outputfiles
    
        close(ioparam)
        close(ioseeds)
        close(iobat)
        
    end subroutine close_preprocessor_outputfiles
    
    !
    !--------open_cfast_inputfile------
    
    subroutine open_cfast_inputfile(io, filename, ios)
        
        character(len=*), intent(in) :: filename
        integer, intent(out) :: io
        integer, intent(out) :: ios
        
        open (newunit=io, file=filename, action='write', iostat=ios)
        
    end subroutine open_cfast_inputfile
    
    end module preprocessor_output_routines
        
    