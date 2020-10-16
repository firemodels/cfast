module statistics_routines
    
    use ifport
    
    use precision_parameters
    
    use exit_routines, only: cfastexit
    use setup_data, only: datapath, project, extension
    
    use pp_params, only: mxgenerators, mxpntsarray, mxseeds, mxfields, rnd_seeds, restart_values, &
        mxstats
    use setup_data, only: cfast_input_file_position, iofili, inputfile
    
    use input_routines, only: exehandle
    use namelist_input_pp_routines, only: namelist_stt_input
    
    use preprocessor_types, only: random_generator_type
    use analysis_types, only: stat_type
    
    use analysis_data, only: statinfo, n_stats
    use montecarlo_data, only: generatorinfo, n_generators, fieldinfo, n_fields, mc_write_seeds
    use preprocessor_output_routines, only: flush_parameters_buffer, setup_col_parameters_output, &
        open_preprocessor_outputfiles, initialize_preprocessor_output_routines, &
        add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer
    
    implicit none
    
    private

    public statistics 

    contains
    
    !-------------------------statistics------------------------------------------------
    
    subroutine statistics 
    
    character(len=256) :: buf
    character(len=256) :: exepath, datapath, project, extension
    integer(4) :: status
    integer :: i, ioerr
    
    call init_stats
    !call test_stats
    cfast_input_file_position = 3
    call exehandle(exepath, datapath, project, extension)
    buf = trim(datapath) // trim(project) // trim(extension)
    inputfile = trim(buf)
    call namelist_stt_input
    
    
    
    open(newunit = ioerr, file = 'cdata_statistics.log')
    
    do  i = 1, n_stats
        buf = ' '
        if (trim(statinfo(i)%analysis_type) == 'CORRELATION_TREE') then 
            buf = '"C:\Program Files\R\R-4.0.2\bin\Rscript" rpart.R outname="'
        else if (trim(statinfo(i)%analysis_type) == 'CONVERGENCE_OF_MEAN') then
            buf = '"C:\Program Files\R\R-4.0.2\bin\Rscript" converg.R outname='''
        else if (trim(statinfo(i)%analysis_type) == 'HISTOGRAM') then
            buf = '"C:\Program Files\R\R-4.0.2\bin\Rscript" hist.R outname="'
        else if (trim(statinfo(i)%analysis_type) == 'PDF_ESTIMATE') then
            buf = '"C:\Program Files\R\R-4.0.2\bin\Rscript" dens.R outname='''
        else 
            call cfastexit('statistics',1)
        end if 
        buf = trim(buf) // trim(statinfo(i)%outfile) // '" fname="' // trim(statinfo(i)%infile)
        buf = trim(buf) // '" i_fmt="' // trim(statinfo(i)%img_format) // '" yvar="' // trim(statinfo(i)%col_title)
        if (trim(statinfo(i)%logfile) == 'NULL') then
            statinfo(i)%logfile = ' '
            statinfo(i)%logfile = trim(statinfo(i)%outfile) // '.err'
        endif 
        buf = trim(buf) // '" >' // trim(statinfo(i)%logfile)
        write(*,*) buf
        write(ioerr,'(a)') buf
        status = system(buf)
    end do 
    close(ioerr)
    
    end subroutine statistics
    
    !
    !----------------------init_stats------------------------------------------------
    !
    
    subroutine init_stats
    
    n_stats = 0
    allocate(statinfo(mxstats))
    statinfo(1:mxstats)%id = 'NULL'
    statinfo(1:mxstats)%fyi = ' '
    statinfo(1:mxstats)%infile = 'NULL'
    statinfo(1:mxstats)%outfile = 'NULL'
    statinfo(1:mxstats)%logfile = 'NULL'
    statinfo(1:mxstats)%errfile = 'NULL'
    statinfo(1:mxstats)%col_title = 'NULL'
    statinfo(1:mxstats)%img_format = 'NULL'
    
    end subroutine init_stats
    
    !
    !----------------------test_stats----------------------------------------
    
    subroutine test_stats
    
    n_stats = 1
    statinfo(n_stats)%id = 'Test 1'
    statinfo(n_stats)%analysis_type = 'CORRELATION_TREE'
    statinfo(n_stats)%infile = 'case0_accumulate.csv'
    statinfo(n_stats)%outfile = 'case0_ctree_Max_Temp_Bedroom_2'
    statinfo(n_stats)%errfile = 'case0_ctree_Max_Temp_Bedroom_2.err'
    statinfo(n_stats)%logfile = 'case0_ctree_Max_Temp_Bedroom_2.log'
    statinfo(n_stats)%col_title = 'Max Temp Bedroom 2'
    statinfo(n_stats)%img_format = 'JPG'
    n_stats = 2
    statinfo(n_stats)%id = 'Test 2'
    statinfo(n_stats)%analysis_type = 'CONVERGENCE_OF_MEAN'
    statinfo(n_stats)%infile = 'case0_accumulate.csv'
    statinfo(n_stats)%outfile = 'case0_convergence_Max_Temp_Bedroom_2'
    statinfo(n_stats)%errfile = 'convergence.err'
    statinfo(n_stats)%logfile = 'convergence.log'
    statinfo(n_stats)%col_title = 'Max Temp Bedroom 2'
    statinfo(n_stats)%img_format = 'JPG'
    n_stats = 3
    statinfo(n_stats)%id = 'Test 3'
    statinfo(n_stats)%analysis_type = 'HISTOGRAM'
    statinfo(n_stats)%infile = 'case0_accumulate.csv'
    statinfo(n_stats)%outfile = 'cased0_histogram_Max_Temp_Bedroom_2'
    statinfo(n_stats)%errfile = 'histogram.err'
    statinfo(n_stats)%logfile = 'histogram.log'
    statinfo(n_stats)%col_title = 'Max Temp Bedroom 2'
    statinfo(n_stats)%img_format = 'JPG'
    
    n_stats = n_stats + 1
    statinfo(n_stats)%id = 'Test 1'
    statinfo(n_stats)%analysis_type = 'CORRELATION_TREE'
    statinfo(n_stats)%infile = 'case0_accumulate.csv'
    statinfo(n_stats)%outfile = 'case0_ctree_Max_Temp_Bedroom_3'
    statinfo(n_stats)%errfile = 'case0_ctree_Max_Temp_Bedroom_3.err'
    statinfo(n_stats)%logfile = 'case0_ctree_Max_Temp_Bedroom_3.log'
    statinfo(n_stats)%col_title = 'Max Temp Bedroom 3'
    statinfo(n_stats)%img_format = 'JPG'
    n_stats = n_stats + 1
    statinfo(n_stats)%id = 'Test 2'
    statinfo(n_stats)%analysis_type = 'CONVERGENCE_OF_MEAN'
    statinfo(n_stats)%infile = 'case0_accumulate.csv'
    statinfo(n_stats)%outfile = 'case0_convergence_Max_Temp_Bedroom_3'
    statinfo(n_stats)%errfile = 'convergence.err'
    statinfo(n_stats)%logfile = 'convergence.log'
    statinfo(n_stats)%col_title = 'Max Temp Bedroom 3'
    statinfo(n_stats)%img_format = 'JPG'
    n_stats = n_stats + 1
    statinfo(n_stats)%id = 'Test 3'
    statinfo(n_stats)%analysis_type = 'HISTOGRAM'
    statinfo(n_stats)%infile = 'case0_accumulate.csv'
    statinfo(n_stats)%outfile = 'cased0_histogram_Max_Temp_Bedroom_3'
    statinfo(n_stats)%errfile = 'histogram.err'
    statinfo(n_stats)%logfile = 'histogram.log'
    statinfo(n_stats)%col_title = 'Max Temp Bedroom 3'
    statinfo(n_stats)%img_format = 'JPG'
    
    end subroutine test_stats
    
end module statistics_routines