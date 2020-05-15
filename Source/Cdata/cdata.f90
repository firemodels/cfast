!  PreProcessor.f90 
!
!  FUNCTIONS:
!  PreProcessor - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: PreProcessor
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program PreProcessor

    use precision_parameters
    
    use initialization_routines, only : initialize_memory
    use input_routines, only : open_files, read_input_file
    use output_routines, only: delete_output_files, closeoutputfiles
    use utility_routines, only : read_command_options

    use dump_data, only: n_dumps
    use option_data, only: total_steps
    use setup_data, only: cfast_version, stime, iofill, i_time_step, time_end, deltat, i_time_end, validation_flag, &
        ss_out_interval, inputfile
    
    use write_inputfile_routines, only: write_cfast_infile
    use montecarlo_data, only: mc_number_of_cases
    use montecarlo_routines, only: create_mc_filename, process_mc_filename_pattern
    use preprocessor_routines, only: create_case
    use namelist_input_pp_routines, only: namelist_pp_input
    use preprocessor_output_routines, only: flush_parameters_buffer

    implicit none

    integer :: i
    real(eb) :: xdelt, tstop, tbeg, tend 
    character :: infilecase*(256)

    cfast_version = 7500        ! Current CFAST version number

    if (command_argument_count().eq.0) then
        call cfastexit('PreProcessor',1)
    end if

    ! initialize the basic memory configuration

    call initialize_memory
    call read_command_options
    call open_files

    call read_input_file
    
    call namelist_pp_input
    call flush_parameters_buffer
    call process_mc_filename_pattern
    do i = 1, mc_number_of_cases
        call create_mc_filename(i, infilecase)
        call create_case
        call write_cfast_infile(infilecase)
        call flush_parameters_buffer
    end do

    call cfastexit ('PreProcessor', 0)

5000 format ('Total execution time = ',1pg10.3,' seconds')
5010 format ('Total time steps = ',i10)

    end program PreProcessor
    
! --------------------------- cfastexit -------------------------------------------

    subroutine cfastexit (name, errorcode)

    ! called when CFAST exits, printing an error code if necessary
    ! inputs    name        routine name calling for exit
    !           errorcode   numeric code indicating which call to cfastexit in routine

    use output_routines, only: closeoutputfiles, delete_output_files
    use setup_data, only: validation_flag, iofill, iofilkernel, stopfile
    
    character, intent(in) :: name*(*)
    integer, intent(in) :: errorcode


    if (errorcode/=0) then
        write (*,'(''***Error exit from PrePocessor, error '',i0,'' from routine '',a)') errorcode, trim(name)
        if (iofill/=0) write (iofill,'(''***Error exit from PreProcessor, error '',i0,'' from routine '',a)') &
            errorcode, trim(name)
    else
        if (.not.validation_flag) write (*, '(''Normal exit from PreProcessor'')')
        if (iofill/=0) write (iofill, '(''Normal exit from PreProcessor'')')
    end if
    
    call closeoutputfiles
    close (unit=iofilkernel, status='delete')
    call delete_output_files (stopfile)

    stop

    end subroutine cfastexit
    
