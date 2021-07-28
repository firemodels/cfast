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

    program CData

    use precision_parameters
    
    use exit_routines, only: cfastexit
    use preprocessor_routines, only: preprocessor
    use accumulator_routines, only: accumulator
    use statistics_routines, only: statistics
    use preprocessor_output_routines, only: flush_parameters_buffer
    use setup_data, only: program_name, cdata_accumulator, cdata_preprocessor, cdata_statistics, &
        exepath, datapath, project, extension
    use input_routines, only: exehandle
    use output_routines, only: output_version
    use utility_routines, only: read_command_options

    implicit none
    
    integer :: program_version

    program_name = 'CData'
    program_version = 7701
    
    if (command_argument_count().eq.0) then
        call output_version(0,program_name,program_version)
        call cfastexit('CData Main',0)
    end if
    
    ! main program loop
    call exehandle (exepath, datapath, project, extension)
    call read_command_options

    if (cdata_preprocessor) then
        call output_version(0,program_name,program_version)
        write(*,'(a)') 'PreProcessor'
        call preprocessor
    elseif (cdata_accumulator) then
        call output_version(0,program_name,program_version)
        write(*,'(a)') 'Accumulator'
        call accumulator
    elseif (cdata_statistics) then
        call output_version(0,program_name,program_version)
        write(*,'(a)') 'Statistics'
        call statistics
    else
        call cfastexit('CData Main', 2)
    end if


    call cfastexit ('CData', 0)

    end program CData
    
    subroutine post_process
    
    ! We don't use this in CData, but need a dummy routine to resolve linker error
    return

    end subroutine  post_process
    