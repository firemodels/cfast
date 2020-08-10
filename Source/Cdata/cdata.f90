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
    use montecarlo_data, only: mc_number_of_cases
    use preprocessor_routines, only: preprocessor
    use accumulator_routines, only: accumulator
    use statistics_routines, only: statistics
    use preprocessor_output_routines, only: flush_parameters_buffer
    use setup_data, only: program_name

    implicit none
    
    integer :: i, loop, status, ilen
    character(len = 255) :: buf

    program_name = 'CData'
    if (command_argument_count().eq.0) then
        call cfastexit('CData Main',1)
    else
        loop = 1
        call get_command_argument(loop, buf, ilen, status)
        if (ilen > 0) then
            if (trim(buf) =='preprocessor') then
                call preprocessor 
            elseif (trim(buf) == 'accumulator') then
                call accumulator
            elseif (trim(buf) == 'statistics') then
                call statistics  
            else 
                call cfastexit('CData Main', 2)
            end if  
        end if
    end if

    call cfastexit ('CData', 0)

    end program CData
    