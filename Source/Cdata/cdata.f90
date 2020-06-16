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
    
    use montecarlo_data, only: mc_number_of_cases
    use preprocessor_routines, only: preprocessor
    use accumulator_routines, only: accumulator
    use correlationtree_routines, only: correlationtree
    use enoughdone_routines, only: enoughdone 
    use preprocessor_output_routines, only: flush_parameters_buffer

    implicit none
    
    integer :: i, loop, status, ilen
    character(len = 255) :: buf

    if (command_argument_count().eq.0) then
        call cfastexit('CData Main',1)
    else
        loop = 2
        call get_command_argument(loop, buf, ilen, status)
        if (ilen > 0) then
            select case (trim(buf))
                case ('preprocessor')
                    call preprocessor 
                case ('accumulator')
                    call accumulator
                case ('correlationtree')
                    call correlationtree
                case ('enoughdone') 
                    call enoughdone
                case default 
                    call cfastexit('CData Main', 2)
            end select 
        end if
    end if

    call cfastexit ('CData', 0)

    end program CData
    
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
    
