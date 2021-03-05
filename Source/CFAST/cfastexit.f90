module exit_routines

    use precision_parameters
    
    use namelist_data, only: input_file_line, input_file_line_number
    use setup_data, only: validation_flag, iofilo, iofill, iofilstat, smv_out_interval, iofilsmv, iofilsmvplt, iofilsmvzone, &
        ss_out_interval, iofilssc, iofilssd, iofilssm, iofilssv, iofilssdiag, iofilcalc, stopfile, program_name, errormessage
    
    implicit none

    contains

    ! --------------------------- cfastexit -------------------------------------------

!> \brief   called when CFAST exits, printing an error code if necessary
!> \param   name        routine name calling for exit
!> \param   errorcode   numeric code indicating which call to cfastexit in routine

    subroutine cfastexit (name, errorcode)
    
    character(len=*), intent(in) :: name
    integer, intent(in) :: errorcode
    integer exitcode

    exitcode = errorcode
    if (errorcode/=0) then
        if (trim(name)=='solve_simulation' .and. errorcode==5) then
            ! validation flag test for the maximum iteration exit is because of CFASTBot's testing to make
            !   sure that CFAST can initialize and run a few steps of all the cases in debug mode but doesn't run
            !   to completion. DO NOT CHANGE WITHOUT CHANGING CFASTBOT.
            if (.not.validation_flag) write (*, '(''Maximum iteration exit from '',a)') program_name
            if (iofill/=0) write (iofill, '(''Maximum iteration exit from CFAST'',a)') program_name
            exitcode = 0
        else
            if (errormessage/='') write (*,'(a)') errormessage
            if (input_file_line/='') write (*,'(a,i0,a,a)') 'Error on line ',input_file_line_number, &
                ' of the input file: ', trim(input_file_line)
            write (*,'(''***Error exit from '',a,'', error '',i0,'' from routine '',a)') program_name, exitcode, trim(name)
            if (iofill/=0) then
                if (errormessage/='') write (*,'(a)') errormessage
                if (input_file_line/='') write (iofill,'(a,i0,a,a)') 'Error on line ',input_file_line_number, &
                    ' of the input file: ', trim(input_file_line)
                write (iofill,'(''***Error exit from '',a,'', error '',i0,'' from routine '',a)') program_name, &
                exitcode, trim(name)
            end if
        end if
    else
        if (.not.validation_flag) write (*, '(''Normal exit from '',a)') program_name
        if (iofill/=0) write (iofill, '(''Normal exit from '',a)') program_name
    end if
    
    call closeoutputfiles
    call delete_output_files (stopfile)
    
    if (exitcode==0) then
        stop
    else
        error stop 1
    end if
    stop
    
    end subroutine cfastexit

! --------------------------- delete_output_files -------------------------------------------

    subroutine delete_output_files (outputfile)

    character(len=*), intent(in) :: outputfile
    integer :: fileunit, ios
    logical doesthefileexist

    inquire (file=outputfile, exist=doesthefileexist)
    if (DoesTheFileExist) then
        open (newunit=fileunit, iostat=ios, file=outputfile, status='old')
        if (ios==0) then
            close(fileunit, status='delete', iostat=ios)
            if (ios/=0) then
                write (iofill,'(a,i0,a)') 'Error opening output file, returned status = ', ios, &
                    '. File may be in use by another application.'
                write (*,'(a,i0,a)') 'Error opening output file, returned status = ', ios, &
                    '. File may be in use by another application.'
                call cfastexit('delete_output_files',1)
            end if
        end if
    end if

    return
    end subroutine delete_output_files
    
    !---------------------closeoutputfiles------------------------------------------------------
    
    subroutine closeoutputfiles

    !	closeoutputfile closes units from open_output_files
    !	Unit numbers defined here and read_input_file

    !	Unit numbers defined for various I/O purposes
    !
    !     iofili        solver.ini and data files (data file, tpp and objects)
    !     iofill        log file
    !     iofilo        output 
    !     iofilstat     write the status file
    !     iofilsmv      smokeview output (header) - note this is rewound each time the plot data is written)
    !     iofilsmvplt   smokeview output (plot data)
    !     iofilsmvzone  smokeview spreadsheet output 
    !     iofilssc      spreadsheet output (compartment and layer related data)
    !     iofilsswd     spreadsheet output (measurement and trigger devices)
    !     iofilssm      spreadsheet output (layer masses)
    !     iofilssv      spreadsheet output (vent flows)
    !     iofilssdiag   spreadsheet output (various diagnostics for verification)
    !     ioresid       diagnostic file of solution vector
    !     ioslab        diagnostic file of flow slabs
    !     iofilcalc     spredsheet output (for monte carlo analysis)
    
    ! other units may be opened with newunit keyword in open statement
    
    logical :: openunit

    ! first the file for "printed" output
    inquire (iofilo, opened=openunit)
    if(openunit) then
        close(iofilo)
    end if

    ! the status files
    inquire (iofilstat, opened=openunit)
    if(openunit) then
        close(iofilstat)
    end if

    ! the smokeview files
    if (smv_out_interval>0) then
        inquire(iofilsmv, opened=openunit)
        if (openunit) then
            close(iofilsmv)
        end if
        inquire(iofilsmvplt, opened=openunit)
        if (openunit) then
            close(iofilsmvplt)
        end if
        inquire(iofilsmvzone, opened=openunit)
        if (openunit) then
            close(iofilsmvzone)
        end if
    end if

    ! the spread sheet files
    if (ss_out_interval>0) then
        ! compartments spreadsheet
        inquire(iofilssc, opened=openunit)
        if(openunit) then
            close(iofilssc)
        end if
        ! devices spreadsheet
        inquire(iofilssd, opened=openunit)
        if (openunit) then
            close(iofilssd)
        end if
        ! masses spreadsheet
        inquire(iofilssm, opened=openunit)
        if (openunit) then
            close(iofilssm)
        end if
        ! vents spreadsheet
        inquire(iofilssv, opened=openunit)
        if (openunit) then 
            close(iofilssv)
        end if
        ! diagnostic spreadsheet
        inquire(iofilssdiag, opened=openunit)
        if (openunit) then
            close(iofilssdiag)
        end if
        ! calculations spreadhseet
        inquire(iofilcalc, opened=openunit)
        if (openunit) then
            close(iofilcalc)
        end if
    end if 
    
    end subroutine closeoutputfiles
    
end module exit_routines