
! --------------------------- cfast -------------------------------------------

    program cfast

    !     Routine: cfast (main program)
    !     Purpose: main program for the model

    !     Permission is hereby granted, free of charge, to any person
    !     obtaining a copy of this software and associated documentation
    !     files (the "Software"), to deal in the Software without
    !     restriction, including without limitation the rights to use,
    !     copy, modify, merge, publish, distribute, sublicense, and/or sell
    !     copies of the Software, and to permit persons to whom the
    !     Software is furnished to do so, subject to the following
    !     conditions:

    !     The above copyright notice and this permission notice shall be
    !     included in all copies or substantial portions of the Software.

    !     The software is provided "as is", without warranty of any kind,
    !     express or implied, including but not limited to the warranties
    !     of merchantability, fitness for a particular purpose and
    !     noninfringement. In no event shall the authors or copyright
    !     holders be liable for any claim, damages or other liability,
    !     whether in an action of contract, tort or otherwise, arising
    !     from, out of or in connection with the software or the use or
    !     other dealings in the software.

    use precision_parameters
    
    use initialization_routines, only : initialize_memory, initialize_fire_objects, initialize_species, initialize_walls
    use input_routines, only : open_files, read_input_file
    use output_routines, only: output_version, output_initial_conditions
    use solve_routines, only : solve_simulation
    use utility_routines, only : cptime, read_command_options
    use radiation_routines, only : radiation

    use setup_data, only: cfast_version, stime, iofill, i_time_step, time_end, deltat, i_time_end, validation_flag, &
                            monte_carlo_flag
    use option_data, only: total_steps

    implicit none

    real(eb) :: xdelt, tstop, tbeg, tend

    cfast_version = 7402        ! Current CFAST version number

    if (command_argument_count().eq.0) then
        call output_version(0)
        call cfastexit('MAIN',0)
    end if

    ! initialize the basic memory configuration

    stime = 0.0_eb
    call initialize_memory
    call read_command_options
    call open_files

    call output_version (iofill)

    call read_input_file

    call initialize_species

    i_time_step = 1
    xdelt = time_end/deltat
    i_time_end = xdelt + 1
    tstop = i_time_end - 1

    call initialize_walls (tstop)

    call output_initial_conditions

    monte_carlo_flag = .true. 
    call cptime(tbeg)
    call solve_simulation (tstop)
    call cptime(tend)

    if (.not.validation_flag) write (*,5000) tend - tbeg
    if (.not.validation_flag) write (*,5010) total_steps
    write (iofill,5000) tend - tbeg
    write (iofill,5010) total_steps
    call cfastexit ('MAIN', 0)

5000 format ('Total execution time = ',1pg10.3,' seconds')
5010 format ('Total time steps = ',i10)

    end program cfast

! --------------------------- cfastexit -------------------------------------------

    subroutine cfastexit (name, errorcode)

    ! called when CFAST exits, printing an error code if necessary
    ! inputs    name        routine name calling for exit
    !           errorcode   numeric code indicating which call to cfastexit in routine

    use output_routines, only : deleteoutputfiles, closeoutputfiles
    use spreadsheet_routines, only : output_spreadsheet_montecarlo
    use setup_data

    character, intent(in) :: name*(*)
    integer, intent(in) :: errorcode

    if (errorcode==0) then
        if (.not.validation_flag) write (*, '(''Normal exit from CFAST routine: '',a)') 'CFAST'
        write (iofill, '(''Normal exit from CFAST routine: '',a)') 'CFAST'
    elseif (trim(name)=='SOLVE_SIMULATION' .and. errorcode==5) then
        ! validation flag test is for the maximum iteration exit is because of CFASTBot's testing to make
        !   sure that CFAST can initialize and run a few steps of all the cases in debug mode but doesn't run
        !   to completion. DO NOT CHANGE WITHOUT CHANGING CFASTBOT. 
        if (.not.validation_flag) write (*, '(''Maximum iteration exit from CFAST routine: '',a)') trim(name)
        write (iofill, '(''Maximum iteration exit from CFAST routine: '',a)') trim(name)
    else
        write (*,'(''***Error exit from CFAST routine: '',a,'' code = '',i0)') trim(name), errorcode
        write (iofill,'(''***Error exit from CFAST routine: '',a,'' code = '',i0)') trim(name), errorcode
    end if

    if (monte_carlo_flag) then
        monte_carlo_flag = .false.
        call output_spreadsheet_montecarlo
    end if
    call closeoutputfiles 
    close (unit=iofilkernel, status='delete')
    call deleteoutputfiles (stopfile)

    stop

    end subroutine cfastexit
    

