
    !> \brief   main program for cfast

    program cfast

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
    
    use exit_routines, only: cfastexit
    use initialization_routines, only : initialize_memory, initialize_species, initialize_walls
    use input_routines, only : open_files, read_input_file
    use output_routines, only: output_version, output_initial_conditions
    use solve_routines, only : solve_simulation
    use utility_routines, only : cptime, read_command_options

    use option_data, only: total_steps
    use setup_data, only: cfast_version, stime, iofill, i_time_step, time_end, deltat, i_time_end, validation_flag, &
        program_name

    implicit none

    real(eb) :: xdelt, tstop, tbeg, tend 

    program_name = 'CFAST'
    ! Current CFAST version number is defined in setup_data

    if (command_argument_count().eq.0) then
        call output_version(0,program_name,cfast_version)
        call cfastexit('CFAST',0)
        stop
    end if

    ! initialize the basic memory configuration

    stime = 0.0_eb
    call initialize_memory
    call read_command_options
    call open_files

    call output_version (iofill,'CFAST',cfast_version)

    call read_input_file

    call initialize_species

    i_time_step = 1
    xdelt = time_end/deltat
    i_time_end = xdelt + 1
    tstop = i_time_end - 1

    call initialize_walls

    call output_initial_conditions

    call cptime(tbeg)
    call solve_simulation (tstop)
    call cptime(tend)

    if (.not.validation_flag) write (*,5000) tend - tbeg
    if (.not.validation_flag) write (*,5010) total_steps
    write (iofill,5000) tend - tbeg
    write (iofill,5010) total_steps

    call post_process

    call cfastexit ('CFAST', 0)

5000 format ('Total execution time = ',1pg10.3,' seconds')
5010 format ('Total time steps = ',i10)

    end program cfast
    
    subroutine post_process
    
    use spreadsheet_routines, only : output_spreadsheet_dump
    use dump_data, only: n_dumps
    use setup_data, only: ss_out_interval
    
    ! create the spreadsheet file of calculation results if necessary
    if (n_dumps/=0.and.ss_out_interval/=0.) then
        call output_spreadsheet_dump
    end if

    end subroutine  post_process
