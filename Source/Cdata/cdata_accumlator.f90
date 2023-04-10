module accumulator_routines
    
    use precision_parameters
    use setup_data, only: datapath, project, extension, iofill, debugging
    
    use exit_routines, only: cfastexit
    use input_routines, only: exehandle
    use initialization_routines, only : initialize_memory
    use input_routines, only : open_files, read_input_file
    use utility_routines, only : read_command_options, readcsvformat
    
    use preprocessor_types, only: random_generator_type
    
    use analysis_data, only: n_stats, statinfo, outpath
    use pp_params, only: mxgenerators, mxpntsarray, mxseeds, mxfields, rnd_seeds, restart_values
    use montecarlo_data, only: generatorinfo, n_generators, fieldinfo, n_fields, mc_write_seeds, &
        workpath, parameterfile
    
    use namelist_input_pp_routines, only: namelist_acc_input
    use preprocessor_output_routines, only: flush_parameters_buffer, setup_col_parameters_output, &
        open_preprocessor_outputfiles, initialize_preprocessor_output_routines, &
        add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer
    
    implicit none
    
    private

    public accumulator, writecsvformat

    contains
    
    !-------------------------accumulator------------------------------------------------
    
    subroutine accumulator 
    
    implicit none

! Variables
    
    integer, parameter :: numr = 2
    integer, parameter :: numc = 3000
    
    integer :: iunit, maxrowio, maxcolio, nstart, iunit2, maxrowtmp, maxcoltmp
    integer :: nend, maxcolout
    integer nfiles
    logical :: lend, tmplend, header
    
    real(eb), allocatable :: iossx(:, :), tmpx(:, :)
    character, allocatable :: iossc(:, :)*(128), tmpc(:,:)*(128)
    
    integer :: i, j, ierr, ios
    character(len=256) :: outfile
    character(len=512) :: lbuf, obuf

    
    allocate(iossx(numr, numc), tmpx(numr, numc))
    allocate(iossc(numr, numc), tmpc(numr, numc))
    
! Body of GetData

    call initialize_memory
    call read_command_options
    call open_files
    
    call namelist_acc_input
    
    if (trim(workpath) == 'NULL') then
        workpath = ' '
        workpath = trim(datapath)
    end if 
    if (trim(parameterfile) == 'NULL') then
        parameterfile = ' '
        parameterfile = trim(project) // '_parameters.csv'
    end if 
    if (trim(outpath) == 'NULL') then
        outpath = ' '
        outpath = trim(datapath)
    end if
    outfile = ' '
    outfile = trim(project) // '_accumulate.csv'
    
    lbuf = ' '
    lbuf = trim(workpath) // trim(parameterfile)
    obuf = ' '
    obuf = trim(outpath) // trim(outfile)
    open(newunit = iunit, file = trim(lbuf))
    
    nstart = 1
    nend = 1
    lend = .false.
    header = .true.
    i = 1
    call readcsvformat(iunit, iossx, iossc, numr, numc, nstart, 2, maxrowio, maxcolio, lend)
    if (.not.lend) then
        call fndOpenMCFile(iossc(2,1), workpath, iunit2, ierr)
        if (ierr == 0) then
            call readcsvformat(iunit2, tmpx, tmpc, 2, numc, 1, -1, maxrowtmp, maxcoltmp, tmplend)
            close(iunit2)
            if (.not. tmplend) then
                do  j = 2, maxcoltmp
                    iossc(1,maxcolio + j - 1) = tmpc(1,j)
                    iossx(2,maxcolio + j - 1) = tmpx(2,j)
                    header = .false.
                end do
            else
                maxcoltmp = 0
            end if
            maxcolout = maxcolio + maxcoltmp - 1
            open(newunit = iunit2, file = obuf, iostat = ios)
            call writecsvformat(iunit2, iossx, iossc, numr, numc, 1, 2, maxcolout, iofill)
            close(iunit2) 
        else
            write(*,*) 'Accumulator:First case does not open'
            call cfastexit('accumulator', 1)
        end if
    else
        write(*,*) 'Accumulator:Unit 1 could not read iofile'
        call cfastexit('acumulator', 2)
    end if
    
    nfiles = 0
    do while (.not. lend)
        call readcsvformat(iunit, iossx, iossc, numr, numc, 1, 1, maxrowio, maxcolio, lend)
        if (debugging) write(*,*)'file = ',trim(iossc(1,1))
        call fndOpenMCFile(iossc(1,1), workpath, iunit2, ierr)
        if (ierr == 0) then
            call readcsvformat(iunit2, tmpx, tmpc, 2, numc, 1, 2, maxrowtmp, maxcoltmp, tmplend)
            close(iunit2)
        else
            maxcoltmp = 0
        end if 
        if (.not. tmplend .and. header) then
            do  j = 2, maxcoltmp
                iossc(1,maxcolio + j - 1) = tmpc(1,j)
                iossx(2,maxcolio + j - 1) = tmpx(2,j)
                 header = .false.
                maxcolout = maxcolio + maxcoltmp - 1
            end do
        else if (tmplend) then
            maxcoltmp = 0
        end if
        do j = 2, maxcoltmp
            iossx(1,maxcolio + j - 1) = tmpx(2,j)
        end do 
        open(newunit=iunit2,file = obuf, position = 'append')
        call writecsvformat(iunit2, iossx, iossc, numr, numc, 1, 1, maxcolout, iofill)
        close(iunit2)
        nfiles = nfiles + 1
    end do 
    close(iunit)
    
    write (*,'(a,i0,a)') 'Created summary file from ', nfiles, ' CFAST input files'
    
    return

    end subroutine accumulator
    
    ! --------------------------- writecsvformat -------------------------------------------

    subroutine writecsvformat (iunit, x, c, numr, numc, nstart, maxrow, maxcol, iofill)

    !     routine: writecsvformat
    !     purpose:writess a comma-delimited file as generated by Micorsoft Excel, assuming that all
    !              the data is in the form of real numbers
    !     arguments: iunit  = logical unit, already open to .csv file
    !                x      = array of dimension (numr,numc) for values in spreadsheet
    !                c      = character array of same dimenaion as x for character values in spreadsheet
    !                numr   = # of rows of arrays x and c
    !                numc   = # of columns of arrays x and c
    !                nstart = starting row of spreadsheet to read
    !                maxrow   = actual number of rows read
    !                maxcol   = actual number of columns read
    !                iofill   = logical unit number for writing error messages (if any)

    implicit none

    integer, parameter :: eb = selected_real_kind(12)


    integer, intent(in) :: iunit, numr, numc, nstart, iofill, maxrow, maxcol

    real(eb), intent(in) :: x(numr,numc)
    character, intent(inout) :: c(numr,numc)*(128)

    character :: buf*10000
    integer :: i, j, ic, ie, d1
    
    write(*,*)'begining of writecsvformat x(1), c(1)',x(1,1), trim(c(1,1))
    write(*,*)nstart, maxrow
    do i = nstart, maxrow
        write(*,*)'first line in loop writecsvformat',i
        buf = '                    '
        ic = 1
        do j = 1, maxcol
            write(*,*)'in j loop',j, ic
            if (x(i,j) /= 0.0) then
                write(c(i,j),'(e16.9)') x(i,j)
            end if
            d1 = len_trim(c(1,j))
            ie = ic + len_trim(c(i,j))
            write(*,*)'i,j,ic,ie,d1',i,j,ic,ie,d1
            buf(ic:ie) = trim(c(i,j))
            ic = ie+1
            buf(ic:ic) = ','
            ic = ic+1
            if (ic > 10000) then
                write(iofill,*) 'WRITECSVFORMAT:Line to long for CSV format'
                call cfastexit('WRITECSVFORMAT', 1)
            end if
        end do
        write(*,*)'buf = ',buf(1:80)
        write(iunit,'(A10000)') buf(1:ic)
    end do
    
    return
    end subroutine writecsvformat
    
    !-------------------clr_arrays(x, c, numr, numc)
    
    subroutine clr_arrays(x, c, numr, numc)

    implicit none
    
    integer, parameter :: eb = selected_real_kind(12)

    integer, intent(in) :: numr, numc
    real(eb), intent(out) :: x(numr, numc)
    character, intent(out) :: c(numr, numc)*(*)

    integer :: i, j
    
    do i = 1, numr
        do j = 1, numc
            x(i,j) = 0.0_eb
            c(i,j) = ' '
        end do
    end do
    return
    end subroutine clr_arrays
    
    !---------------------------------------fndOpenMCFile(filename, path, iunit)----------------------------
    
    subroutine fndOpenMCFile(filename, path, iunit, ierr)
    
    implicit none

    integer, parameter :: eb = selected_real_kind(12)
    
    character, intent(in) :: filename*(*), path*(*)
    integer, intent(out) :: iunit, ierr
    
    character :: fn*(128)
    integer :: i, lc, fc
    logical :: exists
    
    ierr = 0
    fn = trim(filename)
    lc = len_trim(fn)
    fc = 1
    do i = lc-1, 1, -1
        if (fn(i:i)=='/'.or.fn(i:i)=='\') then
            fc = i + 1
            exit
        else if (fn(i:i) == '.') then 
            lc = i-1
        end if
    end do
    
    fn = trim(path) // fn(fc:lc) // '_calculations.csv'
    inquire(file = fn, exist = exists)
    if (exists) then 
        open(newunit = iunit, file = fn, iostat = ierr)
    else
        ierr = -1
    end if 
    
    return
    end subroutine fndOpenMCFile
    
end module accumulator_routines