module diagnostic_routines
    
    use precision_parameters
    use setup_data, only: datapath, project, extension, iofill, debugging, iofili, inputfile, &
        exepath
    
    use exit_routines, only: cfastexit
    use input_routines, only: exehandle
    use initialization_routines, only : initialize_memory
    use input_routines, only : open_files, read_input_file, exehandle
    use utility_routines, only : read_command_options, readcsvformat
    
    use preprocessor_types, only: preprocessor_type, random_generator_type
    use diagnostic_types, only: diagnostic_type
    
    use montecarlo_data, only: generatorinfo, n_generators, fieldinfo, n_fields, mc_write_seeds, &
        workpath, parameterfile
    
    use preprocessor_output_routines, only: flush_parameters_buffer, setup_col_parameters_output, &
        open_preprocessor_outputfiles, initialize_preprocessor_output_routines, &
        add_filename_to_parameters, add_seeds_to_seeds_buffer, flush_seeds_buffer
    
    use ifport
    
    use preprocessor_types, only: preprocessor_type
    use diagnostic_types, only: diagnostic_type
    
    use pp_params, only: mxgenerators, mxpntsarray, mxseeds, mxfields, rnd_seeds, restart_values, &
        mxdiags,mxdiagcols
    use setup_data, only: iofili, inputfile, ss_out_interval, time_end
    
    use namelist_input_pp_routines, only: namelist_diag_input

    use diagnostic_data, only: n_diag, diaginfo, outpath, diagptr
    use accumulator_routines, only: writecsvformat
    
    implicit none
    
    character(len=256) :: diag_output_path
    
    integer :: iunit, maxrowio, maxcolio, nstart, iunit2, maxrowtmp, maxcoltmp
    integer :: nend, maxcolout
    integer :: nfiles
    logical :: lend, tmplend, header
    integer :: iosunit, tmpunit(6)
    
    integer :: i, j, ierr
    character(len=256) :: outfile
    character(len=512) :: lbuf, obuf
    
    
    private

    public diagnostics

    contains
    
    !-------------------------diagnostics------------------------------------------------
    
    subroutine diagnostics
    
    implicit none

! Variables
    
    integer :: numr
    integer :: numc
    
    integer :: iunit, maxrowio, maxcolio, nstart, iunit2, maxrowtmp, maxcoltmp
    integer :: nend, maxcolout, icol, iunit3, iunit4, rcol, iskip
    integer nfiles
    logical :: lend, tmplend, header
    
    !real(eb), allocatable :: issx(:, :), ossx(:, :), tmpx(:, :)
    !character, allocatable :: issc(:, :)*(128), ossc(:, :)*(128), tmpc(:,:)*(128)
    real(eb) :: issx(2, 300), ossx(2, 300), tmpx(6001, 300), tcol(6001)
    character :: issc(2, 300)*(128), ossc(2, 300)*(128), tmpc(6001, 300)*(128)
    
    integer :: i, j, ierr, ioerr, ios
    character(len=256) :: outfile
    character(len=512) :: lbuf, obuf, ebuf, buf
    
! Body of GetData
    
    call exehandle(exepath, datapath, project, extension)
    buf = trim(datapath) // trim(project) // trim(extension)
    inputfile = trim(buf)

    call initialize_memory
    call diagnostic_initialize
    
    call namelist_diag_input

    diagptr => diaginfo(1)
    iskip = diagptr%column_skip
    numr = int(time_end/ss_out_interval)+1
    numc = 300
    !allocate(issx(2, numc), ossx(2, numc + iskip), tmpx(numr, numc))
    !allocate(issc(2, numc), ossx(2, numc + iskip), tmpc(numr, numc))
    !allocate(issx(2, numc), ossx(2, numc + iskip))
    !allocate(issc(2, numc), ossx(2, numc + iskip))
    
    workpath = 'NULL'
    parameterfile = 'NULL'
    outpath = 'NULL'
    if (trim(workpath) == 'NULL'.or.workpath(1:2) == '  ') then
        workpath = ' '
        workpath = trim(datapath)
    end if 
    if (trim(parameterfile) == 'NULL') then
        parameterfile = ' '
        parameterfile = trim(project) // '_accumulate.csv'
        write(*,*) 'parameterfile = ', trim(parameterfile)
    end if 
    if (trim(outpath) == 'NULL') then
        outpath = ' '
        outpath = trim(datapath)
    end if
    outfile = ' '
    outfile = trim(project) // '_diagnostic.csv'
    
    lbuf = ' '
    lbuf = trim(workpath) // trim(parameterfile)
    obuf = ' '
    obuf = trim(outpath) // trim(outfile)
    open(newunit = iunit, file = trim(lbuf))
    open(newunit = iunit2, file = trim(obuf))
    write(*,*) 'After opens n_diag = ', n_diag
    
    if (n_diag>0) then
        ebuf = trim(outpath) // trim('cdata_diagnostics.log')
        open(newunit = ioerr, file = ebuf)
    else
        return
    end if 
    
    nstart = 1
    nend = 1
    lend = .false. 
    header = .true. 
    write(*,*)'before first readcsvformat'
    call readcsvformat(iunit, issx, issc, 2, numc, nstart, 1, maxrowio, maxcolio, lend)
    write(*,*)'after first readcsvformat lend = ', lend
    find_col: do i = 1, maxcolio
        ossx(1,i) = issx(1,i)
        ossc(1,i) = trim(issc(1,i))
        if (trim(diagptr%test_column) == trim(issc(1,i))) then
            icol = i
            exit find_col
        elseif ('Max Time Run' == trim(issc(1,i))) then
            rcol = i
        end if
    end do find_col
    write(*,*)'After find_col loop, icol, rcol', icol, rcol
    
    if(icol < maxcolio)then
        do i = icol + 1, icol + iskip
            ossc(1,i) = trim(diagptr%col_hdrs(i-icol))
            ossx(1,i) = 0.0_eb
        end do
        do i = icol+1, maxcolio
            ossc(1,i+iskip) = trim(issc(1,i))
            ossx(1,i+iskip) = 0.0_eb
        end do
    end if
    open(newunit = iunit2, file = obuf, status='old', iostat=ios)
    if (ios /= 0) then
        call cfastexit('diagnostics',1)
    end if
    write(*,*) 'Before writecsv'
    call writecsvformat(iunit2, ossx, ossc, 2, numc, 1, 1, maxcolio, iofill)
    write(*,*) 'After writecsv'
    close(iunit2)
    
    lend = .false.
    i = 0
    do while(.not.lend)
        i = i + 1
        write(*,*)'lend = ',lend, i
        call readcsvformat(iunit, issx, issc, 2, numc, nstart, 1, maxrowio, maxcolio, lend)
        call copyrow
        if (issx(1, rcol) == time_end) then
            if (issx(1, icol) > 0 .and. issx(1, icol) < time_end) then
                call do_test    
            else 
                ossx(1, icol + 1) = -1
                ossc(1,icol + 1) = ' '
                ossx(1, icol + 2) = time_end
                ossc(1,icol + 2) = ' '
                ossx(1, icol + 3) = -1
                ossc(1,icol + 3) = ' '
            end if
        end if 
        open(newunit=iunit2,file = obuf, position = 'append', iostat = ios)
        if (ios /= 0) then
            call cfastexit('diagnostics',2)
        end if
        write(*,*) 'Before writecsvformat in do while(.not.lend) loop', trim(ossc(1,1))
        call writecsvformat(iunit2, ossx, ossc, 2, numc, 1, 1, maxcolio, iofill)
        close(iunit2)
    end do 
    
    close(ioerr)
    write(*,*) 'Before return'
    return
    
    contains
    
    subroutine copyrow
    
    integer :: i
    
    do i = 1, icol
        if (issx(1,i) /= 0.0_eb) then
            ossx(1,i) = issx(1,i)
            ossc(1,i) = ""
        else
            ossc(1,i) = trim(issc(1,i))
            ossx(1,i) = 0.0_eb
        end if
    end do
    do i = icol+1, icol+iskip
        ossx(1,i) = -1001.0_eb
        ossc(1,i) = " "
    end do
    do i = icol+1, maxcolio
        if (issx(1,i) /= 0.0_eb) then
            ossx(1,i+iskip) = issx(1,i)
            ossc(1,i) = ""
        else
            ossc(1,i+iskip) = trim(issc(1,i))
            ossx(1,i+iskip) = 0.0_eb
        end if
    end do
    
    end subroutine copyrow
    
    subroutine do_test
    
    end subroutine do_test
    
    end subroutine diagnostics
    
    ! --------------------------- diagnositic_initialize ----------------------------------
    
    subroutine diagnostic_initialize
    
    diag_output_path = 'NULL'
    
    n_diag = 0
    allocate(diaginfo(mxdiags))
    diaginfo(1:mxdiags)%id = 'NULL'
    diaginfo(1:mxdiags)%diagnostic = 'NULL'
    diaginfo(1:mxdiags)%test = 'NULL'
    diaginfo(1:mxdiags)%test_column = 'NULL'
    diaginfo(1:mxdiags)%n_col = 0
    do i = 1, 3
        diaginfo(1:mxdiags)%fst_fld(i) = 'NULL' 
        diaginfo(1:mxdiags)%sec_fld(i) = 'NULL'
        if (i < 3) then
            diaginfo(1:mxdiags)%zero_except(i) = 'NULL'
            diaginfo(1:mxdiags)%cutoffs(i) = -1001.0_eb
        end if
    end do
    diaginfo(1:mxdiags)%mx_hdrs = 5
    do i = 1, 5
        diaginfo(1:mxdiags)%col_hdrs(i) = 'NULL'
    end do
    diaginfo(1:mxdiags)%funit = 0
    diaginfo(1:mxdiags)%sunit = 0
    diaginfo(1:mxdiags)%fcol = 0
    diaginfo(1:mxdiags)%scol = 0
    diaginfo(1:mxdiags)%column_file = .false.
    diaginfo(1:mxdiags)%skip_col_cntr = -1001
    diaginfo(1:mxdiags)%criterion = -1001.0_eb
    
    return
    
    end subroutine diagnostic_initialize
    
    
end module diagnostic_routines