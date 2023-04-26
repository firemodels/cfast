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
    
    integer :: maxrowio, maxcolio, nstart, maxrowtmp, maxcoltmp
    integer :: iunit, iunit2, iunitc, iunitd
    integer :: nend, maxcolout, icol, rcol, iskip
    integer :: nfiles
    logical :: lend, tmplend, header
    
    !real(eb), allocatable :: issx(:, :), ossx(:, :), tmpx(:, :)
    !character, allocatable :: issc(:, :)*(128), ossc(:, :)*(128), tmpc(:,:)*(128)
    real(eb) :: issx(2, 600), ossx(2, 600)
    real(eb) :: compx(6010, 600), devx(6010,600), tcol(6010)
    character :: issc(2, 600)*(128), ossc(2, 600)*(128)
    character :: compc(6010, 600)*(128), devc(6010, 600)*(128)
    logical :: test_read
    
    integer :: i, j, ierr, ioerr, ios
    character(len=256) :: outfile, compfile, devfile
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
    numr = 6010
    numc = 600
    
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
        write(*,*)'parameterfile = ',trim(parameterfile)
    end if 
    if (trim(outpath) == 'NULL') then
        outpath = ' '
        outpath = trim(datapath)
    end if
    outfile = ' '
    outfile = trim(project) // '_diagnostic.csv'
    write(*,*)'outfile = ',trim(outfile)
    write(*,*)'workpath = ',trim(workpath)
    write(*,*)'outpath = ',trim(outpath)
    
    lbuf = ' '
    lbuf = trim(workpath) // trim(parameterfile)
    obuf = ' '
    obuf = trim(outpath) // trim(outfile)
    open(newunit = iunit, file = trim(lbuf),iostat = ios)
    write(*,*)'ios 1 = ',ios, iunit
    if (ios /= 0) then
        write(*,*)'ios 1 = ',ios
        call cfastexit('cdata_diagnostic',1)
    end if
    open(newunit = iunit2, file = trim(obuf))
    write(*,*)'test that I am using the correct CData'
    write(*,*)'ios 2 = ',ios, iunit2
    if (ios /= 0) then
        write(*,*)'ios 1 = ',ios
        call cfastexit('cdata_diagnostic',2)
    end if
    
    write(*,*)'Before if n_diag>0', n_diag
    if (n_diag>0) then
        ebuf = trim(outpath) // trim('cdata_diagnostics.log')
        write(*,*)'ebuf = ',trim(ebuf)
        open(newunit = ioerr, file = ebuf, iostat = ios)
        if (ios/=0) then
            write(*,*)'ios = ',ios
            call cfastexit('cdata_diagnostic',3)
        end if
    end if 
    write(*,*)'after '
    
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
    write(*,*)'diagptr%test_column = ', trim(diagptr%test_column)
    
    if(icol < maxcolio)then
        do i = icol + 1, icol + iskip
            ossc(1,i) = trim(diagptr%col_hdrs(i-icol))
            ossx(1,i) = 0.0_eb
            write(*,*)'ossc(1,',i,') = ',trim(ossc(1,i))
        end do
        do i = icol+1, maxcolio
            ossc(1,i+iskip) = trim(issc(1,i))
            ossx(1,i+iskip) = 0.0_eb
        end do
    end if
    open(newunit = iunit2, file = obuf, status='old', iostat=ios)
    if (ios /= 0) then
        call cfastexit('diagnostics',4)
    end if
    write(*,*) 'Before writecsv'
    call writecsvformat(iunit2, ossx, ossc, 2, numc, 1, 1, maxcolio, iofill)
    write(*,*) 'After writecsv'
    close(iunit2)
    
    lend = .false.
    i = 0
    do while(.not.lend)
        i = i + 1
        !write(*,*)'lend = ',lend, i
        call readcsvformat(iunit, issx, issc, 2, numc, nstart, 1, maxrowio, maxcolio, lend)
        call copyrow
        if (issx(1, rcol) == time_end) then
            if (issx(1, icol) > 0 .and. issx(1, icol) < time_end) then
                write(*,*)'Do_test call'
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
        !write(*,*) 'Before writecsvformat in do while(.not.lend) loop', trim(ossc(1,1))
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
    
    integer :: i, j, ilen, ihrr, iflux, n, itime
    integer :: maxrowc, maxcolc, maxrowd, maxcold
    character :: buf1*(512), buf2*(512)
    real(eb) :: tott, tott2
    
    ilen = len_trim(ossc(1,1))
    find_beg:do i = 1, ilen
        if (ossc(1,1)(i:i) /= ' ') then
            buf1 = trim(datapath) // trim(ossc(1,1)(i:ilen-3))
            exit find_beg
        end if    
    end do find_beg
    buf2 = trim(buf1) // '_' // trim(diagptr%sec_fld(1)) // '.csv'
    buf1 = trim(buf1) // '_' // trim(diagptr%fst_fld(1)) // '.csv'
    write(*,*)'buf1 = ',trim(buf1)
    write(*,*)'buf2 = ',trim(buf2)
    open(newunit=iunitc, file=buf1)
    open(newunit=iunitd, file=buf2)
    call readcsvformat(iunitc, compx, compc, numr, numc, nstart, -1, maxrowc, maxcolc, test_read)
    call readcsvformat(iunitd, devx, devc, numr, numc, nstart, -1, maxrowd, maxcold, test_read)
    write(*,*)'End reads'
    write(*,*)'numr,maxrowc,maxrowd',numr,maxrowc,maxrowd
    write(*,*)'numc,maxcolc,maxcold',numc,maxcolc,maxcold
    find_fire:do i = 2, maxcolc
        if (trim(compc(3,i)) == trim(diagptr%fst_fld(2))) then
            do j = i, maxcolc
                if (trim(compc(2,j)) == trim(diagptr%fst_fld(3))) then
                    ihrr = j
                    exit find_fire
                end if
            end do
        end if
    end do find_fire
    write(*,*)'End find_fire', ihrr
    iflux = 0
    find_flux:do i = 2, maxcold
        write(*,*)'devc(3,i) = ',trim(devc(3,i)),i
        write(*,*)'sec_fld(2) = ',trim(diagptr%sec_fld(2))
        if (trim(devc(3,i)) == trim(diagptr%sec_fld(2))) then
            do j = i, maxcold
                write(*,*)'devc(2,j) = ',trim(devc(2,j)),j
                write(*,*)'sec_fld(3) = ',trim(diagptr%sec_fld(3))
                if (trim(devc(2,j)) == trim(diagptr%sec_fld(3))) then
                    iflux = j
                    exit find_flux
                end if
            end do
        end if
    end do find_flux
    write(*,*)'End find_flux', iflux
    if (iflux == 0) then
        write(*,*)trim(diagptr%sec_fld(2)),' ',trim(diagptr%sec_fld(3))
        call cfastexit('diagnostics:do_test',1)
    end if
    n = 0
    tcol(1:5) = 0.0_eb
    tott = 0.0_eb
    tott2 = 0.0_eb
    do i = 5, maxrowc
        if (compx(i,ihrr) >= diagptr%cutoffs(1)) then
            tcol(i) = devx(i,iflux)/compx(i,ihrr)
            tott = tott + tcol(i)
            tott2 = tott2 + tcol(i)**2
            n = n + 1
        else
            tcol(i) = 0.0_eb
        end if 
    end do 
    tott = tott/n
    tott2 = sqrt(tott2/n - tott**2)
    do i = 5, maxrowc
        if (tcol(i)>0.0_eb) then
            tcol(i) = (tcol(i) - tott)/tott2
        end if
    end do
    find_time:do i = 10, maxrowc
        if (compx(i,1) >= issx(1,icol)) then
            itime = i
            exit find_time
        end if
    end do find_time
    if (tcol(itime) < diagptr%cutoffs(2)) then
        ossx(1, icol + 1) = 0.0_eb
        ossc(1,icol + 1) = '0.0'
        ossx(1, icol + 2) = issx(1,icol)
        ossc(1,icol + 2) = ' '
        ossx(1, icol + 3) = tcol(itime)
        ossc(1,icol + 3) = ' '
    else
        ossx(1, icol + 1) = 1.0_eb
        ossc(1,icol + 1) = ' '
        ossx(1, icol + 2) = time_end
        ossc(1,icol + 2) = ' '
        ossx(1, icol + 3) = tcol(itime)
        ossc(1,icol + 3) = ' '
        find_new:do i = itime + 10, maxrowc
            if (devx(i,icol) > diagptr%criterion) then
                write(*,*)'devx(i,icol) = ',devx(i,icol),icol,i
                if(tcol(i) < diagptr%cutoffs(2)) then
                    ossx(1, icol + 1) = 2.0_eb
                    ossx(1, icol + 2) = devx(i,1)
                    write(*,*)'devx(1,i) = ', devx(i,1),i
                    ossx(1, icol + 3)  = tcol(i)
                    write(*,*)'tcol(i) = ',tcol(i)
                    exit find_new
                end if
            end if
        end do find_new
    end if
    close(iunitc)
    close(iunitd)
    write(*,*)'returning to main diagnostics'
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
    do i = 1, 5
        if (i <= 3) then
            diaginfo(1:mxdiags)%fst_fld(i) = 'NULL' 
            diaginfo(1:mxdiags)%sec_fld(i) = 'NULL'
            if (i < 3) then
                diaginfo(1:mxdiags)%zero_except(i) = 'NULL'
            end if
        end if
        diaginfo(1:mxdiags)%cutoffs(i) = -1001.0_eb
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