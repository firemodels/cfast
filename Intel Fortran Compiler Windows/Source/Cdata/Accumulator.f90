!  MCAccumulator.f90 
!
!  FUNCTIONS:
!  MCAccumulator - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: MCAccumulator
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program MCAccumulator

    implicit none

    integer, parameter :: eb = selected_real_kind(12)

! Variables
    
    integer, parameter :: numr = 2
    integer, parameter :: numc = 3000
    
    integer :: iunit, maxrowio, maxcolio, maxrowcmd, maxcolcmd, nstart, iofill, iunit2, maxrowtmp, maxcoltmp
    integer :: nend, maxcolout, iflag
    
    real(eb), allocatable :: iossx(:, :), cmdssx(:, :), tmpx(:, :)
    character, allocatable :: iossc(:, :)*(128), cmdssc(:, :)*(128), tmpc(:,:)*(128)
    
    integer :: i, j, maxrowend
    character :: lbuf*(256), infile*(128), cmdfile*(128), outfile*(128), tmpext*(128)

    
    allocate(iossx(numr, numc), cmdssx(numr, numc), tmpx(2, numc))
    allocate(iossc(numr, numc), cmdssc(numr, numc), tmpc(2, numc))
    
! Body of GetData
    !call clr_arrays(iossx, iossc, numr, numc)
    !call clr_arrays(cmdssx, cmdssc, numr, numc)
    
    call do_cmd_line(infile, cmdfile, outfile)
    open(newunit = iunit, file = infile)
    nstart = 1
    
    open(newunit = iunit2, file = cmdfile)
    call readcsvformat(iunit2, cmdssx, cmdssc, numr, numc, nstart, -1, maxrowcmd, maxcolcmd, iflag, iofill)
    close(iunit2)
    
    nend = 1
    iflag = -1 
    i = 1
    
    call readcsvformat(iunit, iossx, iossc, numr, numc, nstart, 2, maxrowio, maxcolio, iflag, iofill)
    if (iflag > 0) then
        call fndOpenMCFile(iossc(2,1),cmdssc(1,2),iunit2)
        call readcsvformat(iunit2, tmpx, tmpc, 2, numc, 1, -1, maxrowtmp, maxcoltmp, iflag, iofill)
        close(iunit2)
        if (iflag > 0) then
            do  j = 2, maxcoltmp
                iossc(1,maxcolio + j - 1) = tmpc(1,j)
                iossx(2,maxcolio + j - 1) = tmpx(2,j)
                write(iossc(2,maxcolio + j - 1),*) iossx(2,maxcolio + j - 1)
            end do 
            maxcolout = maxcolio + maxcoltmp-1
            open(newunit = iunit2, file = outfile)
            call writecsvformat(iunit2, iossx, iossc, numr, numc, 1, 2, maxcolout, iofill)
            close(iunit2)
        else
            stop 'Number 2 first case does not open'
        end if
    else
        stop 'Number 1 could not read iofile'
    end if
        
    
    !do i = 2, maxrowend
    call readcsvformat(iunit, iossx, iossc, numr, numc, nstart, 1, maxrowio, maxcolio, iflag, iofill)
    do while (iflag > 0)
        write(*,*)'file = ',trim(iossc(i,1))
        if (iflag > 0) then
            call fndOpenMCFile(iossc(i,1),cmdssc(1,2),iunit2)
            call readcsvformat(iunit2, tmpx, tmpc, 2, numc, 1, -1, maxrowtmp, maxcoltmp, iflag, iofill)
            close(iunit2)
            do j = 2, maxcoltmp
                iossx(1,maxcolio + j - 1) = tmpx(2,j)
                write(iossc(i,maxcolio + j - 1),*) iossx(i,maxcolio + j - 1)
            end do 
            open(newunit=iunit2,file = outfile, position = 'append')
            call writecsvformat(iunit2, iossx, iossc, numr, numc, 1, 1, maxcolout, iofill)
            close(iunit2)
        end if 
        call readcsvformat(iunit, iossx, iossc, numr, numc, nstart, 1, maxrowio, maxcolio, iflag, iofill)
    end do 
    close(iunit)

    !open(newunit = iunit, file = outfile, position = 'append')
    !call writecsvformat(iunit, iossx, iossc, numr, numc, 1, nend, maxcolio, iofill)
    !close(iunit)

    end program MCAccumulator
    
    !----------------------------docmdline-------------------------------------------------
    
    subroutine do_cmd_line(infile, cmdfile, outfile)
    
    character, intent(out) :: infile*(128), cmdfile*(128), outfile*(128)
    
    character :: lbuf*(128)
    integer :: i, j
    
    i = command_argument_count()
    
    if (i >= 1) then
        j = 1
        call GET_COMMAND_ARGUMENT(j, lbuf)
        infile = trim(lbuf)
    else
        infile = 'CFASTparameters.csv'
        !stop 'Need at least a input file on command line'
    end if
    
    if (i >= 2) then
        j = 2
        call GET_COMMAND_ARGUMENT(j, lbuf)
        cmdfile = trim(lbuf)
    else
        cmdfile = 'commands.csv'
    end if
    
    if (i > 2) then
        j = 3
        call GET_COMMAND_ARGUMENT(j, lbuf)
        outfile = trim(lbuf)
    else
        j = len_trim(infile)
        do while (infile(j:j) /= '.') 
            j = j - 1
            if (j == 1) stop 'input file must have an extension'
        end do
        k = j
        do while (infile(k:k) /= '\')
            k = k - 1
            if (k == 1) exit
        end do 
        j = j - 1
        k = k + 1
        outfile = infile(k:j)
        !outfile(j:j+8)='_out.csv'
        outfile = trim(outfile) // '_out.csv'
    end if
    
    return
    end subroutine do_cmd_line
    
    
    ! --------------------------- readcsvformat -------------------------------------------

    subroutine readcsvformat (iunit, x, c, numr, numc, nstart, nend, maxrow, maxcol, iflag, iofill)

    !     routine: readcsvformat
    !     purpose: reads a comma-delimited file as generated by Micorsoft Excel, assuming that all
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

    integer, intent(in) :: iunit, numr, numc, nstart, iofill, nend

    integer, intent(out) :: maxrow, maxcol, iflag
    real(eb), intent(out) :: x(numr,numc)
    character, intent(out) :: c(numr,numc)*(*)

    character :: in*204800, token*128
    integer :: i, j, nrcurrent, ic, icomma, ios, nc

    maxrow = 0
    maxcol = 0
    iflag = -1
    do i=1,numr
        do j=1,numc
            x(i,j) = 0.0_eb
            c(i,j) = ' '
        end do
    end do

    ! if we have header rows, then skip them
    if (nstart>1) then
        do  i=1,nstart-1
            read (iunit,'(A)') in
        end do
    end if

    ! read the data
    nrcurrent = 0
20  if (nrcurrent == nend) then
        goto 100
    end if 
    read (iunit,'(A)',end=100) in

    ! Skip comments and blank lines
    if (in(1:1)=='!'.or.in(1:1)=='#'.or.in==' ') then
        go to 20
    end if

    nrcurrent = nrcurrent+1
    iflag = 1
    maxrow = max(maxrow,nrcurrent)

    ! Cannot exceed work array
    if (maxrow>numr) then
        write (*,'(a,i0,1x,i0)') '***Error: Too many rows or columns in input file, r,c = ', maxrow, maxcol
        write (iofill,'(a,i0,1x,i0)') '***Error: Too many rows or columns in input file, r,c = ', maxrow, maxcol
        stop
    end if

    nc=0
    ic=1
30  icomma=index(in,',')
    if (icomma/=0) then
        if (icomma==ic) then
            token=' '
        else
            token=in(ic:icomma-1)
        end if
        ic = icomma+1
        nc = nc + 1
        in(1:ic-1)=' '
        if (nrcurrent<=numr.and.nc<=numc) then
            c(nrcurrent,nc) = token
            read (token,'(f128.0)',iostat=ios) x(nrcurrent,nc)
            if (ios/=0) x(nrcurrent,nc) = 0
            go to 30
        else
            write (*,'(a,i0,a,i0)') 'Too many rows or columns in input file, r,c=', nrcurrent, ' ', nc
            write (iofill,'(a,i0,a,i0)') 'Too many rows or columns in input file, r,c=', nrcurrent, ' ', nc
            stop
        end if
    end if
    nc = nc + 1
    maxcol=max(maxcol,nc)
    token = in(ic:ic+100)
    c(nrcurrent,nc) = token
    read (token,'(f128.0)',iostat=ios) x(nrcurrent,nc)
    if (ios/=0) x(nrcurrent,nc) = 0
    go to 20

100 continue

    return
    end subroutine readcsvformat
    
    
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
    character, intent(inout) :: c(numr,numc)*(*)

    character :: buf*204800, token*128
    integer :: i, j, nrcurrent, ic, icomma, ios, nc, ie
    real(eb) :: tmp
    
    do i = nstart, maxrow
        buf = '                    '
        ic = 1
        do j = 1, maxcol
            if (x(i,j) /= 0.0) then
                write(c(i,j),'(e15.9)') x(i,j)
            end if
            ie = ic + len_trim(c(i,j))
            buf(ic:ie) = trim(c(i,j))
            ic = ie+1
            buf(ic:ic) = ','
            ic = ic+1
        end do
        write(iunit,'(A)') buf(1:ic)
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
    
    !---------------------------------------fndOpenMCFile(filename, path, iunit)---------------------------------------------------------------------------------------------
    
    subroutine fndOpenMCFile(filename, path,iunit)
    
    implicit none

    integer, parameter :: eb = selected_real_kind(12)
    
    character, intent(in) :: filename*(*), path*(*)
    integer, intent(out) :: iunit
    
    character :: fn*(128)
    integer :: i, j, lc, fc
    
    fn = trim(filename)
    lc = len_trim(fn)
    do i = lc-1, 1, -1
        if (fn(i:i)=='/'.or.fn(i:i)=='\') then
            fc = i + 1
            exit
        end if
    end do
    
    fn = trim(path) // fn(fc:lc) // '_mc.csv'
    open(newunit = iunit,file = fn)
    
    return
    end subroutine fndOpenMCFile
        

