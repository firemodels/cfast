    module utility_routines

    use precision_parameters
    
    use exit_routines, only: cfastexit

    use cparams, only: lbufln, mxss
    use room_data, only: nwpts, slab_splits, iwbound
    use setup_data, only: ncol, iofill, rundat, nokbd, initializeonly, debugging, validation_flag, outputformat, &
        netheatflux, ssoutoptions, cdata_accumulator, cdata_preprocessor, cdata_statistics, errormessage

    implicit none

    character(len=lbufln) :: lbuf

    ! unlike most other routines, this one does not have the private specifier since all routines here are intended to be
    ! used by other routines

    contains

    ! --------------------------- ssaddtolist -------------------------------------------

    subroutine ssaddtolist (ic, valu, array)

    real(eb), intent(in) :: valu
    real(eb), intent(out) :: array(*)
    integer, intent(inout) :: ic

    ic = ic + 1
    ! We are imposing an arbitrary limit of 32000 columns
    if (ic>mxss) return
    if (abs(valu)<=1.0e-100_eb) then
        array(ic) = 0.0_eb
    else
        array(ic) = valu
    end if
    return

    end subroutine ssaddtolist

    ! --------------------------- toIntString -------------------------------------------

    subroutine toIntString(i,istring)

    integer, intent(in) :: i
    character(len=*), intent(out) :: istring

    character(len=256) :: string

    if (i<10) then
        write (string,'(i1)') i
    else if (i<100) then
        write (string,'(i2)') i
    else if (i<1000) then
        write (string,'(i3)') i
    else if (i<10000) then
        write (string,'(i4)') i
    else if (i<100000) then
        write (string,'(i5)') i
    else if (i<1000000) then
        write (string,'(i6)') i
    else
        string = 'error'
    end if
    istring = trim(string)
    return
    end subroutine toIntString

    ! --------------------------- tanhsmooth ---------------------------------------

    real(eb) function tanhsmooth (x, xmax, xmin, ymax, ymin)

    ! calculate a smooth transition from 1 (at xmax) to zero (at xmin)
    ! arguments: x    current value
    !            xmax maximum value of independent variable. Return ymax above this value
    !            xmin minimum value of independent variable. Return ymin below this value
    !            ymax value returned at or above xmax
    !            ymin value return at or below xmin

    real(eb), intent(in) :: x, xmax, xmin, ymax, ymin
    real(eb) :: f
    f = min(max(0.5_eb + tanh(8.0_eb/(xmax-xmin)*(x-xmin)-4.0_eb)/2.0_eb,0.0_eb),1.0_eb)
    if (f<1.0d-6) f = 0.0_eb
    tanhsmooth = f*(ymax-ymin)+ymin


    return
    end function tanhsmooth

    ! --------------------------- d1mach -------------------------------------------

    real(eb) function d1mach (i)

    ! obtain machine-dependent parameters for the local machine environment.
    ! it is a function subprogram with one (input) argument. reference  p. a. fox, a. d. hall and
    ! n. l. schryer, framework for a portable library, acm transactions on mathematical software 4,
    ! 2 (june 1978), pp. 177-188.
    !     arguments:  i
    !
    !           where i = 1,...,5.  the (output) value of a above is determined by the (input) value of i.  the
    !           results for various values of i are discussed below.
    !
    !           d1mach(1) = b**(emin-1), the smallest positive magnitude.
    !           d1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
    !           d1mach(3) = b**(-t), the smallest relative spacing.
    !           d1mach(4) = b**(1-t), the largest relative spacing.
    !           d1mach(5) = log10(b)
    !
    !           assume single precision numbers are represented in the t-digit, base-b form
    !
    !              sign (b**e)*( (x(1)/b) + ... + (x(t)/b**t) )
    !
    !           where 0 .le. x(i) .lt. b for i=1,...,t, 0 .lt. x(1), and emin .le. e .le. emax.
    !
    !           the values of b, t, emin and emax are provided in i1mach as follows:
    !           i1mach(10) = b, the base.
    !           i1mach(11) = t, the number of base-b digits.
    !           i1mach(12) = emin, the smallest exponent e.
    !           i1mach(13) = emax, the largest exponent e.

    integer, intent(in) :: i

    real(eb) :: b, x

    x = 1.0_eb
    b = radix(x)
    select case (i)
    case (1)
        d1mach = b**(minexponent(x)-1) ! the smallest positive magnitude.
    case (2)
        d1mach = huge(x)               ! the largest magnitude.
    case (3)
        d1mach = b**(-digits(x))       ! the smallest relative spacing.
    case (4)
        d1mach = b**(1-digits(x))      ! the largest relative spacing.
    case (5)
        d1mach = log10(b)
    case default
        write (errormessage,'(''***Error, Internal error, illegal call to d1mach '',i0)') i
        call cfastexit('d1mach',1) 
    end select
    return
    end function d1mach

    ! --------------------------- cmdline -------------------------------------------

    subroutine cmdline (nargs,strs,iarg,iopt)

    ! gets argument list and options from command line. options may be of the form c:<string> where <c>
    !          is the desired option a-z and <string> is a character string associated with that option.
    ! arguments: nargs maximum number of arguments expected (dimension limit on input, actual number on output.
    !            strs  returned strings of arguments and options
    !            iarg  returned list of pointers to elements in strs corresponding to arguments 1..nargs
    !            iopt  returned list of pointers to elements in strs corresponding to options a-z

    integer, intent(inout) :: nargs
    integer, intent(out) :: iarg(nargs), iopt(26)
    character(len=*), intent(out) :: strs(nargs)

    integer :: ic, ia, i
    character(len=127) :: cmdlin
    character(len=1) :: optsep

    optsep = '-'

    do ic = 1, max(nargs,26)
        if (ic<=nargs) then
            strs(ic) = ' '
            iarg(ic) = 0
        end if
        iopt(ic) = 0
    end do

    ! get the command line to decipher
    call getcl(cmdlin)
    if (cmdlin/=' ') then

        ! get rid of extra spaces in the command line
        ic = 1
20      if (cmdlin(ic:ic+1)=='  ') then
            call cmove(cmdlin,ic,126,ic+1,127,127,' ')
        else
            ic = ic + 1
        end if
        if (cmdlin(ic:127)/=' '.and.ic<=126) go to 20
        if (cmdlin(1:1)==' ') then
            call cmove(cmdlin,1,126,2,127,127,' ')
        end if

        ! put in commas where appropriate to delimit all fields
        ic = 2
30      if (cmdlin(ic:ic)==' ') then
            if (cmdlin(ic-1:ic-1)/=','.and.cmdlin(ic+1:ic+1)/=',') then
                cmdlin(ic:ic) = ','
                ic = ic + 1
            else
                call cmove(cmdlin,ic,126,ic+1,127,127,' ')
            end if
        else if ((cmdlin(ic:ic)==optsep).and.cmdlin(ic-1:ic-1)/=',') then
            call cmove(cmdlin,ic+1,127,ic,126,ic,',')
            ic = ic + 2
        else
            ic = ic + 1
        end if
        if (cmdlin(ic:127)/=' '.and.ic<=126) go to 30
    end if

    ! parse command line into separate fields and process options
    ia = 0
40  ic = index(cmdlin,',')
    if (ic==0.and.cmdlin/=' ') ic = index(cmdlin,' ')
    if (ic/=0) then
        ia = ia + 1
        strs(ia) = ' '
        if (ic>1) strs(ia) = cmdlin(1:ic-1)
        call cmove(cmdlin,1,127,ic+1,127,127,' ')
        go to 40
    end if

    ! assign the parsed fields to appropriate arguments and options
    nargs = 0
    if (ia>0) then
        do i = 1, ia
            if (strs(i)(1:1)==optsep) then
                if (strs(i)(2:2)>='A'.and.strs(i)(2:2)<='Z') then
                    iopt(ichar(strs(i)(2:2))-ichar('A')+1) = i
                else if (strs(i)(2:2)>='a'.and.strs(i)(2:2)<='z') then
                    iopt(ichar(strs(i)(2:2))-ichar('a')+1) = i
                end if
                cmdlin = strs(i)
                call cmove(cmdlin,1,127,3,127,127,' ')
                if (cmdlin(1:1)==':') call cmove(cmdlin,1,127,2,127,127,' ')
                strs(i) = cmdlin
            else
                nargs = nargs + 1
                iarg(nargs) = i
            end if
        end do
    end if
    return
    end subroutine cmdline

    ! --------------------------- cmove -------------------------------------------

    subroutine cmove(cmdlin,i1,i2,i3,i4,i5,chr)

    ! cmove a substring in the command line to remove spaces.
    ! arguments: cmdlin command line string
    !            i1     beginning of substring to be moved
    !            i2     end of substring to be moved
    !            i3     beginning of destination
    !            i4     end of destination
    !            i5     position of newly vacated space in the string
    !            chr    character to fill that space

    character(len=1), intent(in) :: chr
    integer, intent(in) :: i1, i2, i3, i4, i5

    character(len=*), intent(inout) :: cmdlin

    character(len=127) :: temp

    temp = cmdlin
    temp(i1:i2) = cmdlin(i3:i4)
    temp(i5:i5) = chr
    cmdlin = temp
    return
    end subroutine cmove

    ! --------------------------- getcl -------------------------------------------

    subroutine getcl (cmdlin)

    ! get command line as a single string
    ! arguments: cmdlin - command line

    character(len=*), intent(out) :: cmdlin

    integer first, last, lpoint
    integer maxarg, iar, i, ic
    logical valid

    maxarg = 5 + 2
    lpoint = 0
    iar = command_argument_count()

    if (iar==0) then
        cmdlin = ' '
    else
        cmdlin = ' '
        do i = 1, min(iar,maxarg)
            call get_command_argument(i,lbuf)
            call sstrng(lbuf,60,1,first,last,valid)
            if (valid) then
                ic = last - first + 1
                lpoint = lpoint + 1
                cmdlin(lpoint:lpoint+ic) = lbuf(first:last)
                lpoint = lpoint + ic
            end if
        end do
    end if
    return
    end subroutine getcl

    ! --------------------------- cptime -------------------------------------------

    subroutine cptime (cputim)

    ! calculate amount of computer time (cputim) in seconds used so far
    ! arguments: cputim (output) - elapsed cpu time

    real(eb), intent(out) :: cputim

    call CPU_TIME(cputim)
    return
    end subroutine cptime

    ! --------------------------- mat2mult -------------------------------------------

    subroutine mat2mult(mat1,mat2,idim,n)

    ! given an nxn matrix mat1 whose elements are either 0 or 1, this routine computes the matrix
    ! mat1**2 and returns the results in mat1 (after scaling non-zero entries to 1).
    ! arguments: mat1 - matrix
    !            mat2 - work array of same size as mat1
    !            idim - actual dimensino limit on first subscript of mat1
    !            n - size of matrix

    integer, intent(in) :: idim, n
    integer, intent(inout) :: mat1(idim,n)
    integer, intent(out) :: mat2(idim,n)

    integer :: i, j, k

    do i = 1, n
        do j = 1, n
            mat2(i,j) = 0
            do k = 1, n
                mat2(i,j) = mat2(i,j)+mat1(i,k)*mat1(k,j)
            end do
            if (mat2(i,j)>=1) mat2(i,j) = 1
        end do
    end do
    do i = 1, n
        do j = 1, n
            mat1(i,j) = mat2(i,j)
        end do
    end do
    return
    end subroutine mat2mult

    ! --------------------------- indexi -------------------------------------------

    subroutine indexi (n,arrin,indx)

    ! sorts the array arrin passively via the permuation array indx. the elements arrin(indx(i)), i=1, ..., n 
    ! are in increasing order. this routine uses a bubble sort.  it should not be used for large n (n>30), 
    ! since bubble sorts are not efficient.
    ! arguments: n     number of elements in arrin
    !            arrin array to be passively sorted
    !            indx  permuation vector containing ordering such that arrin(indx) is in increasing order.

    integer, intent(in) :: n, arrin(*)
    integer, intent(out) :: indx(*)

    integer ai, aip1, i, iswitch, itemp

    do i = 1, n
        indx(i) = i
    end do
5   continue
    iswitch = 0
    do i = 1, n-1, 2
        ai = arrin(indx(i))
        aip1 = arrin(indx(i+1))
        if (ai<=aip1) cycle
        iswitch = 1
        itemp = indx(i)
        indx(i) = indx(i+1)
        indx(i+1) = itemp
    end do
    do  i = 2, n-1, 2
        ai = arrin(indx(i))
        aip1 = arrin(indx(i+1))
        if (ai<=aip1) cycle
        iswitch = 1
        itemp = indx(i)
        indx(i) = indx(i+1)
        indx(i+1) = itemp
    end do
    if (iswitch==1) go to 5
    return
    end subroutine indexi

    ! --------------------------- interp -------------------------------------------

    subroutine interp (x,y,n,t,icode,yint)

    ! interpolates a table of numbers found in the arrays, x and y.
    ! arguments: x,y - arrays of size n to be interpolated at x=t
    !            icode - code to select how to extrapolate values if t is less than x(1) or greater than x(n).
    !                      if icode = 1 then yint = y(1) for t < x(1) and yint = y(n) for t > x(n).
    !                      if icode = 2 then yint is evaluated by interpolation if x(1) < t < x(n)
    !                          and by extrapolation if t < x(1) or    t > x(n)
    !            yint (output) - interpolated value of the y array at t

    real(eb), intent(in) :: x(*), y(*), t
    integer, intent(in) :: n, icode

    real(eb) :: yint

    integer :: ilast, imid, ia, iz
    real(eb) :: dydx


    save
    data ilast /1/
    if (n==1) then
        yint = y(1)
        return
    end if
    if (t<=x(1)) then
        if (icode==1) then
            yint = y(1)
            return
        else
            imid = 1
            go to 20
        end if
    end if
    if (t>=x(n)) then
        if (icode==1) then
            yint = y(n)
            return
        else
            imid = n - 1
            go to 20
        end if
    end if
    if (ilast+1<=n) then
        imid = ilast
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    end if
    if (ilast+2<=n) then
        imid = ilast + 1
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    end if
    ia = 1
    iz = n - 1
10  continue
    imid = (ia+iz)/2
    if (t<x(imid)) then
        iz = imid - 1
        go to 10
    end if
    if (t>=x(imid+1)) then
        ia = imid + 1
        go to 10
    end if
20  continue
    dydx = (y(imid+1)-y(imid))/(x(imid+1)-x(imid))
    yint = y(imid) + dydx*(t-x(imid))
    ilast = imid
    return

    end subroutine interp

    ! --------------------------- cmdflag -------------------------------------------

    integer function cmdflag (ic,iopt)

    character(len=1), intent(in) :: ic
    integer, intent(in) :: iopt(26)

    cmdflag = iopt(ichar(ic)-ichar('A')+1)

    end function cmdflag

    ! --------------------------- read_command_options -------------------------------------------

    subroutine read_command_options

    ! retrieve and process command line options and date

    ! unit numbers defined in read_command_options, open_output_files, read_input_file

    ! options
    !     a = run cdata accumulator on the specified input file
    !     d to turn on debugging writes
    !     f/c = printed output options, full/compact. default is full
    !	  i = do initialization only
    !     k = do not access keyboard
    !     n = output just target fluxes relative to ambient (like -v but smoke still in od)
    !     o = output "solver.ini" options into the file solve.ini
    !     p = run cdata preprocessor on the specified input file
    !     s = run cdata statistics on the specified input file
    !     v = output target fluxes relative to an ambient target (incident flux - sigma*eps*tamb**4) and smoke in mg/m^3

    integer :: year, month, day, iarg(8), iopt(26), nargs, values(8), i
    character(len=60) :: strs(8)
    character(len=10) :: big_ben(3)
    character(len=26) :: ssselected

    ! current date
    call date_and_time(big_ben(1),big_ben(2),big_ben(3),values)
    year=values(1)
    month=values(2)
    day = values(3)
    rundat(3) = day
    rundat(2) = month
    rundat(1) = year

    ! command line arguments
    nargs = 8
    call cmdline(nargs,strs,iarg,iopt)

    if (cmdflag('A',iopt)/=0) cdata_accumulator = .true.
    if (cmdflag('K',iopt)/=0) nokbd = .true.
    if (cmdflag('I',iopt)/=0) initializeonly = .true.
    if (cmdflag('D',iopt)/=0) debugging = .true.
    if (cmdflag('V',iopt)/=0) validation_flag = .true.
    if (cmdflag('N',iopt)/=0) netheatflux = .true.
    if (cmdflag('O',iopt)/=0) then
        ssoutoptions = 0
        ssselected = strs(cmdflag('O',iopt))
        do i = 1,len(trim(ssselected))
            if (ssselected(i:i)>='A'.and.ssselected(i:i)<='Z') then
                ssoutoptions(ichar(ssselected(i:i))-ichar('A')+1) = i
            else if (ssselected(i:i)>='a'.and.ssselected(i:i)<='z') then
                ssoutoptions(ichar(ssselected(i:i))-ichar('a')+1) = i
            end if
        end do
    end if
    if (cmdflag('P',iopt)/=0) cdata_preprocessor = .true.
    if (cmdflag('S',iopt)/=0) cdata_statistics = .true.

    if (cmdflag('F',iopt)/=0.and.cmdflag('C',iopt)/=0) then
        write (errormessage,*) 'Both compact (/c) and full (/f) output specified. Only one may be included on command line.'
        call cfastexit('read_command_options',1)
        stop
    end if
    if (cmdflag('C',iopt)/=0) outputformat = 1
    if (cmdflag('F',iopt)/=0) outputformat = 2

    return

    end   subroutine read_command_options

    ! --------------------------- shellsort -------------------------------------------

    subroutine shellsort (ra, n)

    integer, intent(in) :: n
    real(eb), intent(inout) :: ra(n)

    integer j, i, inc
    real(eb) rra

    inc = 1
1   inc = 3*inc+1
    if (inc<=n) go to 1
2   continue
    inc = inc/3
    do i = inc+1, n
        rra = ra(i)
        j = i
3       if (ra(j-inc)>rra) then
            ra(j) = ra(j-inc)
            j = j - inc
            if (j<=inc) go to 4
            go to 3
        end if
4       ra(j) = rra
    enddo
    if (inc>1) go to 2
    return
    end  subroutine shellsort

    ! --------------------------- sstrng -------------------------------------------

    subroutine sstrng (string,wcount,sstart,sfirst,slast,svalid)

    ! finds positions of substrings within a character string.  a space, comma, - , (, or )
    !          indicates the beginning or end of a substring. when called, the string is passed as an integer(choose)
    !          along with the number of characters in the string(wcount) and a starting position(sstart).  beginning at
    !          "sstart", the routine searches for a substring. if a substring is found, its first and last character
    !          positions are returned along with a true value in "svalid"; otherwise "svalid" is set false.
    ! arguments: string - the character string
    !            wcount - number of characters in the string
    !            sstart - beginning position in string to look for a substring
    !            sfirst (output) - beginning position of the substring
    !            slast - ending position of the substring
    !            svalid - true if a valid substring is found

    integer, intent(in) :: sstart, wcount
    character(len=1), intent(in) :: string(*)
    logical, intent(out) :: svalid

    integer, intent(out) :: sfirst, slast

    integer :: endstr, i, j
    character(len=1) :: space = ' ', comma = ','

    svalid = .true.

    ! invalid starting position - past end of string
    endstr = sstart + wcount - 1

    ! find position of first element of substring
    do i = sstart, endstr

        ! move to the beginning of the substring
        sfirst = i
        if ((string(i)/=space).and.(string(i)/=comma)) goto 60
    end do

    ! no substring found - only delimiter
    go to 40

    ! find position of last character of substring
60  do j = sfirst, endstr

        ! position of last element of substring
        slast = j-1
        if ((string(j)==space).or.(string(j)==comma)) go to 100
    end do

    ! no substring delimiter => last character of substring is the last character of the string
    slast = endstr
    return

    ! no substring found
40  svalid = .false.
100 return
    end subroutine sstrng

    ! ------------------ fmix ------------------------

    real(fb) function fmix (f,a,b)

    real(fb), intent(in) :: f, a, b

    fmix = (1.0_fb-f)*a + f*b

    return

    end function fmix

    ! ------------------ emix ------------------------

    real(eb) function emix (f,a,b)

    real(eb), intent(in) :: f, a, b

    emix = (1.0_eb-f)*a + f*b

    return

    end function emix
    
    

    ! --------------------------- readcsvformat -------------------------------------------

    subroutine readcsvformat (iunit, x, c, numr, numc, nstart, nend, maxrow, maxcol, lend)

    !     routine:   readcsvformat
    !     purpose:   reads a comma-delimited file as generated by Micorsoft Excel, assuming that all
    !                the data is in the form of real numbers
    !     arguments: iunit    = logical unit, already open to .csv file
    !                x        = array of dimension (numr,numc) for values in spreadsheet
    !                c        = character array of same dimenaion as x for character values in spreadsheet
    !                numr     = # of rows of array x
    !                numc     = # of columns of array x
    !                nstart   = starting row of spreadsheet to read
    !                nend     = stopping row, nend < 0 means read to the end 
    !                maxrow   = actual number of rows read
    !                maxcol   = actual number of columns read

    integer, intent(in) :: iunit, numr, numc, nstart, nend

    integer, intent(out) :: maxrow, maxcol
    real(eb), intent(out) :: x(numr,numc)
    character(len=*), intent(out) :: c(numr,numc)
    logical, intent(out) :: lend

    character(len=64500) :: in*64500                ! 500 cells (for species with 10 rooms) times 129 characters 
    character(len=128) :: token*128                 !      (128 characters per token plus 1 for comma)
    integer :: i, j, nrcurrent, ic, icomma, ios, nc, lastrow

    maxrow = 0
    maxcol = 0
    lend = .false.
    do i=1,numr
        do j=1,numc
            x(i,j) = 0.0_eb
            c(i,j) = ' '
        end do
    end do
    if (nend >= nstart) then
        lastrow = min(nend, numr)
    else if (nend < 0 ) then 
        lastrow = numr
    else 
        write(errormessage,*) '***Error, ending row is less than starting row but >= 0 in spreadsheet read.'
        call cfastexit('readcsvformat',1)
    end if

    ! if we have header rows, then skip them
    if (nstart>1) then
        do  i=1,nstart-1
            read (iunit,'(A)', end = 100, iostat=ios) in
            if (ios /= 0) then
                write (errormessage,'(a)') '***Error, I/O error reading spreadsheet file.'
                call cfastexit('readcsvformat',2)
            end if
        end do
    end if

    ! read the data
    nrcurrent = 0
20  read (iunit,'(A)',end=100) in

    ! Skip comments and blank lines
    if (in(1:1)=='!'.or.in(1:1)=='#'.or.in==' ') then
        go to 20
    end if

    nrcurrent = nrcurrent+1
    maxrow = max(maxrow,nrcurrent)

    ! Cannot exceed work array
    if (maxrow>numr) then
        write (errormessage,'(a,i0,1x,i0)') '***Error, Too many rows or columns in spreadsheet file, r,c = ', maxrow, maxcol
        call cfastexit('sreadcsvformat',3)
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
        else
            write (errormessage,'(a,i0,a,i0)') 'Too many rows or columns in spreadsheet file, r,c=', nrcurrent, ' ', nc
            call cfastexit('readcsvformat',4)
        end if
        go to 30
    end if
    nc = nc + 1
    maxcol=max(maxcol,nc)
    token = in(ic:ic+128)
    c(nrcurrent,nc) = token
    read (token,'(f128.0)',iostat=ios) x(nrcurrent,nc)
    if (ios/=0) x(nrcurrent,nc) = 0
    if (nrcurrent<lastrow) then
        go to 20
    end if 
    
    return
    
100 continue
    lend = .true. 

    return
    end subroutine readcsvformat

    end module utility_routines

    module opening_fractions

    ! implement the simple open/close function for vents.
    ! This is done with a simple, linear interpolation.
    ! The opening arrays are built into the vent data structures and are of the form
    !		(1) Is start of time to change
    !		(2) Is the initial fraction (set in HVENT, VVENT and MVENT)
    !		(3) Is the time to complete the change, Time+Decay_time, and
    !		(4) Is the final fraction

    ! The open/close function is done in the physical/mode interface, wall_flow, vertical_flow and mechanical_flow
    
    use precision_parameters
    
    use cfast_types, only: ramp_type, target_type, vent_type

    use cparams, only: trigger_by_time, trigger_by_temp, trigger_by_flux, idx_tempf_trg
    
    use devc_data, only: targetinfo
    use ramp_data, only: n_ramps, rampinfo
    use vent_data, only: hventinfo, vventinfo, mventinfo
    use room_data, only: roominfo, n_rooms
    use setup_data, only: iofilo, iofill
    use namelist_data

    implicit none

    private

    public get_vent_opening, find_vent_opening_ramp

    contains

    ! --------------------------- find_vent_opening_ramp ------------------------------

    integer function find_vent_opening_ramp (rampid,venttype,room1,room2,counter)

    character(len=64), intent(in) :: rampid
    character(len=1), intent(in) :: venttype
    integer, intent(in) :: room1, room2, counter

    integer iramp, vent_index
    type(ramp_type), pointer :: rampptr

    if (n_ramps>0) then
        ! first see if ramp is named
        do iramp = 1, n_ramps
            rampptr=>rampinfo(iramp)
            if (rampid/='NULL') then
                if (rampptr%id==trim(rampid)) then
                    vent_index = iramp
                    find_vent_opening_ramp = iramp
                    return
                end if
            end if
        end do
        
        ! if ramp is not named, check for specific vent
        do iramp = 1, n_ramps
            rampptr=>rampinfo(iramp)
            if (rampptr%type==venttype.and.rampptr%room1==room1.and.rampptr%room2==room2.and. &
                rampptr%counter==counter) then
            vent_index = iramp
            find_vent_opening_ramp = iramp
            return
            end if
        end do
    end if
    find_vent_opening_ramp = 0
    return

    end function find_vent_opening_ramp

    ! --------------------------- get_vent_opening-------------------------------------

    subroutine get_vent_opening (ventptr,time,fraction)

    type(vent_type), pointer, intent(in) :: ventptr
    real(eb), intent(in) :: time
    real(eb), intent(out) :: fraction

    integer :: i
    real(eb), parameter :: mintime = 1.0e-6_eb
    real(eb) :: dt, dtfull, dy, dydt
    character(len=128) room1c, room2c, vtypec
    
    type(target_type), pointer :: targptr

    fraction = 1.0_eb
    ! check vent triggering by time
    if (ventptr%opening_type==trigger_by_time) then
        if (ventptr%npoints>0) then
            if (time<=ventptr%t(1)) then
                fraction = ventptr%f(1)
                return
            else if (time>ventptr%t(ventptr%npoints)) then
                fraction = ventptr%f(ventptr%npoints)
                return
            else
                do i=2,ventptr%npoints
                    if (time>ventptr%t(i-1).and.time<=ventptr%t(i)) then
                        dt = max(ventptr%t(i)-ventptr%t(i-1),mintime)
                        dtfull = max(time-ventptr%t(i-1),mintime)
                        dy = ventptr%f(i)-ventptr%f(i-1)
                        dydt = dy / dt
                        fraction = ventptr%f(i-1) + dydt*dtfull
                        return
                    end if
                end do
            end if
        end if
        ! check vent triggering by temperature. if tripped, turn it into a time-based change
    else if (ventptr%opening_type==trigger_by_temp.and..not.ventptr%opening_triggered) then
        targptr => targetinfo(ventptr%opening_target)
        if (targptr%temperature(idx_tempf_trg)>ventptr%opening_criterion) then
            ventptr%t(1) = time
            ventptr%t(2) = time + 1.0_eb
            ventptr%opening_type = trigger_by_time
            ventptr%opening_triggered = .true.
            room1c = roominfo(ventptr%room1)%id
            if (ventptr%room1>n_rooms) room1c = 'Outside'
            room2c = roominfo(ventptr%room2)%id
            if (ventptr%room2>n_rooms) room2c = 'Outside'
            vtypec = 'Unknown '
            if (ventptr%vtype=='H') vtypec = 'Wall'
            if (ventptr%vtype=='V') vtypec = 'Ceiling/Floor'
            if (ventptr%vtype=='M') vtypec = 'Mechanical'
            write (iofilo,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by temperature at ',time,' s'
            write (iofill,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by temperature at ',time,' s'
        end if
        ! check vent triggering by flux. if tripped, turn it into a time-based change
    else if (ventptr%opening_type==trigger_by_flux.and..not.ventptr%opening_triggered) then
        targptr => targetinfo(ventptr%opening_target)
        if (targptr%flux_incident_front>ventptr%opening_criterion) then
            ventptr%t(1) = time
            ventptr%t(2) = time + 1.0_eb
            ventptr%opening_type = trigger_by_time
            ventptr%opening_triggered = .true.
            room1c = roominfo(ventptr%room1)%id
            if (ventptr%room1>n_rooms) room1c = 'Outside'
            room2c = roominfo(ventptr%room2)%id
            if (ventptr%room2>n_rooms) room2c = 'Outside'
            vtypec = 'Unknown '
            if (ventptr%vtype=='H') vtypec = 'Wall'
            if (ventptr%vtype=='V') vtypec = 'Ceiling/Floor'
            if (ventptr%vtype=='M') vtypec = 'Mechanical'
            write (iofilo,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by heat flux at ',time,' s'
            write (iofill,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by heat flux at ',time,' s'
        end if
    else
    end if

    ! This is for backwards compatibility with the older EVENT format for single vent changes!
!    fraction = 1.0_eb
!    if (venttype=="H") then
!        ventptr => hventinfo(vent_index)
!        fraction = vfraction(venttype,ventptr, time)
!    else if (venttype=="V") then
!        ventptr => vventinfo(vent_index)
!        fraction = vfraction(venttype,ventptr, time)
!    else if (venttype=="M") then
!        ventptr => mventinfo(vent_index)
!        fraction = vfraction(venttype,ventptr, time)
!    else if (venttype=="F") then
!        ventptr => mventinfo(vent_index)
!        fraction = vfraction(venttype,ventptr, time)
!    end if
    return

    end subroutine get_vent_opening

    ! --------------------------- vfraction -------------------------------------------

    real(eb) function vfraction (vtype, ventptr, time)

    !	This is the open/close function for vent flow

    type(vent_type) :: ventptr
    type(target_type), pointer :: targptr
    real(eb), intent(in) :: time
    character(len=1), intent(in) :: vtype
    character(len=128) room1c, room2c, vtypec

    real(eb) :: dt, dy, dydt, mintime = 1.0e-6_eb
    real(eb) :: deltat

    if (vtype=="F") then
        if (time<ventptr%filter_initial_time) then
            vfraction = ventptr%filter_initial_fraction
        else if (time>ventptr%filter_final_time) then
            vfraction = ventptr%filter_final_fraction
        else
            dt = max(ventptr%filter_final_time - ventptr%filter_initial_time, mintime)
            deltat = max(time - ventptr%filter_initial_time, mintime)
            dy = ventptr%filter_final_fraction - ventptr%filter_initial_fraction
            dydt = dy/dt
            vfraction = ventptr%filter_initial_fraction + dydt*deltat
        end if
    else
        vfraction = ventptr%opening_initial_fraction
        ! check normal vent triggering by time
        if (ventptr%opening_type==trigger_by_time) then
            if (time<ventptr%opening_initial_time) then
                vfraction = ventptr%opening_initial_fraction
            else if (time>ventptr%opening_final_time) then
                vfraction = ventptr%opening_final_fraction
            else
                dt = max(ventptr%opening_final_time - ventptr%opening_initial_time, mintime)
                deltat = max(time - ventptr%opening_initial_time, mintime)
                dy = ventptr%opening_final_fraction - ventptr%opening_initial_fraction
                dydt = dy/dt
                vfraction = ventptr%opening_initial_fraction + dydt*deltat
            end if
            ! check vent triggering by temperature. if tripped, turn it into a time-based change
        else if (ventptr%opening_type==trigger_by_temp.and..not.ventptr%opening_triggered) then
            targptr => targetinfo(ventptr%opening_target)
            if (targptr%temperature(idx_tempf_trg)>ventptr%opening_criterion) then
                ventptr%opening_initial_time = time
                ventptr%opening_final_time = time + 1.0_eb
                ventptr%opening_type = trigger_by_time
                ventptr%opening_triggered = .true.
                room1c = roominfo(ventptr%room1)%id
                if (ventptr%room1>n_rooms) room1c = 'Outside'
                room2c = roominfo(ventptr%room2)%id
                if (ventptr%room2>n_rooms) room2c = 'Outside'
                vtypec = 'Unknown '
                if (ventptr%vtype=='H') vtypec = 'Wall'
                if (ventptr%vtype=='V') vtypec = 'Ceiling/Floor'
                if (ventptr%vtype=='M') vtypec = 'Mechanical'
                write (iofilo,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                    ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                    '), opening change triggered by temperature at ',time,' s'
                write (iofill,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                    ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                    '), opening change triggered by temperature at ',time,' s'
            end if
            ! check vent triggering by flux. if tripped, turn it into a time-based change
        else if (ventptr%opening_type==trigger_by_flux.and..not.ventptr%opening_triggered) then
            targptr => targetinfo(ventptr%opening_target)
            if (targptr%flux_incident_front>ventptr%opening_criterion) then
                ventptr%opening_initial_time = time
                ventptr%opening_final_time = time + 1.0_eb
                ventptr%opening_type = trigger_by_time
                ventptr%opening_triggered = .true.
                room1c = roominfo(ventptr%room1)%id
                if (ventptr%room1>n_rooms) room1c = 'Outside'
                room2c = roominfo(ventptr%room2)%id
                if (ventptr%room2>n_rooms) room2c = 'Outside'
                vtypec = 'Unknown '
                if (vtype=='H') vtypec = 'Wall'
                if (vtype=='V') vtypec = 'Ceiling/Floor'
                if (vtype=='M') vtypec = 'Mechanical'
                write (iofilo,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                    ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                    '), opening change triggered by heat flux at ',time,' s'
                write (iofill,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                    ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                    '), opening change triggered by heat flux at ',time,' s'
            end if
        end if
    end if
    return

    end function vfraction

    end module opening_fractions
