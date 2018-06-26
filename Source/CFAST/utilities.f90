    module utility_routines

    use precision_parameters

    use cparams
    use setup_data
    use option_data
    use solver_data
    use room_data, only: nwpts, wsplit, iwbound

    use namelist_data

    implicit none

    character(lbufln) :: lbuf

    ! unlike most other routines, this one does not have the private specifier since all routines here are intended to be
    ! used by other routines

    contains
    

    ! --------------------------- set_heat_of_combustion -------------------------------------------

    subroutine set_heat_of_combustion (maxint, mdot, qdot, hdot, hinitial)

    !	Routine to implement the algorithm to set the heat of combustion for all fires

    integer, intent(in) :: maxint
    real(eb), intent(in) :: qdot(maxint), hinitial
    real(eb), intent(out) :: mdot(maxint), hdot(maxint)

    integer :: i
    real(eb) :: hcmax = 1.0e8_eb, hcmin = 1.0e6_eb

    do i = 1, maxint
        if (i>1) then
            if (mdot(i)*qdot(i)<=0.0_eb) then
                hdot(i) = hinitial
            else
                hdot(i) = min(hcmax,max(qdot(i)/mdot(i),hcmin))
                mdot(i) = qdot(i)/hdot(i)
            end if
        else
            hdot(1) = hinitial
        end if
    end do

    return

    end subroutine set_heat_of_combustion



    ! --------------------------- position_object -------------------------------------------

    subroutine position_object (xyorz,pos_max,defaultposition,minimumseparation)

    !     Position an object in a compartment
    !     arguments: xyorz: object position in the x, y, or z direction
    !		         pos_max: the maximum extent
    !		         defaultposition: to set to zero (base)(2) or midpoint(1)
    !		         minimumseparation: the closest the object can be to a wall

    integer, intent(in) :: defaultposition
    real(eb), intent(in) :: minimumseparation, pos_max
    real(eb), intent(inout) :: xyorz

    if ((xyorz<0.0_eb).or.(xyorz>pos_max)) then
        select case (defaultposition)
        case (1)
            xyorz = pos_max / 2.0_eb
        case (2)
            xyorz = minimumseparation
            case default
            write (*,*) 'Fire objects positioned specified outside compartment bounds.'
            write (iofill,*) 'Fire objects positioned specified outside compartment bounds.'
            stop
        end select
    else if (xyorz==0.0_eb) then
        xyorz = minimumseparation
    else if (xyorz==pos_max) then
        xyorz = pos_max - minimumseparation
    end if

    return

    end subroutine position_object

    ! --------------------------- ssaddtolist -------------------------------------------

    subroutine ssaddtolist (ic, valu, array)

    real(eb), intent(in) :: valu
    real(eb), intent(out) :: array(*)
    integer, intent(inout) :: ic

    ic = ic + 1
    ! We are imposing an arbitrary limit of 32000 columns
    if (ic>32000) return
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

    character :: string*256

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

    !     routine: tanhsmooth
    !     purpose: calculate a smooth transition from 1 (at xmax) to zero (at xmin)
    !     arguments: x    current value
    !                xmax maximum value of independent variable. Return ymax above this value
    !                xmin minimum value of independent variable. Return ymin below this value
    !                ymax value returned at or above xmax
    !                ymin value return at or below xmin

    real(eb), intent(in) :: x, xmax, xmin, ymax, ymin
    real(eb) :: f
    f = min(max(0.5_eb + tanh(8.0_eb/(xmax-xmin)*(x-xmin)-4.0_eb)/2.0_eb,0.0_eb),1.0_eb)
    if (f<1.0d-6) f = 0.0_eb
    tanhsmooth = f*(ymax-ymin)+ymin


    return
    end function tanhsmooth

    ! --------------------------- xerror -------------------------------------------

    subroutine xerror(messg,nmessg,nerr,level)

    !     routine: xerror
    !     purpose: XERROR processes a diagnostic message. It is a stub routine written for the book above.
    !              Actually, XERROR is a sophisticated error handling package with many options. Our version
    !              has the same calling sequence but only prints an error message and either returns (if
    !              the input value of ABS(LEVEL) is less than 2) or stops (if the input value of ABS(LEVEL) equals 2).
    !     Revision: $Revision$
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: MESSG - the Hollerith message to be processed.
    !                NMESSG- the actual number of characters in MESSG. (this is ignored in this stub routine)
    !                NERR  - the error number associated with this message. NERR must not be zero. (this is
    !                        ignored in this stub routine)
    !                LEVEL - error category.
    !                       = 2 means this is an unconditionally fatal error.
    !                       = 1 means this is a recoverable error.  (I.e., it is non-fatal if XSETF has been appropriately called.)
    !                       = 0 means this is a warning message only.
    !                       =- 1 means this is a warning message which is to be printed at most once, regardless of how many times
    !                          this call is executed. (in this stub routine, LEVEL=2 causes a message to be printed and then
    !                          a stop. LEVEL=-1,0,1 causes a message to  be printed and then a return.

    character(*), intent(in) :: messg
    integer, intent(in) :: nmessg, level, nerr

    character(32) :: error_label

    integer :: nmess

    if (level==2) then
        error_label = "***Fatal error:"
    else if (level==1) then
        error_label = "***Error:"
    else
        error_label = "***Warning:"
    end if

    if (nmessg==0) then
        nmess = len_trim(messg)
    else
        nmess = nmessg
    end if
    nmess = max(1,nmess)

    if (level==-2) then
    else if (level==-3) then
        call xerrwv(messg,nmess,nerr,level,0,0,0,0,0.0_eb,0.0_eb)
    else
        call xerrwv(trim(error_label)//" "//messg,nmess+len_trim(error_label)+1,nerr,level,0,0,0,0,0.0_eb,0.0_eb)
    end if

    return
    end subroutine xerror

    ! --------------------------- xerrwv -------------------------------------------

    subroutine xerrwv(msg,nmes,nerr,level,ni,i1,i2,nnr,r1,r2)

    !     routine: xerrwv
    !     purpose: xerrwv, as given here, constitutes a simplified version of the slatec error handling package. It just prints
    !              our error messages with codes as requested
    !     arguments: msg - the message (character array).
    !                nmes - the length of msg (number of characters).
    !                nerr - the error number (not used).
    !                level - the error level. 0 or 1 means recoverable (control returns to caller). 2 means fatal (run is aborted).
    !                ni - number of integers (0, 1, or 2) to be printed with message.
    !                i1,i2 - integers to be printed, depending on ni.
    !                nnr - number of reals (0, 1, or 2) to be printed with message.
    !                r1,r2 - reals to be printed, depending on nnr.

    integer, intent(in) :: nmes, nerr, level, ni, i1, i2, nnr
    real(eb), intent(in) :: r1, r2
    character, intent(in) :: msg(nmes)*1

    integer :: i, lunit, mesflg

    ! define message print flag and logical unit number
    mesflg = 1
    lunit = iofill
    if (mesflg/=0) then

        ! write the message
        write (lunit,5000) (msg(i),i = 1,nmes)
        if (ni==1) write (lunit,5010) nerr,i1
        if (ni==2) write (lunit,5020) nerr,i1, i2
        if (nnr==1) write (lunit,5030) nerr,r1
        if (nnr==2) write (lunit,5040) nerr,r1, r2
    end if

    ! abort the run if level = 2
    if (level/=2) return
    stop

5000 format (80a1)
5010 format ('nerr, i1 =',2i10)
5020 format ('nerr,i1,i2 =',3i10)
5030 format ('nerr,r1 =',d21.13)
5040 format ('nerr,r1,r2 =',i10,2e21.13)

    end subroutine xerrwv

    ! --------------------------- d1mach -------------------------------------------

    real(eb) function d1mach (i)

    !     routine: d1mach
    !     purpose: d1mach can be used to obtain machine-dependent parameters for the local machine environment.
    !              it is a function subprogram with one (input) argument. reference  p. a. fox, a. d. hall and
    !              nr. l. schryer, framework for a portable library, acm transactions on mathematical software 4,
    !              2 (june 1978), pp. 177-188.
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
        write (*, '(''1error    1 in d1mach - i out of bounds'')')
        stop
    end select
    return
    end function d1mach

    ! --------------------------- xerrormod -------------------------------------------

    subroutine xerrmod(mesg,nerr,nnr,r1,r2)

    !     routine: xerrmod
    !     purpose: xerrmod is a simplified version of the slatec error handling package. it just logs our error messages
    !              with codes as requested. adapted from code written by a. c. hindmarsh and p. nr. brown at llnl.
    !     arguments: msg - the message (character array).
    !                nmes - the length of msg (number of characters).
    !                nerr - the error number (not used).
    !                nnr - number of reals (0, 1, or 2) to be printed with message.
    !                r1,r2 - reals to be printed, depending on nnr.

    integer, intent(in) :: nerr, nnr
    real(eb), intent(in) :: r1, r2
    character, intent(in) :: mesg*(*)

    integer :: lm

    lm = len_trim(mesg)

    ! write the message
    write (*,5000) mesg(1:lm)
    write (iofill,5000) mesg(1:lm)
    if (nnr==1) then
        write (*,5001) nerr,r1
        write (iofill,5001) nerr,r1
    else if (nnr==2) then
        write (*,5002) nerr,r1,r2
        write (iofill,5002) nerr,r1,r2
    end if
    return

5000 format(a)
5001 format('ierror,r1 =',i5,2d14.4)
5002 format('ierror,r1,r2 =',i5,2d14.4)
    end subroutine xerrmod

    ! --------------------------- cmdline -------------------------------------------

    subroutine cmdline (nargs,strs,iarg,iopt)

    !     routine: cmdline
    !     purpose: gets argument list and options from command line. options may be of the form c:<string> where <c>
    !              is the desired option a-z and <string> is a character string associated with that option.
    !     arguments: nargs maximum number of arguments expected (dimension limit on input, actual number on output.
    !                strs  returned strings of arguments and options
    !                iarg  returned list of pointers to elements in strs corresponding to arguments 1..nargs
    !                iopt  returned list of pointers to elements in strs corresponding to options a-z

    integer, intent(inout) :: nargs
    integer, intent(out) :: iarg(nargs), iopt(26)
    character, intent(out) :: strs(nargs)*(*)

    integer :: ic, ia, i
    character :: cmdlin*127, optsep

    optsep = '-'

    do ic = 1, max0(nargs,26)
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

    !     routine: cmove
    !     purpose: move a substring in the command line to remove spaces.
    !     arguments: cmdlin command line string
    !                i1     beginning of substring to be moved
    !                i2     end of substring to be moved
    !                i3     beginning of destination
    !                i4     end of destination
    !                i5     position of newly vacated space in the string
    !                chr    character to fill that space

    character, intent(in) :: chr
    integer, intent(in) :: i1, i2, i3, i4, i5

    character, intent(inout) :: cmdlin*(*)

    character :: temp*127

    temp = cmdlin
    temp(i1:i2) = cmdlin(i3:i4)
    temp(i5:i5) = chr
    cmdlin = temp
    return
    end subroutine cmove

    ! --------------------------- getcl -------------------------------------------

    subroutine getcl (cmdlin)

    !     routine: getcl
    !     purpose: get command line as a single string
    !     arguments: cmdlin - command line

    character, intent(out) :: cmdlin*127

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

    ! --------------------------- countargs -------------------------------------------

    integer function countargs (lcarray)

    !     routine: countargs
    !     purpose: Count the number of non-blank arguments on the input line.
    !     arguments: lcarray - character array of arguments.  There should be tocount non-blank entries
    !                numc - dimension limit on lcarray

    character(128), intent(in) :: lcarray(ncol)

    integer :: i, nret

    nret = 0

    ! check for the expected number of arguments if tocount >=0
    do i = 1, ncol
        if (lcarray(i)==' ') then
            countargs = nret
            return
        end if
        nret = nret + 1
    end do
    countargs = ncol
    return

    end function countargs

    ! --------------------------- cptime -------------------------------------------

    subroutine cptime (cputim)

    !     routine: cptime
    !     purpose: routine to calculate amount of computer time (cputim) in seconds used so far.
    !              this routine will generally be different for each computer.
    !     arguments: cputim (output) - elapsed cpu time

    real(eb), intent(out) :: cputim

    call CPU_TIME(cputim)
    return
    end subroutine cptime

    ! --------------------------- doesthefileexist -------------------------------------------

    logical function doesthefileexist (checkfile)

    !     routine: doesthefileexist
    !     purpose: checks for the existence of given file name
    !     arguments: checkfile - file name

    character(*), intent(in) :: checkfile
    logical yesorno


    inquire (file=checkfile, exist=yesorno)
    if (yesorno) then
        doesthefileexist = .true.
    else
        doesthefileexist = .false.
    end if

    return

    end function doesthefileexist

    ! --------------------------- exehandle -------------------------------------------

    subroutine exehandle (exepath, datapath, project, extension)

    ! get the arguments used to call the main program
    ! arguments: exepath - path (without the name) to the folder where the executable resides
    !                datapath - path (without a file name) to the folder where the project data file resides
    !				 project - name of the project - this name cannot exceed 64 charcters. the total lenght of
    !                          datapath + project cannot exceed 256 characters

    character(*), intent(out) :: exepath, datapath, project, extension

    integer :: i, loop, status, nargs, ld(2), li(2), ln(2), le(2), lb
    character(256) :: buf, xname
    character (64) :: name(2)

    character(3) :: drive(2)
    character(256) :: dir(2)
    character(64) :: ext(2)
    integer(4) :: length, pathcount, splitpathqq, ilen

    nargs = command_argument_count() + 1
    project = ' '
    exepath = ' '
    datapath = ' '

    if (nargs<2) then
        write (*,*) 'CFAST was called with no arguments on the command line.  At least an input file is required.'
        stop
    end if

    ! get the calling program and arguments

    exepath = ' '
    datapath = ' '
    project = ' '
    extension = ' '

    ! only look at the first two arguments (1 = executable name, 2 =cfast input file name)
    do i = 1, 2
        loop = i - 1
        call get_command_argument(loop, buf, ilen, status)
        if (ilen>0) then
            xname = buf

            !	Split out the components
            drive(i) = ' '
            dir(i) = ' '
            name(i) = ' '
            ext(i) = ' '
            length = splitpathqq(xname, drive(i), dir(i), name(i), ext(i))
            ld(i) = len_trim(drive(i))
            li(i) = len_trim(dir(i))
            ln(i) = len_trim(name(i))
            le(i) = len_trim(ext(i))

            pathcount = 5 + ln(i) + li(i) +ld(i) + le(i)

            if (pathcount>255.or.ln(i)>64) then
                write (*,'(a,/,a)') 'Total file name length including path must be less than 256 characters.', &
                    'Individual filenames must be less than 64 characters.'
                stop
            end if
        end if
    end do

    ! Now check that the cfast input file exists = this is the data file
    buf = ' '
    if (le(2)/=0) then
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // ext(2)(1:le(2))
    else
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // '.in'
    end if

    ! buf(1:lb) is the data file to check
    lb = len_trim(buf)
    if (DoesTheFileExist(buf(1:lb))) then
        !	The project file exists
        exepath = drive(1)(1:ld(1)) // dir(1)(1:li(1))
        datapath = drive(2)(1:ld(2)) // dir(2)(1:li(2))
        project = name(2)(1:ln(2))
        if (le(2)/=0) then
            extension = ext(2)(1:le(2))
        else
            extension = '.in'
        end if
    else
        write (*,*) ' Input file does not exist: ', trim(buf)
        stop
    end if
    
    return

    end subroutine exehandle

    ! --------------------------- mat2mult -------------------------------------------

    subroutine mat2mult(mat1,mat2,idim,nr)

    !     routine: mat2mult
    !     purpose: given an nxn matrix mat1 whose elements are either 0 or 1, this routine computes the matrix
    !              mat1**2 and returns the results in mat1 (after scaling non-zero entries to 1).
    !     arguments: mat1 - matrix
    !                mat2 - work array of same size as mat1
    !                idim - actual dimensino limit on first subscript of mat1
    !                nr - size of matrix
    !                matiter - unused

    integer, intent(in) :: idim, nr
    integer, intent(inout) :: mat1(idim,nr)
    integer, intent(out) :: mat2(idim,nr)

    integer :: i, j, k

    do i = 1, nr
        do j = 1, nr
            mat2(i,j) = 0
            do k = 1, nr
                mat2(i,j) = mat2(i,j)+mat1(i,k)*mat1(k,j)
            end do
            if (mat2(i,j)>=1) mat2(i,j) = 1
        end do
    end do
    do i = 1, nr
        do j = 1, nr
            mat1(i,j) = mat2(i,j)
        end do
    end do
    return
    end subroutine mat2mult

    ! --------------------------- indexi -------------------------------------------

    subroutine indexi (nr,arrin,indx)

    !     routine: indexi
    !     purpose: this routines sorts the array arrin passively via the permuation array indx. the
    !              elements arrin(indx(i)), i=1, ..., nr are in increasing order. this routine uses a
    !              bubble sort.  it should not be used for large nr (nr>30), since bubble sorts are not efficient.
    !     arguments: nr     number of elements in nr
    !                arrin array to be passively sorted
    !                indx  permuation vector containing ordering such that arrin(indx) is in increasing order.

    integer, intent(in) :: nr, arrin(*)
    integer, intent(out) :: indx(*)

    integer ai, aip1, i, iswitch, itemp

    do i = 1, nr
        indx(i) = i
    end do
5   continue
    iswitch = 0
    do i = 1, nr-1, 2
        ai = arrin(indx(i))
        aip1 = arrin(indx(i+1))
        if (ai<=aip1) cycle
        iswitch = 1
        itemp = indx(i)
        indx(i) = indx(i+1)
        indx(i+1) = itemp
    end do
    do  i = 2, nr-1, 2
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

    subroutine interp (x,y,nr,t,icode,yint)

    !     routine: indexi
    !     purpose: routine interpolates a table of numbers found in the arrays, x and y.
    !     arguments: x,y - arrays of size nr to be interpolated at x=t
    !                icode - code to select how to extrapolate values if t is less than x(1) or greater than x(nr).
    !                          if icode = 1 then yint = y(1) for t < x(1) and yint = y(nr) for t > x(nr).
    !                          if icode = 2 then yint is evaluated by interpolation if x(1) < t < x(nr)
    !                              and by extrapolation if t < x(1) or    t > x(nr)
    !                yint (output) - interpolated value of the y array at t

    real(eb), intent(in) :: x(*), y(*), t
    integer, intent(in) :: nr, icode

    real(eb) :: yint

    integer :: ilast, imid, ia, iz
    real(eb) :: dydx


    save
    data ilast /1/
    if (nr==1) then
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
    if (t>=x(nr)) then
        if (icode==1) then
            yint = y(nr)
            return
        else
            imid = nr - 1
            go to 20
        end if
    end if
    if (ilast+1<=nr) then
        imid = ilast
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    end if
    if (ilast+2<=nr) then
        imid = ilast + 1
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    end if
    ia = 1
    iz = nr - 1
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

    character(1), intent(in) :: ic
    integer, intent(in) :: iopt(26)

    cmdflag = iopt(ichar(ic)-ichar('A')+1)

    end function cmdflag

    ! --------------------------- read_command_options -------------------------------------------

    subroutine read_command_options

    !     routine: read_command_options
    !     purpose:  retrieve and process command line options and date
    !     arguments: none

    ! unit numbers defined in read_command_options, openoutputfiles, readinputfiles
    !
    !      1 is for the solver.ini and data files (data file, tpp and objects) (iofili)
    !      3 is for the log file  (iofill)
    !      6 is output (iofilo)
    !     11 is the history file
    !     12 is used to write the status file (project.status)
    !     13 smokeview output
    !     14 spreadsheet output

    ! options
    !     k = do not access keyboard
    !     f/c = output options
    !     s = output "solver.ini" options into the file solve.ini
    !	  i = do initialization only
    !     d to turn on debugging writes
    !     t to output trace species mass
    !     v to output target fluxes relative to an ambient target (incident flux - sigma*eps*tamb**4) and smoke in mg/m^3
    !     nr to output just target fluxes relative to ambient (smoke still in od)

    integer :: year, month, day, iarg(8), iopt(26), nargs, values(8)
    character :: strs(8)*60
    character(10) :: big_ben(3)

    ! current date
    call date_and_time(big_ben(1),big_ben(2),big_ben(3),values)
    year=values(1)
    month=values(2)
    day = values(3)
    rundat(3) = day
    rundat(2) = month
    rundat(1) = year
    write (mpsdatc,5010) rundat(1), rundat(2), rundat(3)

    ! command line arguments
    nargs = 8
    call cmdline(nargs,strs,iarg,iopt)

    if (cmdflag('K',iopt)/=0) nokbd = .true.
    if (cmdflag('I',iopt)/=0) initializeonly = .true.
    if (cmdflag('D',iopt)/=0) debugging = .true.
    if (cmdflag('V',iopt)/=0) validation_flag = .true.
    if (cmdflag('N',iopt)/=0) netheatflux = .true.
    iofill = 3

    if (cmdflag('F',iopt)/=0.and.cmdflag('C',iopt)/=0) then
        write (*,*) 'Both compact (/c) and full (/f) output specified. Only one may be included on command line.'
        stop
    end if
    if (cmdflag('C',iopt)/=0) outputformat = 1
    if (cmdflag('F',iopt)/=0) outputformat = 2

    return

5010 format (i4.4,'/',i2.2,'/',i2.2)
    end   subroutine read_command_options

    ! --------------------------- shellsort -------------------------------------------

    subroutine shellsort (ra, nr)

    integer, intent(in) :: nr
    real(eb), intent(inout) :: ra(nr)

    integer j, i, inc
    real(eb) rra

    inc = 1
1   inc = 3*inc+1
    if (inc<=nr) go to 1
2   continue
    inc = inc/3
    do i = inc+1, nr
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

    !     routine: sstrng
    !     purpose: this routine finds positions of substrings within a character string.  a space, comma, - , (, or )
    !              indicates the beginning or end of a substring. when called, the string is passed as an integer(choose)
    !              along with the number of characters in the string(wcount) and a starting position(sstart).  beginning at
    !              "sstart", the routine searches for a substring. if a substring is found, its first and last character
    !              positions are returned along with a true value in "svalid"; otherwise "svalid" is set false.
    !     arguments: string - the character string
    !                wcount - number of characters in the string
    !                sstart - beginning position in string to look for a substring
    !                sfirst (output) - beginning position of the substring
    !                slast - ending position of the substring
    !                svalid - true if a valid substring is found

    integer, intent(in) :: sstart, wcount
    character, intent(in) :: string(*)
    logical, intent(out) :: svalid

    integer, intent(out) :: sfirst, slast

    integer :: endstr, i, j
    character :: space = ' ', comma = ','

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

    ! --------------------------- upperall -------------------------------------------

    subroutine upperall(string)

    !     routine: upperall
    !     purpose: convert a string to upper case
    !     arguments: string - string to be converted

    character, intent(inout) :: string*(*)

    integer nr, i
    character :: c

    nr = len_trim(string)
    do i = 1, nr
        c = string(i:i)
        if (c>='a'.and.c<='z') then
            c = char(ichar(c) + ichar('A')-ichar('a'))
        end if
        string(i:i) = c
    end do
    return
    end subroutine upperall

    ! --------------------------- funit -------------------------------------------

    integer function funit (io)

    !     routine: funit
    !     purpose: finds first avalable i/o unit starting at unit number io
    !     arguments: io - beginning unit number for search

    integer, intent(in) :: io

    integer, parameter :: mxio=32767
    integer :: itmp
    logical opend

    itmp = io-1
10  continue
    itmp = itmp+1
    if (itmp>mxio) then
        funit = -1
        return
    end if
    inquire(unit=itmp,opened = opend)
    if (opend) go to 10
    funit = itmp
    return
    end function funit

    ! --------------------------- opnotpt -------------------------------------------

    subroutine opnotpt (filname, iounit)

    !     routine: opnotpt
    !     purpose: opens a file using the extension to distinguish previous open files
    !     arguments: filname - base filename for file to be opened
    !                iounit - desired unit number for file

    integer, intent(in) :: iounit
    character, intent(in) :: filname*(*)

    integer :: first, last , length, ilen, itmp
    character :: namefil*60, workfil*60, fmt*14
    logical :: existed, valid

    length = len (filname)
    call sstrng (filname, length, 1, first, last, valid)
    if (.not.valid) stop 'Cannot open debug file'
    ilen = last - first + 1
    namefil = ' '
    namefil(1:ilen) = filname(first:last)

    itmp = 0
30  continue
    itmp = itmp + 1
    write (fmt,10) ilen
10  format('(a',i2.2,',','','.','',',i3.3)')
    write (workfil,fmt) namefil, itmp
    inquire (file = workfil, exist = existed)
    if (existed) go to 30
    open (unit = iounit, file = workfil,recl=255)
    return
    end subroutine opnotpt

    ! --------------------------- xerbla -------------------------------------------

    subroutine xerbla ( srname, info )

    !     routine: xerbla
    !     purpose: opens a file using the extension to distinguish previous open files
    !     arguments: srname - specifies the name of the routine which called xerbla
    !                info - on entry, info specifies the position of the invalid parameter in the
    !                       parameter-list of the calling routine.

    integer, intent(in) :: info
    character(6), intent(in) :: srname

    write (*,99999) srname, info
    stop

99999 format ( '***Error: ** on entry to ', a6, ' parameter number ', i2,' had an illegal value' )

    end subroutine xerbla

    ! --------------------------- laame -------------------------------------------

    logical function lsame ( ca, cb )

    !     routine: lsame
    !     purpose: tests if ca is the same letter as cb regardless of case. cb is assumed to be an upper
    !              case letter. lsame returns .true. if ca is either the same as cb or the equivalent lower case letter.
    !     arguments: ca - first character
    !                cb - second character

    character(1), intent(in) :: ca, cb

    integer, parameter :: ioff = 32
    intrinsic ichar

    lsame = ca == cb

    if ( .not.lsame ) then
        lsame = ichar(ca) - ioff == ichar(cb)
    end if
    return

    end function lsame

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

    end module utility_routines

    module opening_fractions

    !	The following functions implement the simple open/close function for vents.
    !	This is done with a simple, linear interpolation.
    !   The opening arrays are built into the vent data structures and are of the form
    !		(1) Is start of time to change
    !		(2) Is the initial fraction (set in HVENT, VVENT and MVENT)
    !		(3) Is the time to complete the change, Time+Decay_time, and
    !		(4) Is the final fraction

    !	The open/close function is done in the physical/mode interface, horizontal_flow, vertical_flow and mechanical_flow

    use precision_parameters
    use cparams, only: trigger_by_time, trigger_by_temp, trigger_by_flux
    use ramp_data
    use vent_data
    use room_data, only: roominfo, nrm1
    use target_data, only: targetinfo
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
            if (nmlflag.and.rampid/='NULL') then
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

    subroutine get_vent_opening (rampid,venttype,room1,room2,counter,vent_index,time,fraction)

    character(len=64), intent(in) :: rampid
    character(len=1), intent(in) :: venttype
    integer, intent(in) :: room1, room2, counter, vent_index
    real(eb), intent(in) :: time
    real(eb), intent(out) :: fraction

    integer :: iramp, i
    real(eb), parameter :: mintime = 1.0e-6_eb
    real(eb) :: dt, dtfull, dy, dydt
    type(ramp_type), pointer :: rampptr
    type(vent_type), pointer :: ventptr

    fraction = 1.0_eb

    if (n_ramps>0) then
        iramp = find_vent_opening_ramp (rampid,venttype,room1,room2,counter)
        if (iramp>0) then
            rampptr=>rampinfo(iramp)
            if (time<=rampptr%x(1)) then
                fraction = rampptr%f_of_x(1)
                return
            else if (time>rampptr%x(rampptr%npoints)) then
                fraction = rampptr%f_of_x(rampptr%npoints)
                return
            else
                do i=2,rampptr%npoints
                    if (time>rampptr%x(i-1).and.time<=rampptr%x(i)) then
                        dt = max(rampptr%x(i)-rampptr%x(i-1),mintime)
                        dtfull = max(time-rampptr%x(i-1),mintime)
                        dy = rampptr%f_of_x(i)-rampptr%f_of_x(i-1)
                        dydt = dy / dt
                        fraction = rampptr%f_of_x(i-1)+dydt * dtfull
                        return
                    end if
                end do
            end if
        end if
    end if

    ! This is for backwards compatibility with the older EVENT format for single vent changes
    fraction = 1.0_eb
    if (venttype=="H") then
        ventptr => hventinfo(vent_index)
        fraction = vfraction(venttype,ventptr, time)
    else if (venttype=="V") then
        ventptr => vventinfo(vent_index)
        fraction = vfraction(venttype,ventptr, time)
    else if (venttype=="M") then
        ventptr => mventinfo(vent_index)
        fraction = vfraction(venttype,ventptr, time)
    else if (venttype=="F") then
        ventptr => mventinfo(vent_index)
        fraction = vfraction(venttype,ventptr, time)
    end if
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
                room1c = roominfo(ventptr%room1)%name
                if (ventptr%room1>nrm1) room1c = 'Outside'
                room2c = roominfo(ventptr%room2)%name
                if (ventptr%room2>nrm1) room2c = 'Outside'
                vtypec = 'Unknown '
                if (vtype=='H') vtypec = 'Wall'
                if (vtype=='V') vtypec = 'Ceiling/Floor'
                if (vtype=='M') vtypec = 'Mechanical'
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
                room1c = roominfo(ventptr%room1)%name
                if (ventptr%room1>nrm1) room1c = 'Outside'
                room2c = roominfo(ventptr%room2)%name
                if (ventptr%room2>nrm1) room2c = 'Outside'
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
