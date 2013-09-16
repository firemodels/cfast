
! --------------------------- xerror -------------------------------------------

    subroutine xerror(messg,nmessg,nerr,level)

    !     routine: xerror
    !     purpose: XERROR processes a diagnostic message. It is a stub routine written for the book above. Actually, XERROR is a sophisticated
    !              error handling package with many options. Our version has the same calling sequence
    !              but only prints an error message and either returns (if the input value of ABS(LEVEL) is less than 2) or stops (if the input value of ABS(LEVEL) equals 2).
    !     Revision: $Revision$
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: MESSG - the Hollerith message to be processed.
    !                NMESSG- the actual number of characters in MESSG. (this is ignored in this stub routine)
    !                NERR  - the error number associated with this message. NERR must not be zero. (this is ignored in this stub routine)
    !                LEVEL - error category.
    !                       = 2 means this is an unconditionally fatal error.
    !                       = 1 means this is a recoverable error.  (I.e., it is non-fatal if XSETF has been appropriately called.)
    !                       = 0 means this is a warning message only.
    !                       =- 1 means this is a warning message which is to be printed at most once, regardless of how many times this call is executed.
    !               (in this stub routine, LEVEL=2 causes a message to be printed and then a stop. LEVEL=-1,0,1 causes a message to be printed and then a return.

    use precision_parameters
    implicit none

    character(*), intent(in) :: messg
    integer, intent(in) :: nmessg, level, nerr
    
    character(32) :: error_label
    
    integer :: nmess

    if(level==2)then
       error_label = "***Fatal error:"
    else if(level==1)then
       error_label = "***Error:"
    else
       error_label = "***Warning:"
    endif

    if(nmessg==0)then
        nmess = len_trim(messg)
    else
        nmess = nmessg
    endif
    nmess = max(1,nmess)

    if(level==-2)then
    else if(level==-3)then
       call xerrwv(messg,nmess,nerr,level,0,0,0,0,0.0_eb,0.0_eb)
    else
       call xerrwv(trim(error_label)//" "//messg,nmess+len_trim(error_label)+1,nerr,level,0,0,0,0,0.0_eb,0.0_eb)
    endif

    return
    end subroutine xerror

! --------------------------- xerrwv -------------------------------------------

    subroutine xerrwv(msg,nmes,nerr,level,ni,i1,i2,nnr,r1,r2)

    !     routine: xerrwv
    !     purpose: xerrwv, as given here, constitutes a simplified version of the slatec error handling package. It just prints our error messages with codes as requested
    !     arguments: msg - the message (character array).
    !                nmes - the length of msg (number of characters).
    !                nerr - the error number (not used).
    !                level - the error level. 0 or 1 means recoverable (control returns to caller). 2 means fatal (run is aborted).
    !                ni - number of integers (0, 1, or 2) to be printed with message.
    !                i1,i2 - integers to be printed, depending on ni.
    !                nnr - number of reals (0, 1, or 2) to be printed with message.
    !                r1,r2 - reals to be printed, depending on nnr.

    use precision_parameters
    use cshell
    implicit none

    integer, intent(in) :: nmes, nerr, level, ni, i1, i2, nnr
    real(eb), intent(in) :: r1, r2
    character, intent(in) :: msg(nmes)*1
    
    integer :: i, lunit, mesflg

    ! define message print flag and logical unit number
    mesflg = 1
    lunit = logerr
    if (mesflg/=0) then

        ! write the message
        write (lunit,5000) (msg(i),i = 1,nmes)
        if (ni==1) write (lunit,5010) nerr,i1
        if (ni==2) write (lunit,5020) nerr,i1, i2
        if (nnr==1) write (lunit,5030) nerr,r1
        if (nnr==2) write (lunit,5040) nerr,r1, r2
    endif

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
    !     purpose: d1mach can be used to obtain machine-dependent parameters for the local machine environment.  it is a function subprogram with one
    !              (input) argument. reference  p. a. fox, a. d. hall and n. l. schryer, framework for a portable library, acm transactions on mathematical software 4, 2 (june 1978), pp. 177-188.
    !     arguments:  i
    !
    !           where i = 1,...,5.  the (output) value of a above is determined by the (input) value of i.  the results for various values of i are discussed below.
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
    
    use precision_parameters
    implicit none
    
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
    end

! --------------------------- xerrormod -------------------------------------------

    subroutine xerrmod(mesg,nerr,nnr,r1,r2)

    !     routine: xerrmod
    !     purpose: xerrmod is a simplified version of the slatec error handling package. it just logs our error messages with codes as requested. adapted from code written by a. c. hindmarsh and p. n. brown at llnl.
    !     arguments: msg - the message (character array).
    !                nmes - the length of msg (number of characters).
    !                nerr - the error number (not used).
    !                nnr - number of reals (0, 1, or 2) to be printed with message.
    !                r1,r2 - reals to be printed, depending on nnr.

    use precision_parameters
    use cparams
    use cshell
    implicit none

    integer, intent(in) :: nerr, nnr
    real(eb), intent(in) :: r1, r2
    character, intent(in) :: mesg*(*)
    
    integer :: lm

    lm = len_trim(mesg)

    ! write the message
    write(logerr,5000) mesg(1:lm)
    if (nnr==1) write(logerr,5001) nerr,r1
    if (nnr==2) write(logerr,5002) nerr,r1,r2
    return

5000 format(a)
5001 format('ierror,r1 =',i5,2d14.4)
5002 format('ierror,r1,r2 =',i5,2d14.4)
    end subroutine xerrmod

! --------------------------- cfastexit -------------------------------------------

    subroutine cfastexit (name, errorcode)

    !     routine: cfastexit
    !     purpose: routine is called when CFAST exits, printing an error code if necessary
    !     arguments: name - routine name calling for exit ... at this point, it's always "CFAST"
    !                errorcode - numeric code indicating reason for an error exit.  0 for a normal exit

    use cparams
    use cshell
    use iofiles

    character, intent(in) :: name*(*)
    integer, intent(in) :: errorcode

    if (errorcode==0) then
        write(logerr, '(''Normal exit from '',a)') trim(name)
    else
        write(logerr,'(''***Error exit from '',a,'' code = '',i5)') trim(name), errorcode
    endif

    stop

    end subroutine cfastexit

! --------------------------- cmdline -------------------------------------------

    subroutine cmdline (nargs,strs,iarg,iopt)

    !     routine: cmdline
    !     purpose: gets argument list and options from command line. options may be of the form c:<string> where <c> is the desired option a-z and <string> is a character string associated with that option.
    !     arguments: nargs maximum number of arguments expected (dimension limit on input, actual number on output.
    !                strs  returned strings of arguments and options
    !                iarg  returned list of pointers to elements in strs corresponding to arguments 1..nargs
    !                iopt  returned list of pointers to elements in strs corresponding to options a-z

    implicit none

    integer, intent(inout) :: nargs
    integer, intent(out) :: iarg(nargs), iopt(26)
    character, intent(out) :: strs(nargs)*(*) 
    
    integer :: ic, ia, i 
    character :: cmdlin*127, optsep

    optsep = '/'

    do ic = 1, max0(nargs,26)
        if (ic<=nargs) then
            strs(ic) = ' '
            iarg(ic) = 0
        endif
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
        endif
        if (cmdlin(ic:127)/=' '.and.ic<=126) go to 20
        if (cmdlin(1:1)==' ') then
            call cmove(cmdlin,1,126,2,127,127,' ')
        endif

        ! put in commas where appropriate to delimit all fields
        ic = 2
30      if (cmdlin(ic:ic)==' ') then
            if (cmdlin(ic-1:ic-1)/=','.and.cmdlin(ic+1:ic+1)/=',') then
                cmdlin(ic:ic) = ','
                ic = ic + 1
            else
                call cmove(cmdlin,ic,126,ic+1,127,127,' ')
            endif
        else if ((cmdlin(ic:ic)==optsep).and.cmdlin(ic-1:ic-1)/=',') then
            call cmove(cmdlin,ic+1,127,ic,126,ic,',')
            ic = ic + 2
        else
            ic = ic + 1
        endif
        if (cmdlin(ic:127)/=' '.and.ic<=126) go to 30
    endif

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
    endif

    ! assign the parsed fields to appropriate arguments and options
    nargs = 0
    if (ia>0) then
        do i = 1, ia
            if (strs(i)(1:1)==optsep) then
                if (strs(i)(2:2)>='A'.and.strs(i)(2:2)<='Z') then
                    iopt(ichar(strs(i)(2:2))-ichar('A')+1) = i
                else if (strs(i)(2:2)>='a'.and.strs(i)(2:2)<='z') then
                    iopt(ichar(strs(i)(2:2))-ichar('a')+1) = i
                endif
                cmdlin = strs(i)
                call cmove(cmdlin,1,127,3,127,127,' ')
                if (cmdlin(1:1)==':') call cmove(cmdlin,1,127,2,127,127,' ')
                strs(i) = cmdlin
            else
                nargs = nargs + 1
                iarg(nargs) = i
            endif
        end do
    endif
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
    
    character, intent(out) :: cmdlin*(*)
    
    character :: temp*127 
    
    temp = cmdlin
    temp(i1:i2) = cmdlin(i3:i4)
    temp(i5:i5) = chr
    cmdlin = temp
    return
    end subroutine cmove

! --------------------------- getcl -------------------------------------------

    subroutine getcl(cmdlin)

    !     routine: getcl
    !     purpose: get command line as a single string
    !     arguments: cmdlin - command line

    use ifport
    use cfin

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
            endif
        end do
    endif
    return
    end

! --------------------------- convrt -------------------------------------------

    subroutine convrt(coord,first,last,type,i0,x0)

    !     routine: convrt
    !     purpose: convert next entry in string coord to a number of correct type.
    !     arguments: coord  string to be parsed
    !                first  beginning position of substring to be parsed
    !                last   end of substring
    !                type   type of number (1=integer, 2=real)
    !                i0     value if integer
    !                x0     value if real

    use precision_parameters
    use cfin

    integer, intent(in) :: first, last
    integer, intent(out) :: type
    character(*), intent(in) :: coord
    
    integer, intent(out) :: i0
    
    real(fb), intent(out) :: x0
    
    character(20) :: decod
    integer lfirst,llast

    ! get data type
    call datype(coord,first,last,type)
    decod = ' '
    lfirst = min(first,lbufln)
    llast = min(last,first+20,lbufln)
    decod = coord(lfirst:llast)

    ! decode by type
    if (type==1) then
        !x0 = rnum(decod)
        read(decod,*) x0
    else if (type==2) then
        !i0 = inum(decod)
        read(decod,*) i0
    endif
    return
    end subroutine convrt

! --------------------------- inum -------------------------------------------

    integer function inum(strng)

    !     routine: inum
    !     purpose: convert string into an integer
    !     arguments: strng - string containing number to be converted.

    character, intent(in) :: strng*(*)
    
    integer ival,isgn,i,ic
    
    ival = 0
    isgn = 1
    do i = 1, len(strng)
        if (strng(i:i)=='-') isgn = -1
        ic = ichar(strng(i:i)) - ichar('0')
        if (ic>=0.and.ic<=9) ival = ival*10 + ic
    end do
    inum = isgn*ival
    return
    end function inum

! --------------------------- rnum -------------------------------------------

    real function rnum(strng)

    !     routine: rnum
    !     purpose: convert string into an real number
    !     arguments: strng - string containing number to be converted.

    use precision_parameters
    implicit none
    
    character, intent(in) :: strng*(*)
    
    character :: chr
    integer isgn,idec,iesgn,iexp,ip,ic
    real rval,eval
    
    rval = 0.0
    isgn = 0
    idec = 0
    iesgn = 0
    iexp = 0
    ip = 1
10  chr = strng(ip:ip)

    ! first comes a sign or mantissa
    ic = ichar(chr) - ichar('0')
    if (isgn==0) then
        isgn = 1
        if (chr=='-') isgn = -1
        if (chr=='.') idec = 1
        if (ic>=0.and.ic<=9) then
            rval = rval*10.0_eb + ic
        endif

        ! if we've found the mantissa, check for exponent
    else if (chr=='E'.or.chr=='e'.or.chr=='D'.or.chr=='d'.or.chr=='+'.or.chr=='-') then
        iesgn = 1
        if (chr=='-') iesgn = -1
        else

        ! if no exponent, keep track of decimal point
        if (iesgn==0) then
        if (chr=='.') then
        idec = 1
    else if (ic>=0.and.ic<=9) then
        rval = rval*10.0_eb + ic
        if (idec/=0) idec = idec + 1
    endif

    ! if exponent just keep track of it
    else
        if (ic>=0.and.ic<=9) iexp = iexp*10 + ic
    endif
    endif
    ip = ip + 1
    if (ip<len(strng)) go to 10
    if (idec/=0) idec = idec - 1
    eval = 10.0_eb**(abs(iesgn*iexp-idec))
    iesgn = isign(1,iesgn*iexp-idec)
    if (iesgn==1) rnum = isgn*rval*eval
    if (iesgn==-1) rnum = isgn*rval/eval
    return
    end function rnum

! --------------------------- countargs -------------------------------------------

    logical function countargs (tocount,lcarray,numc,nret)

    !     routine: countargs
    !     purpose: Count the number of non-blank arguments on the input line. Should be tocount. If not, then return an error (logical false). If tocount is zero or less, just count them
    !     arguments: label - CFAST keyword for this input
    !                tocount - expected number of arguments for this input
    !                lcarray - character array of arguments.  There should be tocount non-blank entries
    !                numc - dimension limit on lcarray
    !                nret - actual number of arguments

    use cshell
    
    implicit none

    integer, intent(in) :: tocount, numc
    character(128), intent(in) :: lcarray(numc)
    
    integer, intent(out) :: nret
    
    integer :: i

    countargs = .false.
    nret = 0.

    ! check for the expected number of arguments if tocount >=0
    if (tocount>0) then
        do i = 1, tocount
            if (lcarray(i)==' ') then
                return
            endif
            nret = nret + 1
        end do
    endif

    ! we have the number expected or tocount <=0, just count them now
    countargs = .true.
    do i = tocount+1, numc
        if (lcarray(i)/=' ') nret = nret + 1
    end do

    return

5000 FORMAT('Error for key word: ',a5,' Required count = ',i2,' and there were only ',i2,' entries')
5001 FORMAT('Key word ',a5,' parameter(s) = ',20a10)
5002 FORMAT('Key word (ext) ',a5,' parameter(s) = ',128a10)
5003 FORMAT('Error for key word: ',a5,' Required count = ',i2,' and there were no entries')
5004 FORMAT('Error for key word: ',a5,' Required count = ',i2,' and there was only 1 entry')
    end function countargs

! --------------------------- cptime -------------------------------------------

    subroutine cptime (cputim)

    !     routine: cptime
    !     purpose: routine to calculate amount of computer time (cputim) in seconds used so far.  this routine will generally be different for each computer.
    !     arguments: cputim (output) - elapsed cpu time 

    use precision_parameters
    use ifport
    
    implicit none
    
    real(eb), intent(out) :: cputim
    
    integer(2) :: hrs, mins, secs, hsecs

    call gettim(hrs,mins,secs,hsecs)
    cputim = real(hrs,eb)*3600.0_eb + real(mins,eb)*60.0_eb + real(secs,eb) + real(hsecs,eb)/100.0_eb
    return
    end subroutine cptime

! --------------------------- datype -------------------------------------------

    subroutine datype (crd,n1,n2,dtype)

    !     routine: datype
    !     purpose: this routine determines the data type of a string. the character string is examined between given starting and ending positions to determine if the substring is an integer, a real number, or a non-numeric string.
    !     arguments: crd    string containing number to be typed
    !                n1     starting position
    !                n2     ending position
    !                dtype (output) - returned type (1=real, 2=integer, 3=non-numeric)

    implicit none
    
    character, intent(in) :: crd*(*)
    integer, intent(in) :: n1, n2
    
    integer, intent(out) :: dtype
    
    
    integer :: i, j
    logical :: period, efmt
    character :: num(12)*1 = (/'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-'/)
    
    period = .false.
    efmt = .false.

    ! determine data type.  assume a numeric entry until a non-numeric character is found.
    do 20 i = n1, n2
        if (crd(i:i)=='.') then
            if (period) then

                ! second period in string - non numeric
                go to 30
            else
                period = .true.
            endif
            go to 20
        endif

        ! check for digits
        do j = 1, 12
            if (crd(i:i)==num(j)) go to 20
        end do
        if (index('EeDd',crd(i:i))==0.or.efmt) go to 30
        efmt = .true.
20  continue

    ! determine type of numeric entry
    if (period.or.efmt) then

        ! real
        dtype = 1
    else

        ! integer
        dtype = 2
    endif
    return

    ! non-numeric
30  dtype = 3
    return
    end subroutine datype

! --------------------------- doesthefileexist -------------------------------------------

    logical function doesthefileexist (checkfile)

    !     routine: doesthefileexist
    !     purpose: checks for the existence of given file name
    !     arguments: checkfile - file name

    implicit none
    
    character(*), intent(in) :: checkfile
    logical yesorno


    inquire (file=checkfile, exist=yesorno)
    if (yesorno) then
        doesthefileexist = .true.
    else
        doesthefileexist = .false.
    endif

    return

    end function doesthefileexist

! --------------------------- exehandle -------------------------------------------

    subroutine exehandle (exepath, datapath, project, errorcode)

    !     routine: exehandle
    !     purpose: get the arguments used to call the main program
    !     arguments: exepath - path (without the name) to the folder where the executable resides
    !                datapath - path (without a file name) to the folder where the project data file resides
    !				 project - name of the project - this name cannot exceed 64 charcters. the total lenght of datapath + project cannot exceed 256 characters
    !                errorcode - error code on return (i*4)
    !					100 program called with no arguments
    !					101 the filename has an extension
    !					102 project files does not exist
    !					103 total file name length includ path >256
    !					0 okay

    implicit none
    
    character(*), intent(out) :: exepath, datapath, project
    integer, intent(out) :: errorcode
    
    integer :: i
    integer(2) :: n, status, loop, ld(2), li(2), ln(2), le(2), lb
    character(256) :: buf, xname
    character (64) :: name(2)
    logical :: doesthefileexist

    character(3) :: drive(2)
    character(256) :: dir(2)
    character(64) :: ext(2)
    integer(4) :: length, pathcount, splitpathqq, ilen

    n = command_argument_count() + 1
    project = ' '
    exepath = ' '
    datapath = ' '

    if (n<2) then
        errorcode = 100
        return
    endif

    errorcode = 0

    ! get the calling program and arguments

    exepath = ' '
    datapath = ' '

    do i = 1, 2
        loop = i - 1
        call get_command_argument(loop, buf, ilen, status)
        if(ilen>0) then
            xname = buf

            !	Split out the components

            length = SPLITPATHQQ(xname, drive(i), dir(i), name(i), ext(i))
            ld(i) = len_trim(drive(i))
            li(i) = len_trim(dir(i))
            ln(i) = len_trim(name(i))
            le(i) = len_trim(ext(i))

            pathcount = 5 + ln(i) + li(i) +ld(i) + le(i)

            if (pathcount>255.or.ln(i)>64) then
                errorcode = 103
                return
            endif
        endif
    end do

    ! Now check that the project.in file exists - this is the data file
    buf = ' '
    if (le(2)/=0) then
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // ext(2)(1:le(2)) // '.in'
    else
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // '.in'
    endif

    lb = len_trim(buf)

    ! buf(1:lb) is the data file to check

    if (DoesTheFileExist(buf(1:lb))) then

        !	The project file exists
        exepath = drive(1)(1:ld(1)) // dir(1)(1:li(1))
        datapath = drive(2)(1:ld(2)) // dir(2)(1:li(2))
        project = name(2)(1:ln(2)) // ext(2)(1:le(2))
        return
    else
        ! Note that we do not yet have the logerr file open, so write to the console
        errorcode = 102
    endif
    return

    end subroutine exehandle

! --------------------------- grabky -------------------------------------------

    subroutine grabky (ich,it)

    use ifport
    
    implicit none
    
    integer(2), intent(out) :: ich, it

    character(1) :: ch, getcharqq
    logical :: peekcharqq

    ich = 0
    it = 0

    if (peekcharqq()) then
        ch = getcharqq()
        ich = ichar(ch)
        if (ich==0) then
            ch = getcharqq()
            ich = ichar (ch)
            it = 2
        else
            it = 1
        endif
    endif

    return
    end subroutine grabky

! --------------------------- mat2mult -------------------------------------------

    subroutine mat2mult(mat1,mat2,idim,n)

    !     routine: mat2mult
    !     purpose: given an nxn matrix mat1 whose elements are either 0 or 1, this routine computes the matrix mat1**2 and returns the results in mat1 (after scaling non-zero entries to 1).
    !     arguments: mat1 - matrix
    !                mat2 - work array of same size as mat1
    !                idim - actual dimensino limit on first subscript of mat1
    !                n - size of matrix
    !                matiter - unused

    implicit none
    
    integer, intent(in) :: idim, n
    integer, intent(inout) :: mat1(idim,n)
    integer, intent(out) :: mat2(idim,n)
    
    integer :: i, j, k

    do i = 1, n
        do j = 1, n
            mat2(i,j) = 0
            do k = 1, n
                mat2(i,j) = mat1(i,k)*mat1(k,j)
            end do
            if(mat2(i,j)>=1)mat2(i,j) = 1
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

    !     routine: indexi
    !     purpose: this routines sorts the array arrin passively via the permuation array indx.  the elements arrin(indx(i)), i=1, ..., n are in increasing order. this routine uses a bubble sort.  it should not be used
    !              for large n (n>30), since bubble sorts are not efficient.  
    !     arguments: n     number of elements in n
    !                arrin array to be passively sorted
    !                indx  permuation vector containing ordering such that arrin(indx) is in increasing order.

    implicit none
    
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
        if(ai<=aip1) cycle
        iswitch = 1
        itemp = indx(i)
        indx(i) = indx(i+1)
        indx(i+1) = itemp
    end do
    do  i = 2, n-1, 2
        ai = arrin(indx(i))
        aip1 = arrin(indx(i+1))
        if(ai<=aip1) cycle
        iswitch = 1
        itemp = indx(i)
        indx(i) = indx(i+1)
        indx(i+1) = itemp
    end do
    if(iswitch==1) go to 5
    return
    end subroutine indexi

! --------------------------- interp -------------------------------------------

    subroutine interp (x,y,n,t,icode,yint)

    !     routine: indexi
    !     purpose: routine interpolates a table of numbers found in the arrays, x and y.  
    !     arguments: x,y - arrays of size n to be interpolated at x=t
    !                icode - code to select how to extrapolate values if t is less than x(1) or greater than x(n).
    !                          if icode = 1 then yint = y(1) for t < x(1) and yint = y(n) for t > x(n).
    !                          if icode = 2 then yint is evaluated by interpolation if x(1) < t < x(n) and by extrapolation if t < x(1) or    t > x(n)
    !                yint (output) - interpolated value of the y array at t

    use precision_parameters
    implicit none

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
    endif
    if (t<=x(1)) then
        if (icode==1) then
            yint = y(1)
            return
        else
            imid = 1
            go to 20
        endif
    endif
    if (t>=x(n)) then
        if (icode==1) then
            yint = y(n)
            return
        else
            imid = n - 1
            go to 20
        endif
    endif
    if (ilast+1<=n) then
        imid = ilast
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    endif
    if (ilast+2<=n) then
        imid = ilast + 1
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    endif
    ia = 1
    iz = n - 1
10  continue
    imid = (ia+iz)/2
    if (t<x(imid)) then
        iz = imid - 1
        go to 10
    endif
    if (t>=x(imid+1)) then
        ia = imid + 1
        go to 10
    endif
20  continue
    dydx = (y(imid+1)-y(imid))/(x(imid+1)-x(imid))
    yint = y(imid) + dydx*(t-x(imid))
    ilast = imid
    return
    end subroutine interp

! --------------------------- length -------------------------------------------

    integer function length(string)
    character, intent(in) :: string*(*)
    
    if (len(string)/=0) then
        length = len_trim(string)
    else
        length = 0
    endif
    return
    end function length

! --------------------------- mess -------------------------------------------

    subroutine mess (p,z)

    !     routine: mess
    !     purpose: write a literal string of characters.  this is a write to stdout with a return/linefeed at the end 
    !     arguments: phrase - character string to be written
    !                z - length of the string

    use cparams
    use cshell
    
    implicit none

    integer, intent(in) :: z
    character(len=z), intent(in) :: p
    
    integer :: i

    if (z>2048) stop 'error in message handler'
    write (iofilo,'(1x,2048a1)') (p(i:i),i=1,z)
    return

    end subroutine mess

! --------------------------- messnrf -------------------------------------------

    subroutine messnrf (string, l)

    !     routine: messnrf
    !     purpose: write a literal string of characters.  this is a write to stdout with a return/linefeed at the end 
    !     arguments: phrase - character string to be written
    !                l - length of the string

    use cparams
    use cshell
    
    implicit none

    integer, intent(in) :: l
    character, intent(in) :: string*(*)
    
    character :: formatt*100
    
    write (formatt,4000) l
    write (iofilo,formatt) string(1:l)
    return

4000 format('(1x,a',i2.2,',$)')

    end subroutine messnrf

! --------------------------- readastu -------------------------------------------

    subroutine readastu (in, count, start, max, valid)

    !     routine: readastu
    !     purpose: read in a string from the standard input device
    !              this version of readas(tu) is similar to the one used by everone else (readas) except that this contains automatic conversion to upper case. this is to filter commands from the console so that they are not case sensitive.
    !     arguments: 

    use cparams
    use cshell
    
    implicit none

    integer, intent(in) :: max
    
    integer, intent(out) :: count, start
    logical, intent(out) :: valid
    character, intent(out) :: in(max)
    
    integer :: i, ich, nc
    character :: ch, inn*256, c

10  inn = ' '
    read(iofili,'(a256)',end=5) inn

    ! filter comments
    c = inn(1:1)
    if (c=='!'.or.c=='#') go to 10

    do i = 255,1,-1
        count = i
        if (inn(i:i)/=' ') go to 6
    end do
5   count = 0
    valid = .false.
    return

    ! check for control characters
6   nc = 0
    count = min(max,count)
    do 1 i = 1,count
        ich = ichar(inn(i:i))
        if (ich<32.or.ich>125) go to 3
        nc = i
1   continue
3   count = nc

    ! convert to upper case
    do i = 1,count
        ch = inn(i:i)
        ich = ichar(ch)
        if (ich>96.and.ich<123) ich = ich - 32
        in(i) = char(ich)
    end do
    start=1
    if (count>0) then
        valid = .true.
    else
        valid = .false.
    endif
    return
    end subroutine readastu

! --------------------------- readin -------------------------------------------

    subroutine readin (nreq, nret, fixed, flting)

    !     routine: readin
    !     purpose: read in a string and process it into the integer and floating variables "fixed" and "fltng"
    !     arguments: nreq - number of values required
    !                nret - actual number of values
    !                fixed - integer numbers returned
    !                flting - floting point numbers returned

    use precision_parameters
    use cfin
    use cfio
    use cparams
    use cshell
    
    implicit none

    integer, intent(in) :: nreq
    integer, intent(out) :: fixed(*)
    
    integer, intent(out) :: nret
    real(eb), intent(out) :: flting(*)
    
    integer :: input = 0, lstart = 0, i, j, llast, i0, iu, lenofch
    real(fb) :: x0, xxbig
    logical :: multi, eof
    character :: label*5, lable*5, slash*1 = '/', file*(*)
    save input, lstart

    nret = 0
    multi = .false.
    xxbig = 10000000.0_eb
    do i = 1, nreq
        call sstrng (inbuf, count, start, first, last, valid)
        if (.not.valid) then
            start = 257
            return
        endif
1       do j = first, last
            if (inbuf(j:j)==slash) then
                llast = j - 1
                multi = .true.
                go to 3
            endif
        end do
        llast = last
3       call convrt (inbuf, first, llast, type, i0, x0)
        if (type==1) then
            flting(nret+1) = x0
            fixed(nret+1) = ifix (min (x0, xxbig))
        else if (type==2) then
            fixed(nret+1) = i0
            flting(nret+1) = float(i0)
        else
            if (logerr>0) then
                write(logerr,12) first,llast,(inbuf(j:j),j=1,50)
12              format(' warning!! non-numeric data in ',2i3,50a1)
            endif
            nret = nret - 1
        endif
        if (multi) then

            ! loop for a continuation
            first = llast + 2
            multi = .false.
            nret = nret + 1
            go to 1
        endif
        count = count - (last-start+1)
        start = last + 1
        nret = nret + 1
    end do

    return

    ! read in a buffer
    entry readbf(iu,lable, eof)

    ! check to see if the buffer is current and full

    if (iu==input.and.start==lstart) then
        lable = label
        return
    endif
    input = iu
    call readas (iu, inbuf, count, start, valid)
    if (.not.valid) go to 11
    call sstrng (inbuf, count, start, first, last, valid)
    if (.not.valid) go to 11
    llast = min(last, first+5)
    label = ' '
    label = inbuf(first:llast)
    lable = label
    count = count - (last-start+1)
    start = last + 1
    lstart = start
    eof = .false.
    write(logerr,14) label
14  format ('label = ',a5)
    return

11  label = '     '
    lable = label
    start = 1
    count = 0
    eof = .true.
    return

    ! entry to force a context switch
    entry readrs

    ! set start to its initial value
    start = 257
    return

    ! read in a file name
    entry readfl (file)

    call sstrng (inbuf, count, start, first, last, valid)
    if (.not.valid) then
        start = 257
        file = ' '
        return
    endif
    lenofch = len (file)
    llast = min(last, first+lenofch)
    file = inbuf(first:llast)
    count = count - (last-start+1)
    start = last + 1
    return
    end  subroutine readin

! --------------------------- readas -------------------------------------------

    subroutine readas (infile, inbuf, count, start, valid)

    !     routine: readas
    !     purpose:  read in a string from the input file, filtering out comments

    use cparams
    use cshell
    
    implicit none

    integer, intent(in) :: infile
    character, intent(out) :: inbuf*(*)
    logical, intent(out) :: valid
    
    integer, intent(out) :: count, start
    
    integer :: irec = 0, ls
    character :: cmt1*1 = '!', cmt2*1 = '#', frmt*30
    save irec

    ! if we have reached an end of file, return nothing
10  inbuf = ' '
    if (irec==0) then
        read(infile,'(a)',end=2,err=2) inbuf
    else
        read(infile,'(a)',rec=irec,err=2) inbuf
        irec = irec + 1
    endif
    ls = len_trim (inbuf)
    write (frmt, 9) ls
    write (logerr,frmt) inbuf
9   format('(''buffer input = '',a',i3,')')

    ! filter comments
    if (inbuf(1:1)==cmt1.or.inbuf(1:1)==cmt2) go to 10
    count = len(inbuf)
    start = 1
    valid = .true.
    return
2   valid = .false.
    count = 0
    start = 1
    write(logerr,*) 'end of file for unit ',infile
    return
    end subroutine readas

! --------------------------- readop -------------------------------------------

    subroutine readop

    !     routine: readop
    !     purpose:  retrieve and process command line options and date
    !     arguments: none

    ! unit numbers defined in readop, openoutputfiles, readinputfiles
    !
    !      1 is for the solver.ini and data files (data file, tpp and objects) (iofili)
    !      3 is for the log file  (logerr)
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
    !     h to include the header in the output file
    !     d to turn on debugging writes
    !     t to output trace species mass
    !     v to output target fluxes relative to an ambient target (incident flux - sigma*eps*tamb**4) and smoke in mg/m^3
    !     n to output just target fluxes relative to ambient (smoke still in od)

    use ifport
    use cparams
    use cshell
    
    implicit none

    integer(2) :: year, month, day
    character :: strs(8)*60, ic
    character(60) :: solveini
    integer :: iarg(8), iopt(26), cmdflag, nargs
    cmdflag(ic) = iopt(ichar(ic)-ichar('A')+1)

    ! current date
    call getdat(year,month,day)
    rundat(3) = day
    rundat(2) = month
    rundat(1) = year
    write (mpsdatc,5010) rundat(1), rundat(2), rundat(3)

    ! command line arguments
    nargs = 8
    call cmdline(nargs,strs,iarg,iopt)

    if (cmdflag('H')/=0) header = .true.
    if (cmdflag('K')/=0) nokbd = .true.
    if (cmdflag('I')/=0) initializeonly = .true.
    if (cmdflag('D')/=0) debugging = .true.
    if (cmdflag('T')/=0) trace = .true.
    if (cmdflag('V')/=0) validate = .true.
    if (cmdflag('N')/=0) netheatflux = .true.
    logerr = 3

    if (cmdflag('F')/=0.and.cmdflag('C')/=0) stop 107
    if (cmdflag('C')/=0) outputformat = 1
    if (cmdflag('F')/=0) outputformat = 2

    if (cmdflag('S')/=0) then
        if (strs(cmdflag('S'))/=' ') then
            solveini = strs(cmdflag('S'))
        else
            solveini = 'SOLVE.INI'
        endif
        call writeini(solveini)
    endif

    return

5010 format (i4.4,'/',i2.2,'/',i2.2)
    end   subroutine readop

! --------------------------- shellsort -------------------------------------------

    subroutine shellsort (ra, n)

    use precision_parameters
    implicit none

    integer, intent(in) :: n
    real(eb), intent(out) :: ra(n)
        
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
3       if(ra(j-inc)>rra) then
            ra(j) = ra(j-inc)
            j = j - inc
            if(j<=inc) go to 4
            go to 3
        endif
4       ra(j) = rra
    enddo
    if(inc>1) go to 2
    return
    end  subroutine shellsort

! --------------------------- sortbrm -------------------------------------------

    subroutine sortbrm (x,lx,ix,lix,nrow,ncolx,ncolix,isort,ldp,nroom,ipoint)

    !     routine: sortbrm
    !     purpose:  sort the two arrays x and ix by the isort'th column of ix which contains room data.  this routine is based on the
    !               now obsolete routine sortfr.  this routine is used to sort fire and detector data structures by room number.
    !     arguments: x       floating point info to be sorted
    !                lx      leading dimension of x 
    !                ix      integer info to be sorted
    !                lix     leading dimension of ix in calling routine
    !                nrow    number of rows in x and ix
    !                ncolx   number of columns in x
    !                ncolix  number of columns in ix
    !                isort   column in ix to sort on (usually contains room numbers)
    !                ldp     leading dimension of ipoint
    !                nroom   number of elements for which ipoint is defined, also the number of rooms
    !                ipoint (output)  pointer array for sorted x and ix list.
    !                                 (r,1) = number of items (fires or detectors so far) in room r
    !                                 (r,2) = pointer to beginning element in ix and x for fire or detector in room r

    use precision_parameters
    implicit none
  
    ! if the number of fires, detectors or rooms ever exceeds 100 then the following dimension statement needs to be changed
    integer, parameter :: lwork=100
    
    
    integer, intent(in) :: lix, ncolix, ncolx
    integer, intent(in) :: nrow, isort, nroom, lx, ldp   
    
    integer, intent(out) :: ix(lix,ncolix), ipoint(ldp,*)
    real(eb), intent(out) :: x(lx,ncolx)
    
    integer :: i, j, iroom, iwork(lwork), iperm(lwork)
    real(eb) :: work(lwork)

    ! create a permutation vector using the isort'th column of ix
    if(nrow>lwork)then
        call xerror('not enough work space in sortbrm',0,1,2)
    endif
    do i = 1, nrow
        iperm(i) = i
    end do
        call indexi(nrow,ix(1,isort),iperm)

        ! reorder integer array using the permutation vector
        do j = 1, ncolix
            do i = 1, nrow
                iwork(i) = ix(iperm(i),j)
            end do
            do i = 1, nrow
                ix(i,j) = iwork(i)
            end do
        end do

        ! reorder the floating point arrays using the permutation vector
        do j = 1, ncolx
            do i = 1, nrow
                work(i) = x(iperm(i),j)
            end do
            do i = 1, nrow
                x(i,j) = work(i)
            end do
        end do

        ! construct the pointer array
        do i = 1, nroom
            ipoint(i,1) = 0
            ipoint(i,2) = 0
        end do
        do i = 1, nrow
            iroom = ix(i,isort)
            ipoint(iroom,1) = ipoint(iroom,1) + 1
            if (ipoint(iroom,2)==0) ipoint(iroom,2) = i
        end do
        do i = 1, nroom
            if (ipoint(i,2)==0) ipoint(i,2) = 1
        end do
        return
    end

! --------------------------- sortfr -------------------------------------------

    subroutine sortfr (nfire,ifroom,xfire,ifrpnt,nm1)

    !     routine: sortbrm
    !     purpose: sort the two arrays ifroom and xfire into increasing room number in ifroom.  these are used in this order by the ceiling jet and radiation algorithms
    !     arguments: nfire   number of fires
    !                ifroom (output)  room numbers for each of the fires
    !                xfire   fire related quantities used by other routines. see routine fires for definition.
    !                ifrpnt  pointer array for sorted fire list. (r,1) = number of fires in room r. (r,2) = pointer to beginning element in ifroom and xfire for fires in room r
    !                nm1 number of compartments minus 1

    use precision_parameters
    use cparams
    
    implicit none
    
    integer, intent(in) :: nm1, nfire
    
    integer, intent(out) :: ifroom(mxfire), ifrpnt(nr,2)
    real(eb), intent(out) :: xfire(mxfire,mxfirp)
    
    integer :: iperm(mxfire), iwork(mxfire), i, j, irm
    real(eb) :: work(mxfire)

    ! create a permutation vector from the list of fire rooms which is ordered by increasing room number
    do i = 1, nfire
        iperm(i) = i
    end do
    call indexi(nfire,ifroom,iperm)

    ! reorder the two arrays with the permutation vector
    do i = 1, nfire
        iwork(i) = ifroom(iperm(i))
    end do
    do i = 1, nfire
        ifroom(i) = iwork(i)
    end do

    do j = 1, mxfirp
        do i = 1, nfire
            work(i) = xfire(iperm(i),j)
        end do
        do i = 1, nfire
            xfire(i,j) = work(i)
        end do
    end do

    ! do the pointer arrays for the radiation and ceiling jet routines
    do i = 1, nm1
        ifrpnt(i,1) = 0
        ifrpnt(i,2) = 0
    end do
    do i = 1, nfire
        irm = ifroom(i)
        ifrpnt(irm,1) = ifrpnt(irm,1) + 1
        if (ifrpnt(irm,2)==0) ifrpnt(irm,2) = i
    end do
    do i = 1, nm1
        if (ifrpnt(i,2)==0) ifrpnt(i,2) = 1
    end do
    return
    end subroutine sortfr

! --------------------------- sstrng -------------------------------------------

    subroutine sstrng (string,wcount,sstart,sfirst,slast,svalid)

    !     routine: sstrng
    !     purpose: this routine finds positions of substrings within a character string.  a space, comma, - , (, or ) indicates the beginning or end of a substring.  
    !              when called, the string is passed as an integer(choose) along with the number of characters in the string(wcount) and a starting position(sstart).  beginning at
    !              "sstart", the routine searches for a substring. if a substring is found, its first and last character positions are returned along with a true value in "svalid"; otherwise "svalid" is set false.
    !     arguments: string - the character string
    !                wcount - number of characters in the string
    !                sstart - beginning position in string to look for a substring
    !                sfirst (output) - beginning position of the substring
    !                slast - ending position of the substring
    !                svalid - true if a valid substring is found

    implicit none

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
    if((string(i)/=space).and.(string(i)/=comma)) goto 60
    end do

    ! no substring found - only delimiter
    go to 40

    ! find position of last character of substring
60  do j = sfirst, endstr

    ! position of last element of substring
    slast = j-1
    if((string(j)==space).or.(string(j)==comma)) go to 100
    end do

    ! no substring delimiter => last character of substring is the last character of the string
    slast = endstr
    return

    ! no substring found
40  svalid = .false.
100 return
    end subroutine sstrng

! --------------------------- sstrngp -------------------------------------------

    subroutine sstrngp (string,wcount,sstart,sfirst,slast,svalid)

    !     routine: sstrngp
    !     purpose: this routine finds positions of substrings within a character string.  similar to sstrng, except entries can be grouped with parenthesis
    !     arguments: string - the character string
    !                wcount - number of characters in the string
    !                sstart - beginning position in string to look for a substring
    !                sfirst (output) - beginning position of the substring
    !                slast - ending position of the substring
    !                svalid - true if a valid substring is found

    integer, intent(in) :: sstart, wcount
    character, intent(in) :: string(128)
    logical, intent(out) :: svalid
    
    integer, intent(out) :: sfirst, slast
    
    integer :: endstr, i, j
    character :: space = ' ', comma = ',', rparen = ')', lparen = '('
    
    svalid = .true.
    endstr = sstart + wcount - 1

    ! find position of first element of substring
    do i = sstart, endstr

        ! move to the beginning of the substring
        sfirst = i
        if ((string(i)/=space).and.(string(i)/=comma).and.(string(i)/=rparen).and.(string(i)/=lparen)) go to 60
    end do

    ! no substring found - only delimiter
    go to 40

    ! find position of last character of substring
60  do j = sfirst, endstr

        ! position of last element of substring
        slast = j-1
        if ((string(j)==space).or.(string(j)==comma).or.(string(j)==rparen).or.(string(j)==lparen)) go to 100
    end do

    ! no substring delimiter => last character of substring is the last character of the string
    slast = endstr
    return

    ! no substring found
40  svalid = .false.
    return
100 if (slast<sfirst) svalid = .false.
    end subroutine sstrngp

! --------------------------- toupper -------------------------------------------

    character function toupper(ch)

    !     routine: toupper / tolower
    !     purpose: convert a single ascii character to upper or lower case
    !     arguments: ch - character to be converted

    implicit none
    
    character, intent(in) :: ch
    
    integer :: ich
    character :: tolower

    ! convert to upper case
    ich = ichar(ch)
    if (ich>96.and.ich<123) ich = ich - 32
    toupper = char(ich)
    return

    ! covert to lower case
    entry tolower(ch)
    ich = ichar(ch)
    if (ich>64.and.ich<91) ich = ich + 32
    tolower = char(ich)
    return
    end function toupper

! --------------------------- upperall -------------------------------------------

    subroutine upperall(from,to)

    !     routine: upperall
    !     purpose: convert a string to upper case
    !     arguments: from - string to be converted
    !                to (output) - converted string

    implicit none
    
    character, intent(in) :: from*(*)
    character, intent(out) :: to*(*)
        
    integer nfrom, nto, nn, i
    character :: c

    nfrom = len_trim(from)
    nto = len(to)
    nn = min(nfrom,nto)
    do i = 1, nn
        c = from(i:i)
        if(c>='a'.and.c<='z')then
            c = char(ichar(c) + ichar('A')-ichar('a'))
        endif
        to(i:i) = c
    end do
    if(nto>nn)to(nn+1:nto)=' '
    return
    end subroutine upperall

! --------------------------- funit -------------------------------------------

    integer function funit (io)

    !     routine: funit
    !     purpose: finds first avalable i/o unit starting at unit number io
    !     arguments: io - beginning unit number for search

    implicit none
    
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
    endif
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

    implicit none

    
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
    write(fmt,10) ilen
10  format('(a',i2.2,',','','.','',',i3.3)')
    write(workfil,fmt) namefil, itmp
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
    !                info - on entry, info specifies the position of the invalid parameter in the parameter-list of the calling routine.

    implicit none
    
    integer, intent(in) :: info
    character(6), intent(in) :: srname

    write (*,99999) srname, info
    stop

99999 format ( '***Error: ** on entry to ', a6, ' parameter number ', i2,' had an illegal value' )

    end subroutine xerbla

! --------------------------- laame -------------------------------------------

    logical function lsame ( ca, cb )

    !     routine: lsame
    !     purpose: tests if ca is the same letter as cb regardless of case. cb is assumed to be an upper case letter. lsame returns .true. if ca is either the same as cb or the equivalent lower case letter.
    !     arguments: ca - first character
    !                cb - second character

    implicit none
    
    character(1), intent(in) :: ca, cb
    
    integer, parameter :: ioff = 32
    intrinsic ichar

    lsame = ca == cb

    if ( .not.lsame ) then
        lsame = ichar(ca) - ioff == ichar(cb)
    endif
    return

    end function lsame

! --------------------------- rev_auxilliary -------------------------------------------

    integer function rev_auxilliary ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$revision: 526 $'
    character(255), parameter :: maindate='$date: 2012-08-30 12:49:31 -0400 (thu, 30 aug 2012) $'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_auxilliary = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_auxilliary
