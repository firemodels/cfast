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

    implicit none

    character*(*) messg
    real*8, parameter :: xx0 = 0.0d0
    integer nmessg, nmess, level, nerr

    if(nmessg==0)then
        nmess = len_trim (messg)
    else
        nmess = nmessg
    endif
    nmess = max(1,nmess)

    if(level/=-2) call xerrwv(messg,nmess,nerr,level,0,0,0,0,xx0,xx0)

    return
    end subroutine xerror

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

    use cshell
    implicit none

    integer :: nmes, nerr, level, ni, i1, i2, nnr, i, lunit, mesflg, imessg(2048)
    real*8 :: r1, r2
    character :: msg(nmes)*1, cc*1, foutnm*60

    ! define message print flag and logical unit number. -------------------
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

    real*8 function d1mach (i)

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
    implicit none
    integer :: i
    double precision :: b, x

    x = 1.0d0
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

    subroutine xerrmod(mesg,nmes,nerr,nnr,r1,r2)

    !     routine: xerrmod
    !     purpose: xerrmod is a simplified version of the slatec error handling package. it just logs our error messages with codes as requested. adapted from code written by a. c. hindmarsh and p. n. brown at llnl.
    !     arguments: msg - the message (character array).
    !                nmes - the length of msg (number of characters).
    !                nerr - the error number (not used).
    !                nnr - number of reals (0, 1, or 2) to be printed with message.
    !                r1,r2 - reals to be printed, depending on nnr.

    use cparams
    use cshell
    include "precis.fi"

    integer :: nmes, nerr, nnr
    real*8 :: r1, r2
    character :: mesg*(*)

    integer i, lunit, mesflg
    character*1  cc
    character*60 foutnm

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

    subroutine cfastexit (name, errorcode)

    !     routine: cfastexit
    !     purpose: routine is called when CFAST exits, printing an error code if necessary
    !     arguments: name - routine name calling for exit ... at this point, it's always "CFAST"
    !                errorcode - numeric code indicating reason for an error exit.  0 for a normal exit

    use cparams
    use cshell
    use iofiles

    character name*(*)
    integer errorcode
    logical doesthefileexist

    if (errorcode==0) then
        write(logerr, '(''Normal exit from '',a)') trim(name)
    else
        write(logerr,'(''Error exit from '',a,'' code = '',i5)') trim(name), errorcode
    endif

    stop

    end subroutine cfastexit

    subroutine cmdline (nargs,strs,iarg,iopt)

    !     routine: cmdline
    !     purpose: gets argument list and options from command line. options may be of the form c:<string> where <c> is the desired option a-z and <string> is a character string associated with that option.
    !     arguments: nargs maximum number of arguments expected (dimension limit on input, actual number on output.
    !                strs  returned strings of arguments and options
    !                iarg  returned list of pointers to elements in strs corresponding to arguments 1..nargs
    !                iopt  returned list of pointers to elements in strs corresponding to options a-z

    character :: strs(nargs)*(*), cmdlin*127, optsep
    integer :: iarg(nargs), iopt(26)

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

    character :: temp*127, cmdlin*(*), chr
    temp = cmdlin
    temp(i1:i2) = cmdlin(i3:i4)
    temp(i5:i5) = chr
    cmdlin = temp
    return

    end subroutine cmove

    SUBROUTINE GETCL(CMDLIN)

    !     routine: getcl
    !     purpose: get command line as a single string
    !     arguments: cmdlin - command line

    use ifport
    use cfin
    implicit none

    character :: cmdlin*127
    integer :: first, last, lpoint, maxarg, iar, i, ic
    logical :: valid

    maxarg = 5 + 2
    lpoint = 0
    iar = iargc()
    if (iar==0) then
        cmdlin = ' '
    else
        cmdlin = ' '
        do i = 1, min(iar,maxarg)
            call getarg(i,lbuf)
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
    end subroutine getcl

    subroutine convrt(coord,first,last,type,i0,x0)

    !     routine: convrt
    !     purpose: convert next entry in string coord to a number of correct type.
    !     arguments: coord  string to be parsed
    !                first  beginning position of substring to be parsed
    !                last   end of substring
    !                type   type of number (1=integer, 2=real)
    !                i0     value if integer
    !                x0     value if real

    use cfin

    character :: coord*(*), decod*20
    integer first, last, type, i0
    real :: x0

    ! get data type
    call datype(coord,first,last,type)
    decod = ' '
    lfirst = min(first,lbufln)
    llast = min(last,first+20,lbufln)
    decod = coord(lfirst:llast)

    ! decode by type
    if (type==1) then
        x0 = rnum(decod)
    else if (type==2) then
        i0 = inum(decod)
    endif

    return
    end subroutine convrt

    integer function inum(strng)

    !     routine: inum
    !     purpose: convert string into an integer
    !     arguments: strng - string containing number to be converted.

    character strng*(*)
    ival = 0
    isgn = 1
    do i = 1, len(strng)
        if (strng(i:i)=='-') isgn = -1
        ic = ichar(strng(i:i)) - ichar('0')
        if (ic>=0.and.ic<=9) ival = ival * 10 + ic
    end do
    inum = isgn * ival

    return
    end function inum

    real function rnum(strng)

    !     routine: rnum
    !     purpose: convert string into an real number
    !     arguments: strng - string containing number to be converted.

    character strng*(*), chr
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
        if (ic>=0.and.ic<=9) rval = rval * 10. + ic

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
                rval = rval * 10. + ic
                if (idec/=0) idec = idec + 1
            endif

            ! if exponent just keep track of it
        else
            if (ic>=0.and.ic<=9) iexp = iexp * 10 + ic
        endif
    endif
    ip = ip + 1
    if (ip<len(strng)) go to 10
    if (idec/=0) idec = idec - 1
    eval = 10. ** (abs(iesgn*iexp-idec))
    iesgn = isign(1,iesgn*iexp-idec)
    if (iesgn==1) rnum = isgn * rval * eval
    if (iesgn==-1) rnum = isgn * rval / eval

    return
    end function rnum

    logical function countargs (label,tocount,lcarray,numc,nret)

    !     routine: countargs
    !     purpose: Count the number of non-blank arguments on the input line. Should be tocount. If not, then return an error (logical false). If tocount is zero or less, just count them
    !     arguments: label - CFAST keyword for this input
    !                tocount - expected number of arguments for this input
    !                lcarray - character array of arguments.  There should be tocount non-blank entries
    !                numc - dimension limit on lcarray
    !                nret - actual number of arguments

    use cparams
    use cshell
    include "precis.fi"

    integer tocount, i,numc,nret
    character lcarray*128(numc), label*5

    countargs = .false.
    nret = 0.

    ! check for the expected number of arguments if tocount >=0
    if (tocount>0) then
        do i = 1, tocount
            if (lcarray(i)==' ') then
                if (i==1.) then
                    write(logerr,5003) label, tocount
                else if (i==2) then
                    write(logerr,5004) label, tocount
                else
                    write(logerr,5000) label, tocount, nret
                endif
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

    subroutine cptime(cputim)

    !     routine: cptime
    !     purpose: routine to calculate amount of computer time (cputim) in seconds used so far.  this routine will generally be different for each computer.
    !     arguments: cputim (output) - elapsed cpu time 

    use ifport

    real*8 cputim
    integer*2 hrs, mins, secs, hsecs
    call gettim(hrs,mins,secs,hsecs)
    cputim = hrs * 3600 + mins * 60 + secs + hsecs / 100.0
    return
    end

    subroutine datype(crd,n1,n2,dtype)

    !     routine: datype
    !     purpose: this routine determines the data type of a string. the character string is examined between given starting and ending positions to determine if the substring is an integer, a real number, or a non-numeric string.
    !     arguments: crd    string containing number to be typed
    !                n1     starting position
    !                n2     ending position
    !                dtype (output) - returned type (1=real, 2=integer, 3=non-numeric)

    include "precis.fi"

    logical period, efmt
    integer n1, n2, dtype
    character crd*(*), num(12)*1
    data num /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-'/
    period = .false.
    efmt = .false.

    ! determine data type.  assume a numeric entry until a non-numeric character is found.
    outer: do i = n1, n2
        if (crd(i:i)=='.') then
            if (period) then

                ! second period in string - non numeric
                dtype = 3
                return
            else
                period = .true.
            endif
            cycle outer
        endif

        ! check for digits
        do j = 1, 12
            if (crd(i:i)==num(j)) cycle outer
        end do
        if (index('EeDd',crd(i:i))==0.or.efmt) then
            dtype = 3
            return
        else 
         efmt = .true.   
        end if
    end do outer

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

    logical function doesthefileexist (checkfile)

    !     routine: doesthefileexist
    !     purpose: checks for the existence of given file name
    !     arguments: checkfile - file name

    implicit none
    
    character (*) checkfile
    logical yesorno

    inquire (file=checkfile, exist=yesorno)
    if (yesorno) then
        doesthefileexist = .true.
    else
        doesthefileexist = .false.
    endif

    return

    end function doesthefileexist

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

    integer(2) n, status, loop, ld(2), li(2), ln(2), le(2), lb
    character(256) buf, xname
    character *(*) exepath, datapath, project
    character (64) name(2)
    logical exists, doesthefileexist

    character(3) drive(2)
    character(256) dir(2)
    character(64) ext(2)
    integer(4) length, errorcode, pathcount, splitpathqq

    n = nargs ()
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
        call getarg (loop, buf, status)

        if(status>0) then
            xname = buf

            !	split out the components
            length = splitpathqq(xname, drive(i), dir(i), name(i), ext(i))
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

    ! now check that the project.in file exists - this is the data file
    buf = ' '
    if (le(2)/=0) then
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // ext(2)(1:le(2)) // '.in'
    else
        buf = drive(2)(1:ld(2)) // dir(2)(1:li(2)) // name(2)(1:ln(2)) // '.in'
    endif

    lb = len_trim(buf)

    ! buf(1:lb) is the data file to check

    if (doesthefileexist(buf(1:lb))) then

        !	the project file exists
        exepath = drive(1)(1:ld(1)) // dir(1)(1:li(1))
        datapath = drive(2)(1:ld(2)) // dir(2)(1:li(2))
        project = name(2)(1:ln(2)) // ext(2)(1:le(2))
        return
    else
        ! note that we do not yet have the logerr file open, so write to the console
        write(*,*) 'the data file does not exist'
        errorcode = 102
    endif
    return

    end subroutine exehandle

    subroutine grabky(ich,it)

    !     routine: grabky
    !     purpose: check for a key press if command line driven
    !     arguments: ich - integer value of character pressed 
    !                it - 2 if a character was pressed, 1 otherwise
    
    use ifport

    character*1 ch, getcharqq
    integer*2 ich, it
    logical peekcharqq

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
    end

    subroutine mat2mult(mat1,mat2,idim,n,matiter)

    !     routine: mat2mult
    !     purpose: given an nxn matrix mat1 whose elements are either 0 or 1, this routine computes the matrix mat1**2 and returns the results in mat1 (after scaling non-zero entries to 1).
    !     arguments: mat1 - matrix
    !                mat2 - work array of same size as mat1
    !                idim - actual dimensino limit on first subscript of mat1
    !                n - size of matrix
    !                matiter - unused

    implicit none
    
    integer :: mat1(idim,n), mat2(idim,n), i, j, idot, idim,n, matiter
    do i = 1, n
        do j = 1, n
            mat2(i,j) = idot(mat1(i,1),idim,mat1(1,j),1,n)
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

    integer function idot(ix,inx,iy,iny,n)

    !     routine: idot
    !     purpose: this routine computes the integer dot product of two integer vectors.
    !     arguments: ix, iy - two integer vectors

    integer ix(*), iy(*)
    idot = 0
    ii = 1 - inx
    jj = 1 - iny
    do i = 1, n
        ii = ii + inx
        jj = jj + iny
        idot = idot + ix(ii)*iy(jj)
    end do
    return
    end function idot

    subroutine indexi(n,arrin,indx)

    !     routine: indexi
    !     purpose: this routines sorts the array arrin passively via the permuation array indx.  the elements arrin(indx(i)), i=1, ..., n are in increasing order. this routine uses a bubble sort.  it should not be used
    !              for large n (n>30), since bubble sorts are not efficient.  
    !     arguments: n     number of elements in n
    !                arrin array to be passively sorted
    !                indx  permuation vector containing ordering such that arrin(indx) is in increasing order.

    integer arrin(*), indx(*), ai, aip1
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
    do i = 2, n-1, 2
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

    subroutine interp(x,y,n,t,icode,yint)

    !     routine: indexi
    !     purpose: routine interpolates a table of numbers found in the arrays, x and y.  
    !     arguments: x,y - arrays of size n to be interpolated at x=t
    !                icode - code to select how to extrapolate values if t is less than x(1) or greater than x(n).
    !                          if icode = 1 then yint = y(1) for t < x(1) and yint = y(n) for t > x(n).
    !                          if icode = 2 then yint is evaluated by interpolation if x(1) < t < x(n) and by extrapolation if t < x(1) or    t > x(n)
    !                yint (output) - interpolated value of the y array at t

    real*8 :: x(*), y(*)
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
    imid = (ia+iz) / 2
    if (t<x(imid)) then
        iz = imid - 1
        go to 10
    endif
    if (t>=x(imid+1)) then
        ia = imid + 1
        go to 10
    endif
20  continue
    dydx = (y(imid+1)-y(imid)) / (x(imid+1)-x(imid))
    yint = y(imid) + dydx * (t-x(imid))
    ilast = imid
    return
    end

    integer function length(string)
    character string*(*)
    if (len(string)/=0) then
        do i=len(string),1,-1
            if (string(i:i)/=' ') then
                length=i
                return
            endif
        end do
    endif
    length=0
    return
    end function length

    subroutine mess(p,z)

    !     routine: mess
    !     purpose: write a literal string of characters.  this is a write to stdout with a return/linefeed at the end 
    !     arguments: phrase - character string to be written
    !                z - length of the string

    use cparams
    use cshell

    integer z, l
    character(len=z) p
    if (z>2048) stop 'error in message handler'
    write (iofilo,'(1x,2048a1)') (p(i:i),i=1,z)
    return

    end subroutine mess

    subroutine messnrf (string, l)

    !     routine: messnrf
    !     purpose: write a literal string of characters.  this is a write to stdout with a return/linefeed at the end 
    !     arguments: phrase - character string to be written
    !                l - length of the string
    use cparams
    use cshell

    character string*(*), formatt*100
    write (formatt,'(''(1x,a'',i2.2,'',$)'')') l
    write (iofilo,formatt) string(1:l)
    return

    end

    subroutine readastu (in, count, start, max, valid)

    !     routine: readastu
    !     purpose: read in a string from the standard input device
    !              this version of readas(tu) is similar to the one used by everone else (readas) except that this contains automatic conversion to upper case. this is to filter commands from the console so that they are not case sensitive.
    !     arguments: 

    use cparams
    use cshell
    include "precis.fi"

    integer start, count
    logical valid
    character ch,inn*256,in(max),c

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
    do i = 1,count
        ich = ichar(inn(i:i))
        if (ich<32.or.ich>125) exit
        nc = i
    end do
    count = nc

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

    subroutine readcv1 (in,count,start,ix,xi,type,valid)

    !     routine: readastu
    !     purpose: string conversion to integers or real numbers
    !     arguments: 

    integer start,first,last,count,type
    logical valid
    character*128 in
    real xi

    call sstrngp (in,count,start,first,last,valid)
    if (valid) then
        call convrt (in,first,last,type,ix,xi)
        count = count - (last-start+1)
        start = last + 1
    endif

    return
    end subroutine readcv1

    subroutine readin (nreq, nret, fixed, flting)

    !     routine: readin
    !     purpose: read in a string and process it into the integer and floating variables "fixed" and "fltng"
    !     arguments: nreq - number of values required
    !                nret - actual number of values
    !                fixed - integer numbers returned
    !                flting - floting point numbers returned

    use cfin
    use cfio
    use cparams
    use cshell
    include "precis.fi"

    dimension flting(*)
    integer fixed(*)
    real x0, xxbig
    logical multi, eof
    character label*5, lable*5, slash*1, file*(*)
    save input, lstart

    data input/0/, lstart/0/, slash/'/'/

    nret = 0
    multi = .false.
    xxbig = 10000000.0d0
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
                exit
            endif
        end do
        llast = last
        call convrt (inbuf, first, llast, type, i0, x0)
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
    write(logerr,'(''Label = '',a5)') label
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
    end subroutine readin

      SUBROUTINE READAS(INFILE, INBUF, COUNT, START, VALID)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READAS
C
C     Functional Class:  UTILITY
C
C     Description:  Read in a string from the input file, filtering out
C                   comments
C
C     Arguments: INFILE
C                INBUF
C                COUNT
C                START
C                VALID
C
C     Revision History:
C        5/16/1991 by WWJ, include the precision file PRECIS.INC
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C
      use cparams
      use cshell
      include "precis.fi"
C
C     READ IN A STRING
C
      INTEGER START, COUNT, IREC
      CHARACTER INBUF*(*), CMT1*1, CMT2*1, frmt*30
      LOGICAL VALID
      DATA CMT1/'!'/, CMT2/'#'/, IREC/0/
      SAVE IREC
C
C     IF WE HAVE REACHED AN END OF FILE, RETURN NOTHING
C
10    INBUF = ' '
      IF (IREC==0) THEN
         READ(INFILE,'(a)',END=2,ERR=2) INBUF
      ELSE
         READ(INFILE,'(a)',REC=IREC,ERR=2) INBUF
         IREC = IREC + 1
      endif
	ls = len_trim (inbuf)
	write (frmt, 9) ls
      WRITE (LOGERR,frmt) INBUF
    9 format('(''Buffer input = '',A',i3,')')

C     FILTER COMMENTS

      IF (INBUF(1:1)==CMT1.OR.INBUF(1:1)==CMT2) GO TO 10

C     OK

      COUNT = LEN(INBUF)
      START = 1
      VALID = .TRUE.
      RETURN
2     VALID = .FALSE.
      COUNT = 0
      START = 1
      WRITE(LOGERR,*) 'END OF FILE FOR UNIT ',INFILE
      RETURN
      END subroutine readas
      
      SUBROUTINE READOP
      use ifport

C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     READOP
C
C     Description:  retrieve and process command line options and date
C
C     Arguments: none
C
C     Revision History:
C        Written:  7/26/1990 by WWJ: implement date on UNIX and PC
C        Modified: 2/8/1993 RDP:
C                  Rewritten to to command parsing format to remove bug when 
C                  options and filename were not separated with a space.
C
C        Modified: 10/21/97 GPF:
C                  added -s option to create a "backdoor" solver input file
C                  aka SOLVER.INI .  note that the default name assigned by
C                  this option is SOLVE.INI not SOLVER.INI so as to not overwrite
C                  this file if already present.
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
!
!	Unit numbers defined in readop, openoutputfiles, readinputfiles
!
!      1 is for the solver.ini and data files (data file, tpp and objects) (IOFILI)
!      3 is for the log file  (LOGERR)
!      6 is output (IOFILO)
!     11 is the history file
!     12 is used to write the status file (project.status)
!     13 smokeview output
!     14 spreadsheet output
!

      use cparams
      use cshell

      INTEGER (2) YEAR, MONTH, DAY
      LOGICAL EXISTS
      CHARACTER STRS(8)*60, IC, TOUPPER*1, LOGFILE*60
      CHARACTER*60 SOLVEINI
      INTEGER IARG(8), IOPT(26), OPTION
      cmdflag(IC) = IOPT(ICHAR(IC)-ICHAR('A')+1)

C     CURRENT DATE

      CALL GETDAT(YEAR,MONTH,DAY)
      RUNDAT(3) = DAY
      RUNDAT(2) = MONTH
      RUNDAT(1) = YEAR
      WRITE (MPSDATC,5010) RUNDAT(1), RUNDAT(2), RUNDAT(3)

C     COMMAND LINE ARGUMENTS

      NARGS = 8
      CALL CMDLINE(NARGS,STRS,IARG,IOPT)

!     OPTIONS
!     K = DO NOT ACCESS KEYBOARD
!     F/C = OUTPUT OPTIONS
!     S = Output "SOLVER.INI" options into the file SOLVE.INI
!	I = do initialization only
!     H to include the header in the output file
!     D to turn on debugging writes
!     T to output trace species mass
!     V to output target fluxes relative to an ambient target (incident flux - sigma*eps*Tamb**4) and smoke in mg/m^3
!     N to output just target fluxes relative to ambient (smoke still in OD)

      IF (cmdflag('H')/=0) HEADER = .TRUE.
      IF (cmdflag('K')/=0) NOKBD = .TRUE.
	IF (cmdflag('I')/=0) INITIALIZEONLY = .TRUE.
	IF (cmdflag('D')/=0) DEBUGGING = .TRUE.
	if (cmdflag('T')/=0) trace = .true.
	if (cmdflag('V')/=0) validate = .true.
      if (cmdflag('N')/=0) netheatflux = .true.
      LOGERR = 3

      IF (cmdflag('F')/=0.and.cmdflag('C')/=0) stop 107
	if (cmdflag('C')/=0) outputformat = 1
	if (cmdflag('F')/=0) outputformat = 2

      IF (cmdflag('S')/=0) THEN
         IF (STRS(cmdflag('S'))/=' ') THEN
            SOLVEINI = STRS(cmdflag('S'))
         ELSE
            SOLVEINI = 'SOLVE.INI'
         ENDIF
         CALL WRITEINI(SOLVEINI)
      ENDIF

      RETURN

 5010 FORMAT (I4.4,'/',I2.2,'/',I2.2)
      END
	 subroutine shellsort (ra, n)

	 implicit none
	 integer n, j, i, inc
	 real*8 ra(n), rra
	 
 	 inc = 1
1	 inc = 3*inc+1
	 if (inc<=n) go to 1
2	 continue
		  inc = inc / 3
		  do i = inc+1, n
				rra = ra(i)
				j = i
3				if(ra(j-inc)>rra) then
					 ra(j) = ra(j-inc)
					 j = j - inc
					 if(j<=inc) go to 4
				go to 3
				endif
4				ra(j) = rra
		  enddo
	 if(inc>1) go to 2
	 return
	 end
      SUBROUTINE SORTBRM(
     I                   X,LX,IX,LIX,NROW,NCOLX,NCOLIX,ISORT,LDP,NROOM,
     O                   IPOINT)

C     Function: Sort the two arrays X and IX by the ISORT'th column of
C               IX which contains room data.  This routine is based on the
C               now obsolete routine SORTFR.  This routine is used
C               to sort fire and detector data structures by room 
C               number.
C
C     Inputs:   X       Floating point info to be sorted
C               LX      Leading dimension of X 
C               IX      Integer info to be sorted
C               LIX     Leading dimension of IX in calling routine
C               NROW    Number of rows in X and IX
C               NCOLX   Number of columns in X
C               NCOLIX  Number of columns in IX
C               ISORT   Column in IX to sort on (usually contains room numbers)
C               LDP     Leading dimension of IPOINT
C               NROOM   number of elements for which ipoint is defined, also
C                       the number of rooms
C
C     Outputs:  IPOINT  Pointer array for sorted X and IX list.
C                       (r,1) = Number of items (fires or detectors so far)
C                               in room r
C                       (r,2) = Pointer to beginning element in IX
C                               and X for fire or detector in room r
C
C     Revision History:
C        Created: 9/3/93 by GPF:
!	   Modified 10/31/2006 by WWJ
!
!				 SORTBRM is used to sort the detector information.
!
C                  This routine was created using SORTFR as a template.
C                  SORTFR was designed specifically to sort fire data 
C                  structures.  This routine is more general.  It is designed
C                  to sort any floating point and integer data structures
C                  by the data given in the ISORT'th colum of IX (usually
C                  room numbers).

      include "precis.fi"
      DIMENSION X(LX,NCOLX), IX(LIX,NCOLIX)
C  
C    IF THE NUMBER OF FIRES, DETECTORS OR ROOMS EVER EXCEEDS 200 THEN 
C    THE FOLLOWING DIMENSION STATEMENT NEEDS TO BE CHANGED
C
      PARAMETER (LWORK=100)
      DIMENSION IPOINT(LDP,*), WORK(LWORK), IWORK(LWORK), IPERM(LWORK)
C
C     CREATE A PERMUTATION VECTOR USING THE ISORT'TH COLUMN OF IX
C
      IF(NROW>LWORK)THEN
              CALL XERROR('NOT ENOUGH WORK SPACE IN SORTBRM',
     .                    0,1,2)
      ENDIF
      DO 2 I = 1, NROW
    2    IPERM(I) = I
      CALL INDEXI(NROW,IX(1,ISORT),IPERM)
C
C     REORDER INTEGER ARRAY USING THE PERMUTATION VECTOR
C
      DO 5 J = 1, NCOLIX
        DO 10 I = 1, NROW
          IWORK(I) = IX(IPERM(I),J)
   10   CONTINUE
        DO 20 I = 1, NROW
          IX(I,J) = IWORK(I)
   20   CONTINUE
    5 CONTINUE
C
C    REORDER THE FLOATING POINT ARRAYS USING THE PERMUTATION VECTOR
C
      DO 50 J = 1, NCOLX
        DO 30 I = 1, NROW
          WORK(I) = X(IPERM(I),J)
   30   CONTINUE
        DO 40 I = 1, NROW
          X(I,J) = WORK(I)
   40   CONTINUE
   50 CONTINUE
C
C     CONSTRUCT THE POINTER ARRAY
C
      DO 60 I = 1, NROOM
        IPOINT(I,1) = 0
        IPOINT(I,2) = 0
   60 CONTINUE
      DO 70 I = 1, NROW
        IROOM = IX(I,ISORT)
        IPOINT(IROOM,1) = IPOINT(IROOM,1) + 1
        IF (IPOINT(IROOM,2)==0) IPOINT(IROOM,2) = I
   70 CONTINUE
      DO 80 I = 1, NROOM
        IF (IPOINT(I,2)==0) IPOINT(I,2) = 1
   80 CONTINUE
      RETURN
      END
      SUBROUTINE SORTFR(NFIRE,IFROOM,XFIRE,IFRPNT,NM1)

C     Function: Sort the two arrays IFROOM and XFIRE into increasing
C               room number in IFROOM.  These are used in this order
C               by the ceiling jet and radiation algorithms
C
C     Inputs:   NFIRE   Number of fires
C     Outputs:  IFROOM  Room numbers for each of the fires
C               XFIRE   Fire related quantities used by other routines.
C                       See routine FIRES for definition.
C               IFRPNT  Pointer array for sorted fire list.
C                       (r,1) = Number of fires in room r
C                       (r,2) = Pointer to beginning element in IFROOM
C                               and XFIRE for fires in room r
C
C     Revision History:
C        Modified: 2/7/93 by GPF:
C                  The radiation routines expect to receive info for each
C                  fire in a room.  Therefore, XFIRE a 2-d array must have 
C                  the number of fires as the first subscript.

      use cparams
      include "precis.fi"
      DIMENSION IFROOM(MXFIRE), XFIRE(MXFIRE,MXFIRP), IPERM(MXFIRE), 
     +    IWORK(MXFIRE), WORK(MXFIRE), IFRPNT(NR,2)

C     CREATE A PERMUTATION VECTOR FROM THE LIST OF FIRE ROOMS WHICH IS 
C     ORDERED BY INCREASING ROOM NUMBER

      DO 2 I = 1, NFIRE
    2    IPERM(I) = I
      CALL INDEXI(NFIRE,IFROOM,IPERM)
C
C     REORDER THE TWO ARRAYS WITH THE PERMUTATION VECTOR
C
      DO 10 I = 1, NFIRE
        IWORK(I) = IFROOM(IPERM(I))
   10 CONTINUE
      DO 20 I = 1, NFIRE
        IFROOM(I) = IWORK(I)
   20 CONTINUE
C
      DO 50 J = 1, MXFIRP
        DO 30 I = 1, NFIRE
          WORK(I) = XFIRE(IPERM(I),J)
   30   CONTINUE
        DO 40 I = 1, NFIRE
          XFIRE(I,J) = WORK(I)
   40   CONTINUE
   50 CONTINUE
C
C     DO THE POINTER ARRAYS FOR THE RADIATION AND CEILING JET ROUTINES
C
      DO 60 I = 1, NM1
        IFRPNT(I,1) = 0
        IFRPNT(I,2) = 0
   60 CONTINUE
      DO 70 I = 1, NFIRE
        IRM = IFROOM(I)
        IFRPNT(IRM,1) = IFRPNT(IRM,1) + 1
        IF (IFRPNT(IRM,2)==0) IFRPNT(IRM,2) = I
   70 CONTINUE
      DO 80 I = 1, NM1
        IF (IFRPNT(I,2)==0) IFRPNT(I,2) = 1
   80 CONTINUE
      RETURN
      END
      SUBROUTINE SSTRNG(STRING,WCOUNT,SSTART,SFIRST,SLAST,SVALID)
C
C  THIS ROUTINE FINDS POSITIONS OF SUBSTRINGS WITHIN A CHARACTER
C  STRING.  A SPACE, COMMA, - , (, OR ) INDICATES THE BEGINNING OR
C  END OF A SUBSTRING.  WHEN CALLED, THE STRING IS PASSED
C  AS AN INTEGER(CHOOSE) ALONG WITH THE NUMBER OF CHARACTERS IN THE
C  STRING(WCOUNT) AND A STARTING POSITION(SSTART).  BEGINNING AT
C  "SSTART", THE ROUTINE SEARCHES FOR A SUBSTRING. IF A SUBSTRING IS
C  FOUND, ITS FIRST AND LAST CHARACTER POSITIONS ARE RETURNED ALONG
C  WITH A TRUE VALUE IN "SVALID"; OTHERWISE "SVALID" IS SET FALSE.
C
      LOGICAL SVALID
      INTEGER SFIRST, SLAST, SSTART, WCOUNT, ENDSTR
      CHARACTER*1 STRING(*), SPACE, COMMA
C
      DATA SPACE/' '/, COMMA/','/
C
      SVALID = .TRUE.
C
C  INVALID STARTING POSITION - PAST END OF STRING
C
      ENDSTR = SSTART + WCOUNT - 1
C
C  FIND POSITION OF FIRST ELEMENT OF SUBSTRING
C
      DO 20 I = SSTART, ENDSTR
C
C     MOVE TO THE BEGINNING OF THE SUBSTRING
C
      SFIRST = I
      IF((STRING(I)/=SPACE).AND.(STRING(I)/=COMMA)) GOTO 60
20    CONTINUE
C
C  NO SUBSTRING FOUND - ONLY DELIMITER
C
      GO TO 40
C
C  FIND POSITION OF LAST CHARACTER OF SUBSTRING
C
60    DO 50 J = SFIRST, ENDSTR
C
C  POSITION OF LAST ELEMENT OF SUBSTRING
C
      SLAST = J-1
      IF((STRING(J)==SPACE).OR.(STRING(J)==COMMA)) GO TO 100
50    CONTINUE
C
C  NO SUBSTRING DELIMITER => LAST CHARACTER OF SUBSTRING IS THE
C  LAST CHARACTER OF THE STRING
C
      SLAST = ENDSTR
      RETURN
C
C  NO SUBSTRING FOUND
C
40    SVALID = .FALSE.
100   RETURN
      END
      SUBROUTINE SSTRNGP (STRING,WCOUNT,SSTART,SFIRST,SLAST,SVALID)
C
C   THIS ROUTINE FINDS POSITIONS OF SUBSTRINGS WITHIN A CHARACTER
C   STRING.  A SPACE, COMMA, - , (, OR ) INDICATES THE BEGINNING OR
C   END OF A SUBSTRING.  WHEN CALLED, THE STRING IS PASSED
C   AS AN INTEGER(CHOOSE) ALONG WITH THE NUMBER OF CHARACTERS IN THE
C   STRING(WCOUNT) AND A STARTING POSITION(SSTART).  BEGINNING AT
C   "SSTART", THE ROUTINE SEARCHES FOR A SUBSTRING. IF A SUBSTRING IS
C   FOUND, ITS FIRST AND LAST CHARACTER POSITIONS ARE RETURNED ALONG
C   WITH A TRUE VALUE IN "SVALID"; OTHERWISE "SVALID" IS SET FALSE.
C   SIMILAR TO SSTRNG, EXCEPT ENTRIES CAN BE GROUPED WITH PARENTHESIS
C
      LOGICAL SVALID
      INTEGER SFIRST,SLAST,SSTART,WCOUNT,ENDSTR
      CHARACTER*1 STRING(128),SPACE,COMMA,RPAREN,LPAREN
      DATA SPACE /' '/, COMMA /','/,RPAREN /')'/, LPAREN /'('/
c
      SVALID = .TRUE.
      ENDSTR = SSTART + WCOUNT - 1
C
C  FIND POSITION OF FIRST ELEMENT OF SUBSTRING
C
      DO 20 I = SSTART, ENDSTR
C
C   MOVE TO THE BEGINNING OF THE SUBSTRING
C
      SFIRST = I
      IF ((STRING(I)/=SPACE).AND.(STRING(I)/=COMMA).AND.
     2 (STRING(I)/=RPAREN).AND.(STRING(I)/=LPAREN)) GO TO 60
20    CONTINUE
C
C  NO SUBSTRING FOUND - ONLY DELIMITER
C
      GO TO 40
C
C  FIND POSITION OF LAST CHARACTER OF SUBSTRING
C
60    DO 50 J = SFIRST, ENDSTR
C
C  POSITION OF LAST ELEMENT OF SUBSTRING
C
      SLAST = J-1
      IF ((STRING(J)==SPACE).OR.(STRING(J)==COMMA).OR.
     . (STRING(J)==RPAREN).OR.(STRING(J)==LPAREN)) GO TO 100
50    CONTINUE
C
C  NO SUBSTRING DELIMITER => LAST CHARACTER OF SUBSTRING IS THE
C  LAST CHARACTER OF THE STRING
C
      SLAST = ENDSTR
      RETURN
C
C  NO SUBSTRING FOUND
C
40    SVALID = .FALSE.
      RETURN
100   IF (SLAST<SFIRST) SVALID = .FALSE.
      END
      CHARACTER FUNCTION TOUPPER(CH)
      CHARACTER*1 CH, TOLOWER
C
C     CONVERT TO UPPER CASE
C
      ICH = ICHAR(CH)
      IF (ICH>96.AND.ICH<123) ICH = ICH - 32
      TOUPPER = CHAR(ICH)
      RETURN
C
C     COVERT TO LOWER CASE
C
      ENTRY TOLOWER(CH)
      ICH = ICHAR(CH)
      IF (ICH>64.AND.ICH<91) ICH = ICH + 32
      TOLOWER = CHAR(ICH)
      RETURN
      END
      SUBROUTINE UPPERALL(FROM,TO)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     UPPERALL
C
C     Source File: UPPERALL.SOR
C
C     Functional Class:  
C
C     Description:  Convert a character string to upper case
C
C     Arguments: FROM
C                TO
C
C     Revision History:
C        Created:  5/5/1995 at 13:51 by GPF
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      CHARACTER*(*) FROM, TO
      CHARACTER*1 C
      EXTERNAL LENGTH
      NFROM = LENGTH (FROM)
      NTO = LEN(TO)
      NN = MIN(NFROM,NTO)
      DO 10 I = 1, NN
         C = FROM(I:I)
         IF(C>='a'.AND.C<='z')THEN
            C = CHAR(ICHAR(C) + ICHAR('A')-ICHAR('a'))
         ENDIF
         TO(I:I) = C
   10 CONTINUE
      IF(NTO>NN)TO(NN+1:NTO)=' '
      RETURN
      END
      INTEGER FUNCTION FUNIT (IO)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     FUNIT
C
C     Source File: UTILITY.SOR
C
C     Functional Class: UTILTY
C
C     Description: finds first avalable unit starting at IO
C
C     Arguments: IO
C
C     Revision History:
C        Created:  12/3/1992 at 10:37 by PAR
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------
C

      PARAMETER(MXIO=32767)
      LOGICAL OPEND

      ITMP = IO-1
   10 CONTINUE
      ITMP = ITMP+1
      IF (ITMP>MXIO) THEN
         FUNIT = -1
         RETURN
      endif
      INQUIRE(UNIT=ITMP,OPENED = OPEND)
      IF (OPEND) GO TO 10
      FUNIT = ITMP
      RETURN
      END

      SUBROUTINE OPNOTPT (FILNAME, IOUNIT)
C
C--------------------------------- NIST/BFRL ---------------------------------
C
C     Routine:     OPNOTPT
C
C     Source File: UTILITY.SOR
C
C     Functional Class: UTILITY
C
C     Description: opens a file using the extension to distinguish previous 
C                  open files
C
C     Arguments: FILNAME
C                IOUNIT
C
C     Revision History:
C        Created:  12/3/1992 at 9:37 by PAR
C        Modified:  2/12/1993 by GPF to use FNDBLANK routine.
C                   Necessary because UNIX treated leading blanks
C                   as significant   
C
C---------------------------- ALL RIGHTS RESERVED ----------------------------

      CHARACTER FILNAME*(*)
      INTEGER IOUNIT, FIRST, LAST
      LOGICAL EXISTED, VALID
      CHARACTER NAMEFIL*60, WORKFIL*60, FMT*14

      LENGTH = LEN (FILNAME)
      CALL SSTRNG (FILNAME, LENGTH, 1, FIRST, LAST, VALID)
      IF (.NOT.VALID) STOP 'CANNOT OPEN DEBUG FILE'
      ILEN = LAST - FIRST + 1
      NAMEFIL = ' '
      NAMEFIL(1:ILEN) = FILNAME(FIRST:LAST)

      ITMP = 0
   30 CONTINUE
      ITMP = ITMP + 1
      WRITE(FMT,10) ILEN
   10 FORMAT('(A',I2.2,',',1H','.',1H',',I3.3)')
      WRITE(WORKFIL,FMT) NAMEFIL, ITMP
      INQUIRE (FILE = WORKFIL, EXIST = EXISTED)
      IF (EXISTED) GO TO 30
      OPEN (UNIT = IOUNIT, FILE = WORKFIL,RECL=255)
      RETURN
      END
      SUBROUTINE XERBLA ( SRNAME, INFO )
*     ..    Scalar Arguments ..
      INTEGER            INFO
      CHARACTER*6        SRNAME
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the Level 2 BLAS routines.
*
*  It is called by the Level 2 BLAS routines if an input parameter is
*  invalid.
*
*  Installers should consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Parameters
*  ==========
*
*  SRNAME - CHARACTER*6.
*           On entry, SRNAME specifies the name of the routine which
*           called XERBLA.
*
*  INFO   - INTEGER.
*           On entry, INFO specifies the position of the invalid
*           parameter in the parameter-list of the calling routine.
*
*
*  Auxiliary routine for Level 2 Blas.
*
*  Written on 20-July-1986.
*
*     .. Executable Statements ..
*
      WRITE (*,99999) SRNAME, INFO
*
      STOP
*
99999 FORMAT ( ' ** On entry to ', A6, ' parameter number ', I2,
     $         ' had an illegal value' )
*
*     End of XERBLA.
*
      END

      LOGICAL FUNCTION LSAME ( CA, CB )
*     .. Scalar Arguments ..
      CHARACTER*1            CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME  tests if CA is the same letter as CB regardless of case.
*  CB is assumed to be an upper case letter. LSAME returns .TRUE. if
*  CA is either the same as CB or the equivalent lower case letter.
*
*  N.B. This version of the routine is only correct for ASCII code.
*       Installers must modify the routine for other character-codes.
*
*       For EBCDIC systems the constant IOFF must be changed to -64.
*       For CDC systems using 6-12 bit representations, the system-
*       specific code in comments must be activated.
*
*  Parameters
*  ==========
*
*  CA     - CHARACTER*1
*  CB     - CHARACTER*1
*           On entry, CA and CB specify characters to be compared.
*           Unchanged on exit.
*
*
*  Auxiliary routine for Level 2 Blas.
*
*  -- Written on 20-July-1986
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, Nag Central Office.
*
*     .. Parameters ..
      INTEGER                IOFF
      PARAMETER            ( IOFF=32 )
*     .. Intrinsic Functions ..
      INTRINSIC              ICHAR
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA == CB
*
*     Now test for equivalence
*
      IF ( .NOT.LSAME ) THEN
         LSAME = ICHAR(CA) - IOFF == ICHAR(CB)
      endif
*
      RETURN
*
*  The following comments contain code for CDC systems using 6-12 bit
*  representations.
*
*     .. Parameters ..
*     INTEGER                ICIRFX
*     PARAMETER            ( ICIRFX=62 )
*     .. Scalar Arguments ..
*     CHARACTER*1            CB
*     .. Array Arguments ..
*     CHARACTER*1            CA(*)
*     .. Local Scalars ..
*     INTEGER                IVAL
*     .. Intrinsic Functions ..
*     INTRINSIC              ICHAR, CHAR
*     .. Executable Statements ..
*
*     See if the first character in string CA equals string CB.
*
*     LSAME = CA(1) == CB .AND. CA(1) /= CHAR(ICIRFX)
*
*     IF (LSAME) RETURN
*
*     The characters are not identical. Now check them for equivalence.
*     Look for the 'escape' character, circumflex, followed by the
*     letter.
*
*     IVAL = ICHAR(CA(2))
*     IF (IVAL>=ICHAR('A') .AND. IVAL<=ICHAR('Z')) THEN
*        LSAME = CA(1) == CHAR(ICIRFX) .AND. CA(2) == CB
*     endif
*
*     RETURN
*
*     End of LSAME.
*
      END
      integer function rev_auxilliary
          
      INTEGER :: MODULE_REV
      CHARACTER(255) :: MODULE_DATE 
      CHARACTER(255), PARAMETER :: 
     * mainrev='$Revision$'
      CHARACTER(255), PARAMETER :: 
     * maindate='$Date$'
      
      WRITE(module_date,'(A)') 
     *    mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
      READ (MODULE_DATE,'(I5)') MODULE_REV
      rev_auxilliary = module_rev
      WRITE(MODULE_DATE,'(A)') maindate
      return
      end function rev_auxilliary