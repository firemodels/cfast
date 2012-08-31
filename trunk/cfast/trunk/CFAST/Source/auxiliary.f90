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

    SUBROUTINE CMDLINE (NARGS,STRS,IARG,IOPT)

    !     routine: cmdline
    !     purpose: gets argument list and options from command line. options may be of the form c:<string> where <c> is the desired option a-z and <string> is a character string associated with that option.
    !     arguments: nargs maximum number of arguments expected (dimension limit on input, actual number on output.
    !                strs  returned strings of arguments and options
    !                iarg  returned list of pointers to elements in strs corresponding to arguments 1..nargs
    !                iopt  returned list of pointers to elements in strs corresponding to options a-z

    CHARACTER STRS(NARGS)*(*), CMDLIN*127, OPTSEP
    INTEGER IARG(NARGS), IOPT(26)

    OPTSEP = '/'

    DO 10 IC = 1, MAX0(NARGS,26)
        IF (IC<=NARGS) THEN
            STRS(IC) = ' '
            IARG(IC) = 0
        endif
        IOPT(IC) = 0
10  CONTINUE

    ! GET THE COMMAND LINE TO DECIPHER
    CALL GETCL(CMDLIN)
    IF (CMDLIN/=' ') THEN

        ! GET RID OF EXTRA SPACES IN THE COMMAND LINE
        IC = 1
20      IF (CMDLIN(IC:IC+1)=='  ') THEN
            CALL CMOVE(CMDLIN,IC,126,IC+1,127,127,' ')
        ELSE
            IC = IC + 1
        endif
        IF (CMDLIN(IC:127)/=' '.AND.IC<=126) GO TO 20
        IF (CMDLIN(1:1)==' ') THEN
            CALL CMOVE(CMDLIN,1,126,2,127,127,' ')
        endif

        ! PUT IN COMMAS WHERE APPROPRIATE TO DELIMIT ALL FIELDS
        IC = 2
30      IF (CMDLIN(IC:IC)==' ') THEN
            IF (CMDLIN(IC-1:IC-1)/=','.AND.CMDLIN(IC+1:IC+1)/=',') THEN
                CMDLIN(IC:IC) = ','
                IC = IC + 1
            ELSE
                CALL CMOVE(CMDLIN,IC,126,IC+1,127,127,' ')
            endif
        ELSE IF ((CMDLIN(IC:IC)==OPTSEP).AND.CMDLIN(IC-1:IC-1)/=',') THEN
            CALL CMOVE(CMDLIN,IC+1,127,IC,126,IC,',')
            IC = IC + 2
        ELSE
            IC = IC + 1
        endif
        IF (CMDLIN(IC:127)/=' '.AND.IC<=126) GO TO 30
    endif

    ! PARSE COMMAND LINE INTO SEPARATE FIELDS AND PROCESS OPTIONS
    IA = 0
40  IC = INDEX(CMDLIN,',')
    IF (IC==0.AND.CMDLIN/=' ') IC = INDEX(CMDLIN,' ')
    IF (IC/=0) THEN
        IA = IA + 1
        STRS(IA) = ' '
        IF (IC>1) STRS(IA) = CMDLIN(1:IC-1)
        CALL CMOVE(CMDLIN,1,127,IC+1,127,127,' ')
        GO TO 40
    endif

    ! ASSIGN THE PARSED FIELDS TO APPROPRIATE ARGUMENTS AND OPTIONS
    NARGS = 0
    IF (IA>0) THEN
        DO 50 I = 1, IA
            IF (STRS(I)(1:1)==OPTSEP) THEN
                IF (STRS(I)(2:2)>='A'.AND.STRS(I)(2:2)<='Z') THEN
                    IOPT(ICHAR(STRS(I)(2:2))-ICHAR('A')+1) = I
                ELSE IF (STRS(I)(2:2)>='a'.AND.STRS(I)(2:2)<='z') THEN
                    IOPT(ICHAR(STRS(I)(2:2))-ICHAR('a')+1) = I
                endif
                CMDLIN = STRS(I)
                CALL CMOVE(CMDLIN,1,127,3,127,127,' ')
                IF (CMDLIN(1:1)==':') CALL CMOVE(CMDLIN,1,127,2,127,127,' ')
                STRS(I) = CMDLIN
            ELSE
                NARGS = NARGS + 1
                IARG(NARGS) = I
            endif
50      CONTINUE
    endif
    RETURN
    END

    SUBROUTINE CMOVE(CMDLIN,I1,I2,I3,I4,I5,CHR)

    !     routine: cmove
    !     purpose: move a substring in the command line to remove spaces.
    !     arguments: cmdlin command line string
    !                i1     beginning of substring to be moved
    !                i2     end of substring to be moved
    !                i3     beginning of destination
    !                i4     end of destination
    !                i5     position of newly vacated space in the string
    !                chr    character to fill that space

    CHARACTER TEMP*127, CMDLIN*(*), CHR
    TEMP = CMDLIN
    TEMP(I1:I2) = CMDLIN(I3:I4)
    TEMP(I5:I5) = CHR
    CMDLIN = TEMP
    RETURN
    END

    SUBROUTINE GETCL(CMDLIN)

    !     routine: getcl
    !     purpose: get command line as a single string
    !     arguments: cmdlin - command line

    use ifport
    use cfin

    CHARACTER CMDLIN*127
    INTEGER FIRST, LAST, LPOINT
    LOGICAL VALID

    MAXARG = 5 + 2
    LPOINT = 0
    IAR = IARGC()
    IF (IAR==0) THEN
        CMDLIN = ' '
    ELSE
        CMDLIN = ' '
        DO 10 I = 1, MIN(IAR,MAXARG)
            CALL GETARG(I,LBUF)
            CALL SSTRNG(LBUF,60,1,FIRST,LAST,VALID)
            IF (VALID) THEN
                IC = LAST - FIRST + 1
                LPOINT = LPOINT + 1
                CMDLIN(LPOINT:LPOINT+IC) = LBUF(FIRST:LAST)
                LPOINT = LPOINT + IC
            endif
10      CONTINUE
    endif
    RETURN
    END


    SUBROUTINE CONVRT(COORD,FIRST,LAST,TYPE,I0,X0)

    !     routine: convrt
    !     purpose: convert next entry in string coord to a number of correct type.
    !     arguments: coord  string to be parsed
    !                first  beginning position of substring to be parsed
    !                last   end of substring
    !                type   type of number (1=integer, 2=real)
    !                i0     value if integer
    !                x0     value if real

    use cfin

    CHARACTER COORD*(*)
    CHARACTER*20 DECOD
    INTEGER FIRST, LAST, TYPE, I0
    REAL X0

    ! GET DATA TYPE
    CALL DATYPE(COORD,FIRST,LAST,TYPE)
    DECOD = ' '
    LFIRST = MIN(FIRST,LBUFLN)
    LLAST = MIN(LAST,FIRST+20,LBUFLN)
    DECOD = COORD(LFIRST:LLAST)

    ! DECODE BY TYPE
    IF (TYPE==1) THEN
        X0 = RNUM(DECOD)
    ELSE IF (TYPE==2) THEN
        I0 = INUM(DECOD)
    endif
    RETURN
    END
    INTEGER FUNCTION INUM(STRNG)

    !     routine: inum
    !     purpose: convert string into an integer
    !     arguments: strng - string containing number to be converted.

    CHARACTER STRNG*(*)
    IVAL = 0
    ISGN = 1
    DO 10 I = 1, LEN(STRNG)
        IF (STRNG(I:I)=='-') ISGN = -1
        IC = ICHAR(STRNG(I:I)) - ICHAR('0')
        IF (IC>=0.AND.IC<=9) IVAL = IVAL * 10 + IC
10  CONTINUE
    INUM = ISGN * IVAL
    RETURN
    END

    REAL FUNCTION RNUM(STRNG)

    !     routine: rnum
    !     purpose: convert string into an real number
    !     arguments: strng - string containing number to be converted.

    CHARACTER STRNG*(*), CHR
    RVAL = 0.0
    ISGN = 0
    IDEC = 0
    IESGN = 0
    IEXP = 0
    IP = 1
10  CHR = STRNG(IP:IP)

    ! FIRST COMES A SIGN OR MANTISSA
    IC = ICHAR(CHR) - ICHAR('0')
    IF (ISGN==0) THEN
        ISGN = 1
        IF (CHR=='-') ISGN = -1
        IF (CHR=='.') IDEC = 1
        IF (IC>=0.AND.IC<=9) THEN
            RVAL = RVAL * 10. + IC
        endif

        ! IF WE'VE FOUND THE MANTISSA, CHECK FOR EXPONENT
    ELSE IF (CHR=='E'.OR.CHR=='e'.OR.CHR=='D'.OR.CHR=='d'.OR.CHR=='+'.OR.CHR=='-') THEN
        IESGN = 1
        IF (CHR=='-') IESGN = -1
        ELSE

        ! IF NO EXPONENT, KEEP TRACK OF DECIMAL POINT
        IF (IESGN==0) THEN
        IF (CHR=='.') THEN
        IDEC = 1
    ELSE IF (IC>=0.AND.IC<=9) THEN
        RVAL = RVAL * 10. + IC
        IF (IDEC/=0) IDEC = IDEC + 1
    endif

    ! IF EXPONENT JUST KEEP TRACK OF IT
    ELSE
        IF (IC>=0.AND.IC<=9) IEXP = IEXP * 10 + IC
    endif
    endif
    IP = IP + 1
    IF (IP<LEN(STRNG)) GO TO 10
    IF (IDEC/=0) IDEC = IDEC - 1
    EVAL = 10. ** (ABS(IESGN*IEXP-IDEC))
    IESGN = ISIGN(1,IESGN*IEXP-IDEC)
    IF (IESGN==1) RNUM = ISGN * RVAL * EVAL
    IF (IESGN==-1) RNUM = ISGN * RVAL / EVAL
    RETURN
    END

    logical function countargs (label,tocount,lcarray,numc,nret)

    !     routine: countargs
    !     purpose: Count the number of non-blank arguments on the input line. Should be tocount. If not, then return an error (logical false). If tocount is zero or less, just count them
    !     arguments: label - CFAST keyword for this input
    !                tocount - expected number of arguments for this input
    !                lcarray - character array of arguments.  There should be tocount non-blank entries
    !                numc - dimension limit on lcarray
    !                nret - actual number of arguments

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
    end

    SUBROUTINE CPTIME(CPUTIM)

    !     routine: cptime
    !     purpose: routine to calculate amount of computer time (cputim) in seconds used so far.  this routine will generally be different for each computer.
    !     arguments: cputim (output) - elapsed cpu time 

    use ifport
    real*8 CPUTIM
    INTEGER*2 HRS, MINS, SECS, HSECS

    CALL GETTIM(HRS,MINS,SECS,HSECS)
    CPUTIM = HRS * 3600 + MINS * 60 + SECS + HSECS / 100.0
    RETURN
    END
    SUBROUTINE DATYPE(CRD,N1,N2,DTYPE)

    !     routine: datype
    !     purpose: this routine determines the data type of a string. the character string is examined between given starting and ending positions to determine if the substring is an integer, a real number, or a non-numeric string.
    !     arguments: crd    string containing number to be typed
    !                n1     starting position
    !                n2     ending position
    !                dtype (output) - returned type (1=real, 2=integer, 3=non-numeric)

    include "precis.fi"
    LOGICAL PERIOD, EFMT
    INTEGER N1, N2, DTYPE
    CHARACTER CRD*(*), NUM(12)*1
    DATA NUM /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-'/
    PERIOD = .FALSE.
    EFMT = .FALSE.

    ! DETERMINE DATA TYPE.  ASSUME A NUMERIC ENTRY UNTIL A NON-NUMERIC CHARACTER IS FOUND.
    DO 20 I = N1, N2
        IF (CRD(I:I)=='.') THEN
            IF (PERIOD) THEN

                ! second PERIOD IN STRING - NON NUMERIC
                GO TO 30
            ELSE
                PERIOD = .TRUE.
            endif
            GO TO 20
        endif

        ! CHECK FOR DIGITS
        DO 10 J = 1, 12
            IF (CRD(I:I)==NUM(J)) GO TO 20
10      CONTINUE
        IF (INDEX('EeDd',CRD(I:I))==0.OR.EFMT) GO TO 30
        EFMT = .TRUE.
20  CONTINUE

    ! DETERMINE TYPE OF NUMERIC ENTRY
    IF (PERIOD.OR.EFMT) THEN

        ! REAL
        DTYPE = 1
    ELSE

        ! INTEGER
        DTYPE = 2
    endif
    RETURN

    ! NON-NUMERIC
30  DTYPE = 3
    RETURN
    END

    LOGICAL FUNCTION DOESTHEFILEEXIST (CHECKFILE)

    !     routine: doesthefileexist
    !     purpose: checks for the existence of given file name
    !     arguments: checkfile - file name

    CHARACTER (*) CHECKFILE
    LOGICAL YESORNO


    INQUIRE (FILE=CHECKFILE, EXIST=YESORNO)
    IF (YESORNO) THEN
        DOESTHEFILEEXIST = .TRUE.
    else
        doesthefileexist = .false.
    ENDIF

    RETURN

    END

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

    INTEGER(2) n, status, loop, ld(2), li(2), ln(2), le(2), lb
    CHARACTER(256) buf, xname
    character *(*) exepath, datapath, project
    character (64) name(2)
    logical exists, doesthefileexist

    CHARACTER(3) drive(2)
    CHARACTER(256) dir(2)
    CHARACTER(64) ext(2)
    INTEGER(4) length, errorcode, pathcount, splitpathqq

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

    do 1 i = 1, 2

        loop = i - 1
        call getarg (loop, buf, status)

        if(status>0) then
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

1   continue

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
        write(*,*) 'The data file does not exist'
        errorcode = 102
    endif
    return

    end
    SUBROUTINE GRABKY(ICH,IT)

    USE ifport
    CHARACTER*1 CH, getcharqq
    INTEGER*2 ICH, IT
    logical peekcharqq

    ICH = 0
    IT = 0

    IF (PEEKCHARQQ()) THEN
        CH = GETCHARQQ()
        ICH = ICHAR(CH)
        IF (ICH==0) THEN
            CH = GETCHARQQ()
            ICH = ICHAR (CH)
            IT = 2
        ELSE
            IT = 1
        ENDIF
    ENDIF

    RETURN
    END

    SUBROUTINE MAT2MULT(MAT1,MAT2,IDIM,N,MATITER)

    !     routine: mat2mult
    !     purpose: given an nxn matrix mat1 whose elements are either 0 or 1, this routine computes the matrix mat1**2 and returns the results in mat1 (after scaling non-zero entries to 1).
    !     arguments: mat1 - matrix
    !                mat2 - work array of same size as mat1
    !                idim - actual dimensino limit on first subscript of mat1
    !                n - size of matrix
    !                matiter - unused

    DIMENSION MAT1(IDIM,N),MAT2(IDIM,N)
    DO 10 I = 1, N
        DO 20 J = 1, N
            MAT2(I,J) = IDOT(MAT1(I,1),IDIM,MAT1(1,J),1,N)
            IF(MAT2(I,J)>=1)MAT2(I,J) = 1
20      CONTINUE
10  CONTINUE
    DO 30 I = 1, N
        DO 40 J = 1, N
            MAT1(I,J) = MAT2(I,J)
40      CONTINUE
30  CONTINUE
    RETURN
    END

    INTEGER FUNCTION IDOT(IX,INX,IY,INY,N)

    !     routine: idot
    !     purpose: this routine computes the integer dot product of two integer vectors.
    !     arguments: ix, iy - two integer vectors

    INTEGER IX(*), IY(*)
    IDOT = 0
    II = 1 - INX
    JJ = 1 - INY
    DO 10 I = 1, N
        II = II + INX
        JJ = JJ + INY
        IDOT = IDOT + IX(II)*IY(JJ)
10  CONTINUE
    RETURN
    END

    SUBROUTINE INDEXI(N,ARRIN,INDX)

    !     routine: indexi
    !     purpose: this routines sorts the array arrin passively via the permuation array indx.  the elements arrin(indx(i)), i=1, ..., n are in increasing order. this routine uses a bubble sort.  it should not be used
    !              for large n (n>30), since bubble sorts are not efficient.  
    !     arguments: n     number of elements in n
    !                arrin array to be passively sorted
    !                indx  permuation vector containing ordering such that arrin(indx) is in increasing order.

    INTEGER ARRIN(*), INDX(*), AI, AIP1
    DO 10 I = 1, N
        INDX(I) = I
10  CONTINUE
5   CONTINUE
    ISWITCH = 0
    DO 20 I = 1, N-1, 2
        AI = ARRIN(INDX(I))
        AIP1 = ARRIN(INDX(I+1))
        IF(AI<=AIP1)GO TO 20
        ISWITCH = 1
        ITEMP = INDX(I)
        INDX(I) = INDX(I+1)
        INDX(I+1) = ITEMP
20  CONTINUE
    DO 30 I = 2, N-1, 2
        AI = ARRIN(INDX(I))
        AIP1 = ARRIN(INDX(I+1))
        IF(AI<=AIP1)GO TO 30
        ISWITCH = 1
        ITEMP = INDX(I)
        INDX(I) = INDX(I+1)
        INDX(I+1) = ITEMP
30  CONTINUE
    IF(ISWITCH==1)GO TO 5
    RETURN
    END
    SUBROUTINE INTERP(X,Y,N,T,ICODE,YINT)

    !     routine: indexi
    !     purpose: routine interpolates a table of numbers found in the arrays, x and y.  
    !     arguments: x,y - arrays of size n to be interpolated at x=t
    !                icode - code to select how to extrapolate values if t is less than x(1) or greater than x(n).
    !                          if icode = 1 then yint = y(1) for t < x(1) and yint = y(n) for t > x(n).
    !                          if icode = 2 then yint is evaluated by interpolation if x(1) < t < x(n) and by extrapolation if t < x(1) or    t > x(n)
    !                yint (output) - interpolated value of the y array at t

    include "precis.fi"

    DIMENSION X(*), Y(*)
    SAVE
    DATA ILAST /1/
    IF (N==1) THEN
        YINT = Y(1)
        RETURN
    endif
    IF (T<=X(1)) THEN
        IF (ICODE==1) THEN
            YINT = Y(1)
            RETURN
        ELSE
            IMID = 1
            GO TO 20
        endif
    endif
    IF (T>=X(N)) THEN
        IF (ICODE==1) THEN
            YINT = Y(N)
            RETURN
        ELSE
            IMID = N - 1
            GO TO 20
        endif
    endif
    IF (ILAST+1<=N) THEN
        IMID = ILAST
        IF (X(IMID)<=T.AND.T<=X(IMID+1)) GO TO 20
    endif
    IF (ILAST+2<=N) THEN
        IMID = ILAST + 1
        IF (X(IMID)<=T.AND.T<=X(IMID+1)) GO TO 20
    endif
    IA = 1
    IZ = N - 1
10  CONTINUE
    IMID = (IA+IZ) / 2
    IF (T<X(IMID)) THEN
        IZ = IMID - 1
        GO TO 10
    endif
    IF (T>=X(IMID+1)) THEN
        IA = IMID + 1
        GO TO 10
    endif
20  CONTINUE
    DYDX = (Y(IMID+1)-Y(IMID)) / (X(IMID+1)-X(IMID))
    YINT = Y(IMID) + DYDX * (T-X(IMID))
    ILAST = IMID
    RETURN
    END
    INTEGER FUNCTION LENGTH(STRING)
    CHARACTER STRING*(*)
    IF (LEN(STRING)/=0) THEN
        DO 10 I=LEN(STRING),1,-1
            IF (STRING(I:I)/=' ') THEN
                LENGTH=I
                RETURN
            endif
10      CONTINUE
    endif
    LENGTH=0
    RETURN
    END

    SUBROUTINE MESS(P,Z)

    !     routine: mess
    !     purpose: write a literal string of characters.  this is a write to stdout with a return/linefeed at the end 
    !     arguments: phrase - character string to be written
    !                z - length of the string

    use cparams
    use cshell

    INTEGER Z, L
    CHARACTER(len=z) P
    IF (Z>2048) STOP 'Error in message handler'
    WRITE (IOFILO,'(1x,2048A1)') (P(I:I),I=1,Z)
    RETURN

    END

    SUBROUTINE MESSNRF (STRING, L)

    !     routine: messnrf
    !     purpose: write a literal string of characters.  this is a write to stdout with a return/linefeed at the end 
    !     arguments: phrase - character string to be written
    !                l - length of the string

    use cparams
    use cshell

    CHARACTER STRING*(*), FORMATT*100
    WRITE (FORMATT,4000) L
    WRITE (IOFILO,FORMATT) STRING(1:L)
    RETURN

4000 FORMAT('(1X,A',I2.2,',$)')

    END
    SUBROUTINE READASTU (IN, COUNT, START, MAX, VALID)

    !     routine: readastu
    !     purpose: read in a string from the standard input device
    !              this version of readas(tu) is similar to the one used by everone else (readas) except that this contains automatic conversion to upper case. this is to filter commands from the console so that they are not case sensitive.
    !     arguments: 

    use cparams
    use cshell
    include "precis.fi"

    INTEGER START, COUNT
    LOGICAL VALID
    CHARACTER CH,INN*256,IN(MAX),C

10  INN = ' '
    READ(IOFILI,4,END=5) INN
4   FORMAT(A256)

    ! FILTER COMMENTS

    C = INN(1:1)
    IF (C=='!'.OR.C=='#') GO TO 10

    DO 7 I = 255,1,-1
        COUNT = I
        IF (INN(I:I)/=' ') GO TO 6
7   CONTINUE
5   COUNT = 0
    VALID = .FALSE.
    RETURN

    ! CHECK FOR CONTROL CHARACTERS
6   NC = 0
    COUNT = MIN(MAX,COUNT)
    DO 1 I = 1,COUNT
        ICH = ICHAR(INN(I:I))
        IF (ICH<32.OR.ICH>125) GO TO 3
        NC = I
1   CONTINUE
3   COUNT = NC

    ! CONVERT TO UPPER CASE
    DO 2 I = 1,COUNT
        CH = INN(I:I)
        ICH = ICHAR(CH)
        IF (ICH>96.AND.ICH<123) ICH = ICH - 32
        IN(I) = CHAR(ICH)
2   CONTINUE
    START=1
    IF (COUNT>0) THEN
        VALID = .TRUE.
    ELSE
        VALID = .FALSE.
    endif
    RETURN
    END
    SUBROUTINE READCV1 (IN,COUNT,START,IX,XI,TYPE,VALID)

    !     routine: readastu
    !     purpose: string conversion to integers or real numbers
    !     arguments: 

    INTEGER START,FIRST,LAST,COUNT,TYPE
    LOGICAL VALID
    CHARACTER*128 IN
    REAL XI

    CALL SSTRNGP (IN,COUNT,START,FIRST,LAST,VALID)
    IF (.NOT.VALID) THEN
        GO TO 5
    endif
    CALL CONVRT (IN,FIRST,LAST,TYPE,IX,XI)
    COUNT = COUNT - (LAST-START+1)
    START = LAST + 1
5   RETURN
    END
    SUBROUTINE READIN (NREQ, NRET, FIXED, FLTING)

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

    DIMENSION FLTING(*)
    INTEGER FIXED(*)
    REAL X0, XXBIG
    LOGICAL MULTI, eof
    CHARACTER LABEL*5, LABLE*5, SLASH*1, FILE*(*)
    SAVE INPUT, LSTART

    DATA INPUT/0/, LSTART/0/, SLASH/'/'/

    NRET = 0
    MULTI = .FALSE.
    XXBIG = 10000000.0D0
    DO 4 I = 1, NREQ
        CALL SSTRNG (INBUF, COUNT, START, FIRST, LAST, VALID)
        IF (.NOT.VALID) THEN
            START = 257
            RETURN
        ENDIF
1       DO 2 J = FIRST, LAST
            IF (INBUF(J:J)==SLASH) THEN
                LLAST = J - 1
                MULTI = .TRUE.
                GO TO 3
            ENDIF
2       CONTINUE
        LLAST = LAST
3       CALL CONVRT (INBUF, FIRST, LLAST, TYPE, I0, X0)
        IF (TYPE==1) THEN
            FLTING(NRET+1) = X0
            FIXED(NRET+1) = IFIX (MIN (X0, XXBIG))
        ELSE IF (TYPE==2) THEN
            FIXED(NRET+1) = I0
            FLTING(NRET+1) = FLOAT(I0)
        ELSE
            IF (LOGERR>0) THEN
                WRITE(LOGERR,12) FIRST,LLAST,(INBUF(J:J),J=1,50)
12              FORMAT(' WARNING!! NON-NUMERIC DATA IN ',2I3,50A1)
            ENDIF
            NRET = NRET - 1
        ENDIF
        IF (MULTI) THEN

            ! LOOP FOR A CONTINUATION
            FIRST = LLAST + 2
            MULTI = .FALSE.
            NRET = NRET + 1
            GO TO 1
        ENDIF
        COUNT = COUNT - (LAST-START+1)
        START = LAST + 1
        NRET = NRET + 1
4   CONTINUE

    RETURN

    ! READ IN A BUFFER
    ENTRY READBF(IU,LABLE, eof)

    ! CHECK TO SEE IF THE BUFFER IS CURRENT AND FULL

    IF (IU==INPUT.AND.START==LSTART) THEN
        LABLE = LABEL
        RETURN
    ENDIF
    INPUT = IU
    CALL READAS (IU, INBUF, COUNT, START, VALID)
    IF (.NOT.VALID) GO TO 11
    CALL SSTRNG (INBUF, COUNT, START, FIRST, LAST, VALID)
    IF (.NOT.VALID) GO TO 11
    LLAST = MIN(LAST, FIRST+5)
    LABEL = ' '
    LABEL = INBUF(FIRST:LLAST)
    LABLE = LABEL
    COUNT = COUNT - (LAST-START+1)
    START = LAST + 1
    LSTART = START
    eof = .false.
    write(logerr,14) label
14  format ('Label = ',a5)
    RETURN

11  LABEL = '     '
    LABLE = LABEL
    START = 1
    COUNT = 0
    eof = .true.
    RETURN

    ! ENTRY TO FORCE A CONTEXT SWITCH

    ENTRY READRS

    ! SET START TO ITS INITIAL VALUE
    START = 257
    RETURN

    ! READ IN A FILE NAME
    ENTRY READFL (FILE)

    CALL SSTRNG (INBUF, COUNT, START, FIRST, LAST, VALID)
    IF (.NOT.VALID) THEN
        START = 257
        FILE = ' '
        RETURN
    ENDIF
    LENOFCH = LEN (FILE)
    LLAST = MIN(LAST, FIRST+LENOFCH)
    FILE = INBUF(FIRST:LLAST)
    COUNT = COUNT - (LAST-START+1)
    START = LAST + 1
    RETURN
    END

    SUBROUTINE READAS(INFILE, INBUF, COUNT, START, VALID)

    !     routine: readas
    !     purpose:  read in a string from the input file, filtering out comments
    !     arguments: infile
    !                inbuf
    !                count
    !                start
    !                valid

    use cparams
    use cshell
    include "precis.fi"

    ! READ IN A STRING
    INTEGER START, COUNT, IREC
    CHARACTER INBUF*(*), CMT1*1, CMT2*1, frmt*30
    LOGICAL VALID
    DATA CMT1/'!'/, CMT2/'#'/, IREC/0/
    SAVE IREC

    ! IF WE HAVE REACHED AN END OF FILE, RETURN NOTHING
10  INBUF = ' '
    IF (IREC==0) THEN
        READ(INFILE,1,END=2,ERR=2) INBUF
    ELSE
        READ(INFILE,1,REC=IREC,ERR=2) INBUF
        IREC = IREC + 1
    endif
1   FORMAT(A)
    ls = len_trim (inbuf)
    write (frmt, 9) ls
    WRITE (LOGERR,frmt) INBUF
9   format('(''Buffer input = '',A',i3,')')

    ! FILTER COMMENTS
    IF (INBUF(1:1)==CMT1.OR.INBUF(1:1)==CMT2) GO TO 10
    COUNT = LEN(INBUF)
    START = 1
    VALID = .TRUE.
    RETURN
2   VALID = .FALSE.
    COUNT = 0
    START = 1
    WRITE(LOGERR,*) 'END OF FILE FOR UNIT ',INFILE
    RETURN
    END

    SUBROUTINE READOP

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

    INTEGER (2) YEAR, MONTH, DAY
    LOGICAL EXISTS
    CHARACTER STRS(8)*60, IC, TOUPPER*1, LOGFILE*60
    CHARACTER*60 SOLVEINI
    INTEGER IARG(8), IOPT(26), OPTION
    cmdflag(IC) = IOPT(ICHAR(IC)-ICHAR('A')+1)

    ! CURRENT DATE
    CALL GETDAT(YEAR,MONTH,DAY)
    RUNDAT(3) = DAY
    RUNDAT(2) = MONTH
    RUNDAT(1) = YEAR
    WRITE (MPSDATC,5010) RUNDAT(1), RUNDAT(2), RUNDAT(3)

    ! Command LINE ARGUMENTS
    NARGS = 8
    CALL CMDLINE(NARGS,STRS,IARG,IOPT)

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
1   inc = 3*inc+1
    if (inc<=n) go to 1
2   continue
    inc = inc / 3
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
    end
    SUBROUTINE SORTBRM(X,LX,IX,LIX,NROW,NCOLX,NCOLIX,ISORT,LDP,NROOM,IPOINT)

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

    include "precis.fi"
    DIMENSION X(LX,NCOLX), IX(LIX,NCOLIX)

    ! IF THE NUMBER OF FIRES, DETECTORS OR ROOMS EVER EXCEEDS 200 THEN THE FOLLOWING DIMENSION STATEMENT NEEDS TO BE CHANGED
    PARAMETER (LWORK=100)
    DIMENSION IPOINT(LDP,*), WORK(LWORK), IWORK(LWORK), IPERM(LWORK)

    ! CREATE A PERMUTATION VECTOR USING THE ISORT'TH COLUMN OF IX
    IF(NROW>LWORK)THEN
        CALL XERROR('NOT ENOUGH WORK SPACE IN SORTBRM',0,1,2)
    ENDIF
    DO 2 I = 1, NROW
2   IPERM(I) = I
    CALL INDEXI(NROW,IX(1,ISORT),IPERM)

    ! REORDER INTEGER ARRAY USING THE PERMUTATION VECTOR
    DO 5 J = 1, NCOLIX
    DO 10 I = 1, NROW
    IWORK(I) = IX(IPERM(I),J)
10  CONTINUE
    DO 20 I = 1, NROW
    IX(I,J) = IWORK(I)
20  CONTINUE
5   CONTINUE

    ! REORDER THE FLOATING POINT ARRAYS USING THE PERMUTATION VECTOR
    DO 50 J = 1, NCOLX
    DO 30 I = 1, NROW
    WORK(I) = X(IPERM(I),J)
30  CONTINUE
    DO 40 I = 1, NROW
    X(I,J) = WORK(I)
40  CONTINUE
50  CONTINUE

    ! CONSTRUCT THE POINTER ARRAY
    DO 60 I = 1, NROOM
    IPOINT(I,1) = 0
    IPOINT(I,2) = 0
60  CONTINUE
    DO 70 I = 1, NROW
    IROOM = IX(I,ISORT)
    IPOINT(IROOM,1) = IPOINT(IROOM,1) + 1
    IF (IPOINT(IROOM,2)==0) IPOINT(IROOM,2) = I
70  CONTINUE
    DO 80 I = 1, NROOM
    IF (IPOINT(I,2)==0) IPOINT(I,2) = 1
80  CONTINUE
    RETURN
    END

    SUBROUTINE SORTFR(NFIRE,IFROOM,XFIRE,IFRPNT,NM1)

    !     routine: sortbrm
    !     purpose: sort the two arrays ifroom and xfire into increasing room number in ifroom.  these are used in this order by the ceiling jet and radiation algorithms
    !     arguments: nfire   number of fires
    !                ifroom (output)  room numbers for each of the fires
    !                xfire   fire related quantities used by other routines. see routine fires for definition.
    !                ifrpnt  pointer array for sorted fire list. (r,1) = number of fires in room r. (r,2) = pointer to beginning element in ifroom and xfire for fires in room r

    use cparams
    include "precis.fi"
    DIMENSION IFROOM(MXFIRE), XFIRE(MXFIRE,MXFIRP), IPERM(MXFIRE), IWORK(MXFIRE), WORK(MXFIRE), IFRPNT(NR,2)

    ! CREATE A PERMUTATION VECTOR FROM THE LIST OF FIRE ROOMS WHICH IS ORDERED BY INCREASING ROOM NUMBER
    DO 2 I = 1, NFIRE
2   IPERM(I) = I
    CALL INDEXI(NFIRE,IFROOM,IPERM)

    ! REORDER THE TWO ARRAYS WITH THE PERMUTATION VECTOR
    DO 10 I = 1, NFIRE
    IWORK(I) = IFROOM(IPERM(I))
10  CONTINUE
    DO 20 I = 1, NFIRE
    IFROOM(I) = IWORK(I)
20  CONTINUE

    DO 50 J = 1, MXFIRP
    DO 30 I = 1, NFIRE
    WORK(I) = XFIRE(IPERM(I),J)
30  CONTINUE
    DO 40 I = 1, NFIRE
    XFIRE(I,J) = WORK(I)
40  CONTINUE
50  CONTINUE

    ! DO THE POINTER ARRAYS FOR THE RADIATION AND CEILING JET ROUTINES
    DO 60 I = 1, NM1
    IFRPNT(I,1) = 0
    IFRPNT(I,2) = 0
60  CONTINUE
    DO 70 I = 1, NFIRE
    IRM = IFROOM(I)
    IFRPNT(IRM,1) = IFRPNT(IRM,1) + 1
    IF (IFRPNT(IRM,2)==0) IFRPNT(IRM,2) = I
70  CONTINUE
    DO 80 I = 1, NM1
    IF (IFRPNT(I,2)==0) IFRPNT(I,2) = 1
80  CONTINUE
    RETURN
    END

    SUBROUTINE SSTRNG(STRING,WCOUNT,SSTART,SFIRST,SLAST,SVALID)

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

    LOGICAL SVALID
    INTEGER SFIRST, SLAST, SSTART, WCOUNT, ENDSTR
    CHARACTER*1 STRING(*), SPACE, COMMA

    DATA SPACE/' '/, COMMA/','/

    SVALID = .TRUE.

    ! INVALID STARTING POSITION - PAST END OF STRING
    ENDSTR = SSTART + WCOUNT - 1

    ! FIND POSITION OF FIRST ELEMENT OF SUBSTRING
    DO 20 I = SSTART, ENDSTR

    ! MOVE TO THE BEGINNING OF THE SUBSTRING
    SFIRST = I
    IF((STRING(I)/=SPACE).AND.(STRING(I)/=COMMA)) GOTO 60
20  CONTINUE

    ! NO SUBSTRING FOUND - ONLY DELIMITER
    GO TO 40

    ! FIND POSITION OF LAST CHARACTER OF SUBSTRING
60  DO 50 J = SFIRST, ENDSTR

    ! POSITION OF LAST ELEMENT OF SUBSTRING
    SLAST = J-1
    IF((STRING(J)==SPACE).OR.(STRING(J)==COMMA)) GO TO 100
50  CONTINUE

    ! NO SUBSTRING DELIMITER => LAST CHARACTER OF SUBSTRING IS THE LAST CHARACTER OF THE STRING
    SLAST = ENDSTR
    RETURN

    ! NO SUBSTRING FOUND
40  SVALID = .FALSE.
100 RETURN
    END

    SUBROUTINE SSTRNGP (STRING,WCOUNT,SSTART,SFIRST,SLAST,SVALID)

    !     routine: sstrngp
    !     purpose: this routine finds positions of substrings within a character string.  similar to sstrng, except entries can be grouped with parenthesis
    !     arguments: string - the character string
    !                wcount - number of characters in the string
    !                sstart - beginning position in string to look for a substring
    !                sfirst (output) - beginning position of the substring
    !                slast - ending position of the substring
    !                svalid - true if a valid substring is found

    LOGICAL SVALID
    INTEGER SFIRST,SLAST,SSTART,WCOUNT,ENDSTR
    CHARACTER*1 STRING(128),SPACE,COMMA,RPAREN,LPAREN
    DATA SPACE /' '/, COMMA /','/,RPAREN /')'/, LPAREN /'('/

    SVALID = .TRUE.
    ENDSTR = SSTART + WCOUNT - 1

    ! FIND POSITION OF FIRST ELEMENT OF SUBSTRING
    DO 20 I = SSTART, ENDSTR

    ! MOVE TO THE BEGINNING OF THE SUBSTRING
    SFIRST = I
    IF ((STRING(I)/=SPACE).AND.(STRING(I)/=COMMA).AND.(STRING(I)/=RPAREN).AND.(STRING(I)/=LPAREN)) GO TO 60
20  CONTINUE

    ! NO SUBSTRING FOUND - ONLY DELIMITER
    GO TO 40

    ! FIND POSITION OF LAST CHARACTER OF SUBSTRING
60  DO 50 J = SFIRST, ENDSTR

    ! POSITION OF LAST ELEMENT OF SUBSTRING
    SLAST = J-1
    IF ((STRING(J)==SPACE).OR.(STRING(J)==COMMA).OR.(STRING(J)==RPAREN).OR.(STRING(J)==LPAREN)) GO TO 100
50  CONTINUE

    ! NO SUBSTRING DELIMITER => LAST CHARACTER OF SUBSTRING IS THE LAST CHARACTER OF THE STRING
    SLAST = ENDSTR
    RETURN

    ! NO SUBSTRING FOUND
40  SVALID = .FALSE.
    RETURN
100 IF (SLAST<SFIRST) SVALID = .FALSE.
    END

    CHARACTER FUNCTION TOUPPER(CH)

    !     routine: toupper / tolower
    !     purpose: convert a single ascii character to upper or lower case
    !     arguments: ch - character to be converted

    CHARACTER*1 CH, TOLOWER

    ! CONVERT TO UPPER CASE
    ICH = ICHAR(CH)
    IF (ICH>96.AND.ICH<123) ICH = ICH - 32
    TOUPPER = CHAR(ICH)
    RETURN

    ! COVERT TO LOWER CASE
    ENTRY TOLOWER(CH)
    ICH = ICHAR(CH)
    IF (ICH>64.AND.ICH<91) ICH = ICH + 32
    TOLOWER = CHAR(ICH)
    RETURN
    END

    SUBROUTINE UPPERALL(FROM,TO)

    !     routine: upperall
    !     purpose: convert a string to upper case
    !     arguments: from - string to be converted
    !                to (output) - converted string

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
10  CONTINUE
    IF(NTO>NN)TO(NN+1:NTO)=' '
    RETURN
    END
    INTEGER FUNCTION FUNIT (IO)

    !     routine: funit
    !     purpose: finds first avalable i/o unit starting at unit number io
    !     arguments: io - beginning unit number for search

    PARAMETER(MXIO=32767)
    LOGICAL OPEND

    ITMP = IO-1
10  CONTINUE
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

    !     routine: opnotpt
    !     purpose: opens a file using the extension to distinguish previous open files
    !     arguments: filname - base filename for file to be opened
    !                iounit - desired unit number for file

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
30  CONTINUE
    ITMP = ITMP + 1
    WRITE(FMT,10) ILEN
10  FORMAT('(A',I2.2,',',1H','.',1H',',I3.3)')
    WRITE(WORKFIL,FMT) NAMEFIL, ITMP
    INQUIRE (FILE = WORKFIL, EXIST = EXISTED)
    IF (EXISTED) GO TO 30
    OPEN (UNIT = IOUNIT, FILE = WORKFIL,RECL=255)
    RETURN
    END


    subroutine xerbla ( srname, info )

    !     routine: xerbla
    !     purpose: opens a file using the extension to distinguish previous open files
    !     arguments: srname - specifies the name of the routine which called xerbla
    !                info - on entry, info specifies the position of the invalid parameter in the parameter-list of the calling routine.


    integer            info
    character*6        srname

    write (*,99999) srname, info
    stop

99999 format ( ' ** on entry to ', a6, ' parameter number ', i2,' had an illegal value' )

    end subroutine xerbla

    logical function lsame ( ca, cb )

    !     routine: lsame
    !     purpose: tests if ca is the same letter as cb regardless of case. cb is assumed to be an upper case letter. lsame returns .true. if ca is either the same as cb or the equivalent lower case letter.
    !     arguments: ca - first character
    !                cb - second character

    character*1 :: ca, cb
    integer, parameter :: ioff = 32
    intrinsic ichar

    lsame = ca == cb

    if ( .not.lsame ) then
        lsame = ichar(ca) - ioff == ichar(cb)
    endif
    return

    end function lsame

    integer function rev_auxilliary

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