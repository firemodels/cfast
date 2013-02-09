
    subroutine dreadin(output,iounit,ierr,ivers0)

    !     routine: dreadin
    !     purpose: read routine for compacted history files
    !     arguments: output   starting location for i/o
    !                iounit   logical unit assigned for read
    !                ierr     status of read (zero if ok)
    !                ivers0   version number of history file

    implicit none
    
    integer, parameter :: mxdmp = 36000
    logical cnvrt
    character :: header*6, rheader(2)*6 = (/'$$CFL$', '$$CFH$'/)
    integer output(*), input(mxdmp), flop, ierr, iounit, ios, ivers0, i, itot, iflt, iint, itemp

    ierr = 0
    read (iounit,end=30,iostat=ios) header, ivers0
    if (header==rheader(1)) then
#ifdef pp_ibmpc
        cnvrt = .false.
#else
        cnvrt = .true.
#endif
    else if (header==rheader(2)) then
#ifdef pp_ibmpc
        cnvrt = .true.
#else
        cnvrt = .false.
#endif
    else
        ierr = 9999
        return
    endif
    if (cnvrt) ivers0 = flop(ivers0)
    if (cnvrt) then
        read (iounit,end=30,iostat=ios) input(1), (input(i),i = 2,flop(input(1)))
        input(1) = flop(input(1))
    else
        read (iounit,end=30,iostat=ios) input(1), (input(i),i = 2,input(1))
    endif
    if (input(1)>mxdmp) then
        call xerror('DREADIN - overwrite input buffer; fatal error',0,1,1)
        ierr = 7
        return
    endif
    if (cnvrt) then
        do i = 2, input(1)
            input(i) = flop(input(i))
        end do
    endif
    call unpack(input,output)
    if (cnvrt) then
        call lenoco(((ivers0-1800)/10),itot,iflt,iint)
        do  i = 1, iflt / 2
            itemp = output(2*i-1)
            output(2*i-1) = output(2*i)
            output(2*i) = itemp
        end do
    endif
30  if (ios/=0) then
        ierr = ios
    else
        ierr = 0
    endif
    return
    end subroutine dreadin

    subroutine unpack(input,output)

    !     routine: unpack
    !     purpose: this routine is to uncrunch the packed binary history file.
    !              the length of the record is contained in the first word and does not include the first word itself.  
    !              see writeot for the reference information
    !     arguments: input    packed array
    !                output   unpack array returned

    implicit none
    
    integer :: output(*), input(*), i, intgr, cntr, mrkr, inlen, inidx, outidx

    mrkr = 106
    inidx = 1
    outidx = 0
    inlen = input(1)
10  if (.true.) then
        inidx = inidx + 1
        if (inidx>inlen) go to 30
        intgr = input(inidx)
        if (intgr==mrkr) then
            inidx = inidx + 1
            if (inidx>inlen) go to 30
            intgr = input(inidx)
            if (intgr==mrkr) then
                outidx = outidx + 1
                output(outidx) = intgr
            else
                inidx = inidx + 1
                if (inidx>inlen) go to 30
                cntr = input(inidx)
                do i = 1, cntr
                    outidx = outidx + 1
                    output(outidx) = intgr
                end do
            endif
        else
            outidx = outidx + 1
            output(outidx) = intgr
        endif
        go to 10
    endif
30  return
    end

    integer function flop(inum)

    !     routine: flop
    !     purpose: this routine flips bytes in an integer word big-endian to little endian or back
    !     arguments: inum: number to be flipped

    implicit none
    
    integer :: inum
    integer*1 :: b(4), c(4), i, j
    equivalence (b,i), (c,j)
    
    i = inum
    c(1) = b(4)
    c(2) = b(3)
    c(3) = b(2)
    c(4) = b(1)
    flop = j
    return
    end function flop

    
    subroutine dumper (istep,ierror)

    !     description:  saves the data files in packed binary format
    !
    !     arguments: istep  current time step
    !                ierror returns error code

    use cfast_main
    use cshell
    use iofiles
    
    implicit none

    integer :: iounit = 11, itot, iflt, iint, ierr, istep, ierror
    save itot

    if (ndumpr==0) stop 106

    termxx = itmstp - 1
    itermxx = itmstp - 1
    call lenoco(version/10,itot,iflt,iint)
    call writeot(dmpoutput,itot,iounit,ierr,version)
    if (ierr==0) then
        if (debugging) write (logerr,5020) istep, itot * 4
        return
    endif

    !error processing
    write (logerr,5030) mod(ierr,256), historyfile
    stop

5020 format ('Write to the history file at',i5,i7)
5030 format ('From dumper, error in accessing history file, error = ',i5,/,' File = ',a256)
    end subroutine dumper 

    subroutine lenoco (iv,itot,iflt,iint)

    !     description:  to calculation the length of the numeric common block

    !     arguments: iv     cfast reduced version number (version -1800) / 10
    !                itot   total length of the common block in storage units
    !                iflt   length of the floating portion in numeric storage units
    !                iint   length of the integer portion in numeric storage units

    use cfast_main
    
    implicit none
    
    integer :: iint, iflt, itot, iv

    iint = (loc(itermxx) - loc(nlspct))/4 + 1
    iflt = (loc(termxx) - loc(gamma))/4

    iflt = iflt + 2
    itot = iint + iflt

    return
    end subroutine lenoco

    subroutine writeot(input,len,iounit,ierr,ivers0)

    !     description:  write compacted history file
    !     arguments: input
    !                len
    !                iounit
    !                ierr
    !                ivers0

    implicit none
    
    integer, parameter :: mxdmp = 36000
    integer :: input(len), parray(mxdmp), ierr, iounit, ios, ivers0, i, len
    character header*6
    data header /'$$CFL$'/

    ierr = 0
    call packot(len,mxdmp,input,parray)
    write (iounit,iostat=ios) header, ivers0
    write (iounit,iostat=ios) parray(1), (parray(i),i = 2,parray(1))

    if (ios/=0) then
        ierr = ios
    else
        ierr = 0
    endif
    return
    end subroutine writeot

     subroutine packot (itin,mxdmp,doit,retbuf)

    !     This is the pack routine.  It crunches the binary common block to reduce
    !     The amount of storage required to hold a single history record.
    !     The length of the record which is written is contained in the first word
    !     of the record and does NOT include itself.
    !     The original implementation of this work was published by 
    !     Andrew Binstock in The C Gazette, December 1987.  It is one of a large
    !     class of compression schemes.  This particular scheme is fast, and is
    !     best at compressing strings of the same character.  Since much of
    !     the history file in CFAST stays the same, 0 for example, this works
    !     fairly well.

    implicit none 
    
    integer :: ic, itin, idx, ridx, mxdmp, doit(itin), retbuf(mxdmp), lc, count, mrkr, ierr, i
    character :: msg*80

    idx = 1
    ridx = 1
    mrkr = 106
    ic = doit(idx)
    idx = idx + 1

    !     checking to make sure the first numbers are not the marker
10  if (ic==mrkr) then
        ridx = ridx + 1
        retbuf(ridx) = mrkr
        ridx = ridx + 1
        retbuf(ridx) = mrkr
        ic = doit(idx)
        idx = idx + 1
        go to 10
    endif

    lc = ic
    count = 1

    !     main loop
20  if (idx<=itin) then
        ic = doit(idx)
        idx = idx + 1

        !     if next number = marker then store what you have
30      if (ic==mrkr) then
            if (count>3) then
                if ((ridx+5)>=mxdmp) then
                    write (msg,*) 'packot - overwrite, input and index = ', itin, idx
                    call xerror(msg,0,1,1)
                    ierr = 19
                    return
                endif
                call oput(lc,count,itin,mxdmp,ridx,retbuf)
            else
                if ((ridx+count+2)>=mxdmp) then
                    write (msg,*) 'packot - overwrite, input and index = ', itin, idx
                    call xerror(msg,0,1,1)
                    ierr = 19
                    return
                endif
                do, i = 1, count
                    ridx = ridx + 1
                    retbuf(ridx) = lc
                end do
            endif
            count = 0
            ridx = ridx + 1
            retbuf(ridx) = mrkr
            ridx = ridx + 1
            retbuf(ridx) = mrkr
            if (idx>itin) go to 60
            ic = doit(idx)
            idx = idx + 1
            lc = ic
            go to 30
        endif
        if (ic==lc) then
            count = count + 1
            if (count==(2**30)) then
                if ((ridx+5)>=mxdmp) then
                    write (msg,*) 'packot - overwrite, input and index = ', itin, idx
                    call xerror(msg,0,1,1)
                    ierr = 19
                    return
                endif
                call oput(lc,count,itin,mxdmp,ridx,retbuf)
                count = 0
            endif
        else
            if (count>3) then
                if ((ridx+5)>=mxdmp) then
                    write (msg,*) 'packot - overwrite, input and index = ', itin, idx
                    call xerror(msg,0,1,1)
                    ierr = 19
                    return
                endif
                call oput(lc,count,itin,mxdmp,ridx,retbuf)
                lc = ic
                count = 1
            else
                if ((ridx+count+2)>=mxdmp) then
                    write (msg,*) 'packot - overwrite, input and index = ', itin, idx
                    call xerror(msg,0,1,1)
                    ierr = 19
                    return
                endif
                do i = 1, count
                    ridx = ridx + 1
                    retbuf(ridx) = lc
                end do
                lc = ic
                count = 1
            endif
        endif
        go to 20
    endif
60  if (count>3) then
        if ((ridx+5)>=mxdmp) then
            write (msg,*) 'packot - overwrite, input and index = ', itin, idx
            call xerror(msg,0,1,1)
            ierr = 19
            return
        endif
        call oput(lc,count,itin,mxdmp,ridx,retbuf)
        lc = ic
        count = 1
    else
        if ((ridx+count+2)>=mxdmp) then
            write (msg,*) 'packot - overwrite, input and index = ', itin, idx
            call xerror(msg,0,1,1)
            ierr = 19
            return
        endif
        do i = 1, count
            ridx = ridx + 1
            retbuf(ridx) = lc
        end do
    endif
    retbuf(1) = ridx
    return
    end subroutine packot
