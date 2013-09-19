
! --------------------------- cfast -------------------------------------------

    program cfast

    !     Routine: cfast (main program)
    !     Purpose: main program for the model
    !     Revision: $Revision$
    !     Revision Date: $Date$

    !     Permission is hereby granted, free of charge, to any person
    !     obtaining a copy of this software and associated documentation
    !     files (the "Software"), to deal in the Software without
    !     restriction, including without limitation the rights to use,
    !     copy, modify, merge, publish, distribute, sublicense, and/or sell
    !     copies of the Software, and to permit persons to whom the
    !     Software is furnished to do so, subject to the following
    !     conditions:

    !     The above copyright notice and this permission notice shall be
    !     included in all copies or substantial portions of the Software.

    !     The software is provided "as is", without warranty of any kind,
    !     express or implied, including but not limited to the warranties
    !     of merchantability, fitness for a particular purpose and
    !     noninfringement. In no event shall the authors or copyright
    !     holders be liable for any claim, damages or other liability,
    !     whether in an action of contract, tort or otherwise, arising
    !     from, out of or in connection with the software or the use or
    !     other dealings in the software.
    !
    !     dummy change to force cfastbot run
    
    use precision_parameters
    use cfast_main
    use cparams
    use cshell
    use dsize
    use iofiles
    use thermp
    implicit none

    integer errorcode, rev_cfast, irev, i
    real(eb) :: xdelt, tstop, tbeg, tend

    version = 6300          ! Current CFAST version number
    crdate = (/2013,6,12/)  ! Current CFAST executable creation date

    errorcode = 0

    if(command_argument_count().eq.0)then
        call versionout(0)
        write (*,*) 'No input file specified'
    endif

    !     initialize the basic memory configuration

    call initmm
    call initob
    call readop     
    call readcf1 (errorcode)

    !     initial output

    write (logerr, 5000) mpsdatc
    if (errorcode>0) then
        write (*, 5001) errorcode
        stop 
    else
        write (logerr, 5002) project
    endif

    mpsdat(1) = rundat(1)
    mpsdat(2) = rundat(2)
    mpsdat(3) = rundat(3)

    call versionout (logerr)
    irev = rev_cfast()

    call initslv
    call readinputfile (errorcode)
    if (errorcode<=0) then

        if (header) call disclaim('CFAST')

        call initspec

        xdelt = nsmax/deltat
        itmmax = xdelt + 1
        tstop = itmmax - 1

        ! add the default thermal property
        maxct = maxct + 1

        nlist(maxct) = 'DEFAULT'
        lnslb(maxct) = 1
        lfkw(1,maxct) = 0.120_eb
        lcw(1,maxct) = 900.0_eb
        lrw(1,maxct) = 800.0_eb
        lflw(1,maxct) = 0.0120_eb
        lepw(maxct) = 0.90_eb
        do i = 1, 7
            lhclbf(i,maxct) = 0.00_eb
        end do

        call initwall(tstop,errorcode)
        if (errorcode<=0) then

            stime = 0.0_eb
            itmstp = 1
            xdelt = nsmax/deltat
            itmmax = xdelt + 1
            tstop = itmmax - 1

            call outinitial

            call cptime(tbeg)
            call solve(tstop,errorcode)
            call cptime(tend)

            write (logerr,5003) tend - tbeg
            errorcode = 0

        endif
    endif

    !     errors

    call cfastexit ('CFAST', errorcode)

5000 format ('Date stamp from CFAST initialization ',a14)
5001 format ('***Error: Error encountered in opening data files; code = ',i4)
5002 format ('The project files are based on the root: ',a64)
5003 format ('Total execution time = ',1pg10.3,' seconds')
    end program cfast

! --------------------------- initsoln -------------------------------------------

    subroutine initsoln(t,pdold,pdzero,rpar,ipar)

    !     Routine: initsoln
    !     Purpose: This routine determines an initial solution to
    !              the zone fire modeling equations.  A non-linear
    !              algebraic solver (SNSQE) is used to calculate initial
    !              room pressures that make dP/dt zero.  If an HVAC system
    !              is modeled then HVAC node pressures and hvac duct
    !              temperatures are also determined to force mass and energy
    !              conservation.

    use precision_parameters
    use cenviro
    use cfast_main
    use opt
    use params
    use solver_parameters
    use wnodes
    implicit none

    external gres, gres2, gres3, gjac
    
    integer, intent(in) :: ipar(*)
    real(eb), intent(in) :: t,pdzero(*), rpar(*)
    real(eb), intent(out) :: pdold(*)

    integer, parameter :: mxalg = 4*nr+mnode+mbr
    real(eb) deltamv(mxalg), hhvp(mxalg)
    integer, parameter :: lrw = (3*mxalg**2+13*mxalg)/2
    real(eb) :: work(lrw)
    integer :: ires, iopt, nhvalg, nalg0, nalg1, nalg2, nprint, i, ioff0,  info, ii, ieq1, ieq2, nodes
    real(eb) :: tol

1   continue

    call roomcon(t)

    rpar2(1) = rpar(1)
    ipar2(1) = ipar(1)
    ipar2(2) = ipar(2)
    call setderv(-1)
    call resid(t,p,pdzero,pdold,ires,rpar2,ipar2)
    iopt = 2
    tol = algtol
    nhvalg = nhvpvar + nhvtvar
    nalg0 = nhvalg
    nalg1 = nm1 + nhvalg
    nprint = -1

    ! room pressures
    do i = 1, nm1
        hhvp(i) = p(i+nofp)
    end do

    ! hvac pressures
    do i = 1, nhvpvar
        hhvp(i+nm1) = p(i+nofpmv)
    end do

    ! hvac temperatures
    do i = 1, nhvtvar
        hhvp(i+nm1+nhvpvar) = p(i+noftmv)
    end do

    do i = 1, nequals
        pinit(i) = p(i)
    end do
    if (option(fpsteady)==1) then
        call snsqe(gres,gjac,iopt,nalg1,hhvp,deltamv,tol,nprint,info, work,lrw)
    elseif (option(fpsteady)==2) then
        ioff0 = nalg1

        ! upper layer temperatures
        nalg2 = nalg1 + 1
        hhvp(1+ioff0) = zzftemp(lfbo,upper)

        ! wall temperatures
        ii = 0
        ieq1 = izwmap2(1,lfbo)
        ieq2 = izwmap2(3,lfbo)
        if(ieq1/=0)then
            ii = ii + 1
            nalg2 = nalg2 + 1
            hhvp(ii+ioff0+1) = p(ieq1)
        endif
        if(ieq2/=0)then
            ii = ii + 1
            nalg2 = nalg2 + 1
            hhvp(ii+ioff0+1) = p(ieq2)
        endif

        call snsqe(gres3,gjac,iopt,nalg2,hhvp,deltamv,tol,nprint,info,work,lrw)
    else
        if (nhvalg>0) then
            call snsqe(gres2,gjac,iopt,nalg0,hhvp(1+nm1),deltamv(1+nm1),tol,nprint,info,work,lrw)
        else
            info = 1
        endif
    endif

    ! couldn't find a solution.  either try to recover or stop
    if (info/=1) then
        if(option(fpsteady)/=off)then
            option(fpsteady) = off
            call xerror('Trying non-steady initial guess' ,0,101,0)
            go to 1
        endif
        call xerror('Solver could not find an initial solution' ,0,102,2)
    endif

    ! if a room is not connected to any other room via a horizontal or
    ! vertical vent then do not use the snsqe pressure solution,
    ! use the original pressure solution that was based on rho*g*h.
    do i = 1, nm1
        if(izcon(i))p(i+nofp) = hhvp(i)
    end do
    do i = 1, nhvpvar
        p(i+nofpmv) = hhvp(i+nm1)
    end do
    do i = 1, nhvtvar
        p(i+noftmv) = hhvp(i+nm1+nhvpvar)
    end do
    if (option(fpsteady)==2) then
        p(lfbo+noftu) = hhvp(1+ioff0)
        ii = 0
        if(ieq1/=0)then
            ii = ii + 1
            p(ieq1) = hhvp(ii+ioff0+1)
        endif
        if(ieq2/=0)then
            ii = ii + 1
            p(ieq2) = hhvp(ii+ioff0+1)
        endif
    endif
    call resid(t,p,pdzero,pdold,ires,rpar,ipar)

    ! Added to resync the species mass with the total mass of each layer at the new pressure
    nodes = nofprd+1
    call resync(p,nodes)
    do i = 1, nhvpvar
        pdold(i+nofpmv) = 0.0_eb
    end do
    do i = 1, nhvtvar
        pdold(i+noftmv) = 0.0_eb
    end do
    do i = 1, nwalls
        pdold(i+nofwt) = 0.0_eb
    end do
    return
    end

! --------------------------- solve -------------------------------------------

    subroutine solve(tstop,ierror)

    !     Routine: solve
    !     Purpose: main solution loop for the model
    !     Arguments: TSTOP   The final time to which CFAST should run to
    !                IERROR  Returns error codes

    !     Offset in the following context is the beginning of the vector for
    !     that particular variable minus one.  Thus, the actual pressure array
    !     goes from NOFP+1 to NOFP+nm1.  The total number of equations to be
    !     considered is NEQUALS, and is the last element in the last vector.
    !     Each physical interface routine is responsible for the COUNT of the
    !     number of elements in the vector for which it is resonsible.

    !     This set of parameters is set by NPUTP and is kept in the environment
    !     common block CFAST.INC.  To index a variable, the list is something
    !     like (for temperature in this case)

    !     NOFTU+1, NOFTU+NM1

    !     The structure of the solver array is

    !     NOFP = offset for the main pressure; the array of base pressures for each compartment
    !     NOFPMV = offset for HVAC node pressuers
    !     NOFTMV = offset for HVAC branch temperatures
    !     NOFTU = upper layer temperature
    !     NOFVU = upper layer volume
    !     NOFTL = lower layer temperature
    !     NOFWT = wall surface temperatures (equivalent to the number of profiles)
    !     NOFPRD = species
    !     NOFHCL = surface deposition of hydrogen chloride
    !     NOFSMKW = surface deposition of soot
    !     NOFSMK = gas phase agglomeration of soot
    !     NEQUALS = last element in the array.

    !     The arrays which use this structure are VATOL, VRTOL, P, PDOLD, PPRIME and PDZERO

    !     An important note - solve sets the last variable to be solved to NOFPRD
    !     which is the beginning of the species (-1) and the end of the array which
    !     is presently used by DASSL. The important point is that NODES is set to
    !     NOFPRD which is the equivalent to NOFWT+NWALLS

    use precision_parameters
    use cenviro
    use cfast_main
    use cfin
    use cshell
    use dervs
    use debug
    use fltarget
    use iofiles
    use objects1
    use objects2
    use opt
    use params
    use smkview
    use solver_parameters
    use vents
    use wnodes
    implicit none
    
    integer, intent(out) :: ierror
    real(eb), intent(in) :: tstop

    integer, parameter :: maxord = 5
    integer, parameter :: lrw = 40+(maxord+4)*maxeq+maxeq**2
    integer, parameter :: liw = 20+maxeq
    integer, parameter :: all = 1, some = 0

    real(eb) :: rwork(lrw), rpar(1)
    integer :: iwork(liw), info(15), ipar(3), info2(15)
    integer :: izp0(0:maxteq), izpmxmn(0:maxteq,2)
    real(eb) :: pprime(maxteq), pdnew(maxteq), p0(maxteq), pmxmn(maxteq,2), vatol(maxeq), vrtol(maxeq)
    real(eb) :: pdzero(maxteq) = 0.0_eb
    logical :: iprint, idump, iplot, ltarg, exists, ispread,firstpassforsmokeview
    integer :: idid, i, nodes, nfires, icode, ieqmax, idisc, ires, idsave, ifdtect, ifobj, isensor, isroom, errorcode
    integer(2) :: filecount, delfilesqq
    real(eb) :: ton, toff, tpaws, tstart, tdout, dprint, dplot, ddump, dspread, t, tprint, tdump, td, &
        tplot, tspread, tout,  ostptime, tdtect, tobj, jac
    character(133) :: messg
    external resid, jac, delfilesqq
    integer :: funit

    call cptime(toff)
    ierror = 0
    tpaws = tstop + 1.0_eb
    tstart = itmstp - 1
    told = tstart
    dt = tstop - tstart
    dprint = abs(lprint)
    dplot = abs(ldiagp)
    ddump = abs(ldiago)
    dspread = abs(lcopyss)
    rpar(1) = rptol

    ! initialize print, dump, plot times
    t = tstart
    tprint = t
    tdump = t
    tplot = t
    tspread = t
    idid = 1
    firstpassforsmokeview = .true.

    ! Output options
    if (dprint<0.0001_eb.or.lprint==0) then
        iprint = .false.
        tprint = tstop + 1.0_eb
    else
        iprint = .true.
    endif

    if (dplot<0.0001_eb.or.ldiagp<=0) then
        iplot = .false.
        tplot = tstop + 1.0_eb
    else
        iplot = .true.
    endif

    if (ddump<0.0001_eb.or.ldiago<=0) then
        idump = .false.
        tdump = tstop + 1.0_eb
    else
        idump = .true.
    endif

    if (dspread<0.0001_eb.or.lcopyss<=0) then
        ispread = .false.
        tspread = tstop + 1.0_eb
    else
        ispread = .true.
    endif

    call setinfo (info, rwork)

    ! copy error tolerances into arrays. if the location of pressure is
    ! changed in the solver array then the following code has to be changed
    do i = 1, nm1
        vatol(i+nofp) = aptol
        vrtol(i+nofp) = rptol
        vatol(i+noftu) = atol
        vrtol(i+noftu) = rtol
        vatol(i+nofvu) = atol
        vrtol(i+nofvu) = rtol
        vatol(i+noftl) = atol
        vrtol(i+noftl) = rtol
        if (option(foxygen)==on) then
            vatol(i+nofoxyu)=atol
            vrtol(i+nofoxyu)=rtol
            vatol(i+nofoxyl)=atol
            vrtol(i+nofoxyl)=rtol
        endif
    end do
    do i = 1, nhvpvar
        vatol(i+nofpmv) = ahvptol
        vrtol(i+nofpmv) = rhvptol
    end do
    do i = 1, nhvtvar
        vatol(i+noftmv) = ahvttol
        vrtol(i+noftmv) = rhvttol
    end do
    do i = 1, nwalls
        vatol(i+nofwt) = awtol
        vrtol(i+nofwt) = rwtol
    end do
    do i = 1, neqtarg(mplicit)
        vatol(i+noftt) = awtol
        vrtol(i+noftt) = rwtol
    end do

    ovtime = 0.0_eb
    tottime = 0.0_eb
    prttime = 0.0_eb

    ! See note in comments about the nodes=nofprd line below
    nodes = nofprd
    ipar(1) = nodes
    ipar(2) = all
    idset = 0

    ! Setting initial vector
    call setp0(p0, izp0, pmxmn, izpmxmn, iofili, ierror)
    if (ierror>0) then
        return
    endif
    if (izp0(0)==on) then
        do i = 1, nodes
            if (izp0(i)==on) p(i) = p0(i)
        end do

        ! if we set pressures with setp0 then over-ride steady state pressure
        ! initialization
        do i = 1, nm1
            if(izp0(i+nofp)==on)option(fpsteady) = off
        end do
    endif

    ! construct initial solution
    do i = 1, nequals
        pdold(i) = 0.0_eb
        pold(i) = p(i)
    end do
    call initsoln(t,pdold,pdzero,rpar,ipar)
    do i = 1, nequals
        pprime(i) = pdold(i)
        pold(i) = p(i)
    end do

    ! Calculate the mass of objects that have been pyrolized
    ! at the moment we do only the total and the radiological species
    ! make sure that the INTEGRATE routine is called before TOXIC
    call integrate_mass (t, dt)
    call toxic(0.0_eb)

    ! If we are running only an initialization test then we do not need to solve anything
    if (initializeonly) then
        call target(steady)
        ! normally, this only needs to be done while running. however, if we are doing an initialonly run then we need the output now
        call remapfires (nfires)
        call svout(pref, exterior_abs_pressure, exterior_temperature, nm1, cxabs, cyabs, hrl, br, dr, hr, nvents, nvvent, nfires, flocal, fxlocal, fylocal, fzlocal, &
        ntarg, 0.0_eb, 1)
        icode = 0
        write (logerr, 5004)
        return
    endif
5004 format ('Initialize only')

    ! main solve loop
    numjac = 0
    numstep = 0
    numresd = 0

80  continue

    ! DASSL equation with most error
    IEQMAX = 0

    ! Check for interactive commands
    ! if a key has been pressed (and we are watching the keyboard) figure out what to do
    ! The escape key returns a code of 1
    if (.not.nokbd) call ntract(t,icode,tpaws,tout,ieqmax)
    inquire (file=stopfile, exist =exists)
    if (exists) then
        icode = 1
    endif
    ! If the stop file exists or the esc key has been pressed, then quit
    if (icode==1) then
        write (logerr, 5000) t, dt
        return
    endif
5000 format (/,'Stopped by request at T = ',1PG11.3,' DT = ',G11.3)

    ! Check the .query file. If it does not exist, do nothing. If if DOES exist, then
    ! rewind/write the status file and delete the query file (in that order).
    ! Ignore errors from deleting the file. It may not exist
    inquire (file=queryfile, exist = exists)
    if (exists) then
        call StatusOutput (T, dT, errorcode)
        filecount = delfilesqq(queryfile)
    endif
    
    !Check to see if diagnostic files .resid and .jac exist. If they do exist
    !set flags and open file, if needed, to print diagnositic information.
    inquire (file=residfile, exist=exists)
    if (exists) then
        residprn = .true.
        if (residfirst) then
            residfirst = .false.
            ioresid = funit(150)
            open(unit=ioresid,file=residcsv)
            ioslab = funit(150)
            open(unit = ioslab, file=slabcsv)
        end if
        inquire (file=jacfile,exist=exists)
        if (exists) then
            if(jacfirst) then
                jacfirst = .false.
                iojac = funit(150)
                open(unit=iojac,file=jaccsv)
            endif 
            jacprn = .true.
        else
            jacprn = .false.
        endif
    else
        residprn = .false.
        jacprn = .false.
    endif

    ! now do normal output (printout, spreadsheets, ...)
    if (idid>0) then
        ltarg = .false.

        if (t+0.0001_eb>min(tprint,tstop).and.iprint) then

            ! update target temperatures (only need to update just before we print target temperatures).
            ! if we actually use target temperatures in a calculation then this call will need to be moved to inside resid.

            if(.not.ltarg)then
                call target(steady)
                ltarg = .true.
            endif

            itmstp = tprint
            call result(t,1)
            call statusoutput (t, dt, errorcode)
            call outjcnt(t)
            tprint = tprint + dprint
            numjac = 0
            numstep = 0
            numresd = 0
            prttime = 0.0_eb
        endif

        if (t+0.0001_eb>min(tplot,tstop).and.iplot) then
            itmstp = tplot
            if(.not.ltarg)then
                call target(steady)
                ltarg = .true.
            endif
            ! note: svout writes the .smv file. we do not close the file but only rewind so that smokeview 
            ! can have the latest time step information. remapfires just puts all of the information in a single list
            call remapfires (nfires)
            call svout(pref, exterior_abs_pressure, exterior_temperature, nm1, cxabs, cyabs, hrl, br, dr, hr, nvents, nvvent, nfires, flocal, fxlocal, & 
            fylocal,fzlocal,ntarg,t,itmstp)
            ! this ought to go earlier and drop the logical test. however, not all of the information 
            ! is available until this point
            if (firstpassforsmokeview) then
                firstpassforsmokeview = .false.
                call svplothdr (version,nm1,nfires)
            endif
            call svplotdata(t,nm1,zzrelp,zzhlay(1,lower),zztemp(1,2),zztemp(1,1),nfires, fqlocal,fhlocal)
            call spreadsheetsmv(t)
            tplot = tplot + dplot
            call statusoutput (t, dt, errorcode)
        endif

        if (t+0.0001_eb>min(tspread,tstop).and.ispread) then
            itmstp = tspread
            if(.not.ltarg)then
                call target(steady)
                ltarg = .true.
            endif
            call spreadsheetnormal (t)
            call spreadsheetspecies (t)
            call spreadsheetflow (t)
            call spreadsheetflux (t)
            if (ierror/=0) return
            tspread =tspread + dspread
            call statusoutput (t, dt, errorcode)
        endif

        ! diagnostics
        if (t+0.0001_eb>tpaws) then
            itmstp = tpaws
            call result(t,1)
            call debugpr(1,t,dt,ieqmax)
            tpaws = tstop + 1.0_eb
            call statusoutput (t, dt, errorcode)
        endif

        ! find the interval next discontinuity is in
        idisc = 0
        do i = 1, izndisc
            if(t>=zzdisc(i-1).and.t<zzdisc(i))then
                idisc = i
                exit
            endif
        end do
        tout = min(tprint,tplot,tdump,tspread,tpaws,tstop)

        ! if there is a discontinuity then tell DASSL
        if(idisc/=0)then
            tout = min(tout,zzdisc(idisc))
            rwork(1) = zzdisc(idisc)
            info(4) = 1
        else
            info(4) = 0
        endif
    endif

    if (t<tstop) then
        idset = 0
        ipar(2) = some
        told = t
        call setderv(-1)
        call cptime(ton)
        call ddassl(resid,nodes,t,p,pprime,tout,info,vrtol,vatol,idid,rwork,lrw,iwork,liw,rpar,ipar,jac)
        ! call cpu timer and measure, solver time within dassl and overhead time (everything else).
        call setderv(-2)
        ieqmax = ipar(3)
        if (option(fpdassl)==on) call debugpr (3,t,dt,ieqmax)
        ostptime = ton - toff
        call cptime(toff)
        stime = t
        stptime = toff - ton
        prttime = prttime + stptime
        tottime = tottime + stptime
        ovtime = ovtime + ostptime
        tovtime = tovtime + ostptime

        ! make sure dassl is happy

        if (idid<0) then
            call fnd_comp(ieqmax)
            write (messg,101)idid
101         format('error, dassl - idid=', i3)
            call xerror(messg,0,1,1)
            ierror = idid
            return
        endif

        dt = t - told
        if(izdtflag)then
            if(dt<zzdtcrit)then
                izdtnum = izdtnum + 1
                if(izdtnum>izdtmax)then
                    ! model has hung (izdtmax consective time step sizes were below zzdtcrit)
                    write(messg,103)izdtmax,zzdtcrit,t
103                 format (i3,' consecutive time steps with size below',e11.4,' at t=',e11.4)
                    call xerror(messg,0,1,2)
                    izdtnum = 0
                endif
            else
                ! this time step is above the critical size so reset counter
                izdtnum = 0
            endif
        endif

        ipar(2) = all
        updatehall = .true.
        call resid(t,p,pdzero,pdnew,ires,rpar,ipar)
        updatehall = .false.
        call updrest(nodes, nequals, nlspct, t, told, p, pold, pdnew, pdold, pdzero)

        ! advance the detector temperature solutions and check for object ignition
        idsave = 0
        call updtect(mdchk,told,dt,ndtect,zzhlay,zztemp,xdtect,ixdtect,iquench,idset,ifdtect,tdtect)
        call updobj(mdchk,told,dt,ifobj,tobj,ierror)
        td = min(tdtect,tobj)

        ! a detector is the first thing that went off
        if (ifdtect>0.and.tdtect<=td) then
            isensor = ifdtect
            isroom = ixdtect(isensor,droom)
            call updtect(mdset,told,dt,ndtect,zzhlay,zztemp,xdtect,ixdtect,iquench,idset,ifdtect,tdtect)
            write(lbuf,*) ' '
            call xerror(lbuf,0,1,-3)
            write(lbuf,76)isensor,tdtect,isroom
76          format(' Sensor ',i3,' has activated at ',f6.1,' seconds in compartment ',i3)
            call xerror(lbuf,0,1,-3)
            ! check to see if we are backing up for detectors going off
            if (option(fbtdtect)==on) then
                idsave = idset
            else
                idsave = ifobj
                td = tobj
                call resid (t, p, pdzero, pdnew, ires, rpar, ipar)
                idset = 0
            endif
        else
            call updtect(mdupdt,told,dt,ndtect,zzhlay,zztemp,xdtect,ixdtect,iquench,idset,ifdtect,tdtect)
        endif

        ! object ignition is the first thing to happen
        if (ifobj>0.and.tobj<=td) then
            call updobj(mdset,told,dt,ifobj,tobj,ierror)
            write(iofilo,5003) ifobj,trim(objnin(ifobj)),max(tobj,0.0_eb) ! this prevents printing out a negative activation time
5003        format(/,' Object #',i3,' (',a,') ignited at ', f10.3,' seconds')
            ! check to see if we are backing up objects igniting
            if (option(fbtobj)==on) then
                idsave = ifobj
            else
                idsave = idset
                td = tdtect
                objon(ifobj) = .true.
                objset(ifobj) = 0
                call setinfo(info,rwork)
                ifobj = 0
            endif
        else
            call updobj(mdupdt,told,dt,ifobj,tobj,ierror)
        endif

        if (idsave/=0)then

            ! a detector has activated so call dassl to integrate backwards
            ! in time to t=td.  this is better than using simple linear interpolation
            ! because in general dassl could be taking very big time steps
            if(told<=td.and.td<t)then
                call result(t,1)
                ipar(2) = some
                tdout = td
                do i = 1, 11
                    info2(i) = 0
                end do
                info2(2) = 1
                told = t
                call ddassl(resid,nodes,t,p,pprime,tdout,info2,vrtol,vatol,idid,rwork,lrw,iwork,liw,rpar,ipar,jac)

                ! make sure dassl is happy (again)
                if (idid<0) then
                    call fnd_comp(ipar(3))
                    write (messg,101)idid
                    call xerror(messg,0,1,-2)
                    write(messg,'(a13,f10.5,1x,a8,f10.5)')'Backing from ',t,'to time ',tdout
                    call xerror(messg,0,1,1)
                    call xerror('Error in DASSL while backing up', 0,1,1)
                    ierror = idid
                    return
                endif

                ! reset dassl flags to integrate forward from t=td and
                ! call resid to get product info at sprinkler activation time
                if (ifdtect>0) idset = idsave
                dt = t - told
                ipar(2) = all

                ! call resid to get product info at the correct time and
                ! to save fire release rates in room where detector has
                ! activated.  (this happens because idset /= 0)
                call resid (t, p, pdzero, pdnew, ires, rpar, ipar)
                call updrest(nodes, nequals, nlspct, t, told, p, pold, pdnew, pdold, pdzero)
                call setinfo(info,rwork)
            else if (td==t) then
                call setinfo(info,rwork)
                call resid (t, p, pdzero, pdnew, ires, rpar, ipar)
            else
                ! updtect said that a sprinkler has gone off but the time is wrong!!
                write(messg,'(a7,f10.5,a13,f10.5,a22,f10.5)') 'Time = ' ,t,' Last time = ',told,' need to back step to ',td
                call xerror(messg,0,1,1)
                call xerror('Back step to large',0,1,1)
                call xerror(' ',0,1,1)
                ierror = idid
                return
            endif
            do  i = 1, mxoin
                objset(i) = 0
            end do
        endif

        ! calculate the mass of objects that have been pyrolized
        ! at the moment we do only the total and the radiological species
        ! It is important to call the routine to integrate the mass before call the toxicology calculatino
        call integrate_mass (t, dt)

        ! calculate gas dosage
        call toxic(dt)

        if (option(fdebug)==on) call debugpr(2,t,dt,ieqmax)
        numstep = numstep + 1
        go to 80
    endif
    return

    end

! --------------------------- updrest -------------------------------------------

    subroutine updrest(nodes, nequals, nlspct,  t, told, p, pold, pdnew, pdold, pdzero)

    !     routine: updrest
    !     purpose: update solution returned by dassl

    use precision_parameters
    use fltarget
    implicit none

    integer, intent(in) :: nodes, nequals, nlspct
    real(eb), intent(in) :: pdzero(*)
    real(eb), intent(in) :: t, told
    
    real(eb), intent(out) :: p(*), pold(*), pdold(*), pdnew(*)
    
    integer :: i
    real(eb) :: dt 

    dt = t - told

    ! advance species
    do i = nodes + 1, nequals
        p(i) = p(i) + dt*pdold(i)
        p(i) = max (0.0_eb, p(i))
        pdold(i) = pdnew(i)
    end do

    ! advance explicit target temperatures and update implicit temperatures
    call trheat(1,xplicit,dt,pdzero,pdnew)
    call trheat(1,mplicit,dt,pdzero,pdnew)
    if (nlspct>0) call resync(p,nodes+1)

    do i = 1, nequals
        pold(i) = p(i)
    end do

    return
    end subroutine updrest

! --------------------------- ntract -------------------------------------------

    subroutine ntract(t,icode,tpaws,tout,ieqmax)

    !     routine: ntract
    !     purpose: keyboard routine for user interaction during simulation

    use precision_parameters
    use cenviro
    use cfast_main
    use dervs
    use opt
    implicit none

    integer, intent(in) :: ieqmax
    real(eb), intent(in) :: t
    
    integer, intent(out) :: icode
    real(eb), intent(out) :: tpaws, tout

    logical :: slvhelp
    integer(2) :: ch, hit
    real(eb) :: rcode

    icode = 0
    call grabky(ch,hit)
    if (hit>0) then
        if (ch==27) then
            icode = 1
            return
        elseif (hit>1) then
            if (option(fkeyeval)==on) then
                if (ch==59) then
                    write (*,5010) t, dt
                    if (slvhelp()) icode = 1
                else if (ch==60) then
                    if (option(fdebug)==on) then
                        option(fdebug) = off
                        write (*,*) 'debug is now off'
                        write (*,*)
                    else
                        option(fdebug) = on
                    endif
                else if (ch==61) then
                    switch(1,nr) = .not. switch(1,nr)
                    write (*,*) 'toggle flow field printing to ',switch(1,nr)
                else if (ch==62) then
                    call debugpr(1,t,dt,ieqmax)
                else if (ch==63) then
                    write (*,5010) t, dt
                else if (ch==64) then
                    write (*,5010) t, dt
                    write (*,*) 'enter time at which to pause: '
                    read (*,*) rcode
                    tpaws = rcode
                    tout = min(tpaws,tout)
                else if (ch==65) then
                    if (option(fpdassl)==on) then
                        option(fpdassl) = off
                        write (*,*) 'dassl debug is now off'
                    else
                        option(fpdassl) = on
                    endif
                endif
            else
                write (*,5010) t, dt
            endif
        endif
    endif

    return
5010 format (' time = ',1pg12.4,', dt = ',1pg12.4)
    end subroutine ntract

! --------------------------- slvhelp -------------------------------------------

    logical function slvhelp()

    !     Routine: slvhelp
    !     Purpose: quick output of keyboard shortcuts available during simulaiton

    use cenviro
    use cfast_main
    use cshell
    use opt
    implicit none

    integer(2) :: ch, hit
    integer :: ii

    write (iofilo,*) '***Options Set***'
    write (iofilo,'(1x,20i3)') (option(ii),ii = 1,mxopt)
    write (iofilo,*) '************************************************************'
    write (iofilo,*) '1=Help,2=debug,3=flow,4=pause,5=time,6=pause time,7=dassl(t)'
    write (iofilo,*) 'Press <esc> to quit, any other key to continue'
    write (iofilo,*) '************************************************************'

10  call grabky(ch,hit)
    if (hit==0) go to 10
    if (ch==27) then
        slvhelp = .true.
        write (iofilo,*) 'Run terminated at user request'
    else
        slvhelp = .false.
        write (iofilo,*) 'continuing'
        write (iofilo,*)
    endif
    return
    end

! --------------------------- setinfo -------------------------------------------

    subroutine setinfo(info,rwork)

    !     routine: setinfo
    !     purpose: update solution flags for dassl solver

    use precision_parameters
    use cparams
    use solver_parameters
    implicit none

    integer, intent(out) :: info(*)
    real(eb), intent(out) :: rwork(*)
    
    integer :: i

    do i = 1, 11
        info(i) = 0
    end do
    info(3) = 1
    info(2) = 1
    if (stpmax<=0.0_eb) then
        info(7) = 0
    else
        info(7) = 1
        rwork(2) = stpmax
    endif
    if (dasslfts<0.0_eb) then
        info(8) = 0
    else
        info(8) = 1
        rwork(3) = dasslfts
    endif

    ! setting jacobian flag
    info(5) = 0
    info(11) = 1
    return
    end

! --------------------------- resid -------------------------------------------

    subroutine resid (tsec,x,xpsolve,delta,ires,rpar,ipar)


    !     Routine: cfast resid
    !     Purpose: Calculates the residual F(t,y,dy/dt) for CFAST
    !              differential and algebraic equations.  For the gas
    !              differential equations (pressure, layer volume,
    !              upper/lower layer temperature) F(t,y,dy/dt) takes
    !              the form F(t,y,dy/dt) = dy/dt - f(t,y) where f(t,y) is
    !              related to the conservation of mass and and energy.
    !              For the wall temperature equations, F is just Fourier's
    !              law taking the form of
    !              F(t,y,dy/dt) = q''(t,y) + K dT/dx
    !              where q'' is the flux striking the wall, K is the wall's
    !              thermal conductivity and dT/dx is the surface wall
    !              temperature gradient.
    !     Revision: $Revision$
    !     Revision Date: $Date$
    !     Arguments: TSEC    Current simulation time (T above in s)
    !                X       Current guess at solution vector (Y above)
    !                XPSOLVE XPSOLVE Current guess at derivative of solution
    !                        vector (Y' above)
    !                DELTA   Residual or value of F(t,y,dy/dt)
    !                IRES    Outputs:  IRES    Integer flag which is always equal to
    !                        zero on input. RESID should alter IRES
    !                        only if it encounters an illegal value of Y or
    !                        a stop condition. Set IRES = -1 if an input
    !                        value is illegal, and DDASSL will try to solve
    !                        the problem without getting IRES = -1. If
    !                        IRES = -2, DASSL return control to the calling
    !                        program with IDID = -11.
    !                RPAR    real parameter arrays
    !                IPAR    integer parameter arrays
    !                        These are used for communication between SOLVE and
    !                        RESID via DASSL. They are not altered by DASSL.
    !                        Currently, only IPAR is used in RESID to pass
    !                        a partial/total flag for solution of the
    !                        species equations.

    use precision_parameters
    use cenviro
    use cfast_main
    use dervs
    use flwptrs
    use fltarget
    use opt
    use params
    use debug
    implicit none

    real(eb), intent(in) :: tsec, x(*), xpsolve(*), rpar(*) 
    integer, intent(in) :: ipar(*)

    integer, intent(out) :: ires
    real(eb), intent(out) :: delta(*)
    
    integer, parameter :: all = 1, some = 0, uu = upper ,ll = lower

    ! data structures for dassl, the numerical solver
    real(eb) :: xprime(maxteq)

    ! data structure for total flows and fluxes
    real(eb) :: flwtot(nr,mxprd+2,2), flxtot(nr,nwal)

    ! data structures for flow through vents
    real(eb) :: flwnvnt(nr,mxprd+2,2)
    real(eb) :: flwhvnt(nr,ns+2,2)

    ! data structures for fires
    real(eb) :: flwf(nr,ns+2,2)

    ! data structures for convection, radiation, and ceiling jets
    real(eb) :: flwcv(nr,2), flxcv(nr,nwal)
    real(eb) :: flwrad(nr,2), flxrad(nr,nwal)
    real(eb) :: flwcjet(nr,2), flxcjet(nr,nwal)

    ! data structures for mechanical vents
    real(eb) :: flwmv(nr,ns+2,2), filtered(nr,ns+2,2)

    ! data structures for hcl deposition
    real(eb) :: flwhcl(nr,ns+2,2), flxhcl(nr,4)

    ! data structures for door jet fires
    real(eb) :: flwdjf(nr,ns+2,2)
    integer :: update

    logical :: vflowflg, hvacflg, djetflg
    integer :: nprod, nirm, i, iroom, iprod, ip, ierror, j, iwall, nprodsv, iprodu, iprodl, iwhcl
    real(eb) :: epsp, xqu, aroom, hceil, pabs, hinter, ql, qu, tmu, tml, oxydu, oxydl, pdot, tlaydu, tlaydl, vlayd, prodl, produ, xmu

    ierror = 0
    nprod = nlspct
    dt = tsec - told
    numresd = numresd + 1
    stime = tsec

    nirm = nm1

    call datacopy(x,odevara)
    call datacopy(x,odevarb)

    ! If RESID is called by SOLVE then IPAR(2)==ALL all residuals
    ! are computed.  If RESID is called by DASSL residuals are not
    ! computed for species.  Further, temperature profiles are only
    ! updated when RESID is called by SOLVE.

    if (ipar(2)==some) then
        update = 0
        prnslab = .false.
    else
        update = 1
        if (residprn) then
            prnslab = .true.
        else
            prnslab = .false.
        end if
        dbtime = tsec
    endif

    epsp = rpar(1)

    do i = 1, n
        qf(i) = 0.0_eb
    end do

    ! calculate flow due to unforced vents (hflow for horizontal flow
    ! through vertical vents and vflow for vertical flow through
    ! horizontal vents)
    call hflow (tsec,epsp,nprod,flwnvnt)
    call vflow (tsec,flwhvnt,vflowflg)
    call mvent (tsec,x(nofpmv+1),x(noftmv+1),xpsolve(noftmv+1),flwmv,delta(nofpmv+1),delta(noftmv+1),xprime(nofhvpr+1),nprod,ierror,hvacflg,filtered)

    if (ierror/=0) then
        ires = -2
        return
    endif

    ! calculate heat and mass flows due to fires
    call fires (tsec,flwf)
    call sortfr (nfire,ifroom,xfire,ifrpnt,nm1)
    call djet (flwdjf,djetflg)

    ! calculate flow and flux due to heat transfer (ceiling jets, convection and radiation
    call cjet (flwcjet,flxcjet)
    call cvheat (flwcv,flxcv)
    call rdheat (flwrad,flxrad,ierror)
    if (ierror/=0) then
        ires = -2
        return
    endif

    ! calculate hcl deposition to walls
    call hcl (flwhcl, flxhcl,ierror)
    if (ierror/=0) then
        ires = -2
        return
    endif

    ! reset parallel data structures
    do i = 1, nm1
        ! add in vent fires to the total.  dofire does the total of
        ! qf for normal fires, but vent fires are done afterwards with djet
        do j = 1, nwal
            qscnv(j,i) = flxcjet(i,j) + flxcv(i,j)
        end do
    end do
    if(djetflg)then
        do i = 1, nm1
            qf(i) = qf(i) + flwdjf(i,q,ll) + flwdjf(i,q,uu)
        end do
    endif

    ! sum flow for inside rooms
    do iroom = 1, nirm

        do iprod = 1, nprod + 2
            ip = izpmap(iprod)
            flwtot(iroom,iprod,ll) = flwnvnt(iroom,iprod,ll) + flwf(iroom,ip,ll)
            flwtot(iroom,iprod,uu) = flwnvnt(iroom,iprod,uu) + flwf(iroom,ip,uu)
        end do
        if(vflowflg)then
            do iprod = 1, nprod + 2
                ip = izpmap(iprod)
                flwtot(iroom,iprod,ll) = flwtot(iroom,iprod,ll) + flwhvnt(iroom,ip,ll)
                flwtot(iroom,iprod,uu) = flwtot(iroom,iprod,uu) + flwhvnt(iroom,ip,uu)
            end do
        endif
        if(hvacflg)then
            do iprod = 1, nprod + 2
                ip = izpmap(iprod)
                flwtot(iroom,iprod,ll) = flwtot(iroom,iprod,ll) + flwmv(iroom,ip,ll) - filtered(iroom,iprod,ll)
                flwtot(iroom,iprod,uu) = flwtot(iroom,iprod,uu) + flwmv(iroom,ip,uu) - filtered(iroom,iprod,uu)
            end do
        endif
        if(djetflg)then
            do iprod = 1, nprod + 2
                ip = izpmap(iprod)
                flwtot(iroom,iprod,ll) = flwtot(iroom,iprod,ll) + flwdjf(iroom,ip,ll)
                flwtot(iroom,iprod,uu) = flwtot(iroom,iprod,uu) + flwdjf(iroom,ip,uu)
            end do
        endif

        ! add in hcl contribution to flwtot
        if (activs(6)) then
            flwtot(iroom,1,ll) = flwtot(iroom,1,ll)+flwhcl(iroom,1,ll)
            flwtot(iroom,1,uu) = flwtot(iroom,1,uu)+flwhcl(iroom,1,uu)
            flwtot(iroom,8,ll) = flwtot(iroom,8,ll)+flwhcl(iroom,8,ll)
            flwtot(iroom,8,uu) = flwtot(iroom,8,uu)+flwhcl(iroom,8,uu)
        endif

        flwtot(iroom,q,ll) = flwtot(iroom,q,ll) + flwcv(iroom,ll) + flwrad(iroom,ll) + flwcjet(iroom,ll)
        flwtot(iroom,q,uu) = flwtot(iroom,q,uu) + flwcv(iroom,uu) + flwrad(iroom,uu) + flwcjet(iroom,uu)
        

        ! if this room is a shaft then solve for only one zone.
        ! this is done by combining flows from to both
        ! layers into upper layer flow and setting lower layer flow to
        ! zero.
        if(izshaft(iroom)==1)then
            do iprod = 1, nprod + 2
                flwtot(iroom,iprod,uu) = flwtot(iroom,iprod,uu) + flwtot(iroom,iprod,ll)
                flwtot(iroom,iprod,ll) = 0.0_eb
            end do
        endif

        ! calculate temperature of flow going into the upper layer
        ! of each room
        if(jaccol<=0)then
            xqu = flwtot(iroom,q,upper)
            xmu = flwtot(iroom,m,upper)
            if(xmu/=0.0_eb)then
                zzftemp(iroom,upper) = xqu/(cp*xmu)
            else
                zzftemp(iroom,upper) = interior_temperature
            endif
        endif


    end do

    if (update==all) then
        if (residprn) then
            call spreadsheetresid(tsec, flwtot, flwnvnt, flwf, flwhvnt, flwmv, filtered, flwdjf, flwcv, flwrad, flwcjet)
        endif
    endif
    ! sum flux for inside rooms
    do iroom = 1, nirm
        do iwall = 1, nwal
            if (switch(iwall,iroom)) then
                flxtot(iroom,iwall) = flxcv(iroom,iwall) + flxrad(iroom,iwall) + flxcjet(iroom,iwall)
            endif
        end do
    end do

    ! set nprod to zero when we are only solving "some" of the ode's
    if (ipar(2)==some) then
        nprodsv = nprod
        nprod = 0
    endif

    ! calculate rhs of ode's for each room
    do iroom = 1, nirm
        aroom = ar(iroom)
        hceil = hr(iroom)
        pabs = zzpabs(iroom)
        hinter = zzhlay(iroom,ll)
        ql = flwtot(iroom,q,ll)
        qu = flwtot(iroom,q,uu)
        tmu = flwtot(iroom,m,uu)
        tml = flwtot(iroom,m,ll)

        if(option(foxygen)==on)then
            oxydu = flwtot(iroom,4,uu)
            oxydl = flwtot(iroom,4,ll)
        endif

        ! pressure equation
        if(deadroom(iroom).eq.0)then
            pdot = (gamma-1.0_eb)*(ql + qu)/(aroom*hceil)
        else
            pdot = 0.0_eb
        endif

        ! upper layer temperature equation
        tlaydu = (qu-cp*tmu*zztemp(iroom,uu))/(cp*zzmass(iroom,uu))
        if (option(fode)==on) then
            tlaydu = tlaydu + pdot/(cp*zzrho(iroom,uu))
        endif

        ! upper layer volume equation
        vlayd = (gamma-1.0_eb)*qu/(gamma*pabs)
        if (option(fode)==on) then
            vlayd = vlayd - zzvol(iroom,uu)*pdot/(gamma*pabs)
        endif
        if(izshaft(iroom)==1)vlayd = 0.0_eb

        ! lower layer temperature equation
        tlaydl = (ql-cp*tml*zztemp(iroom,ll))/(cp*zzmass(iroom,ll))
        if (option(fode)==on) then
            tlaydl = tlaydl + pdot/(cp*zzrho(iroom,ll))
        endif

        xprime(iroom) = pdot
        xprime(iroom+noftl) = tlaydl
        xprime(iroom+nofvu) = vlayd
        xprime(iroom+noftu) = tlaydu

        if(option(foxygen)==on)then
            xprime(iroom+nofoxyu) = oxydu
            xprime(iroom+nofoxyl) = oxydl
        endif
    end do

    ! compute product of combustion terms
    if (nprod>0.and.ipar(2)==all) then
        iprodu = nofprd - 1
        do iprod = 1, nprod
            do iroom = 1, nm1
                hceil = hr(iroom)
                hinter = zzhlay(iroom,ll)
                iprodu = iprodu + 2
                iprodl = iprodu + 1
                prodl = flwtot(iroom,iprod+2,ll)

                ! if this room is a hall and the jet has not reached the end
                ! of the hall then don't solve for it using dassl
                produ = flwtot(iroom,iprod+2,uu)

                if (hinter<hceil) then
                    xprime(iprodu) = produ
                else if(hinter>=hceil.and.flwtot(iroom,m,uu)<0.0_eb)  then
                    xprime(iprodu) = produ
                else
                    xprime(iprodu) = 0.0_eb
                endif
                if (hinter>0.0_eb) then
                    xprime(iprodl) = prodl
                else if (hinter<=0.0_eb.and.flwtot(iroom,m,ll)>0.0_eb) then
                    xprime(iprodl) = prodl
                else
                    xprime(iprodl) = 0.0_eb
                endif
            end do
        end do

        ! HCL deposition.  note that these are done only if hcldep is set
        if (hcldep/=0) then
            iwhcl = nofhcl
            do iroom = 1, nm1
                do iwall = 1, nwal
                    iwhcl = iwhcl + 1
                    xprime(iwhcl) = flxhcl(iroom,iwall)
                end do
            end do
        endif

        ! smoke deposition and agglomeration.
        ! note that these are done only if smkagl is set
        do i = nofsmkw + 1, nofsmkw + 4*nm1*(smkagl+smkagl)
            xprime(i) = 0.0_eb
        end do
    endif

    ! residuals for pressure
    do i = nofp + 1, nofp + nm1
        delta(i) = xprime(i) - xpsolve(i)
    end do

    ! residuals for layer volume, and layer temperatures
    do i = noftu + 1, noftu + 3*nm1
        delta(i) = xprime(i) - xpsolve(i)
    end do

    ! residual for oxygen
    if(option(foxygen)==on)then
        do i = 1, nm1
            delta(i+nofoxyu) = xprime(i+nofoxyu) - xpsolve(i+nofoxyu)
            delta(i+nofoxyl) = xprime(i+nofoxyl) - xpsolve(i+nofoxyl)
        end do
    endif

    ! conduction residual
    call cnheat(update,dt,flxtot,delta)

    ! target residual
    call trheat(0,mplicit,dt,xpsolve,delta)

    ! residuals for stuff that is solved in solve itself, and not by dassl
    if (nprod/=0) then

        ! residuals for gas layer species
        do i = nofprd + 1, nofprd + 2*nprod*nm1
            delta(i) = xprime(i) - xpsolve(i)
        end do

        ! residuals for hcl deposition, smoke deposition and smoke agglomeration
        do i = nofhcl+1, nofhcl + 4*nm1*(hcldep+smkagl+smkagl)
            delta(i) = xprime(i) - xpsolve(i)
        end do

        ! residual for hvac species
        do i = nofhvpr+1, nofhvpr+nlspct*nhvsys
            delta(i) = xprime(i) - xpsolve(i)
        end do
    endif

    if (ipar(2)==some) then
        nprod = nprodsv
    endif

    return
    end

! --------------------------- datacopy -------------------------------------------

    subroutine datacopy(pdif,iflag)

    !     routine: cfast (main program)
    !     purpose: calculate environment variables from the solver vector

    !     arguments: pdif   solver vector
    !                iflag  action flag:
    !     iflag = constvar ==> constant data (data that does not change 
    !                          with time)
    !     iflag = odevara  ==> ode variables: pressure, temperature and upper 
    !                          layer volume
    !     iflag = odevarb  ==> species data and wall temperature profile.  
    !                          use pold and pdold to estimate species
    !     iflag = odevarc  ==> species data and wall temperature profile.
    !                          use pdif array for species

    use precision_parameters
    use wallptrs
    use cenviro
    use cfast_main
    use dervs
    use fltarget
    use opt
    use params
    use vents
    use wdervs
    use wnodes
    implicit none

    integer, intent(in) :: iflag
    real(eb), intent(in) :: pdif(*)
    
    integer frmask(mxccv)

    integer :: iroom, lsp, layer, i, j, k, iijk, itstop, iii, icol, ieq, iwall, icnt, ii, iwfar, ifromr, ifromw, itor, &
        itow, ieqfrom, ieqto, itarg, itype, ibeg, iend, npts, iwalleq, iwalleq2, iinode, ilay, isys, isof
    real(eb) :: wtemp, xwall_center, vminfrac, xx, yy, ywall_center, zz, wcos, havg, windvnew, winddp, xdelt, tstop, zzu, zzl, &
        ylay, ytarg, ppgas, totl, totu, rtotl, rtotu, oxyl, oxyu, ppwgas, pphv
        
    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    if(nfurn>0)then
        call interp(furn_time,furn_temp,nfurn,stime,1,wtemp)
        qfurnout=sigma*(kelvin_c_offset+wtemp)**4
    endif

    xwall_center = 2.0_eb
    vminfrac = 1.0e-4_eb
    if (iflag==constvar) then
        do iroom = 1, n
            zzvmin(iroom) = min(vminfrac*vr(iroom), 1.0_eb)
            zzvmax(iroom) = vr(iroom) - zzvmin(iroom)
        end do
        do iroom = 1, nm1
            roomptr=>roominfo(iroom)
            
            roomptr%yflor = hflr(iroom)
            roomptr%yceil = hrp(iroom)

            ! define wall centers
            xx = br(iroom)
            xwall_center = xx/2.0_eb
            yy = dr(iroom)
            ywall_center = yy/2.0_eb
            zz = hrp(iroom)
            roomptr%wall_center(1,1) = xwall_center
            roomptr%wall_center(1,2) = ywall_center
            roomptr%wall_center(1,3) = zz

            roomptr%wall_center(2,1) = xwall_center
            roomptr%wall_center(2,2) = yy

            roomptr%wall_center(3,1) = xx
            roomptr%wall_center(3,2) = ywall_center

            roomptr%wall_center(4,1) = xwall_center
            roomptr%wall_center(4,2) = 0.0_eb

            roomptr%wall_center(5,1) = 0.0_eb
            roomptr%wall_center(5,2) = ywall_center

            roomptr%wall_center(6,1) = xwall_center
            roomptr%wall_center(6,2) = yy

            roomptr%wall_center(7,1) = xx
            roomptr%wall_center(7,2) = ywall_center

            roomptr%wall_center(8,1) = xwall_center
            roomptr%wall_center(8,2) = 0.0_eb

            roomptr%wall_center(9,1) = 0.0_eb
            roomptr%wall_center(9,2) = ywall_center

            roomptr%wall_center(10,1) = xwall_center
            roomptr%wall_center(10,2) = ywall_center
            roomptr%wall_center(10,3) = 0.0_eb
        end do

        roomptr=>roominfo(n)
        
        roomptr%yflor = 0.0_eb
        roomptr%yceil = 100000.0_eb
        
        zzvol(n,upper) = 0.0_eb
        zzvol(n,lower) = 100000.0_eb
        zzhlay(n,upper) = 0.0_eb
        zzhlay(n,lower) = 100000.0_eb
        zzrelp(n) = 0.0_eb
        zzpabs(n) = pofset
        zztemp(n,upper) = exterior_temperature
        zztemp(n,lower) = exterior_temperature
        do lsp = 3, ns
            zzcspec(n,upper,lsp) = 0.0_eb
            zzcspec(n,lower,lsp) = 0.0_eb
            zzgspec(n,lower,lsp) = 0.0_eb
            zzgspec(n,upper,lsp) = 0.0_eb
        end do
        zzcspec(n,upper,1) = 0.770_eb
        zzcspec(n,lower,1) = 0.770_eb
        zzcspec(n,upper,2) = 0.230_eb
        zzcspec(n,lower,2) = 0.230_eb
        do layer = upper, lower
            zzrho(n,layer) = zzpabs(n)/rgas/zztemp(n,layer)
            zzmass(n,layer) = zzrho(n,layer)*zzvol(n,layer)
        end do

        ! define vent data structures
        do i = 1, mxccv
            frmask(i) = 2**i
        end do
        nvents = 0
        do i = 1, nm1
            roomptr=>roominfo(i)
            
            do j = i + 1, n
                if (nw(i,j)/=0) then
                    do k = 1, mxccv
                        if (iand(frmask(k),nw(i,j))/=0) then
                            nvents = nvents + 1
                            ventptr => ventinfo(nvents)
                            iijk = ijk(i,j,k)
                            ventptr%sill = hl(iijk)
                            ventptr%soffit = hh(iijk)
                            ventptr%width = bw(iijk)
                            
                            ventptr%from_hall_offset = halldist(iijk,1)
                            ventptr%to_hall_offset = halldist(iijk,2)
                            ventptr%from=i
                            ventptr%to=j
                            ventptr%counter=k

                            ! is "from" room a hall?
                            if(izhall(i,ihroom)==1)then
                                ventptr%is_from_hall=1
                            else
                                ventptr%is_from_hall=0
                            endif

                            ! is "to" room a hall?
                            if(izhall(j,ihroom)==1)then
                                ventptr%is_to_hall=1
                            else
                                ventptr%is_to_hall=0
                            endif

                            ! add face (vface) to the data structure
                            ventptr%face = vface(iijk)

                            ! compute pressure rise due to wind.  this value is only defined for outside rooms
                            wcos = windc(iijk)
                            if(j==n.and.wcos/=0.0_eb)then

                                ! compute wind velocity and pressure rise at the average vent height
                                havg = (ventptr%sill + ventptr%soffit)/2.0_eb 
                                havg = havg + roomptr%yflor
                                if(windrf/=0.0_eb)then
                                    windvnew = windv*(havg/windrf)**windpw
                                else
                                    windvnew = windv
                                endif
                                winddp = wcos*exterior_density*windvnew**2/2.0_eb
                                ventptr%wind_dp = winddp
                            else
                                ventptr%wind_dp = 0.0_eb
                            endif

                        endif
                    end do
                endif
            end do
        end do

        !define vents for vertical flow

        nvvent = 0
        do i = 1, n
            do j = 1, n
                if (nwv(i,j)/=0) then
                    nvvent = nvvent + 1
                    ivvent(nvvent,1) = i
                    ivvent(nvvent,2) = j
                    qcvv(1,nvvent) = qcvpp(1,i,j)
                    qcvv(2,nvvent) = qcvpp(2,i,j)
                    qcvv(3,nvvent) = qcvpp(3,i,j)
                    qcvv(4,nvvent) = qcvpp(4,i,j)
                endif
            end do
        end do

        ! define discontinuity array.  first we look at vent openings

        xdelt = nsmax/deltat
        itstop = xdelt + 1
        tstop = itstop - 1

        zzdisc(0) = 0.0_eb
        zzdisc(1) = tstop
        iii = 1

        ! add each of the change arrays to the discontinuity list
        do  i = 1, nvents
            iii = iii + 1
            zzdisc(iii) = qcvh(1,i)
            iii = iii + 1
            zzdisc(iii) = qcvh(3,i)
        end do
        do  i = 1, nvvent
            iii = iii + 1
            zzdisc(iii) = qcvv(1,i)
            iii = iii + 1
            zzdisc(iii) = qcvv(3,i)
        end do
        do  i = 1, nfan
            iii = iii + 1
            zzdisc(iii) = qcvm(1,i)
            iii = iii + 1
            zzdisc(iii) = qcvm(3,i)
        end do
        do i = 1, nfilter
            iii = iii + 1
            zzdisc(iii) = qcvf(1,i)
            iii = iii + 1
            zzdisc(iii) = qcvf(3,i)
        end do
        izndisc = iii

        ! put the discontinuity array into order
        call shellsort (zzdisc(0), izndisc+1)

        ! define izwmap for jac and other constants for the custom linear
        ! algebra routines that are called in dassl
        icol = 0
        ieq = nofwt
        ! set izwmap2 for the outside room first
        do iwall = 1,4
            izwmap2(iwall,nm1+1) = 0
        end do
        do iroom = 1, nm1
            icnt = 0
            iznwall(iroom) = 0
            do iwall = 1, 4
                if (switch(iwall,iroom)) then
                    ieq = ieq + 1
                    izwmap2(iwall,iroom) = ieq
                    icnt = icnt + 1
                    icol = icol + 1
                    iznwall(iroom) = iznwall(iroom) + 1

                    ! define izwall, to describe ceiling-floor connections
                    ! first assume that walls are connected to the outside
                    ii = ieq - nofwt
                    izwall(ii,w_from_room) = iroom
                    izwall(ii,w_from_wall) = iwall
                    izwall(ii,w_to_room) = nm1 + 1
                    if(iwall==1.or.iwall==2)then
                        iwfar = 3 - iwall
                    else
                        iwfar = iwall
                    endif
                    izwall(ii,w_to_wall) = iwfar
                    izwall(ii,w_boundary_condition) = iwbound

                else
                    izwmap2(iwall,iroom) = 0
                endif
            end do
            izwmap(1,iroom) = icol - icnt + 1
            izwmap(2,iroom) = icnt
        end do

        ! update izwall for ceiling/floors that are connected 
        do i = 1, nswal
            ifromr = izswal(i,w_from_room)
            ifromw = izswal(i,w_from_wall)
            itor = izswal(i,w_to_room)
            itow = izswal(i,w_to_wall)
            ieqfrom = izwmap2(ifromw,ifromr) - nofwt
            ieqto = izwmap2(itow,itor) - nofwt

            izwall(ieqfrom,w_to_room) = itor
            izwall(ieqfrom,w_to_wall) = itow
            izwall(ieqfrom,w_boundary_condition) = 1

            izwall(ieqto,w_to_room) = ifromr
            izwall(ieqto,w_to_wall) = ifromw
            izwall(ieqto,w_boundary_condition) = 1

        end do 

        jacn1 = nofpmv - nofp
        jacn2 = nofwt - nofpmv
        jacn3 = nofprd - nofwt
        jacdim = jacn1 + jacn2 + jacn3

        ! define maps for dassl eqs <--> target data structures
        ieq = 0
        do itarg = 1, ntarg
            if(ixtarg(trgmeth,itarg)==mplicit)then
                ieq = ieq + 1
                iztarg(itarg) = ieq
            else
                iztarg(itarg) = 0
            endif
        end do

        ! associate an equation type (ie pressure, temperature etc as defined by offsets)
        ! with each dassl equation
        ieq = 0
        do itype = 1, neqoff
            ibeg = nofsets(itype)
            iend = nofsets(itype+1)-1
            do i = ibeg, iend
                ieq = ieq + 1
                iroom = i + 1 - ibeg
                izeqmap(ieq,1) = itype
                izeqmap(ieq,2) = iroom
            end do
        end do

        ! indicate which rooms are connected to an hvac system
        do i = 1, nm1
            izhvac(i) = .false.
        end do
        do ii = 1, next
            i = hvnode(1,ii)
            izhvac(i) = .true.
        end do

    else if (iflag==odevara) then
        do iroom = 1, nm1
            roomptr=>roominfo(iroom)
            
            zzvol(iroom,upper) = max(pdif(iroom+nofvu),zzvmin(iroom))
            zzvol(iroom,upper) = min(zzvol(iroom,upper),zzvmax(iroom))
            zzvol(iroom,lower) = max(vr(iroom)-zzvol(iroom,upper),zzvmin(iroom))
            zzvol(iroom,lower) = min(zzvol(iroom,lower),zzvmax(iroom))

            ! prevent flow from being withdrawn from a layer if the layer
            ! is at the minimum size
            volfru(iroom) = (zzvol(iroom,upper)-vminfrac*vr(iroom))/vr(iroom)*(1.0_eb-2.0_eb*vminfrac)
            volfru(iroom) = max(min(volfru(iroom),1.0_eb),0.0_eb)
            volfrl(iroom) = 1.0_eb - volfru(iroom)
            volfrl(iroom) = max(min(volfrl(iroom),1.0_eb),0.0_eb)

            ! calculate layer height for non-rectangular rooms
            npts = izrvol(iroom)
            if(npts==0)then
                zzhlay(iroom,upper) = zzvol(iroom,upper)/ar(iroom)
                zzhlay(iroom,lower) = zzvol(iroom,lower)/ar(iroom)
            else
                call interp(zzrvol(1,iroom),zzrhgt(1,iroom),npts,zzvol(iroom,lower),1,zzhlay(iroom,lower))
                zzhlay(iroom,upper) = hr(iroom) - zzhlay(iroom,lower)
            endif

            zzrelp(iroom) = pdif(iroom)
            zzpabs(iroom) = pdif(iroom) + pofset
            zztemp(iroom,upper) = pdif(iroom+noftu)
            zztemp(iroom,lower) = pdif(iroom+noftl)

            ! there is a problem with how flow is being withdrawn from layers
            ! when the layers are small and the flow is large (for example with
            ! ceiling vents.  as a result, dassl, can predict a negative temperature
            ! (because the rhs of the temperature equation is wrong).  the following
            ! code causes the temperature of the opposite layer to be used in these
            ! situations.
            if(zztemp(iroom,upper)<0.0_eb)then
                zztemp(iroom,upper)=zztemp(iroom,lower)
            endif
            if(zztemp(iroom,lower)<0.0_eb)then
                zztemp(iroom,lower)=zztemp(iroom,upper)
            endif
            if(izshaft(iroom)==1)then
                zztemp(iroom,lower) = zztemp(iroom,upper)
            endif

            ! compute area of 10 wall segments
            xx = br(iroom)
            yy = dr(iroom)
            zzu = zzhlay(iroom,upper)
            zzl = zzhlay(iroom,lower)
            zzwarea2(iroom,1) = ar(iroom)
            zzwarea2(iroom,2) = zzu*xx
            zzwarea2(iroom,3) = zzu*yy
            zzwarea2(iroom,4) = zzu*xx
            zzwarea2(iroom,5) = zzu*yy
            zzwarea2(iroom,6) = zzl*xx
            zzwarea2(iroom,7) = zzl*yy
            zzwarea2(iroom,8) = zzl*xx
            zzwarea2(iroom,9) = zzl*yy
            zzwarea2(iroom,10) = ar(iroom)

            ! compute area of 4 wall segments
            zzwarea(iroom,1) = ar(iroom)
            zzwarea(iroom,2) = ar(iroom)
            zzwarea(iroom,3) = (yy + xx)*zzu*xwall_center
            zzwarea(iroom,4) = max(0.0_eb,(yy+xx)*zzl*xwall_center)

            ! define z wall centers (the z coordinate changes with time)
            ! (other coordinates are static and are defined earlier)

            do i = 1, 4
                ylay = zzhlay(iroom,lower)
                roomptr%wall_center(i+1,3) =  (roomptr%yceil+ylay)/2.0_eb
                roomptr%wall_center(i+5,3) = ylay/2.0_eb
            end do

            do layer = upper, lower
                zzrho(iroom,layer) = zzpabs(iroom)/rgas/zztemp(iroom,layer)
                zzmass(iroom,layer) = zzrho(iroom,layer)*zzvol(iroom,layer)
            end do
        end do
        
        do i = 1, nm1
            if(deadroom(i).eq.0)cycle
            zzrelp(i) = zzrelp(deadroom(i))
            zzpabs(i) = zzpabs(deadroom(i))
        end do

        ! record which layer target is in
        do itarg = 1, ntarg
            iroom = ixtarg(trgroom,itarg)
            ylay = zzhlay(iroom,lower)
            ytarg = xxtarg(trgcenz,itarg)
            if(ytarg>=ylay)then
                ixtarg(trglayer,itarg) = upper
            else
                ixtarg(trglayer,itarg) = lower
            endif
        end do

        ! stuff dassl estimate of target temperature's solved implicitly
        ! (ie solved by dassl)

        do itarg = 1, ntarg
            if(ixtarg(trgmeth,itarg)==mplicit) then
                ieq = iztarg(itarg)
                xxtarg(trgtempf,itarg) = p(ieq+noftt)
            endif
        end do

        ! define surface wall temperatures (interior=1,exterior=2)
    else if (iflag==odevarb.or.iflag==odevarc) then
        isof = nofwt
        do iroom = 1, nm1
            do iwall = 1, nwal
                iwalleq = izwmap2(iwall,iroom)
                if(iwalleq/=0)then
                    ieqfrom = iwalleq - nofwt
                    ifromr = izwall(ieqfrom,w_from_room)
                    ifromw = izwall(ieqfrom,w_from_wall)
                    itor = izwall(ieqfrom,w_to_room)
                    itow = izwall(ieqfrom,w_to_wall)
                    zzwtemp(iroom,iwall,1) = pdif(iwalleq)
                    iwalleq2 = izwmap2(itow,itor)
                    iinode = numnode(1,iwall,iroom)
                    if(iwalleq2==0)then
                        zzwtemp(iroom,iwall,2) = twj(iinode,iroom,iwall)
                    else
                        zzwtemp(iroom,iwall,2) = pdif(iwalleq2)
                    endif
                else

                    ! if we're not solving for the wall temperature then set it
                    ! to the layer temperature that it is adjacent too.  note,
                    ! zzwtemp(iroom,iwall,2) is only referenced if the iwall'th
                    ! wall in room iroom is being solved with the heat equation
                    if(iwall==1.or.iwall==3)then
                        ilay = upper
                    else
                        ilay = lower
                    endif
                    zzwtemp(iroom,iwall,1) = zztemp(iroom,ilay)
                endif
            end do
        end do

        ! define species amounts
        isof = nofprd
        do lsp = 1, ns
            if (activs(lsp)) then
                do iroom = 1, nm1
                    isof = isof + 1
                    if (iflag==odevarb) then
                        ppgas = pold(isof) + dt*pdold(isof)
                    else
                        ppgas = pdif(isof)
                    endif
                    zzgspec(iroom,upper,lsp) = max(ppgas,0.0_eb)
                    isof = isof + 1
                    if (iflag==odevarb) then
                        ppgas = pold(isof) + dt*pdold(isof)
                    else
                        ppgas = pdif(isof)
                    endif
                    zzgspec(iroom,lower,lsp) = max(ppgas,0.0_eb)
                end do
            endif
        end do

        ! define species mass fractions: normalize to total product mass 
        ! rather than total mass (this is equivalent to what was begin done 
        ! in chemie)
        do iroom = 1, nm1
            totl = 0.0_eb
            totu = 0.0_eb
            do lsp = 1, min(9,ns)
                if (activs(lsp)) then
                    totu = totu + zzgspec(iroom,upper,lsp)
                    totl = totl + zzgspec(iroom,lower,lsp)
                endif
            end do
            rtotl = 1.0_eb
            rtotu = 1.0_eb
            if (totl>0.0_eb) rtotl = 1.0_eb/totl
            if (totu>0.0_eb) rtotu = 1.0_eb/totu
            do lsp = 1, ns
                if (activs(lsp)) then
                    zzcspec(iroom,upper,lsp) = zzgspec(iroom,upper,lsp)*rtotu
                    zzcspec(iroom,lower,lsp) = zzgspec(iroom,lower,lsp)*rtotl
                    if(izshaft(iroom)==1)then
                        zzcspec(iroom,lower,lsp) = zzcspec(iroom,upper,lsp)
                    endif
                endif
            end do

            ! if oxygen is a dassl variable then use dassl solve array to define
            ! zzgspec and zzcspec values for oxygen.
            ! make sure oxygen never goes negative
            if(option(foxygen)==on)then
                oxyl = max(p(iroom+nofoxyl),0.0_eb)
                oxyu = max(p(iroom+nofoxyu),0.0_eb)
                zzgspec(iroom,lower,2) = oxyl
                zzgspec(iroom,upper,2) = oxyu
                zzcspec(iroom,lower,2) = oxyl/zzmass(iroom,lower)
                zzcspec(iroom,upper,2) = oxyu/zzmass(iroom,upper)
                if(izshaft(iroom)==1)then
                    zzcspec(iroom,lower,2) = zzcspec(iroom,upper,2)
                endif
            endif
        end do

        ! define hcl absorption
        if (activs(6)) then
            isof = nofhcl
            do iroom = 1, nm1
                do lsp = 1, nwal
                    isof = isof + 1
                    if (iflag==odevarb) then
                        ppwgas = pold(isof) + dt*pdold(isof)
                    else
                        ppwgas = pdif(isof)
                    endif
                    zzwspec(iroom,lsp) = ppwgas
                end do
            end do
        endif
    endif

    ! copy hvac product values for each hvac system

    if (nhvsys/=0.and.ns/=0) then
        isof = nofhvpr
        do isys = 1, nhvsys
            zzhvm(isys) = 0.0_eb
        end do
        do lsp = 1, ns
            if (activs(lsp)) then
                do isys = 1, nhvsys
                    isof = isof + 1
                    if (iflag==odevarb) then
                        pphv = max(0.0_eb,pold(isof)+dt*pdold(isof))
                    else
                        pphv = max(0.0_eb,pdif(isof))
                    endif
                    zzhvpr(isys,lsp) = pphv
                    zzhvm(isys) = zzhvm(isys) + zzhvpr(isys,lsp)
                end do
            endif
        end do
    endif
    return
    end

! --------------------------- resync -------------------------------------------

    subroutine resync(pdif,ibeg)

    !     routine: resync
    !     purpose: resyncronize the total mass of the
    !              species with that of the total mass to insure overall and individual mass balance

    !     arguments: pdif   the p array to resync
    !                ibeg   the point at which species are started in p array

    use precision_parameters
    use cenviro
    use cfast_main
    implicit none

    integer, intent(in) :: ibeg
    real(eb), intent(out) :: pdif(*)
    
    real(eb) :: factor(nr,2)
    integer :: iroom, isof, iprod

    do iroom = 1,nm1
        factor(iroom,upper) = 0.0_eb
        factor(iroom,lower) = 0.0_eb
    end do

    isof = ibeg
    do iprod = 1, min(ns,9)
        if (activs(iprod)) then
            do iroom = 1, nm1
                factor(iroom,upper) = factor(iroom,upper) + pdif(isof)
                isof = isof + 1
                factor(iroom,lower) = factor(iroom,lower) + pdif(isof)
                isof = isof + 1
            end do
        endif
    end do

    do iroom = 1, nm1
        if (factor(iroom,upper)>0.0_eb.and.zzmass(iroom,upper)>0.0_eb) then
            factor(iroom,upper) = zzmass(iroom,upper)/factor(iroom,upper)
        else
            factor(iroom,upper) = 1.0_eb
        endif
        if (factor(iroom,lower)>0.0_eb.and.zzmass(iroom,lower)>0.0_eb) then
            factor(iroom,lower) = zzmass(iroom,lower)/factor(iroom,lower)
        else
            factor(iroom,lower) = 1.0_eb
        endif
    end do

    isof = ibeg
    do iprod = 1, min(ns,9)
        if (activs(iprod)) then
            do iroom = 1, nm1
                pdif(isof) = pdif(isof)*factor(iroom,upper)
                isof = isof + 1
                pdif(isof) = pdif(isof)*factor(iroom,lower)
                isof = isof + 1
            end do
        endif
    end do

    return
    end

! --------------------------- rev_cfast -------------------------------------------

    integer function rev_cfast ()

    !     Routine: rev_cfast
    !     Purpose: return current SVN revision or date
    !     Revision: $Revision$
    !     Revision Date: $Date$

    INTEGER :: MODULE_REV, rev_auxilliary, rev_conduction, rev_convection, rev_fire, rev_flowfan, rev_flowhall, rev_flowhorizontal, rev_flowvertical, rev_initialization, &
    rev_input, rev_numerics, rev_output, rev_outputsmv, rev_outputspreadsheet, rev_radiation, rev_target, rev_ssHeaders
    CHARACTER(255) :: MODULE_DATE 
    CHARACTER(255), PARAMETER :: mainrev='$Revision$'
    CHARACTER(255), PARAMETER :: maindate='$Date$'

    WRITE(module_date,'(A)') mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
    READ (MODULE_DATE,'(I5)') MODULE_REV
    rev_cfast = max (module_rev,rev_auxilliary(),rev_conduction(),rev_convection(),rev_fire(),rev_flowfan(),rev_flowhall(),rev_flowhorizontal(), &
    rev_flowvertical(),rev_initialization(),rev_input(),rev_numerics(),rev_output(),rev_outputsmv(),rev_outputspreadsheet(),rev_radiation(),rev_target(),&
    rev_ssHeaders())
    WRITE(MODULE_DATE,'(A)') maindate
    return
    end function rev_cfast
