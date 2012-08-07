    subroutine hallht(iroom,idstart,nd)

    !     routine: hallht
    !     purpose: this routine computes the velocity and temperature
    !               of the ceiling jet at each detector location in
    !               a corridor.
    !     arguments: iroom - room number of corridor
    !                idstart - index of first detector in corridor iroom
    !                nd - number of detectors in room iroom

    use cenviro
    use cfast_main
    implicit none
    
    real*8 :: xx, yy, zz, xlen, temp, rho, vel
    integer :: id, idstart, nd, iroom

    do id = idstart, idstart + nd - 1
        xx = xdtect(id,dxloc)
        yy = xdtect(id,dyloc)
        zz = xdtect(id,dzloc)
        if(izhall(iroom,ihxy)==1)then
            xlen = xx
        else
            xlen = yy
        endif
        call halltrv(iroom,xlen,zz,temp,rho,vel)
        xdtect(id,dtjet) = temp
        xdtect(id,dvel) = vel
    end do
    return
    end

    subroutine halltrv (iroom,xloc,zloc,halltemp,hallrho,hallvel)

    use cenviro
    use cfast_main
    implicit none
    
    real*8 :: cjetheight, xloc, zloc, c1, hhalf, d0, dt0, fact, halltemp, hallrho, hallvel
    integer :: iroom, ihalf

    if(izhall(iroom,ihmode)==ihduring)then
        d0 = zzhall(iroom,ihdepth)
        cjetheight = hr(iroom) - d0

        ! location is in hall ceiling jet
        if(zloc>=cjetheight.and.xloc<=zzhall(iroom,ihdist))then
            c1 = 1.0d0
            ihalf = izhall(iroom,ihhalfflag)
            hhalf = zzhall(iroom,ihhalf)
            dt0 = zzhall(iroom,ihtemp)

            ! check to see if the user specified a hhalf value on the command line. if not (ie if ihalf==0) then calculate it using the correlations.
            if(ihalf==0)then
                ! hhalf = -log10(2)/.018
                hhalf = 16.70d0
                zzhall(iroom,ihhalf) = hhalf
            endif

            ! if hhalf < 0.0 then assume that the temperature does not decay (ie flow is adiabatic)
            if(hhalf>0.0d0)then
                fact = 0.5d0**(xloc/hhalf)
            else
                fact = 1.0d0
            endif

            halltemp = zztemp(iroom,lower) + dt0*fact
            hallrho = zzpabs(iroom)/(rgas*halltemp)
            hallvel = zzhall(iroom,ihvel)
        else
            halltemp = zztemp(iroom,lower)
            hallrho = zzrho(iroom,lower)
            hallvel = 0.10d0
        endif
    else

        ! hall jet is not flowing (either has not started or has finished) so, use regular layer temperatures and densities
        if(zloc>zzhlay(iroom,lower))then
            halltemp = zztemp(iroom,upper)
            hallrho = zzrho(iroom,upper)
        else
            halltemp = zztemp(iroom,lower)
            hallrho = zzrho(iroom,lower)
        endif
        hallvel = 0.10d0
    endif

    return
    end subroutine halltrv

    subroutine sethall(itype, inum, ihall, tsec, width, htemp, hvel, hdepth)

    use cenviro
    use cfast_main
    use dervs
    use vents
    implicit none

    real*8 :: xx0, htemp, hhtemp, roomwidth, roomlength, ventwidth, width, othird, fraction, halldepth, hdepth, hallvel, hvel, tsec, ventdist, ventdist0, ventdistmin, ventdistmax, thall0, f1, f2, cjetdist
    integer :: ihall, inum, i, itype
    
    xx0 = 0.0d0
    hhtemp = htemp - zztemp(ihall,lower)

    ! this routine is only executed if 1) hall flow has not started yet or 2)  hall flow has started and it is coming from the ivent'th vent

    if(izhall(ihall,ihventnum)/=0.and.izhall(ihall,ihventnum)/=inum)return
    roomwidth = min(br(ihall),dr(ihall))
    roomlength = max(br(ihall),dr(ihall))
    ventwidth = min(width,roomwidth)
    othird = 1.0d0/3.0d0
    fraction = (ventwidth/roomwidth)**othird
    halldepth = hdepth*fraction**2
    hallvel = hvel*fraction

    ! hall flow has not started yet
    if(izhall(ihall,ihventnum)==0)then

        ! flow is going into the hall room and flow is below the soffit
        if(hallvel>xx0.and.halldepth>xx0)then
            izhall(ihall,ihventnum) = inum
            izhall(ihall,ihmode) = ihduring
            zzhall(ihall,ihtime0) = tsec
            zzhall(ihall,ihtemp) = hhtemp
            zzhall(ihall,ihdist) = 0.0d0
            if(izhall(ihall,ihvelflag)==0)zzhall(ihall,ihvel) = hallvel
            if(izhall(ihall,ihdepthflag)==0)then
                zzhall(ihall,ihdepth) = halldepth
            endif
            ventdist0 = -1.

            ! corridor flow coming from a vent 

            if(itype==1)then
                if(izvent(inum,1)==ihall)then
                    ventdist0 = zzvent(inum,4)
                elseif(izvent(inum,2)==ihall)then
                    ventdist0 = zzvent(inum,5)
                endif
            endif

            ! corridor flow coming from the main fire. this is a restriction, but lets get it right for the main fire before we worry about objects
            if(itype==2)then
                if(izhall(ihall,ihxy)==1)then
                    ventdist0 = xfire(1,1)
                else
                    ventdist0 = xfire(1,2)
                endif
            endif
            zzhall(ihall,ihorg) = ventdist0

            ventdist = -1.0d0
            ventdistmax = ventdist 

            ! compute distances relative to vent where flow is coming from. also compute the maximum distance
            do i = 1, nvents
                if(izvent(i,1)==ihall)then

                    ! if distances are not defined for the origin or destination vent then assume that the vent at the "far" end of the corridor
                    if(zzvent(i,4)>0.0d0.and.ventdist0>=0.0d0)then
                        ventdist = abs(zzvent(i,4) - ventdist0)
                    else
                        ventdist = roomlength - ventdist0
                    endif
                    zzventdist(ihall,i) = ventdist
                elseif(izvent(i,2)==ihall)then

                    ! if distances are not defined for the origin or destination vent then assume that the vent at the "far" end of the corridor
                    if(zzvent(i,5)>0.0d0.and.ventdist0>=0.0d0)then
                        ventdist = abs(zzvent(i,5) - ventdist0)
                    else
                        ventdist = roomlength - ventdist0
                    endif
                    zzventdist(ihall,i) = ventdist
                else
                    ventdist = -1.0d0
                    zzventdist(ihall,i) = ventdist
                endif
            end do

            ! let the maximum distance that flow in a corridor can flow be the width of the room, ie:
            zzhall(ihall,ihmaxlen) = roomlength - ventdist0

            return
        endif
        return
    endif

    ! hall flow is coming from a vent or a fire
    if(izhall(ihall,ihventnum)==inum)then
        thall0 = zzhall(ihall,ihtime0)
        f1 = (told - thall0)/(stime-thall0)
        f2 = (stime - told)/(stime-thall0)
        if(izhall(ihall,ihvelflag)==0)then
            zzhall(ihall,ihvel) = zzhall(ihall,ihvel)*f1 + abs(hallvel)*f2
        endif
        if(izhall(ihall,ihdepthflag)==0)then
            zzhall(ihall,ihdepth) = zzhall(ihall,ihdepth)*f1 + halldepth*f2
        endif
        zzhall(ihall,ihtemp) = zzhall(ihall,ihtemp)*f1 + hhtemp*f2
        ventdistmax = zzhall(ihall,ihmaxlen)
        ventdistmin = roomlength - ventdistmax
        cjetdist = zzhall(ihall,ihdist) + dt*zzhall(ihall,ihvel)

        ! if ceiling jet has reached the end of the hall then indicate this fact in izhall  
        if(cjetdist>=ventdistmax)then
            izhall(ihall,ihmode) = ihafter
            cjetdist = ventdistmax
        endif
        zzhall(ihall,ihdist) = cjetdist
    endif
    return
    end

    integer function rev_flowhall

    INTEGER :: MODULE_REV
    CHARACTER(255) :: MODULE_DATE 
    CHARACTER(255), PARAMETER :: mainrev='$Revision$'
    CHARACTER(255), PARAMETER :: maindate='$Date$'

    WRITE(module_date,'(A)') mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
    READ (MODULE_DATE,'(I5)') MODULE_REV
    rev_flowhall = module_rev
    WRITE(MODULE_DATE,'(A)') maindate
    return
    end function rev_flowhall