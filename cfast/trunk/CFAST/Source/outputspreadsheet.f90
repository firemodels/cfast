
! --------------------------- SpreadSheetNormal -------------------------------------------

    subroutine SpreadSheetNormal (time)

    ! This routine writes to the {project}_n.csv file, the compartment information and the fires

    use precision_parameters
    use cenviro
    use cfast_main
    use fltarget
    use objects1
    implicit none

    real(eb), intent(in) :: time
    
    integer, parameter :: maxhead = 1+8*nr+5+9*mxfire
    real(eb) :: outarray(maxhead), fheight
    logical :: firstc
    integer :: position, i, itarg, izzvol
    
    type(room_type), pointer :: roomi

    data firstc/.true./
    save firstc

    ! headers
    if (firstc) then
        call ssHeadersNormal
        firstc = .false.
    endif

    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nm1
        roomi=>roominfo(i)
        
        itarg = ntarg - nm1 + i
        izzvol = roomi%zzvol(upper)/roomi%vr*100._eb+0.5_eb
        call ssaddtolist (position,roomi%zztemp(upper)-273.15,outarray)
        if (izshaft(i)==0) then
            call ssaddtolist(position,roomi%zztemp(lower)-273.15,outarray)
            call ssaddtolist (position,roomi%zzhlay(lower),outarray)
        endif
        call ssaddtolist (position,roomi%zzvol(upper),outarray)
        call ssaddtolist (position,roomi%zzrelp - pamb(i),outarray)
        call ssaddtolist (position,ontarget(i),outarray)
        call ssaddtolist (position,xxtarg(trgnfluxf,itarg),outarray)
    end do

    ! Fires
    do i = 1,n
        call SSaddtolist (position,fqdj(i),outarray)
    end do
    
    if (lfmax>0.and.lfbt>0.and.lfbo>0) then
        call flamhgt (fqf(0),farea(0),fheight)
        call ssaddtolist (position,fems(0),outarray)
        call ssaddtolist (position,femp(0),outarray)
        call ssaddtolist (position,fqf(0),outarray)
        call ssaddtolist (position,fheight,outarray)
        call ssaddtolist (position,fqfc(0),outarray)
        call ssaddtolist (position,objmaspy(0),outarray)
        call ssaddtolist (position,radio(0),outarray)
    endif

    if (numobjl/=0) then
        do i = 1, numobjl
            call flamhgt (fqf(i),farea(i),fheight)
            call ssaddtolist (position,fems(i),outarray)
            call ssaddtolist (position,femp(i),outarray)
            call ssaddtolist (position,fqf(i),outarray)
            call SSaddtolist (position,fqlow(i),outarray)
            call SSaddtolist (position,fqupr(i),outarray)
            call ssaddtolist (position,fheight,outarray)
            call ssaddtolist (position,fqfc(i),outarray)
            call ssaddtolist (position,objmaspy(i),outarray)
            call ssaddtolist (position,radio(i),outarray)
        end do
    endif

    call SSprintresults (21, position, outarray)

    return
    end subroutine spreadsheetnormal

! --------------------------- SSaddtolist -------------------------------------------

    subroutine SSaddtolist (ic, valu, array)

    use precision_parameters
    implicit none
    
    real(eb), intent(in) :: valu
    real(eb), intent(out) :: array(*)
    integer, intent(out) :: ic
    
    integer iounit,i

    ic = ic + 1
    ! We are imposing an arbitrary limit of 32000 columns
    if (ic>32000) return
    if (abs(valu)<=1.0d-100) then
        array(ic) = 0.0_eb
    else
        array(ic) = valu
    end if
    return

    entry SSprintresults (iounit,ic,array)

    write (iounit,"(1024(e13.6,','))" ) (array(i),i=1,ic)
    return
    
    entry SSprintresid (iounit,ic,array)

    write (iounit,"(1024(e20.13,','))" ) (array(i),i=1,ic)
    return
    end subroutine SSaddtolist

! --------------------------- SpreadSheetFlow -------------------------------------------

    subroutine SpreadSheetFlow (time)

    !	Routine to output the flow data to the flow spreadsheet {project}_f.csv

    use precision_parameters
    use cenviro
    use cfast_main
    use vents
    implicit none

    integer, parameter :: maxoutput = mxvents*4
    
    real(eb), intent(in) :: time
    real(eb) :: outarray(maxoutput),sum1,sum2,sum3,sum4,sum5,sum6, flow(6), sumin, sumout
    logical :: firstc
    data firstc /.true./
    integer :: position, irm, i,j,k,iijk,ii,iii,inode
    save firstc

    if (firstc) then
        call ssheadersflow
        firstc = .false.
    endif

    position = 0

    ! first the time
    call ssaddtolist (position,time,outarray)

    do irm = 1, n

        ! next the horizontal flow through vertical vents
        do j = 1, n
            do k = 1, mxccv
                i = irm
                if (iand(1,ishft(nw(i,j),-k))/=0) then
                    iijk = ijk(i,j,k)
                    if (i<j)then
                        sum1 = ss2(iijk) + sa2(iijk)
                        sum2 = ss1(iijk) + sa1(iijk)
                        sum3 = aa2(iijk) + as2(iijk)
                        sum4 = aa1(iijk) + as1(iijk)
                    else
                        sum1 = ss1(iijk) + sa1(iijk)
                        sum2 = ss2(iijk) + sa2(iijk)
                        sum3 = aa1(iijk) + as1(iijk)
                        sum4 = aa2(iijk) + as2(iijk)
                    endif
                    if (j==n) then
                        sumin = sum1 + sum3
                        sumout = sum2 + sum4
                        call ssaddtolist (position,sumin,outarray)
                        call ssaddtolist (position,sumout,outarray)
                    else
                        if (i<j)then
                            sum5 = sau2(iijk)
                            sum6 = asl2(iijk)
                        else
                            sum5 = sau1(iijk)
                            sum6 = asl1(iijk)
                        endif
                        ! we show only net flow in the spreadsheets
                        sumin = sum1 + sum3
                        sumout = sum2 + sum4
                        call ssaddtolist (position,sumin,outarray)
                        call ssaddtolist (position,sumout,outarray)
                        call ssaddtolist (position,sum5,outarray)
                        call ssaddtolist (position,sum6,outarray)
                    endif
                endif
            end do
        end do

        ! next natural flow through horizontal vents (vertical flow)
        do j = 1, n
            if (nwv(i,j)/=0.or.nwv(j,i)/=0) then
                do ii = 1, 6
                    flow(ii) = 0.0_eb
                end do
                if (vmflo(j,i,upper)>=0.0_eb) flow(1) = vmflo(j,i,upper)
                if (vmflo(j,i,upper)<0.0_eb) flow(2) = -vmflo(j,i,upper)
                if (vmflo(j,i,lower)>=0.0_eb) flow(3) = vmflo(j,i,lower)
                if (vmflo(j,i,lower)<0.0_eb) flow(4) = -vmflo(j,i,lower)
                ! we show only net flow in the spreadsheets
                sumin = flow(1) + flow(3)
                sumout = flow(2) + flow(4)
                call ssaddtolist (position,sumin,outarray)
                call ssaddtolist (position,sumout,outarray)
            endif
        end do

        ! finally, mechanical ventilation
        if (nnode/=0.and.next/=0) then
            do i = 1, next
                ii = hvnode(1,i)
                if (ii==irm) then
                    inode = hvnode(2,i)
                    do iii = 1, 6
                        flow(iii) = 0.0_eb
                    end do
                    if (hveflo(upper,i)>=0.0_eb) flow(1)=hveflo(upper,i)
                    if (hveflo(upper,i)<0.0_eb) flow(2)=-hveflo(upper,i)
                    if (hveflo(lower,i)>=0.0_eb) flow(3)=hveflo(lower,i)
                    if (hveflo(lower,i)<0.0_eb) flow(4)=-hveflo(lower,i)
                    sumin = flow(1) + flow(3)
                    sumout = flow(2) + flow(4)
                    flow(5) =abs(tracet(upper,i))+abs(tracet(lower,i))
                    flow(6) =abs(traces(upper,i))+abs(traces(lower,i))
                    call ssaddtolist (position, sumin, outarray)
                    call ssaddtolist (position, sumout, outarray)
                    call ssaddtolist (position, flow(5), outarray)
                    call ssaddtolist (position, flow(6), outarray)
                endif
            end do
        endif

    end do

    call ssprintresults(22, position, outarray)
    return

    end subroutine SpreadSheetFlow

! --------------------------- SpreadSheetFlux -------------------------------------------

    subroutine SpreadSheetFlux (time)

    !     Output the temperatures and fluxes on surfaces and targets at the current time

    use precision_parameters
    use cenviro
    use cfast_main
    use cshell
    use fltarget
    implicit none

    integer, parameter :: maxoutput=4*nr+9*mxtarg
    real(eb), intent(in) :: time
    
    real(eb) :: outarray(maxoutput), zdetect, tjet, vel, tlink, xact, rtotal, ftotal, wtotal, gtotal, ctotal, tttemp, tctemp, tlay, tgtemp,total,cjetmin
    integer :: iwptr(4), position,i,iw,itarg,itctemp,iroom
    external length
    data iwptr /1, 3, 4, 2/
    logical :: firstc
    type(room_type), pointer :: roomi
    
    data firstc /.true./
    save firstc

    if (firstc) then
        call ssHeadersFlux
        firstc = .false.
    endif

    position = 0

    !	First the time

    call SSaddtolist (position,time,outarray)

    !     First the temperatures for each compartment

    do i=1,nm1
        do iw = 1, 4
            call ssaddtolist (position,twj(1,i,iwptr(iw))-273.15,outarray)
        end do
    end do

    ! now do the additional targets if defined
    do i = 1, nm1
        if (ntarg>nm1) then
            do itarg = 1, ntarg-nm1
                if (ixtarg(trgroom,itarg)==i) then
                    tgtemp = tgtarg(itarg)
                    if (ixtarg(trgeq,itarg)==cylpde) then
                        tttemp = xxtarg(trgtempb,itarg)
                        itctemp = trgtempf+ xxtarg(trginterior,itarg)*(trgtempb-trgtempf)
                        tctemp = xxtarg(itctemp,itarg)
                    else
                        tttemp = xxtarg(trgtempf,itarg)
                        itctemp = (trgtempf+trgtempb)/2
                        tctemp = xxtarg(itctemp,itarg)
                    endif
                    if (ixtarg(trgeq,itarg)==ode) tctemp = tttemp
                    if (ixtarg(trgmeth,itarg)==steady) tctemp = tttemp
                    if (validate.or.netheatflux) then
                        total = gtflux(itarg,1) /1000._eb
                        ftotal = gtflux(itarg,2) /1000._eb
                        wtotal = gtflux(itarg,3) /1000._eb
                        gtotal = gtflux(itarg,4) /1000._eb
                        ctotal = gtflux(itarg,5) /1000._eb
                        rtotal = total - ctotal
                    else
                        total = xxtarg(trgtfluxf,itarg)
                        ftotal = qtfflux(itarg,1)
                        wtotal = qtwflux(itarg,1)
                        gtotal = qtgflux(itarg,1)
                        ctotal = qtcflux(itarg,1)
                        rtotal = total - ctotal
                    endif
                    call ssaddtolist (position, tgtemp-273.15, outarray)
                    call ssaddtolist (position, tttemp-273.15, outarray)
                    call ssaddtolist (position, tctemp-273.15, outarray)
                    call ssaddtolist (position, total, outarray)
                    call ssaddtolist (position, ctotal, outarray)
                    call ssaddtolist (position, rtotal, outarray)
                    call ssaddtolist (position, ftotal, outarray)
                    call ssaddtolist (position, wtotal, outarray)
                    call ssaddtolist (position, gtotal, outarray)
                endif
            end do
        endif
    end do

    !     Hallways

    !      DO 40 I = 1, NM1
    !        IF(IZHALL(I,IHROOM)==0)GO TO 40
    !        TSTART = ZZHALL(I,IHtime0)
    !        VEL = ZZHALL(I,IHVEL)
    !        DEPTH = ZZHALL(I,IHDEPTH)
    !        DIST = ZZHALL(I,IHDIST)
    !        IF(DIST>ZZHALL(I,IHMAXLEN))DIST = ZZHALL(I,IHMAXLEN)
    !        WRITE(IOFILO,5050)I,TSTART,VEL,DEPTH,DIST
    !   40 CONTINUE

    ! detectors (including sprinklers)
    cjetmin = 0.10_eb
    do i = 1, ndtect
        iroom = ixdtect(i,droom)
        roomi=>roominfo(iroom)
        
        zdetect = xdtect(i,dzloc)
        if(zdetect>roomi%zzhlay(lower))then
            tlay = roomi%zztemp(upper)
        else
            tlay = roomi%zztemp(lower)
        endif
        xact = ixdtect(i,dact)
        tjet = max(xdtect(i,dtjet),tlay)
        vel = max(xdtect(i,dvel),cjetmin)
        tlink =  xdtect(i,dtemp)
        call ssaddtolist(position, tlink-273.15, outarray)
        call ssaddtolist(position, xact, outarray)
        call ssaddtolist(position, tjet-273.15, outarray)
        call ssaddtolist(position, vel, outarray)
    end do

    call ssprintresults (24, position, outarray)
    return

5050 format(4x,i2,7x,1pg10.3,5x,1pg10.3,3x,1pg10.3,5x,1pg10.3)
    end subroutine spreadsheetflux

! --------------------------- SpreadSheetSpecies -------------------------------------------

    subroutine SpreadSheetSpecies (time)

    !	Write out the species to the spread sheet file

    use precision_parameters
    use cenviro
    use cfast_main
    use cshell
    implicit none

    integer, parameter :: maxhead = 1+22*nr
    real(eb), intent(in) :: time
    
    real(eb) :: outarray(maxhead), ssvalue
    integer :: position, i, lsp, layer
    logical :: tooutput(ns),  molfrac(ns), firstc
    
    data tooutput /.false.,5*.true.,.false.,4*.true./
    data molfrac /3*.true.,3*.false.,2*.true.,3*.false./
    data firstc /.true./

    save outarray, firstc

    ! If there are no species, then don't do the output
    if (nlspct==0) return

    ! Set up the headings
    if (firstc) then
        call ssHeadersSpecies
        firstc = .false.
    endif

    ! From now on, just the data, please
    position = 0
    call SSaddtolist (position,time,outarray)

    do i = 1, nm1
        do layer = upper, lower
            do lsp = 1, ns
                if (layer==upper.or.izshaft(i)==0) then
                    if (tooutput(lsp)) then
                        ssvalue = toxict(i,layer,lsp)
                        if (validate.and.molfrac(lsp)) ssvalue = ssvalue*0.01_eb ! converts ppm to  molar fraction
                        if (validate.and.lsp==9) ssvalue = ssvalue *264.6903_eb ! converts od to mg/m^3 (see toxict od calculation)
                        call ssaddtolist (position,ssvalue,outarray)
                        ! we can only output to the maximum array size; this is not deemed to be a fatal error!
                        if (position>=maxhead) go to 90
                    endif
                endif
            end do
        end do
    end do

90  call SSprintresults (23,position, outarray)

    return

110 format('Exceeded size of output files in species spread sheet')
    end subroutine SpreadSheetSpecies

! --------------------------- SpreadSheetSMV -------------------------------------------

    subroutine SpreadSheetSMV (time)

    ! This routine writes to the {project}_zone.csv file, the smokeview information

    use precision_parameters
    use cenviro
    use cfast_main
    use vents
    implicit none

    integer, parameter :: maxhead = 1+7*nr+5+7*mxfire
    real(eb), intent(in) :: time
    
    real(eb) :: outarray(maxhead), fheight, factor2, qchfraction,  height, width, avent, tsec, qcvfraction, flow(4), sumin, sumout
    logical :: firstc
    integer :: position
    integer :: toprm, botrm, i, itarg, izzvol, iroom1, iroom2, ik, im, ix
    integer :: itop, ibot
    
    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomi
    
    data toprm /1/, botrm /2/
    data firstc/.true./
    save firstc

    ! Headers
    if (firstc) then
        call ssHeadersSMV(.true.)
        firstc = .false.
    endif

    position = 0
    call SSaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nm1
        roomi=>roominfo(i)
        
        itarg = ntarg - nm1 + i
        izzvol = roomi%zzvol(upper)/roomi%vr*100._eb+0.5_eb
        call ssaddtolist(position,roomi%zztemp(upper)-273.15,outarray)
        if (izshaft(i)==0) then
            call ssaddtolist(position,roomi%zztemp(lower)-273.15,outarray)
            call ssaddtolist(position,roomi%zzhlay(lower),outarray)
        endif
        call ssaddtolist(position,roomi%zzrelp - pamb(i),outarray)
        call ssaddtolist(position,toxict(i,upper,9),outarray)
        if (izshaft(i)==0) then
            call ssaddtolist(position,toxict(i,lower,9),outarray)
        endif
    end do

    ! fires
    nfire = 0
    if (lfmax>0.and.lfbt>0.and.lfbo>0) then
        nfire = nfire + 1
        call flamhgt (fqf(0),farea(0),fheight)
        call ssaddtolist (position,fqf(0)/1000.,outarray)
        call ssaddtolist (position,fheight,outarray)
        call ssaddtolist (position,xfire(1,3),outarray)
        call ssaddtolist (position,farea(0),outarray)
    endif

    if (numobjl/=0) then
        do i = 1, numobjl
            nfire = nfire + 1
            call flamhgt (fqf(i),farea(i),fheight)
            call ssaddtolist (position,fqf(i)/1000.,outarray)
            call ssaddtolist (position,fheight,outarray)
            call ssaddtolist (position,xfire(nfire,3),outarray)
            call ssaddtolist (position,farea(i),outarray)          
        end do
    endif

    ! vents
    do i = 1, nvents
        ventptr=>ventinfo(i)
        
        iroom1 = ventptr%from
        iroom2 = ventptr%to
        ik = ventptr%counter
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        factor2 = qchfraction (qcvh, ijk(im,ix,ik),time)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = factor2 * height * width
        call ssaddtolist (position,avent,outarray)       
    end do

    do i = 1, nvvent
        itop = ivvent(i,toprm)
        ibot = ivvent(i,botrm)
        avent = qcvfraction(qcvv, i, tsec) * vvarea(itop,ibot)
        call ssaddtolist (position,avent,outarray)
        flow = 0
        if (vmflo(itop,ibot,upper)>=0.0_eb) flow(1) = vmflo(itop,ibot,upper)
        if (vmflo(itop,ibot,upper)<0.0_eb) flow(2) = -vmflo(itop,ibot,upper)
        if (vmflo(itop,ibot,lower)>=0.0_eb) flow(3) = vmflo(itop,ibot,lower)
        if (vmflo(itop,ibot,lower)<0.0_eb) flow(4) = -vmflo(itop,ibot,lower)
        sumin = flow(1) + flow(3)
        sumout = flow(2) + flow(4)
        call ssaddtolist (position,sumin,outarray)
        call ssaddtolist (position,sumout,outarray)
    end do

    call ssprintresults (15, position, outarray)

    return
    end subroutine SpreadSheetSMV

! --------------------------- spreadsheetresid -------------------------------------------

    subroutine spreadsheetresid(time, flwtot, flwnvnt, flwf, flwhvnt, flwmv, filtered, flwdjf, flwcv, flwrad, flwcjet)
    
    use precision_parameters
    use debug
    use cenviro
    use cfast_main
    use fltarget
    use objects1
    implicit none
    

    real(eb), intent(in) :: time
    ! data structure for total flows and fluxes
    real(eb), intent(in) :: flwtot(nr,mxprd+2,2)

    ! data structures for flow through vents
    real(eb), intent(in) :: flwnvnt(nr,mxprd+2,2)
    real(eb), intent(in) :: flwhvnt(nr,ns+2,2)

    ! data structures for fires
    real(eb), intent(in) :: flwf(nr,ns+2,2)

    ! data structures for convection, radiation, and ceiling jets
    real(eb), intent(in) :: flwcv(nr,2)
    real(eb), intent(in) :: flwrad(nr,2)
    real(eb), intent(in) :: flwcjet(nr,2)

    ! data structures for mechanical vents
    real(eb), intent(in) :: flwmv(nr,ns+2,2), filtered(nr,ns+2,2)

    ! data structures for door jet fires
    real(eb), intent(in) :: flwdjf(nr,ns+2,2)
    
    integer, parameter :: maxhead = 1+2*(7*(ns+2)+3)*nr + 4*nr
    real(eb) :: outarray(maxhead)
    logical :: firstc
    integer :: position, i, j, k, nprod
    
    type(room_type), pointer :: roomi
    
    data firstc/.true./
    save firstc
    
    ! headers
    if (firstc) then
        call ssHeadersResid
        firstc = .false.
    endif

    nprod = nlspct
    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nm1
        roomi=>roominfo(i)
        
        call SSaddtolist (position,roomi%zzrelp,outarray)
        call SSaddtolist (position,roomi%zzvol(upper),outarray)
        call SSaddtolist(position,roomi%zztemp(upper),outarray)
        call SSaddtolist(position,roomi%zztemp(lower),outarray)
        do j = 1, 2
            do k = 1, 2
                call ssaddtolist (position,flwtot(i,k,j),outarray)
                call ssaddtolist (position,flwnvnt(i,k,j),outarray)
                call ssaddtolist (position,flwf(i,k,j),outarray)
                call ssaddtolist (position,flwhvnt(i,k,j),outarray)
                call ssaddtolist (position,flwmv(i,k,j),outarray)
                call ssaddtolist (position,filtered(i,k,j),outarray)
                call ssaddtolist (position,flwdjf(i,k,j),outarray)
            end do
            call SSaddtolist (position,flwcv(i,j),outarray)
            call SSaddtolist (position,flwrad(i,j),outarray)
            call SSaddtolist (position,flwcjet(i,j),outarray)
        end do
    end do
    ! species mass flow    
    !do i = 1, nm1
    !    do j = 1, 2
    !        do k = 3, nprod + 2
    !            call ssaddtolist (position,flwtot(i,k,j),outarray)
    !            call ssaddtolist (position,flwnvnt(i,k,j),outarray)
    !            call ssaddtolist (position,flwf(i,k,j),outarray)
    !            call ssaddtolist (position,flwhvnt(i,k,j),outarray)
    !            call ssaddtolist (position,flwmv(i,k,j),outarray)
    !            call ssaddtolist (position,filtered(i,k,j),outarray)
    !            call ssaddtolist (position,flwdjf(i,k,j),outarray)
    !        end do
    !        call SSaddtolist (position,flwcv(i,j),outarray)
    !        call SSaddtolist (position,flwrad(i,j),outarray)
    !        call SSaddtolist (position,flwcjet(i,j),outarray)
    !    end do
    !end do

    call SSprintresid (ioresid, position, outarray)

    return
    end subroutine spreadsheetresid

! --------------------------- SpreadSheetFSlabs -------------------------------------------

    subroutine SpreadSheetFSlabs (time, ir1, ir2, iv, nslab, qslab)
    
    use precision_parameters
    use debug
    use cenviro
    use cfast_main
    use fltarget
    use objects1
    use vents
    use vent_slab
    implicit none
    
    real(eb), intent(in) :: time, qslab(mxslab)
    integer, intent(in) :: ir1, ir2, iv, nslab
    
    real(eb) :: r1, r2, v, slab
    
    integer,parameter :: maxhead = 1 + mxvents*(4 + mxslab)
    real(eb) :: outarray(maxhead)
    integer :: position, i
    logical :: firstc, nwline
    data firstc /.true./
    data nwline /.true./
    save firstc, outarray, position
    
    if (firstc) then 
        call SSHeadersFSlabs
        firstc = .false.
    end if
    
    if (nwline) then 
        position = 0
        call SSaddtolist(position, time, outarray)
        nwline = .false.
    end if
    
    r1 = ir1
    r2 = ir2
    v = iv
    slab = nslab
    call SSaddtolist(position, r1, outarray)
    call SSaddtolist(position, r2, outarray)
    call SSaddtolist(position, v, outarray)
    call SSaddtolist(position, slab, outarray)
    do i = 1, mxslab
        call SSaddtolist(position, dirs12(i)*qslab(i), outarray)
    end do
    return
    
    entry SSprintslab
    
        call SSprintresid(ioslab, position, outarray)
        nwline = .true.
    
    return
    end subroutine SpreadSheetFSlabs

! --------------------------- rev_outputspreadsheet -------------------------------------------

    integer function rev_outputspreadsheet ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_outputspreadsheet = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_outputspreadsheet
    