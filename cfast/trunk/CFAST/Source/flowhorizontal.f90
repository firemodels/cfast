    subroutine hflow(tsec,epsp,nprod,uflw)

    !     routine: hflow
    !     purpose: physical interface routine to calculate flow through all unforced vertical vents (horizontal flow).
    !     it returns rates of mass and energy flows into the layers from all vents in the building.
    !     revision: $revision: 461 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: tsec  current simulation time (s)
    !                epsp  pressure error tolerance
    !                nprod
    !                uflw

    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use params
    use vents
    use vent_slab
    use debug
    implicit none

    real(8) :: conl(mxprd,2), conu(mxprd,2), pmix(mxprd)
    real(8) :: uflw(nr,mxprd+2,2), uflw3(2,mxprd+2,2), uflw2(2,mxprd+2,2)
    real(8) :: yflor(2), yceil(2), ylay(2), pflor(2)
    real(8) :: denl(2), denu(2), tu(2), tl(2)
    real(8) :: rslab(mxslab), tslab(mxslab), yslab(mxslab),xmslab(mxslab), qslab(mxslab), cslab(mxslab,mxprd),pslab(mxslab,mxprd)
    real(8) :: vntopn(nv)
    real(8) :: uflw0(nr,ns+2,2)
    save uflw0
    logical :: ventflg(mxvent), roomflg(nr), anyvents
    real(8) :: factor2, qchfraction, height, width
    integer :: nirm, ifrom, iprod, i, iroom, iroom1, iroom2, ik, im, ix, nslab, nneut, iijk, nprod
    real(8) :: xx0, yvbot, yvtop, tsec, avent, epsp, ventvel, ventheight, vlayerdepth
    integer :: errorcode

    ! temporary declaration
    nirm = nm1

    xx0 = 0.0d0
    do ifrom = 1, nirm
        do iprod = 1, nprod + 2
            uflw(ifrom,iprod,lower) = xx0
            uflw(ifrom,iprod,upper) = xx0
        end do
    end do
    if (option(fhflow)/=on) return

    call ventflag(ventflg,roomflg,anyvents)
    if(anyvents)then
        do i = 1, nvents
            if(.not.ventflg(i)) go to 80
            iroom1 = izvent(i,1)
            iroom2 = izvent(i,2)
            ik = izvent(i,3)

            ! setup data structures for from room
            call getvar(i,iroom1,iroom2,nprod,yflor(1),yceil(1),ylay(1),pflor(1),denl(1),denu(1),conl(1,1),conu(1,1),tl(1),tu(1))

            ! setup data structures for to room
            call getvar(i,iroom2,iroom1,nprod,yflor(2),yceil(2),ylay(2),pflor(2),denl(2),denu(2),conl(1,2),conu(1,2),tl(2),tu(2))

            ! convert vent dimensions to absolute dimensions
            yvbot = zzvent(i,1) + yflor(1)
            yvtop = zzvent(i,2) + yflor(1)
            ylay(1) = ylay(1) + yflor(1)
            ylay(2) = ylay(2) + yflor(2)

            !  use new interpolator to find vent opening fraction
            im = min(iroom1,iroom2)
            ix = max(iroom1,iroom2)
            factor2 = qchfraction (qcvh, ijk(im,ix,ik),tsec)
            height = zzvent(i,2) - zzvent(i,1)
            width = zzvent(i,3)
            avent = factor2 * height * width

            ! augment floor pressure in the second room by the pressure induced by wind.
            ! (note this augmentation will be different for each vent)
            pflor(2) = pflor(2) + zzvent(i,6)
            if (avent>=1.d-10) then
                call vent(yflor,ylay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,nprod,mxprd,mxslab,epsp,cslab,pslab,qslab, &
                vss(1,i),vsa(1,i),vas(1,i),vaa(1,i),dirs12,dpv1m2,rslab,tslab,yslab,yvelev,xmslab,nslab,nneut,ventvel)
                
                if (prnslab) then
                    call SpreadSheetfslabs(dbtime, iroom1, iroom2, ik, nslab, qslab, errorcode)
                endif

                ! update hall info for vents connected from fire room to hall

                if(updatehall)then
                    ventheight = yvtop - yvbot
                    if(izvent(i,4)==1)then
                        vlayerdepth = yvtop - ylay(2)
                        if(vlayerdepth>ventheight)vlayerdepth = ventheight
                        call sethall(1,i,iroom1,tsec,width,tslab(nslab),-ventvel,vlayerdepth)
                    endif
                    if(izvent(i,5)==1)then
                        vlayerdepth = yvtop - ylay(1)
                        if(vlayerdepth>ventheight)vlayerdepth = ventheight
                        call sethall(1,i,iroom2,tsec,width,tslab(nslab),ventvel,vlayerdepth)
                    endif
                endif

                ! copy flows into the cfast data structure. this data structure is for reporting purposes only;

                iijk = ijk(iroom1,iroom2,ik)
                ss1(iijk) = vss(1,i)
                ss2(iijk) = vss(2,i)
                sa1(iijk) = vsa(1,i)
                sa2(iijk) = vsa(2,i)
                as1(iijk) = vas(1,i)
                as2(iijk) = vas(2,i)
                aa1(iijk) = vaa(1,i)
                aa2(iijk) = vaa(2,i)

                !call flogo1(dirs12,yslab,xmslab,nslab,ylay,qslab,pslab,mxprd,nprod,mxslab,uflw2)
                call flogo2(dirs12,yslab,xmslab,tslab,nslab,tu,tl,yflor,yceil,ylay,qslab,pslab,mxprd,nprod,mxslab,uflw2)

                !  calculate entrainment type mixing at the vents

                if (option(fentrain)==on) then
                    call entrain(dirs12,yslab,xmslab,nslab,tu,tl,cp,ylay,conl,conu,pmix,mxprd,nprod,yvbot,yvtop,uflw3,vsas(1,i),vasa(1,i))
                    sau1(iijk) = vsas(2,i)
                    sau2(iijk) = vsas(1,i)
                    asl1(iijk) = vasa(2,i)
                    asl2(iijk) = vasa(1,i)
                else
                    sau1(iijk) = xx0
                    sau2(iijk) = xx0
                    asl1(iijk) = xx0
                    asl2(iijk) = xx0
                endif

                ! sum flows from both rooms for each layer and type of product
                ! (but only if the room is an inside room)

                if (iroom1>=1.and.iroom1<=nirm) then
                    do iprod = 1, nprod + 2
                        uflw(iroom1,iprod,lower) = uflw(iroom1,iprod,lower) + uflw2(1,iprod,l)
                        uflw(iroom1,iprod,upper) = uflw(iroom1,iprod,upper) + uflw2(1,iprod,u)
                    end do
                    if (option(fentrain)==on) then
                        do iprod = 1, nprod + 2
                            uflw(iroom1,iprod,lower) = uflw(iroom1,iprod,lower) + uflw3(1,iprod,l)
                            uflw(iroom1,iprod,upper) = uflw(iroom1,iprod,upper) + uflw3(1,iprod,u)
                        end do
                    endif
                endif
                if (iroom2>=1.and.iroom2<=nirm) then
                    do iprod = 1, nprod + 2
                        uflw(iroom2,iprod,lower) = uflw(iroom2,iprod,lower) + uflw2(2,iprod,l)
                        uflw(iroom2,iprod,upper) = uflw(iroom2,iprod,upper) + uflw2(2,iprod,u)
                    end do
                    if (option(fentrain)==on) then
                        do iprod = 1, nprod + 2
                            uflw(iroom2,iprod,lower) = uflw(iroom2,iprod,lower) + uflw3(2,iprod,l)
                            uflw(iroom2,iprod,upper) = uflw(iroom2,iprod,upper) + uflw3(2,iprod,u)
                        end do
                    endif
                endif
            else
                iijk = ijk(iroom1,iroom2,ik)
                ss1(iijk) = xx0
                ss2(iijk) = xx0
                sa1(iijk) = xx0
                sa2(iijk) = xx0
                as1(iijk) = xx0
                as2(iijk) = xx0
                aa1(iijk) = xx0
                aa2(iijk) = xx0
                sau1(iijk) = xx0
                sau2(iijk) = xx0
                asl1(iijk) = xx0
                asl2(iijk) = xx0
            endif
80          continue
        end do
    endif

    if(option(fmodjac)==on)then
        if(jaccol==0)then

            ! we need to save the solution for later jacobian calculations
            do iroom = 1, nm1
                do iprod = 1, nprod + 2
                    uflw0(iroom,iprod,lower) = uflw(iroom,iprod,lower)
                    uflw0(iroom,iprod,upper) = uflw(iroom,iprod,upper)
                end do
            end do
        elseif(jaccol>0)then

            ! we are computing a jacobian, so get previously save solution for rooms that are not affected by perturbed solution variable
            do iroom = 1, nm1
                if(.not.roomflg(iroom))then
                    do iprod = 1, nprod + 2
                        uflw(iroom,iprod,lower) = uflw0(iroom,iprod,lower)
                        uflw(iroom,iprod,upper) = uflw0(iroom,iprod,upper)
                    end do
                endif
            end do
        endif
    endif
    
    if (prnslab) then
        call SSprintslab
    end if
    return
    end subroutine hflow

    subroutine entrain(dirs12,yslab,xmslab,nslab,tu,tl,cp,ylay,conl,conu,pmix,mxprd,nprod,yvbot,yvtop,uflw3,vsas,vasa)

    !     routine: entrain
    !     purpose: 
    !     arguments: dirs12 - a measure of the direction of the room 1 to room flow in each slab
    !                yslab  - slab heights in rooms 1,2 above datum elevation [m]
    !                xmslab - magnitude of the mass flow rate in slabs [kg/s]
    !                nslab  - number of slabs between bottom and top of vent
    !                tu     - upper layer temperature in each room [k]
    !                tl     - lower layer temperature in each room [k]
    !                ylay   - height of layer in each room above datum elevation [m]
    !                uflw3(i,1,j), i=1 or 2, j=1 or 2 (output) - mass flow rate to upper (j=2) or lower (j=1) layer of room i due to entrainment
    !                uflw3(i,2,j), i=1 or 2, j=1 or 2 (output) - enthalpy flow rate to upper (j=2) or lower (j=1) layer of room i entrainment
    !                uflw3(i,2+k,j), i=1 or 2, k=1 to nprod, j=1 or 2 (output) - product k flow rate to upper (j=2) or lower (j=1) layer of room i due entrainment

    use flwptrs
    implicit none
    integer dirs12(10)
    integer :: i, iprod, nprod, n, nslab, ifrom, ito, mxprd
    real(8) :: yslab(10), xmslab(10), pmix(mxprd)
    real(8) :: tu(2), tl(2), ylay(2)
    real(8) :: conl(mxprd,2), conu(mxprd,2)
    real(8) :: uflw3(2,mxprd+2,2)
    real(8) :: vsas(2), vasa(2)
    real(8), parameter :: xx0 = 0.0d0
    real(8) :: tmix, zd, yvbot, yvtop, cp

    ! initialize outputs
    do i = 1, 2
        do iprod = 1, nprod + 2
            uflw3(i,iprod,l) = xx0
            uflw3(i,iprod,u) = xx0
        end do
        vsas(i) = xx0
        vasa(i) = xx0
    end do

    do n = 1, nslab

        ! eliminate cases where entrainment does not occur, i.e. a slab which is adjacent to the upper layer on both sides or a slab which is adjacent to the lower layer on both sides
        if (yslab(n)<ylay(1).or.yslab(n)<ylay(2)) then
            if (yslab(n)>=ylay(1).or.yslab(n)>=ylay(2)) then

                ! slabs with no flow cause no entrainment
                if (xmslab(n)/=xx0) then

                    ! determine what room flow is coming fro
                    if (dirs12(n)==1) then
                        ifrom = 1
                        ito = 2
                    else if (dirs12(n)==0) then
                        ! no flow in this slab so we can skip it (we should never get here)
                        go to 60
                    else if (dirs12(n)==-1) then
                        ifrom = 2
                        ito = 1
                    endif

                    ! determine temperature and product concentrations of entrained flow
                    if (yslab(n)<ylay(ito)) then
                        tmix = tl(ito)
                        do iprod = 1, nprod
                            pmix(iprod) = conl(iprod,ito)
                        end do
                    else
                        tmix = tu(ito)
                        do iprod = 1, nprod
                            pmix(iprod) = conu(iprod,ito)
                        end do
                    endif

                    ! compute the size of the entrained mass flow
                    if (yslab(n)>=ylay(ifrom)) then

                        ! into upper
                        if (tu(ifrom)>tl(ito).and.xmslab(n)/=xx0) then
                            zd = max(xx0,ylay(ito)-max(yvbot,ylay(ifrom)))
                            call entrfl(tu(ifrom),tl(ito),xmslab(n),zd,uflw3(ito,m,u))
                            uflw3(ito,m,l) = -uflw3(ito,m,u)
                            vsas(ito) = uflw3(ito,m,u)
                        endif
                    else

                        ! into lower
                        if (tl(ifrom)<tu(ito).and.xmslab(n)/=xx0) then
                            ! zd = max(xx0,ylay(ifrom)-max(yvbot,ylay(ito)))

                            ! need to re-work distance zd for both into upper and into upper case.  the above doesn't work for all cases
                            zd = min(yvtop,ylay(ifrom)) - max(ylay(ito),yvbot)
                            call entrfl(tu(ito),tl(ifrom),xmslab(n),zd,uflw3(ito,m,l))

                            ! the following factor (0.25 as of 10/1/93) now multiplies the lower layer entrainment to try to approximate the reduced kelvin-helmholz type mixing.

                            uflw3(ito,m,l) = uflw3(ito,m,l) * 0.25d0
                            vasa(ito) = uflw3(ito,m,l)
                            uflw3(ito,m,u) = -uflw3(ito,m,l)
                        endif
                    endif

                    ! compute enthalpy and product flow rates of entrained flow from the mass flow rate
                    uflw3(ito,q,l) = cp * uflw3(ito,m,l) * tmix
                    uflw3(ito,q,u) = cp * uflw3(ito,m,u) * tmix
                    do iprod = 3, 2 + nprod
                        uflw3(ito,iprod,l) = uflw3(ito,m,l) * pmix(iprod-2)
                        uflw3(ito,iprod,u) = uflw3(ito,m,u) * pmix(iprod-2)
                    end do
                endif
            endif
        endif
60      continue
    end do
    return
    end subroutine entrain


    subroutine entrfl(tu,tl,fmd,z,fmz)

    !     for the reference for this correlation, see the comments
    !     in the routine "firplm."  the offset for the formulation of
    !     an equivalent door jet is provided by requiring the plume
    !     be long enough to be the appropriate plume for the fire of size
    !     qj.  note that mccaffrey's units are kilojoules.  also we assume
    !     that the plume is round as in mccaffrey's plume.  this should
    !     be modified to account for the flat plume verus the round
    !     plume in the theory.

    use cfast_main
    implicit none
    logical firstc
    real(8) :: fm1, fm2, fm3, t1, t2, a1, a2, a3, e1, e2, e3, f1, f2, xqj, tu, tl, qj, fmd, fmdqj, z0dq, zdq, zq, z, fmz, xx0
    save firstc, a1, a2, a3, e1, e2, e3, f1, f2
    data firstc /.true./

    ! define assignment statement subroutines to compute three parts of correlation
    fm1(zq) = zq ** .566d0
    fm2(zq) = zq ** .909d0
    fm3(zq) = zq ** 1.895d0

    ! first time in firplm calculate coeff's to insure that mccaffrey correlation is continuous. that is, for a1 = .011, compute a2, a3 such that

    ! a1*zq**.566 = a2*zq**.909  for zq = .08
    ! a2*zq**.909 = a3*zq**1.895 for zq = .2
    if (firstc) then

        ! reset flag so this code doesn't get executed next time in this routine
        firstc = .false.

        ! breakpoints for "forward" correlation
        t1 = .08d0
        t2 = .20d0

        ! coef's for "forward" correlation
        a1 = .011d0
        a2 = a1 * fm1(t1) / fm2(t1)
        a3 = a2 * fm2(t2) / fm3(t2)

        ! exponents for "inverse" correlation
        e1 = 1.0d0 / .566d0
        e2 = 1.0d0 / .909d0
        e3 = 1.0d0 / 1.895d0

        ! breakpoints for "inverse" correlation
        f1 = a1 * fm1(t1)
        f2 = a2 * fm2(t2)
    endif

    xqj = cp * (tu-tl) * 0.001d0
    qj = xqj * fmd
    fmdqj = 1.d0 / xqj
    if (fmdqj>=0.0d0.and.fmdqj<=f1) then
        z0dq = (fmdqj/a1) ** e1
    else if (fmdqj>f1.and.fmdqj<=f2) then
        z0dq = (fmdqj/a2) ** e2
    else
        z0dq = (fmdqj/a3) ** e3
    endif

    zdq = z / qj ** 0.4d0 + z0dq
    if (zdq>0.2d0) then
        fmz = a3 * fm3(zdq) * qj
    else if (zdq>0.08d0) then
        fmz = a2 * fm2(zdq) * qj
    else
        fmz = a1 * fm1(zdq) * qj
    endif

    xx0 = 0.0d0
    fmz = max(xx0,fmz-fmd)
    return
    end subroutine entrfl

    subroutine ventflag(ventflg,roomflg,anyvents)

    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use vents
    use vent_slab
    implicit none

    logical ventflg(mxvent), roomflg(nr), anyvents
    integer i, ieqtyp, iroom, iroom1, iroom2

    ! turn all vents on
    anyvents = .true.
    do i = 1, nvents
        ventflg(i) = .true.
    end do

    ! if the 2nd modified jacobian option is on and a jacobian is being computed (jaccol>0) then compute vent flows only for vents that that are connected
    ! to rooms whose pressure, layer height, layer temperature,  or oxygen level is being perturbed.

    if(option(fmodjac)==on)then
        if(jaccol>0)then

            ! we are computing a jacobian
            ieqtyp = izeqmap(jaccol,1)
            iroom = izeqmap(jaccol,2)
            anyvents = .false.
            do i = 1, nvents
                ventflg(i) = .false.
            end do
            do i = 1, nm1
                roomflg(i) = .false.
            end do
            if(ieqtyp==eqp.or.ieqtyp==eqtu.or.ieqtyp==eqvu.or.ieqtyp==eqtl.or.ieqtyp==eqoxyl.or.ieqtyp==eqoxyu)then

                ! determine all rooms connected to perturbed rooms
                do i = 1, nvents
                    iroom1 = izvent(i,1)
                    iroom2 = izvent(i,2)
                    if(iroom==iroom1.or.iroom==iroom2)then
                        roomflg(iroom1) = .true.
                        roomflg(iroom2) = .true.
                    endif
                end do
                roomflg(nm1+1) = .false.

                ! determine all vents connected to the above rooms
                do i = 1, nvents
                    iroom1 = izvent(i,1)
                    iroom2 = izvent(i,2)
                    if(roomflg(iroom1).or.roomflg(iroom2))then
                        ventflg(i) = .true.
                        anyvents = .true.
                    endif
                end do
            endif
        endif
    endif

    return
    end subroutine ventflag

    subroutine vent(yflor,ylay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,nprod,mxprd,mxslab,epsp,cslab,pslab,qslab, &
    vss,vsa,vas,vaa,dirs12,dpv1m2,rslab,tslab,yslab,yvelev,xmslab,nslab,nneut,ventvel)

    !     routine: vent
    !     purpose: calculation of the flow of mass, enthalpy, oxygen and other products of combustion through a vertical,
    !              constant-width vent in a wall segment common to two rooms. the subroutine uses input data describing the two-layer
    !              environment in each of the two rooms and other input data calculated in subroutine comwl1.
    !     arguments: yflor - height of floor above datum elevation [m]
    !                ylay  - height of layer above datum elevation [m]
    !                tu    - upper layer temperature [k]
    !                tl    - lower layer temperature [k]
    !                denl  - lower layer density [kg/m**3]
    !                denu  - upper layer density [kg/m**3]
    !                pflor - pressure at floor above datum pressure [kg/(m*s**2) = pascal]
    !                yvtop - elevation of top of vent above datum elevation [m]
    !                yvbot - elevation of bottom of vent above datum elevation [m]
    !                avent - area of the vent [m**2]
    !                dp1m2 - pressure in room 1 - pressure in room 2 at elevations yelev [kg/(m*s**2) = pascal]
    !                cp    - specific heat [w*s/(kg*k)]
    !                conl  - concentration of each product in lower layer [unit of product/(kg layer)]
    !                conu  - concentration of each product in upper layer [unit of product/(kg layer)]
    !                nprod - number of products in current scenario
    !                mxprd - maximum number of products currently available
    !                mxslab- maximum number of slabs currently available
    !                epsp  - error tolerance for pressures at floor
    !                cslab (output) - concentration of other products in each slab [unit product/(kg slab)]
    !                pslab (output) - amount of other products in each slab [unit product/s]
    !                qslab (output) - enthalpy flow rate in each slab [w]
    !                dirs12 (output) - a measure of the direction of the room 1 to room 2 flow in each slab
    !                rslab (output) - density of the flow in each slab [kg/m**3]
    !                tslab (output) - absolute temperature of the flow in each slab [k]
    !                yslab (output) - elevations above the datum elevation of the centroids of momentum of each slab [m]
    !                yvelev - elevations above the datum elevations of vent boundaries, layers, and neutral planes [m]
    !                xmslab - magnitude of the mass flow rate in slabs [kg/s]
    !                nvelev - number of unique elevations delineating slabs
    !                nslab  - number of slabs between bottom and top of the vent

    implicit none
    integer dirs12(*), nelev, nneut, i, nslab, n, jroom, iprod, nprod, nvelev, mxprd, mxslab
    real(8) :: yflor(*), ylay(*), tu(*), tl(*), denl(*), denu(*), pflor(*)
    real(8) ::  pslab(mxslab,*), cslab(mxslab,*), conl(mxprd,2), conu(mxprd,2), yelev(10), dp1m2(10), dpv1m2(10), xmslab(*), yn(10), rslab(*), tslab(*), yslab(*), yvelev(*)
    real(8) :: qslab(*), vss(2), vsa(2), vas(2), vaa(2)
    real(8) :: ventvel, yvbot, yvtop, epsp, xx0, dpp, ptest, p1, p2, p1rt, p2rt, r1, y1, y2, cvent, area, avent, r1m8, cp, sum, ys

    ventvel = 0.0d0

    ! create initial elevation height array (ignoring neutral planes)
    call getelev(yvbot,yvtop,ylay,yelev,nelev)

    ! find pressure drops at above elevations
    call delp(yelev,nelev,yflor,ylay,denl,denu,pflor,epsp,dp1m2)

    ! find neutral planes

    nvelev = 1
    nneut = 0
    xx0 = 0.0d0
    do i = 1, nelev - 1
        yvelev(nvelev) = yelev(i)
        dpv1m2(nvelev) = dp1m2(i)
        nvelev = nvelev + 1

        ! a neutral plane lies between two elevations having opposite signed pressure drops
        if (dp1m2(i)*dp1m2(i+1)<0.0d0) then
            nneut = nneut + 1
            dpp = dp1m2(i) - dp1m2(i+1)
            yn(nneut) = (yelev(i+1)*dp1m2(i)-yelev(i)*dp1m2(i+1)) / dpp

            ! fail safe in case interpolation calculation fails
            if (yn(nneut)<yelev(i).or.yn(nneut)>yelev(i+1)) then
                yn(nneut) = (yelev(i)+yelev(i+1)) / 2.0d0
            endif
            yvelev(nvelev) = yn(nneut)
            dpv1m2(nvelev) = 0.0d0
            nvelev = nvelev + 1
        endif
    end do
    yvelev(nvelev) = yelev(nelev)
    dpv1m2(nvelev) = dp1m2(nelev)
    nslab = nvelev - 1
    do i = 1, nslab
        yslab(i) = (yvelev(i)+yvelev(i+1)) / 2.0d0
    end do

    ! initialize cfast data structures for flow storage
    do n = 1, nslab

        ! determine whether temperature and density properties should come from room 1 or room 2
        ptest = dpv1m2(n+1) + dpv1m2(n)
        if (ptest>0.0d0) then
            jroom = 1
            dirs12(n) = 1
        else if (ptest<0.0d0) then
            dirs12(n) = -1
            jroom = 2
        else
            dirs12(n) = 0
            jroom = 1
        endif

        ! determine whether temperature and density properties should come from upper or lower layer
        if (yslab(n)<=ylay(jroom)) then
            tslab(n) = tl(jroom)
            rslab(n) = denl(jroom)
            do iprod = 1, nprod
                cslab(n,iprod) = conl(iprod,jroom)
            end do
        else
            tslab(n) = tu(jroom)
            rslab(n) = denu(jroom)
            do iprod = 1, nprod
                cslab(n,iprod) = conu(iprod,jroom)
            end do
        endif

        ! for nonzero-flow slabs determine xmslab(n) and yslab(n)
        xmslab(n) = 0.0d0
        qslab(n) = 0.d0
        do iprod = 1, nprod
            pslab(n,iprod) = 0.0d0
        end do
        p1 = abs(dpv1m2(n))
        p2 = abs(dpv1m2(n+1))
        p1rt = sqrt(p1)
        p2rt = sqrt(p2)

        ! if both cross pressures are 0 then then there is no flow
        if (p1>xx0.or.p2>xx0) then
            r1 = max(rslab(n),xx0)
            y2 = yvelev(n+1)
            y1 = yvelev(n)
            cvent = .70d0

            area = avent * (y2-y1) / (yvtop-yvbot)
            r1m8 = 8.0d0*r1
            xmslab(n) = cvent * sqrt(r1m8) * area * (p2+p1rt*p2rt+p1) / (p2rt+p1rt) / 3.0d0
            ventvel = 0.0d0
            if(n==nslab)then
                if(area/=0.0d0.and.r1/=0.0d0)then
                    ventvel = xmslab(n)/(area*r1)
                    if(dirs12(n)<0)ventvel = -ventvel
                endif
            endif
            qslab(n) = cp * xmslab(n) * tslab(n)
            sum = 0.0d0
            do iprod = 1, nprod
                pslab(n,iprod) = cslab(n,iprod) * xmslab(n)
                sum = sum + pslab(n,iprod)
            end do
        endif

        ! construct cfast data structures ss, sa, as, aa
        ys = yslab(n)
        if (ys>max(ylay(1),ylay(2))) then
            if (dirs12(n)>0) then
                vss(1) = xmslab(n)
            else
                vss(2) = xmslab(n)
            endif
        else if (ys<min(ylay(1),ylay(2))) then
            if (dirs12(n)>0) then
                vaa(1) = xmslab(n)
            else
                vaa(2) = xmslab(n)
            endif
        else if (ys>ylay(1)) then
            if (dirs12(n)>0) then
                vsa(1) = xmslab(n)
            else
                vas(2) = xmslab(n)
            endif
        else if (ys>ylay(2)) then
            if (dirs12(n)>0) then
                vas(1) = xmslab(n)
            else
                vsa(2) = xmslab(n)
            endif
        endif
    end do
    return
    end subroutine vent

    subroutine getelev(yvbot,yvtop,ylay,yelev,nelev)
    implicit none
    real(8) :: yelev(*), ylay(*), ymin, ymax, yvbot, yvtop
    integer :: nelev

    ymin = min(ylay(1),ylay(2))
    ymax = max(ylay(1),ylay(2))
    if (ymax>=yvtop.and.(ymin>=yvtop.or.ymin<=yvbot)) then
        nelev = 2
        yelev(1) = yvbot
        yelev(2) = yvtop
    else if (ymax<=yvbot) then
        nelev = 2
        yelev(1) = yvbot
        yelev(2) = yvtop
    else
        if (ymax>=yvtop.and.ymin>yvbot) then
            nelev = 3
            yelev(1) = yvbot
            yelev(2) = ymin
            yelev(3) = yvtop
        else if (ymin<=yvbot.and.ymax<yvtop) then
            nelev = 3
            yelev(1) = yvbot
            yelev(2) = ymax
            yelev(3) = yvtop
        else
            nelev = 4
            yelev(1) = yvbot
            yelev(2) = ymin
            yelev(3) = ymax
            yelev(4) = yvtop
        endif
    endif
    return
    end

    subroutine getvar(ivent,iroom,iroom2,nprod,yflor,yceil,ylay,pflor,denl,denu,conl,conu,tl,tu)

    !     routine: getvar
    !     purpose: routine to interface between global data structures and natural vent data structures.
    !     arguments: ivent - vent number
    !                iroom - room number
    !                yflor   height of floor above datum elevation [m]
    !                yceil - height of ceiling above datum elevation [m]
    !                ylay    height of layer above datum elevation [m]
    !                pflor   pressure at floor relative to ambient [p]
    !                denl    density of lower layer [kg/m**3]
    !                denu    density of upper layer [kg/m**3]
    !                conl    concentration of lower layer for each product [unit of product/kg of layer]
    !                conu    concentration of upper layer for each product [unit of product/kg of layer]
    !                tl      temperature of lower layer [k]
    !                tu      temperature of upper layer [k]

    use cenviro
    use cfast_main
    use vents
    implicit none

    real(8) :: conl(mxprd), conu(mxprd), yflor, yceil, ylay, pflor, denl, denu, tl, tu, xx0, ventdist, time0, vel, cjetdist, zloc, rhou, hallvel
    logical :: hallflag
    integer :: up, ivent, iroom, iprod, ip, iroom2, nprod

    xx0 = 0.0d0
    hallflag = .false.

    ! for rooms that are halls only use upper layer properties if the ceiling jet is beyond the vent
    up = upper

    if (iroom<n) then
        yflor = zzyflor(iroom)
        yceil = zzyceil(iroom)
        pflor = zzrelp(iroom)
        ylay = zzhlay(iroom,lower)

        ! this is a hall, the vent number is defined and flow is occuring
        if(izhall(iroom,ihroom)==1.and.ivent/=0.and.izhall(iroom,ihmode)==ihduring)then
            ventdist = zzventdist(iroom,ivent)
            if(ventdist>xx0)then
                time0 = zzhall(iroom,ihtime0)
                vel = zzhall(iroom,ihvel)
                cjetdist = vel*(stime-time0)
                if(cjetdist<ventdist)then
                    up = lower
                else
                    up = upper
                    hallflag = .true.
                endif
            else
                up = lower
            endif
        endif

        denu = zzrho(iroom,up)
        denl = zzrho(iroom,lower)
        do iprod = 1, nprod
            ip = izpmap(iprod+2) - 2
            conl(iprod) = zzcspec(iroom,lower,ip)
            conu(iprod) = zzcspec(iroom,up,ip)
        end do
        tu = zztemp(iroom,up)
        tl = zztemp(iroom,lower)
        zloc = hr(iroom) - zzhall(iroom,ihdepth)/2.0d0
        if(hallflag)then
            call halltrv(iroom,cjetdist,zloc,tu,rhou,hallvel)
        endif
    else
        yflor = zzyflor(iroom2)
        yceil = zzyceil(iroom2)
        pflor = epa(iroom2)
        ylay = zzhlay(iroom,lower)
        denu = era(iroom2)
        denl = era(iroom2)
        do iprod = 1, nprod
            ip = izpmap(iprod+2) - 2
            conl(iprod) = zzcspec(iroom,lower,ip)
            conu(iprod) = zzcspec(iroom,up,ip)
        end do
        tu = eta(iroom2)
        tl = eta(iroom2)
    endif
    return
    end subroutine getvar

    
    subroutine flogo2(dirs12, yslab, xmslab, tslab, nslab, tu, tl, yflor, yceil, ylay, qslab, pslab, mxprd, nprod, mxslab, uflw2)

    !     routine: flogo2
    !     purpose: deposition of mass, enthalpy, oxygen, and other product-of-combustion flows passing between two rooms
    !              through a vertical, constant-width vent.  this version implements the ccfm rules for flow depostion. 
    !     arguments: dirs12 - a measure of the direction of the room 1 to room 2 flow in each slab
    !                yslab - slab heights in rooms 1,2 above datum elevation [m]
    !                xmslab - mass flow rate in slabs [kg/s]
    !                nslab  - number of slabs between bottom and top of vent
    !                tu     - upper layer temperature in each room [K]
    !                tl     - lower layer temperature in each room [K]
    !                yflor  - height of floor in each room above datum elevation [m]
    !                yceil  - height of ceiling in each room above datum elevation [m]
    !                ylay   - height of layer in each room above datum elevation [m]
    !                qslab  - enthalpy flow rate in each slab [w]
    !                pslab  - flow rate of product in each slab [(unit of product/s]
    !                mxprd  - maximum number of products currently available.
    !                nprod  - number of products
    !                mxslab - maximum number of slabs currently available.
    !                uflw2(i,1,j), i=1 or 2, j=1 or 2 (output) - mass flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [kg/s]
    !                uflw2(i,2,j), i=1 or 2, j=1 or 2 (output) - enthalpy flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [w]
    !                uflw2(i,3,j), i=1 or 2, j=1 or 2 (output) - oxygen flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [(kg oxygen)/s]
    !                uflw2(i,3+k,j), i=1 or 2, k=2 to nprod, j=1 or 2 (output) - product k flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [(unit product k)/s]

    use flwptrs
    implicit none
    integer dirs12(*)
    integer :: i, iprod, nprod, n, nslab, ifrom, ito, ilay, mxprd, mxslab
    real(8) :: yslab(*), xmslab(*), tslab(*), qslab(*), tu(*), tl(*), yflor(*), yceil(*), ylay(*), pslab(mxslab,*), uflw2(2,mxprd+2,2), ff(2), fl, fu, xmterm, qterm
    real(8) :: ylayer, ylow, yup, ttr, tts, ttu, ttl, yslabf, yslabt, yslab1, yslab2

    ! initialize outputs
    do i = 1, 2
        do iprod = 1, nprod + 2
            uflw2(i,iprod,l) = 0.0d0
            uflw2(i,iprod,u) = 0.0d0
        end do
    end do

    ! put each slab flow into appropriate layer of room i to and take slab flow out of appropriate layer of room ifrom
    do n = 1, nslab

        ! determine where room flow is coming from
        if (dirs12(n)==1) then
            ifrom = 1
            ito = 2
        else if (dirs12(n)==-1) then
            ifrom = 2
            ito = 1
        else

            ! no flow in this slab so we can skip it
            go to 70
        endif

        ! apportion flow between layers of destination room
        ylayer = ylay(ito)
        ylow = yflor(ito)
        yup = yceil(ito)
        tts = tslab(n)
        ttu = tu(ito)
        ttl = tl(ito)
        if (ito==1) then
            yslabt = yslab(n)
            yslabf = yslab(n)
        else
            yslabt = yslab(n)
            yslabf = yslab(n)
        end if
        
        ! no upper layer
        if (ylayer.ge.yup) then
            if (tts>ttl+1.0d0) then
                fu = 1.0d0
                ttr = ttl
            else
                fu = 0.0d0
                ttr = ttl
            end if
            
        ! no lower layer
        else if (ylayer<=ylow) then
            if (tts>=ttu+1.0d0) then
                fu = 1.0d0
                ttr = ttu
            else
                fu = 0.0d0
                ttr = ttu
            end if
            
        ! upper layer temperature > lower layer temperature
        else if (ttu>ttl) then
            if (tts>=ttu) then
                fu = 1.0d0
            else if (tts<=ttl) then
                fu = 0.0d0
            else
                fu = (tts-ttl)/(ttu-ttl)
            end if
            if (yslabt>ylayer) then
                ttr = ttu
            else if (yslabt<ylayer) then
                ttr = ttl
            else
                ttr = (ttu+ttl)/2.0d0
            end if
        
        ! upper layer temperature <= lower layer temperature
        else if (ttu<=ttl) then
            if (tts>ttu) then
                fu = 1.0d0
                ttr = ttu
            else if (tts.lt.ttu) then
                fu = 0.0d0
                ttr = ttl
            else
                if (yslabt>ylayer) then
                    fu = 1.0d0
                    ttr = ttu
                else if (yslabt<ylayer) then
                    fu = 0.0d0
                    ttr = ttl
                else
                    fu = 0.5d0
                    ttr = ttl
                end if
            end if
        end if
       
        fl = 1.0d0 - fu
        ff(l) = fl
        ff(u) = fu

        ! put flow into destination room
        xmterm = xmslab(n)
        qterm = qslab(n)
        do ilay = 1, 2
            uflw2(ito,m,ilay) = uflw2(ito,m,ilay) + ff(ilay) * xmterm
            uflw2(ito,q,ilay) = uflw2(ito,q,ilay) + ff(ilay) * qterm
            do iprod = 1, nprod
                uflw2(ito,2+iprod,ilay) = uflw2(ito,2+iprod,ilay) + ff(ilay) * pslab(n,iprod)
            end do
        end do

        ! take it out of the origin room
        if (yslabf>=ylay(ifrom)) then
            uflw2(ifrom,m,u) = uflw2(ifrom,m,u) - xmterm
            uflw2(ifrom,q,u) = uflw2(ifrom,q,u) - qterm
            do iprod = 1, nprod
                uflw2(ifrom,2+iprod,u) = uflw2(ifrom,2+iprod,u) - pslab(n,iprod)
            end do
        else
            uflw2(ifrom,m,l) = uflw2(ifrom,m,l) - xmterm
            uflw2(ifrom,q,l) = uflw2(ifrom,q,l) - qterm
            do iprod = 1, nprod
                uflw2(ifrom,2+iprod,l) = uflw2(ifrom,2+iprod,l) - pslab(n,iprod)
            end do
        endif
70      continue
    end do
    return
    end subroutine flogo2

    subroutine flogo1(dirs12,yslab,xmslab,nslab,ylay,qslab,pslab,mxprd,nprod,mxslab,uflw2)

    !     routine: flogo1
    !     purpose: deposition of mass, enthalpy, oxygen, and other product-of-combustion flows passing between two rooms
    !              through a vertical, constant-width vent.  this version implements the cfast rules for flow depostion. (upper
    !              layer to upper layer and lower layer to lower layer)
    !     arguments: dirs12 - a measure of the direction of the room 1 to room 2 flow in each slab
    !                yslab - slab heights in rooms 1,2 above datum elevation [m]
    !                xmslab - mass flow rate in slabs [kg/s]
    !                nslab  - number of slabs between bottom and top of vent
    !                ylay   - height of layer in each room above datum elevation [m]
    !                qslab  - enthalpy flow rate in each slab [w]
    !                pslab  - flow rate of product in each slab [(unit of product/s]
    !                mxprd  - maximum number of products currently available.
    !                nprod  - number of products
    !                mxslab - maximum number of slabs currently available.
    !                uflw2(i,1,j), i=1 or 2, j=1 or 2 (output) - mass flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [kg/s]
    !                uflw2(i,2,j), i=1 or 2, j=1 or 2 (output) - enthalpy flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [w]
    !                uflw2(i,3,j), i=1 or 2, j=1 or 2 (output) - oxygen flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [(kg oxygen)/s]
    !                uflw2(i,3+k,j), i=1 or 2, k=2 to nprod, j=1 or 2 (output) - product k flow rate to upper (j=2) or lower (j=1) layer of room i due to all slab flows of vent [(unit product k)/s]

    use flwptrs
    implicit none
    integer dirs12(*)
    integer :: i, iprod, nprod, n, nslab, ifrom, ito, ilay, mxprd, mxslab
    real(8) :: yslab(*), xmslab(*), qslab(*), ylay(*), pslab(mxslab,*), uflw2(2,mxprd+2,2), ff(2), fl, fu, xmterm, qterm

    ! initialize outputs
    do i = 1, 2
        do iprod = 1, nprod + 2
            uflw2(i,iprod,l) = 0.0d0
            uflw2(i,iprod,u) = 0.0d0
        end do
    end do

    ! put each slab flow into appropriate layer of room i to and take slab flow out of appropriate layer of room ifrom
    do n = 1, nslab

        ! determine where room flow is coming from
        if (dirs12(n)==1) then
            ifrom = 1
            ito = 2
        else if (dirs12(n)==-1) then
            ifrom = 2
            ito = 1
        else

            ! no flow in this slab so we can skip it
            go to 70
        endif

        ! lower to lower or upper to upper
        if (yslab(n)>=ylay(ifrom)) then
            fu = 1.0d0
        else
            fu = 0.0d0
        endif
        fl = 1.0d0 - fu
        ff(l) = fl
        ff(u) = fu

        ! put flow into destination room
        xmterm = xmslab(n)
        qterm = qslab(n)
        do ilay = 1, 2
            uflw2(ito,m,ilay) = uflw2(ito,m,ilay) + ff(ilay) * xmterm
            uflw2(ito,q,ilay) = uflw2(ito,q,ilay) + ff(ilay) * qterm
            do iprod = 1, nprod
                uflw2(ito,2+iprod,ilay) = uflw2(ito,2+iprod,ilay) + ff(ilay) * pslab(n,iprod)
            end do
        end do

        ! take it out of the origin room
        if (yslab(n)>=ylay(ifrom)) then
            uflw2(ifrom,m,u) = uflw2(ifrom,m,u) - xmterm
            uflw2(ifrom,q,u) = uflw2(ifrom,q,u) - qterm
            do iprod = 1, nprod
                uflw2(ifrom,2+iprod,u) = uflw2(ifrom,2+iprod,u) - pslab(n,iprod)
            end do
        else
            uflw2(ifrom,m,l) = uflw2(ifrom,m,l) - xmterm
            uflw2(ifrom,q,l) = uflw2(ifrom,q,l) - qterm
            do iprod = 1, nprod
                uflw2(ifrom,2+iprod,l) = uflw2(ifrom,2+iprod,l) - pslab(n,iprod)
            end do
        endif
70      continue
    end do
    return
    end subroutine flogo1

    subroutine delp(y,nelev,yflor,ylay,denl,denu,pflor,epsp,dp)

    !     routine: delp
    !     purpose: calculation of the absolute hydrostatic pressures at a specified elevation in each of two adjacent
    !              rooms and the pressure difference.  the basic calculation involves a determination and differencing of hydrostatic
    !              pressures above a specified datum pressure.
    !     arguments: y     - vector of heights above datum elevation where pressure difference is to be calculated [m]
    !                nelev - number of heights to be calculated
    !                yflor - height of floor in each room above datum elevation [m]
    !                ylay  - height of layer in each room above datum elevation [m]
    !                denl  - lower layer density in each room [kg/m**3]
    !                denu  - upper layer density in each room [kg/m**3]
    !                pflor - pressure at base of each room above datum pressure [kg/(m*s**2) = pascal]
    !                dp    - change in pressure between two rooms [kg/(m*s**2) = pascal]

    implicit none
    real(8) :: proom(2), y(*), denl(*), denu(*), yflor(*), ylay(*), pflor(*), dp(*), gdenl(2), gdenu(2), ygden(2)
    real(8), parameter :: g = 9.80d0, x1 = 1.0d0
    integer :: iroom, i, nelev
    real(8) :: dp1, dp2, epscut, epsp, dpold, zz

    do iroom = 1, 2
        ygden(iroom) = -(ylay(iroom)-yflor(iroom)) * denl(iroom) * g
        gdenl(iroom) = -denl(iroom) * g
        gdenu(iroom) = -denu(iroom) * g
    end do

    do i = 1, nelev
        do iroom = 1, 2
            if (yflor(iroom)<=y(i).and.y(i)<=ylay(iroom)) then

                ! the height, y, is in the lower layer
                proom(iroom) = (y(i)-yflor(iroom)) * gdenl(iroom)
            else if (y(i)>ylay(iroom)) then

                ! the height, y, is in the upper layer
                proom(iroom) = ygden(iroom) + gdenu(iroom) * (y(i) - ylay(iroom))
            else
                proom(iroom) = 0.0d0
            endif
        end do

        ! change in pressure is difference in pressures in two rooms
        dp1 = pflor(1) + proom(1)
        dp2 = pflor(2) + proom(2)

        ! test of delp fudge
        epscut = 10.0d0 * epsp * max(x1,abs(dp1),abs(dp2))
        dpold = dp1 - dp2

        ! test for underflow
        if (abs(dpold/epscut)<=130.0d0) then
            zz = 1.d0 - exp(-abs(dpold/epscut))
            dp(i) = zz * dpold
        else
            dp(i) = dpold
        endif
    end do
    return
    end subroutine delp

    !	The following functions are to implement the open/close function for vents.
    !	This is done with a really simple, linear interpolation
    !	The arrays to hold the open/close information are qcvh (4,mxvents), qcvv(4,nr), qcvm(4,mfan),
    !         and qcvi(4,mfan). 

    !	h is for horizontal flow, v for vertical flow, m for mechanical ventilation and i for filtering at mechanical vents

    !   The qcv{x} arrays are of the form
    !		(1,...) Is start of time to change
    !		(2,...) Is the initial fraction (set in HVENT, VVENT and MVENT)
    !		(3,...) Is the time to complete the change, Time+Decay_time, and
    !		(4,...) Is the final fraction

    !	The open/close function is done in the physical/mode interface, HFLOW, VFLOW and HVFAN

    real(8) function qchfraction (points, index, time)

    !	This is the open/close function for buoyancy driven horizontal flow

    real(8) :: points(4,*), time, dt, dy, dydt, mintime
    data mintime/1.0e-6/

    if (time<points(1,index)) then
        qchfraction = points(2,index)
    else if (time>points(3,index)) then
        qchfraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index),mintime)
        deltat = max(time - points(1,index),mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy / dt
        qchfraction = points(2,index) + dydt * deltat
    endif
    return
    end function qchfraction

    real(8) function qcvfraction (points, index, time)

    !	This is the open/close function for buoyancy driven vertical flow

    real(8) :: points(4,*), time, dt, dy, dydt, mintime
    data mintime/1.0e-6/

    if (time<points(1,index)) then
        qcvfraction = points(2,index)
    else if (time>points(3,index)) then
        qcvfraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index),mintime)
        deltat = max(time - points(1,index),mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy / dt
        qcvfraction = points(2,index) + dydt * deltat
    endif
    return
    end function qcvfraction

    real(8) function qcffraction (points, index, time)

    !	This is the open/close function for mechanical ventilation

    real(8) :: points(4,*), time, dt, dy, dydt, mintime
    data mintime/1.0d-6/

    if (time<points(1,index)) then
        qcffraction = points(2,index)
    else if (time>points(3,index)) then
        qcffraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index), mintime)
        deltat = max(time - points(1,index), mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy / dt
        qcffraction = points(2,index) + dydt * deltat
    endif
    return
    end function qcffraction

    real(8) function qcifraction (points, index, time)

    !	This is the open/close function for filtering

    real(8) :: points(4,*), time, dt, dy, dydt, mintime
    data mintime/1.0d-6/

    if (time<points(1,index)) then
        qcifraction = points(2,index)
    else if (time>points(3,index)) then
        qcifraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index),mintime)
        deltat = max(time - points(1,index), mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy / dt
        qcifraction = points(2,index) + dydt * deltat
    endif
    return
    end function qcifraction

    subroutine getventinfo(i,ifrom, ito, iface, vwidth, vbottom, vtop, voffset, vred, vgreen, vblue)

    !       This is a routine to get the shape data for horizontal flow vents

    use vents
    implicit none
    integer :: i,ifrom,ito,iface
    real(8) :: vwidth, voffset,vbottom,vtop,vred,vgreen,vblue

    ifrom =IZVENT(i,1)
    ito = IZVENT(i,2)
    iface = izvent(i,6)
    vwidth = zzvent(i,3)
    voffset = zzvent(i,4)
    vbottom = zzvent(i,1)
    vtop = zzvent(i,2)
    vred = 1.0d0
    vgreen = 0.0d0
    vblue = 1.0d0

    RETURN
    END

    integer function rev_flowhorizontal ()

    INTEGER :: MODULE_REV
    CHARACTER(255) :: MODULE_DATE 
    CHARACTER(255), PARAMETER :: mainrev='$Revision: 461 $'
    CHARACTER(255), PARAMETER :: maindate='$Date: 2012-06-28 16:38:31 -0400 (Thu, 28 Jun 2012) $'

    WRITE(module_date,'(A)') mainrev(INDEX(mainrev,':')+1:LEN_TRIM(mainrev)-2)
    READ (MODULE_DATE,'(I5)') MODULE_REV
    rev_flowhorizontal = module_rev
    WRITE(MODULE_DATE,'(A)') maindate
    return
    end function rev_flowhorizontal