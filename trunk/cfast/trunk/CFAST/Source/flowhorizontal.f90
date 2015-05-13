
    ! --------------------------- horizontal_flow -------------------------------------------

    subroutine horizontal_flow(tsec,epsp,nprod,uflw)

    !     routine: horizontal_flow
    !     purpose: physical interface routine to calculate flow through all unforced vertical vents (horizontal flow).
    !     it returns rates of mass and energy flows into the layers from all vents in the building.
    !     revision: $revision: 461 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: tsec  current simulation time (s)
    !                epsp  pressure error tolerance
    !                nprod
    !                uflw

    use precision_parameters
    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use vents
    use vent_slab
    use debug
    implicit none

    real(eb), intent(in) :: tsec, epsp
    real(eb), intent(out) :: uflw(nr,mxfprd+2,2)
    integer, intent(in) :: nprod

    real(eb) :: conl(mxfprd,2), conu(mxfprd,2), pmix(mxfprd)
    real(eb) :: uflw3(2,mxfprd+2,2), uflw2(2,mxfprd+2,2)
    real(eb) :: yflor(2), yceil(2), ylay(2), pflor(2)
    real(eb) :: denl(2), denu(2), tu(2), tl(2)
    real(eb) :: rslab(mxfslab), tslab(mxfslab), yslab(mxfslab),xmslab(mxfslab), qslab(mxfslab)
    real(eb) :: cslab(mxfslab,mxfprd),pslab(mxfslab,mxfprd)
    real(eb) :: uflw0(nr,ns+2,2)
    save uflw0
    logical :: ventflg(mxhvent), roomflg(nr), anyvents
    real(eb) :: factor2, qchfraction, height, width
    integer :: nirm, ifrom, ilay, islab, iprod, i, iroom, iroom1, iroom2, ik, im, ix, nslab
    real(eb) :: yvbot, yvtop, avent
    integer, parameter :: maxhead = 1 + mxhvents*(4 + mxfslab)
    real(eb) :: outarray(maxhead)
    integer :: position

    type(vent_type), pointer :: ventptr

    position = 0
    nirm = nm1

    do ifrom = 1, nirm
        do iprod = 1, nprod + 2
            uflw(ifrom,iprod,lower) = 0.0_eb
            uflw(ifrom,iprod,upper) = 0.0_eb
        end do
    end do
    if (option(fhflow)/=on) return

    call ventflag(ventflg,roomflg,anyvents)
    if(anyvents)then
        do i = 1, n_hvents
            if(ventflg(i)) then
                ventptr=>hventinfo(i)

                iroom1 = ventptr%from
                iroom2 = ventptr%to
                ik = ventptr%counter

                do ilay = 1,2
                    ventptr%mflow(1,ilay,1) = 0.0_eb
                    ventptr%mflow(2,ilay,1) = 0.0_eb
                    ventptr%mflow(1,ilay,2) = 0.0_eb
                    ventptr%mflow(2,ilay,2) = 0.0_eb
                    ventptr%mflow_mix(1,ilay) = 0.0_eb
                    ventptr%mflow_mix(2,ilay) = 0.0_eb
                end do

                do islab = 1, mxfslab
                    ventptr%temp_slab(islab) = 0.0_eb
                    ventptr%flow_slab(islab) = 0.0_eb
                    ventptr%ybot_slab(islab) = 0.0_eb
                end do

                ! setup data structures for from and to room
                call getvars(iroom1,iroom2,nprod,yflor,yceil,ylay,pflor,denl,denu,conl,conu,tl,tu)

                ! convert vent dimensions to absolute dimensions
                yvbot = ventptr%sill + yflor(1)
                yvtop = ventptr%soffit + yflor(1)
                ylay(1) = ylay(1) + yflor(1)
                ylay(2) = ylay(2) + yflor(2)

                !  use new interpolator to find vent opening fraction
                im = min(iroom1,iroom2)
                ix = max(iroom1,iroom2)
                factor2 = qchfraction (qcvh, ijk(im,ix,ik),tsec)
                height = ventptr%soffit - ventptr%sill
                width = ventptr%width*factor2
                avent = height*width

                if (avent>=1.0e-10_eb) then
                    call vent(yflor,ylay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,nprod,mxfprd,mxfslab,&
                        epsp,cslab,pslab,qslab,vss(1,i),vsa(1,i),vas(1,i),vaa(1,i),dirs12,dpv1m2,rslab,tslab,yslab,&
                        yvelev,xmslab,nslab)

                    ventptr%n_slabs = nslab
                    do islab = 1,nslab
                        ventptr%temp_slab(islab) = tslab(islab)
                        ventptr%flow_slab(islab) = xmslab(islab)*dirs12(islab)
                        ventptr%ybot_slab(islab) = yvelev(islab)
                        ventptr%ytop_slab(islab) = yvelev(islab+1)
                    end do

                    if (prnslab) call SpreadSheetfslabs(dbtime, iroom1, iroom2, ik, nslab, qslab, outarray, position)

                    call flogo(dirs12,yslab,xmslab,tslab,nslab,tu,tl,ylay,qslab,pslab,mxfprd,nprod,mxfslab,ventptr%mflow,uflw2)

                    !  calculate entrainment type mixing at the vents

                    if (option(fentrain)==on) then
                        call spill_plume(dirs12,yslab,width,xmslab,nslab,tu,tl,cp,ylay,conl,conu,pmix,mxfprd,nprod,yvbot,yvtop,&
                            uflw3,vsas(1,i),vasa(1,i))
                        do ilay = 1, 2
                            ventptr%mflow_mix(1,ilay) = uflw3(1,m,ilay)
                            ventptr%mflow_mix(2,ilay) = uflw3(2,m,ilay)
                        end do
                    else
                        do ilay = 1, 2
                            ventptr%mflow_mix(1,ilay) = 0.0_eb
                            ventptr%mflow_mix(2,ilay) = 0.0_eb
                        end do
                    end if

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
                endif
            endif
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

            ! we are computing a jacobian, so get previously saved solution for rooms that are not affected
            ! by perturbed solution variable
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
        call SSprintslab (position, outarray)
    end if
    return
    end subroutine horizontal_flow

    ! --------------------------- spill_plume -------------------------------------------

    subroutine spill_plume(dirs12,yslab,width,xmslab,nslab,tu,tl,cp,ylay,conl,conu,pmix,mxfprd,nprod,yvbot,yvtop,uflw3,vsas,vasa)

    !     routine: spill_plume
    !     purpose:
    !     arguments: dirs12 - a measure of the direction of the room 1 to room flow in each slab
    !                yslab  - slab heights in rooms 1,2 above absolute reference elevation [m]
    !                width  - slab width [m]
    !                xmslab - magnitude of the mass flow rate in slabs [kg/s]
    !                nslab  - number of slabs between bottom and top of vent
    !                tu     - upper layer temperature in each room [k]
    !                tl     - lower layer temperature in each room [k]
    !                ylay   - height of layer in each room above absolute reference elevation [m]
    !                uflw3(i,1,j), i=1 or 2, j=1 or 2 (output) - mass flow rate to upper (j=2) or
    !                         lower (j=1) layer of room i due to entrainment
    !                uflw3(i,2,j), i=1 or 2, j=1 or 2 (output) - enthalpy flow rate to upper (j=2) or
    !                   lower (j=1) layer of room i entrainment
    !                uflw3(i,2+k,j), i=1 or 2, k=1 to nprod, j=1 or 2 (output) - product k flow rate
    !                      to upper (j=2) or lower (j=1) layer of room i due entrainment

    use precision_parameters
    use flwptrs
    implicit none

    integer, intent(in) :: dirs12(10), nprod, nslab, mxfprd
    real(eb), intent(in) :: yslab(10), xmslab(10), tu(2), tl(2), cp, ylay(2), conl(mxfprd,2), conu(mxfprd,2), yvbot, yvtop, width
    real(eb), intent(out) :: uflw3(2,mxfprd+2,2), vsas(2), vasa(2), pmix(mxfprd)

    integer :: i, iprod,n , ifrom, ito
    real(eb) :: tmix, zd

    ! initialize outputs
    do i = 1, 2
        do iprod = 1, nprod + 2
            uflw3(i,iprod,l) = 0.0_eb
            uflw3(i,iprod,u) = 0.0_eb
        end do
        vsas(i) = 0.0_eb
        vasa(i) = 0.0_eb
    end do

    do n = 1, nslab

        ! eliminate cases where entrainment does not occur, i.e. a slab which is adjacent to the upper layer on
        !    both sides or a slab which is adjacent to the lower layer on both sides
        if (yslab(n)<ylay(1).or.yslab(n)<ylay(2)) then
            if (yslab(n)>=ylay(1).or.yslab(n)>=ylay(2)) then

                ! slabs with no flow cause no entrainment
                if (xmslab(n)/=0.0_eb) then

                    ! determine what room flow is coming from
                    if (dirs12(n)==1) then
                        ifrom = 1
                        ito = 2
                    else if (dirs12(n)==-1) then
                        ifrom = 2
                        ito = 1
                    else
                        cycle
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
                        if (tu(ifrom)>tl(ito).and.xmslab(n)/=0.0_eb) then
                            zd = max(0.0_eb,ylay(ito)-max(yvbot,ylay(ifrom)))
                            call poreh_plume (tu(ifrom),tl(ito),xmslab(n),zd,width,uflw3(ito,m,u))
                            uflw3(ito,m,l) = -uflw3(ito,m,u)
                            vsas(ito) = uflw3(ito,m,u)
                        endif
                    else

                        ! into lower
                        if (tl(ifrom)<tu(ito).and.xmslab(n)/=0.0_eb) then
                            ! zd = max(0.0_eb,ylay(ifrom)-max(yvbot,ylay(ito)))

                            ! need to re-work distance zd for both into upper and into upper case.
                            ! the above doesn't work for all cases
                            zd = min(yvtop,ylay(ifrom)) - max(ylay(ito),yvbot)
                            call poreh_plume (tu(ito),tl(ifrom),xmslab(n),zd,width,uflw3(ito,m,l))

                            ! the following factor (0.25 as of 10/1/93) now multiplies the lower layer entrainment
                            !    to try to approximate the reduced kelvin-helmholz type mixing.

                            uflw3(ito,m,l) = uflw3(ito,m,l)*0.25_eb
                            vasa(ito) = uflw3(ito,m,l)
                            uflw3(ito,m,u) = -uflw3(ito,m,l)
                        endif
                    endif

                    ! compute enthalpy and product flow rates of entrained flow from the mass flow rate
                    uflw3(ito,q,l) = cp*uflw3(ito,m,l)*tmix
                    uflw3(ito,q,u) = cp*uflw3(ito,m,u)*tmix
                    do iprod = 3, 2 + nprod
                        uflw3(ito,iprod,l) = uflw3(ito,m,l)*pmix(iprod-2)
                        uflw3(ito,iprod,u) = uflw3(ito,m,u)*pmix(iprod-2)
                    end do
                endif
            endif
        endif
    end do
    return
    end subroutine spill_plume

    ! --------------------------- poreh_plume -------------------------------------------

    subroutine poreh_plume(tu,tl,fmd,zz,w,fm_entrained)

    ! doorway plumes are assumed to be spill plumes from poreh, et. al., Fire Safety Journal, 30:1-19, 1998.
    ! At the moment, we do this by flow slab consistent with the original method that used mccaffrey's plume

    !     arguments: tu - upper layer temperature in the from room (input) (K)
    !                tl - lower layer temperature in the to room (input) (K)
    !                fmd - mass flow, from room --> to room (input) (kg/s)
    !                w - vent width (input) (m)
    !                zz - height over which entrainment takes place (input) (m)
    !                fm_entrained - mass entrained (output) (kg/s)

    use precision_parameters
    use cfast_main, only : cp
    implicit none

    real(eb), intent(in) :: tu, tl, fmd, zz, w
    real(eb), intent(out) :: fm_entrained

    real(eb) :: hdot, rhol

    hdot = cp*(tu-tl)*fmd
    rhol = 352.981915_eb/tl
    fm_entrained = 0.44_eb * (tl/tu)**twothirds * (grav_con*rhol**2/(cp*tl))**onethird * hdot**onethird * w**twothirds * zz
    return
    end subroutine poreh_plume

    ! --------------------------- ventflag -------------------------------------------

    subroutine ventflag(ventflg,roomflg,anyvents)

    use cenviro
    use cfast_main
    use opt
    use vents
    implicit none

    logical, intent(out) :: ventflg(mxhvent), roomflg(nr), anyvents

    integer i, ieqtyp, iroom, iroom1, iroom2
    type(vent_type), pointer :: ventptr

    ! turn all vents on
    anyvents = .true.
    do i = 1, n_hvents
        ventflg(i) = .true.
    end do

    ! if the 2nd modified jacobian option is on and a jacobian is being computed (jaccol>0) then compute
    !  vent flows only for vents that that are connected
    ! to rooms whose pressure, layer height, layer temperature,  or oxygen level is being perturbed.

    if(option(fmodjac)==on)then
        if(jaccol>0)then

            ! we are computing a jacobian
            ieqtyp = izeqmap(jaccol,1)
            iroom = izeqmap(jaccol,2)
            anyvents = .false.
            do i = 1, n_hvents
                ventflg(i) = .false.
            end do
            do i = 1, nm1
                roomflg(i) = .false.
            end do
            if(ieqtyp==eqp.or.ieqtyp==eqtu.or.ieqtyp==eqvu.or.ieqtyp==eqtl.or.ieqtyp==eqoxyl.or.ieqtyp==eqoxyu)then

                ! determine all rooms connected to perturbed rooms
                do i = 1, n_hvents
                    ventptr=>hventinfo(i)

                    iroom1 = ventptr%from
                    iroom2 = ventptr%to
                    if(iroom==iroom1.or.iroom==iroom2)then
                        roomflg(iroom1) = .true.
                        roomflg(iroom2) = .true.
                    endif
                end do
                roomflg(nm1+1) = .false.

                ! determine all vents connected to the above rooms
                do i = 1, n_hvents
                    ventptr=>hventinfo(i)

                    iroom1 = ventptr%from
                    iroom2 = ventptr%to
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

    ! --------------------------- vent -------------------------------------------

    subroutine vent(yflor,ylay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,nprod,mxfprd,mxfslab,epsp,cslab,pslab,qslab, &
        vss,vsa,vas,vaa,dirs12,dpv1m2,rslab,tslab,yslab,yvelev,xmslab,nslab)
    !     routine: vent
    !     purpose: calculation of the flow of mass, enthalpy, oxygen and other products of combustion through a vertical,
    !              constant-width vent in a wall segment common to two rooms. the subroutine uses input data describing
    !              the two-layer environment in each of the two rooms and other input data calculated in subroutine comwl1.
    !     arguments: yflor - height of floor above absolute reference elevation [m]
    !                ylay  - height of layer above absolute reference elevation [m]
    !                tu    - upper layer temperature [k]
    !                tl    - lower layer temperature [k]
    !                denl  - lower layer density [kg/m**3]
    !                denu  - upper layer density [kg/m**3]
    !                pflor - pressure at floor above absolute reference pressure [kg/(m*s**2) = pascal]
    !                yvtop - elevation of top of vent above absolute reference elevation [m]
    !                yvbot - elevation of bottom of vent above absolute reference elevation [m]
    !                avent - area of the vent [m**2]
    !                dp1m2 - pressure in room 1 - pressure in room 2 at elevations yelev [kg/(m*s**2) = pascal]
    !                cp    - specific heat [w*s/(kg*k)]
    !                conl  - concentration of each product in lower layer [unit of product/(kg layer)]
    !                conu  - concentration of each product in upper layer [unit of product/(kg layer)]
    !                nprod - number of products in current scenario
    !                mxfprd - maximum number of products currently available
    !                mxfslab- maximum number of slabs currently available
    !                epsp  - error tolerance for pressures at floor
    !                cslab (output) - concentration of other products in each slab [unit product/(kg slab)]
    !                pslab (output) - amount of other products in each slab [unit product/s]
    !                qslab (output) - enthalpy flow rate in each slab [w]
    !                dirs12 (output) - a measure of the direction of the room 1 to room 2 flow in each slab
    !                rslab (output) - density of the flow in each slab [kg/m**3]
    !                tslab (output) - absolute temperature of the flow in each slab [k]
    !                yslab (output) - elevations above the absolute reference elevation of the centroids of
    !                                 momentum of each slab [m]
    !                yvelev - elevations above the absolute reference elevations of vent boundaries, layers, and neutral planes [m]
    !                xmslab - magnitude of the mass flow rate in slabs [kg/s]
    !                nvelev - number of unique elevations delineating slabs
    !                nslab  - number of slabs between bottom and top of the vent

    use precision_parameters
    implicit none

    integer, intent(in) :: nprod, mxfprd, mxfslab
    integer, intent(out) :: nslab, dirs12(*)

    real(eb), intent(in) :: yflor(*), ylay(*), tu(*), tl(*), denl(*), denu(*), pflor(*)
    real(eb), intent(in) :: yvtop, yvbot, avent, cp, conl(mxfprd,2), conu(mxfprd,2),  epsp

    real(eb), intent(out) :: yvelev(*), dpv1m2(10)
    real(eb), intent(out) :: yslab(*), rslab(*), tslab(*), cslab(mxfslab,*), pslab(mxfslab,*), qslab(*), xmslab(*)
    real(eb), intent(out) :: vss(2), vsa(2), vas(2), vaa(2)

    integer :: nneut, nelev, i, n, jroom, iprod, nvelev

    real(eb) ::  yelev(10), dp1m2(10), yn(10)
    real(eb) :: dpp, ptest, p1, p2, p1rt, p2rt, r1, y1, y2, cvent, area, r1m8, sum, ys

    ! create initial elevation height array (ignoring neutral planes)
    call getelev(yvbot,yvtop,ylay,yelev,nelev)

    ! find pressure drops at above elevations
    call delp(yelev,nelev,yflor,ylay,denl,denu,pflor,epsp,dp1m2)

    ! find neutral planes

    nvelev = 1
    nneut = 0
    do i = 1, nelev - 1
        yvelev(nvelev) = yelev(i)
        dpv1m2(nvelev) = dp1m2(i)
        nvelev = nvelev + 1

        ! a neutral plane lies between two elevations having opposite signed pressure drops
        if (dp1m2(i)*dp1m2(i+1)<0.0_eb) then
            nneut = nneut + 1
            dpp = dp1m2(i) - dp1m2(i+1)
            yn(nneut) = (yelev(i+1)*dp1m2(i)-yelev(i)*dp1m2(i+1))/dpp

            ! fail safe in case interpolation calculation fails
            if (yn(nneut)<yelev(i).or.yn(nneut)>yelev(i+1)) then
                yn(nneut) = (yelev(i)+yelev(i+1))/2.0_eb
            endif
            yvelev(nvelev) = yn(nneut)
            dpv1m2(nvelev) = 0.0_eb
            nvelev = nvelev + 1
        endif
    end do
    yvelev(nvelev) = yelev(nelev)
    dpv1m2(nvelev) = dp1m2(nelev)
    nslab = nvelev - 1
    do i = 1, nslab
        yslab(i) = (yvelev(i)+yvelev(i+1))/2.0_eb
    end do

    ! initialize cfast data structures for flow storage
    do n = 1, nslab

        ! determine whether temperature and density properties should come from room 1 or room 2
        ptest = dpv1m2(n+1) + dpv1m2(n)
        if (ptest>0.0_eb) then
            jroom = 1
            dirs12(n) = 1
        else if (ptest<0.0_eb) then
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
        xmslab(n) = 0.0_eb
        qslab(n) = 0.0_eb
        do iprod = 1, nprod
            pslab(n,iprod) = 0.0_eb
        end do
        p1 = abs(dpv1m2(n))
        p2 = abs(dpv1m2(n+1))
        p1rt = sqrt(p1)
        p2rt = sqrt(p2)

        ! if both cross pressures are 0 then then there is no flow
        if (p1>0.0_eb.or.p2>0.0_eb) then
            r1 = max(rslab(n),0.0_eb)
            y2 = yvelev(n+1)
            y1 = yvelev(n)
            cvent = 0.70_eb

            area = avent*(y2-y1)/(yvtop-yvbot)
            r1m8 = 8.0_eb*r1
            xmslab(n) = cvent*sqrt(r1m8)*area*(p2+p1rt*p2rt+p1)/(p2rt+p1rt)/3.0_eb
            qslab(n) = cp*xmslab(n)*tslab(n)
            sum = 0.0_eb
            do iprod = 1, nprod
                pslab(n,iprod) = cslab(n,iprod)*xmslab(n)
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

    ! --------------------------- getelev -------------------------------------------

    subroutine getelev(yvbot,yvtop,ylay,yelev,nelev)
    use precision_parameters
    implicit none

    integer, intent(out) :: nelev
    real(eb), intent(in) :: ylay(*), yvbot, yvtop
    real(eb), intent(out) :: yelev(*)

    real(eb) :: ymin, ymax

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

    ! --------------------------- getvars -------------------------------------------

    subroutine getvars(from_room,to_room,nprod,yflor,yceil,ylay,pflor,denl,denu,conl,conu,tl,tu)

    !     routine: getvar
    !     purpose: routine to interface between global data structures and natural vent data structures.
    !     arguments: ivent - vent number
    !                iroom - room number
    !                yflor   height of floor above absolute reference elevation [m]
    !                yceil - height of ceiling above absolute reference elevation [m]
    !                ylay    height of layer above absolute reference elevation [m]
    !                pflor   pressure at floor relative to ambient [p]
    !                denl    density of lower layer [kg/m**3]
    !                denu    density of upper layer [kg/m**3]
    !                conl    concentration of lower layer for each product [unit of product/kg of layer]
    !                conu    concentration of upper layer for each product [unit of product/kg of layer]
    !                tl      temperature of lower layer [k]
    !                tu      temperature of upper layer [k]

    use precision_parameters
    use cenviro
    use cfast_main
    use vents
    implicit none

    integer, intent(in) :: from_room, to_room, nprod
    real(eb), intent(out) :: conl(mxfprd,2), conu(mxfprd,2)
    real(eb), intent(out) :: yflor(2), yceil(2), ylay(2), pflor(2), denl(2), denu(2), tl(2), tu(2)

    integer :: up, iprod, ip, room_index(2), iroom, i

    logical :: hallflag
    type(room_type), pointer :: roomptr

    room_index(1)=from_room
    room_index(2)=to_room

    do i = 1, 2
        hallflag = .false.
        up = upper
        iroom = room_index(i)
        roomptr=>roominfo(iroom)
        yflor(i) = roomptr%yflor
        yceil(i) = roomptr%yceil
        pflor(i) = zzrelp(iroom)
        ylay(i) = zzhlay(iroom,lower)
        denu(i) = zzrho(iroom,upper)
        denl(i) = zzrho(iroom,lower)
        do iprod = 1, nprod
            ip = izpmap(iprod+2) - 2
            conl(iprod,i) = zzcspec(iroom,lower,ip)
            conu(iprod,i) = zzcspec(iroom,upper,ip)
        end do
        tu(i) = zztemp(iroom,upper)
        tl(i) = zztemp(iroom,lower)
    end do
    return

    end subroutine getvars

    ! --------------------------- flogo -------------------------------------------

    subroutine flogo(dirs12,yslab,xmslab,tslab,nslab,tu,tl,ylay,qslab,pslab,mxfprd,nprod,mxfslab,mflows,uflw2)

    !     routine: flogo
    !     purpose: deposition of mass, enthalpy, oxygen, and other product-of-combustion flows passing between two rooms
    !              through a vertical, constant-width vent.  this version implements the ccfm rules for flow depostion.
    !              (if inflow is hot, it goes to upper layer, etc.)
    !     arguments: dirs12 - a measure of the direction of the room 1 to room 2 flow in each
    !                  slab, 1 = 1--> 2, -1 = 2 --> 1, 0 = no flow
    !                yslab - slab heights in rooms 1,2 above absolute reference elevation [m]
    !                xmslab - mass flow rate in slabs [kg/s]
    !                tslab  - temperature of slabs [K]
    !                nslab  - number of slabs between bottom and top of vent
    !                tu,tl  - upper and lower layer temperatures in rooms 1,2
    !                ylay   - height of layer in each room above absolute reference elevation [m]
    !                qslab  - enthalpy flow rate in each slab [w]
    !                pslab  - flow rate of product in each slab [(unit of product/s]
    !                mxfprd  - maximum number of products currently available.
    !                nprod  - number of products
    !                mxfslab - maximum number of slabs currently available.
    !                mflows(i,j), i=1 or 2, j=1 or 2 (output) - mass flows through vent with source and destination
    !                             identified (from upper (i=2) or lower (i=1) layer, to upper (j=2) or lower (j=1) layer)
    !                uflw2(i,1,j), i=1 or 2, j=1 or 2 (output) - mass flow rate to upper (j=2) or lower (j=1) layer
    !                             of room i due to all slab flows of vent [kg/s]
    !                uflw2(i,2,j), i=1 or 2, j=1 or 2 (output) - enthalpy flow rate to upper (j=2) or lower (j=1)
    !                             layer of room i due to all slab flows of vent [w]
    !                uflw2(i,3,j), i=1 or 2, j=1 or 2 (output) - oxygen flow rate to upper (j=2) or lower (j=1) layer
    !                             of room i due to all slab flows of vent [(kg oxygen)/s]
    !                uflw2(i,3+k,j), i=1 or 2, k=2 to nprod, j=1 or 2 (output) - product k flow rate to upper (j=2)
    !                             or lower (j=1) layer of room i due to all slab flows of vent [(unit product k)/s]

    use precision_parameters
    use flwptrs
    implicit none

    integer, intent(in) :: dirs12(*)
    integer, intent(in) :: nprod, nslab, mxfprd, mxfslab
    real(eb), intent(in) :: yslab(*), xmslab(*), tslab(*), qslab(*), ylay(*), pslab(mxfslab,*), tu(*), tl(*)
    real(eb), intent(out) :: mflows(2,2,2), uflw2(2,mxfprd+2,2)

    integer :: i, iprod, n, ifrom, ito, ilay
    real(eb) :: flow_fraction(2), flower, fupper, xmterm, qterm, temp_upper, temp_lower, temp_slab
    real(eb), parameter :: deltatemp_min = 0.01_eb

    ! initialize outputs
    mflows = 0.0_eb
    do i = 1, 2
        do iprod = 1, nprod + 2
            uflw2(i,iprod,l) = 0.0_eb
            uflw2(i,iprod,u) = 0.0_eb
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
            cycle
        endif

        ! put slab flow into "to" flow according to slab temperature
        temp_slab = tslab(n)
        temp_upper = tu(ito)
        temp_lower = tl(ito)

        if (temp_slab>=temp_upper+deltatemp_min) then
            ! if it's relatively hot, it goes to the upper layer
            fupper = 1.0_eb
        elseif (temp_slab<=temp_lower-deltatemp_min) then
            ! if it's really cold, it goes to the lower layer
            fupper = 0.0_eb
        else
            ! if the layers are of distinctly different temperatures and the temperature of the incoming flow is in
            ! between then mix the flow
            fupper = (temp_slab - (temp_lower-deltatemp_min))/(temp_upper-temp_lower+2.0_eb*deltatemp_min)
        endif

        flower = 1.0_eb - fupper
        flow_fraction(l) = flower
        flow_fraction(u) = fupper

        xmterm = xmslab(n)
        qterm = qslab(n)

        ! take it out of the origin room
        if (yslab(n)>=ylay(ifrom)) then
            mflows(ifrom,u,2) = mflows(ifrom,u,2) + xmterm
            uflw2(ifrom,m,u) = uflw2(ifrom,m,u) - xmterm
            uflw2(ifrom,q,u) = uflw2(ifrom,q,u) - qterm
            do iprod = 1, nprod
                uflw2(ifrom,2+iprod,u) = uflw2(ifrom,2+iprod,u) - pslab(n,iprod)
            end do
        else
            mflows(ifrom,l,2) = mflows(ifrom,l,2) + xmterm
            uflw2(ifrom,m,l) = uflw2(ifrom,m,l) - xmterm
            uflw2(ifrom,q,l) = uflw2(ifrom,q,l) - qterm
            do iprod = 1, nprod
                uflw2(ifrom,2+iprod,l) = uflw2(ifrom,2+iprod,l) - pslab(n,iprod)
            end do
        endif

        ! put flow into destination room
        do ilay = 1, 2
            mflows(ito,ilay,1) = mflows(ito,ilay,1) + flow_fraction(ilay)*xmterm
            uflw2(ito,m,ilay) = uflw2(ito,m,ilay) + flow_fraction(ilay)*xmterm
            uflw2(ito,q,ilay) = uflw2(ito,q,ilay) + flow_fraction(ilay)*qterm
            do iprod = 1, nprod
                uflw2(ito,2+iprod,ilay) = uflw2(ito,2+iprod,ilay) + flow_fraction(ilay)*pslab(n,iprod)
            end do
        end do

    end do
    return
    end subroutine flogo

    ! --------------------------- delp -------------------------------------------

    subroutine delp(y,nelev,yflor,ylay,denl,denu,pflor,epsp,dp)

    !     routine: delp
    !     purpose: calculation of the absolute hydrostatic pressures at a specified elevation in each of two adjacent
    !              rooms and the pressure difference.  the basic calculation involves a determination and differencing of
    !              hydrostatic pressures above a specified absolute reference pressure.
    !     arguments: y     - vector of heights above absolute reference elevation where pressure difference is to be calculated [m]
    !                nelev - number of heights to be calculated
    !                yflor - height of floor in each room above absolute reference elevation [m]
    !                ylay  - height of layer in each room above absolute reference elevation [m]
    !                denl  - lower layer density in each room [kg/m**3]
    !                denu  - upper layer density in each room [kg/m**3]
    !                pflor - pressure at base of each room above absolute reference pressure [kg/(m*s**2) = pascal]
    !                dp    - change in pressure between two rooms [kg/(m*s**2) = pascal]

    use precision_parameters
    implicit none

    integer, intent(in) :: nelev
    real(eb), intent(in) :: y(*),  yflor(*), ylay(*), denl(*), denu(*), pflor(*), epsp
    real(eb), intent(out) :: dp(*)

    real(eb) :: proom(2), gdenl(2), gdenu(2), ygden(2)
    integer :: iroom, i
    real(eb) :: dp1, dp2, epscut, dpold, zz

    do iroom = 1, 2
        ygden(iroom) = -(ylay(iroom)-yflor(iroom))*denl(iroom)*grav_con
        gdenl(iroom) = -denl(iroom)*grav_con
        gdenu(iroom) = -denu(iroom)*grav_con
    end do

    do i = 1, nelev
        do iroom = 1, 2
            if (yflor(iroom)<=y(i).and.y(i)<=ylay(iroom)) then

                ! the height, y, is in the lower layer
                proom(iroom) = (y(i)-yflor(iroom))*gdenl(iroom)
            else if (y(i)>ylay(iroom)) then

                ! the height, y, is in the upper layer
                proom(iroom) = ygden(iroom) + gdenu(iroom)*(y(i) - ylay(iroom))
            else
                proom(iroom) = 0.0_eb
            endif
        end do

        ! change in pressure is difference in pressures in two rooms
        dp1 = pflor(1) + proom(1)
        dp2 = pflor(2) + proom(2)

        ! test of delp fudge
        epscut = 10.0_eb*epsp*max(1.0_eb,abs(dp1),abs(dp2))
        dpold = dp1 - dp2

        ! test for underflow
        if (abs(dpold/epscut)<=130.0_eb) then
            zz = 1.0_eb - exp(-abs(dpold/epscut))
            dp(i) = zz*dpold
        else
            dp(i) = dpold
        endif
    end do
    return
    end subroutine delp

    !	The following functions implement the open/close function for vents.
    !	This is done with a simple, linear interpolation
    !	The arrays to hold the open/close information are qcvh (4,mxhvents), qcvv(4,nr), qcvm(4,mxfan),
    !         and qcvi(4,mxfan).

    !	h is for horizontal flow, v for vertical flow, m for mechanical ventilation and i for filtering at mechanical vents

    !   The qcv{x} arrays are of the form
    !		(1,...) Is start of time to change
    !		(2,...) Is the initial fraction (set in HVENT, VVENT and MVENT)
    !		(3,...) Is the time to complete the change, Time+Decay_time, and
    !		(4,...) Is the final fraction

    !	The open/close function is done in the physical/mode interface, horizontal_flow, vertical_flow and HVFAN


    ! --------------------------- qchfraction -------------------------------------------

    real(eb) function qchfraction (points, index, time)

    !	This is the open/close function for buoyancy driven horizontal flow

    use precision_parameters
    implicit none

    integer, intent(in) :: index
    real(eb), intent(in) :: points(4,*), time

    real(eb) :: dt, dy, dydt, mintime
    real(eb) :: deltat
    data mintime/1.0e-6/

    if (time<points(1,index)) then
        qchfraction = points(2,index)
    else if (time>points(3,index)) then
        qchfraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index),mintime)
        deltat = max(time - points(1,index),mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy/dt
        qchfraction = points(2,index) + dydt*deltat
    endif
    return
    end function qchfraction

    ! --------------------------- qcvfraction -------------------------------------------

    real(eb) function qcvfraction (points, index, time)

    !	This is the open/close function for buoyancy driven vertical flow

    use precision_parameters
    implicit none

    integer, intent(in) :: index
    real(eb), intent(in) :: points(4,*), time

    real(eb) :: dt, dy, dydt, mintime
    real(eb) :: deltat
    data mintime/1.0e-6/

    if (time<points(1,index)) then
        qcvfraction = points(2,index)
    else if (time>points(3,index)) then
        qcvfraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index),mintime)
        deltat = max(time - points(1,index),mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy/dt
        qcvfraction = points(2,index) + dydt*deltat
    endif
    return
    end function qcvfraction

    ! --------------------------- qcffraction -------------------------------------------

    real(eb) function qcffraction (points, index, time)

    !	This is the open/close function for mechanical ventilation

    use precision_parameters
    implicit none

    integer, intent(in) :: index
    real(eb), intent(in) :: points(4,*), time

    real(eb) :: dt, dy, dydt, mintime
    real(eb) :: deltat
    data mintime/1.0e-6_eb/

    if (time<points(1,index)) then
        qcffraction = points(2,index)
    else if (time>points(3,index)) then
        qcffraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index), mintime)
        deltat = max(time - points(1,index), mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy/dt
        qcffraction = points(2,index) + dydt*deltat
    endif
    return
    end function qcffraction

    ! --------------------------- qcifraction -------------------------------------------

    real(eb) function qcifraction (points, index, time)

    !	This is the open/close function for filtering

    use precision_parameters
    implicit none

    integer, intent(in) :: index
    real(eb), intent(in) :: points(4,*), time

    real(eb) :: dt, dy, dydt, mintime
    real(eb) :: deltat
    data mintime/1.0e-6_eb/

    if (time<points(1,index)) then
        qcifraction = points(2,index)
    else if (time>points(3,index)) then
        qcifraction = points(4,index)
    else
        dt = max(points(3,index) - points(1,index),mintime)
        deltat = max(time - points(1,index), mintime)
        dy = points(4,index) - points(2,index)
        dydt = dy/dt
        qcifraction = points(2,index) + dydt*deltat
    endif
    return
    end function qcifraction

    ! --------------------------- getventinfo -------------------------------------------

    subroutine getventinfo(i,ifrom, ito, iface, vwidth, vbottom, vtop, voffset, vred, vgreen, vblue)

    !       This is a routine to get the shape data for horizontal flow vents

    use precision_parameters
    use vents
    implicit none

    integer, intent(in) :: i
    integer, intent(out) :: ifrom,ito,iface
    real(eb), intent(out) :: vwidth, voffset,vbottom,vtop,vred,vgreen,vblue
    type(vent_type), pointer :: ventptr

    ventptr=>hventinfo(i)

    ifrom =ventptr%from
    ito = ventptr%to
    iface = ventptr%face
    vwidth = ventptr%width
    voffset = ventptr%from_hall_offset
    vbottom = ventptr%sill
    vtop = ventptr%soffit
    vred = 1.0_eb
    vgreen = 0.0_eb
    vblue = 1.0_eb

    RETURN
    END

