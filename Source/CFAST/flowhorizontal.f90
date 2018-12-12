module hflow_routines

    use precision_parameters

    use opening_fractions, only: get_vent_opening
    use debug_routines, only: ssprintslab, spreadsheetfslabs
    use room_data
    use utility_routines, only: tanhsmooth
    
    use precision_parameters
    use cenviro
    use ramp_data
    use cparams
    use option_data
    use vent_data
    use diag_data
    use solver_data, only: i_wallmap, i_speciesmap

    implicit none

    private

    public horizontal_flow

    contains

    ! --------------------------- horizontal_flow -------------------------------------------

    subroutine horizontal_flow(tsec,epsp,uflw_hf)

    !     physical interface routine to calculate flow through all unforced vertical vents (horizontal flow).
    !     it returns rates of mass and energy flows into the layers from all vents in the building.

    !     inputs:   tsec    current simulation time (s)
    !               epsp    pressure error tolerance
    !     output:   uflw_hf change in mass and energy for each layer of each compartment via flow through horizontal vents

    real(eb), intent(in) :: tsec, epsp
    real(eb), intent(out) :: uflw_hf(mxrooms,ns+2,2)

    real(eb) :: conl(ns,2), conu(ns,2), pmix(ns)
    real(eb) :: uflw3(2,ns+2,2), uflw2(2,ns+2,2)
    real(eb) :: zflor(2), zceil(2), zlay(2), pflor(2)
    real(eb) :: denl(2), denu(2), tu(2), tl(2)
    real(eb) :: rslab(mxfslab), tslab(mxfslab), yslab(mxfslab),xmslab(mxfslab), qslab(mxfslab)
    real(eb) :: cslab(mxfslab,ns),pslab(mxfslab,ns)
    real(eb) :: fraction, height, width
    integer :: islab, i, iroom1, iroom2, ik, im, ix, nslab
    real(eb) :: yvbot, yvtop, avent
    integer, parameter :: maxhead = 1 + mxhvents*(4 + mxfslab)
    real(eb) :: outarray(maxhead)
    integer :: position
    character(64) :: rampid

    type(vent_type), pointer :: ventptr

    position = 0

    uflw_hf(1:nrm1,1:ns+2,l) = 0.0_eb
    uflw_hf(1:nrm1,1:ns+2,u) = 0.0_eb
    
    vss(1:2,1:mxhvents) = 0.0_eb
    vsa(1:2,1:mxhvents) = 0.0_eb
    vas(1:2,1:mxhvents) = 0.0_eb
    vaa(1:2,1:mxhvents) = 0.0_eb
    vsas(1:2,1:mxhvents) = 0.0_eb
    vasa(1:2,1:mxhvents) = 0.0_eb
    
    if (option(fhflow)/=on) return
    if (n_hvents==0) return

    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        iroom1 = ventptr%room1
        iroom2 = ventptr%room2
        ik = ventptr%counter

        ventptr%h_mflow(1,1:2,1) = 0.0_eb
        ventptr%h_mflow(2,1:2,1) = 0.0_eb
        ventptr%h_mflow(1,1:2,2) = 0.0_eb
        ventptr%h_mflow(2,1:2,2) = 0.0_eb
        ventptr%h_mflow_mix(1,1:2) = 0.0_eb
        ventptr%h_mflow_mix(2,1:2) = 0.0_eb

        ventptr%temp_slab(1:mxfslab) = 0.0_eb
        ventptr%flow_slab(1:mxfslab) = 0.0_eb
        ventptr%ybot_slab(1:mxfslab) = 0.0_eb
        ventptr%ytop_slab(1:mxfslab) = 0.0_eb

        ! setup data structures for from and to room
        call getvars(iroom1,iroom2,zflor,zceil,zlay,pflor,denl,denu,conl,conu,tl,tu)

        ! convert vent dimensions to absolute dimensions
        yvbot = ventptr%sill + zflor(1)
        yvtop = ventptr%soffit + zflor(1)
        zlay(1) = zlay(1) + zflor(1)
        zlay(2) = zlay(2) + zflor(2)

        !  use new interpolator to find vent opening fraction
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        rampid = ventptr%ramp_id
        call get_vent_opening (rampid,'H',im,ix,ik,i,tsec,fraction)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width*fraction
        avent = height*width

        if (avent>=1.0e-10_eb) then
            call ventw (zflor,zlay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,mxfslab,&
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

            call flogo(dirs12,yslab,xmslab,tslab,nslab,tu,tl,zlay,qslab,pslab,mxfslab,ventptr%h_mflow,uflw2)

            !  calculate entrainment type mixing at the vents

            if (option(fentrain)==on) then
                call spill_plume(dirs12,yslab,width,xmslab,nslab,tu,tl,cp,zlay,conl,conu,pmix,yvbot,yvtop,&
                    uflw3,vsas(1,i),vasa(1,i))
                ventptr%h_mflow_mix(1,1:2) = uflw3(1,m,1:2)
                ventptr%h_mflow_mix(2,1:2) = uflw3(2,m,1:2)
            else
                ventptr%h_mflow_mix(1,1:2) = 0.0_eb
                ventptr%h_mflow_mix(2,1:2) = 0.0_eb
            end if

            ! sum flows from both rooms for each layer and type of product
            ! (but only if the room is an inside room)

            if (iroom1>=1.and.iroom1<=nrm1) then
                uflw_hf(iroom1,1:ns+2,l) = uflw_hf(iroom1,1:ns+2,l) + uflw2(1,1:ns+2,l)
                uflw_hf(iroom1,1:ns+2,u) = uflw_hf(iroom1,1:ns+2,u) + uflw2(1,1:ns+2,u)
                if (option(fentrain)==on) then
                    uflw_hf(iroom1,1:ns+2,l) = uflw_hf(iroom1,1:ns+2,l) + uflw3(1,1:ns+2,l)
                    uflw_hf(iroom1,1:ns+2,u) = uflw_hf(iroom1,1:ns+2,u) + uflw3(1,1:ns+2,u)
                end if
            end if
            if (iroom2>=1.and.iroom2<=nrm1) then
                uflw_hf(iroom2,1:ns+2,l) = uflw_hf(iroom2,1:ns+2,l) + uflw2(2,1:ns+2,l)
                uflw_hf(iroom2,1:ns+2,u) = uflw_hf(iroom2,1:ns+2,u) + uflw2(2,1:ns+2,u)
                if (option(fentrain)==on) then
                    uflw_hf(iroom2,1:ns+2,l) = uflw_hf(iroom2,1:ns+2,l) + uflw3(2,1:ns+2,l)
                    uflw_hf(iroom2,1:ns+2,u) = uflw_hf(iroom2,1:ns+2,u) + uflw3(2,1:ns+2,u)
                end if
            end if
        end if

    end do

    if (prnslab) then
        call SSprintslab (position, outarray)
    end if
    return
    end subroutine horizontal_flow

    ! --------------------------- spill_plume -------------------------------------------

    subroutine spill_plume (dirs12,yslab,width,xmslab,nslab,tu,tl,cp,zlay,conl,conu,pmix,yvbot,yvtop,uflw3,vsas,vasa)

    ! calculate plume entrainment for a door mixing
    ! inputs:   dirs12          direction of the room 1 to room flow in each slab
    !           yslab           slab heights in rooms 1,2 above absolute reference elevation [m]
    !           width           slab width [m]
    !           xmslab          magnitude of the mass flow rate in slabs [kg/s]
    !           nslab           number of slabs between bottom and top of vent
    !           tu              upper layer temperature in each room [k]
    !           tl              lower layer temperature in each room [k]
    !           cp              specific heat of air
    !           zlay            height of layer in each room above absolute reference elevation [m]
    !           conl            species concentrations in lower layer of from room
    !           conu            species concentrations in upper layer of from room
    !           yvbot           absolute height of vent bottom
    !           yvtop           absolute height of vent top
    ! outputs   pmix            species concentrations in plume
    !           uflw3(i,1:3,j)  mass, enthalpy, and species flows to upper (j=2) or lower (j=1) layer of room i due to entrainment
    !           vsas            mixing flow mass from upper layer due to entrainment
    !           vasa            mixing flow mass from lower layer due to entrainment

    integer, intent(in) :: dirs12(10), nslab
    real(eb), intent(in) :: yslab(10), xmslab(10), tu(2), tl(2), cp, zlay(2), conl(ns,2), conu(ns,2), yvbot, yvtop, width
    real(eb), intent(out) :: uflw3(2,ns+2,2), vsas(2), vasa(2), pmix(ns)

    integer :: iprod, nr , ifrom, ito
    real(eb) :: tmix, zd

    ! initialize outputs
    uflw3(1:2,1:ns+2,l) = 0.0_eb
    uflw3(1:2,1:ns+2,u) = 0.0_eb
    vsas(1:2) = 0.0_eb
    vasa(1:2) = 0.0_eb

    do nr = 1, nslab

        ! eliminate cases where entrainment does not occur, i.e. a slab which is adjacent to the upper layer on
        !    both sides or a slab which is adjacent to the lower layer on both sides
        if (yslab(nr)<zlay(1).or.yslab(nr)<zlay(2)) then
            if (yslab(nr)>=zlay(1).or.yslab(nr)>=zlay(2)) then

                ! slabs with no flow cause no entrainment
                if (xmslab(nr)/=0.0_eb) then

                    ! determine what room flow is coming from
                    if (dirs12(nr)==1) then
                        ifrom = 1
                        ito = 2
                    else if (dirs12(nr)==-1) then
                        ifrom = 2
                        ito = 1
                    else
                        cycle
                    end if

                    ! determine temperature and product concentrations of entrained flow
                    if (yslab(nr)<zlay(ito)) then
                        tmix = tl(ito)
                        do iprod = 1, ns
                            pmix(iprod) = conl(iprod,ito)
                        end do
                    else
                        tmix = tu(ito)
                        do iprod = 1, ns
                            pmix(iprod) = conu(iprod,ito)
                        end do
                    end if

                    ! compute the size of the entrained mass flow
                    if (yslab(nr)>=zlay(ifrom)) then

                        ! into upper
                        if (tu(ifrom)>tl(ito).and.xmslab(nr)/=0.0_eb) then
                            zd = max(0.0_eb,zlay(ito)-max(yvbot,zlay(ifrom)))
                            call poreh_plume (tu(ifrom),tl(ito),xmslab(nr),zd,width,uflw3(ito,m,u))
                            uflw3(ito,m,l) = -uflw3(ito,m,u)
                            vsas(ito) = uflw3(ito,m,u)
                        end if
                    else

                        ! into lower
                        if (tl(ifrom)<tu(ito).and.xmslab(nr)/=0.0_eb) then
                            ! zd = max(0.0_eb,zlay(ifrom)-max(yvbot,zlay(ito)))

                            ! need to re-work distance zd for both into upper and into upper case.
                            ! the above doesn't work for all cases
                            zd = min(yvtop,zlay(ifrom)) - max(zlay(ito),yvbot)
                            call poreh_plume (tu(ito),tl(ifrom),xmslab(nr),zd,width,uflw3(ito,m,l))

                            ! the following factor (0.25 as of 10/1/93) now multiplies the lower layer entrainment
                            !    to try to approximate the reduced kelvin-helmholz type mixing.

                            uflw3(ito,m,l) = uflw3(ito,m,l)*0.25_eb
                            vasa(ito) = uflw3(ito,m,l)
                            uflw3(ito,m,u) = -uflw3(ito,m,l)
                        end if
                    end if

                    ! compute enthalpy and product flow rates of entrained flow from the mass flow rate
                    uflw3(ito,q,l) = cp*uflw3(ito,m,l)*tmix
                    uflw3(ito,q,u) = cp*uflw3(ito,m,u)*tmix
                    do iprod = 3, 2 + ns
                        uflw3(ito,iprod,l) = uflw3(ito,m,l)*pmix(iprod-2)
                        uflw3(ito,iprod,u) = uflw3(ito,m,u)*pmix(iprod-2)
                    end do
                end if
            end if
        end if
    end do
    return
    end subroutine spill_plume

    ! --------------------------- poreh_plume -------------------------------------------

    subroutine poreh_plume (tu, tl, mdot, z, w, entrainment_rate)

    ! doorway plumes are assumed to be spill plumes from poreh, et. al., Fire Safety Journal, 30:1-19, 1998.
    ! At the moment, we do this by flow slab consistent with the original method that used mccaffrey's plume

    ! inputs:   tu                  upper layer temperature in the from room (K)
    !           tl                  lower layer temperature in the to room (K)
    !           mdot                mass flow, from room --> to room (kg/s)
    !           w                   vent width (m)
    !           z                   height over which entrainment takes place (m)
    ! output:   entrainment_rate    mass entrained (output) (kg/s)

    real(eb), intent(in) :: tu, tl, mdot, z, w
    real(eb), intent(out) :: entrainment_rate

    real(eb) :: hdot, rhol

    hdot = cp*(tu-tl)*mdot
    rhol = 352.981915_eb/tl
    entrainment_rate = 0.44_eb * (tl/tu)**twothirds * (grav_con*rhol**2/(cp*tl))**onethird * hdot**onethird * w**twothirds * z
    return
    end subroutine poreh_plume

    ! --------------------------- vent -------------------------------------------

    subroutine ventw(zflor,zlay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,mxfslab,epsp,cslab,pslab,qslab, &
        vss,vsa,vas,vaa,dirs12,dpv1m2,rslab,tslab,yslab,yvelev,xmslab,nslab)
    
    ! calculation of the flow of mass, enthalpy, oxygen and other products of combustion through a vertical,
    ! constant-width vent in a wall segment common to two rooms. the subroutine uses input data describing
    ! the two-layer environment in each of the two rooms and other input data.
    
    ! inputs:   zflor   height of floor above absolute reference elevation [m]
    !           zlay    height of layer above absolute reference elevation [m]
    !           tu      upper layer temperature [k]
    !           tl      lower layer temperature [k]
    !           denl    lower layer density [kg/m**3]
    !           denu    upper layer density [kg/m**3]
    !           pflor   pressure at floor above absolute reference pressure [kg/(m*s**2) = pascal]
    !           yvtop   elevation of top of vent above absolute reference elevation [m]
    !           yvbot   elevation of bottom of vent above absolute reference elevation [m]
    !           avent   area of the vent [m**2]
    !           dp1m2   pressure in room 1 - pressure in room 2 at elevations yelev [kg/(m*s**2) = pascal]
    !           cp      specific heat [w*s/(kg*k)]
    !           conl    concentration of each product in lower layer [unit of product/(kg layer)]
    !           conu    concentration of each product in upper layer [unit of product/(kg layer)]
    !           mxfslab maximum number of slabs currently available
    !           epsp    error tolerance for pressures at floor
    ! outputs:  dirs12  direction of the room 1 to room 2 flow in each slab
    !           nslab   number of slabs between bottom and top of the vent
    !           cslab   concentration of other products in each slab [unit product/(kg slab)]
    !           pslab   amount of other products in each slab [unit product/s]
    !           qslab   enthalpy flow rate in each slab [w]
    !           rslab   density of the flow in each slab [kg/m**3]
    !           tslab   absolute temperature of the flow in each slab [k]
    !           yslab   elevations above the absolute reference elevation of the centroids of momentum of each slab [m]
    !           yvelev  elevations above the absolute reference elevations of vent boundaries, layers, and neutral planes [m]
    !           xmslab  magnitude of the mass flow rate in slabs [kg/s]
    !           n_velev number of unique elevations delineating slabs

    integer, intent(in) :: mxfslab
    integer, intent(out) :: nslab, dirs12(*)

    real(eb), intent(in) :: zflor(*), zlay(*), tu(*), tl(*), denl(*), denu(*), pflor(*)
    real(eb), intent(in) :: yvtop, yvbot, avent, cp, conl(ns,2), conu(ns,2),  epsp

    real(eb), intent(out) :: yvelev(*), dpv1m2(10)
    real(eb), intent(out) :: yslab(*), rslab(*), tslab(*), cslab(mxfslab,*), pslab(mxfslab,*), qslab(*), xmslab(*)
    real(eb), intent(out) :: vss(2), vsa(2), vas(2), vaa(2)

    integer :: nneut, nelev, i, nr, jroom, iprod

    real(eb) ::  yelev(10), dp1m2(10), yn(10)
    real(eb) :: dpp, ptest, p1, p2, p1rt, p2rt, r1, y1, y2, cvent, area, r1m8, sum, ys

    ! create initial elevation height array (ignoring neutral planes)
    call getelev(yvbot,yvtop,zlay,yelev,nelev)

    ! find pressure drops at above elevations
    call delp(yelev,nelev,zflor,zlay,denl,denu,pflor,epsp,dp1m2)

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
            end if
            yvelev(nvelev) = yn(nneut)
            dpv1m2(nvelev) = 0.0_eb
            nvelev = nvelev + 1
        end if
    end do
    yvelev(nvelev) = yelev(nelev)
    dpv1m2(nvelev) = dp1m2(nelev)
    nslab = nvelev - 1
    do i = 1, nslab
        yslab(i) = (yvelev(i)+yvelev(i+1))/2.0_eb
    end do

    ! initialize cfast data structures for flow storage
    do nr = 1, nslab

        ! determine whether temperature and density properties should come from room 1 or room 2
        ptest = dpv1m2(nr+1) + dpv1m2(nr)
        if (ptest>0.0_eb) then
            jroom = 1
            dirs12(nr) = 1
        else if (ptest<0.0_eb) then
            dirs12(nr) = -1
            jroom = 2
        else
            dirs12(nr) = 0
            jroom = 1
        end if

        ! determine whether temperature and density properties should come from upper or lower layer
        if (yslab(nr)<=zlay(jroom)) then
            tslab(nr) = tl(jroom)
            rslab(nr) = denl(jroom)
            do iprod = 1, ns
                cslab(nr,iprod) = conl(iprod,jroom)
            end do
        else
            tslab(nr) = tu(jroom)
            rslab(nr) = denu(jroom)
            do iprod = 1, ns
                cslab(nr,iprod) = conu(iprod,jroom)
            end do
        end if

        ! for nonzero-flow slabs determine xmslab(nr) and yslab(nr)
        xmslab(nr) = 0.0_eb
        qslab(nr) = 0.0_eb
        do iprod = 1, ns
            pslab(nr,iprod) = 0.0_eb
        end do
        p1 = abs(dpv1m2(nr))
        p2 = abs(dpv1m2(nr+1))
        p1rt = sqrt(p1)
        p2rt = sqrt(p2)

        ! if both cross pressures are 0 then then there is no flow
        if (p1>0.0_eb.or.p2>0.0_eb) then
            r1 = max(rslab(nr),0.0_eb)
            y2 = yvelev(nr+1)
            y1 = yvelev(nr)
            cvent = 0.70_eb

            area = avent*(y2-y1)/(yvtop-yvbot)
            r1m8 = 8.0_eb*r1
            xmslab(nr) = cvent*sqrt(r1m8)*area*(p2+p1rt*p2rt+p1)/(p2rt+p1rt)/3.0_eb
            qslab(nr) = cp*xmslab(nr)*tslab(nr)
            sum = 0.0_eb
            do iprod = 1, ns
                pslab(nr,iprod) = cslab(nr,iprod)*xmslab(nr)
                sum = sum + pslab(nr,iprod)
            end do
        end if

        ! construct cfast data structures ss, sa, as, aa
        ys = yslab(nr)
        if (ys>max(zlay(1),zlay(2))) then
            if (dirs12(nr)>0) then
                vss(1) = xmslab(nr)
            else
                vss(2) = xmslab(nr)
            end if
        else if (ys<min(zlay(1),zlay(2))) then
            if (dirs12(nr)>0) then
                vaa(1) = xmslab(nr)
            else
                vaa(2) = xmslab(nr)
            end if
        else if (ys>zlay(1)) then
            if (dirs12(nr)>0) then
                vsa(1) = xmslab(nr)
            else
                vas(2) = xmslab(nr)
            end if
        else if (ys>zlay(2)) then
            if (dirs12(nr)>0) then
                vas(1) = xmslab(nr)
            else
                vsa(2) = xmslab(nr)
            end if
        end if
    end do
    return
    end subroutine ventw

    ! --------------------------- getelev -------------------------------------------

    subroutine getelev (yvbot,yvtop,zlay,yelev,nelev)
    
    ! determines elevation of flow slabs in horizontal vent flow
    
    ! inputs:   yvbot   absolute elevation of bottom of the vent
    !           yvtop   absolute elevation of top of the vent
    !           zlay    absolute elevation of the layers in rooms connected to the vent
    ! outputs:  yelev   absolute elevations of flow slabs in the vent
    !           nelev   number of flow slabs in the vent

    integer, intent(out) :: nelev
    real(eb), intent(in) :: zlay(*), yvbot, yvtop
    real(eb), intent(out) :: yelev(*)

    real(eb) :: ymin, ymax

    ymin = min(zlay(1),zlay(2))
    ymax = max(zlay(1),zlay(2))
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
        end if
    end if
    return
    end subroutine getelev

    ! --------------------------- getvars -------------------------------------------

    subroutine getvars(from_room,to_room,zflor,zceil,zlay,pflor,denl,denu,conl,conu,tl,tu)

    ! routine to interface between global data structures and natural vent data structures.
    ! gets room and vent data for calculation of horizontal flow through a wall vent
    
    ! inputs:   ivent - vent number
    !           from_room   first room number connected to vent
    !           to_room     second room number connected to vent
    ! outputs:  zflor       height of floor above absolute reference elevation [m]
    !           zceil       height of ceiling above absolute reference elevation [m]
    !           zlay        height of layer above absolute reference elevation [m]
    !           pflor       pressure at floor relative to ambient [p]
    !           denl        density of lower layer [kg/m**3]
    !           denu        density of upper layer [kg/m**3]
    !           conl        concentration of lower layer for each product [unit of product/kg of layer]
    !           conu        concentration of upper layer for each product [unit of product/kg of layer]
    !           tl          temperature of lower layer [k]
    !           tu          temperature of upper layer [k]

    integer, intent(in) :: from_room, to_room
    real(eb), intent(out) :: conl(ns,2), conu(ns,2)
    real(eb), intent(out) :: zflor(2), zceil(2), zlay(2), pflor(2), denl(2), denu(2), tl(2), tu(2)

    integer :: iprod, ip, room_index(2), iroom, i

    type(room_type), pointer :: roomptr

    room_index(1) = from_room
    room_index(2) = to_room

    do i = 1, 2
        iroom = room_index(i)
        roomptr=>roominfo(iroom)
        zflor(i) = roomptr%z0
        zceil(i) = roomptr%z1
        pflor(i) = roomptr%relp
        zlay(i) = roomptr%depth(l)
        tu(i) = roomptr%temp(u)
        tl(i) = roomptr%temp(l)
        denu(i) = roomptr%rho(u)
        denl(i) = roomptr%rho(l)
        do iprod = 1, ns
            ip = i_speciesmap(iprod+2) - 2
            conl(iprod,i) = roomptr%species_fraction(l,ip)
            conu(iprod,i) = roomptr%species_fraction(u,ip)
        end do
    end do
    return

    end subroutine getvars

    ! --------------------------- flogo -------------------------------------------

    subroutine flogo(dirs12,yslab,xmslab,tslab,nslab,tu,tl,zlay,qslab,pslab,mxfslab,mflows,uflw2)


    ! deposition of mass, enthalpy, oxygen, and other product-of-combustion flows passing between two rooms
    ! through a vertical, constant-width vent.  
    
    !inputs:    dirs12          a measure of the direction of the room 1 to room 2 flow in each slab, 
    !                           = 1--> 2, -1 = 2 --> 1, 0 = no flow
    !           yslab           slab heights in rooms 1,2 above absolute reference elevation [m]
    !           xmslab          mass flow rate in slabs [kg/s]
    !           tslab           temperature of slabs [K]
    !           nslab           number of slabs between bottom and top of vent
    !           tu,tl           upper and lower layer temperatures in rooms 1,2
    !           zlay            height of layer in each room above absolute reference elevation [m]
    !           qslab           enthalpy flow rate in each slab [w]
    !           pslab           flow rate of product in each slab [(unit of product)/s]
    !           mxfslab         maximum number of slabs currently available.
    ! outputs:  mflows(i,j)     i=1 or 2, j=1 or 2 (output) - mass flows through vent with source and destination
    !                           identified (from upper (i=2) or lower (i=1) layer, to upper (j=2) or lower (j=1) layer)
    !           uflw2(i,1,j)    i=1 or 2, j=1 or 2 (output) - mass flow rate to upper (j=2) or lower (j=1) layer
    !                           of room i due to all slab flows of vent [kg/s]
    !           uflw2(i,2,j)    i=1 or 2, j=1 or 2 (output) - enthalpy flow rate to upper (j=2) or lower (j=1)
    !                           layer of room i due to all slab flows of vent [w]
    !           uflw2(i,3,j)    i=1 or 2, j=1 or 2 (output) - oxygen flow rate to upper (j=2) or lower (j=1) layer
    !                           of room i due to all slab flows of vent [(kg oxygen)/s]
    !           uflw2(i,3+k,j)  i=1 or 2, k=2 to ns, j=1 or 2 (output) - product k flow rate to upper (j=2)
    !                           or lower (j=1) layer of room i due to all slab flows of vent [(unit product k)/s]

    integer, intent(in) :: dirs12(*)
    integer, intent(in) :: nslab, mxfslab
    real(eb), intent(in) :: yslab(*), xmslab(*), tslab(*), qslab(*), zlay(*), pslab(mxfslab,*), tu(*), tl(*)
    real(eb), intent(out) :: mflows(2,2,2), uflw2(2,ns+2,2)

    integer :: iprod, nr, ifrom, ito, ilay
    real(eb) :: flow_fraction(2), flower, fupper, xmterm, qterm, temp_upper, temp_lower, temp_slab


    ! initialize outputs
    mflows = 0.0_eb
    uflw2(1:2,1:ns+2,l) = 0.0_eb
    uflw2(1:2,1:ns+2,u) = 0.0_eb

    ! put each slab flow into appropriate layer of room i to and take slab flow out of appropriate layer of room ifrom
    do nr = 1, nslab

        ! determine where room flow is coming from
        if (dirs12(nr)==1) then
            ifrom = 1
            ito = 2
        else if (dirs12(nr)==-1) then
            ifrom = 2
            ito = 1
        else
            ! no flow in this slab so we can skip it
            cycle
        end if

        ! put slab flow into "to" flow according to slab temperature
        temp_slab = tslab(nr)
        temp_upper = tu(ito)
        temp_lower = tl(ito)
        if (temp_lower > temp_upper) temp_lower = temp_upper

        ! transition smoothly from all upper to all lower over the range Tu+dT to Tl-dt
        fupper = tanhsmooth (temp_slab, temp_upper+deltatemp_min, temp_lower-deltatemp_min, 1._eb, 0._eb)
        flower = 1.0_eb - fupper
        flow_fraction(l) = flower
        flow_fraction(u) = fupper

        xmterm = xmslab(nr)
        qterm = qslab(nr)

        ! take it out of the origin room
        if (yslab(nr)>=zlay(ifrom)) then
            mflows(ifrom,u,2) = mflows(ifrom,u,2) + xmterm
            uflw2(ifrom,m,u) = uflw2(ifrom,m,u) - xmterm
            uflw2(ifrom,q,u) = uflw2(ifrom,q,u) - qterm
            do iprod = 1, ns
                uflw2(ifrom,2+iprod,u) = uflw2(ifrom,2+iprod,u) - pslab(nr,iprod)
            end do
        else
            mflows(ifrom,l,2) = mflows(ifrom,l,2) + xmterm
            uflw2(ifrom,m,l) = uflw2(ifrom,m,l) - xmterm
            uflw2(ifrom,q,l) = uflw2(ifrom,q,l) - qterm
            do iprod = 1, ns
                uflw2(ifrom,2+iprod,l) = uflw2(ifrom,2+iprod,l) - pslab(nr,iprod)
            end do
        end if

        ! put flow into destination room
        do ilay = 1, 2
            mflows(ito,ilay,1) = mflows(ito,ilay,1) + flow_fraction(ilay)*xmterm
            uflw2(ito,m,ilay) = uflw2(ito,m,ilay) + flow_fraction(ilay)*xmterm
            uflw2(ito,q,ilay) = uflw2(ito,q,ilay) + flow_fraction(ilay)*qterm
            do iprod = 1, ns
                uflw2(ito,2+iprod,ilay) = uflw2(ito,2+iprod,ilay) + flow_fraction(ilay)*pslab(nr,iprod)
            end do
        end do

    end do
    return
    end subroutine flogo

    ! --------------------------- delp -------------------------------------------

    subroutine delp(y,nelev,zflor,zlay,denl,denu,pflor,epsp,dp)

    ! calculate the absolute hydrostatic pressures at a specified elevation in each of two adjacent
    ! rooms and the pressure difference.  the basic calculation involves a determination and differencing of
    ! hydrostatic pressures above a specified absolute reference pressure.
    
    ! inputs:   y       vector of heights above absolute reference elevation where pressure difference is to be calculated [m]
    !           nelev   number of heights to be calculated
    !           zflor   height of floor in each room above absolute reference elevation [m]
    !           zlay    height of layer in each room above absolute reference elevation [m]
    !           denl    lower layer density in each room [kg/m**3]
    !           denu    upper layer density in each room [kg/m**3]
    !           pflor   pressure at base of each room above absolute reference pressure [kg/(m*s**2) = pascal]
    ! outputs:  dp      change in pressure between two rooms [kg/(m*s**2) = pascal]

    integer, intent(in) :: nelev
    real(eb), intent(in) :: y(*),  zflor(*), zlay(*), denl(*), denu(*), pflor(*), epsp
    real(eb), intent(out) :: dp(*)

    real(eb) :: proom(2), gdenl(2), gdenu(2), ygden(2)
    integer :: iroom, i
    real(eb) :: dp1, dp2, epscut, dpold, zz

    ygden(1:2) = -(zlay(1:2)-zflor(1:2))*denl(1:2)*grav_con
    gdenl(1:2) = -denl(1:2)*grav_con
    gdenu(1:2) = -denu(1:2)*grav_con

    do i = 1, nelev
        do iroom = 1, 2
            if (zflor(iroom)<=y(i).and.y(i)<=zlay(iroom)) then

                ! the height, y, is in the lower layer
                proom(iroom) = (y(i)-zflor(iroom))*gdenl(iroom)
            else if (y(i)>zlay(iroom)) then

                ! the height, y, is in the upper layer
                proom(iroom) = ygden(iroom) + gdenu(iroom)*(y(i) - zlay(iroom))
            else
                proom(iroom) = 0.0_eb
            end if
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
        end if
    end do
    return
    end subroutine delp
    
end module hflow_routines
