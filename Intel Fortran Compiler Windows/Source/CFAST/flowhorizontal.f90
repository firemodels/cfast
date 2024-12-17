module hflow_routines

    use precision_parameters

    use opening_fractions, only: get_vent_opening
    use debug_routines, only: ssprintslab, spreadsheetfslabs
    use utility_routines, only: tanhsmooth
    
    use cfast_types, only: room_type, vent_type
    
    use cenviro, only: cp
    use cparams, only: l, u, m, q, mxrooms, mxhvents, mxfslab, deltatemp_min
    use diag_data, only: dbtime, prnslab
    use option_data, only: fhflow, fentrain, option, on
    use room_data, only: n_rooms, ns, roominfo
    use spreadsheet_output_data, only: outarray
    use solver_data, only: i_wallmap, i_speciesmap
    use vent_data, only: n_hvents, hventinfo, n_leaks, leakinfo, nvelev, dirs12, dpv1m2, yvelev, vss, vsa, vas, vaa, vsas, vasa

    implicit none

    private

    public wall_flow, leakage_flow

    contains

    ! --------------------------- wall_flow -------------------------------------------

!> \brief   physical interface routine to calculate flow through all unforced vertical vents (horizontal flow).
!>          it returns rates of mass and energy flows into the layers from all vents in the building.

!> \param   tsec (input): current simulation time (s)
!> \param   epsp (input): pressure error tolerance
!> \param   uflw_hf (output): change in mass and energy for each layer of each compartment via flow through horizontal vents

    subroutine wall_flow (tsec,epsp,uflw_hf)

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
    integer :: position

    type(vent_type), pointer :: ventptr

    position = 0

    uflw_hf(1:n_rooms,1:ns+2,l) = 0.0_eb
    uflw_hf(1:n_rooms,1:ns+2,u) = 0.0_eb
    
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
        call get_wall_flow_variables(iroom1,iroom2,zflor,zceil,zlay,pflor,denl,denu,conl,conu,tl,tu)

        ! convert vent dimensions to absolute dimensions
        yvbot = ventptr%sill + zflor(1)
        yvtop = ventptr%soffit + zflor(1)
        zlay(1) = zlay(1) + zflor(1)
        zlay(2) = zlay(2) + zflor(2)

        !  use new interpolator to find vent opening fraction
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        call get_vent_opening (ventptr,tsec,fraction)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width*fraction
        avent = height*width
        ventptr%opening_fraction = fraction
        ventptr%current_area = avent

        if (avent>=1.0e-10_eb) then
            call ventw (zflor,zlay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,mxfslab,&
                epsp,cslab,pslab,qslab,vss(1,i),vsa(1,i),vas(1,i),vaa(1,i),dirs12,dpv1m2,rslab,tslab,yslab,&
                yvelev,xmslab,nslab,ventptr%cvent)

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

            if (iroom1>=1.and.iroom1<=n_rooms) then
                uflw_hf(iroom1,1:ns+2,l) = uflw_hf(iroom1,1:ns+2,l) + uflw2(1,1:ns+2,l)
                uflw_hf(iroom1,1:ns+2,u) = uflw_hf(iroom1,1:ns+2,u) + uflw2(1,1:ns+2,u)
                if (option(fentrain)==on) then
                    uflw_hf(iroom1,1:ns+2,l) = uflw_hf(iroom1,1:ns+2,l) + uflw3(1,1:ns+2,l)
                    uflw_hf(iroom1,1:ns+2,u) = uflw_hf(iroom1,1:ns+2,u) + uflw3(1,1:ns+2,u)
                end if
            end if
            if (iroom2>=1.and.iroom2<=n_rooms) then
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
    end subroutine wall_flow

    ! --------------------------- leakage_flow -------------------------------------------

    subroutine leakage_flow(epsp,uflw_lk)

!> \brief   calculate leakage flow between compartments with specified leakage to the outside
!>          leakage is just calculated as a wall vent at the floor or along a wall corner

!> \param   epsp (input): pressure error tolerance
!> \param   uflw_lk (output): change in mass and energy for each layer of each compartment via leakage

    real(eb), intent(in) :: epsp
    real(eb), intent(out) :: uflw_lk(mxrooms,ns+2,2)

    real(eb) :: conl(ns,2), conu(ns,2)
    real(eb) :: uflw2(2,ns+2,2)
    real(eb) :: zflor(2), zceil(2), zlay(2), pflor(2)
    real(eb) :: denl(2), denu(2), tu(2), tl(2)
    real(eb) :: rslab(mxfslab), tslab(mxfslab), yslab(mxfslab),xmslab(mxfslab), qslab(mxfslab)
    real(eb) :: cslab(mxfslab,ns),pslab(mxfslab,ns)
    real(eb) :: height, width
    integer :: islab, i, iroom1, iroom2, nslab
    real(eb) :: yvbot, yvtop, avent
    integer :: position

    type(vent_type), pointer :: ventptr

    position = 0

    uflw_lk(1:n_rooms,1:ns+2,l) = 0.0_eb
    uflw_lk(1:n_rooms,1:ns+2,u) = 0.0_eb

    
    if (option(fhflow)/=on) return
    if (n_leaks==0) return

    do i = 1, n_leaks
        ventptr=>leakinfo(i)

        iroom1 = ventptr%room1
        iroom2 = ventptr%room2

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
        call get_wall_flow_variables(iroom1,iroom2,zflor,zceil,zlay,pflor,denl,denu,conl,conu,tl,tu)

        ! convert vent dimensions to absolute dimensions
        yvbot = ventptr%sill + zflor(1)
        yvtop = ventptr%soffit + zflor(1)
        zlay(1) = zlay(1) + zflor(1)
        zlay(2) = zlay(2) + zflor(2)

        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = height*width
        ventptr%opening_fraction = 1._eb
        ventptr%current_area = avent

        if (avent>=1.0e-10_eb) then
            call ventw (zflor,zlay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,mxfslab,&
                epsp,cslab,pslab,qslab,vss(1,i),vsa(1,i),vas(1,i),vaa(1,i),dirs12,dpv1m2,rslab,tslab,yslab,&
                yvelev,xmslab,nslab,ventptr%cvent)

            ventptr%n_slabs = nslab
            do islab = 1,nslab
                ventptr%temp_slab(islab) = tslab(islab)
                ventptr%flow_slab(islab) = xmslab(islab)*dirs12(islab)
                ventptr%ybot_slab(islab) = yvelev(islab)
                ventptr%ytop_slab(islab) = yvelev(islab+1)
            end do

            call flogo(dirs12,yslab,xmslab,tslab,nslab,tu,tl,zlay,qslab,pslab,mxfslab,ventptr%h_mflow,uflw2)

            ventptr%h_mflow_mix(1,1:2) = 0.0_eb
            ventptr%h_mflow_mix(2,1:2) = 0.0_eb

            ! sum flows from both rooms for each layer and type of product
            ! (but only if the room is an inside room)

            if (iroom1>=1.and.iroom1<=n_rooms) then
                uflw_lk(iroom1,1:ns+2,l) = uflw_lk(iroom1,1:ns+2,l) + uflw2(1,1:ns+2,l)
                uflw_lk(iroom1,1:ns+2,u) = uflw_lk(iroom1,1:ns+2,u) + uflw2(1,1:ns+2,u)
            end if
            if (iroom2>=1.and.iroom2<=n_rooms) then
                uflw_lk(iroom2,1:ns+2,l) = uflw_lk(iroom2,1:ns+2,l) + uflw2(2,1:ns+2,l)
                uflw_lk(iroom2,1:ns+2,u) = uflw_lk(iroom2,1:ns+2,u) + uflw2(2,1:ns+2,u)
            end if
        end if

    end do

    return
    end subroutine leakage_flow

    ! --------------------------- spill_plume -------------------------------------------

!> \brief   calculate plume entrainment for a door mixing
!> \param   dirs12 (input): direction of the room 1 to room flow in each slab
!> \param   yslab (input): slab heights in rooms 1,2 above absolute reference elevation [m]
!> \param   width (input): slab width [m]
!> \param   xmslab (input): magnitude of the mass flow rate in slabs [kg/s]
!> \param   nslab (input): number of slabs between bottom and top of vent
!> \param   tu (input): upper layer temperature in each room [k]
!> \param   tl (input): lower layer temperature in each room [k]
!> \param   cp (input): specific heat of air
!> \param   zlay (input): height of layer in each room above absolute reference elevation [m]
!> \param   conl (input): species concentrations in lower layer of from room
!> \param   conu (input): species concentrations in upper layer of from room
!> \param   yvbot (input): absolute height of vent bottom
!> \param   yvtop (input): absolute height of vent top
    
!> \param   pmix (output): species concentrations in plume
!> \param   uflw3(i,1:3,j) (output): mass, enthalpy, and species flows to upper (j=2) or lower (j=1) layer of room i
!> \param   vsas (output): mixing flow mass from upper layer due to entrainment
!> \param   vasa (output): mixing flow mass from lower layer due to entrainment

    subroutine spill_plume (dirs12,yslab,width,xmslab,nslab,tu,tl,cp,zlay,conl,conu,pmix,yvbot,yvtop,uflw3,vsas,vasa)

    integer, intent(in) :: dirs12(10), nslab
    real(eb), intent(in) :: yslab(10), xmslab(10), tu(2), tl(2), cp, zlay(2), conl(ns,2), conu(ns,2), yvbot, yvtop, width
    real(eb), intent(out) :: uflw3(2,ns+2,2), vsas(2), vasa(2), pmix(ns)

    integer :: iprod, i , ifrom, ito
    real(eb) :: tmix, zd

    ! initialize outputs
    uflw3(1:2,1:ns+2,l) = 0.0_eb
    uflw3(1:2,1:ns+2,u) = 0.0_eb
    vsas(1:2) = 0.0_eb
    vasa(1:2) = 0.0_eb

    do i = 1, nslab

        ! eliminate cases where entrainment does not occur, i.e. a slab which is adjacent to the upper layer on
        !    both sides or a slab which is adjacent to the lower layer on both sides
        if (yslab(i)<zlay(1).or.yslab(i)<zlay(2)) then
            if (yslab(i)>=zlay(1).or.yslab(i)>=zlay(2)) then

                ! slabs with no flow cause no entrainment
                if (xmslab(i)/=0.0_eb) then

                    ! determine what room flow is coming from
                    if (dirs12(i)==1) then
                        ifrom = 1
                        ito = 2
                    else if (dirs12(i)==-1) then
                        ifrom = 2
                        ito = 1
                    else
                        cycle
                    end if

                    ! determine temperature and product concentrations of entrained flow
                    if (yslab(i)<zlay(ito)) then
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
                    if (yslab(i)>=zlay(ifrom)) then

                        ! into upper
                        if (tu(ifrom)>tl(ito).and.xmslab(i)/=0.0_eb) then
                            zd = max(0.0_eb,zlay(ito)-max(yvbot,zlay(ifrom)))
                            call poreh_plume (tu(ifrom),tl(ito),xmslab(i),zd,width,uflw3(ito,m,u))
                            uflw3(ito,m,l) = -uflw3(ito,m,u)
                            vsas(ito) = uflw3(ito,m,u)
                        end if
                    else

                        ! into lower
                        if (tl(ifrom)<tu(ito).and.xmslab(i)/=0.0_eb) then
                            ! zd = max(0.0_eb,zlay(ifrom)-max(yvbot,zlay(ito)))

                            ! need to re-work distance zd for both into upper and into upper case.
                            ! the above doesn't work for all cases
                            zd = min(yvtop,zlay(ifrom)) - max(zlay(ito),yvbot)
                            call poreh_plume (tu(ito),tl(ifrom),xmslab(i),zd,width,uflw3(ito,m,l))

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

!> \brief   vent jet plumes are assumed to be spill plumes from poreh, et. al., Fire Safety Journal, 30:1-19, 1998.
!>          at the moment, we do this by flow slab consistent with the original method that used mccaffrey's plume

!> \param   tu (input): upper layer temperature in the from room (K)
!> \param   tl (input): lower layer temperature in the to room (K)
!> \param   mdot (input): mass flow, from room --> to room (kg/s)
!> \param   w (input): vent width (m)
!> \param   z (input): height over which entrainment takes place (m)
!> \param   entrainment_rate (output): mass entrained (output) (kg/s)

    subroutine poreh_plume (tu, tl, mdot, z, w, entrainment_rate)

    real(eb), intent(in) :: tu, tl, mdot, z, w
    real(eb), intent(out) :: entrainment_rate

    real(eb) :: hdot, rhol

    hdot = cp*(tu-tl)*mdot
    rhol = 352.981915_eb/tl
    entrainment_rate = 0.44_eb * (tl/tu)**twothirds * (grav_con*rhol**2/(cp*tl))**onethird * hdot**onethird * w**twothirds * z
    return
    end subroutine poreh_plume

    ! --------------------------- ventw -------------------------------------------
    
!> \brief   calculation of the flow of mass, enthalpy, oxygen and other products of combustion through a vertical,
!>          constant-width vent in a wall segment common to two rooms. 
    
!> \param   zflor (input): height of floor above absolute reference elevation (m)
!> \param   zlay (input): height of layer above absolute reference elevation (m)
!> \param   tu (input): upper layer temperature (K)
!> \param   tl (input): lower layer temperature (K)
!> \param   denl (input): lower layer density (kg/m**3)
!> \param   denu (input): upper layer density (kg/m**3)
!> \param   pflor (input): pressure at floor above absolute reference pressure (kg/(m*s**2) = Pa)
!> \param   yvtop (input): elevation of top of vent above absolute reference elevation (m)
!> \param   yvbot (input): elevation of bottom of vent above absolute reference elevation (m)
!> \param   avent (input): area of the vent (m**2)
!> \param   dp1m2 (input): pressure in room 1 - pressure in room 2 at elevations yelev (kg/(m*s**2) = Pa)
!> \param   cp (input): specific heat (w*s/(kg*k))
!> \param   conl (input): concentration of each product in lower layer (unit of product/(kg layer))
!> \param   conu (input): concentration of each product in upper layer (unit of product/(kg layer))
!> \param   mxfslab (input): maximum number of slabs
!> \param   epsp (input): error tolerance for pressures at floor
    
!> \param   dirs12 (output): direction of the room 1 to room 2 flow in each slab
!> \param   nslab (output): number of slabs between bottom and top of the vent
!> \param   cslab (output): concentration of other products in each slab (unit product/(kg slab))
!> \param   pslab (output): amount of other products in each slab (unit product/s)
!> \param   qslab (output): enthalpy flow rate in each slab (w)
!> \param   rslab (output): density of the flow in each slab (kg/m**3)
!> \param   tslab (output): absolute temperature of the flow in each slab (K)
!> \param   yslab (output): elevations above the absolute reference elevation of the centroids of momentum of each slab (m)
!> \param   yvelev (output): elevations above the absolute reference elevations of vent boundaries, layers, and neutral planes (m)
!> \param   xmslab (output): magnitude of the mass flow rate in slabs (kg/s)
!> \param   n_velev (output): number of unique elevations delineating slabs

    subroutine ventw (zflor,zlay,tu,tl,denl,denu,pflor,yvtop,yvbot,avent,cp,conl,conu,mxfslab,epsp,cslab,pslab,qslab, &
        vss,vsa,vas,vaa,dirs12,dpv1m2,rslab,tslab,yslab,yvelev,xmslab,nslab,cvent)

    integer, intent(in) :: mxfslab
    integer, intent(out) :: nslab, dirs12(*)

    real(eb), intent(in) :: zflor(*), zlay(*), tu(*), tl(*), denl(*), denu(*), pflor(*)
    real(eb), intent(in) :: yvtop, yvbot, avent, cp, conl(ns,2), conu(ns,2),  epsp, cvent

    real(eb), intent(out) :: yvelev(*), dpv1m2(10)
    real(eb), intent(out) :: yslab(*), rslab(*), tslab(*), cslab(mxfslab,*), pslab(mxfslab,*), qslab(*), xmslab(*)
    real(eb), intent(out) :: vss(2), vsa(2), vas(2), vaa(2)

    integer :: nneut, nelev, i, jroom, iprod

    real(eb) ::  yelev(10), dp1m2(10), yn(10)
    real(eb) :: dpp, ptest, p1, p2, p1rt, p2rt, r1, y1, y2, area, r1m8, sum, ys

    ! create initial elevation height array (ignoring neutral planes)
    call get_slab_elevations(yvbot,yvtop,zlay,yelev,nelev)

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
    do i = 1, nslab

        ! determine whether temperature and density properties should come from room 1 or room 2
        ptest = dpv1m2(i+1) + dpv1m2(i)
        if (ptest>0.0_eb) then
            jroom = 1
            dirs12(i) = 1
        else if (ptest<0.0_eb) then
            dirs12(i) = -1
            jroom = 2
        else
            dirs12(i) = 0
            jroom = 1
        end if

        ! determine whether temperature and density properties should come from upper or lower layer
        if (yslab(i)<=zlay(jroom)) then
            tslab(i) = tl(jroom)
            rslab(i) = denl(jroom)
            do iprod = 1, ns
                cslab(i,iprod) = conl(iprod,jroom)
            end do
        else
            tslab(i) = tu(jroom)
            rslab(i) = denu(jroom)
            do iprod = 1, ns
                cslab(i,iprod) = conu(iprod,jroom)
            end do
        end if

        ! for nonzero-flow slabs determine xmslab(i) and yslab(i)
        xmslab(i) = 0.0_eb
        qslab(i) = 0.0_eb
        do iprod = 1, ns
            pslab(i,iprod) = 0.0_eb
        end do
        p1 = abs(dpv1m2(i))
        p2 = abs(dpv1m2(i+1))
        p1rt = sqrt(p1)
        p2rt = sqrt(p2)

        ! if both cross pressures are 0 then then there is no flow
        if (p1>0.0_eb.or.p2>0.0_eb) then
            r1 = max(rslab(i),0.0_eb)
            y2 = yvelev(i+1)
            y1 = yvelev(i)
            ! This is where CFAST originally hardcoded the vent flow coefficient 
            !cvent = 0.70_eb

            area = avent*(y2-y1)/(yvtop-yvbot)
            r1m8 = 8.0_eb*r1
            xmslab(i) = cvent*sqrt(r1m8)*area*(p2+p1rt*p2rt+p1)/(p2rt+p1rt)/3.0_eb
            qslab(i) = cp*xmslab(i)*tslab(i)
            sum = 0.0_eb
            do iprod = 1, ns
                pslab(i,iprod) = cslab(i,iprod)*xmslab(i)
                sum = sum + pslab(i,iprod)
            end do
        end if

        ! construct cfast data structures ss, sa, as, aa
        ys = yslab(i)
        if (ys>max(zlay(1),zlay(2))) then
            if (dirs12(i)>0) then
                vss(1) = xmslab(i)
            else
                vss(2) = xmslab(i)
            end if
        else if (ys<min(zlay(1),zlay(2))) then
            if (dirs12(i)>0) then
                vaa(1) = xmslab(i)
            else
                vaa(2) = xmslab(i)
            end if
        else if (ys>zlay(1)) then
            if (dirs12(i)>0) then
                vsa(1) = xmslab(i)
            else
                vas(2) = xmslab(i)
            end if
        else if (ys>zlay(2)) then
            if (dirs12(i)>0) then
                vas(1) = xmslab(i)
            else
                vsa(2) = xmslab(i)
            end if
        end if
    end do
    return
        end subroutine ventw

    ! --------------------------- get_slab_elevations -------------------------------------------
    
!> \brief   determines elevation of flow slabs in wall vent flow
    
!> \param   yvbot (input): absolute elevation of bottom of the vent
!> \param   yvtop (input): absolute elevation of top of the vent
!> \param   zlay (input): absolute elevation of the layers in rooms connected to the vent
!> \param   yelev (output): absolute elevations of flow slabs in the vent
!> \param   nelev (output): number of flow slabs in the vent

    subroutine get_slab_elevations (yvbot,yvtop,zlay,yelev,nelev)

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
    end subroutine get_slab_elevations

    ! --------------------------- get_wall_flow_variables -------------------------------------------

!> \brief   routine to interface between global data structures and natural vent data structures.
!>          gets room and vent data for calculation of horizontal flow through a wall vent
    
!> \param   ivent (input): vent number
!> \param   from_room (input): first room number connected to vent
!> \param   to_room (input): second room number connected to vent
    
!> \param   zflor (output): height of floor above absolute reference elevation (m)
!> \param   zceil (output): height of ceiling above absolute reference elevation (m)
!> \param   zlay (output): height of layer above absolute reference elevation (m)
!> \param   pflor (output): pressure at floor relative to ambient (p)
!> \param   denl (output): density of lower layer (kg/m**3)
!> \param   denu (output): density of upper layer (kg/m**3)
!> \param   conl (output): concentration of lower layer for each product (unit of product/kg of layer)
!> \param   conu (output): concentration of upper layer for each product (unit of product/kg of layer)
!> \param   tl (output): temperature of lower layer (K)
!> \param   tu (output): temperature of upper layer (K)

    subroutine get_wall_flow_variables(from_room,to_room,zflor,zceil,zlay,pflor,denl,denu,conl,conu,tl,tu)

    integer, intent(in) :: from_room, to_room
    real(eb), intent(out) :: conl(ns,2), conu(ns,2)
    real(eb), intent(out) :: zflor(2), zceil(2), zlay(2), pflor(2), denl(2), denu(2), tl(2), tu(2)

    integer :: iprod, ip, room_index(2), iroom, i

    type(room_type), pointer :: roomptr, otherptr
    
    room_index(1) = from_room
    room_index(2) = to_room

    do i = 1, 2
        iroom = room_index(i)
        roomptr=>roominfo(iroom)
        zceil(i) = roomptr%z1
        zlay(i) = roomptr%depth(l)
        tu(i) = roomptr%temp(u)
        tl(i) = roomptr%temp(l)
        if (iroom .eq. n_rooms+1) then
            if (i .eq. 1) then
                otherptr=>roominfo(room_index(2))
            else
                otherptr=>roominfo(room_index(1))
            end if
            zflor(i) = otherptr%z0
            pflor(i) = otherptr%exterior_relp_initial
            denu(i) = otherptr%exterior_den_initial
            denl(i) = otherptr%exterior_den_initial
        else
            zflor(i) = roomptr%z0
            pflor(i) = roomptr%relp
            denu(i) = roomptr%rho(u)
            denl(i) = roomptr%rho(l)
        end if
        do iprod = 1, ns
            ip = i_speciesmap(iprod+2) - 2
            conl(iprod,i) = roomptr%species_fraction(l,ip)
            conu(iprod,i) = roomptr%species_fraction(u,ip)
        end do
    end do
    return

    end subroutine get_wall_flow_variables

    ! --------------------------- flogo -------------------------------------------

!> \brief   calculate distribution of mass, enthalpy, oxygen, and other product-of-combustion flows passing between 
!>          layers in two rooms through a vertical, constant-width vent.  
    
!> \param   dirs12 (input): a measure of the direction of the room 1 to room 2 flow in each slab, 
!>                          = 1--> 2, -1 = 2 --> 1, 0 = no flow
!> \param   yslab (input): slab heights in rooms 1,2 above absolute reference elevation (m)
!> \param   xmslab (input): mass flow rate in slabs (kg/s)
!> \param   tslab (input): temperature of slabs (K)
!> \param   nslab (input): number of slabs between bottom and top of vent
!> \param   tu,tl (input): upper and lower layer temperatures in rooms 1,2
!> \param   zlay (input): height of layer in each room above absolute reference elevation (m)
!> \param   qslab (input): enthalpy flow rate in each slab (w)
!> \param   pslab (input): flow rate of product in each slab ((unit of product)/s)
!> \param   mxfslab (input): maximum number of slabs currently available.
    
!> \param   mflows(i,j) (output): i=1 or 2, j=1 or 2 (output) - mass flows through vent with source and destination
!>                                  identified (from upper (i=2) or lower (i=1) layer, to upper (j=2) or lower (j=1) layer)
!> \param   uflw2(i,1,j) (output): i=1 or 2, j=1 or 2 (output) - mass flow rate to upper (j=2) or lower (j=1) layer
!>                                 of room i due to all slab flows of vent (kg/s)
!> \param   uflw2(i,2,j) (output): i=1 or 2, j=1 or 2 (output) - enthalpy flow rate to upper (j=2) or lower (j=1)
!>                                 layer of room i due to all slab flows of vent (w)
!> \param   uflw2(i,3,j) (output): i=1 or 2, j=1 or 2 (output) - oxygen flow rate to upper (j=2) or lower (j=1) layer
!>                                 of room i due to all slab flows of vent ((kg oxygen)/s)
!> \param   uflw2(i,3+k,j) (output): i=1 or 2, k=2 to ns, j=1 or 2 (output) - product k flow rate to upper (j=2)
!>                                   or lower (j=1) layer of room i due to all slab flows of vent ((unit product k)/s)

    subroutine flogo (dirs12,yslab,xmslab,tslab,nslab,tu,tl,zlay,qslab,pslab,mxfslab,mflows,uflw2)

    integer, intent(in) :: dirs12(*)
    integer, intent(in) :: nslab, mxfslab
    real(eb), intent(in) :: yslab(*), xmslab(*), tslab(*), qslab(*), zlay(*), pslab(mxfslab,*), tu(*), tl(*)
    real(eb), intent(out) :: mflows(2,2,2), uflw2(2,ns+2,2)

    integer :: iprod, i, ifrom, ito, ilay
    real(eb) :: flow_fraction(2), flower, fupper, xmterm, qterm, temp_upper, temp_lower, temp_slab


    ! initialize outputs
    mflows = 0.0_eb
    uflw2(1:2,1:ns+2,l) = 0.0_eb
    uflw2(1:2,1:ns+2,u) = 0.0_eb

    ! put each slab flow into appropriate layer of room i to and take slab flow out of appropriate layer of room ifrom
    do i = 1, nslab

        ! determine where room flow is coming from
        if (dirs12(i)==1) then
            ifrom = 1
            ito = 2
        else if (dirs12(i)==-1) then
            ifrom = 2
            ito = 1
        else
            ! no flow in this slab so we can skip it
            cycle
        end if

        ! put slab flow into "to" flow according to slab temperature
        temp_slab = tslab(i)
        temp_upper = tu(ito)
        temp_lower = tl(ito)
        if (temp_lower > temp_upper) temp_lower = temp_upper

        ! transition smoothly from all upper to all lower over the range Tu+dT to Tl-dt
        fupper = tanhsmooth (temp_slab, temp_upper+deltatemp_min, temp_lower-deltatemp_min, 1._eb, 0._eb)
        flower = 1.0_eb - fupper
        flow_fraction(l) = flower
        flow_fraction(u) = fupper

        xmterm = xmslab(i)
        qterm = qslab(i)

        ! take it out of the origin room
        if (yslab(i)>=zlay(ifrom)) then
            mflows(ifrom,u,2) = mflows(ifrom,u,2) + xmterm
            uflw2(ifrom,m,u) = uflw2(ifrom,m,u) - xmterm
            uflw2(ifrom,q,u) = uflw2(ifrom,q,u) - qterm
            do iprod = 1, ns
                uflw2(ifrom,2+iprod,u) = uflw2(ifrom,2+iprod,u) - pslab(i,iprod)
            end do
        else
            mflows(ifrom,l,2) = mflows(ifrom,l,2) + xmterm
            uflw2(ifrom,m,l) = uflw2(ifrom,m,l) - xmterm
            uflw2(ifrom,q,l) = uflw2(ifrom,q,l) - qterm
            do iprod = 1, ns
                uflw2(ifrom,2+iprod,l) = uflw2(ifrom,2+iprod,l) - pslab(i,iprod)
            end do
        end if

        ! put flow into destination room
        do ilay = 1, 2
            mflows(ito,ilay,1) = mflows(ito,ilay,1) + flow_fraction(ilay)*xmterm
            uflw2(ito,m,ilay) = uflw2(ito,m,ilay) + flow_fraction(ilay)*xmterm
            uflw2(ito,q,ilay) = uflw2(ito,q,ilay) + flow_fraction(ilay)*qterm
            do iprod = 1, ns
                uflw2(ito,2+iprod,ilay) = uflw2(ito,2+iprod,ilay) + flow_fraction(ilay)*pslab(i,iprod)
            end do
        end do

    end do
    return
    end subroutine flogo

    ! --------------------------- delp -------------------------------------------

!> \brief   calculate the absolute hydrostatic pressures at a specified elevation in each of two adjacent
!>          rooms and the pressure difference.  the basic calculation involves a determination and differencing of
!>          hydrostatic pressures above a specified absolute reference pressure.
    
!> \param   y (input): vector of heights above absolute reference elevation where pressure difference is to be calculated (m)
!> \param   nelev (input): number of heights to be calculated
!> \param   zflor (input): height of floor in each room above absolute reference elevation (m)
!> \param   zlay (input): height of layer in each room above absolute reference elevation (m)
!> \param   denl (input): lower layer density in each room (kg/m**3)
!> \param   denu (input): upper layer density in each room (kg/m**3)
!> \param   pflor (input): pressure at base of each room above absolute reference pressure (kg/(m*s**2) = Pa)
!> \param   dp (input): change in pressure between two rooms (kg/(m*s**2) = Pa)

    subroutine delp(y,nelev,zflor,zlay,denl,denu,pflor,epsp,dp)

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
