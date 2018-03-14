module target_routines

    use precision_parameters

    use conduction_routines, only: conductive_flux
    use convection_routines, only: convective_flux
    use cylinder_routines, only: cylindrical_conductive_flux
    use fire_routines, only: get_gas_temp_velocity, flame_height
    use numerics_routines, only : ddot, dnrm2
    use radiation_routines, only : absorb, solid_angle_triangle
    use utility_routines, only: xerror

    use cenviro
    use detectorptrs
    use target_data
    use fire_data, only: n_furn, qfurnout, n_fires, fireinfo
    use cparams
    use room_data
    use option_data, only: fcjet, option, off

    implicit none

    real(eb), dimension(2) :: qtcflux, qtfflux, qtwflux, qtgflux    ! temporary variables for target flux calculation
    integer, parameter :: front=1, back=2                           ! subscripts for flux variables

    private

    public target, update_detectors, get_detector_temp_and_velocity, solid_angle_triangle, get_target_temperatures

    contains

! --------------------------- target -------------------------------------------

    subroutine target(update,dt)

    !     routine: target (main target routine)
    !     purpose: compute dassl residuals associated with targets
    !     arguments: update   variable indicating whether temperature profile should be updated
    !                dt       time step

    integer, intent(in) :: update
    real(eb), intent(in) :: dt

    logical :: first = .true.
    real(eb) :: x_node(nnodes_trg), walldx(nnodes_trg), tgrad(2), wk(1), wspec(1), wrho(1), tempin, tempout
    real(eb) :: tderv, wfluxin, wfluxout, wfluxavg, xl
    real(eb) :: flux(2), dflux(2), ttarg(2), t_inf, t_max = 900._eb
    real(eb) :: nu_co2, fed_gas_increment, fed_heat_increment
    integer :: i, itarg, nmnode(2), iieq, iwbound, nslab, iroom, ilayer

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr

    save first,x_node

    ! initialize non-dimensional target node thicknesses the first time target is called
    if (first) then
        first = .false.
        call target_nodes (x_node)
        tempin = 0.0_eb
        tempout = 0.0_eb
    end if

    ! for each target calculate the residual and update target temperature (if update = 1)
    do itarg = 1, n_targets
        targptr => targetinfo(itarg)

        ! calculate net flux striking each side of target
        iroom = targptr%room
        roomptr => roominfo(iroom)
        if (targptr%center(3)>roomptr%depth(l)) then
            t_inf = roomptr%temp(u)
        else
            t_inf = roomptr%temp(l)
        end if
        ttarg(1) = targptr%temperature(idx_tempf_trg)
        ttarg(2) = targptr%temperature(idx_tempb_trg)
        
        call target_flux(front,itarg,ttarg,flux,dflux)
        targptr%flux_incident_front = targptr%flux_surface(front) + targptr%flux_fire(front)+ targptr%flux_gas(front)
        targptr%flux_net_front = flux(front)
        
        call target_flux(back,itarg,ttarg,flux,dflux) 
        targptr%flux_incident_back = targptr%flux_surface(back) + targptr%flux_fire(back) + targptr%flux_gas(back)
        targptr%flux_net_back = flux(back)

        ! do conduction into target
        wfluxin = targptr%flux_net_front
        wfluxout = targptr%flux_net_back
        wspec(1) = targptr%c
        wrho(1) =  targptr%rho
        wk(1) =  targptr%k
        xl = targptr%thickness
        iieq = targptr%equaton_type
        nmnode(1) = nnodes_trg
        nmnode(2) = nnodes_trg - 2
        nslab = 1
        if (iieq==pde) then
            iwbound = 4
            walldx(1:nnodes_trg-1) = xl*x_node(1:nnodes_trg-1)
            call conductive_flux (update,tempin,tempout,dt,wk,wspec,wrho,targptr%temperature,walldx,nmnode,nslab,&
                wfluxin,wfluxout,iwbound,tgrad,tderv)
        else if (iieq==cylpde) then
            wfluxavg = (wfluxin+wfluxout)/2.0_eb
            iwbound = 4
            call cylindrical_conductive_flux (iwbound,tempin,targptr%temperature,nmnode(1),wfluxavg,&
                dt,wk(1),wrho(1),wspec(1),xl,tgrad)
        end if

        ! limit target temperature to flame temperature
        do i = idx_tempf_trg,idx_tempb_trg
            targptr%temperature(i) = min(targptr%temperature(i),t_inf+t_max)
        end do
        
        ! calculate tenability for this time step
        ilayer = u
        if (targptr%center(3)<roomptr%depth(l)) ilayer = l
        nu_co2 = exp(roomptr%species_output(ilayer,co2)/5._eb)
        fed_gas_increment = (dt/60._eb)*nu_co2*((roomptr%species_output(ilayer,co)*10000)/35000._eb + &
            (roomptr%species_output(ilayer,hcn)*10000)**2.36_eb/1.2e6_eb)
        targptr%dfed_gas = targptr%dfed_gas + fed_gas_increment
        targptr%fed_gas = targptr%fed_gas + fed_gas_increment
        if (targptr%flux_incident_front<2500._eb) then
            fed_heat_increment = (dt/60._eb)*1/(4.1e8_eb*(targptr%tgas-kelvin_c_offset)**(-3.61))
        else
            fed_heat_increment = (dt/60._eb)*(1._eb/(4.2*targptr%flux_incident_front/1000._eb)**(-1.9_eb)) + &
                (1._eb/(4.1e8_eb*(targptr%tgas-kelvin_c_offset)**(-3.61)))
        end if
        targptr%dfed_heat = targptr%dfed_heat + fed_heat_increment
        targptr%fed_heat = targptr%fed_heat + fed_heat_increment
    end do
    return
    end subroutine target

! --------------------------- target_flux -------------------------------------------

    subroutine target_flux(iter,itarg,ttarg,flux,dflux)

    !     purpose: routine to calculate flux (and later, temperature) of a target.

    !     arguments: iter   iteration number
    !                itarg  targetnumber
    !                ttarg  front and back target input temperature
    !                flux   front and back output flux
    !                dflux  front and back output flux derivative

    integer, intent(in) :: iter, itarg
    real(eb), intent(in) :: ttarg(2)
    real(eb), intent(out) :: flux(2), dflux(2)

    real(eb) :: svect(3), qwtsum(2), qgassum(2), absu, absl, cosang, s, zfire, fheight
    real(eb) :: xtarg, ytarg, ztarg, zlay, zl, zu, taul, tauu, qfire, qft, qout, zwall, tl, tu, alphal, alphau
    real(eb) :: qwt, qgas, qgt, zznorm, tg, tgb, vg(4)
    real(eb) :: dttarg, dttargb, temis, q1, q2, q1b, q2b, q1g, dqdtarg, dqdtargb
    real(eb) :: target_factors_front(10), target_factors_back(10)
    integer :: map10(10) = (/1,3,3,3,3,4,4,4,4,2/), iroom, i, ifire, iwall, iw, iwb

    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(fire_type), pointer :: fireptr

    absu = 0.50_eb
    absl = 0.01_eb

    targptr => targetinfo(itarg)
    iroom = targptr%room
    roomptr => roominfo(iroom)

    ! terms that do not depend upon the target temperature only need to be calculated once
    if (iter==1) then

        ! initialize flux counters: total, fire, wall, gas
        qtfflux(front:back) = 0.0_eb
        qtgflux(front:back) = 0.0_eb
        qtwflux(front:back) = 0.0_eb

        ! compute radiative flux from fire
        do ifire = 1, n_fires
            fireptr => fireinfo(ifire)
            if (fireptr%room==iroom) then
                svect(1) = targptr%center(1) - fireptr%x_position
                svect(2) = targptr%center(2) - fireptr%y_position
                !zfire = fireptr%z_position + fireptr%z_offset
                ! This is fire radiation at 1/3 the height of the fire (bounded by the ceiling height)
                call flame_height (fireptr%qdot_actual,fireptr%firearea,fheight)
                if (fheight + (fireptr%z_position + fireptr%z_offset)>roomptr%cheight) then
                    zfire = (fireptr%z_position + fireptr%z_offset) + &
                        (roomptr%cheight- (fireptr%z_position + fireptr%z_offset))/3.0_eb
                else
                    zfire = (fireptr%z_position + fireptr%z_offset) + fheight/3.0_eb
                end if
                svect(3) = targptr%center(3) - zfire
                cosang = 0.0_eb
                s = max(dnrm2(3,svect,1),mx_hsep)
                if (s/=0.0_eb) then
                    cosang = -ddot(3,svect,1,targptr%normal,1)/s
                end if
                ztarg = targptr%center(3)
                zlay = roomptr%depth(l)

                ! compute portion of path in lower and upper layers
                call getylyu(zfire,zlay,ztarg,s,zl,zu)
                if (n_furn>0) then
                    absl=0.0
                    absu=0.0
                    taul = 1.0_eb
                    tauu = 1.0_eb
                    qfire = 0.0_eb
                else
                    absl = absorb(iroom, l)
                    absu = absorb(iroom, u)
                    taul = exp(-absl*zl)
                    tauu = exp(-absu*zu)
                    qfire = fireptr%qdot_radiative
                end if
                if (s/=0.0_eb) then
                    qft = qfire*abs(cosang)*tauu*taul/(4.0_eb*pi*s**2)
                else
                    qft = 0.0_eb
                end if

                ! decide whether flux is hitting front or back of target. if it's hitting the back target only add contribution
                ! if the target is interior to the room
                if (cosang>=0.0_eb) then
                    qtfflux(1) = qtfflux(1) + qft
                else
                    if (targptr%back==interior) then
                        qtfflux(2) = qtfflux(2) + qft
                    end if
                end if
            end if
        end do

        ! compute radiative flux from walls and gas
        qwtsum(front) = 0.0_eb
        qgassum(front) = 0.0_eb
        qwtsum(back) = 0.0_eb
        qgassum(back) = 0.0_eb
        call get_target_factors (iroom,itarg,target_factors_front,target_factors_back)
        do iwall = 1, 10
            if (n_furn>0) then
                qout=qfurnout
            else
                qout = roomptr%rad_qout(map10(iwall))
            end if
            svect(1:3) = targptr%center(1:3) - roomptr%wall_center(1:3,iwall)
            s = dnrm2(3,svect,1)
            zwall = roomptr%wall_center(3,iwall)
            ztarg = targptr%center(3)
            zlay = roomptr%depth(l)
            tl = roomptr%temp(l)
            tu = roomptr%temp(u)

            ! compute path length in lower (zl) and upper (zu) layer
            call getylyu(zwall,zlay,ztarg,s,zl,zu)
            ! For no fire cases
            absu = absorb(iroom, u)
            absl = absorb(iroom, l)

            ! find fractions transmitted and absorbed in lower and upper layer
            taul = exp(-absl*zl)
            alphal = 1.0_eb - taul
            tauu = exp(-absu*zu)
            alphau = 1.0_eb - tauu

            qwt = qout*taul*tauu
            if (iwall<=5) then
                qgas = tl**4*alphal*tauu + tu**4*alphau
            else
                qgas = tu**4*alphau*taul + tl**4*alphal
            end if
            qgt = sigma*qgas

            qwtsum(front) = qwtsum(front) + qwt*target_factors_front(iwall)
            qgassum(front) = qgassum(front) + qgt*target_factors_front(iwall)
            if (targptr%back==interior) then
                qwtsum(back) = qwtsum(back) + qwt*target_factors_back(iwall)
                qgassum(back) = qgassum(back) + qgt*target_factors_back(iwall)
            end if
        end do
        qtwflux(front) = qwtsum(front)
        qtgflux(front) = qgassum(front)
        qtwflux(back) = qwtsum(back)
        qtgflux(back) = qgassum(back)

        ! if the target rear was exterior then calculate the flux assuming ambient outside conditions
        if (targptr%back==exterior.or.qtgflux(back)==0.0) qtgflux(back) = sigma*interior_ambient_temperature**4
    end if

    ! compute convective flux
    ! assume target is a 'floor', 'ceiling' or 'wall' depending on how much the target is tilted.
    zznorm = targptr%normal(3)
    if (zznorm<=1.0_eb.and.zznorm>=cos45) then
        iw = 2
        iwb = 1
    else if (zznorm>=-1.0_eb.and.zznorm<=-cos45) then
        iw = 1
        iwb = 2
    else
        iw = 3
        iwb = 3
    end if

    xtarg = targptr%center(1)
    ytarg = targptr%center(2)
    ztarg = targptr%center(3)
    call get_gas_temp_velocity(iroom,xtarg,ytarg,ztarg,tg,vg)
    targptr%tgas = tg
    if (targptr%back==interior) then
        tgb = tg
    else
        tgb = interior_ambient_temperature
    end if
    dttarg = 1.0e-7_eb*ttarg(front)
    dttargb = 1.0e-7_eb*ttarg(back)

    temis = targptr%emissivity

    ! convection for the front
    call convective_flux (iw,tg,ttarg(front),q1)
    call convective_flux (iw,tg,ttarg(front)+dttarg,q2)
    qtcflux(1) = q1
    dqdtarg = (q2-q1)/dttarg

    flux(front) = temis*(qtfflux(front) + qtwflux(front) + qtgflux(front)) + qtcflux(front) - temis*sigma*ttarg(front)**4
    dflux(front) = -4.0_eb*temis*sigma*ttarg(front)**3 + dqdtarg

    ! convection for the back
    call convective_flux(iwb,tgb,ttarg(back),q1b)
    call convective_flux(iwb,tgb,ttarg(back)+dttargb,q2b)
    qtcflux(back) = q1b
    dqdtargb = (q2b-q1b)/dttargb

    flux(back) = temis*(qtfflux(back) + qtwflux(back) + qtgflux(back)) + qtcflux(back) - temis*sigma*ttarg(back)**4
    dflux(back) = -4.0_eb*temis*sigma*ttarg(back)**3 + dqdtargb

    ! store fluxes for printout
    do i = front,back
        targptr%flux_fire(i) = temis*qtfflux(i)
        targptr%flux_gas(i) = temis*qtgflux(i)
        targptr%flux_surface(i) = temis*qtwflux(i)
        targptr%flux_convection(i) = qtcflux(i)
        targptr%flux_target(i) = -temis*sigma*ttarg(i)**4
        targptr%flux_radiation(i) = targptr%flux_fire(i) + targptr%flux_gas(i) + targptr%flux_surface(i) + &
            targptr%flux_target(i)
        targptr%flux_net(i) = targptr%flux_fire(i) + targptr%flux_gas(i) + targptr%flux_surface(i) + &
            targptr%flux_convection(i) + targptr%flux_target(i)

        call convective_flux (iw,tg,interior_ambient_temperature,q1g)
        targptr%flux_convection_gauge = q1g
        targptr%flux_target_gauge(i) = -temis*sigma*interior_ambient_temperature**4
        targptr%flux_radiation_gauge(i) = targptr%flux_fire(i) + targptr%flux_gas(i) + targptr%flux_surface(i) + &
            targptr%flux_target_gauge(i)
        targptr%flux_net_gauge(i) = targptr%flux_fire(i) + targptr%flux_gas(i) + targptr%flux_surface(i) + &
            targptr%flux_convection_gauge(i) + targptr%flux_target_gauge(i)
    end do

    return
    end subroutine target_flux

! ---------------------------- target_nodes -----------------------------------

    subroutine target_nodes (x_node)

    !     purpose: calculate thickness of internal nodes in a plate target
    !     arguments: x_node  array of node thicknesses from front to back

        real(eb), intent(out) :: x_node(nnodes_trg)

        integer :: i
        real(eb) :: sum

        x_node(1:nnodes_trg) = 1.0_eb
        do i = 2, (nnodes_trg-1)/2
            x_node(i) = x_node(i-1)*1.50_eb
            x_node(nnodes_trg-i) = x_node(i)
        end do
        if (mod((nnodes_trg-1),2)==1) x_node((nnodes_trg-1)/2+1) = x_node((nnodes_trg-1)/2)*1.50_eb
        sum = 0.0_eb
        do i = 1, nnodes_trg-1
            sum = sum + x_node(i)
        end do
        do i = 1, nnodes_trg-1
            x_node(i) = x_node(i)/sum
        end do

        return

    end subroutine target_nodes

! --------------------------- get_target_factors -------------------------------------------

    subroutine get_target_factors (iroom,itarg,target_factors_front,target_factors_back)

    integer, intent(in) :: iroom, itarg
    real(eb), intent(out), dimension(10) :: target_factors_front, target_factors_back

    type(room_type), pointer :: roomi
    type(target_type), pointer :: targptr

    real(eb) :: rel_room_vert(3)
    real(eb), dimension(12) :: vert_distance
    real(eb), target :: room_verts(3,12), solid_angle_front_verts(3,8), solid_angle_back_verts(3,8)
    real(eb), pointer, dimension(:) :: v1, v2, v3
    real(eb) :: factor, d1, d2, d3, t1(3), t2(3), t3(3), v0(3)
    real(eb) :: sum_front, sum_back, solid_angle, zlay
    real(eb) :: x0,x1,y0,y1,z0,z1

    integer :: nsolid_front_verts, nsolid_back_verts
    integer, parameter :: front=1, back=2
    integer :: i, iwall, ivert, skiptagr

 ! vertices
 !       3--------------4
 !      /              /
 !     /              /
 !    /              /
 !   1--------------2
 !
 !       7--------------8
 !      /              /
 !     /              /
 !    /              /
 !   5--------------6

 !       11------------12
 !      /              /
 !     /              /
 !    /              /
 !   9-------------10

    integer, dimension(5,10), target :: faces
    integer, dimension(:), pointer :: facei

 ! faces   (vertices listed counter clockwise as you are facing the face)
 ! same convention as used for roomptr%wall_center (see def in cfast.f90 )
    data ((faces(i,iwall),i=1,5),iwall=1,10) /& ! for convenience repeat first and last vertex
    1,  2,  4,  3, 1, &    ! ceiling
    3,  4,  8,  7, 3, &    ! upper back
    2,  6,  8,  4, 2, &    ! upper right
    1,  5,  6,  2, 1, &    ! upper front
    1,  3,  7,  5, 1, &    ! upper left
    7,  8, 12, 11, 7, &    ! lower back
    6, 10, 12,  8, 6, &    ! lower right
    5,  9, 10,  6, 5, &    ! lower front
    5,  7, 11,  9, 5, &    ! lower left
    9, 11, 12, 10, 9  &    ! floor
    /

    roomi => roominfo(iroom)
    targptr => targetinfo(itarg)

    !define vertex locations

    x0 = 0.0_eb
    y0 = 0.0_eb
    z0 = 0.0_eb
    x1 = roomi%cwidth
    y1 = roomi%cdepth
    z1 = roomi%cheight
    zlay = roomi%depth(l)
    
    room_verts(1:3,1)  = (/x0, y0, z1/)
    room_verts(1:3,2)  = (/x1, y0, z1/)
    room_verts(1:3,3)  = (/x0, y1, z1/)
    room_verts(1:3,4)  = (/x1, y1, z1/)
    room_verts(1:3,5)  = (/x0, y0, zlay/)
    room_verts(1:3,6)  = (/x1, y0, zlay/)
    room_verts(1:3,7)  = (/x0, y1, zlay/)
    room_verts(1:3,8)  = (/x1, y1, zlay/)
    room_verts(1:3,9)  = (/x0, y0, z0/)
    room_verts(1:3,10) = (/x1, y0, z0/)
    room_verts(1:3,11) = (/x0, y1, z0/)
    room_verts(1:3,12) = (/x1, y1, z0/)

! vert_distance = target_normal_xyz .dot. (vertex_xyz - target_origin_xyz)

    do ivert = 1, 12
       rel_room_vert(1:3) = room_verts(1:3,ivert) - targptr%center(1:3)
       vert_distance(ivert) = ddot(3,rel_room_vert,1,targptr%normal,1)
    end do

    target_factors_front(1:10) = 0.0_eb
    target_factors_back(1:10) = 0.0_eb
    do iwall=1, 10
       facei=>faces(1:5,iwall)
       skiptagr = 0
       do ivert = 1, 5
           if (vert_distance(facei(ivert)) == 0 .or. vert_distance(facei(ivert)) == 0 .or. &
               vert_distance(facei(ivert)) == 0) skiptagr = skiptagr + 1
       end do
       if (skiptagr /= 5) then
           nsolid_front_verts=0
           nsolid_back_verts=0
           do ivert = 1, 4
               d1 = vert_distance(facei(ivert))
               v1(1:3) => room_verts(1:3,facei(ivert))

               d2 = vert_distance(facei(ivert+1))
               v2(1:3) => room_verts(1:3,facei(ivert+1))

               if (d1.ge.0) then  ! face vertex is above target plane
                   nsolid_front_verts=nsolid_front_verts+1
                   solid_angle_front_verts(1:3,nsolid_front_verts) = v1(1:3)
               end if

               if (d1.le.0) then  ! face vertex is below target plane
                   nsolid_back_verts=nsolid_back_verts+1
                   solid_angle_back_verts(1:3,nsolid_back_verts) = v1(1:3)
               end if
               if (d1*d2.lt.0) then ! two successive face vertices are on opposite sides of target plane
                   ! interpolate to find vertex that lies on target plane
                   !  d1   0    d2
                   !  v1   v0   v2
                   !
                   !  (v0-v1)/(0-d1) = (v2-v1)/(d2-d1)
                   !  solve for v0
                   ! v0 = (1-(-d1)/(d2-d1))*v1 + (-d1/(d2-d1))*v2
                   ! note: d2-d1 can't be 0 since d1*d2<0.0
                   nsolid_front_verts=nsolid_front_verts+1
                   nsolid_back_verts=nsolid_back_verts+1

                   factor = -d1/(d2-d1)
                   v0(1:3) = (1.0_eb-factor)*v1 + factor*v2
                   solid_angle_front_verts(1:3,nsolid_front_verts) = v0(1:3)
                   solid_angle_back_verts(1:3,nsolid_back_verts) =   v0(1:3)
               end if
           end do
           if (nsolid_front_verts.ge.3) then

               ! triangulate polygon, compute solid angle for each triangle and sum
               ! the solid angle is zero whenever a vertex coincides with the target location

               v1 => solid_angle_front_verts(1:3,1)
               t1(1:3) = v1(1:3) - targptr%center(1:3)
               d1 = dnrm2(3,t1,1)

               if (d1.gt.0.0_eb) then
                   t1(1:3) = t1(1:3)/d1
                   do i = 2, nsolid_front_verts-1
                       v2 => solid_angle_front_verts(1:3,i)

                       t2(1:3) = v2(1:3) - targptr%center(1:3)
                       d2 = dnrm2(3,t2,1)
                       if (d2.eq.0.0_eb)cycle

                       v3 => solid_angle_front_verts(1:3,i+1)
                       t3(1:3) = v3(1:3) - targptr%center(1:3)
                       d3 = dnrm2(3,t3,1)
                       if (d3.eq.0.0_eb)cycle

                       t2(1:3) = t2(1:3)/d2
                       t3(1:3) = t3(1:3)/d3

                       call solid_angle_triangle(solid_angle,t1,t2,t3)
                       target_factors_front(iwall) = target_factors_front(iwall) + solid_angle
                   end do
               end if
           end if
           if (nsolid_back_verts.ge.3) then

               ! triangulate polygon, compute solid angle for each triangle and sum
               ! the solid angle is zero whenever a vertex coincides with the target location

               v1 => solid_angle_back_verts(1:3,1)
               t1(1:3) = v1(1:3) - targptr%center(1:3)
               d1 = dnrm2(3,t1,1)

               if (d1.gt.0.0_eb) then
                   t1(1:3) = t1(1:3)/d1
                   do i = 2, nsolid_back_verts-1
                       v2 => solid_angle_back_verts(1:3,i)

                       t2(1:3) = v2(1:3) - targptr%center(1:3)
                       d2 = dnrm2(3,t2,1)
                       if (d2.eq.0.0_eb)cycle

                       v3 => solid_angle_back_verts(1:3,i+1)
                       t3(1:3) = v3(1:3) - targptr%center(1:3)
                       d3 = dnrm2(3,t3,1)
                       if (d3.eq.0.0_eb)cycle

                       t2(1:3) = t2(1:3)/d2
                       t3(1:3) = t3(1:3)/d3

                       call solid_angle_triangle(solid_angle,t1,t2,t3)
                       target_factors_back(iwall) = target_factors_back(iwall) + solid_angle
                   end do
               end if
           end if
       end if
    end do
    sum_front = 0.0_eb
    sum_back = 0.0_eb
    do iwall=1, 10
       sum_front = sum_front + target_factors_front(iwall)
       sum_back = sum_back + target_factors_back(iwall)
    end do
    if (sum_front>0.0_eb)target_factors_front(1:10) = target_factors_front(1:10)/sum_front
    if (sum_back>0.0_eb)target_factors_back(1:10) = target_factors_back(1:10)/sum_back

 end subroutine get_target_factors

! --------------------------- getylyu -------------------------------------------

    subroutine getylyu (yo,y,yt,s,yl,yu)

    !     routine: gettylyu
    !     purpose: compute portion of path in lower and upper layers

    real(eb), intent(in) :: yo, y, yt, s
    real(eb), intent(out) :: yl, yu


    if (yo<=y) then
        if (yt<=y) then
            yl = 1.0_eb
        else
            yl = (y-yo)/(yt-yo)
        end if
    else
        if (yt>=y) then
            yl = 0.0_eb
        else
            yl = (y-yt)/(yo-yt)
        end if
    end if
    yl = yl*s
    yu = s - yl
    return
    end subroutine getylyu

! --------------------------- get_target_temperature -------------------------------------------

    subroutine get_target_temperatures ()

    !   purpose: updates the internal temperature of each target at the depth(s) specified by the user

    integer itarg
    real(eb) :: diam, dr, r, rint, factor, depth, tempx, x_node(nnodes_trg), targx, targdx(nnodes_trg)
    integer :: nx, left, right, i
    type(target_type), pointer :: targptr

    call target_nodes (x_node)

    do itarg = 1, n_targets
        targptr => targetinfo(itarg)

        if (targptr%equaton_type == cylpde) then

            ! cylindrical targets calculate as a function of radius from center
            diam = targptr%thickness
            r = min(diam,max(abs((diam/2) - targptr%depth_loc),0.0_eb))
            nx = nnodes_trg
            dr = (diam/2)/nx

            if (r<=dr/2.0_eb) then
                tempx = targptr%temperature(1)
            else if (r>=(diam/2)-dr/2.0_eb) then
                tempx = targptr%temperature(nx)
            else
                rint = r/dr-0.5_eb
                left = int(rint)+1
                left=max(min(left,nx),1)
                right = left + 1
                right=max(min(right,nx),1)
                factor = (rint-int(rint))
                tempx = factor*targptr%temperature(right) + (1.0_eb-factor)*targptr%temperature(left)
            end if

            targptr%tfront = targptr%temperature(nx)
            targptr%tinternal = tempx
            targptr%tback = targptr%temperature(1)
        else

            ! flat targets calculate as a function of depth from the surface
            nx = nnodes_trg
            targdx(1:nx) = targptr%thickness*x_node(1:nx)
            depth = min(targptr%thickness,max(targptr%depth_loc,0.0_eb))

            if (depth<=x_node(1)/2) then
                tempx = targptr%temperature(1)
            else if (depth>=targptr%thickness-x_node(nx-1)/2) then
                tempx = targptr%temperature(nx)
            else
                targx = 0.0_eb
                do i = 1, nx-1
                    if (depth>=targx.and.depth<=targx+targdx(i)) then
                        left = i
                        exit
                    end if
                    targx = targx + targdx(i)
                end do
                right = left + 1
                factor = (depth-targx)/targdx(left)
                tempx = factor*targptr%temperature(right) + (1.0_eb-factor)*targptr%temperature(left)
            end if
            targptr%tfront = targptr%temperature(1)
            targptr%tinternal = tempx
            targptr%tback = targptr%temperature(nx)

        end if
    end do

    return

    end subroutine get_target_temperatures

! --------------------------- update_detectors -------------------------------------------

    subroutine update_detectors (imode,tcur,dstep,n_detectors,idset,ifdtect,tdtect)

    !     routine: update_detectors
    !     purpose: updates the temperature of each detector link.  it also determine whether the
    !              detector has activated in the time interval (tcur,tcur+dstep).  if this has occured then a
    !              quenching algorithm will be invoked if the appropriate option has been set.
    !     arguments: tcur    current time
    !                dstep   time step size (to next time)
    !                n_detectors  number of detectors
    !                ixdtect 2-d array containing integer detector data structures
    !                idset   room where activated detector resides

    integer, intent(in) :: imode, n_detectors
    real(eb), intent(in) :: tcur, dstep

    integer, intent(out) :: idset, ifdtect
    real(eb), intent(out) :: tdtect

    real(eb) :: cjetmin, tlink, tlinko, zdetect, tlay, tjet, tjeto, vel, velo, rti, trig, an, bn, anp1, &
       bnp1, denom, fact1, fact2, delta, tmp, tlink_smld, tlinko_smld, delta_smld, trig_smld
    integer :: i, iroom, idold, iqu
    character(133) :: messg
    character(11) :: detector_names(3) = (/'Smoke Alarm','Heat Alarm ','Sprinkler  '/)
    type(detector_type), pointer :: dtectptr, previous_activation
    type(room_type), pointer :: roomptr

    idset = 0
    ifdtect = 0
    tdtect = tcur+2*dstep
    cjetmin = 0.10_eb
    do i = 1, n_detectors
        dtectptr => detectorinfo(i)

        iroom = dtectptr%room
        roomptr => roominfo(iroom)

        zdetect = dtectptr%center(3)
        if (zdetect>roomptr%depth(l)) then
            tlay = roomptr%temp(u)
        else
            tlay = roomptr%temp(l)
        end if

        tjet = max(dtectptr%temp_gas,tlay)
        tjeto = max(dtectptr%temp_gas_o,tlay)
        vel = max(dtectptr%velocity,cjetmin)
        velo = max(dtectptr%velocity_o,cjetmin)

        if (dtectptr%dtype==smoked .and. .not.dtectptr%dual_detector) then
            trig = log10(1._eb/(1._eb-dtectptr%trigger/100._eb))
            tlinko = dtectptr%value
            tlink = dtectptr%obscuration
            if (tlinko<trig.and.trig<=tlink.and..not.dtectptr%activated) then
                delta = (trig-tlinko)/(tlink-tlinko)
            else
                delta = 10
            end if
        else if (dtectptr%dtype==smoked .and. dtectptr%dual_detector) then
            trig = log10(1._eb/(1._eb-dtectptr%trigger/100._eb))
            tlinko = dtectptr%value
            tlink = dtectptr%obscuration_flaming
            if (tlinko<trig.and.trig<=tlink.and..not.dtectptr%activated) then
                delta = (trig-tlinko)/(tlink-tlinko)
            else
                delta = 10
            end if
            trig_smld = log10(1._eb/(1._eb-dtectptr%trigger_smolder/100._eb))
            tlinko_smld = dtectptr%value_smolder
            tlink_smld = dtectptr%obscuration_smolder
            if (tlinko_smld<trig_smld.and.trig_smld<=tlink_smld.and..not.dtectptr%activated) then
                delta_smld = (trig_smld-tlinko_smld)/(tlink_smld-tlinko_smld)
            else
                delta_smld = 10
            end if
            if (delta_smld < delta) then
                delta = delta_smld
            end if
        else if (dtectptr%dtype>=heatd) then
            rti = dtectptr%rti
            trig = dtectptr%trigger
            tlinko = dtectptr%value
            bn = sqrt(velo)/rti
            an = bn*tjeto
            bnp1 = sqrt(vel)/rti
            anp1 = bnp1*tjet
            denom = 1.0_eb + dstep*bnp1*.5_eb
            fact1 = (1.0_eb - dstep*bn*.50_eb)/denom
            fact2 = dstep/denom
            tlink = fact1*tlinko + fact2*(an+anp1)*0.5_eb
            if (tlinko<trig.and.trig<=tlink.and..not.dtectptr%activated) then
                delta = (trig-tlinko)/(tlink-tlinko)
            else
                delta = 10
            end if
        end if
        if (imode>0) then
            dtectptr%value_o = tlinko
            dtectptr%value = tlink
            if (dtectptr%dual_detector) then
                dtectptr%value_o_smolder = tlinko_smld
                dtectptr%value_smolder = tlink_smld
            end if
        end if

        ! determine if detector has activated in this time interval (and not earlier)
        !if (tlinko<trig.and.trig<=tlink.and..not.dtectptr%activated) then
        if (delta <= 1.0_eb .and. .not.dtectptr%activated) then 
            !delta = (trig-tlinko)/(tlink-tlinko)
            tmp = tcur+dstep*delta
            tdtect = min(tmp,tdtect)
            ifdtect = i
            if (imode>0) then
                dtectptr%activation_time = tcur + dstep*delta
                dtectptr%activated = .true.
                ! tell the world about the activation
                if (.not.dtectptr%reported) then
                    dtectptr%reported = .true.
                    call device_activated (i, tdtect, 1)
                    write (messg,'(2a,i0,a,i0,a,i0)') trim(detector_names(dtectptr%dtype)),' (Sensor ',i, ') has activated at ', &
                        int(tdtect+0.5_eb), ' s in compartment ',dtectptr%room
                    call xerror(messg,0,1,-3)
                end if

                ! determine if this is the first detector to have activated in this room
                idold = roomptr%sprinkler_activated
                iqu = 0
                if (idold==0) then
                    iqu = i
                else
                    previous_activation => detectorinfo(idold)
                    if (dtectptr%activation_time<previous_activation%activation_time) then

                        ! this can only happen if two detectors have activated in the same room in the same
                        ! (possibly very short) time interval
                        iqu = i
                    end if
                end if

                ! if this detector has activated before all others in this room and the quenching flag was turned on
                !  then let the sprinkler quench the fire
                if (iqu/=0.and.dtectptr%quench) then
                    roomptr%sprinkler_activated = iqu
                    idset = iroom
                end if
            end if
        end if
        dtectptr%temp_gas_o = tjet
        dtectptr%velocity_o = vel
    end do
    return
    end subroutine update_detectors

    ! --------------------------- detector_temp_and_velocity -------------------------------------------

    subroutine get_detector_temp_and_velocity

    !     routine:     get_detector_temp_and_velocity

    !     description:  calculates near-detector gas temperature, velocity, and smoke obscuration

    real(eb) :: xloc, yloc, zloc, tg, vg(4)
    integer :: id, iroom
    type(detector_type), pointer :: dtectptr
    type(room_type), pointer :: roomptr

    ! If ceiling jet option is turned off, conditions default to the appropriate layer temperature
    do id = 1, n_detectors
        dtectptr => detectorinfo(id)
        iroom = dtectptr%room
        roomptr => roominfo(iroom)
        xloc = dtectptr%center(1)
        yloc = dtectptr%center(2)
        zloc = dtectptr%center(3)
        if (option(fcjet)==off) then
            ! if ceiling jet option is off, things default to appropriate layer temperature
            if (zloc>roomptr%depth(l)) then
                dtectptr%temp_gas = roomptr%temp(u)
                !dtectptr%obscuration = roomptr%species_output(u,9)
                dtectptr%obscuration = roomptr%species_output(u,soot)
                dtectptr%obscuration_flaming = roomptr%species_output(u,soot_flaming)
                dtectptr%obscuration_smolder = roomptr%species_output(u,soot_smolder)
            else
                dtectptr%temp_gas = roomptr%temp(l)
                !dtectptr%obscuration = roomptr%species_output(l,9)
                dtectptr%obscuration = roomptr%species_output(l,soot)
                dtectptr%obscuration_flaming = roomptr%species_output(l,soot_flaming)
                dtectptr%obscuration_smolder = roomptr%species_output(l,soot_smolder)
            end if
            dtectptr%velocity = 0.1_eb
        else
            ! if ceiling jet option is on, temeperature is determined by plume and ceiling jet algorithms
            call get_gas_temp_velocity (iroom,xloc,yloc,zloc,tg,vg)
            dtectptr%temp_gas = tg
            dtectptr%velocity = vg(4)
            if (zloc>roomptr%depth(l)) then
                !dtectptr%obscuration = roomptr%species_output(u,9)
                dtectptr%obscuration = roomptr%species_output(u,soot)
                dtectptr%obscuration_flaming = roomptr%species_output(u,soot_flaming)
                dtectptr%obscuration_smolder = roomptr%species_output(u,soot_smolder)
            else
                !dtectptr%obscuration = roomptr%species_output(l,9)
                dtectptr%obscuration = roomptr%species_output(l,soot)
                dtectptr%obscuration_flaming = roomptr%species_output(l,soot_flaming)
                dtectptr%obscuration_smolder = roomptr%species_output(l,soot_smolder)
            end if
        end if
    end do

    return

    end subroutine get_detector_temp_and_velocity

    ! --------------------------- smv_device_activated -------------------------------------------

    subroutine device_activated (idtect, tdtect, istate)

    !
    ! this routines records a device activation in the smv file
    !
    !   idtect: detector number that activated
    !   tdtect: activation time
    !   istate: activation state: 0 for not activated, 1 for activated

    integer, intent(in) :: idtect, istate
    real(eb), intent(in) :: tdtect

    write (13, "(a)") "DEVICE_ACT"
    write (13, "(i6,f10.2,i6)") idtect, tdtect, istate
    return

    end subroutine device_activated

end module target_routines