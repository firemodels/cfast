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
    use cfast_main
    use fltarget
    use fireptrs
    use wnodes, only: nfurn, qfurnout
    use opt, only: fcjet, option, off
    
    implicit none
    
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
    integer :: i, itarg, nmnode(2), iieq, iwbound, nslab, iroom
    
    type(target_type), pointer :: targptr
    
    save first,x_node

    ! initialize non-dimensional target node thicknesses the first time target is called
    if(first)then
        first = .false.
        call target_nodes (x_node)
        tempin = 0.0_eb
        tempout = 0.0_eb
    end if

    ! for each target calculate the residual and update target temperature (if update = 1)
    do itarg = 1, ntarg
        targptr => targetinfo(itarg)
        
        ! calculate net flux striking each side of target
        iroom = targptr%room
        if (targptr%center(3)>zzhlay(iroom,lower)) then
            t_inf = zztemp(iroom,upper)
        else
            t_inf = zztemp(iroom,lower)
        end if
        ttarg(1) = targptr%temperature(idx_tempf_trg)
        ttarg(2) = targptr%temperature(idx_tempb_trg)
        call target_flux(1,itarg,ttarg,flux,dflux)
        call target_flux(2,itarg,ttarg,flux,dflux)
        targptr%flux_front = qtwflux(1) + qtfflux(1) + qtcflux(1) + qtgflux(1)
        targptr%flux_back = qtwflux(2) + qtfflux(2) + qtcflux(2) + qtgflux(2)
        targptr%flux_net_front = flux(1)
        targptr%flux_net_back = flux(2)
        
        ! do conduction into target
        wfluxin = targptr%flux_net_front
        wfluxout = targptr%flux_net_back
        wspec(1) = targptr%cp
        wrho(1) =  targptr%rho
        wk(1) =  targptr%k
        xl = targptr%thickness
        iieq = targptr%equaton_type
        nmnode(1) = nnodes_trg
        nmnode(2) = nnodes_trg - 2
        nslab = 1
        if(iieq==pde)then
            iwbound = 4
            walldx(1:nnodes_trg-1) = xl*x_node(1:nnodes_trg-1)
            call conductive_flux (update,tempin,tempout,dt,wk,wspec,wrho,targptr%temperature,walldx,nmnode,nslab,&
                wfluxin,wfluxout,iwbound,tgrad,tderv)
        else if(iieq==cylpde)then
            wfluxavg = (wfluxin+wfluxout)/2.0_eb
            iwbound = 4
            call cylindrical_conductive_flux (iwbound,tempin,targptr%temperature,nmnode(1),wfluxavg,&
                dt,wk(1),wrho(1),wspec(1),xl,tgrad)
        end if
        
        ! limit target temperature to flame temperature
        do i = idx_tempf_trg,idx_tempb_trg
            targptr%temperature(i) = min(targptr%temperature(i),t_inf+t_max)
        end do
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
    integer :: map10(10), iroom, i, nfirerm, istart, ifire, iwall, iw, iwb
    integer, parameter :: front=1, back=2
    
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr

    data map10/1,3,3,3,3,4,4,4,4,2/

    absu = 0.50_eb
    absl = 0.01_eb
    
    targptr => targetinfo(itarg)
    iroom = targptr%room
    roomptr => roominfo(iroom)
    
    ! terms that do not depend upon the target temperature only need to be calculated once
    if(iter==1)then

        ! initialize flux counters: total, fire, wall, gas
        qtfflux(front:back) = 0.0_eb
        qtgflux(front:back) = 0.0_eb
        qtwflux(front:back) = 0.0_eb

        nfirerm = ifrpnt(iroom,1)
        istart = ifrpnt(iroom,2)

        ! compute radiative flux from fire
        do ifire = istart, istart + nfirerm - 1
            svect(1) = targptr%center(1) - xfire(ifire,f_fire_xpos)
            svect(2) = targptr%center(2) - xfire(ifire,f_fire_ypos)
            !svect(3) = targptr%center(3) - xfire(ifire,f_fire_zpos)! This is point radiation at the base of the fire
            ! This is fire radiation at 1/3 the height of the fire (bounded by the ceiling height)
            call flame_height (xfire(ifire,f_qfr)+xfire(ifire,f_qfc),xfire(ifire,f_obj_area),fheight)
            if(fheight+xfire(ifire,f_fire_zpos)>roomptr%height)then
                zfire = xfire(ifire,f_fire_zpos) + (roomptr%height-xfire(ifire,f_fire_zpos))/3.0_eb
            else
                zfire = xfire(ifire,f_fire_zpos) + fheight/3.0_eb
            end if
            svect(3) = targptr%center(3) - zfire
            cosang = 0.0_eb
            s = max(dnrm2(3,svect,1),mx_hsep)
            if(s/=0.0_eb)then
                cosang = -ddot(3,svect,1,targptr%normal,1)/s
            end if
            ztarg = targptr%center(3)
            zlay = zzhlay(iroom,lower)

            ! compute portion of path in lower and upper layers
            call getylyu(zfire,zlay,ztarg,s,zl,zu)
            if(nfurn>0)then
                absl=0.0
                absu=0.0
                taul = 1.0_eb
                tauu = 1.0_eb
                qfire = 0.0_eb
            else
                absl = absorb(iroom, lower)
                absu = absorb(iroom, upper)
                taul = exp(-absl*zl)
                tauu = exp(-absu*zu)
                qfire = xfire(ifire,f_qfr)
            end if
            if(s/=0.0_eb)then
                qft = qfire*abs(cosang)*tauu*taul/(4.0_eb*pi*s**2)
            else
                qft = 0.0_eb
            end if

            ! decide whether flux is hitting front or back of target. if it's hitting the back target only add contribution
            ! if the target is interior to the room
            if(cosang>=0.0_eb)then
                qtfflux(1) = qtfflux(1) + qft
            else
                if(targptr%back==interior)then
                    qtfflux(2) = qtfflux(2) + qft
                end if
            end if

        end do
        if (stime>10.0_eb .and. itarg>=153) then
            continue
        end if
        ! compute radiative flux from walls and gas
        qwtsum(front) = 0.0_eb
        qgassum(front) = 0.0_eb
        qwtsum(back) = 0.0_eb
        qgassum(back) = 0.0_eb
        call get_target_factors2(iroom,itarg,target_factors_front,target_factors_back)
        do iwall = 1, 10
            if(nfurn>0)then
                qout=qfurnout
            else
                qout = rdqout(map10(iwall),iroom)
            end if
            svect(1:3) = targptr%center(1:3) - roomptr%wall_center(1:3,iwall)
            s = dnrm2(3,svect,1)
            zwall = roomptr%wall_center(3,iwall)
            ztarg = targptr%center(3)
            zlay = zzhlay(iroom,lower)
            tl = zztemp(iroom,lower)
            tu = zztemp(iroom,upper)

            ! compute path length in lower (zl) and upper (zu) layer
            call getylyu(zwall,zlay,ztarg,s,zl,zu)

            ! find fractions transmitted and absorbed in lower and upper layer
            taul = exp(-absl*zl)
            alphal = 1.0_eb - taul
            tauu = exp(-absu*zu)
            alphau = 1.0_eb - tauu

            qwt = qout*taul*tauu
            if(iwall<=5)then
                qgas = tl**4*alphal*tauu + tu**4*alphau
            else
                qgas = tu**4*alphau*taul + tl**4*alphal
            end if
            qgt = sigma*qgas

            qwtsum(front) = qwtsum(front) + qwt*target_factors_front(iwall)
            qgassum(front) = qgassum(front) + qgt*target_factors_front(iwall)
            if(targptr%back==interior)then
              qwtsum(back) = qwtsum(back) + qwt*target_factors_back(iwall)
              qgassum(back) = qgassum(back) + qgt*target_factors_back(iwall)
            end if
        end do
        qtwflux(front) = qwtsum(front)
        qtgflux(front) = qgassum(front)
        qtwflux(back) = qwtsum(back)
        qtgflux(back) = qgassum(back)

        ! if the target rear was exterior then calculate the flux assuming ambient outside conditions
        if(targptr%back==exterior.or.qtgflux(back)==0.0) qtgflux(back) = sigma*interior_temperature**4
    end if

    ! compute convective flux
    ! assume target is a 'floor', 'ceiling' or 'wall' depending on how much the target is tilted.  
    zznorm = targptr%normal(3)
    if(zznorm<=1.0_eb.and.zznorm>=cos45)then
        iw = 2
        iwb = 1
    elseif(zznorm>=-1.0_eb.and.zznorm<=-cos45)then
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
    if(targptr%back==interior)then
        tgb = tg
    else
        tgb = interior_temperature
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

        call convective_flux (iw,tg,interior_temperature,q1g)
        targptr%flux_convection_gauge = q1g
        targptr%flux_target_gauge(i) = -temis*sigma*interior_temperature**4
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
        
        integer :: i, nnn
        real(eb) :: sum
    
        nnn = nnodes_trg - 1
        x_node(1) = 1.0_eb
        x_node(nnn) = 1.0_eb
        do i = 2, nnn/2 
            x_node(i) = x_node(i-1)*1.50_eb
            x_node(nnn+1-i) = x_node(i)
        end do
        if(mod(nnn,2)==1) x_node(nnn/2+1)=x_node(nnn/2)*1.50_eb
        sum = 0.0_eb
        do i = 1, nnn
            sum = sum + x_node(i)
        end do
        do i = 1, nnn
            x_node(i) = x_node(i)/sum
        end do
        
        return
        
    end subroutine target_nodes

! --------------------------- get_target_factors -------------------------------------------

    subroutine get_target_factors2(iroom,itarg,target_factors_front,target_factors_back)
    
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
    
    integer :: nsolid_front_verts, nsolid_back_verts
    integer, parameter :: front=1, back=2
    integer :: i, iwall, ivert
    
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
    
    zlay = roomi%z0 + zzhlay(iroom,lower)
    room_verts(1:3,1)  = (/roomi%x0, roomi%y0, roomi%z1/)
    room_verts(1:3,2)  = (/roomi%x1, roomi%y0, roomi%z1/)
    room_verts(1:3,3)  = (/roomi%x0, roomi%y1, roomi%z1/)
    room_verts(1:3,4)  = (/roomi%x1, roomi%y1, roomi%z1/)
    room_verts(1:3,5)  = (/roomi%x0, roomi%y0, zlay/)
    room_verts(1:3,6)  = (/roomi%x1, roomi%y0, zlay/)
    room_verts(1:3,7)  = (/roomi%x0, roomi%y1, zlay/)
    room_verts(1:3,8)  = (/roomi%x1, roomi%y1, zlay/)
    room_verts(1:3,9)  = (/roomi%x0, roomi%y0, roomi%z0/)
    room_verts(1:3,10) = (/roomi%x1, roomi%y0, roomi%z0/)
    room_verts(1:3,11) = (/roomi%x0, roomi%y1, roomi%z0/)
    room_verts(1:3,12) = (/roomi%x1, roomi%y1, roomi%z0/)
    
! vert_distance = target_normal_xyz .dot. (vertex_xyz - target_origin_xyz)
    
    do ivert = 1, 12
       rel_room_vert(1:3) = room_verts(1:3,ivert) - targptr%center(1:3)
       vert_distance(ivert) = ddot(3,rel_room_vert,1,targptr%normal,1)
    end do
    
    target_factors_front(1:10)=0.0_eb
    target_factors_back(1:10)=0.0_eb
    do iwall=1, 10
       facei=>faces(1:5,iwall)
       nsolid_front_verts=0
       nsolid_back_verts=0
       do ivert = 1, 4
          d1 = vert_distance(facei(ivert))
          v1(1:3) => room_verts(1:3,facei(ivert))
          
          d2 = vert_distance(facei(ivert+1))
          v2(1:3) => room_verts(1:3,facei(ivert+1))
          
          if(d1.gt.0)then  ! face vertex is above target plane
             nsolid_front_verts=nsolid_front_verts+1
             solid_angle_front_verts(1:3,nsolid_front_verts) = v1(1:3)
          end if
          
          if(d1.lt.0)then  ! face vertex is below target plane
             nsolid_back_verts=nsolid_back_verts+1
             solid_angle_back_verts(1:3,nsolid_back_verts) = v1(1:3)
          end if
          if(d1*d2.lt.0)then ! two successive face vertices are on opposite sides of target plane
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
       if(nsolid_front_verts.ge.3)then

          ! triangulate polygon, compute solid angle for each triangle and sum
          ! the solid angle is zero whenever a vertex coincides with the target location

          v1 => solid_angle_front_verts(1:3,1)
          t1(1:3) = v1(1:3) - targptr%center(1:3)
          d1 = dnrm2(3,t1,1)
          
          if(d1.gt.0.0_eb)then
             t1(1:3) = t1(1:3)/d1
             do i = 2, nsolid_front_verts-1
                v2 => solid_angle_front_verts(1:3,i)
             
                t2(1:3) = v2(1:3) - targptr%center(1:3)
                d2 = dnrm2(3,t2,1)
                if(d2.eq.0.0_eb)cycle
             
                v3 => solid_angle_front_verts(1:3,i+1)
                t3(1:3) = v3(1:3) - targptr%center(1:3)
                d3 = dnrm2(3,t3,1)
                if(d3.eq.0.0_eb)cycle

                t2(1:3) = t2(1:3)/d2
                t3(1:3) = t3(1:3)/d3
             
                call solid_angle_triangle(solid_angle,t1,t2,t3)
                target_factors_front(iwall) = target_factors_front(iwall) + solid_angle
             end do
          end if
       end if
       if(nsolid_back_verts.ge.3)then

          ! triangulate polygon, compute solid angle for each triangle and sum
          ! the solid angle is zero whenever a vertex coincides with the target location

          v1 => solid_angle_back_verts(1:3,1)
          t1(1:3) = v1(1:3) - targptr%center(1:3)
          d1 = dnrm2(3,t1,1)
          
          if(d1.gt.0.0_eb)then
             t1(1:3) = t1(1:3)/d1
             do i = 2, nsolid_back_verts-1
                v2 => solid_angle_back_verts(1:3,i)
             
                t2(1:3) = v2(1:3) - targptr%center(1:3)
                d2 = dnrm2(3,t2,1)
                if(d2.eq.0.0_eb)cycle
                
                v3 => solid_angle_back_verts(1:3,i+1)
                t3(1:3) = v3(1:3) - targptr%center(1:3)
                d3 = dnrm2(3,t3,1)
                if(d3.eq.0.0_eb)cycle

                t2(1:3) = t2(1:3)/d2
                t3(1:3) = t3(1:3)/d3
             
                call solid_angle_triangle(solid_angle,t1,t2,t3)
                target_factors_back(iwall) = target_factors_back(iwall) + solid_angle
          end do
          end if
       end if
    end do
    sum_front = 0.0_eb
    sum_back = 0.0_eb
    do iwall=1, 10
       sum_front = sum_front + target_factors_front(iwall)
       sum_back = sum_back + target_factors_back(iwall)
    end do
    if(sum_front>0.0_eb)target_factors_front(1:10) = target_factors_front(1:10)/sum_front
    if(sum_back>0.0_eb)target_factors_back(1:10) = target_factors_back(1:10)/sum_back

 end subroutine get_target_factors2

    ! --------------------------- get_target_factors -------------------------------------------

    subroutine get_target_factors(iroom,itarg,target_factors_front,target_factors_back)
    
    integer, intent(in) :: iroom, itarg
    real(eb), intent(out), dimension(10) :: target_factors_front, target_factors_back

    integer :: iwall
    real(eb) :: awall_sum(2), svect(3)
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    integer, parameter :: front=1, back=2

    
    roomptr => roominfo(iroom)
    targptr => targetinfo(itarg)
    
    awall_sum(front) = 0.0_eb
    awall_sum(back) = 0.0_eb
    do iwall = 1, 10
       svect(1:3) = targptr%center(1:3) - roomptr%wall_center(1:3,iwall)
       if(ddot(3,svect,1,targptr%normal,1)<=0.0_eb)then
          awall_sum(front) = awall_sum(front) + zzwarea2(iroom,iwall)
       else
          awall_sum(back) = awall_sum(back) + zzwarea2(iroom,iwall)
       end if
    end do
    if(awall_sum(front).eq.0.0_eb)awall_sum(front)=1.0_eb
    if(awall_sum(back).eq.0.0_eb)awall_sum(back)=1.0_eb
    target_factors_front(1:10)=0.0_eb
    target_factors_back(1:10)=0.0_eb
    do iwall = 1, 10
       svect(1:3) = targptr%center(1:3) - roomptr%wall_center(1:3,iwall)
       if(ddot(3,svect,1,targptr%normal,1)<=0.0_eb)then
          target_factors_front(iwall) = zzwarea2(iroom,iwall)/awall_sum(front)
       else
          target_factors_back(iwall) = zzwarea2(iroom,iwall)/awall_sum(back)
       end if
    end do
    end subroutine get_target_factors
    
! --------------------------- getylyu -------------------------------------------

    subroutine getylyu (yo,y,yt,s,yl,yu)

    !     routine: gettylyu
    !     purpose: compute portion of path in lower and upper layers

    real(eb), intent(in) :: yo, y, yt, s
    real(eb), intent(out) :: yl, yu


    if(yo<=y)then
        if(yt<=y)then
            yl = 1.0_eb
        else
            yl = (y-yo)/(yt-yo)
        end if
    else
        if(yt>=y)then
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

    do itarg = 1, ntarg
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

    subroutine update_detectors (imode,tcur,dstep,ndtect,zzhlay,zztemp,xdtect,ixdtect,iquench,idset,ifdtect,tdtect)

    !     routine: update_detectors
    !     purpose: updates the temperature of each detector link.  it also determine whether the 
    !              detector has activated in the time interval (tcur,tcur+dstep).  if this has occured then a 
    !              quenching algorithm will be invoked if the appropriate option has been set.
    !     arguments: tcur    current time
    !                dstep   time step size (to next time)
    !                ndtect  number of detectors
    !                xdtect  2-d array containing floating point detector data structures 
    !                ixdtect 2-d array containing integer detector data structures
    !                iquench if the j=iquench(i) is non-zero then the j'th sprinkler in the i'th room is quenching the fire
    !                idset   room where activated detector resides

    integer, intent(in) :: imode, ndtect
    real(eb), intent(in) :: tcur, dstep, zzhlay(nr,2), zztemp(nr,2)
    
    integer, intent(out) :: idset, ifdtect, ixdtect(mxdtect,*), iquench(*)
    real(eb), intent(out) :: xdtect(mxdtect,*), tdtect
    
    real(eb) :: cjetmin, tlink, tlinko, zdetect, tlay, tjet, tjeto, vel, velo, rti, trig, an, bn, anp1, &
       bnp1, denom, fact1, fact2, delta, tmp
    integer :: i, iroom, idold, iqu
    character(133) :: messg

    idset = 0
    ifdtect = 0
    tdtect = tcur+2*dstep
    cjetmin = 0.10_eb
    do i = 1, ndtect
        iroom = ixdtect(i,droom)

        zdetect = xdtect(i,dzloc)
        if(zdetect>zzhlay(iroom,lower))then
            tlay = zztemp(iroom,upper)
        else
            tlay = zztemp(iroom,lower)
        end if

        tjet = max(xdtect(i,dtjet),tlay)
        tjeto = max(xdtect(i,dtjeto),tlay)
        vel = max(xdtect(i,dvel),cjetmin)
        velo = max(xdtect(i,dvelo),cjetmin)
        
        if (ixdtect(i,dtype)==smoked) then  
            trig = log10(1._eb/(1._eb-xdtect(i,dtrig)/100._eb))
            tlinko = xdtect(i,dcond)
            tlink = xdtect(i,dobs)        
            if (tcur>350._eb) then
                continue
            end if
        elseif (ixdtect(i,dtype)>=heatd) then
            rti = xdtect(i,drti)
            trig = xdtect(i,dtrig)
            tlinko = xdtect(i,dcond)
            bn = sqrt(velo)/rti
            an = bn*tjeto
            bnp1 = sqrt(vel)/rti
            anp1 = bnp1*tjet
            denom = 1.0_eb + dstep*bnp1*.5_eb
            fact1 = (1.0_eb - dstep*bn*.50_eb)/denom
            fact2 = dstep/denom
            tlink = fact1*tlinko + fact2*(an+anp1)*0.5_eb
        end if
        if (imode>0) then
            xdtect(i,dcondo) = tlinko
            xdtect(i,dcond) = tlink
        end if

        ! determine if detector has activated in this time interval (and not earlier)
        if(tlinko<trig.and.trig<=tlink.and.ixdtect(i,dact)==0)then
            delta = (trig-tlinko)/(tlink-tlinko)
            tmp = tcur+dstep*delta
            tdtect = min(tmp,tdtect)
            ifdtect = i
            if (imode>0) then
                xdtect(i,dtact)= tcur+dstep*delta
                ixdtect(i,dact) = 1
                ! tell the world about the activation
                if (ixdtect(i,dactreported)==0) then
                    ixdtect(i,dactreported) = 1
                    call device_activated (i, tdtect, 1)
                    write(messg,76) i, int(tdtect+0.5_eb), ixdtect(i,droom)
76                  format(' Sensor ',i3,' has activated at ',i0,' seconds in compartment ',i3)
                    call xerror(messg,0,1,-3)
                end if

                ! determine if this is the first detector to have activated in this room
                idold = iquench(iroom)
                iqu = 0
                if(idold==0)then
                    iqu = i
                else
                    if(xdtect(i,dtact)<xdtect(idold,dtact))then

                        ! this can only happen if two detectors have activated in the same room in the same 
                        ! (possibly very short) time interval
                        iqu = i
                    end if
                end if

                ! if this detector has activated before all others in this room and the quenching flag was turned on 
                !  then let the sprinkler quench the fire
                if(iqu/=0.and.ixdtect(i,dquench)==1)then
                    iquench(iroom)=iqu
                    idset = iroom
                end if
            end if
        end if
        xdtect(i,dtjeto) = tjet
        xdtect(i,dvelo) = vel
    end do
    return
    end subroutine update_detectors
      
    ! --------------------------- detector_temp_and_velocity -------------------------------------------

    subroutine get_detector_temp_and_velocity

    !     routine:     get_detector_temp_and_velocity

    !     description:  calculates near-detector gas temperature, velocity, and smoke obscuration

    real(eb) :: xloc, yloc, zloc, tg, vg(4),obs
    integer :: id, iroom

    ! If ceiling jet option is turned off, conditions default to the appropriate layer temperature
    do id = 1, ndtect
        iroom = ixdtect(id,droom)
        xloc = xdtect(id,dxloc)
        yloc = xdtect(id,dyloc)
        zloc = xdtect(id,dzloc)
        if (option(fcjet)==off) then
            ! if ceiling jet option is off, things default to appropriate layer temperature
            if(zloc>zzhlay(iroom,lower))then
                xdtect(id,dtjet) = zztemp(iroom,upper)
                xdtect(id,dobs) = toxict(iroom,upper,9)
            else
                xdtect(id,dtjet) = zztemp(iroom,lower)
                xdtect(id,dobs) = toxict(iroom,lower,9)
            end if
            xdtect(id,dvel) = 0.1_eb
        else
            ! if ceiling jet option is on, temeperature is determined by plume and ceiling jet algorithms
            call get_gas_temp_velocity(iroom,xloc,yloc,zloc,tg,vg)
            xdtect(id,dtjet) = tg
            xdtect(id,dvel) = vg(4)
            if(zloc>zzhlay(iroom,lower))then
                xdtect(id,dobs) = toxict(iroom,upper,9)
            else
                xdtect(id,dobs) = toxict(iroom,lower,9)
            end if
        end if
        obs = xdtect(id,dobs)
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