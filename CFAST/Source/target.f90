module target_routines

    use precision_parameters
    use cenviro
    use cfast_main
    use fltarget
    use cparams
    use dsize
    use conduction_routines
    use convection_routines
    use fireptrs
    use targptrs
    use objects2
    use wnodes
    use opt
    implicit none

    
private

public target, target_flux, update_detectors, detector_temp_and_velocity

contains

! --------------------------- target -------------------------------------------

    subroutine target(update,method,dt,delta)

    !     routine: target (main target routine)
    !     purpose: compute dassl residuals associated with targets
    !     arguments: update   variable indicating whether temperature profile should be updated
    !                method   one of steady, mplicit or xplicit (note: these are parameter values, not mis-pelld)
    !                dt       time step
    !                xpsolve  dassl derivative estimate
    !                delta    dassl residual array
    !     revision: $revision: 464 $
    !     revision date: $date: 2012-06-29 15:41:23 -0400 (fri, 29 jun 2012) $

    integer, intent(in) :: update, method
    real(eb), intent(in) :: dt
    
    real(eb), intent(out) :: delta(*)

    logical :: first=.true.
    real(eb) :: tmp(nnodes_trg), walldx(nnodes_trg), tgrad(2), wk(1), wspec(1), wrho(1), tempin, tempout
    real(eb) :: tderv, sum, wfluxin, wfluxout, wfluxavg, xl
    integer :: nnn, i, itarg, nmnode(2), ieq, iieq, iwbound, nslab, iimeth
    save first,tmp

    if(method==steady)return

    ! initialize non-dimensional target node locations the first time target is called
    if(first)then
        first = .false.
        nnn = nnodes_trg - 1
        tmp(1) = 1.0_eb
        tmp(nnn) = 1.0_eb
        do i = 2, nnn/2 
            tmp(i) = tmp(i-1)*1.50_eb
            tmp(nnn+1-i) = tmp(i)
        end do
        if(mod(nnn,2)==1)tmp(nnn/2+1)=tmp(nnn/2)*1.50_eb
        sum = 0.0_eb
        do i = 1, nnn
            sum = sum + tmp(i)
        end do
        do i = 1, nnn
            tmp(i) = tmp(i)/sum
        end do
    endif

    ! calculate net flux striking each side of target
    call target_flux(method)

    ! for each target calculate the residual and update target temperature (if update = 1)
    do itarg = 1, ntarg
        if(ixtarg(trgmeth,itarg)==method) then
            wfluxin = xxtarg(trgnfluxf,itarg)
            wfluxout = xxtarg(trgnfluxb,itarg)
            wspec(1) = xxtarg(trgcp,itarg)
            wrho(1) =  xxtarg(trgrho,itarg)
            wk(1) =  xxtarg(trgk,itarg)
            xl = xxtarg(trgl,itarg)
            iimeth = ixtarg(trgmeth,itarg)
            iieq = ixtarg(trgeq,itarg)

            ! compute the pde residual 
            if(iieq==pde.or.iieq==cylpde)then
                if(iimeth==mplicit)then
                    tempin = xxtarg(idx_tempf_trg,itarg)
                    iwbound = 3
                else
                    iwbound = 4
                endif
                nmnode(1) = nnodes_trg
                nmnode(2) = nnodes_trg - 2
                nslab = 1
                if(iieq==pde)then
                    do i = 1, nnodes_trg - 1
                        walldx(i) = xl*tmp(i)
                    end do

                    call conductive_flux (update,tempin,tempout,dt,wk,wspec,wrho,xxtarg(idx_tempf_trg,itarg),walldx,nmnode,nslab,&
                       wfluxin,wfluxout,iwbound,tgrad,tderv)
                    if(iimeth==mplicit)then
                        ieq = iztarg(itarg)
                        delta(noftt+ieq) = xxtarg(trgnfluxf,itarg)+wk(1)*tgrad(1)
                    endif
                else if(iieq==cylpde)then
                !  wfluxout is incorrect
                !    wfluxavg = (wfluxin+wfluxout)/2.0_eb
                    wfluxavg = wfluxin
                    call cylindrical_conductive_flux (xxtarg(idx_tempf_trg,itarg),nmnode(1),wfluxavg,&
                       dt,wk(1),wrho(1),wspec(1),xl)          
                endif
                ! error, the equation type can has to be either pde or ode if the method is not steady
            else

            endif
        end if
    end do
    return
    end subroutine target
    
! --------------------------- target -------------------------------------------

    subroutine target_flux(method)

    !     routine: target
    !     purpose: routine to calculate total flux striking a target. this flux is used to calculate a target temperature,
    !              assuming that the sum of incoming and outgoing flux is zero, ie, assuming that the target is at steady state.
    !     arguments: method  (steady or pde


    integer, intent(in) :: method
    
    real(eb) :: flux(2), dflux(2), ttarg(2), ddif
    integer :: itarg, methtarg, iroom, niter, iter
        
    type(target_type), pointer :: targptr

    ! calculate flux to user specified targets, assuming target is at thermal equilibrium
    do itarg = 1, ntarg
        targptr => targetinfo(itarg)
        methtarg = ixtarg(trgmeth,itarg)
        if(method==methtarg) then
            iroom = targptr%room
            if(methtarg==steady)then
                niter = 10
            else
                niter = 1
            endif
            ttarg(1) = xxtarg(idx_tempf_trg,itarg)
            ttarg(2) = xxtarg(idx_tempb_trg,itarg)
            do iter = 1, niter
                call targflux(iter,itarg,ttarg,flux,dflux)
                if(dflux(1)/=0.0_eb.and.methtarg==steady)then
                    ddif = flux(1)/dflux(1)
                    ttarg(1) = ttarg(1) - ddif
                    if(abs(ddif)<=1.0e-5_eb*ttarg(1)) exit
                endif
            end do
            if(methtarg==steady)then
                xxtarg(idx_tempf_trg,itarg) = ttarg(1)
                xxtarg(idx_tempb_trg,itarg) = ttarg(2)
            endif
            xxtarg(trgtfluxf,itarg) = qtwflux(itarg,1) + qtfflux(itarg,1) + qtcflux(itarg,1) + qtgflux(itarg,1)
            xxtarg(trgtfluxb,itarg) = qtwflux(itarg,2) + qtfflux(itarg,2) + qtcflux(itarg,2) + qtgflux(itarg,2)
            call targflux(niter+1,itarg,ttarg,flux,dflux)
            xxtarg(trgnfluxf,itarg) = flux(1)
            xxtarg(trgnfluxb,itarg) = flux(2)
        end if
    end do

    return
    end subroutine target_flux
    
    subroutine cross_product(c,a,b)

! c = a x b

    real(eb), intent(in) :: a(3),b(3)
    real(eb), intent(out) :: c(3)

    c(1) = a(2)*b(3)-a(3)*b(2)
    c(2) = a(3)*b(1)-a(1)*b(3)
    c(3) = a(1)*b(2)-a(2)*b(1)

    end subroutine cross_product

! --------------------------- solid_angle_triangle -------------------------------------------

    subroutine solid_angle_triangle(solid_angle,v1,v2,v3)
    real(eb), intent(in), dimension(3) :: v1, v2, v3
    real(eb), intent(out) :: solid_angle
    
    real(eb) :: vcross(3), num, denom, ddot
    ! assuming v1, v2 and v3 are unit vectors
    ! tan(solid_angle/2) = (v1 x v2 . v3)/(1 + v2.v3 + v3.v1 + v1.v2)
    
    call cross_product(vcross,v1,v2)
    num = ddot(3,vcross,1,v3,1)
    denom = 1.0_eb + ddot(3,v2,1,v3,1) + ddot(3,v3,1,v1,1) + ddot(3,v1,1,v2,1)
    solid_angle = 2.0_eb*atan(num/denom)
    
    end subroutine solid_angle_triangle

! --------------------------- get_target_factors -------------------------------------------

    subroutine get_target_factors2(iroom,itarg,target_factors_front,target_factors_back)
    integer, intent(in) :: iroom, itarg
    real(eb), intent(out), dimension(10) :: target_factors_front, target_factors_back

    integer :: iwall, ivert
    real(eb) :: rel_room_vert(3)
    real(eb) :: dnrm2, ddot
    type(room_type), pointer :: roomi
    integer, parameter :: front=1, back=2
    integer :: i
    real(eb), dimension(12) :: vert_distance
    
    real(eb) :: ylay
    real(eb), target :: room_verts(3,12)
    real(eb), target :: solid_angle_verts(3,8)
    real(eb) :: factor
    integer :: nsolid_verts
    real(eb) :: d1, d2
    real(eb), pointer, dimension(:) :: v1, v2, v3
    real(eb) :: t1(3), t2(3), t3(3)
    real(eb) :: sum_front, sum_back, solid_angle
    
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
 !  1: 1  2  4  3 ceiling
 !  2: 1  5  6  2 upper front
 !  3: 2  6  8  4 upper right
 !  4: 3  4  8  7 upper back
 !  5: 1  3  7  5 upper left
 !  6: 5  9 10  6 lower front
 !  7: 6 10 12  8 lower right
 !  8: 7  8 12 11 lower back
 !  9: 5  7 11  9 lower left
 ! 10: 9 11 12 10 floor

    data ((faces(i,iwall),i=1,5),iwall=1,10) /& ! for convenience repeat first and last vertex
    1,  2,  4,  3, 1,    1,  5,  6,  2, 1, &
    2,  6,  8,  4, 2,    3,  4,  8,  7, 3, &
    1,  3,  7,  5, 1,    5,  9, 10,  6, 5, &
    6, 10, 12,  8, 6,    7,  8, 12, 11, 7, &
    5,  7, 11,  9, 5,    9, 11, 12, 10, 9  &
    /

    roomi => roominfo(iroom)

    !define vertex locations
    
    ylay = roomi%z0 + zzhlay(iroom,lower)
    room_verts(1:3,1)  = (/roomi%x0, roomi%y0,roomi%z1/)
    room_verts(1:3,2)  = (/roomi%x1, roomi%y0,roomi%z1/)
    room_verts(1:3,3)  = (/roomi%x0, roomi%y1,roomi%z1/)
    room_verts(1:3,4)  = (/roomi%x1, roomi%y1,roomi%z1/)
    room_verts(1:3,5)  = (/roomi%x0, roomi%y0,ylay/)
    room_verts(1:3,6)  = (/roomi%x1, roomi%y0,ylay/)
    room_verts(1:3,7)  = (/roomi%x0, roomi%y1,ylay/)
    room_verts(1:3,8)  = (/roomi%x1, roomi%y1,ylay/)
    room_verts(1:3,9)  = (/roomi%x0, roomi%y0,roomi%z0/)
    room_verts(1:3,10) = (/roomi%x1, roomi%y0,roomi%z0/)
    room_verts(1:3,11) = (/roomi%x0, roomi%y1,roomi%z0/)
    room_verts(1:3,12) = (/roomi%x1, roomi%y1,roomi%z0/)
    
! vert_distance = target_normal_xyz .dot. (vertex_xyz - target_origin_xyz)
    
    do ivert = 1, 12
       rel_room_vert(1:3) = room_verts(1:3,ivert) - xxtarg(trgcenx:trgcenx+2,itarg)  ! check for sign
       vert_distance(ivert) = ddot(3,rel_room_vert,1,xxtarg(trgnormx,itarg),1)
    end do
    
    target_factors_front(1:10)=0.0_eb
    target_factors_back(1:10)=0.0_eb
    do iwall=1, 10
       facei=>faces(1:5,iwall)
       nsolid_verts=0
       do ivert = 1, 4
          d1 = vert_distance(facei(ivert))
          d2 = vert_distance(facei(ivert+1))
          v1(1:3) => room_verts(1:3,facei(ivert))
          v2(1:3) => room_verts(1:3,facei(ivert+1))
          if(d1.ge.0)then  ! face vertex is above target plane
             nsolid_verts=nsolid_verts+1
             solid_angle_verts(1:3,nsolid_verts) = v1(1:3)
          endif
          if(d1*d2.lt.0)then ! two successive face vertices are opposite sides of target plane
                             ! interpolate to find vertex that lies on target plane
             !  d1   0    d2
             !  v1   v0   v2    
             !
             !  (v0-v1)/(0-d1) = (v2-v1)/(d2-d1)
             !  solve for v0
             ! v0 = v1*(1-(-d1)/(d2-d1)) + v2*(-d1/(d2-d1))
             ! note: d2-d1 can't be 0 since d1*d2<0.0
             nsolid_verts=nsolid_verts+1

             factor = -d1/(d2-d1)
             solid_angle_verts(1:3,nsolid_verts) = (1.0_eb-factor)*v1 + factor*v2
          endif
       end do
       if(nsolid_verts.gt.3)then
          ! triangulate polygon, compute solid angle for each triangle and sum
          v1 => solid_angle_verts(1:3,1)
          t1(1:3) = v1(1:3) - xxtarg(trgcenx:trgcenx+2,itarg)
          t1(1:3) = t1(1:3)/dnrm2(3,t1,1)
          do i = 2, nsolid_verts-1
             v2 => solid_angle_verts(1:3,i)
             v3 => solid_angle_verts(1:3,i+1)
             t2(1:3) = v2(1:3) - xxtarg(trgcenx:trgcenx+2,itarg)
             t2(1:3) = t2(1:3)/dnrm2(3,t2,1)
             t3(1:3) = v3(1:3) - xxtarg(trgcenx:trgcenx+2,itarg)
             t3(1:3) = t3(1:3)/dnrm2(3,t3,1)
             call solid_angle_triangle(solid_angle,t1,t2,t3)
             target_factors_front(iwall) = target_factors_front(iwall) + solid_angle
          end do
       endif
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
    real(eb) :: awall_sum(2), svect(3), ddot
    type(room_type), pointer :: roomptr
    integer, parameter :: front=1, back=2

    
    roomptr => roominfo(iroom)
    awall_sum(front) = 0.0_eb
    awall_sum(back) = 0.0_eb
    do iwall = 1, 10
       svect(1) = xxtarg(trgcenx,itarg) - roomptr%wall_center(iwall,1)
       svect(2) = xxtarg(trgceny,itarg) - roomptr%wall_center(iwall,2)
       svect(3) = xxtarg(trgcenz,itarg) - roomptr%wall_center(iwall,3)
       if(ddot(3,svect,1,xxtarg(trgnormx,itarg),1)<=0.0_eb)then
          awall_sum(front) = awall_sum(front) + zzwarea2(iroom,iwall)
       else
          awall_sum(back) = awall_sum(back) + zzwarea2(iroom,iwall)
       endif
    end do
    if(awall_sum(front).eq.0.0_eb)awall_sum(front)=1.0_eb
    if(awall_sum(back).eq.0.0_eb)awall_sum(back)=1.0_eb
    target_factors_front(1:10)=0.0_eb
    target_factors_back(1:10)=0.0_eb
    do iwall = 1, 10
       svect(1) = xxtarg(trgcenx,itarg) - roomptr%wall_center(iwall,1)
       svect(2) = xxtarg(trgceny,itarg) - roomptr%wall_center(iwall,2)
       svect(3) = xxtarg(trgcenz,itarg) - roomptr%wall_center(iwall,3)
       if(ddot(3,svect,1,xxtarg(trgnormx,itarg),1)<=0.0_eb)then
          target_factors_front(iwall) = zzwarea2(iroom,iwall)/awall_sum(front)
       else
          target_factors_back(iwall) = zzwarea2(iroom,iwall)/awall_sum(back)
       endif
    end do
    end subroutine get_target_factors
    
! --------------------------- targflux -------------------------------------------

    subroutine targflux(iter,itarg,ttarg,flux,dflux)

    !     routine: target
    !     purpose: routine to calculate flux (and later, temperature) of a target.
    !     arguments: iter   iteration number
    !                itarg  targetnumber
    !                ttarg  front and back target input temperature
    !                flux   front and back output flux
    !                dflux  front and back output flux derivative

    integer, intent(in) :: iter, itarg
    real(eb), intent(in) :: ttarg(2)

    real(eb) :: flux(2), dflux(2)
    
    real(eb) :: svect(3), qwtsum(2), qgassum(2), absu, absl, cosang, s, dnrm2, ddot, zfire
    real(eb) :: xtarg, ytarg, ztarg, zlay, zl, zu, taul, tauu, qfire, absorb, qft, qout, zwall, tl, tu, alphal, alphau
    real(eb) :: qwt, qgas, qgt, zznorm, tg, tgb, vg(4)
    real(eb) :: ttargb, dttarg, dttargb, temis, q1, q2, q1b, q2b, q1g, dqdtarg, dqdtargb
    real(eb) :: target_factors_front(10), target_factors_back(10)
    integer :: map10(10), iroom, i, nfirerm, istart, ifire, iwall, jj, iw, iwb
    integer, parameter :: front=1, back=2
    
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr

    data map10/1,3,3,3,3,4,4,4,4,2/

    absu = 0.50_eb
    absl = 0.01_eb
    roomptr => roominfo(iroom)
    targptr => targetinfo(itarg)
    
    iroom = targptr%room
    
    ! terms that do not depend upon the target temperature only need to be calculated once
    if(iter==1)then

        ! initialize flux counters: total, fire, wall, gas 
        do i = 1, 2
            qtfflux(itarg,i) = 0.0_eb
            qtgflux(itarg,i) = 0.0_eb
            qtwflux(itarg,i) = 0.0_eb
        end do

        nfirerm = ifrpnt(iroom,1)
        istart = ifrpnt(iroom,2)

        ! compute radiative flux from fire
        do ifire = istart, istart + nfirerm - 1
            svect(1) = xxtarg(trgcenx,itarg) - xfire(ifire,f_fire_xpos)
            svect(2) = xxtarg(trgceny,itarg) - xfire(ifire,f_fire_ypos)
            svect(3) = xxtarg(trgcenz,itarg) - xfire(ifire,f_fire_zpos)! This is point radiation at the base of the fire
            ! This is fire radiation at the center height of the fire (bounded by the ceiling height)
            !call flame_height (xfire(ifire,f_qfr),xfire(ifire,f_obj_area),fheight)
            !if(fheight+xfire(ifire,f_fire_zpos)>room_height(i))then
            !    svect(3) = xfire(ifire,f_fire_zpos) + (room_height(i)-xfire(ifire,f_fire_zpos))/2.0_eb
            !else
            !    svect(3) = xfire(ifire,f_fire_zpos) + fheight/2.0_eb
            !end if
            cosang = 0.0_eb
            s = max(dnrm2(3,svect,1),objclen(ifire))
            if(s/=0.0_eb)then
                cosang = -ddot(3,svect,1,xxtarg(trgnormx,itarg),1)/s
            endif
            zfire = xfire(ifire,f_fire_zpos)
            ztarg = xxtarg(trgcenz,itarg)
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
            endif
            if(s/=0.0_eb)then
                qft = qfire*abs(cosang)*tauu*taul/(4.0_eb*pi*s**2)
            else
                qft = 0.0_eb
            endif

            ! decide whether flux is hitting front or back of target. if it's hitting the back target only add contribution
            ! if the target is interior to the room
            if(cosang>=0.0_eb)then
                qtfflux(itarg,1) = qtfflux(itarg,1) + qft
            else
                if(ixtarg(trgback,itarg)==interior)then
                    qtfflux(itarg,2) = qtfflux(itarg,2) + qft
                endif
            endif

        end do

        ! compute radiative flux from walls and gas

        qwtsum(front) = 0.0_eb
        qgassum(front) = 0.0_eb
        qwtsum(back) = 0.0_eb
        qgassum(back) = 0.0_eb
        call get_target_factors(iroom,itarg,target_factors_front,target_factors_back)
        do iwall = 1, 10
            if(nfurn>0)then
                qout=qfurnout
            else
                qout = rdqout(map10(iwall),iroom)
            endif
            svect(1) = xxtarg(trgcenx,itarg) - roomptr%wall_center(iwall,1)
            svect(2) = xxtarg(trgceny,itarg) - roomptr%wall_center(iwall,2)
            svect(3) = xxtarg(trgcenz,itarg) - roomptr%wall_center(iwall,3)
            s = dnrm2(3,svect,1)
            zwall = roomptr%wall_center(iwall,3)
            ztarg = xxtarg(trgcenz,itarg)
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
            endif
            qgt = sigma*qgas
            if(ddot(3,svect,1,xxtarg(trgnormx,itarg),1)<=0.0_eb)then
                jj = front
            else 
                jj = back
            endif

            ! calculate flux on the target front.  calculate flux on the target back only if the rear of 
            ! the target is interior to the room.
            if(jj==front.or.ixtarg(trgback,itarg)==interior)then
                if(jj==front)then
                  qwtsum(jj) = qwtsum(jj) + qwt*target_factors_front(iwall)
                  qgassum(jj) = qgassum(jj) + qgt*target_factors_front(iwall)
                else
                  qwtsum(jj) = qwtsum(jj) + qwt*target_factors_back(iwall)
                  qgassum(jj) = qgassum(jj) + qgt*target_factors_back(iwall)
                endif
            endif
        end do
        qtwflux(itarg,front) = qwtsum(front)
        qtgflux(itarg,front) = qgassum(front)
        qtwflux(itarg,back) = qwtsum(back)
        qtgflux(itarg,back) = qgassum(back)

        ! if the target rear was exterior then calculate the flux assuming ambient outside conditions
        if(ixtarg(trgback,itarg)==exterior.or.qtgflux(itarg,back)==0.0)then
            qtgflux(itarg,back) = sigma*interior_temperature**4
        endif
    endif

    ! compute convective flux
    ! assume target is a 'floor', 'ceiling' or 'wall' depending on how much the target is tilted.  
    zznorm = xxtarg(trgnormz,itarg)
    if(zznorm<=1.0_eb.and.zznorm>=cos45)then
        iw = 2
        iwb = 1
    elseif(zznorm>=-1.0_eb.and.zznorm<=-cos45)then
        iw = 1
        iwb = 2
    else
        iw = 3
        iwb = 3
    endif

    xtarg = xxtarg(trgcenx,itarg)
    ytarg = xxtarg(trgceny,itarg)
    ztarg = xxtarg(trgcenz,itarg)
    call get_gas_temp_velocity(iroom,xtarg,ytarg,ztarg,tg,vg)
    tgtarg(itarg) = tg
    if(ixtarg(trgback,itarg)==interior)then
        tgb = tg
    else
        tgb = interior_temperature
    endif
    ttargb = ttarg(back)
    dttarg = 1.0e-7_eb*ttarg(front)
    dttargb = 1.0e-7_eb*ttarg(back)

    temis = xxtarg(trgemis,itarg)

    ! convection for the front
    call convective_flux (iw,tg,ttarg(front),q1)
    call convective_flux (iw,tg,ttarg(front)+dttarg,q2)
    qtcflux(itarg,1) = q1
    dqdtarg = (q2-q1)/dttarg

    flux(front) = temis*(qtfflux(itarg,front) + qtwflux(itarg,front) + qtgflux(itarg,front)) + &
                  qtcflux(itarg,front) - temis*sigma*ttarg(front)**4
    dflux(front) = -4.0_eb*temis*sigma*ttarg(front)**3 + dqdtarg
    
    ! convection for the back
    call convective_flux(iwb,tgb,ttargb,q1b)
    call convective_flux(iwb,tgb,ttargb+dttargb,q2b)
    qtcflux(itarg,back) = q1b
    dqdtargb = (q2b-q1b)/dttargb

    flux(back) = temis*(qtfflux(itarg,back) + qtwflux(itarg,back) + qtgflux(itarg,back)) + &
                 qtcflux(itarg,back) - temis*sigma*ttargb**4
    dflux(back) = -4.0_eb*temis*sigma*ttargb**3 + dqdtargb

    ! store fluxes for printout
    do i = front,back
        targptr%flux_fire(i) = temis*qtfflux(itarg,i)
        targptr%flux_gas(i) = temis*qtgflux(itarg,i)
        targptr%flux_surface(i) = temis*qtwflux(itarg,i)
        targptr%flux_convection(i) = qtcflux(itarg,i)
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
    end subroutine targflux
    
! --------------------------- getylyu -------------------------------------------

    subroutine getylyu(yo,y,yt,s,yl,yu)

    !     routine: gettylyu
    !     purpose: compute portion of path in lower and upper layers

    real(eb), intent(in) :: yo, y, yt, s
    real(eb), intent(out) :: yl, yu


    if(yo<=y)then
        if(yt<=y)then
            yl = 1.0_eb
        else
            yl = (y-yo)/(yt-yo)
        endif
    else
        if(yt>=y)then
            yl = 0.0_eb
        else
            yl = (y-yt)/(yo-yt)
        endif
    endif
    yl = yl*s
    yu = s - yl
    return
    end subroutine getylyu
    
! --------------------------- update_detectors -------------------------------------------

    subroutine update_detectors(imode,tcur,dstep,ndtect,zzhlay,zztemp,xdtect,ixdtect,iquench,idset,ifdtect,tdtect)

    !     routine: gettylyu
    !     purpose: this routine updates the temperature of each detector link.  it also determine whether the 
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
        tlinko = xdtect(i,dtemp)

        zdetect = xdtect(i,dzloc)
        if(zdetect>zzhlay(iroom,lower))then
            tlay = zztemp(iroom,upper)
        else
            tlay = zztemp(iroom,lower)
        endif

        tjet = max(xdtect(i,dtjet),tlay)
        tjeto = max(xdtect(i,dtjeto),tlay)
        vel = max(xdtect(i,dvel),cjetmin)
        velo = max(xdtect(i,dvelo),cjetmin)

        rti = xdtect(i,drti)
        trig = xdtect(i,dtrig)
        if(ixdtect(i,dtype)==smoked)then
            tlink = tjet
        elseif(ixdtect(i,dtype)==heatd)then
            bn = sqrt(velo)/rti
            an = bn*tjeto
            bnp1 = sqrt(vel)/rti
            anp1 = bnp1*tjet
            denom = 1.0_eb + dstep*bnp1*.5_eb
            fact1 = (1.0_eb - dstep*bn*.50_eb)/denom
            fact2 = dstep/denom
            tlink = fact1*tlinko + fact2*(an+anp1)*0.5_eb
        else

            ! when soot is calculated then set tlink to soot concentration. set it to zero for now.
            tlink = 0.0_eb
        endif
        if (imode>0) then
            xdtect(i,dtempo) = tlinko
            xdtect(i,dtemp) = tlink
        endif

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
                    call smv_device_activated (i, tdtect, 1)
                    write(messg,76) i, tdtect, ixdtect(i,droom)
76                  format(' Sensor ',i3,' has activated at ',f6.1,' seconds in compartment ',i3)
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
                    endif
                endif

                ! if this detector has activated before all others in this room and the quenching flag was turned on 
                !  then let the sprinkler quench the fire
                if(iqu/=0.and.ixdtect(i,dquench)==1)then
                    iquench(iroom)=iqu
                    idset = iroom
                endif
            endif
        endif
        xdtect(i,dtjeto) = tjet
        xdtect(i,dvelo) = vel
    end do
    return
    end subroutine update_detectors
      
    ! --------------------------- detector_temp_and_velocity -------------------------------------------

    subroutine detector_temp_and_velocity

    !     routine:     detector_temp_and_velocity

    !     description:  calculates near-detector gas temperature and velocity

    real(eb) :: xloc, yloc, zloc, tg, vg(4)
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
            else
                xdtect(id,dtjet) = zztemp(iroom,lower)
            endif
            xdtect(id,dvel) = 0.1_eb
        else
            ! if ceiling jet option is on, temeperature is determined by plume and ceiling jet algorithms
            call get_gas_temp_velocity(iroom,xloc,yloc,zloc,tg,vg)
            xdtect(id,dtjet) = tg
            xdtect(id,dvel) = vg(4)
        end if
    end do

    return
    
    end subroutine detector_temp_and_velocity

end module target_routines    