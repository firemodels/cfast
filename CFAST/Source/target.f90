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

    subroutine target(update,method,dt,xpsolve,delta)

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
    real(eb), intent(in) :: dt,  xpsolve(*)
    
    real(eb), intent(out) :: delta(*)

    logical :: first=.true.
    real(eb) :: tmp(nnodes_trg), walldx(nnodes_trg), tgrad(2), wk(1), wspec(1), wrho(1), tempin, tempout
    real(eb) :: tderv, ddtemp, ttold, ttnew, sum, wfluxin, wfluxout, wfluxavg, xl
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

    ! for each target calculate ode or pde residual and update target temperature (if update=1 or 1)
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
                    tempin = xxtarg(idxtempf_trg,itarg)
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

                    call conductive_flux (update,tempin,tempout,dt,wk,wspec,wrho,xxtarg(idxtempf_trg,itarg),walldx,nmnode,nslab,&
                       wfluxin,wfluxout,iwbound,tgrad,tderv)
                    if(iimeth==mplicit)then
                        ieq = iztarg(itarg)
                        delta(noftt+ieq) = xxtarg(trgnfluxf,itarg)+wk(1)*tgrad(1)
                    endif
                else if(iieq==cylpde)then
                    wfluxavg = (wfluxin+wfluxout)/2.0_eb
                    call cylindrical_conductive_flux (xxtarg(idxtempf_trg,itarg),nmnode(1),wfluxavg,&
                       dt,wk(1),wrho(1),wspec(1),xl)          
                endif

                ! compute the ode residual
            elseif(iieq==ode)then
                ddtemp = (wfluxin+wfluxout)/(wspec(1)*wrho(1)*xl)
                if(iimeth==mplicit)then
                    ieq = iztarg(itarg)
                    delta(noftt+ieq) = ddtemp - xpsolve(noftt+ieq) 
                elseif(iimeth==xplicit)then
                    if(update/=0)then
                        ttold = xxtarg(idxtempf_trg,itarg)
                        ttnew = ttold + dt*ddtemp
                        xxtarg(idxtempf_trg,itarg) = ttnew
                    endif
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
    !     arguments: method  


    integer, intent(in) :: method
    
    real(eb) :: flux(2), dflux(2), ttarg(2), ddif
    integer :: itarg, methtarg, iroom, niter, iter

    ! calculate flux to user specified targets, assuming target is at thermal equilibrium
    do itarg = 1, ntarg
        methtarg = ixtarg(trgmeth,itarg)
        if(method==methtarg) then
            iroom = ixtarg(trgroom,itarg)
            if(methtarg==steady)then
                niter = 10
            else
                niter = 1
            endif
            ttarg(1) = xxtarg(idxtempf_trg,itarg)
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
                xxtarg(idxtempf_trg,itarg) = ttarg(1)
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
    
    real(eb) :: svect(3), qwtsum(2), awallsum(2), qgassum(2), absu, absl, cosang, cosangt, s, dnrm2, ddot, zfire, &
        xtarg, ytarg, ztarg, zlay, zl, zu, taul, tauu, qfire, absorb, qft, qout, zwall, tl, tu, alphal, alphau,&
       awall, qwt, qgas, qgt, zznorm, tg, tgb, vg(4), &
        ttargb, dttarg, dttargb, temis, q1, q2, q1b, q2b, q1g, dqdtarg, dqdtargb, total_radiation, re_radiation
    integer :: map10(10), iroom, i, nfirerm, istart, ifire, iwall, jj, iw, iwb, irtarg
    
    type(room_type), pointer :: roomptr

    data map10/1,3,3,3,3,4,4,4,4,2/

    absu = 0.50_eb
    absl = 0.01_eb
    iroom = ixtarg(trgroom,itarg)
    roomptr => roominfo(iroom)
    
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

        do i = 1, 2
            awallsum(i) = 0.0_eb   
            qwtsum(i) = 0.0_eb
            qgassum(i) = 0.0_eb
        end do
        do iwall = 1, 10
            if(nfurn>0)then
                qout=qfurnout
            else
                qout = rdqout(map10(iwall),iroom)
            endif
            svect(1) = xxtarg(trgcenx,itarg) - roomptr%wall_center(iwall,1)
            svect(2) = xxtarg(trgceny,itarg) - roomptr%wall_center(iwall,2)
            svect(3) = xxtarg(trgcenz,itarg) - roomptr%wall_center(iwall,3)
            cosangt = 0.0_eb
            s = dnrm2(3,svect,1)
            if(s/=0.0_eb)then
                cosangt = -ddot(3,svect,1,xxtarg(trgnormx,itarg),1)/s
            endif
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

            awall = zzwarea2(iroom,iwall)
            qwt = qout*taul*tauu
            if(iwall<=5)then
                qgas = tl**4*alphal*tauu + tu**4*alphau
            else
                qgas = tu**4*alphau*taul + tl**4*alphal
            endif
            qgt = sigma*qgas
            if(cosangt>=0.0_eb)then
                jj = 1
            else 
                jj = 2
            endif

            ! calculate flux on the target front.  calculate flux on the target back only if the rear of 
            ! the target is interior to the room.
            if(jj==1.or.ixtarg(trgback,itarg)==interior)then
                qwtsum(jj) = qwtsum(jj) + qwt*awall
                qgassum(jj) = qgassum(jj) + qgt*awall
                awallsum(jj) = awallsum(jj) + awall
            endif
        end do
        do i = 1, 2
            if(awallsum(i)==0.0_eb)awallsum(i) = 1.0_eb
            qtwflux(itarg,i) = qwtsum(i)/awallsum(i)
            qtgflux(itarg,i) = qgassum(i)/awallsum(i)
        end do       

        ! if the target rear was exterior then calculate the flux assuming ambient outside conditions
        if(ixtarg(trgback,itarg)==exterior.or.qtgflux(itarg,2)==0.0)then
            qtgflux(itarg,2) = sigma*interior_temperature**4
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
    irtarg = ixtarg(trgroom,itarg)
    xtarg = xxtarg(trgcenx,itarg)
    ytarg = xxtarg(trgceny,itarg)
    ztarg = xxtarg(trgcenz,itarg)
    call get_gas_temp_velocity(irtarg,xtarg,ytarg,ztarg,tg,vg)
    tgtarg(itarg) = tg
    if(ixtarg(trgback,itarg)==interior)then
        tgb = tg
    else
        tgb = interior_temperature
    endif
    ttargb = ttarg(2)
    dttarg = 1.0e-7_eb*ttarg(1)
    dttargb = 1.0e-7_eb*ttarg(2)

    temis = xxtarg(trgemis,itarg)

    ! convection for the front
    call convective_flux (iw,tg,ttarg(1),q1)
    call convective_flux (iw,tg,ttarg(1)+dttarg,q2)
    qtcflux(itarg,1) = q1
    dqdtarg = (q2-q1)/dttarg

    flux(1) = temis*(qtfflux(itarg,1) + qtwflux(itarg,1) + qtgflux(itarg,1)) + qtcflux(itarg,1) - temis*sigma*ttarg(1)**4
    dflux(1) = -4.0_eb*temis*sigma*ttarg(1)**3 + dqdtarg

    ! this is for "gauge" heat flux output ... it assumes an ambient temperature target
    gtflux(itarg,t_ftotal) = qtfflux(itarg,1)
    gtflux(itarg,t_wtotal) = qtwflux(itarg,1)
    gtflux(itarg,t_gtotal) = qtgflux(itarg,1)
    
    ! Adjust each one for the ambient losses
    total_radiation = gtflux(itarg,t_ftotal) + gtflux(itarg,t_wtotal) + gtflux(itarg,t_gtotal)
    re_radiation = sigma*interior_temperature**4
    gtflux(itarg,t_ftotal) = gtflux(itarg,t_ftotal) - re_radiation*gtflux(itarg,t_ftotal)/total_radiation
    gtflux(itarg,t_wtotal) = gtflux(itarg,t_wtotal) - re_radiation*gtflux(itarg,t_wtotal)/total_radiation
    gtflux(itarg,t_gtotal) = gtflux(itarg,t_gtotal) - re_radiation*gtflux(itarg,t_gtotal)/total_radiation
    
    !add in the convection 
    call convective_flux (iw,tg,interior_temperature,q1g)
    gtflux(itarg,t_ctotal) = q1g
    
    ! and the total is just the sum of these
    gtflux(itarg,t_total) = gtflux(itarg,t_ftotal) + gtflux(itarg,t_wtotal) + gtflux(itarg,t_gtotal) + gtflux(itarg,t_ctotal)


    ! convection for the back
    call convective_flux(iwb,tgb,ttargb,q1b)
    call convective_flux(iwb,tgb,ttargb+dttargb,q2b)
    qtcflux(itarg,2) = q1b
    dqdtargb = (q2b-q1b)/dttargb

    flux(2) = temis*(qtfflux(itarg,2) + qtwflux(itarg,2) + qtgflux(itarg,2)) + qtcflux(itarg,2) - temis*sigma*ttargb**4
    dflux(2) = -4.0_eb*temis*sigma*ttargb**3 + dqdtargb

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