    subroutine convection (flwcv,flxcv)

    !     routine:    convection
    !     function:   interface between calculate_residuals and convec.  loops over rooms
    !                 setting up varibles.  passes to convec if ceiling jet for
    !                 a surface is off, otherwise sets flxcv to 0.0 and then
    !                 solves for flwcv
    !     outputs:    flwcv       net enthalphy into each layer 
    !                 flxcv       net heat flux onto surface

    use precision_parameters
    use fireptrs
    use wallptrs
    use cparams
    use cenviro
    use cfast_main
    use opt
    use wnodes
    implicit none

    real(eb), intent(out) :: flwcv(nr,2), flxcv(nr,nwal)
    real(eb) :: flwcv0(nr,2), flxcv0(nr,nwal), qconv, qconv_avg
    
    integer i, j, ieqtyp, iroom, iwall, iw, nrmfire, ilay, ifire
    logical roomflg(nr), wallflg(4*nr)
    save flwcv0, flxcv0

    do i = 1, nm1
        flwcv(i,upper) = 0.0_eb
        flwcv(i,lower) = 0.0_eb
        do j = 1, nwal
            flxcv(i,j) = 0.0_eb
        end do
    end do
    if (option(fconvec)/=on) return

    do i = 1, nm1
        roomflg(i) = .true.
    end do
    do i = 1, nwalls
        wallflg(i) = .true.
    end do

    if(option(fmodjac)==on)then
        if(jaccol>0)then

            ! if 2nd modified jacobian is active and dassl is computing a jacobian then
            ! only compute convection heat transfer in the room where the dassl 
            ! solution variable has been perturbed
            do i = 1, nm1
                roomflg(i) = .false.
            end do
            do i = 1, nwalls
                wallflg(i) = .false.
            end do

            ieqtyp = izeqmap(jaccol,1)
            iroom = izeqmap(jaccol,2)
            if(ieqtyp==eqtu.or.ieqtyp==eqvu.or.ieqtyp==eqtl.or.ieqtyp==eqwt)then
                if(ieqtyp==eqwt)iroom = izwall(iroom,w_from_room)
                do iwall = 1, 4
                    roomflg(iroom) = .true.
                    if(switch(iwall,iroom))then
                        iw = izwmap2(iwall,iroom) - nofwt
                        wallflg(iw) = .true.
                    endif
                end do
            endif
        endif
    endif

    ! calculate convection for all surfaces in all rooms
    do iw = 1, nwalls
        if(wallflg(iw)) then
            i = izwall(iw,w_from_room)
            iwall = izwall(iw,w_from_wall)
            nrmfire = ifrpnt(i,1)
            if(mod(iwall,2)==1)then
                ilay = upper
            else
                ilay = lower
            endif
            ! assume no fires in this room.  just use regular convection
            call convec(iwall,zztemp(i,ilay),zzwtemp(i,iwall,1),flxcv(i,iwall))
            ! if there's a fire, we may need to modify the convection to account for the ceiling jet
            if (iwall==1.and.nrmfire>0) then
                qconv = 0.0_eb
                do ifire = 1, nrmfire
                    qconv = max(qconv,xfire(ifrpnt(i,2)+ifire-1,f_qfc))
                end do
                qconv_avg = 0.27_eb*qconv/((br(i)*dr(i))**0.68_eb*hr(i)**0.64_eb)
                if (qconv_avg>flxcv(i,iwall)) flxcv(i,iwall) = qconv_avg
            end if
            flwcv(i,ilay) = flwcv(i,ilay) - zzwarea(i,iwall)*flxcv(i,iwall)
        endif
    end do

    if (option(fmodjac)==on) then
        if (jaccol==0) then

            ! save the flux and flow vectors when we are about to compute a jacobian
            do iroom = 1, nm1
                flwcv0(iroom,1) = flwcv(iroom,1)
                flwcv0(iroom,2) = flwcv(iroom,2)
                do iwall = 1, 4
                    flxcv0(iroom,iwall) = flxcv(iroom,iwall)
                end do
            end do
        elseif (jaccol>0) then

            ! we are computing the jaccol'th column of the jacobian.  if the solution hasn't changed then get it from the vectors saved above.
            do iroom = 1, nm1
                if(.not.roomflg(iroom)) then
                    flwcv(iroom,1) = flwcv0(iroom,1)
                    flwcv(iroom,2) = flwcv0(iroom,2)
                    do iwall = 1, 4
                        flxcv(iroom,iwall) = flxcv0(iroom,iwall)
                    end do
                endif
            end do
        endif
    endif      
    return
    end subroutine convection

! --------------------------- convec -------------------------------------------

    subroutine convec (iw,tg,tw,qdinl)

    !     routine: convec
    !     purpose: calculate convective heat transfer for a wall segment. 
    !     arguments:  iw     wall number, standand cfast numbering convention
    !                 tg     temperature of gas layer adjacent to wall surface
    !                 tw     wall surface temperature
    !                 qdinl  convective flux into wall surface iw

    use precision_parameters
    implicit none

    integer, intent(in) :: iw
    real(eb), intent(in) :: tg, tw
    real(eb), intent(out) :: qdinl
    
    real(eb) :: h
    
    if (iw<=2) then
        h = 1.52_eb*abs(tg - tw)**onethird
    else
        h = 1.31_eb*abs(tg - tw)**onethird
    end if
    
    qdinl = h * (tg - tw)
    return
    end subroutine convec
    
    ! --------------------------- ceiling_jet -------------------------------------------

    subroutine ceiling_jet

    !     routine:     ceiling_jet

    !     description:  interface between calculate_residuals and cjet_detectors.  loops over
    !                 rooms setting up varibles to pass.  calls cjet_detectors
    !                 only when fires are in a room

    use precision_parameters
    use cenviro
    use fireptrs
    use cfast_main
    use opt

    implicit none

    real(eb) :: zloc, tceil, qceil, qfclga, qfwla, qfwua, ftmax, fvmax, fdmax
    integer :: i, id, iroom, nrmfire, nd, ifire, ifpnt

    do id = 1, ndtect
        iroom = ixdtect(id,droom)
        xdtect(id,dvel) = 0.0_eb
        zloc = xdtect(id,dzloc)
        if(zloc>zzhlay(iroom,lower))then
            xdtect(id,dtjet) = zztemp(iroom,upper)
        else
            xdtect(id,dtjet) = zztemp(iroom,lower)
        endif
    end do
    if (option(fcjet)==off) return

    do i = 1, nm1
        nrmfire = ifrpnt(i,1)
        id = idtpnt(i,2)
        nd = idtpnt(i,1)

        ! handle ceiling jets that are not in active halls

        if (nrmfire>0.and.izhall(i,ihmode)/=ihduring) then
            do ifire = 1, nrmfire
                ifpnt = ifrpnt(i,2) + ifire - 1
                if (switch(1,i)) then
                    tceil = twj(1,i,1)
                else
                    tceil = zztemp(i,upper)
                endif
                call cjet_detectors(xfire(ifpnt,f_plume_zpos),xfire(ifpnt,f_qfc),tceil,zztemp(i,lower),zztemp(i,upper),br(i),dr(i), &
                hr(i),xfire(ifpnt,f_fire_xpos),xfire(ifpnt,f_fire_ypos),xfire(ifpnt,f_fire_zpos),zzhlay(i,lower),zzrho(i,lower),zzrho(i,upper), &
                xdtect(id,dxloc),xdtect(id,dyloc),xdtect(id,dzloc),nd,qceil,qfclga,qfwla,qfwua,xdtect(id,dtjet),xdtect(id,dvel),ftmax,fvmax,fdmax)

            end do
        endif

        ! handle ceiling jets that are in active halls
        if(izhall(i,ihmode)==ihduring) call hallht(i,id,nd)

    end do
    return
    end subroutine ceiling_jet

! --------------------------- cjet_detectors -------------------------------------------

    subroutine cjet_detectors(mplume,qconv,atc,tl,tu,xw,yw,zc,axf,ayf,zf,zlay,rhol,rhou,xd,yd,zd,nd,qceil,qfclga, &
    qfwla,qfwua,td,vd,tdmax,vdmax,ddmax)

    !   this subroutine calculates temperatures and velocities at detector locations in the ceiling jet

    !   inputs:
    !	mplume		mass flow rate in the plume at zlay if zf < zlay [kg/s]
    !   nd          number of detectors
    !   qconv		portion of fire energy release rate convected in plume [w]
    !   atc         average temperature of ceiling [k]
    !   tl,tu       temperature of lower, upper layer [k]
    !   xd,yd,zd    x, y, and z positions of the detectors
    !   xw,yw,zc    co-ordinates of room corner diagonally opposite to origin 
    !               of axes (origin is at a corner on the floor, with x, y 
    !               axes along wall/floor junction and z axes upward) [m]
    !   xf,yf,zf    co-ordinates of center of base of fire [m]
    !   zlay        elevation above floor of layer interface [m]
    !   rhol,rhou   density of lower, upper layer [kg/m**3]
    !
    !	outputs
    !   qceil       rate of heat transfer to ceiling [w]
    !   qfclga		average flux of heat transfer to ceiling [w/m**2]
    !   qfwla,qfwua average flux of heat transfer to lower and upper portions of the wall sufaces [w/m**2]
    !   td          temperature of jet at xd,yd,zd locations
    !   vd          velocity of jet at xd,yd,zd locations
    !
    !***	some other definitions and fixed input in this subroutine

    !      alpha    tu/tl
    !         cp    specific heat of air at constant pressure [kj/(kg*k)]
    !         ct    9.115, constant in point source plume eqn.
    !          g    9.8, acceleration of gravity [m/s**2]
    !          h    if layer interface is below ceiling: distance of continuation source below ceiling; if layer interface 
    !               is at ceiling: distance of equivalent source below ceiling [m]
    !      qcont    strength of continuation source [w]
    !        qeq    dimensionless strength of plume at zlay
    !         qh    dimensionless strength of plume at zc
    !         pr    prandtl number
    !        tht    temperature of ambient in unconfined ceiling heat transfer problem, either tl or tu [k] 
    !        zeq    elevation above floor of equivalent source in lower layer [m]
    !         zs    elevation above floor of continuation source [m]
    !       tdmax   maximum temperature of the ceiling jet
    !       vdmax   maximum velocity of the ceiling jet
    !       ddmax   estimate of ceiling jet depth at r/h = 0.2 (given by:  ddmax/(.23*delta) = 2)

    use precision_parameters
    implicit none
    
    integer, intent(in) :: nd
    real(eb), intent(in) :: mplume, qconv, tl, tu, xd(*), yd(*), zd(*), xw, yw, zc, axf, ayf, zf, zlay, rhol, rhou
    real(eb), intent(out) :: qceil, qfclga, qfwla, qfwua, td(*), vd(*)
    
    !   this subroutine and the function qfclg used in its called subroutines use the common blocks aintch

    external qfclg
    integer :: id, ntab
    real(eb) :: xf, yf, rfmin, tc, atc, alpha, qeq, zeq, sigma_convection, a1, ssq, top, bottom, mfrac, qcont, zs, tht, rhoht, &
        h, qh, htct, anu, re, thtqhp, c1, c2, c3, c4, rd, rdh, v, vmax, vdmax, delta, dz, zdel, ddmax, vcj, arg, &
        rlamr, tmaxmtu, ths, thta, tcj, tdmax, qfclg, rmax
    real(eb), parameter :: ct = 9.115_eb, cp = 1012.0_eb, pr = 0.70_eb, rk1 = (0.23_eb/0.77_eb)*log(sqrt(2.0_eb)-1.0_eb)
    common /aintch/ h, htct, tht, thtqhp, c1, c2, c3, xf, yf, tc
    save /aintch/

    qceil = 0.0_eb
    qfclga = 0.0_eb
    qfwla = 0.0_eb
    qfwua = 0.0_eb
    if (mplume==0.0_eb.or.qconv==0.0_eb) then
        do id = 1, nd
            td(id) = max(tu,td(id))
            vd(id) = max(0.0_eb,vd(id))
        end do
        return
    endif
   
    xf = axf
    yf = ayf
    rfmin = 0.20_eb*(zc-zf)
    if (rfmin<xw/2.0_eb) then
        if (xf<rfmin) xf = rfmin
        if (xf>xw-rfmin) xf = xw - rfmin
    else
        xf = xw/2.0_eb
    endif
    if (rfmin<yw/2.0_eb) then
        if (yf<rfmin) yf = rfmin
        if (yf>yw-rfmin) yf = yw - rfmin
    else
        yf = yw/2.0_eb
    endif
    tc = atc
    alpha = tu/tl
    if (zf<zlay) then

        ! fire is below layer interface
        qeq = (0.21_eb*qconv/(cp*tl*mplume))**1.5_eb
        zeq = zlay - (qconv/(qeq*rhol*cp*tl*gsqrt))**(0.4_eb)
        if (zlay<zc) then

            ! layer interface is below ceiling
            if ((alpha - 1.0_eb)/=0.0_eb) then
                sigma_convection = -1.0_eb + ct*qeq**twothirds/(alpha - 1.0_eb)
                if (sigma_convection>0.0_eb) then
                    a1 = sigma_convection/(sigma_convection+1.0_eb)
                    ssq = sigma_convection**2
                    top = 1.04599_eb*sigma_convection + 0.360391_eb*ssq
                    bottom = 1.0_eb + 1.37748_eb*sigma_convection + 0.360391_eb*ssq
                    mfrac = top/bottom
                else
                    mfrac = 0.0_eb
                    qceil = 0.0_eb
                    qfclga = 0.0_eb
                    qfwla = 0.0_eb
                    qfwua = 0.0_eb
                    do id = 1, nd
                        td(id) = max(tu,td(id))
                        vd(id) = max(0.0_eb,vd(id))
                    end do
                    return
                endif
            else
                a1 = 1.0_eb
                mfrac = 1.0_eb
            endif
            qcont = qconv*a1*mfrac
            zs = zlay - (zlay-zeq)*alpha**0.6_eb*mfrac**0.4_eb/a1**0.2_eb
            tht = tu
            rhoht = rhou
        else

            ! layer interface is at ceiling
            qcont = qconv
            zs = zeq
            tht = tl
            rhoht = rhol
        endif
    else

        ! fire is at or above layer interface
        if (zf<zc) then

            ! fire is below ceiling
            qcont = qconv
            zs = zf
            tht = tu
            rhoht = rhou
        else

            ! fire is at ceiling
            qceil = 0.0_eb
            qfclga = 0.0_eb
            qfwla = 0.0_eb
            qfwua = 0.0_eb
            return
        endif
    endif
    h = zc - zs
    qh = qcont/(rhoht*cp*tht*sqrt(grav_con*h)*h**2)
    htct = rhoht*cp*sqrt(grav_con*h)*qh**onethird
    anu = (0.04128e-7_eb*tht**2.5_eb)/(tht+110.4_eb)
    re = sqrt(grav_con*h)*h*qh**onethird/anu
    thtqhp = tht*qh**twothirds
    c1 = 8.82_eb/(sqrt(re)*pr**twothirds)
    c2 = 5.0_eb - 0.284_eb*re**0.2_eb
    c3 = 0.283032655_eb/(re**0.3_eb*pr**twothirds)
    c4 = 0.94_eb/((re**0.42_eb)*pr)

    rmax = sqrt(max(yf,yw-yf)**2+max(xf,xw-xf)**2)

    ! make an integral table of size ntab from zero to rmax.
    ntab = 20
    call maktabl(rmax,ntab,qfclg)

    !calculate velocity and temperatures of the ceiling jet at detector locations
    do id = 1, nd
        rd = sqrt((axf-xd(id))**2+(ayf-yd(id))**2)
        rdh = rd/h
        if (rdh<=.20_eb) then
            rdh = 0.20_eb
            rd = rdh*h
        endif
        v = sqrt(grav_con*h)*qh**onethird
        vmax = 0.85_eb*v/rdh**1.1_eb
        vdmax = vmax
        delta = 0.1_eb*h*rdh**0.9_eb
        dz = zc - zd(id)
        zdel = dz/(0.23_eb*delta)
        ddmax = 2.0_eb*0.23_eb*delta
        if (zdel<=1.0_eb) then
            vcj = vmax*zdel**(1.0_eb/7.0_eb)*(8.0_eb-zdel)/7.0_eb
        else
            arg = rk1*(zdel-1.0_eb)
            vcj = vmax/cosh(arg)**2
        endif

        call inttabl(rd,rlamr)
        rlamr = 2.0_eb*pi*rlamr/qconv
        tmaxmtu = 2.6_eb*(1.0_eb-rlamr)*qh**twothirds*tu/rdh**0.80_eb - 0.90_eb*(tc-tu)
        ths = (tc-tu)/tmaxmtu
        if (zdel<=1.0_eb) then
            thta = ths + 2.0_eb*(1.0_eb-ths)*zdel - (1.0_eb-ths)*zdel** 2
        else
            thta = vcj/vmax
        endif
        tcj = tu + thta*tmaxmtu
        tdmax = tu + tmaxmtu
        td(id) = max(tcj,td(id))
        vd(id) = max(vcj,vd(id))
    end do
    return
    end subroutine cjet_detectors

! --------------------------- maktabl -------------------------------------------

    subroutine maktabl (r,n,func)

    !     Routine:     MAKTABL
    !
    !     Source File: CEILHT.SOR
    !
    !     Functional Class:  
    !
    !     Description:  
    !
    !     Arguments: R
    !                N
    !                FUNC


    use precision_parameters
    implicit none

    external func
    real(eb) :: tabl(100), fun(100), rmax, r, xxntabm1, dr, dr2, xxim1, func, rr
    integer :: i, ntab, n
    common /trptabl/ tabl, rmax, ntab
    save /trptabl/

    ntab = n
    rmax = r
    xxntabm1 = ntab - 1
    dr = rmax/xxntabm1
    dr2 = dr/2.0_eb
    tabl(1) = 0.0_eb
    fun(1) = 0.0_eb
    do i = 2, ntab
        xxim1 = i - 1
        rr = xxim1*dr
        fun(i) = rr*func(rr)
        tabl(i) = tabl(i-1) + (fun(i)+fun(i-1))*dr2
    end do
    return
    end subroutine maktabl

! --------------------------- inttabl -------------------------------------------

    subroutine inttabl (r,ans)

    !     Routine:     INTTABL
    !
    !     Source File: CEILHT.SOR
    !
    !     Functional Class:  
    !
    !     Description:  
    !
    !     Arguments: R
    !                ANS

    use precision_parameters
    implicit none

    real(eb) :: tabl(100), xxntabm1, dr, rmax, r, tab1, tab2, xxir, rr1, rr2, ans
    integer :: ir, ntab
    common /trptabl/ tabl, rmax, ntab
    save /trptabl/
    
    xxntabm1 = ntab - 1
    dr = rmax/xxntabm1 
    ir = 1.0_eb + r/dr
    if (ir<1) ir = 1
    if (ir>ntab-1) ir = ntab - 1
    tab1 = tabl(ir)
    tab2 = tabl(ir+1)
    xxir = ir
    rr1 = (xxir-1.0_eb)*dr
    rr2 = xxir*dr
    ans = (tab1*(rr2-r)+tab2*(r-rr1))/dr
    return
    end subroutine inttabl

! --------------------------- qfclg -------------------------------------------

    real(eb) function qfclg (r)

    !     Description: This function computes the convective heat transfer 
    !                  flux to the ceiling at location (X,Y)=(Z(1),Z(2)) 
    !
    !     Arguments: R
    !

    use precision_parameters
    implicit none

    real(eb) :: rdh, r, h, t0, t1, t2, t3, ff, htcldh, c1, c2, c3, taddim, htcl, htct, tad, thtqhp, tht, tc, xf, yf
    common /aintch/ h, htct, tht, thtqhp, c1, c2, c3, xf, yf, tc
    save /aintch/
    
    rdh = r/h
    t0 = rdh**(0.8_eb)
    t1 = 1.0_eb - 1.1_eb*t0
    t2 = t0**2
    t3 = t0**3
    ff = (t1+0.808_eb*t2)/(t1+2.2_eb*t2+0.69_eb*t3)
    if (rdh<0.2_eb) then
        htcldh = c1*(1.0_eb-c2*rdh)
        taddim = 10.22_eb - 14.9_eb*rdh
    else
        htcldh = c3*(rdh-0.0771_eb)/((rdh+0.279_eb)*rdh**1.2_eb)
        taddim = 8.390913361_eb*ff
    endif
    htcl = htcldh*htct
    tad = taddim*thtqhp + tht
    qfclg = htcl*(tad-tc)
    return
    end function qfclg
    
! --------------------------- convec -------------------------------------------

    integer function rev_convection ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_convection = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_convection