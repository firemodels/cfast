    subroutine rdheat(flwrad,flxrad,ierror)

    !     routine: rdheat
    !     purpose: Interface between RESID and RAD2 or RAD4.  Loops over
    !              rooms setting up varibles to pass.  If one or more fires
    !              are in a room calls RAD4 otherwise RAD2.
    !     Revision: $Revision: 484 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: flwrad      net enthalphy into each layer
    !                flxrad      net enthalphy flux into surface
    !                ierror      returns error codes

    use cenviro
    use cfast_main
    use fltarget
    use opt
    implicit none

    real*8 :: flwrad(nr,2), flxrad(nr,nwal), qlay(2), qflxw(nwal), twall(nwal), emis(nwal), tg(2), defabsup, defabslow, absorb
    integer :: map(nwal) = (/1, 4, 2, 3/), i, j, ieqtyp, iroom, iwall, ilay, imap, ifire, nrmfire, ierror
    logical black

    ! work and dummy arrays passed to rad2 and rad4
    real*8 :: taufl(mxfire,nwal), taufu(mxfire,nwal), firang(mxfire,nwal), dummy(4), flxrad0(nr,nwal), flwrad0(nr,2)
    logical roomflg(nr)
    save flxrad0, flwrad0

    do i = 1, nm1
        do j = 1, nwal
            flxrad(i,j) = 0.0d0
        end do
        flwrad(i,1) = 0.0d0
        flwrad(i,2) = 0.0d0
    end do

    if (option(frad)==off) return
    black = .false.
    if(option(frad)==3) black = .true.

    ! initially assume that we compute radiation transfer in every room
    do i = 1, nm1
        roomflg(i) = .true.
    end do

    if(option(fmodjac)==on)then
        if(jaccol>0)then

            ! if 2nd modified jacobian is active and dassl is computing a jacobian then
            ! only compute the radiation heat transfer in the room where the dassl 
            ! solution variable has been perturbed
            do i = 1, nm1
                roomflg(i) = .false.
            end do
            ieqtyp = izeqmap(jaccol,1)
            iroom = izeqmap(jaccol,2)
            if(ieqtyp==eqvu.or.ieqtyp==eqtu.or.ieqtyp==eqtl.or.ieqtyp==eqwt) then
                if(ieqtyp==eqwt)iroom = izwall(iroom,1)
                roomflg(iroom) = .true.
            endif
        endif
    endif

    do i = 1, nm1
        zzbeam(lower,i) = (1.8d0 * zzvol(i, lower)) / (ar(i) + zzhlay(i, lower) * (dr(i) + br(i)))
        zzbeam(upper,i) = (1.8 * zzvol(i, upper)) / (ar(i) + zzhlay(i, upper) * (dr(i) + br(i)))
    end do

    defabsup = 0.5d0
    defabslow = 0.01d0
    if(lfbo/=0.and.option(frad)/=4.and.lfbt/=1)then
        defabsup = absorb(lfbo,upper)
    endif

    do i = 1, nm1
        if(roomflg(i)) then
            tg(upper) = zztemp(i,upper)
            tg(lower) = zztemp(i,lower)
            zzbeam(lower,i) = (1.8d0 * zzvol(i, lower)) / (ar(i) + zzhlay(i, lower) * (dr(i) + br(i)))
            zzbeam(upper,i) = (1.8 * zzvol(i, upper)) / (ar(i) + zzhlay(i, upper) * (dr(i) + br(i)))
            do iwall = 1, 4
                if(mod(iwall,2)==1)then
                    ilay = upper
                else
                    ilay = lower
                endif
                imap = map(iwall)
                if (switch(iwall,i)) then
                    twall(imap) = zzwtemp(i,iwall,1)
                    emis(imap) = epw(iwall,i)
                else
                    twall(imap) = zztemp(i,ilay)
                    emis(imap) = 1.0d0
                endif
            end do
            ifire = ifrpnt(i,2)
            nrmfire = ifrpnt(i,1)
            if (nrmfire/=0) then
                if(.not.black)then
                    if(option(frad)==4.or.lfbt==1)then
                        zzabsb(upper,i) = defabsup
                        zzabsb(lower,i) = defabslow
                    else
                        zzabsb(upper,i) = absorb(i, upper)
                        zzabsb(lower,i) = absorb(i, lower)
                    endif
                endif
                call rad4(twall,tg,emis,zzabsb(1,i),i,br(i),dr(i),hr(i),zzhlay(i,lower),xfire(ifire,8),xfire(ifire,1),xfire(ifire,2),xfire(ifire,3),nrmfire, &
                qflxw,qlay,mxfire,taufl,taufu,firang,rdqout(1,i),black,ierror)
            else
                if(.not.black)then
                    if(option(frad)==2.or.option(frad)==4.or.lfbt==1)then
                        zzabsb(upper,i) = defabsup
                        zzabsb(lower,i) = defabslow
                    else
                        zzabsb(upper,i) = absorb(i, upper)
                        zzabsb(lower,i) = absorb(i, lower)
                    endif
                endif
                call rad2(twall,tg,emis,zzabsb(1,i),i,br(i),dr(i),hr(i),zzhlay(i,lower),xfire(ifire,8),xfire(ifire,1),xfire(ifire,2),xfire(ifire,3),nrmfire, &
                qflxw,qlay,mxfire,taufl,taufu,firang,rdqout(1,i),black,ierror)

            endif
            if (ierror/=0) return
            do j = 1, nwal
                flxrad(i,j) = qflxw(map(j))
            end do

            flwrad(i,1) = qlay(1)
            flwrad(i,2) = qlay(2)
            qr(1,i) = qlay(1)
            qr(2,i) = qlay(2)
        endif
    end do

    if(option(fmodjac)==on)then
        if(jaccol==0)then

            ! if the jacobian option is active and dassl is computing the base vector for
            ! the jacobian calculation then save the flow and flux calculation for later use
            do iroom = 1, nm1
                do iwall = 1, nwal
                    flxrad0(iroom,iwall) = flxrad(iroom,iwall)
                end do
                flwrad0(iroom,1) = flwrad(iroom,1)
                flwrad0(iroom,2) = flwrad(iroom,2)
            end do
        elseif(jaccol>0)then

            ! dassl is computing the jaccol'th column of a jacobian.  copy values into
            ! the flow and flux vectors that have not changed from the base vector
            do iroom = 1, nm1
                if(.not.roomflg(iroom))then
                    do iwall = 1, nwal
                        flxrad(iroom,iwall) = flxrad0(iroom,iwall)
                    end do
                    flwrad(iroom,1) = flwrad0(iroom,1)
                    flwrad(iroom,2) = flwrad0(iroom,2)
                end if
            end do
        endif
    endif
    return
    end subroutine rdheat

    subroutine rad2(twall,tlay,emis,absorb,iroom,xroom,yroom,zroom,hlay,qfire,xfire,yfire,zfire,nfire,qflux,qlay,mxfire,taufl,taufu,firang,qout,black,ierror)

    !     routine: rad2
    !     purpose: This routine computes the radiative heat flux to 
    !              the extended ceiling (ceiling + upper wall) and the extended 
    !              floor (floor + lower wall) due to a point source fire, emitting 
    !              absorbing gas layers (upper and lower) and heat emitting wall 
    !              segments.  This routine also computes the heat absorbed by the lower and upper layers.
    !     arguments: twall(i): twall(i) is the temperature of the i'th surface [k] . where
    !                          i=1,2,3,4 denotes the ceiling, the upper wall, the lower wall and the floor respectively.  
    !                tlay: tlay(i) is the temperature of the i'th layer [k] where i=1,2 denotes the upper, lower layers respectively
    !                emis: emis(i) is the emisivity of the ceiling (i=1), walls (i=2) and floor (i=3)
    !                absorb: absorb(i) is the absorbivity [1/m] of the upper (i=1), lower layer (i=2)
    !                xroom: size of the room [m] in the x'th coordinate direction.
    !                yroom: size of the room [m] in the y'th coordinate direction.
    !                zroom: size of the room [m] in the z'th coordinate direction.
    !                hlay: height of smoke layer interface above the floor [m]
    !                qfire: array of length nfire, qfire(ifire) is the energy release rate due to radiation of the ifire'th fire [w]
    !                xfire: x coordinate of fire location [m]
    !                yfire: y coordinate of fire location [m]
    !                zfire: z coordinate of fire location [m]
    !                qflux (ouptut): qflux(i) is the radiant heat flux [w/m**2] to the i'th surfaces where i=1,2,3,4 denotes the ceiling, the upper wall,
    !                                the lower wall and the floor respectively.  note that qflux(1)=qflux(2) and qflux(3)=qflux(4)
    !                qlay (output): qlay(i) is the heat absorbed by the i'th layer where i=1,2 denotes the upper, lower layers respectively
    !                qout (output): qout(i) is the output flux from the i'th wall
    !                ierror (output): returns error codes

    implicit none

    real*8 :: tlay(2), twall(4), emis(4), absorb(2), xfire(*), yfire(*), zfire(*), qlay(2), qflux(4), qfire(*), taul(2,2), tauu(2,2), beam(2,2), &
        taufl(mxfire,*), taufu(mxfire,*), firang(mxfire,*), area(2), area4(4), figs(2,2), emis2(2), qout(4), qqout(2), xxl(2), xxu(2), a(2,2), b(2,2), &
        e(2), c(2), rhs(2), dq(2), dqde(2), sigma, x1, pi, third, one, xroom, yroom, zroom, hlay, aread, fl, fu, xf, yf, zf, rdsang, f1d, f2d, rdparfig, &
        tupper4, tlower4, aij, qllay, qulay
    integer :: ipvt(2), ifire, nfire, i, j, k, info, iroom, mxfire, ierror

    logical black, first
    integer, parameter :: u = 1, l = 2
    save sigma,first,x1,pi,third,one

    data first /.true./

    ! define local constants first time rad2 is called
    if (first) then
        sigma = 5.67d-8
        first = .false.
        x1 = 1.0d0
        pi = 4.0d0 * atan(x1)
        third = 1.0d0 / 3.0d0
        one = 1.0d0
    endif

    ! define areas of upper and lower plates
    area4(1) = xroom * yroom
    area4(2) = 2.0d0 * (zroom-hlay) * (xroom+yroom)
    area4(3) = 2.0d0 * hlay * (xroom+yroom)
    area4(4) = area4(1)
    area(1) = area4(1) + area4(2)
    area(2) = area4(3) + area4(4)
    aread = area4(1)

    ! define configuration factors 
    figs(1,1) = 1.0d0 - aread / area(1)
    figs(2,2) = 1.0d0 - aread / area(2)
    figs(1,2) = aread / area(1)
    figs(2,1) = aread / area(2)

    ! define transmission factors for surfaces with respect to themselves

    beam(1,1) = (6.0d0*xroom*yroom*(zroom-hlay)/pi) ** third
    beam(2,2) = (6.0d0*xroom*yroom*hlay/pi) ** third
    beam(1,2) = zroom
    beam(2,1) = zroom
    fl = hlay / zroom
    fu = 1.0d0 - fl
    if(.not.black)then
        tauu(1,1) = exp(-beam(1,1)*absorb(1))
        taul(2,2) = exp(-beam(2,2)*absorb(2))
    else
        tauu(1,1) = 0.0d0
        taul(2,2) = 0.0d0
    endif
    tauu(2,2) = 1.0d0
    taul(1,1) = 1.0d0

    if(.not.black)then
        tauu(1,2) = exp(-fu*beam(1,2)*absorb(1))
        taul(1,2) = exp(-fl*beam(1,2)*absorb(2))
    else
        tauu(1,2) = 0.0d0
        taul(1,2) = 0.0d0
    endif
    tauu(2,1) = tauu(1,2)
    taul(2,1) = taul(1,2)

    ! define tranmission factors for surfaces with respect to fire
    do ifire = 1, nfire
        if (zfire(ifire)>hlay) then
            xxu(1) = zroom - zfire(ifire)
            xxu(2) = zfire(ifire) - hlay
            xxl(1) = 0.0d0
            xxl(2) = hlay
        else
            xxu(1) = zroom - hlay
            xxu(2) = 0.0d0
            xxl(1) = hlay - zfire(ifire)
            xxl(2) = zfire(ifire)
        endif
        do i = 1, 2
            if(.not.black)then 
                taufu(ifire,i) = exp(-absorb(1)*xxu(i))
                taufl(ifire,i) = exp(-absorb(2)*xxl(i))
            else
                taufu(ifire,i) = 0.0d0
                taufl(ifire,i) = 0.0d0
            endif
        end do
    end do

    ! compute solid angles
    do ifire = 1, nfire
        xf = xfire(ifire)
        yf = yfire(ifire)
        zf = zfire(ifire)
        firang(ifire,1) = rdsang(-xf,xroom-xf,-yf,yroom-yf,hlay-zf)
        firang(ifire,2) = 4.0d0*pi - firang(ifire,1)
    end do
    f1d = rdparfig(xroom,yroom,zroom-hlay)
    f2d = rdparfig(xroom,yroom,hlay)

    ! define e vector
    tupper4 = (twall(1)**4*f1d+twall(2)**4*(1.0d0-f1d))
    tlower4 = (twall(4)**4*f2d+twall(3)**4*(1.0d0-f2d))
    e(1) = sigma * tupper4
    e(2) = sigma * tlower4

    ! re-map emissivity vector
    emis2(1) = (emis(1)*area4(1) + emis(2)*area4(2))/area(1)
    emis2(2) = (emis(4)*area4(4) + emis(3)*area4(3))/area(2)

    ! define 'a' and 'b' coefficicnt matrix
    do k = 1, 2
        do j = 1, 2
            aij = figs(k,j) * taul(k,j) * tauu(k,j)
            a(k,j) = -aij * (1.0d0-emis2(j))
            b(k,j) = -aij
        end do
        a(k,k) = a(k,k) + 1.0d0
        b(k,k) = b(k,k) + 1.0d0
    end do

    ! define c vector
    call rdflux(mxfire,2,1,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

    ! construct right hand side (rhs) of linear system to be solved
    rhs(1) = b(1,1) * e(1) + b(1,2) * e(2) - c(1)
    rhs(2) = b(2,1) * e(1) + b(2,2) * e(2) - c(2)
    call dgefa(a,2,2,ipvt,info)
    call dgesl(a,2,2,ipvt,rhs,0)
    if(info/=0) then
        call xerror('RAD2 - singular matrix',0,1,1)
        ierror = 17
        return
    endif

    ! note: each row k of the a matrix as defined by seigal and howell was divided by emis2(k) (in order to insure that this new 'a' was
    ! diagonally dominant.  now we have to multiply the solution to the modified problem by emis2(i) to get the original answers
    do k = 1, 2
        dqde(k) = rhs(k)
        qqout(k) = e(k) - (one - emis(k))*dqde(k)
        dq(k) = rhs(k) * emis2(k)
    end do

    ! take solution and compute energy gain or loss to each panel and each layer.  also compute fluxes.  change sign so that
    ! a postive flux means that heat is flowing to the wall
    qflux(1) = -dq(1)
    qflux(2) = -dq(1)
    qflux(3) = -dq(2)
    qflux(4) = -dq(2)

    qout(1) = qqout(1)
    qout(2) = qqout(1)
    qout(3) = qqout(2)
    qout(4) = qqout(2)

    ! compute radiation absorbed by each layer
    call rdabs(2,1,e,dqde,emis2,area,figs,tauu,taul,qllay,qulay)

    qlay(u) = qulay
    qlay(l) = qllay

    return
    end subroutine rad2

    subroutine rad4(twall,tlay,emis,absorb,iroom,xroom,yroom,zroom,hlay,qfire,xfire,yfire,zfire,nfire,qflux,qlay,mxfire,taufl,taufu,firang,qout,black,ierror)

    !     routine: rad4
    !     purpose: this routine computes the radiative heat flux to the ceiling, upper wall, lower wall and floor due to 
    !              a point source fire, emitting absorbing gas layers (upper and lower) and heat emitting wall segments. this routine 
    !              also computes the heat absorbed by the lower and upper layers.
    !     arguments: twall(i): twall(i) is the temperature of the i'th surface [k] . where
    !                          i=1,2,3,4 denotes the ceiling, the upper wall, the lower wall and the floor respectively
    !                tlay: tlay(i) is the temperature of the i'th layer [k] where i=1,2 denotes the upper, lower layers respectively
    !                emis: emis(i) is the emisivity of the ceiling (i=1), walls (i=2) and floor (i=3)
    !                absorb: absorb(i) is the absorbivity [1/m] of the upper (i=1), lower layer (i=2)
    !                xroom: size of the room [m] in the x'th coordinate direction.
    !                yroom: size of the room [m] in the y'th coordinate direction.
    !                zroom: size of the room [m] in the z'th coordinate direction.
    !                hlay: height of smoke layer interface above the floor [m]
    !                qfire: array of length nfire, qfire(ifire) is the energy release rate due to radiation of the ifire'th fire [w]
    !                xfire: x coordinate of fire location [m]
    !                yfire: y coordinate of fire location [m]
    !                zfire: z coordinate of fire location [m]
    !                qflux (output): qflux(i) is the radiant heat flux [w/m**2] to the i'th surfaces where i=1,2,3,4 denotes the ceiling, the upper wall,
    !                                the lower wall and the floor respectively
    !                qlay (output): qlay(i) is the heat absorbed by the i'th layer where i=1,2 denotes the upper, lower layers respectively
    !                qout (output): qout(i) is the output flux from the i'th wall
    !                ierror - returns error codes

    include "precis.fi"

    dimension tlay(2), twall(4), emis(4), absorb(2)
    dimension xfire(*), yfire(*), zfire(*)
    dimension qlay(2), qflux(4), qfire(*)
    dimension taul(4,4), tauu(4,4), beam(4,4)
    dimension taufl(mxfire,*), taufu(mxfire,*)
    dimension firang(mxfire,*), area(4)
    dimension figs(4,4), qout(4)
    dimension zz(4)
    logical black

    dimension a(4,4), b(4,4), e(4), c(4)
    dimension ipvt(4), rhs(4), dq(4), dqde(4)

    integer l, u
    parameter (u = 1,l = 2,mxroom = 100)
    dimension iflag(mxroom), f14(mxroom)

    logical first

    save first,sigma

    data first /.true./, iflag /mxroom * 0/

    ! define local constants first time rad4 is called
    if (first) then
        sigma = 5.67d-8
        first = .false.
    endif
    if (iflag(iroom)==0) then
        f14(iroom) = rdparfig(xroom,yroom,zroom)
        iflag(iroom) = 1
    endif
    f14(iroom) = rdparfig(xroom,yroom,zroom)

    ! define areas
    area(1) = xroom * yroom
    area(2) = 2.0d0 * (zroom-hlay) * (xroom+yroom)
    area(3) = 2.0d0 * hlay * (xroom+yroom)
    area(4) = area(1)

    ! define configuration factors
    f1d = rdparfig(xroom,yroom,zroom-hlay)
    ff14 = f14(iroom)
    f4d = rdparfig(xroom,yroom,hlay)

    figs(1,1) = 0.0d0
    figs(1,2) = 1.0d0 - f1d
    figs(2,1) = area(1) * figs(1,2) / area(2)
    figs(2,2) = 1.0d0 - 2.0d0 * figs(2,1)
    figs(1,4) = ff14
    figs(4,1) = figs(1,4)

    figs(4,4) = 0.0d0
    figs(4,3) = 1.0d0 - f4d
    figs(3,4) = area(4) * figs(4,3) / area(3)
    figs(3,3) = 1.0d0 - 2.0d0 * figs(3,4)

    figs(1,3) = 1.0d0 - figs(1,4) - figs(1,2)
    figs(3,1) = area(1) * figs(1,3) / area(3)

    figs(3,2) = 1.0d0 - figs(3,4) - figs(3,3) - figs(3,1)
    figs(2,3) = area(3) * figs(3,2) / area(2)

    figs(2,4) = 1.0d0 - figs(2,3) - figs(2,2) - figs(2,1)
    figs(4,2) = area(2) * figs(2,4) / area(4)

    ! define transmission factors for surfaces, but first define beam lengths
    zz(1) = zroom
    zz(2) = (hlay+zroom) *.50d0
    zz(3) = hlay * .50d0
    zz(4) = 0.0d0
    dx2 = (xroom*.50d0) ** 2
    dy2 = (yroom*.50d0) ** 2
    x2 = xroom ** 2
    y2 = yroom ** 2

    beam(1,1) = 0.0d0

    dz2 = (zz(1)-zz(2)) ** 2
    beam(1,2) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*.50d0

    dz2 = (zz(1)-zz(3)) ** 2
    beam(1,3) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*.50d0

    beam(1,4) = zroom
    beam(2,2) = (xroom+yroom)*.50d0
    dz2 = (zroom*.50d0) ** 2
    beam(2,3) = (sqrt(dz2+x2)+sqrt(dz2+y2))*.50d0
    dz2 = ((zroom+hlay)*0.50d0) ** 2
    beam(2,4) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*.50d0
    beam(3,3) = beam(2,2)
    dh2 = (hlay*.50d0) ** 2
    beam(3,4) = (sqrt(dh2+dx2)+sqrt(dh2+dy2))*.50d0
    beam(4,4) = 0.0d0
    do i = 1, 4
        do j = i + 1, 4
            beam(j,i) = beam(i,j)
        end do
    end do

    call rdrtran(4,2,absorb,beam,hlay,zz,tauu,taul,black)

    ! define transmission factors for fires
    if (nfire/=0) then
        call rdftran(mxfire,4,2,absorb,hlay,zz,nfire,zfire,taufu,taufl,black)
    endif

    ! define solid angles for fires
    if (nfire/=0) then
        call rdfang(mxfire,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang)
    endif

    !     note: we want to solve the linear system
    !         a*dq = b*e + c
    !         where a and b are nxn matrices, q, e and c are n vectors

    ! define e vector
    do i = 1, 4
        e(i) = sigma * twall(i) ** 4
    end do

    ! define 'a' and 'b' coefficient matrix
    do k = 1, 4
        do j = 1, 4
            aij = figs(k,j) * taul(k,j) * tauu(k,j)
            a(k,j) = -aij * (1.0d0-emis(j))
            b(k,j) = -aij
        end do
        a(k,k) = a(k,k) + 1.0d0
        b(k,k) = b(k,k) + 1.0d0
    end do


    ! define c vector
    ! also, calculate energy absorbed by upper, lower layer gases due to fires and gas layer emission
    call rdflux(mxfire,4,2,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

    ! construct right hand side (rhs) of linear system to be solved, i.e. compute b*e - c
    do k = 1, 4
        rhs(k) = ddot(4,b(k,1),4,e(1),1) - c(k)
    end do

    ! solve the linear system
    call dgefa(a,4,4,ipvt,info)
    if(info/=0) then
        call xerror('RAD4 - singular matrix',0,1,1)
        ierror = 18
        do k = 1, 4
            rhs(k) = 0.0d0
        end do
    else
        call dgesl(a,4,4,ipvt,rhs,0)
    endif

    ! note: each row k of the a matrix, as defined by seigal and howell was divided by emis(k) (in order to insure that this new 'a' was
    ! diagonally dominant.  now we have to multiply the solution to the modified problem by emis(i) to get the answer to the original problem
    do k = 1, 4
        dqde(k) = rhs(k)
        qout(k) = e(k) - (1.0d0 - emis(k))*dqde(k)
        dq(k) = rhs(k) * emis(k)
    end do

    ! take solution and compute energy gain or loss to each panel and each layer.  also compute fluxes.  change sign so that
    ! a postive flux means that heat is flowing to the wall
    do i = 1, 4
        qflux(i) = -dq(i)
    end do

    ! compute radiation absorbed by each layer
    call rdabs(4,2,e,dqde,emis,area,figs,tauu,taul,qllay,qulay)

    qlay(u) = qulay
    qlay(l) = qllay

    return
    end subroutine rad4

    subroutine rdflux(mxfire,nzone,nup,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

    !     routine: rad4
    !     purpose: this routine calculates the 'c' vector in the net radiation equations of seigel and howell and the 
    !        heat absorbed by the lower and upper layer fires due to gas layer emission and fires..
    !     arguments: mxfire
    !                nzone
    !                nup
    !                area
    !                hlay
    !                tlay
    !                zfire
    !                qfire
    !                figs
    !                taul
    !                tauu
    !                taufl
    !                taufu
    !                firang
    !                nfire
    !                qllay
    !                qulay
    !                c

    include "precis.fi"

    integer l, u
    parameter (u = 1,l = 2)
    dimension c(*)
    dimension figs(nzone,*), taul(nzone,*), tauu(nzone,*)
    dimension taufl(mxfire,*), taufu(mxfire,*)
    dimension firang(mxfire,*), zfire(*)
    dimension area(*), qfire(mxfire), tlay(2)
    logical first
    save first, pi, sigma
    data first /.true./
    xx1 = 1.0d0
    if (first) then
        first = .false.
        pi = 4.0d0 * atan(xx1)
        sigma = 5.67d-8
    endif

    ! define c vector
    qulay = 0.d0
    qllay = 0.d0
    eu = sigma * tlay(u) ** 4
    el = sigma * tlay(l) ** 4
    do k = 1, nup
        c(k) = 0.d0

        ! case: upper to upper
        do j = 1, nup
            qugas = (xx1-tauu(k,j)) * eu
            c(k) = c(k) + figs(k,j) * qugas
            qulay = qulay - area(k) * figs(k,j) * qugas
        end do

        ! case: lower to upper
        do j = nup + 1, nzone
            qugas = (xx1-tauu(k,j)) * eu
            qlgas = (xx1-taul(k,j)) * el
            c(k) = c(k) + figs(k,j) * (qugas+qlgas*tauu(k,j))
            wf = area(k) * figs(k,j)
            qulay = qulay + qlgas * wf * (xx1-tauu(k,j)) - qugas * wf
            qllay = qllay - qlgas * wf
        end do

        ! case: fire to upper layer
        do ifire = 1, nfire
            qfflux = 0.25d0*qfire(ifire)*firang(ifire,k)/(pi*area(k))
            c(k) = c(k) + qfflux * taufl(ifire,k) * taufu(ifire,k)
            if (zfire(ifire)>hlay) then
                factu = xx1 - taufu(ifire,k)
                factl = 0.0d0
            else
                factu = (xx1-taufu(ifire,k)) * taufl(ifire,k)
                factl = xx1 - taufl(ifire,k)
            endif
            qulay = qulay + factu * qfflux * area(k)
            qllay = qllay + factl * qfflux * area(k)
        end do
    end do

    do k = nup + 1, nzone
        c(k) = 0.0d0

        ! case: upper to lower
        do j = 1, nup
            qugas = (xx1-tauu(k,j)) * eu
            qlgas = (xx1-taul(k,j)) * el
            c(k) = c(k) + figs(k,j) * (qugas*taul(k,j)+qlgas)
            wf = area(k) * figs(k,j)
            qulay = qulay - qugas * wf
            qllay = qllay + qugas * wf * (xx1-taul(k,j)) - qlgas * wf
        end do

        !case: lower to lower
        do j = nup + 1, nzone
            qlgas = (1.0d0-taul(k,j)) * el
            c(k) = c(k) + figs(k,j) * qlgas
            qllay = qllay - qlgas * area(k) * figs(k,j)
        end do

        ! case: fire to lower layer
        do ifire = 1, nfire
            qfflux = 0.25d0*qfire(ifire)*firang(ifire,k)/(pi*area(k))
            c(k) = c(k) + qfflux * taufl(ifire,k) * taufu(ifire,k)
            if (zfire(ifire)>hlay) then
                factu = xx1 - taufu(ifire,k)
                factl = (xx1-taufl(ifire,k)) * taufu(ifire,k)
            else
                factu = 0.0d0
                factl = xx1 - taufl(ifire,k)
            endif
            qulay = qulay + factu * qfflux * area(k)
            qllay = qllay + factl * qfflux * area(k)
        end do
    end do
    return
    end subroutine rdflux

    SUBROUTINE RDABS(NZONE,NUP,E,DQDE,EMIS2,AREA,FIGS,TAUU,TAUL,QLLAY,QULAY)

    !     routine: rdabs
    !     purpose: This routine computes the energy absorbed by the upper and lower layer due to radiation given off by heat emiiting rectangles
    !              forming the enclosure.  Coming into this routine, qllay and qulay were previously defined to be the heat absorbed by the lower and
    !              upper layers due to gas emissions and fires.  this routine just adds onto these values.
    !     arguments: NZONE
    !                NUP
    !                E
    !                DQDE
    !                EMIS2
    !                AREA
    !                FIGS
    !                TAUU
    !                TAUL
    !                QLLAY(output)
    !                QULAY (output)

    include "precis.fi"

    dimension e(*), emis2(*), area(*),dqde(*)
    dimension figs(nzone,*), tauu(nzone,*),taul(nzone,*)

    do k = 1, nup
        qout = e(k) - dqde(k)*(1.0d0-emis2(k))
        qk = qout*area(k)
        do j = 1,nup
            qulay = qulay + qk*figs(k,j)*(1.0d0-tauu(k,j))
        end do
        do j = nup + 1, nzone
            qulay = qulay + qk*figs(k,j)*(1.0d0-tauu(k,j))
            qllay = qllay + qk*figs(k,j)*tauu(k,j)*(1.0d0-taul(k,j))
        end do
    end do

    do k = nup+1,nzone
        qout = e(k) - dqde(k)*(1.0d0-emis2(k))
        qk = qout*area(k)
        do j = 1,nup
            qulay = qulay + qk*figs(k,j)*taul(k,j)*(1.0d0-tauu(k,j))
            qllay = qllay + qk*figs(k,j)*(1.0d0-taul(k,j))
        end do
        do j = nup+1,nzone
            qllay = qllay + qk*figs(k,j)*(1.0d0-taul(k,j))
        end do
    end do

    return
    end subroutine rdabs

    real*8 FUNCTION RDPARFIG(X,Y,Z)

    !     routine: rdparfig
    !     purpose: This routine calculates the configuration factor between two paralell plates a distance z a part.  Each 
    !          plate has a dimension of x by y.  the units of x, y and z are un-important except that they must be consistent.
    !     arguments: X
    !                Y
    !                Z

    include "precis.fi"

    SAVE IFIRST, PI, XX0, XX1, XXH
    DATA IFIRST /0/
    IF (IFIRST==0) THEN
        XX1 = 1.0D0
        XXH = 0.5D0
        XX0 = 0.0D0
        PI = 4.0D0 * ATAN(XX1)
        IFIRST = 1
    endif
    RDPARFIG = XX0
    IF (Z==XX0.OR.X==XX0.OR.Y==XX0) RETURN
    XX = X / Z
    YY = Y / Z
    F1 = XXH*LOG((XX1+XX**2)*(XX1+YY**2)/(XX1+XX**2+YY**2))
    YSQ = SQRT(XX1+YY**2)
    F2 = XX * YSQ * ATAN(XX/YSQ)
    XSQ = SQRT(XX1+XX**2)
    F3 = YY * XSQ * ATAN(YY/XSQ)
    F4 = XX * ATAN(XX)
    F5 = YY * ATAN(YY)
    RDPARFIG = 2.0D0 * (F1+F2+F3-F4-F5) / (PI*XX*YY)
    RETURN
    END
    real*8 FUNCTION RDPRPFIG(X,Y,Z)

    !     routine: rdparfig
    !     purpose: this routine calculates the configuration factor between two perpindular plates with a common edge.
    !     arguments: X
    !                Y
    !                Z

    include "precis.fi"

    LOGICAL FIRST
    SAVE FIRST, PI
    DATA FIRST /.TRUE./
    XX1 = 1.0D0
    XX0 = 0.0D0
    IF (FIRST) THEN
        PI = 4.0D0 * ATAN(XX1)
        FIRST = .FALSE.
    endif
    RDPRPFIG = XX0
    IF (Y==XX0.OR.X==XX0.OR.Z==XX0) RETURN
    H = X / Y
    W = Z / Y
    F1 = W * ATAN(XX1/W)
    F2 = H * ATAN(XX1/H)

    HWSUM = H ** 2 + W ** 2
    HWNORM = SQRT(HWSUM)
    RHWNORM = 1.0D0/HWNORM
    F3 = HWNORM * ATAN(RHWNORM)

    WSUM1 = XX1 + W ** 2
    HSUM1 = XX1 + H ** 2
    HWSUM2 = XX1 + HWSUM
    F4A = WSUM1 * HSUM1 / HWSUM2
    F4B = (W**2*HWSUM2/WSUM1/HWSUM)
    F4C = (H**2*HWSUM2/HSUM1/HWSUM)
    F4 = 0.25D0*(LOG(F4A)+LOG(F4B)*W**2+LOG(F4C)*H**2) 
    RDPRPFIG = (F1+F2-F3+F4) / PI / W
    RETURN
    END

    SUBROUTINE RDFANG(MXFIRE,XROOM,YROOM,ZROOM,HLAY,NFIRE,XFIRE,YFIRE,ZFIRE,FIRANG)

    !     routine: rdfang
    !     purpose: 
    !     arguments: MXFIRE
    !                XROOM
    !                YROOM
    !                ZROOM
    !                HLAY
    !                NFIRE
    !                XFIRE
    !                YFIRE
    !                ZFIRE
    !                FIRANG

    include "precis.fi"

    DIMENSION XFIRE(*), YFIRE(*), ZFIRE(*)
    DIMENSION FIRANG(MXFIRE,*)
    LOGICAL FIRST
    SAVE FIRST, FOURPI
    DATA FIRST/.TRUE./

    IF(FIRST)THEN
        FIRST = .FALSE.
        XX1 = 1.0D0
        PI = 4.0D0*ATAN(XX1)
        FOURPI = 4.0D0*PI
    ENDIF

    DO I = 1, NFIRE
        ARG1 = -XFIRE(I)
        ARG2 = XROOM - XFIRE(I)
        ARG3 = -YFIRE(I)
        ARG4 = YROOM - YFIRE(I)
        F1 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZROOM-ZFIRE(I))
        FD = RDSANG(ARG1,ARG2,ARG3,ARG4,HLAY-ZFIRE(I))
        F4 = RDSANG(ARG1,ARG2,ARG3,ARG4,ZFIRE(I))
        FIRANG(I,1) = F1
        FIRANG(I,4) = F4
        IF (ZFIRE(I)<HLAY) THEN
            FIRANG(I,2) = FD - F1
            FIRANG(I,3) = FOURPI - FD - F4
        ELSE
            FIRANG(I,2) = FOURPI - FD - F1
            FIRANG(I,3) = FD - F4
        endif
    end do
    RETURN
    END
    real*8 FUNCTION RDSANG(X1,X2,Y1,Y2,R)

    !     routine: rdsang
    !     purpose: 
    !     arguments: X1
    !                X2
    !                Y1
    !                Y2
    !                R

    include "precis.fi"

    f1 = sign(rdsang1(abs(x2),abs(y2),r),x2*y2)
    f2 = sign(rdsang1(abs(x1),abs(y2),r),x1*y2)
    f3 = sign(rdsang1(abs(x2),abs(y1),r),x2*y1)
    f4 = sign(rdsang1(abs(x1),abs(y1),r),x1*y1)
    rdsang = f1 - f2 - f3 + f4
    return
    end

    real*8 FUNCTION RDSANG1(X,Y,R)

    !     routine: rdsang1
    !     purpose: 
    !     arguments: X
    !                Y
    !                R

    include "precis.fi"

    LOGICAL FIRST
    SAVE FIRST, PI, PIO2

    DATA FIRST /.TRUE./

    XX0 = 0.0D0
    XX1 = 1.0D0
    IF (FIRST) THEN
        FIRST = .FALSE.
        PI = 4.0D0 * ATAN(XX1)
        PIO2 = PI / 2.0D0
    endif
    IF (X<=XX0.OR.Y<=XX0) THEN
        RDSANG1 = XX0
    ELSE
        XR = X * X + R * R
        XYR = X * X + Y * Y + R * R
        XY = X * X + Y * Y
        YR = Y * Y + R * R
        F1 = MIN(XX1, Y * SQRT(XYR/XY/YR))
        F2 = MIN(XX1, X * SQRT(XYR/XY/XR))
        RDSANG1 = (ASIN(F1)+ASIN(F2)-PIO2)
    endif
    RETURN
    END

    SUBROUTINE RDFTRAN(MXFIRE,NZONE,NUP,ABSORB,HLAY,ZZ,NFIRE,ZFIRE,TAUFU,TAUFL,BLACK)

    !     routine: rdftran
    !     purpose: 
    !     arguments: MXFIRE
    !                NZONE
    !                NUP
    !                ABSORB
    !                HLAY
    !                ZZ
    !                NFIRE
    !                ZFIRE
    !                TAUFU
    !                TAUFL

    include "precis.fi"

    DIMENSION ABSORB(*), ZZ(*), ZFIRE(*)
    DIMENSION TAUFU(MXFIRE,*), TAUFL(MXFIRE,*)
    LOGICAL BLACK
    DO I = 1, NFIRE
        DO J = 1, NUP
            IF (ZFIRE(I)>HLAY) THEN
                BEAM = ABS(ZZ(J)-ZFIRE(I))
                TAUFL(I,J) = 1.0D0
                IF(.NOT.BLACK)THEN
                    TAUFU(I,J) = EXP(-ABSORB(1)*BEAM)
                ELSE
                    TAUFU(I,J) = 0.0D0
                    TAUFL(I,J) = 0.0D0
                ENDIF
            ELSE
                BEAMU = ZZ(J) - HLAY
                BEAML = HLAY - ZFIRE(I)
                IF(.NOT.BLACK)THEN
                    TAUFU(I,J) = EXP(-ABSORB(1)*BEAMU)
                    TAUFL(I,J) = EXP(-ABSORB(2)*BEAML)
                ELSE
                    TAUFU(I,J) = 0.0D0
                    TAUFL(I,J) = 0.0D0
                ENDIF
            endif
        end do
        DO J = NUP + 1, NZONE
            IF (ZFIRE(I)<=HLAY) THEN
                BEAM = ABS(ZZ(J)-ZFIRE(I))
                TAUFU(I,J) = 1.0D0
                IF(.NOT.BLACK)THEN
                    TAUFL(I,J) = EXP(-ABSORB(2)*BEAM)
                ELSE
                    TAUFL(I,J) = 0.0D0
                    TAUFU(I,J) = 0.0D0
                ENDIF
            ELSE
                BEAMU = ZFIRE(I) - HLAY
                BEAML = HLAY - ZZ(J)
                IF(.NOT.BLACK)THEN
                    TAUFU(I,J) = EXP(-ABSORB(1)*BEAMU)
                    TAUFL(I,J) = EXP(-ABSORB(2)*BEAML)
                ELSE
                    TAUFU(I,J) = 0.0D0
                    TAUFL(I,J) = 0.0D0
                ENDIF
            endif
        end do
    end do
    RETURN
    END

    SUBROUTINE RDRTRAN(NZONE,NUP,ABSORB,BEAM,HLAY,ZZ,TAUU,TAUL,BLACK)

    !     routine: rdftran
    !     purpose: 
    !     arguments: NZONE
    !                NUP
    !                ABSORB
    !                BEAM
    !                HLAY
    !                ZZ
    !                TAUU
    !                TAUL

    include "precis.fi"
    DIMENSION ABSORB(*), BEAM(NZONE,NZONE), ZZ(*)
    DIMENSION TAUU(NZONE,NZONE), TAUL(NZONE,NZONE)
    LOGICAL BLACK

    ! DEFINE UPPER LAYER TRANSMISSION FACTORS
    ! UPPER TO UPPER
    DO I = 1, NUP
        DO J = I + 1, NUP
            IF(.NOT.BLACK)THEN
                TAUU(I,J) = EXP(-ABSORB(1)*BEAM(I,J))
            ELSE
                TAUU(I,J) = 0.0D0
            ENDIF
            TAUU(J,I) = TAUU(I,J)
        end do
        IF(.NOT.BLACK)THEN
            TAUU(I,I) = EXP(-ABSORB(1)*BEAM(I,I))
        ELSE
            TAUU(I,I) = 0.0D0
        ENDIF
    end do

    ! UPPER TO LOWER AND LOWER TO UPPER
    DO I = 1, NUP
        DO J = NUP + 1, NZONE
            FU = (ZZ(I)-HLAY) / (ZZ(I)-ZZ(J))
            IF(.NOT.BLACK)THEN
                TAUU(I,J) = EXP(-ABSORB(1)*BEAM(I,J)*FU)
            ELSE
                TAUU(I,J) = 0.0D0
            ENDIF
            TAUU(J,I) = TAUU(I,J)
        end do
    end do

    ! LOWER TO LOWER
    DO I = NUP + 1, NZONE
        DO J = NUP + 1, NZONE
            IF(.NOT.BLACK)THEN
                TAUU(I,J) = 1.0D0
            ELSE
                TAUU(I,J) = 0.0D0
            ENDIF
        end do
    end do

    ! DEFINE LOWER LAYER TRANSMISSION FACTORS
    ! LOWER TO LOWER
    DO I = NUP + 1, NZONE
        DO J = I + 1, NZONE
            IF(.NOT.BLACK)THEN
                TAUL(I,J) = EXP(-ABSORB(2)*BEAM(I,J))
            ELSE
                TAUL(I,J) = 0.0D0
            ENDIF
            TAUL(J,I) = TAUL(I,J)
        end do
        IF(.NOT.BLACK)THEN
            TAUL(I,I) = EXP(-ABSORB(2)*BEAM(I,I))
        ELSE
            TAUL(I,I) = 0.0D0
        ENDIF
    end do

    ! UPPER TO UPPER
    DO I = 1, NUP
        DO J = 1, NUP
            IF(.NOT.BLACK)THEN
                TAUL(I,J) = 1.0D0
            ELSE
                TAUL(I,J) = 0.0D0
            ENDIF
        end do
    end do

    ! UPPER TO LOEWR AND LOWER TO UPPER
    DO I = NUP + 1, NZONE
        DO J = 1, NUP
            FL = (HLAY-ZZ(I)) / (ZZ(J)-ZZ(I))
            IF(.NOT.BLACK)THEN
                TAUL(I,J) = EXP(-ABSORB(2)*BEAM(I,J)*FL)
            ELSE
                TAUL(I,J) = 0.0D0
            ENDIF
            TAUL(J,I) = TAUL(I,J)
        end do
    end do
    RETURN
    END

    real*8 function absorb (cmpt, layer)

    !  function calculates absorbance, due to gases (co2 and h2o) and soot, for the specified compartment and layer.

    !  absorbances are assumed to be equal to emissivities. per spfe handbook (1988 ed., pages 1-99 - 1-101), gas absorbance iscalculated as

    !  ag = ch2o * eh2o + cco2 * eco2 - deltae ~ eh2o + 0.5 * eco2;

    !  where ch2o and cco2 are concentrations and deltae is a correction
    !  for overlap of the absorbance bands.

    !  eco2 and eh2o are interpolated from spfe handbook graphs which show e = f(t,pl), where t is the gas temperature (kelvins) and pl is the
    !  partial pressure-path length product (atm-m). temperature and gas partial pressures are based on data calculated elsewhere and stored in
    !  common blocks. using handbook formulae, path length is estimated as

    !  l = c * 4 * v/a; where c ~ 0.9 for typical geometries, v is the gas volume and a is the surface area of the gas volume.

    !  total absorbance is calculated as

    !  at = as + ag * trans = (1 - exp(-as)) + ag * exp(-as);

    !  where as is soot absorpion, ag is gas absorption, trans is soot transmission, a is the effective absorbance coefficient for soot and
    !  s is the physical pathlength. s is apprxominated by l, the mean beam length, and a ~ k*vfs*tg, where vfs is the soot volume fraction, tg the
    !  gas temperature and k is a constant. for typical fuels, k ~ 1195.5.

    !  version 1.0.3

    use cenviro
    include "precis.fi"

    ! declare parameters
    integer noerr, hierr, loerr
    parameter (noerr=0, hierr=+1, loerr=-1)

    integer co2xsize, co2ysize, h2oxsize, h2oysize
    parameter (co2xsize=11, co2ysize=12, h2oxsize=11, h2oysize=12)

    integer co2, h2o, soot
    parameter (co2=3, h2o=8, soot=9)	

    ! declare i/o variables

    integer cmpt, layer

    !  declare internal variables
    !  units:
    !    tg = kelvins; tco2, th2o = log(kelvins)
    !    plg = atm-m; plco2, plh2o = log(atm-m)
    !    l = m; ng = mol; rtv = atm/mol
    !    ag, absorb = number (absorbance)
    !    aco2, ah2o, eco2, eh2o = log(emiss)
    !    vfs = number (soot volume fraction)
    !    rhos = kg/cubic meter (soot density)
    !    trans = number (soot transmission = exp(-k*vfs*tg*l))
    !    k = 1/(kelvin-meter) (ssot absorption constant)
    !    mwco2, mwh2o = gas molecular weight (kg/gm-mole)
    !    rg = ideal gas constant (atm-m^3/mol-k)

    integer xco2, yco2, xh2o, yh2o
    dimension tco2(co2xsize), plco2(co2ysize)
    dimension eco2(co2xsize,co2ysize)
    dimension th2o(h2oxsize)
    dimension plh2o(h2oysize), eh2o(h2oxsize,h2oysize)
    real*8 mwco2,mwh2o, k, rhos, l, ng

    ! declare module data

    ! physical constants [mw in kg/mol; rg in m^3-atm/mol-kelvin]
    data mwco2, mwh2o, rg, k, rhos /44.0088d-3, 18.0153d-3, 82.0562d-6, 1195.5d0, 1800.0d0/

    ! log(t) data for co2 [t in k] 
    data tco2  /2.3010d0, 2.4771d0, 2.6021d0, 2.6990d0, 2.7782d0, 2.8451d0, 2.9031d0, 2.9542d0,3.0000d0, 3.3010d0, 3.4771d0        /

    ! log(pl) data for co2 [pl in atm-m]
    data plco2 /-3.0000d0, -2.6990d0, -2.3979d0, -2.0000d0, -1.6990d0, -1.3979d0, -1.0000d0, -0.6990d0, -0.3979d0,  0.0000d0,  0.3010d0,  0.6021d0/

    ! log(emiss) data for co2 [stored in e(t,pl) format (ascending order by temperature, then by pressure-length)]
    data eco2  /-1.8508d0, -1.8416d0, -1.8508d0, -1.7799d0, -1.6990d0, -1.6799d0, -1.6904d0, -1.6990d0, -1.7399d0, -2.3706d0, -2.8996d0, &
    -1.6990d0, -1.6799d0, -1.6904d0, -1.6308d0, -1.5498d0, -1.5302d0, -1.5302d0, -1.5498d0, -1.5800d0, -2.1002d0, -2.6108d0, &
    -1.5406d0, -1.5200d0, -1.5498d0, -1.4895d0, -1.4401d0, -1.3904d0, -1.3904d0, -1.4101d0, -1.4202d0, -1.8894d0, -2.3002d0, &
    -1.3799d0, -1.3298d0, -1.3497d0, -1.3298d0, -1.2700d0, -1.2403d0, -1.2403d0, -1.2503d0, -1.2700d0, -1.6596d0, -2.0400d0, &
    -1.2403d0, -1.2000d0, -1.2403d0, -1.2104d0, -1.1599d0, -1.1403d0, -1.1302d0, -1.1403d0, -1.1500d0, -1.5200d0, -1.8894d0, &
    -1.1403d0, -1.1002d0, -1.1403d0, -1.1203d0, -1.0799d0, -1.0400d0, -1.0301d0, -1.0301d0, -1.0600d0, -1.3799d0, -1.7305d0, &
    -1.0400d0, -0.9914d0, -1.0200d0, -1.0200d0, -0.9706d0, -0.9547d0, -0.9431d0, -0.9355d0, -0.9431d0, -1.1599d0, -1.4802d0, &
    -0.9914d0, -0.9431d0, -0.9547d0, -0.9508d0, -0.9136d0, -0.8827d0, -0.8666d0, -0.8539d0, -0.8601d0, -1.1002d0, -1.3706d0, &
    -0.9355d0, -0.8697d0, -0.8928d0, -0.8827d0, -0.8477d0, -0.8097d0, -0.7932d0, -0.7852d0, -0.7932d0, -1.0000d0, -1.2700d0, &
    -0.8762d0, -0.8013d0, -0.8097d0, -0.8013d0, -0.7645d0, -0.7352d0, -0.7100d0, -0.6990d0, -0.6990d0, -0.8962d0, -1.1331d0, &
    -0.8297d0, -0.7496d0, -0.7645d0, -0.7472d0, -0.7055d0, -0.6696d0, -0.6421d0, -0.6326d0, -0.6402d0, -0.8097d0, -1.0301d0, &
    -0.8013d0, -0.7144d0, -0.7144d0, -0.6840d0, -0.6478d0, -0.6108d0, -0.5884d0, -0.5817d0, -0.5817d0, -0.7352d0, -0.9431d0  /

    !log(t) data for h2o [t in k] 
    data th2o  /2.3201d0, 2.4771d0, 2.6021d0, 2.6990d0, 2.7782d0, 2.8451d0, 2.9031d0, 2.9542d0,3.0000d0, 3.3010d0, 3.4771d0/

    ! log(pl) data for h2o [pl in atm-m]
    data plh2o /-3.0000d0, -2.6990d0, -2.3979d0, -2.0000d0, -1.6990d0, -1.3979d0, -1.0000d0, -0.6990d0,-0.3979d0,  0.0000d0,  0.3010d0,  0.6021d0/

    ! log(emiss) data for h2o [stored in e(t,pl) format (ascending order by temperature, then by pressure-length)]
    data eh2o  /-1.1500d0, -1.5200d0, -1.7496d0, -1.8996d0, -2.0000d0, -2.1002d0, -2.1898d0, -2.2798d0, -2.3706d0, -3.0555d0, -3.4437d0, &
    -1.0200d0, -1.3298d0, -1.5302d0, -1.6596d0, -1.7595d0, -1.8416d0, -1.9208d0, -2.0000d0, -2.0799d0, -2.7496d0, -3.1871d0, &
    -0.8962d0, -1.1701d0, -1.3242d0, -1.4597d0, -1.5406d0, -1.6003d0, -1.6596d0, -1.7305d0, -1.7905d0, -2.4202d0, -2.8794d0, &
    -0.7696d0, -1.0000d0, -1.1302d0, -1.2204d0, -1.3002d0, -1.3497d0, -1.4001d0, -1.4401d0, -1.4802d0, -1.9914d0, -2.5200d0, &
    -0.6402d0, -0.8729d0, -0.9957d0, -1.0799d0, -1.1302d0, -1.1701d0, -1.2104d0, -1.2503d0, -1.2899d0, -1.6904d0, -2.1500d0, &
    -0.5884d0, -0.7645d0, -0.8729d0, -0.9355d0, -0.9788d0, -1.0200d0, -1.0400d0, -1.0701d0, -1.1002d0, -1.4101d0, -1.8210d0, &
    -0.5003d0, -0.6556d0, -0.7258d0, -0.7545d0, -0.7932d0, -0.8153d0, -0.8447d0, -0.8665d0, -0.8894d0, -1.0799d0, -1.4401d0, &
    -0.4437d0, -0.5670d0, -0.6271d0, -0.6402d0, -0.6517d0, -0.6696d0, -0.6861d0, -0.6990d0, -0.7190d0, -0.8729d0, -1.1403d0, &
    -0.3936d0, -0.5086d0, -0.5302d0, -0.5376d0, -0.5482d0, -0.5528d0, -0.5670d0, -0.5719d0, -0.5817d0, -0.7122d0, -0.9431d0, &
    -0.3458d0, -0.4295d0, -0.4401d0, -0.4365d0, -0.4401d0, -0.4413d0, -0.4510d0, -0.4535d0, -0.4584d0, -0.5376d0, -0.7144d0, &
    -0.2958d0, -0.3686d0, -0.3686d0, -0.3645d0, -0.3645d0, -0.3686d0, -0.3706d0, -0.3757d0, -0.3757d0, -0.4510d0, -0.5952d0, &
    -0.2620d0, -0.3307d0, -0.3233d0, -0.3045d0, -0.3010d0, -0.3045d0, -0.3045d0, -0.3054d0, -0.3080d0, -0.3605d0, -0.5086d0  / 

    ! calculate layer-specific factors
    tg = zztemp(cmpt, layer)
    rtv = (rg * tg) / zzvol(cmpt, layer)
    l = zzbeam(layer,cmpt)

    ag = 0.0d0

    ! calculate absorbance for co2
    ng = zzgspec(cmpt, layer, co2) / mwco2
    plg = ng * rtv * l
    if (plg>1.0d-3) then
        cplg = log10(plg)
        tglog = log10(tg)
        call linterp(co2xsize, co2ysize, tco2, plco2, eco2, tglog, cplg, aco2, xco2, yco2)
        ag = ag + 0.50d0*10.0d0**aco2
    endif

    ! calculate absorbance for h2o
    ng = zzgspec(cmpt, layer, h2o) / mwh2o
    plg = ng * rtv * l
    if (plg>1.0d-3) then
        cplg = log10(plg)
        tglog = log10(tg)
        call linterp(h2oxsize, h2oysize, th2o, plh2o, eh2o, tglog, cplg, ah2o, xh2o, yh2o)
        ag = ag + 10.0d0**ah2o
    endif

    ! calculate total absorbance
    vfs = zzgspec(cmpt,layer,soot)/(zzvol(cmpt,layer) * rhos)
    absorb = max(k*vfs*tg - log(1.0d0-ag)/l,0.01d0)
    return
1000 format ('error in ',a3,' absorbance: xerror = 'i2,'; yerror = ',i2)
    end function absorb

    SUBROUTINE LINTERP(XDIM, YDIM, X, Y, Z, XVAL, YVAL, ZVAL, XERR, YERR)

    !     routine: linterp
    !     purpose: SUBROUTINE CALCULATES A 2-D LINEAR INTERPOLATION OF F(X,Y); WHERE KNOWN F(X,Y) VALUES ARE IN Z, ALLOWED X AND Y VALUES ARE IN X AND Y, THE POINT
    !              TO BE INTERPOLATED IS (XVAL,YVAL) AND THE INTERPOLATED RESULT IS RETURNED AS ZVAL. ARRAY DIMENSIONS ARE SPECIFIED BY XDIM AND YDIM, XERR AND YERR
    !              ARE ERROR VALUES RETURNED TO THE CALLING FUNCTION.

    !  THE EQUATION IMPLIMENTED BY THIS FUNCTION IS:

    !  F(X,Y) = Z(I,J) + {[Z(I+1,J) - Z(I,J)] / [X(I+1) - X(I)]} * [X - X(I)] 
    !          + {[Z(I,J+1) - Z(I,J)] / [Y(J+1) - Y(I)]} * [Y - Y(J)]
    !     arguments: 

    include "precis.fi"
    INTEGER HIERR
    PARAMETER (NOERR=0, HIERR=+1, LOERR=-1) 

    ! DECLARE I/O PARAMETERS 
    INTEGER XDIM, YDIM, XERR, YERR
    DIMENSION X(XDIM), Y(YDIM), Z(XDIM,YDIM)

    ! DECLARE INTERNAL VARIABLES
    INTEGER COUNT

    ! FIND THE VALUE OF I SUCH THAT X(1) <= XVAL <= X(XDIM). IF XVAL IS OUTSIDE THAT RANGE, SET IT TO THE CLOSEST LEGAL VALUE AND SET THE ERROR VALUE, AS APPROPRIATE.

    ! CHECK THE SPECIAL CASE OF XVAL < X(1)
    IF (XVAL < X(1)) THEN
        XERR = LOERR
        XVAL = X(1)
        I = 1

        ! CHECK THE SPECIAL CASE OF XVAL > X(XDIM)
    ELSE IF (XVAL > X(XDIM)) THEN
        XERR = HIERR
        XVAL = X(XDIM)
        I = XDIM

        ! CHECK THE CASES WHERE X(1) <= XVAL < X(XDIM)
    ELSE
        XERR = NOERR
        DO COUNT=2,XDIM
            IF (XVAL < X(COUNT)) THEN
                I = COUNT - 1
                GO TO 20
            endif 
        end do
        ! THEN XVAL = X(XDIM)
        I = XDIM
20      CONTINUE
    endif

    ! CHECK THE SPECIAL CASE OF YVAL < Y(1)
    IF (YVAL < Y(1)) THEN
        YERR = LOERR
        YVAL = Y(1)
        J = 1

        ! CHECK THE SPECIAL CASE OF YVAL > Y(YDIM)
    ELSE IF (YVAL > Y(YDIM)) THEN
        YERR = HIERR
        YVAL = Y(YDIM)
        J = YDIM

        ! CHECK THE CASES OF Y(1) <= YVAL < Y(YDIM)
    ELSE
        YERR = NOERR
        DO COUNT=2,YDIM
            IF (YVAL < Y(COUNT)) THEN
                J = COUNT - 1
                GO TO 40
            endif
        end do

        ! THEN YVAL = Y(YDIM)
        J = YDIM
40      CONTINUE
    endif

    ! CALCULATE DELTA X, SLOPE X AND THE Z INCREMENT DUE TO A CHANGE IN X. IF XVAL = X(XDIM), THEN (I+1) IS UNDEFINED AND THE SLOPE CAN NOT BE
    ! CALCULATED. HOWEVER, IN THOSE CASES, DELTA X IS ZERO, THERE IS NO CONTRIBUTION DUE TO THE CHANGE IN X AND THE ENTIRE TERM MAY BE SET EQUAL TO ZERO.
    DELTAX = XVAL - X(I)
    IF (DELTAX /= 0.0D0) THEN
        DZDX = (Z(I+1,J) - Z(I,J)) / (X(I+1) - X(I))
        DELX = DZDX * DELTAX
    ELSE
        DELX = 0.
    endif

    ! CALCULATE THE Z INCREMENT DUE TO A CHANGE IN Y AS ABOVE.
    DELTAY = YVAL - Y(J)
    IF (DELTAY /= 0.0D0) THEN
        DZDY = (Z(I,J+1) - Z(I,J)) / (Y(J+1) - Y(J))
        DELY = DZDY * DELTAY
    ELSE
        DELY = 0.
    endif

    ! INTERPOLATE A VALUE FOR F(X,Y)
    ZVAL = Z(I,J) + DELX + DELY
    RETURN
    END

    integer function rev_radiation

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision: 484 $'
    character(255), parameter :: maindate='$Date: 2012-07-23 14:49:22 -0400 (Mon, 23 Jul 2012) $'

    write(module_date,'(A)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_radiation = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_radiation