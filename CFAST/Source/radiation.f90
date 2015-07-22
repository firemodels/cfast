
! --------------------------- radiation -------------------------------------------

    subroutine radiation(flwrad,flxrad)

    !     routine: radiation
    !     purpose: Interface between calculate_residuals and RAD2 or RAD4.  Loops over
    !              rooms setting up varibles to pass.  If one or more fires
    !              are in a room calls RAD4 otherwise RAD2.
    !     Revision: $Revision$
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: flwrad      net enthalphy into each layer
    !                flxrad      net enthalphy flux into surface

    use precision_parameters
    use cenviro
    use fireptrs
    use cfast_main
    use opt
    use debug
    implicit none

    real(eb), intent(out), dimension(nr,2) :: flwrad
    real(eb), intent(out), dimension(nr,nwal) :: flxrad

    real(eb) :: qlay(2), qflxw(nwal), twall(nwal), emis(nwal), tg(2), defabsup, defabslow, absorb
    integer :: map(nwal) = (/1, 4, 2, 3/), i, j, ieqtyp, iroom, iwall, ilay, imap, ifire, nrmfire
    logical black

    ! work and dummy arrays passed to rad2 and rad4
    
    real(eb) :: taufl(mxfire,nwal), taufu(mxfire,nwal), firang(mxfire,nwal), flxrad0(nr,nwal), flwrad0(nr,2)
    real(eb) :: xrfirepos(mxfire), yrfirepos(mxfire), zrfirepos(mxfire)
    logical roomflg(nr)
    save flxrad0, flwrad0

    do i = 1, nm1
        do j = 1, nwal
            flxrad(i,j) = 0.0_eb
        end do
        flwrad(i,1) = 0.0_eb
        flwrad(i,2) = 0.0_eb
    end do

    if(option(frad)==off) return
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
            if(ieqtyp==eqvu.or.ieqtyp==eqtu.or.ieqtyp==eqtl.or.ieqtyp==eqwt)then
                if(ieqtyp==eqwt)iroom = izwall(iroom,1)
                roomflg(iroom) = .true.
            endif
        endif
    endif

    do i = 1, nm1
        zzbeam(lower,i) = (1.8_eb*zzvol(i, lower))/(room_area(i) + zzhlay(i, lower)*(room_depth(i) + room_width(i)))
        zzbeam(upper,i) = (1.8_eb*zzvol(i, upper))/(room_area(i) + zzhlay(i, upper)*(room_depth(i) + room_width(i)))
    end do

    defabsup = 0.50_eb
    defabslow = 0.01_eb

    do i = 1, nm1
        if(roomflg(i))then
            tg(upper) = zztemp(i,upper)
            tg(lower) = zztemp(i,lower)
            zzbeam(lower,i) = (1.8_eb*zzvol(i, lower))/(room_area(i) + zzhlay(i, lower)*(room_depth(i) + room_width(i)))
            zzbeam(upper,i) = (1.8_eb*zzvol(i, upper))/(room_area(i) + zzhlay(i, upper)*(room_depth(i) + room_width(i)))
            do iwall = 1, 4
                if(mod(iwall,2)==1)then
                    ilay = upper
                else
                    ilay = lower
                endif
                imap = map(iwall)
                twall(imap) = zzwtemp(i,iwall,1)
                emis(imap) = epw(iwall,i)
            end do
            ifire = ifrpnt(i,2)
            nrmfire = ifrpnt(i,1)
            do j = 1, nrmfire
                xrfirepos(j) = xfire(ifire+j-1,f_fire_xpos)
                yrfirepos(j) = xfire(ifire+j-1,f_fire_ypos)
                zrfirepos(j) = xfire(ifire+j-1,f_fire_zpos) ! This is point radiation at the base of the fire
                ! This is fire radiation at the center height of the fire (bounded by the ceiling height)
                !call flame_height (xfire(ifire+j-1,f_qfr),xfire(ifire+j-1,f_obj_area),fheight) 
                !if(fheight+xfire(ifire+j-1,f_fire_zpos)>room_height(i))then
                !    zrfirepos(j) = xfire(ifire+j-1,f_fire_zpos) + (room_height(i)-xfire(ifire+j,f_fire_zpos))/2.0_eb
                !else
                !    zrfirepos(j) = xfire(ifire+j-1,f_fire_zpos) + fheight/2.0_eb
                !end if
            end do
            if(nrmfire/=0)then
                if(.not.black)then
                    if(option(frad)==4.or.lfbt==1)then
                        zzabsb(upper,i) = defabsup
                        zzabsb(lower,i) = defabslow
                    else
                        zzabsb(upper,i) = absorb(i, upper)
                        zzabsb(lower,i) = absorb(i, lower)
                    endif
                endif
                call rad4(twall,tg,emis,zzabsb(1,i),i,room_width(i),room_depth(i),room_height(i),zzhlay(i,lower), &
                    xfire(ifire,f_qfr),xrfirepos,yrfirepos,zrfirepos,nrmfire, &
                    qflxw,qlay,mxfire,taufl,taufu,firang,rdqout(1,i),black)
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
                call rad2(twall,tg,emis,zzabsb(1,i),room_width(i),room_depth(i),room_height(i),zzhlay(i,lower),xfire(ifire,f_qfr),&
                   xrfirepos,yrfirepos,zrfirepos,nrmfire, &
                qflxw,qlay,mxfire,taufl,taufu,firang,rdqout(1,i),black)
            endif
            do j = 1, nwal
                flxrad(i,j) = qflxw(map(j))
            end do

            flwrad(i,1) = qlay(1)
            flwrad(i,2) = qlay(2)
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
        else if(jaccol>0)then

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
    end subroutine radiation

! --------------------------- rad2 -------------------------------------------

    subroutine rad2(twall,tlay,emis,absorb,xroom,yroom,zroom,hlay,qfire,xfire,yfire,zfire,nfire,qflux,qlay,&
       mxfire,taufl,taufu,firang,qout,black)

    !     routine: rad2
    !     purpose: This routine computes the radiative heat flux to 
    !              the extended ceiling (ceiling + upper wall) and the extended 
    !              floor (floor + lower wall) due to a point source fire, emitting 
    !              absorbing gas layers (upper and lower) and heat emitting wall 
    !              segments.  This routine also computes the heat absorbed by the lower and upper layers.
    !     input arguments: twall(i): twall(i) is the temperature of the i'th surface [k] . where
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
    !      output arguments qflux (ouptut): qflux(i) is the radiant heat flux [w/m**2] to the i'th surfaces where i=1,2,3,4 
    !                                denotes the ceiling, the upper wall, the lower wall and the floor respectively.  
    !                                note that qflux(1)=qflux(2) and qflux(3)=qflux(4)
    !                qlay (output): qlay(i) is the heat absorbed by the i'th layer where i=1,2 denotes the 
    !                               upper, lower layers respectively
    !                qout (output): qout(i) is the output flux from the i'th wall

    use precision_parameters
    use cshell, only: logerr
    implicit none

    integer, intent(in) :: nfire, mxfire
    real(eb), intent(in) :: twall(4), tlay(2), emis(4), absorb(2), xroom, yroom, zroom, hlay,qfire(*), xfire(*), yfire(*), zfire(*)

    real(eb), intent(out) :: qlay(2), qflux(4), qout(4)

    integer :: ipvt(2), ifire, i, j, k, info
    real(eb) :: taufl(mxfire,*), taufu(mxfire,*), firang(mxfire,*)
    real(eb) :: taul(2,2), tauu(2,2), beam(2,2)
    real(eb) :: area(2), area4(4), figs(2,2), emis2(2), qqout(2), xxl(2), xxu(2), a(2,2), b(2,2)
    real(eb) :: e(2), c(2), rhs(2), dq(2), dqde(2),  aread, fl, fu, xf, yf, zf, rdsang, f1d, f2d, rdparfig
    real(eb) :: tupper4, tlower4, aij, qllay, qulay

    logical black
    integer, parameter :: u = 1, l = 2


    ! define local constants first time rad2 is called

    ! define areas of upper and lower plates
    
    area4(1) = xroom*yroom
    area4(2) = 2.0_eb*(zroom-hlay)*(xroom+yroom)
    area4(3) = 2.0_eb*hlay*(xroom+yroom)
    area4(4) = area4(1)
    area(1) = area4(1) + area4(2)
    area(2) = area4(3) + area4(4)
    aread = area4(1)

    ! define configuration factors 
    
    figs(1,1) = 1.0_eb - aread/area(1)
    figs(2,2) = 1.0_eb - aread/area(2)
    figs(1,2) = aread/area(1)
    figs(2,1) = aread/area(2)

    ! define transmission factors for surfaces with respect to themselves

    beam(1,1) = (6.0_eb*xroom*yroom*(zroom-hlay)/pi)**onethird
    beam(2,2) = (6.0_eb*xroom*yroom*hlay/pi)**onethird
    beam(1,2) = zroom
    beam(2,1) = zroom
    fl = hlay/zroom
    fu = 1.0_eb - fl
    if(.not.black)then
        tauu(1,1) = exp(-beam(1,1)*absorb(1))
        taul(2,2) = exp(-beam(2,2)*absorb(2))
    else
        tauu(1,1) = 0.0_eb
        taul(2,2) = 0.0_eb
    endif
    tauu(2,2) = 1.0_eb
    taul(1,1) = 1.0_eb

    if(.not.black)then
        tauu(1,2) = exp(-fu*beam(1,2)*absorb(1))
        taul(1,2) = exp(-fl*beam(1,2)*absorb(2))
    else
        tauu(1,2) = 0.0_eb
        taul(1,2) = 0.0_eb
    endif
    tauu(2,1) = tauu(1,2)
    taul(2,1) = taul(1,2)

    ! define tranmission factors for surfaces with respect to fire
    
    do ifire = 1, nfire
        if(zfire(ifire)>hlay)then
            xxu(1) = zroom - zfire(ifire)
            xxu(2) = zfire(ifire) - hlay
            xxl(1) = 0.0_eb
            xxl(2) = hlay
        else
            xxu(1) = zroom - hlay
            xxu(2) = 0.0_eb
            xxl(1) = hlay - zfire(ifire)
            xxl(2) = zfire(ifire)
        endif
        do i = 1, 2
            if(.not.black)then 
                taufu(ifire,i) = exp(-absorb(1)*xxu(i))
                taufl(ifire,i) = exp(-absorb(2)*xxl(i))
            else
                taufu(ifire,i) = 0.0_eb
                taufl(ifire,i) = 0.0_eb
            endif
        end do
    end do

    ! compute solid angles
    
    do ifire = 1, nfire
        xf = xfire(ifire)
        yf = yfire(ifire)
        zf = zfire(ifire)
        firang(ifire,1) = rdsang(-xf,xroom-xf,-yf,yroom-yf,hlay-zf)
        firang(ifire,2) = 4.0_eb*pi - firang(ifire,1)
    end do
    f1d = rdparfig(xroom,yroom,zroom-hlay)
    f2d = rdparfig(xroom,yroom,hlay)

    ! define e vector
    
    tupper4 = (twall(1)**4*f1d+twall(2)**4*(1.0_eb-f1d))
    tlower4 = (twall(4)**4*f2d+twall(3)**4*(1.0_eb-f2d))
    e(1) = sigma*tupper4
    e(2) = sigma*tlower4

    ! re-map emissivity vector
    
    emis2(1) = (emis(1)*area4(1) + emis(2)*area4(2))/area(1)
    emis2(2) = (emis(4)*area4(4) + emis(3)*area4(3))/area(2)

    ! define 'a' and 'b' coefficicnt matrix
    
    do k = 1, 2
        do j = 1, 2
            aij = figs(k,j)*taul(k,j)*tauu(k,j)
            a(k,j) = -aij*(1.0_eb-emis2(j))
            b(k,j) = -aij
        end do
        a(k,k) = a(k,k) + 1.0_eb
        b(k,k) = b(k,k) + 1.0_eb
    end do

    ! define c vector
    
    call rdflux(mxfire,2,1,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

    ! construct right hand side (rhs) of linear system to be solved
    rhs(1) = b(1,1)*e(1) + b(1,2)*e(2) - c(1)
    rhs(2) = b(2,1)*e(1) + b(2,2)*e(2) - c(2)
    call dgefa(a,2,2,ipvt,info)
    call dgesl(a,2,2,ipvt,rhs,0)
    if(info/=0)then
        write (logerr,*) '***Error RAD2 - singular matrix'
        stop
    endif

    ! note: each row k of the a matrix as defined by seigal and howell was divided by emis2(k) (in order to insure 
    !       that this new 'a' was diagonally dominant.  now we have to multiply the solution to the modified problem 
    !       by emis2(i) to get the original answers

    do k = 1, 2
        dqde(k) = rhs(k)
        qqout(k) = e(k) - (1.0_eb - emis(k))*dqde(k)
        dq(k) = rhs(k)*emis2(k)
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
    
    call rabs(2,1,e,dqde,emis2,area,figs,tauu,taul,qllay,qulay)

    qlay(u) = qulay
    qlay(l) = qllay

    return
    end subroutine rad2

! --------------------------- rad4 -------------------------------------------

    subroutine rad4(twall,tlay,emis,absorb,iroom,xroom,yroom,zroom,hlay,qfire,xfire,yfire,zfire,nfire,&
       qflux,qlay,mxfire,taufl,taufu,firang,qout,black)

    !     routine: rad4
    !     purpose: this routine computes the radiative heat flux to the ceiling, upper wall, lower wall and floor due to 
    !              a point source fire, emitting absorbing gas layers (upper and lower) and heat emitting wall segments. 
    !              this routine also computes the heat absorbed by the lower and upper layers.
    !     intput arguments: twall(i): twall(i) is the temperature of the i'th surface [k] . where
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
    !      output arguments: qflux (output): qflux(i) is the radiant heat flux [w/m**2] to the i'th surfaces 
    !                                where i=1,2,3,4 denotes the ceiling, the upper wall,
    !                                the lower wall and the floor respectively
    !                qlay (output): qlay(i) is the heat absorbed by the i'th layer where i=1,2 denotes the 
    !                               upper, lower layers respectively
    !                qout (output): qout(i) is the output flux from the i'th wall

    use precision_parameters
    use cshell, only: logerr
    implicit none

    integer, parameter :: u = 1, l = 2, mxroom = 100
    integer :: ipvt(4), iflag(mxroom), iroom, i, j, k, nfire, info, mxfire
    
    real(eb), intent(in) :: twall(4), tlay(2), emis(4), absorb(2), xroom, yroom, zroom, hlay, qfire(*), xfire(*), yfire(*), zfire(*)
    real(eb), intent(out) :: taufl(mxfire,*), taufu(mxfire,*), firang(mxfire,*)
    
    real(eb), intent(out) :: qflux(4), qlay(2), qout(4)

    real(eb) :: taul(4,4), tauu(4,4), beam(4,4)
    real(eb) :: area(4), figs(4,4), zz(4), a(4,4), b(4,4), e(4), c(4), rhs(4), dq(4), dqde(4), f14(mxroom)
    real(eb) :: rdparfig, f1d, f4d, dx2, dy2, dz2, x2, y2, dh2, aij, qllay, qulay, ddot, ff14

    logical black

    data iflag /mxroom*0/

    if(iflag(iroom)==0)then
        f14(iroom) = rdparfig(xroom,yroom,zroom)
        iflag(iroom) = 1
    endif
    f14(iroom) = rdparfig(xroom,yroom,zroom)

    ! define areas
    
    area(1) = xroom*yroom
    area(2) = 2.0_eb*(zroom-hlay)*(xroom+yroom)
    area(3) = 2.0_eb*hlay*(xroom+yroom)
    area(4) = area(1)

    ! define configuration factors
    f1d = rdparfig(xroom,yroom,zroom-hlay)
    ff14 = f14(iroom)
    f4d = rdparfig(xroom,yroom,hlay)

    figs(1,1) = 0.0_eb
    figs(1,2) = 1.0_eb - f1d
    figs(2,1) = area(1)*figs(1,2)/area(2)
    figs(2,2) = 1.0_eb - 2.0_eb*figs(2,1)
    figs(1,4) = ff14
    figs(4,1) = figs(1,4)

    figs(4,4) = 0.0_eb
    figs(4,3) = 1.0_eb - f4d
    figs(3,4) = area(4)*figs(4,3)/area(3)
    figs(3,3) = 1.0_eb - 2.0_eb*figs(3,4)

    figs(1,3) = 1.0_eb - figs(1,4) - figs(1,2)
    figs(3,1) = area(1)*figs(1,3)/area(3)

    figs(3,2) = 1.0_eb - figs(3,4) - figs(3,3) - figs(3,1)
    figs(2,3) = area(3)*figs(3,2)/area(2)

    figs(2,4) = 1.0_eb - figs(2,3) - figs(2,2) - figs(2,1)
    figs(4,2) = area(2)*figs(2,4)/area(4)

    ! define transmission factors for surfaces, but first define beam lengths
    
    zz(1) = zroom
    zz(2) = (hlay+zroom)*0.50_eb
    zz(3) = hlay*0.50_eb
    zz(4) = 0.0_eb
    dx2 = (xroom*0.50_eb)**2
    dy2 = (yroom*0.50_eb)**2
    x2 = xroom**2
    y2 = yroom**2

    beam(1,1) = 0.0_eb

    dz2 = (zz(1)-zz(2))**2
    beam(1,2) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*0.5_eb

    dz2 = (zz(1)-zz(3))**2
    beam(1,3) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*0.5_eb

    beam(1,4) = zroom
    beam(2,2) = (xroom+yroom)*0.5_eb
    dz2 = (zroom*0.5_eb)**2
    beam(2,3) = (sqrt(dz2+x2)+sqrt(dz2+y2))*0.5_eb
    dz2 = ((zroom+hlay)*0.5_eb)**2
    beam(2,4) = (sqrt(dz2+dx2)+sqrt(dz2+dy2))*0.5_eb
    beam(3,3) = beam(2,2)
    dh2 = (hlay*0.5_eb)**2
    beam(3,4) = (sqrt(dh2+dx2)+sqrt(dh2+dy2))*0.5_eb
    beam(4,4) = 0.0_eb
    do i = 1, 4
        do j = i + 1, 4
            beam(j,i) = beam(i,j)
        end do
    end do

    call rdrtran(4,2,absorb,beam,hlay,zz,tauu,taul,black)

    ! define transmission factors for fires
    if(nfire/=0)then
        call rdftran(mxfire,4,2,absorb,hlay,zz,nfire,zfire,taufu,taufl,black)
    endif

    ! define solid angles for fires
    if(nfire/=0)then
        call rdfang(mxfire,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang)
    endif

    !     note: we want to solve the linear system
    !         a*dq = b*e + c
    !         where a and b are nxn matrices, q, e and c are n vectors

    ! define e vector
    do i = 1, 4
        e(i) = sigma*twall(i)**4
    end do

    ! define 'a' and 'b' coefficient matrix
    do k = 1, 4
        do j = 1, 4
            aij = figs(k,j)*taul(k,j)*tauu(k,j)
            a(k,j) = -aij*(1.0_eb-emis(j))
            b(k,j) = -aij
        end do
        a(k,k) = a(k,k) + 1.0_eb
        b(k,k) = b(k,k) + 1.0_eb
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
    if(info/=0)then
        write (logerr,*) '***Error: RAD4 - singular matrix'
        do k = 1, 4
            rhs(k) = 0.0_eb
        end do
    else
        call dgesl(a,4,4,ipvt,rhs,0)
    endif

    ! note: each row k of the a matrix, as defined by seigal and howell was divided by emis(k) (in order to insure
    !       that this new 'a' was diagonally dominant.  now we have to multiply the solution to the modified problem 
    !       by emis(i) to get the answer to the original problem
    do k = 1, 4
        dqde(k) = rhs(k)
        qout(k) = e(k) - (1.0_eb - emis(k))*dqde(k)
        dq(k) = rhs(k)*emis(k)
    end do

    ! take solution and compute energy gain or loss to each panel and each layer.  also compute fluxes.  change sign so that
    ! a postive flux means that heat is flowing to the wall
    do i = 1, 4
        qflux(i) = -dq(i)
    end do

    ! compute radiation absorbed by each layer
    call rabs(4,2,e,dqde,emis,area,figs,tauu,taul,qllay,qulay)

    qlay(u) = qulay
    qlay(l) = qllay

    return
    end subroutine rad4

! --------------------------- rdflux -------------------------------------------

    subroutine rdflux(mxfire,nzone,nup,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

    !     routine: rad4
    !     purpose: this routine calculates the 'c' vector in the net radiation equations of seigel and howell and the 
    !        heat absorbed by the lower and upper layer fires due to gas layer emission and fires..

    use precision_parameters
    implicit none

    integer, intent(in) :: mxfire, nzone, nfire, nup
    real(eb), intent(in) :: area(*), hlay, tlay(2), zfire(*), qfire(mxfire)
    real(eb), intent(in) :: figs(nzone,*), taul(nzone,*), tauu(nzone,*), taufl(mxfire,*), taufu(mxfire,*), firang(mxfire,*)
    
    real(eb), intent(out) :: qulay, qllay, c(*)
    
    integer, parameter ::u = 1, l = 2
    integer :: j, k, ifire
    real(eb) :: eu, el, qugas, qlgas, wf, qfflux, factu, factl

    ! define c vector
    
    qulay = 0.0_eb
    qllay = 0.0_eb
    eu = sigma*tlay(u)**4
    el = sigma*tlay(l)**4
    do k = 1, nup
        c(k) = 0.0_eb

        ! case: upper to upper
        do j = 1, nup
            qugas = (1.0_eb - tauu(k,j))*eu
            c(k) = c(k) + figs(k,j)*qugas
            qulay = qulay - area(k)*figs(k,j)*qugas
        end do

        ! case: lower to upper
        do j = nup + 1, nzone
            qugas = (1.0_eb - tauu(k,j))*eu
            qlgas = (1.0_eb - taul(k,j))*el
            c(k) = c(k) + figs(k,j)*(qugas + qlgas*tauu(k,j))
            wf = area(k)*figs(k,j)
            qulay = qulay + qlgas*wf*(1.0_eb - tauu(k,j)) - qugas*wf
            qllay = qllay - qlgas*wf
        end do

        ! case: fire to upper layer
        do ifire = 1, nfire
            qfflux = qfire(ifire)*firang(ifire,k)/(fourpi*area(k))
            c(k) = c(k) + qfflux*taufl(ifire,k)*taufu(ifire,k)
            if(zfire(ifire)>hlay)then
                factu = 1.0_eb - taufu(ifire,k)
                factl = 0.0_eb
            else
                factu = (1.0_eb - taufu(ifire,k))*taufl(ifire,k)
                factl = 1.0_eb - taufl(ifire,k)
            endif
            qulay = qulay + factu*qfflux*area(k)
            qllay = qllay + factl*qfflux*area(k)
        end do
    end do

    do k = nup + 1, nzone
        c(k) = 0.0_eb

        ! case: upper to lower
        
        do j = 1, nup
            qugas = (1.0_eb - tauu(k,j))*eu
            qlgas = (1.0_eb - taul(k,j))*el
            c(k) = c(k) + figs(k,j)*(qugas*taul(k,j) + qlgas)
            wf = area(k)*figs(k,j)
            qulay = qulay - qugas*wf
            qllay = qllay + qugas*wf*(1.0_eb - taul(k,j)) - qlgas*wf
        end do

        !case: lower to lower
        
        do j = nup + 1, nzone
            qlgas = (1.0_eb - taul(k,j))*el
            c(k) = c(k) + figs(k,j)*qlgas
            qllay = qllay - qlgas*area(k)*figs(k,j)
        end do

        ! case: fire to lower layer
        
        do ifire = 1, nfire
            qfflux = qfire(ifire)*firang(ifire,k)/(fourpi*area(k))
            c(k) = c(k) + qfflux*taufl(ifire,k)*taufu(ifire,k)
            if(zfire(ifire)>hlay)then
                factu = 1.0_eb - taufu(ifire,k)
                factl = (1.0_eb - taufl(ifire,k))*taufu(ifire,k)
            else
                factu = 0.0_eb
                factl = 1.0_eb - taufl(ifire,k)
            endif
            qulay = qulay + factu*qfflux*area(k)
            qllay = qllay + factl*qfflux*area(k)
        end do
    end do
    return
    end subroutine rdflux

! --------------------------- rabs -------------------------------------------

    subroutine rabs(nzone,nup,e,dqde,emis2,area,figs,tauu,taul,qllay,qulay)

    !     routine: rabs
    !     purpose: This routine computes the energy absorbed by the upper and lower layer due to radiation 
    !              given off by heat emiiting rectangles forming the enclosure.  Coming into this routine, 
    !              qllay and qulay were previously defined to be the heat absorbed by the lower and
    !              upper layers due to gas emissions and fires.  this routine just adds onto these values.

    use precision_parameters
    implicit none

    integer, intent(in) :: nup, nzone
    real(eb), intent(in) :: e(*), emis2(*), area(*),dqde(*), figs(nzone,*), tauu(nzone,*), taul(nzone,*)
    
    real(eb), intent(out) :: qulay, qllay

    integer :: j, k
    real(eb) :: qout, qk


    do k = 1, nup
        qout = e(k) - dqde(k)*(1.0_eb - emis2(k))
        qk = qout*area(k)
        do j = 1,nup
            qulay = qulay + qk*figs(k,j)*(1.0_eb - tauu(k,j))
        end do
        do j = nup + 1, nzone
            qulay = qulay + qk*figs(k,j)*(1.0_eb - tauu(k,j))
            qllay = qllay + qk*figs(k,j)*tauu(k,j)*(1.0_eb - taul(k,j))
        end do
    end do

    do k = nup+1,nzone
        qout = e(k) - dqde(k)*(1.0_eb-emis2(k))
        qk = qout*area(k)
        do j = 1,nup
            qulay = qulay + qk*figs(k,j)*taul(k,j)*(1.0_eb - tauu(k,j))
            qllay = qllay + qk*figs(k,j)*(1.0_eb - taul(k,j))
        end do
        do j = nup+1,nzone
            qllay = qllay + qk*figs(k,j)*(1.0_eb - taul(k,j))
        end do
    end do

    return
    end subroutine rabs

! --------------------------- rdparfig -------------------------------------------

    real(eb) function rdparfig(x,y,z)

    !     routine: rdparfig
    !     purpose: This routine calculates the configuration factor between two paralell plates a distance z a part.  Each 
    !          plate has a dimension of x by y.  the units of x, y and z are un-important except that they must be consistent.

    use precision_parameters
    implicit none
    
    real(eb), intent(in) :: x, y, z

    real(eb) :: xx, yy, xsq, ysq, f1, f2, f3, f4, f5

    rdparfig = 0.0_eb
    if(z==0.0_eb.or.x==0.0_eb.or.y==0.0_eb) return
    xx = x/z
    yy = y/z
    f1 = 0.5_eb*log((1.0_eb+xx**2)*(1.0_eb+yy**2)/(1.0_eb+xx**2+yy**2))
    ysq = sqrt(1.0_eb+yy**2)
    f2 = xx*ysq*atan(xx/ysq)
    xsq = sqrt(1.0_eb+xx**2)
    f3 = yy*xsq*atan(yy/xsq)
    f4 = xx*atan(xx)
    f5 = yy*atan(yy)
    rdparfig = 2.0_eb*(f1+f2+f3-f4-f5)/(pi*xx*yy)
    return
    end function rdparfig
    
! --------------------------- rdprpfig -------------------------------------------

    real(eb) function rdprpfig(x,y,z)

    !     routine: rdparfig
    !     purpose: this routine calculates the configuration factor between two perpindular plates with a common edge.

    use precision_parameters
    implicit none

    real(eb), intent(in) :: x, y, z

    real(eb) :: h, w, f1, f2, f3, f4a, f4b, f4c, f4, hwsum, hwnorm, rhwnorm, wsum1, hsum1, hwsum2

    rdprpfig = 0.0_eb
    if(y==0.0_eb.or.x==0.0_eb.or.z==0.0_eb) return
    h = x/y
    w = z/y
    f1 = w*atan(1.0_eb/w)
    f2 = h*atan(1.0_eb/h)

    hwsum = h**2 + w**2
    hwnorm = sqrt(hwsum)
    rhwnorm = 1.0_eb/hwnorm
    f3 = hwnorm*atan(rhwnorm)

    wsum1 = 1.0_eb + w**2
    hsum1 = 1.0_eb + h**2
    hwsum2 = 1.0_eb + hwsum
    f4a = wsum1*hsum1/hwsum2
    f4b = (w**2*hwsum2/wsum1/hwsum)
    f4c = (h**2*hwsum2/hsum1/hwsum)
    f4 = 0.25_eb*(log(f4a)+log(f4b)*w**2+log(f4c)*h**2) 
    rdprpfig = (f1+f2-f3+f4)/(pi*w)
    return
    end function rdprpfig

! --------------------------- rdfang -------------------------------------------

    subroutine rdfang(mxfire,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang)

    !     routine: rdfang
    !     purpose: 

    use precision_parameters
    implicit none


    integer, intent(in) :: mxfire, nfire
    real(eb), intent(in) :: xroom, yroom, zroom, hlay, xfire(*), yfire(*), zfire(*)
    
    real(eb), intent(out) :: firang(mxfire,*)
    
    real(eb) :: arg1, arg2, arg3, arg4, f1, f4, fd, rdsang
    integer :: i

    do i = 1, nfire
        arg1 = -xfire(i)
        arg2 = xroom - xfire(i)
        arg3 = -yfire(i)
        arg4 = yroom - yfire(i)
        f1 = rdsang(arg1,arg2,arg3,arg4,zroom-zfire(i))
        fd = rdsang(arg1,arg2,arg3,arg4,hlay-zfire(i))
        f4 = rdsang(arg1,arg2,arg3,arg4,zfire(i))
        firang(i,1) = f1
        firang(i,4) = f4
        if(zfire(i)<hlay)then
            firang(i,2) = fd - f1
            firang(i,3) = fourpi - fd - f4
        else
            firang(i,2) = fourpi - fd - f1
            firang(i,3) = fd - f4
        endif
    end do
    return
    end  subroutine rdfang

! --------------------------- rdsang -------------------------------------------

    real(eb) function rdsang(x1,x2,y1,y2,r)

    !     routine: rdsang
    !     purpose: 

    use precision_parameters
    implicit none
    
    real(eb), intent(in) :: x1, x2, y1, y2, r

    real(eb) :: f1, f2, f3, f4, rdsang1

    f1 = sign(rdsang1(abs(x2),abs(y2),r),x2*y2)
    f2 = sign(rdsang1(abs(x1),abs(y2),r),x1*y2)
    f3 = sign(rdsang1(abs(x2),abs(y1),r),x2*y1)
    f4 = sign(rdsang1(abs(x1),abs(y1),r),x1*y1)
    rdsang = f1 - f2 - f3 + f4
    return
    end function rdsang

! --------------------------- rdsang1 -------------------------------------------

    real(eb) function rdsang1(x,y,r)

    !     routine: rdsang1
    !     purpose: 

    use precision_parameters
    implicit none

    real(eb), intent(in) :: x, y, r

    real(eb) :: xr, yr, xy, xyr, f1, f2

    if(x<=0.0_eb.or.y<=1.0_eb)then
        rdsang1 = 0.0_eb
    else
        xr = x*x + r*r
        xyr = x*x + y*y + r*r
        xy = x*x + y*y
        yr = y*y + r*r
        f1 = min(1.0_eb, y*sqrt(xyr/xy/yr))
        f2 = min(1.0_eb, x*sqrt(xyr/xy/xr))
        rdsang1 = (asin(f1)+asin(f2)-pio2)
    endif
    return
    end function rdsang1

! --------------------------- rdftran -------------------------------------------

    subroutine rdftran(mxfire,nzone,nup,absorb,hlay,zz,nfire,zfire,taufu,taufl,black)

    !     routine: rdftran
    !     purpose: 

    use precision_parameters
    implicit none

    real(eb), intent(in) :: absorb(*), zz(*), zfire(*)
    integer, intent(in) :: mxfire, nzone, nup, nfire
    logical, intent(in) :: black
    real(eb), intent(out) :: taufu(mxfire,*), taufl(mxfire,*)
    
    real(eb) :: hlay, beam, beamu, beaml
    integer :: i, j
    
    do i = 1, nfire
        do j = 1, nup
            if(zfire(i)>hlay)then
                beam = abs(zz(j)-zfire(i))
                taufl(i,j) = 1.0_eb
                if(.not.black)then
                    taufu(i,j) = exp(-absorb(1)*beam)
                else
                    taufu(i,j) = 0.0_eb
                    taufl(i,j) = 0.0_eb
                endif
            else
                beamu = zz(j) - hlay
                beaml = hlay - zfire(i)
                if(.not.black)then
                    taufu(i,j) = exp(-absorb(1)*beamu)
                    taufl(i,j) = exp(-absorb(2)*beaml)
                else
                    taufu(i,j) = 0.0_eb
                    taufl(i,j) = 0.0_eb
                endif
            endif
        end do
        do j = nup + 1, nzone
            if(zfire(i)<=hlay)then
                beam = abs(zz(j)-zfire(i))
                taufu(i,j) = 1.0_eb
                if(.not.black)then
                    taufl(i,j) = exp(-absorb(2)*beam)
                else
                    taufl(i,j) = 0.0_eb
                    taufu(i,j) = 0.0_eb
                endif
            else
                beamu = zfire(i) - hlay
                beaml = hlay - zz(j)
                if(.not.black)then
                    taufu(i,j) = exp(-absorb(1)*beamu)
                    taufl(i,j) = exp(-absorb(2)*beaml)
                else
                    taufu(i,j) = 0.0_eb
                    taufl(i,j) = 0.0_eb
                endif
            endif
        end do
    end do
    return
    end

! --------------------------- rdrtran -------------------------------------------

    subroutine rdrtran(nzone,nup,absorb,beam,hlay,zz,tauu,taul,black)

    !     routine: rdftran
    !     purpose: 

    use precision_parameters
    implicit none

    integer, intent(in) ::  nup, nzone
    real(eb), intent(in) :: absorb(*), beam(nzone,nzone), zz(*), hlay
    real(eb), intent(out) :: tauu(nzone,nzone), taul(nzone,nzone)
    
    integer i, j
    real(eb) :: fu, fl
    logical black

    ! define upper layer transmission factors
    ! upper to upper
    
    do i = 1, nup
        do j = i + 1, nup
            if(.not.black)then
                tauu(i,j) = exp(-absorb(1)*beam(i,j))
            else
                tauu(i,j) = 0.0_eb
            endif
            tauu(j,i) = tauu(i,j)
        end do
        if(.not.black)then
            tauu(i,i) = exp(-absorb(1)*beam(i,i))
        else
            tauu(i,i) = 0.0_eb
        endif
    end do

    ! upper to lower and lower to upper
    
    do i = 1, nup
        do j = nup + 1, nzone
            fu = (zz(i)-hlay)/(zz(i)-zz(j))
            if(.not.black)then
                tauu(i,j) = exp(-absorb(1)*beam(i,j)*fu)
            else
                tauu(i,j) = 0.0_eb
            endif
            tauu(j,i) = tauu(i,j)
        end do
    end do

    ! lower to lower
    do i = nup + 1, nzone
        do j = nup + 1, nzone
            if(.not.black)then
                tauu(i,j) = 1.0_eb
            else
                tauu(i,j) = 0.0_eb
            endif
        end do
    end do

    ! define lower layer transmission factors
    
    ! lower to lower
    do i = nup + 1, nzone
        do j = i + 1, nzone
            if(.not.black)then
                taul(i,j) = exp(-absorb(2)*beam(i,j))
            else
                taul(i,j) = 0.0_eb
            endif
            taul(j,i) = taul(i,j)
        end do
        if(.not.black)then
            taul(i,i) = exp(-absorb(2)*beam(i,i))
        else
            taul(i,i) = 0.0_eb
        endif
    end do

    ! upper to upper
    
    do i = 1, nup
        do j = 1, nup
            if(.not.black)then
                taul(i,j) = 1.0_eb
            else
                taul(i,j) = 0.0_eb
            endif
        end do
    end do

    ! upper to loewr and lower to upper
    
    do i = nup + 1, nzone
        do j = 1, nup
            fl = (hlay-zz(i))/(zz(j)-zz(i))
            if(.not.black)then
                taul(i,j) = exp(-absorb(2)*beam(i,j)*fl)
            else
                taul(i,j) = 0.0_eb
            endif
            taul(j,i) = taul(i,j)
        end do
    end do
    return
    end

! --------------------------- absorb -------------------------------------------

    real(eb) function absorb(cmpt, layer)

    !  function calculates absorbance, due to gases (co2 and h2o) and soot, for the specified compartment and layer.

    !  absorbances are assumed to be equal to emissivities. per spfe handbook (1988 ed., pages 1-99 - 1-101), 
    !  gas absorbance iscalculated as

    !  ag = ch2o*eh2o + cco2*eco2 - deltae ~ eh2o + 0.5*eco2;

    !  where ch2o and cco2 are concentrations and deltae is a correction
    !  for overlap of the absorbance bands.

    !  eco2 and eh2o are interpolated from spfe handbook graphs which show e = f(t,pl), where t is the gas 
    !  temperature (kelvins) and pl is the
    !  partial pressure-path length product (atm-m). temperature and gas partial pressures are based on 
    !  data calculated elsewhere and stored in
    !  common blocks. using handbook formulae, path length is estimated as

    !  l = c*4*v/a; where c ~ 0.9 for typical geometries, v is the gas volume and a is the surface area of the gas volume.

    !  total absorbance is calculated as

    !  at = as + ag*trans = (1 - exp(-as)) + ag*exp(-as);

    !  where as is soot absorpion, ag is gas absorption, trans is soot transmission, a is the effective 
    !  absorbance coefficient for soot and
    !  s is the physical pathlength. s is apprxominated by l, the mean beam length, and a ~ k*vfs*tg, where 
    !  vfs is the soot volume fraction, tg the
    !  gas temperature and k is a constant. for typical fuels, k ~ 1195.5.

    !  version 1.0.3

    use precision_parameters
    use cenviro
    use debug
    
    implicit none

    integer, intent(in) :: cmpt, layer

    ! declare parameters
    integer, parameter :: noerr=0, hierr=+1, loerr=-1

    integer, parameter :: co2xsize=11, co2ysize=12, h2oxsize=11, h2oysize=12

    integer, parameter :: co2=3, h2o=8, soot=9

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

    integer :: xco2, yco2, xh2o, yh2o
    real(eb) :: tco2(co2xsize), plco2(co2ysize), eco2(co2xsize,co2ysize), th2o(h2oxsize), plh2o(h2oysize),&
       eh2o(h2oxsize,h2oysize), mwco2, mwh2o, k, rhos, l, ng
    real(eb) :: tg, rtv, ag, plg, cplg, tglog, aco2, ah2o, vfs, rg

    ! declare module data

    ! physical constants [mw in kg/mol; rg in m^3-atm/mol-kelvin]
    
    data mwco2, mwh2o, rg, k, rhos /44.0088e-3_eb, 18.0153e-3_eb, 82.0562e-6_eb, 1195.5_eb, 1800.0_eb/

    ! log(t) data for co2 [t in k]
    
    data tco2  /2.3010_eb, 2.4771_eb, 2.6021_eb, 2.6990_eb, 2.7782_eb, 2.8451_eb, 2.9031_eb, &
       2.9542_eb,3.0000_eb, 3.3010_eb, 3.4771_eb        /

    ! log(pl) data for co2 [pl in atm-m]
    
    data plco2 /-3.0000_eb, -2.6990_eb, -2.3979_eb, -2.0000_eb, -1.6990_eb, -1.3979_eb, -1.0000_eb, -0.6990_eb, &
       -0.3979_eb,  0.0000_eb,  0.3010_eb,  0.6021_eb/

    ! log(emiss) data for co2 [stored in e(t,pl) format (ascending order by temperature, then by pressure-length)]
    
    data eco2  /-1.8508_eb, -1.8416_eb, -1.8508_eb, -1.7799_eb, -1.6990_eb, -1.6799_eb, -1.6904_eb, -1.6990_eb, -1.7399_eb,&
       -2.3706_eb, -2.8996_eb, &
    -1.6990_eb, -1.6799_eb, -1.6904_eb, -1.6308_eb, -1.5498_eb, -1.5302_eb, -1.5302_eb, -1.5498_eb, -1.5800_eb, -2.1002_eb,&
       -2.6108_eb, &
    -1.5406_eb, -1.5200_eb, -1.5498_eb, -1.4895_eb, -1.4401_eb, -1.3904_eb, -1.3904_eb, -1.4101_eb, -1.4202_eb, -1.8894_eb, &
       -2.3002_eb, &
    -1.3799_eb, -1.3298_eb, -1.3497_eb, -1.3298_eb, -1.2700_eb, -1.2403_eb, -1.2403_eb, -1.2503_eb, -1.2700_eb, -1.6596_eb,&
       -2.0400_eb, &
    -1.2403_eb, -1.2000_eb, -1.2403_eb, -1.2104_eb, -1.1599_eb, -1.1403_eb, -1.1302_eb, -1.1403_eb, -1.1500_eb, -1.5200_eb,&
       -1.8894_eb, &
    -1.1403_eb, -1.1002_eb, -1.1403_eb, -1.1203_eb, -1.0799_eb, -1.0400_eb, -1.0301_eb, -1.0301_eb, -1.0600_eb, -1.3799_eb,&
       -1.7305_eb, &
    -1.0400_eb, -0.9914_eb, -1.0200_eb, -1.0200_eb, -0.9706_eb, -0.9547_eb, -0.9431_eb, -0.9355_eb, -0.9431_eb, -1.1599_eb,&
       -1.4802_eb, &
    -0.9914_eb, -0.9431_eb, -0.9547_eb, -0.9508_eb, -0.9136_eb, -0.8827_eb, -0.8666_eb, -0.8539_eb, -0.8601_eb, -1.1002_eb,&
       -1.3706_eb, &
    -0.9355_eb, -0.8697_eb, -0.8928_eb, -0.8827_eb, -0.8477_eb, -0.8097_eb, -0.7932_eb, -0.7852_eb, -0.7932_eb, -1.0000_eb,&
       -1.2700_eb, &
    -0.8762_eb, -0.8013_eb, -0.8097_eb, -0.8013_eb, -0.7645_eb, -0.7352_eb, -0.7100_eb, -0.6990_eb, -0.6990_eb, -0.8962_eb&
       , -1.1331_eb, &
    -0.8297_eb, -0.7496_eb, -0.7645_eb, -0.7472_eb, -0.7055_eb, -0.6696_eb, -0.6421_eb, -0.6326_eb, -0.6402_eb, -0.8097_eb,&
       -1.0301_eb, &
    -0.8013_eb, -0.7144_eb, -0.7144_eb, -0.6840_eb, -0.6478_eb, -0.6108_eb, -0.5884_eb, -0.5817_eb, -0.5817_eb, -0.7352_eb,&
       -0.9431_eb  /

    !log(t) data for h2o [t in k]
    
    data th2o  /2.3201_eb, 2.4771_eb, 2.6021_eb, 2.6990_eb, 2.7782_eb, 2.8451_eb, 2.9031_eb, 2.9542_eb,3.0000_eb, &
       3.3010_eb, 3.4771_eb/

    ! log(pl) data for h2o [pl in atm-m]
    
    data plh2o /-3.0000_eb, -2.6990_eb, -2.3979_eb, -2.0000_eb, -1.6990_eb, -1.3979_eb, -1.0000_eb, -0.6990_eb,-0.3979_eb, &
       0.0000_eb,  0.3010_eb,  0.6021_eb/

    ! log(emiss) data for h2o [stored in e(t,pl) format (ascending order by temperature, then by pressure-length)]
    
    data eh2o  /-1.1500_eb, -1.5200_eb, -1.7496_eb, -1.8996_eb, -2.0000_eb, -2.1002_eb, -2.1898_eb, -2.2798_eb, -2.3706_eb, &
       -3.0555_eb, -3.4437_eb, &
    -1.0200_eb, -1.3298_eb, -1.5302_eb, -1.6596_eb, -1.7595_eb, -1.8416_eb, -1.9208_eb, -2.0000_eb, -2.0799_eb, -2.7496_eb,&
       -3.1871_eb, &
    -0.8962_eb, -1.1701_eb, -1.3242_eb, -1.4597_eb, -1.5406_eb, -1.6003_eb, -1.6596_eb, -1.7305_eb, -1.7905_eb, -2.4202_eb,&
       -2.8794_eb, &
    -0.7696_eb, -1.0000_eb, -1.1302_eb, -1.2204_eb, -1.3002_eb, -1.3497_eb, -1.4001_eb, -1.4401_eb, -1.4802_eb, -1.9914_eb,&
       -2.5200_eb, &
    -0.6402_eb, -0.8729_eb, -0.9957_eb, -1.0799_eb, -1.1302_eb, -1.1701_eb, -1.2104_eb, -1.2503_eb, -1.2899_eb, -1.6904_eb,&
       -2.1500_eb, &
    -0.5884_eb, -0.7645_eb, -0.8729_eb, -0.9355_eb, -0.9788_eb, -1.0200_eb, -1.0400_eb, -1.0701_eb, -1.1002_eb, -1.4101_eb,&
       -1.8210_eb, &
    -0.5003_eb, -0.6556_eb, -0.7258_eb, -0.7545_eb, -0.7932_eb, -0.8153_eb, -0.8447_eb, -0.8665_eb, -0.8894_eb, -1.0799_eb,&
       -1.4401_eb, &
    -0.4437_eb, -0.5670_eb, -0.6271_eb, -0.6402_eb, -0.6517_eb, -0.6696_eb, -0.6861_eb, -0.6990_eb, -0.7190_eb, -0.8729_eb,&
       -1.1403_eb, &
    -0.3936_eb, -0.5086_eb, -0.5302_eb, -0.5376_eb, -0.5482_eb, -0.5528_eb, -0.5670_eb, -0.5719_eb, -0.5817_eb, -0.7122_eb,&
       -0.9431_eb, &
    -0.3458_eb, -0.4295_eb, -0.4401_eb, -0.4365_eb, -0.4401_eb, -0.4413_eb, -0.4510_eb, -0.4535_eb, -0.4584_eb, -0.5376_eb,&
       -0.7144_eb, &
    -0.2958_eb, -0.3686_eb, -0.3686_eb, -0.3645_eb, -0.3645_eb, -0.3686_eb, -0.3706_eb, -0.3757_eb, -0.3757_eb, -0.4510_eb,&
       -0.5952_eb, &
    -0.2620_eb, -0.3307_eb, -0.3233_eb, -0.3045_eb, -0.3010_eb, -0.3045_eb, -0.3045_eb, -0.3054_eb, -0.3080_eb, -0.3605_eb,&
    -0.5086_eb /

    ! calculate layer-specific factors
    
    tg = zztemp(cmpt, layer)
    rtv = (rg*tg)/zzvol(cmpt, layer)
    l = zzbeam(layer,cmpt)

    ag = 0.0_eb

    ! calculate absorbance for co2
    
    ng = zzgspec(cmpt, layer, co2)/mwco2
    plg = ng*rtv*l
    !if(plg>1.0e-3_eb)then
    if(plg>0.0_eb)then
        cplg = log10(plg)
        tglog = log10(tg)
        call linterp(co2xsize, co2ysize, tco2, plco2, eco2, tglog, cplg, aco2, xco2, yco2)
        ag = ag + 0.50_eb*10.0_eb**aco2
    else
        aco2 = 0.0_eb
    endif

    ! calculate absorbance for h2o
    
    ng = zzgspec(cmpt, layer, h2o)/mwh2o
    plg = ng*rtv*l
    !if(plg>1.0e-3_eb)then
    if(plg>0.0_eb)then
        cplg = log10(plg)
        tglog = log10(tg)
        call linterp(h2oxsize, h2oysize, th2o, plh2o, eh2o, tglog, cplg, ah2o, xh2o, yh2o)
        ag = ag + 10.0_eb**ah2o
    else
        ah2o = 0.0_eb
    endif

    ! calculate total absorbance
    
    vfs = zzgspec(cmpt,layer,soot)/(zzvol(cmpt,layer)*rhos)
    absorb = max(k*vfs*tg - log(1.0_eb-ag)/l,0.01_eb)

    return
!1000 format ('error in ',a3,' absorbance: xerror = ',i2,'; yerror = ',i2)
    end function absorb

! --------------------------- linterp -------------------------------------------

    subroutine linterp (xdim, ydim, x, y, z, xval, yval, zval, xerr, yerr)

    !     routine: linterp
    !     purpose: subroutine calculates a 2-d linear interpolation of f(x,y); where known f(x,y) values are in z, allowed 
    !              x and y values are in x and y, the point
    !              to be interpolated is (xval,yval) and the interpolated result is returned as zval. array dimensions are specified
    !              by xdim and ydim, xerr and yerr are error values returned to the calling function.

    !  the equation implimented by this function is:

    !  f(x,y) = z(i,j) + {[z(i+1,j) - z(i,j)]/[x(i+1) - x(i)]}*[x - x(i)] 
    !          + {[z(i,j+1) - z(i,j)]/[y(j+1) - y(i)]}*[y - y(j)]
    !     arguments: 

    use precision_parameters
    implicit none

    integer, intent(in) :: xdim, ydim
    real(eb), intent(inout) :: xval, yval
    integer, intent(out) :: xerr, yerr

    real(eb), intent(in) :: x(xdim), y(ydim), z(xdim,ydim)
    real(eb) :: zval
    integer, parameter :: noerr=0, hierr=+1, loerr=-1
    integer :: count, i, j

    ! find the value of i such that x(1) <= xval <= x(xdim). if xval is outside that range, set it to the closest legal 
    ! value and set the error value, as appropriate.

    ! check the special case of xval < x(1)
    
    if(xval < x(1))then
        xerr = loerr
        xval = x(1)
        i = 1

        ! check the special case of xval > x(xdim)
        
    else if(xval > x(xdim))then
        xerr = hierr
        xval = x(xdim)
        i = xdim

        ! check the cases where x(1) <= xval < x(xdim)
        
    else
        xerr = noerr
        do count=2,xdim
            if(xval < x(count))then
                i = count - 1
                go to 20
            endif 
        end do
        ! then xval = x(xdim)
        i = xdim
20      continue
    endif

    ! check the special case of yval < y(1)
    
    if(yval < y(1))then
        yerr = loerr
        yval = y(1)
        j = 1

        ! check the special case of yval > y(ydim)
        
    else if(yval > y(ydim))then
        yerr = hierr
        yval = y(ydim)
        j = ydim

        ! check the cases of y(1) <= yval < y(ydim)
        
    else
        yerr = noerr
        do count=2,ydim
            if(yval < y(count))then
                j = count - 1
                go to 40
            endif
        end do

        ! then yval = y(ydim)
        
        j = ydim
40      continue
    endif

    ! interpolate a value for f(x,y)
    
    zval = z(i,j)*(x(i+1)-xval)*(y(j+1)-yval)
    zval = zval + z(i+1,j)*(xval - x(i))*(y(j+1)-yval)
    zval = zval + z(i,j+1)*(x(i+1)-xval)*(yval - y(j))
    zval = zval + z(i+1,j+1)*(xval - x(i))*(yval-y(j))
    zval = zval/((x(i+1)-x(i))*(y(j+1)-y(j))) 
    
    return
    end subroutine linterp
