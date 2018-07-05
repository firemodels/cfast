module radiation_routines

    use precision_parameters

    use fire_routines, only: flame_height
    use numerics_routines, only : ddot, dnrm2, dgefa, dgesl

    use precision_parameters
    use cenviro
    use setup_data, only: iofill
    use cparams
    use room_data
    use fire_data, only: n_fires, fireinfo
    use option_data
    use diag_data

    implicit none

    private

    public radiation, absorb, solid_angle_triangle

    contains

    ! --------------------------- radiation -------------------------------------------

    subroutine radiation(flows_radiation,fluxes_radiation)

    !     routine: radiation
    !     purpose: Interface between calculate_residuals and RAD2 or RAD4.  Loops over
    !              rooms setting up varibles to pass.  If one or more fires
    !              are in a room calls RAD4 otherwise RAD2.
    !     Revision: $Revision$
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: flows_radiation      net enthalphy into each layer
    !                fluxes_radiation      net enthalphy flux into surface

    real(eb), intent(out), dimension(mxrooms,2) :: flows_radiation
    real(eb), intent(out), dimension(mxrooms,nwal) :: fluxes_radiation

    real(eb) :: qlay(2), qflxw(nwal), twall(nwal), emis(nwal), tg(2), defabsup, defabslow, fheight, rabsorb(2)
    real(eb) :: chi10(10)
    integer :: map(nwal) = (/1, 4, 2, 3/), i, j, iwall, imap, ifire, nrmfire
    logical black
    
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    ! work and dummy arrays passed to rad2 and rad4

    real(eb) :: taufl(mxfires,nwal), taufu(mxfires,nwal), firang(nwal,mxfires)
    real(eb) :: taufl10(mxfires,10), taufu10(mxfires,10), firang10(10,mxfires)
    real(eb) :: xrfire(mxfires), yrfire(mxfires), zrfire(mxfires), qrfire(mxfires)

    fluxes_radiation(1:nrm1,1:nwal) = 0.0_eb
    flows_radiation(1:nrm1,1:2) = 0.0_eb

    if (option(frad)==off) return
    black = .false.

    defabsup = 0.50_eb
    defabslow = 0.01_eb

    do i = 1, nrm1
        roomptr => roominfo(i)
        if (radi_verification_flag .and. upper_layer_thickness > 0._eb) then
            roomptr%volume(u) = roomptr%depth(u)*roomptr%floor_area
            roomptr%volume(l) = roomptr%depth(l)*roomptr%floor_area
        end if
        tg(u) = roomptr%temp(u)
        tg(l) = roomptr%temp(l)
        roomptr%abs_length(l) = (1.8_eb*roomptr%volume(l)) / &
            (roomptr%floor_area + roomptr%depth(l)*(roomptr%cdepth + roomptr%cwidth))
        roomptr%abs_length(u) = (1.8_eb*roomptr%volume(u)) / &
            (roomptr%floor_area + roomptr%depth(u)*(roomptr%cdepth + roomptr%cwidth))
        do iwall = 1, 4
            imap = map(iwall)
            if (radi_verification_flag .and. temperature_wall(imap) /= -1001._eb) then
                twall(imap) = temperature_wall(iwall) + kelvin_c_offset
                emis(imap) = emissivity_wall(iwall)
            else
                twall(imap) = roomptr%t_surfaces(1,iwall)
                emis(imap) = roomptr%eps_w(iwall)
            end if
        end do
        chi10(:) = roomptr%chi10(:)
        nrmfire = 0
        do ifire = 1, n_fires
            fireptr => fireinfo(ifire)
            if (fireptr%room==i) then
                nrmfire = nrmfire + 1
                xrfire(nrmfire) = fireptr%x_position
                yrfire(nrmfire) = fireptr%y_position
                fheight = 0.0_eb ! This is fire radiation at base of the fire
                ! This is fire radiation at 1/3 the height of the fire (bounded by the ceiling height)
                call flame_height (fireptr%qdot_actual,fireptr%firearea,fheight)
                if (fheight+(fireptr%z_position+fireptr%z_offset)>roomptr%cheight) then
                    fheight = roomptr%cheight-(fireptr%z_position+fireptr%z_offset)
                end if
                zrfire(nrmfire) = fireptr%z_position + fireptr%z_offset + fheight/3.0_eb
                qrfire(nrmfire) = fireptr%qdot_radiative
            end if
        end do
        if (.not.black) then
            if (option(fgasabsorb)==off) then
                roomptr%absorb(u) = defabsup
                roomptr%absorb(l) = defabslow
            else
                roomptr%absorb(u) = absorb(i, u)
                roomptr%absorb(l) = absorb(i, l)
            end if
        end if
        rabsorb(1) = roomptr%absorb(u)
        rabsorb(2) = roomptr%absorb(l)
        
        if (nrm1 == 1 .and. sum(chi10(:)) /= 0._eb .or. radi_verification_flag) then 
            call rad10(twall,tg,emis,rabsorb,i,roomptr%cwidth,roomptr%cdepth,roomptr%cheight,roomptr%depth(l), &
                      qrfire,xrfire,yrfire,zrfire,nrmfire,chi10, &
                      qflxw,qlay,mxfires,taufl10,taufu10,firang10,roomptr%rad_qout,black)
        else
            ! limiting the new feature for one compartment only, this restriction will be removed in the future
            call rad4(twall,tg,emis,rabsorb,i,roomptr%cwidth,roomptr%cdepth,roomptr%cheight,roomptr%depth(l), &
                      qrfire,xrfire,yrfire,zrfire,nrmfire, &
                      qflxw,qlay,mxfires,taufl,taufu,firang,roomptr%rad_qout,black)
        end if
        
        do j = 1, nwal
            fluxes_radiation(i,j) = qflxw(map(j))
        end do

        flows_radiation(i,1) = qlay(1)
        flows_radiation(i,2) = qlay(2)
    end do

    return
    end subroutine radiation

! --------------------------- rad4 -------------------------------------------

    subroutine rad4(twall,tlay,emis,absorb,iroom,xroom,yroom,zroom,hlay,qfire,xfire,yfire,zfire,nfire,&
       qflux,qlay,mxfires,taufl,taufu,firang,qout,black)

    !     routine: rad4
    !     purpose: this routine computes the radiative heat flux to the ceiling, upper wall, lower wall and floor due to
    !              a point source fire, emitting absorbing gas layers (u and l) and heat emitting wall segments.
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

    integer, parameter :: u = 1, l = 2, mxroom = 100
    integer :: ipvt(4), iflag(mxroom), iroom, i, j, k, nfire, info, mxfires

    real(eb), intent(in) :: twall(4), tlay(2), emis(4), absorb(2), xroom, yroom, zroom, hlay, qfire(*), xfire(*), yfire(*), zfire(*)
    real(eb), intent(out) :: taufl(mxfires,*), taufu(mxfires,*), firang(4,mxfires)

    real(eb), intent(out) :: qflux(4), qlay(2), qout(4)

    real(eb) :: taul(4,4), tauu(4,4), beam(4,4)
    real(eb) :: area(4), figs(4,4), zz(4), a(4,4), b(4,4), e(4), c(4), rhs(4), dq(4), dqde(4), f14(mxroom)
    real(eb) :: f1d, f4d, dx2, dy2, dz2, x2, y2, dh2, aij, qllay, qulay, ff14

    logical black

    data iflag /mxroom*0/

    if (iflag(iroom)==0) then
        f14(iroom) = rdparfig(xroom,yroom,zroom)
        iflag(iroom) = 1
    end if
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
    if (nfire/=0) then
        call rdftran(mxfires,4,2,absorb,hlay,zz,nfire,zfire,taufu,taufl,black)
    end if

    ! define solid angles for fires
    if (nfire/=0) then
        call rdfang(mxfires,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang)
    end if

    !     note: we want to solve the linear system
    !         a*dq = b*e + c
    !         where a and b are nxn matrices, q, e and c are nr vectors

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

    call rdflux(mxfires,4,2,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

    ! construct right hand side (rhs) of linear system to be solved, i.e. compute b*e - c

    do k = 1, 4
        rhs(k) = ddot(4,b(k,1),4,e(1),1) - c(k)
    end do

    ! solve the linear system

    call dgefa(a,4,4,ipvt,info)
    if (info/=0) then
        write (*,*) '***Error: RAD4 - singular matrix'
        write (iofill,*) '***Error: RAD4 - singular matrix'
        do k = 1, 4
            rhs(k) = 0.0_eb
        end do
    else
        call dgesl(a,4,4,ipvt,rhs,0)
    end if

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

    subroutine rdflux(mxfires,nzone,nup,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl,taufu,firang,nfire,qllay,qulay,c)

    !     routine: rad4
    !     purpose: this routine calculates the 'c' vector in the net radiation equations of seigel and howell and the
    !        heat absorbed by the lower and upper layer fires due to gas layer emission and fires..

    integer, intent(in) :: mxfires, nzone, nfire, nup
    real(eb), intent(in) :: area(*), hlay, tlay(2), zfire(*), qfire(mxfires)
    real(eb), intent(in) :: figs(nzone,*), taul(nzone,*), tauu(nzone,*), taufl(mxfires,*), taufu(mxfires,*), firang(4,mxfires)

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
            qfflux = qfire(ifire)*firang(k,ifire)/(fourpi*area(k))
            c(k) = c(k) + qfflux*taufl(ifire,k)*taufu(ifire,k)
            if (zfire(ifire)>hlay) then
                factu = 1.0_eb - taufu(ifire,k)
                factl = 0.0_eb
            else
                factu = (1.0_eb - taufu(ifire,k))*taufl(ifire,k)
                factl = 1.0_eb - taufl(ifire,k)
            end if
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
            qfflux = qfire(ifire)*firang(k,ifire)/(fourpi*area(k))
            c(k) = c(k) + qfflux*taufl(ifire,k)*taufu(ifire,k)
            if (zfire(ifire)>hlay) then
                factu = 1.0_eb - taufu(ifire,k)
                factl = (1.0_eb - taufl(ifire,k))*taufu(ifire,k)
            else
                factu = 0.0_eb
                factl = 1.0_eb - taufl(ifire,k)
            end if
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

    real(eb), intent(in) :: x, y, z

    real(eb) :: xx, yy, xsq, ysq, f1, f2, f3, f4, f5

    rdparfig = 0.0_eb
    if (z==0.0_eb.or.x==0.0_eb.or.y==0.0_eb) return
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

    hwsum = h**2._eb + w**2._eb
    hwnorm = sqrt(hwsum)
    rhwnorm = 1.0_eb/hwnorm
    f3 = hwnorm*atan(rhwnorm)

    wsum1 = 1.0_eb + w**2._eb
    hsum1 = 1.0_eb + h**2._eb
    hwsum2 = 1.0_eb + hwsum
    f4a = wsum1*hsum1/hwsum2
    f4b = (w**2._eb*hwsum2/wsum1/hwsum)
    f4c = (h**2._eb*hwsum2/hsum1/hwsum)
    f4 = 0.25_eb*(log(f4a)+log(f4b)*w**2._eb+log(f4c)*h**2._eb) 
    rdprpfig = (f1+f2-f3+f4)/(pi*w)
    return
    end function rdprpfig

! --------------------------- getvrel -------------------------------------------

    subroutine getvrel(vrel,v1,vf)

    real(eb), dimension(3), intent(in) :: v1, vf
    real(eb), dimension(3), intent(out) :: vrel

    vrel(1:3) = v1(1:3) - vf(1:3)
    vrel(1:3) = vrel(1:3)/dnrm2(3,vrel,1)
    return
    end subroutine getvrel

! --------------------------- rdfang -------------------------------------------

    subroutine rdfang(mxfires,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang)

    !     routine: rdfang
    !     purpose:

    integer, intent(in) :: mxfires, nfire
    real(eb), intent(in) :: xroom, yroom, zroom, hlay, xfire(*), yfire(*), zfire(*)

    real(eb), intent(out) :: firang(4,mxfires)

    real(eb), dimension(3) :: v1ceil, v2ceil, v3ceil, v4ceil
    real(eb), dimension(3) :: v1floor, v2floor, v3floor, v4floor
    real(eb), dimension(3) :: v1lay, v2lay, v3lay, v4lay
    real(eb), dimension(3) :: vrel1, vrel2, vrel3, vrel4
    real(eb), dimension(3) :: vfire
    real(eb) :: solid_angle1, solid_angle2, solid_angle_layer

    integer :: i

    v1floor = (/0.0_eb,0.0_eb,0.0_eb/)
    v2floor = (/ xroom,0.0_eb,0.0_eb/)
    v3floor = (/ xroom, yroom,0.0_eb/)
    v4floor = (/0.0_eb, yroom,0.0_eb/)

    v1lay = (/0.0_eb,0.0_eb,hlay/)
    v2lay = (/ xroom,0.0_eb,hlay/)
    v3lay = (/ xroom, yroom,hlay/)
    v4lay = (/0.0_eb, yroom,hlay/)

    v1ceil = (/0.0_eb,0.0_eb,zroom/)
    v2ceil = (/ xroom,0.0_eb,zroom/)
    v3ceil = (/ xroom, yroom,zroom/)
    v4ceil = (/0.0_eb, yroom,zroom/)

    do i = 1, nfire
       vfire = (/xfire(i),yfire(i),zfire(i)/)
       call getvrel(vrel1,v1ceil,vfire)
       call getvrel(vrel2,v2ceil,vfire)
       call getvrel(vrel3,v3ceil,vfire)
       call getvrel(vrel4,v4ceil,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang(1,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1floor,vfire)
       call getvrel(vrel2,v2floor,vfire)
       call getvrel(vrel3,v3floor,vfire)
       call getvrel(vrel4,v4floor,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang(4,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1lay,vfire)
       call getvrel(vrel2,v2lay,vfire)
       call getvrel(vrel3,v3lay,vfire)
       call getvrel(vrel4,v4lay,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       solid_angle_layer = solid_angle1 + solid_angle2

        if (zfire(i)<hlay) then
            firang(2,i) = solid_angle_layer - firang(1,i)
            firang(3,i) = fourpi - solid_angle_layer - firang(4,i)
        else
            firang(2,i) = fourpi - solid_angle_layer - firang(1,i)
            firang(3,i) = solid_angle_layer - firang(4,i)
        end if
    end do
    return
   end  subroutine rdfang

! --------------------------- rdftran -------------------------------------------

    subroutine rdftran (mxfires,nzone,nup,absorb,hlay,zz,nfire,zfire,taufu,taufl,black)

    !     routine: rdftran
    !     purpose:

    real(eb), intent(in) :: absorb(*), zz(*), zfire(*)
    integer, intent(in) :: mxfires, nzone, nup, nfire
    logical, intent(in) :: black
    real(eb), intent(out) :: taufu(mxfires,*), taufl(mxfires,*)

    real(eb) :: hlay, beam, beamu, beaml
    integer :: i, j

    if (radi_verification_flag .and. partial_pressure_h2o == 99999._eb .or. partial_pressure_co2 == 99999._eb) then
        do i = 1, nfire
            taufu(:,i) = 0._eb
            taufl(:,i) = 0._eb
        end do
        return
    end if

    do i = 1, nfire
        do j = 1, nup
            if (zfire(i)>hlay) then
                beam = abs(zz(j)-zfire(i))
                taufl(i,j) = 1.0_eb
                if (.not.black) then
                    taufu(i,j) = exp(-absorb(1)*beam)
                else
                    taufu(i,j) = 0.0_eb
                    taufl(i,j) = 0.0_eb
                end if
            else
                beamu = zz(j) - hlay
                beaml = hlay - zfire(i)
                if (.not.black) then
                    taufu(i,j) = exp(-absorb(1)*beamu)
                    taufl(i,j) = exp(-absorb(2)*beaml)
                else
                    taufu(i,j) = 0.0_eb
                    taufl(i,j) = 0.0_eb
                end if
            end if
        end do
        do j = nup + 1, nzone
            if (zfire(i)<=hlay) then
                beam = abs(zz(j)-zfire(i))
                taufu(i,j) = 1.0_eb
                if (.not.black) then
                    taufl(i,j) = exp(-absorb(2)*beam)
                else
                    taufl(i,j) = 0.0_eb
                    taufu(i,j) = 0.0_eb
                end if
            else
                beamu = zfire(i) - hlay
                beaml = hlay - zz(j)
                if (.not.black) then
                    taufu(i,j) = exp(-absorb(1)*beamu)
                    taufl(i,j) = exp(-absorb(2)*beaml)
                else
                    taufu(i,j) = 0.0_eb
                    taufl(i,j) = 0.0_eb
                end if
            end if
        end do
    end do
    return
    end subroutine rdftran

! --------------------------- rdrtran -------------------------------------------

    subroutine rdrtran (nzone,nup,absorb,beam,hlay,zz,tauu,taul,black)

    !     routine: rdftran
    !     purpose:

    integer, intent(in) ::  nup, nzone
    real(eb), intent(in) :: absorb(*), beam(nzone,nzone), zz(*), hlay
    real(eb), intent(out) :: tauu(nzone,nzone), taul(nzone,nzone)

    integer i, j
    real(eb) :: fu, fl
    logical black

    if (radi_verification_flag .and. partial_pressure_h2o == 99999._eb .or. partial_pressure_co2 == 99999._eb) then
        tauu(:,:) = 0._eb
        taul(:,:) = 0._eb
        return
    end if

    ! define upper layer transmission factors
    ! upper to upper

    do i = 1, nup
        do j = i + 1, nup
            if (.not.black) then
                tauu(i,j) = exp(-absorb(1)*beam(i,j))
            else
                tauu(i,j) = 0.0_eb
            end if
            tauu(j,i) = tauu(i,j)
        end do
        if (.not.black) then
            tauu(i,i) = exp(-absorb(1)*beam(i,i))
        else
            tauu(i,i) = 0.0_eb
        end if
    end do

    ! upper to lower and lower to upper

    do i = 1, nup
        do j = nup + 1, nzone
            fu = (zz(i)-hlay)/(zz(i)-zz(j))
            if (.not.black) then
                tauu(i,j) = exp(-absorb(1)*beam(i,j)*fu)
            else
                tauu(i,j) = 0.0_eb
            end if
            tauu(j,i) = tauu(i,j)
        end do
    end do

    ! lower to lower
    do i = nup + 1, nzone
        do j = nup + 1, nzone
            if (.not.black) then
                tauu(i,j) = 1.0_eb
            else
                tauu(i,j) = 0.0_eb
            end if
        end do
    end do

    ! define lower layer transmission factors

    ! lower to lower
    do i = nup + 1, nzone
        do j = i + 1, nzone
            if (.not.black) then
                taul(i,j) = exp(-absorb(2)*beam(i,j))
            else
                taul(i,j) = 0.0_eb
            end if
            taul(j,i) = taul(i,j)
        end do
        if (.not.black) then
            taul(i,i) = exp(-absorb(2)*beam(i,i))
        else
            taul(i,i) = 0.0_eb
        end if
    end do

    ! upper to upper

    do i = 1, nup
        do j = 1, nup
            if (.not.black) then
                taul(i,j) = 1.0_eb
            else
                taul(i,j) = 0.0_eb
            end if
        end do
    end do

    ! upper to loewr and lower to upper

    do i = nup + 1, nzone
        do j = 1, nup
            fl = (hlay-zz(i))/(zz(j)-zz(i))
            if (.not.black) then
                taul(i,j) = exp(-absorb(2)*beam(i,j)*fl)
            else
                taul(i,j) = 0.0_eb
            end if
            taul(j,i) = taul(i,j)
        end do
    end do
    return
    end subroutine rdrtran

! --------------------------- solid_angle_triangle -------------------------------------------

    subroutine solid_angle_triangle(solid_angle,v1,v2,v3)

    real(eb), intent(in), dimension(3) :: v1, v2, v3
    real(eb), intent(out) :: solid_angle

    real(eb) :: vcross(3), num, denom
    ! assuming v1, v2 and v3 are unit vectors
    ! tan(solid_angle/2) = (v1 x v2 . v3)/(1 + v2.v3 + v3.v1 + v1.v2)

    call cross_product(vcross,v1,v2)
    num = ddot(3,vcross,1,v3,1)
    denom = 1.0_eb + ddot(3,v2,1,v3,1) + ddot(3,v3,1,v1,1) + ddot(3,v1,1,v2,1)
    solid_angle = ABS(2.0_eb*atan2(num,denom))

    end subroutine solid_angle_triangle

! --------------------------- cross-product -------------------------------------------

    subroutine cross_product(c,a,b)

! c = a x b

    real(eb), intent(in) :: a(3),b(3)
    real(eb), intent(out) :: c(3)

    c(1) = a(2)*b(3)-a(3)*b(2)
    c(2) = a(3)*b(1)-a(1)*b(3)
    c(3) = a(1)*b(2)-a(2)*b(1)

    end subroutine cross_product


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

    integer, intent(in) :: cmpt, layer

    ! declare parameters
    integer, parameter :: noerr=0, hierr=+1, loerr=-1
    integer, parameter :: co2xsize=11, co2ysize=12, h2oxsize=11, h2oysize=12

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
    !    k = 1/(kelvin-meter) (soot absorption constant)
    !    mwco2, mwh2o = gas molecular weight (kg/gm-mole)
    !    rg = ideal gas constant (atm-m^3/mol-k)

    integer :: xco2, yco2, xh2o, yh2o
    real(eb) :: l, ng, tg, rtv, ag, plg, cplg, tglog, aco2, ah2o, vfs

    ! declare module data

    ! physical constants [mw in kg/mol; rg in m^3-atm/mol-kelvin]

    real(eb), parameter :: mwco2 = 44.0088e-3_eb, mwh2o = 18.0153e-3_eb, rg = 82.0562e-6_eb, k = 1195.5_eb, rhos = 1800.0_eb

    ! log(t) data for co2 [t in k]

    real(eb), parameter, dimension(co2xsize) :: tco2 = (/2.3010_eb, 2.4771_eb, 2.6021_eb, 2.6990_eb, 2.7782_eb, &
        2.8451_eb, 2.9031_eb, 2.9542_eb, 3.0000_eb, 3.3010_eb, 3.4771_eb /)

    ! log(pl) data for co2 [pl in atm-m]

    real(eb), parameter, dimension(co2ysize) :: plco2 = (/-3.0000_eb, -2.6990_eb, -2.3979_eb, -2.0000_eb, -1.6990_eb, &
        -1.3979_eb, -1.0000_eb, -0.6990_eb, -0.3979_eb,  0.0000_eb,  0.3010_eb,  0.6021_eb /)

    ! log(emiss) data for co2 [stored in e(t,pl) format (ascending order by temperature, then by pressure-length)]

    real(eb), parameter, dimension(co2xsize,co2ysize) :: eco2 = reshape( &
        [-1.8508_eb, -1.8416_eb, -1.8508_eb, -1.7799_eb, -1.6990_eb, -1.6799_eb, -1.6904_eb, -1.6990_eb, -1.7399_eb, &
        -2.3706_eb, -2.8996_eb, &
        -1.6990_eb, -1.6799_eb, -1.6904_eb, -1.6308_eb, -1.5498_eb, -1.5302_eb, -1.5302_eb, -1.5498_eb, -1.5800_eb, &
        -2.1002_eb, -2.6108_eb, &
        -1.5406_eb, -1.5200_eb, -1.5498_eb, -1.4895_eb, -1.4401_eb, -1.3904_eb, -1.3904_eb, -1.4101_eb, -1.4202_eb, &
        -1.8894_eb, -2.3002_eb, &
        -1.3799_eb, -1.3298_eb, -1.3497_eb, -1.3298_eb, -1.2700_eb, -1.2403_eb, -1.2403_eb, -1.2503_eb, -1.2700_eb, &
        -1.6596_eb, -2.0400_eb, &
        -1.2403_eb, -1.2000_eb, -1.2403_eb, -1.2104_eb, -1.1599_eb, -1.1403_eb, -1.1302_eb, -1.1403_eb, -1.1500_eb, &
        -1.5200_eb, -1.8894_eb, &
        -1.1403_eb, -1.1002_eb, -1.1403_eb, -1.1203_eb, -1.0799_eb, -1.0400_eb, -1.0301_eb, -1.0301_eb, -1.0600_eb, &
        -1.3799_eb, -1.7305_eb, &
        -1.0400_eb, -0.9914_eb, -1.0200_eb, -1.0200_eb, -0.9706_eb, -0.9547_eb, -0.9431_eb, -0.9355_eb, -0.9431_eb, &
        -1.1599_eb, -1.4802_eb, &
        -0.9914_eb, -0.9431_eb, -0.9547_eb, -0.9508_eb, -0.9136_eb, -0.8827_eb, -0.8666_eb, -0.8539_eb, -0.8601_eb, &
        -1.1002_eb, -1.3706_eb, &
        -0.9355_eb, -0.8697_eb, -0.8928_eb, -0.8827_eb, -0.8477_eb, -0.8097_eb, -0.7932_eb, -0.7852_eb, -0.7932_eb, &
        -1.0000_eb, -1.2700_eb, &
        -0.8762_eb, -0.8013_eb, -0.8097_eb, -0.8013_eb, -0.7645_eb, -0.7352_eb, -0.7100_eb, -0.6990_eb, -0.6990_eb, &
        -0.8962_eb, -1.1331_eb, &
        -0.8297_eb, -0.7496_eb, -0.7645_eb, -0.7472_eb, -0.7055_eb, -0.6696_eb, -0.6421_eb, -0.6326_eb, -0.6402_eb, &
        -0.8097_eb, -1.0301_eb, &
        -0.8013_eb, -0.7144_eb, -0.7144_eb, -0.6840_eb, -0.6478_eb, -0.6108_eb, -0.5884_eb, -0.5817_eb, -0.5817_eb, &
        -0.7352_eb, -0.9431_eb], [co2xsize,co2ysize])

    !log(t) data for h2o [t in k]

    real(eb), parameter, dimension(h2oxsize) :: th2o = (/2.3201_eb, 2.4771_eb, 2.6021_eb, 2.6990_eb, 2.7782_eb, 2.8451_eb, &
        2.9031_eb, 2.9542_eb, 3.0000_eb, 3.3010_eb, 3.4771_eb /)

    ! log(pl) data for h2o [pl in atm-m]

    real(eb), parameter, dimension(h2oysize) :: plh2o = (/-3.0000_eb, -2.6990_eb, -2.3979_eb, -2.0000_eb, -1.6990_eb, -1.3979_eb, &
        -1.0000_eb, -0.6990_eb, -0.3979_eb, 0.0000_eb,  0.3010_eb,  0.6021_eb /)

    ! log(emiss) data for h2o [stored in e(t,pl) format (ascending order by temperature, then by pressure-length)]

    real(eb), parameter, dimension(h2oxsize,h2oysize) :: eh2o = reshape( &
        [-1.1500_eb, -1.5200_eb, -1.7496_eb, -1.8996_eb, -2.0000_eb, -2.1002_eb, -2.1898_eb, -2.2798_eb, -2.3706_eb, &
        -3.0555_eb, -3.4437_eb, &
        -1.0200_eb, -1.3298_eb, -1.5302_eb, -1.6596_eb, -1.7595_eb, -1.8416_eb, -1.9208_eb, -2.0000_eb, -2.0799_eb, &
        -2.7496_eb, -3.1871_eb, &
        -0.8962_eb, -1.1701_eb, -1.3242_eb, -1.4597_eb, -1.5406_eb, -1.6003_eb, -1.6596_eb, -1.7305_eb, -1.7905_eb, &
        -2.4202_eb, -2.8794_eb, &
        -0.7696_eb, -1.0000_eb, -1.1302_eb, -1.2204_eb, -1.3002_eb, -1.3497_eb, -1.4001_eb, -1.4401_eb, -1.4802_eb, &
        -1.9914_eb, -2.5200_eb, &
        -0.6402_eb, -0.8729_eb, -0.9957_eb, -1.0799_eb, -1.1302_eb, -1.1701_eb, -1.2104_eb, -1.2503_eb, -1.2899_eb, &
        -1.6904_eb, -2.1500_eb, &
        -0.5884_eb, -0.7645_eb, -0.8729_eb, -0.9355_eb, -0.9788_eb, -1.0200_eb, -1.0400_eb, -1.0701_eb, -1.1002_eb, &
        -1.4101_eb, -1.8210_eb, &
        -0.5003_eb, -0.6556_eb, -0.7258_eb, -0.7545_eb, -0.7932_eb, -0.8153_eb, -0.8447_eb, -0.8665_eb, -0.8894_eb, &
        -1.0799_eb, -1.4401_eb, &
        -0.4437_eb, -0.5670_eb, -0.6271_eb, -0.6402_eb, -0.6517_eb, -0.6696_eb, -0.6861_eb, -0.6990_eb, -0.7190_eb, &
        -0.8729_eb, -1.1403_eb, &
        -0.3936_eb, -0.5086_eb, -0.5302_eb, -0.5376_eb, -0.5482_eb, -0.5528_eb, -0.5670_eb, -0.5719_eb, -0.5817_eb, &
        -0.7122_eb, -0.9431_eb, &
        -0.3458_eb, -0.4295_eb, -0.4401_eb, -0.4365_eb, -0.4401_eb, -0.4413_eb, -0.4510_eb, -0.4535_eb, -0.4584_eb, &
        -0.5376_eb, -0.7144_eb, &
        -0.2958_eb, -0.3686_eb, -0.3686_eb, -0.3645_eb, -0.3645_eb, -0.3686_eb, -0.3706_eb, -0.3757_eb, -0.3757_eb, &
        -0.4510_eb, -0.5952_eb, &
        -0.2620_eb, -0.3307_eb, -0.3233_eb, -0.3045_eb, -0.3010_eb, -0.3045_eb, -0.3045_eb, -0.3054_eb, -0.3080_eb, &
        -0.3605_eb, -0.5086_eb], [h2oxsize,h2oysize] )
    type(room_type), pointer :: roomptr

    roomptr => roominfo(cmpt)
    ! layer-specific factors
    tg = roomptr%temp(layer)
    rtv = (rg*tg)/roomptr%volume(layer)
    l = roomptr%abs_length(layer)

    ag = 0.0_eb

    ! absorbance for co2
    ng = roomptr%species_mass(layer,co2)/mwco2
    plg = ng*rtv*l
    if (plg>0.0_eb) then
        cplg = log10(plg)
        tglog = log10(tg)
        call linterp(co2xsize, co2ysize, tco2, plco2, eco2, tglog, cplg, aco2, xco2, yco2)
        ag = ag + 0.50_eb*10.0_eb**aco2
    else
        aco2 = 0.0_eb
    end if

    ! absorbance for h2o
    ng = roomptr%species_mass(layer,h2o)/mwh2o
    plg = ng*rtv*l
    if (plg>0.0_eb) then
        cplg = log10(plg)
        tglog = log10(tg)
        call linterp(h2oxsize, h2oysize, th2o, plh2o, eh2o, tglog, cplg, ah2o, xh2o, yh2o)
        ag = ag + 10.0_eb**ah2o
    else
        ah2o = 0.0_eb
    end if

    ! total absorbance
    vfs = roomptr%species_mass(layer,soot)/(roomptr%volume(layer)*rhos)
    absorb = max(k*vfs*tg - log(1.0_eb-ag)/l,0.01_eb)
    
    if (radi_verification_flag .and. partial_pressure_h2o == 0._eb .and. partial_pressure_co2 == 0._eb) absorb = 0._eb

    return

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

    integer, intent(in) :: xdim, ydim
    real(eb), intent(in) :: x(xdim), y(ydim), z(xdim,ydim)

    real(eb), intent(inout) :: xval, yval

    integer, intent(out) :: xerr, yerr
    real(eb), intent(out) :: zval

    integer, parameter :: noerr=0, hierr=+1, loerr=-1
    integer :: count, i, j

    ! find the value of i such that x(1) <= xval <= x(xdim). if xval is outside that range, set it to the closest legal
    ! value and set the error value, as appropriate.

    ! check the special case of xval < x(1)

    if (xval < x(1)) then
        xerr = loerr
        xval = x(1)
        i = 1

        ! check the special case of xval > x(xdim)

    else if (xval > x(xdim)) then
        xerr = hierr
        xval = x(xdim)
        i = xdim-1

        ! check the cases where x(1) <= xval < x(xdim)

    else
        xerr = noerr
        do count=2,xdim
            if (xval < x(count)) then
                i = count - 1
                go to 20
            end if
        end do
        ! then xval = x(xdim)
        i = xdim-1
20      continue
    end if

    ! check the special case of yval < y(1)

    if (yval < y(1)) then
        yerr = loerr
        yval = y(1)
        j = 1

        ! check the special case of yval > y(ydim)

    else if (yval > y(ydim)) then
        yerr = hierr
        yval = y(ydim)
        j = ydim-1

        ! check the cases of y(1) <= yval < y(ydim)

    else
        yerr = noerr
        do count=2,ydim
            if (yval < y(count)) then
                j = count - 1
                go to 40
            end if
        end do

        ! then yval = y(ydim)

        j = ydim-1
40      continue
    end if

    ! interpolate a value for f(x,y)

    zval = z(i,j)*(x(i+1)-xval)*(y(j+1)-yval)
    zval = zval + z(i+1,j)*(xval - x(i))*(y(j+1)-yval)
    zval = zval + z(i,j+1)*(x(i+1)-xval)*(yval - y(j))
    zval = zval + z(i+1,j+1)*(xval - x(i))*(yval-y(j))
    zval = zval/((x(i+1)-x(i))*(y(j+1)-y(j)))

    return
    end subroutine linterp

! --------------------------- rad10 ---------------------------------------------

    subroutine rad10(twall,tlay,emis,absorb,iroom,xroom,yroom,zroom,hlay, &
                    qfire,xfire,yfire,zfire,nfire,chi10, &
                    qflux,qlay,mxfires,taufl10,taufu10,firang10,qout,black)

    !     routine: rad10
    !     purpose: Similiar to rad4 except that rad10 has capabilities to account for radiatoin loss due to any openings

    integer, parameter :: u = 1, l = 2, mxroom = 100
    integer :: ipvt(10), iflag(mxroom), iroom, i, j, k, nfire, info, mxfires

    real(eb), intent(in) :: twall(4), tlay(2), emis(4), absorb(2), xroom, yroom, zroom, hlay, qfire(*), xfire(*), yfire(*), zfire(*)
    real(eb), intent(in) :: chi10(10)
    real(eb), intent(out) :: taufl10(mxfires,*), taufu10(mxfires,*), firang10(10,mxfires)
    real(eb), intent(out) :: qflux(4), qlay(2), qout(4)

    real(eb) :: taul(10,10), tauu(10,10), beam(10,10)
    real(eb) :: area(10), figs(10,10), zz(10), a(10,10), b(10,10), e(10), c(10), rhs(10), dq(10), dqde(10), f14(mxroom)
    real(eb) :: aij, qllay, qulay
    
    real(eb) :: twall10(10), emis10(10), qout10(10), qflux10(10)
    real(eb) :: radi_gain_gas(10), radi_loss_gas(10), &
                radi_gain_gas_fire(10), radi_loss_gas_fire(10), radi_gain_wall_fire(10), radi_loss_wall_fire(10), &
                radi_gain_gas_wall(10), radi_loss_gas_wall(10), radi_loss_wall_wall(10), radi_gain_wall_wall(10)
    real(eb) :: summataion(10)
    

    logical black

    data iflag /mxroom*0/

    if (iflag(iroom)==0) then
        f14(iroom) = rdparfig(xroom,yroom,zroom)
        iflag(iroom) = 1
    end if
    
    ! obtain area, zz, twall10 and emis10
    call rad10_initalization(xroom,yroom,zroom,hlay,twall,emis,area,zz,twall10,emis10)
    
    ! determine view factors for 10-wall model
    call ten_view_factors(area,xroom,yroom,zroom,hlay,figs)
    
    ! evaluate center-to-center path-length
    call ten_beam(zz,xroom,yroom,beam)
    
    ! define transmission factors for medium
    call rdrtran(10,5,absorb,beam,hlay,zz,tauu,taul,black)

    ! define transmission factors for fires
    if (nfire/=0) then
        call rdftran(mxfires,10,5,absorb,hlay,zz,nfire,zfire,taufu10,taufl10,black)
    end if
    
    ! define solid angles for fires
    if (nfire/=0) then
        call rdfang10(mxfires,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang10)
    end if
    
    summataion(:) = 0._eb
    do i = 1, 10
        do j = 1, 10
            figs(i,j) = figs(i,j) * (1._eb-chi10(i)) * (1._eb-chi10(j))
            summataion(i) = summataion(i) + figs(i,j)
        end do
        firang10(i,:) = firang10(i,:) * (1._eb-chi10(i))
    end do

    !     note: we want to solve the linear system
    !         a*dq = b*e + c
    !         where a and b are nxn matrices, q, e and c are nr vectors
    
    ! define e vector
    do i = 1, 10
        e(i) = sigma*twall10(i)**4
    end do

    ! define c vector
    call rdflux10(mxfires,10,5,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl10,taufu10,firang10,nfire,qllay,qulay,c, &
                  chi10, &
                  radi_gain_gas,radi_loss_gas,radi_gain_gas_fire,radi_loss_gas_fire, &
                  radi_gain_wall_fire,radi_loss_wall_fire)
        
    ! define 'a' and 'b' coefficient matrix
    do k = 1, 10
        do j = 1, 10
            aij = figs(k,j)*taul(k,j)*tauu(k,j)
            a(k,j) = -aij*(1.0_eb-emis10(j))
            b(k,j) = -aij
        end do
        a(k,k) = a(k,k) + summataion(k)
        b(k,k) = b(k,k) + summataion(k)
    end do
    
    ! construct right hand side (rhs) of linear system to be solved, i.e. compute b*e - c
    do k = 1, 10
        rhs(k) = ddot(10,b(k,1),10,e(1),1) - c(k)
    end do

    ! solve the linear system
    call dgefa(a,10,10,ipvt,info)
    if (info/=0) then
        write (*,*) '***Error: RAD10 - singular matrix'
        write (iofill,*) '***Error: RAD10 - singular matrix'
        do k = 1, 10
            rhs(k) = 0.0_eb
        end do
    else
        call dgesl(a,10,10,ipvt,rhs,0)
    end if

    ! note: each row k of the a matrix, as defined by seigal and howell was divided by emis(k) (in order to insure
    !       that this new 'a' was diagonally dominant.  now we have to multiply the solution to the modified problem
    !       by emis(i) to get the answer to the original problem
    do k = 1, 10
        dqde(k) = rhs(k)
        qout10(k) = e(k) - (1.0_eb - emis10(k))*dqde(k)
        dq(k) = rhs(k)*emis10(k)
    end do

    ! take solution and compute energy gain or loss to each panel and each layer.  also compute fluxes.  change sign so that
    ! a postive flux means that heat is flowing to the wall
    do i = 1, 10
        qflux10(i) = -dq(i)
    end do

    ! compute radiation absorbed by each layer
    call rabs10(10,5,e,dqde,emis10,area,figs,tauu,taul,chi10,radi_gain_wall_wall,radi_loss_wall_wall, &
                radi_gain_gas_wall,radi_loss_gas_wall,qllay,qulay)
    
    qlay(u) = qulay
    qlay(l) = qllay    
    
    ! obtain data for 4-wall model based on 10-wall model
    qflux(:) = 0._eb
    qout(:) = 0._eb
    
    do i = 1, 10
        if (i == 1) then
            qflux(1) = qflux10(i)
            qout(1) = qout10(i)
        else if (i > 1 .and. i < 6) then
            qflux(2) = qflux(2) + qflux10(i)/4._eb
            qout(2) = qout(2) + qout10(i)/4._eb
        else if (i > 5 .and. i < 10) then
            qflux(3) = qflux(3) + qflux10(i)/4._eb
            qout(3) = qout(3) + qout10(i)/4._eb
        else
            qflux(4) = qflux10(i)
            qout(4) = qout10(i)
        end if
    end do

    return
    end subroutine rad10

! --------------------------- rad10_initalization -------------------------------

    subroutine rad10_initalization(xroom,yroom,zroom,hlay,twall,emis,area,zz,twall10,emis10)

    !     purpose: initalization area, zz, and other parameters

    real(eb), intent(in)  :: xroom,yroom,zroom,hlay,twall(4),emis(4)
    real(eb), intent(out) :: area(10),zz(10),twall10(10),emis10(10)
    
    integer :: i

    ! define areas
    area(1) = xroom*yroom
    area(2) = (zroom-hlay)*xroom
    area(3) = (zroom-hlay)*yroom
    area(4) = area(2)
    area(5) = area(3)
    area(6) = hlay*xroom
    area(7) = hlay*yroom
    area(8) = area(6)
    area(9) = area(7)
    area(10) = area(1)
    
    ! define zz 
    zz(1) = zroom
    zz(2) = (hlay+zroom)*0.50_eb
    zz(3) = (hlay+zroom)*0.50_eb
    zz(4) = (hlay+zroom)*0.50_eb
    zz(5) = (hlay+zroom)*0.50_eb
    zz(6) = hlay*0.50_eb
    zz(7) = hlay*0.50_eb
    zz(8) = hlay*0.50_eb
    zz(9) = hlay*0.50_eb
    zz(10) = 0.0_eb

    do i = 1, 10
        if (i == 1) then
            twall10(i) = twall(1)
            emis10(i) = emis(1)
        else if (i > 1 .and. i < 6) then
            twall10(i) = twall(2)
            emis10(i) = emis(2)
        else if (i > 5 .and. i < 10) then
            twall10(i) = twall(3)
            emis10(i) = emis(3)
        else
            twall10(i) = twall(4)
            emis10(i) = emis(4)
        end if
    end do 

    return
    end subroutine rad10_initalization

! --------------------------- ten_view_factors ----------------------------------

    subroutine ten_view_factors(area,xroom,yroom,zroom,hlay,figs)

    !     purpose: to determine 10 view factors

    real(eb), intent(in)  :: area(10),xroom,yroom,zroom,hlay
    real(eb), intent(out) :: figs(10,10)
    
    real(eb) :: f1d, ff14, f4d, f1x, f1y, f10x, f10y
    
    ! define configuration factors
    f1d = rdparfig(xroom,yroom,zroom-hlay)
    ff14 = rdparfig(xroom,yroom,zroom)
    f4d = rdparfig(xroom,yroom,hlay)
    f1x = rdprpfig(zroom-hlay,yroom,xroom)
    f1y = rdprpfig(zroom-hlay,xroom,yroom)
    f10x = rdprpfig(xroom,yroom,hlay)
    f10y = rdprpfig(yroom,xroom,hlay)

    figs(1,1)   = 0.0_eb
    figs(1,2)   = f1y
    figs(1,3)   = f1x
    figs(1,4)   = figs(1,2)
    figs(1,5)   = figs(1,3)
    figs(1,6)   = 1._eb/area(1)*((area(2)+area(6))*rdprpfig(yroom,xroom,zroom)-area(1)*figs(1,2))
    figs(1,7)   = 1._eb/area(1)*((area(3)+area(7))*rdprpfig(xroom,yroom,zroom)-area(1)*figs(1,3))
    figs(1,8)   = figs(1,6)
    figs(1,9)   = figs(1,7)
    figs(1,10)  = ff14
    
    figs(2,1)   = area(1)/area(2)*figs(1,2)
    figs(2,2)   = 0._eb
    figs(2,3)   = rdprpfig(yroom,zroom-hlay,xroom)
    figs(2,4)   = rdparfig(zroom-hlay,xroom,yroom)
    figs(2,5)   = figs(2,3)
    figs(2,6)   = 0._eb
    figs(2,7)   = 1._eb/(2._eb*area(2))*((area(2)+area(6))*rdprpfig(yroom,zroom,xroom)- &
                                          area(2)*rdprpfig(yroom,zroom-hlay,xroom)-area(6)*rdprpfig(yroom,hlay,xroom))
    figs(2,8)   = 1._eb/(2._eb*area(2))*((area(2)+area(6))*rdparfig(xroom,zroom,yroom)- &
                                          area(2)*rdparfig(xroom,zroom-hlay,yroom)-area(6)*rdparfig(xroom,hlay,yroom))
    figs(2,9)   = figs(2,7)
    figs(2,10)  = 1._eb/area(2)*((area(2)+area(6))*rdprpfig(yroom,xroom,zroom)-area(6)*rdprpfig(yroom,xroom,hlay))
    
    figs(3,1)   = area(1)/area(3)*figs(1,3)
    figs(3,2)   = area(2)/area(3)*figs(2,3)
    figs(3,3)   = 0._eb
    figs(3,4)   = figs(3,2)
    figs(3,5)   = rdparfig(zroom-hlay,yroom,xroom)
    figs(3,6)   = 1._eb/(2._eb*area(3))*((area(3)+area(7))*rdprpfig(xroom,zroom,yroom)- &
                                          area(3)*rdprpfig(xroom,zroom-hlay,yroom)-area(7)*rdprpfig(xroom,hlay,yroom))
    figs(3,7)   = 0._eb
    figs(3,8)   = figs(3,6)
    figs(3,9)   = 1._eb/(2._eb*area(3))*((area(3)+area(7))*rdparfig(yroom,zroom,xroom)- &
                                          area(3)*rdparfig(yroom,zroom-hlay,xroom)-area(7)*rdparfig(yroom,hlay,xroom))
    figs(3,10)  = 1._eb/area(3)*((area(3)+area(7))*rdprpfig(xroom,yroom,zroom)-area(7)*rdprpfig(xroom,yroom,hlay))
    
    figs(4,1)   = area(2)/area(4)*figs(2,1)
    figs(4,2)   = area(2)/area(4)*figs(2,4)
    figs(4,3)   = area(2)/area(4)*figs(2,5)
    figs(4,4)   = 0._eb
    figs(4,5)   = area(2)/area(4)*figs(2,3)
    figs(4,6)   = area(2)/area(4)*figs(2,8)
    figs(4,7)   = area(2)/area(4)*figs(2,9)
    figs(4,8)   = 0._eb
    figs(4,9)   = area(2)/area(4)*figs(2,7)
    figs(4,10)  = area(2)/area(4)*figs(2,10) 
    
    figs(5,1)   = area(3)/area(5)*figs(3,1)
    figs(5,2)   = area(3)/area(5)*figs(3,4)
    figs(5,3)   = area(3)/area(5)*figs(3,5)
    figs(5,4)   = area(3)/area(5)*figs(3,2)
    figs(5,5)   = 0._eb
    figs(5,6)   = area(3)/area(5)*figs(3,8)
    figs(5,7)   = area(3)/area(5)*figs(3,9)
    figs(5,8)   = area(3)/area(5)*figs(5,6)
    figs(5,9)   = 0._eb
    figs(5,10)  = area(3)/area(5)*figs(3,10)
    
    figs(6,1)   = 1._eb/area(6)*((area(2)+area(6))*rdprpfig(yroom,xroom,zroom)-area(2)*rdprpfig(yroom,xroom,zroom-hlay))
    figs(6,2)   = 0._eb
    figs(6,3)   = area(3)/area(6)*figs(3,6)
    figs(6,4)   = area(4)/area(6)*figs(4,6)
    figs(6,5)   = area(5)/area(6)*figs(5,6)
    figs(6,6)   = 0._eb
    figs(6,7)   = rdprpfig(yroom,hlay,xroom)
    figs(6,8)   = rdparfig(hlay,xroom,yroom)
    figs(6,9)   = figs(6,7)
    figs(6,10)  = f10y
    
    figs(7,1)   = 1._eb/area(7)*((area(3)+area(7))*rdprpfig(xroom,yroom,zroom)-area(3)*rdprpfig(xroom,yroom,zroom-hlay))
    figs(7,2)   = area(2)/area(7)*figs(2,7)
    figs(7,3)   = 0._eb
    figs(7,4)   = area(4)/area(7)*figs(4,7)
    figs(7,5)   = area(5)/area(7)*figs(5,7)
    figs(7,6)   = area(6)/area(7)*figs(6,7)
    figs(7,7)   = 0._eb
    figs(7,8)   = figs(7,6)
    figs(7,9)   = rdparfig(yroom,hlay,xroom)
    figs(7,10)  = f10x
    
    figs(8,1)   = area(1)/area(8)*figs(1,8)
    figs(8,2)   = area(2)/area(8)*figs(2,8)
    figs(8,3)   = area(3)/area(8)*figs(3,8)
    figs(8,4)   = 0._eb
    figs(8,5)   = area(5)/area(8)*figs(5,8)
    figs(8,6)   = area(6)/area(8)*figs(6,8)
    figs(8,7)   = area(7)/area(8)*figs(7,8)
    figs(8,8)   = 0._eb
    figs(8,9)   = figs(8,7)
    figs(8,10)  = area(6)/area(8)*figs(6,10)
    
    figs(9,1)   = area(1)/area(9)*figs(1,9)
    figs(9,2)   = area(2)/area(9)*figs(2,9)
    figs(9,3)   = area(3)/area(9)*figs(3,9)
    figs(9,4)   = area(4)/area(9)*figs(4,9)
    figs(9,5)   = 0._eb
    figs(9,6)   = area(6)/area(9)*figs(6,9)
    figs(9,7)   = area(7)/area(9)*figs(7,9)
    figs(9,8)   = area(8)/area(9)*figs(8,9)
    figs(9,9)   = 0._eb
    figs(9,10)  = area(7)/area(9)*figs(7,10)
    
    figs(10,1)   = area(1)/area(10)*figs(1,10)
    figs(10,2)   = area(2)/area(10)*figs(2,10)
    figs(10,3)   = area(3)/area(10)*figs(3,10)
    figs(10,4)   = area(4)/area(10)*figs(4,10)
    figs(10,5)   = area(5)/area(10)*figs(5,10)
    figs(10,6)   = area(6)/area(10)*figs(6,10)
    figs(10,7)   = area(7)/area(10)*figs(7,10)
    figs(10,8)   = area(8)/area(10)*figs(8,10)
    figs(10,9)   = area(9)/area(10)*figs(9,10)
    figs(10,10)  = 0._eb

    return
    end subroutine ten_view_factors

! --------------------------- ten_beam ------------------------------------------

    subroutine ten_beam(zz,xroom,yroom,beam)

    !     purpose: to determine 10 view factors

    real(eb), intent(in)  :: zz(10),xroom,yroom
    real(eb), intent(out) :: beam(10,10)
    
    real(eb) :: dx2, dy2, dz1, dz2, dz3, dz4, x2, y2

    ! define transmission factors for surfaces, but first define beam lengths
    dz1 = (zz(1)-zz(2))**2
    dz2 = (zz(1)-zz(6))**2
    dz3 = (zz(2)-zz(6))**2
    dz4 = (zz(6)-zz(10))**2
    dx2 = (xroom*0.50_eb)**2
    dy2 = (yroom*0.50_eb)**2
    x2 = xroom**2
    y2 = yroom**2

    beam(1,1) = 0.0_eb
    beam(1,2) = sqrt(dy2+dz1)
    beam(1,3) = sqrt(dx2+dz1)
    beam(1,4) = beam(1,2)
    beam(1,5) = beam(1,3)
    beam(1,6) = sqrt(dy2+dz2)
    beam(1,7) = sqrt(dx2+dz2)
    beam(1,8) = beam(1,6)
    beam(1,9) = beam(1,7)
    beam(1,10) = zz(1)

    beam(2,1) = beam(1,2)
    beam(2,2) = 0.0_eb
    beam(2,3) = sqrt(dx2+dy2)
    beam(2,4) = sqrt(y2)
    beam(2,5) = beam(2,3)
    beam(2,6) = sqrt(dz3)
    beam(2,7) = sqrt(dz3+(dx2+dy2))
    beam(2,8) = sqrt(dz3+y2)
    beam(2,9) = beam(2,7)
    beam(2,10) = sqrt(zz(2)**2+dy2)

    beam(3,1) = beam(1,3)
    beam(3,2) = beam(2,3)
    beam(3,3) = 0.0_eb
    beam(3,4) = beam(3,2)
    beam(3,5) = sqrt(x2)
    beam(3,6) = sqrt(dz3+(dx2+dy2))
    beam(3,7) = sqrt(dz3)
    beam(3,8) = beam(3,6)
    beam(3,9) = sqrt(dz3+x2)
    beam(3,10) = sqrt(zz(2)**2+dx2)

    beam(4,1) = beam(1,4)
    beam(4,2) = beam(2,4)
    beam(4,3) = beam(3,4)
    beam(4,4) = 0.0_eb
    beam(4,5) = beam(4,3)
    beam(4,6) = sqrt(dz3+y2)
    beam(4,7) = sqrt(dz3+(dx2+dy2))
    beam(4,8) = sqrt(dz3)
    beam(4,9) = beam(4,7)
    beam(4,10) = sqrt(zz(2)**2+dy2)

    beam(5,1) = beam(1,5)
    beam(5,2) = beam(2,5)
    beam(5,3) = beam(3,5)
    beam(5,4) = beam(4,5)
    beam(5,5) = 0._eb
    beam(5,6) = sqrt(dz3+sqrt(dx2+dy2)**2)
    beam(5,7) = sqrt(dz3+x2)
    beam(5,8) = beam(5,6)
    beam(5,9) = sqrt(dz3)
    beam(5,10) = sqrt(zz(2)**2+dx2)

    beam(6,1) = beam(1,6)
    beam(6,2) = beam(2,6)
    beam(6,3) = beam(3,6)
    beam(6,4) = beam(4,6)
    beam(6,5) = beam(5,6)
    beam(6,6) = 0._eb
    beam(6,7) = sqrt(dx2+dy2)
    beam(6,8) = sqrt(y2)
    beam(6,9) = beam(6,7)
    beam(6,10) = sqrt(dz4+dy2)

    beam(7,1) = beam(1,7)
    beam(7,2) = beam(2,7)
    beam(7,3) = beam(3,7)
    beam(7,4) = beam(4,7)
    beam(7,5) = beam(5,7)
    beam(7,6) = beam(6,7)
    beam(7,7) = 0._eb
    beam(7,8) = beam(7,6)
    beam(7,9) = sqrt(x2)
    beam(7,10) = sqrt(dz4+dx2)

    beam(8,1) = beam(1,8)
    beam(8,2) = beam(2,8)
    beam(8,3) = beam(3,8)
    beam(8,4) = beam(4,8)
    beam(8,5) = beam(5,8)
    beam(8,6) = beam(6,8)
    beam(8,7) = beam(7,8)
    beam(8,8) = 0._eb
    beam(8,9) = beam(8,7)
    beam(8,10) = sqrt(dz4+dy2)

    beam(9,1) = beam(1,9)
    beam(9,2) = beam(2,9)
    beam(9,3) = beam(3,9)
    beam(9,4) = beam(4,9)
    beam(9,5) = beam(5,9)
    beam(9,6) = beam(6,9)
    beam(9,7) = beam(7,9)
    beam(9,8) = beam(8,9)
    beam(9,9) = 0._eb
    beam(9,10) = sqrt(dz4+dx2)

    beam(10,1) = beam(1,10)
    beam(10,2) = beam(2,10)
    beam(10,3) = beam(3,10)
    beam(10,4) = beam(4,10)
    beam(10,5) = beam(5,10)
    beam(10,6) = beam(6,10)
    beam(10,7) = beam(7,10)
    beam(10,8) = beam(8,10)
    beam(10,9) = beam(9,10)
    beam(10,10) = 0._eb

    return
    end subroutine ten_beam

! --------------------------- rdfang10 ------------------------------------------

    subroutine rdfang10(mxfires,xroom,yroom,zroom,hlay,nfire,xfire,yfire,zfire,firang10)

    !     routine: rdfang
    !     purpose:

    integer, intent(in) :: mxfires, nfire
    real(eb), intent(in) :: xroom, yroom, zroom, hlay, xfire(*), yfire(*), zfire(*)

    real(eb), intent(out) :: firang10(10,mxfires)

    real(eb), dimension(3) :: v1_1, v2_1, v3_1, v4_1
    real(eb), dimension(3) :: v1_2, v2_2, v3_2, v4_2
    real(eb), dimension(3) :: v1_3, v2_3, v3_3, v4_3
    real(eb), dimension(3) :: v1_4, v2_4, v3_4, v4_4
    real(eb), dimension(3) :: v1_5, v2_5, v3_5, v4_5
    real(eb), dimension(3) :: v1_6, v2_6, v3_6, v4_6
    real(eb), dimension(3) :: v1_7, v2_7, v3_7, v4_7
    real(eb), dimension(3) :: v1_8, v2_8, v3_8, v4_8
    real(eb), dimension(3) :: v1_9, v2_9, v3_9, v4_9
    real(eb), dimension(3) :: v1_10, v2_10, v3_10, v4_10
    real(eb), dimension(3) :: vrel1, vrel2, vrel3, vrel4
    real(eb), dimension(3) :: vfire
    real(eb) :: solid_angle1, solid_angle2

    integer :: i, j
    real(eb) :: temp

    v1_1 = (/0.0_eb,0.0_eb,zroom/)
    v2_1 = (/ xroom,0.0_eb,zroom/)
    v3_1 = (/ xroom, yroom,zroom/)
    v4_1 = (/0.0_eb, yroom,zroom/)

    v1_2 = (/0.0_eb,0.0_eb,hlay/)
    v2_2 = (/ xroom,0.0_eb,hlay/)
    v3_2 = (/ xroom,0.0_eb,zroom/)
    v4_2 = (/0.0_eb,0.0_eb,zroom/)

    v1_3 = (/ xroom,0.0_eb,hlay/)
    v2_3 = (/ xroom, yroom,hlay/)
    v3_3 = (/ xroom, yroom,zroom/)
    v4_3 = (/ xroom,0.0_eb,zroom/)

    v1_4 = (/ xroom, yroom,hlay/)
    v2_4 = (/0.0_eb, yroom,hlay/)
    v3_4 = (/0.0_eb, yroom,zroom/)
    v4_4 = (/ xroom, yroom,zroom/)

    v1_5 = (/0.0_eb, yroom,hlay/)
    v2_5 = (/0.0_eb,0.0_eb,hlay/)
    v3_5 = (/0.0_eb,0.0_eb,zroom/)
    v4_5 = (/0.0_eb, yroom,zroom/)

    v1_6 = (/0.0_eb,0.0_eb,0.0_eb/)
    v2_6 = (/ xroom,0.0_eb,0.0_eb/)
    v3_6 = (/ xroom,0.0_eb,hlay/)
    v4_6 = (/0.0_eb,0.0_eb,hlay/)

    v1_7 = (/ xroom,0.0_eb,0.0_eb/)
    v2_7 = (/ xroom, yroom,0.0_eb/)
    v3_7 = (/ xroom, yroom,hlay/)
    v4_7 = (/ xroom,0.0_eb,hlay/)

    v1_8 = (/ xroom, yroom,0.0_eb/)
    v2_8 = (/0.0_eb, yroom,0.0_eb/)
    v3_8 = (/0.0_eb, yroom,hlay/)
    v4_8 = (/ xroom, yroom,hlay/)

    v1_9 = (/0.0_eb, yroom,0.0_eb/)
    v2_9 = (/0.0_eb,0.0_eb,0.0_eb/)
    v3_9 = (/0.0_eb,0.0_eb,hlay/)
    v4_9 = (/0.0_eb, yroom,hlay/)

    v1_10 = (/0.0_eb,0.0_eb,0.0_eb/)
    v2_10 = (/ xroom,0.0_eb,0.0_eb/)
    v3_10 = (/ xroom, yroom,0.0_eb/)
    v4_10 = (/0.0_eb, yroom,0.0_eb/)

    do i = 1, nfire
       vfire = (/xfire(i),yfire(i),zfire(i)/)
       do j = 1, 3
           if (j == 1) then
               temp = xroom
           else if (j == 2) then
               temp = yroom
           else
               temp = zroom
           end if
           if (vfire(j) == 0._eb) then
               vfire(j) = vfire(j) + 0.0001
           else if  (vfire(1) == temp) then
               vfire(j) = vfire(j) - 0.0001
           end if
       end do
       call getvrel(vrel1,v1_1,vfire)
       call getvrel(vrel2,v2_1,vfire)
       call getvrel(vrel3,v3_1,vfire)
       call getvrel(vrel4,v4_1,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(1,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_2,vfire)
       call getvrel(vrel2,v2_2,vfire)
       call getvrel(vrel3,v3_2,vfire)
       call getvrel(vrel4,v4_2,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(2,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_3,vfire)
       call getvrel(vrel2,v2_3,vfire)
       call getvrel(vrel3,v3_3,vfire)
       call getvrel(vrel4,v4_3,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(3,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_4,vfire)
       call getvrel(vrel2,v2_4,vfire)
       call getvrel(vrel3,v3_4,vfire)
       call getvrel(vrel4,v4_4,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(4,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_5,vfire)
       call getvrel(vrel2,v2_5,vfire)
       call getvrel(vrel3,v3_5,vfire)
       call getvrel(vrel4,v4_5,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(5,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_6,vfire)
       call getvrel(vrel2,v2_6,vfire)
       call getvrel(vrel3,v3_6,vfire)
       call getvrel(vrel4,v4_6,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(6,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_7,vfire)
       call getvrel(vrel2,v2_7,vfire)
       call getvrel(vrel3,v3_7,vfire)
       call getvrel(vrel4,v4_7,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(7,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_8,vfire)
       call getvrel(vrel2,v2_8,vfire)
       call getvrel(vrel3,v3_8,vfire)
       call getvrel(vrel4,v4_8,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(8,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_9,vfire)
       call getvrel(vrel2,v2_9,vfire)
       call getvrel(vrel3,v3_9,vfire)
       call getvrel(vrel4,v4_9,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(9,i) = solid_angle1 + solid_angle2

       call getvrel(vrel1,v1_10,vfire)
       call getvrel(vrel2,v2_10,vfire)
       call getvrel(vrel3,v3_10,vfire)
       call getvrel(vrel4,v4_10,vfire)
       call solid_angle_triangle(solid_angle1,vrel1,vrel2,vrel3)
       call solid_angle_triangle(solid_angle2,vrel1,vrel3,vrel4)
       firang10(10,i) = solid_angle1 + solid_angle2
    end do
    
   return
   end  subroutine rdfang10

! --------------------------- rdflux10 ------------------------------------------

    subroutine rdflux10(mxfires,nzone,nup,area,hlay,tlay,zfire,qfire,figs,taul,tauu,taufl10,taufu10,firang10,nfire,qllay,qulay,c, &
                        chi10, &
                        radi_gain_gas,radi_loss_gas,radi_gain_gas_fire,radi_loss_gas_fire, &
                        radi_gain_wall_fire,radi_loss_wall_fire)
    
    !     routine: rad4
    !     purpose: this routine calculates the 'c' vector in the net radiation equations of seigel and howell and the
    !        heat absorbed by the lower and upper layer fires due to gas layer emission and fires..

    integer, intent(in) :: mxfires, nzone, nfire, nup
    real(eb), intent(in) :: area(*), hlay, tlay(2), zfire(*), qfire(mxfires), chi10(10)
    real(eb), intent(in) :: figs(nzone,*), taul(nzone,*), tauu(nzone,*), taufl10(mxfires,*), taufu10(mxfires,*), &
                            firang10(10,mxfires)

    real(eb), intent(out) :: qulay, qllay, c(*), radi_loss_gas(10), radi_gain_gas(10), &
                             radi_loss_gas_fire(10), radi_gain_gas_fire(10), &
                             radi_loss_wall_fire(10), radi_gain_wall_fire(10)

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
        radi_gain_gas(k) = 0.0_eb
        radi_loss_gas(k) = 0.0_eb
        radi_gain_gas_fire(k) = 0.0_eb
        radi_loss_gas_fire(k) = 0.0_eb
        radi_gain_wall_fire(k) = 0.0_eb
        radi_loss_wall_fire(k) = 0.0_eb
        
        ! Adding energy due to radiation loss from other comparments to "c"
        !c(k) = c(k) + radi_gain_wall_wall(k)

        ! case: upper to upper
        do j = 1, nup
            qugas = (1.0_eb - tauu(k,j))*eu
            c(k) = c(k) + figs(k,j)*qugas
            qulay = qulay - area(k)*figs(k,j)*qugas
            if (figs(k,j) /= 0._eb) then
                radi_gain_gas(k) = radi_gain_gas(k) + area(k)*figs(k,j)*qugas
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(j))*chi10(j)*qugas
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(k))*chi10(k)*qugas
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(j))*chi10(j)/(1._eb-chi10(k))*chi10(k)*qugas
            end if
        end do

        ! case: lower to upper
        do j = nup + 1, nzone
            qugas = (1.0_eb - tauu(k,j))*eu
            qlgas = (1.0_eb - taul(k,j))*el
            c(k) = c(k) + figs(k,j)*(qugas + qlgas*tauu(k,j))
            wf = area(k)*figs(k,j)
            qulay = qulay + qlgas*wf*(1.0_eb - tauu(k,j)) - qugas*wf
            qllay = qllay - qlgas*wf
            if (figs(k,j) /= 0._eb) then
                radi_gain_gas(k) = radi_gain_gas(k) + area(k)*figs(k,j)*(qugas + qlgas*tauu(k,j))
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(j))*chi10(j)*(qugas + qlgas*tauu(k,j))
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(k))*chi10(k)*(qugas + qlgas*tauu(k,j))
                radi_loss_gas(k) = radi_loss_gas(k) + &
                                   area(k)*figs(k,j)/(1._eb-chi10(j))*chi10(j)/(1._eb-chi10(k))*chi10(k)*&
                                   (qugas + qlgas*tauu(k,j))
            end if
        end do

        ! case: fire to upper layer
        do ifire = 1, nfire
            qfflux = qfire(ifire)*firang10(k,ifire)/(fourpi*area(k))
            c(k) = c(k) + qfflux*taufl10(ifire,k)*taufu10(ifire,k)
            if (firang10(k,ifire) /= 0._eb) then
                radi_gain_wall_fire(k) = radi_gain_gas_fire(k) + area(k)*qfflux*taufl10(ifire,k)*taufu10(ifire,k)
                radi_loss_wall_fire(k) = radi_loss_gas_fire(k) + &
                                         area(k)*qfflux*taufl10(ifire,k)*taufu10(ifire,k)*chi10(k)/(1._eb-chi10(k))
            end if
            if (zfire(ifire)>hlay) then
                factu = 1.0_eb - taufu10(ifire,k)
                factl = 0.0_eb
            else
                factu = (1.0_eb - taufu10(ifire,k))*taufl10(ifire,k)
                factl = 1.0_eb - taufl10(ifire,k)
            end if
            qulay = qulay + factu*qfflux*area(k)
            qllay = qllay + factl*qfflux*area(k)
            if (firang10(k,ifire) /= 0._eb) then
                radi_gain_gas_fire(k) = radi_gain_gas_fire(k) + area(k)*(factu+factl)*qfflux
                radi_loss_gas_fire(k) = radi_loss_gas_fire(k) + area(k)*(factu+factl)*qfflux*chi10(k)/(1._eb-chi10(k))
            end if
        end do
    end do

    do k = nup + 1, nzone
        c(k) = 0.0_eb
        radi_gain_gas(k) = 0.0_eb
        radi_loss_gas(k) = 0.0_eb
        radi_gain_gas_fire(k) = 0.0_eb
        radi_loss_gas_fire(k) = 0.0_eb
        radi_gain_wall_fire(k) = 0.0_eb
        radi_loss_wall_fire(k) = 0.0_eb
        
        ! Adding energy due to radiation loss from other comparments to "c"
        !c(k) = c(k) + radi_gain_wall_wall(k)

        ! case: upper to lower
        do j = 1, nup
            qugas = (1.0_eb - tauu(k,j))*eu
            qlgas = (1.0_eb - taul(k,j))*el
            c(k) = c(k) + figs(k,j)*(qugas*taul(k,j) + qlgas)
            wf = area(k)*figs(k,j)
            qulay = qulay - qugas*wf
            qllay = qllay + qugas*wf*(1.0_eb - taul(k,j)) - qlgas*wf
            if (figs(k,j) /= 0._eb) then
                radi_gain_gas(k) = radi_gain_gas(k) + area(k)*figs(k,j)*(qugas*taul(k,j) + qlgas)
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(j))*chi10(j)*(qugas*taul(k,j) + qlgas)
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(k))*chi10(k)*(qugas*taul(k,j) + qlgas)
                radi_loss_gas(k) = radi_loss_gas(k) + &
                                   area(k)*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)* &
                                  (qugas*taul(k,j) + qlgas)
            end if
        end do

        !case: lower to lower
        do j = nup + 1, nzone
            qlgas = (1.0_eb - taul(k,j))*el
            c(k) = c(k) + figs(k,j)*qlgas
            qllay = qllay - qlgas*area(k)*figs(k,j)
            if (figs(k,j) /= 0._eb) then
                radi_gain_gas(k) = radi_gain_gas(k) + area(k)*figs(k,j)*qlgas
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(j))*chi10(j)*qlgas
                radi_loss_gas(k) = radi_loss_gas(k) + area(k)*figs(k,j)/(1._eb-chi10(k))*chi10(k)*qlgas
                radi_loss_gas(k) = radi_loss_gas(k) + &
                                   area(k)*figs(k,j)/(1._eb-chi10(j))*chi10(j)/(1._eb-chi10(k))*chi10(k)*qlgas
            end if
        end do

        ! case: fire to lower layer
        do ifire = 1, nfire
            qfflux = qfire(ifire)*firang10(k,ifire)/(fourpi*area(k))
            c(k) = c(k) + qfflux*taufl10(ifire,k)*taufu10(ifire,k)
            if (firang10(k,ifire) /= 0._eb) then
                radi_gain_wall_fire(k) = radi_gain_gas_fire(k) + area(k)*qfflux*taufl10(ifire,k)*taufu10(ifire,k)
                radi_loss_wall_fire(k) = radi_loss_gas_fire(k) + &
                                         area(k)*qfflux*taufl10(ifire,k)*taufu10(ifire,k)*chi10(k)/(1._eb-chi10(k))
            end if
            if (zfire(ifire)>hlay) then
                factu = 1.0_eb - taufu10(ifire,k)
                factl = (1.0_eb - taufl10(ifire,k))*taufu10(ifire,k)
            else
                factu = 0.0_eb
                factl = 1.0_eb - taufl10(ifire,k)
            end if
            qulay = qulay + factu*qfflux*area(k)
            qllay = qllay + factl*qfflux*area(k)
            if (firang10(k,ifire) /= 0._eb) then
                radi_gain_gas_fire(k) = radi_gain_gas_fire(k) + area(k)*(factu+factl)*qfflux
                radi_loss_gas_fire(k) = radi_loss_gas_fire(k) + area(k)*(factu+factl)*qfflux*chi10(k)/(1._eb-chi10(k))
            end if
        end do
    end do
    
    return
    end subroutine rdflux10
    
! --------------------------- rabs10 --------------------------------------------

    subroutine rabs10(nzone,nup,e,dqde,emis2,area,figs,tauu,taul,chi10,radi_gain_wall_wall,radi_loss_wall_wall, &
                      radi_gain_gas_wall,radi_loss_gas_wall,qllay,qulay)

    integer, intent(in) :: nup, nzone
    real(eb), intent(in) :: e(10), emis2(10), area(10),dqde(10), figs(nzone,10), tauu(nzone,10), &
                            taul(nzone,10), chi10(10)

    real(eb), intent(out) :: qulay, qllay, radi_loss_wall_wall(10), radi_gain_wall_wall(10), &
                             radi_gain_gas_wall(10), radi_loss_gas_wall(10)

    integer :: j, k
    real(eb) :: qout, qk

    radi_gain_gas_wall(:) = 0._eb
    radi_loss_gas_wall(:) = 0._eb
    radi_gain_wall_wall(:) = 0._eb
    radi_loss_wall_wall(:) = 0._eb
    do k = 1, nup
        qout = e(k) - dqde(k)*(1.0_eb - emis2(k))
        qk = qout*area(k)
        do j = 1,nup
            qulay = qulay + qk*figs(k,j)*(1.0_eb - tauu(k,j))
            
            if (figs(k,j) /= 0._eb) then
                radi_gain_wall_wall(k) = radi_gain_wall_wall(k) + qk*figs(k,j)*tauu(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*tauu(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*tauu(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + &
                                         qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*tauu(k,j)
            
                radi_gain_gas_wall(k) = radi_gain_gas_wall(k) + qk*figs(k,j)*(1._eb-tauu(k,j))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*(1._eb-tauu(k,j))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*(1._eb-tauu(k,j))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*(1._eb-tauu(k,j))
            end if
        end do
        do j = nup + 1, nzone
            qulay = qulay + qk*figs(k,j)*(1.0_eb - tauu(k,j))
            qllay = qllay + qk*figs(k,j)*tauu(k,j)*(1.0_eb - taul(k,j))
            
            if (figs(k,j) /= 0._eb) then
                radi_gain_wall_wall(k) = radi_gain_wall_wall(k) + qk*figs(k,j)*tauu(k,j)*taul(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*tauu(k,j)*taul(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*tauu(k,j)*taul(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + &
                                         qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*tauu(k,j)*taul(k,j)
            
                radi_gain_gas_wall(k) = radi_gain_gas_wall(k) + qk*figs(k,j)*(1._eb-tauu(k,j)+tauu(k,j)*(1.0_eb-taul(k,j)))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*(1._eb-tauu(k,j)+tauu(k,j)*(1.0_eb-taul(k,j)))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*(1._eb-tauu(k,j)+tauu(k,j)*(1.0_eb-taul(k,j)))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*&
                                        (1._eb-tauu(k,j)+tauu(k,j)*(1.0_eb-taul(k,j)))
            end if
        end do
    end do

    do k = nup+1,nzone
        qout = e(k) - dqde(k)*(1.0_eb-emis2(k))
        qk = qout*area(k)
        do j = 1,nup
            qulay = qulay + qk*figs(k,j)*taul(k,j)*(1.0_eb - tauu(k,j))
            qllay = qllay + qk*figs(k,j)*(1.0_eb - taul(k,j))
            
            if (figs(k,j) /= 0._eb) then
                radi_gain_wall_wall(k) = radi_gain_wall_wall(k) + qk*figs(k,j)*taul(k,j)*tauu(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*taul(k,j)*tauu(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*taul(k,j)*tauu(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + &
                                         qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*taul(k,j)*tauu(k,j)
            
                radi_gain_gas_wall(k) = radi_gain_gas_wall(k) + qk*figs(k,j)*(1._eb-taul(k,j)+taul(k,j)*(1.0_eb-tauu(k,j)))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*(1._eb-taul(k,j)+taul(k,j)*(1.0_eb-tauu(k,j)))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*(1._eb-taul(k,j)+taul(k,j)*(1.0_eb-tauu(k,j)))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*&
                                        (1._eb-taul(k,j)+taul(k,j)*(1.0_eb-tauu(k,j)))
            end if
        end do
        do j = nup+1,nzone
            qllay = qllay + qk*figs(k,j)*(1.0_eb - taul(k,j))
            
            if (figs(k,j) /= 0._eb) then
                radi_gain_wall_wall(k) = radi_gain_wall_wall(k) + qk*figs(k,j)*taul(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*taul(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*taul(k,j)
                radi_loss_wall_wall(k) = radi_loss_wall_wall(k) + &
                                         qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*taul(k,j)
            
                radi_gain_gas_wall(k) = radi_gain_gas_wall(k) + qk*figs(k,j)*(1._eb-taul(k,j))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + qk*figs(k,j)/(1._eb-chi10(j))*chi10(j)*(1._eb-taul(k,j))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)*(1._eb-taul(k,j))
                radi_loss_gas_wall(k) = radi_loss_gas_wall(k) + &
                                        qk*figs(k,j)/(1._eb-chi10(k))*chi10(k)/(1._eb-chi10(j))*chi10(j)*(1._eb-taul(k,j))
            end if
        end do
    end do

    return
    end subroutine rabs10

end module radiation_routines
