
! --------------------------- vertical_flow -------------------------------------------

    subroutine vertical_flow (tsec,flwvf,vflowflg)

    !     routine: vertical_flow
    !     purpose: interface between cfast and the vertical vent physical routines.
    !     arguments: tsec: current simulation time
    !                flwvf: change in mass and energy for each layer of each compartment
    !                vflowflg (output): true if vertical flow is included in the simulation

    use precision_parameters
    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use params
    use vents
    implicit none

    real(eb), intent(in) :: tsec
    real(eb), intent(out) :: flwvf(nr,ns+2,2)
    logical, intent(out) :: vflowflg
    
    real(eb) :: xmvent(2), tmvent(2), crosover, oco, epscut, vollow, xxmu, xxml, xxqu, xxql, xxtmp, xxtq, fl, fu, volup
    real(eb) :: fumu, fuml, fuqu, fuql, xxmixl, xxmixu, pmtoup, pmtolp
    integer ::  ilay(2), i, j, itop, ibot, iflow, ifrm, ito, lsp, index
    real(eb) :: area, vvfraction

    type(vent_type), pointer :: ventptr

    ! the selection rules are now implemented here.  the crossover is the relative fraction of the 
    ! volume cloesest to the hole from which the mass will come
    vflowflg = .false.
    if (option(fvflow)/=on) return
    if (n_vvents==0) return
    vflowflg = .true.
    crosover = 0.5_eb
    oco = 1.0_eb/crosover
    epscut = 0.0001_eb
    do i = 1, n
        do j = 1, ns + 2
            flwvf(i,j,upper) = 0.0_eb
            flwvf(i,j,lower) = 0.0_eb
        end do
        do j = 1, n
            vmflo(i,j,upper) = 0.0_eb
            vmflo(i,j,lower) = 0.0_eb
        end do
    end do

    do i = 1, n_vvents
        ventptr => vventinfo(i)
        itop = ventptr%top
        ibot = ventptr%bottom
        call getventfraction ('V',itop,ibot,1,i,tsec,vvfraction)
        area = vvfraction * vvarea(itop,ibot)
        ventptr%area = area
        !area = qcvfraction(qcvv, i, tsec)*vvarea(itop,ibot)
        call ventcf (itop, ibot, area, vshape(itop,ibot), epscut, xmvent, tmvent, ilay)
        
        ventptr%n_slabs = 2

        do iflow = 1, 2
            
            ! flow information for smokeview
            ventptr%temp_slab(iflow) = tmvent(iflow)
            ventptr%flow_slab(iflow) = xmvent(iflow)
            ventptr%ybot_slab(iflow) = max(0.0_eb,(vvarea(itop,ibot) - sqrt(area))/2.0_eb)
            ventptr%ytop_slab(iflow) = min(vvarea(itop,ibot),(vvarea(itop,ibot) + sqrt(area))/2.0_eb)
            
            ! determine room where flow comes and goes
            if (iflow==1) then
                ifrm = ibot
                ito = itop
            else
                ifrm = itop
                ito = ibot
            endif

            ! determine mass and enthalpy fractions - first "from," then "to"
            if (ifrm<=nm1) then
                if (ifrm==ibot) then
                    volup = volfru(ifrm)*oco
                    volup = min(volup, 1.0_eb)
                    vollow = max(1.0_eb - volup, 0.0_eb)
                else
                    vollow = volfrl(ifrm)*oco
                    vollow = min(vollow,1.0_eb)
                    volup = max(1.0_eb - vollow, 0.0_eb)
                endif
                xxmu = volup *xmvent(iflow)
                xxml = vollow*xmvent(iflow)
                xxqu = cp*xxmu*zztemp(ifrm,upper)
                xxql = cp*xxml*zztemp(ifrm,lower)
                xxtmp = volup*zztemp(ifrm,upper) + vollow*zztemp(ifrm,lower)
                xxtq = xxqu + xxql
            else
                xxmu = 0.0_eb
                xxml = xmvent(iflow)
                xxqu = 0.0_eb
                xxql = cp*xxml*exterior_temperature
                xxtmp = zztemp(ifrm,lower)
                xxtq = xxqu + xxql
            endif

            fl = 0.0_eb
            if (xxtmp<=zztemp(ito,lower)) fl = 1.0_eb
            fu = 1.0_eb - fl
            fumu = fu*xmvent(iflow)
            fuml = fl*xmvent(iflow)
            fuqu = fu*xxtq
            fuql = fl*xxtq


            ! deposit mass and enthalpy into "to" room varibles (not outside)
            if (ito<=nm1) then
                flwvf(ito,m,upper) = flwvf(ito,m,upper) + fumu
                flwvf(ito,m,lower) = flwvf(ito,m,lower) + fuml
                flwvf(ito,q,upper) = flwvf(ito,q,upper) + fuqu
                flwvf(ito,q,lower) = flwvf(ito,q,lower) + fuql
            endif
            vmflo(ifrm,ito,upper) = fumu + vmflo(ifrm,ito,upper)
            vmflo(ifrm,ito,lower) = fuml + vmflo(ifrm,ito,lower)

            ! extract mass and enthalpy from "from" room (not from outside)
            if (ifrm<=nm1) then
                flwvf(ifrm,m,upper) = flwvf(ifrm,m,upper) - xxmu
                flwvf(ifrm,m,lower) = flwvf(ifrm,m,lower) - xxml
                flwvf(ifrm,q,upper) = flwvf(ifrm,q,upper) - xxqu
                flwvf(ifrm,q,lower) = flwvf(ifrm,q,lower) - xxql
            endif
            vmflo(ito,ifrm,upper) = vmflo(ito,ifrm,upper) - xxmu
            vmflo(ito,ifrm,lower) = vmflo(ito,ifrm,lower) - xxml

            ! species transfer for vertical vents
            do lsp = 1, ns
                if (activs(lsp)) then
                    index = pp+lsp-1
                    xxmixl = zzcspec(ifrm,lower,lsp)*xxml
                    xxmixu = zzcspec(ifrm,upper,lsp)*xxmu

                    ! deposit mass and enthalphy into "to" room variables (not outside)
                    if (ito<=nm1) then
                        pmtoup = (xxmixu + xxmixl)*fu
                        pmtolp = (xxmixu + xxmixl)*fl
                        flwvf(ito,index,upper) = flwvf(ito,index,upper) + pmtoup
                        flwvf(ito,index,lower) = flwvf(ito,index,lower) + pmtolp
                    endif

                    ! extract mass and enthalpy from "from" room (not from the outside)
                    if (ifrm<=nm1) then
                        flwvf(ifrm,index,upper) = flwvf(ifrm,index,upper) - xxmixu
                        flwvf(ifrm,index,lower) = flwvf(ifrm,index,lower) - xxmixl
                    endif
                endif
            end do
        end do
    end do

    return
    end subroutine vertical_flow

! --------------------------- getventfraction-------------------------------------

    subroutine getventfraction (venttype,room1,room2,vent_number,vent_index,time,fraction)
    
    use precision_parameters
    use cenviro
    use cfast_main, only: qcvv
    implicit none
    
    character, intent(in) :: venttype
    integer, intent(in) :: room1, room2, vent_number, vent_index
    real(eb), intent(in) :: time
    real(eb), intent(out) :: fraction
    
    integer :: iramp, i
    real(eb), parameter :: mintime=1.0e-6_eb
    real(eb) :: dt, dtfull, dy, dydt, qcvfraction
    type(ramp_type), pointer :: rampptr
    
    fraction = 1.0_eb

    if (nramps>0) then
        do iramp = 1, nramps
            rampptr=>rampinfo(iramp)
            if (rampptr%type==venttype.and.rampptr%from_room==room1.and.rampptr%to_room==room2.and.&
               rampptr%vent_number==vent_number) then
                if (time<=rampptr%time(1)) then
                    fraction = rampptr%value(1)
                    return
                else if (time>=rampptr%time(rampptr%npoints)) then
                    fraction = rampptr%value(rampptr%npoints)
                    return
                else
                    do i=2,rampptr%npoints
                        if (time>rampptr%time(i-1).and.time<=rampptr%time(i)) then
                            dt = max(rampptr%time(i)-rampptr%time(i-1),mintime)
                            dtfull = max(time-rampptr%time(i-1),mintime)
                            dy = rampptr%value(i)-rampptr%value(i-1)
                            dydt = dy / dt
                            fraction = rampptr%value(i-1)+dydt * dtfull
                            return
                        end if
                    end do
                end if
            end if
        end do
    end if
 
    ! This is for backwards compatibility with the older EVENT format for single vent changes
    fraction = 1.0_eb
    if (venttype=='V') fraction = qcvfraction(qcvv, vent_index, time)

    end subroutine getventfraction

! --------------------------- ventcf -------------------------------------------

    subroutine ventcf(itop,ibot,avent,nshape,epsp,xmvent,tmvent,ilay)

    !     routine: ventcf
    !     purpose: this routine calculates the flow of mass, enthalpy, and products of combustion through a horizontal vent joining 
    !     an upper space 1 to a lower space 2. the subroutine uses input data describing the two-layer environment of 
    !     inside rooms and the uniform environment in outside spaces.
    !     arguments: itop: top room number (physically with respect to the second compartment)
    !                ibot: bottom room number
    !                avent: area of the vent [m**2]                
    !                nshape: number characterizing vent shape: 1 = circle, 2 = square
    !                epsp: error tolerance for dpref [dimensionless]
    !                xmvent(i)   i = 1, mass flow from room ibot to room itop
    !                            i = 2, mass flow from room itop to room ibot
    !                tmvent(i)   i = 1, temperature in layer next to vent in top room
    !                            i = 2, temperature in layer next to vent in bottom room
    !                ilay(i)     i = 1, layer index next to vent in top room
    !                            i = 2, layer index next to vent in bottom room

    use precision_parameters
    use cenviro
    use cfast_main
    implicit none

    integer, intent(in) :: itop, ibot, nshape
    real(eb), intent(in) :: avent, epsp
    real(eb), intent(out) :: xmvent(2), tmvent(2)
    
    integer, intent(out) :: ilay(2)
    
    real(eb) :: pabs(2), den(2), relp(2), denvnt(2), dp(2), vst(2), vvent(2)
    integer ::  iroom(2)
    
    integer, parameter :: l = 2, u = 1, q = 2, m = 1

    real(eb) :: gamcut, zzz, gammax, delp, delden, rho, eps, x, coef, epscut, srdelp, fnoise, w
    real(eb) :: gg, ff, rho2, v, cshape, d, delpfd, dpddpf, vexmax, vex
    integer :: i
    logical firstc

    data firstc /.true./
    save firstc, gamcut, gammax

    ! initialization code - executed the first time ventcf is called.
    if (firstc) then
        firstc = .false.
        gamcut = (2.0_eb/(gamma+1.0_eb))**(gamma/(gamma-1.0_eb))
        zzz = gamma*((2.0_eb/(gamma+1.0_eb))**((gamma+1.0_eb)/(gamma-1.0_eb)))
        gammax = sqrt(zzz)
    endif

    ! calculate the pabs(i), delp, the other properties adjacent to the two sides of the vent, and delden.
    dp(1) = 0.0_eb
    dp(2) = 0.0_eb
    if (ibot<=nm1) then
        dp(2) = -grav_con*(zzrho(ibot,l)*zzhlay(ibot,l)+zzrho(ibot,u)*zzhlay(ibot,u))
        relp(2) = zzrelp(ibot)
    else
        relp(2) = exterior_rel_pressure(itop)
    endif

    if (itop<=nm1) then
        relp(1) = zzrelp(itop)
    else
        dp(1) = -grav_con*ceiling_height(ibot)*exterior_density
        relp(1) = exterior_rel_pressure(ibot)
    endif
    pabs(1) = relp(1) + dp(1) + pofset
    pabs(2) = relp(2) + dp(2) + pofset

    ! delp is pressure immediately below the vent less pressure immediately above the vent.
    delp = relp(2) + dp(2) - (relp(1)+dp(1))

    ! if the room above or the room below is dead then  there is no pressure difference at vent opening
    if(deadroom(itop).ne.0.and.deadroom(ibot).ne.0.and.deadroom(itop).eq.ibot.or.deadroom(ibot).eq.itop)delp=0.0_eb

    ! ilay(1) contains layer index in top room that is adjacent to vent
    ! ilay(2) contains layer index in bottom room that is adjacent to vent
    if (zzvol(itop,l)<=2.0_eb*zzvmin(itop)) then
        ilay(1) = u
    else
        ilay(1) = l
    endif
    if (zzvol(ibot,u)<=2.0_eb*zzvmin(ibot)) then
        ilay(2) = l
    else
        ilay(2) = u
    endif

    ! delden is density immediately above the vent less density immediately below the vent
    if (itop<=nm1) then
        den(1) = zzrho(itop,ilay(1))
    else
        den(1) = exterior_density
    endif
    if (ibot<=nm1) then
        den(2) = zzrho(ibot,ilay(2))
    else
        den(2) = exterior_density
    endif
    delden = den(1) - den(2)

    ! calculate vst(i), the "standard" volume rate of flow through the vent into space i
    if (delp>=0.0_eb) then
        rho = den(2)
        eps = delp/pabs(2)
    else
        rho = den(1)
        eps = -delp/pabs(1)
    endif
    x = 1.0_eb - eps
    coef = 0.68_eb + 0.17_eb*eps
    epscut = epsp*max (1.0_eb, relp(1), relp(2))
    epscut = sqrt(epscut)
    srdelp = sqrt(abs(delp))
    fnoise = 1.0_eb
    if ((srdelp/epscut)<=130.0_eb) fnoise = 1.0_eb - exp(-srdelp/epscut)
    if (eps<=0.1e-5_eb) then
        w = 1.0_eb - 0.75_eb*eps/gamma
    else
        if (eps<gamcut) then
            gg = x**(1.0_eb/gamma)
            ff = sqrt((2.0_eb*gamma/(gamma-1.0_eb))*gg*gg*(1.0_eb-x/gg))
        else
            ff = gammax
        endif
        w = ff/sqrt(eps+eps)
    endif
    rho2 = 2.0_eb/rho
    v = fnoise*coef*w*sqrt(rho2)*avent*srdelp

    ! calculate vst for delp > 0, delp < 0 and delp = 0
    if (delp>0.0_eb) then
        vst(1) = v
        vst(2) = 0.0_eb
    else if (delp<0.0_eb) then
        vst(1) = 0.0_eb
        vst(2) = v
    else
        vst(1) = 0.0_eb
        vst(2) = 0.0_eb
    endif

    ! calculate vex, the exchange volume rate of flow through the vent
    if (delden>0.0_eb.and.avent/=0.0_eb) then

        ! unstable configuration, calculate nonzero vex
        if (nshape==1) then
            cshape = 0.754_eb
            d = 2.0_eb*sqrt(avent/pi)
        else
            cshape = 0.942_eb
            d = sqrt(avent)
        endif
        delpfd = cshape**2*grav_con*delden*d**5/(2.0_eb*avent**2)
        dpddpf = abs(delp/delpfd)
        vexmax = 0.1_eb*sqrt(2.0_eb*grav_con*delden*sqrt(avent**5)/(den(1)+den(2)))
        vex = max(vexmax*(1.0_eb-dpddpf),0.0_eb)
    else

        ! stable configuration, set vex = 0
        vex = 0.0_eb
    endif

    ! calculate vvent(i), the volume rate of flow through the vent into space i
    do i = 1, 2
        vvent(i) = vst(i) + vex
    end do

    ! calculate the vent flow properties
    denvnt(1) = den(2)
    denvnt(2) = den(1)

    ! calculate the vent mass flow rates
    iroom(1) = ibot
    iroom(2) = itop
    do i = 1, 2
        xmvent(i) = denvnt(i)*vvent(i)
        if (iroom(i)<=nm1) then

            ! iroom(i) is an inside room so use the environment variable zztemp for temperature 
            tmvent(i) = zztemp(iroom(i),ilay(3-i))
        else

            ! iroom(i) is an outside room so use exterior_temperature for temperature
            tmvent(i) = exterior_temperature
        endif
    end do
    return
    end

! --------------------------- getvventinfo -------------------------------------------

    subroutine getvventinfo(iinvvent,itop,ibot,harea,hshape,hface)

    !       this is a routine to get the shape data for vertical flow (horizontal) vents

    use cfast_main
    use vents
    
    use precision_parameters
    implicit none

    integer, intent(in) :: iinvvent

    integer, intent(out) :: itop, ibot, hshape, hface 
    real(eb), intent(out) :: harea

    itop = ivvent(iinvvent,1)
    ibot = ivvent(iinvvent,2)
    harea = vvarea(itop,ibot)
    hshape = vshape(itop,ibot)
    if (itop>nm1) then
        hface = 6
    else
        hface = 5
    endif

    return
    end

! --------------------------- rev_flowvertical -------------------------------------------

    integer function rev_flowvertical ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$revision: 461 $'
    character(255), parameter :: maindate='$date: 2012-06-28 16:38:31 -0400 (thu, 28 jun 2012) $'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_flowvertical = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_flowvertical
