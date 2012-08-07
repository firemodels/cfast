    subroutine vflow (tsec,flwvf,vflowflg)

    !     routine: vflow
    !     purpose: interface between cfast and the vertical vent physical routines.
    !     arguments: tsec: current simulation time
    !                flwvf: change in mass and energy for each layer of each compartment
    !                vflowflg (output): true if vertical flow is included in the simulation

    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use params
    use vents
    implicit none

    real*8 :: flwvf(nr,ns+2,2), xmvent(2), tmvent(2), crosover, oco, epscut, xx0, xx1, qcvfraction, voltop, vollow, xxmu, xxml, xxqu, xxql, xxtmp, xxtq, fl, fu, volup, fumu, fuml, fuqu, fuql, xxmixl, xxmixu, pmtoup, pmtolp
    integer ::  toprm = 1, botrm = 2, ilay(2), i, j, itop, ibot, iflow, ifrm, ito, lsp, index
    logical vflowflg
    real*8 area, tsec

    ! the selection rules are now implemented here.  the crossover is the relative fraction of the volume cloesest to the hole from which the mass will come
    vflowflg = .false.
    if (option(fvflow)/=on) return
    if (nvvent==0) return
    vflowflg = .true.
    crosover = 0.5d0
    oco = 1.0d0 / crosover
    epscut = 0.0001d0
    xx0 = 0.0d0
    xx1 = 1.0d0
    do i = 1, n
        do j = 1, ns + 2
            flwvf(i,j,upper) = xx0
            flwvf(i,j,lower) = xx0
        end do
        do j = 1, n
            vmflo(i,j,upper) = xx0
            vmflo(i,j,lower) = xx0
        end do
    end do

    do i = 1, nvvent
        itop = ivvent(i,toprm)
        ibot = ivvent(i,botrm)
        area = qcvfraction(qcvv, i, tsec) * vvarea(itop,ibot)
        call ventcf (itop, ibot, area, vshape(itop,ibot), epscut, xmvent, tmvent, ilay)
        do iflow = 1, 2

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
                    volup = volfru(ifrm) * oco
                    volup = min(volup, xx1)
                    vollow = max(xx1 - volup, xx0)
                else
                    vollow = volfrl(ifrm) * oco
                    vollow = min(vollow,xx1)
                    volup = max(xx1 - vollow, xx0)
                endif
                xxmu = volup  * xmvent(iflow)
                xxml = vollow * xmvent(iflow)
                xxqu = cp * xxmu * zztemp(ifrm,upper)
                xxql = cp * xxml * zztemp(ifrm,lower)
                xxtmp = volup*zztemp(ifrm,upper) + vollow*zztemp(ifrm,lower)
                xxtq = xxqu + xxql
            else
                xxmu = xx0
                xxml = xmvent(iflow)
                xxqu = xx0
                xxql = cp * xxml * eta(ifrm)
                xxtmp = zztemp(ifrm,lower)
                xxtq = xxqu + xxql
            endif

            fl = xx0
            if (xxtmp<=zztemp(ito,lower)) fl = xx1
            fu = xx1 - fl
            fumu = fu * xmvent(iflow)
            fuml = fl * xmvent(iflow)
            fuqu = fu * xxtq
            fuql = fl * xxtq


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
                    xxmixl = zzcspec(ifrm,lower,lsp) * xxml
                    xxmixu = zzcspec(ifrm,upper,lsp) * xxmu

                    ! deposit mass and enthalphy into "to" room variables (not outside)
                    if (ito<=nm1) then
                        pmtoup = (xxmixu + xxmixl) * fu
                        pmtolp = (xxmixu + xxmixl) * fl
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
    end subroutine vflow

    subroutine ventcf(itop,ibot,avent,nshape,epsp,xmvent,tmvent,ilay)

    !     routine: ventcf
    !     purpose: this routine calculates the flow of mass, enthalpy, and products of combustion through a horizontal vent joining 
    !     an upper space 1 to a lower space 2. the subroutine uses input data describing the two-layer environment of inside rooms and the uniform environment in outside spaces.
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

    use cenviro
    use cfast_main
    implicit none

    real*8 :: xmvent(2), tmvent(2),pabs(2), den(2), relp(2), denvnt(2), dp(2), vst(2), vvent(2)
    integer ::  ilay(2), iroom(2)
    
    integer, parameter :: l = 2, u = 1, q = 2, m = 1

    real*8 :: xxzero, xxone, xxtwo, gamcut, zzz, gammax, pi, delp, delden, rho, eps, x, coef, epsp, epscut, srdelp, fnoise, w, gg, ff, rho2, v, avent, cshape, d, delpfd, dpddpf, vexmax, vex
    integer :: itop, ibot, nshape, i
    logical firstc

    data firstc /.true./
    save firstc, gamcut, gammax, pi, xxtwo, xxone, xxzero

    ! initialization code - executed the first time ventcf is called.
    if (firstc) then
        xxzero = 0.0d0
        xxone = 1.0d0
        xxtwo = 2.0d0
        firstc = .false.
        gamcut = (xxtwo/(gamma+xxone)) ** (gamma/(gamma-xxone))
        zzz = gamma * ((xxtwo/(gamma+xxone))**((gamma+xxone)/(gamma-xxone)))
        gammax = sqrt(zzz)
        pi = 4.0d0 * atan(xxone)
    endif

    ! calculate the pabs(i), delp, the other properties adjacent to the two sides of the vent, and delden.
    dp(1) = xxzero
    dp(2) = xxzero
    if (ibot<=nm1) then
        dp(2) = -g * (zzrho(ibot,l)*zzhlay(ibot,l)+zzrho(ibot,u)*zzhlay(ibot,u))
        relp(2) = zzrelp(ibot)
    else
        relp(2) = epa(itop)
    endif

    if (itop<=nm1) then
        relp(1) = zzrelp(itop)
    else
        dp(1) = -g * hrp(ibot) * era(ibot)
        relp(1) = epa(ibot)
    endif
    pabs(1) = relp(1) + dp(1) + pofset
    pabs(2) = relp(2) + dp(2) + pofset

    ! delp is pressure immediately below the vent less pressure immediately above the vent.
    delp = relp(2) + dp(2) - (relp(1)+dp(1))

    ! ilay(1) contains layer index in top room that is adjacent to vent
    ! ilay(2) contains layer index in bottom room that is adjacent to vent
    if (zzvol(itop,l)<=xxtwo*zzvmin(itop)) then
        ilay(1) = u
    else
        ilay(1) = l
    endif
    if (zzvol(ibot,u)<=xxtwo*zzvmin(ibot)) then
        ilay(2) = l
    else
        ilay(2) = u
    endif

    ! delden is density immediately above the vent less density immediately below the vent
    if (itop<=nm1) then
        den(1) = zzrho(itop,ilay(1))
    else
        den(1) = era(ibot)
    endif
    if (ibot<=nm1) then
        den(2) = zzrho(ibot,ilay(2))
    else
        den(2) = era(itop)
    endif
    delden = den(1) - den(2)

    ! calculate vst(i), the "standard" volume rate of flow through the vent into space i
    if (delp>=xxzero) then
        rho = den(2)
        eps = delp / pabs(2)
    else
        rho = den(1)
        eps = -delp / pabs(1)
    endif
    x = xxone - eps
    coef = 0.68d0 + 0.17d0 * eps
    epscut = epsp * max (xxone, relp(1), relp(2))
    epscut = sqrt(epscut)
    srdelp = sqrt(abs(delp))
    fnoise = xxone
    if ((srdelp/epscut)<=130.d0) fnoise = xxone - exp(-srdelp/epscut)
    if (eps<=0.1d-5) then
        w = xxone - 0.75d0 * eps / gamma
    else
        if (eps<gamcut) then
            gg = x ** (xxone/gamma)
            ff = sqrt((xxtwo*gamma/(gamma-xxone))*gg*gg*(xxone-x/gg))
        else
            ff = gammax
        endif
        w = ff / sqrt(eps+eps)
    endif
    rho2 = 2.0d0/rho
    v = fnoise * coef * w * sqrt(rho2) * avent * srdelp

    ! calculate vst for delp > 0, delp < 0 and delp = 0
    if (delp>xxzero) then
        vst(1) = v
        vst(2) = xxzero
    else if (delp<xxzero) then
        vst(1) = xxzero
        vst(2) = v
    else
        vst(1) = xxzero
        vst(2) = xxzero
    endif

    ! calculate vex, the exchange volume rate of flow through the vent
    if (delden>xxzero.and.avent/=xxzero) then

        ! unstable configuration, calculate nonzero vex
        if (nshape==1) then
            cshape = 0.754d0
            d = xxtwo * sqrt(avent/pi)
        else
            cshape = 0.942d0
            d = sqrt(avent)
        endif
        delpfd = cshape ** 2 * g * delden * d ** 5 / (xxtwo*avent**2)
        dpddpf = abs(delp/delpfd)
        vexmax = 0.1d0 * sqrt(xxtwo*g*delden*sqrt(avent**5)/(den(1)+den(2)))
        vex = max(vexmax*(xxone-dpddpf),xxzero)
    else

        ! stable configuration, set vex = 0
        vex = xxzero
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
        xmvent(i) = denvnt(i) * vvent(i)
        if (iroom(i)<=nm1) then

            ! iroom(i) is an inside room so use the environment variable zztemp for temperature 
            tmvent(i) = zztemp(iroom(i),ilay(3-i))
        else

            ! iroom(i) is an outside room so use eta(iroom(3-i) for temperature
            tmvent(i) = eta(iroom(3-i))
        endif
    end do
    return
    end

    subroutine getvventinfo(iinvvent,itop,ibot,harea,hshape,hface)

    !       this is a routine to get the shape data for vertical flow (horizontal) vents

    use cfast_main
    use vents
    include "precis.fi"

    integer itop, ibot, hshape, hface, iinvvent
    real*8 harea

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

    integer function rev_flowvertical

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