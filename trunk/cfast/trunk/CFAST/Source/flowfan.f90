

! --------------------------- mvent -------------------------------------------

    subroutine mvent (tsec, hvpsolv, hvtsolv, tprime, flwmv, deltpmv, delttmv, prprime, nprod, ierror, hvacflg, filtered)

    !     routine: mvent
    !     purpose: physical interface routine to calculate flow through all forced vents (mechanical flow).
    !     it returns rates of mass and energy flows into the layers from all mechancial vents in the building.
    !     revision: $revision: 461 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $
    !     arguments: hvpsolv
    !                hvtsolv
    !                tprime
    !                flwmv
    !                deltpmv
    !                delttmv
    !                prprime
    !                nprod
    !                ierror   returns error codes
    !                hvacflg
    !                filtered

    use precision_parameters
    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use params
    implicit none
    
    real(eb), intent(in) :: hvpsolv(*), hvtsolv(*), tprime(*), tsec
    real(eb), intent(out) :: flwmv(nr,ns+2,2), filtered(nr,ns+2,2), prprime(*), deltpmv(*), delttmv(*) 

    real(eb) :: filter, qcifraction, flwmv0(nr,ns+2,2), deltpmv0(mnode), delttmv0(mbr) 
    integer :: i, ii, j, k, ieqtyp, iroom, isys, ierror, nprod
    logical :: first = .true., doit, hvacflg
    save first,flwmv0,deltpmv0,delttmv0

    ! initialize convection coefficient for hvac ducts. ductcv is read in from solver.ini file by initslv.  chv should eventually be defined elsewhere.

    hvacflg = .false.
    if (.not.mvcalc.or.option(fmvent)/=on.or.(nhvpvar==0.and.nhvtvar==0)) return
    hvacflg = .true.
    if (first) then
        first = .false.
        do i = 1, nbr
            chv(i) = ductcv
        end do
        do i = 1, n
            do j = 1, ns+2
                flwmv0(i,j,upper) = 0.0_eb
                flwmv0(i,j,lower) = 0.0_eb
            end do
        end do
    endif

    do i = 1, n
        do j = 1, ns+2
            flwmv(i,j,upper) = 0.0_eb
            flwmv(i,j,lower) = 0.0_eb
            filtered(i,j,upper) = 0.0_eb
            filtered(i,j,lower) = 0.0_eb
        end do
    end do
    do i = 1, nhvpvar
        deltpmv(i) = hvpsolv(i)
    end do
    do i = 1, nhvtvar
        delttmv(i) = hvtsolv(i)
    end do

    if(option(fmodjac)==on)then

        ! determine where the hvac calculation should be done.  initially assume that it should be.
        doit = .true.
        if(jaccol>0)then
            ieqtyp = izeqmap(jaccol,1)

            ! if we're computing a jacobian and a hvac pressure or temperature is being perturbed then do it.
            if(ieqtyp==eqpmv.or.ieqtyp==eqtmv)then
                doit = .true.

                ! if we're computing a jacobian and a wall or target temperature is being perturbed then don't do it
            elseif(ieqtyp==eqtt.or.ieqtyp==eqwt)then
                doit = .false.

                ! if we're computing a jacobian and anything else is being perturbed then do it. if there are no hvac connections in the room where the variable is being perturbed then we shouldn't have to do the hvac computation but that isn't working now.
            else
                iroom = izeqmap(jaccol,2)
                if(.not.izhvac(iroom))doit = .false.
                doit = .true.
            endif
        endif

        ! if we're not going to do the hvac computation then get the answers from the previously saved vectors
        if(.not.doit)then
            do i = 1, nhvpvar
                deltpmv(i) = deltpmv0(i)
            end do
            do i = 1, nhvtvar
                delttmv(i) = delttmv0(i)
            end do
            do i = 1, n
                flwmv(i,m,upper) = flwmv0(i,m,upper)
                flwmv(i,m,lower) = flwmv0(i,m,lower)
                flwmv(i,q,upper) = flwmv0(i,q,upper)
                flwmv(i,q,lower) = flwmv0(i,q,lower)
                do j = 1, ns
                    if(activs(j))then
                        flwmv(i,j+2,upper) = flwmv0(i,j+2,upper)
                        flwmv(i,j+2,lower) = flwmv0(i,j+2,lower)
                    endif
                end do
            end do
            return
        endif
    endif

    call hvfrex (hvpsolv,hvtsolv)
    call hvmflo (tsec, deltpmv,ierror)
    if (ierror/=0) return
    call hvsflo (tprime,delttmv)
    call hvtoex (prprime,nprod)
    do ii = 1, next
        i = hvnode(1,ii)
        j = hvnode(2,ii)
        isys = izhvsys(j)
        if(i<1.or.i>nm1) cycle
        flwmv(i,m,upper) = flwmv(i,m,upper) + hveflo(upper,ii)
        flwmv(i,m,lower) = flwmv(i,m,lower) + hveflo(lower,ii)
        flwmv(i,q,upper) = flwmv(i,q,upper) + cp*hvextt(ii,upper)*hveflo(upper,ii)
        flwmv(i,q,lower) = flwmv(i,q,lower) + cp*hvextt(ii,lower)*hveflo(lower,ii)
        do k = 1, ns
            if (activs(k)) then
                flwmv(i,2+k,lower) = flwmv(i,2+k,lower) + hvexcn(ii,k,lower)*hveflo(lower,ii)
                flwmv(i,2+k,upper) = flwmv(i,2+k,upper) + hvexcn(ii,k,upper)*hveflo(upper,ii)
            endif
        end do
        !	filter 9 and 11, (2+k)) = 11 and 13, smoke and radiological fraction. note that filtering is always negative. same as agglomeration and settling
        filter = qcifraction(qcvf,isys,tsec)
        filtered(i,13,upper) = max(0.0_eb,filter*flwmv(i,13,upper))
        filtered(i,13,lower) = max(0.0_eb,filter*flwmv(i,13,lower))
        filtered(i,11,upper) = max(0.0_eb,filter*flwmv(i,11,upper))
        filtered(i,11,lower) = max(0.0_eb,filter*flwmv(i,11,lower))
    end do

    if(option(fmodjac)==on)then
        if(jaccol==0)then

            ! save information for a later jacobian calculation
            do i = 1, nhvpvar
                deltpmv0(i) = deltpmv(i)
            end do
            do i = 1, nhvtvar
                delttmv0(i) = delttmv(i)
            end do
            do i = 1, n
                flwmv0(i,m,upper) = flwmv(i,m,upper)
                flwmv0(i,m,lower) = flwmv(i,m,lower)
                flwmv0(i,q,upper) = flwmv(i,q,upper)
                flwmv0(i,q,lower) = flwmv(i,q,lower)
                do j = 1, ns
                    if(activs(j))then
                        flwmv0(i,j+2,upper) = flwmv(i,j+2,upper)
                        flwmv0(i,j+2,lower) = flwmv(i,j+2,lower)
                    endif
                end do
            end do
        endif
    endif

    return
    end subroutine mvent

! --------------------------- hvmflo -------------------------------------------

    subroutine hvmflo (tsec, deltpmv,ierror)

    !     routine: hvmflo
    !     purpose: mass flow solution and calculation for mechanical vents
    !     arguments: tsec
    !                deltpmv
    !                ierror   returns error codes

    use precision_parameters
    use cfast_main
    use params
    implicit none

    real(eb), intent(in) :: tsec
    real(eb), intent(out) :: deltpmv(*)
    
    real(eb) :: pav, xtemp, f, dp, hvfan
    integer, intent(out) :: ierror

    integer :: ib, niter, iter, i, ii, j, k

    ierror = 0
    
    ! calculate average temperatures and densities for each branch
    do ib = 1, nbr
        pav = pofset
        rohb(ib) = pav/(hvrgas*tbr(ib))
        bflo(ib) = 1.0_eb
    end do

    ! start the iteration cycle
    niter = 2
    do iter = 1, niter

        ! initialize conductance
        do ib = 1, nbr
            ce(ib)=0.0_eb
        end do

        ! convert from pressure to mass flow rate coefficients
        do ib = 1, nbr
            if (ce(ib)/=0.0_eb) then
                xtemp = 1.0_eb/sqrt(abs(ce(ib)))
                ce (ib) = sign(xtemp, ce(ib))
            endif
        end do

        ! calculate hydrostatic pressure difference terms
        do i = 1, nnode
            do j = 1, ncnode(i)
                dpz(i,j) = rohb(icmv(i,j))*hvgrav*(hvght(mvintnode(i,j)) - hvght(i))
            end do
        end do

        ! find mass flow for each branch and mass residual at each node
        do i = 1, nnode
            f = 0.0_eb
            do j = 1, ncnode(i)
                dp = hvp(mvintnode(i,j)) - hvp(i) + dpz(i,j)
                if (nf(icmv(i,j))==0) then

                    ! resistive branch connection 
                    hvflow(i,j) = sign(ce(icmv(i,j))*sqrt(abs(dp)), dp)
                    bflo(icmv(i,j)) = abs(hvflow(i,j))
                else

                    ! fan branch connection

                    k = nf(icmv(i,j))
                    if(ne(icmv(i,j)) /= i)then
                        ! flow is at fan inlet
                        hvflow(i,j) = -hvfan(tsec,i,j,k,dp)
                    else
                        ! flow is at fan exit
                        dp = -dp
                        hvflow(i,j) = hvfan(tsec,i,j,k,dp)
                    endif
                endif
                f = f + hvflow(i,j)
                ii = izhvie(mvintnode(i,j))
                if(ii/=0)hvflow(mvintnode(i,j),1) = -hvflow(i,j)
            end do
            ii = izhvmape(i)
            if(ii>0) deltpmv(ii) = f
        end do
    end do

    return
    end subroutine hvmflo

! --------------------------- hvsflo -------------------------------------------

    subroutine hvsflo (tprime, delttmv)

    !     routine: hvsflo
    !     purpose: species calculation for mechanical vents
    !     arguments: tprime
    !                deltpmv

    use precision_parameters
    use cfast_main
    use opt
    use params
    implicit none

    real(eb), intent(in) :: tprime(*)
    real(eb), intent(out) :: delttmv(*)
    
    real(eb) :: hvta, flowin, hvtemp
    integer ib, i, ii, j

    do ib = 1, nbr
        delttmv(ib) = rohb(ib)*hvdvol(ib)*tprime(ib)/gamma
    end do

    do i = 1, nnode

        ! calculate temperatures & smoke flows in following loop at the connecting nodes
        hvta = 0.0_eb
        flowin = 0.0_eb
        do j = 1, ncnode(i)
            if (hvflow(i,j)>0.0_eb) then
                flowin = flowin + hvflow(i,j)
                ib = icmv(i,j)
                hvtemp = hvflow(i,j)
                hvta = hvta + hvtemp*tbr(ib)
            endif
        end do
        if (flowin>0.0_eb) then
            hvta = hvta/flowin
        else
            
            ! this is a bad situation.  we have no flow, yet must calculate the inflow concentrations.
            hvta = tbr(1)
            do ii = 1, next
                if (hvnode(2,ii)==i) then
                    hvta = hvextt(ii,upper)
                    exit
                endif
            end do
        endif

        ! now calculate the resulting temperature and concentrations in the ducts and fans
        do j = 1, ncnode(i)
            if (hvflow(i,j)<0.0_eb) then
                ib = icmv (i,j)
                delttmv(ib) = delttmv(ib) - (hvta-tbr(ib))*abs(hvflow(i,j))
                if(option(fhvloss)==on)then
                    delttmv(ib) = delttmv(ib) + chv(ib)*(tbr(ib)-tamb(1))*hvdara(ib)
                endif
            endif
            ii = izhvie(mvintnode(i,j))
            if(ii/=0.and.hvflow(i,j)>0.0_eb) then
                ib = icmv(i,j)
                hvta = hvextt(ii,upper)
                delttmv(ib) = delttmv(ib) - (hvta-tbr(ib))*hvflow(i,j)
                if(option(fhvloss)==on)then
                    delttmv(ib) = delttmv(ib) + chv(ib)*(tbr(ib)-tamb(1))*hvdara(ib)
                endif
            endif
        end do
    end do
    return
    end

! --------------------------- hvfan -------------------------------------------

    real(eb) function hvfan(tsec,i,j,k,dp)

    !     routine: hvfan
    !     purpose: calculates mass flow through a fan. this function has been modified to prevent negative flow.  
    !              a max function was inserted just after the calculation off, the flow to do this.  if the flow is allowed to be negative (flow reversal) then this statement must be removed.
    !              also, there is now a flow restriction on the fan, using qcmfraction
    !     arguments: tsec   current simulation time
    !                i      node number
    !                j      jj'th connection to node ii 
    !                k      fan number
    !                dp     head pressure across the fan

    use precision_parameters
    use cfast_main
    implicit none

    real(eb), intent(in) :: tsec, dp
    integer, intent(in) :: i,j,k
        
    real(eb) :: hvfanl, openfraction, qcffraction, minimumopen, roh, d1mach, f
    logical :: firstc = .true.
    save firstc, minimumopen

    roh = rohb(icmv(i,j))

    if (firstc) then
        minimumopen = sqrt(d1mach(1))
        firstc = .false.
    endif

    ! the hyperbolic tangent allows for smooth transition from full flow to no flow within the fan cuttoff pressure range
    f = 0.5_eb - tanh(8.0_eb/(hmax(k)-hmin(k))*(dp-hmin(k))-4.0_eb)/2.0_eb
    f = max(minimumopen, f)
    hvfanl = f*qmax(k)*roh
    openfraction = max (minimumopen, qcffraction (qcvm, k, tsec))
    hvfan = hvfanl*openfraction
    return

    end function hvfan

! --------------------------- hvfrex -------------------------------------------

    subroutine hvfrex (hvpsolv, hvtsolv)

    !     routine: hvfrex
    !     purpose: update arrays and assign compartment pressures, temperatures and concentrations to flow into the system from exterior nodes
    !     arguments: tsec   current simulation time
    !                hvpsolv
    !                hvtsolv

    use precision_parameters
    use cenviro
    use cfast_main
    use params
    implicit none

    real(eb) :: hvpsolv(*), hvtsolv(*), z, xxlower, xxlower_clamped, fraction, zl, zu, rl, ru, xxrho
    integer :: i, ii, j, ib, lsp
    
    type(room_type), pointer :: roomi

    do ii = 1, next
        i = hvnode(1,ii)
        roomi=>roominfo(i)

        j = hvnode(2,ii)

        z = roomi%zzhlay(lower)
        
        if (hvorien(ii)==1) then

            ! we have an opening which is oriented vertically - use a smooth crossover. first, calculate the scaling length of the duct
            xxlower = sqrt(arext(ii))
        else
            xxlower = sqrt(arext(ii)) / 10.0_eb
        endif

        ! then the bottom of the vent (above the floor)
        xxlower_clamped = max(0.0_eb,min((hvelxt(ii) - 0.5_eb * xxlower),(roomi%hr-xxlower)))

        ! these are the relative fraction of the upper and lower layer that the duct "sees" these parameters go from 0 to 1
        fraction = max(0.0_eb,min(1.0_eb,max(0.0_eb,(z-xxlower_clamped)/xxlower)))
        hvfrac(upper,ii) = min(1.0_eb,max(1.0_eb-fraction,0.0_eb))
        hvfrac(lower,ii) = min(1.0_eb,max(fraction,0.0_eb))
    end do

    ! this is the actual duct initialization
    do ii = 1, next
        i = hvnode(1,ii)
        roomi=>roominfo(i)
        
        j = hvnode(2,ii)
        
        if (i<n) then
            z = roomi%zzhlay(lower)
            zl = min(z,hvelxt(ii))
            zu = min(0.0_eb,hvelxt(ii)-zl)
            ru = roomi%zzrho(upper)
            rl = roomi%zzrho(lower)
            hvp(j) = roomi%zzrelp - (ru*zu+rl*zl) * hvgrav
            hvextt(ii,upper) = roomi%zztemp(upper)
            hvextt(ii,lower) = roomi%zztemp(lower)
        else
            hvextt(ii,upper) = exta
            hvextt(ii,lower) = exta
            hvp(j) =  expa - exra * hvgrav * hvelxt(ii)
        endif
        do lsp = 1, ns
            if (activs(lsp)) then
                if (i<n) then
                    hvexcn(ii,lsp,upper) = zzcspec(i,upper,lsp)
                    hvexcn(ii,lsp,lower) = zzcspec(i,lower,lsp)
                else
                    xxrho = o2n2(lsp) * exra
                    hvexcn(ii,lsp,upper) = xxrho
                    hvexcn(ii,lsp,lower) = xxrho
                endif
            endif
        end do
    end do
    do i = 1, nhvpvar
        ii = izhvmapi(i)
        hvp(ii) = hvpsolv(i)
    end do
    do ib = 1, nhvtvar
        tbr(ib) = hvtsolv(ib)
    end do
    return
    end subroutine hvfrex


! --------------------------- hvtoex -------------------------------------------

    subroutine hvtoex(prprime,nprod)

    !     routine: hvfrex
    !     purpose: assign results of hvac simulation to the transfer variables (hvextt, hvexcn)
    !     arguments: tsec   current simulation time
    !                prprime
    !                nprod

    use precision_parameters
    use cenviro
    use cfast_main
    use params
    implicit none

    real(eb), intent(out) :: prprime(*) 
    integer, intent(in) :: nprod
    
    integer i, ii, j, k, ib, isys, isof, nhvpr
    
    ! sum product flows entering system
    nhvpr = nlspct*nhvsys
    if(nprod/=0)then
        do i = 1, nhvpr
            prprime(i) = 0.0_eb
        end do
    endif
    if(ns>0)then
        do isys = 1, nhvsys
            hvmfsys(isys) = 0.0_eb
            do k = 1, ns
                dhvprsys(isys,k) = 0.0_eb
            end do
        end do
    endif

    ! flow into the isys system
    do ii = 1, next
        j = hvnode(2,ii)
        ib = icmv(j,1)
        hveflo(upper,ii) = hvflow(j,1)*hvfrac(upper,ii)
        hveflo(lower,ii) = hvflow(j,1)*hvfrac(lower,ii)
        isys = izhvsys(j)
        if (hvflow(j,1)<0.0_eb) then
            hvmfsys(isys) = hvmfsys(isys) + hvflow(j,1)
            if(nprod/=0)then
                do k = 1, ns
                    if (activs(k)) dhvprsys(isys,k) = dhvprsys(isys,k) + abs(hveflo(upper,ii))*hvexcn(ii,k,upper) + abs(hveflo(lower,ii))*hvexcn(ii,k,lower)
                end do
            endif
        endif
    end do

    ! flow out of the isys system
    if(nprod/=0)then
        do k = 1, min(ns,9)
            if(activs(k))then
                do isys = 1, nhvsys
                    if (zzhvm(isys)/=0.0_eb)then
                        dhvprsys(isys,k) = dhvprsys(isys,k) - abs(hvmfsys(isys))*zzhvpr(isys,k)/zzhvm(isys)
                    endif
                end do
            endif
        end do

        ! do a special case for the non-reacting gas(es)
        k = 11
        if(activs(k))then
            do isys = 1, nhvsys
                if (zzhvm(isys)/=0.0_eb)then
                    dhvprsys(isys,k) = dhvprsys(isys,k) - abs(hvmfsys(isys))*zzhvpr(isys,k)/zzhvm(isys)
                endif
            end do
        endif

        ! pack the species change for dassl (actually resid)
        isof = 0
        do k = 1, min(ns,9)
            if(activs(k))then
                do isys = 1, nhvsys
                    isof = isof + 1
                    if (zzhvm(isys)/=0.0_eb)then
                        prprime(isof) = dhvprsys(isys,k)
                    else
                        prprime(isof) = 0.0_eb
                    endif
                end do
            endif
        end do
        ! do a special case for the non-reacting gas(es)
        k = 11
        if(activs(k))then
            do isys = 1, nhvsys
                isof = isof + 1
                if (zzhvm(isys)/=0.0_eb)then
                    prprime(isof) = dhvprsys(isys,k)
                else
                    prprime(isof) = 0.0_eb
                endif
            end do
        endif
    endif           

    ! define flows or temperature leaving system
    do ii = 1, next
        j = hvnode(2,ii)
        isys = izhvsys(j)
        ! we allow only one connection from a node to an external duct
        ib = icmv(j,1)
        if (hvflow(j,1)>0.0_eb) then
            hvextt(ii,upper) = tbr(ib)
            hvextt(ii,lower) = tbr(ib)
            do k = 1, ns
                if (activs(k))then
                    ! case 1 - finite volume and finite mass in the isys mechanical ventilation system
                    if (zzhvm(isys)/=0.0_eb) then
                        hvexcn(ii,k,upper) = zzhvpr(isys,k)/zzhvm(isys)
                        hvexcn(ii,k,lower) = hvexcn(ii,k,upper)
                        ! case 2 - zero volume (no duct). flow through the system is mdot(product)/mdot(total mass) - see keywordcases to change this
                    elseif(hvmfsys(isys)/=0.0_eb) then
                        hvexcn(ii,k,upper) = -(dhvprsys(isys,k)/hvmfsys(isys))
                        hvexcn(ii,k,lower) = hvexcn(ii,k,upper)
                    else
                        hvexcn(ii,k,upper) = 0.0_eb
                        hvexcn(ii,k,lower) = 0.0_eb
                    endif
                endif
            end do
        endif
    end do
    return
    end subroutine hvtoex

! --------------------------- rev_flowfan -------------------------------------------

    integer function rev_flowfan ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_flowfan = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_flowfan