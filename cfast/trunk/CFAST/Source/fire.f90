    subroutine fires(tsec,flwf,update)

    !     routine: fires
    !     purpose: physical interface routine to calculate the current
    !              rates of mass and energy flows into the layers from
    !              all fires in the building.
    !     revision: $Revision$
    !     revision date: $Date$
    !     arguments: tsec   current simulation time (s)
    !                flwf   mass and energy flows into layers due to fires.
    !                       standard source routine data structure.
    !                nfire  total number of fires
    !                ifroom room numbers for each of the fires
    !                xfire  fire related quantities used by other routines.
    !                       (i,1 to 3) = x, y, and z position for fire i
    !                       (i,4) = mass into upper layer from fire i (ems)
    !                       (i,5) = pyrolysis rate from fire i (emp)
    !                       (i,6) = mass entrained in plume by fire i (eme)
    !                       (i,7 & 8) = convective, and radiative heat into
    !                               upper layer, fire i
    !                       (i,9) = total heat released by fire i
    !                       (i,10) = total heat into lower layer by fire i
    !                       (i,11) = total heat into upper layer by fire i
    !                       (i,12 to 18) = heat of combustion, c/co2,
    !                                co/co2, h/c, o/c, hcl, hcn yields for
    !                                fire i
    !					   (i,19) characteristic length of the burning volume
    !                      (i,20) fire area

    use cenviro
    use cfast_main
    use flwptrs
    use objects1
    use objects2
    use opt
    use params
    implicit none

    real(8) :: flwf(nr,ns+2,2), xntms(2,ns), stmass(2,ns), xxfire(1), yyfire(1), zzfire(1), zzloc(1), ftemp(1), fvel(1), n_C, n_H, n_O, n_N, n_Cl, omasst, oareat, ohight, oqdott, objhct, y_soot, y_co, y_trace, xqft, xtl, q1, q2, tsec, xqfr
    integer cjetopt, lsp, iroom, logerr, nobj, iobj, i, j, update

    ! initialize summations and local data
    do lsp = 1, ns + 2
        do iroom = 1, n
            flwf(iroom,lsp,upper) = 0.0d0
            flwf(iroom,lsp,lower) = 0.0d0
        end do
    end do
    nfire = 0

    if (option(ffire)/=fcfast) return

    ! Check to see if there is a main fire specified. We should not be able to get here
    if (lfbo>0.and.lfbo<n.and.lfbt>0) then
        write (logerr,*) 'Stop MAINF keyword is outdated. ','Update input file'
    endif

    nobj = 0
    do i = 1, numobjl
        if (objpnt(i)>0) then
            iroom = objrm(i)
            iobj = objpnt(i)
            call pyrols(i,tsec,iroom,omasst,oareat,ohight,oqdott,objhct,n_C,n_H,n_O,n_N,n_Cl,y_soot,y_co,y_trace)
            oplume(1,iobj) = omasst

            do lsp = 1, ns
                stmass(upper,lsp) = zzgspec(iroom,upper,lsp)
                stmass(lower,lsp) = zzgspec(iroom,lower,lsp)
            end do

            call dofire(i,iroom,oplume(1,iobj),hr(iroom),br(iroom),dr(iroom),objhct,y_soot,y_co,y_trace,n_C,n_H,n_O,n_N,n_Cl,objgmw(i),stmass,objpos(1,iobj),objpos(2,iobj), &
            objpos(3,iobj)+ohight,objclen(i),oplume(2,iobj),oplume(3,iobj),oqdott,xntms,qf(iroom),qfc(1,iroom),xqfr,heatlp(iroom),heatup(iroom))

            ! sum the flows for return to the source routine
            xtl = zztemp(iroom,lower)
            flwf(iroom,m,upper) = flwf(iroom,m,upper) + oplume(3,iobj)
            flwf(iroom,m,lower) = flwf(iroom,m,lower) - oplume(2,iobj)
            q1 = cp * oplume(1,iobj) * te
            q2 = cp * oplume(2,iobj) * xtl
            flwf(iroom,q,upper) = flwf(iroom,q,upper) + qfc(upper,iroom) + q1 + q2
            flwf(iroom,q,lower) = flwf(iroom,q,lower) - q2
            do lsp = 1, ns
                flwf(iroom,lsp+2,upper) = flwf(iroom,lsp+2,upper) + xntms(upper,lsp)
                flwf(iroom,lsp+2,lower) = flwf(iroom,lsp+2,lower) + xntms(lower,lsp)
            end do

            ! put the object information to arrays - xfire and froom, ...
            ! note that we are carrying parallel data structures for the fire information
            ! output uses the unsorted arrays, froom, ..., ordered by object
            ! fire physics uses the sorted arrays, sorted by compartment
            nfire = nfire + 1
            ifroom(nfire) = iroom
            xfire(nfire,1) = objpos(1,iobj)
            xfire(nfire,2) = objpos(2,iobj)
            xfire(nfire,3) = objpos(3,iobj) + ohight
            xfire(nfire,4) = oplume(3,iobj)
            xfire(nfire,5) = oplume(1,iobj)
            xfire(nfire,6) = oplume(2,iobj)
            xfire(nfire,7) = qfc(1,iroom)
            xfire(nfire,8) = xqfr
            xfire(nfire,9) = heatlp(iroom) + heatup(iroom)
            xfire(nfire,10) = heatlp(iroom)
            xfire(nfire,11) = heatup(iroom)
            xfire(nfire,12) = objhct
            xfire(nfire,13) = y_soot
            xfire(nfire,14) = y_co
            !xfire(nfire,15) = hcratt
            !xfire(nfire,16) = ocratt
            !xfire(nfire,17) = clfrat
            !xfire(nfire,18) = cnfrat
            xfire(nfire,19) = objclen(iobj)
            xfire(nfire,20) = oareat
            nobj = nobj + 1
            froom(nobj) = iroom
            femp(nobj) = oplume(1,iobj)
            fems(nobj) = oplume(3,iobj)
            ! note that cnfrat is not reduced by sprinklers, but oplume(1) is so femr is. (see code in chemie and pyrols)
            femr(nobj) = oplume(1,iobj) * y_trace
            fqf(nobj) = heatlp(iroom) + heatup(iroom)
            fqfc(nobj) = qfc(1,iroom)
            fqlow(nobj) = heatlp(iroom)
            fqupr(nobj) = heatup(iroom)
            farea(nobj) = oareat
            do j = 1,3
                fopos (j,nobj) = objpos(j,iobj)
            end do

        endif
    end do

    return
    end

    subroutine dofire(ifire,iroom,xemp,xhr,xbr,xdr,hcombt,y_soot,y_co,y_trace,n_C,n_H,n_O,n_N,n_Cl,mol_mass,stmass,xfx,xfy,xfz,objectsize,xeme,xems,xqpyrl,xntms,xqf,xqfc,xqfr,xqlp,xqup)

    !     routine: dofire
    !     purpose: do heat release and species from a fire
    !     arguments:  ifire: fire number (ifire=0 is the main fire)
    !                 iroom: room containing the fire
    !                 xemp: pyrolysis rate of the fire (kg/s)
    !                 xhr: height of the room (m)
    !                 xbr: breadth of the room (m)
    !                 xdr: Depth of the room (m)
    !                 hcombt: current heat of combustion (j/kg)
    !                 y_soot, y_co, y_trace: species yields for soot, CO, and trace species; others are calculated from the molecular formula of the fuel (kg species produced/kg fuel pyrolyzed)
    !                 n_C, n_H, n_O, n_N, n_Cl: molecular formula for the fuel; these can be fractional; yields of O2, HCl, and HCN are determined from this
    !                 molar_mass: molar mass of the fuel (kg/mol)
    !                 stmass: mass of a species in a layer in the room (kg)
    !                 xfx: position of the fire in x direction
    !                 xfy: position of the fire in y direction
    !                 xfz: position of the fire in z direction
    !                 objectsize: characteristic object diameter for plume models
    !                 xeme (output): plume entrainment rate (kg/s)
    !                 xems (output): plume flow rate into the upper layer (kg/s)
    !                 xqpyrl (output): actual heat release rate of the fire (w)
    !                 xntms (output): net change in mass of a species in a layer
    !                 xqf (output): net heat generation rate into upper layer (w)
    !                 xqfc (output): net convection into layers (w)
    !                 xqfr (output): net radiation from fire (w)
    !                 xqlp (output): heat release in the lower plume (w)
    !                 xqup (output): heat release rate in the upper plume (w)

    use cenviro
    use cfast_main
    use fireptrs
    use interfaces
    implicit none

    real(8) :: xntms(2,ns), xqfc(2), stmass(2,ns), xmass(ns), n_C ,n_H, n_O, n_N, n_Cl, mol_mass, x1, x0, xz, xtl, xtu, xqlp, xeme, xems, xemp, xxfirel, xxfireu, xbr, xhr, xfz, xntfl, qheatl, qheatu, xqfr, &
    y_trace, y_soot, y_co, chirad, xqpyrl, objectsize, xfx, xfy, xdr, source_o2, activated_time, activated_rate, hcombt, xtemp, xnet, xqf, xqup, uplmep, uplmes, uplmee, height
    integer :: iroom, lsp, ipass, source, i, ifire

    x1 = 1.0d0
    x0 = 0.0d0

    ! note: added upper/lower parameters to following three statements.
    ! xtu was incorrectly set to lower layer temp, fixed it
    xz = zzhlay(iroom,upper)
    xtl = zztemp(iroom,lower)
    xtu = zztemp(iroom,upper)
    xqfc(lower) = 0.0d0
    xqfc(upper) = 0.0d0
    xqlp = 0.0d0
    xeme = 0.0d0

    ! these are the lengths ("heights") in the upper and lower layers respectively
    ! if it is negative, then the fire is not in that layer
    xxfirel = xhr - xz - xfz
    xxfireu = xhr - xfz
    xntfl = x0
    qheatl = x0
    qheatu = x0
    xqfr = x0
    xems = x0

    do lsp = 1, ns
        xntms(upper,lsp) = x0
        xntms(lower,lsp) = x0
        xmass(lsp) = x0
    end do

    ! the trace species is assumed to be released by the pyrolysis of the burning object regardless of whether the fuel actually combusts here.
    ! this is consistent with the earlier chemistry routine. release it here and deposit it in the upper layer
    xntms(upper,11) = xemp*y_trace

    ! now do the kinetics scheme

    ! divvy up the plume output into radiation and convective energy.
    ! convection drives the plume entrainment

    chirad = max(min(radconsplit(ifire),x1),x0)
    qheatl = max((xqpyrl+cp*(te-xtl)*xemp)*(x1-chirad),x0)

    if (lfbt==free) then
        ! we have eliminated unconstrained fires, if we reach this point, the input parser has failed!
        stop 101
    else

        ! note that the combination of firplm and chemie can be called twice
        ! in a single iteration to make sure that the plume entrainment is
        ! consistent with the actual fire size for oxygen limited fires
        ! this is done by "re-passing" the actual fire size to firplm in the
        ! second pass
        ipass = 1
        do while (ipass<=2)

            ! calculate the entrainment rate but constrain the actual amount
            ! of air entrained to that required to produce stable stratification
            call firplm(fplume(ifire), ifire, objectsize, qheatl,xxfirel,xemp,xems,xeme,min(xfx,xbr-xfx),min(xfy,xdr-xfy))

            ! check for an upper only layer fire
            if (xxfirel<=x0) go to 90
            xeme = min(xeme,qheatl/(max((xtu-xtl),x1)*cp))
            xems = xemp + xeme

            source_o2 = zzcspec(iroom,lower,2)
            if (iquench(iroom)>0) then
                activated_time = xdtect(iquench(iroom),dtact)
                activated_rate = xdtect(iquench(iroom),drate)
            else
                activated_time = 0
                activated_rate = 0.0
            endif
            call chemie(xemp,mol_mass,xeme,iroom,hcombt,y_soot,y_co,n_C,n_H,n_O,n_N,n_Cl,source_o2,limo2,idset,iquench(iroom),activated_time,activated_rate,stime,qspray(ifire,lower),xqpyrl,xntfl,xmass) 

            ! limit the amount entrained to that actually entrained by the fuel burned
            xqpyrl = max(x0, (xqpyrl+cp*(te-xtl)*xemp)*(x1-chirad))

            if (xqpyrl<qheatl) then
                xeme = xeme * (xqpyrl/qheatl)
                qheatl = xqpyrl
                ipass = ipass + 1
                cycle
            endif
            exit
        end do
        xqpyrl = xqpyrl/(x1-chirad)
        xems = xemp + xeme

        do  i = 1, ns
            xntms(upper,i) = xmass(i) + xntms(upper,i)
        end do

        ! add the species flow entrained by the plume to normalize the yields to unity
        xtemp = x0
        do lsp = 1, 9
            xtemp = xtemp + stmass(lower,lsp)
        end do
        ! including the trace species
        xtemp = xtemp + stmass(lower,11)
        if(xtemp==0.0d0) xtemp = 1.0d0
        do lsp = 1, ns
            if (activs(lsp)) then
                xnet = xeme * stmass(lower,lsp) / xtemp
                xntms(upper,lsp) = xntms(upper,lsp) + xnet
                xntms(lower,lsp) = xntms(lower,lsp) - xnet
            endif
        end do
        
        ! add in the fuel. everything else is done by chemie.
        xntms(upper,7) = xntms(upper,7) + xemp
        
        xqfr = xqpyrl * chirad
        xqfc(upper) = xqpyrl * (x1-chirad)
        xqlp = xqpyrl
        xqf = xqpyrl

        ! add burning in the upper layer to the fire. the heat which drives entrainment in the upper layer is the sum of the
        ! heat released in the lower layer and what can be released in the upper layer.

        ! start with the fuel removed by lower layer burning, xntfl umplm{ep},{es},and {ee} are equivalent to emp, ems and eme
90      xqup = 0.0d0
        uplmep = max(x0,xemp-xntfl)

        if (uplmep>x0) then
            qheatu = hcombt * uplmep + qheatl
            height = max (x0, min(xz,xxfireu))

            call firplm(fplume(ifire), ifire, objectsize,qheatu,height,uplmep,uplmes,uplmee,min(xfx,xbr-xfx),min(xfy,xdr-xfy))

            source_o2 = zzcspec(iroom,upper,2)
            call chemie(uplmep,mol_mass,uplmee,iroom,hcombt,y_soot,y_co,n_C,n_H,n_O,n_N,n_Cl,source_o2,limo2,idset,iquench(iroom),activated_time,activated_rate,stime,qspray(ifire,upper),xqpyrl,xntfl,xmass)

            xqfr = xqpyrl * chirad + xqfr
            xqfc(upper) = xqpyrl * (x1-chirad) + xqfc(upper)
            xqup = xqpyrl
            xqf = xqpyrl + xqf
            do i = 1, ns
                xntms(upper,i) = xmass(i) + xntms(upper,i)
            end do
        endif

    endif
    return
    end subroutine dofire

    subroutine chemie (pyrolysis_rate, molar_mass,entrainment_rate, source_room, h_c, y_soot, y_co,n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, &
    activated_room, activated_sprinkler, activated_time, activated_rate, model_time, hrr_at_activation, hrr_constrained, pyrolysis_rate_constrained, species_rates)

    !     routine: chemie
    !     purpose: do the combustion chemistry - for plumes in both the upper and lower layers.
    !         note that the kinetics scheme is implemented here.  however, applying it to the
    !         various pieces, namely the lower layer plume, the upper layer plume, and the door jet fires, is 
    !         somewhat complex.
    !         care should be exercised in making changes either here or in the source interface routine.
    !     arguments:  pyrolysis_rate: calculated pyrolysis rate of the fuel (kg/s)
    !                 molar_mass: molar mass of the fuel (kg/mol)
    !                 entrainment_rate: calculated entrainment rate (kg/s)
    !                 source_room: compartment that contains this fire
    !                 h_c: heat of combustion of the fuel (W/kg)
    !                 y_soot, y_co: species yields for soot and CO; others are calculated from the molecular formula of the fuel (kg species produced/kg fuel pyrolyzed)
    !                 n_C, n_H, n_O, n_N, n_Cl: molecular formula for the fuel; these can be fractional; yields of O2, HCl, and HCN are determined from this
    !                 source_o2, lower_o2_limit: oxygen concentration in the source layer of the compartment; lower oxygen limit for combustion (as a fraction)
    !                 activated_room: if zero, a sprinkler has gone off in this compartment.  If equal to the source room, HRR is saved for future quenching
    !                 activated_sprinkler: sprinkler that has activated
    !                 activated_time: time of sprinkler activaiton (s)
    !                 activated_rate: sprinkler suppression rate
    !                 model_time: current simulation time (s)

    !                 hrr_at_activation (output): saved hrr in case of future activation (W)
    !                 hrr_constrained (output): actual HRR of the fire constrained by available oxygen (W)
    !                 pyrolysis_rate_constrained (output): actual pyrolysis rate of the fuel constrained by available oxygen (kg/s)
    !                 species_rates (output): production rates of species based on calculated yields and constrained pyrolysis rate (kg/s); fuel and oxygen are naturally negative

    implicit none
    integer, intent(in) :: source_room, activated_room, activated_sprinkler
    real(8), intent(in) :: pyrolysis_rate, molar_mass, entrainment_rate, h_c, y_soot, y_co, n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, activated_time, activated_rate, model_time
    real(8), intent(out) :: hrr_constrained, hrr_at_activation, pyrolysis_rate_constrained, species_rates(:)

    logical :: first=.TRUE.
    real(8) :: o2f, o2fi, o2_entrained, o2_factor, o2_available, quenching_factor
    real(8) :: nu_o2, nu_co2, nu_h2o, nu_co, nu_soot, nu_hcl,nu_hcn
    real(8) :: net_o2, net_co2, net_h2o, net_co, net_soot, net_hcl, net_hcn, net_fuel, net_ct, net_trace

    if (first) then
        o2f = 1.31d+7
        o2fi = 1.0d0 / o2f
        first = .false.
    endif

    ! calculate the actual burning rate constrained by available o2.

    ! note the scaling in the tanh function.  tanh approaches ~2 at
    ! about ~4. the function inside the tanh scales the ordinate to
    ! ~o2range.  the remainder of the function scales the result 
    ! to 0-1
    o2_entrained = entrainment_rate * source_o2
    o2_factor = tanh(800.0d0*(source_o2-lower_o2_limit)-4.0d0) * 0.5d0 + 0.5d0
    o2_available = o2_entrained * o2_factor
    hrr_constrained = max(0.0d0,min(pyrolysis_rate*h_c,o2_available*o2f))
    pyrolysis_rate_constrained = hrr_constrained / h_c


    ! Here we do a reduction for sprinklers if activation has occurred. Otherwise we just save the current value of the HRR
    if (activated_room==source_room) then
        ! if idset=source then save value of fire for later quenching
        hrr_at_activation = hrr_constrained
    else if (activated_room==0) then
        ! a sprinkler reduces the hrr from a fire. the reduction factor is determined by the sprinkler characteristics.
        ! this factor is applied to the fire based on hrr at activation.
        ! however, the hrr might be reduced for other reasons, so the arithmetic min function is used.
        ! the value used is the value at activation. the quenching factor is then a reduction based on time since activation
        if (activated_sprinkler/=0) then
            quenching_factor = exp(-(model_time-activated_time)/activated_rate)
            if (hrr_at_activation>0.0d0) hrr_constrained = min(hrr_constrained,quenching_factor*hrr_at_activation)
        endif
    endif

    ! now do the chemistry balance with supplied inputs.  
    nu_soot = molar_mass/0.01201d0*y_soot
    nu_hcn = n_N
    nu_hcl = n_Cl
    nu_co = molar_mass/0.02801d0*y_co
    nu_h2o = (n_H - nu_hcl - nu_hcn)/2.0d0
    nu_co2 = n_C  - nu_co - nu_hcn - nu_soot
    nu_o2 = nu_co2 + (nu_h2o + nu_co - n_O)/2.0d0

    ! chemistry balance is molar-based so convert back to mass rates. fuel and o2 are consumed, so negative. Others are produced, so positive
    net_fuel = -pyrolysis_rate_constrained
    net_o2 = -pyrolysis_rate_constrained*nu_o2*0.032d0/molar_mass
    net_co2 = pyrolysis_rate_constrained*nu_co2*0.04401d0/molar_mass
    net_co = pyrolysis_rate_constrained*nu_co*0.02801d0/molar_mass
    net_h2o = pyrolysis_rate_constrained*nu_h2o*0.018016d0/molar_mass
    net_hcl = pyrolysis_rate_constrained*nu_hcl*0.036458d0/molar_mass
    net_hcn = pyrolysis_rate_constrained*nu_hcn*0.027028d0/molar_mass
    net_soot = pyrolysis_rate_constrained*nu_soot*0.01201d0/molar_mass
    net_ct = pyrolysis_rate_constrained

    ! set mass "generation" rates in the cfast structure for species
    species_rates(2) = net_o2
    species_rates(3) = net_co2
    species_rates(4) = net_co
    species_rates(5) = net_hcn
    species_rates(6) = net_hcl
    species_rates(7) = net_fuel
    species_rates(8) = net_h2o
    species_rates(9) = net_soot
    species_rates(10) = net_ct

    end subroutine chemie 

    subroutine pyrols (objn,time,iroom,omasst,oareat,ohight,oqdott,objhct,n_C,n_H,n_O,n_N,n_Cl,y_soot,y_co,y_trace)

    !     routine: pyrols
    !     purpose: returns yields for object fires interpolated from user input 
    !     arguments:  objn: the object pointer number, 
    !                 time: current simulation time (s)
    !                 iroom: room contining the object
    !                 omasst (output): pyrolysis rate of object (returned)
    !                 oareat (output): area of pyrolysis of object (returned)
    !                 ohight (output): height of fire (returned)
    !                 oqdott (output): heat release rate of object
    !                 objhct (output): object heat of combustion
    !                 n_C, n_H, n_O, n_N, n_Cl (output): molecular formula for the fuel; these can be fractional; yields of O2, HCl, and HCN are determined from this
    !                 y_soot, y_co, y_trace (output): species yields for soot, CO, and trace species; others are calculated from the molecular formula of the fuel (kg species produced/kg fuel pyrolyzed)

    use cfast_main
    use objects2
    implicit none

    real(8) :: n_C, n_H, n_O, n_N, n_Cl, y_soot, y_co, y_trace, xx0, omasst, oareat, ohight, oqdott, objhct, xxtime, time, tdrate, xxtimef, qt, qtf, tfact
    integer :: objn, lobjlfm, id, iroom, ifact

    if (.not.objon(objn).or.objset(objn)>0) then
        xx0 = 0.0d0
        omasst = xx0
        oareat = xx0
        ohight = xx0
        oqdott = xx0
        n_C = 1.0d0
        n_H = 4.0d0
        n_O = 0.0d0
        n_N = 0.0d0
        n_Cl = 0.0d0
        objhct = 5.0d7
        y_soot = 0.0d0
        y_co = 0.0d0
        y_trace = 0.0d0
        return
    endif

    lobjlfm = objlfm(objn)
    xxtime = time - objcri(1,objn)

    id = iquench(iroom)

    if(id==0)then

        ! if a sprinkler is not active then interpolate at current time
        ifact = 0
    else

        ! if a sprinkler is active then interpolate at current time
        ! and when sprinkler first activated.  make sure that specified
        ! heat release rate is the smaller of rate at current time
        ! and rate at sprinkler activation time * exp( ...) 
        tdrate = xdtect(id,drate)
        xxtimef = xdtect(id,dtact) - objcri(1,objn)
        call interp(otime(1,objn),oqdot(1,objn),lobjlfm,xxtime,1,qt)
        call interp(otime(1,objn),oqdot(1,objn),lobjlfm,xxtimef,1,qtf)
        ifact = 1
        tfact = exp(-(xxtime-xxtimef)/tdrate)
        if(qt<tfact*qtf)then

            ! current time heat release rate is smaller than sprinklerd value
            ! so use current time and reset ifact to 0 so rates are not 
            ! decreased
            ifact = 0
        else
            xxtime = xxtimef
        endif
    endif

    call interp(otime(1,objn),omass(1,objn),lobjlfm,xxtime,1,omasst)
    call interp(otime(1,objn),oqdot(1,objn),lobjlfm,xxtime,1,oqdott)
    call interp(otime(1,objn),objhc(1,objn),lobjlfm,xxtime,1,objhct)
    call interp(otime(1,objn),ood(1,objn),lobjlfm,xxtime,1,y_soot)
    call interp(otime(1,objn),oco(1,objn),lobjlfm,xxtime,1,y_co)
    call interp(otime(1,objn),omprodr(1,11,objn),lobjlfm,xxtime,1,y_trace)
    call interp(otime(1,objn),oarea(1,objn),lobjlfm,xxtime,1,oareat)
    call interp(otime(1,objn),ohigh(1,objn),lobjlfm,xxtime,1,ohight)

    n_C = obj_C(objn)
    n_H = obj_H(objn)
    n_O = obj_O(objn)
    n_N = obj_N(objn)
    n_Cl = obj_Cl(objn)

    ! attenuate mass and energy release rates if there is an active sprinkler in this room
    if(id/=0.and.ifact==1)then
        omasst = omasst*tfact
        oqdott = oqdott*tfact
    endif

    return
    end subroutine pyrols

    subroutine firplm (plumetype, objectnumber, objectsize, qjl, zz, xemp, xems, xeme, xfx, xfy)

    !     routine: fireplm
    !     purpose: physical interface between dofire and the plume models

    implicit none
    integer plumetype, objectnumber
    real(8) :: qjl, zz, xemp, xeme, xems, xfx, xfy, objectsize

    select case (plumetype)
    case (1) !    mccaffrey
        call mccaffrey(qjl,zz,xemp,xems,xeme,xfx,xfy,objectsize)
        return        
    case (2) !    heskestad
        call heskestad (qjl,zz,xemp,xems,xeme,xfx,xfy,objectsize)
        return        
    end select
    stop 'bad case in firplm'
    end subroutine firplm

    subroutine mccaffrey (qjl,zz,xemp,xems,xeme,xfx,xfy,od)

    !     routine: mccaffrey
    !     purpose: calculates plume entrainment for a fire from mccaffrey's correlation
    !     inputs:    qjl    fire size (w)
    !                zz      plume height (m)
    !                xemp  mass loss rate of the fire (kg/s)
    !                xfx   position of the fire in x direction (m)
    !                xfy   position of the fire in y direction (m)
    !                od is the object diameter
    !     outputs:   xems  total mass transfer rate at height z (kg/s)
    !                xeme  net entrainment rate at height z (kg/s)
    !     algorithm: "momentum implications for buoyant diffusion flames", combustion and flame 52, 149 (1983)

    implicit none

    logical :: first = .true.
    real(8) :: fm1, fm2, fm3, t1, t2, a1, a2, a3, x0, xf, xfx, xfy, qj, qjl, zz, zq, zdq, xemp, xeme, xems, od
    real(8), parameter :: fire_at_wall = 1.0d-3
    save first, a1, a2, a3, t1, t2

    ! define assignment statement subroutines to compute three parts of correlation
    fm1(zq) = zq ** .566d0
    fm2(zq) = zq ** .909d0
    fm3(zq) = zq ** 1.895d0

    ! first time in firplm calculate coeff's to insure that mccaffrey correlation is continuous.  
    ! that is, for a1 = .011, compute a2, a3 such that
    ! a1*zq**.566 = a2*zq**.909  for zq = .08
    ! a2*zq**.909 = a3*zq**1.895 for zq = .2
    if (first) then
        first = .false.
        t1 = .08d0
        t2 = .20d0
        a1 = .011d0
        a2 = a1 * fm1(t1) / fm2(t1)
        a3 = a2 * fm2(t2) / fm3(t2)
    endif
    x0 = 0.0d0

    ! determine which entrainment to use by fire position.  if we're on the wall or in the corner, entrainment is modified.
    xf = 1.0d0
    if (xfx<=fire_at_wall.or.xfy<=fire_at_wall) xf = 2.0d0
    if (xfx<=fire_at_wall.and.xfy<=fire_at_wall) xf = 4.0d0
    qj = 0.001d0 * qjl
    if (zz>0.d0.and.qj>0.0d0) then
        zdq = zz / (xf*qj) ** 0.4d0
        if (zdq>t2) then
            xems = a3 * fm3(zdq) * qj
        else if (zdq>t1) then
            xems = a2 * fm2(zdq) * qj
        else
            xems = a1 * fm1(zdq) * qj
        endif
        xems = max(xemp,xems/xf)
        xeme = max(xems-xemp,x0)
    else
        xems = xemp
        xeme = 0.0d0
    endif
    return
    end subroutine mccaffrey

    subroutine heskestad (q, z, emp, ems, eme, x, y, od)

    !     routine: mccaffrey
    !     purpose: calculates plume entrainment for a fire from heskestad's variant of zukoski's correlation
    !     inputs:    q    fire size (w)
    !                z      plume height (m)
    !                emp  mass loss rate of the fire (kg/s)
    !                xfx   position of the fire in x direction (m)
    !                xfy   position of the fire in y direction (m)
    !                od is the characteristic size of the object (diameter)
    !     outputs:   ems  total mass transfer rate at height z (kg/s)
    !                eme  net entrainment rate at height z (kg/s)

    implicit none

    real(8) :: q, qj, z, z0, emp, eme, ems, x, y, od, deltaz

    qj = 0.001d0 * q
    z0 = -1.02d0 * od + 0.083d0 * qj**0.4
    deltaz = max(0.0001d0, z-z0)
    eme = 0.071 * qj**0.333 * deltaz**1.67 * (1.0d0+0.026d0*qj**0.67 * deltaz**(-1.67))
    ems = emp + eme    

    end subroutine heskestad

    subroutine integrate_mass (time, deltt)

    !     routine:  integrate_mass
    !     description: Routine to integrate the pyrolosate of objects
    !         we also integrate the trace species release and total for all fires
    !     Arguments:  time    current simulation time
    !                 deltt   current time step

    use cenviro
    use cfast_main
    use params

    implicit none  

    integer ::i, j, irm, ii, isys
    real(8) :: xx0 = 0.0d0, xx1 = 1.0d0, filter, qcifraction, time, deltt

    do i = 0, numobjl
        objmaspy(i) = objmaspy(i) + femp(i)*deltt
        radio(i) = radio(i) + femr(i)*deltt
    end do

    ! sum the trace release from all of the fires
    tradio = xx0
    do i = 0, numobjl
        tradio = tradio + radio(i)
    end do

    ! sum the hvac flow
    ! tracet is the trace species which gets through the vent, traces is the mass stopped. Has to be calculated here since
    ! there is no equivalent to 1-... 
    do irm = 1, n
        do ii = 1, next
            i = hvnode(1,ii)
            j = hvnode(2,ii)
            isys = izhvsys(j)       
            filter = (xx1-qcifraction(qcvf,isys,time)) 
            if (irm==i) then
                hveflot(upper,ii) = hveflot(upper,ii) + hveflo(upper,ii)*deltt
                hveflot(lower,ii) = hveflot(lower,ii) + hveflo(lower,ii)*deltt 
                tracet(upper,ii)  = tracet(upper,ii) + hveflo(upper,ii)*hvexcn(ii,11,upper)*filter*deltt
                tracet(lower,ii)  = tracet(lower,ii) + hveflo(lower,ii)*hvexcn(ii,11,lower)*filter*deltt
                traces(upper,ii)  = traces(upper,ii) + hveflo(upper,ii)*hvexcn(ii,11,upper)*(xx1-filter)*deltt
                traces(lower,ii)  = traces(lower,ii) + hveflo(lower,ii)*hvexcn(ii,11,lower)*(xx1-filter)*deltt
            endif 
        end do
    end do

    return
    end subroutine integrate_mass

    subroutine djet (flwdjf,djetflg)

    !     routine:  djet
    !     description: physical interface routine to calculate the current
    !                  rates of mass and energy flows into the layers from
    !                  all door jet fires in the building.

    !                  note that we presume that this calculation is performed
    !                  after the normal fires and flow through vents so we
    !                  have a heat of combustion to use for the burning fuel.
    !                  at present, this heat of combustion is presumed to be
    !                  that of the main fire.
    !
    !     inputs:   nfire   total number of normal fires
    !     outputs:  flwdjf  mass and energy flows into layers due to fires.
    !                       standard source routine data structure.

    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use params
    use vents
    implicit none

    real(8) :: flwdjf(nr,ns+2,2), xntms1(2,ns), xntms2(2,ns), flwdjf0(nr,ns+2,2), xx0, flw1to2, flw2to1, hcombt, qpyrol1, qpyrol2
    integer i, iroom1, iroom2, ifrom, lsp, iroom
    save flwdjf0

    logical djetflg, dj1flag, dj2flag, ventflg(mxvent), roomflg(nr), anyvents

    ! initialize summations and local data
    djetflg = .false.
    xx0 = 0.0d0
    if (option(fdfire)/=on.or.nfire<=0) return


    ! if no vents have a door jet fire then exit
    do i = 1, nvents

        ! is there a door jet fire into room iroom1
        iroom1 = izvent(i,1)
        if(zztemp(iroom1,upper)>=tgignt)then
            flw1to2 = vss(1,i)+vsa(1,i)
            if(vsas(2,i)>xx0.and.flw1to2>xx0)then
                djetflg = .true.
                exit
            endif
        endif

        !is there a door jet fire into room iroom2
        iroom2 = izvent(i,2)
        if(zztemp(iroom2,upper)>=tgignt)then
            flw2to1 = vss(2,i)+vsa(2,i)
            if(vsas(1,i)>xx0.and.flw2to1>xx0)then
                djetflg = .true.
                exit
            endif
        endif
    end do

    if(.not.djetflg)return
    do ifrom = 1, n
        do lsp = 1, ns + 2
            flwdjf(ifrom,lsp,lower) = xx0
            flwdjf(ifrom,lsp,upper) = xx0
        end do
    end do

    do i = 1, n
        fqdj(i) = xx0
    end do

    hcombt = 5.005d+07

    ! calculate the heat for each of the door jet fires
    call ventflag(ventflg,roomflg,anyvents)
    if(anyvents)then
        do i = 1, nvents
            if(ventflg(i))then
                iroom1 = izvent(i,1)
                iroom2 = izvent(i,2)
                flw1to2 = zzcspec(iroom1,upper,7)*(vss(1,i)+vsa(1,i))
                flw2to1 = zzcspec(iroom2,upper,7)*(vss(2,i)+vsa(2,i))
                call djfire(iroom2,zztemp(iroom1,upper),flw1to2,vsas(2,i),hcombt,qpyrol2,xntms2,dj2flag)
                call djfire(iroom1,zztemp(iroom2,upper),flw2to1,vsas(1,i),hcombt,qpyrol1,xntms1,dj1flag)

                ! sum the flows for return to the source routine
                if(dj1flag)then
                    flwdjf(iroom1,q,upper) = flwdjf(iroom1,q,upper) + qpyrol1
                    do lsp = 1, ns
                        flwdjf(iroom1,lsp+2,upper) = flwdjf(iroom1,lsp+2,upper) + xntms1(upper,lsp)
                    end do
                endif
                if(dj2flag)then
                    flwdjf(iroom2,q,upper) = flwdjf(iroom2,q,upper) + qpyrol2
                    do lsp = 1, ns
                        flwdjf(iroom2,lsp+2,upper) = flwdjf(iroom2,lsp+2,upper) + xntms2(upper,lsp)
                    end do
                endif
            endif
        end do
    endif

    if(option(fmodjac)==on)then
        if(jaccol==0)then

            ! we need to save the solution for later jacobian calculations
            do iroom = 1, nm1
                do lsp = 1, ns + 2
                    flwdjf0(iroom,lsp,lower) = flwdjf(iroom,lsp,lower)
                    flwdjf0(iroom,lsp,upper) = flwdjf(iroom,lsp,upper)
                end do
            end do
        else if(jaccol>0)then

            ! we are computing a jacobian, so get previously saved solution for rooms
            ! that are not affected by the perturbed solution variable
            do iroom = 1, nm1
                if(.not.roomflg(iroom))then
                    do lsp = 1, ns+2
                        flwdjf(iroom,lsp,lower) = flwdjf0(iroom,lsp,lower)
                        flwdjf(iroom,lsp,upper) = flwdjf0(iroom,lsp,upper)
                    end do
                endif
            end do
        endif
    endif

    do i = 1, n
        fqdj(i) = flwdjf(i,q,upper) + flwdjf(i,q,lower)
        heatvf(i) = flwdjf(i,q,upper)
    end do
    return
    end subroutine djet

    subroutine djfire(ito,tjet,xxnetfl,sas,hcombt,qpyrol,xntms,djflowflg)

    !     routine: djfire
    !     purpose: calculate heat and combustion chemistry for a door jet fire 
    !     arguments:  ito: room number door jet is flowing into
    !                 tjet: temperature of the door jet gas
    !                 xxnetfl: net fuel available to be burned
    !                 sas: mass flow rate of entrained air in door jet
    !                 hcombt: heat of combustion of unburned fuel
    !                 qpyrol (output): total heat released by door jet fire
    !                 xntms (output): net change in mass of species in door jet

    use cenviro
    use cfast_main
    use interfaces

    implicit none

    real(8) :: xntms(2,ns), xmass(ns), x0, flw1to2, flw2to1, flwdjf, fldjf0, hcombt, qpyrol, qpyrol1, qpyrol2, xntms1, xntms2, xxnetfl, sas, tjet, dummy, source_o2, xxmol_mass, xqpyrl, xntfl, xxqspray
    integer :: i, iroom1, iroom2, ifrom, ito, lsp
    logical djflowflg

    x0 = 0.0d0
    qpyrol = x0
    djflowflg = .false.

    ! we only wnat to do the door jet calculation if there is fuel, oxygen, and sufficient temperature in the door jet
    if (xxnetfl>x0.and.sas>x0.and.tjet>=tgignt) then

        ! do combustion chemistry assuming complete comversion to co2 & h2o.
        ! although the real chemistry is more complex, for now we don't know
        ! how to handle it.
        dummy = -1.0d0
        djflowflg = .true.
        do i = 1, ns
            xmass(i) = x0
        end do
        source_o2 = zzcspec(ito,lower,2)
        xxmol_mass = 0.01201d0 ! we assume it's just complete combustion of methane
        xxqspray = 0.0d0
        call chemie(xxnetfl,xxmol_mass,sas,ito,hcombt,0.0d0,0.0d0,1.0d0,4.0d0,0.0d0,0.0d0,0.0d0,source_o2,limo2,0,0,0.0d0,0.0d0,stime,xxqspray,xqpyrl,xntfl,xmass)
        qpyrol = xqpyrl

        do i = 1, ns
            xntms(upper,i) = xmass(i)
            xntms(lower,i) = x0
        end do
    endif
    return
    end

    subroutine flamhgt (qdot, area, fheight)

    !     routine: flamhgt
    !     purpose: Calculates flame height for a given fire size and area 
    !     arguments:  qdot: Fire Size (W)
    !                 area: Area of the base of the fire (m^2)
    !                 fheight (output): Calculated flame height (m)
    !
    !     Source: SFPE handbook, Section 2, Chapter 1

    implicit none
    character str*10
    real(8), parameter :: zero = 0.0d0, four = 4.0d0, pi = 3.14159d0
    real(8) :: qdot, area, fheight, d
    if (area<=0d0) then
        d = 0.09d0
    else
        d = sqrt(four*area/pi)
    endif
    fheight = -1.02*d + 0.235*(qdot/1.0d3)**0.4d0
    fheight = max (zero, fheight)
    return
    end subroutine flamhgt

    subroutine PlumeTemp (qdot, xrad, dfire, tu, tl, zfire, zlayer,zin, tplume)

    !     routine: PlumeTemp
    !     purpose: Calculates plume centerline temperature at a specified height above the fire.
    !
    !     Uses McCaffrey's or Heskestad's correlation to calculate plume centerline temperature
    !     Uses Evan's method to determine virtual fire size and fire origin when fire
    !     is in the lower layer and position is in the upper layer
    !     arguments:  qdot: total heat release rate of the fire (W)
    !                 xrad: fraction of fire HRR released as radiation
    !                 dfire: fire diamater (m)
    !                 tu: upper layer gas temperature (K)
    !                 tl: lower layer gas temperature (K)
    !                 zfire: height of the base of the fire (m)
    !                 zlayer: height of the hot/cold gas layer interface (m)
    !                 z: position to calculate plume centerline temperature (m)
    !                 tplume (output): plume centerline temperature

    implicit none
    real(8) :: qdot, xrad, dfire, tu, tl, zfire, zlayer, zin, tplume
    real(8), parameter :: g = 9.81d0, C_T = 9.115d0, Beta = 0.955d0
    real(8) :: cp, rhoamb, z0, qdot_c, z_i1, q_i1star, xi, fheight, z, q_i2star, z_i2, z_eff, q_eff, dt

    !     for the algorithm to work, there has to be a fire, two layers, and a target point about the fire      
    z = zin - zfire
    if (qdot>0.0d0.and.tu>=tl.and.z>=0.0d0) then

        !       fire and target are both in the lower layer
        if (z<=zlayer) then
            !call PlumeTemp_H (qdot, xrad, dfire, tl, z, tplume)
            call PlumeTemp_M (qdot, tl, z, tplume)

            !       fire and target are both in the upper layer
        else if (zfire>=zlayer) then
            !call PlumeTemp_H (qdot, xrad, dfire, tu, z, tplume)
            call PlumeTemp_M (qdot, tu, z, tplume)

            !       fire is in lower layer and target is in upper layer
        else
            qdot_c = qdot*(1.0d0 - xrad)/1000.d0
            rhoamb = 352.981915d0/tl
            cp = 3.019d-7*tl**2 - 1.217d-4*tl + 1.014d0
            z_i1 = zlayer - zfire
            q_i1star = qdot_c/(rhoamb*cp*tl*sqrt(g)*z_i1**(5.d0/2.d0))
            xi = tu/tl
            !           the effective fire source (qi2star) must be a positive number
            if (1.d0+C_T*q_i1star**(2.d0/3.d0)>xi) then
                q_i2star = ((1.d0+C_T*q_i1star**(2.d0/3.d0))/(xi*C_T)-1.d0/C_T)**(3.d0/2.d0)
                z_i2 = (xi*q_i1star*C_T/(q_i2star**(1.d0/3.d0)*((xi-1.d0)*(Beta+1.d0)+xi*C_T*q_i2star**(2.d0/3.d0))))**(2.d0/5.d0)*z_i1
                rhoamb = 352.981915d0/tu
                cp = 3.019d-7*tu**2 - 1.217d-4*tu + 1.014d0
                q_eff = q_i2star*rhoamb*cp*tu*sqrt(g)*z_i2**(5.d0/2.d0)/(1.0d0-xrad)*1000.d0
                z_eff = z-z_i1+z_i2
                call PlumeTemp_M (q_eff, tu, z_eff, tplume)
            else
                tplume = tu
            endif
        endif
    else
        if (zin<=zlayer) then
            tplume = tl
        else
            tplume = tu
        endif
    endif  
    return
    end subroutine PlumeTemp

    subroutine PlumeTemp_H (qdot, xrad, dfire, tgas, z, tplume)

    !     routine: PlumeTemp_H
    !     purpose: Calculates plume centerline temperature at a specified height above the fire using Heskestad's correlation
    !     arguments:  qdot: total heat release rate of the fire (W)
    !                 xrad: fraction of fire HRR released as radiation
    !                 dfire: fire diamater (m)
    !                 tgas: surrounding gas temperature (K)
    !                 z: distance from fire to position to calculate plume centerline temperature (m)
    !                 tplume (output):  plume centerline temperature

    implicit none
    real(8) :: qdot, xrad, dfire, tgas, tl, z, tplume
    real(8), parameter :: g = 9.81d0, piov4 = (3.14159d0/4.0d0)
    real(8) :: cp, fheight, rhoamb, z0, qdot_c, dt, dstar, zp1, zp2, tp1, tp2, a, b

    ! plume temperature correlation is only valid above the mean flame height      
    call flamhgt (qdot,piov4*dfire**2,fheight)

    ! z0 = virtual origin, qdot_c = convective HRR
    if (dfire>0.d0) then
        z0 = -1.02d0*dfire + 0.083d0*(qdot/1000.d0)**0.4d0
    else
        z0 = 0.d0
    endif
    qdot_c = qdot*(1.0d0 - xrad)/1000.d0

    rhoamb = 352.981915d0/tgas
    cp = 3.019d-7*tgas**2 - 1.217d-4*tgas + 1.014d0
    dstar = (qdot/1000.d0/(rhoamb*cp*tgas*sqrt(g)))**0.4d0

    if ((z-z0)/dstar<1.32) then
        dt = 2.91d0*tgas
    else if ((z-z0)<fheight) then
        zp1 = 1.32*dstar
        tp1 = 2.91*tgas
        zp2 = fheight
        tp2 = 9.1d0*(tgas/(g*cp**2*rhoamb**2))**(1.d0/3.d0)*qdot_c**(2.d0/3.d0)*(zp2)**(-5.d0/3.d0)
        a = ((tp2-tp1)*zp2*zp1)/(zp1-zp2)
        b = tp1-a/zp1
        dt = a/(z-z0) + b
    else
        dt = 9.1d0*(tgas/(g*cp**2*rhoamb**2))**(1.d0/3.d0)*qdot_c**(2.d0/3.d0)*(z-z0)**(-5.d0/3.d0)
    endif
    tplume = tgas + dt

    end subroutine PlumeTemp_H

    subroutine PlumeTemp_M (qdot, tgas, z, tplume)

    !     routine: PlumeTemp_M
    !     purpose: Calculates plume centerline temperature at a specified height above the fire using McCaffrey's correlation
    !     arguments:  qdot: total heat release rate of the fire (W)
    !                 tgas: surrounding gas temperature (K)
    !                 z: distance from fire to position to calculate plume centerline temperature (m)
    !                 tplume (output):  plume centerline temperature

    implicit none
    real(8) :: qdot, dfire, tgas, z, tplume
    real(8), parameter :: g = 9.81d0

    real(8) :: cp, rhoamb, dstar, zstar, dt, n, B, theta

    rhoamb = 352.981915d0/tgas
    cp = 3.019d-7*tgas**2 - 1.217d-4*tgas + 1.014d0
    dstar = (qdot/1000.d0/(rhoamb*cp*tgas*sqrt(g)))**(0.4d0)
    zstar = z/dstar
    if (zstar>=0.d0 .and. zstar<1.32d0) then
        n = 0.5d0
        b = 2.91d0
    else if (zstar>=1.32d0 .and. zstar<3.30d0) then
        n = 0.d0
        b = 3.81d0
    elseif (zstar>=3.30d0) then
        n = -1.d0/3.d0
        b  = 8.41d0
    endif

    theta = b*zstar**(2.*n-1.)
    tplume = tgas*(1.+theta)
    return
    end subroutine PlumeTemp_M

    subroutine toxic(deltt)

    !     routine: toxic
    !     purpose: calculate species concentrations (ppm), mass density (kg/m^3), opacity (1/m), 
    !              ct (g-min/m^3), heat flux to target on floor (w)
    !     arguments:  deltt  length of the latest time step (s)

    use cenviro
    use cfast_main
    use params
    implicit none

    real(8) :: aweigh(ns), air(2), v(2), aweigh7, deltt, avagad
    integer i, k, lsp
    logical ppmcal(ns)

    ! aweigh's are molar weights of the species, avagad is the reciprocal
    ! of avagadro's number (so you can't have less than an atom of a species
    data aweigh, aweigh7 /28.d0, 32.d0, 44.d0, 28.d0, 27.d0, 37.d0, 12.d0, 18.d0, 12.d0, 0.d0, 0.0d0, 12.d0/
    data avagad /1.66d-24/
    data ppmcal /3 * .false., 3 * .true., 5 * .false./
    aweigh(7) = aweigh7 * (1.0d0+hcratt)

    do i = 1, nm1
        v(upper) = zzvol(i,upper)
        v(lower) = zzvol(i,lower)
        do k = upper, lower
            air(k) = 0.0d0
            do lsp = 1, 9
                air(k) = air(k) + zzgspec(i,k,lsp) / aweigh(lsp)
            end do
            air(k) = max(avagad,air(k))
        end do

        ! calcluate the mass density in kg/m^3
        do lsp = 1, ns
            if (activs(lsp)) then
                do k = upper, lower
                    ppmdv(k,i,lsp) = zzgspec(i,k,lsp) / v(k)
                end do
            endif
        end do

        ! calculate the molar density
        do lsp = 1, 8
            if (activs(lsp)) then
                do k = upper, lower
                    if (ppmcal(lsp)) then
                        toxict(i,k,lsp) = 1.d+6 * zzgspec(i,k,lsp) / (air(k)*aweigh(lsp))
                    else
                        toxict(i,k,lsp) = 100.d0 * zzgspec(i,k,lsp) / (air(k)*aweigh(lsp))
                    endif
                end do
            endif
        end do

        ! opacity is calculated from seder's work
        ! note: this value was changed 2/15/2 from 3500 to 3778 to reflect the new value as reported by
        ! mulholland in fire and materials, 24, 227(2000) with recommended value of extinction coefficient
        ! of 8700 m^2/g or 8700/ln(1)=3778 converted to optical density
        lsp = 9
        if (activs(lsp)) then
            do k = upper, lower
                toxict(i,k,lsp) = ppmdv(k,i,lsp) * 3778.0d0
            end do
        endif

        ! ct is the integration of the total "junk" being transported
        lsp = 10
        if (activs(lsp)) then
            do k = upper, lower
                toxict(i,k,lsp) = toxict(i,k,lsp) + ppmdv(k,i,lsp) * 1000.0d0 * deltt / 60.0d0
            end do
        endif

        ! ts (trace species) is the filtered concentration - this is the total mass. 
        ! it is converted to fraction of the total generated by all fires.
        ! this step being correct depends on the integratemass routine
        lsp = 11
        if (activs(lsp)) then
            do k = upper, lower
                toxict(i,k,lsp) = zzgspec(i,k,lsp) ! / (tradio+1.0d-10)
            end do
        endif

    end do

    ! ontarget is the radiation received on a target on the floor
    do i = 1, nm1
        ontarget(i) = sigm * (zztemp(i,upper)**4-tamb(i)**4)
        if (ontarget(i)<1.0d0) ontarget(i) = 0.0d0
    end do
    return
    end subroutine toxic

    subroutine remapfires (nfires)

    ! this routine is to combine the main fire (in lfbo) and any objects into a single list
    ! there does not have to be a main fire nor any objects, so nfires may be zero

    use cfast_main
    use smkview
    implicit none

    real(8) :: fheight
    integer :: nfires, i

    ! first, the mainfire if there is one
    if (lfbo>0) then
        nfires = 1
        flocal(1) = froom(0)
        fxlocal(1) = fopos(1,0)
        fylocal(1) = fopos(2,0)
        fzlocal(1) = fopos(3,0)
        call flamhgt (fqf(0),farea(0),fheight)
        fqlocal(1) = fqf(0)
        fhlocal(1) = fheight
    else
        nfires = 0
    endif

    ! now the other objects
    do i = 1, numobjl
        nfires = nfires + 1
        fxlocal(nfires) = fopos(1,i)
        fylocal(nfires) = fopos(2,i)
        fzlocal(nfires) = fopos(3,i)
        call flamhgt (fqf(i),farea(i),fheight)
        fqlocal(nfires) = fqf(i)
        fhlocal(nfires) = fheight
        flocal(nfires) = froom(i)
    end do
    return
    end

    subroutine sethoc (maxint, mdot, qdot, hdot, hinitial)

    !	Routine to implement the algorithm to set the heat of combustion for all fires

    implicit none

    integer :: i, maxint
    real(8) :: mdot(maxint), qdot(maxint), hdot(maxint), hinitial, hcmax = 1.0d8, hcmin = 1.0d+6

    do i = 1, maxint
        if(i>1) then
            if (mdot(i)*qdot(i)<=0.d0) then
                hdot(i) = hinitial
            else
                hdot(i) = min(hcmax,max(qdot(i)/mdot(i),hcmin))
                mdot(i) = qdot(i)/hdot(i)
            endif
        else
            hdot(1) = hinitial
        endif
    end do

    return
    end subroutine sethoc

    subroutine updobj(iflag, told, dt, ifobj, tobj, ierror)

    !     routine: updobj
    !     purpose: check for and set object fire ignition
    !     arguments:  iflag   flags if check, set, or update variables
    !                 told    time previous to this time step
    !                 dt      length of last time step
    !                 ifobj   object number that ignites (return)
    !                 tobj    time object ignites
    !                 ierror  returns error codes

    use cparams
    use cfast_main
    use fltarget
    use objects2
    use opt
    implicit none

    real(8) :: tmpob(2,mxoin), tobj, told, dt, tnobj
    integer :: ifobj, iobj, ignflg, iobtarg, iflag, ierror

    ifobj = 0
    tobj = told + 2.d0*dt
    tnobj = told + dt

    ! note that ignition type 1 is time, type 2 is temperature and 3 is flux !!! the critiria for temperature and flux are stored backupwards - this historical
    ! see corresponding code in keywordcases
    do iobj = 1, numobjl
        if (.not.objon(iobj)) then
            ignflg = objign(iobj)
            iobtarg = obtarg(iobj)
            if (ignflg==1) then
                if (objcri(1,iobj)<=tnobj) then
                    tobj = min(objcri(1,iobj),tobj)
                    ifobj = iobj
                    tmpob(1,iobj) = 1.d0
                    tmpob(2,iobj) = objcri(1,iobj)
                else
                    tmpob(1,iobj) = 0.0d0
                    tmpob(2,iobj) = tnobj + dt
                endif
            else if (ignflg==2) then
                call do_objck(iflag,told,dt,xxtarg(trgtempf,iobtarg),objcri(3,iobj),obcond(obotemp,iobj),iobj,ifobj,tobj,tmpob(1,iobj))
            else if (ignflg==3) then
                call do_objck(iflag,told,dt,xxtarg(trgtfluxf,iobtarg),objcri(2,iobj),obcond(oboflux,iobj),iobj,ifobj,tobj,tmpob(1,iobj))
            else
                call xerror('updobj-incorrectly defined object type',0,1,1)
                ierror = 20
                return
            endif
        endif
    end do

    if (iflag/=mdchk) then
        do iobj = 1, numobjl
            if (.not.objon(iobj)) then
                iobtarg = obtarg(iobj)
                obcond(obotemp,iobj) = xxtarg(trgtempf,iobtarg)
                obcond(oboflux,iobj) = xxtarg(trgtfluxf,iobtarg)
                if (iflag==mdset.and.tmpob(1,iobj)>0.0d0) then
                    if (tmpob(2,iobj)<=tobj) then
                        objon(iobj) = .true.
                        if (option(fbtobj)==on) then
                            objset(iobj) = 1
                        else
                            objset(iobj) = 0
                        endif
                        objcri(1,iobj) = tmpob(2,iobj)
                    endif
                endif
            endif
        end do
    endif

    return
    end

    subroutine do_objck(iflag,told, dt, cond, trip, oldcond, iobj,ifobj, tobj, tmpob)

    implicit none

    real(8) :: tmpob(2), told, dt, cond, trip, oldcond, tobj, delta
    integer :: iflag, iobj, ifobj

    if (cond>trip) then
        delta = (trip-oldcond)/(cond-oldcond)
        tmpob(1) = 1.0d0
        tmpob(2) = told + dt*delta
        tobj = min(tobj,tmpob(2))
        ifobj = iobj
    else
        tmpob(1) = 0.0d0
        tmpob(2) = told + 2.d0*dt
    endif

    return
    end

    subroutine hcl (flwhcl, flxhcl, ierror)

    !     routine: hcl
    !     purpose: physical interface routine to do hcl deposition on wall surfaces.
    !     arguments: flwhcl  mass and energy flows into layers due to hcl deposition.
    !                flxhcl  hcl surface concentration flux.
    !                ierror  returns error codes

    use cenviro
    use cfast_main
    use opt
    use params
    implicit none

    real(8) :: flwhcl(nr,ns+2,2), flxhcl(nr,4), x0, arw, hclg, hclw, h2o, rho, tg, tw, flux, hwdot, hnet
    integer :: j, iroom, iwall, layer, ierror

    ! initialize summations and local data
    x0 = 0.0d0

    ! only zero out mass (lsp=1) and hcl (lsp=2+6) entries of flwhcl
    do iroom = 1, n
        do j = 1, ns+2
            flwhcl(iroom,j,upper) = x0
            flwhcl(iroom,j,lower) = x0
        end do
        flxhcl(iroom,1) = x0
        flxhcl(iroom,2) = x0
        flxhcl(iroom,3) = x0
        flxhcl(iroom,4) = x0
    end do
    if (option(fhcl)==off) return

    ! calculate the hcl "added" to the layers from each surface
    if (activs(6)) then
        do iroom = 1, nm1
            do iwall = 1, 4
                if (switch(iwall,iroom)) then
                    if (iwall==1) then
                        arw = ar(iroom)
                        layer = upper
                    else if (iwall==2) then
                        arw = ar(iroom)
                        layer = lower
                    else if (iwall==3) then
                        arw = (br(iroom)+dr(iroom)) * zzhlay(iroom,upper) * 2.0d0
                        layer = upper
                    else if (iwall==4) then
                        arw = (br(iroom)+dr(iroom)) * (hr(iroom) - zzhlay(iroom,upper)) * 2.0d0
                        arw = max(x0,arw)
                        layer = lower
                    endif

                    ! use environment variables
                    hclg = zzcspec(iroom,layer,6)
                    h2o = zzcspec(iroom,layer,8)
                    rho = zzrho(iroom,layer)
                    tg = zztemp(iroom,layer)
                    hclw = zzwspec(iroom,iwall)
                    flux = qscnv(iwall,iroom)
                    tw = twj(1,iroom,iwall)
                    call hcltran(iroom,iwall,arw,hclg,h2o,rho,tg,hclw,flux,tw,hwdot,hnet,ierror)
                    if (ierror/=0) return

                    ! sum up the flows and fluxes for the source routine
                    flwhcl(iroom,1,layer) = flwhcl(iroom,1,layer)+hnet
                    flwhcl(iroom,2+6,layer) = flwhcl(iroom,2+6,layer) + hnet
                    flxhcl(iroom,iwall) = hwdot

                endif
            end do
        end do
    endif
    return
    end

    subroutine hcltran(icomp,iwall,arw,hclg,h2o,rho,tg,hclw,flux,tw,hwdot,hnet,ierror)

    !     routine: hcl
    !     purpose: routine to calculate the hydrogen chloride balance in the gas and on the wall surface.
    !     arguments: icomp   compartment number (input)
    !                iwall   wall surface number (input)
    !                arw     area of the wall surface (m^2) (input)
    !                hclg    current hcl gas concentration (kg/m^3) (input)
    !                h2o     current h2o gas concentration (kg/m^3) (input)
    !                rho     current gas density (kg/m^3) (input)
    !                tg      gas layer temperature (k) (input)
    !                hclw    current hcl wall density (kg/m^2) (input)
    !                flux    current convective heat flux on wall (w/m^2) (input)
    !                tw      corresponding wall temperature (k) (input)
    !                hwdot   time derivative of the hcl wall concentration (output)
    !                hnet    time derivative of the hcl gas concentration (output)
    !                ierror  returns error codes (output)

    use cfast_main
    implicit none

    real(8) :: xx0, x001, hwdot, hnet, hclg, hclw, hclp, xhclf, tg, twc, tw, b1, b2, b3, b4, b5, b6, b7, h2os, xtemp, exptw, bcoef, rk, flux, rho, h2o, rke, hclcof, arw
    integer :: iwall, icomp, ierror

    xx0 = 0.0d0
    hwdot = xx0
    hnet = xx0
    if ((hclg==0.).and.(hclw==0.)) return

    ! note that we calculate density on the fly, since ppmdv is not updated often enough
    xhclf = hclg * tg * 2.25d-3
    hclp = xhclf * 1.0d6
    twc = tw - 273.0d0

    ! specific values for painted gypsum - b1 and b2 are for gas phase reactions, and b3 and b4 are for the wall itself
    b1 = hclbf(1,iwall,icomp)
    b2 = hclbf(2,iwall,icomp)
    b3 = hclbf(3,iwall,icomp)
    b4 = hclbf(4,iwall,icomp)
    b5 = hclbf(5,iwall,icomp)
    b6 = hclbf(6,iwall,icomp)
    b7 = hclbf(7,iwall,icomp)

    if (b1<=0) return

    ! calculate hcl gas-surface partition coefficient h2os is the saturation concentration of water.
    if (twc<=40.d0) then
        if (hclp>10.d0) then
            h2os = (1.8204d0-0.18890d0*log(hclp)+0.06466d0*twc+1.650d-3*twc**2+7.408d-5*twc**3) / tw
        else
            xtemp = 17.64262d0 - 5164.1d0 / tw
            exptw = exp(xtemp)
            bcoef = (7.696d-5+3.5920d-6*twc+9.166d-8*twc**2+4.116d-9*twc**3) / tw - 1.d-7 * exptw
            h2os = 0.018d0 * exptw + 1.8d4 * bcoef * hclp
        endif
    else if ((twc>40.0d0).and.(twc<=60.0d0)) then
        h2os = (7.044d0-2.2416d3*xhclf-3.874d-3*twc**2+2.328d-4*twc**3+2.376d6*xhclf**2-5.527d8*xhclf**3+4.918d10*xhclf**4-1.359d12*xhclf**5-1.4033d2*twc*xhclf+2.431d4*twc*xhclf**2-1.6023d6*twc*xhclf**3) / tw
    else if ((twc>60.0d0).and.(twc<=80.0d0)) then
        h2os = (107.46d0-4.129d0*twc+5.096d-2*twc**2-3.1915d8*xhclf**3+1.0408d10*xhclf**4-2.2793d11*xhclf**5-5.8194d0*twc**2*xhclf+7.6883d4*twc*xhclf**2-7.4363d2*twc**2*xhclf**2+.059067d0*twc**3*xhclf+1.8132d6*twc*xhclf**3) / tw
    else if ((twc>80.0d0).and.(twc<=95.0d0)) then
        h2os = (2.583d2-8.0386d0*twc+1.739d5*xhclf+7.608d-2*twc**2-1.5492d7*xhclf**2+3.956d9*xhclf**3-2.065d11*xhclf**4+1.3747d13*xhclf**5-4.086d3*twc*xhclf+24.06d0*twc**2*xhclf+1.3558d5*twc*xhclf**2-3.076d7*twc*xhclf**3) / tw
    else if ((twc>95.0d0).and.(twc<=110.0d0)) then
        h2os = (6.431d2-16.374d0*twc+2.822d5*xhclf+0.12117d0*twc**2-8.224d7*xhclf**2-7.387d6*xhclf**3-5.247d3*twc*xhclf+24.30d0*twc**2*xhclf+1.5465d6*twc*xhclf**2-7.250d3*twc**2*xhclf**2) / tw
    else if (twc>110.0d0) then
        xtemp = 18.3036d0 - 3816.44d0 / (tw-46.13d0)
        h2os = 0.2885d0 * exp(xtemp) / tw
    else
        call xerror('hcltran - h2o out of range',0,1,1)
        ierror = 12
        return
    endif

    ! calculate the coefficients
    ! rk is the constant "kc" which is the deposition coefficient (m/s)
    ! rke is the equilibrium coeffient between the gas and solid phase
    if (tw>=tg) then
        rk = 8.33d-3
    else
        x001 = .001d0
        rk = abs(flux/(max(x001,tg-tw)*rho*cp))
    endif
    if (h2os>h2o) then
        xtemp = 1500.0d0 / tw
        exptw = exp(xtemp)
        rke = b1 * exptw / (1.0d0+b2*exptw*hclg) * (1.0d0+b5*h2o**b6/((h2os-h2o)**b7))
    else
        rke = 1.0d4
    endif

    ! calculate the derivatives
    hclcof = rk * (hclg-hclw/(rke+1.0d-20))
    hnet = -hclcof * arw
    xtemp = -b4 / (8.31d0*tw)
    hwdot = hclcof - b3 * exp(xtemp) * hclw
    return
    end

    integer function rev_fire ()

    integer :: module_rev
    character(255) :: module_date 
    character(255), parameter :: mainrev='$Revision$'
    character(255), parameter :: maindate='$Date$'

    write(module_date,'(a)') mainrev(index(mainrev,':')+1:len_trim(mainrev)-2)
    read (module_date,'(i5)') module_rev
    rev_fire = module_rev
    write(module_date,'(a)') maindate
    return
    end function rev_fire