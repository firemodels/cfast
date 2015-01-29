
! --------------------------- fires -------------------------------------------

    subroutine fire (tsec,flwf)

    !     routine: fire
    !     purpose: physical interface routine to calculate the current rates of mass and energy flows into the layers from
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
    !                       (i,7 & 8) = convective, and radiative heat into upper layer, fire i
    !                       (i,9) = total heat released by fire i
    !                       (i,10) = total heat into lower layer by fire i
    !                       (i,11) = total heat into upper layer by fire i
    !                       (i,12 to 18) = heat of combustion, c/co2, co/co2, h/c, o/c, hcl, hcn yields for fire i
    !					    (i,19) characteristic length of the burning volume
    !                       (i,20) fire area

    use precision_parameters
    use fireptrs
    use cenviro
    use cfast_main
    use flwptrs
    use objects1
    use objects2
    use opt
    use params
    implicit none
    
    real(eb), intent(in) :: tsec
    real(eb), intent(out) :: flwf(nr,ns+2,2)

    real(eb) :: xntms(2,ns), stmass(2,ns), n_C, n_H, n_O, n_N, n_Cl
    real(eb) :: omasst, oareat, ohight, oqdott, objhct, y_soot, y_co, y_trace, xtl, q_firemass, q_entrained, xqfr
    integer lsp, iroom, nobj, iobj, i, j

    ! initialize summations and local data
    do lsp = 1, ns + 2
        do iroom = 1, n
            flwf(iroom,lsp,upper) = 0.0_eb
            flwf(iroom,lsp,lower) = 0.0_eb
        end do
    end do
    nfire = 0

    if (option(ffire)/=fcfast) return

    nobj = 0
    do i = 1, numobjl
        if (objpnt(i)>0) then
            iroom = objrm(i)
            iobj = objpnt(i)
            call interpolate_pyrolysis(i,tsec,iroom,omasst,oareat,ohight,oqdott,objhct,n_C,n_H,n_O,n_N,n_Cl,y_soot,y_co,y_trace)
            oplume(1,iobj) = omasst

            do lsp = 1, ns
                stmass(upper,lsp) = zzgspec(iroom,upper,lsp)
                stmass(lower,lsp) = zzgspec(iroom,lower,lsp)
            end do

            call do_fire(i,iroom,oplume(1,iobj),hr(iroom),br(iroom),dr(iroom),objhct,y_soot,y_co,y_trace,n_C,n_H,n_O,n_N,n_Cl,objgmw(i),stmass,objpos(1,iobj),objpos(2,iobj), &
            objpos(3,iobj)+ohight,oareat,oplume(2,iobj),oplume(3,iobj),oqdott,xntms,qf(iroom),qfc(1,iroom),xqfr,heatlp(iroom),heatup(iroom))

            ! sum the flows for return to the source routine
            xtl = zztemp(iroom,lower)
            flwf(iroom,m,upper) = flwf(iroom,m,upper) + oplume(3,iobj)
            flwf(iroom,m,lower) = flwf(iroom,m,lower) - oplume(2,iobj)
            q_firemass = cp*oplume(1,iobj)*xtl
            q_entrained = cp*oplume(2,iobj)*xtl
            flwf(iroom,q,upper) = flwf(iroom,q,upper) + qfc(upper,iroom) + q_firemass + q_entrained
            flwf(iroom,q,lower) = flwf(iroom,q,lower) - q_entrained
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
            xfire(nfire,f_fire_xpos) = objpos(1,iobj)
            xfire(nfire,f_fire_ypos) = objpos(2,iobj)
            xfire(nfire,f_fire_zpos) = objpos(3,iobj) + ohight
            xfire(nfire,f_plume_zpos) = oplume(3,iobj)
            xfire(nfire,f_plume_xpos) = oplume(1,iobj)
            xfire(nfire,f_plume_ypos) = oplume(2,iobj)
            xfire(nfire,f_qfc) = qfc(1,iroom)
            xfire(nfire,f_qfr) = xqfr
            xfire(nfire,f_heatlpup) = heatlp(iroom) + heatup(iroom)
            xfire(nfire,f_heatlp) = heatlp(iroom)
            xfire(nfire,f_heatup) = heatup(iroom)
            xfire(nfire,f_objct) = objhct
            xfire(nfire,f_ysoot) = y_soot
            xfire(nfire,f_yco) = y_co
            !xfire(nfire,15) = hcratt
            !xfire(nfire,16) = ocratt
            !xfire(nfire,17) = clfrat
            !xfire(nfire,18) = cnfrat
            xfire(nfire,f_obj_length) = objclen(iobj)
            xfire(nfire,f_obj_area) = oareat
            nobj = nobj + 1
            froom(nobj) = iroom
            femp(nobj) = oplume(1,iobj)
            fems(nobj) = oplume(3,iobj)
            ! note that cnfrat is not reduced by sprinklers, but oplume(1) is so femr is. (see code in chemistry and interpolate_pyrolysis)
            femr(nobj) = oplume(1,iobj)*y_trace
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

! --------------------------- do_fire -------------------------------------------

    subroutine do_fire(ifire,iroom,xemp,xhr,xbr,xdr,hcombt,y_soot,y_co,y_trace,n_C,n_H,n_O,n_N,n_Cl,mol_mass,stmass,xfx,xfy,xfz,object_area,xeme,xems,xqpyrl,xntms,xqf,xqfc,xqfr,xqlp,xqup)

    !     routine: do_fire
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
    !                 object_area: characteristic object diameter for plume models
    !                 xeme (output): plume entrainment rate (kg/s)
    !                 xems (output): plume flow rate into the upper layer (kg/s)
    !                 xqpyrl (output): actual heat release rate of the fire (w)
    !                 xntms (output): net change in mass of a species in a layer
    !                 xqf (output): net heat generation rate into upper layer (w)
    !                 xqfc (output): net convection into layers (w)
    !                 xqfr (output): net radiation from fire (w)
    !                 xqlp (output): heat release in the lower plume (w)
    !                 xqup (output): heat release rate in the upper plume (w)

    use precision_parameters
    use cenviro
    use cfast_main
    use fireptrs
    use interfaces
    implicit none

    integer, intent(in) :: ifire, iroom
    real(eb), intent(in) :: xemp, xhr, xbr, xdr, hcombt, y_soot, y_co, y_trace, n_C ,n_H, n_O, n_N, n_Cl
    real(eb), intent(in) :: mol_mass, stmass(2,ns), xfx, xfy, xfz, object_area
    real(eb), intent(out) :: xeme, xems, xntms(2,ns), xqfc(2), xqfr, xqlp, xqup

    real(eb) :: xmass(ns), xz, xtl, xtu, xxfirel, xxfireu, xntfl, qheatl, qheatl_c, qheatu, qheatu_c
    real(eb) :: chirad, xqpyrl, source_o2, activated_time, activated_rate, xtemp, xnet, xqf, uplmep, uplmes, uplmee, height
    integer :: lsp, ipass, i

    ! note: added upper/lower parameters to following three statements.
    ! xtu was incorrectly set to lower layer temp, fixed it
    xz = zzhlay(iroom,upper)
    xtl = zztemp(iroom,lower)
    xtu = zztemp(iroom,upper)
    xqfc(lower) = 0.0_eb
    xqfc(upper) = 0.0_eb
    xqlp = 0.0_eb
    xeme = 0.0_eb

    ! these are the lengths ("heights") in the upper and lower layers respectively
    ! if it is negative, then the fire is not in that layer
    xxfirel = xhr - xz - xfz
    xxfireu = xhr - xfz
    xntfl = 0.0_eb
    qheatl_c = 0.0_eb
    qheatu_c = 0.0_eb
    xqfr = 0.0_eb
    xems = 0.0_eb

    do lsp = 1, ns
        xntms(upper,lsp) = 0.0_eb
        xntms(lower,lsp) = 0.0_eb
        xmass(lsp) = 0.0_eb
    end do

    ! the trace species is assumed to be released by the pyrolysis of the burning object regardless of whether the fuel actually combusts here.
    ! this is consistent with the earlier chemistry routine. release it here and deposit it in the upper layer
    xntms(upper,11) = xemp*y_trace

    ! now do the kinetics scheme

    ! divvy up the plume output into radiation and convective energy.
    ! convection drives the plume entrainment

    chirad = max(min(radconsplit(ifire),1.0_eb),0.0_eb)
    qheatl = xqpyrl
    qheatl_c = max(xqpyrl*(1.0_eb-chirad),0.0_eb)

    ! we have eliminated unconstrained fires, if we reach this point, the input parser has failed!
    if (lfbt==free) Stop 101


    ! note that the combination of fire_plume and chemistry can be called twice
    ! in a single iteration to make sure that the plume entrainment is
    ! consistent with the actual fire size for oxygen limited fires
    ! this is done by "re-passing" the actual fire size to fire_plume in the
    ! second pass
    ipass = 1
    do while (ipass<=2)

        ! calculate the entrainment rate but constrain the actual amount
        ! of air entrained to that required to produce stable stratification
        call fire_plume(fplume(ifire), object_area, qheatl, qheatl_c, xxfirel, zztemp(iroom,lower), xemp, xems, xeme, min(xfx,xbr-xfx), min(xfy,xdr-xfy))

        ! check for an upper only layer fire
        if (xxfirel<=0.0_eb) go to 90
        xeme = min(xeme,qheatl_c/(max((xtu-xtl),1.0_eb)*cp))
        xems = xemp + xeme

        source_o2 = zzcspec(iroom,lower,2)
        if (iquench(iroom)>0) then
            activated_time = xdtect(iquench(iroom),dtact)
            activated_rate = xdtect(iquench(iroom),drate)
        else
            activated_time = 0
            activated_rate = 0.0
        endif
        call chemistry (xemp,mol_mass,xeme,iroom,hcombt,y_soot,y_co,n_C,n_H,n_O,n_N,n_Cl,source_o2,limo2,idset,iquench(iroom),activated_time,activated_rate,stime,qspray(ifire,lower),xqpyrl,xntfl,xmass)

        ! limit the amount entrained to that actually entrained by the fuel burned
        xqpyrl = max(0.0_eb, xqpyrl*(1.0_eb-chirad))

        if (xqpyrl<qheatl_c) then
            xeme = xeme*(xqpyrl/qheatl_c)
            qheatl_c = xqpyrl
            ipass = ipass + 1
            cycle
        endif
        exit
    end do
    xqpyrl = xqpyrl/(1.0_eb-chirad)
    qheatl = xqpyrl
    xems = xemp + xeme

    do  i = 1, ns
        xntms(upper,i) = xmass(i) + xntms(upper,i)
    end do

    ! add the species flow entrained by the plume to normalize the yields to unity
    xtemp = 0.0_eb
    do lsp = 1, 9
        xtemp = xtemp + stmass(lower,lsp)
    end do
    ! including the trace species
    xtemp = xtemp + stmass(lower,11)
    if(xtemp==0.0_eb) xtemp = 1.0_eb
    do lsp = 1, ns
        if (activs(lsp)) then
            xnet = xeme*stmass(lower,lsp)/xtemp
            xntms(upper,lsp) = xntms(upper,lsp) + xnet
            xntms(lower,lsp) = xntms(lower,lsp) - xnet
        endif
    end do

    ! add in the fuel. everything else is done by chemistry.
    xntms(upper,7) = xntms(upper,7) + xemp

    xqfr = xqpyrl*chirad
    xqfc(upper) = xqpyrl*(1.0_eb-chirad)
    xqlp = xqpyrl
    xqf = xqpyrl

    ! add burning in the upper layer to the fire. the heat which drives entrainment in the upper layer is the sum of the
    ! heat released in the lower layer and what can be released in the upper layer.

    ! start with the fuel removed by lower layer burning, xntfl umplm{ep},{es},and {ee} are equivalent to emp, ems and eme
90  xqup = 0.0_eb
    uplmep = max(0.0_eb,xemp-xntfl)

    if (uplmep>0.0_eb) then
        qheatu_c = hcombt*uplmep + qheatl_c
        qheatu = qheatu_c/(1.0_eb-chirad)
        height = max (0.0_eb, min(xz,xxfireu))

        call fire_plume (fplume(ifire), object_area, qheatu, qheatu_c, height, zztemp(iroom,upper), uplmep, uplmes, uplmee, min(xfx,xbr-xfx), min(xfy,xdr-xfy))

        source_o2 = zzcspec(iroom,upper,2)
        call chemistry (uplmep,mol_mass,uplmee,iroom,hcombt,y_soot,y_co,n_C,n_H,n_O,n_N,n_Cl,source_o2,limo2,idset,iquench(iroom),activated_time,activated_rate,stime,qspray(ifire,upper),xqpyrl,xntfl,xmass)

        xqfr = xqpyrl*chirad + xqfr
        xqfc(upper) = xqpyrl*(1.0_eb-chirad) + xqfc(upper)
        xqup = xqpyrl
        xqf = xqpyrl + xqf
        do i = 1, ns
            xntms(upper,i) = xmass(i) + xntms(upper,i)
        end do
    endif

    return
    end subroutine do_fire

! --------------------------- chemistry -------------------------------------------

    subroutine chemistry (pyrolysis_rate, molar_mass,entrainment_rate, source_room, h_c, y_soot, y_co,n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, &
    activated_room, activated_sprinkler, activated_time, activated_rate, model_time, hrr_at_activation, hrr_constrained, pyrolysis_rate_constrained, species_rates)

    !     routine: chemistry
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

    use precision_parameters
    implicit none
    
    integer, intent(in) :: source_room, activated_room, activated_sprinkler
    real(eb), intent(in) :: pyrolysis_rate, molar_mass, entrainment_rate, h_c, y_soot, y_co, n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit
    real(eb), intent(in) :: activated_time, activated_rate, model_time
    real(eb), intent(out) :: hrr_constrained, pyrolysis_rate_constrained, species_rates(:)
    real(eb), intent(inout) :: hrr_at_activation

    real(eb) :: o2f, o2fi, o2_entrained, o2_factor, o2_available, quenching_factor
    real(eb) :: nu_o2, nu_co2, nu_h2o, nu_co, nu_soot, nu_hcl,nu_hcn
    real(eb) :: net_o2, net_co2, net_h2o, net_co, net_soot, net_hcl, net_hcn, net_fuel, net_ct

    o2f = 1.31e7_eb
    o2fi = 1.0_eb/o2f

    ! calculate the actual burning rate constrained by available o2.

    ! note the scaling in the tanh function.  tanh approaches ~2 at
    ! about ~4. the function inside the tanh scales the ordinate to
    ! ~o2range.  the remainder of the function scales the result 
    ! to 0-1
    o2_entrained = entrainment_rate*source_o2
    o2_factor = tanh(800.0_eb*(source_o2-lower_o2_limit)-4.0_eb)*0.5_eb + 0.5_eb
    o2_available = o2_entrained*o2_factor
    hrr_constrained = max(0.0_eb,min(pyrolysis_rate*h_c,o2_available*o2f))
    pyrolysis_rate_constrained = hrr_constrained/h_c


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
            if (hrr_at_activation>0.0_eb) hrr_constrained = min(hrr_constrained,quenching_factor*hrr_at_activation)
        endif
    endif

    ! now do the chemistry balance with supplied inputs.  
    nu_soot = molar_mass/0.01201_eb*y_soot
    nu_hcn = n_N
    nu_hcl = n_Cl
    nu_co = molar_mass/0.02801_eb*y_co
    nu_h2o = (n_H - nu_hcl - nu_hcn)/2.0_eb
    nu_co2 = n_C  - nu_co - nu_hcn - nu_soot
    nu_o2 = nu_co2 + (nu_h2o + nu_co - n_O)/2.0_eb

    ! chemistry balance is molar-based so convert back to mass rates. fuel and o2 are consumed, so negative. Others are produced, so positive
    net_fuel = -pyrolysis_rate_constrained
    net_o2 = -pyrolysis_rate_constrained*nu_o2*0.032_eb/molar_mass
    net_co2 = pyrolysis_rate_constrained*nu_co2*0.04401_eb/molar_mass
    net_co = pyrolysis_rate_constrained*nu_co*0.02801_eb/molar_mass
    net_h2o = pyrolysis_rate_constrained*nu_h2o*0.018016_eb/molar_mass
    net_hcl = pyrolysis_rate_constrained*nu_hcl*0.036458_eb/molar_mass
    net_hcn = pyrolysis_rate_constrained*nu_hcn*0.027028_eb/molar_mass
    net_soot = pyrolysis_rate_constrained*nu_soot*0.01201_eb/molar_mass
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

    end subroutine chemistry 

! --------------------------- interpolate_pyrolysis -------------------------------------------

    subroutine interpolate_pyrolysis (objn,time,iroom,omasst,oareat,ohight,oqdott,objhct,n_C,n_H,n_O,n_N,n_Cl,y_soot,y_co,y_trace)

    !     routine: interpolate_pyrolysis
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

    use precision_parameters
    use cfast_main
    use objects2
    implicit none

    integer, intent(in) :: objn, iroom
    real(eb), intent(in) :: time
    real(eb), intent(out) :: omasst, ohight, oqdott, objhct, n_C, n_H, n_O, n_N, n_Cl, y_soot, y_co, y_trace

    real(eb) :: oareat, xxtime, tdrate, xxtimef, qt, qtf, tfact
    integer :: lobjlfm, id, ifact

    if (.not.objon(objn).or.objset(objn)>0) then
        omasst = 0.0_eb
        oareat = 0.0_eb
        ohight = 0.0_eb
        oqdott = 0.0_eb
        n_C = 1.0_eb
        n_H = 4.0_eb
        n_O = 0.0_eb
        n_N = 0.0_eb
        n_Cl = 0.0_eb
        objhct = 5.0e7_eb
        y_soot = 0.0_eb
        y_co = 0.0_eb
        y_trace = 0.0_eb
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
        ! and rate at sprinkler activation time*exp( ...) 
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
    end subroutine interpolate_pyrolysis

! --------------------------- fireplume -------------------------------------------

    subroutine fire_plume (plumetype, object_area, qfire, qfire_c, z, t_inf, xemp, xems, xeme, xfx, xfy)

    !     routine: fireplm
    !     purpose: physical interface between do_fire and the plume models

    use precision_parameters
    implicit none
    
    integer, intent(in) :: plumetype
    real(eb), intent(in) :: qfire, qfire_c, z, xemp, xfx, xfy, object_area, t_inf
    real(eb), intent(out) :: xeme, xems

    select case (plumetype)
    case (1) !    heskestad
        call heskestad_plume (qfire, qfire_c,z,t_inf,xemp,xems,xeme,object_area,xfx,xfy)
        return        
    case (2) !    mccaffrey plume
        call mccaffrey_plume (qfire_c,z,xemp,xems,xeme,xfx,xfy)
        return        
    end select
    stop 'bad case in fire_plume'
    end subroutine fire_plume

! --------------------------- mccaffrey -------------------------------------------

    subroutine mccaffrey_plume (q,z,emp,ems,eme,xfx,xfy)

    !     routine: mccaffrey
    !     purpose: calculates plume entrainment for a fire from mccaffrey's correlation
    !     inputs:    q     fire size (w)
    !                z     plume height (m)
    !                xemp  mass loss rate of the fire (kg/s)
    !                xfx   distance from fire to wall in x direction (m)
    !                xfy   distance from fire to wall in y direction (m)
    !     outputs:   xems  total mass transfer rate at height z (kg/s)
    !                xeme  net entrainment rate at height z (kg/s)
    !     algorithm: "momentum implications for buoyant diffusion flames", combustion and flame 52, 149 (1983)

    use precision_parameters
    use cparams, only: mx_hsep
    implicit none

    real(eb), intent(in) :: q, z, emp, xfx, xfy
    real(eb), intent(out) :: ems,  eme
    
    real(eb) :: xf, qj, z_star
    ! Ensure that mccaffrey correlation is continuous.  
    ! that is, for a1 = 0.011, compute a2, a3 such that a1*zq**0.566 = a2*zq**0.909  for zq = 0.08 and
    !                                                   a2*zq**0.909 = a3*zq**1.895 for zq = 0.2
    
    real(eb), parameter :: t1 = 0.08_eb, t2 = 0.20_eb, a1 = 0.011_eb, a2 = a1*t1**0.566_eb/t1**0.909_eb, a3 = a2*t2**0.909_eb/t2**1.895_eb

    ! determine which entrainment to use by fire position.  if we're on the wall or in the corner, entrainment is modified.
    xf = 1.0_eb
    if (xfx<=mx_hsep.or.xfy<=mx_hsep) xf = 2.0_eb
    if (xfx<=mx_hsep.and.xfy<=mx_hsep) xf = 4.0_eb
    
    qj = 0.001_eb*q
    
    if (z>0.0_eb.and.qj>0.0_eb) then
        z_star = z/(xf*qj)**0.4_eb
        if (z_star>t2) then
            ems = (a3*z_star**1.895_eb*qj)/xf
        else if (z_star>t1) then
            ems = (a2*z_star**0.909_eb*qj)/xf
        else
            ems = (a1*z_star**0.566_eb*qj)/xf
        endif
        ems = max(emp,ems)
        eme = max(ems-emp,0.0_eb)
    else
        ems = emp
        eme = 0.0_eb
    endif
    return
    end subroutine mccaffrey_plume

! --------------------------- heskestad -------------------------------------------

    subroutine heskestad_plume (q, q_c, z, t_inf, emp, ems, eme, area, xfx, xfy)

    !     purpose: calculates plume entrainment for a fire from heskestad's variant of zukoski's correlation
    !     inputs:    q     fire size (w)
    !                z     plume height (m)
    !                t_inf ambient temperature at base of the fire
    !                emp   mass loss rate of the fire (kg/s)
    !                area  is the cross sectional area at the base of the fire
    !                xfx   distance from fire to wall in x direction (m)
    !                xfy   distance from fire to wall in y direction (m)
    !     outputs:   ems   total mass transfer rate up to height z (kg/s)
    !                eme   net entrainment rate up to height z (kg/s)

    use precision_parameters
    use cparams, only: mx_hsep
    implicit none

    real(eb), intent(in) :: q, q_c, z, t_inf, emp, area, xfx, xfy
    real(eb), intent(out) :: ems, eme
    
    real(eb), parameter :: cp = 1.012
    real(eb) :: d, qj, z0, z_l, deltaz, xf, factor, qstar, rho_inf
    
    ! determine which entrainment factor to use by fire position.  if we're on the wall or in the corner, entrainment is modified.
    ! by reflection, entrainment on a wall is 1/2 the entrainment of a fire 2 times larger; 
    !                            in a corner, 1/4 the entrainment of a fire 4 times larger
    xf = 1.0_eb
    if (xfx<=mx_hsep.or.xfy<=mx_hsep) xf = 2.0_eb
    if (xfx<=mx_hsep.and.xfy<=mx_hsep) xf = 4.0_eb

    ! virtual original correlation is based on total HRR
    qj = 0.001_eb*q*xf
    if (z>0.0_eb.and.qj>0.0_eb) then
        d = sqrt(area/xf/pio4)
        rho_inf = 352.981915_eb/t_inf
        qstar = qj/(rho_inf*cp*t_inf*gsqrt*d**(2.5_eb))
        z0 = -1.02_eb*d + 0.083_eb*qj**0.4_eb
        !z0 = d*(-1.02_eb + 1.4*qstar**0.4_eb)

        ! entrainment is based on covective HRR
        qj = 0.001_eb*q_c*xf
        z_l = 0.166_eb*qj**0.4_eb
        !z_l = d*(-1.02_eb + 3.7*qstar**0.4_eb)
        if (z>z_l) then
            factor = 1.0_eb
            deltaz = max(0.0001_eb, z-z0)
        else
            factor = z/z_l
            deltaz = max(0.0001_eb, z_l-z0)
        end if
        !eme = (0.196*(grav_con*rho_inf**2/(cp*t_inf))**onethird*qj**onethird*deltaz**(5.0_eb/3.0_eb)*(1.0_eb+2.9_eb*qj**twothirds/((gsqrt*cp*rho_inf*t_inf)**(2.0_eb/3.0_eb)*deltaz**(5.0_eb/3.0_eb))) * factor)/xf
        eme = (0.071_eb*qj**onethird*deltaz**(5.0_eb/3.0_eb)*(1.0_eb+0.026_eb*qj**twothirds*deltaz**(-5.0_eb/3.0_eb)) * factor)/xf
        ems = emp + eme
    else
        ems = emp
        eme = 0.0_eb
    endif

    end subroutine heskestad_plume

! --------------------------- integrate_mass -------------------------------------------

    subroutine integrate_mass (time, deltt)

    !     routine:  integrate_mass
    !     description: Routine to integrate the pyrolosate of objects
    !         we also integrate the trace species release and total for all fires
    !     Arguments:  time    current simulation time
    !                 deltt   current time step

    use precision_parameters
    use cfast_main
    use params
    implicit none  

    real(eb), intent(in) :: time, deltt
    
    integer ::i, j, irm, ii, isys
    real(eb) :: filter, qcifraction

    do i = 0, numobjl
        objmaspy(i) = objmaspy(i) + femp(i)*deltt
        radio(i) = radio(i) + femr(i)*deltt
    end do

    ! sum the trace release from all of the fires
    tradio = 0.0_eb
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
            filter = (1.0_eb-qcifraction(qcvf,isys,time)) 
            if (irm==i) then
                hveflot(upper,ii) = hveflot(upper,ii) + hveflo(upper,ii)*deltt
                hveflot(lower,ii) = hveflot(lower,ii) + hveflo(lower,ii)*deltt 
                tracet(upper,ii)  = tracet(upper,ii) + hveflo(upper,ii)*hvexcn(ii,11,upper)*filter*deltt
                tracet(lower,ii)  = tracet(lower,ii) + hveflo(lower,ii)*hvexcn(ii,11,lower)*filter*deltt
                traces(upper,ii)  = traces(upper,ii) + hveflo(upper,ii)*hvexcn(ii,11,upper)*(1.0_eb-filter)*deltt
                traces(lower,ii)  = traces(lower,ii) + hveflo(lower,ii)*hvexcn(ii,11,lower)*(1.0_eb-filter)*deltt
            endif 
        end do
    end do

    return
    end subroutine integrate_mass

! --------------------------- door_jet -------------------------------------------

    subroutine door_jet (flwdjf,djetflg)

    !     routine:  door_jet
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

    use precision_parameters
    use cenviro
    use cfast_main
    use flwptrs
    use opt
    use vents
    implicit none
    
    logical, intent(out) :: djetflg
    real(eb), intent(out) :: flwdjf(nr,ns+2,2)

    real(eb) :: xntms1(2,ns), xntms2(2,ns), flwdjf0(nr,ns+2,2), flw1to2, flw2to1, hcombt, qpyrol1, qpyrol2
    integer :: i, iroom1, iroom2, ifrom, lsp, iroom
    save flwdjf0

    logical :: dj1flag, dj2flag, ventflg(mxvent), roomflg(nr), anyvents
    type(vent_type), pointer :: ventptr

    ! initialize summations and local data
    djetflg = .false.
    if (option(fdfire)/=on.or.nfire<=0) return


    ! if no vents have a door jet fire then exit
    do i = 1, n_hvents
        ventptr=>ventinfo(i)

        ! is there a door jet fire into room iroom1
        iroom1 = ventptr%from
        if(zztemp(iroom1,upper)>=tgignt)then
            flw1to2 = vss(1,i)+vsa(1,i)
            if(vsas(2,i)>0.0_eb.and.flw1to2>0.0_eb)then
                djetflg = .true.
                exit
            endif
        endif

        !is there a door jet fire into room iroom2
        iroom2 = ventptr%to
        if(zztemp(iroom2,upper)>=tgignt)then
            flw2to1 = vss(2,i)+vsa(2,i)
            if(vsas(1,i)>0.0_eb.and.flw2to1>0.0_eb)then
                djetflg = .true.
                exit
            endif
        endif
    end do

    if(.not.djetflg)return
    do ifrom = 1, n
        do lsp = 1, ns + 2
            flwdjf(ifrom,lsp,lower) = 0.0_eb
            flwdjf(ifrom,lsp,upper) = 0.0_eb
        end do
    end do

    do i = 1, n
        fqdj(i) = 0.0_eb
    end do

    hcombt = 5.005e7_eb

    ! calculate the heat for each of the door jet fires
    call ventflag(ventflg,roomflg,anyvents)
    if(anyvents)then
        do i = 1, n_hvents
            ventptr=>ventinfo(i)
            if(ventflg(i))then
                iroom1 = ventptr%from
                iroom2 = ventptr%to
                flw1to2 = zzcspec(iroom1,upper,7)*(vss(1,i)+vsa(1,i))
                flw2to1 = zzcspec(iroom2,upper,7)*(vss(2,i)+vsa(2,i))
                call door_jet_fire (iroom2,zztemp(iroom1,upper),flw1to2,vsas(2,i),hcombt,qpyrol2,xntms2,dj2flag)
                call door_jet_fire (iroom1,zztemp(iroom2,upper),flw2to1,vsas(1,i),hcombt,qpyrol1,xntms1,dj1flag)

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
    end do
    return
    end subroutine door_jet

! --------------------------- door_jet_fire -------------------------------------------

    subroutine door_jet_fire (ito,tjet,xxnetfl,sas,hcombt,qpyrol,xntms,djflowflg)

    !     routine: door_jet_fire
    !     purpose: calculate heat and combustion chemistry for a door jet fire 
    !     arguments:  ito: room number door jet is flowing into
    !                 tjet: temperature of the door jet gas
    !                 xxnetfl: net fuel available to be burned
    !                 sas: mass flow rate of entrained air in door jet
    !                 hcombt: heat of combustion of unburned fuel
    !                 qpyrol (output): total heat released by door jet fire
    !                 xntms (output): net change in mass of species in door jet

    use precision_parameters
    use cenviro
    use cfast_main
    use interfaces

    implicit none
    
    integer, intent(in) :: ito
    real(eb), intent(in) :: tjet, xxnetfl, sas, hcombt
    logical, intent(out) :: djflowflg
    real(eb), intent(out) :: qpyrol, xntms(2,ns)

    real(eb) :: xmass(ns), dummy, source_o2, xxmol_mass, xqpyrl, xntfl, xxqspray
    integer :: i

    qpyrol = 0.0_eb
    djflowflg = .false.

    ! we only wnat to do the door jet calculation if there is fuel, oxygen, and sufficient temperature in the door jet
    if (xxnetfl>0.0_eb.and.sas>0.0_eb.and.tjet>=tgignt) then

        ! do combustion chemistry assuming complete comversion to co2 & h2o.
        ! although the real chemistry is more complex, for now we don't know
        ! how to handle it.
        dummy = -1.0_eb
        djflowflg = .true.
        do i = 1, ns
            xmass(i) = 0.0_eb
        end do
        source_o2 = zzcspec(ito,lower,2)
        xxmol_mass = 0.01201_eb ! we assume it's just complete combustion of methane
        xxqspray = 0.0_eb
        call chemistry (xxnetfl,xxmol_mass,sas,ito,hcombt,0.0_eb,0.0_eb,1.0_eb,4.0_eb,0.0_eb,0.0_eb,0.0_eb,source_o2,limo2,0,0,0.0_eb,0.0_eb,stime,xxqspray,xqpyrl,xntfl,xmass)
        qpyrol = xqpyrl

        do i = 1, ns
            xntms(upper,i) = xmass(i)
            xntms(lower,i) = 0.0_eb
        end do
    endif
    return
    end subroutine door_jet_fire

! --------------------------- flame_height -------------------------------------------

    subroutine flame_height (qdot, area, fheight)

    !     routine: flame_height
    !     purpose: Calculates flame height for a given fire size and area 
    !     arguments:  qdot: Fire Size (W)
    !                 area: Area of the base of the fire (m^2)
    !                 fheight (output): Calculated flame height (m)
    !
    !     Source: SFPE handbook, Section 2, Chapter 1

    use precision_parameters
    implicit none
    
    real(eb), intent(in) :: qdot, area
    real(eb), intent(out) :: fheight
    
    real(eb) :: d

    if (area<=0_eb) then
        d = pio4*0.2_eb**2
    else
        d = sqrt(4.0_eb*area/pi)
    endif
    fheight = -1.02_eb*d + 0.235_eb*(qdot/1.0e3_eb)**0.4_eb
    fheight = max (0.0_eb, fheight)
    return
    end subroutine flame_height

        
! --------------------------- gettgas -------------------------------------------

    subroutine gettgas(iroom,x,y,z,tg)

    !     routine: gettgas
    !     purpose: routine to calculate gas temperature nearby a target
    !     arguments: iroom  compartment number
    !                x  x position of target in compartmentnumber
    !                y  y position of target in compartmentnumber
    !                z  z position of target in compartment
    !                tg (output)   calculated gas temperature

    use precision_parameters
    use fireptrs
    use cfast_main
    use cenviro
    use objects2
    implicit none

    integer, intent(in) :: iroom
    real(eb), intent(in) :: x, y, z
    
    real(eb), intent(out) :: tg
        
    real(eb) :: qdot, xrad, area, tu, tl, zfire, zlayer, zceil, r, tplume, tplume_ceiling, tcj

    integer :: i

    ! default is the appropriate layer temperature
    if (z>=zzhlay(iroom,lower)) then
        tg = zztemp(iroom,upper)
    else
        tg = zztemp(iroom,lower)
    endif

    ! if there is a fire in the room and the target is DIRECTLY above the fire, use plume temperature
    do i = 1,nfire
        if (ifroom(i)==iroom) then
            qdot = fqf(i)
            xrad = radconsplit(i)
            area = farea(i)
            tu = zztemp(iroom,upper)
            tl = zztemp(iroom,lower)
            zfire = xfire(i,f_fire_zpos)
            zlayer = zzhlay(iroom,lower)
            zceil = hr(iroom)
            r = sqrt((x-xfire(i,f_fire_xpos))**2 + (y-xfire(i,f_fire_ypos))**2)
            ! first calculate plume temperature at desired location
            call get_plume_temperature (qdot, xrad, area, tu, tl, zfire, zlayer, z, r, tplume)
            ! include ceiling jet effects if desired location is in the ceiling jet
            call get_plume_temperature (qdot, xrad, area, tu, tl, zfire, zlayer, zceil, 0.0_eb, tplume_ceiling)
            call get_ceilingjet_temperature(qdot, tu, tl, tplume_ceiling, zfire, zlayer, zceil, z, r, tcj)
            tg = max(tg,tplume,tcj)
        endif
    end do
    end subroutine gettgas  
    
! --------------------------- get_ceilingjet_temperature --------------------------------------
    
    subroutine get_ceilingjet_temperature (qdot, tu, tl, tplume, zfire, zlayer, zceil, zin, r, tcj)

    !     routine: get_ceilingjet_temperature
    !     purpose: Calculates ceiling jet temperature at a specified height and distance from the fire.
    !
    !     Uses Alpert / Heskestad's correlation to calculate plume  temperature
    !     arguments:  qdot: total heat release rate of the fire (W)
    !                 tu: upper layer gas temperature (K)
    !                 tl: lower layer gas temperature (K)
    !                 tplume: plume temperature at ceiling (K)
    !                 zfire: height of the base of the fire (m)
    !                 zlayer: height of the hot/cold gas layer interface (m)
    !                 zceil: height of the compartment ceiling (m)
    !                 zin: position to calculate plume centerline temperature (m)
    !                 r: horizontal distance from fire centerline
    !                 tcj (output): plume centerline temperature (K)

    use precision_parameters
    implicit none

    real(eb), intent(in) :: qdot, tu, tl, tplume, zfire, zlayer, zceil, zin, r
    real(eb), intent(out) :: tcj
        
    real(eb), parameter :: cp = 1.012_eb
    real(eb), parameter :: deltaT_0star_at_p2 = (0.225_eb+0.27_eb*0.2_eb)**(-4._eb/3._eb)
    real(eb) :: t_inf, t_layer, rho_inf, qstar_h, h, delta_cj, correction_factor

    !     for the algorithm to work, there has to be a fire, two layers, and a target point about the fire     
    h = zceil - zfire 
    if (qdot>0.0_eb.and.tu>=tl.and.h>=0.0_eb) then
        if (zfire<=zlayer) then
            ! fire is in the lower layer
            t_inf = tl
        else
            t_inf = tu
        end if
        
        if (zin<=zlayer) then
            ! desired point is in the lower layer
            t_layer = tl
        else
            t_layer = tu
        end if
        
        tcj = t_layer
        if (r/h>=0.26_eb) then
            delta_cj = h * 0.112_eb*(1.0_eb-exp(-2.24_eb*r/h))
        else
            delta_cj = h * 0.112_eb*(1.0_eb-exp(-2.24_eb*0.26_eb))
        end if
        if (zin>=zceil-delta_cj) then
            rho_inf = 352.981915_eb/t_inf
            qstar_h = (qdot/1000._eb)/(rho_inf*cp*t_inf*gsqrt*h**2.5_eb)
            if (r/h<0.2_eb) then
                tcj = tplume
            else
                correction_factor = (tplume/t_layer-1.0_eb)/(deltaT_0star_at_p2*qstar_h**twothirds)
                tcj = t_layer + t_layer * correction_factor * qstar_h**twothirds * (0.225_eb+0.27_eb*r/h)**(-4.0_eb/3.0_eb)
            end if
        end if
    end if
    
    return
    end subroutine get_ceilingjet_temperature
    
! --------------------------- get_plume_temperature -------------------------------------------

    subroutine get_plume_temperature (qdot, xrad, area, tu, tl, zfire, zlayer, zin, r, tplume)

    !     routine: get_plume_temperature
    !     purpose: Calculates plume centerline temperature at a specified height and distance from the fire.
    !
    !     Uses Heskestad's correlation to calculate plume  temperature
    !     with Evan's method to determine virtual fire size and fire origin when fire
    !     is in the lower layer and position is in the upper layer
    !     arguments:  qdot: total heat release rate of the fire (W)
    !                 xrad: fraction of fire HRR released as radiation
    !                 area: fire diamater (m)
    !                 tu: upper layer gas temperature (K)
    !                 tl: lower layer gas temperature (K)
    !                 zfire: height of the base of the fire (m)
    !                 zlayer: height of the hot/cold gas layer interface (m)
    !                 zin: position to calculate plume centerline temperature (m)
    !                 r: horizontal distance from fire centerline
    !                 tplume (output): plume centerline temperature

    use precision_parameters
    implicit none
    
    real(eb), intent(in) :: qdot, xrad, area, tu, tl, zfire, zlayer, zin, r
    real(eb), intent(out) :: tplume

    real(eb), parameter :: cp = 1.012
    real(eb) :: t_inf, rho, qdot_c, qstar, z0, z0_prime, z_flame, deltaz, d, t_excess, sigma_deltat


    ! default is for temperature to be the layer temperature at the desired location
    if (zin<=zlayer) then
        tplume = tl
    else
        tplume = tu
    endif

    ! for the algorithm to work, there has to be a fire, two layers, and a target point above the fire
    if (qdot>0.0_eb.and.tu>=tl.and.zin-zfire>=0.0_eb) then
        qdot_c = qdot*(1.0_eb - xrad)/1000.0_eb
        d = sqrt(area/pio4)
      
        if (zfire<=zlayer) then
            t_inf = tl
        else
            t_inf = tu
        end if
        
        rho = 352.981915_eb/t_inf
        qstar = (qdot/1000._eb)/(rho*cp*t_inf*gsqrt*d**2.5_eb)
        z0 = d*(-1.02_eb+1.4_eb*qstar**0.4_eb)
        z_flame = d*(-1.02_eb+3.7_eb*qstar**0.4_eb)
        
        if (zfire<=zlayer.and.zin>zlayer) then
            ! fire is in lower and and target point is in upper layer
            z0_prime = zlayer-(tu/tl)**0.6_eb * (zlayer-z0)
            rho = 352.981915_eb/tu
            deltaz = max(0.0001_eb,zin-zfire-z0_prime)
            t_excess = min(900._eb,9.1_eb*(tu/(grav_con*cp**2*rho**2))**onethird * qdot_c**twothirds * deltaz**(-5.0_eb/3.0_eb))
        else
            ! fire and target point are both in lower layer
            deltaz = max(0.0001_eb,zin-zfire-z0)
            t_excess = min(900._eb,9.1_eb*(t_inf/(grav_con*cp**2*rho**2))**onethird * qdot_c**twothirds * deltaz**(-5.0_eb/3.0_eb))
        end if

        ! if it's within the flame (assumed to be a cone of diameter d and height equal to flame height, it's flame temperature
        if (r<d*(1.0_eb-(zin-zfire)/z_flame)/2.0_eb) then
            t_excess = 900._eb
        else if (r>0.0_eb) then
            sigma_deltat = 0.14_eb * sqrt(1.0_eb + t_excess/t_inf) * deltaz
            t_excess = t_excess*exp(-(r/sigma_deltat)**2)
        end if
        tplume = t_inf + t_excess
    endif  
    return
    end subroutine get_plume_temperature

! --------------------------- update_species -------------------------------------------

    subroutine update_species (deltt)

    !     routine: update_species
    !     purpose: calculate species concentrations (ppm), mass density (kg/m^3), opacity (1/m), 
    !              ct (g-min/m^3), heat flux to target on floor (w)
    !     arguments:  deltt  length of the latest time step (s)

    use precision_parameters
    use cenviro
    use cfast_main
    use params
    implicit none

    real(eb), intent(in) :: deltt
    
    real(eb) :: aweigh(ns), air(2), v(2), aweigh7, avagad
    integer i, k, lsp
    logical ppmcal(ns)

    ! aweigh's are molar weights of the species, avagad is the reciprocal
    ! of avagadro's number (so you can't have less than an atom of a species
    data aweigh, aweigh7 /28.0_eb, 32.0_eb, 44.0_eb, 28.0_eb, 27.0_eb, 37.0_eb, 12.0_eb, 18.0_eb, 12.0_eb, 0.0_eb, 0.0_eb, 12.0_eb/
    data avagad /1.66e-24_eb/
    data ppmcal /3*.false., 3*.true., 5*.false./
    aweigh(7) = aweigh7*(1.0_eb+hcratt)

    do i = 1, nm1
        v(upper) = zzvol(i,upper)
        v(lower) = zzvol(i,lower)
        do k = upper, lower
            air(k) = 0.0_eb
            do lsp = 1, 9
                air(k) = air(k) + zzgspec(i,k,lsp)/aweigh(lsp)
            end do
            air(k) = max(avagad,air(k))
        end do

        ! calcluate the mass density in kg/m^3
        do lsp = 1, ns
            if (activs(lsp)) then
                do k = upper, lower
                    ppmdv(k,i,lsp) = zzgspec(i,k,lsp)/v(k)
                end do
            endif
        end do

        ! calculate the molar density
        do lsp = 1, 8
            if (activs(lsp)) then
                do k = upper, lower
                    if (ppmcal(lsp)) then
                        toxict(i,k,lsp) = 1.0e+6_eb*zzgspec(i,k,lsp)/(air(k)*aweigh(lsp))
                    else
                        toxict(i,k,lsp) = 100.0_eb*zzgspec(i,k,lsp)/(air(k)*aweigh(lsp))
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
                toxict(i,k,lsp) = ppmdv(k,i,lsp)*3778.0_eb
            end do
        endif

        ! ct is the integration of the total "junk" being transported
        lsp = 10
        if (activs(lsp)) then
            do k = upper, lower
                toxict(i,k,lsp) = toxict(i,k,lsp) + ppmdv(k,i,lsp)*1000.0_eb*deltt/60.0_eb
            end do
        endif

        ! ts (trace species) is the filtered concentration - this is the total mass. 
        ! it is converted to fraction of the total generated by all fires.
        ! this step being correct depends on the integratemass routine
        lsp = 11
        if (activs(lsp)) then
            do k = upper, lower
                toxict(i,k,lsp) = zzgspec(i,k,lsp) !/(tradio+1.0d-10)
            end do
        endif

    end do

    ! ontarget is the radiation received on a target on the floor
    do i = 1, nm1
        ontarget(i) = sigma*(zztemp(i,upper)**4-interior_temperature**4)
        if (ontarget(i)<1.0_eb) ontarget(i) = 0.0_eb
    end do
    return
    end subroutine update_species

! --------------------------- remap_fires -------------------------------------------

    subroutine remap_fires (nfires)

    ! this routine is to combine fire objects into a single list
    ! there does not have to be a main fire nor any objects, so nfires may be zero

    use precision_parameters
    use cfast_main
    use smkview
    implicit none

    integer, intent(out) :: nfires
    
    real(eb) :: fheight
    integer :: i

    nfires = 0

    ! now the other objects
    do i = 1, numobjl
        nfires = nfires + 1
        fxlocal(nfires) = fopos(1,i)
        fylocal(nfires) = fopos(2,i)
        fzlocal(nfires) = fopos(3,i)
        call flame_height (fqf(i),farea(i),fheight)
        fqlocal(nfires) = fqf(i)
        fhlocal(nfires) = fheight
        flocal(nfires) = froom(i)
    end do
    return
    end

! --------------------------- set_heat_of_combustion -------------------------------------------

    subroutine set_heat_of_combustion (maxint, mdot, qdot, hdot, hinitial)

    !	Routine to implement the algorithm to set the heat of combustion for all fires

    use precision_parameters
    implicit none

    integer, intent(in) :: maxint
    real(eb), intent(in) :: qdot(maxint), hinitial
    real(eb), intent(out) :: mdot(maxint), hdot(maxint)
    
    integer :: i
    real(eb) :: hcmax = 1.0e8_eb, hcmin = 1.0e6_eb

    do i = 1, maxint
        if(i>1) then
            if (mdot(i)*qdot(i)<=0.0_eb) then
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
    
    end subroutine set_heat_of_combustion

! --------------------------- update_fire_objects -------------------------------------------

    subroutine update_fire_objects(iflag, told, dt, ifobj, tobj, ierror)

    !     routine: update_fire_objects
    !     purpose: check for and set object fire ignition
    !     arguments:  iflag   flags if check, set, or update variables
    !                 told    time previous to this time step
    !                 dt      length of last time step
    !                 ifobj   object number that ignites (return)
    !                 tobj    time object ignites
    !                 ierror  returns error codes

    use precision_parameters
    use cparams
    use cfast_main
    use fltarget
    use objects2
    use opt
    implicit none
    
    integer, intent(in) :: iflag
    integer, intent(out) :: ifobj
    integer, intent(out) :: ierror
    real(eb), intent(in) :: told, dt
    real(eb), intent(out) :: tobj
    
    real(eb) :: tmpob(2,mxfires), tnobj
    
    integer :: iobj, ignflg, iobtarg

    ifobj = 0
    tobj = told + 2.0_eb*dt
    tnobj = told + dt

    ! note that ignition type 1 is time, type 2 is temperature and 3 is flux !!! the critiria for temperature and flux are stored backupwards - this is historical
    ! see corresponding code in keywordcases
    do iobj = 1, numobjl
        if (.not.objon(iobj)) then
            ignflg = objign(iobj)
            iobtarg = obtarg(iobj)
            if (ignflg==1) then
                if (objcri(1,iobj)<=tnobj) then
                    tobj = min(objcri(1,iobj),tobj)
                    ifobj = iobj
                    tmpob(1,iobj) = 1.0_eb
                    tmpob(2,iobj) = objcri(1,iobj)
                else
                    tmpob(1,iobj) = 0.0_eb
                    tmpob(2,iobj) = tnobj + dt
                endif
            else if (ignflg==2) then
                call check_object_ignition(told,dt,xxtarg(idxtempf_trg,iobtarg),objcri(3,iobj),obcond(igntemp,iobj),iobj,ifobj,tobj,tmpob(1,iobj))
            else if (ignflg==3) then
                call check_object_ignition(told,dt,xxtarg(trgtfluxf,iobtarg),objcri(2,iobj),obcond(ignflux,iobj),iobj,ifobj,tobj,tmpob(1,iobj))
            else
                call xerror('update_fire_objects-incorrectly defined object type',0,1,1)
                ierror = 20
                return
            endif
        endif
    end do

    if (iflag/=check_detector_state) then
        do iobj = 1, numobjl
            if (.not.objon(iobj)) then
                iobtarg = obtarg(iobj)
                obcond(igntemp,iobj) = xxtarg(idxtempf_trg,iobtarg)
                obcond(ignflux,iobj) = xxtarg(trgtfluxf,iobtarg)
                if (iflag==set_detector_state.and.tmpob(1,iobj)>0.0_eb) then
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

! --------------------------- check_object_ignition -------------------------------------------

    subroutine check_object_ignition(told, dt, cond, trip, oldcond, iobj,ifobj, tobj, tmpob)

    use precision_parameters
    implicit none

    integer, intent(in) :: iobj
    real(eb), intent(in) :: told, dt, cond, trip, oldcond
    
    integer, intent(out) :: ifobj
    real(eb), intent(out) :: tmpob(2)
    real(eb), intent(inout) :: tobj
    
    real(eb) :: delta

    if (cond>trip) then
        delta = (trip-oldcond)/(cond-oldcond)
        tmpob(1) = 1.0_eb
        tmpob(2) = told + dt*delta
        tobj = min(tobj,tmpob(2))
        ifobj = iobj
    else
        tmpob(1) = 0.0_eb
        tmpob(2) = told + 2.0_eb*dt
    endif

    return

    end subroutine check_object_ignition

! --------------------------- rev_fire -------------------------------------------

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