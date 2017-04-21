 module convection_routines

    use precision_parameters
    use cparams
    use wallptrs
    use cenviro
    use ramp_data
    use fire_data, only: n_fires, fireinfo
    use room_data
    use option_data

    implicit none

    private

    public convection, convective_flux

    contains

    subroutine convection (flows_convection,fluxes_convection)

    !     routine:    convection
    !     function:   interface between calculate_residuals and convective_flux.  loops over rooms
    !                 setting up varibles.  passes to convective_flux if ceiling jet for
    !                 a surface is off, otherwise sets fluxes_convection to 0.0 and then
    !                 solves for flows_convection
    !     outputs:    flows_convection       net enthalphy into each layer
    !                 fluxes_convection       net heat flux onto surface


    real(eb), intent(out) :: flows_convection(mxrooms,2), fluxes_convection(mxrooms,nwal)

    real(eb) :: qconv, qconv_avg, lw_eff

    integer i, iwall, iw, ilay, ifire
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr


    flows_convection(1:nrm1,u) = 0.0_eb
    flows_convection(1:nrm1,l) = 0.0_eb
    fluxes_convection(1:nrm1,1:nwal) = 0.0_eb

    if (option(fconvec)/=on) return

    ! calculate convection for all surfaces in all rooms
    do iw = 1, nhcons
        i = i_hconnections(iw,w_from_room)
        roomptr => roominfo(i)
        iwall = i_hconnections(iw,w_from_wall)
        if (mod(iwall,2)==1) then
            ilay = u
        else
            ilay = l
        end if
        ! assume no fires in this room.  just use regular convection
        call convective_flux(iwall,roomptr%temp(ilay),roomptr%t_surfaces(1,iwall),fluxes_convection(i,iwall))
        ! if there's a fire, we may need to modify the convection to account for the ceiling jet
        if (iwall==1.and.n_fires>0) then
            qconv = 0.0_eb
            do ifire = 1, n_fires
                fireptr => fireinfo(ifire)
                if (fireptr%room==i) then
                    qconv = max(qconv,fireptr%qdot_convective)
                end if
            end do
            lw_eff = min((4.0_eb*roomptr%cheight)**2*pi,roomptr%cwidth*roomptr%cdepth)
            qconv_avg = 0.27_eb*qconv/((lw_eff)**0.68_eb*roomptr%cheight**0.64_eb)
            if (qconv_avg>fluxes_convection(i,iwall)) fluxes_convection(i,iwall) = qconv_avg
        end if
        flows_convection(i,ilay) = flows_convection(i,ilay) - roomptr%wall_area4(iwall)*fluxes_convection(i,iwall)

    end do

    return
    end subroutine convection

! --------------------------- convective_flux -------------------------------------------

    subroutine convective_flux (iw,tg,tw,qdinl)

    !     routine: convective_flux
    !     purpose: calculate convective heat transfer for a wall segment.
    !     arguments:  iw     wall number, standand cfast numbering convention
    !                 tg     temperature of gas layer adjacent to wall surface
    !                 tw     wall surface temperature
    !                 qdinl  convective flux into wall surface iw

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
    end subroutine convective_flux

 end module convection_routines
