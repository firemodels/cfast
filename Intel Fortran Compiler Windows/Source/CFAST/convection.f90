 module convection_routines

    use precision_parameters
    
    use cfast_types, only: room_type, fire_type
    
    use cparams, only: u, l, w_from_room, w_from_wall, mxrooms, nwal
    use fire_data, only: n_fires, fireinfo
    use option_data, only: fconvec, option, on
    use room_data, only: n_rooms, roominfo, n_cons, surface_connections

    implicit none

    private

    public convection, convective_flux

    contains

! --------------------------- convection -------------------------------------------

!> \brief   interface between calculate_residuals and convective_flux.  loops over rooms setting up varibles.  
!> \        passes to convective_flux if ceiling jet for a surface is off, otherwise sets fluxes_convection to 0.0 and then
!> \        solves for flows_convection
    
!> \param   flows_convection (output): net enthalphy into each layer    
!> \param   fluxes_convection (output): net heat flux onto surface

    subroutine convection (flows_convection,fluxes_convection)

    real(eb), intent(out) :: flows_convection(mxrooms,2), fluxes_convection(mxrooms,nwal)

    real(eb) :: qconv, qconv_avg, lw_eff, w_area

    integer i, iwall, iw, ilay, ifire
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr


    flows_convection(1:n_rooms,u) = 0.0_eb
    flows_convection(1:n_rooms,l) = 0.0_eb
    fluxes_convection(1:n_rooms,1:nwal) = 0.0_eb

    if (option(fconvec)/=on) return

    ! calculate convection for all surfaces in all rooms
    do iw = 1, n_cons
        i = surface_connections(iw,w_from_room)
        roomptr => roominfo(i)
        iwall = surface_connections(iw,w_from_wall)
        if (modulo(iwall,2)==1) then
            ilay = u
        else
            ilay = l
        end if
        ! assume no fires in this room.  just use regular convection
        call convective_flux(iwall,roomptr%temp(ilay),roomptr%t_surfaces(1,iwall),fluxes_convection(i,iwall))
        w_area = roomptr%wall_area4(iwall)
        ! if there's a fire, we may need to modify the convection to account for the ceiling jet
        if (iwall==1.and.n_fires>0) then
            qconv = 0.0_eb
            ! use material_data largest fire in the room as the source for the correlation
            do ifire = 1, n_fires
                fireptr => fireinfo(ifire)
                if (fireptr%room==i) then
                    qconv = max(qconv,fireptr%qdot_convective)
                end if
            end do
            ! limit the heat transfer area to the valid limit of the correlation (r/H<4)
            lw_eff = min(pi*(4.0_eb*roomptr%cheight)**2,roomptr%cwidth*roomptr%cdepth)
            qconv_avg = 0.27_eb*qconv/(lw_eff**0.68_eb*roomptr%cheight**0.64_eb)
            if (qconv_avg>fluxes_convection(i,iwall)) then
                fluxes_convection(i,iwall) = qconv_avg
                w_area = lw_eff
            end if
        end if
        
        flows_convection(i,ilay) = flows_convection(i,ilay) - w_area*fluxes_convection(i,iwall)

    end do

    return
    end subroutine convection

! --------------------------- convective_flux -------------------------------------------
    
!> \brief   calculate convective heat transfer for a wall segment.
        
!> \param   iw (input): wall number, standand cfast numbering convention
!> \param   tg (input): temperature of gas layer adjacent to wall surface
!> \param   tw (input): wall surface temperature
!> \param   qdinl (output): convective flux into wall surface iw
    
    subroutine convective_flux (iw,tg,tw,qdinl)

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
