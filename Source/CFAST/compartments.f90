 module compartment_routines

    use precision_parameters
    use cparams
    use cenviro
    use room_data
    use option_data
    use vent_data
    use utility_routines
    use opening_fractions, only : get_vent_opening

    implicit none

    private

    public layer_mixing, synchronize_species_mass, room_connections

    contains

    subroutine layer_mixing (flows_layer_mixing)

    !     routine:    layer_mixing
    !     function:   interface between calculate_residuals and convective_flux.  loops over rooms
    !                 setting up varibles.  passes to convective_flux if ceiling jet for
    !                 a surface is off, otherwise sets fluxes_convection to 0.0 and then
    !                 solves for flows_convection
    !     outputs:    flows_layer_mixing      net enthalphy and mass into each layer


    real(eb), intent(out) :: flows_layer_mixing(mxrooms, ns+2, 2)

    real(eb) :: fm, spmass, ft

    integer iroom, lsp
    type(room_type), pointer :: roomptr


    flows_layer_mixing(1:nrm1,1:ns+2,u) = 0.0_eb
    flows_layer_mixing(1:nrm1,1:ns+2,l) = 0.0_eb
    
    if (option(flayermixing)==off) return
    
    do iroom = 1, nrm1
        roomptr => roominfo(iroom)
        !ft = tanhsmooth(roomptr%temp(l),interior_ambient_temperature+100.0_eb,interior_ambient_temperature,1.0_eb,0.0_eb)
        ft = 0.0_eb
        if (roomptr%temp(l)+ft > roomptr%temp(u)) then
            flows_layer_mixing(iroom,q,u) = 1000.0_eb*(roomptr%temp(l)+ft - roomptr%temp(u))**2
            flows_layer_mixing(iroom,q,u) = roomptr%wall_area4(1)*flows_layer_mixing(iroom,q,u)
            flows_layer_mixing(iroom,q,l) = -flows_layer_mixing(iroom,q,u)
            fm = tanhsmooth(roomptr%mass(l),2*roomptr%vmin*roomptr%rho(l),roomptr%vmin*roomptr%rho(l),0.5_eb,0.0_eb)
            flows_layer_mixing(iroom,m,u) = fm*flows_layer_mixing(iroom,q,u)/(cp*roomptr%temp(l))
            flows_layer_mixing(iroom,m,l) = -flows_layer_mixing(iroom,m,u)
            spmass = 0.0_eb
            do lsp = 1, ns_mass
                spmass = spmass + roomptr%species_mass(l,lsp)
            end do
            do lsp = 1, ns
                flows_layer_mixing(iroom,lsp+2,u) = flows_layer_mixing(iroom,m,u)/spmass*roomptr%species_mass(l,lsp)
                flows_layer_mixing(iroom,lsp+2,l) = -flows_layer_mixing(iroom,lsp+2,u)
            end do
        end if
    end do
    
    return
    end subroutine layer_mixing

! --------------------------- synchronize_species_mass -------------------------------------------

    subroutine synchronize_species_mass (pdif,ibeg)

    !     routine: synchronize_species_mass
    !     purpose: resyncronize the total mass of the
    !              species with that of the total mass to insure overall and individual mass balance

    !     arguments: pdif   the p array to synchronize_species_mass
    !                ibeg   the point at which species are started in p array

    integer, intent(in) :: ibeg
    real(eb), intent(out) :: pdif(*)

    real(eb) :: factor(mxrooms,2), smoke(mxrooms,2)
    integer :: iroom, isof, iprod

    factor(1:nrm1,u) = 0.0_eb
    factor(1:nrm1,l) = 0.0_eb
    smoke(1:nrm1,1:2) = 0.0_eb

    isof = ibeg + 2*nrm1
    do iprod = 2, ns_mass
        do iroom = 1, nrm1
            if (pdif(isof) >= 0.0_eb) then
                factor(iroom,u) = factor(iroom,u) + pdif(isof)
            else
                pdif(isof) = 0.0_eb
            end if
            isof = isof + 1
            if (pdif(isof) >= 0.0_eb) then
                factor(iroom,l) = factor(iroom,l) + pdif(isof)
            else
                pdif(isof) = 0.0_eb
            end if
            isof = isof + 1
        end do
    end do
    
    do iprod = 1, 2
        do iroom = 1, nrm1
            if (pdif(isof) >= 0.0_eb) then
                smoke(iroom, u) = smoke(iroom,u) + pdif(isof)
            else
                pdif(isof) = 0.0_eb
            end if
            isof = isof + 1
            if (pdif(isof) >= 0.0_eb) then
                smoke(iroom, l) = smoke(iroom,l) + pdif(isof)
            else
                pdif(isof) = 0.0_eb
            end if
            isof = isof + 1
        end do
    end do 

    isof = ibeg
    do iroom = 1, nrm1
        pdif(isof) = roominfo(iroom)%mass(u) - factor(iroom,u)
        isof = isof + 1
        pdif(isof) = roominfo(iroom)%mass(l) - factor(iroom,l)
        isof = isof + 1
    end do
    
    isof = ibeg + 2*(ns_mass - 1)*nrm1
    do iroom = 1, nrm1
        if (smoke(iroom,u) > 0.0_eb) then
            smoke(iroom,u) = pdif(isof)/smoke(iroom,u)
        else
            smoke(iroom,u) = 1.0_eb
        end if
        isof = isof + 1
        if (smoke(iroom,l) > 0.0_eb) then
            smoke(iroom,l) = pdif(isof)/smoke(iroom,l)
        else
            smoke(iroom,l) = 1.0_eb
        end if
        isof = isof + 1
    end do
    
    do iprod = 1, 2
        do iroom = 1, nrm1
            pdif(isof) = pdif(isof) * smoke(iroom,u)
            isof  = isof + 1
            pdif(isof) = pdif(isof) * smoke(iroom,l)
            isof  = isof + 1
        end do
    end do

    return

    end subroutine synchronize_species_mass

! --------------------------- room_connections -------------------------------------------

    subroutine room_connections (tsec)

    ! routine: room_connections
    ! purpose: this routine determines whether flow from each room can reach the outside (perhaps through intermediate rooms)
    !           via horizontal or vertical vents.  if a room is isolated from the outside then snsqe has trouble finding an
    !           initial pressure solution.
    ! arguments: tsec: current simulation time

    real(eb), intent(in) :: tsec

    real(eb) :: fraction, height, width, avent
    integer roomc(mxrooms,mxrooms), tempmat(mxrooms,mxrooms), i, iroom1, iroom2, ik, im, ix, matiter
    integer, parameter :: toprm = 1, botrm = 2
    character(64) :: rampid

    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    ! initially assume that no rooms are connected
    roomc(1:nr,1:nr) = 0
    do i = 1, nr
        roomc(i,i) = 1
    end do

    ! check horizontal vent flow
    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        iroom1 = ventptr%room1
        iroom2 = ventptr%room2
        ik = ventptr%counter
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        rampid = ventptr%ramp_id
        call get_vent_opening (rampid,'H',im,ix,ik,i,tsec,fraction)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = fraction*height*width
        if (avent/=0.0_eb) then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        end if
    end do

    ! check vertical vent flow
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        if (ventptr%current_area/=0.0_eb) then
            roomc(iroom1,iroom2) = 1
            roomc(iroom2,iroom1) = 1
        end if
    end do

    ! construct roomc**matiter where matiter > nr
    ! note:  roomc is a transitiion matrix (from markov chain theory). that is, roomc(i,j) is zero if there no connection
    !        between room and room j.  similarly, roomc(i,j) is one if there is a connection between these two rooms.
    !        roomc is symmetric. the matrix roomc**2 is tells us whether flow can get from room i to room j in two steps.
    !        since there are only nr rooms, roomc**nr tells us whether any given room is connected to any
    !        other room in nr steps. the entries roomc**nr(i,nr) then indicates whether a room is connected
    !        to the outside (perhaps through several other intermediate rooms).
    matiter = 1
    do i = 1, nr
        if (nr<=matiter) exit
        call mat2mult(roomc,tempmat,mxrooms,nr)
        matiter = matiter*2
    end do

    do i = 1, nrm1
        roomptr => roominfo(i)
        if (roomc(i,nr)/=0) then
            roomptr%is_connection = .true.
        else
            roomptr%is_connection = .false.
        end if
    end do

    return
    end subroutine room_connections

 end module compartment_routines
    