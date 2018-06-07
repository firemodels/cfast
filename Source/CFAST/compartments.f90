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

    public layer_mixing, synchronize_species_mass, room_connections, wall_opening_fraction

    contains

    subroutine layer_mixing (flows_layer_mixing)

    !     routine:    layer_mixing
    !     function:   interface between calculate_residuals and single line layer mixing model.  
    !                 loops over room setting up varibles, does calculation and fills in data
    !                 structures with results. 
    !     outputs:    flows_layer_mixing      net enthalphy and mass into each layer


    real(eb), intent(out) :: flows_layer_mixing(mxrooms, ns+2, 2)

    real(eb) :: fm, spmass

    integer iroom, lsp
    type(room_type), pointer :: roomptr


    flows_layer_mixing(1:nrm1,1:ns+2,u) = 0.0_eb
    flows_layer_mixing(1:nrm1,1:ns+2,l) = 0.0_eb
    
    if (option(flayermixing)==off) return
    
    do iroom = 1, nrm1
        roomptr => roominfo(iroom)
        if (roomptr%temp(l) > roomptr%temp(u)) then
            flows_layer_mixing(iroom,q,u) = 1000.0_eb*(roomptr%temp(l) - roomptr%temp(u))**2
            flows_layer_mixing(iroom,q,u) = roomptr%wall_area4(1)*flows_layer_mixing(iroom,q,u)
            flows_layer_mixing(iroom,q,l) = -flows_layer_mixing(iroom,q,u)
            fm = tanhsmooth(roomptr%mass(l),2*roomptr%vmin*roomptr%rho(l),roomptr%vmin*roomptr%rho(l),0.5_eb,0.0_eb)
            flows_layer_mixing(iroom,m,u) = fm*flows_layer_mixing(iroom,q,u)/(cp*roomptr%temp(l))
            flows_layer_mixing(iroom,m,l) = -flows_layer_mixing(iroom,m,u)
            spmass = 0.0_eb
            do lsp = 1, ns_mass
                spmass = spmass + roomptr%species_mass(l,lsp)
            end do
            if (spmass > 0.0_eb) then
                do lsp = 1, ns
                    flows_layer_mixing(iroom,lsp+2,u) = flows_layer_mixing(iroom,m,u)/spmass*roomptr%species_mass(l,lsp)
                    flows_layer_mixing(iroom,lsp+2,l) = -flows_layer_mixing(iroom,lsp+2,u)
                end do
            else
                flows_layer_mixing(iroom,m,u) = 0.0_eb
                flows_layer_mixing(iroom,m,l) = 0.0_eb
            end if 
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

! ---------------------------- wall_opening_fraction -------------------------------------------

    subroutine wall_opening_fraction (tsec)

    !     purpose: to calculate the opening ratio of a surface
    
    !     note:
    !     surface number associated with ceiling, upper front, upper right, upper rear, upper left
    !                                       lower front, lower right, lower rear, lower left, floor 
    !     is denoted as 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 and this is different than logic in target subroutine

    real(eb), intent(in) :: tsec          ! current simulation    
    integer :: i, j, k                    ! counter
    integer :: side                       ! surface number (10 surfaces in total)
    integer :: map(4) = (/3, 4, 1, 2/)    ! surface correction mapping
    integer :: top, bottom                ! intermediate integers used to keep track of the compartment number in vertical vents
    integer :: from, to                   ! intermediate integers used to keep track of the compartment number in mechanical vertical vents
    real(eb) :: A_total(10)               ! total surface area
    real(eb) :: A_opening(10)             ! total opening area for a surface
    real(eb) :: temp_opening              ! intermediate value to store an opening area for a surface
    real(eb) :: fraction                  ! opening fraction (0 to 1)
    real(eb) :: length                    ! length of the square opening for mechanical horizontal (wall) vents
    real(eb) :: lowest, highest           ! sill and soffit of an opening (be aware that there are 2 reference points
    real(eb) :: abs_cheight               ! absolute height for the compartment
    
    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr
        
    do i = 1, nrm1
        A_opening(:) = 0._eb
        A_total (:)  = 0._eb
        
        roomptr => roominfo(i)
    
        A_total(1)  = roomptr%cwidth*roomptr%cdepth
        A_total(2)  = roomptr%cwidth*roomptr%depth(u)
        A_total(3)  = roomptr%cdepth*roomptr%depth(u)
        A_total(4)  = A_total(2)
        A_total(5)  = A_total(3)
        A_total(6)  = roomptr%cwidth*roomptr%depth(l)
        A_total(7)  = roomptr%cdepth*roomptr%depth(l)
        A_total(8)  = A_total(6)
        A_total(9)  = A_total(7)
        A_total(10) = A_total(1)
        
        ! Determine areas for horizontal (wall) vents
        do j = 1, n_hvents
            ventptr=>hventinfo(j)
            if (ventptr%room1 == i .or. ventptr%room2 == i) then
                fraction = 0._eb
                call get_vent_opening (ventptr%ramp_id,'H',ventptr%room1,ventptr%room2,ventptr%counter,j,tsec,fraction)
                        
                ! identify surface number
                if (ventptr%room1 == i .or. ventptr%room1 == nrm1+1) then
                    side = ventptr%face
                else if (ventptr%room2 == i) then 
                ! correct location for vent in room i because vent info is given as it is in room2
                    side = map(ventptr%face)
                end if 
                        
                do k = 1, 2
                    if (k == 1) then
                        ! reference point for z is at roomptr%cheight
                        abs_cheight = roomptr%z0 + roomptr%cheight
                        lowest  = abs_cheight - ventptr%absolute_soffit
                        highest = abs_cheight - ventptr%absolute_sill
                        
                        if (roomptr%depth(k) < lowest) then
                            temp_opening = 0._eb
                        else if (roomptr%depth(k) >= lowest .and. roomptr%depth(k) <= highest) then
                            temp_opening = (roomptr%depth(k) - lowest) * ventptr%width
                        else if (roomptr%depth(k) > highest) then
                            temp_opening = (highest - lowest) * ventptr%width
                        end if
                        
                        A_opening(side+1) = A_opening(side+1) + fraction*temp_opening  
                    else if (k == 2) then
                        ! reference point for z is at 0
                        abs_cheight = roomptr%z0 + roomptr%cheight
                        lowest  = ventptr%absolute_sill - roomptr%z0
                        highest = ventptr%absolute_soffit - roomptr%z0
                        
                        if (roomptr%depth(k) < lowest) then
                            temp_opening = 0._eb
                        else if (roomptr%depth(k) >= lowest .and. roomptr%depth(k) <= highest) then
                            temp_opening = (roomptr%depth(k) - lowest) * ventptr%width
                        else if (roomptr%depth(k) > highest) then
                            temp_opening = (highest - lowest) * ventptr%width
                        end if
                        
                        A_opening(side+5) = A_opening(side+5) + fraction*temp_opening  
                    end if                   
                end do
            end if
        end do
        
        ! Determine areas for mechanical vents
        do j = 1, n_mvents
            ventptr=>mventinfo(j)
            ! For horizontal (ceiling/floor) mechanical vents
            if ((ventptr%room1 == i .or. ventptr%room2 == i) .and. ventptr%orientation(1) == 2) then
                fraction = 0._eb
                call get_vent_opening (ventptr%ramp_id,'M',ventptr%room1,ventptr%room2,ventptr%counter,j,tsec,fraction)
                
                from = ventptr%room1
                to = ventptr%room2
                                
                side = 0
                if (from <= nrm1 .and. to == nrm1 + 1) then
                    side = 1
                else if (to <= nrm1 .and. from == nrm1 + 1) then
                    side = 10
                else if (roominfo(from)%z0 > roominfo(to)%z0) then 
                    side = 10
                    if (ventptr%room1 == i .or. ventptr%room1 == nrm1+1) then
                        continue
                    else if (ventptr%room2 == i) then 
                        ! correct location for vent in room i because vent info is given as it is in room2
                        if (side == 1) then
                            side = 10
                        else if (side == 10) then
                            side = 1
                        end if
                    end if
                else if (roominfo(from)%z0 < roominfo(to)%z0) then
                    side = 1
                    if (ventptr%room1 == i .or. ventptr%room1 == nrm1+1) then
                        continue
                    else if (ventptr%room2 == i) then 
                        ! correct location for vent in room i because vent info is given as it is in room2
                        if (side == 1) then
                            side = 10
                        else if (side == 10) then
                            side = 1
                        end if
                    end if
                end if
                
                temp_opening = ventptr%diffuser_area(1)
                if (side .ne. 0) A_opening(side) = A_opening(side) + fraction*temp_opening
                
            ! For vertical (wall) mechanical vents
            else if ((ventptr%room1 == i .or. ventptr%room2 == i) .and. ventptr%orientation(1) == 1) then 
                fraction = 0._eb
                length = sqrt(ventptr%diffuser_area(1))
                call get_vent_opening (ventptr%ramp_id,'M',ventptr%room1,ventptr%room2,ventptr%counter,j,tsec,fraction)
                
                ! There is a possibility that the mechanical vent is not attached to any surfaces. 
                ! If this is the case, the assocated vent area will be counted as opening area for any surfaces.
                side = 0
                if (ventptr%yoffset .eq. 0._eb) side = 1
                if (ventptr%xoffset .eq. roomptr%cwidth) side = 2
                if (ventptr%yoffset .eq. roomptr%cdepth) side = 3
                if (ventptr%xoffset .eq. 0._eb) side = 4
                        
                if (side .ne. 0) then
                    if (ventptr%room1 == i .or. ventptr%room1 == nrm1+1) then
                        continue
                    else if (ventptr%room2 == i) then 
                        ! correct location for vent in room i because vent info is given as it is in room2
                        side = map(side)
                    end if
                end if
                
                do k = 1, 2                    
                    if (k == 1) then
                        ! reference point for z is at roomptr%cheight
                        abs_cheight = roomptr%z0 + roomptr%cheight
                        lowest  = abs_cheight - (roomptr%z0 + ventptr%height(1) + length/2._eb)
                        highest = abs_cheight - (roomptr%z0 + ventptr%height(1) - length/2._eb)
                        
                        if (roomptr%depth(k) < lowest) then
                            temp_opening = 0._eb
                        else if (roomptr%depth(k) >= lowest .and. roomptr%depth(k) <= highest) then
                            temp_opening = (roomptr%depth(k) - lowest) * length
                        else if (roomptr%depth(k) > highest) then
                            temp_opening = (highest - lowest) * length
                        end if
                        
                        A_opening(side+1) = A_opening(side+1) + fraction*temp_opening  
                    else if (k == 2) then
                        ! reference point for z is at 0
                        abs_cheight = roomptr%z0 + roomptr%cheight
                        lowest  = roomptr%z0 + ventptr%height(1) - length/2._eb
                        highest = roomptr%z0 + ventptr%height(1) + length/2._eb
                        
                        if (roomptr%depth(k) < lowest) then
                            temp_opening = 0._eb
                        else if (roomptr%depth(k) >= lowest .and. roomptr%depth(k) <= highest) then
                            temp_opening = (roomptr%depth(k) - lowest) * length
                        else if (roomptr%depth(k) > highest) then
                            temp_opening = (highest - lowest) * length
                        end if
                        
                        A_opening(side+5) = A_opening(side+5) + fraction*temp_opening  
                    end if                  
                end do
            end if
        end do
        
        ! Determine areas for vertical (ceiling/floor) vents
        do j = 1, n_vvents
            ventptr=>vventinfo(j)
            if (ventptr%room1 == i .or. ventptr%room2 == i) then 
                fraction = 0._eb
                call get_vent_opening (ventptr%ramp_id,'V',ventptr%room1,ventptr%room2,ventptr%counter,j,tsec,fraction)
                
                top = ventptr%room1
                bottom = ventptr%room2
                
                side = 0
                if (top <= nrm1 .and. bottom == nrm1 +1 ) then
                    side = 10
                else if (bottom <= nrm1 .and. top == nrm1 + 1) then
                    side = 1
                else if (roominfo(top)%z0 > roominfo(bottom)%z0) then 
                    side = 1
                    if (ventptr%room2 == i .or. ventptr%room2 == nrm1+1) then
                        continue
                    else if (ventptr%room1 == i) then 
                        ! correct location for vent in room i because vent info is given as it is in room2
                        if (side == 1) then
                            side = 10
                        else if (side == 10) then
                            side = 1
                        end if
                    end if
                else if (roominfo(top)%z0 < roominfo(bottom)%z0) then
                    side = 10
                    if (ventptr%room2 == i .or. ventptr%room2 == nrm1+1) then
                        continue
                    else if (ventptr%room1 == i) then 
                        ! correct location for vent in room i because vent info is given as it is in room2
                        if (side == 1) then
                            side = 10
                        else if (side == 10) then
                            side = 1
                        end if
                    end if
                end if
                
                ! Bare in mind that vent shape can be circular or rectangular
                temp_opening = ventptr%area
                if (side .ne. 0) A_opening(side) = A_opening(side) + fraction*temp_opening
            end if
        end do
        
        roomptr%chi(:) = A_opening(:)/A_total(:)
    end do

    return
    end subroutine wall_opening_fraction

 end module compartment_routines
    