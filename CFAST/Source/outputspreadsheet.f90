module spreadsheet_routines
    
    use precision_parameters
    
    use fire_routines, only : flame_height
    use target_routines, only: get_target_temperatures
    use opening_fractions, only : qchfraction
    use spreadsheet_header_routines
    use utility_routines, only: ssaddtolist
    
    use precision_parameters
    use cenviro
    use cfast_main
    use cshell
    use fltarget
    use objects1
    use vents
    
    implicit none
    
    private
    
    public output_spreadsheet, output_spreadsheet_smokeview
    
    contains
    
! --------------------------- output_spreadsheet -------------------------------------------    
    
    subroutine output_spreadsheet(time)

    real(eb), intent(in) :: time

    call output_spreadsheet_normal (time)
    call output_spreadsheet_species (time)
    call output_spreadsheet_flow (time)
    call output_spreadsheet_flux (time)

    return

    end subroutine output_spreadsheet
    
! --------------------------- output_spreadsheet_normal -------------------------------------------

    subroutine output_spreadsheet_normal (time)

    ! This routine writes to the {project}_n.csv file, the compartment information and the fires

    real(eb), intent(in) :: time
    
    integer, parameter :: maxhead = 1+8*nr+5+9*mxfire
    real(eb) :: outarray(maxhead), fheight
    logical :: firstc
    integer :: position, i
    type(room_type), pointer :: roomptr

    data firstc/.true./
    save firstc

    ! headers
    if (firstc) then
        call ssHeadersNormal
        firstc = .false.
    end if

    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nm1
        roomptr => roominfo(i)
        call ssaddtolist (position,zztemp(i,upper)-kelvin_c_offset,outarray)
        if (.not.roomptr%shaft) then
            call ssaddtolist(position,zztemp(i,lower)-kelvin_c_offset,outarray)
            call ssaddtolist (position,zzhlay(i,lower),outarray)
        end if
        call ssaddtolist (position,zzvol(i,upper),outarray)
        call ssaddtolist (position,zzrelp(i) - interior_rel_pressure(i) ,outarray)
    end do

    ! Fires
    do i = 1,n
        call ssaddtolist (position,fqdj(i),outarray)
    end do

    if (numobjl/=0) then
        do i = 1, numobjl
            call flame_height (fqf(i),farea(i),fheight)
            call ssaddtolist (position,fems(i),outarray)
            call ssaddtolist (position,femp(i),outarray)
            call ssaddtolist (position,fqf(i),outarray)
            call ssaddtolist (position,fqlow(i),outarray)
            call ssaddtolist (position,fqupr(i),outarray)
            call ssaddtolist (position,fheight,outarray)
            call ssaddtolist (position,fqfc(i),outarray)
            call ssaddtolist (position,objmaspy(i),outarray)
            call ssaddtolist (position,radio(i),outarray)
        end do
    end if

    call ssprintresults (21, position, outarray)

    return
    end subroutine output_spreadsheet_normal

    subroutine ssprintresults (iounit,ic,array)

    real(eb), intent(in) :: array(*)
    integer, intent(in) :: iounit, ic
    
    integer i
    
    if (validate) then
        write (iounit,"(16384(e19.12,','))" ) (array(i),i=1,ic)
    else
        write (iounit,"(16384(e13.6,','))" ) (array(i),i=1,ic)
    end if
    return
    
    end subroutine ssprintresults

! --------------------------- output_spreadsheet_flow -------------------------------------------

    subroutine output_spreadsheet_flow (time)

    !	Routine to output the flow data to the flow spreadsheet {project}_f.csv

    integer, parameter :: maxoutput = mxhvents*4
    
    real(eb), intent(in) :: time
    
    real(eb) :: outarray(maxoutput),flow(8), sumin, sumout, netflow
    integer :: position, i, ifrom, ito, toprm = 1, botrm = 2
    type(vent_type), pointer :: ventptr
    logical :: firstc = .true.
    save firstc

    if (firstc) then
        call ssheadersflow
        firstc = .false.
    end if

    position = 0

    ! first the time
    call SSaddtolist (position,time,outarray)
        
    ! next the horizontal flow through vertical vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        ifrom = ventptr%from
        ito = ventptr%to
        netflow = ventptr%mflow(2,1,1) - ventptr%mflow(2,1,2) + ventptr%mflow(2,2,1) - ventptr%mflow(2,2,2)
        call SSaddtolist (position,netflow,outarray)
        netflow = ventptr%mflow(1,1,1) - ventptr%mflow(1,1,2) + ventptr%mflow(1,2,1) - ventptr%mflow(1,2,2)
        call SSaddtolist (position,netflow,outarray)
    end do

    ! next natural flow through horizontal vents (vertical flow)
    do i = 1, n_vvents

        ifrom = ivvent(i,botrm)
        ito = ivvent(i,toprm)

        flow = 0.0_eb
        if (vmflo(ifrom,ito,upper)>=0.0_eb) flow(5) = vmflo(ifrom,ito,upper)
        if (vmflo(ifrom,ito,upper)<0.0_eb) flow(6) = -vmflo(ifrom,ito,upper)
        if (vmflo(ifrom,ito,lower)>=0.0_eb) flow(7) = vmflo(ifrom,ito,lower)
        if (vmflo(ifrom,ito,lower)<0.0_eb) flow(8) = -vmflo(ifrom,ito,lower)
        if (vmflo(ito,ifrom,upper)>=0.0_eb) flow(1) = vmflo(ito,ifrom,upper)
        if (vmflo(ito,ifrom,upper)<0.0_eb) flow(2) = -vmflo(ito,ifrom,upper)
        if (vmflo(ito,ifrom,lower)>=0.0_eb) flow(3) = vmflo(ito,ifrom,lower)
        if (vmflo(ito,ifrom,lower)<0.0_eb) flow(4) = -vmflo(ito,ifrom,lower)

        sumin = flow(5) + flow(7)
        sumout = flow(6) + flow(8)
        netflow = sumin - sumout
        call SSaddtolist (position,netflow,outarray)
        sumin = flow(1) + flow(3)
        sumout = flow(2) + flow(4)
        netflow = sumin - sumout
        call SSaddtolist (position,netflow,outarray)
    end do

    ! finally, mechanical ventilation
    if (nnode/=0.and.next/=0) then
        do i = 1, next
            flow = 0.0_eb
            if (hveflo(upper,i)>=0.0_eb) flow(1)=hveflo(upper,i)
            if (hveflo(upper,i)<0.0_eb) flow(2)=-hveflo(upper,i)
            if (hveflo(lower,i)>=0.0_eb) flow(3)=hveflo(lower,i)
            if (hveflo(lower,i)<0.0_eb) flow(4)=-hveflo(lower,i)
            sumin = flow(1) + flow(3)
            sumout = flow(2) + flow(4)
            flow(5) =abs(tracet(upper,i))+abs(tracet(lower,i))
            flow(6) =abs(traces(upper,i))+abs(traces(lower,i))
            netflow = sumin - sumout
            call SSaddtolist (position, netflow, outarray)
            call SSaddtolist (position, flow(5), outarray)
            call SSaddtolist (position, flow(6), outarray)
        end do
    end if

    call ssprintresults(22, position, outarray)
    return

    end subroutine output_spreadsheet_flow

! --------------------------- output_spreadsheet_flux -------------------------------------------

    subroutine output_spreadsheet_flux (time)

    !     Output the temperatures and fluxes on surfaces and targets at the current time

    integer, parameter :: maxoutput=4*nr+26*mxtarg+4*mxdtect
    real(eb), intent(in) :: time
    
    real(eb) :: outarray(maxoutput), zdetect, tjet, vel, tlink, xact
    real(eb) :: tttemp, tctemp, tlay, tgtemp, cjetmin
    integer :: iwptr(4), position, i, iw, itarg, iroom
    
    type(target_type), pointer :: targptr
    
    data iwptr /1, 3, 4, 2/
    logical :: firstc
    data firstc /.true./
    save firstc

    if (firstc) then
        call ssHeadersFlux
        firstc = .false.
    end if

    position = 0

    !	First the time

    call SSaddtolist (position,time,outarray)

    !     First the surface temperatures for each compartment

    do i=1,nm1
        do iw = 1, 4
            call SSaddtolist (position,zzwtemp(i,iwptr(iw),1)-kelvin_c_offset,outarray)
        end do
    end do
    
    call get_target_temperatures

    ! now do targets if defined
    do itarg = 1, ntarg
        targptr => targetinfo(itarg)
        tgtemp = targptr%tgas
        tttemp = targptr%tfront
        tctemp = targptr%tinternal

        call SSaddtolist (position, tgtemp-kelvin_c_offset, outarray)
        call SSaddtolist (position, tttemp-kelvin_c_offset, outarray)
        call SSaddtolist (position, tctemp-kelvin_c_offset, outarray)
        ! front surface
        call SSaddtolist (position, targptr%flux_net(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_radiation(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_convection(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_fire(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_surface(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_gas(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_target(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_net_gauge(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_radiation_gauge(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_convection_gauge(1) / 1000._eb, outarray)
        call SSaddtolist (position, targptr%flux_target_gauge(1) / 1000._eb, outarray)
        ! back surface
        if (validate) then
            tttemp = targptr%tback
            call SSaddtolist (position, tttemp-kelvin_c_offset, outarray)
            call SSaddtolist (position, targptr%flux_net(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_radiation(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_convection(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_fire(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_surface(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_gas(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_target(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_net_gauge(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_radiation_gauge(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_convection_gauge(2) / 1000._eb, outarray)
            call SSaddtolist (position, targptr%flux_target_gauge(2) / 1000._eb, outarray)
        end if
    end do

    ! detectors (including sprinklers)
    cjetmin = 0.10_eb
    do i = 1, ndtect
        iroom = ixdtect(i,droom)
        zdetect = xdtect(i,dzloc)
        if(zdetect>zzhlay(iroom,lower))then
            tlay = zztemp(iroom,upper)
        else
            tlay = zztemp(iroom,lower)
        end if
        xact = ixdtect(i,dact)
        tjet = max(xdtect(i,dtjet),tlay)
        vel = max(xdtect(i,dvel),cjetmin)
        tlink =  xdtect(i,dcond)
        call SSaddtolist(position, tlink-kelvin_c_offset, outarray)
        call SSaddtolist(position, xact, outarray)
        call SSaddtolist(position, tjet-kelvin_c_offset, outarray)
        call SSaddtolist(position, vel, outarray)
    end do

    call ssprintresults (24, position, outarray)
    return

    end subroutine output_spreadsheet_flux

! --------------------------- output_spreadsheet_species -------------------------------------------

    subroutine output_spreadsheet_species (time)

    !	Write out the species to the spread sheet file

    integer, parameter :: maxhead = 1+22*nr
    real(eb), intent(in) :: time
    
    real(eb) :: outarray(maxhead), ssvalue
    integer :: position, i, lsp, layer
    logical :: tooutput(ns),  molfrac(ns), firstc
    type(room_type), pointer :: roomptr
    
    data tooutput /9*.true.,.false.,.true./ 
    data molfrac /8*.true.,3*.false./
    data firstc /.true./

    save outarray, firstc

    ! If there are no species, then don't do the output
    if (nlspct==0) return

    ! Set up the headings
    if (firstc) then
        call ssHeadersSpecies
        firstc = .false.
    end if

    ! From now on, just the data, please
    position = 0
    call SSaddtolist (position,time,outarray)

    do i = 1, nm1
        roomptr => roominfo(i)
        do layer = upper, lower
            do lsp = 1, ns
                if (layer==upper.or..not.roomptr%shaft) then
                    if (tooutput(lsp)) then
                        ssvalue = toxict(i,layer,lsp)
                        if (validate.and.molfrac(lsp)) ssvalue = ssvalue*0.01_eb ! converts ppm to  molar fraction
                        if (validate.and.lsp==9) ssvalue = ssvalue *264.6903_eb ! converts od to mg/m^3 (see toxict od calculation)
                        !ssvalue = zzgspec(i,layer,lsp) ! Use this to print out total mass of species in layers
                        call SSaddtolist (position,ssvalue,outarray)
                        ! we can only output to the maximum array size; this is not deemed to be a fatal error!
                        if (position>=maxhead) go to 90
                    end if
                end if
            end do
        end do
    end do

90  call SSprintresults (23,position, outarray)

    return

    end subroutine output_spreadsheet_species

! --------------------------- output_spreadsheet_smokeview -------------------------------------------

    subroutine output_spreadsheet_smokeview (time)

    ! This routine writes to the {project}_zone.csv file, the smokeview information

    integer, parameter :: maxhead = 1+7*nr+5+7*mxfire
    real(eb), intent(in) :: time
    
    real(eb) :: outarray(maxhead), fheight, factor2, height, width, avent, slabs, vflow
    logical :: firstc
    integer :: position
    integer :: i, j, iroom1, iroom2, ik, im, ix

    
    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    data firstc/.true./
    save firstc

    ! Headers
    if (firstc) then
        call ssHeadersSMV(.true.)
        firstc = .false.
    end if

    position = 0
    call SSaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nm1
        roomptr => roominfo(i)
        call SSaddtolist(position,zztemp(i,upper)-kelvin_c_offset,outarray)
        if (.not.roomptr%shaft) then
            call SSaddtolist(position,zztemp(i,lower)-kelvin_c_offset,outarray)
            call SSaddtolist(position,zzhlay(i,lower),outarray)
        end if
        call SSaddtolist(position,zzrelp(i),outarray)
        call SSaddtolist(position,zzrho(i,upper),outarray)
        if (.not.roomptr%shaft) call SSaddtolist(position,zzrho(i,lower),outarray)
        call SSaddtolist(position,toxict(i,upper,9),outarray)
        if (.not.roomptr%shaft) call SSaddtolist(position,toxict(i,lower,9),outarray)
    end do

    ! fires
    if (numobjl/=0) then
        do i = 1, numobjl
            call flame_height (fqf(i),farea(i),fheight)
            call SSaddtolist (position,fqf(i)/1000.,outarray)
            call SSaddtolist (position,fheight,outarray)
            call SSaddtolist (position,fopos(3,i),outarray)
            call SSaddtolist (position,farea(i),outarray)          
        end do
    end if

    ! horizontal vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)
        
        iroom1 = ventptr%from
        iroom2 = ventptr%to
        ik = ventptr%counter
        im = min(iroom1,iroom2)
        ix = max(iroom1,iroom2)
        factor2 = qchfraction (qcvh,ijk(im,ix,ik),time)
        height = ventptr%soffit - ventptr%sill
        width = ventptr%width
        avent = factor2*height*width
        ! first column is just vent area ... it's for backwards compatibility with old vent flow visualization
        call SSaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call SSaddtolist (position,slabs,outarray)
        do j = 1, mxfslab
            call ssaddtolist(position,ventptr%temp_slab(j),outarray)
            call ssaddtolist(position,ventptr%flow_slab(j),outarray)
            call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
            call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
        end do
    end do

    ! vertical vents
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        avent = ventptr%area
        call SSaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call SSaddtolist (position,slabs,outarray)
        do j = 2, 1, -1
            vflow = ventptr%flow_slab(j)
            if (ventptr%top<=nm1.and.j==1) vflow = -vflow
            call ssaddtolist(position,ventptr%temp_slab(j),outarray)
            call ssaddtolist(position,vflow,outarray)
            call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
            call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
        end do
    end do

    !mechanical vents (note sign of flow is different here to make it relative to compartment instead of hvac system
    if (nnode/=0.and.next/=0) then
        do i = 1, next
            if (hvnode(1,i)<=nm1) then
                ventptr => mventinfo(i)
                avent = arext(i)
                call SSaddtolist (position,avent,outarray)
                ! flow slabs for the vent
                slabs = ventptr%n_slabs
                call SSaddtolist (position,slabs,outarray)
                do j = 1, 2
                    call ssaddtolist(position,ventptr%temp_slab(j),outarray)
                    call ssaddtolist(position,-ventptr%flow_slab(j),outarray)
                    call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
                    call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
                end do
            end if
        end do
    end if
    call ssprintresults (15, position, outarray)

    return
    end subroutine output_spreadsheet_smokeview
    
end module spreadsheet_routines
