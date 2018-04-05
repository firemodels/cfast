module debug_routines

    use precision_parameters

    use spreadsheet_header_routines, only: ssHeaders_fslabs, ssHeaders_resid
    use utility_routines, only: ssaddtolist

    use cenviro
    use cfast_types
    use cparams
    use diag_data
    use room_data
    use vent_data

    implicit none

    private

    public ssprintslab, output_spreadsheet_residuals, spreadsheetfslabs !, ssprintresid,

    contains

! --------------------------- SpreadSheetFSlabs -------------------------------------------

    subroutine spreadsheetfslabs (time, ir1, ir2, iv, nslab, qslab, outarray, position)

    real(eb), intent(in) :: time, qslab(mxfslab)
    real(eb), intent(inout) :: outarray(*)
    integer, intent(in) :: ir1, ir2, iv, nslab
    integer, intent(inout) :: position

    real(eb) :: r1, r2, v, slab

    integer :: i
    logical :: firstc=.true.

    if (firstc) then
        call ssHeaders_fslabs
        firstc = .false.
    end if

    if (nwline) then
        position = 0
        call ssaddtolist(position, time, outarray)
        nwline = .false.
    end if

    r1 = ir1
    r2 = ir2
    v = iv
    slab = nslab
    call ssaddtolist(position, r1, outarray)
    call ssaddtolist(position, r2, outarray)
    call ssaddtolist(position, v, outarray)
    call ssaddtolist(position, slab, outarray)
    do i = 1, mxfslab
        call ssaddtolist(position, dirs12(i)*qslab(i), outarray)
    end do
    return

    end subroutine spreadsheetfslabs

! --------------------------- ssprintslab -------------------------------------------

    subroutine ssprintslab (position, outarray)

    real(eb), intent(in) :: outarray(*)
    integer, intent(in) :: position

    call ssprintresid (ioslab, position, outarray)
    nwline = .true.

    return

    end subroutine ssprintslab

! --------------------------- ssprintresid -------------------------------------------

    subroutine ssprintresid (iounit,ic,array)

    real(eb), intent(in) :: array(*)
    integer, intent(in) :: iounit, ic

    integer i

    write (iounit,"(16384(e20.13,','))" ) (array(i),i=1,ic)
    return

    end subroutine ssprintresid

! --------------------------- output_spreadsheet_residuals -------------------------------------------

    subroutine output_spreadsheet_residuals (time, flows_total, flows_hvents, flows_fires, flows_vvents, flows_mvents, &
        filtered, flows_doorjets, flows_convection, flows_radiation)

    real(eb), intent(in) :: time
    ! data structure for total flows and fluxes
    real(eb), intent(in) :: flows_total(mxrooms,ns+2,2)

    ! data structures for flow through vents
    real(eb), intent(in) :: flows_hvents(mxrooms,ns+2,2)
    real(eb), intent(in) :: flows_vvents(mxrooms,ns+2,2)

    ! data structures for fires
    real(eb), intent(in) :: flows_fires(mxrooms,ns+2,2)

    ! data structures for convection and radiation
    real(eb), intent(in) :: flows_convection(mxrooms,2)
    real(eb), intent(in) :: flows_radiation(mxrooms,2)

    ! data structures for mechanical vents
    real(eb), intent(in) :: flows_mvents(mxrooms,ns+2,2), filtered(mxrooms,ns+2,2)

    ! data structures for door jet fires
    real(eb), intent(in) :: flows_doorjets(mxrooms,ns+2,2)

    integer, parameter :: maxhead = 1+2*(7*(ns+2)+3)*mxrooms + 4*mxrooms
    real(eb) :: outarray(maxhead)
    logical :: firstc
    integer :: position, i, j, k
    data firstc/.true./
    save firstc
    type(room_type), pointer :: roomptr

    ! headers
    if (firstc) then
        call ssHeaders_resid
        firstc = .false.
    end if

    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nrm1
        roomptr => roominfo(i)
        call ssaddtolist (position,roomptr%relp,outarray)
        call ssaddtolist (position,roomptr%volume(u),outarray)
        call ssaddtolist(position,roomptr%temp(u),outarray)
        call ssaddtolist(position,roomptr%temp(l),outarray)
        do j = 1, 2
            do k = 1, 2
                call ssaddtolist (position,flows_total(i,k,j),outarray)
                call ssaddtolist (position,flows_hvents(i,k,j),outarray)
                call ssaddtolist (position,flows_fires(i,k,j),outarray)
                call ssaddtolist (position,flows_vvents(i,k,j),outarray)
                call ssaddtolist (position,flows_mvents(i,k,j),outarray)
                call ssaddtolist (position,filtered(i,k,j),outarray)
                call ssaddtolist (position,flows_doorjets(i,k,j),outarray)
            end do
            call ssaddtolist (position,flows_convection(i,j),outarray)
            call ssaddtolist (position,flows_radiation(i,j),outarray)
        end do
    end do
    ! species mass flow
    do i = 1, nrm1
        do j = 1, 2
            do k = 1, 9
                !call ssaddtolist (position,flows_total(i,k+2,j),outarray)
                !call ssaddtolist (position,flows_hvents(i,k+2,j),outarray)
                call ssaddtolist (position,flows_fires(i,k+2,j),outarray)
                !call ssaddtolist (position,flows_vvents(i,k+2,j),outarray)
                !call ssaddtolist (position,flows_mvents(i,k+2,j),outarray)
                !call ssaddtolist (position,filtered(i,k+2,j),outarray)
                !call ssaddtolist (position,flows_doorjets(i,k+2,j),outarray)
            end do
            !call ssaddtolist (position,flows_convection(i,j),outarray)
            !call ssaddtolist (position,flows_radiation(i,j),outarray)
        end do
    end do

    call ssprintresid (ioresid, position, outarray)

    return
    end subroutine output_spreadsheet_residuals

end module debug_routines