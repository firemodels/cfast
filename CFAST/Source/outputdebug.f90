module debug_routines
    
    use precision_parameters
    
    use cparams
    use cfast_types
    use cfast_main
    use spreadsheet_header_routines
    use utility_routines
    
    implicit none
    
    private
    
    public ssprintslab, output_spreadsheet_residuals, spreadsheetfslabs !, ssprintresid, 
    
    contains

! --------------------------- SpreadSheetFSlabs -------------------------------------------

    subroutine spreadsheetfslabs (time, ir1, ir2, iv, nslab, qslab, outarray, position)
    
    use precision_parameters
    use cparams
    use debug
    use vents
    use vent_slab
    implicit none
    
    real(eb), intent(in) :: time, qslab(mxfslab)
    real(eb), intent(inout) :: outarray(*)
    integer, intent(in) :: ir1, ir2, iv, nslab
    integer, intent(inout) :: position
    
    real(eb) :: r1, r2, v, slab
    
    integer :: i
    logical :: firstc=.true.
    
    if (firstc) then 
        call SSHeadersFSlabs
        firstc = .false.
    end if
    
    if (nwline) then 
        position = 0
        call SSaddtolist(position, time, outarray)
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
        call SSaddtolist(position, dirs12(i)*qslab(i), outarray)
    end do
    return
    
    end subroutine spreadsheetfslabs

! --------------------------- ssprintslab -------------------------------------------
    
    subroutine ssprintslab (position, outarray)

    use precision_parameters
    use debug

    real(eb), intent(in) :: outarray(*)
    integer, intent(in) :: position

    call ssprintresid (ioslab, position, outarray)
    nwline = .true.

    return

    end subroutine ssprintslab

! --------------------------- ssprintresid -------------------------------------------
    
    subroutine ssprintresid (iounit,ic,array)
    
    use precision_parameters
    implicit none
    
    real(eb), intent(in) :: array(*)
    integer, intent(in) :: iounit, ic
    
    integer i
  
    write (iounit,"(16384(e20.13,','))" ) (array(i),i=1,ic)
    return
    
    end subroutine ssprintresid

! --------------------------- output_spreadsheet_residuals -------------------------------------------

    subroutine output_spreadsheet_residuals (time, flwtot, flwnvnt, flwf, flwhvnt, flwmv, filtered, flwdjf, flwcv, flwrad)
    
    use debug
    use cenviro
    use cfast_main
    use objects1
    implicit none
    

    real(eb), intent(in) :: time
    ! data structure for total flows and fluxes
    real(eb), intent(in) :: flwtot(nr,mxfprd+2,2)

    ! data structures for flow through vents
    real(eb), intent(in) :: flwnvnt(nr,mxfprd+2,2)
    real(eb), intent(in) :: flwhvnt(nr,ns+2,2)

    ! data structures for fires
    real(eb), intent(in) :: flwf(nr,ns+2,2)

    ! data structures for convection and radiation
    real(eb), intent(in) :: flwcv(nr,2)
    real(eb), intent(in) :: flwrad(nr,2)

    ! data structures for mechanical vents
    real(eb), intent(in) :: flwmv(nr,ns+2,2), filtered(nr,ns+2,2)

    ! data structures for door jet fires
    real(eb), intent(in) :: flwdjf(nr,ns+2,2)
    
    integer, parameter :: maxhead = 1+2*(7*(ns+2)+3)*nr + 4*nr
    real(eb) :: outarray(maxhead)
    logical :: firstc
    integer :: position, i, j, k
    data firstc/.true./
    save firstc
    
    ! headers
    if (firstc) then
        call ssHeadersResid
        firstc = .false.
    end if

    position = 0
    call SSaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, nm1
        call SSaddtolist (position,zzrelp(i),outarray)
        call SSaddtolist (position,zzvol(i,upper),outarray)
        call SSaddtolist(position,zztemp(i,upper),outarray)
        call SSaddtolist(position,zztemp(i,lower),outarray)
        do j = 1, 2
            do k = 1, 2
                call SSaddtolist (position,flwtot(i,k,j),outarray)
                call SSaddtolist (position,flwnvnt(i,k,j),outarray)
                call SSaddtolist (position,flwf(i,k,j),outarray)
                call SSaddtolist (position,flwhvnt(i,k,j),outarray)
                call SSaddtolist (position,flwmv(i,k,j),outarray)
                call SSaddtolist (position,filtered(i,k,j),outarray)
                call SSaddtolist (position,flwdjf(i,k,j),outarray)
            end do
            call SSaddtolist (position,flwcv(i,j),outarray)
            call SSaddtolist (position,flwrad(i,j),outarray)
        end do
    end do
    ! species mass flow    
    do i = 1, nm1
        do j = 1, 2
            do k = 1, 9
                !call SSaddtolist (position,flwtot(i,k,j),outarray)
                !call SSaddtolist (position,flwnvnt(i,k,j),outarray)
                call SSaddtolist (position,flwf(i,k+2,j),outarray)
                !call SSaddtolist (position,flwhvnt(i,k,j),outarray)
                !call SSaddtolist (position,flwmv(i,k,j),outarray)
                !call SSaddtolist (position,filtered(i,k,j),outarray)
                !call SSaddtolist (position,flwdjf(i,k,j),outarray)
            end do
            !call SSaddtolist (position,flwcv(i,j),outarray)
            !call SSaddtolist (position,flwrad(i,j),outarray)
        end do
    end do

    call ssprintresid (ioresid, position, outarray)

    return
    end subroutine output_spreadsheet_residuals 
    
end module debug_routines