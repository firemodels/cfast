    module utility_routines

    use precision_parameters
    
    use exit_routines, only: cfastexit

    use cparams, only: mxss
    use room_data, only: nwpts, slab_splits, iwbound
    use setup_data, only: iofill, errormessage

    implicit none

    integer :: file_counter = 10

    ! unlike most other routines, this one does not have the private specifier since all routines here are intended to be
    ! used by other routines

    contains

    ! --------------------------- ssaddtolist -------------------------------------------

    subroutine ssaddtolist (ic, valu, array)

    real(eb), intent(in) :: valu
    real(eb), intent(out) :: array(*)
    integer, intent(inout) :: ic

    ic = ic + 1
    ! We are imposing an arbitrary limit of 32000 columns
    if (ic>mxss) return
    if (abs(valu)<=1.0e-100_eb) then
        array(ic) = 0.0_eb
    else
        array(ic) = valu
    end if

    end subroutine ssaddtolist

    ! --------------------------- toIntString -------------------------------------------

    subroutine toIntString(i,istring)

    integer, intent(in) :: i
    character(len=*), intent(out) :: istring

    character(len=256) :: string

    if (i<10) then
        write (string,'(i1)') i
    else if (i<100) then
        write (string,'(i2)') i
    else if (i<1000) then
        write (string,'(i3)') i
    else if (i<10000) then
        write (string,'(i4)') i
    else if (i<100000) then
        write (string,'(i5)') i
    else if (i<1000000) then
        write (string,'(i6)') i
    else
        string = 'error'
    end if
    istring = trim(string)

    end subroutine toIntString

    ! --------------------------- get_filenumber ---------------------------------------

    integer function get_filenumber()
      file_counter = file_counter + 1
      get_filenumber = file_counter
    end function get_filenumber

    ! --------------------------- tanhsmooth ---------------------------------------

    real(eb) function tanhsmooth (x, xmax, xmin, ymax, ymin)

    ! calculate a smooth transition from 1 (at xmax) to zero (at xmin)
    ! arguments: x    current value
    !            xmax maximum value of independent variable. Return ymax above this value
    !            xmin minimum value of independent variable. Return ymin below this value
    !            ymax value returned at or above xmax
    !            ymin value return at or below xmin

    real(eb), intent(in) :: x, xmax, xmin, ymax, ymin
    real(eb) :: f
    
    f = min(max(0.5_eb + 0.5025_eb*tanh(6.0_eb/(xmax-xmin)*(x-xmin)-3.0_eb),0.0_eb),1.0_eb)
    tanhsmooth = f*(ymax-ymin)+ymin

    end function tanhsmooth

    ! --------------------------- d1mach -------------------------------------------

    real(eb) function d1mach (i)

    ! obtain machine-dependent parameters for the local machine environment.
    ! it is a function subprogram with one (input) argument. reference  p. a. fox, a. d. hall and
    ! n. l. schryer, framework for a portable library, acm transactions on mathematical software 4,
    ! 2 (june 1978), pp. 177-188.
    !     arguments:  i
    !
    !           where i = 1,...,5.  the (output) value of a above is determined by the (input) value of i.  the
    !           results for various values of i are discussed below.
    !
    !           d1mach(1) = b**(emin-1), the smallest positive magnitude.
    !           d1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
    !           d1mach(3) = b**(-t), the smallest relative spacing.
    !           d1mach(4) = b**(1-t), the largest relative spacing.
    !           d1mach(5) = log10(b)
    !
    !           assume single precision numbers are represented in the t-digit, base-b form
    !
    !              sign (b**e)*( (x(1)/b) + ... + (x(t)/b**t) )
    !
    !           where 0 .le. x(i) .lt. b for i=1,...,t, 0 .lt. x(1), and emin .le. e .le. emax.
    !
    !           the values of b, t, emin and emax are provided in i1mach as follows:
    !           i1mach(10) = b, the base.
    !           i1mach(11) = t, the number of base-b digits.
    !           i1mach(12) = emin, the smallest exponent e.
    !           i1mach(13) = emax, the largest exponent e.

    integer, intent(in) :: i

    real(eb) :: b, x

    x = 1.0_eb
    b = radix(x)
    select case (i)
    case (1)
        d1mach = b**(minexponent(x)-1) ! the smallest positive magnitude.
    case (2)
        d1mach = huge(x)               ! the largest magnitude.
    case (3)
        d1mach = b**(-digits(x))       ! the smallest relative spacing.
    case (4)
        d1mach = b**(1-digits(x))      ! the largest relative spacing.
    case (5)
        d1mach = log10(b)
    case default
        write (errormessage,'(''***Error, Internal error, illegal call to d1mach '',i0)') i
        call cfastexit('d1mach',1) 
    end select

    end function d1mach

    ! --------------------------- cptime -------------------------------------------

    subroutine cptime (cputim)

    ! calculate amount of computer time (cputim) in seconds used so far
    ! arguments: cputim (output) - elapsed cpu time

    real(eb), intent(out) :: cputim

    call CPU_TIME(cputim)

    end subroutine cptime

    ! --------------------------- mat2mult -------------------------------------------

    subroutine mat2mult(mat1,mat2,idim,n)

    ! given an nxn matrix mat1 whose elements are either 0 or 1, this routine computes the matrix
    ! mat1**2 and returns the results in mat1 (after scaling non-zero entries to 1).
    ! arguments: mat1 - matrix
    !            mat2 - work array of same size as mat1
    !            idim - actual dimensino limit on first subscript of mat1
    !            n - size of matrix

    integer, intent(in) :: idim, n
    integer, intent(inout) :: mat1(idim,n)
    integer, intent(out) :: mat2(idim,n)

    integer :: i, j, k

    do i = 1, n
        do j = 1, n
            mat2(i,j) = 0
            do k = 1, n
                mat2(i,j) = mat2(i,j)+mat1(i,k)*mat1(k,j)
            end do
            if (mat2(i,j)>=1) mat2(i,j) = 1
        end do
    end do
    do i = 1, n
        do j = 1, n
            mat1(i,j) = mat2(i,j)
        end do
    end do

    end subroutine mat2mult

    ! --------------------------- interp -------------------------------------------

    subroutine interp (x,y,n,t,icode,yint)

    ! interpolates a table of numbers found in the arrays, x and y.
    ! arguments: x,y - arrays of size n to be interpolated at x=t
    !            icode - code to select how to extrapolate values if t is less than x(1) or greater than x(n).
    !                      if icode = 1 then yint = y(1) for t < x(1) and yint = y(n) for t > x(n).
    !                      if icode = 2 then yint is evaluated by interpolation if x(1) < t < x(n)
    !                          and by extrapolation if t < x(1) or    t > x(n)
    !            yint (output) - interpolated value of the y array at t

    real(eb), intent(in) :: x(*), y(*), t
    integer, intent(in) :: n, icode

    real(eb) :: yint

    integer :: ilast, imid, ia, iz
    real(eb) :: dydx


    save
    data ilast /1/
    if (n==1) then
        yint = y(1)
        return
    end if
    if (t<=x(1)) then
        if (icode==1) then
            yint = y(1)
            return
        else
            imid = 1
            go to 20
        end if
    end if
    if (t>=x(n)) then
        if (icode==1) then
            yint = y(n)
            return
        else
            imid = n - 1
            go to 20
        end if
    end if
    if (ilast+1<=n) then
        imid = ilast
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    end if
    if (ilast+2<=n) then
        imid = ilast + 1
        if (x(imid)<=t.and.t<=x(imid+1)) go to 20
    end if
    ia = 1
    iz = n - 1
10  continue
    imid = (ia+iz)/2
    if (t<x(imid)) then
        iz = imid - 1
        go to 10
    end if
    if (t>=x(imid+1)) then
        ia = imid + 1
        go to 10
    end if
20  continue
    dydx = (y(imid+1)-y(imid))/(x(imid+1)-x(imid))
    yint = y(imid) + dydx*(t-x(imid))
    ilast = imid

    end subroutine interp

    ! --------------------------- read_command_options -------------------------------------------

    subroutine read_command_options

    ! retrieve date

    integer :: values(8)
    character(len=10) :: big_ben(3)

    call date_and_time(big_ben(1),big_ben(2),big_ben(3),values)

    end subroutine read_command_options

    ! --------------------------- shellsort -------------------------------------------

    subroutine shellsort (ra, n)

    integer, intent(in) :: n
    real(eb), intent(inout) :: ra(n)

    integer j, i, inc
    real(eb) rra

    inc = 1
1   inc = 3*inc+1
    if (inc<=n) go to 1
2   continue
    inc = inc/3
    do i = inc+1, n
        rra = ra(i)
        j = i
3       if (ra(j-inc)>rra) then
            ra(j) = ra(j-inc)
            j = j - inc
            if (j<=inc) go to 4
            go to 3
        end if
4       ra(j) = rra
    enddo
    if (inc>1) go to 2

    end subroutine shellsort

    ! ------------------ fmix ------------------------

    real(fb) function fmix (f,a,b)

    real(fb), intent(in) :: f, a, b

    fmix = (1.0_fb-f)*a + f*b

    end function fmix

    ! ------------------ emix ------------------------

    real(eb) function emix (f,a,b)

    real(eb), intent(in) :: f, a, b

    emix = (1.0_eb-f)*a + f*b

    end function emix
    
    end module utility_routines


    module opening_fractions

    ! implement the simple open/close function for vents.
    ! This is done with a simple, linear interpolation.
    ! The opening arrays are built into the vent data structures and are of the form
    !		(1) Is start of time to change
    !		(2) Is the initial fraction (set in HVENT, VVENT and MVENT)
    !		(3) Is the time to complete the change, Time+Decay_time, and
    !		(4) Is the final fraction

    ! The open/close function is done in the physical/mode interface, wall_flow, vertical_flow and mechanical_flow
    
    use precision_parameters
    
    use cfast_types, only: target_type, vent_type

    use cparams, only: trigger_by_time, trigger_by_temp, trigger_by_flux, idx_tempf_trg
    
    use devc_data, only: targetinfo
    use vent_data, only: hventinfo, vventinfo, mventinfo
    use room_data, only: roominfo, n_rooms
    use setup_data, only: iofilo, iofill
    use namelist_data

    implicit none

    private

    public get_vent_opening

    contains

    ! --------------------------- get_vent_opening-------------------------------------

    subroutine get_vent_opening (ventptr,time,fraction)

    type(vent_type), pointer, intent(in) :: ventptr
    real(eb), intent(in) :: time
    real(eb), intent(out) :: fraction

    integer :: i
    real(eb), parameter :: mintime = 1.0e-6_eb
    real(eb) :: dt, dtfull, dy, dydt
    character(len=128) room1c, room2c, vtypec
    
    type(target_type), pointer :: targptr

    ! check vent triggering by time
    if (ventptr%opening_type==trigger_by_time) then
        fraction = 1.0_eb
        if (ventptr%npoints>0) then
            if (time<=ventptr%t(1)) then
                fraction = ventptr%f(1)
                return
            else if (time>ventptr%t(ventptr%npoints)) then
                fraction = ventptr%f(ventptr%npoints)
                return
            else
                do i=2,ventptr%npoints
                    if (time>ventptr%t(i-1).and.time<=ventptr%t(i)) then
                        dt = max(ventptr%t(i)-ventptr%t(i-1),mintime)
                        dtfull = max(time-ventptr%t(i-1),mintime)
                        dy = ventptr%f(i)-ventptr%f(i-1)
                        dydt = dy / dt
                        fraction = ventptr%f(i-1) + dydt*dtfull
                        return
                    end if
                end do
            end if
        end if
        ! check vent triggering by temperature. if tripped, turn it into a time-based change
    else if (ventptr%opening_type==trigger_by_temp.and..not.ventptr%opening_triggered) then
        targptr => targetinfo(ventptr%opening_target)
        fraction = ventptr%f(1)
        if (targptr%temperature(idx_tempf_trg)>ventptr%opening_criterion) then
            ventptr%t(1) = time
            ventptr%t(2) = time + 1.0_eb
            ventptr%opening_type = trigger_by_time
            ventptr%opening_triggered = .true.
            room1c = roominfo(ventptr%room1)%id
            if (ventptr%room1>n_rooms) room1c = 'Outside'
            room2c = roominfo(ventptr%room2)%id
            if (ventptr%room2>n_rooms) room2c = 'Outside'
            vtypec = 'Unknown '
            if (ventptr%vtype=='H') vtypec = 'Wall'
            if (ventptr%vtype=='V') vtypec = 'Ceiling/Floor'
            if (ventptr%vtype=='M') vtypec = 'Mechanical'
            write (iofilo,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by temperature at ',time,' s'
            write (iofill,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by temperature at ',time,' s'
        end if
        ! check vent triggering by flux. if tripped, turn it into a time-based change
    else if (ventptr%opening_type==trigger_by_flux.and..not.ventptr%opening_triggered) then
        targptr => targetinfo(ventptr%opening_target)
        fraction = ventptr%f(1)
        if (targptr%flux_incident_front>ventptr%opening_criterion) then
            ventptr%t(1) = time
            ventptr%t(2) = time + 1.0_eb
            ventptr%opening_type = trigger_by_time
            ventptr%opening_triggered = .true.
            room1c = roominfo(ventptr%room1)%id
            if (ventptr%room1>n_rooms) room1c = 'Outside'
            room2c = roominfo(ventptr%room2)%id
            if (ventptr%room2>n_rooms) room2c = 'Outside'
            vtypec = 'Unknown '
            if (ventptr%vtype=='H') vtypec = 'Wall'
            if (ventptr%vtype=='V') vtypec = 'Ceiling/Floor'
            if (ventptr%vtype=='M') vtypec = 'Mechanical'
            write (iofilo,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by heat flux at ',time,' s'
            write (iofill,'(a,2(a,i0),3a,i0,3a,f0.0,a)') trim(vtypec),' vent #',ventptr%counter,' from compartment ', &
                ventptr%room1,' (',trim(room1c),') to compartment ',ventptr%room2,' (',trim(room2c), &
                '), opening change triggered by heat flux at ',time,' s'
        end if
    else
    end if

    end subroutine get_vent_opening

    end module opening_fractions
