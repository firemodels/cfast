    subroutine find_column_name(col_names, col_name)

    implicit none

    character :: col_names*128, col_name*128

    if(index(col_names,'|')>0) then
        col_name = col_names(1:(index(col_names,'|')-1))
        col_names = col_names((index(col_names,'|')+1):)
    else
        col_name = col_names
        col_names=''
    end if
    return

    end subroutine find_column_name

    integer function find_column(carray,num_rows,num_columns,search_row, desired_id)

    implicit none

    integer num_rows, num_columns, search_row, i
    character :: carray(num_rows,num_columns)*(*), desired_id*(*)

    do i = 1, num_columns
        if (adjustl(carray(search_row,i))==adjustl(desired_id)) then
            find_column = i
            return
        end if
    end do
    find_column = 0
    return

    end function find_column
    
    subroutine copy_vector(x, xs, x_len, xs_len, nrow, ic)
    implicit none
    
    integer :: nrow, ic, ir, x_len, xs_len(*)
    real :: x(nrow), xs(nrow,*)
    
    do ir = 1, x_len
        xs(ir,ic) = x(ir)
    end do
    xs_len(ic) = x_len
    return
    
    end subroutine copy_vector

    subroutine load_vector (rarray, carray, array_row_dimension, array_row_actual, array_column, array_start_row, &
        vector, vector_length)
    implicit none

    integer :: array_row_dimension, array_row_actual, array_column, array_start_row, vector_length, ir, ic

    real :: rarray (array_row_dimension, *), vector(*)
    character :: carray (array_row_dimension, *)*50

    do ir = 1, array_row_dimension
        vector(ir) = 0.0
    end do

    vector_length = 0
    do ir = array_row_actual, 1, -1
        if (carray(ir,array_column).ne.' '.and.carray(ir,array_column).ne.'NaN') then
            vector_length = max(vector_length, ir)
        end if
    end do
    vector_length = vector_length - array_start_row + 1
    do ir = 1, vector_length
        vector(ir) = rarray(ir+array_start_row-1,array_column)
    end do

    end subroutine load_vector
    
    subroutine find_ranges(x, x_len, x_comp_start, x_comp_end, y, y_len)
    
    implicit none
    
    integer, intent(inout) :: x_len, y_len
    real, intent(in) :: x_comp_start, x_comp_end
    real, intent(inout) :: x(*), y(*)
    integer i, istart, iend
    
    do istart = 1, x_len
        if (x(istart)>=x_comp_start) exit
    end do
    
    do iend = x_len, 1, -1
        if (x(istart)<=x_comp_end) exit
    end do
    
    if (istart/=1.or.iend/=x_len) then
        continue
    end if
    
    do i = istart, iend
        x(i-istart+1) = x(i)
        y(i-istart+1) = y(i)
    end do
    x_len = iend-istart+1
    y_len = x_len
    
    end subroutine find_ranges
    
    subroutine store_vector(vector, vector_length, array_test, array_column, array, array_test_dimension, array_row_dimension)

    implicit none

    integer, intent(in) :: vector_length, array_test, array_column, array_test_dimension, array_row_dimension
    real, intent(in) :: vector(*)
    real, intent(out) :: array(array_test_dimension,array_row_dimension,*)

    integer :: i

    do i = 1, array_row_dimension
        if (i<=vector_length) then
            array(array_test,i,array_column) = vector(i)
        else
            array(array_test,i,array_column) = 0.0
        end if
    end do

    end subroutine store_vector

    real function interpolate (x_value, x, y, x_length)

    implicit none

    integer x_length, ir
    real x_value, x(*), y_value, y(*)

    do ir = 1, x_length

        if (x(ir).le.x_value .and. x(ir+1).ge.x_value) then
            y_value = y(ir) + (x_value - x(ir)) * (y(ir+1) - y(ir))/(x(ir+1) - x(ir))
            interpolate = y_value
            return
        end if
    end do
    interpolate = y(x_length)
    end function interpolate
    
    
    subroutine SSaddtolist (ic, itype, valu, cvalu, array)

    character(30) :: array(*)
    character(*) :: cvalu
    real :: valu
    integer :: ic, itype, iounit

    ic = ic + 1
    ! We are imposing an arbitrary limit of 32000 columns
    if (ic>32000) return
    select case (itype)
    case (1)
        if (abs(valu)<=1.0d-100) then
            array(ic) = "0.0"
        else
            write (array(ic),'(e13.6)') valu
        end if
    case (2)
        array(ic) = cvalu
    end select
    return
    
    end subroutine SSaddtolist

    subroutine SSprintresults (iounit,ic,array)
    character(30) :: array(*)
    integer :: ic, iounit


    write (iounit,"(1024(a,','))" ) (trim(array(i)),i=1,ic)
    return

    end subroutine SSprintresults

