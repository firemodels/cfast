   integer function find_column(carray,num_rows,num_columns,search_row, desired_id)

    implicit none

    character :: carray(num_rows,num_columns)*(*), desired_id*(*)
    integer num_rows, num_columns, search_row, i

    do i = 1, num_columns
        if (adjustl(carray(search_row,i))==adjustl(desired_id)) then
            find_column = i
            return
        end if
    end do
    find_column = 0
    return

    end function find_column

    subroutine load_vector (rarray, carray, array_row_dimension, array_row_actual, array_column, array_start_row, vector, vector_length)
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
