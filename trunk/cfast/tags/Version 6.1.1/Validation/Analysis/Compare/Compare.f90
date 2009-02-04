program Compare
    implicit none

    ! Variables
    integer, parameter :: nrow = 10000, ncol = 1000, list_nrow = 1000, list_ncol = 25
    integer, parameter :: icKeyword = 1, icFilename = 2, icFirstRow = 3, icQuantity = 3, icExptime = 4, icExpvalue = 5, &
                          icModtime = 6, icModvalue = 7, icTimebegin = 8, icTimeend = 9, &
                          icMaxMin = 10, icCLosed = 11, icExpZero = 13, icModZero = 16

    integer, parameter :: icExpPeak = 14, icExpPeakTime = 15, icModPeak = 17, icModPeakTime = 18, icDeltaE = 19, icDeltaM = 20, icRelDiff = 21

    integer exp_numr, exp_numc, mod_numr, mod_numc, list_numr, list_numc
    integer ir, ic, exp_ifirstrow, mod_ifirstrow
    integer x_exp_column, x_exp_length, y_exp_column, y_exp_length, exp_length
    integer x_mod_column, x_mod_length, y_mod_column, y_mod_length, mod_length
    character comparelist_file*128, mod_file*128, exp_file*128, print_file*128
    
    character list_carray(list_nrow, list_ncol)*50, exp_carray(nrow,ncol)*50, mod_carray(nrow,ncol)*50
    real list_rarray(list_nrow, list_ncol), exp_rarray(nrow, ncol), mod_rarray(nrow,ncol)
    real x_exp(nrow), y_exp(nrow), x_mod(nrow), y_mod(nrow), x_exp_value, y_exp_value, x_mod_value, y_mod_value
    real time, end_time
    real exp_zero, min_exp_value, min_exp_time, max_exp_value, max_exp_time
    real mod_zero, min_mod_value, min_mod_time, max_mod_value, max_mod_time
    real interpolate
    real max_relative_difference, min_relative_difference, print_difference
    
    ! Body of NRC Compare

    comparelist_file = '..\CompareList.csv'
    open (unit=8,file=comparelist_file,form='formatted')
	call readcsv (8,list_rarray,list_carray,list_nrow,list_ncol,1,list_numr,list_numc)
	! write (*,*) 'List read, rows and columns=',list_numr,list_numc
	close (unit=8)
	open (unit=9, file='..\list.txt',form='formatted')

    ir=0
10  ir = ir + 1

    ! each set of comparisons begins with file name specifications
    if (list_carray(ir,icKeyword).eq.'Experiment') then
	    exp_file = '..\' // trim(list_carray(ir,2)) // '.csv'
	    print_file = exp_file
	    exp_ifirstrow = list_rarray(ir,icFirstRow)
	    open (unit=8,file=exp_file,form='formatted')
	    call readcsv (8,exp_rarray,exp_carray,nrow,ncol,exp_ifirstrow,exp_numr,exp_numc)
	    write (*,'(a,a,a,2i5)') 'Experiment read, ',trim(exp_file),', rows and columns=',exp_numr,exp_numc
	    close (unit=8)
    end if
    if (list_carray(ir,icKeyword).eq.'Model') then
	    mod_file = '..\' // trim(list_carray(ir,2)) // '.csv'
	    mod_ifirstrow = list_rarray(ir,icFirstRow)
	    open (unit=8,file=mod_file,form='formatted')
	    call readcsv (8,mod_rarray,mod_carray,nrow,ncol,mod_ifirstrow,mod_numr,mod_numc)
	    write (*,'(a,a,a,2i5)') 'Model read, ',trim(mod_file),', rows and columns=',mod_numr,mod_numc
	    close (unit=8)
    end if
    if (list_carray(ir,icKeyword).eq.'Compare') then
        
        ! get data into individual x and y vectors
        x_exp_column = int(list_rarray(ir,icExptime))
        call load_vector (exp_rarray, exp_carray, nrow, exp_numr, x_exp_column, x_exp, x_exp_length)
        y_exp_column = int(list_rarray(ir,icExpvalue))
        call load_vector (exp_rarray, exp_carray, nrow, exp_numr, y_exp_column, y_exp, y_exp_length)
        x_mod_column = int(list_rarray(ir,icModtime))
        call load_vector (mod_rarray, mod_carray, nrow, mod_numr, x_mod_column, x_mod, x_mod_length)
        y_mod_column = int(list_rarray(ir,icModvalue))
        call load_vector (mod_rarray, mod_carray, nrow, mod_numr, y_mod_column, y_mod, y_mod_length)
            
        if (x_exp_length.eq.0 .or. y_exp_length.eq.0) then
            write (*,*) 'No data points in file ',trim(exp_file),' for values of ', trim(list_carray(ir,icFilename))
           stop
        end if
        
        if (x_mod_length.eq.0 .or. y_mod_length.eq.0) then
            write (*,*) 'No data points in file ',trim(mod_file),' for values of ', trim(list_carray(ir,icFilename))
           stop
        end if
            
        ! calculate maximum and minimum over specified time range. Calculate every 10 s over the range
        time = list_rarray(ir,icTimebegin)
        exp_length = min(x_exp_length, y_exp_length)
        mod_length = min(x_mod_length, y_mod_length)
        end_time = min(list_rarray(ir,icTimeend),x_exp(exp_length),x_mod(mod_length))
        if (list_carray(ir,icExpzero).ne.' ') then
            exp_zero = list_rarray(ir,icExpzero)        
        else
            exp_zero = interpolate(0.0, x_exp, y_exp, x_exp_length)
        end if    
        if (list_carray(ir,icModzero).ne.' ') then
            mod_zero = list_rarray(ir,icModzero)
        else
            mod_zero = interpolate(0.0, x_mod, y_mod, x_mod_length)
        end if    

        min_exp_value =  1.e+20
        max_exp_value = -1.e-20
        min_mod_value =  1.e+20
        max_mod_value = -1.e-20
        do while (time .le. end_time)
           
            x_exp_value = time
            y_exp_value = interpolate(x_exp_value, x_exp, y_exp, exp_length)
            if (y_exp_value.lt.min_exp_value) then
                min_exp_value = y_exp_value
                min_exp_time = x_exp_value
            end if
            if (y_exp_value.gt.max_exp_value) then
                max_exp_value = y_exp_value
                max_exp_time = x_exp_value
            end if
            x_mod_value = time
            y_mod_value = interpolate(x_mod_value, x_mod, y_mod, mod_length)
            if (y_mod_value.lt.min_mod_value) then
                min_mod_value = y_mod_value
                min_mod_time = x_mod_value
            end if
            if (y_mod_value.gt.max_mod_value) then
                max_mod_value = y_mod_value
                max_mod_time = x_mod_value
            end if
            time = time + 10./60.
        end do
        max_relative_difference = ((max_mod_value - mod_zero) - (max_exp_value - exp_zero)) / (max_exp_value - exp_zero)
        min_relative_difference = ((min_mod_value - mod_zero) - (min_exp_value - exp_zero)) / (min_exp_value - exp_zero)
        
        if (list_carray(ir,icMaxmin).eq.' ') then
            write (*,'(1X,2A25)') trim(print_file),trim(list_carray(ir,icFilename))
            print_file = ' '
            list_carray(ir,icReldiff) = ' '
        else if (list_carray(ir,icMaxmin).eq.'Max') then
            write (*,'(1X,2A25,4f8.3,5x,f8.2)') trim(print_file),trim(list_carray(ir,icFilename)), &
                exp_zero, max_exp_value, mod_zero, max_mod_value, max_relative_difference*100.
            print_file = ' '
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icExpzero, exp_zero)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icExppeak, max_exp_value)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icExpPeaktime, max_exp_time)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icModzero, mod_zero)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icModpeak, max_mod_value)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icModPeaktime, max_mod_time)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icDeltaE, max_exp_value-exp_zero)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icDeltaM, max_mod_value-mod_zero)
            if (list_carray(ir,icClosed).eq.' ') call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icReldiff, max_relative_difference*100.)
        else if (list_carray(ir,icMaxmin).eq.'Min') then
            write (*,'(1X,2A25,4f8.3,5x,f8.2)') trim(print_file),trim(list_carray(ir,icFilename)), &
                exp_zero, min_exp_value, mod_zero, min_mod_value, min_relative_difference*100.
            print_file = ' '
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icExpzero, exp_zero)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icExppeak, min_exp_value)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icExpPeaktime, min_exp_time)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icModzero, mod_zero)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icModpeak, min_mod_value)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icModPeaktime, min_mod_time)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icDeltaE, min_exp_value-exp_zero)
            call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icDeltaM, min_mod_value-mod_zero)
            if (list_carray(ir,icClosed).eq.' ') call store_value(list_rarray, list_carray, list_nrow, list_ncol, ir, icReldiff, min_relative_difference*100.)
        end if
    end if
    write (9,'(30a25)') (list_carray(ir,ic), ic=1, list_numc)
    if (ir.le.list_numr.and.list_carray(ir,1).ne.'End') go to 10

end program Compare

subroutine store_value (rarray, carray, array_row_dimension, array_column_dimension, array_row, array_column, value)
    implicit none

    integer array_row_dimension, array_column_dimension, array_row, array_column 
    
    real rarray (array_row_dimension, array_column_dimension), value
    character carray (array_row_dimension, array_column_dimension)*50

    rarray(array_row, array_column) = value
    write (carray(array_row, array_column),'(e12.5)') value
end subroutine store_value

subroutine load_vector (rarray, carray, array_row_dimension, array_row_actual, array_column, vector, vector_length)
    implicit none

    integer array_row_dimension, array_row_actual, array_column, vector_length, ir, ic
    
    real rarray (array_row_dimension, *), vector(*)
    character carray (array_row_dimension, *)*50
    
        do ir = 1, array_row_dimension
            vector(ir) = 0.0
        end do

        vector_length = 0
        do ir = array_row_actual, 1, -1
            if (carray(ir,array_column).ne.' ') then
                vector_length = max(vector_length, ir)
            end if
        end do
        do ir = 1, vector_length
            vector(ir) = rarray(ir,array_column)
        end do

end subroutine load_vector

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

