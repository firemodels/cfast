!  ActivationTimes.f90 
!
!  FUNCTIONS:
!  ActivationTimes - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: ActivationTimes
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program ActivationTimes

        implicit none

    ! Variables
    integer, parameter :: ntests = 2000, nrow = 10000, ncol = 500, list_nrow = 1000, list_ncol = 100, test_cols = 3*ntests
    logical, parameter :: write_data = .false.

    character :: base_folder*128, comparelist_file*128, partial_filename*128, metric*3, test_column_names(ntests,3)*50, all_data_row_values(test_cols)*15
    character :: list_carray(list_nrow, list_ncol)*128, model_carray(nrow,ncol)*50, switch_id*3, quantity*50

    integer experimental_numr, experimental_numc, list_numr, list_numc, model_numr, model_numc, find_column, io_error, ir, ic, it, i, ntest, test_row_max, ir_test, test_length(ntests), row_number(ntests)
    integer :: switch_id_column
    integer :: d2_filename_column, d2_col_name_row_column, d2_ind_col_name_column, d2_data_row_column, d2_dep_col_name_column, d2_comp_start_column, d2_comp_end_column, d2_initial_value_column

    character :: d2_filename*128, d2_ind_col_name*128, d2_dep_col_names*128, d2_dep_col_name*128
    integer :: d2_col_name_row, d2_data_row, d2_ind_data_col, d2_dep_data_col, d2x_len, d2y_len

    real :: list_rarray(list_nrow, list_ncol), model_rarray(nrow,ncol), d2x(nrow), d2y(nrow), all_data(ntests,nrow,4), d2_initial_value, activation_time

    ! Body of ModelVandV
    base_folder = '..\..\cfast\Validation\'
    comparelist_file = trim(base_folder) // 'CFAST_activation_inputs.csv'
    open (unit=8,file=comparelist_file,form='formatted', action='read', iostat=io_error)
    call readcsv (8,list_rarray,list_carray,list_nrow,list_ncol,1,list_numr,list_numc)
    ! write (*,*) 'List read, rows and columns=',list_numr,list_numc
    close (unit=8)
    open (unit=9, file='list.txt',form='formatted')

    ! determine column locations
    switch_id_column = find_column(list_carray,list_nrow,list_ncol,1,'switch_id')
    d2_filename_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Filename')
    d2_col_name_row_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Col_Name_Row')
    d2_ind_col_name_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Ind_Col_Name')
    d2_data_row_column =  find_column(list_carray,list_nrow,list_ncol,1,'d2_Data_Row')
    d2_dep_col_name_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Dep_Col_Name')

    ir=0
    ntest = 0
    test_row_max = 0
10  ir = ir + 1
    switch_id = list_carray(ir,switch_id_column)
    if (switch_id=='d') then

        ! read in the model data   
        d2_filename = trim(base_folder) // list_carray(ir,d2_filename_column)
        write (*,*) ir,trim(d2_filename)
        open (unit=8,file=d2_filename,form='formatted',action='read',iostat=io_error)
        call readcsv(8,model_rarray,model_carray,nrow,ncol,1,model_numr, model_numc)
        close (unit=8)
        
        d2_col_name_row = list_rarray(ir,d2_col_name_row_column)
        d2_ind_col_name = list_carray(ir,d2_ind_col_name_column)
        d2_ind_data_col = find_column(model_carray,nrow,ncol,d2_col_name_row,d2_ind_col_name)
        d2_dep_col_names = list_carray(ir,d2_dep_col_name_column)
        d2_data_row = list_rarray(ir,d2_data_row_column)

        ! find dependent column(s) that match the independent columns
        do while (len_trim(d2_dep_col_names)>0)
            if(index(d2_dep_col_names,'|')>0) then
                d2_dep_col_name = d2_dep_col_names(1:(index(d2_dep_col_names,'|')-1))
                d2_dep_col_names = d2_dep_col_names((index(d2_dep_col_names,'|')+1):)
            else
                d2_dep_col_name = d2_dep_col_names
                d2_dep_col_names=''
            end if
            d2_dep_data_col = find_column(model_carray,nrow,ncol,d2_col_name_row,d2_dep_col_name)

            if (d2_ind_data_col==0.or.d2_dep_data_col==0) then
                write (*,*) 'Data column error =',d2_ind_data_col,d2_dep_data_col
                stop
            end if

            ! Get the four vectors for this comparison ... d1x and d1y are the experimental values; d2x and d2y are the model values
            call load_vector(model_rarray,model_carray,nrow,model_numr,d2_ind_data_col,d2_data_row,d2x,d2x_len)
            call load_vector(model_rarray,model_carray,nrow,model_numr,d2_dep_data_col,d2_data_row,d2y,d2y_len)

            ! Calculations of the activation times go here using values of d2x, d2y (model values)
            
            if (d2x_len==d2y_len) then
                do i = 1, d2x_len
                    if (d2y(i)/=0.0) then
                        activation_time = d2x(i)
                        exit
                    end if
                end do
            else
                write (*,*) 'Data error, x and y lengths are not equal', d2x_len, d2y_len
                stop
            end if
                
            partial_filename = d2_filename
            if (len_trim(d2_filename)>20) partial_filename = '...' // d2_filename(len_trim(d2_filename)-20:)
            write (9,'(i4,3x,a25,3x,a15,f12.3)') ir, trim(partial_filename), trim(d2_dep_col_name), activation_time
        end do
    end if

    if (ir.le.list_numr.and.list_carray(ir,1).ne.'End') go to 10
    close (unit=9)

    ! write out the array of data
    if (write_data) then
        open (unit=10,recl=30000,file='data_array.csv',form='formatted')
        write (10,'(2000(i4,'',''))') ((row_number(ir),row_number(ir),row_number(ir)),ir=1,ntest)
        write (10,'(2000(a,'',''))') ((trim(adjustL(test_column_names(ir,ic))),ic=1,3),ir=1,ntest)
        all_data_row_values = ' '
        do ir = 1, test_row_max
            do it = 1, ntest
                ic = 3*it - 2
                if (ir<=test_length(it)) then
                    write(all_data_row_values(ic),'(e15.6)') all_data(it,ir,1)
                    write(all_data_row_values(ic+1),'(e15.6)') all_data(it,ir,2)
                    write(all_data_row_values(ic+2),'(e15.6)') all_data(it,ir,3)
                else
                    all_data_row_values(ic) = ' '
                    all_data_row_values(ic+1) = ' '
                    all_data_row_values(ic+2) = ' '
                endif
            end do
        write (10,'(2000(a,'',''))') ((trim(adjustL(all_data_row_values((it-1)*3+ic))),ic=1,3),it=1,ntest)
    end do
        close (unit=10)
    end if

    end program ActivationTimes

