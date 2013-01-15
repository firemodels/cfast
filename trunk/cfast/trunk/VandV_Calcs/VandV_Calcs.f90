    program VandV_Calcs

    implicit none

    ! Variables
    integer, parameter :: ntests = 2000, nrow = 10000, ncol = 500, list_nrow = 1000, list_ncol = 100

    character :: base_folder*128, comparelist_file*128, partial_filename*128
    character :: list_carray(list_nrow, list_ncol)*128, model_carray(nrow,ncol)*50, switch_id*3

    integer list_numr, list_numc, model_numr, model_numc, find_column, io_error, ir, ic, it, i, ntest
    integer :: switch_id_column
    integer :: d2_filename_column, d2_col_name_row_column, d2_ind_col_name_column, d2_data_row_column, d2_data_col_name_column, d2_data_column_count_column

    character :: d2_filename*128, d2_ind_col_name*128, d2_data_col_names(list_ncol)*128, d2_data_col_name
    integer :: d2_col_name_row, d2_data_row, d2_ind_data_col, d2_data_data_col(list_ncol), d2_data_column_count, d2x_len, d2y_len(list_ncol)

    real :: list_rarray(list_nrow, list_ncol), model_rarray(nrow,ncol), d2x(nrow), d2y(nrow,list_ncol)

    ! Calculation specific variables
    real :: activation_time

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
    d2_data_column_count_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Data_Column_Count')
    d2_data_col_name_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Dep_Col_Name')

    ir=0
    ntest = 0
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
    d2_data_col_names = list_carray(ir,d2_data_col_name_column)
    d2_data_row = list_rarray(ir,d2_data_row_column)
    d2_data_column_count = list_rarray(ir,d2_data_column_count_column)

    ! find dependent column(s) that match the independent columns

    ! Get the vectors for this calculation ...d2x and d2y(i)
    call load_vector(model_rarray,model_carray,nrow,model_numr,d2_ind_data_col,d2_data_row,d2x,d2x_len)

    do while (len_trim(d2_data_col_names)>0)
        do ic = 1,d2_data_column_count
            call find_column_name(d2_data_col_names(ic),d2_data_column_name)
            d2_data_data_col(ic) = find_column(model_carray,nrow,ncol,d2_col_name_row,d2_data_col_name) 
            call load_vector(model_rarray,model_carray,nrow,model_numr,d2_data_data_col(ic),d2_data_row,d2y(ic),d2y_len(ic))
        end do

        if (do_activation_time) then
            ! Calculations of the activation times go here using values of d2x, d2y (model values)
            if (d2x_len==d2y_len(1)) then
                do i = 1, d2x_len
                    if (d2y(i,1)/=0.0) then
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
            write (9,'(i4,3x,a25,3x,a15,f12.3)') ir, trim(partial_filename), trim(d2_data_col_name), activation_time
        end if
    end do

    if (ir.le.list_numr.and.list_carray(ir,1).ne.'End') go to 10
    close (unit=9)

    end program VandV_Calcs

