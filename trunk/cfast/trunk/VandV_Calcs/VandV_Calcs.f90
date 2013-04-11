    program VandV_Calcs

    implicit none

    ! Variables

    ! These parameters decide which calculation to do ... only one should be true at a time, I think
    logical, parameter :: do_activation_time = .false.      ! true if calculating activation times from data
    logical, parameter :: do_temperature_profile = .true.   ! true if calculating temperature profiles in a compartment


    integer, parameter :: ntests = 2000, nrow = 10000, ncol = 500, list_nrow = 1000, list_ncol = 100

    character :: base_folder*128, comparelist_file*128, partial_filename*128, filename*128
    character :: list_carray(list_nrow, list_ncol)*128, model_carray(nrow,ncol)*50, switch_id*3

    integer list_numr, list_numc, model_numr, model_numc, find_column, io_error, ir, ic, it, i, ntest, istat
    integer :: switch_id_column
    integer :: d2_filename_column, d2_col_name_row_column, d2_ind_col_name_column, d2_data_row_column, d2_data_col_name_column, d2_data_column_count_column

    character :: d2_filename*128, d2_ind_col_name*128, d2_data_col_names(list_ncol)*128, d2_data_col_name*128
    integer :: d2_col_name_row, d2_data_row, d2_ind_data_col, d2_data_data_col(list_ncol), d2_data_column_count, d2x_len, d2y_len, d2ys_len(list_ncol)

    real :: list_rarray(list_nrow, list_ncol), model_rarray(nrow,ncol), d2x(nrow), d2y(nrow), d2ys(nrow,list_ncol)

    ! Calculation specific variables
    real :: activation_time
    real :: temperature_profile_data(ntests,3)
    character :: temperature_profile_name(ntests)*30

    ! Body of ModelVandV
    base_folder = '..\..\cfast\Validation\'
    call getarg (1,filename,istat)
    if (istat.le.0) then
        stop 'No data file specified'
    end if
    comparelist_file = trim(base_folder) // trim(adjustl(filename))
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
    d2_data_col_name_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Data_Col_Name')

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
        d2_data_row = list_rarray(ir,d2_data_row_column)
        d2_data_column_count = list_rarray(ir,d2_data_column_count_column)
        do ic = 1, d2_data_column_count
            d2_data_col_names(ic) = list_carray(ir,d2_data_col_name_column+ic-1)
        end do

        ! Get the vectors for this calculation ...d2x and d2y(i)
        call load_vector(model_rarray,model_carray,nrow,model_numr,d2_ind_data_col,d2_data_row,d2x,d2x_len)

        do while (len_trim(d2_data_col_names(1))>0)
            do ic = 1,d2_data_column_count

                call find_column_name(d2_data_col_names(ic),d2_data_col_name)
                d2_data_data_col(ic) = find_column(model_carray,nrow,ncol,d2_col_name_row,d2_data_col_name) 
                call load_vector(model_rarray,model_carray,nrow,model_numr,d2_data_data_col(ic),d2_data_row,d2y,d2y_len)
                call copy_vector(d2y,d2ys,d2y_len,d2ys_len,nrow,ic)
            end do
            
            ! Here's where the various calculations should go
            
            ! Calculations detector / sprinkler activation times
            if (do_activation_time) then
                ! Calculations of the activation times
                if (d2x_len==d2ys_len(1)) then
                    do i = 1, d2x_len
                        if (d2ys(i,1)/=0.0) then
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
                
            !Calculations of layer temperature profiles
            else if (do_temperature_profile) then
                if (d2x_len==d2ys_len(1).and.d2x_len==d2ys_len(2).and.d2x_len==d2ys_len(3)) then
                    ntest = ntest + 1
                    temperature_profile_data(ntest,1) = d2ys(d2ys_len(1),1)
                    temperature_profile_data(ntest,2) = d2ys(d2ys_len(2),2)
                    temperature_profile_data(ntest,3) = d2ys(d2ys_len(3),3)
                    temperature_profile_name(ntest) = 'Test_' // trim(d2_filename(len_trim(d2_filename)-8:len_trim(d2_filename)-6)) ! This works for a 3 digit filename numbering (as the Steckler Compartment tests are done)
                else
                    write (*,*) 'Data error, x and y lengths are not equal', d2x_len, d2y_len
                    stop
                end if
            
            ! Calculations to correct pressure to height matching measured pressure
            else if (do_pressure_correction) then
                
            end if
        end do
    end if

    if (ir.le.list_numr.and.list_carray(ir,1).ne.'End') go to 10
    close (unit=9)
    
    if (do_temperature_profile.and.ntest>=1) then
        open (unit=10,file='profiles.csv',form='formatted', action='write', iostat=io_error)
        if (io_error==0) then
            write (10,'(2000(a,'',''))') ('HGT_'//trim(temperature_profile_name(ic)),'TEMP_'//trim(temperature_profile_name(ic)), ic=1,ntest)
            write (10,'(2000(a,'',''))') ('m','C', ic=1,ntest)
            write (10,'(1000(a,'','',e12.5'',''))') ('0.0',temperature_profile_data(ic,2), ic=1,ntest)
            write (10,'(100(e12.5,'','',e12.5,'',''))') (temperature_profile_data(ic,3),temperature_profile_data(ic,2), ic=1,ntest)
            write (10,'(100(e12.5,'','',e12.5,'',''))') (temperature_profile_data(ic,3),temperature_profile_data(ic,1), ic=1,ntest)
            write (10,'(1000(a,'','',e12.5'',''))') ('100.0',temperature_profile_data(ic,1), ic=1,ntest)
            close (unit=10)
        end if
    end if

    end program VandV_Calcs

