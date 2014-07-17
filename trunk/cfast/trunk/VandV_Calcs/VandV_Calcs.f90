    program VandV_Calcs

    implicit none

    ! Variables

    ! These parameters indicate which calculation to do
    integer, parameter :: do_activation_time = 1        ! if calculating activation times from data
    integer, parameter :: do_temperature_profile = 2    ! if calculating temperature profiles in a compartment
    integer, parameter :: do_pressure_correction = 3    ! if calculating pressure corrected to measurement height
    integer, parameter :: do_add_columns = 4            ! if calculating total net flows through multiple vents


    integer, parameter :: ntests = 200, nrow = 10000, ncol = 500, list_nrow = 1000, list_ncol = 100

    character :: base_folder*128, comparelist_file*128, partial_filename*128, filename*128
    character :: list_carray(list_nrow, list_ncol)*128, model_carray(nrow,ncol)*50, switch_id*3

    integer list_numr, list_numc, model_numr, model_numc, find_column, io_error, ir, ic, it, i, istat, irr
    integer :: switch_id_column
    integer :: d2_filename_column, d2_calculation_type_column, d2_col_name_row_column, d2_ind_col_name_column, d2_data_row_column, &
        d2_data_col_name_column, d2_data_column_count_column, d2_constants_count_column, d2_constants_column, d2_text_column

    character :: d2_filename*128, d2_ind_col_name*128, d2_data_col_names(list_ncol)*128, d2_data_col_name*128
    integer :: d2_calculation_type, d2_col_name_row, d2_data_row, d2_ind_data_col, d2_data_data_col(list_ncol), d2_data_column_count, d2x_len, d2y_len, d2ys_len(list_ncol), &
        d2_constants_count, d2_constants(list_ncol)

    real :: list_rarray(list_nrow, list_ncol), model_rarray(nrow,ncol), d2x(nrow), d2y(nrow), d2ys(nrow,list_ncol)
    character(30) :: print_array(32000)
    integer :: position

    ! Calculation specific variables
    real :: activation_time                         ! for activation time calculation

    real :: temperature_profile_data(ntests,3)      ! for temperature profile calculation
    character :: temperature_profile_name(ntests)*30
    integer ::  ntest_temperature_profile

    real :: pressure_correction_data(ntests,nrow,2) ! for pressure correction calculation
    real :: tu1, tl1, h1, delta_pf1, rhou1, rhol1, delta_p1, tu2, tl2, h2, delta_pf2, rhou2, rhol2, delta_p2
    real :: delta_py, rhoinf, g, hflr, y
    character :: pressure_correction_name(ntests)*30
    integer :: ntest_pressure_correction, numrows_pressure_correction(ntests), max_numrows_pressure_correction
    
    real :: add_columns_data(ntests,nrow,2)           ! for flow summing calculation
    integer :: ntest_add_columns, numrows_add_columns(ntests), max_numrows_add_columns
    character :: add_columns_name(ntests)*30

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
    d2_calculation_type_column =find_column(list_carray,list_nrow,list_ncol,1,'d2_Calculation_Type')
    d2_col_name_row_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Col_Name_Row')
    d2_ind_col_name_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Ind_Col_Name')
    d2_data_row_column =  find_column(list_carray,list_nrow,list_ncol,1,'d2_Data_Row')
    d2_data_column_count_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Data_Column_Count')
    d2_data_col_name_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Data_Col_Name')
    d2_constants_count_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Constants_Column_Count')
    d2_constants_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Constants')
    d2_text_column = find_column(list_carray,list_nrow,list_ncol,1,'d2_Text')

    ir=0
    ntest_temperature_profile = 0
    ntest_pressure_correction = 0
    ntest_add_columns = 0
    max_numrows_pressure_correction = 0
    max_numrows_add_columns = 0
10  ir = ir + 1
    switch_id = list_carray(ir,switch_id_column)

    if (switch_id=='d') then
        ! read in the model data   
        d2_filename = trim(base_folder) // list_carray(ir,d2_filename_column)
        write (*,*) ir,trim(d2_filename)
        open (unit=8,file=d2_filename,form='formatted',action='read',iostat=io_error)
        call readcsv(8,model_rarray,model_carray,nrow,ncol,1,model_numr, model_numc)
        close (unit=8)

        d2_calculation_type = list_rarray(ir,d2_calculation_type_column)
        d2_col_name_row = list_rarray(ir,d2_col_name_row_column)
        d2_ind_col_name = list_carray(ir,d2_ind_col_name_column)
        d2_ind_data_col = find_column(model_carray,nrow,ncol,d2_col_name_row,d2_ind_col_name)
        d2_data_row = list_rarray(ir,d2_data_row_column)
        d2_data_column_count = list_rarray(ir,d2_data_column_count_column)
        do ic = 1, d2_data_column_count
            d2_data_col_names(ic) = list_carray(ir,d2_data_col_name_column+ic-1)
        end do
        d2_constants_count = list_rarray(ir,d2_constants_count_column)
        do ic = 1, d2_constants_count
            d2_constants(ic) = list_rarray(ir,d2_constants_column+ic-1)
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
            select case (d2_calculation_type)
                
            ! Calculations to add flow columns together
            case (do_add_columns)
                do ic = 1,d2_data_column_count
                    if (d2x_len/=d2ys_len(ic)) then
                        write (*,*) 'Data error, x and y lengths are not equal', d2x_len, d2ys_len(ic),ic
                        stop
                    end if
                end do
                if (d2_text_column==0) then
                    write (*,*) 'Data error, no column name for summed flow output', ic
                    stop
                end if
                ntest_add_columns = ntest_add_columns + 1
                numrows_add_columns(ntest_add_columns) = d2x_len
                max_numrows_add_columns=max(max_numrows_add_columns,d2x_len)
                add_columns_name(ntest_add_columns) = list_carray(ir,d2_text_column)
                do irr = 1, d2x_len
                    add_columns_data(ntest_add_columns,irr,1) = d2x(irr)
                    add_columns_data(ntest_add_columns,irr,2) = 0.0
                    do ic = 1, d2_data_column_count
                        add_columns_data(ntest_add_columns,irr,2) = add_columns_data(ntest_add_columns,irr,2) + d2ys(irr,ic)
                    end do
                end do
                
            ! Calculations of detector / sprinkler activation times
            case (do_activation_time)
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
            case (do_temperature_profile)
                if (d2x_len==d2ys_len(1).and.d2x_len==d2ys_len(2).and.d2x_len==d2ys_len(3)) then
                    ntest_temperature_profile = ntest_temperature_profile + 1
                    temperature_profile_data(ntest_temperature_profile,1) = d2ys(d2ys_len(1),1)
                    temperature_profile_data(ntest_temperature_profile,2) = d2ys(d2ys_len(2),2)
                    temperature_profile_data(ntest_temperature_profile,3) = d2ys(d2ys_len(3),3)
                    temperature_profile_name(ntest_temperature_profile) = 'Test_' // trim(d2_filename(len_trim(d2_filename)-8:len_trim(d2_filename)-6)) ! This works for a 3 digit filename numbering (as the Steckler Compartment tests are done)
                else
                    write (*,*) 'Data error, x and y lengths are not equal', d2x_len, d2y_len
                    stop
                end if

            ! Calculations to correct pressure to height matching measured pressure
            case (do_pressure_correction)
                if (d2x_len==d2ys_len(1).and.d2x_len==d2ys_len(2).and.d2x_len==d2ys_len(3).and.d2x_len==d2ys_len(4)) then
                    ntest_pressure_correction = ntest_pressure_correction + 1
                    numrows_pressure_correction(ntest_pressure_correction) = d2x_len
                    max_numrows_pressure_correction=max(max_numrows_pressure_correction,d2x_len)
                    g = 9.8
                    rhoinf = 352.8/(d2ys(1,2)+273.15) ! gas density assuming surrounding ambient is initial temperature of lower layer
                    do irr = 1, d2x_len
                        pressure_correction_data(ntest_pressure_correction,irr,1) = d2x(irr)
                        tu1 = d2ys(irr,1)+273.15    ! upper layer temperature
                        rhou1 = 352.8/tu1           ! upper layer gas density from SPFE handbook chapter on vent flow
                        tu2 = d2ys(irr,5) + 273.15
                        rhou2 = 352.8/tu2
                        tl1 = d2ys(irr,2)+273.15 ! lower layer temperature
                        rhol1 = 352.8/tl1         ! lower layer gas density from SPFE handbook chapter on vent flow
                        tl2 = d2ys(irr,6)+273.15
                        rhol2 = 352.8/tl2
                        h1 = d2ys(irr,3)         ! layer height
                        h2 = d2ys(irr,7)
                        delta_pf1 = d2ys(irr,4)  ! pressure difference at the floor
                        y = d2_constants(1)
                        hflr = d2_constants(2)
                        if (hflr==0.0) then
                            if (y<=h1) then
                                delta_py = delta_pf1 - rhol1*g*y + rhoinf*g*y
                            else
                                delta_py = delta_pf1 - rhol1*g*h1 - rhou1*g*(y-h1) + rhoinf*g*y
                            end if
                        else if (y<=h2) then
                            delta_p1 = delta_pf1 - rhol1*g*h1 - rhou1*g*(hflr-h1) + rhoinf*g*hflr
                            delta_p2 = -rhol1*g*y + rhoinf*g*y
                            delta_py = delta_p1 + delta_p2
                        else if (y>h2) then
                            delta_p1 = delta_pf1 - rhol1*g*h1 - rhou1*g*(hflr-h1) + rhoinf*g*hflr
                            delta_p2 = - rhol2*g*(h2) - rhou2*g*(y-h2) + rhoinf*g*y
                            delta_py = delta_p1 + delta_p2
                        end if
                        pressure_correction_data(ntest_pressure_correction,irr,2) = delta_py
                    end do  
                    if (index(d2_filename,'p_n.csv')/=0) then
                        pressure_correction_name(ntest_pressure_correction) = 'Test_' // trim(d2_filename(len_trim(d2_filename)-8:len_trim(d2_filename)-7)) ! This works for a 2 digit filename numbering (as the LLNL Enclosure tests are done)
                    else
                        pressure_correction_name(ntest_pressure_correction) = 'Test_' // trim(d2_filename(len_trim(d2_filename)-7:len_trim(d2_filename)-6)) ! This works for a 2 digit filename numbering (as the LLNL Enclosure tests are done)
                    end if
                end if
                
            ! Incorrect inputs, just throw up hands and quit
            case default
            stop 'Invalid specifier for d2_calculation_type'
            end select
        end do
    end if

    if (ir.le.list_numr.and.list_carray(ir,1).ne.'End') go to 10
    close (unit=9)
    
    if (ntest_add_columns>=1) then
        open (unit=10,file='added_flows.csv',form='formatted', action='write', iostat=io_error)
        if (io_error==0) then
            write (10,'(2000(a,'',''))') ('TIME_'//trim(add_columns_name(ic)),trim(add_columns_name(ic)), ic=1,ntest_add_columns)
            write (10,'(2000(a,'',''))') ('s','kg/s', ic=1,ntest_add_columns)
            do irr = 1, max_numrows_add_columns
                position = 0
                do it = 1, ntest_add_columns
                    if (irr<=numrows_add_columns(it)) then
                        call SSaddtolist(position,1,add_columns_data(it,irr,1),' ',print_array)
                        call SSaddtolist(position,1,add_columns_data(it,irr,2),' ',print_array)
                    else
                        call SSaddtolist(position,2,0.0,'NaN',print_array)
                        call SSaddtolist(position,2,0.0,'NaN',print_array)
                    end if
                end do
                    call SSprintresults(10,position,print_array)
            end do
        end if
        close (unit=10)
    end if
        
    if (ntest_temperature_profile>=1) then
        open (unit=10,file='profiles.csv',form='formatted', action='write', iostat=io_error)
        if (io_error==0) then
            write (10,'(2000(a,'',''))') ('HGT_'//trim(temperature_profile_name(ic)),'TEMP_'//trim(temperature_profile_name(ic)), ic=1,ntest_temperature_profile)
            write (10,'(2000(a,'',''))') ('m','C', ic=1,ntest_temperature_profile)
            write (10,'(1000(a,'','',e12.5'',''))') ('0.0',temperature_profile_data(ic,2), ic=1,ntest_temperature_profile)
            write (10,'(100(e12.5,'','',e12.5,'',''))') (temperature_profile_data(ic,3),temperature_profile_data(ic,2), ic=1,ntest_temperature_profile)
            write (10,'(100(e12.5,'','',e12.5,'',''))') (temperature_profile_data(ic,3),temperature_profile_data(ic,1), ic=1,ntest_temperature_profile)
            write (10,'(1000(a,'','',e12.5'',''))') ('100.0',temperature_profile_data(ic,1), ic=1,ntest_temperature_profile) ! Note we're assuming ceiling height is less than 100 m
            close (unit=10)
        end if
    end if
    
    if (ntest_pressure_correction>=1) then
        open (unit=10,file='pressures.csv',form='formatted', action='write', iostat=io_error)
        if (io_error==0) then
            write (10,'(2000(a,'',''))') ('TIME_'//trim(pressure_correction_name(ic)),'PRS_'//trim(pressure_correction_name(ic)), ic=1,ntest_pressure_correction)
            write (10,'(2000(a,'',''))') ('s','Pa', ic=1,ntest_pressure_correction)
            do irr = 1, max_numrows_pressure_correction
                position = 0
                do it = 1, ntest_pressure_correction
                    if (irr<=numrows_pressure_correction(it)) then
                        call SSaddtolist(position,1,pressure_correction_data(it,irr,1),' ',print_array)
                        call SSaddtolist(position,1,pressure_correction_data(it,irr,2),' ',print_array)
                    else
                        call SSaddtolist(position,2,0.0,'NaN',print_array)
                        call SSaddtolist(position,2,0.0,'NaN',print_array)
                    end if
                end do
                    call SSprintresults(10,position,print_array)
            end do
        end if
        close (unit=10)
    end if

    end program VandV_Calcs

