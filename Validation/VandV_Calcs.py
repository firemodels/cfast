
import pandas as pd
import numpy as np
import sys
import os

def main():
    # Variables
    
    # These parameters indicate which calculation to do
    do_activation_time = 1        # if calculating activation times from data
    do_temperature_profile = 2    # if calculating temperature profiles in a compartment
    do_pressure_correction = 3    # if calculating pressure corrected to measurement height
    do_add_columns = 4            # if calculating total net flows through multiple vents
    do_flux_profile = 5           # if calculating radiative heat flux at a number of distances from a fire

    # Fortran parameter equivalents
    ntests = 200
    nrow = 1000
    ncol = 9000
    list_nrow = 1000
    list_ncol = 100

    # Initialization of variables
    base_folder = './'
    
    # Get command argument (Fortran: call get_command_argument (1,filename))
    if len(sys.argv) < 2:
        print('No data file specified')
        sys.exit()
    
    filename = sys.argv[1].strip()
    comparelist_file = os.path.join(base_folder, filename)
    print(f'Opening file: {comparelist_file}')

    # Replace readcsv with pandas read_csv
    # Fortran code reads the list file into list_rarray and list_carray
    # We use a single DataFrame to hold the mixed type data
    try:
        list_df = pd.read_csv(comparelist_file, header=None, low_memory=False)
    except Exception as e:
        print(f"Error opening file {comparelist_file}: {e}")
        sys.exit()

    list_numr = list_df.shape[0]
    list_numc = list_df.shape[1]

    # Helper function: find_column equivalent
    def find_column(df, name_row, col_name):
        # name_row is 1-indexed in Fortran
        # If name_row is 1, it checks the first row of the provided data
        col_name = col_name.strip().lower()
        headers = df.iloc[name_row - 1]
        for i, val in enumerate(headers):
            if str(val).strip().lower() == col_name:
                return i + 1 # Return 1-based index
        return 0

    # determine column locations
    switch_id_column = find_column(list_df, 1, 'switch_id')
    d2_filename_column = find_column(list_df, 1, 'd2_Filename')
    d2_calculation_type_column = find_column(list_df, 1, 'd2_Calculation_Type')
    d2_col_name_row_column = find_column(list_df, 1, 'd2_Col_Name_Row')
    d2_ind_col_name_column = find_column(list_df, 1, 'd2_Ind_Col_Name')
    d2_data_row_column = find_column(list_df, 1, 'd2_Data_Row')
    d2_data_column_count_column = find_column(list_df, 1, 'd2_Data_Column_Count')
    d2_data_col_name_column = find_column(list_df, 1, 'd2_Data_Col_Name')
    d2_constants_count_column = find_column(list_df, 1, 'd2_Constants_Column_Count')
    d2_constants_column = find_column(list_df, 1, 'd2_Constants')
    d2_text_column = find_column(list_df, 1, 'd2_Text')

    # Calculation specific storage
    ntest_temperature_profile = 0
    temperature_profile_data = np.zeros((ntests, 3))
    temperature_profile_name = [""] * ntests

    ntest_pressure_correction = 0
    pressure_correction_data = np.zeros((ntests, nrow, 2))
    numrows_pressure_correction = np.zeros(ntests, dtype=int)
    max_numrows_pressure_correction = 0
    pressure_correction_name = [""] * ntests
    
    ntest_add_columns = 0
    add_columns_data = np.zeros((ntests, nrow, 2))
    numrows_add_columns = np.zeros(ntests, dtype=int)
    max_numrows_add_columns = 0
    add_columns_name = [""] * ntests
    
    ntest_flux_profile = 0
    flux_profile_data = np.zeros((ntests, nrow, 2))
    numrows_flux_profile = np.zeros(ntests, dtype=int)
    max_numrows_flux_profile = 0
    flux_profile_name = [""] * ntests

    old_filename = ' '
    model_df = None
    
    list_txt = None

    ir = 0
    # Main loop (Fortran: 10 ir = ir + 1 ... if (...) go to 10)
    while ir < list_numr:
        ir += 1
        
        # switch_id = list_carray(ir,switch_id_column)
        switch_id = str(list_df.iloc[ir-1, switch_id_column-1]).strip()

        if switch_id == 'End':
            break

        if switch_id == 'd':
            # read in the model data   
            d2_filename_val = str(list_df.iloc[ir-1, d2_filename_column-1]).strip()
            d2_filename = os.path.join(base_folder, d2_filename_val)
            
            if old_filename != d2_filename:
                print(f'Opening data file: {d2_filename}')
                try:
                    # Replace readcsv with pandas read_csv
                    model_df = pd.read_csv(d2_filename, header=None, low_memory=False)
                    print(f'{ir} Finished reading file {d2_filename}')
                    old_filename = d2_filename
                except Exception as e:
                    print(f'Error opening file, iostat error: {e}')
                    sys.exit()

            # Helper for safe float conversion
            def to_f(val):
                try:
                    return float(val)
                except:
                    return 0.0

            d2_calculation_type = int(to_f(list_df.iloc[ir-1, d2_calculation_type_column-1]))
            d2_col_name_row = int(to_f(list_df.iloc[ir-1, d2_col_name_row_column-1]))
            d2_ind_col_name = str(list_df.iloc[ir-1, d2_ind_col_name_column-1]).strip()
            
            # find_column replacement for model data
            d2_ind_data_col = find_column(model_df, d2_col_name_row, d2_ind_col_name)
            
            d2_data_row = int(to_f(list_df.iloc[ir-1, d2_data_row_column-1]))
            d2_column_count = int(to_f(list_df.iloc[ir-1, d2_data_column_count_column-1]))
            
            d2_data_col_names = []
            for ic in range(1, d2_column_count + 1):
                col_name_val = str(list_df.iloc[ir-1, d2_data_col_name_column + ic - 2]).strip()
                d2_data_col_names.append(col_name_val)
            
            d2_constants_count = int(to_f(list_df.iloc[ir-1, d2_constants_count_column-1]))
            d2_constants = []
            for ic in range(1, d2_constants_count + 1):
                const_val = to_f(list_df.iloc[ir-1, d2_constants_column + ic - 2])
                d2_constants.append(const_val)

            # Helper function: load_vector equivalent
            def load_vector(df, col_idx, start_row):
                # col_idx and start_row are 1-based
                if col_idx == 0: return np.array([]), 0
                series = df.iloc[start_row-1:, col_idx-1]
                vec = pd.to_numeric(series, errors='coerce').fillna(0.0).values
                # Limit to nrow as per Fortran array definition
                if len(vec) > nrow: vec = vec[:nrow]
                return vec, len(vec)

            # Get the vectors for this calculation ...d2x and d2y(i)
            d2x, d2x_len = load_vector(model_df, d2_ind_data_col, d2_data_row)

            # The Fortran 'do while (len_trim(d2_data_col_names(1))>0)' behaves as an 'if' 
            # in a standard processing loop unless d2_data_col_names is cleared inside.
            if len(d2_data_col_names) > 0 and d2_data_col_names[0] != "":
                d2ys = np.zeros((nrow, d2_column_count))
                d2ys_len = np.zeros(d2_column_count, dtype=int)
                
                for ic in range(1, d2_column_count + 1):
                    d2_data_col_name = d2_data_col_names[ic-1]
                    # find_column_name replacement (implied simple cleanup)
                    d2_data_col_name_clean = d2_data_col_name.strip()
                    d2_data_data_col = find_column(model_df, d2_col_name_row, d2_data_col_name_clean)
                    
                    d2y, d2y_len = load_vector(model_df, d2_data_data_col, d2_data_row)
                    # copy_vector equivalent
                    d2ys[:d2y_len, ic-1] = d2y
                    d2ys_len[ic-1] = d2y_len

                # Calculations based on d2_calculation_type
                if d2_calculation_type == do_add_columns:
                    for ic in range(d2_column_count):
                        if d2x_len != d2ys_len[ic]:
                            print(f'Data error, x and y lengths are not equal {d2x_len} {d2ys_len[ic]} {ic+1}')
                            sys.exit()
                    if d2_text_column == 0:
                        print(f'Data error, no column name for summed flow output {ic+1}')
                        sys.exit()
                    
                    ntest_add_columns += 1
                    numrows_add_columns[ntest_add_columns-1] = d2x_len
                    max_numrows_add_columns = max(max_numrows_add_columns, d2x_len)
                    add_columns_name[ntest_add_columns-1] = str(list_df.iloc[ir-1, d2_text_column-1]).strip()
                    
                    for irr in range(d2x_len):
                        add_columns_data[ntest_add_columns-1, irr, 0] = d2x[irr]
                        sum_val = 0.0
                        for ic in range(d2_column_count):
                            sum_val += d2ys[irr, ic]
                        add_columns_data[ntest_add_columns-1, irr, 1] = sum_val
                
                elif d2_calculation_type == do_activation_time:
                    activation_time = 0.0
                    if d2x_len == d2ys_len[0]:
                        for i in range(d2x_len):
                            if d2ys[i, 0] != 0.0:
                                activation_time = d2x[i]
                                break
                    else:
                        print(f'Data error, x and y lengths are not equal {d2x_len} {d2ys_len[0]}')
                        sys.exit()

                    partial_filename = d2_filename
                    if len(d2_filename) > 20:
                        partial_filename = '...' + d2_filename[-20:]
                    
                    # write (9,'(i4,3x,a25,3x,a15,f12.3)')
                    if list_txt is None:
                        list_txt = open('list.txt', 'w')
                    list_txt.write(f"{ir:4}   {partial_filename:25}   {d2_data_col_names[0][:15]:15}   {activation_time:12.3f}\n")

                elif d2_calculation_type == do_temperature_profile:
                    if d2x_len == d2ys_len[0] and d2x_len == d2ys_len[1] and d2x_len == d2ys_len[2]:
                        ntest_temperature_profile += 1
                        temperature_profile_data[ntest_temperature_profile-1, 0] = d2ys[d2ys_len[0]-1, 0]
                        temperature_profile_data[ntest_temperature_profile-1, 1] = d2ys[d2ys_len[1]-1, 1]
                        temperature_profile_data[ntest_temperature_profile-1, 2] = d2ys[d2ys_len[2]-1, 2]
                        
                        iloc = d2_filename.find('_compartments.csv')
                        # d2_filename(iloc-3:iloc-1)
                        temperature_profile_name[ntest_temperature_profile-1] = 'Test_' + d2_filename[iloc-3:iloc]
                    else:
                        print(f'Data error, x and y lengths are not equal {d2x_len} {d2ys_len[0]}')
                        sys.exit()

                elif d2_calculation_type == do_pressure_correction:
                    if d2x_len == d2ys_len[0] and d2x_len == d2ys_len[1] and d2x_len == d2ys_len[2] and d2x_len == d2ys_len[3]:
                        ntest_pressure_correction += 1
                        numrows_pressure_correction[ntest_pressure_correction-1] = d2x_len
                        max_numrows_pressure_correction = max(max_numrows_pressure_correction, d2x_len)
                        
                        g = 9.8
                        # rhoinf = 352.8/(d2ys(1,2)+273.15) 
                        rhoinf = 352.8 / (d2ys[0, 1] + 273.15)
                        
                        for irr in range(d2x_len):
                            pressure_correction_data[ntest_pressure_correction-1, irr, 0] = d2x[irr]
                            tu1 = d2ys[irr, 0] + 273.15
                            rhou1 = 352.8 / tu1
                            tu2 = d2ys[irr, 4] + 273.15
                            rhou2 = 352.8 / tu2
                            tl1 = d2ys[irr, 1] + 273.15
                            rhol1 = 352.8 / tl1
                            tl2 = d2ys[irr, 5] + 273.15
                            rhol2 = 352.8 / tl2
                            h1 = d2ys[irr, 2]
                            h2 = d2ys[irr, 6]
                            delta_pf1 = d2ys[irr, 3]
                            
                            y = d2_constants[0]
                            hflr = d2_constants[1]
                            
                            if hflr == 0.0:
                                if y <= h1:
                                    delta_py = delta_pf1 - rhol1 * g * y + rhoinf * g * y
                                else:
                                    delta_py = delta_pf1 - rhol1 * g * h1 - rhou1 * g * (y - h1) + rhoinf * g * y
                            elif y <= h2:
                                delta_p1 = delta_pf1 - rhol1 * g * h1 - rhou1 * g * (hflr - h1) + rhoinf * g * hflr
                                delta_p2 = -rhol2 * g * y + rhoinf * g * y
                                delta_py = delta_p1 + delta_p2
                            elif y > h2:
                                delta_p1 = delta_pf1 - rhol1 * g * h1 - rhou1 * g * (hflr - h1) + rhoinf * g * hflr
                                delta_p2 = - rhol2 * g * (h2) - rhou2 * g * (y - h2) + rhoinf * g * y
                                delta_py = delta_p1 + delta_p2
                            
                            pressure_correction_data[ntest_pressure_correction-1, irr, 1] = delta_py
                        
                        iloc = d2_filename.find('_compartments.csv')
                        # Logical checks for filename patterns
                        if d2_filename.find('p_compartments.csv') != -1:
                            pressure_correction_name[ntest_pressure_correction-1] = 'Test_' + d2_filename[iloc-2:iloc]
                        else:
                            pressure_correction_name[ntest_pressure_correction-1] = 'Test_' + d2_filename[iloc-2:iloc]
                    
                elif d2_calculation_type == do_flux_profile:
                    if d2_column_count == d2_constants_count:
                        ntest_flux_profile += 1
                        flux_profile_name[ntest_flux_profile-1] = str(list_df.iloc[ir-1, d2_text_column-1]).strip()
                        numrows_flux_profile[ntest_flux_profile-1] = d2_column_count
                        max_numrows_flux_profile = max(max_numrows_flux_profile, d2_column_count)
                        
                        for i in range(d2_column_count):
                            irrcount = 0
                            d2ysum = 0.0
                            # irr = d2x_len/2, d2x_len (Fortran)
                            # Maps to 0-indexed range(d2x_len // 2 - 1, d2x_len) 
                            for irr in range(d2x_len // 2 - 1, d2x_len):
                                irrcount += 1
                                d2ysum += d2ys[irr, i]
                            
                            flux_profile_data[ntest_flux_profile-1, i, 0] = d2_constants[i]
                            flux_profile_data[ntest_flux_profile-1, i, 1] = d2ysum / irrcount if irrcount > 0 else 0.0
                
                else:
                    print('Invalid specifier for d2_calculation_type')
                    sys.exit()

    if list_txt is not None:
        list_txt.close()
    
    # Helper functions for structured output (SSaddtolist / SSprintresults)
    def ss_add_to_list(print_array, value, text=' '):
        if text != ' ':
            print_array.append(text)
        else:
            print_array.append(str(value))

    def ss_print_results(file_handle, print_array):
        file_handle.write(",".join(print_array) + ",\n")
        print_array.clear()

    # Summed flows output
    if ntest_add_columns >= 1:
        try:
            with open('summed_flows.csv', 'w') as f:
                header1 = []
                header2 = []
                for ic in range(ntest_add_columns):
                    name = add_columns_name[ic].strip()
                    header1.extend([f"TIME_{name}", name])
                    header2.extend(["s", "kg/s"])
                f.write(",".join(header1) + ",\n")
                f.write(",".join(header2) + ",\n")
                
                for irr in range(max_numrows_add_columns):
                    row_list = []
                    for it in range(ntest_add_columns):
                        if irr < numrows_add_columns[it]:
                            ss_add_to_list(row_list, add_columns_data[it, irr, 0])
                            ss_add_to_list(row_list, add_columns_data[it, irr, 1])
                        else:
                            ss_add_to_list(row_list, 0.0, 'NaN')
                            ss_add_to_list(row_list, 0.0, 'NaN')
                    ss_print_results(f, row_list)
        except Exception as e:
            print(f"Error writing summed_flows.csv: {e}")

    # Temperature profile output
    if ntest_temperature_profile >= 1:
        try:
            with open('Steckler_Compartment/profiles.csv', 'w') as f:
                header1 = []
                for ic in range(ntest_temperature_profile):
                    name = temperature_profile_name[ic].strip()
                    header1.extend([f"HGT_{name}", f"TEMP_{name}"])
                f.write(",".join(header1) + ",\n")
                
                header2 = []
                for ic in range(ntest_temperature_profile):
                    header2.extend(["m", "C"])
                f.write(",".join(header2) + ",\n")
                
                # Row 1
                row = []
                for ic in range(ntest_temperature_profile):
                    row.extend(["0.0", f"{temperature_profile_data[ic, 1]:.5e}"])
                f.write(",".join(row) + ",\n")
                
                # Row 2
                row = []
                for ic in range(ntest_temperature_profile):
                    row.extend([f"{temperature_profile_data[ic, 2]:.5e}", f"{temperature_profile_data[ic, 1]:.5e}"])
                f.write(",".join(row) + ",\n")
                
                # Row 3
                row = []
                for ic in range(ntest_temperature_profile):
                    row.extend([f"{temperature_profile_data[ic, 2]:.5e}", f"{temperature_profile_data[ic, 0]:.5e}"])
                f.write(",".join(row) + ",\n")
                
                # Row 4
                row = []
                for ic in range(ntest_temperature_profile):
                    row.extend(["100.0", f"{temperature_profile_data[ic, 0]:.5e}"])
                f.write(",".join(row) + ",\n")
        except Exception as e:
            print(f"Error writing profiles.csv: {e}")
    
    # Pressure height correction output
    if ntest_pressure_correction >= 1:
        try:
            with open('LLNL_Enclosure/LLNL_pressures.csv', 'w') as f:
                header1 = []
                header2 = []
                for ic in range(ntest_pressure_correction):
                    name = pressure_correction_name[ic].strip()
                    header1.extend([f"TIME_{name}", f"PRS_{name}"])
                    header2.extend(["s", "Pa"])
                f.write(",".join(header1) + ",\n")
                f.write(",".join(header2) + ",\n")
                
                for irr in range(max_numrows_pressure_correction):
                    row_list = []
                    for it in range(ntest_pressure_correction):
                        if irr < numrows_pressure_correction[it]:
                            ss_add_to_list(row_list, pressure_correction_data[it, irr, 0])
                            ss_add_to_list(row_list, pressure_correction_data[it, irr, 1])
                        else:
                            ss_add_to_list(row_list, 0.0, 'NaN')
                            ss_add_to_list(row_list, 0.0, 'NaN')
                    ss_print_results(f, row_list)
        except Exception as e:
            print(f"Error writing pressures.csv: {e}")
            
    # Heat flux profile output
    if ntest_flux_profile >= 1:
        try:
            with open('Fleury_Heat_Flux/flux_profiles.csv', 'w') as f:
                header1 = []
                header2 = []
                for ic in range(ntest_flux_profile):
                    name = flux_profile_name[ic].strip()
                    header1.extend([f"DIST_{name}", f"FLUX_{name}"])
                    header2.extend(["m", "KW/m^2"])
                f.write(",".join(header1) + ",\n")
                f.write(",".join(header2) + ",\n")
                
                for irr in range(max_numrows_flux_profile):
                    row_list = []
                    for it in range(ntest_flux_profile):
                        if irr < numrows_flux_profile[it]:
                            ss_add_to_list(row_list, flux_profile_data[it, irr, 0])
                            ss_add_to_list(row_list, flux_profile_data[it, irr, 1])
                        else:
                            ss_add_to_list(row_list, 0.0, 'NaN')
                            ss_add_to_list(row_list, 0.0, 'NaN')
                    ss_print_results(f, row_list)
        except Exception as e:
            print(f"Error writing flux_profiles.csv: {e}")

if __name__ == "__main__":
    main()

