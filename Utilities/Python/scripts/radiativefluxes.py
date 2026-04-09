
import os
import pandas as pd
import numpy as np

def radiativefluxes(data_dir):
    # This function will do a transpose of a matrix for the results obtained 
    # from CFAST verification cases: radiation_3.in, radiation_4.in, and 
    # radiation_5.in
    #
    # Radiative heat flux as a function of location is desired. Yet in CFAST,
    # the calculated results for each target (for different locations)for a
    # specific time step are presented in a single row. With the fact that the   
    # current Matlab script to handle verification and validation take a single 
    # column of values. Thus, a transpose of matrix is needed.

    # Helper to handle the logic of importdata(filename, ',', 5) and prefix matching
    def process_cfast_file(filepath):
        # importdata(filename, ',', 5) skips 5 lines for numeric data.
        # Z.data contains the numeric values.
        # Z.textdata{1,1} contains the first line of the file.
        
        # Read the first line to get column names (W = strsplit(Z.textdata{1,1}, ','))
        with open(filepath, 'r') as f:
            first_line = f.readline()
        w_cols = [col.strip() for col in first_line.split(',')]
        
        # Read the numeric data skipping 5 lines
        # In Matlab, importdata(..., 5) means the numeric data starts after 5 header lines.
        z_data = pd.read_csv(filepath, skiprows=5, header=None).values
        
        return w_cols, z_data

    # Helper for find(strncmpi(W, prefix, len))
    def find_cols_by_prefix(w_list, prefix, length):
        return [i for i, name in enumerate(w_list) if name[:length].upper() == prefix.upper()]

    ## Case 1: radiation_3.in
    filename = os.path.join(data_dir, 'radiation_3_devices.csv')
    # X1 = [0.0909,...]'; (Column vector 11x1)
    x1 = np.array([0.0909,0.2727,0.4545,0.6364,0.8182,1.0000,1.1818,1.3636,1.5455,1.7273,1.9091]).reshape(-1, 1)
    # X2 = [0.125,...]'; (Column vector 16x1)
    x2 = np.array([0.125,0.375,0.625,0.875,1.125,1.375,1.625,1.875,2.125,2.375,2.625,2.875,3.125,3.375,3.625,3.875]).reshape(-1, 1)
    
    w, z_data = process_cfast_file(filename)

    t_gas = 1000 # in K
    e_val = 5.67e-8 * (t_gas**4) / 1000

    # find the locations for all of the variable columns
    # col = find(strncmpi(W,'TRGFLXG',7))';
    col_idx = find_cols_by_prefix(w, 'TRGFLXG', 7)
    cfast_targ_flux_gas = z_data[:, col_idx]
    
    # In Matlab, linear indexing cfast_targ_flux_gas(1:11)' on a matrix 
    # extract the first 11 elements in column-major order.
    # Given the context of CFAST verification, this typically extracts values from the first time row.
    # Flattening in 'F' order (column-major) mimics Matlab linear indexing.
    cfast_targ_top_gas = cfast_targ_flux_gas.flatten('F')[0:11].reshape(-1, 1)
    cfast_targ_side_gas = cfast_targ_flux_gas.flatten('F')[11:27].reshape(-1, 1)
    
    cfast_emittance_top = cfast_targ_top_gas / e_val
    cfast_emittance_side = cfast_targ_side_gas / e_val
    
    col_idx = find_cols_by_prefix(w, 'TRGFLXS', 7)
    cfast_targ_flux_wall = z_data[:, col_idx]
    cfast_targ_top_wall  = cfast_targ_flux_wall.flatten('F')[0:11].reshape(-1, 1)
    cfast_targ_side_wall = cfast_targ_flux_wall.flatten('F')[11:27].reshape(-1, 1)
    
    # note that cfast_targ_top_wall is the sum of the radiative heat transfer to
    # surface A_i due to emission from all hot walls (A_j) transmitted through 
    # medium. Thus, blackbody emissive power - the transmitted energy = energy
    # being absorbed by the medium at (T_w, T_g)
    cfast_absorptance_top = (e_val - cfast_targ_top_wall) / e_val
    cfast_absorptance_side = (e_val - cfast_targ_side_wall) / e_val

    header1 = 'X, GAS(TOP), WALL(TOP)'
    header2 = 'Z, GAS(SIDE), WALL(SIDE)'
    data1 = np.hstack((x1, cfast_targ_top_gas, cfast_targ_top_wall))
    data2 = np.hstack((x2, cfast_targ_side_gas, cfast_targ_side_wall))

    out_path1 = os.path.join(data_dir, 'radiation_3_w_top.csv')
    with open(out_path1, 'w') as outid:
        outid.write(header1 + '\n')
        # dlmwrite with roffset 1 and append effectively starts data on the next line
        np.savetxt(outid, data1, delimiter=',', fmt='%.10f')

    out_path2 = os.path.join(data_dir, 'radiation_3_w_side.csv')
    with open(out_path2, 'w') as outid:
        outid.write(header2 + '\n')
        np.savetxt(outid, data2, delimiter=',', fmt='%.10f')

    ## Case 2: radiation_4.in
    filename = os.path.join(data_dir, 'radiation_4_devices.csv')
    x1 = np.array([0.0909,0.2727,0.4545,0.6364,0.8182,1.0000,1.1818,1.3636,1.5455,1.7273,1.9091]).reshape(-1, 1)
    # x2 = np.array([0.125,0.375,0.625,0.875,1.125,1.375,1.625,1.875,2.125,2.375,2.625,2.875,3.125,3.375,3.625,3.875]).reshape(-1, 1)
    
    w, z_data = process_cfast_file(filename)

    t_wall = 500 # in K
    t_gas = 1000
    e_wall = 5.67e-8 * (t_wall**4) / 1000
    e_gas = 5.67e-8 * (t_gas**4) / 1000

    # find the locations for all of the variable columns
    col_idx = find_cols_by_prefix(w, 'TRGFLXG', 7)
    cfast_targ_flux_gas = z_data[:, col_idx]
    cfast_targ_top_gas  = cfast_targ_flux_gas.flatten('F')[0:11].reshape(-1, 1)
    # cfast_targ_side_gas = cfast_targ_flux_gas.flatten('F')[11:27].reshape(-1, 1)
    cfast_emittance_top = cfast_targ_top_gas / e_gas
    # cfast_emittance_side = cfast_targ_side_gas / e_gas
    
    col_idx = find_cols_by_prefix(w, 'TRGFLXS', 7)
    cfast_targ_flux_wall = z_data[:, col_idx]
    cfast_targ_top_wall  = cfast_targ_flux_wall.flatten('F')[0:11].reshape(-1, 1)
    # cfast_targ_side_wall = cfast_targ_flux_wall.flatten('F')[11:27].reshape(-1, 1)
    cfast_absorptance_top = (e_wall - cfast_targ_top_wall) / e_wall
    # cfast_absorptance_side = cfast_targ_side_wall / e_wall

    header3 = 'X, EMIS(TOP), ABSO(TOP)'
    # header4 = 'Z, EMIS(SIDE), ABSO(SIDE)'
    data3 = np.hstack((x1, cfast_emittance_top, cfast_absorptance_top))
    # data4 = np.hstack((x2, cfast_emittance_side, cfast_absorptance_side))

    out_path3 = os.path.join(data_dir, 'radiation_4_a_top.csv')
    with open(out_path3, 'w') as outid:
        outid.write(header3 + '\n')
        np.savetxt(outid, data3, delimiter=',', fmt='%.10f')

    # outid = open(os.path.join(data_dir, 'radiation_4_a_side.csv'), 'w+')
    # outid.write(header4)
    # outid.close()
    # np.savetxt(os.path.join(data_dir, 'radiation_4_a_side.csv'), data4, delimiter=',', ...)

    ## Case 3: radiation_5.in
    filename = os.path.join(data_dir, 'radiation_5_devices.csv')
    x1 = np.array([0.0909,0.2727,0.4545,0.6364,0.8182,1.0000,1.1818,1.3636,1.5455,1.7273,1.9091]).reshape(-1, 1)
    # x2 = np.array([0.125,0.375,0.625,0.875,1.125,1.375,1.625,1.875,2.125,2.375,2.625,2.875,3.125,3.375,3.625,3.875]).reshape(-1, 1)
    
    w, z_data = process_cfast_file(filename)

    t_wall = 1500 # in K
    t_gas = 1000
    e_wall = 5.67e-8 * (t_wall**4) / 1000
    e_gas = 5.67e-8 * (t_gas**4) / 1000

    # find the locations for all of the variable columns
    col_idx = find_cols_by_prefix(w, 'TRGFLXG', 7)
    cfast_targ_flux_gas = z_data[:, col_idx]
    cfast_targ_top_gas  = cfast_targ_flux_gas.flatten('F')[0:11].reshape(-1, 1)
    # cfast_targ_side_gas = cfast_targ_flux_gas.flatten('F')[11:27].reshape(-1, 1)
    cfast_emittance_top = cfast_targ_top_gas / e_gas
    # cfast_emittance_side = cfast_targ_side_gas / e_gas
    
    col_idx = find_cols_by_prefix(w, 'TRGFLXS', 7)
    cfast_targ_flux_wall = z_data[:, col_idx]
    cfast_targ_top_wall  = cfast_targ_flux_wall.flatten('F')[0:11].reshape(-1, 1)
    # cfast_targ_side_wall = cfast_targ_flux_wall.flatten('F')[11:27].reshape(-1, 1)
    cfast_absorptance_top = (e_wall - cfast_targ_top_wall) / e_wall
    # cfast_absorptance_side = cfast_targ_side_wall / e_wall

    header3 = 'X, EMIS(TOP), ABSO(TOP)'
    # header4 = 'Z, EMIS(SIDE), ABSO(SIDE)'
    data3 = np.hstack((x1, cfast_emittance_top, cfast_absorptance_top))
    # data4 = np.hstack((x2, cfast_emittance_side, cfast_absorptance_side))

    out_path5 = os.path.join(data_dir, 'radiation_5_a_top.csv')
    with open(out_path5, 'w') as outid:
        outid.write(header3 + '\n')
        np.savetxt(outid, data3, delimiter=',', fmt='%.10f')

    # outid = open(os.path.join(data_dir, 'radiation_5_a_side.csv'), 'w+')
    # outid.write(header4)
    # outid.close()
    # np.savetxt(os.path.join(data_dir, 'radiation_5_a_side.csv'), data4, delimiter=',', ...)

if __name__ == "__main__":
    # Dummy data directory
    data_directory = '../../Verification/Radiation/' 
    radiativefluxes(data_directory)

