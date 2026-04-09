
# Reads the CFAST _w.csv file and writes output in a form appropriate for dataplot.py

import pandas as pd
import numpy as np
import os

outdir = '../../Validation/NIST_NRC_Corner_Effects/'

casename = [None] * 18
casename[0]  = 'corner_200_kW'
casename[1]  = 'corner_300_kW'
casename[2]  = 'corner_400_kW'
casename[3]  = 'wall_200_kW'
casename[4]  = 'wall_300_kW'
casename[5]  = 'wall_400_kW'
casename[6]  = 'cabinet_01'
casename[7]  = 'cabinet_02'
casename[8]  = 'cabinet_03'
casename[9]  = 'cabinet_04'
casename[10] = 'cabinet_05'
casename[11] = 'cabinet_06'
casename[12] = 'cabinet_07'
casename[13] = 'cabinet_08'
casename[14] = 'cabinet_09'
casename[15] = 'cabinet_10'
casename[16] = 'cabinet_11'
casename[17] = 'cabinet_12'

for j in range(1, 19):
    try:
        # Construct file path
        filepath = os.path.join(outdir, casename[j-1] + '_devices.csv')
        
        # In Matlab, importdata(..., 5) reads data skipping 5 header lines
        # textdata contains the header lines. W is split from the first line.
        with open(filepath, 'r') as f:
            all_lines = f.readlines()
        
        M_textdata = all_lines[:5]
        # Equivalent to W = strsplit(M.textdata{1,1},',')
        W = M_textdata[0].strip().split(',')
        
        # Equivalent to M = importdata(..., 5) numeric part
        # Using pandas read_csv to skip 5 lines and treat as numeric data
        M_data = pd.read_csv(filepath, skiprows=5, header=None).values
        
        # col = find(strncmpi(W,'TRGGAST',7))'
        # find indices for columns starting with 'TRGGAST' (case-insensitive, first 7 chars)
        col = [i for i, name in enumerate(W) if name[:7].lower() == 'trggast']
        
        # t = M.data(:,col);
        t = M_data[:, col]

        H = [
            ['s', 'C', 'C', 'C'],
            ['Time', 'Lower', 'Middle', 'Upper']
        ]

        # fid = fopen([outdir,casename{j},'_Plume.csv'],'wt','n');
        output_filepath = os.path.join(outdir, casename[j-1] + '_Plume.csv')
        with open(output_filepath, 'w') as fid:
            # fprintf(fid,'%s,%s,%s,%s\n',H{1,:});
            fid.write(f"{H[0][0]},{H[0][1]},{H[0][2]},{H[0][3]}\n")
            # fprintf(fid,'%s,%s,%s,%s\n',H{2,:});
            fid.write(f"{H[1][0]},{H[1][1]},{H[1][2]},{H[1][3]}\n")
            
            n_times = t.shape[0]
            for i in range(n_times):
                # highvals  = [t(i,1) t(i,4) t(i,7) t(i,10) t(i,13) t(i,16) t(i,19)];
                # Matlab indices 1, 4, 7... are 0, 3, 6... in Python
                high_idx = [0, 3, 6, 9, 12, 15, 18]
                highvals = t[i, high_idx]
                high = np.max(highvals)
                
                # midvals  = [t(i,2) t(i,5) t(i,8) t(i,11) t(i,14) t(i,17) t(i,20)];
                # Matlab indices 2, 5, 8... are 1, 4, 7... in Python
                mid_idx = [1, 4, 7, 10, 13, 16, 19]
                midvals = t[i, mid_idx]
                mid = np.max(midvals)
                
                # lowvals = [t(i,3) t(i,6) t(i,9) t(i,12) t(i,15) t(i,18) t(i,21)];
                # Matlab indices 3, 6, 9... are 2, 5, 8... in Python
                low_idx = [2, 5, 8, 11, 14, 17, 20]
                lowvals = t[i, low_idx]
                low = np.max(lowvals)
                
                # fprintf(fid,'%4.0f,%5.1f,%5.1f,%5.1f\n',M.data(i,1),low,mid,high);
                # M.data(i,1) is the first column of the numeric matrix
                fid.write(f"{M_data[i, 0]:4.0f},{low:5.1f},{mid:5.1f},{high:5.1f}\n")
        
        # Matlab clear statements are implicitly handled by scope in Python
        del M_data, W, col, t
        
    except Exception:
        print(f"Warning: Problem with NIST/NRC Corner Test {j}. File not found. Skipping case.")
        continue

