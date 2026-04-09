
import os
import numpy as np

# This script will do a transpose of a matrix for the results obtained 
# from CFAST verification cases: target_2.in
#
# Fire heat flux as a function of location is desired. Yet in CFAST,
# the calculated results for each target (for different locations) for a
# specific time step are presented in a single row. With the fact that the   
# current Matlab script to handle verification and validation take a single 
# column of values. Thus, a transpose of matrix is needed.

data_dir = '../../Verification/Target/' 

filename = os.path.join(data_dir, 'target_2_devices.csv')
X1 = np.array([[8, 6, 5, 4.5, 4.4, 4.3, 4.2, 4.1]]).T

# This imports numeric data starting from line 6 and stores the first 5 lines as text
with open(filename, 'r') as f:
    all_lines = f.readlines()

# Extract text data (first 5 lines)
textdata = all_lines[:5]

# Extract numeric data (starting from line 6)
try:
    numeric_data = np.genfromtxt(filename, delimiter=',', skip_header=5)
except Exception:
    numeric_data = np.array([])

# W = strsplit(Z.textdata{1,1},',');
# Extract the first line and split it by commas
if len(textdata) > 0:
    W = textdata[0].strip().split(',')
else:
    W = []

# col = find(strncmpi(W,'TRGFLXF',7))';
# strncmpi is a case-insensitive comparison of the first 7 characters
col_indices = [i for i, s in enumerate(W) if s[:7].lower() == 'trgflxf'.lower()]

# cfast_targ_flux_first = Z.data(:,col);
# If there is only one row of data, genfromtxt may return a 1D array.
# We ensure it is treated as a 2D matrix for consistent indexing.
if numeric_data.ndim == 1:
    cfast_targ_flux_first = numeric_data.reshape(1, -1)[:, col_indices]
else:
    cfast_targ_flux_first = numeric_data[:, col_indices]

# cfast_targ_flux_fire = cfast_targ_flux_first(1:8)';
# In Matlab, linear indexing (1:8) on a matrix returns a column vector of the first 8 
# elements in column-major order. The single quote (') then transposes it.
# However, as established in the script's logic for horizontal concatenation, 
# for [X1, cfast_targ_flux_fire] to work where X1 is 8x1, cfast_targ_flux_fire 
# must also be 8x1.
cfast_targ_flux_fire = cfast_targ_flux_first.flatten('F')[:8].reshape(-1, 1)

header1 = 'Z, FIRE_HEAT_FLUX'
# data1 = [X1, cfast_targ_flux_fire];
data1 = np.hstack((X1, cfast_targ_flux_fire))

# outid = fopen([data_dir 'target_2_w2.csv'] , 'w+');
# fprintf(outid,'%s',header1);
# fclose(outid);
output_filename = os.path.join(data_dir, 'target_2_w2.csv')
with open(output_filename, 'w') as outid:
    outid.write(header1)

# dlmwrite ([data_dir 'target_2_w2.csv'],data1,'roffset',1,'-append');
# 'roffset', 1 inserts one blank row before the data when appending.
# Since the header was written without a newline, we add two newlines:
# one to finish the header line and one to create the blank offset row.
with open(output_filename, 'a') as outid:
    outid.write('\n\n')
    np.savetxt(outid, data1, delimiter=',', fmt='%g')


