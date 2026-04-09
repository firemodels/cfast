
import pandas as pd
import numpy as np
import os

# Define data_dir with a dummy name at the start
data_dir = '../../Verification/Fires/'

# the script scales the fire igintion columns in the Ignition_test
# verification case to match the HRR, flux, and temperature data for
# v&v plotting

# first the normal output file with heat release rates and ignition times
filename = data_dir + 'Ignition_Test_compartments.csv'
df_n = pd.read_csv(filename, header=0, skiprows=range(1, 4))
Fn = df_n.columns.tolist()
Qn = df_n.values
    
# Find the locations of each of the data columns
# strncmpi is case-insensitive prefix comparison
colT = [i for i, s in enumerate(Fn) if str(s)[:4].lower() == 'time'.lower()]
colign1 = [i for i, s in enumerate(Fn) if str(s)[:5].lower() == 'ign_2'.lower()]
colhrr1 = [i for i, s in enumerate(Fn) if str(s)[:5].lower() == 'hrr_2'.lower()]
colign2 = [i for i, s in enumerate(Fn) if str(s)[:5].lower() == 'ign_3'.lower()]
colhrr2 = [i for i, s in enumerate(Fn) if str(s)[:5].lower() == 'hrr_3'.lower()]

# then the target output file with heat flux and temperature for
# targets associated with each fire ignition
filename = data_dir + 'Ignition_Test_devices.csv'
# Replace dvcread with pandas read_csv where the filename is the file to read, the second arg is the header row, and the third arg is the row where the data starts
# [Ft Qt] = dvcread(filename,1,5);
df_t = pd.read_csv(filename, header=0, skiprows=range(1, 4))
Ft = df_t.columns.tolist()
Qt = df_t.values
    
# Find the locations of each of the data columns
colflx1 = [i for i, s in enumerate(Ft) if str(s)[:9].lower() == 'trgflxi_1'.lower()]
coltemp2 = [i for i, s in enumerate(Ft) if str(s)[:9].lower() == 'trgsurt_2'.lower()]

# Transfer the data from the csv file to this matlab script
time = Qn[:, colT]
ign1 = Qn[:, colign1]
hrr1 = Qn[:, colhrr1]
ign2 = Qn[:, colign2]
hrr2 = Qn[:, colhrr2]
flx1 = Qt[:, colflx1]
temp2 = Qt[:, coltemp2]

# calculate scaled ignition indicators
# Note: np.max() is used to replicate Matlab's max() on these vectors
scalehrr1 = ign1 * np.max(hrr1) * 2
scalehrr2 = ign2 * np.max(hrr2) * 2
scaleflx1 = ign1 * np.max(flx1) * 2
scaletemp2 = ign2 * np.max(temp2) * 2

# output data to file for plotting
header = 'Time , HRR_1 , HRR_2, FLUX_1, TEMP_2, IGNHRR_1, IGNHRR_2, IGNFLUX_1, IGNTEMP_2'
# Horizontal concatenation of vectors into a single matrix
data = np.column_stack((time, hrr1, hrr2, flx1, temp2, scalehrr1, scalehrr2, scaleflx1, scaletemp2))

out_filename = data_dir + 'fire_ignition.csv'
# outid = fopen([data_dir 'fire_ignition.csv'] , 'w+');
# fprintf(outid,'%s',header);
# fclose(outid);
with open(out_filename, 'w') as outid:
    outid.write(header)

# dlmwrite ([data_dir 'fire_ignition.csv'],data,'roffset',1,'-append');
# roffset of 1 in dlmwrite with -append appends data after one blank row
with open(out_filename, 'a') as outid:
    outid.write('\n\n')  # First newline ends the header, second newline creates the blank row offset
    np.savetxt(outid, data, delimiter=',', fmt='%.18e')

