import numpy as np
import pandas as pd
from scipy.interpolate import interp1d
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
import os

t= np.array([50.,58.])

# Dummy argument at the top
data_dir = '../../Verification/Geometry/'

# Read in the _compartments.csv file for the case
filename = os.path.join(data_dir, 'Volume_Check_compartments.csv')

# Read CFAST spreadsheet output by column labels. Rows 2-4 contain
# long names, object IDs, and units.
data = pd.read_csv(filename, header=0, skiprows=range(1, 4))

lh1 = data['HGT_1'].iloc[-1]
lh2 = data['HGT_2'].iloc[-1]
lh3 = data['HGT_3'].iloc[-1]

# comp 1  9 m^2 to 0.5 m then 4 m^2
if (lh1 <= 0.5):
   vol1 = (0.5 - lh1) * 9. + 2.5 * 4.
else:
   vol1 = (3.- lh1) * 4.

v1 = np.array([vol1,vol1])

# comp 2 is 9 m^2 to 0.5 m then taper to 4 m^2 at 2.5 m
vol2 =  (3. - max(2.5,lh2))*4
if (lh2 <= 2.5):
   z = max(0.5,lh2)
   dz = z - 0.5
   sidel = 3. + (2.-3.)*dz/2.
   volful = 2./3. * (9. + 4. +3. * 2.)
   vol = volful - dz / 3. * (9. + sidel**2 + 3*sidel)
   vol2 = vol2 + vol

if (lh2 <=0.5):
   vol2 = vol2 + (0.5 - lh2) * 9.

v2 = np.array([vol2,vol2])

# comp 3 is 4 m^2 to 0.5 m then increase to 9 m^2 at 2.5 m
vol3 =  (3. - max(2.5,lh3))*9.
if (lh3 <= 2.5):
   z = max(0.5,lh3)
   dz = 2.5- z
   sidel = 3. + (2.-3.)*dz/2.
   vol3 = vol3 +  dz / 3. * (9. + sidel**2 + 3*sidel)

if (lh3 <=0.5):
   vol3 = vol3 + (0.5 - lh3) * 4.

v3 = np.array([vol3,vol3])

# Write file for plotting

df = pd.DataFrame({"Time":t,"VOL_1":v1,"VOL_2":v2,"VOL_3":v3})


outfile = os.path.join(data_dir, 'Volume_Check.csv')

df.to_csv(outfile, header=True, index=False)