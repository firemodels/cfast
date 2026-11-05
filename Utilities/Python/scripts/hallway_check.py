import numpy as np
import pandas as pd
import math
import os

t= np.array([50.,58.])

# Dummy argument at the top
data_dir = '../../Verification/Geometry/'

# Read in the _compartments.csv file for the case
filename = os.path.join(data_dir, 'hallway_check_compartments.csv')

# Read CFAST spreadsheet output by column labels. Rows 2-4 contain
# long names, object IDs, and units.
compdata = pd.read_csv(filename, header=0, skiprows=range(1, 4))

# Read in the _targets.csv file for the case
filename = os.path.join(data_dir, 'hallway_check_devices.csv')

# Read CFAST spreadsheet output by column labels. Rows 2-4 contain
# long names, object IDs, and units.
devcdata = pd.read_csv(filename, header=0, skiprows=range(1, 4))

q = 500
x_r = 0.35
qc = 500 * (1-x_r)
D = 2*math.sqrt(1./math.pi)
g = 9.80665
rhocons = 352.981915
cpg = 1.012
qs = q/(rhocons*cpg*math.sqrt(g)*D**2.5)
z0 = D*(-1.02+1.4*qs**0.4)
tk = 273.15

times = compdata['Time']
height = compdata['HGT_1']
ult = compdata['ULT_1']
llt = compdata['LLT_1']

Tp = np.zeros(len(times))
T2 = np.zeros(len(times))
T8A = np.zeros(len(times))
T8B = np.zeros(len(times))

for i in range(len(times)):
   z0p = height[i] - ((ult[i]+tk)/(llt[i]+tk))**0.6*(height[i]-z0)
   Tp[i] = llt[i]+9.1*((ult[i]+tk)/(g*cpg**2*(rhocons/(ult[i]+tk))**2))**(1./3.)*qc**(2./3.)*(2.5-z0p)**(-5./3.)
   T2[i] = ult[i] + (Tp[i]-ult[i])/(0.225+0.27*0.2)**(-4./3.)*(0.225+0.27*2/2.5)**(-4./3.)
   T8A[i] = ult[i] + (Tp[i]-ult[i])/(0.225+0.27*0.2)**(-4./3.)*(0.225+0.27*8/2.5)**(-4./3.)
   T8B[i] = ult[i] + (Tp[i]-ult[i])*.37*(2.5/4)**(1./3.)*math.exp(-0.16*8/2.5*(4/2.5)**(1./3.))

# Write file for plotting

df = pd.DataFrame({"Time":times,"TP":Tp,"T2AB":T2,"T8A":T8A,"T8B":T8B})

df = df.drop(df[(df["Time"] < 0.5)].index)
df = df.drop(df[(df["Time"] > 0.5) & (df["Time"] < 1.0)].index)
df = df.drop(df[(df["Time"] > 1.0) & (df["Time"] < 1.5)].index)
df = df.drop(df[(df["Time"] > 1.5) & (df["Time"] < 2)].index)

outfile = os.path.join(data_dir, 'hallway_check.csv')

df.to_csv(outfile, header=True, index=False)

