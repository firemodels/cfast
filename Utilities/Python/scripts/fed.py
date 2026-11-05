import numpy as np
import pandas as pd
import math
import os

t= np.array([50.,58.])

# Dummy argument at the top
data_dir = '../../Verification/Target/'

# Read in the _compartments.csv file for the case
filename = os.path.join(data_dir, 'fed_compartments.csv')

# Read CFAST spreadsheet output by column labels. Rows 2-4 contain
# long names, object IDs, and units.
compdata = pd.read_csv(filename, header=0, skiprows=range(1, 4))

# Read in the _targets.csv file for the case
filename = os.path.join(data_dir, 'fed_devices.csv')

# Read CFAST spreadsheet output by column labels. Rows 2-4 contain
# long names, object IDs, and units.
devcdata = pd.read_csv(filename, header=0, skiprows=range(1, 4))

times = compdata['Time']
height = compdata['HGT_1']
target1T=devcdata['TRGGAST_1']
target2T=devcdata['TRGGAST_2']

target1Q=devcdata['TRGFLXI_1']
target2Q=devcdata['TRGFLXI_2']

co2u = compdata['ULCO2_1']
cou = compdata['ULCO_1']
hcnu = compdata['ULHCN_1']
co2l = compdata['LLCO2_1']
col = compdata['LLCO_1']
hcnl = compdata['LLHCN_1']

fedgas = np.zeros(len(times))
fedh1 = np.zeros(len(times))
fedh2 = np.zeros(len(times))

for i in range(len(times)-1):
   if height[i+1] < 2.5:
      co22 = math.exp(co2u[i+1]*100./5.)
      co2 = cou[i+1]*1.E6*co22
      hcn2 = hcnu[i+1]*1.E6*co22
   else:
      co22 = math.exp(co2l[i+1]*100./5.)
      co2 = col[i+1]*1.E6*co22
      hcn2 = hcnl[i+1]*1.E6*co22
   if height[i] < 2.5:
      co21 = math.exp(co2u[i]*100./5.)
      co1 = cou[i]*1.E6*co21
      hcn1 = hcnu[i]*1.E6*co21
   else:
      co21 = math.exp(co2l[i]*100./5.)
      co1 = col[i]*1.E6*co21
      hcn1 = hcnl[i]*1.E6*co21
   term2 = co2/35000.+ hcn2**2.36/1.2E6
   term1 = co1/35000.+ hcn1**2.36/1.2E6
   fedgas[i+1] = fedgas[i] + (times[i+1]-times[i])/60*0.5*(term2+term1)

   term1 = 1./(4.1E8*target1T[i]**(-3.61))
   term2 = 1./(4.1E8*target1T[i+1]**(-3.61))
   if target1Q[i] >= 2.5:
      term1 = term1 + 1./(4.2*target1Q[i]**(-1.9))
   if target1Q[i+1] >= 2.5:
      term2 = term2 + 1./(4.2*target1Q[i+1]**(-1.9))
   fedh1[i+1] = fedh1[i] + (times[i+1]-times[i])/60*0.5*(term2+term1)

   term1 = 1./(4.1E8*target2T[i]**(-3.61))
   term2 = 1./(4.1E8*target2T[i+1]**(-3.61))
   if target2Q[i] >= 2.5:
      term1 = term1 + 1./(4.2*target2Q[i]**(-1.9))
   if target2Q[i+1] >= 2.5:
      term2 = term2 + 1./(4.2*target2Q[i+1]**(-1.9))
   fedh2[i+1] = fedh2[i] + (times[i+1]-times[i])/60*0.5*(term2+term1)

# Write file for plotting

df = pd.DataFrame({"Time":times,"FEDG":fedgas,"FEDH1":fedh1,"FEDH2":fedh2})

df = df.drop(df[(df["Time"] > 0) & (df["Time"] < 10)].index)
df = df.drop(df[(df["Time"] > 10) & (df["Time"] < 20)].index)
df = df.drop(df[(df["Time"] > 20) & (df["Time"] < 30)].index)
df = df.drop(df[(df["Time"] > 30) & (df["Time"] < 40)].index)
df = df.drop(df[(df["Time"] > 40) & (df["Time"] < 50)].index)
df = df.drop(df[(df["Time"] > 50) & (df["Time"] < 60)].index)

outfile = os.path.join(data_dir, 'fed.csv')

df.to_csv(outfile, header=True, index=False)

