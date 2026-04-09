
import pandas as pd
import numpy as np
import os

import pandas as pd
import numpy as np

def csvreaderALL(filename, Comp):
    # [H D] = dvcread(filename,1,5);
    # As per instructions: replace dvcread with pandas read_csv.
    # First argument is the file, second is header row (1 in Matlab = 0 in Python), 
    # third is row where data starts (5 in Matlab = index 4 in Python).
    # We read row 0 as header and skip rows 1, 2, and 3 to start data at row index 4.
    df = pd.read_csv(filename, header=0, skiprows=range(1, 4))
    H = df.columns.tolist()
    D = df.values

    colTime = [i for i, h in enumerate(H) if str(h)[:4].lower() == 'Time'.lower()[:4].lower()]
    patternO2u = 'ULMO2' + str(Comp)
    colO2u = [i for i, h in enumerate(H) if str(h)[:7].lower() == patternO2u[:7].lower()]
    patternCO2u = 'ULMCO2' + str(Comp)
    colCO2u = [i for i, h in enumerate(H) if str(h)[:8].lower() == patternCO2u[:8].lower()]
    patternH2Ou = 'ULMH2O' + str(Comp)
    colH2Ou = [i for i, h in enumerate(H) if str(h)[:8].lower() == patternH2Ou[:8].lower()]
    patternO2l = 'LLMO2' + str(Comp)
    colO2l = [i for i, h in enumerate(H) if str(h)[:7].lower() == patternO2l[:7].lower()]
    patternCO2l = 'LLMCO2' + str(Comp)
    colCO2l = [i for i, h in enumerate(H) if str(h)[:8].lower() == patternCO2l[:8].lower()]
    patternH2Ol = 'LLMH2O' + str(Comp)
    colH2Ol = [i for i, h in enumerate(H) if str(h)[:8].lower() == patternH2Ol[:8].lower()]

    Time = D[:, colTime] / 60
    CO2u = D[:, colCO2u]
    H2Ou = D[:, colH2Ou]
    CO2l = D[:, colCO2l]
    H2Ol = D[:, colH2Ol]

    mCO2 = CO2u.copy()
    mH2O = H2Ou.copy()
    
    if CO2u.shape == CO2l.shape:
        mCO2 = CO2u + CO2l
        mH2O = H2Ou + H2Ol
    
    return Time, mCO2, mH2O

# Dummy argument for data_dir
data_dir = '../../Verification/Mass_Balance/'

# This script will calculate the experimental and theoretical yields of H2O
# and CO2 for several test cases. The program functions by reading in
# values from the specific .csv files. Experimental values are calculated by
# adding upper and lower layer values of calculated layer species masses

# format long -> In Python, floating point precision is high by default.

# Constants
numC = 1
numH = 4
numO = 0
HeatOfComb = 50000
MC = 12.01  # g/mol
MH = 1.008  # g/mol
MO = 16.00  # g/mol
MCO2 = 44.01  # g/mol
MH2O = 18.016  # g/mol

# Molceular weight of the fuel source
MWfuel = numC*MC + numH*MH + numO*MO
# Stoichiometric Coefficients of H2O and CO2 production in the reaction
StoicH2O = numH/2
StoicCO2 = numC

# Theoretical Calculations
# at t = 0, 0 fuel has been consumed so 0 kg of CO2 and H2O will be produced
t = np.arange(0, 391, 30)
t = np.append(t, [510, 630, 750, 870])
# Transpose logic is inherent in how we treat 1D arrays in Python/Numpy
t = t * (1/60)

TotEnergyCons = np.array([0, 15, 45, 75, 105, 135, 165, 195, 225, 255, 285, 315, 330, 330, 330, 330, 330, 330])
MassFuelCons = np.zeros(len(t))
MassC = np.zeros(len(t))
MassH = np.zeros(len(t))
MassH2OProd = np.zeros(len(t))
MassCO2Prod = np.zeros(len(t))

for i in range(len(t)):
    MassFuelCons[i] = TotEnergyCons[i] / HeatOfComb  # Solves for total mass of fuel that was consumed in the process
    MassC[i] = ((numC * MC) / MWfuel) * MassFuelCons[i]  # kg
    MassH[i] = ((numH * MH) / MWfuel) * MassFuelCons[i]  # kg
    # Solve explicitly for the theoretical masses of H2O and CO2
    # Handling division by zero for the initial state where MassH and MassC are 0
    if MassH[i] == 0:
        MassH2OProd[i] = 0.0
    else:
        MassH2OProd[i] = 1 / (((numH * MH) / (StoicH2O * MH2O)) / MassH[i])  # kg
    
    if MassC[i] == 0:
        MassCO2Prod[i] = 0.0
    else:
        MassCO2Prod[i] = 1 / (((numC * MC) / (StoicCO2 * MCO2)) / MassC[i])  # kg

header = 't , MassCO2Prod , MassH2OProd'
data = np.column_stack((t, MassCO2Prod, MassH2OProd))
outid_path = os.path.join(data_dir, 'species_mass.csv')
with open(outid_path, 'w') as outid:
    outid.write(header + '\n')

df_out = pd.DataFrame(data)
df_out.to_csv(outid_path, mode='a', index=False, header=False)

## Case 1: Single compartment. There is a fire in the center of the compartment consistent with the above theoretical calculation
filename = os.path.join(data_dir, 'species_mass_1_masses.csv')
# Implementation requires the file to exist; assuming csvreaderALL is available
Time, mCO2, mH2O = csvreaderALL(filename, '_1')

header = 'Time , mCO2 , mH2O'
data = np.column_stack((Time, mCO2, mH2O))
outid_path = os.path.join(data_dir, 'species_mass_1.csv')
with open(outid_path, 'w') as outid:
    outid.write(header + '\n')

df_case1 = pd.DataFrame(data)
df_case1.to_csv(outid_path, mode='a', index=False, header=False)

## Case 2: Two compartments of sizes 2m*5m*8m are placed side by side, with a door of height 6m and width 1m connecting them. There is a fire in the middle of the first compartment that has the same energy production as the fire in the first case study. The fuel is still methane.

# Theoretical Mass calculations will be exactly the same as above. Same fire
# and same fuel source, so the mass of CO2 and H2O produced should
# theoretically still be the same.
filename = os.path.join(data_dir, 'species_mass_2_masses.csv')
Time, mCO21, mH2O1 = csvreaderALL(filename, '_1')
Time, mCO22, mH2O2 = csvreaderALL(filename, '_2')
mCO2 = mCO21 + mCO22
mH2O = mH2O1 + mH2O2

header = 'Time , mCO2 , mH2O'
data = np.column_stack((Time, mCO2, mH2O))
outid_path = os.path.join(data_dir, 'species_mass_2.csv')
with open(outid_path, 'w') as outid:
    outid.write(header + '\n')

df_case2 = pd.DataFrame(data)
df_case2.to_csv(outid_path, mode='a', index=False, header=False)

## Case 3: Two compartments of sizes 9m*5m*4m and 9m*5m*2m are placed directly on top of each other with a 4m^2 ceiling vent in between the two. The same fire that is described in cases 1&2 is placed in the middle of compartment 1. 

# Theoretical Mass calculations will be exactly the same as above. Same fire
# and same fuel source, so the mass of CO2 and H2O produced should
# theoretically still be the same.
filename = os.path.join(data_dir, 'species_mass_3_masses.csv')
Time, mCO21, mH2O1 = csvreaderALL(filename, '_1')
Time, mCO22, mH2O2 = csvreaderALL(filename, '_2')
mCO2 = mCO21 + mCO22
mH2O = mH2O1 + mH2O2

header = 'Time , mCO2 , mH2O'
data = np.column_stack((Time, mCO2, mH2O))
outid_path = os.path.join(data_dir, 'species_mass_3.csv')
with open(outid_path, 'w') as outid:
    outid.write(header + '\n')

df_case3 = pd.DataFrame(data)
df_case3.to_csv(outid_path, mode='a', index=False, header=False)

## Case 4:
filename = os.path.join(data_dir, 'species_mass_4_masses.csv')
Time, mCO21, mH2O1 = csvreaderALL(filename, '_1')
Time, mCO22, mH2O2 = csvreaderALL(filename, '_2')
Time, mCO23, mH2O3 = csvreaderALL(filename, '_3')
Time, mCO24, mH2O4 = csvreaderALL(filename, '_4')

mCO2 = mCO21 + mCO22 + mCO23 + mCO24
mH2O = mH2O1 + mH2O2 + mH2O3 + mH2O4

header = 'Time , mCO2 , mH2O , mCO21 , mH2O1 , mCO22 , mH2O2 , mCO23 , mH2O3 , mCO24 , mH2O4'
data = np.column_stack((Time, mCO2, mH2O, mCO21, mH2O1, mCO22, mH2O2, mCO23, mH2O3, mCO24, mH2O4))
outid_path = os.path.join(data_dir, 'species_mass_4.csv')
with open(outid_path, 'w') as outid:
    outid.write(header + '\n')

df_case4 = pd.DataFrame(data)
df_case4.to_csv(outid_path, mode='a', index=False, header=False)

