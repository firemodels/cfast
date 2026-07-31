
import numpy as np
import pandas as pd
from scipy.interpolate import interp1d
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
import os

# Dummy argument at the top
data_dir = '../../Verification/Sprinkler/'

# This function calculates expected sprinkler temperatures and HRR values 
# once the sprinkler activates.

# The local temperature and velocity calculated by CFAST (test case 'sprinkler_1.in') 
# are read by MATLAB and then the expected results are calculated for
# the sprinkler link temperature and the heat release rate of the fire.
# These results are saved to the file sprinkler_1.csv for use in
# verification ploting script.

# Define the conditions
tempInit = 40  # C
uw = 0.07  # mm/s
tau = 3 * (uw**-1.8)
RTI = 100  # ms^0.5

# Read in the _w.csv file for the case
filename = os.path.join(data_dir, 'sprinkler_1')
filenamew = filename + '_devices.csv'

# Read CFAST spreadsheet output by column labels. Rows 2-4 contain
# long names, object IDs, and units.
devices = pd.read_csv(filenamew, header=0, skiprows=range(1, 4))

# find the locations for all of the variable columns
# strncmpi(W, ..., N) matches the first N characters case-insensitively
colTime = [s for s in devices.columns if str(s)[:4].lower() == 'time'][0]
colv = [s for s in devices.columns if str(s)[:12].lower() == 'sensgasv_1'][0]
colTg = [s for s in devices.columns if str(s)[:10].lower() == 'sensgast_1'][0]
colTL = [s for s in devices.columns if str(s)[:7].lower() == 'senst_1'][0]

v_vec = devices[colv].to_numpy()
Tg_vec = devices[colTg].to_numpy()
TL_vec = devices[colTL].to_numpy()
t_vec = devices[colTime].to_numpy()

# Solve for the TL temperatures
v_interp = interp1d(t_vec, v_vec, fill_value="extrapolate")
Tg_interp = interp1d(t_vec, Tg_vec, fill_value="extrapolate")

def f(t, TL):
    # Matlab: f = @(t,TL) (sqrt(v(t))/RTI)*(Tg(t)-TL);
    # solve_ivp expects f(t, y)
    return (np.sqrt(v_interp(t)) / RTI) * (Tg_interp(t) - TL)

# ode45(f, t_vec, 20) in Matlab uses t_vec as the points to return.
# Initial condition is 20.
sol = solve_ivp(f, [t_vec[0], t_vec[-1]], [20], t_eval=t_vec, method='RK45')
TL = sol.y[0]

# The following section is used to reduce the number of datapoints located in
# the expected results vector
timeEX = np.copy(t_vec)
TLex = np.copy(TL)

for i in range(1, len(timeEX)):
    if timeEX[i] % 250 != 0:
        TLex[i] = 0
        timeEX[i] = 0

# Remove elements where TLex is 0 (mimicking timeEX(TLex == 0) = [];)
timeEX = timeEX[TLex != 0]
TLex = TLex[TLex != 0]

# Plot the expected & CFAST results against one another
plt.figure()
plt.plot(timeEX, TLex, 'ro', label='theoretical')
plt.plot(t_vec, TL_vec, 'b', label='CFAST')
plt.xlabel('Time (s)')
plt.ylabel('Link Temperature')
plt.legend(loc='lower right') # SouthEast equivalent
# plt.show() # Optional: uncomment to see plot

##
# Read in the n.csv file for the case
filenameZone = filename + '_compartments.csv'

compartments = pd.read_csv(filenameZone, header=0, skiprows=range(1, 4))

# Find the locations for all of the variables and store the columns
colQ = [s for s in compartments.columns if str(s)[:5].lower() == 'hrr_1'][0]
hrr = compartments[colQ].to_numpy()

# The expected heat release rate is being calculated
# tactLoc = find(floor(TL) == tempInit); tactLoc = tactLoc(1);
tactLoc = np.where(np.floor(TL) == tempInit)[0][0]
tact = t_vec[tactLoc]
Qtact = hrr[tactLoc]
timeSprink = t_vec[tactLoc+1:]
hrrSprink = Qtact * np.exp(-1/tau * (timeSprink - tact))
hrrCFAST = hrr[tactLoc+1:]

# The following section is used to reduce the number of datapoints located in
# the expected results vector
timeshort = np.copy(timeSprink)
for i in range(1, len(timeshort)):
    if timeshort[i] % 250 != 0:
        hrrSprink[i] = 0
        timeshort[i] = 0

# Remove elements where hrrSprink is 0
timeshort = timeshort[hrrSprink != 0]
hrrSprink = hrrSprink[hrrSprink != 0]

# Plot the expected & CFAST results against one another
plt.figure()
plt.plot(timeshort, hrrSprink, 'ro', label='theoretical')
plt.plot(timeSprink, hrrCFAST, 'b', label='CFAST')
plt.xlabel('Time (s)')
plt.ylabel('HRR (W)')
plt.legend()
# plt.show() # Optional: uncomment to see plot

header = 'TimeLinkEx , TempLinkEx , TimeHRREx , HRREx'

# Note: In the original Matlab, concatenation [timeEX, TLex, timeshort, hrrSprink] 
# only works if the vectors have the same length. If they differ, Matlab throws an error.
# In CFAST validation scripts, these columns often differ in length. 
# We use pandas to align them into a single structure, padding with NaN if necessary.
data_dict = {
    'TimeLinkEx': timeEX,
    'TempLinkEx': TLex,
    'TimeHRREx': timeshort,
    'HRREx': hrrSprink
}
df_out = pd.DataFrame(dict([(k, pd.Series(v)) for k, v in data_dict.items()]))

# Write to file
output_path = os.path.join(data_dir, 'sprinkler_1.csv')
with open(output_path, 'w') as f:
    f.write(header + '\n')
    # dlmwrite with roffset 1 appends below the header
    df_out.to_csv(f, header=False, index=False)
