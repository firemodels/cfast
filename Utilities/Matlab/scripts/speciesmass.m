function [] = speciesmass(data_dir)

%This script will calculate the experimental and theoretical yields of H2O
%and CO2 for several test cases. The program functions by reading in
%values from the specific .csv files. Experimental values are calculated by
%adding upper and lower layer values of calculated layer species masses

format long

%Constants
numC = 1;
numH = 4;
numO = 0;
HeatOfComb = 50000;
MC = 12.01;%g/mol
MH = 1.008;%g/mol
MO = 16.00;%g/mol
MCO2 = 44.01;%g/mol
MH2O = 18.016;%g/mol

%Molceular weight of the fuel source
MWfuel = numC*MC + numH*MH + numO*MO;
%Stoichiometric Coefficients of H2O and CO2 production in the reaction
StoicH2O = numH/2;
StoicCO2 = numC;

%Theoretical Calculations
%at t = 0, 0 fuel has been consumed so 0 kg of CO2 and H2O will be produced
t = [0:30:390];
t = [t 510 630 750 870]';
t = t*(1/60);
TotEnergyCons = [0 15 45 75 105 135 165 195 225 255 285 315 330 330 330 330 330 330]';
MassFuelCons = zeros(length(t),1);
MassC = zeros(length(t),1);
MassH = zeros(length(t),1);
MassH2OProd = zeros(length(t),1);
MassCO2Prod = zeros(length(t),1);
for i = 1:length(t)
    MassFuelCons(i) = TotEnergyCons(i)/HeatOfComb;%Solves for total mass of fuel that was consumed in the process
    MassC(i) = ((numC*MC)/MWfuel)*MassFuelCons(i);%kg
    MassH(i) = ((numH*MH)/MWfuel)*MassFuelCons(i);%kg
    %Solve explicitly for the theoretical masses of H2O and CO2
    MassH2OProd(i) = 1/(((numH*MH)/(StoicH2O*MH2O))/MassH(i));%kg
    MassCO2Prod(i) = 1/(((numC*MC)/(StoicCO2*MCO2))/MassC(i));%kg
end

header = ['t , MassCO2Prod , MassH2OProd'];
data = [t , MassCO2Prod , MassH2OProd];
outid = fopen([data_dir 'species_mass.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass.csv'],data,'roffset',1,'-append');

%% Case 1: Single compartment. There is a fire in the center of the compartment consistent with the above theoretical calculation
filename = [data_dir 'species_mass_1_m.csv'];
[Time,mCO2,mH2O] = csvreaderALL(filename,'_1');

header = ['Time , mCO2 , mH2O'];
data = [Time , mCO2, mH2O];
outid = fopen([data_dir 'species_mass_1.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_1.csv'],data,'roffset',1,'-append');

%% Case 2: Two compartments of sizes 2m*5m*8m are placed side by side, with a door of height 6m and width 1m connecting them. There is a fire in the middle of the first compartment that has the same energy production as the fire in the first case study. The fuel is still methane.

%Theoretical Mass calculations will be exactly the same as above. Same fire
%and same fuel source, so the mass of CO2 and H2O produced should
%theoretically still be the same.
filename = [data_dir 'species_mass_2_m.csv'];
[Time,mCO21,mH2O1] = csvreaderALL(filename,'_1');
[Time,mCO22,mH2O2] = csvreaderALL(filename,'_2');
mCO2 = mCO21 + mCO22;
mH2O = mH2O1 + mH2O2;

header = ['Time , mCO2 , mH2O'];
data = [Time , mCO2, mH2O];
outid = fopen([data_dir 'species_mass_2.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_2.csv'],data,'roffset',1,'-append');

%% Case 3: Two compartments of sizes 9m*5m*4m and 9m*5m*2m are placed directly on top of each other with a 4m^2 ceiling vent in between the two. The same fire that is described in cases 1&2 is placed in the middle of compartment 1. 

%Theoretical Mass calculations will be exactly the same as above. Same fire
%and same fuel source, so the mass of CO2 and H2O produced should
%theoretically still be the same.
filename = [data_dir 'species_mass_3_m.csv'];
[Time,mCO21,mH2O1] = csvreaderALL(filename,'_1');
[Time,mCO22,mH2O2] = csvreaderALL(filename,'_2');
mCO2 = mCO21 + mCO22;
mH2O = mH2O1 + mH2O2;

header = ['Time , mCO2 , mH2O'];
data = [Time , mCO2, mH2O];
outid = fopen([data_dir 'species_mass_3.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_3.csv'],data,'roffset',1,'-append');

%% Case 4:
filename = [data_dir 'species_mass_4_m.csv'];
[Time,mCO21,mH2O1] = csvreaderALL(filename,'_1');
[Time,mCO22,mH2O2] = csvreaderALL(filename,'_2');
[Time,mCO23,mH2O3] = csvreaderALL(filename,'_3');
[Time,mCO24,mH2O4] = csvreaderALL(filename,'_4');

mCO2 = mCO21 + mCO22 + mCO23 + mCO24;
mH2O = mH2O1 + mH2O2 + mH2O3 + mH2O4;

header = ['Time , mCO2 , mH2O , mCO21 , mH2O1 , mCO22 , mH2O2 , mCO23 , mH2O3 , mCO24 , mH2O4'];
data = [Time , mCO2, mH2O, mCO21, mH2O1 , mCO22 , mH2O2 , mCO23 , mH2O3 , mCO24 , mH2O4 ];
outid = fopen([data_dir 'species_mass_4.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_4.csv'],data,'roffset',1,'-append');
%% Case 5: radiation_3.in
filename = ['../../Verification/Radiation/radiation_3_w.csv'];
X1 = [0.0909,0.2727,0.4545,0.6364,0.8182,1.0000,1.1818,1.3636,1.5455,1.7273,1.9091]';
X2 = [0.125,0.375,0.625,0.875,1.125,1.375,1.625,1.875,2.125,2.375,2.625,2.875,3.125,3.375,3.625,3.875]';
Z = importdata(filename,',',5);
W = strsplit(Z.textdata{1,1},',');

%find the locations for all of the variable columns
col = find(strncmpi(W,'TRGFLXG',7))';
cfast_targ_flux_gas = Z.data(:,col);
cfast_targ_top_gas  = cfast_targ_flux_gas(1:11)';
cfast_targ_side_gas = cfast_targ_flux_gas(12:27)';
col = find(strncmpi(W,'TRGFLXS',7))';
cfast_targ_flux_wall = Z.data(:,col);
cfast_targ_top_wall  = cfast_targ_flux_wall(1:11)';
cfast_targ_side_wall = cfast_targ_flux_wall(12:27)';

header1 = ['X, GAS(TOP), WALL(TOP)'];
header2 = ['Z, GAS(SIDE), WALL(SIDE)'];
data1 = [X1, cfast_targ_top_gas, cfast_targ_top_wall];
data2 = [X2, cfast_targ_side_gas, cfast_targ_side_wall];

outid = fopen(['../../Verification/Radiation/radiation_3_w_top.csv'] , 'w+');
fprintf(outid,'%s',header1);
fclose(outid);
dlmwrite (['../../Verification/Radiation/radiation_3_w_top.csv'],data1,'roffset',1,'-append');

outid = fopen(['../../Verification/Radiation/radiation_3_w_side.csv'] , 'w+');
fprintf(outid,'%s',header2);
fclose(outid);
dlmwrite (['../../Verification/Radiation/radiation_3_w_side.csv'],data2,'roffset',1,'-append');
