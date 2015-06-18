%This script will calculate the experimental and theoretical yields of H2O
%and CO2 for several test cases. The program functions by reading in
%values from the specific .csv files. The program finds values of
%Temperature, Pressure and Volume of the upper layer, as well as
%the corresponding mole fractions of O2, CO2 and H2O in the upper layer,
%which are then used to calculate the experimental masses of CO2 and H2O produced.

close all
clear all
format long

data_dir = '../../../Verification/Mass_Balance/';
plot_dir = '../../../Docs/Validation_Guide/FIGURES/Verification/';

numC = 1;
numH = 4;
numO = 0;
Po = 101300;
HeatOfComb = 50000;

%Important Constans
R = 8.314;%kJ/(mol*K)
MO2 = 32.00;%g/mol
MCO2 = 44.01;%g/mol
MH2O = 18.01528;%g/mol
MN2 = 28.02;%g/mol
MC = 12.01;%g/mol
MH = 1.008;%g/mol
MO = 16.00;%g/mol

%Read in the HOC,Totenergy,Po and chemical formula
MWfuel = numC*MC + numH*MH + numO*MO;%Finds the molceular weight of the fuel source
%Stoichiometric Coefficients of H2O and CO2 in the reaction
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
TheoreticalMassH2O = MassH2OProd(length(MassH2OProd));
TheoreticalMassCO2 = MassCO2Prod(length(MassCO2Prod));
%Experimental Calculations
findname = [data_dir 'species_mass_1'];
[Time,mCO2,mH2O,TotH2Omass,TotCO2mass] = csvreaderALL(findname);
%Percent Error Calculations
ErrorCO2 = 100*abs((TheoreticalMassCO2 - TotCO2mass)/(TheoreticalMassCO2));% %
ErrorH2O = 100*abs((TheoreticalMassH2O - TotH2Omass)/(TheoreticalMassH2O));% %
header = ['Time , mCO2 , mH2O'];
data = [Time , mCO2, mH2O];
outid = fopen([data_dir 'species_mass_1.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_1.csv'],data,'roffset',1,'-append');

%% Case 2: Two compartments of sizes 2m*5m*8m are placed side by side, with a door of height 6m and width 1m connecting them. There is a fire in the middle of the first compartment that has the same energy production as the fire in the first case study. The fuel is still methane.
findname = [data_dir 'species_mass_2'];
%Theoretical Mass calculations will be exactly the same as above. Same fire
%and same fuel source, so the mass of CO2 and H2O produced should
%theoretically still be the same.
[Time,mCO2,mH2O,TotH2Omass,TotCO2mass] = csvreaderALL(findname);
%Percent Error Calculations
ErrorCO2 = 100*abs((TheoreticalMassCO2 - TotCO2mass)/(TheoreticalMassCO2));% %
ErrorH2O = 100*abs((TheoreticalMassH2O - TotH2Omass)/(TheoreticalMassH2O));% %
header = ['Time , mCO2 , mH2O'];
data = [Time , mCO2, mH2O];
outid = fopen([data_dir 'species_mass_2.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_2.csv'],data,'roffset',1,'-append');

%% Case 3: Two compartments of sizes 9m*5m*4m and 9m*5m*2m are placed directly on top of each other with a 4m^2 ceiling vent in between the two. The same fire that is described in cases 1&2 is placed in the middle of compartment 1. 
findname = [data_dir 'species_mass_3'];
%Theoretical Mass calculations will be exactly the same as above. Same fire
%and same fuel source, so the mass of CO2 and H2O produced should
%theoretically still be the same.
[Time,mCO2,mH2O,TotH2Omass,TotCO2mass] = csvreaderALL(findname);
%Percent Error Calculations
ErrorCO2 = 100*abs((TheoreticalMassCO2 - TotCO2mass)/(TheoreticalMassCO2));% %
ErrorH2O = 100*abs((TheoreticalMassH2O - TotH2Omass)/(TheoreticalMassH2O));% %
header = ['Time , mCO2 , mH2O'];
data = [Time , mCO2, mH2O];
outid = fopen([data_dir 'species_mass_3.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_3.csv'],data,'roffset',1,'-append');

%%
header = ['t , MassCO2Prod , MassH2OProd'];
data = [t , MassCO2Prod , MassH2OProd];
outid = fopen([data_dir 'species_mass.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass.csv'],data,'roffset',1,'-append');


%% Case 4:
findname = [data_dir 'species_mass_4'];
[Time,mCO2,mH2O,TotH2Omass,TotCO2mass,mCO2comp,mH2Ocomp] = csvreaderALL(findname);
MCO2 = mCO2comp(:,1);%we are examining the flow in compartment 1 only
MH2O = mH2Ocomp(:,1);%we are examining the flow in compartment 1 only
header = ['Time , MCO2 , MH2O'];
data = [Time , MCO2, MH2O];
outid = fopen([data_dir 'species_mass_4.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'species_mass_4.csv'],data,'roffset',1,'-append');


