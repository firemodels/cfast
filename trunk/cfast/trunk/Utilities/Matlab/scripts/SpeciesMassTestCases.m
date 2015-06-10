%This script will calculate the experimental and theoretical yields of H2O
%and CO2 for several test cases. The program functions by reading in
%values from the specific .csv files. The program finds values of
%Temperature, Pressure and Volume of the upper layer, as well as
%the corresponding mole fractions of O2, CO2 and H2O in the upper layer,
%which are then used to calculate the experimental masses of CO2 and H2O produced.


%Case 1: A one compartment room with a fire placed in the center of the room.
%There are no vents of any kind. The energy produced by the fire ramps up
%linearly for 30 seconds to 1 kW, then levels off for 5 minutes and finally
%goes down to zero kW in 30 seconds. The dimensions of the room are
%5m*6m*3m.

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

plot_style
set(gca,'Units',Plot_Units)
set(gca,'Position',[Plot_X,Plot_Y,Plot_Width,Plot_Height])
set(gca,'OuterPosition',[0.4,Plot_Y,Plot_Width,Plot_Height])
set(gcf,'DefaultLineLineWidth',Line_Width)
set(gca,'FontName',Font_Name)

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
t = [0 30 330 360 2000]';
TotEnergyCons = [0 15 315 330 330]';
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
filename = [data_dir 'species_mass_1'];
[Time,mCO2vector,mH2Ovector,TotH2Omass,TotCO2mass] = csvreaderALL(filename);

%Percent Error Calculations
ErrorCO2 = 100*abs((TheoreticalMassCO2 - TotCO2mass)/(TheoreticalMassCO2));% %
ErrorH2O = 100*abs((TheoreticalMassH2O - TotH2Omass)/(TheoreticalMassH2O));% %
disp('The theoretical values for mass of H2O and CO2 in kg, respectively, are:')
disp([TheoreticalMassH2O,TheoreticalMassCO2])
disp('The experimental values for mass of H2O and CO2 in kg, respectiely, are:')
disp([TotH2Omass,TotCO2mass])
disp('The percentage errors for the mass of H2O and CO2 found experimentally are:')
disp([ErrorH2O,ErrorCO2])

plot(Time,mCO2vector,Time,mH2Ovector)
hold on
plot(t,MassCO2Prod,t,MassH2OProd)
hold off
axis([0 3000 0 0.02])
xlabel('Time (s)','Interpreter',Font_Interpreter,'FontSize',Label_Font_Size)
ylabel('Mass (kg)','Interpreter',Font_Interpreter,'FontSize',Label_Font_Size)
legend1 = legend('CFAST CO2','CFAST H2O','Expected CO2','Expected H2O','Location','southeast');
set(legend1, 'interpreter',Font_Interpreter, 'fontsize', 6);
print(gcf,'-dpdf',[plot_dir 'species_mass_1'])
%% Case 2: Two compartments of sizes 2m*5m*8m are placed side by side, with a door of height 6m and width 1m connecting them. There is a fire in the middle of the first compartment that has the same energy production as the fire in the first case study. The fuel is still methane.
filename = [data_dir 'species_mass_2'];
%Theoretical Mass calculations will be exactly the same as above. Same fire
%and same fuel source, so the mass of CO2 and H2O produced should
%theoretically still be the same.
[Time,mCO2vector,mH2Ovector,TotH2Omass,TotCO2mass] = csvreaderALL(filename);

%Percent Error Calculations
ErrorCO2 = 100*abs((TheoreticalMassCO2 - TotCO2mass)/(TheoreticalMassCO2));% %
ErrorH2O = 100*abs((TheoreticalMassH2O - TotH2Omass)/(TheoreticalMassH2O));% %
disp('The theoretical values for mass of H2O and CO2 in kg, respectively, are:')
disp([TheoreticalMassH2O,TheoreticalMassCO2])
disp('The experimental values for mass of H2O and CO2 in kg, respectiely, are:')
disp([TotH2Omass,TotCO2mass])
disp('The percentage errors for the mass of H2O and CO2 found experimentally are:')
disp([ErrorH2O,ErrorCO2])

plot(Time,mCO2vector,Time,mH2Ovector)
hold on
plot(t,MassCO2Prod,t,MassH2OProd)
hold off
axis([0 3000 0 0.02])
xlabel('Time (s)','Interpreter',Font_Interpreter,'FontSize',Label_Font_Size)
ylabel('Mass (kg)','Interpreter',Font_Interpreter,'FontSize',Label_Font_Size)
legend2 = legend('CFAST CO2 upper','CFAST CO2 lower','CFAST H2O upper','CFAST H2O lower','Expected CO2','Expected H2O','Location','southeast');
set(legend2, 'interpreter',Font_Interpreter, 'fontsize', 6);
print(gcf,'-dpdf',[plot_dir 'species_mass_2'])

%% Case 3: Two compartments of sizes 9m*5m*4m and 9m*5m*2m are placed directly on top of each other with a 4m^2 ceiling vent in between the two. The same fire that is described in cases 1&2 is placed in the middle of compartment 1. 
filename = [data_dir 'species_mass_3'];
%Theoretical Mass calculations will be exactly the same as above. Same fire
%and same fuel source, so the mass of CO2 and H2O produced should
%theoretically still be the same.
[Time,mCO2vector,mH2Ovector,TotH2Omass,TotCO2mass] = csvreaderALL(filename);

%Percent Error Calculations
ErrorCO2 = 100*abs((TheoreticalMassCO2 - TotCO2mass)/(TheoreticalMassCO2));% %
ErrorH2O = 100*abs((TheoreticalMassH2O - TotH2Omass)/(TheoreticalMassH2O));% %
disp('The theoretical values for mass of H2O and CO2 in kg, respectively, are:')
disp([TheoreticalMassH2O,TheoreticalMassCO2])
disp('The experimental values for mass of H2O and CO2 in kg, respectiely, are:')
disp([TotH2Omass,TotCO2mass])
disp('The percentage errors for the mass of H2O and CO2 found experimentally are:')
disp([ErrorH2O,ErrorCO2])

plot(Time,mCO2vector,Time,mH2Ovector)
hold on
plot(t,MassCO2Prod,t,MassH2OProd)
hold off
axis([0 3000 0 0.02])
xlabel('Time (s)','Interpreter',Font_Interpreter,'FontSize',Label_Font_Size)
ylabel('Mass (kg)','Interpreter',Font_Interpreter,'FontSize',Label_Font_Size)
legend3 = legend('CFAST CO2 upper','CFAST CO2 lower','CFAST H2O upper','CFAST H2O lower','Expected CO2','Expected H2O','Location','southeast');
set(legend3, 'interpreter',Font_Interpreter, 'fontsize', 6);
print(gcf,'-dpdf',[plot_dir 'species_mass_3'])
%% Case 4:
