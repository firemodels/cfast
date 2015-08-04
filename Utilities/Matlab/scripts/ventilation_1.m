%This script reads the _n.csv file for the test case 'ventilation_1.in' and
%produces the expected calculations for the pressure change in the
%compartment. 


clear all
close all
plot_style
filename = 'ventilation_1';
filenameN = [filename '_n.csv'];
Q = importdata(filenameN,',',2);
F = strsplit(Q.textdata{1,1},',');

%Find locations of Time, T and P
colTime = find(strncmpi(F,'Time',4));
colT = find(strncmpi(F,'LLT_1',5));
colP = find(strncmpi(F,'PRS_1',5));

%Define the necessary constants
A = 0.01*0.01;
C = 0.7;
g = 9.80665;%m/s^2
z= 1.5;%m
Cp = 1012;%J/(kg*K)
gamma = 1.4;
R = ((gamma - 1)/(gamma)) * Cp;

%Create T and P vectors
Time = Q.data(:,colTime);
T = Q.data(:,colT);
P = Q.data(:,colP);
T = T + 273.15;
P = P + 101325;

%Solve for the densities of air inside and outside the compartment
rho = P./(R*T);%inside
rhoamb = P(1)/(R*T(1));%outside

%Solve for mdot
Vdot = 0.01;%m^3/s
mdot = rhoamb * Vdot;

%Solve for deltaP
rhoterm = 1./(2*rho);
mdotterm = (mdot/(C*A))^2;
deltaP = rhoterm*mdotterm + rho*g*z;
Time = Time/3600;%hours

%Only the end pressure changes of the expected results were plotted, the
%following lines of code find the specific positions at the end of the
%simulation
a = find(Time == 10);
b = find(Time == 15);
c = find(Time == 20);
d = find(Time == 25);
TimeNew = [ Time(a) Time(b) Time(c) Time(d) Time(end) ]';
deltaPNew = [deltaP(a) deltaP(b) deltaP(c) deltaP(d) deltaP(end) ]';

P = P - 101325;
plot(Time,P,'k',TimeNew,deltaPNew,'ko')
xlabel 'Time (hr)'
ylabel 'Change in Pressure (Pa)'
legend ('CFAST','Expected','Location','SouthEast')