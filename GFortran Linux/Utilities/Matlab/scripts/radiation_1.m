%This script reads the _w.csv and _n.csv files for the test case
%'radiation_1.in'. The expected values for the temperature of the target are calculated as the fire
%burns in the compartment. The CFAST and expected values are plotted
%against one another.

clear all
close all
format long
plot_style

%define the necessary constants
Qdot = 10;%kW
radfrac = 0.35;
r = 2.00;%m
rho = 7854;%kg/m^3
delta = 0.0015;%m
Cp = 0.559;%kJ/kg*K
Tamb = 20;
C = 1.31;%assume for walls
emiss = 0.9;
sigma = 5.67E-11;
qrdot = emiss * (radfrac*Qdot)/(4*pi*r^2);

%Read in the _n and _w files
filename = 'radiation_1';
filenamew = [filename '_devices.csv'];
filenamen = [filename '_compartments.csv'];
Z = importdata(filenamew,',',2);
W = strsplit(Z.textdata{1,1},',');
Q = importdata(filenamen,',',2);
F = strsplit(Q.textdata{1,1},',');

%find the locations for all of the variable columns
colTime = find(strncmpi(W,'Time',4))';
colT = find(strncmpi(W,'TRGSURT_1',10))';
colTg = find(strncmpi(F,'LLT_1',5))';
Time = Z.data(:,colTime);
TargT = Z.data(:,colT);
Tg_vec = Q.data(:,colTg);

%solve for the theoretical temperature of the target
Tg = @(t) interp1(Time,Tg_vec,t);
h = @(t,T) 2*(C * abs(Tg(t) - T)^(1/3));
qcdot = @(t,T) (h(t,T)*(Tg(t) - T)/1000);
radloss = @(t,T) 2*sigma * emiss * (((Tg(t)+273.15)^4) - ((T+273.15)^4));
dTdt = @(t,T) (qrdot + qcdot(t,T) + radloss(t,T))/(delta*rho*Cp);
[t,theoT] = ode45(dTdt,Time,Tamb);
plot(Time,theoT,'--',Time,TargT,'r')
xlabel 'Time (s)'
ylabel 'Surface Temperature (\circC)'
legend ('theoretical','CFAST','Location','SouthEast')

%The following section is used to obtain the data to be plotted using the
%dataplot function. The following lines of code select a few data points to
%plot, allowing you to clearly put the expected and CFAST curves on top of
%each other.
for i = 2:length(Time)
    if rem(Time(i),200) ~= 0
        theoT(i) = 0;
        Time(i) = 0;
    end
end
Time(theoT == 0) = [];
theoT(theoT == 0) = [];
