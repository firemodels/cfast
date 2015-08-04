%This script reads the _zone.csv file for the test case 'ventilation_4.in'.
%First, the mass flow rates through each slab are calculated and compared with the 
%expected results. In order to calculate the velocity profile through the doorway, 
%the pressure in the compartment as a function of height is calculated. The
%equation for then velocity profile is then applied and a plot is created.

clear all
close all
format long
plot_style
%Define the constants
C = 0.70;
Cp = 1012;%J/(kg*K)
gamma = 1.4;
R = ((gamma - 1)/(gamma)) * Cp;
g = 9.80665;%m/s^2
w = 1.5;%m
L = 4;
A = w * L; 
rhoamb = 101325/(R*293.15);
%Read in the zone.csv file for the case
filename = 'ventilation_4';
filenameZone = [filename '_zone.csv'];
Q = importdata(filenameZone,',',2);

%find the locations for all of the variable columns
colTime = find(strncmpi(Q.colheaders,'Time',4))';
colLLT = find(strncmpi(Q.colheaders,'LLT_1',5))';
colULT = find(strncmpi(Q.colheaders,'ULT_1',5))';
colP = find(strncmpi(Q.colheaders,'PRS_1',5))';
colH = find(strncmpi(Q.colheaders,'HGT_1',5))';
colF = find(strncmpi(Q.colheaders,'HSLABF',6))';
colTop = find(strncmpi(Q.colheaders,'HSLABYT',7))';
colBottom = find(strncmpi(Q.colheaders,'HSLABYB',7))';
colSlabs = find(strncmpi(Q.colheaders,'HSLAB_1',7))';
SlabsCFAST = Q.data(:,colSlabs);%number of slabs present at any given time in CFAST
Time = Q.data(:,colTime);%time vector
H = Q.data(:,colH);%height vector as function of time

%find the dimension of the data in the zone file
dim = size(Q.data);
rows = dim(1);
columns = dim(2);

%Erase all columns that have values of only 0
a = zeros(length(colF),1);
b = zeros(length(colTop),1);
c = zeros(length(colBottom),1);
for i = 2:length(colF)
if 1 && all(Q.data(:,colF(i)) == 0)
    a(i) = colF(i);
end
if 1 && all(Q.data(:,colTop(i)) == 0)
    b(i) = colTop(i);
end
if 1 && all(Q.data(:,colBottom(i)) == 0)
    c(i) = colBottom(i);
end
end
colF( a ~= 0 ) = [];
colTop( b ~= 0 ) = [];
colBottom( c ~= 0 ) = [];

%Determine the max number of slabs you have at any given time
numSlabs = length(colF);

%Find the top and bottom positions for each slab
t = zeros(rows,numSlabs);
b = zeros(rows,numSlabs);
for i = 1:numSlabs
    t(:,i) = Q.data(:,colTop(i));
    b(:,i) = Q.data(:,colBottom(i));
end

%Solve for the density of air in each slab (upper and lower)
P = Q.data(:,colP) + 101325;
Tu = Q.data(:,colULT) + 273.15;
Tl = Q.data(:,colLLT) + 273.15;
rhoU = P./(R*Tu);
rhoL = P./(R*Tl);
P = P - 101325;

deltaPb = zeros(rows,numSlabs);
deltaPt = zeros(rows,numSlabs);
deltaPNum = zeros(rows,numSlabs);
deltaPDenom = zeros(rows,numSlabs);
deltaPterm = zeros(rows,numSlabs);
mdot = zeros(rows,numSlabs);
avg = zeros(rows,numSlabs);
for i = 1:numSlabs    
rho = rhoL;    
%Solve for deltaPt and deltaPb for each slab (formulas apply when slab is in lower layer, but above the neutral plane)   
deltaPb(:,i) = P - (rhoL*g.*b(:,i)) + (rhoamb*g*b(:,i));
deltaPt(:,i) = P - (rhoL*g.*t(:,i)) + (rhoamb*g*t(:,i));

    if b(4:35,i) >= H(4:35,1) %these formulas apply when slab is in upper layer
    deltaPb(:,i) = P - (rhoL*g.*H) - (rhoU*g.*(b(:,i)-H)) + (rhoamb*g*b(:,i));    
    deltaPt(:,i) = P - (rhoL*g.*H) - (rhoU*g.*(t(:,i)-H)) + (rhoamb*g*t(:,i));
    rho = rhoU;%the rho used must now be changed
    end
    
    if b(100:200,i) >= H(100:200,1) %these formulas apply when slab is in upper layer
    deltaPb(:,i) = P - (rhoL*g.*H) - (rhoU*g.*(b(:,i)-H)) + (rhoamb*g*b(:,i));    
    deltaPt(:,i) = P - (rhoL*g.*H) - (rhoU*g.*(t(:,i)-H)) + (rhoamb*g*t(:,i));
    rho = rhoU;%the rho used must now be changed
    end
    
    if b(500,i) >= H(500,1) %these formulas apply when slab is in upper layer
    deltaPb(:,i) = P - (rhoL*g.*H) - (rhoU*g.*(b(:,i)-H)) + (rhoamb*g*b(:,i));    
    deltaPt(:,i) = P - (rhoL*g.*H) - (rhoU*g.*(t(:,i)-H)) + (rhoamb*g*t(:,i));
    rho = rhoU;%the rho used must now be changed
    end

%Apply the equation to solve for the mdot in each case
deltaPNum(:,i) = ((abs(deltaPt(:,i))).^(3/2)) - ((abs(deltaPb(:,i))).^(3/2));
deltaPDenom(:,i) = abs(deltaPt(:,i)) - abs(deltaPb(:,i));
deltaPterm(:,i) = deltaPNum(:,i)./deltaPDenom(:,i);

%If the number of slabs occuring at any given moment is less than the
%number of total slabs, then there is zero flow going through the slabs
%that aren't technically there. Removes deltaPb and deltaPt values that
%occur where the slabs don't exist.

    for j = 1:rows
        if SlabsCFAST(j) < i
            mdot(j,i) = 0;
            deltaPb(j,i) = 0;
            deltaPt(j,i) = 0;
        end
    end
    
avg(:,i) = (deltaPt(:,i)+deltaPb(:,i))/2;
%if the mass flow is neagtive, the air is being pushed into the
%compartment, so the ambient air density must be used
if sign(avg(1:end,i)) <= 0
    rho = rhoamb;
end
mdot(:,i) = sign(avg(:,i))*(2/3)*C.*sqrt(2*rho)*w.*(t(:,i) - b(:,i)).*deltaPterm(:,i);
end
mdot(isnan(mdot)) = 0;
plot(Time,mdot(:,1),'r',Time,Q.data(:,colF(1)),'x')
%%
plot(Time,mdot(:,2),'r',Time,Q.data(:,colF(2)),'x')
%%
plot(Time,mdot(:,3),'r',Time,Q.data(:,colF(3)),'x')

%%
stepsize = (L - 0)/(rows-1);
z = [0:stepsize:4]';
deltaPz = zeros(length(z),1);
v = zeros(length(z),1);
for i = 1:length(z)
    if z(i) >= H(end)
        rho = (P(end)+101325)/(R*Tu(end));
    elseif z(i) < H(end)
        rho = (P(end)+101325)/(R*Tl(end));
    end
deltaPz(i) = (P(end) - (rho*g*z(i))) + (rhoamb*g*z(i));
a = sign(deltaPz(i));
v(i) = a * C * sqrt(2*abs(deltaPz(i))/rho);
end
plot(z,v,'k')
xlabel 'Velocity (m/s)'
ylabel 'Position (m)'

