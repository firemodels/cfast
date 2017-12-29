function [] = radiativefluxes(data_dir)
%This function will do a transpose of a matrix for the results obtained 
%from CFAST verification cases: radiation_3.in, radiation_4.in, and 
%radiation_5.in
%
%Radiative heat flux as a function of location is desired. Yet in CFAST,
%the calculated results for each target (for different locations)for a
%specific time step are presented in a single row. With the fact that the   
%current Matlab script to handle verification and validation take a single 
%column of values. Thus, a transpose of matrix is needed.

%% Case 1: radiation_3.in
filename = [data_dir 'radiation_3_w.csv'];
X1 = [0.0909,0.2727,0.4545,0.6364,0.8182,1.0000,1.1818,1.3636,1.5455,1.7273,1.9091]';
X2 = [0.125,0.375,0.625,0.875,1.125,1.375,1.625,1.875,2.125,2.375,2.625,2.875,3.125,3.375,3.625,3.875]';
Z = importdata(filename,',',5);
W = strsplit(Z.textdata{1,1},',');

T_gas = 1000; % in K
E = 5.67e-8* T_gas^4/ 1000;

%find the locations for all of the variable columns
col = find(strncmpi(W,'TRGFLXG',7))';
cfast_targ_flux_gas = Z.data(:,col);
cfast_targ_top_gas  = cfast_targ_flux_gas(1:11)';
cfast_targ_side_gas = cfast_targ_flux_gas(12:27)';
cfast_emittance_top = cfast_targ_top_gas/E;
cfast_emittance_side = cfast_targ_side_gas/E;
col = find(strncmpi(W,'TRGFLXS',7))';
cfast_targ_flux_wall = Z.data(:,col);
cfast_targ_top_wall  = cfast_targ_flux_wall(1:11)';
cfast_targ_side_wall = cfast_targ_flux_wall(12:27)';
%note that cfast_targ_top_wall is the radiative heat transfer due to
%emission from a hot wall (A_j) transmitted through medium incident to
%surface A_i. Thus, blackbody emissive power - transmitted energy = energy
%being absorbed by the medium at (T_w, T_g)
cfast_absorptance_top = (E-cfast_targ_top_wall)/E;
cfast_absorptance_side = (E-cfast_targ_side_wall)/E;

header1 = ['X, GAS(TOP), WALL(TOP)'];
header2 = ['Z, GAS(SIDE), WALL(SIDE)'];
data1 = [X1, cfast_targ_top_gas, cfast_targ_top_wall];
data2 = [X2, cfast_targ_side_gas, cfast_targ_side_wall];

outid = fopen([data_dir 'radiation_3_w_top.csv'] , 'w+');
fprintf(outid,'%s',header1);
fclose(outid);
dlmwrite ([data_dir 'radiation_3_w_top.csv'],data1,'roffset',1,'-append');

outid = fopen([data_dir 'radiation_3_w_side.csv'] , 'w+');
fprintf(outid,'%s',header2);
fclose(outid);
dlmwrite ([data_dir 'radiation_3_w_side.csv'],data2,'roffset',1,'-append');
%% Case 2: radiation_4.in
filename = [data_dir 'radiation_4_w.csv'];
X1 = [0.0909,0.2727,0.4545,0.6364,0.8182,1.0000,1.1818,1.3636,1.5455,1.7273,1.9091]';
X2 = [0.125,0.375,0.625,0.875,1.125,1.375,1.625,1.875,2.125,2.375,2.625,2.875,3.125,3.375,3.625,3.875]';
Z = importdata(filename,',',5);
W = strsplit(Z.textdata{1,1},',');

T_wall = 500; % in K
T_gas = 1000;
E_wall = 5.67e-8* T_wall^4/ 1000;
E_gas = 5.67e-8* T_gas^4/ 1000;

%find the locations for all of the variable columns
col = find(strncmpi(W,'TRGFLXG',7))';
cfast_targ_flux_gas = Z.data(:,col);
cfast_targ_top_gas  = cfast_targ_flux_gas(1:11)';
%cfast_targ_side_gas = cfast_targ_flux_gas(12:27)';
cfast_emittance_top = cfast_targ_top_gas/E_gas;
%cfast_emittance_side = cfast_targ_side_gas/E_gas;
col = find(strncmpi(W,'TRGFLXS',7))';
cfast_targ_flux_wall = Z.data(:,col);
cfast_targ_top_wall  = cfast_targ_flux_wall(1:11)';
%cfast_targ_side_wall = cfast_targ_flux_wall(12:27)';
cfast_absorptance_top = (E_wall-cfast_targ_top_wall)/E_wall;
%cfast_absorptance_side = cfast_targ_side_wall/E_wall;

header3 = ['X, EMIS(TOP), ABSO(TOP)'];
%header4 = ['Z, EMIS(SIDE), ABSO(SIDE)'];
data3 = [X1, cfast_emittance_top, cfast_absorptance_top];
%data4 = [X2, cfast_emittance_side, cfast_absorptance_side];

outid = fopen([data_dir 'radiation_4_a_top.csv'] , 'w+');
fprintf(outid,'%s',header3);
fclose(outid);
dlmwrite ([data_dir 'radiation_4_a_top.csv'],data3,'roffset',1,'-append');

%outid = fopen([data_dir 'radiation_4_a_side.csv'] , 'w+');
%fprintf(outid,'%s',header4);
%fclose(outid);
%dlmwrite ([data_dir 'radiation_4_a_side.csv'],data4,'roffset',1,'-append');
%% Case 3: radiation_5.in
filename = [data_dir 'radiation_5_w.csv'];
X1 = [0.0909,0.2727,0.4545,0.6364,0.8182,1.0000,1.1818,1.3636,1.5455,1.7273,1.9091]';
X2 = [0.125,0.375,0.625,0.875,1.125,1.375,1.625,1.875,2.125,2.375,2.625,2.875,3.125,3.375,3.625,3.875]';
Z = importdata(filename,',',5);
W = strsplit(Z.textdata{1,1},',');

T_wall = 1500; % in K
T_gas = 1000;
E_wall = 5.67e-8* T_wall^4/ 1000;
E_gas = 5.67e-8* T_gas^4/ 1000;

%find the locations for all of the variable columns
col = find(strncmpi(W,'TRGFLXG',7))';
cfast_targ_flux_gas = Z.data(:,col);
cfast_targ_top_gas  = cfast_targ_flux_gas(1:11)';
%cfast_targ_side_gas = cfast_targ_flux_gas(12:27)';
cfast_emittance_top = cfast_targ_top_gas/E_gas;
%cfast_emittance_side = cfast_targ_side_gas/E_gas;
col = find(strncmpi(W,'TRGFLXS',7))';
cfast_targ_flux_wall = Z.data(:,col);
cfast_targ_top_wall  = cfast_targ_flux_wall(1:11)';
%cfast_targ_side_wall = cfast_targ_flux_wall(12:27)';
cfast_absorptance_top = (E_wall-cfast_targ_top_wall)/E_wall;
%cfast_absorptance_side = cfast_targ_side_wall/E_wall;

header3 = ['X, EMIS(TOP), ABSO(TOP)'];
%header4 = ['Z, EMIS(SIDE), ABSO(SIDE)'];
data3 = [X1, cfast_emittance_top, cfast_absorptance_top];
%data4 = [X2, cfast_emittance_side, cfast_absorptance_side];

outid = fopen([data_dir 'radiation_5_a_top.csv'] , 'w+');
fprintf(outid,'%s',header3);
fclose(outid);
dlmwrite ([data_dir 'radiation_5_a_top.csv'],data3,'roffset',1,'-append');

%outid = fopen([data_dir 'radiation_5_a_side.csv'] , 'w+');
%fprintf(outid,'%s',header4);
%fclose(outid);
%dlmwrite ([data_dir 'radiation_5_a_side.csv'],data4,'roffset',1,'-append');
end

