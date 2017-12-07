function [] = radiativefluxes(data_dir)
%This function will do a transpose of a matrix for the results obtained 
%from CFAST verification case: radiation_3.in
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

outid = fopen([data_dir 'radiation_3_w_top.csv'] , 'w+');
fprintf(outid,'%s',header1);
fclose(outid);
dlmwrite ([data_dir 'radiation_3_w_top.csv'],data1,'roffset',1,'-append');

outid = fopen([data_dir 'radiation_3_w_side.csv'] , 'w+');
fprintf(outid,'%s',header2);
fclose(outid);
dlmwrite ([data_dir 'radiation_3_w_side.csv'],data2,'roffset',1,'-append');
end

