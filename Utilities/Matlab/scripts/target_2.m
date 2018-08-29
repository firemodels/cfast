function [] = target_2(data_dir)
%This function will do a transpose of a matrix for the results obtained 
%from CFAST verification cases: target_2.in
%
%Fire heat flux as a function of location is desired. Yet in CFAST,
%the calculated results for each target (for different locations)for a
%specific time step are presented in a single row. With the fact that the   
%current Matlab script to handle verification and validation take a single 
%column of values. Thus, a transpose of matrix is needed.

%% Case 1: target_2.in
filename = [data_dir 'target_2_w.csv'];
X1 = [8, 6, 5, 4.5, 4.4, 4.3, 4.2, 4.1]';
Z = importdata(filename,',',5);
W = strsplit(Z.textdata{1,1},',');

%find the locations for all of the variable columns
col = find(strncmpi(W,'TRGFLXF',7))';
cfast_targ_flux_first = Z.data(:,col);
cfast_targ_flux_fire = cfast_targ_flux_first(1:8)';

header1 = ['Z, FIRE_HEAT_FLUX'];
data1 = [X1, cfast_targ_flux_fire];

outid = fopen([data_dir 'target_2_w2.csv'] , 'w+');
fprintf(outid,'%s',header1);
fclose(outid);
dlmwrite ([data_dir 'target_2_w2.csv'],data1,'roffset',1,'-append');
end