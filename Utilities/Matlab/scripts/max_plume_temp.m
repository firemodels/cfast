% Peacock
% 12-6-2017
% NIST_NRC_Corner_Effects.m
%
% Reads the CFAST _w.csv file and writes output in a form appropriate for dataplot.m

outdir = '../../Validation/NIST_NRC_Corner_Effects/';

casename{1}  = 'corner_200_kW';
casename{2}  = 'corner_300_kW';
casename{3}  = 'corner_400_kW';
casename{4}  = 'wall_200_kW';
casename{5}  = 'wall_300_kW';
casename{6}  = 'wall_400_kW';
casename{7}  = 'cabinet_01';
casename{8}  = 'cabinet_02';
casename{9}  = 'cabinet_03';
casename{10} = 'cabinet_04';
casename{11} = 'cabinet_05';
casename{12} = 'cabinet_06';
casename{13} = 'cabinet_07';
casename{14} = 'cabinet_08';
casename{15} = 'cabinet_09';
casename{16} = 'cabinet_10';
casename{17} = 'cabinet_11';
casename{18} = 'cabinet_12';

for j=1:1

M = importdata([outdir,casename{j},'_w.csv'],',',5);
W = strsplit(M.textdata{1,1},',');%find the locations for all of the variable columns
col = find(strncmpi(W,'TRGGAST',7))';
t = M.data(:,col);

H = cell(2,4);
H(1,:) = {'s' 'C' 'C' 'C'};
H(2,:) = {'Time' 'Lower' 'Middle' 'Upper'};

fid = fopen([outdir,casename{j},'_Plume.csv'],'wt','n');
fprintf(fid,'%s,%s,%s,%s\n',H{1,:});
fprintf(fid,'%s,%s,%s,%s\n',H{2,:});
n_times = length(t(:,1));
for i=1:n_times
    highvals  = [t(i,1) t(i,4) t(i,7) t(i,10) t(i,13) t(i,16) t(i,19)];
    high = max(highvals);
    midvals  = [t(i,2) t(i,5) t(i,8) t(i,11) t(i,14) t(i,17) t(i,20)];
    mid = max(midvals);
    lowvals = [t(i,3) t(i,6) t(i,9) t(i,12) t(i,15) t(i,18) t(i,21)];
    low = max(lowvals);
    fprintf(fid,'%4.0f,%5.1f,%5.1f,%5.1f\n',M.data(i,1),low,mid,high);
end
fclose(fid);
clear M
clear W
clear col
clear t

end

