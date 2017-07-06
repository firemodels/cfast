function [] = fire_ignition(data_dir)

%the script scales the fire igintion columns in the Ignition_test
%verification case to match the HRR, flux, and temperature data for
%v&v plotting

%first the normal output file with heat release rates and ignition times
filename = [data_dir 'Ignition_Test_n.csv'];
[Fn Qn] = dvcread(filename,1,5);
    
%Find the locations of each of the data columns
colT = find(strncmpi(Fn,'Time',4));
colign1 = find(strncmpi(Fn,'IGN_2',5));
colhrr1 = find(strncmpi(Fn,'HRR_2',5));
colign2 = find(strncmpi(Fn,'IGN_3',5));
colhrr2 = find(strncmpi(Fn,'HRR_3',5));

%then the target output file with heat flux and temperature for
%targets associated with each fire ignition
filename = [data_dir 'Ignition_Test_w.csv'];
[Ft Qt] = dvcread(filename,1,5);
    
%Find the locations of each of the data columns
colflx1 = find(strncmpi(Ft,'TRGFLXI_1',9));
coltemp2 = find(strncmpi(Ft,'TRGSURT_2',9));

%Transfer the data from the csv file to this matlab script
time = Qn(:,colT);
ign1 = Qn(:,colign1);
hrr1 = Qn(:,colhrr1);
ign2 = Qn(:,colign2);
hrr2 = Qn(:,colhrr2);
flx1 = Qt(:,colflx1);
temp2 = Qt(:,coltemp2);

%calculate scaled ignition indicators
scalehrr1 = ign1*max(hrr1)*2;
scalehrr2 = ign2*max(hrr2)*2;
scaleflx1 = ign1*max(flx1)*2;
scaletemp2 = ign2*max(temp2)*2;

%output data to file for plotting
header = ['Time , HRR_1 , HRR_2, FLUX_1, TEMP_2, IGNHRR_1, IGNHRR_2, IGNFLUX_1, IGNTEMP_2'];
data = [time , hrr1, hrr2, flx1, temp2, scalehrr1, scalehrr2, scaleflx1, scaletemp2];
outid = fopen([data_dir 'fire_ignition.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'fire_ignition.csv'],data,'roffset',1,'-append');


