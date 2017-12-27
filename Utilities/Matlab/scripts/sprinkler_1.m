function [] = sprinkler_1(data_dir)
%This function calculates expected sprinkler temperatures and HRR values 
%once the sprinkler activates.

%The local temperature and velocity calculated by CFAST (test case 'sprinkler_1.in') 
%are read by MATLAB and then the expected results are calculated for
%the sprinkler link temperature and the heat release rate of the fire.
%These results are saved to the file sprinkler_1.csv for use in
%verification ploting script.

%Define the conditions
tempInit = 40;%C
uw = 0.07;%mm/s
tau = 3*(uw^-1.8);
RTI = 100;%ms^0.5

%Read in the _w.csv file for the case
filename = [data_dir 'sprinkler_1'];
filenamew = [filename '_w.csv'];
Z = importdata(filenamew,',',5);
W = strsplit(Z.textdata{1,1},',');

%find the locations for all of the variable columns
colTime = find(strncmpi(W,'Time',4))';
colv = find(strncmpi(W,'SENSGASVEL_1',12))';
colTg = find(strncmpi(W,'SENSGAST_1',10))';
colTL = find(strncmpi(W,'SENST_1',7))';
v_vec = Z.data(:,colv);
Tg_vec = Z.data(:,colTg);
TL_vec = Z.data(:,colTL);
t_vec = Z.data(:,colTime);

%Solve for the TL temperatures
v = @(t) interp1(t_vec,v_vec,t);
Tg = @(t) interp1(t_vec,Tg_vec,t);
f = @(t,TL) (sqrt(v(t))/RTI)*(Tg(t)-TL);
[t_vec,TL] = ode45(f,t_vec,20);

%The following section is used to reduce the number of datapoints located in
%the expected results vector
timeEX = t_vec;
TLex = TL;
for i = 2:length(timeEX)
    if rem(timeEX(i),250) ~= 0
        TLex(i) = 0;
        timeEX(i) = 0;
    end
end
timeEX(TLex == 0) = [];
TLex(TLex == 0) = [];

%Plot the expected & CFAST results against one another
plot(timeEX,TLex,'ro',t_vec,TL_vec,'b')
xlabel 'Time (s)'
ylabel 'Link Temperature'
legend('theoretical','CFAST','Location','SouthEast')
%%
%Read in the n.csv file for the case
filenameZone = [filename '_n.csv'];
Q = importdata(filenameZone,',',5);
F = strsplit(Q.textdata{1,1},',');

%Find the locations for all of the variables and store the columns
colQ = find(strncmpi(F,'HRR_1',5))';
hrr = Q.data(:,colQ);

%The expected heat release rate is being calculated
tactLoc = find(floor(TL) == tempInit);
tactLoc = tactLoc(1);
tact = t_vec(tactLoc);
Qtact = hrr(tactLoc);
timeSprink = t_vec(tactLoc+1:end);
hrrSprink = Qtact * exp(-1/tau*(timeSprink-tact));
hrrCFAST = hrr(tactLoc+1:end);

%The following section is used to reduce the number of datapoints located in
%the expected results vector
timeshort = timeSprink;
for i = 2:length(timeshort)
    if rem(timeshort(i),250) ~= 0
        hrrSprink(i) = 0;
        timeshort(i) = 0;
    end
end
timeshort(timeshort == 0) = [];
hrrSprink(hrrSprink == 0) = [];

%Plot the expected &CFAST results against one another
plot(timeshort,hrrSprink,'ro',timeSprink,hrrCFAST,'b')
xlabel 'Time (s)'
ylabel 'HRR (W)'
legend('theoretical','CFAST')

header = ['TimeLinkEx , TempLinkEx , TimeHRREx , HRREx'];
data = [timeEX , TLex , timeshort , hrrSprink];
outid = fopen([data_dir 'sprinkler_1.csv'] , 'w+');
fprintf(outid,'%s',header);
fclose(outid);
dlmwrite ([data_dir 'sprinkler_1.csv'],data,'roffset',1,'-append');
end