function [Time,mCO2vector,mH2Ovector,TotH2Omass,TotCO2mass] = csvreaderALL(filename)

Po = 101300;%Pa
R = 8.314;%kJ/(mol*K)
MO2 = 32.00;%g/mol
MCO2 = 44.01;%g/mol
MH2O = 18.01528;%g/mol
MN2 = 28.02;%g/mol
MC = 12.01;%g/mol
MH = 1.008;%g/mol
MO = 16.00;%g/mol
filenameN = strcat(filename,'_n.csv');
Q = importdata(filenameN,',',2);
F = strsplit(Q.textdata{1,1},',');

colTu = find(strncmpi(F,'ULT',3));
colVu = find(strncmpi(F,'VOL',3));
colTl = find(strncmpi(F,'LLT',3));
colLH = find(strncmpi(F,'HGT',3));
colTime = find(strcmp(F,'Time'));
colP = find(strncmpi(F,'PRS',3));
numComp = length(colTu);
dim = size(Q.data);
a = dim(1);
Time = zeros(dim(1),colTime);
Tu = zeros(dim(1),numComp);
P = zeros(dim(1),numComp);
Vu = zeros(dim(1),numComp);
Tl = zeros(dim(1),numComp);
Vl = zeros(dim(1),numComp);
LH = zeros(dim(1),numComp);
for i = 1:dim(1)
    Time(i) = Q.data(i,colTime);
end
    
for i = 1:numComp
for j = 1:dim(1)    
Tu(j,i) = Q.data(j,colTu(i)) + 273.15;%K
P(j,i) = Q.data(j,colP(i)) + Po;%Pa
Vu(j,i) = Q.data(j,colVu(i));%m^3
Vl(j,i) = Vu(j,i)*Q.data(j,colLH(i))/(Q.data(1,colLH(i)) - Q.data(j,colLH(i)));%m^3
Tl(j,i) = Q.data(j,colTl(i)) + 273.15;%K
end
end
%Analyzing the S file and extracting the nexessary quantities
filenameS = strcat(filename,'_s.csv');
S = importdata(filenameS,',',2);
W = strsplit(S.textdata{1,1},',');
colO2u = find(strncmpi(W,'ULO2',4));
colCO2u = find(strncmpi(W,'ULCO2',5));
colH2Ou = find(strncmpi(W,'ULH2O',5));
colO2l = find(strncmpi(W,'LLO2',4));
colCO2l = find(strncmpi(W,'LLCO2',5));
colH2Ol = find(strncmpi(W,'LLH2O',5));
dim = size(S.data);

mfH2Ou = zeros(dim(1),numComp);
mfCO2u = zeros(dim(1),numComp);
mfO2u = zeros(dim(1),numComp);
mfN2u = zeros(dim(1),numComp);
MWAvgu = zeros(dim(1),numComp);
YH2Ou = zeros(dim(1),numComp);
YCO2u = zeros(dim(1),numComp);

mfH2Ol = zeros(dim(1),numComp);
mfCO2l = zeros(dim(1),numComp);
mfO2l = zeros(dim(1),numComp);
mfN2l = zeros(dim(1),numComp);
MWAvgl = zeros(dim(1),numComp);
YH2Ol = zeros(dim(1),numComp);
YCO2l = zeros(dim(1),numComp);

%upper layer mass fraction of H2O and CO2 calculations
for i = 1:numComp
for j = 1:dim(1)
mfH2Ou(j,i) = S.data(j,colH2Ou(i));
mfCO2u(j,i) = S.data(j,colCO2u(i));
mfO2u(j,i) = S.data(j,colO2u(i));
mfN2u(j,i) = 1 - (mfH2Ou(j,i)+mfCO2u(j,i)+mfO2u(j,i));
MWAvgu(j,i) = mfH2Ou(j,i)*MH2O + mfCO2u(j,i)*MCO2 + mfO2u(j,i)*MO2 + mfN2u(j,i)*MN2;
YH2Ou(j,i) = (mfH2Ou(j,i)*MH2O)/MWAvgu(j,i);
YCO2u(j,i) = (mfCO2u(j,i)*MCO2)/MWAvgu(j,i);
end
end

%lower layer mass fraction of H2O and CO2 calculations
for i = 1:numComp
for j = 1:dim(1)
mfH2Ol(j,i) = S.data(j,colH2Ol(i));
mfCO2l(j,i) = S.data(j,colCO2l(i));
mfO2l(j,i) = S.data(j,colO2l(i));
mfN2l(j,i) = 1 - (mfH2Ol(j,i)+mfCO2l(j,i)+mfO2l(j,i));
MWAvgl(j,i) = mfH2Ol(j,i)*MH2O + mfCO2l(j,i)*MCO2 + mfO2l(j,i)*MO2 + mfN2l(j,i)*MN2;
YH2Ol(j,i) = (mfH2Ol(j,i)*MH2O)/MWAvgl(j,i);
YCO2l(j,i) = (mfCO2l(j,i)*MCO2)/MWAvgl(j,i);
end
end

MWAvguNew = zeros(a,numComp);
Rspecificu = zeros(a,numComp);
mGASupper = zeros(a,numComp);
mCO2expu = zeros(a,numComp);
mH2Oexpu = zeros(a,numComp);
for i = 1:numComp
    for j = 1:a;
MWAvguNew(j,i) = MWAvgu(j,i)/1000;%kg/mol
Rspecificu(j,i) = R/MWAvguNew(j,i);%This formula calculates for the R value in the appropriate units (J/(kg*K))
mGASupper(j,i) = P(j,i)*Vu(j,i)/(Rspecificu(j,i)*Tu(j,i));
mCO2expu(j,i) = mGASupper(j,i) * YCO2u(j,i);
mH2Oexpu(j,i) = mGASupper(j,i) * YH2Ou(j,i);
    end
end
TotH2OmassU = sum(mH2Oexpu(a,:));
TotCO2massU = sum(mCO2expu(a,:));

MWAvglNew = zeros(a,numComp);
Rspecificl = zeros(a,numComp);
mGASlower = zeros(a,numComp);
mCO2expl = zeros(a,numComp);
mH2Oexpl = zeros(a,numComp);
for i = 1:numComp
    for j = 1:a;
MWAvglNew(j,i) = MWAvgl(j,i)/1000;%kg/mol
Rspecificl(j,i) = R/MWAvglNew(j,i);%This formula calculates for the R value in the appropriate units (J/(kg*K))
mGASlower(j,i) = P(j,i)*Vl(j,i)/(Rspecificl(j,i)*Tl(j,i));
mCO2expl(j,i) = mGASlower(j,i) * YCO2l(j,i);
mH2Oexpl(j,i) = mGASlower(j,i) * YH2Ol(j,i);
    end
end
TotH2OmassL = sum(mH2Oexpl(a,:));
TotCO2massL = sum(mCO2expl(a,:));

mCO2vector = mCO2expl + mCO2expu;
mH2Ovector = mH2Oexpl + mH2Oexpu;
TotH2Omass = TotH2OmassL+TotH2OmassU;
TotCO2mass = TotCO2massL+TotCO2massU;

end