function [Time,mCO2,mH2O] = csvreaderALL(filename,Comp)
[H D] = dvcread(filename,1);

colTime = find(strncmpi(H,'Time',4));
colO2u = find(strncmpi(H,strcat('ULO2',Comp),6));
colCO2u = find(strncmpi(H,strcat('ULCO2',Comp),7));
colH2Ou = find(strncmpi(H,strcat('ULH2O',Comp),7));
colO2l = find(strncmpi(H,strcat('LLO2',Comp),6));
colCO2l = find(strncmpi(H,strcat('LLCO2',Comp),7));
colH2Ol = find(strncmpi(H,strcat('LLH2O',Comp),7));

Time = D(:,colTime)/60;
CO2u = D(:,colCO2u);
H2Ou = D(:,colH2Ou);
CO2l = D(:,colCO2l);
H2Ol = D(:,colH2Ol);

mCO2 = CO2u;
mH2O = H2Ou;
if size(CO2u) == size(CO2l) % if it's a one zone case, there is no lower layer
    mCO2 = CO2u + CO2l;
    mH2O = H2Ou + H2Ol;
end

end