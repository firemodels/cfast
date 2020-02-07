function [Time,mCO2,mH2O] = csvreaderALL(filename,Comp)
[H D] = dvcread(filename,1,5);

colTime = find(strncmpi(H,'Time',4));
colO2u = find(strncmpi(H,strcat('ULMO2',Comp),7));
colCO2u = find(strncmpi(H,strcat('ULMCO2',Comp),8));
colH2Ou = find(strncmpi(H,strcat('ULMH2O',Comp),8));
colO2l = find(strncmpi(H,strcat('LLMO2',Comp),7));
colCO2l = find(strncmpi(H,strcat('LLMCO2',Comp),8));
colH2Ol = find(strncmpi(H,strcat('LLMH2O',Comp),8));

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
