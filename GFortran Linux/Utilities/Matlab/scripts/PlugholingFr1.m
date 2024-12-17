%Script that is intended to read case_1.fds --> case_81.fds. The alpha values
%and the Froude1 values are calculated for each of the test cases.
%Afterwards, a plot of alpha vs the Froude number is created and a
%correlation is developed using power regression.

clear all
close all
plot_style
filename = 'case_';
cases = 81;
alpha = zeros(cases,1);%There will be one alpha value per case
vdotAVG = zeros(cases,1);
for i = 1:cases;
    %Locate the correct files and import the data
    newstring = strcat(num2str(i),'_devc.csv');
    findname = strcat(filename,newstring);
    Q = importdata(findname,',',2);
    F = strsplit(Q.textdata{1,1},',');
    
    %Find the locations of each of the data columns
    colT = find(strncmpi(F,'s',1));
    colV = find(strncmpi(F,'m3/',3));
    colM = find(strncmpi(F,'kg/',3));

    %Transfer the data from the csv file to this matlab script
    time = Q.data(:,colT);
    vol = Q.data(:,colV);
    mdote = Q.data(:,colM(1,1));%total mdot
    mdotu = Q.data(:,colM(1,2));%hot gas mdot
    
    %Average the results of mdote, mdotu and vdot
    [row1,column1] = find(floor(time) == 15);
    [row2,column2] = find(floor(time) == 30);
    mdoteINT = mdote(row1:row2,1);
    mdotuINT = mdotu(row1:row2,1);
    vdotINT = vol(row1:row2,1);
    mdoteAVG = mean(mdoteINT);
    mdotuAVG = mean(mdotuINT);
    vdotAVG(i) = mean(vdotINT);
        
    %Create the alpha vector
    alpha(i) = (mdotuAVG/mdoteAVG);
end    

    %Reading the csv file that contains the parameters from each of the
    %cases
    filename = '81cases_data.csv';
    G = importdata(filename,',',1);
    H = G.textdata;
    
    %Find the locations of each of the data columns
    colTemp = find(strcmp(H,'Delta T'));
    cold = find(strcmp(H,'Depth'));
    Temp = G.data(:,colTemp);
    depth = G.data(:,cold);
    
    %Other Parameters needed
    Tamb = 20 + 273.15;%K
    g = 9.81;%m/s^2
    
    %%
    %Find f1 -- The complicated Froude number equation
    dterm = sqrt(depth.^5);
    gTterm1 = sqrt( g * (Temp./(Temp+Tamb)).* (Tamb./(Temp+Tamb)));
    f1 = vdotAVG./(gTterm1.*dterm);
    plot(f1,alpha,'ko')
    title 'Distribution for 10cm Resolution'
    xlabel 'Froude Number'
    ylabel 'Fraction of Upper Mass Layer'
    print('Distribution for 10cm Resolution','-dpng')
    
    %Create an ordered list of f1 and alpha
    f2 = sort(f1);
    alphaNew = zeros(length(f1),1);
    for i = 1:length(f1)
        a = find(f1 == f2(i)); 
        alphaNew(i) = alpha(a);
    end
    %Delete all values of f2 that are less than 1.5
    f2(f2<1.5) = 0;
    alphaNew(f2 == 0) = 0;
    f2(f2 == 0) = [];
    alphaNew(alphaNew == 0) = [];
    
    %%
    plot(f2,alphaNew,'ko')
    hold on
    p = polyfit(log(f2),log(alphaNew),1);
    %fit = exp(p(2))*f3.^p(1);%powerfit
    fit = 1.33 * (f2.^-0.7);%altered powerfit that includes [1.5,1]
    plot(f2,fit,'k')
    axis( [0 20 0 1.1]);
    set(gca,'xTick',0:2:20)
    hold on
    f3 = [0 1.5];
    alphaB = [1 1];
    plot(f3,alphaB,'k')
    hold off 
    xlabel 'Froude Number'
    ylabel 'Fraction of Upper Layer Mass'
    legend('FDS','Best Fit')
    print('Froude1Correlation','-dpng')
    