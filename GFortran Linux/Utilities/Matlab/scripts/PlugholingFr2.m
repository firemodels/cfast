%Script that is intended to read the 'ceiling_vent_correlation' excel sheet. The alpha values
%and the Froude2 values are input to this MATLAB Script from the file.
%Afterwards, a plot of alpha vs the Froude number is created and a
%correlation is developed using gaussian regression.

clear all
close all
plot_style
filename = 'ceiling_vent_correlation';
%Locate the correct files and import the data
findname = strcat(filename,'.xlsx');
Q = importdata(findname,',',2);
%Find the locations of Fr1 and alpha
d = size(Q.textdata);
for i = 1:d(2)
F = strsplit(Q.textdata{1,i},',');
if strncmpi(F,'Fr1',3) == 1
Fcol = i;
end
if strncmpi(F,'alpha',5) == 1
acol = i;
end
end
%Store Froude_vec and alpha_vec
Froude_vec = Q.data(:,Fcol);
alpha_vec = Q.data(:,acol);
%Find and delete the outlier points
k = find(1-alpha_vec < 0.01 & Froude_vec > 1);
Froude_vec(k) = [];
alpha_vec(k) = [];
q = find(alpha_vec < 0.02 & Froude_vec < 2.3);
Froude_vec(q) = [];
alpha_vec(q) = [];
%The Gaussian Regression Function
f = @(x) exp(-((x)/2).^2);
plot(Froude_vec,alpha_vec,'ko')
hold on
fplot(f,[0,5],'b')
hold off
xlabel 'Froude Number'
ylabel 'Fraction of Upper Layer Mass'
legend('FDS','Best Fit')
print('Froude2Correlation','-dpng')
print('Froude2Correlation','-dpdf')

