% McGrattan
% March 21, 2011
% CFAST_verification_script.m
%
% This script creates the plots that are included in the CFAST V&V
% Guide. It consists of calls to other scripts contained within the
% subfolder called "scripts". 
%
% The most important script is called dataplot. It reads the file called
% validation_data_config_matlab.csv and generates 1000+ plots. If you
% want to process only some of these plots, comment out the other 
% scripts and change the data plot line as follows:
%
% [saved_data,drange] = dataplot(cfil,vdir,plotdir,[a:b]);
%
% where a and b are the lines in the .csv file you want to process.
% Alternatively, you can specify the "Dataname" you want:
%
% [saved_data,drange] = dataplot(cfil,vdir,plotdir,'WTC');
%
% In this case, all the WTC results will be plotted. 

close all
clear all

addpath 'scripts'

% Scripts that run prior to dataplot


% dataplot creates most of the plots for the Validation Guide. It must be run before scatplot, which makes the scatter plots.

Dataplot_Inputs_File = [pwd,'/CFAST_verification_justplot_inputs.csv'];
Working_Dir = [pwd, '/../../Verification/'];
Manuals_Dir = [pwd, '/../../Docs/Validation_Guide/FIGURES/'];
Scatterplot_Inputs_File = [pwd, '/CFAST_verification_scatterplot_inputs.csv'];

% Statistics output options

Stats_Output = 'Verification';
Output_File = [pwd, '/CFAST_verification_scatterplot_output.csv'];
Statistics_Tex_Output = [pwd, '/../../Docs/Validation_Guide/FIGURES/ScatterPlots/verification_statistics.tex'];

% Override the plot style options with NRC 1824 plot options

NRC_Options = false;
Append_To_Scatterplot_Title = '';

% Run dataplot and scatplot scripts

[saved_data,drange] = justplot(Dataplot_Inputs_File, Working_Dir, Manuals_Dir);

% Special cases

 
display('Verification scripts completed successfully!')
