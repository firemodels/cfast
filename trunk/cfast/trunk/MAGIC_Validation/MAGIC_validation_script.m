% McGrattan
% March 21, 2011
% master_validation_script.m
%
% This script creates the plots that are included in the FDS Validation 
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

addpath '../Utilities/Matlab/scripts'

% Scripts that run prior to dataplot

%flame_height
%cat_mccaffrey
%NIST_RSE

% dataplot creates most of the plots for the Validation Guide. It must be run before scatplot, which makes the scatter plots.

Dataplot_Inputs_File = [pwd,'/MAGIC_validation_dataplot_inputs.csv'];
Working_Dir = [pwd, '/../MAGIC_Validation/'];
Manuals_Dir = [pwd, '/Docs/FIGURES/'];
Scatterplot_Inputs_File = [pwd, '/MAGIC_validation_scatterplot_inputs.csv'];
Output_File = [pwd, '/MAGIC_validation_scatterplot_outputs.csv'];
Stats_Output = 2;
Statistics_Tex_Output = [pwd, '/Docs/FIGURES/Scatterplots/validation_statistics.tex'];
Histogram_Tex_Output = [pwd, '/Docs/FIGURES/Scatterplots/validation_histograms.tex'];

% Override the plot style options with NRC 1824 plot options
NRC_Options = false;
Append_To_Scatterplot_Title = '';

[saved_data,drange] = dataplot(Dataplot_Inputs_File, Working_Dir, Manuals_Dir);
scatplot(saved_data, drange, ...
         'Scatterplot_Inputs_File', Scatterplot_Inputs_File, ...
         'Manuals_Dir', Manuals_Dir, ...
         'Output_File', Output_File, ...
         'Stats_Output', Stats_Output, ...
         'Statistics_Tex_Output', Statistics_Tex_Output, ...
         'Histogram_Tex_Output', Histogram_Tex_Output, ...
         'NRC_Options', NRC_Options, ...
         'Append_To_Scatterplot_Title', Append_To_Scatterplot_Title)

% Miscellaneous other scripts for special cases

%harrisonplumes
%beyler_hood
%check_hrr
%sandia_helium_plume
%sandia_methane_fire
%spray_attenuation
%Cup_burner
%vettori_flat
%vettori_sloped
%flame_height2
 
display('validation scripts completed successfully!')
