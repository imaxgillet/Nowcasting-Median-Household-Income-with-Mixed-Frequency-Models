% estimate_all.m
% Estimates all model specifications on all backtest dates
% Max Gillet, 2024
%% Set up workspace
clear
clc
close all
addpath(genpath('utils'));
addpath(genpath('models'));
addpath(genpath(fullfile("C:\", "Users", getenv('username'), "AppData", "Roaming", "MathWorks", "MATLAB Add-Ons", "Toolboxes"))) % MFSS install location

%% Set inputs
% which estimation functions do we want to run? 
% What do we want to call their output folder?
estimation_functions = ["estimate_base_model", "estimate_ar1_model", ...
    "estimate_targeted_model", "estimate_targeted_ar1_model", "estimate_mfvar_model"];
savefolder_name = [ "base", "ar1_first", "targeted", "target_ar1", "mfvar"];
max_n_factors = 2;
data_path = fullfile("..", "data");

% where do we find the data files?
all_data_files = dir(fullfile(data_path,"*data.csv"));

% set up the output folder
backtest_name = string(datetime('now'), "yyyyMMdd_HHmmss");
backtest_name = strcat(backtest_name,"_backtest");

%% Run the backtest
% For each date on which to run the backtest (represented by the data
% available on that date...):
total_n = size(all_data_files,1);
for ad = 1:total_n

    % Run one backtest per factor + model specification
    total_per = length(estimation_functions)*max_n_factors;

    % Set up the data to feed into these backtests
    inputStruct= struct();    
    inputStruct.dataFile = fullfile(data_path, all_data_files(ad).name);
    inputStruct.dataSpec = fullfile("..", "input_data.csv");
    
    % Set up saves for all backtests
    short_filename = strrep(all_data_files(ad).name, ".csv", "");

    % For each estimation function...
    for nf=1:length(estimation_functions)
        
        % set up the function name and folder to save in
        f = estimation_functions(nf);
        saveFolder = savefolder_name(nf);

        % set up the estimation input parameters based on the function name
        if f == "estimate_mfvar_model"
            fullSaveFolder = saveFolder;
            inputStruct.model = saveFolder;
            inputStruct.outputPath = fullfile("..", "output", backtest_name, fullSaveFolder, short_filename);
            inputStruct.nlags = 1; % chosen based on find_bic_at_each_lag.m script
            feval(f, inputStruct); % run the estimation
        else
            inputStruct.nlags = 2; % chosen based on find_bic_at_each_lag.m script
            for n=1:max_n_factors % for each specific model (model + n factors)
                
                if (n > 1)
                    fullSaveFolder = saveFolder + string(n);
                else
                    fullSaveFolder = saveFolder;
                end
    
                if contains(f, "ar1")
                    inputStruct.ar1 = "first";
                else
                    inputStruct.ar1 = [];
                end
    
                inputStruct.model = saveFolder;
                inputStruct.outputPath = fullfile("..", "output", backtest_name, fullSaveFolder, short_filename);
                feval(f, inputStruct, 'nFactors', n);
            end
        end

    end

end