% estimate_preferred_model.m
% Estimates the favored model specification on the most recent data
% Max Gillet, 2024

%% Set up workspace
clear
clc
close all
addpath(genpath('utils'));
addpath(genpath('models'));
addpath(genpath(fullfile("C:\", "Users", getenv('username'), "AppData", "Roaming", "MathWorks", "MATLAB Add-Ons", "Toolboxes"))) % MFSS install location

%% Set up inputs
data_path = fullfile("..", "data");

data_file = fullfile(data_path,"data.csv");
run_name = string(datetime('now'), "yyyyMMdd_HHmmss");
run_name = strcat(run_name,"_run");

% Set up the input structure
inputStruct= struct();
inputStruct.nlags = 2;
inputStruct.dataFile = data_file;
inputStruct.dataSpec = fullfile("..", "input_data.csv");
inputStruct.ar1 = "first";
inputStruct.model = "ar1_first";
inputStruct.outputPath = fullfile("..", "output", run_name);

% Run the model
estimate_ar1_model(inputStruct, 'nFactors', 1);