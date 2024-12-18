% find_bic_at_each_lag.m
% Estimates all models with different lags to calc BIC, HQIC
% Max Gillet, 2024

%% set up workspace
clear
clc
close all
addpath(genpath('utils'));
addpath(genpath('models'));
addpath(genpath(fullfile("C:\", "Users", getenv('username'), "AppData", "Roaming", "MathWorks", "MATLAB Add-Ons", "Toolboxes"))) % MFSS install location

%% set up inputs
data_path = fullfile("..", "data");
data_file = fullfile(data_path,"data.csv");

lagtest_name = string(datetime('now'), "yyyyMMdd_HHmmss");
lagtest_name  = strcat(lagtest_name ,"_lagtest");

% test models
estimation_functions = ["estimate_base_model", "estimate_ar1_model", ...
    "estimate_targeted_model", "estimate_targeted_ar1_model", ...
    "estimate_mfvar_model"];
savefolder_name = [ "base", "ar1_first",...
    "targeted", "target_ar1", ...
    "mfvar"];
max_n_factors = 2;

%% test with lags 1-6
lagsToUse = 1:6;

lagModel = [];
factorModel = {};
bicModel = [];
hqicModel = [];
nfModel = [];
%% test on current data only
% for each model...
for nf=1:length(estimation_functions)
    f = estimation_functions(nf);
    saveFolder = savefolder_name(nf);
    
    inputStruct= struct();
    inputStruct.dataFile = data_file;
    inputStruct.dataSpec = fullfile("..", "input_data.csv");

    % fit it with all desired lag orders
    for nl=1:length(lagsToUse)
        inputStruct.nlags = lagsToUse(nl);
        
        if f == "estimate_mfvar_model"
            fullSaveFolder = saveFolder;
            inputStruct.model = saveFolder;
            inputStruct.outputPath = fullfile("..", "output", lagtest_name, fullSaveFolder, string(lagsToUse(nl)));
            [dataDates, estimate, bic, hqic] = feval(f, inputStruct);
            lagModel(end+1) = lagsToUse(nl);
            factorModel{end+1} = f;
            bicModel(end+1) = bic;
            hqicModel(end+1) = hqic;
            nfModel(end+1) = 0;
            fprintf(['Model (' num2str(f) ') with lags = ' num2str(lagsToUse(nl)) ' gives bic = ' num2str(bic)])
        else
            for n=1:max_n_factors % do this for each N = number of factors
                
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
                inputStruct.outputPath = fullfile("..", "output", lagtest_name, fullSaveFolder, string(lagsToUse(nl)));
                [dataDates, estimate, bic, hqic] = feval(f, inputStruct, 'nFactors', n);
                fprintf(['Model (' num2str(f) ') with lags = ' num2str(lagsToUse(nl)) ' and nFactors = ' num2str(n) ' gives bic = ' num2str(bic)])
                lagModel(end+1) = lagsToUse(nl);
                factorModel{end+1} = f;
                bicModel(end+1) = bic;
                hqicModel(end+1) = hqic;
                nfModel(end+1) = n;

            end

        end
    end
end
    

%% write out results
resultTable = table([lagModel' nfModel' string(factorModel') bicModel' hqicModel']);
writetable(resultTable, "../output/lagBIC.csv")