% run_updated_data.m
% updates existing model with new data
% Max Gillet, 2024

%% Set up workspace
clear
clc
close all
addpath(genpath('utils'));
addpath(genpath('models'));
addpath(genpath(fullfile("C:\", "Users", getenv('username'), "AppData", "Roaming", "MathWorks", "MATLAB Add-Ons", "Toolboxes"))) % MFSS install location


%% read in new y data matrix
inputStructure = struct();
basePath = '..';
% data spec file
inputStructure.dataSpec = fullfile(basePath, 'input_data.csv');
% data file
inputStructure.dataFile = fullfile(basePath, 'data', 'data.csv');
% path to read in model
% TODO: Change this to the output path from estimate preferred model
inputStructure.modelWorkspace = fullfile(basePath, 'output', 'yyyyMMdd_HHmmss_run', 'workspace.mat');

% folder path to save output from this procedure
inputStructure.outputPath = fullfile(basePath, 'output', 'updated_forecast');

production = load(inputStructure.modelWorkspace);

% read data spec
dataSpec = readtable(inputStructure.dataSpec);

% read data
data = readtable(inputStructure.dataFile, "ReadRowNames",true);
dataNames = data.Properties.VariableNames;
dataDates = datetime(data.Properties.RowNames);
data = data(dataDates >= datetime("1985-01-01"),:);
dataDates = dataDates(dataDates >= datetime("1985-01-01"),:);

% order dataspec to match data input
dataSpec.Properties.RowNames = dataSpec.code;
dataSpec = dataSpec(dataNames, :);

% check if HHI first
if ~strcmpi(dataNames(1), "HHI")
    error("Need HHI as first column of data.")
end

% drop related series from data and dataSpec
data = data(:, dataSpec.related ~= 1);
dataNames = dataNames(:, dataSpec.related ~= 1);
dataSpec = dataSpec(dataSpec.related ~= 1, :);

y = data{:,:};
ymeans = mean(y, "omitnan");
ysigs = std(y, "omitnan");
y = (y - ymeans) ./ ysigs;

accumulatorType = dataSpec.accumulator_name;
accumulatorN = dataSpec.accumulator_n;
accumulatorN(ismissing(accumulatorType)) = 1;

accum = Accumulator.GenerateRegular(y, ...
    accumulatorType, ...
    accumulatorN);
ss0AUpdate = accum.augmentStateSpace(production.ss0);

ssEUpdate = production.ssE;
ssEAUpdate = accum.augmentStateSpaceEstimation(ssEUpdate);

% need ssOpt, ssEA, sOut
ssOptUpdate = production.ssOpt;
% update ssOpt's tau so that the new y is acceptable
ssOptUpdate.n = size(y,1);
ssOptUpdate.tau = ss0AUpdate.tau;
[alphaUpdate, sOutUpdate] = ssOptUpdate.smooth(y');
stateUpdate = alphaUpdate(1,:)';
smoothedYUpdate = ssOptUpdate.Z * alphaUpdate;
incomeEstimatesUpdate = smoothedYUpdate(1,:)';
incomeEstimatesUpdate = incomeEstimatesUpdate*ysigs(:,1) + ymeans(:,1);

ssOptUpdate = ssOptUpdate.setDefaultInitial();
ssOptUpdate.a0 = zeros(size(ssOptUpdate.a0)); % want to kick it off from
ssOptUpdate.P0 = 100.*eye(size(ssOptUpdate.P0)); % zeros(size(ssOpt.P0)); % diffuse prior

% getting bands
disp('Sampling the states...')
% difference btwn sampling the states and smoothing sample
% issue with positive definite
% [~, mleVariance]  = ssOpt.stateSample(y', [], [], ssEA.ThetaMapping, 1);
tm = ssEAUpdate.ThetaMapping;
theta = tm.system2theta(ssOptUpdate);
alphaHat = ssOptUpdate.smooth(y', [], []);
fisherInformation = -ssOptUpdate.gradFinDiffSecond(y', [], [], tm, theta);
mleVariance = inv(fisherInformation * ssOptUpdate.n);

% Check if outer product of gradients is positive semi-definite
try
  chol(mleVariance);
catch
  [q,D] = eig(mleVariance);
  d= diag(D);
  d(d <= eps) = 1e-10;
  mleVariance = q*diag(d)*q';
end

sOutUpdate.Theta = ssEAUpdate.ThetaMapping.system2theta(ssOptUpdate);
sOutUpdate.varTheta = mleVariance;

% add an extra year, at a minimum
yForecast = [y; nan(12, size(y,2))];

accumExtra = Accumulator.GenerateRegular(yForecast, ...
    accumulatorType, ...
    accumulatorN);
ss0AExtra = accumExtra.augmentStateSpace(production.ss0);


nSamples = 1000;
alphaSim = nan(size(yForecast,1), size(alphaUpdate,1) ,nSamples);
sampledY = nan(size(yForecast,1), 1, size(alphaSim,3));
parfor ii = 1:nSamples
    theta = mvnrnd(sOutUpdate.Theta, sOutUpdate.varTheta, 1)'; %#ok<PFBNS>
    ssFcast = ssEAUpdate.ThetaMapping.theta2system(theta); %#ok<PFBNS>
    ssFcast.a0 = ssOptUpdate.a0;
    ssFcast.P0 = ssOptUpdate.P0;

    ssFcast.n = size(yForecast,1);
    % also add to the tau to forecast an extra 12 months
    ssFcast.tau = ss0AExtra.tau;

    steps = size(yForecast,1);
    alphaSim(:,:,ii) = ssFcast.smoothSample(yForecast', [], [], [], 1);
    smoothedY_i = alphaSim(:,:,ii)*ssFcast.Z';
    sampledY(:, :, ii) = smoothedY_i(:,1)*ysigs(:,1) + ymeans(:,1);
end

est5 = prctile(squeeze(sampledY)',5)';
est10 = prctile(squeeze(sampledY)',10)';
est16 = prctile(squeeze(sampledY)',16)';
est25 = prctile(squeeze(sampledY)',25)';
est50 = prctile(squeeze(sampledY)',50)';
est75 = prctile(squeeze(sampledY)',75)';
est84 = prctile(squeeze(sampledY)',84)';
est90 = prctile(squeeze(sampledY)',90)';
est95 = prctile(squeeze(sampledY)',95)';

% create new dates
nextSom = dataDates(end)+caldays(1);
dataDatesAddl = (nextSom+calmonths(1)):calmonths(1):(nextSom+calmonths(12));
dataDatesAddl = dataDatesAddl - caldays(1);
newDataDates = [dataDates; dataDatesAddl'];

ssOptFcast = ssOptUpdate;
ssOptFcast.n = size(yForecast,1);
% also add to the tau to forecast an extra 12 months
ssOptFcast.tau = ss0AExtra.tau;
[alphaFcast, ~] = ssOptFcast.smooth(yForecast');
incomeEstimatesFcast = ssOptFcast.Z * alphaFcast; %smoothedY(1,:)';
incomeEstimatesFcast = (incomeEstimatesFcast(1,:)')*ysigs(:,1) + ymeans(:,1);
stateFcast = alphaFcast(1,:)';

% 
fill([newDataDates; flipud(newDataDates)],[est10; flipud(est90)], .7*ones(1,3), 'EdgeColor', 'none')
hold on
fill([newDataDates; flipud(newDataDates)],[est25; flipud(est75)], .5*ones(1,3), 'EdgeColor', 'none')
plot(newDataDates, est50, "Color","black")
plot(dataDates, incomeEstimatesUpdate, "Color","red")
xlim([datetime("1990-01-01"), datetime("2024-07-01")])
hold off

% now for the state

stateEst5 = prctile(squeeze(alphaSim(:,1,:))',5)';
stateEst10 = prctile(squeeze(alphaSim(:,1,:))',10)';
stateEst16 = prctile(squeeze(alphaSim(:,1,:))',16)';
stateEst25 = prctile(squeeze(alphaSim(:,1,:))',25)';
stateEst50 = prctile(squeeze(alphaSim(:,1,:))',50)';
stateEst75 = prctile(squeeze(alphaSim(:,1,:))',75)';
stateEst84 = prctile(squeeze(alphaSim(:,1,:))',84)';
stateEst90 = prctile(squeeze(alphaSim(:,1,:))',90)';
stateEst95 = prctile(squeeze(alphaSim(:,1,:))',95)';

fill([newDataDates; flipud(newDataDates)],[stateEst10; flipud(stateEst90)], .7*ones(1,3), 'EdgeColor', 'none')
hold on
fill([newDataDates; flipud(newDataDates)],[stateEst25; flipud(stateEst75)], .5*ones(1,3), 'EdgeColor', 'none')
plot(newDataDates, stateEst50, "Color","black")
plot(dataDates, stateUpdate, "Color","red")
xlim([datetime("1990-01-01"), datetime("2024-07-01")])
ylim([-5,5])
hold off

mkdir(inputStructure.outputPath)
save(fullfile(inputStructure.outputPath, 'updateWorkspace.mat'))

% Note: only use the EOY income estimates! 
newIncomeEstimates = [incomeEstimatesUpdate; incomeEstimatesFcast(end-11:end,1)];
newState = [stateUpdate; stateFcast(end-11:end,1)];
yToMatch = yForecast(:,1)*ysigs(:,1) + ymeans(:,1);


runTime = toc;
outputTable = table(newDataDates, newIncomeEstimates,...
    est5, est10, est16, est25, est50, est75, est84, est90, est95, ...
    newState, stateEst5, stateEst10, stateEst16, stateEst25, stateEst50, stateEst75, stateEst84, stateEst90, stateEst95, ...
    yToMatch, ...
    'VariableNames',["dataDates", "incomeEstimates", ...
    "band5", "band10", "band16", "band25", "band50", "band75", "band84", "band90", "band95", ...
    "state", "stateBand5", "stateBand10", "stateBand16", "stateBand25", "stateBand50", "stateBand75", "stateBand84", "stateBand90", "stateBand95", "incomeToMatch"]);

writetable(outputTable, ...
    fullfile(inputStructure.outputPath, 'updateOutput.csv'))
