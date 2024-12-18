function [dataDates, incomeEstimates, bic, hqic] = estimate_targeted_model(inputStructure, varargin)

% Parse the input arguments
inP = inputParser;
inP.addParameter('nFactors', 1, @isnumeric);
inP.parse(varargin{:});
nFactors = inP.Results.nFactors;

tic;

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

% addpath(genpath('C:\Users\g1img01\AppData\Roaming\MathWorks\MATLAB Add-Ons\Toolboxes\MFSS'))
nLags = inputStructure.nlags;
nSeries = size(data, 2);
nTargets = 1;
nSeries = nSeries - nTargets;
nT = size(data, 1);

target = y(:, strcmpi(dataNames, "HHI"));

y = y(:, ~strcmpi(dataNames, "HHI"));
dataNames = dataNames(:, ~strcmpi(dataNames, "HHI"));

accumulatorType = dataSpec(2:end,:).accumulator_name;
accumulatorN = dataSpec(2:end,:).accumulator_n;
accumulatorN(ismissing(accumulatorType)) = 1;

accum = Accumulator.GenerateRegular(y, ...
    accumulatorType, ...
    accumulatorN);

accum_struct = struct('index', accum.index,...
    'calendar', accum.calendar(1:(end-1),:), ...
    'horizon', accum.horizon(1:(end-1),:));

[f0, G0, S0, YHat, XHat, A] = StockWatsonMF_alt(y(1:(end-1),:), ...
    'accum', accum_struct, 'factors', nFactors);
f0 = [f0; nan(1, nFactors)];

%% Set up Model
Z = [nan(nTargets, nFactors) zeros(nTargets, nFactors*(nLags-1)) ; ...
    eye(nFactors) zeros(nFactors, nFactors*(nLags-1))] ;

H = [nan(nTargets) zeros(nTargets, nFactors); zeros(nFactors, nTargets) nan(nFactors)];

T = [nan(nFactors*nLags, nFactors) ...
    [eye(nFactors*(nLags-1)); ...
    zeros(nFactors, nFactors*(nLags-1))]];
T = T';
R = [eye(nFactors); zeros((nLags-1)*nFactors, nFactors)]; 

Q = eye(nFactors); % diag(nan(1,nFactors));
d = [nan; zeros(nFactors,1)];

newY = [target, f0];
accumFinal = Accumulator.GenerateRegular(newY, ...
    {'AVG'}, ...
    [12]);

% TODO: warn if MFSS not available
ssE = StateSpaceEstimation(Z, H, T, Q, 'R', R, 'd', d);
ssEA = accumFinal.augmentStateSpaceEstimation(ssE);

% Stationarity constraint on the AR() dynamics
ssE.constraints{1} = @(theta,ss) max(abs(eig(ss.T(:,:,1))))-1;
ssE.constraints{2} = @(theta,ss) max(abs(eig(ss.T(:,:,2))))-1;
ssE.constraints{3} = @(theta,ss) max(abs(eig(ss.T(:,:,3))))-1;
ssE.constraints{4} = @(theta,ss) max(abs(eig(ss.T(:,:,4))))-1;
ssE.constraints{5} = @(theta,ss) max(abs(eig(ss.T(:,:,5))))-1;
ssE.constraints{6} = @(theta,ss) max(abs(eig(ss.T(:,:,6))))-1;
ssE.constraints{7} = @(theta,ss) max(abs(eig(ss.T(:,:,7))))-1;
ssE.constraints{8} = @(theta,ss) max(abs(eig(ss.T(:,:,8))))-1;
ssE.constraints{9} = @(theta,ss) max(abs(eig(ss.T(:,:,9))))-1;
ssE.constraints{10} = @(theta,ss) max(abs(eig(ss.T(:,:,10))))-1;
ssE.constraints{11} = @(theta,ss) max(abs(eig(ss.T(:,:,11))))-1;
ssE.constraints{12} = @(theta,ss) max(abs(eig(ss.T(:,:,12))))-1;

%% Initial values
% run bivariate VAR in order to fill in target variable
% gets you beta0 for target ~ beta * factor
% also gets you cov of target

initialization_data = [y(:, strcmpi(dataNames, "PINC")), target];

targetAccum = Accumulator.GenerateRegular(initialization_data, ...
    {'', 'AVG'}, ...
    [0 12]);

initialization_var = MFVAR(initialization_data, 1, targetAccum);
initvar_opt = initialization_var.estimate();
[initAlpha, initSmoother, initFilter] = initvar_opt.smooth(initialization_data);
 
target0 = initAlpha(:,2);
target_var_error = initvar_opt.Q(2,2);

[t_beta,t_xx,t_ee] = ols_var(f0(sum(isnan(f0),2)==0, :),nLags,0);
% collapse_factor = (G0'*G0)\(G0');
% Ytilde = YHat*(collapse_factor'); 

[factor_beta,~,model_resid,~,model_stats] = regress(target0, [ones(height(f0),1), f0]);
target_model_error = cov(model_resid, 'partialrows');

Z0 = Z;
Z0(1,1:nFactors) = factor_beta(2:end);

H0 = H;

H0(1,1) = target_model_error; 
H0(2:end,2:end) = 10.*eye(nFactors);

T0 = T;
T0(1:nFactors,:) = t_beta';
R0 = R;
Q0 = Q;
d0 = d;
d0(1,1) = factor_beta(1);

ss0 = StateSpace(Z0, H0, T0, Q0, 'R', R0, 'd', d0);
ss0A = accumFinal.augmentStateSpace(ss0);

% Check ss0A
[test, ll0] = ss0A.filter(newY');
assert(isfinite(ll0));
%% Estimate the model
ssEA.diagnosticPlot = true;
ssEA.useParallel = true;
ssEA.solver = {'fmincon', 'fminsearch'};
% ssEA.verbose = true;  
ssEA.fminsearchMaxIter = 1000;

[ssOpt, diagnostic, thetaHat, gradient] = ssEA.estimate(newY', ss0A);
[alpha, sOut] = ssOpt.smooth(newY');

bic = ssEA.ThetaMapping.nTheta*log(ssOpt.n) - 2*sOut.logli;
hqic = -2*sOut.logli + 2*ssEA.ThetaMapping.nTheta*log(log(ssOpt.n));

smoothedY = ssOpt.Z * alpha + ssOpt.d;
incomeEstimates = smoothedY(1,:)';

incomeEstimates = incomeEstimates*ysigs(:,1) + ymeans(:,1);

mkdir(inputStructure.outputPath)
save(fullfile(inputStructure.outputPath, 'workspace.mat'))

runTime = toc;
outputTable = table(dataDates, incomeEstimates, repmat(runTime, size(dataDates)), repmat(bic, size(dataDates)), ...
    'VariableNames',["dataDates", "incomeEstimates", "runTime", "bic"]);
writetable(outputTable, ...
    fullfile(inputStructure.outputPath, 'output.csv'))
end