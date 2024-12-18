function [dataDates, incomeEstimates, bic, hqic] = estimate_base_model(inputStructure, varargin)

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

nLags = inputStructure.nlags;
nSeries = size(data, 2);
nT = size(data, 1);

%% Set up Model
Z = [nan(nSeries, nFactors), zeros(nSeries, (nLags-1)*nFactors)];
H = diag(nan(nSeries,1)); 

T = zeros(nLags*nFactors);
T(1:nFactors, 1:(nLags*nFactors)) = nan;
T((nFactors+1):(nFactors*nLags), 1:(nFactors*(nLags-1))) = eye(nFactors*(nLags-1));
R = [eye(nFactors); zeros((nLags-1)*nFactors,nFactors)];
Q = eye(nFactors);

% TODO: warn if MFSS not available
ssE = StateSpaceEstimation(Z, H, T, Q, 'R', R);
 
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

accumulatorType = dataSpec.accumulator_name;
accumulatorN = dataSpec.accumulator_n;
accumulatorN(ismissing(accumulatorType)) = 1;

accum = Accumulator.GenerateRegular(y, ...
    accumulatorType, ...
    accumulatorN);
ssEA = accum.augmentStateSpaceEstimation(ssE);

% very weird final element of f0 w/o this end -1
accum_struct = struct('index', accum.index,...
    'calendar', accum.calendar(1:(end-1),:), ...
    'horizon', accum.horizon(1:(end-1),:));
[f0, G0, S0, YHat, XHat, A] = StockWatsonMF_alt(y(1:(end-1),:), ...
    'accum', accum_struct, 'factors', nFactors);

%% Initial values
% we want the shocks to have var of 1, but PCA forces the factors to have
% that. Let's rescale f0 and G0
[t_beta,t_xx,t_ee] = ols_var(f0(sum(isnan(f0),2)==0, :),nLags,0);

f0 = (1./diag(t_ee)') .* f0;
G0 = (diag(t_ee)') .* G0;

Z0 = Z;
Z0(:,1:nFactors) = G0;

H0 = S0; % observation error

T0 = [t_beta [eye(nFactors*(nLags-1)); ...
    zeros(nFactors, nFactors*(nLags-1))]];
T0 = T0';

R0 = R;
Q0 = Q;

ss0 = StateSpace(Z0, H0, T0, Q0, 'R', R0);
ss0A = accum.augmentStateSpace(ss0);

% Check ss0A
[test, ll0] = ss0A.filter(y');
assert(isfinite(ll0));
%% Estimate the model
ssEA.diagnosticPlot = true;
ssEA.useParallel = true;
ssEA.solver = {'fmincon', 'fminsearch'};
%ssEA.stepTol = opts.tol;   
ssEA.fminsearchMaxIter = 1000;

[ssOpt, diagnostic, thetaHat, gradient] = ssEA.estimate(y', ss0A);
[alpha, sOut] = ssOpt.smooth(y');

bic = ssEA.ThetaMapping.nTheta*log(ssOpt.n) - 2*sOut.logli;
hqic = -2*sOut.logli + 2*ssEA.ThetaMapping.nTheta*log(log(ssOpt.n));

state = alpha(1,:)';
smoothedY = ssOpt.Z * alpha;
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