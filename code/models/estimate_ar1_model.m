function [dataDates, incomeEstimates, bic, hqic] = estimate_ar1_model(inputStructure, varargin)

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
nT = size(data, 1);

%% Set up Model
if inputStructure.ar1 == "all" % AR(1) for idiosyncratic errors in all series
    nStates = (nFactors*nLags) + nSeries;

    % Will keep track of e_{t-1} in the state
    Z = [nan(nSeries, nFactors) zeros(nSeries, nFactors*(nLags-1)) eye(nSeries)];

    % so we do not need errors in measurement equation
    H = blkdiag(zeros(nSeries)); 

    % AR(1) for errors + AR(p) for factor
    T = zeros(nStates);
    T(1:nFactors, 1:(nLags*nFactors)) = nan;
    T((nFactors+1):(nFactors*nLags), 1:(nFactors*(nLags-1))) = eye(nFactors*(nLags-1));
    T((nFactors*(nLags))+1:nStates, (nFactors*(nLags))+1:nStates) = diag(nan(nSeries, 1));

    % in state equation 
    R = blkdiag([eye(nFactors); zeros((nLags-1)*nFactors,nFactors)], eye(nSeries));
    Q = blkdiag(eye(nFactors), diag(nan(nSeries, 1))); % normalization to 1? 

elseif inputStructure.ar1 == "first" % only AR(1) for errors in HHI measurement
    nStates = (nLags*nFactors) + 1;

    % Need to keep track of that single e_{t-1} in the state
    Z = [nan(nSeries, nFactors) ...
        zeros(nSeries, (nLags-1)*nFactors) ...
        [1; zeros(nSeries-1,1)]];

    % factor error in the state eqn, s
    H = blkdiag(zeros(1),diag(diag(nan(nSeries-1)))); 

    T = zeros(nStates);
    T(1:nFactors, 1:(nLags*nFactors)) = nan;
    T((nFactors+1):(nFactors*nLags), 1:(nFactors*(nLags-1))) = eye(nFactors*(nLags-1));
    T(nStates, nStates) = nan;
  
    R = blkdiag([eye(nFactors); zeros((nLags-1)*nFactors,nFactors)], 1);
    Q = blkdiag(eye(nFactors), nan);
else
    error("Unknown ar1 variable")
end

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

accum_struct = struct('index', accum.index,...
    'calendar', accum.calendar(1:(end-1),:), ...
    'horizon', accum.horizon(1:(end-1),:));

%% Initialization
[f0, G0, S0, YHat, XHat, A] = StockWatsonMF_alt(y(1:(end-1),:), ...
    'accum', accum_struct, 'factors', nFactors);

residuals = (YHat - f0 * G0');

if inputStructure.ar1 == "all" % AR(1) for idiosyncratic errors in all series
    
    arCoefs = nan(height(G0),1);
    arErrs = nan(height(G0),1);
    for iS = 1:length(G0)
      ar_y = residuals(:, iS);
      ar_X = [nan; ar_y(1:end-1)];
      
      warning off stats:regress:NoConst
      [arCoefs(iS), ~, ~, ~, stats] = regress(ar_y, ar_X);
      arErrs(iS) = stats(4);
      warning on stats:regress:NoConst
    
    end
    
    % AR(p) for factor
    [t_beta,t_xx,t_ee] = ols_var(f0(sum(isnan(f0),2)==0, :),nLags,0);
    
    f0 = (1./diag(t_ee)') .* f0;
    G0 = (diag(t_ee)') .* G0;
    
    Z0 = Z;
    Z0(:,1:nFactors) = G0;

    H0 = H;

    T0 = T;
    T0(1:nFactors,1:(nLags*nFactors)) = t_beta';
    for ii = (nFactors*(nLags))+1:nStates
        T0(ii,ii) = arCoefs(ii-(nFactors*(nLags))); % why does this miss the final one? 
    end

    Q0 = Q;

    for ii = 1:nSeries
        Q0(ii+nFactors,ii+nFactors) = arErrs(ii);
    end

    R0 = R;

elseif inputStructure.ar1 == "first"
    % AR(1) for idiosyncratic errors
    ar_y1 = residuals(:, 1);
    ar_X1 = [nan; ar_y1(1:end-1)];
    
    warning off stats:regress:NoConst
    [arCoefs, ~, ~, ~, stats] = regress(ar_y1, ar_X1);
    arErrs = stats(4);
    warning on stats:regress:NoConst
    
    
    % AR(p) for factor
    % we want the shocks to have var of 1, but PCA forces the factors to have
    % that. Let's rescale f0 and G0
    [t_beta,t_xx,t_ee] = ols_var(f0(sum(isnan(f0),2)==0, :),nLags,0);
    
    f0 = (1./diag(t_ee)') .* f0;
    G0 = (diag(t_ee)') .* G0;
    
    Z0 = Z;
    Z0(:,1:nFactors) = G0;

    H0 = H;
    for ii = 2:nSeries
        H0(ii,ii) = S0(ii,ii);
    end
    T0 = T;
    T0(1:nFactors,1:(nLags*nFactors)) = t_beta';
    T0(nStates,nStates) = arCoefs;


    Q0 = Q;
    Q0(nFactors+1, nFactors+1) = arErrs;
    R0 = R;

end

%% Initial values

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

ssOpt = ssOpt.setDefaultInitial();
ssOpt.a0 = zeros(size(ssOpt.a0)); % want to kick it off from
ssOpt.P0 = 100.*eye(size(ssOpt.P0)); % zeros(size(ssOpt.P0)); % diffuse prior

% getting bands
disp('Sampling the states...')
% difference btwn sampling the states and smoothing sample
% issue with positive definite
% [~, mleVariance]  = ssOpt.stateSample(y', [], [], ssEA.ThetaMapping, 1);
tm = ssEA.ThetaMapping;
theta = tm.system2theta(ssOpt);
alphaHat = ssOpt.smooth(y', [], []);
fisherInformation = -ssOpt.gradFinDiffSecond(y', [], [], tm, theta);
mleVariance = inv(fisherInformation * ssOpt.n);

% Check if outer product of gradients is positive semi-definite
try
  chol(mleVariance);
catch
  [q,D] = eig(mleVariance);
  d= diag(D);
  d(d <= eps) = 1e-10;
  mleVariance = q*diag(d)*q';
end

sOut.Theta = ssEA.ThetaMapping.system2theta(ssOpt);
sOut.varTheta = mleVariance;

% add an extra year, at a minimum
yForecast = [y; nan(12, size(y,2))];

accumExtra = Accumulator.GenerateRegular(yForecast, ...
    accumulatorType, ...
    accumulatorN);
ss0AExtra = accumExtra.augmentStateSpace(ss0);


nSamples = 1000;
alphaSim = nan(size(yForecast,1), size(alpha,1) ,nSamples);
sampledY = nan(size(yForecast,1), 1, size(alphaSim,3));
parfor ii = 1:nSamples
    theta = mvnrnd(sOut.Theta, sOut.varTheta, 1)'; %#ok<PFBNS>
    ssFcast = ssEA.ThetaMapping.theta2system(theta); %#ok<PFBNS>
    ssFcast.a0 = ssOpt.a0;
    ssFcast.P0 = ssOpt.P0;

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

ssOptFcast = ssOpt;
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
plot(dataDates, incomeEstimates, "Color","red")
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
plot(dataDates, state, "Color","red")
xlim([datetime("1990-01-01"), datetime("2024-07-01")])
ylim([-5,5])
hold off

mkdir(inputStructure.outputPath)
save(fullfile(inputStructure.outputPath, 'workspace.mat'))

% Note: only use the EOY income estimates! 
newIncomeEstimates = [incomeEstimates; incomeEstimatesFcast(end-11:end,1)];
newState = [state; stateFcast(end-11:end,1)];
yToMatch = yForecast(:,1)*ysigs(:,1) + ymeans(:,1);


runTime = toc;
outputTable = table(newDataDates, newIncomeEstimates, repmat(runTime, size(newDataDates)), repmat(bic, size(newDataDates)), ...
    est5, est10, est16, est25, est50, est75, est84, est90, est95, ...
    newState, stateEst5, stateEst10, stateEst16, stateEst25, stateEst50, stateEst75, stateEst84, stateEst90, stateEst95, ...
    yToMatch, ...
    'VariableNames',["dataDates", "incomeEstimates", "runTime", "bic", ...
    "band5", "band10", "band16", "band25", "band50", "band75", "band84", "band90", "band95", ...
    "state", "stateBand5", "stateBand10", "stateBand16", "stateBand25", "stateBand50", "stateBand75", "stateBand84", "stateBand90", "stateBand95", "incomeToMatch"]);

writetable(outputTable, ...
    fullfile(inputStructure.outputPath, 'output.csv'))

end