function [dataDates, incomeEstimates, bic, hqic] = estimate_mfvar_model(inputStructure)

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


accumulatorType = dataSpec.accumulator_name;
accumulatorN = dataSpec.accumulator_n;
accumulatorN(ismissing(accumulatorType)) = 1;

accum = Accumulator.GenerateRegular(y, ...
    accumulatorType, ...
    accumulatorN);

mfvar_model = MFVAR(y, nLags, accum, 'alpha0', y);
mfvar_opt = mfvar_model.estimate();
[alpha, sOut] = mfvar_opt.smooth(y);


modelThetaMapping = mfvar_model.generateTM();
bic = modelThetaMapping.nTheta*log(mfvar_opt.n) - 2*sOut.logli;
hqic = -2*sOut.logli + 2*modelThetaMapping.nTheta*log(log(mfvar_opt.n));


state = alpha(1,:)';
smoothedY = mfvar_opt.Z * alpha';
incomeEstimates = smoothedY(1,:)';

incomeEstimates = incomeEstimates*ysigs(:,1) + ymeans(:,1);
mmIncomeEstimates = cumsum((alpha(1,:).*ysigs(:,1) + ymeans(:,1)/12));

mkdir(inputStructure.outputPath)
save(fullfile(inputStructure.outputPath, 'workspace.mat'))

runTime = toc;
outputTable = table(dataDates, incomeEstimates, repmat(runTime, size(dataDates)), repmat(bic, size(dataDates)), ...
    'VariableNames',["dataDates", "incomeEstimates", "runTime", "bic"]);

writetable(outputTable, ...
    fullfile(inputStructure.outputPath, 'output.csv'))

end