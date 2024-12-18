function [beta,XX,ee] = ols_var(ydata,lags,constant_flag)
%% ols_var.m
% ols_var.m is function that will calculate the Ordinary Least Squares
% (OLS) Vector Auto Regression (VAR) for a given set of data, and number of
% lags.  A constant will be added to the regression provided constant_flag
% is turned on, or = 1.  This function will provide as output the estimated
% beta coefficients as well as the exogenous matrix with the lag structure.
%
%
% input:
%    ydata - matrix of data (obs x series)
%    lags - number of lags to be included in the VAR
%    constant_flag - flag denoting presence of constant
%
% ouput:
%    beta - estimated coefficient matrix
%    XX - exogenous matrix with lag structure
%    ee - estimate of the variance-covariance matrix of residuals
%
%
%  % See also: 
%
%  % Copyright: R. Andrew Butters 2009

[obs,nvar] = size(ydata);
 
% set up lag matrix
XX = ones(obs-lags,constant_flag+nvar*lags);

for ii=1:lags
    XX(:,1+nvar*(ii-1):nvar*ii) = ydata(lags-ii+1:end-ii,:);
end
    
Y = ydata(lags+1:end,:);    
    
% OLS estimation
beta = (XX'*XX) \ (XX'*Y);
ee = cov(Y - XX*beta);
end
%% End of File