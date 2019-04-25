function [ls] = lognormal(x,var,percentile)
    % Parameter
    
    mu = x(1,1);
    sigma2 = x(1,2);
   
   %quantile val
   erf_use = erf(2 * percentile - 1);
   quantile = exp(mu + sqrt(2 * sigma2) ./ erf_use);
    
   %find sse
   squared_diff = (percentile - quantile) .* (percentile - quantile);
   ls = sum(squared_diff);
    
    
end