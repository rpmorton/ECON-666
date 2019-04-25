function [lognormls] = lognormal_ls(x,var,quantile)
    % Parameter
    %%
    mu = x(1,1);
    sigma2 = x(1,2);
   
   %quantile val
   erf_inv_use = erfinv(2 * quantile - 1);
   quantile_emp = exp(mu + sqrt(2 * sigma2) .* erf_inv_use);
    
   %find sse
   squared_diff = (output - quantile_emp) .* (output - quantile_emp);
   output = sum(squared_diff);
    
    
end