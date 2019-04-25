function [ls] = least_squares(x,var,percentile)
    % Parameter
    
    location = x(1,1);
    scale = x(1,2);
    shape = x(1,3);
    
   
    var_minus_loc_over_scale = (var - location) / scale;
    
   %integrate 
   owent =  @(y) ( exp( -.5 * var_minus_loc_over_scale .* var_minus_loc_over_scale * (1+ y * y) ) / (1 + y * y) );
   integral_owent = quadv(owent,0,shape);
   
   %
   cdf_val =  normcdf(var_minus_loc_over_scale) - (1 / pi) * integral_owent;
   
   %find sse
   squared_diff = (percentile - cdf_val) .* (percentile - cdf_val);
   ls = sum(squared_diff);
    
    
end;