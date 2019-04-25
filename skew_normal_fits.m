%Created by RM on 2019.04.24 for Proposal/PAP
%Find least squares fit, equivalent to GMM

%%
%Import Data

data = csvread('/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Proposal and PAP/PowerCalcs/Output/dataskewnormal.csv',1,0);

percentile = data(:,2);
output = data(:,3);
profit_perc = data(:,4);
price = data(:,5);

%%
%Fit Skew Normal: Output

location_hat = 40 * 1000;
scale_hat = 25000;
shape_hat = 50000;

x_hat = [location_hat scale_hat shape_hat];

options  =  optimset('GradObj','off','LargeScale','off','Display','iter','TolFun',1e-16,'TolX',1e-16,'Diagnostics','on','MaxFunEvals',200000,'MaxIter',1000); 
[estimate_output] = fminunc(@(x)least_squares(x,output,percentile),x_hat,options);
%[estimate_entryexit,log_like,exitflag,output,Gradient,Hessian] = fminunc(@(x)llnfxp(x,mle_data,beta,state_probs),x0,options);

