%Created by RM on 2019.04.24 for Proposal/PAP
%Find least squares fit, equivalent to GMM

%%
%Import Data

data = csvread('/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Proposal and PAP/PowerCalcs/Output/dataskewnormal.csv',1,0);

quantile = data(:,2);
output_1000 = data(:,3);
profit_perc = data(:,4);
price = data(:,5);

%%
%Fit Skew Normal: Output

mu_hat = 31 ;
sigma2_hat = 25;

x_hat = [mu_hat sigma2_hat];

output = output_1000 / 1000;

options  =  optimset('GradObj','off','LargeScale','off','Display','iter','TolFun',1e-16,'TolX',1e-16,'Diagnostics','on','MaxFunEvals',200000,'MaxIter',1000); 
[estimate_output] = fminunc(@(x) lognormleastsquares(x,output,quantile),x_hat,options);


