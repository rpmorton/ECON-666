
/* Created by RM on 2019.04.24 fo power calculations for ECON 666 Proposal and PAP */


clear
set more off

/**********
 Simulate Data to Calculate MDE for Various Outcomes 
**********/

*Recall that under null treatment effect is zero so do not need to hypothesize what it might be

*Get empirical distribution of profits for control firms and treatment firms from Atkin
*Recall that treatment might increase variance!!!

/* 
Outcome vars:
 adoption rates (in terms of percentage of workers and soccer balls made using new method): 0 in control
 (self-reported) profits -- PG. 1125
 quantity of soccer balls sold -- PG. 1125
 labor costs in general -- NO DATA
 labor costs for cutters and for monitoring - PG. 1125
 and rexine costs  
 */
 
 global output "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Proposal and PAP/PowerCalcs/Output"
 
 global iter = 10000
 
forv r = 1(1)$iter { 

di "r is `r'"

clear
 
 set obs 88
 
 g counter = [_n]
 
g group = floor( (counter - 1) / 4) + 1
 
g sat = 1 if group <= 11
replace sat = .5 if group > 11

g treat_group = group <= 17

g win_group_counter = counter - (group - 1) * 4

g microtheory = win_group_counter == 2
g wage = win_group_counter == 3
g fixed = win_group_counter == 4

/* Adoption Rates */
/* Under Null of no Treatment Matters, One Firm Adopted Out of 50: TWO PERCENT
 */
/* Make Average of Two i.i.d. Random Uniform Distributions; if average over .98 then adopt */


/* 
ADOPTION VARS
*/

g pre_group_randuni = runiform() if win_group_counter == 1
bys group: egen group_randuni = max(pre_group_randuni)
drop pre_group_randuni

g ind_randuni = runiform()

g uni_sum = ind_randuni + group_randuni

g adopt = uni_sum >= 1.8

g randuni_adopt = runiform()
g adopt_workers = adopt * randuni_adopt
*time adjustment based on Table A6; assuming get random shock for time
g randuni_learning = runiform()
g adopt_balls = adopt_workers * ( randuni_learning + (1-randuni_learning)*.8254)

/* THIS IS EFFECTIVELY AN ICC OF .5 */



/* 
PROFIT AND COSTS: IF CORRELATED THAN MORE VARIANCE--ASSUME PERFECTLY CORRELATED
AS STRATIFYING ON PROFIT, MAKE MOSTLY GROUP
*/

drop group_randuni ind_randuni uni_sum


g pre_group_randuni = runiform() if win_group_counter == 1
bys group: egen group_randuni = max(pre_group_randuni)
drop pre_group_randuni

g ind_randuni = runiform()

g uni_profit_costs_percentile = .2 * ind_randuni + .8 * group_randuni

/* THIS IS EFFECTIVELY AN ICC OF (.8)^2 /  ( .8^2 + .2^2) = .94 */

/* FROM MATLAB */	
g mu_output = 13.4076031045512
g sigma2_output = 20.8809711962930
g mu_profit_perc = 1.95237061462989	
g sigma2_profit_perc = 0.244312581306080
g mu_price = -1.30209022766048	
g sigma2_price = 0.801916315513598
g mu_cutter_count = -0.204136468363557
g sigma2_cutter_count =	4.64902968152020

g z = invnormal(uni_profit_costs_percentile)

g output_month = exp(mu_output + z*sqrt(sigma2_output) ) * 1000
g profit_perc = exp(mu_profit_perc + z*sqrt(sigma2_profit_perc) )
g price = exp(mu_price + z*sqrt(sigma2_price)) * 1000

g profit_month = output * price * profit_perc

*g cutter_count = exp(mu_cutter_count + z*sqrt(sigma2_cutter_count) )
g cutter_piece_rate = exp(mu_cutter_count + z*sqrt(sigma2_cutter_count) )

g cutter_costs = output_month * cutter_piece_rate

/* THINK OF UNIFORM AS FAIRLY CONSERVATIVE RE VARIANCE IF ACTUALLY CENTRALIZED: LIKELY TO BE LESS VARIED THAN UNIFORM */

g sat_100 = sat > .5
g microtheory_100 = microtheory * sat_100
g wage_100 = wage * sat_100
g fixed_100 = fixed * sat_100

xtset group

g adopt_work = adopt_workers
g prof_mth = profit_month
g output_mth = output_month
g cut_costs = cutter_costs

quietly: su prof_mth, d
local mean_prof_mth_`r' = r(mean)

quietly: su output_mth, d
local mean_output_mth_`r' = r(mean)

quietly: su cut_costs, d
local mean_cut_costs_`r' = r(mean)

g l_prof_mth = log(prof_mth)
g l_output_mth = log(output_mth)
g l_cut_costs = log(cut_costs)

quietly: su l_prof_mth, d
local mean_l_prof_mth_`r' = r(mean)

quietly: su l_output_mth, d
local mean_l_output_mth_`r' = r(mean)

quietly: su l_cut_costs, d
local mean_l_cut_costs_`r' = r(mean)


local outvars = "adopt adopt_work adopt_balls prof_mth output_mth cut_costs l_prof_mth l_output_mth l_cut_costs"

foreach var of local outvars {

quietly: xtreg `var' microtheory wage fixed, fe

	local tau_`var'_mtheory_`r' = _b[microtheory]
	local tau_`var'_wage_`r' = _b[wage]
	local tau_`var'_fixed_`r' = _b[fixed]
	
/*	
quietly: xtreg `var' microtheory wage fixed microtheory_100 wage_100 fixed_100, fe
	local ts_`var'_mtheory_`r' = _b[microtheory]
	local ts_`var'_wage_`r' = _b[wage]
	local ts_`var'_fixed_`r' = _b[fixed]
	local ts_`var'_mtheory100_`r' = _b[microtheory_100]
	local ts_`var'_wage100_`r' = _b[wage_100]
	local ts_`var'_fixed100_`r' = _b[fixed_100]
*/	

}


}

clear
set obs $iter

g counter = [_n]

foreach var of local outvars {

	g tau_`var'_mtheory = 0
	g tau_`var'_wage = 0
	g tau_`var'_fixed = 0
	/*
	g ts_`var'_mtheory = 0
	g ts_`var'_wage = 0
	g ts_`var'_fixed = 0
	g ts_`var'_mtheory100 = 0
	g ts_`var'_wage100 = 0
	g ts_`var'_fixed100 = 0
*/
}	

g mean_prof_mth = 0
g mean_output_mth = 0
g mean_cut_costs = 0
g mean_l_prof_mth = 0
g mean_l_output_mth = 0
g mean_l_cut_costs = 0

forv r = 1(1)$iter {



	foreach var of local outvars {

	replace tau_`var'_mtheory = `tau_`var'_mtheory_`r'' if counter == `r'
	replace tau_`var'_wage = `tau_`var'_wage_`r'' if counter == `r'
	replace tau_`var'_fixed = `tau_`var'_fixed_`r'' if counter == `r'
	

	/*
	replace ts_`var'_mtheory = `ts_`var'_mtheory_`r'' if counter == `r'
	replace ts_`var'_wage = `ts_`var'_wage_`r'' if counter == `r'
	replace ts_`var'_fixed = `ts_`var'_fixed_`r'' if counter == `r'
	replace ts_`var'_mtheory100 = `ts_`var'_mtheory100_`r'' if counter == `r'
	replace ts_`var'_wage100 = `ts_`var'_wage100_`r'' if counter == `r'
	replace ts_`var'_fixed100 = `ts_`var'_fixed100_`r'' if counter == `r'
*/

	}	
		
	replace mean_prof_mth = `mean_prof_mth_`r'' if counter == `r'
	replace mean_output_mth = `mean_output_mth_`r'' if counter == `r'
	replace mean_cut_costs = `mean_cut_costs_`r'' if counter == `r'
	
	replace mean_l_prof_mth = `mean_l_prof_mth_`r'' if counter == `r'
	replace mean_l_output_mth = `mean_l_output_mth_`r'' if counter == `r'
	replace mean_l_cut_costs = `mean_l_cut_costs_`r'' if counter == `r'


}

	
	
	
foreach var of local outvars {

		quietly: su tau_`var'_mtheory
		g sd_tau_`var'_mtheory = r(sd)
		g mean_tau_`var'_mtheory = r(mean)
		quietly: su tau_`var'_wage
		g sd_tau_`var'_wage = r(sd)
		g mean_tau_`var'_wage = r(mean)
		quietly: su tau_`var'_fixed
		g sd_tau_`var'_fixed = r(sd)
		g mean_tau_`var'_fixed = r(mean)

		*di "breaks before diff tau"
		
		quietly: corr tau_`var'_mtheory tau_`var'_wage, covariance
		g sd_dt_`var'_mtheory_wage =  sqrt(r(Var_1) + r(Var_2) - 2 * r(cov_12))
		quietly: corr tau_`var'_mtheory tau_`var'_fixed, covariance
		g sd_dt_`var'_mtheory_fixed =  sqrt(r(Var_1) + r(Var_2) - 2 * r(cov_12))
		quietly: corr tau_`var'_fixed tau_`var'_wage, covariance
		g sd_dt_`var'_wage_fixed =  sqrt(r(Var_1) + r(Var_2) - 2 * r(cov_12))
		*quietly: corr tau_`var'_mtheory tau_`var'_wage, covariance
		*g sd_dt_`var'_all = (sd_dt_`var'_mtheory_wage * sd_dt_`var'_mtheory_wage) + (sd_tau_`var'_fixed * sd_tau_`var'_fixed) 
		*quietly: corr tau_`var'_mtheory tau_`var'_fixed, covariance
		*replace sd_dt_`var'_all = sd_dt_`var'_all - 2 * r(cov_12)
		*quietly: corr tau_`var'_wage tau_`var'_fixed, covariance
		*replace sd_dt_`var'_all = sd_dt_`var'_all - 2 * r(cov_12)
		*replace sd_dt_`var'_all = sqrt(sd_dt_`var'_all + 4 * mean_tau_`var'_fixed * (mean_tau_`var'_mtheory + mean_tau_`var'_wage) )
	
		
		*di "breaks after diff tau"

		
/*		
		quietly: su ts_`var'_mtheory
		g sd_ts_`var'_mtheory = r(sd)
		quietly: su ts_`var'_wage
		g sd_ts_`var'_wage = r(sd)
		quietly: su ts_`var'_fixed
		g sd_ts_`var'_fixed = r(sd)
		quietly: su ts_`var'_mtheory100
		g sd_ts_`var'_mtheory100 = r(sd)
		quietly: su ts_`var'_wage100
		g sd_ts_`var'_wage100 = r(sd)
		quietly: su ts_`var'_fixed100
		g sd_ts_`var'_fixed100 = r(sd)
*/
		
}

quietly: su mean_prof_mth
g avg_mean_prof_mth = r(mean)
g sd_mean_prof_mth = r(sd)

quietly: su mean_output_mth
g avg_mean_output_mth = r(mean)
g sd_mean_output_mth = r(sd)

quietly: su mean_cut_costs
g avg_mean_cut_costs = r(mean)
g sd_mean_cut_costs = r(sd)

quietly: su mean_l_prof_mth
g avg_mean_l_prof_mth = r(mean)
g sd_mean_l_prof_mth = r(sd)

quietly: su mean_l_output_mth
g avg_mean_l_output_mth = r(mean)
g sd_mean_l_output_mth = r(sd)

quietly: su mean_l_cut_costs
g avg_mean_l_cut_costs = r(mean)
g sd_mean_l_cut_costs = r(sd)


keep if counter == 1
keep sd* avg*



export delimited "$output/sd_simulation_power.csv", replace


