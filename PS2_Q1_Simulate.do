
/* Created by RM on 02.16.2018
ECON 666 PS 2
Free Step Down Resampling Multiple Hypothesis Test Corrections
*/

*1:26

clear
set more off

*ssc inst egenmore

mata: mata set matafavor speed

global data "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Data/dataverse_files"
global out "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Output/PS1"
global temp "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Data/Temp"

set seed 666666
set matsize 11000

use "$data/120617main", clear

/* Part C: Replicaton of Main estimate in Table III, column 1, pg. 1248 */

xtset firmid

g treatment_midline = treatment * after1
g treatment_endline = treatment * after2

global outcomes "lnwpart5revenue wnet_profit lnwnum_employee lnwfixasset lnwmaterial lnwenergy_cost wTFP_OLS"

local counter = 1

foreach y of global outcomes {

	xtreg `y' after1 after2 treatment_midline treatment_endline, fe cluster(clusterid)
	
	if `counter' < 1 {
			regsave treatment_midline treatment_endline using "$temp/pvals_step_down", pval addlabel(actual_sim, "Actual", outcome, "`y'") append
	}
	
	if `counter' > 0 {
		regsave treatment_midline treatment_endline using "$temp/pvals_step_down", pval addlabel(actual_sim, "Actual", outcome, "`y'") replace
		local counter = 0
	}
	
	
}

*use "$temp/pvals_step_down", clear

egen count_treat = sum(treatment)

global counttreat = count_treat

g obs = [_n]

capture program drop freestepdown

program freestepdown, rclass
	
	g randvec = runiform(0,1)
	sort randvec
	g fisher_treat = [_n] <= $counttreat		
	g f_treat_midline = fisher_treat * after1
	g f_treat_endline = fisher_treat * after2
	
	foreach y of global outcomes {
		xtreg `y' after1 after2 f_treat_midline f_treat_endline, fe cluster(clusterid)
		return scalar p_mid_`y' =  1 - t(e(df_r),abs(_b[f_treat_midline] / _se[f_treat_midline])) + t(e(df_r),-1 * abs(_b[f_treat_midline] / _se[f_treat_midline]))
		return scalar p_end_`y' =  1 - t(e(df_r),abs(_b[f_treat_endline] / _se[f_treat_endline])) + t(e(df_r),-1 * abs(_b[f_treat_endline] / _se[f_treat_endline]))
	}
	
	drop randvec fisher_treat f_*
	
end

global sims = 20000

simulate p_mid_lnwpart5revenue=r(p_mid_lnwpart5revenue) p_end_lnwpart5revenue = r(p_end_lnwpart5revenue) ///
		p_mid_wnet_profit=r(p_mid_wnet_profit) p_end_wnet_profit = r(p_end_wnet_profit) ///
		p_mid_lnwnum_employee=r(p_mid_lnwnum_employee) p_end_lnwnum_employee = r(p_end_lnwnum_employee) ///
		p_mid_lnwfixasset=r(p_mid_lnwfixasset) p_end_lnwfixasset = r(p_end_lnwfixasset) ///
		p_mid_lnwmaterial=r(p_mid_lnwmaterial) p_end_lnwmaterial = r(p_end_lnwmaterial) ///
		p_mid_lnwenergy_cost=r(p_mid_lnwenergy_cost) p_end_lnwenergy_cost = r(p_end_lnwenergy_cost) ///
		p_mid_wTFP_OLS=r(p_mid_wTFP_OLS) p_end_wTFP_OLS = r(p_end_wTFP_OLS) ///
   , reps($sims): freestepdown
   
save "$temp/simulated_pvals", replace

*Generate Pval Ranks

local treatt "mid end"

foreach tr of local treatt {

use "$temp/pvals_step_down", clear

*Rankings
keep if strpos(var,"`tr'") > 0
g obs = [_n]
egen count_obs = max(obs)

local countvars = count_obs

egen rank_pval = rank(pval)
sort rank_pval

drop obs
g obs = [_n]

g outcome_name = outcome

forv i = 1(1)`countvars' {
	preserve
	keep if obs == `i'
	local p_`tr'_rank_`i' = outcome_name
	restore
	}

forv i = 1(1)`countvars' {
	
	g pre_p_`tr'_`p_`tr'_rank_`i'' = pval if obs == `i'
	egen p_act_`tr'_`p_`tr'_rank_`i'' = max(pre_p_`tr'_`p_`tr'_rank_`i'')
	drop pre*

	}

g tomerge = 1
	
keep p_act_* tomerge

keep if [_n] == 1
	
save "$temp/pvals act `tr'", replace	
	
}

*Enforce Monotonicity

use "$temp/simulated_pvals", clear

foreach tr of local treatt {

forv i = 1(1)`countvars' {
	local j = `countvars' - `i' + 1
	
	if `j' == `countvars' {
		g p_`tr'_`p_`tr'_rank_`j''_mon = p_`tr'_`p_`tr'_rank_`j''
	}
	
	local jplus = `j' + 1
	
	if `j' <  `countvars' {
		g p_`tr'_`p_`tr'_rank_`j''_mon = min(p_`tr'_`p_`tr'_rank_`j'', p_`tr'_`p_`tr'_rank_`jplus''_mon)
	}

}

}

g tomerge = 1

save "$temp/simulated_pvals_mon", replace

local treatt "mid end"

foreach tr of local treatt {

	merge m:1 tomerge using "$temp/pvals act `tr'"
	drop _merge

}

drop tomerge

*Calculate Final P-Value and Enforce Monotonicity

foreach tr of local treatt {

forv i = 1(1)`countvars' {
	local j = `countvars' - `i' + 1
	
	g binary_empirical_p = p_act_`tr'_`p_`tr'_rank_`j'' > p_`tr'_`p_`tr'_rank_`j''_mon
	egen numer_empirical_p = sum(binary_empirical_p)
	g empirical_p_`tr'_`p_`tr'_rank_`j'' = numer_empirical_p / $sims
	
	if `j' == `countvars' {
		g fwer_p_`tr'_`p_`tr'_rank_`j''_mon = empirical_p_`tr'_`p_`tr'_rank_`j''
	}
	
	local jplus = `j' + 1
	
	if `j' <  `countvars' {
		g fwer_p_`tr'_`p_`tr'_rank_`j''_mon = min(empirical_p_`tr'_`p_`tr'_rank_`j'', fwer_p_`tr'_`p_`tr'_rank_`jplus''_mon)
	}
	
	drop binary_empirical* numer_empirical*
}

}

keep fwer*

keep if [_n] == 1

save "$temp/fwer_onefam_results", replace

/****
* Now Divide Into Two Families: Profit and Revenue Versus Inputs and Outputs
***/

global outcomesfam1 "lnwpart5revenue wnet_profit"
global outcomesfam2 "lnwnum_employee lnwfixasset lnwmaterial lnwenergy_cost wTFP_OLS"


local treatt "mid end"

forv f = 1(1)2 {

	foreach tr of local treatt {

	use "$temp/pvals_step_down", clear
	
	*Subset to family
	g in_family = 0
	foreach out of global outcomesfam`f' {
	replace in_family = 1 if outcome == "`out'"
	}
	keep if in_family == 1

	*Rankings
	keep if strpos(var,"`tr'") > 0
	g obs = [_n]
	egen count_obs = max(obs)

	local countvars`f' = count_obs

	egen rank_pval = rank(pval)
	sort rank_pval

	drop obs
	g obs = [_n]

	g outcome_name = outcome

	forv i = 1(1)`countvars`f'' {
		preserve
		keep if obs == `i'
		local p_`tr'_rank_`i'_`f' = outcome_name
		restore
		}

	forv i = 1(1)`countvars`f'' {
		
		g pre_p_`tr'_`p_`tr'_rank_`i'_`f'' = pval if obs == `i'
		egen p_act_`tr'_`p_`tr'_rank_`i'_`f'' = max(pre_p_`tr'_`p_`tr'_rank_`i'_`f'')
		drop pre*

		}

	g tomerge = 1
		
	keep p_act_* tomerge

	keep if [_n] == 1
		
	save "$temp/pvals act `tr' fam `f'", replace	
		
	} 
	
}

*Enforce Monotonicity

use "$temp/simulated_pvals", clear
forv f = 1(1)2 {

	foreach tr of local treatt {
	
		*di "countvars f is `countvars`f'' "

		forv i = 1(1)`countvars`f'' {
			local j = `countvars`f'' - `i' + 1
			
			if `j' == `countvars`f'' {
				g p_`tr'_`p_`tr'_rank_`j'_`f''_mon_`f' = p_`tr'_`p_`tr'_rank_`j'_`f''
			}
			
			local jplus = `j' + 1
			
			if `j' <  `countvars`f'' {
				g p_`tr'_`p_`tr'_rank_`j'_`f''_mon_`f' = min(p_`tr'_`p_`tr'_rank_`j'_`f'', p_`tr'_`p_`tr'_rank_`jplus'_`f''_mon_`f')
			}

		}

	}

}

g tomerge = 1
save "$temp/simulated_pvals_mon_families", replace


local treatt "mid end"

forv f = 1(1)2 {

foreach tr of local treatt {

	merge m:1 tomerge using "$temp/pvals act `tr' fam `f'"
	drop _merge

}

}
drop tomerge

*Calculate Final P-Value and Enforce Monotonicity
forv f = 1(1)2 {

foreach tr of local treatt {

forv i = 1(1)`countvars`f'' {
	local j = `countvars`f'' - `i' + 1
	
	g binary_empirical_p = p_act_`tr'_`p_`tr'_rank_`j'_`f'' > p_`tr'_`p_`tr'_rank_`j'_`f''_mon_`f'
	egen numer_empirical_p = sum(binary_empirical_p)
	g empirical_p_`tr'_`p_`tr'_rank_`j'_`f'' = numer_empirical_p / $sims
	
	if `j' == `countvars`f'' {
		g fwer_p_`tr'_`p_`tr'_rank_`j'_`f''_mon_`f' = empirical_p_`tr'_`p_`tr'_rank_`j'_`f''
	}
	
	local jplus = `j' + 1
	
	if `j' <  `countvars`f'' {
		g fwer_p_`tr'_`p_`tr'_rank_`j'_`f''_mon_`f' = min(empirical_p_`tr'_`p_`tr'_rank_`j'_`f'', fwer_p_`tr'_`p_`tr'_rank_`jplus'_`f''_mon_`f')
	}
	
	drop binary_empirical* numer_empirical*
}

}

}
keep fwer*

keep if [_n] == 1

save "$temp/fwer_twofams_results", replace


//* Part F: FDR Control: Full Families */

use "$temp/pvals_step_down", clear

bys var: egen rank_pval = rank(pval)

g min_q = .

forv preq = 0(.01)1{
g q = 1 - `preq'

g reject_var = 0
g reject_final = .

forv i = 1(1)7 {
	local j = 7 - `i' + 1
	g reject_this_hypot = pval < (q * `j') / 7 & rank_pval == `j'
	bys var: egen pre_reject_var = max(reject_this_hypot)
	replace reject_var = max(pre_reject_var,reject_var)
	replace reject_final = 1 if reject_var == 1 & rank_pval == `j' 
	
	drop reject_this_hypot pre_reject_var
	
	replace min_q = q if reject_final == 1 & rank_pval == `j'
}
	
drop q reject_var reject_final
	
}	

order var outcome min_q

save "$temp/FDR_onefam_results", replace

*FDR: Two Fams

use "$temp/pvals_step_down", clear

g min_q = .

g fam = 2
replace fam = 1 if strpos(outcome,"revenue") > 0
replace fam = 1 if strpos(outcome,"profit") > 0

g ones = 1
bys var fam: egen num_fam = sum(ones)
drop ones

bys var fam: egen rank_pval = rank(pval)

forv preq = 0(.01)1{
g q = 1 - `preq'

g reject_var = 0
g reject_final = .

forv i = 1(1)5 {
	g j = num_fam - `i' + 1
	g reject_this_hypot = pval < (q * j) / num_fam & rank_pval == j
	bys var fam: egen pre_reject_var = max(reject_this_hypot)
	replace reject_var = max(pre_reject_var,reject_var)
	replace reject_final = 1 if reject_var == 1 & rank_pval == j
	
	replace min_q = q if reject_final == 1 & rank_pval == j
	
	drop reject_this_hypot pre_reject_var j
	
}
	
drop q reject_var reject_final
	
}	


order var outcome min_q

save "$temp/FDR_onefam_results", replace
