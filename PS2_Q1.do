
/* Created by RM on 02.16.2018
ECON 666 PS 2
Free Step Down Resampling Multiple Hypothesis Test Corrections
*/

clear
set more off

ssc inst egenmore

mata: mata set matafavor speed

global data "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Data/dataverse_files"
global out "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Output/PS1"
global temp "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Data/Temp"

set seed 666666

use "$data/120617main", clear

/* Part C: Replicaton of Main estimate in Table III, column 1, pg. 1248 */

xtset firmid

g treatment_midline = treatment * after1
g treatment_endline = treatment * after2

local outcomes "lnwpart5revenue wnet_profit lnwnum_employee lnwfixasset lnwmaterial lnwenergy_cost wTFP_OLS"



local counter = 1

foreach y of local outcomes {

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



global sims = 500


forv i = 1(1) $sims {

	g randvec = runiform(0,1)
	sort randvec
	g fisher_treat = [_n] <= $counttreat
				
	g fisher_treat_midline = fisher_treat * after1
	g fisher_treat_endline = fisher_treat * after2
	
	foreach y of local outcomes {

		quietly: xtreg `y' after1 after2 fisher_treat_midline fisher_treat_endline, fe cluster(clusterid)
		quietly: regsave fisher_treat_midline fisher_treat_endline using "$temp/pvals_step_down", pval addlabel(actual_sim, "Sim", outcome, "`y'", simcount, `i') append

		
	}
	
	drop randvec fisher_treat fisher_treat_midline fisher_treat_endline
	
}


*Generate Pval Ranks

use "$temp/pvals_step_down", clear

keep if strpos(var,"fisher") == 0

bys var: egen rank_pval = rank(pval)

g pval_actual = pval

keep var rank_pval outcome pval_actual

rename var merge_var

save "$temp/step_down_rank_pvals", replace

use "$temp/pvals_step_down", clear

g merge_var = var if strpos(var,"fisher") == 0
replace merge_var = substr(var,8,length(var) - 7) if merge_var == ""
replace merge_var = regexr(merge_var,"treat_","treatment_")

merge m:1 merge_var outcome using "$temp/step_down_rank_pvals"

drop _merge merge_var

***Enforce Monotonicity

*Count of Outcomes

egen count_outcomes = nvals(outcome)

local countoutcomes = count_outcomes - 1
*di "countoutcomes is `countoutcomes'"
g monotonic_pval = pval if rank_pval == count_outcomes
replace monotonic_pval = 1 if monotonic_pval == .

forv i = 1(1)`countoutcomes' {
	local j = `countoutcomes' - `i' + 1
	
	*di "j is `j' "
	
	g pre_iter_monotonic_pval = monotonic_pval if rank_pval == `j' + 1
	bys sim var: egen iter_monotonic_pval = max(pre_iter_monotonic_pval) 
	replace monotonic_pval = min(pval, iter_monotonic_pval) if rank_pval == `j'	
	
	drop pre_iter_monotonic_pval iter_monotonic_pval

}

*Compute S_r

drop if sim == .

g pre_S_r = monotonic_pval < pval_actual
bys var rank_pval: egen S_r = sum(pre_S_r)
g pre_p_fwer = S_r / $sims

g monotonic_p_fwer = pre_p_fwer if rank_pval == count_outcomes


forv i = 1(1)`countoutcomes' {
	local j = `countoutcomes' - `i' + 1
	
	*di "j is `j' "
	
	g pre_iter_monotonic_pval = monotonic_p_fwer if rank_pval == `j' + 1
	bys sim var: egen iter_monotonic_pval = max(pre_iter_monotonic_pval) 
	replace monotonic_p_fwer = min(pre_p_fwer, iter_monotonic_pval) if rank_pval == `j'	
	
	drop pre_iter_monotonic_pval iter_monotonic_pval

}


sort sim var rank_pval


keep if sim == 1

save "$temp/results_large_family"




