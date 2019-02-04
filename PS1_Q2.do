/* Created by RM on 01.29.19
For ECON 666 PS 1 Q2 */

clear
set more off

global data "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Data/dataverse_files"
global  out "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Output/PS1"

set seed 666666

use "$data/120617main", clear

/* Part C: Replicaton of Main estimate in Table III, column 1, pg. 1248 */

xtset firmid

g treatment_midline = treatment * after1
g treatment_endline = treatment * after2

xtreg lnwpart5revenue after1 after2 treatment_midline treatment_endline, fe cluster(clusterid)

/* Part D: No Covariates */

reg lnwpart5revenue treatment_midline treatment_endline, cluster(clusterid)

xtreg lnwpart5revenue treatment_midline treatment_endline, fe cluster(clusterid)

/* Investigate increase in treatment effect */

g baseline = 0
replace baseline = 1 if after1 != 1 & after2 != 1

reg lnwpart5revenue treatment if baseline == 1, cluster(clusterid)

bys firmid: egen baseline_rev = max(lnwpart5revenue * baseline)

reg lnwpart5revenue treatment_midline treatment_endline baseline_rev, cluster(clusterid)


/* Part E: Add Covariate */

* Get data for calculating covariance
g dep_sq = lnwpart5revenue ^ 2
su dep_sq
su lnwpart5revenue

g covar_dep = .491 * lnwpart5revenue + rnormal(0,1)

corr covar_dep lnwpart5revenue

reg lnwpart5revenue  treatment_midline treatment_endline covar_dep, cluster(clusterid)


/* Part F: Adding Covariate After Removing Treatment Effect */

reg lnwpart5revenue  treatment_midline treatment_endline 
predict resid, resid

g resid_sq = resid^2
su resid_sq
su resid

g covar_dep_indeptreat = .491 * resid + rnormal(0,1)

corr covar_dep_indeptreat resid

reg lnwpart5revenue  treatment_midline treatment_endline covar_dep_indeptreat, cluster(clusterid)


/* Part G: Fisherian Inference */

*First Store Actual Results

xtreg lnwpart5revenue after1 after2 treatment_midline treatment_endline, fe cluster(clusterid)

g beta_treat_midline = _b[treatment_midline]
g beta_treat_endline = _b[treatment_endline]

matrix var = e(V)

g t_treat_midline = beta_treat_midline / (sqrt(var[3,3]))
g t_treat_endline = beta_treat_endline / (sqrt(var[4,4]))

*Now do randomization inference

egen count_treat = sum(treatment)

global counttreat = count_treat

global sims = 5000

g fisher_beta_treat_midline = .
g fisher_t_treat_midline = .
g fisher_beta_treat_endline = .
g fisher_t_treat_endline = .

g obs = [_n]

forv i = 1(1) $sims {

	*load in the actual results as the first randomization to gaurantee that it is included
	if `i' < 2 {
	
		replace fisher_beta_treat_midline = beta_treat_midline if obs == `i'
		replace fisher_beta_treat_endline = beta_treat_endline if obs == `i'
		replace fisher_t_treat_midline = t_treat_midline if obs == `i'
		replace fisher_t_treat_endline = t_treat_endline if obs == `i'
		
	}
	
	*subsequent randomizations: permutation test by generating a random number and sorting
	if `i' > 1 {
		
		preserve 
			
			g randvec = runiform(0,1)
			sort randvec
			g fisher_treat = [_n] <= $counttreat
			
			g fisher_treat_midline = fisher_treat * after1
			g fisher_treat_endline = fisher_treat * after2
			
			xtreg lnwpart5revenue after1 after2 fisher_treat_midline fisher_treat_endline, fe cluster(clusterid)
			matrix beta`i' = e(B)
			matrix var`i' = e(V)
		
		restore
		
		replace fisher_beta_treat_midline = _b[fisher_treat_midline] if obs == `i'
		replace fisher_beta_treat_endline = _b[fisher_treat_endline] if obs == `i'
		replace fisher_t_treat_midline = fisher_beta_treat_midline / (sqrt(var`i'[3,3])) if obs == `i'
		replace fisher_t_treat_endline = fisher_beta_treat_endline / (sqrt(var`i'[4,4])) if obs == `i'

	}
	
	}

* Now Calculate p-values

foreach stat in beta t {
	
	capture drop pr*
	
	di "stat is `stat'"
	
	g pre_pval_midline = abs(fisher_`stat'_treat_midline) >= abs(`stat'_treat_midline) & obs <= $sims
	g pre_pval_endline = abs(fisher_`stat'_treat_endline) >= abs(`stat'_treat_endline) & obs <= $sims
	
	egen pval_`stat'_midline = sum(pre_pval_midline / $sims )
	tostring(pval_`stat'_midline), g(str_pval_`stat'_midline) force
	egen pval_`stat'_endline = sum(pre_pval_endline / $sims )
	tostring(pval_`stat'_endline), g(str_pval_`stat'_endline) force
	
}


**BETAS: MIDLINE AND ENDLINE

g sims = $sims
tostring(sims), replace
local strsims = sims
	
local act_beta_midline = beta_treat_midline
local pval_beta_midline = substr(str_pval_beta_midline,1,4) 	
	
graph twoway (hist fisher_beta_treat_midline, color(eltblue) fintensity(inten80) ) ///
	(scatteri 0 `act_beta_midline' 15 `act_beta_midline', lcolor(maroon) recast(line) ///
	legend(off) title("Randomization Distribution of One Year Treatment Effect") xtitle("Value")   ///
	note("Note: Exact p-value is `pval_beta_midline' using `strsims' random assignment vectors.") )
graph export "$out/PS1_Q2g_BetaMidline.pdf", as (pdf) replace
	
	
local act_beta_endline = beta_treat_endline
local pval_beta_endline =  substr(str_pval_beta_endline,1,4) 	

graph twoway (hist fisher_beta_treat_endline, color(eltblue) fintensity(inten80) ) ///
	(scatteri 0 `act_beta_endline' 12 `act_beta_endline', lcolor(maroon) recast(line) ///
	legend(off) title("Randomization Distribution of Two Years Treatment Effect") xtitle("Value")  /// 
	note("Note: Exact p-value is `pval_beta_endline' using `strsims' random assignment vectors.") )
graph export "$out/PS1_Q2g_BetaEndline.pdf", as (pdf) replace
	
	
**T-STATS: MIDLINE AND ENDLINE

local act_t_midline = t_treat_midline
local pval_t_midline =  substr(str_pval_t_midline,1,4) 	

graph twoway (hist fisher_t_treat_midline, color(eltblue) fintensity(inten80) ) ///
	(scatteri 0 `act_t_midline' .4 `act_t_midline', lcolor(maroon) recast(line) ///
	legend(off) title("Randomization Distribution of One Year Treatment t-Stat") xtitle("Value")  /// 
	note("Note: Exact p-value is `pval_t_midline' using `strsims' random assignment vectors.") )
graph export "$out/PS1_Q2g_TMidline.pdf", as (pdf) replace


local act_t_endline = t_treat_endline
local pval_t_endline =  substr(str_pval_t_endline,1,4) 	

graph twoway (hist fisher_t_treat_endline, color(eltblue) fintensity(inten80) ) ///
	(scatteri 0 `act_t_endline' .4 `act_t_endline', lcolor(maroon) recast(line) ///
	legend(off) title("Randomization Distribution of Two Years Treatment t-Stat") xtitle("Value")  /// 
	note("Note: Exact p-value is `pval_t_endline' using `strsims' random assignment vectors.") )
graph export "$out/PS1_Q2g_TEndline.pdf", as (pdf) replace

