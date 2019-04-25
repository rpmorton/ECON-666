/* Created by RM on 02.16.2018
ECON 666 PS 2
Power Calculations */


clear
set more off

global  out "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Heller/Problem Sets/Problem Set Output/PS2"


**Part B

set obs 1
g obs = 1
g tildet = 0

local on = 1
local tildet = 10

while `on' > 0 {

	capture drop power on_update
	g power =  normal(-1.96 - `tildet' ) - normal(1.96 - `tildet') + 1
	g on_update = power >= .8
	local on = on_update
	
	replace tildet = `tildet'
	
	if `on' > 0 {
		local tildet = `tildet' - .01
	}
	
}

su tildet

/* Validate Using Sampsi Command */
sampsi 10 13, p(.8) alpha(.05) sd(5)


**Part C
g tildet_act = 3 / (sqrt(100/200))
g power_act = 1 - normal(1.96 - tildet_act) + normal(-1.96 - tildet_act)
su power_act

/* Validate Using Power Command */
power twomeans 10 13, sd(5) n(200) alpha(0.05) nratio(1)

clear
set obs 2001
g obs = [_n]
g tau = (obs-1001) / 1000 * 6
g power = .

replace power = 1 - normal(1.96 - tau / sqrt(.5)) + normal(-1.96 - tau / sqrt(.5) )

graph twoway (scatter power tau, color(edklue) msize(vsmall) ///
	legend(off) title("Power Against H0 = 0 for Effect Sizes") xtitle("Effect Size") ytitle("Power") ///
	note("Note: Power is calculated using a std. dev. of 5, size of .05, N of 200, and 50% assignment to treatment.") )


graph export "$out/PS2_Q2c_PowerSampleSize.pdf", as (pdf) replace	
	


**Part D
/* Validate Using Sampsi Command */

sampsi 10 11.5, p(.8) alpha(.05) sd(5)
sampsi 10 11.5, p(.8) alpha(.05) sd(6)

**Part E

clear
set obs 2501
g obs = [_n]
g N = obs + 499
g MDE = (invnormal(.8) + invnormal(.975)) * sqrt(100/N)
*Appears already in units of potatoes, not in units of SD of potatoes
*g MDE_outcome = MDE * 5

/* Validate Using Power */
g MDE_outcome_power = .

forv i = 1(1)2501 {
	local j = `i' + 499
	power twomeans 10, n(`j') sd(5) alpha(0.05) nratio(1) power(.8)
	replace MDE_outcome_power = r(delta) if obs == `i'
	}

graph twoway (scatter MDE N, color(edklue) msize(vsmall) ///
	legend(off) title("MDE Given Sample Size") xtitle("Sample Size") ytitle("MDE") ///
	note("Note: MDE is calculated using a standard deviation of 5, size of .05, and 50% assignment to treatment.") )


graph export "$out/PS2_Q2e_MDESampleSize.pdf", as (pdf) replace	
	
	
**Part G

clear
set obs 1000
g obs = [_n]

*For initial estimates, put 80 units in control and figure out the rest; iterate on number in control!
g num_ctrl = .
g num_tech = .
g num_info = .
g cost = 10000000000

g num_ctrl_best = .
g num_tech_best = .
g num_info_best = .
g cost_best = 10000000000

local counter = 1
local costbest = 10000000000

forv nctr = 110(1)500 {

	replace num_ctrl = `nctr' if obs == `counter'

	power twomeans 10 14.5, n1(`nctr') sd(5) compute(N2) power(.8)
	replace num_tech = r(N2) if obs == `counter'
	local ntech = r(N2)
	di "ntech is `ntech'"

	power twomeans 10 11.5, n1(`nctr') sd(6) compute(N2) power(.5)
	replace num_info = r(N2) if obs == `counter'
	local ninfo = r(N2)
	di "ninfo is `ninfo'"


	local cost = `nctr' + 2 * `ninfo' + 10 * `ntech'
	replace cost = `cost' if obs == `counter'
	
	if `cost' < `costbest' {
		replace num_ctrl_best = `nctr' 
		replace num_tech_best = `ntech' 
		replace num_info_best = `ninfo'
		replace cost_best = `cost' 
		
		local costbest = `cost'
		
	}
	
	local counter = `counter' + 1

}


twoway (scatter cost num_ctrl if cost < 1000, msize(vsmall))

**Part H

global reps = 10000

forv r = 1(1) $reps {

clear
set obs 3000

*Simulate Output
g output = rnormal(10,5)

*Assign Villages
g obs = [_n]
g large_village = floor( (obs ) /500) + 1
g small_village = floor( (obs - 1500) / 50) + 4


g village = large_village if obs < 1501
replace village = small_village if obs > 1500

replace village = 1 if village == 34
*tab village

g village_type = "Large" 
replace village_type = "Small" if village > 3

*Randomize Treatment
g randuni = runiform()
sort village randuni

g randobs = [_n] 

g unit_num = randobs - (village - 1) * 500  if village_type == "Large"
replace unit_num = randobs if randobs < unit_num
replace unit_num = randobs - 1500 - (village - 1) * 50 + 150 if village_type == "Small"

g treat = 0
replace treat = 1 if village_type == "Large" & unit_num <= 150
replace treat = 1 if village_type == "Small" & unit_num <= 35

xtset village
quietly: xtreg output treat, fe

local tauhat`r' = _b[treat]

}

clear
set obs $reps

g tauhat = .
g obs = [_n]

forv r = 1(1) $reps {
	replace tauhat = `tauhat`r'' if obs == `r'
}

su tauhat  

**Part I

global reps = 10000

forv r = 1(1) $reps {

clear
set obs 3000

*Assign Villages
g obs = [_n]
g large_village = floor( (obs ) /500) + 1
g small_village = floor( (obs - 1500) / 50) + 4


g village = large_village if obs < 1501
replace village = small_village if obs > 1500

replace village = 1 if village == 34
*tab village

g village_type = "Large" 
replace village_type = "Small" if village > 3

*Randomize Treatment
g randuni = runiform()
sort village randuni

g randobs = [_n] 

g unit_num = randobs - (village - 1) * 500  if village_type == "Large"
replace unit_num = randobs if randobs < unit_num
replace unit_num = randobs - 1500 - (village - 1) * 50 + 150 if village_type == "Small"

g treat = 0
replace treat = 1 if village_type == "Large" & unit_num <= 150
replace treat = 1 if village_type == "Small" & unit_num <= 35

*Generate Output
g re = rnormal(10,sqrt(8))
g re_first = re if unit_num == 1
bys village: egen re_village = max(re_first)
drop re re_first

g output = re_village + rnormal(0,sqrt(25-8))

xtset village
quietly: xtreg output treat, fe 

local tauhaticc`r' = _b[treat]

}

clear
set obs $reps

g tauhaticc = .
g obs = [_n]

forv r = 1(1) $reps {
	replace tauhaticc = `tauhaticc`r'' if obs == `r'
}

su tauhaticc






