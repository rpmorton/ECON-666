


/* Created by RM on 2019.04.24 fo power calculations for ECON 666 Proposal and PAP */

/**********
 GMM Calculations
**********/

/* For simplicity, only use training balls */


clear
set more off

global output "/Users/russellmorton/Desktop/Coursework/Winter 2019/ECON 666/Proposal and PAP/PowerCalcs/Output"

set obs 7

g counter = [_n]

g quant = .01 if counter == 1
replace quant = .1 if counter == 2
replace quant = .25 if counter == 3
replace quant = .5 if counter == 4
replace quant = .75 if counter == 5
replace quant = .9 if counter == 6
replace quant = .99 if counter == 7


g output = .04 * 1000 if counter == 1
replace output = 1.6 * 1000 if counter == 2
replace output = 4.6 * 1000  if counter == 3
replace output = 15.2 * 1000 if counter == 4
replace output = 37 * 1000 if counter == 5
replace output = 86.2 * 1000 if counter == 6
replace output = 275 * 1000 if counter == 7

g profit_perc = 1.6 if counter == 1
replace profit_perc = 3.2 if counter == 2
replace profit_perc = 5.0   if counter == 3
replace profit_perc = 8.0  if counter == 4
replace profit_perc = 10. if counter == 5
replace profit_perc = 13.0  if counter == 6
replace profit_perc = 22.2  if counter == 7

g price = 200 if counter == 1
replace price = 275 if counter == 2
replace price = 316.7  if counter == 3
replace price = 392.5 if counter == 4
replace price = 490 if counter == 5
replace price = 600  if counter == 6
replace price = 2250 if counter == 7

g cutter_count = .7 if counter == 1
replace cutter_count = 1 if counter == 2
replace cutter_count = 1.2 if counter == 3
replace cutter_count = 2.4 if counter == 4
replace cutter_count = 5 if counter == 5
replace cutter_count = 12 if counter == 6
replace cutter_count = 123 if counter == 7

g cutter_piece_rate = 1 if counter == 1
replace cutter_piece_rate = 1.1 if counter == 2
replace cutter_piece_rate = 1.3 if counter == 3
replace cutter_piece_rate = 1.5 if counter == 4
replace cutter_piece_rate = 1.5 if counter == 5
replace cutter_piece_rate = 1.7 if counter == 6
replace cutter_piece_rate = 2.1 if counter == 7


export delimited "$output/dataskewnormal.csv", replace



