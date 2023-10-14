** Create a dataset with 3 hospitals
** Patient case mix is measured as a z-score
** Hospital 1 has average case mix of -1
** Hospital 2 has average case mix of 0
** Hospital 3 has average case mix of 1.5
** We sample 30 patients from each of the three hospitals

clear
set scheme s1mono
set seed 4321

set obs 90
gen hospid = 1 in 1/30
replace hospid = 2 in 31/60
replace hospid = 3 in 61/90
gen patientid = _n
bys hospid: gen within_hosp_counter = _n

gen casemix = rnormal(0,1)
replace casemix = casemix - 1 if hospid == 1
replace casemix = casemix + 1.5 if hospid == 3
save lab2data, replace

** Part I: Total = between = within
use lab2data, clear 
* Create calculated variables
bys hospid: egen mean_casemix = mean(casemix)
gen centered_casemix = casemix - mean_casemix
egen grand_mean = mean(casemix)
gen grand_mean_centered_casemix = casemix - grand_mean
* Assume the total variance in Y is 1 and the ICC is 0.4
gen junk = rnormal(0,sqrt(0.4)) if within_hosp_counter == 1
bys hospid: egen b0i = min(junk)
* Generate Y
gen Y = 2 + b0i - 1 * centered_casemix  - 1 * mean_casemix + rnormal(0,sqrt(0.6))
* Create a graph of the observed data
twoway (scatter Y casemix if hospid==1, ms(o) mc(red) ///
ylab(-2(2)6, angle(horizontal)) ytitle(Patient Satisfaction) ///
xtitle(Patient Case Mix) legend(pos(1) ring(0) col(1) ///
label(1 "Hospital 1") label(2 "Hospital 2") label(3 "Hospital 3")  ///
region(lstyle(none))) xlab(-4(2)6)) ///
(scatter Y casemix if hospid==2, ms(o) mc(blue)) ///
(scatter Y casemix if hospid==3, ms(o) mc(green))
graph save total, replace
* Create a grand-mean centered figure
twoway (scatter Y grand_mean_centered_casemix if hospid==1, ms(o) mc(red) ///
ylab(-2(2)6, angle(horizontal)) ytitle(Patient Satisfaction) ///
xtitle(Grand Mean Centered Patient Case Mix) legend(off) xlab(-4(2)4)) ///
(scatter Y grand_mean_centered_casemix if hospid==2, ms(o) mc(blue)) ///
(scatter Y grand_mean_centered_casemix if hospid==3, ms(o) mc(green))
graph save grandmean, replace
* Generate the mean Y by hospital and diff in Y and mean Y within 
bys hospid: egen meanY = mean(Y)
gen centered_Y = Y - meanY
* Plot the between effects
twoway (scatter meanY mean_casemix if hospid==1, ms(o) mc(red) ///
ylab(-2(2)4, angle(horizontal)) ytitle(Average Patient Satisfaction) ///
xtitle(Average Patient Case Mix) ///
title(Between Hospitals) xlab(-2(2)2) legend(off)) ///
(scatter meanY mean_casemix if hospid==2, ms(o) mc(blue)) ///
(scatter meanY mean_casemix if hospid==3, ms(o) mc(green)) ///
(lfit meanY mean_casemix, lc(gray))
graph save between, replace
* Plot the within effects
twoway (scatter centered_Y centered_casemix if hospid==1, ms(o) mc(red) ///
ylab(-6(2)4, angle(horizontal)) ytitle(Within Hospital Difference in Y) ///
xtitle(Within Hospital Diff in Casemix) ///
title(Within Hospitals) xlab(-4(2)4) legend(off)) ///
(scatter centered_Y centered_casemix if hospid==2, ms(o) mc(blue)) ///
(scatter centered_Y centered_casemix if hospid==3, ms(o) mc(green)) ///
(lfit centered_Y centered_casemix, lc(black))
graph save within, replace
* Combine all three graphs into one graph
graph combine "total" "grandmean" "between" "within", row(2) col(2)
* Fit the models;
* Estimate the between, within, contextual and total effects
mixed Y mean_casemix centered_casemix || hospid: 
lincom mean_casemix - centered_casemix
mixed Y casemix || hospid:



