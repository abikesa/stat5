
use http://www.stata-press.com/data/mlmus3/sex, clear
set scheme s1mono
* side-by-side boxplots
graph box kscore, over(school) ylab(-8(2)8, angle(horizontal)) ytitle(Knowledge Score)
* Calculate the total and within school variance
anova kscore school
* Fit the random intercept model with main effect for intervention
mixed kscore arm || school:
* Ignore the clustering and fit as independence model
regress kscore arm
* Save the dataset
save "sex.dta", replace


** Simulate a case with larger within school variance:
** The total variance is roughly 5.5
** Assign the within school variance to 2

* Clear all the data
clear all
* Create an indicator for the 25 schools
set obs 25
gen school = _n
* Define the intervention indicator
gen arm = 1 in 1/13
replace arm = 0 in 14/25
* Set the random number seed
set seed 31813
* Generate the random intercept for each school
* We will assume the between school mean variance is 3.5
gen ri = invnorm(runiform())*sqrt(3.5)
* Generate the mean knowledge score for each school
* based on the observed data fit above
gen mean_kscore_cluster = 4 + ri + 0.5*arm
* Save the data 
sort school
save "formerge.dta", replace

** Merge the new school mean onto the original data
use "sex.dta", clear
merge m:1 school using "formerge"
* Generate the individual student scores
* assuming the within school variance is 2
gen kscore_cluster = mean_kscore_cluster + invnorm(runiform())*sqrt(2)
* Graph the data
* NOTE:  this simulated knowledge score is not an integer okay for illustration purposes
set scheme s1mono
graph box kscore_cluster, over(school) ylab(-8(2)11, angle(horizontal)) ytitle(Knowledge Score)
* Get the estimates of total variance and within school variance
anova kscore_cluster school
* fit the random intercept model for estimating intervention effect
mixed kscore_cluster arm || school: 
* Ignore the clustering and assume independence
regress kscore_cluster arm

