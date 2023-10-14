* Change the working directory

cd "C:\Users\Elizabeth\Documents\MLM2014"

* Set the scheme for the plots

set scheme s1mono

* Open the dataset

use "gcse_2014.dta", clear

* Create some new variables

sort school student

* Generate the number of students within each school
by school: egen totalstudents = count(student)
* Generate a counter for the number of students within each school
by school: gen withinschoolcount = _n

* EDA

* How many total subjects in the dataset
describe

* What is the distribution of number of students in each school
summ totalstudents if withinschoolcount==1

* What is the distribution of the gcse and lrt scores
summ gcse
summ lrt

* What is the relationship between gcse and lrt for few schools
twoway (scatter gcse lrt if school==1, legend(off) xlab(30(10)100) ylab(30(10)100,angle(horizontal)) ytitle("GCSE score") xtitle("LRT score")) (lfit gcse lrt if school==1, lc(blue))
graph save figure1, replace
twoway (scatter gcse lrt if school==2, legend(off) xlab(30(10)100) ylab(30(10)100,angle(horizontal)) xtitle("LRT score")) (lfit gcse lrt if school==2, lc(red))
graph save figure2, replace
graph combine "figure1" "figure2", rows(1)


* Create SLR of gcse on lrt for each school and plot the fitted lines
* If you have Stata 10:
* xi: quietly regress gcse_c i.school*lrt_c
* If you have Stata 11 or more recent
bys school: egen mean_lrt = mean(lrt)
gen lrt_groupc = lrt - mean_lrt
quietly regress gcse i.school lrt_groupc i.school#c.lrt_groupc if totalstudents>5
predict mle
sort school lrt_groupc
xtset school
twoway scatter mle lrt_groupc if totalstudents>5, c(L) ms(i) ytitle(Predicted GCSE score) xtitle(LRT score (School mean centered)) xlabel(-40(10)40) ylabel(30(10)100, angle(horizontal))

** Create the same graph we saw in Lecture 3 (between and within effects on same graph)

* Start by decomposing the information in LRT into within and between.

gen lrt_within = lrt - mean_lrt

* Create a the mean gcse across schools, to quantify the between effect

bys school: egen mean_gcse = mean(gcse)

* Create the smooth between and within effects

lowess gcse lrt_within, gen(gcse_within_mean)
lowess mean_gcse mean_lrt, gen(gcse_between_mean)

* Overlay the between and within group effects on the same graph
gen lrt_scaled = lrt_within + 60
twoway (scatter gcse lrt, ms(o) msize(vsmall) legend(off) ylab(20(10)100,angle(horizontal)) xlab(20(10)100) xtitle("LRT score") ytitle("GCSE score")) ///
(scatter mean_gcse mean_lrt,ms(o) mc(blue)) (lfit mean_gcse mean_lrt if withinschoolcount==1, sort lc(blue)) (lfit gcse lrt_scaled, sort lc(red))

****************************************
** Possible models
****************************************
** NOTE:  If you are using <= Stat 12
** then replace "mixed" with "xtmixed"
****************************************


***** MODEL 1

mixed gcse lrt || school:

***** MODEL 2

mixed gcse lrt mean_lrt || school:

***** MODEL 3

mixed gcse lrt_within mean_lrt || school:

***** MODEL 4

mixed gcse lrt_within || school:

***** MODEL 5

mixed gcse mean_lrt || school:

*************************************
** Interpretation of Models 4 and 5
*************************************

corr gcse lrt lrt_within mean_lrt girl schgend

** Demonstrate the appropriate adjustment if you don't center or grand mean center

quietly summ lrt
gen lrt_grandmean = lrt - r(mean)
bys school:  egen mean_girl = mean(girl)
gen girl_within = girl - mean_girl
quietly summ girl
gen girl_grandmean = girl - r(mean)


mixed gcse i.schgend || school:
mixed gcse i.schgend lrt girl || school:
mixed gcse i.schgend lrt_grandmean girl_grandmean || school:
mixed gcse i.schgend lrt_within girl_within || school:

