* set the plotting scheme

set scheme s1mono

* open up the dataset
import delimited "C:\Users\Elizabeth\Dropbox\MLM2019\Lecture8\pisaUSA2000.csv", clear

**** Summarize and explore the data

* Calculate the number of schools

codebook id

* Calculate the number of students by school

bys id: gen copyid = _n
bys id: egen n_student = count(id)
summ n_student if copyid==1, detail

* Summarize the student characteristics

tab female
tab high_school college, cell
tab one_for both_for, cell
tab test_lang
summ isei, detail

* Calculate the proportion of reading proficient students in each school

bys id: egen prop_prof = mean(pass_read)
summ prop_prof if copyid==1, detail

* Calculate the average of level-1 coavariates and summarize at the school level
bys id: egen mean_isei = mean(isei)
bys id: egen mean_female = mean(female)
bys id: egen mean_high_school = mean(high_school)
bys id: egen mean_college = mean(college)
gen neither = high_school==0 & college==0
bys id: egen mean_neither = mean(neither)
bys id: egen mean_one_for = mean(one_for)
bys id: egen mean_both_for = mean(both_for)
gen zero = one_for==0 & both_for==0
bys id: egen mean_zero = mean(zero)
bys id: egen mean_test_lang = mean(test_lang)

summ mean_isei if copyid==1, detail
summ mean_female if copyid==1, detail
summ mean_neither if copyid==1, detail
summ mean_high_school if copyid==1, detail
summ mean_college if copyid==1, detail
summ mean_zero if copyid==1, detail
summ mean_one_for if copyid==1, detail
summ mean_both_for if copyid==1, detail
summ mean_test_lang if copyid==1, detail

* Explore the within and between effects of gender:
* Within effects
bys id: egen junk0 = mean(pass_read) if female==0
bys id: egen school0 = mean(junk0)
bys id: egen junk1 = mean(pass_read) if female==1
bys id: egen school1 = mean(junk1)
drop junk0 junk1
gen gender_LOR = log(school1 / (1-school1) / (school0 / (1-school0)))
hist gender_LOR if copyid==1, freq start(-3) width(0.5)
summ gender_LOR if copyid==1

* Between effects
* Fit the random intercept only model
quietly meqrlogit pass_read || id:
predict b, reffects
gen lo_b = b + _b[_cons]
gen lo = log(prop_prof/(1-prop_prof))
replace lo = . if prop_prof==0
twoway (scatter lo mean_female if copyid==1, ylab(-3(1)2,angle(horizontal)) legend(off) ///
ytitle("Observed Log Odds of Proficient in Reading") xtitle("Proportion of Female Students")) ///
(lowess lo mean_female if copyid==1, lc(red) lw(thick)) (lfit lo mean_female if copyid==1, lc(blue) lw(thick))


gen female_center = female - mean_female
meqrlogit pass_read female || id: , or stddev
meqrlogit pass_read female mean_female || id: , or stddev
meqrlogit pass_read female_center mean_female || id: , or stddev
test female = mean_female
meqrlogit pass_read female_center || id: , or stddev
meqrlogit pass_read mean_female || id: , or stddev

* Rescale the mean_female variable
gen mean_female_10 = mean_female * 10
meqrlogit pass_read female mean_female_10 || id: , or stddev

save "pisa_temp", replace

*****************
** Consider ISEI
*****************

** Scale ISEI by 10 units
gen isei10 = isei/10
gen mean_isei10 = mean_isei/10

** Compute the number that pass per school
bys id: egen numPASS = sum(pass_read)

** Generate the slope variable
gen slope = .

** Save the dataset
save "pisa_temp", replace

*** Exploratory analysis for within cluster ISEI effect
* estimate school-specific log odds ratio 

local i = 1
while (`i' <= 148) {
display `i'
use "pisa_temp", clear
keep if id==`i'
if(numPASS[1]>=2) {
	quietly logistic pass_read isei10 if id==`i' & numPASS>=2
	matrix out = e(b)
	use "pisa_temp", clear
	replace slope = out[1,1] if id==`i'
	save "pisa_temp", replace
}
	local i = `i' + 1
}


use "pisa_temp", clear
hist slope if copyid==1, freq 
summ slope if copyid==1

*** Exploratory analysis for the between cluster ISEI effect
twoway (scatter lo mean_isei10 if copyid==1, ylab(-3(1)2,angle(horizontal)) legend(off) ///
ytitle("Observed Log Odds of Proficient in Reading") xtitle("Average ISEI (per 10 units)")) ///
(lowess lo mean_isei10 if copyid==1, lc(red) lw(thick)) (lfit lo mean_isei10 if copyid==1, lc(blue) lw(thick))


** Fit the models

gen isei_center10 = isei10 - mean_isei10
meqrlogit pass_read isei10 || id: , or stddev
meqrlogit pass_read isei10 mean_isei10 || id: , or stddev
meqrlogit pass_read isei_center10 mean_isei10 || id: , or stddev
test isei_center10 = mean_isei10
meqrlogit pass_read isei_center10 || id: , or stddev
meqrlogit pass_read mean_isei10 || id: , or stddev

************************
*** In the contextual model, model 2
*** illustrate the difference in marginal 
*** vs. random intercept model estimates
************************

xtset id
xtgee pass_read isei10 mean_isei10, family(binomial) i(id) corr(exch) eform
