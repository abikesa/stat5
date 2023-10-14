* set the plotting scheme

set scheme s1mono

* open up the dataset
import delimited "C:\Users\Elizabeth\Dropbox\MLM2019\Lecture8\pisaUSA2000.csv", clear

**** PART A

* Question 1: Calculate the number of schools

codebook id

* Calculate the number of students by school

bys id: gen copyid = _n
bys id: gen total_students = _N
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

* Graph of reading proficient by school
gen upper = prop_prof + 1.96 * sqrt(prop_prof * (1-prop_prof) / total_student)
gen lower = prop_prof - 1.96 * sqrt(prop_prof * (1-prop_prof) / total_student)
replace upper = 1 if upper>1
replace lower = 0 if lower < 0
twoway (rspike upper lower id, sort xlab(1(25)150) ylab(0(0.2)1, angle(horizontal)) xtitle(School Index) ytitle(Proportion Proficient in Reading)) ///
(scatter prop_prof id, ms(O) mc(red) legend(off))


* Random intercept model
* Stata <= 12
* xtmelogit pass_read || id:  
* Stata 13+
meqrlogit pass_read || id: , stddev

* Calculate a 95% range for the log odds of reading proficiency across schools
display -0.71-2*0.91
* -2.53
display -0.71+2*0.91
* 1.11


* Calculate a 95% range for the odds of reading proficiency across schools

display exp(-2.53)
* 0.08
display exp(1.11)
* 3.03

* Calculate a 95% range for the proportion of reading proficiency across schools

display exp(-2.53)/(1+exp(-2.53))
* 07
display exp(1.11)/(1+exp(1.11))
* .75

* Prediction of random effect
quietly meqrlogit pass_read || id:
predict b, reffects 
predict se_b, reses

* Create the school-specific log odds of proportion of proficient readers
gen lo_eb = -0.71 + b
gen lo_mle = log(prop_prof/(1-prop_prof))
replace lo_mle = -3 if prop_prof==0
hist lo_mle if copyid==1, freq start(-3) width(0.25) xtitle("MLE of Log Odds of School Proportion Proficient") ylab(0(5)30, angle(horizontal))
graph save mle, replace
hist lo_eb if copyid==1, freq start(-3) width(0.25) xtitle("EB of Log Odds of School Proportion Proficient") ylab(0(5)30, angle(horizontal))
graph save eb, replace
graph combine "mle" "eb", rows(1)

* Incorporate information about size of the cluster in the assessment of shrinkage
gen inv_n = 1/total_students
egen junk = sum(1/total_students) if copyid==1
egen sum_ss = mean(junk)
drop junk 
gen weight = inv_n / sum_ss
twoway (scatter lo_eb lo_mle [aweight=weight] if copyid==1, ylab(-3(1)2, angle(horizontal)) xlab(-3(1)2) ///
xtitle("MLE of Log Odds of School Proportion Proficient") ytitle("EB of Log Odds of School Proportion Proficient") ///
legend(off)) (line lo_mle lo_mle, lc(red))

** Assess fit based on number of quadrature points
meqrlogit pass_read || id: 
* Default number of integration points is 7, change this to make sure the model results are converging
meqrlogit pass_read || id:  , intp(14)
* No difference

* If you are using a model with single random intercept, you can use xtlogit, re and the quadchk
xtlogit pass_read, i(id) re
quadchk

* Try adaptive quadrature in GLLAMM
gllamm pass_read, i(id) family(binomial) adapt







