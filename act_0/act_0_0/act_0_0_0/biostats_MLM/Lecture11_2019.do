* set the plotting scheme

set scheme s1mono

* open up the dataset (requires an internet connections)

* Read in the data
import delimited "C:\Users\Elizabeth\Dropbox\MLM2019\Lecture11\pisaUSA2000.csv", clear

**** PART A

* Question 1: Calculate the number of schools

codebook id

* Question 1:  Calculate the number of students by school

bys id: gen copyid = _n
bys id: egen n_student = count(id)
summ n_student if copyid==1, detail

* Question 1:  Summarize the student characteristics

tab female
tab high_school college, cell
tab one_for both_for, cell
tab test_lang
summ isei, detail



* Question 2:  Calculate the proportion of reading proficient students in each school

bys id: egen prop_prof = mean(pass_read)
summ prop_prof if copyid==1, detail

* Question 3:  Calculate the composition variables
* and summarize the average isei across schools
bys id: egen mean_isei = mean(isei)
bys id: egen mean_female =mean(female)
bys id: egen mean_high_school =mean(high_school)
bys id: egen mean_college =mean(college) 
bys id: egen mean_one_for = mean(one_for)
bys id: egen mean_both_for = mean(both_for)
bys id: egen mean_test_lang  = mean(test_lang)

** Rescale the proportion variables to represent 10% increase
replace mean_female = mean_female*10
replace mean_high_school = mean_high_school*10
replace mean_college = mean_college*10
replace mean_one_for = mean_one_for*10
replace mean_both_for = mean_both_for*10
replace mean_test_lang = mean_test_lang*10

summ mean_isei if copyid==1, detail


* Question 4:  Random intercept model

gllamm pass_read, i(id) family(binomial) adapt
* NOTE:  also tried nip(12) and nip(16), results were the same
matrix a = e(b)
gllamm, eform

* Calculate a 95% range for the log odds of reading proficiency across schools

*. display -0.705-2*sqrt(0.824)
* -2.5204889

*. display -0.705+2*sqrt(0.824)
*1.1104889

* Calculate a 95% range for the odds of reading proficiency across schools

*. display exp(-2.52)
*.08045961

*. display exp(1.11)
*3.0343584

* Calculate a 95% range for the proportion of reading proficiency across schools

*. display exp(-2.52)/(1+exp(-2.52))
*.07446795

*. display exp(1.11)/(1+exp(1.11))
*.75212911

* Question 5:  Include the composition variables, except for SES
gllamm pass_read mean_female mean_high_school mean_college ///
mean_one_for mean_both_for mean_test_lang, i(id) family(binomial) adapt
* NOTE:  also tried nip(12) and nip(16), results were the same
matrix b = e(b)
gllamm, eform

* Question 6:  Include SES (within and between)
gen isei_c = isei - mean_isei
gllamm pass_read isei_c mean_isei mean_female mean_high_school mean_college ///
mean_one_for mean_both_for mean_test_lang, i(id) family(binomial) adapt
* NOTE:  also tried nip(12) and nip(16), results were the same
matrix c = e(b)
lincom mean_isei - isei_c, eform
gllamm, eform

***** Part B:  Weighted GLLAMM

* Question 1: scale the student level weights

bys id: egen mean_schwt = mean(w_fstuwt)
gen wt1 = w_fstuwt / mean_schwt

* Question 2:  Rename the school level weight

rename wnrschbw wt2

* Question 3: refit the random intercept only model using the weights

gllamm pass_read, i(id) family(binomial) adapt pweight(wt) from(a)

* Question 4: refit the compositional model (ignoring SES) with weights

gllamm pass_read mean_female mean_high_school mean_college ///
mean_one_for mean_both_for mean_test_lang, i(id) family(binomial) ///
adapt from(b) pweight(wt) eform

* Question 5: refit the contextual effects model using the weights

gllamm pass_read isei_c mean_isei mean_female mean_high_school mean_college ///
mean_one_for mean_both_for mean_test_lang, i(id) family(binomial) ///
adapt from(c) pweight(wt) eform
lincom mean_isei - isei_c, eform

** REPLICATE THE WEIGHTED ANALYSIS USING MELOGIT
svyset id, weight(wnrschbw) || _n, weight(wt1)
* Question 3: 
svy: melogit pass_read || id:
* Question 4:
svy: melogit pass_read mean_female mean_high_school mean_college ///
mean_one_for mean_both_for mean_test_lang || id:
* Question 5: 
svy: melogit pass_read isei_c mean_isei mean_female mean_high_school mean_college ///
mean_one_for mean_both_for mean_test_lang || id: 
