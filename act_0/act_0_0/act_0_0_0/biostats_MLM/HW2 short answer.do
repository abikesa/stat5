* Change the working directory
cd "C:\\Users\\Elizabeth\\Dropbox\\MLM2019\\Homework2"

* Open the HW2 MSA 2017.csv file
clear
import delimited "HW2 MSA 2017.csv"

** Short Answers ** 

* Model 1 / Question 1: Random intercept only logistic regression model
meqrlogit pass || school_number: , binomial(tested_count)

* Model 2 / Question 2 and 3: Full interaction model random intercept logistic model
gen grade4 = grade == "Grade 4"
gen grade5 = grade == "Grade 5"
gen grade4charter = grade4*charter
gen grade5charter = grade5*charter
meqrlogit pass grade4 grade5 charter ///
grade4charter grade5charter || school_number: , binomial(tested_count)
lincom charter, eform
lincom charter + grade4charter, eform
lincom charter + grade5charter, eform

* Model 3 / Question 3: Full interaction with confounders random intercept logistic model
meqrlogit pass grade4 grade5 charter ///
grade4charter grade5charter attend_rate_pct farms_pct ///
enrolled_count || school_number: , binomial(tested_count)
lincom charter, eform
lincom charter + grade4charter, eform
lincom charter + grade5charter, eform

* Model 4 / Question 4: Full interaction with confounders marginal logistic model
xtset school_number
xtgee pass grade4 grade5 charter ///
grade4charter grade5charter attend_rate_pct farms_pct ///
enrolled_count, family(binomial tested_count) corr(exchangeable)
lincom charter, eform
lincom charter + grade4charter, eform
lincom charter + grade5charter, eform
