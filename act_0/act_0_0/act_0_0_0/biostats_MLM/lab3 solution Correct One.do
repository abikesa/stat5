* Change the working directory
cd "C:\\Users\\Elizabeth\\Dropbox\\MLM2019\\Homework2"

**** Part I

* Open the MSA2017_individual.csv file
import delimited "MSA2017_individual.csv"

* Question 1:
* Confirm number of schools
codebook school_number
* Confirm the number of charter schools
bys school_number: gen within_school_counter = _n
tab charter if within_school_counter==1
* Compute the number of students in each grade per school
bys school_number grade: gen num_students = _N
bys school_number grade: gen within_grade_counter = _n
bys grade: summ num_students if within_grade_counter==1
* Compute the proportion of students who pass in each grade per school
bys school_number grade: egen prop_pass = mean(pass)
bys grade: summ prop_pass if within_grade_counter==1

* Question 2a:
gen grade4 = grade=="Grade 4"
gen grade5 = grade=="Grade 5"
meqrlogit pass grade4 grade5 || school_number: 
meqrlogit, or

* Question 2e:
meqrlogit pass grade4 grade5 || school_number: , intp(4)
meqrlogit pass grade4 grade5 || school_number: , intp(14)

* Question 3:
meqrlogit pass charter || school_number: 

* Question 3d:
xtset school_number
xtgee pass charter ,family(binomial) corr(exch) 


**** Part 2

* Open the HW2 MSA 2017.csv file
clear
import delimited "HW2 MSA 2018.csv"

list school_number school_name tested_count grade pass charter in 1/7

gen grade4 = grade=="Grade 4"
gen grade5 = grade=="Grade 5"
meqrlogit pass grade4 grade5 || school_number: , binomial(tested_count)

