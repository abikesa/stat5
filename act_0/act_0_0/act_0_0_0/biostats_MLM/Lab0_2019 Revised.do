** Change the working directory
cd "C:\Users\Elizabeth\Dropbox\MLM2019\Labs\Lab0"

** Open the dataset
import delimited "hsb_data.csv", clear

** 1) Data structure
** Create the variables defining the 
** number of students per school and 
** a counter identifing the unique students
** in each school
bys newid: gen N = _N
bys newid: gen n = _n
list newid N n ses sector in 1/47

** 2) School-level variables
summ size - himinty if n == 1
tab sector if n == 1
tab himinty if n == 1
tab sector
tab himinty
summ size pracad disclim


** 3) Student-level variables
summ mathach ses
tab minority
tab female

bys newid: egen mean_mathach = mean(mathach)
bys newid: egen prop_nonwhite = mean(minority)
bys newid: egen prop_female = mean(female)
bys newid: egen mean_ses = mean(ses)
summ mean_mathach - mean_ses if n == 1, detail
gen dummy_female = 0
replace dummy_female = 1 if prop_female > 0 & prop_female < 1
replace dummy_female = 2 if prop_female == 1
tab dummy_female if n == 1

** Summarize the composition of the schools in a figure
set scheme s1mono
hist mean_mathach if n == 1, freq width(1) start(0) xtitle("Average Student Math Achievement")
graph save "hist1", replace
hist prop_nonwhite if n == 1, freq width(.10) start(0) xtitle("Proportion of non-White students")
graph save "hist2", replace
hist prop_female if n == 1, freq width(0.10) start(0) xtitle("Proportion of female students")
graph save "hist3", replace
hist mean_ses if n == 1, freq width(0.25) start(-1.5) xtitle("Average student SES")
graph save "hist4", replace
graph combine "hist1" "hist2" "hist3" "hist4", rows(2)







