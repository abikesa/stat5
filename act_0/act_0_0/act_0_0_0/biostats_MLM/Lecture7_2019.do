* Change the working directory

cd "C:\Users\Elizabeth\Dropbox\MLM2019\Lecture3"

** Set the plotting scheme

set scheme s1mono

** Open the dataset

import delimited "gcse.csv", clear

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
twoway (scatter gcse lrt if school==1, legend(off) xlab(30(10)100) ylab(30(10)100,angle(horizontal))) (lfit gcse lrt if school==1)
twoway (scatter gcse lrt if school==2, legend(off) xlab(30(10)100) ylab(30(10)100,angle(horizontal))) (lfit gcse lrt if school==2)

* Create SLR of gcse on lrt for each school and plot the fitted lines
bys school: egen school_mean_lrt = mean(lrt)
gen lrt_groupc = lrt - school_mean_lrt
quietly regress gcse i.school lrt_groupc i.school#c.lrt_groupc if totalstudents>5
predict mle
sort school lrt_groupc
xtset school
twoway scatter mle lrt_groupc if totalstudents>5, c(L) ms(i) ytitle(Predicted GCSE score) xtitle(LRT score (School mean centered)) xlabel(-40(10)40) ylabel(30(10)100, angle(horizontal))

save "gcse_temp.dta", replace

gen slope = .
gen intercept = .

* NOTE: School 48 and 54 only have a few students
* in the sample, so ignore these schools when
* computing the school specific intercepts and
* slopes

local i = 1
while (`i' <= 65) {
display `i'
if(`i'!=48 & `i'!=54) {
	quietly regress gcse lrt_groupc if school==`i'
	matrix out = e(b)
	replace slope = out[1,1] if school==`i'
	replace intercept = out[1,2] if school==`i'
	}
	local i = `i' + 1
}

twoway (scatter slope intercept if totalstudents>=5 & withinschoolcount==1, ytitle(Estimated gcse vs. lrt relationship) xtitle(Estimated average gcse at school-average lrt) xlab(60(10)80) ylab(0(0.5)1,angle(horizontal)) legend(off)) (lfit slope intercept, lw(thick) lc(red))


* Fit the model
mixed gcse lrt_groupc || school: lrt_groupc , cov(uns) stddev
matrix out = e(b)
predict b*, reffects
predict seb*, reses 
predict shrink, fitted

gen eb_intercept = b2+out[1,2]
gen eb_slope = b1+out[1,1]

* Generate plot of random intercepts vs. MLE intercept
twoway scatter eb_intercept intercept if totalstudents>=5 & withinschoolcount==1, ytitle(EB intercept) xtitle(MLE intercept) xlab(50(10)90) ylab(50(10)90,angle(horizontal))
* Generate plot of random slopes vs. MLE slope
twoway scatter eb_slope slope if totalstudents>=5 & withinschoolcount==1, ytitle(EB slope) xtitle(MLE slope) xlab(0(.25)1) ylab(0(0.25)1,angle(horizontal))

* Generate plot of predicted associations to compare with plot
* of MLE/school specific lines generated above
twoway scatter shrink lrt_groupc if totalstudents>5, c(L) ms(i) ytitle(Predicted GCSE score (centered)) xtitle(LRT score (centered)) xlabel(-40(10)40) ylabel(30(10)100, angle(horizontal))


* Create plot of random intercepts with confidence bounds
save "gcse_temp.dta", replace
gen upper = b2+2*seb2
gen lower = b2-2*seb2
keep school b2 upper lower withinschoolcount totalstudents
keep if withinschoolcount==1 & totalstudents>5
drop withinschoolcount totalstudents

sort b2
gen newid = _n
twoway (rcap upper lower newid, lcolor(gray) yline(0, lc(red) lw(medium))) (scatter b2 upper lower newid, legend(off) pstyle(p1 ci ci) ytitle(EB intercept) xtitle(Ranking) ylabel(-12(4)12, angle(horizontal))) 
