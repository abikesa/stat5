** Set more off

set more off

** Change the working directory

cd "C:\Users\Elizabeth\Dropbox\MLM2019\Lecture3"

** Set the plotting scheme

set scheme s1mono

** Open the dataset

import delimited "gcse.csv", clear

** What variables are included?

codebook

** Create a counter for the number of students within a school

bys school: gen counter = _n

** EDA for possible questions:

* 1)  Quantify the relationship between GCSE score and LRT score.

twoway (scatter gcse lrt, ms(o) msize(small) legend(off) ylab(20(10)100,angle(horizontal)) xlab(20(10)100) xtitle("London Reading Test score") ytitle("GCSE score")) (lowess gcse lrt, lc(black) lw(thick))

* 2) Within a school, quantify the relationship between GCSE score and LRT score

* 3) Does the “context” of the school matter?  I.e. do students from schools with higher school-average LRT scores fair better than similar students in schools with lower school-average LRT scores

* Start by decomposing the information in LRT into within and between.

bys school: egen mean_lrt = mean(lrt)
gen lrt_within = lrt - mean_lrt

* Create a the mean gcse across schools, to quantify the between effect

bys school: egen mean_gcse = mean(gcse)
gen gcse_within = gcse - mean_gcse

* Create two graphs:  gcse_within vs. lrt_within and mean_gcse vs. mean_lrt

lowess gcse_within lrt_within, gen(gcse_within_mean)
twoway (scatter gcse_within lrt_within, sort ms(o) msize(small) legend(off) ///
ylab(-40(10)40,angle(horizontal)) xlab(-40(10)40) ///
xtitle("LRT score - School-mean LRT score") ytitle("GCSE score - School-mean GCSE score")) ///
(line gcse_within_mean lrt_within, sort lc(red))
graph save "gcse_lrt_within", replace

lowess mean_gcse mean_lrt, gen(gcse_between_mean)
twoway (scatter mean_gcse mean_lrt, sort ms(o) msize(small) legend(off) ///
ylab(50(10)80,angle(horizontal)) xlab(50(10)70) xtitle("School-mean LRT score") ///
 ytitle("School-mean GCSE score")) (line gcse_between_mean mean_lrt, sort lc(blue))
graph save "gcse_lrt_between", replace

graph combine "gcse_lrt_between" "gcse_lrt_within", rows(1)

* Overlay the between and within group effects on the same graph

gen lrt_scaled = lrt_within + 60
gen gcse_scaled = gcse_within + 70
twoway (scatter gcse lrt, ms(o) msize(vsmall) legend(off) ylab(20(10)100,angle(horizontal)) ///
xlab(20(10)100) xtitle("LRT score") ytitle("GCSE score")) ///
(scatter mean_gcse mean_lrt,ms(o) mc(blue)) ///
(lfit mean_gcse mean_lrt if counter==1, sort lc(blue)) ///
(lfit gcse_scaled lrt_scaled, sort lc(red))

* Add the total effect on the same graph

twoway (scatter gcse lrt, ms(o) msize(vsmall) legend(off) ylab(20(10)100,angle(horizontal)) ///
xlab(20(10)100) xtitle("LRT score") ytitle("GCSE score")) ///
(scatter mean_gcse mean_lrt,ms(o) mc(blue)) ///
(lfit mean_gcse mean_lrt if counter==1, sort lc(blue)) ///
(lfit gcse_scaled lrt_scaled, sort lc(red)) ///
(lfit gcse lrt, lc(black) lw(thick))

* Save the data

save "lecture3_data", replace

* Create a dataset with no ecological fallacy

set seed 88491
quietly regress gcse lrt_within mean_lrt
gen error = invnormal(uniform())*e(rmse)
gen new_gcse = error + _b[_cons] + _b[lrt_within]*lrt_within + _b[lrt_within]*mean_lrt

drop mean_gcse  
bys school: egen mean_gcse = mean(new_gcse)
twoway (scatter new_gcse lrt, ms(o) msize(vsmall) legend(off) ylab(20(10)100,angle(horizontal)) xlab(20(10)100) xtitle("LRT score") ytitle("GCSE score")) ///
(scatter mean_gcse mean_lrt,ms(o) mc(blue)) (lfit mean_gcse mean_lrt if counter==1, sort lc(blue)) (lfit new_gcse lrt_scaled, sort lc(red)) ///
(lfit new_gcse lrt, lc(black) lw(thick))

** replicate with between effect is 3 times the within effect just to demonstrate

use "lecture3_data", clear

set seed 77432
quietly regress gcse lrt_within mean_lrt
gen error = invnormal(uniform())*e(rmse)
gen new_gcse = error + _b[_cons] + _b[mean_lrt]/3*lrt_within + _b[mean_lrt]*mean_lrt

drop mean_gcse  
bys school: egen mean_gcse = mean(new_gcse)
twoway (scatter new_gcse lrt, ms(o) msize(vsmall) legend(off) ylab(20(10)100,angle(horizontal)) xlab(20(10)100) xtitle("LRT score") ytitle("GCSE score")) ///
(scatter mean_gcse mean_lrt,ms(o) mc(blue)) (lfit mean_gcse mean_lrt if counter==1, sort lc(blue)) (lfit new_gcse lrt_scaled, sort lc(red)) ///
(lfit new_gcse lrt, lc(black) lw(thick))

* 4) Are there differences in performance across school type (mixed gender, girls only and boys only)?

use "lecture3_data", clear

label define type 1 "mixed" 2 "boys" 3 "girls"
label values schgend type
graph box mean_gcse, over(schgend) ylab(60(5)80, angle(horizontal)) ytitle("School Average GCSE")

* 5) Within a school, does student gender modify the relationship between GCSE score and LRT score?

twoway (scatter gcse_within lrt_within if girl==1, ms(o) msize(vsmall) ///
legend(off) ylab(-40(10)40,angle(horizontal)) xlab(-40(10)40) ///
xtitle("LRT score - School-mean LRT score") ytitle("GCSE score - School-mean GCSE score")) ///
(lowess gcse_within lrt_within if girl==1, lc(red) lw(thick))
graph save "girls", replace

twoway (scatter gcse_within lrt_within if girl==0, ms(o) msize(vsmall) ///
 legend(off) ylab(-40(10)40,angle(horizontal)) xlab(-40(10)40) ///
 xtitle("LRT score - School-mean LRT score") ytitle("GCSE score - School-mean GCSE score")) ///
(lowess gcse_within lrt_within if girl==0, lc(red) lw(thick))
graph save "boys", replace

graph combine "girls" "boys", rows(1)

* 6) Does school type modify the relationship between GCSE score and LRT score?

quietly lowess gcse_within lrt_within if schgend==1, gen(within_1)
twoway (scatter gcse_within lrt_within if schgend==1, ms(o) msize(vsmall) ///
 legend(off) ylab(-40(10)40,angle(horizontal)) xlab(-40(10)40) ///
 xtitle("LRT score - School mean LRT score") ytitle("GCSE  - School mean GCSE ")) ///
(line within_1 lrt_within if schgend==1, sort lc(black) lw(thick))
graph save "mixed", replace

quietly lowess gcse_within lrt_within if schgend==2, gen(within_2)
twoway (scatter gcse_within lrt_within if schgend==2, ms(o) msize(vsmall) ///
legend(off) ylab(-40(10)40,angle(horizontal)) xlab(-40(10)40) ///
xtitle("LRT score - School mean LRT score") ytitle("GCSE  - School mean GCSE ")) ///
(line within_2 lrt_within if schgend==2, sort lc(red) lw(thick))
graph save "boys", replace

quietly lowess gcse_within lrt_within if schgend==3, gen(within_3)
twoway (scatter gcse_within lrt_within if schgend==3, ms(o) msize(vsmall) ///
legend(off) ylab(-40(10)40,angle(horizontal)) xlab(-40(10)40) ///
xtitle("LRT score - School mean LRT score") ytitle("GCSE  - School mean GCSE ")) ///
(line within_3 lrt_within if schgend==3, sort lc(blue) lw(thick))
graph save "girls", replace

twoway (line within_1 lrt_within if schgend==1, sort lc(black) lw(thick) legend(label(1 "Mixed") label(2 "Boys") label(3 "Girls") cols(1) ring(0) region(style(none)) position(5)) ///
 ylab(-40(10)40,angle(horizontal)) xlab(-40(10)40) xtitle("LRT score - School mean LRT score") ytitle("GCSE  - School mean GCSE ")) ///
(line within_2 lrt_within if schgend==2, sort lc(red) lw(thick)) ///
(line within_3 lrt_within if schgend==3, sort lc(blue) lw(thick))
graph save "all", replace


graph combine "mixed" "boys" "girls" "all", rows(2)
