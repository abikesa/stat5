* change the working directory

cd "C:\Users\Elizabeth\Dropbox\MLM2019\Lecture5and6"

* Set the plotting scheme

set scheme s1mono

* Open the dataset

import delimited "gcse.csv", clear

* Observed Data
twoway (scatter gcse lrt, ylab(20(20)100, angle(horizontal)) xlab(20(20)100) xtitle("lrt_grandmc score") ytitle("GCSE") legend(off) m(o) mc(gray)) (lfit gcse lrt, lw(thick) lc(black))
corr gcse lrt

bysort school: egen mean_gcse = mean(gcse)
bysort school: egen mean_lrt = mean(lrt)
quietly summ lrt
scalar gm = r(mean) 
gen lrt_grandmc = lrt - gm
gen lrt_groupmc = lrt - mean_lrt
bysort school: gen count = _n

hist mean_lrt if count==1, freq start(50) width(2) xline(60, lc(red) lw(thick))

* Impact of Centering

* Data with highlighted three schools:  smallest, average and largest mean lrt_grandmc
twoway (scatter gcse lrt, m(p) mc(gray)) (scatter gcse lrt if school==37, m(o) mc(blue) ylab(20(20)100, angle(horizontal)) xlab(20(20)100) xtitle("LRT score") ytitle("GCSE") legend(off)) ///
(scatter gcse lrt if school==11, m(o) mc(red)) (scatter gcse lrt if school==17, m(o) mc(green)) ///
(lfit gcse lrt if school==37, lc(blue)) (lfit gcse lrt if school==11, lc(red)) (lfit gcse lrt if school==17, lc(green))


* Grand mean centering
twoway (scatter gcse lrt_grandmc, m(p) mc(gray)) (scatter gcse lrt_grandmc if school==37, m(o) mc(blue) ylab(20(20)100, angle(horizontal)) xlab(-40(20)40) xtitle("Grand mean centered LRT score") ytitle("GCSE") legend(off)) ///
(scatter gcse lrt_grandmc if school==11, m(o) mc(red)) (scatter gcse lrt_grandmc if school==17, m(o) mc(green)) ///
(lfit gcse lrt_grandmc if school==37, lc(blue)) (lfit gcse lrt_grandmc if school==11, lc(red)) (lfit gcse lrt_grandmc if school==17, lc(green))

* Group mean centering
twoway (scatter gcse lrt_groupmc, m(p) mc(gray)) (scatter gcse lrt_groupmc if school==37, m(o) mc(blue) ylab(20(20)100, angle(horizontal)) xlab(-40(20)40) xtitle("Group mean centered LRT score") ytitle("GCSE") legend(off)) ///
(scatter gcse lrt_groupmc if school==11, m(o) mc(red)) (scatter gcse lrt_groupmc if school==17, m(o) mc(green)) ///
(lfit gcse lrt_groupmc if school==37, lc(blue)) (lfit gcse lrt_groupmc if school==11, lc(red)) (lfit gcse lrt_groupmc if school==17, lc(green))

* Between cluster effect
gen lrt_group_grandmc = mean_lrt - gm
twoway (scatter mean_gcse lrt_group_grandmc if count==1, m(o) mc(gray) ylab(60(5)80, angle(horizontal)) xlab(-10(5)10) xtitle("School-mean LRT - Grand Mean LRT") ytitle("School-mean GCSE") legend(off)) ///
(lfit mean_gcse lrt_group_grandmc if count==1, lc(red) lw(thick))
regress mean_gcse lrt_group_grandmc if count==1


* Estimate the parameters from the grand mean and group mean centered approaches

xtmixed gcse lrt_grandmc || school: 
predict shrink_grandmc, fitted
predict grandmc*, reffects

xtmixed gcse lrt_groupmc || school: 
predict shrink_groupmc, fitted
predict groupmc*, reffects

save "lecture5.dta", replace

** Consider a more extreme case of the shrinkage in random intercepts:

import delimited "centering lecture gcse.csv", clear

* Rescale the GCSE variable to have mean 70
replace gcse =  gcse + 70

* Between Effect

bysort school: egen mean_gcse = mean(gcse)
bysort school: egen mean_lrt = mean(lrt_orig)
bysort school: gen count = _n

* Within Effect centered by group mean

twoway (scatter gcse lrt_orig if school==31, ylab(20(20)100, angle(horizontal)) xlab(0(20)100) xtitle("LRT score") ytitle("GCSE") legend(off) m(o) mc(gray)) ///
(scatter gcse lrt_orig if school==52, m(o) mc(gray)) (scatter gcse lrt_orig if school==55, m(o) mc(gray)) (lfit gcse lrt_orig if school==31, lw(thick) lc(blue)) ///
(lfit gcse lrt_orig if school==52, lw(thick) lc(blue)) (lfit gcse lrt_orig if school==55, lw(thick) lc(blue)) ///
(lfit mean_gcse mean_lrt if count==1, lw(thick) lc(red)) (lfit gcse lrt_orig, lw(thick) lc(gray))

gen lrt_c = lrt_orig - mean_lrt
summ lrt_orig
scalar gm = r(mean) 
gen lrt_gmc = lrt_orig - gm

* Within Effects: centered by group or grand mean

twoway (scatter gcse lrt_c if school==31, ylab(20(20)100, angle(horizontal)) xlab(-20(5)20) xtitle("LRT score (Group-mean centered)") ytitle("GCSE") legend(off) m(o) mc(black)) ///
(scatter gcse lrt_c if school==52, m(o) mc(red)) (scatter gcse lrt_c if school==55, m(o) mc(green)) (lfit gcse lrt_c if school==31, lw(thick) lc(blue)) ///
(lfit gcse lrt_c if school==52, lw(thick) lc(blue)) (lfit gcse lrt_c if school==55, lw(thick) lc(blue))

twoway (scatter gcse lrt_gm if school==31, ylab(20(20)100, angle(horizontal)) xlab(-40(20)40) xtitle("LRT score (Grand-mean centered)") ytitle("GCSE") legend(off) m(o) mc(black)) ///
(scatter gcse lrt_gm if school==52, m(o) mc(red)) (scatter gcse lrt_gm if school==55, m(o) mc(green)) (lfit gcse lrt_gm if school==31, lw(thick) lc(blue)) ///
(lfit gcse lrt_gm if school==52, lw(thick) lc(blue)) (lfit gcse lrt_gm if school==55, lw(thick) lc(blue))


** Interaction of level-1 and level-2 predictor

** Use the HSB data
import delimited "hsb_data.csv", clear

** Create grand and group mean centered SES
bys schoolid: egen mean_ses = mean(ses)
quietly summ ses
scalar gm = r(mean)
gen ses_grandmc = ses - gm
gen ses_groupmc = ses - mean_ses
** Create grand mean centered pracad and disclim
quietly summ pracad
scalar gm = r(mean)
gen pracad_grandmc = pracad - gm
quietly summ disclim
scalar gm = r(mean)
gen disclim_grandmc = disclim - gm

* Create the interaction terms.
gen interaction_1 = ses_groupmc * sector 
gen interaction_2 = mean_ses * sector
gen interaction_1gm = ses_grandmc * sector 

xtmixed mathach ses_groupmc sector interaction_1 || schoolid: 
xtmixed mathach ses_grandmc sector interaction_1gm || schoolid: 
xtmixed mathach ses_grandmc mean_ses sector interaction_1gm interaction_2 || schoolid: 



