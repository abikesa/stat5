* Note: change directory before reading in raw data.
 cd "C:\\Users\\Elizabeth\\Dropbox\\MLM2019\\Lecture10"

* Set the plotting scheme
set scheme s1mono

* Basic housekeeping for the data

insheet using "http://www-personal.umich.edu/~bwest/veneer.dat", clear

* Create a new patient ID
bys patient: gen patient_counter = _n
keep if patient_counter == 1
gen patientid = _n
keep patient patientid
sort patient
save "newpatientid.dta", replace

insheet using "http://www-personal.umich.edu/~bwest/veneer.dat", clear
merge n:1 patient using "newpatientid.dta"
drop _merge

* Generate a tooth counter within each subject
bys patient: gen patient_counter = _n
bys patient tooth: gen tooth_counter = _n

* Save the dataset
save "veneer.dta", replace


* Basic Summaries for the data

use "veneer.dta", clear

* Person level characteristics:
sum age if patient_counter==1

* Tooth level characteristics
sum base_gcf cda if tooth_counter==1
 
* Make a plot of variation in CDA across teeth within subject
bys patient: egen mean_CDA = mean(cda) if tooth_counter==1
bys patient: egen mean_baseGCF = mean(base_gcf) if tooth_counter==1
twoway (scatter cda patientid if tooth_counter==1, legend(off) ytitle("CDA") ylab(-4(4)12, angle(horizontal)) xlab(1(1)12) ms(O) mc(red)) (scatter mean_CDA patientid if tooth_counter==1, mc(blue)) 
graph save "panel1.gph", replace
twoway (scatter base_gcf patientid if tooth_counter==1, legend(off) ytitle("Baseline GCF") ylab(0(5)45, angle(horizontal)) xlab(1(1)12) ms(O) mc(red)) (scatter mean_baseGCF patientid if tooth_counter==1, mc(blue)) 
graph save "panel2.gph", replace
graph combine "panel1.gph" "panel2.gph", row(1)

* Make a summary plot of the GCF vs. time for each patient/tooth
keep patientid tooth age time gcf
reshape wide gcf, i(patientid time) j(tooth)

list in 1/4

label var gcf6 "6"
label var gcf7 "7"
label var gcf8 "8"
label var gcf9 "9"
label var gcf10 "10"
label var gcf11 "11"
label var age "Patient Age"

twoway line gcf6 gcf7 gcf8 gcf9 gcf10 gcf11 time, ///
lcolor(black black black black black black) ///
lpattern(solid dash longdash vshortdash shortdash dash_dot) ytitle(GCF) by(age) ///
legend(rows(1))
graph save "subject_data.gph", replace

twoway line gcf6 gcf7 gcf8 gcf9 gcf10 gcf11 time, ///
lcolor(black blue green red orange purple) ///
lpattern(solid solid solid solid solid solid) ytitle(GCF) by(age) ///
legend(rows(1))
graph save "subject_data_color.gph", replace


* Reopen the data and create a figure looking at change in GCF

* Open the dataset again
use "veneer.dta", clear

gen change = gcf - base_gcf
keep patientid tooth age time change
reshape wide change, i(patientid time) j(tooth)

list in 1/4

label var change6 "6"
label var change7 "7"
label var change8 "8"
label var change9 "9"
label var change10 "10"
label var change11 "11"
label var age "Patient Age"

twoway line change6 change7 change8 change9 change10 change11 time, ///
lcolor(black blue green red orange purple) ///
lpattern(solid solid solid solid solid solid) ytitle(Change in GCF) by(age) ///
legend(rows(1))
graph save "subject_data_change_color.gph", replace

** Create a graph correlating change in GCF with CDA, separately at 3 and 6 months

use "veneer.dta", clear
gen change = gcf - base_gcf
twoway (scatter change cda if time==3, ylab(-50(25)50, angle(horizontal)) ytitle("Change in GCF") title("3 Months") xtitle("Average Contour Difference") legend(off)) (lowess change cda if time==3)
graph save "change_cda_3.gph", replace
twoway (scatter change cda if time==6, ylab(-50(25)50, angle(horizontal)) ytitle("Change in GCF") title("6 Months") xtitle("Average Contour Difference") legend(off)) (lowess change cda if time==6)
graph save "change_cda_6.gph", replace
graph combine "change_cda_3.gph" "change_cda_6.gph", row(1)

* Create a similar figure looking at change in GCF with age, separately at 3 and 6 months
twoway (scatter change age if time==3, ylab(-50(25)50, angle(horizontal)) ytitle("Change in GCF") title("3 Months") xtitle("Patient Age") legend(off)) (lowess change age if time==3)
graph save "change_age_3.gph", replace
twoway (scatter change age if time==6, ylab(-50(25)50, angle(horizontal)) ytitle("Change in GCF") title("6 Months") xtitle("Patient Age") legend(off)) (lowess change age if time==6)
graph save "change_age_6.gph", replace
graph combine "change_age_3.gph" "change_age_6.gph", row(1)

*** MODEL 1

gen sixM = time==6
gen time_cda = sixM * cda
gen time_age = sixM * age

mixed change sixM cda age time_cda time_age || patientid: sixM, cov(unstruct) || tooth: , ml stddev
* Save model information.
est store model1
estat ic

*** MODEL 2

* Fit Model 2, allowing for heterogeneous error variance at the two time points.
mixed change sixM cda age time_cda time_age || patientid: sixM, cov(unstruct) || tooth: , residuals(ind, by(time)) ml stddev
* Save model information, for comparison with Model 1.
est store model2
* Test hypothesis 7.2, to see if heterogeneous residual variance at the two time points improves the fit of the model.
lrtest model2 model1


* Simplify fixed effects:

mixed change sixM cda age || patientid: sixM, cov(unstruct) || tooth: , ml stddev
* Save model information.
est store model3
estat ic

quietly mixed change sixM cda age time_cda time_age || patientid: sixM, cov(unstruct) || tooth: , ml
est store model1ML
quietly mixed change sixM cda age || patientid: sixM, cov(unstruct) || tooth: , ml
est store model3ML
lrtest model3ML model1ML

