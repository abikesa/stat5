* import the hsb_data.csv
import delimited C:\Users\Elizabeth\Dropbox\MLM2019\Labs\Lab0\hsb_data.csv

* Generate the school mean and grand mean ses
bys newid: egen mean_ses = mean(ses)
egen grandmean_ses = mean(ses)

* Generate the school-mean centered and grand-mean centered SES
gen centeredSES = ses - mean_ses
gen grandcenteredSES = ses - grandmean_ses

* Assuming no contextual effects of gender and minority status
* Fit the linear mixed model allowing for school specific
* intercept and linear slope for ses
mixed mathach centeredSES female minority || newid: centeredSES, cov(uns)

* From the fit of the model, save the estimated random intercepts and slopes
predict m1, reffects

* Consider an alternative model, where we change the definition of the 
* school specific intercept to the expected mathach for students with the 
* grand mean SES
mixed mathach grandcenteredSES female minority mean_SES || newid: grandcenteredSES, cov(uns)
predict m2, reffects

* Create rankings based on the 
* Random intercept from the school mean centered model
* Random intercept from the grand mean centered model
* Random slope for SES
bys newid: gen counter = _n
keep if counter==1

sort m12
gen ranking_int_schoolmean = _n

sort m22
gen ranking_int_grandmean = _n

sort m11
gen ranking_slope = _n

sort m12
list m12 m22 m11 ranking_int_schoolmean ranking_int_grandmean ranking_slope in 1/15
list m12 m22 m11 ranking_int_schoolmean ranking_int_grandmean ranking_slope in 141/156


