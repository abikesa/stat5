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

