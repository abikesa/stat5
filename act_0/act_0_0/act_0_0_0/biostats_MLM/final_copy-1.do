* Read in the dataset
import delimited C:\Users\Elizabeth\Dropbox\MLM2019\Labs\Lab42019\guatemala.csv, clear

* Run the bootstrap:
* 
* -- The command name in Stata is "bootstrap"
* -- After the commmand name, you tell Stata what values you want 
*    to store from each bootstrap sample, here we will be fitting a
*    logistic regression model and we are asking to store _b, 
*    i.e. the regression coefficients
* -- next are options for the boostrap procedure:
*       -reps(1000) asks to take 1000 bootstrap samples
*       -cluster(cluster) tells Stata that you will be resampling 
*        clusters (and the name of the cluster variable happens to be cluster)
*       -seed(582019) specifies where to start in terms of the random number
*        generator, this will allow you to exactly replicate your findings
* -- bca asks the bootstrap function to compute the BCa confidence interval
*
* After these options, you give the ":"
* -- What follows the ":" is the statistical analysis you want applied to each
*    bootstrap sample.  Here we ask to fit a logistic regression model regressing
*    kid2p on the log odds of immun 
*
* 1000 bootstrap replicates in this dataset, resampling clusters takes about
* 2 minutes to run.
bootstrap _b, reps(1000) cluster(cluster) seed(582019) bca: logistic immun kid2p

* To get the BCa bootstrap confidence interval, use the following:
estat bootstrap, bca
