## Set the working directory
setwd("C:\\Users\\Elizabeth\\Dropbox\\MLM2019\\Final")

## Read in the dataset
data = read.table("guatemala.csv",sep=",",header=T)

## Define a "my.boot" function that will:
##  -- create the bootstrap dataset by appending resampled clusters
##  -- fit the logistic regression model
##  -- output the log odds ratio, i.e. marginal effect

my.boot = function(d,x,fulldata) {
	newd = NULL
	for(i in 1:length(x)) newd = rbind(newd,data[data$cluster==x[i],])
	fit = glm(immun~kid2p,family="binomial",data=newd)
	beta = fit$coeff[2]
	c(beta)
}

## Load the "boot" package
library(boot)

## Call the boot command to take the bootstrap samples
## 
## The values that are passed to boot() include
## -- the indices that will be resampled, here this is
##    the unique values of the cluster id
## -- a function that is to be applied to each sample (with
##    replacement) of the indices, here this is my.boot
## -- R, the number of bootstrap samples to complete, here 
##    I am specifying 1000
## -- the "fulldata=data" argument at the end is the additional
##    inputs to the my.boot function
##
## This command takes about 2 minutes to run
set.seed(582019)
boot.out = boot(matrix(unique(data$cluster),ncol=1),my.boot,R=1000,fulldata=data)

## Use the boot.ci command to compute 95% CIs for the marginal effect
boot.ci(boot.out,index=1)


