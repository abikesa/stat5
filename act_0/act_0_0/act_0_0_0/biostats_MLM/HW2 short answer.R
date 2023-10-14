## Set the working directory
setwd("C:\\Users\\Elizabeth\\Dropbox\\MLM2019\\Homework2")

# Open the HW2 MSA 2017.csv file
data = read.table("HW2 MSA 2017.csv",header=T,sep=",")

# Model 1 / Question 1: Random intercept only logistic regression model
library(lme4)
fit = glmer(cbind(pass,Tested_Count-pass)~1+(1|School_Number),
data=data,family="binomial",nAGQ=7)
summary(fit)

# Model 2 / Question 2 and 3: Full interaction random intercept logistic regression model
data$grade4 = ifelse(data$Grade=="Grade 4",1,0)
data$grade5 = ifelse(data$Grade=="Grade 5",1,0)
data$grade4charter = data$grade4*data$charter
data$grade5charter = data$grade5*data$charter
fit = glmer(cbind(pass,Tested_Count-pass)
~grade4+grade5+charter+grade4charter+grade5charter+(1|School_Number),
data=data,family="binomial",nAGQ=7)
summary(fit)
lincom.out = esticon(fit,L = rbind(c(0,0,0,1,0,0),c(0,0,0,1,1,0),c(0,0,0,1,0,1)))
exp(lincom.out[,2])

# Model 3 / Question 3: Full interaction + confounders random intercept logistic model
# NOTE:  To Address some convergence issues in glmer, I scaled the 
# Level 2 confounding variables, i.e. create these as mean 0, SD 1 variables
data$Enrolled_Count_C = (data$Enrolled_Count - mean(unlist(tapply(data$Enrolled_Count,data$School_Number,unique))))/sqrt(var(unlist(tapply(data$Enrolled_Count,data$School_Number,unique))))
data$Attend_Rate_Pct_C = (data$Attend_Rate_Pct - mean(unlist(tapply(data$Attend_Rate_Pct,data$School_Number,unique))))/sqrt(var(unlist(tapply(data$Attend_Rate_Pct,data$School_Number,unique))))
data$FARMS_Pct_C = (data$FARMS_Pct - mean(unlist(tapply(data$FARMS_Pct,data$School_Number,unique))))/sqrt(var(unlist(tapply(data$FARMS_Pct,data$School_Number,unique))))

fit = glmer(cbind(pass,Tested_Count-pass)
~grade4+grade5+charter+grade4charter+grade5charter+
FARMS_Pct_C+Enrolled_Count_C+Attend_Rate_Pct_C+(1|School_Number),
data=data,family="binomial",nAGQ=7,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fit)
lincom.out = esticon(fit,L = rbind(c(0,0,0,1,0,0,0,0,0),c(0,0,0,1,1,0,0,0,0),c(0,0,0,1,0,1,0,0,0)))
exp(lincom.out[,2])

# Model 4 / Question 4:  Marginal model
library(geepack)
fit = geeglm(cbind(pass,Tested_Count-pass)
~grade4+grade5+charter+grade4charter+grade5charter,
data=data,family="binomial",corstr="exchangeable",id=data$School_Number)
summary(fit)
lincom.out = esticon(fit,L = rbind(c(0,0,0,1,0,0),c(0,0,0,1,1,0),c(0,0,0,1,0,1)))
exp(lincom.out[,2])
