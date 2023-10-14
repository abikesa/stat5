## Set the working directory
setwd("C:\\Users\\Elizabeth\\Dropbox\\MLM2019\\Homework2")

## Part I

## Open the MSA2017_individual.csv file
data = read.table("MSA2017_individual.csv",sep=",",header=T)
names(data) = c("School_Number","Grade","charter","pass")

## Question 1:
# Confirm number of schools
length(unique(data$School_Number))
# Confirm the number of charter schools
table(tapply(data$charter,data$School_Number,FUN=function(x) x[1]))
# Compute the number of students in each grade per school
out = tapply(data$School_Number,list(data$School_Number,data$Grade),length)
summary(out[,1])
summary(out[,2])
summary(out[,3])
#  Compute the proportion of students who pass in each grade per school
out = tapply(data$pass,list(data$School_Number,data$Grade),mean)
summary(out[,1])
summary(out[,2])
summary(out[,3])

# Question 2a:
library(lme4)
data$grade4 = data$Grade=="Grade 4"
data$grade5 = data$Grade=="Grade 5"
summary(glmer(pass~grade4 + grade5 + (1|School_Number),data=data,family="binomial",nAGQ = 7))

# Question 2e:
summary(glmer(pass~grade4 + grade5 + (1|School_Number),data=data,family="binomial",nAGQ = 4))
summary(glmer(pass~grade4 + grade5 + (1|School_Number),data=data,family="binomial",nAGQ = 14))


# Question 3:
summary(glmer(pass~charter + (1|School_Number),data=data,family="binomial",nAGQ = 7))

# Question 3d:
library(geepack)
summary(geeglm(pass~charter,data=data,family="binomial",corstr="exchangeable",id=data$School_Number))

#### Part 2

# Open the HW2 MSA 2017.csv file
data = read.table("HW2 MSA 2018.csv",header=T,sep=",")

# List the first 7 rows of data
data[1:7,c("School_Number","School_Name","Tested_Count","Grade","pass","charter")]

# Refit the model to show equivalence
data$grade4 = ifelse(data$Grade=="Grade 4",1,0)
data$grade5 = ifelse(data$Grade=="Grade 5",1,0)
fit = glmer(cbind(pass,Tested_Count-pass)
~grade4+grade5+(1|School_Number),data=data,family="binomial",nAGQ=7)
summary(fit)




