## Change the working directory
setwd("../MLM_2020/Lab0")

## Open the dataset
data <- read.table("hsb.csv",header=T,sep=",")

## 1) Data structure
N <- unlist(tapply(data$newid,data$newid,length))
data$N <- rep(N,N)
data$n <- unlist(tapply(data$newid,data$newid,FUN=function(x) seq(1,length(x))))
data[1:47,c("newid","N","n","ses","sector")]

## 2) School level variables
schools <- data[data$n==1, ]
table(schools$sector)
mean(schools$sector)
table(schools$himinty)
mean(schools$himinty)
table(data$sector)
mean(data$sector)
table(data$himinty)
mean(data$himinty)

my.fun = function(x) c(mean(x),sqrt(var(x)))
apply(schools,2,my.fun)


## Student level variables
apply(data,2,my.fun)
table(data$minority)
mean(data$minority)
table(data$female)
mean(data$female)

# Calculate summary statistics for the student level variables
# to compare the school composition
data$meanmathach = rep(unlist(tapply(data$mathach,data$newid,mean)),N) 
data$meanses = rep(unlist(tapply(data$ses,data$newid,mean)),N)
data$propnonwhite = rep(unlist(tapply(data$minority,data$newid,mean)),N)
data$propfemale = rep(unlist(tapply(data$female,data$newid,mean)),N)
schools <- data[data$n==1,]
apply(schools,2,summary)

####################################################################################
# Tidyverse Style coding

library(tidyverse)

## Open the dataset
data <- read_csv("hsb.csv")
data

## Create School and Participant IDs
data <- 
data %>%
mutate(newid = dense_rank(schoolid)) %>%
group_by(newid) %>%
mutate(n = 1:n())

## 1) Data structure
# N will be the number of participants per school
data <- 
data %>%
group_by(newid) %>%
mutate(N = n())

## 2) School level variables
data %>%
filter(n==1) %>%
select(sector, himinty, size, pracad, disclim) %>%
gather() %>%
group_by(key) %>%
summarise(mean = mean(value), sd = sd(value))
	
## Student level variables
student.summaries <- 
data %>%
ungroup %>%
select(female, minority, mathach, ses) %>%
gather() %>%
group_by(key) %>%
summarise(mean = mean(value), sd = sd(value))

student.summaries

# Calculate school-level summary statistics for the student level variables
# to compare the school composition
school.summaries <-
data %>%
select(female, minority, mathach, ses) %>%
gather("key", "value", -newid) %>%
group_by(newid, key) %>%
summarise(mean.var = mean(value)) %>%
group_by(key) %>%
summarise(school.mean = mean(mean.var), school.sd = sd(mean.var))

bind_cols(student.summaries, school.summaries)

