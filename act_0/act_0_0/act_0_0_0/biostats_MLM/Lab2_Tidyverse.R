## Create a dataset with 3 hospitals
## Patient case mix is measured as a z-score
## Hospital 1 has average case mix of -1
## Hospital 2 has average case mix of 0
## Hospital 3 has average case mix of 1.5
## We sample 30 patients from each of the three hospitals

#setwd("../MLM_2020/Lab2")

library(tidyverse)
library(gridExtra)
set.seed(20160404)

n = 90 # total patients
N = 3 # total hospitals

lab2data = tibble(hospid = rep(1:N, each = n/N),
	      patientid = 1:n, 
	      counter = rep(1:(n/N),N),
	      casemix = rnorm(n,mean = 0,sd = 1) + 
	      	1.5*(hospid == 3) -
	      	1*(hospid==1))
save(lab2data, file = "lab2data.RData")

## Part I: Total = between = within
#load("lab2data.RData")

# Create calculated variables
lab2data =
lab2data %>%
group_by(hospid) %>%
mutate(mean_casemix = mean(casemix),
       centered_casemix = casemix - mean_casemix) %>%
ungroup %>%
mutate(grand_mean = mean(casemix), 
       grand_mean_centered_casemix = casemix - grand_mean)

# Generate patient satisfaction (Y)
# Assume the total variance in Y is 1 and the ICC is 0.4
lab2data = 
lab2data %>%
group_by(hospid) %>%
mutate(b0i = rnorm(1, 0, sqrt(0.4))) %>%
ungroup %>%
mutate(Y = 2 + b0i - 
       	1 * mean_casemix - 
       	1 * centered_casemix + 
       	rnorm(nrow(lab2data), 0,sqrt(0.6))) %>%
group_by(hospid) %>%
mutate(meanY = mean(Y),
       centered_Y = Y - mean(Y))

# Create a graph of the observed data
observed.plot<-
ggplot(lab2data) +
geom_point(aes(x = casemix, y = Y, color = factor(hospid)))+
theme_classic() +
theme(legend.position = c(.8, .8),
      legend.background = element_rect(color = "grey80"))+
scale_color_discrete("Hospital") +
xlim(-3, 4.2) + ylim(-2.5, 7.5)+
stat_smooth(aes(x = casemix, y = Y), method = "lm", se=F)

# Plot the between-hospital effects
between.plot <- 
ggplot(lab2data) +
geom_point(aes(x = mean_casemix, y = meanY, color = factor(hospid)))+
theme_classic() +
theme(legend.position = "none",
      legend.background = element_rect(color = "grey80"))+
scale_color_discrete("Hospital")+
xlim(-3, 4.2) + ylim(-2.5, 7.5) +
stat_smooth(aes(x = mean_casemix, y = meanY), method = "lm", se=F)

# Plot the within effects
within.plot <- 
ggplot(lab2data) +
geom_point(aes(x = centered_casemix + grand_mean, y = centered_Y + mean(Y), 
	   color = factor(hospid)))+
theme_classic() +
theme(legend.position = "none",
      legend.background = element_rect(color = "grey80"))+
scale_color_discrete("Hospital")+
xlim(-3, 4.2) + ylim(-2.5, 7.5) +
stat_smooth(aes(x = centered_casemix + grand_mean, y = centered_Y + mean(Y)), 
	method = "lm", se=F)

grid.arrange(observed.plot, within.plot, between.plot)

# Fit the models;
# Estimate the between, within, contextual and total effects
library(lme4)
fit1 = lmer(Y ~ mean_casemix + centered_casemix + (1|hospid), data = lab2data)
require(multcomp)
summary(glht(fit1, linfct = c("mean_casemix - centered_casemix = 0")))

print(summary(fit1))
fit2 = lmer(Y ~ casemix + (1| hospid), data = lab2data)
print(summary(fit2))

