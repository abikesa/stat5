## Create a dataset with 3 hospitals
## Patient case mix is measured as a z-score
## Hospital 1 has average case mix of -1
## Hospital 2 has average case mix of 0
## Hospital 3 has average case mix of 1.5
## We sample 30 patients from each of the three hospitals

set.seed(20160404)

obs = 90
hospid = rep(c(1,2,3), each = 30)
patientid = 1:90
within_hosp_counter = rep(1:30,3)
casemix = rnorm(90,mean = 0,sd = 1)
casemix[hospid == 1] = casemix[hospid == 1] - 1
casemix[hospid == 3] = casemix[hospid == 3] + 1.5
lab2data = data.frame(patientid = patientid, hospid = hospid,
	within_hosp_counter = within_hosp_counter,
	casemix = casemix)
save(lab2data, file = "lab2data.RData")


## Part I: Total = between = within
load("lab2data.RData")

# Create calculated variables
mean_casemix = c(mean(lab2data$casemix[lab2data$hospid == 1]),
	mean(lab2data$casemix[lab2data$hospid == 2]),
	mean(lab2data$casemix[lab2data$hospid == 3]))
mean_casemix = rep(mean_casemix, each = 30)
lab2data$mean_casemix = mean_casemix
lab2data$centered_casemix = lab2data$casemix - lab2data$mean_casemix
grand_mean = mean(lab2data$casemix)
lab2data$grand_mean =  rep(grand_mean, 90)
lab2data$grand_mean_centered_casemix = lab2data$casemix - lab2data$grand_mean	

# Assume the total variance in Y is 1 and the ICC is 0.4
junk = rnorm(sum(lab2data$within_hosp_counter == 1), 
	mean = 0, sd = sqrt(0.4))
lab2data$b0i = rep(junk, each = 30)

#  Generate Y
lab2data$Y = (2 + lab2data$b0i - 1 * lab2data$mean_casemix 
	- 1 * lab2data$centered_casemix + rnorm(nrow(lab2data), 0,sqrt(0.6)))
	

# start plotting
pdf("graph.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

# Create a graph of the observed data
xlim1 = round(min(lab2data$casemix))-1
xlim2 = ceiling(max(lab2data$casemix))+1
ylim1 = round(min(lab2data$Y))-1
ylim2 = ceiling(max(lab2data$Y))+1

plot(Y ~ casemix, data = lab2data[lab2data$hospid == 1,],
	col = 'red', ylab = 'Patient Satisfaction',
	xlab = 'Patient Case Mix', yaxt = 'n',
	ylim = c(ylim1, ylim2),
	xlim = c(xlim1, xlim2))
axis(2, at = seq(ylim1, ylim2, 2), labels = seq(ylim1, ylim2, 2), las=1)
points(Y ~ casemix, data = lab2data[lab2data$hospid == 2,],
	col = 'blue')
points(Y ~ casemix, data = lab2data[lab2data$hospid == 3,],
	col = 'green')
legend("topright", legend = c("Hospital 1", "Hospital 2",
	"Hospital 3"), pch = 'o', col = c('red', 'blue','green'),
	bty = 'n')


# Create a grand-mean centered figure
xlim1 = round(min(lab2data$grand_mean_centered_casemix))-1
xlim2 = ceiling(max(lab2data$grand_mean_centered_casemix))+1
ylim1 = round(min(lab2data$Y))-1
ylim2 = ceiling(max(lab2data$Y))+1

plot(Y ~ grand_mean_centered_casemix, data = lab2data[lab2data$hospid == 1,],
	col = 'red', ylab = 'Patient Satisfaction',
	xlab = 'Grand Mean Centered Patient Case Mix', yaxt = 'n',
		ylim = c(ylim1, ylim2),
	xlim = c(xlim1, xlim2))
axis(2, at = seq(ylim1, ylim2, 2), labels = seq(ylim1, ylim2, 2), las=1)
points(Y ~ grand_mean_centered_casemix, data = lab2data[lab2data$hospid == 2,],
	col = 'blue')
points(Y ~ grand_mean_centered_casemix, data = lab2data[lab2data$hospid == 3,],
	col = 'green')
legend("topright", legend = c("Hospital 1", "Hospital 2",
	"Hospital 3"), pch = 'o', col = c('red', 'blue','green'),
	bty = 'n')


# Generate the mean Y by hospital and diff in Y and mean Y within 
lab2data$meanY = c(rep(mean(lab2data$Y[lab2data$hospid == 1]), 30),
	rep(mean(lab2data$Y[lab2data$hospid == 2]), 30),
	rep(mean(lab2data$Y[lab2data$hospid == 3]), 30)	)
lab2data$centered_Y = lab2data$Y - lab2data$meanY

# Plot the between effects
xlim1 = round(min(lab2data$mean_casemix))-1
xlim2 = ceiling(max(lab2data$mean_casemix))+1
ylim1 = round(min(lab2data$meanY))-1
ylim2 = ceiling(max(lab2data$meanY))+1

plot(meanY ~ mean_casemix, data = lab2data[lab2data$hospid == 1,],
	col = 'red', ylab = 'Average Patient Satisfaction',
	xlab = 'Average Patient Case Mix', yaxt = 'n',
		ylim = c(ylim1, ylim2),
	xlim = c(xlim1, xlim2))
axis(2, at = seq(ylim1, ylim2, 2), labels = seq(ylim1, ylim2, 2), las=1)
points(meanY ~ mean_casemix, data = lab2data[lab2data$hospid == 2,],
	col = 'blue')
points(meanY ~ mean_casemix, data = lab2data[lab2data$hospid == 3,],
	col = 'green')
legend("topright", legend = c("Hospital 1", "Hospital 2",
	"Hospital 3"), pch = 'o', col = c('red', 'blue','green'),
	bty = 'n')
lfit = lm(meanY ~ mean_casemix, data = lab2data)
lines(fitted(lfit) ~ lab2data$mean_casemix, col = 'gray')

# Plot the within effects
xlim1 = round(min(lab2data$centered_casemix))-1
xlim2 = ceiling(max(lab2data$centered_casemix))+1
ylim1 = round(min(lab2data$centered_Y))-1
ylim2 = ceiling(max(lab2data$centered_Y))+1

plot(centered_Y ~ centered_casemix, data = lab2data[lab2data$hospid == 1,],
	col = 'red', ylab = 'Within Hospital Difference in Y',
	xlab = 'Within Hospital Diff in Casemix', yaxt = 'n',
		ylim = c(ylim1, ylim2),
	xlim = c(xlim1, xlim2))
axis(2, at = seq(ylim1, ylim2, 2), labels = seq(ylim1, ylim2, 2), las=1)
points(centered_Y ~ centered_casemix, data = lab2data[lab2data$hospid == 2,],
	col = 'blue')
points(centered_Y ~ centered_casemix, data = lab2data[lab2data$hospid == 3,],
	col = 'green')
legend("topright", legend = c("Hospital 1", "Hospital 2",
	"Hospital 3"), pch = 'o', col = c('red', 'blue','green'),
	bty = 'n')
lfit = lm(centered_Y ~ centered_casemix, data = lab2data)
lines(predict(lfit) ~ lab2data$centered_casemix, col = 'black')

dev.off() ## END of plot

# Fit the models;
# Estimate the between, within, contextual and total effects
library(lme4)
fit1 = lmer(Y ~ mean_casemix + centered_casemix + (1|hospid), data = lab2data)
require(multcomp)
summary(glht(fit1, linfct = c("mean_casemix - centered_casemix = 0")))

print(summary(fit1))
fit2 = lmer(Y ~ casemix + (1| hospid), data = lab2data)
print(summary(fit2))

