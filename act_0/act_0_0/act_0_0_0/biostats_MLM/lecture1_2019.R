library("foreign")
data=read.dta("http://www.stata-press.com/data/mlmus3/sex.dta")

# side-by-side boxplots
boxplot(kscore~school,data=data,col="darkgrey",outcol="black",outpch=19,
        ylab="Knowledge Score",xlab="School")
# Calculate the total and within school variance
data$school = as.factor(data$school)
fit = aov(kscore~school,data=data)
summary(fit)

#  Fit the random intercept model with main effect for intervention
library("nlme")
fitRandomIntercept = lme(kscore~arm,random=~1|school,data=data,na.action=na.exclude)
summary(fitRandomIntercept)
VarCorr(fitRandomIntercept)
# Ignore the clustering and fit as independence model
summary(lm(kscore~arm,data=data))


# Simulate a case with larger within school variance:
# The total variance is roughly 5.5
set.seed(1)
school=1:25
arm = c(rep(1,13),rep(0,12))
# between school var = 3.5
btva = rnorm(25,0,sqrt(3.5))
meanc = 4+btva+0.5*arm
prep = data.frame(school=school,arm=arm,btva=btva,meanc=meanc)
# Merge the new school mean onto the original data
datam = merge(data,prep)
# Generate the individual student scores
# assuming the within school variance is 2
sorin = datam$meanc+rnorm(nrow(datam),0,sqrt(2))

# graph
boxplot(sorin~school,data=datam,col="darkgrey",outcol="black",outpch=19,
        ylab="Knowledge Score",xlab="School")
# total & within school var
fit2 = aov(sorin~school,data=datam)
summary(fit2)

# fit the random intercept model
fitRandomIntercept2 = lme(sorin~arm,random=~1|school,data=datam,na.action=na.exclude)
summary(fitRandomIntercept2)
# ignore clustering
summary(lm(sorin~arm,data=datam))
