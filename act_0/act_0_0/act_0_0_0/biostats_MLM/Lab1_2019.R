## Change the working directory
setwd("C:\\Users\\Elizabeth\\Documents\\MLM2013")

## Open the dataset
data <- read.table("hsb_data.csv",header=T,sep=",")

## 2) Data visualization and model fit
# Keep just the first 25 schools
data <- data[data$newid<=25,]
# Create a figure decomposing the within and between information
boxplot(mathach~newid, data=data,cex.axis=0.5,las=1,ylab="School ID",xlab="Math Achievement Score")
# Fit the two-stage normal-normal model
library(nlme)
summary(lme(mathach~1,random=~1|newid, data=data))

## 4a) Fix theta, y-bar, tau^2 and sigma^2, change n
n <- seq(0,70)
b_1 <- 11.1 / (11.1 + 38.4 / n) * (7.6 - 13)
b_2 <- 11.1 / (11.1 + 38.4 / n) * (16.3 - 13)
b_3 <- 11.1 / (11.1 + 38.4 / n) * (11.1 - 13)
plot(n,b_1,type="l",col="black",ylab="Estimated b_i",
xlab="School sample size",xlim=c(0,70),ylim=c(-6,6),las=1)
points(n,b_2,type="l",col="red")
points(n,b_3,type="l",col="blue")
legend(0,6,c("newid = 3","newid = 4","newid = 22"),lty=c(1,1,1),col=c("black","red","blue"),bty="n")
abline(h=0)


