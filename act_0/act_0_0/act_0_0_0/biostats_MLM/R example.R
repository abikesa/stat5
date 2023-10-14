## This example is adapted from the one provided in the spaMM introduction document
## Author: Francois Rousset
## April 14, 2019

## Make the plot demonstrating the exponential correlation structure
d = seq(0,6,0.1)
plot(0,0,type="n",xlim=c(0,6),ylim=c(0,1),xlab="Distance",ylab="Correlation",las=1)
points(d,exp(-0.2*d),type="l",col="red",lwd=2)
points(d,exp(-0.6*d),type="l",col="blue",lwd=2)
points(d,exp(-d),type="l",col="black",lwd=2)
legend(4,1,c("phi = 0.2","phi = 0.6","phi = 1"),col=c("red","blue","black"),bty="n",lty=c(1,1,1))

## Define the data generating distribution
library(MASS)
rSample <- function(nb,nw,rho,sigma2_u,resid,intercept,slope,pairs=TRUE) {
## sample pairs of adjacent locations
if (pairs) {
x <- rnorm(nb/2); x <- c(x,x+0.05)
y <- rnorm(nb/2); y <- c(y,y+0.05)
} else {x <- rnorm(nb);y <- rnorm(nb)}
dist <- dist(cbind(x,y)) ## distance matrix between locations
m <- exp(-rho*as.matrix(dist)) ## correlation matrix
b <- mvrnorm(1,rep(0,nb),m*sigma2_u) ## correlated random effects
nw <- rpois(nb,nw)
d = data.frame(b=b,cluster=seq(1,nb),x=x,y=y,nw=nw)
d = d[rep(row.names(d),d$nw),1:4]
d$pred <- runif(nrow(d)) ## some predictor variable
d$obs <- intercept+slope*d$pred + d$b +rnorm(nrow(d),0,sqrt(resid)) ## response
d
}

## Example 1: rho = phi = 1
set.seed(123)
d1 <- rSample(nb=40,nw=15,rho=1,sigma2_u=1,resid=0.35,intercept=-1,slope=0.25)
plot(d1$x,d1$y)

#install.packages("spaMM")
library(spaMM)

## Fit the model assuming the exponential decay correlation model (i.e. assign nu=0.5)
HLM <- corrHLfit(obs~pred+Matern(1|x+y),data=d1,ranFix=list(nu=0.5),HLmethod="ML")
summary(HLM)
confint(HLM,"pred")

library(lme4)
fit <- lmer(obs~pred+(1|cluster),data=d1,REML=FALSE)
summary(fit)
confint(fit,"pred")
dnew = predict(fit,d1)

## 'maps' required for add.map=TRUE
library(maps)
filled.mapMM(HLM,add.map=TRUE,plot.axes=quote({axis(1);axis(2)}),
decorations=quote(points(pred[,coordinates],pch=15,cex=0.3)),
plot.title=title(main="Example rho=1",
xlab="longitude",ylab="latitude"))

install.packages("geoR")
library(geoR)
dists <- dist(d1[,3:4])
summary(dists)
plot(variog(coords=d1[,3:4],data=d1$obs-lm(obs~pred,data=d1)$fitted,option="smooth"))



## Example 2: rho = phi = 0.2
set.seed(123)
d1 <- rSample(nb=40,nw=15,rho=0.2,sigma2_u=1,resid=0.35,intercept=-1,slope=0.25)

#install.packages("spaMM")
library(spaMM)

## Fit the model assuming the exponential decay correlation model (i.e. assign nu=0.5)
HLM <- corrHLfit(obs~pred+Matern(1|x+y),data=d1,ranFix=list(nu=0.5),HLmethod="ML")
summary(HLM)
confint(HLM,"pred")

library(lme4)
fit <- lmer(obs~pred+(1|cluster),data=d1,REML=FALSE)
summary(fit)
confint(fit,"pred")

library(maps)
filled.mapMM(HLM,add.map=TRUE,plot.axes=quote({axis(1);axis(2)}),
decorations=quote(points(pred[,coordinates],pch=15,cex=0.3)),
plot.title=title(main="Example rho=1",
xlab="longitude",ylab="latitude"))

install.packages("geoR")
library(geoR)
dists <- dist(d1[,3:4])
summary(dists)
plot(variog(coords=d1[,3:4],data=d1$obs-lm(obs~pred,data=d1)$fitted,option="smooth"))