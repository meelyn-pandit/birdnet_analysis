# Regression Diagnostics --------------------------------------------------
#Assessing Outliers
library(car)
library(predictmeans)
library(tidyverse)
library(broom)

# outlierTest(m3) # Bonferonni p-value for most extreme obs
# qqPlot(m3, main="QQ Plot") #qq plot for studentized resid 
# leveragePlots(m3) # leverage plots

assump<-function(x) {       #graphs for checking assumptions
r<-residuals(x)
ft<-fitted(x)
# par(mfrow=c(2,2))
library(MASS)
truehist(r,main="Histogram of Residuals",xlab="Residuals")
curve(dnorm(x,mean=mean(r),sd=sd(r)),add=TRUE)
qqnorm(r, ylim=range(r), main="QQNorm Plot",ylab="Quantiles of (ie, ordered) residuals", xlab="Quantiles of normal distribution")
qqline(r,lty=2)
plot(r~ft,main="Residuals vs Fitted",xlab="Fitted (predicted) values",ylab="Residuals");abline(h=0)
qqnorm(ft, ylim=range(ft), main="QQNorm Plot",ylab="Quantiles of fitted values", xlab="Quantiles of normal distribution")
qqline(ft,lty=2)
acf(resid(x))
  }

par(mfrow = c(2,2))
Assump(m3)
