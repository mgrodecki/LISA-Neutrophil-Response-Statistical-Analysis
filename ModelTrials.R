#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library("readxl")
library("emmeans")

options(max.print=1000000)

#Create standard linear regression and print summary
mydata=read_excel("C:/Users/ericg/Documents/Boulder PhD/Hind Lab/R/Eric's BS.xlsx")

#Fit 1
experiment=as.factor(mydata$experiment)
condition=as.factor(mydata$condition)
time = as.factor(mydata$time)
count=mydata$count
fit1 = lm(count~ experiment+condition:time)
par(mfrow=c(2,2))
plot(fit1)
summary(fit1)

#Fit 2
experiment=as.factor(mydata$experiment)
condition=as.factor(mydata$condition)
time = mydata$time
count=mydata$count
fit2 = lm(count~ experiment+condition:time)
par(mfrow=c(2,2))
plot(fit2)
summary(fit2)

#Fit 3
lncount = mydata$lncountadj
lntime = mydata$lntimeadj

#fit3 = nlme::gls(count ~ experiment+condition:time, data = mydata,
#          weights = varIdent(form = ~1 | condition:experiment))
fit3 = nlme::gls(count ~ experiment+condition:time, data = mydata)
par(mfrow=c(2,2))
plot(fit3)
summary(fit3)

#fit=lm(count~experiment+condition)
##fit = lm(count ~ experiment+condition:time)


fit = lm(count ~ experiment+condition:time)
par(mfrow=c(2,2))
plot(fit)
summary(fit)

#Regression Analysis
activefit=fit3
res = residuals(activefit)
yhat = fitted(activefit)

ggplot(mydata, aes(yhat, res)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth()

ggplot(mydata, aes(as.numeric(condition), res)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)

ggplot(mydata, aes(as.numeric(experiment), res)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)

ggplot(mydata, aes(as.numeric(time), res)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)


#Homoscedasticity

activefit = fit2
yhat = fitted(activefit)
res_sqrt = sqrt(abs(rstandard(activefit)))
ggplot(data, aes(yhat, res_sqrt)) + geom_point() + geom_smooth()

ggplot(data, aes(as.numeric(time), res_sqrt)) + 
  geom_point() +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)

ggplot(data, aes(as.numeric(experiment), res_sqrt)) + 
  geom_point() +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)

ggplot(data, aes(as.numeric(condition), res_sqrt)) + 
  geom_point() +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)
