library(fBasics)
library(forecast) 
install.packages("car")
library(car)
library(data.table)

data<-read.csv("C:/Users/amand/Documents/IE/2nd Term/Forecasting Time Series/2nd Assignment/Homework2DATA.csv",header=TRUE,sep=";",dec=",")
View(data)
head(data)


#1.	Find the best time series model for the variable "ibex"

y= data[,2]
ts.plot(y)
par(mar=c(5,5,5,5))
nlags=104     # play with this parameter.. #52 lags for the weeks of the year 
par(mfrow=c(2,1))
acf(y,nlags)
pacf(y,nlags)  

s=52       # seasonal parameter FOR THIS DATA SET (weekly)
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?

fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,0,0),period=s)) 
fit

ts.plot(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) #regular differences = 0

Box.test(fit$residuals,lag=104) # p-value = 1, the residuals are WhiteNoise.

#Normality check
shapiro.test(fit$residuals)  #It is normally distributed. The residuals are WhiteNoise, so the data is also Gaussian White Noise.

#Checking for Strict WhiteNoise

ts.plot(fit$residuals^2)
par(mfrow=c(2,1))
acf(fit$residuals^2,nlags)
pacf(fit$residuals^2,nlags)

ndiffs(fit$residuals^2, alpha=0.05, test=c("adf")) #regular differences = 0
Box.test(fit$residuals^2,lag=104) # p-value = 1, the residuals are Strict WhiteNoise.

#After running those tests, the residuals are White Noise and Strict WhiteNoise. It means there are no better linear or non-linear models to predict our models.

#2.	Find the best regression model for the dependent variable "ibex". 
#Find the best regression model for IBEX

View(data)

model1 <- lm(IBEX~.,data) #TAKE OUT WEEKS
summary(model1)

model2 <- lm(IBEX~ Long.term.rate + Exchange.rate.... +Short.term.rate ,data)
summary(model2)

#model3 <- lm(IBEX~Week* Long.term.rate + Exchange.rate....,data)
#summary(model3)

#model4 <- lm(IBEX~Week* Long.term.rate*Exchange.rate....*Short.term.rate,data)
#summary(model4)

#model5 <- lm(IBEX~Week**2+ Long.term.rate+ Exchange.rate....+Short.term.rate,data)
#summary(model5)

#model7 <- lm(IBEX~Week**2+Long.term.rate**2+Exchange.rate....**2,data)
#summary(model7)

#model9 <- lm(IBEX~Long.term.rate+Exchange.rate....+Short.term.rate,data)
#summary(model9)

#look for the graph / if there is relationship in the graph.
model10 <- lm(IBEX ~ Long.term.rate + Exchange.rate.... + Short.term.rate, data)
summary(model10)

#Adj. RSquared = 0.9698
# all variables are significant. p-value < 0.05


#checking for multicollinearity / NO

multicollinearity <- vif(lm(IBEX ~ Long.term.rate + Exchange.rate.... + Short.term.rate, data)) #no multicolinnearity because they are all significant together as well as separete.
format(multicollinearity, scientific = FALSE)

library(corrplot)
corrplot(cor(data), type = 'lower')
cor(data$IBEX,data$Week)
#high correlation does not necessarily mean multicolinearity - in this case we have no multicollinearity.
#Correlation between variables:
#IBEX is highly correlated with Week and Exchange Rate. However it is inversely correlated with Short.term and Longt.term rate.

#checking for the residuals

y <- model10$residuals
ts.plot(y)
par(mar=c(5,5,5,5))
nlags=104     # play with this parameter.. #104 lags for the weeks of 2 years 
par(mfrow=c(2,1))
acf(y,nlags)
pacf(y,nlags)  

ndiffs(y, alpha=0.05, test=c("adf")) #stationary
Box.test(y,lag=104) #no white noise
shapiro.test(y) #normally distributed

fit_model10<- arima(y,order = c(2,0,0)) #because we have 2 lag out of limit on the PACF and too many on the ACF. Keep it simple. on the ACF
fit_model10

ts.plot(fit_model10$residuals)
par(mar=c(5,5,5,5))
nlags=104     # play with this parameter.. #104 lags for the weeks of 2 years 
par(mfrow=c(2,1))
acf(fit_model10$residuals,nlags) #Residuals are White Noise
pacf(fit_model10$residuals,nlags)  #Residuals are White Noise
Box.test(fit_model10$residuals, lag = 104) #p-value = 1. The residuals are definetely White Noise.
shapiro.test(fit_model10$residuals) #Gaussian White Noise
#Until now, we do not need a linear-model.

ts.plot(fit_model10$residuals^2)
par(mar=c(5,5,5,5))
nlags=104     # play with this parameter.. #104 lags for the weeks of 2 years 
par(mfrow=c(2,1))
acf(fit_model10$residuals^2,nlags) #Residuals are Strict White Noise
pacf(fit_model10$residuals^2,nlags)  #Residuals are Strict White Noise
Box.test(fit_model10$residuals^2, lag = 104) #p-value = 1. The residuals are definetely Strict White Noise.
#We do not need non-linear model.


#3.	Find the best regression model with time series errors for the dependent variable "ibex"
#Checking for other variables

y = data[,2] #IBEX
exchange.rate <- data[,3]
short_term <- data[,4]
long_term <- data[,5]

#Step1: fit the linear regression model for the variables in the original scale:

# Joint estimation of the model (PURE PROFESSORS CODE)

x=matrix(0,length(y),3)
x[,1]= exchange.rate
x[,2]= short_term
x[,3]= long_term

fit_total=arima(y,order=c(2,0,0),xreg=x)
fit_total

par(mfrow=c(3,1))
ts.plot(fit_total$residuals)
acf(fit_total$residuals)
pacf(fit_total$residuals)  

Box.test(fit_total$residuals,lag=30)

# Remove what is non significant
xx=matrix(0,length(y),2)
xx[,1]=exchange.rate
xx[,2]=long_term

reg2=lm(y~exchange.rate+long_term)
summary(reg2)

par(mfrow=c(3,1))
ts.plot(reg2$residuals)
acf(reg2$residuals)
pacf(reg2$residuals)  

ndiffs(reg2$residuals, alpha=0.05, test=c("adf"))

fit<-arima(reg2$residuals,order=c(1,0,0)) 
fit

fit_total2=arima(y,order=c(1,0,0),xreg=xx)
fit_total2

par(mfrow=c(3,1))
ts.plot(fit_total2$residuals)
acf(fit_total2$residuals)
pacf(fit_total2$residuals)    

Box.test(fit_total2$residuals,lag=30)
Box.test(fit_total2$residuals^2,lag=30)

shapiro.test(fit_total2$residuals)


# Forecasting step (ANSWERING THE FINAL QUESTION)
x=matrix(0,length(y),2) 
x[,1]= exchange.rate # exchange 
x[,2]= long_term # LT 
fit_total=Arima(y,order=c(1,0,0),xreg=x) 
fit_total 

pred_x=t(cbind(c(0.781,10.76))) 
prediction=forecast(fit_total,xreg=pred_x) #we can see the CI
prediction2=predict(fit_total,newxreg = pred_x) #we can see the std error

