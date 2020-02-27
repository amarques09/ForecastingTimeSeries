# Before starting, we have to install and activate some R libraries in order to use the necessary statistics and time series functions
install.packages("fBasics")  # basic statistics 
install.packages("forecast")  # time series functions
library(fBasics)
library(forecast)

# 1.	ANALYZING THE SIMULATED TIME SERIES - file Sessions2&3sim.csv
# Reading the files from "my" computer path
data0<-read.csv("C:/Users/amand/Documents/IE/2nd Term/Forecasting Time Series/1st Assignment/Homework_1_DATA.csv",header=TRUE,sep=";",dec=",")
View(data0)
# Once the time series are read into R, we give a name to each time series data 
# (represented by each column)

series5=data0[,5] [1:2000]

#A.	Let's analyze stationarity for each time series

y<-series5# from now, "y" is the data we are going to work with

par(mar=c(1,1,1,1)) # to adjust graphic size

par(mfrow=c(3,1)) # plot the series, its acf and pacf together
ts.plot(y)   
acf(y)
pacf(y)

mean(y) # compute basic statistics
sd(y)
skewness(y)
kurtosis(y,method=c("moment"))  

# formal unit root test (Augmented Dickey Fuller test). Testing for stationarity.
# Ho: the process is not stationary. We need, at least, a unit root
# H1: the process is stationary. We have to check different models (lags)
ndiffs(y, alpha=0.05, test=c("adf")) # number of regular differences? to see if it is stationary
help(ndiffs)

#Checking for normality graphically
hist(y,prob=T,ylim=c(0,0.6),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
lines(density(y),lwd=2)
mu<-mean(y)
sigma<-sd(y)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")
# formal normality test
# Ho: the data is normally distributed
# H1: the data is not normally distributed
shapiro.test(y)
#B.	Testing for WHITE NOISE graphically
par(mfrow=c(3,1))
ts.plot(y)   
acf(y)
pacf(y)
# Sometimes we will need to do the same for the transformed data "z"
# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test (y, lag = 20, type="Ljung")  # Null: ro1=.=ro20=0

# C.	Testing for STRICT WHITE NOISE
par(mfrow=c(3,1)) # analysis of the squared data
ts.plot(y^2)   
acf(y^2)
pacf(y^2)
# Sometimes we will need to do the same for the transformed data "z"
# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test(y^2,lag=20, type="Ljung")    # Null: ro1=.=ro20=0
#lag20 means the first 20 rows.

# D.	Testing for GAUSSIAN WHITE NOISE
shapiro.test(y)
# GWN ??? SWN

# Just in case we need to take one difference to the original data (as in this case)

z<-diff(y,differences=2) #because it is only ONE transformation
#(lag=1,differences=2)
ts.plot(z)
par(mfrow=c(3,1))
ts.plot(z)   
acf(z)
pacf(z)

ndiffs(z, alpha=0.05, test=c("adf"))

Box.test (z, lag = 20, type="Ljung") 

Box.test (z^2, lag = 20, type="Ljung") 

#Checking for normality

shapiro.test(z)

hist(z,prob=T,ylim=c(0,0.6),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")


# 2.	ANALYZING THE REAL TIME SERIES - file Sessions2&3real.csv
data<-read.csv("C:/Users/usuario/Desktop/LPC/MASTERS/MBD_abril_2019/Sessions2&3real.csv",header=TRUE,sep=";",dec=",")
# we have to follow exactly the same steps as before for the spot price series and for the return series
spot<-data[,1] # spot price series
y<-spot # follow all the steps above
# from now, "y" is the data we are going to work with
par(mfrow=c(3,1))
ts.plot(y)   
acf(y)
pacf(y)

#Checking for normality
shapiro.test(y)

hist(y,prob=T,ylim=c(0,0.02),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
lines(density(y),lwd=2)
mu<-mean(y)
sigma<-sd(y)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

# RETURN TIME SERIES - STATIONARY TRANSFORMATION
returns<-diff(log(spot)) # returns series
z<-returns

# Here is the same code for the transformed variable "z"
ts.plot(z)   # time series plot

mean(z) # basic statistics
sd(z)
skewness(z)
kurtosis(y,method=c("moment"))

shapiro.test(z) # to check normality

hist(z,prob=T,ylim=c(0,30),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")



par(mfrow=c(3,1)) # is the transformed data white noise?
ts.plot(z)   
acf(z)
pacf(z)

Box.test(z,lag=20)  # Null: ro1=.=ro20=0 formal white noise test

par(mfrow=c(3,1)) # is the transformed data strict white noise?
ts.plot(z^2)   
acf(z^2)
pacf(z^2)

Box.test(z^2,lag=20)  # Null: ro1=.=ro20=0
