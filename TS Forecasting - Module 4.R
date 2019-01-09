#---------------Load Libaries and remove contents from global environment--------------------
# install.packages("fpp")
library(fpp)
library(forecast)
library(zoo)
library(reshape2)
library(tseries)
library(readxl)
library(data.table)
library(MLmetrics)
library(ggplot2)

rm(list=ls())

#----------------------Exponential Smoothing--------------------------#
#Single Exponential Smoothing - No Seasonality No trend additive Time Series
oildata <- window(oil, start=1996)
plot(oildata)
fc <- ses(oildata, h=5)
fc$model
plot(fc)
round(accuracy(fc),2)

#Double Exponential Smoothiing - Increasing/Decreasing trend + Additive Timeseries no Seasonality
air <- window(ausair, start=1990)
plot(air)
fc <- holt(air, h=5)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)#assumes trend indefinitely phi is used to dampen trend 
fc$model
plot(fc)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))



#Triple Exponential Smoothing - Seasonality + Trend + Additive Time series 
aust <- window(austourists,start=2005)
plot(aust)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
fit1$model
sum(fit1$residuals^2)

autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

fit1_dmp <- hw(aust,damped = T,seasonal="additive")
fit2_dmp <- hw(aust,damped = T,seasonal="multiplicative")
fit1$model


autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit1_dmp, series="HW additive damped forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))



#ETS Forecasting
aus_ets = ets(aust, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
    gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
    additive.only=FALSE, restrict=TRUE,
    allow.multiplicative.trend=FALSE)
plot(aus_ets)

aus_ets = ets(aust, model = "ZZZ")
summary(aus_ets)
fcst = forecast(aus_ets, h = 4*2)
plot(fcst)

