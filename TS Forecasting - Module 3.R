#---------------Load Libaries and remove contents from global environment--------------------
library(forecast)
library(zoo)
library(reshape2)
library(tseries)
library(readxl)
library(data.table)
library(MLmetrics)

rm(list=ls())

install.packages("fpp")
library(fpp)
data(ausbeer)
class(ausbeer)
frequency(ausbeer)
plot(ausbeer)
start(ausbeer)
timeserie_beer = tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))
start(timeserie_beer)



data(AirPassengers)
timeserie_air = AirPassengers
plot(as.ts(timeserie_air))

#Additive
ts_beer = ts(timeserie_beer, frequency = 4)
decompose_beer = decompose(ts_beer, "additive")
plot(decompose_beer)


plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)

mul_comp_beer = as.data.frame(cbind(decompose_beer$seasonal,decompose_beer$trend,decompose_beer$random))

adjust_beer = ts_beer - decompose_beer$seasonal
plot(as.ts(timeserie_air))
plot(adjust_beer)

#Multiplicative
ts_air = ts(timeserie_air, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")

plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)

adjust_air = ts_air / decompose_air$seasonal

plot((timeserie_air))
plot(adjust_air)


#Problem with Decompose 
#Calculates Trend using moving 2way moving average lagged to the frequecy/cycle in the Data set
#trend is na. analyisis on Trend and Remainder is difficult


#STL Function
#STL usess LOESS (locally weighted smoothing) method to arrive at Seasonal Component
#http://www.gardner.fyi/blog/STL-Part-I/
#http://www.gardner.fyi/blog/STL-Part-II/
#http://www.gardner.fyi/blog/STL-Part-III/

#Multiplicative series can be converted to additive series by taking log

# y = s*t*r || log(y) = log(s)+log(t)+log(r)

ts_beer = ts(timeserie_beer, frequency = 4)
stl_beer = stl(ts_beer, "periodic")
seasonal_stl_beer   <- stl_beer$time.series[,1]
trend_stl_beer     <- stl_beer$time.series[,2]
random_stl_beer  <- stl_beer$time.series[,3]

plot(ts_beer)
plot(as.ts(seasonal_stl_beer))
plot(trend_stl_beer)
plot(random_stl_beer)
plot(stl_beer)


#-----------------------------Using STLF to Forecast--------------------------------
#Use of STLF function which first decomposes the data, and then uses a non-seasonal algorithm to forecast. 
#The forecast is automatically reseasonalized.

fcst_beer = stlf(ts_beer, h = 8, s.window = "periodic",method = "arima")
#S.Window - 
#Default Method to Forecast - ETS

#Extracting Forecast
fcst_beer$mean
fcst_df <- as.data.frame(fcst_beer$mean[1:8])
colnames(fcst_df)=c("STLF_ARIMA")
plot(fcst_beer)

fcst_beer2 = stlf(ts_beer, h = 8, s.window = "periodic",method="naive")
plot(fcst_beer2)

fcst_beer2$model


#----------------------Exponential Smoothing--------------------------#
ets = ets(ts_beer, model = "ZZZ")
ets$fit
plot(ets)
plot(ets$fitted)
fcst_beer_ets = forecast(ets, h = 4*5)
plot(fcst_beer_ets)

install.packages("smooth")
require(smooth)
es = es(ts_beer, h=18, holdout=TRUE, silent=FALSE)
es$fitted
es$actuals

#Triple Exponential Smoothing - takes into account seasonality and trend
fit_hw = HoltWinters(ts_beer)
(fit_hw$fitted)
fcst_beer_hw = forecast(fit_hw, h = 4*5)
plot(fcst_beer_hw)
