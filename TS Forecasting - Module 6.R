#---------------Load Libaries and remove contents from global environment--------------------
library(forecast)   #For time series & forecasting algorithms
library(reshape2)   #For Data Manipulation & Structuring
library(tseries)    #Time Series Analysis - Alternate Package 
library(lubridate)  #Handling Dates
library(MLmetrics)  #Pre-defined Accuracy calculation functions
library(data.table) #Data Handling and Manipulation
library(ggplot2)    #For Plots
library(fpp)        #Package with sample datasets
library(zoo)        
library(pracma)
library(imputeTS)
# install.packages("ggplot2")
# install.packages("fpp2")
# install.packages("GGally")
# install.packages("aTSA")
library(GGally)
library(fpp2)
library(aTSA)

rm(list=ls())


#--------------------------Correlation between Series---------------------------
str(elecdemand)
#Multiple time series with Demand Consumption and Temperature - measured half hourly
#Multiple frequencies
qplot(Temperature, Demand, data=as.data.frame(elecdemand))
data=as.data.frame(elecdemand)
cor(data$Temperature,data$Demand)
#0.27

#Lag Plots
beer2 <- window(ausbeer, start=1992)
plot(beer2)
ggseasonplot(beer2)
ggsubseriesplot(beer2)

gglagplot(beer2) #Inference
  

#------------------------------AutoCorrelation----------------------------------
#Correlation - Measures the extent of linear relationship between 2 variables
#AutoCorrelation measures the linear relationship between lagged value of a series 

#yt = A(yt-k)+Er
plot(beer2)
forecast::Acf(beer2)
ggAcf(beer2)
#R4 is higher than any other lags - due to seasonality in the data - every 4 quarters
#R2 is more negative than any other - due to seasonality - troughs tend to be 2 quarters behind peaks


#ACF plot with Trend in Series
aelec <- window(elec, start=1980) #Electricity Production
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec, lag=48)
#Series with Trend have large autocorrelation for small lag - They are nearby in size
#Such series tend to decrease slowly over time as lags increase
#Series with Seasonality - ACF is larger for seasonal lags  


#------------------------------Partial-AutoCorrelation----------------------------------
#Partial Auto correlation measures the relationship between 2 varibales while also accounting for relationships with common variables
#yt = A(yt-k)+B(yt-k-i)+Er
plot(beer2)
forecast::Pacf(beer2)
ggPacf(beer2)


#Series with White Noise
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)
fit = auto.arima(y,stepwise = FALSE,parallel = TRUE)
fcst = forecast::forecast(fit, h = 8)
plot(fcst)
#Blue dotted Lines represents 95% limit of ACF value - to consider significant lags
#If any lagged acf is greater than blue line - series is not considered to be white noise


#------------------------------ARIMA----------------------------------
#Exponential smoothing models are based on a description of the trend and seasonality in the data

#ARIMA models aim to describe the autocorrelations in the data.

#------------------------------Stationarity---------------------------------
#mean independant of time 
#variance independant of time
#co variance between two data points independant of time
cafe = fpp::cafe
cycle(cafe)
frequency(cafe)
autoplot(cafe)
ggseasonplot(cafe,polar = T) 

#Default Null Hypothesis - Series is not stationary - it is integrated series
#Reject Null Hypothesis if P-Value is Less than specific Signficance Level
#nlag = floor(4*(length(x)/100)^(2/9))

tseries::adf.test(cafe, alternative="stationary")

#Differencing

#Transformations such as logarithms can help to stabilize the variance of a time series. Differencing can help stabilize the mean of a time series by removing changes in the level of a time series, and therefore eliminating (or reducing) trend and seasonality.

plot(diff(cafe))
tseries::adf.test(diff((cafe)), alternative="stationary")

forecast::Acf(cafe)
forecast::Acf(diff(cafe))
forecast::Pacf(diff(cafe))

#Arima Model

#Steps
# 1. Plot the data and identify any unusual observations.
# 2. If necessary, transform the data (using a Box-Cox transformation) to stabilize the variance.
# 3. If the data are non-stationary, take first differences of the data until the data are stationary.
# 4. Examine the ACF/PACF
# 5. Try your chosen model(s), and use the AICc to search for a better model.
# 6. Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals. If they do not look like white noise, try a modified model.
# 7. Once the residuals look like white noise, calculate forecasts.

plot(elecequip)
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

tseries::adf.test(eeadj, alternative = "stationary")

tseries::adf.test(diff(eeadj), alternative = "stationary")

forecast::Acf(diff(eeadj))

forecast::Pacf(diff(eeadj))

fit_arima = arima(eeadj,order = c(3,1,1))
forecast::Acf(fit_arima$residuals)

checkresiduals(fit_arima)

auto_arima = auto.arima(eeadj, stepwise = F,approximation = F, parallel = T, seasonal = F)
auto_arima$arma
checkresiduals(auto_arima)
#stepwise - used to converge faster. use if computational time is a factor
#approximation MLE criterion is aprroximated
