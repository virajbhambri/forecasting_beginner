#---------------Load Libaries and remove contents from global environment--------------------
library(forecast)   #For time series & forecasting algorithms
library(reshape2)   #For Data Manipulation & Structuring
library(tseries)    #Time Series Analysis - Alternate Package 
library(lubridate)  #Handling Dates
library(MLmetrics)  #Pre-defined Accuracy calculation functions
library(data.table) #Data Handling and Manipulation
library(ggplot2)    #For Plots
library(fpp)        #Package with sample datasets


rm(list=ls())

#-------------------------------Reading Dataset------------------------------------------------
Sales <- read.csv("//Sample_Data_Pand.csv",stringsAsFactors = F,check.names = F,strip.white = T)

str(Sales)
max(Sales$Year_Week)
min(Sales$Year_Week)
tapply(Sales$Sales,list(Sales$Year_Month),FUN=sum)

setDT(Sales)

#------------------Aggregate the dataset to required level of forecasting-----------------------
Sales <- Sales[,list(Sales=sum(Sales))
                   ,by=list(ACCTG_YR,ACCTG_MTH,ACCTG_WK,Year_Month,Year_Week,`DV_Renamed`,`Prod Category`)]

#-----------------------------Configuring TS--------------------------------
Sales_Sample <- Sales[DV_Renamed=="DV004",]

Sales_TS <- ts(Sales_Sample$Sales,start = c(Sales_Sample$ACCTG_YR[1],Sales_Sample$ACCTG_WK[1]),frequency = 52)
class(Sales_TS)
Sales_TS

plot(Sales_TS)

#-------------------------------Frequency-----------------------------------
#Class Assignment - Making a TS for CatB With a frequency of 12 (Month Level)
{
Sales_Cat_Sum <- Sales[`Prod Category`=="CatB",list(Sales=sum(Sales))
                           ,by=list(ACCTG_YR,ACCTG_MTH,Year_Month,`Prod Category`)]
Sales_Cat_Sum_ts <- ts(Sales_Cat_Sum$Sales,start = c(Sales_Cat_Sum$ACCTG_YR[1],Sales_Cat_Sum$ACCTG_MTH[1]),frequency = 12)
ggseasonplot(Sales_Cat_Sum_ts) 
ggseasonplot(Sales_Cat_Sum_ts,polar = T) 
}
#--------------------------------Plotting-----------------------------------
#Line Plot of TS
autoplot(Sales_TS) +
  ggtitle("CatB: Sell In Demand") +
  xlab("Year") +
  ylab("Sell In")


ggseasonplot(Sales_TS) 
ggseasonplot(Sales_TS,polar = T) 


#-------------Accounting for Data to have unequal Terms while aggregation----------
Sales_Cat_Avg <- Sales[,list(Sales=sum(Sales))
                           ,by=list(ACCTG_YR,ACCTG_MTH,Year_Month,Year_Week,`Prod Category`)][
                             ,list(Sales=mean(Sales))
                             ,by=list(ACCTG_YR,ACCTG_MTH,Year_Month,`Prod Category`)]

Sales_TS_Avg <- ts(Sales_Cat_Avg[`Prod Category`=="CatB",Sales],start = c(Sales_Sample$ACCTG_YR[1],Sales_Sample$ACCTG_MTH[1]),frequency = 12)
plot(Sales_TS)

ggseasonplot(Sales_TS_Avg) 
ggseasonplot(Sales_TS_Avg,polar = T) 

#------------------------------Stationarity---------------------------------
fpp::cafe
cycle(cafe)
frequency(cafe)
autoplot(cafe)
ggseasonplot(cafe,polar = T) 

#Default Null Hypothesis - Series is not stationary - it is integrated series
#Reject Null Hypothesis if P-Value is Less than specific Signficance Level

adf.test((cafe), alternative="stationary")

#Differencing
plot(diff(cafe))

adf.test(diff((cafe)), alternative="stationary")

adf.test((Sales_TS_Avg), alternative="stationary")
adf.test(diff((Sales_TS_Avg)), alternative="stationary")

#Lag order can be either specified manually or the function will assume it by default based on length.
#Lags are assumed to remove potential autocorrelation from the residuals to make the test valid.
#More discussion on it later

# Debit Card usage in Ireland

#------------------------Components of a TS---------------------------------
#Seasonal Data
fpp::debitcards
autoplot(debitcards) 
ggseasonplot(debitcards) 
ggseasonplot(debitcards,polar = T)

#Data with Linear Trend
autoplot(ausair) 

autoplot(ausbeer,polar = T) 

#Data with Cyclicity
  #Sales of New-One Family houses, USA

#Data With White Noise
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")

#-----------------------------Assignment----------------------------------------
#Construct a Time Series for CatE, CatC, CatA and CatD


#Check whether data is stationery/Difference Data if Required


#Analyse the series to identify Seasonality and Trend

