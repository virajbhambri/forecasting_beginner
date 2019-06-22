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

#-----------------------------Assignment----------------------------------------
#Construct a Time Series for CatE, CatC, CatA and CatD


#Check whether data is stationery/Difference Data if Required


#Analyse the series to identify Seasonality and Trend

