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

rm(list=ls())

Sales <- read.csv("//Sample_Data_Pand.csv",stringsAsFactors = F,check.names = F,strip.white = T)
setDT(Sales)

#Corrections in Dataset
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV136","CatD",`Prod Category`)]
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV496","CatE",`Prod Category`)]
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV521","CatE",`Prod Category`)]
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV601","CatE",`Prod Category`)]

#------------------Aggregate the dataset to required level of forecasting-----------------------

Sales_test <- Sales[Year_Week>=201745,list(Sales=sum(Sales))
                   ,by=list(ACCTG_YR,ACCTG_MTH,ACCTG_WK,Year_Month,Year_Week,`DV_Renamed`,`Prod Category`)]

Sales <- Sales[Year_Week<=201744,list(Sales=sum(Sales))
                   ,by=list(ACCTG_YR,ACCTG_MTH,ACCTG_WK,Year_Month,Year_Week,`DV_Renamed`,`Prod Category`)]

min(Sales$Year_Week)
max(Sales_test$Year_Week)

Sales <- Sales[order(DV_Renamed,Year_Week)]

#--------------------------Transformations on Dataset---------------------------
#Log
Sales$Sales_Log <- log(Sales$Sales)
Sales$Sales_Log[is.infinite(Sales$Sales_Log)] <- 0

#TSClean

# i<-"DV540"
Sales_Clean <- data.frame()
for(i in unique(Sales$DV_Renamed))
{
  Sales_Subset<- Sales[DV_Renamed==i,]

  y <- ts(Sales_Subset$Sales,start = c(Sales_Subset$ACCTG_YR[1],Sales_Subset$ACCTG_WK[1]),frequency = 52)
  y1<-try(tsclean(y))
  
  if (class(y1) == 'try-error')
  {
    y1<-y};
  
  #TS clean leading to NA
  if(sum(is.na(y1))>=1){y1 <- y};
  
  y<-y1
  df <- data.frame(Sales_TSClean = melt(y1)$value)
  df$Sales_TSClean <- as.integer(df$Sales_TSClean)

  Sales_Subset <- cbind(Sales_Subset,df)
  Sales_Clean  <- rbind(Sales_Clean ,Sales_Subset)
  print(i)    
}
rm(Sales_Subset,df)

#-------------------------------Generate Forecasts------------------------------

Sales_Subset_train <- Sales_Clean[DV_Renamed=="DV540",]

y_org <- ts(Sales_Subset_train$Sales,start = c(Sales_Subset_train$ACCTG_YR[1],Sales_Subset_train$ACCTG_WK[1]),frequency = 52)
y_log <- ts(Sales_Subset_train$Sales_Log,start = c(Sales_Subset_train$ACCTG_YR[1],Sales_Subset_train$ACCTG_WK[1]),frequency = 52)
y_clean <- ts(Sales_Subset_train$Sales_TSClean,start = c(Sales_Subset_train$ACCTG_YR[1],Sales_Subset_train$ACCTG_WK[1]),frequency = 52)

plot(y_org)
lines(y_clean,col="red")

STLF_fcst_y = stlf(y, h = 3, s.window = "periodic")
STLF_fcst_y <- as.data.frame(STLF_fcst_y$mean[1:3])
colnames(STLF_fcst_y)=c("STLF_Y_Org")

STLF_fcst_y_clean = stlf(y_clean, h = 3, s.window = "periodic")
STLF_fcst_y_clean <- as.data.frame(STLF_fcst_y_clean$mean[1:3])
colnames(STLF_fcst_y_clean)=c("STLF_Y_Cleaned")

STLF_fcst_y_log = stlf(y_log, h = 3, s.window = "periodic")
STLF_fcst_y_log <- as.data.frame(exp(STLF_fcst_y_log$mean[1:3]))
colnames(STLF_fcst_y_log)=c("STLF_Y_Log")

ETS_fcst_y = ets(y[2:105])
ETS_fcst_y <-forecast::forecast(ETS_fcst_y,h=3)
ETS_fcst_y <- as.data.frame(ETS_fcst_y$mean[1:3])
colnames(ETS_fcst_y)=c("ETS_Y_Org")

ETS_fcst_y_log = ets(y_log[2:105])
ETS_fcst_y_log <-forecast::forecast(ETS_fcst_y_log,h=3)
ETS_fcst_y_log <- as.data.frame(exp(ETS_fcst_y_log$mean[1:3]))
colnames(ETS_fcst_y_log)=c("ETS_Y_Log")

Sales_Subset_test <- Sales_test[DV_Renamed=="DV540",]

Sales_Subset_test <- cbind(Sales_Subset_test,STLF_fcst_y,STLF_fcst_y_clean,STLF_fcst_y_log,ETS_fcst_y,ETS_fcst_y_log)

#-----------------------------Accuracy Calculation------------------------------
#Weekly MAPE
Sales_Subset_test[,STLF_fcst_y_Error:= abs(STLF_fcst_y-`Sales`)]
Sales_Subset_test[,STLF_fcst_y_clean_Error:= abs(STLF_fcst_y_clean-`Sales`)]
Sales_Subset_test[,STLF_fcst_y_log_Error:= abs(STLF_fcst_y_log-`Sales`)]
Sales_Subset_test[,ETS_fcst_y_Error:= abs(ETS_fcst_y-`Sales`)]
Sales_Subset_test[,ETS_fcst_y_log_Error:= abs(ETS_fcst_y_log-`Sales`)]

Forecast_Accuracy<-Sales_Subset_test[,list(`Actual Sell In`=sum(`Sales`)
                                             ,STLF_y_Accuracy=round((1-(sum(STLF_fcst_y_Error)/sum(`Sales`)))*100,2)
                                             ,STLF_y_Clean_Accuracy=round((1-(sum(STLF_fcst_y_clean_Error)/sum(`Sales`)))*100,2)
                                             ,STLF_y_log_Accuracy=round((1-(sum(STLF_fcst_y_log_Error)/sum(`Sales`)))*100,2)
                                             ,ETS_y_Accuracy=round((1-(sum(ETS_fcst_y_Error)/sum(`Sales`)))*100,2)
                                             ,ETS_y_log_Accuracy=round((1-(sum(ETS_fcst_y_log_Error)/sum(`Sales`)))*100,2)),
                                       by=list(DV_Renamed)]

Forecast_Accuracy

#--------------------------Generate Dataframe with Future time stamps-----------------------
# date_vec <- c((max(Sales$Year_Week)+1):201752,201801:201813)
# length(date_vec)
# 
# `Product Number` <- as.data.frame(unique(Sales[,c("DV_Renamed","Prod Category")]))
# 
# merged_combos_oot <- merge(x=`Product Number`,y=date_vec)
# colnames(merged_combos_oot) <- c("DV_Renamed","Prod Category","Year_Week")
# merged_combos_oot$ACCTG_YR <- as.integer(substr(merged_combos_oot$Year_Week,0,4))
# merged_combos_oot$ACCTG_WK <- as.integer(substr(merged_combos_oot$Year_Week,5,6))
# 
# Month_Map <- as.data.frame(unique(Sales[,c("ACCTG_WK","ACCTG_MTH")]))
# merged_combos_oot <- merge(x=`merged_combos_oot`,y=Month_Map)
# merged_combos_oot$Year_Month <- merged_combos_oot$ACCTG_YR*100 + merged_combos_oot$ACCTG_MTH
# 
# Sales_dtps_removed<- merge(x=merged_combos_oot,y=Sales_dtps_removed,all.x = T)
# setDT(Sales_dtps_removed)
# Sales_dtps_removed <- Sales_dtps_removed[order(DV_Renamed,Year_Week),]
