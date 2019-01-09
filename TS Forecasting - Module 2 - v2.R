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
library(dplyr)

rm(list=ls())

#-------------------------------Reading Dataset------------------------------------------------
Sales <- read.csv("//Sample_Data_Pand.csv",stringsAsFactors = F,check.names = F,strip.white = T)
setDT(Sales)

#Corrections in Dataset
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV136","CatD",`Prod Category`)]
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV496","CatE",`Prod Category`)]
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV521","CatE",`Prod Category`)]
Sales[,`Prod Category`:=ifelse(DV_Renamed=="DV601","CatE",`Prod Category`)]

#------------------Aggregate the dataset to required level of forecasting-----------------------
Sales <- Sales[,list(Sales=sum(Sales))
                   ,by=list(ACCTG_YR,ACCTG_MTH,ACCTG_WK,Year_Month,Year_Week,`DV_Renamed`,`Prod Category`)]

min(Sales$Year_Week)
max(Sales$Year_Week)

#---------------------------------Generating Date Vec-------------------------------------------
date_vec <- c(201544:201552,201601:201652,201701:201747)
length(date_vec)

NbrDtpts_df <- Sales[, .(NbrDtpts = length(unique(Year_Week))), by = c('DV_Renamed','Prod Category')]
summary(NbrDtpts_df)

#Removing Weeks
Sales_dtps_removed <- Sales[!(Year_Week %in% c(201625,201626) & `Prod Category`=="CatE"),]


NbrDtpts_df_1 <- Sales_dtps_removed[, .(NbrDtpts = length(unique(Year_Week))), by = c('DV_Renamed','Prod Category')]
summary(NbrDtpts_df_1)

length(date_vec)
#Identification of 2 Weeks as Missing Data Points

#-----------------------------Generate Blank Dataframe with all the dates----------------------------
`Product Number` <- as.data.frame(unique(Sales[,c("DV_Renamed","Prod Category")]))

merged_combos_oot <- merge(x=`Product Number`,y=date_vec)
colnames(merged_combos_oot) <- c("DV_Renamed","Prod Category","Year_Week")
merged_combos_oot$ACCTG_YR <- as.integer(substr(merged_combos_oot$Year_Week,0,4))
merged_combos_oot$ACCTG_WK <- as.integer(substr(merged_combos_oot$Year_Week,5,6))

Month_Map <- as.data.frame(unique(Sales[,c("ACCTG_WK","ACCTG_MTH")]))
merged_combos_oot <- merge(x=`merged_combos_oot`,y=Month_Map)
merged_combos_oot$Year_Month <- merged_combos_oot$ACCTG_YR*100 + merged_combos_oot$ACCTG_MTH

Sales_dtps_removed<- merge(x=merged_combos_oot,y=Sales_dtps_removed,all.x = T)
setDT(Sales_dtps_removed)
Sales_dtps_removed <- Sales_dtps_removed[order(DV_Renamed,Year_Week),]

#-------------------------Sample DV Rolling Average Generation------------------
Sales_Imputed=Sales_dtps_removed%>%group_by(DV_Renamed)%>%
  mutate(ma_4 = lag(rollapplyr(Sales, width = 4, mean, fill = NA, partial = T,na.rm=T,align = "r")))%>%
  data.table()

# 
# #Manual Way to generate Moving Averages
# abc<- Sales_dtps_removed[DV_Renamed=="DV540",]
# ma132 <- rollapply(abc$Sales,4,mean,na.rm=T)
# ma132
# length(ma132)
# 
# date_vec_ma <-date_vec[5:length(date_vec)]
# length(date_vec_ma)
# 
# ma132<-cbind(date_vec_ma,ma132)
# 
# #Rollapply using the particular week's value to compute. Offset dates by one week before  merging. removing Last repeating value
# ma132 <- ma132[1:(nrow(ma132)-1),]
# 
# #i <- "DV540"
# 
# #----------------------Looping through SKU's to generate Rolling Average and Storing output------------
# Sales_Imputed <- data.frame()
# for(i in unique(Sales_dtps_removed$DV_Renamed))
# {
#   abc<- Sales_dtps_removed[DV_Renamed==i,]
#   ma_4 <- rollapply(abc$Sales,4,mean,na.rm=T)
#   date_vec_ma <-date_vec[5:length(date_vec)]
#   
#   ma_4<-cbind(date_vec_ma,ma_4)
#   ma_4 <- ma_4[1:(nrow(ma_4)-1),]
#   
#   colnames(ma_4) <- c("Year_Week","ma_4")
#   abc<- merge(abc,ma_4,all.x = T)
#   Sales_Imputed <- rbind(Sales_Imputed,abc)
#   print(i)    
# }

Sales_Imputed[,Sales:=ifelse(is.na(Sales),round(ma_4,0),Sales)]
Sales_Imputed$ma_4 <- NULL

Sales_Imputed <- Sales_Imputed[,list(Sales=sum(Sales))
                   ,by=list(ACCTG_YR,ACCTG_MTH,ACCTG_WK,Year_Month,Year_Week,`DV_Renamed`,`Prod Category`)]

NbrDtpts_df <- Sales_Imputed[, .(NbrDtpts = length(unique(Year_Week))), by = c('DV_Renamed','Prod Category')]
summary(NbrDtpts_df)
sum(is.na(Sales_Imputed$Sales))
Sales <- Sales_Imputed
rm(list= ls()[!(ls() %in% c('Sales'))])

#------------------Alternate Method to Impute - ImputeTS------------------------
Sales_dtps_removed <- Sales[!(Year_Week %in% c(201625,201626) & `Prod Category`=="CatE"),]

#Generate Blank Dataframe with all the dates
date_vec <- c(201544:201552,201601:201652,201701:201747)
length(date_vec)

`Product Number` <- as.data.frame(unique(Sales[,c("DV_Renamed","Prod Category")]))

merged_combos_oot <- merge(x=`Product Number`,y=date_vec)
colnames(merged_combos_oot) <- c("DV_Renamed","Prod Category","Year_Week")
merged_combos_oot$ACCTG_YR <- as.integer(substr(merged_combos_oot$Year_Week,0,4))
merged_combos_oot$ACCTG_WK <- as.integer(substr(merged_combos_oot$Year_Week,5,6))

Month_Map <- as.data.frame(unique(Sales[,c("ACCTG_WK","ACCTG_MTH")]))
merged_combos_oot <- merge(x=`merged_combos_oot`,y=Month_Map)
merged_combos_oot$Year_Month <- merged_combos_oot$ACCTG_YR*100 + merged_combos_oot$ACCTG_MTH

Sales_dtps_removed<- merge(x=merged_combos_oot,y=Sales_dtps_removed,all.x = T)
setDT(Sales_dtps_removed)
Sales_dtps_removed <- Sales_dtps_removed[order(DV_Renamed,Year_Week),]
abc<- Sales_dtps_removed[DV_Renamed=="DV540",]

abc_ts <- ts(abc$Sales,start = c(abc$ACCTG_YR[1],abc$ACCTG_WK[1]),frequency = 52)

imputeTS::na.ma(abc_ts,k=4,weighting = "simple")

  #The value would not match previous method as the mean is calculated as:
  #For an NA value at position i of a time series, the observations i-1,i+1 and i+1, i+2 
  #(assuming a window size of k=2) are used to calculate the mean.

#Use of Linear Regression for forecasting/imputing TS with Trend   
rm(list= ls())

Yellowstone_df <- read.csv("//Yellowstone_df.csv",stringsAsFactors = F,check.names = F,strip.white = T)
setDT(Yellowstone_df)
Yellowstone_df <- Yellowstone_df[order(Year)]
str(Yellowstone_df)
plot(Yellowstone_df$`Number of Visitors`,type="l")
Yellowstone_df_train <- Yellowstone_df[Year>1946&Year<2010,]

Yellowstone_df_train$Period <- as.numeric(rownames(Yellowstone_df_train))
lm_mod <- lm(`Number of Visitors` ~ Period, data=Yellowstone_df_train)
summary(lm_mod)


plot(Yellowstone_df_train$`Number of Visitors`,type="l")
abline(lm_mod,Yellowstone_df_train,col="red")

Yellowstone_df_test <- Yellowstone_df[Year>2010,]
Yellowstone_df_test$Period <- (max(Yellowstone_df_train$Period)+1):(nrow(Yellowstone_df_test)+max(Yellowstone_df_train$Period))
Yellowstone_df_test$Predicted_Visitors <- predict(lm_mod,Yellowstone_df_test)

Yellowstone_df_pred <- rbind(Yellowstone_df_train,Yellowstone_df_test,fill=T)
plot(Yellowstone_df_pred$`Number of Visitors`,type="l")
lines(Yellowstone_df_pred$Predicted_Visitors,col="blue")

#-----------------------Seasonal Indexes--------------------------------------------
fpp::austourists
autoplot(ausbeer) 
ggseasonplot(austourists) 
ggseasonplot(austourists,polar = T)

df <- data.frame(date=as.Date(time(austourists)), tourists = melt(austourists)$value)
df$Quarter <- as.yearqtr(df$date, format = "%Y-%m-%d")

write.csv(df,"Seasonal_Index.csv",row.names = F)

# Assignment:
#1. Generate Exponential & Weighted Moving Averages. Use movavg() from Library - pracma
#2. Predict the demand for ausair data for period 2010-2015 using a regression model.

