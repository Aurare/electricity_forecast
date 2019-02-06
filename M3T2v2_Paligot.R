library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(tibbletime)
library(hms)
library(fpp2)
library(plyr)
library(forecast)
library(RColorBrewer)
library(GGally)
library(seasonal)
library(gridExtra)

setwd("~/Ubiqum/Module 3_Deep Analytics/Task 2")

powerdata <- read.csv("Data/household_power_consumption.txt", 
                      stringsAsFactors = FALSE, 
                      sep = ";", 
                      header = TRUE)

################-- 0.1. formatage des données --#####################################################################

##transformation des facteurs au bon format
##A/ transformation des colonnes 3 à 9 au format numérique with a loop
for (i in 3:9) 
  powerdata[, i] <- as.numeric(powerdata[, i])
##B/ réunion des colonnes date et time en une
#1- lie les deux colonnes ensemble
powerdata <-cbind(powerdata,paste(powerdata$Date,powerdata$Time), 
                  stringsAsFactors=FALSE) #lie les deux colonnes ensemble
#2 - nomme la colonne (inscription dans le df)
colnames(powerdata)[10] <-"DateTime"
#3 - déplace la colonne de 10 à 1 (position dans le df)
powerdata <- powerdata[,c(ncol(powerdata), 1:(ncol(powerdata)-1))]
#4 - transformation du format de la nouvelle colonne
#OlsonNames(): permet de voir tous les fuseaux horaires
powerdata$DateTime <- strptime(powerdata$DateTime, "%d/%m/%Y %H:%M:%S", tz="GMT")
powerdata$DateTime<- as.POSIXct(powerdata$DateTime, tz="GMT") #to solve error: as.POSIXct(x, tz = "", ...)
powerdata$Date <- as.POSIXct(powerdata$Date, "%d/%m/%Y", tz="GMT")#colonne Date
powerdata$DateTime<- ymd_hms(powerdata$DateTime, tz="GMT")
#5 - transform Time colomn at time format
powerdata$Time<-as.hms(powerdata$Time, tz="GMT")
#6 - creating a tible with the dataframe: 
powerdata<- as_tbl_time(powerdata, index=DateTime)
##C/ Création d'une nouvelle colonne = l'énergie consommée en dehors des pièces sans submeters
powerdata <- powerdata %>% mutate(No_submeter = Global_active_power*1000/60-Sub_metering_1 - Sub_metering_2 - Sub_metering_3)
##D/ Création d'une nouvelle colonne avec GAP transformed in kWhour too
powerdata <- powerdata %>% mutate (GAP_kwh = (Global_active_power*1000/60))
##E/ suppression données 2006
powerdata <- powerdata %>% filter(year(DateTime) != 2006)
##F/ NAS
powerdata <-fill(powerdata, Global_intensity, Global_reactive_power, Global_active_power,
                 Voltage, Sub_metering_3, Sub_metering_2, Sub_metering_1, No_submeter, GAP_kwh)

################-- 0.2. subsampling with timetibble --#################################################

##adding periodic colomns to the dataframe
epowerdata <- powerdata%>%
  dplyr::mutate(QuarterHour = collapse_index(DateTime, "15 minutes"))%>%
  dplyr::mutate(Hourly= collapse_index(DateTime, "hours"))%>%
  dplyr::mutate(Fourhours = collapse_index(DateTime, "4 hours", tz="GMT"))%>%
  dplyr::mutate(DayNight = collapse_index(DateTime, "12 hours"))%>%
  dplyr::mutate(Weekly = collapse_index(DateTime, "weekly"))%>%
  dplyr::mutate(Monthly = collapse_index(DateTime, "months"))%>%
  dplyr::mutate(Quarterly = collapse_index(DateTime, "Q"))%>%
  dplyr::mutate(Yearly = collapse_index(DateTime, "Y"))%>%
  dplyr::mutate(Daily = collapse_index(DateTime, "D"))%>%
  dplyr::mutate(Days=wday(Date, label=TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 2)))%>%
  dplyr::mutate(DaysMonth=day(Date))%>%
  dplyr::mutate(Years=year(Yearly))%>%
  dplyr::mutate(Quarters=quarter(Quarterly))%>%
  dplyr::mutate(Months=month(Monthly, label=TRUE, abbr=FALSE))%>%
  dplyr::mutate(Weeks=week(Weekly))

epowerdata$Weeks<-as.factor(epowerdata$Weeks)
epowerdata$DaysMonth<-as.factor(epowerdata$DaysMonth)
epowerdata$Years<-as.factor(epowerdata$Years)
epowerdata$Quarters<-as.factor(epowerdata$Quarters)

################-- 0.3. création de time aggrégation --###########################################
##select the positions of the interesting factors for POWERDATA sample
positions<-c(8:12) #seulement pour les variables numériques
positions_epow<-c(8:27)

##DAY-HOUR - Powerdata: agregated by days and hours
pd_hourly<-powerdata %>% group_by(date(DateTime),hour(DateTime)) %>% 
  summarise_if(is.numeric, sum)

pd_hourly<-unite(pd_hourly, DateHour, `date(DateTime)`, `hour(DateTime)` )#new colomn
pd_hourly$DateHour <-ymd_h(pd_hourly$DateHour)#rename

pd_hourly<-pd_hourly%>%
  dplyr::mutate(Weekly = collapse_index(DateHour, "weekly"))%>%
  dplyr::mutate(Monthly = collapse_index(DateHour, "months"))%>%
  dplyr::mutate(Quarterly = collapse_index(DateHour, "Q"))%>%
  dplyr::mutate(Yearly = collapse_index(DateHour, "Y"))%>%
  dplyr::mutate(Daily = collapse_index(DateHour, "D"))%>%
  dplyr::mutate(Days=wday(DateHour, label=TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 2)))%>%
  dplyr::mutate(DaysMonth=day(DateHour))%>%
  dplyr::mutate(Years=year(Yearly))%>%
  dplyr::mutate(Quarters=quarter(Quarterly))%>%
  dplyr::mutate(Months=month(Monthly, label=TRUE, abbr=FALSE))%>%
  dplyr::mutate(Weeks=week(Weekly))

pd_hourly$Weeks<-as.factor(pd_hourly$Weeks)
pd_hourly$DaysMonth<-as.factor(pd_hourly$DaysMonth)
pd_hourly$Years<-as.factor(pd_hourly$Years)
pd_hourly$Quarters<-as.factor(pd_hourly$Quarters)

pd_hourly<-as_tbl_time(pd_hourly, index=DateHour)#time tibble

##DAYS - Powerdata: agregated by days
pd_daily<-epowerdata %>% group_by(Date) %>% summarise_if(is.numeric,sum)

pd_daily<-pd_daily%>%
  dplyr::mutate(Weekly = collapse_index(Date, "weekly"))%>%
  dplyr::mutate(Monthly = collapse_index(Date, "months"))%>%
  dplyr::mutate(Quarterly = collapse_index(Date, "Q"))%>%
  dplyr::mutate(Yearly = collapse_index(Date, "Y"))%>%
  dplyr::mutate(Daily = collapse_index(Date, "D"))%>%
  dplyr::mutate(Days=wday(Date, label=TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 2)))%>%
  dplyr::mutate(DaysMonth=day(Date))%>%
  dplyr::mutate(Years=year(Yearly))%>%
  dplyr::mutate(Quarters=quarter(Quarterly))%>%
  dplyr::mutate(Months=month(Monthly, label=TRUE, abbr=FALSE))%>%
  dplyr::mutate(Weeks=week(Weekly))

pd_daily$Weeks<-as.factor(pd_daily$Weeks)
pd_daily$DaysMonth<-as.factor(pd_daily$DaysMonth)
pd_daily$Years<-as.factor(pd_daily$Years)
pd_daily$Quarters<-as.factor(pd_daily$Quarters)

pd_daily<-as_tbl_time(pd_daily, index = Date)#time tibble

write.table(pd_daily, file= "Daily_electricity_consumption", sep = ",", dec = ".",
            row.names = TRUE, col.names = TRUE)

## DAYS of the week, every day per hour
Sun_h<- pd_hourly%>%filter(Days=="dimanche")%>%as_tbl_time(index=DateHour)
Mon_h<- pd_hourly%>%filter(Days=="lundi")%>%as_tbl_time(index=DateHour)
Tue_h<- pd_hourly%>%filter(Days=="mardi")%>%as_tbl_time(index=DateHour)
Wen_h<- pd_hourly%>%filter(Days=="mercredi")%>%as_tbl_time(index=DateHour)
Thu_h<- pd_hourly%>%filter(Days=="jeudi")%>%as_tbl_time(index=DateHour)
Fri_h<- pd_hourly%>%filter(Days=="vendredi")%>%as_tbl_time(index=DateHour)
Sat_h<- pd_hourly%>%filter(Days=="samedi")%>%as_tbl_time(index=DateHour)

ts_sun_h<-ts(Sun_h[,2:17], start(c(2007,1)), frequency=24*52)##WRONG!!!!
ts_mon_h<-ts(Mon_h[,2:17], start(c(2007,1)), frequency=24*52)
ts_tue_h<-ts(Tue_h[,2:17], start(c(2007,1)), frequency=24*52)
ts_wen_h<-ts(Wen_h[,2:17], start(c(2007,1)), frequency=24*52)
ts_thu_h<-ts(Thu_h[,2:17], start(c(2007,1)), frequency=24*52)
ts_fri_h<-ts(Fri_h[,2:17], start(c(2007,1)), frequency=24*52)
ts_sat_h<-ts(Sat_h[,2:17], start(c(2007,1)), frequency=24*52)

## DAYS of the week, one data point per day
Sun<- pd_daily%>%filter(Days=="dimanche")%>%as_tbl_time(index=Date)
Mon<- pd_daily%>%filter(Days=="lundi")%>%as_tbl_time(index=Date)
Tue<- pd_daily%>%filter(Days=="mardi")%>%as_tbl_time(index=Date)
Wen<- pd_daily%>%filter(Days=="mercredi")%>%as_tbl_time(index=Date)
Thu<- pd_daily%>%filter(Days=="jeudi")%>%as_tbl_time(index=Date)
Fri<- pd_daily%>%filter(Days=="vendredi")%>%as_tbl_time(index=Date)
Sat<- pd_daily%>%filter(Days=="samedi")%>%as_tbl_time(index=Date)

ts_sun<-ts(Sun[,2:21], start(c(2007,1)), frequency=52)##WRONG!!!
ts_mon<-ts(Mon[,2:21], start(c(2007,1)), frequency=52)
ts_tue<-ts(Tue[,2:21], start(c(2007,1)), frequency=52)
ts_wen<-ts(Wen[,2:21], start(c(2007,1)), frequency=52)
ts_thu<-ts(Thu[,2:21], start(c(2007,1)), frequency=52)
ts_fri<-ts(Fri[,2:21], start(c(2007,1)), frequency=52)
ts_sat<-ts(Sat[,2:21], start(c(2007,1)), frequency=52)

## DAYS of the week: sums
pd_daily_wdays<-pd_daily%>%
  group_by(wday(Daily, label = TRUE, abbr=FALSE))%>%
  summarise_if(is.numeric,sum)

pd_daily_wdays$Days<-pd_daily_wdays$`wday(Daily, label = TRUE, abbr = FALSE)`
#pd_daily_wdays<-as_tbl_time(pd_daily_wdays, index=Days)# problem bec "Days" in the current format is not time based

ts_daily_wdays<-ts(pd_daily_wdays,start=1, frequency = 7)

##MONTHS - ePowerdata agregated by months for every year

pd_monthly<-epowerdata %>% group_by(Monthly) %>% #,using the preset grouping made with tbltime
  summarise_if(is.numeric,sum)

pd_monthly<-as_tbl_time(pd_monthly, index = Monthly)#time tibble

saveRDS(pd_monthly, file = "Monthly_electricity_consumption")## save a monthly data file
write.table(pd_monthly, file= "Monthly_electricity_consumption", sep = ",", dec = ".",
            row.names = TRUE, col.names = TRUE)

################-- 0.4. correlations between the submeters --#######################################
ts_monthly %>%
  as.data.frame() %>%
  GGally::ggpairs()

################-- 0.5. création de time intervals --#############################
## Each years, agregated hourly
hourly2007<-filter_time(pd_hourly,~"2007")
hourly2008<-filter_time(pd_hourly,~"2008")
hourly2009<-filter_time(pd_hourly,~"2009")
hourly2010<-filter_time(pd_hourly,~"2010")

## Each years agregated daily
daily2007<-filter_time(pd_daily,~"2007")
daily2008<-filter_time(pd_daily,~"2008")
daily2009<-filter_time(pd_daily,~"2009")
daily2010<-filter_time(pd_daily,~"2010")

## Each years, agregated monthly
monthly2007<-filter_time(pd_monthly,~"2007")
monthly2008<-filter_time(pd_monthly,~"2008")
monthly2009<-filter_time(pd_monthly,~"2009")
monthly2010<-filter_time(pd_monthly,~"2010")

################-- 0.6. création de time séries  --################################
## TS DAYHOUR  - create TS with pd_hourly : all data from 2007, at hour format
ts_hourly = ts(pd_hourly[, 6:21], #colomn without the time related one
                start = c(2007, 01), 
                frequency = 24*365.25) 

##TS DAY - reate a TS with pd_daily: data agregated by every day
ts_daily = ts(pd_daily[, 6:21], #colomn without the time related one
               start = c(2007, 01), #start: year + month
               frequency = 365.25) #frequency: 365.25 jours

#TS MONTHLY
ts_monthly= ts(pd_monthly[, 6:10], #colomn without the time related one
               start = c(2007, 01), #start: year + month
               frequency = 12)

################-- 1.1. Simple Forecasting Methods: Monthly intervals, no test set --#####################################
##All the data in this section (point1.1. to ...1.n) uses the pd_monthly tibble
##with one data point per monthe and summurized by SUMM
## For these simple forecats, we define a train and test set, and we can fit and forecast
## in one formula (doing both at the same time). The accuracy may be measured by a 
## cross validation process

### MEAN forecasting ###
## Model (model with all data, without division for training and testing test)
f1_all<- meanf(ts_monthly[,"GAP_kwh"], h = 10, level = c(80, 95))
f1_s1_all<- meanf(ts_monthly[,"Sub_metering_1"], h = 11, level = c(80, 95))
f1_s2_all<- meanf(ts_monthly[,"Sub_metering_2"], h = 11, level = c(80, 95))
f1_s3_all<- meanf(ts_monthly[,"Sub_metering_3"], h = 11, level = c(80, 95))
f1_ns_all<- meanf(ts_monthly[,"No_submeter"], h = 11, level = c(80, 95))

## Graph
autoplot(f1_all)


## Same, but without the confidence interval for GAP
autoplot(ts_monthly[,"GAP_kwh"])+
  autolayer(f1_all, series="Mean GAP", PI=FALSE)

## Plot all submeter with mean model together
autoplot(ts_monthly[,"GAP_kwh"])+
  autolayer(ts_monthly[,"Sub_metering_1"])+
  autolayer(ts_monthly[,"Sub_metering_2"])+
  autolayer(ts_monthly[,"Sub_metering_3"])+
  autolayer(ts_monthly[,"No_submeter"])+
  autolayer(f1_all, series="Mean GAP", PI=FALSE)+
  autolayer(f1_ns_all,series="Mean No sub", PI=FALSE)+
  autolayer(f1_s1_all, series="Mean Sub1", PI=FALSE)+
  autolayer(f1_s2_all, series="Mean Sub2", PI=FALSE)+
  autolayer(f1_s3_all,series="Mean Sub3", PI=FALSE)
      
## Residuals
checkresiduals(f1)

################-- 1.2. Simple Forecasting Methods: Monthly intervals, TEST SETS --######################

## Creating a testing set avec 10 mois en moins: j'enlève 2010

ts_monthly_train<- window(ts_monthly, start=2007, end=c(2009,12))

## Forecast on the testing sets  
f1 <- meanf(ts_monthly_train[,"GAP_kwh"], h = 11, level = c(80, 95))#mean
f2 <- rwf(ts_monthly_train[,"GAP_kwh"], h = 11, level = c(80, 95))#naive
f3 <- snaive(ts_monthly_train[,"GAP_kwh"], h = 11, level = c(80, 95))#seasonal naive
f4 <- rwf(ts_monthly_train[,"GAP_kwh"], h = 11, drift=TRUE, level = c(80, 95))#drift

##simple plots
autoplot(f1)
autoplot(f2)
autoplot(f3)
autoplot(f4)

## Representation
autoplot(window(ts_monthly[,"GAP_kwh"], start=2007))+
  autolayer(f1, series="Mean", PI=FALSE)+
  autolayer(f2, series="Naïve", PI=FALSE) +
  autolayer(f3, series="Seasonal naïve", PI=FALSE) +
  autolayer(f4, series="Drift", PI=FALSE)+
  xlab("Year") + ylab("kwh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

## Metrics
# Residuals
  checkresiduals(f1)
  checkresiduals(f2)
  checkresiduals(f3)
  checkresiduals(f4)

#Test interval
ts_monthly_test <- window(ts_monthly, start=2010)

#Accuracy
accuracy(f1, ts_monthly_test[,"GAP_kwh"])
accuracy(f2, ts_monthly_test[,"GAP_kwh"])
accuracy(f3, ts_monthly_test[,"GAP_kwh"])
accuracy(f4, ts_monthly_test[,"GAP_kwh"])

## Cross validation
f1_cv <- tsCV(ts_monthly_train[,"GAP_kwh"], meanf, h=11)
f2_cv <- tsCV(ts_monthly_train[,"GAP_kwh"], rwf, h=11)
f3_cv <- tsCV(ts_monthly_train[,"GAP_kwh"], snaive, h=11)
f4_cv <- tsCV(ts_monthly_train[,"GAP_kwh"], rwf, h=11, drift=TRUE)

## Cross validation RMSE
sqrt(mean(f1_cv^2, na.rm=TRUE))
sqrt(mean(f2_cv^2, na.rm=TRUE))
sqrt(mean(f3_cv^2, na.rm=TRUE))
sqrt(mean(f4_cv^2, na.rm=TRUE))

## residuals : pipe structure  : mean
ts_monthly_train[,"GAP_kwh"] %>% meanf %>% residuals()->resf1
resf1 %>% mean(na.rm=TRUE)

ts_monthly_train[,"GAP_kwh"] %>% rwf %>% residuals()->resf2
resf2 %>% mean(na.rm=TRUE)

ts_monthly_train[,"GAP_kwh"] %>% snaive %>% residuals()->resf3
resf3 %>% mean(na.rm=TRUE)

ts_monthly_train[,"GAP_kwh"] %>% rwf(drift=TRUE) %>% residuals()->resf4
resf4 %>% mean(na.rm=TRUE)

##summaries

summary(f1)
################-- 2.1. REGRESSION MODEL with TSLM -all data set --################################################################
## With the regression model, we use a two-steps methods: the model (tslm) is applied to
## the train set, then forecast() is applied to the test set. The most accurate model
## is the one with the lowest RMSE values calculated with accuracy(forecats,testset)
## Uses: ts_monthly summed
 
##1 - on ALL the data set

#model all data: TREND + SEASON
tslm.powerdata <- tslm(ts_monthly ~ trend + season) #calculated for all parameters (submeters)
summary(tslm.powerdata)

#modeal GAP - TREND+ SEASON
tslm_gap_ts <- tslm(ts_monthly[,"GAP_kwh"] ~ trend + season)

#graph on actual and fitted data(all dataset)
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(fitted(tslm_gap_ts), series="Fitted") +
  xlab("Year") + ylab("Global Active Power (kwh)") +
  ggtitle("Monthly energy consumption")

autoplot(fitted(tslm_gap_ts))

#graph of actual and fitted data with 0,1 slopes, trend + season
cbind(Data = ts_monthly[,"GAP_kwh"],
      Fitted = fitted(tslm_gap_ts)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Monthly energy consumption") +
  geom_abline(intercept=0, slope=1)

## Plots of the predictors : months, againts fitted values
## Should show no pattern

df <- as.data.frame(pd_monthly)
df[,"Residuals"]  <- as.numeric(residuals(tslm_gap_ts))
p1 <- ggplot(df, aes(x=as.factor(month(Monthly)), y=Residuals)) +
  geom_point()
p1

## plot residuals againts fitted values
## Should show no patter,

cbind(Fitted = fitted(tslm_gap_ts),
      Residuals=residuals(tslm_gap_ts)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()


#summary model
summary(tslm_gap_ts)

#check residuals
checkresiduals(tslm_gap_ts)

####

#modeal GAP - TREND
tslm_gap_t <- tslm(ts_monthly[,"GAP_kwh"] ~ trend)


#graph on actual and fitted data(all dataset)
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(fitted(tslm_gap_t), series="Fitted") +
  xlab("Year") + ylab("Global Active Power (kwh)") +
  ggtitle("Monthly energy consumption")

#graph actual vs fitted
cbind(Data = ts_monthly[,"GAP_kwh"],
      Fitted = fitted(tslm_gap_t)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Monthly energy consumption") +
  geom_abline(intercept=0, slope=1)

summary(tslm_gap_t)

#### 

#modeal GAP - SEASON
tslm_gap_s <- tslm(ts_monthly[,"GAP_kwh"] ~ season)


#graph on actual and fitted data(all dataset)
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(fitted(tslm_gap_s), series="Fitted") +
  xlab("Year") + ylab("Global Active Power (kwh)") +
  ggtitle("Monthly energy consumption")

summary(tslm_gap_s)

##Monthly plot

cbind(Data=ts_monthly[,"GAP_kwh"], Fitted=fitted(tslm_gap_ts)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(ts_monthly[,"GAP_kwh"])))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Monthly energy consumption") +
  scale_colour_brewer(palette="Paired", name="Months") +
  geom_abline(intercept=0, slope=1)

#plot for model with trend
cbind(Data=ts_monthly[,"GAP_kwh"], Fitted=fitted(tslm_gap_t)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(ts_monthly[,"GAP_kwh"])))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Trend") +
  geom_abline(intercept=0, slope=1)

##comparing metrics of the 3 models
CV(tslm_gap_s)
CV(tslm_gap_t)
CV(tslm_gap_ts)

###

##2/ Forecasting using all the dataset


f_ts <- forecast(tslm_gap_ts)
f_t <- forecast(tslm_gap_t)
f_s <-forecast(tslm_gap_s)

autoplot(f_ts) +
  ggtitle("Forecasts of energy consumption using regression") +
  xlab("Year") + ylab("kwh")

autoplot(f_s) +
  ggtitle("Forecasts of energy consumption using regression") +
  xlab("Year") + ylab("kwh")

autoplot(f_t) +
  ggtitle("Forecasts of energy consumption using regression") +
  xlab("Year") + ylab("kwh")

summary(f_ts)

checkresiduals(f_ts)

################-- 2.2. TSLM trend+season using TEST and TRAIN test --################################
## definition of the test & train set
ts_monthly_train<- window(ts_monthly, start=2007, end=c(2009,12))
ts_monthly_test <- window(ts_monthly, start=2010)

## model on train test
tslm_train_gap_ts <- tslm(ts_monthly_train[,"GAP_kwh"]~ trend + season)
tslm_train_gap_t <- tslm(ts_monthly_train[,"GAP_kwh"] ~ trend )
tslm_train_gap_s <- tslm(ts_monthly_train[,"GAP_kwh"] ~ season)

## forecast 

f_test_ts <- forecast(tslm_train_gap_ts, h=11, level = c(80, 95))
f_test_t <- forecast(tslm_train_gap_t, h=11, level = c(80, 95))
f_test_s <- forecast(tslm_train_gap_s, h=11, level = c(80, 95))

## plots
autoplot(f_test_ts)
autoplot(f_test_s)
autoplot(f_test_t)

## Representation
autoplot(window(ts_monthly[,"GAP_kwh"], start=2007))+
  autolayer(f_test_s, series="Months", PI=FALSE)+
  autolayer(f_test_t, series="Trend", PI=FALSE) +
  autolayer(f_test_ts, series="Trend+Months", PI=FALSE) +
  xlab("Year") + ylab("kwh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

# Residuals
checkresiduals(tslm_train_gap_s)
checkresiduals(tslm_train_gap_t)
checkresiduals(tslm_train_gap_ts)

#Accuracy
accuracy(f_test_ts, ts_monthly_test[,"GAP_kwh"])
accuracy(f_test_t, ts_monthly_test[,"GAP_kwh"])
accuracy(f_test_s, ts_monthly_test[,"GAP_kwh"])

# summary
summary(f_test_ts)
summary(f_test_s)
summary(f_test_t)
summary(tslm_train_gap_s)
summary(tslm_train_gap_t)
summary(tslm_train_gap_ts)

################-- 3.1. Decompose (Montly summed intervals) --#######################################
## Used ts_monthly summed

## Using the decompose function
#a - decompose the elements
dec_tsmonthly_gap <- decompose(ts_monthly[,"GAP_kwh"])
#b - get seasonal values
dec_tsmonthly_gap$seasonal
dec_tsmonthly_gap$trend
dec_tsmonthly_gap$random

decMinusRandom<-ts_monthly[,"GAP_kwh"]-dec_tsmonthly_gap$random #data without the random component

plot(decMinusRandom)
#c - plotting
plot(dec_tsmonthly_gap)

###

#additive plot
ts_monthly[,"GAP_kwh"] %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of electrical consumption")

#multiplicative plot
ts_monthly[,"GAP_kwh"] %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
          of electrical consumption")

##Using the mouving average(ma) function to produce the "trend-cycle"
## 5-MA
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(ma(ts_monthly[,"GAP_kwh"],5), series="5-MA") +
  xlab("Year") + ylab("kWh") +
  ggtitle("Monthly electricity consumption") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))
##10-MA
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(ma(ts_monthly[,"GAP_kwh"],10), series="10-MA") +
  xlab("Year") + ylab("kWh") +
  ggtitle("Monthly electricity consumption") +
  scale_colour_manual(values=c("Data"="grey50","10-MA"="red"),
                      breaks=c("Data","10-MA"))

##15-MA
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(ma(ts_monthly[,"GAP_kwh"],15), series="15-MA") +
  xlab("Year") + ylab("kWh") +
  ggtitle("Monthly electricity consumption") +
  scale_colour_manual(values=c("Data"="grey50","15-MA"="red"),
                      breaks=c("Data","15-MA"))

##12 MA=> correspond à la saison de 12 mois
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(ma(ts_monthly[,"GAP_kwh"],12), series="12-MA") +
  xlab("Year") + ylab("kWh") +
  ggtitle("Monthly electricity consumption") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA"))

###

## Using the X11 function
#seas()
fit<-ts_monthly[,"GAP_kwh"] %>% seas(x11="")

#graph in 4 plots
autoplot(fit) +
  ggtitle("X11 decomposition of electrical consumption")
#graph in one plot
autoplot(ts_monthly[,"GAP_kwh"], series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("GAP(kwh)") +
  ggtitle("Electricity consumption") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))
#sub series month
fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")


################-- 4.1. Exponential smoothing ##############################################
## In the book (7.2), it is suggested to compare the models based on a ONE-STEP forecats 
## (h=1) with the help of a cross-validation process. The best model (lowest MAE et SME) is then
## selected in order to produce the actual forecast
## First: ts_monthly summed

### HoltWinter methods
HoltWinters(ts_monthly[,"GAP_kwh"]) 

HW <- hw(ts_monthly[,"GAP_kwh"])
summary(HW)
autoplot(HW)

###

###ETS modelisation
AAA<-ets(ts_monthly[,"GAP_kwh"],model="AAA")
autoplot(AAA)
AAA

ANA<-ets(ts_monthly[,"GAP_kwh"],model="ANA")
autoplot(ANA)
ANA

### ETS

AAA %>% forecast(h=11) %>%
  autoplot() +
  ylab("Electricity Consumption Forecast")

ANA %>% forecast(h=11) %>%
  autoplot() +
  ylab("Electricity Consumption Forecast")

################-- 6.1. PD_DAILY: graph ########################################################

## One data point per day
autoplot(ts_daily[,"GAP_kwh"])

## One point for all days (7)
autoplot(ts_daily_wdays[,"GAP_kwh"])
plot(pd_daily_wdays$GAP_kwh~pd_daily_wdays$Days)
plot(pd_monthly$GAP_kwh~pd_monthly$Monthly)

## Plot for specific days
autoplot(ts_sun[,"GAP_kwh"])
autoplot(ts_sun_h[,"GAP_kwh"])
autoplot(ts_mon[,"GAP_kwh"])

##wrap for days
ggplot(pd_hourly,aes (hour(DateHour),GAP_kwh)) +
  geom_line() +
  facet_wrap(.~Days)

################-- 6.2. Models with daily data/hour: pd_hourly --####################################################

### MEAN forecasting ###
## Model (model with all data, without division for training and testing test)
f1_hour<- meanf(ts_pdhourly[,"GAP_kwh"], h = 4032, level = c(80, 95))
f1_s1_hour<- meanf(ts_pdhourly[,"Sub_metering_1"], h = 4032, level = c(80, 95))
f1_s2_hour<- meanf(ts_pdhourly[,"Sub_metering_2"], h = 4032, level = c(80, 95))
f1_s3_hour<- meanf(ts_pdhourly[,"Sub_metering_3"], h = 4032, level = c(80, 95))
f1_ns_hour<- meanf(ts_pdhourly[,"No_submeter"], h = 4032, level = c(80, 95))
## Graph
autoplot(f1_hour)

## Same, but without the confidence interval for GAP
autoplot(tail(ts_pdhourly[,"GAP_kwh"], n=8000))+
  autolayer(f1_hour, series="Mean GAP", PI=FALSE)

attr(f1_hour$x,"tsp")

autoplot(f1_hour)+
  coord_cartesian(xlim = c(2010.80, 2011))

## Plot all submeter with mean model together
autoplot(ts_pdhourly[,"GAP_kwh"])+
  autolayer(ts_pdhourly[,"Sub_metering_1"])+
  autolayer(ts_pdhourly[,"Sub_metering_2"])+
  autolayer(ts_pdhourly[,"Sub_metering_3"])+
  autolayer(ts_pdhourly[,"No_submeter"])+
  autolayer(f1_hour, series="Mean GAP", PI=FALSE)+
  autolayer(f1_ns_hour,series="Mean No sub", PI=FALSE)+
  autolayer(f1_s1_hour, series="Mean Sub1", PI=FALSE)+
  autolayer(f1_s2_hour, series="Mean Sub2", PI=FALSE)+
  autolayer(f1_s3_hour,series="Mean Sub3", PI=FALSE)

## Residuals
checkresiduals(f1_hour)
################-- 6.3. PD_daily modeling ######################################################

### MEAN forecasting ###
## Model (model with all data, without division for training and testing test)
f1d<- meanf(ts_daily[,"GAP_kwh"], h = 60, level = c(80, 95))
f1_s1d<- meanf(ts_daily[,"Sub_metering_1"], h = 60, level = c(80, 95))
f1_s2d<- meanf(ts_daily[,"Sub_metering_2"], h = 60, level = c(80, 95))
f1_s3d<- meanf(ts_daily[,"Sub_metering_3"], h = 60, level = c(80, 95))
f1_nsd<- meanf(ts_daily[,"No_submeter"], h = 60, level = c(80, 95))


## simple forecast
fmeanf <- meanf(ts_daily[,"GAP_kwh"], h = 330, level = c(80, 95))
frwf <- rwf(ts_daily[,"GAP_kwh"], h = 330, level = c(80, 95))#naive
fsnaive <- snaive(ts_daily[,"GAP_kwh"], h = 330, level = c(80, 95))#seasonal naive
f4drift <- rwf(ts_daily[,"GAP_kwh"], h = 330, drift=TRUE, level = c(80, 95))

## Predictive accuracy
summary(fmeanf)
summary(frwf)
summary(fsnaive)
summary(f4drift)

## Graph
autoplot(f1d)

## Same, but without the confidence interval for GAP
autoplot(tail(ts_daily[,"GAP_kwh"], n=360))+
  autolayer(f1d, series="Mean GAP", PI=FALSE)


## Plot all submeter with mean model together
autoplot(ts_daily[,"GAP_kwh"])+
  autolayer(ts_daily[,"Sub_metering_1"])+
  autolayer(ts_daily[,"Sub_metering_2"])+
  autolayer(ts_daily[,"Sub_metering_3"])+
  autolayer(ts_daily[,"No_submeter"])+
  autolayer(f1_s1d, series="Mean GAP", PI=FALSE)+
  autolayer(f1_s2d,series="Mean No sub", PI=FALSE)+
  autolayer(f1_s3d, series="Mean Sub1", PI=FALSE)+
  autolayer(f1_nsd, series="Mean Sub2", PI=FALSE)+
  autolayer(f1d,series="Mean Sub3", PI=FALSE)

## Residuals
checkresiduals(f1d)

################-- 6.4. Simple Forecasting Methods: daly intervals, TEST SETS ####

## Creating a testing set avec 10 mois en moins: j'enlève 2010

ts_daily_train<- window(ts_daily, start=2007, end=c(2010,01))

## Forecast on the testing sets  
f1dtr <- meanf(ts_daily_train[,"GAP_kwh"], h = 360, level = c(80, 95))#mean
f2dtr <- rwf(ts_daily_train[,"GAP_kwh"], h = 360, level = c(80, 95))#naive
f3dtr <- snaive(ts_daily_train[,"GAP_kwh"], h = 360, level = c(80, 95))#seasonal naive
f4dtr <- rwf(ts_daily_train[,"GAP_kwh"], h = 360, drift=TRUE, level = c(80, 95))#drift

##simple plots
autoplot(f1dtr)+ylab("Global Active Power (kwh)")->g1
autoplot(f2dtr)+ylab("Global Active Power (kwh)")->g2
autoplot(f3dtr)+ylab("Global Active Power (kwh)")->g3
autoplot(f4dtr)+ylab("Global Active Power (kwh)")->g4

grid.arrange(g1, g2,g3,g4)

## Representation
autoplot(window(ts_daily[,"GAP_kwh"], start=2007))+
  autolayer(f1dtr, series="Mean", PI=FALSE)+
  autolayer(f2dtr, series="Naïve", PI=FALSE) +
  autolayer(f3dtr, series="Seasonal naïve", PI=FALSE) +
  autolayer(f4dtr, series="Drift", PI=FALSE)+
  xlab("Year") + ylab("kwh") +
  ggtitle("Forecasts for daily energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

## Metrics
# Residuals
checkresiduals(f1dtr)
checkresiduals(f2dtr)
checkresiduals(f3dtr)
checkresiduals(f4dtr)

#Test interval
ts_daily_test <- window(ts_daily, start=2010)

#Accuracy
accuracy(f1dtr, ts_daily_test[,"GAP_kwh"])
accuracy(f2dtr, ts_daily_test[,"GAP_kwh"])
accuracy(f3dtr, ts_daily_test[,"GAP_kwh"])
accuracy(f4dtr, ts_daily_test[,"GAP_kwh"])

## Cross validation
f1d_cv <- tsCV(ts_daily_train[,"GAP_kwh"], meanf, h=360)
f2d_cv <- tsCV(ts_daily_train[,"GAP_kwh"], rwf, h=360)
f3d_cv <- tsCV(ts_daily_train[,"GAP_kwh"], snaive, h=360)
f4d_cv <- tsCV(ts_daily_train[,"GAP_kwh"], rwf, h=360, drift=TRUE)

## Cross validation RMSE
sqrt(mean(f1d_cv^2, na.rm=TRUE))
sqrt(mean(f2d_cv^2, na.rm=TRUE))
sqrt(mean(f3d_cv^2, na.rm=TRUE))
sqrt(mean(f4d_cv^2, na.rm=TRUE))

## residuals : pipe structure  : mean
ts_daily_train[,"GAP_kwh"] %>% meanf %>% residuals()->resf1
resf1 %>% mean(na.rm=TRUE)

ts_daily_train[,"GAP_kwh"] %>% rwf %>% residuals()->resf2
resf2 %>% mean(na.rm=TRUE)

ts_daily_train[,"GAP_kwh"] %>% snaive %>% residuals()->resf3
resf3 %>% mean(na.rm=TRUE)

ts_daily_train[,"GAP_kwh"] %>% rwf(drift=TRUE) %>% residuals()->resf4
resf4 %>% mean(na.rm=TRUE)

##summaries

summary(f1dtr)

################-- 6.5. PD_daily linear model --##################################################
## definition of the test & train set

## In this section, 
ts_daily_train<- window(ts_daily, start=2007, end=2010.33)
ts_daily_test <- window(ts_daily, start=2010.34)


pd_daily_train<-filter_time(pd_daily, "2007"~"2009-12-31")
pd_daily_test<-filter_time(pd_daily,~"2010")
test_msts<-ts(pd_daily_test, start=2010, frequency = 365)

msts_daily_123 <- msts(pd_daily[, 6:10], seasonal.periods=c(7,91.5,365.25))
msts_daily_12 <- msts(pd_daily[, 6:10], seasonal.periods=c(7,91.5))
msts_daily_13 <- msts(pd_daily[, 6:10], seasonal.periods=c(7,365.25))
msts_daily_23 <- msts(pd_daily[, 6:10], seasonal.periods=c(91.5,365.25))
msts_daily_1 <- msts(pd_daily[, 6:10], seasonal.periods=c(7))
msts_daily_2 <- msts(pd_daily[, 6:10], seasonal.periods=c(91.5))
msts_daily_3 <- msts(pd_daily[, 6:10], seasonal.periods=c(365.25))

msts_daily_train<-msts(pd_daily_train[, 6:10],seasonal.periods=c(7,91.5,365.25))
msts_daily_test<-msts(pd_daily_test[, 6:10],seasonal.periods=c(7,91.5,365.25))


msts_daily <- msts(pd_daily, seasonal.periods=c(7,91.5,365.25))


## model on train test
tslm_train_gap_ts <- tslm(ts_daily_train[,"GAP_kwh"]~ trend + season)
tslm_train_gap_t <- tslm(ts_daily_train[,"GAP_kwh"] ~ trend )
tslm_train_gap_s <- tslm(ts_daily_train[,"GAP_kwh"] ~ season)

mstslm_wmy<-tslm(msts_daily[,"GAP_kwh"]~fourier(msts_daily[,"GAP_kwh"],
                                                      K=c(2,20,50)))

mstslm_train<-tslm(msts_daily_train[,"GAP_kwh"]~fourier(msts_daily_train[,"GAP_kwh"],K=c(2,20,50)))

mstslm_1k2<-tslm(msts_daily_1[,"GAP_kwh"]~fourier(msts_daily_1[,"GAP_kwh"],K=2))
mstslm_1k25<-tslm(msts_daily_1[,"GAP_kwh"]~fourier(msts_daily_1[,"GAP_kwh"],K=2.5))
mstslm_1k3<-tslm(msts_daily_1[,"GAP_kwh"]~fourier(msts_daily_1[,"GAP_kwh"],K=3))

mstslm_2k20<-tslm(msts_daily_2[,"GAP_kwh"]~fourier(msts_daily_2[,"GAP_kwh"],K=20))
mstslm_2k40<-tslm(msts_daily_2[,"GAP_kwh"]~fourier(msts_daily_2[,"GAP_kwh"],K=40))
mstslm_2k10<-tslm(msts_daily_2[,"GAP_kwh"]~fourier(msts_daily_2[,"GAP_kwh"],K=10))

mstslm_3k20<-tslm(msts_daily_3[,"GAP_kwh"]~fourier(msts_daily_3[,"GAP_kwh"],K=20))
mstslm_3k75<-tslm(msts_daily_3[,"GAP_kwh"]~fourier(msts_daily_3[,"GAP_kwh"],K=75))
mstslm_3k150<-tslm(msts_daily_3[,"GAP_kwh"]~fourier(msts_daily_3[,"GAP_kwh"],K=150))

mstslm_123<-tslm(msts_daily_123[,"GAP_kwh"]~fourier(msts_daily_123[,"GAP_kwh"],K=c(2,10,20)))


#test graph
autoplot(msts_daily[,"GAP_kwh"], series="Data") +
  autolayer(fitted(fc), series="Fitted")+
  autolayer(fc_123, series="Forecast")+
  ylab("Global Active Power (kwh)")+
  scale_x_continuous(name="Time",
                     labels = c("1"="2007","2"="2008","3"="2009",
                                "4"="2010","5"="2011","6"="2012"))
                    

#test graph
autoplot(msts_daily[,"GAP_kwh"], series="Data") +
  autolayer(fitted(fc), series="Fitted")+
  autolayer(fc_123, series="Forecast")+
  ylab("Global Active Power (kwh)")+
  scale_x_continuous(name="Time",
                     limits=c(4,6),
                     labels=c("4"="2010","5"="2011","6"="2012"),
                     breaks=c(4,5,6))



autoplot(msts_daily[,"GAP_kwh"], series="Data") +
  autolayer(fitted(fc), series="Fitted")+
  autolayer(fc_123, series="Forecast")+
  ylab("Global Active Power (kwh)")

#test graph
autoplot(msts_daily[,"GAP_kwh"], series="Data") +
  autolayer(fitted(fc), series="Fitted")+
  autolayer(fc_123, series="Forecast")+
  ylab("Global Active Power (kwh)")+
  scale_x_continuous(name="Time",
                     labels = c("1"="2007","2"="2008","3"="2009",
                                "4"="2010","5"="2011","6"="2012"))


autoplot(msts_daily[,"GAP_kwh"], series="Data") +
  autolayer(fitted(arima123), series="Fitted")+
  autolayer(fc_123, series="Forecast")+
  ylab("Global Active Power (kwh)")+
  scale_x_continuous(name="Time",
                     limits = c(4.7,5))

## forecast 
f_test_ts <- forecast(tslm_train_gap_ts, h=180, level = c(80, 95))
f_test_t <- forecast(tslm_train_gap_t, h=180, level = c(80, 95))
f_test_s <- forecast(tslm_train_gap_s, h=180, level = c(80, 95))


fc <- forecast(mstslm_wmy, newdata = data.frame(fourier(msts_daily[,"GAP_kwh"],
                                                 K=c(2,20,50), h = 330)))

fc_train <- forecast(mstslm_train, newdata = data.frame(fourier(msts_daily_train[,"GAP_kwh"],
                                                        K=c(2,20,50), h = 330)))

autoplot(fc)

fc_1k2 <- forecast(mstslm_1k2, newdata = data.frame(fourier(msts_daily_1[,"GAP_kwh"],K=2, h = 330)))
fc_1k25 <- forecast(mstslm_1k25, newdata = data.frame(fourier(msts_daily_1[,"GAP_kwh"],K=2.5, h = 330)))
fc_1k3 <- forecast(mstslm_1k3, newdata = data.frame(fourier(msts_daily_1[,"GAP_kwh"],K=3, h = 330)))

fc_2k10 <- forecast(mstslm_2k10, newdata = data.frame(fourier(msts_daily_2[,"GAP_kwh"],K=10, h = 330)))
fc_2k20 <- forecast(mstslm_2k20, newdata = data.frame(fourier(msts_daily_2[,"GAP_kwh"],K=20, h = 330)))
fc_2k40 <- forecast(mstslm_2k40, newdata = data.frame(fourier(msts_daily_2[,"GAP_kwh"],K=40, h = 330)))

fc_3k20 <- forecast(mstslm_3k20, newdata = data.frame(fourier(msts_daily_3[,"GAP_kwh"],K=20, h = 330)))
fc_3k75 <- forecast(mstslm_3k75, newdata = data.frame(fourier(msts_daily_3[,"GAP_kwh"],K=75, h = 330)))
fc_3k150 <- forecast(mstslm_3k150, newdata = data.frame(fourier(msts_daily_3[,"GAP_kwh"],K=150, h = 330)))
fc_123 <- forecast(mstslm_123, newdata = data.frame(fourier(msts_daily_123[,"GAP_kwh"],K=c(2,10,20), h = 330)))

CV(mstslm_1k2)
CV(mstslm_1k25)
CV(mstslm_1k3)
CV(mstslm_2k10)
CV(mstslm_2k20)
CV(mstslm_2k40)
CV(mstslm_3k150)
CV(mstslm_3k75)
CV(mstslm_3k20)
CV(mstslm_123)


##ARIMA+fourier
autoplot(msts_daily[,"GAP_kwh"])

arima123 <- auto.arima(msts_daily_123[,"GAP_kwh"], xreg = fourier(msts_daily_123[,"GAP_kwh"], K = c(2,10,20)),
                   seasonal = FALSE)

arima123 %>%
  forecast(xreg = fourier(msts_daily_123[,"GAP_kwh"], K = c(2,10,20), h = 330)) %>% autoplot

summary(arima123)
autoplot(arima123)

## plots
autoplot(f_test_ts)+ylab("Global Active Power (kwh)")->g5
autoplot(f_test_s)+ylab("Global Active Power (kwh)")->g6
autoplot(f_test_t)+ylab("Global Active Power (kwh)")->g7
grid.arrange(g5,g6,g7, nrow=3)


## Representation
autoplot(window(ts_daily[,"GAP_kwh"], start=2007))+
  autolayer(f_test_s, series="Seasonality", PI=FALSE)+
  autolayer(f_test_t, series="Trend", PI=FALSE) +
  autolayer(f_test_ts, series="Trend+seasonality", PI=FALSE) +
  xlab("Year") + ylab("kwh") +
  ggtitle("Forecasts for daily energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

# Residuals
checkresiduals(tslm_train_gap_s)
checkresiduals(tslm_train_gap_t)
checkresiduals(tslm_train_gap_ts)

#Accuracy
accuracy(f_test_ts, ts_daily_test[,"GAP_kwh"])
accuracy(f_test_t, ts_daily_test[,"GAP_kwh"])
accuracy(f_test_s, ts_daily_test[,"GAP_kwh"])
accuracy(fc_train,test_msts[,"GAP_kwh"])

# summary
summary(f_test_ts)
summary(f_test_s)
summary(f_test_t)
summary(tslm_train_gap_s)
summary(tslm_train_gap_t)
summary(tslm_train_gap_ts)

################-- 7.1. plotting TS --############################################################

## plot TS daily GAP, all years
plot.ts(ts_pddaily_GAP)


##To estimate the trend, seasonal and irregular components of this time series, we type:

#a - decompose the elements
alldaystimeseriescomponents <- decompose(ts_alldayGAP)
#b - get seasonal values
alldaystimeseriescomponents$seasonal
#c - plotting seasonality
plot(alldaystimeseriescomponents$seasonal)
#d - plotting all components
plot(alldaystimeseriescomponents)

#autoplot
autoplot(ts_pddaily_GAP, facet=TRUE) +
  xlab("Years") + ylab("kwh") +
  ggtitle("Global Active Power")

autoplot(ts_daily[,"GAP_kwh"], facet=TRUE) +
  xlab("Years") + ylab("kwh") +
  ggtitle("Global Active Power")

autoplot(ts_monthly[,2:6], facets=TRUE)

## Plot by HOURS

days7<-epowerdata %>% group_by(hour(DateTime))%>% summarise_if(is.numeric,sum)

days7%>%ggplot(aes (`hour(DateTime)`,GAP_kwh)) +
  geom_line()+ylab("GAP (kwh)")+scale_x_continuous(name="Hours",
                                                   breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))->gh
gh
## Plot by weekdays
Weekdays<-epowerdata %>%group_by(wday(DateTime))%>% summarise_if(is.numeric,sum)


Weekdays%>%ggplot(aes (`wday(DateTime)`,GAP_kwh)) +
  geom_line()+ylab("GAP (kwh)")+scale_x_continuous(name="Days",
                                                   labels=c("1"="Sun","2"= "Mon", "3"="Tue","4"="Wed",
                                                            "5"="Thu", "6"="Fri","7"="Sat"),
                                                   breaks = c(1,2,3,4,5,6,7))->gd
gd
## plot by month

autoplot(ts_monthly)->g
g + xlim= c("2007","2010")->g2

Month<-epowerdata %>%group_by(month(DateTime))%>% summarise_if(is.numeric,sum)

Month%>%ggplot(aes (`month(DateTime)`,GAP_kwh)) +
  geom_line()+ylab("GAP (kwh)")+scale_x_continuous(name="Months",
                                                  labels=c("1"="Jan","2"= "Feb", "3"="Mar","4"="Apr",
                                                           "5"="May", "6"="Jun","7"="Jull","8"="Aug",
                                                           "9"="Sept","10"="Oct","11"="Nov","12"="Dec"),
                                                  breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))->gm
gm

q+scale_x_continuous(name="Months",
                     labels=c("1"="Jan","2"= "Feb", "3"="Mar","4"="Apr",
                              "5"="May", "6"="Jun","7"="Jull","8"="Aug",
                              "9"="Sept","10"="Oct","11"="Nov","12"="Dec"),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))->gm

Month%>%ggplot(aes (`month(DateTime)`,GAP_kwh)) +
  geom_line()+xlim("1","2","3","4","5","6","7","8","9","10","11","12")
## quarter

quarter<-epowerdata %>%group_by(quarter(DateTime))%>% summarise_if(is.numeric,sum)

quarter%>%ggplot(aes (`quarter(DateTime)`,GAP_kwh)) +
  geom_line()+ylab("GAP (kwh)")+
  scale_x_continuous(name="Seasons",
                   labels=c("1"="Winter","2"= "Spring", "3"="Summer","4"="Autumn"))->gq

quarter%>%ggplot(aes (`quarter(DateTime)`,GAP_kwh)) +geom_line()+ylab("GAP (kwh)")->p
  

##year

year<-epowerdata %>%group_by(year(DateTime))%>% summarise_if(is.numeric,sum)

year%>%ggplot(aes (`year(DateTime)`,GAP_kwh)) +
  geom_line()+ylab("GAP (kwh)")+xlab("quarter")->gy

gy

grid.arrange(gh,gd,gm,gq,gy, nrow=5)
################-- 8.1. test subsampling with timetibble --#################################################
## AS_PERIOD : takes a data point every year, week, 2 week, quarters, etc.
## Here, takes a data point out of every week
databyweek<-as_period(powerdata, 
                      period = "W")
## Here, every 15 minutes
databy15min<-as_period(powerdata, 
                       period = "15 minutes") 

##IS THERE AN EQUIVALENT FUNCTION IN REGULAR TIBBLES WITH AN INDEX?

## COLAPSE : Collapse the index of a tbl_time object by time period. The index column is altered 
##so that all dates that fall in a specified interval share a common date.
##Here, one date is retained for every year
cpd<-collapse_by(powerdata,"Y")

##COLAPSE INDEX
##Create a new column with a unique value for every week
cipd<-dplyr::mutate(powerdata, Weekly = collapse_index(DateTime, "weekly"))

##Integrating the data to a grouped value
wcipd<-cipd %>%
  dplyr::group_by(Weekly) %>%
  dplyr::summarise_if(is.numeric, sum)

str(wcipdw)

##create series
wcipdw<-create_series('2007'~'2010-11-26','w')

##bind them together
ndf<-bind_cols(wcipdw,wcipd)

wcipdb<-dplyr::mutate(wcipd, Week = create_series(~'2007','w'))

create_series(~'2013', 'daily')

##create intervals with FILTER TIME

pd2007<-filter_time(powerdata, ~ '2007')


###test to plot a subset of a time serie
## Pour grap: subset 2010 pd_daily
pd_daily_sub<-filter_time(pd_daily,~"2010")
ts_daily_sub = ts(pd_daily_sub[, 6:21], #colomn without the time related one
                  start = c(2010, 01), #start: year + month
                  frequency = 365.25)

## Pour graph pd-hourly: subset
pd_hourly_sub<-filter_time(pd_hourly, ~"2010-10")
ts_hourly_sub<-ts(pd_hourly_sub[, 6:21], #colomn without the time related one
                  start = c(2010,01), #start: year + month
                  frequency = 8766)
print(head(ts_daily))
autoplot(ts_pdhourly)

##create a subset 2 months
subset<-subset(ts_pdhourly, start=)

cut<-cut.Date(pd_hourly,breaks="hour", from="2010-10-01", to="2010-11-26")

pd_hourly_sub<-filter_time(pd_hourly, "2010-10-01"~"2010-11-26")
ts_hourly_sub<-ts(pd_hourly_sub[, 6:21], #colomn without the time related one
                  start = c(2010,10),
                  end=c(2010,11),
                  frequency = 24*7)

ts_selec<-window(ts_pdhourly, start=c(2010,10), end=c(2010,11))
autoplot(ts_selec)

#test complex seasonality
calls %>% mstl(ts_daily) %>%
  autoplot() + xlab("Months")

ts_daily[,"GAP_kwh"] %>%  stlf(ts_daily) %>%
  autoplot() + xlab("Week")

ts_daily[,"GAP_kwh"] %>% tbats() %>% forecast() %>% autoplot()

## Dynamic harmonic model

# fourier(x, K=20, h = NULL)
fit <- auto.arima(ts_daily[,"GAP_kwh"], xreg = fourier(ts_daily[,"GAP_kwh"], K = 20),
                    seasonal = FALSE)
fc<-forecast(fit,xreg = fourier(ts_daily[,"GAP_kwh"], K = 20, h = 360)) 

autoplot(fc)

summary(fc)#AICC 29285.05

# fourier(x, K=25, h = NULL)
fit_5 <- auto.arima(ts_daily[,"GAP_kwh"], xreg = fourier(ts_daily[,"GAP_kwh"], K = 5),
                  seasonal = FALSE)
fc_5<-forecast(fit_5,xreg = fourier(ts_daily[,"GAP_kwh"], K = 5, h = 360)) 

autoplot(fc_5)

summary(fc_5)#AICC 29285.05

# fourier(x, K=70, h = NULL)
fit70 <- auto.arima(ts_daily[,"GAP_kwh"], xreg = fourier(ts_daily[,"GAP_kwh"], K = 70),
                    seasonal = FALSE)
fc70<-forecast(fit70,xreg = fourier(ts_daily[,"GAP_kwh"], K = 70, h = 360)) 

autoplot(fc70)

summary(fc70)#AICC 29285.05

## with two seasonality
pd_daily[, 6:21]

msts_daily <- msts(pd_daily[, 6:10], seasonal.periods=c(7,30.5,365))

msts_monthly <- msts(pd_monthly[, 6:10], seasonal.periods=12)
autoplot(msts_monthly)

autoplot(ts_monthly)+scale_x_continuous(limits=c(2010,2011))

autoplot(msts_daily[,"GAP_kwh"])

fit2 <- auto.arima(msts_daily[,"GAP_kwh"], xreg = fourier(msts_daily[,"GAP_kwh"], K = c(2,6,20)),
                  seasonal = FALSE)

fit2 %>%
  forecast(xreg = fourier(msts_daily[,"GAP_kwh"], K = c(2,6,20), h = 360)) %>%
  autoplot()





