########################################################################################################
#####################-- Visualize and Analyze Energy Data --#################################################
###########################################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(tibbletime)
library(hms)
library(fpp2)
library(plyr)

setwd("~/Ubiqum/Module 3_Deep Analytics/Task 2")

powerdata <- read.csv("Data/household_power_consumption.txt", 
                      stringsAsFactors = FALSE, 
                      sep = ";", 
                      header = TRUE)

#######-- formatage des données --#####################################################################

##transformation des facteurs au bon format
##A/ transformation des colonnes 3 à 9 au format numérique

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
powerdata$Date <- as.Date(powerdata$Date, "%d/%m/%Y", tz="GMT")#colonne Date
powerdata$DateTime<- ymd_hms(powerdata$DateTime, tz="GMT")
powerdata$Time<-as.hms(powerdata$Time, tz="GMT")

#6 - creating a tible time with the dataframe
powerdata<- as_tbl_time(powerdata, index=DateTime)

##C/ Création d'une nouvelle colonne = l'énergie consommée en dehors des pièces sans submeters
powerdata <- powerdata %>% mutate(No_submeter = Global_active_power*1000/60-Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

##D/ Création d'une nouvelle colonne avec GAP transformed in kWhour too
powerdata <- powerdata %>% mutate (GAP_kwh = (Global_active_power*1000/60))

##E/ exlucding the days from 2006
powerdata <- powerdata %>% filter(year(DateTime) != 2006)

######################-- NAS --#####################################################################

##using the "fill" function from tidyr to fill the NAs with the most recent values
powerdata <-fill(powerdata, Global_intensity, Global_reactive_power, Global_active_power,
                 Voltage, Sub_metering_3, Sub_metering_2, Sub_metering_1, No_submeter, GAP_kwh)


##############-- subsampling with timetibble --#################################################

##adding periodic colomns to the dataframe
epowerdata<-powerdata%>%
  dplyr::mutate(QuarterHour = collapse_index(DateTime, "15 minutes"))%>%
  dplyr::mutate(Hourly= collapse_index(DateTime, "hours"))%>%
  dplyr::mutate(Fourhours = collapse_index(DateTime, "4 hours"))%>%
  dplyr::mutate(DayNight = collapse_index(DateTime, "12 hours"))%>%
  dplyr::mutate(Weekly = collapse_index(DateTime, "weekly"))%>%
  dplyr::mutate(Monthly = collapse_index(DateTime, "months"))%>%
  dplyr::mutate(Quarterly = collapse_index(DateTime, "Q"))%>%
  dplyr::mutate(Yearly = collapse_index(DateTime, "Y"))

##create intervals with FILTER TIME : Years
##Quicker equivalent than the function "subset"

year2007<-filter_time(powerdata,~"2007")
year2008<-filter_time(powerdata,~"2008")
year2009<-filter_time(powerdata,~"2009")
year2010<-filter_time(powerdata,~"2010")

##/ Summarise all data set by SUM hour
positions<-c(4:12) #first, defining the positions of the columns for POWERDATA

SumHour_all<-powerdata %>% group_by(Date) %>% 
  select("Voltage") %>% 
  summarise_all(sum)


df_allhour <-unite(df_allhour, DateHour, `date(DateTime)`, `hour(Time)` )
df_allhour$DateHour <-ymd_h(df_allhour$DateHour) #change to the right format with lubridate

str(df_allhour)



##5B/ create a time serie (ts) with df_allhour


ts_allhour = ts(df_allhour[, 2:10], #colomn without the time related one
                start = c(2007, 01), #start: year + month
                frequency = 24*365.25) #frequency: every year: 24h x365.25

##6/ Summarise all data set by sum day

df_allday<-powerdata %>% group_by(date(DateTime)) %>% 
  select( "Global_active_power","Global_reactive_power",
          "Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3",
          "No_submeter", "GAP_kwh") %>% 
  #select the numerical variables to be summed. It adds the missing grouping variables
  summarise_all(sum)

#rename the first colomn
colnames(df_allday)[1] <-"Date"

#structure
str(df_allday)

#create a time serie
ts_allday = ts(df_allday[, 2:10], #colomn without the time related one
               start = c(2007, 01), #start: year + month
               frequency = 365) #frequency: 365.25 jours

#create a subset ts for GAP only (col 9)
ts_alldayGAP = ts(df_allday[,10], #colomn without the time related one
                  start = c(2007, 01), #start: year + month
                  frequency = 365) #frequency: 365.25 jours

ts_alldayGAP
summarise(ts_alldayGAP)

##7  - create a subset for one year - automne to automne

alldayGAP_autumn0708 <- filter (df_allday, Date >= "2007-09-23" & Date <="2008-09-22")

ts_alldayGAP_autumn0708 = ts(alldayGAP_autumn0708[,2:10], #colomn without the time related one
                             start = c(2007, 09), #start: year + month
                             frequency = 366) #frequency: 365.25 jours

############## part aside now ##################################################################
##6/ Define key dates four seasons 2007

#firstday<-ymd("2006-12-16")#first day of the data set
#startspring07<-ymd("2007-03-21")#first day spring 2007
#endspring07<-ymd("2007-06-21")#lastday spring 2007

#Spring07<-startspring07 %--% endspring07 #creates a formal class interval

#df_spring07 <- powerdata %>% filter(Date >= "2007-03-21" & Date <="2007-06-21")

#Attention: les NA sont effacées dans ce cas ...  problème? 

##7/ thurday first week: zoom to see the gap on first week => RESOLVED
#wday("2006-12-21", label=TRUE)

#df_zoomfirstweek <- powerdata %>% filter (Date >= "2006-12-21" & Date <="2006-12-22")

#df_zoomfirstweekhour<-df_zoomfirstweek %>% group_by(date(DateTime),hour(DateTime)) %>% 
#select( "Global_active_power","Global_reactive_power",
#"Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3") %>% 
#select the numerical variables to be summed. It adds the missing grouping variables
#summarise_all(sum)

#df_zoomfirstweekhour <-unite(df_zoomfirstweekhour, DateHour, `date(DateTime)`, `hour(DateTime)` )
#df_zoomfirstweekhour$DateHour <-ymd_h(df_zoomfirstweekhour$DateHour) #change to the right format with lubridate

######################################-- NA(s) --#######################################################
##two types of NA's: long days/ short minute interval
##apply different strategies: short interval: fillers
##NA for longer periods: ??? 0 or smthg else (knn? see online)

###################### graphiques of first week and first week/hour #############################

##histogrames

#one week, minutes/hour
hGAP_oneweek<-hist(df_firstweek$Global_active_power, 
                   main = "Global Active Power", 
                   xlab = "Global Active Power (kilowatts)", 
                   col = "red")

#oneweek, sum/hour
hGAP_oneweekSUM<-hist(df_firstweekhour$Global_active_power, 
                      main = "Global Active Power", 
                      xlab = "Global Active Power (kilowatts)", 
                      col = "red")

#oneweek, mean/hour
hGAP_oneweekMEAN<-hist(df_firstweekmeanhour$Global_active_power, 
                       main = "Global Active Power", 
                       xlab = "Global Active Power (kilowatts)", 
                       col = "red")

##time aligned graph
#attention, pour rendre le graph possible, il faut que la colonne de temps/heure soit lisible au bon format, c'est pour ça que j'ai créé une nouvelle colonne qui lie date(Datetime) and hour(datetime ensemble) + change le format as ymd_h with lubridate

#one week, minutes /hour
tGAP_oneweek<-plot(df_firstweek$Global_active_power ~ df_firstweek$DateTime, 
                   ylab = "Global Active Power (kilowatts)",
                   xlab = "",
                   type = "l")

tGAP_zoomoneweek<-plot(df_zoomfirstweek$Global_active_power ~ df_zoomfirstweek$DateTime, 
                       ylab = "Global Active Power (kilowatts)",
                       xlab = "",
                       type = "l")

tGAP_zoomoneweekhour<-plot(df_zoomfirstweekhour$Global_active_power ~ df_zoomfirstweekhour$DateHour, 
                           ylab = "Global Active Power (kilowatts)",
                           xlab = "",
                           type = "l")

##one week, sum/hour
tGAP_oneweekhour<-plot(df_firstweekhour$Global_active_power ~ df_firstweekhour$DateHour, 
                       ylab = "Global Active Power (kilowatts)",
                       xlab = "",
                       type = "l")
#two weeks, mean/hour
tGAP_oneweekmeanhour<-plot(df_firstweekmeanhour$Global_active_power ~ df_firstweekmeanhour$DateHour, 
                           ylab = "Global Active Power (kilowatts)",
                           xlab = "",
                           type = "l")


#spring07
tGAP_spring07<-plot(df_spring07$Global_active_power ~ df_spring07$DateTime, 
                    main="Global active power Spring 2007",
                    ylab = "Global Active Power (kilowatts)",
                    xlab = "",
                    type = "l")
##years

gr_allhour<-plot(df_allhour$Global_active_power ~ df_allhour$DateHour, 
                 main="Global active power 2006-2011",
                 ylab = "Global Active Power (kilowatts)",
                 xlab = "",
                 type = "l")

##autumn 0708

gr_oneyear<-plot(alldayGAP_autumn0708$GAP_kwh ~ alldayGAP_autumn0708$Date, 
                 main="Global active power 2007-2008",
                 ylab = "Global Active Power (kilowatts)",
                 xlab = "",
                 type = "l")

# Multiple line plot : submeters
str(alldayGAP_autumn0708)

winter2007 <- df_allday %>% filter(Date >= "2007-01-01" & Date <="2007-03-01")

plot(winter2007$No_submeter ~ winter2007$Date, ylab = "Energy sub metering", xlab = "", type = "l") 
lines(winter2007$Sub_metering_3 ~ winter2007$Date, col = 'Red')
lines(winter2007$Sub_metering_2 ~ winter2007$Date, col = 'Blue')
lines(winter2007$Sub_metering_1~ winter2007$Date, col= 'Green')
legend("topright", col = c("black", "red", "blue", "green"), legend = c("No_submeter", "Sub_metering_3", "Sub_metering_2", "Sub_metering_1"), lwd = 1)

#submetering line graph for one week


plot(df_firstweek$Sub_metering_2 ~ df_firstweek$DateTime, ylab = "Energy sub metering", xlab = "", type = "l", col='Blue') 
lines(df_firstweek$Sub_metering_3 ~ df_firstweek$DateTime, col = 'Red')
lines(df_firstweek$Sub_metering_1 ~ df_firstweek$DateTime, col = 'Green')
lines(df_firstweek$Sub_metering_1~ df_firstweek$DateTime, col= 'Green')

#legend("topright", col = c("black", "red", "blue", "green"), legend = c("No_submeter", "Sub_metering_3", "Sub_metering_2", "Sub_metering_1"), lwd = 1)

#three days

threedays <- df_firstweek %>% filter(Date >= "2007-01-01" & Date <="2007-01-03")

plot(threedays$No_submeter ~ threedays$DateTime, ylab = "Energy sub metering", xlab = "", type = "l") 
lines(threedays$Sub_metering_3 ~ threedays$DateTime, col = 'Red')
lines(threedays$Sub_metering_2 ~ threedays$DateTime, col = 'Blue')
lines(threedays$Sub_metering_1~ threedays$DateTime, col= 'Green')

#weekend
weekend <- df_firstweek %>% filter(Date >= "2007-01-04" & Date <="2007-01-06")

plot(weekend$No_submeter ~ weekend$DateTime, ylab = "Energy sub metering", xlab = "", type = "l") 
lines(weekend$Sub_metering_3 ~ weekend$DateTime, col = 'Red')
lines(weekend$Sub_metering_2 ~ weekend$DateTime, col = 'Blue')
lines(weekend$Sub_metering_1~ weekend$DateTime, col= 'Green')

#summerdays
wday("2007-06-29", label=TRUE)
weekendsummer <- powerdata %>% filter(DateTime >= "2007-06-29" & DateTime <="2007-07-05")

plot(weekendsummer$Sub_metering_1 ~ weekendsummer$DateTime, ylab = "Energy sub metering", xlab = "", type = "l", col='Green') 
lines(weekendsummer$Sub_metering_2 ~ weekendsummer$DateTime, col = 'Blue')
lines(weekendsummer$Sub_metering_3 ~ weekendsummer$DateTime, col = 'Red')
lines(weekendsummer$No_submeter ~ weekendsummer$DateTime, col = 'Plum')

lines(weekendsummer$Sub_metering_1~ weekendsummer$DateTime, col= 'Green')
############## plotting ts############################################################

#plot the time serie
plots<- plot.ts(ts_alldayGAP)

plots<- plot.ts(ts_alldayGAP_autumn0708)


##To estimate the trend, seasonal and irregular components of this time series, we type:

#a - decompose the elements
alldaystimeseriescomponents <- decompose(ts_alldayGAP)
#b - get seasonal values
alldaystimeseriescomponents$seasonal
#c - plotting
plot(alldaystimeseriescomponents$seasonal)

plot(alldaystimeseriescomponents)















##seasonal plot

#seasonplot <- ggseasonplot(ts_allhour, year.labels=TRUE, year.labels.left=TRUE) +
#ylab("$ million") +
#ggtitle("Seasonal plot: antidiabetic drug sales")













na.omit()


##création d'un subset avec les données d'un mois
#création d'un GROUP_BY ou agrégation par heure
#créaion d'une sélection de dates

####################-- graphiques --####################################################################
graph1<-ggplot()


##Na's

is.na(powerdata)

Missingvalues<-powerdata[is.na(powerdata)]
Missing values

powerdata[is.na(powerdata)]

##création d'un échantillon avec df_firstweek par heure
#creation d'une nouvelle colonne qui transforme Time au format Time et retient la donnée heure  
#df_firstweekhour<- df_firstweek %>%
# mutate(hour = as.POSIXct(Time, format="%H:%M:%S")) %>% # create a new colomn "hour" avec Time, au format Time
# group_by(lubridate::hour(hour)) #group by: ne retient que cette information de l'heure, via lubridate, nouvelle colonne créée


##test: creating a day-hour selection
df_firstweekhour<- df_firstweek %>%
  group_by(lubridate::hour(DateTime), day(DateTime))

#rename the colomn
colnames(df_test)[12] <- "Day"
colnames(df_test)[11] <- "Hour"

#réunion Date + Hour with Tidyr
df_test <-unite(df_test,Day,Hour)
colnames(df_firstweekhour)[13] <-"DateHour"

##############-- test subsampling with timetibble --#################################################
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

