# Devon Lee
# 3/23/22
# HW 5

#load in packages
library(lubridate)
library(dplyr)
library(ggplot2)

## The following objects are masked from 'package:base':
## date, intersect, setdiff, union

#set working directory to my stream flow data folder
setwd("/Users/nancychoy/Desktop/Env Data Science/GEOG331/streamflow")

#read in streamflow data
datH <- read.csv("stream_flow_data.csv", na.strings = c("Eqp"))

#read in precipitation data (hourly precipitation is in mm)
datP <- read.csv("2049867.csv")

#only use most reliable measurements for streamflow data
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format with a decimal hour
datD$hour <- hour(timesD) + (minute(timesD)/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP)/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))          


############################ Question 3 ############################
nrow(datD)
nrow(datP)

#calculate average daily discharge and SD for each day
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l",
     main="Average Daily Discharge of Harbor Brook, Syracuse NY",
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     xlim=c(0,360),
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes

#show standard deviation around the mean
#define  boundaries of polygon by tracing counterclockwise 
#function automatically connects start and end points of polygon and shades it
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       

legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


############################ Question 5 ############################
lines(datD$discharge[datD$year == 2017], col="red")

#label x-axis by month
axis(1, seq(0,360, by=30), #tick intervals
     lab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec","")) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle


############################ Question 7 ############################
counted_p_obs <- aggregate(datP$HPCP, by = list(datP$doy, datP$year), 
                           FUN = length)
colnames(counted_p_obs) <- c("doy", "year", "num_obs")
complete_precip_days <- counted_p_obs[counted_p_obs$num_obs == 24,]

#get subset of discharge data for days with full 24hr observation of precip data
complete_discharge_df <- dplyr::semi_join(datD, complete_precip_days, by = c("doy", "year"))

#start new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#plot discharge
plot(datD$decYear, datD$discharge, type="l", 
     main="Discharge of Harbor Brook, Syracuse NY",
     xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#symbolize data on days with complete precip measurements
points(x=complete_discharge_df$decYear, y=complete_discharge_df$discharge, pch=19, cex=0.15, col="red")

#add legend
legend("topleft", 
       "data from days with complete hourly precipitation record", #legend items
       pch=19,#point
       col="red",#colors
       bty="n")#no legend border


############################ Question 8 ############################
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy == 7 & datD$year == 2009,]
hydroP <- datP[datP$doy == 7 & datP$year == 2009,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the discharge plot
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

#start new plot
dev.new(width=8,height=8)

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     main="Precipitation and Discharge of Harbor Brook, Syracuse NY on January 7, 2009",
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


############################ Question 9 ############################
#specify season as a factor
datD$season[datD$doy >= 61 & datD$doy < 153] <- "spring"
datD$season[datD$doy >= 153 & datD$doy < 245] <- "summer"
datD$season[datD$doy >= 245 & datD$doy < 336] <- "fall"
datD$season[datD$doy >= 336 | datD$doy < 61] <- "winter"
#specify season as a factor
datD$season <- as.factor(datD$season)

#make a violin plot for 2016
dev.new(width=8,height=8) #start new plot
ggplot(data= datD[datD$year == 2016,], aes(season,discharge)) + 
  geom_violin() +
  ggtitle("Discharge of Harbor Brook, Syracuse NY (2016)") +
  theme(plot.title = element_text(hjust = 0.5))

#make a violin plot for 2017
dev.new(width=8,height=8) #start new plot
ggplot(data= datD[datD$year == 2017,], aes(season,discharge)) + 
  geom_violin() +
  ggtitle("Discharge of Harbor Brook, Syracuse NY (2017)") +
  theme(plot.title = element_text(hjust = 0.5))
