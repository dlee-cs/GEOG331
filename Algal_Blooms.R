# load in packages
library(lubridate)
library(dplyr)
library(ggplot2)
library(janitor)

# # Determine operating system
# OS <- .Platform$OS.type
# # Set path for operating system
# if (OS == "unix") {
#   path <- "/Volumes/class/GEOG331_S22/students/dslee/" # MAC file path
# } else if (OS == "windows") {
#   path <- "Z:/GEOG331_S22/students/dslee/" # windows file path
# } else {
#   print("ERROR: OS could not be identified")
# }

# set working directory
#setwd(path)
setwd("/Users/nancychoy/Desktop/Env Data Science/GEOG331/WErie/0-data")

#read in Western Lake Erie Field Data
df <- read.csv("lake_erie_habs_field_sampling_results_2012_2018_v2.csv", 
                 na.strings=c("","nd","NA"), check.names = F)

#format column headers
df <- clean_names(df)
#subset relevant data
datE <- df[c("date","site","wind_speed_knots", "ctd_temperature_c")]

#only get days with wind speed data
datEW <- datE[!(is.na(datE$wind_speed_knots)),]

#### define time for graphing wind and temp #####
#convert date to date object
dates <- as.Date(datEW$date, "%m/%d/%Y")
#get day of year
datEW$doy <- yday(dates)
#calculate year
datEW$year <- year(dates)

#clean wind speed data
datEW$wind_speed_knots <- as.numeric(datEW$wind_speed_knots)

#calculate average wind speed across sample sites for each day
aveW <- aggregate(datEW$wind_speed_knots, by=list(datEW$date, datEW$doy, datEW$year), 
                  FUN="mean", na.rm = T)
colnames(aveW) <- c("date", "doy", "year", "daily_ave")

#determine graph axis bounds
min(aveW$doy) #115 round -> 100
max(aveW$doy) #292 round -> 300
min(aveW$daily_ave) #1.24 round -> 0
max(aveW$daily_ave) #16.1 round -> 18

#assign colors to each year
colors <- c("#00AFBB", "#FFCC66", "#FC4E07", "#99CC33")

#graph data
par(mai=c(1,1,1,1)) #larger margins
plot(aveW$doy,aveW$daily_ave, 
     main="Average Windspeed in Western Lake Erie",
     xlab="Day of Year", 
     ylab=expression(paste("Wind Speed (knots)")),
     xlim=c(100,300),
     ylim=c(0,18),
     pch=19,
     cex=0.5,
     lwd=2,
     xaxs="i", yaxs ="i", #remove gaps from axes
     col=colors) #color plot by year

legend("bottomleft", c("2015","2016", "2017","2018"), #legend items
       col=colors,
       pch=19, #symbols
       bty="n", #no legend border
       cex=0.5) 

