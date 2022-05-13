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

#------------------------------- Avg Wind Speed ----------------------------
#only get days with wind speed data
datEW <- datE[!(is.na(datE$wind_speed_knots)),]

#determine temporal scope of data
start_date <- as.Date(head(datEW, 1)$date, "%m/%d/%Y")
end_date <- as.Date(tail(datEW, 1)$date, "%m/%d/%Y")

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
colnames(aveW) <- c("date", "doy", "year", "daily_ave_windspeed")
aveW$daily_ave_windspeed <- round(aveW$daily_ave_windspeed, 2)

#determine graph axis bounds
min(aveW$doy) #115 round -> 100
max(aveW$doy) #292 round -> 300
min(aveW$daily_ave_windspeed) #1.24 round -> 0
max(aveW$daily_ave_windspeed) #16.1 round -> 18

#assign colors to each year
colors <- c("#00AFBB", "#FFCC66", "#FC4E07", "#99CC33")

#graph data
par(mai=c(1,1,1,1)) #larger margins
plot(aveW$doy,aveW$daily_ave_windspeed, 
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

#------------------------------- Avg Temperature -------------------------
#ensure temp data is valid
datEW$ctd_temperature_c <- as.numeric(datEW$ctd_temperature_c)

#calculate average temperature across sample sites for each day within 
#period of wind speed data
aveT <- aggregate(datEW$ctd_temperature_c, by=list(datEW$date, datEW$doy, datEW$year), 
                  FUN="mean", na.rm = T)
colnames(aveT) <- c("date", "doy", "year", "daily_ave_temp")
aveT$daily_ave_temp <- round(aveT$daily_ave_temp, 2)

#determine graph axis bounds (doy = 100-300 from Wind Speed calcs)
min(aveT$daily_ave_temp) #10.93 round -> 10
max(aveT$daily_ave_temp) #26.7 round -> 30

#assign colors to each year
colors <- c("#00AFBB", "#FFCC66", "#FC4E07", "#99CC33")

#graph data
par(mai=c(1,1,1,1)) #larger margins
plot(aveT$doy,aveT$daily_ave_temp, 
     main="Average Water Temperature in Western Lake Erie",
     xlab="Day of Year", 
     ylab=expression(paste("Temperature (Degrees Celsius)")),
     xlim=c(100,300),
     ylim=c(10,30),
     pch=19,
     cex=0.5,
     lwd=2,
     xaxs="i", yaxs ="i", #remove gaps from axes
     col=colors) #color plot by year

legend("topleft", c("2015","2016", "2017","2018"), #legend items
       col=colors,
       pch=19, #symbols
       bty="n", #no legend border
       cex=0.5)

#--------------------------------- Regression ----------------------------
#get days with both wind speed and temp measurements
datR <- datEW[!(is.na(datEW$wind_speed_knots)),]
datRF <- datR[!(is.na(datR$ctd_temperature_c)),]

datRF$wind_speed_knots <- as.numeric(datRF$wind_speed_knots)
datRF$ctd_temperature_c <- as.numeric(datRF$ctd_temperature_c)

#### define time for graphing wind and temp #####
#convert date to date object
dates2 <- as.Date(datRF$date, "%m/%d/%Y")
#get day of year
datRF$doy <- yday(dates2)
#calculate year
datRF$year <- year(dates2)

d15 <- datRF[datRF$year=='2015',]
d16 <- datRF[datRF$year=='2016',]
d17 <- datRF[datRF$year=='2017',]
d18 <- datRF[datRF$year=='2018',]

#linear model 
fit15 <- lm(d15$ctd_temperature_c~d15$wind_speed_knots)
fit16 <- lm(d16$ctd_temperature_c~d16$wind_speed_knots)
fit17 <- lm(d17$ctd_temperature_c~d17$wind_speed_knots)
fit18 <- lm(d18$ctd_temperature_c~d18$wind_speed_knots)

#regression results
summary(fit15)
summary(fit16)
summary(fit17)
summary(fit18)

#histogram of residuals 
hist(summary(fit15)$residuals,
     main = "Regression Residuals (2015)",
     xlab = "Residual")
hist(summary(fit16)$residuals,
     main = "Regression Residuals (2016)",
     xlab = "Residual")
hist(summary(fit17)$residuals,
     main = "Regression Residuals (2017)",
     xlab = "Residual")
hist(summary(fit18)$residuals,
     main = "Regression Residuals (2018)",
     xlab = "Residual")

#shapiro wilks test
shapiro.test(summary(fit15)$residuals)
shapiro.test(summary(fit16)$residuals)
shapiro.test(summary(fit17)$residuals)
shapiro.test(summary(fit18)$residuals)
