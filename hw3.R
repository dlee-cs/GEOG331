# GEOG331 Activity 3 script
# DL 02/21/22

#use install.packages to install lubridate
#install.packages(c("lubridate"))

#create a function. The names of the arguments for your function will be in parentheses. 
# Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}

#set working directory to my bewkes data folder
setwd("/Users/nancychoy/Desktop/Env Data Science/GEOG331/bewkes")

#read in bewkes weather data
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <- read.csv("bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <- colnames(sensorInfo)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#-------------------------------QUESTION 5--------------------------------------
assert(length(lightscale) == nrow(datW), 
       "the number of lightscale values and datW dataframe rows are not equal")
#-------------------------------------------------------------------------------
  
#-------------------------------QUESTION 6--------------------------------------
#make a new column to work with that indicates QAQC
#filter out storms in air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
datW$air.tempQ <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy > 0, NA,
                      ifelse(datW$precipitation > 5, NA, datW$air.temperature))
#filter out storms in wind speed measurements
datW$wind.speedQ <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy > 0, NA,
                      ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#make sure all wind speed values measured during storms are NA
assert(all(is.na(datW$wind.speedQ[datW$precipitation  >= 2 & datW$lightning.acvitivy > 0])), 
         "Failed to filter out suspect wind speed measurements coincident with 
          lightning and rainfall greater than 2mm")
assert(all(is.na(datW$wind.speedQ[datW$precipitation  >= 5])),
         "Failed to filter out suspect wind speed measurements coincident with
          rainfall over 5 mm")

#make a plot of wind speed with filled in points (using pch)
plot(datW$DD, datW$wind.speedQ, pch=19, type="b", main = "Bewkes Windspeeds During 2018",
      xlab = "Day of Year", ylab="Windspeed (meters per sec)")
#-------------------------------------------------------------------------------

#-------------------------------QUESTION 7--------------------------------------
outage_start <- datW$DD[is.na(datW$soil.moisture)][1]
pre_outage_start <- outage_start - 10
pre_outage_period <- datW$DD > pre_outage_start & datW$DD < outage_start

soil_moisture_before <- datW$soil.moisture[pre_outage_period]
soil_temp_before <- datW$soil.temp[pre_outage_period]

#make a plot of soil moisture
plot(datW$DD[pre_outage_period], soil_moisture_before,
     pch=19, type="b", main = "Bewkes Soil Moisture Before Outage",
     xlab = "Day of Year", ylab="Soil Moisture (m^3/m^3)")
#make a plot of soil temp
plot(datW$DD[pre_outage_period], soil_temp_before,
     pch=19, type="b", main = "Bewkes Soil Temperature Before Outage",
     xlab = "Day of Year", ylab="Soil Temp (Degrees Celsius)")

#make a plot of precipitation
plot(datW$DD[pre_outage_period], datW$precipitation[pre_outage_period],
     pch=19, type="b", main = "Bewkes Precipitation Before Outage",
     xlab = "Day of Year", ylab="Precipitation (mm)")
#make a plot of air temp
plot(datW$DD[pre_outage_period], datW$air.tempQ[pre_outage_period],
     pch=19, type="b", main = "Bewkes Air Temperature Before Outage",
     xlab = "Day of Year", ylab="Air Temp (Degrees Celsius)")
#-------------------------------------------------------------------------------

#-------------------------------QUESTION 8--------------------------------------
start_time = datW$timestamp[1]
end_time = datW$timestamp[length(datW$timestamp)]
num_obs = nrow(datW)

air_temp_col <- which(colnames(datW)=="air.tempQ")
wind_speed_col <- which(colnames(datW)=="wind.speedQ")
soil_mois_col <- which(colnames(datW)=="soil.moisture")
soil_temp_col <- which(colnames(datW)=="soil.temp")
aves <- apply(datW[,c(air_temp_col,wind_speed_col,soil_mois_col,soil_temp_col)], 2, mean, na.rm = TRUE)
total_precip <- sum(datW$precipitation)
#-------------------------------------------------------------------------------

#-------------------------------QUESTION 9--------------------------------------
#make a plot of soil moisture
plot(datW$DD, datW$soil.moisture,
     pch=19, type="b", main = "Bewkes Soil Moisture 2018",
     xlab = "Day of Year", ylab="Soil Moisture (m^3/m^3)")
#make a plot of soil temp
plot(datW$DD, datW$soil.temp,
     pch=19, type="b", main = "Bewkes Soil Temperature 2018",
     xlab = "Day of Year", ylab="Soil Temp (Degrees Celsius)")

#make a plot of precipitation
plot(datW$DD, datW$precipitation,
     pch=19, type="b", main = "Bewkes Precipitation 2018",
     xlab = "Day of Year", ylab="Precipitation (mm)")
#make a plot of air temp
plot(datW$DD, datW$air.tempQ,
     pch=19, type="b", main = "Bewkes Air Temperature 2018",
     xlab = "Day of Year", ylab="Air Temp (Degrees Celsius)")
#-------------------------------------------------------------------------------
