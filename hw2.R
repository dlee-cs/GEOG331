# GEOG331 Activity 2 script
# DL 01/31/22

#set working directory to my noaa data folder
#CHANGE BEFORE SUBMITTING setwd("Z:/students/dslee/noaa_weather")
setwd("/Users/nancychoy/Desktop/Env Data Science/GEOG331/noaa_weather")

#read in weather data
datW <- read.csv("2011124.csv", stringsAsFactors = T)

#specify a column with a proper date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#create avg daily temp col (avg is halfway between min and max temp)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#by takes a list of variables to index over.
#FUN indicates the function we want to use
#specify any function-specific arguments with comma after the function
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)

#create meaningful col names 
#MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")

#convert level to number for factor data type
#to see the character designation, reference level output (attributes(datW$NAME)) 
#OR look at the row of data 
datW$siteN <- as.numeric(datW$NAME)

# *************histogram for the FIRST site in our levels ***************
#main is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
hist(datW$TAVE[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")

#add mean line with red color with thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line BELOW the mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, #linetype
       lwd = 3)
#add standard deviation line ABOVE the mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, 
       lwd = 3)

# ---------------------------------QUESTION 4-----------------------------------
# *************histogram for the SECOND site in our levels ***************
hist(datW$TAVE[datW$siteN == 2],
     freq = FALSE,
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")

#add mean line with red color with thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line BELOW the mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, #linetype
       lwd = 3)
#add standard deviation line ABOVE the mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, 
       lwd = 3)

# *************histogram for the FOURTH site in our levels ***************
hist(datW$TAVE[datW$siteN == 4],
     freq = FALSE,
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")

#add mean line with red color with thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line BELOW the mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, #linetype
       lwd = 3)
#add standard deviation line ABOVE the mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, 
       lwd = 3)

# *************histogram for the FIFTH site in our levels ***************
hist(datW$TAVE[datW$siteN == 5],
     freq = FALSE,
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")

#add mean line with red color with thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line BELOW the mean
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, #linetype
       lwd = 3)
#add standard deviation line ABOVE the mean
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3, 
       lwd = 3)
#-------------------------------------------------------------------------------

