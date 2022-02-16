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

# ---------------------------------QUESTION 4-----------------------------------
#add all 4 histogram plots into same window
par(mfrow=c(2,2))

# *************histogram for the FIRST site in our levels ***************
#make histogram of daily average temps
h1 <- hist(datW$TAVE[datW$siteN == 1],
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
#add normal distribution line
x.plot <- seq(-10,30, length.out = 100)
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# *************histogram for the SECOND site in our levels ***************
#make histogram of daily average temps
h2 <- hist(datW$TAVE[datW$siteN == 2],
     freq = FALSE,
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "pink",
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

#add normal distribution line
x.plot <- seq(0,40, length.out = 100)
y.plot <-  dnorm(seq(0,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE))
y.scaled <- (max(h2$density)/max(y.plot)) * y.plot
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# *************histogram for the FOURTH site in our levels ***************
#make histogram of daily avg temps
h4 <- hist(datW$TAVE[datW$siteN == 4],
     freq = FALSE,
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "blue",
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

#add normal distribution line
x.plot <- seq(0,40, length.out = 100)
y.plot <-  dnorm(seq(0,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE))
y.scaled <- (max(h4$density)/max(y.plot)) * y.plot
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# *************histogram for the FIFTH site in our levels ***************
#make histogram of daily avg temps
h5 <- hist(datW$TAVE[datW$siteN == 5],
     freq = FALSE,
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "orange",
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

#add normal distribution line
x.plot <- seq(-30,30, length.out = 100)
y.plot <-  dnorm(seq(-30,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE))
y.scaled <- (max(h5$density)/max(y.plot)) * y.plot
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)


#---------------------------------QUESTION 6------------------------------------
#for Aberdeen, WA
curr_extreme_high_threshold <- round(qnorm(0.95,
                                           mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                           sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)),
                                     1)
increased_mean <- mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + 4
curr_sd <- sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)
p_extreme_high <- round(1 - pnorm(curr_extreme_high_threshold,
                                  increased_mean,
                                  curr_sd),
                        4)
print(paste("PROB of EXTREME HIGH TEMPS:", p_extreme_high))


#---------------------------------QUESTION 7------------------------------------
#make histogram of daily precipitation in site 1 (Aberdeen,WA)
h1_daily_p <- hist(datW$PRCP[datW$siteN == 1],
           freq = FALSE,
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily precipitation (mm)",
           ylab = "Relative frequency",
           col = "grey50",
           border = "white")


#---------------------------------QUESTION 8------------------------------------
#get annual precip across sites
annualPrecip <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum", na.rm=TRUE)

#create meaningful col names 
colnames(annualPrecip) <- c("SITE","YEAR", "AP")

#convert level to number for factor data type
annualPrecip$siteN <- as.numeric(annualPrecip$SITE)

#histogram of annual precipitation at site 1 (Aberdeen, WA)
h1_annual_p <- hist(annualPrecip$AP[annualPrecip$siteN == 1],
           freq = FALSE,
           main = paste(levels(annualPrecip$SITE)[1]),
           xlab = "Annual Precipitation (mm)",
           ylab = "Relative frequency",
           col = "grey50",
           border = "white")



