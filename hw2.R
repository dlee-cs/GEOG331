# GEOG331 Activity 2 script
# DL 01/31/22

#set working directory to my noaa data folder
setwd("Z:/students/dslee/noaa_weather")

#make a vector of tree heights
heights <- c(30,41,20,22)

#convert heights to cm
heights_cm <- heights*100

#set up a matrix with 2 cols and fill in by rows
#first arg is vector of numbers to fill in the matrix
Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)

#read weather data
datw <- read.csv("2011124.csv")
