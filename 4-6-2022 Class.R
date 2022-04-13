# Devon Lee
# GEOG 331
# Wed 4/6/22

# load the terra package
library(terra)

# Determine operating system
OS <- .Platform$OS.type
# Set path for operating system
if (OS == "unix") {
  path <- "/Volumes/class/GEOG331_S22/students/dslee/" # MAC file path
} else if (OS == "windows") {
  path <- "Z:/GEOG331_S22/students/dslee" # windows file path
} else {
  print("ERROR: OS could not be identified")
}

# set working directory
setwd(path)

# read a raster from file
p <- rast("/Volumes/class/GEOG331_S22/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

#plot the raster
plot(p)

# plot an rgb rendering of the data
plotRGB(p, r=3, g=2, b=1,
        scale = 65535,
        stretch = "hist") #increase contrast of image

#read file with field observations of canopy cover
tree <- read.csv("/Volumes/class/GEOG331_S22/data/rs_data/siberia_stand_data.csv",
                 header = T)
#convert to vector object using terra package
gtree <- vect(tree, geom = c("Long","Lat"), "epsg:4326") #last param = coord syst

#project data to match the coord system of the raster layer
gtree2 <- project(gtree,p)

#create a polygon from the extent of the points
#must specify coordinate reference system crs()
bounding_box <- as.lines(ext(gtree), "epsg:4326")

# plot an rgb rendering of the data
plotRGB(gtree2, r=3, g=2, b=1,
        scale = 65535,
        stretch = "hist")

plot(gtree2, add=T, col = "red")

#reproject the polygons to the same proj as raster
b2 <-project(bounding_box, crs(p))

#buffer the extent by 200m
b3 <- buffer(b2, width=200)

plot(b2, add=T)
plot(b3, add=T)

#use this to crop the raster layer so we can see just our study area
