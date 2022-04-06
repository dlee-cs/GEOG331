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
