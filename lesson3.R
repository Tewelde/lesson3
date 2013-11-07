setwd("E:/lesson 3")
getwd()
datdir <- file.path("data")
datdir <- "data"
library(rasta)
## Packages for Reading/Writing/Manipulating Spatial Data
library(rgdal) # reading shapefiles and raster data
library(rgeos) # vector manipulations
library(maptools) # mapping
library(spdep) # useful spatial stat functions
library(spatstat) # functions for generating random points
library(raster) # raster data analysis
## Packages for Data Visualization and Manipulation
library(ggplot2) # plotting
library(reshape2) # preparing your data
library(scales)
?scale


##OGR
download.file("http://rasta.r-forge.r-project.org/kenyashape.zip",
                file.path(datdir, "kenyashape.zip"))
unzip(file.path(datdir, "kenyashape.zip"), exdir= datdir)
kenya <- readOGR(dsn = datdir, layer = "kenya")
kenya <- readOGR(dsn = datdir, layer = "kenya")
projection(kenya)
str(kenya)
spplot(kenya)
summary(kenya)
str(kenya,2)
names(kenya)
kenya@data
## ACCESS THE SHAPEFILE DATA
dsdat <- as(kenya, "data.frame")
# extract the data into a regular data.frame
  head(dsdat)
##
kenya$new <- 1:nrow(dsdat)
# add a new colunm,
head(kenya@data)
kenya$new<- 1:nrow(dsdat)
## READ AND EXPLORE A CSV
filepath <- system.file("extdata/kenpop89to99.csv", package ="rasta")
d <- read.csv(filepath)
summary(d)
str(d)
d[1,] 
d[,1]
d[1,1] 
d[,1:5] 
d[,c(1,4,5)] 
d$variable same as above, but returns the column as a vector
d[d$variable>10,] 

## EXTRACT COLUMNS FROM CSV
names(d)
d <- d[,c("ip89DId", "PopChg", "BrateChg", "Y89Pop", "Y99Pop")]
#Grab only the colunms we want
names(d)
str(d)
nrow(d)
d <- unique(d) #get rid of duplicate
str(d)
str(d[!d$PopChg>30,])

## EXPLORE MERGE AND DO A TABLE JOIN
#First a basic Merge Just to Demonstrate
d2 <- kenya@data
names(d)
[1] "ip89DId" "PopChg" "BrateChg" "Y89Pop" "Y99Pop"
names(d2)
[1] "ip89DId" "ip89DName" "new"
d3 <- merge(d,d2)
#They have common colunm names so we don't have to specify what to join on
head(d3)
d4 <- merge(d,kenya) #This will produce the same result.
head(d4)
str(d)

?match
ds1 <- kenya ## take a copy as backup
str(as(kenya,"data.frame"))
str(d)

kenya@data <- data.frame(as(kenya,"data.frame"),
                           d[match(kenya@data[,"ip89DId"], d[,"ip89DId"]),])

a<- as(kenya,"data.frame")     
c<- match(kenya@data[,"ip89DId"], d[,"ip89DId"])
b<-d[c,]

match(kenya@data[,"ip89DId"], d[,"ip89DId"])
kenya@data
kenya extent 

## GENERATE RANDOM POINTS
win <- bbox(kenya)
#the bounding box around the Kenya dataset
win
win <- t(win)
#transpose the bounding box matrix
win
win <- as.vector(win)
#convert to a vector for input into runifpoint()
win
dran1 <- runifpoint(100, win = as.vector(t(bbox(kenya))))
#create 100 random points for dran1 and dran2 ####################################
plot(kenya)
win <- extent(kenya)
dran1 <- runifpoint(n = 100, win = as.vector(win))
plot(dran1, add = TRUE, col = "red")
dran2 <- runifpoint(n = 100, win = as.vector(win))
plot(dran2, add = TRUE, col = "blue", pch = 19, cex = 0.5)

##CONVERT RANDOM POINTS TO DATA.FRAME
dp <- as.data.frame(dran2)
#This creates a simple data frame with 2 colunms, x and y
head(dp)
#Now we will add some values that will be aggregated in the next exercise
dp$values<-rnorm(100,5,10)
#generates 100 values from a Normal distribution with mean 5, and sd-10
head(dp)
plot(dp, add = TRUE, col = "red", pch = 19, cex = 0.5)
## CONVERT RANDOM POINTS TO SPATIAL POINTS DATAFRAME
  dsp <- SpatialPointsDataFrame(coords = dp[, c("x","y")],
                                  data = data.frame("values" = dp$values))
summary(dsp)
plot(dsp)
dsp@proj4string <- kenya@proj4string
summary(dsp)

## POINT IN POLY JOIN
#The data frame tells us for each point the index of the polygon it falls into
dsdat <- over(kenya, dsp, fn = mean)
head(dsdat)

inds <- row.names(dsdat)
#get the row names of dsdat so that we can put the data back into the shape file
head(inds)
names(kenya@data)
kenya@data[inds, "pntvals"] <- dsdat
head(kenya@data)
spplot(kenya)
##cliping raster
library(rasta)
filepath <- system.file("extdata", "anom.2000.03.tiff", package ="rasta")
g <- raster(filepath)
plot(g)
plot(kenya, add = TRUE) # plot kenya on top to get some sense of the extent
## Crop the Raster Dataset to the Extent of the Kenya Shapefile
gc <- crop(g, kenya,filename="data/croppedkenya.tif") #clip the raster to the extent of the shapefile
#Then test again to make sure they line up
plot(gc)
gc<- raster("data/croppedkenya.tif")
writeRaster(gc,filename="data/croppedkenya_1.tif" )
plot(kenya, add = TRUE)
gc
?crop


