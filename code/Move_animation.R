## Move Animation
# Edward Hurme
# 2016-09-21

# Load library
library(xlsx)
library(RgoogleMaps)
library(animation)
library(GISTools)
library(move)

# set working directory
setwd('/Users/Edward/Dropbox/animove16/data')

#store the movebank credentials
cred <- movebankLogin(username="R-Book", password="Obstberg1") #search for studies
# Be sure to login manually on movebank.org to accept the terms and conditions to download the data

# get data
turk <- 'Turkey Vulture Acopian Center USA GPS.csv'

turk.df <- read.csv(turk, as.is=TRUE)

turk.df$timestamp <- as.POSIXct(turk.df$timestamp,
                                   format="%F %T ", tz="UTC")

any(duplicated(turk.df[,c("timestamp",
                             "individual.local.identifier")]))

reduced <- turk.df[!duplicated(
  turk.df[,c("timestamp", "location.long", "location.lat",
                "individual.local.identifier")]),]

reduced <- reduced[order(reduced$individual.local.identifier, reduced$timestamp),]

## Done tyding
# define the data.frame as a move object after cleaning
tracks <- move(x=reduced$location.long,
                y=reduced$location.lat,
                time=reduced$timestamp,
                data=reduced,
                proj=CRS("+proj=longlat +datum=WGS84"),
                animal=reduced$individual.local.identifier)

# if there are no duplication issues then try to load from Movebank
# tracks <- getMovebankData(study=turk, login=cred)

xy <- coordinates(tracks@coords)
names <- tracks@trackId
time <- tracks@timestamps

data <- data.frame(names, xy, time)

head(data)

# sort the data by time step so that you can run through everything chronologically in the video
data <- data[order(data$time),]
head(data)

# get the center of the map
SGsatellite <- GetMap(center= c(lat = mean(data$coords.x2), lon = mean(data$coords.x1)), zoom=3, maptype="satellite")

# plot the map to make sure that this will be a good base for you animation, you will probably need to adjust the zoom
PlotOnStaticMap(SGsatellite)

#pdf("test.pdf") # good way to view data and manually animate by scrolling
saveVideo({ # requires FFmpeg
  #saveGIF({ # requires ImageMagick or GraphicsMagick

  int <- 1000 # interval you step through the data
  tail <- 2000 # how long the tail will be
  for(i in 1:(nrow(data)/int)){
    PlotOnStaticMap(SGsatellite)
    idx = (i*int-tail):(i*int)
    idx = idx[idx > 0]
    PlotOnStaticMap(SGsatellite, lat = c(data$coords.x2[idx]), lon = c(data$coords.x1[idx]), lwd=1.5, col= data$names[idx], FUN = points, add=TRUE)
    #map.scale(90,-250,len=300,"Km",4,0.5,sfcol='red') can't figure out scaling or positioning.

    names <-  sort(unique(data$names[idx]), decreasing = TRUE)

    # add legen for time and names
    legend("topright", legend = names, fill = names, bg = "white")
    legend("bottomleft", legend = data$time[i*int], bg = "white")
  }
  #dev.off() # when using pdf
  #}, "test.gif", interval = 0.05)
}, "flight_new.mp4", interval = 0.08) # you will want to play with the interval for your data


## Also, if you want access to a faster way to download remote sensing data directly to R, I recommend checking out Xtractomatic. https://github.com/rmendels/xtractomatic
# Here is the website to find all the available remote sensing products:
# http://coastwatch.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000
