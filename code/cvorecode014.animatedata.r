source("libs.R")

#'load as rdata
load("cvore.full.rdata")


## load raster layers
library(raster)
#'load ndvi
ndvi = raster::raster("cvore.ndvi.wgs84.tif")
#'load landuse
landuse = raster("cvore.landuse.wgs84.tif")
#'stack
env = stack(ndvi, landuse)

## get move data
#'get the julian dayas, filter for days between 50 and 150, look at the fix freq plot for that
library(move);library(lubridate)
#'arrange by id, time
cvore = cvore.rev %>% filter(id != "Cat 08") %>% arrange(id, time) %>% mutate(jday = round(as.numeric(julian.POSIXt(time, origin = "2017-01-01")))) %>% 
  #filter(jday %in% 90:120) %>% 
  sample_frac(1) %>% arrange(id, time) %>% mutate(overlap = ifelse(is.na(overlap), 0, overlap)) %>% na.omit() %>% arrange(id, time)
#'get utm zone
utm43n = "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs"

#'convert to a move object by id and get basic move parameters
cvore.move = split(move(x = cvore$long, y = cvore$lat, time = as.POSIXct(cvore$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), proj = crs(landuse), animal = cvore$id, data = cvore))


## get external libs

library(moveVis)
#get libraries 
conv_dir <- get_libraries()
#'get external format options
get_formats()


## set video metadata options


img_title <- "Movement of mesocarnivores in human dominated landscapes in India"
img_sub <- "Feb 19 - Mar 22"
img_caption <- "Projection: UTM 43N; Sources: Movebank 2018; Sentinel 2 data"


## load settlements


#library(sf)
#'load settlements as sf, then make df of coords and names
#settle = st_read("Shapefiles/Settlements/Settlements.shp")
#settle = st_transform(settle, 4326)
#ext = st_read("bmt_studyarea/BMT extent.shp")
#ext = st_transform(ext, 4326)

#'get df
#settle = as.data.frame(st_coordinates(st_intersection(y = st_centroid(settle), x = ext)))
#settle$name = paste("settlement",1:4,sep="_")
#colnames(settle) = c("x","y","names")

#'palette
#pal.indiv = viridis(3)[as.numeric(as.factor(unlist(map(cvore.move, function(x) x@idData$sp))))]
pal = c("#a6611a", "#01a750", "#f5f5a2", "#80cd61", "#005c87")
library(RColorBrewer)

pal.indiv = brewer.pal(3, "Set1")[as.numeric(as.factor(unlist(map(cvore.move, function(x) x@idData$sp))))]

gc()
save.image("animate.rdata.RData")
