#### globe ####

source("libs.R")

library(globe)

png(filename = "cvore.globe.png", height = 600, width = 600, res = 150)
globeearth(eye=list(lon = 81.5,lat = 20), top=place("northpole"))
globepoints(loc = list(lon = 74.6, lat = 18.35), eye=list(lon = 81.5,lat = 20), top=place("northpole"), col =2, pch = 20)
globepoints(loc = list(lon = 74.6, lat = 18.35), eye=list(lon = 81.5,lat = 20), top=place("northpole"), col =2, pch = 3)
dev.off()
