source("libs.R")
source("ggplot.opts.r")

library(readr)

a = read_csv("bmt_ndvi_time.csv")

a$time = as.Date(a$`system:time_start`, tz = "IST", format = "%b %d, %Y")
plot(a$ndvi~a$time, type ="l")
