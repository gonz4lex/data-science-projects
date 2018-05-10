## Location Data project


library(jsonlite)
library(lubridate)
library(zoo)
library(tidyverse)
library(ggmap)

maps = fromJSON("Location History.json")

# This is in JSON, therefore nested. Extracting the locations dataframe
loc = maps$locations

# converting time column from posix milliseconds into a readable time scale
loc$datetime = as.POSIXct(as.numeric(maps$locations$timestampMs)/1000, origin = "1970-01-01")

# converting longitude and latitude from E7 to GPS coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

head(loc)

nrow(loc) ; min(loc$datetime) ; max(loc$datetime)

loc$date <- zoo::as.Date(loc$datetime, '%Y/%m/%d')
loc$time <- format(loc$datetime, "%H:%M")
loc$year <- year(loc$date)
loc$month_year <- as.yearmon(loc$date)

# spa <- c("ene", "abr", "ago", "dic")
# eng <- c("jan", "apr", "aug", "dec")
# 
# head(mapply(gsub, spa, eng, loc$month_year))

majorca <- get_googlemap(center = c(lon = 2.9349075, lat = 39.6139885), zoom = 9,
                         maptype = "roadmap", color = "bw")

ggmap(majorca) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.2, color = "red") + 
  coord_quickmap(c(2.3, 3.5), c(39.2, 40)) + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in Majorca",
    caption = "\nA simple point plot shows recorded positions.")

work <- filter(loc, )

# points_p_day <- data.frame(table(loc$date), group = "day")
# points_p_month <- data.frame(table(loc$month_year), group = "month")
# points_p_year <- data.frame(table(loc$year), group = "year")


## ideas: create data subsets for those location points between 9am-5pm, and between 9pm-7am to
## predict home and workplace location
## maybe make a shiny app?
