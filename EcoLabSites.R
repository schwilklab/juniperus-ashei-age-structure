# needed packages

library(sf)
library(raster)
library(tidyr)

# downloaded kml files of properties

setwd("~/Downloads")
p2022 <- st_read("2022Properties.kml")
p2021 <- st_read("2021Properties.kml")

class(p2022$geometrty)


# Finds min and max boundaries of each site

siteboundaries <- function(site) {
  sites <- t(sapply(1:length(site$Name), function(i) as.vector(extent(site[i,]))))
  sites <- sites[rowSums(is.na(sites)) == 0, ] 
  names <- as.data.frame(site$Name)
  sites <- data.frame(names, sites)
  colnames(sites) <- c('propertyID', 'xmin', 'xmax', 'ymin', 'ymax')
  return(sites)
}

## combines sepereate kml files into one data frame
propertyboundaries <- rbind(siteboundaries(p2021), siteboundaries(p2022))
propertyboundaries



# middle point of sites

## selects x values and finds middle point
lats <- propertyboundaries[,2:3]
lats <- apply(lats, 1, mean)
lats <- as.data.frame(lats)

## selects y values and finds middle point
longs <- propertyboundaries[,4:5]
longs <- apply(longs, 1, mean)
longs <- as.data.frame(longs)

## creates data frame of middle points
propertynames <- as.data.frame(propertyboundaries$propertyID)
propertycenter <- data.frame(propertynames, lats, longs)
colnames(propertycenter) <- c('propertyID', 'x', 'y')  



# creates buffer around site

## 1.5 km buffer for 30 degrees lat
lat <- 1.5/96.49
long <- 1.5/96

## adds buffer to make a square and makes it into a data frame
xmax <- as.data.frame(propertycenter$x + lat)
xmin <- as.data.frame(propertycenter$x - lat)
ymax <- as.data.frame(propertycenter$y + long)
ymin <- as.data.frame(propertycenter$y - long)
flamsites <- data.frame(propertynames, xmax, xmin, ymax, ymin)
colnames(flamsites) <- c('propertyID', 'xmin', 'xmax', 'ymin', 'ymax')  
flamsites
csv