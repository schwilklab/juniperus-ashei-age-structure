## ecolab_sites.R
## Alex Bowers, Dylan Schwilk


# needed packages

library(sf)
library(raster)
#library(ts) # What is this? Not used.
library(dplyr)
#library(tidyr)
library(geosphere)

#setwd("~/Downloads")
## DWS: Don't use setwd() in code!





## Data below provided by the EcoLab program through Brown and Gresham
p2022 <- st_read("./data/2022Properties.kml")
p2021 <- st_read("./data/2021Properties.kml")
sites <- rbind(p2022, p2021)

# Clean out NA stuff that was in kml for some reason:
sites <- dplyr::select(sites, Name, geometry)
## DWS but these layers are MULTIPOLYGONS But that is weird, each site should
## be just one polygon.
sites <- st_cast(sites, "POLYGON")


# Just show the polygon for one site
ggplot(dplyr::filter(sites, Name=="Bandera 2022-35")) + geom_sf()
ggplot(dplyr::filter(sites, Name=="Edwards 2022-25")) + geom_sf()

# Get centroids
sites <- sites %>% mutate(centroid = st_centroid(geometry))

# in own matrix
centroids <- st_coordinates(sites$centroid)

destPoint(centroids, 0, 1500)
destPoint(centroids, 0, -1500)
destPoint(centroids, 90, 1500)
destPoint(centroids, 90, -1500)


# Finds min and max boundaries of each site

siteboundaries <- function(site) {
  sites <- t(sapply(1:length(site), function(i) as.vector(extent(site[i,]))))
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
