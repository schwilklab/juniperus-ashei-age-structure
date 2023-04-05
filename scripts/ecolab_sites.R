## ecolab_sites.R
## Alex Bowers, Dylan Schwilk


# needed packages

library(sf)
library(terra)
#library(ts) # What is this? Not used.
library(dplyr)
#library(tidyr)
library(geosphere)

#setwd("~/Downloads")
## DWS: Don't use setwd() in code!

## Creates a square bounding box centered at point p (long, lat) with sides of
## length `size` in meters. Returns matrix with columns: xmin ymin xmax ymax.
bbox_centered_at <- function(p, size=1500) {
  xmin <- destPoint(p, 270, size)[,1]
  ymin <- destPoint(p, 180, size)[,2]
  xmax <- destPoint(p, 90, size)[,1]
  ymax <- destPoint(p, 0, size)[,2]
  return(cbind(xmin, ymin, xmax, ymax))
}

## Data below provided by the EcoLab program through Brown and Gresham
p2022 <- st_read("./data/2022Properties.kml")
p2021 <- st_read("./data/2021Properties.kml")
sites <- rbind(p2022, p2021)
# Clean out extra all NA columns was in kml (probably an artifact of kml
# creation).
sites <- dplyr::select(sites, Name, geometry)
## These layers are coded in the kml as MULTIPOLYGONS, but there is, in fact,
## only one polygon per name so we should cast:
sites <- st_cast(sites, "POLYGON")

## Visual check: just show the polygon for one site
# ggplot(dplyr::filter(sites, Name=="Bandera 2022-35")) + geom_sf()
# ggplot(dplyr::filter(sites, Name=="Edwards 2022-25")) + geom_sf()


# Get centroids for each property:
sites <- sites %>% mutate(centroid = st_centroid(geometry))

# Extract centroid coordinates as matrix for use with geosphere::destPoint(()
centroids <- st_coordinates(sites$centroid)
bbox_centered_at(centroids)



## Read lcp data
hays_travis <- rast("results/Hays-Travis_2022/j365f451e55d241a390edcf03458afed3.tif")

terra::writeRaster(hays_travis, "results/attempt.lcp", filetype = "LCP", overwrite = TRUE)


## Alex's code:

# Finds min and max boundaries of each site

siteboundaries <- function(site) {
  sites <- t(sapply(1:length(site), function(i) as.vector(extent(site[i,]))))
  sites <- sites[rowSums(is.na(sites)) == 0, ] 
  names <- as.data.frame(site$Name) # ? why as.data.frame?
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
