## ecolab_sites.R
## Alex Bowers, Dylan Schwilk

library(sf)
library(terra)
library(dplyr)
library(geosphere)
library(ggplot2)


# landscape size for fire modeling. Should have at least 4980 m buffer on each
# edge according to recommendations such as
# https://iftdss.firenet.gov/firenetHelp/help/pageHelp/content/20-landscapes/lcpbuffering.htm?tocpath=Using%20Landscapes%7C_____6
LANDSCAPE_SIZE <- 10960

# This means we make all of our landscapes squares that are 4980+4980+1000
# since we have no properties with extends over 1 km, right?


## Creates a square bounding box centered at point p (long, lat) with sides of
## length `size` in meters. Returns matrix with columns: xmin ymin xmax ymax.
bbox_centered_at <- function(p, size=LANDSCAPE_SIZE) {
  xmin <- destPoint(p, 270, size)[,1]
  ymin <- destPoint(p, 180, size)[,2]
  xmax <- destPoint(p, 90, size)[,1]
  ymax <- destPoint(p, 0, size)[,2]
  return(cbind(xmin, ymin, xmax, ymax))
}

## Data below provided by the EcoLab program through Brown and Gresham
## p2022 <- st_read("./data/2022Properties.kml")
## p2021 <- st_read("./data/2021Properties.kml")
## sites <- rbind(p2022, p2021)
sites <- st_read("./data/merged_cleaned.kml")

# Clean out extra all NA columns was in kml (probably an artifact of kml
# creation).
sites <- dplyr::select(sites, property_id=Name, geometry)
# Remove Burnet_202216 polygons for property with two parts: merged version is
# already in the data:
sites <- filter(sites, !grepl("[0-9][ab]", property_id))
# Get centroids for each property:
sites <- sites %>% mutate(centroid = st_centroid(geometry))

# Extract centroid coordinates as matrix for use with geosphere::destPoint(()
centroids <- st_coordinates(sites$centroid)
sites <- cbind(sites, bbox_centered_at(centroids))

# read the data on properties
properties <- read.csv("./data/properties.csv", stringsAsFactors=FALSE) %>%
  left_join(sites)

## Visualize one example:
hays_travis <- rast("results/Hays-Travis_2022/j365f451e55d241a390edcf03458afed3.tif")
hays_travis <- rast("./results/bad_aspect.tif")


h2 <- project(hays_travis, sites) 

## Visual check: just show the polygon for one site
rdf <- as.data.frame(h2, xy=TRUE)
ggplot(dplyr::filter(sites, property_id=="Hays-Travis_2022-02")) +
  geom_raster(data=rdf, aes(x=x,y=y,fill=US_ELEV2020)) +  geom_sf(linewidth=1, fill=NA)


# Convert to lcp:


