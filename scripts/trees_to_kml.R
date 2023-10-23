# Alex Bowers

## creates kml file from trees_read_clean.R

library(sf)

head(trees)

trees_sf <- st_as_sf(trees, coords = c("long", "lat"), crs = 4326)            
st_write(trees_sf, "trees.kml")
