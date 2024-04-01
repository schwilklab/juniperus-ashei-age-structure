## trees_to_kml.R
## Alex Bowers

### creates kml file from trees_read_clean.R

library(sf)

# converts data frame for use in sf
trees_sf <- st_as_sf(trees, coords = c("long.x", "lat.x"), crs = 4326)

# writes a kml file of trees
st_write(trees_sf, "trees.kml")

