## properties_map.R
## Alex Bowers

### creates map to display property locations across texas

library(raster)
library(maps)
library(ggmap)
library(ggplot2)
library(scales)
library(mapproj)

## j. ashei cover map
# source: https://www.fs.usda.gov/database/feis/plants/tree/junash/all.html
jasheicover <- st_read("data/j_ashei_cover.kml")
jasheicover <- st_coordinates(jasheicover)
jasheicover <- as.data.frame(jasheicover)


# gets outlines of nearby states
us_states  <- c('Texas', 'New Mexico', 'Arizona', 'Ohklahoma', 'Louisiana', 'Arkansas','Missouri', 'Kansas','Colorado')
mex_states <- c("Tamaulipas", 'Nuevo León', 'Coahuila', 'Chihuahua', 'San Luis Potosí', 'Zacatecas', "Durango")
us <- getData("GADM",country="USA",level=1)
mexico <- getData("GADM",country="MEX",level=1)
us.states <- us[us$NAME_1 %in% us_states,]
mex.states <- mexico[mexico$NAME_1 %in% mex_states,]

# plot of area
ggplot() +
  # plots states
  geom_path(data = us.states, aes(x = long, y = lat, group = group)) +
  geom_path(data = mex.states, aes(x = long, y = lat, group = group)) +
  # limits area ans uses albers projection
  coord_map(xlim = c(-107, -93), ylim = c(25,37), project = 'albers', 
            parameters = c(30, 32)) +
  # adds j. ashei cover shapefile
  geom_polygon(data = jasheicover, aes(long, lat), 
               color = "#4EB265", fill = "#4EB265") +
  # adds property locations
  geom_point(data = properties, aes(long, lat), shape = 19, size = 3) +
  # aestetics
  labs(fill = "",
       x = expression("Longitude ("*~degree*")"),
       y = expression("Latitude ("*~degree*")")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = NA, linewidth = 1),
    text = element_text(family = "Times New Roman"),
    strip.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    panel.border = element_rect(linewidth = 2),
  )
