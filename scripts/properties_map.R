## properties_map.R
## Alex Bowers

### creates map to display property locations across texas

library(sf)
library(rnaturalearth)
library(ggplot2)

# properties
properties <- read.csv("data/properties.csv")

## j. ashei cover map
# source: https://www.fs.usda.gov/database/feis/plants/tree/junash/all.html
jasheicover <- st_read("data/jasheicover.kml")

# gets outlines of nearby states
us_states <- ne_states(country = "United States of America", returnclass = "sf")
mex_states <- ne_states(country = "Mexico", returnclass = "sf")

fig1 <- ggplot() +
  # us states
  geom_sf(data = us_states, fill = "white", color = "black", size = 0.2) +
  # mex states
  geom_sf(data = mex_states, fill = "white", color = "black", size = 0.2) +
  # cover
  geom_sf(data = jasheicover, aes(geometry = geometry), 
          fill = "#4EB265", color = "#4EB265") +
  # boudnign box
  coord_sf(xlim = c(-107, -91), ylim = c(25,37), expand = FALSE) +
  # adds property locations
  geom_point(data = properties, aes(long, lat), shape = 19, size = 2) +
  # theme
  labs(fill = "",
       x = expression("Longitude"),
       y = expression("Latitude")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = NA, linewidth = 1),
    text = element_text(family = "Times New Roman"),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.border = element_rect(linewidth = 1.5),
  )

fig1
