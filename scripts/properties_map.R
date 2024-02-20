## properties_map.R
## Alex Bowers

### creates map to display property locations across texas

library(maps)
library(tmap)
library(dplyr)
library(ggplot2)
library(sf)


# gets map data for texas
texas <- map_data("state") %>%
  filter(region == "texas")

## j. ashei cover map based on https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0173465
jasheicover <- st_read("data/j_ashei_cover.kml")
jasheicover <- st_coordinates(jasheicover)
jasheicover <- as.data.frame(jasheicover)
colnames(jasheicover) <- c("long", "lat", "z", "L1", "L2")


long_labels <- c(-98, -98.4, -101, -96.5, 
                 -99.7, -97.7, -99.5)
lat_labels <- c(29.9, 31.1, 29.95, 30.35,
                30.35, 29.5, 29.2)


## plots map

ggplot(texas, aes(long, lat)) +
  geom_polygon(color = "black", fill = "NA") +
  geom_polygon(data = jasheicover, aes(long, lat), 
               color = "#4EB265", fill = "#4EB265") +
  geom_point(data = properties, shape = 19, size = 4) +
  geom_text(data = properties, aes(label = property_id),
            x = long_labels, y = lat_labels, size=4) +
  labs(fill = "",
       x = expression("Longitude ("*~degree*")"),
       y = expression("Latitude ("*~degree*")")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = NA, linewidth = 1),
    text = element_text(family = "Times New Roman"),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    panel.border = element_rect(linewidth = 2),
  )