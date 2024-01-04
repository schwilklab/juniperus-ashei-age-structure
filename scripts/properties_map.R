## properties_map.R
## Alex Bowers

### creates map to display property locations across texas

library(maps)
library(tmap)


# selects needed variables
site_map <- select(properties, property_id, long, lat)

# get data for mapping texas with counties
### help from azaj

# gets map data for texas
texas <- map_data("state") %>%
  filter(region == "texas")

# gets map data for texas counties
texas_counties <- map_data("county") %>%
  filter(region == "texas")


ggplot(texas, aes(long, lat)) +
  geom_polygon(color = "black", fill = "grey") +
  theme_bw() +
  geom_polygon(aes(group = group), data = texas_counties,
               fill = "NA", color = "white") +
  geom_polygon(color = "black", fill = "NA") +
  geom_point(data = site_map, aes(color = property_id),
             shape = 19, size = 5) +
  labs(fill = "",
       x = expression("Longitude ("*~degree*")"),
       y = expression("Latitude ("*~degree*")"),
       color = "Property") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = NA, linewidth = 1),
    strip.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    panel.border = element_rect(linewidth = 2),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = c(0.15, 0.8)
  )
