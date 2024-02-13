## properties_map.R
## Alex Bowers

### creates map to display property locations across texas

library(maps)
library(tmap)
library(dplyr)
library(ggplot2)


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

colorscheme <- c(
  "Bandera_2022-35" = "#AA6F9E",
  "Burnet_2022-16" = "#1965B0",
  "Edwards_2022-25" = "#7BAFDE",
  "Hays-Travis_2022-02" = "#4EB265",
  "Kerr_2021-36" = "#F7F056",
  "Medina_2022-04" = "#EE8026",
  "Uvalde_2021-03" = "#DC050C"
)

ggplot(texas, aes(long, lat)) +
  geom_polygon(color = "black", fill = "#BBBBBB") +
  theme_bw() +
  geom_polygon(aes(group = group), data = texas_counties,
               fill = "NA", color = "white") +
  geom_polygon(color = "black", fill = "NA") +
  geom_point(data = site_map, aes(color = property_id),
             shape = 19, size = 7) +
  labs(fill = "",
       x = expression("Longitude ("*~degree*")"),
       y = expression("Latitude ("*~degree*")"),
       color = "Property") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = NA, linewidth = 1),
    text = element_text(family = "Times New Roman"),
    strip.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    panel.border = element_rect(linewidth = 2),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 17),
    legend.position = c(0.15, 0.8)
  ) +
  scale_color_manual(values = colorscheme)



#### properties table
data <- data.frame(
  Category = c("A", "B", "C", "D"),
  Value = c(10, 20, 15, 25)
)