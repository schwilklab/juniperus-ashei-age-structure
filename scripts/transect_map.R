library(sf)
library(terra)
library(dplyr)
library(ggspatial)

## long trasect to create map
trees_keb <- trees[trees$transect_id == "KEB",]


## Define area
coords <- data.frame(x = c(-99.402, -99.4, -99.402, -99.4),
                     y = c(30.0725, 30.0725, 30.070, 30.070))
coords.sf <- st_as_sf(coords, coords = c("x", "y"), crs = 4326)

## Download elevation data
elev.ras <- elevatr::get_elev_raster(coords.sf, z = 13)
elev <- rast(elev.ras)
elev <- as.data.frame(elev, xy = TRUE) 
colnames(elev)[3] <- "elev"

# makes bonding box for transect
elev <- elev[elev$x >= -99.402,]
elev <- elev[elev$x <= -99.4,]
elev <- elev[elev$y <= 30.0725,]
elev <- elev[elev$y >= 30.07,]



ggplot(trees_keb, aes(long.x, lat.x)) + 
  geom_contour_filled(data = elev, aes(x, y, z = elev), bins = 10, 
                      show.legend = TRUE) +
  scale_fill_manual(values = c("#D7D7C8", "#A5AA8C", "#C8C3A0", "#E1D2A2", "#D2B496", 
                      "#CDA087", "#AF7873", "#A05555", "#914137"),
                    labels = c("580-586", "586-590", "592-595", "596-600", "601-605", 
                               "606-610", "611-615", "616-620", "621-625")) +
  geom_point(size = 3) +
  xlim(-99.402, -99.4) +
  ylim(30.07, 30.0725) + 
  theme_bw() +
  labs(fill = "Elevation (m)") +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    text = element_text(family = "Times New Roman"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(linewidth = 2)) +
  annotation_scale(plot_unit = "km", height = unit(0.25, "cm")) +
  annotation_north_arrow(pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

