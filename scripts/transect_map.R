library(sf)
library(terra)
library(dplyr)
library(ggspatial)
library(metR)
library(patchwork)

# function to create countor maps of transect
transect_map <- function(transect_id) {
  ## long trasect to create map
  trees_id <- trees[trees$transect_id == transect_id,]
  
  # set bounds
  minlong <- (min(trees_id$long.x) - 0.0005)
  maxlong <- (max(trees_id$long.x) + 0.0005)
  minlat <- (min(trees_id$lat.x) - 0.0005)
  maxlat <- (max(trees_id$lat.x) + 0.0005)
  ## Define area
  coords <- data.frame(x = c(minlong, maxlong, minlong, maxlong),
                       y = c(maxlat, maxlat, minlat, minlat))
  coords.sf <- st_as_sf(coords, coords = c("x", "y"), crs = 4326)
  
  ## Download elevation data
  elev <- elevatr::get_elev_raster(coords.sf, z = 13)
  elev <- rast(elev)
  elev <- as.data.frame(elev, xy = TRUE) 
  colnames(elev)[3] <- "elev"
  
  # makes bonding box for transect
  elev <- elev[elev$x >= minlong,]
  elev <- elev[elev$x <= maxlong,]
  elev <- elev[elev$y <= maxlat,]
  elev <- elev[elev$y >= minlat,]
  
  #plot
  ggplot(trees_id, aes(long.x, lat.x)) + 
    geom_contour(data = elev, aes(x, y, z = elev), bins = 10, 
                 show.legend = TRUE, color = "black") +
    geom_text_contour(data = elev, aes(x, y, z = elev), skip = 1) +
    geom_point(size = 3) +
    xlim(minlong, maxlong) +
    ylim(minlat, maxlat) + 
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
}

kea_map <- transect_map("KEA")
keb_map <- transect_map("KEB")

fig2 <- kea_map + keb_map
