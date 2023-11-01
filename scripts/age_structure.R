# Alex Bowers

library(geosphere)
library(ggplot2)
library(elevatr) 

# merge field data with ring counts
trees <- dplyr::left_join(trees, ages, by = "id")



##############################################################################
## distance from transect start
##############################################################################
samplingpoint <- data.frame(long=trees$long.x, lat=trees$lat.x)
transectpoint <- data.frame(long=trees$transect_long, 
                            lat=trees$transect_lat)
trees$distance <- distHaversine(samplingpoint, transectpoint)



##############################################################################
## elevation from transect start
##############################################################################
# function to find elevation for every point
# gets every coordinates elevation
get_cords <- function(long, lat, id) {
  sample_pts <- data.frame(x = long, 
                           y = lat, 
                           names = id)
  # wgs 84
  elev <- get_elev_point(locations = sample_pts, prj = 4326)
  return(elev[[3]])
}

# finds different in elevation between transect start and sampling point
trees$sample_elev <- get_cords(trees$long.x, trees$lat.x, trees$id)
trees$transect_elev <- get_cords(trees$transect_long, trees$transect_lat, trees$id)

trees$elev_dif <- trees$sample_elev - trees$transect_elev




### plots


# theme for plots
theme <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    strip.background = element_rect(fill = NA, linewidth = 1),
    strip.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    panel.border = element_rect(linewidth = 2)
  )

##############################################################################
## disance plots 
##############################################################################
# all plots age structure 
ggplot(trees, aes(distance, year, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id) +
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring Count",
    color = "Transect"
  )
# dbh vs distance 
ggplot(trees, aes(distance, dbh, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id) +
  labs(
    x = "Distance from start of transect (m)",
    y = "DBH",
    color = "Transect"
  )
# plot all sites together age vs distance
ggplot(trees, aes(distance, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) + theme
 # all sites together age vs distance
ggplot(trees, aes(distance, dbh, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Diameter at breast height (cm)",
    color = "Property"
  ) + theme

##############################################################################
## elevation plots
##############################################################################
# all together 
ggplot(trees, aes(elev_dif, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) + theme +
  theme(legend.position = c(0.85, 0.75))
# seperated by site
ggplot(trees, aes(elev_dif, year, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id) +
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring Count",
    color = "Transect"
  )
