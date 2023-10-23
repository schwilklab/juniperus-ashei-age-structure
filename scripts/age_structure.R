library(geosphere)
library(ggplot2)

# merge field data with ring counts
tree_ages <- dplyr::left_join(trees, ages, by = "id")


# distance from transect start
samplingpoint <- data.frame(long=tree_ages$long, lat=tree_ages$lat)
transectpoint <- data.frame(long=tree_ages$transect_long, lat=tree_ages$transect_lat)
tree_ages$distance <- distHaversine(samplingpoint, transectpoint)

# all plots age structure
ggplot(tree_ages, aes(distance, year, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id)
