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
  facet_wrap(~transect_id) +
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring Count",
    color = "Transect"
  )

# plot all sites together
ggplot(tree_ages, aes(distance, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring Count",
    color = "Property"
  ) +
  theme_bw(base_size = 18)

# distribution of ages
hist(tree_ages$year, breaks = 10, xlab="Ring Count", freq = TRUE,
     main="", xlim=c(0,250), ylim = c(0,50))

summary(tree_ages)
