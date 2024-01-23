## age_structure.R
## Alex Bowers

### calculates distance and elevation from transect start 

library(geosphere)
library(ggplot2)
library(elevatr) 
library(car)

# merge field data with ring counts
trees <- dplyr::left_join(trees, age, by = "id")

## distance from transect start

# calculates distance from transect start to the point sampled
samplingpoint <- data.frame(long=trees$long.x, lat=trees$lat.x)
transectpoint <- data.frame(long=trees$transect_long, 
                            lat=trees$transect_lat)
trees$distance <- distHaversine(samplingpoint, transectpoint)


## elevation from transect start

# function to find elevation for every point
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



## plots

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

## disance plots 

# age vs distance
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

# age vs distance
ggplot(trees, aes(distance, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) + theme

 # dbh vs distance
ggplot(trees, aes(distance, dbh, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Diameter at breast height (cm)",
    color = "Property"
  ) + theme

# age vs elevation
ggplot(trees, aes(elev_dif, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) + theme +
  theme(legend.position = c(0.85, 0.75))

# age vs elevatio
ggplot(trees, aes(elev_dif, year, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id) +
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring Count",
    color = "Transect"
  )





###  linear mixed effect model 
# distance
dist_model <- lmer(year ~ elev_dif + (1 |transect_id) +(1 |dbh), data = trees)

dist_model_anova = Anova(dist_model, type = 2, test.statistic = "F")
# P = 0.8167, not significant

dist_model_anova_coeff = summary(dist_model)$coefficients
dist_model_anova_coeff[1]

###plots
# Linear Mixed effect mdel
ggplot(trees, aes(elev_dif, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) +
  # Estiamte
  geom_abline(intercept = dist_model_anova_coeff[1], 
              slope = dist_model_anova_coeff[2],
              size = 1.5,
              color = "black") +
  # standard error
  geom_abline(intercept = (dist_model_anova_coeff[1] + 
                             dist_model_anova_coeff[1,2]), 
              slope = dist_model_anova_coeff[2],
              size = 1.5,
              color = "red") +
  geom_abline(intercept = (dist_model_anova_coeff[1] - 
                             dist_model_anova_coeff[1,2]), 
              slope = dist_model_anova_coeff[2],
              size = 1.5,
              color = "red")

