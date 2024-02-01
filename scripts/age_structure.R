## age_structure.R
## Alex Bowers

### calculates distance and elevation from transect start 

library(geosphere)
library(ggplot2)
library(elevatr) 
library(car)

# merge field data with ring counts
trees_age <- dplyr::left_join(trees, age, by = "id")

## distance from transect start

# calculates distance from transect start to the point sampled
samplingpoint <- data.frame(long=trees_age$long.x, lat=trees_age$lat.x)
transectpoint <- data.frame(long=trees_age$transect_long, 
                            lat=trees_age$transect_lat)
trees_age$distance <- distHaversine(samplingpoint, transectpoint)


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
trees_age$sample_elev <- get_cords(trees_age$long.x, trees_age$lat.x, trees_age$id)
trees_age$transect_elev <- get_cords(trees_age$transect_long, 
                                     trees_age$transect_lat, trees_age$id)
trees_age$elev_dif <- trees_age$sample_elev - trees_age$transect_elev



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
ggplot(trees_age, aes(distance, year, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id) +
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring Count",
    color = "Transect"
  )

# dbh vs distance 
ggplot(trees_age, aes(distance, dbh, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id) +
  labs(
    x = "Distance from start of transect (m)",
    y = "DBH",
    color = "Transect"
  )

# age vs distance
ggplot(trees_age, aes(distance, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) + theme

 # dbh vs distance
ggplot(trees_age, aes(distance, dbh, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Diameter at breast height (cm)",
    color = "Property"
  ) + theme

# age vs elevation
ggplot(trees_age, aes(elev_dif, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) + theme +
  theme(legend.position = c(0.85, 0.75))

# age vs elevatio
ggplot(trees_age, aes(elev_dif, year, color=transect_id)) + 
  geom_point() +
  facet_wrap(~transect_id) +
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring Count",
    color = "Transect"
  )

# ring count frequ
ggplot(trees_age, aes(year)) + 
  geom_histogram(binwidth = 10) + 
  labs(
    x = "Ring Count",
    y = "Frequency"
  ) + theme

oldest_trees <- trees_age[!is.na(trees_age),]
oldest_trees <- oldest_trees[oldest_trees$year >= 200,]
oldest_trees <- oldest_trees[!is.na(oldest_trees$id),]
max(oldest_trees$year)
### linear regression btheme### linear regression between ring count and dbh
linear_reg_dbh_year <- lm(dbh ~ year, data = trees_age)
linear_reg_dbh_year_mod <- linear_reg_dbh_year[[1]]

summary(linear_reg_dbh_year)
# r^2 is 0.45085

ggplot(big_trees_age, aes(dbh, year)) +
  geom_point(size=4) + 
  labs(
    x = "Diameter at breast height (cm)",
    y = "Ring count"
  ) + 
  geom_abline(intercept = linear_reg_dbh_year_mod[1],
              slope =linear_reg_dbh_year_mod[2]) +
  theme 



###  linear mixed effect model 
# distance
dist_model <- lmer(year ~ distance + (1 |property_code) + (1 |transect_id), 
                   data = trees_age)

dist_model_anova = Anova(dist_model, type = 2, test.statistic = "F")
# P = 0.9284, not significant

dist_model_anova_coeff = summary(dist_model)$coefficients
dist_model_anova_coeff[1]

###plots
# Linear Mixed effect mdel
ggplot(trees_age, aes(distance, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) +
  theme(legend.position = c(0.9, 0.9)) +
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



## for elvation
elev_model <- lmer(year ~ elev_dif + (1 |property_code) + (1 |transect_id) +(1|dbh), 
                   data = trees_age)

elev_model_anova = Anova(elev_model, type = 2, test.statistic = "F")
# P = 0.9284, not significant

elev_model_anova_coeff = summary(elev_model)$coefficients
elev_model_anova_coeff[1]

###plots
# Linear Mixed effect mdel
ggplot(trees_age, aes(elev_dif, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Elevation from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) +
  # Estiamte
  geom_abline(intercept = elev_model_anova_coeff[1], 
              slope = elev_model_anova_coeff[2],
              size = 1.5,
              color = "black") +
  # standard error
  geom_abline(intercept = (elev_model_anova_coeff[1] + 
                             elev_model_anova_coeff[1,2]), 
              slope = elev_model_anova_coeff[2],
              size = 1.5,
              color = "red") +
  geom_abline(intercept = (elev_model_anova_coeff[1] - 
                             elev_model_anova_coeff[1,2]), 
              slope = elev_model_anova_coeff[2],
              size = 1.5,
              color = "red")

