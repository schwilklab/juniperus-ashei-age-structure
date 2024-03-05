## age_structure.R
## Alex Bowers

### calculates distance and elevation from transect start 

library(geosphere)
library(ggplot2)
library(elevatr) 
library(dplyr)
library(car)
library(lqmm)


KEA09 <- filter(age, id %in% c("KEA09top", "KEA09bot"))
KEA09 <- tibble(id = "KEA09", year = sum(KEA09$year))

KEB07 <- filter(age, id %in% c("KEB07out", "KEB07in"))
KEB07 <- tibble(id = "KEB07", year = sum(KEB07$year))

age <- rbind(age, KEA09, KEB07)

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
  elev <- get_elev_point(locations = sample_pts, prj = 4326, src="epqs")
  return(elev[[3]])
}

# finds different in elevation between transect start and sampling point
trees_age$sample_elev <- get_cords(trees_age$long.x, trees_age$lat.x, trees_age$id)
trees_age$transect_elev <- get_cords(trees_age$transect_long, 
                                     trees_age$transect_lat, trees_age$id)
trees_age$elev_dif <- abs(trees_age$sample_elev - trees_age$transect_elev)



# oldest tree in the east
trees_age_HT <- trees_age[trees_age$property_code == "HT",]
max(trees_age_HT$year, na.rm = TRUE)
## plots

# theme for plots
theme <- theme_bw() +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.background = element_rect(fill = NA, linewidth = 1),
    strip.text = element_text(size = 16),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 18),
    panel.border = element_rect(linewidth = 2),
    text = element_text(family = "Times New Roman")
  )


# ring count freq
ggplot(trees_age, aes(year)) + 
  geom_histogram(binwidth = 10, fill = "black", color = "white") + 
  labs(
    x = "Ring count",
    y = "Number of individuals"
  ) + theme +
  scale_x_continuous(breaks= seq(0, 300, 50)) +
  scale_y_continuous(breaks= seq(0, 30, 5))


# ring width freq
trees_rw_rings <- distinct(trees_rw, id, year ,.keep_all = TRUE)
summary(trees_rw_rings)

ggplot(trees_rw_rings, aes(x = ring_width)) + 
  geom_histogram(binwidth = 0.1, fill ="black") + 
  labs(
    x = "Ring width index (mm)",
    y = "Number of rings"
  ) + theme +
  scale_x_continuous(breaks= seq(0, 14, 2)) +
  scale_y_continuous(breaks= seq(0, 2500, 500))




## quantile regression mixed effect model:
trees_age_qrmem <- trees_age[!is.na(trees_age$year),]
model <- lqmm(year ~ distance,
              random = ~1,
              group = transect_id,
              tau = 0.95,
              data = trees_age_qrmem)
summary(model)

###  linear mixed effect model 
# distance
dist_model <- lmer(year ~ distance + (1 |transect_id) + (1 |property_id), 
                   data = trees_age)

dist_model_anova = Anova(dist_model, type = 2, test.statistic = "F")
# P = 0.9284, not significant

dist_model_anova_coeff = summary(dist_model)$coefficients
dist_model_anova_coeff[1]

###plots
# Linear Mixed effect mdel

# removes small and incomplete transect
trees_age_transects <- trees_age[!trees_age$transect_id == "BAB",]
trees_age_transects <- trees_age_transects[!trees_age_transects$transect_id == "EDA",]

# orders transects west to east
location_order <- c("EDB", "KEA", "BUA", "UVA", "KEB", "HTA", 
                  "UVB", "KEC", "HTB", "BAA", "MEA", "MEB")
trees_age_transects <- trees_age_transects %>%
  mutate(transect_id = factor(transect_id, levels = location_order)) %>%
  arrange(transect_id)


# creates label names 
new_trans_ids <- c("Edwards", "Kerr-A", "Burnet", "Uvalde-A", "Kerr-B",
                   "Hays-Travis-A", "Uvalde-B", "Kerr-C", "Hays-Travis-B",
                   "Bandera", "Medina-A", "Medina-B")
names(new_trans_ids) <- c("EDB", "KEA", "BUA", "UVA", "KEB", "HTA", 
                          "UVB", "KEC", "HTB", "BAA", "MEA", "MEB")

# plot
ggplot(trees_age_transects, aes(distance, year)) + 
  geom_point(size=4, alpha =0.8) + 
  facet_wrap(~ transect_id, 
             nrow = 4, ncol = 3,
             labeller = labeller(transect_id = new_trans_ids)) +
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) +
  theme +
  theme(legend.position = "none") +
  scale_y_continuous(breaks= seq(0, 250, 50)) 



## plots with fire refugia

trees_age_transects_sub <- subset(trees_age_transects, transect_id %in% 
                                     c("MEA", "KEC", "MEB", "BAA", "UVB"))

ggplot(trees_age_transects_sub, aes(distance, year)) + 
  geom_point(size=4, alpha =0.8) + 
  facet_wrap(~ transect_id, 
             nrow = 3, ncol = 2,
             labeller = labeller(transect_id = new_trans_ids)) +
  labs(
    x = "Distance from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) +
  theme +
  theme(legend.position = "none") +
  scale_y_continuous(breaks= seq(0, 250, 50)) 







###### extra plots


# color scheme
colorscheme <- c(
  "Bandera_2022-35" = "#AA6F9E",
  "Burnet_2022-16" = "#1965B0",
  "Edwards_2022-25" = "#7BAFDE",
  "Hays-Travis_2022-02" = "#4EB265",
  "Kerr_2021-36" = "#F7F056",
  "Medina_2022-04" = "#EE8026",
  "Uvalde_2021-03" = "#DC050C"
)

## all together
ggplot(trees_age, aes(distance, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Elevation difference from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) +
  theme +
  theme(legend.position = c(0.9, 0.85)) +
  scale_color_manual(values = colorscheme) +
  scale_y_continuous(breaks= seq(0, 300, 50))

unique(trees_age$transect_id)
count(trees_age[trees_age$transect_id == "UVB",])

# taking out farthest points
elev_model<- lmer(year ~ elev_dif + (1 |property_code) + (1 |transect_id), 
                   data = trees_age)

elev_model_anova = Anova(elev_model, type = 2, test.statistic = "F")
# P = 0.9284, not significant

elev_model_anova_coeff = summary(elev_model)$coefficients

ggplot(trees_age_close, aes(elev_dif, year, color=property_id)) + 
  geom_point(size=4) + 
  labs(
    x = "Elevation difference from start of transect (m)",
    y = "Ring count",
    color = "Property"
  ) +
  theme +
  theme(legend.position = c(0.9, 0.85)) +
  scale_color_manual(values = colorscheme) +
  scale_y_continuous(breaks= seq(0, 300, 50))
