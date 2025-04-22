## trees_read_clean.R
## Alex Bowers


#### required packages ####
# core
library(dplyr)
library(tidyr)
library(stringr)
library(zoo) 
# mapping
library(sf)
library(geosphere)
library(rnaturalearth)
# dendrochronology
library(dplR)
# visualization
library(ggplot2)
# statistics
library(lme4)
library(car)
library(lqmm)



#### required data ####
# sampling data
trees <- read.csv("data/trees.csv", na.strings = "NA") 
# property location data
properties <- read.csv("data/properties.csv")
# transect start point
transect_starts <- read.csv("data/transect_starts.csv")
# j. ashei cover shapefile
# source: https://www.fs.usda.gov/database/feis/plants/tree/junash/all.html
jasheicover <- st_read("data/jasheicover.kml")
# rind width length data for slabs and cores
ringwidths_slabs <- read.rwl("data/ringcounts_slabs.raw")
ringwidths_cores <- read.rwl("data/ringcounts_cores.raw")
# ring width data for multistemmed individuals
ringwidths_ms <- read.rwl("data/ringcounts_multistemmed.raw")
# pdsi data from noaa
pdsi <- read.csv("data/dpsi.csv")



#### theme for plots ####
theme <- theme_bw() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 6),
    strip.background = element_rect(fill = NA, linewidth = 1),
    strip.text = element_text(size = 8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    panel.border = element_rect(linewidth = 1),
    text = element_text(family = "Times New Roman")
  )










################################################################
### creates map to display property locations across texas
###############################################################

# gets outlines of nearby states
us_states <- ne_states(country = "United States of America", returnclass = "sf")
mex_states <- ne_states(country = "Mexico", returnclass = "sf")

fig1 <- ggplot() +
  # us states
  geom_sf(data = us_states, fill = "white", color = "black", size = 0.2) +
  # mex states
  geom_sf(data = mex_states, fill = "white", color = "black", size = 0.2) +
  # cover
  geom_sf(data = jasheicover, aes(geometry = geometry), 
          fill = "#4EB265", color = "#4EB265") +
  # boudnign box
  coord_sf(xlim = c(-107, -91), ylim = c(25,37), expand = FALSE) +
  # adds property locations
  geom_point(data = properties, aes(long, lat), shape = 19, size = 2) +
  # theme
  labs(fill = "",
       x = expression("Longitude"),
       y = expression("Latitude")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = NA, linewidth = 1),
    text = element_text(family = "Times New Roman"),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.border = element_rect(linewidth = 1.5),
  )

fig1










################################################
### dendrochronology
##############################################

#### cleans trees for analysis ####
# merges trees and properties data by property_id
trees <- merge(trees, properties, by = "property_id")
# merges trees and transect starts by property_id and transect
trees <- merge(trees, transect_starts, by = c("property_id", "transect"))
# adds leading zeros
trees$individual <- stringr::str_pad(trees$individual, 2, pad = "0")
# creates sample id and transect id for analysis
trees$id <- paste(trees$property_code, trees$transect, 
                  trees$individual, sep="")
trees$transect_id <- paste(trees$property_code, trees$transect, sep="")




#### Cleans data #####
# blank objects stops code from running, so blank objects are removed
ringwidths_slabs <- select(ringwidths_slabs, -KEB07)
ringwidths_slabs <- select(ringwidths_slabs, -UVA02)
ringwidths_slabs <- select(ringwidths_slabs, -UVA04)
# corrects incorrect names
colnames(ringwidths_ms)[7] <- "UVB04_1"
colnames(ringwidths_slabs)[158] <- "KEB12"
colnames(ringwidths_slabs)[255] <- "UVB11"
# oldest stem
colnames(ringwidths_slabs)[192] <- "UVA02"
colnames(ringwidths_slabs)[192] <- "UVA02"

# creates data frame of inital ring counts
rw_sum_slabs <- summary(ringwidths_slabs)
# selects needed variables
rw_sum_slabs <- select(rw_sum_slabs, c("series", "year"))
colnames(rw_sum_slabs)[1] <- "id"
# cleans cores to match sample id
rw_sum_cores <- summary(ringwidths_cores)
# selects out individual ids
rw_sum_cores$core_id <- str_match(rw_sum_cores$series, "[A-Za-z]$")
rw_sum_cores$id <- str_match(rw_sum_cores$series, "^[A-Za-z]{3}[0-9]{2}")
# finds oldest core for each individual 
rw_sum_cores <- select(rw_sum_cores, c(id, year, core_id))
rw_sum_cores <- rw_sum_cores %>% group_by(, id) %>% summarize(max(year))
colnames(rw_sum_cores)[1] <- "id"
colnames(rw_sum_cores)[2] <- "year"

# makes an age data frame for use in age_structure
age <- rbind(rw_sum_slabs, rw_sum_cores)
# for use in crossdating
rwi_slabs <- ringwidths_slabs



#### detrends ####
ringwidths_slabs <- detrend(rwl = ringwidths_slabs, method = "ModNegExp")
ringwidths_cores <- detrend(rwl = ringwidths_cores, method = "ModNegExp")
ringwidths_ms <- detrend(rwl = ringwidths_ms, method = "ModNegExp")



#### more cleaning ####
# widens slabs
ringwidths_slabs$year <- as.numeric(rownames(ringwidths_slabs))
ringwidths_slabs <- pivot_longer(ringwidths_slabs, cols = -year,  
                                 names_to = "id", values_to = "ring_width")
ringwidths_slabs <- na.omit(ringwidths_slabs)

#widens cores
ringwidths_cores$year <- as.numeric(rownames(ringwidths_cores))
ringwidths_cores <- pivot_longer(ringwidths_cores, cols = -year,  
                                 names_to = "id", values_to = "ring_width")
# separates id from core id
ringwidths_cores <- na.omit(ringwidths_cores)
ringwidths_cores$core_id <- str_match(ringwidths_cores$id, "[A-Za-z]$")
ringwidths_cores$id <- str_match(ringwidths_cores$id, "^[A-Za-z]{3}[0-9]{2}")
# corrects column names
ringwidths_cores$id <- ringwidths_cores$id[,1]
ringwidths_cores$core_id <- ringwidths_cores$core_id[,1]
# seperates by core ID to make a column in trees_rw for A, B, and NA cores
ringwidths_cores_A <- ringwidths_cores[ringwidths_cores$core_id == "A",]
colnames(ringwidths_cores_A)[3] <- "ring_width_A"

ringwidths_cores_B <- ringwidths_cores[ringwidths_cores$core_id == "B",]
colnames(ringwidths_cores_B)[3] <- "ring_width_B"

ringwidths_cores_NA <- ringwidths_cores[is.na(ringwidths_cores$core_id),]
colnames(ringwidths_cores_NA)[3] <- "ring_width_NA"
## combines data with trees data drame from "trees_read_clean.R"
trees_rw <- left_join(trees, ringwidths_slabs, by = "id")
trees_rw <- left_join(trees_rw, ringwidths_cores_A, by = "id")
trees_rw <- left_join(trees_rw, ringwidths_cores_B, by = "id", 
                      relationship = "many-to-many")
trees_rw <- left_join(trees_rw, ringwidths_cores_NA, by = "id", 
                      relationship = "many-to-many")
# combines seperate series years into one column
trees_rw$year <- ifelse(is.na(trees_rw$year.x), 
                        trees_rw$year.y, trees_rw$year.x)
trees_rw$year <- ifelse(is.na(trees_rw$year), 
                        trees_rw$year.x.x, trees_rw$year)
trees_rw$year <- ifelse(is.na(trees_rw$year), 
                        trees_rw$year.y.y, trees_rw$year)
# combines seperate series ring widths into one column
trees_rw$ring_width <- ifelse(is.na(trees_rw$ring_width), 
                              trees_rw$ring_width_A, trees_rw$ring_width)
trees_rw$ring_width <- ifelse(is.na(trees_rw$ring_width), 
                              trees_rw$ring_width_B, trees_rw$ring_width)
trees_rw$ring_width <- ifelse(is.na(trees_rw$ring_width), 
                              trees_rw$ring_width_NA, trees_rw$ring_width)
# combines seperate series core_ids into one column
trees_rw$core_id <- ifelse(is.na(trees_rw$core_id), 
                           trees_rw$core_id.x, trees_rw$core_id)
trees_rw$core_id <- ifelse(is.na(trees_rw$core_id), 
                           trees_rw$core_id.y, trees_rw$core_id)
# removes extra columns 
colnames(trees_rw)
trees_rw <- select(trees_rw, -nearest_town, -long.y, -lat.y, -year.x, -year.y,
                   -ring_width_A, -core_id.x, -year.x.x, -ring_width_B, 
                   -core_id.y, -year.y.y, -ring_width_NA)
# removes any extra rows
trees_rw <- trees_rw[!is.na(trees_rw$year),]



#### summaries of tree ring data ####
count(unique(age[age$year >= 200,]))
summary(age$year)
summary(trees_rw$ring_width)



#### attempts at cross dating using dplR ####
# detrends using a conservative approach based on 
# Fritts HC (2001) Tree Rings and Climate. The Blackburn Press.
detrended_slabs <- detrend(rwl = rwi_slabs, method = "ModNegExp")

# Builds a Mean-Value Chronology
# uses uses Tukey’s biweight robust mean
chronology_slabs <- chron(detrended_slabs)

# plots chronology
plot(chronology_slabs, add.spline=TRUE, nyrs=30)



#### ring widths correlations ####
# summary stats for each year
year_corr <- function(year){
  year_rw <- trees_rw[trees_rw$year == year,]
  summary(year_rw$ring_width)
}
year_corr(2008)

# box plots showing no trends in ring width
trees_cor <- trees_rw[trees_rw$year >= 2000,]
ggplot(trees_cor, aes(factor(year), ring_width)) + 
  geom_boxplot()

# model for year vs ring width
rw_model <- lmer(ring_width ~ year +(1|property_id),  
                 data = trees_rw)

# individual property correlations
ke_trees_rw <- trees_rw[trees_rw$property_code == "UV",]
ke_trees_rw <- ke_trees_rw[ke_trees_rw$year > 2000,]
ggplot(ke_trees_rw, aes(factor(year), ring_width)) + geom_boxplot()




#### ring width comapred to Palmer Drought Severity Index ####
# 3 year rolling avg
trees_rw_rings <- distinct(trees_rw, id, year ,.keep_all = TRUE)
trees_rw_pdsi <- trees_rw_rings
trees_rw_pdsi$rw_avg <- rollapply(trees_rw_pdsi$ring_width, width = 3, 
                                  FUN = mean, fill = NA)
trees_rw_pdsi <- trees_rw_pdsi[trees_rw_pdsi$year > 2000,]

# merge data
trees_pdsi <- merge(pdsi, trees_rw_pdsi, by = "year")

# plot of pdsi vs ring width
fig4 <- ggplot(trees_pdsi, aes(value, rw_avg)) + 
  geom_jitter(width = 0.1) +  
  theme +
  labs(
    x = "Palmer Drought Severity Index",
    y = "Ring width index")

fig4

## insignifacnt model
pdsi_model <- lmer(rw_avg ~ value + (1 |transect_id) + (1 |id), 
                   data = trees_pdsi)

pdsi_model_anova <- Anova(pdsi_model)
# P = 0.
pdsi_model_coeff = summary(pdsi_model)$coefficients
pdsi_model_coeff[1]



#### Multistemmed data #### 
# cleans multistemmed data
ringwidths_ms$year <- as.numeric(rownames(ringwidths_ms))
ringwidths_ms <- pivot_longer(ringwidths_ms, cols = -year,  
                              names_to = "id", values_to = "ring_width")
ringwidths_slabs <- na.omit(ringwidths_ms)
# matches stems
ringwidths_ms$individual <- str_match(ringwidths_ms$id, "^[A-Za-z]{3}[0-9]{2}")
ringwidths_ms <- ringwidths_ms[ringwidths_ms$year >= 1970,]
ringwidths_ms_recent <- ringwidths_ms[ringwidths_ms$year >= 2000,]

fig5 <- ggplot(ringwidths_ms_recent, aes(year, ring_width)) + 
  geom_line(size = 0.5) + 
  facet_wrap(~id, ncol = 1, switch = "y") +
  scale_x_continuous(breaks= seq(2000, 2022, 2)) +
  scale_y_continuous(breaks= c(1,3,5)) +
  theme +
  labs(
    x = "Year",
    y = "Ring width index") +
  # remove axis lables
  theme(strip.text = element_blank()) +
  # label each pannel
  geom_text(data = ringwidths_ms_recent %>% group_by(id) %>% slice(1),
            aes(x = min(ringwidths_ms_recent$year), 
                y = max(ringwidths_ms_recent$ring_width), 
                label = id),
            hjust = 0, vjust = 1.5, size = 4)

fig5




#### appendix plots
rw_series <- function(transect) {
  # selects needed data
  trees_rw_transect <- trees_rw[trees_rw$transect_id == transect,]
  trees_rw_transect <- trees_rw_transect[trees_rw_transect$year >= 1970,]
  trees_rw_transect <- trees_rw_transect[trees_rw_transect$year < 2023,]
  # plots
  ggplot(trees_rw_transect, aes(year, ring_width)) + 
    geom_line(size = 0.25) + 
    facet_wrap(~id, ncol = 1, switch = "y") +
    scale_x_continuous(breaks= seq(1970, 2020, 5)) +
    scale_y_continuous(breaks= c(0,5)) +
    theme_bw() +
    theme(
      strip.text.y.left = element_text(angle = 0),
      strip.background = element_rect(fill = NA, linewidth = 0.5),
      strip.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      text = element_text(family = "Times New Roman")
    ) +
    labs(
      x = "Year",
      y = "Ring width index")
}

A1 <- rw_series("BAA")
A2 <- rw_series("BUA")
A3 <- rw_series("EDB")
A4 <- rw_series("HTA")
A5 <- rw_series("HTB")
A6 <- rw_series("KEA")
A7 <- rw_series("KEB")
A8 <- rw_series("KEC")
A9 <- rw_series("MEA")
A10 <- rw_series("MEB")
A11 <- rw_series("UVA")
A12 <- rw_series("UVB")










#########################################
#### age structure 
############################################

# merge field data with dendrochronology
trees_age <- dplyr::left_join(trees, age, by = "id")

# calculates distance from transect start to the point sampled
samplingpoint <- data.frame(long=trees_age$long.x, lat=trees_age$lat.x)
transectpoint <- data.frame(long=trees_age$transect_long, 
                            lat=trees_age$transect_lat)
trees_age$distance <- distHaversine(samplingpoint, transectpoint)

#### plot of ring width summaries ####
# ring count freq
fig3 <- ggplot(trees_age, aes(year)) + 
  geom_histogram(binwidth = 10, fill = "black", color = "white") + 
  labs(
    x = "Ring count",
    y = "Number of individuals"
  ) + theme +
  scale_x_continuous(breaks= seq(0, 300, 50)) +
  scale_y_continuous(breaks= seq(0, 30, 5))

fig3

# ring width freq
trees_rw_rings <- distinct(trees_rw, id, year ,.keep_all = TRUE)
summary(trees_rw_rings)

fig2 <- ggplot(trees_rw_rings, aes(x = ring_width)) + 
  geom_histogram(binwidth = 0.1, fill ="black") + 
  labs(
    x = "Ring width index",
    y = "Frequeny of rings"
  ) + xlim(0,14) +
  theme +
  scale_y_continuous(breaks= seq(0, 2500, 500))

fig2

# linear quantile regression mixed effect model of distannce and age
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
# Linear Mixed effect model
# removes transects that were discarded 
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
fig6 <- ggplot(trees_age_transects, aes(distance, year)) + 
  geom_point(size=2, alpha =0.8) + 
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

fig6
