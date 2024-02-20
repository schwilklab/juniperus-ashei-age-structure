## dendrochronology.R
## Alex Bowers

### tutorial of dplR
### https://opendendro.github.io/dplR-workshop/introduction.html

library(dplR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(stringr)
library(openmeteo)
library(car)

# coverts raw file into readable rind width length
ringwidths_slabs <- read.rwl("data/ringcounts_slabs.raw")
ringwidths_cores <- read.rwl("data/ringcounts_cores.raw")
# loads in multistemmed data
ringwidths_ms <- read.rwl("data/ringcounts_multistemmed.raw")

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

## creates data frame of inital ring counts
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

# for use in dplR package
rwi_slabs <- ringwidths_slabs

## detrends data based on Fritz 2001
ringwidths_slabs <- detrend(rwl = ringwidths_slabs, method = "ModNegExp")
ringwidths_cores <- detrend(rwl = ringwidths_cores, method = "ModNegExp")
ringwidths_ms <- detrend(rwl = ringwidths_ms, method = "ModNegExp")


############# cleans data ############
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
### note I know this is a bit inefficient but its what I have



## summaries for paper
count(age[age$year >= 200,])
summary(age$year)
summary(trees_rw$ring_width)




# cleans multistemmed data
ringwidths_ms$year <- as.numeric(rownames(ringwidths_ms))
ringwidths_ms <- pivot_longer(ringwidths_ms, cols = -year,  
                                 names_to = "id", values_to = "ring_width")
ringwidths_slabs <- na.omit(ringwidths_ms)



############# plots for data visualization ############# 
# separated by site to find trends
transect_rw <- trees_rw[trees_rw$transect_id == "KEA",]
ggplot(transect_rw, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y")
  
 # plot all ring counts for slabs
id_rw <- ringwidths_slabs[!is.na(ringwidths_slabs$ring_width),]
id_rw <- id_rw[grep("UVA02", id_rw$id),]
ggplot(id_rw, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y") +
  scale_x_continuous(breaks = seq(min(id_rw$year), 
                                  max(id_rw$year), by = 3))

# views recent years
recnt_rw <- trees_rw[trees_rw$year > 1990,]
recnt_rw <- recnt_rw[recnt_rw$transect_id == "KEC",]
ggplot(recnt_rw, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y") +
  scale_x_continuous(breaks = seq(min(recnt_rw$year), 
                                  max(recnt_rw$year), by = 1))



############# precipation vs ring widths ############# 
# function that takes lattitude and longitude and returns annual precipitation
# in mm since 1940
get_annual_precip <- function(lat, long, property_code) {
  location <- c(lat, long)
  ### finds precipitation data
  precip_data <- weather_history(location, 
                                 start = "1940-01-01", end = "2023-12-31",
                                 hourly = "precipitation")
  # takes only the year
  precip_data$datetime <- substr(precip_data$datetime, 1,4)
  # sums years
  precip_data <- precip_data %>% group_by(datetime) %>% 
    summarise(sum(hourly_precipitation)) 
  colnames(precip_data)[2] <- "precip"
  
  ### finds lowest and highest precipitation years
  precip_data <- precip_data[!is.na(precip_data$precip),]
  mean_precip <- mean(precip_data$precip)
  sd_precip <- sd(precip_data$precip)
  rainy_years <- precip_data[precip_data$precip > (mean_precip + (1.5 *sd_precip)),]
  drought_years <- precip_data[precip_data$precip < (mean_precip - (1 *sd_precip)),]
  
  ### finds largest and smallest rings for the property
  trees_rw_prop <- trees_rw[trees_rw$property_code == property_code,] 
  # finds sd and mean
  rw_sd <- sd(trees_rw_prop$ring_width, na.rm = TRUE)
  rw_mean <- mean(trees_rw_prop$ring_width, na.rm = TRUE)
  
  # finds big rings
  outliers <- trees_rw_prop[trees_rw_prop$ring_width > rw_mean+(rw_sd*3.5),]
  wide_rings <- sort(unique(outliers$year))
  
  # finding the smallest rings
  # rings smaller than 0.05 mm
  smallest <- trees_rw_prop[trees_rw_prop$ring_width <= 0.05,]
  # years with smallest rings
  small_rings <- sort(unique(smallest$year))
  
  ### makes list to display data 
  return(list(rainy_years = rainy_years$datetime, wide_rings = wide_rings, 
       drought_years = drought_years$datetime, small_rings = small_rings))
}

# gets annual precip for every site
ba_precip <- get_annual_precip(properties$lat[1], properties$long[1], "BA")
bu_precip <- get_annual_precip(properties$lat[2], properties$long[2], "BU")
ed_precip <- get_annual_precip(properties$lat[3], properties$long[3], "ED")
ht_precip <- get_annual_precip(properties$lat[4], properties$long[4], "HT")
ke_precip <- get_annual_precip(properties$lat[5], properties$long[5], "KE")
me_precip <- get_annual_precip(properties$lat[6], properties$long[6], "ME")
uv_precip <- get_annual_precip(properties$lat[7], properties$long[7], "UV")


# graph to visualize above results
ring_size_test <- trees_rw[trees_rw$property_code == "UV",]
ring_size_test <- ring_size_test[ring_size_test$year > 1940,]
ggplot(ring_size_test, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y") +
  scale_x_continuous(breaks = seq(min(1940), 
                                  max(2023), by = 5))



############# ring widths correlations ############# 

## summary stats for each year
year_corr <- function(year){
  year_rw <- trees_rw[trees_rw$year == year,]
  summary(year_rw$ring_width)
}
year_corr(2008)

# boxplots showing no trends
trees_cor <- trees_rw[trees_rw$year >= 2000,]
ggplot(trees_cor, aes(factor(year), ring_width)) + 
  geom_boxplot()

# model for year vs ring width
rw_model <- lmer(ring_width ~ year +(1|property_id),  
                   data = trees_rw)

rw_model_anova = Anova(rw_model)
rw_model_anova_coeff = summary(rw_model)$coefficients

# correaltions of ring width
ggplot(trees_rw, aes(year, ring_width)) + 
  geom_point() +
  geom_abline(intercept = rw_model_anova_coeff[[1]], 
              slope = rw_model_anova_coeff[[2]],
              linewidth = 1.5,
              color = "black")


trees_cor <- trees_rw[trees_rw$property_code == "UV",]
ggplot(trees_cor, aes(factor(id), ring_width)) + 
  geom_boxplot()


# indiviual property correlations
ke_trees_rw <- trees_rw[trees_rw$property_code == "UV",]
ke_trees_rw <- ke_trees_rw[ke_trees_rw$year > 2000,]
ggplot(ke_trees_rw, aes(factor(year), ring_width)) + geom_boxplot()



############# same individual ring widths correlations ############# 
ringwidths_ms$individual <- str_match(ringwidths_ms$id, "^[A-Za-z]{3}[0-9]{2}")

colorscheme_ms <- c(
  "HTA28" = "#4477AA",
  "HTB06" = "#66CCEE",
  "KEA03" = "#228833",
  "UVA02" = "#CCBB44",
  "UVB04" = "#EE6677",
  "UVB16" = "#AA3377"
)
labels_ms <- unique(ringwidths_ms$id)
names(labels_ms) <- c("1", "2", "1", "2", "1", "2", "1", "2", "3", "4", "1", "2", 
           "1", "2")

ringwidths_ms_recent <- ringwidths_ms[ringwidths_ms$year >= 2000,]

# labels for facet wrap
new_ids <- c("1", "2", "1", "2", "3", "4", "1", "2", "1", "2", "1", "2", "1", "2")
names(new_ids) <- unique(ringwidths_ms$id)


ggplot(ringwidths_ms_recent, aes(year, ring_width, color = individual )) + 
  geom_line(size = 1) + 
  facet_wrap(~id, ncol = 1, switch = "y", labeller = labeller(id = new_ids)) +
  scale_color_manual(values = colorscheme_ms) +
  scale_x_continuous(breaks= seq(2000, 2022, 2)) +
  scale_y_continuous(breaks= c(1,3,5)) +
  theme +
  labs(
    x = "Year",
    y = "Ring width index",
    color = "Indiviual")


############# cross-dating using dplR############# 
## detrends

# detrends using a conservative approach based on 
# Fritts HC (2001) Tree Rings and Climate. The Blackburn Press.
detrended_slabs <- detrend(rwl = rwi_slabs, method = "ModNegExp")

# Builds a Mean-Value Chronology
# uses uses Tukey’s biweight robust mean
chronology_slabs <- chron(detrended_slabs)

# plots chronology
plot(chronology_slabs, add.spline=TRUE, nyrs=30)



# Blue correlates well 
# (p-values less or equal to the user-set critical value) 
# Red is potential dating problems 
# (p-values greater than the user-set critical value). 
# Green lines show segments that do not completely overlap the time period
# no correlations calculated. 

# removes extra series
rwi_slabs <- select(rwi_slabs, -KEB07out, -KEB07in, -KEA09top, -KEA09bot,
                           -unknown, -UVA02_1, -UVA02_3, -UVB11A, -KEB12)
# correlation plot
rwl <- corr.rwl.seg(rwi_slabs, seg.length=10, pcrit=0.05)



# correlation for individual series to whole
cor_KEC01 <- corr.series.seg(rwl=ke, series="KEC01",
                          seg.length=10)




# each series is assessed to see if its inclusion in the chronology improves
# the EPS (Expressed Population Signal)
# EPS is the cross correlation matrix of all series. 
# 1 represnts a perfect chronology, ideally above 0.85
# based on https://repository.arizona.edu/bitstream/handle/10150/262571/trr-59-02-053-062.pdf?sequence=1&isAllowed=y
# # Fowler A, Boswijk G (2003) Chronology stripping as a tool for enhancing the statistical quality of tree-ring chronologies. Tree-Ring Research, 59, 53–62.
### warning: takes a long while to run
stripped_rws <- strip.rwl(ringwidths_slabs, ids = rwc_ids)

# keep removing or editing these sequences until no more iterations improve EPS

# intial run started at 0.369 and improves to 0.773, after 82 iterations
# improved >.01
# KEA05, EDA15, MEB11, BUA11, EDB10, HTA26, BAA10, EDA06, UVA07, KEB11, KEC01,
# KEB06, KEA28, HTA32, MEB16, EDA14
# improves >0.05
# KEA09top, UVA02_1, HTA16, HTB13, UVA14, HTB18, HTB05, KEA07, UVA05, UVA08, 
# BUA12, HTB08, KEB14,
# <0.05 improvements
# MEB10, EDB11, UVA02_3,
# KEA03, KEC16, KEC23, unknown, UVA12, UVA11, KEB01, UVB04, HTA21, HTA24, HTB04,
# HTB01, HTB09, UVB09, BUA05, BUA07, KEC12, BAA11, UVB08, HTA29, MEB15, KEB09,
# MEB14, UVA02_2, KEB04, MEB19, UVB12, HTB12, KEA27, UVB05, UVB19, KEC17, UVB13,
# KEC10, UVB11A, BAA27, HTB06, UVB20, EDB19, BUA01, EDB05, KEB10, HTB16, HTA 27,
#
# runs were stopped short as it ran 16 hours on Alex's mac.
# these errors will be assed then EPS will be reran again







# methods from https://www.sciencedirect.com/science/article/pii/S0048969723068778
# "Each tree-ring series was statistically verified for cross-dating and
# measurement errors using the Gleichläufgkeit (“glk”) function and 
# Spearman's rho correlations of the package Dendrochronology Program Library
# in R (“dplR”) (Bunn, 2010; R Core Team, 2020). The tree-ring series that
# cross-dated poorly with the master chronology were corrected for measurement 
# errors when possible or discarded."
glk_slabs <- glk(ringwidths_slabs, overlap = 50, prob = TRUE)
glk_slabs$glk_mat

## note still unsure about this one






### detrend using lmer
trees_lmer <- lmer(ring_width ~ exp(-year) + (1|property_id) + (1|id),
                   data = trees_rw)

Anova(trees_lmer)
summary(trees_lmer)
plot(trees_lmer)
## unsure where to go from here

