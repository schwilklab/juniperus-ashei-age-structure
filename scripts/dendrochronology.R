## dendrochronology.R
## Alex Bowers

### tutorial of dplR
### https://opendendro.github.io/dplR-workshop/introduction.html

library(dplR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)

# coverts raw file into readable rind width length
ringwidths_slabs <- read.rwl("data/ringcounts_slabs.raw")
ringwidths_cores <- read.rwl("data/ringcounts_cores.raw")

# blank objects stops code from running, so blank objects are removed
# data will be edited later
ringwidths_slabs$KEB07 
ringwidths_slabs$UVA02
ringwidths_slabs <- select(ringwidths_slabs, -KEB07)
ringwidths_slabs <- select(ringwidths_slabs, -UVA02)
ringwidths_slabs <- select(ringwidths_slabs, -UVA04)


## creates data frame of inital ring counts
rw_sum_slabs <- summary(ringwidths_slabs)
rw_sum_cores <- summary(ringwidths_cores)
age <- select(rw_sum_slabs, c("series", "year"))
colnames(age)[1] <- "id"

# for use in dplR package
rwi_slabs <- ringwidths_slabs




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
### note I know this is a bit inefficient but its what I got.



############# plots for data visualization ############# 
# separated by site to find trends
test <- trees_rw[trees_rw$transect_id == "UVA",]
ggplot(test, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y")
  
 # plot all ring counts for slabs
test2 <- ringwidths_slabs[!is.na(ringwidths_slabs$ring_width),]
test2 <- test2[grep("KEC", test2$id),]
ggplot(test2, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y") +
  scale_x_continuous(breaks = seq(min(test2$year), 
                                  max(test2$year), by = 3))

# views recent years
test3 <- trees_rw[trees_rw$year > 1970,]
test3 <- test3[test3$transect_id == "UVB",]
ggplot(test3, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y") +
  scale_x_continuous(breaks = seq(min(test3$year), 
                                  max(test3$year), by = 1))



############# precipation vs ring widths ############# 
# finds sd and mean
rw_sd <- sd(trees_rw$ring_width, na.rm = TRUE)
rw_mean <- mean(trees_rw$ring_width, na.rm = TRUE)

# finds big rings
outliers <- trees_rw[trees_rw$ring_width > rw_mean+(rw_sd*6),]
# high growth years 
big_rings <- sort(unique(outliers$year))
# note 2 sds from the mean is negative


# finding the smallest rings
# rings smaller than 0.01 mm
smallest <- trees_rw[trees_rw$ring_width < 0.04,]
# years with smallest rings
small_rings <- sort(unique(smallest$year))


## drought check
rainfall <- read.csv("data/USGS_kerrville_rainfall.csv")

# finds years less that 1.5 sd away from the mean
rainfall_low <- mean(rainfall$TOTAL) - (sd(rainfall$TOTAL)*1.5)
droughts <- rainfall[rainfall$TOTAL < rainfall_low,]
drought_years <- droughts$YEAR
# finds rainy seasons
rainfall_high <- mean(rainfall$TOTAL) + (sd(rainfall$TOTAL)*1.5)
rainy <- rainfall[rainfall$TOTAL > rainfall_high,]
rainy_years <- rainy$YEAR

# do droughts and rainy seasons match up
small_rings
drought_years

big_rings
rainy_years
# nope



############# ring widths correlations ############# 

ggplot(trees_rw, aes(year, ring_width)) + geom_point()
# no trend

## summary stats for each year
year_corr <- function(year){
  year_rw <- trees_rw[trees_rw$year == year,]
  summary(year_rw$ring_width)
}
year_corr(2022)

# boxplots showing no trends
trees_cor <- trees_rw[trees_rw$year > 2000,]
ggplot(trees_cor, aes(factor(year), ring_width)) + geom_boxplot()







### detrending using dplR
## detrends

# detrends using a conservative approach based on 
# Fritts HC (2001) Tree Rings and Climate. The Blackburn Press.
detrended_slabs <- detrend(rwl = rwi_slabs, method = "ModNegExp")

# Builds a Mean-Value Chronology
# uses uses Tukey’s biweight robust mean
chronology_slabs <- chron(detrended_slabs)

# plots chronology
plot(chronology_slabs, add.spline=TRUE, nyrs=30)



## individual series correlation
# seperates by site
ba <- ringwidths_slabs[grep("^BA", names(rwi_slabs))]
bu <- ringwidths_slabs[grep("^BU", names(rwi_slabs))]
ed <- ringwidths_slabs[grep("^ED", names(rwi_slabs))]
ht <- ringwidths_slabs[grep("^HT", names(rwi_slabs))]
ke <- ringwidths_slabs[grep("^KE", names(rwi_slabs))]
me <- ringwidths_slabs[grep("^ME", names(rwi_slabs))]
uv <- ringwidths_slabs[grep("^UV", names(rwi_slabs))]

# Blue correlates well 
# (p-values less or equal to the user-set critical value) 
# Red is potential dating problems 
# (p-values greater than the user-set critical value). 
# Green lines show segments that do not completely overlap the time period
# no correlations calculated. 
rwl <- corr.rwl.seg(ba, seg.length=10, pcrit=0.05)



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






### crossdating using lmer
head(trees_rw)

trees_lmer <- trees_rw[!(is.na(trees_rw$ring_width)),]
trees_lmer <- lmer(ring_width ~ exp(-year) + (1|property_id) + (1|id),
                   data = trees_lmer)
plot(trees_lmer)

trees_lmer <- trees_rw[!(is.na(trees_rw$ring_width)),]
trees_lmer <- lmer(ring_width ~ year + dbh + (1|property_id) +(1|id),
                   data = trees_lmer)
plot(trees_lmer)


