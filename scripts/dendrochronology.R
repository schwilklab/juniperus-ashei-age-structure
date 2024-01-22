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
rw_sum <- summary(ringwidths_slabs)
age <- select(rw_sum, c("series", "year"))
colnames(age)[1] <- "id"

# for use in dplR package
rwi_slabs <- ringwidths_slabs

## cleans data
# widens slabs
ringwidths_slabs$year <- as.numeric(rownames(ringwidths_slabs))
ringwidths_slabs <- pivot_longer(ringwidths_slabs, cols = -year,  
                                 names_to = "id", values_to = "ring_width")


#### note needs to be fixed below
#widens cores
ringwidths_cores$year <- as.numeric(rownames(ringwidths_cores))
ringwidths_cores <- pivot_longer(ringwidths_cores, cols = -year,  
                                 names_to = "id", values_to = "ring_width")
# separates id from core id
ringwidths_cores <- na.omit(ringwidths_cores)
pivot_wider(ringwidths_cores, names_from = core, values_from = ring_width)
# adds core id as a column 
ringwidths_cores <- na.omit(ringwidths_cores)
### note: I have no idea why this wont work :(


## combines data with trees data drame from "trees_read_clean.R"
trees_rw <- merge(ringwidths_slabs, trees, by.x = "id", by.y = "id")

# note: wont work until data is wider.
# produces 17 million rows?
trees_rw <- merge(ringwidths_cores, trees_rw, by.x = "id", by.y = "id")


### plots for data visualization

# separated by site to find trends
test <- trees_rw[trees_rw$transect_id == "UVA",]

ggplot(test, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y")

 # plot all ring couts for slabs
test2 <- ringwidths_slabs[!is.na(ringwidths_slabs$ring_width),]
test2 <- test2[grep("UVA02", test2$id),]
ggplot(test2, aes(year, ring_width)) + geom_line() + 
  facet_wrap(~id, ncol = 1, switch = "y")



### detredning using dplR
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


