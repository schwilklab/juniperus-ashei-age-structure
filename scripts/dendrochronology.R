## dendrochronology.R
## Alex Bowers

### tutorial of dplR
### https://opendendro.github.io/dplR-workshop/introduction.html

library(dplR)
library(dplyr)
library(ggplot2)


# coverts raw file into readable rind width length
ringwidths_slabs <- read.rwl("data/ringcounts_slabs.raw")
ringwidths_cores <- read.rwl("data/ringcounts_cores.raw")

# blank objects stops code from running, so blank objects are removed
ringwidths_slabs$KEB07 
ringwidths_slabs$UVA02
ringwidths_slabs <- select(ringwidths_slabs, -KEB07)
ringwidths_slabs <- select(ringwidths_slabs, -UVA02)
ringwidths_slabs <- select(ringwidths_slabs, -UVA04)


## adds how old inividuals are to trees data
rw_sum <- summary(ringwidths_slabs)
ages <- select(rw_sum, c("series", "year"))
colnames(ages)[1] <- "id"

### option 1 for chronology
## detrends

# detrends using a conservative approach based on 
# Fritts HC (2001) Tree Rings and Climate. The Blackburn Press.
detrended_slabs <- detrend(rwl = ringwidths_slabs, method = "ModNegExp")

# Builds a Mean-Value Chronology
# uses uses Tukey’s biweight robust mean
chronology_slabs <- chron(detrended_slabs)

# selcts only values with at least 5 samples
chronology_slabs <- subset(chronology_slabs, samp.depth > 4)


# reruns detrending and creates chronology with values with at least 5 samples
chronchronology_slabs <- chron(
  detrend(ringwidths_slabs[chronology_slabs$samp.depth > 4,], 
          method="ModNegExp"))

# plots chronology
plot(chronology_slabs, add.spline=TRUE, nyrs=30)

## note: not sure where to go from here


## individual series correlation
# seperates by site
ba <- ringwidths_slabs[grep("^BA", names(ringwidths_slabs))]
bu <- ringwidths_slabs[grep("^BU", names(ringwidths_slabs))]
ed <- ringwidths_slabs[grep("^ED", names(ringwidths_slabs))]
ht <- ringwidths_slabs[grep("^HT", names(ringwidths_slabs))]
ke <- ringwidths_slabs[grep("^KE", names(ringwidths_slabs))]
me <- ringwidths_slabs[grep("^ME", names(ringwidths_slabs))]
uv <- ringwidths_slabs[grep("^UV", names(ringwidths_slabs))]

# Blue correlates well 
# (p-values less or equal to the user-set critical value) 
# Red is potential dating problems 
# (p-values greater than the user-set critical value). 
# Green lines show segments that do not completely overlap the time period
# no correlations calculated. 
rwl <- corr.rwl.seg(uv, seg.length=10, pcrit=0.05)

# correlation for individual series
cor_KEC01 <- corr.series.seg(rwl=ke, series="KEC01",
                          seg.length=10)

## note: unsure how to interpret 
## lower corelation means need to adjust values?
## https://opendendro.github.io/dplR-workshop/xdate.html


### option 2 for master chronology
# removing series based on the subsample signal strength

rwc_ids <- autoread.ids(ringwidths_slabs)
sssThresh <- 0.85
rwc_SSS <- sss(detrended_slabs, rwc_ids)
yrs <- time(ringwidths_slabs)
yrCutoff <- max(yrs[rwc_SSS < sssThresh])

ggplot() +
  geom_rect(aes(ymin=-Inf, ymax=Inf,xmin=-Inf,xmax=yrCutoff),
            fill="darkred",alpha=0.5) +
  annotate(geom = "text",y=1.5,x=1725,label="SSS < 0.85")+
  geom_hline(yintercept = 1,linetype="dashed") +
  geom_line(aes(x = yrs, y = chronchronology_slabs$std)) +
  labs(x = "Year", y = "RWI") + theme_minimal()
## note: cuts out most of the data


# cuts out insignificant data
rwc_SSS_sig <- ringwidths_slabs[rwc_SSS > sssThresh]
rwc_SSS_det <- detrend(rwl =rwc_SSS_sig, method="ModNegExp")


# detrended master chronology excluding insignificant
rwc_SSS_chron <- chron(rwc_SSS_det)
ggplot() +
  geom_hline(yintercept = 1,linetype="dashed") +
  geom_line(aes(x=time(rwc_SSS_chron),y=rwc_SSS_chron$std)) +
  geom_line(aes(x=time(rwc_SSS_chron),
                y=caps(rwc_SSS_chron$std,nyrs = 30)),
            color="darkred") +
  labs(x="Year",y="RWI") + theme_minimal()


#### note: after master chronology need to correct for erroes

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





### Next steps - from Nathan in November 

# try to get around 30 with <2 missing rings into a master chronology 
# find narrowest rings that could be missing in others 

### Next steps - winter break research
# 1: use uses EPS-based chronology stripping 
# 2: recount and check problem areas with "corr.series.seg" to check for 
# problem areas 
# 3: redo step 1 until no series can be removed
# 3: Use  Gleichläufgkeit function and Spearman's rho correlations to check
# chronology significance
