# using COFECHA to crossdate rings

## tutorial of dplR
# https://opendendro.github.io/dplR-workshop/introduction.html

# needed packages
library(dplR)
# library(tidyverse) # DWS: NO. 
#library(signal)



## ring width summaries

# coverts raw file into readable rind width length
ringwidths <- read.rwl("./data/ringwidths.raw")

## how old individuals are
rw_sum <- summary(ringwidths)
##Error in ts(x) : 'ts' object must have one or more observations


## DWS: script fails here. 

ages <- dplyr::select(rw_sum, c("series", "year"))
colnames(ages)[1] <- "id"

# plots ring widths
plot(ringwidths, plot.type="spag")

# reports summary stats and absent rings
rwl.report(ringwidths)
## 0s are accidental double clicks. Will need to remove these


## detrending and creating a chronology

# dimensionless ring-width index (RWI)
ringwidths_rwi <- detrend(rwl = ringwidths, method = "ModNegExp")
## how lined up each individual is
colMeans(ringwidths_rwi, na.rm=TRUE)
## how lined up each year is
rowMeans(ringwidths_rwi, na.rm=TRUE)

# mean-value chronology
ringwidths_crn <- chron(ringwidths_rwi)
plot(ringwidths_crn, add.spline=TRUE, nyrs=20)




## where are the missing rings?

# series correlation
wr_cor <- corr.rwl.seg(ringwidths, seg.length=20, pcrit=0.01)

# where the errors are (range of dates)
summary(wr_cor$flags)
wr_cor$flags


### Next steps

# try to get around 30 with <2 missing rings into a master chronology 
# find narrowest rings that could be missing in others 
