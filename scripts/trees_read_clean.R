# Alex Bowers

## cleans trees.csv for analysis

#library(tidyverse)  ## DWS: avoid, please. Remember my class. Don't dump more
                    ## into the global namespace than needed. NOt sure why you
                    ## are even bothering with tidyr::unite for this trivial
                    ## task of paste().

# load sampling data
trees <- read.csv("./data/trees.csv", na.strings = "NA")

# set lat to W
## DWS: ??? Why? Why are latitudes incorrect to begin with?
trees$long <- trees$long * -1

# change site code
trees[trees == "Edwards_2022-25"] <- "ED"
trees[trees == "Bandera_2022-35"] <- "BA"
trees[trees == "Hays-Travis_2022-02"] <- "HT"
trees[trees == "Kerr_2021-36"] <- "KE"
trees[trees == "Medina_2022-04"] <- "ME"
trees[trees == "Uvalde_2021-03"] <- "UV"
trees[trees == "Burnet_2022-16"] <- "BU"

## DWS: Why the above? Why is data hard-coded here in the script? Poor
## practice.

# merge sampling data with transet starts
trees <- tidyr::unite(trees, col="id", 
                 c("property_id", "transect"), sep="")


## DWS: That is not a merge.

# create sample ID
trees$individual <- string::str_pad(trees$individual, 2, pad = "0")
trees <- tidyr::unite(trees, col="id", 
                 c("id", "individual"), sep="")
