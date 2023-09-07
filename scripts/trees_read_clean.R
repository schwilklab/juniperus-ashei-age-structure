# Alex Bowers

## cleans trees.csv for analysis

library(tidyverse)

# load sampling data
trees <- read.csv("trees.csv", na.strings = "NA")

# set lat to W
trees$long <- trees$long*-1

# change site code
trees[trees == "Edwards_2022-25"] <- "ED"
trees[trees == "Bandera_2022-35"] <- "BA"
trees[trees == "Hays-Travis_2022-02"] <- "HT"
trees[trees == "Kerr_2021-36"] <- "KE"
trees[trees == "Medina_2022-04"] <- "ME"
trees[trees == "Uvalde_2021-03"] <- "UV"
trees[trees == "Burnet_2022-16"] <- "BU"

# merge sampling data with transet starts
trees <- unite(trees, col="id", 
                 c("property_id", "transect"), sep="")


# create sample ID
trees$individual <- str_pad(trees$individual, 2, pad = "0")
trees <- unite(trees, col="id", 
                 c("id", "individual"), sep="")