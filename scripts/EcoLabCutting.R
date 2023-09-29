## library(dplyr)
## library(tidyr)
## library(stringr)
## library(rgdal)
## library(sp)
## library(raster)
## library(sf)

## DWS: Why are all those packages dumped in the global namespace? You never
## use them other than sf, stringr, and dplyr.

samples <- read.csv("./data/EcoLabSampling1.csv")

## makes kml file from csv

# set lat to W
samples$Latitude <- samples$Latitude*-1
## DWS: This is a bad idea.

# create column of species ID
samples[samples == "Edward"] <- "ED"
samples[samples == "Bandera"] <- "BA"
samples[samples == "Hays-Travis"] <- "HT"
samples[samples == "Kerr"] <- "KE"
samples[samples == "Medina"] <- "ME"
samples[samples == "Uvalde"] <- "UV"
samples[samples == "Burnet"] <- "BU"
samples$Individual <- stringr::str_pad(samples$Individual, 2, pad = "0")
samples <- tidyr::unite(samples, col="ID", 
                 c("Site", "Transect", "Individual"), sep="")

## DWS: This code is all repetition of code in trees_read_clean.R! But is "ID"
## the same as "id"? You are managing to create so much confusion in so few
## lines of code.

# reorder columns for simplicity
samples <- samples[, c(2,3,4,1,5)]

## DWS: fragile to rely on column or row order.

# creates kml file
## DWS: why?

samples_sf <- sf::st_as_sf(samples, coords = c("Latitude", "Longitude"), crs = 4326)            
sf::st_write(samples_sf, "./results/EcoLabInitialSampling.kml", driver = "KML", 
         options = c("ID","CBH"))

# One second run: Error: Dataset already exists.
## DWS: Your code should handle this case.



## summary of data collection
# counts amount of cutting needed
sampled <- read.csv("./data/EcoLabSampling1.csv")
## DWS: this is repeated code. Why are you re-reading this?


# counts amount needed to cut
cut <- sampled[grep("^c", sampled$CBH),]
## DWS: bad practice to overwrite a built in function name! Poor object name.

cut <- dplyr::count(sampled, Site)
cut

# adjusts missing data for Uvalde due to storm
cut[7,2] <- cut[7,2]+(4*4)+(4*4)

# creates column based on cutting 4 trees per hour
cut[,3] <- cut[,2]/4
colnames(cut)[2] ="needs_cut"
colnames(cut)[3] ="hours_needed"
cut

# counts amount sampled on first trip
done <- sampled[-grep("^c", sampled$CBH),]
done <- dplyr::count(done, Site)
colnames(done)[2] ="sampled"

# creates summary data frame
summary <- merge(done, cut, by = "Site")
sums <- dplyr::summarise_at(summary, c("sampled", "needs_cut", "hours_needed"), sum)
summary[8,] <- c("Total", sums)
summary             
