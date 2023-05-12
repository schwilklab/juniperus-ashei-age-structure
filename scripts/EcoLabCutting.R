library(dplyr)
library(tidyr)
library(stringr)
library(rgdal)
library(sp)
library(raster)
library(sf)

samples <- read.csv("EcoLabSampling1.csv")

## makes kml file from csv

# set lat to W
samples$Latitude <- samples$Latitude*-1

# create column of species ID
samples[samples == "Edward"] <- "ED"
samples[samples == "Bandera"] <- "BA"
samples[samples == "Hays-Travis"] <- "HT"
samples[samples == "Kerr"] <- "KE"
samples[samples == "Medina"] <- "ME"
samples[samples == "Uvalde"] <- "UV"
samples[samples == "Burnet"] <- "BU"
samples$Individual <- str_pad(samples$Individual, 2, pad = "0")
samples <- unite(samples, col="ID", 
                 c("Site", "Transect", "Individual"), sep="")

# reorder columns for simplicity
samples <- samples[, c(2,3,4,1,5)]

# creates kml file

samples_sf <- st_as_sf(samples, coords = c("Latitude", "Longitude"), crs = 4326)            
st_write(samples_sf, "EcoLabInitialSampling.kml", driver = "KML", 
         options = c("ID","CBH"))



## summary of data collection
# counts amount of cutting needed
sampled <- read.csv("EcoLabSampling1.csv")

# counts amount needed to cut
cut <- sampled[grep("^c", sampled$CBH),]
cut <- count(sampled, Site)
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
done <- count(done, Site)
colnames(done)[2] ="sampled"

# creates summary data frame
summary <- merge(done, cut, by = "Site")
sums <- summarise_at(summary, c("sampled", "needs_cut", "hours_needed"), sum)
summary[8,] <- c("Total", sums)
summary             
