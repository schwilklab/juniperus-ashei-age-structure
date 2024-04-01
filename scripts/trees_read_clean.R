## trees_read_clean.R
## Alex Bowers

### cleans trees.csv for analysis

# load sampling and property data
trees <- read.csv("data/trees.csv", na.strings = "NA") 
properties <- read.csv("data/properties.csv")
transect_starts <- read.csv("data/transect_starts.csv")

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
