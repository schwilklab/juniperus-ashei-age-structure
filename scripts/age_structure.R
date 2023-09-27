library(geosphere)
library(ggplot2)

# merge field data with ring counts

## should use left join to keep all data
tree_ages <- merge(trees, ages, by = "id")

# transect starts csv
transect <- read.csv("transect_starts.csv")


# merge tree data with transect starts 
transect[transect == "Edwards_2022-25"] <- "ED"
transect[transect == "Bandera_2022-35"] <- "BA"
transect[transect == "Hays-Travis_2022-02"] <- "HT"
transect[transect == "Kerr_2021-36"] <- "KE"
transect[transect == "Medina_2022-04"] <- "ME"
transect[transect == "Uvalde_2021-03"] <- "UV"
transect[transect == "Burnet_2022-16"] <- "BU"
tree_ages <- merge(tree_ages, transect, 
                   by.x = c("property_id", "transect"), 
                   by.y = c("property_id", "transect"))

# rename columns 
colnames(tree_ages)[5] <- "sample_lat"
colnames(tree_ages)[6] <- "sample_long"
colnames(tree_ages)[12] <- "age"
colnames(tree_ages)[13] <- "transect_lat"
colnames(tree_ages)[14] <- "transect_long"

# distance from transect start
samplingpoint <- data.frame(long=tree_ages$sample_long, lat=tree_ages$sample_lat)
transectpoint <- data.frame(long=tree_ages$transect_long, lat=tree_ages$transect_lat)
tree_ages$distance <- distHaversine(samplingpoint, transectpoint)

# group by transect
tree_ages <- unite(tree_ages, col="transect", 
               c("property_id", "transect"), sep="")

# all plots age structure
ggplot(tree_ages, aes(distance, age, color=transect)) + 
  geom_point() +
  facet_wrap(~transect)
