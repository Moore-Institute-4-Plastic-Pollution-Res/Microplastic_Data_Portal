# Data Visualization of Microplastics Data


#install all neccessary packages before running

#install.packages("tidyverse")
#install.packages("tidygeocoder")
#install.packages("sf")
#install.packages("mapview")


library(tidyverse)
library(readxl)
library(tidygeocoder)
library(sf)  
library(mapview)
library(mapdata)

# Read Samples Excel file into R
# Data file does not exist
Samples_excel <- read.csv("https://data.ca.gov/dataset/e7624fce-c058-4fa1-a29f-2594d8f8f160/resource/32488529-3a8f-4909-a9c7-fe50ec5f75fa/download/samples_merged.csv")

# Geocode locations to closest precision possible, add XY location values
Samples_GeoCoded <- Samples_excel %>%
  geocode("Location", method = 'osm', lat = latitude, long = longitude)

# Filter out samples which could not be geocoded
Samples_Map <- Samples_GeoCoded %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Save an object to a file
saveRDS(Samples_Map, file = "code/data_visualization/data/Samples_Map.rds")
# Restore the object
Samples_Map <- readRDS(file = "code/data_visualization/data/Samples_Map.rds")

# Generate map of microplastics sample data
World <- data(worldMapEnv)

mapview(Samples_Map, zcol = 'Concentration', legend = FALSE)
