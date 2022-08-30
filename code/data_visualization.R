#install all neccessary packages before running

#install.packages("tidyverse")
#install.packages("tidygeocoder")
#install.packages("sf")
#install.packages("mapview")


library(tidyverse)
library("readxl")
library(tidygeocoder)
library(sf)  
library(mapview)


<<<<<<< HEAD
Samples_excel <- read_excel("data/Samples.xlsx")
=======
Samples_excel <- read_excel("/data/Samples.xlsx")
>>>>>>> 1049c0bcbd848253ad478f42d3154f84c2457e30

Samples_GeoCoded <- Samples_excel %>%
  geocode('Location (most precise sata possible for location where sample was collected)', method = 'osm', lat = latitude, long = longitude)

Samples_Map <- Samples_GeoCoded %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

World <- data(worldMapEnv)

mapview(Samples_Map, zcol = 'Concentration (concentration of microplastics in sample)', legend = FALSE)
