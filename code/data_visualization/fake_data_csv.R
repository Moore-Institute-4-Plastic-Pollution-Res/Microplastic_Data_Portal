library(bs4Dash)
library(leaflet)
library(DT)
library(plotly)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidygeocoder)
library(sf)  
library(mapview)
library(mapdata)
library(data.table)
library(shiny)
library(ggdist)
library(ggthemes)
library(ggplot2)
library(rlang)
library(PupillometryR)
library(gridExtra)
library(networkD3)
library(tidyr)
library(janitor)
library(RColorBrewer)

DWF <- read.csv("/Users/nick_leong/Downloads/9dca2f92-4630-4bee-a9f9-69d2085b57e3.csv")
DWF <- clean_names(DWF)

# Load required libraries
library(tidyverse)  # for data manipulation

# Define a function to generate short Lorem Ipsum phrases
generate_short_lorem_ipsum <- function() {
  lorem_ipsum <- c(
    "Lorem ipsum dolor sit amet",
    "consectetur adipiscing elit",
    "sed do eiusmod tempor incididunt",
    "ut labore et dolore magna aliqua",
    "ut enim ad minim veniam",
    "quis nostrud exercitation ullamco laboris",
    "Duis aute irure dolor in reprehenderit",
    "in voluptate velit esse cillum dolore",
    "eu fugiat nulla pariatur",
    "Excepteur sint occaecat cupidatat non proident"
  )
  return(sample(lorem_ipsum, 1))
}

# Replace the values in the water_system_name column with short Lorem Ipsum phrases
DWF$water_system_name <- sapply(1:nrow(DWF), function(i) generate_short_lorem_ipsum())

DWF_names <- DWF %>%
  select(water_system_name)


# Specify the correct path to your CSV file
file_path1 <- "/Users/nick_leong/Downloads/1_Lake_coordinates_MPs_concentration.csv"

# Read the CSV file using read_delim
X1_Lake_coordinates_MPs_concentration <- read_delim(
  file_path1,
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ",")
)

# Specify the correct path to your CSV file
file_path2 <- "/Users/nick_leong/Downloads/2_MPs_features.csv"

# Read the CSV file using read_delim
X2_MPs_features <- read_delim(
  file_path2,
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ",")
)

i17_California_Jurisdictional_Dams <- read_csv("~/Downloads/i17_California_Jurisdictional_Dams.csv")
i17_California_Jurisdictional_Dams <- clean_names(i17_California_Jurisdictional_Dams)

#cities_sf <- st_read("/Users/nick_leong/Downloads/City_Boundaries/City_Boundaries.shp")
cities <- st_read("/Users/nick_leong/Downloads/ca-places-boundaries/CA_Places_TIGER2016.shp")
cities <- clean_names(cities)
cities <- rename(cities, city = name)
counties <- st_read("/Users/nick_leong/Downloads/CA_Counties/CA_Counties_TIGER2016.shp")
counties <- clean_names(counties)
counties <- rename(counties, county = name)
# Create a new column in cities with the first 5 digits of geoid
cities <- mutate(cities, county_geoid = substr(geoid, 1, 5))

# Convert cities to sf object
cities_sf <- st_as_sf(cities, coords = c("longitude_column_name", "latitude_column_name"))

# Perform spatial join using st_join
cities_sf <- st_join(cities_sf, counties %>% select(geoid, county), by = c("county_geoid" = "geoid"))


# Select and merge variables
dam_data <- i17_California_Jurisdictional_Dams
# Make DWF_names equal in size to merged_data
DWF_names <- DWF_names %>%
  slice(1:nrow(dam_data))
# Replace dam names with water_system_name in merged_data
dam_data$dam_name <- DWF_names$water_system_name
dam_data$water_system_name <- dam_data$dam_name 
dam_data<- dam_data %>%
  select(water_system_name, latitude, longitude)


random_dam_data<- dam_data
random_dam_data <- clean_names(random_dam_data)

combined_lake_data <- full_join(X1_Lake_coordinates_MPs_concentration, X2_MPs_features, by = "lake")
combined_lake_data <- clean_names(combined_lake_data)
combined_lake_data <- combined_lake_data %>%
  mutate(row_num = row_number())
random_dam_data <- random_dam_data %>%
  mutate(row_num = row_number())
merged_data <- merge(combined_lake_data, random_dam_data, by = "row_num", suffixes = c("_old", "_new"))
merged_data <- merged_data %>%
  select(-lake, -latitude_old, -longitude_old, -row_num)
merged_data$latitude <- merged_data$latitude_new
merged_data$longitude <- merged_data$longitude_new
# Add a new column called treatment_level to merged_data
merged_data <- merged_data %>%
  mutate(treatment_level = factor(sample(c("Primary", "Secondary", "Tertiary", "Disinfected", "Filtered"), nrow(merged_data), replace = TRUE)))



set.seed(123)

# Generate random concnetrations from 2000 to 2023
years <-2000:2023

# Add columns for each year and fill with random concentrations
for (year in years) {
  merged_data[paste0("m_ps_m3_", year)] <- rnorm(nrow(merged_data), mean(merged_data$m_ps_m3), sd(merged_data$m_ps_m3))
}

merged_data <- within(merged_data, m_ps_m3_2024 <- m_ps_m3)

melted_data <- reshape2::melt(merged_data)

# Extract the year from the variable column
melted_data$year <- as.numeric(gsub("m_ps_m3_", "", melted_data$variable))

# Clean the cities_sf_wgs84 data before Shiny app starts running
cities_sf_wgs84 <- st_transform(cities_sf, "+proj=longlat +datum=WGS84")
cities_sf_wgs84 <- clean_names(cities_sf_wgs84)

# Convert merged_data to an sf object and set CRS
merged_data_sf <- st_as_sf(merged_data, coords = c("longitude_new", "latitude_new"), crs = st_crs(cities_sf_wgs84))

merged_data_sf <- st_make_valid(merged_data_sf)
cities_sf_wgs84 <- st_make_valid(cities_sf_wgs84)
duplicated_rows <- duplicated(merged_data_sf)
merged_data_sf <- merged_data_sf[!duplicated_rows, ]
merged_data_sf <- st_simplify(merged_data_sf)
cities_sf_wgs84 <- st_simplify(cities_sf_wgs84)

# Spatial join to associate dam points with cities and counties
merged_data_sf <- st_join(merged_data_sf, cities_sf_wgs84, join = st_within)

# Export dataframe to CSV file
write.csv(merged_data_sf, "merged_data_sf.csv", row.names = FALSE)
