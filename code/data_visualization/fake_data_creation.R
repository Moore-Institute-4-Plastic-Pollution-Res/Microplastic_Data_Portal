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

# Get the current working directory
wd <- getwd()

# Define the directory names
code <- c("code")
data_visualization <- c("data_visualization")
data_path <- c("data")

# Construct the full directory path
directory_path1 <- file.path(wd, code, data_visualization)
directory_path2 <- file.path(directory_path1, data_path)

file_name1 <- "9dca2f92-4630-4bee-a9f9-69d2085b57e3.csv"
file_path1 <- file.path(directory_path2, file_name1)

DWF <- read.csv(file_path1)
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
file_name2 <- "1_Lake_coordinates_MPs_concentration.csv"
file_path2 <- file.path(directory_path2, file_name2)
# Read the CSV file using read_delim
X1_Lake_coordinates_MPs_concentration <- read_delim(
  file_path2,
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ",")
)

# Specify the correct path to your CSV file
file_name3 <- "2_MPs_features.csv"
file_path3 <- file.path(directory_path2, file_name3)
# Read the CSV file using read_delim
X2_MPs_features <- read_delim(
  file_path3,
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ",")
)

file_name4 <- "i17_California_Jurisdictional_Dams.csv"
file_path4 <- file.path(directory_path2, file_name4)

i17_California_Jurisdictional_Dams <- read_csv(file_path4)
i17_California_Jurisdictional_Dams <- clean_names(i17_California_Jurisdictional_Dams)

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
combined_lake_data <- combined_lake_data %>%
  select(-longitude, -latitude)
merged_data <- dplyr::left_join(combined_lake_data, random_dam_data, by = "row_num")
merged_data <- merged_data %>%
  select(-lake, -row_num)
# Add a new column called treatment_level to merged_data
merged_data <- merged_data %>%
  mutate(treatment_level = factor(sample(c("Primary", "Secondary", "Tertiary", "Disinfected", "Filtered"), nrow(merged_data), replace = TRUE)))
merged_data <- merged_data[1:1236, ]


set.seed(123)

# Generate random concnetrations from 2000 to 2023
years <-2000:2023

nrow(merged_data)
mean(merged_data$m_ps_m3)
sd(merged_data$m_ps_m3)

# Add columns for each year and fill with random concentrations
for (year in years) {
  merged_data[paste0("m_ps_m3_", year)] <- rnorm(nrow(merged_data), mean(merged_data$m_ps_m3), sd(merged_data$m_ps_m3))
}

merged_data <- within(merged_data, m_ps_m3_2024 <- m_ps_m3)

merged_data <- merged_data %>%
  select(-id_lake, -sample_lake, -slide_numb, -x12, -plastic_code, -shape_fra_fib, -shape_code)

file_name5 <- "merged_data.csv"
file_path5 <- file.path(directory_path2, file_name5)

##### Take data from the updated merged_data and update with correct rules
# If rerunning, change 
# - polymer -> Polymer
# - color -> Color
# - shape -> Morphology
# - width_mm -> Width
# - latitude -> Latitude
# - longitude -> Longitude
# - water_system_name -> AnalysisOrganization
# - m_ps_m3 -> MicroplasticConcentration
# - m_ps_m3 -> MicroplasticConcentration2000-2024
# - treatment_level -> TreatmentLevel
# List of colors
colors <- c("Transparent", "Blue", "Red", "Brown", "Green", "Orange", "White", 
            "Yellow", "Pink", "Black", "Purple", "Grey", "Other")

# Get the number of rows in merged_data
num_rows <- nrow(merged_data)

# Generate random colors
set.seed(123)  # Setting seed for reproducibility
Colors_new <- sample(colors, num_rows, replace = TRUE)

# Assign random colors to "Colors" column
merged_data$Color <- Colors_new

# List of morphology types
morphologies <- c("Fragment", "Fiber", "Sphere", "Rubbery Fragment", 
                  "Film", "Pellet", "Fiber Bundle", "Foam")

# Get the number of rows in merged_data
num_rows <- nrow(merged_data)

# Generate random morphology types
set.seed(123)  # Setting seed for reproducibility
random_morphologies <- sample(morphologies, num_rows, replace = TRUE)

# Assign random morphology types to "Morphology" column
merged_data$Morphology <- random_morphologies

# List of polymer types
polymers <- c("Polyolefins (polyalkenes)", "Polycarbonates", "Polysiloxanes", 
              "Polyesters", "Polyterephthalates", "Polydienes (butadienes, isoprenes)", 
              "Polystyrenes (polyphenylethylenes, -methylstyrene)", 
              "Polyurethanes (isocyanates)", "Polyhaloolefins (vinylhalides)", 
              "Polyacrylonitriles (nitriles)", "Cellulose derivatives (ether cellulose)", 
              "Polyamides (polylactams)", "Polymethacrylates", 
              "Polyethersulfones (polysulfone)", "Polyvinylethers", 
              "Polydiglycidyl ethers (polyepoxides, polyhydroxyethers, phenoxy)", 
              "Polyphenylethers (polyphenyleneoxide)", "Polyhydroxy(meth)acrylates", 
              "Polyacrylates (propenoates)", 
              "Polyethers (polyglycols, oxyalkylenes, glycidyl ethers & cyclic ethers)", 
              "Polyvinylesters", "Polyvinylalcohols", "Polyacrylamides", 
              "Polyvinylketones", "Poly(ether)ketones (polyketones)", 
              "Polyphenylenes (polyaromatics)", "Polysuccinates")

# Get the number of rows in merged_data
num_rows <- nrow(merged_data)

# Generate random polymer types
set.seed(123)  # Setting seed for reproducibility
random_polymers <- sample(polymers, num_rows, replace = TRUE)

# Assign random polymer types to "Polymer" column
merged_data$Polymer <- random_polymers

# Export dataframe to CSV file
write.csv(merged_data, file_path5, row.names = FALSE)
