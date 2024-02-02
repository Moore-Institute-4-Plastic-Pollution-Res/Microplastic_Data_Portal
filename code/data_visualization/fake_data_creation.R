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


#############################
ui <- bs4DashPage(
  bs4DashNavbar(
    title = "Microplastic Locations",
    tags$style(
      HTML(".navbar { background-color: #78909C; }")
    )
  ),
  bs4DashSidebar(
    tags$style(
      HTML(".sidebar { background-color: #78909C; }")
    ),
    sidebarMenu(
      menuItem("Interactive Map", tabName = "mapTab", icon = icon("map"))
    )
  ),
  bs4DashBody(
    tabItems(
      tabItem(
        tabName = "mapTab",
        fluidRow(
          box(
            title = "California Microplastics in Drinking Water", 
            h3(
              tags$div(
                "Disclaimer: The data presented in this tab is entirely simulated for illustrative purposes and does not represent actual observations. This synthetic dataset is generated to demonstrate the functionality of the application and should not be interpreted as real-world information.",
                style = "font-size: 14px;"
              )
            ),
            width = 12
          ),
        ),
        fluidRow(
          column(
            width = 12,
            # Add yearSelect input above the first map
            selectInput("yearSelect", "Select Year", choices = 2000:2024, selected = 2024)
          ),
          column(
            width = 12,
            leafletOutput("mapLocation")
          ),
          column(
            width = 6,
            selectInput("countySelect", "Select County", choices = NULL, multiple = TRUE)
          ),
          column(
            width = 6,
            selectInput("citySelect", "Select City", choices = NULL, multiple = TRUE)
          ),
          column(
            width = 12,
            box(
              title = "Plastic Data by Location",
              style = "overflow-x: auto;",
              DT::dataTableOutput("plastictableLocation"), 
              width = 12
            )
          ),
          column(
            width = 6,
            plotOutput("shapeBarPlot")
          ),
          column(
            width = 6,
            plotOutput("colorBarPlot")
          ),
          column(
            width = 6,
            plotOutput("polymerDistributionPlot")
          ),
          column(
            width = 6,
            plotOutput("widthBarPlot")
          ),
          tabItem(
            tabName = "stackedBarTab",
            fluidRow(
              column(
                width = 12,
                uiOutput("stackedBarPlotUI")  # Add this line to include the plot in the app UI
              )
            )
          ),
          # column(
          #   width = 12,
          #   box(
          #     title = verbatimTextOutput("summaryText"),
          #     style = "overflow-x: auto;",
          #     DT::dataTableOutput("summaryTable"),
          #     width = 12
          #   )
          # ),
          tabItem(
            tabName = "barPlotTab",
            fluidRow(
              column(
                width = 12,
                plotOutput("stackedBarPlot")
              )
            )
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # Bar plot for "shape" within the app with reactivity
  output$shapeBarPlot <- renderPlot({
    filtered_data <- filtered_data()  # Get filtered data based on selectors
    
    ggplot(filtered_data, aes(x = shape)) +
      geom_bar(fill = "#4682B4") +
      labs(title = "Shape Distribution", x = "Shape", y = "Count") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
  })
  
  # Bar plot for "color" within the app with reactivity
  output$colorBarPlot <- renderPlot({
    filtered_data <- filtered_data()  # Get filtered data based on selectors
    
    ggplot(filtered_data, aes(x = color)) +
      geom_bar(fill = "#708090") +
      labs(title = "Color Distribution", x = "Color", y = "Count") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
  })
  
  # Reactive expression for filtering based on county and city input
  polymer_distribution_data <- reactive({
  filtered_data <- filtered_data()  # Get filtered data based on selectors
  
  # Exclude rows with NA values in the polymer column GETTING RID OF NA VALUES, CHANGE WITH REAL DATA
  filtered_data <- filtered_data[!is.na(filtered_data$polymer), ]
  
  ggplot(filtered_data, aes(x = polymer)) +
    geom_bar(fill = "#708090") +
    labs(title = "Polymer Distribution", x = "Polymer", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    theme(text = element_text(size = 12, family = "Arial"))
})

  
  # Bar plot for polymer distribution within the app with reactivity
  output$polymerDistributionPlot <- renderPlot({
    polymer_distribution_data()
  })
  
  
  # Bar plot for "width_mm" within the app with logarithmic scale and reactivity
  output$widthBarPlot <- renderPlot({
    filtered_data <- filtered_data()  # Get filtered data based on selectors
    
    ggplot(filtered_data, aes(x = width_mm)) +
      geom_bar(fill = "#4682B4", color = "#4682B4", linewidth = 0.5) +  # Adjust fill color, outline color, and size
      labs(title = "Width (mm) Distribution", x = "Width (mm)", y = "Count") +
      scale_x_log10() +  # Apply logarithmic scale to x-axis
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
      theme(text = element_text(size = 12, family = "Arial"))
  })
  
  # Stacked Bar Plot using melted data
  output$stackedBarPlot <- renderPlot({
    ggplot(filtered_melted_data(), aes(x = year, y = value, fill = factor(year))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +  # Use viridis blue color palette
      labs(title = "Microplastic Concentrations Over Years",
           x = "Year",
           y = "Concentration (m_ps_m3)",
           fill = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # # Stacked Bar Plot using melted data
  # output$stackedBarPlot <- renderPlot({
  #   filtered_data <- filtered_data() %>%
  #     select(year, starts_with("m_ps_m3_")) %>%
  #     reshape2::melt(id.vars = "year")
  #   
  #   ggplot(filtered_melted_data, aes(x = year, y = value, fill = factor(year))) +
  #     geom_bar(stat = "identity") +
  #     labs(title = "Microplastic Concentrations Over Years",
  #          x = "Year",
  #          y = "Concentration (m_ps_m3)",
  #          fill = "Year") +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })
  # 
  
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
  
  # Populate county choices for selectInput
  observe({
    county_choices <- unique(merged_data_sf$county)
    updateSelectInput(session, "countySelect", choices = county_choices)
  })
  
  # Populate city choices based on selected counties for selectInput
  observe({
    selected_counties <- input$countySelect
    city_choices <- unique(merged_data_sf$city[merged_data_sf$county %in% selected_counties])
    city_choices <- sort(city_choices)  # Sort the city choices alphabetically
    updateSelectInput(session, "citySelect", choices = city_choices)
  })
  
  # Reactive expression for filtering based on county and city input
  filtered_melted_data <- reactive({
    keyword <- input$damSearch
    selected_counties <- input$countySelect
    selected_cities <- input$citySelect
    
    # Filter by dam name
    filtered <- if (is.null(keyword) || keyword == "") {
      melted_data
    } else {
      melted_data[grep(keyword, melted_data$water_system_name, ignore.case = TRUE), ]
    }
    
    # Filter by selected counties
    if (!is.null(selected_counties) && length(selected_counties) > 0) {
      filtered <- filtered %>% filter(county %in% selected_counties)
    }
    
    # Filter by selected cities
    if (!is.null(selected_cities) && length(selected_cities) > 0) {
      filtered <- filtered %>% filter(city %in% selected_cities)
    }
    
    return(filtered)
  })
  
  # Reactive expression for filtering based on county and city input
  filtered_data <- reactive({
    keyword <- input$damSearch
    selected_counties <- input$countySelect
    selected_cities <- input$citySelect
    
    # Filter by water system name
    filtered <- if (is.null(keyword) || keyword == "") {
      merged_data_sf
    } else {
      merged_data_sf[grep(keyword, merged_data_sf$water_system_name, ignore.case = TRUE), ]
    }
    
    # Filter by selected counties
    if (!is.null(selected_counties) && length(selected_counties) > 0) {
      filtered <- filtered %>% filter(county %in% selected_counties)
    }
    
    # Filter by selected cities
    if (!is.null(selected_cities) && length(selected_cities) > 0) {
      filtered <- filtered %>% filter(city %in% selected_cities)
    }
    
    return(filtered)
  })
  
  # Location tab
  output$plastictableLocation <- DT::renderDataTable({
    data_to_display <- filtered_data() %>%
      select(county, city, water_system_name, m_ps_m3, everything()) %>%
      rename(
        County = county,
        City = city,
        "Water System Name" = water_system_name,
        "Concentration (m_ps_m3)" = m_ps_m3,
        "Lake ID" = id_lake,
        "Sample ID" = sample_lake,
        "Slide Number" = slide_numb,
        "Plastic Code" = plastic_code,
        "Morphology" = shape,
        "Fragment/Fiber" = shape_fra_fib
        ##### Continue to do this#####
      )
    
    datatable(data_to_display, style = "bootstrap", class = "cell-border stripe")
  })
  
  output$mapLocation <- renderLeaflet({
    leaflet() %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
      addCircleMarkers(
        data = filtered_data(),
        clusterOptions = markerClusterOptions(),
        popup = paste0(
          "<div class='custom-popup'>",
          "<h4>Water System Details</h4>",
          "<p><strong>Water System Name:</strong> ", filtered_data()$water_system_name, "</p>",
          "<p><strong>Latitude:</strong> ", filtered_data()$latitude, "</p>",
          "<p><strong>Longitude:</strong> ", filtered_data()$longitude, "</p>",
          "<p><strong>m_ps_m3:</strong> ", filtered_data()$m_ps_m3, "</p>",
          "<p><strong>City:</strong> ", filtered_data()$city, "</p>",
          "<p><strong>County:</strong> ", filtered_data()$county, "</p>",
          "</div>"
        ),
        color = ~colorFactor("Set1", unique(filtered_data()$m_ps_m3))(m_ps_m3),
        fillOpacity = 0.8
      )
  })
}
# Summary text*** COMMENTED OUT FOR NOW TO MAKE CLEANER
# output$summaryText <- renderText({
#   county_label <- if (!is.null(input$countySelect) && length(input$countySelect) > 0) {
#     paste("County:", paste(input$countySelect, collapse = ", "))
#   } else {
#     "County: All"
#   }
#   
#   city_label <- if (!is.null(input$citySelect) && length(input$citySelect) > 0) {
#     paste("City:", paste(input$citySelect, collapse = ", "))
#   } else {
#     "City: All"
#   }
#   
#   paste("Summary statistics for", county_label, city_label)
# })

# # Summary table *** COMMENTED OUT FOR NOW TO MAKE CLEANER
# output$summaryTable <- DT::renderDataTable({
#   summary_stats <- data.frame(
#     Statistic = c("Mean", "Median", "Mode", "Min", "Max", "Standard Deviation"),
#     Value = c(
#       mean(filtered_data()$m_ps_m3),
#       median(filtered_data()$m_ps_m3),
#       Mode(filtered_data()$m_ps_m3),
#       min(filtered_data()$m_ps_m3),
#       max(filtered_data()$m_ps_m3),
#       sd(filtered_data()$m_ps_m3)
#     )
#   )
# 
#   datatable(
#     summary_stats,
#     style = "bootstrap",
#     class = "cell-border stripe"
#   )
# })

shinyApp(ui, server)