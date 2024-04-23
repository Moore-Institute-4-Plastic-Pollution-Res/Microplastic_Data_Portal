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
library(One4All)
library(janitor)
library(jsonlite)
library(httr)

# # Load the required configuration
# config<- config::get(file = "code/validator/fake_data_config.yml")
# 
# result <- query_document_by_object_id(
#   apiKey = apiKey,
#   collection = config$mongo_collection,
#   database = 'test', # Official is 'validator'
#   dataSource = 'Cluster0',
#   objectId = "660c85af0eddb6bfe0067db1"
# )
# 
# merged_data <- result[[2]][[1]]
# Get the current working directory
wd <- getwd()

# Define the directory names
data_path <- c("data")

# Construct the full directory path
file_name1 <- "merged_data.csv"

directory_path1 <- file.path(wd, data_path)
file_path1 <- file.path(directory_path1, file_name1)

merged_data <- read_csv(file_path1)

merged_data <- clean_names(merged_data)
merged_data <- merged_data%>% 
  rename(shape = morphology, width_mm = width, m_ps_m3 = microplastic_concentration,
         m_ps_m3_2000 = microplastic_concentration2000,
         m_ps_m3_2001 = microplastic_concentration2001,
         m_ps_m3_2002 = microplastic_concentration2002,
         m_ps_m3_2003 = microplastic_concentration2003,
         m_ps_m3_2004 = microplastic_concentration2004,
         m_ps_m3_2005 = microplastic_concentration2005,
         m_ps_m3_2006 = microplastic_concentration2006,
         m_ps_m3_2007 = microplastic_concentration2007,
         m_ps_m3_2008 = microplastic_concentration2008,
         m_ps_m3_2009 = microplastic_concentration2009,
         m_ps_m3_2010 = microplastic_concentration2010,
         m_ps_m3_2011 = microplastic_concentration2011,
         m_ps_m3_2012 = microplastic_concentration2012,
         m_ps_m3_2013 = microplastic_concentration2013,
         m_ps_m3_2014 = microplastic_concentration2014,
         m_ps_m3_2015 = microplastic_concentration2015,
         m_ps_m3_2016 = microplastic_concentration2016,
         m_ps_m3_2017 = microplastic_concentration2017,
         m_ps_m3_2018 = microplastic_concentration2018,
         m_ps_m3_2019 = microplastic_concentration2019,
         m_ps_m3_2020 = microplastic_concentration2020,
         m_ps_m3_2021 = microplastic_concentration2021,
         m_ps_m3_2022 = microplastic_concentration2022,
         m_ps_m3_2023 = microplastic_concentration2023,
         m_ps_m3_2024 = microplastic_concentration2024,
         water_system_name = analysis_organization
  )

# Construct the full directory path
file_name2 <- "Samples_Geocoded.csv"

file_path2 <- file.path(directory_path1, file_name2)

directory_path2 <- directory_path1

#cities_sf <- st_read("/Users/nick_leong/Downloads/City_Boundaries/City_Boundaries.shp")
file_name3 <- "CA_Places_TIGER2016.shp"
file_path3 <- file.path(directory_path2, "ca-places-boundaries")
file_path3 <- file.path(file_path3, file_name3)
cities <- st_read(file_path3)
cities <- clean_names(cities)
cities <- rename(cities, city = name)
file_name4 <- "CA_Counties_TIGER2016.shp"
file_path4 <- file.path(directory_path2, "CA_Counties")
file_path4 <- file.path(file_path4, file_name4)
counties <- st_read(file_path4)
counties <- clean_names(counties)
counties <- rename(counties, county = name)
# Create a new column in cities with the first 5 digits of geoid
cities <- mutate(cities, county_geoid = substr(geoid, 1, 5))

# Convert cities to sf object
cities_sf <- st_as_sf(cities, coords = c("longitude_column_name", "latitude_column_name"))

# Perform spatial join using st_join
cities_sf <- st_join(cities_sf, counties %>% select(geoid, county), by = c("county_geoid" = "geoid"))

# Clean the cities_sf_wgs84 data before Shiny app starts running
cities_sf_wgs84 <- st_transform(cities_sf, "+proj=longlat +datum=WGS84")
cities_sf_wgs84 <- clean_names(cities_sf_wgs84)

merged_data$longitude_new <- merged_data$longitude
merged_data$latitude_new <- merged_data$latitude

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

# Gets rid of redundant rows
row_numbers <- as.numeric(row.names(merged_data_sf))
merged_data_sf <- merged_data_sf[row_numbers %% 1 == 0, ]

#full file path to data
Samples_Geocoded <- read_csv(file_path2, locale = locale(encoding = "latin1"))

Location_choices <- unique(Samples_Geocoded$Location)
Country_choices <- unique(Samples_Geocoded$Countries)

#TODO: should preallocate this and the dataset below because it is slow to start up.  
sankify <- function(x, starts){
    x %>%
        mutate(across(starts_with(starts), ~as.character(.))) %>%
        select(starts_with(starts), Sample_ID, Subsample_ID) %>%
        # Using rowwise() to handle row operations
        rowwise() %>%
        # Check if all values in a row are NA
        filter(any(!is.na(c_across(starts_with(starts))))) %>%
        ungroup() %>%
        mutate(across(starts_with(starts), ~ifelse(is.na(.), "0", .))) %>%
        mutate(across(starts_with(starts), ~ifelse(. == "Present", 
                                                   if(any(!. %in% c("Present", "0"))){
                                                       sample(.[!. %in% c("Present", "0")], size = 1)
                                                   } else { 
                                                       as.character(runif(n = 1))
                                                   },
                                                   .))) %>%
        mutate(across(starts_with(starts), ~as.numeric(.))) %>%
        rowwise() %>%
        mutate(across(starts_with(starts), ~./sum(c_across(starts_with(starts))))) %>%
        ungroup() %>%
        pivot_longer(cols = starts_with(starts), names_to = "type", values_to = "proportion") %>%
        mutate(proportion = proportion * 100)
}

all <- vector("list", length = 3)
all <- lapply(1:3, function(i){
    sankify(x = Samples_Geocoded, starts = c("Morphology_", "Color_", "Material_")[i])
})



ui <- bs4DashPage(
  bs4DashNavbar(
    title = "Drinking Water Plastics",
    tags$style(
      HTML(".navbar { background-color: #78909C; }")
    )
  ),
  bs4DashSidebar(
    tags$style(
      HTML(".sidebar { background-color: #78909C; }")
    ),
    sidebarMenu(
      menuItem("Map By Raw Data", tabName = "MapByRawData", icon = icon("map")),
      menuItem("Map By Countries", tabName = "MapByCountries", icon = icon("map")),
      menuItem("Other Visuals", tabName = "sankeyPlot", icon = icon("sliders-h")),
      menuItem("CA Microplastic Synthetic Data", tabName = "mapTab", icon = icon("database"))
    )
  ),
  bs4DashBody(
    tags$style(HTML("
    .content-wrapper { background-color: #F4F6F6; }

    .about-box-content h3 {
      font-size: 18px;
    }
    
    .sankey-box-content h3 {
      font-size: 18px;
    }
  ")),
    tabItems(
      tabItem(
        tabName = "MapByRawData",
        fluidRow(
          box(
            title = "About",
            h3(
              tags$div(
                "Welcome to the data visualization portal!",
                br(),
                br(),
                "This app uses microplastics sample data from the California Open Data Portal.",
                br(),
                br(),
                "Below is a map that allows you to select multiple locations, and the popup markers present overview information about the plastics at those sites.",
                br(),
                br(),
                "The concentration and source of plastics of the selected locations are then displayed in a rain cloud plot below.",
                br(),
                br(),
                "The table directly generates the data from the Open Data Portal, and shows additional information regarding the characteristics of the plastics.",
                br(),
                br(),
                "Warning: The latitude and longitude data for some locations are inaccurate due to multiple locations being grouped together within a single sample."
              )
            ),
            class = "about-box-content",
            width = 12
            ),
          column(
            width = 12,
            selectInput("Location", "Location", choices = Location_choices, multiple = TRUE)
          )
        ),
        fluidRow(
          column(
            width = 12,
            leafletOutput("mapLocation")
          )
        ),
        fluidRow(
          column(
            width = 12,
            HTML("<br>")
          )
        ),
        column(
          width = 12,
          box(
            title = "Drinking Water Plastics Plot by Location",
            plotOutput("plotLocation"),
            width = 12
          )
        ),
        column(
          width = 12,
          box(
            title = "Drinking Water Plastics Table by Location",
            style = "overflow-x: auto;",
            DT::dataTableOutput("tableLocation"), 
            width = 12
          )
        )
      ),
      tabItem(
        tabName = "MapByCountries",
        fluidRow(
          column(
            width = 12,
            selectInput("Country", "Country", choices = Country_choices, multiple = TRUE)
          )
        ),
        fluidRow(
          column(
            width = 12,
            leafletOutput("mapCountries")
          )
        ),
        fluidRow(
          column(
            width = 12,
            HTML("<br>")
          )
        ),
        column(
          width = 12,
          box(
            title = "Drinking Water Plastics Plot by Country",
            plotOutput("plotCountries"),
            width = 12
          )
        ),
        column(
          width = 12,
          box(
            title = "Drinking Water Plastics Table by Country",
            style = "overflow-x: auto;",
            DT::dataTableOutput("tableCountries"), 
            width = 12
          )
        )
      ),
      tabItem(
        tabName = "sankeyPlot",
        fluidRow(
          box(
            title = "Other Visuals",
            h3(
              tags$div(
                "Sankey plots are a useful way to visualize the relationship and flow between variables.",
                br(),
                br(),
                "The drop down allows users to look at the relationships of three different microplastic characteristics from the Open Data Portal."
              )
            ),
            class = "sankey-box-content",
            width = 12
          )
          ),
        br(),
        column(
          width = 12,
          selectInput("sankeyPlotSelection", "Select Sankey Plot",
                      choices = c("Color and Material",
                                  "Morphology and Color",
                                  "Morphology and Material"),
                      selected = "Morphology and Color")
        ),
        column(
          width = 12,
          selectInput("Location", "Location", choices = Location_choices, multiple = TRUE)
        ),
        box(
          title = "Comparing Characteristics with Sankey Plot",
          sankeyNetworkOutput("SankeyMorphColorMat", height = "700px"),
          width = 12,
          height = "900px"
          )
        ),
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
            selectInput("yearSelect", "Select Year", choices = 2024, selected = 2024)
          ),
          column(
            width = 12,
            leafletOutput("mapLocation1")
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
            width = 12,
            box(
              title = "Shape Distribution",
              plotOutput("shapeBarPlot"),
              width = 12
            )
          ),
          column(
            width = 12,
            box(
              title = "Color Distribution",
              plotOutput("colorBarPlot"),
              width = 12
            )
          ),
          column(
            width = 12,
            box(
              title = "Polymer Distribution",
              plotOutput("polymerDistributionPlot"),
              width = 12
            )
          ),
          column(
            width = 12,
            box(
              title = "Width Distribution",
              plotOutput("widthBarPlot"),
              width = 12
            )
          ),
          column(
            width = 12,
            box(
              title = "Yearly Average Concentrations",
              plotOutput("stackedBarPlot"),
              width = 12
            )
          ),
          column(
            width = 12,
            box(
              title = "Treatment Library",
              width = 12,
              selectInput("treatmentSelect", "Select Treatment Level",
                          choices = c("Primary", "Secondary", "Tertiary", "Disinfected", "Filtered"),
                          selected = c("Primary", "Secondary", "Tertiary", "Disinfected", "Filtered"),  # Set all options as selected by default
                          multiple = TRUE),
              plotOutput("boxplotTreatment")
            )
          )
        )
      )
      )
    ) 
  )

server <- function(input, output, session) {
  filtered_data <- reactive({
    samples_location <- Samples_Geocoded[Samples_Geocoded$Location %in% input$Location, ]
    samples_country <- Samples_Geocoded[Samples_Geocoded$Countries %in% input$Country, ]
    return(list(samples_location = samples_location, samples_country = samples_country))
  })
  
  # Location tab
  output$mapLocation <- renderLeaflet({
    data_location <- filtered_data()$samples_location
    leaflet() %>%
      addProviderTiles("CartoDB.Voyager", options = tileOptions(minZoom = 2)) %>%
      addCircleMarkers(
        data = data_location,
        lat = ~Approximate_Latitude, 
        lng = ~Approximate_Longitude,
        group = ~Location,
        clusterOptions = markerClusterOptions(),
        popup = paste0(
          "<div class='custom-popup'>",
          "<h4>Location Details</h4>",
          "<p><strong>Latitude:</strong> ", data_location$Approximate_Latitude, "</p>",
          "<p><strong>Longitude:</strong> ", data_location$Approximate_Longitude, "</p>",
          "<p><strong>Source:</strong> ", data_location$Source, "</p>",
          "<p><strong>Concentration:</strong> ", data_location$Concentration, "</p>",
          "</div>"
        ),
        color = "#01579B"
      ) %>%
      addLegend(position = "bottomright", colors = "#01579B", labels = "Location")
  })
  
  # Countries tab
  output$mapCountries <- renderLeaflet({
    data_country <- filtered_data()$samples_country
    leaflet() %>%
      addProviderTiles("CartoDB.Voyager", options = tileOptions(minZoom = 2)) %>%
      addCircleMarkers(
        data = data_country,
        lat = ~Approximate_Latitude,
        lng = ~Approximate_Longitude,
        group = ~Countries,
        clusterOptions = markerClusterOptions(),
        popup = paste0(
          "<div class='custom-popup'>",
          "<h4>Location Details</h4>",
          "<p><strong>Latitude:</strong> ", data_country$Approximate_Latitude, "</p>",
          "<p><strong>Longitude:</strong> ", data_country$Approximate_Longitude, "</p>",
          "<p><strong>Source:</strong> ", data_country$Source, "</p>",
          "<p><strong>Concentration:</strong> ", data_country$Concentration, "</p>",
          "</div>"
        ),
        color = "#01579B"
      ) %>%
      addLegend(position = "bottomright", colors = "#01579B", labels = "Location")
  })
  
  # Location tab
  output$tableLocation <- DT::renderDataTable({
    data_location <- filtered_data()$samples_location
    data_location$Concentration <- as.numeric(gsub("[^0-9.]", "", data_location$Concentration))
    data_location <- data_location[!is.na(data_location$Concentration), ]
    data_location <- data_location[order(data_location$Concentration), ]
    DT::datatable(data_location, style = "bootstrap", class = "cell-border stripe")
  })
  
  # Countries tab
  output$tableCountries <- DT::renderDataTable({
    data_country <- filtered_data()$samples_country
    data_country$Concentration <- as.numeric(gsub("[^0-9.]", "", data_country$Concentration))
    data_country <- data_country[!is.na(data_country$Concentration), ]
    data_country <- data_country[order(data_country$Concentration), ]
    DT::datatable(data_country, style = "bootstrap", class = "cell-border stripe")
  })
  
  # Location tab
  output$plotLocation <- renderPlot({
    data_location <- filtered_data()$samples_location
    data_location$Concentration <- as.numeric(as.character(data_location$Concentration))
    ggplot(data_location, aes(x = Source, y = Concentration, fill = factor(Source))) +
      geom_flat_violin(
        position = position_nudge(x = 0.1),
        alpha = 0.5,
        scale = "width",
        trim = FALSE,
        width = 0.8,
        lwd = 1,
      ) +
      geom_boxplot(
        width = 0.12,
        outlier.shape = 8,
        outlier.color = "navy",
        alpha = 1
      ) +
      stat_dots(
        position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4, jitter.height = 10),
        dotsize = 15,
        side = "left",
        justification = 1.1,
        binwidth = 0.08,
        alpha = 1.0
      ) +
      scale_fill_manual(values = c("#87CEEB", "#4C9900")) +
      labs(
        title = "Plastics by Source and Concentration",
        x = "Source",
        y = "Concentration (particles/L)",
        fill = "Source"
      ) +
      coord_flip() +
      theme_bw() +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18)
      )
  })
  
  # Fake Data tab
  output$mapCalifornia <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Voyager", options = tileOptions(minZoom = 2)) %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6)  # Center the map on California
  })
  
  # Countries tab
  output$plotCountries <- renderPlot({
    data_country <- filtered_data()$samples_country
    data_country$Concentration <- as.numeric(gsub("[^0-9.]", "", data_country$Concentration))
    data_country <- data_country[!is.na(data_country$Concentration), ]
    
    if (length(input$Country) == 0) {
      default_plot <- ggplot() +
        geom_blank() +
        labs(
          title = "Plastics by Source and Concentration",
          x = "Source",
          y = "Concentration (particles/L)",
          fill = "Source"
        ) +
        coord_flip() +
        theme_bw() +
        theme(
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18)
        )
      
      print(default_plot)
    } else {
      country_plots <- list()
      
      for (country in input$Country) {
        country_data <- data_country[data_country$Countries == country, ]
        
        if (nrow(country_data) > 0) {
          p <- ggplot(country_data, aes(x = factor(Source), y = Concentration, fill = factor(Source))) +
            geom_flat_violin(
              position = position_nudge(x = 0.1),
              alpha = 0.5,
              scale = "width",
              trim = FALSE,
              width = 0.8,
              lwd = 1,
            ) +
            geom_boxplot(
              width = 0.12,
              outlier.shape = 8,
              outlier.color = "navy",
              alpha = 1
            ) +
            stat_dots(
              position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4, jitter.height = 10),
              dotsize = 10,
              side = "left",
              justification = 1.1,
              binwidth = 0.08,
              alpha = 1.0
            ) +
            scale_fill_manual(values = c("#87CEEB", "#4C9900")) +
            labs(
              title = paste("Plastics by Source and Concentration -", country),
              x = "Source",
              y = "Concentration (particles/L)",
              fill = "Source"
            ) +
            coord_flip() +
            theme_bw() +
            theme(
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              plot.title = element_text(size = 18)
            )
          
          country_plots[[country]] <- p
        }
      }
      
      if(length(country_plots) > 0) {
        combined_plot <- do.call(grid.arrange, country_plots)
        print(combined_plot)
      }
    }
  })
  
  output$SankeyMorphColorMat <- renderSankeyNetwork({
    data_location <- filtered_data()$samples_location
    
    if(input$sankeyPlotSelection == "Color and Material") {
        joined <- inner_join(all[[2]], all[[3]], by = c("Sample_ID", "Subsample_ID")) %>%
            mutate(mean_prop = (proportion.x + proportion.y)/2) %>%
            group_by(type.x, type.y) %>%
            summarise(mean_prop = mean(mean_prop)) %>%
            ungroup() %>%
            filter(mean_prop > 10)
        
        
        links <- joined %>%
            rename(source = type.x,
                   target = type.y, 
                   value = mean_prop)
        
        nodes <- data.frame(
            name = c(as.character(links$source), as.character(links$target)) %>% unique()
        )
        
        links$source <- match(links$source, nodes$name) - 1
        links$target <- match(links$target, nodes$name) - 1
        
        p <- sankeyNetwork(
            Links = links, Nodes = nodes,
            Source = "source", Target = "target",
            Value = "value", NodeID = "name",
            fontSize = 10, nodeWidth = 50
        )
      
    } else if (input$sankeyPlotSelection == "Morphology and Color") {
        joined <- inner_join(all[[1]], all[[2]], by = c("Sample_ID", "Subsample_ID")) %>%
            mutate(mean_prop = (proportion.x + proportion.y)/2) %>%
            group_by(type.x, type.y) %>%
            summarise(mean_prop = mean(mean_prop)) %>%
            ungroup() %>%
            filter(mean_prop > 10)
        
        
        links <- joined %>%
            rename(source = type.x,
                   target = type.y, 
                   value = mean_prop)
        
        nodes <- data.frame(
            name = c(as.character(links$source), as.character(links$target)) %>% unique()
        )
        
        links$source <- match(links$source, nodes$name) - 1
        links$target <- match(links$target, nodes$name) - 1
        
        p <- sankeyNetwork(
            Links = links, Nodes = nodes,
            Source = "source", Target = "target",
            Value = "value", NodeID = "name",
            fontSize = 10, nodeWidth = 50
        )
      
    } else if (input$sankeyPlotSelection == "Morphology and Material") {
        joined <- inner_join(all[[1]], all[[3]], by = c("Sample_ID", "Subsample_ID")) %>%
            mutate(mean_prop = (proportion.x + proportion.y)/2) %>%
            group_by(type.x, type.y) %>%
            summarise(mean_prop = mean(mean_prop)) %>%
            ungroup() %>%
            filter(mean_prop > 10)
        
        
        links <- joined %>%
            rename(source = type.x,
                   target = type.y, 
                   value = mean_prop)
        
        nodes <- data.frame(
            name = c(as.character(links$source), as.character(links$target)) %>% unique()
        )
        
        links$source <- match(links$source, nodes$name) - 1
        links$target <- match(links$target, nodes$name) - 1
        
        p <- sankeyNetwork(
            Links = links, Nodes = nodes,
            Source = "source", Target = "target",
            Value = "value", NodeID = "name",
            fontSize = 10, nodeWidth = 50
        )
    }
  })

  # Bar plot for "shape" within the app with reactivity
  output$shapeBarPlot <- renderPlot({
    filtered_data2 <- filtered_data2()  # Get filtered data based on selectors

    ggplot(filtered_data2, aes(x = shape)) +
      geom_bar(fill = "#4682B4") +
      labs(x = "Shape", y = "Count") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
  })

  # Bar plot for "color" within the app with reactivity
  output$colorBarPlot <- renderPlot({
    filtered_data2 <- filtered_data2()  # Get filtered data based on selectors

    ggplot(filtered_data2, aes(x = color)) +
      geom_bar(fill = "#708090") +
      labs(x = "Color", y = "Count") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
  })

  # Reactive expression for filtering based on county and city input
  polymer_distribution_data <- reactive({
    filtered_data2 <- filtered_data2()  # Get filtered data based on selectors

    # Exclude rows with NA values in the polymer column GETTING RID OF NA VALUES, CHANGE WITH REAL DATA
    filtered_data2 <- filtered_data2[!is.na(filtered_data2$polymer), ]

    ggplot(filtered_data2, aes(x = polymer)) +
      geom_bar(fill = "#4682B4") +
      labs(x = "Polymer", y = "Count") +
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
    filtered_data2 <- filtered_data2()  # Get filtered data based on selectors

    ggplot(filtered_data2, aes(x = width_mm)) +
      geom_bar(fill = "#708090", color = "#708090", linewidth = 0.5) +  # Adjust fill color, outline color, and size
      labs(x = "Width (mm)", y = "Count") +
      scale_x_log10() +  # Apply logarithmic scale to x-axis
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
      theme(text = element_text(size = 12, family = "Arial"))
  })

  # Bar plot for yearly microplastic concentrations
  output$stackedBarPlot <- renderPlot({
    # Filtered data
    data <- filtered_data2()
    
    # Initialize lists to store means and years
    means <- list()
    years <- list()
    
    # Iterate through years 2000 to 2024
    for (year in 2000:2024) {
      # Get column name for the year
      col_name <- paste0("m_ps_m3_", year)
      
      # Check if column exists in the data
      if (col_name %in% names(data)) {
        # Calculate mean concentration for the year
        means[[col_name]] <- mean(data[[col_name]], na.rm = TRUE)
        years[[col_name]] <- year
      }
    }
    
    # Combine means and years into single vectors
    means <- unlist(means)
    years <- unlist(years)
    
    # Plot the bar plot
    barplot(means, names.arg = years, xlab = "Year", ylab = "Mean Microplastic Concentration (P/m^3)", col = "#4682B4", ylim = c(0, max(means) * 1.2))
    
    # Add horizontal lines at specific y-axis values with ascending colors of concern
    abline(h = c(0.0003), lty = "dotted", lwd = 1, col = "#000000")
    abline(h = c(0.066), lty = "dotdash", lwd = 1, col = "#000000")
    abline(h = c(0.219), lty = "dashed", lwd = 1, col = "#000000")
    abline(h = c(0.859), lty = "solid", lwd = 1, col = "#000000")
    
    
    # Get the width of the plotting area
    plot_width <- par("usr")[2] - par("usr")[1]
    
    space_between_legends <- plot_width / 4
    
    # Calculate the x-coordinate for each legend
    legend_x1 <- par("usr")[1]
    legend_x2 <- legend_x1 + space_between_legends
    legend_x3 <- legend_x1 + 2 * space_between_legends
    legend_x4 <- legend_x1 + 3 * space_between_legends
    # Set the y-coordinate for the legends to be near the top of the plot
    legend_y <- par("usr")[4] - 0.05
    
    # Add legend
    legend(x = legend_x1, y = legend_y, legend = c("Threshold 1"), lty = "dotted", lwd = 0.75, col = "#000000", bty = "n", cex = 1)
    legend(x = legend_x2, y = legend_y, legend = c("Threshold 2"), lty = "dotdash", lwd = 0.75, col = "#000000", bty = "n", cex = 1)
    legend(x = legend_x3, y = legend_y, legend = c("Threshold 3"), lty = "dashed", lwd = 0.75, col = "#000000", bty = "n", cex = 1)
    legend(x = legend_x4, y = legend_y, legend = c("Threshold 4"), lty = "solid", lwd = 0.75, col = "#000000", bty = "n", cex = 1)
    
  })


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
  filtered_data2 <- reactive({
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
    data_to_display <- filtered_data2() %>%
      select(county, city, water_system_name, m_ps_m3,shape,color,width_mm,polymer,latitude,longitude, treatment_level,m_ps_m3_2000, m_ps_m3_2001, m_ps_m3_2002, m_ps_m3_2003, m_ps_m3_2004, m_ps_m3_2005, m_ps_m3_2006, m_ps_m3_2007, m_ps_m3_2008, m_ps_m3_2009, m_ps_m3_2010, m_ps_m3_2011, m_ps_m3_2012, m_ps_m3_2013, m_ps_m3_2014, m_ps_m3_2015, m_ps_m3_2016, m_ps_m3_2017, m_ps_m3_2018, m_ps_m3_2019, m_ps_m3_2020, m_ps_m3_2021, m_ps_m3_2022, m_ps_m3_2023, m_ps_m3_2024 )%>%
      rename(
        County = county,
        City = city,
        "Water System Name" = water_system_name,
        "Concentration (Particles/m^3)" = m_ps_m3,
        "Morphology" = shape,
        "Color" = color,
        "Width (mm)" = width_mm,
        "Polymer" = polymer,
        "Latitude" = latitude,
        "Longitude" = longitude,
        "Treatment Level" = treatment_level,
        "Concentration (Particles/m^3) in 2000" = m_ps_m3_2000,
        "Concentration (Particles/m^3) in 2001" = m_ps_m3_2001,
        "Concentration (Particles/m^3) in 2002" = m_ps_m3_2002,
        "Concentration (Particles/m^3) in 2003" = m_ps_m3_2003,
        "Concentration (Particles/m^3) in 2004" = m_ps_m3_2004,
        "Concentration (Particles/m^3) in 2005" = m_ps_m3_2005,
        "Concentration (Particles/m^3) in 2006" = m_ps_m3_2006,
        "Concentration (Particles/m^3) in 2007" = m_ps_m3_2007,
        "Concentration (Particles/m^3) in 2008" = m_ps_m3_2008,
        "Concentration (Particles/m^3) in 2009" = m_ps_m3_2009,
        "Concentration (Particles/m^3) in 2010" = m_ps_m3_2010,
        "Concentration (Particles/m^3) in 2011" = m_ps_m3_2011,
        "Concentration (Particles/m^3) in 2012" = m_ps_m3_2012,
        "Concentration (Particles/m^3) in 2013" = m_ps_m3_2013,
        "Concentration (Particles/m^3) in 2014" = m_ps_m3_2014,
        "Concentration (Particles/m^3) in 2015" = m_ps_m3_2015,
        "Concentration (Particles/m^3) in 2016" = m_ps_m3_2016,
        "Concentration (Particles/m^3) in 2017" = m_ps_m3_2017,
        "Concentration (Particles/m^3) in 2018" = m_ps_m3_2018,
        "Concentration (Particles/m^3) in 2019" = m_ps_m3_2019,
        "Concentration (Particles/m^3) in 2020" = m_ps_m3_2020,
        "Concentration (Particles/m^3) in 2021" = m_ps_m3_2021,
        "Concentration (Particles/m^3) in 2022" = m_ps_m3_2022,
        "Concentration (Particles/m^3) in 2023" = m_ps_m3_2023,
        "Concentration (Particles/m^3) in 2024" = m_ps_m3_2024
      )

    datatable(data_to_display, style = "bootstrap", class = "cell-border stripe")
  })

  output$mapLocation1 <- renderLeaflet({
    leaflet() %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
      addCircleMarkers(
        data = filtered_data2(),
        clusterOptions = markerClusterOptions(),
        popup = paste0(
          "<div class='custom-popup'>",
          "<h4>Water System Details</h4>",
          "<p><strong>Water System Name:</strong> ", filtered_data2()$water_system_name, "</p>",
          "<p><strong>Latitude:</strong> ", filtered_data2()$latitude, "</p>",
          "<p><strong>Longitude:</strong> ", filtered_data2()$longitude, "</p>",
          "<p><strong>Particles/m^3:</strong> ", filtered_data2()$m_ps_m3, "</p>",
          "<p><strong>City:</strong> ", filtered_data2()$city, "</p>",
          "<p><strong>County:</strong> ", filtered_data2()$county, "</p>",
          "</div>"
        ),
        color = ~colorFactor("Set1", unique(filtered_data2()$m_ps_m3))(m_ps_m3),
        fillOpacity = 0.8
      )
  })

  # Filtered data for box plots
  filtered_boxplot_data <- reactive({
    # Filter data based on selected treatment levels
    filtered_data2 <- merged_data_sf %>%
      filter(treatment_level %in% input$treatmentSelect)

    filtered_data2
  })

  # Box plot for microplastic concentration based on treatment levels
  output$boxplotTreatment <- renderPlot({
    filtered_data2 <- filtered_boxplot_data()  # Get filtered data based on selected treatment levels

    # Plot box plots for microplastic concentration (m_ps_m3) based on treatment levels
    ggplot(filtered_data2, aes(x = treatment_level, y = m_ps_m3)) +
      geom_boxplot(fill = "#708090") +
      labs(x = "Treatment Level", y = "Microplastic Concentration (m_ps_m3)") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
  })
}

shinyApp(ui, server)

