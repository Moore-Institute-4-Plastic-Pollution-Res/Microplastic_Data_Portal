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
directory_name1 <- c("code")
directory_name2 <- c("data_visualization")

# Construct the full directory path
directory_path <- file.path(wd, directory_name1, directory_name2)

# Define the file name
file_name <- "merged_data_sf.csv"

# Construct the full file path
file_path <- file.path(directory_path, file_name)

merged_data_sf <- read_csv(file_path)


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
      menuItem("Interactive Map", tabName = "mapTab", icon = icon("map")),
      menuItem("Treatment Library", tabName = "treatmentLibrary", icon = icon("flask"))
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
              selectInput("treatmentSelect", "Select Treatment Level", choices = c("Primary", "Secondary", "Tertiary", "Disinfected", "Filtered"), multiple = TRUE),
              plotOutput("boxplotTreatment")
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
      labs(x = "Shape", y = "Count") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
  })
  
  # Bar plot for "color" within the app with reactivity
  output$colorBarPlot <- renderPlot({
    filtered_data <- filtered_data()  # Get filtered data based on selectors
    
    ggplot(filtered_data, aes(x = color)) +
      geom_bar(fill = "#708090") +
      labs(x = "Color", y = "Count") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
  })
  
  # Reactive expression for filtering based on county and city input
  polymer_distribution_data <- reactive({
    filtered_data <- filtered_data()  # Get filtered data based on selectors
    
    # Exclude rows with NA values in the polymer column GETTING RID OF NA VALUES, CHANGE WITH REAL DATA
    filtered_data <- filtered_data[!is.na(filtered_data$polymer), ]
    
    ggplot(filtered_data, aes(x = polymer)) +
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
    filtered_data <- filtered_data()  # Get filtered data based on selectors
    
    ggplot(filtered_data, aes(x = width_mm)) +
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
    data <- filtered_data()
    
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
    abline(h = c(0.0003, 0.066, 0.219, 0.859), lty = "dashed", lwd = 0.75, col = "#000000")
    
    # Add text labels for each line with adjusted colors and y-axis values
    text(2000, 0.0003, "Threshold 1: Investigative monitoring", adj = c(0, -0.1), cex = 0.7, col = "#000000", pos = 4)
    text(2000, 0.066, "Threshold 2: Discharge monitoring", adj = c(0, -0.5), cex = 0.7, col = "#000000", pos = 4)
    text(2000, 0.219, "Threshold 3: Management planning", adj = c(0, -0.5), cex = 0.7, col = "#000000", pos = 4)
    text(2000, 0.859, "Threshold 4: Source control measures", adj = c(0, -0.5), cex = 0.7, col = "#000000", pos = 4)
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
        "Concentration (Particles/m^3)" = m_ps_m3,
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
          "<p><strong>Particles/m^3:</strong> ", filtered_data()$m_ps_m3, "</p>",
          "<p><strong>City:</strong> ", filtered_data()$city, "</p>",
          "<p><strong>County:</strong> ", filtered_data()$county, "</p>",
          "</div>"
        ),
        color = ~colorFactor("Set1", unique(filtered_data()$m_ps_m3))(m_ps_m3),
        fillOpacity = 0.8
      )
  })
  
  # Filtered data for box plots
  filtered_boxplot_data <- reactive({
    # Filter data based on selected treatment levels
    filtered_data <- merged_data_sf %>%
      filter(treatment_level %in% input$treatmentSelect)
    
    filtered_data
  })
  
  # Box plot for microplastic concentration based on treatment levels
  output$boxplotTreatment <- renderPlot({
    filtered_data <- filtered_boxplot_data()  # Get filtered data based on selected treatment levels
    
    # Plot box plots for microplastic concentration (m_ps_m3) based on treatment levels
    ggplot(filtered_data, aes(x = treatment_level, y = m_ps_m3)) +
      geom_boxplot(fill = "#708090") +
      labs(x = "Treatment Level", y = "Microplastic Concentration (m_ps_m3)") +
      theme_minimal() +
      theme(text = element_text(size = 12, family = "Arial"))
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
