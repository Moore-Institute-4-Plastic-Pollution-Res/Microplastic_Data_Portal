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

#full file path to data
Samples_Geocoded <- read_csv("data/Samples_Geocoded.csv", locale = locale(encoding = "latin1"))

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
      menuItem("Other Visuals", tabName = "sankeyPlot", icon = icon("sliders-h"))
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
                "The table directly generates the data from the Open Data Portal, and shows additional information regarding the characteristics of the plastics."
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
          selectInput("sankeyPlotSelection", "Select Sankey Plot:",
                      choices = c("Color and Material",
                                  "Morphology and Color",
                                  "Morphology and Material"),
                      selected = "Morphology and Color"),
          box(
            title = "Comparing Characteristics with Sankey Plot",
            sankeyNetworkOutput("SankeyMorphColorMat", height = "700px"),
            width = 12,
            height = "900px"
            )
          )
        )
      ) 
    )
  )

server <- function(input, output) {
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
    p
  })
}

shinyApp(ui, server)
