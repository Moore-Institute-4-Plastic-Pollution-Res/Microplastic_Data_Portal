library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(sf)  
library(mapview)
library(mapdata)
library(tidyverse)
library(leaflet)


# Restore the object
Samples_Map <- readRDS(file = "data/Samples_Map.rds")
# Generate map of microplastics sample data
World <- data(worldMapEnv)

ui <- dashboardPage(
    dashboardHeader(title = "Microplastic Data Analysis"),
    dashboardSidebar(
        sidebarUserPanel(
            name = "Welcome!"
        ),
        
        sidebarMenu(
            id = "sidebarmenu",
            menuItem(
                "About",
                tabName = "item1",
                icon = icon("sliders-h")
            ),
            menuItem(
                "Data Analysis",
                tabName = "item2",
                icon = icon("chart-bar")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "item1",
                box(
                    title = "Overview",
                    h3("Welcome to the microplastic data analysis page, this is a place to visualize and analyze microplastics data from the microplastics data portal."),
                    width = 12
                    )
            ),
            tabItem(
                tabName = "item2",
                box(
                    tags$style(type = "text/css", "#mapplot {height: calc(100vh - 80px) !important;}"),
                    leafletOutput("mapplot"),
                    width = 12,
                    #height = "100vh",
                    maximizable = T
                )
            )
        )
    )
)



server <- function(input, output) {
    output$mapplot <- renderLeaflet({
        mapview(Samples_Map, zcol = 'Concentration', legend = FALSE)@map
    })
}

shinyApp(ui, server)