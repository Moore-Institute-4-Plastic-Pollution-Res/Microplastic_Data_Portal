library(shiny)
library(shinyjs)
library(bs4Dash)
library(shinyWidgets)
library(googlesheets4)
library(dplyr)
library(imager)
library(curl)

gs4_deauth()
file <- read_sheet("https://docs.google.com/spreadsheets/d/1pOLzGuweqyUinMaqYpWop0mqf0LaMNyHdCsWZ-ToV8M/edit?usp=sharing")
file$images <- paste0("https://drive.google.com/uc?id=", gsub(".*id=", "", file$`Image File`))

ui <- dashboardPage(
  dashboardHeader(title = "Microplastic Image Explorer",
                  fluidRow(
                    column(
                      width = 12,
                      tags$div(
                        tags$style(HTML("
                      .navbar {
                      background-color: #6D929B;
                      }
                      .breadcrumb {
                      font-size: 14px;
                      font-family: Arial, sans-serif;
                      }"))
                      ),
                      tags$div(
                        id = "breadcrumb",
                        verbatimTextOutput("breadcrumb_output")
                      )
                    )
                  )
  ),
  dashboardSidebar(
    sidebarUserPanel(
      #image = "https://drive.google.com/file/d/13iCjC10dV3giFhCCoir_8mnbwtHM1rMA/view?usp=sharing",
      name = "Welcome!"
    ),
    tags$style(
      HTML("
      .sidebar { 
      background-color: #6D929B;
      }
           ")
    ),
    sidebarMenu(
      id = "sidebarmenu",
      #sidebarHeader("Header 1"),
      menuItem(
        "Image Query",
        tabName = "item1",
        icon = icon("camera")
      ),
      menuItem(
        "Contribute",
        tabName = "item2",
        icon = icon("sliders-h")
      )
    )
  ),
  dashboardBody(
    tags$style(
      HTML(".content-wrapper {
              background-color: #F5FAFA;
           }
           .selected-border .selectize-input {
              border-color: #C1DAD6;
              border-width: 5px;
              background-color: #C1DAD6;
           }
        ")
    ),
    tags$script(HTML('
    // JavaScript/jQuery to add custom class when a filter is selected
    $(document).on("change", "select", function() {
      $(this).parent().toggleClass("selected-border", $(this).val() !== "ALL");
    });
  ')),
    tabItems(
      tabItem(
        tabName = "item1",
        fluidRow(
          box(
            title = "Overview",
            h3(
              tags$div(
                "Welcome to the microplastic taxonomy page!",
                br(),
                br(),
                "This is a place to improve your use of visual microscopy in microplastic identification.",
                br(),
                br(),
                "Go to the image query tab below to get started querying our database of microplastic images by color, morphology, and polymer types.",
                style = "font-size: 20px;"
              )
            ),
            width = 12
          )
        ),
        fluidRow(
          column(4,
                 selectizeInput(inputId = "citation", 
                                label = "Citation", 
                                choices = c("ALL", toupper(unique(file$Citation))),
                                selected = "ALL"
                 )
          ),
          column(4, 
                 selectizeInput(inputId = "color", 
                                label = "Color", 
                                choices = c("ALL", toupper(unique(file$Color))),
                                selected = "ALL"
                 )
          ),
          column(4,
                 selectizeInput(inputId = "morphology", 
                                label = "Morphology", 
                                choices = c("ALL", toupper(unique(file$Morphology))),
                                selected = "ALL"
                 )
          )
        ),
        fluidRow(
          column(4,
                 selectizeInput(inputId = "polymer", 
                                label = "Polymer", 
                                choices = c("ALL", toupper(unique(file$`Polymer-type of particle`))),
                                selected = "ALL"
                 )
          ),
          column(4,
                 selectizeInput(inputId = "size", 
                                label = "Size", 
                                choices = c("ALL", toupper(unique(file$`Size of particle`))),
                                selected = "ALL"
                 )
          ),
          column(4, actionButton(inputId = "clear_filters", label = "Clear All"))
        ),
        fluidRow(
          column(8, uiOutput("images"))
        ),
        fluidRow(
          column(6, align = "center", actionButton("prev_btn", "Previous")),
          column(6, align = "center", actionButton("next_btn", "Next"))
        )
      ),
      tabItem(
        tabName = "item2",
        fluidRow(
          box(
            title = "Contribute",
            collapsed = F,
            h3("You can help us build this database of microplastic imagery by filling out this form if you have a few images to share:",
               style = "font-size: 20px;"),
            HTML('<a class="btn btn-info" href = "https://forms.gle/kA4ynuHsbu7VWkZm7" role = "button" >Form</a>'),
            h3("If you have over 50 images, please contact wincowger@gmail.com to share a zip folder instead. All we need is a folder with images that have unique names and a spreadsheet that lists the name of the image and relevant metadata following the google form information.",
               style = "font-size: 20px;"
            ),
            width = 12
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  filtered_breadcrumb <- reactive({
    citation <- input$citation
    color <- input$color
    morphology <- input$morphology
    polymer <- input$polymer
    size <- input$size
    
    breadcrumb_text <- paste("Filters:", citation, color, morphology, polymer, size, sep = " > ")
    breadcrumb_text
  })
  
  output$breadcrumb_output <- renderText({
    breadcrumb_text <- filtered_breadcrumb()
  })
  
  observeEvent(list(input$citation, input$color, input$morphology, input$size, input$polymer), {
    current_choices <- filtered()
    updateSelectizeInput(session, "citation", choices = c("ALL", toupper(unique(current_choices$Citation))), selected = input$citation)
    updateSelectizeInput(session, "color", choices = c("ALL", toupper(unique(current_choices$Color))), selected = input$color)
    updateSelectizeInput(session, "morphology", choices = c("ALL", toupper(unique(current_choices$Morphology))), selected = input$morphology)
    updateSelectizeInput(session, "polymer", choices = c("ALL", toupper(unique(current_choices$`Polymer-type of particle`))), selected = input$polymer)
    updateSelectizeInput(session, "size", choices = c("ALL", toupper(unique(current_choices$`Size of particle`))), selected = input$size)
  })
  
  observeEvent(input$clear_filters, {
    updateSelectizeInput(session, "citation", choices = c("ALL", toupper(unique(file$Citation))), selected = "ALL")
    updateSelectizeInput(session, "color", choices = c("ALL", toupper(unique(file$Color))), selected = "ALL")
    updateSelectizeInput(session, "morphology", choices = c("ALL", toupper(unique(file$Morphology))), selected = "ALL")
    updateSelectizeInput(session, "polymer", choices = c("ALL", toupper(unique(file$`Polymer-type of particle`))), selected = "ALL")
    updateSelectizeInput(session, "size", choices = c("ALL", toupper(unique(file$`Size of particle`))), selected = "ALL")
  })
  
  filtered <- reactive({
    file %>% 
      filter(if(input$citation != "ALL") tolower(Citation) == tolower(input$citation) else !is.na(images)) %>%
      filter(if(input$size != "ALL") tolower(`Size of particle`) == tolower(input$size) else !is.na(images)) %>%
      filter(if(input$color != "ALL") tolower(Color) == tolower(input$color) else !is.na(images)) %>%
      filter(if(input$morphology != "ALL") tolower(Morphology) == tolower(input$morphology) else !is.na(images)) %>%
      filter(if(input$polymer != "ALL") tolower(`Polymer-type of particle`) == tolower(input$polymer) else !is.na(images))
  })
  
  images_per_page <- 30
  items_per_row <- 3
  current_page <- reactiveVal(1)
  
  observe({
    current_page(min(current_page(), total_pages()))
  })
  
  total_pages <- reactive({
    ceiling(nrow(filtered()) / images_per_page)
  })
  
  paged_data <- reactive({
    start_index <- (current_page() - 1) * images_per_page + 1
    end_index <- min(start_index + images_per_page - 1, nrow(filtered()))
    filtered()[start_index:end_index, ]
  })
  
  output$images <- renderUI({
    req(paged_data())
    rows <- lapply(1:nrow(paged_data()), function(x) {
      div(
        class = "col-sm-4",
        box(
          id = paste0("box", x),
          title = paged_data()$`Researcher Name`[x],
          div(
            class = "figure",
            style = "display: flex; justify-content: center; align-items: center;",
            tags$figure(
              tags$a(href=paged_data()$images[x], target="_blank",
                     tags$img(src = paged_data()$images[x], style = 'width: 100%; height: 200px; object-fit: contain;')),
              tags$figcaption(tags$small(paged_data()$`Citation`[x]))
            )
          ),
          maximizable = TRUE,
          width = NULL
        )
      )
    })
    fluidRow(tags$div(class = "row", rows))
  })
  
  observeEvent(input$prev_btn, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  observeEvent(input$next_btn, {
    current_page(current_page() + 1)
  })
}

shinyApp(ui, server)