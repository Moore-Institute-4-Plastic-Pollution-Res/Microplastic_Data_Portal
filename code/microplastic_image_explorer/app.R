library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(googlesheets4)
library(dplyr)

gs4_deauth()
file <- read_sheet("https://docs.google.com/spreadsheets/d/1pOLzGuweqyUinMaqYpWop0mqf0LaMNyHdCsWZ-ToV8M/edit?usp=sharing")
file$images <- paste0("https://drive.google.com/uc?export=view&id=", gsub(".*id=", "", file$`Image File`))

ui <- dashboardPage(
    dashboardHeader(title = "Microplastic Image Explorer"),
    dashboardSidebar(
        sidebarUserPanel(
            #image = "https://drive.google.com/file/d/13iCjC10dV3giFhCCoir_8mnbwtHM1rMA/view?usp=sharing",
            name = "Welcome!"
        ),
        
        sidebarMenu(
            id = "sidebarmenu",
            #sidebarHeader("Header 1"),
            menuItem(
                "About",
                tabName = "item1",
                icon = icon("sliders-h")
            ),
            menuItem(
                "Image Query",
                tabName = "item2",
                icon = icon("camera")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "item1",
                box(
                    title = "Overview",
                    h3("Welcome to the microplastic taxonomy page, this is a place to improve your use of visual microscopy in microplastic identification. Go to the image query tab to get started querying our database of microplastic images by color, morphology, and polymer types."),
                    width = 12
                    ),
                box(
                    title = "Contribute",
                    collapsed = T,
                    h3("You can help us build this database of microplastic imagery by filling out this form if you just have a few images to share:"),
                    HTML('<a class="btn btn-info" href = "https://forms.gle/kA4ynuHsbu7VWkZm7" role = "button" >Form</a>'),
                    h3("If you over 50 images to share, please contact wincowger@gmail.com to share a zip folder of images. All we need is a folder with images that have unique names and a spreadsheet that lists the name of the image and relevant metadata following the google form information."),
                    width = 12
                    
                )
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                         column(4,
                                selectInput(inputId = "citation", 
                                            label = "Citation", 
                                            choices = c("ALL", toupper(unique(file$citation))),
                                )),
                    column(4, 
                           selectInput(inputId = "color", 
                                       label = "Color", 
                                       choices = c("ALL", toupper(unique(file$Color))),
                           )
                    ),
                    column(4,
                           selectInput(inputId = "morphology", 
                                       label = "Morphology", 
                                       choices = c("ALL", toupper(unique(file$Morphology))),
                           ))),
                fluidRow(
                    column(4,
                           selectInput(inputId = "polymer", 
                                       label = "Polymer", 
                                       choices = c("ALL", toupper(unique(file$`Polymer-type of particle`))),
                           )),
                    column(4,
                           selectInput(inputId = "size", 
                                       label = "Size", 
                                       choices = c("ALL", toupper(unique(file$`Size of particle`))),
                           ))
                ),
                uiOutput("images")
            )
        )
    )
)



server <- function(input, output) {
    
    
    filtered <- reactive({
            filtered_test <- file %>% 
                                filter(if(input$citation != "ALL") tolower(Citation) == tolower(input$citation) else !is.na(images)) %>%
                                filter(if(input$size != "ALL") tolower(`Size of particle`) == tolower(input$size) else !is.na(images)) %>%
                                filter(if(input$color != "ALL") tolower(Color) == tolower(input$color) else !is.na(images)) %>%
                                filter(if(input$morphology != "ALL") tolower(Morphology) == tolower(input$morphology) else !is.na(images)) %>%
                                filter(if(input$polymer != "ALL") tolower(`Polymer-type of particle`) == tolower(input$polymer) else !is.na(images))

        if(nrow(filtered_test) == 0){
            NULL
        }
        else{
            filtered_test %>%
                slice_sample(n= if(nrow(.) > 100) 100 else nrow(.))
        }
    })
    
    output$images <- renderUI({
        req(filtered())
            boxLayout(
                type = "group",
                #sortable(
                #    width = 4,
                # p(class = "text-center", paste("Column", i)),
                
                lapply(1:nrow(filtered()), function(x){
                    box(
                        id = paste0("box", x),
                        title = filtered()$`Researcher Name`[x],
                        div(class = "figure", 
                        tags$figure(
                            tags$a(href=filtered()$images[x], target="_blank",
                                tags$img(src = filtered()$images[x], style = 'width: 20vw;')),
                               tags$figcaption(tags$small(filtered()$`Citation`[x])))),
                        maximizable = T,
                        style = 'width: 25vw;',
                        width = NULL
                    )
                })
                #)
            )
            
       # )
    })

}

shinyApp(ui, server)