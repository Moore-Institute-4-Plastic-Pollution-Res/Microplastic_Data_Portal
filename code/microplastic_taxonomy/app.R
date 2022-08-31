library(shiny)
library(bs4Dash)
library(googlesheets4)
library(dplyr)

gs4_deauth()
file <- read_sheet("https://docs.google.com/spreadsheets/d/1pOLzGuweqyUinMaqYpWop0mqf0LaMNyHdCsWZ-ToV8M/edit?usp=sharing")
file$images <- paste0("https://drive.google.com/uc?export=view&id=", gsub(".*id=", "", file$`Image File`))

ui <- dashboardPage(
    dashboardHeader(title = "Microplastic Taxonomy"),
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
                icon = icon("sliders")
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
                    h3("You can help us build this database of microplastic imagery by filling out this form:"),
                    HTML('<a class="btn btn-info" href = "https://forms.gle/kA4ynuHsbu7VWkZm7" role = "button" >Form</a>'),
                    width = 12
                    
                )
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                    column(4, 
                           selectInput(inputId = "color", 
                                       label = "Color Selection", 
                                       choices = c("All", unique(file$`Color of particle`)),
                           )
                    ),
                    column(4,
                           selectInput(inputId = "morphology", 
                                       label = "Morphology Selection", 
                                       choices = c("All", unique(file$`Morphology of particle`)),
                           )),
                    column(4,
                           selectInput(inputId = "polymer", 
                                       label = "Polymer Selection", 
                                       choices = c("All", unique(file$`Polymer-type of particle`)),
                           ))
                ),
                uiOutput("images")
            )
        )
    )
)

server <- function(input, output) {
    output$images <- renderUI({
        
        #fluidRow(
            boxLayout(
                type = "group",
            #sortable(
            #    width = 4,
                # p(class = "text-center", paste("Column", i)),
                
                lapply(file %>% 
                           filter(if(input$color != "All") tolower(`Color of particle`) == tolower(input$color) else !is.na(images)) %>%
                           filter(if(input$morphology != "All") tolower(`Morphology of particle`) == tolower(input$morphology) else !is.na(images)) %>%
                           filter(if(input$polymer != "All") tolower(`Polymer-type of particle`) == tolower(input$polymer) else !is.na(images)) %>%
                           slice_sample(n= 100)%>%
                           pull(images), function(x){
                    box(
                        tags$image(src = x, height = "200rem"),
                        width = NULL
                    )
                })
            #)
        )
       # )
    })

}

shinyApp(ui, server)