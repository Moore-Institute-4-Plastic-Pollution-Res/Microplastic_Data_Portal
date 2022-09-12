library(shiny)
library(bs4Dash)
library(googlesheets4)
library(dplyr)
library(prompter)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(validate)
library(digest)
library(data.table)
library(bs4Dash)

options(shiny.maxRequestSize = 30*1024^2)

ui <- dashboardPage(
    dashboardHeader(title = "Data Validator"),
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
                "Validator",
                tabName = "item2",
                icon = icon("check")
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
                    column(1,
                           fileInput("file_rules", NULL,
                                     placeholder = ".csv",
                                     buttonLabel = "Rules...",
                                     width = "100%",
                                     accept=c("text/csv",
                                              "text/comma-separated-values,text/plain")) 
                           #%>%
                           #   add_prompt(
                           #       message = "Upload the rules csv you want to use",
                           #       type = "info", 
                           #       size = "medium", rounded = TRUE
                    ),
                    column(1, 
                           downloadButton("download_rules", "", style = "background-color: #2a9fd6;") #%>%
                           # add_prompt(
                           #     message = "Example Rules File",
                           #     type = "info", 
                           #     size = "medium", rounded = TRUE
                           
                           # )
                    ),
                    column(1,
                           fileInput("file", NULL,
                                     placeholder = ".csv",
                                     buttonLabel = "Data...",
                                     accept=c("text/csv",
                                              "text/comma-separated-values,text/plain")) #%>%
                           #  add_prompt(
                           #      message = "Upload the csv you want to validate",
                           #      type = "info", 
                           #      size = "medium", rounded = TRUE
                    ),
                    column(1,
                           downloadButton("download_sample", "", style = "background-color: #2a9fd6;") #%>%
                           # add_prompt(
                           #     message = "Download example data to validate",
                           #     type = "info", 
                           #     size = "medium", rounded = TRUE
                           # )
                    ),
                    column(8, uiOutput("certificate"), uiOutput("alert"))),
                
                fluidRow(
                    column(4,
                           
                           h3("Issues Raised"), 
                           prettySwitch("show_decision",
                                        label = "Show errors and warnings only?",
                                        inline = T,
                                        value = T,
                                        status = "success",
                                        fill = T),
                           DT::dataTableOutput("show_report")),
                    column(8,
                           h3("Issue Selected"), 
                           DT::dataTableOutput("report_selected")
                           
                           
                    )
                ),
                fluidRow(
                    hr(),
                    p(align = "center", 
                      HTML('<a class="btn btn-info" href = "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/Microplastic_Data_Portal" role = "button" >Open Code</a>')
                    )
                )
            )
        )
    )
)



server <- function(input, output) {
    
    filtered <- reactive({
        filtered_test <- file %>% 
            filter(if(input$color != "ALL") tolower(`Color of particle`) == tolower(input$color) else !is.na(images)) %>%
            filter(if(input$morphology != "ALL") tolower(`Morphology of particle`) == tolower(input$morphology) else !is.na(images)) %>%
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
                    title = filtered()$`Researcher Name`[x],
                    
                    tags$figure(tags$img(src = filtered()$images[x], width = "400em"),
                                tags$figcaption(tags$small(filtered()$`Citation`[x]))),
                    maximizable = T,
                    width = NULL
                )
            })
            #)
        )
        
        # )
    })
    
}

shinyApp(ui, server)