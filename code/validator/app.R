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
                           box(title = "Issues Raised", 
                               dropdownMenu = boxDropdown(
                                   boxDropdownItem(
                                   prettySwitch("show_decision",
                                            label = "Errors only?",
                                            inline = T,
                                            value = T,
                                            status = "success",
                                            fill = T))
                                   ),
                               DT::dataTableOutput("show_report"),
                               width = 4
                               ),
                    box(title = "Issue Selected",
                           DT::dataTableOutput("report_selected"),
                        width = 8
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

server <- function(input, output, session) {
    
    rules_example <- read.csv("www/rules.csv") 
    
    data_example <- read.csv("www/Samples.csv")
    
    dataset <- reactiveValues(data = NULL)
    
    validation_summary <- reactiveValues(results = NULL, report = NULL, rules = NULL)
    
    observeEvent(input$file_rules, {
        file_rules <- input$file_rules$datapath
        if (!grepl("(\\.csv$)",
                   ignore.case = T, as.character(file_rules))) {
            #reset("file")
            dataset$data <- NULL
            validation_summary$rules <- NULL
            validation_summary$report <- NULL
            validation_summary$results <- NULL
            show_alert(
                title = "Data type not supported!",
                text = paste0("Uploaded data type is not currently supported; please
                      upload a .csv file."),
                type = "warning")
            #return(NULL)
        }
        else{
            rules <- read.csv(input$file_rules$datapath)
            
            validation_summary$rules <- validator(.data=rules)
        }
        
    })
    
    observeEvent(input$file, {
        req(input$file)
        file <- input$file$datapath
        
        # Read in data when uploaded based on the file type
        if (!grepl("(\\.csv$)", ignore.case = T, as.character(file))) {
            dataset$data <- NULL
            validation_summary$rules <- NULL
            validation_summary$report <- NULL
            validation_summary$results <- NULL
            show_alert(
                title = "Data type not supported!",
                text = paste0("Uploaded data type is not currently supported; please
                      upload a .csv file."),
                type = "warning")
        }
        else if (is.null(input$file_rules)) {
            #reset("file")
            dataset$data <- NULL
            validation_summary$rules <- NULL
            validation_summary$report <- NULL
            validation_summary$results <- NULL
            show_alert(
                title = "Need Rules File",
                text = paste0("You must upload a rules file before uploading a data file to validate."),
                type = "warning")
            #return(NULL)
        }
        else{
            dataset$data <- read.csv(file)
            
            validation_summary$report <- confront(dataset$data, validation_summary$rules)
            
            validation_summary$results <- summary(validation_summary$report) %>%
                mutate(status = ifelse(fails > 0 | error | warning , "error", "success")) %>%
                mutate(description = meta(validation_summary$rules)$description)
            
        }
        
    })
    
    overview_table <- reactive({
        req(input$file)
        validation_summary$results %>%
            filter(if(input$show_decision){status == "error"} else{status %in% c("error", "success")}) %>%
            select(description, status, name, expression, everything())
    })
    
    selected <- reactive({
        req(input$file)
        req(input$show_report_rows_selected)
        violating(dataset$data, validation_summary$report[overview_table()[input$show_report_rows_selected, "name"]])
    })
    
    
    output$certificate <- renderUI({
        req(file)
        req(validation_summary$results)
        
        if(all(validation_summary$results$status != "error")){
            downloadButton("download_certificate", "Download Certificate", style = "background-color: #2a9fd6; width: 100%;")
        }
        else{
            NULL
        }
    })
    
    
    output$alert <- renderUI({
        req(input$file)
        req(validation_summary$results)
        if(any(validation_summary$results$status == "error")){
            HTML('<button type="button" class="btn btn-danger btn-lg btn-block">ERROR</button>')
        }
        else{
            HTML('<button type="button" class="btn btn-success btn-lg btn-block">SUCCESS</button>')
        }
    })
    
    
    output$show_report <- DT::renderDataTable({
        req(input$file)
        req(any(validation_summary$results$status == "error"))
        datatable({overview_table() %>%
                select(description, status)},
                extensions = 'Buttons',
                options = list(
                    searchHighlight = TRUE,
                    scrollX = TRUE,
                    lengthChange = FALSE, 
                    pageLength = 5,
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                rownames = FALSE,
                filter = "top", 
                style = "bootstrap", 
                selection = list(mode = "single", color = "red", selected = c(1))) %>%
            formatStyle(
                'status',
                target = 'row',
                backgroundColor = styleEqual(c("error", "success"), c('red', 'green')))
    })
    
    output$report_selected <- DT::renderDataTable({
        req(input$file)
        req(input$show_report_rows_selected)
        req(any(validation_summary$results$status == "error"))
        datatable({selected()},
                  rownames = FALSE,
                  filter = "top", 
                  extensions = 'Buttons',
                  options = list(
                      searchHighlight = TRUE,
                      scrollX = TRUE,
                      lengthChange = FALSE, 
                      pageLength = 5,
                      paging = TRUE,
                      searching = TRUE,
                      fixedColumns = TRUE,
                      autoWidth = TRUE,
                      ordering = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf')),
                  class = "display",
                  style = "bootstrap") %>% 
            formatStyle(
                if(any(validation_summary$results$status == "error")){
                    variables(validation_summary$rules[overview_table()[input$show_report_rows_selected, "name"]])  
                }
                else{NULL},
                backgroundColor =  'red'
            )
    })
    
    
    #Downloads ----
    output$download_certificate <- downloadHandler(
        filename = function() {"certificate.csv"},
        content = function(file) {write.csv(data.frame(time = Sys.time(), data = digest(dataset$data), rules = digest(validation_summary$rules), package_version = packageVersion("validate"), web_hash = digest(paste(sessionInfo(), Sys.time(), Sys.info()))), file, row.names = F)}
    )
    output$download_rules <- downloadHandler( 
        
        filename = function() {"rules.csv"},
        content = function(file) {write.csv(rules_example, file, row.names = F)}
    )
    output$download_sample <- downloadHandler(
        filename = function() {"data.csv"},
        content = function(file) {write.csv(data_example, file, row.names = F)}
    )
}

shinyApp(ui, server)