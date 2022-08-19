library(shiny)
library(prompter)
library(dplyr)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(validate)
library(digest)
library(data.table)

options(shiny.maxRequestSize = 30*1024^2)


containerfunction <- function(...) {
        div(class = "jumbotron jumbotron-fluid",
            style = "border:solid #f7f7f9;background-color:rgba(0, 0, 0, 0.5)",
            align = "justify", ... )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cyborg"),
    
    # Application title
    titlePanel("Data Validator"),
    
    fluidRow(
        column(2,
               fileInput("file_rules", NULL,
                         placeholder = "rules .csv",
                         buttonLabel = "Rules...",
                         accept=c("text/csv",
                                  "text/comma-separated-values,text/plain")), 
               p(
                   downloadButton("download_rules", "Rules File Example", style = "background-color: #2a9fd6;")
               )
               ),
        column(2,
               fileInput("file", NULL,
                         placeholder = "data .csv",
                         buttonLabel = "Data...",
                         accept=c("text/csv",
                                  "text/comma-separated-values,text/plain")) %>%
                   add_prompt(
                       message = "Upload your csv spreadsheet for validation",
                       type = "info", 
                       size = "medium", rounded = TRUE
                   ),
               p(
                   downloadButton("download_sample", "Data File Example", style = "background-color: #2a9fd6;")
               )),
        column(8, uiOutput("certificate"), uiOutput("alert"))),

    fluidRow(
        column(4,
               
               containerfunction(h3("Issues Raised"), 
                                 prettySwitch("show_decision",
                                              label = "Show errors and warnings only?",
                                              inline = T,
                                              value = T,
                                              status = "success",
                                              fill = T),
                                 DT::dataTableOutput("show_report"))),
        column(8,
               containerfunction(h3("Issue Selected"), DT::dataTableOutput("report_selected"))
               
               
        )
    ),
    fluidRow(
        hr(),
        p(align = "center", 
            HTML('<a class="btn btn-info" href = "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/Microplastic_Data_Portal" role = "button" >Open Code</a>')
            )
    )
)   

# Define server logic required to draw a histogram
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

# Run the application 
shinyApp(ui = ui, server = server)