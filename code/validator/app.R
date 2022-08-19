library(shiny)
library(prompter)
library(readxl)
library(dplyr)
library(DT)
library(shinythemes)
library(data.validator)
library(assertr)
library(shinyWidgets)
library(validate)
library(digest)


between <- function(a, b) {
    function(x) { a <= x && x <= b }
}

#May want to shift to https://cran.r-project.org/web/packages/validate/vignettes/cookbook.html#52_Group_properties, seems more thorough than the data.validator

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cyborg"),
    
    # Application title
    titlePanel("Data Validator"),
    div(align = "right", 
        HTML('<a class="btn btn-info" href = "https://github.com/wincowgerDEV/waterpact" role = "button" >Open Code</a>')),
    
    fluidRow(
        column(4, 
               fileInput("file", NULL,
                         placeholder = ".xlsx",
                         accept=c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".xlsx")) %>%
                   add_prompt(
                       message = "Upload your Excel spreadsheet for validation",
                       type = "info", 
                       size = "medium", rounded = TRUE
                   )),
        column(8, uiOutput("certificate"), uiOutput("alert"))),
    fluidRow(
        column(3, prettySwitch("show_decision",
                               label = "Show errors and warnings only?",
                               inline = T,
                               value = T,
                               status = "success",
                               fill = T))
    ),
    fluidRow(
        column(4, 
               DT::dataTableOutput("show_report")),
        column(8,
               DT::dataTableOutput("report_selected")
               
               
        )
    )
)   

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    dataset <- reactiveValues(sampledata = NULL, equipmentdata = NULL)
    
    #report <- reactiveValues(data = NULL)
    
    validation_summary <- reactiveValues(results = NULL, report = NULL, rules = NULL)
    
    observeEvent(input$file, {
        # Read in data when uploaded based on the file type
        req(input$file)
        file <- input$file$datapath
        
        if (!grepl("(\\.xlsx$)",
                   ignore.case = T, as.character(file))) {
            #reset("file")
            dataset$sampledata <- NULL
            dataset$equipmentdata <- NULL
            validation_summary$rules <- NULL
            validation_summary$report <- NULL
            validation_summary$results <- NULL
            show_alert(
                title = "Data type not supported!",
                text = paste0("Uploaded data type is not currently supported; please
                      upload a .xlsx file."),
                type = "warning")
            #return(NULL)
        }
        else if (!all(c("SampleData", "EquipmentData") %in% excel_sheets(file))){
            #reset("file")
            dataset$sampledata <- NULL
            dataset$equipmentdata <- NULL
            validation_summary$rules <- NULL
            validation_summary$report <- NULL
            validation_summary$results <- NULL
            show_alert(
                title = "Data type not supported!",
                text = paste0("Uploaded data type does not have required sheets 'SampleData' and 'EquipmentData'."),
                type = "warning")
        }
        else{
            dataset$sampledata <- readxl::read_xlsx(file, sheet = "SampleData")
            dataset$equipmentdata <- readxl::read_xlsx(file, sheet = "EquipmentData")
            
            rules <- read.csv("www/rules.csv")
            
            validation_summary$rules <- validator(.data=rules)
            
            validation_summary$report <- confront(dataset$sampledata, validation_summary$rules)
            
            validation_summary$results <- summary(validation_summary$report) %>%
                mutate(status = ifelse(fails > 0 | error | warning , "error", "success")) %>%
                mutate(description = meta(validation_summary$rules)$description)
            
            #print(report$data)
            #print(validation_summary$data)
            #print(any(validation_summary$data$status == "error"))
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
        violating(dataset$sampledata, validation_summary$report[overview_table()[input$show_report_rows_selected, "name"]])
        #print(unlist(overview_table()[input$show_report_rows_selected,"name"]))
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
            HTML('<button type="button" class="btn btn-danger btn-lg btn-block" disabled>ERROR</button>')
        }
        else{
            HTML('<button type="button" class="btn btn-success btn-lg btn-block" disabled>SUCCESS</button>')
        }
    })
    
    
    output$show_report <- DT::renderDataTable({
        req(input$file)
        req(any(validation_summary$results$status == "error"))
        datatable({overview_table()},
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
                  filter = "top", caption = "Issues Raised",
                  style = "bootstrap", 
                  selection = list(mode = "single", selected = c(1))) %>%
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
                  caption = "Issue Selected", 
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
    
    output$table <- DT::renderDataTable({
        req(input$file)
        datatable(dataset$sampledata,
                  options = list(searchHighlight = TRUE,
                                 scrollX = TRUE,
                                 sDom  = '<"top">lrt<"bottom">ip',
                                 lengthChange = FALSE, pageLength = 5),
                  rownames = FALSE,
                  filter = "top", caption = "Sample Data View",
                  style = "bootstrap")
    })
    
    output$equipmenttable <- DT::renderDataTable({
        req(input$file)
        datatable(dataset$equipmentdata,
                  options = list(searchHighlight = TRUE,
                                 scrollX = TRUE,
                                 sDom  = '<"top">lrt<"bottom">ip',
                                 lengthChange = FALSE, pageLength = 5),
                  rownames = FALSE,
                  filter = "top", caption = "Equipment Data View",
                  style = "bootstrap")
    })
    
    #Downloads ----
    output$download_certificate <- downloadHandler(
        filename = function() {"certificate.csv"},
        content = function(file) {write.csv(data.frame(sampledata = digest(dataset$sampledata), equipmentdata = digest(dataset$equipmentdata), web_hash = digest(paste(sessionInfo(), Sys.time(), Sys.info()))), file)}
    )
}

# Run the application 
shinyApp(ui = ui, server = server)