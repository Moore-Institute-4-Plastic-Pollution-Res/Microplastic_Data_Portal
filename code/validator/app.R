library(shiny)
#library(googlesheets4)
library(dplyr)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(validate)
library(digest)
library(data.table)
library(bs4Dash)
library(ckanr)
library(purrr)
library(shinyjs)
library(detector)

source("validation_function.R")

options(shiny.maxRequestSize = 30*1024^2)

api <- read.csv("secrets/ckan.csv")

ui <- dashboardPage(
        fullscreen = T,
        help = T,
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
        shinyjs::useShinyjs(),    
        tabItems(
            tabItem(
                tabName = "item1",
                box(
                    title = "Overview",
                    p("Welcome to the Data Validator webpage. This tool allows you to validate data interactively by uploading a dataset and rules file. To get started, go to the validator tab on the left."),
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = T,
                    p("You can help us build this tool!"),
                    HTML('<a class="btn btn-info" href = "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/Microplastic_Data_Portal" role = "button" >Github</a>'),
                    width = 12
                    
                )
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                    column(1,
                           popover(
                           fileInput("file_rules", NULL,
                                     placeholder = ".csv",
                                     buttonLabel = "Rules...",
                                     width = "100%",
                                     accept=c("text/csv",
                                              "text/comma-separated-values,text/plain")), 
                           title = "Upload rules",
                           content = "Upload the rules csv to use to validate the data csv")
                    ),
                    column(1, 
                           popover(
                           downloadButton("download_rules", "", style = "background-color: #2a9fd6;"), #%>%
                           title = "Download Rules Example",
                           content = "This is an example rules file, follow the format of this rules file to create your own."
                           )
                    ),
                    column(1,
                           popover(
                            disabled(
                           fileInput("file", NULL,
                                     placeholder = ".csv",
                                     buttonLabel = "Data...",
                                     multiple = T,
                                     accept=c("text/csv",
                                              "text/comma-separated-values,text/plain"))), #%>%
                           
                           title = "Upload CSV to validate",
                           content = "This can only be uploaded after the rules file. This is where you upload the csv file that you want to validate using the rules file."), 
                           #      size = "medium", rounded = TRUE
                    ),
                    column(1,
                           popover(
                           downloadButton("download_sample", "", style = "background-color: #dc3545;"), #%>%
                           
                           title = "Download invalid example data",
                           content = "This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that isn't 100% validated."
                           ),
                           popover(
                               downloadButton("download_good_sample", "", style = "background-color: #28a745;"), #%>%
                               
                               title = "Download validated example data",
                               content = "This is an example file that can be used in tandem with the example rules file to test out the tool with a dataset that is 100% validated."
                           )
                           #     type = "info", 
                           #     size = "medium", rounded = TRUE
                           # )
                    ),
                    column(8, uiOutput("certificate"), uiOutput("alert"))),
                fluidRow(
                    popover(
                           box(title = "Issues Raised", 
                               id = "issues_raised",
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
                               style = 'overflow-x: scroll',
                               maximizable = T,
                               width = 4
                               ),
                           title = "Issues Raised",
                           placement = "left",
                           content = "This is where the rules that are violated (or all rules if the advanced tool is turned on) show up. The table appears after data upload and is selectable which will query the issue selected box."),
                    popover(
                    box(title = "Issue Selected",
                        id = "issue_selected",
                           DT::dataTableOutput("report_selected"),
                        style = 'overflow-x: scroll',
                        maximizable = T,
                        width = 8
                    ),
                    title = "Issue Selected", 
                    placement = "left",
                    content = "This is where the selection in the issues raised box will show up. Whatever rule is selected will query the dataset and show any rows that violate the rule and show any problematic columns in red."
                    )
                    )
            )
        )
    )
)




server <- function(input, output, session) {
    
    rules_example <- read.csv("www/rules.csv")
    
    invalid_example <- read.csv("www/invalid_data.csv")
    
    success_example <- read.csv("www/data_success.csv")
    
    remote <- reactiveValues(creation = NULL, status = NULL)
    rules <- reactiveValues(rules = NULL, status = NULL)
    validation <- reactiveValues(data_formatted = NULL, report = NULL, results = NULL, rules = NULL, status = NULL)
    
    #Reading in rules in correct format -----
    observeEvent(input$file_rules, {
        enable("file")
        shinyjs::reset(id = "file")
        rules_file <- input$file_rules$datapath
        rules_output <- validate_rules(rules_file)
        validation <- reactiveValues(data_formatted = NULL, report = NULL, results = NULL, rules = NULL, status = NULL)
        if(rules_output$status == "error"){
            show_alert(
                title = rules_output$message$title,
                text = rules_output$message$text,
                type = rules_output$message$type)
            disable("file")
        }
        else{
            rules$rules <- rules_output$rules
            rules$status <- rules_output$status
        }
    })
    
    #Reading in data in correct format ----
    observeEvent(input$file, {
        req(input$file)
        data <- input$file$datapath
        updateBox("issue_selected", action = "restore")
        updateBox("issues_raised", action = "restore")
        validation_output <- validate_data(files_data = data, rules = rules$rules)
        if(validation_output$status == "error"){
            show_alert(
                title = validation_output$message$title,
                text  = validation_output$message$text,
                type  = validation_output$message$type)
        }
        else{
            validation$data_formatted <- validation_output$data_formatted
            validation$report <- validation_output$report
            validation$results <- validation_output$results
            validation$rules <- validation_output$rules
            validation$status <- validation_output$status
        }
    })
    
    observeEvent(all(validation$results$status != "error"), {
        if("KEY" %in% names(validation$data_formatted)){
            remote_output <- remote_share(data_formatted = validation$data_formatted, 
                                          api = api, 
                                          rules = validation$rules, 
                                          results = validation$rules)
            if(remote_output$share == "error"){
                show_alert(
                    title = remote_output$message$title,
                    text  = remote_output$message$text,
                    type  = remote_output$message$type)
            }
            else{
                remote$creation <- remote_output$creation
                remote$status <- remote_output$status
            }
            }
        }
    )
    
    overview_table <- reactive({
        req(input$file)
        req(validation$data_formatted)
        req(validation$results)
        rules_broken(results = validation$results, show_decision = input$show_decision)
    })
    
    selected <- reactive({
        req(input$file)
        req(validation$data_formatted)
        req(input$show_report_rows_selected)
        req(validation$results)
        rows_for_rules(data_formatted = validation$data_formatted, report = validation$report, broken_rules = overview_table(), rows = input$show_report_rows_selected)
    })
    
    
    output$certificate <- renderUI({
        req(input$file)
        req(input$file_rules)
        req(validation$results)
        all(validation$results$status != "error")
        #req(dataset$creation)
        
        if(all(validation$results$status != "error") & !is.null(input$file)){
            downloadButton("download_certificate", "Download Certificate", style = "background-color: #2a9fd6; width: 100%;")
        }
        else{
            NULL
        }
    })
    
    
    output$alert <- renderUI({
        req(input$file)
        req(input$file_rules)
        req(validation$results)
        if(any(validation$results$status == "error")){
            HTML('<button type="button" class="btn btn-danger btn-lg btn-block">ERROR</button>')
        }
        else if(!is.null(validation$data_formatted) & !is.null(input$file)){
            HTML('<button type="button" class="btn btn-success btn-lg btn-block">SUCCESS</button>')
        }
        else{
            NULL
        }
    })
    
    #Report tables ----
    output$show_report <- DT::renderDataTable({
        req(input$file)
        req(validation$data_formatted)
        req(nrow(overview_table()) > 0)
        #req(any(validation_summary$results$status == "error"))
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
        req(validation$data_formatted)
        req(input$show_report_rows_selected)
        req(any(validation$results$status == "error"))
        req(nrow(selected()) > 0)
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
                if(any(validation$results$status == "error")){
                    variables(validation$rules[overview_table()[input$show_report_rows_selected, "name"]])  
                }
                else{NULL},
                backgroundColor =  'red'
            )
    })
    
    
    #Downloads ----
    output$download_certificate <- downloadHandler(
        filename = function() {"certificate.csv"},
        content = function(file) {write.csv(data.frame(time = Sys.time(), 
                                                       data = digest(validation$data_formatted), 
                                                       link = if(!is.null(remote$creation)){remote$creation$url} else{NA}, 
                                                       rules = digest(validation$rules), 
                                                       package_version = packageVersion("validate"), 
                                                       web_hash = digest(paste(sessionInfo(), 
                                                                               Sys.time(), 
                                                                               Sys.info()))), 
                                            file, row.names = F)}
    )
    output$download_rules <- downloadHandler( 
        filename = function() {"rules.csv"},
        content = function(file) {write.csv(rules_example, file, row.names = F)}
    )
    output$download_sample <- downloadHandler(
        filename = function() {"invalid_data.csv"},
        content = function(file) {write.csv(invalid_example, file, row.names = F)}
    )
    output$download_good_sample <- downloadHandler(
        filename = function() {"valid_data.csv"},
        content = function(file) {write.csv(success_example, file, row.names = F)}
    )
}

shinyApp(ui, server)