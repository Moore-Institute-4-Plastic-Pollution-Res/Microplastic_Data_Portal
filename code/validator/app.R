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
                           fileInput("file", NULL,
                                     placeholder = ".csv",
                                     buttonLabel = "Data...",
                                     multiple = T,
                                     accept=c("text/csv",
                                              "text/comma-separated-values,text/plain")), #%>%
                           
                           title = "Upload CSV to validate",
                           content = "This is where you upload the csv file that you want to validate using the rules file."), 
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
    
    data_example <- read.csv("www/Samples.csv")
    
    success_example <- read.csv("www/data_success.csv")
    
    api_info <- reactiveValues(data = NULL)
    
    dataset <- reactiveValues(data = NULL, creation = NULL)
    
    validation_summary <- reactiveValues(results = NULL, report = NULL, rules = NULL)
    
    #Reading in rules in correct format -----
    observeEvent(input$file_rules, {
        file_rules <- input$file_rules$datapath
        if (!grepl("(\\.csv$)", ignore.case = T, as.character(file_rules))) {
            #reset("file")
            dataset$data <- NULL
            api_info$data <- NULL
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
            rules <- read.csv(input$file_rules$datapath, fileEncoding = "UTF-8")
            
            if (!all(c("name", "description", "severity", "rule") %in% names(rules))) {
                #reset("file")
                dataset$data <- NULL
                api_info$data <- NULL
                validation_summary$rules <- NULL
                validation_summary$report <- NULL
                validation_summary$results <- NULL
                show_alert(
                    title = "Data type not supported!",
                    text = paste0('Uploaded rules format is not currently supported, please provide a rules file with column names, "name", "description", "severity", "rule"'),
                    type = "warning")
                #return(NULL)
            }
            else if (!all(unlist(lapply(rules, class)) %in% "character")) {
                #reset("file")
                dataset$data <- NULL
                api_info$data <- NULL
                validation_summary$rules <- NULL
                validation_summary$report <- NULL
                validation_summary$results <- NULL
                show_alert(
                    title = "Data type not supported!",
                    text = paste0('Uploaded rules format is not currently supported, please provide a rules file with columns that are all character type.'),
                    type = "warning")
                #return(NULL)
            }
            else{
                validation_summary$rules <- validator(.data=rules)
            }
        }
        
    })
    
    #Reading in data in correct format ----
    observeEvent(input$file, {
        req(input$file)
        file <- input$file$datapath
        updateBox("issue_selected", action = "restore")
        updateBox("issues_raised", action = "restore")
        
        
        # Read in data when uploaded based on the file type
        if (!all(grepl("(\\.csv$)", ignore.case = T, as.character(file)))) {
            dataset$data <- NULL
            api_info$data <- NULL
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
            api_info$data <- NULL
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
            if(length(file) == 1){
                dataset$data <- read.csv(file, fileEncoding = "UTF-8")
            }
            else{
                sout <- tryCatch(lapply(files, function(file) read.csv(file, fileEncoding = "UTF-8")) %>% 
                                             reduce(full_join),
                    warning = function(w) {w}, error = function(e) {e})
                
                if (inherits(sout, "simpleWarning") | inherits(sout, "simpleError")){
                    show_alert(
                        title = "Something went wrong with the merge.",
                        text = paste0("This tool expects at least one column in each dataset with the same name to merge on. There was an error that said ", sout$message),
                        type = "error"
                    )
                }
                else{
                    dataset$data <- sout
                }
            }
            if(!all(variables(validation_summary$rules) %in% names(dataset$data))){
                dataset$data <- NULL
                api_info$data <- NULL
                validation_summary$rules <- NULL
                validation_summary$report <- NULL
                validation_summary$results <- NULL
                show_alert(
                    title = "Rules and data mismatch",
                    text = paste0("All variables in the rules csv (", paste(variables(validation_summary$rules), collapse = "-"), ") need to be in data csv (",  paste(names(dataset$data), collapse = "-"), ") for the validation to work."),
                    type = "warning")
            }
            else{
                if (any(!names(dataset$data) %in% variables(validation_summary$rules))){
                    #dataset$data <- NULL
                    #validation_summary$rules <- NULL
                    #validation_summary$report <- NULL
                    #validation_summary$results <- NULL
                    show_alert(
                        title = "Rules and data mismatch",
                        text = paste0("All variables in the data csv (", paste(names(dataset$data), collapse = "-"), ") should probably be in rules csv (",  paste(variables(validation_summary$rules), collapse = "-"), ") for best data validation, but validation may proceed."),
                        type = "warning")
                }
                #Check for valid api key and format the api if it is valid. This needs to be a bulletproof firewall so lots of checks and even adding additional rules. 
                if("KEY" %in% names(dataset$data)){
                    if(length(unique(dataset$data$KEY)) == 1){
                        if(unique(dataset$data$KEY) %in% api$VALID_KEY){
                            if(any(unlist(lapply(dataset$data %>% select(-KEY), function(x) any(x %in% unique(dataset$data$KEY)))))){
                                dataset$data <- NULL
                                api_info$data <- NULL
                                validation_summary$rules <- NULL
                                validation_summary$report <- NULL
                                validation_summary$results <- NULL
                                show_alert(
                                    title = "Secret Key is misplaced",
                                    text = "The secret key is in locations other than the KEY column, please remove the secret key from any other locations.",
                                    type = "error")
                            }
                            else{
                            api_info$data <- api %>%
                                filter(VALID_KEY == unique(dataset$data$KEY))
                            ckanr_setup(url = api_info$data$URL, key = api_info$data$KEY) 
                            }
                        }
                    }
                }
                if(!is.null(dataset$data)){
                    validation_summary$report <- confront(dataset$data, validation_summary$rules)
                    
                    validation_summary$results <- summary(validation_summary$report) %>%
                        mutate(status = ifelse(fails > 0 | error | warning , "error", "success")) %>%
                        mutate(description = meta(validation_summary$rules)$description)    
                }
            }
            
            
        }
    })
    
    observeEvent(req(all(validation_summary$results$status != "error"), dataset$data, validation_summary$rules, validation_summary$results$status), {
        updateBox("issue_selected", action = "remove")
        if(!is.null(api_info$data)){
            hashed_data <- digest(dataset$data)
            hashed_rules <- digest(validation_summary$rules)
            package_version <- packageVersion("validate")
            file <- tempfile(pattern = "data", fileext = ".csv")
            #dataset
            write.csv(dataset$data %>% select(-KEY), file, row.names = F)
            dataset$creation <- resource_create(package_id = api_info$data$PACKAGE,
                                                description = "validated raw data upload to microplastic data portal",
                                                name = paste0("data_", hashed_data),
                                                upload = file)
        }
        }
    )
    
    overview_table <- reactive({
        req(input$file, dataset$data)
        validation_summary$results %>%
            filter(if(input$show_decision){status == "error"} else{status %in% c("error", "success")}) %>%
            select(description, status, name, expression, everything())
    })
    
    selected <- reactive({
        req(input$file, dataset$data)
        req(input$show_report_rows_selected)
        violating(dataset$data, validation_summary$report[overview_table()[input$show_report_rows_selected, "name"]])
    })
    
    
    output$certificate <- renderUI({
        req(file)
        req(validation_summary$results)
        req(dataset$creation)
        
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
        else if(!is.null(dataset$creation)){
            HTML('<button type="button" class="btn btn-success btn-lg btn-block">SUCCESS</button>')
        }
        else{
            NULL
        }
    })
    
    #Report tables ----
    output$show_report <- DT::renderDataTable({
        req(input$file)
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
        req(input$show_report_rows_selected)
        req(any(validation_summary$results$status == "error"))
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
        content = function(file) {write.csv(data.frame(time = Sys.time(), 
                                                       data = digest(dataset$data), 
                                                       link = if(!is.null(dataset$creation)){dataset$creation$url}else{NULL}, 
                                                       rules = digest(validation_summary$rules), 
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
        content = function(file) {write.csv(data_example, file, row.names = F)}
    )
    output$download_good_sample <- downloadHandler(
        filename = function() {"valid_data.csv"},
        content = function(file) {write.csv(success_example, file, row.names = F)}
    )
}

shinyApp(ui, server)