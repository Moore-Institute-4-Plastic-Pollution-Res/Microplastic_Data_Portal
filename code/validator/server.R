function(input, output, session) {
    
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
        if(rules_output$status == "success"){
            rules$rules <- rules_output$rules
            rules$status <- rules_output$status            
        }
        else{
            show_alert(
                title = rules_output$message$title,
                text = rules_output$message$text,
                type = rules_output$message$type)
            disable("file")
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
    
    observeEvent(validation$results$status == "success", {
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
        if(validation$results$status == "success" && !is.null(validation$results$status)){
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
        if(!is.null(validation$results$status) && validation$results$status == "success"){
            HTML('<button type="button" class="btn btn-success btn-lg btn-block">SUCCESS</button>')
        }
        else if(!is.null(validation$data_formatted) & !is.null(input$file)){
            HTML('<button type="button" class="btn btn-danger btn-lg btn-block">ERROR</button>')
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