function(input, output, session) {

    
    
    validation <- reactive({
        req(input$file)
        req(input$file_rules)
        validate_data(files_data = input$file$datapath, file_rules = input$file_rules$datapath)
    })
    
    remote <- reactive({
        req(all(validation()$results$status == "success"))
        req("KEY" %in% names(validation()$data_formatted))
        api <- read.csv("secrets/ckan.csv")
        remote_share(data_formatted = validation()$data_formatted, 
                     api = api, 
                     rules = validation()$rules, 
                     results = validation()$rules)
    })
    
    overview_table <- reactive({
        req(input$file)
        req(validation()$data_formatted)
        req(validation()$results)
        rules_broken(results = validation()$results, show_decision = input$show_decision)
    })
    
    selected <- reactive({
        req(input$file)
        req(validation()$data_formatted)
        req(input$show_report_rows_selected)
        req(validation()$results)
        rows_for_rules(data_formatted = validation()$data_formatted, report = validation()$report, broken_rules = overview_table(), rows = input$show_report_rows_selected)
    })
    
    output$certificate <- renderUI({
        if(all(validation()$results$status == "success") && !is.null(validation()$results$status)){
            downloadButton("download_certificate", "Download Certificate", style = "background-color: #2a9fd6; width: 100%;")
        }
        else{
            NULL
        }
    })
    
    output$alert <- renderUI({
        req(input$file)
        req(input$file_rules)
        req(validation()$results)
        if(!is.null(validation()$results$status) && all(validation()$results$status == "success")){
            HTML('<button type="button" class="btn btn-success btn-lg btn-block">SUCCESS</button>')
        }
        else if(!is.null(validation()$data_formatted) & !is.null(input$file)){
            HTML('<button type="button" class="btn btn-danger btn-lg btn-block">ERROR</button>')
        }
        else{
            NULL
        }
    })
    
    #Report tables ----
    output$show_report <- DT::renderDataTable({
        req(input$file)
        req(validation()$data_formatted)
        req(nrow(overview_table()) > 0)
        #req(any(validation_summary$results$status == "error"))
        datatable({overview_table() %>%
                select(description, status, expression, name) %>%
                mutate(description = as.factor(description))},
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
        req(validation()$data_formatted)
        req(input$show_report_rows_selected)
        req(any(validation()$results$status == "error"))
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
                if(any(validation()$results$status == "error")){
                    variables(validation()$rules[overview_table()[input$show_report_rows_selected, "name"]])  
                }
                else{NULL},
                backgroundColor =  'red'
            )
    })
    
    
    #Downloads ----
    output$download_certificate <- downloadHandler(
        filename = function() {"certificate.csv"},
        content = function(file) {write.csv(data.frame(time = Sys.time(), 
                                                       data = digest(validation()$data_formatted), 
                                                       link = if(!is.null(remote()$creation)){remote()$creation$url} else{NA}, 
                                                       rules = digest(validation()$rules), 
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
    
    #Alerts ----
    observe({
        if(validation()$status == "error" | is.list(validation()$message)){
            show_alert(
                title = validation()$message$title,
                text  = validation()$message$text,
                type  = validation()$message$type)
        }
        if(remote()$share == "error"){
            show_alert(
                title = remote()$message$title,
                text  = remote()$message$text,
                type  = remote()$message$type)
        }
    })
    
    #Diagnosis ----
    output$validation_out <- renderJsonedit({
        jsonedit(validation())
    })
    output$remote_out <- renderJsonedit({
        jsonedit(remote())
    })
    
    
}