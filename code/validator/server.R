function(input, output, session) {

    rules <- reactive({
        if(!isTruthy(config$rules_to_use)){
            file_rules = input$file_rules$datapath
        }
        else if(length(config$rules_to_use) == 1){
            file_rules = config$rules_to_use
        }
        else{
            file_rule = NULL
        }
        file_rules
    })
    
    validation <- reactive({
        req(input$file)
        req(rules())
        validate_data(files_data = input$file$datapath, data_names = input$file$name, file_rules = rules())
    })

    output$error_query <- renderUI({
        req(input$file)
        req(validation()$data_formatted)
        req(validation()$results)
        
        lapply(1:length(validation()$data_formatted), function(x){
            #Report tables to view ----
            output[[paste0("show_report", x)]] <- DT::renderDataTable({
                #req(nrow(overview_table) > 0)
                datatable({rules_broken(results = validation()$results[[x]], show_decision = input[[paste0("show_decision", x)]]) %>%
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
                        #style = "bootstrap", 
                        selection = list(mode = "single", color = "red")) %>%
                    formatStyle(
                        'status',
                        target = 'row',
                        backgroundColor = styleEqual(c("error", "success"), c('red', 'green')))
            })
            
            output[[paste0("report_selected", x)]] <- DT::renderDataTable({
                if(isTruthy(input[[paste0("show_report", x, "_rows_selected")]])){
                    datatable({rows_for_rules(data_formatted = validation()$data_formatted[[x]], report = validation()$report[[x]], broken_rules = rules_broken(results = validation()$results[[x]], show_decision = input[[paste0("show_decision", x)]]), rows = input[[paste0("show_report", x, "_rows_selected")]]) },
                              rownames = FALSE,
                              escape = FALSE,
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
                              #style = "bootstrap",
                              class = "display") %>% 
                        formatStyle(
                            if(any(validation()$results[[x]]$status == "error")){
                                variables(validation()$rules[[x]][rules_broken(results = validation()$results[[x]], show_decision = input[[paste0("show_decision", x)]])[input[[paste0("show_report", x, "_rows_selected")]], "name"]])  
                            }
                            else{NULL},
                            backgroundColor =  'red'
                        )
                }
                else{
                    datatable({validation()$data_formatted[[x]]},
                              rownames = FALSE,
                              escape = FALSE,
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
                              #style = "bootstrap",
                              class = "display")
                }
                
            })
            box(title = paste0(validation()$data_names[[x]]),
                id = paste0(validation()$data_names[[x]]),
                collapsed = T,
                background = if(validation()$issues[[x]]){"danger"}else{"success"},
                fluidRow(
                    box(title = "Issues Raised",
                        id = paste0("issues_raised", x),
                        background = "white",
                        dropdownMenu = boxDropdown(
                            boxDropdownItem(
                                prettySwitch(paste0("show_decision", x),
                                             label = "Errors only?",
                                             inline = T,
                                             value = T,
                                             status = "success",
                                             fill = T))
                        ),
                        DT::dataTableOutput(paste0("show_report", x)),
                        style = 'overflow-x: scroll',
                        maximizable = T,
                        width = 6
                    ), 
                    box(title = "Issue Selected",
                            id = paste0("issue_selected", x),
                            background = "white",
                            DT::dataTableOutput(paste0("report_selected", x)),
                            style = 'overflow-x: scroll',
                            maximizable = T,
                            width = 6
                        )
                    ),
                width = 12
            )
            }
        )
    })
    
    output$rules_dt <- DT::renderDataTable({
        if(grepl("(\\.csv$)", ignore.case = T, as.character(rules()))){
            rules <- read.csv(rules())
        }
        
        if(grepl("(\\.xlsx$)", ignore.case = T, as.character(rules()))){
            rules <- read_excel(rules())
        }
        datatable({rules},
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
                style = "bootstrap")
    })
    
    remote <- reactive({
        req(validation()$data_formatted)
        req(isTRUE(!any(validation()$issues)))
        req(vals$key)
        req(config$s3_secret_key)
        req(config$mongo_key)
        req(config$ckan_key)
        
        remote_share(validation = validation(),
                     data_formatted = validation()$data_formatted, 
                     verified = vals$key, 
                     valid_rules = config$valid_rules, 
                     valid_key = config$valid_key, 
                     ckan_url = config$ckan_url, 
                     ckan_key = config$ckan_key, 
                     ckan_package = config$ckan_package, 
                     url_to_send = config$url_to_send, 
                     rules = read.csv(rules()), 
                     results = validation()$results, 
                     bucket = config$s3_bucket, 
                     mongo_key = config$mongo_key, 
                     old_cert = input$old_certificate$datapath)
    })
    
    output$dev_options <- renderUI({
        req(config$dev)
        tagList(
            fluidRow(
                popover(
                    box(
                        title = "Rules File",
                        collapsed = T,
                        width = 12,
                        DT::dataTableOutput("rules_dt"),
                        style = 'overflow-x: scroll'
                    ),
                    title = "Rules File",
                    placement = "bottom",
                    content = "Backend file of rules currently in use.")
            ),    
            fluidRow(
                popover(
                    box(
                        title = "Diagnose",
                        collapsed = T,
                        width = 12,
                        jsoneditOutput("remote_out"),
                        jsoneditOutput("validation_out")#,
                    ),
                    title = "Diagnose",
                    placement = "bottom",
                    content = "For Developmental & Debugging Purposes")
            )
        )
    })

    output$alert <- renderUI({
        req(validation()$results)
        if(isTRUE(!any(validation()$issues))){
            downloadButton("download_certificate", "SUCCESS", style = "background-color: #28a745; width: 100%;")
            
            #HTML('<button type="button" class="btn btn-success btn-lg btn-block">SUCCESS</button>')
        }
        else if(isTRUE(any(validation()$issues))){
            HTML('<button type="button" class="btn btn-danger btn-lg btn-block">ERROR</button>')
        }
        else{
            NULL
        }
    })
    
    #Downloads ----
    output$download_certificate <- downloadHandler(
        filename = function() {"certificate.csv"},
        content = function(file) {write.csv(
                                        certificate_df(x = validation()), 
                                                        file, 
                                                        row.names = F)}
    )
    
    output$download_rules_excel <- downloadHandler(
        filename = function() {"rules.xlsx"},
        content = function(file) {saveWorkbook(create_valid_excel(file_rules = rules()), file, TRUE)}
    )
    
    output$download_rules <- downloadHandler( 
        filename = function() {"rules.csv"},
        content = function(file) {write.csv(read.csv(config$rules_example), file, row.names = F)}
    )
    
    output$download_sample <- downloadHandler(
        filename = function() {"invalid_data.zip"},
        content = function(file) {
            zip(file, unzip(config$invalid_data_example))
            }
    )
    
    output$download_good_sample <- downloadHandler(
        filename = function() {"valid_data.zip"},
        content = function(file) {
            zip(file, unzip(config$valid_data_example))
            }
    )
    
    #Alerts ----
    observe({
        if(is.list(validation()$message)){
            show_alert(
                title = validation()$message$title,
                text  = validation()$message$text,
                type  = validation()$message$type)
        }
        if(is.list(remote()$message)){
            show_alert(
                title = remote()$message$title,
                text  = remote()$message$text,
                type  = remote()$message$type)
        }
    })
    
    # reactiveValues object for storing current data set.
    vals <- reactiveValues(key = NULL)
    
    #Secret Key Input ----
    dataModal <- function(failed = FALSE) {
        modalDialog(
            textInput("secret", "Input Key"),
            span('To share the uploaded data to the database you need to provide a key shared with you by', config$contact, '.'),
            if (failed)
                div(tags$b("Invalid key-rules pair please try again or contact", config$contact, "for help.", style = "color: red;")),
            p(),
            box(title = "Is this an update to a previous submission?", 
                id = "update_submission",
                width = 12,
                collapsed = T,
                fileInput(inputId = "old_certificate", label = "Upload previous certificate.")
            ),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK")
            )
        )
    }
    
    observeEvent(req(isTRUE(!any(validation()$issues)), validation()$data_formatted, config$ckan, config$s3_secret_key, config$mongo_key), {
        showModal(dataModal())
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
        # Check that data object exists and is data frame.
        test_valid <- config$ckan %>%
            filter(ckan_valid_key == input$secret & ckan_valid_rules == digest(read.csv(rules())))
        if (!is.null(input$secret) && nrow(test_valid >= 1) && input$ok < 4){
            vals$key <- input$secret
            removeModal()
            show_alert(
                title = "Success Logging In",
                text  = "Your key is valid and you are now logged in.",
                type  = "success")
        } else {
            showModal(dataModal(failed = TRUE))
        }
    })
    
    #Diagnosis ----
    output$validation_out <- renderJsonedit({
        jsonedit(validation())
    })
    output$remote_out <- renderJsonedit({
        jsonedit(input$file)
    })
    
    
}