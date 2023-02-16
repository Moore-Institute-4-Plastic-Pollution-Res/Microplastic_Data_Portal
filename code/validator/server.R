function(input, output, session) {

    rules <- reactive({
        #req(input$file_rules | input$rules_selection == "Microplastic Acc. DW.")
        if(!isTruthy(config$rules)){
            file_rules = input$file_rules$datapath
        }
        else if(length(config$rules) == 1){
            file_rules = config$rules
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
    
    output$rules_upload <- renderUI({
        if(!isTruthy(config$rules)){
            popover(
                fileInput("file_rules", NULL,
                          placeholder = ".csv",
                          buttonLabel = "Rules...",
                          width = "100%",
                          accept=c("text/csv",
                                   "text/comma-separated-values,text/plain")),
                title = "Upload rules",
                content = "Upload the rules csv to use to validate the data csv"
            )    
        }
        else{
            NULL
        }
    })
    
    output$error_query <- renderUI({
        req(input$file)
        req(validation()$data_formatted)
        req(validation()$results)
        
        lapply(1:length(validation()$data_formatted), function(x){
            
            #Tables calculations ----
            #selected <- rows_for_rules(data_formatted = validation()$data_formatted[[x]], report = validation()$report[[x]], broken_rules = rules_broken(results = validation()$results[[x]], show_decision = input[[paste0("show_decision", x)]]), rows = input[[paste0("show_report", x, "_rows_selected")]]) 
            #overview_table <- rules_broken(results = validation()$results[[x]], show_decision = input[[paste0("show_decision", x)]])
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
                #req(input[[paste0("show_report", x, "_rows_selected")]])
                #req(any(validation()$results[[x]]$status == "error"))
                #req(nrow(selected) > 0)
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
                            DT::dataTableOutput(paste0("report_selected", x)),
                            style = 'overflow-x: scroll',
                            maximizable = T,
                            width = 6
                        )
                    ),
                width = 12
            )
                #popover(
                #    box(title = "Issue Selected",
                #        id = paste0("issue_selected", x),
                #        DT::dataTableOutput(paste0("report_selected", x)),
                #        style = 'overflow-x: scroll',
                #        maximizable = T,
                #        width = 8
                #    ),
                #    title = "Issue Selected",
                #    placement = "left",
                #    content = "This is where the selection in the issues raised box will show up. Whatever rule is selected will query the dataset and show any rows that violate the rule and show any problematic columns in red."
                #)
            #)
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
        #req("KEY" %in% names(validation()$data_formatted))
        req(vals$key)
        remote_share(validation = validation(),
                     data_formatted = validation()$data_formatted, 
                     verified = vals$key,
                     api = config$ckan, 
                     rules = read.csv(rules()), 
                     results = validation()$results)
    })
    
    
    output$certificate <- renderUI({
        req(validation()$data_formatted)
        if(isTRUE(all(!validation()$issues))){
            downloadButton("download_certificate", "Download Certificate", style = "background-color: #2a9fd6; width: 100%;")
        }
        else{
            NULL
        }
    })
    

    output$alert <- renderUI({
        #req(input$file)
        #req(input$file_rules)
        req(validation()$results)
        if(isTRUE(!any(validation()$issues))){
            HTML('<button type="button" class="btn btn-success btn-lg btn-block">SUCCESS</button>')
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
            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK")
            )
        )
    }
    
    observeEvent(req(isTRUE(!any(validation()$issues))), {
        showModal(dataModal())
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
        # Check that data object exists and is data frame.
        test_valid <- read.csv("secrets/ckan.csv") %>%
            filter(VALID_KEY == input$secret & VALID_RULES == digest(read.csv(rules())))
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