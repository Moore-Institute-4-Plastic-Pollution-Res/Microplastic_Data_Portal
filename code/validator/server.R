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
        validate_function <- function(){
            data_names <- input$file$name[!grepl(".zip$", input$file$name)]
            validate_data(
                files_data = gsub("\\\\", "/", input$file$datapath[!grepl(".zip$", input$file$datapath)]), 
                data_names = if(all(grepl(".csv", data_names))){data_names} else{NULL},
                zip_data = if(any(grepl(".zip$", input$file$datapath))){gsub("\\\\", "/",input$file$datapath[grepl(".zip$", input$file$datapath)])} else{NULL}, 
                file_rules = rules()
            )
        }
        tryCatch({
            validate_function()
        }, warning = function(w) {
            shinyWidgets::show_alert(
                title = "Warning During Validation", 
                type = "warning", 
                text = paste0("Warning: ", w$message)
            )
            return(validate_function())
        }, error = function(e) {
            shinyWidgets::show_alert(
                title = "Error During Validation", 
                type = "error", 
                text = paste0("Error: ", e$message)
            )
            return(NULL)
        }, message = function(m) {
            shinyWidgets::show_alert(
                title = "Message During Validation", 
                type = "info", 
                text = paste0("Message: ", m$message)
            )
            return(validate_function())
        })
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
                            sScrollY = '50vh', 
                            scrollCollapse = TRUE,
                            lengthChange = FALSE, 
                            #pageLength = 5,
                            paging = FALSE,
                            searching = TRUE,
                            fixedColumns = TRUE,
                            autoWidth = FALSE,
                            ordering = TRUE,
                            dom = 'Bfrtip',
                            buttons = list('csv', 'excel', 'pdf', list(extend= "colvisGroup",text="Show All",show=":hidden")),
                            columnDefs = list(list(visible=FALSE, targets=c(2,3)))),
                        rownames = FALSE,
                        filter = "top", 
                        #style = "bootstrap", 
                        selection = list(mode = "single", color = "#FF817e")) %>%
                    formatStyle(
                        'status',
                        target = 'row',
                        backgroundColor = styleEqual(c("error", "warning", "success"), c('#FF817e', '#FFFFAD', 'white')))
            })
            
            rows_for_rules_selected <- reactive({
                req(validation()$report[[x]], validation()$data_formatted[[x]], input[[paste0("show_report", x, "_rows_selected")]])
            tryCatch({
                rows_for_rules(data_formatted = validation()$data_formatted[[x]], 
                               report = validation()$report[[x]], 
                               broken_rules = rules_broken(results = validation()$results[[x]], 
                                                           show_decision = input[[paste0("show_decision", x)]]), 
                               rows = input[[paste0("show_report", x, "_rows_selected")]]) 
                
               }, warning = function(w) {
                   toast(title = "Explanation", 
                                            body = paste0(w$message))
                   NULL
               }, error = function(e) {
                   toast(title = "Explanation", 
                                            #type = "success", 
                                            body = paste0(e$message))
                   NULL
                })
                
               })
            
            output[[paste0("report_selected", x)]] <- DT::renderDataTable({
                if(isTruthy(input[[paste0("show_report", x, "_rows_selected")]]) & !is.null(rows_for_rules_selected())){
                    datatable({rows_for_rules_selected() |>
                            mutate(across(everything(), check_images)) |>
                            mutate(across(everything(), check_other_hyperlinks))},
                              rownames = FALSE,
                              escape = FALSE,
                              filter = "top", 
                              extensions = 'Buttons',
                              options = list(
                                  searchHighlight = TRUE,
                                  scrollX = TRUE,
                                  sScrollY = '50vh', 
                                  scrollCollapse = TRUE,
                                  lengthChange = FALSE, 
                                  #pageLength = 5,
                                  paging = FALSE,
                                  searching = TRUE,
                                  fixedColumns = TRUE,
                                  autoWidth = FALSE,
                                  ordering = TRUE,
                                  dom = 'Bfrtip',
                                  buttons = c('copy', 'csv', 'excel', 'pdf')),
                              #style = "bootstrap",
                              class = "display") %>% 
                        formatStyle(
                            if(any(validation()$results[[x]]$status %in% c("error", "warning"))){
                                variables(validation()$rules[[x]][rules_broken(results = validation()$results[[x]], show_decision = input[[paste0("show_decision", x)]])[input[[paste0("show_report", x, "_rows_selected")]], "name"]])  
                            }
                            else{NULL},
                            backgroundColor =  '#FF817e'
                        )
                }
                else{
                    datatable({validation()$data_formatted[[x]] |>
                            mutate(across(everything(), check_images)) |>
                            mutate(across(everything(), check_other_hyperlinks))},
                              rownames = FALSE,
                              escape = FALSE,
                              filter = "top", 
                              extensions = 'Buttons',
                              options = list(
                                  searchHighlight = TRUE,
                                  scrollX = TRUE,
                                  sScrollY = '50vh', 
                                  scrollCollapse = TRUE,
                                  lengthChange = FALSE, 
                                  #pageLength = 5,
                                  paging = FALSE,
                                  searching = TRUE,
                                  fixedColumns = TRUE,
                                  autoWidth = FALSE,
                                  ordering = TRUE,
                                  dom = 'Bfrtip',
                                  buttons = c('copy', 'csv', 'excel', 'pdf')),
                              #style = "bootstrap",
                              class = "display")
                }
                
            })
            box(title = div(validation()$data_names[[x]], ": ", 
                            icon("circle-check", style = "color:black;"), 
                            sum(validation()$results[[x]][["status"]] == "success"), ", ", 
                            icon("circle-question", style = "color:#FFD700;"),
                            sum(validation()$results[[x]][["status"]] == "warning"), ",", 
                            icon("circle-xmark", style = "color:red;"), 
                            sum(validation()$results[[x]][["status"]] == "error")),
                id = paste0(validation()$data_names[[x]]),
                collapsed = T,
                background = if(validation()$issues[[x]]){"danger"}else{"white"},
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
                    box(title = "Issues Selected",
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
                    sScrollY = '50vh', 
                    scrollCollapse = TRUE,
                    lengthChange = FALSE, 
                    #pageLength = 5,
                    paging = FALSE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = FALSE,
                    ordering = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                rownames = FALSE,
                filter = "top", 
                style = "bootstrap")
    })
    
    observeEvent(req(validation()$data_formatted, !any(validation()$issues), vals$key), {
      tryCatch({
        remote_share(validation = validation(), 
                     data_formatted = validation()$data_formatted, 
                     files = gsub("\\\\", "/", input$file$datapath),
                     verified = vals$key, 
                     valid_key = config$valid_key, 
                     valid_rules = config$valid_rules, 
                     ckan_url = config$ckan_url, 
                     ckan_key = config$ckan_key, 
                     ckan_package = config$ckan_package, 
                     url_to_send = config$ckan_url_to_send, 
                     rules = read.csv(rules()), 
                     results = validation()$results, 
                     s3_key_id = config$s3_key_id, 
                     s3_secret_key = config$s3_secret_key, 
                     s3_region = config$s3_region, 
                     s3_bucket = config$s3_bucket, 
                     mongo_key = config$mongo_key,
                     mongo_collection = config$mongo_collection,
                     old_cert = input$old_certificate$datapath)
        
      }, warning = function(w) {
        shinyWidgets::show_alert(title = "Warning During Remote Sharing", 
                                 type = "warning", 
                                 text = paste0("Warning: ", w$message))
      }, error = function(e) {
        shinyWidgets::show_alert(title = "Error During Remote Sharing", 
                                 type = "error", 
                                 text = paste0("Error: ", e$message))
      }, message = function(m) {
        shinyWidgets::show_alert(title = "Successful Remote Data Sharing", 
                                 type = "success", 
                                 text = paste0(m$message, ". We recommend downloading a copy of your certificate in the top righthand corner of the screen for your records."))
        return(TRUE)
      })
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
                        verbatimTextOutput("file_info"),
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
            popover(
                downloadButton("download_certificate", "Download Certificate", style = "background-color: #ffffff; width: 100%;"),
                title = "Certificate of Valid Data",
                placement = "bottom",
                content = "Downloading this certificate will provide a verifiable record that you had a validated dataset at the time of certificate download. This certificate will also be shared with a remote repository for verification purposes.")
            
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
    
    output$remote_downloader <- downloadHandler(
        filename = function() {paste0(input$download_id, ".zip")},
        content = function(file) {
            remote_raw_download(hashed_data = input$download_id,
                                                   file_path = file,
                                                   s3_key_id = config$s3_key_id, 
                                                   s3_secret_key = config$s3_secret_key, 
                                                   s3_region = config$s3_region, 
                                                   s3_bucket = config$s3_bucket)}
    )
    
    output$download_rules_excel <- downloadHandler(
        filename = function() {"data_template.xlsx"},
        content = function(file) {saveWorkbook(create_valid_excel(file_rules = rules()), file, TRUE)}
    )
    
    output$download_rules <- downloadHandler( 
        filename = function() {"rules.csv"},
        content = function(file) {write.csv(read.csv(config$rules_example), file, row.names = F)}
    )
    
    output$download_sample <- downloadHandler(
        filename = function() {"invalid_data.zip"},
        content = function(file) {
            zip(file, config$invalid_data_example, extras = '-j')
            }
    )
    
    output$download_good_sample <- downloadHandler(
        filename = function() {"valid_data.zip"},
        content = function(file) {
            copy(config$valid_data_example)
            zip(file, config$valid_data_example, extras = '-j')
            }
    )
    
    # reactiveValues object for storing current data set.
    vals <- reactiveValues(key = NULL)
    
    #Secret Key Input ----
NoKeyModal <- function() {
  modalDialog(
    span('Would you like to share your uploaded data? Proceed with OK or click Cancel to simply test your data.'),
    p(),
    span('If this is a previous submission, enter it below:'),
    p(),
    box(
      title = "Is this an update to a previous submission?", 
      id = "update_submission",
      width = 12,
      collapsed = T,
      fileInput(inputId = "old_certificate", label = "Upload previous certificate.")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok_no_key", "OK")
    )
  )
}


    
    YesKeyModal <- function(failed = FALSE) {
      modalDialog(
        span('Would you like to share your uploaded data? Enter your input key provided by', config$contact, 'or click Cancel to simply test your data.'),
        p(),
        textInput("secret", "Input Key"),
        if (failed)
          div(tags$b("Invalid key-rules pair please try again or contact", config$contact, "for help.", style = "color: red;")),
        p(),
        box(
          title = "Is this an update to a previous submission?", 
          id = "update_submission",
          width = 12,
          collapsed = T,
          fileInput(inputId = "old_certificate", label = "Upload previous certificate.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_with_key", "OK")
        )
      )
    }
    
    observeEvent(req(isTRUE(!any(validation()$issues)), validation()$data_formatted), {
      if (is.null(config$valid_key)) {
        showModal(NoKeyModal())
      } else {
        showModal(YesKeyModal())
      }
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    
    observeEvent(input$ok_no_key, {
      vals$key <- NULL
      removeModal()
      show_alert(
        title = "Success",
        text = "Your data has been uploaded to the remote repository",
        type = "success")
    })
    
    observeEvent(input$ok_with_key, {
      # Check that data object exists and is data frame.
      if (is.null(config$valid_key)) {
        vals$key <- NULL
      } else {
      if (!is.null(input$secret) && input$ok_with_key < 4 && input$secret == config$valid_key){
        vals$key <- input$secret
        removeModal()
        show_alert(
          title = "Valid Key",
          text  = "Your key is valid, you are now logged in and we are sending your data to the remote repository, please wait for a success message before closing this window.",
          type  = "info")
      } else {
        showModal(YesKeyModal(failed = TRUE))
      }
      }
    })

   
    
}

