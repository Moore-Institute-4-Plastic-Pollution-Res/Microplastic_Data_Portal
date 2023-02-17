
# Libraries ----
library(shiny)
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
library(sentimentr)
library(listviewer)
library(RCurl)
library(readxl)
library(stringr)
library(openxlsx)
library(mongolite)
library(config)
library(aws.s3)

#Note for logic using outside functions in the calls. 
#https://github.com/data-cleaning/validate/issues/45

config <- config::get()

#Data checks ----


if(isTruthy(config$mongo_key)) {
    database <- mongo(url = config$mongo_key)
} 


if(isTruthy(config$s3_secret_key)) {
    Sys.setenv(
        "AWS_ACCESS_KEY_ID" = config$s3_key_id,
        "AWS_SECRET_ACCESS_KEY" = config$s3_secret_key,
        "AWS_DEFAULT_REGION" = config$s3_region
    )
}

# Options ----
options(shiny.maxRequestSize = 30*1024^2)

# Functions ----

certificate_df <- function(x){
    df <-  data.frame(time = Sys.time(), 
                      data = digest(x$data_formatted), 
                      rules = digest(x$rules), 
                      package_version = paste(unlist(packageVersion("validate")), collapse = ".", sep = ""), 
                      web_hash = digest(paste(sessionInfo(), 
                                              Sys.time(), 
                                              Sys.info())))
    database$insert(df)
    df
}

validate_data <- function(files_data, data_names = NULL, file_rules = NULL){
    
    #Accepts three fields, files data is the data set we are validating. data_names is optional and can be used to specify the names of the datasets. file_rules is the rules file. 
    
    #Tests that rules file is a csv
    if (!grepl("(\\.csv$)|(\\.xlsx$)", ignore.case = T, as.character(file_rules))) {
        return(list(
            message = data.table(
                title = "Data type not supported!",
                text = paste0("Uploaded data type is not currently supported; please upload a .csv or .xlsx file."),
                type = "warning"), status = "error"))
    }
    
    #Reads the rules file.
    if(grepl("(\\.csv$)", ignore.case = T, as.character(file_rules))){
        rules <- read.csv(file_rules)
    }
    
    if(grepl("(\\.xlsx$)", ignore.case = T, as.character(file_rules))){
        rules <- read_excel(file_rules)
    }
    
    #Test that rules file has the correct required column names. 
    if (!all(c("name", "description", "severity", "rule") %in% names(rules))) {
        return(list(
            message = data.table(
                title = "Data type not supported!",
                text = paste0('Uploaded rules format is not currently supported, please provide a rules file with column names, "name", "description", "severity", "rule"'),
                type = "warning"), status = "error"))
    }
    
    #Tests that the rules do not contain sensitive words that may be malicious. 
    if (any(grepl("config|secret", rules$rule))) {
        return(list(
            message = data.table(
                title = "Rule not supported!",
                text = paste0('At this time we are unable to support any rules with the words config or secret in them as they could be malicious.'),
                type = "warning"), status = "error"))
    }
    
    #Checks that all the rules fields are character type. 
    if (!all(unlist(lapply(rules, class)) %in% c("character"))) {
        return(list(
            message = data.table(
                title = "Data type not supported!",
                text = paste0('Uploaded rules format is not currently supported, please provide a rules file with columns that are all character type.'),
                type = "warning"), status = "error"))
    }
    
    # check files data to make sure it is a csv. 
    if (!all(grepl("(\\.csv$)|(\\.xlsx$)", ignore.case = T, as.character(files_data)))) {
        return(list(
            message = data.table(
            title = "Data type not supported!",
            text = paste0("Uploaded data type is not currently supported; please
                      upload a .csv or .xlsx file."),
            type = "warning"), status = "error"))
    }
    
    # Check that the rules file exists, if not then provide message. 
    if(is.null(rules)) {
        return(list(
            message = data.table(
            title = "Need Rules File",
            text = paste0("You must upload a rules file before uploading a data file to validate."),
            type = "warning"), status = "error"))
    }
    
    #Read in all csv files from files_data as a list. 
    if(all(grepl("(\\.csv$)", ignore.case = T, as.character(files_data)))){
        data_formatted <- tryCatch(lapply(files_data, function(x){read.csv(x)}),
                                   warning = function(w) {w}, error = function(e) {e})
    }
    
    else if(all(grepl("(\\.xlsx$)", ignore.case = T, as.character(files_data)))){
        if(length(as.character(files_data)) > 1){
            data_formatted <- tryCatch(lapply(files_data, function(x){read_excel(x)}),
                                       warning = function(w) {w}, error = function(e) {e})    
        }
        if(length(as.character(files_data)) == 1){
            sheets <- readxl::excel_sheets(files_data)
            data_formatted <- tryCatch(lapply(sheets, function(x){read_excel(files_data, sheet =  x)}),
                                       warning = function(w) {w}, error = function(e) {e})    
        }
    }
    
    else{
        return(list(
            message = data.table(
                title = "Mixed Data Types",
                text = paste0("You cannot mix data types, choose either csv or xlsx for all datasets."),
                type = "warning"), status = "error")) 
    }
    
    
    
    
    #Check if there is a warning when reading in the data. 
    if (inherits(data_formatted, "simpleWarning") | inherits(data_formatted, "simpleError")){
        return(list(
            message = data.table(
            title = "Something went wrong with reading the data.",
            text = paste0("There was an error that said ", data_formatted$message),
            type = "error"),
            status = "error"
                )
            )
    }
    
    #Grab the names of the datasets.
    data_names <- if(isTruthy(data_names)){
                    gsub("(.*/)|(\\..*)", "", data_names)
    } 
    else if(all(grepl("(\\.xlsx$)", ignore.case = T, as.character(files_data))) & length(as.character(files_data)) == 1){
        readxl::excel_sheets(files_data)
    }
    else{
        gsub("(.*/)|(\\..*)", "", files_data)
        } 
                    
    
    #Names the data with the file names. 
    names(data_formatted) <- data_names
    
    #Checks if there is a dataset column in the rules file and tests that all of the datasets exist. 
    if (!"dataset" %in% names(rules) & length(data_names) > 1){
            return(list(
                message = data.table(
                    title = "Missing dataset column",
                    text = paste0("If there is more than one dataset then a dataset column must be specified in the rules file to describe which rule applies to which dataset."),
                    type = "error"),
                status = "error"
            )
            )
    }
    
    #Checks if there is a dataset column in the rules file and tests that all of the datasets exist. 
    if ("dataset" %in% names(rules)){
        if(!all(unique(rules$dataset) %in% data_names)){
            return(list(
            message = data.table(
                title = "Dataset names incompatible",
                text = paste0("If there is a dataset column in the rules file it needs to pertain to the names of the datasets being tested. The rules file lists these datasets ", paste(unique(rules$dataset), collapse = ", "), " while the datasets shared are ", paste(data_names, collapse = ",")),
                type = "error"),
            status = "error"
            )
        )
        }
    }
    
    
    #Add dataset if one doesn't exist so that everything else works. 
    if (!"dataset" %in% names(rules)){
        rules <- rules %>%
            mutate(dataset = data_names)
    }
    
    #Circle back to add logic for multiple dfs
    #Check for special character "___" which is for assessing every column. 
    
    do_to_all <- rules %>%
        filter(grepl("___", rule))
    
    if(nrow(do_to_all) > 0){
       rules <- lapply(data_names, function(x){
            rules_sub <- do_to_all %>% filter(dataset == x)
            lapply(colnames(data_formatted), function(new_name){
                rules_sub %>%
                    mutate(rule = gsub("___", new_name, rule)) %>%
                    mutate(name = paste0(new_name, "_", name))
            }) %>%
                rbindlist(.)
        }) %>%
           rbindlist(.) %>%
           bind_rows(rules %>% filter(!grepl("___", rule)))
    }
   
    # Check special character of is_foreign_key and if so then testing that foreign keys are exact. 
    foreign_keys <- rules %>%
        filter(grepl("is_foreign_key(.*)", rule))
    
    if(nrow(foreign_keys) > 0){
       columns <- gsub("(is_foreign_key\\()|(\\))", "", foreign_keys[["rule"]])
       if (!"dataset" %in% names(rules)){
           return(list(
               message = data.table(
                   title = "Foreign key error.",
                   text = "Foreign key searches only work if there is a dataset column in the rules file that specifies which dataset the foreign key is in.",
                   type = "error"
               ), status = "error"
           ))
       }
       rules <- lapply(1:nrow(foreign_keys), function(x){
           foreign_keys[x,] %>%
           mutate(rule = paste0(columns[x], 
                                ' %in% c("',
                                paste(
                                lapply(data_formatted, function(y){
                                       y[[columns[x]]]
                                            }) %>% 
                                       unlist(.) %>%
                                       unique(.), 
                                collapse = '", "', 
                                sep = ""), 
                                '")'))
       }) %>%
           rbindlist(.) %>%
           bind_rows(rules %>% filter(!grepl("is_foreign_key(.*)", rule)))
    }
    
    #Testing that the rules file has no errors. 
    rules_formatted <- tryCatch(validator(.data=rules), 
                                warning = function(w) {w}, 
                                error = function(e) {e})
    
    #Tests that rules_formatted has only one class and that class is validator. 
    if (length(class(rules_formatted)) != 1 || class(rules_formatted) != "validator"){
        return(list(
            message = data.table(
                title = "Something went wrong with reading the rules file.",
                text = paste0("There was an error that said ", rules_formatted$message),
                type = "error"
            ), status = "error"
        ))
    }
    
    #Tests that columns in rules files are in the same as in the data. 
    if(!all(variables(rules_formatted) %in% unlist(lapply(data_formatted, names))) | !all(unlist(lapply(data_formatted, names)) %in% variables(rules_formatted))){
        warning_2 <- data.table(
                        title = "Rules and data mismatch",
                        text = paste0("All variables in the rules csv (", paste(variables(rules_formatted)[!variables(rules_formatted) %in% unlist(lapply(data_formatted, names))], collapse = ", "), ") need to be in the data csv (",  paste(unlist(lapply(data_formatted, names))[!unlist(lapply(data_formatted, names)) %in% variables(rules_formatted)], collapse = ", "), ") and vice versa for the validation to work."),
                        type = "warning")
    }
    
    #Loops through and makes a validation object for every dataset. 
    report <- lapply(data_names, function(x){
       confront(data_formatted[[x]], validator(.data=rules %>% filter(dataset == x))) 
    })
    
    #Loops through and makes a results report for the validation
    results <- lapply(report, function(x) {summary(x) %>%
        mutate(status = ifelse(fails > 0 | error | warning , "error", "success")) %>%
        left_join(rules)})
    
    any_issues <- vapply(results, function(x){
                            any(x$status == "error")
                            }, FUN.VALUE = TRUE)
    
    #Loops through and makes a rules object for each data set. 
    rules_list_formatted <- tryCatch(lapply(data_names, function(x){validator(.data=rules %>% filter(dataset == x))}), 
                                warning = function(w) {w}, 
                                error = function(e) {e})

    
    #Returns all the results for everything in a formatted list. 
    return(list(data_formatted = lapply(data_formatted, function(x){
                    x %>%
                    mutate(across(everything(), check_images)) %>%
                    mutate(across(everything(), check_other_hyperlinks))}),
                data_names = data_names,
                report = report, 
                results = results, 
                rules = rules_list_formatted, 
                status = "success", 
                issues = any_issues,
                message = if(exists("warning_2")){warning_2} else{NULL}))
}


remote_share <- function(validation, data_formatted, verified, api, rules, results){
    
    if(any(results$status == "error")){
        return(list(
            message = data.table(
            title = "Errors Prevent Upload",
            text = "There are errors in the dataset that persist. Until all errors are remedied, the data cannot be uploaded to the remote repository.",
            type = "error"), status = "error"))
    }
    
    if(!any(digest(as.data.frame(rules)) %in% api$ckan_valid_rules)){
        return(list(
            message = data.table(
            title = "Rules file is not valid",
            text = "If you are using a key to upload data to a remote repo then there must be a valid pair with the rules you are using in our internal database.",
            type = "error"), status = "error"))
    }
    
    if(!any(verified %in% api$ckan_valid_key)){
        return(list(
            message = data.table(
                title = "Secret Key is not valid",
                text = "Any column labeled KEY is considered a secret key and should have a valid pair in our internal database.",
                type = "error"), status = "error"))
    }
    
    api_info <- api %>%
        dplyr::filter(ckan_valid_key == verified & ckan_valid_rules == digest(as.data.frame(rules)))
    
    if(nrow(api_info) != 1){
        return(list(
            message = data.table(
            title = "Mismatched rules file and KEY column",
            text = "The secret key and rules file must be exact matches to one another. One secret key is for one rules file.",
            type = "error"), status = "error"))
    }
    
    ckanr_setup(url = api_info$ckan_url, key = api_info$ckan_key)
    hashed_data <- digest(data_formatted)
    
    for(dataset in 1:length(data_formatted)){
        data_name <- names(data_formatted[dataset])
        #hashed_rules <- digest(rules)
        #package_version <- packageVersion("validate")
        file <- tempfile(pattern = "data", fileext = ".csv")
        write.csv(data_formatted[dataset], file, row.names = F)
        put_object(
                file = file,
                object = paste0(hashed_data, "_", data_name, ".csv"),
                bucket = "microplasticdataportal"
            )
        resource_create(package_id = api_info$ckan_package,
                                    description = "validated raw data upload to microplastic data portal",
                                    name = paste0(hashed_data, "_", data_name),
                                    upload = file)
    }
    certificate <- certificate_df(validation)
    file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(certificate, file, row.names = F)
    put_object(
        file = file,
        object = paste0(hashed_data, "_", "certificate.csv"),
        bucket = "microplasticdataportal"
    )
    resource_create(package_id = api_info$ckan_package,
                    description = "validated raw data upload to microplastic data portal",
                    name = paste0(hashed_data, "_certificate"),
                    upload = file)
    
    return(list(status = "success", 
                message = data.table(title = "Data Upload Successful", 
                                     text = paste0("Data was successfully sent to the state data portal at ", api_info$ckan_url_to_send), 
                                     type = "success")))
}


rules_broken <- function(results, show_decision){
    results %>%
        dplyr::filter(if(show_decision){status == "error"} else{status %in% c("error", "success")}) %>%
        select(description, status, name, expression, everything())
}

rows_for_rules <- function(data_formatted, report, broken_rules, rows){
    violating(data_formatted, report[broken_rules[rows, "name"]])
}

#acknowledgement https://github.com/adamjdeacon/checkLuhn/blob/master/R/checkLuhn.R
checkLuhn <- function(number) {
    # must have at least 2 digits
    if(nchar(number) <= 2) {
        return(FALSE)
    }
    
    # strip spaces
    number <- gsub("-", "", gsub(pattern = " ", replacement = "", number))
    
    # Return FALSE if not a number
    if (!grepl("^[[:digit:]]+$", number)) {
        return(FALSE)
    }
    
    # split the string, convert it to a list, and reverse it
    digits <- unlist(strsplit(number, ""))
    digits <- digits[length(digits):1]
    
    to_replace <- seq(2, length(digits), 2)
    digits[to_replace] <- as.numeric(digits[to_replace]) * 2
    
    # gonna do some maths, let's convert it to numbers
    digits <- as.numeric(digits)
    
    # a digit cannot be two digits, so any that are greater than 9, subtract 9 and
    # make the world a better place
    digits <- ifelse(digits > 9, digits - 9, digits)
    
    # does the sum divide by 10?
    ((sum(digits) %% 10) == 0)
}

check_images <- function(x){
    ifelse(grepl("https://.*\\.png|https://.*\\.jpg", x), 
           paste0('<img src ="', x, '" height = "50"></img>'), 
           x)
}

check_other_hyperlinks <- function(x){
    ifelse(grepl("https://", x) & !grepl("https://.*\\.png|https://.*\\.jpg", x), 
           paste0('<a href ="', x, '">', x, '</a>'), 
           x)
}

test_profanity <- function(x){
    bad_words <- unique(tolower(c(lexicon::profanity_alvarez, 
                                  lexicon::profanity_arr_bad, 
                                  lexicon::profanity_banned, 
                                  lexicon::profanity_zac_anger, 
                                  lexicon::profanity_racist, 
                                  "test")))
    profanity(x, bad_words)$profanity_count == 0
}

#Rules to excel

create_valid_excel <- function(file_rules, 
                               negStyle  = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"),
                               posStyle  = createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
                               row_num   = 1000,
                               file_name = "conditionalFormattingExample.xlsx"){
    #Reads the rules file.
    if(grepl("(\\.csv$)", ignore.case = T, as.character(file_rules))){
        rules <- read.csv(file_rules)
    }
    
    if(grepl("(\\.xlsx$)", ignore.case = T, as.character(file_rules))){
        rules <- read_excel(file_rules)
    }
    
    #Grab the names of the datasets.
    data_names <- if("dataset" %in% names(rules)){
        unique(rules$dataset)
    } 
    else{
        name <- gsub("(.*/)|(\\..*)", "", file_rules)
        rules$dataset <- name
        name
    }
    
    #Circle back to add logic for multiple dfs
    #Check for special character "___" which is for assessing every column. 
    
    do_to_all <- rules %>%
        filter(grepl("___", rule))
    
    if(nrow(do_to_all) > 0){
        rules <- lapply(data_names, function(x){
            rules_sub <- do_to_all %>% filter(dataset == x)
            rules_sub_variables <- variables(validator(.data=rules_sub))
            lapply(rules_sub_variables, function(new_name){
                rules_sub %>%
                    mutate(rule = gsub("___", new_name, rule)) %>%
                    mutate(name = paste0(new_name, "_", name))
            }) %>%
                rbindlist(.)
        }) %>%
            rbindlist(.) %>%
            bind_rows(rules %>% filter(!grepl("___", rule)))
    }
    
    rules <- rules %>%
        filter(!grepl("is_foreign_key(.*)", rule))
    
    lookup_column_index <- 1
    wb <- createWorkbook()
    addWorksheet(wb, "Rules")
    writeData(wb, sheet = "Rules", x = rules, startCol = 1)
    for(sheet_num in 1:length(data_names)){ #Sheet level for loop
        rules_all_raw <- rules %>% filter(dataset == data_names[sheet_num])
        rules_all <- validator(.data = rules_all_raw)
        rule_variables <- variables(rules_all)
        sheet_name <- data_names[sheet_num]
        addWorksheet(wb, sheet_name)
        freezePane(wb, sheet_name, firstRow = TRUE) ## shortcut to freeze first row for every table.
        for(col_name in rule_variables){#Setup the column names with empty rows. 
            df <- as_tibble(rep("", row_num))
            names(df) <- col_name
            column_index_startup <- which(rule_variables == col_name)
            writeData(wb, sheet = sheet_name, x = df, startCol = column_index_startup)
        }
        for(col_num in 1:length(rules_all)){
            rule_test <- rules_all[[col_num]]
            expression <- rule_test@expr
            column_index <- which(rule_variables == variables(rule_test))
            if(any(grepl("(%vin%)|(%in%)", expression))){
                if(lookup_column_index == 1){
                    addWorksheet(wb, "Lookup")
                }
                values <- unlist(strsplit(gsub('(")|(\\))|(.*c\\()', "", as.character(expression[3])), ", "))
                lookup_col <- LETTERS[lookup_column_index] 
                df_lookup <- tibble(values)
                names(df_lookup) <- paste0(variables(rule_test), "_lookup")
                writeData(wb, 
                          sheet = "Lookup", 
                          x = df_lookup, 
                          startCol = lookup_column_index)
                dataValidation(wb, 
                               sheet = sheet_name, 
                               cols = column_index, 
                               rows = 2:row_num,
                               type = "list", 
                               value = paste0("Lookup!$", lookup_col, "$2:$", lookup_col, "$", length(values) +1))  
                lookup_column_index = lookup_column_index + 1
            }
            if(any(grepl("is_unique\\(.*\\)", expression))){
                conditionalFormatting(wb, 
                                      sheet_name, 
                                      cols = column_index, 
                                      rows = 2:row_num, 
                                      type = "duplicates", 
                                      style = negStyle)
            }
            if(sum(grepl("!|is.na(.*)", expression)) == 2){ #Not working yet.
                dataValidation(wb, 
                               sheet_name, 
                               cols = column_index, 
                               rows = 2:row_num, 
                               type = "textlength", 
                               operator = "greaterThanOrEqual",
                               value = "1",
                               allowBlank = FALSE)
            }
            if(any(grepl("in_range(.*)", expression))){
                dataValidation(wb, 
                               sheet_name, 
                               cols = column_index, 
                               rows = 2:row_num, 
                               type = "decimal", 
                               operator = "between",
                               value = c(as.numeric(as.character(expression)[grepl("^-|[0-9]+$", as.character(expression))][1]), 
                                         as.numeric(as.character(expression)[grepl("^-|[0-9]+$", as.character(expression))][2])))
            }
            if(any(grepl("grepl(.*)", expression))){ #could be improved with begins with and ends with logic.  
                good_conditions <- unlist(strsplit(gsub('(\\[[0-9]*-[0-9]*\\])|(\\])|(\\[)|(\\\\)|(\\^)|(\\$)|(\\))|(\\()', "",  as.character(expression)[2]), split = "\\|"))
                for(contain_condition in good_conditions){
                    conditionalFormatting(wb, 
                                          sheet_name, 
                                          cols = column_index, 
                                          rows = 2:row_num, 
                                          type = "contains",
                                          rule = contain_condition,
                                          style = posStyle)
                }
            }
            if(any(grepl("(%vin%)|(%in%)", expression))){
                protectWorksheet(
                    wb,
                    "Lookup",
                    protect = TRUE) #Protects the lookup table without a password just to prevent accidents.
            }
            #Need better way to deal with foreign keys, currently not working well. 
            
        }
    }
    saveWorkbook(wb, file_name, TRUE)
    wb
}



