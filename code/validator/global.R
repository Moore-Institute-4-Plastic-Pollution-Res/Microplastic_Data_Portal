
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


if(isTruthy(config$s3_secret_key)){
    Sys.setenv(
        "AWS_ACCESS_KEY_ID" = config$s3_key_id,
        "AWS_SECRET_ACCESS_KEY" = config$s3_secret_key,
        "AWS_DEFAULT_REGION" = config$s3_region
    )
}

# Options ----
options(shiny.maxRequestSize = 30*1024^2)

# Functions ----
#' Generate a data frame with certificate information
#'
#' This function creates a data frame with certificate information including the current time,
#' data and rule hashes, package version, and web hash.
#'
#' @param x A list containing `data_formatted` and `rules` elements.
#' @param database_true A logical value (default: `isTruthy(config$mongo_key)`). If TRUE, the generated
#'   data frame will be inserted into the database.
#' @return A data frame with certificate information.
#' @importFrom digest digest
#' @importFrom data.table data.frame
#' @importFrom base Sys.time Sys.info
#' @importFrom utils packageVersion
#' @export
certificate_df <- function(x, database_true = isTruthy(config$mongo_key)){
    df <-  data.frame(time = Sys.time(), 
                      data = digest(x$data_formatted), 
                      rules = digest(x$rules), 
                      package_version = paste(unlist(packageVersion("validate")), collapse = ".", sep = ""), 
                      web_hash = digest(paste(sessionInfo(), 
                                              Sys.time(), 
                                              Sys.info())))
    if(database_true){
        database$insert(df)
    }
    df
}

#' Validate_data: Validate data based on specified rules.
#'
#' @param files_data A list of file paths for the datasets to be validated.
#' @param data_names (Optional) A character vector of names for the datasets. If not provided, names will be extracted from the file paths.
#' @param file_rules A file path for the rules file, either in .csv or .xlsx format.
#'
#' @return A list containing the following elements:
#'   - data_formatted: A list of data frames with the validated data.
#'   - data_names: A character vector of dataset names.
#'   - report: A list of validation report objects for each dataset.
#'   - results: A list of validation result data frames for each dataset.
#'   - rules: A list of validator objects for each dataset.
#'   - status: A character string indicating the overall validation status ("success" or "error").
#'   - issues: A logical vector indicating if there are any issues in the validation results.
#'   - message: A data.table containing information about any issues encountered.
#'
#' @examples
#' # Validate data with specified rules
#' result <- validate_data(files_data = list("data1.csv", "data2.csv"),
#'                         data_names = c("Dataset1", "Dataset2"),
#'                         file_rules = "rules.csv")
#'
#' @export
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

#' Remote Share Function
#'
#' This function uploads validated data to specified remote repositories,
#' such as CKAN, MongoDB, and/or Amazon S3.
#'
#' @param validation A list containing validation information.
#' @param data_formatted A list containing formatted data.
#' @param verified The secret key provided by the portal maintainer.
#' @param valid_rules A list of valid rules for the dataset.
#' @param valid_key A list of valid keys.
#' @param ckan_url The URL of the CKAN instance.
#' @param ckan_key The API key for the CKAN instance.
#' @param ckan_package The CKAN package to which the data will be uploaded.
#' @param url_to_send The URL to send the data.
#' @param rules A set of rules used for validation.
#' @param results A list containing results of the validation.
#' @param bucket The name of the Amazon S3 bucket.
#' @param mongo_key The API key for the MongoDB instance.
#' @param old_cert (Optional) An old certificate to be uploaded alongside the new one.
#'
#' @return A list containing the status and message of the operation.
#' @export
#'
#' @examples
#' remote_share(validation, data_formatted, verified, valid_rules, valid_key,
#'              ckan_url, ckan_key, ckan_package, url_to_send, rules, results,
#'              bucket, mongo_key, old_cert = NULL)
remote_share <- function(validation, data_formatted, verified, valid_rules, valid_key, ckan_url, ckan_key, ckan_package, url_to_send, rules, results, bucket, mongo_key, old_cert = NULL){
    
    use_ckan <- isTruthy(ckan_url) & isTruthy(ckan_key) & isTruthy(ckan_package)
    use_mongo <- isTruthy(mongo_key)
    use_s3 <- isTruthy(bucket)  
    
    if(!(use_ckan | use_mongo | use_s3)){
        return(list(
            message = data.table(
                title = "No upload methods available",
                text = "This feature will not work because no upload methods are specified.",
                type = "error"), status = "error"))
    }
    
    if(any(results$status == "error")){
        return(list(
            message = data.table(
            title = "Errors Prevent Upload",
            text = "There are errors in the dataset that persist. Until all errors are remedied, the data cannot be uploaded to the remote repository.",
            type = "error"), status = "error"))
    }
    
    if(!any(digest(as.data.frame(rules)) %in% valid_rules)){
        return(list(
            message = data.table(
            title = "Rules file is not valid",
            text = "If you are using a key to upload data to a remote repo then there must be a valid pair with the rules you are using in our internal database.",
            type = "error"), status = "error"))
    }
    
    if(!any(verified %in% valid_key)){
        return(list(
            message = data.table(
                title = "Secret Key is not valid",
                text = "You must have a valid key provided by the portal maintainer to use this feature.",
                type = "error"), status = "error"))
    }

    hashed_data <- digest(data_formatted)
    
    if(use_ckan){
        ckanr_setup(url = ckan_url, key = ckan_key)
    }    
    
        for(dataset in 1:length(data_formatted)){
            data_name <- names(data_formatted[dataset])
            #hashed_rules <- digest(rules)
            #package_version <- packageVersion("validate")
            file <- tempfile(pattern = "data", fileext = ".csv")
            write.csv(data_formatted[dataset], file, row.names = F)
            if(use_s3){
                put_object(
                    file = file,
                    object = paste0(hashed_data, "_", data_name, ".csv"),
                    bucket = bucket
                )    
            }
            if(use_ckan){
                resource_create(package_id = ckan_package,
                                description = "validated raw data upload to microplastic data portal",
                                name = paste0(hashed_data, "_", data_name),
                                upload = file)
            }
            if(use_mongo){
                database$insert(data_formatted[dataset])
            }
        }        

    certificate <- certificate_df(validation, database_true = use_mongo)
    file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(certificate, file, row.names = F)
    if(use_s3){
       put_object(
        file = file,
        object = paste0(hashed_data, "_", "certificate.csv"),
        bucket = bucket
    ) 
    }
    if(use_ckan){
        resource_create(package_id = ckan_package,
                        description = "validated raw data upload to microplastic data portal",
                        name = paste0(hashed_data, "_certificate"),
                        upload = file)    
    }
    if(isTruthy(old_cert)){
        if(use_mongo){
            database$insert(read.csv(old_cert))
        }
        if(use_s3){
            put_object(
                file = old_cert,
                object = paste0(hashed_data, "_", "old_certificate.csv"),
                bucket = bucket
            )    
        }
        if(use_ckan){
            resource_create(package_id = ckan_package,
                        description = "validated raw data upload to microplastic data portal",
                        name = paste0(hashed_data, "old_certificate"),
                        upload = old_cert)
        }
    }
    return(list(status = "success", 
                message = data.table(title = "Data Upload Successful", 
                                     text = paste0("Data was successfully sent to the data portal at ", url_to_send), 
                                     type = "success")))
}

#' rules_broken
#'
#' Filter the results of validation to show only broken rules, optionally including successful decisions.
#'
#' @param results A data frame with validation results.
#' @param show_decision A logical value to indicate if successful decisions should be included in the output.
#'
#' @return A data frame with the filtered results.
#' @export
#'
#' @examples
#' # Sample validation results data frame
#' sample_results <- data.frame(
#'   description = c("Rule 1", "Rule 2", "Rule 3"),
#'   status = c("error", "success", "error"),
#'   name = c("rule1", "rule2", "rule3"),
#'   expression = c("col1 > 0", "col2 <= 5", "col3 != 10"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Show only broken rules
#' broken_rules <- rules_broken(sample_results, show_decision = FALSE)
rules_broken <- function(results, show_decision){
    results %>%
        dplyr::filter(if(show_decision){status == "error"} else{status %in% c("error", "success")}) %>%
        select(description, status, name, expression, everything())
}

#' rows_for_rules
#'
#' Get the rows in the data that violate the specified rules.
#'
#' @param data_formatted A formatted data frame.
#' @param report A validation report generated by the 'validate' function.
#' @param broken_rules A data frame with broken rules information.
#' @param rows A vector of row indices specifying which rules to check for violations.
#'
#' @return A data frame with rows in the data that violate the specified rules.
#' @export
#'
#' @examples
#' # Sample data
#' sample_data <- data.frame(
#'   col1 = c(1, -2, 3, -4, 5),
#'   col2 = c(6, 7, 8, 9, 10)
#' )
#'
#' # Validation rules
#' rules <- validator(
#'   col1 > 0,
#'   col2 <= 5
#' )
#'
#' # Generate a validation report
#' report <- confront(sample_data, rules)
#'
#' # Find the broken rules
#' broken_rules <- rules_broken(report, show_decision = FALSE)
#'
#' # Get rows for the specified rules
#' violating_rows <- rows_for_rules(sample_data, report, broken_rules, c(1, 2))
rows_for_rules <- function(data_formatted, report, broken_rules, rows){
    violating(data_formatted, report[broken_rules[rows, "name"]])
}

#acknowledgement https://github.com/adamjdeacon/checkLuhn/blob/master/R/checkLuhn.R
#' Check if a number passes the Luhn algorithm
#'
#' This function checks if a given number passes the Luhn algorithm. It is commonly used to validate credit card numbers.
#' @param number A character string of the number to check against the Luhn algorithm.
#' @return A logical value indicating whether the number passes the Luhn algorithm (TRUE) or not (FALSE).
#' @examples
#' checkLuhn("4532015112830366") # TRUE
#' checkLuhn("4532015112830367") # FALSE
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

#' Check if a file can be uploaded to an S3 bucket
#'
#' This function checks if a file located at a given URL can be downloaded and uploaded to an S3 bucket.
#' @param url A character string representing the URL of the file to download.
#' @param bucket A character string representing the S3 bucket name where the file will be uploaded. Defaults to 'config$s3_bucket'.
#' @return A logical value indicating whether the file can be uploaded (TRUE) or not (FALSE).
#' @examples
#' # Note: The example assumes you have valid AWS credentials and an S3 bucket available
#' check_uploadable("https://example.com/file.csv", bucket = "your-s3-bucket-name")
check_uploadable <- function(url, bucket = config$s3_bucket){
    hash_url <- digest(url)
    file_type <- gsub(".*\\.", "", url)
    file_name <- paste0(hash_url, ".", file_type)
    filedest <- paste0(tempfile(), file_name)
    test <- tryCatch(download.file(url = url, 
                                   destfile = filedest, 
                                   quiet = T,
                                   mode = "wb"), #The wb is for windows, need to be changed for remote deployment.
                     warning = function(w) {w}, error = function(e) {e})
    if(length(class(test)) != 1 || class(test) != "integer"){
        return(FALSE)
    }
    if(isTruthy(config$s3_bucket)){
        put_object(
            file = filedest,
            object = file_name,
            bucket = bucket
        )   
    }
}


#' Check and format image URLs
#'
#' This function checks if the input string contains an image URL (PNG or JPG) and formats it as an HTML img tag with a specified height.
#' @param x A character string to check for image URLs.
#' @return A character string with the HTML img tag if an image URL is found, otherwise the input string.
#' @examples
#' check_images("https://example.com/image.png")
#' check_images("https://example.com/image.jpg")
#' check_images("https://example.com/text")
check_images <- function(x){
    ifelse(grepl("https://.*\\.png|https://.*\\.jpg", x), 
           paste0('<img src ="', x, '" height = "50"></img>'), 
           x)
}


#' Check and format non-image hyperlinks
#'
#' This function checks if the input string contains a non-image hyperlink and formats it as an HTML anchor tag.
#' @param x A character string to check for non-image hyperlinks.
#' @return A character string with the HTML anchor tag if a non-image hyperlink is found, otherwise the input string.
#' @examples
#' check_other_hyperlinks("https://example.com/page")
#' check_other_hyperlinks("https://example.com/image.png")
#' check_other_hyperlinks("https://example.com/image.jpg")
check_other_hyperlinks <- function(x){
    ifelse(grepl("https://", x) & !grepl("https://.*\\.png|https://.*\\.jpg", x), 
           paste0('<a href ="', x, '">', x, '</a>'), 
           x)
}

#' Test for profanity in a string
#'
#' This function checks if the input string contains any profane words.
#' @param x A character string to check for profanity.
#' @return A logical value indicating whether the input string contains no profane words.
#' @examples
#' test_profanity("This is a clean sentence.")
#' test_profanity("This sentence contains a badword.")
test_profanity <- function(x){
    bad_words <- unique(tolower(c(#lexicon::profanity_alvarez, 
                                  #lexicon::profanity_arr_bad, 
                                  lexicon::profanity_banned#, 
                                  #lexicon::profanity_zac_anger#, 
                                  #lexicon::profanity_racist
                                  )))
    vapply(bad_words, function(y){
        !grepl(y, x, ignore.case = T)
    }, FUN.VALUE = T) |>
        all()
}

#Rules to excel
#' Create a formatted Excel file based on validation rules
#'
#' This function creates an Excel file with conditional formatting and data validation
#' based on the given validation rules in a CSV or Excel file.
#' @param file_rules A CSV or Excel file containing validation rules.
#' @param negStyle Style to apply for negative conditions (default is red text on a pink background).
#' @param posStyle Style to apply for positive conditions (default is green text on a light green background).
#' @param row_num Number of rows to create in the output file (default is 1000).
#' @param file_name Name of the output Excel file (default is "conditionalFormattingExample.xlsx").
#' @return A workbook object containing the formatted Excel file.
#' @examples
#' create_valid_excel(file_rules = "validation_rules.csv")
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



