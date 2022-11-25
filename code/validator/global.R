# Libraries ----
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
library(tidyxl)
library(XLConnect)

# Options ----
options(shiny.maxRequestSize = 30*1024^2)

# Files ----
api <- read.csv("secrets/ckan.csv")

# Functions ----
validate_rules <- function(file_rules){
    if (!grepl("(\\.csv$)", ignore.case = T, as.character(file_rules))) {
        #reset("file")
        return(list(
            message = data.table(
            title = "Data type not supported!",
            text = paste0("Uploaded data type is not currently supported; please upload a .csv file."),
            type = "warning"), status = "error"))
    }
    
    rules <- read.csv(file_rules)
    
    if (!all(c("name", "description", "severity", "rule") %in% names(rules))) {
        #reset("file")
        return(list(
            message = data.table(
            title = "Data type not supported!",
            text = paste0('Uploaded rules format is not currently supported, please provide a rules file with column names, "name", "description", "severity", "rule"'),
            type = "warning"), status = "error"))
    }
    
    if (!all(unlist(lapply(rules, class)) %in% "character")) {
        #reset("file")
        return(list(
            message = data.table(
            title = "Data type not supported!",
            text = paste0('Uploaded rules format is not currently supported, please provide a rules file with columns that are all character type.'),
            type = "warning"), status = "error"))
    }
    
    rules_formatted <- tryCatch(validator(.data=rules), 
                                warning = function(w) {w}, 
                                error = function(e) {e})
    
    if (inherits(rules_formatted, "simpleWarning") | inherits(rules_formatted, "simpleError") | inherits(rules_formatted, "error") | inherits(rules_formatted, "notSubsetableError")){
        
        
        return(list(
            message = data.table(
            title = "Something else went wrong with reading the rules file.",
            text = paste0("There was an error that said ", rules_formatted$message),
            type = "error"
            ), status = "error"
        ))
    }
    return(list(rules = rules_formatted, status = "success"))
}


validate_data <- function(files_data, rules){
    # Read in data when uploaded based on the file type
    if (!all(grepl("(\\.csv$)", ignore.case = T, as.character(files_data)))) {
        return(list(
            message = data.table(
            title = "Data type not supported!",
            text = paste0("Uploaded data type is not currently supported; please
                      upload a .csv file."),
            type = "warning"), status = "error"))
    }
    if(is.null(rules)) {
        return(list(
            message = data.table(
            title = "Need Rules File",
            text = paste0("You must upload a rules file before uploading a data file to validate."),
            type = "warning"), status = "error"))
    }
    data_formatted <- tryCatch(lapply(files_data, function(x) read.csv(x)) %>% 
                         reduce(full_join),
                            warning = function(w) {w}, error = function(e) {e})
    
    if (inherits(data_formatted, "simpleWarning") | inherits(data_formatted, "simpleError")){
        return(list(
            message = data.table(
            title = "Something went wrong with the merge.",
            text = paste0("This tool expects at least one column in each dataset with the same name to merge on. There was also an error that said ", data_formatted$message),
            type = "error"),
            status = "error"
            )
            )
    }
    if(!all(variables(rules) %in% names(data_formatted)) | !all(names(data_formatted) %in% variables(rules))){
        return(list(
            message = data.table(
            title = "Rules and data mismatch",
            text = paste0("All variables in the rules csv (", paste(variables(rules)[!variables(rules) %in% names(data_formatted)], collapse = ", "), ") need to be in the data csv (",  paste(names(data_formatted)[!names(data_formatted) %in% variables(rules)], collapse = ", "), ") and vice versa for the validation to work."),
            type = "error"), status = "error"))
    }
    report <- confront(data_formatted, rules)
    results <- summary(report) %>%
        mutate(status = ifelse(fails > 0 | error | warning , "error", "success")) %>%
        mutate(description = meta(rules)$description)
    
    return(list(data_formatted = data_formatted, report = report, results = results, rules = rules, status = "success"))
}


remote_share <- function(data_formatted, api, rules, results){
    if(any(results$status == "error")){
        return(list(
            message = data.table(
            title = "Errors Prevent Upload",
            text = "There are errors in the dataset that persist. Until all errors are remedied, the data cannot be uploaded to the remote repository.",
            type = "error"), status = "error"))
    }
    if(any(unlist(lapply(data_formatted %>% select(-KEY), function(x) any(x %in% unique(data_formatted$KEY)))))){
        return(list(
            message = data.table(
            title = "Secret Key is misplaced",
            text = "The secret key is in locations other than the KEY column, please remove the secret key from any other locations.",
            type = "error"), status = "error"))
    }
    if(length(unique(data_formatted$KEY)) != 1){
        return(list(
            message = data.table(
            title = "Multiple Secret Keys",
            text = paste0("There should only be one secret key per data upload, but these keys are in the data (", paste(unique(data_formatted$KEY), collapse = ","), ")"),
            type = "error"), status = "error"))
    }
    if(!any(unique(data_formatted$KEY) %in% api$VALID_KEY)){
        return(list(
            message = data.table(
            title = "Secret Key is not valid",
            text = "Any column labeled KEY is considered a secret key and should have a valid pair in our internal database.",
            type = "error"), status = "error"))
    }
    if(!any(digest(as.data.frame(rules) %>% select(-created)) %in% api$VALID_RULES)){
        return(list(
            message = data.table(
            title = "Rules file is not valid",
            text = "If you are using a key column to upload data to a remote repo then there must be a valid pair with the rules you are using in our internal database.",
            type = "error"), status = "error"))
    }
    api_info <- api %>%
        filter(VALID_KEY == unique(data_formatted$KEY) & VALID_RULES == digest(as.data.frame(rules) %>% select(-created)))
    
    if(nrow(api_info) != 1){
        return(list(
            message = data.table(
            title = "Mismatched rules file and KEY column",
            text = "The secret key and rules file must be exact matches to one another. One secret key is for one rules file.",
            type = "error"), status = "error"))
    }
    
    ckanr_setup(url = api_info$URL, key = api_info$KEY)
    hashed_data <- digest(data_formatted)
    #hashed_rules <- digest(rules)
    #package_version <- packageVersion("validate")
    file <- tempfile(pattern = "data", fileext = ".csv")
    write.csv(data_formatted %>% select(-KEY), file, row.names = F)
    creation <- resource_create(package_id = api_info$PACKAGE,
                                        description = "validated raw data upload to microplastic data portal",
                                        name = paste0("data_", hashed_data),
                                        upload = file)
    return(list(creation = creation, status = "success"))
}


rules_broken <- function(results, show_decision){
    results %>%
        dplyr::filter(if(show_decision){status == "error"} else{status %in% c("error", "success")}) %>%
        select(description, status, name, expression, everything())
}

rows_for_rules <- function(data_formatted, report, broken_rules, rows){
    violating(data_formatted, report[broken_rules[rows, "name"]])
}


#PII Checkers ----
#https://www.servicenow.com/community/developer-articles/common-regular-expressions-and-cheat-sheet/ta-p/2297106
license_plate <- "\\b[0-9A-Z]{3}([^ 0-9A-Z]|\\s)?[0-9]{4}\\b"
email <- "^[[:alnum:].-]+@[[:alnum:].-]+$" #^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$
national_id <- "^[0-9]{3}-[0-9]{2}-[0-9]{4}$"
ip <- "^(?:(25[0-5]|2[0-4]\\d|[01]?\\d{1,2})\\.){3}\\1$"#"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$" #
phone_number <- "\\d{3}?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"
amexcard <- "^3[47][0-9]{13}$"
mastercard <- "^(?:5[1-5][0-9]{2}|222[1-9]|22[3-9][0-9]|2[3-6][0-9]{2}|27[01][0-9]|2720)[0-9]{12}$"
visacard <- "\\b([4]\\d{3}[\\s]\\d{4}[\\s]\\d{4}[\\s]\\d{4}|[4]\\d{3}[-]\\d{4}[-]\\d{4}[-
]\\d{4}|[4]\\d{3}[.]\\d{4}[.]\\d{4}[.]\\d{4}|[4]\\d{3}\\d{4}\\d{4}\\d{4})\\b"
zip <- "^((\\d{5}-\\d{4})|(\\d{5})|([A-Z]\\d[A-Z]\\s\\d[A-Z]\\d))$" #^[0-9]{5}(?:-[0-9]{4})?$
url <- "(((ftp|http|https):\\/\\/)|(www\\.))([-\\w\\.\\/#$\\?=+@&%_:;]+)"
iban <- "^[a-zA-Z]{2}[0-9]{2}[a-zA-Z0-9]{4}[0-9]{7}([a-zA-Z0-9]?){0,16}$"
time <- "^(?:2[0-3]|[01]?\\d):[0-5]\\d$"#"[0-9]?[0-9]:[0-9][0-9]"
currency <- "^\\d+(?:\\.\\d{2})?$"
file_info <- "(\\\\[^\\\\]+$)|(/[^/]+$)"
dates <- "^([1][12]|[0]?[1-9])[\\/-]([3][01]|[12]\\d|[0]?[1-9])[\\/-](\\d{4}|\\d{2})$"

grepl(license_plate, "NT5-6345")
grepl(email, "cowger@gmail.com")
grepl(national_id, "612-49-2884")
grepl(ip, "192.168.1.1")
grepl(phone_number, "+1 515-372-5733")
grepl(amexcard, "372418640982660")
grepl(mastercard, "5258704108753590")
grepl(visacard, "4563-7568-5698-4587")
grepl(zip, "92501")
grepl(url, "https:\\www.wincowger.com")
grepl(iban, "NL02ABNA0123456789")
grepl(time, "23:00")
grepl(currency, "5000.00")
grepl(file_info, "the\\shdhfdk\\test.csv")
grepl(file_info, "the/shdhfdk/test.csv")
grepl(dates, "12-20-2020")
#Tests ----


#setwd("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets")

#Material_PA <= 1| Material_PA %vin% c("N/A") | Material_PA %vin% ("Present")
#api <- read.csv("ckan.csv")
#file_data = read.csv("C:/Users/winco/Downloads/Samples_Merged.copy.csv")

#files_rules = read.csv("C:/Users/winco/Downloads/Validation_Rules_Samples_Merged.copy.csv")

#list_complaints <- lapply(1:nrow(files_rules), function(x){
#    tryCatch(validator(.data=files_rules[x,]), 
#             warning = function(w) {w}, 
#             error = function(e) {e})
#})

#rules_formatted <- tryCatch(validator(.data=rules), 
#                            warning = function(w) {w}, 
#                            error = function(e) {e})

#test_rules <- validate_rules(files_rules)
#test_bad_rules <- validate_rules("rules.txt")
#test_data <- validate_data(files_data = file_data, rules = test_rules$rules)
#test_remote <- remote_share(data_formatted = test_data$data_formatted, api = api, rules = test_rules$rules, results = test_data$results)
#test_rules_2 <- validate_rules("C:/Users/winco/Downloads/rules (14).csv")
#test_invalid <- validate_data(files_data = "C:/Users/winco/Downloads/invalid_data (3).csv", rules = test_rules_2$rules)
#test_rules_broken <- rules_broken(results = test_invalid$results, show_decision = T)
#test_rows <- rows_for_rules(data_formatted = test_invalid$data_formatted, report = test_invalid$report, broken_rules = test_rules_broken, rows = 1)
