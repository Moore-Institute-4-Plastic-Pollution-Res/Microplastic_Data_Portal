
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
            text = paste0("All variables in the rules csv (", paste(variables(rules), collapse = ","), ") need to be in data csv (",  paste(names(data_formatted), collapse = ","), ") and vice versa for the validation to work."),
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



#Tests ----

#setwd("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets")

#api <- read.csv("ckan.csv")
#file_data = "C:/Users/winco/Downloads/Samples_Merged.copy.csv"
#files_rules = "C:/Users/winco/Downloads/Validation_Rules_Samples_Merged.copy.csv"

#test_rules <- validate_rules(files_rules)
#test_bad_rules <- validate_rules("rules.txt")
#test_data <- validate_data(files_data = file_data, rules = test_rules$rules)
#test_remote <- remote_share(data_formatted = test_data$data_formatted, api = api, rules = test_rules$rules, results = test_data$results)
#test_rules_2 <- validate_rules("C:/Users/winco/Downloads/rules (14).csv")
#test_invalid <- validate_data(files_data = "C:/Users/winco/Downloads/invalid_data (3).csv", rules = test_rules_2$rules)
#test_rules_broken <- rules_broken(results = test_invalid$results, show_decision = T)
#test_rows <- rows_for_rules(data_formatted = test_invalid$data_formatted, report = test_invalid$report, broken_rules = test_rules_broken, rows = 1)
