#setwd("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets")

#api <- read.csv("ckan.csv")
#file_rules = "rules_secret.csv"

#test_rules <- validate_rules("rules_secret.csv")
#test_data <- validate_data(files_data = "data_success_secret.csv", rules = test_rules)
#test_remote <- remote_share(data_formatted = test_data$data_formatted, api = api, rules = test_rules, results = test_data$results)
#files_data = "data_success_secret.csv"


validate_rules <- function(file_rules){
    if (!grepl("(\\.csv$)", ignore.case = T, as.character(file_rules))) {
        #reset("file")
        return(data.table(
            title = "Data type not supported!",
            text = paste0("Uploaded data type is not currently supported; please upload a .csv file."),
            type = "warning"))
    }
    
    rules <- read.csv(file_rules, fileEncoding = "UTF-8")
    
    if (!all(c("name", "description", "severity", "rule") %in% names(rules))) {
        #reset("file")
        return(data.table(
            title = "Data type not supported!",
            text = paste0('Uploaded rules format is not currently supported, please provide a rules file with column names, "name", "description", "severity", "rule"'),
            type = "warning"))
    }
    
    if (!all(unlist(lapply(rules, class)) %in% "character")) {
        #reset("file")
        return(data.table(
            title = "Data type not supported!",
            text = paste0('Uploaded rules format is not currently supported, please provide a rules file with columns that are all character type.'),
            type = "warning"))
    }
    
    rules_formatted <- tryCatch(validator(.data=rules), 
                                warning = function(w) {w}, 
                                error = function(e) {e})
    
    if (inherits(rules_formatted, "simpleWarning") | inherits(rules_formatted, "simpleError")){
        return(data.table(
            title = "Something else went wrong with reading the rules file.",
            text = paste0("There was an error that said ", rules_formatted$message),
            type = "error"
            )
        )
    }
    return(rules_formatted)
}


validate_data <- function(files_data, rules){
    # Read in data when uploaded based on the file type
    if (!all(grepl("(\\.csv$)", ignore.case = T, as.character(files_data)))) {
        return(data.table(
            title = "Data type not supported!",
            text = paste0("Uploaded data type is not currently supported; please
                      upload a .csv file."),
            type = "warning"))
    }
    if(is.null(rules)) {
        return(data.table(
            title = "Need Rules File",
            text = paste0("You must upload a rules file before uploading a data file to validate."),
            type = "warning"))
    }
    data_formatted <- tryCatch(lapply(files_data, function(x) read.csv(x, fileEncoding = "UTF-8")) %>% 
                         reduce(full_join),
                            warning = function(w) {w}, error = function(e) {e})
    
    if (inherits(data_formatted, "simpleWarning") | inherits(data_formatted, "simpleError")){
        return(data.table(
            title = "Something went wrong with the merge.",
            text = paste0("This tool expects at least one column in each dataset with the same name to merge on. There was also an error that said ", sout$message),
            type = "error"
        ))
    }
    if(!all(variables(rules) %in% names(data_formatted)) | !all(names(data_formatted) %in% variables(rules))){
        return(data.table(
            title = "Rules and data mismatch",
            text = paste0("All variables in the rules csv (", paste(variables(rules), collapse = ","), ") need to be in data csv (",  paste(names(data_formatted), collapse = ","), ") and vice versa for the validation to work."),
            type = "error"))
    }
    report <- confront(data_formatted, rules)
    results <- summary(report) %>%
        mutate(status = ifelse(fails > 0 | error | warning , "error", "success")) %>%
        mutate(description = meta(rules)$description)
    
    return(list(data_formatted = data_formatted, report = report, results = results, rules = rules))
}


remote_share <- function(data_formatted, api, rules, results){
    if(any(results$status == "error")){
        return(data.table(
            title = "Errors Prevent Upload",
            text = "There are errors in the dataset that persist. Until all errors are remedied, the data cannot be uploaded to the remote repository.",
            type = "error"))
    }
    if(any(unlist(lapply(data_formatted %>% select(-KEY), function(x) any(x %in% unique(data_formatted$KEY)))))){
        return(data.table(
            title = "Secret Key is misplaced",
            text = "The secret key is in locations other than the KEY column, please remove the secret key from any other locations.",
            type = "error"))
    }
    if("KEY" %in% names(data_formatted) & length(unique(data_formatted$KEY)) != 1){
        return(data.table(
            title = "Multiple Secret Keys",
            text = paste0("There should only be one secret key per data upload, but these keys are in the data (", paste(unique(data_formatted$KEY), collapse = ","), ")"),
            type = "error"))
    }
    if(!any(unique(data_formatted$KEY) %in% api$VALID_KEY)){
        return(data.table(
            title = "Secret Key is not valid",
            text = "Any column labeled KEY is considered a secret key and should have a valid pair in our internal database.",
            type = "error"))
    }
    if(!any(digest(as.data.frame(rules) %>% select(-created)) %in% api$VALID_RULES)){
        return(data.table(
            title = "Rules file is not valid",
            text = "If you are using a key column to upload data to a remote repo then there must be a valid pair with the rules you are using in our internal database.",
            type = "error"))
    }
    api_info <- api %>%
        filter(VALID_KEY == unique(data_formatted$KEY) & VALID_RULES == digest(as.data.frame(rules) %>% select(-created)))
    
    if(nrow(api_info) != 1){
        return(data.table(
            title = "Mismatched rules file and KEY column",
            text = "The secret key and rules file must be exact matches to one another. One secret key is for one rules file.",
            type = "error"))
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
    return(creation)
}

