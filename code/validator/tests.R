
#Tests ----

library(readxl)

file_rules = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/www/rules.xlsx"
files_data = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/www/data_success.xlsx"

sheets <- readxl::excel_sheets(file_rules)
all <- readxl::read_excel(file_rules, sheet = sheets)

files_data = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/www/data_success.csv"
file_rules = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/www/rules.csv"

data_validation <- validate_data(files_data = files_data, file_rules = file_rules)

broken <- rules_broken(results = data_validation$results[[1]], show_decision = T) %>%
                select(description, status, expression, name) %>%
                mutate(description = as.factor(description))

#setwd("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets")

#Material_PA <= 1| Material_PA %vin% c("N/A") | Material_PA %vin% ("Present")
#api <- read.csv("ckan.csv")
#files_data = c("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/AccreditedLabs/particles.csv", "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/AccreditedLabs/methodology.csv", "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/AccreditedLabs/samples.csv")
#file_rules = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/AccreditedLabs/rules_all.csv"
#files_data = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets/data_success_secret.csv"
#files_rules = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets/rules_secret.csv"


#list_complaints <- lapply(1:nrow(files_rules), function(x){
#    tryCatch(validator(.data=files_rules[x,]), 
#             warning = function(w) {w}, 
#             error = function(e) {e})
#})

#rules_formatted <- tryCatch(validator(.data=rules), 
#                            warning = function(w) {w}, 
#                            error = function(e) {e})

#test_rules <- validate_rules(files_rules)

#test_data <- validate_data(files_data = files_data, file_rules = file_rules)

#req(all(test_data$results$status == "success"))
#req("KEY" %in% names(validation()$data_formatted))

#api <- read.csv("secrets/ckan.csv")


#variables(test_rules$rules)[variables(test_rules$rules) != "DOI"]
#(test_data$data_formatted$Approximate_Lattitude == "N/A" | suppressWarnings(as.numeric(test_data$data_formatted$Approximate_Lattitude) > -90 & as.numeric(test_data$data_formatted$Approximate_Lattitude) < 90)) & !is.na(test_data$data_formatted$Approximate_Lattitude)
#test_rules$message
#test_data$status
#test_data$results
#test_data$data_formatted$Approximate_Lattitude
#test_bad_rules <- validate_rules("rules.txt")
#test_remote <- remote_share(data_formatted = test_data$data_formatted, api = api, rules = test_rules$rules, results = test_data$results)
#test_rules_2 <- validate_rules("C:/Users/winco/Downloads/rules (14).csv")
#test_invalid <- validate_data(files_data = "C:/Users/winco/Downloads/invalid_data (3).csv", rules = test_rules_2$rules)
#test_rules_broken <- rules_broken(results = test_invalid$results, show_decision = T)
#test_rows <- rows_for_rules(data_formatted = test_invalid$data_formatted, report = test_invalid$report, broken_rules = test_rules_broken, rows = 1)
#RIP LIL PEEP TEST BY GABRIEL