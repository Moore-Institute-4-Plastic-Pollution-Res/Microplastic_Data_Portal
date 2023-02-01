
#Tests ----

library(readxl)
library(openxlsx)

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

#Excel spreadsheet creation. 

#initiate first
create_valid_excel <- function(data_validation, 
                               negStyle = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"),
                               posStyle = createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
                               row_num = 1000,
                               file_name = "conditionalFormattingExample.xlsx"){
    column_index <- 1
    wb <- createWorkbook()
    addWorksheet(wb, "Lookup")
    for(sheet_num in 1:length(data_validation$data_names)){ #Sheet level for loop
        rules_all <- data_validation$rules[[sheet_num]]
        sheet_name <- data_validation$data_names[sheet_num]
        addWorksheet(wb, sheet_name)
        for(col_num in 1:length(rules_all)){
            rule_test <- rules_all[[col_num]]
            expression <- rule_test@expr
            column_name <- as.character(expression[2]) #Name of the column but Needs to be set to 2 to grab the data
            df <- as_tibble(rep("", row_num))
            names(df) <- column_name
            writeData(wb, sheet = sheet_name, x = df, startCol = column_index)
            if(any(grepl("%vin%", expression))){
                values <- unlist(strsplit(gsub('(")|(\\))|(c\\()', "", as.character(expression[3])), ", "))
                lookup_col <- LETTERS[column_index] 
                df_lookup <- tibble(values)
                names(df_lookup) <- paste0(column_name, "_lookup")
                writeData(wb, 
                          sheet = "Lookup", 
                          x = df_lookup, 
                          startCol = column_index)
                dataValidation(wb, 
                               sheet = sheet_name, 
                               cols = column_index, 
                               rows = 2:row_num,
                               type = "list", 
                               value = paste0("Lookup!$", lookup_col, "$2:$", lookup_col, "$", length(values) +1))   
            }
            if(any(grepl("is_unique", expression))){
                conditionalFormatting(wb, 
                                      sheet_name, 
                                      cols = column_index, 
                                      rows = 2:row_num, 
                                      type = "duplicates", 
                                      style = negStyle)
            }
            column_index = column_index + 1
        }
    }
    saveWorkbook(wb, file_name, TRUE)
    wb
}

wb <- create_valid_excel(data_validation = data_validation)
openXL(wb)


for(value in values){
    conditionalFormatting(wb, sheet_name, cols = col_num, rows = 1:1000, type = "contains", rule = value, style = posStyle)
}

create_conditional_excel <- function(rules){
    if(grepl("%vin%", rule_test@expr))
}

variables(rule_test)
data_validation$results[[1]]

wb <- createWorkbook()
addWorksheet(wb, "cellIs")
addWorksheet(wb, "Moving Row")
addWorksheet(wb, "Moving Col")
addWorksheet(wb, "Dependent on 1")
addWorksheet(wb, "Duplicates")
addWorksheet(wb, "containsText")
addWorksheet(wb, "colourScale", zoom = 30)
addWorksheet(wb, "databar")

## cells containing text
fn <- function(x) paste(sample(LETTERS, 10), collapse = "-")
writeData(wb, "containsText", sapply(1:10, fn))
conditionalFormatting(wb, "containsText", cols = 1, rows = 1:10, type = "contains", rule = "A")


negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

## rule applies to all each cell in range
writeData(wb, "cellIs", -5:5)
writeData(wb, "cellIs", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "cellIs", cols=1, rows=1:11, rule="!=0", style = negStyle)
conditionalFormatting(wb, "cellIs", cols=1, rows=1:11, rule="==0", style = posStyle)

## highlight row dependent on first cell in row
writeData(wb, "Moving Row", -5:5)
writeData(wb, "Moving Row", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "Moving Row", cols=1:2, rows=1:11, rule="$A1<0", style = negStyle)
conditionalFormatting(wb, "Moving Row", cols=1:2, rows=1:11, rule="$A1>0", style = posStyle)

## highlight column dependent on first cell in column
writeData(wb, "Moving Col", -5:5)
writeData(wb, "Moving Col", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "Moving Col", cols=1:2, rows=1:11, rule="A$1<0", style = negStyle)
conditionalFormatting(wb, "Moving Col", cols=1:2, rows=1:11, rule="A$1>0", style = posStyle)

## highlight entire range cols X rows dependent only on cell A1
writeData(wb, "Dependent on 1", -5:5)
writeData(wb, "Dependent on 1", LETTERS[1:11], startCol=2)
conditionalFormatting(wb, "Dependent on 1", cols=1:2, rows=1:11, rule="$A$1<0", style = negStyle)
conditionalFormatting(wb, "Dependent on 1", cols=1:2, rows=1:11, rule="$A$1>0", style = posStyle)

## highlight duplicates using default style
writeData(wb, "Duplicates", sample(LETTERS[1:15], size = 10, replace = TRUE))
conditionalFormatting(wb, "Duplicates", cols = 1, rows = 1:10, type = "duplicates")

## cells containing text
fn <- function(x) paste(sample(LETTERS, 10), collapse = "-")
writeData(wb, "containsText", sapply(1:10, fn))
conditionalFormatting(wb, "containsText", cols = 1, rows = 1:10, type = "contains", rule = "A")

## Databars
writeData(wb, "databar", -5:5)
conditionalFormatting(wb, "databar", cols = 1, rows = 1:12, type = "databar") ## Default colours

saveWorkbook(wb, "conditionalFormattingExample.xlsx", TRUE)

openXL(wb)




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