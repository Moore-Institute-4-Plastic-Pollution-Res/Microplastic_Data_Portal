
#Tests ----

library(readxl)
library(openxlsx)

file_rules = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/www/rules.xlsx"
files_data = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/www/data_success.xlsx"

sheets <- readxl::excel_sheets(file_rules)
all <- readxl::read_excel(file_rules, sheet = sheets)

files_data = paste0("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/AccreditedLabs/", c("samples.csv", "particles.csv", "methodology.csv"))
file_rules = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/www/rules_dw_acc.csv"

data_validation <- validate_data(files_data = files_data, file_rules = file_rules)

broken <- rules_broken(results = data_validation$results[[1]], show_decision = T) %>%
                select(description, status, expression, name) %>%
                mutate(description = as.factor(description))

#https://gamagroundwater.waterboards.ca.gov/gama/datadownload
#Excel spreadsheet creation. 
data_validation$rules[[2]]
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
    for(sheet_num in 1:length(data_names)){ #Sheet level for loop
        rules_all <- validator(.data = rules %>% filter(dataset == data_names[sheet_num]))
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
            if(any(grepl("!is.na\\(.*\\)", expression))){ #Not working yet.
                dataValidation(wb, 
                                      sheet_name, 
                                      cols = column_index, 
                                      rows = 2:row_num, 
                                      type = "textLength", 
                                      operator = "greaterThanOrEqual",
                                      value = "1",
                                      allowBlank = F)
            }
            if(any(grepl("in_range\\(.*\\)", expression))){
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

wb <- create_valid_excel(file_rules = file_rules)

openXL(wb)


rules_all <- data_validation$rules[[1]]
sheet_name <- data_validation$data_names[sheet_num]
rule_test <- rules_all[[col_num]]
expression <- rule_test@expr

for(value in values){
    conditionalFormatting(wb, sheet_name, cols = col_num, rows = 1:1000, type = "contains", rule = value, style = posStyle)
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