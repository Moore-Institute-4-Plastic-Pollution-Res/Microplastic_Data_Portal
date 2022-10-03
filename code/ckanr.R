#install.packages("ckanr")
#https://github.com/ropensci/ckanr

library(ckanr)
library(data.table)
library(dplyr)
library(validate)
library(digest)
library(detector)


decimalplaces()
unlist(lapply(c(0.058343413, 0.23432834), function(x) decimalplaces(x))) >= 5

#detect PII
any(detector::detect(test)[,-1])

test <- read.csv("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets/data_success_secret.csv", fileEncoding = "UTF-8")test <- read.csv("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets/data_success_secret.csv", fileEncoding = "UTF-8")
test <- read.csv("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets/rules_secret.csv", fileEncoding = "UTF-8")



digest(validator(.data=test), seed = 3)
checking <- tryCatch(validator(.data=test),
         warning = function(w) {w}, error = function(e) {e})

inherits(checking, "simpleWarning")

data <- api %>%
    filter(VALID_KEY == "8a49f228a877ec3654dfa5ff6aa57849" & VALID_RULES == digest(as.data.frame(validator(.data=test)) %>% select(-created)))

digest(rules)
digest(test)

api <- read.csv("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/validator/secrets/ckan.csv")
ckanr_setup(url = "https://data.ca.gov/", key = api$V1)
#ckanr_setup(url = "[https://data.ca.gov/](https://data.ca.gov/)", key = api$V1) 


#Current info on the server
ckan_info()
ckanr::ckan_version()
changes(limit = 2, as = "table")
ckanr::ckanr_settings()
package_list(as = "table")
tag_list('aviation', as = 'table')

#Check your info
activities <- ckanr::dashboard_activity_list()
ckanr::dashboard_count()

#Working, gathered from URL Under code. 
#Ckan does not like the word none, defaults to NA, may not always be what is intended. 
#Richard Shared Code
#library(ckanr) #write to portal#### # get the data portal API key saved in the local environment (it's available on data.ca.gov by going to your user profile) 
#portal_key <- Sys.getenv('portal_key') # set the ckan defaults 
resource_id <- '707b9fff-441b-42ba-929e-38827e64880b' 
package_id <- "microplastics_data_portal"
path <- system.file("examples", "actinidiaceae.csv", package = "ckanr")
creation_details <- resource_create(package_id = package_id,
                      description = "my resource",
                      name = "bears2",
                      upload = path)
ckan_resource_info <- resource_show(id = resource_id, as = 'table')
file_upload <- ckanr::resource_update(id = resourceID, path = path)
file <- ckan_fetch(ckan_resource_info$url)
testfile <- read.csv(path) #Periods get parsed as spaces in ckan. 
#ckan_resource_info$last_modified

#Will update the file and tell you that it has actually been updated with the assignment. 
ckan <- src_ckan("https://data.ca.gov/")

testdplyr <- dplyr::tbl(src = ckan$con, from = resourceID) %>% 
    select(Family) # %>% 
    #filter(HBT2014 == "S08000015") %>% 
    as_tibble()


#Works pretty well for data download with filters. 
test <- ds_search(resource_id = resourceID, filters = '{"Confidence level": "M"}', as = "table", limit = 100000)
records <- test$records

?ds_search()

ds_create(resource_id = resourceID)
# Could go with this in the long run when the dataset starts getting really large. ?ckanr::ds_create()
# Also a minimal example of something a little more advanced here: https://github.com/ropensci/ckanr/pull/102
# Another useful strategy https://www.r-bloggers.com/2019/11/trying-the-ckanr-package/ 


#test api validate

rules <- read.csv("G:/My Drive/MooreInstitute/Projects/Water PACT/rules.csv", fileEncoding = "UTF-8")
df1 <- read.csv("G:/My Drive/MooreInstitute/Projects/Water PACT/Proposed_WaterPACT_Datasharing_WC_Test_success_samples.csv", fileEncoding = "UTF-8")
df2 <- read.csv("G:/My Drive/MooreInstitute/Projects/Water PACT/Proposed_WaterPACT_Datasharing_WC_Test_success_equipment.csv", fileEncoding = "UTF-8")

files = "G:/My Drive/MooreInstitute/Projects/Water PACT/Proposed_WaterPACT_Datasharing_WC_Test_success_samples.csv"

sout <- tryCatch(lapply(files, function(file) read.csv(file, fileEncoding = "UTF-8")) %>% 
                     reduce(full_join),
                 warning = function(w) {w}, error = function(e) {e})


joined <- full_join(df1, df2)

validation_summary <- validator(.data=rules)
variables(validation_summary)

function(file) read.csv(file, fileEncoding = "UTF-8")
files <- c("G:/My Drive/MooreInstitute/Projects/Water PACT/Proposed_WaterPACT_Datasharing_WC_Test_success_equipment.csv", "G:/My Drive/MooreInstitute/Projects/Water PACT/Proposed_WaterPACT_Datasharing_WC_Test_success_samples.csv")

examplelist <- lapply(files, function(file) read.csv(file, fileEncoding = "UTF-8"))
library(purrr)
joined <- examplelist %>% reduce(full_join)

lapply(examplelist, names)

data <- read.csv("G:/My Drive/MooreInstitute/Projects/Water PACT/Proposed_WaterPACT_Datasharing_WC_Test_success_samples.csv")
library(stringr)

str_detect("09/9/2020", "^(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)(?:0?[13-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$|^(?:29(\\/|-|\\.)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$")

as.Date("9-9-2020", format = "%Y-%m-%d")

as.Date("2020-9-9", format = "%Y-%m-%d") > as.Date("2020-9-1", format = "%Y-%m-%d")
str_detect("09/9/2020", "^(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)(?:0?[13-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$|^(?:29(\\/|-|\\.)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$")

number_format(10, max_dig=1)

in_range("15:00:00", min="15:30:00", max = "16:00:00", format = "%H:%M:%S")

in_range("A", min="05:00:00", max = "20:00:00", format = "%H:%M:%S")

ddata <- read.csv("C:/Users/winco/OneDrive/Documents/test_validate/valid_data (3).csv")
validation <- read.csv("C:/Users/winco/OneDrive/Documents/test_validate/rules (10).csv")

testnames <- names(data)[names(data) != "KEY"]
data2 <- data %>% select(-KEY)
MISSING_KEY <- unique(data$KEY)
rules <- validator(!. %in% KEY)
report <- confront(data2, rules, ref = list(KEY = MISSING_KEY))
any(unlist((lapply(data %>% select(-KEY), function(x) any(x %in% MISSING_KEY)))))

rules <- validator(G:=var_group(names_ref), G != KEY)
report <- confront(data, rules,  ref = list(names_ref = testnames))

results <- summary(report) %>%
    mutate(status = ifelse(fails > 0 | error | warning , "error", "success")) #%>%
    mutate(description = meta(rules)$description)
