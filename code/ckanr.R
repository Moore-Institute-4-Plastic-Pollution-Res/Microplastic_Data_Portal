#install.packages("ckanr")
#https://github.com/ropensci/ckanr

library('ckanr')
library(data.table)
library(dplyr)

api <- read.table("secrets/ckan.txt", sep = ",")
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
resourceID <- '707b9fff-441b-42ba-929e-38827e64880b' 
ckan_resource_info <- resource_show(id = resourceID, as = 'table')
path <- system.file("examples", "actinidiaceae.csv", package = "ckanr")
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
