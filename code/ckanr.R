#install.packages("ckanr")
#https://github.com/ropensci/ckanr

library('ckanr')

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
file <- ckan_fetch("https://data.ca.gov/dataset/27b12123-c913-42c9-81f8-a7d1cbeb22d0/resource/707b9fff-441b-42ba-929e-38827e64880b/download/forbes-global-2000-2019.csv")

# create a package not allowed with current permissions. 
res <- package_create("test", author="wincowger")

# then create a resource
file <- system.file("examples", "actinidiaceae.csv", package = "ckanr")
xx <- resource_create(package_id = res$id,
                       description = "my resource",
                       name = "bears",
                       upload = file,
                       extras = list(species = "grizzly"),
                       rcurl = "http://google.com"
)

package_create("foobbbbbarrrr") %>%
    resource_create(description = "my resource",
                    name = "bearsareus",
                    upload = file,
                    extras = list(my_extra = "some value"),
                    rcurl = "http://google.com")


#Richard Shared Code
#library(ckanr) #write to portal#### # get the data portal API key saved in the local environment (it's available on data.ca.gov by going to your user profile) 
#portal_key <- Sys.getenv('portal_key') # set the ckan defaults 
resourceID <- '707b9fff-441b-42ba-929e-38827e64880b' 
ckan_resource_info <- resource_show(id = resourceID, as = 'table')
path <- system.file("examples", "actinidiaceae.csv", package = "ckanr")
file_upload <- ckanr::resource_update(id = resourceID, path = path)
#Will update the file and tell you that it has actually been updated with the assignment. 

