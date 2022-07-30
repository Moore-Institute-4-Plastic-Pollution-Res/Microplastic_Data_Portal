#install.packages("ckanr")
#https://github.com/ropensci/ckanr

library('ckanr')

api <- read.table("secrets/ckan.txt")
ckanr_setup(url = "https://data.ca.gov/", key = api$V1)

#Current info on the server
ckan_info()
ckanr::ckan_version()
changes(limit = 2, as = "table")[, 1:4]
ckanr::ckanr_settings()
package_list(as = "table")
tag_list('aviation', as = 'table')

#Check your info
ckanr::dashboard_activity_list()
ckanr::dashboard_count()

file <- ckan_fetch("https://data.cnra.ca.gov/dataset/78ac95ac-9665-4e53-aea6-ad0762055cce/resource/4e6aaf6f-7055-484b-84f4-3a66df294f27/download/2020-09-11_microparticledata.xlsx")

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

