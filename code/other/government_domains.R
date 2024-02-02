library(dplyr)
library(data.table)

domains <- fread("data/current-full_gov_domains.csv")

skimr::skim(domains)

unique(domains$State)

ca_domains <- domains %>%
    filter(Organization == "State of California")
    #filter(`Domain Type` == "State") %>%
    #filter(State == "CA")
