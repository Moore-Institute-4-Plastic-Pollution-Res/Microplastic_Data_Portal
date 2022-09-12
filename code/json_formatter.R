library(readxl)
library(jsonlite)

file <- read_xlsx("C:/Users/winco/Downloads/Samples (11).xlsx")

file2 <- file

file2$`Polymers (if particles arent listed individually, list all polymers found and percentages if applicable)` <- strsplit(file2$`Polymers (if particles arent listed individually, list all polymers found and percentages if applicable)`, "; ")

strsplit(file2$`Polymers (if particles arent listed individually, list all polymers found and percentages if applicable)`, "; ")

toJSON(head(file2), pretty=TRUE)














# Working JSON-LD file
{
    "@context": {
        "@version": 1.1,
        "generatedAt": {
            "@id": "http://www.w3.org/ns/prov#generatedAtTime",
            "@type": "http://www.w3.org/2001/XMLSchema#date"
        },
        "Person": {
            "@id": "http://xmlns.com/foaf/0.1/Person",
            "@type": "https://schema.org/Number"},
        "name": "http://xmlns.com/foaf/0.1/name",
        "knows": "http://xmlns.com/foaf/0.1/knows"
    },
    "generatedAt": 2022, 
    
    "@graph": [
        {
            "@id": "1.10.1016/j.watres.2017.11.011",
            "@type": "Sample",
            "DOI": "10.1016/j.watres.2017.11.011", 
            "Location": "Germany",
            "Source": "Single Use Bottles",
            "Concentration": {
                "Amount": "14",
                "Units": "particles/L"},
            "Polymers": {
                "PEST": {
                    "Amount": "59",
                    "Units": "%"}, 
                "PE": {
                    "Amount": "9",
                    "Units": "%"},
                "PP": {
                    "Amount": "1",
                    "Units": "%"},
                "PA": {
                    "Amount": "1",
                    "Units": "%"}
            },
            "Size": {
                "> 100 um":{
                    "Amount": "2",
                    "Units": "%"
                },
                "50-100 um": {
                    "Amount": "5", 
                    "Units": "%"
                },
                "20-50 um":{
                    "Amount": "22",
                    "Units": "%"
                },
                "10-20 um":{
                    "Amount": "30",
                    "Units": "%"
                },
                "5-10 um":{
                    "Amount": "41",
                    "Units": "%"
                } 
                
            }
        }, 
        {
            "@id": "A_BioTreatment_10.1016/j.scitotenv.2021.150545",
            "@type": "Sample",
            "DOI": "10.1016/j.scitotenv.2021.150545",
            "Source": "Drinking water treatment plant",
            "Subsample":[{
                "@id": "A_BioTreatment_PE.10.1016/j.scitotenv.2021.150545", 
                "Concentration": {
                    "Amount": "610", 
                    "Units": "particles/L"
                }
            },
            {
                "@id": "A_BioTreatment_PP.10.1016/j.scitotenv.2021.150545", 
                "Concentration": {
                    "Amount": "610", 
                    "Units": "particles/L"
                }
            }
            ]
        }
    ]
}