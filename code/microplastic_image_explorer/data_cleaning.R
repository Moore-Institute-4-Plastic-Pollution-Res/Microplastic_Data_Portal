library(readxl)
library(googledrive)
library(dplyr)
library("jpeg")
library("tiff")

#Info we want Timestamp	Image File	Researcher Name	Affiliation	Citation	Instrument name	Analysis Date	Polymer-type of particle	Magnification	Color	Morphology	Size of particle	Size dimension




#Leah ----
jpegs <- list.files("C:/Users/winco/Downloads/Leah", pattern = ".jpg", recursive = T, full.names = T)
shortjpegs <- gsub(".jpg", "", gsub(".*/", "", jpegs))
info <- read.csv("C:/Users/winco/Downloads/tbl_qa_master.csv")
microscopy <- read.csv("C:/Users/winco/Downloads/tbl_microscopysettings.csv")
info_clean <- info %>%
    left_join(microscopy, by = "objectid") %>%
    mutate(file = paste0(particleid, ".jpg")) %>%
    mutate(timestamp = NA) %>%
    mutate(citation = "SCCWRP Interlaboratory Comparison 2020-2022",
           instrument = NA, 
           affiliation = "SCCWRP", 
           researcher = paste0("Lab ", labid.x), 
           analysis_date = "2020-2022", 
           dimension = "nominal")

sum(shortjpegs %in% info$particleid)
cleanedjpgs <- jpegs[shortjpegs %in% info$particleid]

for(item in 1:length(cleanedjpgs)){
    file.copy(from = cleanedjpgs[item], to = paste0("C:/Users/winco/Downloads/LeahClean/", shortjpegs[item], ".jpg"))
}

drive_deauth()
files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
leahfiles <- files[,c("name", "id")] %>%
    inner_join(info_clean, by = c("name" = "file")) %>%
    select(timestamp, id, researcher, affiliation, citation, instrument, analysis_date, qa_chemid, magnification, qa_color, qa_morphology, sizefraction, dimension)

write.csv(leahfiles, "data/leah.csv")

# AnnaK ----
annak <- read_xlsx(path = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/microplastic_image_explorer/extra_data/Photos_data.xlsx") %>%
    mutate(name = paste0(`Image File`, ".jpg"))

files_in_list <- list.files("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/Annak", pattern = ".tif", full.names = T)
files_in_list_names <- list.files("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/Annak", pattern = ".tif")


for(image in 1:length(files_in_list)){
    img <- readTIFF(files_in_list[image], native=TRUE)
    writeJPEG(img, target = paste0("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/Annak/",gsub("\\..*", "", files_in_list_names[image]), ".jpg"), quality = 1)
}

drive_deauth()
files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
annafiles <- files[,c("name", "id")] %>%
    right_join(annak)

write.csv(annafiles, "data/annak.csv")



# Fadare ----
fadare <- read_xlsx(path = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/microplastic_image_explorer/extra_data/Fadare and Conkle MP Taxonomy.xlsx")

drive_deauth()
files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
fadarefiles <- files[1:52,c("name", "id")]

joined <- left_join(fadare %>%
                        rename(name = Filename) %>%
                        mutate(name = gsub(" .*", "", name)), 
                    fadarefiles %>%
                        mutate(name = gsub(".jpg", "", name)) %>%
                        mutate(name = gsub(" .*", "", name)))
write.csv(joined, "data/fadare.csv")

#Algalita ----
algalita <- read_xlsx(path = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/microplastic_taxonomy/extra_data/MethodEvaluationStudy_ALGALITA.xlsx", sheet = "tbl_rawdata")

file <- tryCatch(algalita %>%
        slice_sample(n = 0),
    warning = function(w) {w}, error = function(e) {e})

drive_deauth()
files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
algalita_bind <- files %>%
    select(name, id) %>%
    mutate(PhotoID = gsub(".JPG", "" , name, ignore.case = T)) %>%
    right_join(algalita) %>%
    mutate(Researcher = "Charles Moore and Gwen Lattin", 
           Affilitation = "Moore Institute for Plastic Pollution Research", 
           Citation = "Lattin and Moore 2020, Interlab Comparison Study Data, Moore Institute for Plastic Pollution Research", 
           Instrument = "Nikon SMZ1270", 
           Magnification = "60-80x",
           Analysis = "2020",
           Polymer = NA,
           Dimension = "Nominal") %>%
    select(id, Researcher, Affilitation, Citation, Instrument, Analysis, Polymer, Magnification, Color, Morphology, SizeFraction, Dimension)

write.csv(algalita_bind, "data/algalita.csv")

filtered <- file %>% 
    filter(if(input$color != "ALL") tolower(`Color of particle`) == tolower(input$color) else !is.na(images)) %>%
    filter(if(input$morphology != "ALL") tolower(`Morphology of particle`) == tolower(input$morphology) else !is.na(images)) %>%
    filter(if(input$polymer != "ALL") tolower(`Polymer-type of particle`) == tolower(input$polymer) else !is.na(images)) %>%
    slice_sample(n= if(nrow(.) > 100) 100 else if(nrow(.) == 0) 1 else nrow(.)) 

    