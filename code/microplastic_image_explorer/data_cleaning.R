library(readxl)
library(googledrive)
library(dplyr)
library("jpeg")
library("tiff")


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

    