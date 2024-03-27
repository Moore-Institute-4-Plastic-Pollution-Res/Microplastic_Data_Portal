library(readxl)
library(googledrive)
library(dplyr)
library("jpeg")
library("tiff")
library(magick)
library(data.table)

#Info we want Timestamp	Image File	Researcher Name	Affiliation	Citation	Instrument name	Analysis Date	Polymer-type of particle	Magnification	Color	Morphology	Size of particle	Size dimension

#Convert images to thumbnails

#Leah ----
jpegs <- list.files("C:/Users/winco/OneDrive/Documents/Images_MicroplasticImageExplorer/Leah", pattern = ".jpg", recursive = T, full.names = T)
shortjpegs <- gsub(".jpg", "", gsub(".*/", "", jpegs))
info <- read.csv("C:/Users/winco/OneDrive/Documents/Images_MicroplasticImageExplorer/tbl_qa_master.csv")
microscopy <- read.csv("C:/Users/winco/OneDrive/Documents/Images_MicroplasticImageExplorer/tbl_microscopysettings.csv")
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
cleanedshortjpgs <- shortjpegs[shortjpegs %in% info$particleid]

#for(item in 1:length(cleanedjpgs)){
#    file.copy(from = cleanedjpgs[item], to = paste0("C:/Users/winco/OneDrive/Documents/Images_MicroplasticImageExplorer/LeahClean/", cleanedshortjpgs[item], ".jpg"))
#}

#drive_deauth()
#files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
#leahfiles <- files[,c("name", "id")] %>%
#    inner_join(info_clean, by = c("name" = "file")) %>%
#    select(file, timestamp, id, researcher, affiliation, citation, instrument, analysis_date, qa_chemid, magnification, qa_color, qa_morphology, sizefraction, dimension)

#write.csv(leahfiles, "data/leah.csv")

leah_files_raw <- info_clean %>%
    select(file, researcher, affiliation, citation, instrument, analysis_date, qa_chemid, magnification, qa_color, qa_morphology, sizefraction, dimension)


# AnnaK ----
annak <- read_xlsx(path = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/microplastic_image_explorer/extra_data/Photos_data.xlsx") %>%
    mutate(name = paste0(`Image File`, ".jpg"))

files_in_list <- list.files("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/Annak", pattern = ".tif", full.names = T)
files_in_list_names <- list.files("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/Annak", pattern = ".tif")


#for(image in 1:length(files_in_list)){
#    img <- readTIFF(files_in_list[image], native=TRUE)
#    writeJPEG(img, target = paste0("G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/data/Annak/",gsub("\\..*", "", files_in_list_names[image]), ".jpg"), quality = 1)
#}

#drive_deauth()
#files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
#annafiles <- files[,c("name", "id")] %>%
#    right_join(annak)

#write.csv(annafiles, "data/annak.csv")


# Fadare ----
fadare <- read_xlsx(path = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/microplastic_image_explorer/extra_data/Fadare and Conkle MP Taxonomy.xlsx")

#drive_deauth()
#files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
#fadarefiles <- files[1:52,c("name", "id")]

#joined <- left_join(fadare %>%
#                        rename(name = Filename) %>%
#                        mutate(name = gsub(" .*", "", name)), 
#                    fadarefiles %>%
#                        mutate(name = gsub(".jpg", "", name)) %>%
#                        mutate(name = gsub(" .*", "", name)))
#write.csv(joined, "data/fadare.csv")

#Algalita ----
algalita <- read_xlsx(path = "G:/My Drive/MooreInstitute/Projects/PeoplesLab/Code/Microplastic_Data_Portal/code/microplastic_image_explorer/extra_data/MethodEvaluationStudy_ALGALITA.xlsx", sheet = "tbl_rawdata")  %>%
    mutate(Researcher = "Charles Moore and Gwen Lattin", 
           Affilitation = "Moore Institute for Plastic Pollution Research", 
           Citation = "Lattin and Moore 2020, Interlab Comparison Study Data, Moore Institute for Plastic Pollution Research", 
           Instrument = "Nikon SMZ1270", 
           Magnification = "60-80x",
           Analysis = "2020",
           Polymer = NA,
           Dimension = "Nominal")

file <- tryCatch(algalita %>%
        slice_sample(n = 0),
    warning = function(w) {w}, error = function(e) {e})

#drive_deauth()
#files <- drive_ls(drive_get(as_id("https://drive.google.com/drive/folders/103OUoOpOqxgn06fJA2Rq38SjFgfdejbRcvanC9u2juKqelwmgzrL0f7xI8T9G-_z7r6XbAeb")))
#algalita_bind <- files %>%
#    select(name, id) %>%
#    mutate(PhotoID = gsub(".JPG", "" , name, ignore.case = T)) %>%
#    right_join(algalita) %>%
#    select(id, Researcher, Affilitation, Citation, Instrument, Analysis, Polymer, Magnification, Color, Morphology, SizeFraction, Dimension)

#write.csv(algalita_bind, "data/algalita.csv")

# Join the files ----
test <- gsub("\\..{1,4}$", "", list.files("G:\\My Drive\\MooreInstitute\\Projects\\PeoplesLab\\Data\\SEM Data Sharing (File responses)\\Image File (File responses)"))

algalita2 <- algalita %>%
    rename(file_name = ParticleID, 
           citation = Citation, 
           color = Color, 
           morphology = Morphology,
           polymer = Polymer) %>%
    mutate(size = as.character((Length+Width)/2), 
           type = "visual microscopy", 
           researcher = "Gwen Lattin and Charles Moore") %>%
    select(file_name, citation, color, morphology, polymer, size, type, researcher)


annak2 <- annak %>%
    rename(file_name = name, 
           color = `Color of particle`, 
           morphology = `Morphology of particle`, 
           polymer = `Polymer-type of particle`,
           size = `Size of particle`) %>%
    mutate(type = "visual microscopy",
           researcher = "Anna Kukkola", 
           citation = "Kukkola et al. 2022")%>%
    select(file_name, citation, color, morphology, polymer, size, type, researcher)

fadare2 <- fadare %>%
    rename(file_name = Filename, 
           citation = Citation,
           color = `Color of particle`, 
           morphology = `Morphology of particle`,
           polymer = `Polymer-type of particle`, 
           size = `Size of particle`) %>%
    mutate(file_name = gsub(" .*", "", file_name)) %>%
    left_join(data.frame(file_name = gsub(" .*", "", test), id = test)) %>%
    mutate(file_name = id) %>%
    mutate(type = "visual microscopy",
           researcher = "Fadare and Conkle")%>%
    select(file_name, citation, color, morphology, polymer, size, type, researcher)

leah2 <- leah_files_raw %>%
    rename(file_name = file, 
           color = qa_color, 
           morphology = qa_morphology, 
           polymer = qa_chemid, 
           size = sizefraction) %>%
    mutate(type = "visual microscopy") %>%
    select(file_name, citation, color, morphology, polymer, size, type, researcher)


joined <- bind_rows(leah2, fadare2, algalita2, annak2) %>%
    mutate(file_name = gsub("\\..{1,4}$", "", file_name)) %>%
    filter(file_name %in% test)

table(joined$citation)


#AWS
library(aws.s3)


config <- config::get(file = "code/validator/config.yml")

#Data checks ----
Sys.setenv(
    "AWS_ACCESS_KEY_ID" = config$s3_key_id,
    "AWS_SECRET_ACCESS_KEY" = config$s3_secret_key,
    "AWS_DEFAULT_REGION" = config$s3_region
)

# Step 3: List the files in a bucket (Replace 'your-bucket-name' with your actual bucket name)
bucket_contents <- get_bucket(bucket = "microplasticimagespublic", max = 20000)

# Printing the names of the files in the bucket
file_names <- sapply(bucket_contents, function(x) x$Key)

joined2 <- joined %>%
    left_join(data.frame(file_name = gsub("\\..{1,4}$", "", file_names), file_names)) %>%
    select(-file_name) %>%
    mutate(across(color:type, toupper))

fwrite(joined2, "code/microplastic_image_explorer/image_metadata.csv")




