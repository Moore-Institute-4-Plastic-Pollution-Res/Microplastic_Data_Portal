library(utils)
image_files <- c("code/validator/www/microplastic_images/MicroplasticImages/ALGALITA_CW_3_above500_30.jpeg", "code/validator/www/microplastic_images/MicroplasticImages/B_DW_3_above500_96.jpeg", "code/validator/www/microplastic_images/MicroplasticImages/CC_CW_1_20-250_176.jpeg")
MicroplasticImages <- "MicroplasticImages.zip"
zip(MicroplasticImages, files = image_files)

