library(magrittr)
require(tcltk)

wavs <-
  tk_choose.dir(caption = 'Select wav directory') %T>%
  setwd() %>%
  list.files(pattern = "wav$", ignore.case = TRUE)
wavs_wd <- getwd()

##### Move noise only files #####
specs <-
  tk_choose.dir(caption = 'Select spectrograms directory') %T>%
  setwd() %>%
  list.files(pattern = "png$", ignore.case = TRUE)
specs_wd <- getwd()
specs <- gsub(".png", "", specs)

for(file in specs){
file.rename(from = paste0(wavs_wd,"/", file),
            to = paste0("/home/bruno/Projectos/projectos_herreralab/torunos_2019/output/noise_recs/", file))
}

