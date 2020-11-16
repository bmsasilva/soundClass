##### Load all #####
files <- list.files("~/Projectos/R_packages/development/recLabel/R/", pattern = ".R", full.names = TRUE)
for(file in files) source(file)

files <- list.files("~/Projectos/R_packages/development/Rutils/R/", pattern = ".R", full.names = TRUE)
for(file in files) source(file)

files <- list.files("~/Projectos/bat_detector/batdecoder_gui/R/", pattern = ".R", full.names = TRUE)
for(file in files) source(file)
