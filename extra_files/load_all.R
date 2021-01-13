##### Load all #####
files <- list.files("/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms02_package_cnn/soundClass/R/", pattern = ".R", full.names = TRUE)
for(file in files) source(file)

files <- list.files("~/Projectos/R_packages/development/Rutils/R/", pattern = ".R", full.names = TRUE)
for(file in files) source(file)

files <- list.files("~/Projectos/bat_detector/batdecoder_gui/R/", pattern = ".R", full.names = TRUE)
for(file in files) source(file)
