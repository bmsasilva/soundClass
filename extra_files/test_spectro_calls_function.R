files_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms02_package_cnn/soundClass/inst/recordings/"
db_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms02_package_cnn/soundClass/inst/recordings/bat_detection.sqlite3"


files_path <- "c://Users//silva//Projects//R_packages//soundClass//inst//recordings//"
db_path <- "c://Users//silva//Projects//R_packages//soundClass//inst//recordings//bat_detection.sqlite3"


sp <- spectro_calls(files_path, db_path, version ="v2") 


i1 <- matrix(sp[[1]][3,], ncol = 80) 

image(i1)

image(t(apply(i1, 2, rev))) # maneira plotar o pulso na forma certa depois de aplicar o r() na peaks2spec para guardar na base de dados


