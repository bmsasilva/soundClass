
files_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms02_package_cnn/soundClass/inst/recordings/"
db_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms02_package_cnn/soundClass/inst/recordings/demo_db.sqlite3"

# files_path <- "c://Users//silva//Projects//R_packages//soundClass//inst//recordings//"
# db_path <- "c://Users//silva//Projects//R_packages//soundClass//inst//recordings//bat_detection.sqlite3"

sp <- spectro_calls(files_path, db_path, parameters ="v2")


table(sp[[2]])


spectro_calls(files_path, db_path, parameters) # nota: file_path tem de acabar em /
  