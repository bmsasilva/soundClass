## Generation1 torunos - copy misclassified recordings to folder

path_wav <- '~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/'
path_outputs <- '~/Projectos/PHD/ms01/output_base_model/'
path2 <- '~/Projectos/PHD/ms01/results_base_model/misclassifications2/'
files <- read.csv('~/Projectos/PHD/ms01/results_base_model/misclass_files_gen1.csv', 
                  as.is=c(1,2,3), header=T)

# move wav
for(i in seq(files[,1])){
  file.copy(paste0(path_wav, files[i, 1]),
            paste0(path2, files[i, 1]))
}

# move png
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 2]),
            paste0(path2, files[i, 2]))
}

# move RDATA
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 3]),
            paste0(path2, files[i, 3]))
}

## Generation1 hand release - copy misclassifications from 1% recordings to folder
path_wav <- '~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/'
path_outputs <- '~/Projectos/PHD/ms01/output_base_model/'
path2 <- '~/Projectos/PHD/ms01/results_base_model/misclassifications2/'
files <- read.csv('~/Projectos/PHD/ms01/results_base_model/misclass_files_gen1.csv', 
                  as.is=c(1,2,3), header=T)

# move wav
for(i in seq(files[,1])){
  file.copy(paste0(path_wav, files[i, 1]),
            paste0(path2, files[i, 1]))
}

# move png
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 2]),
            paste0(path2, files[i, 2]))
}

# move RDATA
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 3]),
            paste0(path2, files[i, 3]))
}


## Generation1 hand release - copy misclassifications from 5% recordings to folder
path_wav <- '~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/'
path_outputs <- '~/Projectos/PHD/ms01/output_base_model/'
path2 <- '~/Projectos/PHD/ms01/results_base_model/misclassifications2/'
files <- read.csv('~/Projectos/PHD/ms01/results_base_model/misclass_files_gen1.csv', 
                  as.is=c(1,2,3), header=T)

# move wav
for(i in seq(files[,1])){
  file.copy(paste0(path_wav, files[i, 1]),
            paste0(path2, files[i, 1]))
}

# move png
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 2]),
            paste0(path2, files[i, 2]))
}

# move RDATA
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 3]),
            paste0(path2, files[i, 3]))
}

## Generation1 hand release - copy misclassifications from 10% recordings to folder
path_wav <- '~/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/'
path_outputs <- '~/Projectos/PHD/ms01/output_base_model/'
path2 <- '~/Projectos/PHD/ms01/results_base_model/misclassifications2/'
files <- read.csv('~/Projectos/PHD/ms01/results_base_model/misclass_files_gen1.csv', 
                  as.is=c(1,2,3), header=T)

# move wav
for(i in seq(files[,1])){
  file.copy(paste0(path_wav, files[i, 1]),
            paste0(path2, files[i, 1]))
}

# move png
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 2]),
            paste0(path2, files[i, 2]))
}

# move RDATA
for(i in seq(files[,1])){
  file.copy(paste0(path_outputs, files[i, 3]),
            paste0(path2, files[i, 3]))
}