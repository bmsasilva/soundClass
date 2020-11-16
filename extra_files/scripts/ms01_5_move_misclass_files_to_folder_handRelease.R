## Generation1 hand release - copy misclassifications from 10% recordings to folder

path_wav <- '~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/'
path_outputs <- '~/Projectos/PHD/ms01/output_base_model_studyCase2/'
path2 <- '~/Projectos/PHD/ms01/results_case2_handRelease/retrain10percent/'
files <- read.csv('~/Projectos/PHD/ms01/results_case2_handRelease/analise_handRelease_10percent_errors.csv',
                  as.is=c(1,2), header=F)

# move wav
for(i in seq(files[, 1])){
  file.copy(paste0(path_wav, files[i, 1]),
            paste0(path2, files[i, 1]))
}

# move RDATA
for(i in seq(files[, 1])){
  file.copy(paste0(path_outputs, files[i, 2]),
            paste0(path2, files[i, 2]))
}

## Generation1 hand release - copy misclassifications from 5% recordings to folder

path_wav <- '~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/'
path_outputs <- '~/Projectos/PHD/ms01/output_base_model_studyCase2/'
path2 <- '~/Projectos/PHD/ms01/results_case2_handRelease/retrain5percent/'
files <- read.csv('~/Projectos/PHD/ms01/results_case2_handRelease/analise_handRelease_5percent_errors.csv',
                  as.is=c(1,2), header=F)

# move wav
for(i in seq(files[, 1])){
  file.copy(paste0(path_wav, files[i, 1]),
            paste0(path2, files[i, 1]))
}

# move RDATA
for(i in seq(files[, 1])){
  file.copy(paste0(path_outputs, files[i, 2]),
            paste0(path2, files[i, 2]))
}


## Generation1 hand release - copy misclassifications from 1% recordings to folder

path_wav <- '~/Projectos/PHD/ms01/ms01_recs_caseStudy2_handRelease/'
path_outputs <- '~/Projectos/PHD/ms01/output_base_model_studyCase2/'
path2 <- '~/Projectos/PHD/ms01/results_case2_handRelease/retrain1percent/'
files <- read.csv('~/Projectos/PHD/ms01/results_case2_handRelease/analise_handRelease_1percent_errors.csv',
                  as.is=c(1,2), header=F)

# move wav
for(i in seq(files[, 1])){
  file.copy(paste0(path_wav, files[i, 1]),
            paste0(path2, files[i, 1]))
}

# move RDATA
for(i in seq(files[, 1])){
  file.copy(paste0(path_outputs, files[i, 2]),
            paste0(path2, files[i, 2]))
}
