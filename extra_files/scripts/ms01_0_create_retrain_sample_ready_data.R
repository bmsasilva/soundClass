
library(keras)
library(abind)
seed <- 1002

load(paste0("~/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/database/train_ready_noRhinos_MschPpygAdjusted_3samplesPerCall.RDATA"))

# Escolher 5% da base dados original
size <- floor(dim(train_y)[1]*0.05)
re_train_x <- train_x[1:size,,]
re_train_y <- train_y[1:size, -22]

colSums(re_train_y) # falta Mbec
which(train_y[, 6] == 1)
# 4260  4422  4648  8233  8488  8942 20899 20960 24996 30652 31125 37270 42881 44786 51854 55900
re_train_x <- abind(re_train_x, train_x[c(4260,30652),,], along=1)
re_train_y <- rbind(re_train_y, train_y[c(4260,30652),-22])

save(re_train_x, re_train_y, class_labels, input_shape, num_classes, test_x, test_y,
     file="re_train_ready_noRhinos_MschPpygAdjusted_sampleFromOriginalBD.RDATA")
