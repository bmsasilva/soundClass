library(keras)
seed <- 1002

# data prep 1 sample per call ---------------------------------------------------------------
setwd('~/Projectos/bat_detector/batdecoder_model_train/')
load('~/Projectos/bat_detector/batdecoder_model_train/database/db_3samples_per_call/train_ready_noRhinos_3samplesPerCall.RData')


tab <- data.frame("count" = apply(train_y,2,sum))
row.names(tab) <- class_labels
tab$perc <- tab$count/sum(tab$count)*100
tab

train_y <- cbind(train_y,rep(1:length(train_y[,13])))
rand_msch <- sample(train_y[train_y[,13]==1, 22],size = 1900, replace = F)
rand_ppyg <- sample(train_y[train_y[,20]==1, 22],size = 900, replace = F)

filter <- c(rand_msch, rand_ppyg)

train_y <- train_y[-filter, -22]
train_x <- train_x[-filter,,,]

save(test_y, test_x, train_y, train_x, class_labels, input_shape, num_classes,
     file = "train_ready_noRhinos_MschPpygAdjusted_3samplesPerCall.RDATA")

recs2 <- recs[-filter] 
write.csv(recs2, file="recordings_for_training.csv")
