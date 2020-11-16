# http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
# Precision is defined as the fraction of correct predictions for a certain class (sensibility)
# Recall is the fraction of instances of a class that were correctly predicted (specificity)
# F-1 score is the harmonic mean (or a weighted average) of precision and recall

# GEN1 model results torunos (base model)---------------------------------------------------------------

# Preprocess data from model from run_id()
model_data <- read.csv("~/Projectos/PHD/ms01/results_base_model/analise_torunos_base_model.csv",
                       sep = ",", header = T, as.is = c(1,2,3))
copy_model_data <- model_data

# eliminar NA's por causa de erros de correr o modelo
model_data <- model_data[!is.na(model_data$est),]

# criar grupos fonicos
model_data$obs[which(model_data$obs %in% c("Ppyg", "Msch") )] <- "PpygMsch"
model_data$obs[which(model_data$obs %in% c("EE", "Nlei", "Nlas") )] <- "NNEE"

model_data$est[which(model_data$est %in% c("Ppyg", "Msch") )] <- "PpygMsch"
model_data$est[which(model_data$est %in% c("Eser", "Eisa", "Nlei", "Nlas") )] <- "NNEE"

# confusion matrix
cm <- as.matrix(table(obs = model_data$obs, est = model_data$est)) # create the confusion matrix

# List of wrong classifications
misclass <- which(model_data$obs != model_data$est)
misclass_files_base_model <- data.frame(wav = paste0(model_data$file[misclass], ".wav"),
                                  png = paste0(model_data$file[misclass], ".wav.png"),
                                  RDATA = paste0(model_data$file[misclass], ".wav.RDATA"))

# Write outputs
write.csv(misclass_files_base_model, file = "~/Projectos/PHD/ms01/results_base_model/misclass_files_gen1.csv", row.names = F)



#### Para estas metricas funcionarem e preciso arranjar a confusion matrix. 
#### As colunas tem de ficar na mesma ordem e serem as mesmas nos obs e est
# Auxiliar variables for computing model performance metrics
n <- sum(cm) # number of instances
nc <- nrow(cm) # number of classes
diag <- diag(cm) # number of correctly classified instances per class
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class
p <- rowsums / n # distribution of instances over the actual classes
q <- colsums / n # distribution of instances over the predicted classes

# Computing performance metrics
# Precision is defined as the fraction of correct predictions for a certain class
# Recall is the fraction of instances of a class that were correctly predicted
# F-1 score is the harmonic mean (or a weighted average) of precision and recall
accuracy <- sum(diag) / n
precision <- diag / colsums
recall <- diag / rowsums
f1 <- 2 * precision * recall / (precision + recall)
performance_vgg1_gen1 <- data.frame(precision, recall, f1)

# List of wrong classifications
misclass <- which(model_data$obs != model_data$est)
misclass_files_vgg1_gen1 <- paste0(model_data$file[misclass], ".wav")

# Write outputs
write.csv(misclass_files_vgg1_gen1, file = "./validation_models/vgg1/generation1/misclass_files_vgg1_gen1.csv", row.names = F)
write.csv(performance_vgg1_gen1, file = "./validation_models/vgg1/generation1/performance_vgg1_gen1.csv", row.names = F)

# VGG2 GEN1 ---------------------------------------------------------------
# Preprocess data from model from run_id()
model_data <- read.csv("./validation_models/vgg2/generation1/analise_validation_recs_vgg2_gen1.csv",
                       sep = " ", header = F)
colnames(model_data) <- c("file", "est", "proba")
model_data$obs <- substr(model_data$file, 1,4)
model_data$obs[which(model_data$obs == "nois")] <- "Noise"
model_data$obs[which(model_data$obs == "Mnat")] <- "Mesc"
model_data$obs[which(model_data$obs == "Plec")] <- "Paur"

# confusion matrix
cm <- as.matrix(table(obs = model_data$obs, est = model_data$est)) # create the confusion matrix

# Auxiliar variables for computing model performance metrics
n <- sum(cm) # number of instances
nc <- nrow(cm) # number of classes
diag <- diag(cm) # number of correctly classified instances per class
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class
p <- rowsums / n # distribution of instances over the actual classes
q <- colsums / n # distribution of instances over the predicted classes

# Computing performance metrics
# Precision is defined as the fraction of correct predictions for a certain class
# Recall is the fraction of instances of a class that were correctly predicted
# F-1 score is the harmonic mean (or a weighted average) of precision and recall
accuracy <- sum(diag) / n
precision <- diag / colsums
recall <- diag / rowsums
f1 <- 2 * precision * recall / (precision + recall)
performance_vgg2_gen1 <- data.frame(precision, recall, f1)

# List of wrong classifications
misclass <- which(model_data$obs != model_data$est)
misclass_files_vgg2_gen1 <- paste0(model_data$file[misclass], ".wav")

# Write outputs
write.csv(misclass_files_vgg2_gen1, file = "./validation_models/vgg2/generation1/misclass_files_vgg2_gen1.csv", row.names = F)
write.csv(performance_vgg2_gen1, file = "./validation_models/vgg2/generation1/performance_vgg2_gen1.csv", row.names = F)

# http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html


# VGG1 GEN2 V1 ---------------------------------------------------------------

# Preprocess data from model from run_id()
model_data <- read.csv("./validation_models/vgg1/generation2/analise_validation_recs_vgg1_gen2_v1.csv", 
                       sep = " ", header = F)
colnames(model_data) <- c("file", "est", "proba")
model_data$obs <- substr(model_data$file, 1,4)
model_data$obs[which(model_data$obs == "nois")] <- "Noise"
model_data$obs[which(model_data$obs == "Mnat")] <- "Mesc"
model_data$obs[which(model_data$obs == "Plec")] <- "Paur"

# confusion matrix
cm <- as.matrix(table(obs = model_data$obs, est = model_data$est)) # create the confusion matrix

# Auxiliar variables for computing model performance metrics
n <- sum(cm) # number of instances
nc <- nrow(cm) # number of classes
diag <- diag(cm) # number of correctly classified instances per class 
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class
p <- rowsums / n # distribution of instances over the actual classes
q <- colsums / n # distribution of instances over the predicted classes

# Computing performance metrics
# Precision is defined as the fraction of correct predictions for a certain class
# Recall is the fraction of instances of a class that were correctly predicted
# F-1 score is the harmonic mean (or a weighted average) of precision and recall
accuracy <- sum(diag) / n 
precision <- diag / colsums 
recall <- diag / rowsums 
f1 <- 2 * precision * recall / (precision + recall) 
performance_vgg1_gen2 <- data.frame(precision, recall, f1) 

# List of wrong classifications
misclass <- which(model_data$obs != model_data$est)
misclass_files_vgg1_gen2 <- paste0(model_data$file[misclass], ".wav")

# Write outputs
write.csv(misclass_files_vgg1_gen2, file = "./validation_models/vgg1/generation2/misclass_files_vgg1_gen2_v1.csv", row.names = F)
write.csv(performance_vgg1_gen2, file = "./validation_models/vgg1/generation2/performance_vgg1_gen2_v1.csv", row.names = F)



# VGG1 GEN2 v2 ---------------------------------------------------------------

# Preprocess data from model from run_id()
model_data <- read.csv("./validation_models/vgg1/generation2/analise_validation_recs_vgg1_gen2_v2.csv", 
                       sep = " ", header = F)
colnames(model_data) <- c("file", "est", "proba")
model_data$obs <- substr(model_data$file, 1,4)
model_data$obs[which(model_data$obs == "nois")] <- "Noise"
model_data$obs[which(model_data$obs == "Mnat")] <- "Mesc"
model_data$obs[which(model_data$obs == "Plec")] <- "Paur"

# confusion matrix
cm <- as.matrix(table(obs = model_data$obs, est = model_data$est)) # create the confusion matrix

# Auxiliar variables for computing model performance metrics
n <- sum(cm) # number of instances
nc <- nrow(cm) # number of classes
diag <- diag(cm) # number of correctly classified instances per class 
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class
p <- rowsums / n # distribution of instances over the actual classes
q <- colsums / n # distribution of instances over the predicted classes

# Computing performance metrics
# Precision is defined as the fraction of correct predictions for a certain class
# Recall is the fraction of instances of a class that were correctly predicted
# F-1 score is the harmonic mean (or a weighted average) of precision and recall
accuracy <- sum(diag) / n 
precision <- diag / colsums 
recall <- diag / rowsums 
f1 <- 2 * precision * recall / (precision + recall) 
performance_vgg1_gen2 <- data.frame(precision, recall, f1) 

# List of wrong classifications
misclass <- which(model_data$obs != model_data$est)
misclass_files_vgg1_gen2 <- paste0(model_data$file[misclass], ".wav")

# Write outputs
write.csv(misclass_files_vgg1_gen2, file = "./validation_models/vgg1/generation2/misclass_files_vgg1_gen2_v2.csv", row.names = F)
write.csv(performance_vgg1_gen2, file = "./validation_models/vgg1/generation2/performance_vgg1_gen2_v2.csv", row.names = F)
# VGG1 GEN2 v3 ---------------------------------------------------------------

# Preprocess data from model from run_id()
model_data <- read.csv("./validation_models/vgg1/generation2/analise_validation_recs_vgg1_gen2_v3.csv", 
                       sep = " ", header = F)
colnames(model_data) <- c("file", "est", "proba")
model_data$obs <- substr(model_data$file, 1,4)
model_data$obs[which(model_data$obs == "nois")] <- "Noise"
model_data$obs[which(model_data$obs == "Mnat")] <- "Mesc"
model_data$obs[which(model_data$obs == "Plec")] <- "Paur"

# confusion matrix
cm <- as.matrix(table(obs = model_data$obs, est = model_data$est)) # create the confusion matrix

# Auxiliar variables for computing model performance metrics
n <- sum(cm) # number of instances
nc <- nrow(cm) # number of classes
diag <- diag(cm) # number of correctly classified instances per class 
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class
p <- rowsums / n # distribution of instances over the actual classes
q <- colsums / n # distribution of instances over the predicted classes

# Computing performance metrics
# Precision is defined as the fraction of correct predictions for a certain class
# Recall is the fraction of instances of a class that were correctly predicted
# F-1 score is the harmonic mean (or a weighted average) of precision and recall
accuracy <- sum(diag) / n 
precision <- diag / colsums 
recall <- diag / rowsums 
f1 <- 2 * precision * recall / (precision + recall) 
performance_vgg1_gen2 <- data.frame(precision, recall, f1) 

# List of wrong classifications
misclass <- which(model_data$obs != model_data$est)
misclass_files_vgg1_gen2 <- paste0(model_data$file[misclass], ".wav")

# Write outputs
write.csv(misclass_files_vgg1_gen2, file = "./validation_models/vgg1/generation2/misclass_files_vgg1_gen2_v3.csv", row.names = F)
write.csv(performance_vgg1_gen2, file = "./validation_models/vgg1/generation2/performance_vgg1_gen2_v3.csv", row.names = F)
# VGG1 GEN2 v4 ---------------------------------------------------------------

# Preprocess data from model from run_id()
model_data <- read.csv("./validation_models/vgg1/generation2/analise_validation_recs_vgg1_gen2_v4.csv", 
                       sep = " ", header = F)
colnames(model_data) <- c("file", "est", "proba")
model_data$obs <- substr(model_data$file, 1,4)
model_data$obs[which(model_data$obs == "nois")] <- "Noise"
model_data$obs[which(model_data$obs == "Mnat")] <- "Mesc"
model_data$obs[which(model_data$obs == "Plec")] <- "Paur"

# confusion matrix
cm <- as.matrix(table(obs = model_data$obs, est = model_data$est)) # create the confusion matrix

# Auxiliar variables for computing model performance metrics
n <- sum(cm) # number of instances
nc <- nrow(cm) # number of classes
diag <- diag(cm) # number of correctly classified instances per class 
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class
p <- rowsums / n # distribution of instances over the actual classes
q <- colsums / n # distribution of instances over the predicted classes

# Computing performance metrics
# Precision is defined as the fraction of correct predictions for a certain class
# Recall is the fraction of instances of a class that were correctly predicted
# F-1 score is the harmonic mean (or a weighted average) of precision and recall
accuracy <- sum(diag) / n 
precision <- diag / colsums 
recall <- diag / rowsums 
f1 <- 2 * precision * recall / (precision + recall) 
performance_vgg1_gen2 <- data.frame(precision, recall, f1) 

# List of wrong classifications
misclass <- which(model_data$obs != model_data$est)
misclass_files_vgg1_gen2 <- paste0(model_data$file[misclass], ".wav")

# Write outputs
write.csv(misclass_files_vgg1_gen2, file = "./validation_models/vgg1/generation2/misclass_files_vgg1_gen2_v4.csv", row.names = F)
write.csv(performance_vgg1_gen2, file = "./validation_models/vgg1/generation2/performance_vgg1_gen2_v4.csv", row.names = F)

