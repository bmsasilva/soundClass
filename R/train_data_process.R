train_data_process <- function(rdata_list, seed = 1002){

  ## avaliar a classe do rdata_list (primeior tenho de criar uma classe para o rdata_list)
  
# Parametros para o treino
total <- dim(rdata_list[[1]])[1]
img_rows <- rdata_list[[3]]$img_rows
img_cols <- rdata_list[[3]]$img_cols
input_shape <- c(img_rows, img_cols, 1) # incluir este parametro no rdata_list
num_classes <- length(unique(rdata_list[[2]])) # incluir este parametro no rdata_list


rdata_list[[2]] <- factor(rdata_list[[2]])#converter para factor para facilitar os numeros e aos nomes de classe repectivos
labels_code <- as.integer(rdata_list[[2]]) - 1
labels_name <- as.character(rdata_list[[2]])
labels_df <- data.frame(name = levels(rdata_list[[2]]), code = (1:length(levels(rdata_list[[2]])))-1)

#  Randomizar a ordem dos casos
set.seed(seed)
data_x <- rdata_list[[1]]
data_y <- labels_code
randomize <- sample(length(data_y))
data_x <- data_x[randomize,]
data_y <- data_y[randomize]

# preparar data para tensorflow
data_y <- keras::to_categorical(as.numeric(data_y), num_classes = num_classes)
data_x <- array(data_x, dim = c(total, img_rows, img_cols, 1))

return(list(data_x = data_x, data_y = data_y))
}