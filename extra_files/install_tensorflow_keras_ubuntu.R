# first install tensorflow and dependencies
install.packages("Rcpp")
install.packages("reticulate")
install.packages("tensorflow")

library(tensorflow)
install_tensorflow()

# test tensorflow intall
library(tensorflow)
tf$constant("Hellow Tensorflow")
## tf.Tensor(b'Hellow Tensorflow', shape=(), dtype=string) #if correct install this is the output

# Next instal keras
install.packages("keras")
library(keras)
install_keras()


# test everything
library(keras)
library(tensorflow)

model <- keras_model_sequential() %>%
  layer_dense(1)

model %>%
  compile(loss = "mse" , optimizer = "adam", metrics = "mse")

model %>% 
  fit(
    x = matrix(runif(100), ncol = 1), 
    y = matrix(runif(100), ncol = 1),
    verbose = 1
  )
