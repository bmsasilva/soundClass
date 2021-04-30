#https://becominghuman.ai/understanding-and-coding-inception-module-in-keras-eb56e9056b4b
#https://keras.rstudio.com/articles/functional_api.html
# https://www.reddit.com/r/MachineLearning/comments/67gonq/d_batch_normalization_before_or_after_relu/

library(keras)


# input layers
#input_features <- layer_input(shape = c(1), name='in_f') # CREATE THE ARRAY TO USE: x=rep(1:10, 346); train_features <- matrix(x, nrow = c(346), byrow=T)
input_conv <- layer_input(shape = input_shape, name='in_cnn')

# intermediate output
tower1 <- input_conv %>%
  layer_conv_2d(filter=32,padding = "same", kernel_size=c(1,1), kernel_initializer = "lecun_uniform", strides=c(1,1), use_bias=T) %>%
  layer_activation("relu")%>%
  layer_batch_normalization() %>%
  layer_flatten()

tower2 <- input_conv %>%
  layer_conv_2d(filter=32,padding = "same", kernel_size=c(1,1), kernel_initializer = "lecun_uniform", strides=c(1,1), use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_conv_2d(filter=32, padding = "same", kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1), use_bias=T) %>%
  layer_activation("relu")%>%
  layer_batch_normalization() %>%
layer_flatten()

tower3 <- input_conv %>%
  layer_conv_2d(filter=32,padding = "same", kernel_size=c(1,1), kernel_initializer = "lecun_uniform", strides=c(1,1), use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_conv_2d(filter=32, kernel_size=c(5,5),padding = "same", kernel_initializer = "lecun_uniform", strides=c(1,1), use_bias=T) %>%
  layer_activation("relu")%>%
  layer_batch_normalization() %>%
  layer_flatten()

tower4 <- input_conv %>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filter=32, padding = "same", kernel_size=c(1,1), kernel_initializer = "lecun_uniform", strides=c(1,1), use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_flatten()

# Main output
main_out <- layer_concatenate(c(tower1, tower2, tower3, tower4), axis=1) %>%
  
  layer_dense(units = 1024, use_bias=T) %>%
  layer_activation("relu")%>%
  layer_batch_normalization() %>%
  layer_dropout(0.25) %>%
  
  layer_dense(units = 512, use_bias=T) %>%
  layer_activation("relu")%>%
  layer_batch_normalization() %>%
  layer_dropout(0.25) %>%
  
  layer_dense(units = 128, use_bias=T) %>%
  layer_activation("relu")%>%
  layer_batch_normalization() %>%
  layer_dropout(0.25) %>%
  
  layer_dense(units = num_classes)%>%
  layer_activation("softmax")

# Model
model <- keras_model(inputs = c(input_conv),
                     outputs = c(main_out))

model %>%
  compile(
    optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T),
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )




