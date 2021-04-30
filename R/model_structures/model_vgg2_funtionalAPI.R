#https://becominghuman.ai/understanding-and-coding-inception-module-in-keras-eb56e9056b4b
#https://keras.rstudio.com/articles/functional_api.html
# https://www.reddit.com/r/MachineLearning/comments/67gonq/d_batch_normalization_before_or_after_relu/

library(keras)


# input layers
#input_features <- layer_input(shape = c(1), name='in_f') # CREATE THE ARRAY TO USE: x=rep(1:10, 346); train_features <- matrix(x, nrow = c(346), byrow=T)
input_conv <- layer_input(shape = input_shape, name='in_cnn')

# intermediate output
output_conv <- input_conv %>%
  layer_conv_2d(filter=32, kernel_size=c(3,3), padding="valid", input_shape=input_shape, strides=c(1,1), kernel_initializer = "lecun_uniform", use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%

  layer_conv_2d(filter=64, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%

  layer_conv_2d(filter=128, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_conv_2d(filter=128, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size=c(2,2))



# Main output
main_out <- layer_flatten(output_conv) %>%
  layer_dense(units = 256, use_bias=T) %>%
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




