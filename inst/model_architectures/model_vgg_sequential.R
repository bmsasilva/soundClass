#https://www.reddit.com/r/MachineLearning/comments/67gonq/d_batch_normalization_before_or_after_relu/
library(keras)

model<-keras_model_sequential()

model %>%
  layer_conv_2d(filter=16, kernel_size=c(3,3), padding="valid", input_shape=input_shape, strides=c(1,1), kernel_initializer = "lecun_uniform", use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_dropout(0.25) %>%

  layer_conv_2d(filter=32, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_dropout(0.25) %>%

  layer_conv_2d(filter=64, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_conv_2d(filter=64, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_dropout(0.25) %>%
  layer_flatten() %>%

  layer_dense(128, use_bias=T) %>%
  layer_activation("relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(0.25) %>%
  layer_dense(num_classes) %>%
  layer_activation("softmax")

# model %>%
#   compile(
#     optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T),
#     loss = 'categorical_crossentropy',
#     metrics = c('accuracy')
#   )
