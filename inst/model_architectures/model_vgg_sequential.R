model <- keras::keras_model_sequential()

model %>%
  keras::layer_conv_2d(filter=16, kernel_size=c(3,3), padding="valid", input_shape=input_shape, strides=c(1,1), kernel_initializer = "lecun_uniform", use_bias=T) %>%
  keras::layer_activation("relu") %>%
  keras::layer_batch_normalization() %>%
  keras::layer_max_pooling_2d(pool_size=c(2,2)) %>%
  keras::layer_dropout(0.25) %>%

  keras::layer_conv_2d(filter=32, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  keras::layer_activation("relu") %>%
  keras::layer_batch_normalization() %>%
  keras::layer_max_pooling_2d(pool_size=c(2,2)) %>%
  keras::layer_dropout(0.25) %>%

  keras::layer_conv_2d(filter=64, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  keras::layer_activation("relu") %>%
  keras::layer_batch_normalization() %>%
  keras::layer_conv_2d(filter=64, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1),use_bias=T) %>%
  keras::layer_activation("relu") %>%
  keras::layer_batch_normalization() %>%
  keras::layer_max_pooling_2d(pool_size=c(2,2)) %>%
  keras::layer_dropout(0.25) %>%
  keras::layer_flatten() %>%

  keras::layer_dense(128, use_bias=T) %>%
  keras::layer_activation("relu") %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(0.25) %>%
  keras::layer_dense(num_classes) %>%
  keras::layer_activation("softmax")
