# soundClass
Provides an all-in-one solution for automatic classification of 
sound events using convolutional neural networks (CNN). From 
annotating sound events in recordings to automating model usage in real-life
situations. This package is aimed at (but not limited to) biologists and 
ecologists working with sound events that could benefit greatly from 
machine learning algorithms applied to their research. Using the package 
requires a pre-compiled collection of recordings with sound events of 
interest and it can be employed for: 1) Annotation: create a database of 
annotated recordings, 2) Training: prepare train data from annotated 
recordings and fit CNN models and 3) Classification: automate the use of 
the fitted model for classifying new recordings. By using automatic 
feature selection and a user-friendly GUI for managing data and 
training/deploying models, this package is intended to be used by a broad 
audience as it does not require specific expertise in statistics, 
programming or sound analysis.
    
## Install package:
  install.packages("devtools")
  
  library(devtools)
  
  devtools::install_github("bmsasilva/soundClass")
  
  > Note: If you're getting a "Installation failed: Bad credentials (401)" error, 
  > you probably have a GITHUB_PAT set in R environment and you should unset it before installing soundClass. More information at:
  > https://github.com/r-lib/devtools/issues/1566

  Sys.unsetenv("GITHUB_PAT")
    
