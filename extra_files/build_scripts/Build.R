package.name <- "soundClass"
package.dir <- paste0("~/Projectos/phd/0.workfolder/meus_papers/ms02_package_cnn/", package.name)
setwd(package.dir)
### --- Use Roxygenise to generate .RD files from comments
library(roxygen2)
roxygenise(package.dir = package.dir)
system(command = paste("R CMD INSTALL '", package.dir, "'", sep=""))
