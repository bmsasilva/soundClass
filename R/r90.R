#rotate matrix 90º clockwise
#https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r-by-90-degrees-clockwise
r90 <- function(x) t(apply(x, 2, rev))