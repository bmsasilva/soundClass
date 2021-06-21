load2env <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}