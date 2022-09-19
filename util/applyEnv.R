library(renv)

base::load("init/environment.Rda")
applyEnv <- function(fresh = TRUE){
  if(fresh){
    rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv) %notin% environment])
  }
  renv::restore()
  base::load("init/environment.Rdata", envir = .GlobalEnv)
}
