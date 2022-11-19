library(renv)

applyEnv <- function(){
  renv::restore()
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  base::load("config/environment.Rdata", envir = .GlobalEnv)
}

