library(renv)

applyEnv <- function(){
  renv::restore()
  base::load("config/environment.Rdata", envir = .GlobalEnv)
}
