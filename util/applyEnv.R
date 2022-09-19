library(renv)

applyEnv <- function(){
  renv::restore()
  base::load("init/environment.Rdata", envir = .GlobalEnv)
}
