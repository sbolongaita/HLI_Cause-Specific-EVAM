
# 1 CLEARING ENVIRONMENT --------------------------------------------------

rm(list = ls())



# 2 PACKAGES --------------------------------------------------------------
# Loading project packages

# Installing and loading SarahB package
if(!require("devtools")) install.packages("devtools"); library(devtools)
# if(!require("SarahB")) devtools::install_github("sbolongaita/SarahB"); library(SarahB)

# Defining other packages for installation and loading
packages <- c(
  # A, B, C, D
  "colorspace", "countrycode", "cowplot", "DescTools", "devtools",
  # E, F, G, H
  "extrafont", "foreign", "ggnewscale", "ggplot2", "ggrepel", "ggsci", "ggpubr", "ggthemes", "grid",
  "gridExtra", "gtools", "haven", "Hmisc",
  # I, J, K, L
  # M, N, O, P
  "magrittr", "openxlsx", "plyr", "purrr",
  # Q, R, S, T
  "reader", "remotes", "renv", "rlang", "scales", "shades", "stringr", "sysfonts", "tidyr",
  # Last load packages
  "dplyr", "base"
)

loadPackages <- function(packages){

  if(any(c("dplyr", "plyr") %in% packages)){
    a <- unique(c("plyr", packages[!(packages %in% c("dplyr", "plyr"))], "dplyr"))
  }else{
    a <- packages
  }

  b <- a[!(a %in% utils::installed.packages()[, "Package"])]
  if(length(b))
    utils::install.packages(b, dependencies = TRUE,
                            repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
  sapply(packages, require, character.only = TRUE)
}

# Loading packages
loadPackages(packages)



# 3 UTILITY SCRIPTS -------------------------------------------------------
# Running utility scripts

scripts <- list.files("scr", full.names = TRUE, pattern = ".R")
for(script in scripts){
  source(script)
}



# 3 ENVIRONMENT -----------------------------------------------------------

# Initializing/setting/restoring the `renv` environment
# renv::init()
renv::snapshot()

# Saving the default global environment
environment <- c(ls(), "environment")
save(environment, file = "config/environment.Rda")
save.image(file = "config/environment.RData")
