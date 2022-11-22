
### Healthy Longevity Initiative



# 1 CLEARING ENVIRONMENT --------------------------------------------------

rm(list = ls())



# 2 PACKAGES --------------------------------------------------------------
# Loading project packages

# Installing and loading SarahB package
if(!require("devtools")) install.packages(devtools); library(devtools)
if(!require("SarahB")) devtools::install_github("sbolongaita/SarahB"); library(SarahB)

# Defining other packages for installation and loading
packages <- c(
  # A, B, C, D
  "colorspace", "countrycode", "cowplot", "DescTools", "devtools",
  # E, F, G, H
  "extrafont", "foreign", "ggplot2", "ggrepel", "ggsci", "ggthemes", "ggpubr", "grid",
  "gridExtra", "gtools", "haven", "Hmisc",
  # I, J, K, L
  # M, N, O, P
  "magrittr", "openxlsx", "plyr", "purrr",
  # Q, R, S, T
  "reader", "remotes", "renv", "rlang", "scales", "shades", "stringr", "sysfonts", "tidyr",
  # Last load packages
  "dplyr", "base"
)

# Loading packages
SarahB::loadPackages(packages)



# 3 UTILITY SCRIPTS -------------------------------------------------------
# Running utility scripts

scripts <- list.files("scr", full.names = TRUE)
scripts <- scripts[!grepl("raw", scripts)]
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
