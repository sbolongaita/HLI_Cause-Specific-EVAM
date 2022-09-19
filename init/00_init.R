
### Healthy Longevity Initiative
### 00 INITIALIZATION



# 1 PACKAGES --------------------------------------------------------------
# Loading project packages

# Installing and loading SarahB package
if(!require("devtools")) install.packages(devtools); library(devtools)
if(!require("SarahB")) devtools::install_github("sbolongaita/SarahB"); library(SarahB)

# Defining other packages for installation and loading
packages <- c(
  # A, B, C, D
  "colorspace", "countrycode", "DescTools", "devtools",
  # E, F, G, H
  "extrafont", "foreign", "ggplot2", "ggrepel", "ggsci", "ggthemes", "ggpubr", "grid",
  "gridExtra", "gtools", "haven", "Hmisc",
  # I, J, K, L
  # M, N, O, P
  "magrittr", "openxlsx", "plyr", "purrr",
  # Q, R, S, T
  "reader", "remotes", "renv", "scales", "shades", "stringr", "sysfonts", "tidyr",
  # Last load packages
  "dplyr"
)

# Loading packages
SarahB::loadPackages(packages)



# 2 UTILITY SCRIPTS -------------------------------------------------------
# Running utility scripts

scripts <- c(list.files("util", full.names = TRUE))
for(script in scripts){
  source(script)
}



# 3 ENVIRONMENT -----------------------------------------------------------

# Setting the `renv` environment
# renv::init()
renv::snapshot()

# Saving the default global environment
environment <- ls()
save(environment, file = "init/environment.Rda")
save.image(file = "init/environment.RData")
