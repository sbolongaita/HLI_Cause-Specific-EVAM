
### Healthy Longevity Initiative
### 120 - ENVIRONMENT

# This script sets up the R environment for all subsequent scripts. It is
# called at the beginning of each script, and therefore does not need to be
# run independently.

# Requires: N/A

# Returns: The project environment



# 1 PACKAGES --------------------------------------------------------------

# Loading my standard package of helper functions
if(!require("devtools")) install.packages("devtools")
devtools::install_github("sbolongaita/SarahB")
library(SarahB)

# Loading other necessary packages
packages <- c(
  # A, B, C, D
  "colorspace", "countrycode", "DescTools",
  # E, F, G, H
  "extrafont", "foreign", "ggplot2", "ggrepel", "ggsci", "ggthemes", "ggpubr", "grid",
  "gridExtra", "gtools", "haven", "Hmisc",
  # I, J, K, L
  # M, N, O, P
  "openxlsx", "plyr", "purrr",
  # Q, R, S, T
  "reader", "scales", "shades", "stringr", "sysfonts", "tidyr",
  # Last load packages
  "dplyr"
)

# Loading packages
SarahB::loadPackages(packages)



# 2 UTILITY SCRIPTS -------------------------------------------------------

scrs <- list.files("scr", full.names = TRUE)
for(scr in scrs){
  source(scr)
}



# 3 END -------------------------------------------------------------------

# Setting initial environment variable
env <- c("env", ls())

# Notifying end of script
notify("Environment loaded")
