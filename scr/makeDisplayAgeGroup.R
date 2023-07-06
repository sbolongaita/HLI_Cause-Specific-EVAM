library(dplyr)

makeDisplayAgeGroup <- function(x, factor = TRUE){
  y <- dplyr::case_when(x < 1 ~ "<1 year",
                        x < 10 ~ "1-9 years",
                        x < 20 ~ "10-19 years",
                        x < 40 ~ "20-39 years",
                        x < 60 ~ "40-59 years",
                        x < 80 ~ "60-79 years",
                        TRUE ~ "80+ years")
  if(factor){
    y <- factor(y, levels = c("<1 year", "1-9 years", "10-19 years",
                              "20-39 years", "40-59 years", "60-79 years",
                              "80+ years"))
  }
  return(y)
}
