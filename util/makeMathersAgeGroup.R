library(dplyr)

makeMathersAgeGroup <- function(x, factor = TRUE){
  y <- dplyr::case_when(x < 5 ~ "0-4 years",
                        x < 15 ~ "5-14 years",
                        x < 30 ~ "15-29 years",
                        x < 45 ~ "30-44 years",
                        x < 60 ~ "45-59 years",
                        x < 70 ~ "60-69 years",
                        x < 85 ~ "70-84 years",
                        TRUE ~ "85+ years")
  if(factor){
    xx <- c(0, 5, 15, 30, 45, 60, 70, 85)
    yy <- dplyr::case_when(xx < 5 ~ "0-4 years",
                           xx < 15 ~ "5-14 years",
                           xx < 30 ~ "15-29 years",
                           xx < 45 ~ "30-44 years",
                           xx < 60 ~ "45-59 years",
                           xx < 70 ~ "60-69 years",
                           xx < 85 ~ "70-84 years",
                           TRUE ~ "85+ years")
    y <- factor(y, levels = yy)
  }
  return(y)
}
