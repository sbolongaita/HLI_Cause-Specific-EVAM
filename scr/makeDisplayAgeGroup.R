library(dplyr)

makeDisplayAgeGroup <- function(x, factor = TRUE){
  y <- dplyr::case_when(x < 1 ~ "Infants",
                        x < 10 ~ "Children",
                        x < 20 ~ "Adolescents",
                        x < 49 ~ "Young adults",
                        x < 60 ~ "Adults",
                        x < 80 ~ "Elderly",
                        TRUE ~ "Old")
  if(factor){
    y <- factor(y, levels = c("Infants", "Children", "Adolescents", "Young adults", "Adults", "Elderly", "Old"))
  }
  return(y)
}
