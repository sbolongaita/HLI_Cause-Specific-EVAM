library(dplyr)

makeAgeGroup <- function(x, factor = TRUE){
  y <- dplyr::case_when(x == 0 ~ paste0(x, "-", x+1, " years"),
                        x == 1 ~ paste0(x, "-", x+3, " years"),
                        x == 85 ~ paste0(x, "+ years"),
                        TRUE ~ paste0(x, "-", x+4, " years"))
  if(factor){
    xx <- c(0, 1, seq(5, 85, 5))
    yy <- dplyr::case_when(xx == 0 ~ paste0(xx, "-", xx+1, " years"),
                           xx == 1 ~ paste0(xx, "-", xx+3, " years"),
                           xx == 85 ~ paste0(xx, "+ years"),
                           TRUE ~ paste0(xx, "-", xx+4, " years"))
    y <- factor(y, levels = yy)
  }
  return(y)
}
