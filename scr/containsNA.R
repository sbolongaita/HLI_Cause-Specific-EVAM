library(dplyr)

containsNA <- function(df){
  x <- filter_all(df, any_vars(is.na(.) | is.nan(.) | is.infinite(.)))
  if(nrow(x) == 0){
    cat("No `NA` values")
  }else{
    return(x)
  }
}
