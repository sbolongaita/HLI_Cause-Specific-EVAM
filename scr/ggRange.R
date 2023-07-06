library(plyr)

ggRange <- function(x, accuracy = 1, log = FALSE){
  if(!log){
    a <- min(x, na.rm = TRUE)
    b <- max(x, na.rm = TRUE)
    z <- c(plyr::round_any(a, accuracy, floor),
           plyr::round_any(b, accuracy, ceiling))
  }else{
    a <- min(x[x > 0], na.rm = TRUE)
    b <- max(x[x > 0], na.rm = TRUE)
    z <- c(10^floor(log10(a)), 10^ceiling(log10(b)))
  }
  return(z)
}
