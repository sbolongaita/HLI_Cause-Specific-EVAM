wrapper <- function(x, width) {
  out <- character()
  for(i in seq_along(x)){
    out[i] <- paste(strwrap(x[i], width = width), collapse = "\n")
  }
  return(out)
}
