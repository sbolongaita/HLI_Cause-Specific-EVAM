slug <- function(x, lower = TRUE, numbers = TRUE, articles = FALSE){
  if(lower){
    a <- tolower(x)
  }else{
    a <- x
  }
  if(numbers){
    b <- gsub("[^A-Za-z0-9\\d\\s]", " ", a)
  }else{
    b <- gsub("[^A-Za-z\\d\\s]", " ", a)
  }
  c <- b
  if(!articles){
    terms <- c("a", "an", "and", "at", "but", "by", "for", "on", "the", "to")
    for(article in terms){
      c <- gsub(paste0(" ", article, " "), "  ", c)
    }
  }
  d <- trimws(stringr::str_squish(c))
  z <- gsub(" ", "-", d)
  return(z)
}
