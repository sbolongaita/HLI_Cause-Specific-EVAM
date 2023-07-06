library(countrycode)

getCountry <- function(x, iso3 = TRUE, shortest = FALSE){
  if(!iso3){
    a <- countrycode::countryname(x, destination = "iso3c", warn = FALSE)
    a <- ifelse(!grepl("[A-Z]{3}", a), NA, a)
    b <- countrycode::countryname(x, destination = "wb", warn = FALSE)
    b <- ifelse(!grepl("[A-Z]{3}", b), NA, b)
    c <- ifelse(is.na(a), b, a)
  }else{
    c <- x
  }
  if(!shortest){
    d <- countrycode::countrycode(c, origin = "iso3c", destination = "country.name.en", warn = FALSE, nomatch = NA,
                                  custom_match = c("COG" = "Congo",
                                                   "COD" = "Democratic Republic of Congo",
                                                   "HKG" = "Hong Kong",
                                                   "LAO" = "Lao PDR",
                                                   "MAC" = "Macao"))
    e <- countrycode::countrycode(c, origin = "wb", destination = "country.name.en", warn = FALSE, nomatch = NA,
                                  custom_match = c("COG" = "Congo",
                                                   "COD" = "Democratic Republic of Congo",
                                                   "HKG" = "Hong Kong",
                                                   "LAO" = "Lao PDR",
                                                   "MAC" = "Macao"))
    f <- ifelse(is.na(d), e, d)
    z <- gsub("\\s*\\([^\\)]+\\)", "", gsub(" and ", " & ", f))
  }else{
    d <- countrycode::countrycode(c, origin = "iso3c", destination = "country.name.en", warn = FALSE, nomatch = NA,
                                  custom_match = c("ARE" = "UAE",
                                                   "CAF" = "CAR",
                                                   "COD" = "DRC",
                                                   "COG" = "Congo",
                                                   "HKG" = "Hong Kong",
                                                   "GBR" = "UK",
                                                   "MAC" = "Macao",
                                                   "LAO" = "Lao PDR",
                                                   "USA" = "US"))
    e <- countrycode::countrycode(c, origin = "wb", destination = "country.name.en", warn = FALSE, nomatch = NA,
                                  custom_match = c("ARE" = "UAE",
                                                   "CAF" = "CAR",
                                                   "COD" = "DRC",
                                                   "COG" = "Congo",
                                                   "HKG" = "Hong Kong",
                                                   "GBR" = "UK",
                                                   "MAC" = "Macao",
                                                   "LAO" = "Lao PDR",
                                                   "USA" = "US"))
    f <- ifelse(is.na(d), e, d)
    g <- gsub("\\s*\\([^\\)]+\\)", "", gsub(" and ", " & ", f))
    z <- gsub("North ", "N. ",
              gsub("South ", "S. ",
                   gsub("East ", "E. ",
                        gsub("West ", "W. ",
                             gsub(" Island", " Isl.",
                                  gsub(" Islands", " Isl.", g))))))
  }
  if(any(is.na(z))){
    warn <- x[is.na(z)]
    warning(paste("Some values were not matched:", paste(warn, collapse = ", ")))
  }
  return(z)
}
