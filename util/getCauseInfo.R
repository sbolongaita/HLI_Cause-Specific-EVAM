library(dplyr)
library(SarahB)

getCauseInfo <- function(.data, lookup = "ghecause", return = "causename"){

  base::load("data/processed/cause_hierarchy.Rda")

  if(lookup %notin% c("prefix", "ghecause", "causename")){
    stop("Invalid input vector")
  }

  if(return %notin% names(cause_hierarchy)){
    stop("Invalid return vector")
  }

  Lookup <- cause_hierarchy %>% dplyr::pull(lookup) %>% as.character()
  Return <- cause_hierarchy %>% dplyr::pull(return) %>% as.character()
  Return <- setNames(Return, Lookup)

  z <- Return[as.character(.data[[lookup]])]
  return(z)

}
