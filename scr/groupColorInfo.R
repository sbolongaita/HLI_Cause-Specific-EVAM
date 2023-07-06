groupColorInfo <- function(df, group, subgroup){
  out <- df %>%
    dplyr::select(!!as.name(group), !!as.name(subgroup)) %>% unique() %>%
    dplyr::arrange(!!as.name(group), !!as.name(subgroup)) %>%
    dplyr::group_by(dplyr::across(c(!!as.name(group)))) %>%
    dplyr::summarize(n = dplyr::n(),
                     names = paste(!!as.name(subgroup), collapse = "; "))
  n <- out$n
  names <- unlist(stringr::str_split(paste(out$names, collapse = "; "), "; "))
  return <- list(n = n, names = names)
  return(return)
}

