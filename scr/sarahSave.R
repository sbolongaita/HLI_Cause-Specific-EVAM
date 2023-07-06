sarahSave <- function(object, folder = "3 Output data", type = ".rda"){

  for(i in seq_along(object)){

    filename <- as.character(object[i])

    if(".rda" %in% type){
      filepath <- paste0(paste(folder, filename, sep = "/"), ".Rda")
      save(list = filename, file = filepath)
      cat(paste0("'", filepath, "' saved\n"))
    }

    if(".csv" %in% type){
      filepath <- paste0(paste(folder, filename, sep = "/"), ".csv")
      utils::write.csv(x = get(filename), file = filepath, row.names = FALSE, na = "")
      cat(paste0("'", filepath, "' saved\n"))
    }
  }

}
