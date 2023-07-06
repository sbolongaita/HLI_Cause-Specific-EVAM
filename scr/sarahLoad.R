sarahLoad <- function(object, folder = "3 Output data"){

  all_files <- c(list.files(path = folder, pattern = ".Rda", full.names = TRUE, recursive = TRUE),
                 list.files(path = folder, pattern = ".rda", full.names = TRUE, recursive = TRUE))

  object_files <- list()
  for(i in seq_along(object)){

    search1 <- paste0("/",object[i],".Rda")
    search2 <- paste0("/", object[i], ".rda")

    search1.valid <- any(grepl(search1, all_files))
    search2.valid <- any(grepl(search2, all_files))

    search <- ifelse(search1.valid, search1, search2)

    if(all(grepl(search, all_files) == FALSE)){
      warning(paste0("No files matching '", search, "' in the '", folder, "' folder"))
      next
    }else{
      object_files[[object[i]]] <- all_files[grepl(search, all_files)]
    }
  }

  for(j in names(object_files)){
    base::load(object_files[[j]], envir = globalenv()); cat(paste0(j, ": '", object_files[[j]], "' loaded\n"))
  }

}
