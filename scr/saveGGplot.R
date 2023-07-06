saveGGplot <- function(x, name, folder = NULL, width = 6, height = 4, multipage = FALSE){

  if(is.null(folder)){
    folder <- getwd()
  }
  filename <- ifelse(!grepl("\\.pdf$", name), paste0(name, ".pdf"), name)
  path <- paste(folder, filename, sep = "/")

  if(multipage){
    multi <- ggpubr::ggarrange(plotlist = x, nrow = 1, ncol = 1)
    suppressMessages(ggpubr::ggexport(multi, filename = path, width = width, height = height))
  }else{
    suppressMessages(ggpubr::ggexport(x, filename = path, width = width, height = height))
  }

  extrafont::embed_fonts(file = path)
  cat(paste0("'", path, "' saved\n"))

}
