labelAuto <- function(x) {

  # Not currently coded for numbers larger than 999M
  if(max(abs(x)) > 1e9-1){stop("labelAuto cannot handle numbers larger than 999M")}

  # Splitting x into magnitude categories
  dl <- list(
    # sm < 10
    sm = list(ids = which(x %in% x[abs(x) < 10]),
              nums = x[abs(x) < 10],
              scl = 1,
              suf = "",
              dig = 0L,
              acc = 0.1,
              rescale = FALSE),
    # med <1K
    med = list(ids = which(x %in% x[abs(x) >= 10 & abs(x) < 1e3]),
               nums = x[abs(x) >= 10 & abs(x) < 1e3],
               scl = 1,
               suf = "",
               dig = 0L,
               acc = 0.1,
               rescale = FALSE),
    # lg <1M
    lg = list(ids = which(x %in% x[abs(x) >= 1e3 & abs(x) < 1e6]),
              nums = x[abs(x) >= 1e3 & abs(x) < 1e6],
              scl = 1e3,
              suf = "K",
              dig = 0L,
              acc = 0.1,
              rescale = FALSE),
    # xl <1M
    xl = list(ids = which(x %in% x[abs(x) >= 1e6]),
              nums = x[abs(x) >= 1e6],
              scl = 1e6,
              suf = "M",
              dig = 0L,
              acc = 0.1,
              rescale = FALSE))

  # Deleting levels if no data
  for(mag in names(dl)){
    if(length(dl[[mag]]$ids) == 0){
      dl[[mag]] <- NULL
      next
    }
  }

  # Accuracy tries for rounding
  mult <- 10^seq(1, 3)
  tries <- sort(c(1, 2, 5, mult, 2*mult, 5*mult), decreasing = TRUE)

  for(mag in names(dl)){

    # Scaling
    dl[[mag]]$nums.scl <- dl[[mag]]$nums / dl[[mag]]$scl

    # Determining accuracy for rounding
    f.try <- round(dl[[mag]]$nums.scl, digits = 1)
    duplicated <- any(duplicated(f.try))
    if(mag == "sm"){
      if(!duplicated){
        make <- "same"
        dl[[mag]]$dig <- 1
      }else{
        make <- "smaller"
      }
    }else{
      if(!duplicated){
        make <- "bigger"
      }else{
        make <- "smaller"
      }
    }

    if(make == "smaller"){
      try <- 1
      repeat{
        values <- round(dl[[mag]]$nums.scl, digits = try)
        unique <- !any(duplicated(values))
        if(unique){
          dl[[mag]]$dig <- try
          dl[[mag]]$acc <- 10^(-try)
          break
        }else{
          try <- try + 1
        }
      }
    }

    if(make == "bigger"){
      try <- tries[tries <= max(abs(dl[[mag]]$nums.scl))]
      for(i in seq_along(try)){
        values <- plyr::round_any(dl[[mag]]$nums.scl, accuracy = try[i])
        unique <- !any(duplicated(values))
        if(any(abs(values) == 1000)){
          dl[[mag]]$rescale <- TRUE
        }
        if(unique){
          dl[[mag]]$acc <- try[i]
          break
        }
      }
    }

    # Rounding numbers
    if(dl[[mag]]$rescale){
      dl[[mag]]$nums.scl <- dl[[mag]]$nums.scl / 1e3
      dl[[mag]]$suf <- dplyr::case_when(dl[[mag]]$suf == "" ~ "K",
                                        dl[[mag]]$suf == "K" ~ "M",
                                        dl[[mag]]$suf == "M" ~ "B")
      try <- 1
      repeat{
        values <- round(dl[[mag]]$nums.scl, digits = try)
        unique <- !any(duplicated(values))
        if(unique){
          dl[[mag]]$dig <- try
          dl[[mag]]$acc <- 10^(-try)
          break
        }else{
          try <- try + 1
        }
      }
    }
    dl[[mag]]$num.rnd <- plyr::round_any(x = dl[[mag]]$nums.scl, dl[[mag]]$acc)

    # Formatting numbers
    dl[[mag]]$num.fmt <- paste0(format(dl[[mag]]$num.rnd, trim = TRUE, nsmall = dl[[mag]]$dig),
                                dl[[mag]]$suf)

  }

  # Checking for formatting exceptions
  num.fmt <- unname(unlist(sapply(dl, "[", "num.fmt")))
  if(length(dl) > 1){
    not.sci <- !all(grepl("[AZ]", num.fmt))
    sim.dig <- max(nchar(gsub("-", "", num.fmt))) - min(nchar(gsub("-", "", num.fmt))) <= 2
    max.dec <- max(nchar(stringr::str_split(num.fmt, "\\.", simplify = TRUE)[, 2]))
    if(not.sci & sim.dig & max.dec <= 2){
      num.rnd <- unname(unlist(sapply(dl, "[", "num.rnd")))
      num.fmt <- format(num.rnd, trim = TRUE, nsmall = max.dec)
    }
  }

  # Reordering formatted numbers
  ids <- unname(unlist(sapply(dl, "[", "ids")))
  num.fmt <- num.fmt[order(ids)]

  return(num.fmt)

}
